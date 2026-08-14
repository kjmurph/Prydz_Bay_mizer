###############################################################################
# yield_rmse_by_group.R
#
# Group-level and overall RMSE between the MODELLED yield drawn in the
# yield figure (panel a: top-10% RMSE ensemble MEAN, per species per year)
# and the OBSERVED yield, evaluated within each species' effort window.
#
# This is the RMSE of the *plotted line* vs observations (a goodness-of-fit
# summary for the figure), NOT the per-simulation selection metric in
# yield_rmse_evaluation.R (which pools all species into one number per sim).
#
# Two scales are reported for every group and for the pooled total:
#   * natural units  -- RMSE in tonnes / yr (also shown as kt/yr); matches the
#                       figure's y-axis and is directly interpretable.
#   * log10(g + 1)   -- the transform used for simulation selection, so the
#                       overall value is comparable to yield_rmse_per_sim.csv.
#
# Inputs (all pre-existing caches -- nothing heavy is re-run):
#   yield_cached_top10pct_stack.rds   modelled top-10% ensemble MEAN (tonnes/yr)
#   yield_observed_timeseries.csv     observed yield (grams/yr)
#   effort_array_1841_2010.rds        per-species effort windows
#
# Output:
#   yield_rmse_by_group.csv           tidy table (per group + OVERALL rows)
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(reshape2)
})

cat("=== Group-level yield RMSE (modelled top-10% mean vs observed) ===\n\n")

TONNE_G <- 1e6   # 1 tonne = 1e6 g

# ---------------------------------------------------------------------------
# 1. Effort windows (comparison window per species) -- same source & logic as
#    yield_rmse_evaluation.R / yield_stacked_pct_variants.R
# ---------------------------------------------------------------------------
effort_arr <- readRDS("effort_array_1841_2010.rds")
effort_windows <- do.call(rbind, lapply(colnames(effort_arr), function(sp) {
  yrs <- as.numeric(rownames(effort_arr))[effort_arr[, sp] > 0]
  if (length(yrs) > 0)
    data.frame(Species = sp, first_year = min(yrs), last_year = max(yrs),
               stringsAsFactors = FALSE)
}))

# ---------------------------------------------------------------------------
# 2. Modelled yield = exactly the series plotted in panel a.
#    stack_df$median_t is the top-10% ensemble MEAN in tonnes/yr, already
#    zeroed outside each species' effort window.
# ---------------------------------------------------------------------------
stack_df <- readRDS("yield_cached_top10pct_stack.rds") %>%
  transmute(Year, Species = as.character(Species), Yield_mod_t = median_t)

# ---------------------------------------------------------------------------
# 3. Observed yield (grams -> tonnes), restricted to each species' effort
#    window -- identical restriction to yield_rmse_evaluation.R.
# ---------------------------------------------------------------------------
obs_raw <- read.csv("yield_observed_timeseries.csv")
obs_long <- obs_raw %>%
  reshape2::melt(id.vars = "Year", variable.name = "Species",
                 value.name = "Yield_g") %>%
  mutate(Species = gsub("\\.", " ", as.character(Species)),
         Yield_g = pmax(coalesce(as.numeric(Yield_g), 0), 0),
         Yield_obs_t = Yield_g / TONNE_G) %>%
  left_join(effort_windows, by = "Species") %>%
  filter(!is.na(first_year), Year >= first_year, Year <= last_year) %>%
  select(Year, Species, Yield_obs_t)

fished_species <- sort(unique(obs_long$Species))
cat("Fished groups evaluated:\n  ", paste(fished_species, collapse = ", "), "\n\n")

# ---------------------------------------------------------------------------
# 4. Join modelled to observed on the effort-window comparison rows.
#    (obs_long defines the rows; modelled mean joined in, 0 where absent.)
# ---------------------------------------------------------------------------
comp <- obs_long %>%
  left_join(stack_df, by = c("Year", "Species")) %>%
  mutate(Yield_mod_t = coalesce(Yield_mod_t, 0))

# ---------------------------------------------------------------------------
# 5. RMSE helpers
# ---------------------------------------------------------------------------
rmse_nat <- function(mod_t, obs_t)                       # tonnes / yr
  sqrt(mean((mod_t - obs_t)^2, na.rm = TRUE))
rmse_log <- function(mod_t, obs_t)                       # log10(g + 1)
  sqrt(mean((log10(mod_t * TONNE_G + 1) -
             log10(obs_t * TONNE_G + 1))^2, na.rm = TRUE))

# ---------------------------------------------------------------------------
# 6. Per-group RMSE
# ---------------------------------------------------------------------------
per_group <- comp %>%
  group_by(Species) %>%
  summarise(
    n_years       = n(),
    obs_mean_kt   = mean(Yield_obs_t, na.rm = TRUE) / 1e3,
    obs_peak_kt   = max(Yield_obs_t,  na.rm = TRUE) / 1e3,
    rmse_kt       = rmse_nat(Yield_mod_t, Yield_obs_t) / 1e3,
    rmse_log10    = rmse_log(Yield_mod_t, Yield_obs_t),
    .groups = "drop"
  ) %>%
  # normalised RMSE: RMSE as a % of the observed mean over the window.
  # Undefined (NA) for groups with negligible observed yield (obs mean <
  # 0.1 kt/yr) where the ratio is dominated by a near-zero denominator.
  mutate(nrmse_pct = ifelse(obs_mean_kt >= 0.1,
                            100 * rmse_kt / obs_mean_kt, NA_real_)) %>%
  arrange(desc(rmse_kt))

# ---------------------------------------------------------------------------
# 7. Overall RMSE
#    pooled     -- every species-year comparison row weighted equally
#                  (same convention as yield_rmse_evaluation.R)
#    mean_group -- unweighted mean of the per-group RMSEs
# ---------------------------------------------------------------------------
overall_pooled <- tibble(
  Species    = "OVERALL (pooled rows)",
  n_years    = nrow(comp),
  obs_mean_kt = mean(comp$Yield_obs_t, na.rm = TRUE) / 1e3,
  obs_peak_kt = max(comp$Yield_obs_t,  na.rm = TRUE) / 1e3,
  rmse_kt    = rmse_nat(comp$Yield_mod_t, comp$Yield_obs_t) / 1e3,
  rmse_log10 = rmse_log(comp$Yield_mod_t, comp$Yield_obs_t),
  nrmse_pct  = NA_real_
) %>% mutate(nrmse_pct = ifelse(obs_mean_kt >= 0.1,
                               100 * rmse_kt / obs_mean_kt, NA_real_))

overall_meangroup <- tibble(
  Species    = "OVERALL (mean of groups)",
  n_years    = sum(per_group$n_years),
  obs_mean_kt = mean(per_group$obs_mean_kt),
  obs_peak_kt = max(per_group$obs_peak_kt),
  rmse_kt    = mean(per_group$rmse_kt),
  rmse_log10 = mean(per_group$rmse_log10),
  nrmse_pct  = mean(per_group$nrmse_pct)
)

out <- bind_rows(per_group, overall_pooled, overall_meangroup)

# ---------------------------------------------------------------------------
# 8. Report
# ---------------------------------------------------------------------------
fmt <- out %>%
  transmute(
    Group          = Species,
    `n yr`         = n_years,
    `obs mean (kt/yr)` = round(obs_mean_kt, 2),
    `obs peak (kt/yr)` = round(obs_peak_kt, 1),
    `RMSE (kt/yr)`     = round(rmse_kt, 2),
    `NRMSE (% of mean)`= round(nrmse_pct, 0),
    `RMSE log10(g+1)`  = round(rmse_log10, 3)
  )

cat("Per-group and overall RMSE (modelled top-10% mean vs observed,\n")
cat("evaluated within each group's effort window):\n\n")
print(as.data.frame(fmt), row.names = FALSE)

write.csv(out, "yield_rmse_by_group.csv", row.names = FALSE)
cat("\nSaved: yield_rmse_by_group.csv\n")
cat("=== Done ===\n")