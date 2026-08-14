# =============================================================================
# PHASE 8 -- score six alternative yield objectives OFFLINE, on the 320 union
# members, from cached per-species per-year yields. No re-projection.
#
# WHY. The published objective -- RMSE of log10(g+1), pooled over 344
# species-year rows -- has four measured defects:
#   * the "+1" is ONE GRAM: an arbitrary, unit-dependent floor;
#   * it rewards predicting near-zero when the temporal shape cannot be matched
#     (per-member optimisation drove shelf/coastal to 0.060, sperm to 0.039);
#   * structurally-zero rows dilute species unequally (baleen whales get 81 rows
#     of which only 41 have effort; krill 23 of 23);
#   * a 690 g record (bathypelagic fishes, one year) carries 6.3% of it.
#
# AND WHAT NO METRIC FIXES. Effort is BINARY 0/1 per gear per year while observed
# catch varies by 5.0-6.4 decades inside the effort-on years, so ~75% of the
# objective is a structural floor carried equally by every member
# (`R/catch_fit/07_error_floor.R`, docs/catch_fit_RESULTS.md S6). The variants
# below address the other defects; the floor needs time-resolved effort.
#
# THE VARIANTS
#   0  current      pooled RMSE of log10(x+1) over all window rows
#   1  offset       as 0 but log10(x + c_s), c_s = 1% of that species' mean
#                   non-zero observed catch -- removes the 1 g unit dependence
#   2  effort-on    pooled RMSE of log10((mod+c_s)/(obs+c_s)) over effort-ON rows
#                   only -- drops the structurally-zero rows that dilute unequally
#   3  total ratio  RMS across species of log10(sum mod / sum obs) -- equal weight
#                   per species, symmetric in over/under, ignores timing
#   4  magnitude+shape  as 3 plus (1 - Pearson r of the annual log series)
#   5  floor skill  mean across species of sse_s / floor_sse_s
#   6  restricted   as 0 but over the six representable groups only (drops shelf
#                   and coastal, toothfishes, bathypelagic fishes)
#
# A TRAP WORTH STATING. Dividing the POOLED sse by the floor would not reorder
# anything -- the floor is the same constant for every member, so it is a
# rescaling. Variant 5 only reorders because it normalises PER SPECIES and then
# weights species equally, which is where the reweighting happens.
#
# SCOPE. Ranks WITHIN the 320 union members and cuts the top 10% of those (32),
# because per-year yields are cached only for the union. This is a re-ranking of
# that subset, not of all 1,668.
#
# USAGE  Rscript R/wmin_test/49_objective_variants.R
# OUT    Output_large_files/wmin_test/49_objective_variants.{rds,csv}
# =============================================================================

suppressPackageStartupMessages({library(dplyr); library(reshape2); library(tidyr)})
out_dir <- "Output_large_files/wmin_test"
BAL <- "baleen whales"
DROP <- c("shelf and coastal fishes", "toothfishes", "bathypelagic fishes")

Y    <- readRDS(file.path(out_dir, "47_cut_yields.rds"))       # GRAMS
CUTS <- readRDS(file.path(out_dir, "46_selection_cuts.rds"))$cuts
TRAJ <- readRDS(file.path(out_dir, "46_trajectories.rds"))
effort_arr <- readRDS("effort_array_1841_2010.rds")

# --- observed, and the per-species effort window (the canonical objective) ----
ew <- do.call(rbind, lapply(colnames(effort_arr), function(s) {
  y <- as.numeric(rownames(effort_arr))[effort_arr[, s] > 0]
  if (length(y)) data.frame(Species = s, first_year = min(y), last_year = max(y),
                            stringsAsFactors = FALSE)
}))
obs <- read.csv("yield_observed_timeseries.csv") %>%
  melt(id.vars = "Year", variable.name = "Species", value.name = "obs") %>%
  mutate(Species = gsub("\\.", " ", as.character(Species)),
         obs = pmax(coalesce(as.numeric(obs), 0), 0)) %>%
  left_join(ew, by = "Species") %>%
  filter(!is.na(first_year), Year >= first_year, Year <= last_year) %>%
  select(Year, Species, obs)
FIT_SP <- obs %>% group_by(Species) %>% summarise(o = sum(obs), .groups = "drop") %>%
  filter(o > 0) %>% pull(Species)
obs <- obs %>% filter(Species %in% FIT_SP)
stopifnot(nrow(obs) == 344)                      # the canonical 344 rows

eff_long <- melt(effort_arr, varnames = c("Year", "Species"), value.name = "effort") %>%
  mutate(Year = as.numeric(as.character(Year)), Species = as.character(Species))
obs <- obs %>% left_join(eff_long, by = c("Year", "Species")) %>%
  mutate(effort = coalesce(effort, 0))

# per-species offset: 1% of the mean NON-ZERO observed catch in the window
cs <- obs %>% filter(obs > 0) %>% group_by(Species) %>%
  summarise(c_s = 0.01 * mean(obs), .groups = "drop")
obs <- obs %>% left_join(cs, by = "Species")

# --- the structural floor, per species (R/catch_fit/07_error_floor.R) --------
# For a model emitting 0 when effort is off and a constant when it is on, the
# objective is minimised at log10(c+1) = mean of log10(obs+1) over effort-ON
# years. Neither term contains q, so this is what every member carries.
flr <- obs %>% group_by(Species) %>%
  summarise(n = n(),
            m_on = mean(log10(obs[effort > 0] + 1)),
            floor_sse = sum(log10(obs[effort == 0] + 1)^2) +
                        sum((log10(obs[effort > 0] + 1) - m_on)^2),
            .groups = "drop") %>%
  mutate(floor_rmse = sqrt(floor_sse / n))
cat("=== structural floor per species (log10(x+1) basis) ===\n")
print(as.data.frame(flr %>% transmute(Species, n, floor_rmse = signif(floor_rmse, 4))),
      row.names = FALSE)
cat("  pooled floor RMSE:", signif(sqrt(sum(flr$floor_sse) / sum(flr$n)), 4), "\n\n")

# --- join modelled to observed ----------------------------------------------
members <- sort(unique(unlist(CUTS)))
D <- Y %>% filter(Species %in% FIT_SP, sim_index %in% members) %>%
  inner_join(obs, by = c("Year", "Species")) %>%
  rename(mod = Yield_t) %>%
  mutate(mod = pmax(coalesce(mod, 0), 0))
stopifnot(nrow(D) == 344 * length(members))
cat("scoring", length(members), "members x 344 rows\n\n")

# --- the six variants --------------------------------------------------------
per_sp <- D %>% group_by(sim_index, Species) %>%
  summarise(n = n(),
            sse1 = sum((log10(mod + 1) - log10(obs + 1))^2),
            sse_c = sum((log10(mod + c_s) - log10(obs + c_s))^2),
            n_on = sum(effort > 0),
            sse_on = sum(((log10(mod + c_s) - log10(obs + c_s))^2)[effort > 0]),
            r_on = suppressWarnings(cor(log10(mod[effort > 0] + c_s[effort > 0]),
                                        log10(obs[effort > 0] + c_s[effort > 0]))),
            mod_tot = sum(mod), obs_tot = sum(obs), .groups = "drop") %>%
  left_join(flr %>% select(Species, floor_sse), by = "Species") %>%
  mutate(r_on = coalesce(r_on, 0),
         lr = log10((mod_tot + 1) / (obs_tot + 1)))

score <- per_sp %>% group_by(sim_index) %>%
  summarise(
    m0_current   = sqrt(sum(sse1) / sum(n)),
    m1_offset    = sqrt(sum(sse_c) / sum(n)),
    m2_efforton  = sqrt(sum(sse_on) / sum(n_on)),
    m3_totratio  = sqrt(mean(lr^2)),
    m4_magshape  = sqrt(mean(lr^2 + (1 - r_on))),
    m5_floorskill= mean(sse1 / floor_sse),
    m6_restrict  = sqrt(sum(sse1[!Species %in% DROP]) /
                        sum(n[!Species %in% DROP])),
    .groups = "drop")

bal <- per_sp %>% filter(Species == BAL) %>%
  transmute(sim_index, br = mod_tot / obs_tot)
score <- score %>% left_join(bal, by = "sim_index")

# --- the canonical SNR, computable because 46 cached both arms for the union --
snr_at <- function(sel, year = 2010) {
  r <- TRAJ[as.character(sel)]; r <- r[!vapply(r, is.null, logical(1))]
  if (length(r) < 5) return(NA_real_)
  yrs <- r[[1]]$years
  E <- do.call(rbind, lapply(r, `[[`, "exploited"))
  U <- do.call(rbind, lapply(r, `[[`, "unexploited"))
  (apply(E - U, 2, median) / sd(colMeans(U)))[which.min(abs(yrs - year))]
}

n_top <- ceiling(nrow(score) * 0.10)
mets  <- grep("^m[0-6]_", names(score), value = TRUE)
base_sel <- score$sim_index[order(score$m0_current)][seq_len(n_top)]

cat("=== how each objective reorders the", nrow(score), "union members (top 10% =",
    n_top, ") ===\n\n")
out <- do.call(rbind, lapply(mets, function(m) {
  ord <- order(score[[m]])
  sel <- score$sim_index[ord][seq_len(n_top)]
  data.frame(
    objective       = m,
    rho_vs_current  = round(cor(score$m0_current, score[[m]], method = "spearman"), 3),
    rho_vs_baleen   = round(cor(score[[m]], score$br, method = "spearman"), 3),
    top_overlap     = sum(sel %in% base_sel),
    sel_med_baleen  = signif(median(score$br[score$sim_index %in% sel]), 3),
    sel_SNR_2010    = signif(unname(snr_at(sel)), 4),
    stringsAsFactors = FALSE)
}))
print(out, row.names = FALSE)

cat("\n  rho_vs_baleen: negative = a better whale fit gives a better score.\n")
cat("  over all", nrow(score), "union members the baleen catch ratio is median",
    signif(median(score$br), 3), "\n")
cat("  published SNR at 2010: -0.920 | current-rule cut of all 1,668: -0.147\n")

cat("\n=== per-species: how far above its structural floor is the median member? ===\n")
ps <- per_sp %>% group_by(Species) %>%
  summarise(rmse_med = median(sqrt(sse1 / n)), .groups = "drop") %>%
  left_join(flr %>% transmute(Species, floor_rmse), by = "Species") %>%
  mutate(excess = rmse_med - floor_rmse,
         pct_of_floor = round(100 * rmse_med / floor_rmse, 1)) %>%
  arrange(desc(excess))
print(as.data.frame(ps %>% mutate(across(where(is.numeric), ~signif(.x, 4)))),
      row.names = FALSE)
cat("\n  at or below 100% of floor = catchability has nothing left to give there\n")

saveRDS(list(score = score, per_species = per_sp, floor = flr, comparison = out),
        file.path(out_dir, "49_objective_variants.rds"))
write.csv(out, file.path(out_dir, "49_objective_variants.csv"), row.names = FALSE)
cat("\nWrote 49_objective_variants.{rds,csv}\n")