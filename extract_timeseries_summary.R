# Extract per-year time-series summaries for biomass and yield (min,q05,q25,median,q75,q95,max)
# Saves CSVs in `monte_carlo_2111_summaries/` and uses tonnes (divide by 1e6)

library(dplyr)
library(reshape2)
library(mizer)
library(tidyr)

cat("Starting time-series summary extraction...\n")

mc_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/combined_rerun_successful_sims_20250923_122211.rds"
if (!file.exists(mc_file)) {
  stop("MC results file not found: ", mc_file)
}
mc_results <- readRDS(mc_file)
cat("Loaded MC results: total sims =", length(mc_results$simulations), "\n")

# Helper to extract and add sim_id
extract_biomass <- function(sim, sim_id) {
  bm <- try(getBiomass(sim), silent=TRUE)
  if (inherits(bm, "try-error") || is.null(bm)) return(NULL)
  df <- reshape2::melt(bm)
  names(df) <- c("Year","Species","Biomass")
  df$Year <- as.numeric(as.character(df$Year))
  df$sim_id <- sim_id
  return(df)
}

extract_yield <- function(sim, sim_id) {
  yld <- try(getYield(sim), silent=TRUE)
  if (inherits(yld, "try-error") || is.null(yld)) return(NULL)
  df <- reshape2::melt(yld)
  names(df) <- c("Year","Species","Yield")
  df$Year <- as.numeric(as.character(df$Year))
  df$sim_id <- sim_id
  return(df)
}

# Iterate and collect all biomass and yield
all_bm <- list()
all_yld <- list()
nsims <- if (!is.null(mc_results$n_successful)) mc_results$n_successful else length(mc_results$simulations)
for (i in seq_len(nsims)) {
  if (i %% 200 == 0) cat("Processing sim", i, "of", nsims, "\n")
  sim <- mc_results$simulations[[i]]
  bm_df <- extract_biomass(sim, i)
  yld_df <- extract_yield(sim, i)
  if (!is.null(bm_df)) all_bm[[length(all_bm)+1]] <- bm_df
  if (!is.null(yld_df)) all_yld[[length(all_yld)+1]] <- yld_df
}

biomass_df <- bind_rows(all_bm)
yield_df <- bind_rows(all_yld)

cat("Collected biomass rows:", nrow(biomass_df), "; unique sims:", length(unique(biomass_df$sim_id)), "\n")
cat("Collected yield rows:", nrow(yield_df), "; unique sims:", length(unique(yield_df$sim_id)), "\n")

if (nrow(biomass_df) == 0) {
  stop("No biomass rows collected. This usually means getBiomass() failed for all simulations. Ensure 'mizer' is installed and the combined RDS contains valid simulation objects.")
}

# Convert to tonnes
biomass_df <- biomass_df %>% mutate(Biomass_t = Biomass/1e6)
yield_df <- yield_df %>% mutate(Yield_t = Yield/1e6)

# Calculate per-year stats for biomass
cat("Calculating per-year biomass stats...\n")
biomass_ts_summary <- biomass_df %>%
  group_by(Year, Species) %>%
  summarise(
    n = sum(!is.na(Biomass_t)),
    min_t = min(Biomass_t, na.rm=TRUE),
    q05_t = quantile(Biomass_t, probs=0.05, na.rm=TRUE, type=7),
    q25_t = quantile(Biomass_t, probs=0.25, na.rm=TRUE, type=7),
    median_t = median(Biomass_t, na.rm=TRUE),
    q75_t = quantile(Biomass_t, probs=0.75, na.rm=TRUE, type=7),
    q95_t = quantile(Biomass_t, probs=0.95, na.rm=TRUE, type=7),
    max_t = max(Biomass_t, na.rm=TRUE),
    .groups = 'drop'
  )

# Calculate per-year stats for yield (keep species that have any positive yields)
cat("Calculating per-year yield stats...\n")
# some yields may be negative numerically; ensure pmax(0, .) when computing quantiles for plotting
yield_df <- yield_df %>% mutate(Yield_t_pos = pmax(Yield_t, 0))

yield_ts_summary <- yield_df %>%
  group_by(Year, Species) %>%
  summarise(
    n = sum(!is.na(Yield_t_pos)),
    min_t = min(Yield_t_pos, na.rm=TRUE),
    q05_t = quantile(Yield_t_pos, probs=0.05, na.rm=TRUE, type=7),
    q25_t = quantile(Yield_t_pos, probs=0.25, na.rm=TRUE, type=7),
    median_t = median(Yield_t_pos, na.rm=TRUE),
    q75_t = quantile(Yield_t_pos, probs=0.75, na.rm=TRUE, type=7),
    q95_t = quantile(Yield_t_pos, probs=0.95, na.rm=TRUE, type=7),
    max_t = max(Yield_t_pos, na.rm=TRUE),
    .groups = 'drop'
  )

# Ensure output dir
outdir <- "monte_carlo_2111_summaries"
dir.create(outdir, showWarnings = FALSE)

# Observed biomass: place ObsBiomass only for years 2010:2020
cat("Preparing observed biomass entries for 2010-2020 only...\n")
obs_biomass_data <- data.frame(
  Species = c("mesozooplankton","other krill","other macrozooplankton","antarctic krill",
              "salps","mesopelagic fishes","bathypelagic fishes","shelf and coastal fishes",
              "flying birds","small divers","squids","toothfishes","leopard seals",
              "medium divers","large divers","minke whales","orca","sperm whales","baleen whales"),
  ObsBiomass_g = c(1.297420e+13, 2.801248e+12, 1.474341e+13, 5.897364e+12, 9.612703e+11, 
                   1.769209e+12, 1.769209e+12, 4.027900e+12, 4.423023e+09, 2.358946e+10, 
                   2.211512e+11, 1.105756e+12, 2.948682e+09, 3.907004e+11, 1.621775e+10, 
                   2.064077e+10, 8.846046e+09, 1.621775e+10, 1.872413e+11)
)
obs_biomass_data$ObsBiomass_t <- obs_biomass_data$ObsBiomass_g / 1e6
obs_years <- 2010:2020
obs_biomass_ts <- tidyr::crossing(Year = obs_years, obs_biomass_data)

# Merge observed biomass into the biomass summary: add ObsBiomass_t for Year in 2010:2020 only
biomass_ts_summary <- biomass_ts_summary %>%
  left_join(obs_biomass_ts %>% select(Year, Species, ObsBiomass_t), by=c("Year","Species"))

# For yield: observed yields are year-specific; we'll compute observed summary per Year/Species and merge
obs_yield <- readRDS("yield_observed_timeseries_tidy.RDS")
obs_yield <- obs_yield %>% mutate(ObsYield_t = Yield / 1e6)
# Merge observed yield per year/species (only the tonnes column)
yield_ts_summary <- yield_ts_summary %>%
  left_join(obs_yield %>% select(Year, Species, ObsYield_t), by=c("Year","Species"))

# Save CSVs (per-year time series)
cat("Saving CSVs...\n")
write.csv(biomass_ts_summary, file.path(outdir, "biomass_timeseries_summary_per_year_tonnes.csv"), row.names=FALSE)
write.csv(yield_ts_summary, file.path(outdir, "yield_timeseries_summary_per_year_tonnes.csv"), row.names=FALSE)

cat("Saved:", file.path(outdir, "biomass_timeseries_summary_per_year_tonnes.csv"), "and", file.path(outdir, "yield_timeseries_summary_per_year_tonnes.csv"), "\n")

# Quick verification output
cat("Biomass summary years range: ", min(biomass_ts_summary$Year, na.rm=TRUE), "-", max(biomass_ts_summary$Year, na.rm=TRUE), "\n")
cat("Yield summary years range: ", min(yield_ts_summary$Year, na.rm=TRUE), "-", max(yield_ts_summary$Year, na.rm=TRUE), "\n")

cat("Done.\n")
