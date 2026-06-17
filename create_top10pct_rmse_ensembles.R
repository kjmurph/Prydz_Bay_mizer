# =============================================================================
# create_top10pct_rmse_ensembles.R
#
# Build and save top-10% RMSE subsets for the exploited and unexploited
# ensembles, matched by simulation index.
#
# Inputs:
#   - yield_rmse_per_sim.csv
#   - Output_large_files/monte_carlo_results/combined_simulation_results/
#       rerun_results/mc_ensemble_2111_cleaned.rds
#   - Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds
#
# Output:
#   - Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

rmse_path <- "yield_rmse_per_sim.csv"
fished_path <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
climate_path <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
output_dir <- "Output_large_files/community_slope_analysis"
output_path <- file.path(output_dir, "top10pct_rmse_ensembles.rds")

if (!file.exists(rmse_path)) stop("Missing RMSE ranking file: ", rmse_path)
if (!file.exists(fished_path)) stop("Missing exploited ensemble file: ", fished_path)
if (!file.exists(climate_path)) stop("Missing unexploited ensemble file: ", climate_path)

rmse_df <- read.csv(rmse_path) %>% arrange(rank)
n_sims <- nrow(rmse_df)
n_top10pct <- ceiling(n_sims * 0.10)
top10pct_idx <- rmse_df$sim_index[1:n_top10pct]

fished_ensemble <- readRDS(fished_path)
climate_ensemble <- readRDS(climate_path)

fished_sims_all <- fished_ensemble$simulations
climate_sims_all <- if (is.list(climate_ensemble) && !is.null(climate_ensemble$simulations)) {
  climate_ensemble$simulations
} else {
  climate_ensemble
}

valid_fished <- vapply(fished_sims_all, function(sim) {
  inherits(sim, "MizerSim") && !any(is.nan(sim@n)) && !any(is.infinite(sim@n))
}, logical(1))

valid_climate <- vapply(climate_sims_all, function(sim) {
  inherits(sim, "MizerSim") && !any(is.nan(sim@n)) && !any(is.infinite(sim@n))
}, logical(1))

fished_subset <- fished_sims_all[top10pct_idx]
climate_subset <- climate_sims_all[top10pct_idx]

metadata <- list(
  source_rmse_file = normalizePath(rmse_path, winslash = "/", mustWork = FALSE),
  source_fished_file = normalizePath(fished_path, winslash = "/", mustWork = FALSE),
  source_climate_file = normalizePath(climate_path, winslash = "/", mustWork = FALSE),
  n_total_sims = n_sims,
  n_top10pct = n_top10pct,
  top10pct_indices = top10pct_idx,
  selection_rule = "Top 10% of simulations ranked by lowest RMSE of modelled catch vs observed yield",
  valid_fished_count = sum(valid_fished),
  valid_climate_count = sum(valid_climate),
  matched_by_index = TRUE,
  created_at = Sys.time()
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(
  list(
    fished_top10pct = fished_subset,
    climate_top10pct = climate_subset,
    metadata = metadata
  ),
  output_path
)

cat("Saved top-10% RMSE ensembles to:\n")
cat(output_path, "\n")
cat("n_top10pct =", n_top10pct, "\n")