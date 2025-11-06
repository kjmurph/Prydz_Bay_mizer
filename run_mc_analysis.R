# Run Monte Carlo Analysis on 5000 simulations
# This script loads the required data and runs the comprehensive analysis

cat("Starting Monte Carlo Analysis...\n\n")

# ==============================================================================
# 1. Load helper functions
# ==============================================================================
cat("Loading helper functions...\n")
source("mc_analysis_helpers.R")

# ==============================================================================
# 2. Load observed data
# ==============================================================================
cat("\nLoading observed data...\n")

# Load yield data from RDS file
if (file.exists("yield_observed_timeseries_tidy.RDS")) {
  yield_ts_tidy <- readRDS("yield_observed_timeseries_tidy.RDS")
  cat("  ✓ Loaded yield_ts_tidy from RDS file\n")
  cat("    - Years:", range(yield_ts_tidy$Year), "\n")
  cat("    - Species:", length(unique(yield_ts_tidy$Species)), "\n")
} else {
  stop("Error: yield_observed_timeseries_tidy.RDS not found!")
}

# Define observed biomass data (from 09_Uncertainty_Analysis.Rmd, line 1177)
obs_biomass_data <- data.frame(
  Species = c("mesozooplankton","other krill","other macrozooplankton","antarctic krill",
              "salps","mesopelagic fishes","bathypelagic fishes","shelf and coastal fishes",
              "flying birds","small divers","squids","toothfishes","leopard seals",
              "medium divers","large divers","minke whales","orca","sperm whales","baleen whales"),
  ObsBiomass = c(1.297420e+13, 2.801248e+12, 1.474341e+13, 5.897364e+12, 9.612703e+11, 
                 1.769209e+12, 1.769209e+12, 4.027900e+12, 4.423023e+09, 2.358946e+10, 
                 2.211512e+11, 1.105756e+12, 2.948682e+09, 3.907004e+11, 1.621775e+10, 
                 2.064077e+10, 8.846046e+09, 1.621775e+10, 1.872413e+11)
)

cat("  ✓ Created obs_biomass_data\n")
cat("    - Species:", nrow(obs_biomass_data), "\n\n")

# ==============================================================================
# 3. Run comprehensive analysis
# ==============================================================================
cat("Running comprehensive Monte Carlo analysis...\n")
cat("This will:\n")
cat("  1. Load MC results from block files\n")
cat("  2. Analyze parameter distributions (prior vs posterior)\n")
cat("  3. Extract biomass time series from all simulations\n")
cat("  4. Create biomass uncertainty plots\n")
cat("  5. Extract yield time series\n")
cat("  6. Create yield uncertainty plots\n")
cat("  7. Save all plots to mc_analysis_output/\n\n")

results <- analyze_mc_results(
  base_save_path = "Output_large_files/mc_5k_blocks",
  obs_biomass_data = obs_biomass_data,
  obs_yield_data = yield_ts_tidy,
  obs_years = 2010:2020,
  catchability_sd = 2,
  abundance_sd = 4,
  gamma_sd = 2,
  load_simulations = FALSE,  # Memory efficient mode
  save_plots = TRUE,
  output_dir = "mc_analysis_output"
)

# ==============================================================================
# 4. Display summary
# ==============================================================================
cat("\n\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   ANALYSIS COMPLETE!                                          ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

cat("Results summary:\n")
cat("  - Total simulations attempted:", length(results$mc_results$all_attempted_params), "\n")
cat("  - Successful simulations:", length(results$mc_results$successful_params), "\n")
cat("  - Success rate:", 
    round(100 * length(results$mc_results$successful_params) / 
          length(results$mc_results$all_attempted_params), 1), "%\n\n")

cat("Plots saved to mc_analysis_output/:\n")
cat("  1. gamma_prior_posterior.png\n")
cat("  2. abundance_prior_posterior.png\n")
cat("  3. catchability_prior_posterior.png\n")
cat("  4. biomass_timeseries_uncertainty.png\n")
cat("  5. biomass_obs_period_comparison.png\n")
cat("  6. yield_timeseries_uncertainty.png\n\n")

cat("Parameter summaries available in:\n")
cat("  - results$param_analysis$gamma_summary\n")
cat("  - results$param_analysis$abundance_summary\n")
cat("  - results$param_analysis$catchability_summary\n\n")

cat("Time series data available in:\n")
cat("  - results$biomass_timeseries\n")
cat("  - results$yield_timeseries\n\n")

# Optional: Display parameter summaries
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   GAMMA PARAMETER SUMMARY                                     ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")
print(results$param_analysis$gamma_summary)

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   ABUNDANCE PARAMETER SUMMARY                                 ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")
print(results$param_analysis$abundance_summary)

cat("\nAnalysis complete! Check the mc_analysis_output/ folder for all plots.\n")
