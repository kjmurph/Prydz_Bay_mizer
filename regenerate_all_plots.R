# Regenerate All MC Analysis Plots
# This script regenerates biomass and yield plots using cached data

cat("Regenerating All Monte Carlo Plots...\n\n")

# ==============================================================================
# Load helper functions
# ==============================================================================
cat("Loading helper functions...\n")
source("mc_analysis_helpers.R")

# ==============================================================================
# Load observed data
# ==============================================================================
cat("\nLoading observed data...\n")

# Load yield data
yield_ts_tidy <- readRDS("yield_observed_timeseries_tidy.RDS")
cat("  ✓ Loaded yield_ts_tidy\n")

# Load obs biomass
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

# ==============================================================================
# Load cached biomass and yield data
# ==============================================================================
cat("\nLoading cached timeseries data...\n")

# Load biomass
if (file.exists("mc_analysis_output/biomass_timeseries_all_sims.rds")) {
  biomass_df <- readRDS("mc_analysis_output/biomass_timeseries_all_sims.rds")
  cat("  ✓ Loaded biomass for", length(unique(biomass_df$sim_id)), "simulations\n")
} else {
  cat("  ⚠ Biomass data not found in cache\n")
  cat("  Extracting from block files (this may take a few minutes)...\n")
  
  # Load MC results
  mc_results <- load_and_prepare_mc_results(
    base_save_path = "Output_large_files/mc_5k_blocks",
    load_simulations = FALSE,
    verbose = FALSE
  )
  
  # Extract biomass
  biomass_df <- extract_biomass_timeseries(
    mc_results = mc_results,
    load_from_blocks = TRUE
  )
  
  # Save for future use
  saveRDS(biomass_df, "mc_analysis_output/biomass_timeseries_all_sims.rds")
  cat("  ✓ Extracted and saved biomass for", length(unique(biomass_df$sim_id)), "simulations\n")
}

# Load yield
if (file.exists("mc_analysis_output/yield_timeseries_all_sims.rds")) {
  yield_df <- readRDS("mc_analysis_output/yield_timeseries_all_sims.rds")
  cat("  ✓ Loaded yield for", length(unique(yield_df$sim_id)), "simulations\n")
} else {
  stop("Yield data not found! Run full analysis first.")
}

# ==============================================================================
# Regenerate biomass plots
# ==============================================================================
cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   REGENERATING BIOMASS PLOTS                                  ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

# Ensure output directory exists
dir.create("mc_analysis_output", showWarnings = FALSE)

# Create biomass uncertainty plot
cat("Creating biomass timeseries plot...\n")
p_biomass <- plot_biomass_uncertainty(
  biomass_df = biomass_df,
  obs_biomass_data = obs_biomass_data
)

ggsave("mc_analysis_output/biomass_timeseries_uncertainty.png", 
       p_biomass, 
       width = 12, 
       height = 10, 
       dpi = 300)
cat("  ✓ Saved: biomass_timeseries_uncertainty.png\n")

# Note: Skipping biomass_obs_period_comparison.png (function not in helpers)

# ==============================================================================
# Regenerate yield plot
# ==============================================================================
cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   REGENERATING YIELD PLOTS                                    ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

# Create yield uncertainty plot
cat("Creating yield timeseries plot...\n")
p_yield <- plot_yield_uncertainty(
  yield_df = yield_df,
  obs_yield_data = yield_ts_tidy
)

ggsave("mc_analysis_output/yield_timeseries_uncertainty.png", 
       p_yield, 
       width = 12, 
       height = 10, 
       dpi = 300)
cat("  ✓ Saved: yield_timeseries_uncertainty.png\n")

# ==============================================================================
# Display summary
# ==============================================================================
cat("\n\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   REGENERATION COMPLETE!                                      ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

cat("Updated plots:\n")
cat("  ✓ biomass_timeseries_uncertainty.png (with species ordering & colors)\n")
cat("  ✓ yield_timeseries_uncertainty.png (with all species & formatting)\n\n")

cat("Check the mc_analysis_output/ folder for all results!\n\n")

cat("Plots now include:\n")
cat("  Biomass:\n")
cat("    - Species ordered smallest to largest\n")
cat("    - Biomass in tonnes [t]\n")
cat("    - Color-coded by species\n")
cat("    - Observation bounds (±25%)\n\n")
cat("  Yield:\n")
cat("    - All 9 fished species\n")
cat("    - Log scale with dynamic tonne labels\n")
cat("    - Vertical lines at 1961 and 2010\n")
cat("    - Smart zero filtering\n\n")

cat("Regeneration successfully completed!\n")
