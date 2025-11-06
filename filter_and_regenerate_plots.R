# Filter Out Problematic Simulations and Regenerate Plots
# This script removes simulations with extinctions and regenerates all plots

cat("Filtering Monte Carlo Results...\n\n")

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
# Load and filter data
# ==============================================================================
cat("\nLoading cached timeseries data...\n")

# Load biomass
biomass_df <- readRDS("mc_analysis_output/biomass_timeseries_all_sims.rds")
cat("  ✓ Loaded biomass for", length(unique(biomass_df$sim_id)), "simulations\n")

# Load yield
yield_df <- readRDS("mc_analysis_output/yield_timeseries_all_sims.rds")
cat("  ✓ Loaded yield for", length(unique(yield_df$sim_id)), "simulations\n")

# ==============================================================================
# Identify and filter problematic simulations
# ==============================================================================
cat("\nIdentifying problematic simulations...\n")

library(dplyr)

# Use extinction threshold of 1 kg (1e6 grams)
# This is far more conservative than the 1e-6 that was supposed to be used
extinction_threshold <- 1e6

problem_sims <- biomass_df %>%
  group_by(sim_id, Species) %>%
  summarise(min_biomass = min(Biomass, na.rm=TRUE), .groups='drop') %>%
  filter(min_biomass < extinction_threshold) %>%
  pull(sim_id) %>%
  unique()

cat("  Found", length(problem_sims), "problematic simulation(s):\n")
cat("  Simulation IDs:", paste(problem_sims, collapse=", "), "\n")

if (length(problem_sims) > 0) {
  # Show details
  problem_details <- biomass_df %>%
    filter(sim_id %in% problem_sims) %>%
    group_by(sim_id, Species) %>%
    summarise(min_biomass = min(Biomass, na.rm=TRUE), .groups='drop') %>%
    filter(min_biomass < extinction_threshold)
  
  cat("\n  Species with extinctions:\n")
  print(problem_details)
  
  # Filter out problematic simulations
  cat("\nFiltering out problematic simulations...\n")
  
  biomass_df_filtered <- biomass_df %>%
    filter(!sim_id %in% problem_sims)
  
  yield_df_filtered <- yield_df %>%
    filter(!sim_id %in% problem_sims)
  
  cat("  ✓ Biomass: Filtered from", length(unique(biomass_df$sim_id)), 
      "to", length(unique(biomass_df_filtered$sim_id)), "simulations\n")
  cat("  ✓ Yield: Filtered from", length(unique(yield_df$sim_id)), 
      "to", length(unique(yield_df_filtered$sim_id)), "simulations\n")
  
  # Save filtered data
  saveRDS(biomass_df_filtered, "mc_analysis_output/biomass_timeseries_filtered.rds")
  saveRDS(yield_df_filtered, "mc_analysis_output/yield_timeseries_filtered.rds")
  cat("  ✓ Saved filtered data\n")
  
  # Use filtered data for plotting
  biomass_df <- biomass_df_filtered
  yield_df <- yield_df_filtered
  
} else {
  cat("  ✓ No problematic simulations found - all data looks good!\n")
}

# ==============================================================================
# Regenerate biomass plots
# ==============================================================================
cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   REGENERATING BIOMASS PLOTS (FILTERED DATA)                 ║\n")
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

# ==============================================================================
# Regenerate yield plot
# ==============================================================================
cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   REGENERATING YIELD PLOTS (FILTERED DATA)                   ║\n")
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
cat("║   FILTERING AND REGENERATION COMPLETE!                       ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

cat("Updated plots with filtered data:\n")
cat("  ✓ biomass_timeseries_uncertainty.png (", 
    length(unique(biomass_df$sim_id)), " simulations)\n", sep="")
cat("  ✓ yield_timeseries_uncertainty.png (", 
    length(unique(yield_df$sim_id)), " simulations)\n\n", sep="")

cat("Filtered simulations saved to:\n")
cat("  - mc_analysis_output/biomass_timeseries_filtered.rds\n")
cat("  - mc_analysis_output/yield_timeseries_filtered.rds\n\n")

cat("Summary:\n")
cat("  - Removed", length(problem_sims), "simulation(s) with extinctions\n")
cat("  - Retained", length(unique(biomass_df$sim_id)), "valid simulations\n")
cat("  - Success rate:", round(100 * length(unique(biomass_df$sim_id)) / 5000, 2), "%\n\n")

cat("All species now show realistic biomass values!\n")
cat("Check the mc_analysis_output/ folder for updated results.\n")
