# Extract 2111 Simulation Data to CSV Files
# This script extracts biomass, yield, and parameter data from the Monte Carlo results
# and creates CSV files with min, median, max values for each species

cat("Extracting 2111 Simulation Data to CSV Files...\n\n")

library(dplyr)
library(tidyr)
library(mizer)
library(reshape2)

# ==============================================================================
# 1. Load the Monte Carlo results
# ==============================================================================
cat("Loading Monte Carlo results...\n")

# Try the most recent combined results file
mc_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/combined_rerun_successful_sims_20250923_122211.rds"

if (!file.exists(mc_file)) {
  mc_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/combined_rerun_successful_sims_20250917_103731.rds"
}

mc_results <- readRDS(mc_file)

cat("  Loaded file:", mc_file, "\n")
cat("  Total simulations:", mc_results$n_successful, "\n")

# ==============================================================================
# 2. Extract biomass and yield data from all simulations
# ==============================================================================
cat("\nExtracting biomass data from all simulations...\n")

# Function to extract biomass from a simulation
extract_biomass_from_sim <- function(sim) {
  bm <- try(getBiomass(sim), silent = TRUE)
  if (inherits(bm, "try-error") || is.null(bm)) return(NULL)
  
  # Convert to long format
  bm_df <- reshape2::melt(bm, varnames = c("Year", "Species"), value.name = "Biomass")
  bm_df$Year <- as.numeric(as.character(bm_df$Year))
  bm_df$Species <- as.character(bm_df$Species)
  return(bm_df)
}

# Extract biomass from all simulations
all_biomass <- list()
for (i in seq_along(mc_results$simulations)) {
  if (i %% 100 == 0) cat("  Processing simulation", i, "of", length(mc_results$simulations), "\n")
  
  sim_data <- extract_biomass_from_sim(mc_results$simulations[[i]])
  if (!is.null(sim_data)) {
    sim_data$sim_id <- i
    all_biomass[[i]] <- sim_data
  }
}

biomass_df <- bind_rows(all_biomass)
cat("  Extracted biomass from", length(unique(biomass_df$sim_id)), "simulations\n")

# ==============================================================================
# 3. Extract yield data from all simulations
# ==============================================================================
cat("\nExtracting yield data from all simulations...\n")

# Function to extract yield from a simulation
extract_yield_from_sim <- function(sim) {
  yld <- try(getYield(sim), silent = TRUE)
  if (inherits(yld, "try-error") || is.null(yld)) return(NULL)
  
  # Convert to long format
  yld_df <- reshape2::melt(yld, varnames = c("Year", "Species"), value.name = "Yield")
  yld_df$Year <- as.numeric(as.character(yld_df$Year))
  yld_df$Species <- as.character(yld_df$Species)
  return(yld_df)
}

# Extract yield from all simulations
all_yield <- list()
for (i in seq_along(mc_results$simulations)) {
  if (i %% 100 == 0) cat("  Processing simulation", i, "of", length(mc_results$simulations), "\n")
  
  sim_data <- extract_yield_from_sim(mc_results$simulations[[i]])
  if (!is.null(sim_data)) {
    sim_data$sim_id <- i
    all_yield[[i]] <- sim_data
  }
}

yield_df <- bind_rows(all_yield)
cat("  Extracted yield from", length(unique(yield_df$sim_id)), "simulations\n")

# ==============================================================================
# 4. Calculate min, median, max biomass by species
# ==============================================================================
cat("\nCalculating biomass statistics by species...\n")

biomass_summary <- biomass_df %>%
  group_by(Species) %>%
  summarise(
    min_biomass_g = min(Biomass, na.rm = TRUE),
    median_biomass_g = median(Biomass, na.rm = TRUE),
    max_biomass_g = max(Biomass, na.rm = TRUE),
    min_biomass_t = min_biomass_g / 1e6,
    median_biomass_t = median_biomass_g / 1e6,
    max_biomass_t = max_biomass_g / 1e6,
    .groups = 'drop'
  )

# ==============================================================================
# 5. Add observed biomass data
# ==============================================================================
cat("Adding observed biomass data...\n")

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

obs_biomass_data <- obs_biomass_data %>%
  mutate(ObsBiomass_t = ObsBiomass_g / 1e6)

# Merge with summary
biomass_summary_with_obs <- biomass_summary %>%
  left_join(obs_biomass_data, by = "Species") %>%
  select(Species, 
         min_biomass_g, median_biomass_g, max_biomass_g, ObsBiomass_g,
         min_biomass_t, median_biomass_t, max_biomass_t, ObsBiomass_t)

# ==============================================================================
# 6. Calculate min, median, max yield by species
# ==============================================================================
cat("\nCalculating yield statistics by species...\n")

# Filter to species with actual yields
species_with_yield <- yield_df %>%
  group_by(Species) %>%
  summarise(max_yield = max(Yield, na.rm = TRUE), .groups = 'drop') %>%
  filter(max_yield > 0) %>%
  pull(Species)

yield_summary <- yield_df %>%
  filter(Species %in% species_with_yield) %>%
  group_by(Species) %>%
  summarise(
    min_yield_g = min(Yield, na.rm = TRUE),
    median_yield_g = median(Yield, na.rm = TRUE),
    max_yield_g = max(Yield, na.rm = TRUE),
    min_yield_t = min_yield_g / 1e6,
    median_yield_t = median_yield_g / 1e6,
    max_yield_t = max_yield_g / 1e6,
    .groups = 'drop'
  )

cat("  Species with yields:", paste(yield_summary$Species, collapse = ", "), "\n")

# ==============================================================================
# 7. Add observed yield data
# ==============================================================================
cat("Adding observed yield data...\n")

# Load observed yield data
yield_ts_tidy <- readRDS("yield_observed_timeseries_tidy.RDS")

# Calculate mean observed yield by species (across all years)
obs_yield_summary <- yield_ts_tidy %>%
  group_by(Species) %>%
  summarise(
    ObsYield_mean_g = mean(Yield, na.rm = TRUE),
    ObsYield_min_g = min(Yield, na.rm = TRUE),
    ObsYield_max_g = max(Yield, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    ObsYield_mean_t = ObsYield_mean_g / 1e6,
    ObsYield_min_t = ObsYield_min_g / 1e6,
    ObsYield_max_t = ObsYield_max_g / 1e6
  )

# Merge with summary
yield_summary_with_obs <- yield_summary %>%
  left_join(obs_yield_summary, by = "Species") %>%
  select(Species,
         min_yield_g, median_yield_g, max_yield_g,
         ObsYield_min_g, ObsYield_mean_g, ObsYield_max_g,
         min_yield_t, median_yield_t, max_yield_t,
         ObsYield_min_t, ObsYield_mean_t, ObsYield_max_t)

# ==============================================================================
# 8. Extract parameter ranges
# ==============================================================================
cat("\nExtracting parameter ranges...\n")

# Extract all parameters from the parameters list
all_params <- list()
for (i in seq_along(mc_results$parameters)) {
  param_set <- mc_results$parameters[[i]]
  
  param_df <- data.frame(
    sim_id = i,
    species = param_set$species_names,
    gamma = param_set$gamma_values,
    catchability = param_set$catchability,
    abundance_scaling = param_set$abundance_scaling,
    stringsAsFactors = FALSE
  )
  
  all_params[[i]] <- param_df
}

params_df <- bind_rows(all_params)

# Calculate parameter ranges by species
param_summary <- params_df %>%
  group_by(species) %>%
  summarise(
    gamma_min = min(gamma, na.rm = TRUE),
    gamma_median = median(gamma, na.rm = TRUE),
    gamma_max = max(gamma, na.rm = TRUE),
    catchability_min = min(catchability, na.rm = TRUE),
    catchability_median = median(catchability, na.rm = TRUE),
    catchability_max = max(catchability, na.rm = TRUE),
    abundance_scaling_min = min(abundance_scaling, na.rm = TRUE),
    abundance_scaling_median = median(abundance_scaling, na.rm = TRUE),
    abundance_scaling_max = max(abundance_scaling, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(Species = species)

# ==============================================================================
# 9. Extract PRIOR parameter space (all attempted simulations)
# ==============================================================================
cat("\nExtracting prior parameter space from attempted simulations...\n")

# The parameters in mc_results$parameters represent SUCCESSFUL simulations (posterior)
# To get the PRIOR (all attempted), we need to look at the original MC run settings

# Get the settings
settings <- mc_results$settings
cat("  Monte Carlo settings:\n")
cat("    Catchability SD:", settings$catchability_sd, "\n")
cat("    Abundance SD:", settings$abundance_sd, "\n")
cat("    Gamma SD:", settings$gamma_sd, "\n")

# Create prior parameter space summary
prior_summary <- data.frame(
  Parameter = c("catchability_sd", "abundance_sd", "gamma_sd", "total_attempts", "successful"),
  Value = c(
    settings$catchability_sd %||% NA,
    settings$abundance_sd %||% NA,
    settings$gamma_sd %||% NA,
    mc_results$n_attempts,
    mc_results$n_successful
  )
)

# ==============================================================================
# 10. Save all data to CSV files
# ==============================================================================
cat("\nSaving data to CSV files...\n")

# Create output directory
dir.create("monte_carlo_2111_summaries", showWarnings = FALSE)

# Save biomass summary
write.csv(biomass_summary_with_obs, 
          "monte_carlo_2111_summaries/biomass_min_median_max_with_observations.csv",
          row.names = FALSE)
cat("  ✓ Saved: biomass_min_median_max_with_observations.csv\n")

# Save yield summary
write.csv(yield_summary_with_obs,
          "monte_carlo_2111_summaries/yield_min_median_max_with_observations.csv",
          row.names = FALSE)
cat("  ✓ Saved: yield_min_median_max_with_observations.csv\n")

# Save parameter summary (posterior - accepted simulations)
write.csv(param_summary,
          "monte_carlo_2111_summaries/parameters_min_median_max_posterior.csv",
          row.names = FALSE)
cat("  ✓ Saved: parameters_min_median_max_posterior.csv\n")

# Save prior settings
write.csv(prior_summary,
          "monte_carlo_2111_summaries/prior_parameter_space_settings.csv",
          row.names = FALSE)
cat("  ✓ Saved: prior_parameter_space_settings.csv\n")

# Save full parameter dataset for more detailed analysis
write.csv(params_df,
          "monte_carlo_2111_summaries/all_parameters_all_simulations.csv",
          row.names = FALSE)
cat("  ✓ Saved: all_parameters_all_simulations.csv\n")

# ==============================================================================
# 11. Create summary report
# ==============================================================================
cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   EXTRACTION COMPLETE!                                        ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

cat("Summary of extracted data:\n")
cat("  - Number of simulations:", length(unique(biomass_df$sim_id)), "\n")
cat("  - Species analyzed:", nrow(biomass_summary_with_obs), "\n")
cat("  - Species with yields:", nrow(yield_summary_with_obs), "\n")
cat("\nFiles created in 'monte_carlo_2111_summaries/':\n")
cat("  1. biomass_min_median_max_with_observations.csv\n")
cat("  2. yield_min_median_max_with_observations.csv\n")
cat("  3. parameters_min_median_max_posterior.csv\n")
cat("  4. prior_parameter_space_settings.csv\n")
cat("  5. all_parameters_all_simulations.csv\n\n")

cat("Biomass summary preview:\n")
print(head(biomass_summary_with_obs, 10))

cat("\nYield summary preview:\n")
print(yield_summary_with_obs)

cat("\nParameter summary preview:\n")
print(head(param_summary, 10))

cat("\nPrior parameter space:\n")
print(prior_summary)

cat("\n✓ All data successfully extracted and saved!\n")
