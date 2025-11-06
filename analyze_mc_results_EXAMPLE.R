# ===== MONTE CARLO RESULTS ANALYSIS - USAGE EXAMPLE =====
# This script demonstrates how to analyze your 5000 Monte Carlo simulations
# using the helper functions in mc_analysis_helpers.R

# ===== SETUP =====

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(scales)

# Source the helper functions
source("mc_analysis_helpers.R")

# Also source any functions needed from the MC script
source("MEMORY EFFICIENT MONTE CARLO ANALYSIS.r")

# ===== LOAD OBSERVED DATA =====

# Load your observed biomass data (adjust path as needed)
# Expected columns: Species, ObsBiomass
obs_biomass_data <- readRDS("observed_biomass_data.rds")  # Adjust filename

# Or if you have it defined in your main script:
# obs_biomass_data <- obs_biomass_data_complete

# Load your observed yield data (tidy format)
# Expected columns: year, species, yield
yield_ts_tidy <- readRDS("yield_observed_timeseries_tidy.RDS")

# ===== METHOD 1: QUICK ANALYSIS (EASIEST) =====

cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   METHOD 1: QUICK ANALYSIS                                    ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

# This does everything in one function call
results <- analyze_mc_results(
  base_save_path = "Output_large_files/mc_5k_blocks",        # Your MC results directory
  obs_biomass_data = obs_biomass_data,
  obs_yield_data = yield_ts_tidy,
  obs_years = 2010:2020,
  catchability_sd = 2,                      # Match what you used in MC
  abundance_sd = 4,                         # Match what you used in MC
  gamma_sd = 2,                             # Match what you used in MC
  load_simulations = FALSE,                 # Use block files (memory efficient)
  save_plots = TRUE,
  output_dir = "mc_analysis_output"
)

# View the plots
print(results$plots$gamma)                  # Gamma parameter distribution
print(results$plots$abundance)              # Abundance parameter distribution
print(results$plots$catchability)           # Catchability parameter distribution
print(results$plots$biomass_timeseries)     # Biomass time series with uncertainty
print(results$plots$biomass_obs_period)     # Biomass comparison for obs period
print(results$plots$yield_timeseries)       # Yield time series with uncertainty

# Access summary statistics
View(results$param_analysis$gamma_summary)
View(results$param_analysis$abundance_summary)

# ===== METHOD 2: STEP-BY-STEP ANALYSIS (MORE CONTROL) =====

cat("\n\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   METHOD 2: STEP-BY-STEP ANALYSIS                             ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

# Step 1: Load MC results
cat("\n=== Step 1: Loading MC Results ===\n")
mc_results <- load_and_prepare_mc_results(
  base_save_path = "Output_large_files/mc_5k_blocks",
  load_simulations = FALSE,  # Set TRUE to load full MizerSim objects (memory intensive!)
  verbose = TRUE
)

# Inspect what you have
cat("\nSuccessful simulations:", length(mc_results$successful_params), "\n")
cat("Failed simulations:", length(mc_results$failed_params), "\n")
cat("Total attempted:", length(mc_results$all_attempted_params), "\n")

# Step 2: Analyze parameter distributions
cat("\n=== Step 2: Parameter Distributions ===\n")
param_analysis <- analyze_parameter_distributions(
  mc_results = mc_results,
  catchability_sd = 2,
  abundance_sd = 4,
  gamma_sd = 2
)

# View summaries
print(param_analysis$gamma_summary)
print(param_analysis$abundance_summary)

# Step 3: Create parameter plots
cat("\n=== Step 3: Creating Parameter Plots ===\n")

# Gamma plot
p_gamma <- plot_prior_posterior_distributions(
  param_comparison = param_analysis$gamma_comparison,
  param_name = "gamma_change",
  title = "Gamma (Search Rate) - Prior vs Posterior"
)
print(p_gamma)
ggsave("gamma_prior_posterior.png", p_gamma, width = 14, height = 10, dpi = 300)

# Abundance plot
p_abundance <- plot_prior_posterior_distributions(
  param_comparison = param_analysis$abundance_comparison,
  param_name = "abundance_scaling",
  title = "Initial Abundance - Prior vs Posterior"
)
print(p_abundance)
ggsave("abundance_prior_posterior.png", p_abundance, width = 14, height = 10, dpi = 300)

# Catchability plot (if you have catchability data)
if (nrow(param_analysis$catchability_comparison) > 0) {
  p_catch <- plot_catchability_distributions(
    catchability_comparison = param_analysis$catchability_comparison
  )
  print(p_catch)
  ggsave("catchability_prior_posterior.png", p_catch, width = 10, height = 8, dpi = 300)
}

# Step 4: Extract biomass time series
cat("\n=== Step 4: Extracting Biomass Time Series ===\n")
cat("This will take several minutes as it loads from block files...\n")

biomass_df <- extract_biomass_timeseries(
  mc_results = mc_results,
  load_from_blocks = TRUE  # Load from disk (slower but works without full sims in memory)
)

# Save for later use
saveRDS(biomass_df, "biomass_timeseries_all_sims.rds")

# Step 5: Create biomass plots
cat("\n=== Step 5: Creating Biomass Plots ===\n")

# Full time series with uncertainty
p_biomass_ts <- plot_biomass_uncertainty(
  biomass_df = biomass_df,
  obs_biomass_data = obs_biomass_data,
  obs_years = 2010:2020
)
print(p_biomass_ts)
ggsave("biomass_timeseries_uncertainty.png", p_biomass_ts, 
       width = 16, height = 12, dpi = 300)

# Observation period comparison
p_biomass_obs <- plot_biomass_obs_period(
  biomass_df = biomass_df,
  obs_biomass_data = obs_biomass_data,
  obs_years = 2010:2020
)
print(p_biomass_obs)
ggsave("biomass_obs_period.png", p_biomass_obs, width = 12, height = 8, dpi = 300)

# Step 6: Extract yield time series
cat("\n=== Step 6: Extracting Yield Time Series ===\n")

yield_df <- extract_yield_timeseries(
  mc_results = mc_results,
  load_from_blocks = TRUE
)

# Save for later use
saveRDS(yield_df, "yield_timeseries_all_sims.rds")

# Step 7: Create yield plots
cat("\n=== Step 7: Creating Yield Plots ===\n")

p_yield <- plot_yield_uncertainty(
  yield_df = yield_df,
  obs_yield_data = yield_ts_tidy,
  highlight_species = c("baleen whales", "sperm whales", "minke whales", "orca")
)
print(p_yield)
ggsave("yield_timeseries_uncertainty.png", p_yield, width = 12, height = 10, dpi = 300)

# ===== METHOD 3: CUSTOM ANALYSES =====

cat("\n\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   METHOD 3: CUSTOM ANALYSES                                   ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

# If you've already run the analysis and saved intermediate files:

# Load saved biomass data
if (file.exists("biomass_timeseries_all_sims.rds")) {
  biomass_df <- readRDS("biomass_timeseries_all_sims.rds")
  cat("Loaded saved biomass data\n")
}

if (file.exists("yield_timeseries_all_sims.rds")) {
  yield_df <- readRDS("yield_timeseries_all_sims.rds")
  cat("Loaded saved yield data\n")
}

# Custom analysis 1: Focus on specific species
cat("\n=== Custom Analysis 1: Marine Mammals Only ===\n")

marine_mammals <- c("minke whales", "orca", "sperm whales", "baleen whales", 
                   "leopard seals", "large divers", "medium divers", "small divers")

biomass_mammals <- biomass_df %>%
  filter(Species %in% marine_mammals)

p_mammals <- plot_biomass_uncertainty(
  biomass_df = biomass_mammals,
  obs_biomass_data = obs_biomass_data %>% filter(Species %in% marine_mammals),
  obs_years = 2010:2020,
  highlight_species = c("baleen whales", "sperm whales")
)
print(p_mammals)
ggsave("biomass_marine_mammals_only.png", p_mammals, width = 12, height = 8, dpi = 300)

# Custom analysis 2: Parameter correlations
cat("\n=== Custom Analysis 2: Parameter Correlations ===\n")

# Extract successful parameters as wide format
param_wide <- param_analysis$successful_df %>%
  select(sim_id, species, gamma_change, abundance_scaling) %>%
  pivot_wider(
    names_from = species,
    values_from = c(gamma_change, abundance_scaling)
  )

# Calculate correlation matrix
cor_gamma <- cor(param_wide %>% select(starts_with("gamma_")), 
                 use = "pairwise.complete.obs")

# Plot correlation heatmap
library(reshape2)
cor_gamma_melt <- melt(cor_gamma)

p_cor <- ggplot(cor_gamma_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                      midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Gamma Parameter Correlations (Accepted Simulations)",
       x = "", y = "", fill = "Correlation") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7))

print(p_cor)
ggsave("parameter_correlations.png", p_cor, width = 12, height = 10, dpi = 300)

# Custom analysis 3: Temporal stability
cat("\n=== Custom Analysis 3: Temporal Stability Check ===\n")

# Calculate CV by year for each species
biomass_stability <- biomass_df %>%
  group_by(Year, Species) %>%
  summarise(
    mean_biomass = mean(Biomass, na.rm = TRUE),
    sd_biomass = sd(Biomass, na.rm = TRUE),
    cv = sd_biomass / mean_biomass,
    .groups = 'drop'
  )

p_stability <- ggplot(biomass_stability, aes(x = Year, y = cv, color = Species)) +
  geom_line() +
  facet_wrap(~Species, ncol = 4) +
  geom_hline(yintercept = 0.15, linetype = "dashed", color = "red") +
  labs(
    title = "Coefficient of Variation Over Time (Across Accepted Simulations)",
    subtitle = "Red line = 0.15 (stability threshold used in MC)",
    x = "Year",
    y = "CV (SD/Mean)"
  ) +
  theme_bw() +
  theme(legend.position = "none")

print(p_stability)
ggsave("temporal_cv_stability.png", p_stability, width = 14, height = 10, dpi = 300)

# Custom analysis 4: Acceptance rate by parameter range
cat("\n=== Custom Analysis 4: Acceptance Rates ===\n")

# Calculate acceptance rates by parameter bins
gamma_acceptance <- param_analysis$gamma_comparison %>%
  mutate(
    gamma_bin = cut(gamma_change, 
                   breaks = c(0, 0.5, 1, 2, 5, 10, Inf),
                   labels = c("<0.5×", "0.5-1×", "1-2×", "2-5×", "5-10×", ">10×"))
  ) %>%
  group_by(species, gamma_bin) %>%
  summarise(
    n_attempted = sum(distribution == "Prior (All Attempted)"),
    n_accepted = sum(distribution == "Posterior (Accepted)"),
    acceptance_rate = n_accepted / n_attempted,
    .groups = 'drop'
  ) %>%
  filter(n_attempted > 10)  # Only show bins with sufficient data

p_acceptance <- ggplot(gamma_acceptance, 
                       aes(x = gamma_bin, y = acceptance_rate, fill = species)) +
  geom_col() +
  facet_wrap(~species, ncol = 4) +
  labs(
    title = "Acceptance Rate by Gamma Parameter Range",
    x = "Gamma Change Factor",
    y = "Acceptance Rate"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_acceptance)
ggsave("acceptance_rates_by_gamma.png", p_acceptance, width = 14, height = 10, dpi = 300)

# Custom analysis 5: Species-specific uncertainty
cat("\n=== Custom Analysis 5: Species-Specific Uncertainty ===\n")

# Calculate relative uncertainty in obs period
uncertainty_obs <- biomass_df %>%
  filter(Year %in% 2010:2020) %>%
  group_by(Species) %>%
  summarise(
    median_biomass = median(Biomass, na.rm = TRUE),
    q05 = quantile(Biomass, 0.05, na.rm = TRUE),
    q95 = quantile(Biomass, 0.95, na.rm = TRUE),
    relative_uncertainty = (q95 - q05) / median_biomass,
    .groups = 'drop'
  ) %>%
  arrange(desc(relative_uncertainty))

print(uncertainty_obs)

p_uncertainty <- ggplot(uncertainty_obs, 
                        aes(x = reorder(Species, relative_uncertainty), 
                            y = relative_uncertainty)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Relative Uncertainty by Species (2010-2020)",
    subtitle = "Calculated as (95th - 5th percentile) / median",
    x = "",
    y = "Relative Uncertainty"
  ) +
  theme_bw()

print(p_uncertainty)
ggsave("species_uncertainty_ranking.png", p_uncertainty, width = 10, height = 8, dpi = 300)

# ===== SAVE SUMMARY REPORT =====

cat("\n=== Saving Summary Report ===\n")

sink("mc_analysis_summary_report.txt")

cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   MONTE CARLO ANALYSIS SUMMARY REPORT                         ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

cat("Analysis Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("=== SIMULATION SUMMARY ===\n")
cat("Total simulations attempted:", mc_results$master$run_info$total_sims, "\n")
cat("Successful simulations:", mc_results$master$run_info$n_successful, "\n")
cat("Failed simulations:", length(mc_results$failed_params), "\n")
cat("Success rate:", round(mc_results$master$run_info$success_rate * 100, 1), "%\n\n")

cat("=== PARAMETER SETTINGS ===\n")
cat("Catchability SD:", 2, "\n")
cat("Abundance SD:", 4, "\n")
cat("Gamma SD:", 2, "\n\n")

cat("=== GAMMA PARAMETER SUMMARY (ACCEPTED SIMULATIONS) ===\n")
print(param_analysis$gamma_summary)
cat("\n")

cat("=== ABUNDANCE PARAMETER SUMMARY (ACCEPTED SIMULATIONS) ===\n")
print(param_analysis$abundance_summary)
cat("\n")

cat("=== SPECIES UNCERTAINTY RANKING (2010-2020) ===\n")
print(uncertainty_obs)
cat("\n")

if (length(mc_results$failed_params) > 0) {
  cat("=== FAILURE ANALYSIS ===\n")
  failure_reasons <- sapply(mc_results$failed_params, function(f) f$error)
  failure_table <- sort(table(failure_reasons), decreasing = TRUE)
  print(failure_table)
  cat("\n")
}

cat("=== OUTPUT FILES GENERATED ===\n")
cat("Plots:\n")
cat("  - gamma_prior_posterior.png\n")
cat("  - abundance_prior_posterior.png\n")
cat("  - catchability_prior_posterior.png\n")
cat("  - biomass_timeseries_uncertainty.png\n")
cat("  - biomass_obs_period.png\n")
cat("  - yield_timeseries_uncertainty.png\n")
cat("  - parameter_correlations.png\n")
cat("  - temporal_cv_stability.png\n")
cat("  - acceptance_rates_by_gamma.png\n")
cat("  - species_uncertainty_ranking.png\n\n")

cat("Data files:\n")
cat("  - biomass_timeseries_all_sims.rds\n")
cat("  - yield_timeseries_all_sims.rds\n\n")

cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   END OF REPORT                                               ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n")

sink()

cat("\nSummary report saved to: mc_analysis_summary_report.txt\n")

# ===== COMPLETION MESSAGE =====

cat("\n\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   ANALYSIS COMPLETE!                                          ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")
cat("All plots and data have been generated and saved.\n")
cat("Check the output files for detailed results.\n\n")
