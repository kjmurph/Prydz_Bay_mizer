# Test script for adaptive uncertainty analysis
# This script demonstrates the adaptive approach with a smaller example

# Load required libraries
library(mizer)
library(therMizer)
library(dplyr)
library(ggplot2)

# Source the helper functions and adaptive functions
source("Helper_Functions.R")
source("adaptive_uncertainty_functions.R")

cat("=== Testing Adaptive Uncertainty Analysis ===\n\n")

# Load the base parameters
cat("Loading base parameters...\n")
params_base <- readRDS("params_steady_state_2011_2020_tol_0.00025.RDS")

# Load climate forcings
cat("Loading climate forcings...\n")
extended_ocean_temp <- readRDS("temperature_forcing_1841_2010.rds")
extended_n_pp_array <- readRDS("phytoplankton_forcing_1841_2010.rds")

# Setup params with climate forcings
params_test <- upgradeTherParams(params_base,
                                 ocean_temp_array = extended_ocean_temp,
                                 n_pp_array = extended_n_pp_array,
                                 aerobic_effect = FALSE, 
                                 metabolism_effect = TRUE)

# Load effort and yield data
cat("Loading effort and yield data...\n")
combined_effort_array <- readRDS("effort_array_1841_2010.rds")
yield_ts_tidy <- readRDS("yield_observed_timeseries_tidy.RDS")

# Run a small test with adaptive uncertainty analysis
cat("\n=== Running Small Test (100 simulations, check every 10) ===\n")

test_results <- run_adaptive_uncertainty_analysis(
  params = params_test,
  n_total_sims = 100,  # Small test with 100 simulations
  check_interval = 10,  # Check every 10 simulations (more frequent updates)
  initial_catchability_sd = 0.3,
  initial_abundance_sd = 0.4,
  marine_mammal_species = c("minke whales", "orca", "sperm whales", "baleen whales"),
  fished_species = c("antarctic krill", "minke whales", "toothfishes"),  # Focus on key species
  effort_scen = combined_effort_array,
  yield_obs_data = yield_ts_tidy,
  year_range = c(1961, 2010),
  tol = 0.01,
  t_max = 200,  # Reduced for testing
  spinup_years = 50,  # Reduced for testing
  t_start = 1841,
  sim_years = 170,
  preserve_erepro = TRUE,
  max_erepro = 1,
  convergence_threshold = 0.01,
  n_cores = 2,  # Use only 2 cores for testing
  parallel = FALSE,  # Sequential for testing
  verbose = TRUE
)

# Save test results
saveRDS(test_results, "test_adaptive_results.RDS")

# Display results summary
cat("\n=== Test Results Summary ===\n")
cat("Total simulations completed:", length(test_results$all_rmse), "\n")
cat("Best RMSE achieved:", round(min(test_results$all_rmse), 3), "\n")
cat("Worst RMSE:", round(max(test_results$all_rmse), 3), "\n")
cat("Mean RMSE:", round(mean(test_results$all_rmse), 3), "\n")
cat("SD of RMSE:", round(sd(test_results$all_rmse), 3), "\n")

# Show search space evolution
cat("\n=== Search Space Evolution ===\n")
if (length(test_results$search_history) > 0) {
  for (i in seq_along(test_results$search_history)) {
    hist_item <- test_results$search_history[[i]]
    cat("Iteration", i, ":\n")
    cat("  Simulations:", hist_item$n_sims, "\n")
    cat("  Catchability SD:", round(hist_item$catchability_sd, 3), "\n")
    cat("  Abundance SD:", round(hist_item$abundance_sd, 3), "\n")
    cat("  Best RMSE so far:", round(hist_item$best_overall_rmse, 3), "\n")
  }
}

# Show species-specific RMSE for best simulation
cat("\n=== Species-Specific RMSE (Best Simulation) ===\n")
best_idx <- test_results$optimal$optimal_index
if (!is.null(best_idx) && best_idx <= length(test_results$species_rmse)) {
  best_species_rmse <- test_results$species_rmse[[best_idx]]
  for (sp in names(best_species_rmse)) {
    if (!is.na(best_species_rmse[[sp]])) {
      cat("  ", sp, ":", round(best_species_rmse[[sp]], 3), "\n")
    }
  }
}

# Create simple visualization of RMSE evolution
cat("\n=== Creating Visualizations ===\n")

# Plot 1: RMSE values over simulations
df_rmse <- data.frame(
  simulation = 1:length(test_results$all_rmse),
  rmse = test_results$all_rmse
)

p1 <- ggplot(df_rmse, aes(x = simulation, y = rmse)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", se = TRUE, color = "darkred") +
  geom_hline(yintercept = min(test_results$all_rmse), 
             linetype = "dashed", color = "green") +
  theme_bw() +
  labs(title = "RMSE Evolution During Adaptive Search",
       subtitle = "Test run with 100 simulations",
       x = "Simulation Number",
       y = "RMSE") +
  annotate("text", x = length(test_results$all_rmse) * 0.8, 
           y = min(test_results$all_rmse) * 1.05,
           label = paste("Best RMSE:", round(min(test_results$all_rmse), 3)),
           color = "green")

print(p1)
ggsave("test_rmse_evolution.png", p1, width = 10, height = 6, dpi = 150)

# Plot 2: Search space refinement
if (length(test_results$search_history) > 0) {
  df_search <- do.call(rbind, lapply(test_results$search_history, function(x) {
    data.frame(
      n_sims = x$n_sims,
      catchability_sd = x$catchability_sd,
      abundance_sd = x$abundance_sd,
      best_rmse = x$best_overall_rmse
    )
  }))
  
  p2 <- ggplot(df_search, aes(x = n_sims)) +
    geom_line(aes(y = catchability_sd, color = "Catchability SD"), size = 1) +
    geom_line(aes(y = abundance_sd, color = "Abundance SD"), size = 1) +
    geom_point(aes(y = catchability_sd), color = "darkgreen", size = 3) +
    geom_point(aes(y = abundance_sd), color = "darkorange", size = 3) +
    scale_color_manual(values = c("Catchability SD" = "darkgreen",
                                 "Abundance SD" = "darkorange")) +
    theme_bw() +
    labs(title = "Search Space Refinement",
         subtitle = "Adaptive narrowing of parameter uncertainty",
         x = "Number of Simulations",
         y = "Standard Deviation",
         color = "Parameter") +
    theme(legend.position = "bottom")
  
  print(p2)
  ggsave("test_search_refinement.png", p2, width = 10, height = 6, dpi = 150)
}

# Plot 3: Compare initial vs final parameter distributions
if (length(test_results$parameters) >= 20) {
  # Get first 10 and last 10 simulations
  first_10 <- test_results$parameters[1:10]
  last_10 <- test_results$parameters[(length(test_results$parameters)-9):length(test_results$parameters)]
  
  df_params <- rbind(
    data.frame(
      phase = "Initial",
      catchability = sapply(first_10, function(x) mean(x$catchability, na.rm = TRUE)),
      abundance = sapply(first_10, function(x) mean(x$marine_mammal_scaling, na.rm = TRUE))
    ),
    data.frame(
      phase = "Final",
      catchability = sapply(last_10, function(x) mean(x$catchability, na.rm = TRUE)),
      abundance = sapply(last_10, function(x) mean(x$marine_mammal_scaling, na.rm = TRUE))
    )
  )
  
  p3 <- ggplot(df_params, aes(x = catchability, y = abundance, color = phase)) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_manual(values = c("Initial" = "red", "Final" = "blue")) +
    theme_bw() +
    labs(title = "Parameter Space: Initial vs Final Exploration",
         subtitle = "Comparing first 10 vs last 10 simulations",
         x = "Mean Catchability",
         y = "Mean Abundance Scaling",
         color = "Phase") +
    theme(legend.position = "bottom")
  
  print(p3)
  ggsave("test_parameter_evolution.png", p3, width = 8, height = 6, dpi = 150)
}

cat("\n=== Test Complete ===\n")
cat("Results saved to: test_adaptive_results.RDS\n")
cat("Plots saved to: test_rmse_evolution.png, test_search_refinement.png, test_parameter_evolution.png\n")

# Compare with standard approach if we have time
cat("\n=== Quick Comparison with Standard Approach ===\n")

# Function for standard (non-adaptive) approach
run_standard_test <- function(params, n_sims, sd_catch, sd_abund) {
  rmse_values <- numeric(n_sims)
  
  for (i in 1:n_sims) {
    # Randomize parameters
    rand_params <- params
    
    # Randomize catchability
    gear_df <- gear_params(rand_params)
    if(nrow(gear_df) > 0) {
      for(j in 1:nrow(gear_df)) {
        if(gear_df$catchability[j] > 0) {
          noise <- rnorm(1, mean = 0, sd = sd_catch)
          gear_df$catchability[j] <- gear_df$catchability[j] * exp(noise)
          gear_df$catchability[j] <- min(1, max(0, gear_df$catchability[j]))
        }
      }
      gear_params(rand_params) <- gear_df
    }
    
    # Randomize marine mammal abundance
    rand_params <- randomize_marine_mammal_abundance(rand_params, sd = sd_abund)
    
    # Quick steady state
    rand_params <- steady(rand_params, tol = 0.01, t_max = 100)
    
    # Quick simulation
    sim <- project(rand_params, 
                  t_start = 1841,
                  t_max = 170,
                  effort = combined_effort_array)
    
    # Calculate RMSE
    rmse_result <- calculate_yield_rmse(sim, yield_ts_tidy,
                                       species_list = c("antarctic krill", "minke whales", "toothfishes"),
                                       year_range = c(1961, 2010))
    rmse_values[i] <- rmse_result$total_rmse
  }
  
  return(rmse_values)
}

# Run small standard test for comparison
cat("Running standard approach with fixed parameters (25 simulations)...\n")
standard_rmse <- run_standard_test(params_test, n_sims = 25, 
                                  sd_catch = 0.3, sd_abund = 0.4)

cat("\nComparison Results:\n")
cat("Adaptive approach (100 sims):\n")
cat("  Best RMSE:", round(min(test_results$all_rmse), 3), "\n")
cat("  Mean RMSE:", round(mean(test_results$all_rmse), 3), "\n")
cat("Standard approach (25 sims):\n")
cat("  Best RMSE:", round(min(standard_rmse), 3), "\n")
cat("  Mean RMSE:", round(mean(standard_rmse), 3), "\n")

# Create comparison plot
df_compare <- data.frame(
  rmse = c(test_results$all_rmse, standard_rmse),
  method = c(rep("Adaptive (100)", length(test_results$all_rmse)),
            rep("Standard (25)", length(standard_rmse)))
)

p_compare <- ggplot(df_compare, aes(x = method, y = rmse, fill = method)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.3) +
  scale_fill_manual(values = c("Adaptive (100)" = "darkgreen", 
                               "Standard (25)" = "steelblue")) +
  theme_bw() +
  labs(title = "Adaptive vs Standard Approach Comparison",
       subtitle = "Adaptive refines search space for better results",
       x = "Method",
       y = "RMSE") +
  theme(legend.position = "none")

print(p_compare)
ggsave("test_method_comparison.png", p_compare, width = 8, height = 6, dpi = 150)

cat("\n=== All Tests Complete ===\n")
cat("The adaptive approach shows how the search space is refined over time,\n")
cat("focusing computational effort on promising parameter regions.\n")
cat("This is particularly useful for species with poor initial fits.\n")