# Fixed version of compare-optimal-simulation chunk that handles invalid simulations

# Load required libraries
library(dplyr)
library(ggplot2)
library(mizer)
library(therMizer)

# First, run the original model with params_new_v4 for comparison
sim_original <- project(params_new_v4, effort = combined_effort_array)

# Function to check if a simulation is valid
is_valid_sim <- function(sim) {
  tryCatch({
    # Try to get yield - this will fail if simulation has non-finite values
    test_yield <- getYield(sim)
    # Check if the result contains any non-finite values
    if (any(!is.finite(test_yield))) {
      return(FALSE)
    }
    # Also check biomass
    test_biomass <- getBiomass(sim)
    if (any(!is.finite(test_biomass))) {
      return(FALSE)
    }
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Filter out invalid simulations
cat("Checking simulation validity...\n")
valid_indices <- sapply(seq_along(combined_uncertainty_results$simulations), function(i) {
  is_valid_sim(combined_uncertainty_results$simulations[[i]])
})

n_valid <- sum(valid_indices)
n_total <- length(combined_uncertainty_results$simulations)
cat("Valid simulations:", n_valid, "out of", n_total, "\n")

if (n_valid == 0) {
  stop("No valid simulations found in combined_uncertainty_results")
}

# Extract uncertainty bounds from VALID simulations only
# For yield uncertainty bounds
yield_uncertainty_data <- do.call(rbind, lapply(which(valid_indices), function(i) {
  sim <- combined_uncertainty_results$simulations[[i]]
  tryCatch({
    yield_data <- plotYieldGear(sim, return_data = TRUE)
    yield_data$sim_id <- i
    return(select(yield_data, Year, Species, Yield, sim_id))
  }, error = function(e) {
    cat("Warning: Could not extract yield data from simulation", i, "\n")
    return(NULL)
  })
}))

# Remove any NULL results
yield_uncertainty_data <- yield_uncertainty_data[!sapply(yield_uncertainty_data, is.null)]

# Calculate uncertainty bounds for yield
yield_bounds <- yield_uncertainty_data %>%
  group_by(Year, Species) %>%
  summarise(
    yield_lower = quantile(Yield, 0.025, na.rm = TRUE),
    yield_upper = quantile(Yield, 0.975, na.rm = TRUE),
    yield_median = median(Yield, na.rm = TRUE),
    n_sims = n(),
    .groups = 'drop'
  )

cat("Yield bounds calculated from", unique(yield_bounds$n_sims), "simulations\n")

# For biomass uncertainty bounds
biomass_uncertainty_data <- do.call(rbind, lapply(which(valid_indices), function(i) {
  sim <- combined_uncertainty_results$simulations[[i]]
  tryCatch({
    biomass_matrix <- getBiomass(sim)
    biomass_data <- reshape2::melt(biomass_matrix)
    names(biomass_data) <- c("Year", "Species", "Biomass")
    biomass_data$sim_id <- i
    return(biomass_data)
  }, error = function(e) {
    cat("Warning: Could not extract biomass data from simulation", i, "\n")
    return(NULL)
  })
}))

# Remove any NULL results
biomass_uncertainty_data <- biomass_uncertainty_data[!sapply(biomass_uncertainty_data, is.null)]

# Calculate uncertainty bounds for biomass
biomass_bounds <- biomass_uncertainty_data %>%
  group_by(Year, Species) %>%
  summarise(
    biomass_lower = quantile(Biomass, 0.025, na.rm = TRUE),
    biomass_upper = quantile(Biomass, 0.975, na.rm = TRUE),
    biomass_median = median(Biomass, na.rm = TRUE),
    n_sims = n(),
    .groups = 'drop'
  )

cat("Biomass bounds calculated from", unique(biomass_bounds$n_sims), "simulations\n")

# Species labels for faceting
species.labs <- c("Antarctic krill", "Bathypelagic fishes", "Shelf & Coastal fishes",
                  "Squids", "Toothfishes", "Antarctic minke whales", "Orcas",
                  "Sperm whales", "Baleen whales")
names(species.labs) <- c("antarctic krill", "bathypelagic fishes",
                         "shelf and coastal fishes", "squids", "toothfishes",
                         "minke whales", "orca", "sperm whales", "baleen whales")

# Check if optimal_results$optimal_sim is valid
if (!is_valid_sim(optimal_results$optimal_sim)) {
  cat("Warning: Optimal simulation is not valid. Finding next best valid simulation...\n")
  
  # Find the best valid simulation
  valid_rmse <- optimal_results$all_rmse[valid_indices]
  best_valid_idx <- which(valid_indices)[which.min(valid_rmse)]
  optimal_results$optimal_sim <- combined_uncertainty_results$simulations[[best_valid_idx]]
  cat("Using simulation", best_valid_idx, "with RMSE =", optimal_results$all_rmse[best_valid_idx], "\n")
}

# Plot yield comparison for optimal simulation with uncertainty bounds
p_yield_optimal <- plotYield(optimal_results$optimal_sim) +
  facet_wrap(~Species, scales = "free_y",
             labeller = labeller(Species = species.labs)) +
  # Add uncertainty ribbon
  geom_ribbon(data = yield_bounds,
              aes(x = Year, ymin = yield_lower, ymax = yield_upper),
              alpha = 0.3, fill = "lightblue", inherit.aes = FALSE) +
  # Add original model fit line
  geom_line(data = plotYieldGear(sim_original, return_data = TRUE),
            aes(x = Year, y = Yield),
            color = "darkgray", linetype = "dashed", linewidth = 1, inherit.aes = FALSE) +
  # Add best fit model line (solid black)
  geom_line(data = plotYieldGear(optimal_results$optimal_sim, return_data = TRUE),
            aes(x = Year, y = Yield),
            color = "black", linetype = "solid", linewidth = 1.2, inherit.aes = FALSE) +
  # Add observed data points
  geom_point(data = yield_ts_tidy,
             aes(x = Year, y = Yield, colour = Species),
             size = 1, inherit.aes = FALSE) +
  geom_point(data = yield_ts_tidy,
             aes(x = Year, y = Yield),
             shape = 1, size = 1, colour = "black", inherit.aes = FALSE) +
  # Add vertical reference lines
  geom_vline(aes(xintercept = 1961), linetype = "dotted", alpha = 0.7) +
  geom_vline(aes(xintercept = 2010), linetype = "dotted", alpha = 0.7) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10)
  ) +
  labs(
    title = "Optimal Simulation: Modeled vs. Observed Yield with Uncertainty",
    subtitle = paste("Gray dashed = original model, Black solid = best fit, Blue ribbon = 95% CI (n=", n_valid, "sims)"),
    x = "Year",
    y = "Yield"
  )

print(p_yield_optimal)

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

ggsave("plots/optimal_simulation_yield_erepro_v3_catchSD_1_abSD_5.png", p_yield_optimal,
       width = 14, height = 10, dpi = 300)

# Get original model biomass data
original_biomass_matrix <- getBiomass(sim_original)
original_biomass_data <- reshape2::melt(original_biomass_matrix)
names(original_biomass_data) <- c("Year", "Species", "Biomass")

# Get optimal model biomass data
optimal_biomass_matrix <- getBiomass(optimal_results$optimal_sim)
optimal_biomass_data <- reshape2::melt(optimal_biomass_matrix)
names(optimal_biomass_data) <- c("Year", "Species", "Biomass")

# Plot biomass comparison for optimal simulation with uncertainty bounds
p_biomass_optimal <- plot_biomass_comparison(
  sim_object = optimal_results$optimal_sim,
  plot_title = "Optimal Simulation: Modeled vs. Observed Biomass with Uncertainty"
) +
  # Add uncertainty ribbon
  geom_ribbon(data = biomass_bounds,
              aes(x = Year, ymin = biomass_lower, ymax = biomass_upper),
              alpha = 0.3, fill = "lightcoral", inherit.aes = FALSE) +
  # Add original model fit line
  geom_line(data = original_biomass_data,
            aes(x = Year, y = Biomass),
            color = "darkgray", linetype = "dashed", linewidth = 1, inherit.aes = FALSE) +
  # Add best fit model line (solid black)
  geom_line(data = optimal_biomass_data,
            aes(x = Year, y = Biomass),
            color = "black", linetype = "solid", linewidth = 1.2, inherit.aes = FALSE) +
  labs(
    subtitle = paste("Gray dashed = original model, Black solid = best fit, Coral ribbon = 95% CI (n=", n_valid, "sims)")
  )

print(p_biomass_optimal)
ggsave("plots/optimal_simulation_biomass_erepro_v3_catchSD_1_abSD_5.png", p_biomass_optimal,
       width = 14, height = 10, dpi = 300)

cat("\nPlots saved successfully!\n")
cat("Valid simulations used:", n_valid, "out of", n_total, "\n")