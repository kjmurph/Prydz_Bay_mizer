# Fixed plotting code for optimal simulation results
# This version handles zero/near-zero values that cause issues with log scales

library(ggplot2)
library(dplyr)
library(mizer)
library(therMizer)

# Function to safely plot yield with log scale handling
safe_plot_yield <- function(sim_object, 
                           yield_bounds = NULL,
                           sim_original = NULL,
                           yield_obs_data = NULL,
                           species_labels = NULL,
                           use_log_scale = FALSE,
                           min_value_threshold = 1e-10) {
  
  # Get yield data from optimal simulation
  yield_data <- plotYieldGear(sim_object, return_data = TRUE)
  
  # Replace zero or very small values with threshold for log scale
  if (use_log_scale) {
    yield_data$Yield[yield_data$Yield < min_value_threshold] <- min_value_threshold
  }
  
  # Create base plot without log scale initially
  p <- ggplot(yield_data, aes(x = Year, y = Yield, color = Species)) +
    geom_line(linewidth = 1.2) +
    theme_bw()
  
  # Add faceting with species labels if provided
  if (!is.null(species_labels)) {
    p <- p + facet_wrap(~Species, scales = "free_y",
                       labeller = labeller(Species = species_labels))
  } else {
    p <- p + facet_wrap(~Species, scales = "free_y")
  }
  
  # Add uncertainty bounds if provided
  if (!is.null(yield_bounds)) {
    # Clean bounds data
    yield_bounds_clean <- yield_bounds
    if (use_log_scale) {
      yield_bounds_clean$yield_lower[yield_bounds_clean$yield_lower < min_value_threshold] <- min_value_threshold
      yield_bounds_clean$yield_upper[yield_bounds_clean$yield_upper < min_value_threshold] <- min_value_threshold
    }
    
    p <- p + geom_ribbon(data = yield_bounds_clean,
                        aes(x = Year, ymin = yield_lower, ymax = yield_upper),
                        alpha = 0.3, fill = "lightblue", inherit.aes = FALSE)
  }
  
  # Add original model line if provided
  if (!is.null(sim_original)) {
    original_yield <- plotYieldGear(sim_original, return_data = TRUE)
    if (use_log_scale) {
      original_yield$Yield[original_yield$Yield < min_value_threshold] <- min_value_threshold
    }
    
    p <- p + geom_line(data = original_yield,
                      aes(x = Year, y = Yield),
                      color = "darkgray", linetype = "dashed", 
                      linewidth = 1, inherit.aes = FALSE)
  }
  
  # Add observed data points if provided
  if (!is.null(yield_obs_data)) {
    obs_data_clean <- yield_obs_data
    if (use_log_scale) {
      obs_data_clean$Yield[obs_data_clean$Yield < min_value_threshold] <- min_value_threshold
    }
    
    p <- p + 
      geom_point(data = obs_data_clean,
                aes(x = Year, y = Yield, colour = Species),
                size = 1, inherit.aes = FALSE) +
      geom_point(data = obs_data_clean,
                aes(x = Year, y = Yield),
                shape = 1, size = 1, colour = "black", inherit.aes = FALSE)
  }
  
  # Add reference lines
  p <- p +
    geom_vline(aes(xintercept = 1961), linetype = "dotted", alpha = 0.7) +
    geom_vline(aes(xintercept = 2010), linetype = "dotted", alpha = 0.7)
  
  # Apply log scale if requested (after cleaning data)
  if (use_log_scale) {
    p <- p + scale_y_log10()
  }
  
  # Final styling
  p <- p +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 10)
    ) +
    labs(
      title = "Optimal Simulation: Modeled vs. Observed Yield with Uncertainty",
      subtitle = "Gray dashed = original model, Black solid = best fit, Blue ribbon = 95% CI",
      x = "Year",
      y = "Yield"
    )
  
  return(p)
}

# Main plotting code
cat("Creating yield comparison plot...\n")

# Species labels for faceting
species.labs <- c("Antarctic krill", "Bathypelagic fishes", "Shelf & Coastal fishes",
                  "Squids", "Toothfishes", "Antarctic minke whales", "Orcas",
                  "Sperm whales", "Baleen whales")
names(species.labs) <- c("antarctic krill", "bathypelagic fishes",
                         "shelf and coastal fishes", "squids", "toothfishes",
                         "minke whales", "orca", "sperm whales", "baleen whales")

# Create yield plot using the safe function
p_yield_optimal <- safe_plot_yield(
  sim_object = optimal_results$optimal_sim,
  yield_bounds = yield_bounds,
  sim_original = sim_original,
  yield_obs_data = yield_ts_tidy,
  species_labels = species.labs,
  use_log_scale = FALSE  # Set to FALSE to avoid log scale issues
)

# Try to display and save the plot
tryCatch({
  print(p_yield_optimal)
  
  # Create plots directory if it doesn't exist
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  ggsave("plots/optimal_simulation_yield_fixed.png", p_yield_optimal,
         width = 14, height = 10, dpi = 300)
  cat("Yield plot saved successfully!\n")
}, error = function(e) {
  cat("Error in yield plot:", e$message, "\n")
  cat("Attempting alternative plot without log scale...\n")
  
  # Alternative simple plot
  p_yield_simple <- ggplot() +
    geom_line(data = plotYieldGear(optimal_results$optimal_sim, return_data = TRUE),
              aes(x = Year, y = Yield, color = Species), linewidth = 1.2) +
    facet_wrap(~Species, scales = "free_y") +
    theme_bw() +
    labs(title = "Optimal Simulation Yield",
         x = "Year", y = "Yield") +
    theme(legend.position = "none")
  
  print(p_yield_simple)
  ggsave("plots/optimal_simulation_yield_simple.png", p_yield_simple,
         width = 14, height = 10, dpi = 300)
  cat("Simple yield plot saved instead.\n")
})

# Biomass plot (usually doesn't have the same log scale issues)
cat("\nCreating biomass comparison plot...\n")

# Get biomass data
original_biomass_matrix <- getBiomass(sim_original)
original_biomass_data <- reshape2::melt(original_biomass_matrix)
names(original_biomass_data) <- c("Year", "Species", "Biomass")

optimal_biomass_matrix <- getBiomass(optimal_results$optimal_sim)
optimal_biomass_data <- reshape2::melt(optimal_biomass_matrix)
names(optimal_biomass_data) <- c("Year", "Species", "Biomass")

# Create biomass plot
tryCatch({
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
      subtitle = "Gray dashed = original model, Black solid = best fit, Coral ribbon = 95% CI"
    )
  
  print(p_biomass_optimal)
  ggsave("plots/optimal_simulation_biomass_fixed.png", p_biomass_optimal,
         width = 14, height = 10, dpi = 300)
  cat("Biomass plot saved successfully!\n")
  
}, error = function(e) {
  cat("Error in biomass plot:", e$message, "\n")
  cat("Creating simple biomass plot instead...\n")
  
  # Simple biomass plot
  p_biomass_simple <- ggplot(optimal_biomass_data, aes(x = Year, y = Biomass, color = Species)) +
    geom_line(linewidth = 1.2) +
    facet_wrap(~Species, scales = "free_y") +
    theme_bw() +
    labs(title = "Optimal Simulation Biomass",
         x = "Year", y = "Biomass") +
    theme(legend.position = "none")
  
  print(p_biomass_simple)
  ggsave("plots/optimal_simulation_biomass_simple.png", p_biomass_simple,
         width = 14, height = 10, dpi = 300)
  cat("Simple biomass plot saved instead.\n")
})

cat("\nPlotting complete! Check the 'plots' directory for output files.\n")

# Additional diagnostic information
cat("\n=== Diagnostic Information ===\n")

# Check for zero or near-zero values in yield data
yield_data_check <- plotYieldGear(optimal_results$optimal_sim, return_data = TRUE)
zero_yield_species <- unique(yield_data_check$Species[yield_data_check$Yield < 1e-10])
if (length(zero_yield_species) > 0) {
  cat("Species with zero or near-zero yield values:\n")
  print(zero_yield_species)
  cat("\nThese species may cause issues with log-scale plots.\n")
}

# Summary of yield bounds
cat("\nYield bounds summary:\n")
yield_summary <- yield_bounds %>%
  group_by(Species) %>%
  summarise(
    min_lower = min(yield_lower, na.rm = TRUE),
    max_upper = max(yield_upper, na.rm = TRUE),
    has_zeros = any(yield_lower < 1e-10 | yield_upper < 1e-10)
  )
print(yield_summary)

cat("\n=== Analysis Complete ===\n")