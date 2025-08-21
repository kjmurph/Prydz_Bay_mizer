# Helper functions

plot_biomass_comparison <- function(sim_object, 
                                    obs_biomass_data = NULL,
                                    obs_years = 2010:2020,
                                    uncertainty = 0.1,
                                    species_labels = NULL,
                                    plot_title = "Modeled vs. Observed Biomass",
                                    plot_subtitle = NULL) {
  
  # Required libraries
  require(ggplot2)
  require(dplyr)
  require(reshape2)
  require(tidyr)
  
  # Extract modeled biomass time series
  mod_biomass_matrix <- getBiomass(sim_object)
  
  # Convert matrix to data frame
  df_mod_biomass <- mod_biomass_matrix %>% 
    melt()
  names(df_mod_biomass) <- c("Year", "Species", "Biomass")
  
  # Default observed biomass data if not provided
  if (is.null(obs_biomass_data)) {
    obs_biomass_data <- data.frame(
      Species = c("mesozooplankton","other krill","other macrozooplankton",
                  "antarctic krill","salps","mesopelagic fishes",
                  "bathypelagic fishes","shelf and coastal fishes","flying birds",
                  "small divers","squids","toothfishes","leopard seals",
                  "medium divers","large divers","minke whales","orca",
                  "sperm whales","baleen whales"),
      ObsBiomass = c(1.297420e+13, 2.801248e+12, 1.474341e+13, 5.897364e+12, 
                     9.612703e+11, 1.769209e+12, 1.769209e+12, 4.027900e+12, 
                     4.423023e+09, 2.358946e+10, 2.211512e+11, 1.105756e+12, 
                     2.948682e+09, 3.907004e+11, 1.621775e+10, 2.064077e+10, 
                     8.846046e+09, 1.621775e+10, 1.872413e+11)
    )
  }
  
  # Add uncertainty bounds
  obs_biomass <- obs_biomass_data %>%
    mutate(
      Lower = ObsBiomass * (1 - uncertainty),
      Upper = ObsBiomass * (1 + uncertainty)
    )
  
  # Create time series version for observation years
  obs_biomass_ts <- obs_biomass %>%
    crossing(Year = obs_years)
  
  # Define species order (use provided labels or default order)
  if (is.null(species_labels)) {
    species_order <- c("mesozooplankton", "other krill", "other macrozooplankton", 
                       "antarctic krill", "salps", "mesopelagic fishes", 
                       "bathypelagic fishes", "shelf and coastal fishes", 
                       "flying birds", "small divers", "squids", "toothfishes", 
                       "leopard seals", "medium divers", "large divers", 
                       "minke whales", "orca", "sperm whales", "baleen whales")
    species.labs <- species_order  # Use species names as labels
  } else {
    species_order <- names(species_labels)
    species.labs <- species_labels
  }
  
  # Convert Species to factors with specified order
  obs_biomass_ts$Species <- factor(obs_biomass_ts$Species, levels = species_order)
  df_mod_biomass$Species <- factor(df_mod_biomass$Species, levels = species_order)
  
  # Create subtitle if not provided
  if (is.null(plot_subtitle)) {
    plot_subtitle <- paste0("Observed values (", min(obs_years), "-", max(obs_years), 
                            ") shown as points with ±", uncertainty*100, "% uncertainty (shaded)")
  }
  
  # Create the plot
  p_biomass <- ggplot() +
    # Plot model biomass line
    geom_line(data = df_mod_biomass, 
              aes(x = Year, y = Biomass, color = Species), linewidth = 1.5) +
    # Plot observed biomass as points
    geom_point(data = obs_biomass_ts, 
               aes(x = Year, y = ObsBiomass, color = Species), 
               size = 0.8) +
    geom_point(data = obs_biomass_ts, 
               aes(x = Year, y = ObsBiomass), 
               shape = 1, size = 0.8, colour = "black") +
    # Add uncertainty ribbon
    geom_ribbon(data = obs_biomass_ts,
                aes(x = Year, ymin = Lower, ymax = Upper),
                alpha = 0.2) +
    # Facet by species with free y scales
    facet_wrap(~Species, scales = "free_y",
               labeller = labeller(Species = species.labs)) +
    # Theme and labels
    theme_bw() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 12)
    ) +
    labs(
      x = "Year",
      y = "Biomass",
      title = plot_title,
      subtitle = plot_subtitle
    )
  
  return(p_biomass)
}

# Example usage:
# Basic usage with default parameters
# p1 <- plot_biomass_comparison(sim_1841_2010_fishing_only)

# Usage with custom parameters
# p2 <- plot_biomass_comparison(
#   sim_object = sim_1841_2010_fishing_only,
#   obs_years = 2015:2020,
#   uncertainty = 0.15,
#   plot_title = "Custom Biomass Comparison",
#   plot_subtitle = "Custom subtitle here"
# )

# Usage with custom observed data
# custom_obs_data <- data.frame(
#   Species = c("species1", "species2"),
#   ObsBiomass = c(1e12, 2e12)
# )
# p3 <- plot_biomass_comparison(
#   sim_object = sim_1841_2010_fishing_only,
#   obs_biomass_data = custom_obs_data
# )


plot_yield_comparison <- function(sim_object,
                                  yield_obs_data,
                                  free_y_scale = TRUE,
                                  species_labels = NULL,
                                  vertical_lines = c(1961, 2010),
                                  plot_title = "Modeled vs. Observed Yield",
                                  show_legend = FALSE,
                                  point_size = 1) {
  
  # Required libraries
  require(ggplot2)
  require(dplyr)
  
  # Default species labels if not provided
  if (is.null(species_labels)) {
    species_labels <- c("Antarctic krill", "Bathypelagic fishes", 
                        "Shelf & Coastal fishes", "Squids", "Toothfishes", 
                        "Antarctic minke whales", "Orcas", "Sperm whales", 
                        "Baleen whales")
    names(species_labels) <- c("antarctic krill", "bathypelagic fishes", 
                               "shelf and coastal fishes", "squids", "toothfishes", 
                               "minke whales", "orca", "sperm whales", "baleen whales")
  }
  
  # Create base plot using plotYield
  p_yield <- plotYield(sim_object)
  
  # Set up facet wrap based on y-axis scale preference
  facet_scales <- if (free_y_scale) "free_y" else "fixed"
  
  p_yield <- p_yield + 
    facet_wrap(~Species, scales = facet_scales,
               labeller = labeller(Species = species_labels))
  
  # Add observed data points and styling
  p_yield <- p_yield +
    labs(fill = "Functional group", title = plot_title) +
    geom_point(data = yield_obs_data, 
               aes(x = Year, y = Yield, colour = Species), 
               size = point_size) +
    geom_point(data = yield_obs_data, 
               aes(x = Year, y = Yield), 
               shape = 1, size = point_size, colour = "black") +
    theme_bw() +
    theme(
      legend.title = element_text(size = 3), 
      legend.text = element_text(size = 3),
      legend.position = if(show_legend) "bottom" else "none"
    )
  
  # Add vertical lines if specified
  if (!is.null(vertical_lines) && length(vertical_lines) > 0) {
    for (line_year in vertical_lines) {
      p_yield <- p_yield + 
        geom_vline(aes(xintercept = line_year), linetype = "dashed")
    }
  }
  
  return(p_yield)
}

# Example usage:
# Basic usage with free y-axis (default)
# p_yield_free <- plot_yield_comparison(
#   sim_object = sim_1841_2010_fishing_only,
#   yield_obs_data = yield_ts_tidy
# )

# Usage with fixed y-axis scale
# p_yield_fixed <- plot_yield_comparison(
#   sim_object = sim_1841_2010_fishing_only,
#   yield_obs_data = yield_ts_tidy,
#   free_y_scale = FALSE
# )

# Usage with custom parameters
# p_yield_custom <- plot_yield_comparison(
#   sim_object = sim_1841_2010_fishing_only,
#   yield_obs_data = yield_ts_tidy,
#   free_y_scale = TRUE,
#   vertical_lines = c(1950, 1980, 2000),
#   plot_title = "Custom Yield Analysis",
#   show_legend = TRUE,
#   point_size = 1.5
# )

# Using convenience functions
# p_free <- plot_yield_free_scale(sim_1841_2010_fishing_only, yield_ts_tidy)
# p_fixed <- plot_yield_fixed_scale(sim_1841_2010_fishing_only, yield_ts_tidy)

# Function to scale initial abundance for marine mammals
# This allows exploration of uncertainty in initial biomass conditions
scale_marine_mammal_abundance <- function(params,
                                         scaling_factors = NULL,
                                         species_to_scale = NULL) {
  
  # Default marine mammal species if not specified
  if (is.null(species_to_scale)) {
    # Based on the model, these are the marine mammal groups
    species_to_scale <- c("minke whales", "orca", "sperm whales", "baleen whales")
  }
  
  # Get species indices
  sp_params <- species_params(params)
  species_indices <- which(sp_params$species %in% species_to_scale)
  
  # If no scaling factors provided, return params unchanged
  if (is.null(scaling_factors)) {
    return(params)
  }
  
  # Apply scaling factors
  if (length(scaling_factors) == 1) {
    # Apply same scaling to all marine mammals
    params@initial_n[species_indices,] <- params@initial_n[species_indices,] * scaling_factors
  } else if (length(scaling_factors) == length(species_indices)) {
    # Apply individual scaling factors
    for (i in seq_along(species_indices)) {
      params@initial_n[species_indices[i],] <- params@initial_n[species_indices[i],] * scaling_factors[i]
    }
  } else {
    stop("scaling_factors must be either a single value or match the number of species to scale")
  }
  
  return(params)
}

# Function to randomize marine mammal abundance with uncertainty
randomize_marine_mammal_abundance <- function(params,
                                             sd = 0.2,
                                             species_to_scale = NULL) {
  
  # Default marine mammal species if not specified
  if (is.null(species_to_scale)) {
    species_to_scale <- c("minke whales", "orca", "sperm whales", "baleen whales")
  }
  
  # Get species indices
  sp_params <- species_params(params)
  species_indices <- which(sp_params$species %in% species_to_scale)
  
  # Generate random scaling factors (log-normal distribution)
  scaling_factors <- exp(rnorm(length(species_indices), mean = 0, sd = sd))
  
  # Apply scaling
  params <- scale_marine_mammal_abundance(params, scaling_factors, species_to_scale)
  
  return(params)
}

# Function to calculate RMSE between modeled and observed yield
calculate_yield_rmse <- function(sim_object,
                                yield_obs_data,
                                species_list = NULL,
                                year_range = NULL) {
  
  # Get modeled yield
  mod_yield <- getYield(sim_object)
  
  # Convert to data frame
  df_mod <- reshape2::melt(mod_yield)
  names(df_mod) <- c("Year", "Species", "Yield")
  
  # Filter by year range if specified
  if (!is.null(year_range)) {
    df_mod <- df_mod[df_mod$Year >= min(year_range) & df_mod$Year <= max(year_range), ]
    yield_obs_data <- yield_obs_data[yield_obs_data$Year >= min(year_range) &
                                     yield_obs_data$Year <= max(year_range), ]
  }
  
  # Filter by species if specified
  if (!is.null(species_list)) {
    df_mod <- df_mod[df_mod$Species %in% species_list, ]
    yield_obs_data <- yield_obs_data[yield_obs_data$Species %in% species_list, ]
  }
  
  # Merge modeled and observed data
  comparison <- merge(df_mod, yield_obs_data,
                     by = c("Year", "Species"),
                     suffixes = c("_mod", "_obs"))
  
  # Calculate RMSE
  rmse <- sqrt(mean((comparison$Yield_mod - comparison$Yield_obs)^2, na.rm = TRUE))
  
  # Also calculate RMSE by species
  rmse_by_species <- comparison %>%
    group_by(Species) %>%
    summarise(RMSE = sqrt(mean((Yield_mod - Yield_obs)^2, na.rm = TRUE)))
  
  return(list(
    total_rmse = rmse,
    rmse_by_species = rmse_by_species,
    comparison_data = comparison
  ))
}

# Function to run simulations with combined catchability and abundance uncertainty
run_combined_uncertainty_sims <- function(params,
                                         n_sims = 10,
                                         catchability_sd = 0.1,
                                         abundance_sd = 0.2,
                                         marine_mammal_species = NULL,
                                         effort_scen,
                                         tol = 0.01,
                                         t_max = 500,
                                         spinup_years = 118,
                                         t_start = 1841,
                                         sim_years = 170) {
  
  # List to store results
  sim_results <- list()
  param_combinations <- list()
  
  # Run simulations
  for(i in 1:n_sims) {
    
    # Create copy of params
    rand_params <- params
    
    # Randomize catchability for fished species
    gear_df <- gear_params(rand_params)
    if(nrow(gear_df) > 0) {
      for(j in 1:nrow(gear_df)) {
        if(gear_df$catchability[j] > 0) {
          noise <- rnorm(1, mean = 0, sd = catchability_sd)
          gear_df$catchability[j] <- gear_df$catchability[j] * exp(noise)
          gear_df$catchability[j] <- min(1, max(0, gear_df$catchability[j]))
        }
      }
      gear_params(rand_params) <- gear_df
    }
    
    # Randomize marine mammal abundance
    rand_params <- randomize_marine_mammal_abundance(rand_params,
                                                    sd = abundance_sd,
                                                    species_to_scale = marine_mammal_species)
    
    # Store parameter combination
    param_combinations[[i]] <- list(
      catchability = gear_df$catchability,
      initial_n_mammals = rand_params@initial_n[which(species_params(rand_params)$species %in%
                                                      c("minke whales", "orca", "sperm whales", "baleen whales")),]
    )
    
    # Run to steady state
    rand_params <- steady(rand_params, tol = tol, t_max = t_max)
    
    # Run spinup if specified
    if (spinup_years > 0) {
      sim_spinup <- project(rand_params,
                           t_start = t_start,
                           t_max = spinup_years,
                           effort = 0)
      
      # Run main simulation with spinup initial conditions
      sim <- project(rand_params,
                    initial_n = sim_spinup@n[spinup_years,,],
                    t_start = t_start,
                    t_max = sim_years,
                    effort = effort_scen)
    } else {
      # Run simulation without spinup
      sim <- project(rand_params,
                    t_start = t_start,
                    t_max = sim_years,
                    effort = effort_scen)
    }
    
    # Store results
    sim_results[[i]] <- sim
  }
  
  return(list(
    simulations = sim_results,
    parameters = param_combinations
  ))
}

# Function to find optimal parameter combination based on RMSE
find_optimal_parameters <- function(sim_results_list,
                                   yield_obs_data,
                                   species_list = NULL,
                                   year_range = NULL) {
  
  # Calculate RMSE for each simulation
  rmse_values <- numeric(length(sim_results_list$simulations))
  
  for(i in seq_along(sim_results_list$simulations)) {
    rmse_result <- calculate_yield_rmse(sim_results_list$simulations[[i]],
                                       yield_obs_data,
                                       species_list = species_list,
                                       year_range = year_range)
    rmse_values[i] <- rmse_result$total_rmse
  }
  
  # Find optimal (minimum RMSE)
  optimal_idx <- which.min(rmse_values)
  
  return(list(
    optimal_index = optimal_idx,
    optimal_rmse = rmse_values[optimal_idx],
    optimal_sim = sim_results_list$simulations[[optimal_idx]],
    optimal_params = sim_results_list$parameters[[optimal_idx]],
    all_rmse = rmse_values
  ))
}

# Function to plot RMSE distribution from uncertainty analysis
plot_rmse_distribution <- function(rmse_values,
                                  optimal_idx = NULL,
                                  plot_title = "RMSE Distribution from Parameter Uncertainty") {
  
  require(ggplot2)
  
  df <- data.frame(
    Simulation = 1:length(rmse_values),
    RMSE = rmse_values
  )
  
  p <- ggplot(df, aes(x = RMSE)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "black") +
    theme_bw() +
    labs(
      title = plot_title,
      x = "RMSE (Root Mean Square Error)",
      y = "Frequency"
    )
  
  # Add vertical line for optimal value if provided
  if (!is.null(optimal_idx)) {
    p <- p +
      geom_vline(xintercept = rmse_values[optimal_idx],
                color = "red", linetype = "dashed", linewidth = 1) +
      annotate("text",
              x = rmse_values[optimal_idx],
              y = max(table(cut(rmse_values, 30))) * 0.9,
              label = paste("Optimal RMSE =", round(rmse_values[optimal_idx], 2)),
              hjust = -0.1, color = "red")
  }
  
  return(p)
}