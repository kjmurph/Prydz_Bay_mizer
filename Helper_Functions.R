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
  
  # Calculate RMSE - handle potential NA or Inf values
  if (nrow(comparison) == 0) {
    rmse <- NA
  } else {
    rmse_val <- sqrt(mean((comparison$Yield_mod - comparison$Yield_obs)^2, na.rm = TRUE))
    # Check for NaN or Inf and replace with large value
    if (is.nan(rmse_val) || is.infinite(rmse_val)) {
      rmse <- 1e10  # Large penalty value for failed simulations
    } else {
      rmse <- rmse_val
    }
  }
  
  # Also calculate RMSE by species
  rmse_by_species <- comparison %>%
    group_by(Species) %>%
    summarise(RMSE = sqrt(mean((Yield_mod - Yield_obs)^2, na.rm = TRUE))) %>%
    mutate(RMSE = ifelse(is.nan(RMSE) | is.infinite(RMSE), 1e10, RMSE))
  
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

# ============================================================================
# ADAPTIVE UNCERTAINTY ANALYSIS FUNCTIONS
# Functions for adaptive parameter optimization with RMSE checks every N simulations
# ============================================================================

# Function to calculate species-specific RMSE
calculate_species_rmse <- function(sim_object, yield_obs_data, species_name, year_range = NULL) {
  # Get modeled yield
  mod_yield <- getYield(sim_object)
  
  # Convert to data frame
  df_mod <- reshape2::melt(mod_yield)
  names(df_mod) <- c("Year", "Species", "Yield")
  
  # Filter by species
  df_mod <- df_mod[df_mod$Species == species_name, ]
  yield_obs_species <- yield_obs_data[yield_obs_data$Species == species_name, ]
  
  # Filter by year range if specified
  if (!is.null(year_range)) {
    df_mod <- df_mod[df_mod$Year >= min(year_range) & df_mod$Year <= max(year_range), ]
    yield_obs_species <- yield_obs_species[yield_obs_species$Year >= min(year_range) &
                                          yield_obs_species$Year <= max(year_range), ]
  }
  
  # Merge and calculate RMSE
  comparison <- merge(df_mod, yield_obs_species,
                     by = c("Year", "Species"),
                     suffixes = c("_mod", "_obs"))
  
  if (nrow(comparison) == 0) {
    return(NA)
  }
  
  rmse_val <- sqrt(mean((comparison$Yield_mod - comparison$Yield_obs)^2, na.rm = TRUE))
  
  # Check for NaN or Inf and replace with large value
  if (is.nan(rmse_val) || is.infinite(rmse_val)) {
    return(1e10)  # Large penalty value for failed simulations
  } else {
    return(rmse_val)
  }
}

# Function to update parameter search space based on best performers
update_search_space <- function(param_history, rmse_history, species_rmse_history,
                               current_catchability_sd, current_abundance_sd,
                               top_percent = 0.2, reduction_factor = 0.9,
                               min_sd = 0.05, max_sd = 1.0) {
  
  n_best <- max(1, floor(length(rmse_history) * top_percent))
  best_indices <- order(rmse_history)[1:n_best]
  
  # Initialize new search parameters
  new_params <- list(
    catchability_sd = current_catchability_sd,
    abundance_sd = current_abundance_sd,
    species_specific_adjustments = list()
  )
  
  # Analyze best performing parameters
  best_params <- param_history[best_indices]
  
  # Calculate mean and sd of best catchability values
  best_catchability <- sapply(best_params, function(x) mean(x$catchability, na.rm = TRUE))
  catchability_spread <- sd(best_catchability, na.rm = TRUE)
  
  # Calculate mean and sd of best abundance scaling
  best_abundance <- sapply(best_params, function(x) mean(x$marine_mammal_scaling, na.rm = TRUE))
  abundance_spread <- sd(best_abundance, na.rm = TRUE)
  
  # Adjust search space based on spread of best parameters
  if (catchability_spread < current_catchability_sd * 0.5) {
    new_params$catchability_sd <- max(min_sd, current_catchability_sd * reduction_factor)
  } else if (catchability_spread > current_catchability_sd * 1.5) {
    new_params$catchability_sd <- min(max_sd, current_catchability_sd / reduction_factor)
  }
  
  if (abundance_spread < current_abundance_sd * 0.5) {
    new_params$abundance_sd <- max(min_sd, current_abundance_sd * reduction_factor)
  } else if (abundance_spread > current_abundance_sd * 1.5) {
    new_params$abundance_sd <- min(max_sd, current_abundance_sd / reduction_factor)
  }
  
  # Analyze species-specific performance
  if (!is.null(species_rmse_history) && length(species_rmse_history) > 0) {
    species_names <- names(species_rmse_history[[1]])
    
    for (species in species_names) {
      species_rmse <- sapply(species_rmse_history[best_indices],
                             function(x) x[[species]])
      
      # Check if this species needs special attention (high RMSE)
      if (!all(is.na(species_rmse))) {
        mean_rmse <- mean(species_rmse, na.rm = TRUE)
        
        # Store species-specific adjustment recommendations
        new_params$species_specific_adjustments[[species]] <- list(
          mean_rmse = mean_rmse,
          needs_attention = mean_rmse > median(unlist(species_rmse_history), na.rm = TRUE) * 1.5
        )
      }
    }
  }
  
  # Calculate center of best parameter region for targeted search
  new_params$catchability_center <- mean(best_catchability, na.rm = TRUE)
  new_params$abundance_center <- mean(best_abundance, na.rm = TRUE)
  
  return(new_params)
}

# Helper function to run a batch of simulations
run_batch_simulations <- function(params, n_sims, catchability_sd, abundance_sd,
                                 catchability_center = 1, abundance_center = 1,
                                 marine_mammal_species, effort_scen,
                                 tol, t_max, spinup_years, t_start, sim_years,
                                 preserve_erepro, max_erepro,
                                 n_cores, parallel) {
  
  sim_results <- list()
  param_combinations <- list()
  
  # Function to run single simulation
  run_single <- function(i) {
    # Create copy of params
    rand_params <- params
    
    # Randomize catchability (with bias towards center)
    gear_df <- gear_params(rand_params)
    original_catchability <- gear_df$catchability
    
    if(nrow(gear_df) > 0) {
      for(j in 1:nrow(gear_df)) {
        if(gear_df$catchability[j] > 0) {
          # Generate biased noise
          noise <- rnorm(1, mean = log(catchability_center), sd = catchability_sd)
          gear_df$catchability[j] <- gear_df$catchability[j] * exp(noise - log(catchability_center))
          gear_df$catchability[j] <- min(1, max(0, gear_df$catchability[j]))
        }
      }
      gear_params(rand_params) <- gear_df
    }
    
    # Randomize marine mammal abundance (with bias towards center)
    sp_params <- species_params(rand_params)
    species_indices <- which(sp_params$species %in% marine_mammal_species)
    
    # Generate biased scaling factors
    scaling_factors <- exp(rnorm(length(species_indices),
                                 mean = log(abundance_center),
                                 sd = abundance_sd))
    
    for (k in seq_along(species_indices)) {
      rand_params@initial_n[species_indices[k],] <-
        rand_params@initial_n[species_indices[k],] * scaling_factors[k]
    }
    
    # Run to steady state
    if (preserve_erepro) {
      rand_params <- steady(rand_params, tol = tol, t_max = t_max, preserve = c("erepro"))
    } else {
      rand_params <- steady(rand_params, tol = tol, t_max = t_max)
      
      # Check erepro constraint
      current_erepro <- species_params(rand_params)$erepro
      if (any(current_erepro > max_erepro, na.rm = TRUE)) {
        return(NULL)  # Skip this simulation
      }
    }
    
    # Run spinup if specified
    if (spinup_years > 0) {
      sim_spinup <- project(rand_params,
                          t_start = t_start,
                          t_max = spinup_years,
                          effort = 0)
      
      sim <- project(rand_params,
                    initial_n = sim_spinup@n[spinup_years,,],
                    t_start = t_start,
                    t_max = sim_years,
                    effort = effort_scen)
    } else {
      sim <- project(rand_params,
                    t_start = t_start,
                    t_max = sim_years,
                    effort = effort_scen)
    }
    
    # Return results
    return(list(
      simulation = sim,
      params_info = list(
        catchability = gear_df$catchability,
        catchability_change = gear_df$catchability / original_catchability,
        marine_mammal_scaling = scaling_factors,
        marine_mammal_species = marine_mammal_species,
        erepro_values = species_params(rand_params)$erepro
      )
    ))
  }
  
  # Run simulations (parallel or sequential)
  if (parallel && n_cores > 1) {
    require(parallel)
    require(doParallel)
    require(foreach)
    
    # Create cluster with explicit confirmation
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    # Confirm parallel backend is registered
    if (getDoParWorkers() > 1) {
      cat("  Parallel backend confirmed:", getDoParWorkers(), "workers available\n")
    }
    
    results <- foreach(i = 1:n_sims,
                      .packages = c("mizer", "therMizer"),
                      .errorhandling = "pass") %dopar% {
      run_single(i)
    }
    
    stopCluster(cl)
  } else {
    if (parallel && n_cores <= 1) {
      cat("  Note: Parallel requested but only 1 core available, running sequentially\n")
    }
    results <- lapply(1:n_sims, run_single)
  }
  
  # Process results
  for (res in results) {
    if (!is.null(res) && !inherits(res, "error")) {
      sim_results[[length(sim_results) + 1]] <- res$simulation
      param_combinations[[length(param_combinations) + 1]] <- res$params_info
    }
  }
  
  return(list(
    simulations = sim_results,
    parameters = param_combinations
  ))
}

# Main adaptive uncertainty analysis function
run_adaptive_uncertainty_analysis <- function(params,
                                             n_total_sims = 500,
                                             check_interval = 10,  # Default to 10 for more frequent checks
                                             initial_catchability_sd = 0.3,
                                             initial_abundance_sd = 0.4,
                                             marine_mammal_species = NULL,
                                             fished_species = NULL,
                                             effort_scen,
                                             yield_obs_data,
                                             year_range = c(1961, 2010),
                                             tol = 0.01,
                                             t_max = 500,
                                             spinup_years = 118,
                                             t_start = 1841,
                                             sim_years = 170,
                                             preserve_erepro = TRUE,
                                             max_erepro = 1,
                                             convergence_threshold = 0.01,
                                             n_cores = NULL,
                                             parallel = TRUE,
                                             verbose = TRUE) {
  
  require(parallel)
  require(doParallel)
  require(foreach)
  
  # Default species if not specified
  if (is.null(marine_mammal_species)) {
    marine_mammal_species <- c("minke whales", "orca", "sperm whales", "baleen whales")
  }
  
  if (is.null(fished_species)) {
    fished_species <- c("antarctic krill", "bathypelagic fishes",
                       "shelf and coastal fishes", "squids", "toothfishes",
                       "minke whales", "orca", "sperm whales", "baleen whales")
  }
  
  # Initialize tracking variables
  all_simulations <- list()
  all_parameters <- list()
  all_rmse <- numeric()
  all_species_rmse <- list()
  convergence_history <- list()
  search_space_history <- list()
  
  # Current search parameters
  current_catchability_sd <- initial_catchability_sd
  current_abundance_sd <- initial_abundance_sd
  catchability_center <- 1.0  # Start with no bias
  abundance_center <- 1.0     # Start with no bias
  
  # Determine number of cores
  if (is.null(n_cores)) {
    n_cores <- detectCores() - 1
  }
  n_cores <- min(n_cores, detectCores())
  
  # Progress tracking
  total_completed <- 0
  n_iterations <- ceiling(n_total_sims / check_interval)
  
  # Time tracking
  start_time <- Sys.time()
  iteration_times <- list()
  
  if (verbose) {
    cat("Starting adaptive uncertainty analysis\n")
    cat("Total simulations:", n_total_sims, "\n")
    cat("Check interval:", check_interval, "\n")
    cat("Initial catchability SD:", initial_catchability_sd, "\n")
    cat("Initial abundance SD:", initial_abundance_sd, "\n")
    cat("Parallel processing:", ifelse(parallel && n_cores > 1,
                                       paste("YES (using", n_cores, "cores)"),
                                       "NO (sequential)"), "\n")
    cat("Start time:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n\n")
  }
  
  # Main adaptive loop
  for (iteration in 1:n_iterations) {
    
    # Track iteration start time
    iter_start_time <- Sys.time()
    
    # Calculate number of simulations for this batch
    n_batch <- min(check_interval, n_total_sims - total_completed)
    if (n_batch <= 0) break
    
    if (verbose) {
      cat("=== Iteration", iteration, "===\n")
      cat("Running", n_batch, "simulations\n")
      cat("Current catchability SD:", round(current_catchability_sd, 3), "\n")
      cat("Current abundance SD:", round(current_abundance_sd, 3), "\n")
    }
    
    # Run batch of simulations
    batch_results <- run_batch_simulations(
      params = params,
      n_sims = n_batch,
      catchability_sd = current_catchability_sd,
      abundance_sd = current_abundance_sd,
      catchability_center = catchability_center,
      abundance_center = abundance_center,
      marine_mammal_species = marine_mammal_species,
      effort_scen = effort_scen,
      tol = tol,
      t_max = t_max,
      spinup_years = spinup_years,
      t_start = t_start,
      sim_years = sim_years,
      preserve_erepro = preserve_erepro,
      max_erepro = max_erepro,
      n_cores = n_cores,
      parallel = parallel
    )
    
    # Calculate RMSE for each simulation in batch
    batch_rmse <- numeric(length(batch_results$simulations))
    batch_species_rmse <- list()
    
    for (i in seq_along(batch_results$simulations)) {
      # Total RMSE
      rmse_result <- calculate_yield_rmse(
        batch_results$simulations[[i]],
        yield_obs_data,
        species_list = fished_species,
        year_range = year_range
      )
      # Handle NA or invalid RMSE values
      if (is.na(rmse_result$total_rmse)) {
        batch_rmse[i] <- 1e10  # Large penalty for failed simulations
      } else {
        batch_rmse[i] <- rmse_result$total_rmse
      }
      
      # Species-specific RMSE
      species_rmse <- list()
      for (species in fished_species) {
        species_rmse[[species]] <- calculate_species_rmse(
          batch_results$simulations[[i]],
          yield_obs_data,
          species,
          year_range
        )
      }
      batch_species_rmse[[i]] <- species_rmse
    }
    
    # Add to overall results
    all_simulations <- c(all_simulations, batch_results$simulations)
    all_parameters <- c(all_parameters, batch_results$parameters)
    all_rmse <- c(all_rmse, batch_rmse)
    all_species_rmse <- c(all_species_rmse, batch_species_rmse)
    
    total_completed <- total_completed + n_batch
    
    # Track iteration time
    iter_end_time <- Sys.time()
    iter_duration <- as.numeric(difftime(iter_end_time, iter_start_time, units = "secs"))
    iteration_times[[iteration]] <- iter_duration
    
    if (verbose) {
      cat("Iteration", iteration, "completed in", round(iter_duration, 1), "seconds\n")
      elapsed_time <- as.numeric(difftime(iter_end_time, start_time, units = "mins"))
      cat("Total elapsed time:", round(elapsed_time, 1), "minutes\n")
      
      # Estimate remaining time
      if (iteration > 1) {
        avg_iter_time <- mean(unlist(iteration_times))
        remaining_iters <- n_iterations - iteration
        est_remaining_time <- (remaining_iters * avg_iter_time) / 60  # in minutes
        cat("Estimated time remaining:", round(est_remaining_time, 1), "minutes\n")
      }
    }
    
    # Analyze performance and update search space
    if (total_completed < n_total_sims) {
      
      # Update search space based on best performers
      new_search_params <- update_search_space(
        param_history = all_parameters,
        rmse_history = all_rmse,
        species_rmse_history = all_species_rmse,
        current_catchability_sd = current_catchability_sd,
        current_abundance_sd = current_abundance_sd,
        top_percent = 0.2,
        reduction_factor = 0.85
      )
      
      # Store search space history
      search_space_history[[iteration]] <- list(
        iteration = iteration,
        n_sims = total_completed,
        catchability_sd = current_catchability_sd,
        abundance_sd = current_abundance_sd,
        mean_rmse = mean(batch_rmse),
        min_rmse = min(batch_rmse),
        best_overall_rmse = min(all_rmse)
      )
      
      # Update search parameters
      current_catchability_sd <- new_search_params$catchability_sd
      current_abundance_sd <- new_search_params$abundance_sd
      
      # Update centers for targeted search (weighted towards best performers)
      if (!is.null(new_search_params$catchability_center)) {
        catchability_center <- 0.7 * catchability_center + 0.3 * new_search_params$catchability_center
      }
      if (!is.null(new_search_params$abundance_center)) {
        abundance_center <- 0.7 * abundance_center + 0.3 * new_search_params$abundance_center
      }
      
      # Check for convergence (only if we have valid RMSE values)
      if (iteration > 2 && !is.na(search_space_history[[iteration]]$best_overall_rmse) &&
          !is.na(search_space_history[[iteration-1]]$best_overall_rmse) &&
          !is.infinite(search_space_history[[iteration]]$best_overall_rmse) &&
          !is.infinite(search_space_history[[iteration-1]]$best_overall_rmse)) {
        recent_improvement <- abs(search_space_history[[iteration]]$best_overall_rmse -
                                search_space_history[[iteration-1]]$best_overall_rmse)
        
        if (!is.na(recent_improvement) && !is.infinite(recent_improvement) &&
            recent_improvement < convergence_threshold) {
          if (verbose) {
            cat("\nConvergence achieved! Improvement <", convergence_threshold, "\n")
          }
          # Continue with reduced search space for fine-tuning
          current_catchability_sd <- current_catchability_sd * 0.5
          current_abundance_sd <- current_abundance_sd * 0.5
        }
      }
      
      # Report species-specific issues
      if (verbose && !is.null(new_search_params$species_specific_adjustments)) {
        problem_species <- names(which(sapply(new_search_params$species_specific_adjustments,
                                             function(x) x$needs_attention)))
        if (length(problem_species) > 0) {
          cat("\nSpecies needing attention:", paste(problem_species, collapse = ", "), "\n")
          
          # Show RMSE for problem species
          for (sp in problem_species) {
            sp_rmse <- new_search_params$species_specific_adjustments[[sp]]$mean_rmse
            cat("  ", sp, "- Mean RMSE:", round(sp_rmse, 2), "\n")
          }
        }
      }
      
      if (verbose) {
        cat("Best RMSE so far:", round(min(all_rmse), 3), "\n")
        cat("Progress:", total_completed, "/", n_total_sims, "simulations\n\n")
      }
    }
  }
  
  # Find overall optimal parameters (excluding NA and Inf values)
  valid_indices <- which(!is.na(all_rmse) & !is.infinite(all_rmse))
  if (length(valid_indices) == 0) {
    warning("No valid RMSE values found. All simulations may have failed.")
    optimal_idx <- 1
  } else {
    # Find minimum among valid values
    valid_rmse <- all_rmse[valid_indices]
    min_valid_idx <- which.min(valid_rmse)
    optimal_idx <- valid_indices[min_valid_idx]
  }
  
  # Calculate final statistics
  rmse_by_species_final <- do.call(rbind, lapply(all_species_rmse, function(x) {
    data.frame(
      antarctic_krill = x[["antarctic krill"]],
      bathypelagic_fishes = x[["bathypelagic fishes"]],
      shelf_coastal_fishes = x[["shelf and coastal fishes"]],
      squids = x[["squids"]],
      toothfishes = x[["toothfishes"]],
      minke_whales = x[["minke whales"]],
      orca = x[["orca"]],
      sperm_whales = x[["sperm whales"]],
      baleen_whales = x[["baleen whales"]]
    )
  }))
  
  # Calculate total runtime
  end_time <- Sys.time()
  total_runtime <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  # Summary statistics
  final_stats <- list(
    total_simulations = total_completed,
    optimal_index = optimal_idx,
    optimal_rmse = all_rmse[optimal_idx],
    optimal_simulation = all_simulations[[optimal_idx]],
    optimal_parameters = all_parameters[[optimal_idx]],
    rmse_distribution = summary(all_rmse),
    species_rmse_summary = apply(rmse_by_species_final, 2, summary, na.rm = TRUE),
    convergence_history = search_space_history,
    runtime = list(
      total_minutes = total_runtime,
      start_time = start_time,
      end_time = end_time,
      iteration_times = iteration_times,
      avg_time_per_iteration = mean(unlist(iteration_times)),
      avg_time_per_simulation = (total_runtime * 60) / total_completed,  # in seconds
      parallel_used = parallel && n_cores > 1,
      n_cores_used = ifelse(parallel && n_cores > 1, n_cores, 1)
    )
  )
  
  if (verbose) {
    cat("\n=== Final Results ===\n")
    cat("Total simulations completed:", total_completed, "\n")
    cat("Optimal RMSE:", round(all_rmse[optimal_idx], 3), "\n")
    cat("RMSE improvement:", round((max(all_rmse) - min(all_rmse)) / max(all_rmse) * 100, 1), "%\n")
    cat("\nOptimal parameters found at simulation", optimal_idx, "\n")
    cat("\n=== Runtime Summary ===\n")
    cat("Total runtime:", round(total_runtime, 1), "minutes\n")
    cat("Average time per iteration:", round(mean(unlist(iteration_times)), 1), "seconds\n")
    cat("Average time per simulation:", round((total_runtime * 60) / total_completed, 1), "seconds\n")
    cat("Parallel processing:", ifelse(parallel && n_cores > 1,
                                       paste("YES (", n_cores, " cores used)", sep=""),
                                       "NO (sequential)"), "\n")
    cat("End time:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  }
  
  return(list(
    simulations = all_simulations,
    parameters = all_parameters,
    all_rmse = all_rmse,
    species_rmse = all_species_rmse,
    optimal = final_stats,
    search_history = search_space_history
  ))
}

# Function to visualize adaptive convergence
plot_adaptive_convergence <- function(adaptive_results) {
  require(ggplot2)
  require(tidyr)
  
  # Extract search history
  search_history <- adaptive_results$search_history
  
  # Convert to data frame
  df_history <- do.call(rbind, lapply(search_history, function(x) {
    data.frame(
      iteration = x$iteration,
      n_sims = x$n_sims,
      catchability_sd = x$catchability_sd,
      abundance_sd = x$abundance_sd,
      mean_rmse = x$mean_rmse,
      min_rmse = x$min_rmse,
      best_overall_rmse = x$best_overall_rmse
    )
  }))
  
  # Plot 1: RMSE convergence
  p1 <- ggplot(df_history, aes(x = n_sims)) +
    geom_line(aes(y = mean_rmse, color = "Mean RMSE"), size = 1) +
    geom_line(aes(y = min_rmse, color = "Min RMSE (batch)"), size = 1) +
    geom_line(aes(y = best_overall_rmse, color = "Best Overall"), size = 1.5) +
    scale_color_manual(values = c("Mean RMSE" = "gray50",
                                 "Min RMSE (batch)" = "steelblue",
                                 "Best Overall" = "darkred")) +
    theme_bw() +
    labs(title = "RMSE Convergence During Adaptive Search",
         x = "Number of Simulations",
         y = "RMSE",
         color = "Metric") +
    theme(legend.position = "bottom")
  
  # Plot 2: Search space evolution
  df_search <- df_history %>%
    select(n_sims, catchability_sd, abundance_sd) %>%
    pivot_longer(cols = c(catchability_sd, abundance_sd),
                names_to = "Parameter",
                values_to = "SD")
  
  p2 <- ggplot(df_search, aes(x = n_sims, y = SD, color = Parameter)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = c("catchability_sd" = "darkgreen",
                                 "abundance_sd" = "darkorange"),
                      labels = c("Catchability SD", "Abundance SD")) +
    theme_bw() +
    labs(title = "Search Space Refinement",
         x = "Number of Simulations",
         y = "Standard Deviation",
         color = "Parameter") +
    theme(legend.position = "bottom")
  
  # Plot 3: All RMSE values over time
  df_all_rmse <- data.frame(
    simulation = 1:length(adaptive_results$all_rmse),
    rmse = adaptive_results$all_rmse
  )
  
  p3 <- ggplot(df_all_rmse, aes(x = simulation, y = rmse)) +
    geom_point(alpha = 0.5, color = "steelblue") +
    geom_smooth(method = "loess", se = TRUE, color = "darkred") +
    theme_bw() +
    labs(title = "Individual Simulation RMSE Values",
         x = "Simulation Number",
         y = "RMSE") +
    geom_hline(yintercept = min(adaptive_results$all_rmse),
              linetype = "dashed", color = "red", alpha = 0.5)
  
  # Combine plots
  require(gridExtra)
  combined_plot <- grid.arrange(p1, p2, p3, ncol = 1)
  
  return(combined_plot)
}

# Function to plot species-specific RMSE evolution
plot_species_rmse_evolution <- function(adaptive_results, species_focus = NULL) {
  require(ggplot2)
  require(tidyr)
  require(dplyr)
  
  # Extract species RMSE data
  species_rmse_list <- adaptive_results$species_rmse
  
  # Convert to long format data frame
  df_species <- do.call(rbind, lapply(seq_along(species_rmse_list), function(i) {
    rmse_vals <- species_rmse_list[[i]]
    data.frame(
      simulation = i,
      antarctic_krill = rmse_vals[["antarctic krill"]],
      bathypelagic_fishes = rmse_vals[["bathypelagic fishes"]],
      shelf_coastal_fishes = rmse_vals[["shelf and coastal fishes"]],
      squids = rmse_vals[["squids"]],
      toothfishes = rmse_vals[["toothfishes"]],
      minke_whales = rmse_vals[["minke whales"]],
      orca = rmse_vals[["orca"]],
      sperm_whales = rmse_vals[["sperm whales"]],
      baleen_whales = rmse_vals[["baleen whales"]]
    )
  }))
  
  # Reshape to long format
  df_long <- df_species %>%
    pivot_longer(cols = -simulation,
                names_to = "species",
                values_to = "rmse")
  
  # Filter if specific species requested
  if (!is.null(species_focus)) {
    df_long <- df_long %>%
      filter(species %in% species_focus)
  }
  
  # Calculate running average (adjusted for more frequent checks - using window of 10)
  require(zoo)
  df_summary <- df_long %>%
    group_by(species) %>%
    arrange(simulation) %>%
    mutate(
      running_mean = zoo::rollmean(rmse, k = 10, fill = NA, align = "right"),
      running_min = cummin(rmse)
    ) %>%
    ungroup()
  
  # Create plot
  p <- ggplot(df_summary, aes(x = simulation, y = rmse, color = species)) +
    geom_point(alpha = 0.2, size = 0.5) +
    geom_line(aes(y = running_mean), size = 1, alpha = 0.8) +
    facet_wrap(~species, scales = "free_y", ncol = 3) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(title = "Species-Specific RMSE Evolution",
         subtitle = "Points: individual simulations, Lines: 10-simulation moving average",
         x = "Simulation Number",
         y = "RMSE")
  
  return(p)
}

# Function to create parameter correlation heatmap
plot_parameter_correlations <- function(adaptive_results, top_percent = 0.1) {
  require(ggplot2)
  require(corrplot)
  
  # Get top performing simulations
  n_top <- max(1, floor(length(adaptive_results$all_rmse) * top_percent))
  top_indices <- order(adaptive_results$all_rmse)[1:n_top]
  
  # Extract parameters for top performers
  top_params <- adaptive_results$parameters[top_indices]
  
  # Create parameter matrix
  param_matrix <- do.call(rbind, lapply(top_params, function(x) {
    c(
      mean_catchability = mean(x$catchability, na.rm = TRUE),
      mean_mammal_scaling = mean(x$marine_mammal_scaling, na.rm = TRUE),
      minke_scaling = x$marine_mammal_scaling[1],
      orca_scaling = x$marine_mammal_scaling[2],
      sperm_scaling = x$marine_mammal_scaling[3],
      baleen_scaling = x$marine_mammal_scaling[4]
    )
  }))
  
  # Add RMSE values
  param_matrix <- cbind(param_matrix, rmse = adaptive_results$all_rmse[top_indices])
  
  # Calculate correlation matrix
  cor_matrix <- cor(param_matrix, use = "complete.obs")
  
  # Create correlation plot
  png("adaptive_parameter_correlations.png", width = 800, height = 800)
  corrplot(cor_matrix, method = "color", type = "upper",
          addCoef.col = "black", tl.col = "black", tl.srt = 45,
          title = paste("Parameter Correlations (Top", top_percent*100, "% Performers)"),
          mar = c(0,0,2,0))
  dev.off()
  
  return(cor_matrix)
}