# Helper_Functions_Xtra.R
# Additional helper functions for adaptive uncertainty analysis
# These are specialized functions that don't need to be loaded regularly

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
                               species_specific_sd = NULL,
                               top_percent = 0.2, reduction_factor = 0.9,
                               min_sd = 0.05, max_sd = 1.0) {
  
  n_best <- max(1, floor(length(rmse_history) * top_percent))
  best_indices <- order(rmse_history)[1:n_best]
  
  # Initialize new search parameters
  new_params <- list(
    catchability_sd = current_catchability_sd,
    abundance_sd = current_abundance_sd,
    species_specific_adjustments = list(),
    species_specific_sd = list()  # Store species-specific SD values
  )
  
  # Analyze best performing parameters
  best_params <- param_history[best_indices]
  
  # Calculate mean and sd of best catchability values
  best_catchability <- sapply(best_params, function(x) mean(x$catchability, na.rm = TRUE))
  catchability_spread <- sd(best_catchability, na.rm = TRUE)
  
  # Calculate mean and sd of best abundance scaling
  best_abundance <- sapply(best_params, function(x) mean(x$scaled_species_factors, na.rm = TRUE))
  abundance_spread <- sd(best_abundance, na.rm = TRUE)
  
  # Adjust search space based on spread of best parameters
  # Check for NA values (can occur with small sample sizes)
  if (!is.na(catchability_spread)) {
    if (catchability_spread < current_catchability_sd * 0.5) {
      new_params$catchability_sd <- max(min_sd, current_catchability_sd * reduction_factor)
    } else if (catchability_spread > current_catchability_sd * 1.5) {
      new_params$catchability_sd <- min(max_sd, current_catchability_sd / reduction_factor)
    }
  }
  
  if (!is.na(abundance_spread)) {
    if (abundance_spread < current_abundance_sd * 0.5) {
      new_params$abundance_sd <- max(min_sd, current_abundance_sd * reduction_factor)
    } else if (abundance_spread > current_abundance_sd * 1.5) {
      new_params$abundance_sd <- min(max_sd, current_abundance_sd / reduction_factor)
    }
  }
  
  # Analyze species-specific performance and adjust SD accordingly
  if (!is.null(species_rmse_history) && length(species_rmse_history) > 0) {
    species_names <- names(species_rmse_history[[1]])
    
    # Calculate overall RMSE statistics for normalization
    all_species_rmse <- unlist(species_rmse_history)
    overall_median <- median(all_species_rmse, na.rm = TRUE)
    overall_q25 <- quantile(all_species_rmse, 0.25, na.rm = TRUE)
    overall_q75 <- quantile(all_species_rmse, 0.75, na.rm = TRUE)
    
    for (species in species_names) {
      species_rmse <- sapply(species_rmse_history[best_indices],
                             function(x) x[[species]])
      
      # Check if this species needs special attention (high RMSE)
      if (!all(is.na(species_rmse))) {
        mean_rmse <- mean(species_rmse, na.rm = TRUE)
        sd_rmse <- sd(species_rmse, na.rm = TRUE)
        min_rmse <- min(species_rmse, na.rm = TRUE)
        
        # Calculate relative performance
        relative_performance <- mean_rmse / overall_median
        
        # Determine species-specific SD based on performance
        if (!is.null(species_specific_sd) && species %in% names(species_specific_sd)) {
          current_sp_sd <- species_specific_sd[[species]]
        } else {
          current_sp_sd <- list(catchability = current_catchability_sd,
                               abundance = current_abundance_sd)
        }
        
        # Adjust SD based on relative performance
        if (relative_performance < 0.5) {
          # Excellent fit - reduce SD significantly
          sp_catch_sd <- max(min_sd, current_sp_sd$catchability * 0.7)
          sp_abund_sd <- max(min_sd, current_sp_sd$abundance * 0.7)
          convergence_status <- "excellent"
        } else if (relative_performance < 1.0) {
          # Good fit - reduce SD moderately
          sp_catch_sd <- max(min_sd, current_sp_sd$catchability * 0.85)
          sp_abund_sd <- max(min_sd, current_sp_sd$abundance * 0.85)
          convergence_status <- "good"
        } else if (relative_performance < 2.0) {
          # Average fit - small reduction or maintain
          sp_catch_sd <- current_sp_sd$catchability * 0.95
          sp_abund_sd <- current_sp_sd$abundance * 0.95
          convergence_status <- "average"
        } else {
          # Poor fit - maintain or increase SD for exploration
          sp_catch_sd <- min(max_sd, current_sp_sd$catchability * 1.1)
          sp_abund_sd <- min(max_sd, current_sp_sd$abundance * 1.1)
          convergence_status <- "poor"
        }
        
        # Store species-specific adjustments
        new_params$species_specific_sd[[species]] <- list(
          catchability = sp_catch_sd,
          abundance = sp_abund_sd
        )
        
        new_params$species_specific_adjustments[[species]] <- list(
          mean_rmse = mean_rmse,
          min_rmse = min_rmse,
          sd_rmse = sd_rmse,
          relative_performance = relative_performance,
          convergence_status = convergence_status,
          needs_attention = mean_rmse > overall_median * 1.5
        )
      }
    }
  }
  
  # Calculate center of best parameter region for targeted search
  # Handle potential NA values
  catchability_center_val <- mean(best_catchability, na.rm = TRUE)
  abundance_center_val <- mean(best_abundance, na.rm = TRUE)
  
  if (!is.na(catchability_center_val)) {
    new_params$catchability_center <- catchability_center_val
  } else {
    new_params$catchability_center <- 1.0  # Default to no bias if NA
  }
  
  if (!is.na(abundance_center_val)) {
    new_params$abundance_center <- abundance_center_val
  } else {
    new_params$abundance_center <- 1.0  # Default to no bias if NA
  }
  
  return(new_params)
}

# Helper function to run a batch of simulations with species-specific SD
run_batch_simulations <- function(params, n_sims, catchability_sd, abundance_sd,
                                 catchability_center = 1, abundance_center = 1,
                                 species_specific_sd = NULL,
                                 species_to_scale, effort_scen,
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
      sp_params <- species_params(rand_params)
      
      for(j in 1:nrow(gear_df)) {
        if(gear_df$catchability[j] > 0) {
          # Get species name for this gear
          species_name <- gear_df$species[j]
          
          # Use species-specific SD if available
          if (!is.null(species_specific_sd) && species_name %in% names(species_specific_sd)) {
            sp_catch_sd <- species_specific_sd[[species_name]]$catchability
          } else {
            sp_catch_sd <- catchability_sd
          }
          
          # Generate biased noise with species-specific SD
          noise <- rnorm(1, mean = log(catchability_center), sd = sp_catch_sd)
          gear_df$catchability[j] <- gear_df$catchability[j] * exp(noise - log(catchability_center))
          gear_df$catchability[j] <- min(1, max(0, gear_df$catchability[j]))
        }
      }
      gear_params(rand_params) <- gear_df
    }
    
    # Randomize abundance for specified species (with bias towards center)
    sp_params <- species_params(rand_params)
    species_indices <- which(sp_params$species %in% species_to_scale)
    
    # Generate biased scaling factors with species-specific SD
    scaling_factors <- numeric(length(species_indices))
    
    for (k in seq_along(species_indices)) {
      species_name <- sp_params$species[species_indices[k]]
      
      # Use species-specific SD if available
      if (!is.null(species_specific_sd) && species_name %in% names(species_specific_sd)) {
        sp_abund_sd <- species_specific_sd[[species_name]]$abundance
      } else {
        sp_abund_sd <- abundance_sd
      }
      
      # Generate scaling factor with species-specific SD
      scaling_factors[k] <- exp(rnorm(1, mean = log(abundance_center), sd = sp_abund_sd))
      
      # Apply scaling
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
        scaled_species_factors = scaling_factors,
        scaled_species = species_to_scale,
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
                                             species_to_scale = NULL,
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
  
  # Default species to scale if not specified
  if (is.null(species_to_scale)) {
    species_to_scale <- c("minke whales", "orca", "sperm whales", "baleen whales", 
                         "shelf and coastal fishes")
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
  species_specific_sd <- NULL  # Will be populated after first iteration
  
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
    cat("Species to scale:", paste(species_to_scale, collapse = ", "), "\n")
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
      species_specific_sd = species_specific_sd,
      species_to_scale = species_to_scale,
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
        species_specific_sd = species_specific_sd,
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
      
      # Update species-specific SD values
      if (!is.null(new_search_params$species_specific_sd)) {
        species_specific_sd <- new_search_params$species_specific_sd
      }
      
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
      
      # Report species-specific convergence status
      if (verbose && !is.null(new_search_params$species_specific_adjustments)) {
        cat("\nSpecies-specific convergence status:\n")
        
        for (sp in names(new_search_params$species_specific_adjustments)) {
          sp_info <- new_search_params$species_specific_adjustments[[sp]]
          if (!is.null(new_search_params$species_specific_sd[[sp]])) {
            sp_sd <- new_search_params$species_specific_sd[[sp]]
            
            cat("  ", sp, ":\n")
            cat("    Status:", sp_info$convergence_status, "\n")
            cat("    Mean RMSE:", round(sp_info$mean_rmse, 2), "\n")
            cat("    Min RMSE:", round(sp_info$min_rmse, 2), "\n")
            cat("    Catchability SD:", round(sp_sd$catchability, 3), "\n")
            cat("    Abundance SD:", round(sp_sd$abundance, 3), "\n")
          }
        }
        
        # Highlight species still needing attention
        problem_species <- names(which(sapply(new_search_params$species_specific_adjustments,
                                             function(x) x$needs_attention)))
        if (length(problem_species) > 0) {
          cat("\nSpecies still needing attention (high RMSE):", paste(problem_species, collapse = ", "), "\n")
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
    # Create data frame with all species, using NA for missing ones
    df <- data.frame(matrix(NA, nrow = 1, ncol = length(fished_species)))
    names(df) <- gsub(" ", "_", fished_species)
    
    for (sp in names(x)) {
      col_name <- gsub(" ", "_", sp)
      if (col_name %in% names(df)) {
        df[[col_name]] <- x[[sp]]
      }
    }
    return(df)
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
    # Create a data frame with all species
    df <- data.frame(simulation = i)
    for (sp in names(rmse_vals)) {
      df[[gsub(" ", "_", sp)]] <- rmse_vals[[sp]]
    }
    return(df)
  }))
  
  # Reshape to long format
  df_long <- df_species %>%
    pivot_longer(cols = -simulation,
                names_to = "species",
                values_to = "rmse")
  
  # Filter if specific species requested
  if (!is.null(species_focus)) {
    df_long <- df_long %>%
      filter(species %in% gsub(" ", "_", species_focus))
  }
  
  # Calculate running average (adjusted for more frequent checks - using window of 10)
  require(zoo)
  df_summary <- df_long %>%
    group_by(species) %>%
    arrange(simulation) %>%
    mutate(
      running_mean = zoo::rollmean(rmse, k = min(10, n()), fill = NA, align = "right"),
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
      mean_scaling = mean(x$scaled_species_factors, na.rm = TRUE)
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

# Enhanced run_combined_uncertainty_sims with progress tracking and flexible scaling
run_combined_uncertainty_sims <- function(params,
                                         n_sims = 10,
                                         catchability_sd = 0.1,
                                         abundance_sd = 0.2,
                                         species_to_scale = NULL,
                                         effort_scen,
                                         tol = 0.01,
                                         t_max = 500,
                                         spinup_years = 118,
                                         t_start = 1841,
                                         sim_years = 170,
                                         verbose = TRUE) {
  
  # Default species to scale if not specified
  if (is.null(species_to_scale)) {
    species_to_scale <- c("minke whales", "orca", "sperm whales", "baleen whales",
                         "shelf and coastal fishes")
  }
  
  # Time tracking
  start_time <- Sys.time()
  
  if (verbose) {
    cat("Starting combined uncertainty simulations\n")
    cat("Number of simulations:", n_sims, "\n")
    cat("Species to scale:", paste(species_to_scale, collapse = ", "), "\n")
    cat("Start time:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n\n")
  }
  
  # List to store results
  sim_results <- list()
  param_combinations <- list()
  
  # Run simulations
  for(i in 1:n_sims) {
    
    if (verbose && i %% 5 == 0) {
      cat("Progress:", i, "/", n_sims, "simulations completed\n")
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
      cat("  Elapsed time:", round(elapsed, 1), "minutes\n")
      if (i > 1) {
        avg_time_per_sim <- elapsed / i
        est_remaining <- avg_time_per_sim * (n_sims - i)
        cat("  Estimated time remaining:", round(est_remaining, 1), "minutes\n")
      }
    }
    
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
    
    # Randomize abundance for specified species
    sp_params <- species_params(rand_params)
    species_indices <- which(sp_params$species %in% species_to_scale)
    
    # Generate random scaling factors (log-normal distribution)
    scaling_factors <- exp(rnorm(length(species_indices), mean = 0, sd = abundance_sd))
    
    # Apply scaling
    for (k in seq_along(species_indices)) {
      rand_params@initial_n[species_indices[k],] <-
        rand_params@initial_n[species_indices[k],] * scaling_factors[k]
    }
    
    # Store parameter combination
    param_combinations[[i]] <- list(
      catchability = gear_df$catchability,
      scaled_species = species_to_scale,
      scaling_factors = scaling_factors
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
  
  # Calculate total runtime
  end_time <- Sys.time()
  total_runtime <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  if (verbose) {
    cat("\n=== Simulation Complete ===\n")
    cat("Total runtime:", round(total_runtime, 2), "minutes\n")
    cat("Average time per simulation:", round(total_runtime / n_sims, 2), "minutes\n")
    cat("End time:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  }
  
  return(list(
    simulations = sim_results,
    parameters = param_combinations,
    runtime = list(
      total_minutes = total_runtime,
      start_time = start_time,
      end_time = end_time,
      avg_time_per_sim = total_runtime / n_sims
    )
  ))
}