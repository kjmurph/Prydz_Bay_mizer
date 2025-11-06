# ===== MEMORY-EFFICIENT MONTE CARLO IMPLEMENTATION =====

# PROBLEM WITH CURRENT APPROACH:
# - sim_results <- list() keeps ALL MizerSim objects in memory (could be 50+ GB for 10k sims)
# - saveRDS(checkpoint) saves increasingly large files (checkpoint_1.rds = 1GB, checkpoint_100.rds = 100GB)
# - Memory usage grows linearly: 1 block = 500MB, 100 blocks = 50GB

# SOLUTION: Block-based storage with memory management

#' Memory-efficient Monte Carlo with block-based file storage
run_enhanced_uncertainty_sims_memory_efficient <- function(params,
                                                           total_sims = 10000,
                                                           block_size = 100,
                                                           catchability_sd = 1,
                                                           abundance_sd = 1,
                                                           gamma_sd = 1,
                                                           all_species_abundance = TRUE,
                                                           species_to_vary = NULL,
                                                           effort_scen,
                                                           enhanced_steady_tol = 0.025,
                                                           enhanced_steady_t_max = 750,
                                                           spinup_years = 118,
                                                           t_start = 1841,
                                                           sim_years = 170,
                                                           preserve_erepro = TRUE,
                                                           max_erepro = 1,
                                                           n_cores = NULL,
                                                           base_save_path = "mc_blocks",
                                                           memory_mode = "efficient",  # "efficient" or "full"
                                                           verbose = TRUE,
                                                           timing_frequency = 1,
                                                           catchability_bias_mean_log = 0,
                                                           abundance_bias_mean_log = 0,
                                                           gamma_bias_mean_log = 0) {
  
  overall_start_time <- Sys.time()
  
  # =============================================================================
  # SETUP MEMORY-EFFICIENT STORAGE SYSTEM
  # =============================================================================
  
  # Create directory for block files
  if (!dir.exists(base_save_path)) {
    dir.create(base_save_path, recursive = TRUE)
  }
  
  if (verbose) {
    cat("=== MEMORY-EFFICIENT MONTE CARLO ===\n")
    cat("Storage mode:", memory_mode, "\n")
    cat("Block files will be saved to:", base_save_path, "\n")
    cat("Block size:", block_size, "(max", round(block_size * 0.5, 0), "GB per block)\n")
    flush.console()
  }
  
  # Setup parallel processing
  setup_start <- Sys.time()
  if (is.null(n_cores)) n_cores <- max(1, parallel::detectCores() - 1)
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  parallel::clusterEvalQ(cl, { library(therMizer); library(mizer) })
  parallel::clusterSetRNGStream(cl, iseed = 20250907)
  
  if (!exists("precheck_parameters", inherits = TRUE)) {
    precheck_parameters <- function(...) list(ok = TRUE)
    invisible(precheck_parameters)
  }
  
  parallel::clusterExport(cl, c(
    "params", "effort_scen", "verbose", "precheck_parameters",
    "catchability_bias_mean_log", "abundance_bias_mean_log", "gamma_bias_mean_log"
  ), envir = environment())
  
  parallel::clusterExport(cl, c(
    "run_single_enhanced_sim_with_params", "check_biomass_stability_enhanced", 
    "stability_cv_threshold", "stability_check_years_tail", "stability_min_mean_biomass"
  ), envir = .GlobalEnv)
  
  setup_end <- Sys.time()
  setup_time <- as.numeric(difftime(setup_end, setup_start, units = "secs"))
  
  # =============================================================================
  # MEMORY-EFFICIENT STORAGE CONTAINERS
  # =============================================================================
  
  if (memory_mode == "efficient") {
    # EFFICIENT MODE: Only store summaries in memory, not full simulation objects
    simulation_summaries <- list()      # Light summaries only
    param_combinations_successful <- list()
    param_combinations_all <- list()
    failed_sims_with_params <- list()
    
    if (verbose) {
      cat("Efficient mode: Full simulation objects will be saved to disk, not kept in memory\n")
    }
    
  } else {
    # FULL MODE: Keep everything in memory (original behavior)
    sim_results <- list()
    param_combinations_successful <- list()
    param_combinations_all <- list()
    failed_sims_with_params <- list()
    
    if (verbose) {
      cat("Full mode: All objects kept in memory (high memory usage)\n")
    }
  }
  
  # Tracking variables
  successful_count <- 0L
  total_processed <- 0L
  block_times <- numeric()
  block_success_rates <- numeric()
  block_file_paths <- character()
  
  # =============================================================================
  # MAIN SIMULATION LOOP WITH MEMORY MANAGEMENT
  # =============================================================================
  
  blocks <- split(1:total_sims, ceiling((1:total_sims) / block_size))
  
  for (bi in seq_along(blocks)) {
    block_start_time <- Sys.time()
    idx <- blocks[[bi]]
    
    if (verbose) { 
      cat("Block ", bi, "/", length(blocks), ": sims ", idx[1], "-", tail(idx,1), " ... ", sep = "")
      flush.console() 
    }
    
    # ==========================================================================
    # RUN CURRENT BLOCK (TEMPORARY STORAGE)
    # ==========================================================================
    
    results_list <- foreach::foreach(
      i = idx,
      .packages = c("therMizer","mizer"),
      .export = c("run_single_enhanced_sim_with_params","check_biomass_stability_enhanced","precheck_parameters"),
      .errorhandling = "pass"
    ) %dopar% {
      run_single_enhanced_sim_with_params(i, params, catchability_sd, abundance_sd, gamma_sd,
                                          all_species_abundance, species_to_vary, effort_scen,
                                          enhanced_steady_tol, enhanced_steady_t_max,
                                          spinup_years, t_start, sim_years, preserve_erepro, max_erepro,
                                          catchability_bias_mean_log = catchability_bias_mean_log,
                                          abundance_bias_mean_log = abundance_bias_mean_log,
                                          gamma_bias_mean_log = gamma_bias_mean_log,
                                          verbose = FALSE)
    }
    
    # ==========================================================================
    # PROCESS BLOCK RESULTS (MEMORY-EFFICIENT)
    # ==========================================================================
    
    # Temporary containers for this block only
    block_successful_sims <- list()
    block_successful_params <- list()
    block_all_params <- list()
    block_failed_params <- list()
    block_successful_count <- 0
    
    for (k in seq_along(results_list)) {
      r <- results_list[[k]]
      current_sim_id <- idx[k]
      
      # Store attempted parameters (always)
      if (is.list(r) && !is.null(r$attempted_parameters)) {
        if (memory_mode == "efficient") {
          block_all_params[[length(block_all_params) + 1]] <- r$attempted_parameters
        } else {
          param_combinations_all[[length(param_combinations_all) + 1]] <- r$attempted_parameters
        }
      }
      
      if (is.list(r) && isTRUE(r$success)) {
        # Successful simulation
        block_successful_count <- block_successful_count + 1
        successful_count <- successful_count + 1L
        
        if (memory_mode == "efficient") {
          # Store full sim temporarily for block save
          block_successful_sims[[block_successful_count]] <- r$simulation
          block_successful_params[[block_successful_count]] <- r$parameters
          
          # Store only lightweight summary in main memory
          sim_summary <- list(
            sim_id = current_sim_id,
            block_id = bi,
            species_names = r$parameters$species_names,
            max_cv = r$parameters$max_cv,
            stable = r$parameters$stable,
            final_biomass = tryCatch({
              biomass <- getBiomass(r$simulation)
              colSums(tail(biomass, 5))  # Average of last 5 years
            }, error = function(e) rep(NA, length(r$parameters$species_names)))
          )
          simulation_summaries[[successful_count]] <- sim_summary
          
        } else {
          # Full mode: store everything in memory
          sim_results[[successful_count]] <- r$simulation
          param_combinations_successful[[successful_count]] <- r$parameters
        }
        
      } else {
        # Failed simulation
        failed_entry <- list(
          sim_id = current_sim_id, 
          block_id = bi,
          error = if (is.list(r)) r$error else "unknown",
          attempted_parameters = if (is.list(r) && !is.null(r$attempted_parameters)) r$attempted_parameters else NULL,
          stability_details = if (is.list(r) && !is.null(r$stability_details)) r$stability_details else NULL
        )
        
        if (memory_mode == "efficient") {
          block_failed_params[[length(block_failed_params) + 1]] <- failed_entry
        } else {
          failed_sims_with_params[[length(failed_sims_with_params) + 1]] <- failed_entry
        }
      }
    }
    
    # ==========================================================================
    # SAVE BLOCK TO DISK AND FREE MEMORY
    # ==========================================================================
    
    block_end_time <- Sys.time()
    block_duration <- as.numeric(difftime(block_end_time, block_start_time, units = "secs"))
    block_times[bi] <- block_duration
    block_success_rate <- block_successful_count / length(idx)
    block_success_rates[bi] <- block_success_rate
    total_processed <- idx[length(idx)]
    
    # Save this block to its own file
    block_file <- file.path(base_save_path, paste0("block_", sprintf("%04d", bi), ".rds"))
    block_data <- list(
      block_id = bi,
      sim_indices = idx,
      successful_simulations = if (length(block_successful_sims) > 0) block_successful_sims else NULL,
      successful_parameters = if (length(block_successful_params) > 0) block_successful_params else NULL,
      all_attempted_parameters = if (memory_mode == "efficient") block_all_params else NULL,
      failed_parameters = if (memory_mode == "efficient") block_failed_params else NULL,
      block_summary = list(
        n_attempted = length(idx),
        n_successful = block_successful_count,
        success_rate = block_success_rate,
        block_duration = block_duration,
        start_time = block_start_time,
        end_time = block_end_time
      )
    )
    
    saveRDS(block_data, file = block_file)
    block_file_paths[bi] <- block_file
    
    # Clear temporary block containers to free memory
    rm(block_successful_sims, block_successful_params, block_all_params, block_failed_params, results_list)
    gc()  # Force garbage collection
    
    if (verbose) {
      cat("completed in", format_duration(block_duration))
      cat(" (", block_successful_count, "/", length(idx), " successful, ", 
          round(block_success_rate * 100, 1), "%)\n", sep = "")
      cat("  Block saved:", basename(block_file), " | Memory freed\n")
    }
    
    # Show timing statistics
    if (verbose && (bi %% timing_frequency == 0 || bi == length(blocks))) {
      timing_stats <- calculate_timing_stats(
        overall_start_time, 
        block_end_time, 
        total_processed, 
        total_sims, 
        successful_count
      )
    }
    
    # ==========================================================================
    # SAVE LIGHTWEIGHT MASTER INDEX (SMALL FILE)
    # ==========================================================================
    
    master_index <- list(
      run_info = list(
        total_sims = total_sims,
        block_size = block_size,
        memory_mode = memory_mode,
        n_blocks_completed = bi,
        n_successful = successful_count,
        n_attempted = total_processed,
        success_rate = successful_count / total_processed
      ),
      block_files = block_file_paths[1:bi],
      block_summaries = data.frame(
        block_id = 1:bi,
        n_attempted = sapply(blocks[1:bi], length),
        n_successful = sapply(1:bi, function(x) {
          if (x <= length(block_success_rates)) {
            sapply(blocks[x], length)[1] * block_success_rates[x]
          } else 0
        }),
        success_rate = block_success_rates[1:bi],
        duration_sec = block_times[1:bi]
      ),
      timing_summary = list(
        overall_start_time = overall_start_time,
        current_time = block_end_time,
        setup_time = setup_time,
        avg_per_sim = sum(block_times[1:bi]) / total_processed,
        estimated_total_time = if (total_processed > 0) sum(block_times[1:bi]) / total_processed * total_sims else NA
      )
    )
    
    # Add memory-specific data to master index
    if (memory_mode == "efficient") {
      master_index$simulation_summaries <- simulation_summaries[1:successful_count]
      # Note: detailed parameters are in block files, not in master index
    } else {
      master_index$all_simulations <- sim_results[1:successful_count]
      master_index$all_successful_parameters <- param_combinations_successful[1:successful_count]
      master_index$all_attempted_parameters <- param_combinations_all
      master_index$all_failed_parameters <- failed_sims_with_params
    }
    
    # Save lightweight master index
    master_file <- file.path(base_save_path, "master_index.rds")
    saveRDS(master_index, file = master_file)
    
    if (verbose && bi < length(blocks)) {
      flush.console()
    }
  }
  
  # =============================================================================
  # FINAL SUMMARY
  # =============================================================================
  
  overall_end_time <- Sys.time()
  total_elapsed <- as.numeric(difftime(overall_end_time, overall_start_time, units = "secs"))
  
  if (verbose) {
    cat("\n" , rep("=", 60), "\n", sep = "")
    cat("MEMORY-EFFICIENT MONTE CARLO COMPLETE\n")
    cat(rep("=", 60), "\n", sep = "")
    cat("Storage mode:", memory_mode, "\n")
    cat("Blocks saved:", length(block_file_paths), "\n")
    cat("Master index:", file.path(base_save_path, "master_index.rds"), "\n")
    cat("Total successful:", successful_count, "/", total_sims, 
        " (", round(successful_count/total_sims*100, 1), "%)\n", sep = "")
    cat("Total runtime:", format_duration(total_elapsed), "\n")
    
    # Memory usage estimate
    if (memory_mode == "efficient") {
      estimated_memory_mb <- length(simulation_summaries) * 0.01  # ~10KB per summary
      cat("Estimated memory usage: ~", round(estimated_memory_mb), "MB (vs ", 
          round(successful_count * 50), "MB for full mode)\n", sep = "")
    }
    cat(rep("=", 60), "\n\n", sep = "")
  }
  
  return(master_index)
}

# ===== UTILITY FUNCTIONS FOR BLOCK-BASED DATA =====

#' Load all successful simulations from block files
load_all_simulations <- function(base_save_path = "mc_blocks") {
  master_file <- file.path(base_save_path, "master_index.rds")
  if (!file.exists(master_file)) {
    stop("Master index not found: ", master_file)
  }
  
  master <- readRDS(master_file)
  
  if (master$run_info$memory_mode == "full") {
    # Data already in master index
    return(master$all_simulations)
  }
  
  # Load from block files
  cat("Loading simulations from", length(master$block_files), "block files...\n")
  all_sims <- list()
  sim_count <- 0
  
  for (block_file in master$block_files) {
    if (file.exists(block_file)) {
      block_data <- readRDS(block_file)
      if (!is.null(block_data$successful_simulations)) {
        for (sim in block_data$successful_simulations) {
          sim_count <- sim_count + 1
          all_sims[[sim_count]] <- sim
        }
      }
    }
  }
  
  cat("Loaded", sim_count, "simulations\n")
  return(all_sims)
}

#' Load all successful parameters from block files
load_all_parameters <- function(base_save_path = "mc_blocks", type = "successful") {
  master_file <- file.path(base_save_path, "master_index.rds")
  master <- readRDS(master_file)
  
  if (master$run_info$memory_mode == "full") {
    # Data already in master index
    if (type == "successful") return(master$all_successful_parameters)
    if (type == "attempted") return(master$all_attempted_parameters)
    if (type == "failed") return(master$all_failed_parameters)
  }
  
  # Load from block files
  cat("Loading", type, "parameters from", length(master$block_files), "block files...\n")
  all_params <- list()
  param_count <- 0
  
  for (block_file in master$block_files) {
    if (file.exists(block_file)) {
      block_data <- readRDS(block_file)
      
      target_data <- switch(type,
                            "successful" = block_data$successful_parameters,
                            "attempted" = block_data$all_attempted_parameters,
                            "failed" = block_data$failed_parameters
      )
      
      if (!is.null(target_data)) {
        for (param in target_data) {
          param_count <- param_count + 1
          all_params[[param_count]] <- param
        }
      }
    }
  }
  
  cat("Loaded", param_count, "parameter sets\n")
  return(all_params)
}

#' Get memory usage comparison
compare_memory_usage <- function(base_save_path = "mc_blocks") {
  master_file <- file.path(base_save_path, "master_index.rds")
  master <- readRDS(master_file)
  
  n_successful <- master$run_info$n_successful
  
  # Calculate efficient mode usage
  master_size_mb <- file.size(master_file) / 1024^2
  total_block_size_mb <- sum(sapply(master$block_files, function(f) {
    if (file.exists(f)) file.size(f) / 1024^2 else 0
  }))
  
  # Estimate full mode usage
  estimated_full_memory_mb <- n_successful * 50  # ~50MB per MizerSim object
  estimated_full_disk_mb <- estimated_full_memory_mb  # Same for single file
  
  cat("=== MEMORY USAGE COMPARISON ===\n")
  cat("Successful simulations:", n_successful, "\n\n")
  cat("EFFICIENT MODE (current):\n")
  cat("  Master index file:", round(master_size_mb, 1), "MB\n")
  cat("  Block files total:", round(total_block_size_mb, 1), "MB\n")
  cat("  Memory usage: ~", round(master_size_mb, 1), "MB\n\n")
  cat("FULL MODE (would be):\n")
  cat("  Memory usage: ~", round(estimated_full_memory_mb, 1), "MB\n")
  cat("  Single file size: ~", round(estimated_full_disk_mb, 1), "MB\n\n")
  cat("SAVINGS:\n")
  cat("  Memory saved: ~", round(estimated_full_memory_mb - master_size_mb, 1), "MB\n")
  cat("  Efficiency gain: ", round((estimated_full_memory_mb - master_size_mb) / estimated_full_memory_mb * 100, 1), "%\n")
}

# ===== ENHANCED MAIN FUNCTION WITH MEMORY OPTIONS =====
run_monte_carlo_analysis_memory_efficient <- function(params, 
                                                      effort_scenario,
                                                      total_sims = 10000,
                                                      pilot_first = TRUE,
                                                      pilot_sims = 100,
                                                      memory_mode = "efficient",  # "efficient" or "full"
                                                      catchability_sd = 1,
                                                      abundance_sd = 1, 
                                                      gamma_sd = 1,
                                                      use_bias = FALSE,
                                                      base_save_path = "mc_blocks") {
  
  if (pilot_first && total_sims > pilot_sims) {
    cat("Running pilot estimation (will use full mode for pilot)...\n\n")
    
    pilot_analysis <- run_pilot_with_estimation(
      params = params,
      effort_scenario = effort_scenario,
      pilot_sims = pilot_sims,
      target_sims = total_sims,
      catchability_sd = catchability_sd,
      abundance_sd = abundance_sd,
      gamma_sd = gamma_sd,
      use_bias = use_bias
    )
    
    estimated_hours <- pilot_analysis$projections$estimated_total_time / 3600
    
    if (estimated_hours > 24) {
      cat("WARNING: Estimated runtime is", round(estimated_hours, 1), "hours\n")
    }
    
    cat("Memory mode for main run:", memory_mode, "\n")
    if (memory_mode == "efficient") {
      cat("This will save memory and use block-based storage.\n")
    }
    
    response <- readline(prompt = paste0("Continue with ", memory_mode, " mode for ", total_sims, " simulations? (y/n): "))
    if (tolower(substr(response, 1, 1)) != "y") {
      return(pilot_analysis)
    }
  }
  
  # Set bias parameters
  if (use_bias) {
    catch_bias <- log(1.2)
    abund_bias <- log(1.2)
    gamma_bias <- log(1.2)
  } else {
    catch_bias <- 0
    abund_bias <- 0
    gamma_bias <- 0
  }
  
  cat("=== MEMORY-EFFICIENT MONTE CARLO ===\n")
  cat("Mode:", memory_mode, "\n")
  cat("Simulations:", total_sims, "\n")
  cat("Storage location:", base_save_path, "\n\n")
  
  result <- run_enhanced_uncertainty_sims_memory_efficient(
    params = params,
    total_sims = total_sims,
    block_size = min(100, total_sims),
    catchability_sd = catchability_sd,
    abundance_sd = abundance_sd,
    gamma_sd = gamma_sd,
    all_species_abundance = TRUE,
    species_to_vary = NULL,
    effort_scen = effort_scenario,
    enhanced_steady_tol = 0.002,
    enhanced_steady_t_max = 750,
    spinup_years = 118,
    t_start = 1841,
    sim_years = 170,
    preserve_erepro = TRUE,
    max_erepro = 1,
    n_cores = NULL,
    base_save_path = base_save_path,
    memory_mode = memory_mode,
    verbose = TRUE,
    timing_frequency = max(1, ceiling(total_sims / 1000)),
    catchability_bias_mean_log = catch_bias,
    abundance_bias_mean_log = abund_bias,
    gamma_bias_mean_log = gamma_bias
  )
  
  if (memory_mode == "efficient") {
    cat("\nTo load all simulations later, use:\n")
    cat("  all_sims <- load_all_simulations('", base_save_path, "')\n", sep = "")
    cat("  all_params <- load_all_parameters('", base_save_path, "', 'successful')\n", sep = "")
    
    compare_memory_usage(base_save_path)
  }
  
  return(result)
}

optimized_params <- result_balanced_v4$params

# ===== QUICK USAGE EXAMPLES =====

# # Memory-efficient run (recommended for 10k sims)
# MC_results_efficient <- run_monte_carlo_analysis_memory_efficient(
#   params = optimized_params,
#   effort_scenario = combined_effort_array,
#   total_sims = 5000,
#   memory_mode = "efficient",  # <-- This is the key setting
#   catchability_sd = 2,
#   abundance_sd = 4,
#   gamma_sd = 2,
#   use_bias = FALSE,
#   base_save_path = "mc_10k_blocks",
#   pilot_first = F
# )

# # Load results when needed
# all_simulations <- load_all_simulations("mc_10k_blocks")
# successful_params <- load_all_parameters("mc_10k_blocks", "successful")
# attempted_params <- load_all_parameters("mc_10k_blocks", "attempted")

# ===== TIMING UTILITIES =====

#' Format time duration in a human-readable way
format_duration <- function(seconds) {
  if (seconds < 60) {
    return(paste(round(seconds, 1), "seconds"))
  } else if (seconds < 3600) {
    mins <- floor(seconds / 60)
    secs <- round(seconds %% 60)
    return(paste(mins, "min", secs, "sec"))
  } else if (seconds < 86400) {
    hours <- floor(seconds / 3600)
    mins <- floor((seconds %% 3600) / 60)
    return(paste(hours, "hr", mins, "min"))
  } else {
    days <- floor(seconds / 86400)
    hours <- floor((seconds %% 86400) / 3600)
    return(paste(days, "days", hours, "hr"))
  }
}

#' Calculate and display timing statistics
calculate_timing_stats <- function(start_time, end_time, n_completed, n_total, n_successful = NULL) {
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  avg_per_sim <- elapsed / n_completed
  
  remaining_sims <- n_total - n_completed
  estimated_remaining <- remaining_sims * avg_per_sim
  estimated_total <- n_total * avg_per_sim
  
  cat("\n--- TIMING STATISTICS ---\n")
  cat("Completed:", n_completed, "/", n_total, "simulations\n")
  if (!is.null(n_successful)) {
    cat("Successful:", n_successful, "(", round(n_successful/n_completed*100, 1), "%)\n")
  }
  cat("Elapsed time:", format_duration(elapsed), "\n")
  cat("Average per simulation:", round(avg_per_sim, 2), "seconds\n")
  cat("Estimated remaining time:", format_duration(estimated_remaining), "\n")
  cat("Estimated total time:", format_duration(estimated_total), "\n")
  
  if (n_completed < n_total) {
    completion_time <- Sys.time() + estimated_remaining
    cat("Estimated completion:", format(completion_time, "%Y-%m-%d %H:%M:%S"), "\n")
  }
  cat("-------------------------\n\n")
  
  return(list(
    elapsed = elapsed,
    avg_per_sim = avg_per_sim,
    estimated_remaining = estimated_remaining,
    estimated_total = estimated_total,
    completion_time = if (n_completed < n_total) Sys.time() + estimated_remaining else Sys.time()
  ))
}

# ===== ENHANCED PARALLEL FUNCTION WITH DETAILED TIMING =====
run_enhanced_uncertainty_sims_parallel_timed <- function(params,
                                                         total_sims = 100,
                                                         block_size = 20,
                                                         catchability_sd = 1,
                                                         abundance_sd = 1,
                                                         gamma_sd = 1,
                                                         all_species_abundance = TRUE,
                                                         species_to_vary = NULL,
                                                         effort_scen,
                                                         enhanced_steady_tol = 0.025,
                                                         enhanced_steady_t_max = 750,
                                                         spinup_years = 118,
                                                         t_start = 1841,
                                                         sim_years = 170,
                                                         preserve_erepro = TRUE,
                                                         max_erepro = 1,
                                                         n_cores = NULL,
                                                         save_path = "monte_carlo_results_timed.rds",
                                                         verbose = TRUE,
                                                         timing_frequency = 1,  # Show timing every N blocks
                                                         catchability_bias_mean_log = 0,
                                                         abundance_bias_mean_log = 0,
                                                         gamma_bias_mean_log = 0) {
  
  # =============================================================================
  # INITIALIZATION AND SETUP TIMING
  # =============================================================================
  overall_start_time <- Sys.time()
  
  if (verbose) {
    cat("=== TIMED MONTE CARLO SIMULATION ===\n")
    cat("Start time:", format(overall_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("Target simulations:", total_sims, "\n")
    cat("Block size:", block_size, "\n")
  }
  
  setup_start <- Sys.time()
  
  if (is.null(n_cores)) n_cores <- max(1, parallel::detectCores() - 1)
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  parallel::clusterEvalQ(cl, { library(therMizer); library(mizer) })
  parallel::clusterSetRNGStream(cl, iseed = 20250907)
  
  if (!exists("precheck_parameters", inherits = TRUE)) {
    precheck_parameters <- function(...) list(ok = TRUE)
    invisible(precheck_parameters)
  }
  
  parallel::clusterExport(cl, c(
    "params", "effort_scen", "verbose", "precheck_parameters",
    "catchability_bias_mean_log", "abundance_bias_mean_log", "gamma_bias_mean_log"
  ), envir = environment())
  
  parallel::clusterExport(cl, c(
    "run_single_enhanced_sim_with_params", "check_biomass_stability_enhanced", 
    "stability_cv_threshold", "stability_check_years_tail", "stability_min_mean_biomass"
  ), envir = .GlobalEnv)
  
  setup_end <- Sys.time()
  setup_time <- as.numeric(difftime(setup_end, setup_start, units = "secs"))
  
  if (verbose) {
    cat("Setup time:", format_duration(setup_time), "\n")
    cat("Using", n_cores, "cores\n\n")
  }
  
  # =============================================================================
  # MAIN SIMULATION LOOP WITH TIMING
  # =============================================================================
  
  sim_results <- list()
  param_combinations_successful <- list()
  param_combinations_all <- list()
  failed_sims_with_params <- list()
  successful_count <- 0L
  
  # Timing storage
  block_times <- numeric()
  block_success_rates <- numeric()
  
  blocks <- split(1:total_sims, ceiling((1:total_sims) / block_size))
  
  if (verbose) {
    cat("Starting", length(blocks), "blocks of simulations...\n")
    flush.console()
  }
  
  for (bi in seq_along(blocks)) {
    block_start_time <- Sys.time()
    idx <- blocks[[bi]]
    
    if (verbose) { 
      cat("Block ", bi, "/", length(blocks), ": sims ", idx[1], "-", tail(idx,1), " ... ", sep = "")
      flush.console() 
    }
    
    # Run this block in parallel
    results_list <- foreach::foreach(
      i = idx,
      .packages = c("therMizer","mizer"),
      .export = c("run_single_enhanced_sim_with_params","check_biomass_stability_enhanced","precheck_parameters"),
      .errorhandling = "pass"
    ) %dopar% {
      run_single_enhanced_sim_with_params(i, params, catchability_sd, abundance_sd, gamma_sd,
                                          all_species_abundance, species_to_vary, effort_scen,
                                          enhanced_steady_tol, enhanced_steady_t_max,
                                          spinup_years, t_start, sim_years, preserve_erepro, max_erepro,
                                          catchability_bias_mean_log = catchability_bias_mean_log,
                                          abundance_bias_mean_log = abundance_bias_mean_log,
                                          gamma_bias_mean_log = gamma_bias_mean_log,
                                          verbose = FALSE)  # Disable verbose for parallel runs
    }
    
    # Process results
    block_successful <- 0
    for (k in seq_along(results_list)) {
      r <- results_list[[k]]
      current_sim_id <- idx[k]
      
      # Store attempted parameters regardless of success/failure
      if (is.list(r) && !is.null(r$attempted_parameters)) {
        param_combinations_all[[length(param_combinations_all) + 1]] <- r$attempted_parameters
      }
      
      if (is.list(r) && isTRUE(r$success)) {
        successful_count <- successful_count + 1L
        block_successful <- block_successful + 1
        sim_results[[successful_count]] <- r$simulation
        param_combinations_successful[[successful_count]] <- r$parameters
      } else {
        failed_entry <- list(
          sim_id = current_sim_id, 
          error = if (is.list(r)) r$error else "unknown",
          attempted_parameters = if (is.list(r) && !is.null(r$attempted_parameters)) r$attempted_parameters else NULL,
          stability_details = if (is.list(r) && !is.null(r$stability_details)) r$stability_details else NULL
        )
        failed_sims_with_params[[length(failed_sims_with_params) + 1]] <- failed_entry
      }
    }
    
    # Block timing
    block_end_time <- Sys.time()
    block_duration <- as.numeric(difftime(block_end_time, block_start_time, units = "secs"))
    block_times[bi] <- block_duration
    
    block_success_rate <- block_successful / length(idx)
    block_success_rates[bi] <- block_success_rate
    
    if (verbose) {
      cat("completed in", format_duration(block_duration))
      cat(" (", block_successful, "/", length(idx), " successful, ", 
          round(block_success_rate * 100, 1), "%)\n", sep = "")
    }
    
    # Show detailed timing every N blocks
    if (verbose && (bi %% timing_frequency == 0 || bi == length(blocks))) {
      timing_stats <- calculate_timing_stats(
        overall_start_time, 
        block_end_time, 
        idx[length(idx)], 
        total_sims, 
        successful_count
      )
    }
    
    # Save checkpoint with timing info
    checkpoint <- list(
      simulations = sim_results,
      parameters_successful = param_combinations_successful,
      parameters_all_attempted = param_combinations_all,
      failed_with_parameters = failed_sims_with_params,
      n_successful = length(sim_results),
      n_attempts = idx[length(idx)],
      success_rate = length(sim_results) / idx[length(idx)],
      timing_info = list(
        overall_start_time = overall_start_time,
        current_time = block_end_time,
        block_times = block_times,
        block_success_rates = block_success_rates,
        setup_time = setup_time,
        avg_per_sim = sum(block_times) / idx[length(idx)],
        estimated_total_time = if (idx[length(idx)] > 0) sum(block_times) / idx[length(idx)] * total_sims else NA
      )
    )
    saveRDS(checkpoint, file = save_path)
    
    if (verbose && bi < length(blocks)) {
      flush.console()
    }
  }
  
  # =============================================================================
  # FINAL TIMING SUMMARY
  # =============================================================================
  
  overall_end_time <- Sys.time()
  total_elapsed <- as.numeric(difftime(overall_end_time, overall_start_time, units = "secs"))
  
  if (verbose) {
    cat("\n" , rep("=", 60), "\n", sep = "")
    cat("FINAL TIMING SUMMARY\n")
    cat(rep("=", 60), "\n", sep = "")
    cat("Total runtime:", format_duration(total_elapsed), "\n")
    cat("Setup time:", format_duration(setup_time), "\n")
    cat("Simulation time:", format_duration(total_elapsed - setup_time), "\n")
    cat("Average per simulation:", round((total_elapsed - setup_time) / total_sims, 2), "seconds\n")
    cat("Average per successful sim:", round((total_elapsed - setup_time) / successful_count, 2), "seconds\n")
    cat("Fastest block:", format_duration(min(block_times)), "\n")
    cat("Slowest block:", format_duration(max(block_times)), "\n")
    cat("Success rate:", round(successful_count / total_sims * 100, 1), "%\n")
    cat("Completion time:", format(overall_end_time, "%Y-%m-%d %H:%M:%S"), "\n")
    cat(rep("=", 60), "\n\n", sep = "")
  }
  
  # Final comprehensive result with timing
  final_result <- list(
    simulations = sim_results,
    parameters_successful = param_combinations_successful,
    parameters_all_attempted = param_combinations_all,
    failed_with_parameters = failed_sims_with_params,
    n_successful = length(sim_results),
    n_attempts = total_sims,
    success_rate = length(sim_results) / total_sims,
    timing_summary = list(
      total_runtime = total_elapsed,
      setup_time = setup_time,
      simulation_time = total_elapsed - setup_time,
      avg_per_simulation = (total_elapsed - setup_time) / total_sims,
      avg_per_successful = (total_elapsed - setup_time) / successful_count,
      block_times = block_times,
      block_success_rates = block_success_rates,
      start_time = overall_start_time,
      end_time = overall_end_time
    )
  )
  
  return(final_result)
}

# ===== PILOT RUN FUNCTION WITH TIME ESTIMATION =====
run_pilot_with_estimation <- function(params, 
                                      effort_scenario,
                                      pilot_sims = 100,
                                      target_sims = 10000,
                                      catchability_sd = 1,
                                      abundance_sd = 1, 
                                      gamma_sd = 1,
                                      use_bias = FALSE,
                                      n_cores = NULL) {
  
  cat("=== PILOT RUN FOR TIME ESTIMATION ===\n")
  cat("Running", pilot_sims, "pilot simulations to estimate time for", target_sims, "total simulations\n\n")
  
  # Set bias parameters
  if (use_bias) {
    catch_bias <- log(1.2)
    abund_bias <- log(1.2)
    gamma_bias <- log(1.2)
  } else {
    catch_bias <- 0
    abund_bias <- 0
    gamma_bias <- 0
  }
  
  if (is.null(n_cores)) n_cores <- max(1, parallel::detectCores() - 1)
  
  pilot_start_time <- Sys.time()
  
  pilot_results <- run_enhanced_uncertainty_sims_parallel_timed(
    params = params,
    total_sims = pilot_sims,
    block_size = min(20, pilot_sims),
    catchability_sd = catchability_sd,
    abundance_sd = abundance_sd,
    gamma_sd = gamma_sd,
    all_species_abundance = TRUE,
    species_to_vary = NULL,
    effort_scen = effort_scenario,
    enhanced_steady_tol = 0.002,
    enhanced_steady_t_max = 750,
    spinup_years = 118,
    t_start = 1841,
    sim_years = 170,
    preserve_erepro = TRUE,
    max_erepro = 1,
    n_cores = n_cores,
    save_path = paste0("pilot_results_", pilot_sims, ".rds"),
    verbose = TRUE,
    timing_frequency = 1,
    catchability_bias_mean_log = catch_bias,
    abundance_bias_mean_log = abund_bias,
    gamma_bias_mean_log = gamma_bias
  )
  
  pilot_end_time <- Sys.time()
  
  # =============================================================================
  # TIME PROJECTION FOR FULL RUN
  # =============================================================================
  
  pilot_total_time <- pilot_results$timing_summary$total_runtime
  pilot_sim_time <- pilot_results$timing_summary$simulation_time
  pilot_setup_time <- pilot_results$timing_summary$setup_time
  avg_per_sim <- pilot_results$timing_summary$avg_per_simulation
  success_rate <- pilot_results$success_rate
  
  # Estimate full run
  estimated_setup_time <- pilot_setup_time  # Setup time should be similar
  estimated_sim_time <- avg_per_sim * target_sims
  estimated_total_time <- estimated_setup_time + estimated_sim_time
  estimated_successful_sims <- target_sims * success_rate
  
  cat("\n" , rep("=", 70), "\n", sep = "")
  cat("TIME PROJECTION FOR FULL RUN\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("Pilot run results:\n")
  cat("  - Simulations completed:", pilot_sims, "\n")
  cat("  - Successful simulations:", pilot_results$n_successful, 
      " (", round(success_rate * 100, 1), "%)\n", sep = "")
  cat("  - Total pilot time:", format_duration(pilot_total_time), "\n")
  cat("  - Setup time:", format_duration(pilot_setup_time), "\n")
  cat("  - Simulation time:", format_duration(pilot_sim_time), "\n")
  cat("  - Average per simulation:", round(avg_per_sim, 2), "seconds\n\n")
  
  cat("Projections for", target_sims, "simulations:\n")
  cat("  - Estimated setup time:", format_duration(estimated_setup_time), "\n")
  cat("  - Estimated simulation time:", format_duration(estimated_sim_time), "\n")
  cat("  - Estimated total time:", format_duration(estimated_total_time), "\n")
  cat("  - Expected successful sims:", round(estimated_successful_sims), "\n")
  
  if (estimated_total_time > 3600) {
    cat("  - Estimated completion:", format(Sys.time() + estimated_total_time, "%Y-%m-%d %H:%M:%S"), "\n")
  }
  
  # Efficiency recommendations
  cat("\nEfficiency recommendations:\n")
  if (success_rate < 0.3) {
    cat("  - WARNING: Low success rate (", round(success_rate * 100, 1), "%). Consider adjusting parameter SDs.\n", sep = "")
  }
  
  optimal_block_size <- ceiling(sqrt(target_sims / n_cores))
  cat("  - Recommended block size for", target_sims, "sims:", optimal_block_size, "\n")
  
  if (estimated_total_time > 86400) {  # More than 1 day
    recommended_smaller_run <- ceiling(86400 / avg_per_sim)
    cat("  - Consider running", recommended_smaller_run, "sims first (approx 1 day)\n")
  }
  
  cat(rep("=", 70), "\n\n", sep = "")
  
  # Return comprehensive results
  return(list(
    pilot_results = pilot_results,
    projections = list(
      target_sims = target_sims,
      estimated_total_time = estimated_total_time,
      estimated_sim_time = estimated_sim_time,
      estimated_setup_time = estimated_setup_time,
      estimated_successful_sims = estimated_successful_sims,
      avg_per_sim = avg_per_sim,
      success_rate = success_rate,
      recommended_block_size = optimal_block_size
    )
  ))
}

# ===== ENHANCED MAIN FUNCTION WITH TIMING =====
run_monte_carlo_analysis_timed <- function(params, 
                                           effort_scenario,
                                           total_sims = 10000,
                                           pilot_first = TRUE,
                                           pilot_sims = 100,
                                           catchability_sd = 1,
                                           abundance_sd = 1, 
                                           gamma_sd = 1,
                                           use_bias = FALSE,
                                           auto_adjust_block_size = TRUE) {
  
  if (pilot_first && total_sims > pilot_sims) {
    cat("Running pilot estimation first...\n\n")
    
    pilot_analysis <- run_pilot_with_estimation(
      params = params,
      effort_scenario = effort_scenario,
      pilot_sims = pilot_sims,
      target_sims = total_sims,
      catchability_sd = catchability_sd,
      abundance_sd = abundance_sd,
      gamma_sd = gamma_sd,
      use_bias = use_bias
    )
    
    # Ask user if they want to continue
    estimated_hours <- pilot_analysis$projections$estimated_total_time / 3600
    
    if (estimated_hours > 24) {
      cat("WARNING: Estimated runtime is", round(estimated_hours, 1), "hours (", 
          round(estimated_hours/24, 1), "days)\n")
      cat("Consider running a smaller number of simulations first.\n\n")
    }
    
    response <- readline(prompt = paste0("Continue with full run of ", total_sims, " simulations? (y/n): "))
    if (tolower(substr(response, 1, 1)) != "y") {
      cat("Stopping. You can adjust parameters and run again.\n")
      return(pilot_analysis)
    }
    
    # Use pilot recommendations
    if (auto_adjust_block_size) {
      recommended_block_size <- pilot_analysis$projections$recommended_block_size
      cat("Using recommended block size:", recommended_block_size, "\n\n")
    } else {
      recommended_block_size <- min(100, total_sims)
    }
    
  } else {
    recommended_block_size <- min(100, total_sims)
  }
  
  # Set bias parameters
  if (use_bias) {
    catch_bias <- log(1.2)
    abund_bias <- log(1.2)
    gamma_bias <- log(1.2)
  } else {
    catch_bias <- 0
    abund_bias <- 0
    gamma_bias <- 0
  }
  
  cat("=== FULL MONTE CARLO RUN ===\n")
  cat("Starting", total_sims, "simulations with comprehensive timing...\n\n")
  
  full_results <- run_enhanced_uncertainty_sims_parallel_timed(
    params = params,
    total_sims = total_sims,
    block_size = recommended_block_size,
    catchability_sd = catchability_sd,
    abundance_sd = abundance_sd,
    gamma_sd = gamma_sd,
    all_species_abundance = TRUE,
    species_to_vary = NULL,
    effort_scen = effort_scenario,
    enhanced_steady_tol = 0.002,
    enhanced_steady_t_max = 750,
    spinup_years = 118,
    t_start = 1841,
    sim_years = 170,
    preserve_erepro = TRUE,
    max_erepro = 1,
    n_cores = NULL,
    save_path = "monte_carlo_results_timed_final.rds",
    verbose = TRUE,
    timing_frequency = max(1, floor(total_sims / (recommended_block_size * 10))),
    catchability_bias_mean_log = catch_bias,
    abundance_bias_mean_log = abund_bias,
    gamma_bias_mean_log = gamma_bias
  )
  
  return(full_results)
}

# ===== QUICK PILOT FUNCTION =====
quick_pilot <- function(params, effort_scenario, pilot_sims = 100) {
  cat("=== QUICK PILOT RUN ===\n")
  return(run_pilot_with_estimation(
    params = params,
    effort_scenario = effort_scenario,
    pilot_sims = pilot_sims,
    target_sims = 10000,
    catchability_sd = 3,
    abundance_sd = 6,
    gamma_sd = 5,
    use_bias = FALSE
  ))
}

# # ===== EXAMPLE USAGE =====
# # Quick pilot to estimate timing
# params <- readRDS("params_sel_adj.rds")
# pilot_results <- quick_pilot(params, combined_effort_array, pilot_sims = 100)
# 
# # Full run with timing (will run pilot first)
# MC_results_timed <- run_monte_carlo_analysis_timed(
#   params = params,
#   effort_scenario = combined_effort_array,
#   total_sims = 10000,
#   pilot_first = TRUE,      # Run pilot estimation first
#   pilot_sims = 100,        # Size of pilot run
#   catchability_sd = 3,
#   abundance_sd = 6,
#   gamma_sd = 5,
#   use_bias = FALSE
# )

# ===== ENHANCED SINGLE SIMULATION FUNCTION WITH COMPLETE PARAMETER STORAGE =====
run_single_enhanced_sim_with_params <- function(i, params, catchability_sd, abundance_sd, gamma_sd,
                                                all_species_abundance, species_to_vary,
                                                effort_scen, enhanced_steady_tol, enhanced_steady_t_max,
                                                spinup_years, t_start, sim_years, preserve_erepro, max_erepro,
                                                catchability_bias_mean_log = 0,
                                                abundance_bias_mean_log = 0,
                                                gamma_bias_mean_log = 0,
                                                verbose = FALSE) {
  
  result <- tryCatch({
    set.seed(20250907 + i)
    rand_params <- params
    
    # =============================================================================
    # STEP 1: SAMPLE AND STORE ALL PARAMETERS (BEFORE ANY POTENTIAL FAILURES)
    # =============================================================================
    
    # Initialize storage for all attempted parameters
    attempted_params <- list(
      sim_id = i,
      original_values = list(),
      sampled_values = list(),
      multiplicative_factors = list()
    )
    
    # Step 1A: Sample and store catchability parameters
    gear_df <- gear_params(rand_params)
    original_catchability <- gear_df$catchability
    attempted_params$original_values$catchability <- original_catchability
    
    catchability_factors <- numeric(nrow(gear_df))
    if(nrow(gear_df) > 0) {
      for (j in seq_len(nrow(gear_df))) {
        if(gear_df$catchability[j] > 0) {
          mult <- exp(rnorm(1, mean = catchability_bias_mean_log, sd = catchability_sd))
          catchability_factors[j] <- mult
          gear_df$catchability[j] <- gear_df$catchability[j] * mult
          gear_df$catchability[j] <- min(1, max(0, gear_df$catchability[j]))
        } else {
          catchability_factors[j] <- 1  # No change for zero catchability
        }
      }
      gear_df$catchability <- pmin(1, pmax(0, gear_df$catchability))
      gear_params(rand_params) <- gear_df
    }
    
    attempted_params$sampled_values$catchability <- gear_df$catchability
    attempted_params$multiplicative_factors$catchability <- catchability_factors
    
    # Step 1B: Sample and store gamma parameters
    sp_params <- species_params(rand_params)
    original_gamma <- sp_params$gamma
    attempted_params$original_values$gamma <- original_gamma
    
    gamma_factors <- numeric(nrow(sp_params))
    for (j in seq_len(nrow(sp_params))) {
      mult <- exp(rnorm(1, mean = gamma_bias_mean_log, sd = gamma_sd))
      gamma_factors[j] <- mult
      lower <- original_gamma[j] * 0.8
      upper <- original_gamma[j] * 100
      sp_params$gamma[j] <- original_gamma[j] * mult
      sp_params$gamma[j] <- min(upper, max(lower, sp_params$gamma[j]))
    }
    species_params(rand_params) <- sp_params
    
    attempted_params$sampled_values$gamma <- sp_params$gamma
    attempted_params$multiplicative_factors$gamma <- gamma_factors
    
    # Step 1C: Sample and store abundance parameters
    all_species <- sp_params$species
    original_initial_n <- rand_params@initial_n
    attempted_params$original_values$initial_n <- original_initial_n
    
    if (all_species_abundance) {
      scaling_factors <- exp(rnorm(length(all_species), mean = abundance_bias_mean_log, sd = abundance_sd))
      scaling_factors <- pmax(0.5, scaling_factors)
      for (k in seq_along(all_species)) {
        rand_params@initial_n[k,] <- rand_params@initial_n[k,] * scaling_factors[k]
      }
    } else {
      scaling_factors <- rep(1, length(all_species))
      names(scaling_factors) <- all_species
      species_indices <- which(all_species %in% species_to_vary)
      if (length(species_indices) > 0) {
        for (idx in seq_along(species_indices)) {
          k <- species_indices[idx]
          species_scaling <- exp(rnorm(1, mean = abundance_bias_mean_log, sd = abundance_sd))
          species_scaling <- max(0.5, species_scaling)
          scaling_factors[k] <- species_scaling
          rand_params@initial_n[k,] <- rand_params@initial_n[k,] * species_scaling
        }
      }
    }
    
    attempted_params$sampled_values$initial_n <- rand_params@initial_n
    attempted_params$multiplicative_factors$abundance_scaling <- scaling_factors
    attempted_params$species_names <- all_species
    
    # =============================================================================
    # STEP 2: PROCEED WITH SIMULATION (SAME AS BEFORE)
    # =============================================================================
    
    # Pre-check parameters (if function exists)
    if (exists("precheck_parameters", inherits = TRUE)) {
      pc <- try(precheck_parameters(rand_params, effort_scen, sim_years = sim_years, 
                                    min_mature_g = 1, verbose = FALSE), silent = TRUE)
      if (inherits(pc, "try-error")) {
        return(list(success = FALSE, 
                    error = paste0("precheck error: ", as.character(pc)),
                    attempted_parameters = attempted_params))
      }
      if (!isTRUE(pc$ok)) {
        return(list(success = FALSE, 
                    error = paste0("precheck failed: ", pc$reason),
                    attempted_parameters = attempted_params))
      }
    }
    
    # Run to steady state
    steady_result <- tryCatch({
      if (preserve_erepro) {
        steady_params <- steady(rand_params, tol = enhanced_steady_tol, 
                                t_max = enhanced_steady_t_max, preserve = c("erepro"))
      } else {
        steady_params <- steady(rand_params, tol = enhanced_steady_tol, 
                                t_max = enhanced_steady_t_max)
      }
      
      test_sp_params <- species_params(steady_params)
      if (any(is.na(test_sp_params$gamma)) || any(is.infinite(test_sp_params$gamma))) {
        return(list(success = FALSE, error = "steady state produced invalid gamma values"))
      }
      
      list(success = TRUE, params = steady_params)
      
    }, warning = function(w) {
      if (grepl("did not converge", w$message, ignore.case = TRUE)) {
        return(list(success = FALSE, error = paste("steady state convergence failed:", w$message)))
      } else {
        suppressWarnings({
          if (preserve_erepro) {
            steady_params <- steady(rand_params, tol = enhanced_steady_tol, 
                                    t_max = enhanced_steady_t_max, preserve = c("erepro"))
          } else {
            steady_params <- steady(rand_params, tol = enhanced_steady_tol, 
                                    t_max = enhanced_steady_t_max)
          }
        })
        list(success = TRUE, params = steady_params, warning = w$message)
      }
    }, error = function(e) {
      return(list(success = FALSE, error = paste("steady state error:", e$message)))
    })
    
    if (!steady_result$success) {
      return(list(success = FALSE, 
                  error = steady_result$error,
                  attempted_parameters = attempted_params))
    }
    
    rand_params <- steady_result$params
    
    # Additional parameter validation
    if (!preserve_erepro) {
      current_erepro <- tryCatch({
        species_params(rand_params)$erepro
      }, error = function(e) {
        return(rep(NA, nrow(species_params(rand_params))))
      })
      
      if (any(is.na(current_erepro))) {
        return(list(success = FALSE, 
                    error = "erepro values contain NA after steady state",
                    attempted_parameters = attempted_params))
      }
      
      if (any(current_erepro > max_erepro, na.rm = TRUE)) {
        return(list(success = FALSE, 
                    error = "erepro constraint violated",
                    attempted_parameters = attempted_params))
      }
    }
    
    # Run unfished spinup
    sim_spinup <- tryCatch({
      project(rand_params, 
              t_start = t_start, 
              t_max = t_start + spinup_years - 1,
              effort = 0)
    }, error = function(e) {
      return(list(error = paste("project failed:", e$message)))
    })
    
    if(is.list(sim_spinup) && !is.null(sim_spinup$error)) {
      return(list(success = FALSE, 
                  error = sim_spinup$error,
                  attempted_parameters = attempted_params))
    }
    
    if(!is(sim_spinup, "MizerSim")) {
      return(list(success = FALSE, 
                  error = "project returned invalid object type",
                  attempted_parameters = attempted_params))
    }
    
    # Test biomass stability
    stab <- check_biomass_stability_enhanced(
      sim_spinup,
      tolerance = stability_cv_threshold,
      check_years = stability_check_years_tail,
      stability_start_year = 31,
      stability_end_year = 80,
      slope_threshold = 0.02,
      min_mean_biomass = stability_min_mean_biomass,
      verbose = verbose,
      sim_id = i
    )
    
    if (!isTRUE(stab$stable)) {
      fail_sp <- c(names(stab$fail_cv)[stab$fail_cv], names(stab$fail_trend)[stab$fail_trend])
      fail_sp <- unique(fail_sp)
      return(list(success = FALSE, 
                  error = paste("stability test failed:", paste(fail_sp, collapse = ", ")),
                  attempted_parameters = attempted_params,
                  stability_details = stab))
    }
    
    # Run main simulation with fishing
    n_time_steps <- dim(sim_spinup@n)[1]
    final_initial_n <- sim_spinup@n[n_time_steps,,]
    final_n_pp <- sim_spinup@n_pp[n_time_steps,]
    
    sim <- tryCatch({
      project(rand_params,
              initial_n = final_initial_n,
              initial_n_pp = final_n_pp,
              t_start = t_start + spinup_years,
              t_max = t_start + spinup_years + sim_years - 1,
              effort = effort_scen)
    }, error = function(e) {
      return(list(error = paste("main simulation failed:", e$message)))
    })
    
    if(is.list(sim) && !is.null(sim$error)) {
      return(list(success = FALSE, 
                  error = sim$error,
                  attempted_parameters = attempted_params))
    }
    
    if(!is(sim, "MizerSim")) {
      return(list(success = FALSE, 
                  error = "main simulation returned invalid object type",
                  attempted_parameters = attempted_params))
    }
    
    # Return successful result WITH attempted parameters
    return(list(
      success = TRUE,
      simulation = sim,
      parameters = list(
        sim_id = i,
        catchability = gear_df$catchability,
        catchability_change = gear_df$catchability / original_catchability,
        gamma_values = sp_params$gamma,
        gamma_change = sp_params$gamma / original_gamma,
        gamma_change_raw = gamma_factors,
        abundance_scaling = scaling_factors,
        species_names = all_species,
        max_cv = if (!is.null(stab$cv_tail)) max(stab$cv_tail, na.rm = TRUE) else NA_real_,
        stable = TRUE,
        stability_details = stab
      ),
      attempted_parameters = attempted_params  # Include all attempted parameters
    ))
    
  }, error = function(e) {
    # For unexpected errors, we might not have attempted_params yet
    if (exists("attempted_params", inherits = FALSE)) {
      return(list(success = FALSE, 
                  error = paste("Unexpected error in sim", i, ":", e$message),
                  attempted_parameters = attempted_params))
    } else {
      return(list(success = FALSE, 
                  error = paste("Unexpected error in sim", i, ":", e$message)))
    }
  })
  
  return(result)
}

# ===== ENHANCED PARALLEL FUNCTION WITH COMPLETE STORAGE =====
run_enhanced_uncertainty_sims_parallel_complete <- function(params,
                                                            total_sims = 10000,  # Set to 10,000
                                                            block_size = 100,     # Larger blocks for efficiency
                                                            catchability_sd = 1,
                                                            abundance_sd = 1,
                                                            gamma_sd = 1,
                                                            all_species_abundance = TRUE,
                                                            species_to_vary = NULL,
                                                            effort_scen,
                                                            enhanced_steady_tol = 0.025,
                                                            enhanced_steady_t_max = 750,
                                                            spinup_years = 118,
                                                            t_start = 1841,
                                                            sim_years = 170,
                                                            preserve_erepro = TRUE,
                                                            max_erepro = 1,
                                                            n_cores = NULL,
                                                            save_path = "monte_carlo_results_complete.rds",
                                                            verbose = TRUE,
                                                            catchability_bias_mean_log = 0,
                                                            abundance_bias_mean_log = 0,
                                                            gamma_bias_mean_log = 0) {
  
  if (is.null(n_cores)) n_cores <- max(1, parallel::detectCores() - 1)
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  parallel::clusterEvalQ(cl, { library(therMizer); library(mizer) })
  parallel::clusterSetRNGStream(cl, iseed = 20250907)
  
  if (!exists("precheck_parameters", inherits = TRUE)) {
    precheck_parameters <- function(...) list(ok = TRUE)
    invisible(precheck_parameters)
  }
  
  parallel::clusterExport(cl, c(
    "params", "effort_scen", "verbose", "precheck_parameters",
    "catchability_bias_mean_log", "abundance_bias_mean_log", "gamma_bias_mean_log"
  ), envir = environment())
  
  parallel::clusterExport(cl, c(
    "run_single_enhanced_sim_with_params", "check_biomass_stability_enhanced", 
    "stability_cv_threshold", "stability_check_years_tail", "stability_min_mean_biomass"
  ), envir = .GlobalEnv)
  
  # =============================================================================
  # ENHANCED STORAGE: Separate containers for all attempted parameters
  # =============================================================================
  sim_results <- list()
  param_combinations_successful <- list()
  param_combinations_all <- list()        # NEW: Store ALL attempted parameters
  failed_sims_with_params <- list()       # NEW: Enhanced failed sim storage
  successful_count <- 0L
  
  if (verbose) {
    cat("Starting complete MC analysis: total_sims=", total_sims, ", block_size=", block_size,
        ", n_cores=", n_cores, "\n", sep = "")
    flush.console()
  }
  
  blocks <- split(1:total_sims, ceiling((1:total_sims) / block_size))
  for (bi in seq_along(blocks)) {
    idx <- blocks[[bi]]
    if (verbose) { 
      cat("Block ", bi, "/", length(blocks), ": sims ", idx[1], "-", tail(idx,1), " ...\n", sep = "")
      flush.console() 
    }
    
    results_list <- foreach::foreach(
      i = idx,
      .packages = c("therMizer","mizer"),
      .export = c("run_single_enhanced_sim_with_params","check_biomass_stability_enhanced","precheck_parameters"),
      .errorhandling = "pass"
    ) %dopar% {
      run_single_enhanced_sim_with_params(i, params, catchability_sd, abundance_sd, gamma_sd,
                                          all_species_abundance, species_to_vary, effort_scen,
                                          enhanced_steady_tol, enhanced_steady_t_max,
                                          spinup_years, t_start, sim_years, preserve_erepro, max_erepro,
                                          catchability_bias_mean_log = catchability_bias_mean_log,
                                          abundance_bias_mean_log = abundance_bias_mean_log,
                                          gamma_bias_mean_log = gamma_bias_mean_log,
                                          verbose = verbose)
    }
    
    # Enhanced result processing
    for (k in seq_along(results_list)) {
      r <- results_list[[k]]
      current_sim_id <- idx[k]
      
      # Store attempted parameters regardless of success/failure
      if (is.list(r) && !is.null(r$attempted_parameters)) {
        param_combinations_all[[length(param_combinations_all) + 1]] <- r$attempted_parameters
      }
      
      if (is.list(r) && isTRUE(r$success)) {
        # Successful simulation
        successful_count <- successful_count + 1L
        sim_results[[successful_count]] <- r$simulation
        param_combinations_successful[[successful_count]] <- r$parameters
      } else {
        # Failed simulation with enhanced storage
        failed_entry <- list(
          sim_id = current_sim_id, 
          error = if (is.list(r)) r$error else "unknown",
          attempted_parameters = if (is.list(r) && !is.null(r$attempted_parameters)) r$attempted_parameters else NULL,
          stability_details = if (is.list(r) && !is.null(r$stability_details)) r$stability_details else NULL
        )
        failed_sims_with_params[[length(failed_sims_with_params) + 1]] <- failed_entry
      }
    }
    
    # Enhanced checkpoint
    checkpoint <- list(
      simulations = sim_results,
      parameters_successful = param_combinations_successful,
      parameters_all_attempted = param_combinations_all,     # NEW
      failed_with_parameters = failed_sims_with_params,      # NEW
      n_successful = length(sim_results),
      n_attempts = idx[length(idx)],
      success_rate = length(sim_results) / idx[length(idx)]
    )
    saveRDS(checkpoint, file = save_path)
    
    if (verbose) {
      success_rate <- round(length(sim_results) / idx[length(idx)] * 100, 1)
      cat("Checkpoint saved: ", length(sim_results), "/", idx[length(idx)], 
          " successful (", success_rate, "%)\n", sep = "")
      flush.console()
    }
  }
  
  # Final comprehensive result
  list(
    simulations = sim_results,
    parameters_successful = param_combinations_successful,
    parameters_all_attempted = param_combinations_all,     # All attempted parameters
    failed_with_parameters = failed_sims_with_params,      # Failed sims with their parameters
    n_successful = length(sim_results),
    n_attempts = total_sims,
    success_rate = length(sim_results) / total_sims
  )
}

# ===== UPDATED MAIN FUNCTION FOR 10,000 ITERATIONS =====
run_monte_carlo_analysis_complete <- function(params, 
                                              effort_scenario,
                                              total_sims = 10000,      # Set to 10,000
                                              catchability_sd = 1,
                                              abundance_sd = 1, 
                                              gamma_sd = 1,
                                              use_bias = FALSE) {
  
  cat("=== COMPLETE MONTE CARLO ANALYSIS (10,000 iterations) ===\n")
  cat("Running", total_sims, "simulations with complete parameter storage\n")
  cat("Parameter SDs - Catchability:", catchability_sd, 
      "Abundance:", abundance_sd, "Gamma:", gamma_sd, "\n")
  cat("Using bias:", use_bias, "\n\n")
  
  # Set bias parameters
  if (use_bias) {
    catch_bias <- log(1.2)
    abund_bias <- log(1.2)
    gamma_bias <- log(1.2)
  } else {
    catch_bias <- 0
    abund_bias <- 0
    gamma_bias <- 0
  }
  
  available_cores <- parallel::detectCores()
  cores_to_use <- max(1, available_cores - 1)
  cat("Using", cores_to_use, "cores for parallel processing\n")
  
  t_start_mc <- Sys.time()
  
  monte_carlo_results <- run_enhanced_uncertainty_sims_parallel_complete(
    params = params,
    total_sims = total_sims,
    block_size = min(100, total_sims),
    catchability_sd = catchability_sd,
    abundance_sd = abundance_sd,
    gamma_sd = gamma_sd,
    all_species_abundance = TRUE,
    species_to_vary = NULL,
    effort_scen = effort_scenario,
    enhanced_steady_tol = 0.002,
    enhanced_steady_t_max = 750,
    spinup_years = 118,
    t_start = 1841,
    sim_years = 170,
    preserve_erepro = TRUE,
    max_erepro = 1,
    n_cores = cores_to_use,
    save_path = "monte_carlo_results_complete.rds",
    verbose = TRUE,
    catchability_bias_mean_log = catch_bias,
    abundance_bias_mean_log = abund_bias,
    gamma_bias_mean_log = gamma_bias
  )
  
  t_end_mc <- Sys.time()
  elapsed_min <- as.numeric(difftime(t_end_mc, t_start_mc, units = "mins"))
  
  cat("\n=== COMPLETE RESULTS ===\n")
  cat("Successful simulations:", monte_carlo_results$n_successful, "\n")
  cat("Failed simulations:", length(monte_carlo_results$failed_with_parameters), "\n")
  cat("Total attempted parameters stored:", length(monte_carlo_results$parameters_all_attempted), "\n")
  cat("Success rate:", round(monte_carlo_results$success_rate * 100, 1), "%\n")
  cat("Runtime:", round(elapsed_min, 2), "minutes\n")
  
  # Failure analysis
  if (length(monte_carlo_results$failed_with_parameters) > 0) {
    failure_reasons <- sapply(monte_carlo_results$failed_with_parameters, function(x) x$error)
    reason_table <- table(failure_reasons)
    cat("\nFailure reasons:\n")
    for (reason in names(reason_table)) {
      cat("  ", substr(reason, 1, 50), "...: ", reason_table[reason], "\n", sep="")
    }
  }
  
  saveRDS(monte_carlo_results, file = "monte_carlo_results_complete_final.rds")
  cat("\nComplete results saved to: monte_carlo_results_complete_final.rds\n")
  
  return(monte_carlo_results)
}

# ===== EXAMPLE USAGE FOR 10,000 ITERATIONS =====
# params <- readRDS("params_sel_adj.rds")
# 
# MC_results_complete <- run_monte_carlo_analysis_complete(
#   params = params,
#   effort_scenario = combined_effort_array,
#   total_sims = 10000,        # 10,000 iterations
#   catchability_sd = 3,
#   abundance_sd = 6,
#   gamma_sd = 5,
#   use_bias = FALSE           # Mean = 0 (multiplicative factor = 1)
# )

# ===== CORRECTED MONTE CARLO ANALYSIS FOR MIZER =====

# ===== STABILITY SETTINGS (UPDATED FOR OPTIMIZATION APPROACH) =====
stability_cv_threshold <- 0.15                # CV threshold for tail biomass stability  
stability_check_years_tail <- 40              # Years for CV check during spinup
stability_min_mean_biomass <- 1e9             # Minimum biomass to consider (grams)

# ===== STABILITY CHECK FUNCTION =====
check_biomass_stability_enhanced <- function(sim_object,
                                             tolerance = stability_cv_threshold,
                                             check_years = stability_check_years_tail,
                                             stability_start_year = 31,     # Skip first 30 years (1841-1870)
                                             stability_end_year = 80,       # End at 1920
                                             slope_threshold = 0.02,        # 2% threshold for Monte Carlo
                                             min_mean_biomass = stability_min_mean_biomass,
                                             verbose = FALSE,
                                             sim_id = NULL) {
  
  # CHECK IF SIMULATION OBJECT IS VALID FIRST
  if(!is(sim_object, "MizerSim")) {
    if(verbose && !is.null(sim_id)) {
      cat("Sim", sim_id, "failed: invalid simulation object (class:", class(sim_object)[1], ")\n")
    }
    return(list(stable = FALSE, reason = "invalid_simulation_object", 
                object_class = class(sim_object)[1]))
  }
  
  # GET BIOMASS WITH ERROR HANDLING
  bm <- tryCatch({
    getBiomass(sim_object)
  }, error = function(e) {
    if(verbose && !is.null(sim_id)) {
      cat("Sim", sim_id, "failed: getBiomass error -", e$message, "\n")
    }
    return(NULL)
  })
  
  if(is.null(bm)) {
    return(list(stable = FALSE, reason = "getBiomass_failed"))
  }
  
  yrs <- as.numeric(rownames(bm))
  nT <- nrow(bm)
  
  if (nT < 10) {
    return(list(stable = FALSE, reason = "insufficient_time_steps"))
  }
  
  # === TAIL CV CHECK (existing approach) ===
  tail_n <- max(5, min(check_years, nT))
  tail_mat <- tail(bm, tail_n)
  mean_tail <- colMeans(tail_mat, na.rm = TRUE)
  sd_tail <- apply(tail_mat, 2, sd, na.rm = TRUE)
  cv_tail <- ifelse(mean_tail > 0, sd_tail / mean_tail, Inf)
  
  # === OPTIMIZATION-STYLE STABILITY CHECK (1871-1920) ===
  stability_penalty <- 0
  species_trend_failures <- rep(FALSE, ncol(bm))
  species_slopes <- setNames(rep(NA_real_, ncol(bm)), colnames(bm))
  
  max_year <- min(stability_end_year, nT)
  if(max_year >= stability_start_year) {
    stability_years <- stability_start_year:max_year
    stability_biomass <- bm[stability_years, ]
    
    if(nrow(stability_biomass) >= 10) {
      years_numeric <- 1:nrow(stability_biomass)
      
      for(sp in 1:ncol(stability_biomass)) {
        biomass_vals <- stability_biomass[, sp]
        
        # Skip species with zero or NA biomass or below minimum
        mean_biomass <- mean(biomass_vals, na.rm = TRUE)
        if(all(biomass_vals <= 0) || all(is.na(biomass_vals)) || mean_biomass < min_mean_biomass) {
          species_slopes[sp] <- 0
          next
        }
        
        # Use log-biomass to detect exponential trends (same as optimization)
        log_biomass <- log(biomass_vals + 1e-10)
        
        # Fit linear trend if data is valid
        if(length(log_biomass) > 5 && var(log_biomass, na.rm = TRUE) > 0) {
          trend_model <- tryCatch({
            lm(log_biomass ~ years_numeric)
          }, error = function(e) NULL)
          
          if(!is.null(trend_model)) {
            slope <- coef(trend_model)[2]
            species_slopes[sp] <- slope
            
            # Check if slope exceeds threshold
            if(abs(slope) > slope_threshold) {
              species_trend_failures[sp] <- TRUE
              stability_penalty <- stability_penalty + (abs(slope) - slope_threshold)^2
            }
          }
        }
      }
    }
  }
  
  # === DETERMINE OVERALL FAILURES ===
  species <- colnames(bm)
  fail_cv <- cv_tail > tolerance
  fail_trend <- species_trend_failures
  any_fail <- any(fail_cv | fail_trend, na.rm = TRUE)
  
  # === VERBOSE REPORTING ===
  if (verbose && !is.null(sim_id)) {
    if (any(fail_cv, na.rm = TRUE)) {
      cat("Sim", sim_id, "CV failures:", paste(species[which(fail_cv)], collapse = ", "), "\n")
    }
    if (any(fail_trend, na.rm = TRUE)) {
      cat("Sim", sim_id, "Trend failures (1871-1920):", paste(species[which(fail_trend)], collapse = ", "), "\n")
      cat("  Slopes:", paste(round(species_slopes[which(fail_trend)], 4), collapse = ", "), "\n")
    }
  }
  
  # === RETURN RESULTS ===
  list(
    stable = !any_fail,
    fail_cv = fail_cv,
    fail_trend = fail_trend,
    cv_tail = cv_tail,
    log_slopes = species_slopes,
    stability_penalty = stability_penalty,
    slope_threshold = slope_threshold,
    stability_years_used = if(max_year >= stability_start_year) paste(stability_start_year, "-", max_year) else "insufficient_data",
    tolerance_used = tolerance
  )
}

# ===== CORRECTED SINGLE SIMULATION FUNCTION =====
run_single_enhanced_sim <- function(i, params, catchability_sd, abundance_sd, gamma_sd,
                                    all_species_abundance, species_to_vary,
                                    effort_scen, enhanced_steady_tol, enhanced_steady_t_max,
                                    spinup_years, t_start, sim_years, preserve_erepro, max_erepro,
                                    catchability_bias_mean_log = 0,  # CORRECTED: Unbiased default
                                    abundance_bias_mean_log    = 0,  # CORRECTED: Unbiased default
                                    gamma_bias_mean_log        = 0,  # CORRECTED: Unbiased default
                                    verbose = FALSE) {
  
  result <- tryCatch({
    set.seed(20250907 + i)
    rand_params <- params
    
    # Step 1: Randomize catchability
    gear_df <- gear_params(rand_params)
    original_catchability <- gear_df$catchability
    
    if(nrow(gear_df) > 0) {
      for (j in seq_len(nrow(gear_df))) {
        if(gear_df$catchability[j] > 0) {
          mult <- exp(rnorm(1, mean = catchability_bias_mean_log, sd = catchability_sd))
          gear_df$catchability[j] <- gear_df$catchability[j] * mult
          gear_df$catchability[j] <- min(1, max(0, gear_df$catchability[j]))
        }
      }
      gear_df$catchability <- pmin(1, pmax(0, gear_df$catchability))
      gear_params(rand_params) <- gear_df
    }
    
    # Step 2: Randomize gamma
    sp_params <- species_params(rand_params)
    original_gamma <- sp_params$gamma
    gamma_change_raw <- numeric(nrow(sp_params))
    
    for (j in seq_len(nrow(sp_params))) {
      mult <- exp(rnorm(1, mean = gamma_bias_mean_log, sd = gamma_sd))
      gamma_change_raw[j] <- mult
      lower <- original_gamma[j] * 0.8
      upper <- original_gamma[j] * 100
      sp_params$gamma[j] <- original_gamma[j] * mult
      sp_params$gamma[j] <- min(upper, max(lower, sp_params$gamma[j]))
    }
    species_params(rand_params) <- sp_params
    
    # Step 3: Randomize initial abundance (SIMPLIFIED)
    all_species <- sp_params$species
    
    if (all_species_abundance) {
      scaling_factors <- exp(rnorm(length(all_species), mean = abundance_bias_mean_log, sd = abundance_sd))
      scaling_factors <- pmax(0.5, scaling_factors)
      for (k in seq_along(all_species)) {
        rand_params@initial_n[k,] <- rand_params@initial_n[k,] * scaling_factors[k]
      }
    } else {
      scaling_factors <- rep(1, length(all_species))
      names(scaling_factors) <- all_species
      species_indices <- which(all_species %in% species_to_vary)
      if (length(species_indices) > 0) {
        # CORRECTED: Simplified indexing
        for (idx in seq_along(species_indices)) {
          k <- species_indices[idx]
          species_scaling <- exp(rnorm(1, mean = abundance_bias_mean_log, sd = abundance_sd))
          species_scaling <- max(0.5, species_scaling)
          scaling_factors[k] <- species_scaling
          rand_params@initial_n[k,] <- rand_params@initial_n[k,] * species_scaling
        }
      }
    }
    
    # Pre-check parameters (if function exists)
    if (exists("precheck_parameters", inherits = TRUE)) {
      pc <- try(precheck_parameters(rand_params, effort_scen, sim_years = sim_years, 
                                    min_mature_g = 1, verbose = FALSE), silent = TRUE)
      if (inherits(pc, "try-error")) {
        return(list(success = FALSE, error = paste0("precheck error: ", as.character(pc))))
      }
      if (!isTRUE(pc$ok)) {
        return(list(success = FALSE, error = paste0("precheck failed: ", pc$reason)))
      }
    }
    
    # Step 4: Run to steady state
    steady_result <- tryCatch({
      if (preserve_erepro) {
        steady_params <- steady(rand_params, tol = enhanced_steady_tol, 
                                t_max = enhanced_steady_t_max, preserve = c("erepro"))
      } else {
        steady_params <- steady(rand_params, tol = enhanced_steady_tol, 
                                t_max = enhanced_steady_t_max)
      }
      
      test_sp_params <- species_params(steady_params)
      if (any(is.na(test_sp_params$gamma)) || any(is.infinite(test_sp_params$gamma))) {
        return(list(success = FALSE, error = "steady state produced invalid gamma values"))
      }
      
      list(success = TRUE, params = steady_params)
      
    }, warning = function(w) {
      if (grepl("did not converge", w$message, ignore.case = TRUE)) {
        return(list(success = FALSE, error = paste("steady state convergence failed:", w$message)))
      } else {
        # Try to continue with warning
        suppressWarnings({
          if (preserve_erepro) {
            steady_params <- steady(rand_params, tol = enhanced_steady_tol, 
                                    t_max = enhanced_steady_t_max, preserve = c("erepro"))
          } else {
            steady_params <- steady(rand_params, tol = enhanced_steady_tol, 
                                    t_max = enhanced_steady_t_max)
          }
        })
        list(success = TRUE, params = steady_params, warning = w$message)
      }
    }, error = function(e) {
      return(list(success = FALSE, error = paste("steady state error:", e$message)))
    })
    
    if (!steady_result$success) {
      return(list(success = FALSE, error = steady_result$error))
    }
    
    rand_params <- steady_result$params
    
    # Step 4b: Additional parameter validation
    if (!preserve_erepro) {
      current_erepro <- tryCatch({
        species_params(rand_params)$erepro
      }, error = function(e) {
        return(rep(NA, nrow(species_params(rand_params))))
      })
      
      if (any(is.na(current_erepro))) {
        return(list(success = FALSE, error = "erepro values contain NA after steady state"))
      }
      
      if (any(current_erepro > max_erepro, na.rm = TRUE)) {
        return(list(success = FALSE, error = "erepro constraint violated"))
      }
    }
    
    # Step 5: Run unfished spinup
    sim_spinup <- tryCatch({
      project(rand_params, 
              t_start = t_start, 
              t_max = t_start + spinup_years - 1,  # CORRECTED: Proper t_max
              effort = 0)
    }, error = function(e) {
      return(list(error = paste("project failed:", e$message)))
    })
    
    # Check if project succeeded
    if(is.list(sim_spinup) && !is.null(sim_spinup$error)) {
      return(list(success = FALSE, error = sim_spinup$error))
    }
    
    # Verify it's actually a MizerSim object
    if(!is(sim_spinup, "MizerSim")) {
      return(list(success = FALSE, error = "project returned invalid object type"))
    }
    
    # Step 6: Test biomass stability
    stab <- check_biomass_stability_enhanced(
      sim_spinup,
      tolerance = stability_cv_threshold,
      check_years = stability_check_years_tail,
      stability_start_year = 31,
      stability_end_year = 80,
      slope_threshold = 0.02,
      min_mean_biomass = stability_min_mean_biomass,
      verbose = verbose,
      sim_id = i
    )
    
    if (!isTRUE(stab$stable)) {
      fail_sp <- c(names(stab$fail_cv)[stab$fail_cv], names(stab$fail_trend)[stab$fail_trend])
      fail_sp <- unique(fail_sp)
      return(list(success = FALSE, error = paste("stability test failed:", paste(fail_sp, collapse = ", "))))
    }
    
    # Step 7: Run main simulation with fishing
    # CORRECTED: Proper extraction of final state
    n_time_steps <- dim(sim_spinup@n)[1]
    final_initial_n <- sim_spinup@n[n_time_steps,,]
    
    # Also get the final n_pp if needed
    final_n_pp <- sim_spinup@n_pp[n_time_steps,]
    
    sim <- tryCatch({
      project(rand_params,
              initial_n = final_initial_n,
              initial_n_pp = final_n_pp,  # ADDED: Include plankton state
              t_start = t_start + spinup_years,  # CORRECTED: Start after spinup
              t_max = t_start + spinup_years + sim_years - 1,  # CORRECTED: Proper end time
              effort = effort_scen)
    }, error = function(e) {
      return(list(error = paste("main simulation failed:", e$message)))
    })
    
    # Check if main simulation succeeded
    if(is.list(sim) && !is.null(sim$error)) {
      return(list(success = FALSE, error = sim$error))
    }
    
    if(!is(sim, "MizerSim")) {
      return(list(success = FALSE, error = "main simulation returned invalid object type"))
    }
    
    # Return successful result
    return(list(
      success = TRUE,
      simulation = sim,
      parameters = list(
        sim_id = i,
        catchability = gear_df$catchability,
        catchability_change = gear_df$catchability / original_catchability,
        gamma_values = sp_params$gamma,
        gamma_change = sp_params$gamma / original_gamma,
        gamma_change_raw = gamma_change_raw,
        abundance_scaling = scaling_factors,
        species_names = all_species,
        max_cv = if (!is.null(stab$cv_tail)) max(stab$cv_tail, na.rm = TRUE) else NA_real_,
        stable = TRUE,
        stability_details = stab  # ADDED: Include full stability results
      )
    ))
    
  }, error = function(e) {
    return(list(success = FALSE, error = paste("Unexpected error in sim", i, ":", e$message)))
  })
  
  return(result)
}

# ===== PARALLEL MONTE CARLO FUNCTION =====
run_enhanced_uncertainty_sims_parallel_blocked <- function(params,
                                                           total_sims = 100,
                                                           block_size = 20,
                                                           catchability_sd = 1,
                                                           abundance_sd = 1,
                                                           gamma_sd = 1,
                                                           all_species_abundance = TRUE,
                                                           species_to_vary = NULL,
                                                           effort_scen,
                                                           enhanced_steady_tol = 0.025,
                                                           enhanced_steady_t_max = 750,
                                                           spinup_years = 118,
                                                           t_start = 1841,
                                                           sim_years = 170,
                                                           preserve_erepro = TRUE,
                                                           max_erepro = 1,
                                                           n_cores = NULL,
                                                           save_path = "monte_carlo_results_latest.rds",
                                                           verbose = TRUE,
                                                           catchability_bias_mean_log = 0,  # ADDED
                                                           abundance_bias_mean_log = 0,     # ADDED
                                                           gamma_bias_mean_log = 0) {       # ADDED
  
  # Guard checks
  if (!exists("run_single_enhanced_sim", inherits = TRUE)) stop("run_single_enhanced_sim is not defined.")
  if (!exists("check_biomass_stability_enhanced", inherits = TRUE)) stop("check_biomass_stability_enhanced is not defined.")
  
  if (is.null(n_cores)) n_cores <- max(1, parallel::detectCores() - 1)
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  # Load packages and set seed
  parallel::clusterEvalQ(cl, { library(therMizer); library(mizer) })
  parallel::clusterSetRNGStream(cl, iseed = 20250907)
  
  # Provide precheck stub if missing
  if (!exists("precheck_parameters", inherits = TRUE)) {
    precheck_parameters <- function(...) list(ok = TRUE)
    invisible(precheck_parameters)
  }
  
  # Export function environment variables
  parallel::clusterExport(cl, c(
    "params", "effort_scen", "verbose", "precheck_parameters",
    "catchability_bias_mean_log", "abundance_bias_mean_log", "gamma_bias_mean_log"  # ADDED
  ), envir = environment())
  
  # Export global environment functions and settings
  parallel::clusterExport(cl, c(
    "run_single_enhanced_sim", "check_biomass_stability_enhanced", 
    "stability_cv_threshold", "stability_check_years_tail", "stability_min_mean_biomass"
  ), envir = .GlobalEnv)
  
  # Initialize accumulators
  sim_results <- list()
  param_combinations <- list()
  failed_sims <- list()
  successful_count <- 0L
  
  if (verbose) {
    cat("Starting blocked MC: total_sims=", total_sims, ", block_size=", block_size,
        ", n_cores=", n_cores, "\n", sep = "")
    flush.console()
  }
  
  blocks <- split(1:total_sims, ceiling((1:total_sims) / block_size))
  for (bi in seq_along(blocks)) {
    idx <- blocks[[bi]]
    if (verbose) { 
      cat("Block ", bi, "/", length(blocks), ": sims ", idx[1], "-", tail(idx,1), " ...\n", sep = "")
      flush.console() 
    }
    
    # Run this block in parallel
    results_list <- foreach::foreach(
      i = idx,
      .packages = c("therMizer","mizer"),
      .export = c("run_single_enhanced_sim","check_biomass_stability_enhanced","precheck_parameters"),
      .errorhandling = "pass"
    ) %dopar% {
      run_single_enhanced_sim(i, params, catchability_sd, abundance_sd, gamma_sd,
                              all_species_abundance, species_to_vary, effort_scen,
                              enhanced_steady_tol, enhanced_steady_t_max,
                              spinup_years, t_start, sim_years, preserve_erepro, max_erepro,
                              catchability_bias_mean_log = catchability_bias_mean_log,  # ADDED
                              abundance_bias_mean_log = abundance_bias_mean_log,        # ADDED
                              gamma_bias_mean_log = gamma_bias_mean_log,                # ADDED
                              verbose = verbose)
    }
    
    # Process results
    for (k in seq_along(results_list)) {
      r <- results_list[[k]]
      if (is.list(r) && isTRUE(r$success)) {
        successful_count <- successful_count + 1L
        sim_results[[successful_count]] <- r$simulation
        param_combinations[[successful_count]] <- r$parameters
      } else {
        failed_sims[[length(failed_sims) + 1]] <- list(
          sim_id = idx[k], 
          error = if (is.list(r)) r$error else "unknown"
        )
      }
    }
    
    # Save checkpoint
    checkpoint <- list(
      simulations = sim_results,
      parameters = param_combinations,
      failed = failed_sims,
      n_successful = length(sim_results),
      n_attempts = idx[length(idx)]
    )
    saveRDS(checkpoint, file = save_path)
    
    if (verbose) {
      cat("Saved checkpoint to ", save_path, " (", length(sim_results), "/", idx[length(idx)], " successful)\n", sep = "")
      flush.console()
    }
  }
  
  # Return final aggregate
  list(
    simulations = sim_results,
    parameters = param_combinations,
    failed = failed_sims,
    n_successful = length(sim_results),
    n_attempts = total_sims
  )
}

# ===== CORRECTED EXECUTION FUNCTION =====
run_monte_carlo_analysis <- function(params, 
                                     effort_scenario,  # ADDED: Explicit parameter
                                     total_sims = 20,
                                     catchability_sd = 1,
                                     abundance_sd = 1, 
                                     gamma_sd = 1,
                                     use_bias = FALSE) {  # ADDED: Option to control bias
  
  cat("=== MONTE CARLO ANALYSIS WITH OPTIMIZATION-STYLE STABILITY ===\n")
  cat("Running", total_sims, "simulations with consistent parameters\n")
  cat("Parameter SDs - Catchability:", catchability_sd, 
      "Abundance:", abundance_sd, "Gamma:", gamma_sd, "\n")
  cat("Using bias:", use_bias, "\n\n")
  
  # Set bias parameters
  if (use_bias) {
    catch_bias <- log(1.2)
    abund_bias <- log(1.2)
    gamma_bias <- log(1.2)
  } else {
    catch_bias <- 0
    abund_bias <- 0
    gamma_bias <- 0
  }
  
  # Check available cores
  available_cores <- parallel::detectCores()
  cores_to_use <- max(1, available_cores - 1)
  cat("Using", cores_to_use, "cores for parallel processing\n")
  
  # Run Monte Carlo
  t_start_mc <- Sys.time()
  
  # Make sure we have required functions
  if (!exists("run_enhanced_uncertainty_sims_parallel_blocked")) {
    stop("run_enhanced_uncertainty_sims_parallel_blocked function not found")
  }
  
  monte_carlo_results <- run_enhanced_uncertainty_sims_parallel_blocked(
    params = params,
    total_sims = total_sims,
    block_size = min(10, total_sims),  # Don't exceed total_sims
    catchability_sd = catchability_sd,
    abundance_sd = abundance_sd,
    gamma_sd = gamma_sd,
    all_species_abundance = TRUE,
    species_to_vary = NULL,
    effort_scen = effort_scenario,  # CORRECTED: Use passed parameter
    enhanced_steady_tol = 0.002,
    enhanced_steady_t_max = 750,
    spinup_years = 118,
    t_start = 1841,
    sim_years = 170,
    preserve_erepro = TRUE,
    max_erepro = 1,
    n_cores = cores_to_use,
    save_path = "monte_carlo_results_latest.rds",
    verbose = TRUE,
    catchability_bias_mean_log = catch_bias,  # ADDED: Pass bias parameters
    abundance_bias_mean_log = abund_bias,
    gamma_bias_mean_log = gamma_bias
  )
  
  t_end_mc <- Sys.time()
  elapsed_min <- as.numeric(difftime(t_end_mc, t_start_mc, units = "mins"))
  
  cat("\n=== RESULTS ===\n")
  cat("Successful simulations:", monte_carlo_results$n_successful, "\n")
  cat("Failed simulations:", length(monte_carlo_results$failed), "\n")
  cat("Success rate:", round(monte_carlo_results$n_successful/monte_carlo_results$n_attempts*100, 1), "%\n")
  cat("Runtime:", round(elapsed_min, 2), "minutes\n")
  
  # Print failure reasons summary
  if (length(monte_carlo_results$failed) > 0) {
    failure_reasons <- sapply(monte_carlo_results$failed, function(x) x$error)
    reason_table <- table(failure_reasons)
    cat("\nFailure reasons:\n")
    for (reason in names(reason_table)) {
      cat("  ", substr(reason, 1, 50), "...: ", reason_table[reason], "\n", sep="")
    }
  }
  
  # Save final results
  saveRDS(monte_carlo_results, file = "monte_carlo_results_final.rds")
  cat("\nResults saved to: monte_carlo_results_final.rds\n")
  
  return(monte_carlo_results)
}

# ===== USAGE EXAMPLE =====
# Assuming you have:
# - result_balanced_v4: your optimized mizer params object
# - combined_effort_array: your fishing effort scenario

# MC_results <- run_monte_carlo_analysis(
#   params = result_balanced_v4,
#   effort_scenario = combined_effort_array,  # Pass the effort explicitly
#   total_sims = 10,
#   catchability_sd = 1,
#   abundance_sd = 1,
#   gamma_sd = 1,
#   use_bias = FALSE  # Set to TRUE if you want biased sampling
# )

# Extract the actual MizerParams object
actual_params <- result_balanced_v4$params

# Verify it works
cat("Testing extracted params...\n")
test_gear <- gear_params(actual_params)
test_species <- species_params(actual_params)
cat("  Gear params: OK,", nrow(test_gear), "gears\n")
cat("  Species params: OK,", nrow(test_species), "species\n")

# Quick test projection
test_sim <- project(actual_params, t_start = 1841, t_max = 170, effort = 0)
cat("  Test projection: OK\n")

# Now run the Monte Carlo with the correct params object

MC_results_v3 <- run_monte_carlo_analysis(
  params = params_mean,  # Use the MizerParams object, not the list
  effort_scenario = combined_effort_array,
  total_sims = 2000,
  catchability_sd = 3,
  abundance_sd = 6,
  gamma_sd = 3,
  use_bias = FALSE
)


# Species-weighted optimization starting from previous results
fastOptimParallelSpeciesWeighted <- function(previous_result, stability_weight = 1.0, species_weight = 1.0, 
                                             species_weights = NULL, maxit = 50) {
  
  # Use optimized parameters from previous result as starting point
  params_start <- previous_result$params
  
  # Test species-weighted function first
  cat("=== Testing species-weighted function with previous results ===\n")
  vary_start <- c(
    log10(species_params(params_start)$R_max[1:19]),
    species_params(params_start)$erepro[1:19],
    gear_params(params_start)$catchability[c(4,7,8,11,12,16,17,18,19)]
  )
  
  test_error <- getErrorTimeSeriesSpeciesWeighted(
    vary = vary_start,
    params = params_start,
    dat = yield_ts_tidy,
    obs_biomass_data = obs_biomass_data_complete,
    effort = combined_effort_array,
    tol = 0.005,
    t_max = 500,
    stability_weight = stability_weight,
    species_weight = species_weight,
    species_weights = species_weights
  )
  
  cat("Starting error with species weighting:", test_error, "\n")
  cat("Previous optimization error:", previous_result$optim_result$value, "\n")
  
  # Set up optimization parameters
  params_optim <- setParams(params_start)
  vary <- vary_start  # Start from optimized values
  
  # Same bounds as before
  lower_bounds <- c(rep(-1, 19), rep(0.01, 19), rep(1e-6, 9))
  upper_bounds <- c(rep(21, 19), rep(1.0, 19), rep(1.0, 9))
  
  # Set up parallel cluster
  noCores <- parallel::detectCores() - 1
  cat("Setting up cluster with", noCores, "cores for species-weighted optimization...\n")
  
  cl <- parallel::makeCluster(noCores, setup_timeout = 0.5)
  setDefaultCluster(cl = cl)
  
  tryCatch({
    # Export all required objects
    essential_objects <- c("getErrorTimeSeriesSpeciesWeighted", "yield_ts_tidy", "combined_effort_array", 
                           "obs_biomass_data_complete")
    
    mizer_objects <- c("plankton_forcing", "therMizerEncounter", "therMizerEReproAndGrowth", "therMizerPredRate")
    existing_mizer <- mizer_objects[sapply(mizer_objects, exists, envir = globalenv())]
    
    all_objs <- ls(globalenv())
    custom_functions <- all_objs[sapply(all_objs, function(x) is.function(get(x, globalenv())))]
    
    objects_to_export <- unique(c(essential_objects, existing_mizer, custom_functions))
    
    cat("Exporting", length(objects_to_export), "objects to cluster...\n")
    
    clusterExport(cl, varlist = objects_to_export, envir = globalenv())
    clusterExport(cl, "params_optim", envir = environment())
    
    # Export species weights if provided
    if(!is.null(species_weights)) {
      clusterExport(cl, "species_weights", envir = environment())
    }
    
    clusterEvalQ(cl, {
      library(mizerExperimental)
      library(optimParallel)
      library(reshape2)
      library(therMizer)
    })
    
    cat("Starting species-weighted optimization...\n")
    cat("Parameters: stability_weight =", stability_weight, ", species_weight =", species_weight, "\n")
    
    start_time <- Sys.time()
    
    optim_result <- optimParallel::optimParallel(
      par = vary,
      fn = getErrorTimeSeriesSpeciesWeighted,
      params = params_optim, 
      dat = yield_ts_tidy, 
      effort = combined_effort_array, 
      obs_biomass_data = obs_biomass_data_complete,
      tol = 0.005,
      t_max = 500,
      stability_weight = stability_weight,
      species_weight = species_weight,
      species_weights = species_weights,
      method = "L-BFGS-B", 
      lower = lower_bounds,
      upper = upper_bounds,
      parallel = list(loginfo = TRUE, forward = TRUE),
      control = list(trace = 1, maxit = maxit)
    )
    
    end_time <- Sys.time()
    cat("Species-weighted optimization completed in", difftime(end_time, start_time, units = "mins"), "minutes\n")
    cat("Final error value:", optim_result$value, "\n")
    cat("Convergence code:", optim_result$convergence, "(0 = success)\n")
    
  }, error = function(e) {
    cat("Optimization failed:", e$message, "\n")
    stopCluster(cl)
    stop(e)
  })
  
  stopCluster(cl)
  
  # Apply optimized parameters
  species_params(params_optim)$R_max[1:19] <- 10^optim_result$par[1:19]
  species_params(params_optim)$erepro[1:19] <- optim_result$par[20:38]
  gear_params(params_optim)$catchability[c(4,7,8,11,12,16,17,18,19)] <- optim_result$par[39:47]
  
  # Run final simulation
  cat("Running final simulation...\n")
  sim_optim <- project(params_optim, t_max = 2000)
  
  return(list(sim = sim_optim, optim_result = optim_result, params = params_optim, 
              stability_weight = stability_weight, species_weight = species_weight,
              starting_error = test_error, previous_error = previous_result$optim_result$value))
}

# Wrapper functions for different species emphasis strategies
run_marine_mammal_optimization <- function(previous_result, stability_weight = 1.0, maxit = 50) {
  mammal_weights <- get_marine_mammal_weights(obs_biomass_data_complete)
  return(fastOptimParallelSpeciesWeighted(previous_result, stability_weight = stability_weight,
                                          species_weight = 2.0, species_weights = mammal_weights, 
                                          maxit = maxit))
}

run_balanced_optimization <- function(previous_result, stability_weight = 1.0, maxit = 50) {
  balanced_weights <- get_balanced_weights(obs_biomass_data_complete)
  return(fastOptimParallelSpeciesWeighted(previous_result, stability_weight = stability_weight,
                                          species_weight = 1.5, species_weights = balanced_weights, 
                                          maxit = maxit))
}

run_krill_focused_optimization <- function(previous_result, stability_weight = 1.0, maxit = 50) {
  krill_weights <- get_krill_focused_weights(obs_biomass_data_complete)
  return(fastOptimParallelSpeciesWeighted(previous_result, stability_weight = stability_weight,
                                          species_weight = 2.0, species_weights = krill_weights, 
                                          maxit = maxit))
}



# result_balanced <- run_balanced_optimization(result_stable, stability_weight = 1.0, maxit = 50)
# result_balanced_v2 <- run_marine_mammal_optimization(result_balanced, stability_weight = 1.0, maxit = 300)
# result_balanced_v3 <- run_krill_focused_optimization(result_balanced_v2, stability_weight = 1.0, maxit = 100)
# result_balanced_v4 <- run_marine_mammal_optimization(result_balanced_v3, stability_weight = 1.0, maxit = 300)
# result_balanced_v5 <- run_balanced_optimization(result_balanced_v4, stability_weight = 1.0, maxit = 500)

