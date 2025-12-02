# Run Climate-Only Ensemble - Local Parallel Version
# Optimised for local machine with moderate parallelism

library(therMizer)
library(mizer)
library(parallel)
library(pbapply)

cat("=== Climate-Only Ensemble (Local Parallel) ===\n\n")

# ------------------------------------------------------------------------------
# Configuration - Adjust for your machine
# ------------------------------------------------------------------------------
n_cores <- min(8, parallel::detectCores() - 2)  # Cap at 8 cores to avoid memory issues
batch_size <- 50   # Process in small batches for checkpointing
spinup_years <- 118

cat("Configuration:\n")
cat("  Cores:", n_cores, "\n")
cat("  Batch size:", batch_size, "\n")
cat("  Spinup years:", spinup_years, "\n\n")

# Output settings
output_dir <- "Output_large_files/climate_only_ensemble"
sims_dir <- file.path(output_dir, "individual_sims")
if (!dir.exists(sims_dir)) dir.create(sims_dir, recursive = TRUE)

# ------------------------------------------------------------------------------
# Load required data
# ------------------------------------------------------------------------------
cat("Loading required data...\n")

mc_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
mc_results <- readRDS(mc_file)
cat("  Loaded:", mc_file, "\n")

# Note: simulations is a list of lists, each containing a MizerSim
fished_sims <- mc_results$simulations
n_sims <- length(fished_sims)
cat("  Found", n_sims, "simulations to process\n")

# Load climate forcings
extended_ocean_temp <- readRDS("temperature_forcing_1841_2010.rds")
extended_n_pp_array <- readRDS("phytoplankton_forcing_1841_2010.rds")
cat("  Loaded climate forcings\n")

# Check memory
gc()
cat("  Initial memory cleaned\n\n")

# ------------------------------------------------------------------------------
# Check for existing progress
# ------------------------------------------------------------------------------
progress_file <- file.path(output_dir, "progress_local_parallel.rds")

if (file.exists(progress_file)) {
  progress <- readRDS(progress_file)
  completed_indices <- progress$completed
  failed_indices <- progress$failed
  cat("Resuming from checkpoint:\n")
  cat("  Completed:", length(completed_indices), "\n")
  cat("  Failed:", length(failed_indices), "\n")
} else {
  completed_indices <- integer(0)
  failed_indices <- list()
  cat("Starting fresh run\n")
}

remaining_indices <- setdiff(1:n_sims, c(completed_indices, sapply(failed_indices, `[[`, "idx")))
cat("  Remaining:", length(remaining_indices), "\n")

if (length(remaining_indices) == 0) {
  cat("\nAll simulations already completed!\n")
  quit(save = "no")
}

# ------------------------------------------------------------------------------
# Worker function - saves result to disk immediately
# ------------------------------------------------------------------------------
run_single_sim <- function(idx) {
  tryCatch({
    # Extract MizerSim (handle both direct and nested structures)
    fished_sim <- fished_sims[[idx]]
    if (is.list(fished_sim) && !inherits(fished_sim, "MizerSim")) {
      fished_sim <- fished_sim[[1]]
    }
    
    params_original <- fished_sim@params
    
    # Upgrade to therMizer
    params_climate <- upgradeTherParams(
      params_original,
      ocean_temp_array = extended_ocean_temp,
      n_pp_array = extended_n_pp_array,
      aerobic_effect = FALSE,
      metabolism_effect = TRUE
    )
    
    # Run spinup
    sim_spinup <- project(
      params_climate,
      t_start = 1841,
      t_max = spinup_years,
      effort = 0
    )
    
    # Run main simulation (169 years to end at 2010)
    sim_climate_only <- project(
      params_climate,
      initial_n = sim_spinup@n[spinup_years, , ],
      t_start = 1841,
      t_max = 169,
      effort = 0
    )
    
    # Verify time range
    end_time <- max(as.numeric(dimnames(sim_climate_only@n)$time))
    if (end_time != 2010) {
      stop("Unexpected end time: ", end_time)
    }
    
    # Save to disk immediately
    sim_file <- file.path(sims_dir, sprintf("sim_%04d_climate_only.rds", idx))
    saveRDS(sim_climate_only, sim_file)
    
    list(success = TRUE, idx = idx, file = sim_file)
    
  }, error = function(e) {
    list(success = FALSE, idx = idx, error = as.character(e))
  })
}

# ------------------------------------------------------------------------------
# Process in batches
# ------------------------------------------------------------------------------
cat("\n=== Starting Parallel Processing ===\n")
cat("Processing", length(remaining_indices), "simulations across", n_cores, "cores\n\n")

n_batches <- ceiling(length(remaining_indices) / batch_size)
total_start <- Sys.time()

for (batch_num in 1:n_batches) {
  batch_start_idx <- (batch_num - 1) * batch_size + 1
  batch_end_idx <- min(batch_num * batch_size, length(remaining_indices))
  batch_indices <- remaining_indices[batch_start_idx:batch_end_idx]
  
  cat("\n--- Batch", batch_num, "of", n_batches, "---\n")
  cat("Indices", min(batch_indices), "-", max(batch_indices), 
      "(", length(batch_indices), "sims)\n")
  
  batch_start <- Sys.time()
  
  # Set up cluster
  cl <- makeCluster(n_cores)
  
  # Export required objects
  clusterExport(cl, c("fished_sims", "extended_ocean_temp", "extended_n_pp_array", 
                      "spinup_years", "sims_dir"), envir = environment())
  
  # Load packages on workers
  invisible(clusterEvalQ(cl, {
    suppressPackageStartupMessages({
      library(therMizer)
      library(mizer)
    })
  }))
  
  # Run batch with progress bar
  batch_results <- pblapply(batch_indices, run_single_sim, cl = cl)
  
  # Stop cluster
  stopCluster(cl)
  
  batch_end <- Sys.time()
  batch_time <- round(difftime(batch_end, batch_start, units = "mins"), 2)
  
  # Process results
  batch_successes <- 0
  for (res in batch_results) {
    if (res$success) {
      completed_indices <- c(completed_indices, res$idx)
      batch_successes <- batch_successes + 1
    } else {
      failed_indices <- c(failed_indices, list(list(idx = res$idx, error = res$error)))
      cat("  FAILED:", res$idx, "-", res$error, "\n")
    }
  }
  
  cat("Batch completed in", batch_time, "minutes\n")
  cat("  Successes:", batch_successes, "/", length(batch_indices), "\n")
  cat("  Total progress:", length(completed_indices), "/", n_sims, 
      "(", round(length(completed_indices)/n_sims*100, 1), "%)\n")
  
  # Save checkpoint
  progress <- list(
    completed = completed_indices,
    failed = failed_indices,
    last_update = Sys.time()
  )
  saveRDS(progress, progress_file)
  
  # ETA
  elapsed <- as.numeric(difftime(Sys.time(), total_start, units = "mins"))
  rate <- length(completed_indices) / elapsed
  remaining <- n_sims - length(completed_indices)
  eta <- remaining / rate
  cat("  ETA:", round(eta, 1), "minutes remaining\n")
  
  # Memory cleanup between batches
  rm(batch_results, cl)
  gc()
}

total_end <- Sys.time()
total_time <- round(difftime(total_end, total_start, units = "mins"), 2)

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------
cat("\n=== Run Complete ===\n")
cat("Total time:", total_time, "minutes\n")
cat("Successful:", length(completed_indices), "/", n_sims, "\n")
cat("Failed:", length(failed_indices), "\n")
cat("\nResults saved to:", sims_dir, "\n")
cat("\nRun compile_climate_only_ensemble.R to combine results.\n")
