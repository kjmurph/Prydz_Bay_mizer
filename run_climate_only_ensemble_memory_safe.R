# Run Climate-Only Ensemble - Memory-Safe Version
# Reduced parallelism with aggressive memory management

library(therMizer)
library(mizer)
library(parallel)

cat("=== Climate-Only Ensemble (Memory-Safe Parallel) ===\n\n")

# ------------------------------------------------------------------------------
# REDUCED Configuration - Adjust these if still crashing
# ------------------------------------------------------------------------------
n_cores <- 8       # Start low - increase if stable (try 8, 12, 16)
batch_size <- 50   # Smaller batches = more checkpoints, less memory
spinup_years <- 118

cat("Memory-Safe Configuration:\n")
cat("  Cores:", n_cores, "(reduce if crashing)\n")
cat("  Batch size:", batch_size, "\n")
cat("  Spinup years:", spinup_years, "\n\n")

# Output settings
output_dir <- "Output_large_files/climate_only_ensemble"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ------------------------------------------------------------------------------
# Load required data
# ------------------------------------------------------------------------------
cat("Loading required data...\n")

mc_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
mc_results <- readRDS(mc_file)
fished_sims <- mc_results$simulations
n_sims <- length(fished_sims)
cat("  Loaded", n_sims, "simulations\n")

extended_ocean_temp <- readRDS("temperature_forcing_1841_2010.rds")
extended_n_pp_array <- readRDS("phytoplankton_forcing_1841_2010.rds")
cat("  Loaded climate forcings\n")

# Memory status
gc()
mem_info <- gc()
cat("  Memory used:", round(sum(mem_info[,2]), 0), "MB\n\n")

# ------------------------------------------------------------------------------
# Check for existing progress
# ------------------------------------------------------------------------------
progress_file <- file.path(output_dir, "progress_parallel.rds")
sims_dir <- file.path(output_dir, "individual_sims")
if (!dir.exists(sims_dir)) dir.create(sims_dir, recursive = TRUE)

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
cat("  Remaining:", length(remaining_indices), "\n\n")

if (length(remaining_indices) == 0) {
  cat("All simulations already completed!\n")
  quit(save = "no")
}

# ------------------------------------------------------------------------------
# Worker function - saves to disk immediately
# ------------------------------------------------------------------------------
run_single_sim <- function(idx) {
  tryCatch({
    params_original <- fished_sims[[idx]]@params
    
    params_climate <- upgradeTherParams(
      params_original,
      ocean_temp_array = extended_ocean_temp,
      n_pp_array = extended_n_pp_array,
      aerobic_effect = FALSE,
      metabolism_effect = TRUE
    )
    
    sim_spinup <- project(
      params_climate,
      t_start = 1841,
      t_max = spinup_years,
      effort = 0
    )
    
    sim_climate_only <- project(
      params_climate,
      initial_n = sim_spinup@n[spinup_years, , ],
      t_start = 1841,
      t_max = 170,
      effort = 0
    )
    
    # Save immediately to disk (reduces memory pressure)
    sim_file <- file.path(sims_dir, sprintf("sim_%04d_climate_only.rds", idx))
    saveRDS(sim_climate_only, sim_file)
    
    list(success = TRUE, idx = idx)
    
  }, error = function(e) {
    list(success = FALSE, idx = idx, error = as.character(e))
  })
}

# ------------------------------------------------------------------------------
# Process in small batches
# ------------------------------------------------------------------------------
cat("=== Starting Processing ===\n")
cat("Processing", length(remaining_indices), "simulations\n")
cat("Using", n_cores, "cores with batch size", batch_size, "\n\n")

n_batches <- ceiling(length(remaining_indices) / batch_size)
total_start <- Sys.time()

for (batch_num in 1:n_batches) {
  batch_start_idx <- (batch_num - 1) * batch_size + 1
  batch_end_idx <- min(batch_num * batch_size, length(remaining_indices))
  batch_indices <- remaining_indices[batch_start_idx:batch_end_idx]
  
  cat("\n--- Batch", batch_num, "/", n_batches, "---\n")
  cat("Indices:", min(batch_indices), "-", max(batch_indices), 
      "(", length(batch_indices), "sims)\n")
  
  batch_start <- Sys.time()
  
  # FORK cluster is more memory efficient on Linux
  cl <- makeCluster(n_cores, type = "PSOCK")
  
  clusterExport(cl, c("fished_sims", "extended_ocean_temp", "extended_n_pp_array", 
                      "spinup_years", "sims_dir"), envir = environment())
  
  clusterEvalQ(cl, {
    suppressPackageStartupMessages({
      library(therMizer)
      library(mizer)
    })
  })
  
  # Run batch
  batch_results <- parLapply(cl, batch_indices, run_single_sim)
  
  stopCluster(cl)
  
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
  
  batch_time <- round(difftime(Sys.time(), batch_start, units = "mins"), 2)
  cat("Batch time:", batch_time, "mins |",
      "Success:", batch_successes, "/", length(batch_indices), "|",
      "Total:", length(completed_indices), "/", n_sims, 
      "(", round(length(completed_indices)/n_sims*100, 1), "%)\n")
  
  # Save checkpoint
  progress <- list(
    completed = completed_indices,
    failed = failed_indices,
    last_update = Sys.time()
  )
  saveRDS(progress, progress_file)
  
  # Aggressive memory cleanup
  rm(batch_results, cl)
  gc()
  
  # ETA
  elapsed <- as.numeric(difftime(Sys.time(), total_start, units = "mins"))
  if (length(completed_indices) > 0) {
    rate <- length(completed_indices) / elapsed
    remaining <- n_sims - length(completed_indices)
    eta <- remaining / rate
    cat("ETA:", round(eta, 1), "mins remaining\n")
  }
}

total_time <- round(difftime(Sys.time(), total_start, units = "mins"), 2)

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------
cat("\n=== Run Complete ===\n")
cat("Total time:", total_time, "minutes\n")
cat("Successful:", length(completed_indices), "/", n_sims, "\n")
cat("Failed:", length(failed_indices), "\n")
cat("\nResults saved to:", sims_dir, "\n")
cat("Run compile_climate_only_ensemble.R to combine results\n")
