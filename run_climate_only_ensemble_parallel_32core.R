# Run Climate-Only Ensemble - Optimised for 32-core VM
# Runs all 2,111 simulations with climate forcing but no fishing
# Optimised for 32 CPU cores and 64 GB RAM

library(therMizer)
library(mizer)
library(parallel)
library(pbapply)

cat("=== Climate-Only Ensemble (Parallel - 32 Core Optimised) ===\n")
cat("Running all MC parameterisations with climate forcing, no fishing\n\n")

# ------------------------------------------------------------------------------
# Configuration - Optimised for 32 cores, 64 GB RAM
# ------------------------------------------------------------------------------
n_cores <- 30  # Use 30 of 32 cores (leave 2 for system)
batch_size <- 300  # Process in batches for checkpointing
spinup_years <- 118

cat("Configuration:\n")
cat("  Cores to use:", n_cores, "\n")
cat("  Batch size:", batch_size, "\n")
cat("  Spinup years:", spinup_years, "\n\n")

# Output settings
output_dir <- "Output_large_files/climate_only_ensemble"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ------------------------------------------------------------------------------
# Load required data
# ------------------------------------------------------------------------------
cat("Loading required data...\n")

# Load the fished MC ensemble (cleaned version with 2111 valid simulations)
mc_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
mc_results <- readRDS(mc_file)
cat("  Loaded:", mc_file, "\n")

fished_sims <- mc_results$simulations
n_sims <- length(fished_sims)
cat("  Found", n_sims, "simulations to process\n")

# Load climate forcings
extended_ocean_temp <- readRDS("temperature_forcing_1841_2010.rds")
extended_n_pp_array <- readRDS("phytoplankton_forcing_1841_2010.rds")
cat("  Loaded temperature forcing:", dim(extended_ocean_temp), "\n")
cat("  Loaded phytoplankton forcing:", dim(extended_n_pp_array), "\n")

# ------------------------------------------------------------------------------
# Check for existing progress
# ------------------------------------------------------------------------------
progress_file <- file.path(output_dir, "progress_parallel.rds")
results_file <- file.path(output_dir, "climate_only_results_partial.rds")

if (file.exists(progress_file)) {
  progress <- readRDS(progress_file)
  completed_indices <- progress$completed
  failed_indices <- progress$failed
  cat("\nResuming from checkpoint:\n")
  cat("  Completed:", length(completed_indices), "\n")
  cat("  Failed:", length(failed_indices), "\n")
} else {
  completed_indices <- integer(0)
  failed_indices <- list()
  cat("\nStarting fresh run\n")
}

remaining_indices <- setdiff(1:n_sims, c(completed_indices, sapply(failed_indices, `[[`, "idx")))
cat("  Remaining:", length(remaining_indices), "\n")

if (length(remaining_indices) == 0) {
  cat("\nAll simulations already completed!\n")
  quit(save = "no")
}

# ------------------------------------------------------------------------------
# Worker function for parallel processing
# ------------------------------------------------------------------------------
run_single_sim <- function(idx) {
  tryCatch({
    # Extract params
    params_original <- fished_sims[[idx]]@params
    
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
    
    # Run main simulation
    sim_climate_only <- project(
      params_climate,
      initial_n = sim_spinup@n[spinup_years, , ],
      t_start = 1841,
      t_max = 170,
      effort = 0
    )
    
    list(success = TRUE, idx = idx, sim = sim_climate_only)
    
  }, error = function(e) {
    list(success = FALSE, idx = idx, error = as.character(e))
  })
}

# ------------------------------------------------------------------------------
# Process in batches
# ------------------------------------------------------------------------------
cat("\n=== Starting Parallel Processing ===\n")
cat("Processing", length(remaining_indices), "simulations across", n_cores, "cores\n\n")

# Split remaining into batches
n_batches <- ceiling(length(remaining_indices) / batch_size)
all_results <- list()

total_start <- Sys.time()

for (batch_num in 1:n_batches) {
  batch_start_idx <- (batch_num - 1) * batch_size + 1
  batch_end_idx <- min(batch_num * batch_size, length(remaining_indices))
  batch_indices <- remaining_indices[batch_start_idx:batch_end_idx]
  
  cat("\n--- Batch", batch_num, "of", n_batches, "---\n")
  cat("Processing indices", min(batch_indices), "to", max(batch_indices), 
      "(", length(batch_indices), "sims)\n")
  
  batch_start <- Sys.time()
  
  # Set up cluster
  cl <- makeCluster(n_cores)
  
  # Export required objects to workers
  clusterExport(cl, c("fished_sims", "extended_ocean_temp", "extended_n_pp_array", 
                      "spinup_years"), envir = environment())
  
  # Load packages on workers
  clusterEvalQ(cl, {
    suppressPackageStartupMessages({
      library(therMizer)
      library(mizer)
    })
  })
  
  # Run batch with progress bar
  batch_results <- pblapply(batch_indices, run_single_sim, cl = cl)
  
  # Stop cluster
  stopCluster(cl)
  
  batch_end <- Sys.time()
  batch_time <- round(difftime(batch_end, batch_start, units = "mins"), 2)
  
  # Process results
  for (res in batch_results) {
    if (res$success) {
      completed_indices <- c(completed_indices, res$idx)
      all_results[[as.character(res$idx)]] <- res$sim
    } else {
      failed_indices <- c(failed_indices, list(list(idx = res$idx, error = res$error)))
      cat("  Failed:", res$idx, "-", res$error, "\n")
    }
  }
  
  # Stats
  batch_successes <- sum(sapply(batch_results, `[[`, "success"))
  cat("Batch completed in", batch_time, "minutes\n")
  cat("  Successes:", batch_successes, "/", length(batch_indices), "\n")
  cat("  Total progress:", length(completed_indices), "/", n_sims, 
      "(", round(length(completed_indices)/n_sims*100, 1), "%)\n")
  
  # Save checkpoint
  cat("Saving checkpoint...")
  progress <- list(
    completed = completed_indices,
    failed = failed_indices,
    last_update = Sys.time()
  )
  saveRDS(progress, progress_file)
  
  # Save partial results
  saveRDS(all_results, results_file)
  cat(" done\n")
  
  # ETA
  elapsed <- difftime(Sys.time(), total_start, units = "mins")
  rate <- length(completed_indices) / as.numeric(elapsed)
  remaining <- n_sims - length(completed_indices)
  eta <- remaining / rate
  cat("  ETA:", round(eta, 1), "minutes\n")
  
  # Memory cleanup
  gc()
}

total_end <- Sys.time()
total_time <- round(difftime(total_end, total_start, units = "mins"), 2)

# ------------------------------------------------------------------------------
# Compile final results
# ------------------------------------------------------------------------------
cat("\n=== Compiling Final Results ===\n")

# Create final output object
final_output <- list(
  simulations = all_results,
  n_successful = length(all_results),
  n_failed = length(failed_indices),
  failed_info = failed_indices,
  simulation_indices = as.integer(names(all_results)),
  settings = list(
    spinup_years = spinup_years,
    effort = 0,
    n_cores = n_cores,
    source_ensemble = mc_file
  ),
  runtime_minutes = as.numeric(total_time),
  created = Sys.time()
)

# Save final results
final_file <- file.path(output_dir, "climate_only_ensemble_final.rds")
saveRDS(final_output, final_file)
cat("Saved final results:", final_file, "\n")

# File size
file_size_gb <- round(file.size(final_file) / 1e9, 2)
cat("File size:", file_size_gb, "GB\n")

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------
cat("\n=== Run Complete ===\n")
cat("Total time:", total_time, "minutes\n")
cat("Successful:", length(all_results), "/", n_sims, "\n")
cat("Failed:", length(failed_indices), "\n")
cat("\nOutput:", final_file, "\n")

# Cleanup temp files
if (file.exists(progress_file)) file.remove(progress_file)
if (file.exists(results_file)) file.remove(results_file)
cat("\nCleaned up temporary files.\n")
