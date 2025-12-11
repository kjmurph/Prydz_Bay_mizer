# Run Climate-Only (Unfished) Ensemble Simulations
# Runs all 2,111 accepted MC parameterisations with climate forcing but no fishing
# This creates matched controls for each simulation in the fished ensemble

library(therMizer)
library(mizer)
library(parallel)
library(pbapply)

cat("=== Climate-Only Ensemble Simulations ===\n")
cat("Running all MC parameterisations with climate forcing, no fishing\n\n")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
# Number of cores for parallel processing
n_cores <- parallel::detectCores() - 2  # Leave 2 cores free
cat("Using", n_cores, "cores for parallel processing\n\n")

# Output settings
output_dir <- "Output_large_files/climate_only_ensemble"
checkpoint_interval <- 100  # Save checkpoint every N simulations

# Create output directory
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

# Note: Climate forcings are already embedded in the fished ensemble params
# No need to load them separately

# ------------------------------------------------------------------------------
# Function to run a single climate-only simulation
# ------------------------------------------------------------------------------
run_climate_only_sim <- function(fished_sim) {
  
  tryCatch({
    # Extract the params from the fished simulation
    # The params ALREADY contain:
    # 1. Post-spinup initial_n (from the 3-cycle spinup done during MC generation)
    # 2. Climate forcing arrays (ocean_temp and n_pp)
    # 3. Perturbed species parameters (gamma, catchability, etc.)
    # 
    # For a true matched control, we use these exact params and just set effort=0
    # NO ADDITIONAL SPINUP is needed - that would alter the initial conditions
    params_original <- fished_sim@params
    
    # Run simulation with climate forcing but no fishing
    # This gives us the climate-only response using the EXACT same initial
    # conditions as the corresponding fished simulation
    sim_climate_only <- project(
      params_original,
      t_start = 1841,
      t_max = 170,  # 1841-2010
      effort = 0
    )
    
    return(list(
      success = TRUE,
      simulation = sim_climate_only
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = as.character(e)
    ))
  })
}

# ------------------------------------------------------------------------------
# Check for existing progress
# ------------------------------------------------------------------------------
checkpoint_file <- file.path(output_dir, "checkpoint_climate_only.rds")
completed_file <- file.path(output_dir, "completed_indices.rds")

if (file.exists(completed_file)) {
  completed_indices <- readRDS(completed_file)
  cat("\nFound existing progress:", length(completed_indices), "simulations completed\n")
  remaining_indices <- setdiff(1:n_sims, completed_indices)
  cat("Remaining simulations:", length(remaining_indices), "\n")
} else {
  completed_indices <- integer(0)
  remaining_indices <- 1:n_sims
  cat("\nStarting fresh - no previous progress found\n")
}

# If all done, skip processing
if (length(remaining_indices) == 0) {
  cat("\nAll simulations already completed!\n")
  cat("Loading final results...\n")
  final_results <- readRDS(file.path(output_dir, "climate_only_ensemble_results.rds"))
  cat("Done.\n")
  q(save = "no")
}

# ------------------------------------------------------------------------------
# Run simulations in batches (to enable checkpointing)
# ------------------------------------------------------------------------------
cat("\n=== Starting Climate-Only Simulations ===\n")
cat("Processing", length(remaining_indices), "remaining simulations\n")
cat("Checkpoint interval:", checkpoint_interval, "simulations\n\n")

# Store all results (load existing if available)
if (file.exists(checkpoint_file)) {
  all_results <- readRDS(checkpoint_file)
} else {
  all_results <- vector("list", n_sims)
}

# Process in batches
batch_size <- checkpoint_interval
n_batches <- ceiling(length(remaining_indices) / batch_size)

total_start <- Sys.time()

for (batch_idx in 1:n_batches) {
  # Get indices for this batch
  batch_start <- (batch_idx - 1) * batch_size + 1
  batch_end <- min(batch_idx * batch_size, length(remaining_indices))
  batch_indices <- remaining_indices[batch_start:batch_end]
  
  cat("\n--- Batch", batch_idx, "of", n_batches, "---\n")
  cat("Processing simulations", batch_indices[1], "to", batch_indices[length(batch_indices)], "\n")
  
  batch_start_time <- Sys.time()
  
  # Extract only the sims needed for this batch (avoids serialization issues)
  batch_sims <- fished_sims[batch_indices]
  
  # Set up cluster for parallel processing
  cl <- makeCluster(n_cores)
  clusterEvalQ(cl, {
    library(therMizer)
    library(mizer)
  })
  
  # Run batch in parallel with progress bar
  batch_results <- pblapply(batch_sims, run_climate_only_sim, cl = cl)
  
  # Stop cluster
  stopCluster(cl)
  
  batch_end_time <- Sys.time()
  batch_duration <- difftime(batch_end_time, batch_start_time, units = "mins")
  
  # Store results
  for (i in seq_along(batch_results)) {
    idx <- batch_indices[i]
    all_results[[idx]] <- batch_results[[i]]
    if (batch_results[[i]]$success) {
      completed_indices <- c(completed_indices, idx)
    }
  }
  
  # Count successes/failures in this batch
  batch_successes <- sum(sapply(batch_results, function(x) x$success))
  batch_failures <- length(batch_results) - batch_successes
  
  cat("Batch completed in", round(batch_duration, 2), "minutes\n")
  cat("  Successes:", batch_successes, "| Failures:", batch_failures, "\n")
  cat("  Total completed:", length(completed_indices), "/", n_sims, "\n")
  
  # Save checkpoint
  cat("Saving checkpoint...\n")
  saveRDS(all_results, checkpoint_file)
  saveRDS(completed_indices, completed_file)
  
  # Estimate remaining time
  elapsed <- difftime(Sys.time(), total_start, units = "hours")
  rate <- length(completed_indices) / as.numeric(elapsed)
  remaining <- n_sims - length(completed_indices)
  eta_hours <- remaining / rate
  cat("  Estimated time remaining:", round(eta_hours, 1), "hours\n")
}

total_end <- Sys.time()
total_duration <- difftime(total_end, total_start, units = "hours")

# ------------------------------------------------------------------------------
# Compile final results
# ------------------------------------------------------------------------------
cat("\n=== Compiling Final Results ===\n")

# Extract successful simulations
successful_results <- all_results[sapply(all_results, function(x) !is.null(x) && x$success)]
failed_results <- all_results[sapply(all_results, function(x) !is.null(x) && !x$success)]

cat("Successful simulations:", length(successful_results), "\n")
cat("Failed simulations:", length(failed_results), "\n")

# Extract just the simulation objects
climate_only_sims <- lapply(successful_results, function(x) x$simulation)
names(climate_only_sims) <- paste0("sim_", sapply(successful_results, function(x) x$sim_idx))

# Get failed indices and errors
failed_info <- lapply(failed_results, function(x) {
  list(sim_idx = x$sim_idx, error = x$error)
})

# Compile final output
final_output <- list(
  simulations = climate_only_sims,
  n_successful = length(climate_only_sims),
  n_failed = length(failed_results),
  failed_info = failed_info,
  settings = list(
    spinup_years = spinup_years,
    effort = 0,
    source_ensemble = mc_file
  ),
  runtime_hours = as.numeric(total_duration),
  created = Sys.time()
)

# Save final results
final_file <- file.path(output_dir, "climate_only_ensemble_results.rds")
saveRDS(final_output, final_file)
cat("\nSaved final results to:", final_file, "\n")

# Also save just the simulations for easier loading
sims_file <- file.path(output_dir, "climate_only_simulations.rds")
saveRDS(climate_only_sims, sims_file)
cat("Saved simulations to:", sims_file, "\n")

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------
cat("\n=== Climate-Only Ensemble Complete ===\n")
cat("Total runtime:", round(total_duration, 2), "hours\n")
cat("Successful:", length(climate_only_sims), "/", n_sims, "\n")
cat("Failed:", length(failed_results), "\n")

if (length(failed_results) > 0) {
  cat("\nFailed simulation indices:\n")
  print(sapply(failed_results, function(x) x$sim_idx))
}

cat("\nOutput files:\n")
cat(" ", final_file, "\n")
cat(" ", sims_file, "\n")

cat("\nThese climate-only simulations can now be used for matched\n")
cat("comparisons with the fished ensemble, where each simulation\n")
cat("uses identical parameters but different fishing effort.\n")
