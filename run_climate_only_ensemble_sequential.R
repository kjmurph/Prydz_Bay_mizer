# Run Climate-Only (Unfished) Ensemble Simulations - Sequential Version
# Runs all 2,111 accepted MC parameterisations with climate forcing but no fishing
# This creates matched controls for each simulation in the fished ensemble
#
# This is a sequential version that processes one simulation at a time,
# saving progress frequently. More robust than parallel version for long runs.

library(therMizer)
library(mizer)

cat("=== Climate-Only Ensemble Simulations (Sequential) ===\n")
cat("Running all MC parameterisations with climate forcing, no fishing\n\n")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
spinup_years <- 118  # Same as original fished ensemble
checkpoint_interval <- 1  # Save progress after EVERY simulation for maximum safety

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

# Verify the simulation time range expected
cat("\nExpected simulation: 1841-2010 (170 years) with", spinup_years, "year spinup\n")

# ------------------------------------------------------------------------------
# Check for existing progress
# ------------------------------------------------------------------------------
progress_file <- file.path(output_dir, "progress_sequential.rds")
sims_dir <- file.path(output_dir, "individual_sims")
if (!dir.exists(sims_dir)) dir.create(sims_dir, recursive = TRUE)

if (file.exists(progress_file)) {
  progress <- readRDS(progress_file)
  completed_indices <- progress$completed
  failed_indices <- progress$failed
  cat("\nFound existing progress:\n")
  cat("  Completed:", length(completed_indices), "\n")
  cat("  Failed:", length(failed_indices), "\n")
} else {
  completed_indices <- integer(0)
  failed_indices <- list()
  cat("\nStarting fresh - no previous progress found\n")
}

# Determine which simulations still need to be run
all_processed <- c(completed_indices, sapply(failed_indices, function(x) x$idx))
remaining_indices <- setdiff(1:n_sims, all_processed)
cat("Remaining simulations:", length(remaining_indices), "\n")

if (length(remaining_indices) == 0) {
  cat("\nAll simulations already processed!\n")
  cat("Run the compilation script to combine results.\n")
  q(save = "no")
}

# ------------------------------------------------------------------------------
# Main processing loop
# ------------------------------------------------------------------------------
cat("\n=== Starting Climate-Only Simulations ===\n")
cat("Processing", length(remaining_indices), "remaining simulations\n")
cat("Checkpoint interval:", checkpoint_interval, "simulations\n")
cat("Individual sims saved to:", sims_dir, "\n\n")

total_start <- Sys.time()
sims_since_checkpoint <- 0

for (i in seq_along(remaining_indices)) {
  idx <- remaining_indices[i]
  
  # Progress display
  pct_complete <- round((length(completed_indices) + i) / n_sims * 100, 1)
  cat("\n[", i, "/", length(remaining_indices), "] Processing simulation", idx, 
      "(", pct_complete, "% overall)\n")
  
  sim_start <- Sys.time()
  
  tryCatch({
    # Extract params from the fished simulation (handle both direct and nested structures)
    fished_sim <- fished_sims[[idx]]
    if (is.list(fished_sim) && !inherits(fished_sim, "MizerSim")) {
      fished_sim <- fished_sim[[1]]
    }
    params_original <- fished_sim@params
    
    cat("  Upgrading to therMizer params...")
    params_climate <- upgradeTherParams(
      params_original,
      ocean_temp_array = extended_ocean_temp,
      n_pp_array = extended_n_pp_array,
      aerobic_effect = FALSE,
      metabolism_effect = TRUE
    )
    cat(" done\n")
    
    # Run spinup
    cat("  Running spinup (", spinup_years, " years)...", sep = "")
    sim_spinup <- project(
      params_climate,
      t_start = 1841,
      t_max = spinup_years,
      effort = 0
    )
    cat(" done\n")
    
    # Run main simulation (169 years to match fished ensemble ending at 2010)
    cat("  Running main simulation (169 years)...")
    sim_climate_only <- project(
      params_climate,
      initial_n = sim_spinup@n[spinup_years, , ],
      t_start = 1841,
      t_max = 169,
      effort = 0
    )
    cat(" done\n")
    
    # Verify output ends at 2010 (matching fished ensemble)
    sim_times <- as.numeric(dimnames(sim_climate_only@n)$time)
    if (max(sim_times) != 2010) {
      stop("Unexpected end time: ", max(sim_times))
    }
    
    # Save individual simulation
    sim_file <- file.path(sims_dir, paste0("sim_", sprintf("%04d", idx), "_climate_only.rds"))
    saveRDS(sim_climate_only, sim_file)
    
    # Record success
    completed_indices <- c(completed_indices, idx)
    sims_since_checkpoint <- sims_since_checkpoint + 1
    
    sim_end <- Sys.time()
    sim_duration <- round(difftime(sim_end, sim_start, units = "mins"), 2)
    cat("  Completed in", sim_duration, "minutes\n")
    cat("  Saved:", basename(sim_file), "\n")
    
    # Memory cleanup after each simulation
    rm(fished_sim, params_original, params_climate, sim_spinup, sim_climate_only)
    gc()
    
  }, error = function(e) {
    cat("  ERROR:", as.character(e), "\n")
    failed_indices <<- c(failed_indices, list(list(idx = idx, error = as.character(e))))
    sims_since_checkpoint <<- sims_since_checkpoint + 1
  })
  
  # Save checkpoint
  if (sims_since_checkpoint >= checkpoint_interval) {
    cat("\n  Saving checkpoint...")
    progress <- list(
      completed = completed_indices,
      failed = failed_indices,
      last_update = Sys.time()
    )
    saveRDS(progress, progress_file)
    cat(" done\n")
    
    # Estimate remaining time
    elapsed <- difftime(Sys.time(), total_start, units = "hours")
    completed_this_session <- i
    rate <- completed_this_session / as.numeric(elapsed)
    remaining <- length(remaining_indices) - i
    eta_hours <- remaining / rate
    
    cat("  Progress: ", length(completed_indices), "/", n_sims, " completed\n", sep = "")
    cat("  Session rate:", round(rate, 1), "sims/hour\n")
    cat("  Estimated remaining:", round(eta_hours, 2), "hours\n")
    
    sims_since_checkpoint <- 0
  }
}

# Final checkpoint
cat("\n\nSaving final progress...\n")
progress <- list(
  completed = completed_indices,
  failed = failed_indices,
  last_update = Sys.time()
)
saveRDS(progress, progress_file)

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------
total_end <- Sys.time()
total_duration <- difftime(total_end, total_start, units = "hours")

cat("\n=== Session Complete ===\n")
cat("Session duration:", round(total_duration, 2), "hours\n")
cat("Completed this session:", length(remaining_indices), "\n")
cat("Total completed:", length(completed_indices), "/", n_sims, "\n")
cat("Total failed:", length(failed_indices), "\n")

if (length(failed_indices) > 0) {
  cat("\nFailed simulation indices:\n")
  print(sapply(failed_indices, function(x) x$idx))
}

cat("\nIndividual simulations saved to:\n")
cat(" ", sims_dir, "\n")
cat("\nProgress file:\n")
cat(" ", progress_file, "\n")

if (length(completed_indices) == n_sims) {
  cat("\n*** ALL SIMULATIONS COMPLETE ***\n")
  cat("Run compile_climate_only_ensemble.R to combine results.\n")
} else {
  cat("\nTo continue processing, run this script again.\n")
  cat("It will automatically resume from where it left off.\n")
}
