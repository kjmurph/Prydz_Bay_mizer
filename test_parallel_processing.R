# Test Parallel Processing for Climate-Only Ensemble
# Runs 5 simulations across 4 cores to verify parallel setup works

library(therMizer)
library(mizer)
library(parallel)
library(pbapply)

cat("=== Testing Parallel Processing ===\n\n")

# Test with small subset
n_test_sims <- 5
n_cores <- 4  # Use 4 cores for test
spinup_years <- 118

cat("Test Configuration:\n")
cat("  Simulations:", n_test_sims, "\n")
cat("  Cores:", n_cores, "\n")
cat("  Spinup years:", spinup_years, "\n\n")

# ------------------------------------------------------------------------------
# Load required data
# ------------------------------------------------------------------------------
cat("Loading data...\n")

mc_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
mc_results <- readRDS(mc_file)
fished_sims <- mc_results$simulations
cat("  Loaded MC ensemble:", length(fished_sims), "simulations\n")

extended_ocean_temp <- readRDS("temperature_forcing_1841_2010.rds")
extended_n_pp_array <- readRDS("phytoplankton_forcing_1841_2010.rds")
cat("  Loaded climate forcings\n\n")

# ------------------------------------------------------------------------------
# Worker function
# ------------------------------------------------------------------------------
run_single_sim <- function(idx) {
  tryCatch({
    start_time <- Sys.time()
    
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
    
    elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
    
    list(success = TRUE, idx = idx, sim = sim_climate_only, time_secs = elapsed)
    
  }, error = function(e) {
    list(success = FALSE, idx = idx, error = as.character(e), time_secs = NA)
  })
}

# ------------------------------------------------------------------------------
# Test parallel processing
# ------------------------------------------------------------------------------
test_indices <- 1:n_test_sims

cat("Starting parallel test with", n_test_sims, "simulations on", n_cores, "cores...\n\n")

total_start <- Sys.time()

# Set up cluster
cat("Step 1: Creating cluster...")
cl <- makeCluster(n_cores)
cat(" done\n")

# Export objects
cat("Step 2: Exporting data to workers...")
clusterExport(cl, c("fished_sims", "extended_ocean_temp", "extended_n_pp_array", 
                    "spinup_years"), envir = environment())
cat(" done\n")

# Load packages on workers
cat("Step 3: Loading packages on workers...")
clusterEvalQ(cl, {
  suppressPackageStartupMessages({
    suppressWarnings({
      library(therMizer)
      library(mizer)
    })
  })
})
cat(" done\n")

# Run parallel test
cat("Step 4: Running", n_test_sims, "simulations in parallel...\n")
run_start <- Sys.time()

results <- pblapply(test_indices, run_single_sim, cl = cl)

run_end <- Sys.time()

# Stop cluster
cat("Step 5: Stopping cluster...")
stopCluster(cl)
cat(" done\n\n")

total_end <- Sys.time()

# ------------------------------------------------------------------------------
# Analyse results
# ------------------------------------------------------------------------------
cat("=== Results ===\n\n")

successes <- sum(sapply(results, `[[`, "success"))
failures <- n_test_sims - successes

cat("Successes:", successes, "/", n_test_sims, "\n")
cat("Failures:", failures, "\n\n")

# Show individual times
cat("Individual simulation times:\n")
for (res in results) {
  status <- if(res$success) "✓" else "✗"
  time_str <- if(is.na(res$time_secs)) "N/A" else paste0(res$time_secs, " sec")
  cat("  Sim", res$idx, ":", status, time_str, "\n")
}

# Timing summary
run_time <- round(difftime(run_end, run_start, units = "secs"), 1)
total_time <- round(difftime(total_end, total_start, units = "secs"), 1)

cat("\n=== Timing Summary ===\n")
cat("Parallel run time:", run_time, "seconds (", n_test_sims, "sims on", n_cores, "cores)\n")
cat("Total time (incl setup):", total_time, "seconds\n")

# Calculate expected time for full run
secs_per_sim_parallel <- run_time / n_test_sims * n_cores  # Sequential equivalent
time_full_sequential <- secs_per_sim_parallel * 2111 / 60  # minutes
time_full_parallel_30 <- secs_per_sim_parallel * 2111 / 30 / 60  # 30 cores

cat("\n=== Extrapolations for Full Run (2111 sims) ===\n")
cat("Estimated sequential time:", round(time_full_sequential / 60, 1), "hours\n")
cat("Estimated parallel time (30 cores):", round(time_full_parallel_30, 1), "minutes\n")

# Quick data validation
cat("\n=== Data Validation ===\n")
for (res in results) {
  if (res$success) {
    sim <- res$sim
    n_vals <- sum(!is.na(sim@n))
    na_vals <- sum(is.na(sim@n))
    cat("  Sim", res$idx, ": n values =", n_vals, ", NAs =", na_vals, "\n")
  }
}

cat("\n=== Parallel Test Complete ===\n")
if (successes == n_test_sims && failures == 0) {
  cat("✓ All tests PASSED - ready for VM transfer\n")
} else {
  cat("✗ Some tests FAILED - investigate before VM transfer\n")
}
