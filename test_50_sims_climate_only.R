# Final test: Verify corrected climate-only generation on 50 simulations
# This uses the fixed approach (no additional spinup) and validates biomass matching

library(therMizer)
library(mizer)
library(parallel)
library(pbapply)

cat("=== Final Validation Test: 50 Simulations ===\n")
cat("Using corrected approach: no additional spinup\n\n")

# Load required data
cat("Loading data...\n")
fished_mc <- readRDS("Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds")
extended_ocean_temp <- readRDS("temperature_forcing_1841_2010.rds")
extended_n_pp_array <- readRDS("phytoplankton_forcing_1841_2010.rds")

n_test <- 50

# Define the corrected function (no additional spinup)
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

cat("\nRunning", n_test, "test simulations in parallel...\n")

# Set up parallel cluster
n_cores <- min(10, parallel::detectCores() - 2)
cat("Using", n_cores, "cores\n\n")

# Extract only the first n_test simulations to avoid serialization issues
test_sims <- fished_mc$simulations[1:n_test]

cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(therMizer)
  library(mizer)
})

# Run simulations
start_time <- Sys.time()
results <- pblapply(test_sims, run_climate_only_sim, cl = cl)
end_time <- Sys.time()

stopCluster(cl)

elapsed_time <- difftime(end_time, start_time, units = "mins")
cat(sprintf("\n=== TIMING ===\n"))
cat(sprintf("Time for %d simulations: %.2f minutes\n", n_test, as.numeric(elapsed_time)))
cat(sprintf("Average per simulation: %.2f seconds\n", as.numeric(elapsed_time) * 60 / n_test))
cat(sprintf("Estimated time for 2111 sims: %.1f minutes (%.1f hours)\n", 
            as.numeric(elapsed_time) * 2111 / n_test,
            as.numeric(elapsed_time) * 2111 / n_test / 60))
cat("\n")

# Analyze results
cat("\nAnalyzing biomass matching at year 1900 (pre-fishing)...\n")

comparison <- data.frame(
  sim_id = integer(),
  fished_biomass = numeric(),
  climate_biomass = numeric(),
  difference_pct = numeric()
)

for (i in 1:n_test) {
  if (results[[i]]$success) {
    fished_b <- sum(test_sims[[i]]@n[60, , ])  # Year 1900
    climate_b <- sum(results[[i]]$simulation@n[60, , ])
    diff_pct <- ((climate_b - fished_b) / fished_b) * 100
    
    comparison <- rbind(comparison, data.frame(
      sim_id = i,
      fished_biomass = fished_b,
      climate_biomass = climate_b,
      difference_pct = diff_pct
    ))
  }
}

# Summary
cat("\n=== RESULTS SUMMARY ===\n")
cat(sprintf("Successful simulations: %d / %d\n", sum(sapply(results, function(x) x$success)), n_test))
cat(sprintf("\nBiomass comparison at year 1900 (pre-fishing):\n"))
cat(sprintf("  Mean difference:     %+.8f%%\n", mean(comparison$difference_pct)))
cat(sprintf("  Median difference:   %+.8f%%\n", median(comparison$difference_pct)))
cat(sprintf("  Range:               %+.8f%% to %+.8f%%\n", 
            min(comparison$difference_pct), max(comparison$difference_pct)))
cat(sprintf("  Std deviation:       %.8f%%\n", sd(comparison$difference_pct)))
cat(sprintf("  Max absolute diff:   %.8f%%\n", max(abs(comparison$difference_pct))))

# Pass/fail
if (max(abs(comparison$difference_pct)) < 1e-10) {
  cat("\n✓✓✓ PERFECT MATCH: Differences < 1e-10% ✓✓✓\n")
  cat("Climate-only ensemble will have identical pre-fishing biomass to fished ensemble\n")
  cat("\n==> READY TO RUN FULL 2111 ENSEMBLE <==\n")
} else if (max(abs(comparison$difference_pct)) < 0.001) {
  cat("\n✓✓ EXCELLENT: Differences < 0.001% ✓✓\n")
  cat("Likely numerical precision differences only\n")
  cat("\n==> READY TO RUN FULL 2111 ENSEMBLE <==\n")
} else {
  cat("\n✗ ISSUE: Differences still significant\n")
  cat("DO NOT proceed with full ensemble until issue resolved\n")
}

# Show first few examples
cat("\nFirst 10 simulations detail:\n")
print(head(comparison, 10), row.names = FALSE, digits = 10)

# Save results
saveRDS(results, "test_50_sims_climate_only.rds")
saveRDS(comparison, "test_50_sims_comparison.rds")
cat("\nResults saved to test_50_sims_climate_only.rds and test_50_sims_comparison.rds\n")
