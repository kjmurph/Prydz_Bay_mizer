# Test: Verify that using the SAME initial_n (no additional spinup) gives matching biomass
# The fished ensemble params already contain post-spinup initial_n
# Climate-only should use those EXACT initial conditions, not re-spin-up

library(therMizer)
library(mizer)

cat("=== Testing NO Additional Spinup for Climate-Only Ensemble ===\n")
cat("Hypothesis: Fished params already have post-spinup initial_n\n")
cat("Climate-only should use those exact initial conditions\n\n")

# Load required data
cat("Loading data...\n")
fished_mc <- readRDS("Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds")
extended_ocean_temp <- readRDS("temperature_forcing_1841_2010.rds")
extended_n_pp_array <- readRDS("phytoplankton_forcing_1841_2010.rds")

n_test <- 10  # Test first 10 simulations

# Function with NO additional spinup (CORRECT approach)
run_climate_only_no_spinup <- function(fished_sim, ocean_temp, n_pp) {
  params_original <- fished_sim@params
  
  # The params already have climate forcing and post-spinup initial_n
  # Just run the simulation with effort=0
  sim_climate_only <- project(
    params_original,
    t_start = 1841,
    t_max = 170,
    effort = 0
  )
  
  return(sim_climate_only)
}

# Test each of the first 10 simulations
cat("\nRunning test simulations...\n")
cat(sprintf("%-5s | %-20s | %-20s | %-15s\n", 
            "Sim", "Fished (1900)", "Climate No-Spinup", "Difference"))
cat(strrep("-", 75), "\n")

results <- list()

for (i in 1:n_test) {
  cat(sprintf("Processing simulation %d/%d...\n", i, n_test))
  
  # Get fished simulation
  fished_sim <- fished_mc$simulations[[i]]
  
  # Get biomass at year 60 (1900) from fished simulation
  fished_biomass_1900 <- sum(fished_sim@n[60, , ])
  
  # Run climate-only with NO additional spinup
  climate_no_spinup <- run_climate_only_no_spinup(fished_sim, extended_ocean_temp, extended_n_pp_array)
  climate_no_spinup_biomass_1900 <- sum(climate_no_spinup@n[60, , ])
  
  # Calculate difference
  diff <- climate_no_spinup_biomass_1900 - fished_biomass_1900
  pct_diff <- (diff / fished_biomass_1900) * 100
  
  # Store results
  results[[i]] <- list(
    sim_id = i,
    fished = fished_biomass_1900,
    climate = climate_no_spinup_biomass_1900,
    diff = diff,
    pct_diff = pct_diff
  )
  
  # Print summary
  cat(sprintf("%-5d | %16.4e | %16.4e | %+11.4f%%\n",
              i, fished_biomass_1900, climate_no_spinup_biomass_1900, pct_diff))
}

cat(strrep("-", 75), "\n")

# Summary statistics
cat("\nSUMMARY STATISTICS (Year 1900 biomass):\n")
cat(strrep("=", 60), "\n")

all_diff <- sapply(results, function(x) x$pct_diff)

cat(sprintf("\nNo Additional Spinup:\n"))
cat(sprintf("  Mean difference:     %+.6f%%\n", mean(all_diff)))
cat(sprintf("  Median difference:   %+.6f%%\n", median(all_diff)))
cat(sprintf("  Range:               %+.6f%% to %+.6f%%\n", min(all_diff), max(all_diff)))
cat(sprintf("  Std deviation:       %.6f%%\n", sd(all_diff)))
cat(sprintf("  Max absolute diff:   %.6f%%\n", max(abs(all_diff))))

# Test if differences are negligible (< 0.01%)
if (max(abs(all_diff)) < 0.01) {
  cat("\n✓ SUCCESS: No-spinup approach produces identical biomass (< 0.01% difference)\n")
  cat("  This confirms fished params already contain post-spinup initial_n\n")
  cat("  Climate-only should use these exact initial conditions\n")
} else if (max(abs(all_diff)) < 0.1) {
  cat("\n✓ GOOD: Very small differences (< 0.1%)\n")
  cat("  Likely due to numerical precision or minor stochasticity\n")
} else {
  cat("\n✗ ISSUE: Significant differences remain\n")
  cat("  Further investigation needed\n")
}

# Save results
saveRDS(results, "test_no_spinup_results.rds")
cat("\nDetailed results saved to: test_no_spinup_results.rds\n")
