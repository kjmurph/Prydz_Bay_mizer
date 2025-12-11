# Test: Verify that 3-cycle spinup matches fished ensemble biomass in pre-fishing period
# Tests first 10 simulations to verify fix before running full ensemble

library(therMizer)
library(mizer)

cat("=== Testing Spinup Fix for Climate-Only Ensemble ===\n")
cat("Comparing first 10 simulations with 3-cycle vs 1-cycle spinup\n\n")

# Load required data
cat("Loading data...\n")
fished_mc <- readRDS("Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds")
extended_ocean_temp <- readRDS("temperature_forcing_1841_2010.rds")
extended_n_pp_array <- readRDS("phytoplankton_forcing_1841_2010.rds")

# Settings
spinup_years <- 118
n_test <- 10  # Test first 10 simulations

# Function with 3-cycle spinup (FIXED)
run_climate_only_3cycle <- function(fished_sim, ocean_temp, n_pp, spinup_years = 118) {
  params_original <- fished_sim@params
  
  params_climate <- upgradeTherParams(
    params_original,
    ocean_temp_array = ocean_temp,
    n_pp_array = n_pp,
    aerobic_effect = FALSE,
    metabolism_effect = TRUE
  )
  
  # 3 sequential spinup cycles (matching fished ensemble)
  sim_spinup <- NULL
  final_initial_n <- NULL
  for (cyc in 1:3) {
    if (!is.null(final_initial_n)) {
      sim_spinup <- project(
        params_climate,
        initial_n = final_initial_n,
        t_start = 1841,
        t_max = spinup_years,
        effort = 0
      )
    } else {
      sim_spinup <- project(
        params_climate,
        t_start = 1841,
        t_max = spinup_years,
        effort = 0
      )
    }
    final_initial_n <- sim_spinup@n[spinup_years, , ]
  }
  
  # Main simulation
  sim_climate_only <- project(
    params_climate,
    initial_n = final_initial_n,
    t_start = 1841,
    t_max = 170,
    effort = 0
  )
  
  return(sim_climate_only)
}

# Function with 1-cycle spinup (ORIGINAL - for comparison)
run_climate_only_1cycle <- function(fished_sim, ocean_temp, n_pp, spinup_years = 118) {
  params_original <- fished_sim@params
  
  params_climate <- upgradeTherParams(
    params_original,
    ocean_temp_array = ocean_temp,
    n_pp_array = n_pp,
    aerobic_effect = FALSE,
    metabolism_effect = TRUE
  )
  
  # Single spinup cycle (original approach)
  sim_spinup <- project(
    params_climate,
    t_start = 1841,
    t_max = spinup_years,
    effort = 0
  )
  
  # Main simulation
  sim_climate_only <- project(
    params_climate,
    initial_n = sim_spinup@n[spinup_years, , ],
    t_start = 1841,
    t_max = 170,
    effort = 0
  )
  
  return(sim_climate_only)
}

# Test each of the first 10 simulations
cat("\nRunning test simulations...\n")
cat(sprintf("%-5s | %-20s | %-20s | %-20s | %-15s | %-15s\n", 
            "Sim", "Fished (1900)", "Climate 1-cycle", "Climate 3-cycle", "Diff 1-cycle", "Diff 3-cycle"))
cat(strrep("-", 120), "\n")

results <- list()

for (i in 1:n_test) {
  cat(sprintf("Processing simulation %d/%d...\n", i, n_test))
  
  # Get fished simulation
  fished_sim <- fished_mc$simulations[[i]]
  
  # Get biomass at year 60 (1900) from fished simulation
  fished_biomass_1900 <- sum(fished_sim@n[60, , ])
  
  # Run climate-only with 1-cycle spinup
  climate_1cycle <- run_climate_only_1cycle(fished_sim, extended_ocean_temp, extended_n_pp_array, spinup_years)
  climate_1cycle_biomass_1900 <- sum(climate_1cycle@n[60, , ])
  
  # Run climate-only with 3-cycle spinup
  climate_3cycle <- run_climate_only_3cycle(fished_sim, extended_ocean_temp, extended_n_pp_array, spinup_years)
  climate_3cycle_biomass_1900 <- sum(climate_3cycle@n[60, , ])
  
  # Calculate differences
  diff_1cycle <- climate_1cycle_biomass_1900 - fished_biomass_1900
  diff_3cycle <- climate_3cycle_biomass_1900 - fished_biomass_1900
  
  # Calculate percent differences
  pct_diff_1cycle <- (diff_1cycle / fished_biomass_1900) * 100
  pct_diff_3cycle <- (diff_3cycle / fished_biomass_1900) * 100
  
  # Store results
  results[[i]] <- list(
    sim_id = i,
    fished = fished_biomass_1900,
    climate_1cycle = climate_1cycle_biomass_1900,
    climate_3cycle = climate_3cycle_biomass_1900,
    diff_1cycle = diff_1cycle,
    diff_3cycle = diff_3cycle,
    pct_diff_1cycle = pct_diff_1cycle,
    pct_diff_3cycle = pct_diff_3cycle
  )
  
  # Print summary
  cat(sprintf("%-5d | %16.4e | %16.4e | %16.4e | %+11.2f%% | %+11.2f%%\n",
              i, fished_biomass_1900, climate_1cycle_biomass_1900, climate_3cycle_biomass_1900,
              pct_diff_1cycle, pct_diff_3cycle))
}

cat(strrep("-", 120), "\n")

# Summary statistics
cat("\nSUMMARY STATISTICS (Year 1900 biomass):\n")
cat(strrep("=", 60), "\n")

all_diff_1cycle <- sapply(results, function(x) x$pct_diff_1cycle)
all_diff_3cycle <- sapply(results, function(x) x$pct_diff_3cycle)

cat(sprintf("\n1-Cycle Spinup (ORIGINAL):\n"))
cat(sprintf("  Mean difference:     %+.4f%%\n", mean(all_diff_1cycle)))
cat(sprintf("  Median difference:   %+.4f%%\n", median(all_diff_1cycle)))
cat(sprintf("  Range:               %+.4f%% to %+.4f%%\n", min(all_diff_1cycle), max(all_diff_1cycle)))
cat(sprintf("  Std deviation:       %.4f%%\n", sd(all_diff_1cycle)))

cat(sprintf("\n3-Cycle Spinup (FIXED):\n"))
cat(sprintf("  Mean difference:     %+.4f%%\n", mean(all_diff_3cycle)))
cat(sprintf("  Median difference:   %+.4f%%\n", median(all_diff_3cycle)))
cat(sprintf("  Range:               %+.4f%% to %+.4f%%\n", min(all_diff_3cycle), max(all_diff_3cycle)))
cat(sprintf("  Std deviation:       %.4f%%\n", sd(all_diff_3cycle)))

cat(sprintf("\nImprovement (reduction in mean absolute difference):\n"))
cat(sprintf("  1-cycle: %.4f%%\n", mean(abs(all_diff_1cycle))))
cat(sprintf("  3-cycle: %.4f%%\n", mean(abs(all_diff_3cycle))))
cat(sprintf("  Reduction: %.4f%%\n", mean(abs(all_diff_1cycle)) - mean(abs(all_diff_3cycle))))

# Test if 3-cycle is significantly closer to zero
if (mean(abs(all_diff_3cycle)) < mean(abs(all_diff_1cycle))) {
  cat("\n✓ SUCCESS: 3-cycle spinup produces biomass closer to fished ensemble\n")
  cat("  This confirms the fix addresses the discrepancy.\n")
} else {
  cat("\n✗ ISSUE: 3-cycle spinup does not improve alignment\n")
  cat("  Further investigation needed.\n")
}

# Save results for inspection
saveRDS(results, "test_spinup_fix_results.rds")
cat("\nDetailed results saved to: test_spinup_fix_results.rds\n")
