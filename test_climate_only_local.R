# Quick Test - Climate-Only Ensemble Local Parallel
# Tests 3 simulations to verify everything works

library(therMizer)
library(mizer)
library(parallel)

cat("=== Quick Test: Climate-Only Ensemble ===\n\n")

n_cores <- min(3, parallel::detectCores() - 2)
spinup_years <- 118
test_indices <- c(1, 100, 500)

cat("Configuration:\n")
cat("  Test simulations:", paste(test_indices, collapse = ", "), "\n")
cat("  Cores:", n_cores, "\n\n")

# Load data
cat("Loading data...\n")
mc_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
mc_results <- readRDS(mc_file)
fished_sims <- mc_results$simulations
cat("  Loaded", length(fished_sims), "simulations\n")

extended_ocean_temp <- readRDS("temperature_forcing_1841_2010.rds")
extended_n_pp_array <- readRDS("phytoplankton_forcing_1841_2010.rds")
cat("  Loaded climate forcings\n\n")

# Worker function
run_single_sim <- function(idx) {
  tryCatch({
    start_time <- Sys.time()
    
    # Extract MizerSim from nested list
    fished_sim <- fished_sims[[idx]]
    if (is.list(fished_sim) && !inherits(fished_sim, "MizerSim")) {
      fished_sim <- fished_sim[[1]]
    }
    
    params_original <- fished_sim@params
    
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
      t_max = 169,
      effort = 0
    )
    
    end_time <- max(as.numeric(dimnames(sim_climate_only@n)$time))
    elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
    
    list(success = TRUE, idx = idx, end_time = end_time, time_secs = elapsed,
         n_timesteps = length(dimnames(sim_climate_only@n)$time))
    
  }, error = function(e) {
    list(success = FALSE, idx = idx, error = as.character(e))
  })
}

# Run test
cat("Running parallel test...\n")
total_start <- Sys.time()

cl <- makeCluster(n_cores)
clusterExport(cl, c("fished_sims", "extended_ocean_temp", "extended_n_pp_array", 
                    "spinup_years"), envir = environment())
invisible(clusterEvalQ(cl, {
  suppressPackageStartupMessages({
    library(therMizer)
    library(mizer)
  })
}))

results <- parLapply(cl, test_indices, run_single_sim)
stopCluster(cl)

total_time <- round(difftime(Sys.time(), total_start, units = "secs"), 1)

# Results
cat("\n=== Test Results ===\n\n")

successes <- 0
for (res in results) {
  if (res$success) {
    successes <- successes + 1
    cat("Sim", res$idx, ": SUCCESS\n")
    cat("  End time:", res$end_time, "\n")
    cat("  Timesteps:", res$n_timesteps, "\n")
    cat("  Time:", res$time_secs, "sec\n\n")
  } else {
    cat("Sim", res$idx, ": FAILED\n")
    cat("  Error:", res$error, "\n\n")
  }
}

cat("=== Summary ===\n")
cat("Passed:", successes, "/", length(test_indices), "\n")
cat("Total time:", total_time, "seconds\n")

if (successes == length(test_indices)) {
  cat("\n*** ALL TESTS PASSED ***\n")
  cat("Ready to run: Rscript run_climate_only_ensemble_local.R\n")
} else {
  cat("\n*** SOME TESTS FAILED ***\n")
  cat("Fix errors before running full ensemble.\n")
}
