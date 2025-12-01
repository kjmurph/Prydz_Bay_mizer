# Test Climate-Only Simulation - Quick Verification
# Runs just 2 simulations to verify the workflow works correctly

library(therMizer)
library(mizer)

cat("=== Testing Climate-Only Simulation Workflow ===\n")
cat("Running 2 test simulations to verify everything works\n\n")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
spinup_years <- 118
test_indices <- c(1, 100)  # Test first and one from middle

# ------------------------------------------------------------------------------
# Load required data
# ------------------------------------------------------------------------------
cat("Loading required data...\n")

mc_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
if (!file.exists(mc_file)) {
  stop("Cleaned ensemble file not found: ", mc_file)
}
mc_results <- readRDS(mc_file)
cat("  Loaded:", mc_file, "\n")

fished_sims <- mc_results$simulations
cat("  Total simulations available:", length(fished_sims), "\n")

# Load climate forcings
if (!file.exists("temperature_forcing_1841_2010.rds")) {
  stop("Temperature forcing file not found")
}
if (!file.exists("phytoplankton_forcing_1841_2010.rds")) {
  stop("Phytoplankton forcing file not found")
}

extended_ocean_temp <- readRDS("temperature_forcing_1841_2010.rds")
extended_n_pp_array <- readRDS("phytoplankton_forcing_1841_2010.rds")
cat("  Loaded temperature forcing:", dim(extended_ocean_temp), "\n")
cat("  Loaded phytoplankton forcing:", dim(extended_n_pp_array), "\n")

# ------------------------------------------------------------------------------
# Test simulations
# ------------------------------------------------------------------------------
cat("\n=== Running Test Simulations ===\n")

for (idx in test_indices) {
  cat("\n--- Test simulation", idx, "---\n")
  
  test_start <- Sys.time()
  
  tryCatch({
    # Step 1: Extract params from fished simulation
    cat("  Step 1: Extracting params from fished simulation...")
    fished_sim <- fished_sims[[idx]]
    params_original <- fished_sim@params
    cat(" OK\n")
    cat("    Species:", nrow(params_original@species_params), "\n")
    
    # Step 2: Upgrade to therMizer params
    cat("  Step 2: Upgrading to therMizer params with climate forcing...")
    params_climate <- upgradeTherParams(
      params_original,
      ocean_temp_array = extended_ocean_temp,
      n_pp_array = extended_n_pp_array,
      aerobic_effect = FALSE,
      metabolism_effect = TRUE
    )
    cat(" OK\n")
    
    # Step 3: Run spinup
    cat("  Step 3: Running spinup (", spinup_years, " years, unfished)...", sep = "")
    spinup_start <- Sys.time()
    sim_spinup <- project(
      params_climate,
      t_start = 1841,
      t_max = spinup_years,
      effort = 0
    )
    spinup_time <- round(difftime(Sys.time(), spinup_start, units = "secs"), 1)
    cat(" OK (", spinup_time, " sec)\n", sep = "")
    
    # Verify spinup
    spinup_times <- as.numeric(dimnames(sim_spinup@n)$time)
    cat("    Spinup time range:", min(spinup_times), "-", max(spinup_times), "\n")
    
    # Step 4: Run main simulation
    cat("  Step 4: Running main simulation (170 years, climate only)...")
    main_start <- Sys.time()
    sim_climate_only <- project(
      params_climate,
      initial_n = sim_spinup@n[spinup_years, , ],
      t_start = 1841,
      t_max = 170,
      effort = 0
    )
    main_time <- round(difftime(Sys.time(), main_start, units = "secs"), 1)
    cat(" OK (", main_time, " sec)\n", sep = "")
    
    # Verify output
    sim_times <- as.numeric(dimnames(sim_climate_only@n)$time)
    cat("    Output time range:", min(sim_times), "-", max(sim_times), "\n")
    cat("    Output time steps:", length(sim_times), "\n")
    
    # Check for NAs
    na_count <- sum(is.na(sim_climate_only@n))
    cat("    NA values in output:", na_count, "\n")
    
    # Check biomass
    biomass_start <- sum(sim_climate_only@n[1,,] * sim_climate_only@params@w * sim_climate_only@params@dw)
    biomass_end <- sum(sim_climate_only@n[length(sim_times),,] * sim_climate_only@params@w * sim_climate_only@params@dw)
    cat("    Biomass 1841:", format(biomass_start, scientific = TRUE, digits = 3), "g\n")
    cat("    Biomass 2010:", format(biomass_end, scientific = TRUE, digits = 3), "g\n")
    
    test_end <- Sys.time()
    total_time <- round(difftime(test_end, test_start, units = "mins"), 2)
    cat("\n  ✓ Test simulation", idx, "PASSED in", total_time, "minutes\n")
    
  }, error = function(e) {
    cat("\n  ✗ Test simulation", idx, "FAILED\n")
    cat("    Error:", as.character(e), "\n")
  })
}

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------
cat("\n=== Test Complete ===\n")
cat("\nIf both tests passed, you can proceed with the full parallel run.\n")
cat("Expected full run time with 32 cores: ~3.5-4 hours\n")
