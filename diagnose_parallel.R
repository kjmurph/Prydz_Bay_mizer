# Diagnostic test for parallel processing issue
library(therMizer)
library(mizer)
library(parallel)

cat("=== Parallel Processing Diagnostic ===\n\n")

# Step 1: Load data
cat("Step 1: Loading data...\n")
tryCatch({
  mc_results <- readRDS("Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds")
  fished_sims <- mc_results$simulations
  cat("  Loaded", length(fished_sims), "simulations\n")
  
  extended_ocean_temp <- readRDS("temperature_forcing_1841_2010.rds")
  extended_n_pp_array <- readRDS("phytoplankton_forcing_1841_2010.rds")
  cat("  Loaded climate forcings\n")
}, error = function(e) {
  cat("  ERROR loading data:", e$message, "\n")
  stop("Data loading failed")
})

# Step 2: Check simulation structure
cat("\nStep 2: Checking simulation structure...\n")
tryCatch({
  sim1 <- fished_sims[[1]]
  cat("  Class of fished_sims[[1]]:", class(sim1), "\n")
  
  if (is.list(sim1) && !inherits(sim1, "MizerSim")) {
    cat("  Nested list detected, extracting [[1]]...\n")
    sim1 <- sim1[[1]]
  }
  cat("  Final class:", class(sim1), "\n")
  
  params <- sim1@params
  cat("  Params extracted, species:", nrow(params@species_params), "\n")
}, error = function(e) {
  cat("  ERROR:", e$message, "\n")
  stop("Structure check failed")
})

# Step 3: Test cluster creation
cat("\nStep 3: Testing cluster creation (2 cores)...\n")
tryCatch({
  cl <- makeCluster(2)
  cat("  Cluster created successfully\n")
  stopCluster(cl)
  cat("  Cluster stopped\n")
}, error = function(e) {
  cat("  ERROR creating cluster:", e$message, "\n")
  stop("Cluster creation failed")
})

# Step 4: Test data export to cluster
cat("\nStep 4: Testing data export to cluster...\n")
tryCatch({
  cl <- makeCluster(2)
  
  # This is where it might fail - exporting 1.8GB of data
  cat("  Exporting fished_sims (this may take a while)...\n")
  clusterExport(cl, "fished_sims", envir = environment())
  cat("  Exported fished_sims\n")
  
  clusterExport(cl, c("extended_ocean_temp", "extended_n_pp_array"), envir = environment())
  cat("  Exported climate forcings\n")
  
  stopCluster(cl)
  cat("  Export test passed\n")
}, error = function(e) {
  cat("  ERROR exporting data:", e$message, "\n")
  try(stopCluster(cl), silent = TRUE)
  stop("Data export failed")
})

# Step 5: Test package loading on workers
cat("\nStep 5: Testing package loading on workers...\n")
tryCatch({
  cl <- makeCluster(2)
  clusterExport(cl, c("fished_sims", "extended_ocean_temp", "extended_n_pp_array"), envir = environment())
  
  result <- clusterEvalQ(cl, {
    library(therMizer)
    library(mizer)
    "OK"
  })
  cat("  Packages loaded on workers:", result[[1]], "\n")
  
  stopCluster(cl)
}, error = function(e) {
  cat("  ERROR loading packages:", e$message, "\n")
  try(stopCluster(cl), silent = TRUE)
  stop("Package loading failed")
})

# Step 6: Test running one simulation on worker
cat("\nStep 6: Testing one simulation on worker...\n")
tryCatch({
  spinup_years <- 118
  
  cl <- makeCluster(2)
  clusterExport(cl, c("fished_sims", "extended_ocean_temp", "extended_n_pp_array", "spinup_years"), 
                envir = environment())
  clusterEvalQ(cl, { library(therMizer); library(mizer) })
  
  test_fn <- function(idx) {
    sim1 <- fished_sims[[idx]]
    if (is.list(sim1) && !inherits(sim1, "MizerSim")) {
      sim1 <- sim1[[1]]
    }
    
    params_climate <- upgradeTherParams(
      sim1@params,
      ocean_temp_array = extended_ocean_temp,
      n_pp_array = extended_n_pp_array,
      aerobic_effect = FALSE,
      metabolism_effect = TRUE
    )
    
    sim_spinup <- project(params_climate, t_start = 1841, t_max = spinup_years, effort = 0)
    sim_result <- project(params_climate, initial_n = sim_spinup@n[spinup_years,,], 
                          t_start = 1841, t_max = 169, effort = 0)
    
    max(as.numeric(dimnames(sim_result@n)$time))
  }
  
  result <- parLapply(cl, 1, test_fn)
  cat("  Simulation completed, end time:", result[[1]], "\n")
  
  stopCluster(cl)
  cat("  Test passed!\n")
}, error = function(e) {
  cat("  ERROR running simulation:", e$message, "\n")
  try(stopCluster(cl), silent = TRUE)
  stop("Simulation test failed")
})

cat("\n=== All diagnostics passed! ===\n")
