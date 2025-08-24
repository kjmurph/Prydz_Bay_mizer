# Test Script for Parallel Optimization Implementation
# =====================================================

# This script tests the parallel optimization setup before running the full optimization

library(mizer)
library(therMizer)
library(calibrar)
library(tidyverse)
library(parallel)
library(doParallel)
library(foreach)

# Source helper functions
source("Helper_Functions.R")

cat("=== TESTING PARALLEL OPTIMIZATION SETUP ===\n")

# Test 1: Check parallel backend availability
cat("\n1. Testing parallel backend...\n")
n_cores <- detectCores() - 1
n_cores <- min(n_cores, 4)  # Limit for testing

if (n_cores > 1) {
  # Setup cluster
  cl <- makeCluster(n_cores, type = "PSOCK")
  registerDoParallel(cl)
  
  cat("Cores available:", n_cores, "\n")
  cat("Workers registered:", getDoParWorkers(), "\n")
  
  # Test parallel computation
  test_result <- foreach(i = 1:10, .combine = c) %dopar% {
    sqrt(i)
  }
  
  cat("Parallel test result:", paste(round(test_result, 2), collapse = ", "), "\n")
  
  # Stop cluster
  stopCluster(cl)
  registerDoSEQ()
  cat("Cluster stopped successfully\n")
  
} else {
  cat("Only 1 core available - parallel processing not feasible\n")
}

# Test 2: Check memory availability
cat("\n2. Testing memory detection...\n")
system_ram_gb <- tryCatch({
  if (.Platform$OS.type == "windows") {
    as.numeric(system('wmic computersystem get TotalPhysicalMemory /value', intern = TRUE)[3]) / 1e9
  } else {
    as.numeric(system("free -b | grep '^Mem' | awk '{print $2}'", intern = TRUE)) / 1e9
  }
}, error = function(e) {
  cat("Memory detection failed, assuming 16GB\n")
  16
})

cat("System RAM detected:", round(system_ram_gb, 1), "GB\n")
memory_limited_cores <- floor(system_ram_gb / 3)
cat("Memory-limited cores:", memory_limited_cores, "\n")

# Test 3: Check calibrar with parallel
cat("\n3. Testing calibrar parallel functionality...\n")

# Simple test function
test_objective <- function(x) {
  Sys.sleep(0.1)  # Simulate computation time
  sum(x^2)
}

if (n_cores > 1) {
  # Setup cluster again for calibrar test
  cl <- makeCluster(min(2, n_cores), type = "PSOCK")
  registerDoParallel(cl)
  
  # Test calibrar with parallel
  start_time <- Sys.time()
  
  tryCatch({
    test_calibrar <- calibrate(
      par = c(1, 1),
      fn = test_objective,
      method = "optim",
      control = list(method = "L-BFGS-B", maxit = 10),
      parallel = TRUE
    )
    
    end_time <- Sys.time()
    runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    cat("Calibrar parallel test successful\n")
    cat("Test runtime:", round(runtime, 2), "seconds\n")
    cat("Optimal value:", round(test_calibrar$value, 4), "\n")
    
  }, error = function(e) {
    cat("Calibrar parallel test failed:", e$message, "\n")
    cat("Trying sequential calibrar...\n")
    
    test_calibrar_seq <- calibrate(
      par = c(1, 1),
      fn = test_objective,
      method = "SANN",
      control = list(maxit = 10)
    )
    
    cat("Sequential calibrar works\n")
  })
  
  # Cleanup
  stopCluster(cl)
  registerDoSEQ()
  cat("Test cluster stopped\n")
}

# Test 4: Check required files exist
cat("\n4. Checking required data files...\n")
required_files <- c(
  "params_steady_state_2011_2020_tol_0.00025.RDS",
  "constant_array_temp_1841.RDS",
  "constant_array_n_pp_1841.RDS",
  "effort_array_1841_2010.rds",
  "yield_observed_timeseries_tidy.RDS"
)

for (file in required_files) {
  if (file.exists(file)) {
    cat("✓", file, "\n")
  } else {
    cat("✗", file, "- MISSING\n")
  }
}

# Test 5: Check helper functions
cat("\n5. Checking helper functions...\n")
if (exists("plot_yield_comparison")) {
  cat("✓ plot_yield_comparison available\n")
} else {
  cat("✗ plot_yield_comparison missing\n")
}

if (exists("run_adaptive_uncertainty_analysis")) {
  cat("✓ run_adaptive_uncertainty_analysis available\n")
} else {
  cat("✗ run_adaptive_uncertainty_analysis missing\n")
}

cat("\n=== PARALLEL TEST COMPLETE ===\n")
cat("If all tests passed, you can run the full optimization with parallel processing.\n")
cat("If any tests failed, use sequential mode: Sys.setenv(R_PARALLELIZE = 'FALSE')\n")

# Recommendation
final_n_cores <- min(n_cores, memory_limited_cores, 4)
use_parallel_recommended <- final_n_cores > 1 && system_ram_gb > 8

cat("\nRECOMMENDATION:\n")
if (use_parallel_recommended) {
  cat("✓ System suitable for parallel processing\n")
  cat("Recommended cores:", final_n_cores, "\n")
  cat("Set: Sys.setenv(R_PARALLELIZE = 'TRUE')\n")
} else {
  cat("⚠ System may not be suitable for parallel processing\n")
  cat("Recommended: Use sequential mode\n")
  cat("Set: Sys.setenv(R_PARALLELIZE = 'FALSE')\n")
}