# Setup Script for Parallel Optimization
# ======================================

# This script prepares your environment for parallel optimization
# Run this BEFORE executing 12_Calibrar_Optimization_Refined.Rmd

cat("=== PARALLEL OPTIMIZATION SETUP ===\n\n")

# 1. Enable parallel processing
Sys.setenv(R_PARALLELIZE = "TRUE")
cat("✓ Parallel processing environment variable set\n")

# 2. Test basic parallel functionality
library(parallel)
library(doParallel)
library(foreach)

n_cores_available <- detectCores()
n_cores_to_use <- min(n_cores_available - 1, 6)  # Conservative

cat("Cores available:", n_cores_available, "\n")
cat("Cores to use:", n_cores_to_use, "\n")

if (n_cores_to_use > 1) {
  # Test cluster creation
  tryCatch({
    test_cl <- makeCluster(2, type = "PSOCK")  # Small test cluster
    registerDoParallel(test_cl)
    
    cat("✓ Test cluster created successfully\n")
    cat("✓ Workers available:", getDoParWorkers(), "\n")
    
    # Simple parallel test
    test_result <- foreach(i = 1:4, .combine = c) %dopar% {
      i^2
    }
    cat("✓ Parallel computation test:", paste(test_result, collapse = ", "), "\n")
    
    # Cleanup test cluster
    stopCluster(test_cl)
    registerDoSEQ()
    cat("✓ Test cluster stopped\n")
    
    parallel_ready <- TRUE
    
  }, error = function(e) {
    cat("✗ Parallel test failed:", e$message, "\n")
    cat("→ Will fallback to sequential processing\n")
    parallel_ready <- FALSE
  })
} else {
  cat("→ Only 1 core available, will use sequential processing\n")
  parallel_ready <- FALSE
}

# 3. Check required packages
cat("\n=== CHECKING REQUIRED PACKAGES ===\n")
required_packages <- c("mizer", "therMizer", "calibrar", "tidyverse", 
                      "parallel", "doParallel", "foreach", "gridExtra", 
                      "corrplot", "viridis")

missing_packages <- c()
for (pkg in required_packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("✓", pkg, "\n")
  } else {
    cat("✗", pkg, "- MISSING\n")
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) > 0) {
  cat("\nTo install missing packages, run:\n")
  cat("install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))\n")
}

# 4. Check data files
cat("\n=== CHECKING DATA FILES ===\n")
required_files <- c(
  "params_steady_state_2011_2020_tol_0.00025.RDS",
  "constant_array_temp_1841.RDS", 
  "constant_array_n_pp_1841.RDS",
  "effort_array_1841_2010.rds",
  "yield_observed_timeseries_tidy.RDS"
)

missing_files <- c()
for (file in required_files) {
  if (file.exists(file)) {
    cat("✓", file, "\n")
  } else {
    cat("✗", file, "- MISSING\n")
    missing_files <- c(missing_files, file)
  }
}

# 5. Final recommendations
cat("\n=== SETUP SUMMARY ===\n")

if (parallel_ready && length(missing_packages) == 0 && length(missing_files) == 0) {
  cat("🎉 READY FOR PARALLEL OPTIMIZATION!\n\n")
  cat("Next steps:\n")
  cat("1. Run chunks in 12_Calibrar_Optimization_Refined.Rmd sequentially\n")
  cat("2. The script will automatically use", n_cores_to_use, "cores for parallel processing\n")
  cat("3. Monitor progress in console and plots/ directory\n")
  cat("4. Optimization will checkpoint every 50 iterations\n\n")
  
  cat("Expected runtime with parallel:", round((1000 * 35) / 3600 / min(n_cores_to_use * 0.7, 4), 1), "hours\n")
  
} else {
  cat("⚠️  SETUP ISSUES DETECTED\n\n")
  
  if (!parallel_ready) {
    cat("→ Parallel processing not available - will use sequential mode\n")
    cat("→ Expected runtime: ~2-3 days\n")
    Sys.setenv(R_PARALLELIZE = "FALSE")
  }
  
  if (length(missing_packages) > 0) {
    cat("→ Install missing packages first\n")
  }
  
  if (length(missing_files) > 0) {
    cat("→ Missing data files - check file paths\n")
  }
  
  cat("\nYou can still run the optimization sequentially even if parallel setup failed.\n")
}

cat("\n=== ENVIRONMENT CONFIGURED ===\n")
cat("R_PARALLELIZE =", Sys.getenv("R_PARALLELIZE"), "\n")
cat("Ready to run 12_Calibrar_Optimization_Refined.Rmd\n")