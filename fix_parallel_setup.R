# Quick Fix for Parallel Setup Error
# ==================================

# The error "missing value where TRUE/FALSE needed" occurs when use_parallel becomes NA
# This happens when memory detection fails. Here's the fix:

cat("=== DIAGNOSING PARALLEL SETUP ISSUE ===\n")

# Check environment variable
env_var <- Sys.getenv("R_PARALLELIZE", "TRUE")
cat("R_PARALLELIZE environment variable:", env_var, "\n")
use_parallel_env <- env_var == "TRUE"
cat("use_parallel_env:", use_parallel_env, "\n")

# Test memory detection step by step
cat("\nTesting memory detection...\n")

if (.Platform$OS.type == "windows") {
  cat("Detected Windows system\n")
  
  # Test Windows memory command
  tryCatch({
    mem_output <- system('wmic computersystem get TotalPhysicalMemory /value', intern = TRUE)
    cat("Raw memory output:\n")
    print(mem_output)
    
    # Find the line with memory info
    mem_line <- mem_output[grepl("TotalPhysicalMemory=", mem_output)]
    cat("Memory line:", mem_line, "\n")
    
    if (length(mem_line) > 0) {
      mem_value <- gsub("TotalPhysicalMemory=", "", mem_line)
      mem_value <- gsub("\\s", "", mem_value)  # Remove whitespace
      cat("Extracted value:", mem_value, "\n")
      
      mem_gb <- as.numeric(mem_value) / 1e9
      cat("Memory in GB:", mem_gb, "\n")
    }
  }, error = function(e) {
    cat("Windows memory detection failed:", e$message, "\n")
  })
} else {
  cat("Detected Unix-like system\n")
  tryCatch({
    mem_output <- system("free -b | grep '^Mem' | awk '{print $2}'", intern = TRUE)
    cat("Memory output:", mem_output, "\n")
    mem_gb <- as.numeric(mem_output) / 1e9
    cat("Memory in GB:", mem_gb, "\n")
  }, error = function(e) {
    cat("Unix memory detection failed:", e$message, "\n")
  })
}

# Simple fix: Set manual values
cat("\n=== APPLYING SIMPLE FIX ===\n")

# Manual configuration that should work
n_cores <- detectCores() - 1
n_cores <- min(n_cores, 4)  # Conservative
system_ram_gb <- 16  # Safe assumption for modern systems

use_parallel <- TRUE  # Force enable if you want parallel
# use_parallel <- FALSE  # Uncomment this to force sequential mode

cat("Fixed configuration:\n")
cat("- Cores to use:", n_cores, "\n") 
cat("- RAM assumed:", system_ram_gb, "GB\n")
cat("- Parallel enabled:", use_parallel, "\n")

# Set the environment variable properly
if (use_parallel && n_cores > 1) {
  Sys.setenv(R_PARALLELIZE = "TRUE")
  cat("✓ Environment set for parallel processing\n")
} else {
  Sys.setenv(R_PARALLELIZE = "FALSE") 
  cat("✓ Environment set for sequential processing\n")
}

cat("\n=== QUICK TEST ===\n")

# Quick parallel test
if (use_parallel && n_cores > 1) {
  tryCatch({
    library(doParallel)
    cl <- makeCluster(2)
    registerDoParallel(cl)
    
    test <- foreach(i = 1:4, .combine = c) %dopar% i^2
    cat("✓ Parallel test successful:", paste(test, collapse = ", "), "\n")
    
    stopCluster(cl)
    registerDoSEQ()
    cat("✓ Cluster cleanup successful\n")
    
  }, error = function(e) {
    cat("✗ Parallel test failed:", e$message, "\n")
    cat("→ Setting to sequential mode\n")
    Sys.setenv(R_PARALLELIZE = "FALSE")
  })
}

cat("\n=== READY TO RUN OPTIMIZATION ===\n")
cat("Environment variable R_PARALLELIZE:", Sys.getenv("R_PARALLELIZE"), "\n")
cat("Now run the chunks in 12_Calibrar_Optimization_Refined.Rmd\n")
cat("The setup chunk should work without the NA error.\n")