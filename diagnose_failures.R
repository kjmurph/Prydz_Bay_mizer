# Diagnose Failed Simulations
# Run this on the VM to see what errors occurred

cat("=== Diagnosing Failed Simulations ===\n\n")

# Load the progress file
progress_file <- "Output_large_files/climate_only_ensemble/progress_sequential.rds"
progress <- readRDS(progress_file)

cat("Total failed:", length(progress$failed), "\n\n")

# Show first few error messages
cat("=== Sample Error Messages ===\n\n")

n_show <- min(5, length(progress$failed))
for (i in 1:n_show) {
  cat("Simulation", progress$failed[[i]]$idx, ":\n")
  cat("  Error:", progress$failed[[i]]$error, "\n\n")
}

# Check if all errors are the same
unique_errors <- unique(sapply(progress$failed, function(x) x$error))
cat("=== Unique Error Types ===\n")
cat("Number of unique errors:", length(unique_errors), "\n\n")

for (i in seq_along(unique_errors)) {
  count <- sum(sapply(progress$failed, function(x) x$error == unique_errors[i]))
  cat("Error type", i, "(", count, "occurrences):\n")
  cat("  ", unique_errors[i], "\n\n")
}
