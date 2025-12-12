# Compile the completed climate-only ensemble checkpoint into final format

cat("=== Compiling Climate-Only Ensemble ===\n")

# Load checkpoint
checkpoint_file <- "Output_large_files/climate_only_ensemble/checkpoint_climate_only.rds"
cat("Loading checkpoint:", checkpoint_file, "\n")
all_results <- readRDS(checkpoint_file)

cat("Total results in checkpoint:", length(all_results), "\n")

# Separate successful and failed
successful_results <- list()
failed_results <- list()

for (i in seq_along(all_results)) {
  if (!is.null(all_results[[i]]) && all_results[[i]]$success) {
    successful_results[[length(successful_results) + 1]] <- list(
      sim_idx = i,
      simulation = all_results[[i]]$simulation
    )
  } else if (!is.null(all_results[[i]])) {
    failed_results[[length(failed_results) + 1]] <- list(
      sim_idx = i,
      error = all_results[[i]]$error
    )
  }
}

cat("Successful simulations:", length(successful_results), "\n")
cat("Failed simulations:", length(failed_results), "\n")

# Extract simulation objects
climate_only_sims <- lapply(successful_results, function(x) x$simulation)

# Create output in same format as fished ensemble
final_output <- list(
  simulations = climate_only_sims,
  n_successful = length(climate_only_sims),
  n_failed = length(failed_results),
  failed_info = if (length(failed_results) > 0) failed_results else NULL,
  settings = list(
    effort = 0,
    note = "Climate-only (unfished) control - uses identical params as fished ensemble"
  ),
  created = Sys.time()
)

# Save in the format expected by extraction scripts
output_file <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
cat("\nSaving compiled ensemble to:", output_file, "\n")
saveRDS(final_output, output_file)

# Verify
test <- readRDS(output_file)
cat("\nVerification:\n")
cat("  Number of simulations:", length(test$simulations), "\n")
cat("  First sim dimensions:", dim(test$simulations[[1]]@n), "\n")
cat("  File size:", file.size(output_file) / 1e9, "GB\n")

cat("\n✓ Climate-only ensemble compiled successfully!\n")
cat("Ready for FishMIP output extraction.\n")
