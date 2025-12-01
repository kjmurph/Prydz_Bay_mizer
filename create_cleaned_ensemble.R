# Create Cleaned MC Ensemble (2111 simulations)
# Removes simulation 1206 which contains NA values

cat("=== Creating Cleaned MC Ensemble (2111 simulations) ===\n\n")

# Load original
mc <- readRDS("Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/combined_rerun_successful_sims_20250923_122211.rds")
cat("Original simulations:", length(mc[["simulations"]]), "\n")

# Identify problem index (simulation 1206 has NA values)
problem_idx <- 1206
cat("Removing problematic simulation at index:", problem_idx, "\n")

# Create new object without problem simulation
mc_clean <- mc
mc_clean[["simulations"]] <- mc[["simulations"]][-problem_idx]
mc_clean[["n_successful"]] <- length(mc_clean[["simulations"]])

# Also remove from parameters if present
if ("parameters" %in% names(mc_clean) && length(mc_clean[["parameters"]]) == 2112) {
  mc_clean[["parameters"]] <- mc_clean[["parameters"]][-problem_idx]
}
if ("parameters_table" %in% names(mc_clean) && nrow(mc_clean[["parameters_table"]]) == 2112) {
  mc_clean[["parameters_table"]] <- mc_clean[["parameters_table"]][-problem_idx, ]
}
if ("flat_summary" %in% names(mc_clean) && nrow(mc_clean[["flat_summary"]]) == 2112) {
  mc_clean[["flat_summary"]] <- mc_clean[["flat_summary"]][-problem_idx, ]
}

# Add note about cleaning
mc_clean[["removed_indices"]] <- problem_idx
mc_clean[["cleaning_note"]] <- "Removed simulation 1206 due to NA values in abundance array"
mc_clean[["cleaned_date"]] <- Sys.time()

cat("Cleaned simulations:", length(mc_clean[["simulations"]]), "\n")

# Verify no more problems
cat("\nVerifying cleaned ensemble...\n")
problems <- 0
for (i in 1:length(mc_clean[["simulations"]])) {
  n_array <- mc_clean[["simulations"]][[i]]@n
  if (any(is.na(n_array)) || any(is.infinite(n_array))) {
    problems <- problems + 1
  }
}
cat("Remaining problematic simulations:", problems, "\n")

# Save cleaned version
output_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
saveRDS(mc_clean, output_file)
cat("\nSaved cleaned ensemble to:\n", output_file, "\n")

# Check file size
file_size_gb <- file.size(output_file) / 1e9
cat("File size:", round(file_size_gb, 2), "GB\n")

cat("\n=== Done ===\n")
