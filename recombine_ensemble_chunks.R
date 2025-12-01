# Recombine MC Ensemble Chunks on VM
# Run this on the VM after transferring all chunk files

library(mizer)

cat("=== Recombining MC Ensemble Chunks ===\n\n")

# Configuration - adjust this path for your VM
chunk_dir <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/ensemble_chunks"
output_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"

# Check for metadata
metadata_file <- file.path(chunk_dir, "ensemble_metadata.rds")
if (file.exists(metadata_file)) {
  metadata <- readRDS(metadata_file)
  cat("Metadata found:\n")
  cat("  Expected simulations:", metadata$total_simulations, "\n")
  cat("  Expected chunks:", metadata$total_chunks, "\n")
  cat("  Original split date:", as.character(metadata$split_date), "\n\n")
} else {
  cat("Warning: No metadata file found. Proceeding anyway.\n\n")
  metadata <- NULL
}

# Find all chunk files
chunk_files <- list.files(chunk_dir, pattern = "ensemble_chunk_.*\\.rds", full.names = TRUE)
chunk_files <- sort(chunk_files)  # Ensure correct order

cat("Found", length(chunk_files), "chunk files\n\n")

if (!is.null(metadata) && length(chunk_files) != metadata$total_chunks) {
  stop("ERROR: Expected ", metadata$total_chunks, " chunks but found ", length(chunk_files))
}

# Load and combine all chunks
all_simulations <- list()
total_loaded <- 0

for (chunk_file in chunk_files) {
  cat("Loading", basename(chunk_file), "...")
  chunk_data <- readRDS(chunk_file)
  
  # Add simulations to list
  for (j in 1:length(chunk_data$simulations)) {
    global_idx <- chunk_data$start_index + j - 1
    all_simulations[[global_idx]] <- chunk_data$simulations[[j]]
  }
  
  total_loaded <- total_loaded + chunk_data$n_in_chunk
  cat(" done (", chunk_data$n_in_chunk, " sims, total:", total_loaded, ")\n")
}

cat("\nTotal simulations loaded:", length(all_simulations), "\n")

# Validate
if (!is.null(metadata) && length(all_simulations) != metadata$total_simulations) {
  stop("ERROR: Expected ", metadata$total_simulations, " simulations but got ", length(all_simulations))
}

# Create the combined object matching original structure
mc_results_combined <- list(
  simulations = all_simulations,
  n_simulations = length(all_simulations),
  recombined = TRUE,
  recombine_date = Sys.time()
)

# Save
cat("\nSaving combined ensemble to:", output_file, "\n")
saveRDS(mc_results_combined, output_file)

file_size <- round(file.size(output_file) / 1e9, 2)
cat("File size:", file_size, "GB\n")

cat("\n=== Recombine Complete ===\n")
cat("Ready to run: run_climate_only_ensemble_parallel_32core.R\n")
