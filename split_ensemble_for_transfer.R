# Split MC Ensemble for Transfer
# Breaks the 1.78 GB ensemble into smaller chunks for easier transfer

library(mizer)

cat("=== Splitting MC Ensemble for Transfer ===\n\n")

# Force garbage collection before loading large file
gc()

# Configuration
chunk_size <- 50  # Simulations per chunk (~45 MB each)
input_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
output_dir <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/ensemble_chunks"

# Create output directory
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Load the full ensemble
cat("Loading full ensemble (this may take a minute)...\n")
cat("  File:", input_file, "\n")
cat("  File size:", round(file.size(input_file)/1e9, 2), "GB\n")
flush.console()

mc_results <- readRDS(input_file)
cat("  Loaded successfully\n")

fished_sims <- mc_results$simulations
n_sims <- length(fished_sims)
cat("  Total simulations:", n_sims, "\n\n")

# Calculate chunks
n_chunks <- ceiling(n_sims / chunk_size)
cat("Splitting into", n_chunks, "chunks of up to", chunk_size, "simulations each\n\n")

# Split and save
for (i in 1:n_chunks) {
  start_idx <- (i - 1) * chunk_size + 1
  end_idx <- min(i * chunk_size, n_sims)
  
  chunk_sims <- fished_sims[start_idx:end_idx]
  
  chunk_file <- file.path(output_dir, sprintf("ensemble_chunk_%02d.rds", i))
  
  # Save chunk with metadata
  chunk_data <- list(
    simulations = chunk_sims,
    chunk_number = i,
    total_chunks = n_chunks,
    start_index = start_idx,
    end_index = end_idx,
    n_in_chunk = length(chunk_sims)
  )
  
  saveRDS(chunk_data, chunk_file)
  
  file_size <- round(file.size(chunk_file) / 1e6, 1)
  cat(sprintf("  Chunk %2d: sims %4d-%4d (%3d sims) -> %s (%.1f MB)\n", 
              i, start_idx, end_idx, length(chunk_sims), basename(chunk_file), file_size))
}

# Also save the metadata separately
metadata <- list(
  total_simulations = n_sims,
  total_chunks = n_chunks,
  chunk_size = chunk_size,
  original_file = input_file,
  split_date = Sys.time()
)
saveRDS(metadata, file.path(output_dir, "ensemble_metadata.rds"))

cat("\n=== Split Complete ===\n")
cat("Output directory:", output_dir, "\n")
cat("Total chunks:", n_chunks, "\n")

# Show total size
chunk_files <- list.files(output_dir, pattern = "ensemble_chunk_.*\\.rds", full.names = TRUE)
total_size <- sum(file.size(chunk_files)) / 1e6
cat("Total chunk size:", round(total_size, 1), "MB\n")
cat("\nTransfer these files to the VM:\n")
cat("  - All", length(chunk_files), "chunk files\n")
cat("  - ensemble_metadata.rds\n")
