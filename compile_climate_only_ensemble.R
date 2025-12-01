# Compile Climate-Only Ensemble Results
# Combines individual simulation files into a single ensemble object
# Run this after all simulations are complete

library(mizer)

cat("=== Compiling Climate-Only Ensemble Results ===\n\n")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
output_dir <- "Output_large_files/climate_only_ensemble"
sims_dir <- file.path(output_dir, "individual_sims")

# ------------------------------------------------------------------------------
# Load progress info
# ------------------------------------------------------------------------------
progress_file <- file.path(output_dir, "progress_sequential.rds")

if (!file.exists(progress_file)) {
  stop("No progress file found. Run the simulation script first.")
}

progress <- readRDS(progress_file)
completed_indices <- progress$completed
failed_indices <- progress$failed

cat("Found progress file:\n")
cat("  Completed simulations:", length(completed_indices), "\n")
cat("  Failed simulations:", length(failed_indices), "\n")

# ------------------------------------------------------------------------------
# Load individual simulation files
# ------------------------------------------------------------------------------
cat("\nLoading individual simulation files...\n")

# Find all simulation files
sim_files <- list.files(sims_dir, pattern = "sim_\\d+_climate_only\\.rds", full.names = TRUE)
cat("  Found", length(sim_files), "simulation files\n")

# Extract indices from filenames
get_idx <- function(f) {
  as.integer(gsub(".*sim_(\\d+)_climate_only\\.rds", "\\1", basename(f)))
}
file_indices <- sapply(sim_files, get_idx)

# Load simulations
cat("  Loading simulations...")
climate_only_sims <- vector("list", length(sim_files))
names(climate_only_sims) <- paste0("sim_", file_indices)

pb <- txtProgressBar(min = 0, max = length(sim_files), style = 3)
for (i in seq_along(sim_files)) {
  climate_only_sims[[i]] <- readRDS(sim_files[i])
  setTxtProgressBar(pb, i)
}
close(pb)
cat(" done\n")

# Verify all loaded correctly
n_loaded <- sum(!sapply(climate_only_sims, is.null))
cat("  Successfully loaded:", n_loaded, "simulations\n")

# ------------------------------------------------------------------------------
# Verify simulation properties
# ------------------------------------------------------------------------------
cat("\nVerifying simulation properties...\n")

# Check first simulation
sim1 <- climate_only_sims[[1]]
times <- as.numeric(dimnames(sim1@n)$time)
species <- dimnames(sim1@n)$sp

cat("  Time range:", min(times), "-", max(times), "\n")
cat("  Time steps:", length(times), "\n")
cat("  Species:", length(species), "\n")

# Verify all simulations have same structure
all_match <- TRUE
for (i in 2:length(climate_only_sims)) {
  sim_i <- climate_only_sims[[i]]
  times_i <- as.numeric(dimnames(sim_i@n)$time)
  if (!identical(times, times_i)) {
    cat("  WARNING: Simulation", i, "has different time range\n")
    all_match <- FALSE
  }
}

if (all_match) {
  cat("  All simulations have consistent structure\n")
}

# ------------------------------------------------------------------------------
# Load source (fished) ensemble info
# ------------------------------------------------------------------------------
cat("\nLoading source (fished) ensemble info...\n")

mc_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
mc_results <- readRDS(mc_file)
n_original <- length(mc_results$simulations)

cat("  Original fished ensemble:", n_original, "simulations\n")
cat("  Climate-only ensemble:", length(climate_only_sims), "simulations\n")

if (length(climate_only_sims) == n_original) {
  cat("  ✓ Matched 1:1 with fished ensemble\n")
} else {
  cat("  ⚠ Size mismatch - some simulations may have failed\n")
}

# ------------------------------------------------------------------------------
# Compile final ensemble object
# ------------------------------------------------------------------------------
cat("\nCompiling final ensemble object...\n")

ensemble_output <- list(
  simulations = climate_only_sims,
  n_simulations = length(climate_only_sims),
  simulation_indices = file_indices,
  time_range = c(min(times), max(times)),
  species = species,
  failed = failed_indices,
  source_ensemble = mc_file,
  settings = list(
    spinup_years = 118,
    effort = 0,  # No fishing
    climate_forcing = TRUE
  ),
  created = Sys.time()
)

# ------------------------------------------------------------------------------
# Save compiled ensemble
# ------------------------------------------------------------------------------
cat("\nSaving compiled ensemble...\n")

# Main ensemble file
ensemble_file <- file.path(output_dir, "climate_only_ensemble_compiled.rds")
saveRDS(ensemble_output, ensemble_file)
cat("  Saved:", ensemble_file, "\n")

# File size
file_size_gb <- file.size(ensemble_file) / 1e9
cat("  File size:", round(file_size_gb, 2), "GB\n")

# Also save a lightweight version with just simulation references
lightweight <- list(
  simulation_indices = file_indices,
  n_simulations = length(climate_only_sims),
  time_range = ensemble_output$time_range,
  species = species,
  individual_files_dir = sims_dir,
  source_ensemble = mc_file,
  created = Sys.time()
)
lightweight_file <- file.path(output_dir, "climate_only_ensemble_index.rds")
saveRDS(lightweight, lightweight_file)
cat("  Saved index:", lightweight_file, "\n")

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------
cat("\n=== Compilation Complete ===\n")
cat("\nClimate-Only Ensemble Summary:\n")
cat("  Total simulations:", length(climate_only_sims), "\n")
cat("  Time range: 1841-2010\n")
cat("  Species:", length(species), "\n")
cat("  Forcing: Climate only (no fishing)\n")

cat("\nOutput files:\n")
cat("  Full ensemble:", ensemble_file, "\n")
cat("  Lightweight index:", lightweight_file, "\n")
cat("  Individual sims:", sims_dir, "\n")

cat("\nThis ensemble can now be used for matched fished/unfished comparisons.\n")
cat("Each simulation uses identical parameters to its fished counterpart,\n")
cat("enabling direct assessment of fishing impacts on ecosystem structure.\n")
