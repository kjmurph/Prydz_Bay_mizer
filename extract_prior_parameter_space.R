# Extract Prior Parameter Space from Individual MC Run Files
# This script identifies the SD values used in the Monte Carlo simulations

cat("Extracting Prior Parameter Space Information...\n\n")

library(dplyr)

# ==============================================================================
# 1. Parse filenames to extract SD information
# ==============================================================================
cat("Scanning Monte Carlo result files...\n")

# Get all RDS files from rerun_results directory
files <- list.files(
  "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results",
  pattern = "seed_rerun.*\\.rds$",
  full.names = TRUE
)

cat("  Found", length(files), "individual run files\n\n")

# Extract SD information from filenames
parse_sd_from_filename <- function(filename) {
  # Pattern: SD_X_Y_Z where X=catchability, Y=abundance, Z=gamma
  sd_match <- regmatches(filename, regexpr("SD_[0-9]+_[0-9]+_[0-9]+", filename))
  if (length(sd_match) > 0) {
    parts <- strsplit(sd_match, "_")[[1]]
    return(list(
      catchability_sd = as.numeric(parts[2]),
      abundance_sd = as.numeric(parts[3]),
      gamma_sd = as.numeric(parts[4]),
      filename = basename(filename)
    ))
  }
  return(NULL)
}

# Parse all files
sd_info <- lapply(files, parse_sd_from_filename)
sd_info <- sd_info[!sapply(sd_info, is.null)]
sd_df <- bind_rows(sd_info)

cat("Prior Parameter Space (from individual runs):\n")
print(sd_df)

# ==============================================================================
# 2. Summarize unique SD combinations used
# ==============================================================================
cat("\n\nUnique SD combinations used:\n")
unique_sds <- sd_df %>%
  distinct(catchability_sd, abundance_sd, gamma_sd) %>%
  arrange(catchability_sd, abundance_sd, gamma_sd)

print(unique_sds)

# ==============================================================================
# 3. Get total number of attempts across all runs
# ==============================================================================
cat("\n\nCounting total simulations attempted...\n")

total_attempts <- 0
total_successful <- 0

for (file in files) {
  tryCatch({
    mc <- readRDS(file)
    if (!is.null(mc$n_attempts)) {
      total_attempts <- total_attempts + mc$n_attempts
      total_successful <- total_successful + mc$n_successful
      cat("  ", basename(file), "- Attempted:", mc$n_attempts, "Successful:", mc$n_successful, "\n")
    }
  }, error = function(e) {
    # Skip files that can't be read
  })
}

cat("\nTotal across all runs:\n")
cat("  Attempted:", total_attempts, "\n")
cat("  Successful:", total_successful, "\n")
cat("  Success rate:", round(100 * total_successful / total_attempts, 2), "%\n")

# ==============================================================================
# 4. Create comprehensive prior parameter space summary
# ==============================================================================
prior_summary <- data.frame(
  Parameter = c(
    "catchability_sd_values",
    "abundance_sd_values",
    "gamma_sd_values",
    "total_mc_runs",
    "total_attempts_all_runs",
    "total_successful_all_runs",
    "success_rate_percent",
    "final_accepted_count"
  ),
  Value = c(
    paste(unique(sd_df$catchability_sd), collapse = ", "),
    paste(unique(sd_df$abundance_sd), collapse = ", "),
    paste(unique(sd_df$gamma_sd), collapse = ", "),
    nrow(sd_df),
    total_attempts,
    total_successful,
    round(100 * total_successful / total_attempts, 2),
    "2111-2112"
  ),
  Description = c(
    "Standard deviations used for catchability variation",
    "Standard deviations used for abundance scaling variation",
    "Standard deviations used for gamma variation",
    "Number of separate Monte Carlo runs performed",
    "Total number of parameter combinations attempted",
    "Total number of simulations that completed successfully",
    "Percentage of attempted simulations that succeeded",
    "Final number accepted after biomass rejection criteria"
  )
)

# ==============================================================================
# 5. Save updated prior parameter space
# ==============================================================================
cat("\nSaving updated prior parameter space...\n")

write.csv(prior_summary,
          "monte_carlo_2111_summaries/prior_parameter_space_detailed.csv",
          row.names = FALSE)
cat("  ✓ Saved: prior_parameter_space_detailed.csv\n")

# Also save the unique SD combinations
write.csv(unique_sds,
          "monte_carlo_2111_summaries/prior_sd_combinations.csv",
          row.names = FALSE)
cat("  ✓ Saved: prior_sd_combinations.csv\n")

# Save per-run details
write.csv(sd_df,
          "monte_carlo_2111_summaries/prior_all_mc_runs_details.csv",
          row.names = FALSE)
cat("  ✓ Saved: prior_all_mc_runs_details.csv\n")

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   PRIOR PARAMETER SPACE EXTRACTION COMPLETE!                 ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

cat("Summary:\n")
print(prior_summary)

cat("\n✓ All prior parameter space data extracted and saved!\n")
