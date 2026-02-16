###############################################################################
# Run Prydz Bay Ecosystem Assessment
# 
# This script runs the comprehensive ecosystem assessment framework.
# Set TEST_MODE = TRUE to run a quick test with 50 simulations.
# Set TEST_MODE = FALSE (or comment out) for full analysis.
###############################################################################

# =============================================================================
# CONFIGURATION
# =============================================================================

# Set to TRUE for quick test run (5 simulations)
# Set to FALSE for full analysis (all 2111 simulations)
TEST_MODE <- FALSE  # Change to FALSE for production run

# Number of simulations for test mode
TEST_SIMS <- 5

# =============================================================================

# Source the main assessment framework
source("ecosystem_assessment.R")

cat("=============================================================\n")
if (TEST_MODE) {
  cat("STARTING ECOSYSTEM ASSESSMENT (TEST MODE: ", TEST_SIMS, " sims)\n", sep = "")
} else {
  cat("STARTING ECOSYSTEM ASSESSMENT (FULL ANALYSIS)\n")
}
cat("=============================================================\n\n")

# Check that ensemble files exist
cat("Checking ensemble files...\n")
if (!file.exists(ENSEMBLE_PATHS$fishing)) {
  stop("Fishing ensemble not found: ", ENSEMBLE_PATHS$fishing)
} else {
  cat("  ✓ Fishing ensemble found\n")
}

if (!file.exists(ENSEMBLE_PATHS$climate_only)) {
  stop("Climate-only ensemble not found: ", ENSEMBLE_PATHS$climate_only)
} else {
  cat("  ✓ Climate-only ensemble found\n")
}

# Run assessment
# Use max_sims for test mode, NULL for full analysis
n_sims <- if (TEST_MODE) TEST_SIMS else NULL
cat(sprintf("Running with max_sims = %s\n\n", ifelse(is.null(n_sims), "ALL", n_sims)))
results <- main_ecosystem_assessment(max_sims = n_sims)

# Print summary statistics
cat("\n=============================================================\n")
cat("SUMMARY: KEY FINDINGS\n")
cat("=============================================================\n\n")

# Get modern period comparison
modern_data <- results$heatmap_data %>%
  filter(decade == "2001-2010")

cat("Modern Period (2001-2010) Depletion Ratios:\n")
cat("(Values <1 indicate lower biomass/values in fishing scenario)\n\n")

for (i in 1:nrow(modern_data)) {
  row <- modern_data[i, ]
  depletion_pct <- ifelse(is.na(row$depletion_ratio), 
                          "NA", 
                          sprintf("%.1f%%", row$depletion_ratio * 100))
  cat(sprintf("  %s: %s of climate-only baseline (%s)\n", 
              row$metric_name, 
              depletion_pct,
              row$category))
}

# Get peak whaling period
peak_data <- results$heatmap_data %>%
  filter(decade == "1931-1940")

cat("\n\nPeak Whaling Period (1931-1940) Depletion Ratios:\n")

for (i in 1:nrow(peak_data)) {
  row <- peak_data[i, ]
  depletion_pct <- ifelse(is.na(row$depletion_ratio), 
                          "NA", 
                          sprintf("%.1f%%", row$depletion_ratio * 100))
  cat(sprintf("  %s: %s of climate-only baseline (%s)\n", 
              row$metric_name, 
              depletion_pct,
              row$category))
}

cat("\n=============================================================\n")
cat("See ecosystem_assessment_outputs/ for all visualizations\n")
cat("=============================================================\n")
