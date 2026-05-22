###############################################################################
# Quick re-render: absolute 50% CI combined heatmap only
# Sources the updated ecosystem_assessment_v2.R (with IQR annotations)
###############################################################################

suppressPackageStartupMessages({
  library(mizer)
  library(therMizer)
  library(dplyr)
})

source("ecosystem_assessment_v3.R")

cat("Loading cached data...\n")
fishing_raw <- readRDS(file.path(OUTPUT_DIR_LARGE, "fishing_metrics_raw.rds"))
climate_raw <- readRDS(file.path(OUTPUT_DIR_LARGE, "climate_only_metrics_raw.rds"))
b0_ref      <- readRDS(file.path(OUTPUT_DIR_LARGE, "b0_reference.rds"))

cat("Deriving thresholds & paired comparisons...\n")
empirical_thresholds <- derive_empirical_thresholds(b0_ref)
paired_data <- compute_paired_comparisons(fishing_raw, climate_raw, b0_ref, empirical_thresholds)

cat("Summarizing...\n")
biomass_summaries    <- summarize_biomass_ratios(paired_data)
structural_summaries <- summarize_structural_deviations(paired_data, empirical_thresholds)

cat("Generating absolute 50% CI heatmap...\n")
plot_combined_heatmap(
  biomass_summaries$absolute, structural_summaries$absolute,
  "Ecosystem Assessment: Prydz Bay (Absolute, IQR)",
  OUTPUT_DIR, "heatmap_combined_absolute_50ci",
  envelope_col = "prop_outside_b0_50", null_expected = STRUCTURAL_NULL_50CI)

cat("\nDone.\n")
