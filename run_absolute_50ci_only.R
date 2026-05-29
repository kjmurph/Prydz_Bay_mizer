###############################################################################
# Quick re-render: absolute 50% CI combined heatmap only
# Sources the updated ecosystem_assessment_v2.R (with IQR annotations)
###############################################################################

# === CONFIGURATION ===
# Set WMIN_LABEL to match the w_min variant you want to plot.
# Must match the label used in patch_spectrum_min_w.R (TARGET_LABEL).
#   "wmin_3.16e-8g"  -> mesozooplankton w_min (original full-spectrum)
#   "wmin_1g"        -> 1 g cutoff
WMIN_LABEL <- "wmin_1g"

suppressPackageStartupMessages({
  library(mizer)
  library(therMizer)
  library(dplyr)
})

source("ecosystem_assessment_v3.R")

cat("Loading cached data...\n")
cat(sprintf("  w_min variant: %s\n", WMIN_LABEL))
fishing_raw <- readRDS(file.path(OUTPUT_DIR_LARGE, sprintf("fishing_metrics_raw_%s.rds",      WMIN_LABEL)))
climate_raw <- readRDS(file.path(OUTPUT_DIR_LARGE, sprintf("climate_only_metrics_raw_%s.rds", WMIN_LABEL)))
b0_ref      <- readRDS(file.path(OUTPUT_DIR_LARGE, sprintf("b0_reference_%s.rds",             WMIN_LABEL)))

cat("Deriving thresholds & paired comparisons...\n")
empirical_thresholds <- derive_empirical_thresholds(b0_ref)
paired_data <- compute_paired_comparisons(fishing_raw, climate_raw, b0_ref, empirical_thresholds)

cat("Summarizing...\n")
biomass_summaries    <- summarize_biomass_ratios(paired_data)
structural_summaries <- summarize_structural_deviations(paired_data, empirical_thresholds)

cat("Generating absolute 50% CI heatmap...\n")
plot_combined_heatmap(
  biomass_summaries$absolute, structural_summaries$absolute,
  sprintf("Ecosystem Assessment: Prydz Bay (Absolute, IQR) [%s]", WMIN_LABEL),
  OUTPUT_DIR, sprintf("heatmap_combined_absolute_50ci_%s", WMIN_LABEL),
  envelope_col = "prop_outside_b0_50", null_expected = STRUCTURAL_NULL_50CI)

cat("\nDone.\n")
