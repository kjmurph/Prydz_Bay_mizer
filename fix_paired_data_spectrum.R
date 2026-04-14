###############################################################################
# FIX: Rebuild paired_data.rds and empirical_thresholds.rds from the
# already-updated raw files (which have full-spectrum slope/intercept values).
#
# Problem:
#   recalculate_spectrum_slope.R correctly patched fishing_metrics_raw.rds,
#   climate_only_metrics_raw.rds, and b0_reference.rds with full-community-
#   spectrum values (no max_w = 1e6 filter). However, it did NOT regenerate
#   paired_data.rds or empirical_thresholds.rds.
#
#   replot_existing_outputs.R (and summaries) read paired_data.rds, so all
#   heatmaps still use the old filtered spectrum values.
#
# Fix:
#   1. Source ecosystem_assessment_v2.R for functions + constants
#   2. Load updated raw files (fishing, climate, b0)
#   3. Recalculate empirical_thresholds from updated b0_reference
#   4. Regenerate paired_data via compute_paired_comparisons()
#   5. Save both to Output_large_files/ecosystem_assessment/
#   6. Regenerate all heatmaps + summaries (same logic as replot_existing_outputs)
#
# Usage:
#   Rscript fix_paired_data_spectrum.R
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(viridis)
  library(patchwork)
})

setwd("C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/Prydz_Bay_mizer")

cat("=============================================================\n")
cat("FIX: Rebuild paired_data & thresholds with full-spectrum values\n")
cat("=============================================================\n\n")

# Source the v2 script for all function definitions and constants
cat("Sourcing ecosystem_assessment_v2.R for functions...\n")
source("ecosystem_assessment_v2.R")

data_dir   <- "Output_large_files/ecosystem_assessment"
output_dir <- "ecosystem_assessment_outputs"

# -------------------------------------------------------------------------
# 1. Verify timestamps — confirm raw files are newer than paired_data
# -------------------------------------------------------------------------
cat("\n--- File timestamps ---\n")
for (f in c("fishing_metrics_raw.rds", "climate_only_metrics_raw.rds",
            "b0_reference.rds", "paired_data.rds", "empirical_thresholds.rds")) {
  info <- file.info(file.path(data_dir, f))
  cat(sprintf("  %-45s %s\n", f, format(info$mtime, "%Y-%m-%d %H:%M:%S")))
}

# -------------------------------------------------------------------------
# 2. Load updated raw files
# -------------------------------------------------------------------------
cat("\nLoading updated raw files...\n")
fishing_raw <- readRDS(file.path(data_dir, "fishing_metrics_raw.rds"))
climate_raw <- readRDS(file.path(data_dir, "climate_only_metrics_raw.rds"))
b0_ref      <- readRDS(file.path(data_dir, "b0_reference.rds"))

# Load the old paired_data for comparison
paired_old <- readRDS(file.path(data_dir, "paired_data.rds"))

cat(sprintf("  fishing_raw:  %d rows, %d sims\n", nrow(fishing_raw), length(unique(fishing_raw$sim_id))))
cat(sprintf("  climate_raw:  %d rows, %d sims\n", nrow(climate_raw), length(unique(climate_raw$sim_id))))
cat(sprintf("  b0_reference: %d rows\n", nrow(b0_ref)))

# Quick sanity check: raw files should have updated values
cat("\n--- Pre-fix check (sim_id=1, decade='1841-1850') ---\n")
fr1 <- fishing_raw %>% filter(sim_id == 1, decade == "1841-1850")
pd1 <- paired_old %>% filter(sim_id == 1, decade == "1841-1850")
cat(sprintf("  fishing_raw  spectrum_slope: %.6f (UPDATED, full spectrum)\n", fr1$spectrum_slope))
cat(sprintf("  paired_data  spectrum_slope: %.6f (OLD, filtered max_w=1e6)\n", pd1$spectrum_slope_fish))
cat(sprintf("  Values differ: %s\n", abs(fr1$spectrum_slope - pd1$spectrum_slope_fish) > 1e-10))

# -------------------------------------------------------------------------
# 3. Recalculate empirical thresholds from updated b0_reference
# -------------------------------------------------------------------------
cat("\n--- Recalculating empirical thresholds ---\n")
empirical_thresholds <- derive_empirical_thresholds(b0_ref)

# Compare old thresholds for spectrum metrics
old_thresh <- readRDS(file.path(data_dir, "empirical_thresholds.rds"))
cat("\n--- Threshold comparison (spectrum_slope) ---\n")
cat(sprintf("  OLD: median=%.6f  [Q05=%.6f, Q95=%.6f]\n",
            old_thresh$spectrum_slope$median, old_thresh$spectrum_slope$q05, old_thresh$spectrum_slope$q95))
cat(sprintf("  NEW: median=%.6f  [Q05=%.6f, Q95=%.6f]\n",
            empirical_thresholds$spectrum_slope$median, empirical_thresholds$spectrum_slope$q05,
            empirical_thresholds$spectrum_slope$q95))
cat("--- Threshold comparison (spectrum_intercept) ---\n")
cat(sprintf("  OLD: median=%.6f  [Q05=%.6f, Q95=%.6f]\n",
            old_thresh$spectrum_intercept$median, old_thresh$spectrum_intercept$q05,
            old_thresh$spectrum_intercept$q95))
cat(sprintf("  NEW: median=%.6f  [Q05=%.6f, Q95=%.6f]\n",
            empirical_thresholds$spectrum_intercept$median, empirical_thresholds$spectrum_intercept$q05,
            empirical_thresholds$spectrum_intercept$q95))

# -------------------------------------------------------------------------
# 4. Rebuild paired_data from updated raw files + new thresholds
# -------------------------------------------------------------------------
cat("\n--- Rebuilding paired_data ---\n")
paired_data <- compute_paired_comparisons(
  fishing_raw, climate_raw, b0_ref, empirical_thresholds)

cat(sprintf("  New paired_data: %d rows\n", nrow(paired_data)))

# Post-fix check
pd1_new <- paired_data %>% filter(sim_id == 1, decade == "1841-1850")
cat(sprintf("\n--- Post-fix check (sim_id=1, decade='1841-1850') ---\n"))
cat(sprintf("  fishing_raw  spectrum_slope:      %.6f\n", fr1$spectrum_slope))
cat(sprintf("  paired_data  spectrum_slope_fish:  %.6f\n", pd1_new$spectrum_slope_fish))
cat(sprintf("  Values match: %s\n", abs(fr1$spectrum_slope - pd1_new$spectrum_slope_fish) < 1e-10))

# -------------------------------------------------------------------------
# 5. Save updated paired_data & thresholds
# -------------------------------------------------------------------------
cat("\n--- Saving updated files ---\n")

# Backup old paired_data
saveRDS(paired_old, file.path(data_dir, "paired_data_pre_fullspectrum_backup.rds"))
cat("  Backed up old paired_data -> paired_data_pre_fullspectrum_backup.rds\n")

# Save updated files
saveRDS(paired_data, file.path(data_dir, "paired_data.rds"))
cat("  Saved: paired_data.rds\n")

saveRDS(empirical_thresholds, file.path(data_dir, "empirical_thresholds.rds"))
cat("  Saved: empirical_thresholds.rds\n")

# Also save to standard output dir
saveRDS(empirical_thresholds, file.path(output_dir, "empirical_thresholds.rds"))

# -------------------------------------------------------------------------
# 6. Regenerate all summaries and heatmaps
# -------------------------------------------------------------------------
cat("\n=============================================================\n")
cat("REGENERATING ALL SUMMARIES AND HEATMAPS\n")
cat("=============================================================\n\n")

n_sims <- length(unique(paired_data$sim_id))

cat("Summarizing biomass ratios...\n")
biomass_summaries <- summarize_biomass_ratios(paired_data)

cat("Summarizing structural deviations...\n")
structural_summaries <- summarize_structural_deviations(paired_data, empirical_thresholds)

cat("Summarizing exploitation rates...\n")
exploitation_summary <- summarize_exploitation(paired_data)

# ---- Heatmaps ----
cat("\nGenerating heatmaps...\n")
plots <- list()

cat("  Category A: Biomass ratio heatmaps...\n")
plots$bio_exploit_ratio <- plot_biomass_ratio_heatmap(
  biomass_summaries$exploit, "Exploitation Impact: Biomass Metrics",
  "Median ratio: Fishing / Climate-only (paired)",
  output_dir, "heatmap_biomass_exploitation_ratio")
plots$bio_absolute_ratio <- plot_biomass_ratio_heatmap(
  biomass_summaries$absolute, "Absolute Health: Biomass Metrics",
  sprintf("Median ratio: Fishing / B0 (%d-%d)", B0_PERIOD[1], B0_PERIOD[2]),
  output_dir, "heatmap_biomass_absolute_ratio")
plots$bio_climate_ratio <- plot_biomass_ratio_heatmap(
  biomass_summaries$climate, "Climate Impact: Biomass Metrics",
  sprintf("Median ratio: Climate-only / B0 (%d-%d)", B0_PERIOD[1], B0_PERIOD[2]),
  output_dir, "heatmap_biomass_climate_ratio")

cat("  Category A: Biomass proportion heatmaps...\n")
plots$bio_prop_075 <- plot_biomass_proportion_heatmap(
  biomass_summaries$absolute, "prop_below_075", 0.75,
  "CCAMLR \u03B3\u2082 escapement; Constable et al. 2000",
  "Absolute Health: P(Biomass/B0 < 0.75)",
  output_dir, "heatmap_biomass_prop_below_075")
plots$bio_prop_040 <- plot_biomass_proportion_heatmap(
  biomass_summaries$absolute, "prop_below_040", 0.40,
  "BMSY proxy; Restrepo et al. 1998",
  "Absolute Health: P(Biomass/B0 < 0.40)",
  output_dir, "heatmap_biomass_prop_below_040")
plots$bio_prop_020 <- plot_biomass_proportion_heatmap(
  biomass_summaries$absolute, "prop_below_020", 0.20,
  "CCAMLR \u03B3\u2081 / MSST collapse; Constable et al. 2000",
  "Absolute Health: P(Biomass/B0 < 0.20)",
  output_dir, "heatmap_biomass_prop_below_020")
plots$whale_prop_054 <- plot_biomass_proportion_heatmap(
  biomass_summaries$absolute, "prop_below_054", 0.54,
  "IWC RMP protection level; IWC 1994, Punt & Donovan 2007",
  "Whale Populations: P(Biomass/B0 < 0.54)",
  output_dir, "heatmap_whale_prop_below_054", metrics_filter = WHALE_METRICS)
plots$whale_exploit_054 <- plot_biomass_proportion_heatmap(
  biomass_summaries$exploit, "prop_below_054", 0.54,
  "IWC RMP protection level; IWC 1994, Punt & Donovan 2007",
  "Exploitation Impact on Whales: P(Fishing/Climate-only < 0.54)",
  output_dir, "heatmap_whale_exploit_prop_below_054", metrics_filter = WHALE_METRICS)

cat("  Category B: Structural deviation heatmaps...\n")
plots$str_outside_90 <- plot_structural_deviation_heatmap(
  structural_summaries$absolute, "prop_outside_b0_90",
  "5th\u201395th percentile (empirical B0 envelope, 1841\u20131860)",
  "Structural Metrics: Outside B0 Natural Range",
  output_dir, "heatmap_structural_outside_b0_90")
plots$str_outside_98 <- plot_structural_deviation_heatmap(
  structural_summaries$absolute, "prop_outside_b0_98",
  "1st\u201399th percentile (substantially altered)",
  "Structural Metrics: Substantially Altered",
  output_dir, "heatmap_structural_outside_b0_98")
plots$str_zscore_abs <- plot_structural_zscore_heatmap(
  structural_summaries$absolute,
  "Structural Metrics: Standardized Departure from B0",
  output_dir, "heatmap_structural_zscore_absolute")
plots$str_zscore_exploit <- plot_structural_zscore_heatmap(
  structural_summaries$exploit,
  "Structural Metrics: Exploitation-Driven Departure",
  output_dir, "heatmap_structural_zscore_exploitation")

cat("  Combined heatmaps...\n")
plots$combined_absolute <- plot_combined_heatmap(
  biomass_summaries$absolute, structural_summaries$absolute,
  "Ecosystem Assessment: Prydz Bay (Absolute)",
  output_dir, "heatmap_combined_absolute")
plots$combined_exploit <- plot_combined_heatmap(
  biomass_summaries$exploit, structural_summaries$exploit,
  "Ecosystem Assessment: Exploitation Impact",
  output_dir, "heatmap_combined_exploitation")
plots$combined_climate <- plot_combined_heatmap(
  biomass_summaries$climate, structural_summaries$climate,
  "Ecosystem Assessment: Climate Impact",
  output_dir, "heatmap_combined_climate")

cat("  Category C: Exploitation heatmap...\n")
plots$exploitation_F <- plot_exploitation_heatmap(exploitation_summary, output_dir)

# ---- Save summary CSVs ----
cat("\nSaving summary CSVs...\n")
for (comp in c("exploit", "absolute", "climate")) {
  write.csv(biomass_summaries[[comp]],
            file.path(output_dir, sprintf("summary_biomass_%s.csv", comp)), row.names = FALSE)
  write.csv(structural_summaries[[comp]],
            file.path(output_dir, sprintf("summary_structural_%s.csv", comp)), row.names = FALSE)
}
write.csv(exploitation_summary,
          file.path(output_dir, "summary_exploitation_rates.csv"), row.names = FALSE)

# Also copy updated raw files + b0 to standard output dir
saveRDS(fishing_raw, file.path(output_dir, "fishing_metrics_raw.rds"))
saveRDS(climate_raw, file.path(output_dir, "climate_only_metrics_raw.rds"))
saveRDS(b0_ref,      file.path(output_dir, "b0_reference.rds"))

cat("\n=============================================================\n")
cat("FIX COMPLETE\n")
cat("=============================================================\n")
cat(sprintf("  paired_data.rds rebuilt: %d rows (%d sims x %d decades)\n",
            nrow(paired_data), n_sims, length(unique(paired_data$decade))))
cat("  empirical_thresholds.rds recalculated from updated b0_reference\n")
cat("  All heatmaps and summary CSVs regenerated\n\n")
