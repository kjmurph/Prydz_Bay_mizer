###############################################################################
# Regenerate all heatmaps from already-patched RDS data
# (No parallel recomputation — display changes only)
###############################################################################

suppressPackageStartupMessages({
  library(mizer)
  library(therMizer)
  library(dplyr)
})

# source("ecosystem_assessment_v2.R")
source("ecosystem_assessment_v3.R")


cat("==============================================================\n")
cat("REGENERATE HEATMAPS (display changes only)\n")
cat("==============================================================\n\n")

start_time <- Sys.time()

# ── Load patched data ───────────────────────────────────────────────────
cat("Loading patched RDS files...\n")
fishing_patched <- readRDS(file.path(OUTPUT_DIR_LARGE, "fishing_metrics_raw.rds"))
climate_patched <- readRDS(file.path(OUTPUT_DIR_LARGE, "climate_only_metrics_raw.rds"))
b0_patched      <- readRDS(file.path(OUTPUT_DIR_LARGE, "b0_reference.rds"))

n_sims <- length(unique(fishing_patched$sim_id))
cat(sprintf("  Loaded: %d sims, %d fishing rows, %d climate rows, %d B0 rows\n",
            n_sims, nrow(fishing_patched), nrow(climate_patched), nrow(b0_patched)))

# ── Derive empirical thresholds ─────────────────────────────────────────
cat("Deriving empirical thresholds...\n")
empirical_thresholds <- derive_empirical_thresholds(b0_patched)

# ── Compute paired comparisons ──────────────────────────────────────────
cat("Computing paired comparisons...\n")
paired_data <- compute_paired_comparisons(
  fishing_patched, climate_patched, b0_patched, empirical_thresholds)

# ── Summarize by category ───────────────────────────────────────────────
cat("Summarizing results by metric category...\n")
biomass_summaries <- summarize_biomass_ratios(paired_data)
structural_summaries <- summarize_structural_deviations(paired_data, empirical_thresholds)
exploitation_summary <- summarize_exploitation(paired_data)

# Pre-whaling validation
validation <- validate_pre_exploitation(biomass_summaries, structural_summaries)

# ── Generate visualizations ─────────────────────────────────────────────
cat("\n=============================================================\n")
cat("GENERATING VISUALIZATIONS\n")
cat("=============================================================\n\n")

plots <- list()

cat("  Category A: Biomass ratio heatmaps...\n")
plots$bio_exploit_ratio <- plot_biomass_ratio_heatmap(
  biomass_summaries$exploit, "Exploitation Impact: Biomass Metrics",
  "Median ratio: Fishing / Climate-only (paired)",
  OUTPUT_DIR, "heatmap_biomass_exploitation_ratio")
plots$bio_absolute_ratio <- plot_biomass_ratio_heatmap(
  biomass_summaries$absolute, "Absolute Health: Biomass Metrics",
  sprintf("Median ratio: Fishing / B0 (%d-%d)", B0_PERIOD[1], B0_PERIOD[2]),
  OUTPUT_DIR, "heatmap_biomass_absolute_ratio")
plots$bio_climate_ratio <- plot_biomass_ratio_heatmap(
  biomass_summaries$climate, "Climate Impact: Biomass Metrics",
  sprintf("Median ratio: Climate-only / B0 (%d-%d)", B0_PERIOD[1], B0_PERIOD[2]),
  OUTPUT_DIR, "heatmap_biomass_climate_ratio")

cat("  Category A: Biomass proportion heatmaps (metric-specific thresholds)...\n")
plots$bio_absolute_specific <- plot_biomass_metric_specific_heatmap(
  biomass_summaries$absolute,
  "Absolute Health: P(B/B0 < Metric-Specific Threshold)",
  OUTPUT_DIR, "heatmap_biomass_absolute_metric_specific")
plots$bio_exploit_specific <- plot_biomass_metric_specific_heatmap(
  biomass_summaries$exploit,
  "Exploitation Impact: P(Fishing/Climate-only < Threshold)",
  OUTPUT_DIR, "heatmap_biomass_exploit_metric_specific")
plots$bio_climate_specific <- plot_biomass_metric_specific_heatmap(
  biomass_summaries$climate,
  "Climate Impact: P(Climate-only/B0 < Threshold)",
  OUTPUT_DIR, "heatmap_biomass_climate_metric_specific")

cat("  Category B: Structural deviation heatmaps...\n")
plots$str_outside_90 <- plot_structural_deviation_heatmap(
  structural_summaries$absolute, "prop_outside_b0_90",
  "5th\u201395th percentile (empirical B0 envelope, 1841\u20131860)",
  "Structural Metrics: Outside B0 Natural Range",
  OUTPUT_DIR, "heatmap_structural_outside_b0_90")
plots$str_outside_98 <- plot_structural_deviation_heatmap(
  structural_summaries$absolute, "prop_outside_b0_98",
  "1st\u201399th percentile (substantially altered)",
  "Structural Metrics: Substantially Altered",
  OUTPUT_DIR, "heatmap_structural_outside_b0_98")
plots$str_zscore_abs <- plot_structural_zscore_heatmap(
  structural_summaries$absolute,
  "Structural Metrics: Standardized Departure from B0",
  OUTPUT_DIR, "heatmap_structural_zscore_absolute")
plots$str_zscore_exploit <- plot_structural_zscore_heatmap(
  structural_summaries$exploit,
  "Structural Metrics: Exploitation-Driven Departure",
  OUTPUT_DIR, "heatmap_structural_zscore_exploitation")

cat("  Combined heatmaps (biomass + structural, 90% CI)...\n")
plots$combined_absolute <- plot_combined_heatmap(
  biomass_summaries$absolute, structural_summaries$absolute,
  "Ecosystem Assessment: Prydz Bay (Absolute)",
  OUTPUT_DIR, "heatmap_combined_absolute")
plots$combined_exploit <- plot_combined_heatmap(
  biomass_summaries$exploit, structural_summaries$exploit,
  "Ecosystem Assessment: Exploitation Impact",
  OUTPUT_DIR, "heatmap_combined_exploitation")
plots$combined_climate <- plot_combined_heatmap(
  biomass_summaries$climate, structural_summaries$climate,
  "Ecosystem Assessment: Climate Impact",
  OUTPUT_DIR, "heatmap_combined_climate")

cat("  Combined heatmaps (50% CI / IQR)...\n")
plots$combined_absolute_50ci <- plot_combined_heatmap(
  biomass_summaries$absolute, structural_summaries$absolute,
  "Ecosystem Assessment: Prydz Bay (Absolute, IQR)",
  OUTPUT_DIR, "heatmap_combined_absolute_50ci",
  envelope_col = "prop_outside_b0_50", null_expected = STRUCTURAL_NULL_50CI)
plots$combined_exploit_50ci <- plot_combined_heatmap(
  biomass_summaries$exploit, structural_summaries$exploit,
  "Ecosystem Assessment: Exploitation Impact (IQR)",
  OUTPUT_DIR, "heatmap_combined_exploitation_50ci",
  envelope_col = "prop_outside_b0_50", null_expected = STRUCTURAL_NULL_50CI)
plots$combined_climate_50ci <- plot_combined_heatmap(
  biomass_summaries$climate, structural_summaries$climate,
  "Ecosystem Assessment: Climate Impact (IQR)",
  OUTPUT_DIR, "heatmap_combined_climate_50ci",
  envelope_col = "prop_outside_b0_50", null_expected = STRUCTURAL_NULL_50CI)

cat("  Category C: Exploitation rate heatmap...\n")
plots$exploitation_F <- plot_exploitation_heatmap(exploitation_summary, OUTPUT_DIR)

# ── Save summaries ──────────────────────────────────────────────────────
cat("\nSaving summary CSVs...\n")
for (comp in c("exploit", "absolute", "climate")) {
  write.csv(biomass_summaries[[comp]],
            file.path(OUTPUT_DIR, sprintf("summary_biomass_%s.csv", comp)),
            row.names = FALSE)
  write.csv(structural_summaries[[comp]],
            file.path(OUTPUT_DIR, sprintf("summary_structural_%s.csv", comp)),
            row.names = FALSE)
}
write.csv(exploitation_summary,
          file.path(OUTPUT_DIR, "summary_exploitation_rates.csv"),
          row.names = FALSE)
if (!is.null(validation)) {
  if (!is.null(validation$biomass))
    write.csv(validation$biomass,
              file.path(OUTPUT_DIR, "validation_biomass.csv"), row.names = FALSE)
  if (!is.null(validation$structural))
    write.csv(validation$structural,
              file.path(OUTPUT_DIR, "validation_structural.csv"), row.names = FALSE)
}

# ── Done ─────────────────────────────────────────────────────────────────
total_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
cat(sprintf("\n==============================================================\n"))
cat(sprintf("HEATMAP REGENERATION COMPLETE\n"))
cat(sprintf("  Total time: %.1f minutes\n", total_time))
cat(sprintf("  Output: %d plots, %d summary CSVs\n", length(plots), 3 * 2 + 1))
cat(sprintf("  Directory: %s\n", OUTPUT_DIR))
cat(sprintf("==============================================================\n"))
