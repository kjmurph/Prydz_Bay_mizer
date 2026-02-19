#!/usr/bin/env Rscript
# ==============================================================================
# REPLOT HEATMAPS FROM EXISTING OUTPUT FILES
# ==============================================================================
# This script regenerates all heatmaps and summaries from existing RDS files
# without re-running the simulation processing. Useful for:
# - Testing plotting changes
# - Regenerating outputs with updated aesthetics
# - Creating plots while main analysis is still running
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(viridis)
  library(patchwork)
})

cat("=============================================================\n")
cat("REGENERATING PLOTS FROM EXISTING OUTPUTS\n")
cat("=============================================================\n\n")

# Source the main script to get all constants and functions
cat("Loading functions from ecosystem_assessment_v2.R...\n")
source("ecosystem_assessment_v2.R")

# Define data paths
data_dir <- "Output_large_files/ecosystem_assessment"
output_dir <- "ecosystem_assessment_outputs"

# Check that required files exist
required_files <- c(
  "fishing_metrics_raw.rds",
  "climate_only_metrics_raw.rds",
  "b0_reference.rds",
  "paired_data.rds",
  "empirical_thresholds.rds"
)

missing_files <- required_files[!file.exists(file.path(data_dir, required_files))]
if (length(missing_files) > 0) {
  stop("Missing required files: ", paste(missing_files, collapse = ", "))
}

cat("\nLoading existing data files...\n")
fishing_data <- readRDS(file.path(data_dir, "fishing_metrics_raw.rds"))
climate_data <- readRDS(file.path(data_dir, "climate_only_metrics_raw.rds"))
b0_ref <- readRDS(file.path(data_dir, "b0_reference.rds"))
paired_data <- readRDS(file.path(data_dir, "paired_data.rds"))
thresholds <- readRDS(file.path(data_dir, "empirical_thresholds.rds"))

n_sims <- nrow(paired_data) / length(unique(paired_data$decade))
n_records <- nrow(paired_data)
cat(sprintf("  Paired data loaded: %d simulations, %d paired records\n", round(n_sims), n_records))
cat(sprintf("  B0 reference: %d simulations\n", length(unique(b0_ref$sim_id))))

cat("\n=============================================================\n")
cat("COMPUTING SUMMARIES FROM PAIRED DATA\n")
cat("=============================================================\n\n")

# paired_data.rds already contains the fully merged paired comparisons
# (all 2111 sims x 17 decades with ratios and envelope flags computed).
# We use it directly rather than recomputing from raw files, which may
# only contain the final checkpoint batch.
paired_comparisons <- paired_data
cat(sprintf("  Using pre-computed paired data: %d records (%d sims)\n",
            nrow(paired_comparisons), round(n_sims)))

cat("\nSummarizing results by metric category...\n")
cat("  Summarizing biomass ratios...\n")
biomass_summaries <- summarize_biomass_ratios(paired_comparisons)
biomass_exploit <- biomass_summaries$exploit
biomass_absolute <- biomass_summaries$absolute
biomass_climate <- biomass_summaries$climate
cat(sprintf("    exploit: %d records\n", nrow(biomass_exploit)))
cat(sprintf("    absolute: %d records\n", nrow(biomass_absolute)))
cat(sprintf("    climate: %d records\n", nrow(biomass_climate)))

cat("  Summarizing structural deviations...\n")
structural_summaries <- summarize_structural_deviations(paired_comparisons, thresholds)
structural_exploit <- structural_summaries$exploit
structural_absolute <- structural_summaries$absolute
structural_climate <- structural_summaries$climate
cat(sprintf("    exploit: %d records\n", nrow(structural_exploit)))
cat(sprintf("    absolute: %d records\n", nrow(structural_absolute)))
cat(sprintf("    climate: %d records\n", nrow(structural_climate)))

cat("  Summarizing exploitation rates...\n")
exploitation_summary <- summarize_exploitation(paired_comparisons)

cat("\n=============================================================\n")
cat("GENERATING VISUALIZATIONS\n")
cat("=============================================================\n\n")

plots <- list()

# Category A: Biomass ratio heatmaps
cat("  Category A: Biomass ratio heatmaps...\n")
plots$bio_exploit_ratio <- plot_biomass_ratio_heatmap(
  biomass_exploit, "Exploitation Impact: Biomass Metrics",
  "Median ratio: Fishing / Climate-only (paired)",
  output_dir, "heatmap_biomass_exploitation_ratio")
plots$bio_absolute_ratio <- plot_biomass_ratio_heatmap(
  biomass_absolute, "Absolute Health: Biomass Metrics",
  sprintf("Median ratio: Fishing / B0 (%d-%d)", B0_PERIOD[1], B0_PERIOD[2]),
  output_dir, "heatmap_biomass_absolute_ratio")
plots$bio_climate_ratio <- plot_biomass_ratio_heatmap(
  biomass_climate, "Climate Impact: Biomass Metrics",
  sprintf("Median ratio: Climate-only / B0 (%d-%d)", B0_PERIOD[1], B0_PERIOD[2]),
  output_dir, "heatmap_biomass_climate_ratio")

# Biomass proportion heatmaps
cat("  Category A: Biomass proportion heatmaps...\n")
plots$bio_prop_075 <- plot_biomass_proportion_heatmap(
  biomass_absolute, "prop_below_075", 0.75,
  "CCAMLR \u03B3\u2082 escapement; Constable et al. 2000",
  "Absolute Health: P(Biomass/B0 < 0.75)",
  output_dir, "heatmap_biomass_prop_below_075")
plots$bio_prop_040 <- plot_biomass_proportion_heatmap(
  biomass_absolute, "prop_below_040", 0.40,
  "BMSY proxy; Restrepo et al. 1998",
  "Absolute Health: P(Biomass/B0 < 0.40)",
  output_dir, "heatmap_biomass_prop_below_040")
plots$bio_prop_020 <- plot_biomass_proportion_heatmap(
  biomass_absolute, "prop_below_020", 0.20,
  "CCAMLR \u03B3\u2081 / MSST collapse; Constable et al. 2000",
  "Absolute Health: P(Biomass/B0 < 0.20)",
  output_dir, "heatmap_biomass_prop_below_020")
plots$whale_prop_054 <- plot_biomass_proportion_heatmap(
  biomass_absolute, "prop_below_054", 0.54,
  "IWC RMP protection level; IWC 1994, Punt & Donovan 2007",
  "Whale Populations: P(Biomass/B0 < 0.54)",
  output_dir, "heatmap_whale_prop_below_054", metrics_filter = WHALE_METRICS)
plots$whale_exploit_054 <- plot_biomass_proportion_heatmap(
  biomass_exploit, "prop_below_054", 0.54,
  "IWC RMP protection level; IWC 1994, Punt & Donovan 2007",
  "Exploitation Impact on Whales: P(Fishing/Climate-only < 0.54)",
  output_dir, "heatmap_whale_exploit_prop_below_054", metrics_filter = WHALE_METRICS)

# Category B: Structural deviation heatmaps
cat("  Category B: Structural deviation heatmaps...\n")
plots$str_outside_90 <- plot_structural_deviation_heatmap(
  structural_absolute, "prop_outside_b0_90",
  "5th\u201395th percentile (empirical B0 envelope, 1841\u20131860)",
  "Structural Metrics: Outside B0 Natural Range",
  output_dir, "heatmap_structural_outside_b0_90")
plots$str_outside_98 <- plot_structural_deviation_heatmap(
  structural_absolute, "prop_outside_b0_98",
  "1st\u201399th percentile (substantially altered)",
  "Structural Metrics: Substantially Altered",
  output_dir, "heatmap_structural_outside_b0_98")
plots$str_zscore_abs <- plot_structural_zscore_heatmap(
  structural_absolute,
  "Structural Metrics: Standardized Departure from B0",
  output_dir, "heatmap_structural_zscore_absolute")
plots$str_zscore_exploit <- plot_structural_zscore_heatmap(
  structural_exploit,
  "Structural Metrics: Exploitation-Driven Departure",
  output_dir, "heatmap_structural_zscore_exploitation")

# Combined heatmaps
cat("  Combined heatmaps (biomass + structural)...\n")
plots$combined_absolute <- plot_combined_heatmap(
  biomass_absolute, structural_absolute,
  "Ecosystem Assessment: Prydz Bay (Absolute)",
  output_dir, "heatmap_combined_absolute")
plots$combined_exploit <- plot_combined_heatmap(
  biomass_exploit, structural_exploit,
  "Ecosystem Assessment: Exploitation Impact",
  output_dir, "heatmap_combined_exploitation")
plots$combined_climate <- plot_combined_heatmap(
  biomass_climate, structural_climate,
  "Ecosystem Assessment: Climate Impact",
  output_dir, "heatmap_combined_climate")

# Category C: Exploitation heatmap
cat("  Category C: Exploitation rate heatmap...\n")
plots$exploitation_F <- plot_exploitation_heatmap(exploitation_summary, output_dir)

cat("\n=============================================================\n")
cat("SAVING SUMMARY CSV FILES\n")
cat("=============================================================\n\n")

# Save summary CSVs
write.csv(biomass_exploit, file.path(output_dir, "summary_biomass_exploit.csv"), row.names = FALSE)
write.csv(biomass_absolute, file.path(output_dir, "summary_biomass_absolute.csv"), row.names = FALSE)
write.csv(biomass_climate, file.path(output_dir, "summary_biomass_climate.csv"), row.names = FALSE)
write.csv(structural_exploit, file.path(output_dir, "summary_structural_exploit.csv"), row.names = FALSE)
write.csv(structural_absolute, file.path(output_dir, "summary_structural_absolute.csv"), row.names = FALSE)
write.csv(structural_climate, file.path(output_dir, "summary_structural_climate.csv"), row.names = FALSE)
write.csv(exploitation_summary, file.path(output_dir, "summary_exploitation_rates.csv"), row.names = FALSE)

cat("  Summary CSVs saved to ecosystem_assessment_outputs/\n")

cat("\n=============================================================\n")
cat("REPLOTTING COMPLETE\n")
cat("=============================================================\n\n")

cat(sprintf("Regenerated all outputs from %d simulations (%d records)\n", round(n_sims), n_records))
cat("All heatmaps and summaries updated in: ecosystem_assessment_outputs/\n\n")
