###############################################################################
# TEST NEW METRICS — 500-Simulation Run
#
# Purpose:
#   Runs 500 paired simulations through the updated ecosystem assessment pipeline
#   to produce publication-quality plots with robust empirical thresholds.
#   Skips individual function tests (already validated in 20-sim run).
#
# Usage:
#   Rscript test_new_metrics_500sims.R
###############################################################################

cat("=============================================================\n")
cat("PRODUCTION RUN: 500 simulations\n")
cat("=============================================================\n\n")

suppressPackageStartupMessages({
  library(therMizer)
  library(mizer)
  library(dplyr)
})

# Source the main assessment script (loads all functions + constants)
cat("Loading functions from ecosystem_assessment_v2.R...\n")
source("ecosystem_assessment_v2.R")

N_TEST <- 500

cat(sprintf("  KRILL_W_INF = %.4f g\n", KRILL_W_INF))
cat(sprintf("  STRUCTURAL_METRICS (%d): %s\n",
            length(STRUCTURAL_METRICS), paste(STRUCTURAL_METRICS, collapse = ", ")))
cat(sprintf("  STRUCTURAL_METRICS_PLOT (%d): %s\n",
            length(STRUCTURAL_METRICS_PLOT), paste(STRUCTURAL_METRICS_PLOT, collapse = ", ")))

# ── Run full pipeline ───────────────────────────────────────────────────────
cat("\n=============================================================\n")
cat(sprintf("RUNNING FULL PIPELINE (%d simulations)\n", N_TEST))
cat("=============================================================\n\n")

test_start <- Sys.time()
results <- main_ecosystem_assessment(max_sims = N_TEST)
test_end <- Sys.time()
cat(sprintf("\nPipeline completed in %.1f minutes\n",
            difftime(test_end, test_start, units = "mins")))

# ── Validate outputs ────────────────────────────────────────────────────────
cat("\n=============================================================\n")
cat("POST-PIPELINE VALIDATION\n")
cat("=============================================================\n\n")

expected_new <- c("spectrum_mle_exponent", "fish_pb_ratio", "marine_mammal_pb_ratio")

cat("--- Raw Data ---\n")
cat(sprintf("  fishing_raw: %d rows, %d cols\n",
            nrow(results$fishing_raw), ncol(results$fishing_raw)))
cat(sprintf("  climate_raw: %d rows, %d cols\n",
            nrow(results$climate_raw), ncol(results$climate_raw)))
cat(sprintf("  b0_reference: %d rows, %d cols\n",
            nrow(results$b0_reference), ncol(results$b0_reference)))

# Check all empirical thresholds
cat("\n--- All Empirical Thresholds ---\n")
n_thresholds_derived <- 0
all_thresh_names <- names(results$empirical_thresholds)
for (tname in all_thresh_names) {
  th <- results$empirical_thresholds[[tname]]
  if (!is.null(th) && !is.na(th$median)) {
    cat(sprintf("  %-30s: median = %.4f, 90%% CI = [%.4f, %.4f]\n",
                tname, th$median, th$q05, th$q95))
    if (tname %in% expected_new) n_thresholds_derived <- n_thresholds_derived + 1
  } else {
    cat(sprintf("  %-30s: NOT derived\n", tname))
  }
}
cat(sprintf("\n  New metric thresholds: %d/%d derived\n",
            n_thresholds_derived, length(expected_new)))

# Check structural summaries
cat("\n--- Structural Summaries ---\n")
for (comp in c("absolute", "exploit", "climate")) {
  ss <- results$structural_summaries[[comp]]
  new_in_summary <- intersect(expected_new, unique(ss$metric_name))
  cat(sprintf("  %s: %d metrics total, new metrics: %s\n",
              comp, length(unique(ss$metric_name)),
              paste(new_in_summary, collapse = ", ")))
}

# Envelope flags
cat("\n--- Paired Data Envelope Flags ---\n")
pd <- results$paired_data
for (col in expected_new) {
  flag_col <- paste0(col, "_outside_b0_90")
  if (flag_col %in% names(pd)) {
    pct <- mean(pd[[flag_col]], na.rm = TRUE) * 100
    cat(sprintf("  [PASS] %s: %.1f%% outside B0 90%% CI\n", col, pct))
  } else {
    cat(sprintf("  [WARN] %s: envelope flag not found\n", col))
  }
}

# Plots
cat("\n--- Plot Objects ---\n")
n_plots <- sum(sapply(results$plots, function(p) !is.null(p)))
plot_names <- names(results$plots)[sapply(results$plots, function(p) !is.null(p))]
cat(sprintf("  Total plots generated: %d\n", n_plots))
for (pn in plot_names) cat(sprintf("    - %s\n", pn))

# Modern decade values
cat("\n--- New Metric Values (2001-2010, median across sims) ---\n")
ss <- results$structural_summaries$absolute
modern <- ss %>% filter(decade == "2001-2010", metric_name %in% expected_new)
if (nrow(modern) > 0) {
  for (i in seq_len(nrow(modern))) {
    row <- modern[i, ]
    cat(sprintf("  %-30s: value = %.4f, diff = %+.4f, z = %+.2f\n",
                row$metric_name, row$value_median, row$diff_median,
                ifelse(is.na(row$z_median), NA, row$z_median)))
  }
}

# Structural heatmap completeness
cat("\n--- Structural Deviation Heatmap Data ---\n")
for (comp in c("absolute", "exploit", "climate")) {
  ss <- results$structural_summaries[[comp]]
  n_with_z <- sum(!is.na(ss$z_median))
  cat(sprintf("  %s: %d/%d metric-decades have z-scores\n", comp, n_with_z, nrow(ss)))
}

# Summary
cat("\n=============================================================\n")
cat("SUMMARY\n")
cat("=============================================================\n")
cat(sprintf("  Simulations:            %d\n", N_TEST))
cat(sprintf("  New metric thresholds:  %d/%d\n", n_thresholds_derived, length(expected_new)))
cat(sprintf("  Plots generated:        %d\n", n_plots))
cat(sprintf("  Runtime:                %.1f minutes\n",
            difftime(test_end, test_start, units = "mins")))
cat("=============================================================\n")
