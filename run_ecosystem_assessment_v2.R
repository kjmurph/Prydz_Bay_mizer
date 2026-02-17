###############################################################################
# Run Prydz Bay Ecosystem Assessment (v2 - Paired Ensemble)
# 
# Two metric scoring approaches:
#   Category A (Biomass): depletion ratios with CCAMLR/IWC/BMSY thresholds
#   Category B (Structural): empirical deviation from B0 ensemble envelope
#   Category C (Exploitation): absolute fishing mortality rates
#
# Sources: ecosystem_assessment_v2.R
# Diagnostics: ecosystem_assessment_diagnostics_v2.R (run separately after)
###############################################################################

# =============================================================================
# CONFIGURATION
# =============================================================================

TEST_MODE <- FALSE
TEST_SIMS <- 10

# =============================================================================

source("ecosystem_assessment_v2.R")

cat("=============================================================\n")
if (TEST_MODE) {
  cat(sprintf("STARTING ASSESSMENT (TEST MODE: %d sims)\n", TEST_SIMS))
} else {
  cat("STARTING ASSESSMENT (FULL ANALYSIS)\n")
}
cat(sprintf("B0 Reference Period: %d-%d (ISIMIP climate norm)\n", B0_PERIOD[1], B0_PERIOD[2]))
cat("=============================================================\n\n")

cat("Scoring approaches:\n")
cat("  Category A — BIOMASS (ratio-based):\n")
cat("    0.75 B0  CCAMLR gamma_2 escapement (Constable et al. 2000)\n")
cat("    0.40 B0  BMSY proxy limit (Restrepo et al. 1998)\n")
cat("    0.20 B0  CCAMLR gamma_1 / MSST collapse (Constable et al. 2000)\n")
cat("    0.54 K   IWC RMP protection level (IWC 1994) [whales only]\n")
cat("  Category B — STRUCTURAL (empirical deviation):\n")
cat("    P(outside B0 5th-95th percentile)  Natural range envelope\n")
cat("    P(outside B0 1st-99th percentile)  Substantially altered\n")
cat("    z-score: (value - B0 median) / B0 SD\n")
cat("  Category C — EXPLOITATION: absolute F values\n\n")

# Validate files
cat("Checking ensemble files...\n")
stopifnot("Fishing ensemble not found" = file.exists(ENSEMBLE_PATHS$fishing))
cat("  Fishing ensemble found\n")
stopifnot("Climate-only ensemble not found" = file.exists(ENSEMBLE_PATHS$climate_only))
cat("  Climate-only ensemble found\n")

# Run
n_sims <- if (TEST_MODE) TEST_SIMS else NULL
results <- main_ecosystem_assessment(max_sims = n_sims)

# =============================================================================
# Summary Report
# =============================================================================

cat("\n=============================================================\n")
cat("SUMMARY: KEY FINDINGS\n")
cat("=============================================================\n\n")

# --- Empirical B0 thresholds ---
cat("Empirical B0 Reference Values (1841-1860, Climate-only):\n")
cat(sprintf("  Simulations: %d\n", nrow(results$b0_reference)))
cat("  BIOMASS:\n")
for (m in c("total_biomass", "baleen_biomass", "krill_biomass", "apex_biomass")) {
  th <- results$empirical_thresholds[[m]]
  if (!is.null(th) && !is.na(th$median))
    cat(sprintf("    %-25s: median = %.4f, IQR = [%.4f, %.4f]\n",
                m, th$median, th$q25, th$q75))
}
cat("  STRUCTURAL (natural range = Q05-Q95):\n")
for (m in c("spectrum_slope", "spectrum_intercept", "mean_tl", "htl_indicator",
            "large_fish_indicator", "predator_prey_ratio")) {
  th <- results$empirical_thresholds[[m]]
  if (!is.null(th) && !is.na(th$median))
    cat(sprintf("    %-25s: median = %.4f, range = [%.4f, %.4f]\n",
                m, th$median, th$q05, th$q95))
}

# --- Pre-whaling validation ---
cat("\nPre-exploitation Validation:\n")
if (!is.null(results$validation)) {
  if (!is.null(results$validation$biomass)) {
    n_pass <- sum(results$validation$biomass$pass, na.rm = TRUE)
    n_total <- nrow(results$validation$biomass)
    cat(sprintf("  Biomass: %d/%d passed (ratio within 5%% of 1.0)\n", n_pass, n_total))
  }
  if (!is.null(results$validation$structural)) {
    n_pass <- sum(results$validation$structural$pass, na.rm = TRUE)
    n_total <- nrow(results$validation$structural)
    cat(sprintf("  Structural: %d/%d passed (z-score < 0.2)\n", n_pass, n_total))
  }
}

# --- Helper: print biomass summary ---
print_biomass_summary <- function(summary_df, label, decade_str) {
  period_data <- summary_df %>% filter(decade == decade_str)
  if (nrow(period_data) == 0) return()
  cat(sprintf("  %s:\n", label))
  for (i in seq_len(nrow(period_data))) {
    row <- period_data[i, ]
    display <- get_display_name(row$metric_name)
    if (row$metric_name %in% WHALE_METRICS) {
      thresh <- sprintf("P(<0.75)=%.0f%%, P(<0.54 IWC)=%.0f%%, P(<0.40)=%.0f%%",
                        row$prop_below_075 * 100, row$prop_below_054 * 100,
                        row$prop_below_040 * 100)
    } else {
      thresh <- sprintf("P(<0.75)=%.0f%%, P(<0.40)=%.0f%%",
                        row$prop_below_075 * 100, row$prop_below_040 * 100)
    }
    cat(sprintf("    %-30s: ratio = %.3f [%.3f-%.3f], %s\n",
                display, row$ratio_median, row$ratio_q25, row$ratio_q75, thresh))
  }
}

# --- Helper: print structural summary ---
print_structural_summary <- function(summary_df, label, decade_str) {
  period_data <- summary_df %>% filter(decade == decade_str)
  if (nrow(period_data) == 0) return()
  cat(sprintf("  %s:\n", label))
  for (i in seq_len(nrow(period_data))) {
    row <- period_data[i, ]
    display <- get_display_name(row$metric_name)
    z_str <- if (!is.na(row$z_median)) sprintf("z = %+.2f", row$z_median) else "z = NA"
    env_str <- if (!is.na(row$prop_outside_b0_90)) {
      sprintf("P(outside B0 5-95th) = %.0f%%", row$prop_outside_b0_90 * 100)
    } else "P(outside) = NA"
    cat(sprintf("    %-30s: diff = %+.4f, %s, %s\n",
                display, row$diff_median, z_str, env_str))
  }
}

# --- Modern period ---
cat("\n--- Modern Period (2001-2010) ---\n\n")
cat("Category A (Biomass - ratio scoring):\n")
print_biomass_summary(results$biomass_summaries$absolute,
                      "Absolute Health (Fishing / B0)", "2001-2010")
cat("\n")
print_biomass_summary(results$biomass_summaries$exploit,
                      "Exploitation Impact (Fishing / Climate-only)", "2001-2010")

cat("\nCategory B (Structural - empirical deviation):\n")
print_structural_summary(results$structural_summaries$absolute,
                         "Absolute Departure from B0", "2001-2010")
cat("\n")
print_structural_summary(results$structural_summaries$exploit,
                         "Exploitation-Driven Departure", "2001-2010")

# --- Peak whaling ---
cat("\n\n--- Peak Whaling Period (1931-1940) ---\n\n")
cat("Category A (Biomass):\n")
print_biomass_summary(results$biomass_summaries$absolute,
                      "Absolute Health (Fishing / B0)", "1931-1940")
cat("\nCategory B (Structural):\n")
print_structural_summary(results$structural_summaries$absolute,
                         "Absolute Departure from B0", "1931-1940")

# --- Exploitation rates ---
cat("\n\n--- Exploitation Rates (Category C) ---\n\n")
exploit_F <- results$exploitation_summary
for (dec in c("1931-1940", "1961-1970", "2001-2010")) {
  period_data <- exploit_F %>% filter(decade == dec)
  if (nrow(period_data) == 0) next
  cat(sprintf("  %s:\n", dec))
  for (i in seq_len(nrow(period_data))) {
    row <- period_data[i, ]
    cat(sprintf("    %-30s: median F = %.4f [%.4f-%.4f]\n",
                row$metric_name, row$value_median, row$value_q25, row$value_q75))
  }
}

cat("\n=============================================================\n")
cat("See ecosystem_assessment_outputs/ for all visualizations and data\n")
cat("Run ecosystem_assessment_diagnostics_v2.R for detailed time series\n")
cat("=============================================================\n")
