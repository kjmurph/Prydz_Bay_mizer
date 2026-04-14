###############################################################################
# TEST NEW METRICS — 5-Simulation Quick Validation
#
# Purpose:
#   Runs 5 paired simulations through the updated ecosystem assessment pipeline
#   to catch bugs in the new/modified metrics before a full ensemble rerun:
#
#   Modified:
#     - spectrum_slope:           now filters species with w_inf >= krill (4.17g)
#     - spectrum_intercept:       same krill filtering
#     - production_biomass_ratio: now filters species with w_inf >= krill
#
#   New:
#     - spectrum_mle_exponent:    MLE bounded power law (Edwards et al. 2017)
#     - fish_pb_ratio:            P/B for fish groups only
#     - marine_mammal_pb_ratio:   P/B for marine mammals only
#
# Usage:
#   Rscript test_new_metrics_5sims.R
###############################################################################

cat("=============================================================\n")
cat("TEST RUN: New Metric Validation (5 simulations)\n")
cat("=============================================================\n\n")

suppressPackageStartupMessages({
  library(therMizer)
  library(mizer)
  library(dplyr)
})

# Source the main assessment script (loads all functions + constants)
cat("Loading functions from ecosystem_assessment_v2.R...\n")
source("ecosystem_assessment_v2.R")

# ── Verify constants ────────────────────────────────────────────────────────
cat("\n--- Constant Checks ---\n")
cat(sprintf("  KRILL_W_INF = %.4f g\n", KRILL_W_INF))
cat(sprintf("  New metrics in METRIC_CATEGORY: %s\n",
            paste(intersect(c("spectrum_mle_exponent", "fish_pb_ratio",
                              "marine_mammal_pb_ratio"),
                            names(METRIC_CATEGORY)), collapse = ", ")))
cat(sprintf("  STRUCTURAL_METRICS (%d): %s\n",
            length(STRUCTURAL_METRICS), paste(STRUCTURAL_METRICS, collapse = ", ")))
cat(sprintf("  STRUCTURAL_METRICS_PLOT (%d): %s\n",
            length(STRUCTURAL_METRICS_PLOT), paste(STRUCTURAL_METRICS_PLOT, collapse = ", ")))

new_in_display <- intersect(c("spectrum_mle_exponent", "fish_pb_ratio",
                               "marine_mammal_pb_ratio"),
                             names(METRIC_DISPLAY))
cat(sprintf("  New metrics in METRIC_DISPLAY: %s\n",
            paste(sapply(new_in_display, get_display_name), collapse = ", ")))

# ── Load ensembles ──────────────────────────────────────────────────────────
cat("\n--- Loading Ensembles ---\n")
stopifnot("Fishing ensemble not found" = file.exists(ENSEMBLE_PATHS$fishing))
stopifnot("Climate-only ensemble not found" = file.exists(ENSEMBLE_PATHS$climate_only))

mc_fish <- readRDS(ENSEMBLE_PATHS$fishing)
mc_clim <- readRDS(ENSEMBLE_PATHS$climate_only)
fish_sims <- if ("simulations" %in% names(mc_fish)) mc_fish$simulations else mc_fish
clim_sims <- if ("simulations" %in% names(mc_clim)) mc_clim$simulations else mc_clim
cat(sprintf("  Fishing sims available: %d\n", length(fish_sims)))
cat(sprintf("  Climate sims available: %d\n", length(clim_sims)))

N_TEST <- 5
cat(sprintf("  Using first %d simulations for testing\n", N_TEST))

# ── Test get_species_above_size helper ──────────────────────────────────────
cat("\n--- Testing get_species_above_size() ---\n")
test_params <- fish_sims[[1]]@params
krill_filtered <- get_species_above_size(test_params, KRILL_W_INF)
all_species <- as.character(test_params@species_params$species)
excluded <- setdiff(all_species, krill_filtered)
cat(sprintf("  All species (%d): %s\n", length(all_species), paste(all_species, collapse = ", ")))
cat(sprintf("  Krill+ species (%d): %s\n", length(krill_filtered), paste(krill_filtered, collapse = ", ")))
cat(sprintf("  Excluded (< krill w_inf): %s\n", paste(excluded, collapse = ", ")))
stopifnot("Krill filtering should exclude some species" = length(excluded) > 0)
stopifnot("Antarctic krill should be included" = "antarctic krill" %in% krill_filtered)
stopifnot("mesozooplankton should be excluded" = "mesozooplankton" %in% excluded)
cat("  [PASS] Species filtering works correctly\n")

# ── Test individual metric functions on simulation 1 ────────────────────────
cat("\n--- Testing Individual Metric Functions (Sim 1) ---\n")
sim1 <- fish_sims[[1]]
times <- as.numeric(dimnames(sim1@n)$time)
# Use a modern decade for testing
test_range <- which(times >= 2001 & times <= 2010)
if (length(test_range) == 0) {
  # Fallback: use last 10 time steps
  test_range <- tail(seq_along(times), 10)
  cat(sprintf("  Using fallback time range: %d steps\n", length(test_range)))
} else {
  cat(sprintf("  Test time range: 2001-2010 (%d steps)\n", length(test_range)))
}

# 1. Spectrum slope/intercept (modified: krill filtering)
cat("\n  1. calculate_spectrum_slope_intercept (krill-filtered)...\n")
spec <- calculate_spectrum_slope_intercept(sim1, test_range)
cat(sprintf("     slope = %.4f, intercept = %.4f\n", spec["slope"], spec["intercept"]))
stopifnot("Slope should be finite" = is.finite(spec["slope"]))
stopifnot("Intercept should be finite" = is.finite(spec["intercept"]))
stopifnot("Slope should be negative" = spec["slope"] < 0)
cat("     [PASS]\n")

# 2. MLE bounded power law exponent (new)
cat("\n  2. calculate_spectrum_mle_exponent (MLE PLB)...\n")
mle_exp <- calculate_spectrum_mle_exponent(sim1, test_range)
cat(sprintf("     MLE exponent b = %.4f\n", mle_exp))
stopifnot("MLE exponent should be finite" = is.finite(mle_exp))
stopifnot("MLE exponent should be negative" = mle_exp < 0)
cat("     [PASS]\n")

# 3. Production:Biomass ratio (modified: krill filtering)
cat("\n  3. calculate_production_biomass_ratio (krill-filtered community)...\n")
pb_community <- calculate_production_biomass_ratio(sim1, test_range)
cat(sprintf("     Community P/B (krill+) = %.6f yr^-1\n", pb_community))
stopifnot("Community P/B should be finite" = is.finite(pb_community))
stopifnot("Community P/B should be positive" = pb_community > 0)
cat("     [PASS]\n")

# 4. Fish P/B ratio (new)
cat("\n  4. calculate_production_biomass_ratio (fish only)...\n")
pb_fish <- calculate_production_biomass_ratio(sim1, test_range,
                                              species_group = SPECIES_GROUPS$fish)
cat(sprintf("     Fish P/B = %.6f yr^-1\n", pb_fish))
stopifnot("Fish P/B should be finite" = is.finite(pb_fish))
stopifnot("Fish P/B should be positive" = pb_fish > 0)
cat("     [PASS]\n")

# 5. Marine mammal P/B ratio (new)
cat("\n  5. calculate_production_biomass_ratio (marine mammals)...\n")
pb_mm <- calculate_production_biomass_ratio(sim1, test_range,
                                            species_group = SPECIES_GROUPS$marine_mammals)
cat(sprintf("     Marine mammal P/B = %.6f yr^-1\n", pb_mm))
stopifnot("Marine mammal P/B should be finite" = is.finite(pb_mm))
stopifnot("Marine mammal P/B should be positive" = pb_mm > 0)
cat("     [PASS]\n")

# Sanity: fish should have higher P/B than marine mammals
cat(sprintf("\n  Sanity check: Fish P/B (%.4f) vs Marine mammal P/B (%.4f)\n",
            pb_fish, pb_mm))
if (pb_fish > pb_mm) {
  cat("  [PASS] Fish P/B > Marine mammal P/B (expected for smaller, faster-growing organisms)\n")
} else {
  cat("  [WARNING] Fish P/B <= Marine mammal P/B — unexpected but not necessarily wrong\n")
}

# ── Test full metric extraction pipeline (1 sim) ───────────────────────────
cat("\n--- Testing extract_simulation_metrics (Sim 1) ---\n")
metrics_df <- extract_simulation_metrics(sim1)
cat(sprintf("  Rows: %d (decades), Columns: %d\n", nrow(metrics_df), ncol(metrics_df)))

expected_new <- c("spectrum_mle_exponent", "fish_pb_ratio", "marine_mammal_pb_ratio")
missing_cols <- setdiff(expected_new, names(metrics_df))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing columns in extract_simulation_metrics output: %s",
               paste(missing_cols, collapse = ", ")))
}
cat(sprintf("  New metric columns present: %s\n", paste(expected_new, collapse = ", ")))

# Check values across decades
for (col in expected_new) {
  vals <- metrics_df[[col]]
  n_na <- sum(is.na(vals))
  cat(sprintf("  %-30s: range = [%.4f, %.4f], NAs = %d/%d\n",
              col, min(vals, na.rm = TRUE), max(vals, na.rm = TRUE),
              n_na, length(vals)))
}
cat("  [PASS] extract_simulation_metrics\n")

# ── Test B0 extraction ──────────────────────────────────────────────────────
cat("\n--- Testing extract_b0_metrics (Sim 1, climate-only) ---\n")
b0_vec <- extract_b0_metrics(clim_sims[[1]])
cat(sprintf("  B0 vector length: %d\n", length(b0_vec)))
missing_b0 <- setdiff(expected_new, names(b0_vec))
if (length(missing_b0) > 0) {
  stop(sprintf("Missing B0 metrics: %s", paste(missing_b0, collapse = ", ")))
}
for (col in expected_new) {
  cat(sprintf("  %-30s: %.6f\n", col, b0_vec[col]))
}
cat("  [PASS] extract_b0_metrics\n")

# ── Run full pipeline on 5 sims ────────────────────────────────────────────
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

# Check raw data dimensions
cat("--- Raw Data ---\n")
cat(sprintf("  fishing_raw: %d rows, %d cols\n",
            nrow(results$fishing_raw), ncol(results$fishing_raw)))
cat(sprintf("  climate_raw: %d rows, %d cols\n",
            nrow(results$climate_raw), ncol(results$climate_raw)))
cat(sprintf("  b0_reference: %d rows, %d cols\n",
            nrow(results$b0_reference), ncol(results$b0_reference)))

# Check new metrics in raw data
for (col in expected_new) {
  in_fish <- col %in% names(results$fishing_raw)
  in_clim <- col %in% names(results$climate_raw)
  in_b0   <- col %in% names(results$b0_reference)
  status <- ifelse(in_fish & in_clim & in_b0, "PASS", "FAIL")
  cat(sprintf("  [%s] %s — fishing: %s, climate: %s, b0: %s\n",
              status, col, in_fish, in_clim, in_b0))
}

# Check empirical thresholds include new metrics
cat("\n--- Empirical Thresholds ---\n")
for (col in expected_new) {
  th <- results$empirical_thresholds[[col]]
  if (!is.null(th) && !is.na(th$median)) {
    cat(sprintf("  [PASS] %-30s: median = %.4f, range = [%.4f, %.4f]\n",
                col, th$median, th$q05, th$q95))
  } else {
    cat(sprintf("  [WARN] %-30s: threshold not derived (NULL or NA)\n", col))
  }
}

# Check structural summaries include new metrics
cat("\n--- Structural Summaries ---\n")
for (comp in c("absolute", "exploit", "climate")) {
  ss <- results$structural_summaries[[comp]]
  new_in_summary <- intersect(expected_new, unique(ss$metric_name))
  cat(sprintf("  %s: %d metrics total, new metrics present: %s\n",
              comp, length(unique(ss$metric_name)),
              paste(new_in_summary, collapse = ", ")))
}

# Check paired_data has flags for new metrics
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

# Check plots generated
cat("\n--- Plot Objects ---\n")
n_plots <- sum(sapply(results$plots, function(p) !is.null(p)))
cat(sprintf("  Total plots generated: %d\n", n_plots))

# Print modern decade values for new metrics
cat("\n--- New Metric Values (2001-2010, median across sims) ---\n")
for (comp in c("absolute")) {
  ss <- results$structural_summaries[[comp]]
  modern <- ss %>% filter(decade == "2001-2010", metric_name %in% expected_new)
  if (nrow(modern) > 0) {
    for (i in seq_len(nrow(modern))) {
      row <- modern[i, ]
      cat(sprintf("  %-30s: value = %.4f, diff from B0 = %+.4f, z = %+.2f\n",
                  row$metric_name, row$value_median, row$diff_median,
                  ifelse(is.na(row$z_median), NA, row$z_median)))
    }
  }
}

# ── Summary ─────────────────────────────────────────────────────────────────
cat("\n=============================================================\n")
cat("TEST SUMMARY\n")
cat("=============================================================\n")
cat("  Individual function tests:  PASSED\n")
cat("  Full pipeline (5 sims):     COMPLETED\n")
cat(sprintf("  New metrics extracted:      %s\n", paste(expected_new, collapse = ", ")))
cat(sprintf("  Modified metrics verified:  spectrum_slope, spectrum_intercept, production_biomass_ratio\n"))
cat(sprintf("  Output plots:              %d generated\n", n_plots))
cat(sprintf("  Runtime:                   %.1f minutes\n",
            difftime(test_end, test_start, units = "mins")))
cat("\n  Ready for full ensemble rerun.\n")
cat("=============================================================\n")
