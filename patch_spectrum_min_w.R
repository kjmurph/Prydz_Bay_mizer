###############################################################################
# Patch cached ecosystem assessment RDS files after SPECTRUM_MIN_W change
#
# Purpose:
#   Recomputes only the 8 metrics that depend on SPECTRUM_MIN_W (the lower
#   weight cutoff for community size-based calculations):
#     - spectrum_slope, spectrum_intercept   (OLS, old method; data integrity)
#     - spectrum_mle_exponent                (PLBbin; data integrity)
#     - spectrum_lcd_exponent                (LCD; data integrity)
#     - spectrum_lbnbiom_slope               (LBNbiom / NBSS Slope — displayed)
#     - production_biomass_ratio             (community P/B — displayed)
#     - mean_weight                          (community mean wt — displayed)
#     - mean_max_weight                      (community mean max wt — displayed)
#
# All other metric columns are left unchanged.
#
# Files are read from labelled source files and saved to labelled target files,
# so multiple w_min variants can coexist. Edit the CONFIGURATION block below
# to switch the source/target labels and target w_min value.
#
# Naming convention:  {basename}_wmin_{size}g.rds
#   e.g.  fishing_metrics_raw_wmin_3.16e-8g.rds
#         fishing_metrics_raw_wmin_1g.rds
#
# After this script completes, re-run run_absolute_50ci_only.R (with the
# matching WMIN_LABEL) to regenerate the heatmap.
###############################################################################

# === CONFIGURATION ===
# SOURCE_LABEL: the existing labelled RDS files to start from (all non-patch
#               columns are copied from these files unchanged).
# TARGET_MIN_W: the new w_min (grams) to apply for the 8 patch metrics.
# TARGET_LABEL: filename suffix for the output files.
# Change these three values to run a new w_min variant.
SOURCE_LABEL  <- "wmin_3.16e-8g"  # read from {basename}_{SOURCE_LABEL}.rds
TARGET_MIN_W  <- 1                 # new minimum weight (g)
TARGET_LABEL  <- "wmin_1g"        # write to  {basename}_{TARGET_LABEL}.rds

suppressPackageStartupMessages({
  library(mizer)
  library(therMizer)
  library(dplyr)
})

source("ecosystem_assessment_v3.R")

# Override SPECTRUM_MIN_W with the target value AFTER sourcing, so all helper
# functions pick up the new cutoff via their default argument.
SPECTRUM_MIN_W <- TARGET_MIN_W

cat(sprintf("SPECTRUM_MIN_W = %.6e g  (%s)\n", SPECTRUM_MIN_W, TARGET_LABEL))
cat(sprintf("Source files:  *_%s.rds\n", SOURCE_LABEL))
cat(sprintf("Output files:  *_%s.rds\n\n", TARGET_LABEL))

PATCH_COLS <- c(
  "spectrum_slope", "spectrum_intercept",
  "spectrum_mle_exponent", "spectrum_lcd_exponent",
  "spectrum_lbnbiom_slope",
  "production_biomass_ratio",
  "mean_weight", "mean_max_weight"
)

# ---------------------------------------------------------------------------
# Helper: recompute all SPECTRUM_MIN_W-dependent metrics for one sim/decade
# ---------------------------------------------------------------------------
recompute_patch_metrics <- function(sim, time_range) {
  spec <- calculate_spectrum_slope_intercept(sim, time_range)
  list(
    spectrum_slope           = unname(spec["slope"]),
    spectrum_intercept       = unname(spec["intercept"]),
    spectrum_mle_exponent    = calculate_spectrum_mle_exponent(sim, time_range),
    spectrum_lcd_exponent    = calculate_spectrum_lcd_exponent(sim, time_range),
    spectrum_lbnbiom_slope   = calculate_spectrum_lbnbiom_slope(sim, time_range),
    production_biomass_ratio = calculate_production_biomass_ratio(sim, time_range),
    mean_weight              = calculate_mean_weight_mizer(sim, time_range,
                                                           species_group = SPECIES_GROUPS$consumers),
    mean_max_weight          = calculate_mean_max_weight_mizer(sim, time_range,
                                                               species_group = SPECIES_GROUPS$consumers)
  )
}

# ---------------------------------------------------------------------------
# Helper: patch one data frame (fishing or climate) using a list of MizerSims
# ---------------------------------------------------------------------------
patch_metrics_df <- function(df, sims, label) {
  sim_ids <- sort(unique(df$sim_id))
  cat(sprintf("  Patching %s: %d rows / %d simulations\n",
              label, nrow(df), length(sim_ids)))
  pb <- txtProgressBar(min = 0, max = length(sim_ids), style = 3)
  for (k in seq_along(sim_ids)) {
    sid <- sim_ids[k]
    setTxtProgressBar(pb, k)
    if (sid > length(sims)) next
    sim   <- sims[[sid]]
    times <- as.numeric(dimnames(sim@n)$time)
    rows  <- which(df$sim_id == sid)
    for (r in rows) {
      tr <- which(times >= df$start_year[r] & times <= df$end_year[r])
      if (length(tr) == 0) next
      new_vals <- tryCatch(
        recompute_patch_metrics(sim, tr),
        error = function(e) {
          warning(sprintf("sim %d row %d: %s", sid, r, e$message))
          NULL
        }
      )
      if (is.null(new_vals)) next
      for (col in PATCH_COLS) {
        if (col %in% colnames(df) && col %in% names(new_vals))
          df[r, col] <- new_vals[[col]]
      }
    }
  }
  close(pb)
  df
}

# ---------------------------------------------------------------------------
# Load cached data
# ---------------------------------------------------------------------------
cat("Loading cached metric files...\n")
fishing_raw <- readRDS(file.path(OUTPUT_DIR_LARGE, sprintf("fishing_metrics_raw_%s.rds",      SOURCE_LABEL)))
climate_raw <- readRDS(file.path(OUTPUT_DIR_LARGE, sprintf("climate_only_metrics_raw_%s.rds", SOURCE_LABEL)))
b0_ref      <- readRDS(file.path(OUTPUT_DIR_LARGE, sprintf("b0_reference_%s.rds",             SOURCE_LABEL)))
cat(sprintf("  fishing_raw:  %d rows\n", nrow(fishing_raw)))
cat(sprintf("  climate_raw:  %d rows\n", nrow(climate_raw)))
cat(sprintf("  b0_reference: %d rows\n\n", nrow(b0_ref)))

# ---------------------------------------------------------------------------
# Load ensemble simulation objects
# ---------------------------------------------------------------------------
cat("Loading fishing ensemble (this may take a moment)...\n")
mc_fish <- readRDS(ENSEMBLE_PATHS$fishing)
cat("Loading climate-only ensemble...\n")
mc_clim <- readRDS(ENSEMBLE_PATHS$climate_only)

extract_sims <- function(mc) {
  if ("simulations" %in% names(mc)) return(mc$simulations)
  if (is.list(mc) && inherits(mc[[1]], "MizerSim")) return(mc)
  stop("Unrecognized ensemble structure")
}
fish_sims <- extract_sims(mc_fish)
clim_sims <- extract_sims(mc_clim)
cat(sprintf("  Fishing ensemble: %d simulations\n", length(fish_sims)))
cat(sprintf("  Climate ensemble: %d simulations\n\n", length(clim_sims)))

# ---------------------------------------------------------------------------
# Patch fishing_raw
# ---------------------------------------------------------------------------
cat("=== Patching fishing_metrics_raw ===\n")
fishing_raw <- patch_metrics_df(fishing_raw, fish_sims, "fishing_raw")
cat("\n")

# ---------------------------------------------------------------------------
# Patch climate_raw
# ---------------------------------------------------------------------------
cat("=== Patching climate_only_metrics_raw ===\n")
climate_raw <- patch_metrics_df(climate_raw, clim_sims, "climate_only_metrics_raw")
cat("\n")

# ---------------------------------------------------------------------------
# Patch b0_reference (B0-period values computed from climate-only sims)
# ---------------------------------------------------------------------------
cat("=== Patching b0_reference ===\n")
sim_ids_b <- sort(unique(b0_ref$sim_id))
cat(sprintf("  Patching b0_reference: %d rows / %d simulations\n",
            nrow(b0_ref), length(sim_ids_b)))
pb <- txtProgressBar(min = 0, max = length(sim_ids_b), style = 3)
for (k in seq_along(sim_ids_b)) {
  sid <- sim_ids_b[k]
  setTxtProgressBar(pb, k)
  if (sid > length(clim_sims)) next
  sim   <- clim_sims[[sid]]
  times <- as.numeric(dimnames(sim@n)$time)
  tr    <- which(times >= B0_PERIOD[1] & times <= B0_PERIOD[2])
  if (length(tr) == 0) next
  r <- which(b0_ref$sim_id == sid)
  if (length(r) != 1) next
  new_vals <- tryCatch(
    recompute_patch_metrics(sim, tr),
    error = function(e) {
      warning(sprintf("b0 sim %d: %s", sid, e$message))
      NULL
    }
  )
  if (is.null(new_vals)) next
  for (col in PATCH_COLS) {
    if (col %in% colnames(b0_ref) && col %in% names(new_vals))
      b0_ref[r, col] <- new_vals[[col]]
  }
}
close(pb)
cat("\n")

# ---------------------------------------------------------------------------
# Save updated files
# ---------------------------------------------------------------------------
cat("Saving updated cached files...\n")
out_fish <- file.path(OUTPUT_DIR_LARGE, sprintf("fishing_metrics_raw_%s.rds",      TARGET_LABEL))
out_clim <- file.path(OUTPUT_DIR_LARGE, sprintf("climate_only_metrics_raw_%s.rds", TARGET_LABEL))
out_b0   <- file.path(OUTPUT_DIR_LARGE, sprintf("b0_reference_%s.rds",             TARGET_LABEL))
saveRDS(fishing_raw, out_fish); cat(sprintf("  Saved %s\n", basename(out_fish)))
saveRDS(climate_raw, out_clim); cat(sprintf("  Saved %s\n", basename(out_clim)))
saveRDS(b0_ref,      out_b0);   cat(sprintf("  Saved %s\n", basename(out_b0)))

cat(sprintf("\nPatch complete. Run run_absolute_50ci_only.R with WMIN_LABEL = \"%s\" to regenerate the heatmap.\n", TARGET_LABEL))
