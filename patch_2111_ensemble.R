###############################################################################
# Incremental Patch: Recompute 8 changed/new metrics for 2111-sim ensemble
#
# This script avoids a full re-run by:
#   1. Loading the 2111-sim backup RDS files (old methods)
#   2. Loading the raw ensemble simulation objects
#   3. Recomputing ONLY the 8 columns that changed:
#      - spectrum_slope        (recomputed: weight-bin filter >= Antarctic krill w_min)
#      - spectrum_intercept    (recomputed: weight-bin filter)
#      - production_biomass_ratio (recomputed: weight-bin filter)
#      - spectrum_mle_exponent (new metric)
#      - fish_pb_ratio         (new metric)
#      - marine_mammal_pb_ratio (new metric)
#      - sperm_biomass         (new column)
#      - minke_biomass         (new column)
#   4. Patching those columns into the backup dataframes
#   5. Running the downstream pipeline (thresholds, pairing, summaries, plots)
#
# Expected runtime: ~same as full run since every sim must be loaded,
#   but each sim only computes 8 metrics instead of ~28 (per decade).
#   The savings come from skipping ~20 metric calculations per sim-decade.
###############################################################################

# Source the core library (loads all functions, constants, SPECTRUM_MIN_W)
source("ecosystem_assessment_v2.R")

cat("=============================================================\n")
cat("INCREMENTAL PATCH: 2111-sim ensemble\n")
cat("Recomputing 8 metrics with SPECTRUM_MIN_W =", SPECTRUM_MIN_W, "\n")
cat("=============================================================\n\n")

start_time <- Sys.time()

# ── 1. Load backup data ───────────────────────────────────────────────────
cat("Loading backup RDS files (2111 sims, old methods)...\n")
fishing_backup <- readRDS(file.path(OUTPUT_DIR_LARGE, "fishing_metrics_raw_pre_fullspectrum_backup.rds"))
climate_backup <- readRDS(file.path(OUTPUT_DIR_LARGE, "climate_only_metrics_raw_pre_fullspectrum_backup.rds"))
b0_backup      <- readRDS(file.path(OUTPUT_DIR_LARGE, "b0_reference_pre_fullspectrum_backup.rds"))

cat(sprintf("  Fishing backup:  %d rows, %d sims\n", nrow(fishing_backup), length(unique(fishing_backup$sim_id))))
cat(sprintf("  Climate backup:  %d rows, %d sims\n", nrow(climate_backup), length(unique(climate_backup$sim_id))))
cat(sprintf("  B0 backup:       %d rows\n", nrow(b0_backup)))

n_sims <- length(unique(fishing_backup$sim_id))

# ── 2. Load raw ensemble simulations ─────────────────────────────────────
cat("\nLoading fishing ensemble simulations...\n")
mc_fish <- readRDS(ENSEMBLE_PATHS$fishing)
fish_sims <- if ("simulations" %in% names(mc_fish)) mc_fish$simulations else mc_fish

cat("Loading climate-only ensemble simulations...\n")
mc_clim <- readRDS(ENSEMBLE_PATHS$climate_only)
clim_sims <- if ("simulations" %in% names(mc_clim)) mc_clim$simulations else mc_clim

cat(sprintf("  Available: %d fishing, %d climate-only simulations\n",
            length(fish_sims), length(clim_sims)))

# Verify we have enough sims
stopifnot(length(fish_sims) >= n_sims, length(clim_sims) >= n_sims)

# ── 3. Define the 8 columns to patch ────────────────────────────────────
PATCH_COLS <- c("spectrum_slope", "spectrum_intercept", "production_biomass_ratio",
                "spectrum_mle_exponent", "fish_pb_ratio", "marine_mammal_pb_ratio",
                "sperm_biomass", "minke_biomass")

# Helper: extract ONLY the 8 patched metrics for one simulation across decades
extract_patch_metrics <- function(sim) {
  times <- as.numeric(dimnames(sim@n)$time)
  results <- list()
  for (i in seq_len(nrow(DECADES))) {
    decade <- DECADES[i, ]
    time_range <- which(times >= decade$start & times <= decade$end)
    if (length(time_range) == 0) next
    spec <- calculate_spectrum_slope_intercept(sim, time_range)
    results[[i]] <- data.frame(
      decade       = decade$label,
      start_year   = decade$start,
      end_year     = decade$end,
      spectrum_slope     = unname(spec["slope"]),
      spectrum_intercept = unname(spec["intercept"]),
      production_biomass_ratio = calculate_production_biomass_ratio(sim, time_range),
      spectrum_mle_exponent    = calculate_spectrum_mle_exponent(sim, time_range),
      fish_pb_ratio = calculate_production_biomass_ratio(sim, time_range,
                                                         species_group = SPECIES_GROUPS$fish),
      marine_mammal_pb_ratio = calculate_production_biomass_ratio(sim, time_range,
                                                                   species_group = SPECIES_GROUPS$marine_mammals),
      sperm_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$sperm_whales),
      minke_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$minke_whales),
      stringsAsFactors = FALSE
    )
  }
  return(bind_rows(results))
}

# Helper: extract ONLY the 8 patched metrics for B0 reference period
extract_patch_b0 <- function(sim) {
  times <- as.numeric(dimnames(sim@n)$time)
  time_range <- which(times >= B0_PERIOD[1] & times <= B0_PERIOD[2])
  if (length(time_range) == 0) return(NULL)
  spec <- calculate_spectrum_slope_intercept(sim, time_range)
  c(
    spectrum_slope     = unname(spec["slope"]),
    spectrum_intercept = unname(spec["intercept"]),
    production_biomass_ratio = calculate_production_biomass_ratio(sim, time_range),
    spectrum_mle_exponent    = calculate_spectrum_mle_exponent(sim, time_range),
    fish_pb_ratio = calculate_production_biomass_ratio(sim, time_range,
                                                       species_group = SPECIES_GROUPS$fish),
    marine_mammal_pb_ratio = calculate_production_biomass_ratio(sim, time_range,
                                                                 species_group = SPECIES_GROUPS$marine_mammals),
    sperm_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$sperm_whales),
    minke_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$minke_whales)
  )
}

# ── 4. Loop through all sims and compute patch metrics ──────────────────
cat(sprintf("\nComputing 8 patch metrics across %d simulations x 17 decades...\n", n_sims))

# Checkpoint support
checkpoint_dir <- file.path(OUTPUT_DIR_LARGE, "patch_checkpoints")
if (!dir.exists(checkpoint_dir)) dir.create(checkpoint_dir, recursive = TRUE)

CHECKPOINT_INTERVAL <- 50

# Check for resume point
existing_checkpoints <- list.files(checkpoint_dir, pattern = "^patch_checkpoint_\\d+\\.rds$", full.names = TRUE)
start_sim <- 0
if (length(existing_checkpoints) > 0) {
  cp_nums <- as.integer(gsub(".*checkpoint_(\\d+)\\.rds$", "\\1", existing_checkpoints))
  start_sim <- max(cp_nums)
  cat(sprintf("*** RESUMING from checkpoint at simulation %d ***\n", start_sim))
}

fishing_patch_list  <- list()
climate_patch_list  <- list()
b0_patch_list       <- list()

pb <- txtProgressBar(min = start_sim, max = n_sims, style = 3)

for (i in (start_sim + 1):n_sims) {
  setTxtProgressBar(pb, i)

  # Fishing sim
  fish_patch <- tryCatch(extract_patch_metrics(fish_sims[[i]]), error = function(e) {
    warning(sprintf("Sim %d fishing patch failed: %s", i, e$message))
    NULL
  })
  if (!is.null(fish_patch) && nrow(fish_patch) > 0) {
    fish_patch$sim_id <- i
    fishing_patch_list[[length(fishing_patch_list) + 1]] <- fish_patch
  }

  # Climate-only sim
  clim_patch <- tryCatch(extract_patch_metrics(clim_sims[[i]]), error = function(e) {
    warning(sprintf("Sim %d climate patch failed: %s", i, e$message))
    NULL
  })
  if (!is.null(clim_patch) && nrow(clim_patch) > 0) {
    clim_patch$sim_id <- i
    climate_patch_list[[length(climate_patch_list) + 1]] <- clim_patch
  }

  # B0 reference (from climate-only sim)
  b0_patch <- tryCatch(extract_patch_b0(clim_sims[[i]]), error = function(e) {
    warning(sprintf("Sim %d B0 patch failed: %s", i, e$message))
    NULL
  })
  if (!is.null(b0_patch)) {
    b0_df <- as.data.frame(t(b0_patch))
    b0_df$sim_id <- i
    b0_patch_list[[length(b0_patch_list) + 1]] <- b0_df
  }

  # Checkpoint
  if (i %% CHECKPOINT_INTERVAL == 0) {
    saveRDS(list(
      fishing  = bind_rows(fishing_patch_list),
      climate  = bind_rows(climate_patch_list),
      b0       = bind_rows(b0_patch_list),
      last_sim = i
    ), file.path(checkpoint_dir, sprintf("patch_checkpoint_%04d.rds", i)))
    fishing_patch_list  <- list()
    climate_patch_list  <- list()
    b0_patch_list       <- list()
    cat(sprintf("\n  [Checkpoint saved at sim %d]\n", i))
  }
}
close(pb)

# ── 5. Merge all checkpoints + remaining ────────────────────────────────
cat("\nMerging checkpoint data...\n")
all_fishing_patch  <- bind_rows(fishing_patch_list)
all_climate_patch  <- bind_rows(climate_patch_list)
all_b0_patch       <- bind_rows(b0_patch_list)

# Merge in checkpoint files
cp_files <- list.files(checkpoint_dir, pattern = "^patch_checkpoint_.*\\.rds$", full.names = TRUE)
for (cp_file in cp_files) {
  cp <- readRDS(cp_file)
  all_fishing_patch  <- bind_rows(cp$fishing, all_fishing_patch)
  all_climate_patch  <- bind_rows(cp$climate, all_climate_patch)
  all_b0_patch       <- bind_rows(cp$b0, all_b0_patch)
}

cat(sprintf("  Patch data: %d fishing rows, %d climate rows, %d B0 rows\n",
            nrow(all_fishing_patch), nrow(all_climate_patch), nrow(all_b0_patch)))

# ── 6. Patch the backup dataframes ──────────────────────────────────────
cat("\nPatching backup dataframes with new metric values...\n")

patch_dataframe <- function(backup_df, patch_df, join_cols) {
  # Remove old versions of patch columns from backup
  cols_to_drop <- intersect(PATCH_COLS, names(backup_df))
  if (length(cols_to_drop) > 0) {
    backup_df <- backup_df[, !(names(backup_df) %in% cols_to_drop), drop = FALSE]
  }
  # Join patch columns by sim_id + decade key
  merged <- merge(backup_df, patch_df[, c(join_cols, PATCH_COLS)],
                  by = join_cols, all.x = TRUE)
  return(merged)
}

fishing_patched <- patch_dataframe(fishing_backup, all_fishing_patch,
                                    c("sim_id", "decade", "start_year", "end_year"))
climate_patched <- patch_dataframe(climate_backup, all_climate_patch,
                                    c("sim_id", "decade", "start_year", "end_year"))

# B0 has no decade column — join by sim_id only
b0_cols_to_drop <- intersect(PATCH_COLS, names(b0_backup))
if (length(b0_cols_to_drop) > 0) {
  b0_backup <- b0_backup[, !(names(b0_backup) %in% b0_cols_to_drop), drop = FALSE]
}
b0_patched <- merge(b0_backup, all_b0_patch[, c("sim_id", PATCH_COLS)],
                    by = "sim_id", all.x = TRUE)

cat(sprintf("  Patched fishing:  %d rows, %d cols\n", nrow(fishing_patched), ncol(fishing_patched)))
cat(sprintf("  Patched climate:  %d rows, %d cols\n", nrow(climate_patched), ncol(climate_patched)))
cat(sprintf("  Patched B0:       %d rows, %d cols\n", nrow(b0_patched), ncol(b0_patched)))

# Spot-check
cat("\nSpot-check patched values (sim 1, decade 1):\n")
sample_row <- fishing_patched[fishing_patched$sim_id == 1, ][1, ]
for (col in PATCH_COLS) {
  cat(sprintf("  %-30s = %s\n", col, format(sample_row[[col]], digits = 6)))
}

# ── 7. Save patched data as current ────────────────────────────────────
cat("\nSaving patched dataframes as current output...\n")
saveRDS(fishing_patched, file.path(OUTPUT_DIR_LARGE, "fishing_metrics_raw.rds"))
saveRDS(climate_patched, file.path(OUTPUT_DIR_LARGE, "climate_only_metrics_raw.rds"))
saveRDS(b0_patched,      file.path(OUTPUT_DIR_LARGE, "b0_reference.rds"))
# Also save to standard output dir
saveRDS(fishing_patched, file.path(OUTPUT_DIR, "fishing_metrics_raw.rds"))
saveRDS(climate_patched, file.path(OUTPUT_DIR, "climate_only_metrics_raw.rds"))
saveRDS(b0_patched,      file.path(OUTPUT_DIR, "b0_reference.rds"))
cat("  Saved to both output directories\n")

# ── 8. Run downstream pipeline ─────────────────────────────────────────
cat("\n=============================================================\n")
cat("RUNNING DOWNSTREAM PIPELINE\n")
cat("=============================================================\n\n")

# Derive empirical thresholds
cat("Deriving empirical thresholds from patched B0 reference...\n")
empirical_thresholds <- derive_empirical_thresholds(b0_patched)
saveRDS(empirical_thresholds, file.path(OUTPUT_DIR_LARGE, "empirical_thresholds.rds"))
saveRDS(empirical_thresholds, file.path(OUTPUT_DIR, "empirical_thresholds.rds"))

# Compute paired comparisons
cat("Computing paired comparisons...\n")
paired_data <- compute_paired_comparisons(
  fishing_patched, climate_patched, b0_patched, empirical_thresholds)
saveRDS(paired_data, file.path(OUTPUT_DIR_LARGE, "paired_data.rds"))

# Summarize by category
cat("Summarizing results by metric category...\n")
biomass_summaries <- summarize_biomass_ratios(paired_data)
structural_summaries <- summarize_structural_deviations(paired_data, empirical_thresholds)
exploitation_summary <- summarize_exploitation(paired_data)

# Pre-whaling validation
validation <- validate_pre_exploitation(biomass_summaries, structural_summaries)

# Generate visualizations
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

cat("  Combined heatmaps (biomass + structural)...\n")
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

cat("  Category C: Exploitation rate heatmap...\n")
plots$exploitation_F <- plot_exploitation_heatmap(exploitation_summary, OUTPUT_DIR)

# Save summaries
cat("\nSaving summary CSVs...\n")
for (comp in c("exploit", "absolute", "climate")) {
  write.csv(biomass_summaries[[comp]],
            file.path(OUTPUT_DIR, sprintf("summary_biomass_%s.csv", comp)), row.names = FALSE)
  write.csv(structural_summaries[[comp]],
            file.path(OUTPUT_DIR, sprintf("summary_structural_%s.csv", comp)), row.names = FALSE)
}
write.csv(exploitation_summary,
          file.path(OUTPUT_DIR, "summary_exploitation_rates.csv"), row.names = FALSE)
if (!is.null(validation)) {
  if (!is.null(validation$biomass))
    write.csv(validation$biomass, file.path(OUTPUT_DIR, "validation_biomass.csv"), row.names = FALSE)
  if (!is.null(validation$structural))
    write.csv(validation$structural, file.path(OUTPUT_DIR, "validation_structural.csv"), row.names = FALSE)
}

# Clean up patch checkpoints
cat("\nCleaning up patch checkpoints...\n")
unlink(checkpoint_dir, recursive = TRUE)

end_time <- Sys.time()
cat(sprintf("\n=============================================================\n"))
cat(sprintf("PATCH COMPLETE (%.1f minutes)\n", difftime(end_time, start_time, units = "mins")))
cat(sprintf("  Patched %d simulations across 8 metrics\n", n_sims))
cat(sprintf("  Output: %d plots, %d summary CSVs\n", length(plots), 3 * 2 + 1))
cat("=============================================================\n")
