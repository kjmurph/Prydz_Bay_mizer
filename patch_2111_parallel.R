###############################################################################
# Parallel Patch: Recompute 8 changed/new metrics for 2111-sim ensemble
#
# Uses file-based chunking to parallelize across PSOCK workers on Windows.
# Each worker loads its own chunk from disk, avoiding memory copy overhead.
#
# Strategy:
#   1. Load ensembles in main process, split into N chunks, save to disk
#   2. Free ensemble memory in main process
#   3. Launch N PSOCK workers — each loads its chunk + assessment functions
#   4. Workers compute 8 patch metrics per sim, save results to disk
#   5. Main process merges results, patches backup data, runs downstream
#
# Resume support: completed chunk results are saved to disk and skipped
# on restart. Delete parallel_results/ to force full recomputation.
#
# System requirements (16 cores, 32 GB RAM):
#   8 workers × ~2.5 GB each ≈ 20 GB + OS overhead → fits in 32 GB
#   Estimated runtime: ~3 hours (vs ~22 hours serial)
###############################################################################

N_WORKERS <- 4   # 4 of 16 cores — conservative to avoid OOM on 32 GB

suppressPackageStartupMessages({
  library(mizer)
  library(therMizer)
  library(parallel)
  library(dplyr)
})

source("ecosystem_assessment_v2.R")
ASSESSMENT_SCRIPT <- normalizePath("ecosystem_assessment_v2.R")

cat("==============================================================\n")
cat("PARALLEL PATCH: 2111-sim ensemble\n")
cat(sprintf("Workers: %d | SPECTRUM_MIN_W: %g\n", N_WORKERS, SPECTRUM_MIN_W))
cat(sprintf("Metrics: slope, intercept, MLE exponent, LCD exponent, LBNbiom slope,\n"))
cat(sprintf("         P/B, fish P/B, marine mammal P/B, sperm biomass, minke biomass\n"))
cat("==============================================================\n\n")

start_time <- Sys.time()

PATCH_COLS <- c("spectrum_slope", "spectrum_intercept", "production_biomass_ratio",
                "spectrum_mle_exponent", "spectrum_lcd_exponent", "spectrum_lbnbiom_slope",
                "fish_pb_ratio", "marine_mammal_pb_ratio",
                "sperm_biomass", "minke_biomass")

# ── 1. Load backup data ─────────────────────────────────────────────────
cat("Loading backup RDS files (2111 sims, old methods)...\n")
fishing_backup <- readRDS(file.path(OUTPUT_DIR_LARGE, "fishing_metrics_raw_pre_fullspectrum_backup.rds"))
climate_backup <- readRDS(file.path(OUTPUT_DIR_LARGE, "climate_only_metrics_raw_pre_fullspectrum_backup.rds"))
b0_backup      <- readRDS(file.path(OUTPUT_DIR_LARGE, "b0_reference_pre_fullspectrum_backup.rds"))

n_sims <- length(unique(fishing_backup$sim_id))
cat(sprintf("  Backup: %d sims, %d fishing rows, %d climate rows, %d B0 rows\n",
            n_sims, nrow(fishing_backup), nrow(climate_backup), nrow(b0_backup)))

# ── 2. Prepare chunks ───────────────────────────────────────────────────
chunk_dir  <- file.path(OUTPUT_DIR_LARGE, "parallel_chunks")
result_dir <- file.path(OUTPUT_DIR_LARGE, "parallel_results")
# Clean stale chunk files from any previous failed run (keep results for resume)
if (dir.exists(chunk_dir))  unlink(chunk_dir, recursive = TRUE)
dir.create(chunk_dir, recursive = TRUE)
if (!dir.exists(result_dir)) dir.create(result_dir, recursive = TRUE)

chunk_indices <- split(seq_len(n_sims),
                       cut(seq_len(n_sims), N_WORKERS, labels = FALSE))

# Identify which chunks still need processing (resume support)
chunks_needed <- list()  # list of list(k, chunk_file)
for (k in seq_along(chunk_indices)) {
  result_file <- file.path(result_dir, sprintf("result_%02d.rds", k))
  if (file.exists(result_file)) {
    cat(sprintf("  Chunk %d: already completed — skipping\n", k))
  } else {
    chunks_needed[[length(chunks_needed) + 1]] <- list(
      k = k,
      chunk_file = file.path(chunk_dir, sprintf("chunk_%02d.rds", k)),
      result_file = result_file,
      indices = chunk_indices[[k]]
    )
  }
}

if (length(chunks_needed) > 0) {
  cat(sprintf("\n%d of %d chunks need processing — loading ensembles...\n",
              length(chunks_needed), N_WORKERS))

  # Load ensembles
  t0 <- Sys.time()
  cat("  Loading fishing ensemble...\n")
  mc_fish <- readRDS(ENSEMBLE_PATHS$fishing)
  fish_sims <- if ("simulations" %in% names(mc_fish)) mc_fish$simulations else mc_fish
  rm(mc_fish); gc(verbose = FALSE)

  cat("  Loading climate-only ensemble...\n")
  mc_clim <- readRDS(ENSEMBLE_PATHS$climate_only)
  clim_sims <- if ("simulations" %in% names(mc_clim)) mc_clim$simulations else mc_clim
  rm(mc_clim); gc(verbose = FALSE)
  cat(sprintf("  Ensembles loaded in %.0f sec\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  stopifnot(length(fish_sims) >= n_sims, length(clim_sims) >= n_sims)

  # Save needed chunks to disk
  cat("  Saving chunks to disk...\n")
  for (cn in chunks_needed) {
    idx <- cn$indices
    saveRDS(
      list(fish_sims = fish_sims[idx],
           clim_sims = clim_sims[idx],
           sim_ids   = idx),
      cn$chunk_file
    )
    cat(sprintf("    Chunk %d: sims %d–%d (%d sims)\n",
                cn$k, min(idx), max(idx), length(idx)))
  }

  # Free ensemble memory in main process
  rm(fish_sims, clim_sims); gc(verbose = FALSE)
  cat("  Ensemble memory freed\n\n")

  # ── 3. Launch parallel workers ───────────────────────────────────────
  n_active <- min(length(chunks_needed), N_WORKERS)
  cat(sprintf("Launching %d PSOCK workers...\n", n_active))
  cat("(Each worker loads its chunk + assessment functions independently)\n\n")

  t_par_start <- Sys.time()
  worker_log <- normalizePath("patch_worker_output.log", mustWork = FALSE)
  cat(sprintf("  Worker output will be logged to: %s\n", worker_log))
  cl <- makeCluster(n_active, outfile = worker_log)

  # Export only the two paths workers need
  clusterExport(cl, c("ASSESSMENT_SCRIPT"), envir = environment())

  chunk_files_to_process <- vapply(chunks_needed, function(cn) cn$chunk_file, "")
  result_files_to_save   <- vapply(chunks_needed, function(cn) cn$result_file, "")

  # Build a named list so workers know where to save
  work_items <- mapply(function(cf, rf) list(chunk_file = cf, result_file = rf),
                       chunk_files_to_process, result_files_to_save,
                       SIMPLIFY = FALSE)

  worker_messages <- tryCatch(
    parLapply(cl, work_items, function(item) {
    suppressPackageStartupMessages({
      library(mizer)
      library(therMizer)
      library(dplyr)
    })
    # Source all assessment functions and constants
    source(ASSESSMENT_SCRIPT)

    chunk_file  <- item$chunk_file
    result_file <- item$result_file
    cat(sprintf("[Worker %d] Loading chunk: %s\n", Sys.getpid(), basename(chunk_file)))

    chunk     <- readRDS(chunk_file)
    fish_sims <- chunk$fish_sims
    clim_sims <- chunk$clim_sims
    sim_ids   <- chunk$sim_ids
    rm(chunk); gc(verbose = FALSE)
    cat(sprintf("[Worker %d] Chunk loaded: %d sims\n", Sys.getpid(), length(sim_ids)))

    fishing_results <- vector("list", length(sim_ids))
    climate_results <- vector("list", length(sim_ids))
    b0_results      <- vector("list", length(sim_ids))

    for (i in seq_along(sim_ids)) {
      sid <- sim_ids[i]
      if (i %% 50 == 1) cat(sprintf("[Worker %d] Processing sim %d/%d (sim_id=%d)\n",
                                     Sys.getpid(), i, length(sim_ids), sid))

      # ---- Fishing simulation ----
      fishing_results[[i]] <- tryCatch({
        sim   <- fish_sims[[i]]
        times <- as.numeric(dimnames(sim@n)$time)
        rows  <- vector("list", nrow(DECADES))
        for (d in seq_len(nrow(DECADES))) {
          dec <- DECADES[d, ]
          tr  <- which(times >= dec$start & times <= dec$end)
          if (length(tr) == 0) next
          spec <- calculate_spectrum_slope_intercept(sim, tr)
          rows[[d]] <- data.frame(
            sim_id = sid, decade = dec$label,
            start_year = dec$start, end_year = dec$end,
            spectrum_slope     = unname(spec["slope"]),
            spectrum_intercept = unname(spec["intercept"]),
            production_biomass_ratio = calculate_production_biomass_ratio(sim, tr),
            spectrum_mle_exponent    = calculate_spectrum_mle_exponent(sim, tr),
            spectrum_lcd_exponent    = calculate_spectrum_lcd_exponent(sim, tr),
            spectrum_lbnbiom_slope   = calculate_spectrum_lbnbiom_slope(sim, tr),
            fish_pb_ratio = calculate_production_biomass_ratio(
              sim, tr, species_group = SPECIES_GROUPS$fish),
            marine_mammal_pb_ratio = calculate_production_biomass_ratio(
              sim, tr, species_group = SPECIES_GROUPS$marine_mammals),
            sperm_biomass = calculate_group_biomass(sim, tr, SPECIES_GROUPS$sperm_whales),
            minke_biomass = calculate_group_biomass(sim, tr, SPECIES_GROUPS$minke_whales),
            stringsAsFactors = FALSE)
        }
        bind_rows(rows)
      }, error = function(e) { cat(sprintf("[Worker %d] Fishing error sim %d: %s\n", Sys.getpid(), sid, e$message)); NULL })

      # ---- Climate-only simulation ----
      climate_results[[i]] <- tryCatch({
        sim   <- clim_sims[[i]]
        times <- as.numeric(dimnames(sim@n)$time)
        rows  <- vector("list", nrow(DECADES))
        for (d in seq_len(nrow(DECADES))) {
          dec <- DECADES[d, ]
          tr  <- which(times >= dec$start & times <= dec$end)
          if (length(tr) == 0) next
          spec <- calculate_spectrum_slope_intercept(sim, tr)
          rows[[d]] <- data.frame(
            sim_id = sid, decade = dec$label,
            start_year = dec$start, end_year = dec$end,
            spectrum_slope     = unname(spec["slope"]),
            spectrum_intercept = unname(spec["intercept"]),
            production_biomass_ratio = calculate_production_biomass_ratio(sim, tr),
            spectrum_mle_exponent    = calculate_spectrum_mle_exponent(sim, tr),
            spectrum_lcd_exponent    = calculate_spectrum_lcd_exponent(sim, tr),
            spectrum_lbnbiom_slope   = calculate_spectrum_lbnbiom_slope(sim, tr),
            fish_pb_ratio = calculate_production_biomass_ratio(
              sim, tr, species_group = SPECIES_GROUPS$fish),
            marine_mammal_pb_ratio = calculate_production_biomass_ratio(
              sim, tr, species_group = SPECIES_GROUPS$marine_mammals),
            sperm_biomass = calculate_group_biomass(sim, tr, SPECIES_GROUPS$sperm_whales),
            minke_biomass = calculate_group_biomass(sim, tr, SPECIES_GROUPS$minke_whales),
            stringsAsFactors = FALSE)
        }
        bind_rows(rows)
      }, error = function(e) { cat(sprintf("[Worker %d] Climate error sim %d: %s\n", Sys.getpid(), sid, e$message)); NULL })

      # ---- B0 reference (from climate-only sim) ----
      b0_results[[i]] <- tryCatch({
        sim   <- clim_sims[[i]]
        times <- as.numeric(dimnames(sim@n)$time)
        tr    <- which(times >= B0_PERIOD[1] & times <= B0_PERIOD[2])
        if (length(tr) == 0) return(NULL)
        spec <- calculate_spectrum_slope_intercept(sim, tr)
        data.frame(
          sim_id = sid,
          spectrum_slope     = unname(spec["slope"]),
          spectrum_intercept = unname(spec["intercept"]),
          production_biomass_ratio = calculate_production_biomass_ratio(sim, tr),
          spectrum_mle_exponent    = calculate_spectrum_mle_exponent(sim, tr),
          spectrum_lcd_exponent    = calculate_spectrum_lcd_exponent(sim, tr),
          spectrum_lbnbiom_slope   = calculate_spectrum_lbnbiom_slope(sim, tr),
          fish_pb_ratio = calculate_production_biomass_ratio(
            sim, tr, species_group = SPECIES_GROUPS$fish),
          marine_mammal_pb_ratio = calculate_production_biomass_ratio(
            sim, tr, species_group = SPECIES_GROUPS$marine_mammals),
          sperm_biomass = calculate_group_biomass(sim, tr, SPECIES_GROUPS$sperm_whales),
          minke_biomass = calculate_group_biomass(sim, tr, SPECIES_GROUPS$minke_whales),
          stringsAsFactors = FALSE)
      }, error = function(e) { cat(sprintf("[Worker %d] B0 error sim %d: %s\n", Sys.getpid(), sid, e$message)); NULL })

      # Free sim memory periodically
      if (i %% 20 == 0) gc(verbose = FALSE)
    }

    cat(sprintf("[Worker %d] Done: saving results to %s\n", Sys.getpid(), basename(result_file)))
    result <- list(
      fishing = bind_rows(fishing_results),
      climate = bind_rows(climate_results),
      b0      = bind_rows(b0_results)
    )

    # Save to disk for resume support
    saveRDS(result, result_file)

    # Return a status message
    msg <- sprintf("Chunk done: %d fishing rows, %d climate rows, %d B0 rows",
            nrow(result$fishing), nrow(result$climate), nrow(result$b0))
    cat(sprintf("[Worker %d] %s\n", Sys.getpid(), msg))
    msg
  }),
  error = function(e) {
    cat(sprintf("\n*** parLapply FAILED: %s ***\n", e$message))
    try(stopCluster(cl), silent = TRUE)
    stop(e)
  })

  stopCluster(cl)

  t_par_elapsed <- as.numeric(difftime(Sys.time(), t_par_start, units = "mins"))
  cat(sprintf("\nParallel computation completed in %.1f minutes (%.1f hours)\n",
              t_par_elapsed, t_par_elapsed / 60))
  for (msg in worker_messages) cat(sprintf("  %s\n", msg))

  # Clean up chunk files (results are kept)
  unlink(chunk_dir, recursive = TRUE)
  cat("  Chunk files cleaned up\n")

} else {
  cat("\nAll chunks already completed — proceeding to merge\n")
}

# ── 4. Merge all chunk results ──────────────────────────────────────────
cat("\nMerging results from all chunks...\n")
result_files <- list.files(result_dir, pattern = "^result_\\d+\\.rds$",
                           full.names = TRUE)
all_fishing <- vector("list", length(result_files))
all_climate <- vector("list", length(result_files))
all_b0      <- vector("list", length(result_files))

for (i in seq_along(result_files)) {
  r <- readRDS(result_files[i])
  all_fishing[[i]] <- r$fishing
  all_climate[[i]] <- r$climate
  all_b0[[i]]      <- r$b0
}

all_fishing_patch <- bind_rows(all_fishing)
all_climate_patch <- bind_rows(all_climate)
all_b0_patch      <- bind_rows(all_b0)

cat(sprintf("  Merged: %d fishing rows, %d climate rows, %d B0 rows\n",
            nrow(all_fishing_patch), nrow(all_climate_patch), nrow(all_b0_patch)))

# ── 5. Patch backup dataframes ──────────────────────────────────────────
cat("Patching backup dataframes with new metric values...\n")

patch_dataframe <- function(backup_df, patch_df, join_cols) {
  cols_to_drop <- intersect(PATCH_COLS, names(backup_df))
  if (length(cols_to_drop) > 0) {
    backup_df <- backup_df[, !(names(backup_df) %in% cols_to_drop), drop = FALSE]
  }
  merge(backup_df, patch_df[, c(join_cols, PATCH_COLS)],
        by = join_cols, all.x = TRUE)
}

fishing_patched <- patch_dataframe(fishing_backup, all_fishing_patch,
                                    c("sim_id", "decade", "start_year", "end_year"))
climate_patched <- patch_dataframe(climate_backup, all_climate_patch,
                                    c("sim_id", "decade", "start_year", "end_year"))

b0_cols_to_drop <- intersect(PATCH_COLS, names(b0_backup))
if (length(b0_cols_to_drop) > 0) {
  b0_backup <- b0_backup[, !(names(b0_backup) %in% b0_cols_to_drop), drop = FALSE]
}
b0_patched <- merge(b0_backup, all_b0_patch[, c("sim_id", PATCH_COLS)],
                    by = "sim_id", all.x = TRUE)

cat(sprintf("  Patched: %d fishing (%d cols), %d climate (%d cols), %d B0 (%d cols)\n",
            nrow(fishing_patched), ncol(fishing_patched),
            nrow(climate_patched), ncol(climate_patched),
            nrow(b0_patched), ncol(b0_patched)))

# Spot-check
cat("\nSpot-check (sim 1, decade 1):\n")
sample_row <- fishing_patched[fishing_patched$sim_id == 1, ][1, ]
for (col in PATCH_COLS) {
  cat(sprintf("  %-30s = %s\n", col, format(sample_row[[col]], digits = 6)))
}

# ── 6. Save patched data ────────────────────────────────────────────────
cat("\nSaving patched dataframes...\n")
saveRDS(fishing_patched, file.path(OUTPUT_DIR_LARGE, "fishing_metrics_raw.rds"))
saveRDS(climate_patched, file.path(OUTPUT_DIR_LARGE, "climate_only_metrics_raw.rds"))
saveRDS(b0_patched,      file.path(OUTPUT_DIR_LARGE, "b0_reference.rds"))
saveRDS(fishing_patched, file.path(OUTPUT_DIR, "fishing_metrics_raw.rds"))
saveRDS(climate_patched, file.path(OUTPUT_DIR, "climate_only_metrics_raw.rds"))
saveRDS(b0_patched,      file.path(OUTPUT_DIR, "b0_reference.rds"))
cat("  Saved to both output directories\n")

# ── 7. Run downstream pipeline ──────────────────────────────────────────
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

# Save summaries
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

# ── 8. Final summary ────────────────────────────────────────────────────
total_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
cat(sprintf("\n==============================================================\n"))
cat(sprintf("PARALLEL PATCH COMPLETE\n"))
cat(sprintf("  Total time: %.1f minutes (%.1f hours)\n", total_time, total_time / 60))
cat(sprintf("  Patched %d simulations across %d metrics\n", n_sims, length(PATCH_COLS)))
cat(sprintf("  Output: %d plots, %d summary CSVs\n", length(plots), 3 * 2 + 1))
cat(sprintf("==============================================================\n"))
