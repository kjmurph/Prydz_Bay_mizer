###############################################################################
# RECALCULATE SPECTRUM SLOPE — FULL COMMUNITY SIZE RANGE
#
# Purpose:
#   Patches fishing_metrics_raw.rds, climate_only_metrics_raw.rds, and
#   b0_reference.rds with recomputed spectrum_slope and spectrum_intercept
#   values using the full model size range (getCommunitySlope defaults).
#
#   The original calculation truncated the spectrum at max_w = 1e6 g (1 tonne),
#   which excluded medium divers, large divers, and all whale species.
#
# Usage:
#   Rscript recalculate_spectrum_slope.R
#
# After completion, run replot_existing_outputs.R to regenerate figures.
###############################################################################

suppressPackageStartupMessages({
  library(therMizer)
  library(mizer)
  library(dplyr)
})

# Source main script to get constants (DECADES, B0_PERIOD, OUTPUT_DIR, etc.)
# NOTE: This will also execute top-level code; progress messages are expected.
cat("Loading functions and constants from ecosystem_assessment_v2.R...\n")
source("ecosystem_assessment_v2.R")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
FISHING_ENSEMBLE <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
CLIMATE_ENSEMBLE <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"

METRICS_DIR  <- OUTPUT_DIR_LARGE   # "Output_large_files/ecosystem_assessment"
BACKUP_SUFFIX <- "_pre_fullspectrum_backup"

# ------------------------------------------------------------------------------
# Load existing metric files (will be patched in-place)
# ------------------------------------------------------------------------------
cat("=== Loading existing metric files ===\n")

fish_raw_path  <- file.path(METRICS_DIR, "fishing_metrics_raw.rds")
clim_raw_path  <- file.path(METRICS_DIR, "climate_only_metrics_raw.rds")
b0_raw_path    <- file.path(METRICS_DIR, "b0_reference.rds")

for (p in c(fish_raw_path, clim_raw_path, b0_raw_path)) {
  if (!file.exists(p)) stop("Required file not found: ", p)
}

fish_raw <- readRDS(fish_raw_path)
clim_raw <- readRDS(clim_raw_path)
b0_ref   <- readRDS(b0_raw_path)

cat(sprintf("  fishing_metrics_raw:     %d rows, %d sims\n",
            nrow(fish_raw), length(unique(fish_raw$sim_id))))
cat(sprintf("  climate_only_metrics_raw:%d rows, %d sims\n",
            nrow(clim_raw), length(unique(clim_raw$sim_id))))
cat(sprintf("  b0_reference:            %d rows\n", nrow(b0_ref)))

# Back up originals
cat("\nBacking up originals...\n")
saveRDS(fish_raw, sub("\\.rds$", paste0(BACKUP_SUFFIX, ".rds"), fish_raw_path))
saveRDS(clim_raw, sub("\\.rds$", paste0(BACKUP_SUFFIX, ".rds"), clim_raw_path))
saveRDS(b0_ref,   sub("\\.rds$", paste0(BACKUP_SUFFIX, ".rds"), b0_raw_path))
cat("  Backups saved with suffix:", BACKUP_SUFFIX, "\n\n")

# ------------------------------------------------------------------------------
# Load ensembles
# ------------------------------------------------------------------------------
cat("=== Loading ensemble simulation objects ===\n")
cat("  Loading fishing ensemble (this may take several minutes)...\n")
mc_fish <- readRDS(FISHING_ENSEMBLE)
fish_sims <- if ("simulations" %in% names(mc_fish)) mc_fish$simulations else mc_fish
cat(sprintf("  Loaded %d fished simulations\n", length(fish_sims)))

cat("  Loading climate-only ensemble...\n")
mc_clim <- readRDS(CLIMATE_ENSEMBLE)
clim_sims <- if ("simulations" %in% names(mc_clim)) mc_clim$simulations else mc_clim
cat(sprintf("  Loaded %d climate-only simulations\n", length(clim_sims)))

n_sims <- min(length(fish_sims), length(clim_sims))
cat(sprintf("  Processing %d paired simulations\n\n", n_sims))

# ------------------------------------------------------------------------------
# Helper: compute spectrum slope/intercept for all decades in one sim
# (uses updated calculate_spectrum_slope_intercept filtered to species >= krill w_inf)
# ------------------------------------------------------------------------------
compute_spectrum_for_sim <- function(sim) {
  times <- as.numeric(dimnames(sim@n)$time)
  out <- vector("list", nrow(DECADES))
  for (i in seq_len(nrow(DECADES))) {
    decade <- DECADES[i, ]
    time_range <- which(times >= decade$start & times <= decade$end)
    if (length(time_range) == 0) {
      out[[i]] <- data.frame(decade = decade$label, slope = NA_real_, intercept = NA_real_)
      next
    }
    spec <- tryCatch(
      calculate_spectrum_slope_intercept(sim, time_range),  # full model size range (getCommunitySlope defaults)
      error = function(e) c(slope = NA_real_, intercept = NA_real_)
    )
    out[[i]] <- data.frame(
      decade     = decade$label,
      slope      = unname(spec["slope"]),
      intercept  = unname(spec["intercept"])
    )
  }
  bind_rows(out)
}

compute_b0_spectrum_for_sim <- function(sim) {
  times <- as.numeric(dimnames(sim@n)$time)
  time_range <- which(times >= B0_PERIOD[1] & times <= B0_PERIOD[2])
  if (length(time_range) == 0) return(c(slope = NA_real_, intercept = NA_real_))
  tryCatch(
    calculate_spectrum_slope_intercept(sim, time_range),
    error = function(e) c(slope = NA_real_, intercept = NA_real_)
  )
}

# ------------------------------------------------------------------------------
# Compute spectra sequentially
# Note: fish_sims/clim_sims are too large to export to cluster workers on
# Windows; sequential processing is used instead. The spectrum calculation
# via getCommunitySlope() is fast once the data are in memory.
# ------------------------------------------------------------------------------
cat("=== Recomputing spectrum slope/intercept (full model size range) ===\n")

sim_ids <- seq_len(n_sims)

cat(sprintf("  Computing fishing ensemble spectra [%d sims]...\n", n_sims))
fish_spectra <- vector("list", n_sims)
for (i in sim_ids) {
  if (i %% 100 == 0) cat(sprintf("    %d / %d\n", i, n_sims))
  fish_spectra[[i]] <- tryCatch(
    compute_spectrum_for_sim(fish_sims[[i]]),
    error = function(e) NULL
  )
}

cat(sprintf("  Computing climate-only ensemble spectra [%d sims]...\n", n_sims))
clim_spectra <- vector("list", n_sims)
for (i in sim_ids) {
  if (i %% 100 == 0) cat(sprintf("    %d / %d\n", i, n_sims))
  clim_spectra[[i]] <- tryCatch(
    compute_spectrum_for_sim(clim_sims[[i]]),
    error = function(e) NULL
  )
}

cat(sprintf("  Computing B0 reference spectra (from climate-only) [%d sims]...\n", n_sims))
b0_spectra <- vector("list", n_sims)
for (i in sim_ids) {
  if (i %% 100 == 0) cat(sprintf("    %d / %d\n", i, n_sims))
  b0_spectra[[i]] <- tryCatch(
    compute_b0_spectrum_for_sim(clim_sims[[i]]),
    error = function(e) c(slope = NA_real_, intercept = NA_real_)
  )
}

# Check success rate before patching
n_fish_ok <- sum(!sapply(fish_spectra, is.null))
n_clim_ok <- sum(!sapply(clim_spectra, is.null))
cat(sprintf("  Fishing spectra computed:       %d / %d\n", n_fish_ok, n_sims))
cat(sprintf("  Climate-only spectra computed:  %d / %d\n", n_clim_ok, n_sims))
if (n_fish_ok == 0) stop("All fishing spectrum calculations failed. Check that ecosystem_assessment_v2.R was sourced correctly.")

cat("\n=== Patching metric data frames ===\n")

# Helper: build a patch data frame from a list of per-sim spectrum results
build_patch_df <- function(spectra_list, sim_ids) {
  rows <- lapply(sim_ids, function(i) {
    res <- spectra_list[[i]]
    if (is.null(res)) return(NULL)
    res$sim_id <- i
    res
  })
  bind_rows(rows)
}

# ------------------------------------------------------------------------------
# Patch fishing_raw
# ------------------------------------------------------------------------------
patch_fish <- build_patch_df(fish_spectra, sim_ids)
cat(sprintf("  patch_fish rows: %d, cols: %s\n", nrow(patch_fish), paste(names(patch_fish), collapse=", ")))

fish_raw_updated <- fish_raw %>%
  left_join(patch_fish %>% rename(spectrum_slope_new = slope, spectrum_intercept_new = intercept),
            by = c("sim_id", "decade")) %>%
  mutate(
    spectrum_slope     = coalesce(spectrum_slope_new,     spectrum_slope),
    spectrum_intercept = coalesce(spectrum_intercept_new, spectrum_intercept)
  ) %>%
  select(-spectrum_slope_new, -spectrum_intercept_new)

n_patched_fish <- sum(!is.na(patch_fish$slope))
cat(sprintf("  fishing_metrics_raw: patched %d / %d decade-sim rows\n",
            n_patched_fish, nrow(fish_raw)))

# ------------------------------------------------------------------------------
# Patch climate_raw
# ------------------------------------------------------------------------------
patch_clim <- build_patch_df(clim_spectra, sim_ids)

clim_raw_updated <- clim_raw %>%
  left_join(patch_clim %>% rename(spectrum_slope_new = slope, spectrum_intercept_new = intercept),
            by = c("sim_id", "decade")) %>%
  mutate(
    spectrum_slope     = coalesce(spectrum_slope_new,     spectrum_slope),
    spectrum_intercept = coalesce(spectrum_intercept_new, spectrum_intercept)
  ) %>%
  select(-spectrum_slope_new, -spectrum_intercept_new)

n_patched_clim <- sum(!is.na(patch_clim$slope))
cat(sprintf("  climate_only_metrics_raw: patched %d / %d decade-sim rows\n",
            n_patched_clim, nrow(clim_raw)))

# ------------------------------------------------------------------------------
# Patch b0_reference
# ------------------------------------------------------------------------------
b0_patch_df <- bind_rows(lapply(sim_ids, function(i) {
  spec <- b0_spectra[[i]]
  data.frame(
    sim_id                 = i,
    spectrum_slope_new     = unname(spec["slope"]),
    spectrum_intercept_new = unname(spec["intercept"])
  )
}))

b0_ref_updated <- b0_ref %>%
  left_join(b0_patch_df, by = "sim_id") %>%
  mutate(
    spectrum_slope     = coalesce(spectrum_slope_new,     spectrum_slope),
    spectrum_intercept = coalesce(spectrum_intercept_new, spectrum_intercept)
  ) %>%
  select(-spectrum_slope_new, -spectrum_intercept_new)

n_patched_b0 <- sum(!is.na(b0_patch_df$spectrum_slope_new))
cat(sprintf("  b0_reference: patched %d / %d rows\n", n_patched_b0, nrow(b0_ref)))

# ------------------------------------------------------------------------------
# Save updated files
# ------------------------------------------------------------------------------
cat("\n=== Saving updated files ===\n")

saveRDS(fish_raw_updated,  fish_raw_path)
cat("  Saved:", fish_raw_path, "\n")

saveRDS(clim_raw_updated, clim_raw_path)
cat("  Saved:", clim_raw_path, "\n")

saveRDS(b0_ref_updated,    b0_raw_path)
cat("  Saved:", b0_raw_path, "\n")

# Also update the standard output dir copies used by diagnostics
saveRDS(fish_raw_updated, file.path(OUTPUT_DIR, "fishing_metrics_raw.rds"))
saveRDS(clim_raw_updated, file.path(OUTPUT_DIR, "climate_only_metrics_raw.rds"))
cat("  Updated OUTPUT_DIR copies in:", OUTPUT_DIR, "\n")

cat("\n=== Summary ===\n")
cat("  spectrum_slope and spectrum_intercept recalculated over full model size range (getCommunitySlope defaults)\n")
cat("  Previously excluded species (max_w > 1e6 g):\n")
cat("    medium divers (w_max = 1.28e6), large divers (2.02e6),\n")
cat("    minke whales (6e6), orca (1.06e7), sperm whales (3.65e7),\n")
cat("    baleen whales (1.03e8)\n\n")
cat("Next step: run  Rscript replot_existing_outputs.R\n")
cat("  to regenerate all heatmaps and figures with updated spectrum values.\n\n")
cat("=== Done ===\n")
