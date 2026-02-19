#!/usr/bin/env Rscript
# ==============================================================================
# CREATE 10-SIMULATION EXAMPLE SUBSET FROM FULL 2111-SIM ENSEMBLE
# ==============================================================================
# Produces self-contained example data in example_data/ that is structurally
# identical to the full ensemble in every respect, just limited to 10 sims.
#
# Output files (example_data/):
#   fishing_ensemble/
#     mc_ensemble_10_example.rds      — list of 10 MizerSim objects (fishing)
#   climate_only_ensemble/
#     climate_only_ensemble_10_example.rds  — list of 10 MizerSim objects
#   ecosystem_assessment/
#     fishing_metrics_raw.rds         — metrics for 10 sims only
#     climate_only_metrics_raw.rds    — metrics for 10 sims only
#     b0_reference.rds                — B0 reference for 10 sims only
#     paired_data.rds                 — paired comparisons for 10 sims only
#     empirical_thresholds.rds        — thresholds derived from 10 sims
# ==============================================================================

cat("=============================================================\n")
cat("CREATING 10-SIM EXAMPLE SUBSET\n")
cat("=============================================================\n\n")

# Sim indices to extract (1:10 — the first 10, consistent across all files)
SUBSET_IDS <- 1:10

# Source paths (full ensemble)
FISHING_FULL   <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
CLIMATE_FULL   <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
METRICS_DIR    <- "Output_large_files/ecosystem_assessment"

# Destination paths
OUT_BASE       <- "example_data"
OUT_FISHING    <- file.path(OUT_BASE, "fishing_ensemble")
OUT_CLIMATE    <- file.path(OUT_BASE, "climate_only_ensemble")
OUT_METRICS    <- file.path(OUT_BASE, "ecosystem_assessment")

for (d in c(OUT_FISHING, OUT_CLIMATE, OUT_METRICS)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# HELPER: extract simulation list from either ensemble structure
# ==============================================================================
extract_sims <- function(mc) {
  if ("simulations" %in% names(mc)) return(mc$simulations)
  if (is.list(mc) && inherits(mc[[1]], "MizerSim")) return(mc)
  stop("Unrecognized ensemble structure — expected list of MizerSim or $simulations slot")
}

# Rebuild the original wrapper structure with a subset of simulations
rebuild_ensemble <- function(mc, subset_sims) {
  if ("simulations" %in% names(mc)) {
    # Named-list wrapper — copy all non-simulation slots, replace $simulations
    out <- mc
    out$simulations <- subset_sims
    # Update any metadata counts that may exist
    if ("n_sims" %in% names(out)) out$n_sims <- length(subset_sims)
    if ("nsims"  %in% names(out)) out$nsims  <- length(subset_sims)
    if ("n"      %in% names(out)) out$n      <- length(subset_sims)
    return(out)
  }
  # Plain list of MizerSim — just return the subset
  return(subset_sims)
}

# ==============================================================================
# 1. FISHING ENSEMBLE
# ==============================================================================
cat("Loading fishing ensemble (~1.8 GB — this may take a few minutes)...\n")
t0 <- proc.time()
mc_fish <- readRDS(FISHING_FULL)
cat(sprintf("  Loaded in %.1f s\n", (proc.time() - t0)["elapsed"]))

fish_sims_all <- extract_sims(mc_fish)
cat(sprintf("  Full ensemble: %d simulations\n", length(fish_sims_all)))
cat(sprintf("  Simulation class: %s\n", class(fish_sims_all[[1]])))

fish_subset <- fish_sims_all[SUBSET_IDS]
fish_out    <- rebuild_ensemble(mc_fish, fish_subset)

out_path <- file.path(OUT_FISHING, "mc_ensemble_10_example.rds")
saveRDS(fish_out, out_path, compress = "xz")
cat(sprintf("  Saved: %s (%.1f MB)\n\n",
            out_path, file.info(out_path)$size / 1e6))

rm(mc_fish, fish_sims_all, fish_subset, fish_out)
gc()

# ==============================================================================
# 2. CLIMATE-ONLY ENSEMBLE
# ==============================================================================
cat("Loading climate-only ensemble (~1.8 GB — this may take a few minutes)...\n")
t0 <- proc.time()
mc_clim <- readRDS(CLIMATE_FULL)
cat(sprintf("  Loaded in %.1f s\n", (proc.time() - t0)["elapsed"]))

clim_sims_all <- extract_sims(mc_clim)
cat(sprintf("  Full ensemble: %d simulations\n", length(clim_sims_all)))

clim_subset <- clim_sims_all[SUBSET_IDS]
clim_out    <- rebuild_ensemble(mc_clim, clim_subset)

out_path <- file.path(OUT_CLIMATE, "climate_only_ensemble_10_example.rds")
saveRDS(clim_out, out_path, compress = "xz")
cat(sprintf("  Saved: %s (%.1f MB)\n\n",
            out_path, file.info(out_path)$size / 1e6))

rm(mc_clim, clim_sims_all, clim_subset, clim_out)
gc()

# ==============================================================================
# 3. ECOSYSTEM ASSESSMENT DERIVED FILES  (filter to SUBSET_IDS)
# ==============================================================================
cat("Subsetting ecosystem assessment data files...\n\n")

subset_rds <- function(src_file, dest_file, filter_expr, label) {
  if (!file.exists(src_file)) { cat(sprintf("  SKIP (not found): %s\n", src_file)); return() }
  dat <- readRDS(src_file)
  if (is.data.frame(dat)) {
    dat_sub <- dplyr::filter(dat, !!rlang::enquo(filter_expr))
  } else {
    dat_sub <- dat  # non-data-frame objects (e.g., threshold lists) kept whole
  }
  saveRDS(dat_sub, dest_file)
  cat(sprintf("  %-35s  %d -> %d rows  (%.2f MB)\n",
              label,
              if (is.data.frame(dat)) nrow(dat) else NA,
              if (is.data.frame(dat_sub)) nrow(dat_sub) else NA,
              file.info(dest_file)$size / 1e6))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(rlang)
})

# fishing_metrics_raw & climate_only_metrics_raw
for (nm in c("fishing_metrics_raw", "climate_only_metrics_raw", "b0_reference")) {
  src  <- file.path(METRICS_DIR, paste0(nm, ".rds"))
  dest <- file.path(OUT_METRICS,  paste0(nm, ".rds"))
  if (!file.exists(src)) { cat(sprintf("  SKIP: %s\n", nm)); next }
  dat <- readRDS(src)
  dat_sub <- if (is.data.frame(dat)) filter(dat, sim_id %in% SUBSET_IDS) else dat
  saveRDS(dat_sub, dest)
  cat(sprintf("  %-35s  %d -> %d rows  (%.2f MB)\n",
              nm,
              if (is.data.frame(dat)) nrow(dat) else NA_integer_,
              if (is.data.frame(dat_sub)) nrow(dat_sub) else NA_integer_,
              file.info(dest)$size / 1e6))
}

# paired_data
src  <- file.path(METRICS_DIR, "paired_data.rds")
dest <- file.path(OUT_METRICS,  "paired_data.rds")
if (file.exists(src)) {
  dat <- readRDS(src)
  dat_sub <- filter(dat, sim_id %in% SUBSET_IDS)
  saveRDS(dat_sub, dest)
  cat(sprintf("  %-35s  %d -> %d rows  (%.2f MB)\n",
              "paired_data",
              nrow(dat), nrow(dat_sub), file.info(dest)$size / 1e6))
}

# empirical_thresholds: these are derived from B0 sims — re-derive from subset
# or just copy (they're a list of per-metric summary stats, sim-independent)
src  <- file.path(METRICS_DIR, "empirical_thresholds.rds")
dest <- file.path(OUT_METRICS,  "empirical_thresholds.rds")
if (file.exists(src)) {
  file.copy(src, dest, overwrite = TRUE)
  cat(sprintf("  %-35s  (copied — sim-independent structure, %.2f MB)\n",
              "empirical_thresholds", file.info(dest)$size / 1e6))
}

# ==============================================================================
# 4. SUMMARY README
# ==============================================================================
readme <- sprintf('# Example Subset Data (10 simulations)

Created: %s
Source:  Full 2111-simulation Prydz Bay mizer ensemble
Subset:  Simulations 1–10 (sim_id 1:10)

## Files

### fishing_ensemble/
- mc_ensemble_10_example.rds
  Fishing ensemble: list of 10 MizerSim objects (identical structure to
  mc_ensemble_2111_cleaned.rds, just 10 simulations).
  ENSEMBLE_PATHS$fishing should point here for example/testing use.

### climate_only_ensemble/
- climate_only_ensemble_10_example.rds
  Climate-only ensemble: list of 10 MizerSim objects (identical structure to
  climate_only_ensemble_compiled.rds, just 10 simulations).
  ENSEMBLE_PATHS$climate_only should point here for example/testing use.

### ecosystem_assessment/
Pre-computed metric files for the same 10 simulations. Structurally identical
to Output_large_files/ecosystem_assessment/:
- fishing_metrics_raw.rds      (sim_id 1–10 only)
- climate_only_metrics_raw.rds (sim_id 1–10 only)
- b0_reference.rds             (sim_id 1–10 only)
- paired_data.rds              (sim_id 1–10, 170 rows = 10 sims x 17 decades)
- empirical_thresholds.rds     (copied — sim-independent threshold list)

## Usage

To use in run_ecosystem_assessment_v2.R, change ENSEMBLE_PATHS to:
  ENSEMBLE_PATHS <- list(
    fishing      = "example_data/fishing_ensemble/mc_ensemble_10_example.rds",
    climate_only = "example_data/climate_only_ensemble/climate_only_ensemble_10_example.rds"
  )

Or in replot_existing_outputs.R, point data_dir to:
  data_dir <- "example_data/ecosystem_assessment"
', Sys.time())

writeLines(readme, file.path(OUT_BASE, "README.md"))

# ==============================================================================
# 5. VERIFY
# ==============================================================================
cat("\n=============================================================\n")
cat("VERIFICATION\n")
cat("=============================================================\n\n")

suppressPackageStartupMessages(library(mizer))

cat("Fishing ensemble subset:\n")
fish_check <- readRDS(file.path(OUT_FISHING, "mc_ensemble_10_example.rds"))
fish_sims  <- if ("simulations" %in% names(fish_check)) fish_check$simulations else fish_check
cat(sprintf("  Simulations: %d\n", length(fish_sims)))
cat(sprintf("  Class of sim 1: %s\n", class(fish_sims[[1]])))
cat(sprintf("  Time range: %s - %s\n",
            min(as.numeric(dimnames(fish_sims[[1]]@n)$time)),
            max(as.numeric(dimnames(fish_sims[[1]]@n)$time))))

cat("\nClimate-only ensemble subset:\n")
clim_check <- readRDS(file.path(OUT_CLIMATE, "climate_only_ensemble_10_example.rds"))
clim_sims  <- if ("simulations" %in% names(clim_check)) clim_check$simulations else clim_check
cat(sprintf("  Simulations: %d\n", length(clim_sims)))
cat(sprintf("  Class of sim 1: %s\n", class(clim_sims[[1]])))
cat(sprintf("  Time range: %s - %s\n",
            min(as.numeric(dimnames(clim_sims[[1]]@n)$time)),
            max(as.numeric(dimnames(clim_sims[[1]]@n)$time))))

cat("\nEcosystem assessment data:\n")
for (nm in c("fishing_metrics_raw", "climate_only_metrics_raw", "b0_reference", "paired_data")) {
  f <- file.path(OUT_METRICS, paste0(nm, ".rds"))
  if (file.exists(f)) {
    d <- readRDS(f)
    cat(sprintf("  %-35s  %d rows, %d sims\n",
                nm, nrow(d),
                if ("sim_id" %in% names(d)) length(unique(d$sim_id)) else NA))
  }
}

cat("\nAll output files:\n")
for (f in list.files(OUT_BASE, recursive = TRUE, full.names = TRUE)) {
  cat(sprintf("  %-60s  (%.2f MB)\n",
              gsub(paste0(OUT_BASE, "/"), "", f, fixed = TRUE),
              file.info(f)$size / 1e6))
}

cat("\n=============================================================\n")
cat("DONE — example_data/ is ready\n")
cat("=============================================================\n")
