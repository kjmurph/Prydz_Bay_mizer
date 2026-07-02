###############################################################################
# extract_climate_only_spectrum_by_year.R
#
# One-time extraction to support the SNR-consistent (time-resolved) +/-1 SD
# reference on the size-spectrum abundance-ratio panel.
#
# Builds the ENSEMBLE-MEAN unexploited (climate-only) community size spectrum for
# each YEAR over the stationary baseline (1841-2010), across the top-10% RMSE
# matched members. The temporal SD of this trajectory (per body-mass bin) is the
# canonical detection "noise" (cf. biomass_slope_snr_mean_med.R), replacing the
# inter-sim CV previously used on the ratio panel.
#
# Input  : Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds
#          yield_rmse_per_sim.csv                       (top-10% indices)
# Output : ecosystem_assessment_outputs/abundance_ratio_plots/
#              climate_only_community_spectrum_by_year_top10pct.rds
#          Manuscript data/climate_only_community_spectrum_by_year_top10pct.rds
#          (list: years [yr], w_bins [w], Nbar_clim [yr x w], n_members)
#
# The heavy 1.9 GB ensemble is read once; only the small (yr x w) mean matrix is
# retained/cached.
###############################################################################

suppressPackageStartupMessages({
  library(mizer)
})

BASELINE_YEARS <- 1841:2010

out_dir  <- "ecosystem_assessment_outputs/abundance_ratio_plots"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_root <- file.path(out_dir, "climate_only_community_spectrum_by_year_top10pct.rds")
out_ms   <- file.path("Manuscript data",
                      "climate_only_community_spectrum_by_year_top10pct.rds")

climate_file <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
rmse_path    <- "yield_rmse_per_sim.csv"

# ---- top-10% indices (same definition as the ratio-panel script) ----
rmse_df    <- read.csv(rmse_path)
rmse_df    <- rmse_df[order(rmse_df$rank), ]
n_top10pct <- ceiling(nrow(rmse_df) * 0.10)
top_idx    <- rmse_df$sim_index[seq_len(n_top10pct)]
cat(sprintf("Top-10%% members: %d (sim_index %d..%d)\n",
            length(top_idx), min(top_idx), max(top_idx)))

# ---- load climate-only ensemble ----
cat("Loading climate-only ensemble (~1.9 GB)...\n")
climate_ensemble <- readRDS(climate_file)
sims    <- climate_ensemble$simulations
n_clim  <- length(sims)
cat(sprintf("  %d climate-only simulations available.\n", n_clim))

# w grid from the first non-null sim
first_sim <- NULL
for (s in sims) if (!is.null(s)) { first_sim <- s; break }
if (is.null(first_sim)) stop("No valid climate-only simulations found.")
w_bins <- first_sim@params@w
n_w    <- length(w_bins)
rm(first_sim)

# ---- accumulate per-year community spectrum over members ----
top_idx <- top_idx[top_idx <= n_clim]
years_ref <- NULL
Nsum      <- NULL
cnt       <- 0L

cat("Extracting per-year community spectra...\n")
pb <- txtProgressBar(min = 0, max = length(top_idx), style = 3)
for (k in seq_along(top_idx)) {
  setTxtProgressBar(pb, k)
  sim <- tryCatch(sims[[top_idx[k]]], error = function(e) NULL)
  if (is.null(sim)) next
  st <- as.numeric(dimnames(sim@n)$time)
  if (is.null(years_ref)) years_ref <- intersect(BASELINE_YEARS, st)
  idx <- match(years_ref, st)
  if (any(is.na(idx))) next
  ns <- sim@n[idx, , , drop = FALSE]                 # [year x species x size]
  cs <- apply(ns, c(1, 3), sum, na.rm = TRUE)        # [year x size] (sum species)
  if (ncol(cs) != n_w) next
  if (is.null(Nsum)) Nsum <- matrix(0, nrow = length(years_ref), ncol = n_w)
  Nsum <- Nsum + cs
  cnt  <- cnt + 1L
}
close(pb)
rm(climate_ensemble, sims); gc()

if (cnt == 0) stop("No members contributed — check indices / time grid.")
Nbar_clim <- Nsum / cnt
dimnames(Nbar_clim) <- list(year = years_ref, w = NULL)

cat(sprintf("  Averaged %d members over years %d-%d (%d bins).\n",
            cnt, min(years_ref), max(years_ref), n_w))

res <- list(years = years_ref, w_bins = w_bins,
            Nbar_clim = Nbar_clim, n_members = cnt)
saveRDS(res, out_root)
dir.create("Manuscript data", showWarnings = FALSE)
saveRDS(res, out_ms)
cat(sprintf("Saved:\n  %s\n  %s\n", out_root, out_ms))
cat("=== Done ===\n")