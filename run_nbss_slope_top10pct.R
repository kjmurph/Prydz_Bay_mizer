# =============================================================================
# run_nbss_slope_top10pct.R
#
# Computes annual LBNbiom (NBSS) slopes for the top-10% RMSE ensemble members,
# both Exploited and Unexploited, covering all simulation years.
#
# Uses the LBNbiom method (Method 5, Edwards et al. 2017) for methodological
# consistency with ecosystem_assessment_v3.R (spectrum_lbnbiom_slope metric).
# This replaces getCommunitySlope(biomass=TRUE), which fits OLS on log(n*w)
# without octave re-binning or linear-width normalisation.
#
# Method summary (per sim, per year):
#   1. Compute total biomass per mizer bin: n(w) * w * dw
#   2. Aggregate into octave (log2) bins
#   3. Normalise by linear bin width: NBS = oct_biomass / (w_hi - w_lo)
#   4. Fit OLS: log10(NBS) ~ log10(w_geometric_mid)
#
# Reference:
#   Edwards et al. (2017). Accounting for the bin structure of data removes
#   bias when fitting size spectra. Methods Ecol. Evol. 8:57-67.
#
# Inputs:
#   yield_rmse_per_sim.csv
#   Output_large_files/monte_carlo_results/.../mc_ensemble_2111_cleaned.rds
#   Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds
#
# Output:
#   Output_large_files/community_slope_analysis/nbss_slope_top10pct_data.rds
#   Same $all_slopes structure as community_slope_full_2111_data.rds:
#     time, slope, intercept, r_squared, sim_id, ensemble, spectrum_type
# =============================================================================

suppressPackageStartupMessages({
  library(mizer)
  library(dplyr)
  library(purrr)
})

setwd("C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/Prydz_Bay_mizer")

t_start <- proc.time()
cat("=== Annual NBSS slope (LBNbiom, Edwards et al. 2017) — top-10% ===\n\n")

out_dir <- "Output_large_files/community_slope_analysis"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# 1. Load RMSE rankings — top-10% indices
# ---------------------------------------------------------------------------
rmse_df      <- read.csv("yield_rmse_per_sim.csv") %>% arrange(rank)
n_top10pct   <- ceiling(nrow(rmse_df) * 0.10)
top10pct_idx <- rmse_df$sim_index[1:n_top10pct]
cat(sprintf("Top-10%%: n = %d sims\n", n_top10pct))
cat(sprintf("  RMSE range: %.4f – %.4f\n\n",
            rmse_df$rmse[1], rmse_df$rmse[n_top10pct]))

# ---------------------------------------------------------------------------
# 2. LBNbiom helper — returns data.frame of annual slopes for one sim
#    Uses a grouping matrix to aggregate all years in one matrix multiply,
#    then loops only over time steps for the lm() calls.
# ---------------------------------------------------------------------------
SPECTRUM_MIN_W <- 3.16227766016838e-08  # matches ecosystem_assessment_v3.R

compute_annual_lbnbiom <- function(sim, ensemble_label, sim_id_val) {
  tryCatch({
    params     <- sim@params
    w          <- params@w
    dw         <- params@dw
    times_char <- dimnames(sim@n)$time
    times_num  <- as.numeric(sub("\\..*", "", times_char))

    # --- Octave (log2) bin definitions ---
    w_filt <- w[w >= SPECTRUM_MIN_W]
    if (length(w_filt) < 3) return(NULL)
    oct_lo     <- floor(log2(min(w_filt)))
    oct_hi     <- ceiling(log2(max(w_filt)))
    bin_breaks <- 2^(oct_lo:oct_hi)
    n_bins     <- length(bin_breaks) - 1
    if (n_bins < 3) return(NULL)

    bin_idx   <- findInterval(w, bin_breaks, rightmost.closed = TRUE)
    oct_width <- diff(bin_breaks)
    oct_mid   <- sqrt(bin_breaks[-length(bin_breaks)] * bin_breaks[-1])

    # Grouping matrix G [n_w x n_bins]:
    # G[j, b] = w[j]*dw[j] for mizer bins that map to octave bin b (>= min_w),
    # so that n_community %*% G gives total biomass per octave in one step.
    G <- matrix(0, nrow = length(w), ncol = n_bins)
    for (j in seq_along(w)) {
      b <- bin_idx[j]
      if (w[j] >= SPECTRUM_MIN_W && b >= 1 && b <= n_bins)
        G[j, b] <- w[j] * dw[j]
    }

    # --- Extract community n for ALL time steps at once ---
    # sim@n: [time x species x size] -> sum over species -> [time x size]
    n_all <- apply(sim@n, c(1, 3), sum)

    # Aggregate to octave bins via matrix multiply: [time x n_bins]
    oct_bm <- n_all %*% G

    # Normalise by linear octave width: [time x n_bins]
    nbs_all <- sweep(oct_bm, 2, oct_width, "/")

    # --- Fit OLS per time step ---
    n_times    <- nrow(nbs_all)
    slopes     <- numeric(n_times)
    intercepts <- numeric(n_times)
    r2s        <- numeric(n_times)
    log_mid    <- log10(oct_mid)

    for (t in seq_len(n_times)) {
      nbs_t <- nbs_all[t, ]
      valid <- which(nbs_t > 0)
      if (length(valid) < 3) {
        slopes[t] <- NA_real_; intercepts[t] <- NA_real_; r2s[t] <- NA_real_
        next
      }
      fit          <- lm(log10(nbs_t[valid]) ~ log_mid[valid])
      s            <- summary(fit)
      slopes[t]    <- unname(coef(fit)[2])
      intercepts[t] <- unname(coef(fit)[1])
      r2s[t]       <- s$r.squared
    }

    data.frame(
      time          = times_num,
      slope         = slopes,
      intercept     = intercepts,
      r_squared     = r2s,
      sim_id        = sim_id_val,
      ensemble      = ensemble_label,
      spectrum_type = "Biomass"
    )
  }, error = function(e) {
    warning(sprintf("sim_id %d (%s) failed: %s", sim_id_val, ensemble_label, e$message))
    NULL
  })
}

# ---------------------------------------------------------------------------
# 3. Process Exploited (fished) ensemble
# ---------------------------------------------------------------------------
cat("Loading Exploited ensemble...\n")
mc_path <- paste0("Output_large_files/monte_carlo_results/",
                  "combined_simulation_results/rerun_results/",
                  "mc_ensemble_2111_cleaned.rds")
mc      <- readRDS(mc_path)
mc_sims <- mc$simulations
cat(sprintf("  n = %d sims loaded.\n", length(mc_sims)))
cat(sprintf("  Processing %d top-10%% sims...\n", n_top10pct))

exploited_slopes <- map_dfr(seq_along(top10pct_idx), function(k) {
  i <- top10pct_idx[k]
  if (k %% 25 == 0 || k == 1 || k == n_top10pct)
    cat(sprintf("    Exploited %d/%d  (sim_index=%d)  %.1f min\n",
                k, n_top10pct, i,
                (proc.time() - t_start)["elapsed"] / 60))
  sim <- mc_sims[[i]]
  if (is.null(sim) || !inherits(sim, "MizerSim")) return(NULL)
  compute_annual_lbnbiom(sim, "Exploited", i)
})

rm(mc, mc_sims); gc()
cat(sprintf("  Exploited complete: %d records.\n\n",
            nrow(exploited_slopes)))

# ---------------------------------------------------------------------------
# 4. Process Unexploited (climate-only) ensemble
# ---------------------------------------------------------------------------
cat("Loading Unexploited ensemble...\n")
co_path <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
co      <- readRDS(co_path)
co_sims <- co$simulations
cat(sprintf("  n = %d sims loaded.\n", length(co_sims)))
cat(sprintf("  Processing %d top-10%% sims...\n", n_top10pct))

unexploited_slopes <- map_dfr(seq_along(top10pct_idx), function(k) {
  i <- top10pct_idx[k]
  if (k %% 25 == 0 || k == 1 || k == n_top10pct)
    cat(sprintf("    Unexploited %d/%d  (sim_index=%d)  %.1f min\n",
                k, n_top10pct, i,
                (proc.time() - t_start)["elapsed"] / 60))
  sim <- co_sims[[i]]
  if (is.null(sim) || !inherits(sim, "MizerSim")) return(NULL)
  compute_annual_lbnbiom(sim, "Unexploited", i)
})

rm(co, co_sims); gc()
cat(sprintf("  Unexploited complete: %d records.\n\n",
            nrow(unexploited_slopes)))

# ---------------------------------------------------------------------------
# 5. Combine, summarise and save
# ---------------------------------------------------------------------------
all_slopes <- bind_rows(exploited_slopes, unexploited_slopes)

cat(sprintf("Total records: %d\n", nrow(all_slopes)))
cat(sprintf("Year range:    %d – %d\n",
            min(all_slopes$time, na.rm = TRUE),
            max(all_slopes$time, na.rm = TRUE)))
cat(sprintf("Slope range:   %.4f – %.4f\n",
            min(all_slopes$slope, na.rm = TRUE),
            max(all_slopes$slope, na.rm = TRUE)))
cat(sprintf("Ensembles:     %s\n",
            paste(unique(all_slopes$ensemble), collapse = ", ")))

out_path <- file.path(out_dir, "nbss_slope_top10pct_data.rds")
saveRDS(
  list(
    all_slopes     = all_slopes,
    n_sims         = n_top10pct,
    top10pct_idx   = top10pct_idx,
    method         = "LBNbiom (Edwards et al. 2017, Method 5)",
    spectrum_min_w = SPECTRUM_MIN_W,
    run_date       = Sys.time()
  ),
  out_path
)
cat(sprintf("\nSaved: %s\n", out_path))

elapsed <- (proc.time() - t_start)["elapsed"]
cat(sprintf("\n=== Done. Total elapsed: %.1f minutes ===\n", elapsed / 60))
