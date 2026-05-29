###############################################################################
# extract_full_diet_top10pct_timeseries.R
#
# Extracts the full predator × prey annual diet consumption for every
# simulation year (1841–2010) for the top-10% RMSE-filtered ensemble sims
# (~211 sims), for both the fishing and matched climate-only ensembles.
#
# For each sim, stores a named 3-D array [year × predator × prey] of total
# population-level consumption (g yr-1, domain-wide).
#
# Follows the same getDiet-based integration as extract_contemporary_diet_2001_2010.R:
#   consumption[pred, prey] = sum_w( diet[pred, w, prey] * n[pred, w] * dw )
#
# Caches the per-sim raw arrays on first run; subsequent runs load from cache.
#
# Outputs (in whale_consumption_outputs/):
#   full_diet_top10pct_fishing_all_sims.rds         — list of ~211 arrays [yr×pred×prey]
#   full_diet_top10pct_climate_only_all_sims.rds
#   full_diet_top10pct_fishing_summary.csv           — year×pred×prey ensemble stats
#   full_diet_top10pct_climate_only_summary.csv
###############################################################################

suppressPackageStartupMessages({
  library(therMizer)
  library(dplyr)
})

###############################################################################
# Configuration
###############################################################################

OUTPUT_DIR        <- "whale_consumption_outputs"
G_TO_TONNES       <- 1e-6
G_TO_MT           <- 1e-12

MC_PATH   <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
CLIM_PATH <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

###############################################################################
# Load RMSE rankings and select top-10% indices
###############################################################################
if (!file.exists("yield_rmse_per_sim.csv"))
  stop("yield_rmse_per_sim.csv not found — run yield_rmse_evaluation.R first.")

rmse_df      <- read.csv("yield_rmse_per_sim.csv")
rmse_df      <- rmse_df[order(rmse_df$rank), ]
n_sims_all   <- nrow(rmse_df)
n_top10pct   <- ceiling(n_sims_all * 0.10)
top10pct_idx <- rmse_df$sim_index[1:n_top10pct]

message(sprintf("Top-10%% subset: %d sims (RMSE %.4f – %.4f)",
                n_top10pct, rmse_df$rmse[1], rmse_df$rmse[n_top10pct]))

###############################################################################
# Core: extract one time-step consumption matrix
# (identical to extract_contemporary_diet_2001_2010.R)
###############################################################################
extract_consumption_matrix <- function(sim, t_idx) {
  params <- sim@params
  dw     <- params@dw

  n <- sim@n[t_idx, , , drop = FALSE]
  dim(n)      <- dim(sim@n)[2:3]
  dimnames(n) <- dimnames(sim@n)[2:3]

  n_pp    <- sim@n_pp[t_idx, ]
  n_other <- sim@n_other[t_idx, ]
  if (!is.null(dimnames(sim@n_other)))
    names(n_other) <- dimnames(sim@n_other)$component

  diet <- tryCatch(
    getDiet(params, n = n, n_pp = n_pp, n_other = n_other, proportion = FALSE),
    error = function(e) NULL
  )
  if (is.null(diet)) return(NULL)

  predators  <- dimnames(diet)$predator
  prey_names <- dimnames(diet)[[3]]

  cons_mat <- matrix(
    0,
    nrow     = length(predators),
    ncol     = length(prey_names),
    dimnames = list(predator = predators, prey = prey_names)
  )

  for (pred_idx in seq_along(predators)) {
    n_pred <- n[predators[pred_idx], ]
    # Vectorised over prey for this predator
    cons_mat[pred_idx, ] <- colSums(
      diet[pred_idx, , ] * (n_pred * dw)
    )
  }

  cons_mat
}

###############################################################################
# Extract full annual timeseries for one sim
# Returns a 3-D array [year × predator × prey]
###############################################################################
extract_full_timeseries <- function(sim) {
  times <- as.numeric(dimnames(sim@n)$time)

  # Get pred/prey names from the first time step
  first_mat <- extract_consumption_matrix(sim, 1L)
  if (is.null(first_mat)) return(NULL)

  preds <- rownames(first_mat)
  preys <- colnames(first_mat)

  result <- array(
    NA_real_,
    dim      = c(length(times), length(preds), length(preys)),
    dimnames = list(year = times, predator = preds, prey = preys)
  )

  for (t_idx in seq_along(times)) {
    mat <- tryCatch(extract_consumption_matrix(sim, t_idx), error = function(e) NULL)
    if (!is.null(mat)) result[t_idx, , ] <- mat
  }

  result
}

###############################################################################
# Process one ensemble scenario: extract top-10% sims, cache to RDS
###############################################################################
process_scenario <- function(ensemble_path, subset_idx, scenario_label,
                             cache_rds) {

  if (file.exists(cache_rds)) {
    message(sprintf("[%s] Loading cached raw arrays from %s",
                    scenario_label, basename(cache_rds)))
    return(readRDS(cache_rds))
  }

  message(sprintf("[%s] Loading ensemble from %s", scenario_label,
                  basename(ensemble_path)))
  mc   <- readRDS(ensemble_path)
  sims <- if ("simulations" %in% names(mc)) mc$simulations
          else if (is.list(mc) && inherits(mc[[1]], "MizerSim")) mc
          else stop("Unrecognised ensemble structure")
  message(sprintf("  %d simulations in ensemble", length(sims)))

  # Report time coverage
  test_times <- as.numeric(dimnames(sims[[1]]@n)$time)
  message(sprintf("  Year range: %d – %d (%d time steps)",
                  min(test_times), max(test_times), length(test_times)))

  n_subset <- length(subset_idx)
  message(sprintf("[%s] Extracting full timeseries for %d sims...",
                  scenario_label, n_subset))

  out_list <- vector("list", n_subset)
  skipped  <- 0L

  for (k in seq_len(n_subset)) {
    idx <- subset_idx[k]

    if (k %% 20 == 0 || k == 1)
      message(sprintf("  Sim %d / %d (ensemble index %d)",
                      k, n_subset, idx))

    arr <- tryCatch(
      extract_full_timeseries(sims[[idx]]),
      error = function(e) { message("  ERROR sim ", idx, ": ", e$message); NULL }
    )

    if (is.null(arr)) { skipped <- skipped + 1L; next }
    out_list[[k]] <- arr
  }

  out_list <- Filter(Negate(is.null), out_list)
  message(sprintf("[%s] Extracted %d / %d sims (%d skipped)",
                  scenario_label, length(out_list), n_subset, skipped))

  saveRDS(out_list, cache_rds)
  message(sprintf("[%s] Saved: %s", scenario_label, cache_rds))

  rm(mc, sims)
  gc()

  out_list
}

###############################################################################
# Build ensemble summary: median + IQR across sims for each year × pred × prey
###############################################################################
build_summary_csv <- function(sim_arrays, scenario_label, csv_path) {
  message(sprintf("[%s] Building ensemble summary...", scenario_label))

  n_sims  <- length(sim_arrays)
  dn      <- dimnames(sim_arrays[[1]])
  years   <- as.numeric(dn$year)
  preds   <- dn$predator
  preys   <- dn$prey

  n_yr   <- length(years)
  n_pred <- length(preds)
  n_prey <- length(preys)

  # Stack into 4-D array [sim × year × pred × prey]
  message("  Stacking arrays...")
  stacked <- array(NA_real_, dim = c(n_sims, n_yr, n_pred, n_prey))
  for (i in seq_len(n_sims)) stacked[i, , , ] <- sim_arrays[[i]]

  # Pre-allocate output rows
  total_rows <- n_yr * n_pred * n_prey
  message(sprintf("  Computing quantiles across %d sims for %d cells...",
                  n_sims, total_rows))

  out_df <- data.frame(
    year        = integer(total_rows),
    predator    = character(total_rows),
    prey        = character(total_rows),
    median_g_yr = numeric(total_rows),
    q25_g_yr    = numeric(total_rows),
    q75_g_yr    = numeric(total_rows),
    q05_g_yr    = numeric(total_rows),
    q95_g_yr    = numeric(total_rows),
    scenario    = scenario_label,
    stringsAsFactors = FALSE
  )

  r <- 1L
  for (yi in seq_len(n_yr)) {
    for (pi in seq_len(n_pred)) {
      for (qi in seq_len(n_prey)) {
        vals <- stacked[, yi, pi, qi]
        vals <- vals[is.finite(vals)]
        out_df$year[r]        <- years[yi]
        out_df$predator[r]    <- preds[pi]
        out_df$prey[r]        <- preys[qi]
        if (length(vals) > 0) {
          out_df$median_g_yr[r] <- median(vals)
          out_df$q25_g_yr[r]    <- quantile(vals, 0.25)
          out_df$q75_g_yr[r]    <- quantile(vals, 0.75)
          out_df$q05_g_yr[r]    <- quantile(vals, 0.05)
          out_df$q95_g_yr[r]    <- quantile(vals, 0.95)
        }
        r <- r + 1L
      }
    }
    if (yi %% 20 == 0) message(sprintf("  ... year %d / %d done", yi, n_yr))
  }

  write.csv(out_df, csv_path, row.names = FALSE)
  message(sprintf("[%s] Saved summary: %s (%d rows)", scenario_label,
                  basename(csv_path), nrow(out_df)))
  invisible(out_df)
}

###############################################################################
# Main
###############################################################################

message("=============================================================")
message(sprintf("Full timeseries diet extraction — top-10%% RMSE sims (n=%d)",
                n_top10pct))
message("=============================================================\n")

# --- Fishing ---
fish_cache <- file.path(OUTPUT_DIR, "full_diet_top10pct_fishing_all_sims.rds")
fish_arrays <- process_scenario(MC_PATH, top10pct_idx, "fishing", fish_cache)

# --- Climate-only ---
clim_cache <- file.path(OUTPUT_DIR, "full_diet_top10pct_climate_only_all_sims.rds")
clim_arrays <- process_scenario(CLIM_PATH, top10pct_idx, "climate_only", clim_cache)

# --- Summaries ---
message("")
build_summary_csv(
  fish_arrays, "fishing",
  file.path(OUTPUT_DIR, "full_diet_top10pct_fishing_summary.csv")
)

build_summary_csv(
  clim_arrays, "climate_only",
  file.path(OUTPUT_DIR, "full_diet_top10pct_climate_only_summary.csv")
)

message("\n=== Done ===")
