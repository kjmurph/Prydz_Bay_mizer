###############################################################################
# Extract full contemporary diet: all 19 predators × all prey, 2001-2010
#
# Reuses getDiet-based extraction from extract_full_diet_consumption.R.
# Restricts to years 2001-2010 only for efficiency (~10 time steps per sim
# rather than the full 170-year record).
#
# For each simulation, computes the mean annual consumption (g yr-1) for
# every predator × prey combination over 2001-2010.  Results are saved as:
#   - Full per-sim data (RDS): predator × prey × sim_id → mean g yr-1
#   - Ensemble summary CSVs: median, q25, q75 across all sims
#
# Total Antarctic krill consumption by ALL predators is also reported.
#
# Outputs (in whale_consumption_outputs/):
#   full_diet_contemporary_fishing_all_sims.rds
#   full_diet_contemporary_climate_only_all_sims.rds
#   full_diet_contemporary_fishing_summary.csv
#   full_diet_contemporary_climate_only_summary.csv
#   krill_total_all_predators_2001_2010.csv
###############################################################################

suppressPackageStartupMessages({
  library(therMizer)
  library(dplyr)
})

###############################################################################
# Configuration
###############################################################################

YEAR_START <- 2001
YEAR_END   <- 2010

MODEL_DOMAIN_AREA <- 1.474341e+12   # m^2
G_TO_TONNES       <- 1e-6
G_TO_MT           <- 1e-12

OUTPUT_DIR <- "whale_consumption_outputs"

MC_RESULTS_PATHS <- list(
  fishing      = "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds",
  climate_only = "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
)

###############################################################################
# Core extraction functions (from extract_full_diet_consumption.R)
###############################################################################

#' Extract full consumption matrix at a single time step.
#' Returns total population consumption (g/yr) for each predator-prey pair.
extract_consumption_matrix <- function(sim, t_idx) {
  params <- sim@params
  dw     <- params@dw

  n <- sim@n[t_idx, , , drop = FALSE]
  dim(n)      <- dim(sim@n)[2:3]
  dimnames(n) <- dimnames(sim@n)[2:3]

  n_pp <- sim@n_pp[t_idx, ]

  n_other <- sim@n_other[t_idx, ]
  if (!is.null(dimnames(sim@n_other))) {
    names(n_other) <- dimnames(sim@n_other)$component
  }

  diet <- tryCatch(
    getDiet(params, n = n, n_pp = n_pp, n_other = n_other, proportion = FALSE),
    error = function(e) NULL
  )
  if (is.null(diet)) return(NULL)

  predators  <- dimnames(diet)$predator
  prey_names <- dimnames(diet)[[3]]

  consumption_matrix <- matrix(
    0,
    nrow     = length(predators),
    ncol     = length(prey_names),
    dimnames = list(predator = predators, prey = prey_names)
  )

  for (pred_idx in seq_along(predators)) {
    n_pred <- n[predators[pred_idx], ]
    for (prey_idx in seq_along(prey_names)) {
      # consumption_rate[size] * abundance[size] * dw -> total g/year
      consumption_matrix[pred_idx, prey_idx] <-
        sum(diet[pred_idx, , prey_idx] * n_pred * dw)
    }
  }

  return(consumption_matrix)
}

###############################################################################
# Per-simulation extraction: mean over 2001-2010
###############################################################################

#' For one MizerSim, return a predator × prey matrix of mean annual
#' consumption (g yr-1) averaged over YEAR_START:YEAR_END.
extract_contemporary_mean <- function(sim, year_start = YEAR_START,
                                      year_end = YEAR_END) {
  times   <- as.numeric(dimnames(sim@n)$time)
  t_idx_v <- which(times >= year_start & times <= year_end)

  if (length(t_idx_v) == 0) {
    warning("No time steps found in contemporary period")
    return(NULL)
  }

  mat_list <- lapply(t_idx_v, function(ti) {
    extract_consumption_matrix(sim, ti)
  })
  mat_list <- Filter(Negate(is.null), mat_list)
  if (length(mat_list) == 0) return(NULL)

  # Mean across time steps
  mat_sum <- Reduce("+", mat_list)
  mat_mean <- mat_sum / length(mat_list)
  return(mat_mean)
}

###############################################################################
# Process one ensemble
###############################################################################

process_ensemble_contemporary <- function(ensemble_file, scenario_label,
                                          output_prefix) {
  cat(sprintf("\n=== %s ===\n", scenario_label))

  if (!file.exists(ensemble_file)) {
    cat("ERROR: file not found:", ensemble_file, "\n")
    return(NULL)
  }

  cat("Loading ensemble...\n")
  mc <- readRDS(ensemble_file)

  # Handle both possible structures
  sims <- if ("simulations" %in% names(mc)) mc$simulations
          else if (is.list(mc) && inherits(mc[[1]], "MizerSim")) mc
          else stop("Unrecognised ensemble structure")

  n_sims <- length(sims)
  cat(sprintf("  %d simulations loaded\n", n_sims))

  # Verify time coverage
  test_times <- as.numeric(dimnames(sims[[1]]@n)$time)
  n_period   <- sum(test_times >= YEAR_START & test_times <= YEAR_END)
  cat(sprintf("  Time steps in %d-%d: %d\n\n", YEAR_START, YEAR_END, n_period))

  # Storage: list of predator × prey matrices, one per sim
  result_mats <- vector("list", n_sims)

  pb      <- txtProgressBar(min = 0, max = n_sims, style = 3)
  skipped <- 0

  for (i in seq_len(n_sims)) {
    setTxtProgressBar(pb, i)

    sim <- tryCatch(sims[[i]], error = function(e) NULL)
    if (is.null(sim)) { skipped <- skipped + 1; next }

    mat <- tryCatch(
      extract_contemporary_mean(sim),
      error = function(e) NULL
    )

    if (is.null(mat)) { skipped <- skipped + 1; next }
    result_mats[[i]] <- mat
  }
  close(pb)

  result_mats <- Filter(Negate(is.null), result_mats)
  cat(sprintf("\n  Extracted %d simulations (skipped %d)\n",
              length(result_mats), skipped))

  # -------------------------------------------------------------------------
  # Save raw per-sim list
  # -------------------------------------------------------------------------
  rds_path <- file.path(OUTPUT_DIR,
                         sprintf("full_diet_contemporary_%s_all_sims.rds", output_prefix))
  saveRDS(result_mats, rds_path)
  cat(sprintf("  Saved per-sim RDS: %s\n", basename(rds_path)))

  # -------------------------------------------------------------------------
  # Ensemble statistics: for each predator × prey cell
  # -------------------------------------------------------------------------
  # Stack into a 3D array: predator × prey × sim
  preds     <- rownames(result_mats[[1]])
  preys     <- colnames(result_mats[[1]])
  n_pred    <- length(preds)
  n_prey    <- length(preys)
  n_valid   <- length(result_mats)

  cube <- array(
    NA_real_,
    dim      = c(n_pred, n_prey, n_valid),
    dimnames = list(predator = preds, prey = preys, sim = seq_len(n_valid))
  )
  for (i in seq_len(n_valid)) cube[, , i] <- result_mats[[i]]

  # Build summary long-format data frame
  rows <- expand.grid(predator = preds, prey = preys, stringsAsFactors = FALSE)
  rows$median_g_yr  <- NA_real_
  rows$q25_g_yr     <- NA_real_
  rows$q75_g_yr     <- NA_real_
  rows$q05_g_yr     <- NA_real_
  rows$q95_g_yr     <- NA_real_
  rows$mean_g_yr    <- NA_real_

  for (k in seq_len(nrow(rows))) {
    vals <- cube[rows$predator[k], rows$prey[k], ]
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) next
    rows$median_g_yr[k] <- median(vals)
    rows$mean_g_yr[k]   <- mean(vals)
    rows$q25_g_yr[k]    <- quantile(vals, 0.25)
    rows$q75_g_yr[k]    <- quantile(vals, 0.75)
    rows$q05_g_yr[k]    <- quantile(vals, 0.05)
    rows$q95_g_yr[k]    <- quantile(vals, 0.95)
  }

  rows$median_tonnes <- rows$median_g_yr * G_TO_TONNES
  rows$q25_tonnes    <- rows$q25_g_yr    * G_TO_TONNES
  rows$q75_tonnes    <- rows$q75_g_yr    * G_TO_TONNES
  rows$scenario      <- scenario_label

  csv_path <- file.path(OUTPUT_DIR,
                         sprintf("full_diet_contemporary_%s_summary.csv", output_prefix))
  write.csv(rows, csv_path, row.names = FALSE)
  cat(sprintf("  Saved summary CSV: %s\n", basename(csv_path)))

  return(list(cube = cube, summary = rows,
              predators = preds, prey = preys))
}

###############################################################################
# Main
###############################################################################

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

cat("=============================================================\n")
cat(sprintf("Contemporary Diet Extraction: %d-%d\n", YEAR_START, YEAR_END))
cat("=============================================================\n")

fish_res  <- process_ensemble_contemporary(
  MC_RESULTS_PATHS$fishing,
  "Exploited",
  "fishing"
)

clim_res <- process_ensemble_contemporary(
  MC_RESULTS_PATHS$climate_only,
  "Unexploited",
  "climate_only"
)

###############################################################################
# Report: total Antarctic krill consumed by ALL predators
###############################################################################

cat("\n=============================================================\n")
cat("Total Antarctic krill consumed by ALL predators, 2001-2010\n")
cat("=============================================================\n\n")

report_krill_total <- function(res, label) {
  if (is.null(res)) return(NULL)

  prey_name <- "antarctic krill"
  if (!prey_name %in% res$prey) {
    cat("Antarctic krill not found in prey list\n")
    return(NULL)
  }

  # Sum across ALL predators for each simulation
  n_valid  <- dim(res$cube)[3]
  totals   <- numeric(n_valid)
  for (i in seq_len(n_valid)) {
    totals[i] <- sum(res$cube[, prey_name, i], na.rm = TRUE)
  }

  med_Mt <- median(totals) * G_TO_MT
  q25_Mt <- quantile(totals, 0.25) * G_TO_MT
  q75_Mt <- quantile(totals, 0.75) * G_TO_MT
  cat(sprintf("%s:\n  Median = %.3f Mt yr-1  IQR [%.3f - %.3f]\n",
              label, med_Mt, q25_Mt, q75_Mt))

  # Per-predator breakdown
  preds   <- res$predators
  pp_med  <- vapply(preds, function(p)
    median(res$cube[p, prey_name, ], na.rm = TRUE) * G_TO_MT, numeric(1))
  pp_pct  <- pp_med / sum(pp_med) * 100

  cat("  By predator (median Mt yr-1 | % of total):\n")
  ord <- order(pp_med, decreasing = TRUE)
  for (j in ord) {
    cat(sprintf("    %-30s  %7.4f Mt yr-1  (%5.1f%%)\n",
                preds[j], pp_med[j], pp_pct[j]))
  }
  cat("\n")

  return(data.frame(
    predator   = preds,
    median_Mt  = pp_med,
    q25_Mt     = vapply(preds, function(p)
      quantile(res$cube[p, prey_name, ], 0.25, na.rm = TRUE) * G_TO_MT, numeric(1)),
    q75_Mt     = vapply(preds, function(p)
      quantile(res$cube[p, prey_name, ], 0.75, na.rm = TRUE) * G_TO_MT, numeric(1)),
    pct_of_total = pp_pct,
    scenario   = label
  ))
}

krill_fish <- report_krill_total(fish_res,  "Exploited (fishing)")
krill_clim <- report_krill_total(clim_res, "Unexploited (climate-only)")

# Combined table
if (!is.null(krill_fish) && !is.null(krill_clim)) {
  krill_combined <- rbind(krill_fish, krill_clim)
  out_csv <- file.path(OUTPUT_DIR, "krill_total_all_predators_2001_2010.csv")
  write.csv(krill_combined, out_csv, row.names = FALSE)
  cat(sprintf("Saved: %s\n", basename(out_csv)))
}

cat("\nDone.\n")
