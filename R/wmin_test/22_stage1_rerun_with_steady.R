# =============================================================================
# STAGE 1, definitive run: full workflow protocol including steady().
#
#     params -> steady() -> 118 yr unfished spin-up -> stability -> projection
#
# 19_stage1_rerun.R omitted steady() on the reasoning that re-tuning would mask
# the w_min effect. 21_diagnose_extinction.R showed that reasoning was wrong:
# R_max and erepro are calibrated against w_min = 0.001 g, where recruitment is
# clamped by R_max at 10,000x saturation. Moving w_min to the 2942 g grid point
# divides RDI by 3.8e6, releasing the clamp, cutting adult recruitment ~20-40x
# and driving penguins extinct. Without steady() the treated model is simply
# uncalibrated, so the comparison would be meaningless.
#
# steady() is therefore part of the protocol, exactly as 09_Uncertainty_Analysis
# .Rmd runs it, and is applied identically to control and treated.
#
# Writes Output_large_files/wmin_test/22_stage1_steady_sims.rds
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer)
  library(mizer)
})

out_dir <- "Output_large_files/wmin_test"
W_NEW <- 3626.667
PEN <- "small divers"
SPINUP_YEARS <- 118
# Workflow values, 09_Uncertainty_Analysis.Rmd:1585-1590 (the DRIVER values, not
# the function defaults of tol = 0.002 / t_max = 2000 that the brief quotes):
#   enhanced_steady_tol   <- 0.0025
#   enhanced_steady_t_max <- 1500
#   preserve_erepro       <- TRUE   -> steady(..., preserve = c("erepro"))
STEADY_TMAX <- 1500
STEADY_PRESERVE <- c("erepro")
# Approach the workflow tolerance through a ladder rather than in one jump. The
# treated params start much further from steady state than the control (the
# penguin size range collapses from 44 bins to 2), so a single strict call is
# where convergence is most likely to fail. Each rung starts from the previous
# rung's output; the final rung is the workflow value. Applied identically to
# control and treated so the comparison stays fair.
STEADY_TOL_LADDER <- c(0.1, 0.05, 0.01, 0.0025)
N_MEMBERS <- 5

STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

t0 <- proc.time()
cat("=== Stage 1 (with steady) started:", format(Sys.time()), "===\n")

ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
idx <- ens$metadata$top10pct_indices

check_stability <- function(sim_object) {
  bm <- getBiomass(sim_object)
  yrs <- as.numeric(rownames(bm)); nT <- nrow(bm)
  if (nT < 5) return(list(stable = FALSE, max_cv = NA))
  tail_mat <- tail(bm, max(5, min(STAB$check_years_tail, nT)))
  head_n <- max(5, min(STAB$trend_first_years, nT))
  head_mat <- head(bm, head_n); head_years <- head(yrs, head_n)
  mean_tail <- colMeans(tail_mat, na.rm = TRUE)
  cv_tail <- ifelse(mean_tail > 0, apply(tail_mat, 2, sd, na.rm = TRUE) / mean_tail, Inf)
  rel_slope <- setNames(rep(NA_real_, ncol(head_mat)), colnames(head_mat)); pvals <- rel_slope
  for (j in seq_len(ncol(head_mat))) {
    y <- head_mat[, j]; mu <- mean(y, na.rm = TRUE)
    if (!is.finite(mu) || mu < STAB$min_mean_biomass) { rel_slope[j] <- 0; pvals[j] <- 1; next }
    fit <- try(suppressWarnings(lm(b ~ t, data = data.frame(t = head_years, b = as.numeric(y)))),
               silent = TRUE)
    if (inherits(fit, "try-error")) { rel_slope[j] <- 0; pvals[j] <- 1 } else {
      rel_slope[j] <- as.numeric(coef(fit)[["t"]]) / mu
      pv <- try(summary(fit)$coefficients["t", "Pr(>|t|)"], silent = TRUE)
      pvals[j] <- if (inherits(pv, "try-error")) 1 else as.numeric(pv)
    }
  }
  fail_cv <- cv_tail > STAB$cv_threshold
  fail_trend <- (abs(rel_slope) > STAB$trend_rel_slope_max) & (pvals < STAB$trend_pval_max)
  list(stable = !any(fail_cv | fail_trend, na.rm = TRUE),
       fail_cv = names(fail_cv)[which(fail_cv)],
       fail_trend = names(fail_trend)[which(fail_trend)],
       max_cv = max(cv_tail[is.finite(cv_tail)], na.rm = TRUE))
}

set_wmin <- function(p, species, w_new) {
  rf <- p@rates_funcs; rd <- p@resource_dynamics
  i <- which(p@species_params$species == species)
  sp <- p@species_params; sp$w_min[i] <- w_new; p@species_params <- sp
  p <- suppressWarnings(setParams(p))
  ther <- intersect(c("Encounter", "PredRate", "EReproAndGrowth"), names(rf))
  p@rates_funcs[ther] <- rf[ther]
  p@resource_dynamics <- rd
  p@initial_n[i, p@w < w_new] <- 0
  stopifnot(p@w_min_idx[[i]] == max(which(p@w <= w_new)))
  p
}

# steady() up the tolerance ladder. mizer signals non-convergence with a warning
# rather than an error, and the workflow treats "did not converge" as a failure,
# so warnings are captured explicitly instead of being suppressed.
steady_ladder <- function(p, label) {
  converged <- rep(NA, length(STEADY_TOL_LADDER))
  for (k in seq_along(STEADY_TOL_LADDER)) {
    tol <- STEADY_TOL_LADDER[k]
    warned <- FALSE
    out <- withCallingHandlers(
      try(steady(p, tol = tol, t_max = STEADY_TMAX, preserve = STEADY_PRESERVE),
          silent = TRUE),
      warning = function(w) {
        if (grepl("did not converge", conditionMessage(w), ignore.case = TRUE))
          warned <<- TRUE
        invokeRestart("muffleWarning")
      })
    if (inherits(out, "try-error")) {
      cat(sprintf("      %s tol=%.4g ERROR: %s", label, tol, as.character(out)))
      return(list(params = NULL, converged = converged, failed_at = tol))
    }
    converged[k] <- !warned
    p <- out
    cat(sprintf("      %s tol=%-7.4g %s\n", label, tol,
                if (warned) "did NOT converge" else "converged"))
  }
  list(params = p, converged = converged, failed_at = NA)
}

run_condition <- function(p, eff_f, eff_c, label) {
  sl <- steady_ladder(p, label)
  ps <- sl$params
  if (is.null(ps)) {
    cat("    ", label, "steady() FAILED\n")
    return(NULL)
  }
  i <- which(p@species_params$species == PEN)
  cat(sprintf("    %-8s steady ok | R_max %.4e -> %.4e | erepro %.4e -> %.4e\n",
              label, p@species_params$R_max[i], ps@species_params$R_max[i],
              p@species_params$erepro[i], ps@species_params$erepro[i]))
  spin <- project(ps, t_start = 1841, t_max = SPINUP_YEARS, effort = 0)
  stab <- check_stability(spin)
  n_init <- spin@n[SPINUP_YEARS, , ]
  sim_f <- project(ps, initial_n = n_init, t_start = 1841, effort = eff_f)
  sim_c <- project(ps, initial_n = n_init, t_start = 1841, effort = eff_c)
  cat(sprintf("             stable=%-5s max_cv=%.3f fail=[%s]\n", stab$stable,
              stab$max_cv,
              paste(unique(c(stab$fail_cv, stab$fail_trend)), collapse = ",")))
  list(params = ps, spin = spin, stab = stab, fished = sim_f, climate = sim_c,
       steady_converged = sl$converged, steady_tol_ladder = STEADY_TOL_LADDER)
}

results <- list()
for (m in seq_len(N_MEMBERS)) {
  cat("--- member", m, "(sim_index", idx[m], ") ---\n")
  sf <- ens$fished_top10pct[[m]]; sc <- ens$climate_top10pct[[m]]
  p <- sf@params
  ctrl <- run_condition(p, sf@effort, sc@effort, "control")
  trt  <- run_condition(set_wmin(p, PEN, W_NEW), sf@effort, sc@effort, "treated")
  results[[m]] <- list(member = m, sim_index = idx[m], control = ctrl,
                       treated = trt, stored_fished = sf, stored_climate = sc)
  cat("    elapsed", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
}

saveRDS(results, file.path(out_dir, "22_stage1_steady_sims.rds"))
cat("\nSaved. elapsed:", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
