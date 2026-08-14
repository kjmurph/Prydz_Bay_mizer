# =============================================================================
# STAGE 1 -- re-run the top 5 members by RMSE with small divers' w_min corrected
# from 0.001 g to 3626.667 g, both scenarios, with the spin-up re-run.
#
# Validated prerequisite: 17_validate_rerun.R shows that re-projecting an
# unmodified member from sim@params with its stored effort reproduces the stored
# trajectory to ~3e-15 in biomass, using therMizer 1.0.0 + mizer 3.1.0.
#
# DESIGN. sim@params@initial_n is the POST-spin-up state (verified identical to
# sim@n[1,,]); the pre-spin-up, post-steady() state was not saved with the
# ensembles and cannot be recovered. So CONTROL and TREATMENT are both put
# through an identical freshly-run pipeline
#
#     params -> 118 yr unfished spin-up -> stability check -> 170 yr projection
#
# and compared against each other. The stored ensemble is also carried through
# as a third reference, so the effect of restarting the spin-up is visible
# separately from the effect of the w_min correction.
#
# steady() is deliberately NOT re-run: it would re-tune erepro/R_max to absorb
# the w_min change and mask the very effect being measured. The 118 yr unfished
# spin-up does the equilibration, which is what the brief asks for.
#
# Both scenarios share one spin-up per (member, condition) and differ only in
# the effort array, which is what makes them a matched pair.
#
# Writes Output_large_files/wmin_test/19_stage1_sims.rds  (+ log)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer)
  library(mizer)
})

out_dir <- "Output_large_files/wmin_test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

W_NEW        <- 3626.667
PEN          <- "small divers"
SPINUP_YEARS <- 118
N_MEMBERS    <- 5

# stability config, copied from 09_Uncertainty_Analysis.Rmd lines 971-987
STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

t0 <- proc.time()
cat("=== Stage 1 re-run started:", format(Sys.time()), "===\n")
cat("mizer", as.character(packageVersion("mizer")),
    "| therMizer", as.character(packageVersion("therMizer")), "\n")
cat("w_min:", PEN, "0.001 ->", W_NEW, "g; spin-up", SPINUP_YEARS, "yr\n\n")

ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
idx <- ens$metadata$top10pct_indices

# --- stability check (verbatim logic from 09_Uncertainty_Analysis.Rmd:992) ----
check_stability <- function(sim_object) {
  bm <- getBiomass(sim_object)
  yrs <- as.numeric(rownames(bm))
  nT <- nrow(bm)
  if (nT < 5) return(list(stable = FALSE, reason = "insufficient_time_steps"))
  tail_mat <- tail(bm, max(5, min(STAB$check_years_tail, nT)))
  head_n <- max(5, min(STAB$trend_first_years, nT))
  head_mat <- head(bm, head_n); head_years <- head(yrs, head_n)
  mean_tail <- colMeans(tail_mat, na.rm = TRUE)
  cv_tail <- ifelse(mean_tail > 0, apply(tail_mat, 2, sd, na.rm = TRUE) / mean_tail, Inf)
  rel_slope <- setNames(rep(NA_real_, ncol(head_mat)), colnames(head_mat))
  pvals <- rel_slope
  for (j in seq_len(ncol(head_mat))) {
    y <- head_mat[, j]; mu <- mean(y, na.rm = TRUE)
    if (!is.finite(mu) || mu < STAB$min_mean_biomass) {
      rel_slope[j] <- 0; pvals[j] <- 1; next
    }
    fit <- try(suppressWarnings(lm(b ~ t, data = data.frame(t = head_years,
                                                            b = as.numeric(y)))),
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

# --- apply the corrected w_min ----------------------------------------------
# setParams() rebuilds w_min_idx (29 -> 71, verified) but RESETS rates_funcs and
# does NOT re-zero initial_n below the new w_min; both are handled here
# (18_test_wmin_change.R).
set_wmin <- function(p, species, w_new) {
  rf <- p@rates_funcs
  rd <- p@resource_dynamics
  i <- which(p@species_params$species == species)
  sp <- p@species_params
  sp$w_min[i] <- w_new
  p@species_params <- sp
  p <- suppressWarnings(setParams(p))
  # Restore ONLY the therMizer-specific rate functions. Overwriting the whole
  # rates_funcs list with the mizer 2.5.0 version drops entries that mizer 3.x
  # requires (Diffusion), which fails at projection time.
  ther <- intersect(c("Encounter", "PredRate", "EReproAndGrowth"), names(rf))
  p@rates_funcs[ther] <- rf[ther]
  p@resource_dynamics <- rd
  p@initial_n[i, p@w < w_new] <- 0
  stopifnot(p@w_min_idx[[i]] == max(which(p@w <= w_new)),
            identical(p@rates_funcs$Encounter, "therMizerEncounter"))
  p
}

run_condition <- function(p, eff_fished, eff_clim, label, member) {
  spin <- project(p, t_start = 1841, t_max = SPINUP_YEARS, effort = 0)
  stab <- check_stability(spin)
  n_init <- spin@n[SPINUP_YEARS, , ]     # matches the protocol's indexing
  sim_f <- project(p, initial_n = n_init, t_start = 1841, effort = eff_fished)
  sim_c <- project(p, initial_n = n_init, t_start = 1841, effort = eff_clim)
  cat(sprintf("    %-9s stable=%-5s max_cv=%.3f  fail_cv=[%s] fail_trend=[%s]\n",
              label, stab$stable, stab$max_cv,
              paste(stab$fail_cv, collapse = ","),
              paste(stab$fail_trend, collapse = ",")))
  list(spin = spin, stab = stab, fished = sim_f, climate = sim_c)
}

results <- list()
for (m in seq_len(N_MEMBERS)) {
  cat("--- member", m, "(sim_index", idx[m], ") ---\n")
  sf <- ens$fished_top10pct[[m]]
  sc <- ens$climate_top10pct[[m]]
  p  <- sf@params

  same_params <- isTRUE(all.equal(sf@params@species_params,
                                  sc@params@species_params)) &&
                 isTRUE(all.equal(sf@params@initial_n, sc@params@initial_n))
  cat("    arms share params:", same_params, "\n")

  ctrl <- run_condition(p, sf@effort, sc@effort, "control", m)
  trt  <- run_condition(set_wmin(p, PEN, W_NEW), sf@effort, sc@effort, "treated", m)

  results[[m]] <- list(member = m, sim_index = idx[m],
                       same_params = same_params,
                       control = ctrl, treated = trt,
                       stored_fished = sf, stored_climate = sc)
  cat("    elapsed", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
}

saveRDS(results, file.path(out_dir, "19_stage1_sims.rds"))
cat("\nSaved:", file.path(out_dir, "19_stage1_sims.rds"), "\n")
cat("elapsed:", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
