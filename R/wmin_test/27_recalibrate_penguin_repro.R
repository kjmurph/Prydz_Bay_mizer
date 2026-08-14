# =============================================================================
# STAGE B, part 2 -- recalibrate the penguin reproduction pair at
# w_min = 3626.667 g, then run the full workflow protocol.
#
# WHAT STAGE 1 GOT WRONG. Scripts 19-24 ran steady() with the workflow's
# preserve = "erepro", which pins erepro and lets R_max absorb everything. The
# stored penguin erepro is 3.1975e-04, calibrated against w_min = 0.001 g. At
# w_min = 3626.667 g that is ~400x too small (26_repro_feasibility.R), so R_max
# was the only free parameter left and steady() drove it to Inf -- removing
# density dependence entirely. The group still decayed with a 128-404 yr
# half-life. That is a recalibration failure, not a model result.
#
# THE FIX. Set the penguin erepro explicitly to a physically valid value, then
# let steady() find a finite R_max. Constraints:
#   1. erepro < 1 for ALL 19 groups (it is an efficiency, not a rate)
#   2. R_max finite for all 19 (else no density dependence)
# 26_repro_feasibility.R establishes erepro_needed = 0.075-0.457 across pilot
# members -- the value restoring control recruitment with no density dependence.
# That is the FLOOR. Setting erepro above it buys back density dependence:
#     erepro = erepro_needed / (1 - target_reproduction_level)
#
# Three multipliers of erepro_needed are swept so the choice is evidence-based
# rather than assumed. Higher multiplier = more density dependence retained, but
# erepro must stay below 1.
#
# WHY preserve = "erepro" IS STILL RIGHT. It is the workflow value
# (09_Uncertainty_Analysis.Rmd:1585-1590) AND it holds the other 18 groups'
# erepro fixed, so they cannot be pushed over 1 as a side effect. Headroom there
# is thin: orca already sits at 0.9509. Only the penguin row is retuned.
#
# Pilot members are DISTINCT models -- the top 212 contains only 189 distinct
# models (script 25/26).
#
# Writes Output_large_files/wmin_test/27_recal_{sims,summary}.{rds,csv}
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer)
  library(mizer)
  library(dplyr)
})

out_dir <- "Output_large_files/wmin_test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

PEN          <- "small divers"
W_NEW        <- 3626.667
SPINUP_YEARS <- 118
# erepro multipliers on erepro_needed. 1.0 = no density dependence (R_max -> Inf
# by construction); 2.0 = reproduction_level 0.5; 4.0 = 0.75.
EREPRO_MULT  <- c(1.0, 2.0, 4.0)
EREPRO_CAP   <- 0.9                     # never let the penguin row approach 1

# Workflow driver values, 09_Uncertainty_Analysis.Rmd:1585-1590 -- NOT the
# function defaults of tol = 0.002 / t_max = 2000.
STEADY_TMAX       <- 1500
STEADY_PRESERVE   <- c("erepro")
STEADY_TOL_LADDER <- c(0.1, 0.05, 0.01, 0.0025)

STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

N_MEMBERS <- as.integer(Sys.getenv("WMIN_N_MEMBERS", "1"))

t0 <- proc.time()
cat("=== Stage B part 2: penguin recalibration ===\n")
cat("started", format(Sys.time()), "| mizer", as.character(packageVersion("mizer")),
    "| therMizer", as.character(packageVersion("therMizer")), "\n")
cat("w_min", W_NEW, "g | erepro multipliers", paste(EREPRO_MULT, collapse = ", "),
    "| members", N_MEMBERS, "\n\n")

ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
idx <- ens$metadata$top10pct_indices
feas <- readRDS(file.path(out_dir, "26_repro_feasibility.rds"))

# --- stability check, verbatim from 09_Uncertainty_Analysis.Rmd:992 ----------
check_stability <- function(sim_object) {
  bm <- getBiomass(sim_object)
  yrs <- as.numeric(rownames(bm)); nT <- nrow(bm)
  if (nT < 5) return(list(stable = FALSE, max_cv = NA_real_))
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

# --- corrected w_min ---------------------------------------------------------
# setParams() rebuilds w_min_idx but RESETS rates_funcs (dropping therMizer) and
# does not re-zero initial_n below the new w_min. Restore ONLY the three
# therMizer entries -- replacing the whole list drops mizer 3.x's Diffusion entry
# and projection fails. (18_test_wmin_change.R)
set_wmin <- function(p, species, w_new) {
  rf <- p@rates_funcs; rd <- p@resource_dynamics
  i <- which(p@species_params$species == species)
  sp <- p@species_params; sp$w_min[i] <- w_new; p@species_params <- sp
  p <- suppressWarnings(setParams(p))
  ther <- intersect(c("Encounter", "PredRate", "EReproAndGrowth"), names(rf))
  p@rates_funcs[ther] <- rf[ther]
  p@resource_dynamics <- rd
  p@initial_n[i, p@w < w_new] <- 0
  stopifnot(p@w_min_idx[[i]] == max(which(p@w <= w_new)),
            identical(p@rates_funcs$Encounter, "therMizerEncounter"))
  p
}

# erepro and R_max are read from species_params at rate-evaluation time, so they
# are assigned directly. This deliberately avoids setParams(), which would reset
# rates_funcs again. The assertion below confirms RDI really did scale.
set_penguin_repro <- function(p, erepro_new, R_max_new) {
  i <- which(p@species_params$species == PEN)
  p@species_params$erepro[i] <- erepro_new
  p@species_params$R_max[i]  <- R_max_new
  p
}

rates_at_init <- function(p) {
  getRates(p, n = p@initial_n, n_pp = p@initial_n_pp,
           n_other = p@initial_n_other, effort = 0, t = 1841)
}

steady_ladder <- function(p, label) {
  converged <- rep(NA, length(STEADY_TOL_LADDER))
  for (k in seq_along(STEADY_TOL_LADDER)) {
    tol <- STEADY_TOL_LADDER[k]; warned <- FALSE
    out <- withCallingHandlers(
      try(steady(p, tol = tol, t_max = STEADY_TMAX, preserve = STEADY_PRESERVE),
          silent = TRUE),
      warning = function(w) {
        if (grepl("did not converge", conditionMessage(w), ignore.case = TRUE))
          warned <<- TRUE
        invokeRestart("muffleWarning")
      })
    if (inherits(out, "try-error")) {
      cat(sprintf("      %-18s tol=%-7.4g ERROR\n", label, tol))
      return(list(params = NULL, converged = converged))
    }
    converged[k] <- !warned
    p <- out
    cat(sprintf("      %-18s tol=%-7.4g %s\n", label, tol,
                if (warned) "did NOT converge" else "converged"))
  }
  list(params = p, converged = converged)
}

# Penguin decay over the spin-up tail. The workflow's trend test looks only at
# the FIRST 50 years and needs 2.5%/yr to fire, so it cannot see the slow
# terminal decay Stage 1 produced. Measure it directly.
penguin_decay <- function(sim, tail_n = 60) {
  bm <- getBiomass(sim)[, PEN]; yrs <- as.numeric(names(bm))
  keep <- tail(seq_along(bm), min(tail_n, length(bm)))
  b <- bm[keep]; y <- yrs[keep]
  if (any(!is.finite(b)) || any(b <= 0))
    return(list(pct_per_yr = NA_real_, half_life = NA_real_))
  r <- unname(coef(lm(log(b) ~ y))[2])
  list(pct_per_yr = 100 * (exp(r) - 1),
       half_life = if (r < 0) log(0.5) / r else Inf)
}

pen_bins <- function(p) {
  i <- which(p@species_params$species == PEN)
  sum(p@w >= p@w[p@w_min_idx[[i]]] & p@w <= p@species_params$w_max[i])
}

run_condition <- function(p, eff_f, eff_c, label) {
  i <- which(p@species_params$species == PEN)
  R0 <- p@species_params$R_max[i]; e0 <- p@species_params$erepro[i]
  sl <- steady_ladder(p, label); ps <- sl$params
  if (is.null(ps)) { cat("      ", label, "steady() FAILED\n"); return(NULL) }
  spin <- project(ps, t_start = 1841, t_max = SPINUP_YEARS, effort = 0)
  stab <- check_stability(spin); dec <- penguin_decay(spin)
  n_init <- spin@n[SPINUP_YEARS, , ]
  sim_f <- project(ps, initial_n = n_init, t_start = 1841, effort = eff_f)
  sim_c <- project(ps, initial_n = n_init, t_start = 1841, effort = eff_c)
  spf <- ps@species_params
  cat(sprintf("      %-18s R_max %.3e -> %.3e | erepro %.4g -> %.4g | maxErepro(all19) %.4g\n",
              label, R0, spf$R_max[i], e0, spf$erepro[i], max(spf$erepro)))
  cat(sprintf("      %-18s stable=%-5s max_cv=%.3f | penguin %+.4f%%/yr  half-life %s\n",
              "", stab$stable, stab$max_cv, dec$pct_per_yr,
              if (is.finite(dec$half_life)) sprintf("%.0f yr", dec$half_life) else "none"))
  list(params = ps, spin = spin, stab = stab, decay = dec,
       fished = sim_f, climate = sim_c,
       R_max_after = spf$R_max[i], erepro_after = spf$erepro[i],
       max_erepro_all = max(spf$erepro),
       any_erepro_ge1 = any(spf$erepro >= 1),
       any_Rmax_inf = any(!is.finite(spf$R_max)),
       n_bins = pen_bins(ps), steady_converged = sl$converged)
}

pen_biomass_end <- function(cond) {
  if (is.null(cond)) return(NA_real_)
  bm <- getBiomass(cond$spin)[, PEN]; unname(bm[length(bm)])
}

results <- list(); rows <- list()
for (k in seq_len(min(N_MEMBERS, nrow(feas)))) {
  m <- feas$position[k]; si <- feas$sim_index[k]
  en <- feas$erepro_needed[k]
  cat("--- member", k, "(position", m, ", sim_index", si, ") erepro_needed",
      signif(en, 4), "---\n")
  sf <- ens$fished_top10pct[[m]]; sc <- ens$climate_top10pct[[m]]
  p <- sf@params

  ctrl <- run_condition(p, sf@effort, sc@effort, "control")
  pb_ctrl <- pen_biomass_end(ctrl)

  treated <- list()
  for (mult in EREPRO_MULT) {
    e_new <- min(en * mult, EREPRO_CAP)
    rl_target <- 1 - 1 / mult                      # 0, 0.5, 0.75
    p_t <- set_wmin(p, PEN, W_NEW)

    # RDI at the stored erepro and new w_min, then scale to the target.
    i <- which(p_t@species_params$species == PEN)
    r_t <- rates_at_init(p_t)
    rdi_target <- r_t$rdi[i] * e_new / p_t@species_params$erepro[i]
    rdd_target <- feas$rdd_ctrl[k]
    # 1/RDD = 1/RDI + 1/R_max
    R_new <- if (rdi_target > rdd_target)
      1 / (1 / rdd_target - 1 / rdi_target) else Inf
    p_t <- set_penguin_repro(p_t, e_new, R_new)

    # confirm the direct species_params assignment really moved RDI
    r_chk <- rates_at_init(p_t)
    stopifnot(isTRUE(all.equal(unname(r_chk$rdi[i]), unname(rdi_target),
                               tolerance = 1e-8)))

    lab <- sprintf("e=%.4g rl=%.2f", e_new, rl_target)
    cat(sprintf("    erepro x%.1f -> %.4g | R_max -> %.4e | target repro_level %.2f\n",
                mult, e_new, R_new, rl_target))
    tr <- run_condition(p_t, sf@effort, sc@effort, lab)
    treated[[as.character(mult)]] <- tr

    if (!is.null(tr)) {
      pb <- pen_biomass_end(tr)
      cat(sprintf("      %-18s penguin biomass vs control: %.4g%%\n", "",
                  100 * pb / pb_ctrl))
      rows[[length(rows) + 1]] <- data.frame(
        member = k, position = m, sim_index = si,
        erepro_mult = mult, erepro_set = e_new, rl_target = rl_target,
        R_max_set = R_new,
        erepro_after = tr$erepro_after, R_max_after = tr$R_max_after,
        R_max_finite = is.finite(tr$R_max_after),
        max_erepro_all19 = tr$max_erepro_all,
        any_erepro_ge1 = tr$any_erepro_ge1,
        any_Rmax_inf = tr$any_Rmax_inf,
        n_bins = tr$n_bins,
        steady_converged = all(tr$steady_converged, na.rm = TRUE),
        stable = tr$stab$stable, max_cv = tr$stab$max_cv,
        pen_pct_per_yr = tr$decay$pct_per_yr,
        pen_half_life = tr$decay$half_life,
        pen_biomass_pct_of_control = 100 * pb / pb_ctrl)
    }
  }
  results[[k]] <- list(member = k, position = m, sim_index = si,
                       control = ctrl, treated = treated,
                       stored_fished = sf, stored_climate = sc)
  cat("    elapsed", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n\n")
}

summ <- bind_rows(rows)
saveRDS(results, file.path(out_dir, "27_recal_sims.rds"))
saveRDS(summ, file.path(out_dir, "27_recal_summary.rds"))
write.csv(summ, file.path(out_dir, "27_recal_summary.csv"), row.names = FALSE)

cat("\n================ SUMMARY ================\n")
print(as.data.frame(summ %>% select(member, sim_index, erepro_mult, erepro_set,
                                    R_max_after, R_max_finite, max_erepro_all19,
                                    any_erepro_ge1, stable, pen_pct_per_yr,
                                    pen_biomass_pct_of_control)),
      digits = 4, row.names = FALSE)

cat("\n================ GATE ================\n")
cat("HARD constraints (must hold for every run):\n")
cat("  erepro < 1 for all 19 groups :",
    if (!any(summ$any_erepro_ge1)) "PASS" else "FAIL", "\n")
cat("  R_max finite for all 19      :",
    if (!any(summ$any_Rmax_inf)) "PASS" else "FAIL", "\n")
cat("  steady() converged           :",
    sum(summ$steady_converged), "/", nrow(summ), "\n")
cat("  spin-up stable               :", sum(summ$stable), "/", nrow(summ), "\n")
cat("\nBy erepro multiplier:\n")
print(as.data.frame(summ %>% group_by(erepro_mult) %>% summarise(
  n = n(), R_max_finite = sum(R_max_finite),
  max_erepro = max(max_erepro_all19), stable = sum(stable),
  median_decay_pct_yr = median(pen_pct_per_yr, na.rm = TRUE),
  median_pen_pct_ctrl = median(pen_biomass_pct_of_control, na.rm = TRUE),
  .groups = "drop")), digits = 4, row.names = FALSE)

cat("\nelapsed:", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
