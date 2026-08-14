# =============================================================================
# STAGE 3 -- build the two re-run arms so the catchability optimum can be tested
# for sensitivity to the `small divers` w_min correction.
#
#   B  the member re-run through steady(preserve="erepro") + the driver's spin-up,
#      w_min UNCORRECTED.  The control.
#   C  the same, with w_min = 3626.667 for `small divers` and the paired penguin
#      erepro / R_max recalibration at reproduction level 0.50.  The treatment.
#
# WHY B EXISTS.  C cannot be compared with arm A.  Re-entering the pipeline
# reorders the RMSE ranking at rho 0.60 on its own -- roughly 5,500x the size of
# the w_min effect -- because sim@params@initial_n is the POST-spin-up state and
# the pre-spin-up state was never saved, so any fresh run applies steady() and a
# spin-up to an already-equilibrated model.  The only interpretable quantity is
# optimum(C) - optimum(B), both arms having taken the identical path.
#
# The recalibration, the single-steady() fidelity rule and the spin-up protocol
# are taken verbatim from R/wmin_test/39_full_ensemble_paired.R:136-204.  That
# script computes the same states but persists only summary statistics; the only
# change here is to save the resulting MizerParams so Stage 1 can run on them.
#
# A SINGLE steady() CALL, deliberately.  run_single_enhanced_sim
# (09_Uncertainty_Analysis.Rmd:319-543) treats non-convergence as outright
# rejection, so a tolerance ladder would rescue draws the real procedure discards.
#
# steady(preserve="erepro") RE-DERIVES R_max -- it ends by calling
# setBevertonHolt(erepro = old_erepro), which recomputes R_max at the equilibrated
# state.  The penguin reproduction level that comes out is therefore NOT the 0.50
# that went in.  Both are recorded; never assume the setting survived.
#
# USAGE
#   Rscript R/catch_fit/05_wmin_arms.R            # all 5 members, arms B and C
#   Rscript R/catch_fit/05_wmin_arms.R 446
# =============================================================================

source("R/catch_fit/01_harness.R")

args <- commandArgs(trailingOnly = TRUE)
members_wanted <- if (length(args)) as.integer(args) else CF_MEMBERS

OUT <- file.path(cf_out_dir(arm = NA), "wmin_arms")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

PEN         <- "small divers"
W_NEW       <- 3626.667
REPRO_LEVEL <- 0.50
EREPRO_CAP  <- 0.95
STEADY_TOL  <- 0.002
STEADY_TMAX <- 1200
SPINUP_YEARS  <- 118
SPINUP_CYCLES <- 3
STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

# ------------------------------- stability: the driver's own criterion --------
# 09_Uncertainty_Analysis.Rmd:992, transcribed at 39_full_ensemble_paired.R:108.
check_stability <- function(s) {
  bm <- getBiomass(s); yrs <- as.numeric(rownames(bm)); nT <- nrow(bm)
  if (nT < 5) return(list(stable = FALSE, max_cv = NA_real_, fail = "short"))
  tail_mat <- tail(bm, max(5, min(STAB$check_years_tail, nT)))
  hn <- max(5, min(STAB$trend_first_years, nT))
  hm <- head(bm, hn); hy <- head(yrs, hn)
  mt <- colMeans(tail_mat, na.rm = TRUE)
  cv <- ifelse(mt > 0, apply(tail_mat, 2, sd, na.rm = TRUE) / mt, Inf)
  rs <- setNames(rep(NA_real_, ncol(hm)), colnames(hm)); pv <- rs
  for (q in seq_len(ncol(hm))) {
    y <- hm[, q]; mu <- mean(y, na.rm = TRUE)
    if (!is.finite(mu) || mu < STAB$min_mean_biomass) { rs[q] <- 0; pv[q] <- 1; next }
    ft <- try(suppressWarnings(lm(b ~ t, data = data.frame(t = hy, b = as.numeric(y)))),
              silent = TRUE)
    if (inherits(ft, "try-error")) { rs[q] <- 0; pv[q] <- 1 } else {
      rs[q] <- as.numeric(coef(ft)[["t"]]) / mu
      pp <- try(summary(ft)$coefficients["t", "Pr(>|t|)"], silent = TRUE)
      pv[q] <- if (inherits(pp, "try-error")) 1 else as.numeric(pp)
    }
  }
  fcv <- cv > STAB$cv_threshold
  ftr <- (abs(rs) > STAB$trend_rel_slope_max) & (pv < STAB$trend_pval_max)
  list(stable = !any(fcv | ftr, na.rm = TRUE),
       max_cv = max(cv[is.finite(cv)], na.rm = TRUE),
       fail = paste(unique(c(names(fcv)[which(fcv)], names(ftr)[which(ftr)])),
                    collapse = ";"))
}

# ------------------------------------------- w_min correction (arm C only) ----
# setParams() rebuilds the rate functions, so the therMizer entries must be put
# back or the arm would silently run on mizer defaults.
set_wmin <- function(q, i, w_new) {
  rf <- q@rates_funcs; rd <- q@resource_dynamics
  sp <- q@species_params; sp$w_min[i] <- w_new; q@species_params <- sp
  q <- suppressWarnings(setParams(q))
  th <- intersect(c("Encounter", "PredRate", "EReproAndGrowth"), names(rf))
  q@rates_funcs[th] <- rf[th]; q@resource_dynamics <- rd
  q@initial_n[i, q@w < w_new] <- 0
  q
}
rates0 <- function(q) getRates(q, n = q@initial_n, n_pp = q@initial_n_pp,
                               n_other = q@initial_n_other, effort = 0, t = 1841)

# ------------------------------------------------- steady + spin-up -----------
run_arm <- function(p) {
  # NON-CONVERGENCE IS A message(), NOT A warning().
  # mizer:::projectToSteady.MizerParams emits
  #   message("Simulation run did not converge after ", ...)
  # so the `warning =` handler used at 09_Uncertainty_Analysis.Rmd:431 and
  # 39_full_ensemble_paired.R:154 can never see it.  Evidence: across all
  # 1,997 members x 2 arms of the completed 39 run, `steady_no_converge` fired
  # 0 times, while member 446 demonstrably does not converge (distance
  # 0.00216666 against tol 0.002).  See R/catch_fit/tests_steady_signal.R.
  #
  # RECORDED, NOT REJECTED.  The driver's rule is to reject, but rejecting here
  # would empty both arms and destroy the pairing that makes the w_min
  # comparison valid -- and both arms take the identical path, so a shared
  # non-convergence cancels in the B-vs-C difference.  The flag is carried into
  # the results instead.
  converged <- TRUE; distance <- NA_real_
  catch_nonconv <- function(cond) {
    msg <- conditionMessage(cond)
    if (grepl("did not converge", msg, ignore.case = TRUE)) {
      converged <<- FALSE
      d <- suppressWarnings(as.numeric(sub(".*was:\\s*", "", trimws(msg))))
      if (is.finite(d)) distance <<- d
    }
    invisible(NULL)
  }
  out <- withCallingHandlers(
    try(steady(p, tol = STEADY_TOL, t_max = STEADY_TMAX, preserve = c("erepro")),
        silent = TRUE),
    message = function(m) { catch_nonconv(m); invokeRestart("muffleMessage") },
    warning = function(w) { catch_nonconv(w); invokeRestart("muffleWarning") })
  if (inherits(out, "try-error")) return(list(ok = FALSE, reason = "steady_error"))
  sp <- out@species_params
  if (any(is.na(sp$gamma)) || any(is.infinite(sp$gamma)))
    return(list(ok = FALSE, reason = "invalid_gamma"))

  init <- NULL; stab <- NULL; cyc_used <- 0; s <- NULL
  for (cyc in seq_len(SPINUP_CYCLES)) {
    # initial_n must be OMITTED, not passed as NULL
    s <- try(if (is.null(init))
      project(out, t_start = 1841, t_max = SPINUP_YEARS, effort = 0)
      else project(out, t_start = 1841, t_max = SPINUP_YEARS, effort = 0,
                   initial_n = init), silent = TRUE)
    if (inherits(s, "try-error")) return(list(ok = FALSE, reason = "project_error"))
    init <- s@n[SPINUP_YEARS, , ]; cyc_used <- cyc
    stab <- check_stability(s)
    if (isTRUE(stab$stable)) break
  }
  # The post-spin-up state becomes the arm's starting condition, exactly as the
  # driver hands it to the historical projection.  initial_n ONLY: the driver
  # passes `initial_n = final_initial_n` and nothing else, so the resource stays
  # at its post-steady() value.  Immaterial in practice -- resource_dynamics is
  # "plankton_forcing", so n_pp is overwritten from the forcing every step -- but
  # fidelity to run_single_enhanced_sim is the rule here.
  p_out <- out
  p_out@initial_n[] <- s@n[dim(s@n)[1], , ]
  list(ok = TRUE, params = p_out, steady_params = out,
       steady_converged = converged, steady_distance = distance,
       pass = isTRUE(stab$stable), max_cv = stab$max_cv, fail = stab$fail,
       cycles = cyc_used,
       max_erepro = max(sp$erepro), n_erepro_ge1 = sum(sp$erepro >= 1),
       n_rmax_inf = sum(!is.finite(sp$R_max)),
       rmax_inf_groups = paste(sp$species[!is.finite(sp$R_max)], collapse = ";"))
}

# ----------------------------------------------------------------- run -------
cf_log("=== Stage 3: building w_min arms B and C ===")
members <- cf_load_members(members_wanted, arm = "A")
meta_rows <- list()

for (m in members) {
  si <- m$sim_index
  fB <- file.path(OUT, sprintf("params_B_%04d.rds", si))
  fC <- file.path(OUT, sprintf("params_C_%04d.rds", si))
  if (file.exists(fB) && file.exists(fC)) {
    cf_log("member ", si, " arms exist, skipping")
    meta_rows[[as.character(si)]] <- readRDS(fB)$meta_row
    next
  }
  p0 <- m$params
  i  <- which(p0@species_params$species == PEN)
  t0 <- proc.time()

  # ---- arm B: control, uncorrected, through the identical pipeline
  B <- run_arm(p0)
  cf_log("member ", si, " arm B: ", if (B$ok) "ok" else B$reason,
         if (B$ok) paste0(" / stable ", B$pass, " / cycles ", B$cycles) else "")

  # ---- arm C: w_min corrected + paired penguin reproduction recalibration
  pt  <- set_wmin(p0, i, W_NEW)
  r0  <- rates0(p0); rtt <- rates0(pt)
  fl  <- p0@species_params$erepro[i] * r0$rdd[i] / rtt$rdi[i]   # restores control RDD
  e_new  <- min(fl / (1 - REPRO_LEVEL), EREPRO_CAP)
  rdi_t  <- rtt$rdi[i] * e_new / pt@species_params$erepro[i]
  infeas <- rdi_t <= r0$rdd[i]
  R_new  <- if (!infeas) 1 / (1 / r0$rdd[i] - 1 / rdi_t) else Inf
  pt@species_params$erepro[i] <- e_new
  pt@species_params$R_max[i]  <- R_new
  C <- run_arm(pt)
  cf_log("member ", si, " arm C: ", if (C$ok) "ok" else C$reason,
         if (C$ok) paste0(" / stable ", C$pass, " / cycles ", C$cycles) else "")

  # achieved penguin reproduction level -- steady() re-derives R_max, so the 0.50
  # that went in is NOT what comes out
  ach <- NA_real_; rdd_ratio <- NA_real_
  if (isTRUE(C$ok) && isTRUE(B$ok)) {
    ach <- unname(getReproductionLevel(C$params)[i])
    rdd_ratio <- unname(getRDD(C$params)[i] / getRDD(B$params)[i])
  }

  row <- data.frame(
    sim_index = si,
    erepro_floor = fl, erepro_set = e_new, erepro_capped = fl / (1 - REPRO_LEVEL) > EREPRO_CAP,
    repro_infeasible = infeas,
    B_ok = B$ok, B_reason = if (B$ok) "ok" else B$reason,
    B_steady_converged = if (B$ok) B$steady_converged else NA,
    B_steady_distance = if (B$ok) B$steady_distance else NA_real_,
    C_steady_converged = if (C$ok) C$steady_converged else NA,
    C_steady_distance = if (C$ok) C$steady_distance else NA_real_,
    B_stable = if (B$ok) B$pass else NA, B_max_cv = if (B$ok) B$max_cv else NA_real_,
    B_cycles = if (B$ok) B$cycles else NA_integer_,
    B_max_erepro = if (B$ok) B$max_erepro else NA_real_,
    B_n_rmax_inf = if (B$ok) B$n_rmax_inf else NA_integer_,
    C_ok = C$ok, C_reason = if (C$ok) "ok" else C$reason,
    C_stable = if (C$ok) C$pass else NA, C_max_cv = if (C$ok) C$max_cv else NA_real_,
    C_cycles = if (C$ok) C$cycles else NA_integer_,
    C_max_erepro = if (C$ok) C$max_erepro else NA_real_,
    C_n_rmax_inf = if (C$ok) C$n_rmax_inf else NA_integer_,
    repro_level_target = REPRO_LEVEL, repro_level_achieved = ach,
    rdd_ratio_pen = rdd_ratio,
    elapsed_min = (proc.time() - t0)[["elapsed"]] / 60,
    stringsAsFactors = FALSE)
  meta_rows[[as.character(si)]] <- row

  if (isTRUE(B$ok)) saveRDS(list(params = B$params, meta = B[setdiff(names(B),
                    c("params", "steady_params"))], meta_row = row), fB)
  if (isTRUE(C$ok)) saveRDS(list(params = C$params, meta = C[setdiff(names(C),
                    c("params", "steady_params"))], meta_row = row), fC)
  cf_log("member ", si, " done in ", round(row$elapsed_min, 1), " min")
}

M <- do.call(rbind, meta_rows)
saveRDS(M, file.path(OUT, "05_wmin_arms_meta.rds"))
write.csv(M, file.path(OUT, "05_wmin_arms_meta.csv"), row.names = FALSE)

cat("\n=== arm construction summary ===\n")
print(data.frame(sim = M$sim_index,
                 B_ok = M$B_ok, B_stable = M$B_stable, B_cyc = M$B_cycles,
                 C_ok = M$C_ok, C_stable = M$C_stable, C_cyc = M$C_cycles,
                 pen_erepro = signif(M$erepro_set, 4),
                 rl_target = M$repro_level_target,
                 rl_achieved = round(M$repro_level_achieved, 4),
                 rdd_ratio = round(M$rdd_ratio_pen, 4),
                 max_erepro_C = round(M$C_max_erepro, 4),
                 rmax_inf_C = M$C_n_rmax_inf,
                 B_conv = M$B_steady_converged, C_conv = M$C_steady_converged,
                 B_dist = signif(M$B_steady_distance, 4),
                 C_dist = signif(M$C_steady_distance, 4)), row.names = FALSE)
cat("\nNOTE: rl_achieved is what steady() left behind, NOT the 0.50 that was set.\n")
cat("NOTE: *_conv FALSE means steady() hit t_max without reaching tol =",
    STEADY_TOL, "-- recorded, not rejected, because both arms take the\n")
cat("      identical path and a shared non-convergence cancels in C - B.\n")
cat("Next:  CF_ARM=B Rscript R/catch_fit/02_q_response.R   (then 03, then arm C)\n")
