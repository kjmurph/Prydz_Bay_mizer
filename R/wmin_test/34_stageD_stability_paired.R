# =============================================================================
# STAGE D -- does the corrected w_min change Monte Carlo ACCEPTANCE?
#
# THE QUESTION. docs/small_divers_wmin_test_brief.md section 6 carries an
# un-skippable warning: re-running only the fitted subset assumes the accepted
# set and the RMSE ranking are unchanged, and BOTH depend on w_min -- the
# stability check runs on the spin-up, and RMSE depends on modelled catch. This
# settles part (i): do the fitted members still pass the stability criteria?
#
# MEMBERSHIP comes from the DEDUPLICATED ranking (33_dedupe_full_and_rmse.R).
# The old 212 was ceiling(2111 * 0.10) over an ensemble containing 114 redundant
# copies of 94 repeated Monte Carlo runs, so both the cutoff and the ordering
# were contaminated. The clean set is ceiling(n_unique * 0.10).
#
# WHY PAIRED. The accepted ensemble is a concatenation of runs with nine
# different prior-SD combinations and varying tol/t_max, so no single absolute
# pass rate is reproducible. Every member is run TWICE through one identical
# pipeline -- uncorrected base and corrected base -- and only the CHANGE in
# pass/fail is interpreted.
#
# FIDELITY. run_single_enhanced_sim (09_Uncertainty_Analysis.Rmd:319-543) treats
# steady() non-convergence as outright rejection, so a SINGLE steady() call is
# used here, NOT the tolerance ladder of Stages B and C. A ladder would rescue
# draws the real procedure discards and inflate the pass rate. Spin-up follows
# the driver: up to 3 unfished cycles of 118 yr, early exit once stable.
#
# THE CORRECTION (settled in Stages A-C):
#   w_min 0.001 -> 3626.667 g, and the penguin erepro/R_max pair recalibrated to
#   reproduction level 0.50. Changing w_min alone leaves the group uncalibrated
#   and drives it extinct -- that was Stage 1's error.
#
# Writes Output_large_files/wmin_test/34_stageD_stability.{rds,csv}
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel)
})

out_dir <- "Output_large_files/wmin_test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

PEN         <- "small divers"
W_NEW       <- 3626.667
REPRO_LEVEL <- 0.50
EREPRO_CAP  <- 0.95
# seed-rerun driver values -- the configuration that produced the final ensemble
STEADY_TOL     <- 0.002
STEADY_TMAX    <- 1200
SPINUP_YEARS   <- 118
SPINUP_CYCLES  <- 3
SPINUP_MIN_CYC <- 1

STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

N_LIMIT <- as.integer(Sys.getenv("STAGED_N", "0"))     # 0 = all
N_CORES <- as.integer(Sys.getenv("STAGED_CORES",
                                 as.character(max(1, detectCores() - 2))))

t0 <- proc.time()
cat("=== Stage D: paired stability test on the deduplicated top 10% ===\n")
cat("started", format(Sys.time()), "| cores", N_CORES, "\n")

dd <- readRDS(file.path(out_dir, "33_dedupe_full.rds"))
members <- dd$new_top
if (N_LIMIT > 0) members <- head(members, N_LIMIT)
cat("deduplicated ensemble:", dd$n_unique,
    "| clean top 10%:", dd$n_top_new,
    "| running:", length(members), "members\n\n")

# --- fetch each member's params ----------------------------------------------
# Prefer the 375 MB fitted object; fall back to the 1.86 GB full ensemble for
# any member promoted into the clean top 10% from outside the old 212.
cat("loading fitted ensemble (375 MB)...\n")
ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
old_idx <- ens$metadata$top10pct_indices
have <- match(members, old_idx)
plist <- vector("list", length(members))
for (j in which(!is.na(have)))
  plist[[j]] <- ens$fished_top10pct[[have[j]]]@params
eff_f <- ens$fished_top10pct[[1]]@effort
eff_c <- ens$climate_top10pct[[1]]@effort
rm(ens); invisible(gc())
cat("  from fitted object:", sum(!is.na(have)), "\n")

missing <- which(is.na(have))
if (length(missing)) {
  cat("  promoted members needing the full ensemble:", length(missing), "\n")
  cat("loading full ensemble (1.86 GB, 2-4 min)...\n")
  mc <- readRDS("Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds")
  sims_all <- mc$simulations
  valid_idx <- which(vapply(sims_all, function(x)
    inherits(x, "MizerSim") && !any(is.nan(x@n)) && !any(is.infinite(x@n)),
    logical(1)))
  for (j in missing) plist[[j]] <- sims_all[[valid_idx[members[j]]]]@params
  rm(mc, sims_all); invisible(gc())
}
stopifnot(!any(vapply(plist, is.null, logical(1))))
cat("member params ready:", length(plist), "\n\n")

# --- worker ------------------------------------------------------------------
worker <- function(k) {
  suppressPackageStartupMessages({ library(therMizer); library(mizer) })
  p0 <- plist[[k]]
  i <- which(p0@species_params$species == PEN)

  check_stability <- function(sim_object) {
    bm <- getBiomass(sim_object)
    yrs <- as.numeric(rownames(bm)); nT <- nrow(bm)
    if (nT < 5) return(list(stable = FALSE, max_cv = NA_real_, fail = "short"))
    tail_mat <- tail(bm, max(5, min(STAB$check_years_tail, nT)))
    head_n <- max(5, min(STAB$trend_first_years, nT))
    head_mat <- head(bm, head_n); head_years <- head(yrs, head_n)
    mean_tail <- colMeans(tail_mat, na.rm = TRUE)
    cv_tail <- ifelse(mean_tail > 0,
                      apply(tail_mat, 2, sd, na.rm = TRUE) / mean_tail, Inf)
    rel_slope <- setNames(rep(NA_real_, ncol(head_mat)), colnames(head_mat))
    pvals <- rel_slope
    for (j in seq_len(ncol(head_mat))) {
      y <- head_mat[, j]; mu <- mean(y, na.rm = TRUE)
      if (!is.finite(mu) || mu < STAB$min_mean_biomass) {
        rel_slope[j] <- 0; pvals[j] <- 1; next
      }
      fit <- try(suppressWarnings(
        lm(b ~ t, data = data.frame(t = head_years, b = as.numeric(y)))),
        silent = TRUE)
      if (inherits(fit, "try-error")) { rel_slope[j] <- 0; pvals[j] <- 1 } else {
        rel_slope[j] <- as.numeric(coef(fit)[["t"]]) / mu
        pv <- try(summary(fit)$coefficients["t", "Pr(>|t|)"], silent = TRUE)
        pvals[j] <- if (inherits(pv, "try-error")) 1 else as.numeric(pv)
      }
    }
    fail_cv <- cv_tail > STAB$cv_threshold
    fail_tr <- (abs(rel_slope) > STAB$trend_rel_slope_max) &
      (pvals < STAB$trend_pval_max)
    list(stable = !any(fail_cv | fail_tr, na.rm = TRUE),
         max_cv = max(cv_tail[is.finite(cv_tail)], na.rm = TRUE),
         fail = paste(unique(c(names(fail_cv)[which(fail_cv)],
                               names(fail_tr)[which(fail_tr)])), collapse = ";"))
  }

  # setParams() rebuilds w_min_idx but RESETS rates_funcs (dropping therMizer)
  # and does not re-zero initial_n below the new w_min; restore only the three
  # therMizer entries or mizer 3.x's Diffusion entry is lost.
  set_wmin <- function(p, w_new) {
    rf <- p@rates_funcs; rd <- p@resource_dynamics
    sp <- p@species_params; sp$w_min[i] <- w_new; p@species_params <- sp
    p <- suppressWarnings(setParams(p))
    ther <- intersect(c("Encounter", "PredRate", "EReproAndGrowth"), names(rf))
    p@rates_funcs[ther] <- rf[ther]
    p@resource_dynamics <- rd
    p@initial_n[i, p@w < w_new] <- 0
    p
  }
  rates0 <- function(p) getRates(p, n = p@initial_n, n_pp = p@initial_n_pp,
                                 n_other = p@initial_n_other, effort = 0, t = 1841)

  run_arm <- function(p) {
    warned <- FALSE
    out <- withCallingHandlers(
      try(steady(p, tol = STEADY_TOL, t_max = STEADY_TMAX,
                 preserve = c("erepro")), silent = TRUE),
      warning = function(w) {
        if (grepl("did not converge", conditionMessage(w), ignore.case = TRUE))
          warned <<- TRUE
        invokeRestart("muffleWarning")
      })
    if (inherits(out, "try-error"))
      return(list(pass = FALSE, reason = "steady_error", max_cv = NA_real_,
                  fail = "", erepro_ok = NA, rmax_inf = NA_integer_))
    if (warned)
      return(list(pass = FALSE, reason = "steady_no_converge", max_cv = NA_real_,
                  fail = "", erepro_ok = NA, rmax_inf = NA_integer_))
    sp <- out@species_params
    if (any(is.na(sp$gamma)) || any(is.infinite(sp$gamma)))
      return(list(pass = FALSE, reason = "invalid_gamma", max_cv = NA_real_,
                  fail = "", erepro_ok = NA, rmax_inf = NA_integer_))
    init <- NULL; stab <- NULL
    for (cyc in seq_len(SPINUP_CYCLES)) {
      # initial_n must be OMITTED on the first cycle, not passed as NULL:
      # project() does params@initial_n[] <- initial_n and errors with
      # "replacement has length zero". The driver's
      # `initial_n = if (!is.null(...)) ... else NULL` works only because
      # missing() semantics differ inside its own wrapper.
      s <- try(if (is.null(init))
        project(out, t_start = 1841, t_max = SPINUP_YEARS, effort = 0)
        else
          project(out, t_start = 1841, t_max = SPINUP_YEARS, effort = 0,
                  initial_n = init), silent = TRUE)
      if (inherits(s, "try-error"))
        return(list(pass = FALSE, reason = "project_error", max_cv = NA_real_,
                    fail = "", erepro_ok = NA, rmax_inf = NA_integer_))
      init <- s@n[SPINUP_YEARS, , ]
      stab <- check_stability(s)
      if (isTRUE(stab$stable) && cyc >= SPINUP_MIN_CYC) break
    }
    list(pass = isTRUE(stab$stable),
         reason = if (isTRUE(stab$stable)) "ok" else "unstable",
         max_cv = stab$max_cv, fail = stab$fail,
         erepro_ok = all(sp$erepro < 1),
         rmax_inf = sum(!is.finite(sp$R_max)))
  }

  ctl <- run_arm(p0)

  pt <- set_wmin(p0, W_NEW)
  r0 <- rates0(p0); rt <- rates0(pt)
  # erepro that would restore the CONTROL recruitment at the new w_min, with no
  # density dependence. This is the floor; density dependence costs more.
  floor_e <- p0@species_params$erepro[i] * r0$rdd[i] / rt$rdi[i]
  e_new <- min(floor_e / (1 - REPRO_LEVEL), EREPRO_CAP)
  rdi_t <- rt$rdi[i] * e_new / pt@species_params$erepro[i]
  # If even erepro = EREPRO_CAP cannot lift RDI above the control RDD, no finite
  # R_max reproduces that recruitment -- RDD can never exceed RDI. R_max = Inf
  # (no density dependence, RDD = RDI) is then the maximum achievable, and the
  # penguin population necessarily settles BELOW control. That is a real
  # limitation of the correction for those members, not a coding failure, so it
  # is flagged rather than hidden.
  infeasible <- rdi_t <= r0$rdd[i]
  R_new <- if (!infeasible) 1 / (1 / r0$rdd[i] - 1 / rdi_t) else Inf
  pt@species_params$erepro[i] <- e_new
  pt@species_params$R_max[i]  <- R_new
  trt <- run_arm(pt)

  data.frame(k = k, sim_index = members[k],
             erepro_floor = floor_e, erepro_set = e_new,
             erepro_capped = floor_e / (1 - REPRO_LEVEL) > EREPRO_CAP,
             erepro_floor_gt1 = floor_e > 1,
             repro_infeasible = infeasible,
             R_max_set_finite = is.finite(R_new),
             ctl_pass = ctl$pass, ctl_reason = ctl$reason,
             ctl_max_cv = ctl$max_cv, ctl_fail = ctl$fail,
             ctl_rmax_inf = ctl$rmax_inf,
             trt_pass = trt$pass, trt_reason = trt$reason,
             trt_max_cv = trt$max_cv, trt_fail = trt$fail,
             trt_rmax_inf = trt$rmax_inf, trt_erepro_ok = trt$erepro_ok,
             stringsAsFactors = FALSE)
}

cl <- makeCluster(N_CORES)
on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
clusterExport(cl, c("plist", "members", "PEN", "W_NEW", "REPRO_LEVEL",
                    "EREPRO_CAP", "STEADY_TOL", "STEADY_TMAX", "SPINUP_YEARS",
                    "SPINUP_CYCLES", "SPINUP_MIN_CYC", "STAB"),
              envir = environment())
cat("running", length(plist), "members x 2 arms on", N_CORES, "cores...\n")
res <- parLapplyLB(cl, seq_along(plist), worker)
stopCluster(cl)

df <- do.call(rbind, res)
saveRDS(df, file.path(out_dir, "34_stageD_stability.rds"))
write.csv(df, file.path(out_dir, "34_stageD_stability.csv"), row.names = FALSE)

n <- nrow(df)
cat("\n=== acceptance crosstab ===\n")
print(table(control = df$ctl_pass, corrected = df$trt_pass))
cat(sprintf("\ncontrol   pass: %d/%d (%.1f%%)\n", sum(df$ctl_pass), n,
            100 * mean(df$ctl_pass)))
cat(sprintf("corrected pass: %d/%d (%.1f%%)\n", sum(df$trt_pass), n,
            100 * mean(df$trt_pass)))

lost <- df[df$ctl_pass & !df$trt_pass, ]
gain <- df[!df$ctl_pass & df$trt_pass, ]
cat(sprintf("\nlost   (pass -> fail): %d\n", nrow(lost)))
if (nrow(lost)) print(as.data.frame(lost[, c("sim_index", "trt_reason",
                                             "trt_max_cv", "trt_fail")]),
                      row.names = FALSE)
cat(sprintf("gained (fail -> pass): %d\n", nrow(gain)))
if (nrow(gain)) print(as.data.frame(gain[, c("sim_index", "ctl_reason",
                                             "ctl_max_cv", "ctl_fail")]),
                      row.names = FALSE)

cat("\n=== failure reasons ===\n")
cat("control:\n");   print(table(df$ctl_reason))
cat("corrected:\n"); print(table(df$trt_reason))

cat("\n=== recalibration feasibility ===\n")
cat("  erepro floor  min", signif(min(df$erepro_floor), 4),
    "| median", signif(median(df$erepro_floor), 4),
    "| max", signif(max(df$erepro_floor), 4), "\n")
cat("  floor > 1 (unreachable at ANY reproduction level):",
    sum(df$erepro_floor_gt1), "of", n, "\n")
cat("  cannot reach reproduction level", REPRO_LEVEL, "within the cap:",
    sum(df$erepro_capped), "of", n, "\n")
cat("  recalibration infeasible -> R_max = Inf, penguins settle below control:",
    sum(df$repro_infeasible), "of", n, "\n")
cat("  -> for these the correction cannot restore control recruitment; report,\n")
cat("     do not treat as a coding failure\n")

cat("\n=== hard constraints, corrected arm ===\n")
ok <- df$trt_erepro_ok[!is.na(df$trt_erepro_ok)]
cat("  erepro < 1 for all 19, every member:",
    if (length(ok) && all(ok)) "PASS" else "CHECK", "\n")
cat("  penguin R_max finite (feasible members only):",
    if (all(df$R_max_set_finite[!df$repro_infeasible])) "PASS" else "FAIL", "\n")
cat("  new R_max = Inf vs control         :",
    sum(df$trt_rmax_inf > df$ctl_rmax_inf, na.rm = TRUE), "member(s)\n")

cat("\n=== acceptance among FEASIBLE members only ===\n")
fz <- df[!df$repro_infeasible, ]
if (nrow(fz)) {
  print(table(control = fz$ctl_pass, corrected = fz$trt_pass))
  cat(sprintf("  control %d/%d | corrected %d/%d\n",
              sum(fz$ctl_pass), nrow(fz), sum(fz$trt_pass), nrow(fz)))
}

cat("\nelapsed:", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")