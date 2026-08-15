# =============================================================================
# Phase 95 -- does DOUBLING the whale abundance/R_max scaling change the
# reproductive response, or only the units?
#
# THE QUESTION. On phase 88, minke whales deplete under exploitation and then
# recover when effort stops, while baleen and sperm do neither -- 90 unfished
# years close under 4% of their depletion gap (phase 94). Minke differs in two
# reproduction parameters: erepro 0.0226 against baleen's 0.000367, and a
# reproduction level of 0.0069 against baleen's 0.506. Post-hoc tests on the
# best-ranked member showed R_max alone is NOT the lever (taking baleen's level
# to 0.0006 moved the 2010 stock 7.5%), while erepro IS -- but erepro x10 makes
# the baleen stock GROW through the whaling era, destroying the depletion signal.
#
# So the open question is whether raising the scaling that phase 88 ALREADY
# applies -- the drawn abundance_scaling, which sets both the ramp target and,
# postcap, the R_max multiplier -- buys the minke-like dynamic, or only a bigger
# stock.
#
# THE THEORETICAL EXPECTATION, stated so the test can refute it. Under
# Beverton-Holt, RDD = R_max*RDI/(R_max + RDI). Scaling initial_n by F scales
# RDI by F; scaling R_max by F as well gives RDD -> F*RDD, so
# reproduction_level = RDD/R_max is INVARIANT. Joint scaling should therefore be
# a change of units, leaving relative depletion and recovery untouched, and
# changing only how much whale there is to catch. It is not exactly invariant in
# a multispecies model -- more whales eat more krill, and that feeds back -- so
# the size of the departure is the actual result.
#
# ------------------------------------------------------------------ the design
# The 43 members of the phase-93 TOP cut, each rebuilt through the FULL phase-88
# protocol -- draws, ramp with steady() at each step, recap, R_max postcap
# scaling, tolerance ladder, 118-year unfished spin-up -- then projected
# 1841-2010 with the phase-89 catchability.
#
# TWO ARMS, AND THE CONTROL IS NOT OPTIONAL:
#   x1  the existing scaling, rebuilt through this same path
#   x2  the whale entries of abundance_scaling doubled
#
# Re-running the protocol REORDERS BASINS -- several members are bistable and
# steady() picks the basin, which is why the stored ensemble cannot be
# regenerated. The x1 arm is therefore not redundant with phase 88's stored
# members: it is the only way to separate the effect of doubling from the noise
# of having re-run at all. Compare x2 against x1, never against phase 88.
#
# The worker is phase 88's, transcribed, with ONE addition: WH_MULT applied to
# the whale entries of `sc` before the ramp. Because `sc` drives both the ramp
# target and the postcap `R_max * sc`, that single line doubles initial_n and
# R_max together, which is exactly what phase 88 already does at x1.
#
# USAGE  Rscript R/wmin_test/95_whale_scale_test.R [run|collect]
# ENV    P95_MULTS (default "1,2"), P95_MEMBERS, P95_CORES, P95_OUT, P95_BASE
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OL <- "Output_large_files/wmin_test"
BASE_FILE <- Sys.getenv("P95_BASE", "params_ref_p86_agemat.rds")
CORES <- min(as.integer(Sys.getenv("P95_CORES", "10")),
             max(1L, parallel::detectCores() - 2L))
MULTS <- as.numeric(trimws(strsplit(Sys.getenv("P95_MULTS", "1,2"), ",")[[1]]))
OUT <- Sys.getenv("P95_OUT", file.path(OL, "95_whale_scale_test.rds"))
WH_SPECIES <- c("baleen whales", "minke whales", "sperm whales")
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"

# --- the phase-88 protocol constants, transcribed ----------------------------
STEPS_REQ <- 1L; MAX_STEP_X <- 1e9; MAX_STEPS <- 40L
RAMP_TOL <- 0.01; RAMP_PRESERVE <- "erepro"; FINAL_PRESERVE <- "R_max"
TOLS <- c(0.01, 0.005, 0.002, 0.001); RECAP <- 0.9
STEADY_TMAX <- 1500L; RAMP_TMAX <- 300L; SPINUP_YEARS <- 118L
QMAX <- 1
STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

RR <- readRDS(file.path(OL, "93_rerank_p88.rds"))
TOP_NAME <- grep("^TOP ", names(RR$cuts), value = TRUE)[1]
members <- as.integer(RR$cuts[[TOP_NAME]])
# P95_N takes the top N of the phase-93 RANKING instead of the cut, for a larger
# pilot. The ranking is ascending yield RMSE over the usable set, so the top N is
# a superset of the cut whenever N exceeds it.
if (nzchar(Sys.getenv("P95_N"))) {
  NN <- as.integer(Sys.getenv("P95_N"))
  # P95_SKIP takes a later slice of the ranking, so successive pilots cover
  # disjoint members and can be pooled rather than repeating the same ones.
  SK <- as.integer(Sys.getenv("P95_SKIP", "0"))
  all_r <- as.integer(RR$ranking$sim_index)
  if (SK >= length(all_r)) stop("P95_SKIP past the end of the ranking",
                                call. = FALSE)
  members <- all_r[(SK + 1):min(SK + NN, length(all_r))]
  TOP_NAME <- sprintf("ranking rows %d-%d of %d", SK + 1, SK + length(members),
                      length(all_r))
}
if (nzchar(Sys.getenv("P95_MEMBERS")))
  members <- as.integer(trimws(strsplit(Sys.getenv("P95_MEMBERS"), ",")[[1]]))

# P95_DRAWS: which substituted draws to build from. The phase-87 file is what
# phase 88 used; phase 96 lifts the lower groups and widens the whales. With a
# phase-96 file the scaling multiplier is normally 1, because the draws already
# carry the change -- MULTS is then only for stacking a further factor on top.
DRAWS_F <- Sys.getenv("P95_DRAWS", file.path(OL, "87_member_draws_substituted.rds"))
if (!file.exists(DRAWS_F)) stop("missing draws: ", DRAWS_F, call. = FALSE)
DR <- readRDS(DRAWS_F)
cat("draws:", basename(DRAWS_F), "\n")
MULT_Q <- readRDS(file.path(OL, "89_refit_results.rds"))$M
effort_arr <- readRDS("effort_array_1841_2010.rds")
obs <- read.csv("yield_observed_timeseries.csv", check.names = FALSE)

BASE <- suppressWarnings(validParams(readRDS(BASE_FILE)))
stopifnot(identical(BASE@rates_funcs$Encounter, "therMizerEncounter"),
          !all(BASE@ext_encounter == 0))
SPN <- BASE@species_params$species
GAMMA0 <- BASE@species_params$gamma
stopifnot(!anyNA(match(WH_SPECIES, SPN)))

cat("=== Phase 95: whale scaling test ===\n")
cat("base:", BASE_FILE, "| cut:", TOP_NAME, "| members:", length(members), "\n")
cat("multipliers:", paste(MULTS, collapse = ", "), "on",
    paste(WH_SPECIES, collapse = ", "), "\n")
cat("cores:", CORES, "\n\n")

# ------------------------------------------------------------------ worker ---
# Phase 88's worker, transcribed, plus WH_MULT on the whale entries of `sc`.
worker <- function(job) {
  suppressPackageStartupMessages({library(mizer); library(therMizer)})
  si <- job$si; F <- job$mult
  dw_ <- DR$draws[[as.character(si)]]
  fail <- function(r) list(sim_index = si, mult = F, ok = FALSE, reason = r)

  steady_guarded <- function(p, tol, preserve, tmax = STEADY_TMAX) {
    nc <- FALSE
    out <- withCallingHandlers(
      try(steady(p, tol = tol, t_max = tmax, preserve = preserve,
                 progress_bar = FALSE), silent = TRUE),
      message = function(m) {
        if (grepl("did not converge", conditionMessage(m), ignore.case = TRUE))
          nc <<- TRUE
        invokeRestart("muffleMessage")
      }, warning = function(w) invokeRestart("muffleWarning"))
    list(params = out, converged = !nc, errored = inherits(out, "try-error"))
  }
  rlev <- function(p) {
    r <- try(as.numeric(getReproductionLevel(p)), silent = TRUE)
    if (inherits(r, "try-error")) rep(NA_real_, length(SPN)) else r
  }
  check_stability <- function(s) {
    bm <- getBiomass(s); yrs <- as.numeric(rownames(bm)); nT <- nrow(bm)
    if (nT < 5) return(list(stable = FALSE, max_cv = NA_real_))
    tm <- tail(bm, max(5, min(STAB$check_years_tail, nT)))
    hn <- max(5, min(STAB$trend_first_years, nT))
    hm <- head(bm, hn); hy <- head(yrs, hn)
    mt <- colMeans(tm, na.rm = TRUE)
    cv <- ifelse(mt > 0, apply(tm, 2, sd, na.rm = TRUE) / mt, Inf)
    rs <- setNames(rep(NA_real_, ncol(hm)), colnames(hm)); pv <- rs
    for (q in seq_len(ncol(hm))) {
      y <- hm[, q]; mu <- mean(y, na.rm = TRUE)
      if (!is.finite(mu) || mu < STAB$min_mean_biomass) { rs[q] <- 0; pv[q] <- 1; next }
      ft <- try(suppressWarnings(lm(b ~ t, data.frame(t = hy, b = as.numeric(y)))),
                silent = TRUE)
      if (inherits(ft, "try-error")) { rs[q] <- 0; pv[q] <- 1 } else {
        rs[q] <- as.numeric(coef(ft)[["t"]]) / mu
        pp <- try(summary(ft)$coefficients["t", "Pr(>|t|)"], silent = TRUE)
        pv[q] <- if (inherits(pp, "try-error")) 1 else as.numeric(pp)
      }
    }
    list(stable = !any((cv > STAB$cv_threshold) |
           ((abs(rs) > STAB$trend_rel_slope_max) & (pv < STAB$trend_pval_max)),
           na.rm = TRUE),
         max_cv = max(cv[is.finite(cv)], na.rm = TRUE))
  }

  p <- BASE; ext0 <- p@ext_encounter
  gp <- gear_params(p)
  if (!identical(paste(gp$gear, gp$species), names(dw_$catchability)))
    return(fail("gear_params rows differ"))
  gp$catchability <- as.numeric(dw_$catchability); gear_params(p) <- gp
  if (!isTRUE(all.equal(p@ext_encounter, ext0))) return(fail("subsidy_lost"))

  sc <- as.numeric(dw_$abundance_scaling[SPN])
  if (any(!is.finite(sc)) || any(sc <= 0)) return(fail("bad_abundance_scaling"))
  # THE ONE ADDITION. `sc` drives both the ramp target and the postcap
  # R_max * sc, so this doubles initial_n and R_max together for the whales.
  sc[match(WH_SPECIES, SPN)] <- sc[match(WH_SPECIES, SPN)] * F

  # --- ramp (K = 1: the single jump) ----------------------------------------
  K <- 1L
  for (k in seq_len(K)) {
    inc <- sc^(k / K) / sc^((k - 1) / K)
    for (j in seq_along(inc)) p@initial_n[j, ] <- p@initial_n[j, ] * inc[j]
    st <- steady_guarded(p, RAMP_TOL, RAMP_PRESERVE, RAMP_TMAX)
    if (st$errored) return(fail(sprintf("steady_error_step%d", k)))
    p <- st$params
  }
  # --- recap, then R_max postcap scaling ------------------------------------
  rl_in <- rlev(p)
  p2 <- try(suppressWarnings(
    setBevertonHolt(p, reproduction_level = pmin(rl_in, RECAP))), silent = TRUE)
  if (inherits(p2, "try-error")) return(fail("recap_error"))
  p <- p2
  spm <- species_params(p); spm$R_max <- spm$R_max * sc
  species_params(p) <- spm; p@ext_encounter <- ext0
  if (!isTRUE(all.equal(p@ext_encounter, ext0))) return(fail("subsidy_lost_rmax"))

  keep_p <- NULL; keep_tol <- NA_real_
  for (tol in TOLS) {
    st <- steady_guarded(p, tol, FINAL_PRESERVE)
    if (st$errored) break
    p <- st$params
    if (st$converged) { keep_p <- p; keep_tol <- tol }
  }
  if (is.null(keep_p)) return(fail("no_rung_converged"))
  ps <- keep_p

  s0 <- try(project(ps, t_start = 1841, t_max = SPINUP_YEARS, effort = 0,
                    progress_bar = FALSE), silent = TRUE)
  if (inherits(s0, "try-error")) return(fail("spinup_error"))
  init <- s0@n[SPINUP_YEARS, , ]
  stab <- check_stability(s0)

  # --- historical projection with the phase-89 catchability -----------------
  pf <- ps
  gpf <- gear_params(pf)
  mq <- MULT_Q[match(gpf$species, names(MULT_Q))]; mq[is.na(mq)] <- 1
  gpf$catchability <- pmin(QMAX, pmax(0, gpf$catchability * mq))
  gear_params(pf) <- gpf
  sf <- try(project(pf, initial_n = init, t_start = 1841, effort = effort_arr,
                    progress_bar = FALSE), silent = TRUE)
  su <- try(project(pf, initial_n = init, t_start = 1841,
                    t_max = nrow(effort_arr) - 1, effort = 0,
                    progress_bar = FALSE), silent = TRUE)
  if (inherits(sf, "try-error") || inherits(su, "try-error"))
    return(fail("projection_error"))
  bf <- getBiomass(sf); bu <- getBiomass(su); yy <- getYield(sf)
  spq <- species_params(ps)

  list(sim_index = si, mult = F, ok = TRUE, reason = "ok",
       tightest_tol = keep_tol, stable = isTRUE(stab$stable),
       max_cv = stab$max_cv, max_erepro = max(spq$erepro),
       n_erepro_ge1 = sum(spq$erepro >= 1),
       per_species = data.frame(
         sim_index = si, mult = F, species = SPN,
         repro_level = rlev(ps), erepro = spq$erepro, R_max = spq$R_max,
         abundance_scaling = sc,
         biom_1841 = as.numeric(init %*% (ps@w * ps@dw)),
         biom_2010_fished = as.numeric(bf["2010", ]),
         biom_2010_unfished = as.numeric(bu[nrow(bu), ]),
         yield_total = as.numeric(colSums(yy)),
         stringsAsFactors = FALSE))
}

if (mode == "run") {
  jobs <- unlist(lapply(MULTS, function(f)
    lapply(members, function(si) list(si = si, mult = f))), recursive = FALSE)
  cat("jobs:", length(jobs), "\n\n")
  t0 <- proc.time()
  cl <- makeCluster(min(CORES, length(jobs)))
  on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
  clusterExport(cl, c("BASE", "DR", "SPN", "GAMMA0", "WH_SPECIES", "MULT_Q",
                      "effort_arr", "QMAX", "TOLS", "RECAP", "RAMP_TOL",
                      "RAMP_PRESERVE", "FINAL_PRESERVE", "STEADY_TMAX",
                      "RAMP_TMAX", "SPINUP_YEARS", "STAB", "worker"),
                envir = environment())
  res <- parLapplyLB(cl, jobs, function(j)
    tryCatch(worker(j), error = function(e)
      list(sim_index = j$si, mult = j$mult, ok = FALSE,
           reason = conditionMessage(e))))
  stopCluster(cl)
  ok <- vapply(res, function(r) isTRUE(r$ok), logical(1))
  cat("built", sum(ok), "of", length(res), "|",
      round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")
  if (any(!ok)) print(sort(table(vapply(res[!ok], function(r)
    sub(":.*$", "", as.character(r$reason)), "")), decreasing = TRUE))
  PS <- bind_rows(lapply(res[ok], `[[`, "per_species"))
  MEMTAB <- bind_rows(lapply(res[ok], function(r) data.frame(
    sim_index = r$sim_index, mult = r$mult, stable = r$stable,
    tightest_tol = r$tightest_tol, max_cv = r$max_cv,
    max_erepro = r$max_erepro, n_erepro_ge1 = r$n_erepro_ge1)))
  saveRDS(list(per_species = PS, members = MEMTAB, mults = MULTS,
               whales = WH_SPECIES, cut = TOP_NAME,
               meta = list(base = BASE_FILE, members = members, draws = DRAWS_F,
                           catchability = MULT_Q, built = Sys.time())), OUT)
  cat("WROTE", OUT, "\n")
}