# =============================================================================
# Phase 98 -- raise whale GAMMA by 25% and ask whether recovery is released
#
# THE HYPOTHESIS. Phase 94 established that whale recovery is BLOCKED, not slow:
# with all fishing stopped for 90 years the median member closes under 4% of the
# baleen depletion gap and under 0.1% of sperm's. Phase 95 then ruled out
# reproduction as the constraint -- taking baleen's reproduction level from 0.386
# to 0.006, an order of magnitude more unclamped than minke's, moved the 2010
# stock 7.5% and left depletion invariant at 0.59 across a 100x scaling range.
# What was left pointed at the growth/mortality side.
#
# gamma is the search-volume coefficient: encounter = search_vol * available
# prey, so raising gamma raises intake, growth and the feeding level. If whales
# are growth-limited rather than recruitment-limited, +25% gamma should show up
# as a faster post-whaling rebound.
#
# ------------------------------------------------------------ THE PLACEMENT
# Applied to the three whale groups immediately after the draws and BEFORE the
# first steady(preserve = "erepro") in the ramp, which is where the perturbed
# state first settles. Applying it later would let the member equilibrate at the
# unmodified gamma first and then jolt it, which is a different experiment.
#
# THE ext_encounter TRAP. `species_params(p) <- ...` SILENTLY ZEROES
# @ext_encounter, and the phase-57 out-of-domain subsidy lives there. It is
# saved before and restored after, and the restoration is asserted -- phase 88's
# worker does the same thing for its gamma-draw branch.
#
# ------------------------------------------------------------------ the arms
# P98_GAMMA_MULTS, default "1.25". Re-running the protocol REORDERS BASINS, so
# a 1.25 arm is only strictly interpretable against a 1.0 arm run through this
# same path -- pass "1,1.25" for the paired control at double the runtime. The
# default is the single arm the experiment asked for; phase 95's x1 arm on the
# TOP cut is the nearest existing control.
#
# ------------------------------------------------------------------ output
# Unlike phase 95 this stores the ANNUAL biomass trajectory for a species subset
# as well as the endpoints, because "does it recover" is a question about the
# SHAPE of the post-whaling curve, not about 2010 alone.
#
# USAGE  Rscript R/wmin_test/98_whale_gamma_test.R run
# ENV    P98_GAMMA_MULTS, P98_N, P98_CORES, P98_DRAWS, P98_OUT, P98_TRAJ_SPECIES
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OL <- "Output_large_files/wmin_test"
BASE_FILE <- Sys.getenv("P98_BASE", "params_ref_p86_agemat.rds")
CORES <- min(as.integer(Sys.getenv("P98_CORES", "14")),
             max(1L, parallel::detectCores() - 1L))
GMULTS <- as.numeric(trimws(strsplit(Sys.getenv("P98_GAMMA_MULTS", "1.25"),
                                     ",")[[1]]))
OUT <- Sys.getenv("P98_OUT", file.path(OL, "98_whale_gamma_test.rds"))
WH_SPECIES <- c("baleen whales", "minke whales", "sperm whales")
TRAJ_SP <- trimws(strsplit(Sys.getenv("P98_TRAJ_SPECIES",
  "baleen whales,minke whales,sperm whales,antarctic krill"), ",")[[1]])

# the phase-88 protocol constants, transcribed
RAMP_TOL <- 0.01; RAMP_PRESERVE <- "erepro"; FINAL_PRESERVE <- "R_max"
TOLS <- c(0.01, 0.005, 0.002, 0.001); RECAP <- 0.9
STEADY_TMAX <- 1500L; RAMP_TMAX <- 300L; SPINUP_YEARS <- 118L; QMAX <- 1
STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

# members: the phase-88 USABLE set, i.e. phase 93's FULL cut
RR <- readRDS(file.path(OL, "93_rerank_p88.rds"))
members <- as.integer(RR$cuts[["FULL usable"]])
if (nzchar(Sys.getenv("P98_N")))
  members <- head(members, as.integer(Sys.getenv("P98_N")))
if (nzchar(Sys.getenv("P98_MEMBERS")))
  members <- as.integer(trimws(strsplit(Sys.getenv("P98_MEMBERS"), ",")[[1]]))

DRAWS_F <- Sys.getenv("P98_DRAWS",
                      file.path(OL, "87_member_draws_substituted.rds"))
DR <- readRDS(DRAWS_F)
MULT_Q <- readRDS(file.path(OL, "89_refit_results.rds"))$M
effort_arr <- readRDS("effort_array_1841_2010.rds")
obs <- read.csv("yield_observed_timeseries.csv", check.names = FALSE)

BASE <- suppressWarnings(validParams(readRDS(BASE_FILE)))
stopifnot(identical(BASE@rates_funcs$Encounter, "therMizerEncounter"),
          !all(BASE@ext_encounter == 0))
SPN <- BASE@species_params$species
GAMMA0 <- BASE@species_params$gamma
stopifnot(!anyNA(match(c(WH_SPECIES, TRAJ_SP), SPN)))

cat("=== Phase 98: whale gamma test ===\n")
cat("base:", BASE_FILE, "| draws:", basename(DRAWS_F), "\n")
cat("members:", length(members), "| gamma multipliers:",
    paste(GMULTS, collapse = ", "), "on", paste(WH_SPECIES, collapse = ", "), "\n")
cat("cores:", CORES, "of", parallel::detectCores(), "detected\n\n")

worker <- function(job) {
  suppressPackageStartupMessages({library(mizer); library(therMizer)})
  si <- job$si; GM <- job$gm
  dw_ <- DR$draws[[as.character(si)]]
  fail <- function(r) list(sim_index = si, gamma_mult = GM, ok = FALSE, reason = r)

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

  # --- THE EXPERIMENT: whale gamma, before the ramp's first steady() ---------
  # species_params<- ZEROES ext_encounter (phase 57). Restore and assert.
  if (GM != 1) {
    spm <- species_params(p)
    iw <- match(WH_SPECIES, spm$species)
    spm$gamma[iw] <- spm$gamma[iw] * GM
    species_params(p) <- spm
    p@ext_encounter <- ext0
    if (!isTRUE(all.equal(p@ext_encounter, ext0)))
      return(fail("subsidy_lost_in_gamma"))
    if (!isTRUE(all.equal(p@species_params$gamma[iw], GAMMA0[iw] * GM)))
      return(fail("gamma did not take"))
  }
  gamma_applied <- p@species_params$gamma

  sc <- as.numeric(dw_$abundance_scaling[SPN])
  if (any(!is.finite(sc)) || any(sc <= 0)) return(fail("bad_abundance_scaling"))

  # --- ramp (K = 1) ----------------------------------------------------------
  for (j in seq_along(sc)) p@initial_n[j, ] <- p@initial_n[j, ] * sc[j]
  st <- steady_guarded(p, RAMP_TOL, RAMP_PRESERVE, RAMP_TMAX)
  if (st$errored) return(fail("steady_error_ramp"))
  p <- st$params

  # --- recap, then R_max postcap scaling -------------------------------------
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

  # --- historical projection, fished and unfished ----------------------------
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

  # annual trajectory for the subset -- "does it recover" is about the SHAPE
  yrf <- as.numeric(rownames(bf)); yru <- as.numeric(rownames(bu))
  traj <- do.call(rbind, lapply(TRAJ_SP, function(s) data.frame(
    sim_index = si, gamma_mult = GM, species = s, Year = yrf,
    fished = as.numeric(bf[, s]),
    unfished = as.numeric(bu[match(yrf, yru), s]), stringsAsFactors = FALSE)))

  list(sim_index = si, gamma_mult = GM, ok = TRUE, reason = "ok",
       tightest_tol = keep_tol, stable = isTRUE(stab$stable),
       max_cv = stab$max_cv, max_erepro = max(spq$erepro),
       n_erepro_ge1 = sum(spq$erepro >= 1),
       trajectory = traj,
       per_species = data.frame(
         sim_index = si, gamma_mult = GM, species = SPN,
         gamma = spq$gamma, gamma_vs_base = spq$gamma / GAMMA0,
         repro_level = rlev(ps), erepro = spq$erepro, R_max = spq$R_max,
         biom_1841 = as.numeric(init %*% (ps@w * ps@dw)),
         biom_2010_fished = as.numeric(bf["2010", ]),
         biom_2010_unfished = as.numeric(bu[nrow(bu), ]),
         yield_total = as.numeric(colSums(yy)), stringsAsFactors = FALSE))
}

jobs <- unlist(lapply(GMULTS, function(g)
  lapply(members, function(si) list(si = si, gm = g))), recursive = FALSE)
cat("jobs:", length(jobs), "\n\n")
t0 <- proc.time()
cl <- makeCluster(min(CORES, length(jobs)))
on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
clusterExport(cl, c("BASE", "DR", "SPN", "GAMMA0", "WH_SPECIES", "TRAJ_SP",
                    "MULT_Q", "effort_arr", "QMAX", "TOLS", "RECAP", "RAMP_TOL",
                    "RAMP_PRESERVE", "FINAL_PRESERVE", "STEADY_TMAX",
                    "RAMP_TMAX", "SPINUP_YEARS", "STAB", "worker"),
              envir = environment())
res <- parLapplyLB(cl, jobs, function(j)
  tryCatch(worker(j), error = function(e)
    list(sim_index = j$si, gamma_mult = j$gm, ok = FALSE,
         reason = conditionMessage(e))))
stopCluster(cl)

ok <- vapply(res, function(r) isTRUE(r$ok), logical(1))
cat("built", sum(ok), "of", length(res), "|",
    round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")
if (any(!ok)) print(sort(table(vapply(res[!ok], function(r)
  sub(":.*$", "", as.character(r$reason)), "")), decreasing = TRUE))

PS <- bind_rows(lapply(res[ok], `[[`, "per_species"))
TR <- bind_rows(lapply(res[ok], `[[`, "trajectory"))
MEMTAB <- bind_rows(lapply(res[ok], function(r) data.frame(
  sim_index = r$sim_index, gamma_mult = r$gamma_mult, stable = r$stable,
  tightest_tol = r$tightest_tol, max_cv = r$max_cv,
  max_erepro = r$max_erepro, n_erepro_ge1 = r$n_erepro_ge1)))
saveRDS(list(per_species = PS, trajectory = TR, members = MEMTAB,
             gamma_mults = GMULTS, whales = WH_SPECIES,
             meta = list(base = BASE_FILE, draws = DRAWS_F,
                         members = members, catchability = MULT_Q,
                         built = Sys.time())), OUT)
cat("\nWROTE", OUT, "\n")
MEMTAB %>% group_by(gamma_mult) %>%
  summarise(built = n(), n_stable = sum(stable),
            n_usable = sum(stable & n_erepro_ge1 == 0), .groups = "drop") %>%
  as.data.frame() %>% print(row.names = FALSE)