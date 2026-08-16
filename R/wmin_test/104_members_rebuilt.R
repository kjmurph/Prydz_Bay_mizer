# =============================================================================
# Phase 104 -- the member protocol, rebuilt. Replaces 88_full_ensemble.R.
#
# Phase 88's protocol is retained wherever it was not measured to be wrong. FIVE
# things change, each because a measurement said so, and each is listed with the
# number that forced it. Everything else is held: the draws, the catchability,
# K = 1, matchGrowth OFF, the tolerance ladder and its preserve = "R_max", and
# the stability thresholds.
#
# ------------------------------------------------------------ WHAT CHANGED
# 1. R_max IS SCALED AT THE RAMP, WITH initial_n -- not postcap (phase 103).
#    Scaling both by the same factor is a scale transformation that leaves
#    RDD/R_max invariant, so the ramp can actually reach the drawn abundance.
#    Measured on full 19-species draws, whale groups, median:
#
#      placement        realised/drawn  repro level  R_max x  drift 200yr
#      postcap (p88)      0.30-0.63       0.25-0.51   9-32     1.21-1.75
#      ramp               1.05-1.29       0.90        8.7-31   1.004-1.008
#      ramp + postcap     6.1-9.8         0.25-0.54   84-916   1.01-1.03
#
#    NEVER DO BOTH: ramp + postcap SQUARES the multiplier (baleen R_max x249
#    against ramp's 15.9). Under `ramp` the ramp steady() converged on every
#    member; under `postcap`, on half.
#
# 2. THE RAMP steady() BUDGET IS tol 0.001 / t_max 1000, not 0.01 / 300.
#    At the old settings a x5 baleen perturbation returns 4.78 of 5 and reports
#    CONVERGENCE, and that state then collapses to 0.21 over 200 years. At the
#    new settings it returns 1.106 and holds at 0.9917. Cost: 2.5x the runtime.
#
# 3. RECAP IS DRAWN PER MEMBER, U(0.1, 0.9), WHALE GROUPS ONLY.
#    Under `ramp` the reproduction level lands exactly at the cap, so RECAP
#    stops being an overwritten intermediate and becomes the ensemble's density
#    dependence. It maps to Beverton-Holt steepness as h = 0.2/(1 - 0.8L):
#    L 0.1 -> h 0.217, 0.5 -> 0.333, 0.9 -> 0.714. It is drawn rather than fixed
#    because it is genuinely unconstrained and it governs the result -- across
#    the range, baleen depletion spans 0.58-0.95 and recovery 27-97%.
#
#    APPLIED AS pmin(level, RECAP) VIA A NAMED VECTOR, so groups already below
#    the draw keep their own value and the other 15 groups are untouched.
#
#    !! THE YIELD OBJECTIVE CANNOT SELECT ON THIS AXIS. A 7.7x change in baleen
#    catch moves the yield RMSE 0.3%, so the ranking will pass a U(0.1,0.9) draw
#    through almost untouched and the ensemble median will be the PRIOR median.
#    Either the objective gains a whale-sensitive term, or RECAP must be reported
#    as an unconstrained propagated uncertainty. Do not claim it was fitted.
#
# 4. THE SPIN-UP IS t_max = 120 AND INDEXES THE LAST ROW.
#    project(t_start = 1841, t_max = N) returns N+1 rows, so phase 88's
#    s0@n[118, , ] is year 1958 -- 117 elapsed years, landing at ENSO cycle
#    position 17 of 20. The ISIMIP3a spin-up is ctrlclim 1961-1980 repeated six
#    times over 1841-1960 (verified bit-for-bit against this model's arrays), so
#    120 elapsed years is exactly six cycles and lands on position 0.
#
# 5. A DRIFT SCREEN IS ADDED. The phase-88 screen tests CV over the last 40 yr
#    of the spin-up and a trend over the first 50. Phase-99 drift ran at
#    0.0013/yr -- far inside both thresholds -- while 56% of members lost a
#    species entirely over 2000 years. Drift is now measured directly: the
#    post-spin-up state is projected DRIFT_YEARS under the repeated ENSO cycle
#    with zero effort, and the per-species ratio is recorded. steady()'s own
#    convergence report is NOT evidence; it reported success in every failing
#    case above.
#
# ------------------------------------------------------------------- the base
# DEFAULTS TO params_ref_p100_mort_kernel_diet.rds, the production reference.
# Set P104_BASE=params_ref_p86_agemat.rds for the CONTROL arm, which isolates
# the protocol change from the reference change: p86 + phase 88 gave 13/20
# usable and p100 + phase 88 gave 2/20, so neither alone identifies the cause.
#
# USAGE  Rscript R/wmin_test/104_members_rebuilt.R [run|collect|status]
# ENV    P104_BASE, P104_N, P104_CORES, P104_STEM, P104_DRAWS, P104_MEMBERS,
#        P104_SEED, P104_RECAP_LO, P104_RECAP_HI, P104_RECAP_SPECIES,
#        P104_RAMP_TOL, P104_RAMP_TMAX, P104_DRIFT_YEARS
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OL <- "Output_large_files/wmin_test"
BASE_FILE <- Sys.getenv("P104_BASE", "params_ref_p100_mort_kernel_diet.rds")
N       <- as.integer(Sys.getenv("P104_N", "20"))
CORE_LIMIT <- max(1L, parallel::detectCores() - 2L)   # never saturate
CORES   <- min(as.integer(Sys.getenv("P104_CORES", "10")), CORE_LIMIT, max(1, N))
STEM    <- Sys.getenv("P104_STEM", "104_members")

# --- CHANGE 1: R_max at the ramp. Not an option; ramp+postcap squares it. -----
# --- CHANGE 2: the ramp budget -----------------------------------------------
RAMP_TOL   <- as.numeric(Sys.getenv("P104_RAMP_TOL", "0.001"))
RAMP_TMAX  <- as.integer(Sys.getenv("P104_RAMP_TMAX", "1000"))
RAMP_PRESERVE <- "erepro"
# --- held from phase 88 ------------------------------------------------------
TOLS    <- c(0.01, 0.005, 0.002, 0.001)
FINAL_PRESERVE <- "R_max"
STEADY_TMAX <- 1500L
K <- 1L                                   # the single jump; phase 88's default
# --- CHANGE 3: the drawn reproduction cap ------------------------------------
SEED      <- as.integer(Sys.getenv("P104_SEED", "20260816"))
RECAP_LO  <- as.numeric(Sys.getenv("P104_RECAP_LO", "0.1"))
RECAP_HI  <- as.numeric(Sys.getenv("P104_RECAP_HI", "0.9"))
RECAP_SPECIES <- trimws(strsplit(Sys.getenv("P104_RECAP_SPECIES",
  "baleen whales,sperm whales,minke whales,orca"), ",")[[1]])
stopifnot(RECAP_LO > 0, RECAP_HI < 1, RECAP_HI > RECAP_LO)
# --- CHANGE 4: the spin-up ---------------------------------------------------
SPINUP <- 120L                            # six whole ENSO cycles, LAST row
# --- CHANGE 5: the drift screen ----------------------------------------------
DRIFT_YEARS <- as.integer(Sys.getenv("P104_DRIFT_YEARS", "200"))
DRIFT_LO <- 0.5; DRIFT_HI <- 2.0
ENSO_FROM <- 1961L; ENSO_TO <- 1980L
STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"
stopifnot(mode %in% c("run", "collect", "status"))

t0 <- proc.time()
cat("=== Phase 104: member rebuild ===\n")
if (!file.exists(BASE_FILE)) stop("missing base: ", BASE_FILE, call. = FALSE)
BASE <- suppressWarnings(validParams(readRDS(BASE_FILE)))
stopifnot(identical(BASE@rates_funcs$Encounter, "therMizerEncounter"),
          identical(BASE@resource_dynamics, "plankton_forcing"))
if (all(BASE@ext_encounter == 0))
  stop("base carries no out-of-domain subsidy -- wrong reference model",
       call. = FALSE)
SPN <- BASE@species_params$species
WDW <- BASE@w * BASE@dw
BIOM_BASE <- as.numeric(BASE@initial_n %*% WDW)
if (length(setdiff(RECAP_SPECIES, SPN)))
  stop("P104_RECAP_SPECIES not in the model: ",
       paste(setdiff(RECAP_SPECIES, SPN), collapse = ", "), call. = FALSE)

# --- members: the phase-45 ranking ENUMERATES, it does not select -------------
RF <- readRDS(file.path(OL, "45_refit_results.rds"))
RANK <- RF$per_species %>% group_by(sim_index) %>%
  summarise(m = sqrt(sum(sse) / sum(n)), .groups = "drop") %>% arrange(m)
members <- as.integer(head(RANK$sim_index, N))
if (nzchar(Sys.getenv("P104_MEMBERS")))
  members <- as.integer(trimws(strsplit(Sys.getenv("P104_MEMBERS"), ",")[[1]]))
N <- length(members); CORES <- min(CORES, max(1, N))

DRAWS_F <- Sys.getenv("P104_DRAWS",
                      file.path(OL, "87_member_draws_substituted.rds"))
DR <- readRDS(DRAWS_F)
if (length(setdiff(as.character(members), names(DR$draws))))
  stop("missing draws for ", length(setdiff(as.character(members),
       names(DR$draws))), " members", call. = FALSE)

# --- CHANGE 3: draw RECAP, seeded and recorded so this is reproducible --------
set.seed(SEED)
RECAP_DRAW <- setNames(runif(length(members), RECAP_LO, RECAP_HI),
                       as.character(members))

cat("base:", BASE_FILE, "\n")
cat("members:", N, "| cores:", CORES, "of", parallel::detectCores(), "\n")
cat("R_max scaled at the RAMP (never ramp+postcap -- that squares it)\n")
cat("ramp steady: tol", RAMP_TOL, "t_max", RAMP_TMAX, "preserve", RAMP_PRESERVE,
    "\n")
cat("ladder:", paste(TOLS, collapse = " -> "), "preserve", FINAL_PRESERVE,
    "t_max", STEADY_TMAX, "\n")
cat(sprintf("RECAP ~ U(%.2f, %.2f), seed %d, applied to: %s\n",
            RECAP_LO, RECAP_HI, SEED, paste(RECAP_SPECIES, collapse = ", ")))
cat(sprintf("  drawn range %.3f - %.3f (median %.3f)\n", min(RECAP_DRAW),
            max(RECAP_DRAW), median(RECAP_DRAW)))
cat("  NOTE the yield objective cannot select on this axis -- report it as a\n")
cat("  propagated uncertainty, not as a fitted parameter.\n")
cat("spin-up:", SPINUP, "yr (six ENSO cycles), indexing the LAST row\n")
cat("drift screen:", DRIFT_YEARS, "yr under the repeated", ENSO_FROM, "-",
    ENSO_TO, "cycle\n\n")

STATE_DIR <- file.path(OL, paste0(STEM, "_states"))
CHUNK_DIR <- file.path(OL, paste0(STEM, "_chunks"))
OUT_RDS   <- file.path(OL, paste0(STEM, ".rds"))
for (d in c(STATE_DIR, CHUNK_DIR))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------ worker ---
worker <- function(si) {
  # THE WORKER'S ENVIRONMENT IS BARE. Loading these here is not optional -- a
  # dplyr call inside a worker failed every job silently earlier in this work.
  suppressPackageStartupMessages({library(mizer); library(therMizer)})
  dw_ <- DR$draws[[as.character(si)]]
  fail <- function(r, extra = NULL)
    c(list(sim_index = si, ok = FALSE, reason = r), extra)

  # mizer 3.1.0 signals non-convergence with message(), not warning()
  steady_guarded <- function(p, tol, preserve, tmax) {
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
  biom <- function(p) as.numeric(p@initial_n %*% WDW)
  # CHANGE 4/5: the ISIMIP3a ENSO cycle, repeated and phase-aligned so model
  # year t_start is cycle position 0. Never pin a single year -- 1841 sits
  # 0.043 degC above the cycle mean and that alone invented a 39x minke drift.
  cycle_forcing <- function(p, n_years, t_start = 1841L) {
    o <- p@other_params$other
    oyr <- as.numeric(rownames(o$ocean_temp))
    src <- which(oyr >= ENSO_FROM & oyr <= ENSO_TO)
    yrs <- t_start + seq_len(n_years + 2L) - 1L
    idx <- src[((seq_along(yrs) - 1L) %% length(src)) + 1L]
    p@other_params$other$ocean_temp <-
      `rownames<-`(o$ocean_temp[idx, , drop = FALSE], yrs)
    p@other_params$other$n_pp_array <-
      `rownames<-`(o$n_pp_array[idx, , drop = FALSE], yrs)
    p
  }
  check_stability <- function(s) {              # phase 88's screen, unchanged
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
      if (!is.finite(mu) || mu < STAB$min_mean_biomass) {
        rs[q] <- 0; pv[q] <- 1; next
      }
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

  # --- 1. the draws ---------------------------------------------------------
  p <- BASE
  ext0 <- p@ext_encounter
  gp <- gear_params(p)
  if (!identical(paste(gp$gear, gp$species), names(dw_$catchability)))
    return(fail("gear_params rows differ between the base and the stored member"))
  gp$catchability <- as.numeric(dw_$catchability); gear_params(p) <- gp
  if (!isTRUE(all.equal(p@ext_encounter, ext0)))
    return(fail("subsidy_lost_in_catchability"))
  sc <- as.numeric(dw_$abundance_scaling[SPN])
  if (any(!is.finite(sc)) || any(sc <= 0)) return(fail("bad_abundance_scaling"))

  # --- 2. THE RAMP: initial_n AND R_max together (CHANGE 1) -----------------
  for (j in seq_along(sc)) p@initial_n[j, ] <- p@initial_n[j, ] * sc[j]
  spm <- species_params(p)
  spm$R_max <- spm$R_max * sc
  species_params(p) <- spm
  p@ext_encounter <- ext0                    # species_params<- can zero it
  if (!isTRUE(all.equal(p@ext_encounter, ext0)))
    return(fail("subsidy_lost_in_rmax_ramp_scale"))
  st <- steady_guarded(p, RAMP_TOL, RAMP_PRESERVE, RAMP_TMAX)   # CHANGE 2
  if (st$errored) return(fail("steady_error_ramp"))
  p <- st$params
  ramp_conv <- st$converged
  realised_ratio <- biom(p) / BIOM_BASE

  # --- 3. the drawn reproduction cap, whales only (CHANGE 3) ----------------
  rl_in <- rlev(p)
  recap_i <- RECAP_DRAW[[as.character(si)]]
  tgt <- pmin(rl_in[match(RECAP_SPECIES, SPN)], recap_i)
  names(tgt) <- RECAP_SPECIES                # NAMED -> only these are affected
  p2 <- try(suppressWarnings(setBevertonHolt(p, reproduction_level = tgt)),
            silent = TRUE)
  if (inherits(p2, "try-error")) return(fail("recap_error"))
  p <- p2
  p@ext_encounter <- ext0
  rl_capped <- rlev(p)
  # NO POSTCAP SCALING. Scaling R_max again here squares the multiplier.

  # --- 4. the tolerance ladder ---------------------------------------------
  keep_p <- NULL; keep_tol <- NA_real_
  for (tol in TOLS) {
    st <- steady_guarded(p, tol, FINAL_PRESERVE, STEADY_TMAX)
    if (st$errored) break
    p <- st$params
    if (st$converged) { keep_p <- p; keep_tol <- tol }
  }
  if (is.null(keep_p)) return(fail("no_rung_converged"))
  ps <- keep_p

  # --- 5. spin-up: 120 yr, LAST row (CHANGE 4) ------------------------------
  s0 <- try(project(ps, t_start = 1841, t_max = SPINUP, effort = 0,
                    progress_bar = FALSE), silent = TRUE)
  if (inherits(s0, "try-error")) return(fail("spinup_error"))
  init <- s0@n[dim(s0@n)[1], , ]             # NOT [SPINUP, , ] -- that is N-1 yr
  stab <- check_stability(s0)

  # --- 6. the drift screen (CHANGE 5) --------------------------------------
  drift <- rep(NA_real_, length(SPN))
  if (DRIFT_YEARS > 0) {
    pd <- cycle_forcing(ps, DRIFT_YEARS)
    sd_ <- try(project(pd, initial_n = init, t_start = 1841,
                       t_max = DRIFT_YEARS, effort = 0, progress_bar = FALSE),
               silent = TRUE)
    if (!inherits(sd_, "try-error")) {
      bm <- apply(sd_@n, 1, function(n) rowSums(sweep(n, 2, WDW, "*")))
      drift <- bm[, ncol(bm)] / bm[, 1]
    }
  }
  drift_ok <- all(is.finite(drift)) && all(drift >= DRIFT_LO & drift <= DRIFT_HI)

  saveRDS(list(sim_index = si, params = ps, initial_n = init,
               tightest_tol = keep_tol, recap = recap_i),
          file.path(STATE_DIR, sprintf("state_%05d.rds", si)))
  spq <- species_params(ps)
  list(sim_index = si, ok = TRUE, reason = "ok", tightest_tol = keep_tol,
       recap = recap_i, ramp_converged = ramp_conv,
       stable = isTRUE(stab$stable), max_cv = stab$max_cv,
       drift_ok = drift_ok, worst_drift_lo = min(drift), worst_drift_hi = max(drift),
       max_erepro = max(spq$erepro), n_erepro_ge1 = sum(spq$erepro >= 1),
       per_species = data.frame(sim_index = si, species = SPN,
         erepro = spq$erepro, R_max = spq$R_max, repro_level = rlev(ps),
         repro_level_preladder = rl_in, repro_level_capped = rl_capped,
         abundance_scaling = sc, realised_ratio = realised_ratio,
         drift = drift, recap = recap_i, stringsAsFactors = FALSE),
       biomass_1841 = data.frame(sim_index = si, species = SPN,
         biomass = as.numeric(init %*% WDW), stringsAsFactors = FALSE))
}

# --------------------------------------------------------------------- run ---
chunk_file <- function(ci) file.path(CHUNK_DIR, sprintf("res_%03d.rds", ci))
chunks <- split(members, ceiling(seq_along(members) / CORES))
if (mode == "status") {
  done <- sum(vapply(seq_along(chunks), function(ci) file.exists(chunk_file(ci)),
                     logical(1)))
  cat(sprintf("chunks %d/%d (%.1f%%) | %d states written\n", done,
              length(chunks), 100 * done / length(chunks),
              length(list.files(STATE_DIR))))
  quit(save = "no")
}
if (mode == "run") {
  todo <- which(!vapply(seq_along(chunks),
                        function(ci) file.exists(chunk_file(ci)), logical(1)))
  cat("chunks to run:", length(todo), "of", length(chunks), "\n\n")
  if (length(todo)) {
    cl <- makeCluster(CORES)
    on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
    clusterExport(cl, c("BASE", "DR", "SPN", "WDW", "BIOM_BASE", "RECAP_DRAW",
                        "RECAP_SPECIES", "RAMP_TOL", "RAMP_TMAX",
                        "RAMP_PRESERVE", "TOLS", "FINAL_PRESERVE",
                        "STEADY_TMAX", "SPINUP", "DRIFT_YEARS", "DRIFT_LO",
                        "DRIFT_HI", "ENSO_FROM", "ENSO_TO", "STAB",
                        # `worker` itself: it is called BY NAME inside the
                        # tryCatch wrapper below, so passing the wrapper does
                        # not carry it. Omitting this fails every member with
                        # 'could not find function "worker"'.
                        "worker",
                        "STATE_DIR"), envir = environment())
    for (ci in todo) {
      tc <- proc.time()
      # errors are SURFACED, not swallowed -- a NULL here hid three bugs
      r <- parLapply(cl, chunks[[ci]], function(si)
        tryCatch(worker(si), error = function(e)
          list(sim_index = si, ok = FALSE, reason = paste("ERROR:",
               conditionMessage(e)))))
      saveRDS(r, chunk_file(ci))
      cat(sprintf("[%s] chunk %d/%d (%d members, %.1f min)\n",
                  format(Sys.time(), "%H:%M:%S"), ci, length(chunks),
                  length(chunks[[ci]]), (proc.time()-tc)[["elapsed"]]/60))
      flush.console()
    }
    stopCluster(cl)
  }
}

res <- unlist(lapply(seq_along(chunks), function(ci) {
  f <- chunk_file(ci); if (file.exists(f)) readRDS(f) else NULL
}), recursive = FALSE)
if (!length(res)) stop("no chunk results in ", CHUNK_DIR, call. = FALSE)
ok <- vapply(res, function(r) isTRUE(r$ok), logical(1))
cat("\nassembled", length(res), "|", sum(ok), "ok\n")
if (any(!ok)) {
  cat("\n=== failures ===\n")
  print(sort(table(vapply(res[!ok], function(r)
    sub(":.*$", "", as.character(r$reason)), "")), decreasing = TRUE))
}
res <- res[ok]
if (length(res)) {
  MEM <- do.call(rbind, lapply(res, function(r) data.frame(
    sim_index = r$sim_index, recap = r$recap, tightest_tol = r$tightest_tol,
    ramp_converged = r$ramp_converged, stable = r$stable, max_cv = r$max_cv,
    drift_ok = r$drift_ok, worst_drift_lo = r$worst_drift_lo,
    worst_drift_hi = r$worst_drift_hi,
    max_erepro = r$max_erepro, n_erepro_ge1 = r$n_erepro_ge1)))
  adm <- MEM$n_erepro_ge1 == 0
  cat("\n=== members ===\n")
  cat("  built                       :", nrow(MEM), "\n")
  cat("  ramp converged              :", sum(MEM$ramp_converged), "\n")
  cat("  stable (CV screen)          :", sum(MEM$stable), "\n")
  cat("  drift ok [", DRIFT_LO, ",", DRIFT_HI, "]        :",
      sum(MEM$drift_ok), "\n")
  cat("  ADMISSIBLE (no erepro >=1)  :", sum(adm), "\n")
  cat("  USABLE (stable+drift+admis) :",
      sum(MEM$stable & MEM$drift_ok & adm), "\n")
  PS <- do.call(rbind, lapply(res, `[[`, "per_species"))
  cat("\n=== whale groups: does the draw arrive, and is the cap honoured? ===\n")
  print(as.data.frame(PS %>% filter(species %in% RECAP_SPECIES) %>%
    group_by(species) %>%
    summarise(med_drawn = signif(median(abundance_scaling), 3),
              med_realised_over_drawn =
                round(median(realised_ratio / abundance_scaling), 3),
              med_level = round(median(repro_level, na.rm = TRUE), 4),
              med_recap = round(median(recap), 4),
              med_drift = round(median(drift), 4),
              med_erepro = signif(median(erepro), 3), .groups = "drop")),
    row.names = FALSE)
  saveRDS(list(members = MEM, per_species = PS,
               biomass_1841 = do.call(rbind, lapply(res, `[[`, "biomass_1841")),
               meta = list(base = BASE_FILE, seed = SEED,
                           recap_range = c(RECAP_LO, RECAP_HI),
                           recap_species = RECAP_SPECIES,
                           recap_draw = RECAP_DRAW,
                           ramp = c(tol = RAMP_TOL, t_max = RAMP_TMAX),
                           rmax_placement = "ramp", spinup = SPINUP,
                           drift_years = DRIFT_YEARS,
                           built = format(Sys.time()))), OUT_RDS)
  write.csv(MEM, file.path(OL, paste0(STEM, "_members.csv")), row.names = FALSE)
  cat("\nwrote", OUT_RDS, "\n")
}
cat("elapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")
cat("Phase 104 complete.\n")