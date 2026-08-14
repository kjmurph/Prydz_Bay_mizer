# =============================================================================
# Phase 88 -- the FULL ensemble rebuild on the current reference model
#
# A copy of 83_whale_scaling_arms.R. Phase 83 is NOT modified. The worker is
# byte-identical to phase 83's; the protocol is unchanged and must stay so. What
# differs is the CONFIGURATION, the SCALE, and one structural safety change.
#
# ------------------------------------------------------------------- why now
# Two independent faults were found in what the published ensemble rests on, and
# neither can be patched downstream:
#
# 1. THE CATCHABILITY FIT VIOLATED THE ISIMIP3a CALIBRATION WINDOW. The protocol
#    permits catch data for tuning "on the condition that only years up to and
#    including 2004 are used". Phase 45 took its window from the EFFORT series,
#    which runs to 2010, while the FishMIP catch reconstruction ends in 2004, and
#    filled the gap with zeros. 79% of all toothfish effort sits in 2005-2010,
#    including its effort maximum in 2008 against a recorded catch of zero, so the
#    refit drove toothfish q DOWN 65x to suppress six non-existent observations.
#    Toothfishes then carried 26.4% of the pooled objective on 11.6% of the
#    observations. Corrected in phase 89.
#
# 2. THE RANKING THOSE MEMBERS WERE SELECTED BY INHERITS THAT FAULT. Dropping the
#    post-2004 zeros changes 26 of the top 167 and 52 of the top 500, so cut A --
#    and every figure built on it -- moves. A corrected ranking is only meaningful
#    against member states built on the CURRENT reference model, which is why this
#    phase rebuilds all of them rather than re-ranking the old states.
#
# The reference has also moved substantially since phase 45's states were built:
# interaction_resource (62), the interaction matrix and minke PPMR (73, 74),
# per-species reproduction targets (76) and the literature age-at-maturity
# recalibration (86). Phase 45's own header warned that the catchability it
# replaced was stale against a moved stock; that argument now applies to phase 45.
#
# ------------------------------------------------------------ what changes
# A. BASE is params_ref_p86_agemat.rds, not p59. Max erepro 0.6677, max biomass
#    deviation 0.1448, every age-at-maturity target hit within 0.5%.
#
# B. ALL 1,668 ACCEPTED MEMBERS, not a top-N cut. The phase-45 ranking is used
#    here ONLY TO ENUMERATE the accepted set, never to select within it -- every
#    ranked member is built. That matters: the ranking is the thing phase 89
#    replaces, so if it were doing selection work its fault would propagate into
#    which members exist and no later correction could recover them. Taking all
#    of them makes this phase independent of the ranking it will be re-ranked by.
#
# C. THE DRAWS ARE READ PRE-SUBSTITUTED AND IN-SCRIPT SUBSTITUTION IS REFUSED.
#    Phase 83 substituted downward draws inside the worker from a seeded matrix
#    built at run time. Phase 87 now does it ONCE, under a recorded seed, and
#    writes the result to disk. This script asserts the draws file carries that
#    stamp and that no treated species is left below 1, then runs with
#    SUBSTITUTE forced FALSE -- so the worker's substitution branches are provably
#    dead and there is no path to substituting twice.
#
#    This is the change that makes the ensemble REPRODUCIBLE. The standing caveat
#    on the existing ensemble is that it cannot be regenerated: re-running
#    reorders the ranking because several members are bistable and steady() picks
#    the basin. That is a property of the protocol and is NOT fixed here -- what
#    is fixed is the INPUT, which is now a stored file rather than a random
#    stream inside the build.
#
# D. CORES is no longer hard-capped at 14. It is capped at detectCores() - 2, so
#    the 32-core VM runs 30 while a 16-core workstation still runs 14. NEVER
#    saturate: a hard shutdown kills the in-flight run as well as the machine.
#
# E. The output stem is FIXED at "88_full" rather than composed from the switches,
#    because phase 89 defaults to reading 88_full_states. Changing the protocol
#    switches and re-running would therefore OVERWRITE rather than write beside;
#    that is deliberate for a single canonical build, and P88_STEM exists for
#    anyone who wants an arm.
#
# ------------------------------------------------- the settled order of operations
# These four are measured, not argued, and must not be reordered:
#
#   1. R_max scaling AFTER the recap (RMAX_WHEN = "postcap"). setBevertonHolt(
#      reproduction_level = L) sets R_max = RDD/L, so a recap after the scaling
#      DESTROYS it. Phase 67's null result was about a step that never took effect.
#   2. RAMP_PRESERVE = "erepro", FINAL_PRESERVE = "R_max". The ramp pins the
#      bounded parameter while the perturbed state settles; the ladder then holds
#      the scaled ceiling that step 1 just installed.
#   3. matchGrowth OFF, at every position. Tested twice. At the post-perturbation
#      position 17 of 40 members failed outright with "search_vol must not contain
#      non-finite values" and erepro reached 8.4e12. It belongs in the reference
#      recalibration only.
#   4. STEPS = 1, MAX_STEP_X = 1e9, giving K = 1 -- the single jump. Phase 61's
#      own defaults (10, 1.5) give a 25-step ramp and a different ensemble.
#
# Defaults below are set to exactly this configuration, so the env vars in
# docs/VM_RERUN_PLAN.md are belt-and-braces rather than load-bearing.
#
# ------------------------------------------------------------------ resuming
# Results are chunked to disk. Re-running with mode "run" skips completed chunks,
# so an interrupted build resumes where it stopped. "status" reports progress
# without touching anything; "collect" assembles from existing chunks only.
#
# RESUME AND STATUS MUST USE THE SAME P61_CORES AS THE ORIGINAL RUN. Chunk
# boundaries are split(members, ceiling(seq_along(members) / CORES)), so the core
# count decides how members map to res_NNN.rds files. Resuming at a different
# value matches completed chunk files to the WRONG member ranges -- some members
# never get built and others are built twice. It also makes "status" nonsense:
# at the default 10 cores it reported "1/167" against a real "1/56".
# 88_vm_run.sh passes nproc-2 to every mode, which is why it is the safe path.
#
# USAGE  Rscript R/wmin_test/88_full_ensemble.R [run|collect|status]
# ENV    P61_BASE, P61_N, P61_CORES, P83_DRAWS, P88_MEMBERS, P88_STEM,
#        and every phase-61/65/83 switch (defaults are the settled protocol)
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OUT_LARGE <- "Output_large_files/wmin_test"
BASE_FILE <- Sys.getenv("P61_BASE", "params_ref_p86_agemat.rds")
N       <- as.integer(Sys.getenv("P61_N", "1668"))
# PHASE 88: no hard cap at 14. Leave 2 cores free -- saturating every core risks
# a hard shutdown, which kills the in-flight run too.
CORE_LIMIT <- max(1L, parallel::detectCores() - 2L)
CORES   <- min(as.integer(Sys.getenv("P61_CORES", "10")), CORE_LIMIT, max(1, N))
STEPS_REQ  <- as.integer(Sys.getenv("P61_STEPS", "1"))
MAX_STEP_X <- as.numeric(Sys.getenv("P61_MAX_STEP_X", "1e9"))
SCALE_RMAX <- Sys.getenv("P65_SCALE_RMAX", "1") == "1"
# "postcap" is the ONLY placement where the scaled ceiling survives -- the recap
# sets R_max = RDD/level and overwrites anything set before it. Default here,
# unlike phase 83 where it was "ramp".
RMAX_WHEN <- Sys.getenv("P65_RMAX_WHEN", "postcap")
stopifnot(RMAX_WHEN %in% c("ramp", "post", "postcap"))
MEMBER_LIST <- Sys.getenv("P88_MEMBERS", "")
MAX_STEPS  <- as.integer(Sys.getenv("P61_MAX_STEPS", "40"))
RAMP_TOL   <- as.numeric(Sys.getenv("P61_RAMP_TOL", "0.01"))
# erepro through the ramp, R_max through the ladder. See the settled order above.
RAMP_PRESERVE  <- Sys.getenv("P61_RAMP_PRESERVE", "erepro")
FINAL_PRESERVE <- Sys.getenv("P61_FINAL_PRESERVE", "R_max")
TOLS    <- as.numeric(trimws(strsplit(
  Sys.getenv("P61_TOLS", "0.01,0.005,0.002,0.001"), ",")[[1]]))
RECAP   <- suppressWarnings(as.numeric(Sys.getenv("P61_RECAP", "0.9")))
AGE_TARGET <- Sys.getenv("P61_AGE_TARGET", "base_realised")
KEEP    <- Sys.getenv("P61_KEEP", "biomass")
USE_MATCHGROWTH <- Sys.getenv("P61_MATCHGROWTH", "0") == "1"
MG_POST <- Sys.getenv("P83_MG_POST", "0") == "1"
MG_KEEP <- Sys.getenv("P83_MG_KEEP", "biomass")
stopifnot(MG_KEEP %in% c("biomass", "egg", "number"))
USE_GAMMA_DRAW <- Sys.getenv("P61_GAMMA_DRAW", "0") == "1"
PRECAP_STEADY <- Sys.getenv("P61_PRECAP_STEADY", "1") == "1"
STEADY_TMAX  <- as.integer(Sys.getenv("P61_TMAX", "1500"))
RAMP_TMAX    <- as.integer(Sys.getenv("P61_RAMP_TMAX", "300"))
SPINUP_YEARS <- 118
stopifnot(AGE_TARGET %in% c("base_realised", "vb"),
          KEEP %in% c("biomass", "egg", "number"),
          RAMP_PRESERVE %in% c("erepro", "reproduction_level", "R_max"),
          FINAL_PRESERVE %in% c("erepro", "reproduction_level", "R_max"))

# PHASE 88: fixed stem. Phase 89 defaults to reading <stem>_states.
STEM <- Sys.getenv("P88_STEM", "88_full")
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"
stopifnot(mode %in% c("run", "collect", "status"))

STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

t0 <- proc.time()
cat("=== Phase 88: FULL ensemble rebuild, ", N, " members ===\n", sep = "")
cat("base:", BASE_FILE, "| age target:", AGE_TARGET, "| keep:", KEEP, "\n")
cat("R_max scaling:", if (SCALE_RMAX) RMAX_WHEN else "off",
    "| ramp preserve:", RAMP_PRESERVE, "| ladder preserve:", FINAL_PRESERVE, "\n")
if (!file.exists(BASE_FILE))
  stop("missing base params: ", BASE_FILE, call. = FALSE)

# --- members ------------------------------------------------------------------
# THE RANKING IS AN ENUMERATION HERE, NOT A SELECTION. Phase 89 replaces this
# ranking; taking ALL of it means nothing about which members exist depends on
# the fault phase 89 corrects. The cut-A identity is still asserted, because if
# the stored ranking no longer reproduces cut A then the file on disk is not the
# one the existing ensemble was selected with and nothing downstream lines up.
CUTS <- readRDS(file.path(OUT_LARGE, "46_selection_cuts.rds"))
cutA <- as.integer(CUTS$cuts[["A unweighted RMSE"]])
RF <- readRDS(file.path(OUT_LARGE, "45_refit_results.rds"))
RANK <- RF$per_species %>% group_by(sim_index) %>%
  summarise(m = sqrt(sum(sse) / sum(n)), .groups = "drop") %>% arrange(m)
if (!identical(sort(as.integer(head(RANK$sim_index, length(cutA)))), sort(cutA)))
  stop("the top ", length(cutA), " of the post-refit ranking is not cut A -- ",
       "refusing to proceed.", call. = FALSE)
if (N > nrow(RANK))
  stop("asked for ", N, " members but only ", nrow(RANK), " are ranked",
       call. = FALSE)
members <- as.integer(head(RANK$sim_index, N))
cat(sprintf("ranking: %d members | taking %d | RMSE %.4f .. %.4f\n",
            nrow(RANK), N, RANK$m[1], RANK$m[N]))
if (N == nrow(RANK))
  cat("  (the FULL accepted set -- the ranking is enumerating, not selecting)\n")
if (nzchar(MEMBER_LIST)) {
  want <- as.integer(trimws(strsplit(MEMBER_LIST, ",")[[1]]))
  # PHASE 88: membership is checked against the RANKED SET, not against cut A --
  # this phase is not restricted to a cut.
  if (length(setdiff(want, as.integer(RANK$sim_index))))
    stop("P88_MEMBERS contains sim_index not in the ranked set: ",
         paste(head(setdiff(want, as.integer(RANK$sim_index)), 20),
               collapse = ", "), call. = FALSE)
  members <- want
  N <- length(members)
  CORES <- min(CORES, max(1, N))
  cat("explicit member list supplied:", N, "members\n")
}

# --- the draws: PRE-SUBSTITUTED, verified, never substituted again ------------
DRAWS_FILE <- Sys.getenv("P83_DRAWS",
                         file.path(OUT_LARGE, "87_member_draws_substituted.rds"))
if (!file.exists(DRAWS_FILE)) stop("missing draws: ", DRAWS_FILE, call. = FALSE)
DR <- readRDS(DRAWS_FILE)
if (is.null(DR$substitution))
  stop("this draws file carries no substitution stamp: ", DRAWS_FILE,
       "\n  Phase 88 requires the phase-87 output. Run:",
       "\n    Rscript R/wmin_test/87_substitute_draws.R", call. = FALSE)
SUB <- DR$substitution
cat(sprintf("\ndraws: %s\n  PRE-SUBSTITUTED | seed %s | %d cells replaced\n",
            basename(DRAWS_FILE), SUB$seed, nrow(SUB$log)))
cat(sprintf("  mesozooplankton <1 -> U(%g, %g) | %s <1 -> U(%g, %g)\n",
            SUB$mesozoo_range[1], SUB$mesozoo_range[2],
            paste(SUB$whales, collapse = "/"),
            SUB$whale_range[1], SUB$whale_range[2]))
if (length(setdiff(as.character(members), names(DR$draws))))
  stop("missing recovered draws for ", length(setdiff(as.character(members),
       names(DR$draws))), " members", call. = FALSE)

# PHASE 88: in-script substitution is REFUSED. The worker's substitution branches
# are kept byte-identical to phase 83's so the protocol does not drift, but
# SUBSTITUTE is forced FALSE and the invariant is asserted here, so those
# branches cannot fire and there is no path to substituting twice.
SUBSTITUTE <- FALSE
if (Sys.getenv("P83_SUBSTITUTE", "0") == "1")
  stop("P83_SUBSTITUTE=1 but phase 88 reads PRE-SUBSTITUTED draws -- ",
       "that would substitute twice. Unset it.", call. = FALSE)
TREATED <- c("mesozooplankton", SUB$whales)
left_low <- do.call(rbind, lapply(as.character(members), function(m) {
  a <- DR$draws[[m]]$abundance_scaling
  lo <- TREATED[as.numeric(a[TREATED]) < 1]
  if (!length(lo)) return(NULL)
  data.frame(sim_index = as.integer(m), species = lo,
             value = as.numeric(a[lo]), stringsAsFactors = FALSE)
}))
if (!is.null(left_low) && nrow(left_low))
  stop("the draws file still holds ", nrow(left_low), " treated cells below 1 ",
       "(e.g. member ", left_low$sim_index[1], ", ", left_low$species[1],
       " = ", signif(left_low$value[1], 4), ") -- phase 87 did not complete",
       call. = FALSE)
cat("  verified: no treated species below 1 in any of the ", N, " members\n",
    sep = "")

BASE0 <- suppressWarnings(validParams(readRDS(BASE_FILE)))
stopifnot(identical(BASE0@rates_funcs$Encounter, "therMizerEncounter"),
          identical(BASE0@resource_dynamics, "plankton_forcing"),
          identical(BASE0@second_order_w$flux, "upwind"))
SPN <- BASE0@species_params$species
if (all(BASE0@ext_encounter == 0))
  stop("base carries no out-of-domain subsidy -- wrong reference model",
       call. = FALSE)

if (MG_POST) {
  spb <- BASE0@species_params
  if (!"age_mat" %in% names(spb) || anyNA(spb$age_mat))
    stop("P83_MG_POST=1 but the base has no complete age_mat column -- ",
         "matchGrowth would fall back to the k_vb placeholder", call. = FALSE)
  f0 <- as.numeric(mizer::age_mat(BASE0)) / spb$age_mat
  cat(sprintf("\nmatchGrowth POST enabled | keep = %s\n", MG_KEEP))
  cat(sprintf("  factor on the UNPERTURBED base spans %.4f-%.4f\n",
              min(f0), max(f0)))
  cat("  WARNING: measured to fail in the member protocol -- 17 of 40 members\n")
  cat("  errored on non-finite search_vol and erepro reached 8.4e12.\n")
}

# --- anchor the growth target ------------------------------------------------
# species_params<- SILENTLY ZEROES ext_encounter (phase 57). Save and restore by
# SLOT ASSIGNMENT -- setExtEncounter() restores the values exactly but rewrites
# the dimnames attribute structure, so all.equal then fails on attributes.
set_age_target <- function(p, target) {
  ext0 <- p@ext_encounter
  sp <- species_params(p); sp$age_mat <- as.numeric(target)
  species_params(p) <- sp
  p@ext_encounter <- ext0
  p
}
AGE_REAL <- as.numeric(mizer::age_mat(BASE0))
AGE_VB   <- as.numeric(mizer::age_mat_vB(BASE0))
BASE <- if (USE_MATCHGROWTH && AGE_TARGET == "base_realised")
  set_age_target(BASE0, AGE_REAL) else BASE0
stopifnot(isTRUE(all.equal(BASE@ext_encounter, BASE0@ext_encounter)))
GAMMA0 <- BASE@species_params$gamma
AGE_TGT <- if (AGE_TARGET == "base_realised") AGE_REAL else AGE_VB
BIOM_BASE <- as.numeric(BASE@initial_n %*% (BASE@w * BASE@dw))

if (USE_MATCHGROWTH) {
  mg0 <- suppressWarnings(matchGrowth(BASE, keep = KEEP))
  g_noop <- max(abs(mg0@species_params$gamma / GAMMA0 - 1))
  cat(sprintf("\nG1 matchGrowth on the base: max |gamma factor - 1| = %.4g\n",
              g_noop))
  if (AGE_TARGET == "base_realised" && g_noop > 1e-6)
    stop("the realised age anchor is not a no-op on the base -- refusing to proceed",
         call. = FALSE)
  stopifnot(identical(mg0@rates_funcs$Encounter, "therMizerEncounter"),
            identical(mg0@resource_dynamics, "plankton_forcing"),
            !all(mg0@ext_encounter == 0))
  cat("  WARNING: matchGrowth is ON. It drove erepro to 1.4e18 on member 71.\n")
} else {
  cat("\nmatchGrowth OFF at both positions -- the settled protocol.\n")
  stopifnot(identical(BASE@species_params$gamma, GAMMA0))
}
if (USE_GAMMA_DRAW) {
  gr <- range(sapply(members, function(si)
    as.numeric(DR$draws[[as.character(si)]]$gamma) / GAMMA0))
  cat(sprintf("GAMMA DRAW ON: drawn/base gamma spans %.4g x - %.4g x\n",
              gr[1], gr[2]))
} else {
  cat("gamma draw OFF: gamma stays at the base value for every member\n")
}

# --- substitution bookkeeping -------------------------------------------------
# These objects exist only so the worker stays byte-identical to phase 83's. With
# SUBSTITUTE = FALSE they are never read for substitution; WH_SPECIES still
# selects which species get their drawn/used values RECORDED, so it lists all
# four treated whale groups rather than phase 83's single default.
WH_SPECIES <- SUB$whales
WH_SPECIES <- WH_SPECIES[nzchar(WH_SPECIES)]
if (length(setdiff(WH_SPECIES, SPN)))
  stop("substituted whale groups not in the model: ",
       paste(setdiff(WH_SPECIES, SPN), collapse = ", "), call. = FALSE)
WH_FIX <- matrix(NA_real_, nrow = length(members), ncol = length(WH_SPECIES),
                 dimnames = list(as.character(members), WH_SPECIES))
MESO_FIX <- NA_real_

# how the substituted draws actually land, per treated group
cat("\n=== treated groups, as read from disk ===\n")
print(as.data.frame(do.call(rbind, lapply(TREATED, function(s) {
  v <- sapply(members, function(si)
    as.numeric(DR$draws[[as.character(si)]]$abundance_scaling[s]))
  n_sub <- sum(SUB$log$species == s &
               SUB$log$sim_index %in% members)
  data.frame(species = s, n_substituted = n_sub,
             pct = round(100 * n_sub / length(members), 1),
             min = signif(min(v), 4), median = signif(median(v), 4),
             max = signif(max(v), 4), stringsAsFactors = FALSE)
}))), row.names = FALSE)

# --- how many ramp steps -----------------------------------------------------
AS <- sapply(members, function(si)
  as.numeric(DR$draws[[as.character(si)]]$abundance_scaling[SPN]))
max_log <- max(abs(log(AS)))
K <- max(STEPS_REQ, ceiling(max_log / log(MAX_STEP_X)))
if (K > MAX_STEPS) {
  cat(sprintf("  NOTE: %d steps needed for a %.2gx cap; clamped to P61_MAX_STEPS = %d\n",
              K, MAX_STEP_X, MAX_STEPS)); K <- MAX_STEPS
}
STATE_DIR <- file.path(OUT_LARGE, paste0(STEM, "_states"))
CHUNK_DIR <- file.path(OUT_LARGE, paste0(STEM, "_chunks"))
OUT_RDS   <- file.path(OUT_LARGE, paste0(STEM, ".rds"))
for (d in c(STATE_DIR, CHUNK_DIR)) dir.create(d, recursive = TRUE,
                                              showWarnings = FALSE)
cat(sprintf("\nramp: %d steps | largest scaling %.4g x | worst per-step factor %.3g x\n",
            K, max(AS), exp(max_log / K)))
cat("  ramp steady: tol", RAMP_TOL, "preserve", RAMP_PRESERVE,
    "t_max", RAMP_TMAX, "| final ladder:", paste(TOLS, collapse = " -> "),
    "preserve", FINAL_PRESERVE, "t_max", STEADY_TMAX, "| recap", RECAP, "\n")
cat("  cores:", CORES, "of", parallel::detectCores(), "detected (limit",
    CORE_LIMIT, ") | stem:", STEM, "\n\n")

# ------------------------------------------------------------------ worker ---
# BYTE-IDENTICAL TO PHASE 83. Do not edit here -- the protocol is settled and any
# change makes this ensemble incomparable with the arms already run.
worker <- function(si) {
  suppressPackageStartupMessages({library(mizer); library(therMizer)})
  dw_ <- DR$draws[[as.character(si)]]
  fail <- function(r, extra = NULL)
    c(list(sim_index = si, ok = FALSE, reason = r), extra)

  check_stability <- function(s) {              # KC07:161-187, unchanged
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
  # mizer 3.1.0 signals non-convergence with message(), not warning()
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
  wdw <- BASE@w * BASE@dw
  biom <- function(p) as.numeric(p@initial_n %*% wdw)

  # --- 1. the draws ---------------------------------------------------------
  p <- BASE
  ext0 <- p@ext_encounter
  gp <- gear_params(p)
  if (!identical(paste(gp$gear, gp$species), names(dw_$catchability)))
    return(fail("gear_params rows differ between the base and the stored member"))
  gp$catchability <- as.numeric(dw_$catchability); gear_params(p) <- gp
  if (!isTRUE(all.equal(p@ext_encounter, ext0)))
    return(fail("subsidy_lost_in_catchability"))
  if (USE_GAMMA_DRAW) {
    sp <- species_params(p)
    if (!identical(sp$species, names(dw_$gamma)))
      return(fail("species order differs between the base and the draw"))
    sp$gamma <- as.numeric(dw_$gamma)
    species_params(p) <- sp
    p@ext_encounter <- ext0
    if (!isTRUE(all.equal(p@ext_encounter, ext0)))
      return(fail("subsidy_lost_in_gamma_draw"))
    if (identical(p@species_params$gamma, GAMMA0))
      return(fail("gamma draw requested but gamma did not move"))
  } else {
    if (!identical(p@species_params$gamma, GAMMA0))
      return(fail("gamma moved before the ramp"))
  }

  sc <- as.numeric(dw_$abundance_scaling[SPN])
  if (any(!is.finite(sc)) || any(sc <= 0)) return(fail("bad_abundance_scaling"))

  # PHASE 88: SUBSTITUTE is FALSE and the draws are already substituted, so both
  # branches below are dead. They are retained so this worker stays identical to
  # phase 83's; the drawn/used columns still record what was read from disk.
  mz_i <- match("mesozooplankton", SPN)
  sc_mz_drawn <- sc[mz_i]
  if (SUBSTITUTE && sc[mz_i] < 1) sc[mz_i] <- MESO_FIX

  wh_drawn <- setNames(rep(NA_real_, length(SPN)), SPN)
  wh_used  <- wh_drawn
  for (s in WH_SPECIES) {
    k <- match(s, SPN)
    wh_drawn[k] <- sc[k]
    if (SUBSTITUTE && sc[k] < 1) sc[k] <- WH_FIX[as.character(si), s]
    wh_used[k] <- sc[k]
  }

  # --- 2. the ramp ---------------------------------------------------------
  tr <- list(); prev_t <- 0
  for (k in seq_len(K)) {
    tk <- k / K
    inc <- sc^tk / sc^prev_t                 # incremental factor for this step
    prev_t <- tk
    for (j in seq_along(inc)) p@initial_n[j, ] <- p@initial_n[j, ] * inc[j]

    if (SCALE_RMAX && RMAX_WHEN == "ramp") {
      spm <- species_params(p)
      spm$R_max <- spm$R_max * inc
      species_params(p) <- spm
      p@ext_encounter <- ext0
      if (!isTRUE(all.equal(p@ext_encounter, ext0)))
        return(fail(sprintf("subsidy_lost_in_rmax_scale_step%d", k),
                    list(trace = do.call(rbind, tr))))
    }

    if (USE_MATCHGROWTH) {
      mg <- try(suppressWarnings(matchGrowth(p, keep = KEEP)), silent = TRUE)
      if (inherits(mg, "try-error"))
        return(fail(sprintf("matchGrowth_error_step%d: %s", k,
                            trimws(gsub("\\s+", " ", as.character(mg)))),
                    list(trace = do.call(rbind, tr))))
      p <- mg
      if (all(p@ext_encounter == 0))
        return(fail(sprintf("subsidy_lost_in_matchGrowth_step%d", k),
                    list(trace = do.call(rbind, tr))))
    }

    if (!PRECAP_STEADY) next
    st <- steady_guarded(p, RAMP_TOL, RAMP_PRESERVE, RAMP_TMAX)
    if (st$errored)
      return(fail(sprintf("steady_error_step%d", k),
                  list(trace = do.call(rbind, tr))))
    p <- st$params
    er <- p@species_params$erepro; rl <- rlev(p)
    am <- try(as.numeric(mizer::age_mat(p)), silent = TRUE)
    if (inherits(am, "try-error")) am <- rep(NA_real_, length(SPN))
    tr[[length(tr) + 1]] <- data.frame(
      sim_index = si, phase = "ramp", step = k, t = tk, tol = RAMP_TOL,
      converged = st$converged,
      gamma_factor_max = max(p@species_params$gamma / GAMMA0),
      gamma_factor_min = min(p@species_params$gamma / GAMMA0),
      age_dev_max = suppressWarnings(max(abs(am / AGE_TGT - 1), na.rm = TRUE)),
      max_erepro = max(er), n_erepro_ge1 = sum(er >= 1),
      max_repro_level = suppressWarnings(max(rl, na.rm = TRUE)),
      total_biomass = sum(biom(p)), stringsAsFactors = FALSE)
  }
  # The ramp is a PATH, not a constraint: at each step steady() moves the
  # abundances away from the multiplier just applied, so the destination is NOT
  # base x abundance_scaling. Record the gap rather than assume it away.
  b_dest <- biom(p)
  realised_ratio <- b_dest / BIOM_BASE

  if (SCALE_RMAX && RMAX_WHEN == "post") {
    spm <- species_params(p)
    spm$R_max <- spm$R_max * sc
    species_params(p) <- spm
    p@ext_encounter <- ext0
    if (!isTRUE(all.equal(p@ext_encounter, ext0)))
      return(fail("subsidy_lost_in_rmax_post_scale",
                  list(trace = do.call(rbind, tr))))
  }

  if (MG_POST) {
    mgp <- try(suppressWarnings(matchGrowth(p, keep = MG_KEEP)), silent = TRUE)
    if (inherits(mgp, "try-error"))
      return(fail(paste("matchGrowth_post:",
                        trimws(gsub("\\s+", " ", as.character(mgp)))),
                  list(trace = do.call(rbind, tr))))
    p <- mgp
    if (all(p@ext_encounter == 0))
      return(fail("subsidy_lost_in_matchGrowth_post",
                  list(trace = do.call(rbind, tr))))
  }

  # --- 3. re-cap, then the tolerance ladder at the destination -------------
  # A cap set on the reference model is NOT inherited by its members (phase 60):
  # any per-member reproduction treatment must be applied AFTER the draws.
  rl_in <- rlev(p)
  if (is.finite(RECAP)) {
    p2 <- try(suppressWarnings(
      setBevertonHolt(p, reproduction_level = pmin(rl_in, RECAP))), silent = TRUE)
    if (inherits(p2, "try-error"))
      return(fail("recap_error", list(trace = do.call(rbind, tr))))
    p <- p2
  }
  rl_capped <- rlev(p)

  # --- 3b. "postcap": scale R_max AFTER the recap ----------------------------
  # THE ORDER THAT ACTUALLY TESTS THE IDEA. setBevertonHolt(reproduction_level =
  # L) sets R_max = RDD / L, so the recap OVERWRITES any R_max set before it --
  # which is why RMAX_WHEN = "post" produced whale reproduction levels pinned at
  # exactly 0.90 and no change in whale recruitment. Scaling here, with
  # preserve = "R_max" in the ladder that follows, is the only placement where
  # the scaled ceiling survives.
  if (SCALE_RMAX && RMAX_WHEN == "postcap") {
    spm <- species_params(p)
    spm$R_max <- spm$R_max * sc
    species_params(p) <- spm
    p@ext_encounter <- ext0
    if (!isTRUE(all.equal(p@ext_encounter, ext0)))
      return(fail("subsidy_lost_in_rmax_postcap_scale",
                  list(trace = do.call(rbind, tr))))
  }

  keep_p <- NULL; keep_tol <- NA_real_
  for (tol in TOLS) {
    st <- steady_guarded(p, tol, FINAL_PRESERVE)
    if (st$errored) {
      tr[[length(tr) + 1]] <- data.frame(sim_index = si, phase = "ladder",
        step = NA_integer_, t = 1, tol = tol, converged = NA,
        gamma_factor_max = NA_real_, gamma_factor_min = NA_real_,
        age_dev_max = NA_real_, max_erepro = NA_real_,
        n_erepro_ge1 = NA_integer_, max_repro_level = NA_real_,
        total_biomass = NA_real_, stringsAsFactors = FALSE)
      break
    }
    p <- st$params
    er <- p@species_params$erepro; rl <- rlev(p)
    am <- try(as.numeric(mizer::age_mat(p)), silent = TRUE)
    if (inherits(am, "try-error")) am <- rep(NA_real_, length(SPN))
    tr[[length(tr) + 1]] <- data.frame(
      sim_index = si, phase = "ladder", step = NA_integer_, t = 1, tol = tol,
      converged = st$converged,
      gamma_factor_max = max(p@species_params$gamma / GAMMA0),
      gamma_factor_min = min(p@species_params$gamma / GAMMA0),
      age_dev_max = suppressWarnings(max(abs(am / AGE_TGT - 1), na.rm = TRUE)),
      max_erepro = max(er), n_erepro_ge1 = sum(er >= 1),
      max_repro_level = suppressWarnings(max(rl, na.rm = TRUE)),
      total_biomass = sum(biom(p)), stringsAsFactors = FALSE)
    if (st$converged) { keep_p <- p; keep_tol <- tol }
  }
  TR <- do.call(rbind, tr)
  if (is.null(keep_p)) return(fail("no_rung_converged", list(trace = TR)))
  ps <- keep_p

  # --- 4. spin-up ----------------------------------------------------------
  s0 <- try(project(ps, t_start = 1841, t_max = SPINUP_YEARS, effort = 0,
                    progress_bar = FALSE), silent = TRUE)
  if (inherits(s0, "try-error")) return(fail("spinup_error", list(trace = TR)))
  init <- s0@n[SPINUP_YEARS, , ]
  stab <- check_stability(s0)

  saveRDS(list(sim_index = si, params = ps, initial_n = init,
               tightest_tol = keep_tol, trace = TR),
          file.path(STATE_DIR, sprintf("state_%05d.rds", si)))

  spq <- species_params(ps)
  am <- try(as.numeric(mizer::age_mat(ps)), silent = TRUE)
  if (inherits(am, "try-error")) am <- rep(NA_real_, length(SPN))
  list(sim_index = si, ok = TRUE, reason = "ok", trace = TR,
       tightest_tol = keep_tol,
       ratio_vs_drawn_med = median(realised_ratio / sc),
       ratio_vs_drawn_max = max(realised_ratio / sc),
       ratio_vs_drawn_min = min(realised_ratio / sc),
       stable = isTRUE(stab$stable), max_cv = stab$max_cv,
       max_erepro = max(spq$erepro), n_erepro_ge1 = sum(spq$erepro >= 1),
       per_species = data.frame(sim_index = si, species = SPN,
         erepro = spq$erepro, R_max = spq$R_max, repro_level = rlev(ps),
         repro_level_preladder = rl_in, repro_level_capped = rl_capped,
        mesozoo_drawn = sc_mz_drawn, mesozoo_used = sc[mz_i],
        whale_drawn = wh_drawn, whale_used = wh_used,
         gamma = spq$gamma, gamma_factor = spq$gamma / GAMMA0,
         abundance_scaling = sc, realised_ratio = realised_ratio,
         age_mat = am, age_target = AGE_TGT,
         stringsAsFactors = FALSE),
       biomass_1841 = data.frame(sim_index = si, species = SPN,
         biomass = as.numeric(init %*% (ps@w * ps@dw)),
         stringsAsFactors = FALSE))
}

# --------------------------------------------------------------------- run ---
chunk_file <- function(ci) file.path(CHUNK_DIR, sprintf("res_%03d.rds", ci))
chunks <- split(members, ceiling(seq_along(members) / CORES))
if (mode == "status") {
  done <- sum(vapply(seq_along(chunks), function(ci)
    file.exists(chunk_file(ci)), logical(1)))
  cat(sprintf("chunks %d/%d complete (%.1f%%) | %d member states written\n",
              done, length(chunks), 100 * done / length(chunks),
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
    clusterExport(cl, c("BASE", "DR", "SPN", "GAMMA0", "AGE_TGT", "K", "KEEP",
                        "RAMP_TOL", "RAMP_PRESERVE", "FINAL_PRESERVE", "TOLS",
                        "RECAP", "SCALE_RMAX", "RMAX_WHEN", "MESO_FIX",
                        "WH_SPECIES", "WH_FIX", "MG_POST", "MG_KEEP",
                        "SUBSTITUTE",
                        "STEADY_TMAX", "RAMP_TMAX", "SPINUP_YEARS",
                        "STAB", "STATE_DIR", "BIOM_BASE", "USE_MATCHGROWTH",
                        "USE_GAMMA_DRAW", "PRECAP_STEADY"),
                  envir = environment())
    for (ci in todo) {
      tc <- proc.time()
      r <- parLapply(cl, chunks[[ci]], worker)
      saveRDS(r, chunk_file(ci))
      el <- (proc.time() - tc)[["elapsed"]] / 60
      dn <- which(todo == ci)
      cat(sprintf("[%s] chunk %d/%d done (%d members, %.1f min) | %d/%d left, ETA %.1f h\n",
                  format(Sys.time(), "%H:%M:%S"), ci, length(chunks),
                  length(chunks[[ci]]), el, length(todo) - dn, length(todo),
                  el * (length(todo) - dn) / 60))
      flush.console()
    }
    stopCluster(cl)
  }
}

res <- unlist(lapply(seq_along(chunks), function(ci) {
  f <- chunk_file(ci); if (file.exists(f)) readRDS(f) else NULL
}), recursive = FALSE)
if (!length(res)) stop("no chunk results in ", CHUNK_DIR)
ok <- vapply(res, function(r) isTRUE(r$ok), logical(1))
cat("\nassembled", length(res), "members |", sum(ok), "ok\n")
if (any(!ok)) {
  cat("\n=== failures by reason ===\n")
  rr <- vapply(res[!ok], function(r)
    sub(":.*$", "", as.character(r$reason)), "")
  print(sort(table(rr), decreasing = TRUE))
}

TRACE <- do.call(rbind, lapply(res, function(r) r$trace))
if (!is.null(TRACE) && nrow(TRACE)) {
  cat("\n=== the ramp: how the model walks to the drawn abundances ===\n")
  print(as.data.frame(TRACE %>% filter(phase == "ramp") %>% group_by(step) %>%
    summarise(members = n(), converged = sum(converged, na.rm = TRUE),
              gamma_min = signif(min(gamma_factor_min, na.rm = TRUE), 3),
              gamma_max = signif(max(gamma_factor_max, na.rm = TRUE), 3),
              age_dev_max = signif(max(age_dev_max, na.rm = TRUE), 3),
              max_erepro = signif(max(max_erepro, na.rm = TRUE), 4),
              n_bad = sum(n_erepro_ge1 > 0, na.rm = TRUE),
              max_rl = signif(max(max_repro_level, na.rm = TRUE), 5),
              .groups = "drop")), row.names = FALSE)
  cat("\n=== the final ladder ===\n")
  print(as.data.frame(TRACE %>% filter(phase == "ladder") %>% group_by(tol) %>%
    summarise(members = n(), converged = sum(converged, na.rm = TRUE),
              with_erepro_ge1 = sum(n_erepro_ge1 > 0, na.rm = TRUE),
              max_erepro = signif(max(max_erepro, na.rm = TRUE), 4),
              max_repro_level = signif(max(max_repro_level, na.rm = TRUE), 5),
              .groups = "drop") %>% arrange(desc(tol))), row.names = FALSE)
}

res <- res[ok]
if (length(res)) {
  MEM <- do.call(rbind, lapply(res, function(r) data.frame(
    sim_index = r$sim_index, tightest_tol = r$tightest_tol,
    stable = r$stable, max_cv = r$max_cv,
    max_erepro = r$max_erepro, n_erepro_ge1 = r$n_erepro_ge1,
    ratio_vs_drawn_med = r$ratio_vs_drawn_med,
    ratio_vs_drawn_min = r$ratio_vs_drawn_min,
    ratio_vs_drawn_max = r$ratio_vs_drawn_max)))
  # 1,668 rows is too many to print. The counts are what matter here; the full
  # table goes to <stem>_members.csv.
  cat("\n=== members ===\n")
  cat("  built                     :", nrow(MEM), "\n")
  cat("  reached tol", min(TOLS), "          :",
      sum(MEM$tightest_tol <= min(TOLS)), "\n")
  cat("  stable                    :", sum(MEM$stable), "\n")
  cat("  ADMISSIBLE (no erepro >=1):", sum(MEM$n_erepro_ge1 == 0), "\n")
  cat("  USABLE (stable AND admissible):",
      sum(MEM$stable & MEM$n_erepro_ge1 == 0), "\n")
  cat("  realised / drawn abundance ratio: median over members",
      signif(median(MEM$ratio_vs_drawn_med), 3), "| range",
      signif(min(MEM$ratio_vs_drawn_min), 3), "-",
      signif(max(MEM$ratio_vs_drawn_max), 3), "\n")

  PS <- do.call(rbind, lapply(res, `[[`, "per_species"))
  cat("\n=== per species across members ===\n")
  print(as.data.frame(PS %>% group_by(species) %>%
    summarise(med_gamma_factor = signif(median(gamma_factor), 3),
              min_gamma_factor = signif(min(gamma_factor), 3),
              max_gamma_factor = signif(max(gamma_factor), 3),
              med_rl = round(median(repro_level, na.rm = TRUE), 4),
              max_rl = round(max(repro_level, na.rm = TRUE), 5),
              med_erepro = signif(median(erepro), 3),
              max_erepro = signif(max(erepro), 3),
              n_ge1 = sum(erepro >= 1), .groups = "drop") %>%
    slice(match(SPN, species))), row.names = FALSE)
  base_rl <- as.numeric(getReproductionLevel(BASE))
  cat("\n  reference model: median level", round(median(base_rl), 4),
      "| max", round(max(base_rl), 4),
      "| max erepro", round(max(BASE@species_params$erepro), 4), "\n")

  saveRDS(list(members = MEM, per_species = PS, trace = TRACE,
               biomass_1841 = do.call(rbind, lapply(res, `[[`, "biomass_1841")),
               meta = list(base = BASE_FILE, age_target = AGE_TARGET,
                           age_tgt = setNames(AGE_TGT, SPN), keep = KEEP,
                           K = K, ramp_tol = RAMP_TOL,
                           ramp_preserve = RAMP_PRESERVE,
                           final_preserve = FINAL_PRESERVE, tols = TOLS,
                           recap = RECAP, members = members,
                           spinup_years = SPINUP_YEARS,
                           rmax_scaled = SCALE_RMAX, rmax_when = RMAX_WHEN,
                           draws_file = DRAWS_FILE, substitution = SUB,
                           gamma_base = setNames(GAMMA0, SPN),
                           built = format(Sys.time()))), OUT_RDS)
  write.csv(MEM, file.path(OUT_LARGE, paste0(STEM, "_members.csv")),
            row.names = FALSE)
  write.csv(PS, file.path(OUT_LARGE, paste0(STEM, "_per_species.csv")),
            row.names = FALSE)
  write.csv(TRACE, file.path(OUT_LARGE, paste0(STEM, "_trace.csv")),
            row.names = FALSE)
  cat("\nwrote", OUT_RDS, "and", length(list.files(STATE_DIR)),
      "member states\n")
  cat("NEXT: Rscript R/wmin_test/89_catchability_refit_2004.R\n")
}
cat("elapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")
cat("Phase 88 complete.\n")