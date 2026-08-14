# =============================================================================
# Phase 65 -- SCALE R_max BY THE ABUNDANCE DRAW
#
# A copy of 61_members_ramped_matchgrowth.R. Phase 61 is NOT modified. The header
# below is inherited and still describes the shared protocol; THIS BLOCK states
# what differs.
#
# ------------------------------------------------------------- the diagnosis
# apply_draws() scales initial_n by the per-species multiplier but leaves R_max
# at the reference value. RDI then scales with the perturbed spawning stock while
# the recruitment ceiling does not, so reproduction_level = RDI/(RDI + R_max) is
# driven toward 1. That is why members arrive PRE-CAP at sperm 0.983, baleen
# 0.948, minke 0.924 -- the 0.9 cap is a patch for a symptom this creates.
#
# It also erases the ensemble's abundance uncertainty. Slope of log(realised
# biomass ratio) on log(drawn abundance_scaling) over the 122 stable phase-61
# members, where 1.0 is full transmission:
#
#   toothfishes -0.15  salps 0.02  shelf 0.03  squids 0.03  mesozoo 0.05
#   bathypelagic 0.05  mesopelagic 0.05  birds 0.07  small divers 0.13
#   other krill 0.13   other macrozoo 0.19  krill 0.23  leopard seals 0.40
#   large divers 0.47  medium divers 0.57  minke 0.60  orca 0.69
#   baleen 0.87        sperm 0.90
#
# For 12 of 19 groups UNDER 20% of the drawn variation survives into the member.
#
# ------------------------------------------------------------- what changes
# 1. R_max IS SCALED BY THE SAME FACTOR as initial_n, inside the ramp loop.
# 2. preserve = "R_max" for BOTH the ramp and the final ladder. "erepro" lets the
#    scaled R_max float straight back; "reproduction_level" pins the ratio that
#    must be free to move.
# 3. P61_STEPS / P61_MAX_STEP_X default to 1 / 1e9, giving K = 1 -- the single
#    jump the stored ensemble was actually built with. Phase 61's own defaults
#    (10, 1.5) give K = 25 and would NOT reproduce it.
# 4. The recap is left ON by default but P61_RECAP=none disables it, which is the
#    point of interest: with R_max scaled the level should land near 0.90 on its
#    own and the cap should be close to a no-op.
#
# -------------------------------------------------------------- pilot evidence
# On params_ref_p59_cap09_tol001.rds, WHALES ONLY, initial_n and R_max both x10,
# steady(preserve = "R_max"): baleen 10.04x, minke 10.15x, sperm 10.03x against a
# 10x target, erepro 0.0023 / 0.085 / 0.035, reproduction level 0.901-0.905
# UNCAPPED, antarctic krill -19% and salps -12% as a real trophic response.
#
# THE KNOWN RISK: scaling the WHOLE web x10 the same way drives erepro to 2.73
# (mesozooplankton) and 1.56 (leopard seals) -- the resource is PRESCRIBED by
# plankton_forcing and never scales, so a uniformly inflated system cannot feed
# itself. Real draws span 0.5x-24800x across all 19 groups, so admissibility is
# exactly what this run measures.
#
# ISOLATED TRIAL. Writes only new files.
#
# ------------------------------------------------------------- the protocol
# base   params_ref_p59_cap09_tol001.rds
#
#   0. anchor the growth target (see AGE TARGET below)
#   1. apply the catchability draw ONLY -- gamma is NOT drawn any more
#   2. RAMP, K steps, all species advancing together in log space:
#        a. multiply the current abundances by the incremental factor
#           s^(t_k) / s^(t_(k-1)),  t_k = k/K
#        b. matchGrowth(keep = KEEP)  -- OFF BY DEFAULT, see below
#        c. steady(tol = RAMP_TOL, preserve = RAMP_PRESERVE)
#   3. at the destination: re-cap the reproduction level at 0.9, then the
#      phase-60 tolerance ladder under FINAL_PRESERVE
#   4. 118 yr unfished spin-up, stability screen (the 44/53 screen, unchanged)
#
# WHAT CHANGES FROM PHASE 60, AND WHY
#
# A. THE GAMMA DRAW IS GONE. Members now span ABUNDANCE AND CATCHABILITY
#    UNCERTAINTY ONLY; physiology is internally consistent rather than sampled.
#    This is a deliberate change to what the ensemble represents and must be
#    stated in the methods. Drawn gamma ran 0.42x-1708x base, and it is what
#    pushed 110 of 190 member-species pairs above the 0.9 reproduction cap
#    before steady() was ever called -- the direct cause of phase 60's
#    erepro 8111.
#
# B. GAMMA IS NOW AN OUTPUT, NOT AN INPUT. matchGrowth scales the search volume,
#    maximum consumption, metabolic rate AND external encounter rate by one
#    common factor so individuals reach w_mat at the target age, holding the
#    feeding level and critical feeding level fixed. Because it scales
#    ext_encounter too, the phase-57 out-of-domain subsidy rides along with the
#    physiology instead of being stranded -- verified, the ext_encounter ratio
#    equals the gamma ratio to 0.
#
# C. THE ABUNDANCE STEP IS RAMPED. Drawn abundance_scaling spans 0.5x-24800x.
#    Applying that in one jump is what makes steady() search from an absurd
#    starting point. Each step multiplies the CURRENT equilibrated state by the
#    incremental factor, so the model walks to the destination.
#
# ------------------------------------------------------- AGE TARGET (P61_AGE_TARGET)
# matchGrowth needs a target age at maturity. With no age_mat column it falls
# back to age_mat_vB(), which is computed from k_vb -- and k_vb HERE IS A
# PLACEHOLDER: 0.2 for all eight air-breathing groups, 0.5 for all six LTL
# groups, i.e. 14 of 19 species on two round default values. Targeting it moves
# gamma on the UNTOUCHED, already-calibrated base by up to 3.15x (leopard
# seals), 2.03x (sperm whales), 2.93x (salps) -- it would decalibrate the
# reference model before a single draw was applied.
#
#   "base_realised" (DEFAULT) -- age_mat set to the base model's OWN realised
#       age at maturity, age_mat(BASE). matchGrowth is then EXACTLY a no-op on
#       the base (gamma factor 1.000000, measured), and on a member it restores
#       the calibrated growth trajectory after the abundances move. This asks
#       "hold growth where the calibrated model puts it", which is a statement
#       about the reference model rather than about a placeholder k_vb.
#   "vb" -- the mizer default. Available for comparison; expect it to move the
#       base as above.
#
# ---------------------------------------- WHY matchGrowth IS OFF BY DEFAULT
# The proposal was that scaling abundances up STUNTS growth, forcing
# astronomical egg production, and that matchGrowth would repair it. BOTH HALVES
# ARE WRONG ON THIS MODEL, measured, not argued:
#
# 1. THE SIGN IS BACKWARDS. On member 173's full draw, age at maturity FALLS for
#    all 19 species (small divers 19.2 -> 4.3 yr, toothfishes 15.9 -> 6.5 yr):
#    the draw raises prey abundance far more than it raises predation mortality,
#    so growth ACCELERATES. matchGrowth therefore applies factors BELOW 1
#    (0.22x-1.00x) -- it STARVES species back down to the calibrated age.
#
# 2. THAT STARVATION IS WHAT DESTROYS erepro. Cutting gamma cuts the energy
#    available for reproduction, so erepro must rise to sustain the same
#    recruitment -- and it compounds, because every step cuts gamma again.
#    Attribution, 4-step ramp on member 71, max erepro over species:
#
#      with matchGrowth      0.7198 -> 3650 -> 3.99e8 -> 1.44e18
#      SAME ramp, no matchGrowth   0.7198 at every step (preserve = "erepro"
#                                  genuinely holds it; max level 0.958 -> 0.997)
#
#    On the base, where the gamma factor is exactly 1, matchGrowth leaves erepro
#    untouched (ratio 1.000) and moves only R_max (24%). So the explosion is
#    caused by the gamma cut, not by the function being unsound.
#
# The deeper point: growth SHOULD be plastic. Forcing members back to the
# reference age at maturity asserts that these animals mature at a fixed age
# however much food there is, which is not a property the model should have.
# matchGrowth is a tool for calibrating to observed growth data, not for holding
# a model coherent under an abundance perturbation.
#
# So the working protocol is the ramp WITHOUT matchGrowth: gamma stays at the
# base value, erepro is pinned admissible by preserve = "erepro", and the
# reproduction level -- which climbs to ~0.997 as abundances rise -- is dealt
# with at the destination by the phase-58 cap, which LOWERS erepro further
# (level 0.997 -> 0.9 scales erepro by roughly (1-0.997)/(1-0.9) = 0.03).
# P61_MATCHGROWTH=1 reproduces the failure.
#
# USAGE  Rscript R/wmin_test/61_members_ramped_matchgrowth.R [run|collect|status]
# ENV    P61_BASE, P61_N (10), P61_CORES (10), P61_STEPS (10),
#        P61_MAX_STEP_X (1.5), P61_MAX_STEPS (40), P61_RAMP_TOL (0.01),
#        P61_RAMP_PRESERVE (erepro), P61_FINAL_PRESERVE (reproduction_level),
#        P61_TOLS, P61_RECAP (0.9), P61_AGE_TARGET (base_realised),
#        P61_KEEP (biomass)
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OUT_LARGE <- "Output_large_files/wmin_test"
BASE_FILE <- Sys.getenv("P61_BASE", "params_ref_p59_cap09_tol001.rds")
N       <- as.integer(Sys.getenv("P61_N", "10"))
CORES   <- min(as.integer(Sys.getenv("P61_CORES", "10")), 14, max(1, N))
STEPS_REQ  <- as.integer(Sys.getenv("P61_STEPS", "1"))
MAX_STEP_X <- as.numeric(Sys.getenv("P61_MAX_STEP_X", "1e9"))
# PHASE 65: scale R_max by the abundance multiplier. The whole point of the arm.
SCALE_RMAX <- Sys.getenv("P65_SCALE_RMAX", "1") == "1"
# WHEN to scale R_max.
#   "ramp" -- with initial_n, inside the ramp (the first phase-65 arm: 0/10
#             admissible, erepro to 2850)
#   "post" -- AFTER the ramp's steady(preserve = "erepro") has already settled
#             the perturbed state, then steady(preserve = "R_max"). Lets the
#             bounded parameter (erepro) be pinned while the state settles, and
#             only then hands the ceiling its multiplier.
#   "postcap" -- AFTER the recap as well, immediately before the ladder. The
#             recap sets R_max = RDD / level and so DESTROYS any R_max set
#             before it; this is the only placement where the scaled ceiling
#             survives into the ladder.
RMAX_WHEN <- Sys.getenv("P65_RMAX_WHEN", "ramp")
stopifnot(RMAX_WHEN %in% c("ramp", "post", "postcap"))
# Optional explicit member list (comma-separated sim_index), so a subset such as
# "the 32 usable members" can be re-run without redoing all 167.
MEMBER_LIST <- Sys.getenv("P65_MEMBERS", "")
MAX_STEPS  <- as.integer(Sys.getenv("P61_MAX_STEPS", "40"))
RAMP_TOL   <- as.numeric(Sys.getenv("P61_RAMP_TOL", "0.01"))
# PHASE 65: preserve R_max, not erepro / reproduction_level. "erepro" lets the
# scaled R_max float straight back; "reproduction_level" pins the ratio that has
# to be free to move.
RAMP_PRESERVE  <- Sys.getenv("P61_RAMP_PRESERVE", "R_max")
FINAL_PRESERVE <- Sys.getenv("P61_FINAL_PRESERVE", "R_max")
TOLS    <- as.numeric(trimws(strsplit(
  Sys.getenv("P61_TOLS", "0.01,0.005,0.002,0.001"), ",")[[1]]))
RECAP   <- suppressWarnings(as.numeric(Sys.getenv("P61_RECAP", "0.9")))
AGE_TARGET <- Sys.getenv("P61_AGE_TARGET", "base_realised")
KEEP    <- Sys.getenv("P61_KEEP", "biomass")
# DEFAULT OFF. matchGrowth is the thing that breaks this protocol -- measured,
# see the header. Set P61_MATCHGROWTH=1 to reproduce the failure.
USE_MATCHGROWTH <- Sys.getenv("P61_MATCHGROWTH", "0") == "1"
# DEFAULT OFF. Setting this to 1 restores the drawn gamma alongside the drawn
# abundance and catchability. It is the ONE variable separating this phase from
# phase 60, and running both arms at K=1 is the only clean test of it: phase 60
# is NOT a valid control, because it also lacks the pre-cap steady(preserve =
# "erepro") and starts its ladder at 0.1 rather than 0.01.
USE_GAMMA_DRAW <- Sys.getenv("P61_GAMMA_DRAW", "0") == "1"
# The steady(preserve = "erepro") inside each ramp step. Setting this to 0 goes
# straight from the draws to the re-cap and ladder, which is phase 60's
# structure. With K=1 and the gamma draw on, arm-with vs arm-without is the
# clean single-variable test of whether THIS step is what rescues erepro.
PRECAP_STEADY <- Sys.getenv("P61_PRECAP_STEADY", "1") == "1"
STEADY_TMAX  <- as.integer(Sys.getenv("P61_TMAX", "1500"))
# Intermediate ramp steps do not need to converge -- the next step perturbs the
# state again anyway, and steady() burns the FULL t_max whenever it fails to
# converge, which is exactly the expensive case. Bounding the ramp separately
# caps the cost of the steps that are far from equilibrium; the destination
# ladder still runs at the full STEADY_TMAX.
RAMP_TMAX    <- as.integer(Sys.getenv("P61_RAMP_TMAX", "300"))
SPINUP_YEARS <- 118
stopifnot(AGE_TARGET %in% c("base_realised", "vb"),
          KEEP %in% c("biomass", "egg", "number"),
          RAMP_PRESERVE %in% c("erepro", "reproduction_level", "R_max"),
          FINAL_PRESERVE %in% c("erepro", "reproduction_level", "R_max"))

STEM <- sprintf("65_%s_n%d_%s%s_recap%s_K%s_%s",
                if (SCALE_RMAX) paste0("rmax", RMAX_WHEN) else "rmaxfixed", N,
                if (USE_MATCHGROWTH) paste0("mg_", AGE_TARGET) else "nomg",
                paste0(if (USE_GAMMA_DRAW) "_gdraw" else "",
                       if (!PRECAP_STEADY) "_noprecap" else ""),
                if (is.finite(RECAP)) gsub("\\.", "", format(RECAP)) else "off",
                "AUTO", FINAL_PRESERVE)
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"
stopifnot(mode %in% c("run", "collect", "status"))

STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

t0 <- proc.time()
cat("=== Phase 61: ramped matchGrowth, top-", N, " members ===\n", sep = "")
cat("base:", BASE_FILE, "| age target:", AGE_TARGET, "| keep:", KEEP, "\n")
if (!file.exists(BASE_FILE))
  stop("missing base params: ", BASE_FILE, call. = FALSE)

# --- members, in post-refit rank order (KC07:96-104, phase 60) ---------------
CUTS <- readRDS(file.path(OUT_LARGE, "46_selection_cuts.rds"))
cutA <- as.integer(CUTS$cuts[["A unweighted RMSE"]])
RF <- readRDS(file.path(OUT_LARGE, "45_refit_results.rds"))
chk <- RF$per_species %>% group_by(sim_index) %>%
  summarise(m = sqrt(sum(sse) / sum(n)), .groups = "drop") %>%
  arrange(m) %>% head(length(cutA)) %>% pull(sim_index)
if (!identical(as.integer(chk), cutA))
  stop("cut A does not match the post-refit ranking -- refusing to proceed.")
members <- head(cutA, N)
if (nzchar(MEMBER_LIST)) {
  want <- as.integer(trimws(strsplit(MEMBER_LIST, ",")[[1]]))
  if (length(setdiff(want, cutA)))
    stop("P65_MEMBERS contains sim_index not in cut A: ",
         paste(setdiff(want, cutA), collapse = ", "), call. = FALSE)
  members <- want          # keep the caller's order; these ARE cut-A members
  N <- length(members)
  cat("explicit member list supplied:", N, "members\n")
}
DR <- readRDS(file.path(OUT_LARGE, "43_member_draws.rds"))
if (length(setdiff(as.character(members), names(DR$draws))))
  stop("missing recovered draws")
cat("members:", paste(members, collapse = ", "), "\n")

BASE0 <- suppressWarnings(validParams(readRDS(BASE_FILE)))
stopifnot(identical(BASE0@rates_funcs$Encounter, "therMizerEncounter"),
          identical(BASE0@resource_dynamics, "plankton_forcing"),
          identical(BASE0@second_order_w$flux, "upwind"))
SPN <- BASE0@species_params$species
if (all(BASE0@ext_encounter == 0))
  stop("base carries no out-of-domain subsidy -- wrong reference model",
       call. = FALSE)

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
# Only touch the base when matchGrowth will actually use the target. With
# matchGrowth off, the base must stay byte-identical to the reference model.
BASE <- if (USE_MATCHGROWTH && AGE_TARGET == "base_realised")
  set_age_target(BASE0, AGE_REAL) else BASE0
stopifnot(isTRUE(all.equal(BASE@ext_encounter, BASE0@ext_encounter)))
GAMMA0 <- BASE@species_params$gamma
AGE_TGT <- if (AGE_TARGET == "base_realised") AGE_REAL else AGE_VB
BIOM_BASE <- as.numeric(BASE@initial_n %*% (BASE@w * BASE@dw))

# G1 -- with the realised anchor, matchGrowth must be a no-op on the base. This
# is the single check that validates the whole growth-retuning path: if the
# target does not describe the calibrated model, every member is retuned toward
# something the reference itself does not satisfy.
if (USE_MATCHGROWTH) {
  mg0 <- suppressWarnings(matchGrowth(BASE, keep = KEEP))
  g_noop <- max(abs(mg0@species_params$gamma / GAMMA0 - 1))
  cat(sprintf("\nG1 matchGrowth on the base: max |gamma factor - 1| = %.4g\n",
              g_noop))
  if (AGE_TARGET == "base_realised" && g_noop > 1e-6)
    stop("the realised age anchor is not a no-op on the base -- refusing to proceed",
         call. = FALSE)
  if (AGE_TARGET == "vb")
    cat("  (expected to be large: k_vb is a placeholder for 14 of 19 groups)\n")
  stopifnot(identical(mg0@rates_funcs$Encounter, "therMizerEncounter"),
            identical(mg0@resource_dynamics, "plankton_forcing"),
            !all(mg0@ext_encounter == 0))
  cat("  matchGrowth preserves therMizerEncounter, plankton_forcing and the subsidy\n")
  cat("  WARNING: matchGrowth is ON. It drove erepro to 1.4e18 on member 71.\n")
} else {
  cat("\nmatchGrowth OFF (P61_MATCHGROWTH=1 to enable). ",
      "the base is byte-identical to the reference model.\n", sep = "")
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

# --- how many ramp steps -----------------------------------------------------
AS <- sapply(members, function(si)
  as.numeric(DR$draws[[as.character(si)]]$abundance_scaling[SPN]))
max_log <- max(abs(log(AS)))
K <- max(STEPS_REQ, ceiling(max_log / log(MAX_STEP_X)))
if (K > MAX_STEPS) {
  cat(sprintf("  NOTE: %d steps needed for a %.2gx cap; clamped to P61_MAX_STEPS = %d\n",
              K, MAX_STEP_X, MAX_STEPS)); K <- MAX_STEPS
}
STEM <- sub("_KAUTO_", sprintf("_K%d_", K), STEM)
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
cat("  cores:", CORES, "| stem:", STEM, "\n\n")

# ------------------------------------------------------------------ worker ---
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
  # Catchability always. Gamma ONLY if USE_GAMMA_DRAW -- that switch is the
  # single variable this phase isolates.
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
    # species_params<- SILENTLY ZEROES ext_encounter. Restore by SLOT
    # assignment, not setExtEncounter() -- the setter rewrites the dimnames
    # attribute structure and then all.equal fails on attributes.
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

  # --- 2. the ramp ---------------------------------------------------------
  tr <- list(); prev_t <- 0
  for (k in seq_len(K)) {
    tk <- k / K
    inc <- sc^tk / sc^prev_t                 # incremental factor for this step
    prev_t <- tk
    for (j in seq_along(inc)) p@initial_n[j, ] <- p@initial_n[j, ] * inc[j]

    # PHASE 65: scale R_max by the SAME factor. Without this the perturbed
    # spawning stock is pressed against an unscaled recruitment ceiling, the
    # reproduction level is driven toward 1, and steady() relaxes most of the
    # draw back out -- measured transmission below 0.20 for 12 of 19 groups.
    # species_params<- ZEROES ext_encounter; restore by slot as at line 346.
    if (SCALE_RMAX && RMAX_WHEN == "ramp") {
      spm <- species_params(p)
      spm$R_max <- spm$R_max * inc
      species_params(p) <- spm
      p@ext_encounter <- ext0
      if (!isTRUE(all.equal(p@ext_encounter, ext0)))
        return(fail(sprintf("subsidy_lost_in_rmax_scale_step%d", k),
                    list(trace = do.call(rbind, tr))))
    }

    # matchGrowth is OFF by default -- it is what breaks this protocol. See the
    # MEASURED ATTRIBUTION note in the header.
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

    if (!PRECAP_STEADY) next        # phase-60 structure: draws straight to cap
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
  # WHERE DID THE WALK ACTUALLY LAND? The ramp is a PATH, not a constraint: at
  # each step steady() moves the abundances away from the multiplier that was
  # just applied, so the destination is NOT base x abundance_scaling. Record the
  # realised biomass ratio against the base and against the drawn target, so the
  # gap is visible instead of assumed. (Checking prev_t == 1 would only verify
  # the loop arithmetic.)
  b_dest <- biom(p)
  realised_ratio <- b_dest / BIOM_BASE

  # --- 2b. PHASE 65 "post": scale R_max only AFTER the ramp has settled -----
  # The ramp ran under preserve = "erepro", so erepro is pinned at the reference
  # value and R_max has floated to whatever supports the settled state. Handing
  # the ceiling its multiplier HERE, and only then switching to
  # preserve = "R_max" for the ladder, is a different operation from scaling the
  # reference R_max up front.
  if (SCALE_RMAX && RMAX_WHEN == "post") {
    spm <- species_params(p)
    spm$R_max <- spm$R_max * sc
    species_params(p) <- spm
    p@ext_encounter <- ext0
    if (!isTRUE(all.equal(p@ext_encounter, ext0)))
      return(fail("subsidy_lost_in_rmax_post_scale",
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
  # THIS IS THE ORDER THAT ACTUALLY TESTS THE IDEA. setBevertonHolt(
  # reproduction_level = L) sets R_max = RDD / L, so the recap OVERWRITES any
  # R_max set before it -- which is why RMAX_WHEN = "post" produced whale
  # reproduction levels pinned at exactly 0.90 and no change in whale
  # recruitment. Scaling here, with preserve = "R_max" in the ladder that
  # follows, is the only placement where the scaled ceiling survives.
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
  cat(sprintf("chunks %d/%d complete | %d member states written\n",
              sum(vapply(seq_along(chunks), function(ci)
                file.exists(chunk_file(ci)), logical(1))), length(chunks),
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
                        "RECAP", "SCALE_RMAX", "RMAX_WHEN", "STEADY_TMAX",
                        "RAMP_TMAX", "SPINUP_YEARS",
                        "STAB", "STATE_DIR", "BIOM_BASE", "USE_MATCHGROWTH",
                        "USE_GAMMA_DRAW", "PRECAP_STEADY"),
                  envir = environment())
    for (ci in todo) {
      tc <- proc.time()
      r <- parLapply(cl, chunks[[ci]], worker)
      saveRDS(r, chunk_file(ci))
      cat(sprintf("[%s] chunk %d/%d done (%d members, %.1f min)\n",
                  format(Sys.time(), "%H:%M:%S"), ci, length(chunks),
                  length(chunks[[ci]]), (proc.time() - tc)[["elapsed"]] / 60))
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
if (any(!ok)) cat("  failures:\n", paste(vapply(res[!ok], function(r)
  sprintf("    %d: %s", r$sim_index, r$reason), ""), collapse = "\n"), "\n")

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
  cat("\n=== members ===\n"); print(MEM, row.names = FALSE, digits = 4)
  cat("\n  reached tol", min(TOLS), ":", sum(MEM$tightest_tol <= min(TOLS)),
      "of", nrow(MEM), "| stable:", sum(MEM$stable),
      "| ADMISSIBLE (no erepro >= 1):", sum(MEM$n_erepro_ge1 == 0), "\n")
  # The ramp is a PATH, not a constraint -- steady() moves the abundances at
  # every step, so the destination is NOT base x abundance_scaling. This is the
  # size of that gap, and it must be reported rather than assumed away.
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
                           gamma_base = setNames(GAMMA0, SPN),
                           built = format(Sys.time()))), OUT_RDS)
  write.csv(MEM, file.path(OUT_LARGE, paste0(STEM, "_members.csv")),
            row.names = FALSE)
  write.csv(PS, file.path(OUT_LARGE, paste0(STEM, "_per_species.csv")),
            row.names = FALSE)
  write.csv(TRACE, file.path(OUT_LARGE, paste0(STEM, "_trace.csv")),
            row.names = FALSE)
  cat("\nwrote", OUT_RDS, "and", length(list.files(STATE_DIR)), "member states\n")
}
cat("elapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")
cat("Phase 61 complete.\n")