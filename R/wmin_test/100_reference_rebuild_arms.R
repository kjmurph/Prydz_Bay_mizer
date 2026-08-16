# =============================================================================
# Phase 100 -- rebuild the reference model: three changes, six ladder arms
#
# Each arm applies an edit to the reference model and then runs the PROJECT'S
# STANDARD recalibration ladder, transcribed from 63_recalibrate_ladder.R (which
# took it from 59, 54, 51, 42). The ladder is not modified. What is new is that
# six arms run it, so each change can be attributed.
#
# ------------------------------------------------------------------- why now
# Phase 99 established that the phase-88 member states are not steady states:
# 56% of the 427 usable members lose at least one species entirely over 2000
# unfished years under protocol forcing, only 3.3% hold all 19 within [0.5, 2],
# and the stored 1841 state holds baleen 1.34x and sperm 2.07x ABOVE their own
# equilibria. The reference model itself is fine (0.879-1.037), so the defect is
# in what the member protocol does to it -- and the root cause is that steady()
# cannot resolve a stock whose relaxation time is 780 years.
#
# ------------------------------------------------------------- THE THREE EDITS
# A. WHALE MORTALITY (edit "z0"). z0 = 0.6 * w_inf^(-1/3), a fish-derived
#    allometry, gives biomass-weighted total mortality of 0.00128/yr for baleen
#    whales -- a MEAN lifespan of 781 years and an implied MAXIMUM age of 3,592.
#    Kieran's targets, as maximum ages: baleen 90, sperm 90, orca 80, minke 50.
#    Sperm was raised 70 -> 90 after the first run: see the note at P100_TMAX_SPERM.
#
#    CONVENTION. M is taken from Hoenig (1983), M = 4.22 / t_max. Measured
#    against published cetacean M, this is the one that fits:
#
#      species   t_max   3/t_max  HOENIG  4.6/t_max  Then2015   published M
#      minke      50      0.060   0.0844    0.0920    0.1360     ~0.085
#      orca       80      0.0375  0.0528    0.0575    0.0885     ~0.04-0.09
#      sperm      70      0.0429  0.0603    0.0657    0.1000     ~0.055-0.07
#      baleen     90      0.0333  0.0469    0.0511    0.0794     ~0.04-0.06
#
#    Then et al. (2015) was fitted to ~200 FISH stocks and lands above every
#    published cetacean range; do not use it here. P100_CONV switches it anyway,
#    so the choice is visible rather than buried.
#
#    z0 is EXTERNAL mortality, so the target is set on TOTAL M and the realised
#    predation mortality is subtracted. That is why the orca edit is applied
#    FIRST within any arm containing both -- orca predation is 52% of minke's
#    mortality, so the minke z0 depends on it.
#
# B. THE ORCA KERNEL (edit "orca"). beta 0.5579 -> 45.7, sigma 2.5 -> 2.0.
#    The current beta is faithfully derived from
#    csvs/predator_parameters_updated.csv, but that row's SOURCE CELL IS EMPTY,
#    and it puts orca's preferred prey at 19 TONNES -- heavier than the orca.
#    45.7 is from Tucker & Rogers (2014): body mass 10^3.96 kg, prey 10^2.3 kg.
#    sigma 2.5 was an orphaned hand-tune set alongside a beta of 500 that was
#    later reverted; 2.0 is the value every other group carries except leopard
#    seals, and it is the widest sigma at which orca's kernel does not reach
#    into the zooplankton (plankton diet share 0.0000 at 2.0, 0.0027 at 3.0).
#
# C. THE OUT-OF-DOMAIN SUBSIDY (edit "subsidy"). The realised External diet
#    share should match the intended p_feed_outside from phase 57. It does not:
#    leopard seals 0.790 against 0.25, flying birds 0.894 against 0.58, while
#    seven of nine land within 0.54-1.28 of intent. Because the share is an
#    ENCOUNTER share, the exact correction is a ratio of ODDS,
#
#        k_i = [p_i / (1 - p_i)] / [s_i / (1 - s_i)]
#
#    applied as a scalar on each species' ext_encounter row. It is a fixed point,
#    not a one-shot -- changing ext_encounter moves the feeding level, the
#    spectrum and hence the in-domain encounter -- so it is iterated
#    P100_SUB_ROUNDS times against a short steady() before the full ladder.
#
#    EXPECT THE SUBSIDY ARM ALONE TO STRUGGLE, and that is informative. The two
#    groups needing the largest cuts (x0.088, x0.163) are exactly the two whose
#    mean interaction with abundant in-domain prey is lowest (0.056, 0.028
#    against 0.11-0.28 for every other predator). Cutting their subsidy without
#    letting them feed in-domain removes 80-90% of their food.
#    P100_INT_LIFT addresses that, and is OFF by default because the values are
#    a scientific choice, not a derivation -- see below.
#
# --------------------------------------------------------------- THE ARMS
#   control    no edit. NOT optional: re-laddering moves things on its own
#              (phase 54 measured erepro shifts of up to 16% from the ladder
#              alone), so without it nothing below is attributable.
#   z0         A only          orca       B only
#   subsidy    C only          z0_orca    B then A
#   all        B then A then C
#
# --------------------------------------------------------------- NOT INCLUDED
# The phase-88 spin-up fix -- t_max = 120 indexing the LAST row, so the spin-up
# is exactly six ENSO cycles and lands on cycle position 0 -- is a change to the
# MEMBER protocol, not to the reference model, so it does not belong in a ladder
# arm. It goes in the rebuild script that replaces phase 88.
#
# P100_INT_LIFT scales the leopard seal and flying bird interaction rows so
# their mean against the LTL+fish columns reaches the given value (peer median
# is ~0.25). It is an ASSUMPTION LAYER and defaults to 0 = off. Turning it on
# without choosing and recording the values would repeat exactly the mistake
# that produced the orphaned orca sigma.
#
# USAGE  Rscript R/wmin_test/100_reference_rebuild_arms.R [dry|run]
# ENV    P100_BASE, P100_ARMS, P100_CONV, P100_TMAX_BALEEN/SPERM/ORCA/MINKE,
#        P100_SUB_ROUNDS, P100_INT_LIFT, P100_CORES, P100_TARGET,
#        P100_PRESERVE, P100_OUT
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

BASE_FILE <- Sys.getenv("P100_BASE", "params_ref_p86_agemat.rds")
OUT_DIR   <- Sys.getenv("P100_OUT", file.path("Output_large_files", "wmin_test",
                                              "100_rebuild"))
ARMS <- trimws(strsplit(Sys.getenv("P100_ARMS",
  "control,z0,orca,subsidy,z0_orca,all"), ",")[[1]])
CORES <- min(as.integer(Sys.getenv("P100_CORES", "6")),
             max(1L, parallel::detectCores() - 2L), length(ARMS))
REF_YEAR <- 1841L

# --- the mortality targets ----------------------------------------------------
TMAX <- c("baleen whales" = as.numeric(Sys.getenv("P100_TMAX_BALEEN", "90")),
          # 90, not 70: at 70 the recalibration puts sperm erepro at 0.935
          # against the 1.0 admissibility ceiling, leaving no headroom for the
          # member protocol where erepro floats. 90 is also well inside the
          # aged range for sperm whales.
          "sperm whales"  = as.numeric(Sys.getenv("P100_TMAX_SPERM",  "90")),
          "orca"          = as.numeric(Sys.getenv("P100_TMAX_ORCA",   "80")),
          "minke whales"  = as.numeric(Sys.getenv("P100_TMAX_MINKE",  "50")))
# P100_TMAX_CSV overrides the four above with a per-species table (columns
# `species`, `t_max`). Rows with a blank or NA t_max are LEFT ALONE, so the file
# can carry all 19 groups and you enable them one at a time. `dry` writes a
# template pre-filled with each group's CURRENT implied t_max, so an unedited
# template is a no-op by construction.
TMAX_CSV <- Sys.getenv("P100_TMAX_CSV", "")
CONV <- Sys.getenv("P100_CONV", "hoenig")
CONV_FN <- switch(CONV,
  hoenig  = function(t) 4.22 / t,          # Hoenig 1983 -- the default
  rule3   = function(t) 3 / t,             # 5% survival
  rule46  = function(t) 4.6 / t,           # 1% survival
  then2015 = function(t) 4.899 * t^-0.916, # FISH-calibrated; too high for whales
  stop("unknown P100_CONV: ", CONV, call. = FALSE))

# --- the orca kernel ----------------------------------------------------------
ORCA_BETA  <- as.numeric(Sys.getenv("P100_ORCA_BETA", "45.7"))   # Tucker & Rogers 2014
ORCA_SIGMA <- as.numeric(Sys.getenv("P100_ORCA_SIGMA", "2.0"))

# --- the subsidy solve --------------------------------------------------------
SUB_ROUNDS <- as.integer(Sys.getenv("P100_SUB_ROUNDS", "3"))
SUB_POST   <- Sys.getenv("P100_SUB_POST", "1") == "1"   # re-solve after the ladder
SUB_KMIN <- 0.05; SUB_KMAX <- 20        # guard one wild round; not a target
INT_LIFT <- as.numeric(Sys.getenv("P100_INT_LIFT", "0"))
LIFT_SPECIES <- c("leopard seals", "flying birds")

# --- the ladder, transcribed from 63_recalibrate_ladder.R ---------------------
TARGET <- as.numeric(Sys.getenv("P100_TARGET", "0.001"))
PRES   <- Sys.getenv("P100_PRESERVE", "reproduction_level")
STEADY_TMAX   <- as.numeric(Sys.getenv("P100_TMAX_STEADY", "1000"))
MATCH_ROUNDS  <- as.integer(Sys.getenv("P100_MATCH_ROUNDS", "6"))
TARGET_ROUNDS <- as.integer(Sys.getenv("P100_TARGET_ROUNDS", "14"))
CAP    <- as.numeric(Sys.getenv("P100_CAP", "0.9"))
RUNGS  <- c(0.1, 0.05, 0.01, 0.005, 0.002, 0.001)

mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "dry"
stopifnot(mode %in% c("dry", "run"),
          all(ARMS %in% c("control","z0","orca","subsidy","wmin","z0_orca",
                          "wmin_z0","ppmr","diet","feed","feed_z0",
                          "all","all_feed","maxint","maxint_z0","all_max")))
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

t0 <- proc.time()
cat("=== Phase 100: reference rebuild |", mode, "===\n")
if (!file.exists(BASE_FILE)) stop("missing base: ", BASE_FILE, call. = FALSE)
BASE <- suppressWarnings(validParams(readRDS(BASE_FILE)))
stopifnot(identical(BASE@rates_funcs$Encounter, "therMizerEncounter"),
          identical(BASE@resource_dynamics, "plankton_forcing"))
if (all(BASE@ext_encounter == 0))
  stop("base carries no out-of-domain subsidy -- wrong reference model",
       call. = FALSE)
SPN <- BASE@species_params$species
WDW <- BASE@w * BASE@dw
EE0 <- BASE@ext_encounter
stopifnot(!anyNA(match(names(TMAX), SPN)), !anyNA(match(LIFT_SPECIES, SPN)))

FEED_F <- file.path("Output_large_files", "wmin_test", "57_p_feed_outside.csv")
if (!file.exists(FEED_F))
  stop("missing ", FEED_F, " -- the subsidy targets live there", call. = FALSE)
FEED <- read.csv(FEED_F, stringsAsFactors = FALSE)
P_TARGET <- setNames(FEED$p_feed_outside[match(SPN, FEED$species)], SPN)
P_TARGET[is.na(P_TARGET)] <- 0

if (nzchar(TMAX_CSV)) {
  if (!file.exists(TMAX_CSV)) stop("missing P100_TMAX_CSV: ", TMAX_CSV,
                                   call. = FALSE)
  TT <- read.csv(TMAX_CSV, stringsAsFactors = FALSE)
  if (!all(c("species", "t_max") %in% names(TT)))
    stop(TMAX_CSV, " needs columns `species` and `t_max`", call. = FALSE)
  bad <- setdiff(TT$species, SPN)
  if (length(bad)) stop("species in ", TMAX_CSV, " not in the model: ",
                        paste(bad, collapse = ", "), call. = FALSE)
  TT <- TT[!is.na(TT$t_max) & TT$t_max > 0, , drop = FALSE]
  if (!nrow(TT)) stop(TMAX_CSV, " sets no usable t_max", call. = FALSE)
  TMAX <- setNames(as.numeric(TT$t_max), TT$species)
  cat("t_max targets read from", TMAX_CSV, ":", nrow(TT), "group(s)\n")
}
cat("base:", BASE_FILE, "| arms:", paste(ARMS, collapse = ", "), "\n")
cat("M convention:", CONV, "| ladder target", TARGET, "preserve", PRES, "\n")
cat("orca kernel: beta", ORCA_BETA, "sigma", ORCA_SIGMA,
    "| subsidy rounds", SUB_ROUNDS, "| interaction lift",
    if (INT_LIFT > 0) INT_LIFT else "OFF", "\n\n")

# ------------------------------------------------------------------ helpers ---
bio_ratio <- function(p)
  as.numeric(getBiomass(p, use_cutoff = TRUE) / p@species_params$biomass_observed)
max_dev <- function(p) max(abs(bio_ratio(p) - 1))
rl_of <- function(p) as.numeric(getReproductionLevel(p))
er_of <- function(p) as.numeric(p@species_params$erepro)

rates_at <- function(p) mizer::getRates(p, n = p@initial_n, n_pp = p@initial_n_pp,
  n_other = p@initial_n_other, effort = 0, t = REF_YEAR)

# biomass-weighted total mortality per species
tot_M <- function(p, r = NULL) {
  if (is.null(r)) r <- rates_at(p)
  B <- sweep(p@initial_n, 2, WDW, "*")
  rowSums(r$mort * B) / rowSums(B)
}
# realised share of the diet coming from the out-of-domain subsidy
ext_share <- function(p) {
  D <- getDiet(p, proportion = TRUE)
  vapply(seq_along(SPN), function(i) {
    bw <- as.numeric(p@initial_n[i, ] * WDW)
    if (!is.finite(sum(bw)) || sum(bw) <= 0) return(NA_real_)
    v <- colSums(sweep(D[i, , ], 1, bw, "*"))
    if (sum(v) <= 0) return(NA_real_)
    unname(v[["External"]] / sum(v))
  }, 0)
}
# INVERSE OF HOENIG, FOR REPORTING ONLY. mizer has no age dimension, so this is
# not a model output: it says what maximum age a real stock with this mortality
# would have. It is also CIRCULAR for any group whose z0 was set from a t_max
# target -- it returns the target by construction.
implied_tmax <- function(M) 4.22 / M

# ADULT mortality is what z0 should be set against: z0 is size-flat, and both
# published M estimates and Hoenig's t_max relation refer to adults. For 18 of
# 19 groups this is within 10% of the all-size mean; toothfishes are the
# exception (ratio 1.48), because the model gives them heavy juvenile predation
# and near-zero adult mortality.
adult_M <- function(p, r = NULL) {
  if (is.null(r)) r <- rates_at(p)
  sp <- p@species_params; B <- sweep(p@initial_n, 2, WDW, "*")
  vapply(seq_len(nrow(sp)), function(i) {
    s <- p@w >= sp$w_mat[i]
    if (!any(s) || sum(B[i, s]) <= 0) return(NA_real_)
    sum(r$mort[i, s] * B[i, s]) / sum(B[i, s])
  }, 0)
}

# THE MIZER-NATIVE CHECK, with no Hoenig assumption. Age at size is the same
# integral age_mat() uses, dw/g; survival to that size is exp(-int mu/g dw).
# Maximum age = the age at which survival falls to `thresh`. Threshold-sensitive
# where adult mortality is tiny: toothfishes give 15.6 yr at 1% and 472 at 0.1%,
# because the survivors then sit at terminal size almost indefinitely. Use 1%,
# and use it as a CROSS-CHECK rather than as the target.
surv_max_age <- function(p, thresh = 0.01, r = NULL) {
  if (is.null(r)) r <- rates_at(p)
  g <- getEGrowth(p); mu <- r$mort; sp <- p@species_params
  vapply(seq_len(nrow(sp)), function(i) {
    sel <- which(p@w >= sp$w_min[i] & p@w < sp$w_max[i] & g[i, ] > 0)
    if (!length(sel)) return(NA_real_)
    dt <- p@dw[sel] / g[i, sel]
    a <- cumsum(dt); Z <- cumsum(mu[i, sel] * dt)
    k <- which(exp(-Z) <= thresh)[1]
    if (!is.na(k)) return(a[k])
    mt <- mu[i, sel[length(sel)]]
    if (!is.finite(mt) || mt <= 0) return(NA_real_)
    a[length(a)] + (-log(thresh) - Z[length(Z)]) / mt
  }, 0)
}

# mizer 3.1.0 signals non-convergence with message(), not warning()
steady_guarded <- function(p, tol) {
  nc <- FALSE
  out <- withCallingHandlers(
    try(steady(p, tol = tol, t_max = STEADY_TMAX, preserve = PRES,
               progress_bar = FALSE), silent = TRUE),
    message = function(m) {
      if (grepl("did not converge", conditionMessage(m), ignore.case = TRUE))
        nc <<- TRUE
      invokeRestart("muffleMessage")
    }, warning = function(w) invokeRestart("muffleWarning"))
  list(params = out, converged = !nc, errored = inherits(out, "try-error"))
}
match_guarded <- function(p) {
  out <- withCallingHandlers(try(matchBiomasses(p), silent = TRUE),
    message = function(m) invokeRestart("muffleMessage"),
    warning = function(w) invokeRestart("muffleWarning"))
  list(params = out, errored = inherits(out, "try-error"))
}

# -------------------------------------------------------------------- edits ---
# species_params<- can zero ext_encounter (phase 57). Every edit restores it by
# SLOT and asserts, because setExtEncounter() rewrites the dimnames structure and
# then all.equal fails on attributes.
edit_orca <- function(p) {
  ee <- p@ext_encounter
  s <- species_params(p); i <- match("orca", s$species)
  s$beta[i] <- ORCA_BETA; s$sigma[i] <- ORCA_SIGMA
  species_params(p) <- s; p@ext_encounter <- ee
  stopifnot(isTRUE(all.equal(p@ext_encounter, ee)))
  p
}
# z0 is EXTERNAL mortality: subtract the realised predation so TOTAL M hits the
# target. Applied AFTER the orca edit wherever both are present.
# THE TARGET IS ADULT M, not the all-size mean. z0 is size-flat, and both
# published M estimates and Hoenig's t_max relation refer to adults. For 18 of 19
# groups the two bases agree within 10%; toothfishes differ by 1.45x because the
# model gives them heavy juvenile predation and near-zero adult mortality, and
# their target is specified as an adult M directly. Using the all-size mean there
# would undershoot the intended adult M by that same 1.45x.
edit_z0 <- function(p) {
  ee <- p@ext_encounter
  s <- species_params(p)
  Mp <- adult_M(p) - s$z0               # adult predation (effort = 0, no f_mort)
  i <- match(names(TMAX), s$species)
  z_new <- CONV_FN(TMAX) - Mp[i]
  if (any(z_new <= 0))
    warning("predation alone already exceeds the target M for: ",
            paste(names(TMAX)[z_new <= 0], collapse = ", "), call. = FALSE)
  s$z0[i] <- pmax(z_new, 1e-4)
  species_params(p) <- s; p@ext_encounter <- ee
  stopifnot(isTRUE(all.equal(p@ext_encounter, ee)))
  p
}
# w_min for the pinniped groups currently uses the source table's WEANING mass,
# not birth mass: leopard seals 200 kg (44% of adult mass), large divers 135 kg.
# erepro is proportional to w_min via mizerRDI, which uses the GRID-SNAPPED value
# params@w[params@w_min_idx], not the nominal -- 200 kg lands on 155.7 kg and
# 104 kg on 75.7 kg, so the realised reductions are 2.06x and 4.23x.
# MEASURED: on its own this is nearly a no-op (biomass ratio 0.998), but combined
# with the raised mortality it is what makes those groups admissible at all.
WMIN_BIRTH <- c("leopard seals" = 104000, "large divers" = 35000)
edit_wmin <- function(p) {
  ee <- p@ext_encounter; s <- species_params(p)
  i <- match(names(WMIN_BIRTH), s$species)
  s$w_min[i] <- as.numeric(WMIN_BIRTH)
  species_params(p) <- s; p@ext_encounter <- ee
  stopifnot(isTRUE(all.equal(p@ext_encounter, ee)))
  p
}
# D. SMALL DIVER PPMR. beta 293.8 is 1/mean(member ratios), and gentoo's 42.8 g
# outlier dominates Adelie's 1.79 g and macaroni's 1.72 g, so the group targets
# 20.4 g -- 5x the largest krill (4.17 g), which is why their adult feeding level
# is 0.156. 960 is the GEOMETRIC mean of the same three ratios, the standard fix
# for averaging ratios on a log scale, and puts preferred prey at 6.25 g.
SD_BETA <- as.numeric(Sys.getenv("P100_SD_BETA", "960"))
edit_ppmr <- function(p) {
  ee <- p@ext_encounter; s <- species_params(p)
  s$beta[match("small divers", s$species)] <- SD_BETA
  species_params(p) <- s; p@ext_encounter <- ee
  stopifnot(isTRUE(all.equal(p@ext_encounter, ee)))
  p
}
# E. INTERACTION EDITS, from csvs/interaction_edits_v1.csv so the values and
# their basis are auditable rather than inline constants. Applies only to the
# named predator x prey cells; predator-on-predator entries are untouched.
INT_CSV <- Sys.getenv("P100_INT_CSV", "csvs/interaction_edits_v1.csv")
edit_diet <- function(p) {
  if (!file.exists(INT_CSV)) stop("missing ", INT_CSV, call. = FALSE)
  E <- read.csv(INT_CSV, stringsAsFactors = FALSE)
  bad <- setdiff(c(E$predator, E$prey), SPN)
  if (length(bad)) stop("interaction edits name unknown species: ",
                        paste(unique(bad), collapse = ", "), call. = FALSE)
  if (any(E$value < 0 | E$value > 1)) stop("interaction values must be in [0,1]",
                                           call. = FALSE)
  for (k in seq_len(nrow(E)))
    p@interaction[E$predator[k], E$prey[k]] <- E$value[k]
  p
}
# F. THE FEEDING CEILING TRIAL. Every NON-ZERO entry in the three blocking
# groups' PREDATOR rows is set to 1 -- i.e. maximal access to everything they
# already eat at all. This is a BOUNDING TEST, not a parameterisation: it answers
# "is erepro < 1 reachable for these groups by feeding alone?" and it deliberately
# OVERRIDES the diet edits for those rows (large divers' plankton entries go back
# from 0.05 to 1). Zero entries stay zero, so it never invents a new trophic link,
# and only the rows are touched -- raising their columns would increase predation
# ON them, which is the opposite of the intent.
MAXINT_SPECIES <- trimws(strsplit(Sys.getenv("P100_MAXINT_SPECIES",
  "leopard seals,large divers,small divers"), ",")[[1]])
edit_maxint <- function(p) {
  i <- match(MAXINT_SPECIES, SPN)
  if (anyNA(i)) stop("P100_MAXINT_SPECIES not in the model: ",
                     paste(MAXINT_SPECIES[is.na(i)], collapse = ", "), call. = FALSE)
  for (k in i) p@interaction[k, p@interaction[k, ] > 0] <- 1
  p
}
edit_int_lift <- function(p) {
  if (INT_LIFT <= 0) return(p)
  cols <- setdiff(SPN, c("minke whales","orca","sperm whales","baleen whales"))
  for (g in LIFT_SPECIES) {
    i <- match(g, SPN)
    cur <- mean(p@interaction[i, cols])
    if (cur > 0) p@interaction[i, cols] <-
      pmin(1, p@interaction[i, cols] * (INT_LIFT / cur))
  }
  p
}
# the odds-ratio solve, iterated against a short steady()
edit_subsidy <- function(p) {
  p <- edit_int_lift(p)
  for (r in seq_len(SUB_ROUNDS)) {
    s <- ext_share(p)
    k <- rep(1, length(SPN))
    j <- which(P_TARGET > 0 & is.finite(s) & s > 1e-8 & s < 1 - 1e-8)
    k[j] <- (P_TARGET[j] / (1 - P_TARGET[j])) / (s[j] / (1 - s[j]))
    k <- pmin(pmax(k, SUB_KMIN), SUB_KMAX)
    p@ext_encounter <- sweep(p@ext_encounter, 1, k, "*")
    st <- steady_guarded(p, 0.01)
    if (st$errored) break
    p <- st$params
  }
  p
}
# wmin comes BEFORE z0: the z0 target subtracts realised predation, and widening
# the spectrum downward changes it.
ARM_DEF <- list(control = character(0), z0 = "z0", orca = "orca",
                subsidy = "subsidy", wmin = "wmin",
                z0_orca = c("orca", "z0"),
                # `wmin` is RETAINED ONLY AS A DOCUMENTED NEGATIVE RESULT and is
                # deliberately NOT in `all`: measured 2026-08-16, birth-mass
                # w_min takes max erepro 21.8 -> 41.1 because mizer has no
                # parental care and the pups die before maturing.
                wmin_z0 = c("wmin", "z0"),
                # feeding edits come BEFORE z0: the z0 target subtracts realised
                # predation, which they change.
                ppmr = "ppmr", diet = "diet",
                feed = c("ppmr", "diet"),
                feed_z0 = c("ppmr", "diet", "z0"),
                all = c("orca", "z0", "subsidy"),
                all_feed = c("orca", "ppmr", "diet", "z0", "subsidy"),
                # maxint AFTER diet, so the ceiling overrides those rows
                maxint = "maxint",
                maxint_z0 = c("maxint", "z0"),
                all_max = c("orca", "ppmr", "diet", "maxint", "z0", "subsidy"))
apply_arm <- function(arm) {
  p <- BASE
  for (e in ARM_DEF[[arm]])
    p <- switch(e, orca = edit_orca(p), z0 = edit_z0(p),
                subsidy = edit_subsidy(p), wmin = edit_wmin(p),
                ppmr = edit_ppmr(p), diet = edit_diet(p),
                maxint = edit_maxint(p),
                stop("no handler for edit '", e, "'", call. = FALSE))
  if (!is(p, "MizerParams"))
    stop("edit '", e, "' did not return a MizerParams", call. = FALSE)
  p
}

# ------------------------------------------------------- the ladder (from 63) --
run_ladder <- function(P0, arm) {
  ladder <- RUNGS[RUNGS >= TARGET]
  st <- steady_guarded(P0, ladder[1])
  if (st$errored) return(list(pass = FALSE, reason = "steady_error_leading"))
  p <- st$params
  best <- NULL; best_d <- Inf; tr <- list(); n_conv <- 0; n_adm <- 0
  for (tol in ladder) {
    at <- isTRUE(all.equal(tol, TARGET))
    for (r in seq_len(if (at) TARGET_ROUNDS else MATCH_ROUNDS)) {
      mt <- match_guarded(p)
      if (mt$errored) return(list(pass = FALSE, reason = "match_error",
                                  trace = do.call(rbind, tr)))
      p <- mt$params
      st <- steady_guarded(p, tol)
      if (st$errored) return(list(pass = FALSE, reason = "steady_error",
                                  trace = do.call(rbind, tr)))
      p <- st$params
      # the residual is read AFTER steady(), NEVER after matchBiomasses
      d <- max_dev(p); er <- er_of(p); me <- max(er); rl <- rl_of(p)
      adm <- me < 1
      if (at) {
        n_conv <- n_conv + st$converged
        n_adm <- n_adm + (st$converged && adm)
        if (st$converged && adm && d < best_d) { best_d <- d; best <- p }
      }
      tr[[length(tr) + 1]] <- data.frame(arm = arm, tol = tol, round = r,
        max_dev = d, max_erepro = me, n_erepro_ge1 = sum(er >= 1),
        max_repro_level = max(rl), n_above_cap = sum(rl > CAP + 1e-9),
        converged = st$converged, stringsAsFactors = FALSE)
    }
  }
  # `last` is returned even when the arm fails, so the blocking species can be
  # identified without re-running. A 6-round proxy is NOT a substitute: measured
  # 2026-08-16, it reported leopard seal erepro 0.392 where the full ladder
  # reported 41.1.
  list(pass = !is.null(best), params = best, last = p, max_dev = best_d,
       trace = do.call(rbind, tr), n_conv = n_conv, n_adm = n_adm,
       reason = if (is.null(best)) "no converged+admissible round at target" else NA)
}

# ------------------------------------------------------------------- report ---
snapshot <- function(p, arm, stage) {
  M <- tot_M(p); s <- ext_share(p)
  data.frame(arm = arm, stage = stage, species = SPN,
    M_total = signif(M, 4), implied_max_age = round(implied_tmax(M)),
    z0 = signif(p@species_params$z0, 4),
    erepro = signif(er_of(p), 4), repro_level = round(rl_of(p), 4),
    ext_share = round(s, 4), p_target = P_TARGET,
    bio_ratio = round(bio_ratio(p), 4), stringsAsFactors = FALSE)
}

if (mode == "dry") {
  # THE WORKSHEET. One row per group, pre-filled with its CURRENT implied t_max,
  # so an unedited file changes nothing. Edit t_max where you want a target and
  # blank it where you do not.
  r0 <- rates_at(BASE)
  aM <- adult_M(BASE, r0); alM <- tot_M(BASE, r0)
  WS <- data.frame(species = SPN,
    w_max_g = signif(BASE@species_params$w_max, 3),
    age_mat = round(as.numeric(mizer::age_mat(BASE)), 2),
    M_adult = signif(aM, 4), M_all_sizes = signif(alM, 4),
    adult_vs_all = round(alM / aM, 2),
    surv_max_age_1pct = round(surv_max_age(BASE, 0.01, r0), 1),
    t_max = round(implied_tmax(aM), 1),          # <- EDIT THIS COLUMN
    stringsAsFactors = FALSE)
  f <- file.path(OUT_DIR, "100_tmax_worksheet.csv")
  write.csv(WS, f, row.names = FALSE)
  cat("=== per-species mortality worksheet (t_max pre-filled = no change) ===\n")
  print(WS, row.names = FALSE)
  cat("\nWROTE", f, "\n  edit the t_max column, then re-run with P100_TMAX_CSV=",
      f, "\n", sep = "")
  cat("\n  t_max here is 4.22/M_adult (inverse Hoenig) -- NOT a mizer output.\n")
  cat("  surv_max_age_1pct is the model-native check: the age at which 1% of a\n")
  cat("  cohort remains, from the model's own growth and mortality.\n")
  cat("  Where the two disagree, the group's mortality is strongly\n")
  cat("  size-dependent and z0 alone will not reconcile it (see toothfishes).\n\n")
  cat("=== what each edit does BEFORE any recalibration ===\n\n")
  cat("mortality targets (", CONV, "):\n", sep = "")
  M0 <- tot_M(BASE)
  print(data.frame(species = names(TMAX), t_max_target = TMAX,
    M_now = signif(M0[match(names(TMAX), SPN)], 3),
    implied_max_age_now = round(implied_tmax(M0[match(names(TMAX), SPN)])),
    M_target = signif(CONV_FN(TMAX), 3),
    z0_now = signif(BASE@species_params$z0[match(names(TMAX), SPN)], 3),
    row.names = NULL))
  for (arm in ARMS) {
    p <- apply_arm(arm)
    cat("\n--- arm:", arm, "---\n")
    x <- snapshot(p, arm, "pre-ladder")
    print(x[x$p_target > 0 | x$species %in% names(TMAX),
            c("species","M_total","implied_max_age","z0","ext_share","p_target")],
          row.names = FALSE)
  }
  cat("\nDry run only -- no ladder was run, nothing was written.\n")
  cat(sprintf("elapsed %.2f min\n", (proc.time() - t0)[["elapsed"]] / 60))
  quit(save = "no")
}

# ---------------------------------------------------------------------- run ---
worker <- function(arm) {
  suppressPackageStartupMessages({library(mizer); library(therMizer)})
  p0 <- try(apply_arm(arm), silent = TRUE)
  if (inherits(p0, "try-error"))
    return(list(arm = arm, pass = FALSE, reason = paste("edit:", p0)))
  pre <- snapshot(p0, arm, "pre-ladder")
  R <- run_ladder(p0, arm)
  # POST-LADDER SUBSIDY PASS. The odds-ratio solve runs before the ladder, but
  # the ladder then moves the in-domain encounter and the realised shares drift
  # -- measured 2026-08-16, orca landed at 0.47 of its target once the feeding
  # edits were also in. Re-solve on the ladder output and re-ladder; keep the
  # second pass only if it also passes, so this can never turn a pass into a
  # failure.
  if (isTRUE(R$pass) && "subsidy" %in% ARM_DEF[[arm]] && SUB_POST) {
    R2 <- try(run_ladder(edit_subsidy(R$params), arm), silent = TRUE)
    if (!inherits(R2, "try-error") && isTRUE(R2$pass)) {
      s1 <- ext_share(R$params); s2 <- ext_share(R2$params)
      j <- which(P_TARGET > 0)
      err <- function(s) max(abs(log(s[j] / P_TARGET[j])), na.rm = TRUE)
      if (is.finite(err(s2)) && err(s2) < err(s1)) R <- R2
    }
  }
  if (!isTRUE(R$pass))
    return(list(arm = arm, pass = FALSE, reason = R$reason, pre = pre,
                trace = R$trace,
                blocking = if (is.null(R$last)) NULL else {
                  e <- R$last@species_params$erepro
                  d <- data.frame(species = SPN, erepro = signif(e, 4),
                                  stringsAsFactors = FALSE)
                  d[order(-d$erepro), ][seq_len(min(5, nrow(d))), ]
                }))
  P1 <- R$params
  saveRDS(P1, file.path(OUT_DIR, sprintf("100_%s.rds", arm)))
  list(arm = arm, pass = TRUE, max_dev = R$max_dev, trace = R$trace,
       n_conv = R$n_conv, n_adm = R$n_adm, pre = pre,
       post = snapshot(P1, arm, "post-ladder"),
       subsidy_ok = isTRUE(all.equal(as.vector(P1@ext_encounter),
                                     as.vector(EE0))) || arm %in% c("subsidy","all"))
}

cat("running", length(ARMS), "arms on", CORES, "cores\n\n")
if (CORES > 1L) {
  cl <- makeCluster(CORES)
  on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
  clusterExport(cl, ls(envir = environment())[
    !ls(envir = environment()) %in% c("cl")], envir = environment())
  RES <- parLapplyLB(cl, ARMS, function(a)
    tryCatch(worker(a), error = function(e)
      list(arm = a, pass = FALSE, reason = conditionMessage(e))))
  stopCluster(cl)
} else {
  RES <- lapply(ARMS, function(a)
    tryCatch(worker(a), error = function(e)
      list(arm = a, pass = FALSE, reason = conditionMessage(e))))
}

cat("=== arm summary ===\n")
print(do.call(rbind, lapply(RES, function(r) data.frame(
  arm = r$arm, pass = isTRUE(r$pass),
  max_biomass_dev = if (isTRUE(r$pass)) signif(r$max_dev, 5) else NA,
  max_erepro = if (isTRUE(r$pass)) signif(max(r$post$erepro), 4) else NA,
  rounds_ok = if (isTRUE(r$pass)) sprintf("%d/%d", r$n_adm, TARGET_ROUNDS) else "-",
  reason = if (isTRUE(r$pass)) "" else substr(as.character(r$reason), 1, 48),
  stringsAsFactors = FALSE))), row.names = FALSE)

ok <- vapply(RES, function(r) isTRUE(r$pass), logical(1))
# save ALWAYS, not only when something passed -- a run where every arm fails is
# exactly the one whose diagnostics are needed.
saveRDS(RES, file.path(OUT_DIR, "100_arms.rds"))
if (any(!ok)) {
  cat("\n=== blocking species in failed arms (erepro at the last round) ===\n")
  for (r in RES[!ok]) {
    cat("---", r$arm, "---\n")
    if (is.null(r$blocking)) cat("  (no state captured)\n") else
      print(r$blocking, row.names = FALSE)
  }
}
if (any(ok)) {
  POST <- bind_rows(lapply(RES[ok], `[[`, "post"))
  cat("\n=== whale mortality and lifespan, post-ladder ===\n")
  print(as.data.frame(POST %>% filter(species %in% names(TMAX)) %>%
    select(arm, species, M_total, implied_max_age, erepro, repro_level,
           bio_ratio)), row.names = FALSE)
  cat("\n=== realised vs intended out-of-domain share, post-ladder ===\n")
  print(as.data.frame(POST %>% filter(p_target > 0) %>%
    select(arm, species, ext_share, p_target) %>%
    mutate(ratio = round(ext_share / p_target, 2))), row.names = FALSE)
  saveRDS(RES, file.path(OUT_DIR, "100_arms.rds"))
  write.csv(bind_rows(lapply(RES[ok], `[[`, "trace")),
            file.path(OUT_DIR, "100_trace.csv"), row.names = FALSE)
  write.csv(POST, file.path(OUT_DIR, "100_post.csv"), row.names = FALSE)
  cat("\nWROTE", OUT_DIR, "\n")
}
cat(sprintf("elapsed %.2f min\n", (proc.time() - t0)[["elapsed"]] / 60))
cat("Phase 100 complete.\n")