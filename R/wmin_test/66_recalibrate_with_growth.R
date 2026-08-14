# =============================================================================
# Phase 66 -- phase 63's recalibration ladder WITH matchGrowth(keep = "biomass")
#
# A copy of 63_recalibrate_ladder.R. Phase 63 is NOT modified, and IS the control:
# same input (params_ref_p62_intres_balkrill075.rds), same ladder, same settings,
# one extra step. So the A/B is free.
#
# ------------------------------------------------------------------- why
# The phase-63 ladder restored the BIOMASS fit (0.14624 -> 0.01165) but never
# looks at growth, and growth moved a long way. age at maturity, p59 -> p63:
#   minke +50.8%, small divers +37.3%, squids +23.8%, flying birds +18.9%,
#   mesopelagic +16.9%, bathypelagic +12.8%, shelf fishes +9.7%, baleen +8.4%.
# Minke went from 21.5 to 32.4 yr. matchBiomasses fixes abundance by rescaling
# initial_n; nothing in the ladder puts the growth back.
#
# EXPECT THE SIGN TO BE THE OPPOSITE OF PHASE 60's FAILURE. Phase 61 found
# matchGrowth catastrophic in the MEMBER protocol because the abundance draws
# made growth FASTER, so matchGrowth applied factors BELOW 1, starved species
# back down, cut gamma and drove erepro to 1e18. Here growth has SLOWED, so the
# factors are ABOVE 1: more gamma, more food, LESS pressure on erepro. The
# phase-61 warning does not transfer to this context.
#
# ---------------------------------------------------------- what to be aware of
# Restoring growth to the p59 anchor while the food supply has been cut means
# raising gamma/h/metab/ext_encounter to compensate. That is a deliberate choice
# -- it asserts the animals still achieve their calibrated growth -- but it does
# mean the interaction_resource reduction's effect ON GROWTH is compensated away,
# leaving mainly its effect on DIET COMPOSITION and on the redirection of
# predation onto the species spectrum. Say so in the methods.
#
# w_mat IS NOT TOUCHED. The source w_mat is a copy of maximum weight for 20 of 25
# species and the model sits at w_mat/w_inf = 0.900 for four groups, which is the
# real reason the ages are decades -- but that is a separate change requiring
# literature values, and this phase deliberately does not make it.
#
# THE WORKFLOW IS NOT NEW. It is the project's standard post-parameter-change
# recalibration, traced back through 42 -> 51 -> 54 -> 58 -> 59. Stated in
# 54_reference_model_recalibrate.R:52-60 and implemented in
# 59_reladder_tighter_tolerance.R:69-128, from which run_ladder(),
# steady_guarded() and match_guarded() are transcribed here.
#
#   leading steady() at the loosest rung, then alternating
#     matchBiomasses() -> steady()
#   over a tolerance ladder, ending on steady(), keeping the BEST converged
#   state AT THE TARGET RUNG rather than the last one.
#
#   - MATCH_ROUNDS (6) alternations at each rung above the target
#   - TARGET_ROUNDS (14) alternations at the target rung
#   - the residual is read AFTER steady(), NEVER after matchBiomasses, because
#     matchBiomasses leaves the model off steady state by construction
#   - "pass" = converged AND every erepro < 1. Convergence alone is not a pass;
#     an inadmissible erepro is what sank phase 57.
#
# WHY THE CONTROL ARM. Phase 54 ran the unchanged base through the identical
# ladder so the report could separate what the PARAMETER CHANGE did from what
# RE-RUNNING THE LADDER does on its own -- which is not nothing: re-steadying is
# known to move the whale stock. Without it, every number below is unattributable.
#   ARM=treatment  params_ref_p62_intres_balkrill075.rds  (the edited model)
#   ARM=control    params_ref_p59_cap09_tol001.rds        (the unedited reference)
#
# preserve = "reproduction_level" follows phase 59, which produced the current
# reference and the phase-62 object. Phase 54 used "erepro" instead; that is the
# one knob where the lineage is not unanimous, and it matters here because it
# decides which of erepro / reproduction_level is pinned and which absorbs the
# recalibration.
#
# USAGE  Rscript R/wmin_test/63_recalibrate_ladder.R          # treatment
#        P63_ARM=control Rscript R/wmin_test/63_recalibrate_ladder.R
# ENV    P63_ARM (treatment), P63_IN, P63_OUT, P63_TARGET (0.001),
#        P63_PRESERVE (reproduction_level), P63_TMAX (1000),
#        P63_MATCH_ROUNDS (6), P63_TARGET_ROUNDS (14), P63_CAP (0.9)
# =============================================================================

suppressPackageStartupMessages({library(mizer); library(therMizer)})

ARM <- Sys.getenv("P63_ARM", "treatment")
stopifnot(ARM %in% c("treatment", "control"))
DEF_IN <- if (ARM == "treatment") {
  "params_ref_p62_intres_balkrill075.rds"
} else {
  "params_ref_p59_cap09_tol001.rds"
}
IN  <- Sys.getenv("P63_IN", DEF_IN)

out_dir <- file.path("Output_large_files", "wmin_test")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
VARIANT <- if (nzchar(Sys.getenv("P66_MG_EXCLUDE", "small divers"))) "b" else "a"
tag <- function(x) file.path(out_dir, paste0("66", VARIANT, "_", ARM, "_", x))
DEF_OUT <- if (ARM == "treatment") {
  sprintf("params_ref_p66%s_growth_intres_balkrill075.rds", VARIANT)
} else {
  tag("relad.rds")
}
OUT <- Sys.getenv("P63_OUT", DEF_OUT)

TARGET <- as.numeric(Sys.getenv("P63_TARGET", "0.001"))
PRES   <- Sys.getenv("P63_PRESERVE", "reproduction_level")
STEADY_TMAX   <- as.numeric(Sys.getenv("P63_TMAX", "1000"))
MATCH_ROUNDS  <- as.integer(Sys.getenv("P63_MATCH_ROUNDS", "6"))
TARGET_ROUNDS <- as.integer(Sys.getenv("P63_TARGET_ROUNDS", "14"))
CAP    <- as.numeric(Sys.getenv("P63_CAP", "0.9"))
RUNGS  <- c(0.1, 0.05, 0.01, 0.005, 0.002, 0.001)

guard <- function(f) {
  if (file.exists(f) && !nzchar(Sys.getenv("P63_FORCE")))
    stop("refusing to overwrite: ", f, " (set P63_FORCE=1)", call. = FALSE)
  f
}
guard(OUT)

t0 <- proc.time()
cat("=== Phase 63: recalibration ladder |", ARM, "arm ===\n")
cat("input:", IN, "\noutput:", OUT, "\n")
cat("target:", TARGET, "| preserve:", PRES, "| t_max:", STEADY_TMAX,
    "| rounds:", MATCH_ROUNDS, "/", TARGET_ROUNDS, "\n\n")
if (!file.exists(IN)) stop("missing ", IN, call. = FALSE)

P0  <- suppressWarnings(validParams(readRDS(IN)))
SPN <- P0@species_params$species
EE0 <- P0@ext_encounter          # the phase-57 subsidy must survive the ladder

# --- the growth anchor --------------------------------------------------------
# matchGrowth needs an age_mat column. With none it falls back to age_mat_vB(),
# which is built from k_vb -- a ROUND PLACEHOLDER (0.2 or 0.5) for 14 of 19
# groups. Targeting that would decalibrate the reference before a single round.
# Anchor instead on the PRE-EDIT reference's own realised age at maturity, so
# matchGrowth reads "restore growth to where the calibrated model put it".
#
# BASIS: matchGrowth computes factor = age_mat(params) / sp$age_mat, and
# age_mat() integrates from the BOTTOM OF THE SIZE GRID, not the species' egg
# size -- it counts growth through sizes the animal never occupies (62% of the
# reported age for leopard seals). Because BOTH the target and the numerator come
# from that same metric, the two bases match and no conversion is needed. These
# numbers are NOT biological ages and must not be reported as such.
# In-loop reproduction cap. NA (default) = OFF, which reproduces the phase-66,
# 73 and 74 behaviour exactly. Set e.g. P66_CAP_RL=0.9 to cap every round.
CAP_RL <- suppressWarnings(as.numeric(Sys.getenv("P66_CAP_RL", "NA")))
# Per-species ceilings that differ from CAP_RL, as "species=level,species=level".
# These are FIXED TARGETS, not a per-round multiplier: applying a x0.9 every
# round would compound to 0.9^44 = 0.009 over the ladder. Compute the target
# once (e.g. 0.9 x the level the uncapped run settled at) and pass it here.
RL_OVERRIDE <- Sys.getenv("P66_RL_OVERRIDE", "")
ANCHOR <- Sys.getenv("P66_ANCHOR", "params_ref_p59_cap09_tol001.rds")
USE_MG <- Sys.getenv("P66_MATCHGROWTH", "1") == "1"
MG_KEEP <- Sys.getenv("P66_KEEP", "biomass")
# Groups held OUT of matchGrowth. Small divers by default: their growth curve is
# anomalous (they also carry the penguin w_min anomaly, realised 2942 g against a
# nominal 3626.667), the cause is unknown, and the first phase-66 run showed the
# model cannot hold their biomass once their growth is restored -- 1.1513 of
# observed, which alone drove max biomass dev from 0.013 to 0.151. Their biomass
# is small and their maturation assumptions are already noisy, so the reference
# should not be anchored on that abnormality.
#
# THE TRAP: excluding a group by setting its age_mat to NA does NOT work.
# matchGrowth runs set_species_param_default(sp, "age_mat", age_mat_vB(params)),
# which FILLS the NA with the k_vb placeholder and targets that instead. The
# exclusion has to go through the `species` argument.
MG_EXCLUDE <- trimws(strsplit(Sys.getenv("P66_MG_EXCLUDE", "small divers"),
                              ",")[[1]])
MG_EXCLUDE <- MG_EXCLUDE[nzchar(MG_EXCLUDE)]
if (!file.exists(ANCHOR)) stop("missing anchor: ", ANCHOR, call. = FALSE)
PA <- suppressWarnings(validParams(readRDS(ANCHOR)))
if (!identical(PA@species_params$species, SPN))
  stop("anchor species order differs from the input", call. = FALSE)
AGE_TGT <- as.numeric(mizer::age_mat(PA))

# --- per-species BIOLOGICAL age-at-maturity overrides -------------------------
# Given as "species=years" in BIOLOGICAL years, i.e. the time to grow from the
# species' OWN egg size to w_mat. mizer's age_mat() instead integrates from the
# bottom of the whole size grid and so counts growth through sizes the animal
# never occupies -- for these groups that inflates the number by 1.5-2.2x. Since
# matchGrowth targets mizer's metric, the biological target is converted here:
#
#     mizer-basis target = biological target x (mizer age / own-egg-size age)
#
# The ratio is invariant under matchGrowth's uniform scaling of g, so the
# conversion is exact rather than iterative. It is computed from the INPUT
# object, not from a table, so it tracks whatever model is being calibrated.
AGE_BIO <- Sys.getenv("P66_AGE_BIO", "")
if (USE_MG && nzchar(AGE_BIO)) {
  wgrid <- P0@w; dwg <- P0@dw; ggr <- getEGrowth(P0)
  bio_age <- vapply(seq_along(SPN), function(k) {
    s <- wgrid < P0@species_params$w_mat[k] & wgrid >= wgrid[P0@w_min_idx[k]]
    if (!any(s)) return(NA_real_)
    sum(dwg[s] / ggr[k, s])
  }, numeric(1))
  mz_age <- as.numeric(mizer::age_mat(P0))
  infl <- mz_age / bio_age
  cat("\nbiological age-at-maturity overrides:\n")
  cat(sprintf("  %-16s %8s %8s %10s %10s %8s\n", "species", "bio now",
              "bio tgt", "inflation", "mizer tgt", "factor"))
  for (tok in trimws(strsplit(AGE_BIO, ",")[[1]])) {
    kv <- trimws(strsplit(tok, "=")[[1]])
    if (length(kv) != 2) stop("bad P66_AGE_BIO token: ", tok, call. = FALSE)
    k <- match(kv[1], SPN)
    if (is.na(k)) stop("unknown species in P66_AGE_BIO: ", kv[1], call. = FALSE)
    tb <- as.numeric(kv[2])
    if (!is.finite(tb) || tb <= 0)
      stop("P66_AGE_BIO needs a positive number of years: ", tok, call. = FALSE)
    AGE_TGT[k] <- tb * infl[k]
    cat(sprintf("  %-16s %8.2f %8.2f %10.3f %10.2f %8.3f\n", kv[1], bio_age[k],
                tb, infl[k], AGE_TGT[k], mz_age[k] / AGE_TGT[k]))
  }
  cat("  (factor > 1 speeds growth up, < 1 slows it down)\n")
}

if (USE_MG) {
  ext0 <- P0@ext_encounter
  spa <- species_params(P0); spa$age_mat <- AGE_TGT
  species_params(P0) <- spa
  P0@ext_encounter <- ext0        # species_params<- zeroes it (phase 57)
  stopifnot(isTRUE(all.equal(as.vector(P0@ext_encounter), as.vector(EE0))))
}
if (length(setdiff(MG_EXCLUDE, SPN)))
  stop("P66_MG_EXCLUDE names not in the model: ",
       paste(setdiff(MG_EXCLUDE, SPN), collapse = ", "), call. = FALSE)
MG_SPECIES <- setdiff(SPN, MG_EXCLUDE)
MG_SEL <- SPN %in% MG_SPECIES

# --- the per-species reproduction-level ceiling -------------------------------
CAP_VEC <- setNames(rep(CAP_RL, length(SPN)), SPN)
if (nzchar(RL_OVERRIDE)) {
  for (tok in trimws(strsplit(RL_OVERRIDE, ",")[[1]])) {
    kv <- trimws(strsplit(tok, "=")[[1]])
    if (length(kv) != 2) stop("bad P66_RL_OVERRIDE token: ", tok, call. = FALSE)
    if (!kv[1] %in% SPN) stop("unknown species in P66_RL_OVERRIDE: ", kv[1],
                              call. = FALSE)
    v <- as.numeric(kv[2])
    if (!is.finite(v) || v <= 0 || v >= 1)
      stop("P66_RL_OVERRIDE level must be in (0,1): ", tok, call. = FALSE)
    CAP_VEC[kv[1]] <- v
  }
  cat("reproduction-level ceilings overridden:\n")
  for (s in names(CAP_VEC)[CAP_VEC != CAP_RL | is.na(CAP_RL)])
    cat(sprintf("  %-24s %.4f\n", s, CAP_VEC[[s]]))
}

age_of <- function(p) as.numeric(mizer::age_mat(p))
# The growth diagnostic is reported over the MATCHED groups only; excluded
# groups are reported separately so their drift is visible, not hidden.
age_dev <- function(p) max(abs(age_of(p)[MG_SEL] / AGE_TGT[MG_SEL] - 1))
cat("growth anchor:", ANCHOR, "| matchGrowth:", USE_MG, "| keep:", MG_KEEP,
    "\nmatched groups:", length(MG_SPECIES), "of", length(SPN),
    if (length(MG_EXCLUDE))
      paste0(" | EXCLUDED: ", paste(MG_EXCLUDE, collapse = ", ")) else "",
    "\nstart max |age/target - 1| over matched groups =",
    round(age_dev(P0), 4), "\n")

bio_ratio <- function(p)
  as.numeric(getBiomass(p, use_cutoff = TRUE) / p@species_params$biomass_observed)
max_dev <- function(p) max(abs(bio_ratio(p) - 1))
rl_of   <- function(p) as.numeric(getReproductionLevel(p))
er_of   <- function(p) as.numeric(p@species_params$erepro)
rmax_of <- function(p) as.numeric(p@species_params$R_max)

cat(sprintf("start: max biomass dev %.5f | max erepro %.4f | max rl %.4f (%d > %.2f)\n\n",
            max_dev(P0), max(er_of(P0)), max(rl_of(P0)),
            sum(rl_of(P0) > CAP + 1e-9), CAP))

# --- guards, transcribed from 59_reladder_tighter_tolerance.R:69-85 ------------
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
# PHASE 66. matchGrowth scales search_vol, intake_max, metab, ext_encounter,
# gamma, h, ks and k by ONE factor per species, then keep = "biomass" rescales
# initial_n so the biomass calibration is held, and it ends on
# setBevertonHolt(params) which preserves erepro. The subsidy therefore rides
# along with the physiology instead of being stranded -- but assert it, do not
# assume it.
growth_guarded <- function(p) {
  out <- withCallingHandlers(
    try(matchGrowth(p, species = MG_SPECIES, keep = MG_KEEP), silent = TRUE),
    message = function(m) invokeRestart("muffleMessage"),
    warning = function(w) invokeRestart("muffleWarning"))
  if (inherits(out, "try-error")) return(list(params = out, errored = TRUE))
  if (all(out@ext_encounter == 0))
    return(list(params = "subsidy lost in matchGrowth", errored = TRUE))
  list(params = out, errored = FALSE)
}

# --- the ladder, transcribed from 59_reladder_tighter_tolerance.R:87-128 -------
run_ladder <- function(target) {
  ladder <- RUNGS[RUNGS >= target]
  cat("rungs:", paste(ladder, collapse = " -> "), "\n")
  p <- P0
  st <- steady_guarded(p, ladder[1])
  if (st$errored) return(list(pass = FALSE, reason = "steady_error_leading"))
  p <- st$params
  best <- NULL; best_d <- Inf; tr <- list()
  n_conv <- 0; n_adm <- 0
  for (tol in ladder) {
    at <- isTRUE(all.equal(tol, target))
    for (r in seq_len(if (at) TARGET_ROUNDS else MATCH_ROUNDS)) {
      mt <- match_guarded(p)
      if (mt$errored) return(list(pass = FALSE, reason = "match_error"))
      p <- mt$params
      # PHASE 66: growth AFTER biomass, BEFORE steady. matchBiomasses sets the
      # abundance, matchGrowth then fixes the growth while holding that biomass
      # (keep = "biomass"), and steady() re-equilibrates the result.
      if (USE_MG) {
        gt <- growth_guarded(p)
        if (gt$errored)
          return(list(pass = FALSE, reason = paste("matchGrowth:",
                                                   as.character(gt$params))))
        p <- gt$params
      }
      # IN-LOOP REPRODUCTION CAP. Position is not free: matchGrowth ends on
      # setBevertonHolt(params), which preserves erepro and RECOMPUTES R_max,
      # resetting the reproduction level. Measured on p74, one round:
      #   cap BEFORE matchGrowth: 0.9000 (0 over) -> matchGrowth 0.9017 (9 over)
      #                           -> steady 0.9017 (9 over)   CAP LOST
      #   cap AFTER  matchGrowth: matchGrowth 0.9470 (9 over) -> cap 0.9000
      #                           -> steady 0.9000 (0 over)   CAP HELD
      # steady(preserve = "reproduction_level") holds whatever level it is
      # given, so this is the only placement that survives the round.
      if (any(is.finite(CAP_VEC))) {
        tgt <- ifelse(is.finite(CAP_VEC), pmin(rl_of(p), CAP_VEC), rl_of(p))
        cp <- try(suppressWarnings(setBevertonHolt(
          p, reproduction_level = tgt)), silent = TRUE)
        if (inherits(cp, "try-error"))
          return(list(pass = FALSE, reason = "cap_error"))
        p <- cp
      }
      st <- steady_guarded(p, tol)
      if (st$errored) return(list(pass = FALSE, reason = "steady_error"))
      p <- st$params
      d <- max_dev(p); er <- er_of(p)
      me <- max(er); nbad <- sum(er >= 1); rl <- rl_of(p)
      adm <- me < 1
      if (at) {
        n_conv <- n_conv + st$converged
        n_adm  <- n_adm + (st$converged && adm)
        if (st$converged && adm && d < best_d) { best_d <- d; best <- p }
      }
      ad <- age_dev(p)
      tr[[length(tr) + 1]] <- data.frame(
        arm = ARM, tol = tol, round = r, max_dev = d, max_age_dev = ad,
        max_erepro = me, n_erepro_ge1 = nbad, max_repro_level = max(rl),
        n_above_cap = sum(rl > CAP + 1e-9), converged = st$converged)
      cat(sprintf("   tol=%-6.3g r=%-2d dev %.5f  agedev %.4f  max erepro %.4f (%d>=1)  max rl %.4f (%d>cap)  %s\n",
                  tol, r, d, ad, me, nbad, max(rl), sum(rl > CAP + 1e-9),
                  if (st$converged) "conv" else "NO-CONV"))
      flush.console()
    }
  }
  list(pass = !is.null(best), params = best, max_dev = best_d,
       trace = do.call(rbind, tr), n_conv = n_conv, n_adm = n_adm,
       reason = if (is.null(best)) "no converged+admissible round at target" else NA)
}

R <- run_ladder(TARGET)
write.csv(R$trace, tag("trace.csv"), row.names = FALSE)

if (!isTRUE(R$pass)) {
  cat("\nFAILED: ", R$reason, " -- ", OUT, " was NOT written.\n", sep = "")
  cat(sprintf("elapsed %.1f min\n", (proc.time() - t0)[["elapsed"]] / 60))
  quit(save = "no", status = 0)
}

P1 <- R$params

# --- the deliverable: erepro and reproduction level, before and after ----------
before_rl <- rl_of(P0); after_rl <- rl_of(P1)
before_er <- er_of(P0); after_er <- er_of(P1)
rep_df <- data.frame(
  arm = ARM,
  species = SPN,
  growth_matched = MG_SEL,
  age_target = round(AGE_TGT, 3),
  age_before = round(age_of(P0), 3),
  age_after  = round(age_of(P1), 3),
  age_pct_vs_target = round(100 * (age_of(P1) / AGE_TGT - 1), 1),
  erepro_before = signif(before_er, 5),
  erepro_after  = signif(after_er, 5),
  erepro_ratio  = signif(after_er / before_er, 4),
  repro_level_before = round(before_rl, 5),
  repro_level_after  = round(after_rl, 5),
  repro_level_delta  = round(after_rl - before_rl, 5),
  R_max_ratio   = signif(rmax_of(P1) / rmax_of(P0), 4),
  bio_ratio_before = round(bio_ratio(P0), 4),
  bio_ratio_after  = round(bio_ratio(P1), 4),
  stringsAsFactors = FALSE)
print(rep_df, row.names = FALSE)
write.csv(rep_df, tag("per_species.csv"), row.names = FALSE)

cat(sprintf("\nmax biomass dev  %.5f -> %.5f\n", max_dev(P0), max_dev(P1)))
cat(sprintf("max age dev      %.5f -> %.5f  (matched groups, vs %s)\n",
            age_dev(P0), age_dev(P1), basename(ANCHOR)))
if (length(MG_EXCLUDE)) {
  j <- match(MG_EXCLUDE, SPN)
  cat("EXCLUDED from matchGrowth, their drift left in place:\n")
  for (k in seq_along(j))
    cat(sprintf("  %-24s age %.2f -> %.2f (target %.2f, %+.1f%%) | biomass ratio %.4f\n",
                MG_EXCLUDE[k], age_of(P0)[j[k]], age_of(P1)[j[k]], AGE_TGT[j[k]],
                100 * (age_of(P1)[j[k]] / AGE_TGT[j[k]] - 1), bio_ratio(P1)[j[k]]))
}
bex <- max(abs(bio_ratio(P1)[!MG_SEL] - 1))
cat(sprintf("max biomass dev over MATCHED groups only: %.5f\n",
            max(abs(bio_ratio(P1)[MG_SEL] - 1))))
cat(sprintf("max erepro       %.4f -> %.4f (%d >= 1)\n",
            max(before_er), max(after_er), sum(after_er >= 1)))
cat(sprintf("max repro level  %.4f -> %.4f (%d > %.2f)\n",
            max(before_rl), max(after_rl), sum(after_rl > CAP + 1e-9), CAP))
cat("rounds at target: converged", R$n_conv, "| converged+admissible", R$n_adm,
    "of", TARGET_ROUNDS, "\n")
cat("ext_encounter intact: ",
    isTRUE(all.equal(as.vector(P1@ext_encounter), as.vector(EE0))), "\n", sep = "")

saveRDS(P1, OUT)
cat("\nWROTE ", OUT, "\n", sep = "")
cat(sprintf("elapsed %.1f min\n", (proc.time() - t0)[["elapsed"]] / 60))