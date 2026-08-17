# =============================================================================
# Phase 89 -- catchability re-fit, ISIMIP3a-compliant fitting window
#
# Phase 45 with two corrections and a new state source. The method is unchanged:
# divide catchability by the modelled/observed yield ratio, re-project, iterate,
# one GLOBAL multiplier M_s per species applied to every member's own drawn q.
#
# ---------------------------------------------------------- CORRECTION 1: 2004
# The ISIMIP3a protocol: "Modellers are permitted to calibrate or tune their
# models using historical fisheries catch data ... on the condition that ONLY
# YEARS UP TO AND INCLUDING 2004 are used in model calibration/tuning." Years
# after 2004 are the EVALUATION window and must not enter the fit.
#
# Phase 45 took its window from the EFFORT series (first to last year with
# effort > 0), which runs to 2010, while the FishMIP catch reconstruction --
# calibration_catch_histsoc_1850_2004_regional_models.csv -- ends in 2004. The
# missing years were then filled with zeros (`coalesce(Yield_g, 0)`), so absence
# of data entered the objective as an observation of no catch.
#
# The damage was concentrated and severe: 79% of ALL toothfish effort falls in
# 2005-2010, including its effort maximum of 1.000 in 2008, where catch is
# recorded as zero. With the +1 g offset pricing a zero at up to 11.7 log units,
# the refit drove toothfish q DOWN 65x to suppress predictions in six
# non-existent observations, leaving modelled catch at 0.031 of observed across
# the 33 real years. Toothfishes carried 26.4% of the pooled objective on 11.6%
# of the observations.
#
# ------------------------------------------------- CORRECTION 2: zeros vs NAs
# The window now ends at min(2004, last year with REPORTED catch) per species.
# That also catches squids, whose catch record ends in 1993 while its effort
# continues -- four zero-catch-positive-effort years INSIDE the pre-2004 window,
# the same ambiguity as toothfishes. Missing catch is NA and is DROPPED, never
# coerced to zero. Genuine zeros before a fishery began are already excluded
# because the window still starts at the first year with effort.
#
# mizer never sees these NAs: `yield_observed` in the params is all zero/NA and
# unused, so the observed series exists only inside this objective.
#
# ------------------------------------------- CORRECTION 3: the estimator
# The first draft of this script also, unintentionally, changed the ESTIMATOR:
# it updated the multiplier from the ratio of POOLED SUMS across members, where
# phase 45 used the MEDIAN of per-member ratios with a 50x per-iteration step
# cap. That is not a neutral difference. A pooled sum is dominated by the
# largest members, and this project has already been bitten by exactly that
# construction -- one divergent member once owned 83% of an across-member sum.
#
# MEASURED, phase 90, 10 members: the pooled estimator lands EVERY species below
# its target (krill 0.26, toothfishes 0.29, bathypelagic 0.35, squids 0.44,
# shelf 0.52) where the median estimator lands five of them on 1.000. It drives
# the pooled total to match while leaving the median member systematically
# short. Phase 45's rule is restored as the default; P89_ESTIMATOR=pooled
# reproduces the first draft.
#
# --------------------------------- CORRECTION 4: whale catchability is HELD
# baleen, sperm and minke whales are NOT fitted. This is not new -- it is
# recorded in 45_catchability_multipliers.rds, in fields the script does not
# write ($held_at_one and $note), and the first draft of this script silently
# dropped it. Phase 45's own note:
#
#   "baleen/sperm/minke held at 1: their fit is non-identifiable (multiplier
#    diverges to 1e6 while the catch ratio stays flat) and applying it would pin
#    every member at the q ceiling, destroying the sampled spread in whale
#    catchability."
#
# Its trace shows the divergence directly -- baleen 1 -> 40.8 -> 673 -> 1.10e4
# -> 1.81e5 -> 2.96e6 while the catch ratio never leaves 0.061. Phase 90
# reproduced it (baleen 9.07e5, minke 5.43e5) and measured the damage: fitting
# whales cranks q to the ceiling, fishes the stock down harder through the
# whaling era, and collapses the 2005-2010 out-of-sample baleen catch ratio from
# 0.705 to 0.012. The stock is the binding limit, not catchability: observed
# baleen catch is ~23.8x the calibrated standing stock.
#
# ------------------------- CORRECTION 5: FIT ONLY THE MEMBERS THAT PASSED
# The first run of this script fitted every state file in the directory. Phase 88
# writes a state for every member that converges on the tolerance ladder; the
# stability and erepro screens are recorded in 88_full.rds but do NOT gate the
# state files. So the fit ran on all 1,668 members, of which only 427 are USABLE
# (stable AND no erepro >= 1) -- 1,241 rejected members set the multipliers.
#
# WHY THIS WAS HARMLESS BEFORE. Phase 45 also read its whole state directory, and
# ensemble 44 was 99.2% stable (1,654 of 1,668), so the screen would have moved
# 14 members. Phase 88 is 31.2% stable. The convention is inherited from a
# context where it could not matter, into one where it dominates.
#
# MEASURED, on the unfiltered run (89_refit_results_unfiltered_n1668.rds), median
# modelled/observed catch at the fitted multipliers:
#
#   species             all 1,668   usable 427   rejected 1,241
#   antarctic krill        0.204        43.3          0.00022
#   minke whales           0.0109       0.0251        0.00437
#   toothfishes            1.00         1.46          0.867
#
# Krill is the failure. Rejected members produce almost no krill catch, dragging
# the median to near zero and driving the multiplier to M = 2.615e5, at which
# 100% of members have krill q pinned at the QMAX = 1 ceiling -- from drawn
# values spanning 0.002-0.822. That is exactly the harm CORRECTION 4 gives as the
# reason whales are held: it destroys the sampled spread in catchability. On the
# members that count krill catch is OVER-predicted 43x (q10 = 26.5; 99.5% of
# usable members above 1), so the ceiling was never the binding limit and neither
# was the stock. Phase 92's premise rests on the unfiltered 0.204 and does not
# survive this.
#
# P89_SCREEN selects the gate: "usable" (default, phase 88's own definition),
# "stable", "admissible", or "none" to reproduce the unfiltered first run.
#
# ------------------------------------------------------------- STATE SOURCE
# P89_STATE_DIR, defaulting to the phase-88 states on the CURRENT reference. The
# phase-45 multipliers were fitted against ensemble-44 states built on a base
# whose stock has since moved substantially (growth recalibration, interaction
# matrix, minke PPMR, reproduction targets). Its own header warned the original
# catchability was "stale ... with a biomass-calibrated stock the same
# catchability lands a small fraction of the observed catch"; that argument now
# applies to phase 45 itself.
#
# WHY RE-PROJECTING IS EXACT: initial_effort is 0, so steady() and the spin-up
# are UNFISHED and catchability cannot touch the initial condition. It enters
# only the 1841-2010 projection.
#
# USAGE  Rscript R/wmin_test/89_catchability_refit_2004.R [run|collect]
# ENV    P89_STATE_DIR, P89_OUT, P89_CORES, P89_ITERS, P89_CAP_YEAR, P89_QMAX,
#        P89_SCREEN, P89_MEMBERS_RDS
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OL <- "Output_large_files/wmin_test"
STATE_DIR <- Sys.getenv("P89_STATE_DIR",
                        file.path(OL, "88_full_states"))
OUT <- Sys.getenv("P89_OUT", file.path(OL, "89_refit_results.rds"))
CORES <- min(as.integer(Sys.getenv("P89_CORES", "30")),
             max(1L, parallel::detectCores() - 2L))
ITERS <- as.integer(Sys.getenv("P89_ITERS", "6"))
CAP_YEAR <- as.integer(Sys.getenv("P89_CAP_YEAR", "2004"))
QMAX <- as.numeric(Sys.getenv("P89_QMAX", "1"))
# CORRECTION 3. "median" is phase 45's rule and the default; "pooled" reproduces
# this script's first draft, which was measured to undershoot every species.
ESTIMATOR <- Sys.getenv("P89_ESTIMATOR", "median")
stopifnot(ESTIMATOR %in% c("median", "pooled"))
# Phase 45's per-iteration change cap. Keeps the first step sane when the
# starting ratio is extreme (bathypelagic fishes opens at 3.85e6).
M_STEP_CAP <- as.numeric(Sys.getenv("P89_STEP_CAP", "50"))
# CORRECTION 4. Species whose catchability is NOT fitted; their drawn q stands.
# Set P89_HOLD="" to fit everything and reproduce the divergence.
HOLD <- trimws(strsplit(Sys.getenv("P89_HOLD",
  "baleen whales,sperm whales,minke whales"), ",")[[1]])
HOLD <- HOLD[nzchar(HOLD)]
# Optional member subset, for testing the method without a full ensemble.
MEMBER_LIST <- Sys.getenv("P89_MEMBERS", "")
# CORRECTION 5. The screen, and the table it is read from. The default table is
# the sibling of the state directory: <stem>_states -> <stem>.rds.
SCREEN <- Sys.getenv("P89_SCREEN", "usable")
stopifnot(SCREEN %in% c("usable", "stable", "admissible", "none"))
MEMBER_TABLE <- Sys.getenv("P89_MEMBERS_RDS", sub("_states/?$", ".rds", STATE_DIR))
if (!dir.exists(STATE_DIR)) stop("no state dir: ", STATE_DIR, call. = FALSE)

effort_arr <- readRDS("effort_array_1841_2010.rds")
eff_yrs <- as.numeric(rownames(effort_arr))

# --- the fitting window, per species ------------------------------------------
obs_w <- read.csv("yield_observed_timeseries.csv", check.names = FALSE)
ocols <- setdiff(names(obs_w), "Year")
SPN_EFF <- colnames(effort_arr)
win <- do.call(rbind, lapply(SPN_EFF, function(s) {
  cn <- ocols[match(make.names(s), make.names(ocols))]
  if (is.na(cn)) return(NULL)
  ey <- eff_yrs[effort_arr[, s] > 0]
  if (!length(ey)) return(NULL)
  oc <- obs_w[[cn]]; oy <- obs_w$Year
  reported <- oy[is.finite(oc) & oc > 0]
  if (!length(reported)) return(NULL)
  data.frame(Species = s, first_year = min(ey),
             last_year = min(CAP_YEAR, max(reported), max(ey)),
             eff_last = max(ey), reported_last = max(reported),
             stringsAsFactors = FALSE)
}))
win$dropped_years <- win$eff_last - win$last_year

cat("=== Phase 89: catchability re-fit, window capped at", CAP_YEAR, "===\n")
cat("states:", STATE_DIR, "\n\n")
print(win, row.names = FALSE)

obs_long <- obs_w %>%
  tidyr::pivot_longer(-Year, names_to = "col", values_to = "Yield_g") %>%
  mutate(Species = SPN_EFF[match(make.names(col), make.names(SPN_EFF))]) %>%
  filter(!is.na(Species)) %>%
  left_join(win, by = "Species") %>%
  filter(!is.na(first_year), Year >= first_year, Year <= last_year) %>%
  # MISSING CATCH IS DROPPED, NOT ZEROED. Genuine pre-fishery zeros are already
  # outside the window because it starts at the first year with effort.
  filter(is.finite(Yield_g)) %>%
  transmute(Year, Species, Yield_obs = pmax(Yield_g, 0))

FIT_SP <- obs_long %>% group_by(Species) %>%
  summarise(o = sum(Yield_obs), .groups = "drop") %>% filter(o > 0) %>%
  pull(Species)
cat("\nfitting", length(FIT_SP), "species |", nrow(obs_long), "observations\n")

states <- sort(list.files(STATE_DIR, pattern = "^state_\\d+\\.rds$",
                          full.names = TRUE))
if (!length(states)) stop("no states in ", STATE_DIR, call. = FALSE)
si_of <- function(f) as.integer(sub("^state_0*", "", sub("\\.rds$", "",
                                                         basename(f))))

# --- CORRECTION 5: the member screen ------------------------------------------
# A state file exists for every member that converged on the tolerance ladder,
# INCLUDING those that then failed the stability screen or carry erepro >= 1.
# Fitting on those lets rejected members set the multipliers; on the phase-88
# states that drove krill q to the QMAX ceiling in every member. Refusing to
# fall back to "all states" is deliberate: a silent fallback is how the first
# run produced a plausible-looking answer from 1,241 rejected members.
if (SCREEN == "none") {
  cat("\nSCREEN = none: fitting ALL", length(states),
      "states, rejected members included.\n")
  cat("  This reproduces the first run. It is not the default for a reason --\n")
  cat("  see CORRECTION 5 in the header.\n")
} else {
  if (!file.exists(MEMBER_TABLE))
    stop("no member table: ", MEMBER_TABLE,
         "\n  P89_SCREEN='", SCREEN, "' needs the phase-88 summary to know which",
         "\n  members passed. Set P89_MEMBERS_RDS, or P89_SCREEN=none to fit all.",
         call. = FALSE)
  MT <- readRDS(MEMBER_TABLE)
  MT <- if (is.data.frame(MT)) MT else MT$members
  need <- c("sim_index", "stable", "n_erepro_ge1")
  if (!is.data.frame(MT) || length(setdiff(need, names(MT))))
    stop("the member table ", basename(MEMBER_TABLE), " has no $members frame ",
         "with columns ", paste(need, collapse = ", "), call. = FALSE)
  # THE SCREEN FOLLOWS THE TABLE'S OWN DEFINITION. Phase 88 had no drift test, so
  # its `usable` is stable AND admissible. Phase 104 adds a drift screen and its
  # `usable` is stable AND drift_ok AND admissible; fitting the phase-88
  # definition against a phase-104 table would silently include 62 members that
  # the run itself rejected (265 against 203). `drift_ok` is used when the column
  # is present and ignored when it is not, so this is a no-op on phase-88 tables.
  has_drift <- "drift_ok" %in% names(MT)
  keep <- switch(SCREEN,
    usable     = MT$stable & MT$n_erepro_ge1 == 0 &
                 (if (has_drift) MT$drift_ok else TRUE),
    stable     = MT$stable,
    admissible = MT$n_erepro_ge1 == 0)
  if (SCREEN == "usable")
    cat("  screen 'usable' =", if (has_drift)
      "stable AND drift_ok AND admissible (drift_ok present)" else
      "stable AND admissible (no drift_ok column)", "\n")
  pass <- as.integer(MT$sim_index[keep])
  have <- si_of(states)
  # Every state must be accounted for in the table. If it is not, the table and
  # the state directory are from different runs and the screen is meaningless.
  if (length(setdiff(have, as.integer(MT$sim_index))))
    stop(length(setdiff(have, as.integer(MT$sim_index))), " state file(s) are ",
         "absent from ", basename(MEMBER_TABLE), " (e.g. ",
         setdiff(have, as.integer(MT$sim_index))[1], ") -- the table and the ",
         "states are from different runs. Refusing to screen.", call. = FALSE)
  states <- states[have %in% pass]
  if (!length(states))
    stop("the '", SCREEN, "' screen left no members", call. = FALSE)
  cat(sprintf("\nscreen: %s | table %s\n", SCREEN, basename(MEMBER_TABLE)))
  cat(sprintf("  built %d | stable %d | admissible %d | usable %d\n",
              nrow(MT), sum(MT$stable), sum(MT$n_erepro_ge1 == 0),
              sum(MT$stable & MT$n_erepro_ge1 == 0)))
  cat(sprintf("  FITTING %d of %d states (%d rejected)\n", length(states),
              length(have), length(have) - length(states)))
}

if (nzchar(MEMBER_LIST)) {
  want <- as.integer(trimws(strsplit(MEMBER_LIST, ",")[[1]]))
  sel <- file.path(STATE_DIR, sprintf("state_%05d.rds", want))
  if (!all(file.exists(sel)))
    stop("missing states: ", paste(want[!file.exists(sel)], collapse = ", "),
         call. = FALSE)
  # The explicit list is applied AFTER the screen, never instead of it, so a
  # test subset cannot quietly re-admit rejected members.
  blocked <- want[!sel %in% states]
  if (length(blocked))
    stop("P89_MEMBERS asks for ", length(blocked), " member(s) the '", SCREEN,
         "' screen rejected (e.g. ", blocked[1], "). Use P89_SCREEN=none to ",
         "fit them anyway.", call. = FALSE)
  states <- sel
  cat("member subset:", length(states), "|", paste(want, collapse = ", "), "\n")
}
CORES <- min(CORES, length(states))
cat("members:", length(states), "| cores:", CORES, "| iterations:", ITERS,
    "| estimator:", ESTIMATOR, "\n")
cat("held at the drawn q (not fitted):",
    if (length(HOLD)) paste(HOLD, collapse = ", ") else "none", "\n\n")

project_one <- function(f, M) {
  suppressPackageStartupMessages({library(mizer); library(therMizer)})
  z <- readRDS(f); p <- z$params
  gp <- gear_params(p)
  m <- M[match(gp$species, names(M))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
  gear_params(p) <- gp
  s <- try(project(p, initial_n = z$initial_n, t_start = 1841,
                   effort = effort_arr, progress_bar = FALSE), silent = TRUE)
  if (inherits(s, "try-error")) return(NULL)
  y <- getYield(s); yr <- as.numeric(rownames(y))
  si <- as.integer(sub("^state_0*", "", sub("\\.rds$", "", basename(f))))
  data.frame(sim_index = si,
             Year = rep(yr, times = ncol(y)),
             Species = rep(colnames(y), each = length(yr)),
             Yield_mod = as.vector(y), stringsAsFactors = FALSE)
}

M <- setNames(rep(1, length(FIT_SP)), FIT_SP)
if (length(setdiff(HOLD, FIT_SP)))
  cat("NOTE: held species not in the fitted set (no effect):",
      paste(setdiff(HOLD, FIT_SP), collapse = ", "), "\n")
FIT_ACTIVE <- setdiff(FIT_SP, HOLD)
cat("fitting", length(FIT_ACTIVE), "of", length(FIT_SP), "species:",
    paste(FIT_ACTIVE, collapse = ", "), "\n\n")

# The modelled/observed catch ratio per species, by whichever estimator.
# MEDIAN takes the median ACROSS MEMBERS of each member's own ratio, so a single
# divergent member cannot own the answer. POOLED sums first and is the first
# draft's rule, kept only so it can be reproduced.
ratio_of <- function(J) {
  if (ESTIMATOR == "median") {
    J %>% group_by(sim_index, Species) %>%
      summarise(mod = sum(Yield_mod), obs = sum(Yield_obs), .groups = "drop") %>%
      filter(obs > 0) %>% group_by(Species) %>%
      summarise(r = median(mod / obs, na.rm = TRUE), .groups = "drop")
  } else {
    J %>% group_by(Species) %>%
      summarise(mod = sum(Yield_mod), obs = sum(Yield_obs), .groups = "drop") %>%
      filter(obs > 0) %>% transmute(Species, r = mod / obs)
  }
}

cl <- makeCluster(CORES)
on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
clusterExport(cl, c("effort_arr", "QMAX", "project_one"), envir = environment())
trace <- list()
for (it in seq_len(ITERS)) {
  clusterExport(cl, "M", envir = environment())
  res <- parLapplyLB(cl, states, function(f) project_one(f, M))
  nfail <- sum(vapply(res, is.null, logical(1)))
  Y <- bind_rows(res[!vapply(res, is.null, logical(1))])
  if (!nrow(Y)) stop("every member failed to project", call. = FALSE)
  J <- inner_join(Y, obs_long, by = c("Year", "Species")) %>%
    filter(Species %in% FIT_SP)
  rat <- ratio_of(J)
  trace[[it]] <- rat %>% mutate(iter = it, M_before = as.numeric(M[Species]),
                                held = Species %in% HOLD)
  # Only the ACTIVE species move. Held species keep M = 1, so each member's
  # drawn catchability stands and the sampled spread survives.
  upd <- rat %>% filter(Species %in% FIT_ACTIVE)
  if (nrow(upd)) {
    step <- pmin(pmax(1 / pmax(upd$r, 1e-12), 1 / M_STEP_CAP), M_STEP_CAP)
    M[upd$Species] <- M[upd$Species] * step
  }
  cat(sprintf("iter %d | median |log10 ratio| over fitted species %.4f%s\n", it,
              median(abs(log10(pmax(upd$r, 1e-12)))),
              if (nfail) sprintf(" | %d member(s) failed", nfail) else ""))
  flush.console()
}
stopCluster(cl)
TRACE <- bind_rows(trace)
stopifnot(all(M[HOLD[HOLD %in% names(M)]] == 1))

# --- final pass: per-member, per-species SSE on log10(y + 1 g) ----------------
cl <- makeCluster(CORES)
clusterExport(cl, c("effort_arr", "QMAX", "project_one", "M"),
              envir = environment())
res <- parLapplyLB(cl, states, function(f) project_one(f, M))
stopCluster(cl)
Y <- bind_rows(res[!vapply(res, is.null, logical(1))])
J <- inner_join(Y, obs_long, by = c("Year", "Species"))
per_species <- J %>% group_by(sim_index, Species) %>%
  summarise(n = n(),
            sse = sum((log10(Yield_mod + 1) - log10(Yield_obs + 1))^2),
            obs_tot = sum(Yield_obs), mod_tot = sum(Yield_mod),
            .groups = "drop")
summary_tbl <- per_species %>% group_by(sim_index) %>%
  summarise(rmse = sqrt(sum(sse) / sum(n)), .groups = "drop") %>% arrange(rmse)

cat("\n--- multipliers ---\n")
print(data.frame(species = names(M), M = signif(as.numeric(M), 4),
                 held = names(M) %in% HOLD), row.names = FALSE)
cat("\n--- final modelled/observed catch ratio (", ESTIMATOR, ") ---\n", sep = "")
print(as.data.frame(TRACE %>% filter(iter == max(iter)) %>%
  transmute(Species, ratio = signif(r, 4), held) %>% arrange(desc(ratio))),
  row.names = FALSE)
# --- the QMAX ceiling ---------------------------------------------------------
# A multiplier that pushes q past QMAX is CLAMPED, so the fit saturates: M keeps
# rising, the catch ratio stops responding, and every member ends at the same q
# -- the sampled spread in catchability is gone. This is the failure CORRECTION 4
# records for the whales and CORRECTION 5 measured for krill, where it went
# unnoticed. Report it rather than let the next one hide in a plausible number.
Q0 <- do.call(rbind, lapply(states, function(f) {
  gp <- gear_params(readRDS(f)$params)
  data.frame(sim_index = si_of(f), Species = as.character(gp$species),
             q = as.numeric(gp$catchability), stringsAsFactors = FALSE)
}))
CEIL <- Q0 %>% filter(Species %in% names(M)) %>%
  mutate(q_fit = pmin(QMAX, pmax(0, q * as.numeric(M[Species])))) %>%
  group_by(Species) %>%
  summarise(q_drawn_med = signif(median(q), 3),
            q_fitted_med = signif(median(q_fit), 3),
            pct_at_ceiling = round(100 * mean(q_fit >= QMAX * (1 - 1e-12)), 1),
            .groups = "drop") %>%
  mutate(M = signif(as.numeric(M[Species]), 4),
         held = Species %in% HOLD) %>%
  arrange(desc(pct_at_ceiling))
cat("\n--- catchability against the QMAX =", QMAX, "ceiling ---\n")
print(as.data.frame(CEIL), row.names = FALSE)
sat <- CEIL %>% filter(!held, pct_at_ceiling > 50)
if (nrow(sat))
  cat("\n  WARNING: ", nrow(sat), " fitted species have >50% of members AT the ",
      "ceiling (", paste(sat$Species, collapse = ", "), ").\n",
      "  The fit is saturated there: the multiplier is not identifiable and the\n",
      "  sampled spread in catchability has been destroyed. See CORRECTION 5.\n",
      sep = "")

cat("\n--- objective share by species ---\n")
print(as.data.frame(per_species %>% group_by(Species) %>%
  summarise(pct_sse = round(100*sum(sse)/sum(per_species$sse), 1),
            pct_n = round(100*sum(n)/sum(per_species$n), 1), .groups = "drop") %>%
  arrange(desc(pct_sse))), row.names = FALSE)

saveRDS(list(summary = summary_tbl, per_species = per_species, M = M,
             window = win, obs_used = obs_long, trace = TRACE,
             held_at_one = HOLD, ceiling = CEIL,
             screen = list(rule = SCREEN, table = MEMBER_TABLE,
                           members = si_of(states)),
             note = paste0(
               "Multipliers fitted at q<=", QMAX, " by the 08:449-511 ratio ",
               "method, on the ISIMIP3a-compliant window ending at min(",
               CAP_YEAR, ", last reported catch) per species, with missing ",
               "catch dropped rather than zeroed. Estimator: ", ESTIMATOR,
               " with a ", M_STEP_CAP, "x per-iteration step cap. Member ",
               "screen: ", SCREEN, " (", length(states), " members fitted). ",
               "Held at 1: ",
               paste(HOLD, collapse = ", "),
               " -- their fit is non-identifiable (multiplier diverges to 1e6 ",
               "while the catch ratio stays flat) and applying it would pin ",
               "every member at the q ceiling, destroying the sampled spread ",
               "in whale catchability."),
             meta = list(state_dir = STATE_DIR, cap_year = CAP_YEAR,
                         qmax = QMAX, iters = ITERS, estimator = ESTIMATOR,
                         step_cap = M_STEP_CAP, n_members = length(states),
                         screen = SCREEN, member_table = MEMBER_TABLE,
                         built = Sys.time())), OUT)
cat("\nWROTE", OUT, "\n")