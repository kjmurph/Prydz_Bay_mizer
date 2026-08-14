# =============================================================================
# PHASE 1 -- recalibrate the base params to the 2001-2010 observed biomass AFTER
# correcting the small divers w_min.
#
# WHY THIS EXISTS. Every previous corrected object (30_stageC, 39, 40) recalibrated
# penguin `erepro` against a RECRUITMENT-RESTORATION target -- restore the buggy
# model's RDD at reproduction level 0.50. None of them ever re-fitted the model to
# `biomass_observed`, which is the target the original calibration used
# (06_steady_state_therMizer.Rmd:158-165, 238-299; the 2001:2010 window is stated
# at 06:2854-2855). This script closes that gap.
#
# THE BASIS MATTERS. matchBiomasses() compares getBiomass(params, use_cutoff =
# TRUE), NOT total biomass, and every group carries a biomass_cutoff. For
# `small divers` the cutoff is 4500 g -- the same value whose collision with w_mat
# (4266.667) caused mizer to clamp w_min to 0.001 g in the first place. Measured
# on the stored base:
#
#   cutoff basis : all 19 groups within 1.72% of target  <- the calibration is fine
#   total basis  : small divers 1.1713                   <- 15.1% of the group's
#                                                           biomass sat BELOW the
#                                                           cutoff. That is the bug.
#
# The cutoff STAYS at 4500 g. It targets adult biomass, which is what the
# McCormack et al. 2020 estimate is; moving it would be a second, separate change
# to the calibration target.
#
# matchBiomasses SEMANTICS IN MIZER 3.1.0 -- read the installed source, not the
# repo's comment. 03_model_setup_pre_therMizer.rmd:953 says "set rmax to inf and
# adjust erepro"; that describes mizer 2.x and is now WRONG. In 3.1.0 it scales
# initial_n by observed/model and calls setBevertonHolt(params) with no arguments,
# which takes the `erepro` branch: erepro is HELD and R_max is re-derived -- the
# same contract as steady(preserve = "erepro"). erepro moves only when forced
# upward (rdi_new < rdd_new), and that path warns.
#
# PAIRED, ALWAYS. The control arm is the stored params through the IDENTICAL
# ladder with w_min untouched. Only treated - control is attributable to the
# correction; control - stored is what recalibrating costs on its own. See
# docs/small_divers_wmin_stageD2_results.md.
#
# Writes  params_sel_adj_wmin_corrected_biocal.rds        (treated)
#         Output_large_files/wmin_test/42_control_biocal.rds
#         Output_large_files/wmin_test/42_biomass_ratios.csv
#         Output_large_files/wmin_test/42_ladder_trace.csv
# Overwrites nothing that already exists.
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(dplyr)
})
source("R/check_size_params.R")

out_dir <- "Output_large_files/wmin_test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

PEN         <- "small divers"
W_NEW       <- 3626.667      # 0.85 * w_mat; FIXED, not open for reconsideration
REPRO_LEVEL <- 0.50          # only sets the penguin's STARTING erepro scale
STEADY_TMAX <- 1000          # 09_Uncertainty_Analysis.Rmd:1444
STEADY_PRESERVE <- c("erepro")
TOL_LADDER  <- c(0.1, 0.05, 0.01, 0.002)
STEADY_TOL  <- 0.002         # the target tolerance; 09_Uncertainty_Analysis.Rmd:1444
MATCH_ROUNDS <- 6            # (matchBiomasses -> steady) repeats per approach rung
TARGET_ROUNDS <- 14          # more rounds at the target tolerance -- see below
ACCEPT_DEV   <- 0.0172       # the stored base's own worst deviation

# WHY "BEST OF N AT THE TARGET TOLERANCE" AND NOT "LAST".
# matchBiomasses lands on the targets to machine precision; steady() then pulls
# the model back off them. The two objectives genuinely conflict, and TIGHTENING
# the steady tolerance makes the biomass fit WORSE, because a tighter tolerance
# lets steady() run further before it stops. Measured on the first attempt, the
# max deviation per round at tol = 0.002 was
#     control  0.227 0.028 0.022 0.043 0.015 0.015
#     treated  0.124 0.028 0.052 0.016 0.017 0.017
# i.e. it oscillates rather than converging monotonically, and a trailing extra
# steady() on an already-converged state is a pure loss (it took the control from
# 0.0146 to 0.0279). So: iterate at the target tolerance and keep the BEST state.
# Every candidate is a matchBiomasses -> steady(tol = 0.002) product that
# converged and passed all invariants, which is exactly what the stored base is;
# selecting among them on the calibration objective is what a calibration does.
# The full per-round trace is written to 42_ladder_trace.csv so this is auditable.

t0 <- proc.time()
cat("=== Phase 1: recalibrate base params to 2001-2010 observed biomass ===\n")
cat("started", format(Sys.time()), "| mizer", as.character(packageVersion("mizer")),
    "| therMizer", as.character(packageVersion("therMizer")), "\n\n")

# --- load and upgrade --------------------------------------------------------
stored <- readRDS("Manuscript data/params_sel_adj.rds")
stored <- suppressWarnings(validParams(stored))
# The one numerical difference mizer 3.1.0 could introduce. NEWS.md: the
# first-order path is byte-identical to previous mizer, and validParams() sets
# exactly that. Assert it rather than trust it.
stopifnot(identical(stored@second_order_w$flux, "upwind"),
          isFALSE(stored@second_order_w$bin_average),
          identical(stored@rates_funcs$Encounter, "therMizerEncounter"),
          identical(stored@rates_funcs$RDD, "BevertonHoltRDD"))

i_pen <- which(stored@species_params$species == PEN)
stopifnot(length(i_pen) == 1)
sp0 <- stored@species_params
cat("stored penguin: w_min =", sp0$w_min[i_pen],
    "| biomass_cutoff =", sp0$biomass_cutoff[i_pen],
    "| erepro =", signif(sp0$erepro[i_pen], 5), "\n")
cat("all 19 have a biomass_observed target:",
    all(is.finite(sp0$biomass_observed) & sp0$biomass_observed > 0), "\n\n")

# --- helpers -----------------------------------------------------------------

# Reuse 30_stageC_corrected_params.R:74-86 verbatim. setParams() rebuilds
# w_min_idx but RESETS rates_funcs (dropping therMizer) and does not re-zero
# initial_n below the new w_min. Splice back only the three therMizer entries --
# restoring the whole 2.5.0 list would drop mizer 3.x's Diffusion entry.
set_wmin <- function(p, species, w_new) {
  rf <- p@rates_funcs; rd <- p@resource_dynamics
  i <- which(p@species_params$species == species)
  sp <- p@species_params; sp$w_min[i] <- w_new; p@species_params <- sp
  p <- suppressWarnings(setParams(p))
  ther <- intersect(c("Encounter", "PredRate", "EReproAndGrowth"), names(rf))
  p@rates_funcs[ther] <- rf[ther]
  p@resource_dynamics <- rd
  p@initial_n[i, p@w < w_new] <- 0
  stopifnot(p@w_min_idx[[i]] == max(which(p@w <= w_new)),
            identical(p@rates_funcs$Encounter, "therMizerEncounter"))
  p
}

rates_at_init <- function(p) {
  getRates(p, n = p@initial_n, n_pp = p@initial_n_pp,
           n_other = p@initial_n_other, effort = 0, t = 1841)
}

bio_ratio <- function(p) {
  as.numeric(getBiomass(p, use_cutoff = TRUE) / p@species_params$biomass_observed)
}
bio_ratio_full <- function(p) {
  as.numeric(getBiomass(p, use_cutoff = FALSE) / p@species_params$biomass_observed)
}
max_dev <- function(p) max(abs(bio_ratio(p) - 1))

# THE NON-CONVERGENCE GUARD. Verified against the installed mizer 3.1.0:
# getS3method("projectToSteady", "MizerParams") signals non-convergence with
#   message("Simulation run did not converge after ", ...)
# The only warning() in that function is the extinction notice. Every handler in
# this repo (30:100-104, 39:152-160, 40:192-198) listens for `warning` and so has
# NEVER detected a non-converged run. Listen for both.
steady_guarded <- function(p, tol, label) {
  no_conv <- FALSE; extinct <- character(0)
  out <- withCallingHandlers(
    try(steady(p, tol = tol, t_max = STEADY_TMAX, preserve = STEADY_PRESERVE,
               progress_bar = FALSE), silent = TRUE),
    message = function(m) {
      if (grepl("did not converge", conditionMessage(m), ignore.case = TRUE))
        no_conv <<- TRUE
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      if (grepl("extinct", conditionMessage(w), ignore.case = TRUE))
        extinct <<- c(extinct, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  if (inherits(out, "try-error"))
    stop("steady() errored for ", label, " at tol = ", tol, ": ", as.character(out))
  list(params = out, converged = !no_conv, extinct = extinct)
}

# setBevertonHolt() only WARNS when it is forced to raise erepro, or when erepro
# lands above 1. Capture both; the hard invariant (erepro < 1) is asserted
# separately after every rung, because that is what actually matters.
match_guarded <- function(p, label) {
  bumped <- character(0)
  out <- withCallingHandlers(
    try(matchBiomasses(p), silent = TRUE),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("smallest possible value|unrealistic value greater", msg))
        bumped <<- c(bumped, trimws(gsub("\\s+", " ", msg)))
      invokeRestart("muffleWarning")
    })
  if (inherits(out, "try-error"))
    stop("matchBiomasses() errored for ", label, ": ", as.character(out))
  list(params = out, bumped = bumped)
}

assert_invariants <- function(p, label, rung) {
  sp <- p@species_params
  bad_e <- sp$species[!(sp$erepro < 1)]
  if (length(bad_e))
    stop(label, " rung ", rung, ": erepro >= 1 for ", paste(bad_e, collapse = ", "),
         " -- abort. orca starts at 0.9509 with only 5% headroom.")
  chk <- check_size_params(p, quiet = TRUE)
  if (!all(chk$ordering_ok))
    stop(label, " rung ", rung, ": w_min < w_mat < w_max violated for ",
         paste(chk$species[!chk$ordering_ok], collapse = ", "))
  if (!identical(p@rates_funcs$Encounter, "therMizerEncounter"))
    stop(label, " rung ", rung, ": therMizer Encounter override was lost")
  invisible(TRUE)
}

# --- the ladder --------------------------------------------------------------
# 06_steady_state_therMizer.Rmd:158-165 alternates steady() and matchBiomasses()
# and ENDS on steady(). That is why the stored base sits at 1.72% rather than
# exactly 1.000 -- the final steady() moves off the matched state. Reproducing the
# protocol means reproducing that, so the acceptance bar is the stored base's own
# worst deviation, not zero.
trace_rows <- list()
recalibrate <- function(p, label) {
  cat("--- ", label, " ---\n", sep = "")
  cat(sprintf("   start: max dev %.4f (penguin cutoff ratio %.4f)\n",
              max_dev(p), bio_ratio(p)[i_pen]))
  # Leading steady(), as at 06:158. Then alternate match -> steady, ending on
  # steady. The residual MUST be read after steady(): matchBiomasses lands on the
  # targets to machine precision (measured: 2.2e-16), so a check taken after it
  # would always read zero and break the loop on the first round. The quantity
  # that actually converges is how far steady() pulls the model back off target.
  st <- steady_guarded(p, TOL_LADDER[1], label); p <- st$params
  best <- NULL; best_d <- Inf
  for (tol in TOL_LADDER) {
    at_target <- isTRUE(all.equal(tol, STEADY_TOL))
    for (r in seq_len(if (at_target) TARGET_ROUNDS else MATCH_ROUNDS)) {
      mt <- match_guarded(p, label);       p <- mt$params
      st <- steady_guarded(p, tol, label); p <- st$params
      assert_invariants(p, label, sprintf("tol=%g r=%d", tol, r))
      d <- max_dev(p)
      # Only states converged at the TARGET tolerance are eligible, so the
      # returned object is on the same footing as the stored base.
      if (at_target && st$converged && d < best_d) { best_d <- d; best <- p }
      trace_rows[[length(trace_rows) + 1]] <<- data.frame(
        arm = label, tol = tol, round = r, max_dev = d,
        pen_ratio_cut = bio_ratio(p)[i_pen],
        pen_ratio_full = bio_ratio_full(p)[i_pen],
        max_erepro = max(p@species_params$erepro),
        n_rmax_inf = sum(!is.finite(p@species_params$R_max)),
        converged = st$converged,
        bumped = if (length(mt$bumped)) paste(mt$bumped, collapse = " ") else "",
        stringsAsFactors = FALSE)
      cat(sprintf("   tol=%-6.3g r=%-2d max dev %.5f  pen %.4f  max erepro %.4f  %s%s%s\n",
                  tol, r, d, bio_ratio(p)[i_pen], max(p@species_params$erepro),
                  if (st$converged) "conv" else "NO-CONV",
                  if (length(mt$bumped)) "  [erepro bumped]" else "",
                  if (at_target && identical(best, p)) "  <- best" else ""))
    }
  }
  if (is.null(best))
    stop(label, ": no state converged at the target tolerance ", STEADY_TOL)
  cat(sprintf("   selected: max dev %.5f  pen cutoff %.4f  pen total %.4f\n\n",
              best_d, bio_ratio(best)[i_pen], bio_ratio_full(best)[i_pen]))
  best
}

# --- CONTROL: stored params, w_min untouched, identical ladder ---------------
p_ctl <- recalibrate(stored, "control")

# --- TREATED: corrected w_min, penguin reproduction rescaled, same ladder ----
# The explicit erepro floor is needed BEFORE the ladder. RDI scales as
# erepro / w_min, so the stored penguin erepro of 3.1975e-04 -- calibrated against
# w_min = 0.001 g -- is ~400x too small at 3626.667 g. Without this step the group
# starts effectively extinct and matchBiomasses has to recover it from nothing.
p_trt <- set_wmin(stored, PEN, W_NEW)
r_ctl0 <- rates_at_init(stored)
r_trt0 <- rates_at_init(p_trt)
rdd_target   <- r_ctl0$rdd[i_pen]
erepro_floor <- stored@species_params$erepro[i_pen] * rdd_target / r_trt0$rdi[i_pen]
erepro_new   <- erepro_floor / (1 - REPRO_LEVEL)
cat(sprintf("penguin erepro floor %.5g -> start %.5g (reproduction level %.2f)\n",
            erepro_floor, erepro_new, REPRO_LEVEL))
stopifnot(erepro_new < 1)

rdi_target <- r_trt0$rdi[i_pen] * erepro_new / p_trt@species_params$erepro[i_pen]
R_new <- if (rdi_target > rdd_target) 1 / (1 / rdd_target - 1 / rdi_target) else Inf
stopifnot(is.finite(R_new))
p_trt@species_params$erepro[i_pen] <- erepro_new
p_trt@species_params$R_max[i_pen]  <- R_new
stopifnot(isTRUE(all.equal(unname(rates_at_init(p_trt)$rdi[i_pen]),
                           unname(rdi_target), tolerance = 1e-8)))
cat(sprintf("penguin R_max %.5g -> %.5g\n\n",
            stored@species_params$R_max[i_pen], R_new))

p_trt <- recalibrate(p_trt, "treated")

# --- report ------------------------------------------------------------------
sp_t <- p_trt@species_params
cmp <- data.frame(
  species        = sp_t$species,
  biomass_cutoff = sp_t$biomass_cutoff,
  obs            = sp_t$biomass_observed,
  ratio_cut_stored  = bio_ratio(stored),
  ratio_cut_control = bio_ratio(p_ctl),
  ratio_cut_treated = bio_ratio(p_trt),
  ratio_full_stored  = bio_ratio_full(stored),
  ratio_full_control = bio_ratio_full(p_ctl),
  ratio_full_treated = bio_ratio_full(p_trt),
  erepro_stored  = stored@species_params$erepro,
  erepro_control = p_ctl@species_params$erepro,
  erepro_treated = sp_t$erepro,
  R_max_control  = p_ctl@species_params$R_max,
  R_max_treated  = sp_t$R_max,
  stringsAsFactors = FALSE)

cat("=== modelled/observed biomass, cutoff basis (what matchBiomasses fits) ===\n")
print(as.data.frame(cmp %>% select(species, obs, ratio_cut_stored,
                                   ratio_cut_control, ratio_cut_treated)),
      digits = 5, row.names = FALSE)

cat("\n=== hard constraints ===\n")
for (nm in c("control", "treated")) {
  p <- if (nm == "control") p_ctl else p_trt
  sp <- p@species_params
  cat(sprintf("  %-8s erepro < 1: %s (max %.4f, %s) | R_max finite: %s | max dev %.5f\n",
              nm, all(sp$erepro < 1), max(sp$erepro),
              sp$species[which.max(sp$erepro)],
              all(is.finite(sp$R_max)), max_dev(p)))
}

dev_ctl <- max_dev(p_ctl); dev_trt <- max_dev(p_trt)
cat(sprintf("\n  acceptance (<= %.4f, the stored base's own worst):\n", ACCEPT_DEV))
cat(sprintf("    control %.5f  %s\n", dev_ctl, if (dev_ctl <= ACCEPT_DEV) "PASS" else "FAIL"))
cat(sprintf("    treated %.5f  %s\n", dev_trt, if (dev_trt <= ACCEPT_DEV) "PASS" else "FAIL"))

cat("\n=== penguin: how much biomass still sits below the 4500 g cutoff ===\n")
below <- function(p) 100 * (1 - bio_ratio(p)[i_pen] / bio_ratio_full(p)[i_pen])
cat(sprintf("  stored  %.1f%%\n  control %.1f%%\n  treated %.1f%%   <- the correction shrinks this\n",
            below(stored), below(p_ctl), below(p_trt)))

write.csv(cmp, file.path(out_dir, "42_biomass_ratios.csv"), row.names = FALSE)
write.csv(bind_rows(trace_rows), file.path(out_dir, "42_ladder_trace.csv"),
          row.names = FALSE)
saveRDS(p_trt, "params_sel_adj_wmin_corrected_biocal.rds")
saveRDS(p_ctl, file.path(out_dir, "42_control_biocal.rds"))

cat("\nWrote params_sel_adj_wmin_corrected_biocal.rds and 42_control_biocal.rds\n")
cat("      42_biomass_ratios.csv, 42_ladder_trace.csv\n")
cat("elapsed:", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")

if (dev_ctl > ACCEPT_DEV || dev_trt > ACCEPT_DEV)
  stop("acceptance FAILED -- do not proceed to Phase 2 until this is resolved")