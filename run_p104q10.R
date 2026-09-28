# =============================================================================
# run_p104q10.R -- the single definition of "the current ensemble"
#
# WHY THIS EXISTS. Every script in the figure pipeline is parameterised by
# environment variables whose DEFAULTS point at phase 88 or earlier. The
# phase-104 build was driven entirely by an env block that lived only in prose
# (AGENTS.md, docs/monte_carlo_workflow_review.md). Reassembling it by hand each
# time is how `F0_QMAX=1` silently reaches a refit fitted at 10, and how a
# figure gets built on 88_full_states without anything saying so.
#
# This file holds that block once, validates it against the stored objects, and
# runs a target script with it applied.
#
# ------------------------------------------------------------------ USAGE
#   Rscript run_p104q10.R                          print the shell export block
#   Rscript run_p104q10.R <script.R>               run <script.R> on the full 203
#   Rscript run_p104q10.R --top <script.R>         run it on the top cut (20)
#   source("run_p104q10.R")                        set the base block in-session
#
# --------------------------------------------------------------- THE TWO SETS
#   full  all 203 usable members  (stable AND drift_ok AND erepro < 1)
#   top   the best 20 by pooled log10 yield RMSE -- floor(0.10 * 203)
#
# The top membership is NEVER typed as a literal here. It is read from the
# ranking object, so the floor rule propagates instead of being re-entered.
#
# ------------------------------------------------------------------ NOT SET
# Figure suffixes that differ per script (FIG_SUF is `p104q10` for F02/F03/F04
# but `1g_p104q10` for F02b) are in PER_SCRIPT below, keyed by basename.
#
# The SNR denominator is also not set: F02/F02b build ONE of classic/medsd/
# paired per run and the published set carries all three. Set FIG_SNR yourself.
# =============================================================================

OL   <- "Output_large_files/wmin_test"
DATA <- "Manuscript data"

STATES <- file.path(OL, "104_full_states")
RANK   <- file.path(OL, "104_q10_rerank.rds")
MEMBERS<- file.path(OL, "104_q10.rds")
REFIT  <- file.path(OL, "104_refit_wh_q10.rds")
KC20   <- file.path(OL, "104_q10_KC20.rds")
YIELD  <- file.path(OL, "80_yield_by_species_p104q10.rds")
MANIF  <- file.path(OL, "104_q10_manifest.rds")
QMAX   <- "10"          # MUST match REFIT's meta$qmax; the guard checks it
SUF    <- "p104q10"

# --- where the figures go -----------------------------------------------------
# WITHOUT THESE every figure lands in `Manuscript figures/` root, alongside the
# rebuilt167 and p88 lineages, and has to be filed by hand afterwards -- which is
# how the p104 folder came to have a hand-made `pdf/` subfolder. The scripts
# below are the ones that actually read an output path from the environment.
#
# NOT COVERED, because their output directory is a hardcoded literal:
#   F05_supp_biomass_grid  FIGS <- "Manuscript figures/Supplemental figures"
#   KC16b, KC06            FIG  <- <KC_ROOT>/figures
# Those still need their output moved by hand, or a one-line patch to the script.
FIGDIR  <- file.path("Manuscript figures", "p104 figures")
FIGSUPP <- file.path(FIGDIR, "Supplemental figures")
# The p104 folder files the diet and krill-counterfactual figures in their own
# subfolders, so those scripts are pointed there rather than at FIGSUPP.
FIGDIET <- file.path(FIGDIR, "Diet")
FIGKC   <- file.path(FIGDIR, "Krill counterfactual")

# --- where the FishMIP ISIMIP3a outputs go ------------------------------------
# A NEW tree. The existing `FishMIP_ISIMIP3a_submission/` was built from the
# 2,111-member ensemble on a domain area 13.23x too large, and is left alone so
# the old submission stays auditable. FM05 refuses to write into it.
FMOUT <- "FishMIP_ISIMIP3a_submission_p104"

# --- the block every script shares -------------------------------------------
BASE <- c(
  F0_STATE_DIR = STATES,
  F0_RANK      = RANK,
  F0_MEMBERS   = MEMBERS,
  F0_REFIT     = REFIT,
  F0_SUFFIX    = SUF,
  F0_QMAX      = QMAX,
  # read by F02, F02b, F03, F04 and KC18; ignored by the F00 builders and by 80,
  # so it is safe in the shared block.
  FIG_OUT      = FIGDIR,
  FIG_SUPP     = FIGSUPP
)

# --- what each script needs on top of BASE -----------------------------------
# `set` is "full" or "top"; only the entries that actually differ are branched.
per_script <- function(set) {
  top <- identical(set, "top")
  cut_full <- "FULL usable"
  list(
    "F00_build_p88_data.R" = c(),
    "F00c_krill_baseline_1841_p88.R" = c(),
    "F00r_build_1g_from_states.R" = c(
      F0R_STATE_DIR   = STATES,
      F0R_CUTS_RDS    = RANK,
      F0R_CUT         = cut_full,     # the 1 g build always carries all 203;
      F0R_SUFFIX      = "1g_p104q10", # the top subset rides along in meta$cuts
      F0R_MULT        = REFIT,
      F0R_MIN_W       = "1",
      # F0R_STABLE_ONLY=1 reads a _summary.csv that does not exist for a phase-93
      # cut. 0 is correct here: a cut's members are already screened.
      F0R_STABLE_ONLY = "0"),
    "F00s_diet_composition_from_states.R" = c(
      F0S_STATE_DIR   = STATES,
      F0S_MANIFEST    = MANIF,        # written by ensure_manifest() below
      F0S_SUFFIX      = SUF,
      F0S_MULT        = REFIT,
      F0S_ARM         = "",           # phase-104 states carry no arm token
      F0S_STABLE_ONLY = "0"),
    "80_yield_by_species.R" = c(
      P80_STEM = "104_q10", P80_SUFFIX = SUF, P80_MULT = REFIT, P80_QMAX = QMAX),
    # KNOWN QUIRK: phase 81 appends `_top` to the filename whenever a cuts file
    # is given, regardless of which cut is applied, so the full-set run lands as
    # `yield_facets_p104q10_n203_top`. RENAME IT to drop `_top` -- the published
    # figure is `yield_facets_p104q10_n203`. See the note at that script's STEM.
    "81_yield_facets_figure.R" = c(
      P81_SUFFIX   = if (top) SUF else paste0(SUF, "_n203"),
      P81_IN       = YIELD,
      P81_CUTS_RDS = RANK,
      P81_CUT      = if (top) top_cut_name() else cut_full,
      P81_OUT      = FIGSUPP),
    "85_recruitment_vs_biomass.R" = c(
      P85_STEM   = "104_q10",
      P85_SUFFIX = if (top) "p104q10top20" else "p104q10n203",
      P85_RANK   = RANK,
      # derived from the ranking, never typed as a literal
      P85_TOP_N  = if (top) as.character(n_top()) else "0",
      P85_OUT    = FIGSUPP),
    # NOTE for F02/F02b: each run builds ONE SNR denominator, and the published
    # set carries three. Set FIG_SNR=medsd and FIG_SNR=paired alongside this for
    # the other two; classic is the default and needs nothing.
    "F02_figure2_snr_rebuilt167.R"      = c(FIG_SUF = SUF,
                                            FIG_SET = if (top) "top" else "all"),
    "F03_figure3_pctchange_rebuilt167.R" = c(FIG_SUF = SUF,
                                            FIG_SET = if (top) "top" else "all"),
    "F04_figure4_krill_ratio_rebuilt167.R" = c(FIG_SUF = SUF,
                                            FIG_SET = if (top) "top" else "all"),
    # F05 READS `F05_SUF`, NOT `FIG_SUF`. Setting FIG_SUF here (as this block did
    # until 2026-08-29) left it on its `rebuilt167` default, so it drew the WRONG
    # ENSEMBLE and then died on guard() against the existing rebuilt167 file.
    # Its output directory is a hardcoded literal, so FIG_OUT does not reach it
    # either -- the files land in `Manuscript figures/Supplemental figures/` and
    # must be moved into the p104 folder by hand.
    "F05_supp_biomass_grid_rebuilt167.R" = c(F05_SUF = SUF,
                                            FIG_SET = if (top) "top" else "all"),
    "F02b_figure2_snr_1g_rebuilt167.R" = c(FIG_SUF = "1g_p104q10",
                                           FIG_REF_SUF = SUF,
                                           FIG_SET = if (top) "top" else "all"),
    "F07_diet_composition_figures.R" = c(
      F07_SUF = SUF, F07_OUT = FIGDIET,
      FIG_SET = if (top) "top" else "all"),
    "KC20b_usable_only.R" = c(KC20_STEM = "104_q10", KC20_MULT = REFIT),
    # The four below take the `_top` token in their OWN suffix variable, so no
    # naming logic had to go into the scripts -- they gained only the filter.
    "KC16b_recruitment_usable.R" = c(
      KC16_IN  = KC20,
      KC16_SUF = paste0("_", SUF, if (top) "_top" else ""),
      FIG_SET  = if (top) "top" else "all"),
    "KC18_fig3_counterfactual.R" = c(
      KC18_IN  = KC20,
      KC18_SUF = paste0(SUF, if (top) "_top" else ""),
      # overrides BASE's FIG_OUT: this one is filed with the other KC figures,
      # not at the top level with figures 2-4.
      FIG_OUT  = FIGKC,
      FIG_SET  = if (top) "top" else "all"),
    # KC_SUF MUST carry its own leading underscore: KC06 builds the stem as
    # paste0("KC_supp_whale_abund_mass", SUF, ...) and defaults SUF to "".
    # Passing a bare "p104q10" writes `KC_supp_whale_abund_massp104q10.png`.
    "KC06_supp_whale_abund_mass.R" = c(
      KC_IN5  = KC20,
      KC_SUF  = paste0("_", SUF, if (top) "_top" else ""),
      FIG_SET = if (top) "top" else "all"),
    # --- FishMIP ISIMIP3a submission (R/fishmip_isimip3a/) --------------------
    # These read the already-projected member simulations in
    # `Output_large_files/wmin_test/p104q10_sims/`, so they need NEITHER
    # F0_STATE_DIR nor F0_REFIT/F0_QMAX -- catchability is already baked into
    # those sims. They take F0_RANK from BASE to assert membership, and write
    # to their own tree. The submission is always the FULL usable set, so
    # `--top` is deliberately not honoured here.
    "FM01_totals.R"       = c(FM_OUT_DIR = FMOUT),
    "FM02_species.R"      = c(FM_OUT_DIR = FMOUT),
    "FM03_ancillary.R"    = c(FM_OUT_DIR = FMOUT),
    "FM04_netcdf.R"       = c(FM_OUT_DIR = FMOUT),
    "FM05_stage_bundle.R" = c(FM_OUT_DIR = FMOUT),
    "FM06_verify.R"       = c(FM_OUT_DIR = FMOUT)
  )
}

# --- read the cut from the ranking, never from a literal ----------------------
top_cut_name <- function() {
  nm <- grep("^TOP ", names(readRDS(RANK)$cuts), value = TRUE)[1]
  if (is.na(nm)) stop("no TOP cut in ", RANK, call. = FALSE)
  nm
}
n_top <- function() length(readRDS(RANK)$cuts[[top_cut_name()]])

# --- validation ---------------------------------------------------------------
# Runs before anything is set, so a stale path or a QMAX mismatch is an error
# here rather than a wrong figure three hours later.
validate <- function(need_states = FALSE) {
  need <- c(rank = RANK, members = MEMBERS, refit = REFIT)
  miss <- need[!file.exists(need)]
  if (length(miss))
    stop("missing input(s):\n  ", paste(names(miss), unname(miss), sep = ": ",
         collapse = "\n  "), call. = FALSE)
  # THE STATES ARE VM-ONLY (~300 MB, see AGENTS.md). Only the scripts that
  # re-project members need them; every figure script downstream reads the
  # built products in Manuscript data/ instead. So this is fatal on demand and
  # silent otherwise, rather than blocking the whole config on a local machine.
  if (!dir.exists(STATES)) {
    if (need_states)
      stop("this script re-projects members but the state directory is absent:\n  ",
           STATES, "\nIt is VM-only -- run this on the VM, or point F0_STATE_DIR ",
           "at a local copy.", call. = FALSE)
    message("note: ", STATES, " absent (VM-only). Fine for scripts that read ",
            "Manuscript data/; fatal for the F00/80 state builders.")
  }

  RF <- readRDS(REFIT)
  if (!is.null(RF$meta$qmax) && !isTRUE(all.equal(as.numeric(QMAX), RF$meta$qmax)))
    stop("QMAX ", QMAX, " does not match ", basename(REFIT),
         " (fitted at qmax = ", RF$meta$qmax, ")", call. = FALSE)

  RR <- readRDS(RANK)
  n_full <- length(RR$cuts[["FULL usable"]]); nt <- n_top()
  expect <- floor(n_full * 0.10)
  if (nt != expect)
    stop("the stored top cut is ", nt, " but floor(0.10 * ", n_full, ") = ",
         expect, ". Re-run 93_rerank_p88.R.", call. = FALSE)
  if (!is.null(RF$meta$n_members) && RF$meta$n_members != n_full)
    warning("refit fitted ", RF$meta$n_members, " members, ranking carries ",
            n_full, call. = FALSE)
  invisible(list(n_full = n_full, n_top = nt))
}

# F00s wants a manifest holding $members; the phase-104 build wrote one by hand
# and did not keep it. Generate it from the ranking so it cannot drift.
ensure_manifest <- function() {
  if (file.exists(MANIF)) return(invisible(MANIF))
  saveRDS(list(members = as.integer(readRDS(RANK)$cuts[["FULL usable"]]),
               source = RANK, built = Sys.time()), MANIF)
  message("wrote manifest: ", MANIF)
  invisible(MANIF)
}

apply_env <- function(script = NULL, set = "full") {
  v <- BASE
  if (!is.null(script)) {
    extra <- per_script(set)[[basename(script)]]
    if (is.null(extra))
      message("note: no per-script block for ", basename(script),
              " -- BASE only. Check its own env names before trusting this.")
    v <- c(v, extra)
  }
  v <- v[!duplicated(names(v), fromLast = TRUE)]
  do.call(Sys.setenv, as.list(v))
  invisible(v)
}

# --- entry point ---------------------------------------------------------------
if (!interactive()) {
  a <- commandArgs(trailingOnly = TRUE)
  set <- if ("--top" %in% a) "top" else "full"
  a <- a[!a %in% c("--top", "--full")]
  # the scripts that re-project members from stored states
  STATE_READERS <- c("F00_build_p88_data.R", "F00c_krill_baseline_1841_p88.R",
                     "F00r_build_1g_from_states.R",
                     "F00s_diet_composition_from_states.R",
                     "80_yield_by_species.R")
  info <- validate(need_states = length(a) > 0 &&
                                 basename(a[1]) %in% STATE_READERS)

  if (!length(a)) {
    # BASE is safe to export globally. The per-script blocks are NOT: FIG_SUF is
    # `p104q10` for F02/F03/F04 but `1g_p104q10` for F02b, so flattening them
    # into one block silently gives every figure the last value written. They
    # are printed per script, commented, so the conflict stays visible.
    cat(sprintf("# p104q10 | %d usable | top %d | set=%s\n",
                info$n_full, info$n_top, set))
    cat(sprintf("export %s=%s\n", names(BASE), shQuote(unname(BASE))), sep = "")
    cat("\n# --- per script (do NOT export these together) ---\n")
    ps <- per_script(set)
    for (nm in names(ps)) {
      e <- ps[[nm]]
      if (!length(e)) { cat(sprintf("# %s: BASE only\n", nm)); next }
      cat(sprintf("# %s\n", nm))
      cat(sprintf("#   export %s=%s\n", names(e), shQuote(unname(e))), sep = "")
    }
  } else {
    target <- a[1]
    if (!file.exists(target)) stop("no such script: ", target, call. = FALSE)
    if (identical(basename(target), "F00s_diet_composition_from_states.R"))
      ensure_manifest()
    v <- apply_env(target, set)
    cat(sprintf("=== p104q10 [%s] | %d usable | top %d ===\n",
                set, info$n_full, info$n_top))
    cat(sprintf("  %-16s %s\n", names(v), unname(v)), sep = "")
    cat("--- running", target, "---\n")
    source(target, echo = FALSE)
  }
} else {
  validate(); apply_env()
  message("p104q10 base block applied to this session.")
}
