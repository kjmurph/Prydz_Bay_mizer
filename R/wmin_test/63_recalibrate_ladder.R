# =============================================================================
# Phase 63 -- recalibrate the phase-62 object with the standard steady() /
# matchBiomasses ladder, with a paired CONTROL arm
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
tag <- function(x) file.path(out_dir, paste0("63_", ARM, "_", x))
DEF_OUT <- if (ARM == "treatment") {
  "params_ref_p63_recal_intres_balkrill075.rds"
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
      tr[[length(tr) + 1]] <- data.frame(
        arm = ARM, tol = tol, round = r, max_dev = d, max_erepro = me,
        n_erepro_ge1 = nbad, max_repro_level = max(rl),
        n_above_cap = sum(rl > CAP + 1e-9), converged = st$converged)
      cat(sprintf("   tol=%-6.3g r=%-2d dev %.5f  max erepro %.4f (%d>=1)  max rl %.4f (%d>cap)  %s\n",
                  tol, r, d, me, nbad, max(rl), sum(rl > CAP + 1e-9),
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