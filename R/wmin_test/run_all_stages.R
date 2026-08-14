# =============================================================================
# `small divers` w_min investigation -- stage driver.
#
# Documents and runs the whole sequence in dependency order. Narrative and
# results: docs/small_divers_wmin_SYNTHESIS.md
#
# USAGE (from the repository root):
#   Rscript R/wmin_test/run_all_stages.R            # list stages, run nothing
#   Rscript R/wmin_test/run_all_stages.R 26 27 29   # run these stages
#   Rscript R/wmin_test/run_all_stages.R A          # run a named group
#   Rscript R/wmin_test/run_all_stages.R all        # everything (many hours)
#
# Nothing runs unless stages are named -- the default prints the plan. Several
# stages take over an hour and two need the 1.86 GB ensemble, so accidental
# full re-runs are worth avoiding.
#
# ENVIRONMENT
#   mizer 3.1.0, therMizer 1.0.0, R 4.6.0
#   Rscript: C:/Program Files/R/R-4.6.0/bin/Rscript.exe
#   library(therMizer) must be ATTACHED for anything touching params@rates_funcs
#   -- including getYield(), which routes through projectRateFunctions.
#   Saved sims are mizer 2.5.0: use validParams() (verified bit-identical).
#   Do NOT downgrade mizer -- it breaks therMizer 1.0.0 and invalidates 17.
#
# GROUND RULES (observed throughout, keep them)
#   * Nothing existing is overwritten. New outputs go to
#     Output_large_files/wmin_test/ or a clearly new filename.
#   * Exploited and unexploited are treated as matched pairs.
#   * Spin-ups are re-run, never reused -- w_min changes the steady state.
#   * A re-run CANNOT reproduce the stored ensemble (initial_n is the
#     post-spin-up state; the pre-spin-up state was never saved). Always run a
#     paired control through the identical pipeline and interpret only
#     treated-minus-control. See docs/small_divers_wmin_stageD2_results.md.
# =============================================================================

RSCRIPT <- file.path(R.home("bin"), "Rscript")
HERE <- "R/wmin_test"

stages <- list(
  # ---- group A: source fix and audit (fast, no ensemble needed) ------------
  list(id = "28", group = "A", script = "28_size_param_audit.R", mins = 0.2,
       needs = "params only",
       what = "Audit all 19 groups vs the source trait table. Finds BOTH mizer clamps and shows only the w_min one bit.",
       out = "docs/size_parameter_audit.md, 28_audit.csv"),
  list(id = "35", group = "A", script = "35_emit_trait_table_v5.R", mins = 0.1,
       needs = "params only",
       what = "Emit trait table v5 carrying the documented 0.85*w_mat compromise. v4 kept as ensemble provenance.",
       out = "group params/trait_groups_params_vCWC_v5.csv + _PROVENANCE.md"),

  # ---- group B: is a valid recalibration possible? -------------------------
  list(id = "26", group = "B", script = "26_repro_feasibility.R", mins = 1,
       needs = "375 MB fitted ensemble",
       what = "Analytic: what erepro restores control recruitment at the new w_min? Establishes the floor is < 1.",
       out = "26_repro_feasibility.{rds,csv}"),
  list(id = "27", group = "B", script = "27_recalibrate_penguin_repro.R", mins = 27,
       needs = "375 MB fitted ensemble",
       what = "Recalibrate the penguin erepro/R_max pair at 3 reproduction levels, 6 distinct members, paired vs control.",
       out = "27_recal_{sims,summary}.{rds,csv}",
       env = "WMIN_N_MEMBERS=6"),
  list(id = "29", group = "B", script = "29_stageB_gate.R", mins = 0.5,
       needs = "27 output",
       what = "PAIRED gate (treated vs control, not vs an ideal). Also exposes the pre-existing R_max = Inf degeneracy.",
       out = "docs/small_divers_wmin_stageB_results.md, 29_gate_table.csv"),

  # ---- group C: the corrected calibrated steady state ---------------------
  list(id = "30", group = "C", script = "30_stageC_corrected_params.R", mins = 0.5,
       needs = "Manuscript data/params_sel_adj.rds",
       what = "Re-enter at 09_Uncertainty_Analysis.Rmd:1444's steady() call. ANSWERS THE ORIGINAL QUESTION: -0.005% community biomass.",
       out = "params_sel_adj_wmin_corrected.rds, 30_stageC_comparison.csv"),

  # ---- group F: deduplication (must precede D) ----------------------------
  list(id = "31", group = "F", script = "31_dedupe_parameter_draws.R", mins = 0.5,
       needs = "parameter summary CSV",
       what = "Duplicate count from the parameter CSV. DIAGNOSTIC ONLY -- that file misaligns with the RMSE table; 33 supersedes it.",
       out = "31_dedupe_{groups,unique_ranking}.csv"),
  list(id = "33", group = "F", script = "33_dedupe_full_and_rmse.R", mins = 32,
       needs = "1.86 GB full ensemble",
       what = "AUTHORITATIVE dedupe from the ensemble objects + full RMSE recompute. 2,111 -> 1,997; clean top 10% = 200.",
       out = "33_dedupe_full.rds, Manuscript data/yield_rmse_per_sim_deduped.csv"),

  # ---- group D: do the Monte Carlo gates move? ---------------------------
  list(id = "34", group = "D", script = "34_stageD_stability_paired.R", mins = 74,
       needs = "33 output + both ensembles",
       what = "Condition (i): stability acceptance. Single steady() call, matching the driver. Result: 0 of 200 change status.",
       out = "34_stageD_stability.{rds,csv}"),
  list(id = "36", group = "D", script = "36_stageD2_rmse_ranking.R", mins = 136,
       needs = "33 output + both ensembles",
       what = "Condition (ii): project 1841-2010, recompute RMSE. rho 0.998, net 1 member. Also quantifies the re-run effect (rho 0.60).",
       out = "36_stageD2_rmse.{rds,csv}")
)

# earlier scripts, kept for provenance -- not part of the current sequence
superseded <- c(
  "00-08   Stage 0 analytic bound and its verification (docs/small_divers_wmin_stage0_results.md).",
  "        Its option (c) 'crop' recommendation is SUPERSEDED.",
  "16-25   Stage 1: w_min changed WITHOUT recalibrating reproduction. SUPERSEDED --",
  "        it measured an uncalibrated model (penguins extinct, R_max -> Inf).",
  "        17_validate_rerun.R is still the environment gate and worth re-running",
  "        after any package change. 21 and 25 diagnosed the extinction and the",
  "        duplicate members respectively and are still cited."
)

fmt <- function(x, w) formatC(x, width = w, flag = "-")

print_plan <- function() {
  cat("=== `small divers` w_min investigation -- stage driver ===\n\n")
  cat("Narrative and results: docs/small_divers_wmin_SYNTHESIS.md\n")
  cat("Independent review brief: docs/small_divers_wmin_REVIEW_BRIEF.md\n\n")
  cat(fmt("id", 4), fmt("grp", 4), fmt("script", 36), fmt("mins", 6), "needs\n")
  cat(strrep("-", 100), "\n")
  tot <- 0
  for (s in stages) {
    cat(fmt(s$id, 4), fmt(s$group, 4), fmt(s$script, 36),
        fmt(s$mins, 6), s$needs, "\n")
    cat("     ", s$what, "\n")
    cat("      -> ", s$out, "\n\n", sep = "")
    tot <- tot + s$mins
  }
  cat("total if run end to end: ~", round(tot / 60, 1), " hours\n\n", sep = "")
  cat("Superseded / provenance:\n")
  for (l in superseded) cat("  ", l, "\n")
  cat("\nOutstanding (not scripted here):\n")
  cat("  1. Rerun acceptance over the full deduplicated 1,997, paired (~10 h).\n")
  cat("  2. Apply the erepro>1 / R_max=Inf filter, re-cut a refined top 10%.\n")
  cat("     NOTE: erepro is capped below 1, so 'erepro > 1' and 'penguin R_max =\n")
  cat("     Inf' select the SAME members. The second rule additionally catches\n")
  cat("     members degenerate in a DIFFERENT group -- a separate, pre-existing\n")
  cat("     problem -- so the filter does two jobs. Do not attribute the counts\n")
  cat("     solely to w_min.\n")
  cat("  3. Regenerate downstream caches, figures, FishMIP outputs (~1 day).\n\n")
  cat("Usage: Rscript R/wmin_test/run_all_stages.R <id|group|all> ...\n")
}

run_stage <- function(s) {
  path <- file.path(HERE, s$script)
  if (!file.exists(path)) {
    cat("!! missing:", path, "\n"); return(invisible(FALSE))
  }
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat("STAGE ", s$id, " (", s$group, ")  ", s$script, "\n", sep = "")
  cat(s$what, "\n")
  cat("expected ~", s$mins, " min | needs: ", s$needs, "\n", sep = "")
  cat(strrep("=", 78), "\n", sep = "")
  t0 <- proc.time()
  env <- if (!is.null(s$env)) s$env else character(0)
  status <- system2(RSCRIPT, args = path, env = env)
  el <- round((proc.time() - t0)["elapsed"] / 60, 1)
  cat("\n-- stage ", s$id, if (status == 0) " OK" else
    paste0(" FAILED (exit ", status, ")"), " in ", el, " min\n", sep = "")
  invisible(status == 0)
}

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) { print_plan(); quit(save = "no") }

sel <- if (identical(tolower(args[1]), "all")) stages else
  Filter(function(s) s$id %in% args | s$group %in% toupper(args), stages)

if (!length(sel)) {
  cat("No stage matched:", paste(args, collapse = " "), "\n\n"); print_plan()
  quit(save = "no", status = 1)
}

cat("Running", length(sel), "stage(s):",
    paste(vapply(sel, `[[`, character(1), "id"), collapse = ", "), "\n")
cat("Estimated ~", round(sum(vapply(sel, `[[`, numeric(1), "mins")) / 60, 1),
    " hours\n", sep = "")

ok <- TRUE
for (s in sel) if (!run_stage(s)) { ok <- FALSE; break }
cat("\n", if (ok) "All requested stages completed." else
  "Stopped on failure -- fix before continuing.", "\n", sep = "")
quit(save = "no", status = if (ok) 0 else 1)