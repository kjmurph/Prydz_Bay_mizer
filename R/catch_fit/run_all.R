# =============================================================================
# CATCH FITTING -- dependency order and how to drive it.
#
# This file DOCUMENTS the sequence; it does not run it.  Each stage is
# checkpointed per member and skips work already on disk, so any stage can be
# interrupted and resumed, and the core count can change between launches.
#
# CORE BUDGET.  Always leave at least one core free -- saturating all 16 risks
# taking the machine down and losing whatever long run is in flight.  Count busy
# workers first:
#     Get-Process Rscript | Where-Object { $_.CPU -gt 100 }
# then set CF_CORES to at most (16 - busy - 1).
#
# ---------------------------------------------------------------------------
# ARM A -- the headline.  Stored post-spin-up state, w_min uncorrected, control
#          bit-exact against the published ensemble (4.6e-15).
# ---------------------------------------------------------------------------
#   Rscript R/catch_fit/tests_levers.R                  # unit checks, seconds
#   Rscript R/catch_fit/tests_baseline_admissibility.R  # stored-params audit
#   Rscript R/catch_fit/00_validate.R                   # 5 gates, ~2 min
#   Rscript R/catch_fit/02_q_response.R                 # 1-D surfaces, ~20 min/member
#   Rscript R/catch_fit/02b_q_extend.R                  # extend edge optima
#   Rscript R/catch_fit/03_q_optimise.R                 # coordinate descent + cap diag
#   Rscript R/catch_fit/04_repro_sweep.R                # reproduction, ~20 min/member
#
# ---------------------------------------------------------------------------
# ARMS B and C -- the w_min sensitivity of the optimum.
# ---------------------------------------------------------------------------
#   Rscript R/catch_fit/05_wmin_arms.R                  # build both, ~7 min/member
#   $env:CF_ARM="B"; Rscript R/catch_fit/00_validate.R
#   $env:CF_ARM="B"; Rscript R/catch_fit/02_q_response.R
#   $env:CF_ARM="B"; Rscript R/catch_fit/02b_q_extend.R
#   $env:CF_ARM="B"; Rscript R/catch_fit/03_q_optimise.R
#   ... same for CF_ARM="C"
#
# Report optimum(C) - optimum(B).  NEVER C - A: re-entering the pipeline reorders
# the RMSE ranking at rho 0.60 on its own, ~5,500x the size of the w_min effect,
# so an A-vs-C difference is dominated by the re-run, not by w_min.
#
# ---------------------------------------------------------------------------
# OUTPUTS
# ---------------------------------------------------------------------------
#   Output_large_files/catch_fit/          arm A
#   Output_large_files/catch_fit_armB/     arm B
#   Output_large_files/catch_fit_armC/     arm C
#   Output_large_files/catch_fit/wmin_arms/   the rebuilt params (arm-independent)
#   docs/catch_fit_RESULTS.md              the deliverable
#
# Nothing existing is overwritten.  The ensembles, the wmin_test outputs and
# every yield_rmse_per_sim*.csv are read-only inputs.
#
# ---------------------------------------------------------------------------
# ENV
# ---------------------------------------------------------------------------
#   CF_CORES          worker count (default 1)
#   CF_ARM            A (default), B or C
#   CF_REPRO_GROUPS   ";"-separated override for the Stage 2a targets
# =============================================================================

cat(readLines(file.path("R", "catch_fit", "run_all.R"))[1:60], sep = "\n")
