# =============================================================================
# STAGE 2a -- reproduction, at fixed initial_n.
#
# THE QUESTION.  Where catchability saturates -- more q buys less and less catch
# while the population is mined down -- is the binding constraint recruitment?
# Member 446's minke whales are the clean case: q from 0.403 to the cap 1.0
# multiplies catch by only 1.37 while halving the surviving biomass, and even at
# q = 1 the model delivers 15% of the observed catch.
#
# TWO LEVERS, and they are NOT interchangeable.  With
#   RDD = RDI / (1 + RDI/R_max)   and   reproduction_level = RDD / R_max
#
#   A  R_max x k at fixed erepro.  Raises RDD, so it raises both the recruitment
#      CEILING and the standing population.  This is the lever that can supply
#      more catch.  erepro untouched, so erepro < 1 is preserved by construction.
#
#   B  reproduction_level -> target at fixed RDD.  R_max up, erepro DOWN, and the
#      state at initial_n is unchanged -- it changes only how strongly
#      recruitment compensates as the population is depleted.  Included because
#      the brief's hypothesis is about compensation, and the sign has to be
#      measured rather than assumed.
#
# At reproduction_level ~ 1 (member 446: baleen 1.0000, sperm 0.9994, orca
# 0.9989) RDD ~ R_max, so erepro is a DEAD lever for those groups -- raising it
# raises RDI with almost no effect on RDD.  Lever A is the only one that moves
# them.  The sweep demonstrates this rather than asserting it.
#
# NEVER setBevertonHolt(reproduction_level = <full-length vector>).  Verified:
# feeding the OTHER 18 groups their own unchanged levels back perturbs them ~2%
# and pushes orca from erepro 0.9509 to 1.0104, breaching the hard constraint --
# because initial_n is the post-spin-up state, not the state the stored R_max and
# erepro were derived at.  Both levers below write the target row only.
#
# PAIRED UNFISHED CONTROL.  A reproduction change moves the unfished trajectory
# too, so every configuration is also run at effort = 0 and the depletion ratio
# (fished end / unfished end) is reported next to the RMSE.  Otherwise "the fit
# improved" cannot be separated from "the population grew".  The unfished arm
# does not depend on q, so it is run once per configuration, not once per q.
#
# NO 1929 CACHE HERE.  The cache is only valid while the pre-1930 trajectory is
# unchanged; reproduction changes it.  Every evaluation is a full 1841-2010 run.
#
# USAGE
#   Rscript R/catch_fit/04_repro_sweep.R                 # all 5 members
#   Rscript R/catch_fit/04_repro_sweep.R 446
# Env: CF_CORES, CF_REPRO_GROUPS (";"-separated, overrides the default targets)
# =============================================================================

source("R/catch_fit/01_harness.R")
suppressPackageStartupMessages(library(parallel))

args <- commandArgs(trailingOnly = TRUE)
members_wanted <- if (length(args)) as.integer(args) else CF_MEMBERS

OUT <- cf_out_dir("repro_sweep")

# The brief's three named groups are always swept so the hypothesis gets a direct
# answer, whatever Stage 1 concluded about them.
DEFAULT_GROUPS <- c("minke whales", "sperm whales", "baleen whales")
gs <- Sys.getenv("CF_REPRO_GROUPS", "")
TARGETS <- if (nzchar(gs)) strsplit(gs, ";")[[1]] else DEFAULT_GROUPS

RMAX_MULT <- c(1, 2, 5, 10, 100, 1000)      # lever A
REPRO_LVL <- c(0.75, 0.5, 0.25)             # lever B
Q_MULT    <- c(1, 1.78, 3.16)               # clipped to the q <= 1 box, deduped

# cf_scale_rmax(), cf_set_repro_level() and cf_repro_state() live in
# 01_harness.R so they can be exercised without launching the sweep.

# ------------------------------------------------------------------- run -----
cf_log("=== Stage 2a: reproduction sweep at fixed initial_n ===")
cf_log("targets: ", paste(TARGETS, collapse = ", "))

members <- cf_load_members(members_wanted)
eff <- cf_effort()
CORES <- cf_cores()

# Stage 1b optima, keyed by sim_index -- the starting catchability for the sweep
QOPT <- local({
  fs <- sort(list.files(cf_out_dir("q_optimum"), pattern = "^opt_\\d+\\.rds$",
                        full.names = TRUE))
  if (!length(fs))
    stop("Stage 1b output missing -- run 03_q_optimise.R first")
  z <- lapply(fs, readRDS)
  setNames(lapply(z, function(x) {
    o <- if (x$winner == "grid") x$grid else x$baseline
    o$q
  }), vapply(z, function(x) as.character(x$sim_index), character(1)))
})
missing_q <- setdiff(as.character(members_wanted), names(QOPT))
if (length(missing_q))
  stop("no Stage 1b optimum for member(s) ", paste(missing_q, collapse = ", "))
cf_log("starting from the Stage 1b optimum for ", length(QOPT), " members")

sweep_member <- function(m) {
  suppressPackageStartupMessages({ library(therMizer); library(mizer) })
  si <- m$sim_index
  f  <- file.path(OUT, sprintf("rep_%04d.rds", si))

  # Start from the Stage 1b OPTIMUM, not the member's Monte Carlo draw.  The
  # level-2 question is "catchability has already been pushed as far as it goes;
  # does reproduction buy anything more?", so every other gear is held at its
  # optimised value and the ladder is applied on top of the target gear's.
  P0 <- cf_set_q(m$params, QOPT[[as.character(si)]], cap = 1)
  q0 <- cf_get_q(P0)
  inh <- cf_inherited_flags(P0)   # pre-existing low-trophic R_max = Inf, carried
  rows <- list(); t0 <- proc.time(); nev <- 0L

  # configurations: (group, lever, level), including the k = 1 / no-change control
  cfgs <- rbind(
    do.call(rbind, lapply(TARGETS, function(g)
      data.frame(group = g, lever = "R_max", level = RMAX_MULT,
                 stringsAsFactors = FALSE))),
    do.call(rbind, lapply(TARGETS, function(g)
      data.frame(group = g, lever = "repro_level", level = REPRO_LVL,
                 stringsAsFactors = FALSE))))
  cf_log("member ", si, ": ", nrow(cfgs), " reproduction configurations")

  for (r in seq_len(nrow(cfgs))) {
    g <- cfgs$group[r]; lev <- cfgs$lever[r]; val <- cfgs$level[r]
    P <- if (lev == "R_max") cf_scale_rmax(P0, g, val) else
                             cf_set_repro_level(P0, g, val)
    st <- cf_repro_state(P, g)
    ok <- tryCatch({ cf_assert_admissible(P, inherited = inh,
                                          label = paste(si, g, lev, val)); TRUE },
                   error = function(e) { cf_log("  REJECTED ", g, " ", lev, "=",
                                                val, ": ", conditionMessage(e)); FALSE })
    if (!ok) next

    # unfished control -- independent of q, so run once per configuration
    su <- project(P, t_start = CF_T_START, t_max = CF_T_MAX, effort = 0)
    bu <- getBiomass(su); nev <- nev + 1L
    unf_end <- bu[nrow(bu), ]; unf_start <- bu[1, ]

    qs <- unique(pmin(1, q0[[g]] * Q_MULT))
    for (qv in qs) {
      q <- q0; q[[g]] <- qv
      P2 <- cf_set_q(P, q, cap = 1)
      s  <- project(P2, t_start = CF_T_START, t_max = CF_T_MAX, effort = eff)
      nev <- nev + 1L
      y  <- getYield(s); y <- y[as.numeric(rownames(y)) <= CF_LAST_YEAR, , drop = FALSE]
      sc <- cf_score_yield_matrix(y)
      bf <- getBiomass(s); fis_end <- bf[nrow(bf), ]
      ps <- sc$per_species
      rows[[length(rows) + 1]] <- data.frame(
        sim_index = si, group = g, lever = lev, level = val,
        q = qv, q_baseline = q0[[g]], q_mult = qv / q0[[g]],
        erepro = unname(st["erepro"]), R_max = unname(st["R_max"]),
        repro_level = unname(st["repro_level"]),
        rdd = unname(st["rdd"]), rdi = unname(st["rdi"]),
        max_erepro_all = unname(st["max_erepro_all"]),
        n_rmax_inf_all = unname(st["n_rmax_inf_all"]),
        total_rmse = sc$rmse,
        own_rmse = ps$rmse[match(g, ps$Species)],
        own_mod_obs = ps$mod_obs_ratio[match(g, ps$Species)],
        own_end_fished = unname(fis_end[g]),
        own_end_unfished = unname(unf_end[g]),
        own_start = unname(unf_start[g]),
        depletion = unname(fis_end[g] / unf_end[g]),
        unfished_growth = unname(unf_end[g] / unf_start[g]),
        community_unfished_pct = 100 * (sum(unf_end) - sum(unf_start)) / sum(unf_start),
        stringsAsFactors = FALSE)
    }
    el <- (proc.time() - t0)[["elapsed"]]
    cf_log(sprintf("  [%2d/%2d] %-14s %-11s %-8.4g  %d evals  %.1f min elapsed, ETA %.1f min",
                   r, nrow(cfgs), g, lev, val, nev, el / 60,
                   (el / r) * (nrow(cfgs) - r) / 60))
  }

  res <- do.call(rbind, rows)
  saveRDS(res, f)
  sprintf("member %d: %d rows, %d projections, %.1f min", si, nrow(res), nev,
          (proc.time() - t0)[["elapsed"]] / 60)
}

# ------------------------------------------------------------- dispatch ------
# Parallelism is across MEMBERS: they are independent, each needs its own params,
# and the per-member work is a long serial chain of projections.
todo <- Filter(function(m)
  !file.exists(file.path(OUT, sprintf("rep_%04d.rds", m$sim_index))), members)
cf_log(length(todo), " members to sweep (", length(members) - length(todo),
       " already done) / cores ", CORES)

if (length(todo)) {
  t0 <- proc.time()
  if (CORES > 1 && length(todo) > 1) {
    cl <- makeCluster(min(CORES, length(todo)))
    wd <- getwd()
    clusterExport(cl, "wd", envir = environment())
    clusterEvalQ(cl, setwd(wd))
    clusterExport(cl, c("OUT", "TARGETS", "RMAX_MULT", "REPRO_LVL", "Q_MULT",
                        "eff", "QOPT"), envir = environment())
    clusterEvalQ(cl, source("R/catch_fit/01_harness.R"))
    clusterExport(cl, "sweep_member", envir = environment())
    msgs <- tryCatch(parLapplyLB(cl, todo, sweep_member),
                     finally = stopCluster(cl))
  } else {
    msgs <- lapply(todo, sweep_member)
  }
  for (s in msgs) cf_log(s)
  cf_log("Stage 2a elapsed ",
         round((proc.time() - t0)[["elapsed"]] / 60, 1), " min")
}

fs <- sort(list.files(OUT, pattern = "^rep_\\d+\\.rds$", full.names = TRUE))
if (length(fs)) {
  D <- do.call(rbind, lapply(fs, readRDS))
  saveRDS(D, file.path(cf_out_dir(), "04_repro_sweep.rds"))
  write.csv(D, file.path(cf_out_dir(), "04_repro_sweep.csv"), row.names = FALSE)
  cf_log("collected ", nrow(D), " rows from ", length(fs), " members")
}
