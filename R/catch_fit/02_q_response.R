# =============================================================================
# STAGE 1a -- per-gear catchability response surfaces.
#
# For each member and each of the 9 gears with non-zero catchability, sweep that
# gear's q over a log grid with every other gear held at the member's own value.
# This is the deliberate exploration the Monte Carlo did not do: it draws the
# response curve rather than sampling it.
#
# The grid is 10^seq(-3, 3, 0.25) as a MULTIPLIER on the member's q, clipped to
# the pipeline's admissible box q <= 1 (09_Uncertainty_Analysis.Rmd:351) and then
# deduplicated -- for a gear already near the cap the upper multipliers all clip
# to the same q and would be the same projection.  Multiplier 1 is always kept as
# the in-place reference.
#
# Every row records the pooled RMSE, all 9 per-species RMSEs, and the end/start
# biomass ratio of the swept group, so a population collapse can never be
# mistaken for a fit improvement.  Cross-gear coupling falls out of the per-
# species columns: it is how much the OTHER eight groups move when one q changes.
#
# CHECKPOINTED per member.  Re-running skips members already written.
#
# USAGE
#   Rscript R/catch_fit/02_q_response.R            # all 5 members
#   Rscript R/catch_fit/02_q_response.R 446        # one member
# Env: CF_CORES (default 1)
# =============================================================================

source("R/catch_fit/01_harness.R")
suppressPackageStartupMessages(library(parallel))

args <- commandArgs(trailingOnly = TRUE)
members_wanted <- if (length(args)) as.integer(args) else CF_MEMBERS

OUT   <- cf_out_dir("q_response")
MULTS <- 10^seq(-3, 3, by = 0.25)
CORES <- cf_cores()

# --------------------------------------------------------------- grid --------
#' Deduplicated q grid for one gear: multipliers mapped to absolute q, clipped
#' to the cap, unique.  Multiplier 1 always present.
cf_q_grid <- function(q0, mults = MULTS, cap = 1) {
  if (q0 <= 0) return(NULL)
  m <- sort(unique(c(mults, 1)))
  q <- pmin(cap, q0 * m)
  keep <- !duplicated(round(q, 15))
  data.frame(mult = m[keep], q = q[keep],
             at_cap = abs(q[keep] - cap) < 1e-12, stringsAsFactors = FALSE)
}

# ------------------------------------------------------------ evaluate -------
cf_eval_point <- function(k) {
  suppressPackageStartupMessages({ library(therMizer); library(mizer) })
  g  <- GRID[k, ]
  qv <- setNames(g$q, g$gear)
  sc <- cf_eval_q(PARAMS, qv, cache = CACHE, cap = 1)
  ps <- sc$per_species
  data.frame(
    sim_index   = SIM_INDEX,
    gear        = g$gear,
    mult        = g$mult,
    q           = g$q,
    q_baseline  = g$q0,
    at_cap      = g$at_cap,
    total_rmse  = sc$rmse,
    own_rmse    = ps$rmse[match(g$gear, ps$Species)],
    own_sse_pct = ps$sse_pct[match(g$gear, ps$Species)],
    own_mod_obs = ps$mod_obs_ratio[match(g$gear, ps$Species)],
    own_biomass_ratio = unname(sc$biomass_ratio[g$gear]),
    min_biomass_ratio = min(sc$biomass_ratio),
    min_biomass_group = names(sc$biomass_ratio)[which.min(sc$biomass_ratio)],
    # per-species RMSE for all 9 fished groups -> cross-gear coupling
    setNames(as.list(ps$rmse), paste0("rmse_", gsub(" ", "_", ps$Species))),
    stringsAsFactors = FALSE, check.names = FALSE)
}

# ----------------------------------------------------------------- run -------
cf_log("=== Stage 1a: per-gear q response surfaces ===")
cf_log("members ", paste(members_wanted, collapse = ", "), " / cores ", CORES)

members <- cf_load_members(members_wanted)
fished  <- cf_fished_species()
cf_log("gears swept (", length(fished), "): ", paste(fished, collapse = ", "))

t_start_all <- proc.time()
done_pts <- 0L

for (m in members) {
  si <- m$sim_index
  f  <- file.path(OUT, sprintf("res_%04d.rds", si))
  if (file.exists(f)) { cf_log("member ", si, " done, skipping"); next }

  PARAMS <- m$params
  SIM_INDEX <- si
  inh <- cf_inherited_flags(PARAMS)
  if (length(inh$rmax_inf))
    cf_log("  member ", si, " carries inherited R_max = Inf: ",
           paste(inh$rmax_inf, collapse = ", "),
           " (pre-existing, unfished groups, carried not fixed)")
  cf_assert_admissible(PARAMS, inherited = inh,
                       label = paste("member", si, "baseline"))
  q0_all <- cf_get_q(PARAMS)

  CACHE <- cf_make_cache(PARAMS)

  GRID <- do.call(rbind, lapply(fished, function(g) {
    gr <- cf_q_grid(q0_all[[g]])
    if (is.null(gr)) return(NULL)
    cbind(gear = g, q0 = q0_all[[g]], gr, stringsAsFactors = FALSE)
  }))
  cf_log("member ", si, ": ", nrow(GRID), " grid points across ",
         length(unique(GRID$gear)), " gears")

  t0 <- proc.time()
  if (CORES > 1) {
    cl <- makeCluster(CORES)
    # PSOCK workers do not inherit the master's working directory, and every
    # path in the harness is repo-relative.  Set it before sourcing anything.
    wd <- getwd()
    clusterExport(cl, "wd", envir = environment())
    clusterEvalQ(cl, setwd(wd))
    clusterExport(cl, c("GRID", "PARAMS", "CACHE", "SIM_INDEX"),
                  envir = environment())
    clusterEvalQ(cl, source("R/catch_fit/01_harness.R"))
    clusterExport(cl, "cf_eval_point", envir = environment())
    rows <- tryCatch(parLapplyLB(cl, seq_len(nrow(GRID)), cf_eval_point),
                     finally = stopCluster(cl))
  } else {
    rows <- vector("list", nrow(GRID))
    for (k in seq_len(nrow(GRID))) {
      rows[[k]] <- cf_eval_point(k)
      if (k %% 10 == 0) {
        el <- (proc.time() - t0)[["elapsed"]]
        cf_log(sprintf("  %s %3d/%3d  %.1f s/eval  ETA %.1f min",
                       si, k, nrow(GRID), el / k,
                       (el / k) * (nrow(GRID) - k) / 60))
      }
    }
  }
  res <- do.call(rbind, rows)
  saveRDS(res, f)
  done_pts <- done_pts + nrow(GRID)
  el <- (proc.time() - t0)[["elapsed"]]
  cf_log(sprintf("member %d complete: %d points in %.1f min (%.2f s/eval) -> %s",
                 si, nrow(GRID), el / 60, el / nrow(GRID), basename(f)))

  # per-gear best, printed as it goes so the shape is visible early
  best <- do.call(rbind, lapply(split(res, res$gear), function(d) {
    d[which.min(d$total_rmse), c("gear", "mult", "q", "total_rmse", "own_rmse",
                                 "own_mod_obs", "own_biomass_ratio", "at_cap")]
  }))
  base_rmse <- res$total_rmse[res$mult == 1][1]
  best$d_total <- best$total_rmse - base_rmse
  print(data.frame(gear = best$gear, mult = signif(best$mult, 3),
                   q = signif(best$q, 3), total = round(best$total_rmse, 4),
                   d_total = round(best$d_total, 4),
                   own = round(best$own_rmse, 3),
                   mod_obs = signif(best$own_mod_obs, 3),
                   endB = signif(best$own_biomass_ratio, 3),
                   at_cap = best$at_cap)[order(best$d_total), ], row.names = FALSE)
  cat("\n")
}

# --------------------------------------------------------------- collect -----
fs <- sort(list.files(OUT, pattern = "^res_\\d+\\.rds$", full.names = TRUE))
if (length(fs)) {
  D <- do.call(rbind, lapply(fs, readRDS))
  saveRDS(D, file.path(cf_out_dir(), "02_q_response.rds"))
  write.csv(D, file.path(cf_out_dir(), "02_q_response.csv"), row.names = FALSE)
  cf_log("collected ", nrow(D), " evaluations from ", length(fs), " members")
}
cf_log("total elapsed ", round((proc.time() - t_start_all)[["elapsed"]] / 60, 1), " min")
