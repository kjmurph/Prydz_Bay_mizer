# =============================================================================
# STAGE 1b -- joint catchability optimisation, and 1c the cap diagnostic.
#
# Coordinate descent on log10(q) over the 9 fished gears, inside the pipeline's
# admissible box q in (0, 1].  Deliberate and structured, not sampled: each
# coordinate is refined on a shrinking local grid (+-0.5, then +-0.25, then
# +-0.125 dex), so the final resolution is ~0.06 dex, about 15% in q.
#
# TWO STARTS, because a single descent cannot tell a global optimum from a local
# one:
#   "grid"     the per-gear 1-D optima from Stage 1a  (the informed start)
#   "baseline" the member's own Monte Carlo q          (the incumbent)
# If the two converge to the same point the surface is effectively unimodal over
# this box; if not, that is itself a reportable property of the response surface.
#
# 1c CAP DIAGNOSTIC.  For any gear whose optimum lands on q = 1, the box
# constraint is binding and the fit cannot say whether the observed catch is
# merely out of reach of the pipeline's clamp or unattainable at any q.  Those
# gears get a second, UNBOUNDED 1-D scan to q = 100.  Those results are labelled
# outside the admissible set and are never folded into the reported optimum.
#
# USAGE
#   Rscript R/catch_fit/03_q_optimise.R            # all 5 members
#   Rscript R/catch_fit/03_q_optimise.R 446
# Env: CF_CORES (default 1)
# =============================================================================

source("R/catch_fit/01_harness.R")
suppressPackageStartupMessages(library(parallel))

args <- commandArgs(trailingOnly = TRUE)
members_wanted <- if (length(args)) as.integer(args) else CF_MEMBERS

OUT   <- cf_out_dir("q_optimum")
CORES <- cf_cores()
REFINE <- c(0.5, 0.25, 0.125)     # dex half-widths, one per sweep
Q_CAP  <- 1                        # the admissible box
CAP_DIAG_MAX <- 100                # 1c only, explicitly outside the box

resp_file <- file.path(cf_out_dir(), "02_q_response.rds")
if (!file.exists(resp_file))
  stop("Stage 1a output missing: ", resp_file, " -- run 02_q_response.R first")
RESP <- readRDS(resp_file)

# ------------------------------------------------------------- evaluation ----
# PARALLELISM IS ACROSS MEMBERS, not inside the line search.  A coordinate step
# only offers 4 candidates, so parallelising there caps the speed-up at 4x and
# pays cluster-setup cost hundreds of times over.  Members are independent and
# there are 7 of them, so one worker per member is both simpler and faster.

#' Score a full q vector against the member held in `params` / `cache`.
cf_obj <- function(qv, params, cache, cap = Q_CAP) {
  sc <- cf_eval_q(params, qv, cache = cache, cap = cap)
  list(rmse = sc$rmse, q = sc$q,
       min_biomass_ratio = min(sc$biomass_ratio),
       min_biomass_group = names(sc$biomass_ratio)[which.min(sc$biomass_ratio)],
       per_species = sc$per_species)
}

cf_obj_many <- function(qlist, params, cache, cap = Q_CAP)
  lapply(qlist, function(q) cf_obj(q, params, cache, cap))

# ------------------------------------------------------- coordinate descent --
cf_descend <- function(q_start, gears, label, params, cache,
                       cap = Q_CAP, log_every = TRUE) {
  cur <- q_start
  r0  <- cf_obj(cur, params, cache, cap)
  cur <- r0$q                       # canonicalise through the same clamp
  best <- r0$rmse
  trace <- data.frame(sweep = 0L, gear = NA_character_, rmse = best,
                      n_eval = 1L, stringsAsFactors = FALSE)
  n_eval <- 1L

  for (s in seq_along(REFINE)) {
    h <- REFINE[s]
    improved_this_sweep <- 0
    for (g in gears) {
      if (cur[[g]] <= 0) next
      offs <- c(-h, -h / 2, h / 2, h)
      cand <- lapply(offs, function(o) {
        q <- cur; q[[g]] <- min(cap, cur[[g]] * 10^o); q
      })
      # drop candidates that clamp onto the current value (no new information)
      keep <- vapply(cand, function(q) abs(q[[g]] - cur[[g]]) > 1e-15, logical(1))
      cand <- cand[keep]
      if (!length(cand)) next
      rs <- cf_obj_many(cand, params, cache, cap)
      n_eval <- n_eval + length(cand)
      rm_v <- vapply(rs, function(z) z$rmse, numeric(1))
      k <- which.min(rm_v)
      if (rm_v[k] < best - 1e-10) {
        improved_this_sweep <- improved_this_sweep + (best - rm_v[k])
        best <- rm_v[k]
        cur  <- rs[[k]]$q
      }
      trace <- rbind(trace, data.frame(sweep = s, gear = g, rmse = best,
                                       n_eval = n_eval, stringsAsFactors = FALSE))
    }
    if (log_every)
      cf_log(sprintf("  [%s] sweep %d (h=%.3f dex): RMSE %.5f  (gained %.5f, %d evals)",
                     label, s, h, best, improved_this_sweep, n_eval))
    if (improved_this_sweep < 1e-5 && s < length(REFINE)) {
      if (log_every) cf_log("  [", label, "] converged early")
      break
    }
  }
  final <- cf_obj(cur, params, cache, cap)
  list(q = final$q, rmse = final$rmse, trace = trace, n_eval = n_eval,
       min_biomass_ratio = final$min_biomass_ratio,
       min_biomass_group = final$min_biomass_group,
       per_species = final$per_species)
}

# ----------------------------------------------------------------- run -------
cf_log("=== Stage 1b: joint q optimisation ===")
cf_log("members ", paste(members_wanted, collapse = ", "), " / cores ", CORES,
       " / refine ", paste(REFINE, collapse = ", "), " dex")

members <- cf_load_members(members_wanted)
fished  <- cf_fished_species()

todo <- Filter(function(m)
  !file.exists(file.path(OUT, sprintf("opt_%04d.rds", m$sim_index))), members)
cf_log(length(todo), " members to optimise (",
       length(members) - length(todo), " already done)")

optimise_member <- function(m) {
  suppressPackageStartupMessages({ library(therMizer); library(mizer) })
  si <- m$sim_index
  f  <- file.path(OUT, sprintf("opt_%04d.rds", si))
  params <- m$params
  cache  <- cf_make_cache(params)
  q_base <- cf_get_q(params)

  # informed start: each gear at its own 1-D optimum from Stage 1a
  r <- RESP[RESP$sim_index == si, ]
  if (!nrow(r)) stop("no Stage 1a response for member ", si)
  q_grid <- q_base
  for (g in unique(r$gear)) {
    d <- r[r$gear == g, ]
    q_grid[[g]] <- d$q[which.min(d$total_rmse)]
  }

  t0 <- proc.time()
  quiet <- CORES > 1        # interleaved logs from many workers are unreadable
  o_grid <- cf_descend(q_grid, fished, paste0(si, "/grid"), params, cache,
                       log_every = !quiet)
  o_base <- cf_descend(q_base, fished, paste0(si, "/base"), params, cache,
                       log_every = !quiet)

  winner <- if (o_grid$rmse <= o_base$rmse) "grid" else "baseline"
  o_best <- if (winner == "grid") o_grid else o_base
  agree  <- max(abs(log10(pmax(o_grid$q[fished], 1e-300)) -
                    log10(pmax(o_base$q[fished], 1e-300))))

  # ---- 1c: which gears are pinned at the cap, and what lies beyond
  capped <- fished[abs(o_best$q[fished] - Q_CAP) < 1e-12]
  cap_diag <- NULL
  if (length(capped)) {
    cap_diag <- do.call(rbind, lapply(capped, function(g) {
      qs <- Q_CAP * 10^seq(0, log10(CAP_DIAG_MAX), length.out = 9)
      cand <- lapply(qs, function(v) { q <- o_best$q; q[[g]] <- v; q })
      rs <- cf_obj_many(cand, params, cache, cap = Inf)
      data.frame(sim_index = si, gear = g, q = qs,
                 total_rmse = vapply(rs, function(z) z$rmse, numeric(1)),
                 own_rmse = vapply(rs, function(z)
                   z$per_species$rmse[match(g, z$per_species$Species)], numeric(1)),
                 own_mod_obs = vapply(rs, function(z)
                   z$per_species$mod_obs_ratio[match(g, z$per_species$Species)], numeric(1)),
                 min_biomass_ratio = vapply(rs, function(z) z$min_biomass_ratio, numeric(1)),
                 min_biomass_group = vapply(rs, function(z) z$min_biomass_group, character(1)),
                 stringsAsFactors = FALSE)
    }))
  }

  el <- (proc.time() - t0)[["elapsed"]]
  saveRDS(list(sim_index = si, q_baseline = q_base, q_grid_start = q_grid,
               grid = o_grid, baseline = o_base, winner = winner,
               start_agreement_dex = agree, capped_gears = capped,
               cap_diagnostic = cap_diag, elapsed_min = el / 60), f)
  sprintf("member %d: %.5f -> %.5f (%s start), starts agree to %.2f dex, capped {%s}, %.1f min",
          si, r$total_rmse[abs(r$mult - 1) < 1e-12][1], o_best$rmse, winner,
          agree, paste(capped, collapse = ","), el / 60)
}

if (length(todo)) {
  t0 <- proc.time()
  if (CORES > 1 && length(todo) > 1) {
    cl <- makeCluster(min(CORES, length(todo)))
    # PSOCK workers do not inherit the master's working directory, and every
    # path in the harness is repo-relative.  Set it before sourcing anything.
    wd <- getwd()
    clusterExport(cl, "wd", envir = environment())
    clusterEvalQ(cl, setwd(wd))
    clusterExport(cl, c("RESP", "OUT", "fished", "REFINE", "Q_CAP",
                        "CAP_DIAG_MAX", "CORES"), envir = environment())
    clusterEvalQ(cl, source("R/catch_fit/01_harness.R"))
    clusterExport(cl, c("cf_obj", "cf_obj_many", "cf_descend"),
                  envir = environment())
    msgs <- tryCatch(parLapplyLB(cl, todo, optimise_member),
                     finally = stopCluster(cl))
  } else {
    msgs <- lapply(todo, optimise_member)
  }
  for (s in msgs) cf_log(s)
  cf_log("Stage 1b elapsed ",
         round((proc.time() - t0)[["elapsed"]] / 60, 1), " min")
}

# per-member optimum tables
for (f in sort(list.files(OUT, pattern = "^opt_\\d+\\.rds$", full.names = TRUE))) {
  z <- readRDS(f)
  o <- if (z$winner == "grid") z$grid else z$baseline
  ps <- o$per_species
  cat(sprintf("\n--- member %d optimum (%s start), RMSE %.4f ---\n",
              z$sim_index, z$winner, o$rmse))
  print(data.frame(gear = fished,
                   q_before = signif(z$q_baseline[fished], 3),
                   q_after  = signif(o$q[fished], 3),
                   dex      = round(log10(o$q[fished] / z$q_baseline[fished]), 2),
                   rmse_after = round(ps$rmse[match(fished, ps$Species)], 3),
                   mod_obs_after = signif(ps$mod_obs_ratio[match(fished, ps$Species)], 3)),
        row.names = FALSE)
  cat(sprintf("worst end/start biomass: %s = %.4g\n",
              o$min_biomass_group, o$min_biomass_ratio))
}

# --------------------------------------------------------------- collect -----
fs <- sort(list.files(OUT, pattern = "^opt_\\d+\\.rds$", full.names = TRUE))
if (length(fs)) {
  L <- lapply(fs, readRDS)
  saveRDS(L, file.path(cf_out_dir(), "03_q_optimum.rds"))

  summ <- do.call(rbind, lapply(L, function(z) {
    o <- if (z$winner == "grid") z$grid else z$baseline
    data.frame(sim_index = z$sim_index,
               rmse_before = NA_real_, rmse_after = o$rmse,
               winner = z$winner, start_agreement_dex = z$start_agreement_dex,
               n_capped = length(z$capped_gears),
               capped = paste(z$capped_gears, collapse = ";"),
               min_biomass_ratio = o$min_biomass_ratio,
               min_biomass_group = o$min_biomass_group,
               stringsAsFactors = FALSE)
  }))
  base <- read.csv(file.path(cf_out_dir(), "00_baseline_per_species.csv"))
  summ$rmse_before <- base$total_rmse[match(summ$sim_index, base$sim_index)]
  summ$pct_improvement <- 100 * (summ$rmse_before - summ$rmse_after) / summ$rmse_before
  write.csv(summ, file.path(cf_out_dir(), "03_q_optimum_summary.csv"), row.names = FALSE)

  per <- do.call(rbind, lapply(L, function(z) {
    o <- if (z$winner == "grid") z$grid else z$baseline
    cbind(sim_index = z$sim_index, o$per_species,
          q_before = z$q_baseline[o$per_species$Species],
          q_after  = o$q[o$per_species$Species])
  }))
  write.csv(per, file.path(cf_out_dir(), "03_q_optimum_per_species.csv"), row.names = FALSE)

  caps <- do.call(rbind, lapply(L, function(z) z$cap_diagnostic))
  if (!is.null(caps))
    write.csv(caps, file.path(cf_out_dir(), "03_cap_diagnostic.csv"), row.names = FALSE)

  cat("\n=== Stage 1b summary ===\n")
  print(data.frame(sim = summ$sim_index,
                   before = round(summ$rmse_before, 4),
                   after = round(summ$rmse_after, 4),
                   pct = round(summ$pct_improvement, 1),
                   winner = summ$winner,
                   agree_dex = round(summ$start_agreement_dex, 2),
                   capped = summ$capped), row.names = FALSE)
}
