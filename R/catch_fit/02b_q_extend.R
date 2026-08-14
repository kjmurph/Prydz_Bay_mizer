# =============================================================================
# STAGE 1a (continued) -- extend any response curve whose optimum sits on a grid
# EDGE.
#
# The Stage 1a grid spans +-3 dex around each member's own q.  A minimum found at
# the extreme low multiplier is not a minimum, it is the edge of the box, and
# reporting it as an optimum would understate what catchability can do.  Member
# 446's bathypelagic fishes is exactly this: monotone decreasing to the floor,
# still 553x over-catching there.
#
# Extends downward in 0.5-dex steps until the pooled RMSE has failed to improve
# twice in a row, or MAX_EXTRA_DEX is used up.  Upper edges are extended the same
# way but only while q stays inside the admissible box -- an optimum pinned at
# q = 1 is a genuine cap, and belongs to the 1c diagnostic in 03, not here.
#
# USAGE
#   Rscript R/catch_fit/02b_q_extend.R
# Env: CF_CORES (default 1)
# =============================================================================

source("R/catch_fit/01_harness.R")

STEP_DEX      <- 0.5
MAX_EXTRA_DEX <- 12
PATIENCE      <- 2

OUT <- cf_out_dir("q_response")
resp_file <- file.path(cf_out_dir(), "02_q_response.rds")
if (!file.exists(resp_file)) stop("run 02_q_response.R first")
RESP <- readRDS(resp_file)

cf_log("=== Stage 1a extension: edge optima ===")

# which (member, gear) pairs have their optimum on an edge?
edges <- do.call(rbind, lapply(split(RESP, list(RESP$sim_index, RESP$gear),
                                     drop = TRUE), function(d) {
  k <- which.min(d$total_rmse)
  data.frame(sim_index = d$sim_index[1], gear = d$gear[1],
             at_low  = d$mult[k] <= min(d$mult) + 1e-12,
             at_high = d$mult[k] >= max(d$mult) - 1e-12 && !d$at_cap[k],
             best_mult = d$mult[k], best_q = d$q[k],
             best_rmse = d$total_rmse[k], stringsAsFactors = FALSE)
}))
edges <- edges[edges$at_low | edges$at_high, ]

if (!nrow(edges)) {
  cf_log("no edge optima -- nothing to extend")
  quit(save = "no")
}
cf_log(nrow(edges), " edge optima to extend:")
print(data.frame(sim = edges$sim_index, gear = edges$gear,
                 dir = ifelse(edges$at_low, "down", "up"),
                 mult = signif(edges$best_mult, 3),
                 rmse = round(edges$best_rmse, 4)), row.names = FALSE)

members <- cf_load_members(unique(edges$sim_index))
res <- list()

for (m in members) {
  si <- m$sim_index
  f  <- file.path(OUT, sprintf("ext_%04d.rds", si))
  if (file.exists(f)) { cf_log("member ", si, " extension done, skipping"); next }
  PARAMS <- m$params
  CACHE  <- cf_make_cache(PARAMS)
  q0_all <- cf_get_q(PARAMS)
  e_m <- edges[edges$sim_index == si, ]
  rows <- list()

  for (r in seq_len(nrow(e_m))) {
    g   <- e_m$gear[r]
    dir <- if (e_m$at_low[r]) -1 else 1
    best <- e_m$best_rmse[r]; bad <- 0L
    cf_log("  member ", si, " / ", g, ": extending ",
           if (dir < 0) "down" else "up", " from mult ",
           signif(e_m$best_mult[r], 3))
    for (s in seq_len(MAX_EXTRA_DEX / STEP_DEX)) {
      mult <- e_m$best_mult[r] * 10^(dir * s * STEP_DEX)
      q    <- min(1, q0_all[[g]] * mult)
      if (dir > 0 && q >= 1) break        # a cap, not an edge -- 1c handles it
      if (q <= 0) break
      qv <- setNames(q, g)
      sc <- cf_eval_q(PARAMS, qv, cache = CACHE, cap = 1)
      ps <- sc$per_species
      rows[[length(rows) + 1]] <- data.frame(
        sim_index = si, gear = g, mult = mult, q = q,
        q_baseline = q0_all[[g]], at_cap = FALSE,
        total_rmse = sc$rmse,
        own_rmse = ps$rmse[match(g, ps$Species)],
        own_sse_pct = ps$sse_pct[match(g, ps$Species)],
        own_mod_obs = ps$mod_obs_ratio[match(g, ps$Species)],
        own_biomass_ratio = unname(sc$biomass_ratio[g]),
        min_biomass_ratio = min(sc$biomass_ratio),
        min_biomass_group = names(sc$biomass_ratio)[which.min(sc$biomass_ratio)],
        setNames(as.list(ps$rmse), paste0("rmse_", gsub(" ", "_", ps$Species))),
        stringsAsFactors = FALSE, check.names = FALSE)
      cf_log(sprintf("    mult %.3g  q %.3g  total %.5f  own %.3f  modobs %.3g",
                     mult, q, sc$rmse, ps$rmse[match(g, ps$Species)],
                     ps$mod_obs_ratio[match(g, ps$Species)]))
      if (sc$rmse < best - 1e-10) { best <- sc$rmse; bad <- 0L } else bad <- bad + 1L
      if (bad >= PATIENCE) break
    }
  }
  out <- do.call(rbind, rows)
  saveRDS(out, f)
  res[[as.character(si)]] <- out
}

# merge extensions into the response surface
fs <- sort(list.files(OUT, pattern = "^ext_\\d+\\.rds$", full.names = TRUE))
if (length(fs)) {
  E <- do.call(rbind, lapply(fs, readRDS))
  ALL <- rbind(RESP, E[, names(RESP)])
  # the merge target is also the merge source, so re-running this script would
  # otherwise duplicate every extension point
  ALL <- ALL[!duplicated(ALL[, c("sim_index", "gear", "q")]), ]
  ALL <- ALL[order(ALL$sim_index, ALL$gear, ALL$q), ]
  saveRDS(ALL, file.path(cf_out_dir(), "02_q_response.rds"))
  write.csv(ALL, file.path(cf_out_dir(), "02_q_response.csv"), row.names = FALSE)
  cf_log("merged ", nrow(E), " extension points; response surface now ",
         nrow(ALL), " rows")

  cat("\n=== per-gear optima after extension ===\n")
  b <- do.call(rbind, lapply(split(ALL, list(ALL$sim_index, ALL$gear), drop = TRUE),
    function(d) {
      k <- which.min(d$total_rmse)
      base <- d$total_rmse[abs(d$mult - 1) < 1e-12][1]
      data.frame(sim = d$sim_index[1], gear = d$gear[1],
                 mult = signif(d$mult[k], 3), q = signif(d$q[k], 3),
                 total = round(d$total_rmse[k], 4),
                 d_total = round(d$total_rmse[k] - base, 4),
                 own = round(d$own_rmse[k], 3),
                 mod_obs = signif(d$own_mod_obs[k], 3),
                 stringsAsFactors = FALSE)
    }))
  print(b[order(b$sim, b$d_total), ], row.names = FALSE)
}
