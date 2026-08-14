# =============================================================================
# Build the deliverable tables from whatever stages have completed.
#
# The central output is the ATTAINABILITY VERDICT per group, which is the
# question the brief actually asks.  It is decided from the 1-D response surface
# by three measurable properties, not by eyeballing:
#
#   reach       max modelled/observed cumulative catch anywhere in q in (0,1]
#   elasticity  d log(modelled catch) / d log(q) at the TOP of the scan.
#               ~1 means catch still scales with effort -- the population is not
#               the limit.  << 1 means the population is being mined and extra
#               catchability buys almost nothing.
#   depletion   smallest end/start biomass reached in the scan
#
# giving:
#   solved_by_q        the own-RMSE optimum is a real improvement and lands at
#                      modelled/observed ~ 1
#   overfished_in_model  the model catches MORE than observed; the fix is less q
#   timing_mismatch    the observed LEVEL is reachable but the fit gets worse
#                      when you reach it -- the error is in when, not how much,
#                      and no scalar catchability can fix it
#   population_limited catch cannot reach the observed level, elasticity has
#                      collapsed and biomass is being depleted -> the level-2
#                      reproduction question
#   cap_limited        catch cannot reach it inside the box, but elasticity is
#                      still ~1 -- q > 1 would get there
#
#   Rscript R/catch_fit/06_report.R
# Env: CF_ARM
# =============================================================================

source("R/catch_fit/01_harness.R")

D <- cf_out_dir()
say <- function(...) cat(..., "\n", sep = "")

# Read the per-member checkpoints rather than the collected file, so the report
# works while 02_q_response.R is still running.
rf <- sort(list.files(cf_out_dir("q_response"),
                      pattern = "^(res|ext)_\\d+\\.rds$", full.names = TRUE))
if (!length(rf)) stop("no response-surface checkpoints in ", cf_out_dir("q_response"))
R <- do.call(rbind, lapply(rf, function(f) {
  z <- readRDS(f); z[, intersect(names(z), names(readRDS(rf[1])))]
}))
R <- R[!duplicated(R[, c("sim_index", "gear", "q")]), ]
members <- sort(unique(R$sim_index))
say("=== arm ", CF_ARM, " | ", length(members), " members: ",
    paste(members, collapse = ", "), " ===")

# --------------------------------------------------------- 1. before ---------
base_f <- file.path(D, "00_baseline_per_species.csv")
BASE <- if (file.exists(base_f)) read.csv(base_f, check.names = FALSE) else NULL

if (!is.null(BASE)) {
  say("\n### 1. BEFORE -- per-species error, all members")
  w <- reshape(BASE[, c("sim_index", "Species", "rmse")], idvar = "Species",
               timevar = "sim_index", direction = "wide")
  names(w) <- sub("^rmse\\.", "m", names(w))
  sse <- reshape(BASE[, c("sim_index", "Species", "sse_pct")], idvar = "Species",
                 timevar = "sim_index", direction = "wide")
  w$mean_sse_pct <- round(rowMeans(sse[match(w$Species, sse$Species), -1]), 1)
  w[, -1] <- lapply(w[, -1], function(z) round(z, 3))
  print(w[order(-w$mean_sse_pct), ], row.names = FALSE)
  say("\ntotal RMSE per member:")
  tt <- unique(BASE[, c("sim_index", "total_rmse")])
  print(data.frame(sim = tt$sim_index, rmse = round(tt$total_rmse, 4)),
        row.names = FALSE)
}

# ------------------------------------------- 2. response surface + verdict ---
say("\n### 2. CATCHABILITY RESPONSE SURFACE -- per-gear optima and verdict")

MAT <- 0.03   # RMSE units that count as a material change in a group's own fit

classify <- function(d) {
  d <- d[order(d$q), ]
  base  <- d[abs(d$mult - 1) < 1e-12, ][1, ]
  k_tot <- which.min(d$total_rmse)      # best for the POOLED objective
  k_own <- which.min(d$own_rmse)        # best for this group alone
  reach <- max(d$own_mod_obs, na.rm = TRUE)

  # the q that best matches the observed LEVEL, and what the fit is worth there
  k_mat <- which.min(abs(log10(pmax(d$own_mod_obs, 1e-300))))

  # elasticity of catch wrt q at the TOP of the scan: ~1 means catch still
  # scales with effort, << 1 means the population, not q, is the limit
  n  <- nrow(d)
  el <- if (n >= 2 && d$q[n] > d$q[n - 1] && all(d$own_mod_obs[c(n - 1, n)] > 0))
    diff(log10(d$own_mod_obs[c(n - 1, n)])) / diff(log10(d$q[c(n - 1, n)]))
    else NA_real_

  attainable <- reach >= 1
  verdict <-
    if (!attainable && !is.na(el) && el < 0.5)          "population_limited"
    else if (!attainable)                               "cap_limited"
    else if (d$own_rmse[k_mat] <= base$own_rmse - MAT)  "solved_by_q"
    else if (d$own_rmse[k_own] <= base$own_rmse - MAT)  "partially_improved"
    else                                                "timing_mismatch"

  data.frame(
    sim_index = d$sim_index[1], gear = d$gear[1],
    q_baseline = base$q, own_rmse_base = base$own_rmse,
    mod_obs_base = base$own_mod_obs,
    direction = if (d$mult[k_own] < 1) "lower q" else
                if (d$mult[k_own] > 1) "raise q" else "no change",
    q_best_total = d$q[k_tot], mult_best_total = d$mult[k_tot],
    total_at_best = d$total_rmse[k_tot], total_base = base$total_rmse,
    d_total = d$total_rmse[k_tot] - base$total_rmse,
    q_best_own = d$q[k_own], mult_best_own = d$mult[k_own],
    own_rmse_best = d$own_rmse[k_own], mod_obs_at_own_best = d$own_mod_obs[k_own],
    q_level_match = d$q[k_mat], own_rmse_at_level_match = d$own_rmse[k_mat],
    mod_obs_at_level_match = d$own_mod_obs[k_mat],
    reach_max_mod_obs = reach, attainable_in_box = attainable, elasticity_top = el,
    min_biomass_ratio = min(d$own_biomass_ratio, na.rm = TRUE),
    biomass_at_own_best = d$own_biomass_ratio[k_own],
    biomass_at_level_match = d$own_biomass_ratio[k_mat],
    verdict = verdict, stringsAsFactors = FALSE)
}

V <- do.call(rbind, lapply(split(R, list(R$sim_index, R$gear), drop = TRUE), classify))
V <- V[order(V$sim_index, V$d_total), ]
write.csv(V, file.path(D, "06_gear_verdicts.csv"), row.names = FALSE)

for (si in members) {
  d <- V[V$sim_index == si, ]
  say("\n--- member ", si, " (baseline RMSE ", round(d$total_base[1], 4), ") ---")
  print(data.frame(
    gear = d$gear,
    q0 = signif(d$q_baseline, 3),
    own = paste0(round(d$own_rmse_base, 2), "->", round(d$own_rmse_best, 2)),
    at_mult = signif(d$mult_best_own, 3),
    dir = d$direction,
    modobs0 = signif(d$mod_obs_base, 3),
    reach = signif(d$reach_max_mod_obs, 3),
    elast = round(d$elasticity_top, 2),
    own_if_matched = round(d$own_rmse_at_level_match, 2),
    minB = signif(d$min_biomass_ratio, 3),
    d_total = round(d$d_total, 4),
    verdict = d$verdict, check.names = FALSE), row.names = FALSE)
}

say("\n### verdict frequency across members")
print(table(V$gear, V$verdict))

# ---------------------------------------------------------- 3. after ---------
opt_f <- file.path(D, "03_q_optimum_summary.csv")
if (file.exists(opt_f)) {
  say("\n### 3. AFTER -- joint catchability optimum")
  S <- read.csv(opt_f)
  print(data.frame(sim = S$sim_index,
                   before = round(S$rmse_before, 4),
                   after = round(S$rmse_after, 4),
                   pct = round(S$pct_improvement, 1),
                   winner = S$winner,
                   starts_agree_dex = round(S$start_agreement_dex, 2),
                   worst_endB = signif(S$min_biomass_ratio, 3),
                   worst_group = S$min_biomass_group), row.names = FALSE)

  P <- read.csv(file.path(D, "03_q_optimum_per_species.csv"), check.names = FALSE)
  say("\nper-species, before -> after (RMSE):")
  P$lab <- paste0(round(P$rmse, 3))
  w <- reshape(P[, c("sim_index", "Species", "rmse")], idvar = "Species",
               timevar = "sim_index", direction = "wide")
  names(w) <- sub("^rmse\\.", "m", names(w))
  if (!is.null(BASE)) {
    b <- reshape(BASE[, c("sim_index", "Species", "rmse")], idvar = "Species",
                 timevar = "sim_index", direction = "wide")
    names(b) <- sub("^rmse\\.", "m", names(b))
    out <- data.frame(Species = w$Species)
    for (cn in setdiff(names(w), "Species"))
      out[[cn]] <- paste0(round(b[match(w$Species, b$Species), cn], 2), "->",
                          round(w[[cn]], 2))
    print(out, row.names = FALSE)
  } else print(w, row.names = FALSE)

  cap_f <- file.path(D, "03_cap_diagnostic.csv")
  if (file.exists(cap_f)) {
    say("\n### 3c. CAP DIAGNOSTIC -- q > 1, OUTSIDE the admissible box")
    print(read.csv(cap_f), row.names = FALSE)
  }
}

# ----------------------------------------------------- 4. reproduction -------
rep_f <- file.path(D, "04_repro_sweep.rds")
if (file.exists(rep_f)) {
  say("\n### 4. REPRODUCTION SWEEP")
  Rr <- readRDS(rep_f)
  for (si in sort(unique(Rr$sim_index))) for (g in unique(Rr$group)) {
    d <- Rr[Rr$sim_index == si & Rr$group == g, ]
    if (!nrow(d)) next
    say("\n--- member ", si, " / ", g, " ---")
    print(data.frame(lever = d$lever, level = signif(d$level, 4),
                     q = signif(d$q, 3), erepro = signif(d$erepro, 3),
                     R_max = signif(d$R_max, 4), rl = round(d$repro_level, 4),
                     total = round(d$total_rmse, 4), own = round(d$own_rmse, 3),
                     modobs = signif(d$own_mod_obs, 3),
                     depl = signif(d$depletion, 3),
                     unf_growth = signif(d$unfished_growth, 3)), row.names = FALSE)
  }
  say("\nbest configuration per member/group by OWN rmse:")
  bb <- do.call(rbind, lapply(split(Rr, list(Rr$sim_index, Rr$group), drop = TRUE),
    function(d) d[which.min(d$own_rmse), ]))
  print(data.frame(sim = bb$sim_index, group = bb$group, lever = bb$lever,
                   level = signif(bb$level, 4), q = signif(bb$q, 3),
                   own = round(bb$own_rmse, 3), modobs = signif(bb$own_mod_obs, 3),
                   total = round(bb$total_rmse, 4),
                   erepro = signif(bb$erepro, 3), R_max = signif(bb$R_max, 4),
                   maxErepro19 = round(bb$max_erepro_all, 4),
                   rmaxInf19 = bb$n_rmax_inf_all), row.names = FALSE)
}

say("\nwrote 06_gear_verdicts.csv to ", D)
