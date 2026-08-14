# =============================================================================
# Stage 2a summary: what each reproduction lever actually bought.
#
# The decisive column is `unf_growth` -- how much the UNFISHED population grows
# over 1841-2010 under the change.  A configuration that fits the catch only by
# growing the unfished stock 8x has not fixed the fit; it has moved the model to
# a different pre-exploitation abundance, which is a recalibration, not a tuning.
#
#   Rscript R/catch_fit/08_repro_summary.R
# =============================================================================

source("R/catch_fit/01_harness.R")

D <- readRDS(file.path(cf_out_dir(), "04_repro_sweep.rds"))

# the k = 1 R_max rows ARE the no-change control (same q ladder, same code path)
ctl <- D[D$lever == "R_max" & D$level == 1, ]
ctl_best <- do.call(rbind, lapply(
  split(ctl, list(ctl$sim_index, ctl$group), drop = TRUE),
  function(d) d[which.min(d$own_rmse), ]))
ck <- paste(ctl_best$sim_index, ctl_best$group)

best_of <- function(sub) {
  if (!nrow(sub)) return(NULL)
  b <- do.call(rbind, lapply(split(sub, list(sub$sim_index, sub$group), drop = TRUE),
                             function(d) d[which.min(d$own_rmse), ]))
  k <- paste(b$sim_index, b$group)
  b$own0    <- ctl_best$own_rmse[match(k, ck)]
  b$modobs0 <- ctl_best$own_mod_obs[match(k, ck)]
  b$total0  <- ctl_best$total_rmse[match(k, ck)]
  b[order(b$group, b$sim_index), ]
}

show <- function(b, title) {
  cat("\n### ", title, "\n", sep = "")
  print(data.frame(
    sim = b$sim_index, group = b$group, level = signif(b$level, 4),
    q = signif(b$q, 3),
    own = paste0(round(b$own0, 3), "->", round(b$own_rmse, 3)),
    mod_obs = paste0(signif(b$modobs0, 3), "->", signif(b$own_mod_obs, 3)),
    unf_growth = signif(b$unfished_growth, 4),
    depletion = signif(b$depletion, 3),
    total = paste0(round(b$total0, 4), "->", round(b$total_rmse, 4)),
    erepro = signif(b$erepro, 3), R_max = signif(b$R_max, 4),
    rl = round(b$repro_level, 3)), row.names = FALSE)
}

cat("=== Stage 2a: reproduction, starting from the Stage 1b catchability optimum ===\n")
cat("control = R_max x1 (no reproduction change), best over the same q ladder\n")

show(best_of(D[D$lever == "repro_level", ]),
     "LEVER B -- reproduction_level at fixed RDD (steady state preserved)")
show(best_of(D[D$lever == "R_max" & D$level > 1, ]),
     "LEVER A -- R_max multiplier at fixed erepro")

# Can the observed level be reached, and at what cost in unfished abundance?
cat("\n### can the observed catch be MATCHED, and what does it cost?\n")
cat("closest configuration to modelled/observed = 1, per member and group\n")
m <- do.call(rbind, lapply(split(D, list(D$sim_index, D$group), drop = TRUE),
  function(d) d[which.min(abs(log10(pmax(d$own_mod_obs, 1e-300)))), ]))
m <- m[order(m$group, m$sim_index), ]
print(data.frame(sim = m$sim_index, group = m$group, lever = m$lever,
                 level = signif(m$level, 4), q = signif(m$q, 3),
                 mod_obs = signif(m$own_mod_obs, 3),
                 own = round(m$own_rmse, 3),
                 unf_growth = signif(m$unfished_growth, 4),
                 erepro = signif(m$erepro, 3), R_max = signif(m$R_max, 4),
                 maxErepro19 = round(m$max_erepro_all, 4),
                 rmaxInf19 = m$n_rmax_inf_all), row.names = FALSE)

cat("\n### admissibility of every configuration evaluated\n")
cat("  configurations:            ", nrow(D), "\n")
cat("  with erepro >= 1 anywhere: ", sum(D$max_erepro_all >= 1), "\n")
cat("  with a NEW R_max = Inf:    ", sum(D$n_rmax_inf_all >
                                          ave(D$n_rmax_inf_all, D$sim_index,
                                              FUN = min)), "\n")
cat("  max erepro seen (all 19):  ", round(max(D$max_erepro_all), 4), "\n")
cat("=> every configuration in this sweep is admissible on erepro and R_max;\n")
cat("   what binds is the unfished abundance it implies, not the constraints.\n")

write.csv(D, file.path(cf_out_dir(), "08_repro_summary_full.csv"), row.names = FALSE)
