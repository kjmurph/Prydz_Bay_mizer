# =============================================================================
# Re-derive every quantitative claim in docs/catch_fit_RESULTS.md straight from
# the saved outputs, so the document can be checked rather than trusted.
#
#   Rscript R/catch_fit/09_verify_claims.R
# =============================================================================

source("R/catch_fit/01_harness.R")
D <- cf_out_dir()
say <- function(...) cat(..., "\n", sep = "")

B <- read.csv(file.path(D, "00_baseline_per_species.csv"), check.names = FALSE)
V <- read.csv(file.path(D, "06_gear_verdicts.csv"))
S <- read.csv(file.path(D, "03_q_optimum_summary.csv"))
P <- read.csv(file.path(D, "03_q_optimum_per_species.csv"), check.names = FALSE)
R <- readRDS(file.path(D, "04_repro_sweep.rds"))
rank <- read.csv(CF_RANKING)

say("=== S3: SSE shares (mean over members) ===")
a <- aggregate(sse_pct ~ Species, B, mean)
a <- a[order(-a$sse_pct), ]
print(data.frame(Species = a$Species, mean_sse_pct = round(a$sse_pct, 1)),
      row.names = FALSE)
say("  toothfishes + shelf and coastal  = ",
    round(sum(a$sse_pct[a$Species %in% c("toothfishes",
                                         "shelf and coastal fishes")]), 1), "%")
say("  sperm + minke + baleen           = ",
    round(sum(a$sse_pct[a$Species %in% c("sperm whales", "minke whales",
                                         "baleen whales")]), 1), "%")

say("\n=== S3: per-species RMSE and mod/obs ranges across members ===")
rr <- do.call(rbind, lapply(split(B, B$Species), function(d) data.frame(
  Species = d$Species[1],
  rmse_min = min(d$rmse), rmse_max = max(d$rmse),
  modobs_min = min(d$mod_obs_ratio), modobs_max = max(d$mod_obs_ratio))))
print(data.frame(Species = rr$Species,
                 rmse = paste0(round(rr$rmse_min, 2), "-", round(rr$rmse_max, 2)),
                 mod_obs = paste0(signif(rr$modobs_min, 2), "-",
                                  signif(rr$modobs_max, 3))), row.names = FALSE)

say("\n=== S5: optimisation vs the whole Monte Carlo ===")
tt <- unique(B[, c("sim_index", "total_rmse")])
S$before <- tt$total_rmse[match(S$sim_index, tt$sim_index)]
S$rank <- rank$rank[match(S$sim_index, rank$sim_index)]
S <- S[order(S$rank), ]
print(data.frame(rank = S$rank, sim = S$sim_index,
                 before = round(S$before, 4), after = round(S$rmse_after, 4),
                 pct = round(100 * (S$before - S$rmse_after) / S$before, 1)),
      row.names = FALSE)
say("  best random draw of ", nrow(rank), ": ", round(min(rank$rmse), 4))
say("  worst optimised member:        ", round(max(S$rmse_after), 4))
say("  all optimised beat it:         ", all(S$rmse_after < min(rank$rmse)))
say("  rank the WORST optimised member would take: ",
    sum(rank$rmse < max(S$rmse_after)) + 1)

say("\n=== S4b: minke verdict, all members ===")
mk <- V[V$gear == "minke whales", ]
print(data.frame(sim = mk$sim_index, reach = signif(mk$reach_max_mod_obs, 3),
                 elast = round(mk$elasticity_top, 2),
                 own = paste0(round(mk$own_rmse_base, 2), "->",
                              round(mk$own_rmse_best, 2)),
                 verdict = mk$verdict), row.names = FALSE)
say("  population_limited in ", sum(mk$verdict == "population_limited"),
    " of ", nrow(mk))

say("\n=== S7: RDI/RDD headroom, every member, every fished group ===")
mem <- cf_load_members()
hd <- do.call(rbind, lapply(mem, function(m) {
  p <- m$params
  data.frame(sim_index = m$sim_index, species = cf_fished_species(),
             headroom = getRDI(p)[cf_fished_species()] /
                        getRDD(p)[cf_fished_species()])
}))
h <- aggregate(headroom ~ species, hd, function(z) c(min(z), median(z), max(z)))
h <- data.frame(species = h$species, min = h$headroom[, 1],
                median = h$headroom[, 2], max = h$headroom[, 3])
print(data.frame(species = h$species, min = signif(h$min, 4),
                 median = signif(h$median, 4), max = signif(h$max, 4))[
                   order(h$median), ], row.names = FALSE)
say("  minke has the smallest headroom of any fished group: ",
    h$species[which.min(h$median)] == "minke whales")

say("\n=== S7: lever A on minke -- best per member ===")
a5 <- R[R$group == "minke whales" & R$lever == "R_max" & R$level > 1, ]
bb <- do.call(rbind, lapply(split(a5, a5$sim_index),
                            function(d) d[which.min(d$own_rmse), ]))
c0 <- R[R$group == "minke whales" & R$lever == "R_max" & R$level == 1, ]
c0b <- do.call(rbind, lapply(split(c0, c0$sim_index),
                             function(d) d[which.min(d$own_rmse), ]))
bb$own0 <- c0b$own_rmse[match(bb$sim_index, c0b$sim_index)]
bb$mo0  <- c0b$own_mod_obs[match(bb$sim_index, c0b$sim_index)]
print(data.frame(sim = bb$sim_index, k = bb$level, R_max = signif(bb$R_max, 4),
                 rl = round(bb$repro_level, 3),
                 own = paste0(round(bb$own0, 2), "->", round(bb$own_rmse, 2)),
                 mod_obs = paste0(signif(bb$mo0, 2), "->",
                                  signif(bb$own_mod_obs, 2)),
                 unf_growth = signif(bb$unfished_growth, 3),
                 depletion = signif(bb$depletion, 3),
                 erepro = signif(bb$erepro, 4)), row.names = FALSE)
say("  minke erepro unchanged across all lever-A rows: ",
    length(unique(round(a5$erepro, 10))) == 1, " (value ",
    signif(a5$erepro[1], 4), ")")
say("  depletion range at these optima: ",
    round(min(bb$depletion), 3), " - ", round(max(bb$depletion), 3))
say("  unfished growth range: ", round(min(bb$unfished_growth), 2), " - ",
    round(max(bb$unfished_growth), 2))

say("\n=== S7: admissibility over all 531 configurations ===")
say("  configurations:            ", nrow(R))
say("  any erepro >= 1:           ", sum(R$max_erepro_all >= 1))
say("  max erepro (all 19):       ", round(max(R$max_erepro_all), 4))
say("  any non-finite R_max seen: ", sum(!is.finite(R$R_max)))
base_inf <- ave(R$n_rmax_inf_all, R$sim_index, FUN = min)
say("  NEW R_max = Inf vs member baseline: ", sum(R$n_rmax_inf_all > base_inf))

say("\n=== S7: exact level match for minke ===")
mm <- R[R$group == "minke whales", ]
em <- do.call(rbind, lapply(split(mm, mm$sim_index),
  function(d) d[which.min(abs(log10(pmax(d$own_mod_obs, 1e-300)))), ]))
print(data.frame(sim = em$sim_index, lever = em$lever, level = em$level,
                 q = signif(em$q, 3), mod_obs = signif(em$own_mod_obs, 3),
                 own = round(em$own_rmse, 3),
                 unf_growth = signif(em$unfished_growth, 4)), row.names = FALSE)
