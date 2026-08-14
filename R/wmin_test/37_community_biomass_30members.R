# =============================================================================
# The community-biomass gap -- the weakest link in the investigation.
#
# WHY THIS EXISTS. An independent review identified this as the single biggest
# hole. The headline "community biomass changes by -0.005%" comes from Stage C,
# which is **n = 1** (the unperturbed base params, compared at the post-steady()
# state with no spin-up). The only ensemble-level evidence is Stage B's six
# members, compared after a 118 yr spin-up, where total biomass spans
# **93.29% to 100.004%** of control -- member 446 shows **-6.7%**. Stage B's doc
# quoted only the median (99.6%, "essentially unchanged"), which with n = 6
# conceals the largest observation, and the synthesis omitted it entirely.
#
# There is also an unreconciled SIGN DISAGREEMENT: Stage B puts penguin biomass at
# 100.8-122.2% of control (an increase), Stage C at 96.16% (a decrease).
#
# WHAT THIS RESOLVES. Both are measured here, on the same members, at BOTH
# comparison points:
#     post-steady()   <- Stage C's comparison point
#     post-spin-up    <- Stage B's comparison point
# If the two disagree on the same member, the 118 yr spin-up is the cause. If they
# agree, the difference was the object (base params vs perturbed MC member).
#
# It also records the ACHIEVED reproduction level. Stage C targeted 0.50 but the
# shipped object sits at 0.698, because steady(preserve = "erepro") ends with
# setBevertonHolt(erepro = old_erepro), which RE-DERIVES R_max at the equilibrated
# state. erepro is preserved exactly; R_max is not. The documents describe a
# controlled comparison at matched recruitment that the objects do not implement,
# so the realised level is logged per member from now on.
#
# MEMBERS. 30 distinct members of the clean deduplicated top 200: the 6 original
# Stage B pilots (for continuity) plus a seeded random 24 from the rest, so the
# sample spans the ranking rather than just its best-fitting head.
#
# Tolerance ladder is retained (this is a diagnostic, not an acceptance
# replication -- a single strict call is only required when reproducing the
# rejection procedure).
#
# Writes Output_large_files/wmin_test/37_community_30.{rds,csv}
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})

out_dir <- "Output_large_files/wmin_test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

PEN         <- "small divers"
W_NEW       <- 3626.667
REPRO_LEVEL <- 0.50
EREPRO_CAP  <- 0.95
STEADY_TMAX <- 1500
TOL_LADDER  <- c(0.1, 0.05, 0.01, 0.0025)
SPINUP_YEARS <- 118
N_MEMBERS   <- as.integer(Sys.getenv("N30", "30"))
SEED        <- 20260730
N_CORES     <- as.integer(Sys.getenv("STAGED_CORES",
                                     as.character(max(1, detectCores() - 2))))

t0 <- proc.time()
cat("=== community biomass, treated vs control, n =", N_MEMBERS, "===\n")
cat("started", format(Sys.time()), "| cores", N_CORES, "\n\n")

dd <- readRDS(file.path(out_dir, "33_dedupe_full.rds"))
top <- dd$new_top
pilots <- c(446, 1512, 1819, 2082, 1776, 173)      # the original Stage B six
pilots <- pilots[pilots %in% top]
set.seed(SEED)
rest <- sample(setdiff(top, pilots), max(0, N_MEMBERS - length(pilots)))
members <- c(pilots, rest)[seq_len(min(N_MEMBERS, length(top)))]
cat("members:", length(members), "(", length(pilots), "pilots +",
    length(members) - length(pilots), "seeded random )\n")

cat("loading fitted ensemble (375 MB)...\n")
ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
old_idx <- ens$metadata$top10pct_indices
have <- match(members, old_idx)
plist <- vector("list", length(members))
for (j in which(!is.na(have))) plist[[j]] <- ens$fished_top10pct[[have[j]]]@params
sp_names <- ens$fished_top10pct[[1]]@params@species_params$species
rm(ens); invisible(gc())
miss <- which(is.na(have))
if (length(miss)) {
  cat("promoted members needing the full ensemble:", length(miss), "\n")
  mc <- readRDS("Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds")
  sims <- mc$simulations
  vidx <- which(vapply(sims, function(x)
    inherits(x, "MizerSim") && !any(is.nan(x@n)) && !any(is.infinite(x@n)),
    logical(1)))
  for (j in miss) plist[[j]] <- sims[[vidx[members[j]]]]@params
  rm(mc, sims); invisible(gc())
}
stopifnot(!any(vapply(plist, is.null, logical(1))))
cat("ready\n\n")

worker <- function(k) {
  suppressPackageStartupMessages({ library(therMizer); library(mizer) })
  p0 <- plist[[k]]
  i <- which(p0@species_params$species == PEN)

  bio_vec <- function(p) rowSums(sweep(p@initial_n, 2, p@w * p@dw, "*"))
  bio_sim <- function(s) {
    bm <- getBiomass(s); bm[nrow(bm), ]
  }
  rates0 <- function(p) getRates(p, n = p@initial_n, n_pp = p@initial_n_pp,
                                 n_other = p@initial_n_other, effort = 0, t = 1841)
  lvl <- function(p) {
    r <- rates0(p); unname(1 - r$rdd[i] / r$rdi[i])
  }
  # ACHIEVED recruitment, not the level. rdd_ratio = treated RDD / control RDD is
  # the variable that actually explains penguin biomass; the reproduction level
  # does not, because steady() re-derives R_max and both move together.
  rdd_of <- function(p) unname(rates0(p)$rdd[i])

  set_wmin <- function(p, w_new) {
    rf <- p@rates_funcs; rd <- p@resource_dynamics
    sp <- p@species_params; sp$w_min[i] <- w_new; p@species_params <- sp
    p <- suppressWarnings(setParams(p))
    ther <- intersect(c("Encounter", "PredRate", "EReproAndGrowth"), names(rf))
    p@rates_funcs[ther] <- rf[ther]; p@resource_dynamics <- rd
    p@initial_n[i, p@w < w_new] <- 0
    p
  }

  ladder <- function(p) {
    for (tol in TOL_LADDER) {
      warned <- FALSE
      out <- withCallingHandlers(
        try(steady(p, tol = tol, t_max = STEADY_TMAX, preserve = c("erepro")),
            silent = TRUE),
        warning = function(w) {
          if (grepl("did not converge", conditionMessage(w), ignore.case = TRUE))
            warned <<- TRUE
          invokeRestart("muffleWarning")
        })
      if (inherits(out, "try-error")) return(NULL)
      p <- out
    }
    p
  }

  arm <- function(p) {
    ps <- ladder(p)
    if (is.null(ps)) return(NULL)
    sp <- try(project(ps, t_start = 1841, t_max = SPINUP_YEARS, effort = 0),
              silent = TRUE)
    if (inherits(sp, "try-error")) return(NULL)
    list(bio_steady = bio_vec(ps), bio_spin = bio_sim(sp),
         lvl = lvl(ps), rdd = rdd_of(ps),
         erepro = ps@species_params$erepro[i],
         R_max = ps@species_params$R_max[i],
         n_inf = sum(!is.finite(ps@species_params$R_max)))
  }

  ctl <- arm(p0)
  if (is.null(ctl)) return(NULL)

  pt <- set_wmin(p0, W_NEW)
  r0 <- rates0(p0); rtt <- rates0(pt)
  floor_e <- p0@species_params$erepro[i] * r0$rdd[i] / rtt$rdi[i]
  e_new <- min(floor_e / (1 - REPRO_LEVEL), EREPRO_CAP)
  rdi_t <- rtt$rdi[i] * e_new / pt@species_params$erepro[i]
  infeas <- rdi_t <= r0$rdd[i]
  R_new <- if (!infeas) 1 / (1 / r0$rdd[i] - 1 / rdi_t) else Inf
  pt@species_params$erepro[i] <- e_new
  pt@species_params$R_max[i] <- R_new
  lvl_input <- lvl(pt)
  trt <- arm(pt)
  if (is.null(trt)) return(NULL)

  pct <- function(a, b) 100 * (a - b) / b
  list(summary = data.frame(
      member = k, sim_index = members[k],
      erepro_floor = floor_e, erepro_set = e_new, repro_infeasible = infeas,
      lvl_input = lvl_input, lvl_achieved = trt$lvl,
      erepro_achieved = trt$erepro, R_max_achieved = trt$R_max,
      rdd_control = ctl$rdd, rdd_treated = trt$rdd,
      rdd_ratio = trt$rdd / ctl$rdd,
      new_rmax_inf = trt$n_inf > ctl$n_inf,
      # Stage C's comparison point
      tot_pct_steady = pct(sum(trt$bio_steady), sum(ctl$bio_steady)),
      pen_pct_steady = pct(trt$bio_steady[i], ctl$bio_steady[i]),
      # Stage B's comparison point
      tot_pct_spin = pct(sum(trt$bio_spin), sum(ctl$bio_spin)),
      pen_pct_spin = pct(trt$bio_spin[i], ctl$bio_spin[i]),
      stringsAsFactors = FALSE),
    per_group = data.frame(
      sim_index = members[k], species = sp_names,
      pct_steady = pct(trt$bio_steady, ctl$bio_steady),
      pct_spin   = pct(trt$bio_spin,   ctl$bio_spin),
      stringsAsFactors = FALSE))
}

cl <- makeCluster(N_CORES)
on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
clusterExport(cl, c("plist", "members", "sp_names", "PEN", "W_NEW", "REPRO_LEVEL",
                    "EREPRO_CAP", "STEADY_TMAX", "TOL_LADDER", "SPINUP_YEARS"),
              envir = environment())
cat("running", length(plist), "members x 2 arms on", N_CORES, "cores...\n")
res <- parLapplyLB(cl, seq_along(plist), worker)
stopCluster(cl)

ok <- !vapply(res, is.null, logical(1))
cat("completed:", sum(ok), "/", length(res), "\n")
res <- res[ok]
S <- bind_rows(lapply(res, `[[`, "summary"))
G <- bind_rows(lapply(res, `[[`, "per_group"))
saveRDS(list(summary = S, per_group = G, members = members, seed = SEED),
        file.path(out_dir, "37_community_30.rds"))
write.csv(S, file.path(out_dir, "37_community_30.csv"), row.names = FALSE)

f <- function(x) signif(x, 4)
cat("\n================ COMMUNITY BIOMASS, treated vs control ================\n")
for (nm in c("tot_pct_steady", "tot_pct_spin")) {
  v <- S[[nm]]
  cat(sprintf("\n%-16s (%s)\n", nm,
              if (grepl("steady", nm)) "Stage C's comparison point" else
                "Stage B's comparison point"))
  cat("   median", f(median(v)), "| IQR [", f(quantile(v, .25)), ",",
      f(quantile(v, .75)), "] | range [", f(min(v)), ",", f(max(v)), "]\n")
  cat("   members |shift| > 1%:", sum(abs(v) > 1), "of", nrow(S),
      "| > 0.1%:", sum(abs(v) > 0.1), "\n")
}

cat("\n================ PENGUIN BIOMASS -- the sign disagreement ================\n")
cat("  Stage B reported +100.8 to +122.2% of control (an INCREASE)\n")
cat("  Stage C reported 96.16% of control (a DECREASE, -3.84%)\n\n")
for (nm in c("pen_pct_steady", "pen_pct_spin")) {
  v <- S[[nm]]
  cat(sprintf("  %-16s median %+8s | range [%s, %s] | n positive %d/%d\n", nm,
              f(median(v)), f(min(v)), f(max(v)), sum(v > 0), nrow(S)))
}
cat("\n  -> if the two rows differ in sign, the 118 yr SPIN-UP is the cause;\n")
cat("     if they agree, the Stage B/C difference was the OBJECT.\n")

cat("\n================ ACHIEVED reproduction level ================\n")
cat("  target at input :", REPRO_LEVEL, "\n")
cat("  input   median", f(median(S$lvl_input)),
    "| range [", f(min(S$lvl_input)), ",", f(max(S$lvl_input)), "]\n")
cat("  ACHIEVED median", f(median(S$lvl_achieved)),
    "| range [", f(min(S$lvl_achieved)), ",", f(max(S$lvl_achieved)), "]\n")
cat("  members within 0.05 of the 0.50 target:",
    sum(abs(S$lvl_achieved - REPRO_LEVEL) < 0.05), "of", nrow(S), "\n")
cat("  (Stage C's shipped object achieved 0.698)\n")
cat("  -> if none land on 0.50, 0.698 is the NORM, not an anomaly.\n")

cat("\n================ ACHIEVED RECRUITMENT (rdd_ratio) ================\n")
cat("  rdd_ratio = treated RDD / control RDD, both measured after steady().\n")
cat("  1.0 would mean control recruitment was restored.\n\n")
cat("  median", f(median(S$rdd_ratio)), "| range [", f(min(S$rdd_ratio)), ",",
    f(max(S$rdd_ratio)), "]\n")
cat("  members reaching >= 0.99 (recruitment effectively restored):",
    sum(S$rdd_ratio >= 0.99), "of", nrow(S), "\n")
cat("  -> the recalibration does NOT restore control recruitment;",
    "best case", f(max(S$rdd_ratio)), "\n")

cat("\n================ WHAT EXPLAINS WHAT ================\n")
cr <- function(x, y) if (length(unique(x)) > 2) signif(cor(x, y), 3) else NA
cat("  rdd_ratio vs penguin biomass (post-spin) : r =",
    cr(S$rdd_ratio, S$pen_pct_spin), "\n")
cat("  rdd_ratio vs COMMUNITY biomass           : r =",
    cr(S$rdd_ratio, S$tot_pct_spin), "\n")
cat("  penguin vs community biomass             : r =",
    cr(S$pen_pct_spin, S$tot_pct_spin), "\n")
cat("\n  -> penguin biomass should track rdd_ratio strongly. If the COMMUNITY\n")
cat("     correlation is weak, recruitment does not explain the community effect\n")
cat("     and something else does -- which is the open question.\n")

cat("\n=== is the community effect concentrated in well-fitting members? ===\n")
rk <- dd$dedup %>% select(sim_index, rmse, rank_ens = rank)
Sj <- S %>% left_join(rk, by = "sim_index")
cat("  RMSE rank vs |community shift| : r =",
    cr(Sj$rank_ens, abs(Sj$tot_pct_spin)), "\n")
print(as.data.frame(Sj %>% arrange(tot_pct_spin) %>%
  select(sim_index, rank_ens, rmse, tot_pct_spin, pen_pct_spin, rdd_ratio,
         lvl_achieved) %>% head(10)), digits = 4, row.names = FALSE)

cat("\n================ flags ================\n")
cat("  infeasible (floor > 1)      :", sum(S$repro_infeasible), "of", nrow(S), "\n")
cat("  erepro hit the", EREPRO_CAP, "cap      :",
    sum(S$erepro_floor / (1 - REPRO_LEVEL) > EREPRO_CAP), "of", nrow(S), "\n")
cat("  NEW non-penguin R_max = Inf :", sum(S$new_rmax_inf), "of", nrow(S), "\n")

cat("\n================ worst community shifts ================\n")
print(as.data.frame(S %>% arrange(tot_pct_spin) %>%
  select(sim_index, tot_pct_steady, tot_pct_spin, pen_pct_steady, pen_pct_spin,
         lvl_achieved, repro_infeasible) %>% head(8)), digits = 4, row.names = FALSE)

cat("\n================ which groups move most (post-spin-up) ================\n")
print(as.data.frame(G %>% group_by(species) %>%
  summarise(median_pct = median(pct_spin), min_pct = min(pct_spin),
            max_pct = max(pct_spin), .groups = "drop") %>%
  arrange(desc(abs(median_pct)))), digits = 4, row.names = FALSE)

cat("\n================ VERDICT ================\n")
v <- S$tot_pct_spin
cat("  Is member 446's -6.7% typical or an outlier?\n")
cat("   members with community shift worse than -1%:", sum(v < -1), "of", nrow(S), "\n")
cat("   members with community shift worse than -3%:", sum(v < -3), "of", nrow(S), "\n")
if (sum(v < -1) <= 1) {
  cat("   -> OUTLIER at this n. But note the MEDIAN below, which is the number\n")
  cat("      that should have been reported instead of Stage C's n = 1.\n")
} else {
  cat("   -> NOT an outlier. w_min is a first-order reason to regenerate.\n")
}
cat(sprintf("\n  Stage C (n = 1, base params) reported %+.4f%%\n", -0.004807))
cat(sprintf("  This sample (n = %d, perturbed members) median %+.4f%%  -- ratio %.0fx\n",
            nrow(S), median(v), abs(median(v) / -0.004807)))

cat("\nelapsed:", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")