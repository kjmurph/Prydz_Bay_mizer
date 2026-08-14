# =============================================================================
# STAGE D part (ii) -- does the corrected w_min move the yield RMSE ranking?
#
# THE QUESTION. docs/small_divers_wmin_test_brief.md section 6 requires two
# things before a reduced re-run is defensible: (i) all fitted members still pass
# the stability rejection, and (ii) the top-10% membership by RMSE is unchanged,
# or changes only at the margin. Part (i) is settled -- acceptance is identical,
# 196/200 in both arms, 0 changes (docs/small_divers_wmin_stageD_results.md).
# This is part (ii). RMSE depends on modelled catch, so it can move even with
# acceptance fixed.
#
# SCOPE. The clean top 10% (200 of 1,997 distinct members;
# docs/ensemble_deduplication.md). Rerunning acceptance over the whole
# deduplicated ensemble, then filtering members with erepro > 1 or
# R_max = Inf and re-cutting a refined top 10%, is the agreed NEXT step -- this
# run is the cheaper assessment on a manageable subset first.
#
# PIPELINE, per member per arm:
#   params -> steady() -> up to 3 x 118 yr unfished spin-up -> project 1841-2010
#          -> getYield -> RMSE vs observed catch
# steady() is a SINGLE call at the driver's tol/t_max, matching Stage D, so the
# two parts are directly comparable.
#
# RMSE is the exact metric from yield_rmse_evaluation.R:112-140 -- log10(g+1),
# restricted per species to its own effort window.
#
# WHAT COUNTS AS A MARGINAL CHANGE. Reported against the clean top-10% cutoff
# (the rank-200 RMSE of the deduplicated ranking): how many members would cross
# it under the corrected model, plus the paired per-member difference and the
# rank correlation.
#
# Writes Output_large_files/wmin_test/36_stageD2_rmse.{rds,csv}
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel)
  library(dplyr); library(reshape2)
})

out_dir <- "Output_large_files/wmin_test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

PEN         <- "small divers"
W_NEW       <- 3626.667
REPRO_LEVEL <- 0.50
EREPRO_CAP  <- 0.95
STEADY_TOL     <- 0.002
STEADY_TMAX    <- 1200
SPINUP_YEARS   <- 118
SPINUP_CYCLES  <- 3
PROJ_YEARS     <- 170          # 1841-2010

STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

N_LIMIT <- as.integer(Sys.getenv("STAGED_N", "0"))
N_CORES <- as.integer(Sys.getenv("STAGED_CORES",
                                 as.character(max(1, detectCores() - 2))))

t0 <- proc.time()
cat("=== Stage D(ii): yield RMSE under the corrected w_min ===\n")
cat("started", format(Sys.time()), "| cores", N_CORES, "\n")

dd <- readRDS(file.path(out_dir, "33_dedupe_full.rds"))
members <- dd$new_top
if (N_LIMIT > 0) members <- head(members, N_LIMIT)
clean_cutoff <- dd$dedup$rmse[dd$n_top_new]
cat("clean top 10%:", dd$n_top_new, "| rank-200 RMSE cutoff:",
    signif(clean_cutoff, 8), "| running:", length(members), "\n\n")

# --- observed yield, effort windows ------------------------------------------
effort_arr <- readRDS("effort_array_1841_2010.rds")
effort_windows <- do.call(rbind, lapply(colnames(effort_arr), function(sp) {
  yrs <- as.numeric(rownames(effort_arr))[effort_arr[, sp] > 0]
  if (length(yrs) > 0)
    data.frame(Species = sp, first_year = min(yrs), last_year = max(yrs),
               stringsAsFactors = FALSE)
}))
obs_long <- read.csv("yield_observed_timeseries.csv") %>%
  reshape2::melt(id.vars = "Year", variable.name = "Species", value.name = "Yield_g") %>%
  mutate(Species = gsub("\\.", " ", as.character(Species)),
         Yield_g = pmax(coalesce(as.numeric(Yield_g), 0), 0)) %>%
  left_join(effort_windows, by = "Species") %>%
  filter(!is.na(first_year), Year >= first_year, Year <= last_year) %>%
  select(Year, Species, Yield_obs = Yield_g)
cat("comparison rows:", nrow(obs_long), "\n")

# --- member params + their own effort ----------------------------------------
cat("loading fitted ensemble (375 MB)...\n")
ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
old_idx <- ens$metadata$top10pct_indices
have <- match(members, old_idx)
plist <- vector("list", length(members))
elist <- vector("list", length(members))
for (j in which(!is.na(have))) {
  plist[[j]] <- ens$fished_top10pct[[have[j]]]@params
  elist[[j]] <- ens$fished_top10pct[[have[j]]]@effort
}
rm(ens); invisible(gc())
missing <- which(is.na(have))
if (length(missing)) {
  cat("promoted members needing the full ensemble:", length(missing), "\n")
  cat("loading full ensemble (1.86 GB)...\n")
  mc <- readRDS("Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds")
  sims_all <- mc$simulations
  vidx <- which(vapply(sims_all, function(x)
    inherits(x, "MizerSim") && !any(is.nan(x@n)) && !any(is.infinite(x@n)),
    logical(1)))
  for (j in missing) {
    s <- sims_all[[vidx[members[j]]]]
    plist[[j]] <- s@params; elist[[j]] <- s@effort
  }
  rm(mc, sims_all); invisible(gc())
}
stopifnot(!any(vapply(plist, is.null, logical(1))))
cat("members ready:", length(plist), "\n\n")

worker <- function(k) {
  suppressPackageStartupMessages({
    library(therMizer); library(mizer); library(dplyr); library(reshape2)
  })
  p0 <- plist[[k]]; eff <- elist[[k]]
  i <- which(p0@species_params$species == PEN)

  check_stability <- function(sim_object) {
    bm <- getBiomass(sim_object)
    yrs <- as.numeric(rownames(bm)); nT <- nrow(bm)
    if (nT < 5) return(list(stable = FALSE, max_cv = NA_real_))
    tail_mat <- tail(bm, max(5, min(STAB$check_years_tail, nT)))
    head_n <- max(5, min(STAB$trend_first_years, nT))
    head_mat <- head(bm, head_n); head_years <- head(yrs, head_n)
    mean_tail <- colMeans(tail_mat, na.rm = TRUE)
    cv_tail <- ifelse(mean_tail > 0,
                      apply(tail_mat, 2, sd, na.rm = TRUE) / mean_tail, Inf)
    rel_slope <- setNames(rep(NA_real_, ncol(head_mat)), colnames(head_mat))
    pvals <- rel_slope
    for (j in seq_len(ncol(head_mat))) {
      y <- head_mat[, j]; mu <- mean(y, na.rm = TRUE)
      if (!is.finite(mu) || mu < STAB$min_mean_biomass) {
        rel_slope[j] <- 0; pvals[j] <- 1; next
      }
      fit <- try(suppressWarnings(
        lm(b ~ t, data = data.frame(t = head_years, b = as.numeric(y)))),
        silent = TRUE)
      if (inherits(fit, "try-error")) { rel_slope[j] <- 0; pvals[j] <- 1 } else {
        rel_slope[j] <- as.numeric(coef(fit)[["t"]]) / mu
        pv <- try(summary(fit)$coefficients["t", "Pr(>|t|)"], silent = TRUE)
        pvals[j] <- if (inherits(pv, "try-error")) 1 else as.numeric(pv)
      }
    }
    fail_cv <- cv_tail > STAB$cv_threshold
    fail_tr <- (abs(rel_slope) > STAB$trend_rel_slope_max) &
      (pvals < STAB$trend_pval_max)
    list(stable = !any(fail_cv | fail_tr, na.rm = TRUE),
         max_cv = max(cv_tail[is.finite(cv_tail)], na.rm = TRUE))
  }

  set_wmin <- function(p, w_new) {
    rf <- p@rates_funcs; rd <- p@resource_dynamics
    sp <- p@species_params; sp$w_min[i] <- w_new; p@species_params <- sp
    p <- suppressWarnings(setParams(p))
    ther <- intersect(c("Encounter", "PredRate", "EReproAndGrowth"), names(rf))
    p@rates_funcs[ther] <- rf[ther]
    p@resource_dynamics <- rd
    p@initial_n[i, p@w < w_new] <- 0
    p
  }
  rates0 <- function(p) getRates(p, n = p@initial_n, n_pp = p@initial_n_pp,
                                 n_other = p@initial_n_other, effort = 0, t = 1841)

  # mizer 2.5.0 objects lack @second_order_w, which getYield dereferences.
  # validParams() is mizer's own upgrade and sets bin_average = FALSE, i.e. the
  # 2.5.0 branch -- verified bit-identical and reproducing the stored RMSE to
  # ~3e-15 (see docs/ensemble_deduplication.md).
  rmse_of <- function(sim) {
    sim@params <- suppressWarnings(validParams(sim@params))
    df <- reshape2::melt(getYield(sim))
    names(df) <- c("Year", "Species", "Yield_mod")
    df$Year <- as.numeric(as.character(df$Year))
    df$Species <- as.character(df$Species)
    df$Yield_mod <- pmax(df$Yield_mod, 0)
    comp <- obs_long %>% left_join(df, by = c("Year", "Species")) %>%
      mutate(Yield_mod = coalesce(Yield_mod, 0))
    list(rmse = sqrt(mean((log10(comp$Yield_mod + 1) -
                             log10(comp$Yield_obs + 1))^2, na.rm = TRUE)),
         cor_log = suppressWarnings(cor(log10(comp$Yield_mod + 1),
                                        log10(comp$Yield_obs + 1),
                                        use = "complete.obs")))
  }

  run_arm <- function(p) {
    warned <- FALSE
    out <- withCallingHandlers(
      try(steady(p, tol = STEADY_TOL, t_max = STEADY_TMAX,
                 preserve = c("erepro")), silent = TRUE),
      warning = function(w) {
        if (grepl("did not converge", conditionMessage(w), ignore.case = TRUE))
          warned <<- TRUE
        invokeRestart("muffleWarning")
      })
    if (inherits(out, "try-error"))
      return(list(ok = FALSE, reason = "steady_error"))
    init <- NULL; stab <- NULL
    for (cyc in seq_len(SPINUP_CYCLES)) {
      # initial_n must be OMITTED, not passed as NULL -- project() does
      # params@initial_n[] <- initial_n and errors on a zero-length replacement.
      s <- try(if (is.null(init))
        project(out, t_start = 1841, t_max = SPINUP_YEARS, effort = 0)
        else project(out, t_start = 1841, t_max = SPINUP_YEARS, effort = 0,
                     initial_n = init), silent = TRUE)
      if (inherits(s, "try-error"))
        return(list(ok = FALSE, reason = "spinup_error"))
      init <- s@n[SPINUP_YEARS, , ]
      stab <- check_stability(s)
      if (isTRUE(stab$stable)) break
    }
    pr <- try(project(out, initial_n = init, t_start = 1841,
                      t_max = PROJ_YEARS, effort = eff), silent = TRUE)
    if (inherits(pr, "try-error"))
      return(list(ok = FALSE, reason = "projection_error"))
    y <- try(rmse_of(pr), silent = TRUE)
    if (inherits(y, "try-error"))
      return(list(ok = FALSE, reason = "yield_error"))
    list(ok = TRUE, reason = "ok", rmse = y$rmse, cor_log = y$cor_log,
         steady_converged = !warned, stable = isTRUE(stab$stable),
         max_cv = stab$max_cv)
  }

  ctl <- run_arm(p0)

  pt <- set_wmin(p0, W_NEW)
  r0 <- rates0(p0); rt <- rates0(pt)
  floor_e <- p0@species_params$erepro[i] * r0$rdd[i] / rt$rdi[i]
  e_new <- min(floor_e / (1 - REPRO_LEVEL), EREPRO_CAP)
  rdi_t <- rt$rdi[i] * e_new / pt@species_params$erepro[i]
  infeasible <- rdi_t <= r0$rdd[i]
  R_new <- if (!infeasible) 1 / (1 / r0$rdd[i] - 1 / rdi_t) else Inf
  pt@species_params$erepro[i] <- e_new
  pt@species_params$R_max[i]  <- R_new
  trt <- run_arm(pt)

  gv <- function(x, f, d = NA_real_) if (isTRUE(x$ok)) x[[f]] else d
  data.frame(k = k, sim_index = members[k],
             erepro_floor = floor_e, erepro_set = e_new,
             repro_infeasible = infeasible,
             ctl_ok = ctl$ok, ctl_reason = ctl$reason,
             ctl_rmse = gv(ctl, "rmse"), ctl_cor = gv(ctl, "cor_log"),
             ctl_stable = gv(ctl, "stable", NA),
             trt_ok = trt$ok, trt_reason = trt$reason,
             trt_rmse = gv(trt, "rmse"), trt_cor = gv(trt, "cor_log"),
             trt_stable = gv(trt, "stable", NA),
             stringsAsFactors = FALSE)
}

cl <- makeCluster(N_CORES)
on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
clusterExport(cl, c("plist", "elist", "members", "obs_long", "PEN", "W_NEW",
                    "REPRO_LEVEL", "EREPRO_CAP", "STEADY_TOL", "STEADY_TMAX",
                    "SPINUP_YEARS", "SPINUP_CYCLES", "PROJ_YEARS", "STAB"),
              envir = environment())
cat("projecting", length(plist), "members x 2 arms on", N_CORES, "cores...\n")
res <- parLapplyLB(cl, seq_along(plist), worker)
stopCluster(cl)

df <- do.call(rbind, res)
saveRDS(df, file.path(out_dir, "36_stageD2_rmse.rds"))
write.csv(df, file.path(out_dir, "36_stageD2_rmse.csv"), row.names = FALSE)

cat("\n=== completion ===\n")
cat("  control  ok:", sum(df$ctl_ok), "/", nrow(df), "\n")
cat("  corrected ok:", sum(df$trt_ok), "/", nrow(df), "\n")
if (any(!df$ctl_ok)) print(table(df$ctl_reason[!df$ctl_ok]))
if (any(!df$trt_ok)) print(table(df$trt_reason[!df$trt_ok]))

ok <- df[df$ctl_ok & df$trt_ok, ]
cat("\n=== paired RMSE difference (corrected - control), n =", nrow(ok), "===\n")
d <- ok$trt_rmse - ok$ctl_rmse
cat("  median", signif(median(d), 4), "| IQR [", signif(quantile(d, .25), 4), ",",
    signif(quantile(d, .75), 4), "] | range [", signif(min(d), 4), ",",
    signif(max(d), 4), "]\n")
cat("  as % of control RMSE: median",
    signif(100 * median(d / ok$ctl_rmse), 4), "%\n")
cat("  members whose RMSE worsened:", sum(d > 0), "of", nrow(ok), "\n")

cat("\n=== ranking stability within these members ===\n")
ok$rank_ctl <- rank(ok$ctl_rmse, ties.method = "first")
ok$rank_trt <- rank(ok$trt_rmse, ties.method = "first")
cat("  Spearman rho:", signif(cor(ok$rank_ctl, ok$rank_trt, method = "spearman"), 6), "\n")
cat("  Kendall tau :", signif(cor(ok$rank_ctl, ok$rank_trt, method = "kendall"), 6), "\n")
rc <- abs(ok$rank_trt - ok$rank_ctl)
cat("  |rank change|: median", median(rc), "| 90th pct", quantile(rc, .9),
    "| max", max(rc), "\n")

cat("\n=== would membership change? against the clean top-10% cutoff ===\n")
cat("  cutoff (rank-200 RMSE of the deduplicated ranking):",
    signif(clean_cutoff, 8), "\n")
cat("  control   members above the cutoff (would drop out):",
    sum(ok$ctl_rmse > clean_cutoff), "\n")
cat("  corrected members above the cutoff (would drop out):",
    sum(ok$trt_rmse > clean_cutoff), "\n")
cat("  net change attributable to the correction:",
    sum(ok$trt_rmse > clean_cutoff) - sum(ok$ctl_rmse > clean_cutoff), "\n")
cat("\n  NOTE: indicative only. A definitive membership test needs the corrected\n")
cat("  model run over the whole deduplicated ensemble, not just its top 10%.\n")

cat("\n=== correlation screen (cor_log > 0.5) ===\n")
cat("  control  passing:", sum(ok$ctl_cor > 0.5, na.rm = TRUE), "/", nrow(ok), "\n")
cat("  corrected passing:", sum(ok$trt_cor > 0.5, na.rm = TRUE), "/", nrow(ok), "\n")

cat("\n=== feasible members only ===\n")
fz <- ok[!ok$repro_infeasible, ]
if (nrow(fz)) {
  df2 <- fz$trt_rmse - fz$ctl_rmse
  cat("  n:", nrow(fz), "| median paired diff", signif(median(df2), 4),
      "| Spearman rho",
      signif(cor(rank(fz$ctl_rmse), rank(fz$trt_rmse), method = "spearman"), 6), "\n")
}
inf <- ok[ok$repro_infeasible, ]
if (nrow(inf)) {
  cat("  infeasible members (n =", nrow(inf), ") median paired diff",
      signif(median(inf$trt_rmse - inf$ctl_rmse), 4), "\n")
}

cat("\nelapsed:", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
