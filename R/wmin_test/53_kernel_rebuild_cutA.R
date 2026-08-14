# =============================================================================
# PHASE 53 -- rebuild the FULL cut-A subset (167 members) from the revised base
# params (lognormal whale kernel, baleen beta 2.468e7, baleen & minke krill
# interaction 0.5), following 44_rebuild_from_base.R exactly.
#
# THE PROTOCOL IS 44's, AND THAT IS VERIFIED, NOT ASSERTED. The 10-member pilot
# (52_kernel_pilot.R) ran this same code path on the CURRENT base and reproduced
# the stored production pipeline to machine precision:
#     post-refit yield RMSE vs 45_refit_results.rds   max rel diff 1.45e-16
#     pre-refit  yield RMSE vs 44_rebuild_summary.csv max rel diff 2.85e-15
# So: apply the three recovered draws (catchability, gamma, abundance_scaling) to
# the base -> steady(tol = 0.0025, t_max = 1500, preserve = "erepro") -> 118 yr
# unfished spin-up, 1 cycle -> stability screen -> 1841-2010 projection. Settings
# are 09_Uncertainty_Analysis.Rmd:1581-1591, the production values.
#
# SINGLE ARM, DELIBERATELY. 44's paired control is not re-run here because the
# published `rebuilt167` data products ARE the matched control at n = 167 -- they
# come from 44_states/ over these same 167 members from the current base, and the
# verification above shows this code path reproduces that build exactly. Re-running
# it would burn ~22 minutes to regenerate numbers we already hold.
#
# TWO THINGS THIS RUN INHERITS RATHER THAN RE-DERIVES, both flagged because they
# bound what the output can be used for:
#
#  1. THE CATCHABILITY MULTIPLIERS come from phase 45, which fitted them globally
#     across the CURRENT 1,668-member ensemble. Re-fitting them for the revised
#     kernel would require rebuilding all 1,848 draw-distinct members first
#     (~2.5 h on the VM), then re-running 45. Until that happens the `refit`
#     yield RMSE here is indicative, not final. The `drawn` RMSE is unaffected.
#
#  2. THE MEMBERSHIP is cut A as defined by the CURRENT ensemble's post-refit
#     ranking. Under the revised kernel the ranking shifts slightly -- the pilot
#     measured a median RMSE ratio of 1.0008 -- so a genuine re-selection could
#     move a few members in or out. That too needs the full ensemble. This run
#     therefore answers "these 167 members under the new params", which is the
#     right paired comparison for the figures, and not "the new best 167".
#
# CHUNKED AND RESUMABLE. Re-running after an interruption skips completed chunks.
# A settings change (different member count or base) makes existing chunks stale;
# the script refuses to resume across one rather than silently mixing builds.
#
# USAGE
#   Rscript R/wmin_test/53_kernel_rebuild_cutA.R run
#   Rscript R/wmin_test/53_kernel_rebuild_cutA.R collect
# ENV: K53_BASE (default params_whale_lognormal_kernel_bk05_mk05.rds)
#      K53_CORES (default 14), K53_CHUNK (default 28), K53_N (default 167)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel)
  library(dplyr); library(reshape2)
})

out_dir <- "Output_large_files/wmin_test"
BASE_P  <- Sys.getenv("K53_BASE", "params_whale_lognormal_kernel_bk05_mk05.rds")
CORES   <- as.integer(Sys.getenv("K53_CORES", "14"))
CHUNK   <- as.integer(Sys.getenv("K53_CHUNK", "28"))
N       <- as.integer(Sys.getenv("K53_N", "167"))
TAG     <- sub("^params_whale_lognormal_kernel", "",
               sub("\\.rds$", "", basename(BASE_P)))
STEM       <- sprintf("53%s_n%d", TAG, N)
result_dir <- file.path(out_dir, paste0(STEM, "_chunks"))
state_dir  <- file.path(out_dir, paste0(STEM, "_states"))
for (d in c(result_dir, state_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

PEN <- "small divers"; BALEEN <- "baleen whales"; MINKE <- "minke whales"
KRILL <- "antarctic krill"
STEADY_TOL <- 0.0025; STEADY_TMAX <- 1500
SPINUP_YEARS <- 118
STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)
QMAX <- 1
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"

# --- members: cut A, in post-refit rank order --------------------------------
CUTS <- readRDS(file.path(out_dir, "46_selection_cuts.rds"))
cutA <- as.integer(CUTS$cuts[["A unweighted RMSE"]])
RF <- readRDS(file.path(out_dir, "45_refit_results.rds"))
chk <- RF$per_species %>% group_by(sim_index) %>%
  summarise(m = sqrt(sum(sse) / sum(n)), .groups = "drop") %>%
  arrange(m) %>% head(length(cutA)) %>% pull(sim_index)
if (!identical(as.integer(chk), cutA))
  stop("cut A does not match the post-refit ranking -- refusing to proceed.")
members <- head(cutA, N)
DR <- readRDS(file.path(out_dir, "43_member_draws.rds"))
miss <- setdiff(as.character(members), names(DR$draws))
if (length(miss)) stop("no recovered draws for: ", paste(miss, collapse = ", "))
if (!all(members %in% DR$members_distinct))
  stop("some members are not in the draw-distinct set")

MULT <- readRDS(file.path(out_dir, "45_catchability_multipliers.rds"))$M
effort_arr <- readRDS("effort_array_1841_2010.rds")

# --- observed yield windows, identical to 44 / yield_rmse_evaluation.R -------
ew <- do.call(rbind, lapply(colnames(effort_arr), function(s) {
  y <- as.numeric(rownames(effort_arr))[effort_arr[, s] > 0]
  if (length(y)) data.frame(Species = s, first_year = min(y), last_year = max(y),
                            stringsAsFactors = FALSE)
}))
obs_long <- read.csv("yield_observed_timeseries.csv") %>%
  reshape2::melt(id.vars = "Year", variable.name = "Species", value.name = "Yield_g") %>%
  mutate(Species = gsub("\\.", " ", as.character(Species)),
         Yield_g = pmax(coalesce(as.numeric(Yield_g), 0), 0)) %>%
  left_join(ew, by = "Species") %>%
  filter(!is.na(first_year), Year >= first_year, Year <= last_year) %>%
  select(Year, Species, Yield_obs = Yield_g)

BASE <- suppressWarnings(validParams(readRDS(BASE_P)))
stopifnot(identical(BASE@second_order_w$flux, "upwind"),
          isFALSE(BASE@second_order_w$bin_average),
          identical(BASE@rates_funcs$Encounter, "therMizerEncounter"),
          identical(BASE@resource_dynamics, "plankton_forcing"),
          BASE@species_params$w_min[BASE@species_params$species == PEN] > 1)
spb <- BASE@species_params
cat("=== 53 KERNEL REBUILD, cut A ===\n")
cat("base:", BASE_P, "\n")
cat(sprintf("  baleen kernel %-9s beta %.4g | minke kernel %-9s beta %.4g\n",
            as.character(spb$pred_kernel_type)[spb$species == BALEEN],
            spb$beta[spb$species == BALEEN],
            as.character(spb$pred_kernel_type)[spb$species == MINKE],
            spb$beta[spb$species == MINKE]))
cat(sprintf("  interaction[baleen, krill] %.3g | [minke, krill] %.3g | symmetric %s\n",
            BASE@interaction[BALEEN, KRILL], BASE@interaction[MINKE, KRILL],
            isTRUE(all.equal(BASE@interaction, t(BASE@interaction),
                             check.attributes = FALSE))))
cat("members:", length(members), "| cores", CORES, "| chunk", CHUNK, "\n")

n_tot  <- length(members)
chunks <- split(seq_len(n_tot), ceiling(seq_len(n_tot) / CHUNK))

# ------------------------------------------------------------------ worker ----
worker <- function(j) {
  suppressPackageStartupMessages({
    library(therMizer); library(mizer); library(dplyr); library(reshape2)
  })
  si <- CHUNK_SIM[j]; dw_ <- CHUNK_DRAW[[j]]

  check_stability <- function(s) {
    bm <- getBiomass(s); yrs <- as.numeric(rownames(bm)); nT <- nrow(bm)
    if (nT < 5) return(list(stable = FALSE, max_cv = NA_real_))
    tm <- tail(bm, max(5, min(STAB$check_years_tail, nT)))
    hn <- max(5, min(STAB$trend_first_years, nT))
    hm <- head(bm, hn); hy <- head(yrs, hn)
    mt <- colMeans(tm, na.rm = TRUE)
    cv <- ifelse(mt > 0, apply(tm, 2, sd, na.rm = TRUE) / mt, Inf)
    rs <- setNames(rep(NA_real_, ncol(hm)), colnames(hm)); pv <- rs
    for (q in seq_len(ncol(hm))) {
      y <- hm[, q]; mu <- mean(y, na.rm = TRUE)
      if (!is.finite(mu) || mu < STAB$min_mean_biomass) { rs[q] <- 0; pv[q] <- 1; next }
      ft <- try(suppressWarnings(lm(b ~ t, data = data.frame(t = hy, b = as.numeric(y)))),
                silent = TRUE)
      if (inherits(ft, "try-error")) { rs[q] <- 0; pv[q] <- 1 } else {
        rs[q] <- as.numeric(coef(ft)[["t"]]) / mu
        pp <- try(summary(ft)$coefficients["t", "Pr(>|t|)"], silent = TRUE)
        pv[q] <- if (inherits(pp, "try-error")) 1 else as.numeric(pp)
      }
    }
    list(stable = !any((cv > STAB$cv_threshold) |
           ((abs(rs) > STAB$trend_rel_slope_max) & (pv < STAB$trend_pval_max)),
           na.rm = TRUE),
         max_cv = max(cv[is.finite(cv)], na.rm = TRUE))
  }
  yield_metrics <- function(sim) {
    d <- reshape2::melt(getYield(sim)); names(d) <- c("Year", "Species", "Yield_mod")
    d$Year <- as.numeric(as.character(d$Year)); d$Species <- as.character(d$Species)
    d$Yield_mod <- pmax(d$Yield_mod, 0)
    cmp <- obs_long %>% left_join(d, by = c("Year", "Species")) %>%
      mutate(Yield_mod = coalesce(Yield_mod, 0))
    per_sp <- cmp %>% group_by(Species) %>%
      summarise(n = n(),
                sse = sum((log10(Yield_mod + 1) - log10(Yield_obs + 1))^2),
                obs_tot = sum(Yield_obs), mod_tot = sum(Yield_mod), .groups = "drop")
    list(rmse = sqrt(mean((log10(cmp$Yield_mod + 1) -
                             log10(cmp$Yield_obs + 1))^2, na.rm = TRUE)),
         cor_log = suppressWarnings(cor(log10(cmp$Yield_mod + 1),
                                        log10(cmp$Yield_obs + 1), use = "complete.obs")),
         per_sp = per_sp)
  }
  steady_guarded <- function(p) {
    no_conv <- FALSE
    out <- withCallingHandlers(
      try(steady(p, tol = STEADY_TOL, t_max = STEADY_TMAX,
                 preserve = c("erepro"), progress_bar = FALSE), silent = TRUE),
      message = function(m) {
        if (grepl("did not converge", conditionMessage(m), ignore.case = TRUE))
          no_conv <<- TRUE
        invokeRestart("muffleMessage")
      }, warning = function(w) invokeRestart("muffleWarning"))
    list(params = out, converged = !no_conv)
  }
  apply_draws <- function(p) {            # 09:337-402, exactly as 44
    gp <- gear_params(p)
    if (!identical(paste(gp$gear, gp$species), names(dw_$catchability)))
      stop("gear_params rows differ between the base and the stored member")
    gp$catchability <- as.numeric(dw_$catchability); gear_params(p) <- gp
    sp <- species_params(p)
    if (!identical(sp$species, names(dw_$gamma)))
      stop("species order differs between the base and the draw")
    sp$gamma <- as.numeric(dw_$gamma)
    species_params(p) <- sp               # rebuilds search_vol AND the kernel
    sc <- as.numeric(dw_$abundance_scaling[sp$species])
    for (k in seq_along(sc)) p@initial_n[k, ] <- p@initial_n[k, ] * sc[k]
    stopifnot(identical(p@rates_funcs$Encounter, "therMizerEncounter"))
    p
  }
  refit_q <- function(p) {                # 46_selection_cuts.R:71-75
    gp <- gear_params(p)
    m <- MULT[match(gp$species, names(MULT))]; m[is.na(m)] <- 1
    gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
    gear_params(p) <- gp; p
  }

  p <- try(apply_draws(BASE), silent = TRUE)
  if (inherits(p, "try-error")) return(list(sim_index = si, ok = FALSE, reason = "draw_error"))
  st <- steady_guarded(p)
  if (inherits(st$params, "try-error")) return(list(sim_index = si, ok = FALSE, reason = "steady_error"))
  if (!st$converged) return(list(sim_index = si, ok = FALSE, reason = "steady_no_converge"))
  ps <- st$params

  s <- try(project(ps, t_start = 1841, t_max = SPINUP_YEARS, effort = 0,
                   progress_bar = FALSE), silent = TRUE)
  if (inherits(s, "try-error")) return(list(sim_index = si, ok = FALSE, reason = "spinup_error"))
  init <- s@n[SPINUP_YEARS, , ]; stab <- check_stability(s)

  pr_d <- try(project(ps, initial_n = init, t_start = 1841, effort = effort_arr,
                      progress_bar = FALSE), silent = TRUE)
  if (inherits(pr_d, "try-error")) return(list(sim_index = si, ok = FALSE, reason = "projection_error"))
  pq <- refit_q(ps)
  pr_r <- try(project(pq, initial_n = init, t_start = 1841, effort = effort_arr,
                      progress_bar = FALSE), silent = TRUE)
  if (inherits(pr_r, "try-error")) return(list(sim_index = si, ok = FALSE, reason = "projection_refit_error"))

  ym_d <- yield_metrics(pr_d); ym_r <- yield_metrics(pr_r)
  # States carry the DRAWN catchability, exactly as 44_states/ does, so the
  # downstream extraction (F00p) applies the multipliers itself.
  saveRDS(list(sim_index = si, arm = "kernel", params = ps, initial_n = init),
          file.path(STATE_DIR, sprintf("state_kernel_%05d.rds", si)))

  sp <- ps@species_params; wdw <- ps@w * ps@dw
  bio_init <- as.numeric(init %*% wdw)
  list(sim_index = si, ok = TRUE, reason = "ok",
       rmse_drawn = ym_d$rmse, rmse_refit = ym_r$rmse, cor_log = ym_r$cor_log,
       stable = isTRUE(stab$stable), max_cv = stab$max_cv,
       bw_1841 = bio_init[sp$species == BALEEN],
       mw_1841 = bio_init[sp$species == MINKE],
       kr_1841 = bio_init[sp$species == KRILL],
       total_1841 = sum(bio_init),
       max_erepro = max(sp$erepro), n_erepro_ge1 = sum(sp$erepro >= 1),
       n_rmax_inf = sum(!is.finite(sp$R_max)),
       per_sp = ym_r$per_sp)
}

# --------------------------------------------------------------------- run ---
if (mode == "run") {
  cat("started", format(Sys.time()), "\n")
  stale <- NULL
  for (ci in names(chunks)) {
    rf <- file.path(result_dir, sprintf("res_%03d.rds", as.integer(ci)))
    if (!file.exists(rf)) next
    m <- tryCatch(readRDS(rf)$meta, error = function(e) NULL)
    if (is.null(m) || !identical(as.integer(m$chunk), as.integer(CHUNK)) ||
        !identical(as.integer(m$n_total), as.integer(n_tot)) ||
        !identical(m$base, BASE_P)) stale <- c(stale, basename(rf))
  }
  if (length(stale))
    stop("existing chunk results were written under different settings (",
         paste(head(stale, 3), collapse = ", "), "). Clear ", result_dir,
         " and relaunch.")

  t0 <- proc.time(); done0 <- length(list.files(result_dir, pattern = "^res_"))
  for (ci in names(chunks)) {
    rf <- file.path(result_dir, sprintf("res_%03d.rds", as.integer(ci)))
    if (file.exists(rf)) { cat("chunk", ci, "done, skipping\n"); next }
    ms <- members[chunks[[ci]]]
    cl <- makeCluster(CORES)
    assign("STATE_DIR", state_dir, envir = environment())
    assign("CHUNK_SIM", ms, envir = environment())
    assign("CHUNK_DRAW", DR$draws[as.character(ms)], envir = environment())
    clusterExport(cl, c("PEN", "BALEEN", "MINKE", "KRILL", "STEADY_TOL",
                        "STEADY_TMAX", "SPINUP_YEARS", "STAB", "obs_long",
                        "BASE", "MULT", "QMAX", "effort_arr", "STATE_DIR",
                        "CHUNK_SIM", "CHUNK_DRAW", "worker"), envir = environment())
    r <- parLapplyLB(cl, seq_along(ms), function(j)
      tryCatch(worker(j), error = function(e)
        list(sim_index = CHUNK_SIM[j], ok = FALSE,
             reason = paste("worker_error:", conditionMessage(e)))))
    stopCluster(cl)
    saveRDS(list(res = r, meta = list(chunk = CHUNK, n_total = n_tot,
                                      base = BASE_P, sim_index = ms)), rf)
    el <- (proc.time() - t0)["elapsed"] / 60
    nd <- length(list.files(result_dir, pattern = "^res_"))
    cat(sprintf("chunk %s (%d/%d) | ok %d/%d | elapsed %.1f min | ETA %.1f min\n",
                ci, nd, length(chunks),
                sum(vapply(r, function(x) isTRUE(x$ok), logical(1))), length(r),
                el, (el / max(1, nd - done0)) * (length(chunks) - nd)))
    rm(r); invisible(gc())
  }
  cat("\nrun complete.", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
  cat("Now: Rscript R/wmin_test/53_kernel_rebuild_cutA.R collect\n")
  quit(save = "no")
}

# ----------------------------------------------------------------- collect ---
fs <- sort(list.files(result_dir, pattern = "^res_\\d+\\.rds$", full.names = TRUE))
stopifnot(length(fs) > 0)
Z <- unlist(lapply(fs, function(f) readRDS(f)$res), recursive = FALSE)
ok <- vapply(Z, function(x) isTRUE(x$ok), logical(1))
cat("=== collected", sum(ok), "of", length(Z), "members ===\n")
if (any(!ok)) {
  bad <- do.call(rbind, lapply(Z[!ok], function(z)
    data.frame(sim_index = z$sim_index, reason = z$reason)))
  print(bad, row.names = FALSE)
}
S <- bind_rows(lapply(Z[ok], function(z)
  as.data.frame(z[c("sim_index", "rmse_drawn", "rmse_refit", "cor_log", "stable",
                    "max_cv", "bw_1841", "mw_1841", "kr_1841", "total_1841",
                    "max_erepro", "n_erepro_ge1", "n_rmax_inf")])))
PSP <- bind_rows(lapply(Z[ok], function(z) z$per_sp %>% mutate(sim_index = z$sim_index)))

guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE); f
}
saveRDS(list(summary = S, per_species = PSP, members = S$sim_index,
             arms = "kernel", base_path = c(kernel = BASE_P),
             n_requested = n_tot, failed = if (any(!ok))
               vapply(Z[!ok], function(z) z$sim_index, numeric(1)) else numeric(0),
             settings = list(steady_tol = STEADY_TOL, steady_tmax = STEADY_TMAX,
                             spinup_years = SPINUP_YEARS),
             built = Sys.time()),
        guard(file.path(out_dir, paste0(STEM, "_rebuild.rds"))))
write.csv(S, guard(file.path(out_dir, paste0(STEM, "_summary.csv"))), row.names = FALSE)

cat("\n=== outcome ===\n")
cat(sprintf("  converged %d/%d | stable %d | erepro>=1 anywhere %d | R_max Inf anywhere %d\n",
            sum(ok), length(Z), sum(S$stable), sum(S$n_erepro_ge1 > 0),
            sum(S$n_rmax_inf > 0)))
cat(sprintf("  yield RMSE (refit q): median %.4f | range %.4f - %.4f\n",
            median(S$rmse_refit), min(S$rmse_refit), max(S$rmse_refit)))
STORED <- readRDS(file.path(out_dir, "45_refit_results.rds"))$summary
ref <- STORED$rmse[match(S$sim_index, STORED$sim_index)]
cat(sprintf("  vs the stored current-base RMSE: median ratio %.5f\n",
            median(S$rmse_refit / ref)))
cat("\nStates in", state_dir, "\n")
cat("Next: Rscript \"Manuscript scripts/F00p_build_pilot_data.R\"",
    "with F0P_STATE_DIR /", "F0P_MANIFEST / F0P_SUFFIX set\n")