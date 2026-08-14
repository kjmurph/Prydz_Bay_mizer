# =============================================================================
# FULL DEDUPLICATED ENSEMBLE -- paired control vs corrected, all 1,997 members.
#
# PURPOSE. Two things at once:
#   1. ACCEPTANCE. Does each member still pass the biomass-stability rejection
#      under the corrected w_min? Replicates the driver faithfully.
#   2. FILTER DATA. Per-member flags for the agreed cull -- erepro >= 1 anywhere,
#      R_max = Inf anywhere, and penguin recalibration infeasibility -- so the
#      refined top 10% can be cut from clean members tomorrow.
#
# Membership is the 1,997 DISTINCT members (docs/ensemble_deduplication.md); the
# 114 duplicate copies are excluded, so nothing is double-counted.
#
# WHAT IT DOES NOT DO. No 1841-2010 projection, hence no new RMSE ranking. Adding
# it would take the run from ~12 h to ~23 h. Cheaper order: filter first, then
# project only the survivors.
#
# FIDELITY. A SINGLE steady() call at the driver's tol/t_max --
# run_single_enhanced_sim (09_Uncertainty_Analysis.Rmd:319-543) treats
# non-convergence as outright rejection, so a tolerance ladder would rescue draws
# the real procedure discards and inflate the pass rate. Ladders are for
# diagnostics only. Spin-up follows the driver: up to 3 unfished 118 yr cycles,
# early exit once stable.
#
# PAIRED, ALWAYS. A re-run cannot reproduce the stored ensemble (initial_n is the
# post-spin-up state; the pre-spin-up state was never saved), so absolute pass
# rates and biomasses are NOT comparable with stored values. Only
# treated-minus-control is interpretable. Both arms get identical treatment.
#
# MEMORY. The 1.86 GB ensemble is read once and the 1,997 params objects are
# cached to disk in chunks. Workers only ever receive one chunk (~17 MB), never
# the whole set -- exporting 340 MB to 14 workers would need ~5 GB.
#
# CHECKPOINTED. Each chunk's results are written as they complete, so a crash or
# reboot loses at most one chunk. Re-running skips finished chunks.
#
# USAGE
#   Rscript R/wmin_test/39_full_ensemble_paired.R prep     # cache params (~5 min)
#   Rscript R/wmin_test/39_full_ensemble_paired.R run      # the long run (~12 h)
#   Rscript R/wmin_test/39_full_ensemble_paired.R collect  # summarise
# Env: FULL_CHUNK (default 100), FULL_CORES (default cores-2), FULL_LIMIT (0=all)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})

out_dir    <- "Output_large_files/wmin_test"
cache_dir  <- file.path(out_dir, "39_params_cache")
result_dir <- file.path(out_dir, "39_results")
for (d in c(out_dir, cache_dir, result_dir))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

PEN         <- "small divers"
W_NEW       <- 3626.667
REPRO_LEVEL <- 0.50
EREPRO_CAP  <- 0.95
STEADY_TOL     <- 0.002
STEADY_TMAX    <- 1200
SPINUP_YEARS   <- 118
SPINUP_CYCLES  <- 3
STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

CHUNK <- as.integer(Sys.getenv("FULL_CHUNK", "100"))
CORES <- as.integer(Sys.getenv("FULL_CORES", as.character(max(1, detectCores() - 2))))
LIMIT <- as.integer(Sys.getenv("FULL_LIMIT", "0"))
mode  <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(mode)) mode <- "run"

dd <- readRDS(file.path(out_dir, "33_dedupe_full.rds"))
members <- dd$dedup$sim_index                      # 1,997 distinct members
if (LIMIT > 0) members <- head(members, LIMIT)
n_tot <- length(members)
chunks <- split(seq_len(n_tot), ceiling(seq_len(n_tot) / CHUNK))

# ---------------------------------------------------------------- prep --------
if (mode == "prep") {
  cat("=== prep: caching params for", n_tot, "distinct members ===\n")
  cat("loading full ensemble (1.86 GB)...\n")
  mc <- readRDS("Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds")
  sims <- mc$simulations
  vidx <- which(vapply(sims, function(x)
    inherits(x, "MizerSim") && !any(is.nan(x@n)) && !any(is.infinite(x@n)),
    logical(1)))
  stopifnot(length(vidx) >= max(members))
  for (ci in names(chunks)) {
    f <- file.path(cache_dir, sprintf("chunk_%03d.rds", as.integer(ci)))
    if (file.exists(f)) { cat("  chunk", ci, "exists, skipping\n"); next }
    ix <- chunks[[ci]]
    pk <- lapply(ix, function(j) sims[[vidx[members[j]]]]@params)
    saveRDS(list(idx = ix, sim_index = members[ix], params = pk), f)
    cat("  chunk", ci, ":", length(ix), "members ->", basename(f), "\n")
  }
  rm(mc, sims); invisible(gc())
  cat("\nprep complete. Now:  Rscript R/wmin_test/39_full_ensemble_paired.R run\n")
  quit(save = "no")
}

# ------------------------------------------------------------- worker ---------
worker <- function(j) {
  suppressPackageStartupMessages({ library(therMizer); library(mizer) })
  p0 <- CHUNK_PARAMS[[j]]
  si <- CHUNK_SIM_INDEX[j]
  i <- which(p0@species_params$species == PEN)

  check_stability <- function(s) {
    bm <- getBiomass(s); yrs <- as.numeric(rownames(bm)); nT <- nrow(bm)
    if (nT < 5) return(list(stable = FALSE, max_cv = NA_real_, fail = "short"))
    tail_mat <- tail(bm, max(5, min(STAB$check_years_tail, nT)))
    hn <- max(5, min(STAB$trend_first_years, nT))
    hm <- head(bm, hn); hy <- head(yrs, hn)
    mt <- colMeans(tail_mat, na.rm = TRUE)
    cv <- ifelse(mt > 0, apply(tail_mat, 2, sd, na.rm = TRUE) / mt, Inf)
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
    fcv <- cv > STAB$cv_threshold
    ftr <- (abs(rs) > STAB$trend_rel_slope_max) & (pv < STAB$trend_pval_max)
    list(stable = !any(fcv | ftr, na.rm = TRUE),
         max_cv = max(cv[is.finite(cv)], na.rm = TRUE),
         fail = paste(unique(c(names(fcv)[which(fcv)], names(ftr)[which(ftr)])),
                      collapse = ";"))
  }

  set_wmin <- function(q, w_new) {
    rf <- q@rates_funcs; rd <- q@resource_dynamics
    sp <- q@species_params; sp$w_min[i] <- w_new; q@species_params <- sp
    q <- suppressWarnings(setParams(q))
    th <- intersect(c("Encounter", "PredRate", "EReproAndGrowth"), names(rf))
    q@rates_funcs[th] <- rf[th]; q@resource_dynamics <- rd
    q@initial_n[i, q@w < w_new] <- 0
    q
  }
  rates0 <- function(q) getRates(q, n = q@initial_n, n_pp = q@initial_n_pp,
                                 n_other = q@initial_n_other, effort = 0, t = 1841)

  # SINGLE steady() call -- non-convergence is a rejection, as the driver has it
  run_arm <- function(p) {
    warned <- FALSE
    out <- withCallingHandlers(
      try(steady(p, tol = STEADY_TOL, t_max = STEADY_TMAX, preserve = c("erepro")),
          silent = TRUE),
      warning = function(w) {
        if (grepl("did not converge", conditionMessage(w), ignore.case = TRUE))
          warned <<- TRUE
        invokeRestart("muffleWarning") })
    if (inherits(out, "try-error"))
      return(list(ok = FALSE, reason = "steady_error"))
    if (warned) return(list(ok = FALSE, reason = "steady_no_converge"))
    sp <- out@species_params
    if (any(is.na(sp$gamma)) || any(is.infinite(sp$gamma)))
      return(list(ok = FALSE, reason = "invalid_gamma"))

    init <- NULL; stab <- NULL; cyc_used <- 0
    for (cyc in seq_len(SPINUP_CYCLES)) {
      # initial_n must be OMITTED, not passed as NULL
      s <- try(if (is.null(init))
        project(out, t_start = 1841, t_max = SPINUP_YEARS, effort = 0)
        else project(out, t_start = 1841, t_max = SPINUP_YEARS, effort = 0,
                     initial_n = init), silent = TRUE)
      if (inherits(s, "try-error")) return(list(ok = FALSE, reason = "project_error"))
      init <- s@n[SPINUP_YEARS, , ]; cyc_used <- cyc
      stab <- check_stability(s)
      if (isTRUE(stab$stable)) break
    }
    bm_spin <- getBiomass(s); bm_spin <- bm_spin[nrow(bm_spin), ]
    bm_st <- rowSums(sweep(out@initial_n, 2, out@w * out@dw, "*"))
    r <- rates0(out)
    list(ok = TRUE, reason = if (isTRUE(stab$stable)) "ok" else "unstable",
         pass = isTRUE(stab$stable), max_cv = stab$max_cv, fail = stab$fail,
         cycles = cyc_used,
         # ---- the filter variables, over ALL 19 groups ----
         max_erepro = max(sp$erepro), n_erepro_ge1 = sum(sp$erepro >= 1),
         erepro_ge1_groups = paste(sp$species[sp$erepro >= 1], collapse = ";"),
         n_rmax_inf = sum(!is.finite(sp$R_max)),
         rmax_inf_groups = paste(sp$species[!is.finite(sp$R_max)], collapse = ";"),
         pen_erepro = sp$erepro[i], pen_rmax = sp$R_max[i], pen_rdd = r$rdd[i],
         total_steady = sum(bm_st), total_spin = sum(bm_spin),
         pen_steady = bm_st[i], pen_spin = bm_spin[i])
  }

  ctl <- run_arm(p0)

  pt <- set_wmin(p0, W_NEW)
  r0 <- rates0(p0); rtt <- rates0(pt)
  fl <- p0@species_params$erepro[i] * r0$rdd[i] / rtt$rdi[i]
  e_new <- min(fl / (1 - REPRO_LEVEL), EREPRO_CAP)
  rdi_t <- rtt$rdi[i] * e_new / pt@species_params$erepro[i]
  infeas <- rdi_t <= r0$rdd[i]
  R_new <- if (!infeas) 1 / (1 / r0$rdd[i] - 1 / rdi_t) else Inf
  pt@species_params$erepro[i] <- e_new
  pt@species_params$R_max[i]  <- R_new
  trt <- run_arm(pt)

  g <- function(a, f, d = NA) if (isTRUE(a$ok)) a[[f]] else d
  pc <- function(a, b) if (is.na(a) || is.na(b) || b == 0) NA_real_ else 100*(a-b)/b
  data.frame(
    sim_index = si,
    erepro_floor = fl, erepro_set = e_new,
    erepro_floor_gt1 = fl > 1, erepro_capped = fl/(1-REPRO_LEVEL) > EREPRO_CAP,
    repro_infeasible = infeas,
    # control arm
    ctl_ok = ctl$ok, ctl_reason = ctl$reason, ctl_pass = g(ctl,"pass",FALSE),
    ctl_max_cv = g(ctl,"max_cv"), ctl_fail = g(ctl,"fail",""),
    ctl_cycles = g(ctl,"cycles"),
    ctl_max_erepro = g(ctl,"max_erepro"), ctl_n_erepro_ge1 = g(ctl,"n_erepro_ge1"),
    ctl_n_rmax_inf = g(ctl,"n_rmax_inf"),
    ctl_rmax_inf_groups = g(ctl,"rmax_inf_groups",""),
    ctl_total_spin = g(ctl,"total_spin"), ctl_pen_spin = g(ctl,"pen_spin"),
    ctl_pen_rdd = g(ctl,"pen_rdd"),
    # treated arm
    trt_ok = trt$ok, trt_reason = trt$reason, trt_pass = g(trt,"pass",FALSE),
    trt_max_cv = g(trt,"max_cv"), trt_fail = g(trt,"fail",""),
    trt_cycles = g(trt,"cycles"),
    trt_max_erepro = g(trt,"max_erepro"), trt_n_erepro_ge1 = g(trt,"n_erepro_ge1"),
    trt_erepro_ge1_groups = g(trt,"erepro_ge1_groups",""),
    trt_n_rmax_inf = g(trt,"n_rmax_inf"),
    trt_rmax_inf_groups = g(trt,"rmax_inf_groups",""),
    trt_pen_erepro = g(trt,"pen_erepro"), trt_pen_rmax = g(trt,"pen_rmax"),
    trt_pen_rdd = g(trt,"pen_rdd"),
    trt_total_spin = g(trt,"total_spin"), trt_pen_spin = g(trt,"pen_spin"),
    # paired
    rdd_ratio = if (isTRUE(ctl$ok) && isTRUE(trt$ok)) trt$pen_rdd/ctl$pen_rdd else NA_real_,
    tot_pct = pc(g(trt,"total_spin"), g(ctl,"total_spin")),
    pen_pct = pc(g(trt,"pen_spin"),   g(ctl,"pen_spin")),
    new_rmax_inf = isTRUE(g(trt,"n_rmax_inf",0) > g(ctl,"n_rmax_inf",0)),
    stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------- run ---------
if (mode == "run") {
  cat("=== FULL PAIRED RUN:", n_tot, "distinct members x 2 arms ===\n")
  cat("started", format(Sys.time()), "| cores", CORES, "| chunk", CHUNK, "\n")
  done <- list.files(result_dir, pattern = "^res_\\d+\\.rds$")
  cat("chunks:", length(chunks), "| already done:", length(done), "\n")
  cat("estimated remaining: ~",
      round((length(chunks) - length(done)) * CHUNK * 0.37 / 60, 1), " h\n\n", sep = "")
  t0 <- proc.time()
  for (ci in names(chunks)) {
    rf <- file.path(result_dir, sprintf("res_%03d.rds", as.integer(ci)))
    if (file.exists(rf)) { cat("chunk", ci, "done, skipping\n"); next }
    cf <- file.path(cache_dir, sprintf("chunk_%03d.rds", as.integer(ci)))
    if (!file.exists(cf)) stop("missing params cache: ", cf, " -- run prep first")
    ch <- readRDS(cf)
    cl <- makeCluster(CORES)
    clusterExport(cl, c("PEN","W_NEW","REPRO_LEVEL","EREPRO_CAP","STEADY_TOL",
                        "STEADY_TMAX","SPINUP_YEARS","SPINUP_CYCLES","STAB"),
                  envir = environment())
    clusterExport(cl, "worker", envir = environment())
    # only THIS chunk's params go to the workers
    assign("CHUNK_PARAMS", ch$params, envir = environment())
    assign("CHUNK_SIM_INDEX", ch$sim_index, envir = environment())
    clusterExport(cl, c("CHUNK_PARAMS", "CHUNK_SIM_INDEX"), envir = environment())
    r <- parLapplyLB(cl, seq_along(ch$params), function(j) {
      tryCatch(worker(j), error = function(e)
        data.frame(sim_index = CHUNK_SIM_INDEX[j], ctl_ok = FALSE,
                   ctl_reason = paste("worker_error:", conditionMessage(e)),
                   trt_ok = FALSE, trt_reason = "worker_error",
                   stringsAsFactors = FALSE))
    })
    stopCluster(cl)
    saveRDS(bind_rows(r), rf)
    el <- (proc.time() - t0)["elapsed"] / 60
    nd <- length(list.files(result_dir, pattern = "^res_\\d+\\.rds$"))
    cat(sprintf("chunk %s done (%d/%d) | elapsed %.1f min | ETA %.1f h | %s\n",
                ci, nd, length(chunks), el,
                (el / max(1, nd - length(done))) * (length(chunks) - nd) / 60,
                format(Sys.time(), "%H:%M")))
    rm(ch); invisible(gc())
  }
  cat("\nALL CHUNKS COMPLETE. elapsed",
      round((proc.time() - t0)["elapsed"] / 3600, 2), "h\n")
  cat("Now:  Rscript R/wmin_test/39_full_ensemble_paired.R collect\n")
  quit(save = "no")
}

# ------------------------------------------------------------ collect --------
files <- sort(list.files(result_dir, pattern = "^res_\\d+\\.rds$", full.names = TRUE))
stopifnot(length(files) > 0)
D <- bind_rows(lapply(files, readRDS))
saveRDS(D, file.path(out_dir, "39_full_ensemble_paired.rds"))
write.csv(D, file.path(out_dir, "39_full_ensemble_paired.csv"), row.names = FALSE)
cat("=== collected", nrow(D), "members from", length(files), "chunks ===\n\n")

cat("=== completion ===\n")
cat("  control  ok:", sum(D$ctl_ok), "| corrected ok:", sum(D$trt_ok), "\n")
if (any(!D$ctl_ok)) { cat("  control failures:\n"); print(table(D$ctl_reason[!D$ctl_ok])) }
if (any(!D$trt_ok)) { cat("  corrected failures:\n"); print(table(D$trt_reason[!D$trt_ok])) }

cat("\n=== ACCEPTANCE (paired) ===\n")
print(table(control = D$ctl_pass, corrected = D$trt_pass))
cat(sprintf("  control %d/%d (%.1f%%) | corrected %d/%d (%.1f%%)\n",
            sum(D$ctl_pass), nrow(D), 100*mean(D$ctl_pass),
            sum(D$trt_pass), nrow(D), 100*mean(D$trt_pass)))
cat("  lost (pass->fail):", sum(D$ctl_pass & !D$trt_pass),
    "| gained (fail->pass):", sum(!D$ctl_pass & D$trt_pass), "\n")

cat("\n=== FILTER: erepro and R_max issues ===\n")
f <- function(x) sprintf("%d (%.1f%%)", sum(x, na.rm = TRUE), 100*mean(x, na.rm = TRUE))
cat("  CONTROL arm\n")
cat("    any erepro >= 1        :", f(D$ctl_n_erepro_ge1 > 0), "\n")
cat("    any R_max = Inf        :", f(D$ctl_n_rmax_inf > 0), "\n")
cat("  CORRECTED arm\n")
cat("    any erepro >= 1        :", f(D$trt_n_erepro_ge1 > 0), "\n")
cat("    any R_max = Inf        :", f(D$trt_n_rmax_inf > 0), "\n")
cat("    penguin recalibration infeasible (floor > 1):", f(D$repro_infeasible), "\n")
cat("    erepro hit the", EREPRO_CAP, "cap :", f(D$erepro_capped), "\n")
cat("    NEW R_max = Inf vs control :", f(D$new_rmax_inf), "\n")

cat("\n  which groups carry R_max = Inf (corrected arm)?\n")
gg <- unlist(strsplit(D$trt_rmax_inf_groups[nzchar(D$trt_rmax_inf_groups)], ";"))
if (length(gg)) print(sort(table(gg), decreasing = TRUE)) else cat("    none\n")

cat("\n=== CLEAN SET after the agreed filter ===\n")
clean <- D %>% filter(trt_ok, trt_pass, trt_n_erepro_ge1 == 0,
                      trt_n_rmax_inf == 0, !repro_infeasible)
cat("  members surviving: ", nrow(clean), " of ", nrow(D),
    sprintf(" (%.1f%%)\n", 100*nrow(clean)/nrow(D)), sep = "")
cat("  -> refined top 10% would be ceiling(", nrow(clean), " * 0.10) = ",
    ceiling(nrow(clean)*0.10), "\n", sep = "")

cat("\n=== community effect of the correction ===\n")
v <- D$tot_pct[!is.na(D$tot_pct)]
cat("  median", signif(median(v),4), "| IQR [", signif(quantile(v,.25),4), ",",
    signif(quantile(v,.75),4), "] | range [", signif(min(v),4), ",",
    signif(max(v),4), "]\n")
cat("  members |shift| > 1%:", sum(abs(v) > 1), "| > 3%:", sum(abs(v) > 3), "\n")
cat("  NOTE: large shifts may be bistable members, not w_min effects --\n")
cat("        member 446 was shown to basin-hop (38_diagnose_member_446.R).\n")
pv <- D$pen_pct[!is.na(D$pen_pct)]
cat("  penguin median", signif(median(pv),4), "| range [", signif(min(pv),4), ",",
    signif(max(pv),4), "] | n positive", sum(pv > 0), "/", length(pv), "\n")
rr <- D$rdd_ratio[!is.na(D$rdd_ratio)]
cat("  rdd_ratio median", signif(median(rr),4), "| range [", signif(min(rr),4),
    ",", signif(max(rr),4), "] | >= 0.99:", sum(rr >= 0.99), "/", length(rr), "\n")
cat("\ncollect complete.\n")