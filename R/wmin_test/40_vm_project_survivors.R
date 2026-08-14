# =============================================================================
# VM RUN -- project the 1,831 surviving members and recompute yield RMSE, so the
# refined top 10% (184) can be cut from a clean, duplicate-free ranking.
#
# CONTEXT. The count chain is 2,111 accepted -> 1,997 distinct (114 duplicates
# removed) -> 1,831 clean (erepro >= 1 / R_max = Inf / penguin-infeasible removed)
# -> top 10% = 184. `39_full_ensemble_paired.R` established the clean set but
# deliberately skipped projections to fit an overnight window, so **184 is the
# SIZE, not the LIST**. This produces the list.
#
# WHY BOTH ARMS. The treated arm must re-enter steady(), because w_min changes the
# steady state. Re-entering steady() from a stored member's post-spin-up state
# reorders the RMSE ranking on its own (rho 0.599 vs stored) -- so a treated
# ranking cannot be compared against the STORED ranking. It can only be compared
# against a control put through the identical pipeline. Hence paired, always.
#
#   ARMS=both     (default) valid paired comparison, ~15 h on 30 cores
#   ARMS=treated  corrected ranking only, ~7.5 h -- but then its absolute RMSE is
#                 NOT comparable with the stored table, only internally ranked
#
# NOTE: if you decide NOT to adopt the w_min correction, this run is unnecessary.
# For the uncorrected model the stored ranking is already exact (verified to
# 4.9e-15), so the refined top 10% is just: take the deduplicated ranking, drop
# members failing the clean-set filter, cut the first 184. Minutes, locally.
# See the manifest for that alternative.
#
# WHAT IT SAVES, AND WHY THAT MATTERS. Per member per arm it stores the
# post-steady() params and post-spin-up abundances. `39_...R` did this work and
# threw the states away, which is why they must be recomputed now. Keeping them
# makes the follow-on catchability fitting nearly free: `initial_effort` is 0 for
# all gears, so steady() and the spin-up are UNFISHED and catchability enters only
# the 1841-2010 projection -- it can never touch the initial condition. Starting
# from these cached states, one catchability evaluation is seconds rather than
# minutes. Set SAVE_STATES=0 to skip (~600 MB saved, but the catch-fit work then
# has to redo all of this).
#
# USAGE (from the repository root on the VM)
#   Rscript R/wmin_test/40_vm_project_survivors.R run       # chunked, resumable
#   Rscript R/wmin_test/40_vm_project_survivors.R collect   # rank and cut the 184
#
# ENV: VM_CORES (default 30), VM_CHUNK (default 50), ARMS (both|treated),
#      SAVE_STATES (1|0), VM_LIMIT (0 = all, else first N members for a smoke test)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel)
  library(dplyr); library(reshape2)
})

out_dir    <- "Output_large_files/wmin_test"
cache_dir  <- file.path(out_dir, "39_params_cache")     # reused from the 39 run
result_dir <- file.path(out_dir, "40_results")
state_dir  <- file.path(out_dir, "40_states")
for (d in c(result_dir, state_dir))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

PEN         <- "small divers"
W_NEW       <- 3626.667
REPRO_LEVEL <- 0.50
EREPRO_CAP  <- 0.95
STEADY_TOL  <- 0.002
STEADY_TMAX <- 1200
SPINUP_YEARS  <- 118
SPINUP_CYCLES <- 3
CORR_THRESHOLD <- 0.5
STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

CORES <- as.integer(Sys.getenv("VM_CORES", "30"))
CHUNK <- as.integer(Sys.getenv("VM_CHUNK", "50"))
ARMS  <- Sys.getenv("ARMS", "both")
SAVE_STATES <- as.integer(Sys.getenv("SAVE_STATES", "1")) == 1
LIMIT <- as.integer(Sys.getenv("VM_LIMIT", "0"))
mode  <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(mode)) mode <- "run"
stopifnot(ARMS %in% c("both", "treated"))

# --- the clean set ------------------------------------------------------------
D <- readRDS(file.path(out_dir, "39_full_ensemble_paired.rds"))
clean <- D %>% filter(trt_ok, trt_pass, trt_n_erepro_ge1 == 0,
                      trt_n_rmax_inf == 0, !repro_infeasible)
members <- clean$sim_index
if (LIMIT > 0) members <- head(members, LIMIT)
n_tot <- length(members)
chunks <- split(seq_len(n_tot), ceiling(seq_len(n_tot) / CHUNK))

# --- observed yield and effort windows (identical to yield_rmse_evaluation.R) --
effort_arr <- readRDS("effort_array_1841_2010.rds")
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

# --- an index of which cached chunk holds each member -------------------------
build_index <- function() {
  fs <- sort(list.files(cache_dir, pattern = "^chunk_\\d+\\.rds$", full.names = TRUE))
  if (!length(fs)) stop("no params cache in ", cache_dir,
                        " -- copy 39_params_cache/ from the source machine")
  do.call(rbind, lapply(fs, function(f) {
    z <- readRDS(f); data.frame(file = f, sim_index = z$sim_index,
                                pos = seq_along(z$sim_index),
                                stringsAsFactors = FALSE)
  }))
}

# ------------------------------------------------------------------ worker ----
worker <- function(j) {
  suppressPackageStartupMessages({
    library(therMizer); library(mizer); library(dplyr); library(reshape2)
  })
  p0 <- CHUNK_PARAMS[[j]]
  si <- CHUNK_SIM[j]
  eff <- CHUNK_EFF[[j]]
  i <- which(p0@species_params$species == PEN)

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
    fcv <- cv > STAB$cv_threshold
    ftr <- (abs(rs) > STAB$trend_rel_slope_max) & (pv < STAB$trend_pval_max)
    list(stable = !any(fcv | ftr, na.rm = TRUE),
         max_cv = max(cv[is.finite(cv)], na.rm = TRUE))
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

  # mizer 2.5.0 objects lack @second_order_w, which getYield dereferences.
  # validParams() is mizer's own upgrade and sets bin_average = FALSE, i.e. the
  # 2.5.0 branch -- verified bit-identical, reproduces the stored RMSE to 4.9e-15.
  yield_metrics <- function(sim) {
    sim@params <- suppressWarnings(validParams(sim@params))
    d <- reshape2::melt(getYield(sim))
    names(d) <- c("Year", "Species", "Yield_mod")
    d$Year <- as.numeric(as.character(d$Year))
    d$Species <- as.character(d$Species)
    d$Yield_mod <- pmax(d$Yield_mod, 0)
    cmp <- obs_long %>% left_join(d, by = c("Year", "Species")) %>%
      mutate(Yield_mod = coalesce(Yield_mod, 0))
    per_sp <- cmp %>% group_by(Species) %>%
      summarise(n = n(),
                sse = sum((log10(Yield_mod + 1) - log10(Yield_obs + 1))^2),
                obs_tot = sum(Yield_obs), mod_tot = sum(Yield_mod),
                .groups = "drop")
    list(rmse = sqrt(mean((log10(cmp$Yield_mod + 1) -
                             log10(cmp$Yield_obs + 1))^2, na.rm = TRUE)),
         cor_raw = suppressWarnings(cor(cmp$Yield_mod, cmp$Yield_obs,
                                        use = "complete.obs")),
         cor_log = suppressWarnings(cor(log10(cmp$Yield_mod + 1),
                                        log10(cmp$Yield_obs + 1),
                                        use = "complete.obs")),
         per_sp = per_sp)
  }

  run_arm <- function(p, tag) {
    warned <- FALSE
    ps <- withCallingHandlers(
      try(steady(p, tol = STEADY_TOL, t_max = STEADY_TMAX, preserve = c("erepro")),
          silent = TRUE),
      warning = function(w) {
        if (grepl("did not converge", conditionMessage(w), ignore.case = TRUE))
          warned <<- TRUE
        invokeRestart("muffleWarning") })
    if (inherits(ps, "try-error")) return(list(ok = FALSE, reason = "steady_error"))
    if (warned) return(list(ok = FALSE, reason = "steady_no_converge"))

    init <- NULL; stab <- NULL
    for (cyc in seq_len(SPINUP_CYCLES)) {
      # initial_n must be OMITTED, not passed as NULL -- project() does
      # params@initial_n[] <- initial_n and errors on a zero-length replacement.
      s <- try(if (is.null(init))
        project(ps, t_start = 1841, t_max = SPINUP_YEARS, effort = 0)
        else project(ps, t_start = 1841, t_max = SPINUP_YEARS, effort = 0,
                     initial_n = init), silent = TRUE)
      if (inherits(s, "try-error")) return(list(ok = FALSE, reason = "spinup_error"))
      init <- s@n[SPINUP_YEARS, , ]
      stab <- check_stability(s)
      if (isTRUE(stab$stable)) break
    }
    # t_max omitted deliberately: mizer takes the horizon from the effort array's
    # own time dimension. This is the pattern verified to 3e-15 in
    # 17_validate_rerun.R; passing t_max as well risks an off-by-one year.
    pr <- try(project(ps, initial_n = init, t_start = 1841, effort = eff),
              silent = TRUE)
    if (inherits(pr, "try-error")) return(list(ok = FALSE, reason = "projection_error"))
    ym <- try(yield_metrics(pr), silent = TRUE)
    if (inherits(ym, "try-error")) return(list(ok = FALSE, reason = "yield_error"))

    if (SAVE_STATES)
      saveRDS(list(sim_index = si, arm = tag, params = ps, initial_n = init),
              file.path(STATE_DIR, sprintf("state_%s_%05d.rds", tag, si)))

    sp <- ps@species_params; r <- rates0(ps)
    list(ok = TRUE, reason = "ok", rmse = ym$rmse, cor_raw = ym$cor_raw,
         cor_log = ym$cor_log, per_sp = ym$per_sp,
         stable = isTRUE(stab$stable), max_cv = stab$max_cv,
         pen_erepro = sp$erepro[i], pen_rmax = sp$R_max[i], pen_rdd = r$rdd[i],
         max_erepro = max(sp$erepro), n_rmax_inf = sum(!is.finite(sp$R_max)))
  }

  trt_p <- set_wmin(p0, W_NEW)
  r0 <- rates0(p0); rtt <- rates0(trt_p)
  fl <- p0@species_params$erepro[i] * r0$rdd[i] / rtt$rdi[i]
  e_new <- min(fl / (1 - REPRO_LEVEL), EREPRO_CAP)
  rdi_t <- rtt$rdi[i] * e_new / trt_p@species_params$erepro[i]
  R_new <- if (rdi_t > r0$rdd[i]) 1 / (1 / r0$rdd[i] - 1 / rdi_t) else Inf
  trt_p@species_params$erepro[i] <- e_new
  trt_p@species_params$R_max[i]  <- R_new

  trt <- run_arm(trt_p, "treated")
  ctl <- if (ARMS == "both") run_arm(p0, "control")
         else list(ok = NA, reason = "not_run")

  g <- function(a, f, d = NA) if (isTRUE(a$ok)) a[[f]] else d
  sp_rows <- NULL
  if (isTRUE(trt$ok)) sp_rows <- trt$per_sp %>% mutate(sim_index = si, arm = "treated")
  if (isTRUE(ctl$ok)) sp_rows <- bind_rows(sp_rows,
                                  ctl$per_sp %>% mutate(sim_index = si, arm = "control"))

  list(summary = data.frame(
      sim_index = si,
      trt_ok = isTRUE(trt$ok), trt_reason = trt$reason,
      trt_rmse = g(trt,"rmse"), trt_cor_raw = g(trt,"cor_raw"),
      trt_cor_log = g(trt,"cor_log"), trt_stable = g(trt,"stable"),
      trt_pen_erepro = g(trt,"pen_erepro"), trt_pen_rmax = g(trt,"pen_rmax"),
      trt_pen_rdd = g(trt,"pen_rdd"), trt_max_erepro = g(trt,"max_erepro"),
      trt_n_rmax_inf = g(trt,"n_rmax_inf"),
      ctl_ok = isTRUE(ctl$ok), ctl_reason = ctl$reason,
      ctl_rmse = g(ctl,"rmse"), ctl_cor_log = g(ctl,"cor_log"),
      ctl_stable = g(ctl,"stable"), ctl_pen_rdd = g(ctl,"pen_rdd"),
      rdd_ratio = if (isTRUE(trt$ok) && isTRUE(ctl$ok))
        trt$pen_rdd / ctl$pen_rdd else NA_real_,
      erepro_floor = fl, erepro_set = e_new,
      stringsAsFactors = FALSE),
    per_species = sp_rows)
}

# --------------------------------------------------------------------- run ----
if (mode == "run") {
  cat("=== VM RUN: project", n_tot, "clean members | arms =", ARMS, "===\n")
  cat("started", format(Sys.time()), "| cores", CORES, "| chunk", CHUNK,
      "| save_states", SAVE_STATES, "\n")
  idx <- build_index()
  miss <- setdiff(members, idx$sim_index)
  if (length(miss)) stop("params cache is missing ", length(miss), " member(s)")
  done <- list.files(result_dir, pattern = "^res_\\d+\\.rds$")
  per_pair <- if (ARMS == "both") 1.06 else 0.55       # min/member on 14 cores
  cat("chunks:", length(chunks), "| done:", length(done), "| est. remaining ~",
      round((length(chunks) - length(done)) * CHUNK * per_pair * 14 / CORES / 60, 1),
      "h\n\n")
  # Refuse to resume across a settings change. Existing results written under a
  # different VM_CHUNK / ARMS / member count do not correspond to the chunks this
  # run would compute, and skipping them would silently drop members.
  stale <- NULL
  for (ci in names(chunks)) {
    rf <- file.path(result_dir, sprintf("res_%03d.rds", as.integer(ci)))
    if (!file.exists(rf)) next
    m <- tryCatch(readRDS(rf)$meta, error = function(e) NULL)
    bad <- is.null(m) || !identical(as.integer(m$chunk), as.integer(CHUNK)) ||
      !identical(m$arms, ARMS) || !identical(as.integer(m$n_total), as.integer(n_tot))
    if (bad) stale <- c(stale, basename(rf))
  }
  if (length(stale)) {
    cat("\nSTOP: ", length(stale), " existing result file(s) were written under\n",
        "different settings (chunk size, arms, or member count) than this run.\n",
        sep = "")
    cat("Resuming would skip them and silently drop members.\n\n")
    cat("  offending: ", paste(head(stale, 5), collapse = ", "),
        if (length(stale) > 5) sprintf(" ... (+%d more)", length(stale) - 5) else "",
        "\n\n", sep = "")
    cat("This is what a leftover smoke test looks like. Clear and relaunch:\n")
    cat("  rm -f ", result_dir, "/*.rds ", state_dir, "/*.rds\n", sep = "")
    if (interactive()) stop("stale results present", call. = FALSE)
    quit(save = "no", status = 1)
  }

  t0 <- proc.time()
  for (ci in names(chunks)) {
    rf <- file.path(result_dir, sprintf("res_%03d.rds", as.integer(ci)))
    if (file.exists(rf)) { cat("chunk", ci, "done, skipping\n"); next }
    ms <- members[chunks[[ci]]]
    sel <- idx[match(ms, idx$sim_index), ]
    pk <- vector("list", length(ms)); ek <- vector("list", length(ms))
    for (f in unique(sel$file)) {
      z <- readRDS(f); w <- which(sel$file == f)
      for (q in w) {
        pk[[q]] <- z$params[[sel$pos[q]]]
        ek[[q]] <- effort_arr           # every member shares the effort forcing
      }
      rm(z)
    }
    cl <- makeCluster(CORES)
    clusterExport(cl, c("PEN","W_NEW","REPRO_LEVEL","EREPRO_CAP","STEADY_TOL",
                        "STEADY_TMAX","SPINUP_YEARS","SPINUP_CYCLES","STAB",
                        "obs_long","ARMS","SAVE_STATES","worker"),
                  envir = environment())
    assign("STATE_DIR", state_dir, envir = environment())
    assign("CHUNK_PARAMS", pk, envir = environment())
    assign("CHUNK_SIM", ms, envir = environment())
    assign("CHUNK_EFF", ek, envir = environment())
    clusterExport(cl, c("STATE_DIR","CHUNK_PARAMS","CHUNK_SIM","CHUNK_EFF"),
                  envir = environment())
    r <- parLapplyLB(cl, seq_along(pk), function(j)
      tryCatch(worker(j), error = function(e)
        list(summary = data.frame(sim_index = CHUNK_SIM[j], trt_ok = FALSE,
             trt_reason = paste("worker_error:", conditionMessage(e)),
             ctl_ok = FALSE, ctl_reason = "worker_error",
             stringsAsFactors = FALSE), per_species = NULL)))
    stopCluster(cl)
    # Record the provenance of this chunk so a later resume can detect that it
    # was written under different settings. Without this, running a 6-member
    # smoke test at VM_CHUNK=3 and then the real job at VM_CHUNK=50 would find
    # res_001.rds already present, SKIP it, and silently drop 50 members.
    saveRDS(list(summary = bind_rows(lapply(r, `[[`, "summary")),
                 per_species = bind_rows(lapply(r, `[[`, "per_species")),
                 meta = list(chunk = CHUNK, arms = ARMS, n_members = length(ms),
                             sim_index = ms, n_total = n_tot)), rf)
    el <- (proc.time() - t0)["elapsed"] / 60
    nd <- length(list.files(result_dir, pattern = "^res_\\d+\\.rds$"))
    cat(sprintf("chunk %s (%d/%d) | elapsed %.1f min | ETA %.1f h | %s\n",
                ci, nd, length(chunks), el,
                (el / max(1, nd - length(done))) * (length(chunks) - nd) / 60,
                format(Sys.time(), "%H:%M")))
    rm(pk, ek); invisible(gc())
  }
  cat("\nCOMPLETE. elapsed", round((proc.time() - t0)["elapsed"]/3600, 2), "h\n")
  cat("Now: Rscript R/wmin_test/40_vm_project_survivors.R collect\n")
  quit(save = "no")
}

# ----------------------------------------------------------------- collect ----
fs <- sort(list.files(result_dir, pattern = "^res_\\d+\\.rds$", full.names = TRUE))
stopifnot(length(fs) > 0)
Z <- lapply(fs, readRDS)
S <- bind_rows(lapply(Z, `[[`, "summary"))
P <- bind_rows(lapply(Z, `[[`, "per_species"))
cat("=== collected", nrow(S), "members from", length(fs), "chunks ===\n\n")
cat("treated ok:", sum(S$trt_ok), "| control ok:", sum(S$ctl_ok, na.rm = TRUE), "\n")
if (any(!S$trt_ok)) print(table(S$trt_reason[!S$trt_ok]))

ok <- S %>% filter(trt_ok)
rank_trt <- ok %>% arrange(trt_rmse) %>%
  mutate(rank = row_number(), pass_corr = !is.na(trt_cor_log) & trt_cor_log > CORR_THRESHOLD)
n_top <- ceiling(nrow(rank_trt) * 0.10)
cat("\n=== REFINED TOP 10% (corrected model) ===\n")
cat("  clean members projected:", nrow(rank_trt), "\n")
cat("  refined top 10% = ceiling(", nrow(rank_trt), "* 0.10) =", n_top, "\n")
cat("  passing cor_log >", CORR_THRESHOLD, ":", sum(rank_trt$pass_corr[1:n_top]),
    "of", n_top, "\n")
new_top <- rank_trt$sim_index[1:n_top]

old <- read.csv("Manuscript data/yield_rmse_per_sim.csv") %>% arrange(rank)
old_top <- old$sim_index[1:ceiling(nrow(old) * 0.10)]
cat("\n  vs the ORIGINAL top 212: retained", sum(new_top %in% old_top),
    "| new", sum(!new_top %in% old_top), "\n")

if (any(S$ctl_ok, na.rm = TRUE)) {
  pr <- S %>% filter(trt_ok, ctl_ok)
  d <- pr$trt_rmse - pr$ctl_rmse
  cat("\n=== paired: does the CORRECTION move the ranking? ===\n")
  cat("  n", nrow(pr), "| median diff", signif(median(d), 4),
      "| Spearman", signif(cor(rank(pr$ctl_rmse), rank(pr$trt_rmse),
                               method = "spearman"), 6), "\n")
  ct <- pr %>% arrange(ctl_rmse) %>% slice_head(n = n_top) %>% pull(sim_index)
  cat("  control top", n_top, "vs treated top", n_top, ": shared",
      length(intersect(ct, new_top)), "| differ", n_top - length(intersect(ct, new_top)), "\n")
  cat("\n  NOTE: the re-run itself reorders the ranking (rho ~0.6 vs stored).\n")
  cat("  Only treated-minus-control is attributable to the correction.\n")
}

cat("\n=== per-species share of total squared error (treated, top 10%) ===\n")
print(as.data.frame(P %>% filter(arm == "treated", sim_index %in% new_top) %>%
  group_by(Species) %>%
  summarise(mean_sse = mean(sse), rmse_sp = sqrt(mean(sse / n)),
            obs_total = mean(obs_tot), mod_total = mean(mod_tot), .groups = "drop") %>%
  mutate(pct_of_error = 100 * mean_sse / sum(mean_sse),
         mod_over_obs = mod_total / obs_total) %>%
  arrange(desc(pct_of_error)) %>%
  select(Species, pct_of_error, rmse_sp, obs_total, mod_total, mod_over_obs)),
  digits = 4, row.names = FALSE)
cat("\n  ^ this is the starting diagnostic for the catchability-fitting work\n")

saveRDS(list(summary = S, per_species = P, ranking = rank_trt,
             refined_top = new_top, n_top = n_top),
        file.path(out_dir, "40_refined_ranking.rds"))
write.csv(rank_trt, file.path(out_dir, "40_refined_ranking.csv"), row.names = FALSE)
write.csv(P, file.path(out_dir, "40_per_species_yield.csv"), row.names = FALSE)
cat("\nWrote 40_refined_ranking.{rds,csv} and 40_per_species_yield.csv\n")
