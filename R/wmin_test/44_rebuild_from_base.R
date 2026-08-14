# =============================================================================
# PHASE 3/4 -- rebuild each ensemble member from the RECALIBRATED BASE params plus
# its recovered Monte Carlo draws, instead of re-steadying its stored
# post-spin-up state.
#
# THE PROBLEM THIS SOLVES. Scripts 39 and 40 took each member's STORED params --
# whose initial_n is the post-spin-up state -- and re-entered steady() on it. That
# is not what the original Monte Carlo did, and it is an artefact: it halves the
# unexploited baleen whale stock at 1841, worsens the yield fit in 89% of 1,831
# members, and drives the biomass SNR from -0.92 to -0.38
# (docs/SNR_regression_FINDING.md). The stored state was reached under
# time-varying therMizer forcing, so it is already off the steady state steady()
# targets; re-entering it is a second, uncalibrated optimisation.
#
# WHAT THIS DOES INSTEAD. Replicates run_single_enhanced_sim
# (09_Uncertainty_Analysis.Rmd:331-518) exactly: start from the calibrated base,
# apply the three draws, THEN steady() -> unfished spin-up -> 1841-2010 projection.
# Whether that reproduces the original is an empirical question, which is what the
# `stored` arm below is for. Do not assume it; measure it.
#
# ARMS
#   stored   raw Manuscript data/params_sel_adj.rds, w_min UNCORRECTED.
#            THE REPRODUCTION CHECK. Its yield RMSE should land close to
#            Manuscript data/yield_rmse_per_sim.csv. If it does, rebuilding from
#            base + draws is a faithful replication of the original pipeline and
#            the whole approach is validated. If it does not, stop and reconsider
#            before spending the full run.
#   control  42_control_biocal.rds -- recalibrated, w_min UNCORRECTED
#   treated  params_sel_adj_wmin_corrected_biocal.rds -- recalibrated, w_min CORRECTED
#
# Only treated - control is attributable to the w_min correction. control - stored
# is what the recalibration costs on its own. Never compare a new absolute against
# a stored value (docs/small_divers_wmin_stageD2_results.md).
#
# HOW THE DRAWS MUST BE APPLIED -- verified against mizer 3.1.0, not assumed:
#   species_params(p) <- sp   DOES rebuild search_vol AND preserves the therMizer
#                             rates_funcs and resource_dynamics.
#   p@species_params$gamma <- ...   does NOT rebuild search_vol. The draw would
#                             silently do nothing. Direct slot assignment is wrong
#                             here; the original uses the replacement functions and
#                             so does this.
#   initial_n scaling is a direct slot operation, as in the original (09:382).
#
# USAGE (from the repository root)
#   Rscript R/wmin_test/44_rebuild_from_base.R run
#   Rscript R/wmin_test/44_rebuild_from_base.R collect
#
# ENV: VM_CORES (default 30), VM_CHUNK (50), ARMS (comma list, default
#      "control,treated"), SAVE_STATES (1|0), VM_LIMIT (0 = all)
#
# Reproduction check, ~10 min locally:
#   ARMS=stored VM_LIMIT=40 VM_CHUNK=10 VM_CORES=8 Rscript ... run
#   ARMS=stored VM_LIMIT=40 Rscript ... collect
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel)
  library(dplyr); library(reshape2)
})

out_dir    <- "Output_large_files/wmin_test"
result_dir <- file.path(out_dir, "44_results")
state_dir  <- file.path(out_dir, "44_states")
for (d in c(result_dir, state_dir))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

PEN <- "small divers"
BALEEN <- "baleen whales"
# 09_Uncertainty_Analysis.Rmd:1581-1591 -- the PRODUCTION settings, not the
# 0.002/1200 that scripts 39 and 40 used.
STEADY_TOL    <- 0.0025
STEADY_TMAX   <- 1500
SPINUP_YEARS  <- 118
# The driver never overrode the blocked runner's default of 1 (09:857-859), so the
# real ensemble had exactly ONE 118-year unfished cycle. 40_* used 3.
SPINUP_CYCLES <- as.integer(Sys.getenv("SPINUP_CYCLES", "1"))
CORR_THRESHOLD <- 0.5
STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

CORES <- as.integer(Sys.getenv("VM_CORES", "30"))
CHUNK <- as.integer(Sys.getenv("VM_CHUNK", "50"))
ARMS  <- trimws(strsplit(Sys.getenv("ARMS", "control,treated"), ",")[[1]])
SAVE_STATES <- as.integer(Sys.getenv("SAVE_STATES", "1")) == 1
LIMIT <- as.integer(Sys.getenv("VM_LIMIT", "0"))
mode  <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(mode)) mode <- "run"
stopifnot(all(ARMS %in% c("stored", "control", "treated")))

# --- base params, one per arm ------------------------------------------------
BASE_PATH <- c(stored  = "Manuscript data/params_sel_adj.rds",
               control = file.path(out_dir, "42_control_biocal.rds"),
               treated = "params_sel_adj_wmin_corrected_biocal.rds")

load_base <- function(arm) {
  p <- suppressWarnings(validParams(readRDS(BASE_PATH[[arm]])))
  # The one numerical difference mizer 3.1.0 could introduce. NEWS.md: the
  # first-order path is byte-identical to previous mizer, and this is it.
  stopifnot(identical(p@second_order_w$flux, "upwind"),
            isFALSE(p@second_order_w$bin_average),
            identical(p@rates_funcs$Encounter, "therMizerEncounter"),
            identical(p@resource_dynamics, "plankton_forcing"))
  i <- which(p@species_params$species == PEN)
  expect_corrected <- arm == "treated"
  is_corrected <- p@species_params$w_min[i] > 1
  if (is_corrected != expect_corrected)
    stop("arm '", arm, "': penguin w_min is ", p@species_params$w_min[i],
         " but this arm expects w_min ",
         if (expect_corrected) "CORRECTED" else "at the stored 0.001")
  p
}

# --- the members and their draws ---------------------------------------------
DR <- readRDS(file.path(out_dir, "43_member_draws.rds"))
# The DRAW-distinct set (1,848), not the 1,997 that 33_dedupe_full deduplicated on
# post-spin-up state. Rebuilding is deterministic in the draws, so running a
# repeated draw twice yields two identical members and re-inflates the ensemble
# with exactly the redundancy the deduplication existed to remove. See 43_*.R.
members <- DR$members_distinct
if (is.null(members)) stop("43_member_draws.rds predates the draw-level dedupe; re-run `build`")

# Order by the STORED deduplicated RMSE rank, so a VM_LIMIT pilot draws the
# best-fitting members -- the ones the fitted ensemble is actually made of, and
# the ones for which cached ground truth exists (all of the top 40 are in
# biomass_top10pct_raw_clim.rds). It is also where the re-steady artefact does its
# damage: the top 200 degrade ~3.5x more than the ensemble average.
#
# VM_ORDER=random is the right choice for CHARACTERISING the new ensemble.
# Ordering by the stored rank selects members that ranked well in the published
# ensemble, where part of the whale stock came from basin selection rather than
# from the draw -- so those members cannot come back, and a pilot drawn from them
# understates the rebuild. A representative sample is what answers "is the new
# ensemble any good on its own terms".
ord <- Sys.getenv("VM_ORDER", "rank")
if (ord == "rank") {
  rk <- read.csv("Manuscript data/yield_rmse_per_sim_deduped.csv")
  rk <- rk[rk$sim_index %in% members, ]
  members <- rk$sim_index[order(rk$rank)]
} else if (ord == "random") {
  set.seed(20260803)
  members <- sample(members)
}
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

# ------------------------------------------------------------------ worker ----
worker <- function(j) {
  suppressPackageStartupMessages({
    library(therMizer); library(mizer); library(dplyr); library(reshape2)
  })
  si <- CHUNK_SIM[j]
  dw_ <- CHUNK_DRAW[[j]]

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

  yield_metrics <- function(sim) {
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

  # THE NON-CONVERGENCE GUARD. Verified against the installed mizer 3.1.0:
  # projectToSteady.MizerParams signals with message("Simulation run did not
  # converge after ", ...). The only warning() there is the extinction notice.
  # 30:100-104, 39:152-160 and 40:192-198 all listen for `warning` and have
  # therefore NEVER rejected a non-converged member. Listen for message.
  steady_guarded <- function(p) {
    no_conv <- FALSE
    out <- withCallingHandlers(
      try(steady(p, tol = STEADY_TOL, t_max = STEADY_TMAX,
                 preserve = c("erepro"), progress_bar = FALSE), silent = TRUE),
      message = function(m) {
        if (grepl("did not converge", conditionMessage(m), ignore.case = TRUE))
          no_conv <<- TRUE
        invokeRestart("muffleMessage")
      },
      warning = function(w) invokeRestart("muffleWarning"))
    list(params = out, converged = !no_conv)
  }

  # Apply the three draws exactly as run_single_enhanced_sim does (09:337-402).
  apply_draws <- function(p) {
    gp <- gear_params(p)
    key_base <- paste(gp$gear, gp$species)
    # the draw's catchability is named "<gear> <species>" from the member's own
    # gear_params; the rows must line up with the base's before we overwrite
    if (!identical(key_base, names(dw_$catchability)))
      stop("gear_params rows differ between the base and the stored member")
    gp$catchability <- as.numeric(dw_$catchability)
    gear_params(p) <- gp

    sp <- species_params(p)
    if (!identical(sp$species, names(dw_$gamma)))
      stop("species order differs between the base and the draw")
    sp$gamma <- as.numeric(dw_$gamma)
    species_params(p) <- sp          # rebuilds search_vol, keeps therMizer

    sc <- as.numeric(dw_$abundance_scaling[sp$species])
    for (k in seq_along(sc)) p@initial_n[k, ] <- p@initial_n[k, ] * sc[k]
    p
  }

  run_arm <- function(arm) {
    p <- try(apply_draws(BASE[[arm]]), silent = TRUE)
    if (inherits(p, "try-error"))
      return(list(ok = FALSE, reason = paste("draw_error:", as.character(p))))

    st <- steady_guarded(p)
    if (inherits(st$params, "try-error"))
      return(list(ok = FALSE, reason = "steady_error"))
    if (!st$converged) return(list(ok = FALSE, reason = "steady_no_converge"))
    ps <- st$params

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
    # own time dimension. Verified to 3e-15 in 17_validate_rerun.R; passing t_max
    # as well risks an off-by-one year.
    pr <- try(project(ps, initial_n = init, t_start = 1841, effort = CHUNK_EFF),
              silent = TRUE)
    if (inherits(pr, "try-error")) return(list(ok = FALSE, reason = "projection_error"))
    ym <- try(yield_metrics(pr), silent = TRUE)
    if (inherits(ym, "try-error")) return(list(ok = FALSE, reason = "yield_error"))

    if (SAVE_STATES)
      saveRDS(list(sim_index = si, arm = arm, params = ps, initial_n = init),
              file.path(STATE_DIR, sprintf("state_%s_%05d.rds", arm, si)))

    sp <- ps@species_params
    i_pen <- which(sp$species == PEN); i_bw <- which(sp$species == BALEEN)
    wdw <- ps@w * ps@dw
    # The unexploited 1841 biomass IS the post-spin-up state -- the climate-only
    # run starts here. This is the decisive gate metric: the re-steadied pipeline
    # halves the baleen whale stock (3.02e12 stored -> 1.44e12), visible at t = 0
    # before any drift.
    bio_init <- as.numeric(init %*% wdw)
    r <- getRates(ps, n = ps@initial_n, n_pp = ps@initial_n_pp,
                  n_other = ps@initial_n_other, effort = 0, t = 1841)

    list(ok = TRUE, reason = "ok", rmse = ym$rmse, cor_raw = ym$cor_raw,
         cor_log = ym$cor_log, per_sp = ym$per_sp,
         stable = isTRUE(stab$stable), max_cv = stab$max_cv, cycles = cyc,
         bw_1841 = bio_init[i_bw], pen_1841 = bio_init[i_pen],
         total_1841 = sum(bio_init),
         pen_erepro = sp$erepro[i_pen], pen_rmax = sp$R_max[i_pen],
         pen_rdd = r$rdd[i_pen],
         max_erepro = max(sp$erepro), n_erepro_ge1 = sum(sp$erepro >= 1),
         n_rmax_inf = sum(!is.finite(sp$R_max)))
  }

  res <- lapply(setNames(ARMS, ARMS), run_arm)
  g <- function(a, f, d = NA) if (isTRUE(res[[a]]$ok)) res[[a]][[f]] else d

  row <- data.frame(sim_index = si, stringsAsFactors = FALSE)
  for (a in ARMS) {
    row[[paste0(a, "_ok")]]      <- isTRUE(res[[a]]$ok)
    row[[paste0(a, "_reason")]]  <- res[[a]]$reason
    for (f in c("rmse", "cor_raw", "cor_log", "stable", "max_cv", "cycles",
                "bw_1841", "pen_1841", "total_1841", "pen_erepro", "pen_rmax",
                "pen_rdd", "max_erepro", "n_erepro_ge1", "n_rmax_inf"))
      row[[paste0(a, "_", f)]] <- g(a, f)
  }
  if (all(c("treated", "control") %in% ARMS) &&
      isTRUE(res$treated$ok) && isTRUE(res$control$ok))
    row$rdd_ratio <- res$treated$pen_rdd / res$control$pen_rdd

  sp_rows <- NULL
  for (a in ARMS)
    if (isTRUE(res[[a]]$ok))
      sp_rows <- bind_rows(sp_rows,
                           res[[a]]$per_sp %>% mutate(sim_index = si, arm = a))

  list(summary = row, per_species = sp_rows)
}

# --------------------------------------------------------------------- run ----
if (mode == "run") {
  cat("=== 44 REBUILD FROM BASE:", n_tot, "members | arms:",
      paste(ARMS, collapse = ","), "===\n")
  cat("started", format(Sys.time()), "| cores", CORES, "| chunk", CHUNK,
      "| spinup_cycles", SPINUP_CYCLES, "| save_states", SAVE_STATES, "\n")
  cat("steady: tol", STEADY_TOL, "t_max", STEADY_TMAX, "(the 09:1581-1591 production settings)\n")
  BASE <- lapply(setNames(ARMS, ARMS), load_base)
  for (a in ARMS)
    cat(sprintf("  base %-8s %-52s penguin w_min %.4g\n", a, BASE_PATH[[a]],
                BASE[[a]]@species_params$w_min[
                  which(BASE[[a]]@species_params$species == PEN)]))
  gp0 <- gear_params(BASE[[ARMS[1]]])
  DRAW_GEAR_KEY <- paste(gp0$gear, gp0$species)

  # Refuse to resume across a settings change: results written under a different
  # chunk size, arm set or member count do not correspond to the chunks this run
  # would compute, and skipping them would silently drop members.
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
        "different settings than this run. Resuming would silently drop members.\n",
        "  offending: ", paste(head(stale, 5), collapse = ", "), "\n\n",
        "Clear and relaunch:\n  rm -f ", result_dir, "/*.rds ", state_dir, "/*.rds\n",
        sep = "")
    quit(save = "no", status = 1)
  }

  t0 <- proc.time()
  done0 <- length(list.files(result_dir, pattern = "^res_\\d+\\.rds$"))
  for (ci in names(chunks)) {
    rf <- file.path(result_dir, sprintf("res_%03d.rds", as.integer(ci)))
    if (file.exists(rf)) { cat("chunk", ci, "done, skipping\n"); next }
    ms <- members[chunks[[ci]]]
    dks <- DR$draws[as.character(ms)]

    cl <- makeCluster(CORES)
    assign("STATE_DIR", state_dir, envir = environment())
    assign("CHUNK_SIM", ms, envir = environment())
    assign("CHUNK_DRAW", dks, envir = environment())
    assign("CHUNK_EFF", effort_arr, envir = environment())
    clusterExport(cl, c("PEN", "BALEEN", "STEADY_TOL", "STEADY_TMAX",
                        "SPINUP_YEARS", "SPINUP_CYCLES", "STAB", "obs_long",
                        "ARMS", "SAVE_STATES", "BASE", "DRAW_GEAR_KEY",
                        "STATE_DIR", "CHUNK_SIM", "CHUNK_DRAW", "CHUNK_EFF",
                        "worker"),
                  envir = environment())
    r <- parLapplyLB(cl, seq_along(ms), function(j)
      tryCatch(worker(j), error = function(e) {
        row <- data.frame(sim_index = CHUNK_SIM[j], stringsAsFactors = FALSE)
        for (a in ARMS) {
          row[[paste0(a, "_ok")]] <- FALSE
          row[[paste0(a, "_reason")]] <- paste("worker_error:", conditionMessage(e))
        }
        list(summary = row, per_species = NULL)
      }))
    stopCluster(cl)

    saveRDS(list(summary = bind_rows(lapply(r, `[[`, "summary")),
                 per_species = bind_rows(lapply(r, `[[`, "per_species")),
                 meta = list(chunk = CHUNK, arms = ARMS, n_members = length(ms),
                             sim_index = ms, n_total = n_tot,
                             steady_tol = STEADY_TOL, steady_tmax = STEADY_TMAX,
                             spinup_cycles = SPINUP_CYCLES)), rf)
    el <- (proc.time() - t0)["elapsed"] / 60
    nd <- length(list.files(result_dir, pattern = "^res_\\d+\\.rds$"))
    cat(sprintf("chunk %s (%d/%d) | elapsed %.1f min | ETA %.1f h | %s\n",
                ci, nd, length(chunks), el,
                (el / max(1, nd - done0)) * (length(chunks) - nd) / 60,
                format(Sys.time(), "%H:%M")))
    invisible(gc())
  }
  cat("\nCOMPLETE. elapsed", round((proc.time() - t0)["elapsed"] / 3600, 2), "h\n")
  cat("Now: Rscript R/wmin_test/44_rebuild_from_base.R collect\n")
  quit(save = "no")
}

# ----------------------------------------------------------------- collect ----
fs <- sort(list.files(result_dir, pattern = "^res_\\d+\\.rds$", full.names = TRUE))
stopifnot(length(fs) > 0)
Z <- lapply(fs, readRDS)
S <- bind_rows(lapply(Z, `[[`, "summary"))
P <- bind_rows(lapply(Z, `[[`, "per_species"))
arms_run <- Z[[1]]$meta$arms
cat("=== collected", nrow(S), "members from", length(fs), "chunks | arms:",
    paste(arms_run, collapse = ","), "===\n\n")

for (a in arms_run) {
  okc <- sum(S[[paste0(a, "_ok")]], na.rm = TRUE)
  cat(sprintf("%-8s ok %d/%d\n", a, okc, nrow(S)))
  bad <- S[[paste0(a, "_reason")]][!S[[paste0(a, "_ok")]]]
  if (length(bad)) print(table(bad))
}

# --- THE NON-CONVERGENCE GUARD SANITY CHECK ---------------------------------
# 0 of 3,994 rejections fired in the previous pipeline because every handler
# listened for warning() instead of message(). If this is still 0, the handler is
# wrong again -- do not read that as "every member converged".
nc <- sum(unlist(lapply(arms_run, function(a)
  sum(S[[paste0(a, "_reason")]] == "steady_no_converge", na.rm = TRUE))))
cat(sprintf("\nsteady_no_converge rejections: %d of %d arm-runs\n",
            nc, nrow(S) * length(arms_run)))
if (nc == 0)
  cat("  NOTE: zero. Verify the message-based handler actually fires before\n",
      "  concluding that every member converged.\n", sep = "")

# --- THE GATE, PAIRED PER MEMBER ---------------------------------------------
# Every comparison below is against THE SAME member's stored value, never against
# an ensemble median. A median-vs-median test would be meaningless on a pilot,
# because a subset of arbitrary members has no reason to match the full
# ensemble's median (docs/small_divers_wmin_stageD2_results.md: never compare a
# re-run absolute against a stored value except paired).
cat("\n=== GATE (paired per member) ===\n")
old <- read.csv("Manuscript data/yield_rmse_per_sim.csv")
top_idx <- old$sim_index[order(old$rank)][seq_len(ceiling(nrow(old) * 0.10))]
bc <- readRDS("Manuscript data/biomass_top10pct_raw_clim.rds")
bw_ref <- bc[bc$Year == 1841 & bc$Species == "baleen whales", ]
bw_ref$sim_index <- top_idx[bw_ref$sim_i]      # sim_i is rank position, not sim_index

for (a in arms_run) {
  ok <- which(S[[paste0(a, "_ok")]])
  if (!length(ok)) next
  si <- S$sim_index[ok]
  cat(sprintf("\n%-8s n=%d\n", a, length(ok)))

  # 1 -- unexploited baleen whale biomass at 1841, rebuilt / stored
  jb <- match(si, bw_ref$sim_index)
  if (any(!is.na(jb))) {
    ratio <- S[[paste0(a, "_bw_1841")]][ok] / bw_ref$Biomass[jb]
    med <- median(ratio, na.rm = TRUE)
    cat(sprintf("  1: baleen whale 1841, rebuilt/stored   median %.4f  (n=%d paired)   %s\n",
                med, sum(!is.na(ratio)), if (!is.na(med) && med >= 0.80) "PASS" else "FAIL"))
    cat(sprintf("       IQR [%.3f, %.3f]   the 39/40 re-steady pipeline gave ~0.48\n",
                quantile(ratio, 0.25, na.rm = TRUE), quantile(ratio, 0.75, na.rm = TRUE)))
  } else cat("  1: no cached baleen reference for these members (not in the old top 212)\n")

  # 2 -- yield RMSE, paired difference against the stored table
  jr <- match(si, old$sim_index)
  d <- S[[paste0(a, "_rmse")]][ok] - old$rmse[jr]
  medd <- median(d, na.rm = TRUE)
  cat(sprintf("  2: yield RMSE, rebuilt - stored        median %+.5f (%+.2f%%)      %s\n",
              medd, 100 * median(d / old$rmse[jr], na.rm = TRUE),
              if (!is.na(medd) && medd <= 0.01) "PASS" else "FAIL"))
  cat(sprintf("       the 39/40 re-steady pipeline gave +0.0413 (+2.43%%) on the top 200\n"))

  # 3 -- rank correlation against the stored ranking
  rho <- suppressWarnings(cor(S[[paste0(a, "_rmse")]][ok], old$rmse[jr],
                              method = "spearman", use = "complete.obs"))
  cat(sprintf("  3: Spearman rho vs stored ranking      %.4f                        %s\n",
              rho, if (!is.na(rho) && rho >= 0.85) "PASS" else "FAIL"))
  cat("       the 39/40 re-steady pipeline gave 0.599 (0.562 on the top 200)\n")

  if (a == "stored")
    cat(sprintf("  REPRODUCTION CHECK: max abs RMSE difference %.5f over %d members\n",
                max(abs(d), na.rm = TRUE), sum(!is.na(d))))
}

if (all(c("treated", "control") %in% arms_run)) {
  pr <- S %>% filter(treated_ok, control_ok)
  if (nrow(pr)) {
    d <- pr$treated_rmse - pr$control_rmse
    cat("\n=== paired: does the CORRECTION move the ranking? ===\n")
    cat("  n", nrow(pr), "| median treated-control RMSE", signif(median(d), 4),
        "| Spearman", signif(cor(rank(pr$control_rmse), rank(pr$treated_rmse),
                                 method = "spearman"), 6), "\n")
    cat("  median baleen whale 1841 treated/control:",
        signif(median(pr$treated_bw_1841 / pr$control_bw_1841), 6), "\n")
  }
}

saveRDS(list(summary = S, per_species = P, arms = arms_run),
        file.path(out_dir, "44_rebuild_results.rds"))
write.csv(S, file.path(out_dir, "44_rebuild_summary.csv"), row.names = FALSE)
cat("\nWrote 44_rebuild_results.rds and 44_rebuild_summary.csv\n")
