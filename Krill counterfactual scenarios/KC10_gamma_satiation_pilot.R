# =============================================================================
# KC10 -- are whales unresponsive to krill fishing because they are SATIATED?
#
# THE QUESTION. KC04/KC06/KC08 found whale abundance and body size unmoved by
# krill fishing. Two buffers were tested one at a time and neither explained it:
# halving whale access to the undepletable background resource (phase 55) grew
# the response only 1.3x, and releasing the reproduction clamp on its own did
# nothing. A third candidate has never been tested directly -- SATIATION.
#
# gamma is the ONLY physiological parameter the Monte Carlo perturbs (h, beta,
# sigma all come from the base), and apply_draws() ASSIGNS the drawn value over
# the base one. Over the top 10 cut-A members the drawn/base ratio is:
#
#            384    71  1769   411   446   826   173  1862   757  1055
#   baleen   6.25  6.25  1.12  6.62  9.73  6.25  21.5  0.72  6.25  6.25
#   minke    1708  1708  1.17   112  13.7  13.7  34.5  51.6  18.1  13.7
#
# so members sit at feeding level ~0.977 where the phase-55 reference sits at
# 0.790 (baleen) / 0.491 (minke). gamma is byte-identical across the whole base
# lineage (biocal = phase 51 = 54 = 55), so those ARE the MC multipliers.
#
# THE DESIGN. 10 members x 5 gamma arms x 2 reproduction arms x 4 effort arms.
# Everything is paired within the member, and the protocol is byte-identical to
# KC07 up to and including steady(), so gamma is the only thing that differs.
#
#   gamma arm   species        gamma
#   drawn       --             drawn                       <- the control
#   wh_mid      baleen+minke   sqrt(g_base * g_drawn)
#   wh_base     baleen+minke   base
#   all_mid     all 19         sqrt(g_base * g_drawn)
#   all_base    all 19         base
#
#   repro arm   asis | rl25 = setBevertonHolt(reproduction_level = 0.25) on the
#               two whale groups, applied AFTER steady() so there is no
#               `preserve` confound. The base is clamped at 0.9975 (baleen) /
#               0.9222 (minke), so relieving satiation alone may not free
#               numbers -- the crossed cell is the untested one.
#
# The mid rungs are not decoration: minke gamma falls 1708x in members 384/71
# and 112x in 411, so the *_base arms may fail steady(). A dose-response
# survives losing the endpoint.
#
# WHAT IS SAVED. Everything KC07 saved (biomass, ABUNDANCE, krill/resource/
# total consumption, feeding level, yield) plus the mechanism variables a null
# would otherwise be uninterpretable without: per-member reproduction level for
# all 19 groups, realised whale gamma and its ratio to base, and erepro/R_max
# for the whale groups. Full prey-resolved diet is OFF by default -- 19x20 over
# 100 jobs is far too large and nothing downstream needs it.
#
# THIS IS A MECHANISM DIAGNOSTIC, NOT A RECALIBRATED ENSEMBLE. It deliberately
# takes members off their yield calibration. Nothing here may re-rank members or
# rebuild a manuscript figure.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC10_gamma_satiation_pilot.R" [run|collect|status]
# ENV    KC10_N (10), KC10_CORES (10), KC10_DIET_FROM (1841), KC10_FULL_DIET (0),
#        KC10_GAMMA_ARMS, KC10_REPRO_ARMS, KC10_RL (0.25), KC10_BASE, KC10_STEM
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OUT_LARGE <- "Output_large_files/wmin_test"
KC_ROOT   <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
ANA <- file.path(KC_ROOT, "analysis")
BASE_FILE <- Sys.getenv("KC10_BASE",
                        "params_ref_sw2000_balror_mnkfish05_whres05.rds")

GAMMA_ARMS_ALL <- c("drawn", "wh_mid", "wh_base", "all_mid", "all_base")
REPRO_ARMS_ALL <- c("asis", "rl25")
pick <- function(env, all) {
  v <- Sys.getenv(env, paste(all, collapse = ","))
  v <- trimws(strsplit(v, ",")[[1]]); v <- v[nzchar(v)]
  if (length(setdiff(v, all)))
    stop(env, " must be a subset of: ", paste(all, collapse = ", "),
         call. = FALSE)
  # keep the canonical order so `drawn`/`asis` is always the reference level
  all[all %in% v]
}
GA <- pick("KC10_GAMMA_ARMS", GAMMA_ARMS_ALL)
RP <- pick("KC10_REPRO_ARMS", REPRO_ARMS_ALL)

N         <- as.integer(Sys.getenv("KC10_N", "10"))
DIET_FROM <- as.integer(Sys.getenv("KC10_DIET_FROM", "1841"))
FULL_DIET <- Sys.getenv("KC10_FULL_DIET", "0") != "0"
RL_TARGET <- as.numeric(Sys.getenv("KC10_RL", "0.25"))
# 16 cores on this machine; never commit all of them.
CORES     <- min(as.integer(Sys.getenv("KC10_CORES", "10")), 15,
                 max(1, N * length(GA) * length(RP)))
# A trimmed grid gets its own stem so a smoke test cannot clobber the full run.
FULLGRID  <- identical(GA, GAMMA_ARMS_ALL) && identical(RP, REPRO_ARMS_ALL)
STEM      <- Sys.getenv("KC10_STEM",
                        sprintf("KC10_gamma_n%d%s", N,
                                if (FULLGRID) "" else "_partial"))
CHUNK_DIR <- file.path(OUT_LARGE, paste0(STEM, "_chunks"))
OUT_RDS   <- file.path(OUT_LARGE, paste0(STEM, ".rds"))
for (d in c(ANA, CHUNK_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"
stopifnot(mode %in% c("run", "collect", "status"))

STEADY_TOL <- 0.0025; STEADY_TMAX <- 1500; SPINUP_YEARS <- 118
QMAX <- 1; KRILL <- "antarctic krill"; END_YEAR <- 2010
WHALES <- c("baleen whales", "minke whales")
STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

t0 <- proc.time()
cat("=== KC10: gamma / satiation pilot ===\n")
cat("base      :", BASE_FILE, "\n")
cat("gamma arms:", paste(GA, collapse = ", "), "\n")
cat("repro arms:", paste(RP, collapse = ", "),
    if ("rl25" %in% RP) sprintf("(reproduction_level = %.2f)", RL_TARGET) else "",
    "\n")
if (!file.exists(BASE_FILE))
  stop("missing base params: ", BASE_FILE,
       "\n  run R/wmin_test/55_whale_resource_interaction.R first", call. = FALSE)

# --- members, in post-refit rank order (KC07:96-104) -------------------------
CUTS <- readRDS(file.path(OUT_LARGE, "46_selection_cuts.rds"))
cutA <- as.integer(CUTS$cuts[["A unweighted RMSE"]])
RF <- readRDS(file.path(OUT_LARGE, "45_refit_results.rds"))
chk <- RF$per_species %>% group_by(sim_index) %>%
  summarise(m = sqrt(sum(sse) / sum(n)), .groups = "drop") %>%
  arrange(m) %>% head(length(cutA)) %>% pull(sim_index)
if (!identical(as.integer(chk), cutA))
  stop("cut A does not match the post-refit ranking -- refusing to proceed.")
members <- head(cutA, N)
DR <- readRDS(file.path(OUT_LARGE, "43_member_draws.rds"))
if (length(setdiff(as.character(members), names(DR$draws))))
  stop("missing recovered draws")
if (!all(members %in% DR$members_distinct))
  stop("some members are not in the draw-distinct set")
MULT <- readRDS(file.path(OUT_LARGE, "45_catchability_multipliers.rds"))$M
cat("members   :", paste(members, collapse = ", "), "\n")

# --- base, and the assertions the whole lineage relies on --------------------
BASE <- suppressWarnings(validParams(readRDS(BASE_FILE)))
stopifnot(identical(BASE@rates_funcs$Encounter, "therMizerEncounter"),
          identical(BASE@resource_dynamics, "plankton_forcing"),
          identical(BASE@second_order_w$flux, "upwind"),
          isFALSE(BASE@second_order_w$bin_average))
SP <- BASE@species_params$species
i_kr <- which(SP == KRILL); i_wh <- match(WHALES, SP)
stopifnot(!anyNA(i_wh), length(i_kr) == 1)
G_BASE <- BASE@species_params$gamma
names(G_BASE) <- SP

# The manipulation, stated in the log before anything is run.
GR <- vapply(members, function(m)
  as.numeric(DR$draws[[as.character(m)]]$gamma)[i_wh] / G_BASE[i_wh], numeric(2))
dimnames(GR) <- list(WHALES, members)
cat("\ndrawn / base gamma, whales:\n")
print(round(GR, 2))
cat(sprintf("\nbase gamma: baleen %.4g, minke %.4g | base interaction_resource: %s\n",
            G_BASE[["baleen whales"]], G_BASE[["minke whales"]],
            paste(BASE@species_params$interaction_resource[i_wh], collapse = "/")))

# --- effort arms (KC07:137-144) ----------------------------------------------
eff_obs <- readRDS("effort_array_1841_2010.rds")
stopifnot(identical(colnames(eff_obs), SP))
yrs_eff <- as.numeric(rownames(eff_obs)); kr_eff <- eff_obs[, KRILL]
peak_year <- yrs_eff[which.max(kr_eff)]; peak_val <- max(kr_eff)
eff_none <- eff_obs; eff_none[, KRILL] <- 0
eff_peak <- eff_obs; eff_peak[yrs_eff >= peak_year, KRILL] <- peak_val
# every other gear must be untouched (KC00:96-101)
stopifnot(identical(eff_none[, setdiff(SP, KRILL)], eff_obs[, setdiff(SP, KRILL)]),
          identical(eff_peak[, setdiff(SP, KRILL)], eff_obs[, setdiff(SP, KRILL)]),
          all(eff_peak[yrs_eff < peak_year, KRILL] ==
                eff_obs[yrs_eff < peak_year, KRILL]))
ARM_EFF <- list(unexploited = NULL, exploited = eff_obs, no_krill = eff_none,
                peak_krill = eff_peak)
cat("krill multiplier:", signif(MULT[[KRILL]], 5), "| peak", peak_year,
    "| cores", CORES, "| full diet:", FULL_DIET, "\n\n")

# sim_index varies fastest, so one chunk of `length(members)` rows is exactly
# one configuration -- readable progress and a clean resume boundary.
JOBS <- expand.grid(sim_index = members, gamma_arm = GA, repro = RP,
                    stringsAsFactors = FALSE)

# ------------------------------------------------------------------ worker ---
worker <- function(k) {
  suppressPackageStartupMessages({
    library(mizer); library(therMizer); library(dplyr)
  })
  source(file.path("R", "wmin_test", "thermizer_shim.R"))
  si <- JOBS$sim_index[k]; garm <- JOBS$gamma_arm[k]; rarm <- JOBS$repro[k]
  dw_ <- DR$draws[[as.character(si)]]
  fail <- function(r) list(sim_index = si, gamma_arm = garm, repro = rarm,
                           ok = FALSE, reason = r)

  check_stability <- function(s) {              # KC07:161-187
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
      if (!is.finite(mu) || mu < STAB$min_mean_biomass) {
        rs[q] <- 0; pv[q] <- 1; next
      }
      ft <- try(suppressWarnings(lm(b ~ t, data.frame(t = hy, b = as.numeric(y)))),
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
  apply_draws <- function(p) {                  # KC07:188-202
    gp <- gear_params(p)
    if (!identical(paste(gp$gear, gp$species), names(dw_$catchability)))
      stop("gear_params rows differ between the base and the stored member")
    gp$catchability <- as.numeric(dw_$catchability); gear_params(p) <- gp
    sp <- species_params(p)
    if (!identical(sp$species, names(dw_$gamma)))
      stop("species order differs between the base and the draw")
    sp$gamma <- as.numeric(dw_$gamma)
    species_params(p) <- sp
    sc <- as.numeric(dw_$abundance_scaling[sp$species])
    for (j in seq_along(sc)) p@initial_n[j, ] <- p@initial_n[j, ] * sc[j]
    stopifnot(identical(p@rates_funcs$Encounter, "therMizerEncounter"))
    p
  }
  # THE INTERVENTION. species_params(p) <- sp is mandatory: direct slot
  # assignment does NOT rebuild search_vol (44_rebuild_from_base.R:35-42) and
  # the change would silently do nothing.
  apply_gamma_arm <- function(p, arm) {
    if (arm == "drawn") return(p)
    sp  <- species_params(p)
    idx <- if (startsWith(arm, "wh_")) i_wh else seq_len(nrow(sp))
    gb  <- as.numeric(G_BASE)[idx]; gd <- sp$gamma[idx]
    sp$gamma[idx] <- if (endsWith(arm, "_base")) gb else sqrt(gb * gd)
    species_params(p) <- sp
    p
  }
  refit_q <- function(p) {                      # 46_selection_cuts.R:71-75
    gp <- gear_params(p)
    m <- MULT[match(gp$species, names(MULT))]; m[is.na(m)] <- 1
    gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
    gear_params(p) <- gp; p
  }
  steady_guarded <- function(p) {
    # mizer 3.1.0 signals non-convergence with message(), not warning().
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

  p <- try(apply_draws(BASE), silent = TRUE)
  if (inherits(p, "try-error")) return(fail("draw_error"))
  # the phase-55 intervention must survive apply_draws() (KC07:224-227)
  if (!isTRUE(all.equal(species_params(p)$interaction_resource,
                        BASE@species_params$interaction_resource)))
    return(fail("interaction_resource_lost_in_draws"))

  g_drawn <- species_params(p)$gamma
  p <- try(apply_gamma_arm(p, garm), silent = TRUE)
  if (inherits(p, "try-error")) return(fail("gamma_arm_error"))
  g_got <- species_params(p)$gamma
  # what the arm was supposed to produce, recomputed independently
  idx <- if (garm == "drawn") integer(0) else
    if (startsWith(garm, "wh_")) i_wh else seq_len(length(SP))
  g_want <- g_drawn
  if (length(idx))
    g_want[idx] <- if (endsWith(garm, "_base")) as.numeric(G_BASE)[idx] else
      sqrt(as.numeric(G_BASE)[idx] * g_drawn[idx])
  if (max(abs(g_got - g_want) / g_want) > 1e-12) return(fail("gamma_not_applied"))
  # non-target species must be untouched to EXACTLY zero
  off <- setdiff(seq_along(SP), idx)
  if (length(off) && !identical(g_got[off], g_drawn[off]))
    return(fail("gamma_leaked"))
  if (!identical(p@rates_funcs$Encounter, "therMizerEncounter"))
    return(fail("rates_funcs_lost_in_gamma_arm"))

  st <- steady_guarded(p)
  if (inherits(st$params, "try-error")) return(fail("steady_error"))
  if (!st$converged) return(fail("steady_no_converge"))
  ps <- st$params

  # The reproduction arm, applied AFTER steady() so the protocol above is
  # byte-identical to the control. setBevertonHolt preserves the initial state
  # exactly, so this needs no re-ladder; a NAMED vector affects only the
  # species named (mizer 3.1.0 ?setBevertonHolt).
  if (rarm == "rl25") {
    ps2 <- try(suppressWarnings(setBevertonHolt(
      ps, reproduction_level = setNames(rep(RL_TARGET, length(WHALES)), WHALES))),
      silent = TRUE)
    if (inherits(ps2, "try-error")) return(fail("repro_error"))
    ps <- ps2
    rl_chk <- try(getReproductionLevel(ps)[WHALES], silent = TRUE)
    if (inherits(rl_chk, "try-error")) return(fail("repro_check_error"))
    if (max(abs(as.numeric(rl_chk) - RL_TARGET)) > 0.01)
      return(fail("repro_not_set"))
  }

  RL <- try(getReproductionLevel(ps), silent = TRUE)
  if (inherits(RL, "try-error")) RL <- setNames(rep(NA_real_, length(SP)), SP)

  s0 <- try(project(ps, t_start = 1841, t_max = SPINUP_YEARS, effort = 0,
                    progress_bar = FALSE), silent = TRUE)
  if (inherits(s0, "try-error")) return(fail("spinup_error"))
  init <- s0@n[SPINUP_YEARS, , ]
  stab <- check_stability(s0)

  pq <- refit_q(ps)
  dw <- pq@dw; wdw <- pq@w * pq@dw
  BIO <- CONS <- FULL <- FL <- YL <- list()
  for (nm in names(ARM_EFF)) {
    e <- ARM_EFF[[nm]]
    # t_max omitted for the effort-array arms: mizer takes the horizon from the
    # array's own time dimension. Only the effort = 0 arm needs it.
    sim <- try(if (is.null(e))
      project(pq, initial_n = init, t_start = 1841, t_max = 169, effort = 0,
              progress_bar = FALSE)
      else project(pq, initial_n = init, t_start = 1841, effort = e,
                   progress_bar = FALSE), silent = TRUE)
    if (inherits(sim, "try-error")) return(fail(paste0("projection_error_", nm)))
    yr <- as.numeric(dimnames(sim@n)$time); ki <- which(yr <= END_YEAR)

    BIO[[nm]] <- do.call(rbind, lapply(ki, function(j) data.frame(
      sim_index = si, gamma_arm = garm, repro = rarm, arm = nm, Year = yr[j],
      Species = SP,
      Abundance = as.numeric(sim@n[j, , ] %*% dw),
      Biomass   = as.numeric(sim@n[j, , ] %*% wdw), stringsAsFactors = FALSE)))

    fl <- getFeedingLevel(sim)                 # time x species x size
    FL[[nm]] <- do.call(rbind, lapply(ki, function(j) {
      wt <- sim@n[j, , ] * rep(wdw, each = length(SP))
      tot <- rowSums(wt)
      data.frame(sim_index = si, gamma_arm = garm, repro = rarm, arm = nm,
                 Year = yr[j], Species = SP,
                 feeding_level = ifelse(tot > 0,
                                        rowSums(fl[j, , ] * wt) / tot, NA_real_),
                 stringsAsFactors = FALSE) }))

    # ther_diet() takes the year explicitly; mizer's getDiet() defaults to t = 0
    # and mis-indexes ocean_temp, giving rates 13-80x too large.
    ys <- yr[yr >= DIET_FROM & yr <= END_YEAR]
    dl <- lapply(ys, function(y) {
      ti <- which(yr == y)
      d <- ther_diet(sim@params, n = sim@n[ti, , ], n_pp = sim@n_pp[ti, ],
                     n_other = sim@params@initial_n_other, year = y)
      E <- sim@n[ti, , ] * rep(dw, each = length(SP))
      C <- apply(d, 3, function(mm) rowSums(mm * E))   # predator x prey
      dimnames(C) <- list(SP, dimnames(d)$prey)
      C
    })
    names(dl) <- ys
    CONS[[nm]] <- do.call(rbind, lapply(ys, function(y) {
      C <- dl[[as.character(y)]]
      data.frame(sim_index = si, gamma_arm = garm, repro = rarm, arm = nm,
                 Year = y, Species = SP,
                 total_consumed = rowSums(C),
                 krill_consumed = C[, KRILL],
                 resource_consumed = C[, "Resource"],
                 stringsAsFactors = FALSE) }))
    if (FULL_DIET)
      FULL[[nm]] <- do.call(rbind, lapply(ys, function(y) {
        C <- dl[[as.character(y)]]
        data.frame(sim_index = si, gamma_arm = garm, repro = rarm, arm = nm,
                   Year = y, predator = rep(rownames(C), ncol(C)),
                   prey = rep(colnames(C), each = nrow(C)),
                   consumed = as.numeric(C), stringsAsFactors = FALSE) }))

    yy <- getYield(sim)
    YL[[nm]] <- data.frame(sim_index = si, gamma_arm = garm, repro = rarm,
                           arm = nm, Year = as.numeric(rownames(yy))[ki],
                           krill_yield_g = as.numeric(yy[ki, KRILL]),
                           stringsAsFactors = FALSE)
  }

  spq <- species_params(ps)
  MECH <- data.frame(
    sim_index = si, gamma_arm = garm, repro = rarm, Species = SP,
    gamma = g_got, gamma_drawn = g_drawn, gamma_base = as.numeric(G_BASE),
    repro_level = as.numeric(RL[SP]),
    erepro = spq$erepro, R_max = spq$R_max, stringsAsFactors = FALSE)

  list(sim_index = si, gamma_arm = garm, repro = rarm, ok = TRUE, reason = "ok",
       stable = isTRUE(stab$stable), max_cv = stab$max_cv,
       q_krill = gear_params(pq)$catchability[match(KRILL, gear_params(pq)$species)],
       biomass = do.call(rbind, BIO), consumption = do.call(rbind, CONS),
       full_diet = if (FULL_DIET) do.call(rbind, FULL) else NULL,
       feeding = do.call(rbind, FL), yield = do.call(rbind, YL), mech = MECH)
}

# --------------------------------------------------------------------- run ---
CHUNK <- as.integer(Sys.getenv("KC10_CHUNK", as.character(length(members))))
chunks <- split(seq_len(nrow(JOBS)), ceiling(seq_len(nrow(JOBS)) / CHUNK))
chunk_file <- function(ci) file.path(CHUNK_DIR, sprintf("res_%03d.rds", ci))

if (mode == "status") {
  done <- vapply(seq_along(chunks), function(ci) file.exists(chunk_file(ci)),
                 logical(1))
  cat(sprintf("chunks %d/%d complete | jobs %d\n", sum(done), length(chunks),
              nrow(JOBS)))
  quit(save = "no")
}
if (mode == "run") {
  todo <- which(!vapply(seq_along(chunks),
                        function(ci) file.exists(chunk_file(ci)), logical(1)))
  cat("jobs:", nrow(JOBS), "(", N, "members x", length(GA), "gamma x",
      length(RP), "repro ) |", length(chunks), "chunks |", length(todo),
      "to run\n\n")
  if (length(todo)) {
    cl <- makeCluster(CORES)
    on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
    clusterExport(cl, c("BASE", "JOBS", "DR", "MULT", "ARM_EFF", "SP", "G_BASE",
                        "i_kr", "i_wh", "WHALES", "RL_TARGET", "STEADY_TOL",
                        "STEADY_TMAX", "SPINUP_YEARS", "QMAX", "KRILL",
                        "END_YEAR", "DIET_FROM", "FULL_DIET", "STAB"),
                  envir = environment())
    for (k in seq_along(todo)) {
      ci <- todo[k]; tc <- proc.time()
      r <- parLapply(cl, chunks[[ci]], worker)
      saveRDS(r, chunk_file(ci))
      cat(sprintf("[%s] chunk %d/%d done (%s/%s, %d jobs, %.1f min) | elapsed %.1f min\n",
                  format(Sys.time(), "%H:%M:%S"), ci, length(chunks),
                  JOBS$gamma_arm[chunks[[ci]][1]], JOBS$repro[chunks[[ci]][1]],
                  length(chunks[[ci]]), (proc.time() - tc)[["elapsed"]] / 60,
                  (proc.time() - t0)[["elapsed"]] / 60))
      flush.console()
    }
    stopCluster(cl)
  }
}

res <- unlist(lapply(seq_along(chunks), function(ci) {
  f <- chunk_file(ci); if (file.exists(f)) readRDS(f) else NULL
}), recursive = FALSE)
if (!length(res)) stop("no chunk results found in ", CHUNK_DIR)
ok <- vapply(res, function(r) isTRUE(r$ok), logical(1))
cat("\nassembled", length(res), "jobs |", sum(ok), "ok\n")
if (any(!ok)) {
  FL_ <- do.call(rbind, lapply(res[!ok], function(r) data.frame(
    sim_index = r$sim_index, gamma_arm = r$gamma_arm, repro = r$repro,
    reason = r$reason)))
  cat("\n=== failures by configuration ===\n")
  print(as.data.frame(FL_ %>% count(gamma_arm, repro, reason)), row.names = FALSE)
  write.csv(FL_, file.path(ANA, paste0(STEM, "_failures.csv")), row.names = FALSE)
}
res <- res[ok]

MEM <- do.call(rbind, lapply(res, function(r) data.frame(
  sim_index = r$sim_index, gamma_arm = r$gamma_arm, repro = r$repro,
  stable = r$stable, max_cv = r$max_cv, q_krill = r$q_krill)))
NCFG <- length(GA) * length(RP)
cat("\n=== members usable in EVERY configuration ===\n")
both <- MEM %>% group_by(sim_index) %>%
  summarise(n_ok = n(), all_stable = all(stable), .groups = "drop") %>%
  filter(n_ok == NCFG, all_stable) %>% pull(sim_index)
cat(length(both), "of", N, ":", paste(both, collapse = ", "), "\n")
cat("\nper-configuration counts (ok / stable):\n")
print(as.data.frame(MEM %>% group_by(gamma_arm, repro) %>%
  summarise(ok = n(), stable = sum(stable), .groups = "drop")), row.names = FALSE)

BIOMASS <- do.call(rbind, lapply(res, `[[`, "biomass"))

# =============================================================================
# G1 -- the control cell must reproduce KC07
# =============================================================================
# `drawn` x `asis` is the same base, members, draws and protocol as the phase-55
# arm of KC07. Re-projecting a stored member is bit-exact (the spin-up is
# unfished), so agreement should be ~0 and anything else means the new code path
# is not the faithful extension it claims to be. This one number validates
# membership, draws, the catchability refit, the spin-up and all four arms.
G1_REF <- Sys.getenv("KC10_G1_REF", file.path(OUT_LARGE, "KC07_whres05_n10.rds"))
G1_TOL <- as.numeric(Sys.getenv("KC10_G1_TOL", "1e-10"))
if ("drawn" %in% GA && "asis" %in% RP && file.exists(G1_REF)) {
  K7 <- readRDS(G1_REF)
  ref <- K7$biomass %>% filter(base == "phase55") %>%
    select(sim_index, arm, Year, Species, Abundance_r = Abundance,
           Biomass_r = Biomass)
  new <- BIOMASS %>% filter(gamma_arm == "drawn", repro == "asis") %>%
    select(sim_index, arm, Year, Species, Abundance, Biomass)
  j <- inner_join(new, ref, by = c("sim_index", "arm", "Year", "Species"))
  cat("\n=== G1. control cell vs KC07 phase-55 ===\n")
  if (!nrow(j)) {
    cat("  no overlapping rows -- gate SKIPPED\n")
  } else {
    rel <- function(a, b) max(abs(a - b) / pmax(abs(b), .Machine$double.xmin))
    rB <- rel(j$Biomass, j$Biomass_r); rA <- rel(j$Abundance, j$Abundance_r)
    cat(sprintf("  %d rows | max rel diff: biomass %.3g, abundance %.3g\n",
                nrow(j), rB, rA))
    if (max(rB, rA) > G1_TOL)
      stop("G1 FAILED: the control cell does not reproduce KC07 (tol ", G1_TOL,
           "). The new code path is not a faithful extension -- do not read the",
           " results.", call. = FALSE)
    cat("  G1 PASSED\n")
  }
} else {
  cat("\n=== G1 skipped (control cell not in this grid, or reference missing) ===\n")
}

# =============================================================================
# Q1 preview -- did the manipulation bite?
# =============================================================================
FEED <- do.call(rbind, lapply(res, `[[`, "feeding"))
MECH <- do.call(rbind, lapply(res, `[[`, "mech"))
cat("\n=== whale feeding level, unexploited arm, 2001-2010 median ===\n")
print(as.data.frame(FEED %>%
  filter(Species %in% WHALES, arm == "unexploited", Year %in% 2001:2010,
         sim_index %in% both) %>%
  group_by(gamma_arm, repro, Species) %>%
  summarise(f = round(median(feeding_level, na.rm = TRUE), 4), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Species, values_from = f)), row.names = FALSE)
cat("  phase-55 base sits at 0.7904 (baleen) / 0.4906 (minke).\n")
cat("\n=== whale reproduction level, 2001-2010 (median over members) ===\n")
print(as.data.frame(MECH %>% filter(Species %in% WHALES, sim_index %in% both) %>%
  group_by(gamma_arm, repro, Species) %>%
  summarise(rl = round(median(repro_level), 4), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Species, values_from = rl)), row.names = FALSE)

meta <- list(base = BASE_FILE, gamma_arms = GA, repro_arms = RP,
             rl_target = RL_TARGET, n_requested = N, members = members,
             members_all_cfg = both, spinup_years = SPINUP_YEARS,
             peak_year = peak_year, diet_from = DIET_FROM,
             full_diet = FULL_DIET, multipliers = MULT,
             gamma_base = G_BASE, built = format(Sys.time()))
saveRDS(list(biomass = BIOMASS,
             consumption = do.call(rbind, lapply(res, `[[`, "consumption")),
             full_diet = if (FULL_DIET)
               do.call(rbind, lapply(res, `[[`, "full_diet")) else NULL,
             feeding = FEED,
             yield = do.call(rbind, lapply(res, `[[`, "yield")),
             mech = MECH, members = MEM, meta = meta), OUT_RDS)
write.csv(MEM, file.path(ANA, paste0(STEM, "_members.csv")), row.names = FALSE)
write.csv(MECH, file.path(ANA, paste0(STEM, "_mechanism.csv")), row.names = FALSE)
cat("\nwrote", OUT_RDS, sprintf("(%.0f MB)\n", file.info(OUT_RDS)$size / 1024^2))
cat("elapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")
cat("KC10 complete -- run KC11_compare_gamma_arms.R for the assessment.\n")