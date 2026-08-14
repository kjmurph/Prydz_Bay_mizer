# =============================================================================
# KC14 -- the four krill counterfactuals on the cut-A ensemble, under the
# phase-56 reference model (whale interaction_resource = 0)
#
# WHY. Whale abundance and body size are unmoved by krill fishing even with
# satiation and the reproduction clamp both relieved (KC10/KC11), and getRDD()
# says recruitment numbers are not obviously unrealistic. The remaining
# explanation is PREY SWITCHING onto food the fishery cannot touch. Phase 56
# removes the undepletable background resource from whale diets entirely
# (interaction_resource 1 -> 0; it calibrates, max biomass deviation 0.01207,
# slightly better than phase 54's 0.01229).
#
# THE POINT THIS RUN HAS TO SETTLE. Removing the resource does not remove the
# switching pathway -- it REDIRECTS it. At the phase-56 calibrated state baleen
# whales take 72.1% krill and 27.0% "other LTL" (the four non-krill zooplankton
# groups), where at phase 54 they took 40.8% krill and 43.5% resource. Unlike
# the resource, those four groups are dynamic and depletable, so the switching
# now has a cost that the model can actually represent. This run therefore
# stores consumption of EACH of the five LTL groups separately, not just krill.
#
# WHAT IS SAVED, beyond KC07:
#   consumption   krill, mesozooplankton, other krill, other macrozooplankton,
#                 salps, Resource, total -- per predator-year, so both the krill
#                 figure and the Other-LTL figure come out of one run
#   RECRUITMENT   rdd and rdi per species-year via ther_rdd(). getRDD() defaults
#                 to t = 0, which under therMizer indexes ocean_temp at the
#                 wrong year -- the same defect as getDiet(). The shim threads
#                 the by-year temperature effect through the whole chain, and
#                 shares the encounter and feeding level with the diet
#                 calculation so the extra cost is small.
#   mech          reproduction level, erepro, R_max, gamma per member
#
# PROTOCOL is KC07 verbatim: cut-A rank order, same draws, same steady() guard,
# same 118-year unfished spin-up, same four arms, and the same catchability
# trap -- states carry DRAWN catchability and refit_q() must be applied before
# projecting, because the krill multiplier is 0.1904.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC14_ensemble_whres000.R" [run|collect|status]
# ENV    KC14_N (167), KC14_CORES (10), KC14_DIET_FROM (1841), KC14_BASE
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OUT_LARGE <- "Output_large_files/wmin_test"
KC_ROOT   <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
ANA <- file.path(KC_ROOT, "analysis")
BASE_FILE <- Sys.getenv("KC14_BASE",
                        "params_ref_sw2000_balror_mnkfish05_whres000.rds")
N         <- as.integer(Sys.getenv("KC14_N", "167"))
DIET_FROM <- as.integer(Sys.getenv("KC14_DIET_FROM", "1841"))
CORES     <- min(as.integer(Sys.getenv("KC14_CORES", "10")), 15, max(1, N))
STEM      <- sprintf("KC14_whres000_n%d", N)
CHUNK_DIR <- file.path(OUT_LARGE, paste0(STEM, "_chunks"))
OUT_RDS   <- file.path(OUT_LARGE, paste0(STEM, ".rds"))
for (d in c(ANA, CHUNK_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"
stopifnot(mode %in% c("run", "collect", "status"))

STEADY_TOL <- 0.0025; STEADY_TMAX <- 1500; SPINUP_YEARS <- 118
QMAX <- 1; KRILL <- "antarctic krill"; END_YEAR <- 2010
WHALES <- c("baleen whales", "minke whales")
# "Other LTL" -- the four non-krill zooplankton groups, stored separately so the
# figure can sum them and a reader can still decompose.
OTHER_LTL <- c("mesozooplankton", "other krill", "other macrozooplankton",
               "salps")
STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

t0 <- proc.time()
cat("=== KC14: krill counterfactuals under whale interaction_resource = 0 ===\n")
cat("base:", BASE_FILE, "| members:", N, "| cores:", CORES, "\n")
if (!file.exists(BASE_FILE))
  stop("missing base params: ", BASE_FILE,
       "\n  run R/wmin_test/56_whale_resource_sweep.R first", call. = FALSE)

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
MULT <- readRDS(file.path(OUT_LARGE, "45_catchability_multipliers.rds"))$M

BASE <- suppressWarnings(validParams(readRDS(BASE_FILE)))
stopifnot(identical(BASE@rates_funcs$Encounter, "therMizerEncounter"),
          identical(BASE@resource_dynamics, "plankton_forcing"),
          identical(BASE@second_order_w$flux, "upwind"),
          isFALSE(BASE@second_order_w$bin_average))
SP <- BASE@species_params$species
i_wh <- match(WHALES, SP); i_kr <- which(SP == KRILL)
stopifnot(!anyNA(i_wh), length(i_kr) == 1, !anyNA(match(OTHER_LTL, SP)))
# the phase-56 intervention, asserted rather than assumed
if (!all(BASE@species_params$interaction_resource[i_wh] == 0))
  stop("base does not have whale interaction_resource = 0", call. = FALSE)
cat("whale interaction_resource:",
    paste(BASE@species_params$interaction_resource[i_wh], collapse = "/"), "\n")

# --- effort arms (KC07:137-144) ----------------------------------------------
eff_obs <- readRDS("effort_array_1841_2010.rds")
stopifnot(identical(colnames(eff_obs), SP))
yrs_eff <- as.numeric(rownames(eff_obs)); kr_eff <- eff_obs[, KRILL]
peak_year <- yrs_eff[which.max(kr_eff)]; peak_val <- max(kr_eff)
eff_none <- eff_obs; eff_none[, KRILL] <- 0
eff_peak <- eff_obs; eff_peak[yrs_eff >= peak_year, KRILL] <- peak_val
stopifnot(identical(eff_none[, setdiff(SP, KRILL)], eff_obs[, setdiff(SP, KRILL)]),
          identical(eff_peak[, setdiff(SP, KRILL)], eff_obs[, setdiff(SP, KRILL)]))
ARM_EFF <- list(unexploited = NULL, exploited = eff_obs, no_krill = eff_none,
                peak_krill = eff_peak)
cat("krill multiplier:", signif(MULT[[KRILL]], 5), "| peak", peak_year, "\n\n")

# ------------------------------------------------------------------ worker ---
worker <- function(si) {
  suppressPackageStartupMessages({
    library(mizer); library(therMizer); library(dplyr)
  })
  source(file.path("R", "wmin_test", "thermizer_shim.R"))
  dw_ <- DR$draws[[as.character(si)]]
  fail <- function(r) list(sim_index = si, ok = FALSE, reason = r)

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
  refit_q <- function(p) {
    gp <- gear_params(p)
    m <- MULT[match(gp$species, names(MULT))]; m[is.na(m)] <- 1
    gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
    gear_params(p) <- gp; p
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

  p <- try(apply_draws(BASE), silent = TRUE)
  if (inherits(p, "try-error")) return(fail("draw_error"))
  # the phase-56 intervention must survive apply_draws()
  if (!all(species_params(p)$interaction_resource[i_wh] == 0))
    return(fail("interaction_resource_lost_in_draws"))
  st <- steady_guarded(p)
  if (inherits(st$params, "try-error")) return(fail("steady_error"))
  if (!st$converged) return(fail("steady_no_converge"))
  ps <- st$params

  s0 <- try(project(ps, t_start = 1841, t_max = SPINUP_YEARS, effort = 0,
                    progress_bar = FALSE), silent = TRUE)
  if (inherits(s0, "try-error")) return(fail("spinup_error"))
  init <- s0@n[SPINUP_YEARS, , ]
  stab <- check_stability(s0)

  pq <- refit_q(ps)
  dw <- pq@dw; wdw <- pq@w * pq@dw
  BIO <- CONS <- FL <- YL <- REC <- list()
  for (nm in names(ARM_EFF)) {
    e <- ARM_EFF[[nm]]
    sim <- try(if (is.null(e))
      project(pq, initial_n = init, t_start = 1841, t_max = 169, effort = 0,
              progress_bar = FALSE)
      else project(pq, initial_n = init, t_start = 1841, effort = e,
                   progress_bar = FALSE), silent = TRUE)
    if (inherits(sim, "try-error")) return(fail(paste0("projection_error_", nm)))
    yr <- as.numeric(dimnames(sim@n)$time); ki <- which(yr <= END_YEAR)

    BIO[[nm]] <- do.call(rbind, lapply(ki, function(j) data.frame(
      sim_index = si, arm = nm, Year = yr[j], Species = SP,
      Abundance = as.numeric(sim@n[j, , ] %*% dw),
      Biomass   = as.numeric(sim@n[j, , ] %*% wdw), stringsAsFactors = FALSE)))

    ys <- yr[yr >= DIET_FROM & yr <= END_YEAR]
    # Encounter and feeding level are computed ONCE per year and shared between
    # the diet and the recruitment, which is what keeps the extra RDD cost small.
    out <- lapply(ys, function(y) {
      ti <- which(yr == y)
      nn <- sim@n[ti, , ]; npp <- sim@n_pp[ti, ]
      no <- sim@params@initial_n_other
      te <- ther_temp_effect(sim@params, y)
      enc <- sweep(mizer::mizerEncounter(sim@params, n = nn, n_pp = npp,
                                         n_other = no, t = 0), 1, te, "*")
      flm <- mizer::mizerFeedingLevel(sim@params, n = nn, n_pp = npp,
                                      n_other = no, encounter = enc, t = 0)
      d <- ther_diet(sim@params, n = nn, n_pp = npp, n_other = no, year = y,
                     temp_eff = te, feeding_level = flm)
      E <- nn * rep(dw, each = length(SP))
      C <- apply(d, 3, function(mm) rowSums(mm * E))   # predator x prey
      dimnames(C) <- list(SP, dimnames(d)$prey)
      r <- ther_rdd(sim@params, n = nn, n_pp = npp, n_other = no, year = y,
                    temp_eff = te, encounter = enc, feeding_level = flm)
      wt <- nn * rep(wdw, each = length(SP)); tot <- rowSums(wt)
      list(C = C, rdd = r$rdd, rdi = r$rdi,
           fl = ifelse(tot > 0, rowSums(flm * wt) / tot, NA_real_))
    })
    names(out) <- ys

    CONS[[nm]] <- do.call(rbind, lapply(ys, function(y) {
      C <- out[[as.character(y)]]$C
      data.frame(sim_index = si, arm = nm, Year = y, Species = SP,
                 total_consumed = rowSums(C),
                 krill_consumed = C[, KRILL],
                 other_ltl_consumed = rowSums(C[, OTHER_LTL, drop = FALSE]),
                 mesozoo_consumed = C[, "mesozooplankton"],
                 other_krill_consumed = C[, "other krill"],
                 other_macrozoo_consumed = C[, "other macrozooplankton"],
                 salps_consumed = C[, "salps"],
                 resource_consumed = C[, "Resource"],
                 stringsAsFactors = FALSE) }))
    FL[[nm]] <- do.call(rbind, lapply(ys, function(y) data.frame(
      sim_index = si, arm = nm, Year = y, Species = SP,
      feeding_level = out[[as.character(y)]]$fl, stringsAsFactors = FALSE)))
    REC[[nm]] <- do.call(rbind, lapply(ys, function(y) data.frame(
      sim_index = si, arm = nm, Year = y, Species = SP,
      rdd = out[[as.character(y)]]$rdd, rdi = out[[as.character(y)]]$rdi,
      stringsAsFactors = FALSE)))

    yy <- getYield(sim)
    YL[[nm]] <- data.frame(sim_index = si, arm = nm,
                           Year = as.numeric(rownames(yy))[ki],
                           krill_yield_g = as.numeric(yy[ki, KRILL]),
                           stringsAsFactors = FALSE)
  }

  spq <- species_params(ps)
  rl <- try(getReproductionLevel(ps), silent = TRUE)
  MECH <- data.frame(
    sim_index = si, Species = SP, gamma = spq$gamma, erepro = spq$erepro,
    R_max = spq$R_max,
    repro_level = if (inherits(rl, "try-error")) NA_real_ else as.numeric(rl),
    stringsAsFactors = FALSE)

  list(sim_index = si, ok = TRUE, reason = "ok",
       stable = isTRUE(stab$stable), max_cv = stab$max_cv,
       q_krill = gear_params(pq)$catchability[match(KRILL, gear_params(pq)$species)],
       biomass = do.call(rbind, BIO), consumption = do.call(rbind, CONS),
       feeding = do.call(rbind, FL), recruit = do.call(rbind, REC),
       yield = do.call(rbind, YL), mech = MECH)
}

# --------------------------------------------------------------------- run ---
CHUNK <- as.integer(Sys.getenv("KC14_CHUNK", as.character(CORES)))
chunks <- split(members, ceiling(seq_along(members) / CHUNK))
chunk_file <- function(ci) file.path(CHUNK_DIR, sprintf("res_%03d.rds", ci))

if (mode == "status") {
  done <- vapply(seq_along(chunks), function(ci) file.exists(chunk_file(ci)),
                 logical(1))
  cat(sprintf("chunks %d/%d complete | members %d\n", sum(done), length(chunks),
              length(members)))
  quit(save = "no")
}
if (mode == "run") {
  todo <- which(!vapply(seq_along(chunks),
                        function(ci) file.exists(chunk_file(ci)), logical(1)))
  cat("members:", length(members), "|", length(chunks), "chunks |",
      length(todo), "to run\n\n")
  if (length(todo)) {
    cl <- makeCluster(CORES)
    on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
    clusterExport(cl, c("BASE", "DR", "MULT", "ARM_EFF", "SP", "i_wh", "i_kr",
                        "OTHER_LTL", "STEADY_TOL", "STEADY_TMAX",
                        "SPINUP_YEARS", "QMAX", "KRILL", "END_YEAR",
                        "DIET_FROM", "STAB"), envir = environment())
    for (k in seq_along(todo)) {
      ci <- todo[k]; tc <- proc.time()
      r <- parLapply(cl, chunks[[ci]], worker)
      saveRDS(r, chunk_file(ci))
      cat(sprintf("[%s] chunk %d/%d done (%d members, %.1f min) | elapsed %.1f min\n",
                  format(Sys.time(), "%H:%M:%S"), ci, length(chunks),
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
cat("\nassembled", length(res), "members |", sum(ok), "ok\n")
if (any(!ok))
  cat("  failures:", paste(vapply(res[!ok], function(r)
    sprintf("%d(%s)", r$sim_index, r$reason), ""), collapse = ", "), "\n")
res <- res[ok]

MEM <- do.call(rbind, lapply(res, function(r) data.frame(
  sim_index = r$sim_index, stable = r$stable, max_cv = r$max_cv,
  q_krill = r$q_krill)))
cat("stable:", sum(MEM$stable), "of", nrow(MEM), "\n")

meta <- list(base = BASE_FILE, n_requested = N, members = members,
             members_stable = MEM$sim_index[MEM$stable],
             spinup_years = SPINUP_YEARS, peak_year = peak_year,
             diet_from = DIET_FROM, multipliers = MULT,
             other_ltl = OTHER_LTL, whales = WHALES,
             built = format(Sys.time()))
saveRDS(list(biomass = do.call(rbind, lapply(res, `[[`, "biomass")),
             consumption = do.call(rbind, lapply(res, `[[`, "consumption")),
             feeding = do.call(rbind, lapply(res, `[[`, "feeding")),
             recruit = do.call(rbind, lapply(res, `[[`, "recruit")),
             yield = do.call(rbind, lapply(res, `[[`, "yield")),
             mech = do.call(rbind, lapply(res, `[[`, "mech")),
             members = MEM, meta = meta), OUT_RDS)
write.csv(MEM, file.path(ANA, paste0(STEM, "_members.csv")), row.names = FALSE)
cat("wrote", OUT_RDS, sprintf("(%.0f MB)\n", file.info(OUT_RDS)$size / 1024^2))
cat("elapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")
cat("KC14 complete.\n")