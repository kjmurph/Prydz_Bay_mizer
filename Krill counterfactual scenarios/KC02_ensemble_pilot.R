# =============================================================================
# KC02 -- krill counterfactual scenarios on the TOP N cut-A ensemble members
#
# WHY THIS EXISTS. KC00's single-params pilot found essentially no krill-fishing
# effect, but it cannot be trusted as an ensemble answer: the base object
# carries baleen catchability 0.0223 against a cut-A member median of 0.0899,
# so it barely depletes whales (biomass ratio 0.83 against the published
# ensemble's ~0.42) and therefore has little recovery left to impair. This runs
# the identical four arms on real ensemble members, N = 10 by default, before
# committing to all 167. It is the 52 -> 53 pattern.
#
# MEMBER REBUILD, transcribed from R/wmin_test/53_kernel_rebuild_cutA.R:
#   cut A in POST-REFIT rank order (46_selection_cuts.rds, cross-checked against
#   45_refit_results.rds), draws from 43_member_draws.rds applied to the phase-54
#   base, then steady(tol 0.0025, t_max 1500, preserve erepro), then a 118-year
#   unfished spin-up.
#
# THE CATCHABILITY TRAP. States carry the DRAWN catchability; the published
# figure pipeline (F00p:115-118) applies 45_catchability_multipliers.rds itself
# before projecting, and the cut-A ranking is defined post-refit. So refit_q()
# is applied here too. This is not cosmetic for these scenarios: the krill
# multiplier is 0.1904, so skipping the refit would make the krill fishery over
# five times stronger than the calibrated model. The whale multipliers are all
# exactly 1.0 -- whale q is deliberately held at its drawn values because the
# observed baleen catch is 23.8x the calibrated stock and no refit can fix it.
#
# FOUR ARMS per member, all from the same spin-up state: unexploited (effort 0),
# exploited (observed), no_krill (krill column zeroed), peak_krill (krill held
# at its 1979 peak from 1979 to 2010). See KC00 for the full rationale.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC02_ensemble_pilot.R"
# ENV    KC_N (default 10), KC_CORES (default 10), KC_PARAMS, KC_OUT,
#        KC_DIET_FROM (default 1900)
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OUT_LARGE <- "Output_large_files/wmin_test"
KC_PARAMS <- Sys.getenv("KC_PARAMS", "params_ref_sw2000_balror_mnkfish05.rds")
KC_ROOT   <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
N         <- as.integer(Sys.getenv("KC_N", "10"))
DIET_FROM <- as.integer(Sys.getenv("KC_DIET_FROM", "1900"))
# 16 cores on this machine; never commit all of them -- saturating them risks a
# hard shutdown that kills the in-flight run too.
CORES     <- min(as.integer(Sys.getenv("KC_CORES", "10")), 15, N)
CHUNK     <- as.integer(Sys.getenv("KC_CHUNK", as.character(CORES)))
mode      <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(mode)) mode <- "run"
stopifnot(mode %in% c("run", "collect", "status"))

# Outputs are tagged by member count and diet window so a full run cannot
# clobber the pilot, and so the two remain separately readable.
STEM      <- sprintf("KC02_n%d_from%d", N, DIET_FROM)
ANA <- file.path(KC_ROOT, "analysis")
STATE_DIR <- file.path(OUT_LARGE, paste0(STEM, "_states"))
CHUNK_DIR <- file.path(OUT_LARGE, paste0(STEM, "_chunks"))
for (d in c(ANA, STATE_DIR, CHUNK_DIR))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

STEADY_TOL <- 0.0025; STEADY_TMAX <- 1500; SPINUP_YEARS <- 118
QMAX <- 1; KRILL <- "antarctic krill"; END_YEAR <- 2010
STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

t0 <- proc.time()
cat("=== KC02: krill counterfactuals on the top", N, "cut-A members ===\n")

# --- members, in post-refit rank order (53:76-90) -----------------------------
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
cat("members (rank order):", paste(members, collapse = ", "), "\n")

# --- base and effort arrays ---------------------------------------------------
BASE <- suppressWarnings(validParams(readRDS(KC_PARAMS)))
stopifnot(identical(BASE@rates_funcs$Encounter, "therMizerEncounter"),
          identical(BASE@resource_dynamics, "plankton_forcing"),
          identical(BASE@second_order_w$flux, "upwind"))
SP <- BASE@species_params$species
i_kr <- which(SP == KRILL)
eff_obs <- readRDS("effort_array_1841_2010.rds")
stopifnot(identical(colnames(eff_obs), SP))
yrs_eff <- as.numeric(rownames(eff_obs)); kr_eff <- eff_obs[, KRILL]
peak_year <- yrs_eff[which.max(kr_eff)]; peak_val <- max(kr_eff)
active <- range(yrs_eff[kr_eff > 0])
eff_none <- eff_obs; eff_none[, KRILL] <- 0
eff_peak <- eff_obs; eff_peak[yrs_eff >= peak_year, KRILL] <- peak_val
cat("base:", KC_PARAMS, "| krill fishery", active[1], "-", active[2],
    "| peak", peak_year, "| cores", CORES, "\n")
cat("krill catchability multiplier:", MULT[[KRILL]],
    "| whale multipliers:", paste(round(MULT[c("baleen whales", "minke whales",
      "sperm whales")], 4), collapse = "/"), "\n\n")

ARM_EFF <- list(unexploited = NULL, exploited = eff_obs, no_krill = eff_none,
                peak_krill = eff_peak)

# ------------------------------------------------------------------ worker ----
worker <- function(si) {
  suppressPackageStartupMessages({
    library(mizer); library(therMizer); library(dplyr)
  })
  source(file.path("R", "wmin_test", "thermizer_shim.R"))
  dw_ <- DR$draws[[as.character(si)]]

  check_stability <- function(s) {          # 53:139-163
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
  apply_draws <- function(p) {              # 53:192-206
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
    for (k in seq_along(sc)) p@initial_n[k, ] <- p@initial_n[k, ] * sc[k]
    stopifnot(identical(p@rates_funcs$Encounter, "therMizerEncounter"))
    p
  }
  refit_q <- function(p) {                  # 46_selection_cuts.R:71-75, F00p:115
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
  if (inherits(p, "try-error"))
    return(list(sim_index = si, ok = FALSE, reason = "draw_error"))
  st <- steady_guarded(p)
  if (inherits(st$params, "try-error"))
    return(list(sim_index = si, ok = FALSE, reason = "steady_error"))
  if (!st$converged)
    return(list(sim_index = si, ok = FALSE, reason = "steady_no_converge"))
  ps <- st$params

  s0 <- try(project(ps, t_start = 1841, t_max = SPINUP_YEARS, effort = 0,
                    progress_bar = FALSE), silent = TRUE)
  if (inherits(s0, "try-error"))
    return(list(sim_index = si, ok = FALSE, reason = "spinup_error"))
  init <- s0@n[SPINUP_YEARS, , ]
  stab <- check_stability(s0)

  pq <- refit_q(ps)                          # projections use the REFIT q
  saveRDS(list(sim_index = si, params = ps, params_refit = pq,
               initial_n = init),
          file.path(STATE_DIR, sprintf("kc02_state_%05d.rds", si)))

  wdw <- pq@w * pq@dw
  BIO <- KR <- YL <- list()
  for (nm in names(ARM_EFF)) {
    e <- ARM_EFF[[nm]]
    sim <- try(if (is.null(e))
      project(pq, initial_n = init, t_start = 1841, t_max = 169, effort = 0,
              progress_bar = FALSE)
      else project(pq, initial_n = init, t_start = 1841, effort = e,
                   progress_bar = FALSE), silent = TRUE)
    if (inherits(sim, "try-error"))
      return(list(sim_index = si, ok = FALSE,
                  reason = paste0("projection_error_", nm)))
    yr <- as.numeric(dimnames(sim@n)$time); ki <- which(yr <= END_YEAR)
    BIO[[nm]] <- do.call(rbind, lapply(ki, function(j) data.frame(
      sim_index = si, arm = nm, Year = yr[j], Species = SP,
      biomass_g = as.numeric(sim@n[j, , ] %*% wdw), stringsAsFactors = FALSE)))
    ys <- yr[yr >= DIET_FROM & yr <= END_YEAR]
    KR[[nm]] <- do.call(rbind, lapply(ys, function(y) {
      ti <- which(yr == y)
      d <- ther_diet(sim@params, n = sim@n[ti, , ], n_pp = sim@n_pp[ti, ],
                     n_other = sim@params@initial_n_other, year = y)
      data.frame(sim_index = si, arm = nm, Year = y, Species = SP,
                 krill_consumed = rowSums(d[, , i_kr, drop = TRUE] *
                   sim@n[ti, , ] * rep(pq@dw, each = length(SP))),
                 stringsAsFactors = FALSE)
    }))
    yy <- getYield(sim)
    YL[[nm]] <- data.frame(sim_index = si, arm = nm,
                           Year = as.numeric(rownames(yy))[ki],
                           krill_yield_g = as.numeric(yy[ki, KRILL]),
                           stringsAsFactors = FALSE)
  }
  bi <- as.numeric(init %*% wdw)
  list(sim_index = si, ok = TRUE, reason = "ok",
       stable = isTRUE(stab$stable), max_cv = stab$max_cv,
       q_baleen = gear_params(pq)$catchability[
         match("baleen whales", gear_params(pq)$species)],
       q_krill = gear_params(pq)$catchability[
         match(KRILL, gear_params(pq)$species)],
       bw_1841 = bi[SP == "baleen whales"], kr_1841 = bi[i_kr],
       biomass = do.call(rbind, BIO), krill = do.call(rbind, KR),
       yield = do.call(rbind, YL))
}

# --------------------------------------------------------------------- run ---
# Chunked and RESUMABLE. A multi-hour parLapply that returns only at the end has
# two failure modes this avoids: no visibility into progress, and total loss of
# work if anything interrupts it. Each chunk writes its own result file; a
# re-run skips chunks already on disk. `status` mode reports progress without
# touching anything, so it is safe to call while a run is in flight.
chunks <- split(members, ceiling(seq_along(members) / CHUNK))
chunk_file <- function(ci) file.path(CHUNK_DIR, sprintf("res_%03d.rds", ci))

if (mode == "status") {
  done <- vapply(seq_along(chunks), function(ci) file.exists(chunk_file(ci)),
                 logical(1))
  n_done <- sum(vapply(seq_along(chunks)[done],
                       function(ci) length(readRDS(chunk_file(ci))), integer(1)))
  cat(sprintf("chunks %d/%d complete | members %d/%d | states on disk %d\n",
              sum(done), length(chunks), n_done, length(members),
              length(list.files(STATE_DIR, "\\.rds$"))))
  if (sum(done) < length(chunks))
    cat("  next incomplete chunk:", which(!done)[1], "of", length(chunks), "\n")
  quit(save = "no")
}

if (mode == "run") {
  todo <- which(!vapply(seq_along(chunks),
                        function(ci) file.exists(chunk_file(ci)), logical(1)))
  cat("chunks:", length(chunks), "of", CHUNK, "members |",
      length(todo), "to run |", CORES, "cores\n")
  if (length(todo)) {
    cl <- makeCluster(CORES)
    on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
    clusterExport(cl, c("BASE", "DR", "MULT", "ARM_EFF", "SP", "i_kr",
                        "STATE_DIR", "STEADY_TOL", "STEADY_TMAX",
                        "SPINUP_YEARS", "QMAX", "KRILL", "END_YEAR",
                        "DIET_FROM", "STAB"), envir = environment())
    for (k in seq_along(todo)) {
      ci <- todo[k]; tc <- proc.time()
      r <- parLapply(cl, chunks[[ci]], worker)
      saveRDS(r, chunk_file(ci))
      el <- (proc.time() - tc)[["elapsed"]] / 60
      tot <- (proc.time() - t0)[["elapsed"]] / 60
      cat(sprintf("[%s] chunk %d/%d done (%d members, %.1f min) | elapsed %.1f min | ETA %.1f min\n",
                  format(Sys.time(), "%H:%M:%S"), ci, length(chunks),
                  length(chunks[[ci]]), el, tot,
                  tot / k * (length(todo) - k)))
      flush.console()
    }
    stopCluster(cl)
  }
}

res <- unlist(lapply(seq_along(chunks), function(ci) {
  f <- chunk_file(ci); if (file.exists(f)) readRDS(f) else NULL
}), recursive = FALSE)
if (!length(res)) stop("no chunk results found in ", CHUNK_DIR)
cat("\nassembled", length(res), "member results from",
    length(list.files(CHUNK_DIR, "^res_")), "chunks\n")

ok <- vapply(res, function(r) isTRUE(r$ok), logical(1))
cat("\nconverged:", sum(ok), "of", length(res), "\n")
if (any(!ok))
  cat("  failures:", paste(vapply(res[!ok], function(r)
    sprintf("%d(%s)", r$sim_index, r$reason), ""), collapse = ", "), "\n")
if (!any(ok)) stop("no members completed")
res <- res[ok]

MEM <- do.call(rbind, lapply(res, function(r) data.frame(
  sim_index = r$sim_index, stable = r$stable, max_cv = r$max_cv,
  q_baleen = r$q_baleen, q_krill = r$q_krill,
  bw_1841 = r$bw_1841, kr_1841 = r$kr_1841)))
cat("\n=== members ===\n")
print(as.data.frame(MEM %>% mutate(across(where(is.numeric), ~signif(.x, 5)))),
      row.names = FALSE)
cat("stable:", sum(MEM$stable), "of", nrow(MEM), "\n")
cat(sprintf("baleen catchability: median %.4f, range %.4f-%.4f (base %.4f)\n",
            median(MEM$q_baleen), min(MEM$q_baleen), max(MEM$q_baleen),
            gear_params(BASE)$catchability[
              match("baleen whales", gear_params(BASE)$species)]))

meta <- list(params = KC_PARAMS, n_requested = N, n_ok = nrow(MEM),
             members = MEM$sim_index, spinup_years = SPINUP_YEARS,
             peak_year = peak_year, peak_val = peak_val,
             krill_active = active, diet_from = DIET_FROM,
             multipliers = MULT, qmax = QMAX, built = format(Sys.time()))
OUT_RDS <- file.path(ANA, paste0(STEM, "_scenarios.rds"))
saveRDS(list(biomass = do.call(rbind, lapply(res, `[[`, "biomass")),
             krill   = do.call(rbind, lapply(res, `[[`, "krill")),
             yield   = do.call(rbind, lapply(res, `[[`, "yield")),
             members = MEM, meta = meta), OUT_RDS)
write.csv(MEM, file.path(ANA, paste0(STEM, "_members.csv")), row.names = FALSE)
cat("\nwrote", basename(OUT_RDS), "in", ANA, "\n")
cat("elapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")