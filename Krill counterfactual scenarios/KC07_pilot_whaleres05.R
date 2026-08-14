# =============================================================================
# KC07 -- the four krill counterfactuals on the top 10 cut-A members, run under
# BOTH reference models, saving everything KC02 did not
#
# THE QUESTION. KC04/KC06 found whale abundance and biomass unmoved by krill
# fishing. The proposed explanation is that whales switch onto the background
# resource, which `plankton_forcing` PRESCRIBES from n_pp_array and never
# depletes -- an infinite buffer supplying 43.5% of baleen and 49.4% of minke
# intake at the phase-54 calibrated state. Phase 55 halves whale access to it.
# This asks whether that changes the counterfactual result.
#
# BOTH BASES, SAME MEMBERS, ONE RUN. Each of the 10 members is rebuilt and
# projected twice -- once under the phase-54 reference and once under phase 55 --
# so every comparison is paired on the member and nothing has to be
# cross-referenced against KC02/KC05, which stored different quantities. Cost is
# 2x a 10-member pilot, which is minutes.
#
# WHAT IS SAVED, AND WHAT KC02 MISSED
#   biomass          as KC02
#   ABUNDANCE        numbers -- KC02 stored biomass only, which is why KC05 had
#                    to exist at all. Mean individual mass = Biomass / Abundance.
#   krill_consumed   as KC02
#   RESOURCE + TOTAL consumption per predator -- the quantity this whole phase
#                    turns on. Without it there is no way to see the switching.
#   FULL PREY-RESOLVED consumption (19 predators x 20 prey), so no diet question
#                    needs another run. Set KC7_FULL_DIET=0 to drop it if this is
#                    ever scaled to 167 members, where it would be ~43M rows.
#   FEEDING LEVEL    biomass-weighted mean per species-year. Diagnostic for
#                    whether whales are food-limited at all; if feeding level
#                    sits near 1 they are satiated and no prey change can matter.
#   yield            as KC02
#
# PROTOCOL otherwise transcribed from KC02_ensemble_pilot.R -- same cut-A rank
# order, same draws, same steady() guard, same 118-year unfished spin-up, same
# four arms, and the same catchability trap: states carry DRAWN catchability and
# refit_q() must be applied before projecting, because the krill multiplier is
# 0.1904 and skipping it makes the fishery over five times too strong.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC07_pilot_whaleres05.R"
# ENV    KC7_N (10), KC7_CORES (10), KC7_DIET_FROM (1841), KC7_FULL_DIET (1),
#        KC7_BASE54, KC7_BASE55
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OUT_LARGE <- "Output_large_files/wmin_test"
KC_ROOT   <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
ANA <- file.path(KC_ROOT, "analysis")
BASES <- c(
  phase54 = Sys.getenv("KC7_BASE54", "params_ref_sw2000_balror_mnkfish05.rds"),
  phase55 = Sys.getenv("KC7_BASE55",
                       "params_ref_sw2000_balror_mnkfish05_whres05.rds"))
# KC7_BASES selects which of them to run, comma separated. The default runs both
# for the paired pilot; a full 167-member run wants ONE, because the phase-54
# arm at that size already exists as KC02 (krill consumption, reproduced here to
# 4e-16) and KC05 (abundance/biomass, reproduced exactly).
sel <- Sys.getenv("KC7_BASES", paste(names(BASES), collapse = ","))
sel <- trimws(strsplit(sel, ",")[[1]]); sel <- sel[nzchar(sel)]
if (length(setdiff(sel, names(BASES))))
  stop("KC7_BASES must be a subset of: ", paste(names(BASES), collapse = ", "),
       call. = FALSE)
BASES <- BASES[sel]
N         <- as.integer(Sys.getenv("KC7_N", "10"))
DIET_FROM <- as.integer(Sys.getenv("KC7_DIET_FROM", "1841"))
FULL_DIET <- Sys.getenv("KC7_FULL_DIET", "1") != "0"
# 16 cores on this machine; never commit all of them.
CORES     <- min(as.integer(Sys.getenv("KC7_CORES", "10")), 15, 2 * N)
# The unsuffixed name is the both-bases pilot, so KC08's default input keeps
# working; a single-base run gets its own stem and cannot clobber it.
STEM      <- sprintf("KC07_whres05_n%d%s", N,
                     if (length(BASES) > 1) "" else paste0("_", sel))
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
cat("=== KC07: krill counterfactuals, top", N, "members,", length(BASES),
    "reference model(s):", paste(names(BASES), collapse = ", "), "===\n")
for (b in names(BASES)) if (!file.exists(BASES[[b]]))
  stop("missing base params for ", b, ": ", BASES[[b]],
       if (b == "phase55") "\n  run R/wmin_test/55_whale_resource_interaction.R first"
       else "", call. = FALSE)

# --- members, in post-refit rank order (KC02:71-87) --------------------------
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
cat("members:", paste(members, collapse = ", "), "\n")

# --- bases, and the assertion that they differ ONLY in the intended way ------
BASE_OBJ <- lapply(BASES, function(f) suppressWarnings(validParams(readRDS(f))))
for (b in names(BASE_OBJ)) stopifnot(
  identical(BASE_OBJ[[b]]@rates_funcs$Encounter, "therMizerEncounter"),
  identical(BASE_OBJ[[b]]@resource_dynamics, "plankton_forcing"),
  identical(BASE_OBJ[[b]]@second_order_w$flux, "upwind"))
SP <- BASE_OBJ[[1]]@species_params$species
for (b in names(BASE_OBJ))
  if (!identical(SP, BASE_OBJ[[b]]@species_params$species))
    stop("species order differs between bases: ", b, call. = FALSE)
i_kr <- which(SP == KRILL); i_wh <- match(WHALES, SP)
ir <- vapply(BASE_OBJ, function(p) p@species_params$interaction_resource,
             numeric(length(SP)))
cat("\ninteraction_resource, whales:\n")
for (b in names(BASE_OBJ))
  cat(sprintf("  %-8s %s\n", b, paste(ir[i_wh, b], collapse = "/")))
# Only meaningful with more than one base loaded; with a single base there is
# nothing to compare and the check is silently satisfied.
if (length(BASE_OBJ) > 1) {
  if (!all(apply(ir[-i_wh, , drop = FALSE], 1,
                 function(v) length(unique(v)) == 1)))
    stop("the bases differ in interaction_resource for a non-whale group")
  cat("all other groups identical on that column across bases: TRUE\n")
}

# --- effort arms (KC02:96-110) ------------------------------------------------
eff_obs <- readRDS("effort_array_1841_2010.rds")
stopifnot(identical(colnames(eff_obs), SP))
yrs_eff <- as.numeric(rownames(eff_obs)); kr_eff <- eff_obs[, KRILL]
peak_year <- yrs_eff[which.max(kr_eff)]; peak_val <- max(kr_eff)
eff_none <- eff_obs; eff_none[, KRILL] <- 0
eff_peak <- eff_obs; eff_peak[yrs_eff >= peak_year, KRILL] <- peak_val
ARM_EFF <- list(unexploited = NULL, exploited = eff_obs, no_krill = eff_none,
                peak_krill = eff_peak)
cat("krill multiplier:", signif(MULT[[KRILL]], 5), "| peak", peak_year,
    "| cores", CORES, "| full diet:", FULL_DIET, "\n\n")

JOBS <- expand.grid(sim_index = members, base = names(BASES),
                    stringsAsFactors = FALSE)

# ------------------------------------------------------------------ worker ---
worker <- function(k) {
  suppressPackageStartupMessages({
    library(mizer); library(therMizer); library(dplyr)
  })
  source(file.path("R", "wmin_test", "thermizer_shim.R"))
  si <- JOBS$sim_index[k]; btag <- JOBS$base[k]
  BASE <- BASE_OBJ[[btag]]; dw_ <- DR$draws[[as.character(si)]]
  fail <- function(r) list(sim_index = si, base = btag, ok = FALSE, reason = r)

  check_stability <- function(s) {              # KC02:120-146
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
  apply_draws <- function(p) {                  # KC02:147-161
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
  refit_q <- function(p) {                      # KC02:162-167
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
  # the intervention must survive apply_draws()
  if (!isTRUE(all.equal(species_params(p)$interaction_resource,
                        BASE@species_params$interaction_resource)))
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
  BIO <- CONS <- FULL <- FL <- YL <- list()
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
      sim_index = si, base = btag, arm = nm, Year = yr[j], Species = SP,
      Abundance = as.numeric(sim@n[j, , ] %*% dw),
      Biomass   = as.numeric(sim@n[j, , ] %*% wdw), stringsAsFactors = FALSE)))

    fl <- getFeedingLevel(sim)                 # time x species x size
    FL[[nm]] <- do.call(rbind, lapply(ki, function(j) {
      wt <- sim@n[j, , ] * rep(wdw, each = length(SP))
      tot <- rowSums(wt)
      data.frame(sim_index = si, base = btag, arm = nm, Year = yr[j],
                 Species = SP,
                 feeding_level = ifelse(tot > 0,
                                        rowSums(fl[j, , ] * wt) / tot, NA_real_),
                 stringsAsFactors = FALSE) }))

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
      data.frame(sim_index = si, base = btag, arm = nm, Year = y, Species = SP,
                 total_consumed = rowSums(C),
                 krill_consumed = C[, KRILL],
                 resource_consumed = C[, "Resource"],
                 stringsAsFactors = FALSE) }))
    if (FULL_DIET)
      FULL[[nm]] <- do.call(rbind, lapply(ys, function(y) {
        C <- dl[[as.character(y)]]
        data.frame(sim_index = si, base = btag, arm = nm, Year = y,
                   predator = rep(rownames(C), ncol(C)),
                   prey = rep(colnames(C), each = nrow(C)),
                   consumed = as.numeric(C), stringsAsFactors = FALSE) }))

    yy <- getYield(sim)
    YL[[nm]] <- data.frame(sim_index = si, base = btag, arm = nm,
                           Year = as.numeric(rownames(yy))[ki],
                           krill_yield_g = as.numeric(yy[ki, KRILL]),
                           stringsAsFactors = FALSE)
  }
  list(sim_index = si, base = btag, ok = TRUE, reason = "ok",
       stable = isTRUE(stab$stable), max_cv = stab$max_cv,
       q_krill = gear_params(pq)$catchability[match(KRILL, gear_params(pq)$species)],
       biomass = do.call(rbind, BIO), consumption = do.call(rbind, CONS),
       full_diet = if (FULL_DIET) do.call(rbind, FULL) else NULL,
       feeding = do.call(rbind, FL), yield = do.call(rbind, YL))
}

# --------------------------------------------------------------------- run ---
CHUNK <- as.integer(Sys.getenv("KC7_CHUNK", as.character(CORES)))
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
  cat("jobs:", nrow(JOBS), "(", N, "members x", length(BASES), "bases ) |",
      length(chunks), "chunks |", length(todo), "to run\n\n")
  if (length(todo)) {
    cl <- makeCluster(CORES)
    on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
    clusterExport(cl, c("BASE_OBJ", "JOBS", "DR", "MULT", "ARM_EFF", "SP",
                        "i_kr", "STEADY_TOL", "STEADY_TMAX", "SPINUP_YEARS",
                        "QMAX", "KRILL", "END_YEAR", "DIET_FROM", "FULL_DIET",
                        "STAB"), envir = environment())
    for (k in seq_along(todo)) {
      ci <- todo[k]; tc <- proc.time()
      r <- parLapply(cl, chunks[[ci]], worker)
      saveRDS(r, chunk_file(ci))
      cat(sprintf("[%s] chunk %d/%d done (%d jobs, %.1f min) | elapsed %.1f min\n",
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
cat("\nassembled", length(res), "jobs |", sum(ok), "ok\n")
if (any(!ok))
  cat("  failures:", paste(vapply(res[!ok], function(r)
    sprintf("%d/%s(%s)", r$sim_index, r$base, r$reason), ""), collapse = ", "),
    "\n")
res <- res[ok]

MEM <- do.call(rbind, lapply(res, function(r) data.frame(
  sim_index = r$sim_index, base = r$base, stable = r$stable,
  max_cv = r$max_cv, q_krill = r$q_krill)))
cat("\n=== members ===\n")
print(as.data.frame(MEM %>% mutate(across(where(is.numeric), ~signif(.x, 5)))),
      row.names = FALSE)
# A member that converges under one base and not the other cannot be compared;
# the summary below uses only members stable under BOTH.
both <- MEM %>% group_by(sim_index) %>%
  summarise(n_ok = n(), all_stable = all(stable), .groups = "drop") %>%
  filter(n_ok == length(BASES), all_stable) %>% pull(sim_index)
cat("\nmembers usable under BOTH bases:", length(both), "of", N, "\n")

meta <- list(bases = BASES, n_requested = N, members = members,
             members_both = both, spinup_years = SPINUP_YEARS,
             peak_year = peak_year, diet_from = DIET_FROM,
             full_diet = FULL_DIET, multipliers = MULT,
             built = format(Sys.time()))
saveRDS(list(biomass = do.call(rbind, lapply(res, `[[`, "biomass")),
             consumption = do.call(rbind, lapply(res, `[[`, "consumption")),
             full_diet = if (FULL_DIET)
               do.call(rbind, lapply(res, `[[`, "full_diet")) else NULL,
             feeding = do.call(rbind, lapply(res, `[[`, "feeding")),
             yield = do.call(rbind, lapply(res, `[[`, "yield")),
             members = MEM, meta = meta), OUT_RDS)
write.csv(MEM, file.path(ANA, paste0(STEM, "_members.csv")), row.names = FALSE)
cat("\nwrote", OUT_RDS, sprintf("(%.0f MB)\n", file.info(OUT_RDS)$size / 1024^2))
cat("elapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")
cat("KC07 complete -- run KC08_compare_whaleres05.R for the assessment.\n")