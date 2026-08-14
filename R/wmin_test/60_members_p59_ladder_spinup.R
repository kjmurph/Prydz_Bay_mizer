# =============================================================================
# Phase 60 -- top-10 cut-A members on the phase-59 reference, with a TOLERANCE
# LADDER at steady() and erepro left free
#
# ISOLATED TRIAL. Different reference model, different steady() protocol and a
# different reproduction treatment from every previous member run. Writes only
# new files.
#
# ------------------------------------------------------------- the protocol
# base   params_ref_p59_cap09_tol001.rds  (phase 57 subsidy + 1 g in-domain
#        resource ceiling + reproduction level capped at 0.9 + re-laddered to
#        tol 0.001 with erepro free)
#
#   1. apply_draws   gamma, catchability, abundance_scaling -- verbatim from
#                    KC07/KC14/44, so the draws enter exactly as before
#   2. LADDER steady() 0.1 -> 0.05 -> 0.01 -> 0.005 -> 0.002 -> 0.001, each rung
#                    starting from the previous rung's output
#   3. 118 yr unfished spin-up, 1 cycle
#   4. stability screen (the 44/53 screen, unchanged)
#
# TWO DEPARTURES FROM THE ESTABLISHED MEMBER PIPELINE, both deliberate:
#
# A. THE LADDER REPLACES THE SINGLE steady() CALL. KC07/KC14/44 run one bare
#    steady(tol = 0.0025). Here six rungs tighten to 0.001, each starting from
#    the last, and the state carried forward is the one from the TIGHTEST rung
#    that CONVERGED -- not simply the last, because a rung that fails to
#    converge can leave a worse state behind than the rung before it.
#
# B. preserve = "reproduction_level", NOT "erepro". Every previous member run
#    held erepro and let R_max absorb the mismatch, which is why reproduction
#    level sat near 1 for almost every group. Holding the LEVEL instead lets
#    erepro update freely while the members inherit the phase-58 cap of 0.9
#    rather than drifting back toward 0.999.
#
#    mizer's steady() ALWAYS holds exactly one of reproduction_level / erepro /
#    R_max -- the argument is match.arg'd, there is no "preserve nothing". So
#    with the level held, erepro is free but the level is NOT. If the level also
#    needs to move, the setting is preserve = "R_max" (P60_PRESERVE), which is
#    the only one under which both are free -- but it is also the setting that
#    produced erepro 2.14 (minke) and 17.05 (small divers) at reference level in
#    phase 57, and members carry drawn gamma 6-1708x the base for the whale
#    groups. Admissibility is therefore TRACKED PER RUNG under any setting: a
#    member whose erepro exceeds 1 is a finding, not an error, and must not be
#    silently kept.
#
# C. NO matchBiomasses. The reference ladders alternate matchBiomasses with
#    steady, but at member level that would force every member back onto the
#    observed biomasses and erase the very spread the ensemble represents. The
#    established member pipeline (KC07:228, KC14, 44) uses a bare steady(); that
#    is kept, and only the single call is replaced by the ladder.
#
# WHAT IS SAVED. Per member: the post-steady params, the post-spin-up abundances
# and the full convergence trace, so projections can be run later without
# repeating the expensive part -- the same pattern as 44_states/ and
# kc02_state_*. Re-projecting a stored state is exact because the spin-up is
# unfished.
#
# USAGE  Rscript R/wmin_test/60_members_p59_ladder_spinup.R [run|collect|status]
# ENV    P60_BASE, P60_N (10), P60_CORES (10), P60_TOLS,
#        P60_PRESERVE (reproduction_level)
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OUT_LARGE <- "Output_large_files/wmin_test"
BASE_FILE <- Sys.getenv("P60_BASE", "params_ref_p59_cap09_tol001.rds")
N       <- as.integer(Sys.getenv("P60_N", "10"))
CORES   <- min(as.integer(Sys.getenv("P60_CORES", "10")), 15, max(1, N))
TOLS    <- as.numeric(trimws(strsplit(
  Sys.getenv("P60_TOLS", "0.1,0.05,0.01,0.005,0.002,0.001"), ",")[[1]]))
PRESERVE <- Sys.getenv("P60_PRESERVE", "reproduction_level")
# Re-cap the reproduction level AFTER apply_draws and before the ladder. Set to
# NA to reproduce the first run, which did not and was unusable -- see the
# comment at the re-cap itself.
RECAP <- suppressWarnings(as.numeric(Sys.getenv("P60_RECAP", "0.9")))
STEADY_TMAX  <- as.integer(Sys.getenv("P60_TMAX", "1500"))
SPINUP_YEARS <- 118
STEM      <- sprintf("60_p59_ladder_n%d_%s%s", N, PRESERVE,
                     if (is.finite(RECAP)) sprintf("_recap%g", RECAP) else "")
STATE_DIR <- file.path(OUT_LARGE, paste0(STEM, "_states"))
CHUNK_DIR <- file.path(OUT_LARGE, paste0(STEM, "_chunks"))
OUT_RDS   <- file.path(OUT_LARGE, paste0(STEM, ".rds"))
for (d in c(STATE_DIR, CHUNK_DIR)) dir.create(d, recursive = TRUE,
                                              showWarnings = FALSE)
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"
stopifnot(mode %in% c("run", "collect", "status"))

STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

t0 <- proc.time()
cat("=== Phase 60: top-", N, " members on ", BASE_FILE, " ===\n", sep = "")
cat("steady ladder:", paste(TOLS, collapse = " -> "), "| preserve:", PRESERVE,
    "| t_max:", STEADY_TMAX, "| cores:", CORES, "\n")
cat(if (PRESERVE == "reproduction_level")
  "  erepro is free; the level is HELD at whatever each member inherits (capped at 0.9).\n"
  else if (PRESERVE == "R_max")
  "  NOTE: preserve = R_max frees BOTH reproduction parameters, and is what\n  produced erepro > 1 at reference level in phase 57. Watch the trace.\n"
  else "  NOTE: preserve = erepro reproduces the established behaviour.\n")
if (!file.exists(BASE_FILE))
  stop("missing base params: ", BASE_FILE, call. = FALSE)

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
cat("members:", paste(members, collapse = ", "), "\n")

BASE <- suppressWarnings(validParams(readRDS(BASE_FILE)))
stopifnot(identical(BASE@rates_funcs$Encounter, "therMizerEncounter"),
          identical(BASE@resource_dynamics, "plankton_forcing"),
          identical(BASE@second_order_w$flux, "upwind"))
SPN <- BASE@species_params$species
# the phase-57/58/59 inheritance, asserted rather than assumed
if (all(BASE@ext_encounter == 0))
  stop("base carries no out-of-domain subsidy -- wrong reference model",
       call. = FALSE)
cat(sprintf("base: w_pp_cutoff %g g | max erepro %.4f | max repro level %.4f\n\n",
            BASE@resource_params$w_pp_cutoff, max(BASE@species_params$erepro),
            max(as.numeric(getReproductionLevel(BASE)))))

# ------------------------------------------------------------------ worker ---
worker <- function(si) {
  suppressPackageStartupMessages({library(mizer); library(therMizer)})
  dw_ <- DR$draws[[as.character(si)]]
  fail <- function(r, extra = NULL)
    c(list(sim_index = si, ok = FALSE, reason = r), extra)

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
  # KC07:188-202, with ONE addition forced by the phase-57 subsidy.
  #
  # `species_params(p) <- sp` SILENTLY ZEROES ext_encounter. Measured against
  # mizer 3.1.0: gear_params<- preserves it, direct initial_n slot assignment
  # preserves it, steady() preserves it, but species_params<- and setParams()
  # both reset it to 0. Same class of trap as phase 54's set_betas() losing
  # rates_funcs to setParams(). Without the save/restore below every member
  # would run on a model with NO out-of-domain subsidy while claiming to have
  # one -- the whole point of the phase, gone in silence.
  apply_draws <- function(p) {
    ext0 <- p@ext_encounter
    gp <- gear_params(p)
    if (!identical(paste(gp$gear, gp$species), names(dw_$catchability)))
      stop("gear_params rows differ between the base and the stored member")
    gp$catchability <- as.numeric(dw_$catchability); gear_params(p) <- gp
    sp <- species_params(p)
    if (!identical(sp$species, names(dw_$gamma)))
      stop("species order differs between the base and the draw")
    sp$gamma <- as.numeric(dw_$gamma)
    species_params(p) <- sp
    # Slot assignment, NOT setExtEncounter(): the setter restores the values
    # exactly (max abs diff 0) but rewrites the dimnames attribute structure,
    # so an all.equal check against the original fails on attributes. The slot
    # assignment round-trips exactly and still passes validParams().
    p@ext_encounter <- ext0
    sc <- as.numeric(dw_$abundance_scaling[sp$species])
    for (j in seq_along(sc)) p@initial_n[j, ] <- p@initial_n[j, ] * sc[j]
    stopifnot(identical(p@rates_funcs$Encounter, "therMizerEncounter"),
              isTRUE(all.equal(p@ext_encounter, ext0)))
    p
  }
  # mizer 3.1.0 signals non-convergence with message(), not warning()
  steady_guarded <- function(p, tol) {
    nc <- FALSE
    out <- withCallingHandlers(
      try(steady(p, tol = tol, t_max = STEADY_TMAX, preserve = PRESERVE,
                 progress_bar = FALSE), silent = TRUE),
      message = function(m) {
        if (grepl("did not converge", conditionMessage(m), ignore.case = TRUE))
          nc <<- TRUE
        invokeRestart("muffleMessage")
      }, warning = function(w) invokeRestart("muffleWarning"))
    list(params = out, converged = !nc, errored = inherits(out, "try-error"))
  }

  p <- try(apply_draws(BASE), silent = TRUE)
  # carry the message: a bare "draw_error" hides which assertion tripped
  if (inherits(p, "try-error"))
    return(fail(paste0("draw_error: ",
                       trimws(gsub("\\s+", " ", as.character(p))))))
  if (all(p@ext_encounter == 0)) return(fail("subsidy_lost_in_draws"))
  rl_draws <- try(as.numeric(getReproductionLevel(p)), silent = TRUE)

  # RE-CAP AFTER THE DRAWS. preserve = "reproduction_level" holds the level of
  # the object PASSED IN, and apply_draws has already moved it: drawn gamma is
  # 6-1708x base for the whale groups, which raises RDI and pushes the level up.
  # Measured on the first run of this phase, 110 of 190 member-species pairs
  # entered steady() above 0.9 where the base had only 2 of 19, and the required
  # erepro diverges as the level approaches 1 -- small divers entered at 1.0000
  # and came out needing erepro 8111. Capping here restores the phase-58 intent:
  # steady() then preserves a capped level instead of an inflated one.
  if (is.finite(RECAP)) {
    rl_now <- try(as.numeric(getReproductionLevel(p)), silent = TRUE)
    if (inherits(rl_now, "try-error")) return(fail("repro_level_error"))
    p2 <- try(suppressWarnings(
      setBevertonHolt(p, reproduction_level = pmin(rl_now, RECAP))),
      silent = TRUE)
    if (inherits(p2, "try-error")) return(fail("recap_error"))
    p <- p2
  }
  rl_in <- try(as.numeric(getReproductionLevel(p)), silent = TRUE)

  # --- the ladder ---------------------------------------------------------
  tr <- list(); keep <- NULL; keep_tol <- NA_real_
  for (tol in TOLS) {
    st <- steady_guarded(p, tol)
    if (st$errored) {
      tr[[length(tr) + 1]] <- data.frame(sim_index = si, tol = tol,
        converged = NA, errored = TRUE, max_erepro = NA_real_,
        n_erepro_ge1 = NA_integer_, max_repro_level = NA_real_)
      break
    }
    p <- st$params
    er <- p@species_params$erepro
    rl <- try(as.numeric(getReproductionLevel(p)), silent = TRUE)
    if (inherits(rl, "try-error")) rl <- NA_real_
    tr[[length(tr) + 1]] <- data.frame(
      sim_index = si, tol = tol, converged = st$converged, errored = FALSE,
      max_erepro = max(er), n_erepro_ge1 = sum(er >= 1),
      max_repro_level = suppressWarnings(max(rl, na.rm = TRUE)))
    if (st$converged) { keep <- p; keep_tol <- tol }
  }
  TR <- do.call(rbind, tr)
  if (is.null(keep)) return(fail("no_rung_converged", list(trace = TR)))
  ps <- keep

  s0 <- try(project(ps, t_start = 1841, t_max = SPINUP_YEARS, effort = 0,
                    progress_bar = FALSE), silent = TRUE)
  if (inherits(s0, "try-error"))
    return(fail("spinup_error", list(trace = TR)))
  init <- s0@n[SPINUP_YEARS, , ]
  stab <- check_stability(s0)

  saveRDS(list(sim_index = si, params = ps, initial_n = init,
               tightest_tol = keep_tol, trace = TR),
          file.path(STATE_DIR, sprintf("state_%05d.rds", si)))

  spq <- species_params(ps)
  rl <- try(as.numeric(getReproductionLevel(ps)), silent = TRUE)
  if (inherits(rl, "try-error")) rl <- rep(NA_real_, length(SPN))
  wdw <- ps@w * ps@dw
  list(sim_index = si, ok = TRUE, reason = "ok", trace = TR,
       tightest_tol = keep_tol,
       stable = isTRUE(stab$stable), max_cv = stab$max_cv,
       max_erepro = max(spq$erepro), n_erepro_ge1 = sum(spq$erepro >= 1),
       repro = data.frame(sim_index = si, species = SPN, erepro = spq$erepro,
                          R_max = spq$R_max, repro_level = rl,
                          repro_level_draws = if (inherits(rl_draws, "try-error"))
                            NA_real_ else rl_draws,
                          repro_level_in = if (inherits(rl_in, "try-error"))
                            NA_real_ else rl_in, stringsAsFactors = FALSE),
       biomass_1841 = data.frame(sim_index = si, species = SPN,
                                 biomass = as.numeric(init %*% wdw),
                                 stringsAsFactors = FALSE))
}

# --------------------------------------------------------------------- run ---
chunk_file <- function(ci) file.path(CHUNK_DIR, sprintf("res_%03d.rds", ci))
chunks <- split(members, ceiling(seq_along(members) / CORES))
if (mode == "status") {
  cat(sprintf("chunks %d/%d complete\n",
              sum(vapply(seq_along(chunks), function(ci)
                file.exists(chunk_file(ci)), logical(1))), length(chunks)))
  quit(save = "no")
}
if (mode == "run") {
  todo <- which(!vapply(seq_along(chunks),
                        function(ci) file.exists(chunk_file(ci)), logical(1)))
  cat("chunks to run:", length(todo), "of", length(chunks), "\n\n")
  if (length(todo)) {
    cl <- makeCluster(CORES)
    on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
    clusterExport(cl, c("BASE", "DR", "SPN", "TOLS", "PRESERVE", "STEADY_TMAX",
                        "SPINUP_YEARS", "STAB", "STATE_DIR", "RECAP"),
                  envir = environment())
    for (ci in todo) {
      tc <- proc.time()
      r <- parLapply(cl, chunks[[ci]], worker)
      saveRDS(r, chunk_file(ci))
      cat(sprintf("[%s] chunk %d/%d done (%d members, %.1f min)\n",
                  format(Sys.time(), "%H:%M:%S"), ci, length(chunks),
                  length(chunks[[ci]]), (proc.time() - tc)[["elapsed"]] / 60))
      flush.console()
    }
    stopCluster(cl)
  }
}

res <- unlist(lapply(seq_along(chunks), function(ci) {
  f <- chunk_file(ci); if (file.exists(f)) readRDS(f) else NULL
}), recursive = FALSE)
if (!length(res)) stop("no chunk results in ", CHUNK_DIR)
ok <- vapply(res, function(r) isTRUE(r$ok), logical(1))
cat("\nassembled", length(res), "members |", sum(ok), "ok\n")
if (any(!ok)) cat("  failures:", paste(vapply(res[!ok], function(r)
  sprintf("%d(%s)", r$sim_index, r$reason), ""), collapse = ", "), "\n")

# A member that fails BEFORE the ladder (draw_error, subsidy_lost) carries no
# trace, so this is NULL when every member fails early -- report that rather
# than dying inside group_by and hiding the actual failure reasons above.
TRACE <- do.call(rbind, lapply(res, function(r) r$trace))
cat("\n=== ladder: convergence and admissibility by rung ===\n")
if (is.null(TRACE) || !nrow(TRACE)) {
  cat("  no member reached the ladder -- see the failure reasons above\n")
} else {
  print(as.data.frame(TRACE %>% group_by(tol) %>%
    summarise(members = n(), converged = sum(converged, na.rm = TRUE),
              with_erepro_ge1 = sum(n_erepro_ge1 > 0, na.rm = TRUE),
              max_erepro = signif(max(max_erepro, na.rm = TRUE), 4),
              max_repro_level = signif(max(max_repro_level, na.rm = TRUE), 4),
              .groups = "drop") %>% arrange(desc(tol))), row.names = FALSE)
}

res <- res[ok]
if (length(res)) {
  MEM <- do.call(rbind, lapply(res, function(r) data.frame(
    sim_index = r$sim_index, tightest_tol = r$tightest_tol,
    stable = r$stable, max_cv = r$max_cv, max_erepro = r$max_erepro,
    n_erepro_ge1 = r$n_erepro_ge1)))
  cat("\n=== members ===\n"); print(MEM, row.names = FALSE, digits = 4)
  cat("\n  reached tol", min(TOLS), ":", sum(MEM$tightest_tol <= min(TOLS)),
      "of", nrow(MEM), "| stable:", sum(MEM$stable),
      "| with erepro >= 1:", sum(MEM$n_erepro_ge1 > 0), "\n")

  REP <- do.call(rbind, lapply(res, `[[`, "repro"))
  cat("\n=== reproduction across members ===\n")
  print(as.data.frame(REP %>% group_by(species) %>%
    summarise(median_rl = round(median(repro_level, na.rm = TRUE), 4),
              min_rl = round(min(repro_level, na.rm = TRUE), 4),
              max_rl = round(max(repro_level, na.rm = TRUE), 4),
              median_erepro = signif(median(erepro), 3),
              max_erepro = signif(max(erepro), 3),
              n_ge1 = sum(erepro >= 1), .groups = "drop") %>%
    slice(match(SPN, species))), row.names = FALSE)
  base_rl <- as.numeric(getReproductionLevel(BASE))
  cat("\n  reference model for comparison: median level",
      round(median(base_rl), 4), "| max", round(max(base_rl), 4),
      "| max erepro", round(max(BASE@species_params$erepro), 4), "\n")

  saveRDS(list(members = MEM, repro = REP, trace = TRACE,
               biomass_1841 = do.call(rbind, lapply(res, `[[`, "biomass_1841")),
               meta = list(base = BASE_FILE, tols = TOLS, preserve = PRESERVE,
                           members = members, spinup_years = SPINUP_YEARS,
                           built = format(Sys.time()))), OUT_RDS)
  write.csv(MEM, file.path(OUT_LARGE, paste0(STEM, "_members.csv")),
            row.names = FALSE)
  write.csv(REP, file.path(OUT_LARGE, paste0(STEM, "_reproduction.csv")),
            row.names = FALSE)
  write.csv(TRACE, file.path(OUT_LARGE, paste0(STEM, "_ladder_trace.csv")),
            row.names = FALSE)
  cat("\nwrote", OUT_RDS, "and", length(list.files(STATE_DIR)), "member states\n")
}
cat("elapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")
cat("Phase 60 complete.\n")