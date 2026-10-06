# =============================================================================
# mc_functions.R
# Helper functions for the preliminary Monte Carlo (MC) parameter exploration
# of the Prydz Bay therMizer model. Used by 09_MC_uncertainty_analysis.Rmd.
#
# Requires mizer (2.5.x) and therMizer attached; ggplot2 for the figures.
# Every function is prefixed mc_ so the set can be exported to parallel workers.
#
# -----------------------------------------------------------------------------
# WORKFLOW
# -----------------------------------------------------------------------------
# Aim: find combinations of three poorly constrained parameter groups for which
# the model, run through the historical period (1841-2010), ends close to the
# contemporary (2010-2020) biomass estimates.
#
#   Parameter group                       Perturbed          Unit of variation
#   1. Gear catchability                  fished gears (9)   multiplier per gear
#   2. Search-volume coefficient (gamma)  all groups (19)    multiplier per group
#   3. Initial abundance                  all groups (19)    multiplier per group
#
# Each ensemble member i:
#   a. SAMPLE. Draws log-normal multipliers m = exp(N(log_mean, sd)) using its
#      own random-number stream, set.seed(seed_base + i, kind = "L'Ecuyer-CMRG").
#      Applied values are bounded: catchability to [0, 1] (absolute), gamma to
#      [0.8, 100] x the stage-base value, abundance multipliers to >= 0.5.
#   b. RE-EQUILIBRATE. mizer::steady() with erepro preserved. steady() holds
#      recruitment at the level implied by the perturbed initial abundances and
#      then recalculates R_max, so the abundance multiplier acts on the model
#      through the recalculated R_max rather than only as an initial condition.
#      Whether steady() converged, and any erepro it had to raise above the
#      preserved value, are recorded; neither rejects a member by default (as in
#      the original analysis), see reject_unconverged_steady / reject_erepro_above.
#   c. SPIN UP. Unfished projection of 118 years from 1841 (forced by the
#      1841-1959 temperature and plankton series, as in the baseline runs). The
#      state in 1958 (row 118) starts the historical run. Stability test: every
#      group must have CV <= 0.25 over the last 40 years of the spin-up and no
#      significant linear trend (|slope/mean| > 0.025 per year, p < 0.05) over
#      the first 50 years.
#   d. HISTORICAL RUN. 1841-2010 with the historical effort array (the effort
#      array's time axis defines the years).
#   e. STORE. Annual biomass and yield, all multipliers and diagnostics.
#
# ACCEPTANCE (rejection sampling): a completed member is accepted if its mean
# biomass over 2000-2010 lies within the observational bounds for all but at
# most 3 of the 19 groups.
#
# STAGES: Stage 1 samples widely around the reference model. Stage 2 samples
# again around the centre (arithmetic mean, as in the original analysis) of the
# stage-1 accepted multipliers. The stage-2 centre, expressed relative to the
# reference model, gives the final parameter set.
#
# Multipliers are stored on three scales: "raw" (the draw), "stage" (applied,
# relative to that stage's base model) and "reference" (applied, relative to
# the reference model, i.e. params_sel_adj). Stage-2 centres are computed on the
# reference scale so they compose correctly with the stage-1 centre.
# =============================================================================


# ---- 1. Settings --------------------------------------------------------------

#' All MC settings in one list. Defaults reproduce stage 1 of the original 09
#' analysis. Pass named arguments to override (unknown names are an error).
mc_settings <- function(...) {
  s <- list(
    # sampling: multiplier = exp(N(log_mean, sd))
    catchability_sd       = 2,
    abundance_sd          = 3,
    gamma_sd              = 2,
    catchability_log_mean = log(1.2),
    abundance_log_mean    = log(1.2),
    gamma_log_mean        = log(1.2),
    gamma_bounds          = c(0.8, 100),
    abundance_floor       = 0.5,
    catchability_limits   = c(0, 1),
    seed_base             = 20250907,
    # steady state
    steady_tol                = 0.0025,
    steady_t_max              = 1500,
    steady_preserve           = "erepro",
    reject_unconverged_steady = FALSE,
    reject_erepro_above       = Inf,
    # spin-up and stability
    t_start                       = 1841,
    spinup_years                  = 118,
    spinup_cycles                 = 1,
    spinup_early_exit             = TRUE,
    spinup_min_cycles             = 1,
    stability_cv_max              = 0.25,
    stability_cv_years            = 40,
    stability_trend_years         = 50,
    stability_trend_rel_slope_max = 0.025,
    stability_trend_p             = 0.05,
    stability_min_mean_biomass    = 1,
    # acceptance
    accept_years       = 2000:2010,
    accept_max_outside = 3,
    # storage
    keep_sims  = FALSE,
    block_size = NULL
  )
  over <- list(...)
  unknown <- setdiff(names(over), names(s))
  if (length(unknown)) stop("Unknown MC setting(s): ", paste(unknown, collapse = ", "))
  s[names(over)] <- over
  if (s$spinup_min_cycles > s$spinup_cycles) stop("spinup_min_cycles cannot exceed spinup_cycles")
  s
}

#' One-line description of each setting (used by mc_settings_table()).
mc_settings_descriptions <- function() {
  c(
    catchability_sd       = "SD of log catchability multiplier (fished gears only)",
    abundance_sd          = "SD of log initial-abundance multiplier (all groups)",
    gamma_sd              = "SD of log search-volume (gamma) multiplier (all groups)",
    catchability_log_mean = "Mean of log catchability multiplier (log(1.2): median 1.2x)",
    abundance_log_mean    = "Mean of log abundance multiplier",
    gamma_log_mean        = "Mean of log gamma multiplier",
    gamma_bounds          = "Applied gamma limited to this range x stage-base gamma",
    abundance_floor       = "Minimum applied abundance multiplier",
    catchability_limits   = "Applied catchability limited to this absolute range",
    seed_base             = "Member i: set.seed(seed_base + i, kind = \"L'Ecuyer-CMRG\")",
    steady_tol            = "mizer::steady() convergence tolerance",
    steady_t_max          = "Maximum years allowed for mizer::steady()",
    steady_preserve       = "Parameter held fixed by steady() (R_max recalculated)",
    reject_unconverged_steady = "Reject members whose steady() did not converge",
    reject_erepro_above   = "Reject members whose post-steady erepro exceeds this (Inf: off, as originally)",
    t_start               = "First model year",
    spinup_years          = "Years per unfished spin-up cycle; row spinup_years starts the historical run",
    spinup_cycles         = "Maximum number of spin-up cycles",
    spinup_early_exit     = "Stop cycling once the stability test passes",
    spinup_min_cycles     = "Cycles completed before the first stability test",
    stability_cv_max      = "Max CV of biomass over the last stability_cv_years (every group)",
    stability_cv_years    = "Years at the end of the spin-up used for the CV test",
    stability_trend_years = "Years at the start of the spin-up used for the trend test",
    stability_trend_rel_slope_max = "Max |slope / mean biomass| per year",
    stability_trend_p     = "A trend fails only if its slope p-value is below this",
    stability_min_mean_biomass = "Groups with mean biomass (g) below this skip the trend test",
    accept_years          = "Model years averaged for the biomass acceptance test",
    accept_max_outside    = "Max number of groups allowed outside their biomass bounds",
    keep_sims             = "Also store full MizerSim objects (large)",
    block_size            = "Members per saved block (NULL: 4 x cores)"
  )
}

#' Settings as a table; pass several settings lists to compare stages.
mc_settings_table <- function(...) {
  sets <- list(...)
  if (is.null(names(sets)) || any(names(sets) == "")) names(sets) <- paste0("stage", seq_along(sets))
  fmt <- function(v) {
    if (is.null(v)) return("NULL")
    if (is.numeric(v) && length(v) > 3) return(paste0(min(v), "-", max(v)))
    paste(format(v, digits = 4), collapse = ", ")
  }
  keys <- names(sets[[1]])
  out <- data.frame(setting = keys, stringsAsFactors = FALSE)
  for (nm in names(sets)) out[[nm]] <- vapply(sets[[nm]][keys], fmt, "")
  out$description <- unname(mc_settings_descriptions()[keys])
  out
}


# ---- 2. Observations ----------------------------------------------------------

#' Biomass acceptance bounds. Data-derived bounds are used where given; missing
#' bounds are filled with (1 -/+ assumed_fraction) x the observed mean.
#' @param obs data.frame(Species, ObsBiomass) in grams.
mc_biomass_bounds <- function(obs, lower_g = NULL, upper_g = NULL, assumed_fraction = 0.5) {
  n <- nrow(obs)
  lower <- if (is.null(lower_g)) rep(NA_real_, n) else as.numeric(lower_g)
  upper <- if (is.null(upper_g)) rep(NA_real_, n) else as.numeric(upper_g)
  assumed <- is.na(lower) | is.na(upper)
  lower[is.na(lower)] <- (1 - assumed_fraction) * obs$ObsBiomass[is.na(lower)]
  upper[is.na(upper)] <- (1 + assumed_fraction) * obs$ObsBiomass[is.na(upper)]
  data.frame(Species = as.character(obs$Species), ObsBiomass = obs$ObsBiomass,
             Lower_g = lower, Upper_g = upper,
             RangeSource = ifelse(assumed, "assumed", "data"), stringsAsFactors = FALSE)
}


# ---- 3. Sampling --------------------------------------------------------------

#' Draw the member's multipliers. The draw order (catchability for fished gears
#' in gear order, then gamma, then abundance) and the RNG kind reproduce the
#' original 09 code exactly. The caller's RNG state is restored afterwards.
mc_draw_multipliers <- function(i, base_params, s) {
  gp <- gear_params(base_params)
  species <- species_params(base_params)$species
  fished <- which(gp$catchability > 0)
  old_kind <- RNGkind()
  old_seed <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
    get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    RNGkind(old_kind[1], old_kind[2], old_kind[3])
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
        rm(".Random.seed", envir = globalenv())
    } else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(s$seed_base + i, kind = "L'Ecuyer-CMRG")
  list(
    catchability = setNames(exp(rnorm(length(fished), s$catchability_log_mean, s$catchability_sd)),
                            gp$gear[fished]),
    gamma        = setNames(exp(rnorm(length(species), s$gamma_log_mean, s$gamma_sd)), species),
    abundance    = setNames(exp(rnorm(length(species), s$abundance_log_mean, s$abundance_sd)), species)
  )
}

#' Apply drawn multipliers to a params object, with the bounds in the settings.
#' Returns the perturbed params and the multipliers actually applied.
mc_apply_multipliers <- function(params, draws, s) {
  gp <- gear_params(params)
  q0 <- gp$catchability
  fished <- which(q0 > 0)
  q <- q0
  q[fished] <- pmin(s$catchability_limits[2],
                    pmax(s$catchability_limits[1], q0[fished] * draws$catchability))
  gp$catchability <- q
  gear_params(params) <- gp

  sp <- species_params(params)
  g0 <- sp$gamma
  g <- pmin(g0 * s$gamma_bounds[2], pmax(g0 * s$gamma_bounds[1], g0 * draws$gamma))
  sp$gamma <- g
  species_params(params) <- sp

  a <- pmax(s$abundance_floor, draws$abundance)
  initialN(params) <- sweep(initialN(params), 1, a, "*")

  list(params = params,
       applied = list(catchability = setNames(q[fished] / q0[fished], gp$gear[fished]),
                      gamma        = setNames(g / g0, sp$species),
                      abundance    = setNames(a, sp$species)))
}

#' Multipliers of a perturbed (pre-steady) params object relative to a reference.
mc_relative_multipliers <- function(params, reference) {
  gp <- gear_params(params); gp0 <- gear_params(reference)
  fished <- gp0$catchability > 0
  sp <- species_params(params); sp0 <- species_params(reference)
  list(catchability = setNames(gp$catchability[fished] / gp0$catchability[fished], gp0$gear[fished]),
       gamma        = setNames(sp$gamma / sp0$gamma, sp0$species),
       abundance    = setNames(rowSums(initialN(params)) / rowSums(initialN(reference)), sp0$species))
}


# ---- 4. One ensemble member ----------------------------------------------------

#' Cheap validity checks before steady() (condensed from the original precheck;
#' the original called getMaturity(), which does not exist in mizer 2.5, so its
#' mature-biomass check was silently skipped inside try()).
mc_precheck <- function(params, effort) {
  gp <- gear_params(params); sp <- species_params(params); n0 <- initialN(params)
  bad <- function(reason) list(ok = FALSE, reason = reason)
  if (anyNA(gp$catchability) || any(gp$catchability < 0 | gp$catchability > 1))
    return(bad("catchability missing or outside [0, 1]"))
  sig <- !is.na(gp$sel_func) & gp$sel_func == "sigmoid_length"
  if (any(sig) && (any(!is.finite(gp$l25[sig]) | !is.finite(gp$l50[sig])) ||
                   any(gp$l25[sig] >= gp$l50[sig])))
    return(bad("invalid sigmoid selectivity"))
  if (any(!is.finite(sp$gamma) | sp$gamma <= 0)) return(bad("invalid gamma"))
  if (any(!is.finite(sp$erepro) | sp$erepro <= 0)) return(bad("non-positive erepro"))
  if (any(!is.finite(n0)) || any(n0 < 0)) return(bad("invalid initial abundance"))
  mature <- rowSums(n0 * maturity(params) *
                      matrix(params@w, nrow(n0), ncol(n0), byrow = TRUE))
  if (all(mature <= 1)) return(bad("no mature biomass"))
  if (any(!is.finite(effort))) return(bad("non-finite effort"))
  if (length(setdiff(unique(gp$gear), colnames(effort)))) return(bad("effort array is missing gears"))
  list(ok = TRUE, reason = NA_character_)
}

#' mizer::steady() with its messages and warnings captured as diagnostics.
#' mizer 2.5 reports non-convergence with message(), not warning(), so this is
#' the only way to detect it. "erepro has been increased" warnings mean
#' preserve = "erepro" could not be honoured for some groups.
mc_steady <- function(params, s) {
  msgs <- character(0)
  warns <- character(0)
  out <- tryCatch(
    withCallingHandlers(
      steady(params, t_max = s$steady_t_max, tol = s$steady_tol,
             preserve = s$steady_preserve, progress_bar = FALSE),
      message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") },
      warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }
    ),
    error = function(e) e
  )
  num_after <- function(pattern, txt) {
    hit <- grep(pattern, txt, value = TRUE)
    if (!length(hit)) return(NA_real_)
    as.numeric(sub(paste0(".*", pattern, "\\s*([-0-9.eE+]+).*"), "\\1", hit[length(hit)]))
  }
  diag <- list(
    converged     = any(grepl("Convergence was achieved", msgs)),
    years         = num_after("achieved in", msgs),
    distance      = num_after("distance function was:", msgs),
    extinction    = any(grepl("going extinct", warns)),
    erepro_raised = any(grepl("has been increased to the smallest", warns)),
    warnings      = warns,
    error         = if (inherits(out, "error")) conditionMessage(out) else NA_character_
  )
  if (inherits(out, "error")) return(list(params = NULL, diag = diag))
  if (any(!is.finite(species_params(out)$gamma))) {
    diag$error <- "invalid gamma after steady state"
    return(list(params = NULL, diag = diag))
  }
  list(params = out, diag = diag)
}

#' Spin-up stability test (same logic as the original check_biomass_stability_enhanced).
mc_check_stability <- function(sim, s) {
  bm <- getBiomass(sim)
  yrs <- as.numeric(rownames(bm))
  nT <- nrow(bm)
  if (nT < 5) return(list(stable = FALSE, max_cv = NA_real_, fail_cv = character(0),
                          fail_trend = character(0), reason = "too few time steps"))
  tail_mat <- utils::tail(bm, max(5, min(s$stability_cv_years, nT)))
  head_n <- max(5, min(s$stability_trend_years, nT))
  head_mat <- utils::head(bm, head_n)
  head_yrs <- utils::head(yrs, head_n)

  mu <- colMeans(tail_mat, na.rm = TRUE)
  cv <- ifelse(mu > 0, apply(tail_mat, 2, sd, na.rm = TRUE) / mu, Inf)

  rel_slope <- setNames(rep(0, ncol(bm)), colnames(bm))
  p_val <- setNames(rep(1, ncol(bm)), colnames(bm))
  for (j in seq_len(ncol(bm))) {
    y <- head_mat[, j]
    m <- mean(y, na.rm = TRUE)
    if (!is.finite(m) || m < s$stability_min_mean_biomass) next
    fit <- try(suppressWarnings(stats::lm(y ~ head_yrs)), silent = TRUE)
    if (inherits(fit, "try-error")) next
    rel_slope[j] <- stats::coef(fit)[[2]] / m
    pv <- try(summary(fit)$coefficients[2, 4], silent = TRUE)
    p_val[j] <- if (inherits(pv, "try-error")) 1 else as.numeric(pv)
  }
  fail_cv <- cv > s$stability_cv_max
  fail_trend <- abs(rel_slope) > s$stability_trend_rel_slope_max & p_val < s$stability_trend_p
  list(stable = !any(fail_cv | fail_trend, na.rm = TRUE),
       max_cv = suppressWarnings(max(cv, na.rm = TRUE)),
       fail_cv = names(which(fail_cv)), fail_trend = names(which(fail_trend)),
       reason = NA_character_)
}

#' Unfished spin-up cycles with the stability test. Each cycle starts in t_start
#' from the previous cycle's state in year t_start + spinup_years - 1.
mc_spinup <- function(params, s) {
  state_n <- NULL
  stab <- NULL
  cycles_run <- 0
  for (cyc in seq_len(s$spinup_cycles)) {
    # (The original code passed initial_n = NULL on the first cycle, which errors
    #  in mizer 2.4-2.5; the params' own initial state is used instead.)
    if (!is.null(state_n)) initialN(params) <- state_n
    spin <- project(params, t_start = s$t_start, t_max = s$spinup_years, effort = 0,
                    progress_bar = FALSE)
    state_n <- spin@n[s$spinup_years, , ]
    cycles_run <- cyc
    assess <- (cyc == s$spinup_cycles) || (s$spinup_early_exit && cyc >= s$spinup_min_cycles)
    if (assess) {
      stab <- mc_check_stability(spin, s)
      if (!isTRUE(stab$stable)) {
        if (cyc == s$spinup_cycles || !s$spinup_early_exit) {
          return(list(ok = FALSE, state_n = NULL, stability = stab, cycles = cycles_run,
                      message = paste("unstable spin-up:",
                                      paste(unique(c(stab$fail_cv, stab$fail_trend)), collapse = ", "))))
        }
      } else if (s$spinup_early_exit && cyc < s$spinup_cycles) break
    }
  }
  list(ok = TRUE, state_n = state_n, stability = stab, cycles = cycles_run, message = NA_character_)
}

#' Run one ensemble member. Never throws: failures are returned as records with
#' status "failed" and the step at which they failed (errors included).
mc_run_member <- function(i, base_params, effort, s, reference_params = base_params,
                          stage = NA_character_) {
  t0 <- Sys.time()
  rec <- list(member_id = i, stage = stage, seed = s$seed_base + i, status = "failed",
              fail_step = NA_character_, message = NA_character_)
  finish <- function(rec, step = NA_character_, msg = NA_character_) {
    rec$fail_step <- step
    rec$message <- msg
    rec$runtime_s <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    rec
  }
  step <- "sampling"
  tryCatch({
    rec$raw <- mc_draw_multipliers(i, base_params, s)
    ap <- mc_apply_multipliers(base_params, rec$raw, s)
    rec$applied <- ap$applied
    rec$relative <- mc_relative_multipliers(ap$params, reference_params)

    step <- "precheck"
    pc <- mc_precheck(ap$params, effort)
    if (!pc$ok) return(finish(rec, "precheck", pc$reason))

    step <- "steady"
    st <- mc_steady(ap$params, s)
    rec$steady <- st$diag
    if (is.null(st$params)) return(finish(rec, "steady", st$diag$error))
    if (isTRUE(s$reject_unconverged_steady) && !isTRUE(st$diag$converged))
      return(finish(rec, "steady", "steady state did not converge"))
    rec$steady_params <- species_params(st$params)[, c("species", "R_max", "erepro", "gamma")]
    if (max(rec$steady_params$erepro) > s$reject_erepro_above)
      return(finish(rec, "steady", "erepro above limit after steady state"))

    step <- "stability"
    su <- mc_spinup(st$params, s)
    rec$stability <- su$stability[c("stable", "max_cv", "fail_cv", "fail_trend")]
    rec$spinup_cycles <- su$cycles
    if (!su$ok) return(finish(rec, "stability", su$message))

    step <- "historical_run"
    p_run <- st$params
    initialN(p_run) <- su$state_n
    sim <- project(p_run, effort = effort, progress_bar = FALSE)
    if (any(!is.finite(sim@n))) return(finish(rec, "historical_run", "non-finite abundances"))

    rec$biomass <- getBiomass(sim)
    rec$yield <- getYield(sim)
    if (isTRUE(s$keep_sims)) rec$sim <- sim
    rec$status <- "completed"
    finish(rec)
  }, error = function(e) finish(rec, step, conditionMessage(e)))
}


# ---- 5. Running, resuming and loading a stage ------------------------------------

#' Run (or resume) a stage. Members already saved in out_dir are skipped, so the
#' same call continues an interrupted run, and further batches with new member
#' ids can be added to a stage later. Use member ids that do not overlap with
#' other batches or stages: the id sets the random-number stream.
mc_run_stage <- function(base_params, effort, member_ids, settings, out_dir,
                         reference_params = base_params, stage_name = basename(out_dir),
                         n_cores = max(1L, parallel::detectCores() - 1L), verbose = TRUE) {
  s <- settings
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  info_file <- file.path(out_dir, "stage_info.rds")
  if (file.exists(info_file)) {
    info <- readRDS(info_file)
    keys <- setdiff(names(s), c("block_size", "keep_sims"))
    if (!identical(info$settings[keys], s[keys]))
      stop("Settings differ from those already used in ", out_dir, ". Use a new out_dir.")
    same_base <- isTRUE(all.equal(species_params(info$base_params)$gamma, species_params(base_params)$gamma)) &&
      isTRUE(all.equal(gear_params(info$base_params)$catchability, gear_params(base_params)$catchability)) &&
      isTRUE(all.equal(initialN(info$base_params), initialN(base_params)))
    if (!same_base)
      stop("The base model differs from the one already used in ", out_dir,
           " (e.g. a changed stage-1 centre). Use a new out_dir.")
    info$member_ids <- sort(union(info$member_ids, member_ids))
  } else {
    info <- list(stage = stage_name, settings = s, member_ids = sort(member_ids),
                 base_params = base_params, reference_params = reference_params,
                 effort_years = range(as.numeric(dimnames(effort)[[1]])), created = Sys.time(),
                 versions = c(R = R.version.string,
                              mizer = format(utils::packageVersion("mizer")),
                              therMizer = format(utils::packageVersion("therMizer"))))
  }
  saveRDS(info, info_file)

  index_file <- file.path(out_dir, "member_index.csv")
  done <- if (file.exists(index_file)) utils::read.csv(index_file)$member_id else integer(0)
  todo <- setdiff(member_ids, done)
  if (!length(todo)) {
    if (verbose) message("All requested members already saved in ", out_dir)
    return(invisible(mc_load_stage(out_dir)))
  }

  n_cores <- max(1L, as.integer(n_cores))
  if (n_cores > 1) {
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, suppressPackageStartupMessages({ library(mizer); library(therMizer) }))
    fun_env <- environment(mc_run_member)
    fun_names <- Filter(function(n) is.function(get(n, envir = fun_env)), ls(fun_env, pattern = "^mc_"))
    parallel::clusterExport(cl, fun_names, envir = fun_env)
    .mc_job <- list(base = base_params, effort = effort, s = s, ref = reference_params, stage = stage_name)
    parallel::clusterExport(cl, ".mc_job", envir = environment())
    task <- function(i) mc_run_member(i, .mc_job$base, .mc_job$effort, .mc_job$s, .mc_job$ref, .mc_job$stage)
    environment(task) <- globalenv()  # do not ship this function's frame to the workers
  }

  bs <- if (is.null(s$block_size)) 4 * n_cores else s$block_size
  blocks <- split(todo, ceiling(seq_along(todo) / bs))
  t0 <- Sys.time()
  n_run <- 0
  n_ok <- 0
  for (b in seq_along(blocks)) {
    ids <- blocks[[b]]
    res <- if (n_cores > 1) parallel::parLapplyLB(cl, ids, task) else
      lapply(ids, mc_run_member, base_params = base_params, effort = effort, s = s,
             reference_params = reference_params, stage = stage_name)
    f <- file.path(out_dir, sprintf("block_%07d-%07d.rds", min(ids), max(ids)))
    if (file.exists(f)) f <- sub("\\.rds$", paste0("_", format(Sys.time(), "%Y%m%d%H%M%S"), ".rds"), f)
    saveRDS(res, f)
    idx <- data.frame(member_id = vapply(res, `[[`, 0, "member_id"),
                      status = vapply(res, `[[`, "", "status"),
                      fail_step = vapply(res, function(r) as.character(r$fail_step), ""),
                      block_file = basename(f))
    utils::write.table(idx, index_file, sep = ",", row.names = FALSE,
                       col.names = !file.exists(index_file), append = file.exists(index_file))
    n_run <- n_run + length(ids)
    n_ok <- n_ok + sum(idx$status == "completed")
    if (verbose) {
      el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
      message(sprintf("[%s] %d/%d members run (%d completed) | %.1f min elapsed, ~%.1f min left",
                      stage_name, n_run, length(todo), n_ok, el, el / n_run * (length(todo) - n_run)))
    }
  }
  invisible(mc_load_stage(out_dir))
}

#' Load all saved members of a stage (any number of batches).
mc_load_stage <- function(out_dir) {
  info_file <- file.path(out_dir, "stage_info.rds")
  if (!file.exists(info_file)) stop("No stage_info.rds in ", out_dir)
  files <- sort(list.files(out_dir, pattern = "^block_.*\\.rds$", full.names = TRUE))
  recs <- unlist(lapply(files, readRDS), recursive = FALSE)
  if (length(recs)) {
    ids <- vapply(recs, `[[`, 0, "member_id")
    keep <- !duplicated(ids, fromLast = TRUE)  # a re-run member replaces the earlier record
    recs <- recs[keep][order(ids[keep])]
    names(recs) <- as.character(sort(ids[keep]))
  }
  structure(list(info = readRDS(info_file), records = recs, dir = out_dir), class = "mc_stage")
}


# ---- 6. Tables ----------------------------------------------------------------

#' One row per member: outcome and diagnostics.
mc_members <- function(stage) {
  recs <- stage$records
  get1 <- function(r, path, default) {
    v <- r
    for (p in path) { if (is.null(v[[p]])) return(default); v <- v[[p]] }
    if (length(v) != 1 || is.na(v)) default else v
  }
  data.frame(
    member_id       = vapply(recs, function(r) r$member_id, 0),
    status          = vapply(recs, function(r) r$status, ""),
    fail_step       = vapply(recs, function(r) as.character(r$fail_step), ""),
    message         = vapply(recs, function(r) as.character(r$message), ""),
    steady_converged = vapply(recs, get1, NA, path = c("steady", "converged"), default = NA),
    steady_distance = vapply(recs, get1, 0, path = c("steady", "distance"), default = NA_real_),
    steady_extinction = vapply(recs, get1, NA, path = c("steady", "extinction"), default = NA),
    erepro_raised   = vapply(recs, get1, NA, path = c("steady", "erepro_raised"), default = NA),
    max_erepro      = vapply(recs, function(r) if (is.null(r$steady_params)) NA_real_ else
                               max(r$steady_params$erepro), 0),
    spinup_cycles   = vapply(recs, get1, 0, path = "spinup_cycles", default = NA_real_),
    spinup_max_cv   = vapply(recs, get1, 0, path = c("stability", "max_cv"), default = NA_real_),
    runtime_s       = vapply(recs, get1, 0, path = "runtime_s", default = NA_real_),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

#' Long table of multipliers. scale: "stage" (applied, vs the stage base),
#' "reference" (applied, vs the reference model) or "raw" (the draw).
mc_multipliers <- function(stage, scale = c("stage", "reference", "raw"), member_ids = NULL) {
  scale <- match.arg(scale)
  field <- c(stage = "applied", reference = "relative", raw = "raw")[[scale]]
  recs <- stage$records
  if (!is.null(member_ids)) recs <- recs[as.character(member_ids)]
  rows <- lapply(recs, function(r) {
    m <- r[[field]]
    if (is.null(m)) return(NULL)
    do.call(rbind, lapply(names(m), function(par)
      data.frame(member_id = r$member_id, parameter = par, group = names(m[[par]]),
                 value = unname(m[[par]]), stringsAsFactors = FALSE)))
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Biomass or yield of completed members as an array [year, group, member].
mc_timeseries <- function(stage, what = c("biomass", "yield"), member_ids = NULL) {
  what <- match.arg(what)
  recs <- stage$records
  if (!is.null(member_ids)) recs <- recs[as.character(member_ids)]
  recs <- Filter(function(r) identical(r$status, "completed"), recs)
  if (!length(recs)) stop("No completed members to extract")
  m1 <- recs[[1]][[what]]
  arr <- array(unlist(lapply(recs, `[[`, what)), dim = c(dim(m1), length(recs)),
               dimnames = list(year = rownames(m1), group = colnames(m1),
                               member = vapply(recs, function(r) as.character(r$member_id), "")))
  arr
}


# ---- 7. Acceptance --------------------------------------------------------------

#' Biomass acceptance test: mean biomass over `years` within bounds for all but
#' at most `max_outside` groups. Groups without bounds are ignored.
mc_accept <- function(stage, bounds, years = stage$info$settings$accept_years,
                      max_outside = stage$info$settings$accept_max_outside) {
  mem <- mc_members(stage)
  ok_ids <- mem$member_id[mem$status == "completed"]
  if (!length(ok_ids)) stop("No completed members")
  arr <- mc_timeseries(stage, "biomass", ok_ids)
  sel <- as.numeric(dimnames(arr)[[1]]) %in% years
  mean_b <- apply(arr[sel, , , drop = FALSE], c(2, 3), mean)
  i <- match(rownames(mean_b), bounds$Species)
  has_bounds <- !is.na(i)
  lo <- bounds$Lower_g[i]
  hi <- bounds$Upper_g[i]
  within <- mean_b >= lo & mean_b <= hi
  within[is.na(within)] <- FALSE
  within[!has_bounds, ] <- TRUE
  n_out <- colSums(!within)
  list(members = data.frame(member_id = ok_ids, n_outside = unname(n_out),
                            accepted = unname(n_out <= max_outside)),
       accepted_ids = ok_ids[n_out <= max_outside],
       within = within, mean_biomass = mean_b, years = years, max_outside = max_outside)
}

#' How often each group falls outside its bounds (all completed vs accepted members).
mc_outside_by_group <- function(acc) {
  w <- acc$within
  data.frame(group = rownames(w),
             outside_completed = rowSums(!w),
             outside_accepted = rowSums(!w[, acc$members$accepted, drop = FALSE]),
             row.names = NULL)
}


# ---- 8. Centre and parameter objects ----------------------------------------------

#' Centre of the accepted multipliers. stat = "mean" reproduces the original
#' analysis (arithmetic mean of multipliers); "geometric_mean" or "median" are
#' the natural centres for log-normal multipliers.
mc_centre <- function(stage, accepted_ids, stat = c("mean", "geometric_mean", "median"),
                      scale = c("reference", "stage")) {
  stat <- match.arg(stat)
  scale <- match.arg(scale)
  if (!length(accepted_ids)) stop("No accepted members")
  f <- switch(stat, mean = mean, median = stats::median,
              geometric_mean = function(x) exp(mean(log(x))))
  ml <- mc_multipliers(stage, scale = scale, member_ids = accepted_ids)
  ctr <- tapply(ml$value, list(ml$group, ml$parameter), f)
  pick <- function(par) {
    v <- ml[ml$parameter == par, ]
    grp <- unique(v$group)
    setNames(ctr[grp, par], grp)
  }
  list(catchability = pick("catchability"), gamma = pick("gamma"), abundance = pick("abundance"),
       stat = stat, scale = scale, n_accepted = length(accepted_ids))
}

#' Centre as one row per group (the format of the original seed_params_*.csv).
mc_centre_table <- function(centre, reference_params) {
  sp <- species_params(reference_params)$species
  gp <- gear_params(reference_params)
  q <- centre$catchability[match(sp, gp$species[match(names(centre$catchability), gp$gear)])]
  data.frame(species = sp,
             gamma_change = unname(centre$gamma[sp]),
             catchability_change = unname(q),
             abundance_scaling = unname(centre$abundance[sp]),
             row.names = NULL)
}

#' Apply a centre to the reference model: gamma and initial abundance multiplied,
#' catchability multiplied and limited to [0, 1]. No steady() is run, as in the
#' original construction of params_mean_from_rerun_results.rds.
mc_apply_centre <- function(reference_params, centre) {
  if (!identical(centre$scale, "reference"))
    warning("Centre is not on the reference scale; results are relative to that stage's base")
  p <- reference_params
  gp <- gear_params(p)
  m <- centre$catchability[gp$gear]
  m[is.na(m)] <- 1
  gp$catchability <- pmin(1, pmax(0, gp$catchability * m))
  gear_params(p) <- gp
  sp <- species_params(p)
  sp$gamma <- sp$gamma * centre$gamma[sp$species]
  species_params(p) <- sp
  initialN(p) <- sweep(initialN(p), 1, centre$abundance[sp$species], "*")
  p
}

#' Rebuild any member's params exactly (draw + apply, optionally + steady()).
mc_rebuild_member <- function(stage, member_id, run_steady = TRUE) {
  s <- stage$info$settings
  base <- stage$info$base_params
  p <- mc_apply_multipliers(base, mc_draw_multipliers(member_id, base, s), s)$params
  if (!run_steady) return(p)
  mc_steady(p, s)$params
}


# ---- 9. Summaries and figures ------------------------------------------------------

#' Counts at each step of the workflow.
mc_funnel <- function(stage, acc = NULL) {
  mem <- mc_members(stage)
  step_n <- function(x) sum(mem$fail_step == x, na.rm = TRUE)
  out <- data.frame(
    step = c("Members run", "Failed: pre-check", "Failed: steady state",
             "Failed: spin-up (stability or error)", "Failed: historical run or sampling",
             "Completed", "Accepted (biomass test)"),
    n = c(nrow(mem), step_n("precheck"), step_n("steady"), step_n("stability"),
          step_n("historical_run") + step_n("sampling"), sum(mem$status == "completed"),
          if (is.null(acc)) NA else sum(acc$members$accepted)))
  out$percent_of_run <- round(100 * out$n / nrow(mem), 1)
  out
}

#' Prior (all members run) vs accepted summaries of each multiplier, with the
#' share of prior draws held at a bound.
mc_multiplier_summary <- function(stage, accepted_ids, scale = c("stage", "reference")) {
  scale <- match.arg(scale)
  prior <- mc_multipliers(stage, scale)
  if (scale == "stage") {
    raw <- mc_multipliers(stage, "raw")
    names(raw)[names(raw) == "value"] <- "raw_value"
    prior <- merge(prior, raw, by = c("member_id", "parameter", "group"), all.x = TRUE)
    prior$at_bound <- abs(prior$value - prior$raw_value) > 1e-9 * prior$raw_value
  } else prior$at_bound <- NA
  acc <- prior[prior$member_id %in% accepted_ids, ]
  keys <- unique(prior[, c("parameter", "group")])
  q <- function(x, p) if (length(x)) unname(stats::quantile(x, p)) else NA_real_
  rows <- lapply(seq_len(nrow(keys)), function(k) {
    a <- prior$value[prior$parameter == keys$parameter[k] & prior$group == keys$group[k]]
    ab <- prior$at_bound[prior$parameter == keys$parameter[k] & prior$group == keys$group[k]]
    b <- acc$value[acc$parameter == keys$parameter[k] & acc$group == keys$group[k]]
    data.frame(parameter = keys$parameter[k], group = keys$group[k],
               prior_median = q(a, 0.5), prior_q05 = q(a, 0.05), prior_q95 = q(a, 0.95),
               prior_share_at_bound = mean(ab),
               n_accepted = length(b), accepted_median = q(b, 0.5),
               accepted_q05 = q(b, 0.05), accepted_q95 = q(b, 0.95),
               accepted_geomean = if (length(b)) exp(mean(log(b))) else NA_real_,
               accepted_mean = if (length(b)) mean(b) else NA_real_)
  })
  do.call(rbind, rows)
}

#' Histogram of one multiplier, prior (all members run) vs accepted, log10 axis.
mc_plot_multipliers <- function(stage, accepted_ids, parameter = c("gamma", "abundance", "catchability"),
                                scale = c("stage", "reference"), group_order = NULL) {
  parameter <- match.arg(parameter)
  scale <- match.arg(scale)
  d <- mc_multipliers(stage, scale)
  d <- d[d$parameter == parameter & is.finite(d$value) & d$value > 0, ]
  d <- rbind(transform(d, set = "All members run"),
             transform(d[d$member_id %in% accepted_ids, ], set = "Accepted"))
  d$set <- factor(d$set, levels = c("All members run", "Accepted"))
  if (!is.null(group_order)) d$group <- factor(d$group, levels = intersect(group_order, unique(d$group)))
  ggplot2::ggplot(d, ggplot2::aes(x = value, fill = set)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 40,
                            position = "identity", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed") +
    ggplot2::scale_x_log10() +
    ggplot2::facet_wrap(~group, scales = "free_y") +
    ggplot2::scale_fill_manual(values = c("All members run" = "grey60", "Accepted" = "steelblue")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom", strip.text = ggplot2::element_text(size = 7, face = "bold")) +
    ggplot2::labs(x = paste0(parameter, " multiplier (", scale, " scale, log10)"),
                  y = "Density", fill = NULL,
                  title = paste0(stage$info$stage, ": ", parameter, " multipliers"))
}

#' Quantile envelope of an array [year, group, member] as a long data frame.
mc_envelope <- function(arr, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  qs <- apply(arr, c(1, 2), stats::quantile, probs = probs, na.rm = TRUE)
  out <- expand.grid(Year = as.numeric(dimnames(arr)[[1]]), Species = dimnames(arr)[[2]],
                     stringsAsFactors = FALSE)
  for (k in seq_along(probs)) out[[sprintf("q%02d", round(100 * probs[k]))]] <- as.vector(qs[k, , ])
  out
}

#' Biomass envelopes (median, 50% and 90% ranges) of the given members with the
#' observed biomass and its acceptance bounds over the acceptance window.
mc_plot_biomass <- function(stage, member_ids, bounds, years = stage$info$settings$accept_years,
                            species_order = NULL, title = NULL) {
  env <- mc_envelope(mc_timeseries(stage, "biomass", member_ids))
  obs <- merge(bounds, data.frame(Year = years), by = NULL)
  if (!is.null(species_order)) {
    env$Species <- factor(env$Species, levels = species_order)
    obs$Species <- factor(obs$Species, levels = species_order)
  }
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = env, ggplot2::aes(x = Year, ymin = q05 / 1e6, ymax = q95 / 1e6), fill = "steelblue", alpha = 0.2) +
    ggplot2::geom_ribbon(data = env, ggplot2::aes(x = Year, ymin = q25 / 1e6, ymax = q75 / 1e6), fill = "steelblue", alpha = 0.35) +
    ggplot2::geom_line(data = env, ggplot2::aes(x = Year, y = q50 / 1e6), colour = "navy", linewidth = 0.7) +
    ggplot2::geom_ribbon(data = obs, ggplot2::aes(x = Year, ymin = Lower_g / 1e6, ymax = Upper_g / 1e6), fill = "grey40", alpha = 0.3) +
    ggplot2::geom_point(data = obs, ggplot2::aes(x = Year, y = ObsBiomass / 1e6), shape = 21, size = 0.8, fill = "red") +
    ggplot2::facet_wrap(~Species, scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 7, face = "bold")) +
    ggplot2::labs(x = "Year", y = "Biomass (t)",
                  title = if (is.null(title)) paste0(stage$info$stage, ": biomass of ", length(member_ids), " members") else title,
                  subtitle = "Median, 50% and 90% ranges; grey band and red points: observed bounds and mean")
}

#' Yield envelopes of the given members (log10 axis) with observed catches.
#' @param obs_yield data.frame(Year, Species, Yield) in grams per year.
mc_plot_yield <- function(stage, member_ids, obs_yield, species_order = NULL,
                          from_year = NULL, ref_years = c(1961, 2010), title = NULL) {
  env <- mc_envelope(mc_timeseries(stage, "yield", member_ids))
  fished <- unique(env$Species[which(env$q95 > 0)])
  env <- env[env$Species %in% fished, ]
  for (k in c("q05", "q25", "q50", "q75", "q95")) env[[k]][env[[k]] <= 0] <- NA
  obs <- obs_yield[obs_yield$Species %in% fished & obs_yield$Yield > 0, ]
  if (is.null(from_year)) from_year <- min(obs$Year, na.rm = TRUE)
  if (!is.null(species_order)) {
    env$Species <- factor(env$Species, levels = intersect(species_order, fished))
    obs$Species <- factor(obs$Species, levels = intersect(species_order, fished))
  }
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = env, ggplot2::aes(x = Year, ymin = q05 / 1e6, ymax = q95 / 1e6), fill = "darkorange", alpha = 0.2, na.rm = TRUE) +
    ggplot2::geom_ribbon(data = env, ggplot2::aes(x = Year, ymin = q25 / 1e6, ymax = q75 / 1e6), fill = "darkorange", alpha = 0.35, na.rm = TRUE) +
    ggplot2::geom_line(data = env[!is.na(env$q50), ], ggplot2::aes(x = Year, y = q50 / 1e6), colour = "darkorange4", linewidth = 0.7) +
    ggplot2::geom_point(data = obs, ggplot2::aes(x = Year, y = Yield / 1e6), shape = 21, size = 1, fill = "black") +
    ggplot2::geom_vline(xintercept = ref_years, linetype = "dashed") +
    ggplot2::scale_y_log10() +
    ggplot2::coord_cartesian(xlim = c(from_year, max(env$Year))) +
    ggplot2::facet_wrap(~Species, scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 7, face = "bold")) +
    ggplot2::labs(x = "Year", y = "Yield (t per year, log10)",
                  title = if (is.null(title)) paste0(stage$info$stage, ": yield of ", length(member_ids), " members") else title,
                  subtitle = "Median, 50% and 90% ranges; points: observed catches")
}
