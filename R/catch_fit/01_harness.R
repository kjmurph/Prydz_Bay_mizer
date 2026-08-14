# =============================================================================
# CATCH FITTING -- shared harness.  SOURCE this; it is not a runnable analysis.
#
# WHY THIS IS CHEAP.  `initial_effort` is 0 for all 19 gears, so steady() and the
# spin-up are UNFISHED and catchability cannot touch the initial condition.  It
# enters only the 1841-2010 projection.  `sim@params@initial_n` is bit-identical
# to `sim@n[1,,]`, so a stored member can be re-projected exactly:
# verified 4.6e-15 relative on the yield.  Nothing here calls steady(), so the
# "a re-run cannot reproduce the stored ensemble" finding does not apply, the
# control is exact, and the known bistability of members 446/1512 (a steady()
# convergence-path artefact) is irrelevant.
#
# THE OBJECTIVE IS THE EXISTING ONE, unchanged: RMSE of log10(g+1) modelled vs
# observed yield, per species only within that species' own effort window,
# 344 rows.  Defined at yield_rmse_evaluation.R:112-140.  Do not substitute.
#
# 1929 CACHE.  The earliest effort year across all gears is 1930, so 1841-1929 is
# catchability-independent and is computed once per member.  Guarded: gate 4 in
# validate_harness() compares it against the full projection and the cache is
# refused unless it is exact.
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer)   # MUST be attached: getYield() routes through
  library(mizer)       # projectRateFunctions, which reads params@rates_funcs
})

# ---------------------------------------------------------------- paths ------
CF_ENSEMBLE  <- "Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds"
CF_RANKING   <- "Manuscript data/yield_rmse_per_sim_deduped.csv"
CF_OBSERVED  <- "yield_observed_timeseries.csv"
CF_EFFORT    <- "effort_array_1841_2010.rds"
CF_OUT       <- "Output_large_files/catch_fit"

# MEMBER SELECTION -- two defensible readings, both run.
#
# PRIMARY is the top 5 of the agreed filter from 39_full_ensemble_paired.R
# (trt_ok, trt_pass, no erepro >= 1, no R_max = Inf, penguin recalibration
# feasible): deduped ranks 1, 2, 5, 6, 8.  Ranks 3, 4, 7 carry R_max = Inf and
# ranks 10, 11 are penguin-infeasible.
#
# But that filter measures `out@species_params` -- the state AFTER
# steady(preserve="erepro"), which ends by calling setBevertonHolt() and
# re-derives R_max.  steady() removes most inherited infinities, so a member can
# pass the filter while its STORED params carry R_max = Inf.  1776 (mesozoo-
# plankton) and 173 (other macrozooplankton) both do.  This is the pre-existing
# low-trophic degeneracy noted in docs/small_divers_wmin_SYNTHESIS.md section 2,
# not something introduced here -- the brief's rule is "do not add to it".
#
# STRICT is the top 5 that are clean on BOTH the filter and their stored params.
# Every conclusion is checked against both sets; the union is what actually runs.
CF_MEMBERS_PRIMARY <- c(446L, 1512L, 1776L, 173L, 71L)   # ranks 1, 2, 5, 6, 8
CF_MEMBERS_STRICT  <- c(446L, 1512L, 71L, 1806L, 384L)   # ranks 1, 2, 8, 9, 16
CF_MEMBERS <- unique(c(CF_MEMBERS_PRIMARY, CF_MEMBERS_STRICT))

# ARMS.  Every stage runs against one arm; the arm decides where the starting
# state comes from and where results are written.  See 05_wmin_arms.R.
#   A  the member's STORED post-spin-up state, w_min uncorrected.  The headline
#      arm, and the only one whose control is bit-exact against stored values.
#   B  the same member re-run through steady() + spin-up, still uncorrected.
#   C  the same member with the `small divers` w_min correction, re-run.
# C is NOT comparable with A -- re-entering the pipeline reorders the RMSE
# ranking at rho 0.60 on its own, ~5,500x the size of the w_min effect.  The
# w_min sensitivity of the catchability optimum is optimum(C) - optimum(B).
CF_ARM <- toupper(Sys.getenv("CF_ARM", "A"))
if (!CF_ARM %in% c("A", "B", "C")) stop("CF_ARM must be A, B or C")

# Projection geometry.  t_max = 169 gives 170 rows, 1841-2010.  t_max = 170 runs
# to 2011 and must not be used: the exploited arm ends 2010.
CF_T_START   <- 1841L
CF_T_MAX     <- 169L
CF_LAST_YEAR <- 2010L

# First year any gear has non-zero effort.  Asserted, not assumed, in cf_effort().
CF_FIRST_FISHED_YEAR <- 1930L

# ---------------------------------------------------------------- effort -----
cf_effort <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      eff <- readRDS(CF_EFFORT)
      yrs <- as.numeric(rownames(eff))
      first <- min(yrs[apply(eff > 0, 1, any)])
      if (first != CF_FIRST_FISHED_YEAR)
        stop("first fished year is ", first, ", not ", CF_FIRST_FISHED_YEAR,
             " -- the 1929 cache assumption is void")
      cache <<- eff
    }
    cache
  }
})

#' Per-species effort windows -- the comparison window, exactly as
#' yield_rmse_evaluation.R:62-67 builds it.
cf_effort_windows <- function() {
  eff <- cf_effort()
  yrs <- as.numeric(rownames(eff))
  do.call(rbind, lapply(colnames(eff), function(sp) {
    y <- yrs[eff[, sp] > 0]
    if (length(y) > 0)
      data.frame(Species = sp, first_year = min(y), last_year = max(y),
                 stringsAsFactors = FALSE)
  }))
}

#' Gears with non-zero catchability in the baseline -- the levers.
cf_fished_species <- function() cf_effort_windows()$Species

# ------------------------------------------------------- comparison rows -----
#' The 344 observed rows.  Same construction as yield_rmse_evaluation.R:75-83:
#' melt observed, coerce negatives to 0, keep only years inside each species'
#' own effort window.  Years with zero effort INSIDE the window are kept -- the
#' window is [first, last], not the set of effort-on years.
cf_comparison_rows <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    win <- cf_effort_windows()
    obs <- read.csv(CF_OBSERVED, check.names = FALSE)
    names(obs) <- gsub("\\.", " ", names(obs))
    out <- do.call(rbind, lapply(seq_len(nrow(win)), function(k) {
      sp <- win$Species[k]
      yy <- obs$Year[obs$Year >= win$first_year[k] & obs$Year <= win$last_year[k]]
      data.frame(Year = yy, Species = sp,
                 Yield_obs = pmax(as.numeric(obs[[sp]][match(yy, obs$Year)]), 0),
                 stringsAsFactors = FALSE)
    }))
    out$Yield_obs[is.na(out$Yield_obs)] <- 0
    if (nrow(out) != 344L)
      stop("expected 344 comparison rows, got ", nrow(out))
    cache <<- out
    out
  }
})

# ------------------------------------------------------------- scoring -------
#' Score a yield matrix (years x species, grams) against the observed rows.
#'
#' @return list(rmse, per_species = data.frame(Species, n, rmse, sse, sse_pct,
#'         mod_cum, obs_cum, mod_obs_ratio))
cf_score_yield_matrix <- function(ymat) {
  rows <- cf_comparison_rows()
  yrs  <- as.numeric(rownames(ymat))
  ri <- match(rows$Year, yrs)
  ci <- match(rows$Species, colnames(ymat))
  if (anyNA(ci)) stop("yield matrix is missing species: ",
                      paste(unique(rows$Species[is.na(ci)]), collapse = ", "))
  mod <- ifelse(is.na(ri), 0, pmax(ymat[cbind(ri, ci)], 0))
  e2  <- (log10(mod + 1) - log10(rows$Yield_obs + 1))^2

  sse <- tapply(e2, rows$Species, sum)
  n   <- tapply(e2, rows$Species, length)
  mc  <- tapply(mod, rows$Species, sum)
  oc  <- tapply(rows$Yield_obs, rows$Species, sum)
  per <- data.frame(
    Species  = names(sse),
    n        = as.integer(n),
    rmse     = as.numeric(sqrt(sse / n)),
    sse      = as.numeric(sse),
    sse_pct  = as.numeric(100 * sse / sum(sse)),
    mod_cum  = as.numeric(mc),
    obs_cum  = as.numeric(oc),
    stringsAsFactors = FALSE
  )
  per$mod_obs_ratio <- ifelse(per$obs_cum > 0, per$mod_cum / per$obs_cum, NA_real_)
  list(rmse = sqrt(mean(e2)), per_species = per)
}

#' Score a MizerSim.  Truncates to <= 2010 before scoring.
cf_score_sim <- function(sim) {
  y <- getYield(sim)
  y <- y[as.numeric(rownames(y)) <= CF_LAST_YEAR, , drop = FALSE]
  cf_score_yield_matrix(y)
}

# ------------------------------------------------------------- members -------
#' Load a member's MizerParams by its ensemble sim_index.
#'
#' The stored objects were written by mizer 2.5.0 and lack @second_order_w, which
#' getYield() dereferences under 3.1.0.  validParams() is mizer's own sanctioned
#' upgrade (bit-identical here; the recomputed RMSE table reproduces the stored
#' one to 4.9e-15).  Do NOT downgrade mizer -- that breaks therMizer 1.0.0.
#' Arms B and C carry no stored sim -- their starting state was rebuilt by
#' 05_wmin_arms.R, so `stored_sim` is NULL and the gates that compare against
#' stored values do not apply to them.
cf_load_members <- function(sim_indices = CF_MEMBERS, arm = CF_ARM) {
  if (arm != "A") {
    d <- file.path(CF_OUT, "wmin_arms")
    out <- lapply(sim_indices, function(si) {
      f <- file.path(d, sprintf("params_%s_%04d.rds", arm, si))
      if (!file.exists(f))
        stop("arm ", arm, " params missing for member ", si,
             " -- run 05_wmin_arms.R first (", f, ")")
      z <- readRDS(f)
      list(sim_index = si, params = z$params, stored_sim = NULL, arm_meta = z$meta)
    })
    names(out) <- as.character(sim_indices)
    return(out)
  }
  x   <- readRDS(CF_ENSEMBLE)
  idx <- x$metadata$top10pct_indices
  pos <- match(sim_indices, idx)
  if (anyNA(pos))
    stop("members not in the top-10% ensemble file: ",
         paste(sim_indices[is.na(pos)], collapse = ", "))
  out <- lapply(seq_along(pos), function(k) {
    s <- x$fished_top10pct[[pos[k]]]
    s@params <- suppressWarnings(validParams(s@params))
    list(sim_index = sim_indices[k], params = s@params, stored_sim = s)
  })
  names(out) <- as.character(sim_indices)
  out
}

# ------------------------------------------------- catchability evaluation ---
#' Set catchability for named gears, preserving everything else.
#'
#' Verified: `gear_params<-` preserves @rates_funcs (therMizer entries intact),
#' @initial_n, @resource_dynamics and @other_dynamics, and a null treatment
#' through this path reproduces the stored yield to 4.6e-15.
#'
#' @param q named numeric vector, names = species.  Values are absolute
#'   catchabilities, not multipliers.
#' @param cap upper bound.  The pipeline clamps to [0,1]
#'   (09_Uncertainty_Analysis.Rmd:351; precheck rejects q > 1).  Pass Inf only
#'   for the explicitly flagged cap diagnostic.
cf_set_q <- function(params, q, cap = 1) {
  g <- params@gear_params
  i <- match(names(q), as.character(g$species))
  if (anyNA(i)) stop("unknown gear species: ",
                     paste(names(q)[is.na(i)], collapse = ", "))
  vals <- pmax(0, q)
  if (is.finite(cap)) vals <- pmin(cap, vals)
  g$catchability[i] <- vals
  gear_params(params) <- g
  params
}

cf_get_q <- function(params) {
  setNames(params@gear_params$catchability, as.character(params@gear_params$species))
}

#' Cache the catchability-independent 1841-1929 segment for a member.
#'
#' Returns NULL when disabled, in which case cf_project() runs the full 1841-2010
#' projection.  Never construct this without passing gate 4 of validate_harness().
cf_make_cache <- function(params) {
  if (length(params@other_dynamics) > 0)
    stop("model has dynamic `other` components; the 1929 cache does not carry them")
  n_pre <- CF_FIRST_FISHED_YEAR - 1L - CF_T_START      # 1841 -> 1929 = 88 steps
  s <- project(params, t_start = CF_T_START, t_max = n_pre, effort = 0)
  list(t_start = CF_FIRST_FISHED_YEAR - 1L,
       initial_n = s@n[dim(s@n)[1], , ],
       initial_n_pp = s@n_pp[dim(s@n_pp)[1], ],
       pre_yield = { y <- getYield(s); y[as.numeric(rownames(y)) < CF_FIRST_FISHED_YEAR, ,
                                         drop = FALSE] })
}

#' Project a member 1841-2010 (or 1929-2010 from a cache) and return the yield
#' matrix restricted to <= 2010.
cf_project_yield <- function(params, cache = NULL) {
  eff <- cf_effort()
  if (is.null(cache)) {
    s <- project(params, t_start = CF_T_START, t_max = CF_T_MAX, effort = eff)
    y <- getYield(s)
    return(list(yield = y[as.numeric(rownames(y)) <= CF_LAST_YEAR, , drop = FALSE],
                sim = s))
  }
  eff2 <- eff[as.numeric(rownames(eff)) >= cache$t_start, , drop = FALSE]
  s <- project(params, t_start = cache$t_start,
               t_max = CF_LAST_YEAR - cache$t_start,
               effort = eff2, initial_n = cache$initial_n,
               initial_n_pp = cache$initial_n_pp)
  y <- getYield(s)
  y <- y[as.numeric(rownames(y)) <= CF_LAST_YEAR, , drop = FALSE]
  list(yield = rbind(cache$pre_yield, y[rownames(y) != as.character(cache$t_start), ,
                                        drop = FALSE]),
       sim = s)
}

#' One objective evaluation.  Returns the pooled RMSE, the per-species table, and
#' the end/start biomass ratio per species so a collapse is never mistaken for an
#' improvement.
cf_eval_q <- function(params, q, cache = NULL, cap = 1) {
  p  <- cf_set_q(params, q, cap = cap)
  pr <- cf_project_yield(p, cache)
  sc <- cf_score_yield_matrix(pr$yield)
  bm <- getBiomass(pr$sim)
  sc$biomass_ratio <- setNames(bm[nrow(bm), ] / bm[1, ], colnames(bm))
  sc$q <- cf_get_q(p)
  sc
}

# --------------------------------------------------------- hard constraints --
#' The constraint violations a member ALREADY carries in its stored params.
#' Pre-existing R_max = Inf in unfished low-trophic groups is a documented,
#' pre-existing degeneracy of the ensemble; the brief's rule is "do not add to
#' it", so it is recorded and carried, not treated as this work's error.
cf_inherited_flags <- function(params) {
  sp <- params@species_params
  list(rmax_inf = sp$species[!is.finite(sp$R_max)],
       erepro_ge1 = sp$species[sp$erepro >= 1])
}

#' Assert the constraints that are physics, not preferences.  Checks all 19
#' groups every time -- orca sits at erepro 0.9509 and breaches first.
#'
#' @param inherited output of cf_inherited_flags() on the member's untreated
#'   params.  Violations present there are tolerated; anything NEW is fatal.
#'   Leave NULL to demand an unconditionally clean object.
cf_assert_admissible <- function(params, inherited = NULL,
                                 allow_q_above_1 = FALSE, label = "") {
  sp <- params@species_params
  old_inf <- if (is.null(inherited)) character(0) else inherited$rmax_inf
  old_er  <- if (is.null(inherited)) character(0) else inherited$erepro_ge1

  bad <- setdiff(sp$species[sp$erepro >= 1], old_er)
  if (length(bad))
    stop(label, ": erepro >= 1 for ", paste(bad, collapse = ", "))

  new_inf <- setdiff(sp$species[!is.finite(sp$R_max)], old_inf)
  if (length(new_inf))
    stop(label, ": R_max made infinite for ", paste(new_inf, collapse = ", "))

  # a FISHED group with no density dependence is never acceptable, inherited or
  # not -- it would let catchability draw on an unbounded recruitment supply
  fished_inf <- intersect(sp$species[!is.finite(sp$R_max)], cf_fished_species())
  if (length(fished_inf))
    stop(label, ": R_max not finite for FISHED group(s) ",
         paste(fished_inf, collapse = ", "))

  q <- cf_get_q(params)
  if (any(q < 0)) stop(label, ": negative catchability")
  if (!allow_q_above_1 && any(q > 1))
    stop(label, ": catchability > 1 for ", paste(names(q)[q > 1], collapse = ", "))
  invisible(TRUE)
}

# ------------------------------------------------------ reproduction levers --
# Both levers write ONE row of species_params and leave the other 18 groups
# bit-identical.  Never route this through setBevertonHolt() with a full-length
# `reproduction_level` vector: feeding the other groups their own unchanged
# levels back perturbs them ~2% and pushes orca from erepro 0.9509 to 1.0104,
# breaching erepro < 1 -- because initial_n is the post-spin-up state, not the
# state the stored R_max and erepro were derived at.

#' Lever A -- multiply R_max for one group, `erepro` untouched.
#' Raises RDD, hence the recruitment ceiling AND the standing population, so the
#' unfished trajectory moves too and needs a paired unfished control.
cf_scale_rmax <- function(p, sp, k) {
  i <- match(sp, p@species_params$species)
  if (is.na(i)) stop("unknown species: ", sp)
  p@species_params$R_max[i] <- p@species_params$R_max[i] * k
  p
}

#' Lever B -- set the reproduction level for one group at fixed RDD.
#' R_max rises, erepro falls, the state at initial_n is unchanged; only the
#' strength of compensation under depletion changes.
cf_set_repro_level <- function(p, sp, rl) {
  i <- match(sp, p@species_params$species)
  if (is.na(i)) stop("unknown species: ", sp)
  if (rl <= 0 || rl >= 1) stop("reproduction level must be in (0, 1)")
  rdd <- unname(getRDD(p)[i])
  rdi <- unname(getRDI(p)[i])
  p@species_params$R_max[i]  <- rdd / rl
  p@species_params$erepro[i] <- p@species_params$erepro[i] * (rdd / (1 - rl)) / rdi
  p
}

#' Reproduction diagnostics for one group, plus the two all-group constraints.
cf_repro_state <- function(p, sp) {
  i <- match(sp, p@species_params$species)
  c(erepro = p@species_params$erepro[i], R_max = p@species_params$R_max[i],
    repro_level = unname(getReproductionLevel(p)[i]),
    rdd = unname(getRDD(p)[i]), rdi = unname(getRDI(p)[i]),
    max_erepro_all = max(p@species_params$erepro),
    n_rmax_inf_all = sum(!is.finite(p@species_params$R_max)))
}

# ------------------------------------------------------------ validation -----
#' The five gates.  Nothing downstream runs until all pass.
cf_validate_harness <- function(members = NULL, tol = 1e-12, verbose = TRUE) {
  if (is.null(members)) members <- cf_load_members()
  rank <- read.csv(CF_RANKING)
  res  <- list()
  use_cache <- TRUE

  for (m in members) {
    si <- m$sim_index; p <- m$params; s <- m$stored_sim
    if (verbose) cat("\n--- member", si, "---\n")

    # gate 1: re-projection reproduces the stored yield.
    # Arms B and C have no stored sim, so the reference becomes the arm's own
    # first projection and gate 1 degrades to a determinism check.
    y_re <- cf_project_yield(p, NULL)$yield
    if (is.null(s)) {
      y_stored <- y_re
      g1 <- max(abs(cf_project_yield(p, NULL)$yield - y_stored) /
                pmax(abs(y_stored), 1e-300))
    } else {
      y_stored <- getYield(s)
      y_stored <- y_stored[as.numeric(rownames(y_stored)) <= CF_LAST_YEAR, , drop = FALSE]
      g1 <- max(abs(y_re - y_stored) / pmax(abs(y_stored), 1e-300))
    }

    # gate 2: null treatment through gear_params<-
    p_null <- cf_set_q(p, cf_get_q(p))
    y_null <- cf_project_yield(p_null, NULL)$yield
    g2 <- max(abs(y_null - y_stored) / pmax(abs(y_stored), 1e-300))

    # gate 3: therMizer rate functions survive the mutation
    g3 <- identical(p@rates_funcs, p_null@rates_funcs) &&
          identical(p@initial_n, p_null@initial_n) &&
          identical(p@resource_dynamics, p_null@resource_dynamics) &&
          identical(p@other_dynamics, p_null@other_dynamics)

    # gate 4: 1929-cached start equals the full projection
    cache <- cf_make_cache(p)
    y_cache <- cf_project_yield(p, cache)$yield
    common <- intersect(rownames(y_cache), rownames(y_stored))
    g4 <- max(abs(y_cache[common, ] - y_stored[common, ]) /
              pmax(abs(y_stored[common, ]), 1e-300))
    if (!is.finite(g4) || g4 > tol) use_cache <- FALSE

    # gate 5: pooled RMSE matches the published ranking.  Arm A only -- B and C
    # were rebuilt through steady() and are not comparable with stored values.
    sc <- cf_score_yield_matrix(y_stored)
    ref <- if (is.null(s)) NA_real_ else rank$rmse[rank$sim_index == si]
    g5 <- if (is.null(s)) 0 else abs(sc$rmse - ref) / ref

    res[[as.character(si)]] <- data.frame(
      sim_index = si, arm = CF_ARM, stored_rmse = ref, recomputed_rmse = sc$rmse,
      g1_reproject = g1, g2_null = g2, g3_slots = g3,
      g4_cache = g4, g5_rmse = g5, stringsAsFactors = FALSE)

    if (verbose)
      cat(sprintf("  g1 reproject %.2e / g2 null %.2e / g3 slots %s / g4 cache %.2e / g5 rmse %.2e\n",
                  g1, g2, g3, g4, g5))
  }

  out <- do.call(rbind, res)
  pass <- all(out$g1_reproject <= tol) && all(out$g2_null <= tol) &&
          all(out$g3_slots) && all(out$g5_rmse <= 1e-10)
  attr(out, "pass") <- pass
  attr(out, "use_cache") <- use_cache
  if (verbose) {
    cat("\n=== gates 1,2,3,5:", if (pass) "PASS" else "FAIL", "===\n")
    cat("=== 1929 cache:", if (use_cache) "ENABLED" else "DISABLED (falling back to full projections)", "===\n")
  }
  out
}

# ------------------------------------------------------------- utilities -----
#' Output directory for the ACTIVE arm.  Arm A writes to the plain paths; B and C
#' write to arm-suffixed siblings so no arm can overwrite another.
#' `arm = NA` gives the arm-independent root (used for the arm-building outputs).
cf_out_dir <- function(sub = NULL, arm = CF_ARM) {
  root <- if (is.na(arm) || arm == "A") CF_OUT else paste0(CF_OUT, "_arm", arm)
  d <- if (is.null(sub)) root else file.path(root, sub)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

cf_cores <- function() {
  n <- as.integer(Sys.getenv("CF_CORES", "1"))
  max(1L, n)
}

cf_log <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "", ..., "\n", sep = "")
