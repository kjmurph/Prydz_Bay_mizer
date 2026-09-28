# =============================================================================
# GF00 -- shared machinery for the growth-curve and feeding-level assessment
#
# ---------------------------------------------------- WHY growth_curves() EXISTS
# `mizer::plotGrowthCurves()` / `getGrowthCurves()` FAIL on this model with
#
#     Error in ws[j, ] <- deSolve::ode(...)[, 2] :
#       number of items to replace is not a multiple of replacement length
#
# preceded by a DLSODA warning "too much accuracy requested ... TOLSF = nan".
# That message is misleading: it is NOT a tolerance problem, and loosening
# rtol/atol does not help (tested: 39 -> 40 of 50 points).
#
# THE ACTUAL CAUSE is in mizer's own getGrowthCurves.MizerParams. It builds the
# growth interpolator as
#
#     g_fn <- stats::approxfun(c(params@w[keep], w_max), c(g[i, keep], 0))
#
# with no `rule` argument, so `rule = 1` applies and the function returns **NA**
# outside [w_min, w_max]. The lsoda solver inevitably overshoots w_max slightly
# once a species reaches its asymptotic size inside `max_age`; the derivative
# then evaluates to NA, the solver aborts early, and the short result fails the
# array assignment with the cryptic message above.
#
# In this model that is MINKE WHALES, which reach w_max = 6.0e6 g at ~15.2 y of
# a 20 y window. The other 18 groups integrate fine, which is why the failure
# looks arbitrary. Verified: g_fn(w_max * 1.000001) is NA under mizer's call and
# 0 with `rule = 2`.
#
# THE FIX is `rule = 2` (constant extrapolation, so growth stays 0 above w_max).
# Everything else is transcribed from mizer 3.1.0 unchanged. Verified identical
# to `mizer::getGrowthCurves()` for all 18 species where mizer succeeds, so this
# repairs the failure without altering any working result.
#
# ------------------------------------------------------------------ ON TIME
# therMizer's rates are temperature-dependent and indexed by `t`.
# `scaled_temp_effect()` wraps an out-of-range t as `t %% 2010 + 1841`, so the
# mizer default of t = 0 resolves to **model year 1841** -- the unexploited
# baseline, and the year the member spin-up ends on. That is the right reference
# point for both the reference model and the ensemble, but it is a CHOICE and it
# is stated on every figure. Pass `t` to evaluate a different year.
#
# USAGE  source("R/growth_feeding/GF00_common.R")
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer)
  library(dplyr); library(ggplot2); library(tidyr)
})

# --- growth curves, with mizer's bug repaired --------------------------------
# Mirrors getGrowthCurves.MizerParams (mizer 3.1.0) except for `rule = 2`.
growth_curves <- function(params, max_age = 20, t = 0, n_age = 50) {
  sp  <- params@species_params$species
  age <- seq(0, max_age, length.out = n_age)
  ws  <- array(dim = c(length(sp), length(age)),
               dimnames = list(Species = sp, Age = age))
  g <- mizer::getEGrowth(params, t = t)
  for (i in seq_along(sp)) {
    w_max <- params@species_params$w_max[[i]]
    keep  <- params@w < w_max
    g_fn  <- stats::approxfun(c(params@w[keep], w_max),
                              c(g[i, keep], 0), rule = 2)   # <- THE FIX
    ws[i, ] <- deSolve::ode(y = params@w[params@w_min_idx[i]], times = age,
                 func = function(t, state, parameters) list(g_fn(state)))[, 2]
  }
  ws
}

# --- feeding level ------------------------------------------------------------
# getFeedingLevel() defaults time_range to 0, i.e. the same year as above; it is
# passed explicitly here so the two figures cannot silently diverge.
feeding_level <- function(params, t = 0) {
  mizer::getFeedingLevel(params, time_range = t)
}

# --- long-format helpers ------------------------------------------------------
# Below w_min a species does not exist; mizer still returns a feeding level
# there, and plotting it draws a flat shoulder that is not a model result.
# Masked to NA rather than dropped so the size grid stays aligned across members.
fl_long <- function(fl, params) {
  w <- params@w; sp <- params@species_params$species
  w_min <- params@w[params@w_min_idx]
  as.data.frame.table(fl, responseName = "feeding_level",
                      stringsAsFactors = FALSE) |>
    setNames(c("Species", "w_chr", "feeding_level")) |>
    mutate(w = rep(w, each = length(sp)),
           w_min = w_min[match(Species, sp)],
           w_max = params@species_params$w_max[match(Species, sp)],
           feeding_level = ifelse(w >= w_min & w <= w_max, feeding_level, NA_real_)) |>
    select(Species, w, feeding_level)
}

gc_long <- function(ws) {
  as.data.frame.table(ws, responseName = "w", stringsAsFactors = FALSE) |>
    setNames(c("Species", "Age", "w")) |>
    mutate(Age = as.numeric(Age))
}

# --- von Bertalanffy comparison ----------------------------------------------
# TRANSCRIBED FROM mizer's internal plot_growth_curves() so the comparison is
# mizer's, not a reinvention:
#     L_inf  <- (w_inf / a)^(1/b)
#     length <- L_inf * (1 - exp(-k_vb * (age - t0)))
#     weight <- a * length^b
# mizer draws it only when a, b, k_vb and w_inf are all present; t0 defaults to 0.
#
# READ THE `k_vb_round` FLAG BEFORE BELIEVING ANY OF THESE CURVES. In this model
# 15 of the 19 groups carry a ROUND DEFAULT k_vb -- 0.2 for every whale, seal and
# diver, 0.5 for every plankton group, 1.0 for flying birds. Only mesopelagic
# fishes (0.3608), bathypelagic fishes (0.1825), shelf and coastal fishes (0.15)
# and toothfishes (0.06) carry a value that is not a round placeholder. For the
# other 15 the "von Bertalanffy curve" is not independent data to test the model
# against -- it is a default, and agreement or disagreement means nothing. This
# is the same placeholder that makes matchGrowth() target a fictitious age_mat.
VB_ROUND <- c(0.05, 0.1, 0.2, 0.25, 0.5, 1)

vb_curve <- function(params, age) {
  sp <- params@species_params
  need <- c("a", "b", "k_vb", "w_inf")
  if (!all(need %in% names(sp))) return(NULL)
  t0 <- if ("t0" %in% names(sp)) sp$t0 else rep(0, nrow(sp))
  L_inf <- (sp$w_inf / sp$a)^(1 / sp$b)
  out <- do.call(rbind, lapply(seq_len(nrow(sp)), function(i) {
    len <- L_inf[i] * (1 - exp(-sp$k_vb[i] * (age - t0[i])))
    len[len < 0] <- 0                     # ages below t0 are not a size
    data.frame(Species = sp$species[i], Age = age,
               w_vb = sp$a[i] * len^sp$b[i],
               k_vb = sp$k_vb[i],
               k_vb_round = sp$k_vb[i] %in% VB_ROUND,
               stringsAsFactors = FALSE)
  }))
  out$vb_class <- ifelse(out$k_vb_round,
                         "von Bertalanffy (round-default k_vb)",
                         "von Bertalanffy (fitted k_vb)")
  out
}

# --- critical feeding level ---------------------------------------------------
# metab / intake_max / alpha -- the feeding level at which intake exactly covers
# metabolic cost, so growth is zero. Below it an individual is losing mass.
# It depends only on physiology, NOT on abundance, so it is IDENTICAL across
# every ensemble member and equal to the reference model's (verified: max
# absolute difference 0 over 25 sampled members). It is therefore drawn as a
# single reference line, never as a median with a ribbon.
critical_feeding_level <- function(params) {
  unname(as.matrix(unclass(mizer::getCriticalFeedingLevel(params))))
}

cfl_long <- function(params) {
  cfl <- critical_feeding_level(params)
  sp <- params@species_params$species; w <- params@w
  wmin <- params@w[params@w_min_idx]; wmax <- params@species_params$w_max
  data.frame(Species = rep(sp, times = length(w)),
             w = rep(w, each = length(sp)),
             f_crit = as.vector(cfl), stringsAsFactors = FALSE) |>
    filter(w >= wmin[match(Species, sp)], w <= wmax[match(Species, sp)])
}

# --- the canonical manuscript palette ----------------------------------------
# Transcribed from 81_yield_facets_figure.R so these figures key to the rest of
# the set. Do not regenerate with hue_pal().
sp_cols <- c(
  "antarctic krill"          = "#F8766D", "bathypelagic fishes" = "#B07A00",
  "shelf and coastal fishes" = "#93AA00", "squids"              = "#E8B33C",
  "toothfishes"              = "#00C19F", "minke whales"        = "#00B9E3",
  "orca"                     = "#619CFF", "sperm whales"        = "#DB72FB",
  "baleen whales"            = "#FF61C3", "mesopelagic fishes"  = "#D39200",
  "leopard seals"            = "#E07B39", "medium divers"       = "#2B6CB0",
  "large divers"             = "#4C8FD0", "flying birds"        = "#9E9E9E",
  "small divers"             = "#BDBDBD", "mesozooplankton"     = "#6A1B9A",
  "other krill"              = "#8E44AD", "other macrozooplankton" = "#9B59B6",
  "salps"                    = "#C39BD3")

# w_inf order, as the groups appear in species_params
sp_order <- function(params) params@species_params$species

# --- member states ------------------------------------------------------------
# A stored state carries $params and $initial_n; the abundance must be installed
# before any rate is computed, or every member returns the base model's rates.
load_member <- function(state_file) {
  st <- readRDS(state_file)
  p <- suppressWarnings(validParams(st$params))
  p@initial_n <- st$initial_n
  p
}

state_path <- function(dir, si) file.path(dir, sprintf("state_%05d.rds", si))