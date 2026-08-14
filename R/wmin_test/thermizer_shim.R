# =============================================================================
# therMizer shim for the w_min test
#
# The saved ensembles were projected with rate functions "therMizerEncounter"
# and "therMizerPredRate" (see params@rates_funcs), but the therMizer package is
# not installed on this machine. mizer's own getDiet()/getPredMort() would
# therefore either fail or silently drop the temperature scaling.
#
# This file reimplements the temperature scaling explicitly. Two facts make the
# reimplementation safe, both verified in R/wmin_test/_verify_tempscale.R and
# _explore_therm_arrays.R:
#
#  1. The unscaled temperature effect is
#         U_i(T) = T_K * (T_K - (temp_min_i + 273)) * sqrt((temp_max_i + 273) - T_K)
#     with T_K = T_celsius + 273. Taking the maximum of U_i over a 0.1-degree
#     grid spanning [temp_min_i, temp_max_i] reproduces the stored
#     species_params$encounterpred_scale for all 19 groups with a maximum
#     relative error of 0 (exact). No other candidate form comes close.
#
#  2. other_params$vertical_migration is FLAT IN BODY SIZE for every group
#     (0.2 in each of the 5 realms, exposure 1). The temperature effect is
#     therefore a per-species SCALAR, identical across all 100 size bins. This
#     is the reason the Stage 0 within-species size decomposition is robust:
#     a species-level scalar cancels from any within-species share except
#     through its (weak, bounded) effect on the feeding level.
#
# Temperature is indexed by YEAR NAME, not by therMizer's `t + t_idx` offset
# arithmetic (t_idx = -1841 against rows named 1841..2010 is off by one and
# getDiet() would in any case call it with the default t = 0). Indexing by name
# is unambiguous; ocean temperature varies slowly enough (median -0.93 C, full
# range -1.19..-0.47 C across realms) that a one-year offset is immaterial.
# =============================================================================

suppressPackageStartupMessages(library(mizer))

#' Per-species temperature scaling factor on encounter and predation rate.
#'
#' @param params MizerParams from a saved ensemble member
#' @param year   calendar year (must be a row name of other_params$other$ocean_temp)
#' @return named numeric vector, one scalar per species
ther_temp_effect <- function(params, year) {
  op <- params@other_params$other
  ot <- op$ocean_temp                 # time x realm, degrees C
  vm <- op$vertical_migration         # realm x species x w
  ex <- op$exposure                   # realm x species
  sp <- params@species_params

  yr <- as.character(year)
  if (!yr %in% rownames(ot))
    stop("year ", yr, " not present in ocean_temp (", rownames(ot)[1], "..",
         rownames(ot)[nrow(ot)], ")")
  temp_c <- ot[yr, ]                  # one temperature per realm
  temp_k <- temp_c + 273

  tmin_k <- sp$temp_min + 273
  tmax_k <- sp$temp_max + 273

  # unscaled effect: realm x species
  U <- outer(temp_k, seq_len(nrow(sp)), function(TK, i) {
    inner <- tmax_k[i] - TK
    inner[inner < 0] <- 0             # outside thermal tolerance -> no effect
    TK * (TK - tmin_k[i]) * sqrt(inner)
  })
  # zero out where below thermal minimum
  below <- outer(temp_k, tmin_k, "<")
  U[below] <- 0

  # weight realms by vertical migration x exposure. vertical_migration is flat
  # in w (verified), so take the w = 1 slice; assert that flatness here.
  vm_w1 <- vm[, , 1, drop = TRUE]     # realm x species
  flat <- max(apply(vm, c(1, 2), function(v) diff(range(v))))
  if (flat > 1e-12)
    stop("vertical_migration varies with body size (max range ", flat,
         "); the per-species scalar assumption in this shim is invalid.")

  scaled <- colSums(U * vm_w1 * ex) / sp$encounterpred_scale
  names(scaled) <- sp$species
  scaled
}

#' Feeding level under the therMizer temperature scaling.
ther_feeding_level <- function(params, n, n_pp, n_other, year, temp_eff = NULL) {
  if (is.null(temp_eff)) temp_eff <- ther_temp_effect(params, year)
  enc <- mizer::mizerEncounter(params, n = n, n_pp = n_pp, n_other = n_other, t = 0)
  enc <- sweep(enc, 1, temp_eff, "*")
  mizer::mizerFeedingLevel(params, n = n, n_pp = n_pp, n_other = n_other,
                           encounter = enc, t = 0)
}

#' Prey-resolved consumption rate per individual predator (g prey / year).
#'
#' Mirrors mizer::getDiet(proportion = FALSE) but applies the therMizer
#' per-species temperature scaling to both the encounter numerator and the
#' feeding level, so the two are mutually consistent.
#'
#' @return array [predator, w, prey], prey = species..., "Resource", "External"
ther_diet <- function(params, n, n_pp, n_other, year, temp_eff = NULL,
                      feeding_level = NULL) {
  if (is.null(temp_eff)) temp_eff <- ther_temp_effect(params, year)

  species  <- params@species_params$species
  no_sp    <- length(species)
  no_w     <- length(params@w)
  no_w_full<- length(params@w_full)
  no_other <- length(params@other_encounter)

  diet <- array(0, dim = c(no_sp, no_w, no_sp + 2 + no_other),
                dimnames = list(predator = species,
                                w = dimnames(params@initial_n)$w,
                                prey = c(species, "Resource", "External",
                                         names(params@other_encounter))))
  idx_sp <- (no_w_full - no_w + 1):no_w_full

  # mizer 2.5.0 objects (which these are) have no `second_order_w` slot; that
  # slot arrived in mizer 3.x. slotNames() reads the CLASS definition (3.1.0),
  # so it reports the slot as present even when the instance lacks it -- test
  # the instance instead. Absent means the 2.5.0 behaviour: plain w.
  use_bin_average <- tryCatch(isTRUE(params@second_order_w[["bin_average"]]),
                              error = function(e) FALSE)
  if (use_bin_average) {
    w_eff      <- mizer:::bin_average_weight(params@w)
    w_full_eff <- mizer:::bin_average_weight(params@w_full)
  } else {
    w_eff      <- params@w
    w_full_eff <- params@w_full
  }

  if (!is.null(comment(params@pred_kernel))) {
    ae <- matrix(params@pred_kernel[, , idx_sp, drop = FALSE], ncol = no_w) %*%
          t(sweep(n, 2, w_eff * params@dw, "*"))
    diet[, , 1:no_sp]   <- ae
    diet[, , no_sp + 1] <- rowSums(sweep(params@pred_kernel, 3,
                                         params@dw_full * w_full_eff * n_pp, "*"),
                                   dims = 2)
  } else {
    prey <- matrix(0, nrow = no_sp + 1, ncol = no_w_full)
    prey[1:no_sp, idx_sp] <- sweep(n, 2, w_eff * params@dw, "*")
    prey[no_sp + 1, ]     <- n_pp * w_full_eff * params@dw_full
    ft <- array(rep(params@ft_pred_kernel_e, times = no_sp + 1) *
                  rep(mvfft(t(prey)), each = no_sp),
                dim = c(no_sp, no_w_full, no_sp + 1))
    ft <- matrix(aperm(ft, c(2, 1, 3)), nrow = no_w_full)
    ae <- array(Re(mvfft(ft, inverse = TRUE) / no_w_full),
                dim = c(no_w_full, no_sp, no_sp + 1))
    ae <- ae[idx_sp, , , drop = FALSE]
    ae <- aperm(ae, c(2, 1, 3))
    ae[ae < 1e-18] <- 0
    diet[, , 1:(no_sp + 1)] <- ae
  }

  inter <- cbind(params@interaction, params@species_params$interaction_resource)
  diet[, , 1:(no_sp + 1)] <-
    sweep(sweep(diet[, , 1:(no_sp + 1), drop = FALSE], c(1, 3), inter, "*"),
          c(1, 2), params@search_vol, "*")
  diet[, , no_sp + 2] <- params@ext_encounter

  # therMizer: scale the encounter by the per-species temperature effect
  diet <- sweep(diet, 1, temp_eff, "*")

  if (is.null(feeding_level))
    feeding_level <- ther_feeding_level(params, n, n_pp, n_other, year,
                                        temp_eff = temp_eff)
  fish_mask <- n > 0
  diet <- sweep(diet, c(1, 2), (1 - feeding_level) * fish_mask, "*")
  diet
}

#' Predation mortality [prey_species, w] (1/year) under therMizer scaling.
ther_pred_mort <- function(params, n, n_pp, n_other, year, temp_eff = NULL,
                           feeding_level = NULL) {
  if (is.null(temp_eff)) temp_eff <- ther_temp_effect(params, year)
  if (is.null(feeding_level))
    feeding_level <- ther_feeding_level(params, n, n_pp, n_other, year,
                                        temp_eff = temp_eff)
  pred_rate <- mizer::mizerPredRate(params, n = n, n_pp = n_pp, n_other = n_other,
                                    t = 0, feeding_level = feeding_level)
  # therMizerPredRate scales the predation rate by the predator's temp effect
  pred_rate <- sweep(pred_rate, 1, temp_eff, "*")
  idx_sp <- (length(params@w_full) - length(params@w) + 1):length(params@w_full)
  pm <- base::t(params@interaction) %*% pred_rate[, idx_sp, drop = FALSE]
  dimnames(pm) <- list(prey = params@species_params$species,
                       w = dimnames(params@initial_n)$w)
  pm
}

#' Recruitment (eggs produced per year) under the therMizer temperature scaling.
#'
#' getRDD()/getRDI() have the same defect as getDiet(): they default to t = 0,
#' which under therMizer indexes ocean_temp at the wrong year. This threads the
#' by-year temperature effect through the whole chain -- encounter -> feeding
#' level -> energy for reproduction -> RDI -> the params' own RDD function -- so
#' the recruitment is consistent with the diet and feeding level computed above.
#'
#' TWO API FACTS, both established empirically against mizer 3.1.0 rather than
#' assumed, because getting either wrong is silent:
#'
#'   1. Use `getRDI(params, ..., e_repro = )`. Calling `mizerRDI()` directly with
#'      `e_growth`/`mort` omitted RUNS WITHOUT ERROR and returns ZEROS. The
#'      generic reproduces getRDI(params) to a relative difference of exactly 0;
#'      the direct call is out by a factor of 1.
#'   2. Apply the RDD function named in `params@rates_funcs$RDD` (BevertonHoltRDD
#'      here) rather than hard-coding it, and call it as `f(rdi, species_params)`.
#'
#' Note that `reproduction_level` in mizer is RDD / R_max, i.e. 1 - RDD/RDI --
#' not RDD/RDI. See ?setBevertonHolt.
#'
#' @return list(rdi, rdd), each a named numeric vector of eggs per year
ther_rdd <- function(params, n, n_pp, n_other, year, temp_eff = NULL,
                     encounter = NULL, feeding_level = NULL) {
  if (is.null(temp_eff)) temp_eff <- ther_temp_effect(params, year)
  if (is.null(encounter)) {
    encounter <- mizer::mizerEncounter(params, n = n, n_pp = n_pp,
                                       n_other = n_other, t = 0)
    encounter <- sweep(encounter, 1, temp_eff, "*")
  }
  if (is.null(feeding_level))
    feeding_level <- mizer::mizerFeedingLevel(params, n = n, n_pp = n_pp,
                                              n_other = n_other,
                                              encounter = encounter, t = 0)
  e <- mizer::mizerEReproAndGrowth(params, n = n, n_pp = n_pp, n_other = n_other,
                                   t = 0, encounter = encounter,
                                   feeding_level = feeding_level)
  e_repro <- mizer::mizerERepro(params, n = n, n_pp = n_pp, n_other = n_other,
                                t = 0, e = e)
  rdi <- mizer::getRDI(params, n = n, n_pp = n_pp, n_other = n_other, t = 0,
                       e_repro = e_repro)
  rdd <- do.call(params@rates_funcs$RDD, list(rdi, params@species_params))
  sp <- params@species_params$species
  list(rdi = setNames(as.numeric(rdi), sp), rdd = setNames(as.numeric(rdd), sp))
}

#' Pull the state (n, n_pp, n_other) out of a MizerSim at a given year.
sim_state_at <- function(sim, year) {
  times <- as.numeric(dimnames(sim@n)$time)
  ti <- which(times == year)
  if (length(ti) != 1) stop("year ", year, " not found in sim")
  n <- sim@n[ti, , , drop = FALSE]
  dim(n) <- dim(sim@n)[2:3]
  dimnames(n) <- dimnames(sim@n)[2:3]
  n_pp <- sim@n_pp[ti, ]
  n_other <- sim@params@initial_n_other   # empty in this model
  list(n = n, n_pp = n_pp, n_other = n_other)
}
