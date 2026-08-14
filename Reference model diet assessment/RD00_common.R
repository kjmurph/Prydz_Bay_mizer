# =============================================================================
# RD00 -- shared setup for the reference model diet assessment
#
# WHAT
#   Loads the revised-kernel reference params object and provides the pieces the
#   RD01..RD05 scripts share: prey groupings and palettes (transcribed from the
#   published figure so the new ones are directly comparable), the figure/CSV
#   writers, the prey-biomass field used by the PPMR and availability
#   calculations, and a trophic-level function that actually works on this model.
#
# WHY THIS FILE EXISTS AT ALL -- two mizer/therMizer interactions to know about:
#
#  1. DIET PROPORTIONS ARE SAFE WITH PLAIN mizer::getDiet(); ABSOLUTE RATES ARE NOT.
#     This params carries rates_funcs$Encounter = "therMizerEncounter". therMizer
#     scales encounter by a temperature effect that is a PER-SPECIES SCALAR
#     (vertical_migration is flat in body size -- asserted in the shim), so it
#     cancels exactly from within-predator proportions, including through the
#     feeding level. Measured: max |getDiet(proportion=TRUE) - ther_diet()
#     proportions| = 2.2e-16. The startup assertion below re-checks this every run.
#     Absolute rates still differ by up to 1.03 log10 units, so anything in
#     grams/year must go through ther_diet() from R/wmin_test/thermizer_shim.R.
#
#  2. mizer::getTrophicLevel() IS WRONG ON THIS MODEL. It returns orca 192.1,
#     toothfishes 60.9, baleen whales 27.5. The cause is an interaction, not a
#     bug in mizer alone: getTrophicLevel.MizerParams builds its numerator E_tl
#     from params@search_vol %*% pred_kernel DIRECTLY, but takes its denominator
#     from getEncounter(), which DOES dispatch to therMizerEncounter. The
#     temperature factor therefore fails to cancel from cumA/cumB, inflating TL
#     by ~1/temp_eff (0.09-0.71 by species) and compounding recursively up the
#     food web. ther_trophic_level() below is a transcription of mizer's function
#     with E_tl scaled by the same factor, so it cancels as it should. RD02
#     asserts the transcription is faithful by reproducing mizer's own output
#     bit-identically when the scaling is switched off.
#     Any mizer extension that rescales encounter hits this. Worth reporting.
#
# USAGE  sourced by RD01..RD05; not run directly.
# ENV    RD_PARAMS (default params_whale_lognormal_kernel_bk05_mk05.rds)
#        RD_YEAR   (default 2010 -- see note below)
#        RD_OUT    (default "Reference model diet assessment")
#        RD_FORCE  (set to 1 to overwrite existing figures/CSVs)
#
# ON RD_YEAR: the therMizer forcing arrays (ocean_temp, n_pp_array) run
# 1841-2010, so a literal 2011-2020 window does not exist in this model. The
# steady state IS contemporary: initial_n_pp matches plankton_forcing(t = 2005)
# to 0.021 log10. Diet proportions are temperature-invariant (point 1 above), so
# RD_YEAR only affects the trophic-level growth weighting, weakly.
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer)
  library(ggplot2); library(dplyr); library(tidyr); library(scales)
  library(patchwork); library(grid)
})

RD_PARAMS <- Sys.getenv("RD_PARAMS", "params_whale_lognormal_kernel_bk05_mk05.rds")
RD_YEAR   <- as.numeric(Sys.getenv("RD_YEAR", "2010"))
RD_ROOT   <- Sys.getenv("RD_OUT", "Reference model diet assessment")
RD_FORCE  <- nzchar(Sys.getenv("RD_FORCE"))

RD_FIG <- file.path(RD_ROOT, "figures")
RD_ANA <- file.path(RD_ROOT, "analysis")
for (d in c(RD_FIG, file.path(RD_FIG, "per_species"), RD_ANA))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

source(file.path("R", "wmin_test", "thermizer_shim.R"))

# --- output writers, transcribed from Manuscript scripts/F07_*.R:44-53 --------
guard <- function(f) {
  if (file.exists(f) && !RD_FORCE)
    stop("refusing to overwrite: ", f, "\n  set RD_FORCE=1 to replace",
         call. = FALSE)
  f
}
sv <- function(p, stem, w, h, subdir = NULL) {
  out <- if (is.null(subdir)) RD_FIG else file.path(RD_FIG, subdir)
  png <- guard(file.path(out, paste0(stem, ".png")))
  pdf <- guard(file.path(out, paste0(stem, ".pdf")))
  suppressWarnings({
    ggsave(png, p, width = w, height = h, dpi = 300, limitsize = FALSE)
    ggsave(pdf, p, width = w, height = h, limitsize = FALSE)
  })
  cat("  wrote", file.path(basename(out), basename(png)), "and .pdf\n")
}
wcsv <- function(df, stem) {
  f <- guard(file.path(RD_ANA, paste0(stem, ".csv")))
  write.csv(df, f, row.names = FALSE)
  cat("  wrote", basename(f), sprintf("(%d rows)\n", nrow(df)))
}
slugify <- function(x) gsub("[^a-z0-9]+", "_", tolower(x))

# --- prey groupings, palette, labels ------------------------------------------
# Transcribed verbatim from Manuscript scripts/F07_diet_composition_figures.R:65-93
# so the 19-panel figure here is directly comparable to the published
# diet_by_size_contemporary_kernel158 built from the 158-member ensemble.
PREY_GROUPS <- list(
  "Antarctic krill" = "antarctic krill",
  "Other LTL"       = c("mesozooplankton", "other krill",
                        "other macrozooplankton", "salps"),
  "Fishes"          = c("mesopelagic fishes", "bathypelagic fishes",
                        "shelf and coastal fishes", "toothfishes"),
  "Squids"          = "squids",
  "Seabirds"        = c("flying birds", "small divers"),
  "Pinnipeds"       = c("leopard seals", "medium divers", "large divers"),
  "Cetaceans"       = c("minke whales", "orca", "sperm whales", "baleen whales"),
  "Plankton resource" = "Resource")     # "External" deliberately excluded
PREY_GROUP_COLS <- c("Antarctic krill" = "#e8534a", "Other LTL" = "#6dbf6b",
  "Fishes" = "#1a6faf", "Squids" = "#e87c10", "Seabirds" = "#9e9e9e",
  "Pinnipeds" = "#a0522d", "Cetaceans" = "#9467bd",
  "Plankton resource" = "#c8e6a0")
PREY_GROUP_ORDER <- rev(names(PREY_GROUP_COLS))
PRED_DISPLAY <- c(
  "mesozooplankton" = "Mesozooplankton", "other krill" = "Other krill",
  "other macrozooplankton" = "Other macrozooplankton",
  "antarctic krill" = "Antarctic krill", "salps" = "Salps",
  "mesopelagic fishes" = "Mesopelagic fishes",
  "bathypelagic fishes" = "Bathypelagic fishes",
  "shelf and coastal fishes" = "Shelf & coastal fishes",
  "flying birds" = "Flying birds", "small divers" = "Small divers",
  "squids" = "Squids", "toothfishes" = "Toothfishes",
  "leopard seals" = "Leopard seals", "medium divers" = "Medium divers",
  "large divers" = "Large divers", "minke whales" = "Minke whales",
  "orca" = "Orca", "sperm whales" = "Sperm whales",
  "baleen whales" = "Large baleen whales")

theme_panels <- function() theme_bw(base_size = 10) + theme(
  strip.text = element_text(face = "bold", size = 8),
  axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7),
  legend.position = "bottom", legend.key.size = unit(0.4, "cm"),
  legend.text = element_text(size = 8),
  legend.title = element_text(size = 9, face = "bold"),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank())

theme_rd <- function(base = 9) theme_bw(base_size = base) + theme(
  strip.text = element_text(face = "bold", size = base - 1),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(colour = "grey93", linewidth = 0.3),
  legend.key.size = unit(0.4, "cm"))

# --- load the reference model -------------------------------------------------
cat("=== RD00: reference model diet assessment ===\n")
cat("params:", RD_PARAMS, "| temperature year:", RD_YEAR, "\n")
PARAMS <- suppressWarnings(validParams(readRDS(RD_PARAMS)))
SPECIES <- PARAMS@species_params$species
NS      <- length(SPECIES)
NW      <- length(PARAMS@w)
NWF     <- length(PARAMS@w_full)
IDX_SP  <- (NWF - NW + 1):NWF          # species bins inside the w_full grid
OCC     <- PARAMS@initial_n > 0        # size bins each species actually occupies

stopifnot(identical(PARAMS@w[1], PARAMS@w_full[IDX_SP][1]))
cat("species:", NS, "| w bins:", NW, "| w_full bins:", NWF, "\n")
cat("rates_funcs$Encounter:", PARAMS@rates_funcs$Encounter, "\n")

#' Prey biomass field on the w_full grid.
#'
#' Row j (j <= NS) is species j's biomass per bin, placed in the species portion
#' of the w_full grid; row NS+1 is the background resource. Multiplying by the
#' predation kernel and summing over prey size gives the encounter integrand up
#' to the search_vol factor, which is constant in prey size and so cancels from
#' every ratio computed here.
#'
#' @return numeric matrix [(NS + 1) x NWF], rows named species..., "Resource"
prey_field <- function(params, n = params@initial_n, n_pp = params@initial_n_pp) {
  m <- matrix(0, nrow = nrow(params@species_params) + 1L,
              ncol = length(params@w_full),
              dimnames = list(c(params@species_params$species, "Resource"),
                              names(params@initial_n_pp)))
  ns <- nrow(params@species_params); nwf <- length(params@w_full)
  m[seq_len(ns), (nwf - length(params@w) + 1):nwf] <-
    sweep(n, 2, params@w * params@dw, "*")
  m[ns + 1L, ] <- n_pp * params@w_full * params@dw_full
  m
}

#' Interaction matrix extended with the resource column, [NS x (NS + 1)].
interaction_full <- function(params)
  cbind(params@interaction,
        Resource = params@species_params$interaction_resource)

#' therMizer's encounter temperature effect as a per-species scalar.
#'
#' therMizer::scaled_temp_effect() returns [species x size]. It is flat in size
#' because vertical_migration is (the shim asserts the same thing), and the
#' whole argument for using plain getDiet() proportions rests on that flatness,
#' so assert it here rather than silently taking a column.
temp_effect_scalar <- function(params, t) {
  s <- therMizer::scaled_temp_effect(params, t)
  spread <- max(apply(s, 1, function(r) diff(range(r))))
  if (spread > 1e-12)
    stop("temperature effect varies with body size (max range ", spread,
         "); the per-species scalar assumption is invalid.", call. = FALSE)
  setNames(as.numeric(s[, 1]), params@species_params$species)
}

#' Trophic level under the therMizer temperature scaling.
#'
#' Transcription of mizer:::getTrophicLevel.MizerParams (mizer 3.1.0) with ONE
#' change: E_tl -- the trophic-level-weighted encounter that forms the numerator
#' -- is scaled by the same per-species temperature factor that
#' therMizerEncounter applies to the denominator. See the header of this file.
#'
#' Because the factor then appears in both cumA and cumB it cancels from their
#' ratio exactly; the only residual effect of temperature on TL is through the
#' dw/g lifetime weighting, which is why this is not simply equal to running
#' mizer's function on a params with the rate functions reset. RD02 reports both.
#'
#' @param temp_eff per-species scaling; NULL means take it from therMizer at
#'   time `t`. Pass rep(1, NS) to switch the correction off (used by the
#'   faithfulness assertion in RD02).
#' @return ArraySpeciesBySize [species x size], NA below each species' egg size
ther_trophic_level <- function(params, t = RD_YEAR,
                               n = params@initial_n,
                               n_pp = params@initial_n_pp,
                               n_other = params@initial_n_other,
                               w_R = 1e-10, beta_R = 1000,
                               temp_eff = NULL) {
  stopifnot(is.numeric(w_R), w_R > 0, is.numeric(beta_R), beta_R > 1)
  no_sp <- nrow(params@species_params)
  no_w  <- length(params@w)
  no_w_full <- length(params@w_full)
  idx_sp <- (no_w_full - no_w + 1):no_w_full

  if (is.null(temp_eff)) {
    temp_eff <- if (identical(params@rates_funcs$Encounter, "therMizerEncounter"))
      temp_effect_scalar(params, t) else rep(1, no_sp)
  }
  stopifnot(length(temp_eff) == no_sp)

  # rates through whatever rate functions the params actually carries, at the
  # requested year -- getRates returns a mutually consistent set
  r <- getRates(params, n = n, n_pp = n_pp, n_other = n_other,
                effort = params@initial_effort, t = t)
  encounter     <- r$encounter
  feeding_level <- r$feeding_level
  growth        <- r$e_growth
  consumption   <- (1 - feeding_level) * encounter

  pred_kernel <- getPredKernel(params)
  w_ba      <- mizer:::bin_average_summary_weight(params@w, params)
  w_full_ba <- mizer:::bin_average_summary_weight(params@w_full, params)

  prey_mass_tl <- sweep(n, 2, w_ba * params@dw, "*")
  tl_R <- pmax(1, 1 + log(params@w_full / w_R) / log(beta_R))
  resource_mass_tl <- tl_R * n_pp * w_full_ba * params@dw_full
  ae_R <- rowSums(sweep(pred_kernel, 3, resource_mass_tl, "*"), dims = 2)
  # THE CORRECTION: scale by temp_eff, as therMizerEncounter does downstream
  E_tl_resource <- temp_eff *
    (params@search_vol * (params@species_params$interaction_resource * ae_R))

  cumA <- numeric(no_sp); cumB <- numeric(no_sp)
  tl <- matrix(NA_real_, nrow = no_sp, ncol = no_w,
               dimnames = dimnames(params@initial_n))
  for (k in seq_len(no_w)) {
    pred_kernel_k <- matrix(pred_kernel[, k, idx_sp], nrow = no_sp)
    ae_k <- pred_kernel_k %*% t(prey_mass_tl)
    # THE CORRECTION, again
    E_tl <- temp_eff * (params@search_vol[, k] *
                          rowSums(params@interaction * ae_k))
    E_tl <- E_tl + E_tl_resource[, k]
    for (i in seq_len(no_sp)) {
      if (k < params@w_min_idx[i]) next
      g_ik <- growth[i, k]
      if (!is.finite(g_ik) || g_ik <= 0) {
        tl[i, k] <- if (k > params@w_min_idx[i]) tl[i, k - 1L] else 1
        next
      }
      weight <- params@dw[k] / g_ik
      cumA[i] <- cumA[i] + (1 - feeding_level[i, k]) * E_tl[i] * weight
      cumB[i] <- cumB[i] + consumption[i, k] * weight
      tl[i, k] <- if (cumB[i] > 0) 1 + cumA[i] / cumB[i] else 1
    }
    active_k <- k >= params@w_min_idx
    tl_k <- tl[, k]; tl_k[is.na(tl_k)] <- 1
    prey_mass_tl[active_k, k] <-
      n[active_k, k] * tl_k[active_k] * w_ba[k] * params@dw[k]
  }
  ArraySpeciesBySize(tl, value_name = "Trophic level", params = params)
}

# --- startup assertion: why plain getDiet() is used for proportions -----------
# If this ever fails, the per-species-scalar argument has broken and every
# proportion in RD01/RD04 must go back through ther_diet().
local({
  gd <- getDiet(PARAMS, proportion = TRUE)
  td <- ther_diet(PARAMS, PARAMS@initial_n, PARAMS@initial_n_pp,
                  PARAMS@initial_n_other, year = RD_YEAR)
  tp <- sweep(td, c(1, 2), pmax(apply(td, c(1, 2), sum), 1e-300), "/")
  d  <- max(abs(sweep(gd - tp, c(1, 2), OCC, "*")))
  cat(sprintf("assert getDiet vs ther_diet proportions: max |diff| = %.3g\n", d))
  if (d > 1e-12)
    stop("getDiet proportions no longer match the therMizer-corrected diet ",
         "(max diff ", d, "); use ther_diet() throughout.", call. = FALSE)
})
cat("RD00 ready.\n\n")
