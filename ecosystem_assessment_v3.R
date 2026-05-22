###############################################################################
# Prydz Bay Ecosystem Assessment Framework (v2)
# 
# Comprehensive ecosystem-level assessment using PAIRED ensemble comparisons
# across three baseline frameworks:
#   1. Absolute health:     fishing / B0 (pre-exploitation 1841-1860)
#   2. Exploitation impact: fishing / climate-only (decade-matched)
#   3. Climate impact:      climate-only / B0
#
# Metrics are classified into three categories with different scoring:
#
#   Category A — BIOMASS METRICS: scored as depletion ratios against
#     literature-based thresholds (CCAMLR, IWC, BMSY).
#
#   Category B — STRUCTURAL/TROPHIC METRICS: scored as empirical deviation
#     from pre-exploitation (B0) ensemble distribution. Reports proportion
#     of simulations where the metric has been pushed outside the B0
#     5th-95th percentile envelope.
#
#   Category C — EXPLOITATION METRICS: absolute fishing mortality rates.
#
# Key design principles:
#   - Paired simulation comparisons (sim_i fishing vs sim_i climate-only)
#   - Empirically derived reference envelopes from pre-exploitation ensemble
#   - Probabilistic scoring: proportion of simulations exceeding thresholds
#   - B0 reference period: 1841-1860 (ISIMIP climate norm)
#   - Metric-specific thresholds for biomass; self-calibrating envelopes
#     for structural metrics
#
# Ecosystem assessment approach following:
#   Link & Watson (2019). Global ecosystem overfishing: Clear delineation
#     within real limits to production. Sci. Adv. 5:eaav0474.
#   Link et al. (2015). NOAA Fish. Proc. Doc., Ecosystem Assessment
#     methodology.
#   Morrison et al. (2024). Ecosystem assessment approaches for Southern
#     Ocean fisheries management.
#   Zhang, C., Chen, Y. & Ren, Y. (2015). Assessing uncertainty of a
#     multispecies size-spectrum model. ICES J. Mar. Sci. 72:2223-2233.
#
# Author: Generated for Prydz Bay mizer project
# Date: February 2026
###############################################################################

library(therMizer)
library(mizer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(patchwork)
library(ggtext)

# Configuration
OUTPUT_DIR <- "ecosystem_assessment_outputs"
OUTPUT_DIR_LARGE <- "Output_large_files/ecosystem_assessment"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)
if (!dir.exists(OUTPUT_DIR_LARGE)) dir.create(OUTPUT_DIR_LARGE, recursive = TRUE)

# Ensemble file paths
ENSEMBLE_PATHS <- list(
  fishing = "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds",
  climate_only = "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
)

# Decade definitions
DECADES <- data.frame(
  label = c("1841-1850", "1851-1860", "1861-1870", "1871-1880",
            "1881-1890", "1891-1900", "1901-1910", "1911-1920",
            "1921-1930", "1931-1940", "1941-1950", "1951-1960",
            "1961-1970", "1971-1980", "1981-1990", "1991-2000",
            "2001-2010"),
  start = seq(1841, 2001, by = 10),
  end = seq(1850, 2010, by = 10),
  stringsAsFactors = FALSE
)

B0_PERIOD <- c(1841, 1860)
PLOT_START_YEAR <- 1900  # Only display decades from this year onward in heatmaps

HISTORICAL_PERIODS <- list(
  pre_exploitation = c(1841, 1860),
  transition        = c(1861, 1900),
  early_whaling     = c(1901, 1920),
  peak_whaling      = c(1921, 1960),
  post_moratorium   = c(1961, 1976),
  krill_fishing     = c(1977, 1995),
  modern            = c(2001, 2010)
)

###############################################################################
#             METRIC CATEGORY CLASSIFICATION
###############################################################################

METRIC_CATEGORY <- list(
  # Category A: Biomass metrics
  total_biomass   = "biomass",
  whale_biomass   = "biomass",
  baleen_biomass  = "biomass",
  sperm_biomass   = "biomass",
  minke_biomass   = "biomass",
  seal_biomass    = "biomass",
  fish_biomass    = "biomass",
  krill_biomass   = "biomass",
  ltl_biomass     = "biomass",
  apex_biomass    = "biomass",
  # Category B: Structural/trophic metrics
  spectrum_slope           = "structural",
  spectrum_intercept       = "structural",
  mean_tl                  = "structural",
  htl_indicator            = "structural",
  large_fish_indicator     = "structural",   # REVISED: 100g fish-only (Zhang et al. 2015)
  fish_lfi                 = "structural",   # RETAINED: original 1000g fish-only LFI
  shannon_diversity        = "structural",   # NEW: Shannon-Wiener H' (Zhang et al. 2015)
  w_statistic              = "structural",   # NEW: Clarke & Warwick W (Zhang et al. 2015)
  production_biomass_ratio = "structural",   # NEW: community P/B turnover rate (krill+ species)
  fish_pb_ratio            = "structural",   # NEW: fish-only P/B turnover rate
  marine_mammal_pb_ratio   = "structural",   # NEW: marine mammal P/B turnover rate
  spectrum_mle_exponent    = "structural",   # NEW: MLE bounded power law exponent (Edwards et al. 2017)
  spectrum_lcd_exponent    = "structural",   # NEW: LCD cumulative distribution exponent (Edwards et al. 2017)
  spectrum_lbnbiom_slope   = "structural",   # NEW: Log-binned normalised biomass slope (Edwards et al. 2017)
  mean_weight              = "structural",
  mean_max_weight          = "structural",
  predator_prey_ratio      = "structural",
  consumer_ltl_ratio       = "structural",
  # Category C: Exploitation metrics
  exploitation_total = "exploitation",
  exploitation_whale = "exploitation",
  exploitation_krill = "exploitation"
)

# Biomass metrics in size-ordered display order (top to bottom):
# Total > All Whale > Baleen > Sperm > Apex > Minke > Seal > Fish > Krill > LTL
BIOMASS_METRICS <- c("total_biomass",
                     "ltl_biomass", "krill_biomass",
                     "minke_biomass", "apex_biomass", "sperm_biomass", "baleen_biomass",
                     "fish_biomass")
STRUCTURAL_METRICS <- names(METRIC_CATEGORY)[METRIC_CATEGORY == "structural"]
EXPLOITATION_METRICS <- names(METRIC_CATEGORY)[METRIC_CATEGORY == "exploitation"]

# Structural metrics to include in plots, in display order:
# Size-related first, then trophic/diversity
STRUCTURAL_METRICS_PLOT <- c(
  "spectrum_lbnbiom_slope",
  "mean_weight", "mean_max_weight",
  "large_fish_indicator", "w_statistic", "mean_tl",
  "production_biomass_ratio", "fish_pb_ratio", "marine_mammal_pb_ratio",
  "shannon_diversity")

###############################################################################
# Reference Points with Full Provenance
###############################################################################

THRESHOLDS_GENERAL <- c(ccamlr_gamma2 = 0.75, bmsy_proxy = 0.40, ccamlr_gamma1 = 0.20)
THRESHOLDS_WHALE <- c(ccamlr_gamma2 = 0.75, iwc_rmp_054 = 0.54, bmsy_proxy = 0.40, ccamlr_gamma1 = 0.20)
WHALE_METRICS <- c("baleen_biomass", "whale_biomass", "sperm_biomass", "minke_biomass")
MARINE_MAMMAL_METRICS <- c("baleen_biomass", "whale_biomass", "sperm_biomass", "minke_biomass", "seal_biomass", "apex_biomass")

# Metric-specific biomass depletion thresholds
BIOMASS_THRESHOLD_MAP <- c(
  total_biomass  = 0.90,  # Near-pristine reference level
  krill_biomass  = 0.75,  # CCAMLR gamma2 (Constable et al. 2000)
  ltl_biomass    = 0.75,  # CCAMLR gamma2 (Constable et al. 2000)
  whale_biomass  = 0.54,  # IWC RMP protection level (IWC 1994)
  baleen_biomass = 0.54,  # IWC RMP protection level (IWC 1994)
  sperm_biomass  = 0.54,  # IWC RMP protection level (IWC 1994)
  minke_biomass  = 0.54,  # IWC RMP protection level (IWC 1994)
  seal_biomass   = 0.54,  # IWC RMP protection level (IWC 1994)
  apex_biomass   = 0.54,  # IWC RMP protection level (IWC 1994)
  fish_biomass   = 0.40   # BMSY proxy (Restrepo et al. 1998)
)

get_thresholds_for_metric <- function(metric_name) {
  if (metric_name %in% WHALE_METRICS) return(THRESHOLDS_WHALE)
  return(THRESHOLDS_GENERAL)
}

###############################################################################
# Species Group Definitions
###############################################################################

SPECIES_GROUPS <- list(
  baleen_whales = c("baleen whales", "minke whales"),
  sperm_whales = c("sperm whales"),
  minke_whales = c("minke whales"),
  all_whales = c("baleen whales", "minke whales", "sperm whales", "orca"),
  apex_predators = c("sperm whales", "orca", "leopard seals"),
  marine_mammals = c("baleen whales", "minke whales", "sperm whales", "orca",
                     "leopard seals", "small divers", "medium divers", "large divers"),
  seals = c("leopard seals", "small divers", "medium divers", "large divers"),
  birds = c("flying birds"),
  fish = c("mesopelagic fishes", "bathypelagic fishes",
           "shelf and coastal fishes", "toothfishes"),
  fish_large = c("toothfishes", "shelf and coastal fishes"),
  fish_small = c("mesopelagic fishes", "bathypelagic fishes"),
  krill = c("antarctic krill"),
  ltl = c("antarctic krill", "other krill", "mesozooplankton",
          "other macrozooplankton", "salps"),
  consumers = c("mesopelagic fishes", "bathypelagic fishes",
                "shelf and coastal fishes", "flying birds", "small divers",
                "squids", "toothfishes", "leopard seals", "medium divers",
                "large divers", "minke whales", "orca", "sperm whales",
                "baleen whales"),
  mid_trophic_prey = c("mesopelagic fishes", "bathypelagic fishes",
                       "shelf and coastal fishes", "squids"),
  apex_prey = c("mesopelagic fishes", "bathypelagic fishes",
                "shelf and coastal fishes", "toothfishes", "squids",
                "small divers", "medium divers", "large divers",
                "flying birds")
)

# Minimum individual weight (g) for size spectrum and P:B ratio calculations
# (Community P/B, NBSS slope, mean individual weight, mean max weight).
# Weight bins below this threshold are excluded from fitting/summation.
# Set to mesozooplankton w_min from trait_groups_params_vCWC_v4.csv (3.16e-08 g),
# the smallest functional group included in the LTL biomass aggregate, ensuring
# community-level metrics span the full LTL size range.
SPECTRUM_MIN_W <- 3.16227766016838e-08

# Trophic levels from McCormack et al. (2020) Prydz Bay Ecopath, Table 2
TROPHIC_LEVELS <- c(
  "mesozooplankton" = 3.272, "other krill" = 2.398,
  "other macrozooplankton" = 3.231, "antarctic krill" = 2.398,
  "salps" = 2.284, "mesopelagic fishes" = 3.539,
  "bathypelagic fishes" = 4.055, "shelf and coastal fishes" = 4.281,
  "toothfishes" = 4.966, "flying birds" = 4.103,
  "small divers" = 3.787, "medium divers" = 4.999,
  "large divers" = 5.075, "leopard seals" = 4.858,
  "squids" = 4.336, "minke whales" = 3.955,
  "orca" = 5.301, "sperm whales" = 5.342,
  "baleen whales" = 3.867
)

###############################################################################
# Helper Functions
###############################################################################

`%||%` <- function(x, y) if (is.null(x)) y else x

safe_time_extract <- function(x, time_range = NULL) {
  if (is.null(x)) return(NA)
  if (is.matrix(x) || is.data.frame(x)) {
    if (is.null(time_range)) time_range <- seq_len(nrow(x))
    vals <- if (ncol(x) == 1) x[time_range, 1] else rowSums(x[time_range, , drop = FALSE])
  } else {
    if (is.null(time_range)) time_range <- seq_along(x)
    vals <- x[time_range]
  }
  return(as.numeric(vals))
}

###############################################################################
# Metric Calculation Functions
###############################################################################

calculate_group_biomass <- function(sim, time_range = NULL, species_group = NULL) {
  biomass <- getBiomass(sim)
  if (is.null(time_range)) time_range <- seq_len(nrow(biomass))
  if (is.null(species_group)) {
    total <- rowSums(biomass[time_range, , drop = FALSE])
  } else {
    valid_sp <- species_group[species_group %in% colnames(biomass)]
    if (length(valid_sp) == 0) return(NA)
    total <- rowSums(biomass[time_range, valid_sp, drop = FALSE])
  }
  return(mean(total, na.rm = TRUE))
}

# =========================================================================
# Size Spectrum Slope & Intercept
# Linear regression of log10(biomass density) ~ log10(w) on the community
# spectrum. All species are included but only weight bins >= min_w are
# used for fitting, filtering out individuals smaller than Antarctic krill.
# =========================================================================
calculate_spectrum_slope_intercept <- function(sim, time_range = NULL,
                                                min_w = SPECTRUM_MIN_W) {
  tryCatch({
    params <- sim@params
    w <- params@w
    dw <- params@dw
    times <- as.numeric(dimnames(sim@n)$time)
    if (is.null(time_range)) time_range <- seq_along(times)
    n_sp <- dim(sim@n)[2]

    # Weight-bin filter: only include bins >= min_w
    w_keep <- which(w >= min_w)
    if (length(w_keep) < 3) return(c(slope = NA, intercept = NA))

    # Build community biomass spectrum from ALL species
    # Biomass density at weight w: sum over all species of N(w) * w
    slopes <- numeric(length(time_range))
    intercepts <- numeric(length(time_range))
    for (i in seq_along(time_range)) {
      t_idx <- time_range[i]
      biomass_density <- colSums(sim@n[t_idx, , ]) * w

      # Only fit over retained weight bins where biomass > 0
      valid <- w_keep[biomass_density[w_keep] > 0]
      if (length(valid) < 3) {
        slopes[i] <- NA; intercepts[i] <- NA; next
      }
      log_w <- log10(w[valid])
      log_b <- log10(biomass_density[valid])
      fit <- lm(log_b ~ log_w)
      slopes[i] <- coef(fit)[2]
      intercepts[i] <- coef(fit)[1]
    }
    return(c(slope = mean(slopes, na.rm = TRUE),
             intercept = mean(intercepts, na.rm = TRUE)))
  }, error = function(e) {
    warning(sprintf("Spectrum slope/intercept calculation failed: %s", e$message))
    return(c(slope = NA, intercept = NA))
  })
}

# =========================================================================
# Size Spectrum Exponent via MLE for Bounded Power Law (PLBbin)
#
# Fits a bounded power law (PLB) to the community biomass size spectrum
# using maximum likelihood estimation that accounts for the bin structure
# of the data, following Edwards et al. (2017).
#
# The bounded power law assumes:
#   f(x) proportional to x^b,  x_min <= x <= x_max
#
# For binned data with bin boundaries [w_j, w_{j+1}], the probability
# of an observation falling in bin j is:
#   P_j(b) = (w_{j+1}^(b+1) - w_j^(b+1)) / (x_max^(b+1) - x_min^(b+1))
# when b != -1, and using log ratios when b = -1.
#
# The log-likelihood is:
#   LL(b) = sum_j [ c_j * log(P_j(b)) ]
# where c_j is the count (effective number of individuals) in bin j.
#
# This method is more robust than OLS regression on log-binned data,
# particularly for the largest size classes where bins may be sparsely
# populated, and avoids the systematic biases documented by Edwards et al.
#
# Reference:
#   Edwards, A.M. et al. (2017). Accounting for the bin structure of
#   data removes bias when fitting size spectra. Methods Ecol. Evol.
#   8:57-67. doi:10.1111/2041-210X.12641
# =========================================================================
calculate_spectrum_mle_exponent <- function(sim, time_range = NULL,
                                             min_w = SPECTRUM_MIN_W) {
  tryCatch({
    params <- sim@params
    w <- params@w
    dw <- params@dw
    times <- as.numeric(dimnames(sim@n)$time)
    if (is.null(time_range)) time_range <- seq_along(times)

    # Weight-bin filter: only include bins >= min_w
    n_w <- length(w)
    w_keep <- which(w >= min_w)
    if (length(w_keep) < 3) return(NA)

    # Bin boundaries: lower edge = w - dw/2, upper edge = w + dw/2
    # (mizer bins are log-spaced; boundaries approximate the bin edges)
    w_lower <- w - dw / 2
    w_upper <- w + dw / 2
    w_lower[w_lower <= 0] <- w[1] / 2  # ensure positive lower bound

    # Negative log-likelihood for bounded power law (binned data)
    neg_log_lik <- function(b, counts, w_lo, w_hi, x_min, x_max) {
      if (abs(b + 1) < 1e-6) {
        # b ≈ -1: use log form
        p_j <- (log(w_hi) - log(w_lo)) / (log(x_max) - log(x_min))
      } else {
        bp1 <- b + 1
        p_j <- (w_hi^bp1 - w_lo^bp1) / (x_max^bp1 - x_min^bp1)
      }
      p_j[p_j <= 0] <- .Machine$double.xmin
      p_j <- p_j / sum(p_j)  # renormalize for numerical stability
      -sum(counts * log(p_j))
    }

    exponents <- numeric(length(time_range))
    for (i in seq_along(time_range)) {
      t_idx <- time_range[i]

      # Community abundance spectrum (number density) from ALL species
      n_community <- colSums(sim@n[t_idx, , ])

      # Effective counts per bin — restricted to retained weight bins
      counts <- n_community * dw
      valid <- w_keep[counts[w_keep] > 0]
      if (length(valid) < 3) { exponents[i] <- NA; next }

      c_valid <- counts[valid]
      wl <- w_lower[valid]
      wh <- w_upper[valid]
      x_min <- min(wl)
      x_max <- max(wh)

      # Optimize: search for b in a wide range
      result <- tryCatch(
        optim(par = -1.5, fn = neg_log_lik,
              counts = c_valid, w_lo = wl, w_hi = wh,
              x_min = x_min, x_max = x_max,
              method = "Brent", lower = -5, upper = 1),
        error = function(e) NULL
      )
      exponents[i] <- if (!is.null(result)) result$par else NA
    }
    return(mean(exponents, na.rm = TRUE))
  }, error = function(e) {
    warning(sprintf("MLE spectrum exponent calculation failed: %s", e$message))
    return(NA)
  })
}

# =========================================================================
# Size Spectrum Exponent via LCD — Logarithmic Cumulative Distribution
#
# Classical method: sorts effective individual counts by body size,
# computes the survival function P(X >= x) at each bin midpoint, then
# fits OLS on log10(P) ~ log10(x). For a bounded power law p(x) ~ x^b,
# the CDF slope equals b + 1, so the exponent b = slope - 1.
#
# This is "Method 1" in Edwards et al. (2017) — widely used in marine
# ecology but known to produce biased estimates. Included here for
# comparison with the recommended PLBbin method.
#
# Reference:
#   Edwards, A.M. et al. (2017). Methods Ecol. Evol. 8:57-67.
# =========================================================================
calculate_spectrum_lcd_exponent <- function(sim, time_range = NULL,
                                             min_w = SPECTRUM_MIN_W) {
  tryCatch({
    params <- sim@params
    w <- params@w
    dw <- params@dw
    times <- as.numeric(dimnames(sim@n)$time)
    if (is.null(time_range)) time_range <- seq_along(times)

    w_keep <- which(w >= min_w)
    if (length(w_keep) < 3) return(NA)

    exponents <- numeric(length(time_range))
    for (i in seq_along(time_range)) {
      t_idx <- time_range[i]

      # Effective counts per bin (all species summed)
      counts <- colSums(sim@n[t_idx, , ]) * dw
      valid <- w_keep[counts[w_keep] > 0]
      if (length(valid) < 3) { exponents[i] <- NA; next }

      c_valid <- counts[valid]
      w_valid <- w[valid]

      # Survival function: P(X >= w_j) = sum_{k>=j} c_k / sum(c)
      total <- sum(c_valid)
      # Sort by ascending weight (should already be, but ensure)
      ord <- order(w_valid)
      c_sorted <- c_valid[ord]
      w_sorted <- w_valid[ord]
      cum_from_above <- rev(cumsum(rev(c_sorted))) / total

      log_w <- log10(w_sorted)
      log_p <- log10(cum_from_above)

      # Drop any -Inf (P = 0 at the very end)
      finite <- is.finite(log_p)
      if (sum(finite) < 3) { exponents[i] <- NA; next }

      fit <- lm(log_p[finite] ~ log_w[finite])
      cdf_slope <- coef(fit)[2]
      exponents[i] <- cdf_slope - 1  # b = CDF slope - 1
    }
    return(mean(exponents, na.rm = TRUE))
  }, error = function(e) {
    warning(sprintf("LCD spectrum exponent calculation failed: %s", e$message))
    return(NA)
  })
}

# =========================================================================
# Size Spectrum Slope via LBNbiom — Log-Binned Normalised Biomass Spectrum
#
# "Method 5" (LBNbiom) from Edwards et al. (2017).
# Re-bins mizer weight classes into octave bins (log2, factor-of-2),
# computes total biomass per octave, normalises by the linear bin width,
# and fits OLS on log10(NBS) ~ log10(geometric midpoint).
#
# Octave re-binning distinguishes this from the existing OLS slope
# (which uses native mizer bins directly). A common approach in
# size-spectrum ecology, but biased for sparse large-size bins.
# Included for comparison with PLBbin.
#
# Reference:
#   Edwards, A.M. et al. (2017). Methods Ecol. Evol. 8:57-67.
# =========================================================================
calculate_spectrum_lbnbiom_slope <- function(sim, time_range = NULL,
                                              min_w = SPECTRUM_MIN_W) {
  tryCatch({
    params <- sim@params
    w <- params@w
    dw <- params@dw
    times <- as.numeric(dimnames(sim@n)$time)
    if (is.null(time_range)) time_range <- seq_along(times)

    # ---- Define octave bins (log2, factor-of-2) ----
    w_filt <- w[w >= min_w]
    if (length(w_filt) < 3) return(NA)
    oct_lo <- floor(log2(min(w_filt)))
    oct_hi <- ceiling(log2(max(w_filt)))
    bin_breaks <- 2^(oct_lo:oct_hi)
    n_bins <- length(bin_breaks) - 1
    if (n_bins < 3) return(NA)

    # Assign each mizer weight class to an octave bin
    bin_idx <- findInterval(w, bin_breaks, rightmost.closed = TRUE)

    slopes <- numeric(length(time_range))
    for (i in seq_along(time_range)) {
      t_idx <- time_range[i]

      # Total biomass per mizer bin: N(w) * w * dw
      biomass_per_bin <- colSums(sim@n[t_idx, , ]) * w * dw

      # Aggregate biomass into octave bins
      oct_biomass <- numeric(n_bins)
      for (j in seq_along(w)) {
        b <- bin_idx[j]
        if (b >= 1 && b <= n_bins && w[j] >= min_w) {
          oct_biomass[b] <- oct_biomass[b] + biomass_per_bin[j]
        }
      }

      # Normalise by linear bin width; use geometric mean as midpoint
      oct_width <- diff(bin_breaks)
      oct_mid   <- sqrt(bin_breaks[-length(bin_breaks)] * bin_breaks[-1])
      nbs <- oct_biomass / oct_width

      valid <- which(nbs > 0)
      if (length(valid) < 3) { slopes[i] <- NA; next }

      log_w   <- log10(oct_mid[valid])
      log_nbs <- log10(nbs[valid])

      fit <- lm(log_nbs ~ log_w)
      slopes[i] <- coef(fit)[2]
    }
    return(mean(slopes, na.rm = TRUE))
  }, error = function(e) {
    warning(sprintf("LBNbiom spectrum slope calculation failed: %s", e$message))
    return(NA)
  })
}

# =========================================================================
# Large Fish Indicator — REVISED (Fish-only, 100g)
# Adapted from Zhang et al. (2015) Table 2. Uses 100g threshold applied
# to fish species only (proportion of fish biomass in individuals >100g).
# =========================================================================
calculate_lfi_100g <- function(sim, time_range = NULL,
                                threshold_w = 100, min_w = 1,
                                max_w = 1e6) {
  lfi_data <- tryCatch(
    mizer::getProportionOfLargeFish(sim, species = SPECIES_GROUPS$fish,
                                    min_w = min_w, max_w = max_w,
                                    threshold_w = threshold_w,
                                    biomass_proportion = TRUE),
    error = function(e) {
      warning(sprintf("getProportionOfLargeFish (fish 100g) failed: %s", e$message))
      NULL
    }
  )
  if (is.null(lfi_data)) return(NA)
  if (is.null(time_range)) time_range <- seq_len(NROW(lfi_data))
  if (is.matrix(lfi_data) || is.data.frame(lfi_data)) {
    vals <- if (ncol(lfi_data) == 1) lfi_data[time_range, 1] else lfi_data[time_range, ]
  } else {
    vals <- lfi_data[time_range]
  }
  return(mean(as.numeric(vals), na.rm = TRUE))
}

# =========================================================================
# Fish-specific LFI — RETAINED (original 1000g fish-only definition)
# Greenstreet et al. (2011): proportion of fish biomass above 1000g
# =========================================================================
calculate_lfi_fish_only <- function(sim, time_range = NULL,
                                     threshold_w = 1000, min_w = 10,
                                     max_w = 1e6) {
  lfi_data <- tryCatch(
    mizer::getProportionOfLargeFish(sim, species = SPECIES_GROUPS$fish,
                                    min_w = min_w, max_w = max_w,
                                    threshold_w = threshold_w,
                                    biomass_proportion = TRUE),
    error = function(e) NULL
  )
  if (is.null(lfi_data)) return(NA)
  if (is.null(time_range)) time_range <- seq_len(NROW(lfi_data))
  if (is.matrix(lfi_data) || is.data.frame(lfi_data)) {
    vals <- if (ncol(lfi_data) == 1) lfi_data[time_range, 1] else lfi_data[time_range, ]
  } else {
    vals <- lfi_data[time_range]
  }
  return(mean(as.numeric(vals), na.rm = TRUE))
}

calculate_mean_weight_mizer <- function(sim, time_range = NULL,
                                        species_group = NULL,
                                        min_w = 1, max_w = 1e8) {
  mw_data <- tryCatch(
    mizer::getMeanWeight(sim, species = species_group,
                         min_w = min_w, max_w = max_w),
    error = function(e) NULL
  )
  if (is.null(mw_data)) return(NA)
  vals <- safe_time_extract(mw_data, time_range)
  return(mean(vals, na.rm = TRUE))
}

calculate_mean_max_weight_mizer <- function(sim, time_range = NULL,
                                            species_group = NULL) {
  mmw_data <- tryCatch(
    mizer::getMeanMaxWeight(sim, species = species_group, measure = "biomass"),
    error = function(e) NULL
  )
  if (is.null(mmw_data)) return(NA)
  vals <- safe_time_extract(mmw_data, time_range)
  return(mean(vals, na.rm = TRUE))
}

# =========================================================================
# Shannon-Wiener Diversity Index H'
# H' = -sum(p_i * ln(p_i)) where p_i = B_i / sum(B)
# Zhang et al. (2015) Table 2; Shannon (1948)
# =========================================================================
calculate_shannon_diversity <- function(sim, time_range = NULL) {
  biomass <- getBiomass(sim)
  if (is.null(time_range)) time_range <- seq_len(nrow(biomass))
  h_values <- numeric(length(time_range))
  for (i in seq_along(time_range)) {
    t <- time_range[i]
    b <- biomass[t, ]
    b <- b[!is.na(b) & b > 0]
    if (length(b) < 2) { h_values[i] <- NA; next }
    p <- b / sum(b)
    h_values[i] <- -sum(p * log(p))
  }
  return(mean(h_values, na.rm = TRUE))
}

# =========================================================================
# W-statistic (Clarke & Warwick ABC Curves)
# W = sum(B_cum - N_cum) / [50 * (S - 1)]
# Species ranked by w_inf (largest first)
# Zhang et al. (2015) Table 2; Clarke & Warwick (1994)
# =========================================================================
calculate_w_statistic <- function(sim, time_range = NULL) {
  biomass <- getBiomass(sim)
  if (is.null(time_range)) time_range <- seq_len(nrow(biomass))
  sp_names <- colnames(biomass)
  if (length(sp_names) < 2) return(NA)
  params <- sim@params
  w_inf <- params@species_params$w_inf
  names(w_inf) <- as.character(params@species_params$species)
  valid_sp <- sp_names[sp_names %in% names(w_inf)]
  if (length(valid_sp) < 2) return(NA)
  rank_order <- order(w_inf[valid_sp], decreasing = TRUE)
  ranked_sp <- valid_sp[rank_order]
  dw <- params@dw
  w_values <- numeric(length(time_range))
  for (i in seq_along(time_range)) {
    t_idx <- time_range[i]
    b_sp <- biomass[t_idx, ranked_sp]
    n_sp <- numeric(length(ranked_sp))
    names(n_sp) <- ranked_sp
    for (j in seq_along(ranked_sp)) {
      sp <- ranked_sp[j]
      sp_idx <- which(as.character(params@species_params$species) == sp)
      if (length(sp_idx) == 1) {
        n_sp[j] <- sum(sim@n[t_idx, sp_idx, ] * dw, na.rm = TRUE)
      }
    }
    keep <- (b_sp > 0) | (n_sp > 0)
    if (sum(keep) < 2) { w_values[i] <- NA; next }
    b_sp <- b_sp[keep]; n_sp <- n_sp[keep]
    S_eff <- length(b_sp)
    b_cum <- cumsum(b_sp) / sum(b_sp) * 100
    n_cum <- cumsum(n_sp) / sum(n_sp) * 100
    w_values[i] <- sum(b_cum - n_cum) / (50 * (S_eff - 1))
  }
  return(mean(w_values, na.rm = TRUE))
}

# =========================================================================
# Production:Biomass Ratio (P/B)
# P/B = sum[g(w) * N(w) * dw] / sum[N(w) * w * dw]
# where g(w) is the somatic growth rate from mizer's energy budget
# (getEGrowth). Computed for all species.
#
# Ecological significance:
#   P/B reflects the community-level biomass turnover rate. Higher P/B
#   indicates a community dominated by fast-growing, small-bodied
#   organisms; lower P/B indicates dominance by large, slow-growing
#   species. Selective removal of large predators (e.g., whaling)
#   typically increases community P/B as biomass shifts to faster-
#   turnover species. Changes in P/B indicate restructuring of energy
#   flow through the food web.
#
# Implementation note:
#   For therMizer models, getEGrowth is called with a time argument
#   (t) to allow temperature-dependent rate functions to look up the
#   correct environmental conditions. Falls back to standard mizer
#   calling convention if the time argument is not accepted.
#
# References:
#   Brey, T. (2012). A multi-parameter artificial neural network model
#     to estimate macrobenthic invertebrate productivity and production.
#     Limnol. Oceanogr. Methods 10:581-589.
#   Jennings, S. et al. (2002). Long-term trends in the trophic
#     structure of the North Sea fish community. Mar. Biol. 141:1085-97.
# =========================================================================
calculate_production_biomass_ratio <- function(sim, time_range = NULL,
                                               species_group = NULL,
                                               min_w = SPECTRUM_MIN_W) {
  params <- sim@params
  w <- params@w
  dw <- params@dw
  times <- as.numeric(dimnames(sim@n)$time)

  sp_all <- as.character(params@species_params$species)
  if (!is.null(species_group)) {
    sp_indices <- which(sp_all %in% species_group)
  } else {
    sp_indices <- seq_along(sp_all)  # All species
  }
  if (length(sp_indices) == 0) return(NA)

  # Weight-bin filter: only include bins >= min_w
  w_keep <- which(w >= min_w)
  if (length(w_keep) == 0) return(NA)
  if (is.null(time_range)) time_range <- seq_along(times)

  pb_values <- numeric(length(time_range))
  for (i in seq_along(time_range)) {
    t_idx <- time_range[i]

    # Extract abundance and resource spectra at this timestep
    n_at_t <- sim@n[t_idx, , , drop = FALSE]
    dim(n_at_t) <- dim(sim@n)[2:3]
    n_pp_at_t <- sim@n_pp[t_idx, ]

    # Get somatic growth rates — try with time arg (therMizer) first
    eg <- tryCatch(
      getEGrowth(params, n = n_at_t, n_pp = n_pp_at_t, t = times[t_idx]),
      error = function(e) {
        tryCatch(
          getEGrowth(params, n = n_at_t, n_pp = n_pp_at_t),
          error = function(e2) NULL
        )
      }
    )
    if (is.null(eg)) { pb_values[i] <- NA; next }

    total_production <- 0
    total_biomass <- 0
    for (j in sp_indices) {
      # Production: growth_rate * abundance * bin_width (filtered bins)
      total_production <- total_production +
        sum(eg[j, w_keep] * n_at_t[j, w_keep] * dw[w_keep], na.rm = TRUE)
      # Biomass: abundance * weight * bin_width (filtered bins)
      total_biomass <- total_biomass +
        sum(n_at_t[j, w_keep] * w[w_keep] * dw[w_keep], na.rm = TRUE)
    }
    pb_values[i] <- if (total_biomass > 0) total_production / total_biomass else NA
  }
  return(mean(pb_values, na.rm = TRUE))
}

calculate_mean_tl <- function(sim, time_range = NULL,
                              tl_vector = TROPHIC_LEVELS,
                              consumers_only = TRUE) {
  biomass <- getBiomass(sim)
  if (is.null(time_range)) time_range <- seq_len(nrow(biomass))
  sp_names <- colnames(biomass)
  valid_sp <- sp_names[sp_names %in% names(tl_vector)]
  if (consumers_only) valid_sp <- valid_sp[tl_vector[valid_sp] >= 3.0]
  if (length(valid_sp) == 0) return(NA)
  mtl_values <- numeric(length(time_range))
  for (i in seq_along(time_range)) {
    t <- time_range[i]
    b <- biomass[t, valid_sp]
    tl <- tl_vector[valid_sp]
    total_b <- sum(b)
    mtl_values[i] <- if (total_b > 0) sum(b * tl) / total_b else NA
  }
  return(mean(mtl_values, na.rm = TRUE))
}

calculate_htl_indicator <- function(sim, time_range = NULL,
                                    tl_threshold = 4.0,
                                    tl_vector = TROPHIC_LEVELS) {
  biomass <- getBiomass(sim)
  if (is.null(time_range)) time_range <- seq_len(nrow(biomass))
  sp_names <- colnames(biomass)
  valid_sp <- sp_names[sp_names %in% names(tl_vector)]
  high_tl_sp <- valid_sp[tl_vector[valid_sp] >= tl_threshold]
  consumer_sp <- valid_sp[tl_vector[valid_sp] >= 3.0]
  if (length(high_tl_sp) == 0 || length(consumer_sp) == 0) return(NA)
  htl_values <- numeric(length(time_range))
  for (i in seq_along(time_range)) {
    t <- time_range[i]
    htl_values[i] <- sum(biomass[t, high_tl_sp]) / sum(biomass[t, consumer_sp])
  }
  return(mean(htl_values, na.rm = TRUE))
}

calculate_predator_prey_ratio <- function(sim, time_range = NULL,
                                          predator_group = NULL,
                                          prey_group = NULL) {
  if (is.null(predator_group)) predator_group <- SPECIES_GROUPS$apex_predators
  if (is.null(prey_group)) prey_group <- SPECIES_GROUPS$apex_prey
  pred_b <- calculate_group_biomass(sim, time_range, predator_group)
  prey_b <- calculate_group_biomass(sim, time_range, prey_group)
  if (is.na(pred_b) || is.na(prey_b) || prey_b == 0) return(NA)
  return(pred_b / prey_b)
}

calculate_consumer_ltl_ratio <- function(sim, time_range = NULL) {
  consumer_b <- calculate_group_biomass(sim, time_range, SPECIES_GROUPS$consumers)
  ltl_b <- calculate_group_biomass(sim, time_range, SPECIES_GROUPS$ltl)
  if (is.na(consumer_b) || is.na(ltl_b) || ltl_b == 0) return(NA)
  return(consumer_b / ltl_b)
}

calculate_exploitation_rate <- function(sim, time_range = NULL,
                                        species_group = NULL) {
  catch_data <- tryCatch(getYield(sim), error = function(e) NULL)
  biomass_data <- getBiomass(sim)
  if (is.null(catch_data)) return(0)
  if (is.null(time_range)) time_range <- seq_len(nrow(biomass_data))
  sp_names <- colnames(biomass_data)
  if (is.null(species_group)) {
    total_catch <- colSums(catch_data[time_range, , drop = FALSE])
    species_group <- names(total_catch[total_catch > 0])
    if (length(species_group) == 0) return(0)
  }
  valid_sp <- species_group[species_group %in% sp_names]
  if (length(valid_sp) == 0) return(NA)
  exploitation_rates <- numeric(length(time_range))
  for (i in seq_along(time_range)) {
    t <- time_range[i]
    total_catch <- sum(catch_data[t, valid_sp, drop = TRUE], na.rm = TRUE)
    total_biomass <- sum(biomass_data[t, valid_sp, drop = TRUE], na.rm = TRUE)
    exploitation_rates[i] <- if (total_biomass > 0) total_catch / total_biomass else NA
  }
  return(mean(exploitation_rates, na.rm = TRUE))
}

calculate_whale_exploitation <- function(sim, time_range = NULL) {
  calculate_exploitation_rate(sim, time_range, SPECIES_GROUPS$all_whales)
}

calculate_krill_exploitation <- function(sim, time_range = NULL) {
  calculate_exploitation_rate(sim, time_range, SPECIES_GROUPS$krill)
}

###############################################################################
# Paired Ensemble Processing
###############################################################################

extract_simulation_metrics <- function(sim, decades_df = DECADES) {
  times <- as.numeric(dimnames(sim@n)$time)
  results <- list()
  for (i in seq_len(nrow(decades_df))) {
    decade <- decades_df[i, ]
    time_range <- which(times >= decade$start & times <= decade$end)
    if (length(time_range) == 0) next
    spec <- calculate_spectrum_slope_intercept(sim, time_range)
    metrics <- data.frame(
      decade = decade$label, start_year = decade$start, end_year = decade$end,
      # Category A: Biomass
      total_biomass = calculate_group_biomass(sim, time_range),
      whale_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$all_whales),
      baleen_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$baleen_whales),
      sperm_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$sperm_whales),
      minke_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$minke_whales),
      seal_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$seals),
      fish_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$fish),
      krill_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$krill),
      ltl_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$ltl),
      apex_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$apex_predators),
      # Category B: Structural
      spectrum_slope = unname(spec["slope"]),
      spectrum_intercept = unname(spec["intercept"]),
      mean_tl = calculate_mean_tl(sim, time_range, consumers_only = TRUE),
      htl_indicator = calculate_htl_indicator(sim, time_range),
      large_fish_indicator = calculate_lfi_100g(sim, time_range),
      fish_lfi = calculate_lfi_fish_only(sim, time_range),
      shannon_diversity = calculate_shannon_diversity(sim, time_range),
      w_statistic = calculate_w_statistic(sim, time_range),
      spectrum_mle_exponent = calculate_spectrum_mle_exponent(sim, time_range),
      spectrum_lcd_exponent = calculate_spectrum_lcd_exponent(sim, time_range),
      spectrum_lbnbiom_slope = calculate_spectrum_lbnbiom_slope(sim, time_range),
      production_biomass_ratio = calculate_production_biomass_ratio(sim, time_range),
      fish_pb_ratio = calculate_production_biomass_ratio(sim, time_range,
                                                         species_group = SPECIES_GROUPS$fish),
      marine_mammal_pb_ratio = calculate_production_biomass_ratio(sim, time_range,
                                                                  species_group = SPECIES_GROUPS$marine_mammals),
      mean_weight = calculate_mean_weight_mizer(sim, time_range,
                                                 species_group = SPECIES_GROUPS$consumers),
      mean_max_weight = calculate_mean_max_weight_mizer(sim, time_range,
                                                         species_group = SPECIES_GROUPS$consumers),
      predator_prey_ratio = calculate_predator_prey_ratio(sim, time_range),
      consumer_ltl_ratio = calculate_consumer_ltl_ratio(sim, time_range),
      # Category C: Exploitation
      exploitation_total = calculate_exploitation_rate(sim, time_range),
      exploitation_whale = calculate_whale_exploitation(sim, time_range),
      exploitation_krill = calculate_krill_exploitation(sim, time_range),
      stringsAsFactors = FALSE
    )
    results[[i]] <- metrics
  }
  return(bind_rows(results))
}

extract_b0_metrics <- function(sim) {
  times <- as.numeric(dimnames(sim@n)$time)
  time_range <- which(times >= B0_PERIOD[1] & times <= B0_PERIOD[2])
  if (length(time_range) == 0) { warning("No B0 time steps"); return(NULL) }
  spec <- calculate_spectrum_slope_intercept(sim, time_range)
  c(
    total_biomass = calculate_group_biomass(sim, time_range),
    whale_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$all_whales),
    baleen_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$baleen_whales),
    sperm_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$sperm_whales),
    minke_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$minke_whales),
    seal_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$seals),
    fish_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$fish),
    krill_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$krill),
    ltl_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$ltl),
    apex_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$apex_predators),
    spectrum_slope = unname(spec["slope"]),
    spectrum_intercept = unname(spec["intercept"]),
    mean_tl = calculate_mean_tl(sim, time_range, consumers_only = TRUE),
    htl_indicator = calculate_htl_indicator(sim, time_range),
    large_fish_indicator = calculate_lfi_100g(sim, time_range),
    fish_lfi = calculate_lfi_fish_only(sim, time_range),
    shannon_diversity = calculate_shannon_diversity(sim, time_range),
    w_statistic = calculate_w_statistic(sim, time_range),
    spectrum_mle_exponent = calculate_spectrum_mle_exponent(sim, time_range),
    spectrum_lcd_exponent = calculate_spectrum_lcd_exponent(sim, time_range),
    spectrum_lbnbiom_slope = calculate_spectrum_lbnbiom_slope(sim, time_range),
    production_biomass_ratio = calculate_production_biomass_ratio(sim, time_range),
    fish_pb_ratio = calculate_production_biomass_ratio(sim, time_range,
                                                       species_group = SPECIES_GROUPS$fish),
    marine_mammal_pb_ratio = calculate_production_biomass_ratio(sim, time_range,
                                                                species_group = SPECIES_GROUPS$marine_mammals),
    mean_weight = calculate_mean_weight_mizer(sim, time_range,
                                               species_group = SPECIES_GROUPS$consumers),
    mean_max_weight = calculate_mean_max_weight_mizer(sim, time_range,
                                                       species_group = SPECIES_GROUPS$consumers),
    predator_prey_ratio = calculate_predator_prey_ratio(sim, time_range),
    consumer_ltl_ratio = calculate_consumer_ltl_ratio(sim, time_range)
  )
}

###############################################################################
# Checkpoint Functions for Progressive Saving and Resume
###############################################################################

save_checkpoint <- function(checkpoint_dir, sim_num, fishing_results, climate_results, b0_results) {
  if (!dir.exists(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  }
  checkpoint_file <- file.path(checkpoint_dir, sprintf("checkpoint_sim_%04d.rds", sim_num))
  checkpoint_data <- list(
    last_sim = sim_num,
    fishing_results = fishing_results,
    climate_results = climate_results,
    b0_results = b0_results,
    timestamp = Sys.time()
  )
  saveRDS(checkpoint_data, checkpoint_file)
  cat(sprintf("  [Checkpoint saved: sim %d]\n", sim_num))
}

find_checkpoints <- function(checkpoint_dir) {
  if (!dir.exists(checkpoint_dir)) return(character(0))
  checkpoint_files <- list.files(checkpoint_dir, pattern = "^checkpoint_sim_\\d{4}\\.rds$", full.names = TRUE)
  if (length(checkpoint_files) == 0) return(character(0))
  sim_nums <- as.integer(sub(".*checkpoint_sim_(\\d{4})\\.rds", "\\1", basename(checkpoint_files)))
  checkpoint_files[order(sim_nums)]
}

get_resume_point <- function(checkpoint_dir) {
  checkpoints <- find_checkpoints(checkpoint_dir)
  if (length(checkpoints) == 0) {
    return(list(resume_from = 0, checkpoint_file = NULL))
  }
  last_checkpoint <- checkpoints[length(checkpoints)]
  checkpoint_data <- readRDS(last_checkpoint)
  list(resume_from = checkpoint_data$last_sim, checkpoint_file = last_checkpoint, checkpoint_data = checkpoint_data)
}

merge_checkpoints <- function(checkpoint_dir, current_fishing, current_climate, current_b0) {
  checkpoints <- find_checkpoints(checkpoint_dir)
  all_fishing <- current_fishing
  all_climate <- current_climate
  all_b0 <- current_b0
  if (length(checkpoints) > 0) {
    cat(sprintf("\nMerging %d checkpoint files...\n", length(checkpoints)))
    for (cp_file in checkpoints) {
      cp_data <- readRDS(cp_file)
      all_fishing <- c(all_fishing, cp_data$fishing_results)
      all_climate <- c(all_climate, cp_data$climate_results)
      all_b0 <- c(all_b0, cp_data$b0_results)
    }
  }
  list(
    fishing_raw = bind_rows(all_fishing),
    climate_raw = bind_rows(all_climate),
    b0_reference = bind_rows(all_b0)
  )
}

cleanup_checkpoints <- function(checkpoint_dir) {
  if (!dir.exists(checkpoint_dir)) return(invisible(NULL))
  checkpoints <- find_checkpoints(checkpoint_dir)
  if (length(checkpoints) > 0) {
    cat(sprintf("Cleaning up %d checkpoint files...\n", length(checkpoints)))
    file.remove(checkpoints)
    if (length(list.files(checkpoint_dir)) == 0) {
      unlink(checkpoint_dir, recursive = TRUE)
    }
  }
  invisible(NULL)
}

process_paired_ensemble <- function(fishing_file, climate_file, max_sims = NULL, checkpoint_interval = 10) {
  cat("=============================================================\n")
  cat("Processing PAIRED ensemble (with checkpoint support)\n")
  cat("=============================================================\n\n")
  checkpoint_dir <- file.path(OUTPUT_DIR_LARGE, "checkpoints")
  resume_info <- get_resume_point(checkpoint_dir)
  start_sim <- resume_info$resume_from
  if (start_sim > 0) {
    cat(sprintf("*** RESUMING from checkpoint at simulation %d ***\n\n", start_sim))
  } else {
    cat("Starting fresh (no checkpoints found)\n\n")
  }
  cat("Loading fishing ensemble...\n")
  mc_fish <- readRDS(fishing_file)
  cat("Loading climate-only ensemble...\n")
  mc_clim <- readRDS(climate_file)
  extract_sims <- function(mc) {
    if ("simulations" %in% names(mc)) return(mc$simulations)
    if (is.list(mc) && inherits(mc[[1]], "MizerSim")) return(mc)
    stop("Unrecognized ensemble structure")
  }
  fish_sims <- extract_sims(mc_fish)
  clim_sims <- extract_sims(mc_clim)
  n_sims <- min(length(fish_sims), length(clim_sims))
  if (!is.null(max_sims)) n_sims <- min(n_sims, max_sims)
  cat(sprintf("  Processing %d PAIRED simulations\n", n_sims))
  cat(sprintf("  Checkpoint interval: every %d simulations\n", checkpoint_interval))
  cat(sprintf("  Starting from simulation: %d\n\n", start_sim + 1))
  fishing_results <- climate_results <- b0_results <- list()
  pb <- txtProgressBar(min = start_sim, max = n_sims, style = 3)
  for (i in (start_sim + 1):n_sims) {
    setTxtProgressBar(pb, i)
    fish_metrics <- tryCatch(extract_simulation_metrics(fish_sims[[i]]), error = function(e) NULL)
    clim_metrics <- tryCatch(extract_simulation_metrics(clim_sims[[i]]), error = function(e) NULL)
    b0_ref <- tryCatch(extract_b0_metrics(clim_sims[[i]]), error = function(e) NULL)
    if (!is.null(fish_metrics) && nrow(fish_metrics) > 0) {
      fish_metrics$sim_id <- i
      fishing_results[[length(fishing_results) + 1]] <- fish_metrics
    }
    if (!is.null(clim_metrics) && nrow(clim_metrics) > 0) {
      clim_metrics$sim_id <- i
      climate_results[[length(climate_results) + 1]] <- clim_metrics
    }
    if (!is.null(b0_ref)) {
      b0_df <- as.data.frame(t(b0_ref))
      b0_df$sim_id <- i
      b0_results[[length(b0_results) + 1]] <- b0_df
    }
    if (i %% checkpoint_interval == 0) {
      save_checkpoint(checkpoint_dir, i, fishing_results, climate_results, b0_results)
      fishing_results <- climate_results <- b0_results <- list()
    }
  }
  close(pb)
  cat(sprintf("\n  Completed processing loop\n"))
  cat("\nMerging checkpoint files and final results...\n")
  final_results <- merge_checkpoints(checkpoint_dir, fishing_results, climate_results, b0_results)
  cat(sprintf("  Final data: %d fishing, %d climate-only, %d B0 refs\n",
              nrow(final_results$fishing_raw), nrow(final_results$climate_raw), nrow(final_results$b0_reference)))
  cleanup_checkpoints(checkpoint_dir)
  return(final_results)
}

###############################################################################
# Empirical Reference Thresholds (B0 Envelope)
###############################################################################

derive_empirical_thresholds <- function(b0_reference) {
  all_metrics <- c(BIOMASS_METRICS, STRUCTURAL_METRICS)
  thresholds <- list()
  for (col in all_metrics) {
    if (!(col %in% names(b0_reference))) next
    vals <- b0_reference[[col]]
    vals <- vals[!is.na(vals)]
    if (length(vals) < 10) {
      thresholds[[col]] <- list(median = NA, mean = NA, sd = NA,
                                q01 = NA, q05 = NA, q10 = NA, q25 = NA,
                                q75 = NA, q90 = NA, q95 = NA, q99 = NA)
      next
    }
    thresholds[[col]] <- list(
      median = median(vals), mean = mean(vals), sd = sd(vals),
      q01 = quantile(vals, 0.01, names = FALSE),
      q05 = quantile(vals, 0.05, names = FALSE),
      q10 = quantile(vals, 0.10, names = FALSE),
      q25 = quantile(vals, 0.25, names = FALSE),
      q75 = quantile(vals, 0.75, names = FALSE),
      q90 = quantile(vals, 0.90, names = FALSE),
      q95 = quantile(vals, 0.95, names = FALSE),
      q99 = quantile(vals, 0.99, names = FALSE)
    )
  }
  cat("\nEmpirical B0 reference thresholds:\n")
  cat("  BIOMASS METRICS (median [Q05-Q95]):\n")
  for (col in BIOMASS_METRICS) {
    th <- thresholds[[col]]
    if (!is.null(th) && !is.na(th$median))
      cat(sprintf("    %-25s: %10.4f [%10.4f - %10.4f]\n", col, th$median, th$q05, th$q95))
  }
  cat("  STRUCTURAL METRICS (median [Q05-Q95] = natural range envelope):\n")
  for (col in STRUCTURAL_METRICS) {
    th <- thresholds[[col]]
    if (!is.null(th) && !is.na(th$median))
      cat(sprintf("    %-25s: %10.4f [%10.4f - %10.4f]\n", col, th$median, th$q05, th$q95))
  }
  return(thresholds)
}

###############################################################################
# Paired Comparison Calculation
###############################################################################

compute_paired_comparisons <- function(fishing_raw, climate_raw, b0_reference,
                                       empirical_thresholds) {
  cat("\nComputing paired comparisons...\n")
  paired <- fishing_raw %>%
    inner_join(climate_raw, by = c("sim_id", "decade", "start_year", "end_year"),
               suffix = c("_fish", "_clim"))
  paired <- paired %>%
    inner_join(b0_reference, by = "sim_id", suffix = c("", "_b0"))
  cat(sprintf("  Paired records: %d (%d sims x %d decades)\n",
              nrow(paired), length(unique(paired$sim_id)),
              length(unique(paired$decade))))

  # Category A: Biomass — RATIOS
  for (metric in BIOMASS_METRICS) {
    fish_col <- paste0(metric, "_fish")
    clim_col <- paste0(metric, "_clim")
    b0_col <- metric
    if (!(fish_col %in% names(paired))) next
    paired[[paste0(metric, "_exploit_ratio")]] <- ifelse(
      paired[[clim_col]] > 0, paired[[fish_col]] / paired[[clim_col]], NA)
    if (b0_col %in% names(paired)) {
      paired[[paste0(metric, "_absolute_ratio")]] <- ifelse(
        paired[[b0_col]] > 0, paired[[fish_col]] / paired[[b0_col]], NA)
      paired[[paste0(metric, "_climate_ratio")]] <- ifelse(
        paired[[b0_col]] > 0, paired[[clim_col]] / paired[[b0_col]], NA)
    }
  }

  # Category B: Structural — DIFFERENCES (kept for z-score analyses)
  for (metric in STRUCTURAL_METRICS) {
    fish_col <- paste0(metric, "_fish")
    clim_col <- paste0(metric, "_clim")
    b0_col <- metric
    if (!(fish_col %in% names(paired))) next
    if (b0_col %in% names(paired)) {
      paired[[paste0(metric, "_absolute_diff")]] <- paired[[fish_col]] - paired[[b0_col]]
      paired[[paste0(metric, "_climate_diff")]] <- paired[[clim_col]] - paired[[b0_col]]
    }
    paired[[paste0(metric, "_exploit_diff")]] <- paired[[fish_col]] - paired[[clim_col]]
  }

  # Category B: Structural — envelope flags (unpaired: raw value vs B0 global quantiles)
  for (metric in STRUCTURAL_METRICS) {
    fish_col <- paste0(metric, "_fish")
    clim_col <- paste0(metric, "_clim")
    if (!(fish_col %in% names(paired))) next

    th <- empirical_thresholds[[metric]]
    if (is.null(th) || is.na(th$median)) next

    # IQR envelope (25th-75th)
    paired[[paste0(metric, "_outside_b0_50")]] <-
      paired[[fish_col]] < th$q25 | paired[[fish_col]] > th$q75
    # 80% envelope (10th-90th)
    paired[[paste0(metric, "_outside_b0_80")]] <-
      paired[[fish_col]] < th$q10 | paired[[fish_col]] > th$q90
    # 90% CI envelope (5th-95th)
    paired[[paste0(metric, "_outside_b0_90")]] <-
      paired[[fish_col]] < th$q05 | paired[[fish_col]] > th$q95
    # 98% CI envelope (1st-99th)
    paired[[paste0(metric, "_outside_b0_98")]] <-
      paired[[fish_col]] < th$q01 | paired[[fish_col]] > th$q99
    # Directional flags
    paired[[paste0(metric, "_below_b0_q05")]] <- paired[[fish_col]] < th$q05
    paired[[paste0(metric, "_above_b0_q95")]] <- paired[[fish_col]] > th$q95

    # Climate-only ensemble flags
    if (clim_col %in% names(paired)) {
      paired[[paste0(metric, "_clim_outside_b0_50")]] <-
        paired[[clim_col]] < th$q25 | paired[[clim_col]] > th$q75
      paired[[paste0(metric, "_clim_outside_b0_90")]] <-
        paired[[clim_col]] < th$q05 | paired[[clim_col]] > th$q95
      paired[[paste0(metric, "_clim_outside_b0_98")]] <-
        paired[[clim_col]] < th$q01 | paired[[clim_col]] > th$q99
    }
  }

  return(paired)
}

# NOTE: recompute_structural_flags_paired() was REMOVED.
# Structural envelope flags are now computed directly in compute_paired_comparisons()
# using the original unpaired approach (raw value vs B0 global quantiles).
# The null-subtraction display (prop_outside - expected_null) is applied at plot time.

###############################################################################
# Summary Functions
###############################################################################

summarize_biomass_ratios <- function(paired_data) {
  cat("  Summarizing biomass ratios...\n")
  comparison_types <- c("exploit", "absolute", "climate")
  all_summaries <- list()
  for (comp in comparison_types) {
    summary_rows <- list()
    for (metric in BIOMASS_METRICS) {
      ratio_col <- paste0(metric, "_", comp, "_ratio")
      if (!(ratio_col %in% names(paired_data))) next
      decade_summaries <- paired_data %>%
        group_by(decade, start_year, end_year) %>%
        summarise(
          metric_name = metric, metric_category = "biomass", comparison = comp,
          n_sims = n(),
          ratio_median = median(.data[[ratio_col]], na.rm = TRUE),
          ratio_mean = mean(.data[[ratio_col]], na.rm = TRUE),
          ratio_sd = sd(.data[[ratio_col]], na.rm = TRUE),
          ratio_q05 = quantile(.data[[ratio_col]], 0.05, na.rm = TRUE),
          ratio_q25 = quantile(.data[[ratio_col]], 0.25, na.rm = TRUE),
          ratio_q75 = quantile(.data[[ratio_col]], 0.75, na.rm = TRUE),
          ratio_q95 = quantile(.data[[ratio_col]], 0.95, na.rm = TRUE),
          n_valid = sum(!is.na(.data[[ratio_col]])),
          prop_below_090 = mean(.data[[ratio_col]] < 0.90, na.rm = TRUE),
          prop_below_075 = mean(.data[[ratio_col]] < 0.75, na.rm = TRUE),
          prop_below_040 = mean(.data[[ratio_col]] < 0.40, na.rm = TRUE),
          prop_below_020 = mean(.data[[ratio_col]] < 0.20, na.rm = TRUE),
          prop_below_054 = if (metric %in% MARINE_MAMMAL_METRICS) {
            mean(.data[[ratio_col]] < 0.54, na.rm = TRUE)
          } else { NA_real_ },
          .groups = "drop"
        )
      summary_rows[[length(summary_rows) + 1]] <- decade_summaries
    }
    all_summaries[[comp]] <- bind_rows(summary_rows)
    cat(sprintf("    %s: %d records\n", comp, nrow(all_summaries[[comp]])))
  }
  return(all_summaries)
}

summarize_structural_deviations <- function(paired_data, empirical_thresholds) {
  cat("  Summarizing structural deviations...\n")
  diff_suffix <- c(exploit = "_exploit_diff", absolute = "_absolute_diff",
                   climate = "_climate_diff")
  envelope_prefix <- c(exploit = "", absolute = "", climate = "_clim")
  all_summaries <- list()

  for (comp in c("exploit", "absolute", "climate")) {
    summary_rows <- list()
    for (metric in STRUCTURAL_METRICS) {
      diff_col <- paste0(metric, diff_suffix[comp])
      fish_col <- paste0(metric, "_fish")
      if (!(diff_col %in% names(paired_data))) next

      th <- empirical_thresholds[[metric]]
      b0_median <- if (!is.null(th)) th$median else NA
      b0_sd <- if (!is.null(th)) th$sd else NA

      env_suffix <- envelope_prefix[comp]
      outside_50_col <- paste0(metric, env_suffix, "_outside_b0_50")
      outside_90_col <- paste0(metric, env_suffix, "_outside_b0_90")
      outside_98_col <- paste0(metric, env_suffix, "_outside_b0_98")
      below_q05_col <- paste0(metric, "_below_b0_q05")
      above_q95_col <- paste0(metric, "_above_b0_q95")

      decade_summaries <- paired_data %>%
        group_by(decade, start_year, end_year) %>%
        summarise(
          metric_name = metric, metric_category = "structural", comparison = comp,
          n_sims = n(),
          diff_median = median(.data[[diff_col]], na.rm = TRUE),
          diff_mean = mean(.data[[diff_col]], na.rm = TRUE),
          diff_sd = sd(.data[[diff_col]], na.rm = TRUE),
          diff_q05 = quantile(.data[[diff_col]], 0.05, na.rm = TRUE),
          diff_q25 = quantile(.data[[diff_col]], 0.25, na.rm = TRUE),
          diff_q75 = quantile(.data[[diff_col]], 0.75, na.rm = TRUE),
          diff_q95 = quantile(.data[[diff_col]], 0.95, na.rm = TRUE),
          z_median = if (!is.na(b0_sd) && b0_sd > 0) {
            median(.data[[diff_col]], na.rm = TRUE) / b0_sd
          } else NA_real_,
          value_median = if (fish_col %in% names(paired_data)) {
            median(.data[[fish_col]], na.rm = TRUE)
          } else NA_real_,
          value_q25 = if (fish_col %in% names(paired_data)) {
            quantile(.data[[fish_col]], 0.25, na.rm = TRUE)
          } else NA_real_,
          value_q75 = if (fish_col %in% names(paired_data)) {
            quantile(.data[[fish_col]], 0.75, na.rm = TRUE)
          } else NA_real_,
          n_valid = sum(!is.na(.data[[diff_col]])),
          prop_outside_b0_50 = if (outside_50_col %in% names(paired_data)) {
            mean(.data[[outside_50_col]], na.rm = TRUE)
          } else NA_real_,
          prop_outside_b0_90 = if (outside_90_col %in% names(paired_data)) {
            mean(.data[[outside_90_col]], na.rm = TRUE)
          } else NA_real_,
          prop_outside_b0_98 = if (outside_98_col %in% names(paired_data)) {
            mean(.data[[outside_98_col]], na.rm = TRUE)
          } else NA_real_,
          prop_below_b0_q05 = if (below_q05_col %in% names(paired_data) && comp != "climate") {
            mean(.data[[below_q05_col]], na.rm = TRUE)
          } else NA_real_,
          prop_above_b0_q95 = if (above_q95_col %in% names(paired_data) && comp != "climate") {
            mean(.data[[above_q95_col]], na.rm = TRUE)
          } else NA_real_,
          b0_median = b0_median,
          b0_sd = b0_sd,
          .groups = "drop"
        )
      summary_rows[[length(summary_rows) + 1]] <- decade_summaries
    }
    all_summaries[[comp]] <- bind_rows(summary_rows)
    cat(sprintf("    %s: %d records\n", comp, nrow(all_summaries[[comp]])))
  }
  return(all_summaries)
}

summarize_exploitation <- function(paired_data) {
  cat("  Summarizing exploitation rates...\n")
  exploit_summaries <- list()
  for (metric in EXPLOITATION_METRICS) {
    fish_col <- paste0(metric, "_fish")
    if (!(fish_col %in% names(paired_data))) next
    decade_summaries <- paired_data %>%
      group_by(decade, start_year, end_year) %>%
      summarise(
        metric_name = metric, comparison = "absolute_F", n_sims = n(),
        value_median = median(.data[[fish_col]], na.rm = TRUE),
        value_mean = mean(.data[[fish_col]], na.rm = TRUE),
        value_sd = sd(.data[[fish_col]], na.rm = TRUE),
        value_q05 = quantile(.data[[fish_col]], 0.05, na.rm = TRUE),
        value_q25 = quantile(.data[[fish_col]], 0.25, na.rm = TRUE),
        value_q75 = quantile(.data[[fish_col]], 0.75, na.rm = TRUE),
        value_q95 = quantile(.data[[fish_col]], 0.95, na.rm = TRUE),
        n_valid = sum(!is.na(.data[[fish_col]])),
        .groups = "drop"
      )
    exploit_summaries[[length(exploit_summaries) + 1]] <- decade_summaries
  }
  return(bind_rows(exploit_summaries))
}

###############################################################################
# Pre-Whaling Validation
###############################################################################

validate_pre_exploitation <- function(biomass_summaries, structural_summaries,
                                      tolerance = 0.05) {
  cat("\n=============================================================\n")
  cat("PRE-EXPLOITATION VALIDATION\n")
  cat("=============================================================\n\n")
  cat("Category A (Biomass) - exploitation ratios should be ~1.0:\n")
  pre_bio <- biomass_summaries$exploit %>% filter(start_year <= 1890)
  if (nrow(pre_bio) > 0) {
    pre_bio <- pre_bio %>% mutate(deviation = abs(ratio_median - 1.0),
                                   pass = deviation <= tolerance)
    for (i in seq_len(nrow(pre_bio))) {
      row <- pre_bio[i, ]
      cat(sprintf("  [%s] %s | %-25s: ratio = %.4f (dev = %.4f)\n",
                  ifelse(row$pass, "PASS", "FAIL"), row$decade, row$metric_name,
                  row$ratio_median, row$deviation))
    }
    cat(sprintf("  Biomass: %d/%d passed\n\n", sum(pre_bio$pass), nrow(pre_bio)))
  }
  cat("Category B (Structural) - exploitation differences should be ~0:\n")
  pre_str <- structural_summaries$exploit %>% filter(start_year <= 1890)
  if (nrow(pre_str) > 0) {
    pre_str <- pre_str %>%
      mutate(z_abs = abs(z_median), pass = !is.na(z_abs) & z_abs <= 0.2)
    for (i in seq_len(nrow(pre_str))) {
      row <- pre_str[i, ]
      cat(sprintf("  [%s] %s | %-25s: diff = %+.4f (z = %.3f)\n",
                  ifelse(row$pass, "PASS", "FAIL"), row$decade, row$metric_name,
                  row$diff_median, row$z_abs))
    }
    cat(sprintf("  Structural: %d/%d passed\n", sum(pre_str$pass, na.rm = TRUE), nrow(pre_str)))
  }
  return(list(biomass = pre_bio, structural = pre_str))
}

###############################################################################
# Heatmap Display Configuration
###############################################################################

METRIC_DISPLAY <- list(
  total_biomass            = list(name = "Total Biomass",              group = "Biomass",    order = 1),
  whale_biomass            = list(name = "All Whale Biomass",          group = "Biomass",    order = 2),
  baleen_biomass           = list(name = "Baleen Whale Biomass",       group = "Biomass",    order = 3),
  sperm_biomass            = list(name = "Sperm Whale Biomass",        group = "Biomass",    order = 4),
  apex_biomass             = list(name = "Apex Predator Biomass",      group = "Biomass",    order = 5),
  minke_biomass            = list(name = "Minke Whale Biomass",        group = "Biomass",    order = 6),
  seal_biomass             = list(name = "Seal Biomass",               group = "Biomass",    order = 7),
  fish_biomass             = list(name = "Fish Biomass",               group = "Biomass",    order = 8),
  krill_biomass            = list(name = "Antarctic Krill Biomass",    group = "Biomass",    order = 9),
  ltl_biomass              = list(name = "LTL Biomass",                group = "Biomass",    order = 10),
  spectrum_slope           = list(name = "Size Spectrum Slope",        group = "Structural", order = 11),
  spectrum_intercept       = list(name = "Size Spectrum Intercept",    group = "Structural", order = 12),
  spectrum_mle_exponent    = list(name = "Spectrum MLE Exponent (PLB)", group = "Structural", order = 13),
  spectrum_lcd_exponent    = list(name = "LCD Exponent",               group = "Structural", order = 13.1),
  spectrum_lbnbiom_slope   = list(name = "NBSS Slope",                 group = "Structural", order = 13.2),
  mean_tl                  = list(name = "Mean Trophic Level",         group = "Structural", order = 14),
  htl_indicator            = list(name = "High TL Indicator",          group = "Structural", order = 15),
  large_fish_indicator     = list(name = "Large Fish Indicator (\u2265100g)", group = "Structural", order = 16),
  fish_lfi                 = list(name = "Fish LFI (1000g)",           group = "Structural", order = 17),
  shannon_diversity        = list(name = "Shannon Diversity (H')",     group = "Structural", order = 18),
  w_statistic              = list(name = "W-Statistic (ABC)",          group = "Structural", order = 19),
  production_biomass_ratio = list(name = "Community P/B Ratio",        group = "Structural", order = 20),
  fish_pb_ratio            = list(name = "Fish P/B Ratio",             group = "Structural", order = 21),
  marine_mammal_pb_ratio   = list(name = "Marine Mammal P/B Ratio",   group = "Structural", order = 22),
  mean_weight              = list(name = "Mean Individual Weight",     group = "Structural", order = 23),
  mean_max_weight          = list(name = "Mean Max Weight",            group = "Structural", order = 24),
  predator_prey_ratio      = list(name = "Apex:Prey Biomass Ratio",   group = "Structural", order = 25),
  consumer_ltl_ratio       = list(name = "Consumer:LTL Ratio",        group = "Structural", order = 26)
)

METRIC_ORDER <- names(sort(sapply(METRIC_DISPLAY, function(m) m$order)))
METRIC_ORDER_NAMES <- sapply(METRIC_ORDER, function(m) METRIC_DISPLAY[[m]]$name)

get_display_name <- function(m) {
  if (m %in% names(METRIC_DISPLAY)) METRIC_DISPLAY[[m]]$name else m
}

# Display names for biomass metrics with letter suffix encoding thresholds:
#   [a] = CCAMLR gamma2 (< 0.75)
#   [b] = IWC RMP (< 0.54)
#   [c] = BMSY proxy (< 0.40)
get_biomass_rich_label <- function(metric_name) {
  display <- get_display_name(metric_name)
  thresh <- BIOMASS_THRESHOLD_MAP[metric_name]
  if (is.na(thresh)) return(display)
  if (thresh == 0.90) return(paste0(display, " [a]"))    # [a] = Near-pristine
  if (thresh == 0.75) return(paste0(display, " [b]"))    # [b] = CCAMLR
  if (thresh == 0.54) return(paste0(display, " [c]"))    # [c] = IWC
  return(paste0(display, " [d]"))                        # [d] = BMSY
}

# Style definitions for the threshold reference legend
STYLE_DEFS <- c(
  pristine = "[a] Near-pristine: B/B0 < 0.90",
  ccamlr   = "[b] CCAMLR gamma2: B/B0 < 0.75",
  iwc      = "[c] IWC RMP: B/B0 < 0.54",
  bmsy     = "[d] BMSY proxy: B/B0 < 0.40"
)

# Helper: return ggplot layers that add a 'Threshold Reference' legend via a
# hidden dummy geom_point + scale_shape_manual.  Usage: p + make_style_legend_layers(plot_data)
# plot_data must contain 'decade' and 'display_name' columns.
make_style_legend_layers <- function(plot_data) {
  first_x <- as.character(plot_data$decade[1])
  first_y <- as.character(plot_data$display_name[1])
  dummy <- data.frame(
    x = rep(first_x, 4),
    y = rep(first_y, 4),
    style = factor(c("pristine", "ccamlr", "iwc", "bmsy"),
                   levels = c("pristine", "ccamlr", "iwc", "bmsy"))
  )
  list(
    geom_point(data = dummy, aes(x = x, y = y, shape = style),
               inherit.aes = FALSE, alpha = 0, show.legend = TRUE, na.rm = TRUE),
    scale_shape_manual(
      name = "Threshold\nReference",
      values = c(pristine = 16, ccamlr = 16, iwc = 16, bmsy = 16),
      labels = STYLE_DEFS,
      guide = guide_legend(
        override.aes = list(alpha = 0),
        order = 2
      )
    )
  )
}

# Helper: extract a threshold style legend as a grob for patchwork composition
make_style_legend_grob <- function() {
  dummy <- data.frame(
    x = c(1, 1, 1, 1), y = c(1, 1, 1, 1),
    style = factor(c("pristine", "ccamlr", "iwc", "bmsy"),
                   levels = c("pristine", "ccamlr", "iwc", "bmsy"))
  )
  p <- ggplot(dummy, aes(x = x, y = y, shape = style)) +
    geom_point(alpha = 0) +
    scale_shape_manual(
      name = "Threshold Reference",
      values = c(pristine = 16, ccamlr = 16, iwc = 16, bmsy = 16),
      labels = STYLE_DEFS,
      guide = guide_legend(override.aes = list(alpha = 0))
    ) +
    theme_void() +
    theme(legend.text = element_text(size = 9.9, hjust = 0),
          legend.title = element_text(size = 11.3, hjust = 0),
          legend.key.size = unit(0.1, "cm"),
          legend.key.width = unit(0.1, "cm"),
          legend.title.position = "top",
          legend.justification = c(0, 0.5),
          legend.margin = margin(0, 2, 0, 0),
          legend.position = "right")
  cowplot::get_legend(p)
}

# Filter a summary to only include decades from PLOT_START_YEAR onward
filter_decades <- function(df) {
  df %>% filter(start_year >= PLOT_START_YEAR)
}

###############################################################################
# Heatmap: Biomass (Category A)
###############################################################################

plot_biomass_ratio_heatmap <- function(ratio_summary, title, subtitle,
                                       output_path, filename,
                                       metrics_filter = NULL) {
  plot_data <- filter_decades(ratio_summary)
  if (!is.null(metrics_filter))
    plot_data <- plot_data %>% filter(metric_name %in% metrics_filter)
  plot_data <- plot_data %>% mutate(display_name = sapply(metric_name, get_display_name))
  bio_names <- sapply(BIOMASS_METRICS, get_display_name)
  decade_order <- unique(plot_data$decade[order(plot_data$start_year)])
  plot_data$decade <- factor(plot_data$decade, levels = decade_order)
  plot_data$display_name <- factor(plot_data$display_name, levels = rev(bio_names))
  plot_data$ratio_clamped <- pmin(pmax(plot_data$ratio_median, 0), 2.5)

  p <- ggplot(plot_data, aes(x = decade, y = display_name, fill = ratio_clamped)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.2f", ratio_median)), size = 2.8, color = "black") +
    scale_fill_gradientn(
      colours = c("#d73027", "#fc8d59", "#fee08b", "#ffffbf", "#d9ef8b", "#91cf60", "#1a9850"),
      values = scales::rescale(c(0, 0.4, 0.75, 1.0, 1.25, 1.75, 2.5)),
      limits = c(0, 2.5), na.value = "grey80", name = "Median\nRatio") +
    labs(title = title, subtitle = subtitle, x = "Decade", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          legend.position = "right", panel.grid = element_blank())

  ggsave(file.path(output_path, paste0(filename, ".png")), p, width = 15, height = 5.5, dpi = 300)
  return(p)
}

plot_biomass_proportion_heatmap <- function(ratio_summary, prop_col, threshold_val,
                                            threshold_label, title,
                                            output_path, filename,
                                            metrics_filter = NULL) {
  plot_data <- filter_decades(ratio_summary)
  if (!is.null(metrics_filter))
    plot_data <- plot_data %>% filter(metric_name %in% metrics_filter)
  plot_data <- plot_data %>% filter(!is.na(.data[[prop_col]]))
  if (nrow(plot_data) == 0) { cat(sprintf("  Skipping %s\n", filename)); return(NULL) }
  plot_data <- plot_data %>%
    mutate(display_name = sapply(metric_name, get_display_name),
           prop_value = .data[[prop_col]])
  bio_names <- sapply(BIOMASS_METRICS, get_display_name)
  valid_names <- bio_names[bio_names %in% plot_data$display_name]
  decade_order <- unique(plot_data$decade[order(plot_data$start_year)])
  plot_data$decade <- factor(plot_data$decade, levels = decade_order)
  plot_data$display_name <- factor(plot_data$display_name, levels = rev(valid_names))

  p <- ggplot(plot_data, aes(x = decade, y = display_name, fill = prop_value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", prop_value * 100)), size = 2.8, color = "black") +
    scale_fill_gradientn(
      colours = c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027"),
      values = c(0, 0.1, 0.25, 0.5, 0.75, 1),
      limits = c(0, 1), na.value = "grey80",
      name = sprintf("P(ratio\n< %.2f)", threshold_val), labels = scales::percent) +
    labs(title = title,
         subtitle = sprintf("P(ratio < %.2f) \u2014 %s", threshold_val, threshold_label),
         x = "Decade", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 9.5, hjust = 0.5),
          legend.position = "right", panel.grid = element_blank())

  ggsave(file.path(output_path, paste0(filename, ".png")), p, width = 15, height = 7, dpi = 300)
  return(p)
}

###############################################################################
# Heatmap: Biomass Metric-Specific Thresholds
###############################################################################
# Uses BIOMASS_THRESHOLD_MAP to apply the appropriate reference level per metric:
#   krill/LTL/total -> 0.75 (CCAMLR gamma2; Constable et al. 2000)
#   whales/seals/apex -> 0.54 (IWC RMP; IWC 1994, Punt & Donovan 2007)
#   fish -> 0.40 (BMSY proxy; Restrepo et al. 1998)

plot_biomass_metric_specific_heatmap <- function(ratio_summary, title,
                                                  output_path, filename) {
  # Build per-metric prop_value using the metric-specific threshold
  plot_data <- filter_decades(ratio_summary) %>%
    mutate(
      threshold = BIOMASS_THRESHOLD_MAP[metric_name],
      prop_value = case_when(
        threshold == 0.90 ~ prop_below_090,
        threshold == 0.75 ~ prop_below_075,
        threshold == 0.54 ~ prop_below_054,
        threshold == 0.40 ~ prop_below_040,
        TRUE ~ prop_below_075
      )
    ) %>%
    filter(!is.na(prop_value))

  if (nrow(plot_data) == 0) { cat(sprintf("  Skipping %s\n", filename)); return(NULL) }

  # Rich-text y-axis labels: bold = CCAMLR 0.75, italic = IWC 0.54, plain = BMSY 0.40
  plot_data <- plot_data %>%
    mutate(display_name = sapply(metric_name, get_biomass_rich_label))
  bio_rich <- sapply(BIOMASS_METRICS, get_biomass_rich_label)
  valid_labels <- bio_rich[bio_rich %in% plot_data$display_name]
  decade_order <- unique(plot_data$decade[order(plot_data$start_year)])
  plot_data$decade <- factor(plot_data$decade, levels = decade_order)
  plot_data$display_name <- factor(plot_data$display_name, levels = rev(valid_labels))

  p <- ggplot(plot_data, aes(x = decade, y = display_name, fill = prop_value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", prop_value * 100)), size = 2.8, color = "black") +
    make_style_legend_layers(plot_data) +
    scale_fill_gradientn(
      colours = c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027"),
      values = c(0, 0.1, 0.25, 0.5, 0.75, 1),
      limits = c(0, 1), na.value = "grey80",
      name = "P(ratio\n< threshold)", labels = scales::percent) +
    labs(title = NULL, subtitle = NULL,
         x = "Decade", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 10),
          legend.position = "right", panel.grid = element_blank())

  ggsave(file.path(output_path, paste0(filename, ".png")), p, width = 15, height = 5.5, dpi = 300)
  return(p)
}

###############################################################################
# Heatmap: Structural Deviations (Category B)
###############################################################################

plot_structural_deviation_heatmap <- function(structural_summary,
                                              envelope_col, envelope_label,
                                              title, output_path, filename,
                                              null_expected = STRUCTURAL_NULL_90CI) {
  plot_data <- filter_decades(structural_summary) %>%
    filter(metric_name %in% STRUCTURAL_METRICS_PLOT) %>%
    mutate(display_name = sapply(metric_name, get_display_name),
           prop_value = pmax((.data[[envelope_col]] - null_expected) / (1 - null_expected), 0),
           conf_level = bin_ipcc_confidence(prop_value))
  if (all(is.na(plot_data$prop_value))) {
    cat(sprintf("  Skipping %s (no valid data)\n", filename)); return(NULL)
  }
  str_names <- sapply(STRUCTURAL_METRICS_PLOT, get_display_name)
  valid_names <- str_names[str_names %in% plot_data$display_name]
  decade_order <- unique(plot_data$decade[order(plot_data$start_year)])
  plot_data$decade <- factor(plot_data$decade, levels = decade_order)
  plot_data$display_name <- factor(plot_data$display_name, levels = rev(valid_names))
  if (nrow(plot_data) == 0) { cat(sprintf("  Skipping %s\n", filename)); return(NULL) }

  p <- ggplot(plot_data, aes(x = decade, y = display_name, fill = conf_level)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", prop_value * 100)), size = 2.8, color = "black") +
    scale_fill_manual(
      values = IPCC_CONF_COLOURS, drop = FALSE, na.value = "grey80",
      name = "Confidence\nin change") +
    labs(title = title,
         subtitle = sprintf("Skill-score normalised confidence (null = %.0f%%)", null_expected * 100),
         x = "Decade", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 9.5, hjust = 0.5),
          legend.position = "right", panel.grid = element_blank())

  ggsave(file.path(output_path, paste0(filename, ".png")), p, width = 15, height = 7, dpi = 300)
  return(p)
}

plot_structural_zscore_heatmap <- function(structural_summary, title,
                                           output_path, filename) {
  plot_data <- filter_decades(structural_summary) %>%
    filter(metric_name %in% STRUCTURAL_METRICS_PLOT) %>%
    mutate(display_name = sapply(metric_name, get_display_name),
           z_clamped = ifelse(is.na(z_median), NA, pmin(pmax(z_median, -5), 5)))
  if (nrow(plot_data) == 0) {
    cat(sprintf("  Skipping %s (no data)\n", filename)); return(NULL)
  }
  str_names <- sapply(STRUCTURAL_METRICS_PLOT, get_display_name)
  valid_names <- str_names[str_names %in% plot_data$display_name]
  decade_order <- unique(plot_data$decade[order(plot_data$start_year)])
  plot_data$decade <- factor(plot_data$decade, levels = decade_order)
  plot_data$display_name <- factor(plot_data$display_name, levels = rev(valid_names))

  p <- ggplot(plot_data, aes(x = decade, y = display_name, fill = z_clamped)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%+.2f", z_median)), size = 2.8, color = "black") +
    scale_fill_gradientn(
      colours = c("#2166ac", "#67a9cf", "#d1e5f0", "#f7f7f7",
                  "#fddbc7", "#ef8a62", "#b2182b"),
      values = scales::rescale(c(-5, -2, -1, 0, 1, 2, 5)),
      limits = c(-5, 5), na.value = "grey80",
      name = "z-score") +
    labs(title = title,
         subtitle = "Median standardized departure: (value - B0 median) / B0 SD",
         x = "Decade", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          legend.position = "right", panel.grid = element_blank())

  ggsave(file.path(output_path, paste0(filename, ".png")), p, width = 15, height = 7, dpi = 300)
  return(p)
}

###############################################################################
# Combined Heatmap — Single-Stacked (Biomass + Structural)
#
# Biomass:     P(B/B0 < metric-specific threshold) — paired ratios
# Structural:  Skill-score normalised P(outside B0 envelope) — (p - p_null) / (1 - p_null)
# Uses patchwork for two-panel layout (avoids facet_grid + element_markdown issues)
#
# envelope_col:  column name in structural_summary (default: "prop_outside_b0_90")
# null_expected:  expected proportion outside this envelope by construction
#                 (0.10 for 90% CI, 0.50 for IQR/50% CI)
###############################################################################

STRUCTURAL_NULL_90CI <- 0.10  # expected proportion outside 5th-95th by construction
STRUCTURAL_NULL_50CI <- 0.50  # expected proportion outside 25th-75th (IQR)

# IPCC AR6 discrete confidence levels for structural indicators
# Skill-score normalised: (p_obs - p_null) / (1 - p_null)
IPCC_CONF_LEVELS  <- c("Very low", "Low", "Medium", "High", "Very high")
IPCC_CONF_COLOURS <- c("#1a9850", "#a6d96a", "#fee08b", "#f46d43", "#a50026")
IPCC_CONF_BREAKS  <- c(0, 0.10, 0.33, 0.66, 0.90, 1.0)
names(IPCC_CONF_COLOURS) <- IPCC_CONF_LEVELS

# Helper: bin a numeric 0-1 skill-score into IPCC confidence factor
bin_ipcc_confidence <- function(x) {
  cut(x, breaks = IPCC_CONF_BREAKS, labels = IPCC_CONF_LEVELS,
      include.lowest = TRUE, right = TRUE)
}

plot_combined_heatmap <- function(biomass_summary, structural_summary,
                                  title, output_path, filename,
                                  envelope_col = "prop_outside_b0_90",
                                  null_expected = STRUCTURAL_NULL_90CI) {

  # --- Biomass panel data: metric-specific thresholds ---
  bio_data <- filter_decades(biomass_summary) %>%
    mutate(
      threshold = BIOMASS_THRESHOLD_MAP[metric_name],
      prop_value = case_when(
        threshold == 0.90 ~ prop_below_090,
        threshold == 0.75 ~ prop_below_075,
        threshold == 0.54 ~ prop_below_054,
        threshold == 0.40 ~ prop_below_040,
        TRUE ~ prop_below_075
      )
    ) %>%
    filter(!is.na(prop_value)) %>%
    mutate(display_name = sapply(metric_name, get_biomass_rich_label))

  bio_rich <- sapply(BIOMASS_METRICS, get_biomass_rich_label)
  bio_valid <- bio_rich[bio_rich %in% bio_data$display_name]
  decade_order <- unique(bio_data$decade[order(bio_data$start_year)])
  bio_data$decade <- factor(bio_data$decade, levels = decade_order)
  bio_data$display_name <- factor(bio_data$display_name, levels = rev(bio_valid))

  # --- Structural panel data: skill-score normalised ---
  str_data <- filter_decades(structural_summary) %>%
    filter(metric_name %in% STRUCTURAL_METRICS_PLOT) %>%
    filter(!is.na(.data[[envelope_col]])) %>%
    mutate(
      display_name = sapply(metric_name, get_display_name),
      raw_prop = .data[[envelope_col]],
      prop_value = pmax((.data[[envelope_col]] - null_expected) / (1 - null_expected), 0),
      conf_level = bin_ipcc_confidence(prop_value)
    )
  str_names <- sapply(STRUCTURAL_METRICS_PLOT, get_display_name)
  str_valid <- str_names[str_names %in% str_data$display_name]
  str_data$decade <- factor(str_data$decade, levels = decade_order)
  str_data$display_name <- factor(str_data$display_name, levels = rev(str_valid))

  cat(sprintf("  Biomass panel: %d metrics x %d decades\n",
              length(bio_valid), length(decade_order)))
  cat(sprintf("  Structural panel: %d metrics x %d decades (skill-score normalised, null=%.0f%%)\n",
              length(str_valid), length(decade_order), null_expected * 100))

  # --- Biomass fill scale ---
  bio_fill_colours <- c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027")
  bio_fill_values  <- c(0, 0.1, 0.25, 0.5, 0.75, 1)

  # --- Biomass panel ---
  p_bio <- ggplot(bio_data, aes(x = decade, y = display_name, fill = prop_value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", prop_value * 100)),
              size = 2.5, color = "black") +
    scale_fill_gradientn(
      colours = bio_fill_colours, values = bio_fill_values,
      limits = c(0, 1), na.value = "grey80",
      name = "% below\nthreshold", labels = scales::percent) +
    labs(x = "", y = "", tag = "A") +
    theme_minimal(base_size = 8, base_family = "Arial") +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 7),
      plot.tag = element_text(size = 9, face = "bold", hjust = 0, vjust = 1),
      plot.tag.position = c(0, 1),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.margin = margin(6, 1, 1, 3))

  # --- Structural panel (IPCC discrete confidence levels) ---
  p_str <- ggplot(str_data, aes(x = decade, y = display_name, fill = conf_level)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", prop_value * 100)),
              size = 2.5, color = "black") +
    scale_fill_manual(
      values = IPCC_CONF_COLOURS,
      drop = FALSE, na.value = "grey80",
      name = "Confidence\nin change") +
    labs(x = "Decade", y = "", tag = "B") +
    theme_minimal(base_size = 8, base_family = "Arial") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      plot.tag = element_text(size = 9, face = "bold", hjust = 0, vjust = 1),
      plot.tag.position = c(0, 1),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.margin = margin(6, 1, 3, 3))

  # --- Biomass fill legend ---
  p_bio_legend <- ggplot(data.frame(x = "a", y = "b", v = 0.5),
                          aes(x, y, fill = v)) +
    geom_tile() +
    scale_fill_gradientn(
      colours = bio_fill_colours, values = bio_fill_values,
      limits = c(0, 1), na.value = "grey80",
      name = "% below\nthreshold", labels = scales::percent) +
    theme(legend.position = "right",
          legend.title = element_text(size = 7, family = "Arial"),
          legend.text = element_text(size = 7, family = "Arial"))
  bio_legend <- cowplot::get_legend(p_bio_legend)

  # --- Structural fill legend (IPCC discrete confidence) ---
  p_str_legend <- ggplot(data.frame(x = rep("a", 5), y = rep("b", 5),
                                     conf = factor(IPCC_CONF_LEVELS, levels = IPCC_CONF_LEVELS)),
                          aes(x, y, fill = conf)) +
    geom_tile() +
    scale_fill_manual(values = IPCC_CONF_COLOURS, drop = FALSE, na.value = "grey80",
                      name = "Confidence\nin change") +
    theme(legend.position = "right",
          legend.title = element_text(size = 7, family = "Arial"),
          legend.text = element_text(size = 7, family = "Arial"))
  str_legend <- cowplot::get_legend(p_str_legend)

  # --- Assemble with patchwork: 2x2 grid ---
  p_combined <- (p_bio + wrap_elements(bio_legend) +
                 p_str + wrap_elements(str_legend)) +
    plot_layout(
      ncol = 2,
      nrow = 2,
      widths = c(1, 0.22),
      heights = c(length(bio_valid), length(str_valid))
    ) +
    plot_annotation(theme = theme())

  # Nature portfolio: double-column width = 183 mm = 7.2 in; height ≤ 247 mm
  ggsave(file.path(output_path, paste0(filename, ".png")), p_combined,
         width = 7.2, height = 8.5, units = "in", dpi = 600, device = ragg::agg_png)
  tryCatch(
    ggsave(file.path(output_path, paste0(filename, ".pdf")), p_combined,
           width = 7.2, height = 8.5, units = "in", device = cairo_pdf),
    error = function(e) cat(sprintf("  Warning: PDF save failed (%s)\n", e$message))
  )
  tryCatch(
    ggsave(file.path(output_path, paste0(filename, ".tiff")), p_combined,
           width = 7.2, height = 8.5, units = "in", dpi = 600, device = "tiff", compression = "lzw"),
    error = function(e) cat(sprintf("  Warning: TIFF save failed (%s)\n", e$message))
  )
  cat(sprintf("  Saved combined heatmap: %s.png + .pdf + .tiff\n", filename))
  return(p_combined)
}

###############################################################################
# Exploitation Rate Heatmap (Category C)
###############################################################################

plot_exploitation_heatmap <- function(exploit_summary, output_path) {
  exploit_display <- list(
    exploitation_total = list(name = "Total Exploitation Rate"),
    exploitation_whale = list(name = "Whale Exploitation Rate"),
    exploitation_krill = list(name = "Krill Exploitation Rate")
  )
  plot_data <- filter_decades(exploit_summary) %>%
    mutate(display_name = sapply(metric_name, function(m) {
      if (m %in% names(exploit_display)) exploit_display[[m]]$name else m }))
  decade_order <- unique(plot_data$decade[order(plot_data$start_year)])
  plot_data$decade <- factor(plot_data$decade, levels = decade_order)
  name_order <- sapply(names(exploit_display), function(m) exploit_display[[m]]$name)
  plot_data$display_name <- factor(plot_data$display_name, levels = rev(name_order))

  p <- ggplot(plot_data, aes(x = decade, y = display_name, fill = value_median)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.3f", value_median)), size = 3.1, color = "black") +
    scale_fill_gradientn(
      colours = c("#1a9850", "#91cf60", "#fee08b", "#fc8d59", "#d73027"),
      values = c(0, 0.05, 0.2, 0.4, 1),
      limits = c(0, max(0.5, max(plot_data$value_median, na.rm = TRUE))),
      na.value = "grey80", name = "Median F") +
    labs(title = "Exploitation Rates (Fishing Scenario)",
         subtitle = "Median fishing mortality rates across ensemble",
         x = "Decade", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5),
          legend.position = "right", panel.grid = element_blank())

  ggsave(file.path(output_path, "exploitation_heatmap.png"), p, width = 15, height = 4, dpi = 300)
  return(p)
}

###############################################################################
# Per-Species Exploitation Rate Computation & Heatmap (Category C v2)
###############################################################################

# Display name mapping for fished species (ordered top-to-bottom in plot)
FISHED_SPECIES_DISPLAY <- c(
  "baleen whales"            = "Baleen Whales",
  "sperm whales"             = "Sperm Whales",
  "minke whales"             = "Minke Whales",
  "orca"                     = "Orca",
  "toothfishes"              = "Toothfishes",
  "shelf and coastal fishes" = "Shelf & Coastal Fishes",
  "bathypelagic fishes"      = "Bathypelagic Fishes",
  "squids"                   = "Squids",
  "antarctic krill"          = "Antarctic Krill"
)

# Compute per-species per-decade exploitation rate from a single simulation
# Returns a data.frame with columns: species, decade, start_year, end_year, F_rate
compute_sim_species_exploitation <- function(sim, decades_df = DECADES) {
  yield_mat <- tryCatch(getYield(sim), error = function(e) NULL)
  biomass_mat <- getBiomass(sim)
  if (is.null(yield_mat)) return(NULL)

  times <- as.numeric(rownames(biomass_mat))
  sp_names <- colnames(yield_mat)

  # Identify species with any non-zero yield
  total_yield <- colSums(yield_mat, na.rm = TRUE)
  fished_sp <- names(total_yield[total_yield > 0])
  if (length(fished_sp) == 0) return(NULL)

  results <- list()
  for (d_i in seq_len(nrow(decades_df))) {
    d <- decades_df[d_i, ]
    t_idx <- which(times >= d$start & times <= d$end)
    if (length(t_idx) == 0) next

    for (sp in fished_sp) {
      # Per-year F = yield / biomass, then average across the decade
      yield_vals <- yield_mat[t_idx, sp]
      biom_vals  <- biomass_mat[t_idx, sp]
      f_annual <- ifelse(biom_vals > 0, yield_vals / biom_vals, NA_real_)
      f_mean <- mean(f_annual, na.rm = TRUE)
      if (is.nan(f_mean)) f_mean <- NA_real_

      results[[length(results) + 1]] <- data.frame(
        species = sp, decade = d$label, start_year = d$start, end_year = d$end,
        F_rate = f_mean, stringsAsFactors = FALSE
      )
    }
  }
  return(bind_rows(results))
}

# Compute species exploitation summary across the full ensemble.
# Loads the ensemble, iterates through each sim, computes per-species F,
# and returns summary statistics (median, q05, q25, q75, q95) per species per decade.
# Results are cached to avoid re-computation.
compute_species_exploitation_summary <- function(ensemble_file = ENSEMBLE_PATHS$fishing,
                                                  cache_file = file.path(OUTPUT_DIR_LARGE,
                                                    "species_exploitation_summary.rds"),
                                                  force_recompute = FALSE) {
  if (!force_recompute && file.exists(cache_file)) {
    cat("  Loading cached species exploitation summary...\n")
    return(readRDS(cache_file))
  }

  cat("  Computing per-species exploitation rates from ensemble...\n")
  mc <- readRDS(ensemble_file)
  if ("simulations" %in% names(mc)) {
    sims <- mc$simulations
  } else {
    sims <- mc
  }
  n_sims <- length(sims)
  cat(sprintf("  Processing %d simulations...\n", n_sims))

  all_results <- list()
  pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
  for (i in seq_len(n_sims)) {
    setTxtProgressBar(pb, i)
    sim_data <- tryCatch(compute_sim_species_exploitation(sims[[i]]), error = function(e) NULL)
    if (!is.null(sim_data)) {
      sim_data$sim_id <- i
      all_results[[length(all_results) + 1]] <- sim_data
    }
  }
  close(pb)

  raw_data <- bind_rows(all_results)
  cat(sprintf("  Raw data: %d records (%d sims x species x decades)\n", nrow(raw_data), n_sims))

  # Summarize across ensemble
  summary_data <- raw_data %>%
    group_by(species, decade, start_year, end_year) %>%
    summarise(
      n_sims = n(),
      F_median = median(F_rate, na.rm = TRUE),
      F_mean = mean(F_rate, na.rm = TRUE),
      F_q05 = quantile(F_rate, 0.05, na.rm = TRUE),
      F_q25 = quantile(F_rate, 0.25, na.rm = TRUE),
      F_q75 = quantile(F_rate, 0.75, na.rm = TRUE),
      F_q95 = quantile(F_rate, 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  cat(sprintf("  Summary: %d species x decade combinations\n", nrow(summary_data)))

  # Cache
  saveRDS(summary_data, cache_file)
  cat(sprintf("  Cached to: %s\n", cache_file))

  return(summary_data)
}

# Plot per-species exploitation heatmap
plot_species_exploitation_heatmap <- function(species_exploit_summary, output_path) {
  # Filter to plotted decades and only species with known display names
  plot_data <- filter_decades(species_exploit_summary) %>%
    filter(species %in% names(FISHED_SPECIES_DISPLAY)) %>%
    mutate(display_name = FISHED_SPECIES_DISPLAY[species])

  if (nrow(plot_data) == 0) {
    cat("  No species exploitation data to plot.\n")
    return(NULL)
  }

  decade_order <- unique(plot_data$decade[order(plot_data$start_year)])
  plot_data$decade <- factor(plot_data$decade, levels = decade_order)
  plot_data$display_name <- factor(plot_data$display_name,
    levels = rev(FISHED_SPECIES_DISPLAY))

  # Determine scale max — use at least 0.1, round up to next 0.05
  max_F <- max(plot_data$F_median, na.rm = TRUE)
  scale_max <- max(0.1, ceiling(max_F * 20) / 20)  # round up to nearest 0.05

  # Adaptive label: show 3 decimal places for small F, 2 for large
  plot_data$label_text <- ifelse(
    plot_data$F_median >= 0.01,
    sprintf("%.2f", plot_data$F_median),
    ifelse(plot_data$F_median > 0.0005,
           sprintf("%.3f", plot_data$F_median),
           "")
  )

  p <- ggplot(plot_data, aes(x = decade, y = display_name, fill = F_median)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = label_text), size = 2.8, color = "black") +
    scale_fill_gradientn(
      colours = c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027"),
      values = scales::rescale(c(0, 0.005, 0.02, 0.05, 0.15, scale_max), to = c(0, 1)),
      limits = c(0, scale_max),
      na.value = "grey90", name = "Median F\n(Yield/Biomass)",
      labels = scales::label_number(accuracy = 0.01)) +
    labs(title = "Species-Specific Exploitation Rates (Fishing Scenario)",
         subtitle = "Median fishing mortality rate (F = Yield/Biomass) across 2111 ensemble members",
         x = "Decade", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          legend.position = "right", panel.grid = element_blank())

  ggsave(file.path(output_path, "exploitation_heatmap_species.png"), p,
         width = 16, height = 6, dpi = 300)
  cat("  Saved: exploitation_heatmap_species.png\n")
  return(p)
}

###############################################################################
# Per-Species Exploitation Dual-Panel Heatmap (Median F | 95th Percentile F)
###############################################################################

# Helper: make an exploitation panel (used by dual-panel and triple-stacked)
# f_col: which column to plot ("F_median", "F_q75", "F_q95")
# scale_max: upper limit for color scale
make_exploit_panel <- function(species_exploit_summary, f_col, scale_max,
                                show_y = TRUE, show_x = TRUE, text_size = 2.3) {
  plot_data <- filter_decades(species_exploit_summary) %>%
    filter(species %in% names(FISHED_SPECIES_DISPLAY)) %>%
    mutate(display_name = FISHED_SPECIES_DISPLAY[species],
           f_val = .data[[f_col]])

  decade_order <- unique(plot_data$decade[order(plot_data$start_year)])
  plot_data$decade <- factor(plot_data$decade, levels = decade_order)
  plot_data$display_name <- factor(plot_data$display_name,
    levels = rev(FISHED_SPECIES_DISPLAY))

  # Adaptive label: 2 dp for >=0.01, 3 dp for small, empty for near-zero
  plot_data$label_text <- ifelse(
    plot_data$f_val >= 0.01,
    sprintf("%.2f", plot_data$f_val),
    ifelse(plot_data$f_val > 0.0005,
           sprintf("%.3f", plot_data$f_val), ""))

  p <- ggplot(plot_data, aes(x = decade, y = display_name, fill = f_val)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = label_text), size = text_size, color = "black") +
    scale_fill_gradientn(
      colours = c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027"),
      values = scales::rescale(c(0, 0.005, 0.02, 0.05, 0.15, scale_max), to = c(0, 1)),
      limits = c(0, scale_max), na.value = "grey90",
      name = "F\n(Yield/Biomass)",
      labels = scales::label_number(accuracy = 0.01)) +
    labs(x = if (show_x) "Decade" else "", y = "") +
    theme_minimal() +
    theme(
      axis.text.x = if (show_x) element_text(angle = 45, hjust = 1, size = 7) else element_blank(),
      axis.text.y = if (show_y) element_text(size = 8) else element_blank(),
      legend.position = "none", panel.grid = element_blank())
  return(p)
}

# Dual-panel exploitation heatmap: Median F (left) | 95th percentile F (right)
plot_species_exploitation_dual <- function(species_exploit_summary, output_path,
                                            filename = "exploitation_heatmap_species_dual") {
  exploit_data <- filter_decades(species_exploit_summary) %>%
    filter(species %in% names(FISHED_SPECIES_DISPLAY))

  if (nrow(exploit_data) == 0) {
    cat("  No species exploitation data to plot.\n")
    return(NULL)
  }

  # Determine shared scale max from 95th percentile column
  max_F <- max(exploit_data$F_q95, na.rm = TRUE)
  scale_max <- max(0.2, ceiling(max_F * 10) / 10)  # round up to nearest 0.1

  p_median <- make_exploit_panel(species_exploit_summary, "F_median", scale_max,
                                  show_y = TRUE, show_x = TRUE, text_size = 2.5) +
    labs(title = "Median F (50th percentile)") +
    theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

  p_q95 <- make_exploit_panel(species_exploit_summary, "F_q95", scale_max,
                                show_y = FALSE, show_x = TRUE, text_size = 2.5) +
    labs(title = "Upper 95th Percentile F") +
    theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

  # Shared legend
  p_for_legend <- ggplot(data.frame(x = "a", y = "b", v = scale_max / 2),
                          aes(x, y, fill = v)) +
    geom_tile() +
    scale_fill_gradientn(
      colours = c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027"),
      values = scales::rescale(c(0, 0.005, 0.02, 0.05, 0.15, scale_max), to = c(0, 1)),
      limits = c(0, scale_max), na.value = "grey90",
      name = "F\n(Yield/Biomass)",
      labels = scales::label_number(accuracy = 0.01)) +
    theme(legend.position = "right")
  shared_legend <- cowplot::get_legend(p_for_legend)

  p_combined <- (p_median | p_q95 | wrap_elements(shared_legend)) +
    plot_annotation(
      title = "Species-Specific Exploitation Rates: Median vs Upper 95th Percentile",
      subtitle = "Fishing mortality (F = Yield/Biomass) across 2111 ensemble members",
      theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                    plot.subtitle = element_text(size = 10, hjust = 0.5))
    ) +
    plot_layout(widths = c(1, 0.85, 0.12))

  ggsave(file.path(output_path, paste0(filename, ".png")), p_combined,
         width = 20, height = 7, dpi = 300)
  cat(sprintf("  Saved: %s.png\n", filename))
  return(p_combined)
}

###############################################################################
# Main Execution
###############################################################################

main_ecosystem_assessment <- function(max_sims = NULL) {
  cat("=============================================================\n")
  cat("PRYDZ BAY ECOSYSTEM ASSESSMENT (v2 - Paired Ensemble)\n")
  cat(sprintf("B0 Reference Period: %d-%d (ISIMIP climate norm)\n", B0_PERIOD[1], B0_PERIOD[2]))
  cat("=============================================================\n\n")
  start_time <- Sys.time()

  # 1. Process paired ensemble
  ensemble_data <- process_paired_ensemble(
    ENSEMBLE_PATHS$fishing, ENSEMBLE_PATHS$climate_only, max_sims = max_sims)
  fishing_raw <- ensemble_data$fishing_raw
  climate_raw <- ensemble_data$climate_raw
  b0_reference <- ensemble_data$b0_reference

  # 2. Derive empirical thresholds
  cat("\n=============================================================\n")
  cat("DERIVING EMPIRICAL THRESHOLDS\n")
  cat("=============================================================\n")
  empirical_thresholds <- derive_empirical_thresholds(b0_reference)

  # 3. Compute paired comparisons
  paired_data <- compute_paired_comparisons(
    fishing_raw, climate_raw, b0_reference, empirical_thresholds)

  # 4. Summarize by category
  cat("\nSummarizing results by metric category...\n")
  biomass_summaries <- summarize_biomass_ratios(paired_data)
  structural_summaries <- summarize_structural_deviations(paired_data, empirical_thresholds)
  exploitation_summary <- summarize_exploitation(paired_data)

  # 5. Pre-whaling validation
  validation <- validate_pre_exploitation(biomass_summaries, structural_summaries)

  # 6. Generate visualizations
  cat("\n=============================================================\n")
  cat("GENERATING VISUALIZATIONS\n")
  cat("=============================================================\n\n")
  plots <- list()

  cat("  Category A: Biomass ratio heatmaps...\n")
  plots$bio_exploit_ratio <- plot_biomass_ratio_heatmap(
    biomass_summaries$exploit, "Exploitation Impact: Biomass Metrics",
    "Median ratio: Fishing / Climate-only (paired)",
    OUTPUT_DIR, "heatmap_biomass_exploitation_ratio")
  plots$bio_absolute_ratio <- plot_biomass_ratio_heatmap(
    biomass_summaries$absolute, "Absolute Health: Biomass Metrics",
    sprintf("Median ratio: Fishing / B0 (%d-%d)", B0_PERIOD[1], B0_PERIOD[2]),
    OUTPUT_DIR, "heatmap_biomass_absolute_ratio")
  plots$bio_climate_ratio <- plot_biomass_ratio_heatmap(
    biomass_summaries$climate, "Climate Impact: Biomass Metrics",
    sprintf("Median ratio: Climate-only / B0 (%d-%d)", B0_PERIOD[1], B0_PERIOD[2]),
    OUTPUT_DIR, "heatmap_biomass_climate_ratio")

  cat("  Category A: Biomass proportion heatmaps (metric-specific thresholds)...\n")
  plots$bio_absolute_specific <- plot_biomass_metric_specific_heatmap(
    biomass_summaries$absolute,
    "Absolute Health: P(B/B0 < Metric-Specific Threshold)",
    OUTPUT_DIR, "heatmap_biomass_absolute_metric_specific")
  plots$bio_exploit_specific <- plot_biomass_metric_specific_heatmap(
    biomass_summaries$exploit,
    "Exploitation Impact: P(Fishing/Climate-only < Threshold)",
    OUTPUT_DIR, "heatmap_biomass_exploit_metric_specific")
  plots$bio_climate_specific <- plot_biomass_metric_specific_heatmap(
    biomass_summaries$climate,
    "Climate Impact: P(Climate-only/B0 < Threshold)",
    OUTPUT_DIR, "heatmap_biomass_climate_metric_specific")

  cat("  Category B: Structural deviation heatmaps...\n")
  plots$str_outside_90 <- plot_structural_deviation_heatmap(
    structural_summaries$absolute, "prop_outside_b0_90",
    "5th\u201395th percentile (empirical B0 envelope, 1841\u20131860)",
    "Structural Metrics: Outside B0 Natural Range",
    OUTPUT_DIR, "heatmap_structural_outside_b0_90")
  plots$str_outside_98 <- plot_structural_deviation_heatmap(
    structural_summaries$absolute, "prop_outside_b0_98",
    "1st\u201399th percentile (substantially altered)",
    "Structural Metrics: Substantially Altered",
    OUTPUT_DIR, "heatmap_structural_outside_b0_98")
  plots$str_zscore_abs <- plot_structural_zscore_heatmap(
    structural_summaries$absolute,
    "Structural Metrics: Standardized Departure from B0",
    OUTPUT_DIR, "heatmap_structural_zscore_absolute")
  plots$str_zscore_exploit <- plot_structural_zscore_heatmap(
    structural_summaries$exploit,
    "Structural Metrics: Exploitation-Driven Departure",
    OUTPUT_DIR, "heatmap_structural_zscore_exploitation")

  cat("  Combined heatmaps (biomass + structural)...\n")
  plots$combined_absolute <- plot_combined_heatmap(
    biomass_summaries$absolute, structural_summaries$absolute,
    "Ecosystem Assessment: Prydz Bay (Absolute)",
    OUTPUT_DIR, "heatmap_combined_absolute")
  plots$combined_exploit <- plot_combined_heatmap(
    biomass_summaries$exploit, structural_summaries$exploit,
    "Ecosystem Assessment: Exploitation Impact",
    OUTPUT_DIR, "heatmap_combined_exploitation")
  plots$combined_climate <- plot_combined_heatmap(
    biomass_summaries$climate, structural_summaries$climate,
    "Ecosystem Assessment: Climate Impact",
    OUTPUT_DIR, "heatmap_combined_climate")

  cat("  Combined heatmaps (50% CI / IQR)...\n")
  plots$combined_absolute_50ci <- plot_combined_heatmap(
    biomass_summaries$absolute, structural_summaries$absolute,
    "Ecosystem Assessment: Prydz Bay (Absolute, IQR)",
    OUTPUT_DIR, "heatmap_combined_absolute_50ci",
    envelope_col = "prop_outside_b0_50", null_expected = STRUCTURAL_NULL_50CI)
  plots$combined_exploit_50ci <- plot_combined_heatmap(
    biomass_summaries$exploit, structural_summaries$exploit,
    "Ecosystem Assessment: Exploitation Impact (IQR)",
    OUTPUT_DIR, "heatmap_combined_exploitation_50ci",
    envelope_col = "prop_outside_b0_50", null_expected = STRUCTURAL_NULL_50CI)
  plots$combined_climate_50ci <- plot_combined_heatmap(
    biomass_summaries$climate, structural_summaries$climate,
    "Ecosystem Assessment: Climate Impact (IQR)",
    OUTPUT_DIR, "heatmap_combined_climate_50ci",
    envelope_col = "prop_outside_b0_50", null_expected = STRUCTURAL_NULL_50CI)

  cat("  Category C: Exploitation rate heatmap...\n")
  plots$exploitation_F <- plot_exploitation_heatmap(exploitation_summary, OUTPUT_DIR)

  # 7. Save outputs
  cat("\n=============================================================\n")
  cat("SAVING OUTPUTS\n")
  cat("=============================================================\n\n")
  cat("Large data files (RDS) -> Output_large_files/ecosystem_assessment/\n")
  saveRDS(fishing_raw, file.path(OUTPUT_DIR_LARGE, "fishing_metrics_raw.rds"))
  saveRDS(climate_raw, file.path(OUTPUT_DIR_LARGE, "climate_only_metrics_raw.rds"))
  saveRDS(b0_reference, file.path(OUTPUT_DIR_LARGE, "b0_reference.rds"))
  saveRDS(paired_data, file.path(OUTPUT_DIR_LARGE, "paired_data.rds"))
  saveRDS(empirical_thresholds, file.path(OUTPUT_DIR_LARGE, "empirical_thresholds.rds"))
  # Also save to standard output dir for diagnostics script
  saveRDS(fishing_raw, file.path(OUTPUT_DIR, "fishing_metrics_raw.rds"))
  saveRDS(climate_raw, file.path(OUTPUT_DIR, "climate_only_metrics_raw.rds"))
  saveRDS(b0_reference, file.path(OUTPUT_DIR, "b0_reference.rds"))
  saveRDS(empirical_thresholds, file.path(OUTPUT_DIR, "empirical_thresholds.rds"))
  cat("Summary csvs/plots -> ecosystem_assessment_outputs/\n")
  for (comp in c("exploit", "absolute", "climate")) {
    write.csv(biomass_summaries[[comp]],
              file.path(OUTPUT_DIR, sprintf("summary_biomass_%s.csv", comp)), row.names = FALSE)
    write.csv(structural_summaries[[comp]],
              file.path(OUTPUT_DIR, sprintf("summary_structural_%s.csv", comp)), row.names = FALSE)
  }
  write.csv(exploitation_summary,
            file.path(OUTPUT_DIR, "summary_exploitation_rates.csv"), row.names = FALSE)
  if (!is.null(validation)) {
    if (!is.null(validation$biomass))
      write.csv(validation$biomass, file.path(OUTPUT_DIR, "validation_biomass.csv"), row.names = FALSE)
    if (!is.null(validation$structural))
      write.csv(validation$structural, file.path(OUTPUT_DIR, "validation_structural.csv"), row.names = FALSE)
  }

  end_time <- Sys.time()
  cat(sprintf("\n=============================================================\n"))
  cat(sprintf("ASSESSMENT COMPLETE (%.1f minutes)\n", difftime(end_time, start_time, units = "mins")))
  cat("=============================================================\n")

  return(list(
    fishing_raw = fishing_raw, climate_raw = climate_raw,
    b0_reference = b0_reference, paired_data = paired_data,
    empirical_thresholds = empirical_thresholds,
    biomass_summaries = biomass_summaries,
    structural_summaries = structural_summaries,
    exploitation_summary = exploitation_summary,
    validation = validation, plots = plots
  ))
}
