###############################################################################
# Prydz Bay Ecosystem Assessment Framework
# 
# Comprehensive ecosystem-level assessment comparing exploitation scenarios
# (climate + fishing/whaling) against climate-only baselines.
#
# Based on: Morrison et al. (2024), Link & Watson (2019), Link et al. (2015)
#
# Outputs:
#   - Heatmap of ecosystem metrics across decades
#   - Individual metric time series
#   - Summary statistics and comparison tables
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
library(parallel)

# Configuration
OUTPUT_DIR <- "ecosystem_assessment_outputs"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

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
  end = seq(1850, 2010, by = 10)
)

# Historical period labels for grouping
HISTORICAL_PERIODS <- list(
  pre_whaling = c(1841, 1880),
  early_whaling = c(1900, 1920),
  peak_whaling = c(1930, 1965),
  post_whaling = c(1970, 1976),
  krill_fishing = c(1977, 1995),
  modern = c(2001, 2010)
)

###############################################################################
# Species Group Definitions
###############################################################################

# Define species groups based on model structure
# Note: Baleen whales excluded from apex predators as they feed on LTL prey
SPECIES_GROUPS <- list(
  # Baleen whales - primary focus (feed on LTL, not apex predators)
  baleen_whales = c("baleen whales", "minke whales"),
  
  # All whales including toothed
  all_whales = c("baleen whales", "minke whales", "sperm whales", "orca"),
  
  # True apex predators - high TL species that feed on other predators
  # Excludes baleen whales as they feed primarily on krill and zooplankton
  apex_predators = c("sperm whales", "orca", "leopard seals"),
  
  # All marine mammals
  marine_mammals = c("baleen whales", "minke whales", "sperm whales", "orca",
                     "leopard seals", "small divers", "medium divers", "large divers"),
  
  # Seals and seabirds
  seals = c("leopard seals", "small divers", "medium divers", "large divers"),
  birds = c("flying birds"),
  
  # Fish groups
  fish = c("mesopelagic fishes", "bathypelagic fishes", 
           "shelf and coastal fishes", "toothfishes"),
  fish_large = c("toothfishes", "shelf and coastal fishes"),
  fish_small = c("mesopelagic fishes", "bathypelagic fishes"),
  
  # Lower trophic levels
  krill = c("antarctic krill"),
  ltl = c("antarctic krill", "other krill", "mesozooplankton", 
          "other macrozooplankton", "salps"),
  
  # Consumers (all non-plankton species)
  consumers = c("mesopelagic fishes", "bathypelagic fishes", 
                "shelf and coastal fishes", "flying birds", "small divers",
                "squids", "toothfishes", "leopard seals", "medium divers",
                "large divers", "minke whales", "orca", "sperm whales", 
                "baleen whales")
)

# Trophic levels from Trebilco et al. (2020) Prydz Bay Ecopath model
# Source: https://doi.org/10.1016/j.dsr2.2019.07.001 Table 2
# "Alternative energy pathways in Southern Ocean food webs"
TROPHIC_LEVELS <- c(
  # Zooplankton and LTL
  "mesozooplankton" = 3.272,       # From Ecopath balanced model
  "other krill" = 2.398,           # Euphausiidae other than E. superba
  "other macrozooplankton" = 3.231,# Macrozooplankton group
  "antarctic krill" = 2.398,       # Euphausia superba
  "salps" = 2.284,                 # Salpa thompsoni
  
  # Fish
  "mesopelagic fishes" = 3.539,    # Myctophidae dominated
  "bathypelagic fishes" = 4.055,   # Using Antarctic silverfish TL
  "shelf and coastal fishes" = 4.281, # Other fish group
  "toothfishes" = 4.966,           # Large toothfish (Dissostichus spp.)
  
  # Birds
  "flying birds" = 4.103,          # Mixed seabirds
  
  # Seals - mapped to Ecopath groups
  "small divers" = 3.787,          # Using Adelie penguin TL as proxy
  "medium divers" = 4.999,         # Using Weddell seal TL
  "large divers" = 5.075,          # Using S. elephant seal TL
  "leopard seals" = 4.858,         # Hydrurga leptonyx
  
  # Cephalopods
  "squids" = 4.336,                # Cephalopods group
  
  # Whales
  "minke whales" = 3.955,          # Balaenoptera bonaerensis
  "orca" = 5.301,                  # Orcinus orca (killer whales)
  "sperm whales" = 5.342,          # Physeter macrocephalus
  "baleen whales" = 3.867          # Large baleen whales (fin, humpback, blue)
)

###############################################################################
# Reference Points (Literature-based)
###############################################################################

REFERENCE_POINTS <- list(
  # Exploitation rates
  exploitation_rate = list(
    target_general = 0.2,
    target_k_strategist = 0.04,  # Whales, large sharks
    limit = 0.4
  ),
  
  # Depletion ratios (B/B0 or B/B_climate)
  depletion = list(
    good = 0.75,        # CCAMLR krill predator target
    target = 0.54,      # IWC lower target
    limit = 0.40,       # BMSY proxy
    critical = 0.20     # Severely depleted
  ),
  
  # Large fish indicator
  lfi = list(
    target = 0.30,
    limit = 0.15
  ),
  
  # Mean trophic level
  mtl = list(
    healthy = 3.5,
    degraded = 3.0
  ),
  
  # Size spectrum slope (typical range)
  spectrum_slope = list(
    healthy_range = c(-2.5, -1.5),  # Typical marine community
    steeper_limit = -3.0,
    shallower_limit = -1.0
  )
)

###############################################################################
# Metric Calculation Functions
# 
# Uses mizer built-in functions where available:
# - getCommunitySlope(): size spectrum slope
# - getProportionOfLargeFish(): LFI indicator
# - getMeanWeight(): community mean weight
# - getMeanMaxWeight(): mean maximum weight
###############################################################################

#' Calculate total biomass by species group for a time range
#' @param sim MizerSim object
#' @param time_range Vector of time indices to average over
#' @param species_group Vector of species names (NULL = all)
#' @return Numeric: mean biomass in g/m^2
calculate_group_biomass <- function(sim, time_range = NULL, species_group = NULL) {
  biomass <- getBiomass(sim)
  
  if (is.null(time_range)) {
    time_range <- 1:nrow(biomass)
  }
  
  if (is.null(species_group)) {
    total <- rowSums(biomass[time_range, , drop = FALSE])
  } else {
    valid_sp <- species_group[species_group %in% colnames(biomass)]
    if (length(valid_sp) == 0) return(NA)
    total <- rowSums(biomass[time_range, valid_sp, drop = FALSE])
  }
  
  return(mean(total, na.rm = TRUE))
}


#' Calculate size spectrum slope using mizer::getCommunitySlope
#' @param sim MizerSim object
#' @param time_range Vector of time indices to average over
#' @param min_w Minimum weight to include (g)
#' @param max_w Maximum weight to include (g)
#' @return Numeric: mean slope across time range
calculate_spectrum_slope <- function(sim, time_range = NULL, 
                                      min_w = 1, max_w = 1e6) {
  # Use mizer's built-in getCommunitySlope function
  slope_data <- tryCatch({
    mizer::getCommunitySlope(sim, min_w = min_w, max_w = max_w, biomass = TRUE)
  }, error = function(e) NULL)
  
  if (is.null(slope_data)) return(NA)
  
  if (is.null(time_range)) {
    time_range <- 1:nrow(slope_data)
  }
  
  # Return mean slope across time range
  return(mean(slope_data[time_range, "slope"], na.rm = TRUE))
}


#' Calculate Large Fish Indicator using mizer::getProportionOfLargeFish
#' @param sim MizerSim object
#' @param time_range Vector of time indices to average over
#' @param species_group Vector of species names
#' @param threshold_w Size threshold for "large" fish (g)
#' @param min_w Minimum weight for community (g)
#' @param max_w Maximum weight for community (g)
#' @return Numeric: mean LFI across time range
calculate_lfi_mizer <- function(sim, time_range = NULL, 
                                 species_group = NULL,
                                 threshold_w = 1000,
                                 min_w = 10, max_w = 1e6) {
  # Use mizer's built-in getProportionOfLargeFish function
  lfi_data <- tryCatch({
    mizer::getProportionOfLargeFish(sim, 
                                     species = species_group,
                                     min_w = min_w, 
                                     max_w = max_w,
                                     threshold_w = threshold_w,
                                     biomass_proportion = TRUE)
  }, error = function(e) NULL)
  
  if (is.null(lfi_data)) return(NA)
  
  if (is.null(time_range)) {
    time_range <- 1:length(lfi_data)
  }
  
  return(mean(lfi_data[time_range], na.rm = TRUE))
}


#' Calculate mean weight using mizer::getMeanWeight
#' @param sim MizerSim object
#' @param time_range Vector of time indices to average over
#' @param species_group Vector of species names
#' @param min_w Minimum weight (g)
#' @param max_w Maximum weight (g)
#' @return Numeric: mean individual weight across time range
calculate_mean_weight_mizer <- function(sim, time_range = NULL,
                                         species_group = NULL,
                                         min_w = 1, max_w = 1e8) {
  # Use mizer's built-in getMeanWeight function
  mw_data <- tryCatch({
    mizer::getMeanWeight(sim, 
                          species = species_group,
                          min_w = min_w, 
                          max_w = max_w)
  }, error = function(e) NULL)
  
  if (is.null(mw_data)) return(NA)
  
  if (is.null(time_range)) {
    time_range <- 1:length(mw_data)
  }
  
  return(mean(mw_data[time_range], na.rm = TRUE))
}


#' Calculate mean max weight using mizer::getMeanMaxWeight
#' @param sim MizerSim object
#' @param time_range Vector of time indices to average over
#' @param species_group Vector of species names
#' @return Numeric: mean maximum weight across time range
calculate_mean_max_weight_mizer <- function(sim, time_range = NULL,
                                             species_group = NULL) {
  # Use mizer's built-in getMeanMaxWeight function
  mmw_data <- tryCatch({
    mizer::getMeanMaxWeight(sim, 
                             species = species_group,
                             measure = "biomass")
  }, error = function(e) NULL)
  
  if (is.null(mmw_data)) return(NA)
  
  if (is.null(time_range)) {
    time_range <- 1:length(mmw_data)
  }
  
  return(mean(mmw_data[time_range], na.rm = TRUE))
}


#' Calculate mean trophic level (biomass-weighted)
#' @param sim MizerSim object
#' @param time_range Vector of time indices
#' @param tl_vector Named vector of trophic levels by species
#' @return Numeric: biomass-weighted mean TL
calculate_mean_tl <- function(sim, time_range = NULL, 
                               tl_vector = TROPHIC_LEVELS) {
  biomass <- getBiomass(sim)
  
  if (is.null(time_range)) {
    time_range <- nrow(biomass)
  }
  
  # Match species
  sp_names <- colnames(biomass)
  valid_sp <- sp_names[sp_names %in% names(tl_vector)]
  
  if (length(valid_sp) == 0) return(NA)
  
  # Calculate weighted mean for each time step
  mtl_values <- numeric(length(time_range))
  
  for (i in seq_along(time_range)) {
    t <- time_range[i]
    b <- biomass[t, valid_sp]
    tl <- tl_vector[valid_sp]
    mtl_values[i] <- sum(b * tl) / sum(b)
  }
  
  return(mean(mtl_values, na.rm = TRUE))
}


#' Calculate high trophic level indicator (% biomass at TL >= threshold)
#' @param sim MizerSim object
#' @param time_range Vector of time indices
#' @param tl_threshold Minimum TL to include (default 4.0)
#' @param tl_vector Named vector of trophic levels
#' @return Numeric: proportion of consumer biomass at high TL
calculate_htl_indicator <- function(sim, time_range = NULL, 
                                     tl_threshold = 4.0,
                                     tl_vector = TROPHIC_LEVELS) {
  biomass <- getBiomass(sim)
  
  if (is.null(time_range)) {
    time_range <- nrow(biomass)
  }
  
  sp_names <- colnames(biomass)
  valid_sp <- sp_names[sp_names %in% names(tl_vector)]
  
  # Identify high TL and consumer species
  high_tl_sp <- valid_sp[tl_vector[valid_sp] >= tl_threshold]
  consumer_sp <- valid_sp[tl_vector[valid_sp] > 2.0]
  
  if (length(high_tl_sp) == 0 || length(consumer_sp) == 0) return(NA)
  
  htl_values <- numeric(length(time_range))
  
  for (i in seq_along(time_range)) {
    t <- time_range[i]
    high_tl_biomass <- sum(biomass[t, high_tl_sp])
    consumer_biomass <- sum(biomass[t, consumer_sp])
    htl_values[i] <- high_tl_biomass / consumer_biomass
  }
  
  return(mean(htl_values, na.rm = TRUE))
}


#' Calculate predator-prey biomass ratio
#' @param sim MizerSim object
#' @param time_range Vector of time indices
#' @param predator_group Predator species
#' @param prey_group Prey species
#' @return Numeric: ratio of predator to prey biomass
calculate_predator_prey_ratio <- function(sim, time_range = NULL,
                                           predator_group = NULL,
                                           prey_group = NULL) {
  if (is.null(predator_group)) {
    predator_group <- SPECIES_GROUPS$apex_predators
  }
  if (is.null(prey_group)) {
    prey_group <- SPECIES_GROUPS$ltl
  }
  
  predator_biomass <- calculate_group_biomass(sim, time_range, predator_group)
  prey_biomass <- calculate_group_biomass(sim, time_range, prey_group)
  
  if (is.na(predator_biomass) || is.na(prey_biomass) || prey_biomass == 0) {
    return(NA)
  }
  
  return(predator_biomass / prey_biomass)
}


#' Calculate exploitation rate (catch/biomass) for species groups
#' @param sim MizerSim object
#' @param time_range Vector of time indices to average over
#' @param species_group Vector of species names (NULL = all fished)
#' @return Numeric: mean exploitation rate (F/B proxy)
calculate_exploitation_rate <- function(sim, time_range = NULL, 
                                         species_group = NULL) {
  # Get yield (catch) and biomass
  catch_data <- tryCatch(getYield(sim), error = function(e) NULL)
  biomass_data <- getBiomass(sim)
  
  if (is.null(catch_data)) {
    # No fishing in this simulation
    return(0)
  }
  
  if (is.null(time_range)) {
    time_range <- seq_len(nrow(biomass_data))
  }
  
  sp_names <- colnames(biomass_data)
  
  if (is.null(species_group)) {
    # Use all species with non-zero catch
    total_catch <- colSums(catch_data[time_range, , drop = FALSE])
    species_group <- names(total_catch[total_catch > 0])
    if (length(species_group) == 0) return(0)
  }
  
  valid_sp <- species_group[species_group %in% sp_names]
  if (length(valid_sp) == 0) return(NA)
  
  # Calculate mean exploitation rate across time range
  exploitation_rates <- numeric(length(time_range))
  
  for (i in seq_along(time_range)) {
    t <- time_range[i]
    total_catch <- sum(catch_data[t, valid_sp, drop = TRUE], na.rm = TRUE)
    total_biomass <- sum(biomass_data[t, valid_sp, drop = TRUE], na.rm = TRUE)
    
    if (total_biomass > 0) {
      exploitation_rates[i] <- total_catch / total_biomass
    } else {
      exploitation_rates[i] <- NA
    }
  }
  
  return(mean(exploitation_rates, na.rm = TRUE))
}


#' Calculate exploitation rate for whales specifically
#' @param sim MizerSim object
#' @param time_range Vector of time indices
#' @return Numeric: mean exploitation rate for whales
calculate_whale_exploitation <- function(sim, time_range = NULL) {
  calculate_exploitation_rate(sim, time_range, SPECIES_GROUPS$all_whales)
}


#' Calculate exploitation rate for krill
#' @param sim MizerSim object  
#' @param time_range Vector of time indices
#' @return Numeric: mean exploitation rate for krill
calculate_krill_exploitation <- function(sim, time_range = NULL) {
  calculate_exploitation_rate(sim, time_range, SPECIES_GROUPS$krill)
}


###############################################################################
# Ensemble Processing Functions
###############################################################################

#' Extract metrics from a single simulation for specified decades
#' @param sim MizerSim object
#' @param decades_df Data frame with start/end years for each decade
#' @return Data frame with metrics for each decade
extract_simulation_metrics <- function(sim, decades_df = DECADES) {
  times <- as.numeric(dimnames(sim@n)$time)
  
  results <- list()
  
  for (i in 1:nrow(decades_df)) {
    decade <- decades_df[i, ]
    time_range <- which(times >= decade$start & times <= decade$end)
    
    if (length(time_range) == 0) next
    
    metrics <- data.frame(
      decade = decade$label,
      start_year = decade$start,
      end_year = decade$end,
      
      # Biomass metrics
      total_biomass = calculate_group_biomass(sim, time_range),
      whale_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$all_whales),
      baleen_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$baleen_whales),
      seal_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$seals),
      fish_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$fish),
      krill_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$krill),
      ltl_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$ltl),
      apex_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$apex_predators),
      
      # Size spectrum metrics (using mizer::getCommunitySlope)
      spectrum_slope = calculate_spectrum_slope(sim, time_range),
      
      # Trophic indicators
      mean_tl = calculate_mean_tl(sim, time_range),
      htl_indicator = calculate_htl_indicator(sim, time_range),
      
      # Structural indicators (using mizer built-in functions)
      large_fish_indicator = calculate_lfi_mizer(sim, time_range,
                                                   species_group = SPECIES_GROUPS$fish,
                                                   threshold_w = 1000),
      mean_weight = calculate_mean_weight_mizer(sim, time_range,
                                                  species_group = SPECIES_GROUPS$consumers),
      mean_max_weight = calculate_mean_max_weight_mizer(sim, time_range,
                                                         species_group = SPECIES_GROUPS$consumers),
      predator_prey_ratio = calculate_predator_prey_ratio(sim, time_range),
      
      # Exploitation metrics (catch/biomass)
      exploitation_total = calculate_exploitation_rate(sim, time_range),
      exploitation_whale = calculate_whale_exploitation(sim, time_range),
      exploitation_krill = calculate_krill_exploitation(sim, time_range),
      
      stringsAsFactors = FALSE
    )
    
    results[[i]] <- metrics
  }
  
  return(bind_rows(results))
}


#' Process entire ensemble and extract metrics
#' @param ensemble_file Path to ensemble RDS file
#' @param scenario_name Name for labeling
#' @param max_sims Maximum number of simulations to process
#' @return Data frame with metrics for all simulations and decades
process_ensemble_metrics <- function(ensemble_file, scenario_name, 
                                      max_sims = NULL) {
  cat(sprintf("\n=============================================================\n"))
  cat(sprintf("Processing %s ensemble\n", scenario_name))
  cat(sprintf("=============================================================\n\n"))
  
  # Load ensemble
  cat("Loading ensemble...\n")
  mc <- readRDS(ensemble_file)
  
  if ("simulations" %in% names(mc)) {
    simulations_list <- mc$simulations
  } else if (is.list(mc) && inherits(mc[[1]], "MizerSim")) {
    simulations_list <- mc
  } else {
    stop("Unrecognized ensemble structure")
  }
  
  n_sims <- length(simulations_list)
  if (!is.null(max_sims)) {
    n_sims <- min(n_sims, max_sims)
    simulations_list <- simulations_list[1:n_sims]
  }
  
  cat(sprintf("  Processing %d simulations\n", n_sims))
  
  # Process each simulation
  all_results <- list()
  pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
  
  for (i in seq_len(n_sims)) {
    setTxtProgressBar(pb, i)
    
    sim <- tryCatch(simulations_list[[i]], error = function(e) NULL)
    if (is.null(sim)) next
    
    metrics <- tryCatch({
      extract_simulation_metrics(sim)
    }, error = function(e) NULL)
    
    if (!is.null(metrics) && nrow(metrics) > 0) {
      metrics$sim_id <- i
      metrics$scenario <- scenario_name
      all_results[[length(all_results) + 1]] <- metrics
    }
  }
  close(pb)
  
  cat(sprintf("\n  Successfully processed %d simulations\n", length(all_results)))
  
  return(bind_rows(all_results))
}


###############################################################################
# Classification and Scoring Functions
###############################################################################

#' Calculate ensemble statistics for each decade
#' @param metrics_df Data frame from process_ensemble_metrics
#' @return Data frame with summary statistics by decade
calculate_ensemble_stats <- function(metrics_df) {
  metrics_df %>%
    group_by(decade, start_year, end_year, scenario) %>%
    summarise(
      n_sims = n(),
      across(
        c(total_biomass, whale_biomass, baleen_biomass, seal_biomass,
          fish_biomass, krill_biomass, ltl_biomass, apex_biomass,
          spectrum_slope, mean_tl, htl_indicator,
          large_fish_indicator, mean_weight, mean_max_weight, predator_prey_ratio,
          exploitation_total, exploitation_whale, exploitation_krill),
        list(
          median = ~median(.x, na.rm = TRUE),
          mean = ~mean(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          q05 = ~quantile(.x, 0.05, na.rm = TRUE),
          q25 = ~quantile(.x, 0.25, na.rm = TRUE),
          q75 = ~quantile(.x, 0.75, na.rm = TRUE),
          q95 = ~quantile(.x, 0.95, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = 'drop'
    )
}


#' Classify metric value using metric-specific scoring rules
#' 
#' Different metrics use different scoring approaches:
#' - Biomass metrics: depletion-based scoring (B/B_climate)
#' - Exploitation rates: absolute reference points (target F)
#' - Size spectrum: deviation from healthy range
#' - Trophic indicators: threshold-based scoring
#' 
#' @param value Value to classify
#' @param baseline_values Vector of baseline values for same decade (for relative metrics)
#' @param metric_config List with metric properties (type, thresholds, etc.)
#' @return List with score (0-1), category, and explanation
classify_metric_specific <- function(value, baseline_values, metric_config) {
  
  if (is.na(value)) {
    return(list(score = NA, category = "unknown", deviation = NA))
  }
  
  metric_type <- metric_config$type
  scoring <- metric_config$scoring
  
  # =========================================================================
  # DEPLETION-BASED SCORING (biomass metrics)
  # Score based on B/B_climate ratio with CCAMLR/IWC thresholds
  # =========================================================================
  if (scoring == "depletion") {
    if (length(baseline_values) == 0 || all(is.na(baseline_values))) {
      return(list(score = NA, category = "unknown", deviation = NA))
    }
    
    baseline_mean <- mean(baseline_values, na.rm = TRUE)
    if (is.na(baseline_mean) || baseline_mean == 0) {
      return(list(score = NA, category = "unknown", deviation = NA))
    }
    
    depletion <- value / baseline_mean
    
    # Thresholds from CCAMLR and IWC
    if (depletion >= 0.75) {
      score <- 1.0
      category <- "good"
    } else if (depletion >= 0.54) {
      score <- 0.75  # IWC lower target
      category <- "moderate"
    } else if (depletion >= 0.40) {
      score <- 0.5   # BMSY proxy
      category <- "poor"
    } else if (depletion >= 0.20) {
      score <- 0.25  # Severely depleted
      category <- "bad"
    } else {
      score <- 0.1   # Critical
      category <- "critical"
    }
    
    return(list(score = score, category = category, deviation = depletion))
  }
  
  # =========================================================================
  # EXPLOITATION RATE SCORING (F-based metrics)
  # Absolute reference points for fishing mortality
  # =========================================================================
  if (scoring == "exploitation") {
    # Get target F from config or use defaults
    target_f <- metric_config$target_f %||% 0.2
    limit_f <- metric_config$limit_f %||% 0.4
    
    if (value <= target_f * 0.5) {
      score <- 1.0
      category <- "good"
    } else if (value <= target_f) {
      score <- 0.75
      category <- "moderate"
    } else if (value <= limit_f) {
      score <- 0.5
      category <- "poor"
    } else if (value <= limit_f * 1.5) {
      score <- 0.25
      category <- "bad"
    } else {
      score <- 0.1
      category <- "critical"
    }
    
    return(list(score = score, category = category, deviation = value / target_f))
  }
  
  # =========================================================================
  # SLOPE/RANGE SCORING (size spectrum slope)
  # Score based on deviation from healthy range
  # =========================================================================
  if (scoring == "range") {
    healthy_low <- metric_config$healthy_low %||% -2.5
    healthy_high <- metric_config$healthy_high %||% -1.5
    limit_low <- metric_config$limit_low %||% -3.5
    limit_high <- metric_config$limit_high %||% -0.5
    
    if (value >= healthy_low && value <= healthy_high) {
      score <- 1.0
      category <- "good"
    } else if (value >= limit_low && value <= limit_high) {
      # Calculate distance from healthy range
      if (value < healthy_low) {
        dist <- (healthy_low - value) / (healthy_low - limit_low)
      } else {
        dist <- (value - healthy_high) / (limit_high - healthy_high)
      }
      score <- max(0.25, 0.75 - dist * 0.5)
      category <- ifelse(score >= 0.5, "moderate", "poor")
    } else {
      score <- 0.1
      category <- "critical"
    }
    
    return(list(score = score, category = category, deviation = value))
  }
  
  # =========================================================================
  # LFI SCORING (Large Fish Indicator)
  # Absolute threshold-based scoring
  # =========================================================================
  if (scoring == "lfi") {
    target_lfi <- metric_config$target %||% 0.30
    limit_lfi <- metric_config$limit %||% 0.15
    
    if (value >= target_lfi) {
      score <- 1.0
      category <- "good"
    } else if (value >= target_lfi * 0.75) {
      score <- 0.75
      category <- "moderate"
    } else if (value >= limit_lfi) {
      score <- 0.5
      category <- "poor"
    } else if (value >= limit_lfi * 0.5) {
      score <- 0.25
      category <- "bad"
    } else {
      score <- 0.1
      category <- "critical"
    }
    
    return(list(score = score, category = category, deviation = value / target_lfi))
  }
  
  # =========================================================================
  # TROPHIC LEVEL SCORING
  # Based on deviation from baseline with literature thresholds
  # =========================================================================
  if (scoring == "trophic") {
    if (length(baseline_values) == 0 || all(is.na(baseline_values))) {
      # Use absolute thresholds if no baseline
      if (value >= 3.5) {
        score <- 1.0
        category <- "good"
      } else if (value >= 3.25) {
        score <- 0.75
        category <- "moderate"
      } else if (value >= 3.0) {
        score <- 0.5
        category <- "poor"
      } else {
        score <- 0.25
        category <- "bad"
      }
      return(list(score = score, category = category, deviation = value))
    }
    
    baseline_mean <- mean(baseline_values, na.rm = TRUE)
    ratio <- value / baseline_mean
    
    if (ratio >= 0.98) {
      score <- 1.0
      category <- "good"
    } else if (ratio >= 0.95) {
      score <- 0.75
      category <- "moderate"
    } else if (ratio >= 0.90) {
      score <- 0.5
      category <- "poor"
    } else {
      score <- 0.25
      category <- "bad"
    }
    
    return(list(score = score, category = category, deviation = ratio))
  }
  
  # =========================================================================
  # DEFAULT: SD-DEVIATION SCORING (original method)
  # =========================================================================
  if (length(baseline_values) == 0 || all(is.na(baseline_values))) {
    return(list(score = NA, category = "unknown", deviation = NA))
  }
  
  baseline_mean <- mean(baseline_values, na.rm = TRUE)
  baseline_sd <- sd(baseline_values, na.rm = TRUE)
  
  if (is.na(baseline_sd) || baseline_sd == 0) {
    baseline_sd <- abs(baseline_mean) * 0.1
  }
  
  deviation <- (value - baseline_mean) / baseline_sd
  
  if (metric_type == "higher_better") {
    if (deviation >= -0.5) {
      score <- 1.0
      category <- "good"
    } else if (deviation >= -1.0) {
      score <- 0.75
      category <- "moderate"
    } else if (deviation >= -2.0) {
      score <- 0.5
      category <- "poor"
    } else if (deviation >= -3.0) {
      score <- 0.25
      category <- "bad"
    } else {
      score <- 0.1
      category <- "critical"
    }
  } else if (metric_type == "lower_better") {
    if (deviation <= 0.5) {
      score <- 1.0
      category <- "good"
    } else if (deviation <= 1.0) {
      score <- 0.75
      category <- "moderate"
    } else if (deviation <= 2.0) {
      score <- 0.5
      category <- "poor"
    } else if (deviation <= 3.0) {
      score <- 0.25
      category <- "bad"
    } else {
      score <- 0.1
      category <- "critical"
    }
  } else {
    # Neutral
    abs_dev <- abs(deviation)
    if (abs_dev <= 0.5) {
      score <- 1.0
      category <- "good"
    } else if (abs_dev <= 1.0) {
      score <- 0.75
      category <- "moderate"
    } else if (abs_dev <= 2.0) {
      score <- 0.5
      category <- "poor"
    } else if (abs_dev <= 3.0) {
      score <- 0.25
      category <- "bad"
    } else {
      score <- 0.1
      category <- "critical"
    }
  }
  
  return(list(score = score, category = category, deviation = deviation))
}


# Helper for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x


#' Classify metric value relative to baseline (legacy wrapper)
#' @param value Value to classify
#' @param baseline_values Vector of baseline values for same decade
#' @param metric_type "higher_better", "lower_better", or "neutral"
#' @return List with score (0-1) and category
classify_metric <- function(value, baseline_values, metric_type = "higher_better") {
  classify_metric_specific(value, baseline_values, 
                           list(type = metric_type, scoring = "default"))
}


#' Calculate depletion ratio (exploitation / climate-only)
#' @param exploit_value Exploitation scenario value
#' @param climate_value Climate-only baseline value
#' @return Numeric: depletion ratio
calculate_depletion_ratio <- function(exploit_value, climate_value) {
  if (is.na(exploit_value) || is.na(climate_value) || climate_value == 0) {
    return(NA)
  }
  return(exploit_value / climate_value)
}


###############################################################################
# Heatmap Generation Functions
###############################################################################

#' Create heatmap score matrix
#' @param fishing_stats Statistics for fishing ensemble
#' @param climate_stats Statistics for climate-only ensemble
#' @param fishing_raw Raw fishing metrics for distribution comparison
#' @param climate_raw Raw climate-only metrics
#' @return Data frame ready for heatmap plotting
create_heatmap_data <- function(fishing_stats, climate_stats,
                                 fishing_raw, climate_raw) {
  
  decades <- unique(fishing_stats$decade)
  
  # Define metrics with metric-specific scoring approaches
  # scoring types: "depletion", "exploitation", "range", "lfi", "trophic", "default"
  metrics_config <- list(
    # Biomass metrics - use depletion-based scoring (B/B_climate with CCAMLR thresholds)
    list(name = "Total Biomass", col = "total_biomass", 
         type = "higher_better", scoring = "depletion", group = "Biomass"),
    list(name = "Baleen Whale Biomass", col = "baleen_biomass", 
         type = "higher_better", scoring = "depletion", group = "Biomass"),
    list(name = "All Whale Biomass", col = "whale_biomass", 
         type = "higher_better", scoring = "depletion", group = "Biomass"),
    list(name = "Seal Biomass", col = "seal_biomass", 
         type = "higher_better", scoring = "depletion", group = "Biomass"),
    list(name = "Fish Biomass", col = "fish_biomass", 
         type = "higher_better", scoring = "depletion", group = "Biomass"),
    list(name = "Krill Biomass", col = "krill_biomass", 
         type = "higher_better", scoring = "depletion", group = "Biomass"),
    list(name = "LTL Biomass", col = "ltl_biomass", 
         type = "higher_better", scoring = "depletion", group = "Biomass"),
    list(name = "Apex Predator Biomass", col = "apex_biomass", 
         type = "higher_better", scoring = "depletion", group = "Biomass"),
    
    # Size spectrum - healthy range scoring
    list(name = "Size Spectrum Slope", col = "spectrum_slope", 
         type = "neutral", scoring = "range", group = "Structure",
         healthy_low = -2.5, healthy_high = -1.5, limit_low = -3.5, limit_high = -0.5),
    
    # Trophic indicators - trophic-specific scoring
    list(name = "Mean Trophic Level", col = "mean_tl", 
         type = "higher_better", scoring = "trophic", group = "Trophic"),
    list(name = "High TL Indicator", col = "htl_indicator", 
         type = "higher_better", scoring = "depletion", group = "Trophic"),
    
    # LFI - absolute threshold scoring
    list(name = "Large Fish Indicator", col = "large_fish_indicator", 
         type = "higher_better", scoring = "lfi", group = "Structure",
         target = 0.30, limit = 0.15),
    
    # Structural - depletion-based
    list(name = "Mean Individual Weight", col = "mean_weight", 
         type = "higher_better", scoring = "depletion", group = "Structure"),
    list(name = "Mean Max Weight", col = "mean_max_weight", 
         type = "higher_better", scoring = "depletion", group = "Structure"),
    list(name = "Predator-Prey Ratio", col = "predator_prey_ratio", 
         type = "neutral", scoring = "default", group = "Structure"),
    
    # Exploitation rates - absolute F reference points
    list(name = "Total Exploitation Rate", col = "exploitation_total", 
         type = "lower_better", scoring = "exploitation", group = "Exploitation",
         target_f = 0.2, limit_f = 0.4),
    list(name = "Whale Exploitation Rate", col = "exploitation_whale", 
         type = "lower_better", scoring = "exploitation", group = "Exploitation",
         target_f = 0.04, limit_f = 0.08),  # K-strategist targets
    list(name = "Krill Exploitation Rate", col = "exploitation_krill", 
         type = "lower_better", scoring = "exploitation", group = "Exploitation",
         target_f = 0.1, limit_f = 0.2)     # Forage fish/krill targets
  )
  
  heatmap_results <- list()
  
  for (decade in decades) {
    fishing_decade <- fishing_stats %>% filter(decade == !!decade)
    climate_decade <- climate_stats %>% filter(decade == !!decade)
    
    fishing_raw_decade <- fishing_raw %>% filter(decade == !!decade)
    climate_raw_decade <- climate_raw %>% filter(decade == !!decade)
    
    for (metric in metrics_config) {
      # Get median values
      fishing_median_col <- paste0(metric$col, "_median")
      
      if (!(fishing_median_col %in% names(fishing_decade))) next
      
      fishing_value <- fishing_decade[[fishing_median_col]]
      
      # Get raw climate values for this decade
      if (metric$col %in% names(climate_raw_decade)) {
        climate_values <- climate_raw_decade[[metric$col]]
        climate_values <- climate_values[!is.na(climate_values)]
      } else {
        climate_values <- numeric(0)
      }
      
      # Classify using metric-specific scoring
      classification <- classify_metric_specific(fishing_value, climate_values, metric)
      
      # Calculate depletion ratio
      climate_median_col <- paste0(metric$col, "_median")
      climate_value <- climate_decade[[climate_median_col]]
      depletion <- calculate_depletion_ratio(fishing_value, climate_value)
      
      heatmap_results[[length(heatmap_results) + 1]] <- data.frame(
        decade = decade,
        metric_name = metric$name,
        metric_group = metric$group,
        scoring_method = metric$scoring,
        fishing_median = fishing_value,
        climate_median = climate_value,
        depletion_ratio = depletion,
        score = classification$score,
        category = classification$category,
        deviation = classification$deviation,
        stringsAsFactors = FALSE
      )
    }
  }
  
  return(bind_rows(heatmap_results))
}


#' Plot ecosystem assessment heatmap
#' @param heatmap_data Data frame from create_heatmap_data
#' @param output_path Path for saving figure
#' @return ggplot object
plot_ecosystem_heatmap <- function(heatmap_data, output_path) {
  
  # Order decades chronologically
  decade_order <- unique(heatmap_data$decade)
  heatmap_data$decade <- factor(heatmap_data$decade, levels = decade_order)
  
  # Order metrics by group
  metric_order <- c(
    # Biomass group
    "Total Biomass", "Baleen Whale Biomass", "All Whale Biomass",
    "Seal Biomass", "Fish Biomass", "Krill Biomass", 
    "LTL Biomass", "Apex Predator Biomass",
    # Trophic group
    "Mean Trophic Level", "High TL Indicator",
    # Structure group
    "Size Spectrum Slope", "Large Fish Indicator", 
    "Mean Individual Weight", "Mean Max Weight", "Predator-Prey Ratio",
    # Exploitation group
    "Total Exploitation Rate", "Whale Exploitation Rate", "Krill Exploitation Rate"
  )
  
  heatmap_data$metric_name <- factor(heatmap_data$metric_name, 
                                      levels = rev(metric_order))
  
  # Create plot using scores
  p <- ggplot(heatmap_data, aes(x = decade, y = metric_name, fill = score)) +
    geom_tile(color = "white", linewidth = 0.5) +
    
    # Color scale: red = poor, yellow = moderate, green = good
    scale_fill_gradientn(
      colours = c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#91cf60", "#1a9850"),
      values = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      limits = c(0, 1),
      na.value = "grey80",
      name = "Ecosystem\nCondition\nScore"
    ) +
    
    labs(
      title = "Prydz Bay Marine Ecosystem Assessment",
      subtitle = "Fishing/Whaling Scenario vs. Climate-Only Baseline Comparison",
      x = "Decade",
      y = "Ecosystem Metric"
    ) +
    
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      legend.position = "right",
      panel.grid = element_blank()
    )
  
  # Save plot
  ggsave(file.path(output_path, "ecosystem_heatmap.png"), 
         p, width = 14, height = 8, dpi = 300)
  ggsave(file.path(output_path, "ecosystem_heatmap.pdf"), 
         p, width = 14, height = 8)
  
  return(p)
}


#' Plot depletion ratio heatmap
#' @param heatmap_data Data frame from create_heatmap_data
#' @param output_path Path for saving figure
#' @return ggplot object
plot_depletion_heatmap <- function(heatmap_data, output_path) {
  
  # Order decades and metrics
  decade_order <- unique(heatmap_data$decade)
  heatmap_data$decade <- factor(heatmap_data$decade, levels = decade_order)
  
  metric_order <- c(
    "Total Biomass", "Baleen Whale Biomass", "All Whale Biomass",
    "Seal Biomass", "Fish Biomass", "Krill Biomass", 
    "LTL Biomass", "Apex Predator Biomass",
    "Mean Trophic Level", "High TL Indicator",
    "Size Spectrum Slope", "Large Fish Indicator", 
    "Mean Individual Weight", "Mean Max Weight", "Predator-Prey Ratio",
    "Total Exploitation Rate", "Whale Exploitation Rate", "Krill Exploitation Rate"
  )
  
  heatmap_data$metric_name <- factor(heatmap_data$metric_name, 
                                      levels = rev(metric_order))
  
  # Create depletion plot
  p <- ggplot(heatmap_data, aes(x = decade, y = metric_name, fill = depletion_ratio)) +
    geom_tile(color = "white", linewidth = 0.5) +
    
    # Diverging color scale centered on 1.0
    scale_fill_gradientn(
      colours = c("#d73027", "#fc8d59", "#fee08b", "#ffffbf", "#d9ef8b", "#91cf60", "#1a9850"),
      values = scales::rescale(c(0, 0.25, 0.5, 1.0, 1.5, 2.0, 3.0)),
      limits = c(0, 3),
      na.value = "grey80",
      name = "Depletion\nRatio\n(Fishing/\nClimate-only)"
    ) +
    
    # Add reference line annotations
    geom_vline(xintercept = c(7.5, 10.5, 13.5), linetype = "dashed", 
               color = "grey40", alpha = 0.5) +
    
    labs(
      title = "Prydz Bay Ecosystem Depletion Assessment",
      subtitle = "Ratio of Fishing Scenario to Climate-Only Baseline (1.0 = no difference)",
      x = "Decade",
      y = "Ecosystem Metric"
    ) +
    
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      legend.position = "right",
      panel.grid = element_blank()
    )
  
  # Save plot
  ggsave(file.path(output_path, "depletion_heatmap.png"), 
         p, width = 14, height = 8, dpi = 300)
  ggsave(file.path(output_path, "depletion_heatmap.pdf"), 
         p, width = 14, height = 8)
  
  return(p)
}


###############################################################################
# Main Execution
###############################################################################

main_ecosystem_assessment <- function(max_sims = NULL) {
  
  cat("=============================================================\n")
  cat("PRYDZ BAY ECOSYSTEM ASSESSMENT\n")
  cat("=============================================================\n\n")
  
  start_time <- Sys.time()
  
  # 1. Process fishing ensemble
  fishing_raw <- process_ensemble_metrics(
    ENSEMBLE_PATHS$fishing, 
    "Fishing",
    max_sims = max_sims
  )
  
  # 2. Process climate-only ensemble
  climate_raw <- process_ensemble_metrics(
    ENSEMBLE_PATHS$climate_only, 
    "Climate-only",
    max_sims = max_sims
  )
  
  # 3. Calculate ensemble statistics
  cat("\nCalculating ensemble statistics...\n")
  fishing_stats <- calculate_ensemble_stats(fishing_raw)
  climate_stats <- calculate_ensemble_stats(climate_raw)
  
  # 4. Create heatmap data
  cat("Creating heatmap classifications...\n")
  heatmap_data <- create_heatmap_data(
    fishing_stats, climate_stats,
    fishing_raw, climate_raw
  )
  
  # 5. Generate visualizations
  cat("\nGenerating visualizations...\n")
  p1 <- plot_ecosystem_heatmap(heatmap_data, OUTPUT_DIR)
  p2 <- plot_depletion_heatmap(heatmap_data, OUTPUT_DIR)
  
  # 6. Save results
  cat("\nSaving results...\n")
  
  saveRDS(fishing_raw, file.path(OUTPUT_DIR, "fishing_metrics_raw.rds"))
  saveRDS(climate_raw, file.path(OUTPUT_DIR, "climate_only_metrics_raw.rds"))
  write.csv(fishing_stats, file.path(OUTPUT_DIR, "fishing_metrics_stats.csv"), 
            row.names = FALSE)
  write.csv(climate_stats, file.path(OUTPUT_DIR, "climate_only_metrics_stats.csv"), 
            row.names = FALSE)
  write.csv(heatmap_data, file.path(OUTPUT_DIR, "heatmap_scores.csv"), 
            row.names = FALSE)
  
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "mins")
  
  cat("\n=============================================================\n")
  cat(sprintf("ASSESSMENT COMPLETE (%.1f minutes)\n", duration))
  cat("=============================================================\n")
  
  cat("\nOutput files:\n")
  cat("  - ecosystem_heatmap.png/pdf\n")
  cat("  - depletion_heatmap.png/pdf\n")
  cat("  - fishing_metrics_raw.rds\n")
  cat("  - climate_only_metrics_raw.rds\n")
  cat("  - fishing_metrics_stats.csv\n")
  cat("  - climate_only_metrics_stats.csv\n")
  cat("  - heatmap_scores.csv\n")
  
  return(list(
    fishing_raw = fishing_raw,
    climate_raw = climate_raw,
    fishing_stats = fishing_stats,
    climate_stats = climate_stats,
    heatmap_data = heatmap_data
  ))
}

# Run with test subset first (e.g., 50 simulations)
# results <- main_ecosystem_assessment(max_sims = 50)

# Run full analysis
# results <- main_ecosystem_assessment()
