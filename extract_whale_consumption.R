###############################################################################
# Whale Consumption Rate Extraction Script
# 
# Extracts consumption rates from Monte Carlo ensemble simulations, focusing on
# baleen whale and all whale consumption of lower trophic level prey, especially
# Antarctic krill, comparing modern context with pre-historical whaling impacts.
#
# Methodology:
#   Uses mizer's getDiet(proportion = FALSE) to directly extract consumption 
#   rates in grams/year, then integrates over population size structure.
#
# Outputs:
#   - Time series of whale consumption by prey species
#   - Comparison across key historical periods (pre-whaling, peak whaling, modern)
#   - Diet composition analysis for baleen whales and all whales
#   - Krill consumption estimates with uncertainty bounds
#   - Outputs formatted following Savoca et al. (2024) presentation style
#
# Key Literature:
#   - Savoca, M.S. et al. (2024) Baleen whale prey consumption based on 
#     high-resolution foraging measurements. Nature Communications.
#     https://doi.org/10.1038/s41467-024-51954-x
#   - This paper provides key context for interpreting whale consumption 
#     estimates and their ecosystem implications.
#
# Monte Carlo Ensemble File Locations:
#   - Primary: Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/combined_rerun_successful_sims_20250923_122211.rds
#   - Cleaned (2111 sims): Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds
#   - Climate-only: Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds
#
# Author: Generated for Prydz Bay mizer project
# Date: 2025
###############################################################################

library(therMizer)  # therMizer loads mizer and provides therMizerEncounter etc.
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(scales)

###############################################################################
# Configuration
###############################################################################

# Model domain area in m^2 (from 02_Preparing_Climate_Forcings.Rmd)
MODEL_DOMAIN_AREA <- 1.474341e+12  # m^2 (therMizer calibration domain, 05_therMizer_calibration_scale_model_domain.Rmd)

# Input paths - Monte Carlo ensemble locations documented here
MC_RESULTS_PATHS <- list(
  # Primary ensemble file (latest combined rerun results) - contains 2112 sims but 1 is invalid
  primary = "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/combined_rerun_successful_sims_20250923_122211.rds",
  
  # Cleaned ensemble with 2111 valid simulations (recommended - 1 invalid sim removed)
  cleaned_2111 = "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds",
  
  # Climate-only ensemble (unfished simulations for comparison)
  climate_only = "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
)

# Select which ensemble to use - using cleaned 2111 sims (1 invalid removed)
MC_RESULTS_FILE <- MC_RESULTS_PATHS$cleaned_2111

# Output directory
OUTPUT_DIR <- "whale_consumption_outputs"

# Key analysis periods
ANALYSIS_PERIODS <- list(
  pre_whaling = 1841:1880,           # Pre-industrial whaling baseline
  early_whaling = 1900:1920,         # Early industrial whaling
  peak_whaling = 1930:1965,          # Peak industrial whaling
  post_whaling = 1970:1976,          # Post-whaling moratorium, pre-krill fishing
  krill_fishing = 1977:1995,         # Krill fishing period
  modern = 2001:2010                 # Modern reference period
)

# Species groups
WHALE_SPECIES <- list(
  baleen = c("baleen whales", "minke whales"),
  toothed = c("sperm whales", "orca"),
  all_whales = c("baleen whales", "minke whales", "sperm whales", "orca")
)

# Lower trophic level prey of interest
LTL_PREY <- c("antarctic krill", "other krill", "mesozooplankton", 
              "other macrozooplankton", "salps")

# All prey species order (for consistent plotting)
SPECIES_ORDER <- c(
  "mesozooplankton", "other krill", "other macrozooplankton", "antarctic krill",
  "salps", "mesopelagic fishes", "bathypelagic fishes", "shelf and coastal fishes",
  "flying birds", "small divers", "squids", "toothfishes", "leopard seals",
  "medium divers", "large divers", "minke whales", "orca", "sperm whales", "baleen whales"
)

###############################################################################
# Helper Functions
###############################################################################

#' Extract consumption rates using getDiet(proportion = FALSE)
#' 
#' getDiet with proportion = FALSE returns consumption rate in grams/year
#' for each predator at each size, resolved by prey species.
#' 
#' The diet array dimensions are: predator x predator_size x prey
#' To get total population consumption, we integrate over predator sizes
#' weighted by abundance (n * dw).
#' 
#' @param sim A MizerSim object  
#' @param predator_species Vector of predator species names
#' @param time_step Which time step to extract
#' @param aggregate_predators If TRUE, sums consumption across predator species
#' @return Data frame with prey species and consumption rates (g/year)
extract_consumption <- function(sim, predator_species, time_step, aggregate_predators = TRUE) {
  params <- sim@params
  times <- as.numeric(dimnames(sim@n)$time)
  all_species <- dimnames(sim@n)$sp
  

  # Filter to valid predators
  predator_species <- predator_species[predator_species %in% all_species]
  if (length(predator_species) == 0) return(NULL)
  
  # Get time index
  t_idx <- which.min(abs(times - time_step))
  t <- times[t_idx]
  
  # Extract state at this time
  n <- sim@n[t_idx, , , drop = FALSE]
  dim(n) <- dim(sim@n)[2:3]
  dimnames(n) <- dimnames(sim@n)[2:3]
  
  n_pp <- sim@n_pp[t_idx, ]
  
  n_other <- sim@n_other[t_idx, ]
  if (!is.null(dimnames(sim@n_other))) {
    names(n_other) <- dimnames(sim@n_other)$component
  }
  
  # Get diet composition using getDiet(proportion = FALSE)
  # This returns consumption rate in grams/year per individual at each size
  # Array dimensions: predator x predator_size x prey
  diet <- tryCatch({
    getDiet(params, n = n, n_pp = n_pp, n_other = n_other, proportion = FALSE)
  }, error = function(e) NULL)
  
  if (is.null(diet)) return(NULL)
  
  w <- params@w
  dw <- params@dw
  
  # Initialize result - consumption by predator and prey
  consumption_list <- list()
  
  for (pred in predator_species) {
    pred_idx <- which(dimnames(diet)$predator == pred)
    if (length(pred_idx) == 0) next
    
    # Diet for this predator: size x prey
    # getDiet returns consumption rate per individual (g/year)
    pred_diet <- diet[pred_idx, , , drop = FALSE]
    dim(pred_diet) <- dim(diet)[2:3]
    dimnames(pred_diet) <- dimnames(diet)[2:3]
    
    # Abundance at each size for this predator
    n_pred <- n[pred, ]
    
    # Total population consumption by prey species:
    # Sum over sizes: consumption_per_individual[w] * n[w] * dw
    # This gives total grams/year consumed by the population
    total_consumption_by_prey <- rep(0, ncol(pred_diet))
    names(total_consumption_by_prey) <- colnames(pred_diet)
    
    for (prey_idx in 1:ncol(pred_diet)) {
      # Integrate consumption rate over population abundance
      total_consumption_by_prey[prey_idx] <- sum(pred_diet[, prey_idx] * n_pred * dw)
    }
    
    consumption_list[[pred]] <- total_consumption_by_prey
  }
  
  if (length(consumption_list) == 0) return(NULL)
  
  # Convert to data frame
  consumption_df <- do.call(rbind, consumption_list)
  consumption_df <- as.data.frame(consumption_df)
  consumption_df$predator <- rownames(consumption_df)
  
  if (aggregate_predators) {
    # Sum across predator species to get total consumption by prey
    prey_cols <- setdiff(names(consumption_df), "predator")
    total_consumption <- colSums(consumption_df[, prey_cols, drop = FALSE])
    return(data.frame(
      prey = names(total_consumption), 
      consumption_g_yr = as.numeric(total_consumption),
      stringsAsFactors = FALSE
    ))
  }
  
  return(consumption_df)
}

#' Calculate consumption timeseries for predator group on specific prey
#' 
#' Uses extract_consumption() at each time step to build a time series
#' of prey consumption by the specified predator group.
#' 
#' @param sim MizerSim object
#' @param predator_species Vector of predator names
#' @param prey_species Vector of prey names (if NULL, returns all prey)
#' @return Data frame with year, total consumption, and per-prey consumption
calc_prey_consumption_timeseries <- function(sim, predator_species, prey_species = NULL) {
  params <- sim@params
  times <- as.numeric(dimnames(sim@n)$time)
  all_species <- dimnames(sim@n)$sp
  
  # Validate species
  predator_species <- predator_species[predator_species %in% all_species]
  if (!is.null(prey_species)) {
    prey_species <- prey_species[prey_species %in% all_species]
  }
  
  if (length(predator_species) == 0) {
    return(NULL)
  }
  
  results <- list()
  
  for (t_idx in seq_along(times)) {
    t <- times[t_idx]
    
    # Use extract_consumption which uses getDiet(proportion = FALSE)
    consumption_df <- tryCatch({
      extract_consumption(sim, predator_species, t, aggregate_predators = TRUE)
    }, error = function(e) NULL)
    
    if (is.null(consumption_df)) next
    
    # Filter to prey of interest (if specified)
    if (!is.null(prey_species)) {
      consumption_df <- consumption_df[consumption_df$prey %in% prey_species, ]
    }
    
    if (nrow(consumption_df) == 0) next
    
    result_row <- data.frame(
      year = t,
      total_consumption = sum(consumption_df$consumption_g_yr),
      stringsAsFactors = FALSE
    )
    
    # Add per-prey columns
    target_prey <- if (!is.null(prey_species)) prey_species else unique(consumption_df$prey)
    for (prey in target_prey) {
      prey_val <- consumption_df$consumption_g_yr[consumption_df$prey == prey]
      result_row[[paste0("consumption_", gsub(" ", "_", prey))]] <- 
        if (length(prey_val) > 0) prey_val else 0
    }
    
    results[[length(results) + 1]] <- result_row
  }
  
  if (length(results) == 0) return(NULL)
  
  return(do.call(rbind, results))
}

###############################################################################
# Main Processing
###############################################################################

cat("=============================================================\n")
cat("Whale Consumption Rate Extraction\n")
cat("=============================================================\n\n")

# Create output directory
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# Document ensemble file locations
cat("Monte Carlo Ensemble File Locations:\n")
cat("-------------------------------------\n")
for (name in names(MC_RESULTS_PATHS)) {
  cat(sprintf("  %s: %s\n", name, MC_RESULTS_PATHS[[name]]))
  cat(sprintf("    Exists: %s\n", file.exists(MC_RESULTS_PATHS[[name]])))
}
cat("\n")

# Load Monte Carlo results
cat(sprintf("Loading Monte Carlo results from:\n  %s\n", MC_RESULTS_FILE))
mc <- readRDS(MC_RESULTS_FILE)

# Handle different possible structures
if ("simulations" %in% names(mc)) {
  simulations_list <- mc$simulations
  n_sims <- length(simulations_list)
} else if (is.list(mc) && inherits(mc[[1]], "MizerSim")) {
  simulations_list <- mc
  n_sims <- length(simulations_list)
} else {
  stop("Unrecognized MC results structure")
}

cat(sprintf("  Loaded %d simulations\n", n_sims))

# Get structure from first simulation
first_sim <- simulations_list[[1]]
sim_times <- as.numeric(dimnames(first_sim@n)$time)
all_species <- dimnames(first_sim@n)$sp
cat(sprintf("  Time range: %d - %d (%d years)\n", 
            min(sim_times), max(sim_times), length(sim_times)))
cat(sprintf("  Species (%d): %s\n", length(all_species), 
            paste(all_species, collapse = ", ")))

# Validate whale and prey species
whale_species_found <- WHALE_SPECIES$all_whales[WHALE_SPECIES$all_whales %in% all_species]
baleen_species_found <- WHALE_SPECIES$baleen[WHALE_SPECIES$baleen %in% all_species]
ltl_prey_found <- LTL_PREY[LTL_PREY %in% all_species]

cat(sprintf("\n  Whale species found: %s\n", paste(whale_species_found, collapse = ", ")))
cat(sprintf("  Baleen whale species found: %s\n", paste(baleen_species_found, collapse = ", ")))
cat(sprintf("  LTL prey found: %s\n", paste(ltl_prey_found, collapse = ", ")))

###############################################################################
# Extract consumption for each simulation
###############################################################################

cat("\n=============================================================\n")
cat("Extracting whale consumption time series...\n")
cat("=============================================================\n")

# Storage for all simulations
all_baleen_consumption <- list()
all_whale_consumption <- list()
all_krill_consumption_by_baleen <- list()
all_krill_consumption_by_whales <- list()

# Sample time points for diet analysis (every 10 years + key periods)
diet_sample_years <- unique(c(
  seq(1850, 2010, by = 10),
  ANALYSIS_PERIODS$pre_whaling,
  ANALYSIS_PERIODS$peak_whaling,
  ANALYSIS_PERIODS$modern
))
diet_sample_years <- sort(diet_sample_years[diet_sample_years %in% sim_times])

# Process simulations
pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
skipped <- 0

for (i in seq_len(n_sims)) {
  setTxtProgressBar(pb, i)
  
  sim <- tryCatch(simulations_list[[i]], error = function(e) NULL)
  if (is.null(sim)) {
    skipped <- skipped + 1
    next
  }
  
  # 1. Krill consumption by baleen whales (time series)
  krill_cons_baleen <- tryCatch({
    calc_prey_consumption_timeseries(sim, baleen_species_found, c("antarctic krill"))
  }, error = function(e) NULL)
  
  if (!is.null(krill_cons_baleen)) {
    all_krill_consumption_by_baleen[[length(all_krill_consumption_by_baleen) + 1]] <- krill_cons_baleen
  }
  
  # 2. Krill consumption by all whales (time series)
  krill_cons_all <- tryCatch({
    calc_prey_consumption_timeseries(sim, whale_species_found, c("antarctic krill"))
  }, error = function(e) NULL)
  
  if (!is.null(krill_cons_all)) {
    all_krill_consumption_by_whales[[length(all_krill_consumption_by_whales) + 1]] <- krill_cons_all
  }
  
  # 3. All LTL prey consumption by baleen whales (time series)
  ltl_cons_baleen <- tryCatch({
    calc_prey_consumption_timeseries(sim, baleen_species_found, ltl_prey_found)
  }, error = function(e) NULL)
  
  if (!is.null(ltl_cons_baleen)) {
    all_baleen_consumption[[length(all_baleen_consumption) + 1]] <- ltl_cons_baleen
  }
  
  # 4. All LTL prey consumption by all whales (time series)
  ltl_cons_whales <- tryCatch({
    calc_prey_consumption_timeseries(sim, whale_species_found, ltl_prey_found)
  }, error = function(e) NULL)
  
  if (!is.null(ltl_cons_whales)) {
    all_whale_consumption[[length(all_whale_consumption) + 1]] <- ltl_cons_whales
  }
}
close(pb)

cat(sprintf("\n  Successfully processed %d simulations (skipped %d)\n", 
            n_sims - skipped, skipped))

###############################################################################
# Calculate ensemble statistics
###############################################################################

cat("\n=============================================================\n")
cat("Calculating ensemble statistics...\n")
cat("=============================================================\n")

# Function to calculate ensemble stats from list of data frames
calc_ensemble_stats <- function(df_list, value_col = "total_consumption") {
  if (length(df_list) == 0) return(NULL)
  
  # Combine all simulations
  for (i in seq_along(df_list)) {
    df_list[[i]]$sim_id <- i
  }
  combined <- do.call(rbind, df_list)
  
  # Calculate statistics by year
  stats <- combined %>%
    group_by(year) %>%
    summarise(
      n_sims = n(),
      median = median(!!sym(value_col), na.rm = TRUE),
      mean = mean(!!sym(value_col), na.rm = TRUE),
      q05 = quantile(!!sym(value_col), 0.05, na.rm = TRUE),
      q25 = quantile(!!sym(value_col), 0.25, na.rm = TRUE),
      q75 = quantile(!!sym(value_col), 0.75, na.rm = TRUE),
      q95 = quantile(!!sym(value_col), 0.95, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(year)
  
  return(stats)
}

# Calculate stats for krill consumption by baleen whales
krill_baleen_stats <- calc_ensemble_stats(all_krill_consumption_by_baleen)
krill_whale_stats <- calc_ensemble_stats(all_krill_consumption_by_whales)

# Calculate stats for LTL (all lower trophic level) consumption by baleen whales
ltl_baleen_stats <- calc_ensemble_stats(all_baleen_consumption)
ltl_whale_stats <- calc_ensemble_stats(all_whale_consumption)

###############################################################################
# Period comparisons
###############################################################################

cat("\nCalculating period statistics...\n")

calc_period_stats <- function(stats_df, periods) {
  if (is.null(stats_df)) return(NULL)
  
  period_stats <- list()
  
  for (period_name in names(periods)) {
    period_years <- periods[[period_name]]
    period_data <- stats_df[stats_df$year %in% period_years, ]
    
    if (nrow(period_data) == 0) next
    
    period_stats[[period_name]] <- data.frame(
      period = period_name,
      start_year = min(period_years),
      end_year = max(period_years),
      n_years = nrow(period_data),
      mean_median = mean(period_data$median, na.rm = TRUE),
      mean_mean = mean(period_data$mean, na.rm = TRUE),
      min_q05 = min(period_data$q05, na.rm = TRUE),
      max_q95 = max(period_data$q95, na.rm = TRUE)
    )
  }
  
  return(do.call(rbind, period_stats))
}

krill_baleen_periods <- calc_period_stats(krill_baleen_stats, ANALYSIS_PERIODS)
krill_whale_periods <- calc_period_stats(krill_whale_stats, ANALYSIS_PERIODS)
ltl_baleen_periods <- calc_period_stats(ltl_baleen_stats, ANALYSIS_PERIODS)
ltl_whale_periods <- calc_period_stats(ltl_whale_stats, ANALYSIS_PERIODS)

###############################################################################
# Save outputs
###############################################################################

cat("\n=============================================================\n")
cat("Saving outputs...\n")
cat("=============================================================\n")

# Save time series
if (!is.null(krill_baleen_stats)) {
  # Convert to density (g m^-2 yr^-1)
  krill_baleen_stats$median_density <- krill_baleen_stats$median / MODEL_DOMAIN_AREA
  krill_baleen_stats$mean_density <- krill_baleen_stats$mean / MODEL_DOMAIN_AREA
  
  write.csv(krill_baleen_stats, 
            file.path(OUTPUT_DIR, "krill_consumption_by_baleen_whales_timeseries.csv"),
            row.names = FALSE)
  cat(sprintf("  Saved: krill_consumption_by_baleen_whales_timeseries.csv\n"))
}

if (!is.null(krill_whale_stats)) {
  krill_whale_stats$median_density <- krill_whale_stats$median / MODEL_DOMAIN_AREA
  krill_whale_stats$mean_density <- krill_whale_stats$mean / MODEL_DOMAIN_AREA
  
  write.csv(krill_whale_stats,
            file.path(OUTPUT_DIR, "krill_consumption_by_all_whales_timeseries.csv"),
            row.names = FALSE)
  cat(sprintf("  Saved: krill_consumption_by_all_whales_timeseries.csv\n"))
}

# Save period comparisons
if (!is.null(krill_baleen_periods)) {
  # Add relative change from pre-whaling
  baseline <- krill_baleen_periods$mean_median[krill_baleen_periods$period == "pre_whaling"]
  if (length(baseline) > 0 && !is.na(baseline)) {
    krill_baleen_periods$relative_to_baseline <- krill_baleen_periods$mean_median / baseline
    krill_baleen_periods$percent_of_baseline <- krill_baleen_periods$relative_to_baseline * 100
  }
  
  write.csv(krill_baleen_periods,
            file.path(OUTPUT_DIR, "krill_consumption_baleen_period_comparison.csv"),
            row.names = FALSE)
  cat(sprintf("  Saved: krill_consumption_baleen_period_comparison.csv\n"))
}

if (!is.null(krill_whale_periods)) {
  baseline <- krill_whale_periods$mean_median[krill_whale_periods$period == "pre_whaling"]
  if (length(baseline) > 0 && !is.na(baseline)) {
    krill_whale_periods$relative_to_baseline <- krill_whale_periods$mean_median / baseline
    krill_whale_periods$percent_of_baseline <- krill_whale_periods$relative_to_baseline * 100
  }
  
  write.csv(krill_whale_periods,
            file.path(OUTPUT_DIR, "krill_consumption_all_whales_period_comparison.csv"),
            row.names = FALSE)
  cat(sprintf("  Saved: krill_consumption_all_whales_period_comparison.csv\n"))
}

# Save LTL time series
if (!is.null(ltl_baleen_stats)) {
  ltl_baleen_stats$median_density <- ltl_baleen_stats$median / MODEL_DOMAIN_AREA
  ltl_baleen_stats$mean_density <- ltl_baleen_stats$mean / MODEL_DOMAIN_AREA
  
  write.csv(ltl_baleen_stats, 
            file.path(OUTPUT_DIR, "ltl_consumption_by_baleen_whales_timeseries.csv"),
            row.names = FALSE)
  cat(sprintf("  Saved: ltl_consumption_by_baleen_whales_timeseries.csv\n"))
}

if (!is.null(ltl_whale_stats)) {
  ltl_whale_stats$median_density <- ltl_whale_stats$median / MODEL_DOMAIN_AREA
  ltl_whale_stats$mean_density <- ltl_whale_stats$mean / MODEL_DOMAIN_AREA
  
  write.csv(ltl_whale_stats,
            file.path(OUTPUT_DIR, "ltl_consumption_by_all_whales_timeseries.csv"),
            row.names = FALSE)
  cat(sprintf("  Saved: ltl_consumption_by_all_whales_timeseries.csv\n"))
}

# Save LTL period comparisons
if (!is.null(ltl_baleen_periods)) {
  baseline <- ltl_baleen_periods$mean_median[ltl_baleen_periods$period == "pre_whaling"]
  if (length(baseline) > 0 && !is.na(baseline)) {
    ltl_baleen_periods$relative_to_baseline <- ltl_baleen_periods$mean_median / baseline
    ltl_baleen_periods$percent_of_baseline <- ltl_baleen_periods$relative_to_baseline * 100
  }
  
  write.csv(ltl_baleen_periods,
            file.path(OUTPUT_DIR, "ltl_consumption_baleen_period_comparison.csv"),
            row.names = FALSE)
  cat(sprintf("  Saved: ltl_consumption_baleen_period_comparison.csv\n"))
}

# Save raw simulation data as RDS
saveRDS(all_krill_consumption_by_baleen,
        file.path(OUTPUT_DIR, "krill_consumption_baleen_all_sims.rds"))
saveRDS(all_krill_consumption_by_whales,
        file.path(OUTPUT_DIR, "krill_consumption_all_whales_all_sims.rds"))
saveRDS(all_baleen_consumption,
        file.path(OUTPUT_DIR, "ltl_consumption_baleen_all_sims.rds"))
saveRDS(all_whale_consumption,
        file.path(OUTPUT_DIR, "ltl_consumption_all_whales_all_sims.rds"))
cat(sprintf("  Saved: raw simulation data (RDS)\n"))

###############################################################################
# Generate plots
###############################################################################

cat("\n=============================================================\n")
cat("Generating plots...\n")
cat("=============================================================\n")

# Unit conversion: grams to tonnes
G_TO_TONNES <- 1e-6

# Plot 1: Krill consumption time series - Baleen whales (in tonnes/year)
if (!is.null(krill_baleen_stats)) {
  # Create plot data in tonnes
  plot_data <- krill_baleen_stats %>%
    mutate(
      median_t = median * G_TO_TONNES,
      q05_t = q05 * G_TO_TONNES,
      q25_t = q25 * G_TO_TONNES,
      q75_t = q75 * G_TO_TONNES,
      q95_t = q95 * G_TO_TONNES
    )
  
  p1 <- ggplot(plot_data, aes(x = year)) +
    geom_ribbon(aes(ymin = q05_t, ymax = q95_t), alpha = 0.2, fill = "steelblue") +
    geom_ribbon(aes(ymin = q25_t, ymax = q75_t), alpha = 0.4, fill = "steelblue") +
    geom_line(aes(y = median_t), color = "steelblue", linewidth = 1) +
    # Add period markers
    geom_vline(xintercept = c(1930, 1965), linetype = "dashed", color = "red", alpha = 0.5) +
    geom_vline(xintercept = 1977, linetype = "dashed", color = "orange", alpha = 0.5) +
    annotate("text", x = 1947, y = max(plot_data$q95_t) * 0.95, 
             label = "Peak Whaling", size = 3, color = "red") +
    annotate("text", x = 1990, y = max(plot_data$q95_t) * 0.95,
             label = "Krill Fishing", size = 3, color = "orange") +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Antarctic Krill Consumption by Baleen Whales",
      subtitle = "Prydz Bay region, Monte Carlo ensemble (n = 2111)",
      x = "Year",
      y = "Krill consumption (tonnes/year)",
      caption = "Shaded areas: 50% and 90% credible intervals"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 11)
    )
  
  ggsave(file.path(OUTPUT_DIR, "krill_consumption_baleen_timeseries.png"),
         p1, width = 10, height = 6, dpi = 300)
  ggsave(file.path(OUTPUT_DIR, "krill_consumption_baleen_timeseries.pdf"),
         p1, width = 10, height = 6)
  cat(sprintf("  Saved: krill_consumption_baleen_timeseries plots\n"))
}

# Plot 2: Period comparison bar plot (in tonnes/year)
if (!is.null(krill_baleen_periods)) {
  krill_baleen_periods$period <- factor(krill_baleen_periods$period, 
                                         levels = names(ANALYSIS_PERIODS))
  # Convert to tonnes
  krill_baleen_periods$mean_median_t <- krill_baleen_periods$mean_median * G_TO_TONNES
  krill_baleen_periods$min_q05_t <- krill_baleen_periods$min_q05 * G_TO_TONNES
  krill_baleen_periods$max_q95_t <- krill_baleen_periods$max_q95 * G_TO_TONNES
  
  p2 <- ggplot(krill_baleen_periods, aes(x = period, y = mean_median_t)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    geom_errorbar(aes(ymin = min_q05_t, ymax = max_q95_t), width = 0.3) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Krill Consumption by Baleen Whales: Period Comparison",
      x = "Historical Period",
      y = "Mean annual krill consumption (tonnes/year)"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold")
    )
  
  ggsave(file.path(OUTPUT_DIR, "krill_consumption_period_comparison.png"),
         p2, width = 8, height = 6, dpi = 300)
  cat(sprintf("  Saved: krill_consumption_period_comparison.png\n"))
}

# Plot 3: Relative change from pre-whaling
if (!is.null(krill_baleen_periods) && "percent_of_baseline" %in% names(krill_baleen_periods)) {
  p3 <- ggplot(krill_baleen_periods, aes(x = period, y = percent_of_baseline)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "darkred") +
    labs(
      title = "Krill Consumption Relative to Pre-Whaling Baseline",
      subtitle = "Baleen whales, Prydz Bay",
      x = "Historical Period",
      y = "Percent of pre-whaling consumption (%)"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold")
    )
  
  ggsave(file.path(OUTPUT_DIR, "krill_consumption_relative_change.png"),
         p3, width = 8, height = 6, dpi = 300)
  cat(sprintf("  Saved: krill_consumption_relative_change.png\n"))
}

# Plot 4: LTL consumption time series - Baleen whales (in tonnes/year)
if (!is.null(ltl_baleen_stats)) {
  # Create plot data in tonnes
  ltl_plot_data <- ltl_baleen_stats %>%
    mutate(
      median_t = median * G_TO_TONNES,
      q05_t = q05 * G_TO_TONNES,
      q25_t = q25 * G_TO_TONNES,
      q75_t = q75 * G_TO_TONNES,
      q95_t = q95 * G_TO_TONNES
    )
  
  p4 <- ggplot(ltl_plot_data, aes(x = year)) +
    geom_ribbon(aes(ymin = q05_t, ymax = q95_t), alpha = 0.2, fill = "darkgreen") +
    geom_ribbon(aes(ymin = q25_t, ymax = q75_t), alpha = 0.4, fill = "darkgreen") +
    geom_line(aes(y = median_t), color = "darkgreen", linewidth = 1) +
    # Add period markers
    geom_vline(xintercept = c(1930, 1965), linetype = "dashed", color = "red", alpha = 0.5) +
    geom_vline(xintercept = 1977, linetype = "dashed", color = "orange", alpha = 0.5) +
    annotate("text", x = 1947, y = max(ltl_plot_data$q95_t) * 0.95, 
             label = "Peak Whaling", size = 3, color = "red") +
    annotate("text", x = 1990, y = max(ltl_plot_data$q95_t) * 0.95,
             label = "Krill Fishing", size = 3, color = "orange") +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Lower Trophic Level Prey Consumption by Baleen Whales",
      subtitle = paste0("Prydz Bay region, Monte Carlo ensemble (n = 2111)\n",
                        "Prey: ", paste(ltl_prey_found, collapse = ", ")),
      x = "Year",
      y = "LTL prey consumption (tonnes/year)",
      caption = "Shaded areas: 50% and 90% credible intervals"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 11)
    )
  
  ggsave(file.path(OUTPUT_DIR, "ltl_consumption_baleen_timeseries.png"),
         p4, width = 10, height = 6, dpi = 300)
  ggsave(file.path(OUTPUT_DIR, "ltl_consumption_baleen_timeseries.pdf"),
         p4, width = 10, height = 6)
  cat(sprintf("  Saved: ltl_consumption_baleen_timeseries plots\n"))
}

# Plot 5: Combined Krill vs LTL consumption comparison
if (!is.null(krill_baleen_stats) && !is.null(ltl_baleen_stats)) {
  # Create combined data
  krill_plot <- krill_baleen_stats %>%
    mutate(
      median_t = median * G_TO_TONNES,
      q05_t = q05 * G_TO_TONNES,
      q95_t = q95 * G_TO_TONNES,
      type = "Antarctic Krill"
    ) %>%
    select(year, median_t, q05_t, q95_t, type)
  
  ltl_plot <- ltl_baleen_stats %>%
    mutate(
      median_t = median * G_TO_TONNES,
      q05_t = q05 * G_TO_TONNES,
      q95_t = q95 * G_TO_TONNES,
      type = "All LTL Prey"
    ) %>%
    select(year, median_t, q05_t, q95_t, type)
  
  combined_plot_data <- rbind(krill_plot, ltl_plot)
  combined_plot_data$type <- factor(combined_plot_data$type, 
                                     levels = c("All LTL Prey", "Antarctic Krill"))
  
  p5 <- ggplot(combined_plot_data, aes(x = year, color = type, fill = type)) +
    geom_ribbon(aes(ymin = q05_t, ymax = q95_t), alpha = 0.15, color = NA) +
    geom_line(aes(y = median_t), linewidth = 1) +
    geom_vline(xintercept = c(1930, 1965), linetype = "dashed", color = "gray40", alpha = 0.5) +
    geom_vline(xintercept = 1977, linetype = "dashed", color = "gray40", alpha = 0.5) +
    scale_color_manual(values = c("All LTL Prey" = "darkgreen", "Antarctic Krill" = "steelblue")) +
    scale_fill_manual(values = c("All LTL Prey" = "darkgreen", "Antarctic Krill" = "steelblue")) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Baleen Whale Consumption: Krill vs All Lower Trophic Level Prey",
      subtitle = "Prydz Bay region, Monte Carlo ensemble (n = 2111)",
      x = "Year",
      y = "Consumption (tonnes/year)",
      color = "Prey Type",
      fill = "Prey Type",
      caption = "Shaded areas: 90% credible intervals. Dashed lines: Peak whaling (1930-1965), Krill fishing (1977+)"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 11),
      legend.position = "bottom"
    )
  
  ggsave(file.path(OUTPUT_DIR, "krill_vs_ltl_consumption_comparison.png"),
         p5, width = 10, height = 6, dpi = 300)
  ggsave(file.path(OUTPUT_DIR, "krill_vs_ltl_consumption_comparison.pdf"),
         p5, width = 10, height = 6)
  cat(sprintf("  Saved: krill_vs_ltl_consumption_comparison plots\n"))
}

###############################################################################
# Summary statistics (following Savoca et al. 2024 presentation style)
###############################################################################

cat("\n=============================================================\n")
cat("SUMMARY STATISTICS\n")
cat("Following Savoca et al. (2024) presentation format\n")
cat("=============================================================\n\n")

# Unit conversion factors
G_TO_TONNES <- 1e-6
DAYS_PER_YEAR <- 365

cat("Krill Consumption by Baleen Whales:\n")
cat("------------------------------------\n")
if (!is.null(krill_baleen_periods)) {
  # Add more interpretable units
  krill_baleen_periods$consumption_tonnes_yr <- krill_baleen_periods$mean_median * G_TO_TONNES
  krill_baleen_periods$consumption_tonnes_day <- krill_baleen_periods$consumption_tonnes_yr / DAYS_PER_YEAR
  
  print(krill_baleen_periods[, c("period", "consumption_tonnes_yr", "consumption_tonnes_day", "percent_of_baseline")], 
        row.names = FALSE, digits = 3)
}

cat("\nKrill Consumption by All Whales:\n")
cat("---------------------------------\n")
if (!is.null(krill_whale_periods)) {
  krill_whale_periods$consumption_tonnes_yr <- krill_whale_periods$mean_median * G_TO_TONNES
  krill_whale_periods$consumption_tonnes_day <- krill_whale_periods$consumption_tonnes_yr / DAYS_PER_YEAR
  
  print(krill_whale_periods[, c("period", "consumption_tonnes_yr", "consumption_tonnes_day", "percent_of_baseline")], 
        row.names = FALSE, digits = 3)
}

# Calculate key comparisons (Savoca-style)
cat("\n=============================================================\n")
cat("KEY FINDINGS: Historical Whaling Impact on Krill Consumption\n")
cat("=============================================================\n\n")

if (!is.null(krill_baleen_periods)) {
  pre_whaling <- krill_baleen_periods$mean_median[krill_baleen_periods$period == "pre_whaling"]
  peak_whaling <- krill_baleen_periods$mean_median[krill_baleen_periods$period == "peak_whaling"]
  modern <- krill_baleen_periods$mean_median[krill_baleen_periods$period == "modern"]
  
  if (length(pre_whaling) > 0 && !is.na(pre_whaling)) {
    cat("BALEEN WHALES:\n")
    cat("--------------\n")
    
    # Pre-whaling consumption
    pre_tonnes_yr <- pre_whaling * G_TO_TONNES
    pre_tonnes_day <- pre_tonnes_yr / DAYS_PER_YEAR
    cat(sprintf("  Pre-whaling annual consumption: %.2e tonnes/year\n", pre_tonnes_yr))
    cat(sprintf("  Pre-whaling daily consumption:  %.2e tonnes/day\n", pre_tonnes_day))
    
    # Peak whaling reduction
    if (length(peak_whaling) > 0 && !is.na(peak_whaling)) {
      peak_tonnes_yr <- peak_whaling * G_TO_TONNES
      reduction_pct <- (pre_whaling - peak_whaling) / pre_whaling * 100
      reduction_factor <- pre_whaling / peak_whaling
      cat(sprintf("\n  Peak whaling consumption: %.2e tonnes/year\n", peak_tonnes_yr))
      cat(sprintf("  Reduction from pre-whaling: %.1f%%\n", reduction_pct))
      cat(sprintf("  Pre-whaling was %.1fx higher than peak whaling\n", reduction_factor))
    }
    
    # Modern recovery
    if (length(modern) > 0 && !is.na(modern)) {
      modern_tonnes_yr <- modern * G_TO_TONNES
      recovery_pct <- modern / pre_whaling * 100
      cat(sprintf("\n  Modern annual consumption: %.2e tonnes/year\n", modern_tonnes_yr))
      cat(sprintf("  Modern recovery: %.1f%% of pre-whaling level\n", recovery_pct))
      
      # "Missing" consumption - what would be consumed if whales had fully recovered
      if (recovery_pct < 100) {
        missing_consumption <- pre_whaling - modern
        missing_tonnes_yr <- missing_consumption * G_TO_TONNES
        cat(sprintf("\n  'Missing' consumption (if fully recovered): %.2e tonnes/year\n", missing_tonnes_yr))
        cat(sprintf("  This represents %.2e tonnes/year of krill NOT consumed\n", missing_tonnes_yr))
        cat(sprintf("  due to reduced whale populations from historical whaling.\n"))
      }
    }
  }
}

# Density-based metrics (per unit area)
cat("\n\nDensity-Based Metrics (per m² of model domain):\n")
cat("------------------------------------------------\n")
cat(sprintf("  Model domain area: %.2e m²\n", MODEL_DOMAIN_AREA))

if (!is.null(krill_baleen_periods)) {
  pre_density <- krill_baleen_periods$mean_median[krill_baleen_periods$period == "pre_whaling"] / MODEL_DOMAIN_AREA
  modern_density <- krill_baleen_periods$mean_median[krill_baleen_periods$period == "modern"] / MODEL_DOMAIN_AREA
  
  if (length(pre_density) > 0 && !is.na(pre_density)) {
    cat(sprintf("  Pre-whaling krill consumption: %.4e g/m²/year\n", pre_density))
  }
  if (length(modern_density) > 0 && !is.na(modern_density)) {
    cat(sprintf("  Modern krill consumption:      %.4e g/m²/year\n", modern_density))
  }
}

cat("\n=============================================================\n")
cat("Output files saved to:", OUTPUT_DIR, "\n")
cat("=============================================================\n")
