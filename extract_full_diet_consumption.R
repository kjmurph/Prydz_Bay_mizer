###############################################################################
# Full Diet Consumption Extraction Script
# 
# Extracts complete consumption matrices for ALL predator-prey combinations
# from Monte Carlo ensemble simulations. This comprehensive extraction enables
# flexible analysis of any predator group consuming any prey group.
#
# Outputs:
#   - Full consumption arrays: predator x prey x year for each simulation
#   - Aggregated time series for key predator/prey combinations
#   - Both fishing and climate-only (unfished) ensembles
#
# This is a one-time extraction that caches results for future analysis.
#
# Author: Generated for Prydz Bay mizer project
# Date: 2025
###############################################################################

library(therMizer)
library(dplyr)
library(tidyr)
library(reshape2)

###############################################################################
# Configuration
###############################################################################

# Model domain area in m^2
MODEL_DOMAIN_AREA <- 1.474341e+12  # m^2 (therMizer calibration domain, 05_therMizer_calibration_scale_model_domain.Rmd)
G_TO_TONNES <- 1e-6

# Input paths
MC_RESULTS_PATHS <- list(
  fishing = "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds",
  climate_only = "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
)

# Output directory
OUTPUT_DIR <- "whale_consumption_outputs"

# Species groupings for aggregation
SPECIES_GROUPS <- list(
  baleen_whales = c("baleen whales", "minke whales"),
  toothed_whales = c("sperm whales", "orca"),
  all_whales = c("baleen whales", "minke whales", "sperm whales", "orca"),
  seals = c("leopard seals", "small divers", "medium divers", "large divers"),
  birds = c("flying birds"),
  fish = c("mesopelagic fishes", "bathypelagic fishes", "shelf and coastal fishes", "toothfishes"),
  krill = c("antarctic krill"),
  ltl_prey = c("antarctic krill", "other krill", "mesozooplankton", "other macrozooplankton", "salps"),
  zooplankton = c("mesozooplankton", "other krill", "other macrozooplankton", "salps")
)

###############################################################################
# Core extraction function
###############################################################################

#' Extract full consumption matrix at a single time step
#' 
#' Returns consumption rate (g/year) for all predator-prey combinations
#' integrated over predator size structure.
#'
#' @param sim MizerSim object
#' @param t_idx Time index
#' @return Matrix: predator x prey with total population consumption (g/year)
extract_consumption_matrix <- function(sim, t_idx) {
  params <- sim@params
  dw <- params@dw
  
  # Get state at this time
  n <- sim@n[t_idx, , , drop = FALSE]
  dim(n) <- dim(sim@n)[2:3]
  dimnames(n) <- dimnames(sim@n)[2:3]
  
  n_pp <- sim@n_pp[t_idx, ]
  
  n_other <- sim@n_other[t_idx, ]
  if (!is.null(dimnames(sim@n_other))) {
    names(n_other) <- dimnames(sim@n_other)$component
  }
  
  # Get diet array: predator x predator_size x prey
  diet <- tryCatch({
    getDiet(params, n = n, n_pp = n_pp, n_other = n_other, proportion = FALSE)
  }, error = function(e) NULL)
  
  if (is.null(diet)) return(NULL)
  
  predators <- dimnames(diet)$predator
  prey_names <- dimnames(diet)[[3]]
  
  # Initialize consumption matrix
  consumption_matrix <- matrix(0, nrow = length(predators), ncol = length(prey_names),
                                dimnames = list(predator = predators, prey = prey_names))
  
  # Integrate consumption over predator sizes
  for (pred_idx in seq_along(predators)) {
    pred <- predators[pred_idx]
    n_pred <- n[pred, ]
    
    for (prey_idx in seq_along(prey_names)) {
      # consumption_rate[size] * abundance[size] * dw -> total g/year
      consumption_matrix[pred_idx, prey_idx] <- sum(diet[pred_idx, , prey_idx] * n_pred * dw)
    }
  }
  
  return(consumption_matrix)
}

#' Extract full consumption time series for one simulation
#' 
#' @param sim MizerSim object
#' @return List with consumption_array (predator x prey x time) and years
extract_full_consumption_timeseries <- function(sim) {
  times <- as.numeric(dimnames(sim@n)$time)
  n_times <- length(times)
  
  # Get dimensions from first time step
  first_matrix <- extract_consumption_matrix(sim, 1)
  if (is.null(first_matrix)) return(NULL)
  
  predators <- rownames(first_matrix)
  prey <- colnames(first_matrix)
  
  # Initialize 3D array: predator x prey x time
  consumption_array <- array(0, 
                              dim = c(length(predators), length(prey), n_times),
                              dimnames = list(predator = predators, prey = prey, year = times))
  
  # Fill array
  for (t_idx in seq_len(n_times)) {
    mat <- extract_consumption_matrix(sim, t_idx)
    if (!is.null(mat)) {
      consumption_array[, , t_idx] <- mat
    }
  }
  
  return(list(
    consumption = consumption_array,
    years = times,
    predators = predators,
    prey = prey
  ))
}

#' Aggregate consumption by predator and prey groups
#' 
#' @param consumption_data Output from extract_full_consumption_timeseries
#' @param predator_group Vector of predator species names
#' @param prey_group Vector of prey species names
#' @return Data frame with year and total_consumption
aggregate_consumption <- function(consumption_data, predator_group, prey_group) {
  if (is.null(consumption_data)) return(NULL)
  
  arr <- consumption_data$consumption
  years <- consumption_data$years
  
  # Filter to valid species
  valid_preds <- predator_group[predator_group %in% consumption_data$predators]
  valid_prey <- prey_group[prey_group %in% consumption_data$prey]
  
  if (length(valid_preds) == 0 || length(valid_prey) == 0) return(NULL)
  
  # Sum over predators and prey
  total_consumption <- numeric(length(years))
  for (t in seq_along(years)) {
    total_consumption[t] <- sum(arr[valid_preds, valid_prey, t])
  }
  
  return(data.frame(
    year = years,
    total_consumption = total_consumption
  ))
}

###############################################################################
# Process ensemble function
###############################################################################

process_ensemble <- function(ensemble_file, ensemble_name, output_prefix) {
  cat(sprintf("\n=============================================================\n"))
  cat(sprintf("Processing %s ensemble\n", ensemble_name))
  cat(sprintf("=============================================================\n\n"))
  
  if (!file.exists(ensemble_file)) {
    cat(sprintf("ERROR: File not found: %s\n", ensemble_file))
    return(NULL)
  }
  
  # Load ensemble
  cat("Loading ensemble file...\n")
  mc <- readRDS(ensemble_file)
  
  # Handle different structures
  if ("simulations" %in% names(mc)) {
    simulations_list <- mc$simulations
  } else if (is.list(mc) && inherits(mc[[1]], "MizerSim")) {
    simulations_list <- mc
  } else {
    stop("Unrecognized ensemble structure")
  }
  
  n_sims <- length(simulations_list)
  cat(sprintf("  Loaded %d simulations\n", n_sims))
  
  # Get structure from first simulation
  first_sim <- simulations_list[[1]]
  all_species <- dimnames(first_sim@n)$sp
  times <- as.numeric(dimnames(first_sim@n)$time)
  cat(sprintf("  Time range: %d - %d (%d years)\n", min(times), max(times), length(times)))
  cat(sprintf("  Species: %s\n", paste(all_species, collapse = ", ")))
  
  # Storage for aggregated results
  results <- list(
    # Full consumption by simulation (compressed - key combinations only)
    baleen_krill = list(),
    baleen_ltl = list(),
    baleen_all_prey = list(),
    all_whales_krill = list(),
    all_whales_ltl = list(),
    all_whales_all_prey = list(),
    seals_krill = list(),
    seals_ltl = list(),
    fish_krill = list(),
    fish_ltl = list()
  )
  
  # Process simulations
  cat(sprintf("\nExtracting consumption for %d simulations...\n", n_sims))
  pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
  skipped <- 0
  
  for (i in seq_len(n_sims)) {
    setTxtProgressBar(pb, i)
    
    sim <- tryCatch(simulations_list[[i]], error = function(e) NULL)
    if (is.null(sim)) {
      skipped <- skipped + 1
      next
    }
    
    # Extract full consumption time series
    consumption_data <- tryCatch({
      extract_full_consumption_timeseries(sim)
    }, error = function(e) NULL)
    
    if (is.null(consumption_data)) {
      skipped <- skipped + 1
      next
    }
    
    # Aggregate by key predator-prey combinations
    # Baleen whales
    ts <- aggregate_consumption(consumption_data, SPECIES_GROUPS$baleen_whales, SPECIES_GROUPS$krill)
    if (!is.null(ts)) results$baleen_krill[[length(results$baleen_krill) + 1]] <- ts
    
    ts <- aggregate_consumption(consumption_data, SPECIES_GROUPS$baleen_whales, SPECIES_GROUPS$ltl_prey)
    if (!is.null(ts)) results$baleen_ltl[[length(results$baleen_ltl) + 1]] <- ts
    
    # Baleen whales eating all prey (for total consumption)
    ts <- aggregate_consumption(consumption_data, SPECIES_GROUPS$baleen_whales, consumption_data$prey)
    if (!is.null(ts)) results$baleen_all_prey[[length(results$baleen_all_prey) + 1]] <- ts
    
    # All whales
    ts <- aggregate_consumption(consumption_data, SPECIES_GROUPS$all_whales, SPECIES_GROUPS$krill)
    if (!is.null(ts)) results$all_whales_krill[[length(results$all_whales_krill) + 1]] <- ts
    
    ts <- aggregate_consumption(consumption_data, SPECIES_GROUPS$all_whales, SPECIES_GROUPS$ltl_prey)
    if (!is.null(ts)) results$all_whales_ltl[[length(results$all_whales_ltl) + 1]] <- ts
    
    # All whales eating all prey (for total consumption)
    ts <- aggregate_consumption(consumption_data, SPECIES_GROUPS$all_whales, consumption_data$prey)
    if (!is.null(ts)) results$all_whales_all_prey[[length(results$all_whales_all_prey) + 1]] <- ts
    
    # Seals
    ts <- aggregate_consumption(consumption_data, SPECIES_GROUPS$seals, SPECIES_GROUPS$krill)
    if (!is.null(ts)) results$seals_krill[[length(results$seals_krill) + 1]] <- ts
    
    ts <- aggregate_consumption(consumption_data, SPECIES_GROUPS$seals, SPECIES_GROUPS$ltl_prey)
    if (!is.null(ts)) results$seals_ltl[[length(results$seals_ltl) + 1]] <- ts
    
    # Fish (as predators of krill)
    ts <- aggregate_consumption(consumption_data, SPECIES_GROUPS$fish, SPECIES_GROUPS$krill)
    if (!is.null(ts)) results$fish_krill[[length(results$fish_krill) + 1]] <- ts
    
    ts <- aggregate_consumption(consumption_data, SPECIES_GROUPS$fish, SPECIES_GROUPS$ltl_prey)
    if (!is.null(ts)) results$fish_ltl[[length(results$fish_ltl) + 1]] <- ts
  }
  close(pb)
  
  cat(sprintf("\n  Successfully processed %d simulations (skipped %d)\n", 
              n_sims - skipped, skipped))
  
  # Save all results
  cat("\nSaving results...\n")
  
  for (name in names(results)) {
    if (length(results[[name]]) > 0) {
      filename <- file.path(OUTPUT_DIR, sprintf("%s_%s_all_sims.rds", output_prefix, name))
      saveRDS(results[[name]], filename)
      cat(sprintf("  Saved: %s (%d sims)\n", basename(filename), length(results[[name]])))
    }
  }
  
  return(results)
}

###############################################################################
# Calculate ensemble statistics
###############################################################################

calc_ensemble_stats <- function(df_list, value_col = "total_consumption") {
  if (length(df_list) == 0) return(NULL)
  
  for (i in seq_along(df_list)) {
    df_list[[i]]$sim_id <- i
  }
  combined <- do.call(rbind, df_list)
  
  stats <- combined %>%
    group_by(year) %>%
    summarise(
      n_sims = n(),
      median = median(!!sym(value_col), na.rm = TRUE),
      mean = mean(!!sym(value_col), na.rm = TRUE),
      sd = sd(!!sym(value_col), na.rm = TRUE),
      q05 = quantile(!!sym(value_col), 0.05, na.rm = TRUE),
      q25 = quantile(!!sym(value_col), 0.25, na.rm = TRUE),
      q75 = quantile(!!sym(value_col), 0.75, na.rm = TRUE),
      q95 = quantile(!!sym(value_col), 0.95, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(year)
  
  return(stats)
}

###############################################################################
# Main execution
###############################################################################

cat("=============================================================\n")
cat("Full Diet Consumption Extraction\n")
cat("=============================================================\n")

# Create output directory
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# Process fishing ensemble
fishing_results <- process_ensemble(
  MC_RESULTS_PATHS$fishing,
  "Fishing",
  "fishing"
)

# Process climate-only ensemble
climate_results <- process_ensemble(
  MC_RESULTS_PATHS$climate_only,
  "Climate-Only (Unfished)",
  "climate_only"
)

###############################################################################
# Calculate and save ensemble statistics
###############################################################################

cat("\n=============================================================\n")
cat("Calculating ensemble statistics...\n")
cat("=============================================================\n")

# Function to save stats
save_stats <- function(results_list, name, prefix) {
  if (is.null(results_list) || length(results_list) == 0) return(NULL)
  
  stats <- calc_ensemble_stats(results_list)
  if (!is.null(stats)) {
    # Add density columns
    stats$median_density <- stats$median / MODEL_DOMAIN_AREA
    stats$mean_density <- stats$mean / MODEL_DOMAIN_AREA
    # Add tonnes columns
    stats$median_tonnes <- stats$median * G_TO_TONNES
    stats$mean_tonnes <- stats$mean * G_TO_TONNES
    
    filename <- file.path(OUTPUT_DIR, sprintf("%s_%s_stats.csv", prefix, name))
    write.csv(stats, filename, row.names = FALSE)
    cat(sprintf("  Saved: %s\n", basename(filename)))
  }
  return(stats)
}

# Save fishing stats
if (!is.null(fishing_results)) {
  cat("\nFishing ensemble statistics:\n")
  for (name in names(fishing_results)) {
    save_stats(fishing_results[[name]], name, "fishing")
  }
}

# Save climate-only stats
if (!is.null(climate_results)) {
  cat("\nClimate-only ensemble statistics:\n")
  for (name in names(climate_results)) {
    save_stats(climate_results[[name]], name, "climate_only")
  }
}

###############################################################################
# Summary output
###############################################################################

cat("\n=============================================================\n")
cat("EXTRACTION COMPLETE\n")
cat("=============================================================\n\n")

cat("Predator-Prey Combinations Extracted:\n")
cat("--------------------------------------\n")
cat("  baleen_krill:        Baleen whales eating Antarctic krill\n")
cat("  baleen_ltl:          Baleen whales eating all LTL prey\n")
cat("  baleen_all_prey:     Baleen whales eating all prey (total consumption)\n")
cat("  all_whales_krill:    All whales eating Antarctic krill\n")
cat("  all_whales_ltl:      All whales eating all LTL prey\n")
cat("  all_whales_all_prey: All whales eating all prey (total consumption)\n")
cat("  seals_krill:         Seals eating Antarctic krill\n")
cat("  seals_ltl:           Seals eating all LTL prey\n")
cat("  fish_krill:          Fish eating Antarctic krill\n")
cat("  fish_ltl:            Fish eating all LTL prey\n")

cat("\nSpecies Groups:\n")
cat("---------------\n")
for (name in names(SPECIES_GROUPS)) {
  cat(sprintf("  %s: %s\n", name, paste(SPECIES_GROUPS[[name]], collapse = ", ")))
}

cat("\nOutput files saved to:", OUTPUT_DIR, "\n")

# Print summary statistics for key comparisons
if (!is.null(fishing_results) && !is.null(climate_results)) {
  cat("\n=============================================================\n")
  cat("QUICK COMPARISON: Fishing vs Climate-Only\n")
  cat("=============================================================\n\n")
  
  # Baleen whales - LTL consumption
  fishing_stats <- calc_ensemble_stats(fishing_results$baleen_ltl)
  climate_stats <- calc_ensemble_stats(climate_results$baleen_ltl)
  
  if (!is.null(fishing_stats) && !is.null(climate_stats)) {
    cat("Baleen Whale LTL Consumption (tonnes/year):\n")
    cat("-------------------------------------------\n")
    
    # Pre-whaling period (1841-1880)
    pre_fish <- mean(fishing_stats$median[fishing_stats$year >= 1841 & fishing_stats$year <= 1880]) * G_TO_TONNES
    pre_clim <- mean(climate_stats$median[climate_stats$year >= 1841 & climate_stats$year <= 1880]) * G_TO_TONNES
    cat(sprintf("  Pre-whaling (1841-1880):  Fishing: %.0f, Climate-only: %.0f, Ratio: %.1f%%\n", 
                pre_fish, pre_clim, pre_fish/pre_clim*100))
    
    # Modern period (2001-2010)
    mod_fish <- mean(fishing_stats$median[fishing_stats$year >= 2001 & fishing_stats$year <= 2010]) * G_TO_TONNES
    mod_clim <- mean(climate_stats$median[climate_stats$year >= 2001 & climate_stats$year <= 2010]) * G_TO_TONNES
    cat(sprintf("  Modern (2001-2010):       Fishing: %.0f, Climate-only: %.0f, Ratio: %.1f%%\n", 
                mod_fish, mod_clim, mod_fish/mod_clim*100))
    
    cat(sprintf("\n  Change from pre-whaling to modern:\n"))
    cat(sprintf("    Fishing scenario:     %.1f%% of pre-whaling\n", mod_fish/pre_fish*100))
    cat(sprintf("    Climate-only:         %.1f%% of pre-whaling\n", mod_clim/pre_clim*100))
    cat(sprintf("    Fishing impact:       %.0f tonnes/yr 'missing' consumption\n", mod_clim - mod_fish))
  }
}

cat("\n=============================================================\n")
cat("Done!\n")
cat("=============================================================\n")
