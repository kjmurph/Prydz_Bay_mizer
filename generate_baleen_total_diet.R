###############################################################################
# Generate missing baleen_all_prey statistics
# 
# This script generates statistics for baleen whale total consumption
# from the existing RDS files by loading baleen_ltl and calculating
# baleen_all_prey from the raw ensemble data.
###############################################################################

library(therMizer)
library(dplyr)

# Configuration
MODEL_DOMAIN_AREA <- 1.95e+13
G_TO_TONNES <- 1e-6
OUTPUT_DIR <- "whale_consumption_outputs"

SPECIES_GROUPS <- list(
  baleen_whales = c("baleen whales", "minke whales")
)

# Helper functions
aggregate_consumption <- function(consumption_data, predator_group, prey_group) {
  if (is.null(consumption_data)) return(NULL)
  
  arr <- consumption_data$consumption
  years <- consumption_data$years
  
  valid_preds <- predator_group[predator_group %in% consumption_data$predators]
  valid_prey <- prey_group[prey_group %in% consumption_data$prey]
  
  if (length(valid_preds) == 0 || length(valid_prey) == 0) return(NULL)
  
  total_consumption <- numeric(length(years))
  for (t in seq_along(years)) {
    total_consumption[t] <- sum(arr[valid_preds, valid_prey, t])
  }
  
  return(data.frame(
    year = years,
    total_consumption = total_consumption
  ))
}

extract_consumption_matrix <- function(sim, t_idx) {
  params <- sim@params
  dw <- params@dw
  
  n <- sim@n[t_idx, , , drop = FALSE]
  dim(n) <- dim(sim@n)[2:3]
  dimnames(n) <- dimnames(sim@n)[2:3]
  
  n_pp <- sim@n_pp[t_idx, ]
  n_other <- sim@n_other[t_idx, ]
  if (!is.null(dimnames(sim@n_other))) {
    names(n_other) <- dimnames(sim@n_other)$component
  }
  
  diet <- tryCatch({
    getDiet(params, n = n, n_pp = n_pp, n_other = n_other, proportion = FALSE)
  }, error = function(e) NULL)
  
  if (is.null(diet)) return(NULL)
  
  predators <- dimnames(diet)$predator
  prey_names <- dimnames(diet)[[3]]
  
  consumption_matrix <- matrix(0, nrow = length(predators), ncol = length(prey_names),
                                dimnames = list(predator = predators, prey = prey_names))
  
  for (pred_idx in seq_along(predators)) {
    pred <- predators[pred_idx]
    n_pred <- n[pred, ]
    
    for (prey_idx in seq_along(prey_names)) {
      consumption_matrix[pred_idx, prey_idx] <- sum(diet[pred_idx, , prey_idx] * n_pred * dw)
    }
  }
  
  return(consumption_matrix)
}

extract_full_consumption_timeseries <- function(sim) {
  times <- as.numeric(dimnames(sim@n)$time)
  n_times <- length(times)
  
  first_matrix <- extract_consumption_matrix(sim, 1)
  if (is.null(first_matrix)) return(NULL)
  
  predators <- rownames(first_matrix)
  prey <- colnames(first_matrix)
  
  consumption_array <- array(0, 
                              dim = c(length(predators), length(prey), n_times),
                              dimnames = list(predator = predators, prey = prey, year = times))
  
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
      .groups = 'drop'
    )
  
  return(stats)
}

# Process each ensemble
for (scenario in c("fishing", "climate_only")) {
  cat(sprintf("\n=============================================================\n"))
  cat(sprintf("Processing %s ensemble for baleen_all_prey\n", scenario))
  cat(sprintf("=============================================================\n\n"))
  
  # Load ensemble
  if (scenario == "fishing") {
    ensemble_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
  } else {
    ensemble_file <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
  }
  
  if (!file.exists(ensemble_file)) {
    cat(sprintf("ERROR: File not found: %s\n", ensemble_file))
    next
  }
  
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
  cat(sprintf("  Loaded %d simulations\n", n_sims))
  
  # Extract baleen_all_prey
  results <- list()
  cat(sprintf("\nExtracting baleen total consumption for %d simulations...\n", n_sims))
  pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
  skipped <- 0
  
  for (i in seq_len(n_sims)) {
    setTxtProgressBar(pb, i)
    
    sim <- tryCatch(simulations_list[[i]], error = function(e) NULL)
    if (is.null(sim)) {
      skipped <- skipped + 1
      next
    }
    
    consumption_data <- tryCatch({
      extract_full_consumption_timeseries(sim)
    }, error = function(e) NULL)
    
    if (is.null(consumption_data)) {
      skipped <- skipped + 1
      next
    }
    
    # Baleen whales eating all prey
    ts <- aggregate_consumption(consumption_data, SPECIES_GROUPS$baleen_whales, consumption_data$prey)
    if (!is.null(ts)) {
      results[[length(results) + 1]] <- ts
    }
  }
  close(pb)
  
  cat(sprintf("\n  Successfully processed %d simulations (skipped %d)\n", 
              n_sims - skipped, skipped))
  
  # Save RDS
  rds_file <- file.path(OUTPUT_DIR, sprintf("%s_baleen_all_prey_all_sims.rds", scenario))
  saveRDS(results, rds_file)
  cat(sprintf("\nSaved: %s\n", basename(rds_file)))
  
  # Calculate and save stats
  stats <- calc_ensemble_stats(results)
  if (!is.null(stats)) {
    stats$median_density <- stats$median / MODEL_DOMAIN_AREA
    stats$mean_density <- stats$mean / MODEL_DOMAIN_AREA
    stats$median_tonnes <- stats$median * G_TO_TONNES
    stats$mean_tonnes <- stats$mean * G_TO_TONNES
    
    csv_file <- file.path(OUTPUT_DIR, sprintf("%s_baleen_all_prey_stats.csv", scenario))
    write.csv(stats, csv_file, row.names = FALSE)
    cat(sprintf("Saved: %s\n", basename(csv_file)))
    
    # Print summary
    cat(sprintf("\nSummary for %s baleen total consumption (tonnes/yr):\n", scenario))
    pre_whaling <- stats %>% filter(year >= 1841 & year <= 1880) %>%
      summarise(median = mean(median_tonnes), mean = mean(mean_tonnes))
    modern <- stats %>% filter(year >= 2001 & year <= 2010) %>%
      summarise(median = mean(median_tonnes), mean = mean(mean_tonnes))
    
    cat(sprintf("  Pre-whaling (1841-1880): Median = %.0f, Mean = %.0f\n", 
                pre_whaling$median, pre_whaling$mean))
    cat(sprintf("  Modern (2001-2010):      Median = %.0f, Mean = %.0f\n", 
                modern$median, modern$mean))
    cat(sprintf("  Modern as %% of pre-whaling: Median = %.1f%%, Mean = %.1f%%\n",
                modern$median/pre_whaling$median*100, modern$mean/pre_whaling$mean*100))
  }
}

cat("\n=============================================================\n")
cat("BALEEN_ALL_PREY EXTRACTION COMPLETE\n")
cat("=============================================================\n")
