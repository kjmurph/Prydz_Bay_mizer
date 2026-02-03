# Test script for full diet extraction
# Tests on 5 simulations to ensure everything works

library(therMizer)
library(dplyr)

cat("=== Testing Full Diet Extraction on 5 simulations ===\n\n")

# Configuration
SPECIES_GROUPS <- list(
  baleen_whales = c("baleen whales", "minke whales"),
  all_whales = c("baleen whales", "minke whales", "sperm whales", "orca"),
  krill = c("antarctic krill"),
  ltl_prey = c("antarctic krill", "other krill", "mesozooplankton", "other macrozooplankton", "salps")
)

G_TO_TONNES <- 1e-6

# Core extraction function
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

# Full time series extraction
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

# Aggregation function
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

# Load fishing ensemble
cat("Loading fishing ensemble...\n")
mc <- readRDS("Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds")

if ("simulations" %in% names(mc)) {
  simulations_list <- mc$simulations
} else {
  simulations_list <- mc
}

cat(sprintf("Loaded %d simulations total\n", length(simulations_list)))

# Test on first simulation - single time step
cat("\n=== Test 1: Single time step extraction ===\n")
sim <- simulations_list[[1]]
times <- as.numeric(dimnames(sim@n)$time)
cat(sprintf("Time range: %d - %d\n", min(times), max(times)))

mat <- extract_consumption_matrix(sim, 170)  # Last time step (2010)
cat(sprintf("Matrix dimensions: %d x %d\n", nrow(mat), ncol(mat)))
cat(sprintf("Predators: %s\n", paste(rownames(mat), collapse = ", ")))
cat(sprintf("Prey: %s\n", paste(colnames(mat), collapse = ", ")))

# Check baleen whale consumption
baleen_idx <- which(rownames(mat) %in% SPECIES_GROUPS$baleen_whales)
cat(sprintf("\nBaleen whale consumption at 2010 (tonnes/yr):\n"))
for (pred in rownames(mat)[baleen_idx]) {
  krill_cons <- mat[pred, "antarctic krill"] * G_TO_TONNES
  total_cons <- sum(mat[pred, ]) * G_TO_TONNES
  cat(sprintf("  %s: Krill=%.1f, Total=%.1f\n", pred, krill_cons, total_cons))
}

# Test full time series on one simulation
cat("\n=== Test 2: Full time series extraction (1 sim) ===\n")
consumption_data <- extract_full_consumption_timeseries(sim)
cat(sprintf("Array dimensions: %s\n", paste(dim(consumption_data$consumption), collapse=" x ")))

# Aggregate
baleen_krill <- aggregate_consumption(consumption_data, SPECIES_GROUPS$baleen_whales, SPECIES_GROUPS$krill)
baleen_ltl <- aggregate_consumption(consumption_data, SPECIES_GROUPS$baleen_whales, SPECIES_GROUPS$ltl_prey)

cat(sprintf("\nBaleen whale krill consumption (tonnes/yr):\n"))
cat(sprintf("  First year (1841): %.1f\n", baleen_krill$total_consumption[1] * G_TO_TONNES))
cat(sprintf("  Last year (2010):  %.1f\n", baleen_krill$total_consumption[170] * G_TO_TONNES))

cat(sprintf("\nBaleen whale LTL consumption (tonnes/yr):\n"))
cat(sprintf("  First year (1841): %.1f\n", baleen_ltl$total_consumption[1] * G_TO_TONNES))
cat(sprintf("  Last year (2010):  %.1f\n", baleen_ltl$total_consumption[170] * G_TO_TONNES))

# Test on 5 simulations
cat("\n=== Test 3: Processing 5 simulations ===\n")

results_krill <- list()
results_ltl <- list()

for (i in 1:5) {
  cat(sprintf("Processing simulation %d...\n", i))
  sim <- simulations_list[[i]]
  
  consumption_data <- tryCatch({
    extract_full_consumption_timeseries(sim)
  }, error = function(e) { cat("  Error:", e$message, "\n"); NULL })
  
  if (!is.null(consumption_data)) {
    ts_krill <- aggregate_consumption(consumption_data, SPECIES_GROUPS$baleen_whales, SPECIES_GROUPS$krill)
    ts_ltl <- aggregate_consumption(consumption_data, SPECIES_GROUPS$baleen_whales, SPECIES_GROUPS$ltl_prey)
    
    if (!is.null(ts_krill)) {
      results_krill[[length(results_krill) + 1]] <- ts_krill
      cat(sprintf("  Krill - Mean: %.1f tonnes/yr\n", mean(ts_krill$total_consumption) * G_TO_TONNES))
    }
    if (!is.null(ts_ltl)) {
      results_ltl[[length(results_ltl) + 1]] <- ts_ltl
      cat(sprintf("  LTL   - Mean: %.1f tonnes/yr\n", mean(ts_ltl$total_consumption) * G_TO_TONNES))
    }
  }
}

# Calculate ensemble stats
cat("\n=== Test 4: Ensemble statistics from 5 sims ===\n")

calc_ensemble_stats <- function(df_list, value_col = "total_consumption") {
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
      q05 = quantile(!!sym(value_col), 0.05, na.rm = TRUE),
      q95 = quantile(!!sym(value_col), 0.95, na.rm = TRUE),
      .groups = "drop"
    )
  return(stats)
}

krill_stats <- calc_ensemble_stats(results_krill)
ltl_stats <- calc_ensemble_stats(results_ltl)

cat("\nKrill consumption statistics (tonnes/yr):\n")
cat(sprintf("  Pre-whaling (1841-1880) median: %.1f\n", 
            mean(krill_stats$median[krill_stats$year <= 1880]) * G_TO_TONNES))
cat(sprintf("  Modern (2001-2010) median:      %.1f\n", 
            mean(krill_stats$median[krill_stats$year >= 2001]) * G_TO_TONNES))

cat("\nLTL consumption statistics (tonnes/yr):\n")
cat(sprintf("  Pre-whaling (1841-1880) median: %.1f\n", 
            mean(ltl_stats$median[ltl_stats$year <= 1880]) * G_TO_TONNES))
cat(sprintf("  Modern (2001-2010) median:      %.1f\n", 
            mean(ltl_stats$median[ltl_stats$year >= 2001]) * G_TO_TONNES))

# Compare krill vs LTL
cat("\n=== Krill as % of LTL consumption ===\n")
krill_pre <- mean(krill_stats$median[krill_stats$year <= 1880])
ltl_pre <- mean(ltl_stats$median[ltl_stats$year <= 1880])
krill_mod <- mean(krill_stats$median[krill_stats$year >= 2001])
ltl_mod <- mean(ltl_stats$median[ltl_stats$year >= 2001])

cat(sprintf("  Pre-whaling: Krill is %.1f%% of LTL consumption\n", krill_pre/ltl_pre*100))
cat(sprintf("  Modern:      Krill is %.1f%% of LTL consumption\n", krill_mod/ltl_mod*100))

cat("\n=== TEST SUCCESSFUL! Full extraction ready to run. ===\n")
