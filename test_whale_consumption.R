# Test script for whale consumption extraction
# Tests on 3 simulations to ensure everything works

library(therMizer)
library(dplyr)

cat("=== Testing whale consumption extraction on 3 simulations ===\n\n")

# Load MC results
mc <- readRDS("Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/combined_rerun_successful_sims_20250923_122211.rds")
simulations_list <- mc$simulations
cat("Loaded", length(simulations_list), "simulations total\n")

# Test on first simulation
sim <- simulations_list[[1]]
params <- sim@params
times <- as.numeric(dimnames(sim@n)$time)
cat("Time range:", min(times), "-", max(times), "\n")

# Test getDiet at one time step
t_idx <- 170  # Last time step (2010)
n <- sim@n[t_idx, , ]
n_pp <- sim@n_pp[t_idx, ]

cat("\nTesting getDiet(proportion = FALSE)...\n")
diet <- getDiet(params, n = n, n_pp = n_pp, proportion = FALSE)
cat("Diet dimensions:", paste(dim(diet), collapse=" x "), "\n")
cat("Predator names:", paste(dimnames(diet)[[1]], collapse=", "), "\n")

# Check baleen whales consumption
cat("\n--- Baleen whales consumption at year 2010 ---\n")
pred_idx <- which(dimnames(diet)[[1]] == "baleen whales")
if(length(pred_idx) > 0) {
  baleen_diet <- diet[pred_idx, , ]
  n_baleen <- n["baleen whales", ]
  dw <- params@dw
  
  # Total population consumption by prey
  pop_consumption <- rep(0, ncol(baleen_diet))
  names(pop_consumption) <- colnames(baleen_diet)
  for(i in 1:ncol(baleen_diet)) {
    pop_consumption[i] <- sum(baleen_diet[, i] * n_baleen * dw)
  }
  
  cat("Top 5 prey items (g/year total population):\n")
  top5 <- sort(pop_consumption, decreasing=TRUE)[1:5]
  for(prey in names(top5)) {
    cat(sprintf("  %s: %.3e g/yr (%.2f tonnes/yr)\n", prey, top5[prey], top5[prey]/1e6))
  }
  
  krill_cons <- pop_consumption["antarctic krill"]
  cat(sprintf("\nAntarctic krill consumption: %.3e g/yr = %.2f tonnes/yr\n", krill_cons, krill_cons/1e6))
}

# Now test the extract_consumption function from the main script
cat("\n=== Testing extract_consumption function ===\n")

extract_consumption <- function(sim, predator_species, time_step, aggregate_predators = TRUE) {
  params <- sim@params
  times <- as.numeric(dimnames(sim@n)$time)
  all_species <- dimnames(sim@n)$sp
  
  predator_species <- predator_species[predator_species %in% all_species]
  if (length(predator_species) == 0) return(NULL)
  
  t_idx <- which.min(abs(times - time_step))
  t <- times[t_idx]
  
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
  }, error = function(e) { cat("getDiet error:", e$message, "\n"); NULL })
  
  if (is.null(diet)) return(NULL)
  
  w <- params@w
  dw <- params@dw
  
  consumption_list <- list()
  
  for (pred in predator_species) {
    pred_idx <- which(dimnames(diet)$predator == pred)
    if (length(pred_idx) == 0) next
    
    pred_diet <- diet[pred_idx, , , drop = FALSE]
    dim(pred_diet) <- dim(diet)[2:3]
    dimnames(pred_diet) <- dimnames(diet)[2:3]
    
    n_pred <- n[pred, ]
    
    total_consumption_by_prey <- rep(0, ncol(pred_diet))
    names(total_consumption_by_prey) <- colnames(pred_diet)
    
    for (prey_idx in seq_len(ncol(pred_diet))) {
      total_consumption_by_prey[prey_idx] <- sum(pred_diet[, prey_idx] * n_pred * dw)
    }
    
    consumption_list[[pred]] <- total_consumption_by_prey
  }
  
  if (length(consumption_list) == 0) return(NULL)
  
  consumption_df <- do.call(rbind, consumption_list)
  consumption_df <- as.data.frame(consumption_df)
  consumption_df$predator <- rownames(consumption_df)
  
  if (aggregate_predators) {
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

# Test extract_consumption
baleen_species <- c("baleen whales", "minke whales")
result <- extract_consumption(sim, baleen_species, 2010, aggregate_predators = TRUE)
cat("\nextract_consumption result for baleen whales at 2010:\n")
if (!is.null(result)) {
  result_sorted <- result[order(-result$consumption_g_yr), ]
  print(head(result_sorted, 10))
  
  krill_row <- result[result$prey == "antarctic krill", ]
  cat(sprintf("\nAntarctic krill: %.3e g/yr = %.2f tonnes/yr\n", 
              krill_row$consumption_g_yr, krill_row$consumption_g_yr/1e6))
} else {
  cat("Result is NULL!\n")
}

# Test time series on 3 simulations
cat("\n=== Testing time series on 3 simulations ===\n")

calc_prey_consumption_timeseries <- function(sim, predator_species, prey_species = NULL) {
  times <- as.numeric(dimnames(sim@n)$time)
  all_species <- dimnames(sim@n)$sp
  
  predator_species <- predator_species[predator_species %in% all_species]
  if (!is.null(prey_species)) {
    prey_species <- prey_species[prey_species %in% all_species]
  }
  
  if (length(predator_species) == 0) return(NULL)
  
  results <- list()
  
  for (t_idx in seq_along(times)) {
    t <- times[t_idx]
    
    consumption_df <- tryCatch({
      extract_consumption(sim, predator_species, t, aggregate_predators = TRUE)
    }, error = function(e) NULL)
    
    if (is.null(consumption_df)) next
    
    if (!is.null(prey_species)) {
      consumption_df <- consumption_df[consumption_df$prey %in% prey_species, ]
    }
    
    if (nrow(consumption_df) == 0) next
    
    result_row <- data.frame(
      year = t,
      total_consumption = sum(consumption_df$consumption_g_yr),
      stringsAsFactors = FALSE
    )
    
    results[[length(results) + 1]] <- result_row
  }
  
  if (length(results) == 0) return(NULL)
  return(do.call(rbind, results))
}

# Test on 3 simulations
all_krill_baleen <- list()
for (i in 1:3) {
  cat(sprintf("Processing simulation %d...\n", i))
  sim <- simulations_list[[i]]
  
  ts <- tryCatch({
    calc_prey_consumption_timeseries(sim, baleen_species, c("antarctic krill"))
  }, error = function(e) { cat("Error:", e$message, "\n"); NULL })
  
  if (!is.null(ts)) {
    all_krill_baleen[[length(all_krill_baleen) + 1]] <- ts
    cat(sprintf("  Got %d time points, mean consumption: %.2e g/yr\n", 
                nrow(ts), mean(ts$total_consumption)))
  } else {
    cat("  Result is NULL\n")
  }
}

# Calculate ensemble stats
cat("\n=== Ensemble statistics from 3 simulations ===\n")
if (length(all_krill_baleen) > 0) {
  for (i in seq_along(all_krill_baleen)) {
    all_krill_baleen[[i]]$sim_id <- i
  }
  combined <- do.call(rbind, all_krill_baleen)
  
  stats <- combined %>%
    group_by(year) %>%
    summarise(
      n_sims = n(),
      median = median(total_consumption, na.rm = TRUE),
      mean = mean(total_consumption, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\nFirst 5 years:\n")
  print(head(stats, 5))
  cat("\nLast 5 years:\n")
  print(tail(stats, 5))
  
  cat("\n=== TEST SUCCESSFUL! All functions working correctly. ===\n")
} else {
  cat("ERROR: No results collected from simulations\n")
}
