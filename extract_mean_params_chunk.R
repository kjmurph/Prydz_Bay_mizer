# Extract mean parameter values from rerun_results to create a single params object
# This chunk takes the mean parameter values for catchability, gamma, and initial abundances
# from the rerun_results object to provide a single param object for manual simulation use

```{r extract-mean-params-from-rerun, echo=TRUE, message=FALSE, warning=FALSE}
# Check if rerun_results exists
if (!exists("rerun_results")) {
  stop("rerun_results object not found. Please run the Monte Carlo analysis first.")
}

# Check if we have successful simulations
if (rerun_results$n_successful == 0) {
  stop("No successful simulations found in rerun_results.")
}

# Load baseline params if not already loaded
if (!exists("params_sel_adj")) {
  if (file.exists("params_sel_adj.rds")) {
    params_sel_adj <- readRDS("params_sel_adj.rds")
  } else {
    stop("Baseline params object 'params_sel_adj' not found.")
  }
}

# Extract parameter combinations from successful simulations
param_combinations <- rerun_results$parameters

if (length(param_combinations) == 0) {
  stop("No parameter combinations found in rerun_results.")
}

# Create data frame of all parameter values across simulations
all_params_df <- do.call(rbind, lapply(seq_along(param_combinations), function(i) {
  params <- param_combinations[[i]]
  
  # Extract species names
  species_names <- params$species_names
  n_species <- length(species_names)
  
  # Create data frame for this simulation
  data.frame(
    sim_id = i,
    species = species_names,
    catchability_change = if(!is.null(params$catchability_change)) params$catchability_change else rep(1, n_species),
    gamma_change = if(!is.null(params$gamma_change)) params$gamma_change else rep(1, n_species),
    abundance_scaling = if(!is.null(params$abundance_scaling)) params$abundance_scaling else rep(1, n_species),
    stringsAsFactors = FALSE
  )
}))

# Calculate mean parameter changes by species
mean_params_df <- all_params_df %>%
  group_by(species) %>%
  summarise(
    mean_catchability_change = mean(catchability_change, na.rm = TRUE),
    mean_gamma_change = mean(gamma_change, na.rm = TRUE),
    mean_abundance_scaling = mean(abundance_scaling, na.rm = TRUE),
    .groups = 'drop'
  )

cat("Mean parameter changes across all successful simulations:\n")
print(mean_params_df)

# Create the mean parameter object
params_mean <- params_sel_adj

# Get current species and gear parameters
sp_params <- species_params(params_mean)
gear_params_df <- gear_params(params_mean)

# Apply mean parameter changes
for (i in seq_len(nrow(mean_params_df))) {
  sp <- mean_params_df$species[i]
  
  # Update species parameters (gamma)
  sp_idx <- which(sp_params$species == sp)
  if (length(sp_idx) == 1) {
    # Apply mean gamma change
    gamma_mult <- mean_params_df$mean_gamma_change[i]
    if (is.finite(gamma_mult)) {
      sp_params$gamma[sp_idx] <- sp_params$gamma[sp_idx] * gamma_mult
    }
    
    # Apply mean abundance scaling
    abund_mult <- mean_params_df$mean_abundance_scaling[i]
    if (is.finite(abund_mult)) {
      params_mean@initial_n[sp_idx, ] <- params_mean@initial_n[sp_idx, ] * abund_mult
    }
  }
  
  # Update gear parameters (catchability)
  gear_idx <- which(gear_params_df$species == sp)
  if (length(gear_idx) == 1) {
    catch_mult <- mean_params_df$mean_catchability_change[i]
    if (is.finite(catch_mult)) {
      # Apply catchability change with bounds [0, 1]
      new_catchability <- gear_params_df$catchability[gear_idx] * catch_mult
      gear_params_df$catchability[gear_idx] <- pmin(1, pmax(0, new_catchability))
    }
  }
}

# Update the params object
species_params(params_mean) <- sp_params
gear_params(params_mean) <- gear_params_df

# Summary of changes applied
cat("\n=== SUMMARY OF MEAN PARAMETER OBJECT ===\n")
cat("Created params_mean object with mean parameter values from", rerun_results$n_successful, "successful simulations\n")

# Show gamma changes
original_gamma <- species_params(params_sel_adj)$gamma
new_gamma <- species_params(params_mean)$gamma
gamma_changes <- new_gamma / original_gamma
names(gamma_changes) <- sp_params$species

cat("\nGamma changes (new/original ratio):\n")
for (sp in names(gamma_changes)) {
  cat(sprintf("  %s: %.3f\n", sp, gamma_changes[sp]))
}

# Show catchability changes
original_catchability <- gear_params(params_sel_adj)$catchability
new_catchability <- gear_params(params_mean)$catchability
catchability_changes <- new_catchability / original_catchability
names(catchability_changes) <- gear_params_df$species

cat("\nCatchability changes (new/original ratio):\n")
for (sp in names(catchability_changes)) {
  if (is.finite(catchability_changes[sp])) {
    cat(sprintf("  %s: %.3f\n", sp, catchability_changes[sp]))
  }
}

# Show abundance scaling summary
cat("\nAbundance scaling summary:\n")
for (i in seq_len(nrow(mean_params_df))) {
  sp <- mean_params_df$species[i]
  scaling <- mean_params_df$mean_abundance_scaling[i]
  cat(sprintf("  %s: %.3f\n", sp, scaling))
}

cat("\n=== USAGE ===\n")
cat("The 'params_mean' object is now ready for manual simulation use.\n")
cat("Example usage:\n")
cat("  # Run simulation with mean parameters\n")
cat("  sim_mean <- project(params_mean, \n")
cat("                     t_start = 1841, \n")
cat("                     t_max = 170, \n")
cat("                     effort = combined_effort_array)\n")
cat("\n")
cat("  # Plot results\n")
cat("  plotBiomass(sim_mean)\n")
cat("  plotYield(sim_mean)\n")

# Optionally save the mean params object
save_mean_params <- TRUE  # Set to FALSE if you don't want to save
if (save_mean_params) {
  saveRDS(params_mean, "params_mean_from_rerun_results.rds")
  cat("\nSaved params_mean object to: params_mean_from_rerun_results.rds\n")
}
```