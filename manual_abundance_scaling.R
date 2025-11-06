# Manual Initial Abundance Scaling for Mizer Parameters
# This code allows you to manually scale initial abundances for selected species
# in a single parameter object with full control over which species and scaling factors

```{r manual-abundance-scaling, echo=TRUE}
# Function to manually scale initial abundances for selected species
scale_initial_abundances <- function(params, 
                                   species_scaling = NULL, 
                                   verbose = TRUE) {
  
  # Create a copy of the parameter object to avoid modifying the original
  params_scaled <- params
  
  # Get species information
  sp_params <- species_params(params_scaled)
  all_species <- sp_params$species
  
  if (verbose) {
    cat("=== MANUAL INITIAL ABUNDANCE SCALING ===\n")
    cat("Available species:\n")
    for (i in seq_along(all_species)) {
      cat(sprintf("  %2d. %s\n", i, all_species[i]))
    }
    cat("\n")
  }
  
  # If no scaling provided, return original params
  if (is.null(species_scaling)) {
    if (verbose) cat("No scaling factors provided. Returning original parameters.\n")
    return(params_scaled)
  }
  
  # Apply scaling factors
  scaling_applied <- data.frame(
    Species = character(0),
    Original_Total_Abundance = numeric(0),
    Scaling_Factor = numeric(0),
    New_Total_Abundance = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (species_name in names(species_scaling)) {
    # Find species index
    species_idx <- which(all_species == species_name)
    
    if (length(species_idx) == 0) {
      if (verbose) cat("Warning: Species '", species_name, "' not found. Skipping.\n", sep = "")
      next
    }
    
    if (length(species_idx) > 1) {
      if (verbose) cat("Warning: Multiple matches for '", species_name, "'. Using first match.\n", sep = "")
      species_idx <- species_idx[1]
    }
    
    # Get scaling factor
    scaling_factor <- species_scaling[[species_name]]
    
    # Calculate original total abundance
    original_abundance <- sum(params_scaled@initial_n[species_idx, ])
    
    # Apply scaling
    params_scaled@initial_n[species_idx, ] <- params_scaled@initial_n[species_idx, ] * scaling_factor
    
    # Calculate new total abundance
    new_abundance <- sum(params_scaled@initial_n[species_idx, ])
    
    # Record the change
    scaling_applied <- rbind(scaling_applied, data.frame(
      Species = species_name,
      Original_Total_Abundance = original_abundance,
      Scaling_Factor = scaling_factor,
      New_Total_Abundance = new_abundance,
      stringsAsFactors = FALSE
    ))
    
    if (verbose) {
      cat(sprintf("Scaled %s: %.2e -> %.2e (factor: %.3f)\n", 
                  species_name, original_abundance, new_abundance, scaling_factor))
    }
  }
  
  if (verbose && nrow(scaling_applied) > 0) {
    cat("\n=== SCALING SUMMARY ===\n")
    print(scaling_applied)
  }
  
  # Add scaling information as an attribute
  attr(params_scaled, "abundance_scaling") <- scaling_applied
  
  return(params_scaled)
}

# Example usage with different scenarios:

# Load your base parameters (adjust path as needed)
if (!exists("params_sel_adj")) {
  if (file.exists("params_sel_adj.rds")) {
    params_sel_adj <- readRDS("params_sel_adj.rds")
  } else {
    stop("Please load your base parameter object first (e.g., params_sel_adj)")
  }
}

# ===== EXAMPLE 1: Scale only whale species =====
cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
cat("EXAMPLE 1: Scaling whale species only\n")
cat(paste(rep("=", 60), collapse = ""), "\n", sep = "")

whale_scaling <- list(
  "minke whales" = 2.0,      # Double minke whale abundance
  "orca" = 1.5,              # Increase orca by 50%
  "sperm whales" = 3.0,      # Triple sperm whale abundance
  "baleen whales" = 0.8      # Reduce baleen whales by 20%
)

params_whales_scaled <- scale_initial_abundances(params_sel_adj, 
                                                 species_scaling = whale_scaling,
                                                 verbose = TRUE)

# ===== EXAMPLE 2: Scale krill and key predators =====
cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
cat("EXAMPLE 2: Scaling krill and key predators\n")
cat(paste(rep("=", 60), collapse = ""), "\n", sep = "")

krill_predator_scaling <- list(
  "antarctic krill" = 1.8,         # Increase krill by 80%
  "other krill" = 1.6,             # Increase other krill by 60%
  "leopard seals" = 1.3,           # Increase leopard seals by 30%
  "toothfishes" = 0.9,             # Slight decrease in toothfish
  "squids" = 1.2                   # Increase squids by 20%
)

params_krill_predator_scaled <- scale_initial_abundances(params_sel_adj, 
                                                        species_scaling = krill_predator_scaling,
                                                        verbose = TRUE)

# ===== EXAMPLE 3: Custom single species adjustment =====
cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
cat("EXAMPLE 3: Single species adjustment\n")
cat(paste(rep("=", 60), collapse = ""), "\n", sep = "")

single_species_scaling <- list(
  "mesozooplankton" = 0.5    # Halve mesozooplankton abundance
)

params_single_scaled <- scale_initial_abundances(params_sel_adj, 
                                                 species_scaling = single_species_scaling,
                                                 verbose = TRUE)

# ===== HELPER FUNCTION: Get current abundances =====
get_species_abundances <- function(params, species_names = NULL) {
  sp_params <- species_params(params)
  
  if (is.null(species_names)) {
    species_names <- sp_params$species
  }
  
  abundances <- data.frame(
    Species = character(0),
    Total_Abundance = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (sp in species_names) {
    idx <- which(sp_params$species == sp)
    if (length(idx) == 1) {
      total_abund <- sum(params@initial_n[idx, ])
      abundances <- rbind(abundances, data.frame(
        Species = sp,
        Total_Abundance = total_abund,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(abundances)
}

# ===== QUICK COMPARISON FUNCTION =====
compare_abundances <- function(params_original, params_scaled, species_names = NULL) {
  require(dplyr)
  
  original_abundances <- get_species_abundances(params_original, species_names)
  scaled_abundances <- get_species_abundances(params_scaled, species_names)
  
  comparison <- original_abundances %>%
    rename(Original_Abundance = Total_Abundance) %>%
    left_join(scaled_abundances %>% rename(Scaled_Abundance = Total_Abundance), 
              by = "Species") %>%
    mutate(
      Scaling_Factor = Scaled_Abundance / Original_Abundance,
      Percent_Change = (Scaling_Factor - 1) * 100
    )
  
  return(comparison)
}

# ===== USAGE TEMPLATES =====
cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
cat("USAGE TEMPLATES\n")
cat(paste(rep("=", 60), collapse = ""), "\n", sep = "")

cat("
# Template for your own scaling:
my_scaling <- list(
  'species_name_1' = scaling_factor_1,   # e.g., 2.0 for doubling
  'species_name_2' = scaling_factor_2,   # e.g., 0.5 for halving
  'species_name_3' = scaling_factor_3    # e.g., 1.5 for 50% increase
)

params_my_scaled <- scale_initial_abundances(
  params = params_sel_adj,           # your base parameters
  species_scaling = my_scaling,      # your scaling factors
  verbose = TRUE                     # show detailed output
)

# Compare before and after:
comparison <- compare_abundances(params_sel_adj, params_my_scaled)
print(comparison)

# Use the scaled parameters in simulations:
sim_scaled <- project(params_my_scaled, 
                     t_start = 1841, 
                     t_max = 170, 
                     effort = combined_effort_array)
")

cat("\n=== AVAILABLE SPECIES FOR SCALING ===\n")
if (exists("params_sel_adj")) {
  sp_names <- species_params(params_sel_adj)$species
  for (i in seq_along(sp_names)) {
    cat(sprintf("'%s'%s\n", sp_names[i], 
                if (i < length(sp_names)) "," else ""))
  }
}

cat("\n=== QUICK ABUNDANCE CHECK ===\n")
if (exists("params_sel_adj")) {
  current_abundances <- get_species_abundances(params_sel_adj)
  print(current_abundances)
}
```