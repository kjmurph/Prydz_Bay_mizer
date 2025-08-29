# Create plots matching the user's exact examples
# This recreates the style from the attached yield and biomass plots

cat("=== RECREATING PLOTS TO MATCH USER EXAMPLES ===\n")

# Load required packages
suppressMessages({
  library(ggplot2)
  library(dplyr)
  library(viridis)
  library(mizer)
  library(therMizer)  # For upgradeTherParams function
  library(reshape2)
  library(RColorBrewer)
  library(tidyr)
})

# Load helper functions
source('Helper_Functions.R')

# Global variable to store baseline simulation (to avoid running multiple times)
baseline_simulation_cache <- NULL

# Function to get or create baseline simulation
get_baseline_simulation <- function() {
  if (!is.null(baseline_simulation_cache)) {
    cat("Using cached baseline simulation\n")
    return(baseline_simulation_cache)
  }
  
  cat("Creating baseline simulation with fishing effort...\n")
  tryCatch({
    # Load required parameters and forcings
    if (file.exists('params_steady_state_2011_2020_tol_0.00025.RDS') &&
        file.exists('temperature_forcing_1841_2010.rds') &&
        file.exists('phytoplankton_forcing_1841_2010.rds') &&
        file.exists('effort_array_1841_2010.rds')) {
      
      # Load parameters, forcings, and fishing effort
      params_new_v4 <- readRDS('params_steady_state_2011_2020_tol_0.00025.RDS')
      extended_ocean_temp <- readRDS('temperature_forcing_1841_2010.rds')
      extended_n_pp_array <- readRDS('phytoplankton_forcing_1841_2010.rds')
      effort_array <- readRDS('effort_array_1841_2010.rds')
      
      # Setup params with forcings
      params_baseline_with_forcings <- upgradeTherParams(params_new_v4, 
                                                        ocean_temp_array = extended_ocean_temp,
                                                        n_pp_array = extended_n_pp_array,
                                                        aerobic_effect = FALSE, 
                                                        metabolism_effect = TRUE)
      
      # Run baseline simulation from 1841 to 2010 (full range) with fishing effort
      t_start <- 1841
      t_end <- 2010
      t_max <- t_end - t_start + 1
      
      cat("Running baseline simulation with fishing effort from", t_start, "to", t_end, "(", t_max, "years )\n")
      
      baseline_simulation_cache <<- project(params_baseline_with_forcings, 
                                           t_max = t_max, 
                                           t_start = t_start,
                                           effort = effort_array)
      cat("✓ Baseline simulation with fishing effort complete and cached\n")
      return(baseline_simulation_cache)
      
    } else {
      missing_files <- c()
      if (!file.exists('params_steady_state_2011_2020_tol_0.00025.RDS')) missing_files <- c(missing_files, "params file")
      if (!file.exists('temperature_forcing_1841_2010.rds')) missing_files <- c(missing_files, "temperature forcing")
      if (!file.exists('phytoplankton_forcing_1841_2010.rds')) missing_files <- c(missing_files, "phytoplankton forcing")
      if (!file.exists('effort_array_1841_2010.rds')) missing_files <- c(missing_files, "fishing effort")
      
      cat("! Missing required files for baseline simulation:", paste(missing_files, collapse=", "), "\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("! Could not create baseline simulation:", e$message, "\n")
    return(NULL)
  })
}

# Define the required functions
trimYield_uncertainty <- function(sim) {
  tryCatch({
    y <- plotYieldGear(sim, return_data = TRUE)
    y <- select(y, c(Year, Species, Yield))
    return(y)
  }, error = function(e) {
    cat("Warning: Could not extract yield from simulation\n")
    return(NULL)
  })
}

getBiomassData <- function(sim) {
  tryCatch({
    biomass <- getBiomass(sim)
    df_biomass <- reshape2::melt(biomass)
    names(df_biomass) <- c("Year", "Species", "Biomass")
    return(df_biomass)
  }, error = function(e) {
    cat("Warning: Could not extract biomass from simulation\n")
    return(NULL)
  })
}

# Function to create yield plot matching the user's example
create_yield_plot_like_example <- function(sim_list, observed_data = NULL, n_test = NULL) {
  
  cat("Creating yield plot to match user example...\n")
  
  # Use subset for testing if specified
  if (!is.null(n_test)) {
    sim_list <- sim_list[1:min(n_test, length(sim_list))]
    cat("Using", length(sim_list), "simulations\n")
  }
  
  # Extract yield data
  cat("Extracting yield data...\n")
  yield_list <- list()
  for (i in seq_along(sim_list)) {
    sim <- sim_list[[i]]
    if (!is.null(sim)) {
      yield_data <- trimYield_uncertainty(sim)
      if (!is.null(yield_data) && nrow(yield_data) > 0) {
        yield_data$sim_id <- i
        yield_list[[i]] <- yield_data
      }
    }
  }
  
  yield_list <- yield_list[!sapply(yield_list, is.null)]
  cat("Extracted yield from", length(yield_list), "simulations\n")
  
  if (length(yield_list) == 0) {
    cat("No yield data available\n")
    return(NULL)
  }
  
  # Combine all yield data
  all_yield_data <- do.call(rbind, yield_list)
  
  # Calculate uncertainty statistics
  uncertainty_yield_data <- all_yield_data %>%
    group_by(Species, Year) %>%
    summarise(
      sim_mean = mean(Yield, na.rm = TRUE),
      sim_median = median(Yield, na.rm = TRUE),
      sim_q05 = quantile(Yield, 0.05, na.rm = TRUE),
      sim_q95 = quantile(Yield, 0.95, na.rm = TRUE),
      sim_q25 = quantile(Yield, 0.25, na.rm = TRUE),
      sim_q75 = quantile(Yield, 0.75, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Filter for fished species (non-zero yields)
  fished_species <- uncertainty_yield_data %>%
    group_by(Species) %>%
    summarise(max_yield = max(sim_median, na.rm = TRUE)) %>%
    filter(max_yield > 0) %>%
    pull(Species)
  
  cat("Fished species:", paste(fished_species, collapse=", "), "\n")
  
  uncertainty_yield_data <- uncertainty_yield_data %>%
    filter(Species %in% fished_species)
  
  # Create species-specific colors matching the original examples
  # Use consistent color palette for all species to ensure consistency between plots
  species_color_mapping <- c(
    # Fished species (appear in both yield and biomass plots)
    "antarctic krill" = "#E31A1C",        # Red
    "bathypelagic fishes" = "#FF7F00",     # Orange  
    "shelf and coastal fishes" = "#1F78B4", # Blue
    "squids" = "#33A02C",                  # Green
    "toothfishes" = "#6A3D9A",             # Purple
    "minke whales" = "#FB9A99",            # Light red
    "orca" = "#A6CEE3",                    # Light blue
    "sperm whales" = "#B2DF8A",            # Light green
    "baleen whales" = "#FDBF6F",           # Light orange
    # Additional species for biomass plot only
    "mesozooplankton" = "#CAB2D6",         # Light purple
    "other krill" = "#FFFF99",             # Light yellow
    "other macrozooplankton" = "#B15928",  # Brown
    "salps" = "#999999",                   # Grey
    "mesopelagic fishes" = "#BC80BD",      # Light magenta
    "flying birds" = "#CCEBC5",            # Very light green
    "small divers" = "#FFED6F",            # Gold
    "leopard seals" = "#FBB4AE",           # Light salmon
    "medium divers" = "#B3CDE3",           # Light steel blue
    "large divers" = "#CCEBC5"             # Very light green
  )
  
  # Filter colors to only fished species for yield plot
  fished_species_colors <- species_color_mapping[names(species_color_mapping) %in% fished_species]
  
  # Create complete color mapping for all species (for biomass plots)
  all_species_colors <- species_color_mapping
  
  cat("Using consistent colors for fished species...\n")
  
  # Create the plot matching the user's example
  p <- ggplot(uncertainty_yield_data, aes(x = Year, group = Species, fill = Species, color = Species)) +
    # Add uncertainty ribbons (like in the example)
    geom_ribbon(aes(ymin = sim_q05, ymax = sim_q95), alpha = 0.3, color = NA) +
    geom_ribbon(aes(ymin = sim_q25, ymax = sim_q75), alpha = 0.5, color = NA) +
    # Add median line (like in the example)
    geom_line(aes(y = sim_median), linewidth = 1) +
    # Use log scale for y-axis (like in the example)
    scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    # Add vertical reference lines (like in the example)
    geom_vline(xintercept = c(1950, 2000), linetype = "dashed", color = "black", alpha = 0.7) +
    # Use species-specific colors matching original examples
    scale_fill_manual(values = fished_species_colors) +
    scale_color_manual(values = fished_species_colors) +
    # Facet by species (like in the example)
    facet_wrap(~Species, scales = "free_y", ncol = 3) +
    # Styling to match the example
    labs(
      title = "",
      x = "Year",
      y = "Yield [g/year]"
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 10, face = "bold"),
      strip.background = element_rect(fill = "lightgray", color = "black"),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "none",  # No legend like in the example
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 14, hjust = 0.5)
    )
  
  # Add optimal/baseline model line if available
  cat("Setting up optimal/baseline model for yield...\n")
  baseline_sim <- get_baseline_simulation()
  
  if (!is.null(baseline_sim)) {
    # Extract yield from baseline simulation
    optimal_yield_data <- trimYield_uncertainty(baseline_sim)
    
    if (!is.null(optimal_yield_data) && nrow(optimal_yield_data) > 0) {
      # Filter to only fished species and time range of uncertainty data
      time_range <- range(uncertainty_yield_data$Year)
      optimal_yield_filtered <- optimal_yield_data %>%
        filter(Species %in% fished_species,
               Year >= time_range[1],
               Year <= time_range[2])
      
      if (nrow(optimal_yield_filtered) > 0) {
        p <- p + geom_line(data = optimal_yield_filtered, 
                          aes(x = Year, y = Yield, group = Species), 
                          linetype = "dashed", linewidth = 1.2, color = "grey50",
                          inherit.aes = FALSE)
        cat("✓ Added optimal model dashed grey lines to yield plot\n")
      }
    }
  }
  
  # Add observed data points if available (like the dots in the example)
  if (!is.null(observed_data)) {
    cat("Adding observed data points...\n")
    
    # Debug: Check what's in the observed data
    cat("Observed data columns:", paste(names(observed_data), collapse=", "), "\n")
    
    # Work with the actual column names (Year, Species, Yield)
    if ("Species" %in% names(observed_data)) {
      cat("Unique species in observed data:", paste(unique(observed_data$Species), collapse=", "), "\n")
      
      # Filter observed data to fished species and time range
      obs_filtered <- observed_data %>%
        filter(Year >= min(uncertainty_yield_data$Year) & 
               Year <= max(uncertainty_yield_data$Year)) %>%
        filter(Species %in% fished_species)  # Direct match since both use proper case
      
      cat("Filtered observed data rows:", nrow(obs_filtered), "\n")
      cat("Species with observations:", paste(unique(obs_filtered$Species), collapse=", "), "\n")
      
      if (nrow(obs_filtered) > 0) {
        # Add as points - now each species' observations will only appear in its own facet
        p <- p + geom_point(data = obs_filtered, 
                           aes(x = Year, y = Yield), 
                           color = "black", size = 1.5, alpha = 0.8,
                           inherit.aes = FALSE)
        cat("Added", nrow(obs_filtered), "observed data points\n")
      } else {
        cat("No matching observations found for fished species\n")
      }
    } else {
      cat("No 'Species' column found in observed data\n")
    }
  }
  
  cat("Yield plot created to match example style\n")
  return(p)
}

# Function to create biomass plot matching the user's example
create_biomass_plot_like_example <- function(sim_list, obs_biomass_data = NULL, n_test = NULL) {
  
  cat("Creating biomass plot to match user example...\n")
  
  # Create consistent species color mapping (same as yield function)
  species_color_mapping <- c(
    # Fished species (appear in both yield and biomass plots)
    "antarctic krill" = "#E31A1C",        # Red
    "bathypelagic fishes" = "#FF7F00",     # Orange  
    "shelf and coastal fishes" = "#1F78B4", # Blue
    "squids" = "#33A02C",                  # Green
    "toothfishes" = "#6A3D9A",             # Purple
    "minke whales" = "#FB9A99",            # Light red
    "orca" = "#A6CEE3",                    # Light blue
    "sperm whales" = "#B2DF8A",            # Light green
    "baleen whales" = "#FDBF6F",           # Light orange
    # Additional species for biomass plot only
    "mesozooplankton" = "#CAB2D6",         # Light purple
    "other krill" = "#FFFF99",             # Light yellow
    "other macrozooplankton" = "#B15928",  # Brown
    "salps" = "#999999",                   # Grey
    "mesopelagic fishes" = "#BC80BD",      # Light magenta
    "flying birds" = "#CCEBC5",            # Very light green
    "small divers" = "#FFED6F",            # Gold
    "leopard seals" = "#FBB4AE",           # Light salmon
    "medium divers" = "#B3CDE3",           # Light steel blue
    "large divers" = "#CCEBC5"             # Very light green
  )
  
  # Create complete color mapping for all species
  all_species_colors <- species_color_mapping
  
  # Use subset for testing if specified
  if (!is.null(n_test)) {
    sim_list <- sim_list[1:min(n_test, length(sim_list))]
    cat("Using", length(sim_list), "simulations\n")
  }
  
  # Extract biomass data
  cat("Extracting biomass data...\n")
  biomass_list <- list()
  for (i in seq_along(sim_list)) {
    sim <- sim_list[[i]]
    if (!is.null(sim)) {
      biomass_data <- getBiomassData(sim)
      if (!is.null(biomass_data) && nrow(biomass_data) > 0) {
        biomass_data$sim_id <- i
        biomass_list[[i]] <- biomass_data
      }
    }
  }
  
  biomass_list <- biomass_list[!sapply(biomass_list, is.null)]
  cat("Extracted biomass from", length(biomass_list), "simulations\n")
  
  if (length(biomass_list) == 0) {
    cat("No biomass data available\n")
    return(NULL)
  }
  
  # Combine all biomass data
  all_biomass_data <- do.call(rbind, biomass_list)
  
  # Calculate uncertainty statistics
  uncertainty_biomass_data <- all_biomass_data %>%
    group_by(Species, Year) %>%
    summarise(
      sim_mean = mean(Biomass, na.rm = TRUE),
      sim_median = median(Biomass, na.rm = TRUE),
      sim_min = min(Biomass, na.rm = TRUE),
      sim_max = max(Biomass, na.rm = TRUE),
      sim_q25 = quantile(Biomass, 0.25, na.rm = TRUE),
      sim_q75 = quantile(Biomass, 0.75, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Define species order to match the original example
  # Based on the original example layout (5 columns, 4 rows)
  species_order <- c("mesozooplankton", "other krill", "other macrozooplankton", "antarctic krill", "salps",
                     "mesopelagic fishes", "bathypelagic fishes", "shelf and coastal fishes", "flying birds", "small divers",
                     "squids", "toothfishes", "leopard seals", "medium divers", "large divers",
                     "minke whales", "orca", "sperm whales", "baleen whales")
  
  # Filter to species present in data and set factor order
  available_species <- intersect(species_order, unique(uncertainty_biomass_data$Species))
  uncertainty_biomass_data$Species <- factor(uncertainty_biomass_data$Species, levels = available_species)
  
  cat("Species in order:", paste(available_species, collapse=", "), "\n")
  
  # Create species-specific colors (matching the example style)
  n_species <- length(available_species)
  species_colors <- RColorBrewer::brewer.pal(min(n_species, 11), "Spectral")
  if (n_species > 11) {
    species_colors <- rainbow(n_species)
  }
  names(species_colors) <- available_species
  
  cat("Creating biomass plot with species-specific colors...\n")
  
  # Create the plot matching the user's biomass example
  p <- ggplot(uncertainty_biomass_data, aes(x = Year, group = Species, fill = Species, color = Species)) +
    # Add simulation range as light shading (like in the example)
    geom_ribbon(aes(ymin = sim_min, ymax = sim_max), alpha = 0.2, color = NA) +
    # Add IQR as darker shading (like in the example)
    geom_ribbon(aes(ymin = sim_q25, ymax = sim_q75), alpha = 0.4, color = NA) +
    # Add mean line (like the solid lines in the example)
    geom_line(aes(y = sim_mean), linewidth = 1.2) +
    # Use species-specific colors
    scale_fill_manual(values = all_species_colors) +
    scale_color_manual(values = all_species_colors) +
    # Facet by species (like in the example) - 5 columns to match original
    facet_wrap(~Species, scales = "free_y", ncol = 5) +
    # Styling to match the example
    labs(
      title = "Modeled vs. Observed Biomass with Simulation Range",
      subtitle = "Simulation range (light shade), IQR (darker shade), mean (solid), baseline with fishing (dashed), observed values (points)",
      x = "Year",
      y = "Biomass"
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 9),
      strip.background = element_rect(fill = "lightgray", color = "black"),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
      axis.text.y = element_text(size = 8),
      legend.position = "none",  # No legend like in the example
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 12, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5)
    )
  
  # Add baseline model if available (dashed lines like in the example)
  cat("Setting up baseline model...\n")
  baseline_sim <- get_baseline_simulation()
  
  if (!is.null(baseline_sim)) {
    # Extract biomass from shared baseline simulation
    baseline_biomass_data <- getBiomassData(baseline_sim)
    
    if (!is.null(baseline_biomass_data) && nrow(baseline_biomass_data) > 0) {
      # Filter to time range of uncertainty data if needed
      time_range <- range(uncertainty_biomass_data$Year)
      baseline_biomass_filtered <- baseline_biomass_data %>%
        filter(Year >= time_range[1],
               Year <= time_range[2])
      
      if (nrow(baseline_biomass_filtered) > 0) {
        baseline_biomass_filtered$Species <- factor(baseline_biomass_filtered$Species, levels = available_species)
        
        # Add baseline as dashed grey lines
        p <- p + geom_line(data = baseline_biomass_filtered, 
                          aes(x = Year, y = Biomass, group = Species), 
                          linetype = "dashed", linewidth = 1, color = "grey50",
                          inherit.aes = FALSE)
        cat("✓ Added baseline model dashed grey lines\n")
      }
    }
  }
  
  # Add observed biomass with ±20% uncertainty for 2010-2020 period only
  if (!is.null(obs_biomass_data)) {
    cat("Adding observed biomass with ±20% uncertainty for 2010-2020...\n")
    
    # Define observation period
    obs_years <- 2010:2020
    
    # Create observed data for the 2010-2020 period only
    obs_data_period <- data.frame()
    for (year in obs_years) {
      year_data <- obs_biomass_data
      year_data$Year <- year
      # Calculate ±20% bounds
      year_data$ObsBiomass_lower <- year_data$ObsBiomass * 0.8
      year_data$ObsBiomass_upper <- year_data$ObsBiomass * 1.2
      obs_data_period <- rbind(obs_data_period, year_data)
    }
    
    # Filter to species in the plot and set factor order
    obs_data_filtered <- obs_data_period %>%
      filter(Species %in% available_species)
    obs_data_filtered$Species <- factor(obs_data_filtered$Species, levels = available_species)
    
    cat("Adding observed biomass ribbons for", length(obs_years), "years and", length(unique(obs_data_filtered$Species)), "species\n")
    
    # Add ±20% uncertainty ribbon (grey shading for 2010-2020)
    p <- p + geom_ribbon(data = obs_data_filtered,
                        aes(x = Year, ymin = ObsBiomass_lower, ymax = ObsBiomass_upper),
                        fill = "grey50", alpha = 0.3, color = NA, inherit.aes = FALSE)
    
    # Add observed biomass as horizontal line for 2010-2020 period
    p <- p + geom_line(data = obs_data_filtered,
                      aes(x = Year, y = ObsBiomass),
                      color = "black", linewidth = 1.5, inherit.aes = FALSE)
    
    # Add observed points at the ends for clarity
    p <- p + geom_point(data = obs_data_filtered %>% filter(Year %in% c(2010, 2020)),
                       aes(x = Year, y = ObsBiomass),
                       color = "black", size = 2, inherit.aes = FALSE)
  }
  
  cat("Biomass plot created to match example style\n")
  return(p)
}

# Test the new functions
cat("Loading test data...\n")
enhanced_sims_test <- readRDS('enhanced_sims_test.RDS')
observed_yield <- readRDS('yield_observed_timeseries_tidy.RDS')

# Create observed biomass data (from Helper_Functions.R style)
obs_biomass_data <- data.frame(
  Species = c("mesozooplankton","other krill","other macrozooplankton",
              "antarctic krill","salps","mesopelagic fishes",
              "bathypelagic fishes","shelf and coastal fishes","flying birds",
              "small divers","squids","toothfishes","leopard seals",
              "medium divers","large divers","minke whales","orca",
              "sperm whales","baleen whales"),
  ObsBiomass = c(1.297420e+13, 2.801248e+12, 1.474341e+13, 5.897364e+12, 
                 9.612703e+11, 1.769209e+12, 1.769209e+12, 4.027900e+12, 
                 4.423023e+09, 2.358946e+10, 2.211512e+11, 1.105756e+12, 
                 2.948682e+09, 3.907004e+11, 1.621775e+10, 2.064077e+10, 
                 8.846046e+09, 1.621775e+10, 1.872413e+11)
)

cat("Creating yield plot matching your example...\n")
p_yield_example <- create_yield_plot_like_example(
  sim_list = enhanced_sims_test,
  observed_data = observed_yield,
  n_test = 20  # Use 20 simulations for better uncertainty
)

if (!is.null(p_yield_example)) {
  print(p_yield_example)
  ggsave("yield_plot_matching_example.png", p_yield_example, width = 12, height = 8, dpi = 300)
  cat("✓ Yield plot saved as yield_plot_matching_example.png\n")
}

cat("Creating biomass plot matching your example...\n")
p_biomass_example <- create_biomass_plot_like_example(
  sim_list = enhanced_sims_test,
  obs_biomass_data = obs_biomass_data,
  n_test = 20  # Use 20 simulations for better uncertainty
)

if (!is.null(p_biomass_example)) {
  print(p_biomass_example)
  ggsave("biomass_plot_matching_example.png", p_biomass_example, width = 15, height = 10, dpi = 300)
  cat("✓ Biomass plot saved as biomass_plot_matching_example.png\n")
}

cat("\n=== PLOTS CREATED TO MATCH YOUR EXAMPLES ===\n")
cat("Key changes made:\n")
cat("- Species-specific colors instead of all blue\n")
cat("- Log scale for yield plot\n")
cat("- Uncertainty ribbons with proper transparency\n")
cat("- Observed data as black points\n")
cat("- Dashed vertical reference lines\n")
cat("- Proper faceting and styling\n")
cat("- Baseline model with fishing effort as dashed grey lines\n")
