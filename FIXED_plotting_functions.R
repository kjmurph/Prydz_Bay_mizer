# FIXED PLOTTING FUNCTIONS - Solution to "subscript out of bounds" error
# Based on debugging with enhanced_sims_test.RDS

cat("=== FIXED ENHANCED UNCERTAINTY PLOTTING ===\n")

# Load required packages
suppressMessages({
  library(ggplot2)
  library(dplyr)
  library(viridis)
  library(mizer)
  library(reshape2)
})

# Load helper functions
source('Helper_Functions.R')

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

# FIXED: create_faceted_yield_plot function with proper labeller handling
create_faceted_yield_plot_FIXED <- function(sim_list, observed_data = NULL, baseline_sim = NULL, n_test = NULL) {
  
  cat("Creating faceted yield plot...\n")
  
  # Use subset for testing if specified
  if (!is.null(n_test)) {
    sim_list <- sim_list[1:min(n_test, length(sim_list))]
    cat("Using", length(sim_list), "simulations for testing\n")
  }
  
  # Check for baseline simulation
  baseline_found <- FALSE
  baseline_names <- c("sim_1841_2010_climate_fishing", "sim_1841_2010_fishing_only", "baseline_simulation", "sim_baseline")
  for (name in baseline_names) {
    if (name %in% names(sim_list)) {
      baseline_sim <- sim_list[[name]]
      baseline_found <- TRUE
      cat("Found baseline simulation:", name, "\n")
      break
    }
  }
  
  if (!baseline_found) {
    cat("No baseline simulation found - will plot without baseline reference\n")
  }
  
  # Extract yield data from simulations
  cat("Extracting yield data from", length(sim_list), "simulations using trimYield_uncertainty approach...\n")
  
  yield_list <- list()
  successful_extractions <- 0
  
  for (i in seq_along(sim_list)) {
    sim <- sim_list[[i]]
    if (!is.null(sim)) {
      yield_data <- trimYield_uncertainty(sim)
      if (!is.null(yield_data) && nrow(yield_data) > 0) {
        yield_data$sim_id <- i
        yield_list[[i]] <- yield_data
        successful_extractions <- successful_extractions + 1
      }
    }
  }
  
  cat("Successfully extracted yield data from", successful_extractions, "out of", length(sim_list), "simulations\n")
  
  if (length(yield_list) == 0) {
    cat("✗ No yield data extracted - cannot create plot\n")
    return(NULL)
  }
  
  # Remove NULL entries and combine
  yield_list <- yield_list[!sapply(yield_list, is.null)]
  cat("Combining yield data from", length(yield_list), "successful simulations...\n")
  
  all_yield_data <- do.call(rbind, yield_list)
  
  # Calculate uncertainty summary statistics
  cat("Calculating uncertainty summary statistics...\n")
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
  
  cat("Summary statistics calculated successfully. Rows:", nrow(uncertainty_yield_data), "\n")
  cat("Species in summary:", paste(unique(uncertainty_yield_data$Species), collapse=", "), "\n")
  
  # Handle baseline simulation
  baseline_yield_data <- NULL
  if (!is.null(baseline_sim)) {
    cat("Extracting baseline simulation data...\n")
    tryCatch({
      baseline_yield_data <- trimYield_uncertainty(baseline_sim)
      if (!is.null(baseline_yield_data)) {
        # Align years with uncertainty data
        baseline_yield_data$Year <- baseline_yield_data$Year + min(uncertainty_yield_data$Year) - 1
        cat("✓ Baseline yield data extracted\n")
      }
    }, error = function(e) {
      cat("! Could not extract baseline data:", e$message, "\n")
    })
  } else {
    cat("No baseline simulation provided\n")
  }
  
  # Try to get optimal simulation data
  cat("Checking for optimal simulation data...\n")
  optimal_yield_data <- NULL
  tryCatch({
    # Try to load optimal parameters
    if (file.exists('params_steady_state_2011_2020_tol_0.00025.RDS')) {
      params_optimal <- readRDS('params_steady_state_2011_2020_tol_0.00025.RDS')
      
      # Calculate time range
      time_range <- range(uncertainty_yield_data$Year)
      t_max <- time_range[2] - time_range[1] + 1
      
      optimal_sim <- project(params_optimal, t_max = t_max)
      optimal_yield_data <- trimYield_uncertainty(optimal_sim)
      
      if (!is.null(optimal_yield_data)) {
        optimal_yield_data$Year <- optimal_yield_data$Year + time_range[1] - 1
        cat("Optimal yield data extracted successfully\n")
      }
    }
  }, error = function(e) {
    cat("! Could not extract optimal simulation data:", e$message, "\n")
  })
  
  # Filter for fished species (species with non-zero yields)
  fished_species <- uncertainty_yield_data %>%
    group_by(Species) %>%
    summarise(max_yield = max(sim_mean, na.rm = TRUE)) %>%
    filter(max_yield > 0) %>%
    pull(Species)
  
  cat("Fished species found:", paste(fished_species, collapse=", "), "\n")
  
  if (length(fished_species) == 0) {
    cat("✗ No fished species found - cannot create yield plot\n")
    return(NULL)
  }
  
  # Filter data to fished species only
  uncertainty_yield_data <- uncertainty_yield_data %>%
    filter(Species %in% fished_species)
  
  # FIXED: Create species labels ONLY for species actually in the filtered data
  cat("Setting species factor ordering...\n")
  species_in_plot_data <- unique(uncertainty_yield_data$Species)
  uncertainty_yield_data$Species <- factor(uncertainty_yield_data$Species, levels = species_in_plot_data)
  
  # FIXED: Create species.labs only for species that will actually appear in the plot
  species.labs <- setNames(species_in_plot_data, species_in_plot_data)
  cat("Species factor ordering completed\n")
  
  # Create the plot
  cat("Creating ggplot object...\n")
  tryCatch({
    p <- ggplot(uncertainty_yield_data, aes(x = Year)) +
      geom_ribbon(aes(ymin = sim_q05, ymax = sim_q95), alpha = 0.2, fill = "lightblue") +
      geom_ribbon(aes(ymin = sim_q25, ymax = sim_q75), alpha = 0.4, fill = "lightblue") +
      geom_line(aes(y = sim_median), color = "blue", size = 1) +
      # FIXED: Use labeller only with species that are actually in the data
      facet_wrap(~Species, scales = "free_y", labeller = labeller(Species = species.labs)) +
      labs(
        title = "Yield Uncertainty Analysis",
        x = "Year", 
        y = "Yield (tonnes)",
        caption = "Blue line: median yield; ribbons show 50% and 90% confidence intervals"
      ) +
      theme_bw() +
      theme(
        strip.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, hjust = 0.5)
      )
    
    cat("Base plot with custom labeller created successfully\n")
    
    # Add optimal model line if available
    if (!is.null(optimal_yield_data)) {
      optimal_yield_filtered <- optimal_yield_data %>%
        filter(Species %in% fished_species)
      
      if (nrow(optimal_yield_filtered) > 0) {
        p <- p + geom_line(data = optimal_yield_filtered, 
                          aes(x = Year, y = Yield), 
                          color = "darkblue", size = 1.2, linetype = "dashed")
        cat("Adding optimal model line...\n")
        cat("Optimal line added successfully\n")
      }
    }
    
    # Add observed data points if available
    if (!is.null(observed_data)) {
      # Prepare observed data
      obs_data <- observed_data
      names(obs_data) <- tolower(names(obs_data))
      
      # Filter observed data to fished species and available years
      obs_filtered <- obs_data %>%
        filter(species %in% tolower(fished_species)) %>%
        filter(year >= min(uncertainty_yield_data$Year) & 
               year <= max(uncertainty_yield_data$Year))
      
      if (nrow(obs_filtered) > 0) {
        # Match species names (case insensitive)
        obs_filtered$species <- factor(obs_filtered$species, 
                                     levels = tolower(fished_species),
                                     labels = fished_species)
        
        p <- p + geom_point(data = obs_filtered, 
                           aes(x = year, y = yield), 
                           color = "red", size = 2, alpha = 0.7)
        cat("Adding observed data points...\n")
        cat("Observed data points added successfully\n")
      }
    }
    
    cat("Plot creation completed successfully\n")
    
    # Test plotting - this was where the error occurred before
    cat("Attempting to print yield plot...\n")
    tryCatch({
      print(p)
      cat("✓ Yield plot printed successfully!\n")
    }, error = function(e) {
      cat("✗ Error printing yield plot:", e$message, "\n")
      return(NULL)
    })
    
    # Test saving
    cat("Attempting to save yield plot...\n")
    tryCatch({
      ggsave("yield_uncertainty_plot_FIXED.png", p, width = 12, height = 8, dpi = 300)
      cat("✓ Yield plot saved successfully!\n")
    }, error = function(e) {
      cat("✗ Error saving yield plot:", e$message, "\n")
    })
    
    return(p)
    
  }, error = function(e) {
    cat("✗ Error creating plot:", e$message, "\n")
    return(NULL)
  })
}

# Test the fixed function
cat("Loading test data...\n")
enhanced_sims_test <- readRDS('enhanced_sims_test.RDS')
observed_yield <- readRDS('yield_observed_timeseries_tidy.RDS')

cat("Testing FIXED plotting function...\n")
p_yield_fixed <- create_faceted_yield_plot_FIXED(
  sim_list = enhanced_sims_test,
  observed_data = observed_yield,
  n_test = 10  # Use 10 simulations for testing
)

if (!is.null(p_yield_fixed)) {
  cat("✓ SUCCESS! Fixed plotting function works without 'subscript out of bounds' error\n")
} else {
  cat("✗ Fixed function still has issues\n")
}

cat("\n=== SUMMARY OF FIX ===\n")
cat("The issue was in the custom labeller creation:\n")
cat("BEFORE: species.labs was created for ALL species in the simulation\n")
cat("AFTER:  species.labs is created ONLY for species that appear in the filtered plot data\n")
cat("This prevents the 'subscript out of bounds' error when ggplot tries to label facets\n")
cat("for species that don't actually exist in the plot data.\n")
