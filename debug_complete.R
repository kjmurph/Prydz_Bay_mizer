# Complete debugging script with all required functions
# Debug plotting with enhanced_sims_test.RDS and identify the subscript error

cat("=== COMPLETE DEBUGGING WITH ALL FUNCTIONS ===\n")

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

# Define the missing functions from 09_Summary.Rmd
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

# Load the enhanced test simulations
cat("Loading enhanced_sims_test.RDS...\n")
enhanced_sims_test <- readRDS('enhanced_sims_test.RDS')
cat("✓ Enhanced test simulations loaded:", length(enhanced_sims_test), "simulations\n")

# Remove any NULL simulations
enhanced_sims_test <- enhanced_sims_test[!sapply(enhanced_sims_test, is.null)]
cat("Valid simulations after cleaning:", length(enhanced_sims_test), "\n")

# Load other required data
observed_yield <- readRDS('yield_observed_timeseries_tidy.RDS')
params_optimal <- readRDS('params_steady_state_2011_2020_tol_0.00025.RDS')

# Test with a small subset first
cat("\n=== TESTING YIELD PLOTTING (source of subscript error) ===\n")

# Use first 5 simulations for quick testing
test_sims <- enhanced_sims_test[1:5]
cat("Testing with", length(test_sims), "simulations\n")

# Extract yield data
cat("Extracting yield data...\n")
yield_list <- list()
for (i in seq_along(test_sims)) {
  sim <- test_sims[[i]]
  if (!is.null(sim)) {
    yield_data <- trimYield_uncertainty(sim)
    if (!is.null(yield_data) && nrow(yield_data) > 0) {
      yield_data$sim_id <- i
      yield_list[[i]] <- yield_data
    }
  }
}

# Remove NULL entries
yield_list <- yield_list[!sapply(yield_list, is.null)]
cat("✓ Extracted yield from", length(yield_list), "simulations\n")

if (length(yield_list) > 0) {
  # Combine all yield data
  all_yield_data <- do.call(rbind, yield_list)
  cat("✓ Combined data:", nrow(all_yield_data), "rows\n")
  
  # Show species in the data
  species_in_data <- unique(all_yield_data$Species)
  cat("Species found:", paste(species_in_data, collapse=", "), "\n")
  
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
  
  cat("✓ Uncertainty statistics:", nrow(uncertainty_yield_data), "rows\n")
  
  # Test optimal yield extraction
  cat("Testing optimal yield extraction...\n")
  optimal_yield_data <- NULL
  tryCatch({
    # Extract time range from uncertainty data
    time_range <- range(uncertainty_yield_data$Year)
    t_max <- time_range[2] - time_range[1] + 1
    
    optimal_sim <- project(params_optimal, t_max = t_max)
    optimal_yield_data <- trimYield_uncertainty(optimal_sim)
    
    if (!is.null(optimal_yield_data)) {
      # Adjust years to match uncertainty data
      optimal_yield_data$Year <- optimal_yield_data$Year + time_range[1] - 1
      cat("✓ Optimal yield data extracted\n")
    }
  }, error = function(e) {
    cat("! Could not extract optimal data:", e$message, "\n")
  })
  
  # Now test the problematic plotting code step by step
  cat("\n=== STEP-BY-STEP PLOT DEBUGGING ===\n")
  
  # Step 1: Basic plot (should work)
  cat("Step 1: Testing basic uncertainty plot...\n")
  tryCatch({
    p1 <- ggplot(uncertainty_yield_data, aes(x = Year)) +
      geom_ribbon(aes(ymin = sim_q05, ymax = sim_q95), alpha = 0.2, fill = "lightblue") +
      geom_line(aes(y = sim_median), color = "blue") +
      theme_bw()
    print(p1)
    cat("✓ Basic plot works\n")
  }, error = function(e) {
    cat("✗ Basic plot failed:", e$message, "\n")
  })
  
  # Step 2: Add facets without custom labeller
  cat("Step 2: Testing facets without custom labeller...\n")
  tryCatch({
    p2 <- ggplot(uncertainty_yield_data, aes(x = Year)) +
      geom_ribbon(aes(ymin = sim_q05, ymax = sim_q95), alpha = 0.2, fill = "lightblue") +
      geom_line(aes(y = sim_median), color = "blue") +
      facet_wrap(~Species, scales = "free_y") +
      theme_bw()
    print(p2)
    cat("✓ Faceted plot without custom labeller works\n")
  }, error = function(e) {
    cat("✗ Faceted plot failed:", e$message, "\n")
  })
  
  # Step 3: Test the problematic custom labeller
  cat("Step 3: Testing custom labeller (likely source of error)...\n")
  
  # Create species labels
  species_for_labels <- unique(uncertainty_yield_data$Species)
  cat("Species for labeller:", paste(species_for_labels, collapse=", "), "\n")
  
  # This is the exact code that was causing issues
  species.labs <- setNames(species_for_labels, species_for_labels)
  cat("Species.labs structure:\n")
  print(species.labs)
  
  tryCatch({
    p3 <- ggplot(uncertainty_yield_data, aes(x = Year)) +
      geom_ribbon(aes(ymin = sim_q05, ymax = sim_q95), alpha = 0.2, fill = "lightblue") +
      geom_line(aes(y = sim_median), color = "blue") +
      facet_wrap(~Species, scales = "free_y", labeller = labeller(Species = species.labs)) +
      theme_bw()
    
    cat("Plot object created with custom labeller...\n")
    print(p3)
    cat("✓ Custom labeller works!\n")
    
  }, error = function(e) {
    cat("✗ FOUND THE ERROR! Custom labeller failed:\n")
    cat("Error message:", e$message, "\n")
    
    if (grepl("subscript out of bounds", e$message)) {
      cat("CONFIRMED: This is the 'subscript out of bounds' error from the conversation!\n")
      
      # Debug the labeller issue
      cat("\nDebugging labeller mismatch...\n")
      
      # Check species factor levels vs labels
      species_factor <- factor(uncertainty_yield_data$Species)
      factor_levels <- levels(species_factor)
      label_names <- names(species.labs)
      
      cat("Species as factor levels:", paste(factor_levels, collapse=", "), "\n")
      cat("Species in labels:", paste(label_names, collapse=", "), "\n")
      
      # Check for mismatches
      missing_in_labels <- setdiff(factor_levels, label_names)
      extra_in_labels <- setdiff(label_names, factor_levels)
      
      if (length(missing_in_labels) > 0) {
        cat("Missing from labels:", paste(missing_in_labels, collapse=", "), "\n")
      }
      if (length(extra_in_labels) > 0) {
        cat("Extra in labels:", paste(extra_in_labels, collapse=", "), "\n")
      }
      
      # Try alternative labeller approach
      cat("Trying fixed labeller...\n")
      tryCatch({
        # Create labels only for species actually in the data
        fixed_species_labs <- setNames(factor_levels, factor_levels)
        
        p3_fixed <- ggplot(uncertainty_yield_data, aes(x = Year)) +
          geom_ribbon(aes(ymin = sim_q05, ymax = sim_q95), alpha = 0.2, fill = "lightblue") +
          geom_line(aes(y = sim_median), color = "blue") +
          facet_wrap(~Species, scales = "free_y", labeller = labeller(Species = fixed_species_labs)) +
          theme_bw()
        
        print(p3_fixed)
        cat("✓ FIXED! Custom labeller now works with proper species matching\n")
        
      }, error = function(e2) {
        cat("✗ Fixed labeller also failed:", e2$message, "\n")
        
        # Try without any custom labeller
        cat("Final fallback: using default labels...\n")
        p3_default <- ggplot(uncertainty_yield_data, aes(x = Year)) +
          geom_ribbon(aes(ymin = sim_q05, ymax = sim_q95), alpha = 0.2, fill = "lightblue") +
          geom_line(aes(y = sim_median), color = "blue") +
          facet_wrap(~Species, scales = "free_y") +
          theme_bw()
        
        print(p3_default)
        cat("✓ Default labeller works as fallback\n")
      })
    }
  })
  
} else {
  cat("✗ No yield data available\n")
}

cat("\n=== DEBUGGING COMPLETE ===\n")
