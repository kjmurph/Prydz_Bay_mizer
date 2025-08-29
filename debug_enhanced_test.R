# Debug plotting with enhanced_sims_test.RDS
# This contains the subset of enhanced simulations for debugging the plotting issues

cat("=== DEBUGGING WITH enhanced_sims_test.RDS ===\n")

# Load required packages
suppressMessages({
  library(ggplot2)
  library(dplyr)
  library(viridis)
  library(mizer)
})

# Load helper functions
source('Helper_Functions.R')

# Load the enhanced test simulations
cat("Loading enhanced_sims_test.RDS...\n")
enhanced_sims_test <- readRDS('enhanced_sims_test.RDS')
cat("✓ Enhanced test simulations loaded\n")
cat("Type:", class(enhanced_sims_test), "\n")
cat("Length:", length(enhanced_sims_test), "\n")

# Check structure
if (length(enhanced_sims_test) > 0) {
  cat("First element class:", class(enhanced_sims_test[[1]]), "\n")
  if ('MizerSim' %in% class(enhanced_sims_test[[1]])) {
    cat("✓ Contains MizerSim objects\n")
    species_names <- dimnames(enhanced_sims_test[[1]]@n)$sp
    cat("Species in simulations:", paste(species_names, collapse=", "), "\n")
  }
}

# Remove any NULL simulations
enhanced_sims_test <- enhanced_sims_test[!sapply(enhanced_sims_test, is.null)]
cat("Valid simulations after cleaning:", length(enhanced_sims_test), "\n")

# Load other required data
observed_yield <- readRDS('yield_observed_timeseries_tidy.RDS')
params_optimal <- readRDS('params_steady_state_2011_2020_tol_0.00025.RDS')

# Now recreate the exact plotting issue from the conversation history
cat("\n=== RECREATING THE PLOTTING ISSUE ===\n")

# Test yield data extraction (this was working)
cat("Testing yield data extraction...\n")
tryCatch({
  yield_list <- list()
  successful_extractions <- 0
  
  # Use smaller subset for initial testing
  test_subset <- enhanced_sims_test[1:min(10, length(enhanced_sims_test))]
  
  for (i in 1:length(test_subset)) {
    sim <- test_subset[[i]]
    if (!is.null(sim)) {
      yield_data <- trimYield_uncertainty(sim)
      if (!is.null(yield_data) && nrow(yield_data) > 0) {
        yield_data$sim_id <- i
        yield_list[[i]] <- yield_data
        successful_extractions <- successful_extractions + 1
      }
    }
  }
  
  cat("✓ Successfully extracted yield from", successful_extractions, "out of", length(test_subset), "simulations\n")
  
  if (length(yield_list) > 0) {
    # Combine all yield data
    all_yield_data <- do.call(rbind, yield_list)
    cat("✓ Combined yield data:", nrow(all_yield_data), "rows\n")
    
    # Calculate uncertainty statistics (this was working)
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
    
    cat("✓ Uncertainty statistics calculated:", nrow(uncertainty_yield_data), "rows\n")
    cat("Species in uncertainty data:", paste(unique(uncertainty_yield_data$Species), collapse=", "), "\n")
    
    # Get optimal yield data
    optimal_yield_data <- NULL
    tryCatch({
      optimal_sim <- project(params_optimal, t_max = max(uncertainty_yield_data$Year) - min(uncertainty_yield_data$Year) + 1)
      optimal_yield_data <- trimYield_uncertainty(optimal_sim)
      if (!is.null(optimal_yield_data)) {
        optimal_yield_data$Year <- optimal_yield_data$Year + min(uncertainty_yield_data$Year) - 1
        cat("✓ Optimal yield data extracted\n")
      }
    }, error = function(e) {
      cat("! Could not extract optimal yield data:", e$message, "\n")
    })
    
    # Now test the exact plotting code that was failing
    cat("\n=== TESTING THE EXACT FAILING PLOT CODE ===\n")
    
    # This recreates the exact code from create_faceted_yield_plot
    cat("Creating ggplot object with custom labeller...\n")
    tryCatch({
      # Step 1: Create species labels (this might be the issue)
      species_in_data <- unique(uncertainty_yield_data$Species)
      cat("Species for labeller:", paste(species_in_data, collapse=", "), "\n")
      
      # The original code used this approach:
      species.labs <- setNames(species_in_data, species_in_data)
      cat("Species.labs structure:\n")
      print(species.labs)
      cat("Names:", paste(names(species.labs), collapse=", "), "\n")
      cat("Values:", paste(species.labs, collapse=", "), "\n")
      
      # Check if species in data match labeller
      species_factor_levels <- levels(factor(uncertainty_yield_data$Species))
      cat("Species factor levels:", paste(species_factor_levels, collapse=", "), "\n")
      
      # This is the exact plot code that was failing:
      p_yield <- ggplot(uncertainty_yield_data, aes(x = Year)) +
        geom_ribbon(aes(ymin = sim_q05, ymax = sim_q95), alpha = 0.2, fill = "lightblue") +
        geom_ribbon(aes(ymin = sim_q25, ymax = sim_q75), alpha = 0.4, fill = "lightblue") +
        geom_line(aes(y = sim_median), color = "blue", size = 1) +
        facet_wrap(~Species, scales = "free_y", labeller = labeller(Species = species.labs)) +
        labs(
          title = "Yield Uncertainty Analysis",
          x = "Year",
          y = "Yield (tonnes)",
          caption = "Ribbons show 50% and 90% confidence intervals"
        ) +
        theme_bw() +
        theme(
          strip.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      cat("✓ Plot object created successfully\n")
      
      # This is where the error occurred - trying to print the plot
      cat("Attempting to print the plot...\n")
      print(p_yield)
      cat("✓ Plot printed successfully!\n")
      
    }, error = function(e) {
      cat("✗ PLOT FAILED - This is the error we need to fix!\n")
      cat("Error message:", e$message, "\n")
      cat("Error class:", class(e), "\n")
      if (grepl("subscript out of bounds", e$message)) {
        cat("CONFIRMED: This is the 'subscript out of bounds' error!\n")
        
        # Try to identify the exact issue
        cat("Debugging the labeller issue...\n")
        
        # Check if all species in data have corresponding labels
        species_in_uncertainty <- unique(uncertainty_yield_data$Species)
        species_in_labels <- names(species.labs)
        
        cat("Species in uncertainty data:", paste(species_in_uncertainty, collapse=", "), "\n")
        cat("Species in labels:", paste(species_in_labels, collapse=", "), "\n")
        
        missing_in_labels <- setdiff(species_in_uncertainty, species_in_labels)
        missing_in_data <- setdiff(species_in_labels, species_in_uncertainty)
        
        if (length(missing_in_labels) > 0) {
          cat("Species in data but missing from labels:", paste(missing_in_labels, collapse=", "), "\n")
        }
        if (length(missing_in_data) > 0) {
          cat("Species in labels but missing from data:", paste(missing_in_data, collapse=", "), "\n")
        }
        
        # Try without custom labeller
        cat("Trying plot without custom labeller...\n")
        tryCatch({
          p_simple <- ggplot(uncertainty_yield_data, aes(x = Year)) +
            geom_ribbon(aes(ymin = sim_q05, ymax = sim_q95), alpha = 0.2, fill = "lightblue") +
            geom_line(aes(y = sim_median), color = "blue") +
            facet_wrap(~Species, scales = "free_y") +
            theme_bw()
          print(p_simple)
          cat("✓ Plot without custom labeller works!\n")
          cat("SOLUTION: The issue is with the custom labeller\n")
        }, error = function(e2) {
          cat("✗ Even simple plot fails:", e2$message, "\n")
        })
      }
    })
    
  } else {
    cat("✗ No yield data available for plotting\n")
  }
  
}, error = function(e) {
  cat("✗ Data extraction failed:", e$message, "\n")
})

cat("\n=== DEBUGGING COMPLETE ===\n")
