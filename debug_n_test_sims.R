# Debug plotting with n_test_sims data
# This should be the subset of enhanced_sims for testing create_enhanced_uncertainty_plots

cat("=== DEBUGGING WITH n_test_sims DATA ===\n")

# Load required packages
suppressMessages({
  library(ggplot2)
  library(dplyr)
  library(viridis)
  library(mizer)
})

# Load helper functions
source('Helper_Functions.R')

# Load the test simulations you provided
cat("Loading n_test_sims data...\n")
n_test_sims <- readRDS('n_test_sims.RDS')
cat("✓ n_test_sims loaded\n")
cat("Type:", class(n_test_sims), "\n")
cat("Length:", length(n_test_sims), "\n")

# If it's just a number, it might be telling us how many sims to use
if (is.numeric(n_test_sims) && length(n_test_sims) == 1) {
  cat("n_test_sims appears to be a number:", n_test_sims, "\n")
  cat("This might indicate how many simulations to use from enhanced_sims\n")
  
  # Check if we have enhanced_sims or lhs_results
  if (file.exists('lhs_uncertainty_results.RData')) {
    cat("Loading full LHS results...\n")
    load('lhs_uncertainty_results.RData')
    if (exists('lhs_results')) {
      cat("Using first", n_test_sims, "simulations from lhs_results\n")
      test_sims <- lhs_results[1:n_test_sims]
      test_sims <- test_sims[!sapply(test_sims, is.null)]
      cat("Got", length(test_sims), "valid simulations\n")
    }
  } else {
    cat("lhs_uncertainty_results.RData not found - creating minimal test\n")
    # Create a few test simulations
    params <- readRDS('params_steady_state_2011_2020_tol_0.00025.RDS')
    test_sims <- list()
    for (i in 1:min(3, n_test_sims)) {
      test_sims[[i]] <- project(params, t_max = 50)
    }
    cat("Created", length(test_sims), "test simulations\n")
  }
} else if (is.list(n_test_sims)) {
  cat("n_test_sims is already a list of simulations\n")
  test_sims <- n_test_sims
  test_sims <- test_sims[!sapply(test_sims, is.null)]
  cat("Got", length(test_sims), "simulations\n")
} else {
  cat("Unexpected n_test_sims format - creating minimal test\n")
  params <- readRDS('params_steady_state_2011_2020_tol_0.00025.RDS')
  test_sims <- list(project(params, t_max = 50))
}

# Load other required data
observed_yield <- readRDS('yield_observed_timeseries_tidy.RDS')
params_optimal <- readRDS('params_steady_state_2011_2020_tol_0.00025.RDS')

# Now test the create_enhanced_uncertainty_plots logic step by step
cat("\n=== TESTING PLOTTING FUNCTIONS ===\n")

# Test yield data extraction
cat("Testing yield data extraction...\n")
tryCatch({
  yield_list <- list()
  successful_extractions <- 0
  
  for (i in 1:length(test_sims)) {
    if (!is.null(test_sims[[i]])) {
      yield_data <- trimYield_uncertainty(test_sims[[i]])
      if (!is.null(yield_data) && nrow(yield_data) > 0) {
        yield_data$sim_id <- i
        yield_list[[i]] <- yield_data
        successful_extractions <- successful_extractions + 1
      }
    }
  }
  
  cat("✓ Successfully extracted yield from", successful_extractions, "simulations\n")
  
  if (length(yield_list) > 0) {
    # Combine all yield data
    all_yield_data <- do.call(rbind, yield_list)
    cat("✓ Combined yield data:", nrow(all_yield_data), "rows\n")
    
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
    
    cat("✓ Uncertainty statistics calculated:", nrow(uncertainty_yield_data), "rows\n")
    cat("Species in data:", paste(unique(uncertainty_yield_data$Species), collapse=", "), "\n")
    
    # Test plotting step by step
    cat("\n=== STEP-BY-STEP PLOTTING TEST ===\n")
    
    # Step 1: Basic plot
    cat("Step 1: Basic uncertainty plot...\n")
    tryCatch({
      p1 <- ggplot(uncertainty_yield_data, aes(x = Year)) +
        geom_ribbon(aes(ymin = sim_q05, ymax = sim_q95), alpha = 0.3, fill = "blue") +
        geom_line(aes(y = sim_median), color = "blue") +
        theme_bw()
      print(p1)
      cat("✓ Basic uncertainty plot works\n")
    }, error = function(e) {
      cat("✗ Basic uncertainty plot failed:", e$message, "\n")
    })
    
    # Step 2: Add facets
    cat("Step 2: Adding facets...\n")
    tryCatch({
      p2 <- ggplot(uncertainty_yield_data, aes(x = Year)) +
        geom_ribbon(aes(ymin = sim_q05, ymax = sim_q95), alpha = 0.3, fill = "blue") +
        geom_line(aes(y = sim_median), color = "blue") +
        facet_wrap(~Species) +
        theme_bw()
      print(p2)
      cat("✓ Faceted plot works\n")
    }, error = function(e) {
      cat("✗ Faceted plot failed:", e$message, "\n")
      print(e)
    })
    
    # Step 3: Test custom labeller (likely source of subscript error)
    cat("Step 3: Testing custom labeller...\n")
    tryCatch({
      species_names <- unique(uncertainty_yield_data$Species)
      cat("Species for labeller:", paste(species_names, collapse=", "), "\n")
      
      # Create species labels - this was likely the issue!
      species.labs <- setNames(species_names, species_names)
      cat("Species labels structure:\n")
      print(species.labs)
      
      p3 <- ggplot(uncertainty_yield_data, aes(x = Year)) +
        geom_ribbon(aes(ymin = sim_q05, ymax = sim_q95), alpha = 0.3, fill = "blue") +
        geom_line(aes(y = sim_median), color = "blue") +
        facet_wrap(~Species, labeller = labeller(Species = species.labs)) +
        theme_bw()
      print(p3)
      cat("✓ Custom labeller works\n")
    }, error = function(e) {
      cat("✗ Custom labeller failed:", e$message, "\n")
      cat("This is likely the source of the 'subscript out of bounds' error!\n")
      print(e)
    })
    
  } else {
    cat("✗ No yield data extracted - cannot test plotting\n")
  }
  
}, error = function(e) {
  cat("✗ Yield extraction failed:", e$message, "\n")
  print(e)
})

cat("\n=== DEBUGGING COMPLETE ===\n")
