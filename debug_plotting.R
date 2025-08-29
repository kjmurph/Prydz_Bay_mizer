# Debug plotting issue from LHS uncertainty analysis
# Based on conversation history - "subscript out of bounds" error

cat("=== DEBUGGING LHS PLOTTING ISSUE ===\n")

# Load required packages
suppressMessages({
  library(ggplot2)
  library(dplyr)
  library(viridis)
  library(mizer)
})

# Load helper functions
source('Helper_Functions.R')

# Check what test data we have
cat("Checking available test data...\n")
if (file.exists('n_test_sims.RDS')) {
  test_data <- readRDS('n_test_sims.RDS')
  cat("n_test_sims.RDS loaded - type:", class(test_data), "length/value:", length(test_data), "\n")
} else {
  cat("n_test_sims.RDS not found\n")
}

# Load a working simulation to test plotting functions
cat("Loading a known good simulation for testing...\n")
params <- readRDS('params_steady_state_2011_2020_tol_0.00025.RDS')
observed_yield <- readRDS('yield_observed_timeseries_tidy.RDS')

# Create a simple test simulation
cat("Creating test simulation...\n")
sim <- project(params, t_max = 50)

# Test the trimYield_uncertainty function
cat("Testing trimYield_uncertainty function...\n")
tryCatch({
  yield_data <- trimYield_uncertainty(sim)
  if (!is.null(yield_data)) {
    cat("✓ trimYield_uncertainty works - got", nrow(yield_data), "rows\n")
    cat("Species in yield data:", paste(unique(yield_data$Species), collapse=", "), "\n")
  } else {
    cat("✗ trimYield_uncertainty returned NULL\n")
  }
}, error = function(e) {
  cat("✗ trimYield_uncertainty failed:", e$message, "\n")
})

# Test basic plotting with minimal data
cat("Testing basic plotting...\n")
if (exists("yield_data") && !is.null(yield_data)) {
  tryCatch({
    # Test 1: Very basic plot
    cat("Test 1: Basic ggplot...\n")
    p1 <- ggplot(yield_data, aes(x = Year, y = Yield)) + 
      geom_line() + 
      theme_bw()
    print(p1)
    cat("✓ Basic plot works\n")
    
    # Test 2: Faceted plot
    cat("Test 2: Faceted plot...\n")
    p2 <- ggplot(yield_data, aes(x = Year, y = Yield)) + 
      geom_line() + 
      facet_wrap(~Species) + 
      theme_bw()
    print(p2)
    cat("✓ Faceted plot works\n")
    
    # Test 3: Custom labeller (this was likely the issue)
    cat("Test 3: Custom labeller...\n")
    species_names <- unique(yield_data$Species)
    species.labs <- setNames(species_names, species_names)
    cat("Species labels:", paste(names(species.labs), "=", species.labs, collapse=", "), "\n")
    
    p3 <- ggplot(yield_data, aes(x = Year, y = Yield)) + 
      geom_line() + 
      facet_wrap(~Species, labeller = labeller(Species = species.labs)) + 
      theme_bw()
    print(p3)
    cat("✓ Custom labeller works\n")
    
  }, error = function(e) {
    cat("✗ Plotting test failed:", e$message, "\n")
    cat("Error details:\n")
    print(e)
  })
} else {
  cat("✗ No yield data available for plotting tests\n")
}

cat("=== DEBUGGING COMPLETE ===\n")
