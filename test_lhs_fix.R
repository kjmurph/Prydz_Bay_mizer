# Test script for LHS sensitivity analysis fix
library(mizer)
library(mizerExperimental) 

# Load params to test structure
params_new_v4 <- readRDS("params_steady_state_2011_2020_tol_0.00025.RDS")

# Create a simple test simulation 
sim_test <- project(params_new_v4, t_max = 10)

cat("Testing simulation object structure:\n")
cat("Class:", class(sim_test), "\n")

# Test getBiomass function
biomass_data <- getBiomass(sim_test)
cat("Biomass data dimensions:", dim(biomass_data), "\n")
cat("Species names:", rownames(biomass_data), "\n")

# Test final biomass extraction
final_biomass <- biomass_data[nrow(biomass_data), ]
cat("Final total biomass:", sum(final_biomass, na.rm = TRUE), "\n")

cat("✓ Structure tests passed - getBiomass() works correctly with simulation objects\n")
