# Parallel Optimization Implementation Guide for Prydz Bay Mizer
# Enhanced Calibrar Integration with Comprehensive Analysis
# ================================================================

# This guide shows how to use the enhanced 12_Calibrar_Optimization_Refined.Rmd
# with parallel processing and comprehensive analysis features.

# REQUIREMENTS
# ============
# - R packages: mizer, therMizer, calibrar, tidyverse, parallel, doParallel, 
#   foreach, gridExtra, corrplot, viridis
# - At least 8GB RAM (16GB+ recommended for parallel processing)
# - Multi-core processor (4+ cores recommended)

# QUICK START GUIDE
# =================

# 1. ENABLE/DISABLE PARALLEL PROCESSING
#    Set this environment variable before running the script:
Sys.setenv(R_PARALLELIZE = "TRUE")   # Enable parallel processing
# Sys.setenv(R_PARALLELIZE = "FALSE") # Disable parallel processing

# 2. MEMORY AND CORE CONFIGURATION
#    The script automatically detects:
#    - Available system RAM
#    - Number of CPU cores
#    - Adjusts cores based on memory (each core needs ~3GB)
#    - Caps at 6 cores for stability

# 3. CHECKPOINT AND RECOVERY SYSTEM
#    The optimization automatically saves:
#    - "optimization_best_params.RDS" - Best parameters found so far
#    - "optimization_latest_checkpoint.RDS" - Full checkpoint every 50 iterations
#    - "optimization_checkpoint_iter_X.RDS" - Individual iteration checkpoints

# USAGE PATTERNS
# ===============

# PATTERN 1: Full Parallel Optimization (Recommended)
# ----------------------------------------------------
# Run the entire 12_Calibrar_Optimization_Refined.Rmd with parallel enabled.
# Expected runtime: 12-24 hours with 4-6 cores
# Memory usage: 12-18GB

# PATTERN 2: Sequential Optimization (Fallback)
# ----------------------------------------------
# If parallel fails or insufficient memory:
Sys.setenv(R_PARALLELIZE = "FALSE")
# Expected runtime: 2-4 days
# Memory usage: 4-6GB

# PATTERN 3: Resume from Checkpoint
# ----------------------------------
# If optimization is interrupted, the script automatically detects checkpoints
# Uncomment these lines in the script to resume:
# initial_params <- last_checkpoint$best_params
# .GlobalEnv$iter_count <- last_checkpoint$iteration
# .GlobalEnv$best_rmse <- last_checkpoint$best_rmse

# PATTERN 4: Analysis Only
# ------------------------
# If you have optimization results and just want analysis:
# Skip to the "analyze-optimal-results" chunk

# KEY FEATURES IMPLEMENTED
# =========================

# 1. PARALLEL PROCESSING
# - Automatic detection of system capabilities
# - Memory-aware core allocation
# - Fallback to sequential if parallel fails
# - Efficient cluster management

# 2. ENHANCED SPECIES PRIORITIZATION
# - 10x weight for sperm whales (poorest historical fit)
# - 8x weight for baleen whales
# - 5x weight for minke whales  
# - 3x weight for orcas
# - 1x weight for all other species

# 3. ROBUST OPTIMIZATION STRATEGY
# - Only marine mammal abundance and gamma optimization
# - Limited catchability variation for well-fitting species
# - Stricter biomass stability criteria (CV < 5%)
# - Extended burn-in (300 years)

# 4. COMPREHENSIVE MONITORING
# - Real-time progress tracking
# - Automatic checkpoint saving every 50 iterations
# - Progress plots every 100 iterations
# - Thread-safe parameter tracking

# 5. PLOTTING INTEGRATION FROM SCRIPT 11
# - Enhanced yield vs observed comparison plots
# - Biomass trajectory plots with uncertainty
# - Species-specific RMSE analysis
# - Parameter evolution tracking
# - Performance summary plots

# OPTIMIZATION METHODS
# ====================

# The script automatically selects the best method:
# - PARALLEL: L-BFGS-B (box-constrained, gradient-based)
# - SEQUENTIAL: SANN (Simulated Annealing, robust but slower)

# EXPECTED PERFORMANCE IMPROVEMENTS
# ==================================

# Parallel processing with calibrar provides:
# - 2-4x speedup with 4-6 cores (70% efficiency)
# - Better convergence with L-BFGS-B method
# - More stable parameter tracking
# - Reduced memory per core (better than manual parallelization)

# TROUBLESHOOTING
# ===============

# If optimization freezes or crashes:
# 1. Check available memory (need 3GB per core)
# 2. Reduce number of cores by setting:
#    n_cores <- 2  # in the setup chunk
# 3. Switch to sequential mode:
#    Sys.setenv(R_PARALLELIZE = "FALSE")
# 4. Check for existing temporary files and remove:
#    file.remove(list.files(pattern = "temp_parallel_.*\\.rds"))

# If parallel cluster fails:
# 1. Restart R session
# 2. Clear any existing clusters:
#    try(stopCluster(cl), silent = TRUE)
# 3. Run with parallel disabled first to test

# MONITORING OPTIMIZATION PROGRESS
# =================================

# The script provides multiple ways to monitor progress:

# 1. Console output every 10-50 iterations:
#    "*** NEW BEST RMSE: 0.145 at iteration 234 ***"
#    "Progress: 250/1000 iterations | 2.3 hours | Best RMSE: 0.145"

# 2. Automatic plots saved to plots/ directory:
#    - optimization_progress.png (updated every 100 iterations)
#    - yield_comparison_enhanced.png
#    - biomass_trajectories.png
#    - species_rmse.png

# 3. Checkpoint files for recovery:
#    - optimization_best_params.RDS (continuous)
#    - optimization_latest_checkpoint.RDS (every 50 iterations)

# INTERPRETING RESULTS
# =====================

# After optimization completes, the script generates:

# 1. PARAMETER VALUES
#    - Marine mammal abundance scaling (how much historical populations exceeded current)
#    - Catchability coefficients (fishing efficiency)
#    - Gamma values (search rates for predators)

# 2. PERFORMANCE METRICS
#    - Overall RMSE between modeled and observed yields
#    - Species-specific RMSE, correlation, and bias
#    - Biomass stability measures (coefficient of variation)

# 3. COMPREHENSIVE PLOTS
#    - Yield time series comparing model vs observations
#    - Biomass trajectories showing ecosystem dynamics
#    - Performance summary by species
#    - Parameter evolution during optimization

# NEXT STEPS AFTER OPTIMIZATION
# ==============================

# 1. Validate results:
#    - Check that RMSE is reasonable (< 0.5 for good fit)
#    - Ensure correlations > 0.7 for most species
#    - Verify biomass stability (CV < 20%)

# 2. Run projections:
#    - Use optimal parameters for future scenario testing
#    - Apply different fishing or climate scenarios
#    - Generate uncertainty bounds through ensemble runs

# 3. Model validation:
#    - Test on independent data if available
#    - Cross-validate with different time periods
#    - Compare with other ecosystem models

# FILES CREATED BY THE OPTIMIZATION
# ==================================

# Data files:
# - sim_optimal_calibrar_parallel.RDS          # Optimal simulation object
# - params_optimal_calibrar_parallel.RDS       # Optimal mizer parameters
# - calibrar_comprehensive_analysis.RDS        # Complete analysis results
# - calibrar_optimization_refined_config.RDS   # Configuration and metadata

# Plots (in plots/ directory):
# - optimal_yield_comparison_enhanced.png      # Yield vs observations
# - optimal_biomass_trajectories.png           # Biomass time series
# - optimal_species_rmse.png                   # Species performance
# - optimization_progress.png                  # Convergence plot
# - optimization_parameter_evolution.png       # Parameter tracking
# - optimization_performance_summary.png       # Overall summary

# Example usage in R console:
# ============================

# # Load and examine results
# results <- readRDS("calibrar_comprehensive_analysis.RDS")
# str(results)
# 
# # Load optimal simulation for further analysis
# sim_optimal <- readRDS("sim_optimal_calibrar_parallel.RDS")
# plotBiomass(sim_optimal)
# 
# # Check parameter values
# params_optimal <- readRDS("params_optimal_calibrar_parallel.RDS")
# species_params(params_optimal)

cat("Parallel optimization guide created. Ready for implementation!\n")
cat("Run: source('parallel_optimization_guide.R') to load this guide\n")
cat("Then execute: 12_Calibrar_Optimization_Refined.Rmd chunks in sequence\n")