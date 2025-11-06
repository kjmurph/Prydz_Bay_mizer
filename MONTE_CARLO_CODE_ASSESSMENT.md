# Monte Carlo Code Assessment and Compatibility Analysis

## Overview
This document assesses the R script "MEMORY EFFICIENT MONTE CARLO ANALYSIS.r" for potential incompatibilities and missing dependencies after running 5000 simulations (51% success rate) on a virtual machine.

---

## ✅ CORE FUNCTIONS PRESENT

### 1. Main Execution Functions
- ✅ `run_monte_carlo_analysis_memory_efficient()` - Primary memory-efficient wrapper
- ✅ `run_enhanced_uncertainty_sims_memory_efficient()` - Core MC engine with block storage
- ✅ `run_single_enhanced_sim_with_params()` - Individual simulation runner with complete parameter storage
- ✅ `run_single_enhanced_sim()` - Alternative single simulation function (simpler version)

### 2. Parallel Processing Functions
- ✅ `run_enhanced_uncertainty_sims_parallel_timed()` - Parallel MC with detailed timing
- ✅ `run_enhanced_uncertainty_sims_parallel_complete()` - Parallel MC with complete parameter storage
- ✅ `run_enhanced_uncertainty_sims_parallel_blocked()` - Parallel MC with block-based checkpointing

### 3. Utility Functions
- ✅ `load_all_simulations()` - Load simulations from block files
- ✅ `load_all_parameters()` - Load parameters from block files
- ✅ `compare_memory_usage()` - Compare memory usage across modes
- ✅ `format_duration()` - Format time durations
- ✅ `calculate_timing_stats()` - Calculate and display timing statistics

### 4. Pilot/Estimation Functions
- ✅ `run_pilot_with_estimation()` - Run pilot to estimate full run time
- ✅ `run_monte_carlo_analysis_timed()` - Main function with pilot option
- ✅ `quick_pilot()` - Quick pilot run wrapper

### 5. Stability Functions
- ✅ `check_biomass_stability_enhanced()` - Enhanced stability checker with CV and trend analysis

---

## ⚠️ POTENTIAL INCOMPATIBILITIES & MISSING DEPENDENCIES

### Critical Missing Functions (MUST BE DEFINED)

#### 1. **`precheck_parameters()`** 
**Status:** Has fallback stub, but functionality may be limited
```r
# Currently has this fallback:
if (!exists("precheck_parameters", inherits = TRUE)) {
  precheck_parameters <- function(...) list(ok = TRUE)
}
```
**Issue:** The stub always returns `ok=TRUE`, bypassing actual validation
**Solution:** Need full implementation from `09_Uncertainty_Analysis.Rmd` (lines 216-317)

**Required Implementation:**
```r
precheck_parameters <- function(p, effort_scen, sim_years = NULL, 
                                min_mature_g = 1, verbose = FALSE) {
  # Validate gear_params
  # Validate species_params  
  # Check catchability bounds
  # Check gamma validity
  # Check erepro values
  # Validate effort scenario
  # Return list(ok = TRUE/FALSE, reason = "...")
}
```

#### 2. **Stability Settings (Global Variables)**
**Status:** Defined but need to be in correct environment
```r
# These must be in .GlobalEnv for cluster export to work:
stability_cv_threshold <- 0.15           # Or 0.25 from your main script
stability_check_years_tail <- 40
stability_min_mean_biomass <- 1e9        # Or 1 from your main script
```

**Issue:** If these are defined in a different script, parallel workers won't have access

**Solution:** Ensure these are defined before calling MC functions

#### 3. **therMizer Functions**
**Status:** Assumed to be loaded from package
```r
# Required from therMizer/mizer packages:
- therMizerEncounter()
- therMizerEReproAndGrowth()
- therMizerPredRate()
- upgradeTherParams()
```

**Issue:** If using custom versions, they need to be exported to cluster

---

### Potential Compatibility Issues

#### Issue 1: Function Environment Mismatches
**Location:** `run_enhanced_uncertainty_sims_memory_efficient()` lines 69-82

**Problem:**
```r
parallel::clusterExport(cl, c(
  "params", "effort_scen", "verbose", "precheck_parameters",
  "catchability_bias_mean_log", "abundance_bias_mean_log", "gamma_bias_mean_log"
), envir = environment())  # ← exports from FUNCTION environment

parallel::clusterExport(cl, c(
  "run_single_enhanced_sim_with_params", "check_biomass_stability_enhanced", 
  "stability_cv_threshold", "stability_check_years_tail", "stability_min_mean_biomass"
), envir = .GlobalEnv)  # ← exports from GLOBAL environment
```

**Risk:** If `run_single_enhanced_sim_with_params` or `check_biomass_stability_enhanced` are not in `.GlobalEnv`, cluster export will fail

**Solution:** Either:
1. Source all functions to `.GlobalEnv` before running
2. Use unified `envir = environment()` strategy
3. Explicitly check function existence before export

#### Issue 2: Missing Data Objects for Cluster
**Location:** Various parallel functions

**Problem:** Some functions reference data objects that may not be exported:
```r
# Referenced but not explicitly exported:
- yield_ts_tidy
- obs_biomass_data_complete
- combined_effort_array
- plankton_forcing (if using therMizer)
```

**Solution:** Add data export to cluster setup:
```r
parallel::clusterExport(cl, c(
  "params", "effort_scen", 
  "yield_ts_tidy",              # ADD
  "obs_biomass_data_complete",  # ADD
  "combined_effort_array"       # ADD if needed
), envir = environment())
```

#### Issue 3: Inconsistent Bias Parameter Defaults
**Location:** Multiple functions

**Problem:** Different default values across functions:
- `run_single_enhanced_sim()`: defaults to `0` (unbiased)
- `run_single_enhanced_sim_with_params()`: defaults to `0` (unbiased)  
- Some older versions may have `log(1.2)` (biased)

**Current Status:** ✅ Appears consistent in current version

#### Issue 4: Block File Path Assumptions
**Location:** `load_all_simulations()`, `load_all_parameters()`

**Problem:**
```r
master_file <- file.path(base_save_path, "master_index.rds")
```

**Risk:** If running on VM with different directory structure, paths may break

**Solution:** Always use absolute paths or check file existence:
```r
if (!file.exists(master_file)) {
  stop("Master index not found: ", master_file)
}
```

#### Issue 5: Memory Mode Confusion
**Location:** Results object structure

**Problem:** Different structures for "efficient" vs "full" mode:

**Efficient mode:**
```r
result$simulation_summaries  # Lightweight summaries in memory
# Full sims stored in block files
```

**Full mode:**
```r
result$all_simulations       # All sims in memory
result$all_successful_parameters
```

**Your situation:** You ran with `memory_mode = "efficient"`, so:
- ✅ `MC_results_efficient` contains only summaries
- ✅ Full simulations are in block files: `mc_10k_blocks/block_0001.rds`, etc.
- ⚠️ Need to use `load_all_simulations()` to reconstruct full dataset

---

## 🔧 WORKFLOW ASSESSMENT FOR YOUR SITUATION

### What You Have:
```r
MC_results_efficient <- run_monte_carlo_analysis_memory_efficient(
  params = optimized_params,
  effort_scenario = combined_effort_array,
  total_sims = 5000,
  memory_mode = "efficient",
  catchability_sd = 2,
  abundance_sd = 4,
  gamma_sd = 2,
  use_bias = FALSE,
  base_save_path = "mc_10k_blocks",  # ← Block files location
  pilot_first = FALSE
)
```

### What's in `MC_results_efficient`:
```r
# Structure:
$run_info
  $total_sims = 5000
  $n_successful = ~2550 (51%)
  $memory_mode = "efficient"

$block_files
  # Vector of block file paths

$simulation_summaries
  # Lightweight summaries (sim_id, block_id, final_biomass, etc.)
  # NOT full MizerSim objects

$timing_summary
  # Performance metrics
```

### What You Need to Do:

#### Step 1: Load Full Simulations (if needed)
```r
# This reconstructs all MizerSim objects from block files
all_sims <- load_all_simulations("mc_10k_blocks")

# Result: List of ~2550 MizerSim objects
length(all_sims)  # Should be ~2550
```

#### Step 2: Load All Parameters
```r
# Get successful parameters
successful_params <- load_all_parameters("mc_10k_blocks", type = "successful")

# Get ALL attempted parameters (including failed)
all_attempted_params <- load_all_parameters("mc_10k_blocks", type = "attempted")

# Get failed simulation parameters
failed_params <- load_all_parameters("mc_10k_blocks", type = "failed")
```

#### Step 3: Check Block File Integrity
```r
# Verify all block files are present
block_files <- MC_results_efficient$block_files
missing_files <- block_files[!file.exists(block_files)]

if (length(missing_files) > 0) {
  warning("Missing block files: ", paste(missing_files, collapse = ", "))
}
```

#### Step 4: Extract Parameter Distributions
```r
# Convert parameters to data frame for analysis
param_df <- do.call(rbind, lapply(successful_params, function(p) {
  data.frame(
    sim_id = p$sim_id,
    species = p$species_names,
    gamma_change = p$gamma_change,
    abundance_scaling = p$abundance_scaling,
    catchability_change = if (!is.null(p$catchability_change)) p$catchability_change else NA,
    stringsAsFactors = FALSE
  )
}))

# Now you can analyze distributions
library(ggplot2)
ggplot(param_df, aes(x = gamma_change, fill = species)) +
  geom_histogram() +
  facet_wrap(~species, scales = "free") +
  scale_x_log10() +
  theme_minimal()
```

---

## 📋 MISSING FUNCTIONS CHECKLIST

### Required for Your Workflow:

- [x] **`run_monte_carlo_analysis_memory_efficient()`** - ✅ Present
- [x] **`run_enhanced_uncertainty_sims_memory_efficient()`** - ✅ Present
- [x] **`run_single_enhanced_sim_with_params()`** - ✅ Present
- [x] **`check_biomass_stability_enhanced()`** - ✅ Present
- [x] **`load_all_simulations()`** - ✅ Present
- [x] **`load_all_parameters()`** - ✅ Present
- [ ] **`precheck_parameters()`** - ⚠️ Only stub present
- [ ] **Stability settings** - ⚠️ Need to verify values match
- [ ] **`getErrorTimeSeriesSpeciesWeighted()`** - ❌ Referenced but not defined
- [ ] **Species weight helper functions** - ❌ Referenced but not defined

### Functions Referenced but NOT Defined:

#### 1. `getErrorTimeSeriesSpeciesWeighted()`
**Used by:** `fastOptimParallelSpeciesWeighted()` (lines 2308-2362)
**Status:** ❌ NOT PRESENT in this file
**Needed for:** Optimization functions (not needed for MC analysis directly)

#### 2. Weight Helper Functions
**Used by:** Wrapper optimization functions
**Status:** ❌ NOT PRESENT
```r
- get_marine_mammal_weights()
- get_balanced_weights()  
- get_krill_focused_weights()
```
**Needed for:** Optimization only (not MC)

#### 3. Optimization Infrastructure
**Status:** ❌ Partially present (lines 2248-2469)
**Components:**
- `fastOptimParallelSpeciesWeighted()` - Defined but depends on missing functions
- `run_marine_mammal_optimization()` - Wrapper (depends on above)
- `run_balanced_optimization()` - Wrapper (depends on above)
- `run_krill_focused_optimization()` - Wrapper (depends on above)

**Impact:** These are for a different workflow (optimization), not required for MC analysis

---

## 🎯 RECOMMENDATIONS FOR CLEANUP

### Priority 1: Essential for MC Analysis

1. **Add Full `precheck_parameters()` Implementation**
   - Copy from `09_Uncertainty_Analysis.Rmd` lines 216-317
   - Place at top of script before any MC functions

2. **Consolidate Stability Settings**
   - Define once at script top:
   ```r
   # === STABILITY SETTINGS ===
   stability_cv_threshold <- 0.15  # Match your optimization value
   stability_check_years_tail <- 40
   stability_min_mean_biomass <- 1e9  # Match your optimization value
   ```

3. **Add Data Export Check**
   - Before cluster export, verify objects exist:
   ```r
   required_objects <- c("params", "effort_scen", "combined_effort_array")
   missing <- required_objects[!sapply(required_objects, exists)]
   if (length(missing) > 0) {
     stop("Missing required objects: ", paste(missing, collapse = ", "))
   }
   ```

### Priority 2: Workflow Improvements

4. **Add Block File Validation Function**
   ```r
   validate_block_files <- function(base_save_path) {
     master_file <- file.path(base_save_path, "master_index.rds")
     if (!file.exists(master_file)) {
       stop("Master index not found: ", master_file)
     }
     
     master <- readRDS(master_file)
     missing <- master$block_files[!file.exists(master$block_files)]
     
     if (length(missing) > 0) {
       warning("Missing ", length(missing), " block files")
       return(list(valid = FALSE, missing = missing))
     }
     
     return(list(valid = TRUE, n_blocks = length(master$block_files)))
   }
   ```

5. **Add Parameter Extraction Helper**
   ```r
   extract_parameter_distributions <- function(base_save_path, param_type = "successful") {
     params_list <- load_all_parameters(base_save_path, param_type)
     
     # Convert to tidy data frame
     param_df <- do.call(rbind, lapply(seq_along(params_list), function(i) {
       p <- params_list[[i]]
       data.frame(
         sim_id = if (!is.null(p$sim_id)) p$sim_id else i,
         species = p$species_names,
         gamma_change = p$gamma_change,
         abundance_scaling = p$abundance_scaling,
         stringsAsFactors = FALSE
       )
     }))
     
     return(param_df)
   }
   ```

### Priority 3: Remove/Separate Optimization Code

6. **Move Optimization Functions to Separate File**
   - Lines 2248-2469 contain optimization functions
   - These depend on missing functions (`getErrorTimeSeriesSpeciesWeighted`, etc.)
   - Move to `optimization_functions.R` or comment out
   - They're not needed for MC analysis

7. **Create Modular Script Structure**
   ```
   ├── mc_core_functions.R          # Core MC functions
   ├── mc_stability_checks.R        # Stability functions
   ├── mc_memory_efficient.R        # Memory management
   ├── mc_utilities.R               # Load/extract/analyze
   ├── optimization_functions.R     # Separate optimization code
   └── mc_main_workflow.R           # User-facing wrappers
   ```

---

## 🔍 SPECIFIC ISSUES FOR YOUR 5000-SIM RUN

### Issue 1: Accessing Results
**Problem:** `MC_results_efficient` is in memory, but full simulations are in block files

**Solution:**
```r
# Option A: Load all simulations (high memory!)
all_sims <- load_all_simulations("mc_10k_blocks")

# Option B: Work with summaries only
summaries <- MC_results_efficient$simulation_summaries

# Option C: Load specific blocks as needed
block_1 <- readRDS("mc_10k_blocks/block_0001.rds")
sims_block_1 <- block_1$successful_simulations
```

### Issue 2: Parameter Analysis
**Problem:** Need to extract parameter distributions from block files

**Solution:**
```r
# Load all successful parameters
successful_params <- load_all_parameters("mc_10k_blocks", "successful")

# Check structure
str(successful_params[[1]])

# Extract to data frame
param_df <- extract_parameter_distributions("mc_10k_blocks", "successful")
```

### Issue 3: Failure Analysis
**Problem:** 49% failure rate - need to understand why

**Solution:**
```r
# Load failed parameters
failed_params <- load_all_parameters("mc_10k_blocks", "failed")

# Analyze failure reasons
failure_reasons <- sapply(failed_params, function(f) f$error)
table(failure_reasons)

# Check if failed parameters were stored
has_params <- sapply(failed_params, function(f) !is.null(f$attempted_parameters))
sum(has_params)  # Should be close to 2450 (49% of 5000)
```

---

## 📦 COMPLETE DEPENDENCY LIST

### R Packages Required:
```r
library(therMizer)        # Core mizer with temperature
library(mizer)            # Base mizer package
library(parallel)         # Parallel processing
library(doParallel)       # Parallel foreach backend
library(foreach)          # Parallel loops
library(optimParallel)    # For optimization functions only
library(reshape2)         # Data reshaping
library(ggplot2)          # Plotting (for analysis)
library(dplyr)            # Data manipulation (for analysis)
library(tidyr)            # Data tidying (for analysis)
```

### Data Objects Required:
```r
# For running MC:
- optimized_params (MizerParams object)
- combined_effort_array (effort array)

# For analysis (optional):
- yield_ts_tidy (observed yield data)
- obs_biomass_data_complete (observed biomass data)
```

### Functions Required in Environment:
```r
# Core (must exist):
- run_single_enhanced_sim_with_params()
- check_biomass_stability_enhanced()
- precheck_parameters()  # ⚠️ Currently only stub

# Utility (in script):
- load_all_simulations()
- load_all_parameters()
- format_duration()
- calculate_timing_stats()
```

### Global Variables Required:
```r
stability_cv_threshold <- 0.15  # or your value
stability_check_years_tail <- 40
stability_min_mean_biomass <- 1e9  # or your value
```

---

## ✨ QUICK FIX TEMPLATE

Here's a minimal script to get your results working:

```r
# === LOAD SCRIPT ===
source("MEMORY EFFICIENT MONTE CARLO ANALYSIS.r")

# === FIX MISSING PRECHECK (if needed) ===
if (!exists("precheck_parameters") || identical(body(precheck_parameters), quote(list(ok = TRUE)))) {
  # Load from your main analysis file
  source("09_Uncertainty_Analysis.Rmd")  # Or define manually
}

# === VERIFY STABILITY SETTINGS ===
if (!exists("stability_cv_threshold")) stability_cv_threshold <- 0.15
if (!exists("stability_check_years_tail")) stability_check_years_tail <- 40
if (!exists("stability_min_mean_biomass")) stability_min_mean_biomass <- 1e9

# === LOAD YOUR RESULTS ===
# If not already in memory:
MC_results_efficient <- readRDS("mc_10k_blocks/master_index.rds")

# === BASIC ANALYSIS ===
cat("Total simulations:", MC_results_efficient$run_info$total_sims, "\n")
cat("Successful:", MC_results_efficient$run_info$n_successful, "\n")
cat("Success rate:", MC_results_efficient$run_info$success_rate * 100, "%\n")

# === LOAD DATA AS NEEDED ===
# Load all successful simulations (WARNING: Memory intensive!)
# all_sims <- load_all_simulations("mc_10k_blocks")

# Load just parameters (lighter)
successful_params <- load_all_parameters("mc_10k_blocks", "successful")
failed_params <- load_all_parameters("mc_10k_blocks", "failed")

# Analyze failures
if (length(failed_params) > 0) {
  failure_reasons <- sapply(failed_params, function(f) f$error)
  print(table(failure_reasons))
}
```

---

## 🎓 SUMMARY

### What's Working:
✅ Core memory-efficient MC engine
✅ Block-based storage system
✅ Parameter storage (successful & failed)
✅ Load/extract utilities
✅ Timing functions

### What Needs Attention:
⚠️ `precheck_parameters()` is only a stub
⚠️ Stability settings may need verification
⚠️ Optimization functions depend on missing code
⚠️ Need helpers to extract parameter distributions

### What's Missing (for optimization only):
❌ `getErrorTimeSeriesSpeciesWeighted()`
❌ Species weight helper functions
❌ Full optimization infrastructure

### Next Steps:
1. Add full `precheck_parameters()` implementation
2. Verify stability settings match optimization values
3. Create parameter extraction helpers
4. Move/remove optimization code (separate workflow)
5. Document expected directory structure for block files

Your MC analysis should work fine with these fixes - the core functionality is all there!
