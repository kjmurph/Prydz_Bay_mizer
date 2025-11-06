# Monte Carlo Analysis - Execution Summary

## Analysis Started: October 16, 2025

### Issues Encountered and Resolved:

1. **Missing `load_all_parameters()` function**
   - **Problem**: Helper functions called a function defined in the main MC script
   - **Solution**: Added the function to `mc_analysis_helpers.R` (lines 11-56)
   
2. **Block file path mismatch**
   - **Problem**: Master index stored old paths (`mc_10k_blocks/`) but files are in `Output_large_files/mc_5k_blocks/`
   - **Solution**: Modified `load_all_parameters()`, `extract_biomass_timeseries()`, and `extract_yield_timeseries()` to reconstruct paths using `basename()` and the current `base_save_path`
   
3. **Missing package dependencies**
   - **Problem**: `dplyr`, `ggplot2`, `tidyr`, `scales` not loaded
   - **Solution**: Added `require()` calls at top of `mc_analysis_helpers.R` (lines 11-14)
   
4. **Yield data column name mismatch**
   - **Problem**: Script looked for lowercase `year` and `species` but RDS has `Year` and `Species`
   - **Solution**: Fixed `run_mc_analysis.R` to use correct column names

### Current Status:

✅ **Analysis is RUNNING successfully**

Progress:
- ✅ Loaded 2534 successful parameter sets from 50 block files
- ✅ Loaded 4994 attempted parameter sets  
- ✅ Loaded 2466 failed parameter sets
- ✅ Created parameter distribution plots (gamma, abundance, catchability)
- 🔄 Currently extracting biomass time series from 2534 simulations

### Expected Outputs:

The analysis will create in `mc_analysis_output/`:

1. **gamma_prior_posterior.png** - Gamma (search rate) parameter distributions
2. **abundance_prior_posterior.png** - Initial abundance scaling distributions
3. **catchability_prior_posterior.png** - Catchability distributions by gear
4. **biomass_timeseries_uncertainty.png** - Biomass over time with 90%/50% CI
5. **biomass_obs_period_comparison.png** - Model vs observed biomass (2010-2020)
6. **yield_timeseries_uncertainty.png** - Yield over time with uncertainty

### Parameter Summary Statistics (Successful Simulations):

#### Gamma Changes (Search Rate):
- Most species: median ~1.0× (little change from baseline)
- Range: 0.8× to 31× (5th to 95th percentile)
- Highest variation: Orca (median=1.14, mean=6.77)
- Lowest variation: Medium divers (median=0.862, mean=4.92)

#### Abundance Scaling:
- Highly variable across simulations
- Medians mostly 0.5-1.1×
- Some species show extreme values in mean (due to long tails)
- Examples:
  - Shelf fishes: median=1.10, mean=5536 (huge outliers)
  - Orca: median=0.5, mean=1853

#### Catchability Changes:
- Only fished species show values (gears 4, 7, 8, 11, 12, 16-19)
- Most medians near 1.0× (little change)
- Wide 90% CI (0.03× to 30×) indicates high uncertainty

### Interpretation:

**Success Rate: 50.7% (2534/5000)**
- About half of parameter combinations led to stable simulations
- This is a reasonable acceptance rate for ecological models
- Failed simulations likely violated stability criteria or produced unrealistic dynamics

**Parameter Constraints:**
- Posterior distributions (successful sims) vs Prior (all attempted) will show which parameter combinations are viable
- Narrow posteriors = strong constraints from data/stability
- Wide posteriors = parameter flexibility

**Next Steps After Analysis Completes:**
1. Review parameter distribution plots - identify constrained parameters
2. Check biomass uncertainty plots - assess model fit to observations
3. Examine yield predictions - validate against historical catch data
4. Consider sensitivity analyses for highly uncertain species
5. Use posterior parameter distributions for future projections

### Estimated Completion Time:

- Parameter extraction: ~5 minutes ✅ DONE
- Plot creation: ~2 minutes ✅ DONE  
- Biomass extraction: ~8-12 minutes 🔄 IN PROGRESS
- Biomass plotting: ~2-3 minutes
- Yield extraction: ~8-12 minutes
- Yield plotting: ~2-3 minutes

**Total: ~25-35 minutes from start**

### Files Created:

1. `mc_analysis_helpers.R` - Complete analysis function library (1095 lines)
2. `analyze_mc_results_EXAMPLE.R` - Usage examples (442 lines)
3. `MC_ANALYSIS_QUICK_REFERENCE.md` - User guide
4. `run_mc_analysis.R` - Automated analysis script (this run)
5. `analysis_log.txt` - Log file (if needed)

### Technical Notes:

**Memory Management:**
- Using "efficient" mode: only parameter summaries in RAM
- Full MizerSim objects loaded from disk as needed
- Block-by-block processing prevents memory overflow
- Current RAM usage: ~100-200 MB (vs ~127 GB if all loaded)

**Data Processing:**
- 2534 successful sims × 170 years × 19 species = ~8.2 million data points for biomass
- Similar for yield (fewer years with fishing)
- Uses progress bars for visibility during long operations

**Robustness:**
- Handles missing block files gracefully
- Path-agnostic (works regardless of folder structure)
- Validates data at each step
- Continues even if some species lack data

---

**Last Updated:** Analysis running as of this writing
**Check:** View `mc_analysis_output/` folder for completed plots
**Monitor:** Terminal shows real-time progress with progress bars
