# Summary of 2111 Monte Carlo Simulation Data Extraction

## Overview
This document summarizes the data extracted from the Monte Carlo simulations that produced the biomass and yield figures showing 2111 accepted simulations.

## Data Source
- **Source File**: `Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/combined_rerun_successful_sims_20250923_122211.rds`
- **Total Simulations in File**: 2112
- **Displayed in Figures**: 2111 (one simulation likely filtered out due to biomass rejection criteria)
- **Date Created**: September 23, 2025

## Prior Parameter Space (All Attempted Simulations)

### Total Simulation Attempts
- **Total MC Runs**: 33 separate Monte Carlo runs
- **Total Attempts**: 14,812 parameter combinations
- **Successful**: 2,112 simulations (14.26% success rate)
- **Final Accepted**: 2,111-2,112 (after biomass rejection criteria)

### Parameter Variation (Standard Deviations Used)

**Catchability SD**: 1, 2, 3, 4
- Controls the variation in fishing catchability parameters
- Higher values = more variation in catchability

**Abundance Scaling SD**: 2, 3, 4, 5
- Controls the variation in initial abundance scaling
- Higher values = more variation in starting abundances

**Gamma SD**: 1, 2, 3, 4, 5
- Controls the variation in the gamma parameter (search volume rate)
- Higher values = more variation in species interaction strength

### SD Combinations Explored
Nine unique combinations of standard deviations were used across all runs:
1. catchability=1, abundance=2, gamma=1
2. catchability=1, abundance=2, gamma=2
3. catchability=1, abundance=3, gamma=2
4. catchability=1, abundance=4, gamma=2
5. catchability=2, abundance=4, gamma=3
6. catchability=3, abundance=4, gamma=3
7. catchability=3, abundance=5, gamma=4
8. catchability=3, abundance=5, gamma=5
9. catchability=4, abundance=5, gamma=4

## Posterior Parameter Space (Accepted Simulations Only)

### Per-Species Parameter Ranges
The accepted simulations span the following parameter ranges for each species (see `parameters_min_median_max_posterior.csv`):

- **Gamma**: Species-specific search volume rates
  - Example: Antarctic krill gamma ranges from 7.98e-13 to 2.12e-10
  
- **Catchability**: Fishing vulnerability (for fished species only)
  - Example: Antarctic krill catchability ranges from 0.000007 to 0.0304
  
- **Abundance Scaling**: Initial abundance multipliers
  - Each species has its own range of scaling factors
  - Represents uncertainty in starting biomass estimates

## Biomass Results

### Format
Min, Median, and Max biomass across all 2111 simulations, plus observed biomass.

### Units
- Grams (g): Raw model output
- Tonnes (t): Converted for easier interpretation (divide by 1,000,000)

### Species Covered
All 19 species in the model:
1. mesozooplankton
2. other krill
3. other macrozooplankton
4. antarctic krill
5. salps
6. mesopelagic fishes
7. bathypelagic fishes
8. shelf and coastal fishes
9. flying birds
10. small divers
11. squids
12. toothfishes
13. leopard seals
14. medium divers
15. large divers
16. minke whales
17. orca
18. sperm whales
19. baleen whales

### Key Findings
- Median values represent the central tendency across the ensemble
- Min/Max values show the full range of uncertainty
- Observed values can be compared to modeled ranges to assess model fit

## Yield Results

### Format
Min, Median, and Max yield across all 2111 simulations, plus observed yield statistics.

### Units
- Grams per year (g/year): Raw model output
- Tonnes per year (t/year): Converted for easier interpretation

### Fished Species (9 species)
1. antarctic krill
2. bathypelagic fishes
3. shelf and coastal fishes
4. squids
5. toothfishes
6. minke whales
7. orca
8. sperm whales
9. baleen whales

### Observed Yield Data
For each fished species, the CSV includes:
- **ObsYield_min**: Minimum observed yield across all years
- **ObsYield_mean**: Mean observed yield across all years
- **ObsYield_max**: Maximum observed yield across all years

## Output Files

All files are located in the `monte_carlo_2111_summaries/` directory:

### Primary Data Files
1. **biomass_min_median_max_with_observations.csv**
   - Min, median, max modeled biomass for each species
   - Observed biomass for comparison
   - Both grams and tonnes units

2. **yield_min_median_max_with_observations.csv**
   - Min, median, max modeled yield for each fished species
   - Observed yield statistics (min, mean, max)
   - Both grams/year and tonnes/year units

3. **parameters_min_median_max_posterior.csv**
   - Parameter ranges for accepted simulations (posterior distribution)
   - Gamma, catchability, and abundance scaling for each species
   - Shows the parameter space that produced viable simulations

### Prior Parameter Space Files
4. **prior_parameter_space_detailed.csv**
   - Overall summary of Monte Carlo setup
   - Standard deviations used
   - Total attempts and success rate

5. **prior_sd_combinations.csv**
   - The 9 unique SD combinations explored
   - Shows the breadth of parameter space searched

6. **prior_all_mc_runs_details.csv**
   - Details of each individual MC run
   - Which SD combination was used in each run

### Detailed Data File
7. **all_parameters_all_simulations.csv**
   - Complete parameter sets for all 2112 simulations
   - One row per species per simulation (40,128 rows)
   - Useful for detailed statistical analysis

## Interpretation Guide

### Biomass Ensemble
The ensemble median represents the "best estimate" from the 2111 accepted simulations. The min and max show the range of uncertainty given the parameter variations explored.

**Example**: If the median biomass for a species is close to observed, and observed falls within the min-max range, this suggests the model can reproduce realistic biomass levels.

### Yield Ensemble  
Similar to biomass, but note that many yields have median=0 because yields are only non-zero in specific years when fishing occurs. The max values are more informative for yield.

**Example**: For species like baleen whales with historical whaling, the max modeled yield should be comparable to historical catch records.

### Parameter Ranges (Posterior)
These show which parameter combinations led to successful simulations. Species with wide parameter ranges have more uncertainty, while narrow ranges indicate parameters are well constrained by the data.

## Use Cases

### For Publications
- Use median values as point estimates
- Use min-max ranges for uncertainty bounds
- Compare observed vs. modeled to assess model fit

### For Projections
- The parameter posterior can be sampled to generate projections
- Each parameter set represents a plausible ecosystem state
- Ensemble projections preserve parametric uncertainty

### For Model Evaluation
- Check if observed values fall within modeled ranges
- Identify species with poor fit (observed outside range)
- Use parameter constraints to improve future calibrations

## Notes

1. The difference between 2112 (in file) and 2111 (in figures) likely reflects one simulation being rejected during biomass filtering with a 1 kg extinction threshold.

2. The success rate of 14.26% indicates that most random parameter combinations did not produce viable ecosystems - this is expected and reflects the strong ecological constraints.

3. All simulations passed initial stability checks and yield fitting criteria before being included in the ensemble.

4. The parameter space was explored using Latin Hypercube Sampling (implicit in the MC approach) to ensure good coverage of the prior distributions.

## Contact & Reproducibility

All extraction scripts are available in the repository:
- `extract_2111_simulation_data_to_csv.R` - Main extraction script
- `extract_prior_parameter_space.R` - Prior parameter space analysis

To reproduce these results, run the extraction scripts from the project root directory. The scripts automatically find and process the Monte Carlo results files.

---
Generated: November 5, 2025
