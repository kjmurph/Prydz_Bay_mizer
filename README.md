# Prydz Bay Size-Spectrum Model

This repository contains a dynamic size-spectrum model for the Prydz Bay region of the Southern Ocean, implemented using the [therMizer](https://github.com/sizespectrum/therMizer) package. The model simulates the dynamics of 19 functional groups, from plankton to whales, under the influence of fishing pressure and climate.

## Model Overview

The Prydz Bay model captures the trophic interactions between marine organisms spanning several orders of magnitude in body size, with a focus on:

- The effects of historical fishing pressure on ecosystem structure
- The impacts of temperature changes on metabolic rates and growth
- The role of vertical migration in structuring the food web 
- The response of key commercial and protected species to these pressures

## Repository Structure

The workflow is organized into sequential R Markdown files:

1. **Data Preparation (00-02)**: Process input data from IWC, FishMIP, and Earth System Models
2. **Model Setup and Calibration (03-05)**: Build and calibrate the model parameters
3. **Simulations (06-08)**: Run steady-state and historical simulations

### Key Files

- **00_Tidying IWC Southern Hemisphere data.Rmd**: Processes whaling catch and effort data
- **01_FishMIP_Fishing_Data.Rmd**: Prepares fishing catch and effort data from FishMIP
- **02_Preparing_Climate_Forcings.Rmd**: Prepares temperature and plankton forcing data
- **03_model_setup_pre_therMizer.rmd**: Initial model setup and calibration
- **04_therMizer_calibration_scale_g_m2.Rmd**: Calibration with therMizer using g/m² units
- **05_therMizer_calibration_scale_model_domain.Rmd**: Scaling to the full model domain
- **06_steady_state_therMizer.Rmd**: Steady-state reference model with fishing effects
- **07_Calibrate_New_Reference_Period_pre_1961.Rmd**: Historical reference period calibration
- **08_ISIMIP3a_simulations_Prydz_Bay.Rmd**: Historical simulations (1961-2010)

## Getting Started

### Prerequisites

- R version 4.0.0 or higher
- Required R packages:
  - therMizer
  - mizer
  - mizerExperimental
  - tidyverse
  - lubridate
  - abind

### Installation

```r
# Install required packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("abind")

# Install mizer packages from GitHub
remotes::install_github("sizespectrum/mizer")
remotes::install_github("sizespectrum/mizerExperimental")
remotes::install_github("sizespectrum/therMizer")
```

### Workflow

1. Run the R Markdown files in sequence from 00 through 08
2. Intermediate model states are saved as RDS files
3. Visualization outputs are saved in the plots directory

## Data Sources

- **Fishing data**: International Whaling Commission (IWC) and FishMIP
- **Climate data**: GFDL Earth System Model from ISIMIP3a
- **Ecosystem structure**: Based on Ecopath model from McCormack et al. (2020)

## Key Parameter Files

- `params_latest_xx.RDS`: Current best parameter set with all 19 functional groups
- `params_07_06_2024.rds`: Recent calibrated parameter set used in simulations

## Results

The model produces several key outputs:

- Biomass time series for each functional group
- Yield comparisons with observed catch data
- Size spectra showing community structure
- Ecosystem indicators (mean size, community slope, etc.)

## Notes

- Some parameter files contain 15 functional groups, newer versions include salps
- The model domain covers approximately 1.47×10¹² m² 
- Historical catch data is limited:
  - Whaling data for minke and baleen whales
  - Krill catch from FishMIP (1974-1996)
  - Limited data for fish (toothfish and demersal fish)

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Citation

If you use this model in your research, please cite:
[Citation information to be added]
