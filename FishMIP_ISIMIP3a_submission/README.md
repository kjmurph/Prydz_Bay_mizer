# Prydz Bay mizer — FishMIP ISIMIP3a submission bundle

This folder contains the outputs for the Prydz Bay mizer model prepared for the FishMIP ISIMIP3a protocol (https://github.com/Fish-MIP/FishMIP2.0_ISIMIP3a).

## Structure
- **submission_csv/** — *upload-ready* CSVs only (no NetCDF/RDS)
  - `histsoc/` — historical socio-economic (with fishing) CSV outputs
  - `nat/` — natural/no-fishing CSV outputs
- **submission_csv_split/** — upload-ready CSVs split by period for DKRZ targets
  - `histsoc/spinup_1841_1960` → upload to `/work/bb0820/ISIMIP/ISIMIP3a/UploadArea/marine-fishery_regional/model_name/temp2`
  - `histsoc/experiment_1961_2010` → upload to `/work/bb0820/ISIMIP/ISIMIP3a/UploadArea/marine-fishery_regional/model_name/temp`
  - `nat/spinup_1841_1960` → upload to `/work/bb0820/ISIMIP/ISIMIP3a/UploadArea/marine-fishery_regional/model_name/temp2`
  - `nat/experiment_1961_2010` → upload to `/work/bb0820/ISIMIP/ISIMIP3a/UploadArea/marine-fishery_regional/model_name/temp`
- **optional_full/** — reference copies not required for submission
  - `histsoc/` — NetCDF + per-simulation RDS for the histsoc ensemble
  - `nat/` — NetCDF + per-simulation RDS for the nat ensemble
- **effort/** — fishing effort used for the histsoc simulations

## Contents
### Upload set (submission_csv/<soc>/)
- CSV per variable: `..._tcblog10_...csv`, `..._tcb_...csv`, `..._tclog10_...csv`, `..._tc_...csv` (FishMIP naming pattern)
- Ensemble-stat CSVs: `tcblog10_ensemble_stats.csv`, `tcb_ensemble_stats.csv`, `tclog10_ensemble_stats.csv`, `tc_ensemble_stats.csv`

### Upload set split by period (submission_csv_split/<soc>/)
- `spinup_1841_1960/` — same filenames, rows filtered to 1841–1960 (use for DKRZ `temp2` upload)
- `experiment_1961_2010/` — same filenames, rows filtered to 1961–2010 (use for DKRZ `temp` upload)

### Reference (optional_full/<soc>/)
- NetCDF allvars bundle per scenario
- Per-simulation raw RDS files (`*_all_sims.rds`)

### Units and conventions (for CSVs which are otherwise metadata-light)
- `tcb` columns: biomass density, units = g m-2.
- `tc` columns: catch density, units = g m-2.
- `tcblog10` and `tclog10` columns: biomass/catch density by FishMIP log10 size class (1-10g, 10-100g, 100g-1kg, 1-10kg, 10-100kg, >100kg), units = g m-2.
- Time column: days since 1841-01-01 00:00:00 (FishMIP ISIMIP3a convention).
- Statistics: median, q05, q25, q75, q95 correspond to ensemble quantiles.

## Effort files
- `effort_array_1841_2010.rds` — effort array used directly in the mizer runs (years 1841–2010, gears × species).
- `effort_array.csv` — readable table of the modified histsoc effort used to build the array.

If anything else is needed for the ISIMIP3a handoff (e.g., metadata form), let me know.
