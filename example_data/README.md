# Example Subset Data (10 simulations)

Created: 2026-02-19 12:21:05.541591
Source:  Full 2111-simulation Prydz Bay mizer ensemble
Subset:  Simulations 1–10 (sim_id 1:10)

## Files

### fishing_ensemble/
- mc_ensemble_10_example.rds
  Fishing ensemble: list of 10 MizerSim objects (identical structure to
  mc_ensemble_2111_cleaned.rds, just 10 simulations).
  ENSEMBLE_PATHS$fishing should point here for example/testing use.

### climate_only_ensemble/
- climate_only_ensemble_10_example.rds
  Climate-only ensemble: list of 10 MizerSim objects (identical structure to
  climate_only_ensemble_compiled.rds, just 10 simulations).
  ENSEMBLE_PATHS$climate_only should point here for example/testing use.

### ecosystem_assessment/
Pre-computed metric files for the same 10 simulations. Structurally identical
to Output_large_files/ecosystem_assessment/:
- fishing_metrics_raw.rds      (sim_id 1–10 only)
- climate_only_metrics_raw.rds (sim_id 1–10 only)
- b0_reference.rds             (sim_id 1–10 only)
- paired_data.rds              (sim_id 1–10, 170 rows = 10 sims x 17 decades)
- empirical_thresholds.rds     (copied — sim-independent threshold list)

## Usage

To use in run_ecosystem_assessment_v2.R, change ENSEMBLE_PATHS to:
  ENSEMBLE_PATHS <- list(
    fishing      = "example_data/fishing_ensemble/mc_ensemble_10_example.rds",
    climate_only = "example_data/climate_only_ensemble/climate_only_ensemble_10_example.rds"
  )

Or in replot_existing_outputs.R, point data_dir to:
  data_dir <- "example_data/ecosystem_assessment"

