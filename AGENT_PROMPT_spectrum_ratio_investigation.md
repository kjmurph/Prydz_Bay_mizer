# Agent Investigation Prompt: Prydz Bay mizer — Size Spectrum Ratio + Slope Figure

## Context

This is an R-based marine ecosystem modelling project using the **therMizer** / **mizer** framework (size-spectrum modelling) applied to Prydz Bay, East Antarctica. The model runs from **1841 to 2010** with 19 functional groups covering everything from mesozooplankton to baleen whales. The project is a Monte Carlo (MC) uncertainty analysis: the ensemble consists of **2111 simulations**, each with a different parameter draw, run under both:

- **Exploited** (fished) conditions — historical fishing effort applied
- **Unexploited** (climate-only) conditions — no fishing, climate forcing only

The two ensembles are **matched by index** (sim 1 fished ↔ sim 1 climate-only, same parameter draw), enabling per-pair ratio calculations.

---

## The Calibration / RMSE Filter

The ensemble was filtered to a **top-10% best-fitting subset** based on catch RMSE:

**Script:** `yield_rmse_evaluation.R`  
**Output:** `yield_rmse_per_sim.csv` (columns: `sim_index`, `valid_sim_index`, `rmse`, `rank`)

- Computes RMSE of modelled vs. observed yield across all fished species and years within each species' effort window
- RMSE is on log₁₀(g + 1) scale to equalise species contributions
- Top 10% = 212 sims (RMSE range: 1.54 – 1.75)
- Used by all downstream analysis scripts

---

## The Full Ensemble Files

| File | Description |
|------|-------------|
| `Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds` | Fished (Exploited) MC ensemble — 2111 MizerSim objects. Cleaned by `create_cleaned_ensemble.R` to remove sim 1206 (NA values). Original source was `combined_rerun_successful_sims_20250923_122211.rds` |
| `Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds` | Climate-only (Unexploited) ensemble — compiled by `compile_climate_only_ensemble.R` from individual sim files in `Output_large_files/climate_only_ensemble/individual_sims/` |

Both ensembles have matching `$simulations` list structure where index `i` corresponds to the same parameter draw.

---

## Pipeline for the Previous Figure (v6 — full-ensemble, 2-panel with barplot)

**Main script:** `create_fished_vs_unfished_ratio_plot_FIXED_v6.R`

### What it does:

**Step 1 — Extract community and species spectra (reference period 2001–2010):**
- Loads both full ensembles (2111 sims each)
- For each sim, averages abundance `n[time, species, size]` over years 2001–2010
- Sums across all species → community spectrum per sim (a vector of length = n size bins = 100)
- Also extracts per-species spectra matrices
- Caches results to avoid reloading the ~10GB ensembles

**Caches produced:**
- `ecosystem_assessment_outputs/abundance_ratio_plots/spectra_cache_ref_period.rds`
  - `$w_bins` (100 size bins in grams)
  - `$fished_spectra` — matrix [2111 × 100], mean community abundance per sim over 2001–2010
  - `$climate_spectra` — matrix [2111 × 100], same for climate-only
- `ecosystem_assessment_outputs/abundance_ratio_plots/spectra_cache_species_ref_period.rds`
  - `$sp_names` — vector of 19 species names
  - `$fished_sp` — named list of 19 matrices [2111 × 100]
  - `$climate_sp` — named list of 19 matrices [2111 × 100]

**Step 2 — Compute per-pair ratio:**
- `ratio_matrix[i, j] = fished_spectra[i, j] / climate_spectra[i, j]` for each sim i and size bin j
- Non-finite values set to NA
- Summary stats (median, Q25, Q75, Q05, Q95) computed across the 2111 pairs per size bin

**Step 3 — Compute data-driven dominance bands:**
- Uses `fished_sp_list` median abundances to determine which species group dominates each size bin
- Groups: Krill, Salps, Pelagic fishes, Commercial fishes, Squids, Toothfishes, Flying birds & penguins, Seals, Large marine mammals
- Contiguous runs of the same dominant group are merged into coloured background bands for panel a

**Step 4 — Whale trough annotations:**
- Detects the minimum ratio within each large marine mammal's body-mass window
- Labels Minke whales, Orca, Sperm whales, Baleen whales on the plot

**Step 5 — Panel b (biomass barplot):**
- Uses the per-species spectra to build log-stacked biomass bars (in t km⁻²)
- Bars are aggregated to 60 equal-log-width size bins using `shared_log_breaks`
- Filled by species colour using the "original" palette
- IQR error bars added per bin

**Output:** Multiple PNG variants of the combined 2-panel figure in `ecosystem_assessment_outputs/abundance_ratio_plots/`

---

## Pipeline for the Current Figure (top-10% RMSE, 2-panel with slope timeseries)

**Main script:** `spectrum_ratio_slope_top10pct.R`

### What it does:

**Step 1 — Load RMSE rankings:**
- Reads `yield_rmse_per_sim.csv`
- Selects top 10% = indices `top10pct_idx` (n=212)

**Step 2 — Subset spectra cache to top-10%:**
- Reads `spectra_cache_ref_period.rds` (produced by v6 script above)
- Subsets rows: `fished_top = fished_spectra[top10pct_idx, ]` (212 × 100 matrix)
- Same for `climate_top`
- Computes per-pair ratio matrix and Q25/Q75 across the 212 pairs

**Step 3 — Load species cache for dominance bands:**
- Reads `spectra_cache_species_ref_period.rds`
- Subsets per-species matrices to top-10% rows
- Used only for computing dominance band assignments (not for panel b)

**Step 4 — Panel a (ratio plot, same style as v6):**
- Dominance bands, whale trough annotations, IQR ribbon (Q25–Q75 across the 212 matched pairs), median line

**Step 5 — Load and filter slope data:**
- Reads `Output_large_files/community_slope_analysis/community_slope_full_2111_data.rds`
  - Produced by `run_community_slope_full.R`
  - Contains `$all_slopes` — long-format data frame with columns: `time`, `slope`, `intercept`, `r_squared`, `sim_id`, `ensemble`, `spectrum_type`
  - `ensemble` = "Exploited" or "Unexploited"
  - `spectrum_type` = "Biomass" or "Abundance" (from mizer's `getCommunitySlope()`)
  - Covers years 1841–2010 for all 2111 sims
- Filters to: `sim_id %in% top10pct_idx`, `time >= 1901`, `spectrum_type == "Biomass"`
- Computes annual median, Q25, Q75 across the 212 top-10% sims per ensemble

**Step 6 — Panel b (slope timeseries):**
- Shows biomass-weighted community size spectrum slope from 1901–2010
- Median line + IQR ribbon (Q25–Q75) for both Exploited (red) and Unexploited (blue)
- Vertical dashed lines at 1930 (whaling starts in this model domain) and 1974 (krill fishing starts)

**Output:** `ecosystem_assessment_outputs/abundance_ratio_plots/spectrum_ratio_slope_top10pct.png`

---

## Key Model Facts

- **Model domain:** Prydz Bay, East Antarctica (~1.04 × 10⁶ km²)
- **Simulation period:** 1841–2010
- **Fishing history (approximate):**
  - Large baleen whale hunting: ~1930–1975
  - Sperm whale hunting: ~1950–1979
  - Minke whale hunting: ~1950–1986
  - Krill fishing: 1974–1996 (in this region)
  - Toothfish, coastal fish, squid: varies
- **19 functional groups:** mesozooplankton, other krill, other macrozooplankton, antarctic krill, salps, mesopelagic fishes, bathypelagic fishes, shelf and coastal fishes, flying birds, small divers, squids, toothfishes, leopard seals, medium divers, large divers, minke whales, orca, sperm whales, baleen whales
- **Size bins:** 100 bins on log scale in grams, from ~1 mg to ~100 t
- **Slope metric:** `getCommunitySlope(sim, biomass = TRUE)` from the mizer package — fits a linear regression of log₁₀(biomass density) ~ log₁₀(body mass) across all size bins and species

---

## Files Summary Table

| File | Role |
|------|------|
| `yield_rmse_evaluation.R` | Computes per-sim catch RMSE, produces rankings |
| `yield_rmse_per_sim.csv` | RMSE table: `sim_index`, `rmse`, `rank` — 2111 rows |
| `create_cleaned_ensemble.R` | Removes sim 1206 (NA), saves cleaned 2111-sim ensemble |
| `compile_climate_only_ensemble.R` | Assembles individual climate-only sim files into compiled RDS |
| `run_community_slope_full.R` | Runs `getCommunitySlope()` on all 2111 sims × both ensembles × all years |
| `create_fished_vs_unfished_ratio_plot_FIXED_v6.R` | Previous figure script — extracts spectra, builds 2-panel figure using full 2111 sims. Creates the two `.rds` caches used downstream. Panel a = ratio, Panel b = stacked biomass bars |
| `spectrum_ratio_slope_top10pct.R` | Current figure script — reads the caches, subsets to top-10%, builds Panel a (ratio) + Panel b (slope timeseries) |
| `ecosystem_assessment_outputs/abundance_ratio_plots/spectra_cache_ref_period.rds` | Community spectra matrices [2111 × 100] for both ensembles, reference period 2001–2010 |
| `ecosystem_assessment_outputs/abundance_ratio_plots/spectra_cache_species_ref_period.rds` | Per-species spectra matrices [2111 × 100] for 19 species |
| `Output_large_files/community_slope_analysis/community_slope_full_2111_data.rds` | Per-sim annual slope/intercept for all 2111 sims, both ensembles, both spectrum types |
| `Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds` | Full fished MC ensemble (MizerSim objects) |
| `Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds` | Full climate-only ensemble (MizerSim objects) |
| `params_sel_adj.rds` | Model parameter object — used for `dw` (size bin widths) and `w_max` per species |

---

## Questions / Investigation Tasks for the Agent

Please investigate the following in this codebase:

1. **Verify the top-10% subsetting is correct end-to-end.** Confirm that `top10pct_idx` in `spectrum_ratio_slope_top10pct.R` corresponds to the same parameter draws in both the spectra cache and the slope data. The spectra cache rows are ordered 1:2111 by sim position in the MC list; the slope data uses `sim_id` from `seq_along(sim_list)`. Are these indices consistent?

2. **Investigate the community slope values.** The current panel b shows slopes around −1.08, which seems unusual — most marine ecosystems have biomass spectrum slopes closer to −1 or even flatter. Confirm what `getCommunitySlope(biomass=TRUE)` actually computes and whether the slope values in `community_slope_full_2111_data.rds` are the raw regression slope or normalised in some other way.

3. **Check for year labelling issues in the slope data.** The run log shows `Year range: 1841 – 20111` which suggests a possible data artefact (20111 instead of 2011). Investigate whether `all_slopes$time` contains any spurious year values and whether they affect the filtered subset (1901–2011).

4. **Explore whether the Exploited and Unexploited slope timeseries diverge meaningfully.** Given that the two lines overlap substantially in the current figure, assess whether there is a statistically detectable difference in slope between the Exploited and Unexploited top-10% ensembles, and from which year any divergence begins.
