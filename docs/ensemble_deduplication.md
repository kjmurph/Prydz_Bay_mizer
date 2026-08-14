# Duplicate members in the 2,111 Monte Carlo ensemble

Generated from `R/wmin_test/31_dedupe_parameter_draws.R` and
`33_dedupe_full_and_rmse.R`, 2026-07-29. Outputs in
`Output_large_files/wmin_test/` and `Manuscript data/yield_rmse_per_sim_deduped.csv`.

**This is not a `w_min` issue.** It was found while investigating that, but it is
an independent defect in the ensemble and needs fixing regardless of the outcome
of the `w_min` work.

## Headline

| | value |
|---|---|
| accepted ensemble | 2,111 |
| **distinct parameter sets** | **1,997** |
| redundant copies | **114** |
| duplicate groups | 94 |
| largest group | 7 copies of one model |
| old top 10% | `ceiling(2111 * 0.10)` = 212 |
| **clean top 10%** | **`ceiling(1997 * 0.10)` = 200** |
| duplicates inside the old 212 | **23** |
| members promoted into the clean 200 | **11** |

## What counts as a duplicate

All **three** varied quantities must match. Matching on two while the third
differs is a distinct parameter set and is kept:

| quantity | source |
|---|---|
| `gamma` (search rate) | `species_params$gamma` |
| `catchability` | `gear_params$catchability` |
| abundance scaling | `initial_n` (deterministic in the abundance draw) |

## Mechanism -- repeated runs, concatenated

`combined_rerun_params_summary_20250923_122211.csv` carries `run_id`,
`source_file` and `sim_id_within_file`. It shows the same Monte Carlo
configuration was executed repeatedly and every output concatenated:

| sim_id | run_id | source_file | sim_id_within_file | max_cv |
|---|---|---|---|---|
| 2082 | `seed=median;nsims=50;sd=1_4_2;tol=0.002;tmax=1000` | `..._20250916_051255.rds` | 36 | 0.07491564 |
| 2085 | same | `..._20250916_051542.rds` | 36 | 0.07491564 |
| 2093 | same | `..._20250916_064722.rds` | 36 | 0.07491564 |

Three output files, three timestamps, one simulation. `run_single_enhanced_sim`
seeds as `set.seed(20250907 + i)` -- keyed on the **draw index, not the run** --
so every repeat of a configuration re-draws identical parameters. Results are
then collected by success order, so the copies survive into the accepted set.
**241 of 241 duplicate groups in the parameter summary span multiple
`source_file` entries (100%).**

## Verification

Two independent routes agree exactly:

| route | duplicate groups | redundant copies |
|---|---|---|
| ensemble objects, all three parameters | **94** | **114** |
| RMSE table, identical `rmse` + `cor_raw` + `cor_log` | **94** | 208 members - 94 groups = **114** |

and the consistency test passes: **94 of 94 duplicate groups share exactly one
RMSE (100%)**, as they must, since identical parameters give identical results.

The recomputed RMSE also reproduces the stored `yield_rmse_per_sim.csv` for all
2,111 members to **max |difference| 4.9e-15**, which validates both the stored
table and the `validParams()` upgrade used to read the mizer 2.5.0 objects under
mizer 3.1.0.

## Do not dedupe from the parameter summary CSV

`monte_carlo_2111_summaries/all_parameters_all_simulations.csv` and the
provenance file hold **2,112** rows; `yield_rmse_per_sim.csv` holds **2,111**
(member 1206 dropped by `create_cleaned_ensemble.R`). They do not align under any
offset tried -- `+0`, `+1`, `-1`, or shifting indices `>= 1206`:

| mapping | duplicate groups with consistent RMSE |
|---|---|
| offset +0 | 39/241 (16%) |
| offset +1 | 61/241 (25%) |
| offset -1 | 30/241 (12%) |
| shift >= 1206 by +1 | 92/241 (38%) |

A correct mapping must give 100%. That CSV reports 241 groups / 263 redundant,
disagreeing with both verified routes above. It is accurate about the draws it
records but sits in a different indexing, so it cannot be used to say **which**
ensemble members are duplicates. The ensemble objects share the RMSE table's
indexing (`yield_rmse_evaluation.R` sets `sim_index = seq_len(n_sims)` over the
same valid-sim filter), and Stage 0 verified that mapping independently by
matching recomputed biomass to relative difference 0.

## Why the whole ensemble had to be deduplicated, not just the 212

The fitted set was `ceiling(2111 * 0.10)` taken from a ranking over an ensemble
containing repeated draws, so **both the cutoff and the ordering were
contaminated**. Deduplicating only the 212 would leave two problems: the cutoff N
is computed from an inflated total, and members ranked just outside 212 that
deserve promotion once duplicates are removed are never considered. Correct order
is **dedupe -> recompute RMSE -> re-cut**.

Doing so drops 23 redundant members from the old 212 and promotes 11 genuinely
new models into the clean 200. All 200 pass the `cor_log > 0.5` screen.

## Consequence for results already produced

Any ensemble statistic computed over the old 212 -- median, IQR, sign
consistency, "100% of pairs negative" -- **overweights the repeated draws and
overstates agreement**, because 23 of the 212 are copies and the duplication is
concentrated near the top of the ranking. Those statistics should be recomputed
over the clean 200.

## Outputs

- `Manuscript data/yield_rmse_per_sim_deduped.csv` -- clean ranking, 1,997 members
- `Output_large_files/wmin_test/33_dedupe_full.rds` -- full result incl. the clean top-200 membership
- `Output_large_files/wmin_test/33_all_sims_rmse_dedup_flags.csv` -- per-member duplicate flags

Nothing existing was overwritten; `yield_rmse_per_sim.csv` is untouched.