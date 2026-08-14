# `small divers` w_min -- Stage C: the corrected steady-state model

Generated from `R/wmin_test/30_stageC_corrected_params.R`, 2026-07-29.
mizer 3.1.0, therMizer 1.0.0. Full table:
`Output_large_files/wmin_test/30_stageC_comparison.csv`.

## The question

> Would the initial steady-state model be vastly different with the corrected
> `w_min` for small divers?

**No.** Once the penguin reproduction pair is recalibrated alongside `w_min`, the
corrected steady state is almost indistinguishable from the original at community
level, and the change is confined to the penguin group.

## What was done

Re-entry point is `09_Uncertainty_Analysis.Rmd`'s own scripted `steady()` call
(`:1444`), not `06_steady_state_therMizer.Rmd`, which interleaves interactive
`tuneParams()` Shiny sessions and cannot be reproduced non-interactively.

Starting object is the saved `Manuscript data/params_sel_adj.rds`. The
gear-selectivity block at `:1403-1443` derives `l50`/`l25` from observed whale
catch lengths only -- it does not depend on `w_min` -- so replaying it would
reproduce the same numbers with more ways to go wrong.

| | value |
|---|---|
| `w_min` | 0.001 -> **3626.667 g** (0.85 x `w_mat`) |
| penguin `erepro` | 3.1975e-04 -> **0.73276** |
| penguin `R_max` | 2.9938e+05 -> **5.9857e+05** |
| reproduction level | 0.9997 -> **0.50** |
| `steady()` | `tol = 0.002`, `t_max = 1000`, `preserve = "erepro"`, via a 0.1 -> 0.05 -> 0.01 -> 0.002 ladder |

**Control** is the stored object put through the identical fresh `steady()` call,
because the original run cannot be bit-reproduced. `treated - control` isolates
the w_min correction; `control - stored` shows what re-running `steady()` alone
costs.

## Hard constraints -- all pass

| constraint | result |
|---|---|
| `w_min < w_mat < w_max`, all 19 groups | PASS |
| `erepro < 1`, all 19 groups | PASS (max 0.9509, `orca`, untouched) |
| penguin `R_max` finite | PASS |
| no new `R_max = Inf` degeneracy | PASS (none in either arm) |
| `steady()` converged at every rung | PASS, both arms |

## Result -- the correction is smaller than the noise of re-calibrating

| quantity | treated vs control |
|---|---|
| **total community biomass** | **-0.0048%** |
| penguin biomass | -3.84% |
| largest non-penguin shift | -0.31% (`flying birds`) |
| median absolute shift, other 18 groups | 0.030% |
| largest growth-rate shift at `w_mat` | -0.049% (`leopard seals`) |
| feeding level, all groups except penguins | unchanged to 4 significant figures |

Penguin feeding level moves from 0.1442 to 0.1348 -- the only meaningful
per-group change, and expected, since recruits now enter at 3.6 kg rather than
0.001 g.

**The framing that matters:** re-running `steady()` alone moves the stored
biomasses by 0.2-5.8% (`pct_control_vs_stored`), while the w_min correction moves
them by a median of 0.03%. For penguins the numbers are 5.83% from re-steadying
against -3.84% from the correction; for `flying birds`, 5.13% against -0.31%; for
`salps`, 4.13% against -0.013%.

> The effect of correcting `w_min` on the calibrated steady state is **smaller
> than the numerical cost of re-running the calibration at all**.

## Why this differs from the Stage 1 verdict

Stage 1 (`19`-`24`) changed `w_min` without recalibrating reproduction. Because
`RDI = 0.5 * (E_repro %*% dw) * erepro / w[w_min_idx]`, RDI scales as
`erepro / w_min`, and the stored `erepro` of 3.1975e-04 was calibrated against
`w_min = 0.001 g`. At the corrected size it is ~400x too small, so `R_max` ran to
infinity trying to compensate and penguins collapsed to 0.15-0.25% of control.
Stage 1 measured an uncalibrated model. See
`docs/small_divers_wmin_stageB_results.md`.

## Output

- `params_sel_adj_wmin_corrected.rds` -- the corrected calibrated params
- `Output_large_files/wmin_test/30_params_sel_adj_control_resteadied.rds` -- the control arm

Neither stored copy of `params_sel_adj.rds` (repo root, `Manuscript data/`) was
modified.

## What this does and does not settle

**Settled:** the corrected steady state is not materially different at community
level, and a physically valid calibration exists (`erepro < 1`, finite `R_max`).

**Not settled:** whether the *Monte Carlo* acceptance changes. The stability
rejection runs on a 118-year spin-up per parameter draw, and RMSE depends on
modelled catch. Both could shift even though the base steady state barely moves.
That is Stage D, and it is the gate that decides whether the accepted 2,111 and
the fitted 212 have to be regenerated.

## Caveat to carry into the manuscript

At `w_min = 3626.667 g` the group occupies **2 size bins** against 44 under the
erroneous value (0.219 decades on a 0.155 decades/bin grid), so it is effectively
unstructured in size. This is inherent to a species whose independent-feeding
mass is 85% of its maturation mass, not a consequence of the recalibration. For
context, `leopard seals` already occupies 3 bins and minke and sperm whales 8.
Any penguin-specific size-structure result carries this caveat.
