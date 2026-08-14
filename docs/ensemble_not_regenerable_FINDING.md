# The ensemble cannot be regenerated from its own recorded parameters

Run 2026-08-02/03 from the repository root, mizer 3.1.0 / therMizer 1.0.0 / R 4.6.0.
Scripts: `R/wmin_test/42_recalibrate_base_biomass.R`,
`R/wmin_test/43_recover_member_draws.R`, `R/wmin_test/44_rebuild_from_base.R`.

## Headline

`docs/SNR_regression_FINDING.md` established that re-entering `steady()` on a
stored member's post-spin-up state is an artefact. The obvious remedy was to
rebuild each member the way the original Monte Carlo did — from the calibrated
base params plus that member's recorded draws, then `steady()` → spin-up →
projection. **That does not work either, and the reason is more fundamental: the
recorded draws do not determine the stored states.**

The stored ensemble members are **basin selections**, not consequences of the
Monte Carlo draw. Two members with byte-identical draws sit at whale stocks
differing by 1.88×, and both were accepted, and both entered the published top 10%
— one of them at rank 1.

**Consequence: there is no re-run route that reproduces this ensemble.** Not from
stored states, not from base + draws. The w_min correction cannot be propagated
into a corrected ensemble by re-running.

## 1. The reconstruction itself is exact

Rebuilding a member as base + draws reproduces every recorded parameter exactly.
Over all 1,997 members, comparing the ensemble's own `parameters` record against
the stored member params:

| quantity | max relative difference |
|---|---|
| `gamma` | **0** |
| `catchability` | **0** |
| `erepro` after `steady()` (member 446) | **0** |

So the draws are recovered correctly and applied correctly. Two traps had to be
cleared to get there, both verified against mizer 3.1.0:

- `p@species_params$gamma <- ...` leaves `@search_vol` unchanged — the draw
  silently does nothing. The replacement function `species_params(p) <- sp`
  rebuilds `search_vol` *and* preserves the therMizer `rates_funcs` and
  `resource_dynamics`, so no splice is needed. The original uses the replacement
  functions.
- The parameter CSV cannot supply `abundance_scaling` at all (see §5).

## 2. Identical draws, different stored states

Members **446** and **1512** carry byte-identical `gamma`, `catchability` and
`abundance_scaling`:

| | 446 | 1512 |
|---|---|---|
| stored baleen whale biomass, 1841 | 2.2254e13 | 1.1815e13 |
| ratio | **1.88×** | |

Both are in the accepted 2,111. Both are in the published top 212 (446 is rank 1,
1512 rank 2). They are the same Monte Carlo draw index executed in two different
production runs — the 2,111 came from 33 runs with varying `tol`/`t_max`.

This needs no modelling assumption. The parameters are identical; the outcomes are
not. **The draw does not determine the member.**

## 3. The mechanism: multistability, with the initial condition selecting the basin

Member 446's parameters, run through `steady()` from two different starting
states, at the *same* production settings (`tol = 0.0025`, `t_max = 1500`):

| starting state | resulting baleen whale biomass |
|---|---|
| base × `abundance_scaling` (what the pipeline specifies) | 2.474e12 |
| the stored post-spin-up state | **2.188e13** |
| stored value for reference | 2.225e13 |

**A 9× difference from the initial condition alone**, with identical parameters and
identical numerics. Starting from the stored state, `steady()` returns essentially
the stored value (1.7% apart), so the stored states *are* converged fixed points —
they are simply fixed points of a different basin from the one the documented
pipeline reaches.

Confirmed across the top 6 members: re-entering `steady()` on a stored state leaves
baleen at 0.983–0.991 of its stored value (member 173 is the one exception at
0.59), and total community biomass moves only +1.0% to +4.6%.

## 4. The numerical settings also move the answer

Same parameters, same starting state (base × scaling), varying only `steady()`:

| `tol` | `t_max` | baleen after `steady()` | post-spin-up | converged |
|---|---|---|---|---|
| 0.002 | 1000 | 9.231e11 | 8.217e11 | **no** |
| 0.0025 | 1500 | 2.474e12 | 2.154e12 | yes |
| 0.005 | 1000 | 2.501e12 | 2.176e12 | yes |
| 0.001 | 2000 | 4.014e11 | 3.733e11 | **no** |
| 0.01 | 500 | 2.532e12 | 2.203e12 | yes |

A 6× spread from the tolerance alone. Note `tol = 0.002, t_max = 1000` — the
setting at `09_Uncertainty_Analysis.Rmd:1444` and in scripts 39/40 — **does not
converge** here.

## 5. Two data defects found on the way

**The parameter CSV cannot supply the draws.** Over all 100 members of
`39_params_cache/chunk_001.rds`, matched to
`monte_carlo_2111_summaries/all_parameters_all_simulations.csv` by exact `gamma`:

- the offset is **piecewise, not the uniform +1 previously recorded** — 58 align at
  offset 0 (to `sim_index` 1101), 42 at +1 (from 1296);
- **`gamma` is not a unique key.** 59 of 100 match more than one `sim_id`, and 36 of
  those have candidates that *disagree* on `abundance_scaling` (spread to 2.49×).
  Members 446 and 1512 both match exactly `{446, 1513}`, whose rows carry identical
  `abundance_scaling`.

Use the ensemble's own `$parameters` list (`09:521-536`) instead. It is exact.

**The 1,997 are 1,848 distinct draws.** `33_dedupe_full_and_rmse.R` required
`gamma`, `catchability` **and `initial_n`** to match, where `initial_n` is the
*post-spin-up* state. It therefore deduplicated on **outcome**, not on parameters,
and kept both halves of every pair like 446/1512. Deduplicating on the draws gives
**1,848** (149 further redundant copies, in 149 groups of 2). If an ensemble is
ever rebuilt, 1,848 is the count and the top 10% is `ceiling(1848 × 0.10) = 185`.

**The non-convergence guard now works.** `projectToSteady.MizerParams` signals with
`message("Simulation run did not converge after ", ...)`; the only `warning()` there
is the extinction notice. Every handler in the repo (`30:100-104`, `39:152-160`,
`40:192-198`) listens for `warning` and so never fired. With a `message` handler,
**3 of 40** pilot members were correctly rejected.

## 6. What this means

The published ensemble's whale stocks — which carry the community-biomass signal —
are not reproducible from the recorded parameters, because the equilibration path
that selected each member's basin was not recorded and varied across the 33
production runs.

- **Do not re-run.** Neither route works, and a re-run produces a new ensemble
  unrelated to the published one. The ~9 h VM job was not launched.
- **Rebuild the fitted set from the stored ranking** — dedupe → filter → cut. That
  is bit-exact against the caches and is what `docs/SNR_regression_FINDING.md` §6
  already recommends; this document is a second, independent route to the same
  conclusion.
- **Report the w_min correction at the base-params level**, where it is now properly
  calibrated (§7), and state its ensemble-level effect from the paired Stage D
  measurements (community biomass ×1.000, acceptance unchanged for 0 of 1,997).
- **The multistability is worth reporting in its own right.** 446/1512 is a
  controlled demonstration: identical parameters, 1.88× different baleen whale
  biomass, both accepted. It bears directly on how much weight per-member results
  can carry.

## 6b. Building a NEW ensemble is feasible — and the selection metric is the catch

Kieran reframed the objective on 2026-08-03: not reproduction, but an ensemble
built comprehensively and transparently (dedupe genuine parameter repeats → correct
w_min → recalibrate to observed biomass → steady → spin-up → rejection criteria →
full simulations). Measured on a **random 60 of the 1,848** draw-distinct members,
treated arm, unfished calibration:

| | value |
|---|---|
| acceptance (converged **and** stable) | **53/60 = 88%** |
| Spearman rho vs the stored ranking | **0.914** (re-steady pipeline: 0.599) |
| abundance draw → 1841 whale stock, log-log r | **0.957** (published ensemble: 0.766) |
| best yield RMSE in sample | 1.738 — would rank ~157 of 1,997 stored |

The design works: the outcome follows the recorded draw more tightly than in the
published ensemble, and `steady()` retains ~77% of the abundance perturbation
rather than erasing it.

**An earlier claim in this document was too pessimistic and is corrected here.**
The whale-catch shortfall is not a stock limit. Catchability sweep on two cached
members (exact — the spin-up is unfished, so catchability enters only the
projection):

| member 1339, 1841 stock 3.69e12 | catch ratio | yield RMSE |
|---|---|---|
| q = 0.0145 (as drawn) | 0.094 | 1.8854 |
| q = 0.10 | 0.460 | 1.8742 |
| **q = 0.25** | **0.721** | 1.8801 |
| q = 1.00 | 1.039 | 1.9604 |

| member 1265, 1841 stock 5.91e11 | catch ratio | yield RMSE |
|---|---|---|
| q = 0.155 (as drawn) | 0.098 | 1.7842 |
| q = 1.00 | 0.173 | 1.8739 |

**The published ensemble's 0.69 is reachable** — member 1339 exceeds it at
`catchability = 0.25`, only 5× the base 0.05 and an unremarkable draw. But it needs
**both** a large abundance draw *and* a workable catchability: at a 5.91e11 stock,
catchability saturates the catch at 0.17 and cannot compensate. Joint requirement
(abundance ≥ ~15×, catchability ≥ ~0.1) is ~1.7% of draws, so expect **~30 such
members across the 1,848** — the random 60 contained large-stock members (3.69e12,
3.16e12, both above the published 2.53e12 median) that simply drew catchability of
0.014 and 0.002.

**The consequential finding: the selection metric barely responds to the whale
catch.** Member 1339 moves from catch ratio 0.094 to 0.721 — a 7.7× change in the
manuscript's headline group — while its log10 yield RMSE moves 1.8854 → 1.8801,
i.e. 0.3%. A top-10% cut on this metric therefore **does not select for whale
realism**, and never did. If baleen whales carry the argument, the fitted-subset
criterion needs a whale-specific term (or a per-species weighting); that is a
selection-design issue independent of w_min, the deduplication and the re-run.

## 7. What is usable from this work

`R/wmin_test/42_recalibrate_base_biomass.R` produced the first base params that are
**both** w_min-corrected **and** re-fitted to the 2001–2010 observed biomass — the
step every earlier corrected object skipped, having recalibrated penguin `erepro`
against a recruitment-restoration target rather than against data.

Ratios are on the `getBiomass(use_cutoff = TRUE)` basis that `matchBiomasses`
actually fits (every group carries a `biomass_cutoff`; `small divers` is 4500 g):

| object | max deviation, 19 groups | `small divers` |
|---|---|---|
| stored `params_sel_adj.rds` | 1.72% | 1.0172 |
| **recalibrated control** (w_min uncorrected) | **1.18%** | 1.0118 |
| **recalibrated treated** (w_min corrected) | **1.23%** | 1.0049 |

Both arms beat the stored base's own calibration quality, `erepro < 1` for all 19
(max 0.9509, orca, unchanged), `R_max` finite throughout. Penguin biomass sitting
below the 4500 g cutoff falls from **13.5%** (control) to **9.2%** (treated) — the
residual the correction is meant to shrink; it does not reach zero because the
realised `w_min` (2942.204 g) is still below the cutoff.

Outputs: `params_sel_adj_wmin_corrected_biocal.rds`,
`Output_large_files/wmin_test/42_control_biocal.rds`, `42_biomass_ratios.csv`,
`42_ladder_trace.csv`, `43_member_draws.rds`.
