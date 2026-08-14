# `small divers` w_min investigation -- synthesis and index

**Single entry point.** Detail lives in the linked documents; this is the through-line
and the state of play. Last updated 2026-08-02.

> **Update 2026-08-02 — finding 3 is worse than "a reordering".** Re-entering
> `steady()` **halves the unexploited baleen whale stock** and degrades the yield
> fit in 89% of members, which collapsed the manuscript's biomass SNR from -0.920
> to -0.378. It is an artefact and must not be adopted. Rebuild fitted subsets
> from the **stored** ranking instead (bit-exact, ~1 min), which restores
> -0.94. See [`SNR_regression_FINDING.md`](SNR_regression_FINDING.md).

---

## 1. The problem

The `small divers` (penguin) group ran with `w_min = 0.001 g` instead of its
empirical value of ~3.6 kg, through the entire steady-state calibration, the
Monte Carlo campaign, the biomass-stability rejection that produced the accepted
2,111 members, and the top-10% RMSE screen that produced the fitted 212. The
question was whether the calibrated steady state would be materially different
under the correct value, and what that would mean for everything downstream.

## 2. What the answer turned out to be

**The corrected steady state is not materially different, and neither Monte Carlo
gate moves.** But three separate problems surfaced along the way, two of which
matter more than the bug that started it.

| # | finding | severity |
|---|---|---|
| 1 | The w_min correction changes community biomass by **-0.005%** and neither the stability acceptance nor the RMSE ranking materially shifts | low -- resolved |
| 2 | **114 of the 2,111 members are duplicates** (repeated MC configurations concatenated); the fitted 212 contains 23 of them | **high** |
| 3 | **A faithful re-run cannot reproduce the stored ensemble** -- it reorders the RMSE ranking at rho 0.60, ~5,500x more than the correction does | **high** |
| 4 | 3.5% of members cannot have penguin recruitment restored at all (`erepro` > 1 required) | medium |
| 5 | Some accepted members carry `R_max = Inf` for a non-penguin group -- pre-existing, unrelated to w_min | medium |

## 3. Root cause -- not an indexing error

`mizer:::validGivenSpeciesParams` silently rewrites `w_min` whenever
`w_min >= w_mat`, warning only:

```r
wrong <- !is.na(sp$w_min) & !is.na(sp$w_mat) & sp$w_min >= sp$w_mat
sp$w_min[wrong] <- pmin(0.001, sp$w_mat[wrong]/10)     # -> 0.001
```

`small divers` is the **only** group where that holds (`w_min = 4500` vs
`w_mat = 4266.667`), so 0.001 g is mizer's hard-coded fallback, never an authored
value. The "0.001 g equals the mesopelagic fishes w_min" coincidence is a red
herring -- that group genuinely has 0.001 g.

The clamp fired at `newMultispeciesParams`
(`optim_model_setup_old/model_setup_v4.Rmd:378`). A repair was written at `:524-525`
into `params_v2` and then **abandoned at `:545`**, which continues from `params_v1`.

**Why the source value is invalid, and why that is real biology.** Predator `w_min`
derives as `min(w_indep)` -- weight at independence. That is the right quantity:
mizer's `w_min` is the size at which an individual **feeds for itself**, prey
selection scales with body size, and the model has no parental-energy-transfer
mechanism. But penguins fledge heavier than adult breeding mass, so
`w_indep > w_mat` for all three constituent species individually. The constraint is
unsatisfiable with the empirical values, so **`0.85 * w_mat = 3626.667 g` is a
deliberate, necessary compromise**.

**The value is settled -- decided by the project lead, not open for
reconsideration:**

- **Not 94 g / 100 g** (hatchling mass) -- a 94 g penguin would be modelled as an
  independent forager eating 94 g-scaled prey it could never catch.
- **Not 4500 g** (`min(w_indep)`, the empirical value) -- it exceeds `w_mat` and
  triggers the clamp; that is the origin of the bug.
- **No intermediate value**, **no finer `w` grid** to widen the bin count, and
  **no splitting `small divers`** into dependent/independent stages.

The resulting 2-bin size structure (see §6) is an **accepted, reportable
limitation**, to be caveated rather than engineered around.

-> Detail: [`size_parameter_audit.md`](size_parameter_audit.md),
[`../group params/trait_groups_params_vCWC_v5_PROVENANCE.md`](../group%20params/trait_groups_params_vCWC_v5_PROVENANCE.md)

## 4. Stage-by-stage

### Stage A -- fix the source, audit for siblings

Audited all 19 groups across three layers (source trait table, first built object,
ensemble params).

- `small divers` is the **only** group mizer clamped.
- A **second** clamp exists (`w_mat >= w_inf` -> `w_mat <- w_inf/4`) and four
  groups violate it in the source -- but it **did not bite**: three were fixed by a
  manual `w_mat <- 0.9 * w_max` rule and `leopard seals` by raising `w_max` to the
  observed 450 kg. Those exact 0.900 ratios are a *deliberate correction*, not a
  default. Without it minke whales would have been silently set to 1.5e6 rather
  than 5.4e6. In `model_setup_v4.Rmd` the `w_mat` fix lived in `params_v1` and the
  `w_min` fix in `params_v2`, so the `w_mat` half survived and **only the `w_min`
  half was lost**.
- Guard functions added so the clamp can never fire unnoticed again.
- Trait table `v5` emitted carrying the documented compromise; `v4` retained as the
  provenance of the existing ensemble.

-> [`size_parameter_audit.md`](size_parameter_audit.md) |
`R/check_size_params.R`, `R/wmin_test/28_size_param_audit.R`, `35_emit_trait_table_v5.R`

### Stage B -- is a valid recalibration even possible?

An earlier attempt (scripts 19-24) changed `w_min` alone and concluded the
corrected model was *worse* than the uncorrected one: penguins went extinct 5/5,
and with `steady()` they survived only via `R_max = Inf` and `erepro` raised
200-560x.

**That was a method error, not a result.** Since
`RDI = 0.5 * (E_repro %*% dw) * erepro / w[w_min_idx]`, RDI scales as
`erepro / w_min`. The stored penguin `erepro` of 3.1975e-04 was calibrated against
`w_min = 0.001 g` and is ~400x too small at the corrected size. Running
`steady(preserve = "erepro")` pins it, leaving `R_max` as the only free parameter.

The fix is to recalibrate the **pair**. Setting
`erepro = floor / (1 - reproduction_level)`, where the floor restores control
recruitment, gives (6 distinct members, paired):

| erepro setting | R_max after | penguin biomass vs control |
|---|---|---|
| x1 (repro level 0.00) | 1.5e6-2.1e6 finite | 100-120% |
| **x2 (0.50) -- agreed** | 2.8e5-5.2e5 finite | 100-122% |
| x4 (0.75) | 2.9e5-3.8e5 finite | 105-123% |

against `R_max = Inf` and 0.15-0.25% before. All hard constraints hold:
`erepro < 1` for all 19 groups, `R_max` finite, stability 18/18.
`preserve = "erepro"` is kept deliberately -- it holds the other 18 groups fixed so
none can be pushed over `erepro = 1` (`orca` sits at 0.9509).

Sanity check: the recalibrated `erepro` lands among the other endotherms (leopard
seals 0.73, large divers 0.42, minke 0.24), where 3.2e-4 was a
four-order-of-magnitude outlier.

-> [`small_divers_wmin_stageB_results.md`](small_divers_wmin_stageB_results.md) |
`R/wmin_test/26_repro_feasibility.R`, `27_recalibrate_penguin_repro.R`, `29_stageB_gate.R`

### Stage C -- the corrected calibrated steady state

Re-entered at `09_Uncertainty_Analysis.Rmd`'s own scripted `steady()` call
(`:1444`), not `06_steady_state_therMizer.Rmd`, which interleaves interactive
`tuneParams()` Shiny sessions and cannot be reproduced non-interactively.

**This answers the original question:**

| quantity | corrected vs control |
|---|---|
| **total community biomass** | **-0.0048%** |
| penguin biomass | -3.84% |
| largest non-penguin shift | -0.31% (flying birds) |
| median absolute shift, other 18 groups | 0.030% |
| feeding levels, all groups except penguins | unchanged to 4 s.f. |

**Re-running `steady()` alone moves biomasses 0.2-5.8%. The correction is smaller
than the numerical cost of recalibrating at all.**

Output: `params_sel_adj_wmin_corrected.rds`. Neither stored copy of
`params_sel_adj.rds` was touched.

-> [`small_divers_wmin_stageC_results.md`](small_divers_wmin_stageC_results.md) |
`R/wmin_test/30_stageC_corrected_params.R`

### Stage F -- deduplication (run before D, on evidence)

Deduplicating only the fitted subset would have left the ranking contaminated: the
cutoff `ceiling(2111 * 0.10)` is computed from an inflated total, and members just
outside 212 that deserve promotion were never considered. So the whole ensemble was
deduplicated, RMSE recomputed from scratch, and the top 10% re-cut.

| | value |
|---|---|
| ensemble -> distinct parameter sets | 2,111 -> **1,997** |
| redundant copies / groups | **114** / 94 (largest group: 7 copies) |
| clean top 10% | **200** (was 212) |
| retained / promoted / dropped | 189 / **11** / 23 (all 23 were duplicates) |

**Mechanism:** the same MC configuration was executed repeatedly and every output
concatenated. sim_ids 2082/2085/2093 all carry `sim_id_within_file = 36` with
identical stability diagnostics from three different output files.
`set.seed(20250907 + i)` keys the draw on the **index, not the run**, so every
repeat re-draws identical parameters. 241 of 241 duplicate groups span multiple
`source_file` entries.

**Verified two independent ways** -- ensemble objects (94 groups / 114 redundant)
and the RMSE table (94 groups / 208 members - 94 = 114) agree exactly, and 94 of 94
duplicate groups share exactly one RMSE (100%), as they must.

**Do not dedupe from `all_parameters_all_simulations.csv`** -- it holds 2,112 rows
against the RMSE table's 2,111 and aligns under no offset tried (best 38%, where a
correct mapping must give 100%).

The recomputed RMSE reproduces the stored table to **4.9e-15**, validating both the
stored values and the `validParams()` upgrade used to read mizer 2.5.0 objects.

-> [`ensemble_deduplication.md`](ensemble_deduplication.md) |
`R/wmin_test/31_dedupe_parameter_draws.R`, `33_dedupe_full_and_rmse.R`

### Stage D -- do the Monte Carlo gates move?

`small_divers_wmin_test_brief.md` section 6 sets two un-skippable conditions. Both
were tested on the clean top 200, paired, with a **single** `steady()` call
(the driver treats non-convergence as rejection, so a tolerance ladder would
inflate the pass rate).

| condition | result |
|---|---|
| **(i) all fitted members still pass stability** | **PASS** -- 196/200 in *both* arms, **0 change status**, crosstab perfectly diagonal |
| **(ii) top-10% membership unchanged or marginal** | **PASS** -- net **1** member; Spearman rho **0.998**, Kendall tau 0.986; paired RMSE diff median **-7.9e-06 (-0.00044%)** |

-> [`small_divers_wmin_stageD_results.md`](small_divers_wmin_stageD_results.md),
[`small_divers_wmin_stageD2_results.md`](small_divers_wmin_stageD2_results.md) |
`R/wmin_test/34_stageD_stability_paired.R`, `36_stageD2_rmse_ranking.R`

## 5. The finding that most affects what happens next

**A re-run cannot reproduce the stored ensemble.**
`sim@params@initial_n` is the *post*-spin-up state; the pre-spin-up post-`steady()`
state was never saved and cannot be recovered. Any fresh pipeline therefore applies
`steady()` and a spin-up to an already-equilibrated state.

| effect | median RMSE shift | rank correlation vs stored |
|---|---|---|
| **re-running the pipeline** | **+0.0432 (+2.55%)**, max +65.7% | **rho = 0.599** |
| the w_min correction | -7.9e-06 (-0.00044%) | rho = 0.998 |

Of the 102 members crossing the stored rank-200 cutoff, **101 are the re-run and 1
is the correction**.

Consequences: a full re-run produces a **new** ensemble whose top 10% will differ
substantially for reasons unrelated to w_min; **the paired control must be kept**
because re-run absolutes are not comparable with stored values; and the current
fitted-ensemble selection is more fragile than it looks (rho 0.60 under faithful
re-run).

## 6. Verdict

**The w_min correction does not compel an ensemble re-run on its own evidence.**
Both gates pass, and community-level effects are ~0.005%. The reasons to re-run are
the **deduplication** and the **erepro/`R_max` filter`** -- not w_min.

If the ensemble is regenerated anyway, the correction should be applied at the same
time, since it is nearly free once the pipeline is running.

**Caveats to carry into the manuscript regardless:**

- At 3626.667 g the penguin group occupies **2 size bins** against 44 before, so it
  is effectively unstructured in size. Inherent to a species whose
  independent-feeding mass is 85% of its maturation mass. Any penguin-specific
  abundance, biomass or size-structure result needs this caveat.
- **3.5% of members cannot have penguin recruitment restored** (required
  `erepro` > 1, so `R_max = Inf` and penguins settle below control).
- Statistics over the **old 212** overweight 23 duplicated members and overstate
  agreement -- recompute over the clean 200.

## 7. Outstanding

| # | task | cost |
|---|---|---|
| 1 | Rerun acceptance over the full deduplicated ensemble (1,997), paired | ~10 h locally on 14 cores (~5 h corrected-arm only, but see §5 on keeping the control) |
| 2 | Apply the `erepro > 1` / `R_max = Inf` filter, re-cut a refined top 10% | minutes once (1) is done |
| 3 | Regenerate downstream caches, figures and FishMIP outputs | ~1 day |
| 4 | Investigate the pre-existing non-penguin `R_max = Inf` degeneracy | unscoped |
| 5 | Resolve the `whale_consumption_outputs` discrepancy (13-80x, pre-existing) | unscoped |

Note on (2): because `erepro` is capped below 1 in the implementation, "erepro > 1"
and "penguin `R_max = Inf`" identify the **same** members. The two-rule filter is
still worth stating, because `R_max = Inf` *also* catches members degenerate in a
**different** group (mesozooplankton, other macrozooplankton, mesopelagic fishes,
toothfishes) -- a separate, pre-existing problem. The filter therefore does two
jobs, and the counts should not be attributed solely to w_min.

## 8. Reproducing the work

`R/wmin_test/run_all_stages.R` documents and drives the sequence. Scripts are
numbered in dependency order; `00`-`25` are the earlier investigation (Stage 0 and
the superseded Stage 1), `26` onward is the current work.

**Environment:** mizer 3.1.0, therMizer 1.0.0, R 4.6.0 at
`C:\Program Files\R\R-4.6.0\bin\Rscript.exe`. `library(therMizer)` must be attached
for anything touching `params@rates_funcs` -- including `getYield()`. Saved sims are
mizer 2.5.0; use `validParams()` (verified bit-identical, do **not** downgrade
mizer).

**Ground rules observed throughout:** nothing existing was overwritten, every new
output went to `Output_large_files/wmin_test/` or a clearly new filename, exploited
and unexploited arms were treated as matched pairs, and spin-ups were always re-run
rather than reused.

## 9. Document index

| document | contents |
|---|---|
| [`small_divers_wmin_test_brief.md`](small_divers_wmin_test_brief.md) | the original brief; §6 sets the two re-run conditions, §7 the repository gotchas |
| [`small_divers_wmin_stage0_results.md`](small_divers_wmin_stage0_results.md) | Stage 0 analytic bound; **its option (c) recommendation is superseded** |
| [`small_divers_wmin_stage1_results.md`](small_divers_wmin_stage1_results.md) | the w_min-only attempt; **superseded -- it measured an uncalibrated model** |
| [`size_parameter_audit.md`](size_parameter_audit.md) | all 19 groups, both mizer clamps, why only one bit |
| [`small_divers_wmin_stageB_results.md`](small_divers_wmin_stageB_results.md) | reproduction recalibration and the paired gate |
| [`small_divers_wmin_stageC_results.md`](small_divers_wmin_stageC_results.md) | the corrected calibrated steady state |
| [`ensemble_deduplication.md`](ensemble_deduplication.md) | 114 duplicates, mechanism, clean top 200 |
| [`small_divers_wmin_stageD_results.md`](small_divers_wmin_stageD_results.md) | condition (i), stability acceptance |
| [`small_divers_wmin_stageD2_results.md`](small_divers_wmin_stageD2_results.md) | condition (ii), RMSE ranking + the re-run finding |
| [`small_divers_wmin_REVIEW_BRIEF.md`](small_divers_wmin_REVIEW_BRIEF.md) | brief for an independent adversarial review |