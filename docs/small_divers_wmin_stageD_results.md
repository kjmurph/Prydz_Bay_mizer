# `small divers` w_min -- Stage D: does the correction change Monte Carlo acceptance?

Generated from `R/wmin_test/34_stageD_stability_paired.R`, 2026-07-30.
mizer 3.1.0, therMizer 1.0.0. Per-member results in
`Output_large_files/wmin_test/34_stageD_stability.csv`. Runtime 73.9 min on 14
cores.

## The question

`docs/small_divers_wmin_test_brief.md` section 6 carries an un-skippable warning:
re-running only the fitted subset assumes the accepted set and the RMSE ranking
are unchanged, and **both depend on `w_min`** -- the stability check runs on the
spin-up, and RMSE depends on modelled catch. This settles part (i).

> Do the fitted members still pass the biomass-stability rejection criteria under
> the corrected `w_min`?

## Answer -- yes, acceptance is unchanged

Membership is the **deduplicated** clean top 10% (200 members of 1,997 distinct;
see `docs/ensemble_deduplication.md`), each run twice through one identical
pipeline.

| | control | corrected |
|---|---|---|
| pass | **196 / 200 (98.0%)** | **196 / 200 (98.0%)** |
| lost (pass -> fail) | -- | **0** |
| gained (fail -> pass) | -- | **0** |

The crosstab is perfectly diagonal:

```
         corrected
control   FALSE  TRUE
  FALSE       4     0
  TRUE        0   196
```

The same four members fail in both arms, for the same reason (`unstable`).
**No member changes rejection status.** The brief's trigger for "a full re-run of
all 2,111 is mandatory" does **not** fire on stability grounds.

The four that fail in both arms were accepted by the original campaign under
different settings -- the accepted ensemble is a concatenation of runs with nine
prior-SD combinations and varying `tol`/`t_max`, so a single replication setting
cannot reproduce every acceptance. That is precisely why the design is paired and
only the *change* is interpreted.

## Method

| | |
|---|---|
| correction | `w_min` 0.001 -> 3626.667 g, penguin `erepro`/`R_max` recalibrated to reproduction level 0.50 |
| `steady()` | **single call**, `tol = 0.002`, `t_max = 1200`, `preserve = "erepro"` |
| spin-up | up to 3 unfished cycles of 118 yr, early exit once stable |
| rejection | `check_biomass_stability_enhanced` thresholds: CV 0.25 over the final 40 yr, trend 2.5%/yr with p < 0.05 over the first 50 yr |

A **single** `steady()` call is used, not the tolerance ladder of Stages B and C:
`run_single_enhanced_sim` (`09_Uncertainty_Analysis.Rmd:319-543`) treats
non-convergence as outright rejection, so a ladder would rescue draws the real
procedure discards and inflate the pass rate.

`preserve = "erepro"` is retained because it holds the other 18 groups fixed, so
none can be pushed over `erepro = 1` as a side effect. `orca` sits at 0.9509.

## The real limitation -- 3.5% of members cannot have penguin recruitment restored

The `erepro` that restores control recruitment at the corrected `w_min` (the
floor, with no density dependence) varies far more across 200 members than the
6-member pilot suggested:

| | value |
|---|---|
| floor, min / median / max | 0.024 / **0.088** / **2.76** |
| floor > 1 -- unreachable at ANY reproduction level | **7 of 200 (3.5%)** |
| cannot reach reproduction level 0.50 within the 0.95 cap | 18 of 200 (9%) |
| new `R_max = Inf` relative to control | 2 members |

For those 7, no `erepro <= 1` lifts RDI above the control RDD, and since
`RDD <= RDI` always, **no finite `R_max` reproduces control recruitment**.
`R_max = Inf` (no density dependence, `RDD = RDI`) is the maximum achievable and
their penguin population necessarily settles **below** control. This is a genuine
limitation of correcting `w_min` on this grid, not a coding failure, and it should
be reported rather than smoothed over.

It does not change acceptance: restricting to the 193 feasible members gives
189/193 passing in **both** arms, again identical.

## Hard constraints

| constraint | result |
|---|---|
| `erepro < 1` for all 19 groups, every member | PASS |
| penguin `R_max` finite, feasible members | PASS |
| no member loses stability | PASS (0 of 200) |

## What this settles, and what remains

**Settled -- part (i).** The accepted set is stable under the correction. Nothing
about the stability rejection compels regenerating the full 2,111.

**Outstanding -- part (ii).** Whether the **RMSE ranking** shifts. RMSE depends on
modelled catch, so it can move even with acceptance fixed. Testing it needs the
corrected members projected 1841-2010 under the fishing effort array and the yield
RMSE recomputed -- the natural next step, and cheap now that the pipeline is
validated and parallel.

Note the ranking has *already* changed for a reason unrelated to `w_min`:
deduplication drops 23 redundant members from the old 212 and promotes 11 new
models into the clean 200 (`docs/ensemble_deduplication.md`).

## Caveat to carry into the manuscript

At `w_min = 3626.667 g` the group occupies **2 size bins** against 44 under the
erroneous value, so it is effectively unstructured in size. Combined with the 3.5%
of members whose penguin recruitment cannot be fully restored, any
penguin-specific abundance, biomass or size-structure result needs an explicit
caveat. Community-level results are unaffected -- Stage C puts total community
biomass at -0.0048%.