# `small divers` w_min -- Stage D part (ii): does the RMSE ranking move?

Generated from `R/wmin_test/36_stageD2_rmse_ranking.R`, 2026-07-30.
mizer 3.1.0, therMizer 1.0.0. 200 members x 2 arms, 135.6 min on 14 cores.
Per-member results in `Output_large_files/wmin_test/36_stageD2_rmse.csv`.

## The question

`docs/small_divers_wmin_test_brief.md` section 6 requires two things before a
reduced re-run is defensible: (i) all fitted members still pass the stability
rejection, and (ii) the top-10% membership by RMSE is unchanged or changes only at
the margin. Part (i) is settled -- acceptance is identical, 0 of 200 members
change status (`docs/small_divers_wmin_stageD_results.md`). This is part (ii).

Scope is the clean top 10% (200 of 1,997 distinct members,
`docs/ensemble_deduplication.md`). Each member was run twice through an identical
pipeline: `steady()` -> up to 3 x 118 yr unfished spin-up -> project 1841-2010
under its own effort array -> `getYield` -> RMSE against observed catch, using the
exact metric from `yield_rmse_evaluation.R:112-140`.

## Answer -- the correction barely moves it

All 200 completed in both arms.

| quantity | value |
|---|---|
| paired RMSE difference (corrected - control), median | **-7.878e-06** |
| as % of control RMSE | **-0.00044%** |
| IQR | [-2.32e-05, +2.03e-05] |
| range | [-0.0015, +0.0735] |
| members whose RMSE worsened | 65 of 200 (i.e. roughly symmetric noise) |
| **Spearman rho** | **0.9977** |
| **Kendall tau** | **0.9861** |
| \|rank change\|: median / 90th pct / max | 1 / 2 / 46 |
| correlation screen `cor_log > 0.5` | 200/200 in both arms |
| **net membership change attributable to the correction** | **1 member** |

Restricting to the 193 feasible members gives the same picture (median difference
-8.3e-06, rho 0.9975). The 7 members whose recalibration is infeasible shift by a
median of +0.0011 -- worse, as expected, but the largest is +0.20%.

## The finding that matters more for planning

**Re-running the pipeline at all moves RMSE ~5,500x more than the correction
does.**

| effect | median RMSE shift | IQR width | rank correlation |
|---|---|---|---|
| re-running (control vs stored) | **+0.0432 (+2.55%)**, max +65.7% | 0.0610 | **rho = 0.599** |
| the w_min correction (corrected vs control) | -7.9e-06 (-0.00044%) | 4.4e-05 | rho = 0.998 |

Against the stored rank-200 cutoff (1.751062):

| arm | members above the cutoff |
|---|---|
| stored | 0 of 200 (by construction -- these *are* the top 200) |
| control re-run | **101** |
| corrected re-run | **102** |

So **101 of the 102 crossings are the re-run and 1 is the correction.**

### Why the re-run cannot reproduce the stored ensemble

`sim@params@initial_n` on every saved member is the **post**-spin-up state -- the
pre-spin-up, post-`steady()` state was never saved with the ensembles and cannot
be recovered (noted in `R/wmin_test/19_stage1_rerun.R`). A fresh pipeline
therefore applies `steady()` and a 118 yr spin-up to an already-equilibrated
state, which is not what the original run did. It lands somewhere slightly
different, and yield RMSE is sensitive to that.

This is consistent with Stage C, where re-running `steady()` alone shifted
steady-state biomasses by 0.2-5.8% while the correction shifted them by a median
of 0.030%.

### Consequences

1. **A full re-run produces a new ensemble, not a corrected copy of the old one.**
   Its top 10% will differ substantially from the current one, and that difference
   is overwhelmingly attributable to the re-run, not to fixing `w_min`. Budget
   ~10 h locally on 14 cores for 1,997 members paired (~5 h for the corrected arm
   alone) and expect the membership to change regardless.
2. **The paired design is essential and must be kept.** Absolute pass rates and
   absolute RMSE values from a re-run are not comparable with stored values. Only
   treated-minus-control is interpretable.
3. **The current fitted-ensemble selection is more fragile than it looks.** A
   faithful re-run reorders it at rho = 0.599. That is a robustness property of
   selecting 10% on a yield RMSE that is sensitive to the equilibration path, and
   it is worth stating in the methods independently of `w_min`.

## Verdict on the brief's two conditions

| condition | result |
|---|---|
| (i) all fitted members still pass stability | **PASS** -- 0 of 200 change status |
| (ii) top-10% membership unchanged or marginal | **PASS** -- 1 member, rho 0.998, tau 0.986 |

Both conditions in `docs/small_divers_wmin_test_brief.md` section 6 are met, so
the correction does **not** compel a full re-run of the ensemble on its own
evidence. The reasons to re-run are the *deduplication* (114 redundant members,
23 of them inside the old fitted 212) and the erepro/`R_max` filter -- not `w_min`.

## Caveats

- Membership figures are **indicative**. A definitive test needs the corrected
  model run over the whole deduplicated ensemble, not just its top 10%; members
  currently outside the top 200 could enter.
- The 7 infeasible members (required `erepro` > 1, so `R_max = Inf` and penguins
  settle below control) remain a limitation of the correction on this grid. They
  pass stability and barely move in RMSE, so excluding them is a judgement about
  whether a group that cannot reach its calibrated recruitment belongs in the
  fitted set.
- The 2-bin penguin size structure caveat stands for any penguin-specific result.
