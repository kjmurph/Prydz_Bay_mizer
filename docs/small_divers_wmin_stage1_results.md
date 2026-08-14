# `small divers` w_min test — Stage 1 results (top 5 members)

Follows `docs/small_divers_wmin_stage0_results.md`. Date: 2026-07-29.
Scripts `R/wmin_test/17_*` – `25_*`, outputs `Output_large_files/wmin_test/`.
Nothing existing was modified or overwritten.

**Bottom line.** Correcting `small divers` w_min from 0.001 g to 3626.667 g is
**not a drop-in parameter change**. Applied through the workflow's own protocol
it either kills the penguin group outright (without recalibration) or, after
recalibration, leaves penguins at **0.15–0.25% of their calibrated biomass**,
with density-dependent recruitment removed entirely (`R_max → Inf`) and
reproductive efficiency raised 200–560×. The correction as specified produces a
*worse* model, not a corrected one. Stage 0's recommendation of option (c)
stands, but for a different and stronger reason than Stage 0 gave.

---

## 1. Correction to the Stage 0 report

Two claims in `small_divers_wmin_stage0_results.md` §6 were wrong and are
withdrawn:

- I claimed the ensembles came from a **modified therMizer fork**, inferring
  from the square-root term in the temperature polynomial that upstream lacked
  it. Upstream `therMizer::scaled_temp_effect` computes exactly that form. There
  is no fork. `pak::pak("sizespectrum/therMizer")` installs v1.0.0, which is what
  the workflow Rmds use.
- I therefore advised *against* installing upstream. That advice was wrong and
  cost time. therMizer should simply be installed, and was.

Two Stage 0 results were re-verified against the real package and hold:
its temperature scaling matches `therMizer::scaled_temp_effect` to **2.1e-16**,
and `ther_diet()` remains a bit-identical transcription of `mizer::getDiet`.
The Stage 0 numbers are unaffected.

---

## 2. Method and its validation

**Validation gate.** With therMizer 1.0.0 + mizer 3.1.0,
`project(sim@params, t_start = 1841, effort = sim@effort)` reproduces a stored
ensemble member to **1e-14 in abundance and 3e-15 in biomass** — machine
precision, despite the sims having been written by mizer 2.5.0
(`17_validate_rerun.R`). `sim@params@initial_n` is the post-spin-up state, which
is why this works directly.

**Protocol**, applied identically to control and treated, matching
`09_Uncertainty_Analysis.Rmd`:

```
params -> steady(preserve = "erepro", t_max = 1500) -> 118 yr unfished spin-up
       -> stability check -> 170 yr projection with the member's stored effort
```

`preserve = c("erepro")` and `t_max = 1500` are the driver's values (lines
1585–1590), not the function defaults of `tol = 0.002` / `t_max = 2000` that the
brief quotes. `steady()` is approached through a tolerance ladder
0.1 → 0.05 → 0.01 → 0.0025, each rung starting from the previous, because the
treated params start much further from steady state. All rungs converged for all
members in both conditions.

Both scenarios share one spin-up per (member, condition) and differ only in the
effort array — that is what makes them a matched pair. Three series are carried
throughout: **stored** (existing ensemble), **control** (re-run, no w_min
change), **treated** (re-run, w_min corrected). `treated − control` isolates the
w_min effect; `control − stored` shows what re-running costs, so the two are
never conflated.

**Members.** Top 5 by yield RMSE: `sim_index` 446, 1512, 1819, 2082, 2085.

---

## 3. The correction cannot be applied without recalibration

Run first *without* `steady()` (`19_stage1_rerun.R`), on the reasoning that
re-tuning would mask the effect. That reasoning was wrong, and the failure is
informative:

> **5 of 5 treated members failed the spin-up stability check, entirely on
> `small divers`** (CV 0.467–0.504 against the 0.25 threshold; controls passed at
> 0.055–0.098). Penguins went **extinct**.

Cause (`21_diagnose_extinction.R`). Under `w_min = 0.001 g`, penguin recruitment
is clamped by Beverton–Holt `R_max` at ~10,000× saturation (RDI 3.06e9 vs
R_max 3.04e5). mizer computes `RDI = 0.5 · E_repro · erepro / w[w_min_idx]`, so
moving `w_min_idx` from bin 29 (0.00077 g) to bin 71 (2942 g) divides RDI by
3.8e6. RDI drops to 802, far *below* `R_max`, releasing the clamp. Recruit
biomass is conserved but adult recruitment falls ~20–40×, and the population
declines ~4%/yr to extinction.

So `R_max` and `erepro` are calibrated against `w_min = 0.001 g` and are invalid
at the corrected value. **Recalibration is not optional.**

---

## 4. With recalibration, the group survives but is not restored

`22_stage1_rerun_with_steady.R`. All 5 members converge and **all pass the
stability check** (treated `max_cv` identical to control: 0.055–0.094). But:

| | control | treated |
|---|---|---|
| penguin `R_max` | 3.19e5 – 3.25e5 | **Inf** (all 5) |
| penguin `erepro` | 3.1975e-04 (unchanged) | **6.44e-02 – 1.80e-01** (200–560×) |
| penguin biomass vs control, 2010 | — | **−99.85%** |
| penguin size bins occupied | 45 | **3** (2942 / 4221 / 6054 g) |

Three things follow.

**(a) `preserve = "erepro"` could not be honoured.** For the control, erepro is
held exactly and `R_max` absorbs the adjustment — the intended behaviour. For
the treated runs mizer instead drove `R_max` to infinity and raised `erepro` by
two to three orders of magnitude. Penguin recruitment in the corrected model has
**no density dependence at all**.

**(b) The population is not at a stable new equilibrium — it is slowly
collapsing, and the stability check does not catch it.** Penguin biomass is
already at 0.25% of control at 1841 (the loss happens during `steady()` and
spin-up) and continues to decline through the projection with a half-life of
**128–404 years** (0.17–0.54%/yr). The trend test uses a 2.5%/yr threshold on the
first 50 spin-up years, so a decline this slow passes. **Section 5 output #7 is
not a sufficient screen for this failure mode.**

**(c) The size grid barely represents the corrected group.** The grid is 100 bins
over 15.51 decades = 0.157 decades/bin; corrected penguins span
log10(6000/3626.667) = 0.219 decades. They get 2–3 bins against 45 in the
control. Only `leopard seals` (3 bins) is comparable — every other endotherm has
8–14. Any genuine correction at this `w_min` probably needs a finer `w` grid.

---

## 5. Community-level outputs (treated − control, 2010, median of 5)

| Output | Fished | Unexploited | `control − stored` for scale |
|---|---|---|---|
| 1. Total community biomass | **−1.71%** (range −1.04 to −5.00) | −2.40% | −2.04% |
| 3. Size-spectrum slope λ | **−0.00300** (range −0.00281 to −0.01087) | −0.00314 | −0.00065 |
| 5. Krill consumption, baleen whales | −0.46% | −2.47% | −7.49% |
| 6. Krill consumption, fishes | **+3.44%** | +3.09% | −5.44% |
| 8. Yield RMSE | worsens in **5/5** | — | +0.15% |

For **total biomass the w_min effect (−1.7%) is smaller than the cost of simply
re-running the pipeline (−2.0%)** — not separable from protocol noise. For **λ it
is ~4.6× the pipeline noise** and therefore real, though still only ~21% of the
manuscript's headline paired effect (−0.01404) at the median; for members 4/5 it
reaches −0.0109, i.e. 77% of it.

**The manuscript's headline paired statistic is robust.** Exploited −
unexploited λ at 2010:

| | median | sign consistency |
|---|---|---|
| stored | −0.011277 | 5/5 negative |
| control | −0.010980 | 5/5 negative |
| treated | −0.010840 | 5/5 negative |

The correction shifts both arms almost equally, so the paired difference — the
quantity the manuscript actually reports — moves by **1.3%**. This is the single
most reassuring result in Stage 1.

**RMSE ranking (brief §6 condition ii).** Treated RMSE 1.547–1.599 against a
top-10% cutoff of 1.748. All 5 remain comfortably inside the fitted set, so the
membership condition holds for these members. (Indicative only: a full re-run
would move all 2111.)

---

## 6. Incidental finding — duplicate ensemble members

Not a w_min issue, but it affects how Stage 1 should be read and should be
checked independently.

**208 of the 2111 members share identical RMSE *and* both correlation
statistics** — 94 duplicate groups (`25_rmse_context.R`). The duplication is
concentrated at the top of the ranking: ranks **4–9 are all the same model**
(`sim_index` 2082, 2085, 2088, 2091, 2095, 2111, all RMSE 1.556488). Members 4
and 5 of this test are therefore the same simulation, and reproduce each other
exactly in every number above.

So the "top 5 by RMSE" contains only **3 distinct models** (446, 1512, 1819, and
2082≡2085), and the top-10% fitted ensemble contains fewer independent members
than its size suggests. This inflates apparent agreement in any ensemble
statistic and is worth investigating on its own.

---

## 7. Recommendation

**Unchanged: option (c), crop in post-processing — but the reasoning has
changed and strengthened.**

Stage 0 recommended (c) because sub-threshold penguins exert negligible trophic
effect (≤0.03% of community krill consumption). That remains true. Stage 1 adds
the decisive point: **the corrected model that can actually be built today is
worse than the uncorrected one.** It has penguins at 0.15% of their calibrated
biomass, no density dependence in their recruitment, a reproductive efficiency
raised 500×, a group resolved by 3 size bins, and a degraded catch RMSE.
Substituting it into the manuscript would trade a cosmetic error for a
substantive one.

**Options (a) and (b) are not "re-runs".** Both assume `w_min` can be corrected
and the ensemble regenerated. It cannot, in one step: the penguin group needs
re-calibrating against observed biomass (upstream of the Monte Carlo entirely,
in the `04`/`05`/`06` therMizer calibration Rmds), and probably a finer size
grid. That is a modelling exercise, not a compute job, and it should be
scheduled separately rather than treated as a prerequisite for this manuscript.

**Conditions carried forward from Stage 0**, both still binding: cropping cannot
repair any penguin-specific figure (output #10), and cropping is presentational
— it does not undo predation already exerted.

**What would change this.** If a re-calibrated penguin parameterisation can be
produced — biomass matched to observation, adequate grid resolution, density
dependence retained, stability passed on the *trend* as well as the CV — then
option (b) becomes viable and preferable, and Stage 2 should run on it. Until
then (c) is the honest choice.

**One caveat on scope.** n = 5, and effectively n = 3 distinct models (§6). The
mechanism in §3–§4 is structural rather than stochastic — it follows from how
mizer places recruits at `w[w_min_idx]` — so it will not go away with more
members. But the *magnitudes* in §5 should not be treated as ensemble estimates.

---

## 8. Files

| Script | Purpose |
|---|---|
| `16_inspect_thermizer.R` | upstream therMizer source; settles the fork question |
| `17_validate_rerun.R` | **validation gate** — reproduces stored trajectory to 3e-15 |
| `18_test_wmin_change.R` | establishes how to change w_min (w_min_idx, rates_funcs, initial_n) |
| `19_stage1_rerun.R` | re-run WITHOUT steady() — the extinction result (§3) |
| `20_stage1_compare.R` | comparison for the no-steady run |
| `21_diagnose_extinction.R` | RDI/R_max diagnosis of the extinction |
| `22_stage1_rerun_with_steady.R` | **definitive run**, full protocol with tolerance ladder |
| `23_stage1_compare_steady.R` | **definitive comparison**, section 5 outputs 1/3/5/6/7/8 |
| `24_diagnose_treated.R` | grid resolution and the slow post-recalibration decline |
| `25_rmse_context.R` | RMSE ranking context and the duplicate-member finding |

Outputs: `22_stage1_steady_sims.rds` (all sims), `23_series.rds`, `23_krill.rds`,
`23_yield_rmse.csv`, plus run logs. The no-steady run is kept as
`19_stage1_sims.rds` because it documents the failure mode in §3.
