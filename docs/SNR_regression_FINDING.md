# Why the biomass SNR halved: it is the re-run, and it is an artefact

Answer to `docs/SNR_INVESTIGATION_BRIEF.md`. Run 2026-08-02 from the repository
root, mizer 3.1.0 / therMizer 1.0.0 / R 4.6.0.

## Headline

The drop from **-0.920 to -0.378** is **entirely** caused by re-entering
`steady()`. Membership contributes nothing (it pushes the other way), and the
w_min correction contributes 0.03%.

The re-run is not a neutral reordering. It **halves the unexploited baleen whale
stock**, and because the community-biomass signal is essentially the absolute
biomass of whales removed by whaling, the signal halves with it. The re-steadied
ensemble also fits the observed yield **worse** in 89% of members, and lands only
15% of the observed baleen whale catch against the stored ensemble's 69%.

**Recommendation: the manuscript should report ≈ -0.94, not -0.378.** A refined
ensemble that fixes the duplicates *without* re-entering `steady()` gives
**-0.9393** (deduplicated 200) or **-0.9432** (deduplicated + clean-set 184),
both within 2.5% of the published -0.9204. The published figure is robust to the
deduplication; it is not robust to the re-run, and it should not be.

## 1. The decomposition

Canonical SNR (`noise` = temporal SD 1841-2010 of the across-member mean
unexploited trajectory; `signal` = per-year median of exploited - unexploited).
Both published numbers reproduce exactly, so the machinery is verified.

| # | ensemble | n | SNR 2010 | signal 2010 | unexp. drift |
|---|---|---|---|---|---|
| 1 | OLD cache, all 212 **[published]** | 212 | **-0.9204** | -1.5728e12 | +2.65% |
| 2 | OLD cache, shared subset | 127 | -1.1578 | -2.0178e12 | +2.80% |
| 3 | NEW **control**, shared subset | 127 | -0.3652 | -5.9930e11 | +4.27% |
| 4 | NEW treated, shared subset | 127 | -0.3654 | -5.9914e11 | +4.33% |
| 5 | NEW control, all 183 | 183 | -0.3806 | -6.2666e11 | +4.24% |
| 6 | NEW treated, all 183 **[refined]** | 183 | **-0.3779** | -6.2172e11 | +4.29% |

Membership: 212 old, 183 new, **127 shared**, 85 old-only, 56 new-only.

Attribution, with membership held fixed at the shared 127 for the pipeline steps:

| step | ΔSNR | signal ratio |
|---|---|---|
| membership 212 → shared 127 | **-0.2374** | ×1.283 |
| **re-run (steady + spin-up)** | **+0.7926** | **×0.297** |
| w_min correction | **-0.0002** | ×1.000 |
| membership shared 127 → 183 | -0.0125 | ×1.038 |
| **total** | **+0.5425** | ×0.395 |

Two things to read off this:

- **The correction is nil**, as Stage C predicted — 1.6e8 out of 6e11, 0.026%.
  Confirmed rather than assumed, using both arms through the identical pipeline.
- **Membership is not merely innocent, it works the other way.** Restricting the
  old cache to the shared 127 makes the SNR *more* negative (-0.92 → -1.16). The
  refinement partially masks the re-run rather than causing it.

## 2. The mechanism: the re-run halves the whale stock

Not the drift. The brief's hypothesis (Q2) was that both arms drift and their
difference shrinks. That is not what happens — the difference is visible at
**1841, t = 0, before any drift**:

| median unexploited baleen whale biomass (g), shared 127 | 1841 | 2010 |
|---|---|---|
| OLD cache | 3.0173e12 | 2.4689e12 |
| NEW control | 1.4427e12 | 1.2017e12 |
| NEW treated | 1.4036e12 | 1.1703e12 |

The re-steadied members **start** at less than half the whale biomass. Drift is a
symptom of landing on a different attractor, not the cause. Drift is also
common-mode and cancels in the exploited - unexploited difference: the *relative*
depletion of baleen whales is essentially unchanged (-68.1% old vs -66.5% new).
The **fraction** removed is the same; the **absolute amount** halves.

Species decomposition of the change in the 2010 signal (shared 127) confirms it —
baleen whales and krill are 91% of the shift:

| species | signal old | signal new | Δ |
|---|---|---|---|
| baleen whales | -1.286e12 | -5.608e11 | **+7.256e11** (51%) |
| antarctic krill | -8.477e11 | -2.742e11 | **+5.735e11** (40%) |
| bathypelagic fishes | +2.314e11 | +8.312e10 | -1.483e11 |
| mesopelagic fishes | +1.906e11 | +6.680e10 | -1.238e11 |
| shelf and coastal fishes | +1.836e11 | +6.329e10 | -1.203e11 |

The positive entries are the trophic cascade (release from predation). It weakens
in step, consistent with a single upstream cause rather than several.

The shift is broad, not a handful of bistable members: **125 of 127** members
have a smaller \|signal\|, and excluding the flagged bistable members (446, 1512,
both in the shared set) moves the median signal only from -5.99e11 to -5.52e11.
The two-test protocol of `38_diagnose_member_446.R` is therefore not needed —
there is no large per-member difference left to attribute.

### Why `steady()` moves the state at all

`R/wmin_test/40_vm_project_survivors.R:192` calls

```r
steady(p, tol = 0.002, t_max = 1200, preserve = c("erepro"))
```

`preserve = "erepro"` holds `erepro` and lets `steady()` re-derive **`R_max`**.
Measured over 40 shared members × 19 species:

- `erepro` preserved exactly (verified, all rows).
- **`R_max` changed in 760 of 760 species-member rows** — 100%.
- mean reproduction level rose 0.8446 → 0.8588, i.e. systematically *more*
  density-dependent compensation, concentrated in mesopelagic (+0.088),
  bathypelagic (+0.069), mesozooplankton (+0.029).

The stored states were never at the steady state `steady()` targets: the spin-up
runs under time-varying therMizer forcing, so the post-spin-up state has already
moved off it. Re-entering `steady()` from *that* state is a second, uncalibrated
optimisation, and it lands somewhere else.

## 3. The re-run is an artefact, not an improvement

The ensemble is *selected* by yield RMSE, so the yield fit is the objective test.
Across all 1,831 clean members, paired per member (stored vs re-steadied control,
same parameter sets):

| | median RMSE | mean RMSE |
|---|---|---|
| stored | **1.8825** | 1.9177 |
| re-steadied control | 1.9079 | 1.9429 |
| re-steadied treated | 1.9080 | 1.9438 |

- **1,629 of 1,831 (89%) fit worse** after re-steadying.
- paired Wilcoxon p ≈ 0, median paired Δ = **+0.0137** — directional, not noise.
- best member degrades 1.5389 → 1.5542 (control), → 1.5892 (treated).

**The damage is concentrated in the best-fitting members** — precisely the ones
the fitted ensemble is made of:

| stored-ranking subset | n | Spearman rho | median RMSE shift |
|---|---|---|---|
| **top 200** | 200 | **0.562** | **+0.0413 (+2.43%)** |
| top 300 | 300 | 0.692 | +0.0421 (+2.48%) |
| top 500 | 500 | 0.750 | +0.0369 (+2.12%) |
| top 1000 | 1000 | 0.851 | +0.0200 (+1.10%) |
| all clean | 1831 | 0.951 | +0.0137 (+0.68%) |

This reconciles the brief's **rho = 0.599**: Stage D2
(`36_stageD2_rmse.rds`, 200 rows) measured it on the top 200, where the RMSE
range is narrow — range restriction, not a different effect. Its +2.55% median
shift matches the +2.43% above. Over the full clean set the rank correlation is
0.951, but that is the less alarming statistic and the less relevant one: the
members that actually enter the figure degrade ~3.5× more than the ensemble
average, and it is the **level** shift, not the reordering, that breaks the SNR.

Modelled vs observed total yield makes the damage concrete:

| species | observed | stored / obs | re-steadied / obs |
|---|---|---|---|
| baleen whales | 4.48e12 g | **0.69** | **0.15** |
| sperm whales | 3.62e11 g | 3.01 | 0.098 |
| minke whales | 1.92e11 g | 2.05 | 0.138 |
| antarctic krill | 9.86e10 g | 10.7 | 6.42 |

The re-steadied model cannot land the observed whale catch because the whales are
not there. (Caveat: the stored column is a median-of-year-medians over the old
212 and the re-steadied column a median of member totals over the shared 127 —
different estimators, so read the direction and the order of magnitude, not the
third digit. The paired RMSE test above is the rigorous version and agrees.)

## 4. Which number the manuscript should report

**≈ -0.94.** The deduplication defect is real and worth fixing; the re-run is not
part of fixing it. Rebuilding the fitted set from the **stored** ranking — no
`steady()` re-entered — gives:

| ensemble | n | biomass SNR 2010 | slope SNR 2010 |
|---|---|---|---|
| published (212, contains 23 duplicates) | 212 | -0.9204 | -10.404 |
| **rebuild A — deduplicated top 10%** | **200** | **-0.9393** | -10.011 |
| **rebuild B — deduplicated + clean-set top 10%** | **184** | **-0.9432** | -9.753 |
| re-steadied refined | 183 | -0.3779 | -10.238 |

Either rebuild is defensible and both are within 2.5% of the published value, so
**the published -0.920 survives the deduplication and the headline claim does not
change.** Prefer rebuild A: its membership rule (`ceiling(1997 × 0.10) = 200`) is
the documented one and it does not import a filter defined on the treated arm.

Rebuild B applies the clean-set filter (`trt_ok, trt_pass, trt_n_erepro_ge1 == 0,
trt_n_rmax_inf == 0, !repro_infeasible`), which is defined on the *treated
re-steadied* params. Checking the same conditions on the **stored** params shows
why that filter should not be carried across:

| set (stored params) | n | members with erepro ≥ 1 | members with any R_max = Inf |
|---|---|---|---|
| published 212 | 212 | **0** | 142 (67%) |
| rebuild A, deduped 200 | 200 | **0** | 148 (74%) |
| rebuild B, 184 | 184 | **0** | 136 (74%) |

`erepro ≥ 1` **never occurs** in the stored ensemble (max 0.951), and
`R_max = Inf` is pervasive and normal — two thirds of the *published* 212 have
it, typically in a single group (162 of 3,800 species-member rows, 4.3%). The
"166 members with a degenerate reproduction parameterisation" removed in the
1,997 → 1,831 step is therefore a property of the **re-steadied** arm, not a
defect in the stored ensemble. Carrying that filter into a stored-pipeline
rebuild would change what the ensemble *is*, not clean it. Rebuild A avoids the
question entirely; both give the same answer, so nothing here is load-bearing for
the headline.

Also note `ceiling(1831 × 0.10) = 184`, not 183. The 183 in
`F00_build_refined183_data.R` is a manual instruction and departs from the
ceiling rule used everywhere else. If the methods describe the top 10% as a
ceiling, 184 is the number.

## 5. Q3 — why the slope survives

Because the slope responds to **proportional** depletion and community biomass to
**absolute** depletion. The re-run preserves the former and halves the latter.

Baleen whale relative depletion at 2010: -68.1% old, -66.5% new — a 2% change.
Slope signal on the shared 127: -1.3707e-2 old, -1.3180e-2 control — a 3.8%
change. Biomass signal over the same members and pipeline: -70%.

The slope is a log-log regression across octave bins, so a uniform rescaling of
any group's abundance shifts the intercept, not the gradient. Halving the whale
stock moves the top of the spectrum down but leaves the *tilt* whaling imposes on
it almost untouched. Community biomass has no such invariance: it is a sum in
grams, dominated at the margin by whichever fished group has the largest absolute
standing stock — here baleen whales, which supply the majority of the signal
despite being ~2.5% of community biomass, because they are depleted by two-thirds
while everything else moves a few percent.

## 6. Can the fitted ensemble be rebuilt without re-entering `steady()`?

**For the uncorrected model, yes — it is already done, in this document.** The
stored ensembles hold the calibrated trajectories; the refined set is just
dedupe → filter → cut. Only 11 (rebuild A) or 16 (rebuild B) members are not
already in the cached 212 and had to be extracted from
`mc_ensemble_2111_cleaned.rds` / `climate_only_ensemble_compiled.rds`; that
extraction was verified **bit-exact** against the existing cache (max relative
difference **0** over 6,460 member-year-species biomass rows). Cost: ~1 minute
plus a 1-minute load of each 1.9 GB ensemble.

One caveat on the slope column only: the newly extracted members use F00's
`lbnbiom_slope_series`, the cached ones use `run_nbss_slope_top10pct.R`. The two
disagree by up to 0.16% on the slope *level* (a binning-edge detail), but the
disagreement is common-mode and cancels in the exploited − unexploited
difference: max discrepancy in the signal is **9.0e-07** against a signal of
-1.3e-02, i.e. 0.06% of the noise. The biomass column has no such caveat.

**For the corrected-w_min arm, no**, and this is not fixable: changing `w_min`
changes the steady state, so the treated arm must re-enter `steady()` and
therefore inherits this artefact. That was already known
(`R/wmin_test/40_vm_project_survivors.R:12-14`).

**This does not block anything**, because the correction changes the biomass
signal by 0.026% (×1.000 in the table above) and the slope signal by ~1%. The
w_min correction and the SNR figure are effectively independent. So:

- Use the **stored, uncorrected, deduplicated** ensemble for Figure 2.
- Report the w_min correction where it matters (the penguin group itself) and
  state, with the ×1.000 measurement above, that it does not affect the
  community-level results.

Recommended follow-up, in order:

1. Rebuild Figures 2-4 from rebuild A (deduplicated 200, stored pipeline). Only
   Figure 2 has been checked here; Figure 4's krill ratio and the Figure 3
   spectra should be re-derived on the same membership before anything is
   published.
2. Do **not** discard the re-steadied states. They are the correct basis for the
   catchability work (`catch-fit-reprojection-exact`), where re-projection from a
   stored state is bit-exact and this trap does not apply.
3. If a re-steadied ensemble is ever wanted for its own sake, re-fit catchability
   afterwards — the 4.5× loss of whale catch is exactly what a catchability
   re-fit would absorb.

## Reproducing this

| what | where |
|---|---|
| control-arm projection (new) | `Manuscript scripts/F00b_build_arm_data.R`, `F0_ARM=control` |
| control-arm data (new) | `Manuscript data/{biomass_abund_fish,biomass_abund_clim,nbss_slope,meta}_control183.rds` |
| memberships, per-member signals, repro comparison | `Output_large_files/wmin_test/SNR_*.rds` |
| run logs | `Output_large_files/wmin_test/F00b_{control,treated}.log` |

`F00b_build_arm_data.R` is a stripped-down `F00_build_refined183_data.R`: arm is
a parameter, diet/spectra extraction dropped, projection and slope routine
transcribed unchanged.

**It was validated by running its treated arm over all 183 members and comparing
against F00's own output: max relative difference in biomass = 0 over 1,182,180
rows, max absolute difference in slope = 0 over 62,220 rows.** Identical, not
merely close — so the control-arm numbers differ from F00 only by the arm, which
is the entire point of the experiment.

Nothing existing was overwritten; every output carries a new suffix and the
collect step is guarded. Total compute: ~7 min per arm on 14 cores (one core pair
left free), plus ~2 min for the stored-ensemble extraction.
