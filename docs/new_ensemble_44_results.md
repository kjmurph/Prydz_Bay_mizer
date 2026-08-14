# The rebuilt ensemble: full-run results

`R/wmin_test/44_rebuild_from_base.R`, 1,848 draw-distinct members, treated arm,
run on the VM 2026-08-03. mizer 3.1.0 / therMizer 1.0.0 / Ubuntu 22.04, 30 cores.

Pipeline: dedupe genuine parameter repeats (1,997 → 1,848) → corrected `small
divers` w_min → base recalibrated to 2001–2010 observed biomass (unfished, per
`06:142-155`) → `steady(tol = 0.0025, t_max = 1500, preserve = "erepro")` → 118 yr
unfished spin-up → convergence + stability rejection → 1841–2010 projection.

## Acceptance

| | |
|---|---|
| members run | 1,848 |
| **accepted** | **1,668 (90.3%)** |
| rejected | 180, all `steady_no_converge` |
| stable among accepted | 1,654 |
| top 10% of accepted | `ceiling(1668 × 0.10)` = **167** |

The 180 rejections matter beyond the count: **the non-convergence guard had never
fired in any previous script** (they listened for `warning()`; mizer signals with
`message()`). This is the first run in which non-converged members were actually
excluded.

## Agreement with the published ensemble

Spearman rho against the stored ranking is **0.941** over all 1,668 — against
0.599 for the re-steady pipeline of scripts 39/40. The unweighted top 167 retains
**119 of the published 212**. So the rebuild reorders the ensemble far less than
re-steadying did, and its fitted subset is substantially the same membership.

Best yield RMSE **1.5610** (member 1819) against the stored best of 1.53887.

## Whale catch — the ensemble does contain members that fit

| modelled/observed baleen catch | members | % |
|---|---|---|
| ≥ 0.10 | 133 | 8.0% |
| ≥ 0.30 | 31 | 1.9% |
| ≥ 0.50 | 17 | 1.0% |
| ≥ 0.69 (the published level) | 14 | 0.8% |
| ≥ 1.00 | 11 | 0.7% |

The random-60 pilot topped out at 0.099 and made this look impossible; it was a
sampling artefact. 1841 whale stock reaches the published top-212 median (2.53e12)
in 42 members (2.5%), and the top-ranked members now carry real stocks — 1819 sits
at 1.45e13 with a catch ratio of 0.409.

*Caveat on the 0.69 comparison:* the published figure is a median-of-year-medians
over the old 212, whereas these are per-member totals. Read the order of
magnitude, not the third digit.

## Selection rules compared

All on the accepted 1,668; top 10% = 167 except where the gate binds.

| rule | n | median catch ratio | % ≥ 0.3 | median 1841 stock | shared with unweighted | shared with published 212 |
|---|---|---|---|---|---|---|
| **log10 RMSE (current)** | 167 | 0.038 | 6.6% | 2.75e11 | — | **119** |
| baleen ×2 | 167 | 0.058 | 10.8% | 4.87e11 | 132 | 105 |
| baleen ×5 | 167 | 0.079 | 10.8% | 8.61e11 | 93 | 77 |
| baleen ×10 | 167 | 0.097 | 12.0% | 1.22e12 | 64 | 51 |
| baleen ×20 | 167 | 0.099 | 12.6% | 1.28e12 | 52 | 40 |
| **gate ≥ 0.1, then RMSE** | **133** | **0.179** | 23.3% | 1.27e12 | 33 | 26 |
| gate ≥ 0.3, then RMSE | 31 | **0.544** | **100%** | 3.68e12 | 11 | 9 |

Reading this:

- **Weighting works but churns membership.** ×10 gives 2.6× the whale fit of the
  unweighted rule, but only 38% of its members survive and the overlap with the
  published 212 falls from 119 to 51. Returns flatten after ×10.
- **Only a gate delivers whale realism.** At ≥0.3 every member fits the whales
  (median 0.544, stock 3.68e12 vs the published 2.53e12) — but n = 31.
- **The ≥0.1 gate is the practical middle.** n = 133, close to a 10% cut in size,
  with 4.7× the whale fit of the unweighted rule.

The trade-off is real: 167 members with poor whale behaviour, or 31 with
defensible whale behaviour, or 133 in between.

## The catchability re-tune (`R/wmin_test/45_catchability_refit.R`)

Global per-species, by the documented `08:449-511` ratio method — divide
catchability by the modelled/observed ratio, re-project, iterate — applied to the
1841–2010 projection yield rather than a steady-state yield, because the base
params carry `yield_observed = 0` (zeroed at `06:152`).

One multiplier `M_s` per species is applied to **each member's own drawn
catchability**, `q_new[i,s] = min(qmax, q_drawn[i,s] * M_s)`, so the Monte Carlo
spread is preserved and only its centre moves. Fitting q per member would maximise
the fit but turn catchability from a sampled uncertainty into an outcome.

**Catchability is not bounded at 1.** The `[0, 1]` clamp is an author's choice at
`09:347,351`, not a mizer constraint — q is a rate in `F = q · effort ·
selectivity`, not a probability. `REFIT_QMAX` controls the ceiling, and raising it
is the clean way to separate two explanations of the poor whale catch: if the catch
keeps rising past q = 1 the cap was binding, if it saturates the **stock** is the
limit and no catchability can fix it. Fits were run at both `qmax = 1` and
`qmax = 10`.

### Result: six species fit, the three whale groups do not

150 members, 6 iterations, both ceilings. Modelled/observed ratio at convergence
(1.000 = the catch is reproduced):

| species | `q ≤ 1` | `q ≤ 10` | gain | final multiplier at `q ≤ 1` |
|---|---|---|---|---|
| antarctic krill | **1.000** | 1.000 | — | 0.190 |
| bathypelagic fishes | **1.000** | 1.000 | — | 2.52e-7 |
| orca | **1.000** | 1.000 | — | 1.076 |
| shelf and coastal fishes | **1.000** | 1.000 | — | 17.18 |
| squids | **1.000** | 1.000 | — | 0.157 |
| toothfishes | **1.000** | 1.000 | — | 0.0155 |
| minke whales | 0.163 | 0.358 | 2.20× | 1.21e4 |
| **baleen whales** | **0.061** | **0.144** | 2.35× | **2.96e6** |
| sperm whales | 0.057 | 0.097 | 1.70× | 1.74e6 |

Median yield RMSE by iteration:

| ceiling | trajectory |
|---|---|
| `q ≤ 1` | 1.9601 → 1.8969 → 1.8894 → 1.8789 → **1.8751** |
| `q ≤ 10` | 1.9601 → 2.1596 → 2.2218 → 2.2366 → **2.2369** |

**The stock is the limit, not the ceiling.** Three independent signatures:

1. A **10× increase in the ceiling buys only ~2.2×** of catch — catch ∝ q^0.35 in
   this regime, i.e. heavily saturated.
2. **It makes the overall fit worse.** `q ≤ 1` improves the ensemble
   (1.9601 → 1.8751); `q ≤ 10` degrades it (1.9601 → 2.2369). Forcing whale
   catchability past 1 buys a little total catch and loses the time-series fit.
3. **The whale multipliers diverge** — baleen to 2.96e6, sperm to 1.74e6. A
   parameter running away against a flat objective is a non-identifiable fit.

This is the same conclusion the calibration arithmetic gives from the other
direction: the observed baleen catch is **23.8× the calibrated standing stock**, so
no catchability can extract it.

**A side-effect that must not be adopted silently.** Multipliers of that size pin
every member's whale catchability at the ceiling, which **collapses the Monte Carlo
spread in whale catchability to a single point** — destroying an explored
uncertainty, the opposite of what the global-multiplier design was chosen to
preserve. The six converging species should take their fitted multipliers; the
three whale groups should not take theirs.

*Implementation note:* `parLapply` serialises the worker function but **not** the
globals it references. Passing the multiplier vector via a closure fails on every
worker with `object 'M' not found`, and if those failures are swallowed to `NULL`
it looks like an empty result rather than an error. Pass it as an argument.

## Why the re-tune was needed

Deliberately deferred to after the run, and it should change the table above. The
base baleen catchability of 0.05 was hand-set at `09:1511` against whale stocks
~13× larger than the recalibrated model's, so it is stale. A sweep on member 1339
reached a catch ratio of **0.72 at catchability 0.25** versus 0.094 as drawn.

Re-tuning is cheap and exact: the spin-up is unfished, so catchability enters only
the projection, and all 1,668 post-spin-up states are cached in
`Output_large_files/wmin_test/44_states/` (333 MB). Re-projecting the whole
ensemble once is ~20 min on 14 cores.

If the re-tune lifts typical catch ratios, the ≥0.3 gate stops being restrictive
and the selection question largely dissolves — which is why the gate should not be
fixed before it is done.

## The selection rule decides the headline number

`R/wmin_test/46_selection_cuts.R`, on the re-tuned ensemble (1,668 members, top
10% = 167). SNR is the canonical one — numerator `median_i(E_i − U_i)`, denominator
the SD across years 1841–2010 of the across-member mean unexploited trajectory.

| cut | n | median RMSE | median whale catch | % ≥ 0.3 | **biomass SNR 2010** | signal 2010 | shared with A |
|---|---|---|---|---|---|---|---|
| **A** unweighted RMSE | 167 | 1.6465 | 0.041 | 7.2% | **−0.147** | −2.12e11 | 167 |
| **B** RMSE, baleen ×10 | 167 | 1.6887 | 0.094 | 12.6% | **−0.271** | −3.91e11 | 70 |
| **C** gate ≥0.10, then RMSE | 133 | 1.7517 | 0.179 | 23.3% | **−0.428** | −6.18e11 | 32 |
| **D** gate ≥0.30, then RMSE | 31 | 1.7210 | 0.544 | 100% | **−1.338** | −2.10e12 | 12 |

Published: **−0.920** (212 members, stored pipeline). Stored-ranking rebuilds:
−0.9393 (deduped 200), −0.9432 (clean 184).

**The SNR is a readout of the whale catch.** It grows monotonically with how much
whale realism the cut demands — −0.147 → −0.271 → −0.428 → −1.338, a **9× range**
driven purely by the selection rule. For comparison, the w_min correction moves the
signal by ×1.000 and the re-steady artefact by ~2.4×. Nothing else examined in this
project comes close.

This is the same mechanism `docs/SNR_regression_FINDING.md` identified — the
community-biomass signal is essentially the absolute biomass of whales removed by
whaling — but seen from the selection side rather than the pipeline side.

**Consequences, and they are load-bearing:**

- The published rule (A), applied to this ensemble, gives **−0.147** — effectively
  no emergent signal. The published −0.920 depended on the old ensemble's inflated
  whale stocks, which arrived by basin selection rather than by the draw.
- Only a whale-screened cut recovers a signal of the published magnitude, and the
  published value sits **between the 0.10 and 0.30 gates**. Cut D overshoots at
  −1.338, cut C undershoots at −0.428.
- So whale realism and the whaling signal are the same quantity. A fitted ensemble
  that does not reproduce the whale catch cannot show the whaling signal, whatever
  else it fits well.

The selection rule can no longer be treated as a presentational choice; it has to
be argued for on its own terms and stated explicitly in the methods.

### What each cut costs elsewhere

Modelled/observed total catch, **median across members** (a mean is useless here —
the per-member totals are heavy-tailed, and on cut D the mean puts toothfishes at
75.6× while the median is 0.39):

| species | A | B | C | D |
|---|---|---|---|---|
| antarctic krill | 1.47 | 0.91 | 0.65 | 0.30 |
| bathypelagic fishes | 0.94 | 0.67 | 0.66 | 0.29 |
| shelf and coastal fishes | 0.43 | 0.86 | 1.03 | 0.42 |
| squids | 0.74 | 0.80 | 0.96 | 0.74 |
| toothfishes | 0.76 | 0.80 | 0.77 | 0.39 |
| orca | 1.12 | 1.36 | 1.28 | 1.18 |
| minke whales | 0.135 | 0.135 | 0.113 | 0.141 |
| sperm whales | 0.083 | 0.055 | 0.052 | 0.058 |
| **baleen whales** | 0.041 | 0.094 | 0.179 | **0.544** |
| **species within 2× of observed** | **5/9** | **6/9** | **6/9** | **3/9** |

So B and C fit the ensemble as a whole best; D buys its whale fit by degrading
every other group. **Minke and sperm whales are poorly fitted under every cut**
(0.05–0.18) — they are stock-limited in the same way baleen whales are, and holding
their catchability at the drawn values does not rescue them.

Figures, two per cut, in `Manuscript figures/Supplemental figures/`:
`yield_stacked_<cut>.png` (stacked area, observed inset, IQR bars at peaks) and
`yield_facets_<cut>.png` (per-species, pseudo-log, ribbons, observed points).

## Alternative yield objectives, scored offline

`R/wmin_test/49_objective_variants.R`, on the 320 union members, from cached
per-species per-year yields — no re-projection. Top 10% of the union = 32.

| objective | rho vs current | rho vs baleen catch | top-32 overlap | median baleen catch | **SNR 2010** |
|---|---|---|---|---|---|
| **0** current, pooled log10(x+1) | 1.000 | +0.517 | 32 | 0.053 | −0.192 |
| **1** per-species offset log10(x+c_s) | 0.784 | +0.216 | 18 | 0.089 | **−0.389** |
| **2** effort-on rows only | 0.784 | +0.216 | 18 | 0.089 | **−0.389** |
| **3** total-catch ratio per species | 0.307 | +0.040 | 3 | **0.155** | −0.358 |
| **4** magnitude + shape | 0.358 | +0.071 | 5 | 0.097 | −0.297 |
| **5** floor-normalised skill | 0.755 | +0.526 | 14 | 0.043 | −0.151 |
| **6** six representable groups | 0.786 | +0.312 | 14 | 0.054 | −0.187 |

### 1 and 2 are the same ranking — provably

Zero of the 344 rows have effort off with a non-zero observed catch, and modelled
yield is **exactly 0 in all 27,840 effort-off rows**. So every effort-off row
contributes exactly 0 to the SSE, and the two variants share one SSE differing
only by a constant divisor (344 vs n_on). They cannot reorder relative to each
other. Treat them as one option.

### The dilution is real and unequal

**87 of 344 rows (25%) are structurally zero** — both modelled and observed 0.
By species: baleen whales **40 of 81 (49%)**, squids 18 of 31 (58%), minke 8 of 48,
toothfishes 7 of 40. The pooled RMSE divides by 344 when only 257 rows can carry
error, and it halves baleen whales' share of the metric.

### The floor table inverts the earlier diagnosis

Median member RMSE against each species' structural floor (`07_error_floor.R`):

| species | median RMSE | floor | % of floor |
|---|---|---|---|
| **sperm whales** | 1.637 | 0.419 | **391%** |
| toothfishes | 2.885 | 2.203 | 131% |
| baleen whales | 0.867 | 0.849 | 102% |
| minke whales | 0.811 | 0.837 | 97% |
| shelf and coastal fishes | 2.362 | 2.518 | 94% |
| squids | 1.154 | 1.533 | 75% |
| orca | 0.533 | 0.728 | 73% |
| antarctic krill | 0.604 | 1.119 | 54% |
| bathypelagic fishes | 0.234 | 1.159 | 20% |

`docs/catch_fit_RESULTS.md` §6 found shelf-and-coastal and toothfishes carrying
~75% of the error, structurally. **That no longer holds on this ensemble.** After
the catchability re-fit, shelf-and-coastal sits *below* its constant-catch
reference (94%) and six of nine species are at or below their floor. The residual
error has moved to the whales.

**Sperm whales carry much the largest excess over floor (391%) — but it is NOT
remediable by catchability.** Their observed series is the smoothest of any group
(2.3 decades of variation, the least), which is why their floor is the lowest at
0.419. The excess is a pure *level* deficit: the model lands ~3.9% of observed
catch, so every residual is about log10(0.039) = −1.4, and an RMSE of 1.637 is
essentially that constant offset. The global fit already established the level
cannot be raised — sperm whales went 0.052 → 0.097 even at a **10×** q ceiling,
with the multiplier diverging to 1.7e6. So this is the same stock limitation as
baleen and minke ([[whale-catch-is-stock-limited]]), made stark by a low floor
rather than a new opportunity.

The practical reading: **there is very little catchability-recoverable error left
anywhere in this objective.** Six species are at or below their floor, and the
three above it are the three whale groups, all stock-limited.

### Caveat that limits the correlation column

**The 320 union is not a random sample.** Two of the four cuts are gated on baleen
catch, so it over-represents high-baleen members, and conditioning on it induces
the positive `rho vs baleen` seen here — the same quantity measured on a random 53
was **−0.153**, the opposite sign. Read that column as "within this selected set",
not as an unbiased property. The SNR and overlap columns are unaffected.

**None of variants 1–6 selects for whale realism.** Explicit weighting (baleen ×10,
rho −0.598) or an explicit gate remain the only mechanisms that do.

## Files

| path | |
|---|---|
| `Output_large_files/wmin_test/44_rebuild_results.rds` | summary + per-species yield |
| `Output_large_files/wmin_test/44_rebuild_summary.csv` | per-member summary |
| `Output_large_files/wmin_test/44_results/` | 37 chunk files |
| `Output_large_files/wmin_test/44_states/` | 1,668 post-steady params + post-spin-up abundances |
| `Output_large_files/wmin_test/44_selection_rules.rds` | the memberships in the table above |
