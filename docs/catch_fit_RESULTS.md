# Systematic catch fitting — results

Improving modelled against observed catch for the best-fitting ensemble members by
adjusting **catchability** and then **reproduction** deliberately, rather than by
random sampling. Objective, members, constraints and method are as briefed; the
one change of scope is noted in §1.

Last updated 2026-07-31. Scripts: `R/catch_fit/`. Outputs:
`Output_large_files/catch_fit*/`. Nothing pre-existing was overwritten.

---

## 0. Headline

1. **Systematic search beats the entire Monte Carlo campaign, from every starting
   member.** The best of 1,997 random draws is RMSE **1.5389**. Optimising
   catchability brings all seven members to **1.399–1.507** — *every one of them
   below the best random draw*, and four to within 0.02 of each other.
2. **The premise about which groups fit worst is inverted on the stated
   objective.** On the log10(g+1) scale, toothfishes and shelf-and-coastal fishes
   carry **75.4%** of the error; sperm + minke + baleen carry **10.3%**.
3. **Sperm whales are solved by catchability, in the opposite direction to the
   hypothesis.** The model *over*-catches them; lowering `q` by ~1 dex takes
   modelled/observed from 3.2 to 1.00 and their RMSE from 0.78 to 0.27.
4. **Minke whales are the real stock-limited group — unanimously, 7 of 7
   members.** No catchability reaches the observed catch: the most any admissible
   `q` delivers is **11–26%** of it, and even at `q = 100` (100× outside the
   admissible box) only **39%**, while the population collapses to **1.1%** of its
   1841 biomass.
5. **The minke gap is closable by reproduction, and it is admissible — but the
   price is a 5–8× larger whale population.** `R_max × 5` at fixed `erepro`
   matches the observed catch (modelled/observed 0.99) with `erepro` untouched,
   `R_max` finite at ~350, and `erepro < 1` for all 19 groups in all 531
   configurations. What it also does is grow the *unfished* minke population
   **4.7–8.2×** over 1841–2010. The binding constraint is neither `erepro` nor
   `R_max` — it is that the model would no longer be at its calibrated abundance.
6. **Toothfishes and shelf-and-coastal fishes cannot be fixed by any scalar
   catchability**, and the reason is structural, not parametric (§6).
7. **The `small divers` w_min correction does not move the optimum.** Measured
   properly as C − B against a paired re-run control: median effect **5.7e-06**,
   against **0.0232** for the re-run alone — a factor of **4,072** (§8).
8. Two defects in the existing pipeline surfaced on the way (§9). One of them —
   a dead non-convergence guard — is a code-correctness issue rather than a
   problem with any state already computed.

---

## 1. Scope and what was actually run

| | |
|---|---|
| objective | the existing one, unchanged: RMSE of log10(g+1) modelled vs observed yield, per species within its own effort window, **344 rows** (`yield_rmse_evaluation.R:112-140`) |
| members | **7** — see below |
| catchability box | `q ∈ (0, 1]`, the pipeline's clamp (`09_Uncertainty_Analysis.Rmd:351`). `q > 1` probed only as a labelled diagnostic |
| w_min | uncorrected (arm A) is the headline; corrected arm built and compared (§8) |
| evaluations | 1,318 response-surface + ~1,500 optimiser + 531 reproduction |

**Members.** The brief specifies the top 5 of the agreed filter from
`39_full_ensemble_paired.R`. That filter measures `erepro`/`R_max` on
`out@species_params` — the state **after** `steady(preserve="erepro")`, which ends
by calling `setBevertonHolt()` and re-derives `R_max`. `steady()` removes most
inherited infinities, so a member can pass the filter while its **stored** params
carry `R_max = Inf`. Members 1776 (mesozooplankton) and 173 (other
macrozooplankton) both do.

Rather than choose between the two readings, both were run — the union is 7
members and the extra cost was ~40 minutes:

| set | members (deduped ranks) |
|---|---|
| **PRIMARY** — top 5 of the 39-run filter, as briefed | 446, 1512, 1776, 173, 71 (1, 2, 5, 6, 8) |
| **STRICT** — also clean in the stored params | 446, 1512, 71, 1806, 384 (1, 2, 8, 9, 16) |

Every conclusion below holds on both sets. The inherited `R_max = Inf` groups are
unfished low-trophic ones — the pre-existing degeneracy in
`small_divers_wmin_SYNTHESIS.md` §2 — and the brief's rule is "do not add to it",
which was enforced: no configuration anywhere in this work introduced a new one.

**Scope change.** The brief asked for the top 5 and the deliverable for the top 10;
the instruction during the run was 5 members on 1 core. Seven were run because
cores freed up and the member-selection question above needed both sets. Extending
to more members is a longer member list, not new code.

---

## 2. The method, and why it is cheap and exact

Five facts, each verified before any search ran:

1. `initial_effort` is **0 for all 19 gears**, so `steady()` and the spin-up are
   *unfished* — **catchability cannot affect the initial condition.** It enters
   only the 1841–2010 projection.
2. `sim@params@initial_n` is **bit-identical** to `sim@n[1,,]`.
3. Re-projecting a stored member reproduces its stored yield to **4.6e-15**
   relative (3 of 7 members to exactly 0).
4. A **null treatment** through `gear_params(p) <- g` is equally exact, and that
   path preserves `@rates_funcs` (therMizer entries intact), `@initial_n`,
   `@resource_dynamics`, `@other_dynamics`.
5. The recomputed pooled RMSE matches `yield_rmse_per_sim_deduped.csv` to ~1e-15.

**Consequence: the "a re-run cannot reproduce the stored ensemble" finding does
not apply to catchability work at all.** That is a property of re-entering
`steady()`; nothing in Stage 1 or Stage 2a calls it. The control is bit-exact, so
absolute RMSE *is* comparable with stored values, and the known bistability of
members 446/1512 — a `steady()` convergence-path artefact — is irrelevant here.

One further saving: the earliest effort year across all gears is **1930**, so
1841–1929 is catchability-independent and is cached once per member. Verified
exact (gate 4), and it halves the cost to ~4 s per evaluation.

---

## 3. Before — where the error actually is

Per-species RMSE at each member's own catchability, and the share of the 344-row
sum of squared errors (mean over the 7 members):

| group | n | mean SSE share | RMSE (range over members) | modelled/observed |
|---|---|---|---|---|
| toothfishes | 40 | **42.8%** | 2.84 – 3.19 | 0.24 – 10.8 |
| shelf and coastal fishes | 61 | **32.5%** | 2.12 – 2.23 | 0.011 – 0.066 |
| bathypelagic fishes | 3 | 6.1% | 3.52 – 4.62 | 7.3e4 – 1.7e6 |
| squids | 31 | 5.5% | 1.02 – 1.51 | 0.36 – 5.66 |
| sperm whales | 48 | 5.4% | 0.47 – 1.61 | 0.028 – 3.82 |
| minke whales | 48 | 3.4% | 0.67 – 0.85 | 0.084 – 0.179 |
| antarctic krill | 23 | 2.5% | 0.51 – 1.28 | 0.094 – 13.9 |
| baleen whales | 81 | 1.4% | 0.31 – 0.59 | 0.12 – 1.13 |
| orca | 9 | 0.3% | 0.36 – 0.80 | 0.072 – 2.32 |

The brief states the worst-fitting groups are sperm whales, then minke and baleen.
On the **kt/yr** scale (`yield_rmse_by_group.csv`) that is correct — baleen 62.6,
sperm 44.6, minke 18.9 kt/yr. On the **log10 objective that actually drives
selection** it inverts: the three whale groups together are 10.3% of the error.
Both rankings are true of their own metric; they are not in conflict, but only the
log-scale one is what the ensemble was ranked on.

Setting all whale error to zero would move member 446 from 1.5389 to 1.471 (−4.4%).
Setting toothfishes and shelf-and-coastal to zero would move it to 0.705 (−54%).

---

## 4. Catchability — the response surface

Each of 9 gears swept over `10^seq(-3,3,0.25)` as a multiplier on the member's own
`q`, clipped to `q ≤ 1`, all other gears held fixed; curves whose optimum sat on a
grid edge were extended in 0.5-dex steps until the improvement stopped
(bathypelagic fishes needed ~6 further decades).

Medians over the 7 members. `reach` is the largest modelled/observed cumulative
catch attainable anywhere in the box; `elast` is d log(catch)/d log(q) at the top
of the scan — ~1 means catch still scales with effort, ≪1 means the population,
not catchability, is the limit.

| gear | RMSE base → best | mod/obs at base | reach | elast | verdict (of 7 members) |
|---|---|---|---|---|---|
| bathypelagic fishes | 4.33 → **0.09** | 7.3e5 | 7e8 | 1.00 | solved by q, 7/7 |
| antarctic krill | 1.02 → **0.39** | 8.0 | 71 | 0.55 | solved by q, 7/7 |
| toothfishes | 3.07 → 2.85 | 5.0 | 4e3 | 0.82 | solved 6, timing 1 |
| sperm whales | 0.83 → **0.40** | 1.09 | 1.45 | 0.10 | solved 3, pop-limited 3, partial 1 |
| squids | 1.15 → 0.94 | 0.95 | 945 | 1.00 | solved 3, partial 4 |
| orca | 0.42 → 0.36 | 0.53 | 5.2 | 0.86 | solved 3, timing 3, partial 1 |
| baleen whales | 0.36 → 0.31 | 0.46 | 1.34 | 0.27 | partial 3, timing 2, pop-limited 2 |
| shelf and coastal fishes | 2.15 → 2.12 | 0.026 | 25 | 0.99 | **timing mismatch 4, partial 3** |
| **minke whales** | 0.79 → 0.79 | 0.11 | **0.15** | 0.31 | **population-limited 7/7** |

Two rows carry the argument.

**Minke whales — unanimous, 7 of 7.** `reach` is **0.109–0.256** across members
(median 0.153): the largest cumulative catch the model can produce anywhere in
`q ∈ (0,1]` is 11–26% of what was landed, and elasticity at the top of the scan is
0.29–0.33 in every member. For member 446, raising `q` from 0.403 to the cap 1.0
(×2.5) multiplies catch by only 1.37 while taking the surviving biomass from 43%
to 25% of its 1841 value. That is the saturation the level-2 hypothesis predicts.

**Shelf and coastal fishes — the opposite failure.** `reach = 25` and
`elast = 0.99`: catch scales essentially linearly with `q`, biomass is untouched
(end/start 1.28 flat across three decades of `q`), so the observed *level* is
trivially reachable at about `q × 18`. But getting there makes the fit **worse** —
RMSE rises from 2.12 to 2.39. Nothing about the population limits this group; the
error is in *when*, not *how much*.

### 4b. Cap diagnostic — outside the admissible box

Only member 173 had a gear pinned at `q = 1` at its joint optimum (minke). Taken
unbounded, with everything else at the optimum:

| q | total RMSE | minke RMSE | minke mod/obs | worst end/start biomass |
|---|---|---|---|---|
| 1 (the cap) | 1.3990 | 0.836 | 0.102 | 0.196 (baleen) |
| 10 | 1.4099 | 0.958 | 0.210 | 0.128 (minke) |
| 31.6 | 1.4231 | 1.089 | 0.300 | 0.050 (minke) |
| **100** | 1.4501 | 1.320 | **0.391** | **0.011 (minke)** |

**Minke catch is not cap-limited. It is unattainable at any catchability.** Two
further decades of fishing mortality buy 0.10 → 0.39 of the observed catch and cost
the population 94% of what remained. Elasticity over that range is 0.29.

---

## 5. After — the joint optimum

Coordinate descent on log10(q) over the 9 gears inside `q ∈ (0,1]`, refined at
±0.5, ±0.25, ±0.125 dex, from two starts (the per-gear 1-D optima, and the
member's own catchability).

| deduped rank | member | before | after | change | worst end/start biomass |
|---|---|---|---|---|---|
| 1 | 446 | 1.5389 | **1.3990** | −9.1% | 0.391 minke |
| 2 | 1512 | 1.5500 | **1.3999** | −9.7% | 0.363 minke |
| 5 | 1776 | 1.5751 | 1.4595 | −7.3% | 0.229 sperm |
| 6 | 173 | 1.6174 | **1.3990** | −13.5% | 0.196 baleen |
| 8 | 71 | 1.6269 | **1.4049** | −13.6% | 0.275 baleen |
| 9 | 1806 | 1.6273 | 1.5070 | −7.4% | 0.206 baleen |
| 16 | 384 | 1.6383 | **1.4187** | −13.4% | 0.276 baleen |

**Every optimised member beats the best of all 1,997 random draws (1.5389).** Even
the worst of them, 1806 at 1.5070, would rank **first** in the published ranking.
The Monte Carlo did not fail to sample well — it failed to search.

It also follows that the members' *ranking* is largely an artefact of where their
random catchability draw landed. Ranks 6, 8 and 16 optimise to 1.399, 1.405 and
1.419 — indistinguishable from rank 1.

### Per-species, before → after (RMSE)

| group | 446 | 1512 | 1776 | 173 | 71 | 1806 | 384 |
|---|---|---|---|---|---|---|---|
| bathypelagic fishes | 4.23→**0.02** | 4.32→0.02 | 3.52→0.02 | 4.62→0.02 | 4.55→0.07 | 3.63→0.02 | 4.56→0.07 |
| antarctic krill | 0.78→**0.39** | 0.80→0.39 | 1.28→0.39 | 0.51→0.39 | 1.01→0.39 | 1.23→0.39 | 1.01→0.39 |
| squids | 1.02→0.93 | 1.02→0.93 | 1.05→0.94 | 1.15→0.93 | 1.51→0.93 | 1.38→0.94 | 1.51→0.94 |
| sperm whales | 0.78→**0.27** | 0.86→0.39 | 1.27→1.22 | 0.83→0.26 | 0.47→0.39 | 1.61→1.60 | 0.67→0.64 |
| orca | 0.38→0.37 | 0.38→0.36 | 0.80→0.36 | 0.63→0.39 | 0.43→0.39 | 0.36→0.36 | 0.42→0.40 |
| baleen whales | 0.36→0.31 | 0.31→0.31 | 0.59→0.29 | 0.32→0.28 | 0.39→0.37 | 0.32→0.28 | 0.40→0.38 |
| toothfishes | 3.04→2.85 | 3.07→2.85 | 2.84→2.84 | 3.19→2.85 | 3.19→2.85 | 2.92→2.85 | 3.19→2.84 |
| shelf and coastal | 2.12→2.12 | 2.12→2.12 | 2.15→2.12 | 2.23→2.12 | 2.19→2.12 | 2.14→2.12 | 2.19→2.12 |
| **minke whales** | 0.79→0.79 | 0.78→0.78 | 0.79→0.75 | 0.85→0.84 | 0.81→0.80 | 0.67→0.67 | 0.81→0.80 |

Seven members starting from seven different catchability vectors converge to
**the same per-species error**: krill 0.39 in all seven, squids 0.93–0.94,
shelf-and-coastal 2.12, toothfishes 2.84–2.85, orca 0.36–0.40. That is a property
of the model and the data, not of the member — which is the strongest available
evidence that this is a genuine optimum rather than seven local ones.

**Caveat on the two-start test.** The informed start won in all 7 cases and the
two starts end 4.0–5.4 dex apart, so the intended multimodality check is weak: the
baseline start can move each coordinate at most 0.875 dex in total, which is not
enough to reach bathypelagic fishes' optimum ~6 dex away. The convergence of seven
independent members to a common per-species optimum is the better evidence, and it
is what the claim rests on.

---

## 6. Why toothfishes and shelf-and-coastal cannot be fixed by catchability

Effort is **binary** — `effort_array_1841_2010.rds` is 0/1 per gear per year — and
catchability is one scalar per gear. So modelled catch is exactly 0 in an
effort-off year whatever `q` is, and `q × selectivity × biomass` when effort is on,
varying only as slowly as biomass does. The model has an on/off mechanism but no
graded one.

The observations are graded, by a lot. `decades` is the log10 range of non-zero
observed catch inside each window:

| group | rows | effort-on | effort-on but nothing landed | decades of variation |
|---|---|---|---|---|
| shelf and coastal fishes | 61 | 57 | 6 | **6.4** |
| antarctic krill | 23 | 23 | 0 | 5.6 |
| toothfishes | 40 | 33 | 6 | **5.0** |
| baleen whales | 81 | 41 | 0 | 4.4 |
| minke whales | 48 | 40 | 0 | 3.2 |
| sperm whales | 48 | 42 | 0 | 2.3 |

Shelf-and-coastal fishes landed a median of 56 t in its non-zero years and a
maximum of 7,376 t. No single catchability can track a series that moves over six
orders of magnitude while the modelled biomass moves by a factor of 1.3. This is
why its RMSE sits at 2.12 for every member and every `q` tried.

The fix, if one is wanted, is **time-resolved effort** for these gears — not
catchability and not reproduction. That is a change to the forcing, outside the
scope of this work, and it is where ~75% of the objective lives.

**A data caveat that matters more than its size suggests.** Bathypelagic fishes'
entire observed record inside its window is **0.69 kg**, landed in one year (1991),
across a 3-row window. It contributed 6.3% of the objective before optimisation
purely because the model was producing ~7e5 times that. Driving `q` to ~5e-10
removes that error, which is why the group shows the single largest per-species
improvement in §5. Whether a 690 g record should carry 6% of a model-selection
metric is a question for the manuscript, not for the optimiser.

---

## 7. Reproduction — can it close the minke gap?

Run from the **Stage 1b catchability optimum**, so the question is "catchability
has already been pushed as far as it goes; does reproduction buy anything more?".
531 configurations. Every one carries a **paired unfished projection** from the
same `initial_n`, so "the fit improved because the population grew" is separated
from "the fit improved because fishing matched better".

Two levers, deliberately distinguished, with `RDD = RDI/(1 + RDI/R_max)` and
`reproduction_level = RDD/R_max`:

- **Lever A — `R_max × k` at fixed `erepro`.** Raises `RDD`, so it raises both the
  recruitment ceiling and the standing stock.
- **Lever B — `reproduction_level` at fixed `RDD`.** `R_max` up, `erepro` down,
  state at `initial_n` unchanged; only the strength of compensation changes.

Both were applied surgically to one species row at a time.
`setBevertonHolt(reproduction_level = <full-length vector>)` must **not** be used:
feeding the other 18 groups their own *unchanged* levels back perturbs them ~2% and
pushes **orca from `erepro` 0.9509 to 1.0104**, breaching the hard constraint —
because `initial_n` is the post-spin-up state, not the state the stored `R_max` and
`erepro` were derived at.

### Lever B does nothing

Minke RMSE 0.79 → 0.82 / 0.84 / 0.86 as the reproduction level is lowered to
0.75 / 0.50 / 0.25 — consistently *worse*, modelled/observed stuck at 0.11–0.13.
`erepro` falls from 0.244 to 0.022, so it is entirely constraint-safe and entirely
useless. This is the expected sign: at fixed `RDD`, weakening density dependence
reduces compensation under depletion. It is reported because the brief's hypothesis
is about compensation and the sign had to be measured, not assumed.

### Lever A closes the gap — at a price

Minke whales, best configuration per member:

| member | k | `R_max` | repro level | RMSE | mod/obs | **unfished growth 1841→2010** | depletion |
|---|---|---|---|---|---|---|---|
| 446 | 5 | 354 | 0.735 | 0.79 → **0.54** | 0.12 → 0.79 | **8.20×** | 0.279 |
| 384 | 5 | 349 | 0.763 | 0.80 → 0.59 | 0.13 → 0.88 | 8.09× | 0.225 |
| 71 | 5 | 348 | 0.767 | 0.80 → 0.59 | 0.13 → 0.88 | 8.00× | 0.224 |
| 1512 | 5 | 350 | 0.763 | 0.78 → 0.54 | 0.13 → 0.83 | 7.56× | 0.269 |
| 173 | 2 | 299 | 0.487 | 0.84 → 0.64 | 0.10 → 0.25 | 5.69× | 0.367 |
| 1806 | 5 | 330 | 0.893 | 0.67 → 0.50 | 0.17 → 0.81 | 5.39× | 0.373 |
| 1776 | 5 | 282 | 0.707 | 0.75 → 0.47 | 0.13 → 0.57 | 4.75× | 0.385 |

Pushing to an exact level match (modelled/observed ≈ 1.00) needs `R_max × 5` with
`q` raised toward the cap, and lands at minke RMSE 0.56–0.66 with unfished growth
**5.4–8.2×** in six members and **29.6×** in member 173.

**Is it physically admissible?** On the stated constraints, yes, and comfortably:

| check | result |
|---|---|
| `erepro < 1` for all 19 groups | **yes** — max seen across all 531 configurations is **0.9509**, which is orca, unchanged |
| minke `erepro` | **untouched** — lever A does not move it (0.2442 in six members, 0.7172 in 1806) |
| `R_max` finite for all 19 | **yes** — minke goes ~71 → 282–355, finite |
| new `R_max = Inf` introduced | **none**, in any of the 531 configurations |
| population stable / not collapsed | fished minke ends at **22–39%** of its unfished counterpart — heavy depletion, but no collapse |

**So the honest answer is:** the observed minke catch is attainable, it does not
require anything inadmissible in `erepro` or `R_max`, and the constraint that
actually binds is one the brief did not list — **the model would need a
pre-exploitation minke population roughly 5–8× larger than the calibrated steady
state.** That is not a tuning; it is a different calibration, and whether it is
defensible is an empirical question about pre-whaling minke abundance in Prydz Bay,
not a modelling one.

**What is *not* the mechanism.** It would be natural to conclude that minke sit at
a hard recruitment ceiling. They do not. With `RDD → RDI` as `R_max → ∞`, the most
recruitment can rise is `RDI/RDD`, and minke's is **17.1×** (median over the 7
members; range 2.9–42.8). Reaching the observed catch uses `R_max × 5`, which
raises `RDD` by only ~3.9× — about a quarter of what is available. There is ample
reproductive headroom.

The problem is that **recruitment and standing stock move together**: raising
`R_max` raises `RDD`, and raising `RDD` raises the unfished population in the same
proportion. There is no setting that raises sustainable catch without also raising
pre-exploitation abundance. That is the real finding, and it is why the level-2
hypothesis is only half right — the ceiling is real, but it is a ceiling on
*abundance*, not on *recruitment*.

Among the whale groups minke is nonetheless the tightest by a wide margin
(headroom 17.1 against sperm 803, orca 900, baleen 14,040), which is why it is the
one that saturates. Across all fished groups it is fourth-tightest — antarctic
krill (2.1), bathypelagic fishes (5.6) and shelf-and-coastal fishes (9.0) have
less, though none of those is limited by recruitment in practice.

### Sperm and baleen whales

- **Sperm whales**: solved by catchability alone in 3 of 7 members (mod/obs → 1.00,
  RMSE 0.78 → 0.27). In members 1776 and 1806 the stock is too small and lever A
  fixes it — RMSE 1.22 → 0.21 and 1.60 → 0.19 — but at **33× and 111×** unfished
  growth, which is not credible.
- **Baleen whales**: already the best-fitting whale group. Lever A gives 0.37 →
  0.32 at `k = 100` with 2.3× unfished growth; `k = 2` gives essentially all the
  available gain at 0.82× growth. Nothing here needs reproduction.

---

## 8. Does the `small divers` w_min correction move the optimum?

No — by a factor of four thousand.

Three arms, each internally bit-exact, each optimised by the identical Stage 1a +
1b procedure:

| arm | starting state |
|---|---|
| **A** | the member's stored post-spin-up state, w_min uncorrected (the headline arm) |
| **B** | the same member re-run through `steady(preserve="erepro")` + spin-up, still uncorrected |
| **C** | as B, with `w_min = 3626.667` for `small divers` and the paired penguin `erepro`/`R_max` recalibration at reproduction level 0.50 |

**C is not comparable with A**, so B exists to absorb the re-run: the w_min
sensitivity of the optimum is **C − B**.

| member | A | B | C | C − B | B − A |
|---|---|---|---|---|---|
| 446 | 1.39902 | 1.42223 | 1.42223 | +7.9e-07 | +0.0232 |
| 1512 | 1.39991 | 1.40170 | 1.44477 | +4.3e-02 | +0.0018 |
| 1776 | 1.45954 | 1.46692 | 1.46979 | +2.9e-03 | +0.0074 |
| 173 | 1.39903 | 1.43204 | 1.43203 | −5.2e-06 | +0.0330 |
| 71 | 1.40489 | 1.47953 | 1.47953 | −5.7e-06 | +0.0746 |
| 1806 | 1.50703 | 1.50334 | 1.50248 | −8.6e-04 | −0.0037 |
| 384 | 1.41865 | 1.47224 | 1.47224 | −5.7e-06 | +0.0536 |

- **median \|C − B\| = 5.7e-06** — the w_min effect on the optimum
- **median \|B − A\| = 0.0232** — the re-run alone
- **ratio 4,072×**

The optimum *catchability* is likewise unmoved: the median shift is **0 dex for
every one of the 9 gears**, and in 5 of 7 members C and B land on exactly the same
vector.

Two members are exceptions worth stating rather than hiding. Member 1512 shows
C − B = +0.043 and member 1776 +0.0029, with baleen-whale catchability differing by
up to 0.56 dex. These are the coordinate descent taking a different path on a
discrete grid, not a physical w_min effect — the same members show a *smaller*
B − A than the ones that agree exactly, which is the wrong signature for a real
treatment effect. They set the practical resolution of this comparison at about
±0.04 RMSE for an individual member, which is still far above anything w_min does
in the median.

This reproduces, on a different metric, what `small_divers_wmin_SYNTHESIS.md` §5
found on the RMSE ranking (rho 0.998 for the correction against rho 0.599 for the
re-run). **Proceeding on the uncorrected members was the right call, and the choice
is now measured rather than assumed.**

Penguin recalibration held in all 7 members: `erepro` 0.186–0.914 (all < 1),
`R_max` finite, `rdd_ratio` 0.969–0.998, stability passed on the first spin-up
cycle in every arm. The achieved penguin reproduction level was **0.567–0.711**
against the 0.500 that was set — the documented `steady()` re-derivation, recorded
rather than assumed.

---

## 9. Two defects found in the existing pipeline

**(a) The non-convergence guard cannot fire.**
`mizer:::projectToSteady.MizerParams` signals with
`message("Simulation run did not converge after ", ...)`. Both
`09_Uncertainty_Analysis.Rmd:431` and `R/wmin_test/39_full_ensemble_paired.R:154`
detect it with a `warning =` handler, which never sees a message. Confirmed
empirically: across all **1,997 members × 2 arms** of the just-completed 39 run,
`ctl_reason`/`trt_reason` contain only `ok` (1,980) and `unstable` (17) —
`steady_no_converge` is **0 of 3,994** — while member 446 demonstrably does not
converge (distance 0.00216666 against `tol = 0.002`).

So the documented fidelity rule, "a single `steady()` call because the driver
treats non-convergence as outright rejection", is **not actually being enforced** —
non-converged draws pass through as converged. Whether the guard worked under mizer
2.5.0, when the ensemble was built, is unverified; checking would require
downgrading mizer, which breaks therMizer 1.0.0.

**How much does it matter in practice? Very little, on the evidence here.**
Rebuilding the w_min arms with a handler that listens on both conditions, 3 of the
7 members stop just short of tolerance — 446 at distance 0.002167, 173 at 0.002396
and 71 at 0.002075, against `tol = 0.002`. These are within 20% of tolerance and
comfortably inside the 0.0025 used elsewhere in the project, so **the resulting
steady states are accepted** (project lead's call, 2026-07-31). The control and
corrected arms also produce identical distances to 7 significant figures, so a
shared near-miss cancels exactly in the C − B comparison.

The finding is therefore about code correctness, not about the states already
computed: the guard cannot fire, so it provides no protection if a genuinely bad
draw ever appears. Worth repairing where it is cheap, not worth re-running
anything over.

Demonstration: `R/catch_fit/tests_steady_signal.R`. Repaired in
`R/catch_fit/05_wmin_arms.R`, which listens on both conditions and **records**
rather than rejects — rejecting would empty both w_min arms and destroy the pairing
that makes the comparison valid.

**(b) The 39-run filter certifies the post-`steady()` state, not the ensemble.**
As in §1. Members 1776 and 173 pass the filter with `R_max = Inf` in their stored
params. If the filter is meant to describe the members as they exist in the
ensemble, it should be recomputed on `sim@params` directly; if it is meant to
describe them after re-equilibration, that should be stated, because the refined
top-10% cut inherits the distinction.

---

## 10. Answers to the questions as posed

**Can catchability alone close the sperm/minke/baleen gap?**

| group | verdict |
|---|---|
| **sperm whales** | **Yes**, and the lever points *down*, not up — the model over-catches them. 3 of 7 members reach mod/obs = 1.00 and RMSE 0.78 → 0.27. The two members where it fails are stock-limited, not catchability-limited. |
| **baleen whales** | **Yes, mostly** — 0.36 → 0.31, and it was never a large contributor (1.6% of the objective). |
| **minke whales** | **No, and not at any catchability.** 11–26% of observed catch inside the box, 39% at `q = 100` with the population at 1.1%. Unanimous across 7 of 7 members. |

**If not, what reproduction change is required, and is it admissible?**
`R_max × 5` at fixed `erepro` for minke whales — `erepro` untouched, `R_max` goes
~71 → ~350 (finite), `erepro < 1` holds for all 19 groups with the maximum still
orca's untouched 0.9509, no new infinities anywhere in 531 configurations, and the
population depletes to 22–39% of unfished without collapsing. **Admissible on every
stated constraint.** The cost is a 5–8× larger pre-exploitation minke stock, which
is the real constraint and is an empirical question rather than a modelling one.

**Negative results, stated as findings.**
- The observed minke catch is not attainable at any admissible catchability, and
  is attainable through reproduction only at an unfished abundance 5–8× the
  calibrated one.
- The mechanism is *not* a recruitment ceiling — minke retain ~17× of reproductive
  headroom and the fix uses only ~4× of it. It is that recruitment and standing
  stock cannot be moved independently.
- Toothfishes and shelf-and-coastal fishes — 75% of the objective — cannot be
  improved materially by catchability *or* reproduction. Their error is
  inter-annual structure that a binary effort switch and a scalar catchability
  cannot represent. Time-resolved effort is the only lever that would touch them.
- Reproduction *level* (lever B) is a dead end for every group tested.
- `erepro` is a dead lever for groups at reproduction level ≈ 1 (member 446:
  baleen 1.0000, sperm 0.9994, orca 0.9989) — raising it raises `RDI` with almost
  no effect on `RDD`. Only `R_max` moves those groups.

---

## 11. Reproducing this

```
Rscript R/catch_fit/tests_levers.R                  # lever arithmetic, seconds
Rscript R/catch_fit/tests_steady_signal.R           # the message-vs-warning defect
Rscript R/catch_fit/tests_baseline_admissibility.R  # stored-params audit
Rscript R/catch_fit/00_validate.R                   # 5 gates
Rscript R/catch_fit/02_q_response.R                 # 1-D surfaces
Rscript R/catch_fit/02b_q_extend.R                  # extend edge optima
CF_CORES=13 Rscript R/catch_fit/03_q_optimise.R     # joint optimum + cap diagnostic
CF_CORES=7  Rscript R/catch_fit/04_repro_sweep.R    # reproduction
Rscript R/catch_fit/06_report.R                     # verdict tables
Rscript R/catch_fit/07_error_floor.R                # structural bound
Rscript R/catch_fit/08_repro_summary.R              # reproduction summary
Rscript R/catch_fit/09_verify_claims.R              # re-derive every number here
Rscript R/catch_fit/05_wmin_arms.R                  # arms B and C
CF_ARM=B ... ; CF_ARM=C ...                         # Stage 1 on each arm
Rscript R/catch_fit/10_arm_comparison.R             # C - B
```

Every quantitative claim in this document is re-derived from the saved outputs by
`09_verify_claims.R` and `10_arm_comparison.R`, so it can be checked rather than
taken on trust. Running them is the fastest way to audit what is written here.

`R/catch_fit/run_all.R` documents the order and the core budget. **Leave at least
one core free** — saturating all 16 risks taking the machine down mid-run.
