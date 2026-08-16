# Natural mortality targets for the Prydz Bay mizer model

Decided 2026-08-16. Machine-readable version: `csvs/mortality_targets_v1.csv`,
consumed by `R/wmin_test/100_reference_rebuild_arms.R` via `P100_TMAX_CSV`.

## Why this exists

The model set external mortality from a single allometry, `z0 = 0.6 *
w_inf^(-1/3)`. That is a fish-derived power law, and it degrades systematically
with body size: it is adequate below ~5 g, roughly 2x out for fish and birds,
12-33x out for the pinnipeds, and was 30-3600x out for the whales. Before this
revision the model implied a **mean** lifespan of 781 years and a **maximum** age
of 3,592 years for baleen whales.

The consequence was not cosmetic. A stock with M = 0.00128/yr has a relaxation
time of ~780 years, which is longer than `steady()` can resolve and far longer
than the 90-year projections the manuscript reports, so whale recovery was
blocked by the mortality field rather than by reproduction.

## How a maximum age becomes a mortality rate

**Convention: Hoenig (1983), `M = 4.22 / t_max`.** Checked against published
cetacean M before adoption:

| species | t_max | 3/t_max | **Hoenig** | 4.6/t_max | Then et al. 2015 | published M |
|---|---|---|---|---|---|---|
| minke | 50 | 0.060 | **0.084** | 0.092 | 0.136 | ~0.085 |
| orca | 80 | 0.038 | **0.053** | 0.058 | 0.089 | ~0.04-0.09 |
| sperm | 90 | 0.047 | **0.047** | 0.051 | 0.079 | ~0.055-0.07 |
| baleen | 90 | 0.033 | **0.047** | 0.051 | 0.079 | ~0.04-0.06 |

Then et al. (2015) is fitted to ~200 **fish** stocks and lands above every
published cetacean range; it is available as `P100_CONV=then2015` but is not
used. `3/t_max` (the 5%-survival rule) runs low.

**The target is set on ADULT M**, weighted over `w >= w_mat`, and the realised
predation mortality is subtracted to give `z0`. `z0` is size-flat, and both
published M estimates and Hoenig's relation refer to adults. For 18 of 19 groups
the adult and all-size means agree within 10%; toothfishes differ by 1.45x.

**Ordering constraint.** Where the orca kernel is also being changed, apply it
**before** the mortality edit: orca predation supplies a large share of minke's
and leopard seals' M, so the subtraction depends on it.

## What "maximum age" means here

mizer has no age dimension. Two diagnostics are reported and they answer
different questions:

- `4.22 / M_adult` — the **inverse of Hoenig**. Says what maximum age a real
  stock with this mortality would have. Circular for any group whose `z0` was
  set from a `t_max` target; it returns the target by construction.
- `surv_max_age()` — the **model-native** check, with no Hoenig assumption. Age
  at size from the same `dw/g` integral `age_mat()` uses, survival from
  `exp(-∫ mu/g dw)`, and maximum age as the age at which survival falls to 1%.
  Threshold-sensitive where adult mortality is tiny, so it is a cross-check, not
  a target.

The two agree within ~10% for 18 of 19 groups, because their mortality is nearly
size-flat. **Toothfishes are the exception** (15.7 yr by survivorship against
201 yr by inversion): heavy juvenile predation, near-zero adult mortality.

## The targets

| group | t_max (yr) | basis |
|---|---|---|
| mesozooplankton | unchanged | allometric `z0` is adequate at this size (0.6 yr, copepods ~1) |
| other krill | unchanged | 2.9 yr, *Thysanoessa* ~2-3 |
| other macrozooplankton | unchanged | 3.7 yr, amphipods/pteropods ~1-3 |
| antarctic krill | unchanged | 6.7 yr, *E. superba* 5-7 |
| salps | 1 | *Salpa thompsoni* is annual, seasonal bloom-and-crash |
| mesopelagic fishes | 8 | typical published maximum for Antarctic myctophids |
| bathypelagic fishes | unchanged | both diagnostics agree at ~14 yr, and predation alone already exceeds any longer target, so `z0` cannot reach it |
| shelf and coastal fishes | 20 | *Pleuragramma antarctica*, *Notothenia rossii*; matches the 18.7 yr survivorship estimate |
| flying birds | 50 | albatross-weighted — the group's `w_max` of 4,191 g is wandering-albatross sized |
| small divers | 20 | Adelie recorded to ~20 yr; macaroni and gentoo similar |
| squids | 2 | near-annual; covers the larger onychoteuthids without exceeding published Southern Ocean squid lifespans |
| toothfishes | **adult M = 0.13** | CCAMLR value for *Dissostichus mawsoni*, entered as `t_max = 32.46`. Specified as M rather than `t_max` because this group's mortality is strongly size-dependent |
| leopard seals | 26 | standard published maximum for *Hydrurga leptonyx* |
| medium divers | 35 | unweighted mean over the 8 member taxa (fur/crabeater/Ross/Weddell seals, emperor + king penguins, ziphiids, dolphins), matching how this group's `beta` was aggregated |
| large divers | 23 | standard maximum for female southern elephant seals (the group is elephant seals only) |
| minke whales | 50 | |
| orca | 80 | |
| sperm whales | 90 | raised from 70 to give `erepro` headroom; well inside the aged range |
| baleen whales | 90 | |

## Known limitation: three groups are not currently feasible

Applying the full set, the recalibration ladder converges (max biomass deviation
0.0138) but is **inadmissible** — three groups require `erepro > 1`:

| group | erepro required | control-arm erepro |
|---|---|---|
| large divers | 16.6 | 0.047 |
| leopard seals | 14.6 | 0.393 |
| small divers | 4.5 | 0.648 |

`erepro` rises with the mortality multiplier, so the headroom available is
roughly `1/erepro`. Leopard seals and small divers begin at 0.393 and 0.648 —
within a factor of 1.5-2.5 of the ceiling **before any change**. This predates
the mortality revision and also causes the unmodified model's own admissibility
failures.

The underlying cause is offspring size relative to reproductive output. Small
divers carry a realised `w_min` of 2,942 g against a `w_max` of 6,000 g —
offspring at half the adult mass — which also produces their implausible
`age_mat` of 15.7 yr. Leopard seals carry `w_min` = 104 kg from the source
table's "weight at birth", against a real pup mass of ~30 kg.

**These three targets therefore stand as the intent, not as implemented values.**
They require the reproduction parameterisation to be corrected first, or a less
ambitious target accepted. The other eleven changes apply cleanly.

## Sources

Longevity figures are literature-informed judgement recorded per group in
`csvs/mortality_targets_v1.csv`. **They have not been checked against the primary
papers** and must be before citation — the same caveat the phase-57 subsidy
assumptions carry. The best-constrained entries are toothfishes (CCAMLR M = 0.13
for *D. mawsoni*), antarctic krill, and the four whale groups.