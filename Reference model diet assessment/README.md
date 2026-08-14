# Reference model diet assessment

Emergent diet, trophic level and predator-prey mass ratio for the revised
reference-period model, `params_whale_lognormal_kernel_bk05_mk05.rds`
(small-diver `w_min` correction plus the whale feeding-kernel revision: baleen
and minke switched from a box kernel to lognormal, baleen `beta` = 2.468e7,
krill interaction 0.5 for both).

Everything here is built from that **single params object at its calibrated
steady state**, not from the 158-member ensemble. Ensemble options are listed at
the bottom.

## Running it

From the repository root, in order:

```bash
Rscript "Reference model diet assessment/RD00b_build_ppmr_benchmark.R"
Rscript "Reference model diet assessment/RD01_diet_by_size.R"
Rscript "Reference model diet assessment/RD02_trophic_level.R"
Rscript "Reference model diet assessment/RD03_ppmr.R"
Rscript "Reference model diet assessment/RD04_squid_availability.R"
Rscript "Reference model diet assessment/RD05_benchmark_comparison.R"
Rscript "Reference model diet assessment/RD06_sperm_whale_ppmr.R"
Rscript "Reference model diet assessment/RD07_parameter_provenance.R"
```

`RD00_common.R` is sourced by each and is not run directly. Scripts refuse to
overwrite existing outputs; set `RD_FORCE=1` to replace them. Other knobs:
`RD_PARAMS`, `RD_YEAR` (default 2010), `RD_OUT`.

## Two traps this code exists to avoid

**1. `mizer::getTrophicLevel()` is wrong on this model.** It returns orca 192,
toothfishes 61, baleen whales 27. `getTrophicLevel.MizerParams` builds its
numerator from `search_vol %*% pred_kernel` directly but takes its denominator
from `getEncounter()`, which dispatches to `therMizerEncounter`. The temperature
factor fails to cancel and compounds up the food web. `ther_trophic_level()` in
`RD00_common.R` fixes it; RD02 asserts the transcription is faithful by
reproducing mizer's own output bit-identically with the scaling switched off.
This affects any mizer extension that rescales encounter.

**2. `getDiet(proportion = TRUE)` is safe here; `proportion = FALSE` is not.**
The therMizer temperature effect is a per-species scalar, so it cancels exactly
from within-predator proportions (measured agreement with the repo's
`ther_diet()`: 2.2e-16, re-asserted on every run). Absolute consumption rates
differ by up to 1.03 log10 units and must go through
`R/wmin_test/thermizer_shim.R`.

## What each script produces

| script | figures | tables |
|---|---|---|
| `RD00b_build_ppmr_benchmark.R` | -- | `benchmarks/ppmr_benchmark_{observed,groups}.csv` |
| `RD01_diet_by_size.R` | `diet_by_size_reference`, 19x `per_species/diet_*`, `diet_by_size_mizer_native` | `RD01_diet_by_size_{grouped,full}`, `RD01_diet_summary` |
| `RD02_trophic_level.R` | `trophic_level_by_size{,_panels}`, `trophic_level_vs_ecopath`, `trophic_level_sensitivity` | `RD02_trophic_level_{by_size,methods,sensitivity,summary}` |
| `RD03_ppmr.R` | `ppmr_prey_size_vs_predator_size`, `ppmr_all_species`, `ppmr_whales`, `prey_size_vs_predator_size_whales` (last three also `*_no_krill_only`) | `RD03_ppmr_{by_size,summary}` |
| `RD04_squid_availability.R` | `sperm_whale_prey_overlap`, `sperm_whale_diet_factors`, `sperm_whale_squid_share_by_size` | `RD04_*` (5 files) |
| `RD05_benchmark_comparison.R` | `model_vs_benchmark_diet`, `background_resource_share_of_diet` | `RD05_model_vs_benchmark` |
| `RD06_sperm_whale_ppmr.R` | `sperm_whale_ppmr_sweep` | `RD06_*` (5 files) |
| `RD07_parameter_provenance.R` | — | `RD07_*` (3 files) |

Figures are written as PNG (300 dpi) and PDF pairs to `figures/`, tables to
`analysis/`.

## The two benchmarks are not equally trustworthy

**`benchmarks/ppmr_benchmark_*.csv` is derived, not transcribed.**
`RD00b_build_ppmr_benchmark.R` reads `csvs/predator_parameters_updated.csv`,
already in the repository with its own per-species sources and SDs, and derives
observed PPMR from `mean prey size / max body size`. It does not assume group
membership: for every predator group it searches subsets of the empirical
species for the one whose mean ratio reproduces the model's own `beta`, and
errors out if none does. All nine groups reproduce exactly.

**`benchmarks/diet_benchmark_southern_ocean.csv` is a hand transcription by an
LLM and is UNVERIFIED.** Every row carries a claimed source, a confidence flag
and a note ending "VERIFY BEFORE QUOTING". Use it as a sanity screen only. If
you later pull the SCAR Southern Ocean Diet and Energetics Database via
`sohungry` (not installed), it drops into the same schema without changing
`RD05`.

## Headline results

**Baleen whale PPMR is acceptable; no kernel change is indicated.** The answer
depends on which mean prey mass you use, so all three are reported:

| basis | log10 PPMR, w_min to w_max | slope |
|---|---|---|
| abundance-weighted arithmetic, all prey | 8.23 to 8.24 | -0.001 |
| abundance-weighted arithmetic, modelled prey only | 7.97 to 8.09 | +0.093 |
| biomass-weighted geometric, all prey | 7.20 to 7.68 | +0.289 |

Under the convention used in `PPMR_Plot_Example/` (arithmetic, abundance
weighted) the realised PPMR is essentially flat. The sharpest test is the mean
mass of the Antarctic krill actually taken: **0.47 g at w_min to 1.67 g at
w_max, a 3.6-fold shift across a 53-fold body-mass range**, against ~1-2 g for
an adult *E. superba*. Minke are 0.50 to 1.37 g.

**The kernel revision is corroborated by the benchmark.** The original baleen
`beta` of 219,113 was the arithmetic mean of the prey/predator mass *ratios*
across blue, fin, sei, humpback and southern right whales, so the single
copepod-feeding southern right whale (400-2000x the rorquals) dominated it. The
revised `beta` = 2.468e7, log10 7.392, sits inside the rorqual-only observed
range of 7.165-8.089 (mean 7.539).

**Squid contribute little to sperm whale diet mostly because squid are scarce,
not because of a size mismatch.** Decomposing `share = theta x availability x
biomass` against bathypelagic fishes (both `theta` = 0.5): squid are 2.1x less
size-available per gram but 8.0x less abundant, so in logs the 16.8-fold share
gap is **74% standing biomass, 26% size match**. Raising squid biomass 8x to
match bathypelagic fishes lifts squid from 2.3% to 15.7% of the diet.

**Trophic levels are sane once corrected but sit above Ecopath for all 19
groups** (+0.01 to +1.59, mean +0.69). Read that alongside
`trophic_level_sensitivity`: mizer's default resource trophic-level convention
(`w_R = 1e-10`, `beta_R = 1000`) assigns the largest resource particles
(`w_pp_cutoff` = 100 g) a trophic level of 5.0, and moving `w_R`/`beta_R` over a
plausible grid shifts species means by 1.25 to 3.99 trophic levels. The offset
from Ecopath is well inside what that convention alone can produce.

## Two data-quality flags, one of them consequential

**1. A corrupted cell in `csvs/predator_parameters_updated.csv`, and it is the
southern right whale row, not the sperm whale row.** `RD06` establishes the
direction:

- The five krill feeders (blue, fin, sei, humpback, minke) all encode the *same*
  prey mass, 1.19 g — an adult *E. superba* — as `1.19 / w_max`.
- The sperm whale row encodes 828 g, sourced to Evans & Hindell (2004).
- The southern right whale row encodes neither. Its implied prey mass of
  1247.67 g is exactly its own `w_max` × the **sperm whale** ratio, which is
  what copying a ratio cell rather than a prey mass produces. Its source cell is
  empty. And 1.25 kg is absurd for a copepod feeder.

The sperm whale value therefore stands at **PPMR 44,082, mean prey 828 g**; the
note recording Williams' 1/400 is a contrasting figure the compiler did not
adopt.

The consequence lands on the baleen group, because the original baleen `beta`
was the arithmetic mean of five ratios and the corrupted entry is ~1000x the
rorqual values:

| baleen `beta` | value | log10 |
|---|---|---|
| as recorded (corrupted SRW) | 219,113 | 5.341 |
| rorquals only, SRW dropped | 2.977e7 | 7.474 |
| SRW given the same 1.19 g krill | 3.205e7 | 7.506 |
| **revised, adopted in phase 51** | **2.468e7** | **7.392** |

One cell moved it by a factor of 136. Repaired, the compilation lands within 21%
of the value phase 51 chose on entirely independent grounds (`w_max` baleen /
`w_max` krill).

**2. `leopard seals` carry `beta` = 100 and `sigma` = 3.0** in the params object,
against 11.236 in `group params/trait_groups_params_vCWC_v5.csv` (Forcada et al.
2009) and `sigma` = 2.0 for every other group. An override not recorded in the
trait table.

## Would the Williams value help the squid? (RD06)

Yes, but not enough, and at a cost — and the intuitive geometric argument for
"no" is wrong. `beta` = 400 puts the preferred prey at 91 kg while the squid
group tops out at 20.7 kg, so the kernel peak sits 4.4x *above* the largest
squid; but `sigma` = 2 is wide enough that the lower tail still covers the squid
spectrum, and moving up abandons the background plankton resource (16.9% → 0.07%
of the diet). Net effect: **squid rise 5.1x, from 2.3% to 11.6%**.

| `beta` | preferred prey at `w_max` | squid % | toothfish % | resource % |
|---|---|---|---|---|
| 44,082 (in use) | 828 g | 2.27 | 2.49 | 16.91 |
| 1,995 (squid-maximising) | 18.3 kg | 15.93 | 55.81 | 1.05 |
| 400 (Williams) | 91.3 kg | 11.60 | 83.00 | 0.07 |

It should still not be adopted: 91 kg is far above any plausible *mean* prey
mass for a sperm whale, toothfishes take 83% of the diet, and the value has no
source behind it in this table where 828 g does.

The useful result is that **neither lever alone reaches the 50–90% squid the
diet studies report, but the two compound**:

| | `beta` in use | `beta` ≈ 2000 |
|---|---|---|
| squid biomass as modelled | 2.3% | 15.9% |
| 8x squid biomass | 15.7% | **60.2%** |

8x is the factor that brings squid level with the bathypelagic fishes (RD04's
counterfactual). So RD04's "74% biomass, 26% size" split describes the current
operating point, not a fixed property of the model.

## Where the PPMR parameters come from (RD07)

Three sources with three different evidential standards. `RD07` asserts that
every "published" beta reproduces exactly from its stated source.

| groups | source | standard |
|---|---|---|
| 5 zooplankton | Heneghan et al. 2020, *Ecol. Modelling*, doi 10.1016/j.ecolmodel.2020.109265 — published log10 PPMR ranges, midpoints taken. Recorded at [03_model_setup_pre_therMizer.rmd:1026-1052](03_model_setup_pre_therMizer.rmd#L1026-L1052) | published |
| 9 predators | `csvs/predator_parameters_updated.csv` — per-species *mean prey size / max body size* with SDs and named sources (Lea 2002, Slip 1995, Croxall & Prince 1980, Forcada 2009, Hindell 1989, Cherel 2002, Evans & Hindell 2004, …). Group value = mean of members' ratios, inverted | published |
| 4 fish groups + squids | `csvs/fish_parameters_updated.csv`, `csvs/squid_parameters_updated.csv` — **every PPMR row reads "calculated or assumed"** | **no source** |

So 14 of 19 betas trace to literature and **5 do not**: the four fish groups
(assumed 100–500) and squids (assumed 50, identical for all three cephalopod
taxa). Squid beta is one of the two levers RD06 identified, and it is among the
least defensible numbers in the kernel.

## The squid size ceiling (RD07)

`w_max` = 20,718.35 g **is** sourced, but for a taxon list that excludes the
largest Antarctic squid. It is `max(wmax)` over three taxa, all Lmax values from
Phillips 2004:

| taxon | Lmax (cm) | w_max (g) | w_mat (g) |
|---|---|---|---|
| **Onychoteuthiids** | **115** | **20,718.4** | 233.2 |
| Ommastrephiids | 56 | 3,271.6 | 111.5 |
| Small-medium nectonic squids | 50 | 2,446.3 | 39.4 |

Two caveats:

1. The length–weight conversion `W(g) = 2.92158e-4 × L(mm)^2.565` is itself
   "calculated or assumed". **The 115 cm is sourced; the 20.7 kg is not.**
2. Cranchiidae (*Mesonychoteuthis hamiltoni*, colossal squid) and Architeuthidae
   are simply absent from the taxon list — and *Mesonychoteuthis* is the taxon
   sperm whales in this sector are best known for taking.

**If you add colossal squid**, note the aggregation rule is
`Wmax = max`, `Wmat = mean`, `beta = mean`, so a fourth taxon moves `w_mat` and
`beta` too, not just the ceiling — supply its Lmat and PPMR as well. And do not
use the group's generic length–weight coefficients: applied to a 250 cm mantle
they give 152 kg against the ~500 kg of the largest landed specimen, because
`b` = 2.565 describes slender squid, not a cranchiid.

| candidate `w_max` | value | vs current |
|---|---|---|
| as modelled (3 taxa) | 20.7 kg | 1.0x |
| generic L–W at ML 250 cm | 152 kg | 7.3x |
| largest landed specimen | 500 kg | 24.1x |

**A larger ceiling alone would make squid *less* available, not more.** The
calibration pins total squid biomass to the observed 0.15 t/km², so extending
`w_max` redistributes the same biomass over a wider range — away from the sperm
whale's 828 g feeding window. At the current beta the squid share falls from
2.27% to 0.13%. Even paired with the squid-maximising beta the best case with
the total pinned is 15.9%, still short of the 50–90% observed. (An "unpinned"
variant reaches 54–99%, but it requires total squid biomass to rise ~900x, which
the calibration would never permit — it is an upper bound, not a scenario.)

This is the same conclusion RD06 reached from the other direction: **squid
standing stock is the binding constraint**, and the ceiling, the kernel and the
biomass should be treated as one decision rather than three.

## Phase 54: the recalibrated object

`R/wmin_test/54_reference_model_recalibrate.R` applies four corrections and puts
them through phase 51's steady()/matchBiomasses ladder, with a paired control
arm. Output: **`params_ref_sw2000_balror_mnkfish05.rds`** at the repo root.

| change | from | to | basis |
|---|---|---|---|
| sperm whale `beta` | 44,082 | **2,000** | modelling choice, from RD06's sweep — **departs from the source table** |
| baleen whale `beta` | 2.468e7 | **2.9765e7** | data correction: 1 / mean of the four rorqual ratios, dropping the corrupted SRW cell |
| `theta[minke, mesopelagic]` | 0.0125 | **0.5** | symmetric write |
| `theta[minke, shelf & coastal]` | 0.0069 | **0.5** | symmetric write |
| squid `w_max` | 20,718.35 | *unchanged* | data-driven; see above |
| minke `beta` | 5,042,016.81 | *unchanged* | **verified correct** = 1 / 1.98333333333333e-07, implying 1.19 g prey |

The symmetric write is provably inert in reverse: both fish groups place a
maximum kernel weight of **exactly 0** on minke-sized prey, asserted in the
script rather than assumed.

**Calibration quality is unchanged.** Selected max biomass deviation: base
0.01229, control (re-ladder, no change) 0.01218, revised **0.01220**. Every
group's modelled/observed ratio stays within 1.012, and identical to base for
all but sperm whales (1.0015 → 0.9977). Reproduction is an outcome: `erepro` is
unchanged throughout; sperm whale `R_max` moves 0.717 → 0.642.

### What the changes did

| | base | recalibrated |
|---|---|---|
| **sperm whale** squid share | 2.26% | **15.87%** |
| sperm whale fish share | 79.2% | 83.0% |
| sperm whale plankton resource | 17.0% | **1.15%** |
| sperm whale total intake | 2,312 t/yr | 1,246 t/yr |
| sperm whale trophic level | 5.82 | 6.26 |
| **minke** fish share | 0.06% | **1.70%** (26x) |
| minke krill share | 43.9% | 43.5% |
| **baleen** krill share | 40.9% | 40.8% |
| baleen mean krill taken at w_max | 1.67 g | 1.61 g |

Three things worth noting:

1. **The sperm whale sweep prediction held.** RD06 forecast 15.9% squid at
   `beta` = 2000 against the fixed prey field; the recalibrated model gives
   15.87%. So the recalibration did not undo the kernel change.
2. **Realised sperm whale PPMR moved *onto* the observed value even though the
   nominal beta moved away from it.** Realised log10 PPMR at `w_max` is now
   4.70, against an observed band of 3.97–5.32 centred on 4.64 — inside it,
   where the base model's 5.91 was outside. The realised mean is pulled down by
   abundant small prey, so a nominal beta far from the compilation can still
   produce a realistic mean prey size.
3. **The squid shortfall is reduced but not closed.** On the identifiable-prey
   basis squid go from 2.7% to 16.0% against a benchmark of 50–90%: the gap
   narrows from −47.3 to −34.0 points. Overall benchmark fit is unchanged at
   12/27 inside range. As RD04/RD06/RD07 all concluded, squid standing stock is
   the remaining binding constraint.

Diagnostics for the recalibrated object are in `recalibrated_54/`, produced by
re-running RD00b–RD07 with `RD_PARAMS` and `RD_OUT` set.

## Ensemble spread -- not built

The deliverable above is the single params object. Options, in cost order:

- **E1 (minutes, no new simulation).** `Manuscript data/diet_composition_kernel158.rds`
  already holds `size_prop` as `[sim x predator x w x prey]` for 2001-2010
  across the 158 stable members. A short `RD06` would give 10th/50th/90th
  percentile bands per (predator, size, prey group) with the reference model
  overlaid, answering whether it is representative.
- **E2 (about an hour).** Extend `Manuscript scripts/F00s_diet_composition_from_states.R`
  to also emit corrected trophic level and realised PPMR per member from the
  states in `Output_large_files/wmin_test/53_bk05_mk05_n167_states/`, giving
  ensemble bands for RD02 and RD03 as well. This is the option that would say
  whether the PPMR result is a property of the kernel or of one calibration.
- **E3.** Re-derive everything from the 164 states. Only if E1/E2 fall short.

## Not done

`RD03_ppmr.R` defines `sweep_kernel()` -- a `beta` x `sigma` grid for baleen and
minke, recomputing realised PPMR and krill diet share against the fixed current
prey field in seconds -- but **does not call it**. No kernel parameter is changed
anywhere in this folder.