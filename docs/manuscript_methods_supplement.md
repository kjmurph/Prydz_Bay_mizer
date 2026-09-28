# Supplementary Materials and Methods

For `M et al Science.pdf`. Companion file: `docs/manuscript_methods_main_text.md`.

Written against the **phase-104 / q10** ensemble and the **phase-100** reference
model, as defined by `run_p104q10.R`. Repo paths are given inline so every
statement can be re-checked against the file it came from; they are working
annotations and should be stripped before submission.

Two lists at the end must be cleared before this goes out: **§S12 (citations to
verify)** and **§S13 (values still to read out of the reference object)**.

---

## S1. Study domain and functional groups

The model represents the Prydz Bay region of East Antarctica as a single
well-mixed spatial domain of **1,474,341 km²** (1.474 × 10¹² m²). All biomass,
catch and yield quantities are expressed per whole domain: observed densities in
g m⁻² are multiplied by the domain area at model construction, so the state
variable is grams of wet mass over the region
(`05_therMizer_calibration_scale_model_domain.Rmd:45-58`).

The ecosystem is resolved as **19 functional groups**, chosen to span the
Southern Ocean food web from primary consumers to apex predators while keeping
groups that are trophically and morphologically coherent:

mesozooplankton, other krill, other macrozooplankton, Antarctic krill, salps,
mesopelagic fishes, bathypelagic fishes, shelf and coastal fishes, flying birds,
small divers, squids, toothfishes, leopard seals, medium divers, large divers,
minke whales, orca, sperm whales, baleen whales.

Nine of these have a recorded catch history in the region and are the harvested
groups throughout: Antarctic krill, bathypelagic fishes, shelf and coastal
fishes, squids, toothfishes, minke whales, orca, sperm whales and baleen whales.

Group definitions and the aggregation of constituent species into them were
compiled by an expert working group; the per-species parameter tables with their
literature sources are `csvs/predator_parameters_updated.csv` (the nine
air-breathing predator groups), `csvs/fish_parameters_updated.csv` and
`csvs/squid_parameters_updated.csv`, with the underlying collation workbooks in
`excel data/`. Aggregation to group level is documented in
`group params/1f_simplified_groups_params.Rmd` and
`group params/1g_simplified_groups_params.Rmd`.

For presentation, results are shown for **12 aggregated groups**: eight single
groups (large baleen whales, sperm whales, minke whales, orca, leopard seals,
toothfishes, shelf and coastal fishes, Antarctic krill) plus pinnipeds (medium +
large divers), seabirds (flying birds + small divers), pelagic fishes and squid
(mesopelagic + bathypelagic fishes + squids) and zooplankton (mesozooplankton +
other krill + other macrozooplankton + salps).

---

## S2. Size structure

Individuals are tracked on **100 logarithmically spaced body-mass classes**
covering **15.51 decades**, from 3.162 × 10⁻⁸ g (the smallest egg size, that of
mesozooplankton) to 1.03 × 10⁸ g (the asymptotic mass of baleen whales), i.e.
0.155 decades per bin. The plankton resource occupies a longer grid of **142
classes** extending below the consumer grid
(`docs/size_parameter_audit.md:46`; `02_Preparing_Climate_Forcings.Rmd:399`).

Each group is defined by an egg mass `w_min`, a maturation mass `w_mat` and an
asymptotic mass `w_max`. As-built values, from
`group params/trait_groups_params_vCWC_v5.csv`:

| group | w_min (g) | w_mat (g) | w_max (g) | bins |
|---|---|---|---|---|
| mesozooplankton | 3.162 × 10⁻⁸ | 3.162 × 10⁻⁵ | 3.162 × 10⁻³ | 32 |
| other krill | 2.923 × 10⁻⁷ | 1.585 × 10⁻³ | 0.342 | 39 |
| other macrozooplankton | 1.0 × 10⁻⁵ | 0.01 | 1.0 | 33 |
| Antarctic krill | 1.104 × 10⁻⁶ | 1.585 × 10⁻² | 4.173 | 43 |
| salps | 3.162 × 10⁻⁵ | 0.2512 | 25.12 | 38 |
| mesopelagic fishes | 1.0 × 10⁻³ | 11.28 | 240 | 36 |
| bathypelagic fishes | 6.969 × 10⁻⁴ | 62.55 | 603.7 | 39 |
| shelf and coastal fishes | 3.591 × 10⁻³ | 46.75 | 2,422 | 38 |
| flying birds | 40 | 1,719 | 4,191 | 13 |
| small divers | 3,627 | 4,267 | 6,000 | 2 |
| squids | 3.591 × 10⁻³ | 128.1 | 2.072 × 10⁴ | 44 |
| toothfishes | 3.351 × 10⁻² | 1.277 × 10⁴ | 1.576 × 10⁵ | 44 |
| leopard seals | 2.0 × 10⁵ | 3.48 × 10⁵ | 4.5 × 10⁵ | 3 |
| medium divers | 1.0 × 10⁴ | 2.948 × 10⁵ | 1.283 × 10⁶ | 14 |
| large divers | 1.35 × 10⁵ | 1.822 × 10⁶ | 2.024 × 10⁶ | 9 |
| minke whales | 6.0 × 10⁵ | 5.4 × 10⁶ | 6.0 × 10⁶ | 8 |
| orca | 4.9 × 10⁵ | 9.565 × 10⁶ | 1.063 × 10⁷ | 9 |
| sperm whales | 3.65 × 10⁶ | 3.285 × 10⁷ | 3.65 × 10⁷ | 8 |
| baleen whales | 2.25 × 10⁶ | 5.07 × 10⁷ | 1.03 × 10⁸ | 11 |

Three conventions require statement.

**Offspring mass for air-breathing groups is mass at independence** — weaning
mass for pinnipeds and cetaceans, fledging mass for birds — not mass at birth.
`mizer` has no mechanism for parental provisioning, so an individual entering
the spectrum at birth mass would be a fully independent forager competing for
prey it could not in reality catch. Entering at birth mass was tested explicitly
and made the model worse: reproductive efficiency rose from a maximum of 21.8 to
41.1 and several groups went extinct in the steady-state solve
(`docs/mortality_targets.md:114-124`; `R/wmin_test/100_reference_rebuild_arms.R:329-344`).

**Realised egg masses are snapped to the size grid** and therefore differ
slightly from the nominal values above (small divers 3,627 → 2,942 g; leopard
seals 2.0 × 10⁵ → 1.557 × 10⁵ g).

**Four groups carry `w_mat` set to 0.9 × `w_max`** (large divers, minke, orca and
sperm whales) and one (leopard seals) carries a raised `w_max`. In the source
compilation, maturation mass equals asymptotic mass for these groups; left
uncorrected, `mizer`'s internal validator silently rewrites `w_mat` to
`w_max`/4, which would have shrunk minke maturation mass 3.6-fold. The
corrections are deliberate and are documented at
`docs/size_parameter_audit.md:70-97`.

**Caveat.** Small divers occupy only two size bins, because penguin chicks fledge
at close to adult mass. The group is therefore effectively unstructured in size,
and any penguin-specific size-structure result carries that limitation
(`docs/size_parameter_audit.md:139-143`).

---

## S3. Feeding

### S3.1 Predation kernel

Prey selection follows a **lognormal feeding kernel** for all 19 groups,
parameterised by a preferred predator–prey mass ratio β and a width σ. β values
come from three tiers of evidence:

- **Five zooplankton groups** — from the log₁₀ PPMR ranges compiled by Heneghan
  et al. (2020, *Ecological Modelling*, doi:10.1016/j.ecolmodel.2020.109265),
  taking the midpoint of each functional type's range: mesozooplankton 531
  (omnivorous and carnivorous copepods), other krill and Antarctic krill
  1.585 × 10⁷ (euphausiids), other macrozooplankton 447 (chaetognaths), salps
  1.778 × 10⁹ (`03_model_setup_pre_therMizer.rmd:1036-1052`).
- **Nine air-breathing predator groups** — from `csvs/predator_parameters_updated.csv`,
  as the inverse of the mean across constituent species of (mean prey mass /
  maximum body mass), with the underlying prey-size observations cited per
  species. Benchmark bands are in
  `Reference model diet assessment/benchmarks/ppmr_benchmark_groups.csv`.
- **Fish and squid groups** — assumed rather than measured. These four values
  (mesopelagic 442, bathypelagic 425, shelf and coastal 350, squids 50) are the
  least constrained parameters in the kernel and are flagged as such in the
  provenance record (`Reference model diet assessment/analysis/RD07_beta_provenance.csv`).

Five values were revised from the source compilation during model development:

| group | value used | basis |
|---|---|---|
| baleen whales | 2.977 × 10⁷ | inverse mean of the four rorqual prey/body ratios; the southern right whale row in the source table duplicates the sperm whale entry exactly and implies 1.2 kg prey for a copepod feeder, so it was excluded (`R/wmin_test/54_reference_model_recalibrate.R:24-35`) |
| sperm whales | 2,000 | **a modelling choice, not a data correction.** The source value (4.4 × 10⁴) places preferred prey at 828 g; 2,000 places it at 18.25 kg. Sweeping β showed squid diet share peaking at 15.9% near 2,000 against 2.3% at the source value, against a diet-literature benchmark of 50–90% (`R/wmin_test/54_...R:11-19`) |
| orca | 45.7 | Tucker & Rogers (2014): body mass 10³·⁹⁶ kg, prey 10²·³ kg. The source value (0.558) is faithfully derived from the compilation but that row carries no source and places preferred prey at 19 t — heavier than the predator (`R/wmin_test/100_reference_rebuild_arms.R:43-52`) |
| small divers | 960 | geometric mean of the three penguin prey/body ratios. The arithmetic-inverse value (294) is dominated by a single gentoo outlier of 42.8 g against Adélie 1.79 g and macaroni 1.72 g (`100_...R:345-357`) |
| minke whales | 4.08 × 10⁶ | 0.81 × the source value, applied in two 10% steps while tuning the group's diet composition (`R/wmin_test/73_...R`, `74_interaction_matrix_edits2.R:19-21`) |

Kernel width σ is **2.0 for all groups except leopard seals (3.0)**. The
whale groups originally used a box (uniform-in-log) kernel; this was replaced
with the lognormal form for baleen and minke whales in model development
(`R/wmin_test/51_whale_kernel_recalibrate.R:18-20`), and the current model uses
the lognormal kernel throughout.

### S3.2 Interaction matrix

Which groups can encounter one another is set by a **19 × 19 symmetric
interaction matrix built from habitat overlap**, not from a diet matrix. It is
constructed in `interaction matrix/2g_trait_groups_interaction_matrix_vCWC.R`
from each group's depth range and water-column-use category, by:

1. base value = overlapping depth range / total depth range of the pair; zero if
   the ranges do not overlap;
2. × 0.5 where one group is a diel vertical migrator and the other is not;
3. diving predator × diving predator set to zero unless orca or leopard seals is
   one of the pair (× 4 in that case), and zero for any flying group;
4. × 0.5 for diving predator × resident pairs, representing seasonal presence;
5. × 0.2 for any pair involving the shelf group, this being the fraction of the
   domain deeper than 1,000 m;
6. capped at 1.

with pre-calculation depth overrides (flying birds capped at 50 m, both krill
groups at 500 m, all groups deeper than 2,000 m capped at 2,000 m). The base
matrix is `interaction matrix/trait_groups_interaction_matrix_vCWC_v4.csv`.

A small number of cells were subsequently revised where the depth rule produced
an ordering contradicted by the diet literature, most importantly the whale–krill
cells: the construction gives minke whales the joint-lowest krill interaction of
all 19 groups, purely because minke carry a 50 m maximum depth against baleen
whales' 200 m, which inverts the observed ordering (Ichii & Kato 1991, *Polar
Biology* 11:479–487). Baleen–krill was raised to 0.75 and minke–krill to 0.5.
Further revisions to minke, small-diver, leopard-seal and large-diver rows are
recorded with a stated basis per cell in `csvs/interaction_edits_v1.csv` and in
`R/wmin_test/73_interaction_matrix_edits.R` / `74_interaction_matrix_edits2.R`.
All edits are written symmetrically and the matrix is asserted symmetric after
each.

Access to the plankton resource is scaled per group by an
`interaction_resource` coefficient: 1.00 for the five low-trophic-level groups,
0.75 for the three fish groups and toothfishes, 0.25 for birds, divers, squids,
seals and the two baleen-feeding whale groups, and 0 for orca and sperm whales,
which do not feed on plankton
(`R/wmin_test/62_interaction_resource_balkrill.R:8-16`).

### S3.3 Feeding outside the model domain

Nine of the 19 groups are migratory or wide-ranging and obtain part of their
annual intake outside the Prydz Bay domain. Representing this by widening their
access to the in-domain resource is not adequate, because the resource spectrum
plays two size-segregated roles: for orca and sperm whales essentially 100% of
resource encounter comes from above 80 g, while for baleen and minke whales it is
2.4% and 1.0%. A single scalar per group cannot serve both
(`R/wmin_test/57_offdomain_subsidy.R:9-86`).

Instead, an **external encounter subsidy** is added as a separate term. A
reference encounter rate is computed against a resource spectrum log-linearly
extrapolated to 5 × 10⁷ g with the modelled prey removed — i.e. assuming the
out-of-domain environment has the same size-spectrum productivity as the domain —
and scaled per group by

`p_feed_outside = (proportion of the year spent outside the domain) × (proportion of that time spent feeding)`

| group | p_feed_outside | | group | p_feed_outside |
|---|---|---|---|---|
| flying birds | 0.58 | | minke whales | 0.15 |
| small divers | 0.31 | | orca | 0.60 |
| leopard seals | 0.25 | | sperm whales | 0.50 |
| medium divers | 0.35 | | baleen whales | 0.15 |
| large divers | 0.45 | | all other 10 groups | 0 |

Resident groups receive exactly zero by construction. Because the realised
external share of the diet is an emergent quantity rather than the input, the
subsidy is re-solved against the realised share by an odds-ratio adjustment,
iterated three times against a short steady-state solve and once more after
calibration; this brings the worst realised-versus-intended share from 2.1-fold
out to within 2% across all nine subsidised groups
(`100_reference_rebuild_arms.R:53-72, 463-477`; `docs/mortality_targets.md:190-193`).

**These fractions are an assumption layer and are declared as such.** They are
literature-informed judgements, they carry no seasonality (emperor and Adélie
penguin fasting periods occur in-domain), and the medium-diver value is an
unweighted mean across eight taxa. See §S12.

---

## S4. Background mortality

Mortality has two components: predation, which emerges from the size spectrum,
and an external ("background") rate `z0` representing everything the model does
not resolve — disease, senescence, unmodelled predators and emigration.

`z0` was originally set from a single fish-derived allometry,
`z0 = 0.6 w_max^(−1/3)`. That relation degrades systematically with body size:
adequate below ~5 g, roughly 2-fold out for fish and birds, 12–33-fold out for
pinnipeds and 30–3,600-fold out for whales. Left in place it implied a mean
lifespan of 781 years and a maximum age of 3,592 years for baleen whales, giving
the stock a relaxation time far longer than the period analysed and blocking
recovery through the mortality field rather than through reproduction
(`docs/mortality_targets.md:6-18`).

It was therefore reset from published longevity. For each group a target **adult**
natural mortality was obtained from maximum age by Hoenig's (1983) relation,
`M = 4.22 / t_max`, and the realised predation mortality (biomass-weighted over
`w ≥ w_mat`) was subtracted to give `z0`, which is applied size-flat:

`z0 = 4.22 / t_max − M_predation, adult`,  floored at 10⁻⁴ yr⁻¹

The target is set on adult mortality because both the published estimates and
Hoenig's relation refer to adults; for 18 of 19 groups the adult and all-size
means agree within 10%. Hoenig was checked against published cetacean mortality
before adoption and reproduces it closely (minke 0.084 against ~0.085 published;
sperm 0.047 against 0.055–0.070; baleen 0.047 against 0.04–0.06); the
alternative relation of Then et al. (2015), fitted to ~200 fish stocks, lands
above every published cetacean range and was not used
(`docs/mortality_targets.md:20-44`).

**Eleven of the 19 groups take a longevity-based `z0`:** salps 1 yr, mesopelagic
fishes 8, shelf and coastal fishes 20, flying birds 50, small divers 20, squids
2, toothfishes (see below), medium divers 35, minke whales 50, orca 80, sperm
whales 90, baleen whales 90 (`csvs/mortality_targets_v2.csv`).

**Five groups retain the allometric value**, which is adequate at their size:
mesozooplankton (implying 0.6 yr against ~1 yr for copepods), other krill (2.9 yr
against 2–3 for *Thysanoessa*), other macrozooplankton (3.7 yr), Antarctic krill
(6.7 yr against 5–7 for *Euphausia superba*), and bathypelagic fishes, where
predation alone already exceeds any longer target.

**Three groups retain the allometric value because the revised target is not
feasible**: leopard seals, large divers and small divers would require
reproductive efficiency above 1 — a physical impossibility — under any mortality
consistent with their published longevity. The cause is reproductive throughput,
not mortality: these three produce few, very large offspring
(`w_min`/`w_max` of 0.44, 0.67 and 0.60), and since `erepro ∝ w_min × RDI / E_R`
they sit near the ceiling before any change is made. Satisfying the constraint
would require maximum ages of roughly 38–50 yr (small divers), 95–120 yr (large
divers) and 384 yr (leopard seals), 2- to 15-fold above published values. This is
a stated limitation of the current parameterisation
(`docs/mortality_targets.md:87-176`).

**Toothfishes** are the one group specified by mortality rather than longevity:
**M = 0.13 yr⁻¹**, the CCAMLR value for *Dissostichus mawsoni*, from Dunn, Horn &
Hanchet (2006, WG-SAM-06/8), who estimated M by the methods of Chapman–Robson
(1960), Hoenig (1983) and Punt et al. (2005), obtained 0.11–0.17 yr⁻¹ and
proposed 0.13 for stock modelling; carried in the CCAMLR Stock Annex 2022 for
Subarea 88.1 §3.4 and used in the Ross Sea assessment of Mormede, Dunn & Hanchet
(2014, *CCAMLR Science* 21:39–62). Because M rather than `t_max` is specified,
this group's mortality is strongly size-dependent, and the corresponding
`t_max` of 32.5 yr is an arithmetic convenience, **not a longevity claim** —
*D. mawsoni* is routinely aged past 40.

Realised `z0` per group is in
`Output_large_files/wmin_test/100_final/100_post.csv` (rows `arm = "all_feed"`),
and ranges from 4.09 yr⁻¹ (mesozooplankton) to 0.0047 yr⁻¹ (baleen and sperm
whales, and large divers).

---

## S5. Reproduction

Reproduction follows a Beverton–Holt stock–recruitment relationship, with
density dependence expressed through the **reproduction level** — the ratio of
realised to maximum recruitment, so that 0 is density-independent and 1 is fully
saturated — and the reproductive efficiency `erepro` converting spawning output
to eggs.

Three groups reached reproduction levels above 0.99 in early calibration
(toothfishes 0.993, baleen whales 0.997, orca 0.999), leaving them numerically
pinned at their recruitment ceiling and unable to respond to any change in food
supply. These were capped at 0.9; the other 16 groups were left as calibrated.
Applying the cap holds the steady state exactly (maximum relative biomass change
0) (`R/wmin_test/58_cap_repro_then_free_erepro.R`).

Thereafter the calibration ladder (§S8) preserves the reproduction level and
allows `erepro` to absorb the recalibration, so reproduction level is a set
quantity and `erepro` is an outcome. Realised values in the reference model
(`100_final/100_post.csv`, `arm = "all_feed"`) span `erepro` 6.8 × 10⁻⁶
(Antarctic krill) to 0.844 (sperm whales), and reproduction level 0.22 (other
macrozooplankton) to 0.93 (toothfishes). Admissibility — `erepro < 1` for every
group — is a hard acceptance condition at every stage of both the reference
calibration and the ensemble build.

---

## S6. Environmental forcing

### S6.1 Products, variables and extraction

Forcing follows the **ISIMIP3a protocol as adopted for FishMIP 2.0**
(<https://github.com/Fish-MIP/FishMIP2.0_ISIMIP3a>). Fields are taken from the
**GFDL-MOM6-COBALT2** `obsclim` simulation, extracted over the Prydz Bay regional
domain at 15 arc-minute (0.25°) resolution as monthly means for **1961–2010**
(4,191 grid cells; `FishMIP_Plankton_Forcing/`):

| variable | use |
|---|---|
| `tos` | sea-surface temperature |
| `phypico-vint` | picophytoplankton carbon |
| `phydiat-vint`, `phydiaz-vint` | diatom and diazotroph carbon (summed) |
| `zmicro-vint` | microzooplankton carbon |
| `zmeso-vint` | mesozooplankton carbon |

Cell values are area-weighted by the supplied `area_m2` field and summed to a
domain total, then averaged from monthly to annual means; the model runs on an
annual timestep.

### S6.2 Spin-up and the historical period

The forcing arrays run **1841–2010**. Following the protocol, the 1841–1960
period repeats a 20-year window (1961–1980) six times — a window chosen by the
protocol because it spans a full ENSO cycle and carries no detectable trend — so
that model year 1841 corresponds to forcing year 1961 and the phase relationship
is preserved throughout. The transient period runs 1961–2010. Verified as exactly
six bit-identical repeats (`R/wmin_test/99_convergence_audit.R:109-119`;
`06_steady_state_therMizer.Rmd:918-979`).

> **Note for the authors, not for the supplement.** The protocol specifies the
> *control* simulation (`ctrlclim`) for the repeated window, and every text in
> the project uses that word. No ctrlclim extraction exists in the project; the
> repeated window is built from the obsclim rows. The wording above is accurate
> as written. If `ctrlclim` is to be named, the extraction must first be obtained.

### S6.3 Temperature

Temperature acts through `therMizer`, which rescales the encounter rate, the
predation rate and the energy available for growth and reproduction by a
thermal-performance function

`U_i(T) = T_K · (T_K − T_min,i) · sqrt(T_max,i − T_K)`,   T in kelvin

zeroed outside the group's tolerance range and normalised by its own maximum, so
the scaling is 1 at each group's thermal optimum and declines toward its limits.
Group thermal tolerances `temp_min`/`temp_max` (°C) are taken from the AquaMaps
model-based "preferred temperature" estimates served through FishBase and
SeaLifeBase, aggregated over constituent species:

| group | min | max | | group | min | max |
|---|---|---|---|---|---|---|
| mesozooplankton | −2 | 2 | | squids | −0.8 | 1.1 |
| other krill | −2 | 2 | | toothfishes | −1.5 | 8.8 |
| other macrozooplankton | −2 | 2 | | leopard seals | −1.9 | 1.4 |
| Antarctic krill | −2 | 2 | | medium divers | −1.8 | 2 |
| salps | −2 | 8.57 | | large divers | 0.1 | 1.6 |
| mesopelagic fishes | −2 | 5 | | minke whales | 0.2 | 7 |
| bathypelagic fishes | −2 | 5 | | orca | 0.3 | 13.1 |
| shelf and coastal fishes | −1 | 5 | | sperm whales | 0.3 | 3.8 |
| flying birds | 0.2 | 18.2 | | baleen whales | 0.2 | 10.2 |
| small divers | −0.4 | 1 | | | | |

Salps are the one non-AquaMaps entry, from Henschke & Pakhomov (2018,
doi:10.1002/lno.11061). Small-diver values are an assumption by analogy to medium
divers, and no source is recorded for the four whale groups (§S12).

Temperature is supplied as five depth realms (surface, 500 m, 1,000 m, 1,500 m
and bottom). The three intermediate realms are **linearly interpolated** between
surface and bottom temperature assuming the bottom realm lies at ~2,000 m; a
three-dimensional temperature field (`thetao`) was not extracted. Each group is
exposed to all five realms equally, so the temperature effect is a per-species
scalar that is constant across body size. Realised temperatures across realms
have a median of −0.93 °C and a range of −1.19 to −0.47 °C.

> **Note for the authors.** A per-species five-realm residence table exists
> (`08_ISIMIP3a_simulations_Prydz_Bay.Rmd:163-320`) but is commented out in every
> production call, so the model carries `therMizer`'s equal-residence fallback.
> The paragraph above describes what the model does. Applying the residence
> table would be a substantive change, not a documentation fix, and is a
> candidate improvement rather than an erratum.

### S6.4 Plankton resource

The resource spectrum is prescribed from the Earth system model rather than
simulated dynamically. For each year, the five plankton carbon fields are
converted from mol C to grams wet mass (× 12.001 g mol⁻¹, then × 10 for
carbon-to-wet-mass) and assigned to four size classes by equivalent spherical
diameter, with class midpoints converted to mass as spheres of unit density:

| class | ESD range | midpoint | source field(s) |
|---|---|---|---|
| pico | 0.2–10 µm | 5.1 µm | `phypico` |
| micro | 2–200 µm | 101 µm | `zmicro` |
| large | 10–200 µm | 105 µm | `phydiat` + `phydiaz` |
| meso | 200–20,000 µm | 10,100 µm | `zmeso` |

Size classes follow Dunne et al. (2005, 2012, 2013), Liu et al. (2021) and Stock
et al. (2020). Numerical abundance in each class is the class carbon divided by
its midpoint mass. A linear regression of log₁₀ abundance on log₁₀ midpoint mass
gives an annual slope and intercept, which are extrapolated across the full
142-class resource grid, following the method of Woodworth-Jefcoats et al.
(2019) (`02_Preparing_Climate_Forcings.Rmd:49-65, 325-360, 441-458`).

The fitted spectrum enters the model as an **anomaly**: each year's forcing is
that year's fitted spectrum minus the 1961–2010 mean fitted spectrum, added to
the model's own calibrated resource level. The absolute abundance of the resource
is therefore set by the model's calibration to observed biomass, and the Earth
system model supplies only its interannual and interdecadal variation
(`06_steady_state_therMizer.Rmd:1085-1089`). The resource is truncated above a
maximum size, beyond which the dynamic groups themselves represent the prey field
(§S13), and is initialised at the 2000–2010 mean of the forced spectrum.

Because the resource is prescribed, the classical background-spectrum parameters
(`kappa`, `lambda`, `r_pp`) do not govern the projections; the annually fitted
intercept and slope play their role.

---

## S7. Fishing and whaling

### S7.1 Catch records

Observed catch, `yield_observed_timeseries.csv`, covers **1930–2019** in grams
wet mass per whole domain per year, and merges two sources.

**Fisheries.** The FishMIP `histsoc` reconstruction
(`calibration_catch_histsoc_1850_2004_regional_models.csv`) filtered to the
Prydz Bay region, with catch taken as reported + illegal, unreported and
unregulated + discards, and mapped from FishMIP functional groups onto the
model's groups. Catches extracted at the Marine Ecosystem level for East
Antarctic Dronning Maud Land, Enderby Land and Wilkes Land — a closer match to
the model domain than the CCAMLR statistical area — fall entirely within the
shelf and coastal fishes group, which is why the small-pelagic FishMIP category
is assigned there rather than to mesopelagic fishes
(`01_FishMIP_Fishing_Data.Rmd:137, 153-171`).

**Whaling.** The International Whaling Commission individual-catch database
(Southern Hemisphere pelagic, land-station and revised Soviet Union records),
with positions converted to decimal degrees, quality-screened on the accuracy
flags, and **clipped spatially to the BANZARE Bank polygon** within the model
domain. Individual mass was obtained from body length by published
length–weight relations (`W = aL^b`, coefficients from SeaLifeBase): blue whale
(a = 0.0061, b = 3) applied to the aggregate baleen group, Antarctic minke
(0.0115, 3), sperm (0.0109, 3) and killer whale (0.2080, 2.577). Blue, fin,
humpback, sei and pygmy blue whales are aggregated into the single baleen group
(`00_Tidying IWC Southern Hemisphere data.Rmd`).

Non-zero catch coverage by group:

| group | years | n years | total (g) |
|---|---|---|---|
| baleen whales | 1930–2010 | 41 | 4.48 × 10¹² |
| sperm whales | 1932–1979 | 42 | 3.62 × 10¹¹ |
| minke whales | 1963–2019 | 43 | 1.94 × 10¹¹ |
| Antarctic krill | 1974–1996 | 23 | 9.86 × 10¹⁰ |
| shelf and coastal fishes | 1950–2004 | 51 | 1.67 × 10¹⁰ |
| orca | 1972–1980 | 6 | 1.77 × 10⁹ |
| toothfishes | 1971–2004 | 27 | 9.12 × 10⁷ |
| squids | 1979–1993 | 9 | 2.42 × 10⁶ |
| bathypelagic fishes | 1991 | 1 | 690 |

### S7.2 Effort

Fishing mortality is applied as effort × catchability × selectivity. Effort is
constructed per group and **normalised to its own maximum**, so each series runs
on [0, 1] and the absolute scale is carried by catchability.

For fisheries, effort is the nominal active effort from the FishMIP `histsoc`
regional effort product, summed across gears within each model group. For whaling,
no logbook effort field exists, so effort was derived from the IWC individual
records as a catch-per-unit-effort day count: expedition duration divided among
individuals in proportion to mass, summed by year and species. The merged array
is extended back to 1841 with zeros and is applied 1841–2010, 19 groups, one gear
per group.

**Exploitation begins in 1930** in this build; effort before that year is zero,
rather than the ISIMIP3a reconstructed transition effort, even though histsoc
effort exists for 1841–1949 in the regional product. This is a deviation from
the protocol text and should be stated
(`06_steady_state_therMizer.Rmd:472-518`).

Selectivity is a sigmoid function of body length. For the four whale groups it
was fitted from the IWC catch-length distribution: `l50` is the catch-weighted
60th percentile of observed catch length and `l25` is set just below it, giving a
steep, near-knife-edge selection consistent with the size preference of the
historical fishery (`09_Uncertainty_Analysis.Rmd:1358-1479`). For the remaining
groups, selectivity parameters were assigned from the size at which each group
enters the fishery (§S13).

### S7.3 Catchability

Catchability is not fitted in the reference model. It is fitted once per ensemble,
after the members are built, by a ratio-based iteration: a single global
multiplier per species is applied to every member's own drawn catchability, and
the multiplier is updated by the **median across members of the observed-to-modelled
catch ratio**, capped at a 50-fold change per iteration, for six iterations. This
preserves the Monte Carlo spread and moves only its centre
(`R/wmin_test/89_catchability_refit_2004.R`).

Two protocol conditions apply. First, ISIMIP3a permits calibration against
historical catch only for **years up to and including 2004**; each species'
fitting window therefore runs from its first year of non-zero effort to the
earlier of 2004, its last reported catch and its last year of effort. Second,
years with no reported catch are **dropped as missing, not treated as zero
catch** — coercing them to zero drove toothfish catchability down 65-fold, since
79% of toothfish effort falls in 2005–2010 where catch is not reported in this
product. Across the nine groups this gives 302 catch observations.

Catchability was fitted for all nine harvested groups including whales, with an
upper bound of 10. After fitting, the median modelled-to-observed catch ratio is
within 7% of one for every group (baleen 0.93, sperm 0.95, minke 1.01, the other
six 1.00). A lower bound of 1 on catchability, used in earlier development, was
binding for 68% of baleen and 67% of sperm whale members and distorted the fit;
at a bound of 10 only 9.4% of baleen members and no sperm whale members remain at
the ceiling. Median fitted catchability is 0.96 for baleen and 1.78 for sperm
whales; with effort normalised to [0, 1] these correspond to peak fishing
mortalities of order 1–2 yr⁻¹, which is a statement about the historical
fishery rather than an outcome of the model
(`docs/monte_carlo_workflow_review.md` §3.6).

---

## S8. Reference-model calibration

The reference model is the single parameter set from which every ensemble member
is derived. It is calibrated to observed group biomass by an alternating
procedure (`R/wmin_test/100_reference_rebuild_arms.R`):

1. solve for the steady state;
2. rescale each group's maximum recruitment to match its observed biomass;
3. re-solve for the steady state;
4. repeat, tightening the convergence tolerance through
   0.1 → 0.05 → 0.01 → 0.005 → 0.002 → 0.001, with six alternations at each
   tolerance above the target and 14 at the target;
5. retain the best state that both converged and was admissible at the target
   tolerance.

The steady-state solver uses a 1,000-year integration budget and preserves the
reproduction level, so `erepro` absorbs the recalibration. Two conventions
matter: the residual is read **after** the steady-state solve, never after the
biomass rescaling (which lands on the targets to machine precision and would
terminate the loop immediately), and a round counts as a pass only if it both
converged **and** left every group with `erepro < 1`.

Biomass targets are the balanced Prydz Bay Ecopath model of McCormack et al.
(2020), taken under no-fishing conditions, in g m⁻² and scaled to the domain:
mesozooplankton 8.8, other krill 1.9, other macrozooplankton 10, Antarctic krill
4, salps 0.652, mesopelagic fishes 1.2, bathypelagic fishes 1.2, shelf and
coastal fishes 2.732, flying birds 0.003, small divers 0.016, squids 0.15,
toothfishes 0.75, leopard seals 0.002, medium divers 0.265, large divers 0.011,
minke whales 0.014, orca 0.006, sperm whales 0.011, baleen whales 0.127
(`04_therMizer_calibration_scale_g_m2.Rmd:403-425`).

The accepted reference model reaches a maximum biomass deviation of 1.5% —
every group within 1.3% of its target — with a maximum `erepro` of 0.844, and is
admissible in all 14 rounds at the target tolerance. Realised maximum ages land
on their targets (baleen 90 yr, sperm 90, orca 80, minke 50, toothfishes 24).

> **Note for the authors.** Neither `matchGrowth()` nor `calibrateBiomass()` is
> used in the accepted configuration. `matchGrowth()` was tested and rejected: in
> this model it targets a von Bertalanffy growth coefficient that is a round
> default for 15 of the 19 groups rather than an independent observation, so it
> is not a constraint the model can meaningfully be fitted to. This is worth one
> sentence if a reviewer asks why growth was not matched.

---

## S9. Monte Carlo ensemble

### S9.1 What is sampled

Ten thousand parameter sets were drawn. Three axes of uncertainty are
propagated:

- **Group abundance** — a multiplier applied to each group's initial abundance
  spectrum *and* to its maximum recruitment, so that a member's abundance
  perturbation persists rather than being erased at the next steady-state solve.
- **Catchability** — a multiplier per harvested group.
- **Density-dependent reproduction** — a single reproduction-level ceiling per
  member, drawn from U(0.1, 0.9) and applied to the four large-whale groups
  (baleen, sperm, minke, orca).

Abundance and catchability multipliers are drawn independently per group per
member from lognormal distributions. The raw abundance draws placed a large
fraction of low-trophic-level groups below their calibrated abundance — 68.4% of
Antarctic krill draws fell below 1, with 59.7% sitting exactly on the lower bound
— which is implausible for a pre-exploitation state and left the lower half of
the distribution degenerate. The low-trophic draws were therefore resampled
(`R/wmin_test/96_substitute_draws_ltl.R`):

- **Mesozooplankton and Antarctic krill**: any draw below 1 was redrawn from
  U(1, 10), so no member starts these two groups below their calibrated
  abundance.
- **Other krill, other macrozooplankton, salps, and the three fish groups**: half
  of the members were redrawn from U(2, 10), a quarter were set to exactly 1, and
  a quarter retained a draw below 1, so that the low tail is represented but no
  longer dominates.
- **Baleen, minke and sperm whales**: every draw was replaced by U(5, 25),
  spanning the range of pre-exploitation abundance implied by historical
  estimates. Orca draws were left untouched.

The reproduction-level ceiling is **sampled, not fitted**. The catch data are
effectively blind to it: a 7.7-fold change in baleen whale catch moves the fit
measure by 0.3%, and its distribution is statistically indistinguishable between
retained and rejected members (Kolmogorov–Smirnov D = 0.076, p = 0.267). It is
reported as a propagated uncertainty throughout.

### S9.2 Member construction

The ensemble began as **10,000 sampled parameter sets**. Sets whose calibration
did not converge were discarded as they arose, and the surviving pool was rebuilt
under the single protocol described here so that every retained member is
constructed identically (`R/wmin_test/104_members_rebuilt.R`):

1. apply the drawn catchabilities;
2. scale initial abundance and maximum recruitment by the drawn multiplier;
3. re-solve for the steady state (tolerance 0.001, 1,000-year budget, preserving
   reproductive efficiency);
4. apply the drawn reproduction-level ceiling to the four whale groups;
5. re-solve through tolerances 0.01 → 0.005 → 0.002 → 0.001, preserving maximum
   recruitment, keeping the tightest tolerance that converged;
6. project 120 years with no fishing under the cycled spin-up forcing, and take
   the final state as the 1841 initial condition.

The spin-up length is six complete 20-year forcing cycles, so the initial
condition sits at the same phase of the forcing cycle as the start of the
historical run. Maximum recruitment is scaled once, at step 2, and never again;
scaling it at both the perturbation and after the reproduction cap would square
the multiplier.

### S9.3 Acceptance criteria

A member is retained only if it passes all four:

| criterion | test |
|---|---|
| **convergence** | at least one tolerance rung converged |
| **stability** | over the 120-year spin-up, the coefficient of variation of each group's biomass over the final 40 years is below 0.25, and no group shows a significant trend over the first 50 years (relative slope > 0.025 with p < 0.05) |
| **drift** | over a further 200 unfished years under cycled forcing, every group's final biomass lies within a factor of two of its initial value |
| **admissibility** | `erepro < 1` for all 19 groups |

Of the 10,000 sampled parameter sets, **203 satisfied all four conditions** and
form the analysed ensemble. The criteria are not nested — a member may be stable
but drift, or admissible but not stable — so the retained set is smaller than any
single screen would give. Attrition across the four conditions, over the members
carried into the final protocol
(`Output_large_files/wmin_test/104_full_members.csv`):

| condition | members passing |
|---|---|
| convergence | 1,419 |
| stability | 293 |
| drift | 223 |
| admissibility | 727 |
| **all four jointly** | **203** |

> **Note for the authors, not for the supplement.** The counts in this table are
> over the 1,668 parameter sets carried into the final protocol, not over the
> 10,000 initially sampled — the earlier campaign discarded its calibration
> failures as it went and did not retain a per-stage tally. The table is
> internally consistent and the 203 is exact; if a reviewer asks for a single
> attrition chain from 10,000, that chain is not reconstructible from the stored
> objects and the honest answer is to report the four conditions and the final
> count, which is what the main text does.

The drift screen is the strictest of the four and is deliberately so: the
steady-state solver's convergence test compares recruitment over a 1.5-year
window, which cannot resolve groups whose relaxation time is measured in
centuries. Under a loose tolerance a five-fold perturbation of baleen whale
abundance returns 4.78 of the intended 5 and is reported as converged, then
decays to 0.21 of the calibrated abundance over the following 200 years. Drift is
therefore measured directly rather than inferred from the convergence report.

**Two consequences to state.** First, the screens condition the prior: the
retained ensemble is not a fair sample of the sampled abundance distribution for
mid-trophic groups, which are selected toward lower draws (other macrozooplankton
median 1.00 retained against 3.29 rejected, KS p = 1.8 × 10⁻⁴⁰). Second, the
ensemble cannot be exactly regenerated. The steady-state solve selects among
basins of attraction, and members with more than one stable state may resolve
differently on a re-run; the stored ensemble is the analysed object.

### S9.4 Fit to observed catch

Fit to observed catch is measured by the root-mean-square error of
log₁₀(catch + 1 g), over the nine harvested groups and the 302 observations
within their permitted windows (§S7.3):

RMSE = sqrt( Σ_s Σ_t [ log₁₀(Y_mod + 1) − log₁₀(Y_obs + 1) ]² / N )

with catch in grams, so the offset is one gram. The logarithmic transform
equalises the contribution of groups spanning six orders of magnitude in catch,
from squids to baleen whales. Errors are pooled rather than normalised per
species, so every observation carries equal weight.

The offset keeps years of zero recorded catch finite. Sixty-eight of the 302
observations fall in such years, most of them baleen whale years — 36 of that
group's 75, including the 1941–45 cessation of Antarctic whaling. Because the
effort series was itself derived from the catch record, every one of those years
also carries zero modelled catch, so they contribute nothing to the measure:
recomputing it with them excluded changes no member's position relative to any
other (Spearman ρ = 1.000). The offset is therefore a numerical safeguard, not a
weighting of presence against absence.

**All 203 retained members are carried through the analyses.** Pooled across the
nine groups, their RMSE runs from 0.533 to 2.107 with a median of 0.832.

Per-group fit across the 203 members, over the years in which each group was
actually caught — median and interquartile range of the root-mean-square error of
log₁₀ annual catch:

| group | years | median | IQR |
|---|---|---|---|
| sperm whales | 42 | 1.050 | 0.684–1.352 |
| toothfishes | 27 | 0.931 | 0.785–1.185 |
| shelf and coastal fishes | 51 | 0.889 | 0.733–1.271 |
| Antarctic krill | 23 | 0.715 | 0.483–1.065 |
| orca | 6 | 0.696 | 0.505–1.057 |
| minke whales | 36 | 0.682 | 0.549–0.799 |
| baleen whales | 39 | 0.625 | 0.486–0.939 |
| squids | 9 | 0.619 | 0.482–1.012 |
| bathypelagic fishes | 1 | 0.605 | 0.290–1.054 |

Ranking members by pooled RMSE and taking the best-fitting 10%
(`floor(0.10 × 203) = 20`) gives a subset whose per-group errors are roughly 30%
tighter (0.473–0.781); it is used only as a sensitivity check on the ensemble
figures (§S11) and selects nothing in the main analyses.

Full table, including both cuts and the full-window variant:
`Manuscript data/yield_rmse_per_species_p104q10.csv`.

---

## S10. Counterfactual design and analysis metrics

### S10.1 Pairing

Each retained member was projected twice from its **identical spun-up 1841
state**, under identical parameters and identical environmental forcing:
once with the historical effort series and once with effort set to zero. The
pair therefore differs only in exploitation, and every reported quantity is a
within-member comparison formed before any across-member statistic. This removes
parameter uncertainty from the estimated effect of exploitation and leaves it in
the ensemble spread.

The pairing is exact rather than approximate. Initial effort is zero for every
gear, so the steady-state solve and the 120-year spin-up are unfished and
catchability cannot influence the initial condition; it enters only the
1841–2010 projection.

Baseline period for all natural-variability statistics: **1841–2010**. Analysis
and display window: **1900–2010**.

### S10.2 Signal-to-noise

For a metric X, in member i and year t, the signal is the ensemble median of the
paired difference and the noise is a single scalar — the temporal standard
deviation, over the 1841–2010 baseline, of the across-member mean unexploited
trajectory:

SNR(t) = median_i [ X_i,exploited(t) − X_i,unexploited(t) ] / sd_t [ mean_i X_i,unexploited ]

Ribbons show the interquartile range across members on the same scale. Because
the signal is already a paired quantity, the noise must not reintroduce the
across-member spread; dividing by the across-member standard deviation of the
paired differences would measure ensemble disagreement rather than natural
variability, and is not used.

Two alternative denominators were computed and are reported in the supplementary
figures: the median across members of each member's own temporal standard
deviation, and a fully paired form in which each member is scaled by its own
control. The fully paired form is the most conservative — it lowers the 2010
community-biomass signal-to-noise by about 14% and the slope signal-to-noise by
about 27% relative to the form above — and is the one that most closely matches
the paired design. Conclusions are unchanged under all three.

### S10.3 Community biomass and the 1 g cutoff

Community biomass sums biomass across all 19 groups over size classes of **1 g
and above**. The cutoff retains 52 of the 100 size classes, spanning 27 octaves:
mesozooplankton and other krill leave the calculation entirely, Antarctic krill
contributes only its largest size classes, and every bird, seal and whale is
unaffected. It is applied to the size-resolved state, so it requires
re-projection rather than post-hoc filtering of summed biomass. Results for the
full size range are given in the supplementary figures.

### S10.4 Size-spectrum slope

The normalised biomass size spectrum is formed by binning biomass into octaves
(successive powers of two in body mass) from the cutoff upward, dividing each
bin's total biomass by the bin's mass width, and regressing log₁₀ of that
normalised biomass on log₁₀ of the geometric midpoint of the bin by ordinary
least squares. The reported slope is the regression coefficient. Empty bins are
excluded and at least three occupied bins are required. This follows Method 5 of
Edwards et al. (2017).

### S10.5 Variability

Biomass and slope variability are the **15-year trailing rolling standard
deviation** of the corresponding series, computed per member and per arm before
the paired difference is taken. Windows of 3, 6, 9 and 12 years were also
computed and are shown in the supplementary figures; the choice of window affects
only the variability panels, not the level panels.

### S10.6 Group-level responses

For the 12 aggregated groups, abundance is the sum of numerical abundance over
the size grid and mean individual body mass is total group biomass divided by
total group abundance. **Aggregation is performed within each member before any
across-member statistic**, so a group's mean mass is a genuine abundance-weighted
mean and not an average of per-species ratios. The exploitation effect is the
paired percentage difference (exploited / unexploited − 1) × 100, summarised
across members as the median and interquartile range. The ±1 s.d. reference band
is the temporal standard deviation of the ensemble-mean unexploited trajectory
over 1841–2010, expressed as a percentage of its baseline mean.

### S10.7 Krill consumption

Consumption of Antarctic krill by three predator groupings (all predators;
baleen plus minke whales; all fishes) was extracted from the size- and
prey-resolved feeding rates of each member, with the temperature scaling of
§S6.3 applied consistently to both the encounter rate and the feeding level. The
exploited-to-unexploited ratio is formed within each member before the ensemble
median and interquartile range are taken.

> **Note for the authors.** This is why the consumption calculation does not use
> `mizer::getDiet()` directly. On this model that function returns exact diet
> *proportions* (agreeing to 2 × 10⁻¹⁶) but incorrect *absolute* rates, because
> the temperature scaling is applied outside it. Similarly,
> `mizer::getTrophicLevel()` returns invalid values on this model and no
> trophic-level quantity is computed from it; the mean trophic index in Fig. 1 is
> computed from assigned trophic levels for the harvested groups, not from the
> model's internal calculation.

---

## S11. Model evaluation

Supplementary figures, all built on the phase-104 ensemble:

| figure | file stem | shows |
|---|---|---|
| **S1** | `yield_facets_p104q10_n203` | modelled against observed catch, per group, per year |
| S2 | `biomass_rmse_grid_p104q10` | modelled against observed biomass, per group |
| S3 | `growth_curves_p104q10n203` | realised growth trajectories |
| S4 | `feeding_level_p104q10n203` | realised feeding level against body size |
| S5 | `recruitment_vs_biomass_p104q10n203` | stock–recruitment relationships |
| S6 | `diet_by_size_contemporary_p104q10` | diet composition against predator size |
| S7 | `diet_proportion_timeseries_p104q10` | diet composition through time |
| S8 | `fig2_snr_p104q10` | Fig. 2 over the full size range (no cutoff) |
| S9 | `fig2_snr_1g_p104q10_{paired,medsd}` | Fig. 2 under the two alternative noise definitions |
| S10 | `fig2_snr_1g_p104q10_{3,6,9,12}yr` | Fig. 2 variability panels at other window lengths |

All are in `Manuscript figures/p104 figures/` and its `Supplemental figures/`
subdirectory, in both PNG and PDF.

**Table S1** — per-group fit to observed catch: root-mean-square error of
log₁₀ annual catch across the 203 members, with median, interquartile range and
range. Source: `Manuscript data/yield_rmse_per_species_p104q10.csv`, which
carries all four combinations of member set (all 203 / best-fitting 20) and year
set (fished years only / full fitting window). **Report the `all203` +
`fished_only` rows**, and state that catch is in grams: the per-group ordering is
mildly sensitive to the offset scale, because three groups (toothfishes, squids,
bathypelagic fishes) have annual catches near or below one tonne. Sperm whales
are the worst-fitting group under a one-gram, one-kilogram or one-tonne offset
alike, so the headline ordering does not depend on the choice.

---

## S12. Citations to verify before submission

Every item below is used in the model and appears in the text above. Each is
recorded in the project as indicative and **must be checked against the primary
source before this supplement is submitted.**

| item | what needs checking | where it is used |
|---|---|---|
| The nine `p_feed_outside` fractions | The project's own record states the sources are indicative and unverified and that every one must be checked before citation | §S3.3 |
| Maximum ages for 10 of the 11 revised groups | Recorded as literature-informed judgement, not checked against primary papers. Only toothfishes, Antarctic krill and the four whale groups are well constrained | §S4 |
| Thermal tolerances, four whale groups | Listed with **no source recorded** | §S6.3 |
| Thermal tolerances, small divers | Explicitly an assumption by analogy to medium divers | §S6.3 |
| Leopard seal β = 100, σ = 3.0 | An undocumented override; the source compilation gives β = 11.2 and σ = 2.0 is used everywhere else | §S3.1 |
| σ = 2.0 generally | No numeric justification is recorded for the kernel width of any group | §S3.1 |
| Sperm whale β = 2,000 | Must be presented as a modelling choice with the diet-composition evidence, **not** as a data correction | §S3.1 |
| Ichii & Kato (1991) | Check the citation supports the minke/baleen krill-access ordering as stated | §S3.2 |
| Tucker & Rogers (2014) | Check the orca body- and prey-mass values | §S3.1 |
| Heneghan et al. (2020) | Check the five zooplankton PPMR midpoints against the published ranges | §S3.1 |
| McCormack et al. (2020) | Confirm the biomass values and the period they represent — the project records both "2001–2010" and "2010–2020" in different places and these cannot both be right | §S8 |
| Dunne et al. (2005, 2012, 2013), Liu et al. (2021), Stock et al. (2020) | Confirm they support the four plankton size-class boundaries as used | §S6.4 |
| Woodworth-Jefcoats et al. (2019) | The method is followed but its slope/intercept rescaling is **not** applied — check the citation is worded accordingly | §S6.4 |
| Hoenig (1983), Then et al. (2015), Dunn et al. (2006), CCAMLR Stock Annex 2022, Mormede et al. (2014) | Full bibliographic details | §S4 |
| Edwards et al. (2017) | Confirm "Method 5" is the correct designation for the normalised-biomass regression | §S10.4 |
| SeaLifeBase length–weight coefficients | Four relations, and the decision to apply the blue whale relation to the whole aggregated baleen group | §S7.1 |
| AquaMaps / SeaLifeBase | Correct citation form for model-derived preferred-temperature estimates | §S6.3 |

---

## S13. Values still to be read from the reference model

These are not recorded in any script or data file in the project and exist only
inside `params_ref_p100_mort_kernel_diet.rds`. Each is marked in the text above
where it belongs.

| value | why it is needed | note |
|---|---|---|
| resource maximum size (`w_pp_cutoff`) | §S6.4 states the resource is truncated; the size is not given | The project contains both a 1 g and a 100 g variant of the immediate ancestor and the evidence is genuinely ambiguous. Only the stored object settles it |
| `kappa`, `lambda`, `r_pp` | To confirm the framing in §S6.4 that they do not govern the projections | Expected to be superseded by the prescribed spectrum |
| `min_w_pp` | resource grid lower bound in §S2 | |
| `alpha`, `n`, `p`, `f0` | assimilation efficiency and the metabolic exponents | Expected to be `mizer` defaults; to be labelled as such once confirmed |
| per-group selectivity (`sel_func`, `l25`, `l50`, `knife_edge_size`) | §S7.2 describes the whale fit but not the other groups' values | |
| final `interaction_resource` vector | §S3.2 quotes the values as set; confirm they survived later recalibration | |
| per-group maximum recruitment (`R_max`) | for the parameter table | |
| the realised β and σ for all 19 groups | §S3.1 reconstructs them by tracing the edits through the development sequence | The five revised values are individually documented, but minke β in particular is inferred from two successive 10% reductions rather than read from the object. Confirm the whole vector |
| `age_mat` values and their source | used in the reference calibration; the script that produced them is not in the project | This one may not be recoverable from the object alone |

A short read-only script loading the reference object and writing all of these to
a CSV would resolve the table in one pass.

---

## S14. Documented but not part of this model

Listed so that the boundary of the model is explicit.

- **Sea ice.** The model has no sea-ice forcing of any kind: no ice variable, no
  ice field extracted. For an East Antarctic model this is a real structural
  omission, since ice governs both *Euphausia superba* recruitment and
  ice-obligate predator breeding, and it belongs in the limitations rather than
  the methods. A design for adding it exists
  (`docs/sea_ice_forcing_plan.md`) but nothing has been built.
- **Future projections.** Machinery to extend the forcing to 2100 exists and has
  been exercised as a test only; no result in this paper uses it.
- **Krill counterfactual scenarios.** A separate experiment on hypothetical krill
  fishing effort, not part of this paper's design.
- **Growth, feeding-level and diet assessments** (`R/growth_feeding/`,
  `Reference model diet assessment/`). Diagnostic evaluations of the reference
  model; they supply the supplementary evaluation figures in §S11 but are not
  part of the model's construction.
