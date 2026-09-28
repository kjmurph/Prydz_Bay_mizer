# Methods in the main text — placement map and prose

For `M et al Science.pdf` (draft of 2026-08). Companion file:
`docs/manuscript_methods_supplement.md`.

Written against the **phase-104 / q10** ensemble, as defined by `run_p104q10.R`.
Every number below carries the file it came from, so it can be re-checked.

**Science format.** There is no Methods section in the main text. Method enters
in three places only: short qualifying clauses inside the narrative, one compact
model-and-ensemble block, and the figure captions. Everything quantitative goes
to the supplement.

The model-and-ensemble block below is **367 words** in three paragraphs, plus
~120 words of one-clause additions elsewhere in the narrative. That is the whole
main-text methods budget.

---

## 1. Placement map

| Anchor (PDF page:line) | What is there now | Action |
|---|---|---|
| p.2:15 | "shifting the characteristic body size of harvested organisms by approximately **8 orders of magnitude**" | **Check.** The model's own span, Antarctic krill `w_max` 4.17 g to baleen whales 1.03 × 10⁸ g, is **7.4 orders**. Fig. 1B's caption says "10³ t to 4.2 g", the same 7.4. Suggest "more than seven orders of magnitude", or state the basis if the 8 comes from a different quantity. |
| **p.4:9–18** | One sentence: "we developed a mechanistic size-structured food-web model (Methods, supp) forced by observed catch records and historical fishing and whaling effort since 1870" | **The single methods insertion.** Replace with the three paragraphs in §2. Note also that **1870 is wrong for this build** — effort is zero before 1930 (§2, footnote). |
| p.4:17 | "RMSE < X for all species and functional groups" | **Filled.** Per-species RMSE computed across all 203 members; table in §3, full CSV in `Manuscript data/yield_rmse_per_species_p104q10.csv`. |
| p.4:20–34 | Counterfactual paragraph | **Keep.** Two clauses to add: both arms start from the *same* per-member 1841 state, and name the forcing product. Text in §2.4. |
| p.6:13–21 | "Compensatory food web responses…" | Attach the ±1 s.d. emergence definition **once**, here. Text in §2.5. |
| p.6:23–31 | Fig. 3 paragraph | Add that aggregation to 12 groups happens **within member, before any ensemble statistic**. Text in §2.5. |
| p.8:26–34 | Fig. 4 paragraph | Add that consumption is a within-member exploited ÷ unexploited ratio. Text in §2.5. |
| Figs 2–4 captions | `n = 167`, generic metric descriptions | **Rewrite.** Full replacements in §4. |

---

## 2. Prose to insert

### 2.0 The target passage, before and after

Everything goes into **one paragraph, p.4 lines 4–18**, which currently does four
different jobs in six sentences. Splitting it is what makes room for the methods.

**As it stands** (p.4:4–18), with the four jobs marked:

| | sentence | job |
|---|---|---|
| 4–9 | "Yet despite this exceptional record of exploitation… successive waves of exploitation." | **motivation** — the observational gap |
| 9–12 | "To reconstruct how this century-long exploitation pattern affected ecosystem structure and function through time, we developed a mechanistic size-structured food-web model (Methods, supp) forced by observed catch records and historical fishing and whaling effort since 1870." | **the model** — one sentence |
| 12–13 | "We used Monte Carlo sampling of ecological parameters to represent uncertainty and generate an ensemble of plausible ecosystem histories." | **the ensemble** — one sentence |
| 13–18 | "The single model ensemble accurately reproduced the observed catch trajectories… (Fig. S1; RMSE < X for all species and functional groups)." | **the fit claim** |

**After**: the motivation sentence stays as its own paragraph and ends the
section; the one-sentence model description becomes §2.1; a new forcing paragraph
(§2.2) is inserted, which the draft has nowhere at all; the Monte Carlo sentence
expands into §2.3 and runs straight into the fit claim, which keeps its position
and gains its number.

```
p.4:4-9    KEEP AS IS        motivation, ends "...successive waves of exploitation."
                             -> break paragraph here

p.4:9-12   REPLACE with §2.1   the model            (112 w)
[new]      INSERT   §2.2       the forcing          (126 w)
p.4:12-13  REPLACE with §2.3   the ensemble         (129 w)
p.4:13-18  KEEP, fill RMSE     the fit claim        (§3)

p.4:20-34  KEEP, two clauses   the counterfactual   (§2.4)
```

The three new paragraphs are consecutive, so in the submitted text they read as a
single methods block sitting between the motivation and the fit claim. Paste-ready
version of the whole passage is in **§2.6**.

---

### 2.1 Replacing p.4:9–12 — paragraph 1 of 3: the model

> To reconstruct how this century-long exploitation pattern affected ecosystem
> structure and function through time, we built a mechanistic size-spectrum
> food-web model of the Prydz Bay shelf and adjacent Southern Ocean
> (1.47 × 10⁶ km²) in the `mizer` framework, extended with `therMizer` for
> temperature-dependent rates. The model resolves 19 functional groups —
> from mesozooplankton to baleen whales — across 100 logarithmically spaced
> body-mass classes spanning 3.2 × 10⁻⁸ to 1.0 × 10⁸ g, so that predation,
> growth and mortality emerge from individual body size rather than being
> prescribed at the population level. Predation is size-selective through a
> lognormal feeding kernel with group-specific preferred predator–prey mass
> ratios; who can encounter whom is set by an interaction matrix derived from
> overlap in depth range and water-column use, and background mortality is set
> per group from published maximum age.

*(112 words. Sources: `05_therMizer_calibration_scale_model_domain.Rmd:48-52`;
`docs/size_parameter_audit.md:46`; `group params/trait_groups_params_vCWC_v5.csv`;
`interaction matrix/2g_trait_groups_interaction_matrix_vCWC.R`;
`docs/mortality_targets.md`.)*

### 2.2 Paragraph 2 of 3: the forcing

> The model was forced following the ISIMIP3a protocol adopted for FishMIP 2.0.
> Sea-surface temperature and five plankton fields were taken from the
> GFDL-MOM6-COBALT2 simulation over the model domain at 15 arc-minute resolution,
> 1961–2010, and averaged to annual means. Plankton carbon was assigned to four
> size classes and fitted each year as a log–log size spectrum, extrapolated
> across the model's resource grid; temperature acts on encounter, predation and
> the energy available for growth and reproduction through a thermal-performance
> function bounded by each group's thermal tolerance. The 1841–1960 spin-up
> repeats the 1961–1980 window six times, as the protocol prescribes, so no
> climate trend enters before 1961. Catch and effort come from the FishMIP
> `histsoc` fisheries reconstruction for the Prydz Bay region and, for whales,
> from the International Whaling Commission's individual-catch database clipped
> to the harvesting grounds within the domain.

*(126 words. Sources: `02_Preparing_Climate_Forcings.Rmd:49-65, 330-344, 441-458`;
`06_steady_state_therMizer.Rmd:472-483, 918-979, 1085-1089`;
`01_FishMIP_Fishing_Data.Rmd:42-43, 254`;
`00_Tidying IWC Southern Hemisphere data.Rmd:187-193`.)*

> **Two corrections to the current draft sentence.**
> (i) *"since 1870"* is not this build: the effort array runs 1841–2010 but is
> **zero before 1930**, when the whaling record begins
> (`06_steady_state_therMizer.Rmd:491-518`). Either say "since 1930" or say that
> the model is initialised in 1841 and exploitation begins in 1930.
> (ii) The repo consistently *calls* the repeated 1961–1980 window "ctrlclim",
> which is the protocol's wording, but the array actually repeated is built from
> the **obsclim** extraction — no ctrlclim file exists in the project. The
> sentence above is worded so it is true either way; if you want to name the
> product, it must be obsclim unless a ctrlclim extraction is obtained.

### 2.3 Paragraph 3 of 3: the ensemble

> Because many ecological parameters are poorly constrained for this region, we
> propagated that uncertainty with a Monte Carlo ensemble of 10,000 parameter
> sets, drawing group abundances, fishing catchabilities and the strength of
> density-dependent reproduction independently for each. Every set was brought to
> a steady state, spun up for 120 unfished years and then screened against four
> conditions: that the calibration converged, that the spun-up state was
> stationary, that every functional group persisted within a factor of two over a
> further 200 unfished years, and that reproductive efficiency remained physically
> admissible. Members failing any condition were discarded, leaving 203 that
> satisfied all four. Fishing catchability was then fitted to the observed catch
> of the nine harvested groups, using only years up to and including 2004 as the
> ISIMIP3a protocol requires, and all 203 members are carried through the analyses
> below.

*(139 words. Sources: `R/wmin_test/104_members_rebuilt.R`;
`R/wmin_test/93_rerank_p88.R:100-114`; `R/wmin_test/89_catchability_refit_2004.R:9-12`;
`Output_large_files/wmin_test/104_full_members.csv`; `run_p104q10.R:152-156`.)*

> **On the 10,000.** Written as the size of the initial draw, per your decision.
> What the repository traces end-to-end is the *final* protocol: 1,668 parameter
> sets rebuilt and screened to 203. The 10,000 comes from the original sampling
> campaign — `Output_large_files/monte_carlo_results_nsims_10000_SD_2_4_3_tol_0.0025_tmax_1500_nectar.rds`
> is on disk — whose surviving members were pooled and later rebuilt under the
> current protocol. The paragraph above is worded so that no intermediate count is
> asserted, only the start and the end, which is the broad overview you asked for.
> If a reviewer asks for the attrition chain, it is in the supplement (§S9.3) and
> in `104_full_members.csv`.

> **Note the last clause changed.** The draft's ranking-and-cut wording is gone,
> since all 203 are used. The pooled RMSE still exists and still ranks the members
> — it is what the best-fitting-10% subset in the supplement is built on — but it
> is no longer a selection step in the main narrative, so it is described as a
> catchability fit rather than a ranking.

### 2.4 Additions to the counterfactual paragraph (p.4:20–34)

Two insertions, no deletions.

After *"...an otherwise identical counterfactual simulation without exploitation
(materials and methods)"* add:

> Both members of each pair start from the same spun-up 1841 state and share the
> same parameters and environmental forcing, differing only in whether the
> historical effort series is applied, so the difference between them isolates
> exploitation exactly.

Replace *"outputs of Earth system model variables from the Fisheries and Marine
Ecosystem Model Intercomparison Project"* with:

> outputs of the GFDL-MOM6-COBALT2 Earth system model under the ISIMIP3a
> protocol adopted for the Fisheries and Marine Ecosystem Model
> Intercomparison Project

### 2.5 One-clause additions in the Results

**p.6:16, after "emergence threshold of one standard deviation from natural
variability (Fig. 2a)"** — the definition, stated once and never repeated:

> (natural variability is the temporal standard deviation of the ensemble-mean
> unexploited trajectory over 1841–2010, so the threshold is set by the
> unexploited system alone and is independent of the exploitation signal)

**p.6:23, in "Persistent changes in ecosystem size structure arose from
contrasting responses among functional groups (Fig. 3)"** — append:

> Species were aggregated into 12 groups within each ensemble member before any
> across-member statistic was formed, so that group mean body mass is a genuine
> abundance-weighted mean rather than an average of per-species ratios.

**p.8:27, in "Total krill consumption by predators remained close to
counterfactual levels"** — append:

> (the exploited-to-unexploited consumption ratio is formed within each member
> before the ensemble median is taken)

---

### 2.5a Find-and-place guide — locating the insertion points in the .docx

Every change is on **p.4**, in the two paragraphs that begin *"Yet despite this
exceptional record…"* and *"To detect and attribute ecosystem changes…"*. Search
strings below are unique in the document; the first six or seven words are enough
to land on them.

---

#### ▸ ANCHOR 1 — where the methods block begins

**Search for:** `successive waves of exploitation.`

Full sentence in the draft:

> …This observational gap limits our ability to assess how the ecosystem
> responded to, and recovered from, **successive waves of exploitation.**

**Action:** leave this sentence and everything before it untouched. Put a
**paragraph break** immediately after it. Everything from here to Anchor 4 is
replaced.

---

#### ▸ ANCHOR 2 — DELETE this sentence, replace with paragraphs 1 and 2

**Search for:** `To reconstruct how this century-long`

Delete in full:

> **To reconstruct how this century-long exploitation pattern affected ecosystem
> structure and function through time, we developed a mechanistic size-structured
> food-web model (Methods, supp) forced by observed catch records and historical
> fishing and whaling effort since 1870.**

**Replace with:** §2.1 (the model) **and then** §2.2 (the forcing), as two
paragraphs. §2.1 deliberately reopens with the same words — *"To reconstruct how
this century-long exploitation pattern affected ecosystem structure and function
through time, we built…"* — so the join reads continuously with what precedes it.

⚠️ *"since 1870"* disappears with this sentence. That is intended; effort is zero
before 1930.

---

#### ▸ ANCHOR 3 — DELETE this sentence, replace with paragraph 3

**Search for:** `We used Monte Carlo sampling of ecological parameters`

Delete in full:

> **We used Monte Carlo sampling of ecological parameters to represent uncertainty
> and generate an ensemble of plausible ecosystem histories.**

**Replace with:** §2.3 (the ensemble), as its own paragraph.

---

#### ▸ ANCHOR 4 — where the methods block ends; KEEP, with two edits

**Search for:** `The single model ensemble accurately reproduced`

> **The single model ensemble** accurately reproduced the observed catch
> trajectories of all nine harvested species between 1930 and 2010, capturing the
> rise and collapse of industrial whaling, the subsequent development of
> commercial fisheries and the emergence of the Antarctic krill fishery across
> yields spanning six orders of magnitude (Fig. S1; **RMSE < X for all species and
> functional groups**).

Two edits, nothing else:

1. `The single model ensemble` → `The ensemble` (there is no longer a *single*
   ensemble being contrasted with anything).
2. `RMSE < X for all species and functional groups` →
   `median per-species root-mean-square error of log₁₀-transformed annual catch
   between 0.61 and 1.05 across the nine harvested groups`

This sentence stays where it is and closes the block.

---

#### ▸ ANCHOR 5 — the counterfactual paragraph; KEEP, two insertions

**Search for:** `without exploitation (materials and methods).`

**Insert immediately after that full stop:**

> Both members of each pair start from the same spun-up 1841 state and share the
> same parameters and environmental forcing, differing only in whether the
> historical effort series is applied, so the difference between them isolates
> exploitation exactly.

**Then search for:** `outputs of Earth system model variables from`

Replace the phrase

> outputs of Earth system model variables from the Fisheries and Marine Ecosystem
> Model Intercomparison Project

with

> outputs of the GFDL-MOM6-COBALT2 Earth system model under the ISIMIP3a protocol
> adopted for the Fisheries and Marine Ecosystem Model Intercomparison Project

---

#### ▸ ANCHOR 6 — the sentence to rewrite yourself (Results, not methods)

**Search for:** `Despite more than a century of sequential exploitation`

> **Despite more than a century of sequential exploitation, we show that total
> ecosystem biomass (for all animals >1 g) remained largely within natural
> variability**, whereas ecosystem size structure underwent a large and persistent
> shift attributable to harvesting (Fig. 2).

The bolded clause is contradicted by Fig. 2 on this ensemble — biomass above 1 g
emerges *positively*, SNR +3.68. Flagged here because it sits inside the passage
being edited; the rewrite is yours.

---

**Summary of the six anchors**

| # | search string | action |
|---|---|---|
| 1 | `successive waves of exploitation.` | keep; paragraph break after |
| 2 | `To reconstruct how this century-long` | delete sentence → §2.1 + §2.2 |
| 3 | `We used Monte Carlo sampling of ecological parameters` | delete sentence → §2.3 |
| 4 | `The single model ensemble accurately reproduced` | keep; two edits |
| 5 | `without exploitation (materials and methods).` | keep; insert one sentence |
| 5b | `outputs of Earth system model variables from` | replace phrase |
| 6 | `Despite more than a century of sequential exploitation` | yours to rewrite |

---

### 2.6 Paste-ready: the whole of p.4 as it would read

Replaces the draft from p.4 line 4 to line 34. Paragraph breaks are as shown.
Square brackets mark the two decisions still open.

---

Yet despite this exceptional record of exploitation, we have remarkably little
information on how Southern Ocean ecosystems have changed. Historical catch and
whaling records document the identity, magnitude and timing of removals, but no
equivalent long-term, ecosystem-wide observations of biomass, size structure and
trophic function exist for the Southern Ocean. This observational gap limits our
ability to assess how the ecosystem responded to, and recovered from, successive
waves of exploitation.

To reconstruct how this century-long exploitation pattern affected ecosystem
structure and function through time, we built a mechanistic size-spectrum
food-web model of the Prydz Bay shelf and adjacent Southern Ocean
(1.47 × 10⁶ km²) in the *mizer* framework, extended with *therMizer* for
temperature-dependent rates. The model resolves 19 functional groups — from
mesozooplankton to baleen whales — across 100 logarithmically spaced body-mass
classes spanning 3.2 × 10⁻⁸ to 1.0 × 10⁸ g, so that predation, growth and
mortality emerge from individual body size rather than being prescribed at the
population level. Predation is size-selective through a lognormal feeding kernel
with group-specific preferred predator–prey mass ratios; who can encounter whom
is set by an interaction matrix derived from overlap in depth range and
water-column use, and background mortality is set per group from published
maximum age.

The model was forced following the ISIMIP3a protocol adopted for FishMIP 2.0.
Sea-surface temperature and five plankton fields were taken from the
GFDL-MOM6-COBALT2 simulation over the model domain at 15 arc-minute resolution,
1961–2010, and averaged to annual means. Plankton carbon was assigned to four
size classes and fitted each year as a log–log size spectrum, extrapolated across
the model's resource grid; temperature acts on encounter, predation and the
energy available for growth and reproduction through a thermal-performance
function bounded by each group's thermal tolerance. The 1841–1960 spin-up repeats
the 1961–1980 window six times, as the protocol prescribes, so no climate trend
enters before 1961. Catch and effort come from the FishMIP *histsoc* fisheries
reconstruction for the Prydz Bay region and, for whales, from the International
Whaling Commission's individual-catch database clipped to the harvesting grounds
within the domain.

Because many ecological parameters are poorly constrained for this region, we
propagated that uncertainty with a Monte Carlo ensemble of 10,000 parameter sets,
drawing group abundances, fishing catchabilities and the strength of
density-dependent reproduction independently for each. Every set was brought to a
steady state, spun up for 120 unfished years and then screened against four
conditions: that the calibration converged, that the spun-up state was
stationary, that every functional group persisted within a factor of two over a
further 200 unfished years, and that reproductive efficiency remained physically
admissible. Members failing any condition were discarded, leaving 203 that
satisfied all four. Fishing catchability was then fitted to the observed catch of
the nine harvested groups, using only years up to and including 2004 as the
ISIMIP3a protocol requires, and all 203 members are carried through the analyses
below.

The ensemble accurately reproduced the observed catch trajectories of all nine
harvested species between 1930 and 2010, capturing the rise and collapse of
industrial whaling, the subsequent development of commercial fisheries and the
emergence of the Antarctic krill fishery across yields spanning six orders of
magnitude (Fig. S1; median per-species root-mean-square error of log₁₀-transformed
annual catch between 0.61 and 1.05 across the nine harvested groups).

To detect and attribute ecosystem changes to exploitation, we paired each
historical simulation with an otherwise identical counterfactual simulation
without exploitation (materials and methods). Both members of each pair start
from the same spun-up 1841 state and share the same parameters and environmental
forcing, differing only in whether the historical effort series is applied, so
the difference between them isolates exploitation exactly. Counterfactual
experiments are widely used to detect and attribute anthropogenic climate change
and have recently been extended to climate impacts on marine ecosystems [IPCC;
Barrier et al.]. Both trajectories experienced identical historical environmental
forcing, allowing exploitation-driven changes to be distinguished from background
environmental variability while propagating uncertainty across the model
ensemble. Natural background variability was captured by forcing the model with
outputs of the GFDL-MOM6-COBALT2 Earth system model under the ISIMIP3a protocol
adopted for the Fisheries and Marine Ecosystem Model Intercomparison Project. We
used these paired trajectories to determine when changes in total biomass and
ecosystem size structure became detectable, whether compensatory redistribution
explained their contrasting responses, and whether recovery of biomass was
accompanied by recovery of food-web structure and trophic function.
**[FINAL SENTENCE — see note below.]**

---

**Two things to settle in the pasted text.**

1. **"since 1870" is gone.** The draft's model sentence said the model was forced
   "since 1870"; the replacement does not, because the effort array is zero before
   1930. If you want a date in the main text it should be 1841 (model start) or
   1930 (exploitation start), not 1870.

2. **The final sentence of the counterfactual paragraph is left out deliberately.**
   The draft ends: *"Despite more than a century of sequential exploitation, we
   show that total ecosystem biomass (for all animals >1 g) remained largely
   within natural variability, whereas ecosystem size structure underwent a large
   and persistent shift attributable to harvesting (Fig. 2)."* On the phase-104
   ensemble at the 1 g cutoff that first clause is contradicted by the figure —
   biomass rises and emerges, SNR **+3.68** across the 203 members. It is a
   Results sentence, not a methods one, so it is yours to rewrite; I have not
   guessed at the replacement.

*(`n = 203` is now settled throughout: main-text prose, all three figure captions
and the supplement. The `_top` files are the n = 20 sensitivity variant and are
referred to only in the supplement.)*

---

## 3. Filling the `RMSE < X` placeholder (p.4:17)

Computed 2026-08-29. Full table:
**`Manuscript data/yield_rmse_per_species_p104q10.csv`** (36 rows: nine species ×
two cuts × two year-sets).

**Recommended sentence**, over the years in which each group was actually caught,
across all 203 members:

> ...across yields spanning six orders of magnitude (Fig. S1; median per-species
> root-mean-square error of log₁₀-transformed annual catch between 0.61 and 1.05
> across the nine harvested groups).

### Per-species RMSE, all 203 members, fished years only

Median and interquartile range across members.

| group | years | median | IQR | worst member |
|---|---|---|---|---|
| sperm whales | 42 | **1.050** | 0.684–1.352 | 2.03 |
| toothfishes | 27 | 0.931 | 0.785–1.185 | 3.72 |
| shelf and coastal fishes | 51 | 0.889 | 0.733–1.271 | 3.46 |
| Antarctic krill | 23 | 0.715 | 0.483–1.065 | 3.57 |
| orca | 6 | 0.696 | 0.505–1.057 | 2.26 |
| minke whales | 36 | 0.682 | 0.549–0.799 | 1.60 |
| baleen whales | 39 | 0.625 | 0.486–0.939 | 2.11 |
| squids | 9 | 0.619 | 0.482–1.012 | 3.96 |
| bathypelagic fishes | 1 | 0.605 | 0.290–1.054 | 2.83 |

For comparison, the best-fitting 10% subset (n = 20) runs 0.473–0.781 on the same
basis — roughly 30% tighter, with the ordering rearranged (toothfishes worst,
sperm whales fourth). Both cuts are in the CSV.

**Use the fished-years table, not the full-window one.** Over the full window the
medians are systematically lower (baleen 0.451 against 0.625) because 22.5% of
the observations are years of zero catch that the model also scores as exactly
zero — see §3.1. Those rows are trivially correct and deflate the per-species
comparison unevenly: baleen whales gain most, since 36 of their 75 window years
are zero-against-zero.

### The pooled statistic, for reference

The members are also *ranked* on a single pooled RMSE over all nine groups and all
302 window observations. This no longer selects anything now that all 203 members
are used, but it is what the supplementary best-fitting-10% subset rests on. From
`Output_large_files/wmin_test/104_q10_rerank.rds`:

| | pooled log₁₀ RMSE |
|---|---|
| best member (644) | 0.533 |
| median of all 203 | 0.832 |
| worst of 203 (member 1921) | 2.107 |
| median of the best-fitting 20 | 0.610 |
| boundary of the best-fitting 20 (member 1823) | 0.633 |

> ⚠️ **Do not read these from `93_rerank_p88_ranking.csv`.** That path is
> hardcoded in `93_rerank_p88.R:173` while the RDS output path is
> environment-configurable, so the CSV is whatever rerank ran last. The copy on
> disk is from `104_rerank.rds` (the **non-q10** refit), written one minute after
> the q10 one and overwriting it — its numbers are 0.526 / 0.804 / 1.985, close
> enough to the q10 values to pass unnoticed. The authoritative source is
> `104_q10_rerank.rds`, which `run_p104q10.R` points at.

A separate, already-measured statement that could stand alongside either form
(`docs/monte_carlo_workflow_review.md` §3.6): after catchability fitting the
median modelled-to-observed catch ratio is within 7% of one for all nine
harvested groups (baleen 0.93, sperm 0.95, minke 1.01, the other six 1.00).

### 3.1 The `+1 g` offset — redundant in effect, but load-bearing in code

Checked directly, because the fitting window is easy to misread.

**The window is not "catch-only years."** It runs from each group's first year of
non-zero effort to `min(2004, last reported catch year, last effort year)`. The
phase-89 correction drops values that are *missing*; but the catch file is dense
and writes unfished years as explicit `0`, so **nothing is ever dropped as
missing** (`n_dropped_NA = 0` for all nine groups). What that correction actually
removed was *trailing* zero-catch years, by capping the window — toothfish
2005–2010, squids 1994–2009. Interior zeros remain:

| group | window | obs | of which observed catch = 0 |
|---|---|---|---|
| baleen whales | 1930–2004 | 75 | **36 (48.0%)** |
| toothfishes | 1971–2004 | 34 | 7 (20.6%) |
| minke whales | 1963–2004 | 42 | 6 (14.3%) |
| sperm whales | 1932–1979 | 48 | 6 (12.5%) |
| squids | 1979–1993 | 15 | 6 (40.0%) |
| shelf and coastal fishes | 1950–2004 | 55 | 4 (7.3%) |
| orca | 1972–1980 | 9 | 3 (33.3%) |
| Antarctic krill | 1974–1996 | 23 | 0 |
| bathypelagic fishes | 1991 | 1 | 0 |
| **total** | | **302** | **68 (22.5%)** |

Baleen whales' 36 include the 1941–45 cessation of Antarctic whaling.

**So the offset cannot simply be deleted** — without it those 68 rows evaluate
`log10(0)` and the objective is undefined.

**But it is inert.** Every one of the 68 is also a year of exactly zero *modelled*
catch: across all 203 members × 302 rows there are **13,804 rows with observed = 0
and modelled = 0, and zero rows where one is zero and the other is not.** That
follows from the construction — effort was derived from the catch record, so a
year with no catch is a year with no effort and the model lands on zero by
identity. Those rows contribute **0.0% of the total sum of squares**.

Consequences, measured:

- Dropping the zero rows gives an **identical ranking**: Spearman ρ = 1.000 over
  the 203 members, and the same 20 members in the top cut.
- Only the reported value moves, and only through the denominator: 302 → 234
  observations takes the median pooled RMSE from 0.832 to 0.945.
- On fished years alone the offset changes nothing past the third decimal. It is
  visible only for bathypelagic fishes, the one group whose modelled catch can
  fall below 1 g against a positive observation.

**Recommendation: leave the objective as it is.** It is what selected the
members, it is reproducible from the stored refit (verified here to a maximum
relative difference of 3 × 10⁻¹⁵), and changing it would alter nothing except the
numbers printed. Describe it accurately in the supplement — the offset exists to
keep zero-catch years finite, not to weight presence against absence — and report
the *per-species* table over fished years, where the offset plays no role at all.

> **This retires half of a standing note.** The offset was previously understood
> to "price a zero at up to 11.7 log units, so presence/absence dominates the
> objective" — measured on the phase-45 window, which ran to 2010 and contained
> rows where effort was at its maximum but catch was recorded as zero (toothfish
> 2005–2010 above all). The 2004 cap removed exactly those rows: no year in
> 2005–2010 now falls inside any fitting window, and no mismatched zero remains.

### 3.2 One caution to carry into the table: the ordering is offset-dependent

The per-species *ordering* still moves with the offset scale, though no longer
for the reason above. Median per-species RMSE across all 203, full window:

| group | +1 g | +1 kg | +1 t |
|---|---|---|---|
| sperm whales | 0.982 | 0.982 | 0.981 |
| shelf and coastal fishes | 0.856 | 0.851 | 0.746 |
| toothfishes | 0.830 | 0.751 | 0.344 |
| Antarctic krill | 0.715 | 0.715 | 0.684 |
| minke whales | 0.632 | 0.632 | 0.626 |
| bathypelagic fishes | 0.605 | 0.199 | 0.000 |
| orca | 0.569 | 0.568 | 0.525 |
| squids | 0.480 | 0.467 | 0.075 |
| baleen whales | 0.451 | 0.451 | 0.450 |

Spearman ρ between the 1 g and 1 t orderings is **0.667**. The mechanism is now
compression of genuinely small catches rather than pricing of zeros: toothfishes
average about 3 t yr⁻¹, squids about 0.3 t yr⁻¹ and bathypelagic fishes 690 g in
total, so a one-tonne offset swamps their observations while leaving the whales
untouched.

**This is much less of a problem on the full 203 than on the top 20** (where
ρ = 0.283). Sperm whales are the worst-fitting group under all three offsets, and
the three groups that move — toothfishes, squids, bathypelagic fishes — are
exactly the three with sub-tonne annual catches. The headline ordering is stable.

**Practical consequence:** state the units and the offset wherever the
per-species table appears. It costs one clause and forecloses the question.

---

## 4. Rewritten figure captions

Written for the **full retained ensemble, n = 203**. The best-fitting-10% subset
(n = 20) is a supplementary variant; its figure file is named in each note.

### Figure 2

> **Figure 2: Size-structure metrics detect exploitation more strongly than
> total biomass.** Signal-to-noise (SNR) emergence analysis across the model
> ensemble (n = 203 matched exploited–unexploited pairs), 1900–2010. In each
> panel the ensemble median of the paired differences
> (exploited minus unexploited; solid line) is divided by the natural-variability
> noise — the temporal s.d. of the ensemble-mean unexploited trajectory over the
> 1841–2010 baseline — with ribbons showing the interquartile range on the same
> scale. **a**, Community biomass, summed over all 19 groups above a 1 g body-mass
> cutoff. **b**, Biomass variability (15-yr trailing rolling s.d.). **c**, Slope of
> the normalised biomass size spectrum, fitted by ordinary least squares to
> log₁₀ biomass per octave bin, normalised by bin width, against log₁₀ of the
> geometric bin midpoint, over the same size range. **d**, Slope variability
> (15-yr trailing rolling s.d.). Dashed lines at SNR = ±1 mark the emergence
> threshold; the solid grey line marks SNR = 0. Vertical grey dashed lines mark
> the onset of whaling (1930) and krill fishing (1974); dotted lines mark peak
> effort (baleen 1933, sperm 1948, minke 1973, krill 1979), taken as the maximum
> of each group's effort series.

Notes for the caption writer:
- The 1 g cutoff retains **52 of the 100 size bins, spanning 27 octaves**;
  mesozooplankton and other krill leave the calculation entirely, Antarctic krill
  keeps only its largest bins, and every bird, seal and whale is untouched
  (`Manuscript scripts/F02b_figure2_snr_1g_rebuilt167.R:20-22`).
- Figure file: `Manuscript figures/p104 figures/fig2_snr_1g_p104q10.png`.
  The best-fitting-10% variant is `fig2_snr_1g_p104q10_top.png` (n = 20).
- The peak-effort years are **derived** from `effort_array_1841_2010.rds`, not
  hardcoded; they agree with the values above.
- The noise definition above is the `classic` mode and is the published one. Two
  alternatives were computed and are available for the supplement (`medsd`,
  `paired`); the `paired` denominator is the more conservative and is the one
  that matches the paired design. See the supplement, §S10.

### Figure 3

> **Figure 3: Exploitation-induced changes in abundance and mean individual body
> mass across functional groups.** Percentage change relative to a matched
> unexploited (climate-only) counterfactual in **a**, total abundance and **b**,
> mean individual body mass, for 12 aggregated functional groups, 1900–2010.
> Change is (exploited/unexploited − 1) × 100 formed within each of n = 203
> paired simulations; species are aggregated
> into the 12 groups within each member before any across-member statistic, so
> group mean mass is an abundance-weighted mean. Lines show the median and
> ribbons the interquartile range. Within each row the y-axis is fixed across
> **a** and **b** to allow direct comparison; scales differ between rows. Red
> dashed lines mark ±1 s.d. of natural variability (temporal s.d. of the
> unexploited ensemble mean over 1841–2010, as a percentage of its baseline
> mean); a median crossing them exceeds natural variability. Vertical grey dotted
> lines mark the onset of whaling (1930) and krill fishing (1974); coloured
> dashed vertical lines mark each group's peak exploitation effort.

Figure file: `fig3_pctchange_p104q10.png` (n = 203). The best-fitting-10% variant
is `fig3_pctchange_p104q10_top.png`.

### Figure 4

> **Figure 4: Historical exploitation reduces baleen whale krill consumption to a
> fraction of unexploited levels.** Ratio of Antarctic krill consumption under
> exploitation to a matched unexploited (climate-only) counterfactual, 1900–2010,
> for all predators (black), large baleen plus minke whales (pink) and fishes
> (ochre). Consumption is computed from the temperature-scaled diet of each
> member and the exploited ÷ unexploited ratio is formed within each of n = 203
> matched members before summarising; lines show the ensemble median and bands
> the interquartile range. The grey dashed line at ratio = 1 denotes consumption
> equal to the unexploited baseline. Colour-matched dashed lines mark each
> group's ±1 s.d. of natural variability (s.d. of the unexploited ensemble-mean
> consumption over 1841–2010, expressed as a coefficient of variation on the
> ratio scale); a median leaving its band marks an effect beyond natural
> variability. Vertical red dashed lines mark the onset (1974) and cessation
> (1996) of commercial krill fishing.

Figure file: `fig4_krill_ratio_p104q10.png` (n = 203). The best-fitting-10%
variant is `fig4_krill_ratio_p104q10_top.png` — note it tells a materially
different whale story (baleen + minke krill-consumption ratio at 2010 is about
0.72 on the top 20 against about 0.30 on the full 203), so the two cuts must not
be mixed between panels or between figures.

Note: "temperature-scaled diet" is not decoration. `mizer::getDiet()` returns
correct *proportions* on this model but incorrect *absolute* rates under
therMizer, so consumption is computed with the project's own `ther_diet()`
(`R/wmin_test/thermizer_shim.R`). Worth one sentence in the supplement, not the
caption.

---

## 5. Consistency checks before the figures are finalised

1. **Every `n` in the draft is currently 167**, from the superseded `rebuilt167`
   build. Each must become **203**, and must match the figure file actually
   placed — the `_top` files are the n = 20 variant and belong in the supplement.
2. **The Fig. 2 result changes on this ensemble.** Community biomass above 1 g at
   2010 has SNR **+3.68** on the full 203, against −0.75 on `rebuilt167` — biomass
   rises and emerges. The abstract and the "Compensatory food web responses"
   section are written for the old sign. This is a Results matter, not a methods
   one, but the two must be reconciled before submission. Verified in
   `Manuscript data/fig2_snr_series_1g_p104q10.csv` (15-yr window). Full set at
   2010: biomass +3.681, biomass variability 1.308, slope −25.633, slope
   variability 3.497.
3. **Fig. 1 is still built on `rebuilt167`** and is a catch-record figure, so it
   is unaffected by the ensemble change — but its caption should not carry an
   ensemble `n`.
4. **Do not mix cuts across figures.** Figure 4 in particular differs materially
   between them (§4). If any panel uses `_top`, say so in that caption.
5. Run `run_p104q10.R` with no arguments: it validates the member counts and that
   the catchability ceiling matches the refit. Any count in the manuscript that
   disagrees with it is wrong.
