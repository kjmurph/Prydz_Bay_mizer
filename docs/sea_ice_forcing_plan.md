# Sea ice forcing for ice-dependent groups — implementation plan

Drafted 2026-08-19. A plan, not a record: nothing below has been built. Every
number marked **(verified)** was measured in this session against
`params_ref_p100_mort_kernel_diet.rds` or read out of the installed mizer 3.1.0;
everything else is a design proposal and is flagged as a decision.

---

## 1. The gap

The model has **no habitat link to sea ice at all**. Its only climate forcings are
`ocean_temp` (from `tos`) and `n_pp_array` (a size spectrum fitted to the ISIMIP3a
plankton fields), both entering through therMizer. There is no ice variable in
`other_params$other`, no ice file anywhere in the repository, and `siconc` was never
extracted **(verified)**.

For a Prydz Bay model that is a real structural omission. Sea ice is the dominant
habitat variable in East Antarctica, and the two processes it governs — *Euphausia
superba* recruitment and ice-obligate predator breeding — are precisely the two the
model currently represents as if ice did not exist.

**The objective**: add sea ice as a forcing on **recruitment** and **survival** for
the groups whose habitat depends on it, in a form that (a) leaves the calibrated
historical run untouched, (b) fits the ISIMIP3a structure the project already
follows, and (c) supports a bespoke forward scenario set for hypothesis testing
until a protocol-aligned future forcing is adopted.

---

## 2. What already exists — the ground to build on

| | | source |
|---|---|---|
| Year-indexed forcing pattern | `other_params$other$ocean_temp`, `$n_pp_array`, rows named by year, 1841–2010 | verified |
| Forcing ingestion | `02_Preparing_Climate_Forcings.Rmd`, reading domain-clipped monthly CSVs from `FishMIP_Plankton_Forcing/` | verified |
| File naming | `gfdl-mom6-cobalt2_obsclim_<var>_15arcmin_prydz-bay_monthly_1961_2010.csv` | verified |
| Variables already extracted | `tos`, `phydiat`, `phydiaz`, `phypico`, `zmeso`, `zmicro` | verified |
| Future-projection machinery | `R/wmin_test/94_future_projection.R` — extends the forcing arrays explicitly to 2100, three effort arms plus krill arms | verified |
| Spin-up construction | ctrlclim 1961–1980 repeated ×6 over 1841–1960, bit-for-bit | verified, phase 99 |
| `other_mort` slot | **empty** — nothing to conflict with | verified |

**The forcing trap that must be respected.** therMizer does *not* error on years
outside its record: both `scaled_temp_effect()` and `plankton_forcing()` open with

```r
if (!floor(t) %in% <years>) t <- t %% <last year> + <first year>
```

so an ice array that is shorter than the projection would silently wrap to the early
historical period with no warning. `plankton_forcing()` also takes its row index from
`dimnames(ocean_temp)`, not from its own array. **Any ice array must be built to
exactly the same years and length as `ocean_temp`, and asserted.** Phase 94 already
does this for temperature and plankton; the ice array joins that assertion.

---

## 3. The two hooks — verified mechanics

This is the part that most constrains the design, and two of the four obvious
approaches do not work.

### 3.1 Recruitment: replace `RDI`, **not** `RDD`

`mizerRates` calls them differently **(verified)**:

```r
r$rdi <- rates_fns$RDI(params, n = n, n_pp = n_pp, n_other = n_other, t = t, ...)
r$rdd <- rates_fns$RDD(rdi = r$rdi, species_params = params@species_params, ...)
```

**`RDD` never receives `t`**, so it cannot host a time-varying forcing.
`RDI` does. The hook is therefore:

```r
iceRDI <- function(params, n, n_pp, n_other, t, ...) {
  rdi <- mizerRDI(params, n, n_pp, n_other, t, ...)
  rdi * ice_repro_multiplier(params, t)      # named vector, 1 for untreated groups
}
params <- setRateFunction(params, "RDI", "iceRDI")
```

This is also the biologically correct placement: ice acts on egg-to-larval survival,
*then* Beverton–Holt density dependence acts on what survives. Scaling `RDI` says
"fewer larvae entered the recruitment function", which is the claim being made.

Note this composes with therMizer, which replaces only `Encounter`, `PredRate` and
`EReproAndGrowth` — `RDI` is untouched at `mizerRDI` **(verified)**.

### 3.2 Survival: `setComponent(..., mort_fun = )`, **not** `z_ext`

`z_ext · w^d` is a monotonic power law and cannot produce a size band. But
`mizerMort` does this **(verified)**:

```r
mort <- pred_mort + params@mu_b + f_mort
for (i in seq_along(params@other_mort))
    mort <- mort + do.call(params@other_mort[[i]],
        list(params =, n =, n_pp =, n_other =, t = t, component =, ...))
```

So `setComponent(params, "sea_ice", mort_fun = "iceMort")` gives an **additive,
time-aware, arbitrarily size-banded** mortality term that leaves `mu_b` — and
therefore the calibration, `erepro` and the steady state — completely untouched.
`mort_fun` is a documented argument of `setComponent` **(verified)**.

**Warning that must be in the script header**: `steady()` projects, `project()` calls
`mizerMort`, and `mizerMort` runs the `other_mort` loop. **A component added before a
member is built enters the calibration.** Add it to the stored state at projection
time only, or make it strictly neutral over the reference window.

---

## 4. Which groups — and the targeting problem stated honestly

### 4.1 Taxon composition (verified, `csvs/predator_group_names.csv`)

| model group | member taxa | ice relationship |
|---|---|---|
| **medium divers** | Emperor Penguin, King Penguin, Weddell Seal, Crabeater Seal, Ross Seal, Antarctic Fur Seal, dolphins, ziphiids | **4 of 8 ice-obligate breeders** (emperor: fast ice; Weddell: fast ice; crabeater, Ross: pack ice); king penguin and fur seal sub-Antarctic; dolphins/ziphiids irrelevant |
| **small divers** | Adélie, macaroni (Crested), Gentoo penguins | **mixed sign** — Adélie ice-associated, gentoo ice-*avoiding* and expanding as ice retreats, macaroni sub-Antarctic |
| **antarctic krill** | *Euphausia superba* | **strongly ice-dependent recruitment** — larval overwintering under pack ice, ice-algal feeding |
| leopard seals | *Hydrurga leptonyx* | pack-ice associated |
| flying birds | coastal/small/med/large flying birds | mostly ice-independent; snow petrel is the exception and is not separable |

**The aggregation problem is worst exactly where the motivating literature points.**
Small divers mixes an ice-associated species with an ice-avoiding one, so a single
group-level forcing imposes one sign on a group whose members disagree and is
partly fitting a cancellation. Medium divers is the better-composed target — 4 of 8
taxa are genuine ice-obligate breeders — but it is diluted with sub-Antarctic and
pelagic taxa.

**This is a limitation to report, not to solve.** It is a consequence of the 19-group
aggregation and cannot be fixed inside this work. The honest framing is that the
forcing represents the *ice-dependent fraction* of each group, and the coupling
strength absorbs the dilution — which is one more reason it cannot be interpreted as
a species-level parameter.

### 4.2 Size structure — what can actually be targeted (verified)

| group | w_min → w_mat | **juvenile bins** | % numbers | % biomass | age_mat |
|---|---|---|---|---|---|
| **antarctic krill** | — | **28** | 100 | 0.2 | 0.38 yr |
| other krill | — | 24 | 100 | 1.3 | 0.36 yr |
| **flying birds** | 39 → 1,719 g | 11 | 83.6 | 5.5 | 4.75 yr |
| **medium divers** | 8,685 → 294,750 g | 10 | 95.3 | 8.1 | 7.71 yr |
| large divers | 108,549 → 1,821,600 g | 8 | 42.4 | 2.0 | 22.8 yr |
| leopard seals | 155,712 → 348,000 g | 3 | 25.6 | 8.5 | 34.4 yr |
| **small divers** | 2,942 → 4,267 g | **2** | 13.4 | 5.4 | 9.54 yr |

**Small divers has two juvenile bins out of thirty occupied.** A size-banded mortality
there is a two-bin spike indistinguishable from a flat multiplier on 13% of the
numbers. **Small divers is excluded from the survival pathway.** That is a consequence
of the known `w_min` defect (offspring at half adult mass, and the resulting
implausible 9.54 yr age at maturity), not of this design.

### 4.3 The pre-fledging problem

The best-documented low-ice mortality is **pre-fledging**: fast ice breaks out early,
chicks enter the water before they have waterproof plumage, and the colony fails.
In this model `w_min` for the air-breathing groups is deliberately **weaning/fledging
mass** — phase 100 measured that moving it to birth mass is worse (max `erepro`
21.8 → 41.1) because mizer has no parental care. So those individuals are, by
construction, below the smallest represented size.

**Therefore: breeding failure is a recruitment term (§3.1), never a mortality term.**
The survival pathway (§3.2) represents a genuinely different process — post-fledging
first-year survival — and the two must not be conflated in the write-up.

---

## 5. The compensation check — which pathway is viable for whom

A recruitment perturbation is buffered by Beverton–Holt. To first order the fraction
of a proportional `RDI` change reaching `RDD` is `1 − reproduction_level`
**(verified on the reference)**:

| group | reproduction level | % of an RDI cut reaching RDD |
|---|---|---|
| **antarctic krill** | 0.582 | **41.8** |
| small divers | 0.690 | 31.0 |
| flying birds | 0.745 | 25.5 |
| mesopelagic fishes | 0.763 | 23.7 |
| other krill | 0.790 | 21.0 |
| leopard seals | 0.803 | 19.7 |
| **medium divers** | **0.894** | **10.6** |
| baleen whales | 0.901 | 9.9 |

**This is the single most decisive result for the design.**

- **Krill transmits 42% of a recruitment perturbation** — the pathway works.
- **Medium divers absorbs 89% of one.** A recruitment forcing on the ice-obligate
  predator group would be almost entirely compensated away. For that group the
  *survival* pathway is the one that will register.

So the biologically natural split and the mechanically viable split agree:

| group | pathway | hook |
|---|---|---|
| **antarctic krill** | recruitment | `RDI` multiplier |
| **medium divers** | post-fledging survival | banded `other_mort` |
| flying birds | survival (optional arm) | banded `other_mort` |
| small divers | **recruitment only** (2 bins bar the survival route) | `RDI` multiplier |
| leopard seals | either, weakly (3 bins, 19.7%) | optional arm |

**Caveat on the elasticity.** `1 − L` is the instantaneous transmission at the current
state. Under a *sustained* press the stock falls, `RDI` falls, the reproduction level
falls and transmission rises — so these figures are a **lower bound for sustained
forcing** and roughly correct for single-year pulses. Note also that whale `RECAP` is
already drawn `U(0.1, 0.9)` per member, so for any whale group the transmission is a
member-level random variable, not a constant.

**Priority recommendation: build krill first.** It is the strongest documented ice
link in the Southern Ocean, it has 28 juvenile bins and the highest transmission of
any group, it is the model's central node, and every predator response then arrives
*endogenously* rather than being imposed twice. The predator pathways are the second
increment and should be read against a krill-only arm.

---

## 6. The forcing index

### 6.1 Data

`siconc` (sea ice area fraction, %) **is** part of the ISIMIP3a ocean input set for
GFDL-MOM6-COBALT2, monthly, at both 0.25° and 1°, remapped from JRA-55 reanalysis
ice cover **(verified against the ISIMIP/FishMIP documentation)**. Both `obsclim` and
`ctrlclim` are available.

Two acquisition routes, in preference order:

1. **The FishMIP Input Explorer** (<https://rstudio.global-ecosystem-model.cloud.edu.au/shiny/FishMIP_Input_Explorer/>)
   serves inputs already clipped to FishMIP regional model boundaries, which is
   exactly the Prydz Bay polygon this project uses. This should give a
   `gfdl-mom6-cobalt2_obsclim_siconc_15arcmin_prydz-bay_monthly_1961_2010.csv` in the
   same shape as the six files already in `FishMIP_Plankton_Forcing/`.
2. Failing that, DKRZ levante
   `/work/bb0820/ISIMIP/ISIMIP3a/InputData/climate/ocean/obsclim/global/monthly/historical/GFDL-MOM6-COBALT2/`
   and clip locally with the same domain mask used for `tos`.

**Pull `ctrlclim` as well as `obsclim`** — the spin-up is ctrlclim 1961–1980 ×6, so
the ice array must be built the same way or the pre-1961 period is inconsistent with
temperature and plankton.

### 6.2 Construction

Two indices, because the two pathways care about different things:

- **`ice_winter`** — mean `siconc` over the winter advance/maximum months
  (proposed: **June–September**), annual. This is the krill larval overwintering
  index and the pack-ice breeding-habitat index.
- **`ice_spring`** — mean `siconc` over the fast-ice break-out window (proposed:
  **November–December**), annual, plus a **break-out-early flag** for years falling
  below a percentile threshold. This is the emperor/Weddell breeding-failure index
  and is the one that should drive episodic events rather than a smooth response.

Both are converted to a **multiplicative anomaly, normalised to mean 1 over
1961–2010**, so that a neutral arm is exactly 1 and the historical run is unchanged.

**Decision required (§11):** monthly windows, and whether the response is linear in
the anomaly or threshold-triggered. The literature on breeding failure is
threshold-like (colonies fail or they don't); krill recruitment is closer to
continuous.

### 6.3 The response function

Proposed, deliberately simple and with one free parameter per pathway:

```
recruitment:  RDI_i(t) *= max(0, 1 + k_repro,i * (ice_winter(t) - 1))
survival:     mu_extra,i(w, t) = k_mort,i * max(0, 1 - ice_index(t)) * band_i(w)
```

`band_i(w)` is 1 between `w_min` and `w_mat` and 0 elsewhere. `k` is the coupling
strength, and **`k` cannot be fitted** — see §10. It is swept.

---

## 7. Scenario design

ISIMIP3a is historical only, so the forward arms are explicitly **bespoke**. The
historical arms remain protocol-compliant and are the anchor.

### 7.1 Historical (ISIMIP3a-aligned, 1841–2010)

| arm | ice forcing | purpose |
|---|---|---|
| **H0 control** | none (component absent) | **not optional** — must reproduce the current run bit-for-bit |
| **H1 neutral** | applied, index pinned to 1.0 | proves the plumbing is inert; any difference from H0 is a bug |
| **H2 obsclim** | observed `siconc` 1961–2010, ctrlclim cycle before | the attribution run: how much of the historical trajectory does ice explain? |
| **H3 ctrlclim** | ctrlclim throughout (no ice trend) | the detection counterpart to H2, mirroring the protocol's obsclim/ctrlclim pairing |

H2 vs H3 is a genuine ISIMIP3a-style detection-and-attribution contrast and is the
part of this work that is protocol-aligned rather than bespoke.

### 7.2 Forward (bespoke, 2011–2100)

Built on `94_future_projection.R`'s machinery — explicit array extension, repeated
ENSO cycle, effort arms including the `unfished` control that supplies the
contemporaneous denominator.

| arm | ice trajectory | hypothesis it tests |
|---|---|---|
| **F0 stable** | repeat the 1991–2010 ice cycle | the no-ice-change control |
| **F1 decline** | linear trend to a stated fraction of the 1991–2010 mean by 2100 | does a *mean* decline matter? |
| **F2 episodic** | stable mean, but extreme-low years injected at increasing frequency | do *episodic breeding failures* dominate the mean trend? |
| **F3 decline + episodic** | both | are they additive or does one saturate? |

**F2 is the scientifically interesting arm** and the one the recent literature
motivates: 2022 and 2023 were not a trend, they were extreme years. A model in which
the mean decline matters more than the extremes would be a real, reportable result —
as would the reverse.

Crossed with a **coupling sweep** `k ∈ {weak, central, strong}` and, at minimum, the
`unfished` and `hold` effort arms.

### 7.3 The hypotheses, stated so they can be refuted

- **H-A**: ice acting through *krill recruitment* moves predators more than ice
  acting *directly* on predator habitat. (Compare a krill-only arm against a
  predator-only arm.)
- **H-B**: episodic extreme-low years (F2) dominate the mean decline (F1) for
  ice-obligate predators, because breeding failure is threshold-like.
- **H-C**: ice-obligate predators decline even when krill is held stable — i.e. the
  direct habitat pathway is not redundant with the trophic one.

H-A and H-C together are the double-counting test. If a predator-only arm and a
krill-only arm produce the same predator response, the model cannot separate them and
only one should be retained.

---

## 8. Implementation phases

Following the project convention, one script per phase in `R/wmin_test/`, each with a
header stating protocol, what changed, and why. Next free numbers are 105+.

| phase | script | does |
|---|---|---|
| **105** | `105_ice_forcing_data.R` | acquire `siconc` (obsclim + ctrlclim), clip to domain, build `ice_winter` / `ice_spring` annual series to 2010, write `Output_large_files/wmin_test/105_ice_index.rds`. Asserts year coverage and the ctrlclim ×6 spin-up construction. Pure data, no model. |
| **106** | `106_ice_hooks.R` | define and unit-test `iceRDI` and `iceMort`. **No ensemble.** Tests: neutral forcing is bit-identical to no forcing; a named-vector multiplier touches only the named species; `ext_encounter` survives; the band covers exactly the intended bins; `other_mort` returns a conformable array. |
| **107** | `107_ice_pilot.R` | 10–20 members, arms H0/H1/H2 plus one forward arm, krill pathway only. Answers: does the plumbing hold at scale, and is the krill response detectable above member spread? |
| **108** | `108_ice_scenarios.R` | the full crossed design (§7) on the 203-member usable set, or a ranked subset if runtime demands. |

**Runtime anchor**: phase 104 cost ~22 core-minutes per member for a build; these are
*projections from stored states*, which are far cheaper — phase 94's 1841–2100 runs
are minutes per member. But the design is `arms × k × effort × members`, which
multiplies fast. Size the pilot before committing; leave two cores free.

---

## 9. Verification — the acceptance tests

Non-negotiable, in order:

1. **H0 reproduces the current run bit-for-bit.** Same `all.equal(tolerance = 0)`
   standard used for the ISIMIP spin-up verification.
2. **H1 (neutral forcing) equals H0.** If applying an identity forcing changes
   anything, the hook is wrong. This is the test that catches the therMizer year-wrap.
3. **The ice array has the same years, length and row names as `ocean_temp`**, asserted
   in-script, because `plankton_forcing()` indexes off `ocean_temp`'s dimnames.
4. **Only the named species are affected.** Project one arm and confirm the untreated
   16 groups are unchanged to machine precision. (`species_params<-` is not used here,
   but assert `ext_encounter` survives anyway — it has bitten this project repeatedly.)
5. **The component does not leak into calibration.** Confirm `other_mort` is empty on
   the stored member states and is attached only at projection time.
6. **Transmission sanity.** A 50% `RDI` cut on krill for one year should move krill
   `RDD` by ≈ 42% of that, per §5. If it doesn't, the hook is in the wrong place.
7. **Drift.** Run the 200-year zero-effort drift screen with the neutral forcing
   attached; it must pass exactly as it does without.

---

## 10. What this cannot do — to be stated in the methods

- **The coupling strength `k` cannot be fitted.** The yield objective contains no
  penguin, seal or krill-recruitment observation that constrains it, and the same
  logic that made `RECAP` an unconstrained propagated uncertainty applies here. `k` is
  **swept, and reported as a scenario axis, never as an estimated parameter.** Do not
  let a ranking select on it and then describe it as calibrated.
- **The group aggregation limits interpretation.** Medium divers is 4 ice-obligate
  taxa of 8; small divers mixes opposite signs. Results are statements about
  *ice-dependent fractions of aggregate groups*, not about emperor penguins.
- **Pre-fledging mortality is outside the model domain** and is represented as
  recruitment, not survival. Say so explicitly rather than letting a reader assume
  chicks are in the size spectrum.
- **Small divers cannot carry a survival pathway** (2 juvenile bins).
- **The forward arms are not ISIMIP-protocol projections.** ISIMIP3a is historical.
  The protocol-aligned route to a future forcing is ISIMIP3b's scenario-driven ocean
  output (SSP-based), which this project has not yet ingested and whose availability
  and variable list should be confirmed before it is promised in a manuscript. Until
  then F0–F3 are **bespoke hypothesis-testing scenarios** and must be labelled as such.
- **Double counting is a live risk**, and H-A/H-C in §7.3 are the test for it, not a
  guarantee against it.

---

## 11. Decisions needed before phase 105

1. **Scope of the first increment.** Recommended: krill recruitment only. Predator
   pathways in a second increment, read against a krill-only arm. Confirm.
2. **The two index windows** — June–September for `ice_winter`, November–December for
   `ice_spring`. These are proposals and should be checked against the Prydz Bay ice
   climatology rather than adopted from general Antarctic seasonality.
3. **Linear or threshold response**, per pathway. Recommended: continuous for krill
   recruitment, threshold for predator breeding failure.
4. **The coupling sweep range.** There is no empirical anchor, so the defensible
   choice is a range wide enough to bracket "no effect" and "the group tracks ice
   one-for-one", reported as a sensitivity envelope.
5. **Which ensemble.** The 203-member phase-104 usable set, or a ranked subset for
   the pilot. Note the mid-trophic prior is already conditioned by the stability
   screen, so krill abundance across members is not a fair prior sample.
6. **Whether leopard seals and flying birds get arms at all**, given 3 and 11
   juvenile bins and weak ice association for flying birds.

---

## Related documents

- `docs/monte_carlo_workflow_review.md` — the phase 59→104 chain, the member protocol,
  and why `RECAP` is reported as propagated rather than fitted (the same argument
  applies to `k`)
- `docs/mortality_targets.md` — how external mortality is set and why `z_ext = 0`
- `R/wmin_test/94_future_projection.R` — the projection machinery and the therMizer
  forcing-wrap trap
- `R/wmin_test/99_convergence_audit.R` — the ENSO-cycle forcing construction
- FishMIP ISIMIP3a protocol: <https://github.com/Fish-MIP/FishMIP2.0_ISIMIP3a>
