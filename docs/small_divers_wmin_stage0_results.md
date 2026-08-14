# `small divers` w_min test — Stage 0 results

Response to `docs/small_divers_wmin_test_brief.md`, worked from section 3.
Date: 2026-07-28. All scripts in `R/wmin_test/`, all outputs in
`Output_large_files/wmin_test/`. Nothing existing was modified or overwritten.

**Bottom line.** Sub-threshold penguins are a real part of the penguin group —
16% of its individuals, 2.2% of its biomass, and the target of a third of the
predation events on penguins — but they exert **≤0.03% of community-level
consumption of Antarctic krill and of the fish groups in every one of the 212
members, in both scenarios**. The causal pathway for a cascade is
therefore absent.

> **Stage 1 has since been run on the top 5 members —
> `docs/small_divers_wmin_stage1_results.md`.** It confirms the trophic finding
> above but adds a decisive one: the correction cannot be applied as a parameter
> edit. Recalibration is mandatory, and even after it penguins sit at 0.15% of
> their calibrated biomass with density-dependent recruitment removed. Read the
> two documents together; where they differ, Stage 1 supersedes.

---

## 1. Verification of the brief's section 1

Every claim checked rather than trusted (`00_verify_wmin.R`,
`00d_params_dir_scan.R`, `04_check_index_mapping.R`).

| Claim | Verdict |
|---|---|
| `small divers` w_min = 0.001 g in the ensemble parameter set | **Confirmed** (`Manuscript data/params_sel_adj.rds`) |
| Earlier generation had 3626.667 g | **Confirmed** (`params/params_16_March_2023.rds`) |
| w_min identical across all 212 exploited and all 212 unexploited sims | **Confirmed** — 1 unique w_min row per arm, and the two arms are identical |
| Only `small divers` and `leopard seals` changed between generations | **Confirmed in substance**, see caveat below |
| `small divers` size span 6.78 decades; next widest endotherm 2.11 | **Confirmed** (`medium divers` 2.11; ectotherm range 5.00–6.76) |
| 0.001 g is exactly the `mesopelagic fishes` w_min | **Confirmed** |
| Across the 46 objects in `params/`, 24 carry 0.001 g and 9 carry 3626.667 g | **Confirmed exactly** |

**Caveat on "bit-identical".** Four groups (`other krill`,
`other macrozooplankton`, `squids`, `toothfishes`) differ between the two
parameter generations by ≤4.3e-8 relative — floating-point noise from
re-derivation, not a parameter change. Only `small divers` (×2.76e-7) and
`leopard seals` (×0.751) changed materially. The brief's conclusion stands; the
word "bit-identical" is very slightly overstated.

**A detail that bears on the section 2 question.** 3626.667 g is exactly
0.85 × 4266.667 g, the **old** `w_mat`. The current `w_mat` is 4267 g, so
0.85 × current `w_mat` = 3626.95 g. The figure 3626.667 is therefore a literal
restoration of the historical number, not a rule re-evaluated against current
parameters.

**Index mapping confirmed** (the brief's "two incompatible keys" gotcha): for
`top10pct_rmse_ensembles.rds`, position *m* ↔ biomass-cache `sim_i` = *m* ↔
original ensemble index `top10pct_indices[m]`, verified by recomputing biomass
from the sim objects and matching the cache to a relative difference of 0 across
all 19 groups. The membership derived from `yield_rmse_per_sim.csv` by
`rank ≤ 212` is `identical()` to the stored metadata.

---

## 2. What had to be built first

`therMizer is not installed on this machine`, but `params@rates_funcs` names
`therMizerEncounter` and `therMizerPredRate`. `mizer::getDiet()` therefore fails
outright on these objects. `R/wmin_test/thermizer_shim.R` reimplements the
scaling. Three independent validations:

1. **Functional form pinned exactly.** The temperature effect is
   `T_K · (T_K − (temp_min+273)) · √((temp_max+273) − T_K)`, with `T_K` in
   Kelvin. Taking its maximum over a 0.1-degree grid reproduces the stored
   `species_params$encounterpred_scale` for all 19 groups with **relative error
   0**. Four other candidate forms were tested and rejected (next best is off by
   5.5e-4, the rest by ≥1.8). `00b_verify_temp_scale_formula.R`.
2. **Diet transcription bit-identical.** With the temperature effect set to 1,
   `ther_diet()` reproduces `mizer::getDiet(proportion = FALSE)` with **max
   relative difference 0** over the full 19×100×21 array.
   `03_transcription_check.R`.
3. **Internally consistent.** `rowSums(diet)` equals `(1 − f) × encounter` to
   3.6e-15 relative once the scaling is threaded through both.
   `02_shim_internal_check.R`.

**Why the size decomposition is robust.** `other_params$vertical_migration` is
**flat in body size** for every group (0.2 in each of 5 realms, `exposure` 1), so
the temperature effect is a *per-species scalar*, identical across all 100 size
bins (`00c_verify_vertical_migration_flat.R`; the shim asserts this at runtime
and stops if it ever stops being true). A per-species scalar cancels from any
within-species size share except through its bounded effect on the feeding
level. The Stage 0 headline numbers do not depend on getting the temperature
scaling right; the cross-species shares do, and those are pinned by (1).

**Independent check that the whole pipeline is right.** The brief states that at
the ensemble median penguins hold "0.0001% of their biomass and 1.43% of their
individuals" below 1 g. Recomputed from scratch here: **6.1e-5%** (which is
0.0001% to one significant figure) and **1.40%**. Reproduced.

---

## 3. Stage 0 results

Threshold 3626.667 g. 2001–2010 reference period, per-member means, all 212
fitted members, both scenarios. `05_stage0_analytic_bound.R`, reported as
**median [IQR] (max over members)**.

### (a) Size structure — the artefact is not numerically thin at this threshold

| Quantity | Exploited | Unexploited |
|---|---|---|
| Penguin **biomass** below threshold | 2.29% [1.25, 2.85] (max 19.4%) | 2.23% [1.24, 2.80] (max 19.4%) |
| Penguin **abundance** below threshold | 16.3% [9.3, 19.3] (max 71.4%) | 15.9% [9.2, 19.0] (max 71.4%) |

The brief's "numerically thin tail" framing was based on the 1 g threshold. At
the *actual proposed* threshold the tail is not thin: a sixth of all modelled
penguin individuals sit below it, and in 30 of 212 members more than a quarter
do. Bin-convention does not drive this — the whole-bin lower-edge rule gives
2.73%/16.8% against the prorated 2.23%/15.9%.

### (b) Predation actually exerted — the number that matters most

| Quantity | Exploited | Unexploited |
|---|---|---|
| Sub-threshold share of **penguin** krill consumption | 4.34% [2.27, 5.89] | 4.29% [2.20, 5.77] |
| Sub-threshold share of **penguin** fish consumption | 16.3% [9.3, 19.3] | 15.9% [9.2, 19.0] |
| Penguins' share of **community** krill consumption | 0.269% [0.20, 0.54] | 0.301% [0.21, 0.62] |
| **Sub-threshold penguins' share of community krill consumption** | **0.0114%** [0.0099, 0.0131] (max 0.0239%) | **0.0126%** [0.0107, 0.0150] (max 0.0285%) |
| **Sub-threshold penguins' share of community fish consumption** | **0.0168%** [0.0141, 0.0192] (max 0.0287%) | **0.0146%** [0.0123, 0.0173] (max 0.0246%) |

**Stated threshold for "material": 1% of community consumption of the prey
group.** Rationale: a trophic pathway carrying under 1% of the flux to a prey
group cannot plausibly move community biomass or the size-spectrum slope by
anything close to the manuscript's headline effects. Against that threshold:

> **0 of 212 members, in either scenario, reach even 0.1%.** The largest value
> anywhere in the ensemble is 0.029% — 35× below the stated threshold.

### (c) Predation mortality falling on penguins

| Quantity | Exploited | Unexploited |
|---|---|---|
| Share of penguin **numbers** killed that are sub-threshold | 33.2% [19.3, 40.5] | 33.2% [18.7, 40.7] |
| Share of penguin **biomass** killed that is sub-threshold | 3.19% [1.83, 4.08] | 3.14% [1.81, 4.06] |

A third of predation *events* on penguins fall on sub-threshold individuals, but
they carry only ~3% of the biomass flux. This is the clearest statement of the
artefact's character: it is numerically conspicuous and energetically trivial.

### Context: who actually eats the krill

`06_krill_predator_breakdown.csv` — median share of community Antarctic krill
consumption, unexploited, 2005: shelf and coastal fishes **52.2%**, mesopelagic
fishes **18.6%**, bathypelagic fishes **16.6%**, other macrozooplankton 3.7%,
squids 1.9%, small divers **0.47%**, baleen whales **0.0013%**.

> **Flagged separately, not a w_min issue.** In this model penguins consume
> **~210–230× more Antarctic krill than baleen whales do** (median over 212
> members, both arms), and baleen whales take 81% of their diet from the forced
> Resource spectrum rather than the explicit krill group. That may be exactly as
> intended, but given the manuscript's whale/krill framing it is worth a
> deliberate look. See also section 6.

---

## 4. Robustness

**The reference period is representative** (`06_stage0_robustness.R`). Computed
for all 170 years × 212 members × both arms, the sub-threshold shares are flat
across the whole record — abundance 15.2% (1841–1900) rising only to 15.8%
(1991–2010); biomass 2.10% → 2.23%. Whaling-era dynamics do not open a window
where the artefact matters more. On a 30-member decadal subsample, sub-threshold
penguins' share of community krill consumption never exceeds 0.038% in any
member-year across 1850–2010.

**The section 2 question is de-risked** (`07_threshold_sweep.R`). Because the
target value is unresolved, the whole calculation was swept over candidate
thresholds so the answer is already available either way:

| Threshold (g) | Penguin abundance below | Penguin biomass below | Sub-thr. share of **community** krill consumption |
|---|---|---|---|
| 1 | 1.40% | 0.00006% | 0.0000000168% |
| 100 (≈ Adélie egg mass) | 5.58% | 0.021% | 0.000134% |
| 1000 | 10.7% | 0.38% | 0.00316% |
| 3626.667 (historical) | 15.9% | 2.23% | 0.0127% |

The community-level trophic effect stays four to ten orders of magnitude below
the 1% threshold at *every* candidate value. **The Stage 0 verdict does not
depend on how the section 2 question is answered** — though the size-structure
numbers, and hence how visible the artefact is in any penguin-specific figure,
change by roughly 3× between 100 g and 3626.667 g.

---

## 5. Verdict and recommendation

**Stage 0 verdict: negligible trophic effect, confirmed at community level, with
a non-negligible effect inside the penguin group itself.**

Per the brief's section 4, "if they account for essentially none, the later
stages should confirm negligible change" — that is the branch we are on.

**Recommended option: (c), crop in post-processing — with two conditions that
must be stated in the manuscript, not assumed.**

Reasons:

- The brief makes (c) defensible precisely when sub-threshold penguins exert
  negligible trophic effect. They do: ≤0.03% of community krill and fish
  consumption in 424 member-scenario combinations, against a stated 1% bar.
- The pathway to any of outputs 1–6 and 9 (community biomass, its variability,
  λ, its variability, krill consumption by whales and by fishes, the community
  spectrum) runs through that flux. There is no mechanism by which 1 part in
  ~8,000 of krill offtake reorders them.
- Options (a) and (b) both require re-running with a corrected parameter, which
  is currently blocked (section 6) and would cost the entire ensemble's compute
  to chase an effect bounded here at 0.03%.

**Condition 1 — cropping cannot repair any penguin-specific result.** Output 10
(per-group spectra, `spectra_cache_species_ref_period.rds`) is the exception.
16% of modelled penguin individuals lie below the threshold; cropping removes
them from the plot but the *remaining* spectrum was still produced by a model in
which recruits entered at 0.001 g and grew up through that range. Any figure or
statement about the penguin group's own size structure or abundance must either
be re-run with the corrected parameter or carry an explicit caveat. Cropping is
honest for the community panels; for the penguin panel it would conceal the
error rather than bound it.

**Condition 2 — correcting is not deleting.** Raising `w_min` to 3626.667 g does
not remove the sub-threshold tail and leave everything else fixed; it changes
where recruits enter, and therefore penguin growth, reproduction and the whole
group's spectrum. Stage 0 bounds what the sub-threshold individuals *did*; it
does not simulate what the corrected model *would do*. That distinction is why
Stage 1 remains the confirmation step even though Stage 0 does not compel it.

**What would change this recommendation**

- Stage 1 showing any member changing stability-rejection status, or the top-10%
  RMSE membership shifting → (a), per the brief's un-skippable warning.
- Any manuscript claim resting on penguin abundance, penguin biomass, or the
  penguin size spectrum → (b), because condition 1 then binds on a load-bearing
  result rather than a supporting panel.
- The `pen_share_krill` figure being judged wrong on its own terms — if penguins
  should not be a 200× larger krill consumer than baleen whales, the diet
  structure is a bigger problem than `w_min` and supersedes this test.

---

## 6. Blockers and open questions

> **RESOLVED 2026-07-29 — this blocker was wrong. See
> `docs/small_divers_wmin_stage1_results.md`.** therMizer simply needed
> installing (`pak::pak("sizespectrum/therMizer")` → v1.0.0, exactly what the
> workflow Rmds use). My inference that the square-root term implied a
> *modified fork* was incorrect: upstream `therMizer::scaled_temp_effect`
> computes precisely that form. The advice below not to install upstream was
> wrong and cost time. With therMizer installed, re-projecting a stored member
> reproduces its trajectory to 3e-15 in biomass, and Stage 1 has been run.
> The Stage 0 numbers are unaffected — the temperature scaling used here matches
> the real package to 2.1e-16.

**1. ~~Stage 1 cannot be run on this machine.~~** Re-running requires
`therMizerEReproAndGrowth` and `plankton_forcing` in addition to the two rate
functions reimplemented here, plus the spin-up machinery in
`09_Uncertainty_Analysis.Rmd`. therMizer is not installed and not vendored in
the repo.

~~**Do not simply `install_github` the upstream package.**~~ The temperature
formula recovered from the stored `encounterpred_scale` carries a **square root**
on the upper-tolerance term — this is correct, and is also what upstream
therMizer does, so it carries no implication about provenance.

**2. The existing whale-consumption artefacts do not reconcile.** For the same
member and year, `whale_consumption_outputs/fishing_baleen_krill_all_sims.rds`
gives baleen krill consumption 13–80× larger than a faithful recomputation, and
the discrepancy varies with year, so it is not a units constant. The stored
series also span 10 orders of magnitude across members at a single year. The
member↔index mapping has been verified independently (section 1), so this is not
an indexing error on my side. These files are section 5's outputs #5 and #6 —
**they cannot serve as a control arm for Stage 1/2 until this is resolved.**
Diagnostics in `01_validate_shim.R`, `02_shim_internal_check.R`. Note the
generating script `extract_whale_consumption.R` calls `getDiet()` via
`getFeedingLevel()`, which defaults to `t = 0` and would index `ocean_temp` at
`0 + t_idx = -1841` — a plausible starting point for the investigation.

**3. The section 2 question still needs your answer**, though it no longer
blocks: is the target 3626.667 g (historical restoration) or a biologically
derived egg mass (~100 g)? Section 4 gives the Stage 0 answer for both. Note
that 3626.667 g is 0.85 × the *old* `w_mat`, so restoring it does not preserve
the 0.85 rule against the current `w_mat` of 4267 g.

---

## 7. Files

Scripts, `R/wmin_test/` — run from the repository root, in order:

| File | Purpose |
|---|---|
| `thermizer_shim.R` | temperature scaling, diet, predation mortality (sourced by the rest) |
| `00_verify_wmin.R` | section 1 claims: parameter generations, size spans, 212×2 w_min |
| `00b_verify_temp_scale_formula.R` | recovers the temperature formula from `encounterpred_scale` |
| `00c_verify_vertical_migration_flat.R` | proves the effect is a per-species scalar |
| `00d_params_dir_scan.R` | 46-object `params/` scan (case-insensitive) |
| `01_validate_shim.R` | legacy whale-consumption comparison (blocker 2) |
| `02_shim_internal_check.R` | internal consistency + legacy diagnosis |
| `03_transcription_check.R` | `ther_diet` vs `mizer::getDiet`, bit-identical |
| `04_check_index_mapping.R` | member ↔ `sim_i` ↔ original index |
| `05_stage0_analytic_bound.R` | **Stage 0 main result** (2.5 min) |
| `06_stage0_robustness.R` | all-years size structure, decadal subsample, krill breakdown |
| `07_threshold_sweep.R` | threshold sensitivity (section 2 de-risking) |
| `08_summary_stats.R` | the figures quoted in this document |

Outputs, `Output_large_files/wmin_test/` — per-member results in
`05_stage0_per_member.{rds,csv}`, per-member-per-year in
`05_stage0_per_member_year.rds`, plus the robustness and sweep objects and the
run logs. No existing file was modified.
