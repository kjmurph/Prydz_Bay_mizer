# Brief: test the impact of correcting `small divers` w_min

Prydz Bay mizer model. Hand this to a fresh Claude Code session working from the
repository root.

---

## 1. What was found

The `small divers` group (penguins) has **`w_min = 0.001 g`** in the parameter set
used by the 2,111-member Monte Carlo ensemble. An earlier parameter generation
had **`w_min = 3626.667 g`** (3.63 kg, exactly 0.85 x `w_mat`).

Three independent lines of evidence that 0.001 g is an error, not a modelling
choice:

1. **It is the only material w_min change between parameter generations.**
   Diffing `params/params_16_March_2023.rds` against
   `Manuscript data/params_sel_adj.rds` over the 14 groups common to both, only
   `small divers` (factor 2.8e-7) and `leopard seals` (factor 0.75, a plausible
   refinement) changed at all. Every other group is bit-identical.
2. **Size span.** `log10(w_max / w_min)` for the nine endotherm groups is
   0.35–2.11 decades, except `small divers` at **6.78** — more than 3x the next
   widest, and squarely inside the ectotherm range (5.0–6.8). Penguins currently
   span like a fish.
3. **0.001 g is exactly the `mesopelagic fishes` w_min** — a fish egg size.

Across the 46 objects in `params/`, 24 carry 0.001 g (all therMizer-era) and 9
carry 3626.667 g (the earlier 15–16 species model). `w_min` is identical across
all 212 exploited and all 212 unexploited simulation objects — verify this
yourself rather than trusting it.

**Current magnitude of the artefact** (2001–2010 reference period, unexploited
ensemble median): penguins hold **0.0001% of their biomass** and **1.43% of their
individuals** below 1 g (5.96% below 100 g). So it is a numerically thin tail,
not a hidden biomass pool. That is why this needs testing rather than assuming.

---

## 2. The question

Does correcting `small divers` w_min from 0.001 g to 3626.667 g materially change
any result the manuscript relies on?

If the answer is "no", the fix can be scoped down — see section 6.

**Before starting, raise one thing with Kieran:** 3626.667 g is exactly
`0.85 x w_mat`, which is an unusual rule for a minimum size. A true egg/hatchling
mass for an Adélie penguin is nearer 100 g. Confirm whether the target value is
the historical 3626.667 g (restoring the earlier parameterisation) or a
biologically derived egg size. The test design is the same either way, but the
number matters.

---

## 3. Ground rules

- **Do not modify or overwrite the existing ensembles or caches.** Every new run
  writes to a clearly separate path (suggest `Output_large_files/wmin_test/`).
  The existing outputs are the control arm.
- **Exploited and unexploited must be re-run as matched pairs** with identical
  parameters and climate forcing. The entire analysis rests on that pairing; if
  you re-run one arm only, every paired statistic is meaningless.
- **The spin-up must be re-run**, not reused. `w_min` changes the steady state,
  so it feeds into the stability rejection criteria.
- Report negative results as clearly as positive ones. "No detectable difference"
  is the outcome that unlocks the cheap fix, and it needs to be trustworthy.

---

## 4. Staged test design

Do not start by re-running 2,111 members. Work up in cost.

### Stage 0 — analytic bound (no simulation)

Quantify what is actually at stake before spending compute.

- Fraction of penguin **biomass** and **abundance** below 3626.667 g, per member,
  both scenarios. (Already known at the ensemble median: 0.0001% biomass /
  1.43% individuals below 1 g — extend this to the actual proposed threshold,
  which is far higher and will capture much more.)
- **Predation actually exerted** by sub-threshold penguins: their share of total
  consumption on Antarctic krill and on the fish groups. This is the causal
  pathway for any cascade, and it is the number that matters most. Penguins below
  3.63 kg feed on a much smaller prey size range than adults, so their krill
  consumption share may be far from negligible even if their biomass share is.
- Their share of predation mortality *on* them from higher predators.

If sub-threshold penguins account for a substantial share of krill consumption,
Stage 1 is mandatory and a full re-run becomes likely. If they account for
essentially none, the later stages should confirm negligible change.

### Stage 1 — pilot, 20–30 members

Re-run a random but reproducible subset (set a seed, record the indices) of the
accepted parameter sets with the corrected `w_min`, both scenarios, full spin-up
and projection. Compare against the same members in the existing ensembles.

### Stage 2 — the 212 fitted members

Only if Stage 1 shows non-negligible change, or to confirm a negligible result at
the resolution the manuscript actually uses.

### Stage 3 — full 2,111

Only if Stage 2 shows the accepted set or the RMSE ranking shifts (see the
warning in section 6).

---

## 5. Outputs to compare

For every metric: report the paired per-member difference (corrected minus
original), not just ensemble summaries. Give median, IQR, and the fraction of
members whose difference exceeds a stated threshold. State the threshold you
used and why.

| # | Output | Where the existing version comes from |
|---|---|---|
| 1 | Total community biomass, annual | `Manuscript data/biomass_top10pct_raw_fish.rds` / `_clim.rds` (cols `Year, Species, Biomass, sim_i`) |
| 2 | Biomass variability | rolling SD of (1); see `var_tables()` in `Plotting scripts/biomass_slope_snr_mean_med.R` |
| 3 | Size-spectrum slope lambda, annual | `Manuscript data/nbss_slope_top10pct_data.rds` `$all_slopes` |
| 4 | Slope variability | rolling SD of (3), same `var_tables()` |
| 5 | Antarctic krill consumption by **baleen whales** | `whale_consumption_outputs/fishing_baleen_krill_all_sims.rds`, `climate_only_baleen_krill_all_sims.rds` |
| 6 | Antarctic krill consumption by **fishes** | `whale_consumption_outputs/fishing_fish_krill_all_sims.rds`, `climate_only_fish_krill_all_sims.rds` |
| 7 | Biomass-stability rejection in spin-up | `check_biomass_stability_enhanced()`, defined at `09_Uncertainty_Analysis.Rmd:992`, called at ~line 165 with `spinup_years = 118`, `enhanced_steady_tol = 0.002` |
| 8 | RMSE of modelled vs observed catch | `yield_rmse_evaluation.R` (writes `yield_rmse_per_sim.csv`); observed catch in `Manuscript data/yield_cached_obs_stack.rds` |

Add, because they feed Figure 1 directly:

| 9 | Community size spectrum, 2001–2010 mean | `Manuscript data/spectra_cache_ref_period.rds` (`w_bins`, `fished_spectra`, `climate_spectra`, each [2111 x 100]) |
| 10 | Per-group spectra | `Manuscript data/spectra_cache_species_ref_period.rds` (`sp_names`, `fished_sp`, `climate_sp`) |

Also re-check the two headline paired statistics:

- Terminal-year (2010) paired difference in lambda: currently median −0.01404,
  100% of 212 pairs negative.
- Year each metric first reaches **and holds** 100% sign consistency: currently
  **1931** for total biomass, **1974** for lambda. Series in
  `output/results/sign_consistency_series_1841_2010.csv`, regenerable via
  `R/figures/fig1/sign_consistency_emergence.R`.

---

## 6. Decision framework

The three options, and what each actually requires:

**(a) Full re-run of all 2,111.** Necessary if the corrected `w_min` changes
which members pass the stability rejection, or materially reorders the RMSE
ranking.

> **Warning that must not be skipped.** "Re-run only the 212" quietly assumes the
> accepted set and the RMSE ranking are unchanged. Both depend on `w_min`: the
> stability check runs on the spin-up, and RMSE depends on modelled catch. So
> Stage 2 must explicitly verify that (i) all 212 still pass the stability
> criteria under the corrected parameter, and (ii) the top-10% membership by
> RMSE is unchanged, or changes only at the margin. If either shifts, the fitted
> ensemble is no longer the same set of members and the shortcut is invalid.

**(b) Re-run only the 212 fitted members.** Valid only if (a)'s two conditions
hold. Note this still changes every paired statistic slightly, so all Figure 1
and results-figure numbers need regenerating.

**(c) Crop the `small divers` spectrum below 3626.667 g in post-processing.**

> **Be clear about what this does and does not do.** Cropping is *presentational
> only*. It hides sub-threshold penguins from plots; it does not undo the
> predation they exerted, the prey they removed, or their contribution to
> competition during the simulation. It is defensible **only if** Stage 0/1 show
> that sub-threshold penguins exert negligible trophic effect — i.e. the
> dynamics are effectively unchanged and the artefact is purely cosmetic. If they
> do exert real predation, cropping conceals a genuine model error rather than
> fixing it, and should not be used.

Recommend one option with reasons, and state what would change the recommendation.

---

## 7. Repository gotchas — read before writing code

These have already cost time. They are not obvious.

- **mizer version.** Every saved `MizerSim` was written by **mizer 2.5.0**; the
  installed version is **3.1.0**. `getCommunitySlope()` and anything else routing
  through `valid_species_arg()` fails with `invalid argument type` because 2.5.0
  objects lack `species_params$is_background`. Shim before calling:
  ```r
  if (is.null(sim@params@species_params$is_background))
    sim@params@species_params$is_background <- FALSE
  ```
  Verified a no-op on results: slopes recomputed under the shim are bit-identical
  to the stored values. Already applied in `run_community_slope_full.R`.

- **Two incompatible simulation keys.**
  | object | column | meaning | range |
  |---|---|---|---|
  | biomass caches | `sim_i` | position in RMSE-rank order | 1…212 |
  | `nbss_slope_top10pct_data` | `sim_id` | original accepted-ensemble index | 24…2111 |
  | spectra caches | row index | original accepted-ensemble index | 1…2111 |

  Map: `top10pct_idx[sim_i] == sim_id`. Joining on the bare number silently
  mispairs every member.

- **Terminal years differ.** Exploited runs end **2010**, unexploited end
  **2011**. Always filter to 2010 or the unexploited arm gains a phantom year.

- **Fitted-ensemble membership is derived, not flagged.** Sort
  `Manuscript data/yield_rmse_per_sim.csv` by `rank`, take the first
  `ceiling(2111 * 0.10) = 212` values of `sim_index`.

- **Canonical slope estimator** is LBNbiom (Edwards et al. 2017 Method 5):
  `calculate_spectrum_lbnbiom_slope()` at `ecosystem_assessment_v3.R:493`, with
  `spectrum_min_w = 3.16227766e-08`. Do not substitute a plain OLS.

- **Canonical SNR** is `make_snr()` in
  `Plotting scripts/biomass_slope_snr_mean_med.R` (~line 89). Its denominator is
  the SD **across years 1841–2010 of the across-member mean unexploited
  trajectory** — a temporal SD of one curve, one scalar per metric. It is **not**
  a rolling SD; the 3/6/9/12/15yr windows apply only to the *variability* panels,
  and the level panels are window-independent. At 2010 it gives biomass −0.920
  and lambda −10.404. Reproduce those before trusting any new SNR number.

- **`Output_large_files/community_slope_analysis/community_slope_full_2111_data.rds`**
  had corrupted years in its `Abundance` rows (a trailing digit appended by
  `rbind` row-name disambiguation). Fixed and regenerated 2026-07-27; the
  pre-fix copy is kept as `*.CORRUPT_BACKUP_20260727.rds`. The Biomass half was
  always fine. `nbss_slope_top10pct_data.rds` is Biomass-only and was never
  affected. `test_community_slope_5members_data.rds` still carries the same bug.

- **Paired runs are identical to ~1e-15 relative before 1930** and first diverge
  in 1931; first observed catch is 1930. Any new run should reproduce this — if
  it does not, the pairing is broken.

- **Rscript on this machine:** `C:\Program Files\R\R-4.6.0\bin\Rscript.exe`.
  R 4.5.1 is present but has no working `bin`.

- Loading either full ensemble takes ~2–4 minutes and several GB
  (`mc_ensemble_2111_cleaned.rds` and `climate_only_ensemble_compiled.rds` are
  1.86 GB each). The 212-member paired object
  (`Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds`,
  375 MB) is usually enough and much faster.

---

## 8. Deliverable

A short written comparison covering all ten outputs in section 5, each with the
paired per-member difference and an explicit verdict against a stated threshold;
a recommendation among options (a), (b), (c) with reasoning; and any scripts
written left in the repository so the test is reproducible. Do not change any
manuscript figure until the recommendation is agreed.
