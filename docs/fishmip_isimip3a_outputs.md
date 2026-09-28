# FishMIP ISIMIP3a outputs — provenance

Written 2026-09-17. The FishMIP regional-model submission now tracks the
**phase-104 (q10) ensemble**. This note records what changed and why, so it is
not re-derived.

## Which tree to use

| tree | status |
|---|---|
| **`FishMIP_ISIMIP3a_submission_p104/`** | **CURRENT.** Phase-104 ensemble, 203 usable members, domain area 1.474341e+12 m². Built by `R/fishmip_isimip3a/FM01`–`FM06`. |
| `FishMIP_ISIMIP3a_submission/` | Superseded. December 2025. 2,111-member ensemble, and its densities are **13.23× too small** (divided by 1.95e+13 m², the climate-forcing grid *extent*). Kept so the earlier submission stays auditable. **Do not upload.** |
| root `fishmip_outputs/`, `fishmip_outputs_climate_only/`, `fishmip_outputs_species/` | Superseded. May 2026 re-run that fixed the area to 1.474341e+12 but still used the 2,111-member ensemble. The fix was never copied back into the bundle above. |

The root-level `extract_fishmip_outputs*.R` scripts and the two scripts inside
`FishMIP_ISIMIP3a_submission/` are the generators for the superseded trees.
They are unchanged and still read `mc_ensemble_2111_cleaned.rds`.

## What changed

1. **Ensemble.** From the 2,111-member Monte Carlo run to phase-104 arm q10:
   203 usable members (stable AND `drift_ok` AND `erepro < 1`), reference model
   `params_ref_p100_mort_kernel_diet.rds`, catchability re-fitted on the
   ISIMIP3a window ending 2004 with a `q ≤ 10` ceiling. The 2,111-ensemble's
   catchability fit used a window the project itself declared
   protocol-non-compliant (`docs/VM_RERUN_PLAN.md:5-20`), so the old
   `tc`/`tclog10`/`csp` rested on a retired calibration.

2. **Domain area.** **1.474341e+12 m² (1,474,341 km²)** — the Prydz Bay model
   domain polygon: the study box 60–90°E, 80–60°S unioned with Atlantis Box21
   and differenced against land and ice shelf, measured in a Lambert azimuthal
   equal-area projection (`model_domains/Stacey/BanzareBank.R`). `FM06_verify.R`
   reads the stored polygon `BanzareBank.rds`, re-projects it and re-measures
   it, so the constant is verified rather than quoted (recomputes to
   1,474,341,307,111 m², ratio 1.0000002).

   **This is the definitive basis, and it is the only one that makes the
   reported number a physical quantity.** The model's state variable *is*
   observed density × 1.474341e+12: densities in g m⁻² were multiplied by this
   area at model construction
   (`05_therMizer_calibration_scale_model_domain.Rmd:45-58`). Dividing by it is
   exactly invertible — it returns the density basis the model was calibrated
   and fitted to, over the region the model represents. Any other divisor would
   report grams-over-this-polygon per square metre of somewhere else.

   It also means these outputs agree exactly with the rest of the project: the
   manuscript (Methods §S1), `MA02_report.R`, the calibration targets and the
   consumption analyses are all on this same basis.

   > Guard against reopening this: the ISIMIP3a forcing extraction carries its
   > own `area_m2` field, which sums to a similar figure over its 4,191 cells.
   > It is **not** an alternative measurement of this domain — that grid is a
   > different footprint, overhanging ~3.1° north of the model polygon's −60°
   > edge while stopping ~10° short of its southern reach. It was considered
   > and rejected, and it appears nowhere in the scripts or outputs.

3. **`tcb` rule — reviewed, and left alone.** All 19 functional groups over the
   full modelled size range, with the prescribed plankton resource excluded.
   Every group is a consumer (TL > 1), invertebrates included. The resource is
   fitted to `phypico` + `phydiat` + `phydiaz` (TL = 1) together with `zmicro` +
   `zmeso` (`docs/manuscript_methods_supplement.md:425-455`), so including it
   would both add primary producers and double-count the explicit
   `mesozooplankton` group. `tcb > sum(tcblog10)` is expected and is asserted,
   because `tcblog10` starts at 1 g and `tcb` does not.

4. **New: a fisheries-only catch variant.** `tc` and `tclog10` recomputed with
   minke whales, orca, sperm whales and baleen whales removed, filed under
   `sens = "nowhales"`. This exists because **whaling is ~96–98% of the catch
   mass here** — 96.5% of the modelled `tc` and 97.8% of the recorded catch — so
   the full `tc` is not comparable with FishMIP's fisheries catch
   reconstruction. A matching `catchobs` variant is produced alongside it.

   > The `sens` slot is the closest fit in the ISIMIP3a vocabulary but is not a
   > registered token. **Confirm it with the FishMIP regional coordinators
   > before upload**; renaming is one line in `FM00_common.R`.

## The pipeline

`R/fishmip_isimip3a/`, run through `run_p104q10.R` (which carries the per-script
env block, so the ensemble cannot drift to a stale default):

```
Rscript run_p104q10.R R/fishmip_isimip3a/FM01_totals.R        # tcb tcblog10 tc tclog10 + nowhales
Rscript run_p104q10.R R/fishmip_isimip3a/FM02_species.R       # bsp csp
Rscript run_p104q10.R R/fishmip_isimip3a/FM03_ancillary.R     # catchobs effort
Rscript run_p104q10.R R/fishmip_isimip3a/FM04_netcdf.R        # NetCDF (needs ncdf4)
Rscript run_p104q10.R R/fishmip_isimip3a/FM05_stage_bundle.R  # bundle + README
Rscript run_p104q10.R R/fishmip_isimip3a/FM06_verify.R        # 82 assertions
```

Input is `Output_large_files/wmin_test/p104q10_sims/` — members already
projected 1841–2010 with catchability applied, from
`R/wmin_test/105_export_member_sims.R`. **Nothing re-projects, re-steadies or
re-applies a multiplier**, so none of the AGENTS.md ensemble traps apply. Total
runtime is roughly ten minutes.

`FM01`/`FM02` stream the `_chunks/` files, which hold both arms for 14 members
at a time, so peak memory is ~200 MB rather than the ~1 GB a collected arm file
expands to. The source is asserted against the ranking either way, and
`fm_member_source()` falls back to the collected files if the chunks are gone.

## Things that bit, and are now guarded

- **Windows MAX_PATH.** The repo sits 90 characters deep under OneDrive and the
  protocol filenames run to 88, so `submission_csv_split/<scenario>/
  experiment_1961_2010/<file>` reached **261 characters** and failed mid-write;
  `LongPathsEnabled` is 0 on this machine. The protocol filenames must not
  change, so the split directories are `split_csv/<scenario>/{spinup,experiment}`
  and the year ranges live in the README. `fm_check_paths()` now checks every
  intended path *before* writing any of them, and `fm_dir()` no longer swallows
  a failed `dir.create()`.

- **`getFMortGear()` allocates 490 MB per member** (170 × 19 gears × 19 species
  × 100 sizes). `getFMort()` returns the gear-summed array directly — verified
  identical (max abs difference 0) — and is what the pipeline uses.

- **Catch is computed, never assumed.** The `nat` arm's zero catch is computed
  and then asserted, unlike `extract_fishmip_outputs_climate_only.R:176-177`,
  which hardcoded the zeros.

## The check that matters most

`FM06` §6 asserts that `bsp × MODEL_DOMAIN_AREA` reproduces
`Manuscript data/biomass_abund_fish_p104q10.rds` at 1841 / 1950 / 2010 for all
19 groups (max relative difference 2.0e-13 over 11,571 values). If that ever
fails, the submission and the manuscript are describing different ensembles.
It is also the end-to-end proof that the area round-trips: the manuscript build
holds domain totals in grams, so an exact match means the divisor recovers the
model's own basis.

`FM06` §4 additionally ties `FM01` to `FM02`: `tc` equals the sum of `csp` over
all groups, and `tc − tc_nowhales` equals the whale groups' `csp`.

## Known caveats carried forward

- **`effort` is model-internal relative effort (0–1), not the protocol's
  `NomActive`** (kW × days at sea). Converting would need data the project does
  not hold.
- **No `ctrlclim` extraction exists** for this domain. The 1841–1960 spin-up
  repeats the 1961–1980 `obsclim` window six times (verified bit-for-bit) and
  the files are named `obsclim` accordingly. Same caveat as
  `docs/manuscript_methods_supplement.md:372-375`.
- **`bsp`, `csp`, `catchobs` and `effort` are not protocol vocabulary.** They
  are supporting detail, carried over from the previous submission.
- The ensemble-quantile columns (`median`, `q05`, `q25`, `q75`, `q95`) are not
  part of the protocol variable definition either. Take `median` as the protocol
  value; in the NetCDF that is `statistic = 1`.
