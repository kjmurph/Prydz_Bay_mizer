# Brief: why did the biomass SNR halve?

Hand this to a fresh session working from the repository root.

---

## The problem

Rebuilding the manuscript figures on the refined 183-member fitted ensemble
changed a headline number:

| quantity | published (212 members) | refined (183 members) |
|---|---|---|
| **biomass SNR at 2010** | **-0.920** | **-0.378** |
| slope SNR at 2010 | -10.404 | -10.24 (unchanged) |

Decomposed, the change is **entirely in the signal**:

| component | old | new | change |
|---|---|---|---|
| signal at 2010 (median exploited - unexploited community biomass) | -1.5728e12 | **-6.2172e11** | **2.53x smaller** |
| noise (temporal SD of the across-member mean unexploited trajectory) | 1.7089e12 | 1.6453e12 | 3.7%, negligible |
| unexploited drift 1841 -> 2010 | +2.65% | **+4.29%** | larger |

The slope SNR is essentially untouched, so this is specific to community biomass.

**It is NOT the w_min correction.** Stage C measured that at ~0.0007% on community
biomass and 0 of 1,997 members changed acceptance status — three orders of
magnitude too small to move this.

## The three candidate causes, and how to separate them

1. **Membership change.** The fitted set went 212 -> 183 via deduplication (114
   exact duplicates removed from the 2,111) and an `erepro`/`R_max` filter. 57 of
   the refined set were not in the original 212; 23 removed members were duplicate
   copies.
2. **Re-entering `steady()`.** The refined states were produced by
   `steady()` + a 118 yr spin-up applied to each stored member's *post*-spin-up
   state. That is the documented re-run effect, previously measured at Spearman
   **rho = 0.599** against the stored RMSE ranking — enormous compared with the
   correction's rho = 0.998. The larger unexploited drift (+4.29% vs +2.65%) is
   its signature.
3. **The w_min correction.** Almost certainly negligible; confirm rather than
   assume.

### The clean experiment (~6 min of compute)

`Output_large_files/wmin_test/40_states/` holds **both** arms for all 1,831 clean
members: `state_treated_XXXXX.rds` (corrected w_min) and
`state_control_XXXXX.rds` (uncorrected), each produced by the *identical*
steady + spin-up pipeline. So:

| comparison | isolates |
|---|---|
| new-control vs new-treated | **the correction alone** |
| new-control vs old cached (212) | **re-run + membership** |
| restrict new-control to the 127 members shared with the old 212 | **the re-run alone**, membership held fixed |

That third comparison is the decisive one and nobody has run it.

`Manuscript scripts/F00_build_refined183_data.R` already does all the projection
and extraction. It reads `state_treated_*`; parameterise the arm and run it
against `state_control_*` to a separate output suffix. Do **not** overwrite the
`_refined183` files.

## The canonical SNR — reproduce this exactly

From `Plotting scripts/biomass_slope_snr_mean_med.R` (~line 89), transcribed in
`Manuscript scripts/F02_figure2_snr_refined183.R`:

```
noise  = sd, across years 1841-2010, of the ACROSS-MEMBER MEAN UNEXPLOITED
         trajectory  -> ONE scalar per metric
signal = per-year median across members of (exploited - unexploited)
SNR    = signal / noise
```

**It is not a rolling SD and not the across-member spread.** The 15 yr window
applies only to the *variability* panels (b, d); the level panels (a, c) are
window-independent. Getting this wrong produces a plausible-looking but
meaningless number. `BASELINE_YEARS <- 1841:2010`.

## Data

| file | contents |
|---|---|
| `Manuscript data/biomass_abund_{fish,clim}_refined183.rds` | new: biomass, abundance, mean mass per member-year-species, both arms |
| `Manuscript data/nbss_slope_refined183.rds` | new: LBNbiom slope per member-year, both arms |
| `Manuscript data/biomass_top10pct_raw_{fish,clim}.rds` | old 212-member caches (`sim_i` keys 1..212) |
| `Output_large_files/wmin_test/40_states/` | 3,662 post-steady params + post-spin-up abundances |
| `Output_large_files/wmin_test/40_refined_ranking.rds` | the refined ranking and the 183/184 membership |

**Key trap:** the old caches key on `sim_i` (1…212, RMSE-rank position); the new
data key on `sim_index` (original ensemble index). `top10pct_idx[sim_i] == sim_id`.
Joining on the bare number silently mispairs every member.

## Questions to answer

1. How much of the 2.5x signal reduction is membership, how much is the re-run?
2. Is the larger unexploited drift (+4.29%) the mechanism? Both arms drift because
   `steady()` runs under time-varying therMizer forcing, so "steady state" means
   the criterion was met around simulated year ~1875 and both arms then drift
   ~2.75%/century. If the corrected/re-steadied unexploited arm drifts *more*, and
   both arms drift together, their difference shrinks — check directly.
3. Does the *slope* SNR survive because slope is scale-free while biomass is not?
4. **Which number should the manuscript report?** If the refined ensemble is the
   better one, -0.378 is the answer and the text needs revising. If the re-run
   introduced an artefact, the fitted set may need regenerating without
   re-entering `steady()` — note that is impossible for the corrected arm, since
   changing `w_min` changes the steady state.

## Environment and traps

- mizer **3.1.0**, therMizer **1.0.0**, R 4.6.0 at
  `C:\Program Files\R\R-4.6.0\bin\Rscript.exe`.
- `library(therMizer)` must be **attached** for anything touching
  `params@rates_funcs`, including `getYield()`.
- Saved sims are mizer 2.5.0 — use `validParams()`; verified bit-identical. Do not
  downgrade mizer.
- `project(..., initial_n = NULL)` errors; omit the argument entirely.
- Exploited runs end 2010, unexploited 2011 — filter to 2010.
- **Several top-ranked members are bistable** (446, 1512, 1819, 2082 are ranks
  1-4 of the old set). Member 1512 gives -0.61% or -10.11% community biomass
  depending only on the `steady()` protocol. Before attributing any large
  per-member difference to a parameter change, run the two-test protocol in
  `R/wmin_test/38_diagnose_member_446.R`.
- Leave at least one core free; the machine has 16.
- **Do not overwrite** existing figures or data. The refined outputs use the
  `_refined183` suffix; use a new suffix for anything you generate.

## Deliverable

A written finding that attributes the signal reduction between membership, re-run
and correction with numbers for each; a recommendation on which SNR value the
manuscript should report; and if the re-run is implicated, an assessment of
whether the fitted ensemble can be rebuilt without re-entering `steady()`.

Context: `docs/small_divers_wmin_SYNTHESIS.md` is the index to the whole
investigation. `docs/ensemble_deduplication.md` covers the 212 -> 183 chain.
