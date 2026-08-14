# Full ensemble re-run — VM handoff and calibration documentation

Written 2026-08-14. Target: `https://prydzbaymizerv3.southernoeansized.cloud.edu.au/rstudio/`, 32 cores.

## Why this re-run exists

The ISIMIP3a protocol states that historical fisheries catch data may be used for
calibration **"on the condition that only years up to and including 2004 are
used in model calibration/tuning."** Years after 2004 are the evaluation window.

The existing calibration violates that, and it is not a technicality:

- The FishMIP catch reconstruction is `calibration_catch_histsoc_1850_2004_regional_models.csv` — it **ends in 2004**. The effort forcing is `effort_histsoc_1841_2010_regional_models.csv` — it runs to **2010**.
- `01_FishMIP_Fishing_Data.Rmd` fills the missing catch years with zeros (`# Replace NA values with 0`, applied to `toothfishes = 0` in three places).
- Phase 45's fitting window is taken from the **effort** series (first to last year with effort > 0), so it runs to 2010, and `coalesce(Yield_g, 0)` turns absence of data into an observation of no catch.
- **79% of all toothfish effort falls in 2005–2010**, including its effort maximum of 1.000 in 2008, where catch is recorded as zero. With the `+1 g` offset pricing a zero at up to 11.7 log units, the refit drove toothfish `q` **down 65x**, leaving modelled catch at **0.031 of observed** across the 33 real years.
- Toothfishes carried **26.4% of the pooled objective on 11.6% of the observations**. Dropping it changes **26 of the top 167** members and **52 of the top 500** — so cut A itself, and every ensemble built on it, is affected.

## Calibration documentation (for the methods section)

| | |
|---|---|
| Reference model | `params_ref_p86_agemat.rds` |
| Calibration data | FishMIP `calibration_catch_histsoc_1850_2004`, via `yield_observed_timeseries.csv` |
| Effort forcing | FishMIP `effort_histsoc_1841_2010`, via `effort_array_1841_2010.rds` |
| Fitting window | per species, `first year with effort` → `min(2004, last reported catch, last effort year)` |
| Missing catch | dropped as `NA`; never coerced to zero |
| Objective | pooled SSE on `log10(yield + 1 g)`, per species per member |
| Optimisation | divide `q` by the modelled/observed yield ratio, re-project, iterate (6 rounds) |
| Estimator | **median of per-member ratios**, 50x per-iteration step cap |
| Weighting | none; one global multiplier `M_s` per species applied to each member's drawn `q`, capped at `QMAX = 1` |
| Not fitted | **baleen, sperm and minke whale `q` held at the drawn values** — non-identifiable, see below |
| Spatial domain | Prydz Bay regional model, 19 functional groups |

`yield_observed` in the params is all zero/NA and unused, so mizer never sees the
observed series — it exists only inside this objective.

### Two further corrections, found by validating phase 89 before running it

`R/wmin_test/90_catchability_ab_test.R` runs the fit on 10 cached members under
one rule change at a time. It found that the first draft of phase 89 had, beyond
the window fix, changed two things it should not have:

1. **The estimator.** It updated from the ratio of *pooled sums* across members;
   phase 45 used the *median of per-member ratios* with a 50x step cap. Pooling
   is dominated by the largest members and left every species short (krill 0.26,
   toothfishes 0.29, squids 0.44, shelf 0.52) where the median lands them on
   1.000. Restored.
2. **Whale catchability must be held.** Phase 45 held baleen, sperm and minke at
   the drawn `q`, recorded in `45_catchability_multipliers.rds` under
   `$held_at_one` and `$note` — fields the *script* never wrote, which is how the
   decision got lost. Their fit is non-identifiable: phase 45's trace runs baleen
   `1 → 40.8 → 673 → 1.10e4 → 1.81e5 → 2.96e6` while its catch ratio never leaves
   0.061, because `q` is clamped at 1 and the stock is the binding limit. Fitting
   them pins every member at the ceiling and collapses the 2005–2010 out-of-sample
   baleen catch ratio from 0.705 to 0.012. Restored.

Validation on the top 10 usable phase-77 members, median member RMSE on the 2004
window — lower is better:

| multiplier set | RMSE |
|---|---|
| **phase 89, both corrections** | **0.9641** |
| the current ensemble's stored multipliers | 1.0783 |
| no correction at all (drawn `q`) | 1.1635 |
| window fix alone, whales fitted | 1.3291 |
| phase 89 first draft | 1.3623 |

The window fix *alone* is worse than doing nothing — the whale runaway swamps it.
Both corrections are needed, and together they beat the current ensemble on 7 of
10 members. Toothfish falls from 40.2% to 19.7% of the pooled objective, against
11.3% of the observations.

**Open:** bathypelagic fishes has a total observed catch of 690 g across 90 years
and a single observation inside the 2004 window, yet is still assigned a
multiplier of ~3e-07. The direction is unambiguous (the model over-predicts it by
six orders of magnitude) and it is 0.1% of the objective, so it is left fitted —
but it should be stated in the methods rather than left implicit.

## Draw substitution (phase 87)

Downward draws are replaced **once, under seed 20260814, and written to disk**, so
this ensemble is reproducible from a stored input. The existing ensemble is not:
re-running the protocol reorders the ranking because several members are
bistable and `steady()` picks whichever basin it lands in — members 446 and 1512
carry identical draws yet differ 1.88x in whale stock. Fixing the *input* does
not fix that; what it does mean is that the draws are no longer a random stream
generated inside the build.

| Group | rule | rationale |
|---|---|---|
| mesozooplankton | draw `< 1` → `U(1, 10)` | No hypothesis supports a member holding *less* mesozooplankton: it takes 79% of its encounter from the prescribed resource, identical across members, so a reduction is an artefact of applying one draw to all 19 groups. The range samples possibilities. Conservative against the draws themselves (>1 median 11.19, upper quartile 60.5). |
| baleen, minke, sperm whales, **orca** | draw `< 1` → `U(5, 50)` | All four were direct targets of extensive exploitation; the hypothesis is that pre-exploitation populations were much larger than the contemporary-calibrated reference. |

Draws above 1 are never touched, for any group. All other species untouched.

## Run sequence

```bash
# 1. substitute draws (seconds) -- writes 87_member_draws_substituted.rds
Rscript R/wmin_test/87_substitute_draws.R

# 2. build all 1,668 member states on the current reference (~12 h at 30 cores)
#    Every protocol switch below is already the DEFAULT in phase 88 -- only
#    P61_CORES has to be set. The rest are stated for the record.
P61_CORES=30 Rscript R/wmin_test/88_full_ensemble.R run

# progress, without touching the run:
Rscript R/wmin_test/88_full_ensemble.R status

# 3. corrected catchability refit + ranking (~1-2 h)
P89_CORES=30 Rscript R/wmin_test/89_catchability_refit_2004.R
```

Phase 88's defaults, all of which encode the settled protocol below:

| | |
|---|---|
| `P61_BASE` | `params_ref_p86_agemat.rds` |
| `P83_DRAWS` | `.../87_member_draws_substituted.rds` (a substitution stamp is **required**) |
| `P83_SUBSTITUTE` | forced off; setting it to `1` is a hard error, so no double substitution |
| `P65_SCALE_RMAX` / `P65_RMAX_WHEN` | `1` / `postcap` |
| `P61_RAMP_PRESERVE` / `P61_FINAL_PRESERVE` | `erepro` / `R_max` |
| `P61_RECAP` | `0.9` |
| `P61_N` | `1668` — the full accepted set |
| `P61_MATCHGROWTH` / `P83_MG_POST` | both off |
| `P61_STEPS` / `P61_MAX_STEP_X` | `1` / `1e9`, giving K = 1 |
| cores | capped at `detectCores() - 2`, so 30 on the VM and 14 locally |

Phase 88 chunks results to disk, so `run` resumes from where an interrupted
build stopped. It writes to `88_full_states`, which is phase 89's default input.

**Leave 2 cores free** — saturating every core risks a hard shutdown, which kills
the in-flight run as well as the machine. Phase 88 enforces this itself by
capping at `detectCores() - 2`, so `P61_CORES=30` on the 32-core VM is the cap
rather than a request that could be exceeded.

## Settled protocol constraints — do not reorder

1. **`R_max` scaling must follow the recap.** `setBevertonHolt(reproduction_level = L)` sets `R_max = RDD/L`, so a recap after the scaling destroys it. Phase 67 got whale reproduction levels pinned at exactly 0.90 and a null result about a step that never took effect.
2. **A cap must follow matchGrowth**, wherever matchGrowth is used — it ends on `setBevertonHolt(params)`, which resets the level (measured 0.9000 → 0.9017).
3. **matchGrowth must NOT be used in the member protocol.** Tested at two positions; at the post-perturbation position 17 of 40 members failed outright (`search_vol must not contain non-finite values`) and `erepro` reached 8.4e12. It belongs in the reference recalibration only. `P83_MG_POST=0`.
4. **`P61_STEPS=1`, `P61_MAX_STEP_X=1e9`** if reproducing the K=1 single jump; phase 61's own defaults (10, 1.5) give a 25-step ramp.

## Git state at handoff

- Branch `Monte_Carlo_test_v2`, remote `github.com/kjmurph/Prydz_Bay_mizer.git`
- **`R/wmin_test/` has 0 tracked files**; **0 `params_ref_*.rds` tracked**
- 624 untracked files, 7 modified, **1 unpushed commit** (`9fbf961`)
- Already tracked and available: `effort_array_1841_2010.rds`, `yield_observed_timeseries.csv`
- `Output_large_files/` is gitignored — `43_member_draws.rds` must already exist on the VM; the substituted draws are generated there from the seed

Minimal commit for this run:

```
R/wmin_test/87_substitute_draws.R
R/wmin_test/83_whale_scaling_arms.R
R/wmin_test/88_full_ensemble.R
R/wmin_test/89_catchability_refit_2004.R
params_ref_p86_agemat.rds
docs/VM_RERUN_PLAN.md
```

## Open items

- Confirm the VM has a current `43_member_draws.rds`. `Output_large_files/` is gitignored, so it does not travel with the commit; phase 87 reads it and will stop if it is absent.
- Decide whether the reference recalibration scripts (66, 73, 74) should also be committed so `params_ref_p86_agemat.rds` is rebuildable rather than trusted as a binary.
- 1952 shelf-and-coastal-fishes catch is 7,380 t against neighbouring years of 8–56 t (catch/effort ratio 17,985 vs ~300–1,000). Kieran: likely a legitimate large single-species catch, retained.
- Whales cannot be fixed by catchability: baleen has 45x of `q` headroom against an 80x catch shortfall, and raising `q` also raises fishing mortality. Stock-limited.