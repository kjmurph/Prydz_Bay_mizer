# VM run: project the 1,831 survivors and cut the refined top 10%

Script: `R/wmin_test/40_vm_project_survivors.R`. Target: 32-CPU VM, 30 workers.
Smoke-tested end to end locally (6 members, both arms, states saved, collect
verified) before this was written.

---

## What it produces

The refined top-10% **membership** (184 members). `39_full_ensemble_paired.R`
established the clean set of 1,831 but skipped projections to fit an overnight
window, so 184 is currently the *size*, not the *list*. This produces the list,
plus the per-species yield diagnostic that the catchability-fitting work starts
from.

It also caches the **post-steady() params and post-spin-up abundances** for every
member in both arms. That is what makes the follow-on catch fitting cheap:
`initial_effort` is 0 for all gears, so `steady()` and the spin-up are unfished and
catchability enters only the 1841-2010 projection — starting from these states one
catchability evaluation is seconds rather than minutes. Set `SAVE_STATES=0` to skip
(saves ~600 MB, but the catch-fit work then has to redo all of this).

---

## Files to copy to the VM

Preserve the relative paths — the script uses them.

| path | size | why |
|---|---|---|
| `R/wmin_test/40_vm_project_survivors.R` | 20 KB | the script |
| `Output_large_files/wmin_test/39_params_cache/` (20 files) | **324 MB** | all 1,997 members' params |
| `Output_large_files/wmin_test/39_full_ensemble_paired.rds` | 226 KB | the clean-set flags |
| `effort_array_1841_2010.rds` | 2.9 KB | fishing effort forcing |
| `yield_observed_timeseries.csv` | 6.5 KB | observed catch |
| `Manuscript data/yield_rmse_per_sim.csv` | 150 KB | for the "vs original top 212" comparison in `collect` |

**Total ~325 MB.** Note this does **not** need the 1.86 GB
`mc_ensemble_2111_cleaned.rds` — `39_params_cache/` already holds every member's
params, extracted from it.

## Packages

```r
install.packages(c("dplyr", "reshape2"))
# mizer 3.1.0 and therMizer 1.0.0 -- the versions everything was validated against
pak::pak("sizespectrum/therMizer")
```

`library(therMizer)` must be **attached**, not merely installed: `params@rates_funcs`
names `therMizerEncounter` etc., and `getYield()` routes through
`projectRateFunctions`, so it fails with `object 'therMizerEncounter' not found`
without it. The script attaches it in both the master and the workers.

## Run

### Step 0 -- environment check (do this first, ~1 min)

`R/wmin_test/41_vm_environment_check.R` reports R/package versions AND runs an
empirical test: it re-projects 5 stored members and compares the recomputed yield
RMSE against `yield_rmse_per_sim.csv`. Locally it agrees to **2.89e-15**.

**This matters more than matching version strings.** If the VM reproduces the
stored numbers, the toolchain is equivalent whatever the versions say. If it does
not, the 15-hour run would produce output that cannot be compared with anything
already reported.

```bash
Rscript R/wmin_test/41_vm_environment_check.R
```

Proceed only on `PASS`.

```bash
cd <repo root>

# smoke test -- ~15 min, exercises the real pipeline end to end
VM_LIMIT=6 VM_CHUNK=3 VM_CORES=6 Rscript R/wmin_test/40_vm_project_survivors.R run
VM_LIMIT=6 Rscript R/wmin_test/40_vm_project_survivors.R collect
rm -f Output_large_files/wmin_test/40_results/*.rds \
      Output_large_files/wmin_test/40_states/*.rds     # clear before the real run

# the real run
VM_CORES=30 VM_CHUNK=50 nohup Rscript R/wmin_test/40_vm_project_survivors.R run \
  > vm_run.log 2>&1 &

# when it finishes
Rscript R/wmin_test/40_vm_project_survivors.R collect
```

**Runtime: expect 12-18 h**, most likely ~15. Derived from the completed
`39_full_ensemble_paired.R` (1,997 members x 2 arms, no projection, 19.1 h on 14
cores) scaled for 30 cores and the ~84% that adding a 170-year projection cost on
the same members elsewhere. Treat the lower bound with suspicion: my two previous
estimates for this ensemble were both low, because rates measured on the
best-fitting members underestimate — those converge fastest.

Chunked (37 chunks of 50), checkpointed per chunk, and **resumable** — re-running
the same command skips completed chunks and loses at most one chunk on a crash.
Each chunk logs elapsed time, a running ETA and a wall-clock timestamp.

## Bring back

| path | approx size |
|---|---|
| `Output_large_files/wmin_test/40_refined_ranking.{rds,csv}` | small |
| `Output_large_files/wmin_test/40_per_species_yield.csv` | small |
| `Output_large_files/wmin_test/40_results/` (37 files) | modest |
| `Output_large_files/wmin_test/40_states/` | **~600 MB** (only if continuing the catch-fit work there) |

---

## Options

| env var | default | effect |
|---|---|---|
| `VM_CORES` | 30 | workers |
| `VM_CHUNK` | 50 | members per checkpoint |
| `ARMS` | `both` | `treated` runs the corrected arm only: ~half the time, but its absolute RMSE is then **not** comparable with stored values, only internally ranked |
| `SAVE_STATES` | 1 | cache post-steady/post-spin-up states |
| `VM_LIMIT` | 0 | first N members, for smoke tests |

**Why both arms by default.** The treated arm must re-enter `steady()`, because
w_min changes the steady state. Re-entering `steady()` from a stored member's
post-spin-up state reorders the RMSE ranking on its own (rho ~0.6 against stored),
so a treated ranking cannot be compared against the stored table — only against a
control put through the identical pipeline. Dropping the control halves the cost
and destroys the only valid comparison.

---

## You may not need this run at all

The projections are needed **only if you adopt the w_min correction**.

For the **uncorrected** model the stored ranking is already exact — re-projecting a
stored member reproduces its `getYield()` to ~4.6e-15, and the recomputed RMSE
matched the stored table to 4.9e-15 across all 2,111. So the refined top 10% is
just: take the deduplicated ranking, drop members failing the clean-set filter, cut
the first 184. That is minutes, locally:

```r
dd <- readRDS("Output_large_files/wmin_test/33_dedupe_full.rds")
D  <- readRDS("Output_large_files/wmin_test/39_full_ensemble_paired.rds")
clean <- D$sim_index[D$trt_ok & D$trt_pass & D$trt_n_erepro_ge1 == 0 &
                     D$trt_n_rmax_inf == 0 & !D$repro_infeasible]
refined <- dd$dedup[dd$dedup$sim_index %in% clean, ]
top184  <- head(refined[order(refined$rmse), "sim_index"], 184)
```

Given the correction moves community biomass by a median of +0.0007% and changes
acceptance for **0 of 1,997** members, the uncorrected route is defensible. The
VM run is what makes the corrected route defensible. Worth deciding before
spending 15 hours.

---

## First result to look at

`collect` prints the per-species share of total squared error. On the 6-member
smoke test:

| species | % of error | modelled/observed catch |
|---|---|---|
| toothfishes | 37.1 | 0.22 |
| shelf and coastal fishes | 32.3 | **0.026** |
| sperm whales | 9.7 | 0.085 |
| antarctic krill | 4.6 | 0.088 |
| bathypelagic fishes | 4.3 | **74,000** |
| baleen whales | 3.9 | 0.092 |
| squids | 3.9 | 0.45 |
| minke whales | 3.5 | 0.16 |
| orca | 0.6 | 0.075 |

Two things to note before the catch-fitting work, though n = 6 and these are all
top-ranked members so treat it as indicative:

1. **The model under-predicts catch for almost every fished group, by 4-40x.**
   That looks systematic rather than group-specific.
2. **On this metric sperm whales are not the largest contributor** — toothfishes
   and shelf-and-coastal fishes carry 69% of the squared error between them. Sperm
   whales matter (9.7%, and only 8.5% of observed catch reproduced) but the
   log-scale RMSE weights the fish groups more heavily. Worth confirming on the
   full 184 before setting priorities.
3. `bathypelagic fishes` is the opposite problem — 74,000x **over**-prediction
   against an observed total of just 690 g. Near-zero observed catch, non-trivial
   modelled catch. A different failure mode from the rest.
