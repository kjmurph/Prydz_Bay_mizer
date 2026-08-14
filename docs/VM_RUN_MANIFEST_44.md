# VM run: rebuild the ensemble from recalibrated base params (script 44)

Target: the same 32-CPU VM used for the `40_*` job, 30 workers. Runs in tmux with
an ntfy alert on completion.

Script: `R/wmin_test/44_rebuild_from_base.R`
Wrapper: `R/wmin_test/44_vm_run.sh`

---

## What this produces

A **new ensemble**, not a reproduction of the published one. Built from the
recalibrated base params plus each member's recovered Monte Carlo draws:
dedupe genuine parameter repeats (1,997 → **1,848**) → corrected `small divers`
w_min → recalibrated to 2001–2010 observed biomass → `steady()` → 118 yr unfished
spin-up → convergence and stability rejection → 1841–2010 projection → yield RMSE.

Why it cannot reproduce the published ensemble, and why that is accepted: the
stored members are basin selections, not consequences of their draws — members 446
and 1512 carry byte-identical draws yet differ 1.88× in baleen whale biomass. See
`docs/ensemble_not_regenerable_FINDING.md`.

Expected from a random 60-member pilot: **88% acceptance**, Spearman rho **0.914**
against the stored ranking, abundance draw → whale stock log-log r **0.957**.

## Settings baked in

| | value | source |
|---|---|---|
| `steady()` | `tol = 0.0025`, `t_max = 1500`, `preserve = "erepro"` | `09_Uncertainty_Analysis.Rmd:1585-1590` (production) |
| spin-up | 118 yr unfished, **1 cycle** | `09:1587`, `09:857-859` |
| projection | 1841–2010, `t_max` omitted | horizon taken from the effort array |
| base params | `initial_effort = 0` for all 19 groups | `06:142-155`, confirmed all-zero through the whole published lineage |
| arms | `treated` only | the w_min contrast was settled at n = 1,997 in Stage D |

**`SAVE_STATES=1` is required, not optional.** Catchability is being re-tuned
*after* this run, and that works by re-projecting the cached post-spin-up states —
exact and near-free, because the spin-up is unfished so catchability enters only
the projection. Without the states that work has to redo the whole job.

---

## The VM

Surveyed 2026-08-03 over ssh. Everything needed is already installed.

| | |
|---|---|
| host | `prydzbaymizerv3.southernoeansized.cloud.edu.au` (131.217.175.129) |
| user | `prydzbaymizer3` (key auth, `~/.ssh/id_ed25519`) |
| OS | Ubuntu 22.04.4 LTS |
| cores | **32** |
| memory | 62 GB (50 free) |
| disk | 30 GB, **8.1 GB free (72% used)** |
| repo | `~/Prydz_Bay_mizer` — already checked out |
| tools | tmux, curl, Rscript all present |
| R packages | mizer **3.1.0**, therMizer **1.0.0**, dplyr 1.2.1, reshape2 1.4.5 — the validated set |

No package installation is needed. `library(therMizer)` must be *attached*, not
merely installed — `@rates_funcs` names `therMizerEncounter` and `getYield()` routes
through `projectRateFunctions`; the script attaches it in the master and workers.

**Disk is the one thing to watch.** 8.1 GB free is ample for this run (`44_states/`
is ~300 MB), but `~/40_states.tar` (727 MB) duplicates
`Output_large_files/wmin_test/40_states/` (730 MB) and has already been pulled down
locally, so deleting the tar frees ~727 MB if needed.

## Files to copy — only 5.2 MB

Six files; everything else is already on the VM (`effort_array_1841_2010.rds`,
`yield_observed_timeseries.csv` and `Manuscript data/yield_rmse_per_sim.csv` are all
present). This job needs **neither** `39_params_cache/` (324 MB) nor any 1.9 GB
ensemble — `43_member_draws.rds` is self-contained.

| path | size | needed for |
|---|---|---|
| `R/wmin_test/44_rebuild_from_base.R` | 28 KB | run + collect |
| `R/wmin_test/44_vm_run.sh` | 8 KB | wrapper |
| `params_sel_adj_wmin_corrected_biocal.rds` | 200 KB | run (the treated base) |
| `Output_large_files/wmin_test/43_member_draws.rds` | 560 KB | run (the draws) |
| `Manuscript data/yield_rmse_per_sim_deduped.csv` | 144 KB | run (`VM_ORDER=rank`) |
| `Manuscript data/biomass_top10pct_raw_clim.rds` | 4.5 MB | collect only |

`mizer44.tar.gz` in the repo root already contains exactly these.

### Transfer, from PowerShell in the repo root

```powershell
$vm = 'prydzbaymizer3@prydzbaymizerv3.southernoeansized.cloud.edu.au'
scp mizer44.tar.gz "${vm}:~/"
ssh $vm 'cd ~/Prydz_Bay_mizer && tar xzf ~/mizer44.tar.gz && chmod +x R/wmin_test/44_vm_run.sh && ls -l R/wmin_test/44_*'
```

---

## Run

All commands from `~/Prydz_Bay_mizer` on the VM. Connect with:

```powershell
ssh prydzbaymizer3@prydzbaymizerv3.southernoeansized.cloud.edu.au
```

The ntfy topic is **`zoomss-someme-2026`**, already the default in the wrapper — no
`export` needed. Subscribe at `https://ntfy.sh/zoomss-someme-2026` or in the ntfy
app. Note that ntfy.sh topics are readable by anyone who knows the name.

### 1. Preflight (~5 min) — do not skip

```bash
./R/wmin_test/44_vm_run.sh preflight
```

Checks tmux, the four R packages, every input file, and then runs a **6-member
known-answer test**: it re-runs members the local machine already computed and
compares the yield RMSE.

| sim_index | expected RMSE | expected 1841 baleen biomass |
|---|---|---|
| 1116 | 2.046299 | 1.667298e11 |
| 524 | 2.008199 | 6.029570e11 |
| 1983 | 2.083614 | 1.715258e11 |
| 989 | 2.079076 | 2.386908e11 |
| 2059 | 1.860046 | 3.162695e12 |
| 862 | rejected — `steady_no_converge` | — |

It fails the run if the maximum absolute difference exceeds 1e-5. **This matters
more than matching version strings**: if the VM reproduces these numbers the
toolchain is equivalent whatever the versions say, and if it does not, a 9-hour
run would produce output that cannot be compared with anything already reported.

Member 862 rejecting is itself a check — the non-convergence guard was dead in
every earlier script (they listened for `warning()`; mizer signals with
`message()`), so a run with **zero** rejections means the handler is broken again.

Then clear the smoke-test output, as the script reminds you:

```bash
rm -f Output_large_files/wmin_test/44_results/*.rds \
      Output_large_files/wmin_test/44_states/*.rds
```

### 2. Start

```bash
VM_CORES=30 ./R/wmin_test/44_vm_run.sh start
```

Launches a detached tmux session `mizer44`, tees to
`Output_large_files/wmin_test/44_full_run.log`, and fires ntfy on completion —
**high priority on success, urgent on failure**, so silence never reads as
success.

```bash
tmux attach -t mizer44     # watch it;  Ctrl-b then d to detach again
./R/wmin_test/44_vm_run.sh status
```

**Runtime ~9 h.** Measured at 4.2 core-minutes per member-arm on the local pilot:
1,848 members × 1 arm ≈ 129 core-hours ≈ 4.3 h on 30 cores. Treat that as a lower
bound — previous estimates for this ensemble have been low twice, because rates
measured on the best-fitting members underestimate the rest. Budget 9 h.

Chunked (37 chunks of 50), checkpointed per chunk, **resumable** — re-running
`start` skips completed chunks and loses at most one chunk to a crash. There is a
guard that refuses to resume across a settings change, so a leftover smoke test
cannot silently drop members.

### 3. Collect

```bash
./R/wmin_test/44_vm_run.sh collect
```

## Bring back

| path | approx size |
|---|---|
| `Output_large_files/wmin_test/44_rebuild_results.rds` | small |
| `Output_large_files/wmin_test/44_rebuild_summary.csv` | small |
| `Output_large_files/wmin_test/44_results/` (37 files) | modest |
| `Output_large_files/wmin_test/44_states/` | **~300 MB** — required for the catchability re-tune |

> **These are two different machines.** The `tar` commands run **on the VM**; the
> `scp` commands run **locally in PowerShell**, in a separate window or after
> `exit`. Running the `scp` lines inside the ssh session fails with
> `cp: cannot stat ':~/44_results.tar'` — `$vm` is a local PowerShell variable and
> is empty on the VM.

### On the VM

```bash
cd ~/Prydz_Bay_mizer
tar cf ~/44_results.tar Output_large_files/wmin_test/44_results \
    Output_large_files/wmin_test/44_rebuild_results.rds \
    Output_large_files/wmin_test/44_rebuild_summary.csv
tar cf ~/44_states.tar  Output_large_files/wmin_test/44_states
ls -lh ~/44_*.tar
```

### Locally, in PowerShell — `exit` the ssh session first

```powershell
cd "C:\Users\uqkmur13\OneDrive - The University of Queensland\Documents\GitHub\Prydz_Bay_mizer"
$vm = 'prydzbaymizer3@prydzbaymizerv3.southernoeansized.cloud.edu.au'
scp "${vm}:~/44_results.tar" .
scp "${vm}:~/44_states.tar" .
tar xf 44_results.tar
tar xf 44_states.tar
```

---

## What happens next, and why the run does not decide it

Two things are deliberately left until after the run, both computed from its
output rather than baked into it:

1. **Catchability re-tune.** The base baleen catchability of 0.05 was hand-set at
   `09:1511` against whale stocks ~13× larger than the recalibrated model's. A
   sweep on cached member 1339 (1841 stock 3.69e12) reaches a modelled/observed
   whale catch of **0.72 at catchability 0.25**, versus 0.094 as drawn — so the
   catch is reproducible, and the re-tune is done by re-projecting `44_states/`.
2. **Selection rule.** The published log10 yield RMSE barely responds to the whale
   catch: a 7.7× change moves it 0.3%. Weighting the baleen rows ×10 lifts the
   rank correlation with whale fit from −0.153 to −0.598. The plan is to cut the
   ensemble several ways from the same run and compare. Nothing about that
   requires re-running.

## Troubleshooting

- **`object 'therMizerEncounter' not found`** — therMizer installed but not
  attached in the workers. The script does attach it; check the install succeeded.
- **`no slot of name "second_order_w"`** — a mizer 2.5.0 object reached `getYield()`
  without `validParams()`. Should not happen here; the base params are upgraded and
  asserted at load.
- **`STOP: N existing result file(s) were written under different settings`** —
  the resume guard. Clear `44_results/` and `44_states/` and relaunch.
- **Zero `steady_no_converge` rejections in collect** — the non-convergence handler
  is broken. Do not read it as "everything converged".
