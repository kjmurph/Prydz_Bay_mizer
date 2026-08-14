# Brief: independently review and interrogate the `small divers` w_min work

Hand this to a fresh session, ideally a different model. Work from the repository
root. **Your job is to find what is wrong, overstated or unverified — not to
confirm it.**

---

## Your task

A previous session investigated a parameter error in the Prydz Bay mizer model and
reached conclusions that are about to inform decisions on whether to regenerate a
multi-day Monte Carlo ensemble and what to state in a manuscript. Those conclusions
need adversarial review.

Read `docs/small_divers_wmin_SYNTHESIS.md` first — it is the index and through-line,
and links every other document and script.

**Do not take the synthesis at face value.** It was written by the session that did
the work, so it is exactly where motivated reasoning would hide. Verify claims
against code and data. Where you cannot verify something, say so explicitly rather
than assuming it holds.

## Fixed decisions — settled, not under review

These were decided by the project lead. Treat them as constraints, not as options.
Do not spend effort relitigating them, and do not recommend alternatives.

1. **`w_min` for `small divers` is 3626.667 g (= 0.85 × `w_mat`). Full stop.**
   - Not 94 g or 100 g (hatchling mass). Rejected: mizer's `w_min` individuals
     forage for themselves at their own body size, and a provisioned chick cannot
     be represented — a 94 g penguin would eat 94 g-scaled prey it could never
     catch.
   - Not 4500 g (`min(w_indep)`, the empirical value). It exceeds `w_mat` and
     triggers the clamp; that is the whole origin of the bug.
   - **No intermediate value** between hatchling and 3626.667 g.
   - **No finer `w` grid** to widen the group's bin count.
   - **No splitting `small divers`** into dependent and independent stages, or into
     separate species.

   The 2-bin size structure that follows is an accepted, reportable limitation. Your
   job is to check it is honestly characterised and correctly caveated — **not** to
   solve it by changing the parameterisation.

2. **Reproduction level 0.50** is the agreed setting for the penguin
   `erepro`/`R_max` recalibration. You may and should interrogate whether it is
   *defensible and correctly implemented* (claim 4), but the choice itself stands.

3. **Re-entry point is `09_Uncertainty_Analysis.Rmd`'s own `steady()` call**, not a
   redo of the interactive `06_steady_state_therMizer.Rmd` calibration.

Everything else — every number, method, inference and caveat — is open.

## Background in one paragraph

The `small divers` (penguin) group ran with `w_min = 0.001 g` instead of ~3.6 kg
through the whole calibration, a 2,111-member Monte Carlo campaign, its
biomass-stability rejection, and the top-10% RMSE screen that produced the fitted
212-member ensemble. The previous session concluded: the root cause is a silent
mizer clamp; the correction is viable if penguin reproduction is recalibrated
alongside it; the corrected steady state differs by ~0.005% at community level;
neither Monte Carlo gate moves; and separately, 114 of the 2,111 members are
duplicates and a faithful re-run cannot reproduce the stored ensemble at all.

## The load-bearing claims, in the order they matter

Each of these would change a decision if wrong.

### 1. Root cause — `mizer:::validGivenSpeciesParams` clamps `w_min` to 0.001 g

Claim: `w_min` was never authored as 0.001 g; mizer silently rewrites it to
`pmin(0.001, w_mat/10)` when `w_min >= w_mat`, and `small divers` is the only group
where that holds (4500 vs 4266.667).

Check: read the mizer source yourself. Is the condition and replacement as stated?
Is `small divers` genuinely the only group affected? Does the claim that the
"0.001 = mesopelagic fishes w_min" coincidence is a red herring hold up? Does the
abandoned-repair story at `optim_model_setup_old/model_setup_v4.Rmd:520-546` read
the way it is described?

### 2. The rationale for `w_min = 3626.667 g` is correctly documented

**The value itself is settled and NOT under review — see "Fixed decisions" above.**
What is reviewable is whether the *stated justification* is accurate and whether
the *consequences* are properly characterised.

Claim: mizer's `w_min` is where an individual feeds for itself; there is no
parental-energy-transfer mechanism; penguins fledge above adult breeding mass so
`w_indep > w_mat` for all three constituent species, making the empirical value
unsatisfiable; therefore `0.85 * w_mat` is the necessary compromise.

Check: is `w_min` really where independent feeding begins in mizer's formulation,
or does the model treat it purely as a computational boundary? If the latter, the
stated justification is weaker than claimed even though the chosen value stands —
say so. Verify the three per-species figures (Adélie 6000 vs 4000, Crested 4500 vs
4300, Gentoo 6700 vs 4500) against `csvs/predator_parameters_updated.csv`, and that
`min(w_indep) = 4500` and `mean(w_mat) = 4266.667` are what the derivation at
`group params/1g_simplified_groups_params.Rmd:442-447` actually produces.

Then interrogate the **consequence**: at 3626.667 g the group occupies 2 size bins
against 44 before. Is that limitation stated clearly enough, in the right places,
and are the right results caveated? Is any reported quantity invalidated by it
rather than merely caveated? Do **not** propose changing `w_min` to address it —
the question is whether the limitation is honestly and completely reported.

### 3. Stage 1's failure was a method error, not a result

Claim: changing `w_min` alone drove penguins extinct because
`RDI ∝ erepro / w_min` and the stored `erepro` was calibrated for 0.001 g, so
`preserve = "erepro"` forced `R_max` to infinity. Recalibrating the pair fixes it.

Check: verify the RDI formula in the installed mizer. Is the ~400× figure right?
Is recalibrating the reproduction pair legitimate, or does it amount to tuning the
model to hide the correction's effect? **This is the most important question in the
brief** — if the recalibration is over-permissive, the "~0.005% effect" conclusion
collapses. Consider specifically: is holding `preserve = "erepro"` while manually
overriding the penguin `erepro` internally consistent?

### 4. Reproduction level 0.50 is a defensible choice

Claim: `erepro = floor / (1 - 0.5)` where the floor restores control recruitment.
Three settings were tested; all passed; 0.50 was chosen as the middle.

Check: is 0.50 arbitrary? The original calibration had penguins at reproduction
level 0.9997 and the achievable ceiling is 0.54–0.93 — is dropping density
dependence that far a material change to the model being defended as unchanged?

### 5. Community-level effects are negligible (Stage C)

Claim: total community biomass −0.0048%, largest non-penguin shift −0.31%, median
0.030%, and re-running `steady()` alone moves things 0.2–5.8% — so the correction
is smaller than the noise of recalibrating.

Check: recompute from `Output_large_files/wmin_test/30_stageC_comparison.csv`. Is
"smaller than the re-run noise" a legitimate argument or does it prove the *test*
is underpowered rather than the effect small? Is `params_sel_adj.rds` the right
starting object, given the plan originally specified
`params_steady_state_2011_2020_tol_0.00025.RDS`? The session justified the
substitution on the grounds that gear selectivity is w_min-independent — is that
sound?

### 6. Deduplication: 114 redundant members, clean top 10% = 200

Claim: verified two independent ways (ensemble objects and RMSE table, both giving
94 groups / 114 redundant), with 94/94 duplicate groups sharing one RMSE.
`all_parameters_all_simulations.csv` was rejected as misaligned (best 38% where
100% is required).

Check: is the duplicate criterion right — all three of gamma, catchability and
`initial_n`? Is using `initial_n` (the post-spin-up state) as a proxy for the
abundance draw valid? Was the parameter CSV rejected too readily; is there a
mapping that reconciles it? Is `ceiling(1997 * 0.10) = 200` the right way to
re-cut, or should the cut be on some other basis?

### 7. Both Monte Carlo gates pass (Stage D)

Claim: (i) 0 of 200 members change stability status; (ii) Spearman rho 0.998,
net 1 member change.

Check: the same 4 members fail in **both** arms — is the explanation ("the
replication setting, not those members") adequate, or does it indicate the
replication does not reproduce the original procedure well enough for the test to
mean anything? Is testing on the top 10% sufficient to claim membership is stable,
given members outside it were never run? The session flags this as indicative —
is the caveat strong enough?

### 8. A re-run cannot reproduce the stored ensemble (rho 0.599)

Claim: `initial_n` is the post-spin-up state and the pre-spin-up state was never
saved, so any fresh pipeline reorders the ranking at rho 0.60 — ~5,500× the
correction's effect.

Check: this is presented as the most consequential finding. Is it right? Is there
any way to recover or reconstruct the pre-spin-up state? **If true, it implies the
existing fitted-ensemble selection is fragile — does the previous session draw
that conclusion strongly enough, or too strongly?**

## Known self-corrections — verify these were actually fixed

The previous session made and corrected several errors. Confirm the corrections are
real and complete, and look for others of the same kind:

1. **First Stage D run reported 0/200 passing in both arms.** Cause:
   `project(..., initial_n = NULL)` errors, whereas omitting the argument works.
   Fixed in `34_stageD_stability_paired.R`. Check the same mistake is not present
   elsewhere.
2. **Claimed the parameter summary CSV was "unreliable", then retracted** — it
   accurately records 263 redundant draws but sits in a different indexing.
3. **Proposed 94 g as the target and was overruled** on the feeding-mechanism
   argument. Check no 94 g assumption survives anywhere in the scripts or docs.
4. **Proposed installing mizer 2.5.0, then showed it unnecessary** —
   `validParams()` reproduces the stored RMSE to 4.9e-15. Verify that equivalence
   independently.
5. **Initially framed the four `w_mat/w_max` = 0.900 groups as suspicious
   defaults**; they are a deliberate, necessary manual correction. Confirm.

## Things to be actively suspicious of

- **Every "verified bit-identical" and "reproduced to Ne-15" claim.** Re-derive at
  least two of them from scratch.
- **The paired-design argument.** It is used repeatedly to dismiss absolute
  discrepancies. Is it doing legitimate work, or excusing a pipeline that does not
  reproduce the original?
- **Numbers quoted in prose that no longer match the CSVs** — several documents
  were written before later runs completed.
- **The 7 infeasible members.** Required `erepro` > 1, so `R_max = Inf` and
  penguins settle below control. Is carrying them defensible? Does their presence
  contaminate any reported statistic?
- **Scope creep.** Two large problems (duplicates, re-run irreproducibility) were
  found while investigating a third. Is the w_min conclusion cleanly separable from
  them, or are they entangled?
- **Unexamined pre-existing problems** the session flagged but did not resolve:
  non-penguin `R_max = Inf` in some accepted members, and
  `whale_consumption_outputs/*` disagreeing with recomputation by 13–80×.

## Environment and traps

- mizer **3.1.0**, therMizer **1.0.0**, R 4.6.0 at
  `C:\Program Files\R\R-4.6.0\bin\Rscript.exe`.
- `library(therMizer)` must be **attached** for anything touching
  `params@rates_funcs` — including `getYield()`, which is not obviously a rate
  function but routes through `projectRateFunctions`.
- Saved sims are **mizer 2.5.0**. `getCommunitySlope()` fails on missing
  `species_params$is_background`; `getYield()` fails on missing `@second_order_w`.
  Use `validParams()`. **Do not downgrade mizer.**
- `slotNames()` will not tell you whether an *instance* has a slot — it reads the
  3.1.0 class definition. Test `names(attributes(params))`.
- Two incompatible simulation keys: biomass caches use `sim_i` (1…212, RMSE-rank
  position); `nbss_slope_top10pct_data` uses `sim_id` (24…2111, original index);
  spectra caches use row index (1…2111). Map: `top10pct_idx[sim_i] == sim_id`.
  Joining on the bare number silently mispairs every member.
- Exploited runs end **2010**, unexploited **2011**. Filter to 2010.
- Paired runs should be identical to ~1e-15 before 1930 and first diverge in 1931.
- Loading either full ensemble takes 2–4 min and several GB.
- `git log --all` is broken on this repo (`fatal: bad object refs/heads/Monte_Carlo - Copy`).
  Use `git log HEAD`.
- **Do not overwrite anything.** The existing ensembles and caches are the control
  arm. Write to a new path.

## Deliverable

A written critique that, for each of the eight claims above, states **confirmed /
overstated / unverified / wrong**, with the evidence you used. Then:

1. Anything the previous session got wrong or cannot support.
2. Anything it missed — particularly a flaw in the recalibration logic, or a
   reported quantity that the 2-bin size structure invalidates rather than merely
   caveats. **Not** alternative `w_min` values or group structures; those are fixed.
3. Your independent recommendation on whether the ensemble needs regenerating, and
   on what grounds — taking `w_min = 3626.667 g` as given.
4. Whether the caveats proposed for the manuscript are sufficient, insufficient, or
   overstated.

Negative results are as valuable as positive ones. If the work holds up, say so
plainly and identify the weakest remaining link. If it does not, be specific about
which conclusion fails and why.