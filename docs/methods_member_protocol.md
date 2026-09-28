# Methods: the per-member construction protocol

Manuscript-ready description of the phase-104 member protocol, for the methods
section. Two tables (the protocol, and the attrition through the screens),
followed by a section of **drafting notes that are not for the manuscript**.

**Source of truth:** `R/wmin_test/104_members_rebuilt.R` (`worker()`). The
ensemble described is `Output_large_files/wmin_test/104_full.rds`; the attrition
counts were re-verified against that object on 2026-08-19 and are recorded in
`docs/monte_carlo_workflow_review.md` §3.5. If the script and this document ever
disagree, the script is correct.

---

**Table X.** Per-member construction protocol for the Monte Carlo ensemble. Each
of the 1,668 candidate members was built independently from a single common
reference model by the following sequence.

| # | Procedure | Settings and implementation | Justification |
|---|---|---|---|
| 1 | **Initialise from the reference model.** Each member began from an identical calibrated `MizerParams` object (19 functional groups, temperature-dependent rates via therMizer, forced plankton resource, out-of-domain encounter subsidy). | `params_ref_p100_mort_kernel_diet.rds`; encounter, predation and growth rate functions replaced by `therMizerEncounter`, `therMizerPredRate`, `therMizerEReproAndGrowth`; resource dynamics `plankton_forcing`. Presence of the encounter subsidy asserted before use. | Members differ only in their drawn parameters; no state is carried between members. |
| 2 | **Apply the drawn catchability.** Gear-specific catchability *q* replaced by the member's draw. | `gear_params(params) <- ...`, with gear × species row order verified against the stored draw. | Catchability uncertainty is one of the three propagated axes. |
| 3 | **Perturb abundance.** Initial abundance density and the Beverton–Holt recruitment ceiling of every group were multiplied by the *same* per-group drawn factor *s<sub>i</sub>*. | `initial_n[i, ] × s_i` and `R_max_i × s_i`, applied jointly. | Scaling both leaves the reproduction level *RDD*/*R*<sub>max</sub> invariant, so the perturbation reaches the drawn abundance rather than being drawn back by an unchanged ceiling. Scaling `R_max` a second time later would square the multiplier and was therefore excluded. |
| 4 | **Re-equilibrate (ramp).** The perturbed model was returned to steady state with the recruitment ceiling free to re-derive. | `steady(tol = 0.001, t_max = 1000, preserve = "erepro")`. | Tolerance and time budget were set by measurement: at a looser budget (tol 0.01, *t*<sub>max</sub> 300) a fivefold whale perturbation was reported as converged while retaining 96% of the perturbation, and that state subsequently collapsed to 4% over 200 y. |
| 5 | **Draw and apply the reproduction cap.** The steady-state reproduction level of the four whale groups was capped at a per-member value drawn from a uniform prior; groups already below their draw retained their own value. | `RECAP ~ U(0.1, 0.9)`, one draw per member, seed 20260816; applied as `setBevertonHolt(reproduction_level = min(L_i, RECAP))` through a **named** vector restricted to baleen, sperm and minke whales and orca. | Density dependence in the whale groups is unconstrained by the available data and materially governs depletion and recovery. It is propagated as an uncertainty; the yield-based ranking is insensitive to it and it is **not** presented as a fitted parameter. |
| 6 | **Re-equilibrate (tolerance ladder).** The capped model was re-steadied over a descending sequence of tolerances, each rung starting from the previous rung's output, with the recruitment ceiling held fixed. | `steady(preserve = "R_max", t_max = 1500)` at tol = 0.01, 0.005, 0.002, 0.001; the state from the **tightest converged rung** was retained. Members converging at no rung were discarded. | Holding `R_max` preserves the reproduction level set in step 5; reproductive efficiency absorbs the adjustment. Non-convergence is signalled by mizer as a message rather than a warning and was trapped accordingly. |
| 7 | **Spin-up to the pre-exploitation state.** The steady state was projected unfished under the historical climate forcing, and the terminal state taken as the model's 1841 initial condition. | `project(t_start = 1841, t_max = 120, effort = 0)`; the **final** row of the abundance array retained (`project` returns *t*<sub>max</sub> + 1 rows). | The spin-up forcing repeats the ISIMIP3a 1961–1980 control window six times over 1841–1960, so 120 elapsed years is a whole number of ENSO cycles and the terminal state is phase-aligned with the start of the historical period. |
| 8 | **Stability screen.** Applied to the spin-up trajectory of each of the 19 groups. | Coefficient of variation of biomass over the final 40 y < 0.25; and no significant trend over the first 50 y (\|relative slope\| > 0.025 y⁻¹ with *p* < 0.05 rejected). Groups with mean biomass < 1 g were exempt from the trend test only. | Rejects members that oscillate or have not settled. |
| 9 | **Drift screen.** The 1841 state was projected forward unfished under a repeated climate cycle and the terminal-to-initial biomass ratio recorded per group. | 200 y, zero effort, ocean temperature and plankton forcing constructed by repeating the 1961–1980 window with phase alignment to the projection start; retained if every group fell within [0.5, 2]. | Convergence diagnostics are not sufficient evidence of stationarity for the slow-growing groups: members passing step 8 at drift rates of ~0.1% y⁻¹ nonetheless lost groups entirely over long integrations. A single fixed forcing year was not used, as the nearest year departs from the cycle mean by 0.043 °C and alone generated spurious long-term drift. |
| 10 | **Admissibility screen.** Members requiring physically impossible reproductive efficiency were discarded. | All groups required `erepro` < 1. | Reproductive efficiency ≥ 1 implies more recruits than the energy budget allows. |
| 11 | **Retention.** Members satisfying steps 8, 9 and 10 simultaneously were carried into all subsequent analyses. | Retained state stored as the post-ladder parameter object plus the 1841 abundance array. | Both objects are required: downstream analyses re-project from the stored pair and never re-steady it (see note c). |

**Table X footnotes**

(a) Weights in grams, time in years. Effort is normalised to [0, 1] and varies
annually.

(b) `matchGrowth()` was deliberately excluded from the member protocol; it is
applied only in the reference-model calibration, where there is no perturbation
and no fixed-`R_max` re-equilibration to follow.

(c) Stored members were never re-steadied after step 7. Re-projection from the
stored state is exact because the spin-up is unfished, which is what permits
catchability to be re-fitted without rebuilding the ensemble.

(d) Quantities recorded as diagnostics, not used as screens: realised/drawn
abundance ratio, tightest converged tolerance, ramp convergence, reproduction
level before and after capping, per-group drift ratio.

---

**Table X+1.** Attrition through the screens (full ensemble, 30 cores, 20.7 h).

| Stage | Members |
|---|---|
| Attempted | 1,668 |
| Successfully built (step 6 converged at ≥ 1 rung) | 1,419 |
| — of which the ramp (step 4) converged | 1,383 |
| Passing the stability screen (step 8) | 293 |
| Passing the drift screen (step 9) | 223 |
| Admissible (step 10) | 727 |
| **Retained (steps 8, 9 and 10 jointly)** | **203** |

All 249 build failures were non-convergence at every rung of step 6. The screens
are not nested, so the retained count is smaller than the smallest individual
screen.

---

## Drafting notes — NOT for the manuscript

Two gaps to close before this goes in.

**1. Step 3 needs the prior stated.** The table says "the member's drawn factor"
without saying what it was drawn from. The abundance priors are not a single
distribution: lower-trophic groups, krill and mesozooplankton, and the whale
groups each have their own substitution rule (generator
`R/wmin_test/96_substitute_draws_ltl.R`; the ensemble ran on
`104_draws_noorca_wh25rep.rds`, in which orca is untouched, krill and
mesozooplankton are never below 1, and the three exploited whale groups are
replaced by U(5, 25)). That belongs in a preceding table or a sentence before
this one. It has been left as a hole rather than compressed misleadingly.

**2. Step 5's justification is the one a reviewer will press on.** As written it
is honest that `RECAP` is sampled and not estimated, which is the correct claim,
but it invites "why uniform, why those bounds". Worth one extra sentence if a
defensible source exists. The mapping to Beverton–Holt steepness, if useful:
*h* = 0.2/(1 − 0.8*L*), so *L* = 0.1 → *h* = 0.217, 0.5 → 0.333, 0.9 → 0.714.

**Reproducibility caveat for anyone re-running the script.** `104_members_rebuilt.R`
defaults `P104_DRAWS` to `87_member_draws_substituted.rds`, but the production
ensemble was run on `104_draws_noorca_wh25rep.rds`. Re-running without setting
`P104_DRAWS` does not reproduce `104_full.rds`. Note also that the ensemble as a
whole is not regenerable — members are basin selections, and re-running the
protocol reorders the ranking.
