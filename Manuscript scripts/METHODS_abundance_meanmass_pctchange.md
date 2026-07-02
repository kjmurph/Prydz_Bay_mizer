# Methods — Ensemble percentage-change figure (abundance & mean individual mass)

Methods and reproducibility notes for the side-by-side figure of the
percentage change in **numerical abundance** (panel **a**) and **mean
individual body mass** (panel **b**) of Prydz Bay functional groups under
historical exploitation, relative to an unexploited counterfactual, across the
top-10% RMSE ensemble.

Primary script: [`abundance_meanweight_rmse_top10pct_grid.R`](../abundance_meanweight_rmse_top10pct_grid.R)
Figure outputs: `pctchange_1col_sidebyside.png` (free y-axis per group) and
`pctchange_1col_sidebyside_fixedy.png` (y-axis shared between columns within
each group row).

---

## 1. Ecosystem model and simulation ensemble

Projections were produced with a size-spectrum ecosystem model of the Prydz Bay
region implemented in the `mizer` R framework. The model resolves **19
species/functional types** spanning zooplankton to great whales:

> mesozooplankton, other krill, other macrozooplankton, antarctic krill, salps,
> mesopelagic fishes, bathypelagic fishes, shelf and coastal fishes, flying
> birds, small divers, squids, toothfishes, leopard seals, medium divers, large
> divers, minke whales, orca, sperm whales, baleen whales.

To propagate parameter and structural uncertainty, a **Monte Carlo ensemble of
2,111 model calibrations** was generated, each a distinct draw from the prior
parameter space. Every calibration was run over 1841–2011 under time-varying
climate forcing and a historical exploitation (fishing and marine-mammal
harvest) effort forcing spanning 1841–2010
(`effort_array_1841_2010.rds`), then projected forward.

### 1.1 Paired exploited vs. unexploited ensembles

Two matched ensembles were used:

- **Exploited ("fished") ensemble** — the full Monte Carlo ensemble run with the
  historical exploitation effort forcing
  (`mc_ensemble_2111_cleaned.rds`).
- **Unexploited ("climate-only") ensemble** — the *same* 2,111 calibrations, with
  identical parameters and climate forcing but **zero exploitation effort**
  (`climate_only_ensemble_compiled.rds`).

The two ensembles are **paired by simulation index**: each exploited member has
exactly one unexploited counterpart that differs only in the presence of
exploitation. All exploited-minus-unexploited comparisons below are therefore
computed *within* a calibration first, so that cross-calibration (parameter)
uncertainty is removed from the estimated exploitation effect.

Only members with numerically valid state (`MizerSim` objects containing no
`NaN`/`Inf` in the abundance array `n`) were retained.

## 2. Calibration filtering and top-10% subset

Each calibration was scored against observed yields
([`yield_rmse_evaluation.R`](../yield_rmse_evaluation.R)). For every fished
species, modelled catch was compared with the observed yield time series
(`yield_observed_timeseries.csv`) over that species' **effort window** (the
years in which its effort forcing was non-zero). The goodness of fit was the
**root-mean-square error (RMSE) on the log10(g + 1) scale**, pooled across all
fished species and comparison years; the log transform equalises the
contribution of species spanning many orders of magnitude in mass (e.g. krill
vs. baleen whales).

Calibrations were ranked by ascending RMSE and the **top 10%
(n = ⌈2,111 × 0.10⌉ = 212 members)** were retained
([`create_top10pct_rmse_ensembles.R`](../create_top10pct_rmse_ensembles.R)). A
complementary Pearson-correlation screen between modelled and observed
log-yields (threshold 0.5, DBPM-style) was computed as a diagnostic for this
subset (`yield_top10pct_corr_screen.csv`). The same 212 indices were drawn from
both the exploited and unexploited ensembles, preserving pairing.

## 3. Ecological metrics

For each retained member, species, and year the following were extracted
directly from the size-resolved state (`extract_raw_abund_bm()` in the primary
script):

- **Numerical abundance**, obtained by integrating the abundance density over
  the model size grid,

  $$N_{s,t} = \sum_{w} n_{s}(w,t)\,\Delta w,$$

  where $n_s(w,t)$ is the number density of species $s$ at size $w$ and time
  $t$, and $\Delta w$ is the size-bin width.

- **Biomass**, $B_{s,t} = \sum_{w} n_{s}(w,t)\,w\,\Delta w$ (via `mizer::getBiomass`).

- **Mean individual body mass**, derived as the abundance-weighted mean,

  $$\bar{w}_{s,t} = B_{s,t} \,/\, N_{s,t}.$$

### 3.1 Functional-group aggregation

Results are presented in **12 panels**: eight are single species and four are
aggregated groups. Aggregation is performed **within each member before any
ensemble statistic**, so that group mean mass is a genuine abundance-weighted
group mean rather than an average of per-species ratios:

| Panel | Constituents |
|---|---|
| Large baleen whales | baleen whales |
| Sperm whales | sperm whales |
| Minke whales | minke whales |
| Orca | orca |
| Leopard seals | leopard seals |
| Toothfishes | toothfishes |
| Shelf & coastal fishes | shelf and coastal fishes |
| Antarctic krill | antarctic krill |
| **Pinnipeds** | medium divers, large divers |
| **Seabirds** | flying birds, small divers |
| **Pelagic fishes & squid** | mesopelagic fishes, bathypelagic fishes, squids |
| **Zooplankton** | mesozooplankton, other krill, other macrozooplankton, salps |

For an aggregated group $g$ in member $i$: $N_{g,t}^{(i)} = \sum_{s\in g} N_{s,t}^{(i)}$,
$B_{g,t}^{(i)} = \sum_{s\in g} B_{s,t}^{(i)}$, and
$\bar{w}_{g,t}^{(i)} = B_{g,t}^{(i)}/N_{g,t}^{(i)}$.

## 4. Percentage change from the unexploited counterfactual

For each panel $p$, member $i$, and year $t$, the exploitation effect on a metric
$M \in \{N, \bar{w}\}$ was expressed as the **paired percentage difference**
between the exploited (F) and unexploited (U) runs of the *same* calibration:

$$\Delta M_{p,t}^{(i)} = \left( \frac{M^{(i,\mathrm{F})}_{p,t}}{M^{(i,\mathrm{U})}_{p,t}} - 1 \right)\times 100\%.$$

Ensemble central tendency and uncertainty were then summarised **across the 212
paired members** for each panel and year:

- central line = **median** of $\Delta M_{p,t}^{(i)}$;
- shaded ribbon = **interquartile range (Q25–Q75)**.

The median is used (rather than the mean) because the across-calibration
distribution of the exploitation effect is right-skewed; the median tracks the
typical calibration and stays within the IQR ribbon. Non-finite ratios (from
zero denominators) were dropped before summarising.

## 5. ±1 SD detectability band (signal-to-noise reference)

Red dashed horizontal lines mark a **panel-specific ±1 SD reference band** that
indicates whether a change is large relative to the natural variability of the
unexploited system. The definition follows the signal-to-noise (SNR)
detection/time-of-emergence framework used elsewhere in this study
([`Plotting scripts/biomass_slope_snr_mean_med.R`](biomass_slope_snr_mean_med.R);
after Barrier et al. 2024, *Earth's Future*, doi:10.1029/2024EF004736):

1. Compute the **ensemble-mean unexploited trajectory** of the metric,
   $\bar{M}^{\mathrm{U}}_{p,t} = \operatorname{mean}_i\!\big(M^{(i,\mathrm{U})}_{p,t}\big)$.
2. Define the **noise** as the temporal standard deviation of that trajectory
   over the **full unexploited record, 1841–2010**:
   $\sigma_p = \operatorname{SD}_{t\in[1841,2010]}\big(\bar{M}^{\mathrm{U}}_{p,t}\big)$.
3. Express it on the percentage-change axis as
   $\pm\,\mathrm{ref}_p = \pm\,\sigma_p \big/ \operatorname{mean}_t\!\big(\bar{M}^{\mathrm{U}}_{p,t}\big)\times 100\%$.

**Rationale for the noise definition.** Because the plotted signal is a *paired*
quantity (§4), the cross-calibration spread has already been removed from it; the
noise must therefore *not* re-introduce that spread. Using the pooled
inter-calibration SD instead would inflate the band by 1–2 orders of magnitude
(e.g. to > 100% for the great-whale abundance panels) and would spuriously render
even large declines "non-significant". The full 1841–2010 record is used as the
baseline because the climate-only ensemble carries no exploitation and no
detectable climate trend, so the whole record is effectively stationary and
gives the most robust natural-variability estimate.

**Interpretation.** When the ensemble-median change crosses ±ref$_p$, the
exploitation effect exceeds ±1 SD of the unexploited system's natural
variability (equivalently |SNR| > 1 in the companion SNR figures) and is
considered detectable; changes that remain within the band are not
distinguishable from natural variability. The band is drawn on every panel; in
all panels except zooplankton it lies within the plotted ribbon, so it does not
alter the panel scaling.

## 6. Exploitation-event annotations

Two classes of vertical markers are drawn:

- **Regime-onset lines** (grey dotted), shared across all panels: onset of
  whaling (1930) and onset of the krill fishery (1974), labelled on the top row.
- **Species-specific peak-effort lines** (coloured dashed), drawn only in the
  matching panel: the year of maximum effort for each gear/species, taken as the
  `argmax` of that species' column in the effort forcing
  (`effort_array_1841_2010.rds`) — e.g. peak baleen, sperm, minke whaling, peak
  krill/toothfish/coastal/squid/bathypelagic fishing.

## 7. Figure construction

Each metric is rendered as a single-column, 12-row `ggplot2` `facet_grid`
(one row per functional group, free y-axis per group), and the two metrics are
placed side-by-side with `patchwork` and tagged **a** (abundance) and **b** (mean
individual mass). The x-axis is restricted to 1900–2010 with
`coord_cartesian()`, which trims the display window without dropping any data
from the ribbons or reference statistics.

A **fixed-y variant** (`pctchange_1col_sidebyside_fixedy.png`) synchronises the
y-axis of the two columns within each group row (via invisible anchor points set
to the union of both metrics' Q25–Q75 envelopes), so that the relative magnitude
of abundance vs. mean-mass responses can be compared by scanning horizontally.

## 8. Software and reproducibility

Analyses used R with `mizer`, `tidyverse`, `reshape2`, `scales`, and
`patchwork`. To avoid re-running the ~1.8 GB ensembles, the primary script caches
per-member abundance and biomass to `abund_mw_top10pct_raw_fish.rds` and
`abund_mw_top10pct_raw_clim.rds` on first run and reloads them thereafter.

**Pipeline order**

1. [`yield_rmse_evaluation.R`](../yield_rmse_evaluation.R) — score all 2,111
   calibrations against observed yield → `yield_rmse_per_sim.csv`.
2. [`create_top10pct_rmse_ensembles.R`](../create_top10pct_rmse_ensembles.R) —
   select the paired top-10% (n = 212) exploited and unexploited subsets.
3. [`abundance_meanweight_rmse_top10pct_grid.R`](../abundance_meanweight_rmse_top10pct_grid.R)
   — extract metrics, compute paired percentage change and the ±1 SD reference
   band, and render the figures.

**Key inputs:** `mc_ensemble_2111_cleaned.rds`,
`climate_only_ensemble_compiled.rds`, `yield_rmse_per_sim.csv`,
`effort_array_1841_2010.rds`.

**Key outputs:** `pctchange_1col_sidebyside.png`,
`pctchange_1col_sidebyside_fixedy.png` (and companion 3-column grid variants,
including `*_sdref.png` versions carrying the same ±1 SD band).
