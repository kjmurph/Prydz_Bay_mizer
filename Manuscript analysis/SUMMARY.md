# Krill consumption attribution — Prydz Bay size-spectrum ensemble

Generated 2026-08-09 20:55:33 by `Manuscript scripts/MA02_report.R`.

**Ensemble.** Rebuilt ensemble 44: 2,111 accepted → 1,997 distinct → 1,848 draw-distinct → **1,668 accepted**, catchability globally re-fitted per species. Main analyses use **cut A**, the best-fitting decile by unweighted pooled log10 yield RMSE, **n = 167**. Every exploited member is paired with the unexploited (zero-effort, climate-only) run of the *same* member; all exploited/unexploited comparisons are formed **within** a member and only then summarised across the ensemble.

**Units.** Consumption t yr⁻¹, biomass t, body mass g, mortality yr⁻¹. All consumption and biomass figures are **domain totals** — the model domain (1.474341e+12 m²,  1474341 km²) is encoded in the initial abundances, so no area normalisation is applied.

**Intervals.** Every quantity is reported as ensemble median, IQR (25th–75th) and 5th–95th percentiles across members, written `median [q25–q75], 5-95% [p05–p95]`.

---

## Validation gates

All four gates passed; nothing below would have been written otherwise.

- **V3** — PASS -- 167 matched pairs in MA00 and in MA01's n=167 prefix; both sets identical to cut A. Members dropped: 0 (MA00), 0 (MA01).
- **V4** — PASS -- max relative difference against the cached F00 products: biomass 0, krill consumption 4.19e-16, 2001-2010 spectra 0; MA00 vs MA01 over the shared 167 4.19e-16. Diet-vs-predation-mortality identity: max per-predator disagreement is 1.53e-13 of the total krill flux (and 8.46e-08 in per-group relative terms, over groups carrying more than 1e-6 of the total).
- **V1** — PASS -- the 2010 ensemble-median exploited/unexploited krill consumption ratio recomputed from MA01 matches `fig4_krill_ratio_series_rebuilt167.csv` for all three groups to a max relative difference of 4.66e-15 (baleen+minke 0.42451 vs 0.42451 published).
- **V2** — PASS -- per-member krill consumption shares sum to 100% for all 167 members (max deviation 1.42e-14). NOTE the ensemble MEDIAN of those shares sums to 92.6354%, not exactly 100%, because a median is not additive; no renormalisation has been applied.
- **Magnitudes** — pre-exploitation (1841–1860) total Antarctic krill consumption is **2.11e+06 [1.19e+06-3.68e+06], 5-95% [4.61e+05-5.49e+06] t yr⁻¹** against a standing krill stock of 9.3e+06 [4.84e+06-1.55e+07], 5-95% [1.55e+06-2.67e+07] t, i.e. a consumption:biomass turnover of 0.238 [0.215-0.271], 5-95% [0.194-0.312] yr⁻¹. Scaled by area (1.474e+06 km², ~7–8% of the Southern Ocean), that is a Southern-Ocean-equivalent ~27.2 Mt yr⁻¹, against published Southern Ocean krill consumption estimates of roughly 100–300 Mt yr⁻¹. That is low by a factor of ~4–8 but **within one order of magnitude**, so no magnitude is flagged as implausible. Standing stock scales to ~ 120 Mt, squarely inside the published 60–400 Mt range.

---

## A. Pre-exploitation krill consumption partition (unexploited, 1841–1860)

Total Antarctic krill consumption across all predators: **2.11e+06 [1.19e+06-3.68e+06], 5-95% [4.61e+05-5.49e+06] t yr⁻¹**.

### A.1 By predator group — Antarctic krill

| predator_group | cons_t_per_yr_median | cons_IQR | cons_5_95 | share_pct_median | share_IQR | share_5_95 |
|---|---|---|---|---|---|---|
| shelf and coastal fishes | 1.117e+06 | 6.05e+05–1.96e+06 | 2e+05–3.3e+06 | 50.63 | 47.5–  57 | 41.6–62.9 |
| bathypelagic fishes | 4.144e+05 | 1.96e+05–8.01e+05 | 7.58e+04–1.28e+06 | 20.17 | 12.9–29.9 | 6.09–36.9 |
| mesopelagic fishes | 3.982e+05 | 2.07e+05–7.04e+05 | 4.25e+04–1.5e+06 | 17.41 | 11.1–  31 | 7.78–44.2 |
| other macrozooplankton | 4.929e+04 | 3.39e+04–7.23e+04 | 1.38e+04–1.29e+05 | 2.259 | 1.57–3.85 | 0.764–5.57 |
| squids | 2.939e+04 | 1.56e+04–6.04e+04 | 5.6e+03–1.33e+05 | 1.403 | 1.17–1.84 | 0.926–2.46 |
| toothfishes |  6708 | 3.22e+03–1.36e+04 |  959–2.5e+04 | 0.3266 | 0.261–0.401 | 0.17–0.472 |
| small divers |  6281 | 2.63e+03–1.19e+04 | 1.24e+03–2.04e+04 | 0.2471 | 0.194–0.436 | 0.126–0.744 |
| medium divers |  1532 |  655–2.86e+03 |  223–6.33e+03 | 0.07123 | 0.052–0.0932 | 0.0365–0.133 |
| minke whales |  1116 |  501–1.95e+03 |  190–3.85e+03 | 0.05521 | 0.0332–0.0736 | 0.0169–0.107 |
| flying birds |  1057 |  630–2.2e+03 |  200–4.41e+03 | 0.05445 | 0.0467–0.0663 | 0.0329–0.0884 |
| large divers | 117.6 | 50.5– 221 | 19.9– 480 | 0.004888 | 0.00346–0.00705 | 0.00234–0.0103 |
| mesozooplankton | 60.71 |   35– 118 | 19.6– 238 | 0.003845 | 0.00154–0.00591 | 0.000487–0.0158 |
| sperm whales | 78.17 | 16.8– 234 | 3.91– 980 | 0.003537 | 0.00102–0.00922 | 0.000649–0.025 |
| leopard seals | 36.87 | 15.6–62.4 | 6.05–  95 | 0.001562 | 0.0013–0.00192 | 0.000814–0.0027 |
| baleen whales | 28.52 | 16.6–46.6 | 5.58–62.6 | 0.001259 | 0.00115–0.00144 | 0.000986–0.0019 |
| antarctic krill | 3.101 | 0.952–6.25 | 0.153–13.7 | 0.0001322 | 7.94e-05–0.000186 | 3.66e-05–0.000266 |
| other krill | 0.06421 | 0.0204–0.207 | 0.00682–1.48 | 3.344e-06 | 1.46e-06–1.16e-05 | 2.41e-07–7.12e-05 |
| salps | 0.006702 | 0.00411–0.0149 | 0.00206–0.0291 | 4.12e-07 | 1.73e-07–5.96e-07 | 7.63e-08–2.39e-06 |
| orca | 0.0003079 | 0.000159–0.000738 | 4.78e-05–0.00165 | 1.505e-08 | 1.17e-08–2.04e-08 | 8.03e-09–3.47e-08 |

### A.2 Aggregates — Antarctic krill

| predator_group | cons_t_per_yr_median | share_pct_median | share_pct_q25 | share_pct_q75 | share_pct_p05 | share_pct_p95 |
|---|---|---|---|---|---|---|
| All predators | 2.107e+06 |   100 |   100 |   100 |   100 |   100 |
| All fishes + squids | 2.038e+06 |  97.1 | 95.52 | 97.92 | 93.78 |  98.8 |
| All fishes | 2.009e+06 | 95.53 | 94.13 | 96.51 | 92.16 | 97.07 |
| Invertebrate predators | 8.955e+04 | 4.001 |  2.95 | 5.322 | 2.384 | 7.272 |
| All marine mammals + birds | 1.104e+04 | 0.4979 | 0.3841 | 0.6634 | 0.2685 | 0.902 |
| Seabirds |  7407 | 0.3159 | 0.2502 | 0.4938 | 0.1757 | 0.7994 |
| Pinnipeds |  1657 | 0.07668 | 0.05624 | 0.1012 | 0.03912 | 0.1395 |
| All great whales |  1344 | 0.06282 | 0.04043 | 0.08519 | 0.02512 | 0.1237 |
| Large baleen + minke whales |  1153 | 0.05643 | 0.03439 | 0.075 | 0.01847 | 0.108 |

### A.3 By predator group — all five lower-trophic-level prey combined

Total LTL consumption (Antarctic krill + other krill + mesozooplankton + other macrozooplankton + salps): **1.03e+08 [8.36e+07-1.27e+08], 5-95% [5.31e+07-1.82e+08] t yr⁻¹**.

| predator_group | cons_t_per_yr_median | share_pct_median | share_pct_q25 | share_pct_q75 | share_pct_p05 | share_pct_p95 |
|---|---|---|---|---|---|---|
| other macrozooplankton | 4.737e+07 | 51.59 | 40.87 | 57.81 | 29.03 | 66.49 |
| mesozooplankton | 3.175e+07 |    32 |    24 | 42.87 | 14.05 | 61.11 |
| mesopelagic fishes | 3.67e+06 | 3.663 | 1.908 | 6.246 | 1.169 | 11.83 |
| shelf and coastal fishes | 3.253e+06 | 2.957 | 1.927 | 4.204 | 1.417 | 8.812 |
| bathypelagic fishes | 3.101e+06 | 2.918 | 1.823 |  4.56 | 0.9418 | 8.083 |
| antarctic krill | 2.139e+06 | 2.179 | 0.9805 | 5.805 | 0.2872 |  13.2 |
| other krill | 2.759e+05 | 0.3278 | 0.1414 | 0.7396 | 0.04084 | 4.244 |
| squids | 7.553e+04 | 0.07445 | 0.04748 | 0.1176 | 0.02951 | 0.2115 |
| salps | 3.587e+04 | 0.03579 | 0.02093 | 0.07732 | 0.007939 | 0.1351 |
| toothfishes | 2.109e+04 | 0.0198 | 0.01269 | 0.02916 | 0.006885 | 0.05255 |
| baleen whales | 1.941e+04 | 0.01777 | 0.01046 | 0.04667 | 0.003946 | 0.1626 |
| small divers | 1.119e+04 | 0.01045 | 0.005878 | 0.01743 | 0.003232 | 0.04024 |
| medium divers |  4894 | 0.004403 | 0.002947 | 0.007338 | 0.001694 | 0.01443 |
| flying birds |  2036 | 0.001882 | 0.001188 | 0.003525 | 0.0007758 | 0.006197 |
| minke whales |  1441 | 0.001263 | 0.000707 | 0.002395 | 0.0003094 | 0.005278 |
| large divers | 310.1 | 0.000265 | 0.0001587 | 0.0004393 | 0.0001031 | 0.0008218 |
| sperm whales | 206.7 | 0.0002165 | 7.591e-05 | 0.0006398 | 2.048e-05 | 0.001587 |
| leopard seals | 69.27 | 6.097e-05 | 3.735e-05 | 0.0001034 | 2.397e-05 | 0.0002243 |
| orca | 0.00311 | 3.151e-09 | 1.714e-09 | 4.868e-09 | 9.912e-10 | 9.537e-09 |

| predator_group | cons_t_per_yr_median | share_pct_median | share_pct_q25 | share_pct_q75 |
|---|---|---|---|---|
| All predators | 1.03e+08 |   100 |   100 |   100 |
| Invertebrate predators | 9.158e+07 | 89.85 | 87.22 | 91.96 |
| All fishes + squids | 1.048e+07 | 10.16 | 8.038 |  12.8 |
| All fishes | 1.032e+07 | 10.11 | 7.961 | 12.72 |
| All marine mammals + birds | 4.549e+04 | 0.04343 | 0.02947 | 0.07821 |
| All great whales | 2.126e+04 | 0.02053 | 0.01208 | 0.05579 |
| Large baleen + minke whales | 2.105e+04 | 0.01963 | 0.01131 | 0.05362 |
| Seabirds | 1.337e+04 | 0.01206 | 0.007834 | 0.02055 |
| Pinnipeds |  5149 | 0.004837 | 0.003108 | 0.007789 |

Per-prey partitions for each of the five LTL groups separately are in `A_ltl_partition_1841_1860.csv`; each predator's own diet composition (percentage of its total intake by prey) is in `A_predator_diet_composition_1841_1860.csv`.

### What this implies for the mechanism

**Before exploitation, baleen plus minke whales accounted for 0.0564 [0.0344-0.075], 5-95% [0.0185-0.108]% of total Antarctic krill predation** — a few hundredths of one percent, not a dominant share. Krill predation is overwhelmingly a fish process in this model: shelf and coastal fishes, bathypelagic fishes and mesopelagic fishes together take  88.2% of it. The same holds for lower-trophic-level prey as a whole: whales take 0.0196 [0.0113-0.0536], 5-95% [0.00522-0.167]% of it.

The cause is structural, not a calibration outcome — see section F. Baleen and minke whales are the only two groups with a **box** feeding kernel, and its hard prey-size window puts Antarctic krill outside the reach of most of the baleen whale size range. **Any manuscript claim that whales were the dominant pre-exploitation krill predator is not supported by this model**, and the reason is a parameter choice that can be revisited, not an emergent result.

**What the model's whales eat instead.** From the diet-composition table, baleen whales take 69.3% of their total intake from the **forced plankton `Resource`** (which extends to the  100 g cutoff and so overlaps the lower part of their 20.6– 1030 g window), 20.5% from bathypelagic + mesopelagic + shelf and coastal fishes, 8.81% from salps, and 0.0139% from Antarctic krill. Minke whales take 84.3% from the Resource and 12.1% from Antarctic krill.

This matters beyond the partition: because the Resource is a **prescribed forcing** rather than a state variable (section C), the majority of whale intake in this model is donor-controlled. Removing the whales cannot free up the food they were eating, because that food was never depleted in the first place. That is a second, independent reason — on top of the kernel — why whaling produces so little krill response here.

---

## B. Size-resolved attribution of the change in krill predation

Paired exploited − unexploited change in Antarctic krill consumption, by predator group and predator body-mass bin. Full native-bin resolution (100 bins) in `B_krill_delta_by_size_bin.csv`; log10 body-mass decades in `B_krill_delta_by_decade_bin.csv`; ranked extremes in `B_top_contributions.csv`.

**Null check.** 1841–1860 predates all fishing effort, so the paired delta must be identically zero. Max |median delta| over all predator × bin combinations in that period: **   0 t yr⁻¹**.

### 1930-1970

Net change summed over all predators and bins: ** 2914 t yr⁻¹** (positive contributions  5312, negative -2399).

Largest positive contributions (predator × log10 body-mass decade):

| predator_group | log10_w_decade | delta_t_per_yr_median | delta_t_per_yr_q25 | delta_t_per_yr_q75 | unexploited_t_per_yr_median | exploited_t_per_yr_median |
|---|---|---|---|---|---|---|
| bathypelagic fishes |     2 |  3324 | 644.5 |  9600 | 3.819e+05 | 3.865e+05 |
| mesopelagic fishes |     2 |  1986 | 551.4 |  5632 | 3.463e+05 | 3.482e+05 |
| toothfishes |     3 | 2.092 | -0.7921 | 11.98 |  1638 |  1639 |
| toothfishes |     4 | 0.2844 | -2.221 |  8.21 |  1502 |  1512 |
| other macrozooplankton |    -2 | 0.1239 | -2.208 | 1.091 | 858.6 | 859.7 |
| salps |     1 | 9.072e-05 | 3.014e-05 | 0.0002053 | 0.006419 | 0.006557 |

Largest negative contributions:

| predator_group | log10_w_decade | delta_t_per_yr_median | delta_t_per_yr_q25 | delta_t_per_yr_q75 | unexploited_t_per_yr_median | exploited_t_per_yr_median |
|---|---|---|---|---|---|---|
| shelf and coastal fishes |     2 | -630.3 | -1253 |  -256 | 2.041e+05 | 2.031e+05 |
| mesopelagic fishes |     1 |  -495 | -1008 | -280.9 | 5.992e+04 | 5.928e+04 |
| shelf and coastal fishes |     3 | -365.6 | -2220 |  1248 | 8.958e+05 | 8.893e+05 |
| bathypelagic fishes |     1 | -364.9 | -693.1 | -191.7 | 3.233e+04 | 3.189e+04 |
| other macrozooplankton |     0 | -111.2 | -233.1 | -55.65 | 1.222e+04 | 1.21e+04 |
| other macrozooplankton |    -1 | -88.92 | -281.1 | -46.13 | 3.525e+04 | 3.504e+04 |
| squids |     4 | -79.68 | -238.5 | -5.608 | 2.195e+04 | 2.147e+04 |
| shelf and coastal fishes |     1 | -60.83 | -112.8 | -30.58 |  7131 |  7014 |
| squids |     3 | -45.36 | -70.23 | -26.53 |  5918 |  5678 |
| small divers |     3 | -36.16 | -68.55 | -19.86 |  6138 |  6018 |

### 1974-1990

Net change summed over all predators and bins: **-1.485e+04 t yr⁻¹** (positive contributions 375.8, negative -1.522e+04).

Largest positive contributions (predator × log10 body-mass decade):

| predator_group | log10_w_decade | delta_t_per_yr_median | delta_t_per_yr_q25 | delta_t_per_yr_q75 | unexploited_t_per_yr_median | exploited_t_per_yr_median |
|---|---|---|---|---|---|---|
| bathypelagic fishes |     2 | 375.8 | -2292 |  8336 | 3.754e+05 | 3.761e+05 |
| salps |     1 | 9.305e-05 | -5.199e-06 | 0.0002545 | 0.006245 | 0.006179 |

Largest negative contributions:

| predator_group | log10_w_decade | delta_t_per_yr_median | delta_t_per_yr_q25 | delta_t_per_yr_q75 | unexploited_t_per_yr_median | exploited_t_per_yr_median |
|---|---|---|---|---|---|---|
| shelf and coastal fishes |     3 | -7646 | -1.536e+04 | -2325 | 8.888e+05 | 8.733e+05 |
| shelf and coastal fishes |     2 | -3161 | -5303 | -1725 | 2.03e+05 | 2.01e+05 |
| mesopelagic fishes |     1 | -1411 | -2364 | -854.7 | 5.89e+04 | 5.721e+04 |
| bathypelagic fishes |     1 | -819.9 | -1500 | -413.8 | 3.186e+04 | 3.07e+04 |
| minke whales |     6 | -576.2 | -1104 | -271.3 |  1179 | 398.4 |
| squids |     4 | -445.4 |  -748 | -215.8 | 2.164e+04 | 2.059e+04 |
| other macrozooplankton |    -1 | -265.4 |  -768 | -98.19 | 3.484e+04 | 3.474e+04 |
| other macrozooplankton |     0 | -230.8 | -403.7 | -110.3 | 1.203e+04 | 1.184e+04 |
| shelf and coastal fishes |     1 | -149.8 | -284.4 | -97.03 |  7013 |  6624 |
| squids |     3 | -129.4 | -221.3 | -79.43 |  5739 |  5526 |

### 2001-2010

Net change summed over all predators and bins: **-46.72 t yr⁻¹** (positive contributions  5605, negative -5652).

Largest positive contributions (predator × log10 body-mass decade):

| predator_group | log10_w_decade | delta_t_per_yr_median | delta_t_per_yr_q25 | delta_t_per_yr_q75 | unexploited_t_per_yr_median | exploited_t_per_yr_median |
|---|---|---|---|---|---|---|
| bathypelagic fishes |     2 |  3373 | 273.7 | 1.014e+04 | 3.817e+05 | 3.846e+05 |
| mesopelagic fishes |     2 |  2232 | -4.143 |  6966 | 3.356e+05 | 3.416e+05 |
| other macrozooplankton |    -2 | 0.0119 | -7.479 | 1.377 |   874 |   873 |
| salps |     1 | 0.0001238 | 3.89e-05 | 0.0002912 | 0.006296 | 0.006414 |
| mesozooplankton |    -7 | 5.617e-12 | -4.892e-11 | 8.164e-11 | 5.181e-10 | 5.357e-10 |
| mesozooplankton |    -8 | 1.282e-13 | -1.167e-11 | 1.603e-11 | 3e-11 | 2.608e-11 |

Largest negative contributions:

| predator_group | log10_w_decade | delta_t_per_yr_median | delta_t_per_yr_q25 | delta_t_per_yr_q75 | unexploited_t_per_yr_median | exploited_t_per_yr_median |
|---|---|---|---|---|---|---|
| shelf and coastal fishes |     3 | -1421 | -6529 | 884.7 | 9.178e+05 | 9.065e+05 |
| shelf and coastal fishes |     2 | -1168 | -2624 | -502.1 | 1.997e+05 | 1.984e+05 |
| mesopelagic fishes |     1 | -859.5 | -1699 | -481.4 | 6.024e+04 | 5.905e+04 |
| minke whales |     6 | -546.3 | -1120 |  -238 |  1167 | 467.3 |
| bathypelagic fishes |     1 | -542.2 | -1118 |  -316 | 3.274e+04 | 3.212e+04 |
| squids |     4 | -378.7 | -682.3 |  -204 | 2.175e+04 | 2.064e+04 |
| other macrozooplankton |     0 | -182.4 | -348.4 | -84.42 | 1.197e+04 | 1.182e+04 |
| other macrozooplankton |    -1 | -159.5 |  -579 | -56.35 | 3.493e+04 | 3.467e+04 |
| shelf and coastal fishes |     1 | -101.2 | -176.9 | -51.69 |  6940 |  6607 |
| squids |     3 | -86.2 | -138.7 | -49.56 |  5840 |  5684 |

### The specific hypothesis tested: small toothfishes and mid-size pelagic fishes, 1930–1970

Of the total **positive** change in krill predation during 1930–1970 ( 5312 t yr⁻¹ summed over predator × decade), the share contributed by each candidate is:

| candidate | share_of_positive_change_pct |
|---|---|
| toothfishes (all sizes) | 0.04473 |
| mesopelagic + bathypelagic fishes | 99.95 |
| shelf and coastal fishes |     0 |
| everything else | 0.002334 |

### What this implies for the mechanism

The decade column is the predator body-mass class in log10 g: decade 0 is 1–10 g, decade 2 is 100–1000 g, decade 3 is 1–10 kg, decade 6 is 1–10 t.

**The hypothesis is half right.** The 1930–1970 increase in krill predation *is* concentrated in mid-size pelagic fishes — the two largest contributions are bathypelagic fishes at decade 2 ( 3324 t yr⁻¹) and mesopelagic fishes at decade 2 ( 1986 t yr⁻¹), together 99.95% of all positive change, and both sit in the 100–1000 g class. **It is not toothfishes**, which contribute 0.0447% of the positive change — three to four orders of magnitude less than the pelagic fishes and negligible at any size. Shelf and coastal fishes, the largest krill predator overall, move in the *opposite* direction (   0% of positive change; they dominate the negative side).

---

## C. Lower-trophic-level disaggregation: predation release vs competition release

Paired percentage change from the unexploited counterfactual, per LTL group. Annual series 1900–2010 in `C_ltl_pctchange_timeseries_1900_2010.csv`; period means below and in `C_ltl_pctchange_period_summary.csv`.

### abundance — % change from unexploited

| ltl_group | period | pct_change_median | pct_change_q25 | pct_change_q75 | pct_change_p05 | pct_change_p95 |
|---|---|---|---|---|---|---|
| antarctic krill | 1841-1860 |     0 |     0 |     0 |     0 |     0 |
| antarctic krill | 1930-1970 | -0.2859 | -0.8316 | -0.1432 | -3.226 | -0.07731 |
| antarctic krill | 1974-1990 | -1.162 | -2.633 | -0.4828 | -11.47 | -0.2054 |
| antarctic krill | 2001-2010 | -0.4583 | -1.596 | -0.203 | -11.83 | -0.0859 |
| other krill | 1841-1860 |     0 |     0 |     0 |     0 |     0 |
| other krill | 1930-1970 | 0.01379 | -0.004902 | 0.03342 | -0.1021 | 0.2065 |
| other krill | 1974-1990 | 0.02448 | -0.0002381 | 0.07023 | -0.2101 | 0.3857 |
| other krill | 2001-2010 | 0.01981 | -0.006287 | 0.04805 | -0.1904 | 0.333 |
| mesozooplankton | 1841-1860 |     0 |     0 |     0 |     0 |     0 |
| mesozooplankton | 1930-1970 | 0.03743 | 0.01318 | 0.0784 | 0.001501 | 0.4401 |
| mesozooplankton | 1974-1990 | 0.1096 | 0.06502 | 0.2693 | 0.01732 | 0.9324 |
| mesozooplankton | 2001-2010 | 0.0644 | 0.0261 | 0.1375 | 0.002951 | 0.5742 |
| other macrozooplankton | 1841-1860 |     0 |     0 |     0 |     0 |     0 |
| other macrozooplankton | 1930-1970 | -0.02352 | -0.06928 | 0.008121 | -0.3028 | 0.06333 |
| other macrozooplankton | 1974-1990 | 0.000189 | -0.06685 | 0.06481 | -0.403 | 0.2626 |
| other macrozooplankton | 2001-2010 | -0.02538 | -0.08358 | 0.01918 | -0.4188 | 0.09947 |
| salps | 1841-1860 |     0 |     0 |     0 |     0 |     0 |
| salps | 1930-1970 | 0.3859 | 0.1566 | 0.6333 | -0.1022 | 2.112 |
| salps | 1974-1990 | 0.4992 | 0.2424 | 0.8662 | -0.1474 | 2.691 |
| salps | 2001-2010 | 0.5615 | 0.3062 | 0.9167 | -0.141 | 2.659 |

### biomass — % change from unexploited

| ltl_group | period | pct_change_median | pct_change_q25 | pct_change_q75 | pct_change_p05 | pct_change_p95 |
|---|---|---|---|---|---|---|
| antarctic krill | 1841-1860 |     0 |     0 |     0 |     0 |     0 |
| antarctic krill | 1930-1970 | -0.4885 | -1.363 | -0.2813 | -4.338 | -0.146 |
| antarctic krill | 1974-1990 | -1.856 | -3.966 | -0.8824 | -13.47 | -0.41 |
| antarctic krill | 2001-2010 | -0.8337 | -2.675 | -0.4122 | -13.49 | -0.1714 |
| other krill | 1841-1860 |     0 |     0 |     0 |     0 |     0 |
| other krill | 1930-1970 | 0.012 | -0.04056 | 0.05932 | -0.2702 | 0.3413 |
| other krill | 1974-1990 | 0.002944 | -0.05637 | 0.09788 | -0.3913 | 0.5749 |
| other krill | 2001-2010 | 0.002749 | -0.04694 | 0.08747 | -0.3583 | 0.5376 |
| mesozooplankton | 1841-1860 |     0 |     0 |     0 |     0 |     0 |
| mesozooplankton | 1930-1970 | 0.0545 | 0.02266 | 0.1248 | 0.004533 | 0.6095 |
| mesozooplankton | 1974-1990 | 0.1559 | 0.09621 | 0.3767 | 0.02933 | 1.377 |
| mesozooplankton | 2001-2010 | 0.09488 | 0.04195 | 0.1811 | 0.006179 | 0.7959 |
| other macrozooplankton | 1841-1860 |     0 |     0 |     0 |     0 |     0 |
| other macrozooplankton | 1930-1970 | -0.06969 | -0.2221 | -0.03557 | -0.6049 | -0.002601 |
| other macrozooplankton | 1974-1990 | -0.06041 | -0.2374 | -0.005142 | -0.7746 | 0.1949 |
| other macrozooplankton | 2001-2010 | -0.07947 | -0.2689 | -0.0388 | -0.7799 | 0.02625 |
| salps | 1841-1860 |     0 |     0 |     0 |     0 |     0 |
| salps | 1930-1970 |  1.01 | 0.4706 | 1.859 | -0.03195 | 5.413 |
| salps | 1974-1990 | 1.385 | 0.6941 | 2.496 | -0.07129 | 7.593 |
| salps | 2001-2010 | 1.505 | 0.8394 | 2.623 | -0.016 | 7.615 |

### mean individual mass — % change from unexploited

| ltl_group | period | pct_change_median | pct_change_q25 | pct_change_q75 | pct_change_p05 | pct_change_p95 |
|---|---|---|---|---|---|---|
| antarctic krill | 1841-1860 |     0 |     0 |     0 |     0 |     0 |
| antarctic krill | 1930-1970 | -0.218 | -0.3809 | -0.117 | -1.616 | -0.05669 |
| antarctic krill | 1974-1990 | -0.5786 | -1.133 | -0.3889 | -2.644 | -0.1632 |
| antarctic krill | 2001-2010 | -0.3306 | -0.6514 | -0.1797 | -2.347 | -0.06885 |
| other krill | 1841-1860 |     0 |     0 |     0 |     0 |     0 |
| other krill | 1930-1970 | -0.003576 | -0.0387 | 0.03031 | -0.189 | 0.1583 |
| other krill | 1974-1990 | -0.01435 | -0.06676 | 0.03965 | -0.2306 | 0.2533 |
| other krill | 2001-2010 | -0.006473 | -0.05352 | 0.04629 | -0.232 | 0.2455 |
| mesozooplankton | 1841-1860 |     0 |     0 |     0 |     0 |     0 |
| mesozooplankton | 1930-1970 | 0.01636 | 0.007029 | 0.04533 | -0.0009196 | 0.1904 |
| mesozooplankton | 1974-1990 | 0.04678 | 0.02511 | 0.1131 | 0.006062 | 0.4718 |
| mesozooplankton | 2001-2010 | 0.02613 | 0.01508 | 0.06442 | 0.001869 | 0.2531 |
| other macrozooplankton | 1841-1860 |     0 |     0 |     0 |     0 |     0 |
| other macrozooplankton | 1930-1970 | -0.04654 | -0.1294 | -0.0178 | -0.5599 | -0.003122 |
| other macrozooplankton | 1974-1990 | -0.05518 | -0.1629 | -0.01774 | -0.7261 | -0.004268 |
| other macrozooplankton | 2001-2010 | -0.06158 | -0.1703 | -0.02315 | -0.7288 | -0.005028 |
| salps | 1841-1860 |     0 |     0 |     0 |     0 |     0 |
| salps | 1930-1970 | 0.5738 | 0.3269 | 1.168 | 0.08112 | 3.243 |
| salps | 1974-1990 | 0.8061 | 0.4529 | 1.577 | 0.06706 | 4.782 |
| salps | 2001-2010 | 0.848 | 0.5075 | 1.608 | 0.1172 | 4.759 |

### The background resource spectrum

The resolved species sit on a forced plankton resource (`resource_dynamics = "plankton_forcing"`), which spans 8.29e-15 g to the  100 g cutoff (last non-zero bin 79.75 g) and therefore **overlaps the resolved size range rather than sitting entirely below it** — the resolved grid starts at 3.16e-08 g.

**The resource is identical in the exploited and unexploited runs.** Measured over all 167 members, all 170 years and all 142 resource bins, the maximum relative difference between arms is **   0**. It is also identical across members (max relative spread across the ensemble    0), as expected for an exogenous forcing. See `C_resource_spectrum_check.csv`.

### What this implies for the mechanism

Because the shared resource is *forced*, it cannot be depressed by competition — it is a boundary condition, not a state variable. **The competition-release mechanism is therefore not testable in this model through the resource term**, and any competition signal must appear as elevated biomass in the other four LTL groups instead.

Reading the 2001-2010 biomass row: Antarctic krill -0.834%, against other krill 0.00275%, mesozooplankton 0.0949%, other macrozooplankton -0.0795%, salps 1.51%.

Two things follow. First, **every LTL response is smaller than 2% in magnitude** — an order of magnitude below what either hypothesis would predict as a headline effect, so the mechanistic question is being asked of a very small signal. Second, the pattern is **not the clean predation-release signature**, which would be Antarctic krill moving alone: salps move 1.81x as much as krill does, and in the opposite direction. Krill down with salps up is the signature of a *shared-predator or shared-space* rearrangement among the LTL groups rather than of krill predation release in isolation; with the resource fixed, the two cannot be separated further within this model.

---

## D. Krill mortality decomposition

Total Antarctic krill mortality split into predation mortality attributable to each predator, plus fishing and background mortality, both arms. Rates are krill-biomass-weighted means over the size spectrum. Full tables in `D_krill_mortality_decomposition.csv`; the paired exploited − unexploited difference in `D_krill_mortality_paired_delta.csv`.

**Consistency.** Predation mortality and diet-derived consumption are the same flux computed two independent ways. The largest per-predator disagreement, over all 167 members, four periods and both arms, is 1.53e-13 of the total krill flux (8.46e-08 in per-group relative terms over groups carrying more than 1e-6 of the total). The mortality table is therefore a genuine rate-space restatement of the consumption table, not a second estimate of it.

### 1841-1860

| arm | mortality_source | removal_t_per_yr_median | removal_t_per_yr_q25 | removal_t_per_yr_q75 | mort_rate_per_yr_median | krill_biomass_t_median |
|---|---|---|---|---|---|---|
| exploited | TOTAL mortality | 5.691e+06 | 3.114e+06 | 9.322e+06 | 0.6108 | 9.301e+06 |
| exploited | BACKGROUND | 3.466e+06 | 1.803e+06 | 5.763e+06 | 0.3727 | 9.301e+06 |
| exploited | TOTAL predation | 2.107e+06 | 1.189e+06 | 3.683e+06 | 0.2381 | 9.301e+06 |
| exploited | shelf and coastal fishes | 1.117e+06 | 6.052e+05 | 1.957e+06 | 0.1253 | 9.301e+06 |
| exploited | bathypelagic fishes | 4.144e+05 | 1.96e+05 | 8.014e+05 | 0.04441 | 9.301e+06 |
| exploited | mesopelagic fishes | 3.982e+05 | 2.068e+05 | 7.044e+05 | 0.03688 | 9.301e+06 |
| exploited | other macrozooplankton | 4.929e+04 | 3.389e+04 | 7.23e+04 | 0.005541 | 9.301e+06 |
| exploited | squids | 2.939e+04 | 1.563e+04 | 6.038e+04 | 0.003424 | 9.301e+06 |
| unexploited | TOTAL mortality | 5.691e+06 | 3.114e+06 | 9.322e+06 | 0.6108 | 9.301e+06 |
| unexploited | BACKGROUND | 3.466e+06 | 1.803e+06 | 5.763e+06 | 0.3727 | 9.301e+06 |
| unexploited | TOTAL predation | 2.107e+06 | 1.189e+06 | 3.683e+06 | 0.2381 | 9.301e+06 |
| unexploited | shelf and coastal fishes | 1.117e+06 | 6.052e+05 | 1.957e+06 | 0.1253 | 9.301e+06 |
| unexploited | bathypelagic fishes | 4.144e+05 | 1.96e+05 | 8.014e+05 | 0.04441 | 9.301e+06 |
| unexploited | mesopelagic fishes | 3.982e+05 | 2.068e+05 | 7.044e+05 | 0.03688 | 9.301e+06 |
| unexploited | other macrozooplankton | 4.929e+04 | 3.389e+04 | 7.23e+04 | 0.005541 | 9.301e+06 |
| unexploited | squids | 2.939e+04 | 1.563e+04 | 6.038e+04 | 0.003424 | 9.301e+06 |

### 1930-1970

| arm | mortality_source | removal_t_per_yr_median | removal_t_per_yr_q25 | removal_t_per_yr_q75 | mort_rate_per_yr_median | krill_biomass_t_median |
|---|---|---|---|---|---|---|
| exploited | TOTAL mortality | 5.666e+06 | 3.01e+06 | 9.181e+06 | 0.6153 | 9.143e+06 |
| exploited | BACKGROUND | 3.407e+06 | 1.783e+06 | 5.695e+06 | 0.3727 | 9.143e+06 |
| exploited | TOTAL predation | 2.156e+06 | 1.195e+06 | 3.702e+06 | 0.2426 | 9.143e+06 |
| exploited | shelf and coastal fishes | 1.134e+06 | 5.991e+05 | 1.96e+06 | 0.1265 | 9.143e+06 |
| exploited | bathypelagic fishes | 4.185e+05 | 1.967e+05 | 8.149e+05 | 0.04539 | 9.143e+06 |
| exploited | mesopelagic fishes | 4.008e+05 | 2.074e+05 | 7.077e+05 | 0.03802 | 9.143e+06 |
| exploited | other macrozooplankton | 4.911e+04 | 3.242e+04 | 7.183e+04 | 0.005566 | 9.143e+06 |
| exploited | squids | 2.839e+04 | 1.536e+04 | 6.016e+04 | 0.003419 | 9.143e+06 |
| unexploited | TOTAL mortality | 5.695e+06 | 3.082e+06 | 9.34e+06 | 0.6118 | 9.242e+06 |
| unexploited | BACKGROUND | 3.444e+06 | 1.802e+06 | 5.761e+06 | 0.3727 | 9.242e+06 |
| unexploited | TOTAL predation | 2.112e+06 | 1.19e+06 | 3.69e+06 | 0.2391 | 9.242e+06 |
| unexploited | shelf and coastal fishes | 1.143e+06 | 6.013e+05 | 1.957e+06 | 0.1256 | 9.242e+06 |
| unexploited | bathypelagic fishes | 4.157e+05 | 1.963e+05 | 8.038e+05 | 0.0447 | 9.242e+06 |
| unexploited | mesopelagic fishes | 3.99e+05 | 2.07e+05 | 7.049e+05 | 0.03721 | 9.242e+06 |
| unexploited | other macrozooplankton | 4.947e+04 | 3.337e+04 | 7.229e+04 | 0.005568 | 9.242e+06 |
| unexploited | squids | 2.937e+04 | 1.557e+04 | 6.033e+04 | 0.003415 | 9.242e+06 |

### 1974-1990

| arm | mortality_source | removal_t_per_yr_median | removal_t_per_yr_q25 | removal_t_per_yr_q75 | mort_rate_per_yr_median | krill_biomass_t_median |
|---|---|---|---|---|---|---|
| exploited | TOTAL mortality | 5.592e+06 | 2.93e+06 | 9.178e+06 | 0.6206 | 8.872e+06 |
| exploited | BACKGROUND | 3.306e+06 | 1.699e+06 | 5.65e+06 | 0.3727 | 8.872e+06 |
| exploited | TOTAL predation | 2.116e+06 | 1.173e+06 | 3.7e+06 | 0.2429 | 8.872e+06 |
| exploited | shelf and coastal fishes | 1.11e+06 | 5.775e+05 | 1.96e+06 | 0.1274 | 8.872e+06 |
| exploited | bathypelagic fishes | 4.087e+05 | 1.886e+05 | 8.117e+05 | 0.0452 | 8.872e+06 |
| exploited | mesopelagic fishes | 4.047e+05 | 1.986e+05 | 6.945e+05 | 0.03764 | 8.872e+06 |
| exploited | other macrozooplankton | 4.849e+04 | 3.068e+04 | 6.917e+04 | 0.005536 | 8.872e+06 |
| exploited | squids | 2.663e+04 | 1.458e+04 | 6.011e+04 | 0.003418 | 8.872e+06 |
| unexploited | TOTAL mortality | 5.649e+06 | 2.978e+06 | 9.341e+06 | 0.6134 | 9.184e+06 |
| unexploited | BACKGROUND | 3.423e+06 | 1.775e+06 | 5.743e+06 | 0.3727 | 9.184e+06 |
| unexploited | TOTAL predation | 2.101e+06 | 1.177e+06 | 3.686e+06 | 0.2407 | 9.184e+06 |
| unexploited | shelf and coastal fishes | 1.129e+06 | 5.912e+05 | 1.965e+06 | 0.1265 | 9.184e+06 |
| unexploited | bathypelagic fishes | 4.063e+05 | 1.934e+05 | 8.021e+05 | 0.0442 | 9.184e+06 |
| unexploited | mesopelagic fishes | 3.925e+05 | 1.967e+05 | 6.948e+05 | 0.03644 | 9.184e+06 |
| unexploited | other macrozooplankton | 4.908e+04 | 3.257e+04 | 7.163e+04 | 0.005517 | 9.184e+06 |
| unexploited | squids | 2.867e+04 | 1.524e+04 | 6.053e+04 | 0.003429 | 9.184e+06 |

### 2001-2010

| arm | mortality_source | removal_t_per_yr_median | removal_t_per_yr_q25 | removal_t_per_yr_q75 | mort_rate_per_yr_median | krill_biomass_t_median |
|---|---|---|---|---|---|---|
| exploited | TOTAL mortality | 5.605e+06 | 3.004e+06 | 9.181e+06 | 0.6161 | 8.997e+06 |
| exploited | BACKGROUND | 3.353e+06 | 1.756e+06 | 5.621e+06 | 0.3727 | 8.997e+06 |
| exploited | TOTAL predation | 2.126e+06 | 1.188e+06 | 3.711e+06 | 0.2434 | 8.997e+06 |
| exploited | shelf and coastal fishes | 1.139e+06 | 6.036e+05 | 1.979e+06 | 0.1294 | 8.997e+06 |
| exploited | bathypelagic fishes | 4.184e+05 | 1.912e+05 | 8.06e+05 | 0.04436 | 8.997e+06 |
| exploited | mesopelagic fishes | 4.029e+05 | 1.947e+05 | 7.019e+05 | 0.03713 | 8.997e+06 |
| exploited | other macrozooplankton | 4.861e+04 | 3.121e+04 | 7.086e+04 | 0.005437 | 8.997e+06 |
| exploited | squids | 2.731e+04 | 1.524e+04 | 6.047e+04 | 0.003444 | 8.997e+06 |
| unexploited | TOTAL mortality | 5.62e+06 | 3.017e+06 | 9.323e+06 | 0.6151 | 9.169e+06 |
| unexploited | BACKGROUND | 3.417e+06 | 1.781e+06 | 5.714e+06 | 0.3727 | 9.169e+06 |
| unexploited | TOTAL predation | 2.1e+06 | 1.187e+06 | 3.688e+06 | 0.2425 | 9.169e+06 |
| unexploited | shelf and coastal fishes | 1.14e+06 | 6.074e+05 | 1.979e+06 | 0.1278 | 9.169e+06 |
| unexploited | bathypelagic fishes | 4.161e+05 | 1.915e+05 | 7.943e+05 | 0.04378 | 9.169e+06 |
| unexploited | mesopelagic fishes | 3.883e+05 | 1.941e+05 | 6.987e+05 | 0.03598 | 9.169e+06 |
| unexploited | other macrozooplankton | 4.895e+04 | 3.284e+04 | 7.285e+04 | 0.005433 | 9.169e+06 |
| unexploited | squids | 2.904e+04 | 1.588e+04 | 6.105e+04 | 0.003469 | 9.169e+06 |

### Paired change, by source

| period | mortality_source | delta_removal_t_per_yr_median | delta_removal_t_per_yr_q25 | delta_removal_t_per_yr_q75 | delta_rate_per_yr_median | delta_krill_biomass_pct_median |
|---|---|---|---|---|---|---|
| 1841-1860 | mesozooplankton |     0 |     0 |     0 |     0 |     0 |
| 1841-1860 | other krill |     0 |     0 |     0 |     0 |     0 |
| 1841-1860 | other macrozooplankton |     0 |     0 |     0 |     0 |     0 |
| 1841-1860 | antarctic krill |     0 |     0 |     0 |     0 |     0 |
| 1841-1860 | salps |     0 |     0 |     0 |     0 |     0 |
| 1841-1860 | mesopelagic fishes |     0 |     0 |     0 |     0 |     0 |
| 1930-1970 | BACKGROUND | -1.938e+04 | -3.253e+04 | -1.055e+04 |     0 | -0.4885 |
| 1930-1970 | TOTAL mortality | -1.219e+04 | -2.744e+04 | -6377 | 0.001711 | -0.4885 |
| 1930-1970 | TOTAL predation |  3426 | -652.6 | 1.245e+04 | 0.001711 | -0.4885 |
| 1930-1970 | bathypelagic fishes |  3118 | 496.7 |  8500 | 0.0005731 | -0.4885 |
| 1930-1970 | mesopelagic fishes |  1396 | 58.74 |  4587 | 0.00039 | -0.4885 |
| 1930-1970 | shelf and coastal fishes | -642.8 | -3421 | 473.9 | 0.0004826 | -0.4885 |
| 1974-1990 | BACKGROUND | -5.89e+04 | -1.037e+05 | -3.556e+04 |     0 | -1.856 |
| 1974-1990 | TOTAL mortality | -5.596e+04 | -1.057e+05 | -2.993e+04 | 0.004162 | -1.856 |
| 1974-1990 | TOTAL predation | -1.191e+04 | -2.722e+04 |  2330 | 0.002297 | -1.856 |
| 1974-1990 | shelf and coastal fishes | -1.152e+04 | -2.028e+04 | -3697 | 0.0006803 | -1.856 |
| 1974-1990 | FISHING |  8475 |  4054 | 2.158e+04 | 0.001283 | -1.856 |
| 1974-1990 | mesopelagic fishes | -694.6 | -5035 |  2247 | 0.0005631 | -1.856 |
| 2001-2010 | BACKGROUND | -3.212e+04 | -6.216e+04 | -1.833e+04 |     0 | -0.8337 |
| 2001-2010 | TOTAL mortality | -2.189e+04 | -6.275e+04 | -1.319e+04 | 0.002099 | -0.8337 |
| 2001-2010 | bathypelagic fishes |  3015 |  -174 |  8979 | 0.0007571 | -0.8337 |
| 2001-2010 | TOTAL predation |  2430 | -6402 | 1.334e+04 | 0.002099 | -0.8337 |
| 2001-2010 | shelf and coastal fishes | -2162 | -9003 | -49.4 | 0.0005957 | -0.8337 |
| 2001-2010 | mesopelagic fishes |  1635 | -411.5 |  5957 | 0.0005251 | -0.8337 |

### What this implies for the mechanism

Krill fishing effort in this model runs **1974–1996 only** and is zero throughout 2001–2010, and the fitted krill catchability is small, so direct fishing mortality on krill is negligible in every period (see the FISHING rows above). Whatever moves the krill stock must therefore act through predation mortality or through the growth/reproduction terms, and the per-predator rows say which predators changed.

**Background mortality is the largest single term.** At 2001-2010 the unexploited krill mortality budget is total 0.615 yr⁻¹, of which background (`mu_b`, a fixed per-size rate) is 0.373 yr⁻¹ (60.6%) and all resolved predation is 0.242 yr⁻¹ (39.4%). Because `mu_b` is a *rate*, its removal in tonnes changes when krill biomass changes but its rate never does — that is why the BACKGROUND row in the paired-delta table shows a large tonnage change against a rate change of exactly zero. The corollary is that **the majority of krill mortality in this model is not attributable to any modelled predator**, which caps how much of a krill signal any predator change can generate.

**Direction of the whaling-era change.** Total predation on krill *rises* by  3426 t yr⁻¹ under exploitation over 1930-1970, driven by bathypelagic ( 3118) and mesopelagic fishes ( 1396), and partly offset by shelf and coastal fishes (-642.8). So the krill decline from 1930 in this model **is** driven by increased predation mortality, and the predators responsible are mid-water fishes — not whales, whose own krill take is four orders of magnitude smaller than the change itself.

---

## E. Compensation biomass budget

Community biomass above 1 t of individual body mass lost under exploitation, against biomass gained below 1 t. The 1 t threshold falls between size bins   87 (9.458e+05 g) and   88 (1.357e+06 g); the straddling bin is split linearly (`boundary_bin = "split"`, the convention transcribed from `R/wmin_test/06_stage0_robustness.R:36`), with an unsplit variant (`"hard"`) reported alongside as a sensitivity.

| period | boundary_bin | quantity | unit | median | q25 | q75 | p05 | p95 |
|---|---|---|---|---|---|---|---|---|
| 1960-1970 | split | delta biomass above 1 t | t | -2.69e+05 | -4.145e+05 | -1.527e+05 | -2.098e+06 | -7.676e+04 |
| 1960-1970 | split | delta biomass below 1 t | t | 3.156e+04 |  7989 | 6.503e+04 | -6.814e+04 | 2.711e+05 |
| 1960-1970 | split | residual (above + below) | t | -2.231e+05 | -3.858e+05 | -1.277e+05 | -2.141e+06 | -6.094e+04 |
| 1960-1970 | split | offset fraction (gain below / |loss above|) | fraction | 0.1519 | 0.03202 | 0.2457 | -0.1802 | 0.378 |
| 1960-1970 | hard | delta biomass above 1 t | t | -2.69e+05 | -4.145e+05 | -1.526e+05 | -2.098e+06 | -7.675e+04 |
| 1960-1970 | hard | delta biomass below 1 t | t | 3.153e+04 |  7979 | 6.502e+04 | -6.815e+04 | 2.71e+05 |
| 1960-1970 | hard | residual (above + below) | t | -2.231e+05 | -3.858e+05 | -1.277e+05 | -2.141e+06 | -6.094e+04 |
| 1960-1970 | hard | offset fraction (gain below / |loss above|) | fraction | 0.1519 | 0.03198 | 0.2457 | -0.1803 | 0.378 |
| 2001-2010 | split | delta biomass above 1 t | t | -2.569e+05 | -4.13e+05 | -1.561e+05 | -2.008e+06 | -6.835e+04 |
| 2001-2010 | split | delta biomass below 1 t | t | 3.182e+04 | 610.6 | 7.533e+04 | -1.389e+05 | 2.713e+05 |
| 2001-2010 | split | residual (above + below) | t | -2.126e+05 | -3.994e+05 | -1.35e+05 | -2.016e+06 | -6.998e+04 |
| 2001-2010 | split | offset fraction (gain below / |loss above|) | fraction | 0.146 | 0.001364 | 0.2583 | -0.4121 | 0.4072 |
| 2001-2010 | hard | delta biomass above 1 t | t | -2.569e+05 | -4.131e+05 | -1.561e+05 | -2.008e+06 | -6.855e+04 |
| 2001-2010 | hard | delta biomass below 1 t | t | 3.206e+04 | 930.8 | 7.53e+04 | -1.389e+05 | 2.712e+05 |
| 2001-2010 | hard | residual (above + below) | t | -2.126e+05 | -3.994e+05 | -1.35e+05 | -2.016e+06 | -6.998e+04 |
| 2001-2010 | hard | offset fraction (gain below / |loss above|) | fraction | 0.1462 | 0.002644 | 0.2585 | -0.4122 | 0.4072 |

Per-group breakdowns are in `E_compensation_budget_2001_2010.csv` and `E_compensation_budget_1960_1970.csv`.

### What this implies for the mechanism

The **offset fraction** is the headline: gains below 1 t divided by the magnitude of losses above 1 t. A value near 1 means the community is biomass-conserving — what the great whales lost was taken up by smaller classes; a value well below 1 means exploitation removed biomass from the system without compensation.

- **1960-1970**: 2.69e+05 t lost above 1 t against 3.156e+04 t gained below, leaving a residual of -2.231e+05 t. **Offset fraction 0.152 [0.032-0.246], 5-95% [-0.18-0.378]** — so only about  15% of the biomass removed from the large classes reappears in the small ones.
- **2001-2010**: 2.569e+05 t lost above 1 t against 3.182e+04 t gained below, leaving a residual of -2.126e+05 t. **Offset fraction 0.146 [0.00136-0.258], 5-95% [-0.412-0.407]** — so only about  15% of the biomass removed from the large classes reappears in the small ones.

**The community is not biomass-conserving under exploitation.** Roughly six-sevenths of the great-whale biomass removed is simply absent from the system; it is not redistributed downward. Two caveats on reading that. First, the 5th percentile of the offset fraction is **negative** in both periods (-0.18 and -0.412), meaning that in a minority of members the classes below 1 t *also* lose biomass — exploitation propagates down rather than compensating. Second, the split-vs-hard boundary treatment changes nothing (the two rows agree to three significant figures), so the result is not an artefact of where the 1 t threshold falls between size bins.

---

## F. Baleen whale feeding kernel parameters

Verified constant across all 167 members: the Monte Carlo perturbs only `gamma`, `catchability` and `abundance_scaling` (`R/wmin_test/43_recover_member_draws.R:6-13`), so no kernel parameter is a drawn quantity. Full table in `F_feeding_kernel_parameters.csv`.

### F.1 What is actually used — the box kernel

**Baleen whales and minke whales are the only two of the 19 groups with `pred_kernel_type = "box"`.** For those two the lognormal parameters `beta` and `sigma` are present in `species_params` but never evaluated; the operative parameters are `ppmr_min` and `ppmr_max`, which define a hard prey-mass window [w/ppmr_max, w/ppmr_min] with kernel value exactly 1 inside and exactly 0 outside.

| species | ppmr_min | ppmr_max | w_min_g | w_mat_g | w_max_g | box_prey_lo_at_w_max_g | box_prey_hi_at_w_max_g | box_prey_lo_at_w_min_g | box_prey_hi_at_w_min_g | max_predator_mass_that_can_eat_krill_g |
|---|---|---|---|---|---|---|---|---|---|---|
| minke whales | 1e+05 | 5e+06 | 6e+05 | 5.4e+06 | 6e+06 |   1.2 |    60 |  0.12 |     6 | 2.087e+07 |
| baleen whales | 1e+05 | 5e+06 | 2.25e+06 | 5.07e+07 | 1.03e+08 |  20.6 |  1030 |  0.45 |  22.5 | 2.087e+07 |

Antarctic krill spans 1.1e-06 g to 4.173 g. Consequences, read straight off the window:

- A baleen whale **at its asymptotic size (  103 t)** has a preferred prey window of ** 20.6– 1030 g** — entirely above the Antarctic krill size range. **The largest baleen whales cannot eat Antarctic krill at all in this model.**
- Baleen whales can access krill only below 20.87 t, which is below `w_mat` ( 50.7 t) — so **only immature baleen whales take krill**, and only the largest krill.
- Minke whales (`w_max`    6 t) sit entirely below that ceiling, so their whole size range can take krill. That is why minke whales out-consume baleen whales on krill despite the far smaller stock.

### F.2 What is stored but not used — the lognormal parameters

For completeness, the stored `beta`/`sigma` and the geometry they *would* imply if the kernel were lognormal (peak at w/beta, ±1σ at w/beta·e^∓σ):

| species | operative_kernel | beta_stored | sigma_stored | beta_is_used | lognormal_pref_prey_at_w_max_g | lognormal_prey_minus1sd_at_w_max_g | lognormal_prey_plus1sd_at_w_max_g | krill_sd_from_pref_at_w_max |
|---|---|---|---|---|---|---|---|---|
| minke whales | box | 5.042e+06 |     2 | FALSE |  1.19 | 0.161 | 8.793 | -0.6274 |
| orca | lognormal | 0.5579 |   2.5 | TRUE | 1.905e+07 | 1.564e+06 | 2.321e+08 | 6.134 |
| sperm whales | lognormal | 4.408e+04 |     2 | TRUE |   828 | 112.1 |  6118 | 2.645 |
| baleen whales | box | 2.191e+05 |     2 | FALSE | 470.1 | 63.62 |  3473 | 2.362 |

Even under the lognormal reading the conclusion would not change sign: a baleen whale at `w_max` would prefer 470.1 g prey, putting maximum-size Antarctic krill 2.36 kernel σ away. The box makes it a hard zero rather than a small number.

### F.3 How the whales differ from the other groups

| species | beta_stored | sigma_stored | w_max_g | lognormal_pref_prey_at_w_max_g |
|---|---|---|---|---|
| mesozooplankton | 530.9 |     2 | 0.003162 | 5.957e-06 |
| other krill | 1.585e+07 |     2 | 0.342 | 2.158e-08 |
| other macrozooplankton | 446.7 |     2 |     1 | 0.002239 |
| antarctic krill | 1.585e+07 |     2 | 4.173 | 2.633e-07 |
| salps | 1.778e+09 |     2 | 25.12 | 1.413e-08 |
| mesopelagic fishes | 441.7 |     2 |   240 | 0.5434 |
| bathypelagic fishes |   425 |     2 | 603.7 |  1.42 |
| shelf and coastal fishes |   350 |     2 |  2422 | 6.921 |
| flying birds | 58.14 |     2 |  4191 |  72.1 |
| small divers | 293.8 |     2 |  6000 | 20.42 |
| squids |    50 |     2 | 2.072e+04 | 414.4 |
| toothfishes |   100 |     2 | 1.576e+05 |  1576 |
| leopard seals |   100 |     3 | 4.5e+05 |  4500 |
| medium divers | 686.2 |     2 | 1.283e+06 |  1870 |
| large divers |  3610 |     2 | 2.024e+06 | 560.6 |
| orca | 0.5579 |   2.5 | 1.063e+07 | 1.905e+07 |
| sperm whales | 4.408e+04 |     2 | 3.65e+07 |   828 |

The other 17 groups all use the lognormal kernel with `sigma` = 2.0 except leopard seals (3.0) and orca (2.5). There is **no allometric default in force**: `beta` is a per-group table value, not a function of body size, and it spans 0.558 to 1.78e+09 across the 19 groups. For reference, mizer's own package default is `beta = 100, sigma = 1.3`.

### F.4 Source and justification recorded in the codebase

- `group params/trait_groups_params_vCWC_v5.csv` carries the per-group `beta` column, including the whale values (baleen 2.191126e5, minke 5.042017e6); this is the trait table the params object is built from. No `sigma` column — sigma is set in the setup scripts.
- `03_model_setup_pre_therMizer.rmd:1027-1039` — zooplankton `beta` values are sourced from **Heneghan et al. 2020** (*Ecological Modelling*, doi:10.1016/j.ecolmodel.2020.109265) log10 PPMR values; `04_therMizer_calibration_scale_g_m2.Rmd:150` repeats this for all zooplankton except salps.
- The operative **1e5 / 5e6** pair is set at `model_setup_old/New_steady_state_07_06_2024.Rmd:263-267`, applied identically to baleen and minke whales. Verified by reading the stored objects: `params/params_07_06_2024.rds` (that script's output) and `params_sel_adj.rds` (the Monte Carlo seed) both carry `box / beta 219112.6 / sigma 2 / ppmr 1e5-5e6` for baleen and `box / beta 5042016.8 / sigma 2 / ppmr 1e5-5e6` for minke — identical to every one of the 167 ensemble members.
- **The box type itself is not reproducible from the live setup script.** `03_model_setup_pre_therMizer.rmd:1379` — the `pred_kernel_type <- "box"` assignment for baleen whales — is **commented out**, and the `ppmr_min = 4e6` it sets at `:1377` does not survive into any stored object. The only live `"box"` assignment anywhere in the repository is for **minke whales** at `model_setup_old/New_steady_state_07_06_2024.Rmd:179` (with `ppmr_max = 5e7`, later overwritten to 5e6 at `:263-267`). Baleen whales' box type is inherited from a stored params object, traceable only to the archived `optim_model_setup_old/model_setup*.Rmd` lineage (e.g. `model_setup.Rmd:156, :257, :623`). So the single most consequential structural parameter in this report cannot be regenerated by running the current setup scripts.
- The stated rationale is at `optim_model_setup_old/model_setup.Rmd:233-234`: *"max ppmr value are based on w_max baleen whale feeding on w_min euphausiids; min ppmr value based on w_min baleen whale feeding on w_max euphausiids"*. **That rationale and the implemented numbers do not agree.** The stated construction gives ppmr_max = w_max(baleen)/w_min(krill) = 9.327e+13 and ppmr_min = w_min(baleen)/w_max(krill) = 5.392e+05, against the implemented 5e6 and 1e5. Implementing the stated rationale would make every baleen whale size class able to eat krill; the implemented ceiling of 5e6 excludes all baleen whales above 20.87 t. Earlier versions did carry values close to the stated construction (`optim_model_setup_old/model_setup.Rmd:252-253`: `ppmr_min = 711512.4`, `ppmr_max = 1.63233e12`) and were commented out.
- No numeric justification for the whale `beta`/`sigma` values themselves is recorded anywhere in the repository; they are inert for these two groups in any case.

---

## G. Refreshed headline numbers, n = 167

| quantity | unit | median | q25 | q75 | p05 | p95 |
|---|---|---|---|---|---|---|
| krill consumption ratio, Large baleen + minke whales, 2010 | ratio | 0.4245 | 0.2813 | 0.5733 | 0.1864 | 0.8149 |
| krill consumption ratio, Large baleen + minke whales, 2001-2010 mean | ratio | 0.4343 | 0.2916 | 0.5809 | 0.1944 | 0.8114 |
| implied fold increase to close the gap, Large baleen + minke whales, 2001-2010 | factor | 2.302 | 1.721 | 3.431 | 1.233 | 5.144 |
| absolute deficit (unexploited - exploited), Large baleen + minke whales, 2001-2010 | t/yr | 546.4 | 238.7 |  1123 | 76.16 |  2614 |
| krill consumption ratio, All predators, 2010 | ratio | 1.001 | 0.9952 | 1.003 | 0.9011 | 1.014 |
| krill consumption ratio, All predators, 2001-2010 mean | ratio | 1.001 | 0.9946 | 1.004 | 0.901 | 1.014 |
| implied fold increase to close the gap, All predators, 2001-2010 | factor | 0.9991 | 0.9965 | 1.005 | 0.9863 |  1.11 |
| absolute deficit (unexploited - exploited), All predators, 2001-2010 | t/yr | -2430 | -1.334e+04 |  6402 | -5.52e+04 | 5.457e+04 |
| krill consumption ratio, All fishes, 2010 | ratio | 1.002 | 0.9962 | 1.005 | 0.9034 | 1.017 |
| krill consumption ratio, All fishes, 2001-2010 mean | ratio | 1.002 | 0.9955 | 1.004 | 0.9032 | 1.017 |
| implied fold increase to close the gap, All fishes, 2001-2010 | factor | 0.9984 | 0.9956 | 1.005 | 0.9837 | 1.107 |
| absolute deficit (unexploited - exploited), All fishes, 2001-2010 | t/yr | -3380 | -1.594e+04 |  4605 | -5.948e+04 | 5.094e+04 |
| abundance ratio, baleen whales, 2001-2010 mean | ratio | 0.4565 | 0.2856 | 0.6724 | 0.1011 | 0.8541 |
| abundance ratio, baleen whales, 2010 | ratio | 0.4592 | 0.2878 | 0.6731 | 0.1024 | 0.8547 |
| abundance ratio, sperm whales, 2001-2010 mean | ratio | 0.1254 | 0.04667 | 0.2222 | 0.008092 | 0.5942 |
| abundance ratio, sperm whales, 2010 | ratio | 0.1312 | 0.04939 | 0.2242 | 0.008789 | 0.597 |
| abundance ratio, minke whales, 2001-2010 mean | ratio | 0.4715 | 0.3314 | 0.6259 | 0.2279 | 0.8264 |
| abundance ratio, minke whales, 2010 | ratio | 0.4622 | 0.3196 | 0.6171 | 0.2176 | 0.8304 |
| abundance ratio, orca, 2001-2010 mean | ratio | 0.8252 | 0.6738 | 0.9131 | 0.3903 | 0.9799 |
| abundance ratio, orca, 2010 | ratio | 0.8275 | 0.6776 | 0.9142 | 0.398 | 0.9801 |

---

## H. Sensitivity to the RMSE ranking threshold

The four subsets are prefixes of one ranking — the post-catchability-refit pooled log10 yield RMSE that defines cut A — so no member is re-scored between rows.

| quantity | subset | unit | median | q25 | q75 |
|---|---|---|---|---|---|
| absolute deficit (unexploited - exploited), All fishes, 2001-2010 | n=1668 | t/yr | -1177 | -9556 |  2085 |
| absolute deficit (unexploited - exploited), All fishes, 2001-2010 | n=167 | t/yr | -3380 | -1.594e+04 |  4605 |
| absolute deficit (unexploited - exploited), All fishes, 2001-2010 | n=417 | t/yr | -2584 | -1.241e+04 |  3161 |
| absolute deficit (unexploited - exploited), All fishes, 2001-2010 | n=834 | t/yr | -2091 | -1.123e+04 |  2392 |
| absolute deficit (unexploited - exploited), All predators, 2001-2010 | n=1668 | t/yr | -140.3 | -7485 |  2993 |
| absolute deficit (unexploited - exploited), All predators, 2001-2010 | n=167 | t/yr | -2430 | -1.334e+04 |  6402 |
| absolute deficit (unexploited - exploited), All predators, 2001-2010 | n=417 | t/yr | -1461 | -9975 |  4047 |
| absolute deficit (unexploited - exploited), All predators, 2001-2010 | n=834 | t/yr | -914.4 | -8682 |  3214 |
| absolute deficit (unexploited - exploited), Large baleen + minke whales, 2001-2010 | n=1668 | t/yr | 482.7 | 138.4 |  1145 |
| absolute deficit (unexploited - exploited), Large baleen + minke whales, 2001-2010 | n=167 | t/yr | 546.4 | 238.7 |  1123 |
| absolute deficit (unexploited - exploited), Large baleen + minke whales, 2001-2010 | n=417 | t/yr |   551 | 237.9 |  1212 |
| absolute deficit (unexploited - exploited), Large baleen + minke whales, 2001-2010 | n=834 | t/yr | 538.2 | 234.9 |  1205 |
| abundance ratio, baleen whales, 2001-2010 mean | n=1668 | ratio | 0.6028 | 0.2431 | 0.8566 |
| abundance ratio, baleen whales, 2001-2010 mean | n=167 | ratio | 0.4565 | 0.2856 | 0.6724 |
| abundance ratio, baleen whales, 2001-2010 mean | n=417 | ratio | 0.4682 | 0.2774 | 0.6916 |
| abundance ratio, baleen whales, 2001-2010 mean | n=834 | ratio | 0.5361 | 0.2844 | 0.7629 |
| abundance ratio, baleen whales, 2010 | n=1668 | ratio | 0.6044 | 0.2464 | 0.8576 |
| abundance ratio, baleen whales, 2010 | n=167 | ratio | 0.4592 | 0.2878 | 0.6731 |
| abundance ratio, baleen whales, 2010 | n=417 | ratio | 0.4712 | 0.2779 | 0.6932 |
| abundance ratio, baleen whales, 2010 | n=834 | ratio | 0.5375 | 0.2867 | 0.7633 |
| abundance ratio, minke whales, 2001-2010 mean | n=1668 | ratio | 0.4733 | 0.3312 | 0.7195 |
| abundance ratio, minke whales, 2001-2010 mean | n=167 | ratio | 0.4715 | 0.3314 | 0.6259 |
| abundance ratio, minke whales, 2001-2010 mean | n=417 | ratio | 0.4406 | 0.3386 | 0.6062 |
| abundance ratio, minke whales, 2001-2010 mean | n=834 | ratio | 0.4501 | 0.3365 | 0.6395 |
| abundance ratio, minke whales, 2010 | n=1668 | ratio | 0.4615 | 0.3191 | 0.7148 |
| abundance ratio, minke whales, 2010 | n=167 | ratio | 0.4622 | 0.3196 | 0.6171 |
| abundance ratio, minke whales, 2010 | n=417 | ratio | 0.4302 | 0.3248 | 0.5976 |
| abundance ratio, minke whales, 2010 | n=834 | ratio | 0.4359 | 0.324 | 0.6316 |
| abundance ratio, orca, 2001-2010 mean | n=1668 | ratio | 0.8352 | 0.6317 | 0.9344 |
| abundance ratio, orca, 2001-2010 mean | n=167 | ratio | 0.8252 | 0.6738 | 0.9131 |
| abundance ratio, orca, 2001-2010 mean | n=417 | ratio | 0.8302 |  0.67 | 0.9184 |
| abundance ratio, orca, 2001-2010 mean | n=834 | ratio | 0.8285 | 0.6779 | 0.9171 |
| abundance ratio, orca, 2010 | n=1668 | ratio | 0.8372 | 0.6365 | 0.9352 |
| abundance ratio, orca, 2010 | n=167 | ratio | 0.8275 | 0.6776 | 0.9142 |
| abundance ratio, orca, 2010 | n=417 | ratio | 0.8326 | 0.6732 | 0.9194 |
| abundance ratio, orca, 2010 | n=834 | ratio | 0.8305 | 0.6824 | 0.9182 |
| abundance ratio, sperm whales, 2001-2010 mean | n=1668 | ratio | 0.0401 | 0.02376 | 0.1925 |
| abundance ratio, sperm whales, 2001-2010 mean | n=167 | ratio | 0.1254 | 0.04667 | 0.2222 |
| abundance ratio, sperm whales, 2001-2010 mean | n=417 | ratio | 0.08555 | 0.02708 | 0.1858 |
| abundance ratio, sperm whales, 2001-2010 mean | n=834 | ratio | 0.05491 | 0.02401 | 0.1591 |
| abundance ratio, sperm whales, 2010 | n=1668 | ratio | 0.04424 | 0.0267 | 0.196 |
| abundance ratio, sperm whales, 2010 | n=167 | ratio | 0.1312 | 0.04939 | 0.2242 |
| abundance ratio, sperm whales, 2010 | n=417 | ratio | 0.09047 | 0.02934 | 0.1913 |
| abundance ratio, sperm whales, 2010 | n=834 | ratio | 0.05952 | 0.027 | 0.1658 |
| implied fold increase to close the gap, All fishes, 2001-2010 | n=1668 | factor | 0.9995 | 0.9975 | 1.002 |
| implied fold increase to close the gap, All fishes, 2001-2010 | n=167 | factor | 0.9984 | 0.9956 | 1.005 |
| implied fold increase to close the gap, All fishes, 2001-2010 | n=417 | factor | 0.9989 | 0.9968 | 1.003 |
| implied fold increase to close the gap, All fishes, 2001-2010 | n=834 | factor | 0.9992 | 0.997 | 1.002 |
| implied fold increase to close the gap, All predators, 2001-2010 | n=1668 | factor | 0.9999 | 0.9982 | 1.003 |
| implied fold increase to close the gap, All predators, 2001-2010 | n=167 | factor | 0.9991 | 0.9965 | 1.005 |
| implied fold increase to close the gap, All predators, 2001-2010 | n=417 | factor | 0.9994 | 0.9975 | 1.004 |
| implied fold increase to close the gap, All predators, 2001-2010 | n=834 | factor | 0.9997 | 0.9977 | 1.003 |
| implied fold increase to close the gap, Large baleen + minke whales, 2001-2010 | n=1668 | factor | 2.298 | 1.473 | 3.358 |
| implied fold increase to close the gap, Large baleen + minke whales, 2001-2010 | n=167 | factor | 2.302 | 1.721 | 3.431 |
| implied fold increase to close the gap, Large baleen + minke whales, 2001-2010 | n=417 | factor | 2.499 | 1.776 | 3.318 |
| implied fold increase to close the gap, Large baleen + minke whales, 2001-2010 | n=834 | factor |  2.45 | 1.693 |  3.32 |
| krill consumption ratio, All fishes, 2001-2010 mean | n=1668 | ratio |     1 | 0.9976 | 1.003 |
| krill consumption ratio, All fishes, 2001-2010 mean | n=167 | ratio | 1.002 | 0.9955 | 1.004 |
| krill consumption ratio, All fishes, 2001-2010 mean | n=417 | ratio | 1.001 | 0.9972 | 1.003 |
| krill consumption ratio, All fishes, 2001-2010 mean | n=834 | ratio | 1.001 | 0.9976 | 1.003 |
| krill consumption ratio, All fishes, 2010 | n=1668 | ratio |     1 | 0.9977 | 1.002 |
| krill consumption ratio, All fishes, 2010 | n=167 | ratio | 1.002 | 0.9962 | 1.005 |
| krill consumption ratio, All fishes, 2010 | n=417 | ratio | 1.001 | 0.9975 | 1.003 |
| krill consumption ratio, All fishes, 2010 | n=834 | ratio | 1.001 | 0.9979 | 1.003 |
| krill consumption ratio, All predators, 2001-2010 mean | n=1668 | ratio |     1 | 0.9971 | 1.002 |
| krill consumption ratio, All predators, 2001-2010 mean | n=167 | ratio | 1.001 | 0.9946 | 1.004 |
| krill consumption ratio, All predators, 2001-2010 mean | n=417 | ratio | 1.001 | 0.9963 | 1.003 |
| krill consumption ratio, All predators, 2001-2010 mean | n=834 | ratio |     1 | 0.9971 | 1.002 |
| krill consumption ratio, All predators, 2010 | n=1668 | ratio |     1 | 0.9972 | 1.002 |
| krill consumption ratio, All predators, 2010 | n=167 | ratio | 1.001 | 0.9952 | 1.003 |
| krill consumption ratio, All predators, 2010 | n=417 | ratio | 1.001 | 0.9968 | 1.002 |
| krill consumption ratio, All predators, 2010 | n=834 | ratio |     1 | 0.9973 | 1.002 |
| krill consumption ratio, Large baleen + minke whales, 2001-2010 mean | n=1668 | ratio | 0.4352 | 0.2978 | 0.6791 |
| krill consumption ratio, Large baleen + minke whales, 2001-2010 mean | n=167 | ratio | 0.4343 | 0.2916 | 0.5809 |
| krill consumption ratio, Large baleen + minke whales, 2001-2010 mean | n=417 | ratio | 0.4001 | 0.3013 | 0.563 |
| krill consumption ratio, Large baleen + minke whales, 2001-2010 mean | n=834 | ratio | 0.4081 | 0.3012 | 0.5907 |
| krill consumption ratio, Large baleen + minke whales, 2010 | n=1668 | ratio | 0.4246 | 0.2851 | 0.6754 |
| krill consumption ratio, Large baleen + minke whales, 2010 | n=167 | ratio | 0.4245 | 0.2813 | 0.5733 |
| krill consumption ratio, Large baleen + minke whales, 2010 | n=417 | ratio | 0.3901 | 0.2901 | 0.5545 |
| krill consumption ratio, Large baleen + minke whales, 2010 | n=834 | ratio | 0.3962 | 0.2895 | 0.5819 |

### Stability

| quantity | sign_stable | med_min | med_max | fold_range |
|---|---|---|---|---|
| absolute deficit (unexploited - exploited), All predators, 2001-2010 | TRUE | -2430 | -140.3 | 17.32 |
| abundance ratio, sperm whales, 2001-2010 mean | TRUE | 0.0401 | 0.1254 | 3.128 |
| abundance ratio, sperm whales, 2010 | TRUE | 0.04424 | 0.1312 | 2.966 |
| absolute deficit (unexploited - exploited), All fishes, 2001-2010 | TRUE | -3380 | -1177 | 2.871 |
| abundance ratio, baleen whales, 2001-2010 mean | TRUE | 0.4565 | 0.6028 | 1.321 |
| abundance ratio, baleen whales, 2010 | TRUE | 0.4592 | 0.6044 | 1.316 |
| absolute deficit (unexploited - exploited), Large baleen + minke whales, 2001-2010 | TRUE | 482.7 |   551 | 1.141 |
| krill consumption ratio, Large baleen + minke whales, 2010 | TRUE | 0.3901 | 0.4246 | 1.088 |
| implied fold increase to close the gap, Large baleen + minke whales, 2001-2010 | TRUE | 2.298 | 2.499 | 1.088 |
| krill consumption ratio, Large baleen + minke whales, 2001-2010 mean | TRUE | 0.4001 | 0.4352 | 1.088 |
| abundance ratio, minke whales, 2010 | TRUE | 0.4302 | 0.4622 | 1.075 |
| abundance ratio, minke whales, 2001-2010 mean | TRUE | 0.4406 | 0.4733 | 1.074 |
| abundance ratio, orca, 2001-2010 mean | TRUE | 0.8252 | 0.8352 | 1.012 |
| abundance ratio, orca, 2010 | TRUE | 0.8275 | 0.8372 | 1.012 |
| krill consumption ratio, All fishes, 2010 | TRUE |     1 | 1.002 | 1.001 |
| implied fold increase to close the gap, All fishes, 2001-2010 | TRUE | 0.9984 | 0.9995 | 1.001 |
| krill consumption ratio, All fishes, 2001-2010 mean | TRUE |     1 | 1.002 | 1.001 |
| krill consumption ratio, All predators, 2010 | TRUE |     1 | 1.001 | 1.001 |
| implied fold increase to close the gap, All predators, 2001-2010 | TRUE | 0.9991 | 0.9999 | 1.001 |
| krill consumption ratio, All predators, 2001-2010 mean | TRUE |     1 | 1.001 | 1.001 |

**Sign is stable for every quantity at every threshold.** The headline is robust: the baleen + minke krill consumption ratio at 2010 moves only across 0.39–0.425 (a 1.09x range) as the subset widens from 167 to all 1,668 members, and the ordering of the three predator groups — whales far below 1, all predators and all fishes both at 1.00 — never changes. So the decile cut is not what produces the whale result.

Three quantities are threshold-sensitive and should not be quoted without the subset named:

- **Sperm whale abundance ratio** falls monotonically 0.125 → 0.0401 (3.13x) as the subset widens: the wider ensemble depicts a *deeper* sperm whale depletion than the best-fitting decile does.
- **Baleen whale abundance ratio** moves the other way, 0.456 → 0.603 (1.32x) — a less severe depletion in the wider ensemble. These two run in opposite directions, so this is a genuine change in which members are included, not a monotone widening of spread.
- **Absolute deficit for all predators** ranges -2430 to -140.3 t yr⁻¹ (17.3x). That fold range is not meaningful: the quantity is a small difference between two nearly equal totals and sits close to zero, with an IQR that spans zero at every threshold. Read it as "indistinguishable from zero" rather than as a 17-fold instability.

---

## Surprises and concerns

1. **Baleen and minke whales use a box feeding kernel, not the lognormal one.** This was not flagged anywhere in the analysis brief or in the figure scripts, and it is the single most consequential structural fact for every krill number in this report. Their stored `beta`/`sigma` are inert. Anyone reading `species_params$beta` for these two groups is reading a value the model never uses.

2. **The implemented whale PPMR window contradicts its own stated rationale** (section F.4). The comment says the window was built so that whales across their whole size range can eat krill across theirs; the implemented `ppmr_max = 5e6` does the opposite, excluding all baleen whales above 20.87 t — which is below maturity. This looks like an unintended consequence of a later parameter edit rather than a considered choice, and it is worth resolving before the krill narrative is written. Compounding this, the baleen whale `"box"` assignment is **commented out** in the live setup script (`03_model_setup_pre_therMizer.rmd:1379`), so the parameter that drives every whale krill number here cannot be regenerated by running the current setup lineage — it survives only inside stored params objects.

3. **Whales are a rounding error in krill predation** (0.0564 [0.0344-0.075], 5-95% [0.0185-0.108]% pre-exploitation). Any manuscript sentence built on whales as the dominant pre-exploitation krill consumer needs rewriting, or the kernel needs revisiting. Note this is a *model structure* result, not an ensemble-uncertainty result — the spread across 167 members is narrow because the kernel is not a drawn parameter.

4. **The model's baleen whales feed mostly on the exogenous forcing.** 69.3% of baleen whale intake and 84.3% of minke intake comes from the prescribed plankton `Resource`, which is not depleted by anything. Whale feeding is therefore largely donor-controlled, and removing whales frees very little real food. Combined with the box kernel, this means the model has **two independent structural reasons** why whaling cannot produce a krill response — worth stating explicitly in the methods rather than leaving as an emergent surprise.

5. **Whale ration is low by roughly an order of magnitude.** Total baleen whale intake divided by baleen whale standing stock is 0.542 [0.518-0.607], 5-95% [0.31-0.626] t of prey per t of whale per year over 1841–1860. Published field estimates for Antarctic baleen whales are of order 3–10 t per t per year once the seasonal feeding window is accounted for. This is a check on the whales, not on the krill, and it is consistent with a feeding kernel that excludes their principal real-world prey — but it should be reconciled before whale consumption is quoted in absolute terms.

6. **Competition release is not testable via the shared resource.** `resource_dynamics = "plankton_forcing"` makes the background spectrum a prescribed boundary condition, identical between arms to a relative difference of exactly    0. A depressed shared resource cannot occur by construction, so the C-hypothesis test rests entirely on the other four LTL groups.

7. **Direct krill fishing is negligible throughout.** Effort is non-zero only 1974–1996 and is exactly zero across the whole 2001–2010 contemporary window, so the contemporary krill numbers contain no direct krill-fishery signal at all — every contemporary difference is an indirect, food-web-mediated effect.

8. **The consumption:biomass turnover is low.** Krill standing stock scales to a plausible Southern-Ocean-equivalent (~ 120 Mt) but total predation on it is only 0.238 [0.215-0.271], 5-95% [0.194-0.312] yr⁻¹ of standing stock, giving a Southern-Ocean-equivalent consumption ~4–8x below published estimates. The stock is right and the flux through it is low; that is consistent with the kernel excluding the largest krill predators.

9. **The ensemble medians of per-member shares sum to 92.6354%, not 100%.** A median is not additive, and the gap here is large enough to notice because the three dominant fish groups have broad, overlapping share distributions — bathypelagic fishes span 6.09-36.9% and mesopelagic 7.78-44.2% across members, so which of them dominates is itself uncertain. Per-member shares sum to 100% exactly (max deviation 1.42e-14); no renormalisation has been applied. **Use the aggregate rows, not the sum of species medians**, when a partition has to add up.

10. **The absolute krill totals carry a 3x ensemble spread** (2.11e+06 [1.19e+06-3.68e+06], 5-95% [4.61e+05-5.49e+06] t yr⁻¹) while the whale *share* is tight (0.0564 [0.0344-0.075], 5-95% [0.0185-0.108]%). Shares are far better constrained than totals here, because the drawn parameters (`gamma`, `abundance_scaling`) move the whole community together while the feeding kernel that sets the partition is fixed. Quote shares with confidence; quote absolute tonnages with the interval attached.

---

## Files written

- `A_krill_partition_1841_1860.csv`
- `A_ltl_partition_1841_1860.csv`
- `A_predator_diet_composition_1841_1860.csv`
- `B_krill_delta_by_decade_bin.csv`
- `B_krill_delta_by_size_bin.csv`
- `B_top_contributions.csv`
- `C_ltl_pctchange_period_summary.csv`
- `C_ltl_pctchange_timeseries_1900_2010.csv`
- `C_resource_spectrum_check.csv`
- `D_krill_mortality_decomposition.csv`
- `D_krill_mortality_paired_delta.csv`
- `E_compensation_budget_1960_1970.csv`
- `E_compensation_budget_2001_2010.csv`
- `E_compensation_budget_summary.csv`
- `F_feeding_kernel_parameters.csv`
- `G_headline_ratios_n167.csv`
- `H_threshold_sensitivity.csv`
- `MA02_all_tables.rds`
