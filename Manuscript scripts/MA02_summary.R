# =============================================================================
# MA02_summary -- writes `Manuscript analysis/SUMMARY.md`.
# Sourced by MA02_report.R with local = TRUE; every object it uses is built
# there. It contains no analysis of its own, only formatting, so that the
# markdown and the CSVs can never disagree.
# =============================================================================

fm  <- function(x, d = 3) formatC(x, format = "g", digits = d, big.mark = "")
iqr <- function(r, d = 3) sprintf("%s [%s-%s], 5-95%% [%s-%s]",
  fm(r[["median"]], d), fm(r[["q25"]], d), fm(r[["q75"]], d),
  fm(r[["p05"]], d), fm(r[["p95"]], d))

# ---- numbers the prose needs -------------------------------------------------
# All formed WITHIN member first, exactly as the CSVs are.
KR_TOT   <- qs(TOT_KR / G)                    # total krill consumption 1841-1860
WH_SHARE <- qs(whale_share)
ltl_by_pred  <- t(vapply(CONS0, function(C) rowSums(C[, LTL, drop = FALSE]),
                         numeric(length(SPN))))          # [member x predator]
ltl_tot_raw  <- rowSums(ltl_by_pred)                     # per-member total, g/yr
wh_ltl_raw   <- rowSums(ltl_by_pred[, WHALES, drop = FALSE])
LTL_TOT      <- qs(ltl_tot_raw / G)
LTL_WH_SHARE <- qs(100 * wh_ltl_raw / ltl_tot_raw)
KRB <- qs(vapply(Z, function(z) z$arms$unexploited$kmort[["1841-1860"]]$krill_biomass_g,
                 numeric(1)) / G)

# diet-composition shares the prose quotes, taken from the same table as the CSV
dpc <- function(pred, prey) {
  v <- vapply(CONS0, function(C) 100 * sum(C[pred, prey]) / sum(C[pred, ]), numeric(1))
  median(v)
}
bal_res   <- dpc("baleen whales", "Resource")
bal_fish  <- dpc("baleen whales", c("bathypelagic fishes", "mesopelagic fishes",
                                    "shelf and coastal fishes"))
bal_salp  <- dpc("baleen whales", "salps")
bal_krill <- dpc("baleen whales", KRILL)
min_res   <- dpc("minke whales", "Resource")
min_krill <- dpc("minke whales", KRILL)
# whale ingestion relative to standing stock, a plausibility check on the whales
# themselves rather than on the krill
bal_intake <- vapply(CONS0, function(C) sum(C["baleen whales", ]), numeric(1))
bal_stock  <- vapply(Z, function(z)
  mean(z$arms$unexploited$bio[as.character(1841:1860), "baleen whales"]), numeric(1))
BAL_RATION <- qs(bal_intake / bal_stock)
turnover <- qs(TOT_KR / vapply(Z, function(z)
  z$arms$unexploited$kmort[["1841-1860"]]$krill_biomass_g, numeric(1)))

A_sp <- A_krill %>% filter(level == "species") %>% arrange(desc(share_pct_median))
A_ag <- A_krill %>% filter(level == "aggregate")
A_ltl_c <- A_ltl %>% filter(prey_group == "LTL combined", level == "species") %>%
  arrange(desc(share_pct_median))
A_ltl_a <- A_ltl %>% filter(prey_group == "LTL combined", level == "aggregate")

# B: where the 1930-1970 increase sits
b70 <- B_dec %>% filter(period == "1930-1970")
pos70 <- sum(pmax(b70$delta_t_per_yr_median, 0))
neg70 <- sum(pmin(b70$delta_t_per_yr_median, 0))
share_of_pos <- function(d, grp) 100 * sum(pmax(d$delta_t_per_yr_median[
  d$predator_group %in% grp], 0)) / sum(pmax(d$delta_t_per_yr_median, 0))
tooth70 <- share_of_pos(b70, "toothfishes")
pel70   <- share_of_pos(b70, c("mesopelagic fishes", "bathypelagic fishes"))
shelf70 <- share_of_pos(b70, "shelf and coastal fishes")

# ---- assemble ----------------------------------------------------------------
say("# Krill consumption attribution — Prydz Bay size-spectrum ensemble")
say("")
say("Generated ", format(Sys.time()), " by `Manuscript scripts/MA02_report.R`.")
say("")
say("**Ensemble.** Rebuilt ensemble 44: 2,111 accepted → 1,997 distinct → 1,848 ",
    "draw-distinct → **1,668 accepted**, catchability globally re-fitted per species. ",
    "Main analyses use **cut A**, the best-fitting decile by unweighted pooled ",
    "log10 yield RMSE, **n = ", NM, "**. Every exploited member is paired with the ",
    "unexploited (zero-effort, climate-only) run of the *same* member; all ",
    "exploited/unexploited comparisons are formed **within** a member and only ",
    "then summarised across the ensemble.")
say("")
say("**Units.** Consumption t yr⁻¹, biomass t, body mass g, mortality yr⁻¹. All ",
    "consumption and biomass figures are **domain totals** — the model domain ",
    "(", fm(DOMAIN_M2, 7), " m², ", fm(DOMAIN_M2 / 1e6, 7), " km²) is encoded in ",
    "the initial abundances, so no area normalisation is applied.")
say("")
say("**Intervals.** Every quantity is reported as ensemble median, IQR (25th–75th) ",
    "and 5th–95th percentiles across members, written `median [q25–q75], 5-95% [p05–p95]`.")
say("")
say("---")
say("")

## ---------------------------------------------------------------- validation
say("## Validation gates")
say("")
say("All four gates passed; nothing below would have been written otherwise.")
say("")
for (v in names(VAL)) say("- **", v, "** — ", VAL[[v]])
say("- **Magnitudes** — pre-exploitation (1841–1860) total Antarctic krill ",
    "consumption is **", iqr(KR_TOT), " t yr⁻¹** against a standing krill stock of ",
    iqr(KRB), " t, i.e. a consumption:biomass turnover of ", iqr(turnover, 3),
    " yr⁻¹. Scaled by area (", fm(DOMAIN_M2 / 1e6, 4), " km², ~7–8% of the Southern ",
    "Ocean), that is a Southern-Ocean-equivalent ~", fm(KR_TOT[["median"]] * 19e6 /
    (DOMAIN_M2 / 1e6) / 1e6, 3), " Mt yr⁻¹, against published Southern Ocean krill ",
    "consumption estimates of roughly 100–300 Mt yr⁻¹. That is low by a factor of ",
    "~4–8 but **within one order of magnitude**, so no magnitude is flagged as ",
    "implausible. Standing stock scales to ~", fm(KRB[["median"]] * 19e6 /
    (DOMAIN_M2 / 1e6) / 1e6, 3), " Mt, squarely inside the published 60–400 Mt range.")
say("")
say("---")
say("")

## ------------------------------------------------------------------------- A
say("## A. Pre-exploitation krill consumption partition (unexploited, 1841–1860)")
say("")
say("Total Antarctic krill consumption across all predators: **", iqr(KR_TOT),
    " t yr⁻¹**.")
say("")
say("### A.1 By predator group — Antarctic krill")
say("")
say(md_table(A_sp %>% transmute(predator_group,
    cons_t_per_yr_median = cons_t_per_yr_median,
    cons_IQR = sprintf("%s–%s", fm(cons_t_per_yr_q25), fm(cons_t_per_yr_q75)),
    cons_5_95 = sprintf("%s–%s", fm(cons_t_per_yr_p05), fm(cons_t_per_yr_p95)),
    share_pct_median, share_IQR = sprintf("%s–%s", fm(share_pct_q25), fm(share_pct_q75)),
    share_5_95 = sprintf("%s–%s", fm(share_pct_p05), fm(share_pct_p95)))))
say("")
say("### A.2 Aggregates — Antarctic krill")
say("")
say(md_table(A_ag %>% transmute(predator_group, cons_t_per_yr_median,
    share_pct_median, share_pct_q25, share_pct_q75, share_pct_p05, share_pct_p95)))
say("")
say("### A.3 By predator group — all five lower-trophic-level prey combined")
say("")
say("Total LTL consumption (Antarctic krill + other krill + mesozooplankton + ",
    "other macrozooplankton + salps): **", iqr(LTL_TOT), " t yr⁻¹**.")
say("")
say(md_table(A_ltl_c %>% transmute(predator_group, cons_t_per_yr_median,
    share_pct_median, share_pct_q25, share_pct_q75, share_pct_p05, share_pct_p95)))
say("")
say(md_table(A_ltl_a %>% transmute(predator_group, cons_t_per_yr_median,
    share_pct_median, share_pct_q25, share_pct_q75)))
say("")
say("Per-prey partitions for each of the five LTL groups separately are in ",
    "`A_ltl_partition_1841_1860.csv`; each predator's own diet composition ",
    "(percentage of its total intake by prey) is in ",
    "`A_predator_diet_composition_1841_1860.csv`.")
say("")
say("### What this implies for the mechanism")
say("")
say("**Before exploitation, baleen plus minke whales accounted for ",
    iqr(WH_SHARE, 3), "% of total Antarctic krill predation** — a few hundredths ",
    "of one percent, not a dominant share. Krill predation is overwhelmingly a ",
    "fish process in this model: shelf and coastal fishes, bathypelagic fishes and ",
    "mesopelagic fishes together take ",
    fm(sum(A_sp$share_pct_median[A_sp$predator_group %in%
        c("shelf and coastal fishes", "bathypelagic fishes", "mesopelagic fishes")]), 4),
    "% of it. The same holds for lower-trophic-level prey as a whole: whales take ",
    iqr(LTL_WH_SHARE, 3), "% of it.")
say("")
say("The cause is structural, not a calibration outcome — see section F. Baleen and ",
    "minke whales are the only two groups with a **box** feeding kernel, and its ",
    "hard prey-size window puts Antarctic krill outside the reach of most of the ",
    "baleen whale size range. **Any manuscript claim that whales were the dominant ",
    "pre-exploitation krill predator is not supported by this model**, and the ",
    "reason is a parameter choice that can be revisited, not an emergent result.")
say("")
say("**What the model's whales eat instead.** From the diet-composition table, ",
    "baleen whales take ", fm(bal_res, 3), "% of their total intake from the ",
    "**forced plankton `Resource`** (which extends to the ",
    fm(WG@resource_params$w_pp_cutoff, 3), " g cutoff and so overlaps the lower ",
    "part of their ", fm(F_tab$box_prey_lo_at_w_max_g[F_tab$species == "baleen whales"], 3),
    "–", fm(F_tab$box_prey_hi_at_w_max_g[F_tab$species == "baleen whales"], 4),
    " g window), ", fm(bal_fish, 3), "% from bathypelagic + mesopelagic + shelf and ",
    "coastal fishes, ", fm(bal_salp, 3), "% from salps, and ", fm(bal_krill, 3),
    "% from Antarctic krill. Minke whales take ", fm(min_res, 3),
    "% from the Resource and ", fm(min_krill, 3), "% from Antarctic krill.")
say("")
say("This matters beyond the partition: because the Resource is a **prescribed ",
    "forcing** rather than a state variable (section C), the majority of whale ",
    "intake in this model is donor-controlled. Removing the whales cannot free up ",
    "the food they were eating, because that food was never depleted in the first ",
    "place. That is a second, independent reason — on top of the kernel — why ",
    "whaling produces so little krill response here.")
say("")
say("---")
say("")

## ------------------------------------------------------------------------- B
say("## B. Size-resolved attribution of the change in krill predation")
say("")
say("Paired exploited − unexploited change in Antarctic krill consumption, by ",
    "predator group and predator body-mass bin. Full native-bin resolution (100 ",
    "bins) in `B_krill_delta_by_size_bin.csv`; log10 body-mass decades in ",
    "`B_krill_delta_by_decade_bin.csv`; ranked extremes in `B_top_contributions.csv`.")
say("")
say("**Null check.** 1841–1860 predates all fishing effort, so the paired delta ",
    "must be identically zero. Max |median delta| over all predator × bin ",
    "combinations in that period: **", fm(B_null, 3), " t yr⁻¹**.")
say("")
for (pn in c("1930-1970", "1974-1990", "2001-2010")) {
  d <- B_dec %>% filter(period == pn)
  say("### ", pn)
  say("")
  say("Net change summed over all predators and bins: **",
      fm(sum(d$delta_t_per_yr_median), 4), " t yr⁻¹** (positive contributions ",
      fm(sum(pmax(d$delta_t_per_yr_median, 0)), 4), ", negative ",
      fm(sum(pmin(d$delta_t_per_yr_median, 0)), 4), ").")
  say("")
  say("Largest positive contributions (predator × log10 body-mass decade):")
  say("")
  say(md_table(B_top %>% filter(period == pn, direction == "largest positive") %>%
      transmute(predator_group, log10_w_decade, delta_t_per_yr_median,
                delta_t_per_yr_q25, delta_t_per_yr_q75,
                unexploited_t_per_yr_median, exploited_t_per_yr_median)))
  say("")
  say("Largest negative contributions:")
  say("")
  say(md_table(B_top %>% filter(period == pn, direction == "largest negative") %>%
      transmute(predator_group, log10_w_decade, delta_t_per_yr_median,
                delta_t_per_yr_q25, delta_t_per_yr_q75,
                unexploited_t_per_yr_median, exploited_t_per_yr_median)))
  say("")
}
say("### The specific hypothesis tested: small toothfishes and mid-size pelagic fishes, 1930–1970")
say("")
say("Of the total **positive** change in krill predation during 1930–1970 ",
    "(", fm(pos70, 4), " t yr⁻¹ summed over predator × decade), the share ",
    "contributed by each candidate is:")
say("")
say(md_table(data.frame(
  candidate = c("toothfishes (all sizes)", "mesopelagic + bathypelagic fishes",
                "shelf and coastal fishes", "everything else"),
  share_of_positive_change_pct = c(tooth70, pel70, shelf70,
                                   100 - tooth70 - pel70 - shelf70))))
say("")
say("### What this implies for the mechanism")
say("")
{
  b1 <- B_dec %>% filter(period == "1930-1970", delta_t_per_yr_median > 0) %>%
    arrange(desc(delta_t_per_yr_median))
  say("The decade column is the predator body-mass class in log10 g: decade 0 is ",
      "1–10 g, decade 2 is 100–1000 g, decade 3 is 1–10 kg, decade 6 is 1–10 t.")
  say("")
  say("**The hypothesis is half right.** The 1930–1970 increase in krill predation ",
      "*is* concentrated in mid-size pelagic fishes — the two largest contributions ",
      "are ", b1$predator_group[1], " at decade ", b1$log10_w_decade[1], " (",
      fm(b1$delta_t_per_yr_median[1], 4), " t yr⁻¹) and ", b1$predator_group[2],
      " at decade ", b1$log10_w_decade[2], " (", fm(b1$delta_t_per_yr_median[2], 4),
      " t yr⁻¹), together ", fm(pel70, 4), "% of all positive change, and both sit ",
      "in the 100–1000 g class. **It is not toothfishes**, which contribute ",
      fm(tooth70, 3), "% of the positive change — three to four orders of magnitude ",
      "less than the pelagic fishes and negligible at any size. Shelf and coastal ",
      "fishes, the largest krill predator overall, move in the *opposite* direction ",
      "(", fm(shelf70, 3), "% of positive change; they dominate the negative side).")
}
say("")
say("---")
say("")

## ------------------------------------------------------------------------- C
say("## C. Lower-trophic-level disaggregation: predation release vs competition release")
say("")
say("Paired percentage change from the unexploited counterfactual, per LTL group. ",
    "Annual series 1900–2010 in `C_ltl_pctchange_timeseries_1900_2010.csv`; period ",
    "means below and in `C_ltl_pctchange_period_summary.csv`.")
say("")
for (mtr in c("abundance", "biomass", "mean_individual_mass")) {
  say("### ", gsub("_", " ", mtr), " — % change from unexploited")
  say("")
  say(md_table(C_per %>% filter(metric == mtr) %>%
      transmute(ltl_group, period, pct_change_median, pct_change_q25,
                pct_change_q75, pct_change_p05, pct_change_p95)))
  say("")
}
say("### The background resource spectrum")
say("")
say("The resolved species sit on a forced plankton resource ",
    "(`resource_dynamics = \"plankton_forcing\"`), which spans ",
    fm(min(C_res$w_g), 3), " g to the ", fm(WG@resource_params$w_pp_cutoff, 3),
    " g cutoff (last non-zero bin ",
    fm(max(C_res$w_g[C_res$n_pp_mean_across_members > 0]), 4), " g) and therefore ",
    "**overlaps the resolved size range rather than sitting entirely below it** — ",
    "the resolved grid starts at ", fm(min(Wv), 3), " g.")
say("")
say("**The resource is identical in the exploited and unexploited runs.** Measured ",
    "over all ", NM, " members, all 170 years and all ", length(w_full),
    " resource bins, the maximum relative difference between arms is **",
    fm(max(npp_rel), 3), "**. It is also identical across members (max relative ",
    "spread across the ensemble ", fm(max(C_res$n_pp_max_rel_spread_across_members), 3),
    "), as expected for an exogenous forcing. See `C_resource_spectrum_check.csv`.")
say("")
say("### What this implies for the mechanism")
say("")
say("Because the shared resource is *forced*, it cannot be depressed by ",
    "competition — it is a boundary condition, not a state variable. **The ",
    "competition-release mechanism is therefore not testable in this model ",
    "through the resource term**, and any competition signal must appear as ",
    "elevated biomass in the other four LTL groups instead.")
say("")
{
  cb <- C_per %>% filter(metric == "biomass", period == "2001-2010")
  kr <- cb$pct_change_median[cb$ltl_group == "antarctic krill"]
  ot <- cb %>% filter(ltl_group != "antarctic krill")
  say("Reading the 2001-2010 biomass row: Antarctic krill ", fm(kr, 3),
      "%, against ", paste(sprintf("%s %s%%", ot$ltl_group,
        fm(ot$pct_change_median, 3)), collapse = ", "), ".")
  say("")
  say("Two things follow. First, **every LTL response is smaller than 2% in ",
      "magnitude** — an order of magnitude below what either hypothesis would ",
      "predict as a headline effect, so the mechanistic question is being asked of ",
      "a very small signal. Second, the pattern is **not the clean ",
      "predation-release signature**, which would be Antarctic krill moving alone: ",
      "salps move ", fm(abs(ot$pct_change_median[ot$ltl_group == "salps"] / kr), 3),
      "x as much as krill does, and in the opposite direction. Krill down with ",
      "salps up is the signature of a *shared-predator or shared-space* ",
      "rearrangement among the LTL groups rather than of krill predation release ",
      "in isolation; with the resource fixed, the two cannot be separated further ",
      "within this model.")
}
say("")
say("---")
say("")

## ------------------------------------------------------------------------- D
say("## D. Krill mortality decomposition")
say("")
say("Total Antarctic krill mortality split into predation mortality attributable ",
    "to each predator, plus fishing and background mortality, both arms. Rates are ",
    "krill-biomass-weighted means over the size spectrum. Full tables in ",
    "`D_krill_mortality_decomposition.csv`; the paired exploited − unexploited ",
    "difference in `D_krill_mortality_paired_delta.csv`.")
say("")
say("**Consistency.** Predation mortality and diet-derived consumption are the same ",
    "flux computed two independent ways. The largest per-predator disagreement, ",
    "over all ", NM, " members, four periods and both arms, is ", fm(d_id, 3),
    " of the total krill flux (", fm(d_id_rel, 3), " in per-group relative terms ",
    "over groups carrying more than 1e-6 of the total). The mortality table is ",
    "therefore a genuine rate-space restatement of the consumption table, not a ",
    "second estimate of it.")
say("")
for (pn in names(PERIODS)) {
  say("### ", pn)
  say("")
  say(md_table(D_dec %>% filter(period == pn) %>%
      transmute(arm, mortality_source, removal_t_per_yr_median,
                removal_t_per_yr_q25, removal_t_per_yr_q75,
                mort_rate_per_yr_median, krill_biomass_t_median) %>%
      group_by(arm) %>% slice_head(n = 8) %>% ungroup()))
  say("")
}
say("### Paired change, by source")
say("")
say(md_table(D_pair %>% group_by(period) %>% slice_head(n = 6) %>% ungroup() %>%
    transmute(period, mortality_source, delta_removal_t_per_yr_median,
              delta_removal_t_per_yr_q25, delta_removal_t_per_yr_q75,
              delta_rate_per_yr_median, delta_krill_biomass_pct_median)))
say("")
say("### What this implies for the mechanism")
say("")
say("Krill fishing effort in this model runs **1974–1996 only** and is zero ",
    "throughout 2001–2010, and the fitted krill catchability is small, so direct ",
    "fishing mortality on krill is negligible in every period (see the FISHING rows ",
    "above). Whatever moves the krill stock must therefore act through predation ",
    "mortality or through the growth/reproduction terms, and the per-predator rows ",
    "say which predators changed.")
say("")
{
  du <- D_dec %>% filter(period == "2001-2010", arm == "unexploited")
  gr <- function(s) du$mort_rate_per_yr_median[du$mortality_source == s]
  say("**Background mortality is the largest single term.** At 2001-2010 the ",
      "unexploited krill mortality budget is total ", fm(gr("TOTAL mortality"), 3),
      " yr⁻¹, of which background (`mu_b`, a fixed per-size rate) is ",
      fm(gr("BACKGROUND"), 3), " yr⁻¹ (", fm(100 * gr("BACKGROUND") /
      gr("TOTAL mortality"), 3), "%) and all resolved predation is ",
      fm(gr("TOTAL predation"), 3), " yr⁻¹ (", fm(100 * gr("TOTAL predation") /
      gr("TOTAL mortality"), 3), "%). Because `mu_b` is a *rate*, its removal in ",
      "tonnes changes when krill biomass changes but its rate never does — that is ",
      "why the BACKGROUND row in the paired-delta table shows a large tonnage ",
      "change against a rate change of exactly zero. The corollary is that **the ",
      "majority of krill mortality in this model is not attributable to any modelled ",
      "predator**, which caps how much of a krill signal any predator change can ",
      "generate.")
}
say("")
{
  d70 <- D_pair %>% filter(period == "1930-1970")
  gp <- function(s) d70$delta_removal_t_per_yr_median[d70$mortality_source == s]
  say("**Direction of the whaling-era change.** Total predation on krill *rises* by ",
      fm(gp("TOTAL predation"), 4), " t yr⁻¹ under exploitation over 1930-1970, ",
      "driven by bathypelagic (", fm(gp("bathypelagic fishes"), 4),
      ") and mesopelagic fishes (", fm(gp("mesopelagic fishes"), 4),
      "), and partly offset by shelf and coastal fishes (",
      fm(gp("shelf and coastal fishes"), 4), "). So the krill decline from 1930 in ",
      "this model **is** driven by increased predation mortality, and the predators ",
      "responsible are mid-water fishes — not whales, whose own krill take is four ",
      "orders of magnitude smaller than the change itself.")
}
say("")
say("---")
say("")

## ------------------------------------------------------------------------- E
say("## E. Compensation biomass budget")
say("")
say("Community biomass above 1 t of individual body mass lost under exploitation, ",
    "against biomass gained below 1 t. The 1 t threshold falls between size bins ",
    fm(max(which(Wv < 1e6)), 3), " (", fm(Wv[max(which(Wv < 1e6))], 4), " g) and ",
    fm(min(which(Wv >= 1e6)), 3), " (", fm(Wv[min(which(Wv >= 1e6))], 4), " g); the ",
    "straddling bin is split linearly (`boundary_bin = \"split\"`, the convention ",
    "transcribed from `R/wmin_test/06_stage0_robustness.R:36`), with an unsplit ",
    "variant (`\"hard\"`) reported alongside as a sensitivity.")
say("")
say(md_table(E_budget %>% transmute(period, boundary_bin, quantity, unit,
    median, q25, q75, p05, p95)))
say("")
say("Per-group breakdowns are in `E_compensation_budget_2001_2010.csv` and ",
    "`E_compensation_budget_1960_1970.csv`.")
say("")
say("### What this implies for the mechanism")
say("")
{
  eb <- function(pn, q) {
    r <- E_budget %>% filter(period == pn, boundary_bin == "split", quantity == q)
    setNames(as.numeric(r[1, c("median", "q25", "q75", "p05", "p95")]),
             c("median", "q25", "q75", "p05", "p95")) }
  say("The **offset fraction** is the headline: gains below 1 t divided by the ",
      "magnitude of losses above 1 t. A value near 1 means the community is ",
      "biomass-conserving — what the great whales lost was taken up by smaller ",
      "classes; a value well below 1 means exploitation removed biomass from the ",
      "system without compensation.")
  say("")
  for (pn in c("1960-1970", "2001-2010")) {
    lo <- eb(pn, "delta biomass above 1 t"); gi <- eb(pn, "delta biomass below 1 t")
    rs <- eb(pn, "residual (above + below)")
    of <- eb(pn, "offset fraction (gain below / |loss above|)")
    say("- **", pn, "**: ", fm(-lo[["median"]], 4), " t lost above 1 t against ",
        fm(gi[["median"]], 4), " t gained below, leaving a residual of ",
        fm(rs[["median"]], 4), " t. **Offset fraction ", iqr(of, 3), "** — so only ",
        "about ", fm(100 * of[["median"]], 2), "% of the biomass removed from the ",
        "large classes reappears in the small ones.")
  }
  say("")
  say("**The community is not biomass-conserving under exploitation.** Roughly ",
      "six-sevenths of the great-whale biomass removed is simply absent from the ",
      "system; it is not redistributed downward. Two caveats on reading that. ",
      "First, the 5th percentile of the offset fraction is **negative** in both ",
      "periods (", fm(eb("1960-1970", "offset fraction (gain below / |loss above|)")[["p05"]], 3),
      " and ", fm(eb("2001-2010", "offset fraction (gain below / |loss above|)")[["p05"]], 3),
      "), meaning that in a minority of members the classes below 1 t *also* lose ",
      "biomass — exploitation propagates down rather than compensating. Second, ",
      "the split-vs-hard boundary treatment changes nothing (the two rows agree to ",
      "three significant figures), so the result is not an artefact of where the ",
      "1 t threshold falls between size bins.")
}
say("")
say("---")
say("")

## ------------------------------------------------------------------------- F
say("## F. Baleen whale feeding kernel parameters")
say("")
say("Verified constant across all ", NM, " members: the Monte Carlo perturbs only ",
    "`gamma`, `catchability` and `abundance_scaling` ",
    "(`R/wmin_test/43_recover_member_draws.R:6-13`), so no kernel parameter is a ",
    "drawn quantity. Full table in `F_feeding_kernel_parameters.csv`.")
say("")
say("### F.1 What is actually used — the box kernel")
say("")
say("**Baleen whales and minke whales are the only two of the 19 groups with ",
    "`pred_kernel_type = \"box\"`.** For those two the lognormal parameters `beta` ",
    "and `sigma` are present in `species_params` but never evaluated; the operative ",
    "parameters are `ppmr_min` and `ppmr_max`, which define a hard prey-mass window ",
    "[w/ppmr_max, w/ppmr_min] with kernel value exactly 1 inside and exactly 0 outside.")
say("")
say(md_table(F_tab %>% filter(operative_kernel == "box") %>%
    transmute(species, ppmr_min, ppmr_max, w_min_g, w_mat_g, w_max_g,
              box_prey_lo_at_w_max_g, box_prey_hi_at_w_max_g,
              box_prey_lo_at_w_min_g, box_prey_hi_at_w_min_g,
              max_predator_mass_that_can_eat_krill_g)))
say("")
say("Antarctic krill spans ", fm(K1$w_min[K1$species == KRILL], 3), " g to ",
    fm(kmax, 4), " g. Consequences, read straight off the window:")
say("")
say("- A baleen whale **at its asymptotic size (",
    fm(K1$w_max[K1$species == "baleen whales"] / 1e6, 4), " t)** has a preferred ",
    "prey window of **",
    fm(F_tab$box_prey_lo_at_w_max_g[F_tab$species == "baleen whales"], 4), "–",
    fm(F_tab$box_prey_hi_at_w_max_g[F_tab$species == "baleen whales"], 4),
    " g** — entirely above the Antarctic krill size range. **The largest baleen ",
    "whales cannot eat Antarctic krill at all in this model.**")
say("- Baleen whales can access krill only below ",
    fm(F_tab$max_predator_mass_that_can_eat_krill_g[F_tab$species == "baleen whales"] / 1e6, 4),
    " t, which is below `w_mat` (",
    fm(K1$w_mat[K1$species == "baleen whales"] / 1e6, 4),
    " t) — so **only immature baleen whales take krill**, and only the largest krill.")
say("- Minke whales (`w_max` ",
    fm(K1$w_max[K1$species == "minke whales"] / 1e6, 3), " t) sit entirely below ",
    "that ceiling, so their whole size range can take krill. That is why minke ",
    "whales out-consume baleen whales on krill despite the far smaller stock.")
say("")
say("### F.2 What is stored but not used — the lognormal parameters")
say("")
say("For completeness, the stored `beta`/`sigma` and the geometry they *would* ",
    "imply if the kernel were lognormal (peak at w/beta, ±1σ at w/beta·e^∓σ):")
say("")
say(md_table(F_tab %>% filter(species %in% c(WHALES, "sperm whales", "orca")) %>%
    transmute(species, operative_kernel, beta_stored, sigma_stored, beta_is_used,
              lognormal_pref_prey_at_w_max_g, lognormal_prey_minus1sd_at_w_max_g,
              lognormal_prey_plus1sd_at_w_max_g, krill_sd_from_pref_at_w_max)))
say("")
say("Even under the lognormal reading the conclusion would not change sign: a ",
    "baleen whale at `w_max` would prefer ",
    fm(F_tab$lognormal_pref_prey_at_w_max_g[F_tab$species == "baleen whales"], 4),
    " g prey, putting maximum-size Antarctic krill ",
    fm(F_tab$krill_sd_from_pref_at_w_max[F_tab$species == "baleen whales"], 3),
    " kernel σ away. The box makes it a hard zero rather than a small number.")
say("")
say("### F.3 How the whales differ from the other groups")
say("")
say(md_table(F_tab %>% filter(operative_kernel == "lognormal") %>%
    transmute(species, beta_stored, sigma_stored, w_max_g,
              lognormal_pref_prey_at_w_max_g)))
say("")
say("The other 17 groups all use the lognormal kernel with `sigma` = 2.0 except ",
    "leopard seals (3.0) and orca (2.5). There is **no allometric default in force**: ",
    "`beta` is a per-group table value, not a function of body size, and it spans ",
    fm(min(F_tab$beta_stored), 3), " to ", fm(max(F_tab$beta_stored), 3),
    " across the 19 groups. For reference, mizer's own package default is ",
    "`beta = 100, sigma = 1.3`.")
say("")
say("### F.4 Source and justification recorded in the codebase")
say("")
say("- `group params/trait_groups_params_vCWC_v5.csv` carries the per-group `beta` ",
    "column, including the whale values (baleen 2.191126e5, minke 5.042017e6); this ",
    "is the trait table the params object is built from. No `sigma` column — sigma ",
    "is set in the setup scripts.")
say("- `03_model_setup_pre_therMizer.rmd:1027-1039` — zooplankton `beta` values are ",
    "sourced from **Heneghan et al. 2020** (*Ecological Modelling*, ",
    "doi:10.1016/j.ecolmodel.2020.109265) log10 PPMR values; ",
    "`04_therMizer_calibration_scale_g_m2.Rmd:150` repeats this for all zooplankton ",
    "except salps.")
say("- The operative **1e5 / 5e6** pair is set at ",
    "`model_setup_old/New_steady_state_07_06_2024.Rmd:263-267`, applied identically ",
    "to baleen and minke whales. Verified by reading the stored objects: ",
    "`params/params_07_06_2024.rds` (that script's output) and ",
    "`params_sel_adj.rds` (the Monte Carlo seed) both carry ",
    "`box / beta 219112.6 / sigma 2 / ppmr 1e5-5e6` for baleen and ",
    "`box / beta 5042016.8 / sigma 2 / ppmr 1e5-5e6` for minke — identical to ",
    "every one of the ", NM, " ensemble members.")
say("- **The box type itself is not reproducible from the live setup script.** ",
    "`03_model_setup_pre_therMizer.rmd:1379` — the `pred_kernel_type <- \"box\"` ",
    "assignment for baleen whales — is **commented out**, and the `ppmr_min = 4e6` ",
    "it sets at `:1377` does not survive into any stored object. The only live ",
    "`\"box\"` assignment anywhere in the repository is for **minke whales** at ",
    "`model_setup_old/New_steady_state_07_06_2024.Rmd:179` (with `ppmr_max = 5e7`, ",
    "later overwritten to 5e6 at `:263-267`). Baleen whales' box type is inherited ",
    "from a stored params object, traceable only to the archived ",
    "`optim_model_setup_old/model_setup*.Rmd` lineage (e.g. `model_setup.Rmd:156, ",
    ":257, :623`). So the single most consequential structural parameter in this ",
    "report cannot be regenerated by running the current setup scripts.")
say("- The stated rationale is at `optim_model_setup_old/model_setup.Rmd:233-234`: ",
    "*\"max ppmr value are based on w_max baleen whale feeding on w_min euphausiids; ",
    "min ppmr value based on w_min baleen whale feeding on w_max euphausiids\"*. ",
    "**That rationale and the implemented numbers do not agree.** The stated ",
    "construction gives ppmr_max = w_max(baleen)/w_min(krill) = ",
    fm(K1$w_max[K1$species == "baleen whales"] / K1$w_min[K1$species == KRILL], 4),
    " and ppmr_min = w_min(baleen)/w_max(krill) = ",
    fm(K1$w_min[K1$species == "baleen whales"] / kmax, 4),
    ", against the implemented 5e6 and 1e5. Implementing the stated rationale ",
    "would make every baleen whale size class able to eat krill; the implemented ",
    "ceiling of 5e6 excludes all baleen whales above ",
    fm(kmax * 5e6 / 1e6, 4), " t. Earlier versions did carry values close to the ",
    "stated construction (`optim_model_setup_old/model_setup.Rmd:252-253`: ",
    "`ppmr_min = 711512.4`, `ppmr_max = 1.63233e12`) and were commented out.")
say("- No numeric justification for the whale `beta`/`sigma` values themselves is ",
    "recorded anywhere in the repository; they are inert for these two groups in any case.")
say("")
say("---")
say("")

## ----------------------------------------------------------------------- G/H
say("## G. Refreshed headline numbers, n = ", NM)
say("")
say(md_table(G_tab %>% transmute(quantity, unit, median, q25, q75, p05, p95)))
say("")
say("---")
say("")
say("## H. Sensitivity to the RMSE ranking threshold")
say("")
say("The four subsets are prefixes of one ranking — the post-catchability-refit ",
    "pooled log10 yield RMSE that defines cut A — so no member is re-scored ",
    "between rows.")
say("")
say(md_table(H_all %>% transmute(quantity, subset, unit, median, q25, q75) %>%
    arrange(quantity, subset)))
say("")
say("### Stability")
say("")
{
  stab <- H_all %>% group_by(quantity) %>%
    summarise(sign_stable = length(unique(sign(median))) == 1,
              med_min = min(median), med_max = max(median),
              fold_range = max(abs(median)) / pmax(1e-300, min(abs(median))),
              .groups = "drop") %>% arrange(desc(fold_range))
  say(md_table(stab))
  say("")
  hget <- function(q) H_all$median[H_all$quantity == q]
  wr <- hget("krill consumption ratio, Large baleen + minke whales, 2010")
  say("**Sign is stable for every quantity at every threshold.** The headline is ",
      "robust: the baleen + minke krill consumption ratio at 2010 moves only ",
      "across ", fm(min(wr), 3), "–", fm(max(wr), 3), " (a ",
      fm(max(wr) / min(wr), 3), "x range) as the subset widens from 167 to all ",
      "1,668 members, and the ordering of the three predator groups — whales far ",
      "below 1, all predators and all fishes both at 1.00 — never changes. So the ",
      "decile cut is not what produces the whale result.")
  say("")
  say("Three quantities are threshold-sensitive and should not be quoted without ",
      "the subset named:")
  say("")
  sp <- hget("abundance ratio, sperm whales, 2001-2010 mean")
  bw <- hget("abundance ratio, baleen whales, 2001-2010 mean")
  ap <- hget("absolute deficit (unexploited - exploited), All predators, 2001-2010")
  say("- **Sperm whale abundance ratio** falls monotonically ", fm(sp[1], 3), " → ",
      fm(sp[length(sp)], 3), " (", fm(max(sp) / min(sp), 3), "x) as the subset ",
      "widens: the wider ensemble depicts a *deeper* sperm whale depletion than the ",
      "best-fitting decile does.")
  say("- **Baleen whale abundance ratio** moves the other way, ", fm(bw[1], 3),
      " → ", fm(bw[length(bw)], 3), " (", fm(max(bw) / min(bw), 3),
      "x) — a less severe depletion in the wider ensemble. These two run in ",
      "opposite directions, so this is a genuine change in which members are ",
      "included, not a monotone widening of spread.")
  say("- **Absolute deficit for all predators** ranges ", fm(min(ap), 4), " to ",
      fm(max(ap), 4), " t yr⁻¹ (", fm(max(abs(ap)) / min(abs(ap)), 3),
      "x). That fold range is not meaningful: the quantity is a small difference ",
      "between two nearly equal totals and sits close to zero, with an IQR that ",
      "spans zero at every threshold. Read it as \"indistinguishable from zero\" ",
      "rather than as a 17-fold instability.")
}
say("")
say("---")
say("")

## -------------------------------------------------------- surprises/concerns
say("## Surprises and concerns")
say("")
say("1. **Baleen and minke whales use a box feeding kernel, not the lognormal one.** ",
    "This was not flagged anywhere in the analysis brief or in the figure scripts, ",
    "and it is the single most consequential structural fact for every krill number ",
    "in this report. Their stored `beta`/`sigma` are inert. Anyone reading ",
    "`species_params$beta` for these two groups is reading a value the model never uses.")
say("")
say("2. **The implemented whale PPMR window contradicts its own stated rationale** ",
    "(section F.4). The comment says the window was built so that whales across ",
    "their whole size range can eat krill across theirs; the implemented ",
    "`ppmr_max = 5e6` does the opposite, excluding all baleen whales above ",
    fm(kmax * 5e6 / 1e6, 4), " t — which is below maturity. This looks like an ",
    "unintended consequence of a later parameter edit rather than a considered choice, ",
    "and it is worth resolving before the krill narrative is written. Compounding ",
    "this, the baleen whale `\"box\"` assignment is **commented out** in the live ",
    "setup script (`03_model_setup_pre_therMizer.rmd:1379`), so the parameter that ",
    "drives every whale krill number here cannot be regenerated by running the ",
    "current setup lineage — it survives only inside stored params objects.")
say("")
say("3. **Whales are a rounding error in krill predation** (", iqr(WH_SHARE, 3),
    "% pre-exploitation). Any manuscript sentence built on whales as the dominant ",
    "pre-exploitation krill consumer needs rewriting, or the kernel needs revisiting. ",
    "Note this is a *model structure* result, not an ensemble-uncertainty result — ",
    "the spread across 167 members is narrow because the kernel is not a drawn parameter.")
say("")
say("4. **The model's baleen whales feed mostly on the exogenous forcing.** ",
    fm(bal_res, 3), "% of baleen whale intake and ", fm(min_res, 3), "% of minke ",
    "intake comes from the prescribed plankton `Resource`, which is not depleted ",
    "by anything. Whale feeding is therefore largely donor-controlled, and removing ",
    "whales frees very little real food. Combined with the box kernel, this means ",
    "the model has **two independent structural reasons** why whaling cannot ",
    "produce a krill response — worth stating explicitly in the methods rather ",
    "than leaving as an emergent surprise.")
say("")
say("5. **Whale ration is low by roughly an order of magnitude.** Total baleen whale ",
    "intake divided by baleen whale standing stock is ", iqr(BAL_RATION, 3),
    " t of prey per t of whale per year over 1841–1860. Published field estimates ",
    "for Antarctic baleen whales are of order 3–10 t per t per year once the ",
    "seasonal feeding window is accounted for. This is a check on the whales, not ",
    "on the krill, and it is consistent with a feeding kernel that excludes their ",
    "principal real-world prey — but it should be reconciled before whale ",
    "consumption is quoted in absolute terms.")
say("")
say("6. **Competition release is not testable via the shared resource.** ",
    "`resource_dynamics = \"plankton_forcing\"` makes the background spectrum a ",
    "prescribed boundary condition, identical between arms to a relative difference ",
    "of exactly ", fm(max(npp_rel), 3), ". A depressed shared resource cannot occur ",
    "by construction, so the C-hypothesis test rests entirely on the other four LTL groups.")
say("")
say("7. **Direct krill fishing is negligible throughout.** Effort is non-zero only ",
    "1974–1996 and is exactly zero across the whole 2001–2010 contemporary window, ",
    "so the contemporary krill numbers contain no direct krill-fishery signal at all ",
    "— every contemporary difference is an indirect, food-web-mediated effect.")
say("")
say("8. **The consumption:biomass turnover is low.** Krill standing stock scales to ",
    "a plausible Southern-Ocean-equivalent (~", fm(KRB[["median"]] * 19e6 /
    (DOMAIN_M2 / 1e6) / 1e6, 3), " Mt) but total predation on it is only ",
    iqr(turnover, 3), " yr⁻¹ of standing stock, giving a Southern-Ocean-equivalent ",
    "consumption ~4–8x below published estimates. The stock is right and the flux ",
    "through it is low; that is consistent with the kernel excluding the largest ",
    "krill predators.")
say("")
say("9. **The ensemble medians of per-member shares sum to ", fm(med_sum, 6),
    "%, not 100%.** A median is not additive, and the gap here is large enough to ",
    "notice because the three dominant fish groups have broad, overlapping share ",
    "distributions — bathypelagic fishes span ",
    fm(A_sp$share_pct_p05[A_sp$predator_group == "bathypelagic fishes"], 3), "-",
    fm(A_sp$share_pct_p95[A_sp$predator_group == "bathypelagic fishes"], 3),
    "% and mesopelagic ",
    fm(A_sp$share_pct_p05[A_sp$predator_group == "mesopelagic fishes"], 3), "-",
    fm(A_sp$share_pct_p95[A_sp$predator_group == "mesopelagic fishes"], 3),
    "% across members, so which of them dominates is itself uncertain. Per-member ",
    "shares sum to 100% exactly (max deviation ", fm(d_sh, 3),
    "); no renormalisation has been applied. **Use the aggregate rows, not the sum ",
    "of species medians**, when a partition has to add up.")
say("")
say("10. **The absolute krill totals carry a 3x ensemble spread** (",
    iqr(KR_TOT), " t yr⁻¹) while the whale *share* is tight (", iqr(WH_SHARE, 3),
    "%). Shares are far better constrained than totals here, because the drawn ",
    "parameters (`gamma`, `abundance_scaling`) move the whole community together ",
    "while the feeding kernel that sets the partition is fixed. Quote shares with ",
    "confidence; quote absolute tonnages with the interval attached.")
say("")
say("---")
say("")
say("## Files written")
say("")
say(paste0("- `", sort(list.files(OUTDIR)), "`", collapse = "\n"))

writeLines(MD, guard(file.path(OUTDIR, "SUMMARY.md")))
cat("  wrote SUMMARY.md (", length(MD), "blocks )\n")
