###############################################################################
# Prydz Bay Ecosystem Assessment Framework (v2)
# 
# Comprehensive ecosystem-level assessment using PAIRED ensemble comparisons
# across three baseline frameworks:
#   1. Absolute health:     fishing / B0 (pre-exploitation 1841-1860)
#   2. Exploitation impact: fishing / climate-only (decade-matched)
#   3. Climate impact:      climate-only / B0
#
# Metrics are classified into three categories with different scoring:
#
#   Category A — BIOMASS METRICS: scored as depletion ratios against
#     literature-based thresholds (CCAMLR, IWC, BMSY).
#
#   Category B — STRUCTURAL/TROPHIC METRICS: scored as empirical deviation
#     from pre-exploitation (B0) ensemble distribution. Reports proportion
#     of simulations where the metric has been pushed outside the B0
#     5th-95th percentile envelope.
#
#   Category C — EXPLOITATION METRICS: absolute fishing mortality rates.
#
# Key design principles:
#   - Paired simulation comparisons (sim_i fishing vs sim_i climate-only)
#   - Empirically derived reference envelopes from pre-exploitation ensemble
#   - Probabilistic scoring: proportion of simulations exceeding thresholds
#   - B0 reference period: 1841-1860 (ISIMIP climate norm)
#   - Metric-specific thresholds for biomass; self-calibrating envelopes
#     for structural metrics
#
# Ecosystem assessment approach following:
#   Link & Watson (2019). Global ecosystem overfishing: Clear delineation
#     within real limits to production. Sci. Adv. 5:eaav0474.
#   Link et al. (2015). NOAA Fish. Proc. Doc., Ecosystem Assessment
#     methodology.
#   Morrison et al. (2024). Ecosystem assessment approaches for Southern
#     Ocean fisheries management.
#
# Author: Generated for Prydz Bay mizer project
# Date: February 2026
###############################################################################

library(therMizer)
library(mizer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(patchwork)

# Configuration
OUTPUT_DIR <- "ecosystem_assessment_outputs"
OUTPUT_DIR_LARGE <- "Output_large_files/ecosystem_assessment"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)
if (!dir.exists(OUTPUT_DIR_LARGE)) dir.create(OUTPUT_DIR_LARGE, recursive = TRUE)

# Ensemble file paths
ENSEMBLE_PATHS <- list(
  fishing = "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds",
  climate_only = "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
)

# Decade definitions
DECADES <- data.frame(
  label = c("1841-1850", "1851-1860", "1861-1870", "1871-1880",
            "1881-1890", "1891-1900", "1901-1910", "1911-1920",
            "1921-1930", "1931-1940", "1941-1950", "1951-1960",
            "1961-1970", "1971-1980", "1981-1990", "1991-2000",
            "2001-2010"),
  start = seq(1841, 2001, by = 10),
  end = seq(1850, 2010, by = 10),
  stringsAsFactors = FALSE
)

B0_PERIOD <- c(1841, 1860)

HISTORICAL_PERIODS <- list(
  pre_exploitation = c(1841, 1860),
  transition        = c(1861, 1900),
  early_whaling     = c(1901, 1920),
  peak_whaling      = c(1921, 1960),
  post_moratorium   = c(1961, 1976),
  krill_fishing     = c(1977, 1995),
  modern            = c(2001, 2010)
)

###############################################################################
#                         SCORING METHODOLOGY
###############################################################################
#
# Three distinct scoring approaches are used because different metric types
# have fundamentally different relationships with ecosystem health. Applying
# a single scoring method (e.g., depletion ratios) across all metrics would
# produce ecologically misleading results for non-biomass indicators.
#
# =====================================================================
# CATEGORY A: BIOMASS METRICS — Depletion Ratio Scoring
# =====================================================================
#
# Methodology:
#   For each paired simulation i and decade d:
#     ratio_i,d = biomass_fishing_i,d / biomass_reference_i,d
#   where reference is B0 (absolute), climate-only (exploitation), or
#   B0 vs climate-only (climate impact).
#
#   Scoring reports the proportion of simulations where the ratio falls
#   below literature-derived depletion thresholds.
#
# Rationale:
#   Biomass depletion ratios have a direct, proportional ecological
#   interpretation: "X% of pre-exploitation biomass remains." This is the
#   standard basis for fisheries reference points (CCAMLR, IWC, ICES, US
#   Magnuson-Stevens Act) and ecosystem overfishing indicators (Link &
#   Watson 2019). The proportional decline is meaningful across taxa
#   because biomass is a conserved, additive quantity measured in the same
#   units (g/m^2) for all groups.
#
# Reference: Link, J.S. & Watson, R.A. (2019). Global ecosystem
#   overfishing: Clear delineation within real limits to production.
#   Sci. Adv. 5:eaav0474. — Defines ecosystem overfishing thresholds
#   based on total community biomass depletion and exploitation rates.
#
# =====================================================================
# CATEGORY B: STRUCTURAL METRICS — Empirical Deviation Scoring
# =====================================================================
#
# Methodology:
#   For each paired simulation i and decade d:
#     difference_i,d = metric_fishing_i,d - metric_reference_i,d
#     z_i,d = difference_i,d / SD(metric across B0 ensemble)
#
#   The pre-exploitation ensemble (B0, 1841-1860, climate-only) defines
#   the empirical reference envelope. For each metric, the distribution
#   of values across all B0 simulations characterises the "natural range"
#   under parameter uncertainty. Scoring reports:
#     - P(outside B0 5th-95th percentile): outside natural range
#     - P(outside B0 1st-99th percentile): substantially altered
#     - z-score: standardized departure in units of B0 variability
#     - Directional proportions: P(below Q05) vs P(above Q95)
#
# Rationale:
#   Structural and trophic metrics cannot be scored as depletion ratios
#   because:
#
#   (a) Non-proportional scales: The size spectrum slope is negative
#       (typically -1.5 to -2.5). A ratio of slopes (e.g., -2.1 / -1.9
#       = 1.105) has no consistent ecological interpretation — it does
#       not distinguish healthy steepening from pathological steepening.
#       Similarly, mean trophic level operates on a compressed scale
#       (~3.0-5.5); MTL ratios are always near 1.0 and obscure
#       ecologically significant shifts of 0.1-0.2 TL units.
#
#   (b) No literature-based proportional thresholds: Unlike biomass,
#       there are no established reference points stating "a community
#       is impaired when mean trophic level falls below 75% of its
#       pristine value." The proportional framing is simply not how
#       these metrics are interpreted in the literature.
#
#   (c) Non-linear responses: Proportional indicators (HTL, LFI) are
#       bounded [0,1] and can show large absolute changes (e.g., 0.45
#       to 0.35 = 10 percentage points) that appear small as ratios
#       (0.78). The absolute change is more informative.
#
#   The empirical deviation approach is self-calibrating: each metric's
#   "significance" threshold is determined by its own variability in the
#   pre-exploitation ensemble, rather than by an externally imposed
#   proportion. This means metrics with high natural variability (e.g.,
#   predator-prey ratio) require larger departures to be flagged, while
#   metrics with low variability (e.g., MTL) are flagged by smaller
#   absolute changes.
#
# Reference: Blanchard, J.L. et al. (2014). Evaluating targets and
#   trade-offs among fisheries and conservation objectives using a
#   multispecies size spectrum model. J. Appl. Ecol. 51:612-622.
#   — Uses community indicators from size spectrum models with
#   empirical reference ranges from unfished simulations.
#
# =====================================================================
# CATEGORY C: EXPLOITATION METRICS — Absolute Rates
# =====================================================================
#
# Methodology:
#   For each simulation i and decade d:
#     F_i,d = total_catch_i,d / total_biomass_i,d
#   computed for each exploited species group. Reported as absolute
#   fishing mortality rates (year^-1), not as ratios to any baseline.
#
# Rationale:
#   Fishing mortality is a direct forcing variable, not an ecosystem
#   response. It has no meaningful "pre-exploitation" value (F = 0 by
#   definition before exploitation begins), so ratio-based and deviation-
#   based scoring are both inappropriate. Instead, absolute F values are
#   compared against management reference points and reported as ensemble
#   statistics (median, IQR, 90% CI).
#
# Reference: Butterworth, D.S. et al. (1999). Experiences in the
#   evaluation and implementation of management procedures.
#   ICES J. Mar. Sci. 56:985-998. — Framework for evaluating
#   exploitation rates against target and limit reference points.
#
###############################################################################
#                       METRIC DEFINITIONS
###############################################################################
#
# =====================================================================
# CATEGORY A: BIOMASS METRICS
# =====================================================================
#
# total_biomass
#   Definition: Sum of biomass (g/m^2) across all modelled species groups,
#     averaged over each decade.
#   Calculation: rowSums(getBiomass(sim)) across all species.
#   Ecological significance: Reflects total system productivity and
#     standing stock. Declines indicate net removal exceeding production.
#   Assessment context: Link & Watson (2019) define ecosystem overfishing
#     partly based on total community biomass trends. Cury et al. (2011)
#     use total biomass as a primary ecosystem indicator.
#   Reference: Cury, P.M. et al. (2011). Global seabird response to
#     forage fish depletion — one-third for the birds. Science 334:1703-6.
#
# baleen_biomass
#   Definition: Combined biomass of baleen whales and minke whales.
#   Species: baleen whales + minke whales (Prydz Bay Ecopath groups).
#   Ecological significance: Baleen whales are the dominant megafauna
#     consumers of Antarctic krill and were reduced to <5% of pristine
#     biomass during 20th century whaling. Their recovery trajectory
#     is a key ecosystem health indicator for the Southern Ocean.
#   Assessment context: Receives additional IWC RMP 0.54 K threshold
#     (see threshold documentation below).
#   References:
#     IWC (1994). Rep. Int. Whal. Commn 44:145-152.
#     Tulloch, V.J.D. et al. (2019). Future recovery of baleen whales
#       is imperiled by climate change. Glob. Change Biol. 25:1263-81.
#
# whale_biomass
#   Definition: Combined biomass of all cetacean groups.
#   Species: baleen whales + minke whales + sperm whales + orca.
#   Ecological significance: Total cetacean biomass including toothed
#     whales. Sperm whales and orca occupy apex trophic positions and
#     exert top-down control on fish and seal populations.
#   Assessment context: Receives IWC RMP 0.54 K threshold.
#
# seal_biomass
#   Definition: Combined biomass of all pinniped groups.
#   Species: leopard seals + small divers + medium divers + large divers.
#   Ecological significance: Pinnipeds are major krill and fish consumers
#     in the Prydz Bay system. Some species (e.g., crabeater seals)
#     potentially benefited from krill surplus following whale removal.
#   Reference: Ainley, D.G. et al. (2007). Paradigm lost, or is
#     top-down forcing no longer significant in the Antarctic marine
#     ecosystem? Antarct. Sci. 19:283-290.
#
# fish_biomass
#   Definition: Combined biomass of all modelled fish groups.
#   Species: mesopelagic fishes + bathypelagic fishes + shelf and
#     coastal fishes + toothfishes.
#   Ecological significance: Fish provide critical mid-trophic linkages
#     between zooplankton and apex predators. Toothfish are the primary
#     commercially exploited fish in the region.
#   Assessment context: Toothfish are managed under CCAMLR with specific
#     catch limits for Divisions 58.4.1 and 58.4.2.
#
# krill_biomass
#   Definition: Biomass of Antarctic krill (Euphausia superba).
#   Species: antarctic krill (single group).
#   Ecological significance: Antarctic krill is the keystone species
#     of the Southern Ocean food web, supporting whales, seals, penguins,
#     and fish. Krill biomass is the primary target of CCAMLR's
#     ecosystem-based management approach.
#   Assessment context: CCAMLR's krill decision rules (gamma_1 and
#     gamma_2) were specifically designed to maintain krill escapement
#     for dependent predators.
#   References:
#     Constable, A.J. et al. (2000). Managing fisheries to conserve the
#       Antarctic marine ecosystem. ICES J. Mar. Sci. 57:778-791.
#     Atkinson, A. et al. (2019). Krill (Euphausia superba) distribution
#       contracts southward during rapid regional warming. Nat. Clim.
#       Change 9:142-147.
#
# ltl_biomass
#   Definition: Combined biomass of all lower trophic level groups.
#   Species: antarctic krill + other krill + mesozooplankton + other
#     macrozooplankton + salps.
#   Ecological significance: Total zooplankton production underpinning
#     the food web. Includes both krill and non-krill zooplankton, which
#     have different responses to climate change (e.g., salp increases
#     as sea ice declines).
#   Reference: Atkinson, A. et al. (2004). Long-term decline in krill
#     stock and increase in salps within the Southern Ocean. Nature
#     432:100-103.
#
# apex_biomass
#   Definition: Combined biomass of apex predator groups.
#   Species: sperm whales + orca + leopard seals.
#   Ecological significance: Top predators exerting top-down control.
#     Their biomass reflects cumulative impacts propagating through the
#     food web. Depletion of apex predators can trigger trophic cascades.
#   Reference: Estes, J.A. et al. (2011). Trophic downgrading of
#     planet Earth. Science 333:301-306.
#
# =====================================================================
# CATEGORY B: STRUCTURAL / TROPHIC METRICS
# =====================================================================
#
# spectrum_slope
#   Definition: Slope of the log-log relationship between community
#     biomass density and body size, fitted across the size range
#     1g to 1,000,000g (1 tonne).
#   Calculation: mizer::getCommunitySlope(sim, min_w=1, max_w=1e6,
#     biomass=TRUE). Returns the slope of log10(biomass density) ~
#     log10(body mass). Decade-averaged.
#   Ecological significance: The size spectrum slope is a fundamental
#     descriptor of community structure in marine ecosystems. In an
#     unexploited, equilibrium community, metabolic scaling theory
#     predicts a slope near -2.05 (Andersen & Beyer 2006). Steepening
#     (more negative slope) indicates selective removal of large
#     organisms; flattening (less negative) can indicate LTL collapse
#     or overcompensation at small sizes.
#   Why not scored as ratio: The slope is negative, so ratios of slopes
#     behave counter-intuitively (e.g., -2.1/-1.9 = 1.105 appears as
#     an "increase" when the community has actually degraded). The
#     absolute difference and z-score relative to B0 are ecologically
#     interpretable.
#   References:
#     Andersen, K.H. & Beyer, J.E. (2006). Asymptotic size determines
#       species abundance in the marine size spectrum. Am. Nat. 168:54-61.
#     Blanchard, J.L. et al. (2005). Do climate and fishing influence
#       size-based indicators of Celtic Sea fish community structure?
#       ICES J. Mar. Sci. 62:405-411.
#     Jennings, S. & Blanchard, J.L. (2004). Fish abundance with no
#       fishing: predictions based on macroecological theory. J. Anim.
#       Ecol. 73:632-642.
#
# spectrum_intercept
#   Definition: Intercept of the log-log size spectrum regression,
#     jointly extracted with slope from getCommunitySlope().
#   Ecological significance: The intercept reflects the overall
#     abundance level of the community at a reference body size. A
#     system can maintain its slope (relative size structure intact)
#     while shifting the entire spectrum up or down (change in total
#     abundance across all sizes). The slope-intercept pair together
#     provides a more complete characterisation of community structure
#     than slope alone.
#   Reference: Sprules, W.G. & Barth, L.E. (2016). Surfing the
#     biomass size spectrum: some remarks on history, theory, and
#     application. Can. J. Fish. Aquat. Sci. 73:477-495.
#
# mean_tl (Mean Trophic Level of Consumers)
#   Definition: Biomass-weighted mean trophic level across all consumer
#     species (TL >= 3.0), excluding lower trophic level groups.
#   Calculation: For each timestep, MTL = sum(B_i * TL_i) / sum(B_i)
#     where i indexes species with TL >= 3.0. Trophic levels from
#     McCormack et al. (2020) Prydz Bay Ecopath model, Table 2.
#     Decade-averaged.
#   Ecological significance: The Marine Trophic Index (MTI) is a CBD
#     indicator of marine ecosystem health. Declines in MTL indicate
#     "fishing down the food web" — selective removal of high-TL
#     species shifts the community toward smaller, lower-TL organisms.
#     Pauly & Watson (2005) reported widespread global declines of
#     0.05-0.10 TL units per decade.
#   Interpretation caveat: In this system, MTL is heavily influenced by
#     whichever group dominates community biomass. Removing whales (TL
#     ~3.9) can shift biomass-weighted MTL upward if the remaining
#     consumers are higher-TL fish and seals. Direction of change should
#     be interpreted jointly with the biomass metrics.
#   Why not scored as ratio: MTL operates on a compressed scale (~3.0-
#     5.5). Ratios are always close to 1.0 (e.g., 3.8/4.0 = 0.95) and
#     mask ecologically significant shifts of 0.1-0.2 TL units. The
#     absolute difference in TL units is the standard reporting format.
#   References:
#     Pauly, D. & Watson, R. (2005). Background and interpretation of
#       the 'Marine Trophic Index' as a measure of biodiversity. Phil.
#       Trans. R. Soc. B 360:415-423.
#     Shannon, L. et al. (2014). Trophic level-based indicators to
#       track fishing impacts across marine ecosystems. Mar. Ecol. Prog.
#       Ser. 512:115-140.
#
# htl_indicator (High Trophic Level Indicator)
#   Definition: Proportion of total consumer biomass (TL >= 3.0) that
#     is at trophic level >= 4.0.
#   Calculation: sum(B_i for TL_i >= 4.0) / sum(B_i for TL_i >= 3.0).
#     Decade-averaged.
#   Ecological significance: Captures the relative dominance of high-TL
#     predators within the consumer community. Declines indicate loss
#     of top predators relative to mid-trophic consumers. Complementary
#     to MTL but more sensitive to changes at the top of the food web.
#   Why not scored as ratio: Already a proportion (0-1). A ratio of
#     proportions obscures the absolute percentage-point change, which
#     is the ecologically meaningful quantity (e.g., a decline from 0.45
#     to 0.35 is a major 10 pp shift but only a ratio of 0.78).
#   Reference: Shannon, L. et al. (2014). Mar. Ecol. Prog. Ser.
#     512:115-140.
#
# large_fish_indicator (Fish Community LFI)
#   Definition: Proportion of total fish biomass above 1000g body weight,
#     restricted to modelled fish species only.
#   Calculation: mizer::getProportionOfLargeFish(sim,
#     species=SPECIES_GROUPS$fish, threshold_w=1000, min_w=10,
#     max_w=1e6, biomass_proportion=TRUE). Decade-averaged.
#   Species scope: mesopelagic fishes, bathypelagic fishes, shelf and
#     coastal fishes, toothfishes. Explicitly excludes marine mammals
#     and seals, which dominate total community large-organism biomass
#     but would overwhelm a community-level LFI.
#   Ecological significance: The LFI captures selective depletion of
#     large-bodied fish. In this system, it primarily tracks toothfish
#     and shelf/coastal fish relative to total fish biomass, since
#     mesopelagic and bathypelagic fishes rarely exceed 1 kg. The LFI
#     is a GES (Good Environmental Status) indicator under the EU MSFD
#     and is commonly used in ICES ecosystem assessments.
#   Why restricted to fish: A whole-community LFI would be dominated
#     by marine mammal biomass (whales, seals), making it insensitive
#     to changes in fish community structure. The mean_weight and
#     mean_max_weight metrics (computed across all consumers) capture
#     community-wide size shifts including marine mammals.
#   Why not scored as ratio: Same as HTL — already a proportion.
#   References:
#     Greenstreet, S.P.R. et al. (2011). Assessing the status of
#       demersal fish in the North Sea: a practical approach using the
#       Large Fish Indicator. ICES J. Mar. Sci. 68:1821-1830.
#     Shin, Y.-J. et al. (2010). Using indicators for evaluating,
#       comparing, and communicating the ecological status of exploited
#       marine ecosystems. ICES J. Mar. Sci. 67:686-691.
#
# mean_weight (Mean Individual Weight of Consumers)
#   Definition: Mean body weight of individuals in the consumer community
#     (all species with TL >= 3.0), weighted by abundance across the
#     size spectrum from 1g to 100,000,000g (100 tonnes).
#   Calculation: mizer::getMeanWeight(sim, species=SPECIES_GROUPS$consumers,
#     min_w=1, max_w=1e8). Decade-averaged.
#   Ecological significance: Reflects community-wide size structure
#     shifts. Declines indicate truncation of the size distribution —
#     loss of large individuals due to exploitation or habitat change.
#     Unlike LFI (which is restricted to fish), this metric spans all
#     consumers including marine mammals, so it captures the loss of
#     large whales during the whaling era and their subsequent recovery.
#   References:
#     Shin, Y.-J. et al. (2005). Using size-based indicators to
#       evaluate the ecosystem effects of fishing. ICES J. Mar. Sci.
#       62:384-396.
#     Blanchard, J.L. et al. (2014). J. Appl. Ecol. 51:612-622.
#
# mean_max_weight (Mean Maximum Weight of Consumers)
#   Definition: Biomass-weighted mean of the asymptotic maximum weight
#     (w_inf) across all consumer species.
#   Calculation: mizer::getMeanMaxWeight(sim,
#     species=SPECIES_GROUPS$consumers, measure="biomass").
#     Decade-averaged.
#   Ecological significance: A complementary size indicator to
#     mean_weight. While mean_weight reflects the actual size of
#     individuals (influenced by growth, mortality, and recruitment),
#     mean_max_weight reflects the species composition — which
#     functional groups dominate community biomass. A decline in
#     mean_max_weight without a corresponding decline in mean_weight
#     indicates a shift from large-bodied species to small-bodied
#     species, even if the remaining individuals are well-grown.
#   Reference: Jennings, S. et al. (2002). Long-term trends in the
#     trophic structure of the North Sea fish community: evidence from
#     stable-isotope analysis, size-spectra and community metrics.
#     Mar. Biol. 141:1085-1097.
#
# predator_prey_ratio
#   Definition: Ratio of apex predator biomass to mid-trophic prey
#     biomass.
#   Calculation: sum(B_apex) / sum(B_mid_trophic_prey), where
#     apex = sperm whales + orca + leopard seals, and
#     mid_trophic_prey = mesopelagic fishes + bathypelagic fishes +
#     shelf and coastal fishes + squids. Decade-averaged.
#   Ecological significance: Captures the balance between top-down
#     control and mid-trophic prey availability. Declines indicate
#     loss of apex predator biomass relative to prey, suggesting
#     weakened top-down regulation. Increases could indicate prey
#     depletion outpacing predator decline.
#   Why not scored as ratio-to-B0: A ratio of ratios is difficult to
#     interpret and communicate. The absolute predator-prey ratio and
#     its departure from the B0 distribution directly describe the
#     trophic balance, and the B0 envelope provides context for natural
#     variability.
#   Reference: Link, J.S. (2005). Translating ecosystem indicators
#     into decision criteria. ICES J. Mar. Sci. 62:569-576.
#
# consumer_ltl_ratio
#   Definition: Ratio of total consumer biomass to total lower trophic
#     level biomass.
#   Calculation: sum(B_consumers) / sum(B_ltl), where
#     consumers = all species with TL >= 3.0 (fish, seals, whales,
#     birds, squids), and ltl = krill + other krill + mesozooplankton
#     + macrozooplankton + salps. Decade-averaged.
#   Ecological significance: A whole-ecosystem trophic efficiency
#     indicator. Reflects the proportion of lower trophic production
#     that is channelled into higher trophic levels. Changes indicate
#     shifts in energy flow pathways, trophic transfer efficiency, or
#     food web structure.
#   Reference: Fulton, E.A. et al. (2005). Which ecological indicators
#     can robustly detect effects of fishing? ICES J. Mar. Sci.
#     62:540-551.
#
# =====================================================================
# CATEGORY C: EXPLOITATION METRICS
# =====================================================================
#
# exploitation_total
#   Definition: Total catch / total biomass across all exploited species.
#   Calculation: Only species with non-zero catch are included in the
#     denominator. Yields fishing mortality rate F (year^-1) at the
#     community level.
#   Context: Link & Watson (2019) use system-level exploitation rate
#     as a primary ecosystem overfishing indicator, with thresholds
#     at community-level F.
#
# exploitation_whale
#   Definition: Total cetacean catch / total cetacean biomass.
#   Species: baleen whales + minke whales + sperm whales + orca.
#   Context: Historical whaling rates reached F > 0.05-0.10 during
#     peak exploitation (1920s-1960s). IWC management aims for
#     near-zero catch in the modern era under the moratorium.
#
# exploitation_krill
#   Definition: Krill catch / krill biomass.
#   Species: antarctic krill.
#   Context: CCAMLR manages krill harvest using precautionary catch
#     limits derived from gamma_1 and gamma_2 decision rules. Current
#     exploitation rates are low (<1% of estimated biomass) but
#     concentrated in predator foraging areas.
#   Reference: Constable, A.J. et al. (2000). ICES J. Mar. Sci.
#     57:778-791.
#
###############################################################################
#             METRIC CATEGORY CLASSIFICATION
###############################################################################
#
# Three categories determine how each metric is scored:
#
# Category A: BIOMASS — depletion ratios with literature thresholds
#   These metrics have direct ecological interpretation as proportional
#   declines from pristine biomass.
#
# Category B: STRUCTURAL — empirical deviation from B0 ensemble
#   These metrics have non-linear, non-proportional relationships with
#   ecosystem health. A ratio to B0 is ecologically misleading (e.g.,
#   slope_fishing / slope_B0 has no consistent interpretation because
#   the slope is negative; MTL ratios are always near 1.0 and mask
#   meaningful changes). Instead, we ask: "has exploitation/climate
#   pushed this metric outside the range of natural variability in the
#   pre-exploitation ensemble?"
#
# Category C: EXPLOITATION — absolute fishing mortality rates
#   Not ratio-based; reported as direct F values.
###############################################################################

METRIC_CATEGORY <- list(
  # Category A: Biomass metrics
  total_biomass   = "biomass",
  whale_biomass   = "biomass",
  baleen_biomass  = "biomass",
  seal_biomass    = "biomass",
  fish_biomass    = "biomass",
  krill_biomass   = "biomass",
  ltl_biomass     = "biomass",
  apex_biomass    = "biomass",
  # Category B: Structural/trophic metrics
  spectrum_slope       = "structural",
  spectrum_intercept   = "structural",
  mean_tl              = "structural",
  htl_indicator        = "structural",
  large_fish_indicator = "structural",
  mean_weight          = "structural",
  mean_max_weight      = "structural",
  predator_prey_ratio  = "structural",
  consumer_ltl_ratio   = "structural",
  # Category C: Exploitation metrics
  exploitation_total = "exploitation",
  exploitation_whale = "exploitation",
  exploitation_krill = "exploitation"
)

BIOMASS_METRICS <- names(METRIC_CATEGORY)[METRIC_CATEGORY == "biomass"]
STRUCTURAL_METRICS <- names(METRIC_CATEGORY)[METRIC_CATEGORY == "structural"]
EXPLOITATION_METRICS <- names(METRIC_CATEGORY)[METRIC_CATEGORY == "exploitation"]

###############################################################################
# Reference Points with Full Provenance
###############################################################################
#
# =====================================================================
# BIOMASS THRESHOLDS (Category A)
# =====================================================================
#
# 0.75 B0: CCAMLR krill decision rule gamma_2 (escapement criterion).
#   "Choose gamma_2 so that the median escapement at the end of a 20-year
#    period is 75% of the median pre-exploitation level."
#   Adopted by CCAMLR to maintain krill biomass for dependent predators.
#   Applied here to all biomass metrics as a general ecosystem target:
#   maintaining >= 75% of pre-exploitation biomass across all functional
#   groups ensures CAMLR Convention Article II objectives are met.
#   Source: Constable, A.J. et al. (2000). Managing fisheries to
#     conserve the Antarctic marine ecosystem: practical implementation
#     of the Convention on the Conservation of Antarctic Marine Living
#     Resources (CCAMLR). ICES J. Mar. Sci. 57:778-791.
#   See also: SC-CAMLR (2010). Report of the 29th Meeting of the
#     Scientific Committee, Annex 4 (krill fishery management).
#
# 0.54 K: IWC Revised Management Procedure (RMP) protection level.
#   The RMP sets catch limits that maintain whale populations above
#   54% of estimated carrying capacity with high probability. This is
#   the boundary below which the RMP begins to reduce and eventually
#   halt commercial catch.
#   Source: IWC (1994). The Revised Management Procedure (RMP) for
#     baleen whales. Rep. Int. Whal. Commn 44:145-152.
#   See also: Punt, A.E. & Donovan, G.P. (2007). Developing management
#     procedures that are robust to uncertainty: lessons from the
#     International Whaling Commission. ICES J. Mar. Sci. 64:603-612.
#   Applicability: ONLY baleen_biomass and whale_biomass. This threshold
#     is specific to cetacean management and has no equivalent for other
#     taxa.
#
# 0.40 B0: BMSY proxy (general fisheries limit reference point).
#   Under Schaefer surplus production dynamics, BMSY ~ 0.4 B0 for
#   typical fish stocks. This is used as a limit reference point (below
#   which rebuilding is required) under the US Magnuson-Stevens Act
#   and is adopted in many RFMO management frameworks.
#   Source: Restrepo, V.R. et al. (1998). Technical guidance on the
#     use of precautionary approaches to implementing National Standard
#     1 of the Magnuson-Stevens Fishery Conservation and Management Act.
#     NOAA Tech. Memo. NMFS-F/SPO-31, 54 pp.
#   Note: For marine mammals, 0.4 B0 represents severe depletion well
#     below any management target. It is included as a universal limit.
#
# 0.20 B0: Severely depleted / collapse risk threshold.
#   Combines two sources:
#   (a) CCAMLR gamma_1: "Choose gamma_1 so that the probability of
#       the spawning biomass dropping below 20% of the pre-exploitation
#       median over a 20-year period is less than 10%."
#       Source: Constable et al. (2000).
#   (b) US Magnuson-Stevens MSST (Minimum Stock Size Threshold):
#       MSST = 0.5 * BMSY ~ 0.5 * 0.4 * B0 = 0.20 B0.
#       Below MSST, a stock is classified as "overfished."
#       Source: Restrepo et al. (1998).
#   Applied here as a universal collapse threshold: any biomass metric
#   below 20% of B0 indicates severe depletion approaching functional
#   extinction.
#
# =====================================================================
# STRUCTURAL THRESHOLDS (Category B)
# =====================================================================
#
# No literature-based proportional thresholds exist for structural
# metrics. Instead, the B0 ensemble (1841-1860, climate-only) provides
# an empirical reference envelope:
#
#   B0 5th-95th percentile: "Natural range"
#     The 90% credible interval of each metric's value under pre-
#     exploitation conditions and parameter uncertainty. Fishing values
#     outside this range indicate the metric has been pushed beyond
#     what natural variability alone would produce.
#
#   B0 1st-99th percentile: "Substantially altered"
#     A wider envelope capturing extreme parameter combinations. Values
#     outside this range represent departures that exceed virtually all
#     plausible pre-exploitation states.
#
#   z-score: (value - B0 median) / SD(B0 ensemble)
#     Standardized departure in units of B0 variability. Allows
#     comparison across metrics with different units and scales.
#     |z| > 2 corresponds approximately to outside the 5th-95th range
#     for normally distributed metrics.
#
# This approach follows Blanchard et al. (2014) who used unfished
# simulations as a reference baseline for evaluating community
# indicators from size spectrum models.
###############################################################################

THRESHOLDS_GENERAL <- c(ccamlr_gamma2 = 0.75, bmsy_proxy = 0.40, ccamlr_gamma1 = 0.20)
THRESHOLDS_WHALE <- c(ccamlr_gamma2 = 0.75, iwc_rmp_054 = 0.54, bmsy_proxy = 0.40, ccamlr_gamma1 = 0.20)
WHALE_METRICS <- c("baleen_biomass", "whale_biomass")

get_thresholds_for_metric <- function(metric_name) {
  if (metric_name %in% WHALE_METRICS) return(THRESHOLDS_WHALE)
  return(THRESHOLDS_GENERAL)
}

###############################################################################
# Species Group Definitions
###############################################################################

SPECIES_GROUPS <- list(
  baleen_whales = c("baleen whales", "minke whales"),
  all_whales = c("baleen whales", "minke whales", "sperm whales", "orca"),
  apex_predators = c("sperm whales", "orca", "leopard seals"),
  marine_mammals = c("baleen whales", "minke whales", "sperm whales", "orca",
                     "leopard seals", "small divers", "medium divers", "large divers"),
  seals = c("leopard seals", "small divers", "medium divers", "large divers"),
  birds = c("flying birds"),
  fish = c("mesopelagic fishes", "bathypelagic fishes",
           "shelf and coastal fishes", "toothfishes"),
  fish_large = c("toothfishes", "shelf and coastal fishes"),
  fish_small = c("mesopelagic fishes", "bathypelagic fishes"),
  krill = c("antarctic krill"),
  ltl = c("antarctic krill", "other krill", "mesozooplankton",
          "other macrozooplankton", "salps"),
  consumers = c("mesopelagic fishes", "bathypelagic fishes",
                "shelf and coastal fishes", "flying birds", "small divers",
                "squids", "toothfishes", "leopard seals", "medium divers",
                "large divers", "minke whales", "orca", "sperm whales",
                "baleen whales"),
  mid_trophic_prey = c("mesopelagic fishes", "bathypelagic fishes",
                       "shelf and coastal fishes", "squids")
)

# Trophic levels from McCormack et al. (2020) Prydz Bay Ecopath, Table 2
TROPHIC_LEVELS <- c(
  "mesozooplankton" = 3.272, "other krill" = 2.398,
  "other macrozooplankton" = 3.231, "antarctic krill" = 2.398,
  "salps" = 2.284, "mesopelagic fishes" = 3.539,
  "bathypelagic fishes" = 4.055, "shelf and coastal fishes" = 4.281,
  "toothfishes" = 4.966, "flying birds" = 4.103,
  "small divers" = 3.787, "medium divers" = 4.999,
  "large divers" = 5.075, "leopard seals" = 4.858,
  "squids" = 4.336, "minke whales" = 3.955,
  "orca" = 5.301, "sperm whales" = 5.342,
  "baleen whales" = 3.867
)

###############################################################################
# Helper Functions
###############################################################################

`%||%` <- function(x, y) if (is.null(x)) y else x

safe_time_extract <- function(x, time_range = NULL) {
  if (is.null(x)) return(NA)
  if (is.matrix(x) || is.data.frame(x)) {
    if (is.null(time_range)) time_range <- seq_len(nrow(x))
    vals <- if (ncol(x) == 1) x[time_range, 1] else rowSums(x[time_range, , drop = FALSE])
  } else {
    if (is.null(time_range)) time_range <- seq_along(x)
    vals <- x[time_range]
  }
  return(as.numeric(vals))
}

###############################################################################
# Metric Calculation Functions
###############################################################################

calculate_group_biomass <- function(sim, time_range = NULL, species_group = NULL) {
  biomass <- getBiomass(sim)
  if (is.null(time_range)) time_range <- seq_len(nrow(biomass))
  if (is.null(species_group)) {
    total <- rowSums(biomass[time_range, , drop = FALSE])
  } else {
    valid_sp <- species_group[species_group %in% colnames(biomass)]
    if (length(valid_sp) == 0) return(NA)
    total <- rowSums(biomass[time_range, valid_sp, drop = FALSE])
  }
  return(mean(total, na.rm = TRUE))
}

calculate_spectrum_slope_intercept <- function(sim, time_range = NULL,
                                               min_w = 1, max_w = 1e6) {
  # Try getCommunitySlope first (preferred method)
  slope_data <- tryCatch(
    mizer::getCommunitySlope(sim, min_w = min_w, max_w = max_w, biomass = TRUE),
    error = function(e) {
      warning(sprintf("getCommunitySlope failed: %s. Using manual calculation.", e$message))
      NULL
    }
  )
  
  # If getCommunitySlope fails, calculate manually from size spectrum
  if (is.null(slope_data)) {
    tryCatch({
      times <- as.numeric(dimnames(sim@n)$time)
      if (is.null(time_range)) time_range <- seq_along(times)
      w <- sim@params@w
      n <- sim@n
      
      # Calculate community size spectrum (sum across species)
      community_n <- apply(n, c(1, 3), sum)  # Sum over species dimension
      
      # Fit linear model on log-log scale for specified time range
      log_w <- log10(w[w >= min_w & w <= max_w])
      w_idx <- which(w >= min_w & w <= max_w)
      
      slopes <- numeric(length(time_range))
      intercepts <- numeric(length(time_range))
      
      for (i in seq_along(time_range)) {
        t_idx <- time_range[i]
        log_n <- log10(community_n[t_idx, w_idx] + 1e-20)  # Add small constant to avoid log(0)
        fit <- lm(log_n ~ log_w)
        slopes[i] <- coef(fit)[2]
        intercepts[i] <- coef(fit)[1]
      }
      
      return(c(slope = mean(slopes, na.rm = TRUE), 
               intercept = mean(intercepts, na.rm = TRUE)))
    }, error = function(e) {
      warning(sprintf("Manual spectrum calculation also failed: %s", e$message))
      return(c(slope = NA, intercept = NA))
    })
  } else {
    # Use existing getCommunitySlope results
    if (is.null(time_range)) time_range <- seq_len(nrow(slope_data))
    slope_val <- mean(slope_data[time_range, "slope"], na.rm = TRUE)
    intercept_val <- mean(slope_data[time_range, "intercept"], na.rm = TRUE)
    return(c(slope = slope_val, intercept = intercept_val))
  }
}

calculate_lfi_mizer <- function(sim, time_range = NULL, species_group = NULL,
                                threshold_w = 1000, min_w = 10, max_w = 1e6) {
  lfi_data <- tryCatch(
    mizer::getProportionOfLargeFish(sim, species = species_group,
                                    min_w = min_w, max_w = max_w,
                                    threshold_w = threshold_w,
                                    biomass_proportion = TRUE),
    error = function(e) NULL
  )
  if (is.null(lfi_data)) return(NA)
  vals <- safe_time_extract(lfi_data, time_range)
  return(mean(vals, na.rm = TRUE))
}

calculate_mean_weight_mizer <- function(sim, time_range = NULL,
                                        species_group = NULL,
                                        min_w = 1, max_w = 1e8) {
  mw_data <- tryCatch(
    mizer::getMeanWeight(sim, species = species_group,
                         min_w = min_w, max_w = max_w),
    error = function(e) NULL
  )
  if (is.null(mw_data)) return(NA)
  vals <- safe_time_extract(mw_data, time_range)
  return(mean(vals, na.rm = TRUE))
}

calculate_mean_max_weight_mizer <- function(sim, time_range = NULL,
                                            species_group = NULL) {
  mmw_data <- tryCatch(
    mizer::getMeanMaxWeight(sim, species = species_group, measure = "biomass"),
    error = function(e) NULL
  )
  if (is.null(mmw_data)) return(NA)
  vals <- safe_time_extract(mmw_data, time_range)
  return(mean(vals, na.rm = TRUE))
}

calculate_mean_tl <- function(sim, time_range = NULL,
                              tl_vector = TROPHIC_LEVELS,
                              consumers_only = TRUE) {
  biomass <- getBiomass(sim)
  if (is.null(time_range)) time_range <- seq_len(nrow(biomass))
  sp_names <- colnames(biomass)
  valid_sp <- sp_names[sp_names %in% names(tl_vector)]
  if (consumers_only) valid_sp <- valid_sp[tl_vector[valid_sp] >= 3.0]
  if (length(valid_sp) == 0) return(NA)
  mtl_values <- numeric(length(time_range))
  for (i in seq_along(time_range)) {
    t <- time_range[i]
    b <- biomass[t, valid_sp]
    tl <- tl_vector[valid_sp]
    total_b <- sum(b)
    mtl_values[i] <- if (total_b > 0) sum(b * tl) / total_b else NA
  }
  return(mean(mtl_values, na.rm = TRUE))
}

calculate_htl_indicator <- function(sim, time_range = NULL,
                                    tl_threshold = 4.0,
                                    tl_vector = TROPHIC_LEVELS) {
  biomass <- getBiomass(sim)
  if (is.null(time_range)) time_range <- seq_len(nrow(biomass))
  sp_names <- colnames(biomass)
  valid_sp <- sp_names[sp_names %in% names(tl_vector)]
  high_tl_sp <- valid_sp[tl_vector[valid_sp] >= tl_threshold]
  consumer_sp <- valid_sp[tl_vector[valid_sp] >= 3.0]
  if (length(high_tl_sp) == 0 || length(consumer_sp) == 0) return(NA)
  htl_values <- numeric(length(time_range))
  for (i in seq_along(time_range)) {
    t <- time_range[i]
    htl_values[i] <- sum(biomass[t, high_tl_sp]) / sum(biomass[t, consumer_sp])
  }
  return(mean(htl_values, na.rm = TRUE))
}

calculate_predator_prey_ratio <- function(sim, time_range = NULL,
                                          predator_group = NULL,
                                          prey_group = NULL) {
  if (is.null(predator_group)) predator_group <- SPECIES_GROUPS$apex_predators
  if (is.null(prey_group)) prey_group <- SPECIES_GROUPS$mid_trophic_prey
  pred_b <- calculate_group_biomass(sim, time_range, predator_group)
  prey_b <- calculate_group_biomass(sim, time_range, prey_group)
  if (is.na(pred_b) || is.na(prey_b) || prey_b == 0) return(NA)
  return(pred_b / prey_b)
}

calculate_consumer_ltl_ratio <- function(sim, time_range = NULL) {
  consumer_b <- calculate_group_biomass(sim, time_range, SPECIES_GROUPS$consumers)
  ltl_b <- calculate_group_biomass(sim, time_range, SPECIES_GROUPS$ltl)
  if (is.na(consumer_b) || is.na(ltl_b) || ltl_b == 0) return(NA)
  return(consumer_b / ltl_b)
}

calculate_exploitation_rate <- function(sim, time_range = NULL,
                                        species_group = NULL) {
  catch_data <- tryCatch(getYield(sim), error = function(e) NULL)
  biomass_data <- getBiomass(sim)
  if (is.null(catch_data)) return(0)
  if (is.null(time_range)) time_range <- seq_len(nrow(biomass_data))
  sp_names <- colnames(biomass_data)
  if (is.null(species_group)) {
    total_catch <- colSums(catch_data[time_range, , drop = FALSE])
    species_group <- names(total_catch[total_catch > 0])
    if (length(species_group) == 0) return(0)
  }
  valid_sp <- species_group[species_group %in% sp_names]
  if (length(valid_sp) == 0) return(NA)
  exploitation_rates <- numeric(length(time_range))
  for (i in seq_along(time_range)) {
    t <- time_range[i]
    total_catch <- sum(catch_data[t, valid_sp, drop = TRUE], na.rm = TRUE)
    total_biomass <- sum(biomass_data[t, valid_sp, drop = TRUE], na.rm = TRUE)
    exploitation_rates[i] <- if (total_biomass > 0) total_catch / total_biomass else NA
  }
  return(mean(exploitation_rates, na.rm = TRUE))
}

calculate_whale_exploitation <- function(sim, time_range = NULL) {
  calculate_exploitation_rate(sim, time_range, SPECIES_GROUPS$all_whales)
}

calculate_krill_exploitation <- function(sim, time_range = NULL) {
  calculate_exploitation_rate(sim, time_range, SPECIES_GROUPS$krill)
}

###############################################################################
# Paired Ensemble Processing
###############################################################################

extract_simulation_metrics <- function(sim, decades_df = DECADES) {
  times <- as.numeric(dimnames(sim@n)$time)
  results <- list()
  for (i in seq_len(nrow(decades_df))) {
    decade <- decades_df[i, ]
    time_range <- which(times >= decade$start & times <= decade$end)
    if (length(time_range) == 0) next
    spec <- calculate_spectrum_slope_intercept(sim, time_range)
    metrics <- data.frame(
      decade = decade$label, start_year = decade$start, end_year = decade$end,
      total_biomass = calculate_group_biomass(sim, time_range),
      whale_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$all_whales),
      baleen_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$baleen_whales),
      seal_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$seals),
      fish_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$fish),
      krill_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$krill),
      ltl_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$ltl),
      apex_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$apex_predators),
      spectrum_slope = unname(spec["slope"]),
      spectrum_intercept = unname(spec["intercept"]),
      mean_tl = calculate_mean_tl(sim, time_range, consumers_only = TRUE),
      htl_indicator = calculate_htl_indicator(sim, time_range),
      large_fish_indicator = calculate_lfi_mizer(sim, time_range,
                                                 species_group = SPECIES_GROUPS$fish,
                                                 threshold_w = 1000),
      mean_weight = calculate_mean_weight_mizer(sim, time_range,
                                                 species_group = SPECIES_GROUPS$consumers),
      mean_max_weight = calculate_mean_max_weight_mizer(sim, time_range,
                                                         species_group = SPECIES_GROUPS$consumers),
      predator_prey_ratio = calculate_predator_prey_ratio(sim, time_range),
      consumer_ltl_ratio = calculate_consumer_ltl_ratio(sim, time_range),
      exploitation_total = calculate_exploitation_rate(sim, time_range),
      exploitation_whale = calculate_whale_exploitation(sim, time_range),
      exploitation_krill = calculate_krill_exploitation(sim, time_range),
      stringsAsFactors = FALSE
    )
    results[[i]] <- metrics
  }
  return(bind_rows(results))
}

extract_b0_metrics <- function(sim) {
  times <- as.numeric(dimnames(sim@n)$time)
  time_range <- which(times >= B0_PERIOD[1] & times <= B0_PERIOD[2])
  if (length(time_range) == 0) { warning("No B0 time steps"); return(NULL) }
  spec <- calculate_spectrum_slope_intercept(sim, time_range)
  c(
    total_biomass = calculate_group_biomass(sim, time_range),
    whale_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$all_whales),
    baleen_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$baleen_whales),
    seal_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$seals),
    fish_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$fish),
    krill_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$krill),
    ltl_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$ltl),
    apex_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$apex_predators),
    spectrum_slope = unname(spec["slope"]),
    spectrum_intercept = unname(spec["intercept"]),
    mean_tl = calculate_mean_tl(sim, time_range, consumers_only = TRUE),
    htl_indicator = calculate_htl_indicator(sim, time_range),
    large_fish_indicator = calculate_lfi_mizer(sim, time_range,
                                               species_group = SPECIES_GROUPS$fish,
                                               threshold_w = 1000),
    mean_weight = calculate_mean_weight_mizer(sim, time_range,
                                               species_group = SPECIES_GROUPS$consumers),
    mean_max_weight = calculate_mean_max_weight_mizer(sim, time_range,
                                                       species_group = SPECIES_GROUPS$consumers),
    predator_prey_ratio = calculate_predator_prey_ratio(sim, time_range),
    consumer_ltl_ratio = calculate_consumer_ltl_ratio(sim, time_range)
  )
}

###############################################################################
# Checkpoint Functions for Progressive Saving and Resume
###############################################################################

#' Save checkpoint for ongoing ensemble processing
#' @param checkpoint_dir Directory for checkpoint files
#' @param sim_num Current simulation number
#' @param fishing_results List of fishing metrics accumulated so far
#' @param climate_results List of climate-only metrics accumulated so far
#' @param b0_results List of B0 reference metrics accumulated so far
save_checkpoint <- function(checkpoint_dir, sim_num, fishing_results, climate_results, b0_results) {
  if (!dir.exists(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  }
  checkpoint_file <- file.path(checkpoint_dir, sprintf("checkpoint_sim_%04d.rds", sim_num))
  checkpoint_data <- list(
    last_sim = sim_num,
    fishing_results = fishing_results,
    climate_results = climate_results,
    b0_results = b0_results,
    timestamp = Sys.time()
  )
  saveRDS(checkpoint_data, checkpoint_file)
  cat(sprintf("  [Checkpoint saved: sim %d]\n", sim_num))
}

#' Find all existing checkpoint files
#' @param checkpoint_dir Directory containing checkpoints
#' @return Vector of checkpoint file paths, sorted by simulation number
find_checkpoints <- function(checkpoint_dir) {
  if (!dir.exists(checkpoint_dir)) return(character(0))
  checkpoint_files <- list.files(checkpoint_dir, pattern = "^checkpoint_sim_\\d{4}\\.rds$", full.names = TRUE)
  if (length(checkpoint_files) == 0) return(character(0))
  # Sort by simulation number
  sim_nums <- as.integer(sub(".*checkpoint_sim_(\\d{4})\\.rds", "\\1", basename(checkpoint_files)))
  checkpoint_files[order(sim_nums)]
}

#' Determine resume point from existing checkpoints
#' @param checkpoint_dir Directory containing checkpoints
#' @return List with resume_from (sim number) and checkpoint_file (path to load), or NULL if starting fresh
get_resume_point <- function(checkpoint_dir) {
  checkpoints <- find_checkpoints(checkpoint_dir)
  if (length(checkpoints) == 0) {
    return(list(resume_from = 0, checkpoint_file = NULL))
  }
  # Get highest checkpoint
  last_checkpoint <- checkpoints[length(checkpoints)]
  checkpoint_data <- readRDS(last_checkpoint)
  list(resume_from = checkpoint_data$last_sim, checkpoint_file = last_checkpoint, checkpoint_data = checkpoint_data)
}

#' Merge all checkpoint files into final results
#' @param checkpoint_dir Directory containing checkpoints
#' @param current_fishing Current fishing results list
#' @param current_climate Current climate results list
#' @param current_b0 Current B0 results list
#' @return List with merged fishing_raw, climate_raw, b0_reference
merge_checkpoints <- function(checkpoint_dir, current_fishing, current_climate, current_b0) {
  checkpoints <- find_checkpoints(checkpoint_dir)
  
  all_fishing <- current_fishing
  all_climate <- current_climate
  all_b0 <- current_b0
  
  if (length(checkpoints) > 0) {
    cat(sprintf("\nMerging %d checkpoint files...\n", length(checkpoints)))
    for (cp_file in checkpoints) {
      cp_data <- readRDS(cp_file)
      all_fishing <- c(all_fishing, cp_data$fishing_results)
      all_climate <- c(all_climate, cp_data$climate_results)
      all_b0 <- c(all_b0, cp_data$b0_results)
    }
  }
  
  list(
    fishing_raw = bind_rows(all_fishing),
    climate_raw = bind_rows(all_climate),
    b0_reference = bind_rows(all_b0)
  )
}

#' Clean up checkpoint files after successful completion
#' @param checkpoint_dir Directory containing checkpoints
cleanup_checkpoints <- function(checkpoint_dir) {
  if (!dir.exists(checkpoint_dir)) return(invisible(NULL))
  checkpoints <- find_checkpoints(checkpoint_dir)
  if (length(checkpoints) > 0) {
    cat(sprintf("Cleaning up %d checkpoint files...\n", length(checkpoints)))
    file.remove(checkpoints)
    # Remove directory if empty
    if (length(list.files(checkpoint_dir)) == 0) {
      unlink(checkpoint_dir, recursive = TRUE)
    }
  }
  invisible(NULL)
}

process_paired_ensemble <- function(fishing_file, climate_file, max_sims = NULL, checkpoint_interval = 10) {
  cat("=============================================================\n")
  cat("Processing PAIRED ensemble (with checkpoint support)\n")
  cat("=============================================================\n\n")
  
  # Set up checkpoint directory
  checkpoint_dir <- file.path(OUTPUT_DIR_LARGE, "checkpoints")
  
  # Check for existing checkpoints
  resume_info <- get_resume_point(checkpoint_dir)
  start_sim <- resume_info$resume_from
  
  if (start_sim > 0) {
    cat(sprintf("*** RESUMING from checkpoint at simulation %d ***\n\n", start_sim))
  } else {
    cat("Starting fresh (no checkpoints found)\n\n")
  }
  
  cat("Loading fishing ensemble...\n")
  mc_fish <- readRDS(fishing_file)
  cat("Loading climate-only ensemble...\n")
  mc_clim <- readRDS(climate_file)
  extract_sims <- function(mc) {
    if ("simulations" %in% names(mc)) return(mc$simulations)
    if (is.list(mc) && inherits(mc[[1]], "MizerSim")) return(mc)
    stop("Unrecognized ensemble structure")
  }
  fish_sims <- extract_sims(mc_fish)
  clim_sims <- extract_sims(mc_clim)
  n_sims <- min(length(fish_sims), length(clim_sims))
  if (!is.null(max_sims)) n_sims <- min(n_sims, max_sims)
  
  cat(sprintf("  Processing %d PAIRED simulations\n", n_sims))
  cat(sprintf("  Checkpoint interval: every %d simulations\n", checkpoint_interval))
  cat(sprintf("  Starting from simulation: %d\n\n", start_sim + 1))
  
  # Initialize results lists (empty - will merge with checkpoints at end)
  fishing_results <- climate_results <- b0_results <- list()
  
  pb <- txtProgressBar(min = start_sim, max = n_sims, style = 3)
  for (i in (start_sim + 1):n_sims) {
    setTxtProgressBar(pb, i)
    fish_metrics <- tryCatch(extract_simulation_metrics(fish_sims[[i]]), error = function(e) NULL)
    clim_metrics <- tryCatch(extract_simulation_metrics(clim_sims[[i]]), error = function(e) NULL)
    b0_ref <- tryCatch(extract_b0_metrics(clim_sims[[i]]), error = function(e) NULL)
    if (!is.null(fish_metrics) && nrow(fish_metrics) > 0) {
      fish_metrics$sim_id <- i
      fishing_results[[length(fishing_results) + 1]] <- fish_metrics
    }
    if (!is.null(clim_metrics) && nrow(clim_metrics) > 0) {
      clim_metrics$sim_id <- i
      climate_results[[length(climate_results) + 1]] <- clim_metrics
    }
    if (!is.null(b0_ref)) {
      b0_df <- as.data.frame(t(b0_ref))
      b0_df$sim_id <- i
      b0_results[[length(b0_results) + 1]] <- b0_df
    }
    
    # Save checkpoint every N simulations
    if (i %% checkpoint_interval == 0) {
      save_checkpoint(checkpoint_dir, i, fishing_results, climate_results, b0_results)
      # Clear current results after checkpoint (they're saved)
      fishing_results <- climate_results <- b0_results <- list()
    }
  }
  close(pb)
  
  cat(sprintf("\n  Completed processing loop\n"))
  
  # Merge all checkpoints with final results
  cat("\nMerging checkpoint files and final results...\n")
  final_results <- merge_checkpoints(checkpoint_dir, fishing_results, climate_results, b0_results)
  
  cat(sprintf("  Final data: %d fishing, %d climate-only, %d B0 refs\n",
              nrow(final_results$fishing_raw), nrow(final_results$climate_raw), nrow(final_results$b0_reference)))
  
  # Clean up checkpoint files after successful merge
  cleanup_checkpoints(checkpoint_dir)
  
  return(final_results)
}

###############################################################################
# Empirical Reference Thresholds (B0 Envelope)
###############################################################################

derive_empirical_thresholds <- function(b0_reference) {
  all_metrics <- c(BIOMASS_METRICS, STRUCTURAL_METRICS)
  thresholds <- list()
  for (col in all_metrics) {
    if (!(col %in% names(b0_reference))) next
    vals <- b0_reference[[col]]
    vals <- vals[!is.na(vals)]
    if (length(vals) < 10) {
      thresholds[[col]] <- list(median = NA, mean = NA, sd = NA,
                                q01 = NA, q05 = NA, q10 = NA, q25 = NA,
                                q75 = NA, q90 = NA, q95 = NA, q99 = NA)
      next
    }
    thresholds[[col]] <- list(
      median = median(vals), mean = mean(vals), sd = sd(vals),
      q01 = quantile(vals, 0.01, names = FALSE),
      q05 = quantile(vals, 0.05, names = FALSE),
      q10 = quantile(vals, 0.10, names = FALSE),
      q25 = quantile(vals, 0.25, names = FALSE),
      q75 = quantile(vals, 0.75, names = FALSE),
      q90 = quantile(vals, 0.90, names = FALSE),
      q95 = quantile(vals, 0.95, names = FALSE),
      q99 = quantile(vals, 0.99, names = FALSE)
    )
  }
  cat("\nEmpirical B0 reference thresholds:\n")
  cat("  BIOMASS METRICS (median [Q05-Q95]):\n")
  for (col in BIOMASS_METRICS) {
    th <- thresholds[[col]]
    if (!is.null(th) && !is.na(th$median))
      cat(sprintf("    %-25s: %10.4f [%10.4f - %10.4f]\n", col, th$median, th$q05, th$q95))
  }
  cat("  STRUCTURAL METRICS (median [Q05-Q95] = natural range envelope):\n")
  for (col in STRUCTURAL_METRICS) {
    th <- thresholds[[col]]
    if (!is.null(th) && !is.na(th$median))
      cat(sprintf("    %-25s: %10.4f [%10.4f - %10.4f]\n", col, th$median, th$q05, th$q95))
  }
  return(thresholds)
}

###############################################################################
# Paired Comparison Calculation
###############################################################################

compute_paired_comparisons <- function(fishing_raw, climate_raw, b0_reference,
                                       empirical_thresholds) {
  cat("\nComputing paired comparisons...\n")
  paired <- fishing_raw %>%
    inner_join(climate_raw, by = c("sim_id", "decade", "start_year", "end_year"),
               suffix = c("_fish", "_clim"))
  paired <- paired %>%
    inner_join(b0_reference, by = "sim_id", suffix = c("", "_b0"))
  cat(sprintf("  Paired records: %d (%d sims x %d decades)\n",
              nrow(paired), length(unique(paired$sim_id)),
              length(unique(paired$decade))))

  # Category A: Biomass — RATIOS
  for (metric in BIOMASS_METRICS) {
    fish_col <- paste0(metric, "_fish")
    clim_col <- paste0(metric, "_clim")
    b0_col <- metric
    if (!(fish_col %in% names(paired))) next
    paired[[paste0(metric, "_exploit_ratio")]] <- ifelse(
      paired[[clim_col]] > 0, paired[[fish_col]] / paired[[clim_col]], NA)
    if (b0_col %in% names(paired)) {
      paired[[paste0(metric, "_absolute_ratio")]] <- ifelse(
        paired[[b0_col]] > 0, paired[[fish_col]] / paired[[b0_col]], NA)
      paired[[paste0(metric, "_climate_ratio")]] <- ifelse(
        paired[[b0_col]] > 0, paired[[clim_col]] / paired[[b0_col]], NA)
    }
  }

  # Category B: Structural — DIFFERENCES + B0 ENVELOPE
  for (metric in STRUCTURAL_METRICS) {
    fish_col <- paste0(metric, "_fish")
    clim_col <- paste0(metric, "_clim")
    b0_col <- metric
    if (!(fish_col %in% names(paired))) next

    # Absolute differences
    if (b0_col %in% names(paired)) {
      paired[[paste0(metric, "_absolute_diff")]] <- paired[[fish_col]] - paired[[b0_col]]
      paired[[paste0(metric, "_climate_diff")]] <- paired[[clim_col]] - paired[[b0_col]]
    }
    paired[[paste0(metric, "_exploit_diff")]] <- paired[[fish_col]] - paired[[clim_col]]

    # B0 envelope membership
    th <- empirical_thresholds[[metric]]
    if (!is.null(th) && !is.na(th$q05)) {
      paired[[paste0(metric, "_outside_b0_90")]] <-
        paired[[fish_col]] < th$q05 | paired[[fish_col]] > th$q95
      paired[[paste0(metric, "_outside_b0_80")]] <-
        paired[[fish_col]] < th$q10 | paired[[fish_col]] > th$q90
      paired[[paste0(metric, "_outside_b0_98")]] <-
        paired[[fish_col]] < th$q01 | paired[[fish_col]] > th$q99
      paired[[paste0(metric, "_below_b0_q05")]] <- paired[[fish_col]] < th$q05
      paired[[paste0(metric, "_above_b0_q95")]] <- paired[[fish_col]] > th$q95

      # Same for climate-only values (for climate impact panels)
      paired[[paste0(metric, "_clim_outside_b0_90")]] <-
        paired[[clim_col]] < th$q05 | paired[[clim_col]] > th$q95
      paired[[paste0(metric, "_clim_outside_b0_98")]] <-
        paired[[clim_col]] < th$q01 | paired[[clim_col]] > th$q99
    }
  }

  return(paired)
}

###############################################################################
# Summary Functions
###############################################################################

summarize_biomass_ratios <- function(paired_data) {
  cat("  Summarizing biomass ratios...\n")
  comparison_types <- c("exploit", "absolute", "climate")
  all_summaries <- list()
  for (comp in comparison_types) {
    summary_rows <- list()
    for (metric in BIOMASS_METRICS) {
      ratio_col <- paste0(metric, "_", comp, "_ratio")
      if (!(ratio_col %in% names(paired_data))) next
      decade_summaries <- paired_data %>%
        group_by(decade, start_year, end_year) %>%
        summarise(
          metric_name = metric, metric_category = "biomass", comparison = comp,
          n_sims = n(),
          ratio_median = median(.data[[ratio_col]], na.rm = TRUE),
          ratio_mean = mean(.data[[ratio_col]], na.rm = TRUE),
          ratio_sd = sd(.data[[ratio_col]], na.rm = TRUE),
          ratio_q05 = quantile(.data[[ratio_col]], 0.05, na.rm = TRUE),
          ratio_q25 = quantile(.data[[ratio_col]], 0.25, na.rm = TRUE),
          ratio_q75 = quantile(.data[[ratio_col]], 0.75, na.rm = TRUE),
          ratio_q95 = quantile(.data[[ratio_col]], 0.95, na.rm = TRUE),
          n_valid = sum(!is.na(.data[[ratio_col]])),
          prop_below_075 = mean(.data[[ratio_col]] < 0.75, na.rm = TRUE),
          prop_below_040 = mean(.data[[ratio_col]] < 0.40, na.rm = TRUE),
          prop_below_020 = mean(.data[[ratio_col]] < 0.20, na.rm = TRUE),
          prop_below_054 = if (metric %in% WHALE_METRICS) {
            mean(.data[[ratio_col]] < 0.54, na.rm = TRUE)
          } else { NA_real_ },
          .groups = "drop"
        )
      summary_rows[[length(summary_rows) + 1]] <- decade_summaries
    }
    all_summaries[[comp]] <- bind_rows(summary_rows)
    cat(sprintf("    %s: %d records\n", comp, nrow(all_summaries[[comp]])))
  }
  return(all_summaries)
}

summarize_structural_deviations <- function(paired_data, empirical_thresholds) {
  cat("  Summarizing structural deviations...\n")
  diff_suffix <- c(exploit = "_exploit_diff", absolute = "_absolute_diff",
                   climate = "_climate_diff")
  # For envelope columns: fishing values for exploit/absolute, climate values for climate
  envelope_prefix <- c(exploit = "", absolute = "", climate = "_clim")
  all_summaries <- list()

  for (comp in c("exploit", "absolute", "climate")) {
    summary_rows <- list()
    for (metric in STRUCTURAL_METRICS) {
      diff_col <- paste0(metric, diff_suffix[comp])
      fish_col <- paste0(metric, "_fish")
      if (!(diff_col %in% names(paired_data))) next

      th <- empirical_thresholds[[metric]]
      b0_median <- if (!is.null(th)) th$median else NA
      b0_sd <- if (!is.null(th)) th$sd else NA

      # For climate comparison, use climate-only envelope columns
      env_suffix <- envelope_prefix[comp]
      outside_90_col <- paste0(metric, env_suffix, "_outside_b0_90")
      outside_98_col <- paste0(metric, env_suffix, "_outside_b0_98")
      below_q05_col <- paste0(metric, "_below_b0_q05")
      above_q95_col <- paste0(metric, "_above_b0_q95")

      decade_summaries <- paired_data %>%
        group_by(decade, start_year, end_year) %>%
        summarise(
          metric_name = metric, metric_category = "structural", comparison = comp,
          n_sims = n(),
          diff_median = median(.data[[diff_col]], na.rm = TRUE),
          diff_mean = mean(.data[[diff_col]], na.rm = TRUE),
          diff_sd = sd(.data[[diff_col]], na.rm = TRUE),
          diff_q05 = quantile(.data[[diff_col]], 0.05, na.rm = TRUE),
          diff_q25 = quantile(.data[[diff_col]], 0.25, na.rm = TRUE),
          diff_q75 = quantile(.data[[diff_col]], 0.75, na.rm = TRUE),
          diff_q95 = quantile(.data[[diff_col]], 0.95, na.rm = TRUE),
          z_median = if (!is.na(b0_sd) && b0_sd > 0) {
            median(.data[[diff_col]], na.rm = TRUE) / b0_sd
          } else NA_real_,
          value_median = if (fish_col %in% names(paired_data)) {
            median(.data[[fish_col]], na.rm = TRUE)
          } else NA_real_,
          value_q25 = if (fish_col %in% names(paired_data)) {
            quantile(.data[[fish_col]], 0.25, na.rm = TRUE)
          } else NA_real_,
          value_q75 = if (fish_col %in% names(paired_data)) {
            quantile(.data[[fish_col]], 0.75, na.rm = TRUE)
          } else NA_real_,
          n_valid = sum(!is.na(.data[[diff_col]])),
          prop_outside_b0_90 = if (outside_90_col %in% names(paired_data)) {
            mean(.data[[outside_90_col]], na.rm = TRUE)
          } else NA_real_,
          prop_outside_b0_98 = if (outside_98_col %in% names(paired_data)) {
            mean(.data[[outside_98_col]], na.rm = TRUE)
          } else NA_real_,
          prop_below_b0_q05 = if (below_q05_col %in% names(paired_data) && comp != "climate") {
            mean(.data[[below_q05_col]], na.rm = TRUE)
          } else NA_real_,
          prop_above_b0_q95 = if (above_q95_col %in% names(paired_data) && comp != "climate") {
            mean(.data[[above_q95_col]], na.rm = TRUE)
          } else NA_real_,
          b0_median = b0_median,
          b0_sd = b0_sd,
          .groups = "drop"
        )
      summary_rows[[length(summary_rows) + 1]] <- decade_summaries
    }
    all_summaries[[comp]] <- bind_rows(summary_rows)
    cat(sprintf("    %s: %d records\n", comp, nrow(all_summaries[[comp]])))
  }
  return(all_summaries)
}

summarize_exploitation <- function(paired_data) {
  cat("  Summarizing exploitation rates...\n")
  exploit_summaries <- list()
  for (metric in EXPLOITATION_METRICS) {
    fish_col <- paste0(metric, "_fish")
    if (!(fish_col %in% names(paired_data))) next
    decade_summaries <- paired_data %>%
      group_by(decade, start_year, end_year) %>%
      summarise(
        metric_name = metric, comparison = "absolute_F", n_sims = n(),
        value_median = median(.data[[fish_col]], na.rm = TRUE),
        value_mean = mean(.data[[fish_col]], na.rm = TRUE),
        value_sd = sd(.data[[fish_col]], na.rm = TRUE),
        value_q05 = quantile(.data[[fish_col]], 0.05, na.rm = TRUE),
        value_q25 = quantile(.data[[fish_col]], 0.25, na.rm = TRUE),
        value_q75 = quantile(.data[[fish_col]], 0.75, na.rm = TRUE),
        value_q95 = quantile(.data[[fish_col]], 0.95, na.rm = TRUE),
        n_valid = sum(!is.na(.data[[fish_col]])),
        .groups = "drop"
      )
    exploit_summaries[[length(exploit_summaries) + 1]] <- decade_summaries
  }
  return(bind_rows(exploit_summaries))
}

###############################################################################
# Pre-Whaling Validation
###############################################################################

validate_pre_exploitation <- function(biomass_summaries, structural_summaries,
                                      tolerance = 0.05) {
  cat("\n=============================================================\n")
  cat("PRE-EXPLOITATION VALIDATION\n")
  cat("=============================================================\n\n")
  cat("Category A (Biomass) - exploitation ratios should be ~1.0:\n")
  pre_bio <- biomass_summaries$exploit %>% filter(start_year <= 1890)
  if (nrow(pre_bio) > 0) {
    pre_bio <- pre_bio %>% mutate(deviation = abs(ratio_median - 1.0),
                                   pass = deviation <= tolerance)
    for (i in seq_len(nrow(pre_bio))) {
      row <- pre_bio[i, ]
      cat(sprintf("  [%s] %s | %-25s: ratio = %.4f (dev = %.4f)\n",
                  ifelse(row$pass, "PASS", "FAIL"), row$decade, row$metric_name,
                  row$ratio_median, row$deviation))
    }
    cat(sprintf("  Biomass: %d/%d passed\n\n", sum(pre_bio$pass), nrow(pre_bio)))
  }
  cat("Category B (Structural) - exploitation differences should be ~0:\n")
  pre_str <- structural_summaries$exploit %>% filter(start_year <= 1890)
  if (nrow(pre_str) > 0) {
    pre_str <- pre_str %>%
      mutate(z_abs = abs(z_median), pass = !is.na(z_abs) & z_abs <= 0.2)
    for (i in seq_len(nrow(pre_str))) {
      row <- pre_str[i, ]
      cat(sprintf("  [%s] %s | %-25s: diff = %+.4f (z = %.3f)\n",
                  ifelse(row$pass, "PASS", "FAIL"), row$decade, row$metric_name,
                  row$diff_median, row$z_abs))
    }
    cat(sprintf("  Structural: %d/%d passed\n", sum(pre_str$pass, na.rm = TRUE), nrow(pre_str)))
  }
  return(list(biomass = pre_bio, structural = pre_str))
}

###############################################################################
# Heatmap Display Configuration
###############################################################################

METRIC_DISPLAY <- list(
  total_biomass        = list(name = "Total Biomass",           group = "Biomass", order = 1),
  baleen_biomass       = list(name = "Baleen Whale Biomass",    group = "Biomass", order = 2),
  whale_biomass        = list(name = "All Whale Biomass",       group = "Biomass", order = 3),
  seal_biomass         = list(name = "Seal Biomass",            group = "Biomass", order = 4),
  fish_biomass         = list(name = "Fish Biomass",            group = "Biomass", order = 5),
  krill_biomass        = list(name = "Krill Biomass",           group = "Biomass", order = 6),
  ltl_biomass          = list(name = "LTL Biomass",             group = "Biomass", order = 7),
  apex_biomass         = list(name = "Apex Predator Biomass",   group = "Biomass", order = 8),
  spectrum_slope       = list(name = "Size Spectrum Slope",     group = "Structural", order = 9),
  spectrum_intercept   = list(name = "Size Spectrum Intercept", group = "Structural", order = 10),
  mean_tl              = list(name = "Mean Trophic Level",      group = "Structural", order = 11),
  htl_indicator        = list(name = "High TL Indicator",       group = "Structural", order = 12),
  large_fish_indicator = list(name = "Large Fish Indicator",    group = "Structural", order = 13),
  mean_weight          = list(name = "Mean Individual Weight",  group = "Structural", order = 14),
  mean_max_weight      = list(name = "Mean Max Weight",         group = "Structural", order = 15),
  predator_prey_ratio  = list(name = "Predator-Prey Ratio",    group = "Structural", order = 16),
  consumer_ltl_ratio   = list(name = "Consumer:LTL Ratio",     group = "Structural", order = 17)
)

METRIC_ORDER <- names(sort(sapply(METRIC_DISPLAY, function(m) m$order)))
METRIC_ORDER_NAMES <- sapply(METRIC_ORDER, function(m) METRIC_DISPLAY[[m]]$name)

get_display_name <- function(m) {
  if (m %in% names(METRIC_DISPLAY)) METRIC_DISPLAY[[m]]$name else m
}

###############################################################################
# Heatmap: Biomass (Category A)
###############################################################################

plot_biomass_ratio_heatmap <- function(ratio_summary, title, subtitle,
                                       output_path, filename,
                                       metrics_filter = NULL) {
  plot_data <- ratio_summary
  if (!is.null(metrics_filter))
    plot_data <- plot_data %>% filter(metric_name %in% metrics_filter)
  plot_data <- plot_data %>% mutate(display_name = sapply(metric_name, get_display_name))
  bio_names <- sapply(BIOMASS_METRICS, get_display_name)
  decade_order <- unique(plot_data$decade[order(plot_data$start_year)])
  plot_data$decade <- factor(plot_data$decade, levels = decade_order)
  plot_data$display_name <- factor(plot_data$display_name, levels = rev(bio_names))
  plot_data$ratio_clamped <- pmin(pmax(plot_data$ratio_median, 0), 2.5)

  p <- ggplot(plot_data, aes(x = decade, y = display_name, fill = ratio_clamped)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.2f", ratio_median)), size = 2.5, color = "black") +
    scale_fill_gradientn(
      colours = c("#d73027", "#fc8d59", "#fee08b", "#ffffbf", "#d9ef8b", "#91cf60", "#1a9850"),
      values = scales::rescale(c(0, 0.4, 0.75, 1.0, 1.25, 1.75, 2.5)),
      limits = c(0, 2.5), na.value = "grey80", name = "Median\nRatio") +
    labs(title = title, subtitle = subtitle, x = "Decade", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          legend.position = "right", panel.grid = element_blank())

  ggsave(file.path(output_path, paste0(filename, ".png")), p, width = 15, height = 7, dpi = 300)
  ggsave(file.path(output_path, paste0(filename, ".pdf")), p, width = 15, height = 7)
  return(p)
}

plot_biomass_proportion_heatmap <- function(ratio_summary, prop_col, threshold_val,
                                            threshold_label, title,
                                            output_path, filename,
                                            metrics_filter = NULL) {
  plot_data <- ratio_summary
  if (!is.null(metrics_filter))
    plot_data <- plot_data %>% filter(metric_name %in% metrics_filter)
  plot_data <- plot_data %>% filter(!is.na(.data[[prop_col]]))
  if (nrow(plot_data) == 0) { cat(sprintf("  Skipping %s\n", filename)); return(NULL) }
  plot_data <- plot_data %>%
    mutate(display_name = sapply(metric_name, get_display_name),
           prop_value = .data[[prop_col]])
  bio_names <- sapply(BIOMASS_METRICS, get_display_name)
  valid_names <- bio_names[bio_names %in% plot_data$display_name]
  decade_order <- unique(plot_data$decade[order(plot_data$start_year)])
  plot_data$decade <- factor(plot_data$decade, levels = decade_order)
  plot_data$display_name <- factor(plot_data$display_name, levels = rev(valid_names))

  p <- ggplot(plot_data, aes(x = decade, y = display_name, fill = prop_value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", prop_value * 100)), size = 2.5, color = "black") +
    scale_fill_gradientn(
      colours = c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027"),
      values = c(0, 0.1, 0.25, 0.5, 0.75, 1),
      limits = c(0, 1), na.value = "grey80",
      name = sprintf("P(ratio\n< %.2f)", threshold_val), labels = scales::percent) +
    labs(title = title,
         subtitle = sprintf("P(ratio < %.2f) \u2014 %s", threshold_val, threshold_label),
         x = "Decade", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 9.5, hjust = 0.5),
          legend.position = "right", panel.grid = element_blank())

  ggsave(file.path(output_path, paste0(filename, ".png")), p, width = 15, height = 7, dpi = 300)
  ggsave(file.path(output_path, paste0(filename, ".pdf")), p, width = 15, height = 7)
  return(p)
}

###############################################################################
# Heatmap: Structural Deviations (Category B)
###############################################################################

plot_structural_deviation_heatmap <- function(structural_summary,
                                              envelope_col, envelope_label,
                                              title, output_path, filename) {
  # Include all metrics, even if some decades have NA (will show as grey)
  plot_data <- structural_summary %>%
    mutate(display_name = sapply(metric_name, get_display_name),
           prop_value = .data[[envelope_col]])
  
  # Only skip if NO valid data for ANY metric
  if (all(is.na(plot_data$prop_value))) {
    cat(sprintf("  Skipping %s (no valid data)\n", filename)); 
    return(NULL)
  }
  
  str_names <- sapply(STRUCTURAL_METRICS, get_display_name)
  valid_names <- str_names[str_names %in% plot_data$display_name]
  decade_order <- unique(plot_data$decade[order(plot_data$start_year)])
  plot_data$decade <- factor(plot_data$decade, levels = decade_order)
  plot_data$display_name <- factor(plot_data$display_name, levels = rev(valid_names))
  if (nrow(plot_data) == 0) { cat(sprintf("  Skipping %s\n", filename)); return(NULL) }

  p <- ggplot(plot_data, aes(x = decade, y = display_name, fill = prop_value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", prop_value * 100)), size = 2.5, color = "black") +
    scale_fill_gradientn(
      colours = c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027"),
      values = c(0, 0.1, 0.25, 0.5, 0.75, 1),
      limits = c(0, 1), na.value = "grey80",
      name = "P(outside\nB0 range)", labels = scales::percent) +
    labs(title = title,
         subtitle = sprintf("Proportion of simulations outside B0 %s", envelope_label),
         x = "Decade", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 9.5, hjust = 0.5),
          legend.position = "right", panel.grid = element_blank())

  ggsave(file.path(output_path, paste0(filename, ".png")), p, width = 15, height = 8, dpi = 300)
  ggsave(file.path(output_path, paste0(filename, ".pdf")), p, width = 15, height = 8)
  return(p)
}

plot_structural_zscore_heatmap <- function(structural_summary, title,
                                           output_path, filename) {
  # Include all metrics, show NA as grey (happens when SD ~ 0)
  plot_data <- structural_summary %>%
    mutate(display_name = sapply(metric_name, get_display_name),
           z_clamped = ifelse(is.na(z_median), NA, pmin(pmax(z_median, -5), 5)))
  
  # Only skip if NO metrics at all
  if (nrow(plot_data) == 0) {
    cat(sprintf("  Skipping %s (no data)\n", filename)); 
    return(NULL)
  }
  
  str_names <- sapply(STRUCTURAL_METRICS, get_display_name)
  valid_names <- str_names[str_names %in% plot_data$display_name]
  decade_order <- unique(plot_data$decade[order(plot_data$start_year)])
  plot_data$decade <- factor(plot_data$decade, levels = decade_order)
  plot_data$display_name <- factor(plot_data$display_name, levels = rev(valid_names))

  p <- ggplot(plot_data, aes(x = decade, y = display_name, fill = z_clamped)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%+.2f", z_median)), size = 2.5, color = "black") +
    scale_fill_gradientn(
      colours = c("#2166ac", "#67a9cf", "#d1e5f0", "#f7f7f7",
                  "#fddbc7", "#ef8a62", "#b2182b"),
      values = scales::rescale(c(-5, -2, -1, 0, 1, 2, 5)),
      limits = c(-5, 5), na.value = "grey80",
      name = "z-score") +
    labs(title = title,
         subtitle = "Median standardized departure: (value - B0 median) / B0 SD",
         x = "Decade", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          legend.position = "right", panel.grid = element_blank())

  ggsave(file.path(output_path, paste0(filename, ".png")), p, width = 15, height = 8, dpi = 300)
  ggsave(file.path(output_path, paste0(filename, ".pdf")), p, width = 15, height = 8)
  return(p)
}

###############################################################################
# Combined Heatmap with Horizontal Divider
###############################################################################

plot_combined_heatmap <- function(biomass_summary, structural_summary,
                                  title, output_path, filename) {
  bio_data <- biomass_summary %>%
    filter(!is.na(prop_below_075)) %>%
    mutate(display_name = sapply(metric_name, get_display_name),
           prop_value = prop_below_075,
           category = "Biomass Depletion: P(ratio < 0.75)")
  str_data <- structural_summary %>%
    filter(!is.na(prop_outside_b0_90)) %>%
    mutate(display_name = sapply(metric_name, get_display_name),
           prop_value = prop_outside_b0_90,
           category = "Structural Deviation: P(outside B0 5th\u201395th)")
  bio_names <- sapply(BIOMASS_METRICS, get_display_name)
  str_names <- sapply(STRUCTURAL_METRICS, get_display_name)
  all_names <- c(bio_names, str_names)
  plot_data <- bind_rows(bio_data, str_data) %>%
    select(decade, start_year, display_name, prop_value, category)
  decade_order <- unique(plot_data$decade[order(plot_data$start_year)])
  plot_data$decade <- factor(plot_data$decade, levels = decade_order)
  plot_data$display_name <- factor(plot_data$display_name, levels = rev(all_names))
  plot_data$category <- factor(plot_data$category,
    levels = c("Biomass Depletion: P(ratio < 0.75)",
               "Structural Deviation: P(outside B0 5th\u201395th)"))

  p <- ggplot(plot_data, aes(x = decade, y = display_name, fill = prop_value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", prop_value * 100)), size = 2.3, color = "black") +
    facet_grid(category ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_fill_gradientn(
      colours = c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027"),
      values = c(0, 0.1, 0.25, 0.5, 0.75, 1),
      limits = c(0, 1), na.value = "grey80",
      name = "Proportion\naltered", labels = scales::percent) +
    labs(title = title,
         subtitle = paste0(
           "Top: P(biomass ratio < 0.75, CCAMLR \u03B3\u2082; Constable et al. 2000)\n",
           "Bottom: P(structural metric outside B0 5th\u201395th percentile, empirical envelope)"),
         x = "Decade", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = element_text(size = 9),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 9, hjust = 0.5),
          strip.text.y.left = element_text(angle = 0, face = "bold", size = 9, hjust = 1),
          strip.placement = "outside",
          legend.position = "right", panel.grid = element_blank(),
          panel.spacing = unit(0.8, "lines"))

  ggsave(file.path(output_path, paste0(filename, ".png")), p, width = 16, height = 12, dpi = 300)
  ggsave(file.path(output_path, paste0(filename, ".pdf")), p, width = 16, height = 12)
  return(p)
}

###############################################################################
# Exploitation Rate Heatmap (Category C)
###############################################################################

plot_exploitation_heatmap <- function(exploit_summary, output_path) {
  exploit_display <- list(
    exploitation_total = list(name = "Total Exploitation Rate"),
    exploitation_whale = list(name = "Whale Exploitation Rate"),
    exploitation_krill = list(name = "Krill Exploitation Rate")
  )
  plot_data <- exploit_summary %>%
    mutate(display_name = sapply(metric_name, function(m) {
      if (m %in% names(exploit_display)) exploit_display[[m]]$name else m }))
  decade_order <- unique(plot_data$decade[order(plot_data$start_year)])
  plot_data$decade <- factor(plot_data$decade, levels = decade_order)
  name_order <- sapply(names(exploit_display), function(m) exploit_display[[m]]$name)
  plot_data$display_name <- factor(plot_data$display_name, levels = rev(name_order))

  p <- ggplot(plot_data, aes(x = decade, y = display_name, fill = value_median)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.3f", value_median)), size = 2.8, color = "black") +
    scale_fill_gradientn(
      colours = c("#1a9850", "#91cf60", "#fee08b", "#fc8d59", "#d73027"),
      values = c(0, 0.05, 0.2, 0.4, 1),
      limits = c(0, max(0.5, max(plot_data$value_median, na.rm = TRUE))),
      na.value = "grey80", name = "Median F") +
    labs(title = "Exploitation Rates (Fishing Scenario)",
         subtitle = "Median fishing mortality rates across ensemble",
         x = "Decade", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5),
          legend.position = "right", panel.grid = element_blank())

  ggsave(file.path(output_path, "exploitation_heatmap.png"), p, width = 15, height = 5, dpi = 300)
  ggsave(file.path(output_path, "exploitation_heatmap.pdf"), p, width = 15, height = 5)
  return(p)
}

###############################################################################
# Main Execution
###############################################################################

main_ecosystem_assessment <- function(max_sims = NULL) {
  cat("=============================================================\n")
  cat("PRYDZ BAY ECOSYSTEM ASSESSMENT (v2 - Paired Ensemble)\n")
  cat(sprintf("B0 Reference Period: %d-%d (ISIMIP climate norm)\n", B0_PERIOD[1], B0_PERIOD[2]))
  cat("=============================================================\n\n")
  start_time <- Sys.time()

  # 1. Process paired ensemble
  ensemble_data <- process_paired_ensemble(
    ENSEMBLE_PATHS$fishing, ENSEMBLE_PATHS$climate_only, max_sims = max_sims)
  fishing_raw <- ensemble_data$fishing_raw
  climate_raw <- ensemble_data$climate_raw
  b0_reference <- ensemble_data$b0_reference

  # 2. Derive empirical thresholds
  cat("\n=============================================================\n")
  cat("DERIVING EMPIRICAL THRESHOLDS\n")
  cat("=============================================================\n")
  empirical_thresholds <- derive_empirical_thresholds(b0_reference)

  # 3. Compute paired comparisons
  paired_data <- compute_paired_comparisons(
    fishing_raw, climate_raw, b0_reference, empirical_thresholds)

  # 4. Summarize by category
  cat("\nSummarizing results by metric category...\n")
  biomass_summaries <- summarize_biomass_ratios(paired_data)
  structural_summaries <- summarize_structural_deviations(paired_data, empirical_thresholds)
  exploitation_summary <- summarize_exploitation(paired_data)

  # 5. Pre-whaling validation
  validation <- validate_pre_exploitation(biomass_summaries, structural_summaries)

  # 6. Generate visualizations
  cat("\n=============================================================\n")
  cat("GENERATING VISUALIZATIONS\n")
  cat("=============================================================\n\n")
  plots <- list()

  # 6a. Biomass ratio heatmaps
  cat("  Category A: Biomass ratio heatmaps...\n")
  plots$bio_exploit_ratio <- plot_biomass_ratio_heatmap(
    biomass_summaries$exploit, "Exploitation Impact: Biomass Metrics",
    "Median ratio: Fishing / Climate-only (paired)",
    OUTPUT_DIR, "heatmap_biomass_exploitation_ratio")
  plots$bio_absolute_ratio <- plot_biomass_ratio_heatmap(
    biomass_summaries$absolute, "Absolute Health: Biomass Metrics",
    sprintf("Median ratio: Fishing / B0 (%d-%d)", B0_PERIOD[1], B0_PERIOD[2]),
    OUTPUT_DIR, "heatmap_biomass_absolute_ratio")
  plots$bio_climate_ratio <- plot_biomass_ratio_heatmap(
    biomass_summaries$climate, "Climate Impact: Biomass Metrics",
    sprintf("Median ratio: Climate-only / B0 (%d-%d)", B0_PERIOD[1], B0_PERIOD[2]),
    OUTPUT_DIR, "heatmap_biomass_climate_ratio")

  # Biomass proportion heatmaps
  cat("  Category A: Biomass proportion heatmaps...\n")
  plots$bio_prop_075 <- plot_biomass_proportion_heatmap(
    biomass_summaries$absolute, "prop_below_075", 0.75,
    "CCAMLR \u03B3\u2082 escapement; Constable et al. 2000",
    "Absolute Health: P(Biomass/B0 < 0.75)",
    OUTPUT_DIR, "heatmap_biomass_prop_below_075")
  plots$bio_prop_040 <- plot_biomass_proportion_heatmap(
    biomass_summaries$absolute, "prop_below_040", 0.40,
    "BMSY proxy; Restrepo et al. 1998",
    "Absolute Health: P(Biomass/B0 < 0.40)",
    OUTPUT_DIR, "heatmap_biomass_prop_below_040")
  plots$bio_prop_020 <- plot_biomass_proportion_heatmap(
    biomass_summaries$absolute, "prop_below_020", 0.20,
    "CCAMLR \u03B3\u2081 / MSST collapse; Constable et al. 2000",
    "Absolute Health: P(Biomass/B0 < 0.20)",
    OUTPUT_DIR, "heatmap_biomass_prop_below_020")
  plots$whale_prop_054 <- plot_biomass_proportion_heatmap(
    biomass_summaries$absolute, "prop_below_054", 0.54,
    "IWC RMP protection level; IWC 1994, Punt & Donovan 2007",
    "Whale Populations: P(Biomass/B0 < 0.54)",
    OUTPUT_DIR, "heatmap_whale_prop_below_054", metrics_filter = WHALE_METRICS)
  plots$whale_exploit_054 <- plot_biomass_proportion_heatmap(
    biomass_summaries$exploit, "prop_below_054", 0.54,
    "IWC RMP protection level; IWC 1994, Punt & Donovan 2007",
    "Exploitation Impact on Whales: P(Fishing/Climate-only < 0.54)",
    OUTPUT_DIR, "heatmap_whale_exploit_prop_below_054", metrics_filter = WHALE_METRICS)

  # 6b. Structural deviation heatmaps
  cat("  Category B: Structural deviation heatmaps...\n")
  plots$str_outside_90 <- plot_structural_deviation_heatmap(
    structural_summaries$absolute, "prop_outside_b0_90",
    "5th\u201395th percentile (empirical B0 envelope, 1841\u20131860)",
    "Structural Metrics: Outside B0 Natural Range",
    OUTPUT_DIR, "heatmap_structural_outside_b0_90")
  plots$str_outside_98 <- plot_structural_deviation_heatmap(
    structural_summaries$absolute, "prop_outside_b0_98",
    "1st\u201399th percentile (substantially altered)",
    "Structural Metrics: Substantially Altered",
    OUTPUT_DIR, "heatmap_structural_outside_b0_98")
  plots$str_zscore_abs <- plot_structural_zscore_heatmap(
    structural_summaries$absolute,
    "Structural Metrics: Standardized Departure from B0",
    OUTPUT_DIR, "heatmap_structural_zscore_absolute")
  plots$str_zscore_exploit <- plot_structural_zscore_heatmap(
    structural_summaries$exploit,
    "Structural Metrics: Exploitation-Driven Departure",
    OUTPUT_DIR, "heatmap_structural_zscore_exploitation")

  # 6c. Combined heatmaps
  cat("  Combined heatmaps (biomass + structural)...\n")
  plots$combined_absolute <- plot_combined_heatmap(
    biomass_summaries$absolute, structural_summaries$absolute,
    "Ecosystem Assessment: Prydz Bay (Absolute)",
    OUTPUT_DIR, "heatmap_combined_absolute")
  plots$combined_exploit <- plot_combined_heatmap(
    biomass_summaries$exploit, structural_summaries$exploit,
    "Ecosystem Assessment: Exploitation Impact",
    OUTPUT_DIR, "heatmap_combined_exploitation")
  plots$combined_climate <- plot_combined_heatmap(
    biomass_summaries$climate, structural_summaries$climate,
    "Ecosystem Assessment: Climate Impact",
    OUTPUT_DIR, "heatmap_combined_climate")

  # 6d. Exploitation heatmap
  cat("  Category C: Exploitation rate heatmap...\n")
  plots$exploitation_F <- plot_exploitation_heatmap(exploitation_summary, OUTPUT_DIR)

  # 7. Save outputs
  cat("\n=============================================================\n")
  cat("SAVING OUTPUTS\n")
  cat("=============================================================\n\n")
  cat("Large data files (RDS) -> Output_large_files/ecosystem_assessment/\n")
  saveRDS(fishing_raw, file.path(OUTPUT_DIR_LARGE, "fishing_metrics_raw.rds"))
  saveRDS(climate_raw, file.path(OUTPUT_DIR_LARGE, "climate_only_metrics_raw.rds"))
  saveRDS(b0_reference, file.path(OUTPUT_DIR_LARGE, "b0_reference.rds"))
  saveRDS(paired_data, file.path(OUTPUT_DIR_LARGE, "paired_data.rds"))
  saveRDS(empirical_thresholds, file.path(OUTPUT_DIR_LARGE, "empirical_thresholds.rds"))
  cat("Summary csvs/plots -> ecosystem_assessment_outputs/\n")
  for (comp in c("exploit", "absolute", "climate")) {
    write.csv(biomass_summaries[[comp]],
              file.path(OUTPUT_DIR, sprintf("summary_biomass_%s.csv", comp)), row.names = FALSE)
    write.csv(structural_summaries[[comp]],
              file.path(OUTPUT_DIR, sprintf("summary_structural_%s.csv", comp)), row.names = FALSE)
  }
  write.csv(exploitation_summary,
            file.path(OUTPUT_DIR, "summary_exploitation_rates.csv"), row.names = FALSE)
  if (!is.null(validation)) {
    if (!is.null(validation$biomass))
      write.csv(validation$biomass, file.path(OUTPUT_DIR, "validation_biomass.csv"), row.names = FALSE)
    if (!is.null(validation$structural))
      write.csv(validation$structural, file.path(OUTPUT_DIR, "validation_structural.csv"), row.names = FALSE)
  }

  end_time <- Sys.time()
  cat(sprintf("\n=============================================================\n"))
  cat(sprintf("ASSESSMENT COMPLETE (%.1f minutes)\n", difftime(end_time, start_time, units = "mins")))
  cat("=============================================================\n\n")
  cat("Output files:\n")
  cat("  LARGE DATA FILES (Output_large_files/ecosystem_assessment/):\n")
  cat("    - fishing_metrics_raw.rds\n")
  cat("    - climate_only_metrics_raw.rds\n")
  cat("    - b0_reference.rds\n")
  cat("    - paired_data.rds\n")
  cat("    - empirical_thresholds.rds\n")
  cat("  SUMMARY CSVS (ecosystem_assessment_outputs/):\n")
  cat("    - summary_biomass_{exploit,absolute,climate}.csv\n")
  cat("    - summary_structural_{exploit,absolute,climate}.csv\n")
  cat("    - summary_exploitation_rates.csv\n")
  cat("    - validation_{biomass,structural}.csv\n")
  cat("  HEATMAPS (ecosystem_assessment_outputs/):\n")
  cat("    Category A (Biomass - ratio-based):\n")
  cat("      - heatmap_biomass_{exploitation,absolute,climate}_ratio\n")
  cat("      - heatmap_biomass_prop_below_{075,040,020}\n")
  cat("      - heatmap_whale_{prop,exploit}_below_054\n")
  cat("    Category B (Structural - empirical deviation):\n")
  cat("      - heatmap_structural_outside_b0_{90,98}\n")
  cat("      - heatmap_structural_zscore_{absolute,exploitation}\n")
  cat("    Combined (A + B):\n")
  cat("      - heatmap_combined_{absolute,exploitation,climate}\n")
  cat("    Category C (Exploitation):\n")
  cat("      - exploitation_heatmap\n")

  return(list(
    fishing_raw = fishing_raw, climate_raw = climate_raw,
    b0_reference = b0_reference, paired_data = paired_data,
    empirical_thresholds = empirical_thresholds,
    biomass_summaries = biomass_summaries,
    structural_summaries = structural_summaries,
    exploitation_summary = exploitation_summary,
    validation = validation, plots = plots
  ))
}
