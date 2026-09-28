# =============================================================================
# prydz_whale_validation_points.R
#
# Validation points for the Prydz Bay size-spectrum (mizer) whaling runs,
# built only from published estimates
# -----------------------------------------------------------------------------
# Every point is a status value that a published study reports for a stated
# year: abundance as a percentage of the pre-whaling level (for minke whales,
# carrying capacity relative to 1930; see Section 2). Nothing is estimated
# here. The only operations applied to a published value are
#   (1) the change to the model's axis: % change = status - 100,
#   (2) for values reported with an SE only, error bars of +/- se_multiplier x SE,
#   (3) for a reported range with no central value (southern right whales,
#       20-25%), a point at the midpoint with the reported bounds as the error
#       bar (flagged in the point_is_midpoint column).
#
# Selection rules
#   1. Published status values only. Section 2 ends with the published values
#      that were considered but not used, and why.
#   2. Regional first. For each species, only the most regionally specific
#      estimates in the chosen time window are kept (Prydz Bay / Indian-sector
#      stock > Southern Hemisphere > global). Broader values are used only
#      where nothing more specific exists; they are flagged as proxies and
#      carry a caveat.
#   3. Time. Estimates for 2010 and earlier by default; include_2011_2020 = TRUE
#      adds 2011-2020. Nothing after 2020 is used.
#
# Outputs (in out_dir)
#   validation_points.csv    points to plot, one row per published value
#   excluded_estimates.csv   published values removed by rules 2-3, with reason
#   trend_checks.csv         published rates of change for the same stocks
#   refs.csv                 full citations for the source keys
#   validation_points_preview.png
#
# Requires base R; ggplot2 is needed only for the preview and overlay helper.
# =============================================================================


## ---- 0. Settings ------------------------------------------------------------------
include_2011_2020 <- FALSE  # TRUE adds published estimates for 2011-2020
regional_first    <- TRUE   # rule 2; FALSE keeps broader-scale values alongside regional ones
include_proxies   <- TRUE   # FALSE drops Southern Hemisphere and global proxies entirely
se_multiplier     <- 1      # error-bar half-width for SE-only values (1 = +/- 1 SE)
pct_scale         <- 1      # 1 = percent (-85); 0.01 = proportion (-0.85)
write_outputs     <- TRUE
out_dir           <- "validation_outputs"

# Facet labels: edit to match the model output exactly
LB <- "Large baleen whales"
SP <- "Sperm whales"
MI <- "Minke whales"


## ---- 1. Record constructors -------------------------------------------------------
# status, status_lo, status_hi: % of the baseline, exactly as reported.
# A reported range with no central value (e.g. "20-25%") has status = NA.
est <- function(group, species_key, label, year, status = NA_real_,
                status_lo = NA_real_, status_hi = NA_real_, se = NA_real_,
                interval_type = "None reported", qualifier = "",
                baseline, region, scale, source, reported, caveat) {
  data.frame(group = group, species_key = species_key, label = label, year = year,
             status = status, status_lo = status_lo, status_hi = status_hi, se = se,
             interval_type = interval_type, qualifier = qualifier, baseline = baseline,
             region = region, scale = scale, source = source, reported = reported,
             caveat = caveat, stringsAsFactors = FALSE)
}

# Published rates of change (instantaneous, %/yr) over a window of years
rate <- function(group, species, region, window_start, window_end, rate_pct_yr,
                 lo = NA_real_, hi = NA_real_, se = NA_real_, interval_type, source) {
  data.frame(group = group, species = species, region = region,
             window_start = window_start, window_end = window_end,
             rate_pct_yr = rate_pct_yr, lo = lo, hi = hi, se = se,
             interval_type = interval_type, source = source, stringsAsFactors = FALSE)
}


## ---- 2. Published status values ----------------------------------------------------
# scale: "regional" = an assessment unit that contains Prydz Bay (IWC Areas
# IV/V, the breeding stock feeding in Area IV, or the Indian-sector minke
# stock); "Southern Hemisphere" = circumpolar or SH-wide; "global".
# Seasons are assigned to the January year (1997/98 -> 1998).

estimates <- rbind(

  # Large baleen whales ------------------------------------------------------------
  est(LB, "blue", "Antarctic blue (Areas IV+V)", 2009, 0.8, 0.3, 1.9,
      interval_type = "95% CI", baseline = "Prewhaling abundance",
      region = "Areas IV+V (70E-170W), south of 60S", scale = "regional",
      source = "Hamabe2023",
      reported = "Abstract: 476 whales (95% CI 242-972) in 2009, 0.8% (0.3-1.9%) of prewhaling levels",
      caveat = "Assessment unit spans 70E-170W, with Prydz Bay at its western edge"),
  est(LB, "blue", "Antarctic blue, circumpolar", 1996, 1, qualifier = "about",
      baseline = "Pre-exploitation abundance (239,000; 202,000-311,000)",
      region = "Circumpolar", scale = "Southern Hemisphere",
      source = "Branch2004 (as quoted in MH2014)",
      reported = "About 1% (1,700 whales; 95% interval 860-2,900) in 1996; interval given for N only",
      caveat = "Circumpolar"),
  est(LB, "blue", "Antarctic blue, circumpolar", 1998, NA, 0.7, 1.0, qualifier = "less than",
      interval_type = "95% PI (reported as less than 1%)", baseline = "Pre-exploitation numbers",
      region = "Circumpolar, south of 60S", scale = "Southern Hemisphere",
      source = "Bamford (N from Branch2007)",
      reported = "2,280 whales (CV 0.36) in 1998, less than 1% (95% PI 0.7-1%) of pre-exploitation",
      caveat = "Circumpolar"),
  est(LB, "blue", "Antarctic blue, circumpolar", 2018, 2, qualifier = "less than",
      interval_type = "Upper bound", baseline = "Pre-whaling abundance",
      region = "Antarctic (circumpolar)", scale = "Southern Hemisphere", source = "Cooke2018",
      reported = "IUCN supplementary assessment: less than 2% of pre-whaling abundance",
      caveat = "Circumpolar; described by the IUCN document as a crude indication"),
  est(LB, "humpback", "Humpback (Breeding Stock D)", 2012, 90, 74, 98,
      interval_type = "90% PI", baseline = "Pre-exploitation abundance",
      region = "Breeding Stock D (western Australia; feeds in Area IV)", scale = "regional",
      source = "IWC2015 (as reported in Bejder2016)",
      reported = "90% (74-98%; 90% PI) in 2012; 19,200 whales (17,553-24,012)",
      caveat = "Breeding-stock assessment; Stock D feeds in Area IV (70-130E)"),
  est(LB, "humpback", "Humpback, all SH stocks", 2015, 70, qualifier = "about",
      baseline = "Pre-exploitation abundance", region = "Southern Hemisphere, all breeding stocks",
      scale = "Southern Hemisphere", source = "Bamford (citing IWC2016)",
      reported = "About 70% of pre-exploitation across all breeding stocks in 2015",
      caveat = "Hemispheric"),
  est(LB, "southern_right", "Southern right", 2009, NA, 20, 25,
      interval_type = "Reported range", baseline = "Pre-exploitation abundance",
      region = "Circumpolar (Southern Hemisphere)", scale = "Southern Hemisphere",
      source = "Bamford (citing IWC2013SRW)",
      reported = "20-25% of pre-exploitation in 2009; combined abundance 11,984",
      caveat = "PROXY: no Indian-sector status has been published; circumpolar value"),
  est(LB, "fin", "Fin", 2018, 25, qualifier = "about",
      baseline = "Pre-exploitation level", region = "Global", scale = "global",
      source = "Cooke2018",
      reported = "IUCN supplementary assessment: fin and sei whales at about 25% of pre-exploitation levels",
      caveat = "PROXY: global crude indication; no Southern Hemisphere recovery level is available (Bamford)"),
  est(LB, "sei", "Sei", 2018, 25, qualifier = "about",
      baseline = "Pre-exploitation level", region = "Global", scale = "global",
      source = "Cooke2018",
      reported = "IUCN supplementary assessment: fin and sei whales at about 25% of pre-exploitation levels",
      caveat = "PROXY: global crude indication; no Southern Hemisphere status is available"),

  # Minke whales -------------------------------------------------------------------
  est(MI, "minke", "Antarctic minke I-stock (K ratio)", 1960, 162.1, se = 28,
      interval_type = "SE", baseline = "Carrying capacity in 1930 (unexploited equilibrium)",
      region = "I-stock (Areas IIIE, IV and V-W)", scale = "regional", source = "Punt2014",
      reported = "Reference case K1960/K1930 = 162.1% (Table 6a); SE 28% (text)",
      caveat = paste("Carrying capacity used as an abundance proxy: the stock is estimated to",
                     "stay close to K throughout (Punt2014); the cause of the change in K is not attributed")),
  est(MI, "minke", "Antarctic minke I-stock (K ratio)", 2000, 80, qualifier = "about",
      baseline = "Carrying capacity in 1930 (unexploited equilibrium)",
      region = "I-stock (Areas IIIE, IV and V-W)", scale = "regional", source = "Punt2014",
      reported = "Text: K in 2000 about 80% of K in 1930; no interval reported",
      caveat = "Carrying capacity used as an abundance proxy (see the 1960 row)"),

  # Sperm whales -------------------------------------------------------------------
  est(SP, "sperm", "Sperm", 1880, 71, 52, 100, interval_type = "95% CI",
      baseline = "Original (pre-whaling) level", region = "Global", scale = "global",
      source = "Whitehead2002",
      reported = "Abstract: about 71% (95% CI 52-100%) of original level in 1880, as open-boat whaling ended",
      caveat = paste("PROXY: global; reflects 19th-century open-boat whaling, which the model does not",
                     "impose; Antarctic sperm whales (mostly mature males) have no separate assessment")),
  est(SP, "sperm", "Sperm", 1999, 32, 19, 62, interval_type = "95% CI",
      baseline = "Original (pre-whaling) level", region = "Global", scale = "global",
      source = "Whitehead2002",
      reported = "Abstract: about 32% (95% CI 19-62%) of original level in 1999",
      caveat = paste("PROXY: global; the baseline predates the model's whaling period; Antarctic",
                     "sperm whales have no separate assessment"))
)

# Considered but not used (no published status value for a stated year):
#   - Survey abundance series (JARPA/JARPAII: HM2014b, MH2014; IDCR/SOWER:
#     Branch2007, Branch 2011) and breeding-ground estimates (e.g. Breeding
#     Stock D, Salgado Kent et al. 2012; southwest Australian right whales,
#     Smith et al. 2021). Expressing these as % of pre-whaling would need our
#     own K or anchoring.
#   - Whitehead & Shin (2022, Sci. Rep. 12: 19468) report K (1710) and N (2022)
#     but not their ratio; 2022 is also after 2020.
#   - Kasamatsu & Joyce (1995): sperm whales south of the Antarctic
#     Convergence, with no baseline.
#   - Reported minima without a year (e.g. fin whales at 1-2% of K; Bamford).
#   - IWC status summaries for southern right whales (~14,000 in 2009; K of
#     70,000-100,000): no status stated.
#   Our search found no published status for Indian-sector fin or sei whales,
#   or for Breeding Stock D humpbacks before 2012.


## ---- 3. Published rates of change (for trend comparison) --------------------------
rates <- rbind(
  rate(LB, "Humpback", "Area IV, south of 60S (JARPA/JARPAII)", 1990, 2008, 13.6,
       lo = 8.4, hi = 18.7, interval_type = "95% CI", source = "HM2014b"),
  rate(LB, "Antarctic blue", "Areas IIIE-VIW, south of 60S (JARPA/JARPAII)", 1996, 2009, 8.2,
       lo = 3.9, hi = 12.5, interval_type = "95% CI", source = "MH2014 (Table 5)"),
  rate(LB, "Fin (Indian Ocean stock)", "Areas IIIE+IV, south of 60S (JARPA/JARPAII)", 1996, 2008, 8.9,
       lo = -14.5, hi = 32.4, interval_type = "95% CI", source = "MH2014 (Table 5)"),
  rate(LB, "Southern right", "Area IV, south of 60S (JARPA/JARPAII)", 1990, 2008, 5.9,
       lo = -16.4, hi = 28.1, interval_type = "95% CI", source = "MH2014"),
  rate(MI, "Antarctic minke I-stock (1+)", "Areas IIIE, IV and V-W (SCAA reference case)", 1945, 1968,
       1.912, se = 0.7, interval_type = "SE", source = "Punt2014 (Table 6a; SE in text)"),
  rate(MI, "Antarctic minke I-stock (1+)", "Areas IIIE, IV and V-W (SCAA reference case)", 1968, 1988,
       -3.718, interval_type = "None reported", source = "Punt2014 (Table 6a)"),
  rate(MI, "Antarctic minke I-stock (1+)", "Areas IIIE, IV and V-W (SCAA reference case)", 1988, 2012,
       -0.168, se = 0.5, interval_type = "SE", source = "Punt2014 (Table 6a; SE in text)"),
  rate(MI, "Antarctic minke", "Areas IIIE+IV, south of 60S (JARPA/JARPAII)", 1990, 2008, 1.1,
       lo = -2.3, hi = 4.5, interval_type = "95% CI",
       source = "Murase2020 (citing Hakamada & Matsuoka 2014a)")
)


## ---- 4. Apply the selection rules --------------------------------------------------
scale_rank <- c("regional" = 1, "Southern Hemisphere" = 2, "global" = 3)
estimates$rank   <- unname(scale_rank[estimates$scale])
estimates$proxy  <- estimates$rank > 1
estimates$period <- ifelse(estimates$year <= 2010, "2010 and earlier",
                           ifelse(estimates$year <= 2020, "2011-2020", "after 2020"))

# Rule 3: time window
in_time <- estimates$year <= 2010 | (include_2011_2020 & estimates$year <= 2020)
estimates$reason <- ifelse(in_time, NA_character_,
                           ifelse(estimates$year > 2020, "After 2020",
                                  "2011-2020 (include_2011_2020 = FALSE)"))

# Rule 2: keep only the most regional scale available per species in the window
if (regional_first) {
  best    <- tapply(estimates$rank[in_time], estimates$species_key[in_time], min)
  broader <- which(in_time & estimates$rank > best[estimates$species_key])
  estimates$reason[broader] <- "A more regional estimate exists for this species in the time window"
}
if (!include_proxies) {
  estimates$reason[is.na(estimates$reason) & estimates$proxy] <- "Proxy (include_proxies = FALSE)"
}

trend_checks <- rates[rates$window_end <= 2010 | (include_2011_2020 & rates$window_end <= 2020), ]
rownames(trend_checks) <- NULL


## ---- 5. Points on the model's axis -------------------------------------------------
vp <- estimates[is.na(estimates$reason), ]
# A reported range with no central value (southern right whales, 20-25%) is
# plotted at its midpoint, with the reported bounds as the error bar.
vp$point_is_midpoint <- is.na(vp$status) & !is.na(vp$status_lo) & !is.na(vp$status_hi)
vp$pct           <- ifelse(vp$point_is_midpoint, (vp$status_lo + vp$status_hi) / 2, vp$status) - 100
vp$pct_lo        <- ifelse(is.na(vp$se), vp$status_lo - 100, vp$pct - se_multiplier * vp$se)
vp$pct_hi        <- ifelse(is.na(vp$se), vp$status_hi - 100, vp$pct + se_multiplier * vp$se)
vp$interval_type <- ifelse(!is.na(vp$se),
                           sprintf("+/- %g x reported SE (%g%%)", se_multiplier, vp$se),
                           ifelse(vp$point_is_midpoint,
                                  paste0(vp$interval_type, "; point = midpoint of range"),
                                  vp$interval_type))
vp$key <- ifelse(!vp$proxy, vp$label,
                 sprintf("%s (%s proxy)", vp$label, ifelse(vp$scale == "global", "global", "SH")))
vp[c("pct", "pct_lo", "pct_hi")] <- vp[c("pct", "pct_lo", "pct_hi")] * pct_scale
vp <- vp[order(match(vp$group, c(LB, SP, MI)), vp$label, vp$year), ]

validation_points <- vp[, c("group", "label", "year", "pct", "pct_lo", "pct_hi",
                            "interval_type", "qualifier", "point_is_midpoint", "scale", "proxy",
                            "key", "region", "baseline", "reported", "source", "caveat", "period")]
rownames(validation_points) <- NULL

excluded_estimates <- estimates[!is.na(estimates$reason),
                                c("group", "label", "year", "status", "status_lo", "status_hi",
                                  "qualifier", "scale", "region", "source", "reported", "reason")]
rownames(excluded_estimates) <- NULL


## ---- 6. References -----------------------------------------------------------------
refs <- data.frame(
  key = c("Bamford", "Bejder2016", "Branch2004", "Branch2007", "Cooke2018", "Hamabe2023",
          "HM2014b", "IWC2013SRW", "IWC2015", "IWC2016", "MH2014", "Murase2020",
          "Punt2014", "Whitehead2002"),
  citation = c(
    "Bamford, C., Kelly, N., Herr, H., Seyboth, E. & Jackson, J.A. The recovery of Antarctica's giants - baleen whales. Antarctic Environments Portal. https://environments.aq/publications/the-recovery-of-antarcticas-giants-baleen-whales/ (accessed 25 Sep 2026; confirm the year shown on the page).",
    "Bejder, M., Johnston, D.W., Smith, J., Friedlaender, A. & Bejder, L. (2016) Embracing conservation success of recovering humpback whale populations: evaluating the case for downlisting their conservation status in Australia. Marine Policy 66: 137-141. doi:10.1016/j.marpol.2015.05.007",
    "Branch, T.A., Matsuoka, K. & Miyashita, T. (2004) Evidence for increases in Antarctic blue whales based on Bayesian modelling. Marine Mammal Science 20(4): 726-754. doi:10.1111/j.1748-7692.2004.tb01190.x",
    "Branch, T.A. (2007) Abundance of Antarctic blue whales south of 60S from three complete circumpolar sets of surveys. Journal of Cetacean Research and Management 9(3): 253-262. (Some papers cite pp. 87-96; check the journal PDF.)",
    "Cooke, J.G. (2018) Balaenoptera physalus. The IUCN Red List of Threatened Species 2018: e.T2478A50349982, including its supplementary population assessments for fin, sei and Antarctic blue whales.",
    "Hamabe, K., Matsuoka, K. & Kitakado, T. (2023) Estimation of abundance and population dynamics of the Antarctic blue whale in the Antarctic Ocean south of 60S, from 70E to 170W. Marine Mammal Science 39(2): 671-687. doi:10.1111/mms.13006",
    "Hakamada, T. & Matsuoka, K. (2014b) Estimates of abundance and abundance trend of the humpback whale in Areas IIIE-VIW, south of 60S, based on JARPA and JARPAII sighting data (1989/90-2008/09). Paper SC/F14/J4, IWC Scientific Committee JARPAII Review Workshop, Tokyo, February 2014 (unpublished). 36 pp.",
    "IWC (2013) Report of the workshop on the assessment of southern right whales. Journal of Cetacean Research and Management 14 (Suppl.): 439-462.",
    "IWC (2015) Report of the Scientific Committee, Annex H: Report of the Sub-Committee on Other Southern Hemisphere Whale Stocks (Bled, 2014; IWC/65/Rep01). Journal of Cetacean Research and Management 16 (Suppl.); check the page range.",
    "IWC (2016) Report of the Scientific Committee, Annex H: Report of the Sub-Committee on Other Southern Hemisphere Whale Stocks. Journal of Cetacean Research and Management 17 (Suppl.): 250-282.",
    "Matsuoka, K. & Hakamada, T. (2014) Estimates of abundance and abundance trend of the blue, fin and southern right whales in the Antarctic Areas IIIE-VIW, south of 60S, based on JARPA and JARPAII sighting data (1989/90-2008/09). Paper SC/F14/J05 (unpublished). https://www.icrwhale.org/pdf/SC-F14-J05.pdf",
    "Murase, H., Palka, D., Punt, A.E., Pastene, L.A., Kitakado, T., Matsuoka, K., Hakamada, T., Okamura, H., Bando, T., Tamura, T., Konishi, K., Yasunaga, G., Isoda, T. & Kato, H. (2020) Review of the assessment of two stocks of Antarctic minke whales (eastern Indian Ocean and western South Pacific). Journal of Cetacean Research and Management 21: 95-122.",
    "Punt, A.E., Hakamada, T., Bando, T. & Kitakado, T. (2014) Assessment of Antarctic minke whales using statistical catch-at-age analysis (SCAA). Journal of Cetacean Research and Management 14: 93-116.",
    "Whitehead, H. (2002) Estimates of the current global population size and historical trajectory for sperm whales. Marine Ecology Progress Series 242: 295-304."
  ),
  stringsAsFactors = FALSE
)


## ---- 7. Write and print ------------------------------------------------------------
if (write_outputs) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(validation_points,  file.path(out_dir, "validation_points.csv"),  row.names = FALSE)
  write.csv(excluded_estimates, file.path(out_dir, "excluded_estimates.csv"), row.names = FALSE)
  write.csv(trend_checks,       file.path(out_dir, "trend_checks.csv"),       row.names = FALSE)
  write.csv(refs,               file.path(out_dir, "refs.csv"),               row.names = FALSE)
}

cat("\nValidation points used (include_2011_2020 = ", include_2011_2020, "):\n", sep = "")
print(format(validation_points[, c("group", "label", "year", "pct", "pct_lo", "pct_hi",
                                   "interval_type")], digits = 3), row.names = FALSE)
cat("\nPublished values not used:\n")
print(excluded_estimates[, c("label", "year", "reason")], row.names = FALSE)


## ---- 8. Plotting: overlay helper and preview ---------------------------------------
# validation_layers() returns ggplot2 layers for the abundance panel (column a,
# before patchwork). `facet_var` must be the facet variable's name in the model
# data, and its values must match LB / SP / MI. Each record is one point with
# its error bar; the southern right range is plotted at its midpoint.
# cap_width (in years) adds end caps so intervals shorter than the symbol
# (e.g. the blue and southern right whale bars) stay visible; 0 removes them.
# Symbols are fixed per series (solid = regional, open = proxy) so they do not
# change when include_2011_2020 or the other switches change.
shape_map <- c(
  "Antarctic blue (Areas IV+V)"             = 16,
  "Humpback (Breeding Stock D)"             = 15,
  "Antarctic minke I-stock (K ratio)"       = 17,
  "Southern right (SH proxy)"               = 2,
  "Fin (global proxy)"                      = 1,
  "Sei (global proxy)"                      = 0,
  "Sperm (global proxy)"                    = 5,
  "Antarctic blue, circumpolar (SH proxy)"  = 6,
  "Humpback, all SH stocks (SH proxy)"      = 4
)

validation_layers <- function(pts, facet_var = "group", key_var = "key", shapes = shape_map,
                              point_size = 2, cap_width = 4, colour = "grey10",
                              add_shape_scale = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required for validation_layers()")
  pts[[facet_var]] <- pts$group
  pts$vkey <- pts[[key_var]]
  bars <- pts[!is.na(pts$pct_lo) & !is.na(pts$pct_hi), , drop = FALSE]
  keys <- unique(pts$vkey)
  shp  <- setNames(unname(shapes[keys]), keys)
  shp[is.na(shp)] <- rep(c(3, 8, 7, 9, 10), length.out = sum(is.na(shp)))  # any series not in shape_map
  list(
    if (nrow(bars) > 0) ggplot2::geom_errorbar(
      data = bars, ggplot2::aes(x = year, ymin = pct_lo, ymax = pct_hi),
      inherit.aes = FALSE, width = cap_width, linewidth = 0.4, colour = colour),
    ggplot2::geom_point(
      data = pts, ggplot2::aes(x = year, y = pct, shape = vkey),
      inherit.aes = FALSE, size = point_size, colour = colour, stroke = 0.8),
    if (add_shape_scale) ggplot2::scale_shape_manual(values = shp, name = NULL)
  )
}

if (requireNamespace("ggplot2", quietly = TRUE)) {
  prev <- validation_points
  prev$group <- factor(prev$group, levels = c(LB, SP, MI))
  p_prev <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    validation_layers(prev) +
    ggplot2::facet_wrap(~ group, ncol = 1, scales = "free_y", drop = FALSE) +
    ggplot2::coord_cartesian(xlim = c(1841, if (include_2011_2020) 2020 else 2010)) +
    ggplot2::labs(x = "Year",
                  y = if (pct_scale == 1) "% change from pre-whaling baseline"
                      else "Change from pre-whaling baseline (proportion)") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")
  if (write_outputs) {
    ggplot2::ggsave(file.path(out_dir, "validation_points_preview.png"), p_prev,
                    width = 7, height = 8, dpi = 200)
  }
}

# Adding the points to the model figure (example):
#   pts <- validation_points
#   pts$group <- factor(pts$group, levels = levels(model_df$group))  # if the facet variable is a factor
#   p_abund <- p_abund + validation_layers(pts, facet_var = "group")
# If the plot already has a shape scale, use add_shape_scale = FALSE.
# With free y scales, the 1960 minke point (+62%) will stretch the minke panel.


## ---- 9. Model slopes over the trend_checks windows ---------------------------------
# model_df: one row per year x group (x ensemble member), with % change from
# the unexploited arm. Slopes are instantaneous: 100 * d log(1 + pct/100) / dt,
# the same form as the published rates. The model slope is whaling-attributable
# change (exploited vs unexploited arm), so it matches an observed rate only
# where the unexploited arm is roughly stationary. Rates for single species are
# indicative for the large baleen composite.
model_slopes <- function(model_df, windows = trend_checks, year_col = "year",
                         group_col = "group", pct_col = "pct", member_col = NULL,
                         pct_is_proportion = FALSE) {
  out <- lapply(seq_len(nrow(windows)), function(k) {
    w <- windows[k, ]
    d <- model_df[model_df[[group_col]] == w$group &
                  model_df[[year_col]] >= w$window_start &
                  model_df[[year_col]] <= w$window_end, , drop = FALSE]
    if (nrow(d) < 3) return(NULL)
    ratio <- if (pct_is_proportion) 1 + d[[pct_col]] else 1 + d[[pct_col]] / 100
    d$y_  <- log(pmax(ratio, 1e-9))
    d$x_  <- d[[year_col]]
    slope <- function(dd) 100 * unname(stats::coef(stats::lm(y_ ~ x_, data = dd))[2])
    s  <- if (is.null(member_col)) slope(d) else vapply(split(d, d[[member_col]]), slope, numeric(1))
    qs <- if (length(s) > 1) stats::quantile(s, c(0.025, 0.5, 0.975), names = FALSE) else c(NA_real_, s, NA_real_)
    data.frame(group = w$group, species = w$species, window_start = w$window_start,
               window_end = w$window_end, observed = w$rate_pct_yr, observed_lo = w$lo,
               observed_hi = w$hi, observed_se = w$se,
               model = qs[2], model_lo = qs[1], model_hi = qs[3], stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}
# Example:
#   slope_comparison <- model_slopes(model_df, trend_checks, member_col = "member")
