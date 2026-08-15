# =============================================================================
# FIGURE 4 -- Exploited / Unexploited Antarctic krill consumption, by predator
# group, rebuilt ensemble 44, cut A (unweighted yield RMSE), 167 members.
#
# THE BUG THIS FIXES. The published version came from
# extract_whale_consumption.R:144, which calls
#     getDiet(params, n = n, n_pp = n_pp, n_other = n_other, proportion = FALSE)
# with NO `t` argument. getFeedingLevel then defaults to t = 0 and indexes
# ocean_temp at 0 + t_idx = -1841. Its stored outputs disagree with a faithful
# recomputation by **13-80x**, varying with year (so not a units constant), and
# span 10 orders of magnitude across members at a single year. The member/index
# mapping was verified independently, so it is not an indexing error.
#
# The consumption used here comes from F00_build_rebuilt167_data.R, which uses
# ther_diet(). NOTE: despite the `ther_` prefix, that is NOT a therMizer function
# -- it is local project code at R/wmin_test/thermizer_shim.R:97. It takes `year`
# EXPLICITLY and was verified bit-identical to mizer::getDiet(proportion = FALSE)
# (max relative difference 0) once the temperature scaling is threaded through.
#
# HOW MUCH THIS ACTUALLY CHANGES THE FIGURE -- less than the bug size suggests.
# The stored values are 13-80x too LARGE (the recomputation is the smaller
# number). But the bug inflates the exploited and unexploited arms similarly, so
# it **largely cancels in the ratio** plotted here. So the published Figure 4 is
# probably approximately right despite resting on wrong absolute values.
#
# What the bug definitely does invalidate is any ABSOLUTE krill-consumption
# quantity drawn from whale_consumption_outputs/ -- tonnes per year, per-predator
# totals, or consumption compared across predator groups. Those are 13-80x too
# high and should not be quoted.
#
# Writes Manuscript figures/fig4_krill_ratio_rebuilt167.{png,pdf}
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
})

DATA <- "Manuscript data"
# FIG_SUF / FIG_OUT allow this script to be re-pointed at an alternative data
# build without editing it; the defaults reproduce the published behaviour.
FIGS <- Sys.getenv("FIG_OUT", "Manuscript figures")
dir.create(FIGS, recursive = TRUE, showWarnings = FALSE)
SUF <- Sys.getenv("FIG_SUF", "rebuilt167")
# FIG_SET / FIG_OUTER select the member set and the outer percentile band; both
# default to the published behaviour.
source("Manuscript scripts/F00z_member_set.R")
guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE); f
}

KR <- readRDS(file.path(DATA, sprintf("krill_consumption_%s.rds", SUF)))
meta <- readRDS(file.path(DATA, sprintf("meta_%s.rds", SUF)))
message("members: ", meta$n_members, " | cut: ", meta$cut,
        " | years ", min(KR$Year), "-", max(KR$Year))
# Filter BEFORE the ratio is formed. The ratio is a per-member paired quantity,
# so filtering members here and filtering the ratios later are the same thing --
# but doing it here keeps the exploited/unexploited pairing obviously intact.
KR <- fig_filter(KR, fig_members(meta), "krill consumption")

FISHES <- c("mesopelagic fishes", "bathypelagic fishes",
            "shelf and coastal fishes", "toothfishes")
WHALES <- c("baleen whales", "minke whales")

# --- era markers, derived from the effort array -------------------------------
eff <- readRDS("effort_array_1841_2010.rds")
yr <- as.numeric(rownames(eff))
onset <- function(s) { y <- yr[eff[, s] > 0]; if (length(y)) min(y) else NA }
peak  <- function(s) { v <- eff[, s]; if (any(v > 0)) yr[which.max(v)] else NA }
last  <- function(s) { y <- yr[eff[, s] > 0]; if (length(y)) max(y) else NA }
whaling_start <- min(c(onset("baleen whales"), onset("sperm whales")), na.rm = TRUE)
krill_start   <- onset("antarctic krill")
krill_end     <- last("antarctic krill")
pk <- c("Peak baleen whaling" = peak("baleen whales"),
        "Peak sperm whaling"  = peak("sperm whales"),
        "Peak minke whaling"  = peak("minke whales"),
        "Peak krill fishing"  = peak("antarctic krill"))
pk <- pk[!is.na(pk)]

# --- ratio per member-year, then summarise across members ---------------------
grp_ratio <- function(species, label) {
  KR %>% filter(Species %in% species) %>%
    group_by(sim_index, arm, Year) %>%
    summarise(cons = sum(krill_consumed), .groups = "drop") %>%
    pivot_wider(names_from = arm, values_from = cons) %>%
    filter(is.finite(exploited), is.finite(unexploited), unexploited > 0) %>%
    mutate(ratio = exploited / unexploited, group = label)
}
ALL_PRED <- sort(unique(KR$Species))
R <- bind_rows(
  grp_ratio(ALL_PRED, "All predators"),
  grp_ratio(WHALES,   "Large baleen + minke whales"),
  grp_ratio(FISHES,   "All fishes"))

OP <- fig_outer_probs()
S <- R %>% group_by(group, Year) %>%
  summarise(med = median(ratio, na.rm = TRUE),
            lo  = quantile(ratio, 0.25, na.rm = TRUE),
            hi  = quantile(ratio, 0.75, na.rm = TRUE),
            lo_o = quantile(ratio, OP[1], na.rm = TRUE),
            hi_o = quantile(ratio, OP[2], na.rm = TRUE), .groups = "drop") %>%
  mutate(group = factor(group, levels = c("All predators",
                                          "Large baleen + minke whales",
                                          "All fishes")))

cat("\n=== ratio at 2010 (exploited / unexploited krill consumption) ===\n")
print(as.data.frame(S %>% filter(Year == 2010) %>%
                      select(group, med, lo, hi)), digits = 4, row.names = FALSE)

# --- per-group +-1 SD natural-variability band --------------------------------
# Transcribed from plot_krill_ratio_top10pct_timeseries.R:146-157. Noise follows
# the canonical SNR denominator: the SD, ACROSS YEARS, of the across-member MEAN
# UNEXPLOITED trajectory -- a temporal SD of one curve, one scalar per group.
# Dividing by that curve's own mean turns it into a CV, which is what puts it on
# the multiplicative ratio scale around the null of 1.0.
#
# BASELINE IS 1841-2010, the manuscript-standard window -- the same one the SNR
# figure and the abundance / body-size series use. It does NOT come from
# krill_consumption_*.rds, which F00 truncates to 1900 (DIET_YEARS, Figure 4's x
# axis). F00c_krill_baseline_1841_rebuilt167.R extracts the unexploited arm over
# the full window by F00's own protocol; its 1900-2010 overlap reproduces F00 to
# a max relative difference of EXACTLY 0 over 352,203 values, which is what makes
# the added 1841-1899 years trustworthy. The check is repeated below.
#
# The band is MEAN-based while the ribbons are IQR. That asymmetry is inherited,
# not introduced -- the SNR definition this must match is defined on the mean.
#
# WHY THE WHALE BAND IS ~8x NARROWER THAN THE PUBLISHED FIGURE'S. That figure
# (whale_consumption_outputs/krill_ratio_top10pct_timeseries.png) draws
# [0.83, 1.17]; this one draws [0.979, 1.021]. Now a like-for-like comparison --
# same statistic, same 1841-2010 window -- so the whole gap is the data:
#
#   old data, mean, 1841-2010   sigma = 0.1700   <- the published band, exactly
#   new data, mean, 1841-2010   sigma = 0.0212   <- this figure, 8.0x narrower
#
# It is the getDiet(t = 0) bug, and specifically the fact that this band is built
# on the across-member MEAN. In the old arrays that mean sits 12-15x above the
# median and max/min across members at one year is 5e4-8e4; in the rebuilt data
# those are 1.2-1.3x and 2e2-1.5e3. So the old mean was set by a handful of
# bug-inflated members, and its year-to-year wobble is what the wide band was.
#
# The decisive check is the ROBUST statistic: sigma(median) is 0.0445 on the old
# data and 0.0286 on the new, only 1.6x apart. A change of ensemble membership
# (212 -> 167 after de-duplication and re-run) would move the median too. It does
# not. The difference lives in the tail of the old distribution, not in the
# ensemble's central behaviour.
#
# The window itself turned out to matter little for the rebuilt data -- 1841-2010
# vs 1900-2010 is 1.11x for whales and 0.93x for the other two groups, because
# the rebuilt unexploited trajectory is stationary (1.006x across the 1950 split,
# against 1.199x in the old data). In the OLD data the same window change was
# 1.6x, itself a tail artefact.
BASELINE_YEARS <- 1841:2010
KB <- readRDS(file.path(DATA,
  sprintf("krill_baseline_1841_unexploited_%s.rds", SUF)))
# The baseline is built over the full member set, so it must be put through the
# SAME filter as KR before the membership assertion below -- otherwise the band
# would be drawn from 427 members while the ratio came from 43.
KB <- fig_filter(KB, fig_members(meta), "krill baseline")
stopifnot(identical(unique(KB$arm), "unexploited"),
          setequal(KB$Year, BASELINE_YEARS),
          setequal(unique(KB$sim_index), unique(KR$sim_index)))

sd_band <- function(species, src = KB, yrs = BASELINE_YEARS) {
  u <- src %>% filter(Species %in% species, arm == "unexploited",
                      Year %in% yrs) %>%
    group_by(sim_index, Year) %>%
    summarise(cons = sum(krill_consumed), .groups = "drop") %>%
    group_by(Year) %>%
    summarise(mu = mean(cons, na.rm = TRUE), .groups = "drop")
  sigma <- sd(u$mu, na.rm = TRUE) / mean(u$mu, na.rm = TRUE)
  data.frame(lo = max(1 - sigma, 0), hi = 1 + sigma, sigma = sigma,
             n_yr = nrow(u))
}

# Guard against building the figure on a stale or mismatched baseline file: over
# the years the two sources share, sigma must come out the same from either. F00c
# already checks the values themselves to a max relative difference of 0; this
# re-checks the file actually loaded here, through the statistic that is used.
OVERLAP <- intersect(BASELINE_YEARS, unique(KR$Year))
d_ovl <- max(abs(vapply(list(ALL_PRED, WHALES, FISHES), function(s)
  sd_band(s, KB, OVERLAP)$sigma - sd_band(s, KR, OVERLAP)$sigma, numeric(1))))
cat(sprintf("\nbaseline file check: max |sigma_KB - sigma_KR| over %d-%d = %.3g\n",
            min(OVERLAP), max(OVERLAP), d_ovl))
if (d_ovl > 1e-12) stop("baseline file disagrees with krill_consumption_", SUF,
                        " over the shared years -- refusing to plot")
BAND <- bind_rows(
  cbind(group = "All predators",               sd_band(ALL_PRED)),
  cbind(group = "Large baleen + minke whales", sd_band(WHALES)),
  cbind(group = "All fishes",                  sd_band(FISHES)))

# The top of the y axis: the largest thing actually drawn, plus headroom for the
# "Unexploited" annotation and the two-line event labels. Data-driven, because
# the old fixed c(0, 1.15) was tuned when the widest band edge was 1.019 and it
# CLIPPED the outer ribbons on the phase-88 ensemble.
Y_TOP <- local({
  edges <- c(S$hi, S$med, if (fig_outer()) S$hi_o else NULL, BAND$hi)
  max(1.15, ceiling(max(edges, na.rm = TRUE) * 1.12 * 20) / 20)
})
cat(sprintf("\ny-axis top: %.3f (largest plotted band edge %.4f)\n", Y_TOP,
            max(c(S$hi, if (fig_outer()) S$hi_o else NULL), na.rm = TRUE)))

cat("\n=== +-1 SD natural-variability bands (unexploited",
    min(BASELINE_YEARS), "-", max(BASELINE_YEARS), "baseline) ===\n")
print(as.data.frame(BAND), digits = 4, row.names = FALSE)
# How far the 2010 signal sits outside its own band, in sigma. Printed because
# the bands are only +-1.4 to 1.9% on an axis spanning 0-1.28, so they read as
# hairlines at y = 1 and the separation is not legible from the figure.
sig2010 <- S %>% filter(Year == 2010) %>%
  left_join(BAND, by = "group") %>%
  mutate(dev_sigma = (med - 1) / sigma)
cat("\ndeviation at 2010 in units of each group's own sigma:\n")
print(as.data.frame(sig2010 %>% select(group, med, sigma, dev_sigma)),
      digits = 4, row.names = FALSE)

cols <- c("All predators" = "black",
          "Large baleen + minke whales" = "#FF3E96",
          "All fishes" = "#D9A404")

p <- ggplot(S, aes(Year, med, colour = group, fill = group)) +
  geom_hline(yintercept = 1, colour = "grey45", linetype = "dashed",
             linewidth = 0.5) +
  # clear of the +-1 SD band cluster, which occupies 0.981-1.019 and used to be
  # empty space when this label sat at 1.02
  annotate("text", x = 1903, y = 1.05, label = "Unexploited", hjust = 0,
           size = 3.2, colour = "grey40") +
  geom_vline(xintercept = c(whaling_start, krill_start, krill_end),
             colour = "grey40", linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = pk, colour = "grey55", linetype = "dotted",
             linewidth = 0.4) +
  # Outer percentile band under the IQR, at a lower alpha. Off unless FIG_OUTER=1.
  {if (fig_outer())
    geom_ribbon(aes(ymin = lo_o, ymax = hi_o), alpha = 0.12, colour = NA)} +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.25, colour = NA) +
  # +-1 SD natural-variability references, colour-matched to their group. Over
  # the ribbons so they are not buried, under the medians so those stay legible.
  # A group's median ratio leaving its own band = change beyond the variability
  # of the unexploited run.
  geom_hline(data = BAND, aes(yintercept = lo, colour = group),
             linetype = "dashed", linewidth = 0.5, show.legend = FALSE,
             inherit.aes = FALSE) +
  geom_hline(data = BAND, aes(yintercept = hi, colour = group),
             linetype = "dashed", linewidth = 0.5, show.legend = FALSE,
             inherit.aes = FALSE) +
  geom_line(linewidth = 1) +
  # anchored at the panel top rather than at a fixed y, so they cannot be clipped
  # or left floating when the y limit changes
  annotate("text", x = whaling_start, y = Inf, label = "Whaling\nstarts",
           hjust = -0.06, vjust = 1.25, size = 3.3, colour = "grey30") +
  annotate("text", x = krill_start, y = Inf, label = "Krill fishing\nstarts",
           hjust = -0.06, vjust = 1.25, size = 3.3, colour = "grey30") +
  annotate("text", x = krill_end, y = Inf, label = "End of\nkrill fishing",
           hjust = -0.06, vjust = 1.25, size = 3.3, colour = "grey30") +
  lapply(seq_along(pk), function(i)
    annotate("text", x = pk[i], y = 0.02, label = names(pk)[i], angle = 90,
             hjust = 0, vjust = -0.3, size = 2.9, colour = "grey45")) +
  scale_colour_manual(values = cols, name = "Predator group") +
  scale_fill_manual(values = cols, name = "Predator group") +
  scale_x_continuous(breaks = seq(1900, 2010, 10), limits = c(1898, 2012),
                     expand = expansion(mult = 0.005)) +
  # Y LIMITS ARE DATA-DRIVEN, not fixed. The old c(0, 1.15) was chosen when the
  # widest band edge was 1.019; on the phase-88 ensemble the outer percentile
  # ribbons rise well above that and were being CLIPPED. The top is now the
  # largest plotted band edge plus headroom for the "Unexploited" label and the
  # two-line event labels hung off the panel top. The floor stays at 0: the
  # rotated peak-effort labels start at 0.02, and pinning it at 0 keeps the
  # ratio scale honest (a band reaching down toward 0 must look like it does).
  scale_y_continuous(
    limits = c(0, Y_TOP),
    breaks = seq(0, floor(Y_TOP * 4) / 4, 0.25)) +
  labs(x = "Year",
       y = "Exploited / Unexploited Antarctic krill consumption (ratio)") +
  theme_classic(base_size = 13) +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"))

# Member set and band go in the STEM so the variants cannot overwrite each other.
STEM <- sprintf("fig4_krill_ratio_%s%s", SUF,
                if (identical(Sys.getenv("FIG_SET", "all"), "all")) ""
                else paste0("_", fig_set_tag()))
if (fig_outer()) STEM <- paste0(STEM, "_iqr", 100 * fig_outer_probs()[2])
png_out <- guard(file.path(FIGS, sprintf("%s.png", STEM)))
pdf_out <- guard(file.path(FIGS, sprintf("%s.pdf", STEM)))
ggsave(png_out, p, width = 11, height = 7.5, dpi = 300)
ggsave(pdf_out, p, width = 11, height = 7.5)
write.csv(S, guard(file.path(DATA, sprintf("%s_series.csv", STEM))),
          row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")