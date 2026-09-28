# =============================================================================
# FIGURE 4 -- Exploited / Unexploited Antarctic krill consumption, by predator
# group, v2 (drawn for Science, matching Figures 1-3).
#
# ENSEMBLE. Defaults to the p104 ensemble, FIG_SUF=p104q10, all 203 usable
# members (FIG_SET=top for the top cut). Canonical invocation:
#   Rscript run_p104q10.R "Manuscript scripts/F04_figure4_krill_ratio_rebuilt167_v2.R"
# which also points FIG_OUT at Manuscript figures/p104 figures/.
#
# EVERY NUMBER IS COMPUTED EXACTLY AS IN v1 (F04_figure4_krill_ratio_rebuilt167.R):
# the per-member ratio, the IQR summary, the +/-1 SD natural-variability bands
# on the 1841-2010 unexploited baseline and the baseline-file guard are
# transcribed unchanged, and the series CSV this writes must equal v1's. v1's
# header documents the getDiet(t = 0) bug this data build avoids and why the
# bands are ~8x narrower than the published figure's; none of that changes here.
#
# WHAT THE DRAWING CHANGES FROM v1 (Kieran, 2026-09-26):
#   - Drawn at PRINTED SIZE, 180 x 115 mm, with Figure 2/3's type sizes: axis
#     text and titles 9 pt, legend 8 pt (v1 drew 11 x 7.5 in at base 13 and
#     printed at ~0.64x, i.e. ~5-8 pt).
#   - The HARVEST STRIP replaces every vertical line and its label -- the
#     "Whaling starts" / "Krill fishing starts" / "End of krill fishing" dashed
#     lines and the four dotted "Peak ..." lines. It is observed catch weight,
#     from F00h_harvest_strip.R, the same bar as Figures 2 and 3.
#   - The ratio = 1 reference is a solid grey55 line, as the zero line in
#     Figures 2 and 3 (v1: dashed grey45). The per-group +/-1 SD band edges stay
#     dashed and group-coloured, since each group has its own band, but thinner
#     (BAND_LW): six of them fall within +-4% of 1.
#   - x runs 1925-2010 with no padding, as Figure 3, ticks every 10 years.
#   - The y title is v1's wording on two lines, so it fits the panel height.
#
# Writes FIG_OUT/fig4_krill_ratio_<SUF>[_top]_v2.{png,pdf}
#        Manuscript data/fig4_krill_ratio_<SUF>[_top]_v2_series.csv
# guard() refuses to overwrite: move earlier outputs before re-running.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
  library(patchwork)
})

DATA <- "Manuscript data"
FIGS <- Sys.getenv("FIG_OUT", "Manuscript figures")
dir.create(FIGS, recursive = TRUE, showWarnings = FALSE)
SUF <- Sys.getenv("FIG_SUF", "p104q10")
VER <- "_v2"
source("Manuscript scripts/F00z_member_set.R")
source("Manuscript scripts/F00h_harvest_strip.R")
guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE); f
}

# --- output size and typography -----------------------------------------------
# Drawn at PRINTED size, so every size is points on the page. Geoms take mm.
FIG_W    <- 180 / 25.4   # in
FIG_H    <- 115 / 25.4   # in
BASE     <- 9      # pt -- axis text
AXT      <- 9      # pt -- axis titles
LEG      <- 8      # pt -- legend entries
STRIP_PT <- 8      # pt -- harvest strip labels
NOTE_PT  <- 7      # pt -- "Unexploited"
X_LIM    <- c(1925, 2010)
X_BRK    <- seq(1930, 2010, by = 10)
BAND_LW  <- 0.25   # band-edge linewidth: 0.25 x .pt = 0.71 lwd = 0.53 pt
                   # (was 0.35, 0.75 pt; Kieran 2026-09-26). >= 0.24 for 0.5 pt

KR <- readRDS(file.path(DATA, sprintf("krill_consumption_%s.rds", SUF)))
meta <- readRDS(file.path(DATA, sprintf("meta_%s.rds", SUF)))
message("members: ", meta$n_members, " | cut: ", meta$cut,
        " | years ", min(KR$Year), "-", max(KR$Year))
# Filter BEFORE the ratio is formed (as v1).
KR <- fig_filter(KR, fig_members(meta), "krill consumption")

FISHES <- c("mesopelagic fishes", "bathypelagic fishes",
            "shelf and coastal fishes", "toothfishes")
WHALES <- c("baleen whales", "minke whales")

# --- ratio per member-year, then summarise across members (identical to v1) ---
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

# --- per-group +-1 SD natural-variability band (identical to v1) ---------------
# Noise = temporal SD of the across-member MEAN unexploited trajectory over
# 1841-2010, divided by that curve's mean (a CV, on the ratio scale around 1).
BASELINE_YEARS <- 1841:2010
KB <- readRDS(file.path(DATA,
  sprintf("krill_baseline_1841_unexploited_%s.rds", SUF)))
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

# Guard against a stale or mismatched baseline file (as v1).
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

# y-axis top, data-driven, as v1.
Y_TOP <- local({
  edges <- c(S$hi, S$med, if (fig_outer()) S$hi_o else NULL, BAND$hi)
  max(1.15, ceiling(max(edges, na.rm = TRUE) * 1.12 * 20) / 20)
})
cat(sprintf("\ny-axis top: %.3f (largest plotted band edge %.4f)\n", Y_TOP,
            max(c(S$hi, if (fig_outer()) S$hi_o else NULL), na.rm = TRUE)))

cat("\n=== +-1 SD natural-variability bands (unexploited",
    min(BASELINE_YEARS), "-", max(BASELINE_YEARS), "baseline) ===\n")
print(as.data.frame(BAND), digits = 4, row.names = FALSE)
sig2010 <- S %>% filter(Year == 2010) %>%
  left_join(BAND, by = "group") %>%
  mutate(dev_sigma = (med - 1) / sigma)
cat("\ndeviation at 2010 in units of each group's own sigma:\n")
print(as.data.frame(sig2010 %>% select(group, med, sigma, dev_sigma)),
      digits = 4, row.names = FALSE)

cols <- c("All predators" = "black",
          "Large baleen + minke whales" = "#FF3E96",
          "All fishes" = "#D9A404")

# =============================================================================
# DRAWING (v2)
# =============================================================================
x_sc <- scale_x_continuous(limits = X_LIM, breaks = X_BRK, expand = c(0, 0))

p <- ggplot(S, aes(Year, med, colour = group, fill = group)) +
  geom_hline(yintercept = 1, colour = "grey55", linewidth = 0.35) +
  # At the RIGHT end, under the lowest band edge: at the left the dashed band
  # edges strike through it and the fish ribbon rises before it ends, while in
  # the 2000s the gap between the bands (>= 0.96) and the whale ribbon (<= 0.70)
  # is empty.
  annotate("text", x = X_LIM[2] - 0.5, y = min(BAND$lo), label = "Unexploited",
           hjust = 1, vjust = 1.5, size = NOTE_PT / .pt, colour = "grey30") +
  {if (fig_outer())
    geom_ribbon(aes(ymin = lo_o, ymax = hi_o), alpha = 0.12, colour = NA)} +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.25, colour = NA) +
  # +-1 SD natural-variability references, colour-matched to their group (v1).
  # Six of them fall within +-4% of 1, so they are drawn at BAND_LW, just
  # above Science's 0.5 pt floor, to keep the cluster from reading as a stripe.
  geom_hline(data = BAND, aes(yintercept = lo, colour = group),
             linetype = "dashed", linewidth = BAND_LW, show.legend = FALSE,
             inherit.aes = FALSE) +
  geom_hline(data = BAND, aes(yintercept = hi, colour = group),
             linetype = "dashed", linewidth = BAND_LW, show.legend = FALSE,
             inherit.aes = FALSE) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = cols, name = "Predator group") +
  scale_fill_manual(values = cols, name = "Predator group") +
  x_sc +
  scale_y_continuous(limits = c(0, Y_TOP),
                     breaks = seq(0, floor(Y_TOP * 4) / 4, 0.25)) +
  coord_cartesian(clip = "off") +
  labs(x = "Year",
       y = "Exploited / Unexploited\nAntarctic krill consumption (ratio)") +
  theme_classic(base_size = BASE) +
  theme(panel.grid   = element_blank(),
        axis.text    = element_text(size = BASE, colour = "grey20"),
        axis.title   = element_text(size = AXT, lineheight = 0.95),
        legend.position = "bottom",
        legend.title = element_text(size = LEG, face = "bold"),
        legend.text  = element_text(size = LEG),
        legend.key.size = unit(4, "mm"),
        legend.margin = margin(0, 0, 0, 0),
        plot.margin  = margin(1, 6, 2, 2))

STRIP <- harvest_strip_data(X_LIM[1]:X_LIM[2])
bar <- harvest_strip_plot(STRIP, x_sc, X_LIM, label_pt = STRIP_PT,
                          plot_margin = margin(2, 6, 1, 2))
fig <- (bar / p) + plot_layout(heights = unit(c(7, 1), c("mm", "null")))

set_tag <- if (identical(Sys.getenv("FIG_SET", "all"), "all")) "" else
  paste0("_", fig_set_tag())
STEM <- sprintf("fig4_krill_ratio_%s%s%s", SUF, set_tag, VER)
if (fig_outer()) STEM <- paste0(STEM, "_iqr", 100 * fig_outer_probs()[2])
png_out <- guard(file.path(FIGS, sprintf("%s.png", STEM)))
pdf_out <- guard(file.path(FIGS, sprintf("%s.pdf", STEM)))
ggsave(png_out, fig, width = FIG_W, height = FIG_H, dpi = 600)
ggsave(pdf_out, fig, width = FIG_W, height = FIG_H)
write.csv(S, guard(file.path(DATA, sprintf("%s_series.csv", STEM))),
          row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")
