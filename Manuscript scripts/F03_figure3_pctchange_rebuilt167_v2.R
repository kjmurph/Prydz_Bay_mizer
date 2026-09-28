# =============================================================================
# FIGURE 3 -- paired percentage change from unexploited, by functional group,
# v2 (drawn for Science, matching Figures 1 and 2).
#
#   A  Abundance change      (numbers)
#   B  Body size change      (mean individual mass)
#
# ENSEMBLE. Defaults to the p104 ensemble, FIG_SUF=p104q10, all 203 usable
# members (FIG_SET=top for the top cut). Canonical invocation:
#   Rscript run_p104q10.R "Manuscript scripts/F03_figure3_pctchange_rebuilt167_v2.R"
# which also points FIG_OUT at Manuscript figures/p104 figures/.
#
# EVERY NUMBER IS COMPUTED EXACTLY AS IN v1 (F03_figure3_pctchange_rebuilt167.R):
# the panel definitions, the panel aggregation, the paired percentage change,
# the IQR summary and the +/-1 SD detectability band are transcribed unchanged,
# and the series CSV this writes must equal v1's. Only the drawing changes.
#
# WHAT THE DRAWING CHANGES FROM v1 (Kieran, 2026-09-26):
#   - Drawn at PRINTED SIZE, 180 x 225 mm, with Figure 2's type sizes: axis text
#     9 pt, axis titles 9 pt, column headers 10 pt bold, panel letters 9 pt bold
#     UPPERCASE (v1 drew 14 x 20 in and printed at ~0.5x, i.e. ~4.6 pt text).
#     Exception: y tick labels are 8 pt, the size of the row labels.
#   - Column headers are Figure 2's (bold title over a rule), each with the
#     HARVEST STRIP beneath it -- observed catch weight, from
#     F00h_harvest_strip.R, the same bar as Figures 2 and 4.
#   - The onset lines ("Whaling starts", "Krill fishing starts") and every
#     in-panel label are gone: at print size a row is ~14 mm tall and a rotated
#     "Peak ... whaling" label is 20-25 mm. Each group keeps its coloured
#     PEAK-EFFORT line, unlabelled; the caption explains it. Peak EFFORT, not
#     peak catch, as asked -- they differ for shelf & coastal fishes (1978 vs
#     1952), toothfishes (2008 vs 2004) and sperm whales (1948 vs 1951).
#   - The +/-1 SD band is Figure 2's: a grey70 ribbon at alpha 0.25 instead of
#     red dashed lines, with a solid grey55 zero line instead of a dashed one.
#   - Row strips are horizontal and wrapped: rotated, the longest name needs
#     ~30 mm in a ~14 mm row.
#   - x runs 1925-2010 with no padding, first labelled tick 1930.
#   - Seabirds are #B2182B, not grey #9E9E9E, which was indistinguishable from
#     the grey band (OKLCH chroma 0). #B2182B is the best of six candidates:
#     dE >= 17.7 (OKLab x100) from every other panel colour under normal vision
#     and simulated protan/deutan/tritan CVD (Machado 2009), 6.9:1 on white.
#   - OPTIONAL PUBLISHED VALIDATION POINTS on the abundance column (A), for
#     large baleen, sperm and minke whales, OFF BY DEFAULT: FIG_WHALE_OBS=TRUE
#     draws them and appends "_whaleobs" to the file names, so the two variants
#     cannot overwrite each other. From prydz_whale_validation_points.R -- one
#     shape per series, NO LEGEND (the caption carries it). That script is sourced as
#     written, in a temporary directory so its own CSV / preview outputs do not
#     land in the repo; its switches (include_2011_2020 etc.) therefore carry
#     through. Points outside the x range are dropped and reported (by default
#     the 1880 sperm-whale value).
#
# A VALIDATION POINT IS NOT THE SAME QUANTITY AS THE MODEL LINE. The line is the
# exploited run against its own unexploited run in the same year; a published
# status is abundance against a pre-whaling level (for minke, carrying capacity
# in 1930). They coincide only where the unexploited arm is roughly stationary.
#
# Writes FIG_OUT/fig3_pctchange_<SUF>[_top]_v2[_whaleobs].{png,pdf}
#        Manuscript data/fig3_pctchange_<SUF>[_top]_v2[_whaleobs]_series.csv
# guard() refuses to overwrite: move earlier outputs before re-running.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(grid)
})

DATA <- "Manuscript data"
FIGS <- Sys.getenv("FIG_OUT", "Manuscript figures")
dir.create(FIGS, recursive = TRUE, showWarnings = FALSE)
SUF <- Sys.getenv("FIG_SUF", "p104q10"); BASELINE <- 1841:2010
VER <- "_v2"
VAL_SCRIPT <- "Manuscript scripts/prydz_whale_validation_points.R"
# Whale survey / assessment points on panel A: FIG_WHALE_OBS=TRUE to draw them.
SHOW_WHALE_OBS <- as.logical(Sys.getenv("FIG_WHALE_OBS", "FALSE"))
if (is.na(SHOW_WHALE_OBS))
  stop("FIG_WHALE_OBS must be TRUE or FALSE", call. = FALSE)
source("Manuscript scripts/F00z_member_set.R")
source("Manuscript scripts/F00h_harvest_strip.R")
guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE); f
}

# --- output size and typography -----------------------------------------------
# Drawn at PRINTED size, so every size is points on the page (Figure 2's values).
# Geoms take millimetres, hence <pt>/.pt.
FIG_W    <- 180 / 25.4   # in
FIG_H    <- 225 / 25.4   # in
BASE     <- 9      # pt -- axis text (x)
YTICK    <- 8      # pt -- y tick labels, = the row labels (Kieran, 2026-09-26)
AXT      <- 9      # pt -- axis titles
TAG      <- 9      # pt -- panel letters (Science: 9 pt bold)
HDR      <- 10     # pt -- column headers
STRIP_PT <- 8      # pt -- harvest strip labels and row strip labels
X_LIM    <- c(1925, 2010)
X_BRK    <- seq(1930, 2010, by = 20)
BAND_FILL  <- "grey70"   # Figure 2's natural-variability band
BAND_ALPHA <- 0.25
LN_MED   <- 0.6    # median line
LN_PEAK  <- 0.4    # peak-effort lines
VP_SIZE  <- 1.6    # validation point size
VP_CAP   <- 3      # validation error-bar cap width, years

# --- panel definitions, identical to v1 ---------------------------------------
group_defs <- list(
  "Zooplankton"            = c("mesozooplankton", "other krill",
                               "other macrozooplankton", "salps"),
  "Pelagic fishes & squid" = c("mesopelagic fishes", "bathypelagic fishes", "squids"),
  "Seabirds"               = c("flying birds", "small divers"),
  "Pinnipeds"              = c("medium divers", "large divers"))
ind_to_panel <- c("antarctic krill" = "Antarctic krill", "toothfishes" = "Toothfishes",
  "shelf and coastal fishes" = "Shelf & coastal fishes",
  "leopard seals" = "Leopard seals", "minke whales" = "Minke whales",
  "orca" = "Orca", "sperm whales" = "Sperm whales",
  "baleen whales" = "Large baleen whales")
panel_levels <- c("Large baleen whales", "Sperm whales", "Minke whales", "Orca",
  "Leopard seals", "Pinnipeds", "Seabirds", "Toothfishes",
  "Shelf & coastal fishes", "Pelagic fishes & squid", "Antarctic krill",
  "Zooplankton")
# v1's palette except Seabirds (#9E9E9E -> #B2182B; see the header).
panel_colors <- c("Large baleen whales" = "#FF61C3", "Minke whales" = "#00B9E3",
  "Sperm whales" = "#DB72FB", "Orca" = "#619CFF", "Leopard seals" = "#E07B39",
  "Pinnipeds" = "#2B6CB0", "Seabirds" = "#B2182B", "Toothfishes" = "#00C19F",
  "Shelf & coastal fishes" = "#93AA00", "Pelagic fishes & squid" = "#D39200",
  "Antarctic krill" = "#F8766D", "Zooplankton" = "#6A1B9A")
# Row-strip text, wrapped to fit a horizontal strip.
panel_strip <- c("Large baleen whales" = "Large baleen\nwhales",
  "Sperm whales" = "Sperm\nwhales", "Minke whales" = "Minke\nwhales",
  "Orca" = "Orca", "Leopard seals" = "Leopard\nseals", "Pinnipeds" = "Pinnipeds",
  "Seabirds" = "Seabirds", "Toothfishes" = "Toothfishes",
  "Shelf & coastal fishes" = "Shelf &\ncoastal fishes",
  "Pelagic fishes & squid" = "Pelagic fishes\n& squid",
  "Antarctic krill" = "Antarctic\nkrill", "Zooplankton" = "Zooplankton")

sp2panel <- c(ind_to_panel,
  setNames(rep(names(group_defs), lengths(group_defs)), unlist(group_defs)))

# --- data (identical to v1) ---------------------------------------------------
bf <- readRDS(file.path(DATA, sprintf("biomass_abund_fish_%s.rds", SUF)))
bc <- readRDS(file.path(DATA, sprintf("biomass_abund_clim_%s.rds", SUF)))
meta <- readRDS(file.path(DATA, sprintf("meta_%s.rds", SUF)))
message("members: ", meta$n_members, " | cut: ", meta$cut)
KEEP <- fig_members(meta)
bf <- fig_filter(bf, KEEP, "exploited")
bc <- fig_filter(bc, KEEP, "unexploited")

# Aggregate to panels FIRST: for multi-species panels the abundance is summed and
# the mean mass is the biomass-weighted group mean (total biomass / total number),
# not the mean of per-species means.
to_panel <- function(d) d %>%
  mutate(panel = unname(sp2panel[Species])) %>% filter(!is.na(panel)) %>%
  group_by(sim_index, Year, panel) %>%
  summarise(Abundance = sum(Abundance), Biomass = sum(Biomass), .groups = "drop") %>%
  mutate(MeanMass = ifelse(Abundance > 0, Biomass / Abundance, NA_real_))
PF <- to_panel(bf); PC <- to_panel(bc)

# --- paired percentage change (identical to v1) -------------------------------
pair <- inner_join(PF, PC, by = c("sim_index", "Year", "panel"),
                   suffix = c("_f", "_c")) %>%
  mutate(pct_abund = 100 * (Abundance_f - Abundance_c) / Abundance_c,
         pct_mass  = 100 * (MeanMass_f  - MeanMass_c)  / MeanMass_c)

OP <- fig_outer_probs()
summ <- function(v) pair %>% group_by(panel, Year) %>%
  summarise(med = median(.data[[v]], na.rm = TRUE),
            lo = quantile(.data[[v]], 0.25, na.rm = TRUE),
            hi = quantile(.data[[v]], 0.75, na.rm = TRUE),
            lo_o = quantile(.data[[v]], OP[1], na.rm = TRUE),
            hi_o = quantile(.data[[v]], OP[2], na.rm = TRUE),
            .groups = "drop") %>%
  mutate(panel = factor(panel, levels = panel_levels))
S_ab <- summ("pct_abund"); S_mw <- summ("pct_mass")

# --- +/-1 SD detectability band, per panel and metric (identical to v1) --------
band <- function(metric) PC %>% group_by(panel, Year) %>%
  summarise(mu = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
  filter(Year %in% BASELINE) %>% group_by(panel) %>%
  summarise(ref_pct = 100 * sd(mu, na.rm = TRUE) / mean(mu, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(panel = factor(panel, levels = panel_levels))
B_ab <- band("Abundance"); B_mw <- band("MeanMass")

# --- per-group peak-effort lines (as v1, now unlabelled) -----------------------
eff <- readRDS("effort_array_1841_2010.rds"); yr <- as.numeric(rownames(eff))
fishing_peak <- do.call(rbind, lapply(colnames(eff), function(sp) {
  if (any(eff[, sp] > 0))
    data.frame(Species = sp, first_year = yr[which.max(eff[, sp])],
               stringsAsFactors = FALSE)
}))
species_events_df <- fishing_peak %>%
  mutate(panel = unname(sp2panel[Species])) %>%
  filter(!is.na(panel)) %>%
  mutate(panel = factor(panel, levels = panel_levels))
cat("\n=== peak-effort lines (for the caption) ===\n")
print(as.data.frame(species_events_df %>% arrange(panel) %>%
                      select(panel, Species, peak_effort_year = first_year)),
      row.names = FALSE)

# --- harvest strip -------------------------------------------------------------
STRIP <- harvest_strip_data(X_LIM[1]:X_LIM[2])

# --- published validation points (only when FIG_WHALE_OBS=TRUE) ----------------
# Sourced as written, in a temporary directory: the script writes its own CSVs
# and a preview PNG to "validation_outputs/" relative to the working directory.
load_validation <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  ve <- new.env()
  old <- setwd(tempdir()); on.exit(setwd(old))
  source(path, local = ve)
  ve
}
vp <- NULL
if (SHOW_WHALE_OBS) {
  VAL <- load_validation(VAL_SCRIPT)
  if (!identical(VAL$pct_scale, 1))
    stop("validation points must be in percent (pct_scale = 1) to sit on this ",
         "axis", call. = FALSE)
  vp <- VAL$validation_points
  if (!all(vp$group %in% panel_levels))
    stop("validation group(s) not in panel_levels: ",
         paste(setdiff(vp$group, panel_levels), collapse = ", "), call. = FALSE)
  out_x <- vp$year < X_LIM[1] | vp$year > X_LIM[2]
  if (any(out_x))
    message("validation points outside ", X_LIM[1], "-", X_LIM[2], ", not drawn: ",
            paste(sprintf("%s %d", vp$label[out_x], vp$year[out_x]), collapse = "; "))
  vp <- vp[!out_x, ] %>% mutate(panel = factor(group, levels = panel_levels))
  # One shape per series: the validation script's own shape_map (solid =
  # regional, open = proxy), so the symbols do not move when its switches change.
  vp_shapes <- VAL$shape_map[unique(vp$key)]
  if (anyNA(vp_shapes))
    stop("no shape in shape_map for: ",
         paste(unique(vp$key)[is.na(vp_shapes)], collapse = ", "), call. = FALSE)
  cat("\n=== validation points drawn on panel A (shapes for the caption) ===\n")
  print(data.frame(vp[, c("group", "label", "year", "pct", "pct_lo", "pct_hi",
                          "interval_type")], shape = unname(vp_shapes[vp$key])),
        digits = 3, row.names = FALSE)
} else {
  message("whale validation points: off (FIG_WHALE_OBS=TRUE to draw them)")
}

# =============================================================================
# DRAWING (v2)
# =============================================================================
x_sc <- scale_x_continuous(limits = X_LIM, breaks = X_BRK, expand = c(0, 0))

# y ticks: the step scales::extended_breaks() would pick for ~3 ticks, laid on
# MULTIPLES of that step, so 0 -- the unexploited reference -- is always labelled.
# extended_breaks alone skipped it in some rows (e.g. "-10%, 10%", "1%, 3%").
zero_breaks <- function(lims) {
  b <- scales::extended_breaks(n = 3)(lims)
  step <- if (length(b) > 1) diff(b)[1] else NA
  if (!is.finite(step) || step <= 0) return(b)
  seq(ceiling(lims[1] / step - 1e-9) * step, floor(lims[2] / step + 1e-9) * step,
      by = step)
}

th_fig3 <- theme_bw(base_size = BASE) +
  theme(legend.position  = "none",
        panel.grid       = element_blank(),
        panel.spacing.y  = unit(2.2, "mm"),   # keeps adjacent tick labels apart
        axis.text        = element_text(size = BASE, colour = "grey20"),
        axis.text.y      = element_text(size = YTICK, colour = "grey20"),
        axis.title       = element_text(size = AXT),
        strip.text.y.right = element_text(angle = 0, hjust = 0, size = STRIP_PT,
                                          lineheight = 0.9,
                                          margin = margin(1, 2, 1, 2)),
        plot.margin      = margin(1, 4, 2, 2))

build_panel <- function(summ_df, ref_df, events_sp_df, show_strips,
                        points = NULL) {
  p <- ggplot() +
    geom_rect(data = ref_df, aes(xmin = -Inf, xmax = Inf,
                                 ymin = -ref_pct, ymax = ref_pct),
              fill = BAND_FILL, alpha = BAND_ALPHA, inherit.aes = FALSE) +
    geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.35) +
    geom_vline(data = events_sp_df, aes(xintercept = first_year, colour = panel),
               linetype = "dashed", linewidth = LN_PEAK, alpha = 0.8) +
    # Outer percentile band UNDER the IQR, off unless FIG_OUTER=1 (as v1).
    {if (fig_outer())
      geom_ribbon(data = summ_df,
                  aes(x = Year, ymin = lo_o, ymax = hi_o, fill = panel),
                  alpha = 0.14)} +
    geom_ribbon(data = summ_df, aes(x = Year, ymin = lo, ymax = hi, fill = panel),
                alpha = 0.3) +
    geom_line(data = summ_df, aes(x = Year, y = med, colour = panel),
              linewidth = LN_MED)
  if (!is.null(points)) {
    bars <- points[!is.na(points$pct_lo) & !is.na(points$pct_hi), ]
    p <- p +
      geom_errorbar(data = bars, aes(x = year, ymin = pct_lo, ymax = pct_hi),
                    width = VP_CAP, linewidth = 0.35, colour = "grey10",
                    inherit.aes = FALSE) +
      geom_point(data = points, aes(x = year, y = pct, shape = key),
                 size = VP_SIZE, stroke = 0.6, colour = "grey10",
                 inherit.aes = FALSE) +
      scale_shape_manual(values = vp_shapes, guide = "none")
  }
  p <- p +
    facet_grid(rows = vars(panel), scales = "free_y",
               labeller = labeller(panel = panel_strip)) +
    x_sc +
    # 8% padding (default 5%) pulls the end ticks in from the panel edges, so a
    # row's bottom label does not meet the next row's top label.
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       breaks = zero_breaks,
                       expand = expansion(mult = 0.08)) +
    scale_fill_manual(values = panel_colors, guide = "none") +
    scale_colour_manual(values = panel_colors, guide = "none") +
    # clip off: x has no padding at 2010, so the 2009 validation points and
    # their caps would otherwise be cut by the panel border. Nothing else can
    # spill -- the scale limits have already dropped out-of-range data.
    coord_cartesian(clip = "off") +
    th_fig3 + labs(x = "Year", y = "% change from unexploited")
  # strip.text.y.RIGHT, not strip.text.y: th_fig3 sets the more specific
  # element, which would otherwise win and keep the labels.
  if (show_strips) p else
    p + theme(strip.text.y.right = element_blank(),
              strip.background.y = element_blank())
}

# Column header: bold title over a rule spanning the panel width, with the panel
# letter at its upper left (Figure 2's header, plus the tag).
col_header <- function(title, tag) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.62, label = title, fontface = "bold",
             size = HDR / .pt, colour = "grey10") +
    annotate("segment", x = 0, xend = 1, y = 0.1, yend = 0.1,
             linewidth = 0.4, colour = "grey35") +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    coord_cartesian(clip = "off") + theme_void() +
    labs(tag = tag) +
    theme(plot.tag = element_text(size = TAG, face = "bold", hjust = 0, vjust = 1),
          plot.tag.position = c(0, 1),
          plot.margin = margin(0, 4, 0, 2))
}

p_ab <- build_panel(S_ab, B_ab, species_events_df, show_strips = FALSE,
                    points = vp)
p_mw <- build_panel(S_mw, B_mw, species_events_df, show_strips = TRUE)
bar  <- function() harvest_strip_plot(STRIP, x_sc, X_LIM, label_pt = STRIP_PT,
                                      plot_margin = margin(0.5, 4, 0.5, 2))

fig <- wrap_plots(
  col_header("Abundance change", "A"), col_header("Body size change", "B"),
  bar(), bar(),
  p_ab, p_mw,
  ncol = 2, widths = c(1, 1),
  heights = unit(c(6, 7, 1), c("mm", "mm", "null")))

set_tag <- if (identical(Sys.getenv("FIG_SET", "all"), "all")) "" else
  paste0("_", fig_set_tag())
STEM <- sprintf("fig3_pctchange_%s%s%s", SUF, set_tag, VER)
if (fig_outer()) STEM <- paste0(STEM, "_iqr", 100 * fig_outer_probs()[2])
if (SHOW_WHALE_OBS) STEM <- paste0(STEM, "_whaleobs")
png_out <- guard(file.path(FIGS, sprintf("%s.png", STEM)))
pdf_out <- guard(file.path(FIGS, sprintf("%s.pdf", STEM)))
ggsave(png_out, fig, width = FIG_W, height = FIG_H, dpi = 600)
ggsave(pdf_out, fig, width = FIG_W, height = FIG_H)

out <- bind_rows(S_ab %>% mutate(metric = "abundance"),
                 S_mw %>% mutate(metric = "mean_mass")) %>%
  left_join(bind_rows(B_ab %>% mutate(metric = "abundance"),
                      B_mw %>% mutate(metric = "mean_mass")),
            by = c("panel", "metric"))
write.csv(out, guard(file.path(DATA, sprintf("%s_series.csv", STEM))),
          row.names = FALSE)

cat("\n=== % change at 2010 (median across members) ===\n")
print(as.data.frame(out %>% filter(Year == 2010) %>%
  select(panel, metric, med, ref_pct) %>% arrange(metric, med)), digits = 4,
  row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")
