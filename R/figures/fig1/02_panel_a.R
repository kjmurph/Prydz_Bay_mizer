###############################################################################
# 02_panel_a.R -- Figure 1 panel A: observed catch time series
#
# Scope is deliberately minimal. This reproduces the observed-yield inset from
# Manuscript scripts/yield_ratio_combined_panels.R (lines ~172-189) at full
# panel size, with only the changes the spec permits:
#   1  theme_science() in place of the Nature-portfolio theme
#   2  the shared taxon palette (this DOES change the colours -- the original
#      used scales::hue_pal(), which is not colourblind-safe)
#   3  no panel-internal title; the description belongs in the figure legend
#   4  era onset markers added (see below)
#   5  legend collected at figure level, shared with panel B
#
# Geometry is otherwise unchanged: one panel, all species on one set of linear
# axes, stacked area. Not split, no broken axis, no log scale.
#
# ERA MARKERS: onsets only -- whaling 1930 and krill fishing 1974, taken from
# ERA_ONSETS in 01_theme_fig1.R, which derives them from the model effort
# forcing and asserts them against the values the results figure draws. The four
# peak-effort markers used by the results figure are deliberately NOT drawn
# here: effort-derived peak sperm whaling is 1948 but the observed sperm-whale
# catch peaks in 1951, and this panel plots that catch curve directly, so the
# marker would contradict its own data.
#
# Drawn as dashed lines rather than translucent bands: bands over a stacked area
# tint the fills underneath and make the taxon colours ambiguous, which is worse.
# Dashed lines also match the convention in panel C and in the results figure.
#
# Data: Manuscript data/yield_cached_obs_stack.rds (Year, Species, Yield_g,
# Yield_t; 1900-2010; 9 taxa with non-zero catch).
#
# Exposes panel_a(); no side effects, no printing, no file writing.
###############################################################################

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(scales)
})

# Stacking order, smallest-bodied at the bottom, as in the source figure.
CATCH_STACK_ORDER <- c(
  "antarctic krill", "squids", "bathypelagic fishes",
  "shelf and coastal fishes", "toothfishes",
  "orca", "minke whales", "sperm whales", "baleen whales"
)

panel_a <- function(data_dir = DATA_DIR) {

  f <- file.path(data_dir, "yield_cached_obs_stack.rds")
  if (!file.exists(f)) stop("Missing observed catch cache: ", f)
  obs <- readRDS(f)

  present <- intersect(CATCH_STACK_ORDER, unique(as.character(obs$Species)))
  extra   <- setdiff(unique(as.character(obs$Species)), CATCH_STACK_ORDER)
  if (length(extra))
    warning("Catch taxa not in the stacking order, appended: ",
            paste(extra, collapse = ", "))
  lvls <- c(present, extra)

  obs <- obs %>%
    dplyr::filter(!is.na(Yield_t)) %>%
    dplyr::mutate(Species = factor(as.character(Species), levels = lvls))

  # Legend keys only for the taxa the figure names; the remainder still fill the
  # stack. Ordered largest-bodied first so the key reads top-down like the stack.
  leg <- rev(intersect(lvls, PANEL_A_LEGEND_TAXA))

  y_max <- obs %>% dplyr::group_by(Year) %>%
    dplyr::summarise(tot = sum(Yield_t, na.rm = TRUE), .groups = "drop") %>%
    dplyr::pull(tot) %>% max(na.rm = TRUE)

  ggplot(obs, aes(x = Year, y = Yield_t, fill = Species)) +
    geom_area(position = "stack", alpha = 0.9, colour = NA) +
    geom_vline(xintercept = as.numeric(ERA_ONSETS), linetype = "dashed",
               colour = "grey45", linewidth = 0.25) +
    annotate("text", x = as.numeric(ERA_ONSETS) - 1.5, y = y_max * 1.02,
             label = sub(" ", "\n", names(ERA_ONSETS)),
             hjust = 1, vjust = 1, lineheight = 0.9,
             size = ANNOT_SIZE_PT / .pt, colour = "grey40") +
    scale_fill_manual(values = TAXON_COLOURS, breaks = leg,
                      labels = TAXON_LABELS[leg], name = NULL,
                      drop = FALSE) +
    scale_x_continuous(breaks = seq(1900, 2010, by = 10),
                       expand = expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(labels = function(x) comma(x / 1e3),
                       expand = expansion(mult = c(0, 0.10))) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE,
                               override.aes = list(alpha = 1))) +
    labs(x = "Year", y = expression(Catch~(10^3~t~y^{-1}))) +
    theme_science() +
    theme(axis.text.x = element_text(angle = 40, hjust = 1),
          legend.position = "bottom",
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(-2, 0, 0, 0)) +
    coord_cartesian(clip = "off")
}
