###############################################################################
# 04_panel_c.R -- Figure 1 panel C: the paired counterfactual
#
# Two stacked sub-plots sharing a vertical alignment, returned as a patchwork:
#
#   upper  ensemble trajectories, 212 fitted members and their paired
#          unexploited counterfactuals, in NATIVE units (slope units for
#          lambda). Median line + 50% and 90% across-member quantile ribbons.
#
#   lower  three spread distributions on one axis, showing that between-member
#          parameter uncertainty cancels within pairs.
#
# The lower sub-plot is NOT a signal-to-noise panel and is never labelled as
# one. The manuscript's SNR (biomass_slope_snr_mean_med.R) divides by a temporal
# SD of the unexploited ensemble-mean trajectory over 1841-2010 and answers a
# time-of-emergence question. This panel divides by nothing: it compares
# across-member spread against within-pair spread, which is a parameter-
# uncertainty question. Conflating the two would put two different quantities
# called "SNR" in the same manuscript. No scalar summary is annotated here.
#
# Data (pre-cached; the ensemble is never re-run):
#   Manuscript data/nbss_slope_top10pct_data.rds   lambda, 212 matched pairs
#   Manuscript data/biomass_top10pct_raw_fish.rds  ) total community biomass,
#   Manuscript data/biomass_top10pct_raw_clim.rds  ) for metric = "biomass"
#
# Exposes panel_c(); no side effects, no printing, no file writing.
###############################################################################

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)
  library(ggdist)
})

TERMINAL_YEAR_C <- 2010   # last year common to both ensembles (unexploited runs to 2011)
PLOT_YEARS_C    <- 1841:2010

# ---------------------------------------------------------------------------
# Data assembly -- returns one long table: member, Year, E, U
# ---------------------------------------------------------------------------
.panel_c_pairs <- function(metric, data_dir = DATA_DIR) {
  if (metric == "lambda") {
    f <- file.path(data_dir, "nbss_slope_top10pct_data.rds")
    if (!file.exists(f)) stop("Missing: ", f)
    obj <- readRDS(f)
    d <- obj$all_slopes %>%
      dplyr::filter(spectrum_type == "Biomass") %>%
      dplyr::select(member = sim_id, Year = time, ensemble, slope) %>%
      tidyr::pivot_wider(names_from = ensemble, values_from = slope,
                         values_fn = mean) %>%
      dplyr::rename(E = Exploited, U = Unexploited)
    attr(d, "n_declared") <- obj$n_sims
    attr(d, "method")     <- obj$method

  } else if (metric == "biomass") {
    ff <- file.path(data_dir, "biomass_top10pct_raw_fish.rds")
    fc <- file.path(data_dir, "biomass_top10pct_raw_clim.rds")
    if (!file.exists(ff) || !file.exists(fc)) stop("Missing biomass caches in ", data_dir)
    e <- readRDS(ff) %>% dplyr::group_by(sim_i, Year) %>%
      dplyr::summarise(E = sum(Biomass, na.rm = TRUE), .groups = "drop")
    u <- readRDS(fc) %>% dplyr::group_by(sim_i, Year) %>%
      dplyr::summarise(U = sum(Biomass, na.rm = TRUE), .groups = "drop")
    d <- dplyr::inner_join(e, u, by = c("sim_i", "Year")) %>%
      dplyr::rename(member = sim_i)
    attr(d, "n_declared") <- length(unique(d$member))
    attr(d, "method")     <- "total community biomass"
  } else stop("Unknown metric: ", metric)

  d <- d %>%
    dplyr::filter(!is.na(E), !is.na(U), Year %in% PLOT_YEARS_C) %>%
    dplyr::arrange(member, Year)

  # Verify the fitted ensemble really is n = 212, in code, per the spec.
  n_members <- dplyr::n_distinct(d$member)
  if (n_members != 212)
    stop(sprintf("Expected the fitted ensemble n = 212, found %d members.", n_members))

  # Exploited and unexploited must be identical by construction before the first
  # catch year. Check rather than cosmetically force it.
  pre <- d %>% dplyr::filter(Year < ERA_ONSETS[["Whaling starts"]])
  rel <- max(abs(pre$E - pre$U) / pmax(abs(pre$U), .Machine$double.eps), na.rm = TRUE)
  if (rel > 1e-8)
    warning(sprintf(paste("Pre-exploitation divergence: max relative |E-U| = %.3g before %d.",
                          "The paired runs should be identical by construction -- investigate."),
                    rel, ERA_ONSETS[["Whaling starts"]]))
  attr(d, "pre_divergence") <- rel
  d
}

# ---------------------------------------------------------------------------
# Upper sub-plot: trajectories in native units
# ---------------------------------------------------------------------------
.panel_c_upper <- function(pairs, metric, y_lab) {
  qs <- pairs %>%
    tidyr::pivot_longer(c(E, U), names_to = "scenario", values_to = "val") %>%
    dplyr::group_by(scenario, Year) %>%
    dplyr::summarise(
      med = median(val, na.rm = TRUE),
      lo50 = quantile(val, 0.25, na.rm = TRUE), hi50 = quantile(val, 0.75, na.rm = TRUE),
      lo90 = quantile(val, 0.05, na.rm = TRUE), hi90 = quantile(val, 0.95, na.rm = TRUE),
      .groups = "drop") %>%
    dplyr::mutate(scenario = factor(scenario, levels = c("U", "E"),
                                    labels = c("Unexploited", "Exploited")))

  sc_cols <- c(Unexploited = col_unexploited, Exploited = col_exploited)
  sc_ltys <- c(Unexploited = lty_unexploited, Exploited = lty_exploited)

  # Terminal-year gap between the two medians, for the deficit bracket.
  tt <- qs %>% dplyr::filter(Year == TERMINAL_YEAR_C)
  y_e <- tt$med[tt$scenario == "Exploited"]
  y_u <- tt$med[tt$scenario == "Unexploited"]
  x_end <- TERMINAL_YEAR_C
  x_br  <- x_end + 2.5           # bracket sits just outside the data
  yrng  <- range(c(qs$lo90, qs$hi90), na.rm = TRUE)

  p <- ggplot(qs, aes(x = Year)) +
    geom_ribbon(aes(ymin = lo90, ymax = hi90, fill = scenario), alpha = 0.16) +
    geom_ribbon(aes(ymin = lo50, ymax = hi50, fill = scenario), alpha = 0.30) +
    geom_vline(xintercept = as.numeric(ERA_ONSETS), linetype = "dashed",
               colour = "grey50", linewidth = 0.25) +
    geom_line(aes(y = med, colour = scenario, linetype = scenario), linewidth = 0.45) +
    scale_fill_manual(values = sc_cols, name = NULL) +
    scale_colour_manual(values = sc_cols, name = NULL) +
    scale_linetype_manual(values = sc_ltys, name = NULL) +
    scale_x_continuous(breaks = seq(1850, 2010, by = 40),
                       expand = expansion(mult = c(0.01, 0.10))) +
    labs(y = y_lab, x = NULL) +
    theme_science() +
    theme(legend.position      = c(0.015, 0.045),
          legend.justification = c(0, 0),
          legend.key.width     = unit(0.45, "cm"),
          legend.margin        = margin(1, 2, 1, 2),
          legend.background    = element_rect(fill = "white", colour = NA)) +
    coord_cartesian(clip = "off")

  # Era onset labels, at the top of the panel. Wrapped to two lines so they do
  # not collide at the final 9 cm panel width (same wording as the results figure).
  p <- p + annotate("text", x = as.numeric(ERA_ONSETS) - 2, y = yrng[2],
                    label = sub(" ", "\n", names(ERA_ONSETS)),
                    hjust = 1, vjust = 0.9,
                    size = ANNOT_SIZE_PT / .pt, colour = "grey40", lineheight = 0.9)

  # Terminal-year deficit bracket: double-headed segment spanning the two medians.
  p <- p +
    annotate("segment", x = x_br, xend = x_br, y = y_u, yend = y_e,
             colour = "grey20", linewidth = 0.3,
             arrow = arrow(length = unit(0.035, "cm"), ends = "both", type = "closed")) +
    annotate("text", x = x_br + 1.5, y = (y_u + y_e) / 2,
             label = "Ecological\ndeficit", hjust = 0, vjust = 0.5,
             size = ANNOT_SIZE_PT / .pt, colour = "grey20", lineheight = 0.9)
  p
}

# ---------------------------------------------------------------------------
# Lower sub-plot: across-member spread vs within-pair spread
# ---------------------------------------------------------------------------
.panel_c_lower <- function(pairs, metric, x_lab) {
  tt <- pairs %>% dplyr::filter(Year == TERMINAL_YEAR_C)

  if (metric == "lambda") {
    # lambda is negative, so log ratios are undefined: use raw differences
    # referenced to the median unexploited lambda.
    m <- median(tt$U, na.rm = TRUE)
    rows <- dplyr::bind_rows(
      data.frame(row = "Exploited",            value = tt$E - m),
      data.frame(row = "Unexploited",          value = tt$U - m),
      data.frame(row = "Within-pair difference", value = tt$E - tt$U)
    )
  } else {
    # biomass is strictly positive: log units relative to median unexploited.
    m <- median(log(tt$U), na.rm = TRUE)
    rows <- dplyr::bind_rows(
      data.frame(row = "Exploited",            value = log(tt$E) - m),
      data.frame(row = "Unexploited",          value = log(tt$U) - m),
      data.frame(row = "Within-pair difference", value = log(tt$E) - log(tt$U))
    )
  }

  lvl <- c("Within-pair difference", "Unexploited", "Exploited")  # bottom-up
  rows$row <- factor(rows$row, levels = lvl)
  row_cols <- c("Exploited" = col_exploited, "Unexploited" = col_unexploited,
                "Within-pair difference" = col_paired)
  row_labs <- c("Exploited"               = "Exploited\n(across-member)",
                "Unexploited"             = "Unexploited\n(across-member)",
                "Within-pair difference"  = "E − U\n(within-pair)")

  ggplot(rows, aes(x = value, y = row, fill = row, colour = row)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey45", linewidth = 0.3) +
    ggdist::stat_halfeye(
      adjust = 0.9, height = 0.72, slab_alpha = 0.45,
      .width = c(0.5, 0.9), point_size = 0.7, interval_size_range = c(0.25, 0.6),
      slab_linewidth = 0.25
    ) +
    scale_fill_manual(values = row_cols, guide = "none") +
    scale_colour_manual(values = row_cols, guide = "none") +
    scale_y_discrete(labels = row_labs, expand = expansion(add = c(0.35, 0.55))) +
    labs(x = x_lab, y = NULL) +
    theme_science() +
    theme(axis.text.y = element_text(size = BASE_SIZE_PT - 1, lineheight = 0.9,
                                     colour = "grey10"),
          axis.line.y = element_blank(), axis.ticks.y = element_blank())
}

# ---------------------------------------------------------------------------
# panel_c()
# ---------------------------------------------------------------------------
panel_c <- function(metric = c("lambda", "biomass"), data_dir = DATA_DIR) {
  metric <- match.arg(metric)
  pairs  <- .panel_c_pairs(metric, data_dir)

  if (metric == "lambda") {
    # The qualifier that used to sit under the axis title is dropped: the row
    # labels already carry "(across-member)" and "(within-pair)", and at final
    # figure size the second line ran under the collected legend.
    y_lab <- expression(paste("Size-spectrum slope ", lambda))
    x_lab <- expression(paste("Difference in ", lambda, " (slope units)"))
  } else {
    y_lab <- "Total community biomass (g)"
    x_lab <- "Log difference in biomass"
  }

  up <- .panel_c_upper(pairs, metric, y_lab)
  lo <- .panel_c_lower(pairs, metric, x_lab)

  (up / lo) + patchwork::plot_layout(heights = c(1.45, 1))
}
