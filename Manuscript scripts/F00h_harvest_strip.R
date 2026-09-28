# =============================================================================
# F00h -- the harvest strip: the whaling / krill-fishing bar drawn above the
# panels of Figures 2, 3 and 4.
#
# ONE DEFINITION, sourced by F02_figure2_snr_rebuilt167_v2.R,
# F03_figure3_pctchange_rebuilt167_v2.R and F04_figure4_krill_ratio_rebuilt167_v2.R,
# so the three figures cannot drift apart in what the bar encodes.
#
# WHAT THE BAR SHOWS: OBSERVED CATCH WEIGHT (Kieran, 2026-09-26), read from
# yield_observed_timeseries.csv (grams per year, one column per species). Each
# row sums its species' catch in each year and is scaled to THAT ROW'S OWN PEAK,
# so the shading says when each activity happened and how heavy it was relative
# to its own maximum -- not how the two activities compare in magnitude (whaling
# peaked at 499.6 kt in 1933, krill fishing at 36.2 kt in 1979).
#
# It replaces the EFFORT strip of the first Figure 2 v2, which scaled each whale
# gear's effort to its own peak and took the max across gears. That drew the 1973
# minke-whaling peak at full intensity although it landed 30.2 kt, 6% of the
# 1933 whale catch, and drew 2006 at 0.79 for 3.8 kt.
#
# The rows are the same as before: whaling = baleen + sperm + minke whales (orca
# is not whaling here, as in the effort strip; its total catch is 1.8 kt of
# 5,036 kt), krill fishing = antarctic krill.
# =============================================================================

HARVEST_CSV <- "yield_observed_timeseries.csv"
HARVEST_ACTIVITY <- list("Whaling"       = c("baleen whales", "sperm whales",
                                             "minke whales"),
                         "Krill fishing" = "antarctic krill")
# Full-intensity colours, each row ramping from white. Identical to Figure 2's
# ACT_COL -- see the palette note there for why these two hues.
HARVEST_COL <- c("Whaling" = "#B48A4C", "Krill fishing" = "#5E9AC0")

# Year x activity matrix of catch weight scaled to each activity's own peak (0-1).
harvest_intensity <- function(csv = HARVEST_CSV, activity = HARVEST_ACTIVITY) {
  obs  <- read.csv(csv, check.names = FALSE)
  miss <- setdiff(unlist(activity), names(obs))
  if (length(miss)) stop("catch file lacks column(s): ",
                         paste(miss, collapse = ", "), call. = FALSE)
  if (anyNA(obs[unlist(activity)]))
    stop("catch file has NA catches -- refusing to read them as zero", call. = FALSE)
  m <- sapply(activity, function(cols) {
    v <- rowSums(obs[, cols, drop = FALSE])
    if (max(v) > 0) v / max(v) else 0 * v
  })
  m <- matrix(m, nrow = nrow(obs), dimnames = list(obs$Year, names(activity)))
  # The spans a caption would quote. A trace catch is why ">0" alone overstates a
  # period -- report both.
  for (a in colnames(m)) {
    v <- m[, a]; y <- obs$Year
    message(sprintf("%-14s catch > 0: %d-%d | >= 10%% of peak: %d-%d | peak %d",
                    a, min(y[v > 0]), max(y[v > 0]), min(y[v >= 0.1]),
                    max(y[v >= 0.1]), y[which.max(v)]))
  }
  m
}

# One tile per activity-year with positive catch inside `years`, fill ramping
# from white at zero to act_col at the activity's peak. `row` puts the first
# activity on top.
harvest_strip_data <- function(years, act_col = HARVEST_COL, ...) {
  I  <- harvest_intensity(...)
  yr <- as.numeric(rownames(I))
  n  <- ncol(I)
  out <- do.call(rbind, lapply(seq_len(n), function(i) {
    a <- colnames(I)[i]; v <- I[, a]; keep <- yr %in% years & v > 0
    ramp <- colorRampPalette(c("white", act_col[[a]]), space = "Lab")(101)
    data.frame(act = a, row = n + 1 - i, Year = yr[keep],
               fill = ramp[round(100 * v[keep]) + 1])
  }))
  attr(out, "n_rows") <- n
  out
}

# The strip as a ggplot, transcribed from Figure 2's harvest_strip() so the bars
# look the same in every figure. Each row's label sits just before its first
# tile, right-aligned; `x_scale` must be the scale of the panels below, so that
# patchwork's alignment puts each tile over its year.
harvest_strip_plot <- function(strip, x_scale, xlim, label_pt = 8,
                               plot_margin = ggplot2::margin(1, 4, 1, 2)) {
  n   <- attr(strip, "n_rows")
  lab <- do.call(rbind, lapply(split(strip, strip$act), function(d)
    data.frame(label = d$act[1], row = d$row[1], x = min(d$Year) - 1.5)))
  ggplot2::ggplot(strip) +
    ggplot2::geom_rect(ggplot2::aes(xmin = pmax(Year - 0.5, xlim[1]),
                                    xmax = pmin(Year + 0.52, xlim[2]),  # no seams
                                    ymin = row - 0.4, ymax = row + 0.4, fill = fill),
                       colour = NA) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_text(data = lab, ggplot2::aes(x = x, y = row, label = label),
                       hjust = 1, size = label_pt / ggplot2::.pt, colour = "grey20") +
    x_scale +
    ggplot2::scale_y_continuous(limits = c(0.5, n + 0.5), expand = c(0, 0)) +
    ggplot2::coord_cartesian(clip = "off") + ggplot2::theme_void() +
    ggplot2::theme(plot.margin = plot_margin)
}
