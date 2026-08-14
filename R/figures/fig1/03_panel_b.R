###############################################################################
# 03_panel_b.R -- Figure 1 panel B: the realised community size spectrum,
#                 decomposed into functional groups
#
# Layers, bottom to top:
#   1  a single light tint over the large-marine-mammal size range, derived from
#      the data (first bin where any whale group carries biomass)
#   2  the 19 modelled functional groups, UNEXPLOITED only, as thin lines
#   3  the community spectrum for BOTH scenarios, as thick lines -- this is what
#      makes the large-mammal decline visible as data rather than inference
#   4  the fitted community slope lambda (exploited), with both values annotated
#   5  PhyloPic silhouettes at each group's peak body mass, read from the local
#      cache only (never the network)
#
# PERIOD: the reference period, the 2001-2010 MEAN. Both caches are built by
# create_fished_vs_unfished_ratio_plot_FIXED_v6.R with ref_years <- 2001:2010,
# averaged over those years per member (extract_community_spectrum /
# extract_species_spectrum), and this panel then takes the median across the 212
# fitted members. State the period in the figure legend.
#
# QUANTITY PLOTTED: native-resolution biomass density n(w)*w on the model's own
# 100 mizer bins. This is deliberately NOT the octave-binned LBNbiom spectrum:
# the large mammals occupy very few bins (baleen whales 2, sperm whales 4), and
# octave re-binning merges exactly the structure the panel exists to show.
#
# SLOPE: lambda remains the canonical LBNbiom value (Edwards et al. 2017 Method
# 5), the same estimator behind nbss_slope_top10pct_data.rds and the manuscript's
# slope time series -- NOT an OLS through the plotted points. The two live on the
# same scale (native log10 range 0.83-17.46 slope -1.089; octave 0.90-17.50 slope
# -1.069), so the line sits on the data without an offset, but it is ~0.02
# shallower than the native points' own trend, drifting ~0.3 log units across the
# full axis. The figure legend must say the line is the manuscript's estimator.
#
# CAVEAT for the legend: community medians and group medians are each taken
# across the 212 members independently, so the group lines do NOT sum exactly to
# the community line (median of a sum != sum of medians). This matches the
# convention in compute_fg_biomass_df() of yield_ratio_combined_panels.R, so it
# is consistent with the rest of the manuscript, but it is not an exact
# decomposition and should not be presented as one.
#
# Data (pre-cached; the ensemble is never re-run):
#   Manuscript data/spectra_cache_ref_period.rds          community n(w), 2001-2010
#   Manuscript data/spectra_cache_species_ref_period.rds  per-group n(w), both scenarios
#   Manuscript data/nbss_slope_top10pct_data.rds          fitted-member indices, min_w
#   Manuscript data/params_sel_adj.rds                    dw
#
# Exposes panel_b(); no side effects, no printing, no file writing.
###############################################################################

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(rphylopic)
})

PHYLOPIC_CACHE <- "R/figures/fig1/phylopic_cache"

# The four groups whose size range defines the large-marine-mammal tint.
WHALE_GROUPS <- c("minke whales", "orca", "sperm whales", "baleen whales")

# Local axis formatter. mass_label() in 01_theme_fig1.R renders "1e-08 g", which
# is unreadable across 15.5 decades; powers of ten are cleaner. Kept local so the
# shared theme is untouched.
.mass_pow_labels <- function(x) parse(text = paste0("10^", round(log10(x))))

# ---------------------------------------------------------------------------
# LBNbiom, transcribed from calculate_spectrum_lbnbiom_slope()
# (ecosystem_assessment_v3.R). Serves lambda only -- it no longer supplies the
# plotted data layer.
# ---------------------------------------------------------------------------
.lbnbiom <- function(nw, w, dw, min_w) {
  w_filt <- w[w >= min_w]
  if (length(w_filt) < 3) return(NULL)
  breaks <- 2^(floor(log2(min(w_filt))):ceiling(log2(max(w_filt))))
  nb <- length(breaks) - 1L
  if (nb < 3) return(NULL)
  bin_idx <- findInterval(w, breaks, rightmost.closed = TRUE)
  bpb <- nw * w * dw                       # biomass per native mizer bin
  oct <- numeric(nb)
  for (j in seq_along(w)) {
    b <- bin_idx[j]
    if (b >= 1L && b <= nb && w[j] >= min_w) oct[b] <- oct[b] + bpb[j]
  }
  mid <- sqrt(breaks[-length(breaks)] * breaks[-1])
  nbs <- oct / diff(breaks)                # normalise by linear bin width
  ok  <- which(nbs > 0 & is.finite(nbs))
  if (length(ok) < 3) return(NULL)
  fit <- stats::lm(log10(nbs[ok]) ~ log10(mid[ok]))
  list(w = mid[ok], nbs = nbs[ok],
       slope = unname(coef(fit)[2]), intercept = unname(coef(fit)[1]),
       r2 = summary(fit)$r.squared)
}

# ---------------------------------------------------------------------------
# Silhouettes: cache only. Anything missing from the cache is simply omitted --
# never substituted with a different taxon.
# ---------------------------------------------------------------------------
.load_silhouettes <- function(keys) {
  out <- list()
  for (k in keys) {
    f <- file.path(PHYLOPIC_CACHE, paste0(gsub("[^a-z0-9]+", "_", k), ".rds"))
    out[[k]] <- if (file.exists(f)) readRDS(f) else NULL
  }
  out
}

# Ensemble-median biomass density n(w)*w for one group under one scenario.
.group_density <- function(ssp, scenario, nm, idx, w) {
  m <- ssp[[scenario]][[nm]][idx, , drop = FALSE]
  apply(m, 2, median, na.rm = TRUE) * w
}

# ---------------------------------------------------------------------------
# panel_b()
# ---------------------------------------------------------------------------
panel_b <- function(data_dir = DATA_DIR, silhouettes = TRUE) {

  sc  <- readRDS(file.path(data_dir, "spectra_cache_ref_period.rds"))
  ns  <- readRDS(file.path(data_dir, "nbss_slope_top10pct_data.rds"))
  ssp <- readRDS(file.path(data_dir, "spectra_cache_species_ref_period.rds"))
  po  <- readRDS(file.path(data_dir, "params_sel_adj.rds"))
  w   <- sc$w_bins
  dw  <- po@dw
  idx <- ns$top10pct_idx
  min_w <- ns$spectrum_min_w
  if (length(idx) != 212)
    stop(sprintf("Expected 212 fitted members, found %d.", length(idx)))

  # --- community spectra, both scenarios -----------------------------------
  med_f <- apply(sc$fished_spectra[idx, , drop = FALSE],  2, median, na.rm = TRUE)
  med_c <- apply(sc$climate_spectra[idx, , drop = FALSE], 2, median, na.rm = TRUE)

  # lambda: canonical LBNbiom, still fitted over the full resolved spectrum
  Lf <- .lbnbiom(med_f, w, dw, min_w)
  Lc <- .lbnbiom(med_c, w, dw, min_w)
  if (is.null(Lf)) stop("LBNbiom failed on the exploited median spectrum.")

  comm <- rbind(
    data.frame(scenario = "Unexploited", w = w, dens = med_c * w,
               colour = col_unexploited, lty = lty_unexploited,
               stringsAsFactors = FALSE),
    data.frame(scenario = "Exploited",   w = w, dens = med_f * w,
               colour = col_exploited,   lty = lty_exploited,
               stringsAsFactors = FALSE)
  )
  comm <- comm[is.finite(comm$dens) & comm$dens > 0, , drop = FALSE]

  # --- per-group spectra, unexploited only ---------------------------------
  grp <- lapply(ssp$sp_names, function(nm) {
    d  <- .group_density(ssp, "climate_sp", nm, idx, w)
    mx <- suppressWarnings(max(d, na.rm = TRUE))
    if (!is.finite(mx) || mx <= 0) return(NULL)
    # clip the near-zero tails so lines do not run off to -Inf
    k <- which(is.finite(d) & d > mx * 1e-4)
    if (length(k) < 2) return(NULL)
    data.frame(group = nm, w = w[k], dens = d[k], stringsAsFactors = FALSE)
  })
  grp <- do.call(rbind, grp)

  # Same colour rule as the silhouettes: the five panel A taxa take their shared
  # taxon colour, every other group is neutral grey. No separate legend needed --
  # those five colours are collected at figure level from panel A.
  # Every group now carries a colour, keyed to manuscript Figure 3. Species that
  # share a Figure 3 panel share its colour, so the four zooplankton groups and
  # the three pelagic fish/squid groups each read as one band.
  grp$colour <- unname(TAXON_COLOURS[grp$group])
  grp$colour[is.na(grp$colour)] <- SILHOUETTE_GREY

  # --- plotted window -------------------------------------------------------
  w_min_plot <- min(w)
  w_max_plot <- max(w) * 1.9          # right margin for the whale silhouettes

  y_all <- log10(c(comm$dens, grp$dens))
  y_rng <- range(y_all[is.finite(y_all)], na.rm = TRUE)
  y_top <- y_rng[2] + diff(y_rng) * 0.10
  # Headroom below the data for the large baleen whale silhouette, which sits
  # under the large-marine-mammal spectra rather than to their left.
  # Also carries the lambda annotation block, which is moved clear of the data:
  # the penguin and squid silhouettes land at sub-milligram masses (their spectra
  # start at egg size), which is exactly where the text used to sit.
  y_bot <- y_rng[1] - diff(y_rng) * 0.165

  # --- large-marine-mammal tint, derived from the data ----------------------
  whale_start <- suppressWarnings(min(vapply(WHALE_GROUPS, function(nm) {
    d <- .group_density(ssp, "climate_sp", nm, idx, w)
    k <- which(is.finite(d) & d > 0)
    if (length(k)) w[min(k)] else NA_real_
  }, numeric(1)), na.rm = TRUE))

  p <- ggplot()

  if (is.finite(whale_start))
    p <- p + annotate("rect", xmin = whale_start, xmax = w_max_plot,
                      ymin = -Inf, ymax = Inf, fill = "#7bafd4", alpha = 0.22) +
      annotate("text", x = sqrt(whale_start * w_max_plot),
               y = y_rng[2] + diff(y_rng) * 0.045,
               label = "Large marine\nmammals", hjust = 0.5, vjust = 1,
               size = ANNOT_SIZE_PT / .pt, colour = "grey25",
               lineheight = 0.85)

  # 2 -- functional groups, unexploited
  p <- p + geom_line(data = grp,
                     aes(x = w, y = log10(dens), group = group, colour = colour),
                     linewidth = 0.22, alpha = 0.85)

  # 3 -- community spectrum, both scenarios, both dashed.
  # The two coincide over almost the whole spectrum and separate only at the
  # largest body masses -- that IS the result, so it must not read as a
  # rendering artefact. Different dash patterns (not just colour) let both stay
  # legible where they overlap, and survive greyscale.
  p <- p +
    geom_line(data = comm[comm$scenario == "Unexploited", , drop = FALSE],
              aes(x = w, y = log10(dens)),
              colour = col_unexploited, linetype = "22", linewidth = 0.5) +
    geom_line(data = comm[comm$scenario == "Exploited", , drop = FALSE],
              aes(x = w, y = log10(dens)),
              colour = col_exploited, linetype = "62", linewidth = 0.45)

  # 4 -- fitted lambda (exploited only; the unexploited value is annotated)
  #
  # The canonical LBNbiom SLOPE is drawn exactly. The INTERCEPT is refitted to
  # the plotted native spectrum, because the two normalisations differ --
  # octave NBS is biomass / octave width, native density is biomass / dw, and an
  # octave spans several native bins. Transferring Lf$intercept directly puts the
  # line ~2 log units above the data at the small-mass end. Only the slope is a
  # scientific quantity here; the intercept is a display choice.
  # Both scenarios are drawn so the reader can see directly whether the fitted
  # slopes differ. They differ by 0.015 slope units over 15.5 decades, so the two
  # lines are expected to be near-coincident -- that near-coincidence is itself
  # the point, and is why panel C carries the quantitative paired contrast.
  .lam_line <- function(L, scen) {
    fw <- comm$w[comm$scenario == scen & comm$w >= min_w]
    fd <- comm$dens[comm$scenario == scen & comm$w >= min_w]
    b0 <- mean(log10(fd) - L$slope * log10(fw))
    xs <- range(fw)
    data.frame(w = xs, y = b0 + L$slope * log10(xs), stringsAsFactors = FALSE)
  }
  # Solid: both community spectra are dashed, so the straight lines are
  # unambiguously fits rather than further spectra.
  p <- p +
    geom_line(data = .lam_line(Lc, "Unexploited"), aes(x = w, y = y),
              colour = col_unexploited, linewidth = 0.32, alpha = 0.9) +
    geom_line(data = .lam_line(Lf, "Exploited"), aes(x = w, y = y),
              colour = col_exploited, linewidth = 0.32, alpha = 0.9)

  # 5 -- silhouettes, anchored at the START of each group's line and set below
  #      and to its left, so they label the line's origin rather than sit on the
  #      data. Sizes scale with each group's maximum body mass, so the elephant
  #      seal reads larger than the leopard seal and the large baleen whale
  #      larger than every other marine mammal.
  if (silhouettes) {
    sp_wmax <- setNames(mizer::species_params(po)$w_max,
                        as.character(mizer::species_params(po)$species))

    anchors <- lapply(ssp$sp_names, function(nm) {
      d  <- .group_density(ssp, "climate_sp", nm, idx, w)
      mx <- suppressWarnings(max(d, na.rm = TRUE))
      if (!is.finite(mx) || mx <= 0) return(NULL)
      k <- which(is.finite(d) & d > mx * 1e-4)   # same clip as the plotted line
      if (!length(k)) return(NULL)
      j <- min(k)                                 # left-hand end of the line
      data.frame(key = nm, x0 = w[j], y0 = log10(d[j]), stringsAsFactors = FALSE)
    })
    anchors <- do.call(rbind, anchors)
    sil  <- .load_silhouettes(anchors$key)
    have <- vapply(sil, Negate(is.null), logical(1))
    anchors <- anchors[anchors$key %in% names(sil)[have], , drop = FALSE]

    if (nrow(anchors)) {
      # Placement, per group. Two forms:
      #   c(dx, dy)             dx = decades to the LEFT of the line start,
      #                         dy = fraction of the y range DOWN from it.
      #                         dy is 0 for every group but one, so each
      #                         silhouette sits directly left of its own
      #                         spectrum at that spectrum's own height.
      #   c(dx, NA, y_abs)      absolute y. Used only for the large baleen
      #                         whales, which sit directly below the
      #                         large-marine-mammal spectra in the gap above the
      #                         axis -- there is no room to their left, since
      #                         their line starts at the extreme right edge.
      # dx must clear the silhouette's own width: at these heights a silhouette
      # is roughly one decade wide, so offsets below ~0.7 will overlap the line.
      place <- list(
        "mesozooplankton"          = c(-0.05, 0),
        "other krill"              = c( 0.90, 0),
        "other macrozooplankton"   = c( 0.90, 0),
        "antarctic krill"          = c( 0.90, 0),
        "salps"                    = c( 0.85, 0),
        "mesopelagic fishes"       = c( 0.85, 0),
        "bathypelagic fishes"      = c( 0.85, 0),
        "shelf and coastal fishes" = c( 0.90, 0),
        "flying birds"             = c( 0.85, 0),
        "small divers"             = c( 0.90, 0),
        "squids"                   = c( 0.95, 0),
        "toothfishes"              = c( 0.95, 0),
        "leopard seals"            = c( 0.90, 0),
        "medium divers"            = c( 1.05, 0),
        "large divers"             = c( 1.05, 0),
        # right and down, so it reads against its own line rather than the
        # elephant seal's, whose spectrum starts at a similar body mass
        "minke whales"             = c( 0.30, 0.030),
        "orca"                     = c( 1.60, 0),
        "sperm whales"             = c( 1.15, 0),
        "baleen whales"            = c( 0.35, NA,
                                        y_rng[1] - diff(y_rng) * 0.032)
      )
      getp <- function(k, j, default) {
        v <- place[[k]]
        if (is.null(v) || length(v) < j || is.na(v[j])) default else v[j]
      }
      anchors$x <- 10^(log10(anchors$x0) -
                         vapply(anchors$key, getp, numeric(1), 1, 0.45))
      dyf <- vapply(anchors$key, getp, numeric(1), 2, 0.040)
      yab <- vapply(anchors$key, getp, numeric(1), 3, NA_real_)
      anchors$y <- ifelse(is.finite(yab), yab, anchors$y0 - diff(y_rng) * dyf)
      # keep everything inside the panel
      anchors$x <- pmax(anchors$x, w_min_plot * 1.15)
      anchors$colour <- unname(TAXON_COLOURS[anchors$key])
      anchors$colour[is.na(anchors$colour)] <- SILHOUETTE_GREY

      # Height scales with log10(w_max) so relative body size reads correctly --
      # elephant seal larger than leopard seal, baleen whale larger than every
      # other marine mammal. Heavily compressed: the range spans 10 decades of
      # mass, and silhouettes annotate the spectrum rather than compete with it.
      lw  <- log10(sp_wmax[anchors$key])
      rel <- (lw - min(log10(sp_wmax))) / diff(range(log10(sp_wmax)))

      # Per-group correction. Height alone does not give equal apparent size:
      # the PhyloPic images have very different aspect ratios and margins, so a
      # compact leopard seal at the same height reads larger than an elongated
      # elephant seal. These multipliers restore the intended visual ordering --
      # elephant seal > leopard seal, and baleen > sperm > orca > minke.
      size_mult <- c(
        "leopard seals" = 0.70, "medium divers" = 0.80, "large divers" = 1.30,
        "minke whales"  = 0.72, "orca"          = 1.05,
        "sperm whales"  = 1.15, "baleen whales" = 1.45
      )
      mult <- ifelse(anchors$key %in% names(size_mult),
                     size_mult[anchors$key], 1)
      # Raised floor with a shallower slope: everything is a little larger, and
      # the small groups gain most (a copepod at the old floor was barely a
      # smudge at 7 pt) while the whales grow only slightly.
      anchors$h <- diff(y_rng) * (0.022 + 0.016 * rel) * mult

      anchors$img <- I(lapply(anchors$key, function(k) sil[[k]]$img))
      # One layer per group: height and fill must vary per silhouette, and a
      # fixed-parameter layer is more predictable here than mapping them.
      for (i in seq_len(nrow(anchors))) {
        p <- p + rphylopic::geom_phylopic(
          data = anchors[i, , drop = FALSE],
          aes(x = x, y = y, img = img),
          fill = anchors$colour[i], height = anchors$h[i], alpha = 1)
      }
    }
  }

  # lambda annotations, bottom-left where the panel is empty.
  # The qualifier matters: over 15.5 decades the spectrum is not a clean power
  # law (residual sd ~1.07 log units against this line), so the line must read as
  # a whole-spectrum slope reference, not as a local fit to the curve it crosses.
  p <- p +
    annotate("text", x = w_min_plot * 2, y = y_bot + diff(y_rng) * 0.115,
             label = sprintf("lambda[exploited] == %.3f", Lf$slope),
             parse = TRUE, hjust = 0, vjust = 0,
             size = (BASE_SIZE_PT - 1) / .pt, colour = col_exploited) +
    annotate("text", x = w_min_plot * 2, y = y_bot + diff(y_rng) * 0.060,
             label = sprintf("lambda[unexploited] == %.3f", Lc$slope),
             parse = TRUE, hjust = 0, vjust = 0,
             size = (BASE_SIZE_PT - 1) / .pt, colour = col_unexploited) +
    annotate("text", x = w_min_plot * 2, y = y_bot + diff(y_rng) * 0.010,
             label = "LBNbiom slope, whole spectrum",
             hjust = 0, vjust = 0,
             size = ANNOT_SIZE_PT / .pt, colour = "grey40")

  p +
    scale_colour_identity() +
    scale_linetype_identity() +
    scale_x_log10(breaks = 10^seq(-8, 8, by = 2),
                  labels = .mass_pow_labels,
                  expand = expansion(mult = c(0.01, 0.01))) +
    coord_cartesian(xlim = c(w_min_plot, w_max_plot),
                    ylim = c(y_bot, y_top), clip = "off") +
    labs(x = "Body mass (g)",
         y = expression(paste(log[10], " biomass density  (g ", g^-1, ")"))) +
    theme_science()
}
