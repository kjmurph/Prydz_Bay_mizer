###############################################################################
# 01_theme_fig1.R -- shared style layer for Figure 1 (Prydz Bay, Science)
#
# Provides: theme_science(), the taxon colour vector shared by panels A and B,
# the scenario colours for panel C, and the era-onset years derived from the
# model effort forcing.
#
# Sourced by 02_panel_a.R / 03_panel_b.R / 04_panel_c.R / 05_assemble_fig1.R.
# Run everything from the repository root.
#
# Nothing here reads or writes the ensemble. No side effects beyond defining
# objects (the era-onset derivation reads effort_array_1841_2010.rds).
###############################################################################

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

# ---------------------------------------------------------------------------
# Journal format
# ---------------------------------------------------------------------------
FIG1_WIDTH_CM  <- 18.4   # three-column width, design target
FIG1_MAXHEIGHT <- 22.7   # hard ceiling
BASE_SIZE_PT   <- 7      # nothing below 6 pt after scaling

# Journal floor for in-panel annotation text. Every annotate("text", ...) in the
# panel scripts uses this, so no label can drift under 6 pt. Divide by .pt when
# passing to ggplot's `size` argument, which is in mm.
ANNOT_SIZE_PT  <- 6

# Arial is present on this machine; fall back to the generic sans mapping if a
# build ever runs somewhere it is not.
FIG1_FAMILY <- local({
  fams <- tryCatch(unique(systemfonts::system_fonts()$family), error = function(e) character())
  if ("Arial" %in% fams) "Arial" else if ("Helvetica" %in% fams) "Helvetica" else "sans"
})

# ---------------------------------------------------------------------------
# theme_science()
#
# Adapted from theme_prydz() (the Nature-portfolio theme in
# plot_baleen_diet_composition.R et al.), which is left untouched. Differences:
# white background enforced throughout, gridlines dropped to match the results
# figure, and tag styling set for patchwork's uppercase bold (A)/(B)/(C).
# ---------------------------------------------------------------------------
theme_science <- function(base_size = BASE_SIZE_PT, base_family = FIG1_FAMILY) {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      plot.background   = element_rect(fill = "white", colour = NA),
      panel.background  = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key        = element_rect(fill = "white", colour = NA),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.line         = element_line(colour = "grey20", linewidth = 0.3),
      axis.ticks        = element_line(colour = "grey20", linewidth = 0.3),
      axis.text         = element_text(size = base_size - 1, colour = "grey10"),
      axis.title        = element_text(size = base_size, colour = "grey10"),
      legend.title      = element_text(size = base_size, colour = "grey10"),
      legend.text       = element_text(size = base_size - 1, colour = "grey10"),
      legend.key.size   = unit(0.30, "cm"),
      strip.background  = element_blank(),
      strip.text        = element_text(size = base_size, face = "bold", colour = "grey10"),
      plot.title        = element_blank(),      # descriptions live in the legend
      plot.subtitle     = element_blank(),
      plot.tag          = element_text(size = base_size + 2, face = "bold", colour = "black"),
      plot.margin       = margin(3, 3, 3, 3, "pt")
    )
}

# ---------------------------------------------------------------------------
# Taxon palette -- SHARED by panels A and B
#
# Okabe-Ito (colourblind-safe) for the taxa that carry the figure. The three
# minor catch groups take muted non-Okabe hues so that no taxon occupies a
# neutral grey or a deep red, which are reserved for the panel C scenarios.
# ---------------------------------------------------------------------------
# Taken from `panel_colors` in abundance_meanweight_rmse_top10pct_grid.R, the
# script behind manuscript Figure 3 (pctchange_1col_sidebyside.png), so a taxon
# carries the same colour in Figures 1 and 3. That script aggregates the 19
# modelled groups into 12 panels via group_defs + ind_to_panel (lines 36-52);
# the mapping is reproduced here at species level, so several species share a
# colour exactly as they share a panel in Figure 3.
#
# NOTE: this palette is the ggplot hue family, not Okabe-Ito, so it is NOT
# colourblind-safe -- it trades spec section 5's accessibility requirement for
# cross-figure consistency. Every colour distinction in Figure 1 is redundantly
# encoded (line type, line weight, silhouette shape, position on the mass axis),
# so nothing is lost in greyscale, but the legend keys themselves are not
# separable for a deuteranope.
TAXON_COLOURS <- c(
  # individually panelled in Figure 3
  "baleen whales"            = "#FF61C3",  # Large baleen whales
  "sperm whales"             = "#DB72FB",  # Sperm whales
  "minke whales"             = "#00B9E3",  # Minke whales
  "orca"                     = "#619CFF",  # Orca
  "leopard seals"            = "#E07B39",  # Leopard seals
  "toothfishes"              = "#00C19F",  # Toothfishes
  "shelf and coastal fishes" = "#93AA00",  # Shelf & coastal fishes
  "antarctic krill"          = "#F8766D",  # Antarctic krill
  # aggregated panels in Figure 3
  "medium divers"            = "#2B6CB0",  # Pinnipeds
  "large divers"             = "#2B6CB0",  # Pinnipeds
  "flying birds"             = "#9E9E9E",  # Seabirds
  "small divers"             = "#9E9E9E",  # Seabirds
  "mesopelagic fishes"       = "#D39200",  # Pelagic fishes & squid
  "bathypelagic fishes"      = "#D39200",  # Pelagic fishes & squid
  "squids"                   = "#D39200",  # Pelagic fishes & squid
  "mesozooplankton"          = "#6A1B9A",  # Zooplankton
  "other krill"              = "#6A1B9A",  # Zooplankton
  "other macrozooplankton"   = "#6A1B9A",  # Zooplankton
  "salps"                    = "#6A1B9A"   # Zooplankton
)

# Display labels (manuscript wording, as used in the figure being replaced).
# Display labels, matching Figure 3's panel names where a taxon is panelled
# there individually.
TAXON_LABELS <- c(
  "baleen whales"            = "Large baleen whales",
  "sperm whales"             = "Sperm whales",
  "minke whales"             = "Minke whales",
  "orca"                     = "Orca",
  "leopard seals"            = "Leopard seals",
  "toothfishes"              = "Toothfishes",
  "shelf and coastal fishes" = "Shelf & coastal fishes",
  "antarctic krill"          = "Antarctic krill",
  "medium divers"            = "Pinnipeds",
  "large divers"             = "Pinnipeds",
  "flying birds"             = "Seabirds",
  "small divers"             = "Seabirds",
  "mesopelagic fishes"       = "Pelagic fishes & squid",
  "bathypelagic fishes"      = "Pelagic fishes & squid",
  "squids"                   = "Pelagic fishes & squid",
  "mesozooplankton"          = "Zooplankton",
  "other krill"              = "Zooplankton",
  "other macrozooplankton"   = "Zooplankton",
  "salps"                    = "Zooplankton"
)

# Taxa that appear in the panel A legend. Panel B colours exactly these
# silhouettes and group lines, and leaves everything else neutral grey.
#
# These are the taxa carrying non-negligible observed catch. Toothfishes and
# shelf & coastal fishes are included because both are genuinely fished
# (16.7 kt and 0.09 kt cumulative), even though they are dwarfed by the whales.
# Squids (2.4 t) and bathypelagic fishes (0.7 g) also appear in the catch record
# but are too small to render or to justify a legend key.
PANEL_A_LEGEND_TAXA <- c("baleen whales", "sperm whales", "orca",
                         "minke whales", "antarctic krill",
                         "shelf and coastal fishes", "toothfishes")

SILHOUETTE_GREY <- "grey55"

# ---------------------------------------------------------------------------
# Scenario colours -- panel C only. Deliberately outside the taxon palette:
# no taxon is grey, deep red, or teal.
# Colour is never the only cue; the median lines also differ in line type.
# ---------------------------------------------------------------------------
col_exploited   <- "#B2182B"   # deep red; darker than krill's #F8766D
col_unexploited <- "#404040"   # near-black grey; darker than Seabirds' #9E9E9E
col_paired      <- "#01665E"   # dark teal, for the within-pair difference

lty_exploited   <- "solid"
lty_unexploited <- "22"

# ---------------------------------------------------------------------------
# Era onsets -- DERIVED from the model effort forcing, not hard-coded.
#
# effort_array_1841_2010.rds is [year x species]; onset = first year with
# non-zero effort. The results figure (biomass_slope_snr_mean_med.R) hard-codes
# KEY_YEARS = c(1930, 1974); we derive and then assert equality so any drift in
# the forcing fails loudly instead of silently disagreeing between figures.
#
# Peak-effort years are derived here too for provenance, but Figure 1 panel A
# does NOT draw them: panel A plots the observed catch curve directly, and the
# effort-derived peak for sperm whales (1948) is three years off the observed
# catch peak (1951), so the marker would read as an error against its own data.
# The results figure keeps all six markers and is unchanged.
# ---------------------------------------------------------------------------
derive_era_years <- function(effort_path = "effort_array_1841_2010.rds") {
  if (!file.exists(effort_path))
    stop("Effort forcing not found: ", effort_path,
         " -- run from the repository root.")
  ea   <- readRDS(effort_path)
  yrs  <- as.numeric(rownames(ea))
  per_gear <- lapply(colnames(ea), function(sp) {
    v <- ea[, sp]
    if (!any(v > 0)) return(NULL)
    data.frame(gear  = sp,
               onset = yrs[which(v > 0)[1]],
               peak  = yrs[which.max(v)],
               stringsAsFactors = FALSE)
  })
  per_gear <- do.call(rbind, per_gear)

  whaling_start <- min(per_gear$onset)                                  # 1930
  krill_start   <- per_gear$onset[per_gear$gear == "antarctic krill"]   # 1974

  # Assert against the values the results figure draws.
  stopifnot(
    "derived whaling onset != 1930 used by the results figure" =
      whaling_start == 1930,
    "derived krill onset != 1974 used by the results figure" =
      krill_start == 1974
  )
  peaks <- setNames(per_gear$peak, per_gear$gear)
  stopifnot(
    "derived peak-effort years != those used by the results figure" =
      all(peaks[c("baleen whales", "sperm whales",
                  "minke whales", "antarctic krill")] == c(1933, 1948, 1973, 1979))
  )

  list(
    onsets = c("Whaling starts" = whaling_start, "Krill fishing starts" = krill_start),
    peaks  = peaks,
    table  = per_gear[order(per_gear$onset), ]
  )
}

ERA <- derive_era_years()
ERA_ONSETS <- ERA$onsets    # the only era markers panel A draws

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------
# Body-mass axis labels in the convention of the figure being replaced
# (1 g / 10 g / ... / 1 t / 100 t).
mass_label <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (v >= 1e6) sprintf("%g t",  v / 1e6)
    else if (v >= 1e3) sprintf("%g kg", v / 1e3)
    else sprintf("%g g", v)
  }, character(1))
}

DATA_DIR <- "Manuscript data"
OUT_DIR  <- "output/figures"
