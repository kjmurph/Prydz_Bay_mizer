# =============================================================================
# FIGURE 2 -- signal-to-noise ratio panels, v2 (drawn for Science).
#
# ENSEMBLE. Defaults to the p104 ensemble: FIG_SUF=p104q10, which with
# FIG_SET=all (the default) keeps all 203 usable members. "rebuilt167" in the
# file name is legacy -- the script was first written for ensemble 44 / cut A,
# still reachable with FIG_SUF=rebuilt167. Canonical invocation is the wrapper,
# which also points FIG_OUT/FIG_SUPP at Manuscript figures/p104 figures/:
#   Rscript run_p104q10.R "Manuscript scripts/F02_figure2_snr_rebuilt167_v2.R"
#   Rscript run_p104q10.R --top "Manuscript scripts/F02_figure2_snr_rebuilt167_v2.R"
# Every number is computed exactly as in v1 (F02_figure2_snr_rebuilt167.R);
# only the drawing changes.
#
#   A  Community biomass            (v1 y-axis: Biomass SNR)
#   B  Biomass stability            (v1: Biomass variability SNR, sign flipped)
#   C  Community size structure     (v1: Size-spectrum slope SNR)
#   D  Size-structure stability     (v1: Slope variability SNR, sign flipped)
# All four are SNRs by construction -- the paired difference over a temporal SD
# -- but from v4 the titles carry no unit; the caption does.
#
# Builds FIVE variants of the 1 x 4 stack, W = 3, 6, 9, 12, 15 yr; the 15 yr
# version is the main figure, the other four go to the Supplement. For the
# main window only it also builds a 2 x 2 variant -- columns "Ecosystem
# structure" (A, C) and "Ecosystem stability" (B, D); letters run row-wise, so
# A-D are the same panels in both layouts -- and a standalone phase plane
# (biomass SNR against size-structure SNR, the median trajectory coloured by
# year, IQR crosses at PP_CROSS_YEARS, the |SNR| < 1 box shaded).
#
# DRAWING CONVENTIONS
#   - Science style: uppercase 9 pt bold panel letters at the upper left; the
#     panel names are the y-axis labels, on one line, with NO unit (v4, Kieran
#     2026-09-27): every panel is the paired difference in units of the
#     counterfactual's natural variability, and the caption says so -- including
#     which SD is the denominator (FIG_SNR) -- so one set of titles serves both
#     the classic and the paired build. No panel titles. The B/D rolling-SD
#     window goes in the caption, so the supplementary window variants differ
#     only in file name.
#   - STABILITY IS AN SD, NOT A CV. roll_sd() is a trailing rolling standard
#     deviation (zoo::rollapplyr(val, k, sd)); B and D are the SNR of that SD,
#     built exactly like A and C. A CV would only be defined for biomass (the
#     slope's mean is negative, and itself shifts under exploitation).
#   - B AND D PLOT STABILITY, NOT VARIABILITY: STAB_SIGN = -1 draws the
#     NEGATIVE of the variability SNR, i.e. (unexploited - exploited rolling
#     SD) / noise. Positive then means the exploited run is less variable, so
#     more stable, and the axis name reads true with an ordinary axis -- down is
#     less stable, the same way as "fewer large animals" in C. This is a sign
#     convention, not a reversed axis: a reversed axis gives the same picture
#     but tick labels that rise downward, a well-known cause of misreading.
#     Negating swaps the quartiles (q25 <- -q75, q75 <- -q25). The CSV and the
#     console keep the v1 VARIABILITY sign. STAB_SIGN = +1 restores v1.
#   - The +/-1 emergence thresholds are one shaded band, marked in every panel
#     by a black line at NV_X capped with inward-pointing triangles, and
#     labelled once, in A. grey70 at alpha 0.25: at 0.18 the band on white is
#     #F1F1F1, a screen light enough to drop out in print.
#   - HARVEST STRIP, not era blocks. The first v2 drew contiguous eras
#     (pre-whaling / whaling / krill fishing), which misstated both periods:
#     whaling did not stop when krill fishing began, and krill fishing did not
#     run to 2010. Each activity now has its own row, shaded year by year by
#     its OBSERVED CATCH WEIGHT, scaled to that activity's own peak -- so the
#     strip shows when each one happened and how heavy it was, not how they
#     compare in magnitude. Activities that overlap cannot be shown as
#     background blocks, so the panels are no longer tinted. v3 (2026-09-26):
#     the strip was EFFORT until v2 and is catch weight from v3; it now comes
#     from F00h_harvest_strip.R, shared with Figures 3 and 4 -- see there.
#   - Direction arrows sit in the right margin and ORIGINATE AT ZERO, the level
#     where exploited and unexploited runs agree. With STAB_SIGN = -1, down
#     means "less" in every panel: less biomass, fewer large animals, less
#     stable. STAB_ARROWS turns the B/D arrows off.
#   - ZERO_MODE sets where zero sits. "centred" (default): symmetric axes, so
#     zero is at mid-height in every panel. "anchored": axes follow the data,
#     with zero held at least MIN_SIDE of the axis from either edge so both
#     arrows fit. Science's instructions ask that axes not extend beyond the
#     range of the plotted data; "centred" does, by about the data range, in
#     C and D.
#   - Drawn at final printed size, 180 mm wide (Science's 3-column width is
#     18.4 cm). Year labels on the bottom panel(s) only. Axis titles are 9 pt,
#     except in the 1 x 4, whose 47 mm panels need 8 pt for the longest
#     one-line name (46.5 mm at 9 pt). Needs ggplot2 >= 3.5.
#
# THE WINDOW ONLY AFFECTS PANELS B AND D. The level panels A and C are computed
# once, outside the window loop, and are byte-identical across all five files --
# the rolling SD is the METRIC being measured in B and D, not a smoother applied
# to the whole figure. The console prints this, since nothing on the figures
# now says which window they use.
#
# CANONICAL SNR, transcribed from Plotting scripts/biomass_slope_snr_mean_med.R
# (~line 89). The denominator is the SD **across years 1841-2010 of the
# across-member mean UNEXPLOITED trajectory** -- a temporal SD of one curve,
# giving ONE scalar per metric. It is NOT a rolling SD, and it is not the
# across-member spread.
#
# Getting this wrong is the single easiest way to produce a plausible-looking
# but meaningless SNR, so the definition is reproduced here rather than
# re-derived.
#
# CROSS-CHECK. Only FIG_SUF=rebuilt167 has an independent reference: cut A on
# ensemble 44 gives a biomass SNR at 2010 of about -0.147
# (Output_large_files/wmin_test/46_selection_cuts.csv), against a published
# -0.920 -- a property of that selection rule, which never selected for whale
# realism. The check warns loudly on disagreement for that build and is skipped
# (with a console note) for every other suffix. NOTHING VALIDATES THE p104
# NUMBERS HERE: if an independently computed p104 value exists, wire it in the
# same way.
#
# Writes, for suffix S (p104q10 by default) and set tag T ("" or "_top"):
#        FIG_OUT/fig2_snr_S T_v4.{png,pdf}             (15 yr, 1 x 4)
#        FIG_OUT/fig2_snr_S T_v4_2x2.{png,pdf}         (15 yr, 2 x 2)
#        FIG_OUT/fig2_snr_S T_v4_phaseplane.{png,pdf}
#        FIG_SUPP/fig2_snr_S T_v4_{3,6,9,12}yr.{png,pdf}
#        Manuscript data/fig2_snr_series_S T_v4.csv   (all windows, tall)
# T also carries the denominator when it is not classic (e.g. "_paired").
# (the suffix is VER; _v2 outputs carry the older EFFORT strip, _v3 the
# "(SNR)" titles and two-line arrow labels)
# guard() refuses to overwrite: move earlier outputs before re-running.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(zoo); library(grid)
})

DATA <- "Manuscript data"
# FIG_SUF / FIG_OUT / FIG_SUPP / FIG_SET / FIG_SNR re-point the script without
# editing it. The defaults build the manuscript figure: p104, all members,
# classic denominator.
FIGS <- Sys.getenv("FIG_OUT", "Manuscript figures")
SUPP <- Sys.getenv("FIG_SUPP", file.path(FIGS, "Supplemental figures"))
dir.create(FIGS, recursive = TRUE, showWarnings = FALSE)
dir.create(SUPP, recursive = TRUE, showWarnings = FALSE)
SUF <- Sys.getenv("FIG_SUF", "p104q10")
VER <- "_v4"                         # every output name carries this
                                     # (v3 = v2 drawing, catch-weight strip;
                                     #  v4 = plain titles, one-line arrows)
BASELINE_YEARS <- 1841:2010     # full unexploited record
WINDOWS <- c(3L, 6L, 9L, 12L, 15L)   # trailing rolling-SD windows, years
MAIN_WINDOW <- 15L                   # the one that is the manuscript figure
PLOT_YEARS <- 1900:2010
THRESH1 <- 1                    # emergence threshold
EXPECTED_BIOMASS_SNR_2010 <- -0.1465   # 46_selection_cuts.csv, cut A (rebuilt167)

# --- palette (Kieran, 2026-08-05) ---------------------------------------------
# ONE COLOUR PER METRIC FAMILY, replacing the archived script's four constants
# (COL_B steelblue / COL_B_VAR #1F4E79 / COL_SLOPE #C0392B / COL_SL_VAR #8B1A1A),
# which gave each family a light/dark pair, level light and variability dark.
# Level vs variability is already carried by the y-axis titles and the stacking,
# and the dark half of that scheme did not survive the standard palette checks:
# #1F4E79 and #8B1A1A both fall below the OKLCH lightness band (L 0.414/0.416 vs
# a 0.43 floor) AND below the 0.10 chroma floor, so they read as near-black
# rather than as a hue. Steelblue also missed the chroma floor at 0.099.
#
# Coral / indigo passes all six checks: L 0.660/0.445, C 0.173/0.127, worst
# adjacent CVD dE 22.3 (protan), normal-vision dE 33.4, contrast 3.27:1 and
# 6.90:1 against white. Teal is the intuitive complement to coral and was
# rejected on measurement, not taste -- every sRGB teal misses the chroma floor
# (best #008B8B at 0.098) and its CVD separation from coral is 9.5-10.8, less
# than half indigo's. Green is worse again (dE 6.2, the red-green axis).
COL_BIOMASS <- "#3F4B99"   # indigo   -- panels A and B
COL_SLOPE   <- "#E9604F"   # coral    -- panels C and D
BAND_FILL   <- "grey70"
BAND_ALPHA  <- 0.25
COL_NOTE    <- "grey30"    # band label, arrows
# Harvest strip, full-intensity colours (each row ramps from white). Hues on the
# blue-yellow axis, which protan/deutan CVD preserves, and away from both data
# families: ochre OKLCH (0.66, 0.095, 75) and blue (0.66, 0.085, 238), both
# >= 3:1 against white at peak effort (3.14 / 3.06).
ACT_COL <- c("Whaling" = "#B48A4C", "Krill fishing" = "#5E9AC0")
# Phase-plane year ramp: viridis trimmed to 0-0.58, so every year keeps at
# least 3.16:1 against white (the full ramp's yellow end is 1.26:1) and the
# ramp never reaches the red end of plasma/rocket, which would read as coral.
PP_BEGIN <- 0
PP_END   <- 0.58

# --- output size and typography -----------------------------------------------
# Every canvas is drawn at its PRINTED size, so every size here is points on the
# page. Science: panel letters 9 pt bold, labels ~7 pt and never below 5 pt,
# line weights >= 0.5 pt, sans serif. Geoms take millimetres, hence <pt>/.pt.
FIG_W    <- 180 / 25.4   # in -- all three layouts
H_STACK  <- 225 / 25.4   # in -- 1 x 4
H_GRID   <- 150 / 25.4   # in -- 2 x 2
PP_W     <- 120 / 25.4   # in -- phase plane
PP_H     <- 120 / 25.4
BASE     <- 9      # pt -- axis text
AXT      <- 9      # pt -- axis titles (2 x 2, phase plane)
AXT_STACK <- 8     # pt -- axis titles in the 1 x 4
TAG      <- 9      # pt -- panel letters (Science: 9 pt bold)
HDR      <- 10     # pt -- 2 x 2 column headers
STRIP_PT <- 8      # pt -- harvest strip labels
NOTE_PT  <- 7      # pt -- band label, arrow labels
ARROW_PT_STACK <- 6  # pt -- arrow labels in the 1 x 4 only: one line at 7 pt,
                     #  "Fewer large animals" (~22.5 mm) just fits C's ~23 mm
                     #  half-panel; 6 pt leaves ~1 mm each end (Kieran 2026-09-27)
MARGIN_R <- 22     # pt -- right margin reserved for the direction arrows
                   #  (30 until v4, sized for two-line labels)
MARGIN_T <- 11     # pt -- top margin that holds the panel letter

STAB_SIGN   <- -1          # -1: B/D show stability (down = less stable); +1: v1
STAB_ARROWS <- TRUE
NV_X        <- 1903        # x position of the natural-variability marker

# Panel names = y-axis labels, one line each, no unit (the caption carries it).
LAB <- c(A = "Community biomass",
         B = if (STAB_SIGN < 0) "Biomass stability" else "Biomass variability",
         C = "Community size structure",
         D = if (STAB_SIGN < 0) "Size-structure stability" else
               "Size-structure variability")
# Arrow labels on ONE line (Kieran 2026-09-27): the longest, "Fewer large
# animals", is ~23 mm at NOTE_PT, inside the ~25 mm half-panel of the 1 x 4.
ARR <- list(biomass = c("More biomass",       "Less biomass"),
            size    = c("More large animals", "Fewer large animals"),
            stab    = if (STAB_SIGN < 0) c("More stable", "Less stable") else
                                         c("Less stable", "More stable"))
ZERO_MODE   <- "centred"   # or "anchored"
MIN_SIDE    <- 0.3         # "anchored": least share of the axis on each side of 0
PP_CROSS_YEARS <- c(1940, 1960, 1980, 2010)
PP_BAR_MM      <- 50       # phase-plane colourbar length

guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE)
  f
}

# --- harvest timing (derived from the effort array, not hard-coded) ----------
eff <- readRDS("effort_array_1841_2010.rds")
yr <- as.numeric(rownames(eff))
onset <- function(sp) { y <- yr[eff[, sp] > 0]; if (length(y)) min(y) else NA }
peak  <- function(sp) { v <- eff[, sp]; if (any(v > 0)) yr[which.max(v)] else NA }
whaling_start <- min(c(onset("baleen whales"), onset("sperm whales"),
                       onset("minke whales")), na.rm = TRUE)
krill_start   <- onset("antarctic krill")
peaks <- c("Peak baleen" = peak("baleen whales"), "Peak sperm" = peak("sperm whales"),
           "Peak minke" = peak("minke whales"),   "Peak krill" = peak("antarctic krill"))
peaks <- peaks[!is.na(peaks)]
message("onsets: ", whaling_start, ", ", krill_start,
        " | peaks: ", paste(unname(peaks), collapse = ", "))

# Strip rows (label -> species) and their catch-weight shading live in
# F00h_harvest_strip.R, shared with Figures 3 and 4, so all three bars encode
# the same thing. The onsets and peaks above stay on EFFORT: they are the
# model's forcing, and only the phase-plane colourbar labels use them.
source("Manuscript scripts/F00h_harvest_strip.R")
ACTIVITY <- HARVEST_ACTIVITY
STRIP <- harvest_strip_data(PLOT_YEARS, ACT_COL)

# --- data --------------------------------------------------------------------
bf <- readRDS(file.path(DATA, sprintf("biomass_abund_fish_%s.rds", SUF)))
bc <- readRDS(file.path(DATA, sprintf("biomass_abund_clim_%s.rds", SUF)))
sl <- readRDS(file.path(DATA, sprintf("nbss_slope_%s.rds", SUF)))
meta <- readRDS(file.path(DATA, sprintf("meta_%s.rds", SUF)))
message("members: ", meta$n_members, " | cut: ", meta$cut)
# FIG_SET=top restricts to the selection carried in meta$cuts. NOTE that this
# moves BOTH terms of the SNR under the classic denominator: the noise is the
# temporal SD of the across-member MEAN unexploited trajectory, so a different
# member set is a different noise scale, not just a different signal.
source("Manuscript scripts/F00z_member_set.R")
KEEP <- fig_members(meta)
bf <- fig_filter(bf, KEEP, "exploited")
bc <- fig_filter(bc, KEEP, "unexploited")
sl <- fig_filter(sl, KEEP, "slope")

# community biomass per member-year
tot <- function(d) d %>% group_by(member = sim_index, Year) %>%
  summarise(val = sum(Biomass), .groups = "drop")
bio_f <- tot(bf); bio_c <- tot(bc)

slope_f <- sl %>% filter(arm == "exploited")   %>% transmute(member = sim_index, Year, val = slope)
slope_c <- sl %>% filter(arm == "unexploited") %>% transmute(member = sim_index, Year, val = slope)


# --- the SNR, three denominators ---------------------------------------------
# Identical to the block in F02b; see there for the full argument. The NUMERATOR
# is paired in every mode (median over members of E_i - U_i); the modes differ
# only in the noise:
#
#   classic  sd_t( mean_i U_i )      the published definition, DEFAULT
#   medsd    median_i( sd_t U_i )    fixes the scale mismatch, shared scalar
#   paired   median_i( (E_i-U_i) / sd_t U_i )   every member its own control
#
# ON THE FULL SIZE RANGE the scale mismatch is expected to matter MORE than it
# does under the 1 g cutoff. The cutoff strips out the zooplankton and krill
# that carry the largest abundance draws, leaving a member-level distribution
# that is nearly symmetric (mean/median 1.031). The full range keeps them, so
# `classic` -- which divides a median-scaled numerator by a mean-scaled
# denominator -- can be biased in EITHER direction here, and by more. Read the
# level-skew diagnostic this script prints before interpreting the difference.
#
# NOT THE FORBIDDEN LOOKALIKE. All three keep a TEMPORAL SD. Dividing by the
# ACROSS-MEMBER sd of the paired differences answers a parameter-uncertainty
# question and must never be called SNR.
SNR_MODE <- Sys.getenv("FIG_SNR", "classic")
stopifnot(SNR_MODE %in% c("classic", "medsd", "paired"))

make_snr <- function(fish_m, clim_m) {
  rep_unexp <- clim_m %>% group_by(Year) %>%
    summarise(mu = mean(val, na.rm = TRUE), .groups = "drop")
  noise_classic <- sd(rep_unexp$mu[rep_unexp$Year %in% BASELINE_YEARS],
                      na.rm = TRUE)
  per <- clim_m %>% filter(Year %in% BASELINE_YEARS) %>% group_by(member) %>%
    summarise(s = sd(val, na.rm = TRUE), .groups = "drop")
  noise_medsd <- median(per$s, na.rm = TRUE)
  noise <- switch(SNR_MODE, classic = noise_classic, medsd = noise_medsd,
                  paired = noise_medsd)   # reported only; paired divides per member
  if (is.na(noise) || noise == 0) stop("baseline noise is NA or 0")

  J <- inner_join(fish_m, clim_m, by = c("member", "Year"),
                  suffix = c("_f", "_c")) %>%
    mutate(signal = val_f - val_c)

  if (SNR_MODE == "paired") {
    J %>% left_join(per, by = "member") %>%
      filter(is.finite(s), s > 0) %>%
      mutate(snr_i = signal / s) %>%
      group_by(Year) %>%
      summarise(signal_med = median(signal, na.rm = TRUE),
                snr_med = median(snr_i, na.rm = TRUE),
                snr_q25 = quantile(snr_i, 0.25, na.rm = TRUE),
                snr_q75 = quantile(snr_i, 0.75, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(noise = noise,
             signal_q25 = snr_q25 * noise, signal_q75 = snr_q75 * noise)
  } else {
    J %>% group_by(Year) %>%
      summarise(signal_med = median(signal, na.rm = TRUE),
                signal_q25 = quantile(signal, 0.25, na.rm = TRUE),
                signal_q75 = quantile(signal, 0.75, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(noise = noise, snr_med = signal_med / noise,
             snr_q25 = signal_q25 / noise, snr_q75 = signal_q75 / noise)
  }
}

# --- level-skew diagnostic ----------------------------------------------------
# The full size range keeps the groups carrying the big abundance draws, so this
# is where a mean-scaled denominator can bite. Printed, not asserted.
lev <- bio_c %>% filter(Year %in% BASELINE_YEARS) %>% group_by(member) %>%
  summarise(a = mean(val), .groups = "drop")
cat(sprintf("\nmember level (unexploited community biomass, full range):\n"))
cat(sprintf("  mean/median %.4g | max/median %.4g | largest member %.2f%% of the sum\n",
            mean(lev$a) / median(lev$a), max(lev$a) / median(lev$a),
            100 * max(lev$a) / sum(lev$a)))

# rolling SD per member, then the same SNR machinery on the variability series
roll_sd <- function(d, k) d %>% arrange(member, Year) %>% group_by(member) %>%
  mutate(val = zoo::rollapplyr(val, k, sd, fill = NA)) %>% ungroup()

# --- LEVEL panels: window-independent, computed once -------------------------
snr_bio   <- make_snr(bio_f, bio_c)
snr_slope <- make_snr(slope_f, slope_c)

# --- the load-bearing cross-check --------------------------------------------
# It is only load-bearing for the build it was computed on. 46_selection_cuts.csv
# is ENSEMBLE 44 under the classic denominator; a phase-88 build, or either of
# the new denominators, is a different quantity and MUST disagree. Firing the
# warning there would be crying wolf, and a stale guard that always fires is
# worse than no guard -- it trains you to ignore it.
CHECKABLE <- SUF == "rebuilt167" && SNR_MODE == "classic" &&
  identical(Sys.getenv("FIG_SET", "all"), "all")
got <- snr_bio$snr_med[snr_bio$Year == 2010]
cat("\n=== SNR at 2010 (level panels; window-independent) ===\n")
cat("  biomass SNR:", signif(got, 5),
    if (CHECKABLE)
      sprintf(" (46_selection_cuts.csv cut A: %.4f)\n", EXPECTED_BIOMASS_SNR_2010)
    else sprintf(" [suffix %s | mode %s -- no cross-check applies]\n",
                 SUF, SNR_MODE))
cat("  slope   SNR:", signif(snr_slope$snr_med[snr_slope$Year == 2010], 5), "\n")
if (CHECKABLE) {
  rel <- abs(got - EXPECTED_BIOMASS_SNR_2010) / abs(EXPECTED_BIOMASS_SNR_2010)
  cat(sprintf("  relative difference: %.3f%%\n", 100 * rel))
  if (rel > 0.01)
    warning("biomass SNR at 2010 disagrees with 46_selection_cuts.csv by ",
            sprintf("%.2f%%", 100 * rel),
            " -- check the cut A membership and the catchability multipliers ",
            "before trusting any figure built from this data.", call. = FALSE)
}

# =============================================================================
# DRAWING (v2)
# =============================================================================
x_sc <- scale_x_continuous(breaks = seq(1900, 2010, by = 20),
                           expand = expansion(mult = c(0.01, 0.01)),
                           limits = range(PLOT_YEARS))

th_fig2 <- function() {
  theme_classic(base_size = BASE) +
    theme(panel.grid   = element_blank(),
          axis.text    = element_text(size = BASE, colour = "grey20"),
          axis.title   = element_text(size = AXT, lineheight = 0.95),
          axis.title.x = element_blank(),
          # Science: the letter goes at the upper left of each part. Anchored at
          # the top-left of the plot's inner area (= the panel top, now there are
          # no titles) with vjust = 0, so it sits ABOVE the panel, inside the
          # MARGIN_T strip -- clear of the y-axis title however long that is.
          plot.tag.location = "plot",
          plot.tag.position = c(0, 1),
          plot.tag     = element_text(size = TAG, face = "bold",
                                      hjust = 0, vjust = 0),
          plot.margin  = margin(MARGIN_T, MARGIN_R, 2, 2))
}

# y-limits by ZERO_MODE. Both modes always include the +/-1 band.
y_lims <- function(v) {
  lo <- min(v, -THRESH1, na.rm = TRUE); hi <- max(v, THRESH1, na.rm = TRUE)
  if (ZERO_MODE == "centred") return(c(-1, 1) * max(-lo, hi))
  k  <- MIN_SIDE / (1 - MIN_SIDE)           # "anchored"
  hi <- max(hi, -k * lo); lo <- min(lo, -k * hi)
  c(lo, hi)
}

band <- annotate("rect", xmin = -Inf, xmax = Inf,
                 ymin = -THRESH1, ymax = THRESH1,
                 fill = BAND_FILL, alpha = BAND_ALPHA)
# Natural-variability marker, in every panel: a black line at NV_X spanning
# the band, capped by two triangles whose flat sides sit exactly on
# +/-THRESH1 and whose tips point back towards zero. annotation_custom's
# viewport IS the band (xmin = xmax = NV_X gives zero width), so the caps land
# on +/-1 whatever the y-range; their size is physical, shrunk to 35% of the
# band where the band is thin (C, D on centred axes). Drawn before the data.
nv_marker <- function() {
  h <- unit.pmin(unit(1.4, "mm"), unit(0.35, "npc"))   # cap height
  w <- unit(0.95, "mm")                                  # cap half-width
  x <- unit(0, "npc")
  cap <- function(y0, tip) polygonGrob(unit.c(x - w, x + w, x),
                                       unit.c(y0, y0, tip),
                                       gp = gpar(col = NA, fill = "black"))
  annotation_custom(
    gTree(children = gList(
      segmentsGrob(x, unit(0, "npc"), x, unit(1, "npc"),
                   gp = gpar(col = "black", lwd = 1, lineend = "butt")),
      cap(unit(1, "npc"), unit(1, "npc") - h),
      cap(unit(0, "npc"), unit(0, "npc") + h))),
    xmin = NV_X, xmax = NV_X, ymin = -THRESH1, ymax = THRESH1)
}
# Label beside the marker, in A only. It sits in the upper half of the band in
# the pre-whaling years, where the signal is identically zero, so it cannot
# collide with the data whatever the build.
band_text <- annotate("text", x = NV_X + 2, y = THRESH1 / 2,
                      label = "Within natural\nvariability",
                      hjust = 0, vjust = 0.5, lineheight = 0.9,
                      size = NOTE_PT / .pt, colour = COL_NOTE)

# Direction arrows in the right margin, ORIGINATING AT ZERO. Two annotation
# viewports on the panel's right edge (xmin = xmax = Inf gives zero width): one
# from y = 0 up to the panel top, one from the panel bottom up to y = 0. Lengths
# are npc of each half and offsets are pt, so the geometry holds for any
# y-range or layout. The zero line continues out of the panel as a stub to a
# dot, and both arrows leave the dot -- without the dot, two arrows meeting at
# zero read as one double-headed arrow. Requires clip = "off".
arrows_v <- function(up, down, dx = 7, pad = 3, gap = 2.6, fs = NOTE_PT) {
  ax <- unit(0, "npc") + unit(dx, "pt")
  tx <- ax + unit(3.5, "pt")
  a  <- arrow(length = unit(1.5, "mm"), angle = 25, type = "closed")
  ga <- gpar(col = COL_NOTE, fill = COL_NOTE, lwd = 0.9)
  gt <- gpar(col = COL_NOTE, fontsize = fs, lineheight = 0.9)
  gz <- gpar(col = "grey55", lwd = 0.35 * .pt)       # = the zero line's weight
  upper <- gTree(children = gList(
    segmentsGrob(unit(0, "npc"), unit(0, "npc"), ax, unit(0, "npc"), gp = gz),
    circleGrob(ax, unit(0, "npc"), r = unit(1.3, "pt"),
               gp = gpar(col = NA, fill = COL_NOTE)),
    segmentsGrob(ax, unit(0, "npc") + unit(gap, "pt"),
                 ax, unit(1, "npc") - unit(pad, "pt"), arrow = a, gp = ga),
    textGrob(up, tx, unit(0.5, "npc"), rot = 90, just = c("centre", "top"), gp = gt)))
  lower <- gTree(children = gList(
    segmentsGrob(ax, unit(1, "npc") - unit(gap, "pt"),
                 ax, unit(0, "npc") + unit(pad, "pt"), arrow = a, gp = ga),
    textGrob(down, tx, unit(0.5, "npc"), rot = 90, just = c("centre", "top"), gp = gt)))
  list(annotation_custom(upper, xmin = Inf, xmax = Inf, ymin = 0, ymax = Inf),
       annotation_custom(lower, xmin = Inf, xmax = Inf, ymin = -Inf, ymax = 0))
}

# Ribbon fill is the line colour at alpha 0.30 -- one colour per panel, not a
# line/fill pair, so the panels read as four variants of the same object.
snr_panel <- function(d, col, ylab, tag, arrows = NULL, band_label = FALSE,
                      show_x = FALSE, axt = AXT, arrow_pt = NOTE_PT) {
  d <- d %>% filter(Year %in% PLOT_YEARS)
  p <- ggplot(d, aes(x = Year)) +
    band +
    geom_ribbon(aes(ymin = snr_q25, ymax = snr_q75), fill = col, alpha = 0.30) +
    geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.35) +
    geom_line(aes(y = snr_med), colour = col, linewidth = 0.8) +
    nv_marker() +
    x_sc + th_fig2() + theme(axis.title.y = element_text(size = axt)) +
    coord_cartesian(ylim = y_lims(c(d$snr_q25, d$snr_q75, d$snr_med)),
                    clip = "off") +
    labs(y = ylab, tag = tag)
  if (band_label)       p <- p + band_text
  if (!is.null(arrows)) p <- p + arrows_v(arrows[1], arrows[2], fs = arrow_pt)
  if (show_x) p + labs(x = "Year") + theme(axis.title.x = element_text(size = AXT))
  else        p + theme(axis.text.x = element_blank())
}

# Harvest strip: one row per ACTIVITY, one tile per year, fill ramping from
# white at zero effort to ACT_COL at that activity's peak. Each row's label
# sits just before its first year of effort. Shares x_sc, so patchwork's panel
# alignment puts every tile over its year in the panels below. (A row whose
# effort starts before ~1905 leaves no room for its label on the left.)
harvest_strip <- function() {
  n   <- length(ACTIVITY)
  lab <- data.frame(label = names(ACTIVITY), row = n + 1 - seq_len(n),
                    x = sapply(names(ACTIVITY),
                               function(a) min(STRIP$Year[STRIP$act == a])) - 1.5)
  ggplot(STRIP) +
    geom_rect(aes(xmin = pmax(Year - 0.5, min(PLOT_YEARS)),
                  xmax = pmin(Year + 0.52, max(PLOT_YEARS)),   # 0.02 overlap: no seams
                  ymin = row - 0.4, ymax = row + 0.4, fill = fill), colour = NA) +
    scale_fill_identity() +
    geom_text(data = lab, aes(x = x, y = row, label = label), hjust = 1,
              size = STRIP_PT / .pt, colour = "grey20") +
    x_sc + scale_y_continuous(limits = c(0.5, n + 0.5), expand = c(0, 0)) +
    coord_cartesian(clip = "off") + theme_void() +
    theme(plot.margin = margin(1, MARGIN_R, 1, 2))
}

# 2 x 2 column header: bold title over a rule spanning the panel width.
col_header <- function(title) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.62, label = title, fontface = "bold",
             size = HDR / .pt, colour = "grey10") +
    annotate("segment", x = 0, xend = 1, y = 0.1, yend = 0.1,
             linewidth = 0.4, colour = "grey35") +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    coord_cartesian(clip = "off") + theme_void() +
    theme(plot.margin = margin(0, MARGIN_R, 0, 2))
}

# --- one 1 x 4 figure per window; the 2 x 2 for the main window ---------------
cat("\n=== building", length(WINDOWS), "window variants ===\n")
cat("  panels A and C are window-independent and identical in all of them;",
    "only B and D change.\n  Nothing on the figures names the window -- the",
    "captions must.\n")
series_all <- list()
set_tag <- paste0(
  if (identical(Sys.getenv("FIG_SET", "all"), "all")) "" else
    paste0("_", fig_set_tag()),
  if (SNR_MODE == "classic") "" else paste0("_", SNR_MODE))
stab_arr <- if (STAB_ARROWS) ARR$stab else NULL

# B and D are drawn as STAB_SIGN x the variability SNR. Negating a quantile
# band swaps its ends, so q25 and q75 trade places. transform() evaluates
# every right-hand side on the ORIGINAL columns, so the swap is safe.
as_stability <- function(d) {
  if (STAB_SIGN > 0) return(d)
  transform(d, snr_med = -snr_med, snr_q25 = -snr_q75, snr_q75 = -snr_q25)
}

# grid = TRUE for the 2 x 2, where C sits on the bottom row and needs years
four_panels <- function(snr_biov, snr_slov, grid = FALSE, axt = AXT,
                        arrow_pt = NOTE_PT) list(
  A = snr_panel(snr_bio, COL_BIOMASS, LAB[["A"]], "A", ARR$biomass,
                band_label = TRUE, axt = axt, arrow_pt = arrow_pt),
  B = snr_panel(as_stability(snr_biov), COL_BIOMASS, LAB[["B"]], "B", stab_arr,
                axt = axt, arrow_pt = arrow_pt),
  C = snr_panel(snr_slope, COL_SLOPE, LAB[["C"]], "C", ARR$size, show_x = grid,
                axt = axt, arrow_pt = arrow_pt),
  D = snr_panel(as_stability(snr_slov), COL_SLOPE, LAB[["D"]], "D", stab_arr,
                show_x = TRUE, axt = axt, arrow_pt = arrow_pt))

for (W in WINDOWS) {
  snr_biov <- make_snr(roll_sd(bio_f, W), roll_sd(bio_c, W))
  snr_slov <- make_snr(roll_sd(slope_f, W), roll_sd(slope_c, W))

  P <- four_panels(snr_biov, snr_slov, axt = AXT_STACK, arrow_pt = ARROW_PT_STACK)
  fig <- (harvest_strip() / P$A / P$B / P$C / P$D) +
    plot_layout(heights = c(0.13, 1, 1, 1, 1))

  is_main <- W == MAIN_WINDOW
  dir_out <- if (is_main) FIGS else SUPP
  stem <- if (is_main) sprintf("fig2_snr_%s%s%s", SUF, set_tag, VER)
          else          sprintf("fig2_snr_%s%s%s_%dyr", SUF, set_tag, VER, W)
  png_out <- guard(file.path(dir_out, paste0(stem, ".png")))
  pdf_out <- guard(file.path(dir_out, paste0(stem, ".pdf")))
  ggsave(png_out, fig, width = FIG_W, height = H_STACK, dpi = 600)
  ggsave(pdf_out, fig, width = FIG_W, height = H_STACK)
  cat(sprintf("  %2dyr %-12s -> %s\n", W, if (is_main) "(MAIN)" else "(supplement)",
              png_out))

  if (is_main) {
    G <- four_panels(snr_biov, snr_slov, grid = TRUE)
    grid_fig <- wrap_plots(
      col_header("Ecosystem structure"), col_header("Ecosystem stability"),
      harvest_strip(), harvest_strip(),
      G$A, G$B, G$C, G$D,
      ncol = 2, heights = c(0.085, 0.11, 1, 1))
    g_png <- guard(file.path(FIGS, paste0(stem, "_2x2.png")))
    g_pdf <- guard(file.path(FIGS, paste0(stem, "_2x2.pdf")))
    ggsave(g_png, grid_fig, width = FIG_W, height = H_GRID, dpi = 600)
    ggsave(g_pdf, grid_fig, width = FIG_W, height = H_GRID)
    cat(sprintf("  %2dyr %-12s -> %s\n", W, "(2 x 2)", g_png))
  }

  series_all[[as.character(W)]] <- bind_rows(
    snr_bio   %>% mutate(metric = "biomass"),
    snr_biov  %>% mutate(metric = "biomass_variability"),
    snr_slope %>% mutate(metric = "slope"),
    snr_slov  %>% mutate(metric = "slope_variability")) %>%
    mutate(window_yr = W)
}

# --- phase plane (level panels only, so window-independent) -------------------
# One point per year: the across-member MEDIAN of each level SNR. These are
# marginal medians -- the path is a summary, not any single member's history --
# which is why the IQR crosses matter: they show where the members actually sit.
pp <- inner_join(
  snr_bio   %>% transmute(Year, bx = snr_med, bx_lo = snr_q25, bx_hi = snr_q75),
  snr_slope %>% transmute(Year, sy = snr_med, sy_lo = snr_q25, sy_hi = snr_q75),
  by = "Year") %>%
  filter(Year >= whaling_start - 1, Year <= max(PLOT_YEARS)) %>%
  arrange(Year)
cr    <- pp %>% filter(Year %in% PP_CROSS_YEARS)
tail2 <- pp %>% slice_tail(n = 2)
end_seg <- data.frame(x = tail2$bx[1], y = tail2$sy[1],
                      xend = tail2$bx[2], yend = tail2$sy[2], Year = tail2$Year[2])

# Horizontal twin of arrows_v for the x-axis, in the top margin, originating at
# x = 0: viewports from x = 0 to the right edge and from the left edge to 0.
arrows_h <- function(left, right, dy = 7, pad = 3, gap = 2.6) {
  ay <- unit(0, "npc") + unit(dy, "pt")
  a  <- arrow(length = unit(1.5, "mm"), angle = 25, type = "closed")
  ga <- gpar(col = COL_NOTE, fill = COL_NOTE, lwd = 0.9)
  gt <- gpar(col = COL_NOTE, fontsize = NOTE_PT)
  gz <- gpar(col = "grey55", lwd = 0.35 * .pt)
  right_g <- gTree(children = gList(
    segmentsGrob(unit(0, "npc"), unit(0, "npc"), unit(0, "npc"), ay, gp = gz),
    circleGrob(unit(0, "npc"), ay, r = unit(1.3, "pt"),
               gp = gpar(col = NA, fill = COL_NOTE)),
    segmentsGrob(unit(0, "npc") + unit(gap, "pt"), ay,
                 unit(1, "npc") - unit(pad, "pt"), ay, arrow = a, gp = ga),
    textGrob(right, unit(0.5, "npc"), ay + unit(2.5, "pt"),
             just = c("centre", "bottom"), gp = gt)))
  left_g <- gTree(children = gList(
    segmentsGrob(unit(1, "npc") - unit(gap, "pt"), ay,
                 unit(0, "npc") + unit(pad, "pt"), ay, arrow = a, gp = ga),
    textGrob(left, unit(0.5, "npc"), ay + unit(2.5, "pt"),
             just = c("centre", "bottom"), gp = gt)))
  list(annotation_custom(right_g, xmin = 0, xmax = Inf, ymin = Inf, ymax = Inf),
       annotation_custom(left_g, xmin = -Inf, xmax = 0, ymin = Inf, ymax = Inf))
}

yr_brk <- c(whaling_start, krill_start, max(pp$Year))
yr_lab <- c(sprintf("%d\nwhaling starts", whaling_start),
            sprintf("%d\nkrill fishing starts", krill_start),
            as.character(max(pp$Year)))

p_pp <- ggplot(pp, aes(bx, sy)) +
  annotate("rect", xmin = -THRESH1, xmax = THRESH1,
           ymin = -THRESH1, ymax = THRESH1, fill = BAND_FILL, alpha = BAND_ALPHA) +
  annotate("text", x = THRESH1, y = THRESH1, label = "Within natural variability",
           hjust = 1, vjust = -0.6, size = NOTE_PT / .pt, colour = COL_NOTE) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.35) +
  geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.35) +
  geom_segment(data = cr, aes(x = bx_lo, xend = bx_hi, y = sy, yend = sy,
                              colour = Year), linewidth = 0.45, alpha = 0.75) +
  geom_segment(data = cr, aes(x = bx, xend = bx, y = sy_lo, yend = sy_hi,
                              colour = Year), linewidth = 0.45, alpha = 0.75) +
  geom_path(aes(colour = Year), linewidth = 0.7, lineend = "round",
            linejoin = "round") +
  # Arrowhead on the final step only: geom_path() with a varying colour is
  # drawn segment by segment, so its own arrow= would put a head on every year.
  geom_segment(data = end_seg, aes(x = x, y = y, xend = xend, yend = yend,
                                   colour = Year),
               linewidth = 0.7, inherit.aes = FALSE,
               arrow = arrow(length = unit(2.2, "mm"), angle = 25, type = "closed")) +
  geom_point(data = cr, aes(fill = Year), shape = 21, colour = "white",
             stroke = 0.5, size = 2.1) +
  annotate("text", x = 0, y = 0, label = "Pre-whaling", hjust = 1.06,
           vjust = -0.55, size = NOTE_PT / .pt, colour = COL_NOTE) +
  annotate("text", x = tail2$bx[2], y = tail2$sy[2], label = max(pp$Year),
           hjust = -0.35, vjust = 0.5, size = NOTE_PT / .pt, colour = "grey15") +
  scale_colour_viridis_c(option = "D", begin = PP_BEGIN, end = PP_END,
                         limits = range(pp$Year), breaks = yr_brk, labels = yr_lab,
                         name = NULL) +
  scale_fill_viridis_c(option = "D", begin = PP_BEGIN, end = PP_END,
                       limits = range(pp$Year), guide = "none") +
  # Bar size is set on the GUIDE: set globally, ggplot2 >= 3.5 multiplies the
  # long side by 5 (a 50 mm request became ~250 mm and ran off the canvas).
  guides(colour = guide_colourbar(theme = theme(
    legend.key.width = unit(PP_BAR_MM, "mm"),
    legend.key.height = unit(2.4, "mm")))) +
  arrows_h("Less biomass", "More biomass") +
  arrows_v(ARR$size[1], ARR$size[2]) +
  coord_cartesian(xlim = y_lims(c(pp$bx, pp$bx_lo, pp$bx_hi)),   # same ZERO_MODE
                  ylim = y_lims(c(pp$sy, pp$sy_lo, pp$sy_hi)),   # rule, both axes
                  clip = "off") +
  labs(x = LAB[["A"]], y = LAB[["C"]]) +
  theme_classic(base_size = BASE) +
  theme(axis.text  = element_text(size = BASE, colour = "grey20"),
        axis.title = element_text(size = AXT),
        legend.position = "bottom",
        legend.text = element_text(size = NOTE_PT, colour = "grey20",
                                   lineheight = 0.9),
        legend.margin = margin(0, 0, 0, 0),
        plot.margin = margin(22, MARGIN_R, 2, 2))

pp_stem <- sprintf("fig2_snr_%s%s%s_phaseplane", SUF, set_tag, VER)
pp_png <- guard(file.path(FIGS, paste0(pp_stem, ".png")))
pp_pdf <- guard(file.path(FIGS, paste0(pp_stem, ".pdf")))
ggsave(pp_png, p_pp, width = PP_W, height = PP_H, dpi = 600)
ggsave(pp_pdf, p_pp, width = PP_W, height = PP_H)
cat(sprintf("  %-17s -> %s\n", "(phase plane)", pp_png))

write.csv(bind_rows(series_all),
          guard(file.path(DATA, sprintf("fig2_snr_series_%s%s%s.csv", SUF,
                                        set_tag, VER))),
          row.names = FALSE)

# The variability panels are what the window changes -- report their endpoint so
# the choice of 15 yr for the main figure can be seen to matter or not.
cat("\n=== variability SNR at 2010, by window ===\n")
if (STAB_SIGN < 0)
  cat("  (v1 VARIABILITY sign, as in the CSV; panels B and D plot the negative)\n")
print(as.data.frame(bind_rows(series_all) %>%
  filter(Year == 2010, grepl("variability", metric)) %>%
  select(window_yr, metric, snr_med) %>%
  pivot_wider(names_from = metric, values_from = snr_med)),
  digits = 4, row.names = FALSE)
cat("\ndone.\n")
