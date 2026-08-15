# =============================================================================
# FIGURE 2 -- signal-to-noise ratio panels, rebuilt ensemble 44, cut A (167).
#
#   a  Biomass SNR
#   b  Biomass variability SNR (W yr trailing rolling SD)
#   c  Size-spectrum slope SNR
#   d  Slope variability SNR (W yr trailing rolling SD)
#
# Builds FIVE variants, W = 3, 6, 9, 12, 15 yr. The 15 yr version is the main
# figure; the other four go to the Supplement.
#
# THE WINDOW ONLY AFFECTS PANELS b AND d. The level panels a and c are computed
# once, outside the window loop, and are byte-identical across all five files --
# the rolling SD is the METRIC being measured in b and d, not a smoother applied
# to the whole figure. This is easy to misread from the five near-identical
# outputs, so the console prints it and the panel titles carry the window.
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
# LABELLING FOLLOWS THE ARCHIVED REFERENCE
# Manuscript figures/Archived/biomass_slope_snr_median_main_15yr_trailing_base1841_2010.png,
# produced by Plotting scripts/biomass_slope_snr_mean_med.R:181-224:
#   - onset vlines in every panel, their text on the UPPERMOST panel only
#   - peak-effort vlines in every panel, labels rotated 90 deg off the bottom
#     axis, also uppermost panel only
#   - ribbon fill is the LINE colour at alpha 0.30, not a separate lighter tint
#   - tags come from plot_annotation(tag_levels = "a")
# The peak years are DERIVED from effort_array_1841_2010.rds (per-gear argmax)
# rather than hardcoded as c(1933, 1948, 1973, 1979); they agree, and deriving
# them keeps the figure honest if the effort array is ever revised.
#
# EXPECTED RESULT. Cut A on this ensemble gives a biomass SNR at 2010 of about
# -0.147 (Output_large_files/wmin_test/46_selection_cuts.csv), against a
# published -0.920. That is a property of the selection rule, not of this build:
# the yield RMSE barely responds to whale catch, so cut A never selected for
# whale realism, and the community-biomass signal is essentially the whale
# biomass removed by whaling. This script CHECKS that value and warns loudly on
# disagreement -- 46_selection_cuts.R computed it from the same states through a
# different code path, so agreement validates membership, the catchability
# multipliers, both projections and the SNR definition in one number.
#
# Data: Manuscript data/*_rebuilt167.rds, built by F00_build_rebuilt167_data.R.
#
# Writes Manuscript figures/fig2_snr_rebuilt167.{png,pdf}                (15 yr)
#        Manuscript figures/Supplemental figures/
#            fig2_snr_rebuilt167_{3,6,9,12}yr.{png,pdf}
#        Manuscript data/fig2_snr_series_rebuilt167.csv  (all windows, tall)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(zoo)
})

DATA <- "Manuscript data"
# FIG_SUF / FIG_OUT / FIG_SUPP exist so this script can be re-pointed at an
# alternative data build (e.g. the 10-member kernel pilot) without editing it.
# The defaults reproduce the published behaviour exactly.
FIGS <- Sys.getenv("FIG_OUT", "Manuscript figures")
SUPP <- Sys.getenv("FIG_SUPP", file.path(FIGS, "Supplemental figures"))
dir.create(FIGS, recursive = TRUE, showWarnings = FALSE)
dir.create(SUPP, recursive = TRUE, showWarnings = FALSE)
SUF <- Sys.getenv("FIG_SUF", "rebuilt167")
BASELINE_YEARS <- 1841:2010     # full unexploited record
WINDOWS <- c(3L, 6L, 9L, 12L, 15L)   # trailing rolling-SD windows, years
MAIN_WINDOW <- 15L                   # the one that is the manuscript figure
PLOT_YEARS <- 1900:2010
THRESH1 <- 1                    # emergence threshold
EXPECTED_BIOMASS_SNR_2010 <- -0.1465   # 46_selection_cuts.csv, cut A

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
COL_BIOMASS <- "#3F4B99"   # indigo   -- panels a and b
COL_SLOPE   <- "#E9604F"   # coral    -- panels c and d
# The threshold lines were pure "red", which competed with the old #C0392B in
# panels c/d and would compete far worse with coral: two reds, one meaning data
# and one meaning threshold. Graphite keeps them legible and recessive.
COL_THRESH  <- "#3A3A38"

guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE)
  f
}

# --- era markers (derived from the effort array, not hard-coded) --------------
eff <- readRDS("effort_array_1841_2010.rds")
yr <- as.numeric(rownames(eff))
onset <- function(sp) { y <- yr[eff[, sp] > 0]; if (length(y)) min(y) else NA }
peak  <- function(sp) { v <- eff[, sp]; if (any(v > 0)) yr[which.max(v)] else NA }
whaling_start <- min(c(onset("baleen whales"), onset("sperm whales"),
                       onset("minke whales")), na.rm = TRUE)
krill_start   <- onset("antarctic krill")
KEY_YEARS  <- c(whaling_start, krill_start)
KEY_LABELS <- c("Whaling\nstarts", "Krill fishing\nstarts")

peaks <- c("Peak baleen" = peak("baleen whales"), "Peak sperm" = peak("sperm whales"),
           "Peak minke" = peak("minke whales"),   "Peak krill" = peak("antarctic krill"))
peaks <- peaks[!is.na(peaks)]
PEAK_YEARS  <- unname(peaks)
PEAK_LABELS <- names(peaks)
message("onsets: ", paste(KEY_YEARS, collapse = ", "),
        " | peaks: ", paste(PEAK_YEARS, collapse = ", "))

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

# --- plot scaffolding, transcribed from the archived reference ---------------
x_sc <- scale_x_continuous(breaks = seq(1900, 2010, by = 20),
                           expand = expansion(mult = c(0.01, 0.01)),
                           limits = range(PLOT_YEARS))
base_th <- theme_classic(base_size = 11) +
  theme(panel.grid = element_blank(), axis.title.x = element_blank())

start_vlines <- geom_vline(xintercept = KEY_YEARS, linetype = "dashed",
                           colour = "grey50", linewidth = 0.6, alpha = 0.8)
peak_vlines  <- geom_vline(xintercept = PEAK_YEARS, linetype = "dotted",
                           colour = "grey50", linewidth = 0.6, alpha = 0.8)
start_text <- annotate("text", x = KEY_YEARS - 1.5, y = Inf, label = KEY_LABELS,
                       hjust = 1, vjust = 2.2, size = 3.0, colour = "grey35")
# Peak labels: rotated 90 deg, lifted just off the bottom axis (negative hjust),
# running up just LEFT of each line.
peak_text <- annotate("text", x = PEAK_YEARS - 1.0, y = -Inf, label = PEAK_LABELS,
                      angle = 90, hjust = -0.08, vjust = 0.5, size = 2.6,
                      colour = "grey35")

# Ribbon fill is the line colour at alpha 0.30 -- one colour per panel, not a
# line/fill pair, so the panels read as four variants of the same object.
snr_panel <- function(d, col, ylab,
                      show_start_text = FALSE, show_peak_text = FALSE,
                      show_xtitle = FALSE) {
  d <- d %>% filter(Year %in% PLOT_YEARS)
  p <- ggplot(d, aes(x = Year)) +
    geom_ribbon(aes(ymin = snr_q25, ymax = snr_q75), fill = col, alpha = 0.30) +
    geom_hline(yintercept = 0, linetype = "solid",
               colour = "grey60", linewidth = 0.4) +
    geom_hline(yintercept = c(-THRESH1, THRESH1), linetype = "dashed",
               colour = COL_THRESH, linewidth = 0.6) +
    start_vlines + peak_vlines +
    geom_line(aes(y = snr_med), colour = col, linewidth = 0.9) +
    x_sc + base_th + coord_cartesian(clip = "off") + labs(y = ylab)
  if (show_start_text) p <- p + start_text
  if (show_peak_text)  p <- p + peak_text
  if (show_xtitle)     p <- p + theme(axis.title.x = element_text()) + labs(x = "Year")
  p
}

# --- one figure per window ---------------------------------------------------
cat("\n=== building", length(WINDOWS), "window variants ===\n")
cat("  panels a and c are window-independent and identical in all of them;",
    "only b and d change\n")
series_all <- list()

for (W in WINDOWS) {
  snr_biov <- make_snr(roll_sd(bio_f, W), roll_sd(bio_c, W))
  snr_slov <- make_snr(roll_sd(slope_f, W), roll_sd(slope_c, W))

  pa <- snr_panel(snr_bio,  COL_BIOMASS, "Biomass SNR",
                  show_start_text = TRUE, show_peak_text = TRUE)
  pb <- snr_panel(snr_biov, COL_BIOMASS,
                  sprintf("Biomass variability SNR\n(%dyr rolling SD)", W))
  pc <- snr_panel(snr_slope, COL_SLOPE, "Size-spectrum slope SNR")
  pd <- snr_panel(snr_slov, COL_SLOPE,
                  sprintf("Slope variability SNR\n(%dyr rolling SD)", W),
                  show_xtitle = TRUE)

  fig <- (pa / pb / pc / pd) +
    plot_annotation(tag_levels = "a") &
    theme(plot.tag = element_text(face = "bold"))

  is_main <- W == MAIN_WINDOW
  dir_out <- if (is_main) FIGS else SUPP
  set_tag <- paste0(
    if (identical(Sys.getenv("FIG_SET", "all"), "all")) "" else
      paste0("_", fig_set_tag()),
    if (SNR_MODE == "classic") "" else paste0("_", SNR_MODE))
  stem <- if (is_main) sprintf("fig2_snr_%s%s", SUF, set_tag)
          else          sprintf("fig2_snr_%s%s_%dyr", SUF, set_tag, W)
  png_out <- guard(file.path(dir_out, paste0(stem, ".png")))
  pdf_out <- guard(file.path(dir_out, paste0(stem, ".pdf")))
  ggsave(png_out, fig, width = 8, height = 13, dpi = 300)
  ggsave(pdf_out, fig, width = 8, height = 13)
  cat(sprintf("  %2dyr %-12s -> %s\n", W, if (is_main) "(MAIN)" else "(supplement)",
              png_out))

  series_all[[as.character(W)]] <- bind_rows(
    snr_bio   %>% mutate(metric = "biomass"),
    snr_biov  %>% mutate(metric = "biomass_variability"),
    snr_slope %>% mutate(metric = "slope"),
    snr_slov  %>% mutate(metric = "slope_variability")) %>%
    mutate(window_yr = W)
}

write.csv(bind_rows(series_all),
          guard(file.path(DATA, sprintf("fig2_snr_series_%s%s%s.csv", SUF,
            if (identical(Sys.getenv("FIG_SET", "all"), "all")) ""
            else paste0("_", fig_set_tag()),
            if (SNR_MODE == "classic") "" else paste0("_", SNR_MODE)))),
          row.names = FALSE)

# The variability panels are what the window changes -- report their endpoint so
# the choice of 15 yr for the main figure can be seen to matter or not.
cat("\n=== variability SNR at 2010, by window ===\n")
print(as.data.frame(bind_rows(series_all) %>%
  filter(Year == 2010, grepl("variability", metric)) %>%
  select(window_yr, metric, snr_med) %>%
  pivot_wider(names_from = metric, values_from = snr_med)),
  digits = 4, row.names = FALSE)
cat("\ndone.\n")