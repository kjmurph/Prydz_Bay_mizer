###############################################################################
# biomass_slope_snr_mean_med.R
#
# Publication SNR figures for the top-10% RMSE ensemble (212 matched members),
# 1900-2010. Paired framing (difference within each matched member first, then
# aggregate across members); noise baseline = full unexploited record 1841-2010
# (climate carries no detectable trend, so the full record is stationary and
# gives the most robust natural-variability estimate).
#
# Two SEPARATE single-column figures are produced (per rolling window):
#   * MEDIAN (main)      : median of paired member differences + 25-75 IQR ribbon
#   * MEAN   (supplement): mean   of paired member differences + mean +/- 1 SD ribbon
# Each figure has four stacked panels:
#   a  Biomass SNR                 (level; window-independent)
#   b  Biomass variability SNR     (rolling SD)
#   c  Size-spectrum slope SNR     (level; window-independent)
#   d  Slope variability SNR       (rolling SD)
#
# Variants are produced for rolling windows of 3, 6, 9, 12, 15 years (trailing).
#
# Annotations:
#   * Onset lines (grey dashed): whaling 1930, krill fishing 1974. Their text
#     labels appear on the UPPERMOST panel only.
#   * Peak-effort lines (black long-dash), from the model effort forcing
#     (effort_array_1841_2010.rds, per-gear argmax): peak baleen whaling 1933,
#     peak sperm whaling 1948, peak minke whaling 1973, peak krill fishing 1979.
#     Labels are rotated 90 deg (running along each line) at the bottom of the
#     lowest panel.
#   * Only the +/-1 emergence lines are drawn (red dashed). No +/-2 lines, no
#     zero line, no gridlines.
#
# Inputs (in "Manuscript data/"; the 1.8 GB ensemble is NOT re-run). Run from the
# repo root so these relative paths resolve:
#   Manuscript data/biomass_top10pct_raw_fish.rds / _clim.rds
#   Manuscript data/nbss_slope_top10pct_data.rds
#
# Outputs (per window W in {3,6,9,12,15}):
#   biomass_slope_snr_median_main_Wyr_trailing_base1841_2010.png
#   biomass_slope_snr_mean_supp_Wyr_trailing_base1841_2010.png
#   biomass_slope_snr_mean_med_series.csv   (all windows, median & mean columns)
###############################################################################

if (!requireNamespace("slider", quietly = TRUE))
  stop("Install the 'slider' package first: install.packages('slider')")

suppressPackageStartupMessages({
  library(tidyverse)
  library(slider)
  library(patchwork)
  library(scales)
})

message("=== Biomass & slope SNR -- median (main) & mean (supp) figures ===")

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
PLOT_YEARS     <- 1900:2010
BASELINE_YEARS <- 1841:2010          # full unexploited record (climate negligible)
BASE_LABEL     <- "base1841_2010"

WINDOW_SIZES <- c(3L, 6L, 9L, 12L, 15L)   # trailing rolling-SD windows (years)

# Onset lines (grey dashed) + labels (uppermost panel only)
KEY_YEARS  <- c(1930, 1974)
KEY_LABELS <- c("Whaling\nstarts", "Krill fishing\nstarts")

# Peak-effort lines (black long-dash) from effort_array_1841_2010.rds (argmax)
PEAK_YEARS  <- c(1933, 1948, 1973, 1979)
PEAK_LABELS <- c("Peak baleen", "Peak sperm", "Peak minke", "Peak krill")

COL_B      <- "steelblue"
COL_B_VAR  <- "#1F4E79"
COL_SLOPE  <- "#C0392B"
COL_SL_VAR <- "#8B1A1A"

THRESH1 <- 1   # emergence threshold (red dashed)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
roll_sd <- function(x, before, after) {
  slide_dbl(x, ~ sd(.x), .before = before, .after = after, .complete = TRUE)
}

# Per-year SNR summary of the paired (fish - clim) member differences:
# median, mean, across-member SD, and 25/75 IQR, all divided by the baseline
# noise (SD of the unexploited ensemble-mean trajectory over BASELINE_YEARS).
make_snr <- function(fish_m, clim_m) {
  rep_unexp <- clim_m %>%
    group_by(Year) %>%
    summarise(mu = mean(val, na.rm = TRUE), .groups = "drop")
  noise <- sd(rep_unexp$mu[rep_unexp$Year %in% BASELINE_YEARS], na.rm = TRUE)
  if (is.na(noise) || noise == 0) {
    warning("Baseline noise is NA or 0; SNR set to NA.")
    noise <- NA_real_
  }
  inner_join(fish_m, clim_m, by = c("member", "Year"),
             suffix = c("_f", "_c")) %>%
    mutate(signal = val_f - val_c) %>%
    group_by(Year) %>%
    summarise(
      signal_med  = median(signal,        na.rm = TRUE),
      signal_mean = mean(signal,          na.rm = TRUE),
      signal_sd   = sd(signal,            na.rm = TRUE),
      signal_q25  = quantile(signal, 0.25, na.rm = TRUE),
      signal_q75  = quantile(signal, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      noise    = noise,
      snr_med  = signal_med  / noise,
      snr_mean = signal_mean / noise,
      snr_sd   = signal_sd   / noise,
      snr_q25  = signal_q25  / noise,
      snr_q75  = signal_q75  / noise
    ) %>%
    filter(Year %in% PLOT_YEARS)
}

# Rolling-SD (variability) member tables for a given trailing window size.
var_tables <- function(size) {
  wb <- size - 1L; wa <- 0L
  svb_fish <- total_fish %>% group_by(sim_i) %>% arrange(Year) %>%
    mutate(val = roll_sd(total_g, wb, wa)) %>% ungroup() %>%
    transmute(member = sim_i, Year, val)
  svb_clim <- total_clim %>% group_by(sim_i) %>% arrange(Year) %>%
    mutate(val = roll_sd(total_g, wb, wa)) %>% ungroup() %>%
    transmute(member = sim_i, Year, val)

  sl_roll <- all_slopes %>% group_by(sim_id, ensemble) %>% arrange(time) %>%
    mutate(sval = roll_sd(slope, wb, wa)) %>% ungroup()
  slv_wide <- sl_roll %>% select(sim_id, time, ensemble, sval) %>%
    pivot_wider(names_from = ensemble, values_from = sval, values_fn = mean)
  svbeta_fish <- slv_wide %>% transmute(member = sim_id, Year = time, val = Exploited)
  svbeta_clim <- slv_wide %>% transmute(member = sim_id, Year = time, val = Unexploited)

  list(bvar = make_snr(svb_fish, svb_clim),
       svar = make_snr(svbeta_fish, svbeta_clim))
}

# ---------------------------------------------------------------------------
# Load biomass caches -> total community biomass per member per year
# ---------------------------------------------------------------------------
message("Loading biomass caches...")
raw_fish <- readRDS("Manuscript data/biomass_top10pct_raw_fish.rds")
raw_clim <- readRDS("Manuscript data/biomass_top10pct_raw_clim.rds")

total_fish <- raw_fish %>% group_by(sim_i, Year) %>%
  summarise(total_g = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
  arrange(sim_i, Year)
total_clim <- raw_clim %>% group_by(sim_i, Year) %>%
  summarise(total_g = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
  arrange(sim_i, Year)
rm(raw_fish, raw_clim)

bl_fish <- total_fish %>% transmute(member = sim_i, Year, val = total_g)
bl_clim <- total_clim %>% transmute(member = sim_i, Year, val = total_g)

# ---------------------------------------------------------------------------
# Load NBSS slope data -> wide (Exploited/Unexploited) per member per year
# ---------------------------------------------------------------------------
message("Loading NBSS slope data...")
all_slopes <- readRDS(
  "Manuscript data/nbss_slope_top10pct_data.rds"
)$all_slopes %>% filter(spectrum_type == "Biomass") %>% arrange(sim_id, ensemble, time)

slope_wide <- all_slopes %>% select(sim_id, time, ensemble, slope) %>%
  pivot_wider(names_from = ensemble, values_from = slope, values_fn = mean)
beta_fish <- slope_wide %>% transmute(member = sim_id, Year = time, val = Exploited)
beta_clim <- slope_wide %>% transmute(member = sim_id, Year = time, val = Unexploited)
rm(slope_wide)

# Level SNRs are window-independent -> compute once
snr_bl   <- make_snr(bl_fish,   bl_clim)
snr_beta <- make_snr(beta_fish, beta_clim)

# ---------------------------------------------------------------------------
# Plot scaffolding
# ---------------------------------------------------------------------------
x_sc <- scale_x_continuous(breaks = seq(1900, 2010, by = 20),
                           expand = expansion(mult = c(0.01, 0.01)),
                           limits = range(PLOT_YEARS))
# No gridlines; x-axis title blanked by default (restored on the bottom panel).
base_th <- theme_classic(base_size = 11) +
  theme(axis.title.x = element_blank())

start_vlines <- geom_vline(xintercept = KEY_YEARS, linetype = "dashed",
                           colour = "grey50", linewidth = 0.6, alpha = 0.8)
peak_vlines  <- geom_vline(xintercept = PEAK_YEARS, linetype = "dotted",
                           colour = "grey50", linewidth = 0.6, alpha = 0.8)
start_text <- annotate("text", x = KEY_YEARS - 1.5, y = Inf, label = KEY_LABELS,
                       hjust = 1, vjust = 2.2, size = 3.0, colour = "grey35")
# Peak labels: rotated 90 deg, lifted just off the bottom axis (negative hjust),
# running up just LEFT of each line.
peak_text <- annotate("text", x = PEAK_YEARS - 1.0, y = -Inf, label = PEAK_LABELS,
                      angle = 90, hjust = -0.08, vjust = 0.5, size = 2.6, colour = "grey35")

# central = "med"  -> median line + 25-75 IQR ribbon
# central = "mean" -> mean line + mean +/- SD ribbon
snr_panel <- function(df, col, ylab, central = c("med", "mean"),
                      show_start_text = FALSE, show_peak_text = FALSE,
                      show_xtitle = FALSE) {
  central <- match.arg(central)
  d <- df
  if (central == "med") {
    d$ymid <- d$snr_med;  d$ylo <- d$snr_q25;             d$yhi <- d$snr_q75
  } else {
    d$ymid <- d$snr_mean; d$ylo <- d$snr_mean - d$snr_sd; d$yhi <- d$snr_mean + d$snr_sd
  }
  p <- ggplot(d, aes(x = Year)) +
    geom_ribbon(aes(ymin = ylo, ymax = yhi), fill = col, alpha = 0.30) +
    geom_hline(yintercept = 0, linetype = "solid",
               colour = "grey60", linewidth = 0.4) +
    geom_hline(yintercept = c(-THRESH1, THRESH1), linetype = "dashed",
               colour = "red", linewidth = 0.6) +
    start_vlines + peak_vlines +
    geom_line(aes(y = ymid), colour = col, linewidth = 0.9) +
    x_sc + base_th + coord_cartesian(clip = "off") + labs(y = ylab)
  if (show_start_text) p <- p + start_text
  if (show_peak_text)  p <- p + peak_text
  if (show_xtitle)     p <- p + theme(axis.title.x = element_text()) + labs(x = "Year")
  p
}

# Build & save one single-column, 4-panel figure for a given central stat.
build_figure <- function(central, snr_bvar, snr_svar, size) {
  wlab <- paste0(size, "yr")
  vlab_b <- sprintf("Biomass variability SNR\n(%s rolling SD)", wlab)
  vlab_s <- sprintf("Slope variability SNR\n(%s rolling SD)", wlab)

  p_a <- snr_panel(snr_bl,   COL_B,      "Biomass SNR", central,
                   show_start_text = TRUE, show_peak_text = TRUE)
  p_b <- snr_panel(snr_bvar, COL_B_VAR,  vlab_b, central)
  p_c <- snr_panel(snr_beta, COL_SLOPE,  "Size-spectrum slope SNR", central)
  p_d <- snr_panel(snr_svar, COL_SL_VAR, vlab_s, central, show_xtitle = TRUE)

  # No embedded caption/subtitle: the panel description belongs in the
  # manuscript figure legend (Nature portfolio format).
  p_out <- (p_a / p_b / p_c / p_d) +
    plot_annotation(tag_levels = "a") &
    theme(plot.tag = element_text(face = "bold"))

  role    <- if (central == "med") "median_main" else "mean_supp"
  outfile <- sprintf("biomass_slope_snr_%s_%s_trailing_%s.png", role, wlab, BASE_LABEL)
  ggsave(outfile, p_out, width = 8, height = 13, dpi = 300)
  message(sprintf("  Saved: %s", outfile))
}

# ---------------------------------------------------------------------------
# Loop over rolling-window sizes
# ---------------------------------------------------------------------------
series_all <- list()

for (size in WINDOW_SIZES) {
  wlab <- paste0(size, "yr")
  message(sprintf("Window %s (trailing) ...", wlab))
  vt <- var_tables(size)

  build_figure("med",  vt$bvar, vt$svar, size)   # main
  build_figure("mean", vt$bvar, vt$svar, size)   # supplement

  series_all[[wlab]] <- bind_rows(
    snr_bl    %>% mutate(metric = "Biomass"),
    vt$bvar   %>% mutate(metric = "Biomass variability"),
    snr_beta  %>% mutate(metric = "Slope"),
    vt$svar   %>% mutate(metric = "Slope variability")
  ) %>% mutate(window = wlab, align = "trailing", baseline = BASE_LABEL)
}

# ---------------------------------------------------------------------------
# Write series CSV (all windows; level metrics repeated per window)
# ---------------------------------------------------------------------------
write.csv(bind_rows(series_all), "biomass_slope_snr_mean_med_series.csv",
          row.names = FALSE)
message("Saved: biomass_slope_snr_mean_med_series.csv")

message("=== Done ===")