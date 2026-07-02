###############################################################################
# biomass_slope_snr.R
#
# Detection / time-of-emergence figure for the top-10% RMSE ensemble
# (212 matched members), 1900-2010. Replaces the inverse-dispersion stability
# metrics (1/CV, 1/SD) used in biomass_slope_stability*.R with a simpler,
# better-behaved signal-to-noise (SNR) framing, following Julia's suggestion
# and the detection/emergence approach of Barrier et al. (2024, Earth's Future
# 10.1029/2024EF004736).
#
# Four metrics, each computed per matched member i and year t:
#   Level:        B[i,t]            total community biomass (sum of 19 species)
#                 beta[i,t]         NBSS biomass-spectrum slope
#   Variability:  s_B[i,t]   = SD over a rolling window of B
#                 s_beta[i,t]= SD over a rolling window of beta
#   (Variability is the *direct* rolling SD: higher = more variable = less
#    stable. This avoids the nonlinear blow-ups of 1/CV and 1/SD that appear
#    whenever a window's SD approaches zero.)
#
# For each metric M:
#   signal(t) = median_i( M_exploited[i,t] - M_unexploited[i,t] )    (paired)
#               (median, not mean: the across-calibration distribution of the
#                fishing effect is strongly right-skewed, so the mean is pulled
#                into the tail and would sit outside the IQR ribbon; the median
#                tracks the typical calibration. snr_mean is kept in the CSV.)
#   noise     = SD over the baseline window of the *representative*
#               (ensemble-mean) unexploited trajectory of M. (Equivalent to the
#               mean per-member temporal SD here, A~=C; we do NOT pool the
#               cross-calibration spread as Barrier does, because pairing has
#               already removed it from the signal.)
#   SNR(t)    = signal(t) / noise        emergence when |SNR| > 1 (also > 2)
#
# Time of emergence is reported three ways (first / sustained-5yr / permanent),
# the last following Barrier et al. 2025 (signal stays beyond threshold).
#
# Because there is no exploitation in the baseline (whaling 1930, krill 1974),
# SNR ~ 0 in the baseline by construction; later excursions past +/-1 are
# genuinely exploitation-driven.
#
# Panels (Julia's layout):
#   a  Biomass SNR
#   b  Biomass variability SNR
#   c  Size-spectrum slope SNR
#   d  Slope variability SNR
# Reference lines at +/-1 (primary, red) and +/-2 (stricter, grey).
#
# Sensitivity (SUPP): looped over window sizes {3,5,10,12,15} and over two
# baseline definitions (1901-1929 primary; full 1900-2010). The legacy 1/CV and
# 1/SD figures remain in biomass_slope_stability*.R as the old-metric SUPP.
#
# Inputs (pre-existing caches; the 1.8 GB ensemble is NOT re-run):
#   biomass_top10pct_raw_fish.rds
#   biomass_top10pct_raw_clim.rds
#   Output_large_files/community_slope_analysis/nbss_slope_top10pct_data.rds
#
# Outputs:
#   biomass_slope_snr_<win>_<baseline>.png          (10 figures)
#   biomass_slope_snr_series.csv                     (primary combo, all metrics)
#   biomass_slope_snr_emergence.csv                  (time-of-emergence, all combos)
###############################################################################

if (!requireNamespace("slider", quietly = TRUE))
  stop("Install the 'slider' package first: install.packages('slider')")

suppressPackageStartupMessages({
  library(tidyverse)
  library(slider)
  library(patchwork)
  library(scales)
})

message("=== Biomass & slope SNR (detection / time-of-emergence) ===")

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
PLOT_YEARS <- 1900:2010

KEY_YEARS  <- c(1930, 1974)
KEY_LABELS <- c("Whaling\nstarts", "Krill\nfishing\nstarts")

# Colours: biomass panels blue family, slope panels red family;
# level = lighter, variability = darker.
COL_B      <- "steelblue"
COL_B_VAR  <- "#1F4E79"
COL_SLOPE  <- "#C0392B"
COL_SL_VAR <- "#8B1A1A"

# Rolling-window sizes (years) and alignment for the variability metrics.
# "trailing" = right-aligned/causal: SD at year t uses only [t-size+1, t], so no
#   signal can appear before its driver exists, and the emergence year is not
#   biased early. Recommended for the detection/time-of-emergence framing.
# "centered" = symmetric window (reaches forward in time; reproduces the legacy
#   biomass_slope_stability*.R convention) -- available for comparison.
WINDOW_ALIGN <- "trailing"
WINDOW_SIZES <- c(3L, 5L, 10L, 12L, 15L)

# Convert a window size + alignment into slider's before/after offsets.
win_offsets <- function(size, align) {
  if (align == "trailing") {
    list(before = size - 1L, after = 0L)
  } else {
    before <- as.integer(ceiling((size - 1) / 2))
    list(before = before, after = size - 1L - before)
  }
}

# Baseline windows for the natural-variability (noise) denominator
BASELINE_CONFIGS <- list(
  list(label = "base1901_1929", years = 1901:1929),  # primary (pre-exploitation)
  list(label = "baseFull",      years = 1900:2010)    # SUPP (trend-inflated)
)

PRIMARY_WIN  <- "10yr"
PRIMARY_BASE <- "base1901_1929"

THRESH1 <- 1   # primary emergence threshold (Julia)
THRESH2 <- 2   # stricter threshold (common in time-of-emergence work)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Direct rolling SD (NA where window is incomplete). SD of a constant window
# is a legitimate 0 here -- no inverse, so no blow-up.
roll_sd <- function(x, before, after) {
  slide_dbl(x, ~ sd(.x), .before = before, .after = after, .complete = TRUE)
}

# Given per-member fish/clim metric tables (cols: member, Year, val), return a
# per-year SNR summary using the supplied baseline window.
#   noise = SD over baseline years of the ensemble-mean unexploited trajectory
#   central signal(t) = MEDIAN across members of the paired (fish - clim)
#     differences (robust to the right-skewed across-calibration distribution;
#     keeps the central line inside the IQR ribbon). The ensemble mean is also
#     retained (snr_mean) for reference.
#   ribbon = 25-75th pct of (fish - clim) / noise (parameter sensitivity of the
#     fishing effect, NOT the noise envelope).
make_snr <- function(fish_m, clim_m, baseline_years) {
  rep_unexp <- clim_m %>%
    group_by(Year) %>%
    summarise(mu = mean(val, na.rm = TRUE), .groups = "drop")
  noise <- sd(rep_unexp$mu[rep_unexp$Year %in% baseline_years], na.rm = TRUE)
  if (is.na(noise) || noise == 0) {
    warning("Baseline noise is NA or 0; SNR set to NA.")
    noise <- NA_real_
  }

  inner_join(fish_m, clim_m, by = c("member", "Year"),
             suffix = c("_f", "_c")) %>%
    mutate(signal = val_f - val_c) %>%
    group_by(Year) %>%
    summarise(
      signal_med  = median(signal,       na.rm = TRUE),
      signal_mean = mean(signal,         na.rm = TRUE),
      signal_q25  = quantile(signal, 0.25, na.rm = TRUE),
      signal_q75  = quantile(signal, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      noise    = noise,
      snr_med  = signal_med  / noise,
      snr_mean = signal_mean / noise,
      snr_q25  = signal_q25  / noise,
      snr_q75  = signal_q75  / noise
    ) %>%
    filter(Year %in% PLOT_YEARS)
}

# Time of emergence (computed on the plotted median SNR):
#   first     = first year |SNR| > thr
#   sustained = first year of a >= 5 consecutive-year emergence
#   permanent = first year after which |SNR| stays > thr for the rest of the
#               record (Barrier et al. 2025 definition)
emergence_years <- function(df, thr) {
  d <- df %>% arrange(Year) %>% filter(!is.na(snr_med))
  if (nrow(d) == 0) return(c(first = NA, sustained = NA, permanent = NA))
  em <- abs(d$snr_med) > thr
  first_year <- if (any(em)) d$Year[which(em)[1]] else NA_real_
  r <- rle(em); sustained <- NA_real_; idx <- 1L
  for (k in seq_along(r$lengths)) {
    if (isTRUE(r$values[k]) && r$lengths[k] >= 5) { sustained <- d$Year[idx]; break }
    idx <- idx + r$lengths[k]
  }
  permanent <- NA_real_
  if (any(em)) {
    below <- which(!em)
    if (length(below) == 0) permanent <- d$Year[1]
    else if (max(below) < length(em)) permanent <- d$Year[max(below) + 1]
  }
  c(first = first_year, sustained = sustained, permanent = permanent)
}

# ---------------------------------------------------------------------------
# Load biomass caches -> total community biomass per member per year
# ---------------------------------------------------------------------------
message("Loading biomass caches...")
raw_fish <- readRDS("biomass_top10pct_raw_fish.rds")
raw_clim <- readRDS("biomass_top10pct_raw_clim.rds")

total_fish <- raw_fish %>%
  group_by(sim_i, Year) %>%
  summarise(total_g = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
  arrange(sim_i, Year)
total_clim <- raw_clim %>%
  group_by(sim_i, Year) %>%
  summarise(total_g = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
  arrange(sim_i, Year)
rm(raw_fish, raw_clim)

# Member-level biomass LEVEL tables (window-independent)
bl_fish <- total_fish %>% transmute(member = sim_i, Year, val = total_g)
bl_clim <- total_clim %>% transmute(member = sim_i, Year, val = total_g)

# ---------------------------------------------------------------------------
# Load NBSS slope data -> wide (Exploited/Unexploited) per member per year
# ---------------------------------------------------------------------------
message("Loading NBSS slope data...")
slope_rds <- readRDS(
  "Output_large_files/community_slope_analysis/nbss_slope_top10pct_data.rds"
)
all_slopes <- slope_rds$all_slopes %>%
  filter(spectrum_type == "Biomass") %>%
  arrange(sim_id, ensemble, time)
rm(slope_rds)

# Member-level slope LEVEL tables (window-independent)
slope_wide <- all_slopes %>%
  select(sim_id, time, ensemble, slope) %>%
  pivot_wider(names_from = ensemble, values_from = slope, values_fn = mean)
beta_fish <- slope_wide %>% transmute(member = sim_id, Year = time, val = Exploited)
beta_clim <- slope_wide %>% transmute(member = sim_id, Year = time, val = Unexploited)
rm(slope_wide)

# ---------------------------------------------------------------------------
# Shared plot scaffolding
# ---------------------------------------------------------------------------
x_sc <- scale_x_continuous(
  breaks = seq(1900, 2010, by = 20),
  expand = expansion(mult = c(0.01, 0.01)),
  limits = range(PLOT_YEARS)
)
base_th <- theme_classic(base_size = 11) +
  theme(panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
        axis.title.x = element_blank())
key_vlines <- geom_vline(xintercept = KEY_YEARS, linetype = "dashed",
                         colour = "grey50", linewidth = 0.6, alpha = 0.8)
key_text <- annotate("text", x = KEY_YEARS - 0.25, y = Inf, label = KEY_LABELS,
                     hjust = 1, vjust = 1.3, size = 3.0, colour = "grey35")

snr_panel <- function(df, col, ylab, show_keys = FALSE, show_xtitle = FALSE) {
  p <- ggplot(df, aes(x = Year)) +
    geom_ribbon(aes(ymin = snr_q25, ymax = snr_q75), fill = col, alpha = 0.30) +
    geom_hline(yintercept = 0, linetype = "solid",
               colour = "grey60", linewidth = 0.4) +
    geom_hline(yintercept = c(-THRESH1, THRESH1), linetype = "dashed",
               colour = "red", linewidth = 0.6) +
    geom_hline(yintercept = c(-THRESH2, THRESH2), linetype = "dotted",
               colour = "grey45", linewidth = 0.5)
  if (show_keys) p <- p + key_vlines + key_text
  else           p <- p + key_vlines
  p <- p +
    geom_line(aes(y = snr_med), colour = col, linewidth = 0.9) +
    x_sc + base_th + coord_cartesian(clip = "off") +
    labs(y = ylab)
  if (show_xtitle) p <- p + theme(axis.title.x = element_text()) + labs(x = "Year")
  p
}

# ---------------------------------------------------------------------------
# Main loop: baseline x window
# ---------------------------------------------------------------------------
series_primary <- NULL
emergence_rows <- list()

for (bcfg in BASELINE_CONFIGS) {
  base_years <- bcfg$years
  base_label <- bcfg$label

  # Level SNRs are window-independent -> compute once per baseline
  snr_bl   <- make_snr(bl_fish,   bl_clim,   base_years)  # panel a
  snr_beta <- make_snr(beta_fish, beta_clim, base_years)  # panel c

  for (size in WINDOW_SIZES) {
    off <- win_offsets(size, WINDOW_ALIGN)
    wb <- off$before; wa <- off$after; wlab <- paste0(size, "yr")
    message(sprintf("Baseline %s | window %s (%s) ...",
                    base_label, wlab, WINDOW_ALIGN))

    # Biomass variability (panel b)
    svb_fish <- total_fish %>% group_by(sim_i) %>% arrange(Year) %>%
      mutate(val = roll_sd(total_g, wb, wa)) %>% ungroup() %>%
      transmute(member = sim_i, Year, val)
    svb_clim <- total_clim %>% group_by(sim_i) %>% arrange(Year) %>%
      mutate(val = roll_sd(total_g, wb, wa)) %>% ungroup() %>%
      transmute(member = sim_i, Year, val)
    snr_bvar <- make_snr(svb_fish, svb_clim, base_years)

    # Slope variability (panel d)
    sl_roll <- all_slopes %>% group_by(sim_id, ensemble) %>% arrange(time) %>%
      mutate(sval = roll_sd(slope, wb, wa)) %>% ungroup()
    slv_wide <- sl_roll %>% select(sim_id, time, ensemble, sval) %>%
      pivot_wider(names_from = ensemble, values_from = sval, values_fn = mean)
    svbeta_fish <- slv_wide %>% transmute(member = sim_id, Year = time, val = Exploited)
    svbeta_clim <- slv_wide %>% transmute(member = sim_id, Year = time, val = Unexploited)
    snr_svar <- make_snr(svbeta_fish, svbeta_clim, base_years)

    # --- assemble figure ---
    p_a <- snr_panel(snr_bl,   COL_B,      "Biomass SNR", show_keys = TRUE)
    p_b <- snr_panel(snr_bvar, COL_B_VAR,
                     sprintf("Biomass variability SNR\n(%s rolling SD)", wlab))
    p_c <- snr_panel(snr_beta, COL_SLOPE,  "Size-spectrum slope SNR", show_keys = TRUE)
    p_d <- snr_panel(snr_svar, COL_SL_VAR,
                     sprintf("Slope variability SNR\n(%s rolling SD)", wlab),
                     show_xtitle = TRUE)

    cap <- sprintf(paste0("Signal-to-noise ratio (Exploited - Unexploited / baseline SD). ",
                          "Baseline: %s. Variability: %s %s rolling SD. ",
                          "Dashed red = +/-1 (emergence); dotted grey = +/-2."),
                   if (base_label == "base1901_1929") "1901-1929" else "full 1900-2010",
                   WINDOW_ALIGN, wlab)

    p_out <- (p_a / p_b / p_c / p_d) +
      plot_annotation(tag_levels = "a", caption = cap) &
      theme(plot.tag = element_text(face = "bold"),
            plot.caption = element_text(colour = "grey40", hjust = 0, size = 8))

    outfile <- sprintf("biomass_slope_snr_%s_%s_%s.png", wlab, WINDOW_ALIGN, base_label)
    ggsave(outfile, p_out, width = 9, height = 14, dpi = 300)
    message(sprintf("  Saved: %s", outfile))

    # --- record emergence years for every metric/combo ---
    for (m in list(list("Biomass", snr_bl),
                   list("Biomass variability", snr_bvar),
                   list("Slope", snr_beta),
                   list("Slope variability", snr_svar))) {
      for (thr in c(THRESH1, THRESH2)) {
        ey <- emergence_years(m[[2]], thr)
        emergence_rows[[length(emergence_rows) + 1]] <- data.frame(
          metric = m[[1]], window = wlab, align = WINDOW_ALIGN,
          baseline = base_label, threshold = thr,
          first_emergence = ey[["first"]], sustained_emergence = ey[["sustained"]],
          permanent_emergence = ey[["permanent"]]
        )
      }
    }

    # --- save tidy series for the primary combo ---
    if (wlab == PRIMARY_WIN && base_label == PRIMARY_BASE) {
      series_primary <- bind_rows(
        snr_bl   %>% mutate(metric = "Biomass"),
        snr_bvar %>% mutate(metric = "Biomass variability"),
        snr_beta %>% mutate(metric = "Slope"),
        snr_svar %>% mutate(metric = "Slope variability")
      ) %>% mutate(window = wlab, align = WINDOW_ALIGN, baseline = base_label)
    }
  }
}

# ---------------------------------------------------------------------------
# Write CSV outputs
# ---------------------------------------------------------------------------
if (!is.null(series_primary)) {
  write.csv(series_primary, "biomass_slope_snr_series.csv", row.names = FALSE)
  message("Saved: biomass_slope_snr_series.csv")
}
emergence_df <- bind_rows(emergence_rows)
write.csv(emergence_df, "biomass_slope_snr_emergence.csv", row.names = FALSE)
message("Saved: biomass_slope_snr_emergence.csv")

message("=== Done ===")
