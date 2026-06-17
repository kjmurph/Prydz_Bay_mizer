###############################################################################
# biomass_slope_stability_v2.R
#
# 4-panel figure (top-10% RMSE ensemble, 1900-2010), produced for three
# sliding window sizes: 3-year, 5-year, and 10-year.
# All panels show the paired difference (Exploited − Unexploited):
#
#   a  Delta total biomass (normalised fraction of unexploited)
#   b  Delta biomass stability (1/CV, sliding window)
#   c  Delta NBSS slope (LBNbiom, Biomass spectrum)
#   d  Delta slope stability (1/SD of slope, sliding window)
#
# Panels a & c are identical across window sizes (no sliding window used).
# Panels b & d use the window size for the rolling stability metric.
#
# Reference lines: -1 SD of the unexploited metric pooled across all
#   simulations and all years in 1901-1929 (combines temporal variability
#   and inter-simulation parameter spread).
#
# Inputs (pre-existing):
#   biomass_top10pct_raw_fish.rds
#   biomass_top10pct_raw_clim.rds
#   Output_large_files/community_slope_analysis/nbss_slope_top10pct_data.rds
#
# Outputs:
#   biomass_slope_stability_v2_3yr_top10pct.png
#   biomass_slope_stability_v2_5yr_top10pct.png
#   biomass_slope_stability_v2_10yr_top10pct.png
###############################################################################

if (!requireNamespace("slider", quietly = TRUE))
  stop("Install the 'slider' package first: install.packages('slider')")

suppressPackageStartupMessages({
  library(tidyverse)
  library(slider)
  library(patchwork)
  library(scales)
})

PLOT_YEARS <- 1900:2010
PAD_YEARS  <- (min(PLOT_YEARS) - 1L):(max(PLOT_YEARS) + 1L)

KEY_YEARS  <- c(1930, 1974)
KEY_LABELS <- c("Whaling\nstarts", "Krill\nfishing\nstarts")
DIFF_COL   <- "#5D4037"
BM_COL     <- "steelblue"
SLOPE_COL  <- "#8B1A1A"

# Window configurations: label, years before t, years after t
# Total window size = win_before + 1 + win_after
WINDOW_CONFIGS <- list(
  list(label = "3yr",  win_before = 1L, win_after = 1L),
  list(label = "5yr",  win_before = 2L, win_after = 2L),
  list(label = "10yr", win_before = 5L, win_after = 4L),
  list(label = "12yr", win_before = 6L, win_after = 5L),
  list(label = "15yr", win_before = 7L, win_after = 7L)
)

# ---------------------------------------------------------------------------
# 1. Biomass: load once, compute total per sim per year
# ---------------------------------------------------------------------------
message("Loading biomass caches...")
raw_fish <- readRDS("biomass_top10pct_raw_fish.rds")
raw_clim <- readRDS("biomass_top10pct_raw_clim.rds")

total_fish <- raw_fish %>%
  filter(Year %in% PAD_YEARS) %>%
  group_by(sim_i, Year) %>%
  summarise(total_g = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
  arrange(sim_i, Year)

total_clim <- raw_clim %>%
  filter(Year %in% PAD_YEARS) %>%
  group_by(sim_i, Year) %>%
  summarise(total_g = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
  arrange(sim_i, Year)

rm(raw_fish, raw_clim)

# ---------------------------------------------------------------------------
# 1a. Panel a: Delta total biomass — computed once (no sliding window)
# ---------------------------------------------------------------------------
delta_bm <- inner_join(total_fish, total_clim,
                       by = c("sim_i", "Year"), suffix = c("_fish", "_clim")) %>%
  mutate(delta_rel = (total_g_fish - total_g_clim) / total_g_clim) %>%
  filter(Year %in% PLOT_YEARS)

delta_bm_summary <- delta_bm %>%
  group_by(Year) %>%
  summarise(
    diff_median = median(delta_rel,         na.rm = TRUE),
    diff_q25    = quantile(delta_rel, 0.25, na.rm = TRUE),
    diff_q75    = quantile(delta_rel, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

clim_pre   <- total_clim$total_g[total_clim$Year %in% 1901:1929]
ref_delta_bm <- -(sd(clim_pre, na.rm = TRUE) / mean(clim_pre, na.rm = TRUE))
rm(clim_pre, delta_bm)

# ---------------------------------------------------------------------------
# 2. Slopes: load once
# ---------------------------------------------------------------------------
message("Loading NBSS slope data...")
slope_rds  <- readRDS(
  "Output_large_files/community_slope_analysis/nbss_slope_top10pct_data.rds"
)
all_slopes <- slope_rds$all_slopes %>%
  filter(spectrum_type == "Biomass", time %in% PAD_YEARS) %>%
  arrange(sim_id, time)
rm(slope_rds)

# ---------------------------------------------------------------------------
# 2a. Panel c: Delta NBSS slope — computed once (no sliding window)
# ---------------------------------------------------------------------------
slope_paired <- all_slopes %>%
  select(sim_id, time, ensemble, slope) %>%
  pivot_wider(names_from = ensemble, values_from = slope, values_fn = mean) %>%
  mutate(slope_diff = Exploited - Unexploited) %>%
  filter(!is.na(slope_diff), time %in% PLOT_YEARS)

delta_slope_summary <- slope_paired %>%
  group_by(time) %>%
  summarise(
    diff_median = median(slope_diff,         na.rm = TRUE),
    diff_q25    = quantile(slope_diff, 0.25, na.rm = TRUE),
    diff_q75    = quantile(slope_diff, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Year = time)

ref_delta_slope <- -sd(
  all_slopes$slope[all_slopes$ensemble == "Unexploited" &
                     all_slopes$time    %in% 1901:1929],
  na.rm = TRUE
)
rm(slope_paired)

# ---------------------------------------------------------------------------
# 3. Shared plot helpers
# ---------------------------------------------------------------------------
x_sc_clip <- scale_x_continuous(
  breaks = seq(1900, 2010, by = 20),
  expand = expansion(mult = c(0.01, 0.01)),
  limits = range(PLOT_YEARS)
)

base_th <- theme_classic(base_size = 11) +
  theme(
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    axis.title.x       = element_blank()
  )

key_vlines <- geom_vline(xintercept = KEY_YEARS, linetype = "dashed",
                         colour = "grey50", linewidth = 0.6, alpha = 0.8)

key_text <- annotate("text", x = KEY_YEARS - 0.25, y = Inf,
                     label = KEY_LABELS,
                     hjust = 1, vjust = 1.3, size = 3.2, colour = "grey35")

ref_line_label <- function(y, label) {
  annotate("text", x = max(PLOT_YEARS) + 1.5, y = y,
           label = label, hjust = 0, vjust = 1.2,
           size = 3.2, colour = "red")
}

# ---------------------------------------------------------------------------
# 4. Loop over window sizes — compute panels b & d, build and save figure
# ---------------------------------------------------------------------------
for (cfg in WINDOW_CONFIGS) {

  win_before <- cfg$win_before
  win_after  <- cfg$win_after
  win_label  <- cfg$label
  message(sprintf("Processing %s sliding window...", win_label))

  # Panel b: biomass stability (1/CV) per sim, then paired difference ----------
  inv_cv_fish <- total_fish %>%
    group_by(sim_i) %>%
    mutate(inv_cv = slide_dbl(
      total_g,
      ~{ s <- sd(.x); if (is.na(s) || s == 0) NA_real_ else mean(.x) / s },
      .before = win_before, .after = win_after, .complete = TRUE
    )) %>%
    ungroup() %>%
    filter(Year %in% PLOT_YEARS) %>%
    select(sim_i, Year, inv_cv)

  inv_cv_clim <- total_clim %>%
    group_by(sim_i) %>%
    mutate(inv_cv = slide_dbl(
      total_g,
      ~{ s <- sd(.x); if (is.na(s) || s == 0) NA_real_ else mean(.x) / s },
      .before = win_before, .after = win_after, .complete = TRUE
    )) %>%
    ungroup() %>%
    filter(Year %in% PLOT_YEARS) %>%
    select(sim_i, Year, inv_cv)

  delta_inv_cv_summary <- inner_join(
    inv_cv_fish, inv_cv_clim,
    by = c("sim_i", "Year"), suffix = c("_fish", "_clim")
  ) %>%
    mutate(delta_inv_cv = inv_cv_fish - inv_cv_clim) %>%
    group_by(Year) %>%
    summarise(
      median = median(delta_inv_cv,         na.rm = TRUE),
      q25    = quantile(delta_inv_cv, 0.25, na.rm = TRUE),
      q75    = quantile(delta_inv_cv, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  ref_delta_inv_cv <- -sd(
    inv_cv_clim$inv_cv[inv_cv_clim$Year %in% 1901:1929],
    na.rm = TRUE
  )
  rm(inv_cv_fish, inv_cv_clim)

  # Panel d: slope stability (1/SD) per sim, then paired difference -----------
  inv_sd_per_sim <- all_slopes %>%
    group_by(sim_id, ensemble) %>%
    mutate(inv_sd_slope = slide_dbl(
      slope,
      ~{ s <- sd(.x); if (is.na(s) || s == 0) NA_real_ else 1 / s },
      .before = win_before, .after = win_after, .complete = TRUE
    )) %>%
    ungroup() %>%
    filter(time %in% PLOT_YEARS)

  delta_inv_slope_sd_summary <- inv_sd_per_sim %>%
    select(sim_id, time, ensemble, inv_sd_slope) %>%
    pivot_wider(names_from  = ensemble, values_from = inv_sd_slope,
                values_fn = mean) %>%
    mutate(delta_inv_sd = Exploited - Unexploited) %>%
    filter(!is.na(delta_inv_sd)) %>%
    group_by(time) %>%
    summarise(
      median = median(delta_inv_sd,         na.rm = TRUE),
      q25    = quantile(delta_inv_sd, 0.25, na.rm = TRUE),
      q75    = quantile(delta_inv_sd, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(Year = time)

  ref_delta_inv_slope_sd <- -sd(
    inv_sd_per_sim$inv_sd_slope[inv_sd_per_sim$ensemble == "Unexploited" &
                                  inv_sd_per_sim$time    %in% 1901:1929],
    na.rm = TRUE
  )
  rm(inv_sd_per_sim)

  # Build panels --------------------------------------------------------------
  p_a <- ggplot(delta_bm_summary, aes(x = Year)) +
    geom_ribbon(aes(ymin = diff_q25, ymax = diff_q75),
                fill = DIFF_COL, alpha = 0.35, colour = NA) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.8) +
    geom_hline(yintercept = ref_delta_bm, linetype = "dashed",
               colour = "red", linewidth = 0.6) +
    geom_line(aes(y = diff_median), colour = DIFF_COL, linewidth = 0.9) +
    key_vlines + key_text +
    annotate("text", x = 1901, y = Inf,
             label = "Exploitation reduces biomass ↓",
             hjust = 0, vjust = 1.5, size = 3.25, colour = "grey45",
             fontface = "italic") +
    ref_line_label(ref_delta_bm, "1 SD\nfrom unexploited mean") +
    x_sc_clip +
    base_th + theme(plot.margin = margin(5.5, 80, 5.5, 5.5)) +
    coord_cartesian(clip = "off") +
    labs(y = "Δ Total biomass\n(Exploited − Unexploited,\nfraction of unexploited)")

  p_b <- ggplot(delta_inv_cv_summary, aes(x = Year)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = BM_COL, alpha = 0.35) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.8) +
    geom_hline(yintercept = ref_delta_inv_cv, linetype = "dashed",
               colour = "red", linewidth = 0.6) +
    geom_line(aes(y = median), colour = BM_COL, linewidth = 0.9) +
    ref_line_label(ref_delta_inv_cv, "1 SD\nof unexploited\n1/CV") +
    x_sc_clip +
    base_th + theme(plot.margin = margin(5.5, 80, 5.5, 5.5)) +
    coord_cartesian(clip = "off") +
    labs(y = sprintf("Δ Biomass stability (1/CV)\n(Exploited − Unexploited, %s window)",
                     win_label))

  p_c <- ggplot(delta_slope_summary, aes(x = Year)) +
    geom_ribbon(aes(ymin = diff_q25, ymax = diff_q75),
                fill = DIFF_COL, alpha = 0.35, colour = NA) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.8) +
    geom_hline(yintercept = ref_delta_slope, linetype = "dashed",
               colour = "red", linewidth = 0.6) +
    geom_line(aes(y = diff_median), colour = DIFF_COL, linewidth = 0.9) +
    key_vlines + key_text +
    annotate("text", x = 1901, y = Inf,
             label = "Exploitation steepens slope ↓",
             hjust = 0, vjust = 1.5, size = 3.25, colour = "grey45",
             fontface = "italic") +
    ref_line_label(ref_delta_slope, "1 SD\nfrom unexploited mean") +
    x_sc_clip +
    base_th + theme(plot.margin = margin(5.5, 80, 5.5, 5.5)) +
    coord_cartesian(clip = "off") +
    labs(y = expression(paste(Delta, " NBSS slope (Exploited − Unexploited)")))

  p_d <- ggplot(delta_inv_slope_sd_summary, aes(x = Year)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = SLOPE_COL, alpha = 0.35) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.8) +
    geom_hline(yintercept = ref_delta_inv_slope_sd, linetype = "dashed",
               colour = "red", linewidth = 0.6) +
    geom_line(aes(y = median), colour = SLOPE_COL, linewidth = 0.9) +
    ref_line_label(ref_delta_inv_slope_sd, "1 SD\nof unexploited\nslope 1/SD") +
    x_sc_clip +
    base_th + theme(axis.title.x = element_text(),
                    plot.margin = margin(5.5, 80, 5.5, 5.5)) +
    coord_cartesian(clip = "off") +
    labs(x = "Year",
         y = sprintf("Δ Slope stability (1/SD)\n(Exploited − Unexploited, %s window)",
                     win_label))

  p_out <- (p_a / p_b / p_c / p_d) +
    plot_annotation(tag_levels = "a") &
    theme(plot.tag = element_text(face = "bold"))

  outfile <- sprintf("biomass_slope_stability_v2_%s_top10pct.png", win_label)
  ggsave(outfile, p_out, width = 10, height = 16, dpi = 300)
  message(sprintf("Saved: %s", outfile))

  rm(inv_cv_clim, inv_sd_per_sim,
     delta_inv_cv_summary, delta_inv_slope_sd_summary,
     ref_delta_inv_cv, ref_delta_inv_slope_sd,
     p_a, p_b, p_c, p_d, p_out)
}

rm(total_fish, total_clim, all_slopes)
message("=== Done ===")
