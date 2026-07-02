###############################################################################
# biomass_slope_snr_decomposition.R
#
# SUPPLEMENTARY driver-attribution figure for the top-10% RMSE ensemble
# (212 matched members), 1900-2010. Decomposes the signal into three curves,
# all expressed as SNR (divided by the same natural-variability noise N), using
# the FULL unexploited record 1841-2010 as the reference baseline:
#
#   Fishing only  = (Exploited - Unexploited)            / N   [paired; climate
#                                                               cancels]
#   Climate only  = (Unexploited - baseline_mean)        / N   [unexploited run's
#                                                               own departure]
#   Combined      = (Exploited   - baseline_mean)        / N   [fishing + climate]
#
# By construction Combined = Fishing only + Climate only. baseline_mean and N are
# the mean and SD of the *unexploited* ensemble-mean trajectory over 1841-2010.
# The "Fishing only" curve here is identical to the ensemble-MEAN SNR (snr_mean)
# of biomass_slope_snr(_mean_med).R at the 1841-2010 baseline.
#
# Purpose / interpretation: an independent check confirmed that the unexploited
# (climate-only) trajectory carries no detectable climate-change TREND in total
# biomass or slope over 1841-2010 -- the Climate-only curve never permanently
# leaves +/-1 SD. It still fluctuates by ~1 SD (that IS the natural variability
# N), so it is not flat; but with no trend, exploitation drives the emergence.
# For the slope, whose natural variability is tiny, Combined sits essentially on
# top of Fishing-only. For biomass, both share the same downward trend but
# Combined is noisier because natural variability is larger there, so it emerges
# later (permanent >1 only ~2006) than the clean paired Fishing-only (1935). The
# paired signal removes natural variability and emerges earliest/cleanest. This
# justifies using the exploited-vs-unexploited paired difference (full 1841-2010
# unexploited baseline) as an "exploitation only" signal. (Caveat for the paper:
# climate may still matter at the species / size-class level, which these
# community-aggregate metrics mask.)
#
# Inputs (in "Manuscript data/"; the 1.8 GB ensemble is NOT re-run). Run from the
# repo root so these relative paths resolve:
#   Manuscript data/biomass_top10pct_raw_fish.rds / _clim.rds
#   Manuscript data/nbss_slope_top10pct_data.rds
#
# Outputs:
#   biomass_slope_snr_decomposition_10yr_trailing_base1841_2010.png
#   biomass_slope_snr_decomposition_series.csv
#   biomass_slope_snr_decomposition_emergence.csv
###############################################################################

if (!requireNamespace("slider", quietly = TRUE))
  stop("Install the 'slider' package first: install.packages('slider')")

suppressPackageStartupMessages({
  library(tidyverse)
  library(slider)
  library(patchwork)
  library(scales)
})

message("=== Biomass & slope SNR -- driver decomposition (fishing / climate / combined) ===")

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
PLOT_YEARS     <- 1900:2010
BASELINE_YEARS <- 1841:2010          # full unexploited record (climate negligible)
BASE_LABEL     <- "base1841_2010"

KEY_YEARS  <- c(1930, 1974)
KEY_LABELS <- c("Whaling\nstarts", "Krill\nfishing\nstarts")

WIN_BEFORE <- 9L                     # 10yr trailing / causal rolling window
WIN_AFTER  <- 0L
WLAB       <- "10yr"

THRESH1 <- 1
THRESH2 <- 2

COL_FISH  <- "black"
COL_CLIM  <- "#2166AC"
COL_COMB  <- "#B2182B"
LAB_FISH  <- "Fishing only (Exp - Unexp)"
LAB_CLIM  <- "Climate only (Unexp - baseline)"
LAB_COMB  <- "Combined (Exp - baseline)"

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
roll_sd <- function(x, before, after) {
  slide_dbl(x, ~ sd(.x), .before = before, .after = after, .complete = TRUE)
}

# Given exploited/unexploited ensemble-mean trajectories (cols Year, val),
# return the three SNR curves (fishing / climate / combined) per plotted year.
make_decomp <- function(exp_df, unexp_df) {
  d <- inner_join(exp_df, unexp_df, by = "Year", suffix = c("_e", "_u")) %>%
    arrange(Year)
  base_idx  <- d$Year %in% BASELINE_YEARS
  ref_mean  <- mean(d$val_u[base_idx], na.rm = TRUE)
  N         <- sd(d$val_u[base_idx],   na.rm = TRUE)
  if (is.na(N) || N == 0) { warning("Baseline noise NA/0; SNR set to NA."); N <- NA_real_ }
  d %>%
    mutate(fishing  = (val_e - val_u)    / N,
           climate  = (val_u - ref_mean) / N,
           combined = (val_e - ref_mean) / N) %>%
    filter(Year %in% PLOT_YEARS) %>%
    select(Year, fishing, climate, combined)
}

# Time of emergence on a single SNR curve: first crossing and permanent
# (stays beyond threshold for the rest of the record).
emergence <- function(z, yr, thr) {
  ok <- !is.na(z); zz <- z[ok]; yy <- yr[ok]
  if (length(zz) == 0) return(c(first = NA, permanent = NA))
  ex <- abs(zz) > thr
  first <- if (any(ex)) yy[which(ex)[1]] else NA_real_
  below <- which(!ex)
  perm <- if (!any(ex)) NA_real_ else if (length(below) == 0) yy[1] else
    if (max(below) < length(ex)) yy[max(below) + 1] else NA_real_
  c(first = first, permanent = perm)
}

# ---------------------------------------------------------------------------
# Load caches -> exploited & unexploited ensemble-mean trajectories
# ---------------------------------------------------------------------------
message("Loading biomass caches...")
raw_fish <- readRDS("Manuscript data/biomass_top10pct_raw_fish.rds")
raw_clim <- readRDS("Manuscript data/biomass_top10pct_raw_clim.rds")

total_fish <- raw_fish %>% group_by(sim_i, Year) %>%
  summarise(total_g = sum(Biomass, na.rm = TRUE), .groups = "drop")
total_clim <- raw_clim %>% group_by(sim_i, Year) %>%
  summarise(total_g = sum(Biomass, na.rm = TRUE), .groups = "drop")
rm(raw_fish, raw_clim)

# Level: ensemble-mean total biomass
bm_exp   <- total_fish %>% group_by(Year) %>% summarise(val = mean(total_g, na.rm = TRUE), .groups = "drop")
bm_unexp <- total_clim %>% group_by(Year) %>% summarise(val = mean(total_g, na.rm = TRUE), .groups = "drop")

# Variability: per-member 10yr trailing rolling SD, then ensemble mean
bmv_exp <- total_fish %>% group_by(sim_i) %>% arrange(Year) %>%
  mutate(rs = roll_sd(total_g, WIN_BEFORE, WIN_AFTER)) %>% ungroup() %>%
  group_by(Year) %>% summarise(val = mean(rs, na.rm = TRUE), .groups = "drop")
bmv_unexp <- total_clim %>% group_by(sim_i) %>% arrange(Year) %>%
  mutate(rs = roll_sd(total_g, WIN_BEFORE, WIN_AFTER)) %>% ungroup() %>%
  group_by(Year) %>% summarise(val = mean(rs, na.rm = TRUE), .groups = "drop")

message("Loading NBSS slope data...")
all_slopes <- readRDS(
  "Manuscript data/nbss_slope_top10pct_data.rds"
)$all_slopes %>% filter(spectrum_type == "Biomass") %>% arrange(sim_id, ensemble, time)

beta_exp   <- all_slopes %>% filter(ensemble == "Exploited")   %>%
  group_by(time) %>% summarise(val = mean(slope, na.rm = TRUE), .groups = "drop") %>% rename(Year = time)
beta_unexp <- all_slopes %>% filter(ensemble == "Unexploited") %>%
  group_by(time) %>% summarise(val = mean(slope, na.rm = TRUE), .groups = "drop") %>% rename(Year = time)

sl_roll <- all_slopes %>% group_by(sim_id, ensemble) %>% arrange(time) %>%
  mutate(rs = roll_sd(slope, WIN_BEFORE, WIN_AFTER)) %>% ungroup()
betav_exp   <- sl_roll %>% filter(ensemble == "Exploited")   %>%
  group_by(time) %>% summarise(val = mean(rs, na.rm = TRUE), .groups = "drop") %>% rename(Year = time)
betav_unexp <- sl_roll %>% filter(ensemble == "Unexploited") %>%
  group_by(time) %>% summarise(val = mean(rs, na.rm = TRUE), .groups = "drop") %>% rename(Year = time)

# ---------------------------------------------------------------------------
# Build the three curves for each metric
# ---------------------------------------------------------------------------
metrics <- list(
  list(name = "Biomass",              ylab = "Biomass SNR",
       exp = bm_exp,    unexp = bm_unexp),
  list(name = "Biomass variability",  ylab = "Biomass variability SNR\n(10yr rolling SD)",
       exp = bmv_exp,   unexp = bmv_unexp),
  list(name = "Slope",                ylab = "Size-spectrum slope SNR",
       exp = beta_exp,  unexp = beta_unexp),
  list(name = "Slope variability",    ylab = "Slope variability SNR\n(10yr rolling SD)",
       exp = betav_exp, unexp = betav_unexp)
)
decomp <- lapply(metrics, function(m) make_decomp(m$exp, m$unexp))
names(decomp) <- vapply(metrics, `[[`, character(1), "name")

# ---------------------------------------------------------------------------
# Plot scaffolding
# ---------------------------------------------------------------------------
x_sc <- scale_x_continuous(breaks = seq(1900, 2010, by = 20),
                           expand = expansion(mult = c(0.01, 0.01)),
                           limits = range(PLOT_YEARS))
base_th <- theme_classic(base_size = 11) +
  theme(panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
        axis.title.x = element_blank())
key_vlines <- geom_vline(xintercept = KEY_YEARS, linetype = "dashed",
                         colour = "grey50", linewidth = 0.6, alpha = 0.8)
key_text <- annotate("text", x = KEY_YEARS - 0.25, y = Inf, label = KEY_LABELS,
                     hjust = 1, vjust = 1.3, size = 3.0, colour = "grey35")

decomp_panel <- function(df, ylab, show_keys = FALSE, show_xtitle = FALSE) {
  p <- ggplot(df, aes(x = Year)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_hline(yintercept = c(-THRESH1, THRESH1), linetype = "dashed",
               colour = "red", linewidth = 0.5) +
    geom_hline(yintercept = c(-THRESH2, THRESH2), linetype = "dotted",
               colour = "grey45", linewidth = 0.4) +
    key_vlines +
    # combined drawn first (thick), fishing dashed on top -> overlap visible
    geom_line(aes(y = combined, colour = LAB_COMB), linewidth = 1.3) +
    geom_line(aes(y = climate,  colour = LAB_CLIM), linewidth = 0.9) +
    geom_line(aes(y = fishing,  colour = LAB_FISH), linewidth = 0.8, linetype = "22") +
    scale_colour_manual(name = NULL,
                        values = setNames(c(COL_COMB, COL_CLIM, COL_FISH),
                                          c(LAB_COMB, LAB_CLIM, LAB_FISH)),
                        breaks = c(LAB_FISH, LAB_CLIM, LAB_COMB)) +
    x_sc + base_th + coord_cartesian(clip = "off") + labs(y = ylab)
  if (show_keys)   p <- p + key_text
  if (show_xtitle) p <- p + theme(axis.title.x = element_text()) + labs(x = "Year")
  p
}

p_a <- decomp_panel(decomp[["Biomass"]],             "Biomass SNR", show_keys = TRUE)
p_b <- decomp_panel(decomp[["Biomass variability"]], "Biomass variability SNR\n(10yr rolling SD)")
p_c <- decomp_panel(decomp[["Slope"]],               "Size-spectrum slope SNR", show_keys = TRUE)
p_d <- decomp_panel(decomp[["Slope variability"]],   "Slope variability SNR\n(10yr rolling SD)",
                    show_xtitle = TRUE)

cap <- paste0(
  "Driver decomposition (all curves / natural-variability SD N). Baseline = full ",
  "unexploited record 1841-2010. Fishing only = (Exploited - Unexploited)/N ",
  "(climate cancels); Climate only = (Unexploited - baseline mean)/N; Combined = ",
  "(Exploited - baseline mean)/N = Fishing + Climate. Climate-only shows no trend ",
  "(never permanently emerges), so exploitation drives the trend; the paired ",
  "Fishing-only signal removes natural variability, whereas Combined carries it ",
  "(hence noisier/later for biomass). Dashed red = +/-1; dotted grey = +/-2.")

p_out <- (p_a / p_b / p_c / p_d) +
  plot_annotation(tag_levels = "a", caption = cap) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey40", hjust = 0, size = 8))

outfile <- sprintf("biomass_slope_snr_decomposition_%s_trailing_%s.png", WLAB, BASE_LABEL)
ggsave(outfile, p_out, width = 9, height = 14, dpi = 300)
message(sprintf("  Saved: %s", outfile))

# ---------------------------------------------------------------------------
# CSV outputs: tidy series + emergence table
# ---------------------------------------------------------------------------
series <- bind_rows(lapply(names(decomp), function(nm) {
  decomp[[nm]] %>% pivot_longer(c(fishing, climate, combined),
                                names_to = "signal", values_to = "snr") %>%
    mutate(metric = nm)
})) %>% mutate(window = WLAB, align = "trailing", baseline = BASE_LABEL)
write.csv(series, "biomass_slope_snr_decomposition_series.csv", row.names = FALSE)
message("Saved: biomass_slope_snr_decomposition_series.csv")

emergence_rows <- list()
sig_map <- c(fishing = LAB_FISH, climate = LAB_CLIM, combined = LAB_COMB)
for (nm in names(decomp)) {
  df <- decomp[[nm]]
  for (sig in c("fishing", "climate", "combined")) {
    for (thr in c(THRESH1, THRESH2)) {
      ey <- emergence(df[[sig]], df$Year, thr)
      emergence_rows[[length(emergence_rows) + 1]] <- data.frame(
        metric = nm, signal = sig_map[[sig]], threshold = thr,
        first_emergence = ey[["first"]], permanent_emergence = ey[["permanent"]],
        baseline = BASE_LABEL)
    }
  }
}
write.csv(bind_rows(emergence_rows), "biomass_slope_snr_decomposition_emergence.csv",
          row.names = FALSE)
message("Saved: biomass_slope_snr_decomposition_emergence.csv")

message("=== Done ===")