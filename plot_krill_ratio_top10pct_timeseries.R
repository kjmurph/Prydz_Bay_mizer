###############################################################################
# Antarctic krill consumption ratio: exploited / unexploited
# Using the top-10% RMSE-filtered ensemble (212 paired sims)
#
# Three overlaid timeseries, 1900-2010:
#   - All predators combined (ribbon + line)  — total community signal
#   - Large baleen + minke whales (line)       — losers under exploitation
#   - Fishes (line)                            — winners under exploitation
#
# Central metric (paired framing): within each of the 212 matched ensemble
# members the exploited/unexploited krill-consumption ratio is formed FIRST
# (exploited ÷ unexploited, so climate/parameter differences cancel within the
# pair), and the paired ratios are THEN summarised across members as the median
# (line) and 25-75th percentile IQR (ribbon). This is the ratio analogue of the
# paired-difference (median + IQR) framing in biomass_slope_snr_mean_med.R.
#
# ±1 SD natural-variability reference (per group, colour-matched dashed lines):
# the noise is the SD of the UNEXPLOITED (climate-only) ensemble-mean krill
# consumption over the full 1841-2010 baseline, expressed as a CV (÷ its baseline
# mean) so it lands on the multiplicative ratio scale around the null of 1.0.
# This mirrors the 1841-2010 baseline of biomass_slope_snr_mean_med.R and the
# CV-on-ratio-scale reference of yield_ratio_combined_panels.R. Where a group's
# median ratio leaves its ±1 SD band, the exploitation effect has emerged beyond
# natural variability.
#
# Data source: full_diet_top10pct_fishing_all_sims.rds
#              full_diet_top10pct_climate_only_all_sims.rds
# Each element is a 3D array [year × predator × prey] of domain-wide
# consumption (g yr-1).
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

OUTPUT_DIR <- "whale_consumption_outputs"

###############################################################################
# Colours (identical to original figure)
###############################################################################
# Colours harmonised with the manuscript species palette (panel_colors in
# abundance_meanweight_rmse_top10pct_grid.R):
#   whales  -> baleen pink  #FF61C3  (baleen dominate this group's krill intake)
#   fishes  -> pelagic-fish ochre #D39200  (blue is reserved for orca/pinnipeds)
#   total   -> neutral charcoal (community aggregate; no single-species mapping)
FILL_COL  <- "grey65"    # ribbon fill — total all predators (neutral aggregate)
LINE_COL  <- "grey15"    # median line — total all predators
WHALE_COL <- "#FF61C3"   # large baleen + minke (losers)
FISH_COL  <- "#D39200"   # fishes (winners)

# Exploitation event lines, styled to match biomass_slope_snr_mean_med.R:
#   onset/offset events -> grey50 dashed;  peak-effort events -> grey50 dotted;
#   labels in grey35.
EVENT_COL    <- "grey50"   # vline colour (matches SNR figure)
EVENT_TXTCOL <- "grey35"   # label colour

# Dashed onset/offset lines
ONSET_YEARS  <- c(1930, 1974, 1996)
ONSET_LABELS <- c("Whaling\nstarts", "Krill fishing\nstarts", "End of\nkrill fishing")

# Dotted peak-effort lines (per-gear argmax of the model effort forcing;
# same years as PEAK_YEARS in biomass_slope_snr_mean_med.R)
PEAK_YEARS   <- c(1933, 1948, 1973, 1979)
PEAK_LABELS  <- c("Peak baleen whaling", "Peak sperm whaling",
                  "Peak minke whaling", "Peak krill fishing")

YEAR_MIN      <- 1900         # plot start year
BASELINE_YEARS <- 1841:2010   # unexploited natural-variability baseline (SNR)

###############################################################################
# Predator group definitions
###############################################################################
PREDS_BALEEN_MINKE <- c("baleen whales", "minke whales")
PREDS_TOOTHED      <- c("sperm whales", "orca")
PREDS_FISH         <- c("mesopelagic fishes", "bathypelagic fishes",
                        "shelf and coastal fishes", "toothfishes")
PREDS_SEALS        <- c("leopard seals", "small divers",
                        "medium divers", "large divers")
PREY_KRILL         <- "antarctic krill"

###############################################################################
# Helper: extract per-sim annual krill consumption for a predator group
# Returns a list of data.frames(year, total_consumption), one per sim
###############################################################################
extract_group_krill <- function(arr_list, pred_names) {
  lapply(arr_list, function(arr) {
    years       <- as.numeric(dimnames(arr)$year)
    valid_preds <- intersect(pred_names, dimnames(arr)$predator)
    if (length(valid_preds) == 0) {
      return(data.frame(year = years, total_consumption = 0))
    }
    slice <- arr[, valid_preds, PREY_KRILL, drop = FALSE]
    vals  <- if (length(valid_preds) == 1) as.numeric(slice) else rowSums(slice)
    data.frame(year = years, total_consumption = as.numeric(vals))
  })
}

###############################################################################
# Helper: element-wise sum of per-sim consumption lists
###############################################################################
sum_sim_lists <- function(...) {
  lists <- list(...)
  lapply(seq_along(lists[[1]]), function(i) {
    total <- lists[[1]][[i]]$total_consumption
    for (lst in lists[-1]) total <- total + lst[[i]]$total_consumption
    data.frame(year = lists[[1]][[i]]$year, total_consumption = total)
  })
}

###############################################################################
# Helper: compute annual exploited/unexploited ratio ensemble quantiles
###############################################################################
compute_annual_ratio <- function(fish_list, clim_list, year_min = YEAR_MIN) {
  rows <- lapply(seq_along(fish_list), function(i) {
    f   <- fish_list[[i]]
    cl  <- clim_list[[i]]
    mrg <- merge(f, cl, by = "year", suffixes = c("_f", "_c"))
    mrg$ratio <- ifelse(mrg$total_consumption_c > 0,
                        mrg$total_consumption_f / mrg$total_consumption_c,
                        NA_real_)
    mrg[mrg$year >= year_min, c("year", "ratio")]
  })

  do.call(rbind, rows) %>%
    group_by(year) %>%
    summarise(
      q25 = quantile(ratio, 0.25, na.rm = TRUE),
      med = median(ratio,         na.rm = TRUE),
      q75 = quantile(ratio, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
}

###############################################################################
# Helper: per-group ±1 SD natural-variability band (in ratio units)
#
# Noise follows the SNR framing of biomass_slope_snr_mean_med.R: the SD, over
# the full unexploited 1841-2010 baseline, of the ensemble-mean UNEXPLOITED
# (climate-only) krill-consumption trajectory. It is divided by that baseline
# mean (a CV) so it maps onto the multiplicative ratio scale around the null of
# 1.0, exactly as yield_ratio_combined_panels.R does for its ratio panel.
# Returns sd_lo / sd_hi (band edges) and sigma (the CV itself).
###############################################################################
compute_sd_band <- function(clim_list, baseline_years = BASELINE_YEARS) {
  rep_unexp <- do.call(rbind, clim_list) %>%
    group_by(year) %>%
    summarise(mu = mean(total_consumption, na.rm = TRUE), .groups = "drop") %>%
    filter(year %in% baseline_years)

  ref_mean <- mean(rep_unexp$mu, na.rm = TRUE)
  noise    <- sd(rep_unexp$mu,   na.rm = TRUE)
  sigma    <- noise / ref_mean

  data.frame(sd_lo = max(1 - sigma, 0), sd_hi = 1 + sigma, sigma = sigma)
}

###############################################################################
# Load top-10% diet arrays
###############################################################################
cat("Loading top-10% RMSE diet arrays...\n")

fish_arrays <- readRDS(file.path(OUTPUT_DIR, "full_diet_top10pct_fishing_all_sims.rds"))
clim_arrays <- readRDS(file.path(OUTPUT_DIR, "full_diet_top10pct_climate_only_all_sims.rds"))

n_sims <- length(fish_arrays)
cat(sprintf("  %d simulation pairs loaded\n", n_sims))

# Report predator/prey names from first array
preds_available <- dimnames(fish_arrays[[1]])$predator
preys_available <- dimnames(fish_arrays[[1]])$prey
cat(sprintf("  Predators: %s\n", paste(preds_available, collapse = ", ")))
cat(sprintf("  Prey items: %s\n\n", paste(preys_available, collapse = ", ")))

###############################################################################
# Extract per-sim krill consumption by predator group
###############################################################################
cat("Extracting per-sim krill consumption by group...\n")

cat("  Baleen + minke whales (fishing)...\n")
f_baleen_minke <- extract_group_krill(fish_arrays, PREDS_BALEEN_MINKE)
c_baleen_minke <- extract_group_krill(clim_arrays, PREDS_BALEEN_MINKE)

cat("  Toothed whales (fishing)...\n")
f_toothed <- extract_group_krill(fish_arrays, PREDS_TOOTHED)
c_toothed <- extract_group_krill(clim_arrays, PREDS_TOOTHED)

cat("  Fishes (fishing)...\n")
f_fish <- extract_group_krill(fish_arrays, PREDS_FISH)
c_fish <- extract_group_krill(clim_arrays, PREDS_FISH)

cat("  Seals (fishing)...\n")
f_seals <- extract_group_krill(fish_arrays, PREDS_SEALS)
c_seals <- extract_group_krill(clim_arrays, PREDS_SEALS)

cat("  All predators (sum)...\n")
f_total <- sum_sim_lists(f_baleen_minke, f_toothed, f_fish, f_seals)
c_total <- sum_sim_lists(c_baleen_minke, c_toothed, c_fish, c_seals)

###############################################################################
# Compute ratios
###############################################################################
cat("\nComputing ratios...\n")

cat("  Total all predators...\n")
total_summary <- compute_annual_ratio(f_total, c_total)

cat("  Large baleen + minke whales...\n")
whale_summary <- compute_annual_ratio(f_baleen_minke, c_baleen_minke)

cat("  Fishes...\n")
fish_summary  <- compute_annual_ratio(f_fish, c_fish)

cat(sprintf("\n  Year range: %d – %d\n", min(total_summary$year), max(total_summary$year)))
cat(sprintf("  Total median range:  %.3f – %.3f\n", min(total_summary$med), max(total_summary$med)))
cat(sprintf("  Whale median range:  %.3f – %.3f\n", min(whale_summary$med), max(whale_summary$med)))
cat(sprintf("  Fish  median range:  %.3f – %.3f\n\n", min(fish_summary$med),  max(fish_summary$med)))

###############################################################################
# Per-group ±1 SD natural-variability bands (1841-2010 unexploited baseline)
###############################################################################
cat("Computing ±1 SD natural-variability bands...\n")
sd_total <- compute_sd_band(c_total)
sd_whale <- compute_sd_band(c_baleen_minke)
sd_fish  <- compute_sd_band(c_fish)

cat(sprintf("  All predators: sigma = %.4f  ->  band [%.3f, %.3f]\n",
            sd_total$sigma, sd_total$sd_lo, sd_total$sd_hi))
cat(sprintf("  Whales:        sigma = %.4f  ->  band [%.3f, %.3f]\n",
            sd_whale$sigma, sd_whale$sd_lo, sd_whale$sd_hi))
cat(sprintf("  Fishes:        sigma = %.4f  ->  band [%.3f, %.3f]\n\n",
            sd_fish$sigma,  sd_fish$sd_lo,  sd_fish$sd_hi))

###############################################################################
# Build long-format ratio data
###############################################################################
total_summary$group <- "All predators"
whale_summary$group <- "Large baleen + minke whales"
fish_summary$group  <- "All fishes"   # sum of all fish groups (meso/bathy-pelagic,
                                       # shelf & coastal, toothfishes)

ratio_all <- bind_rows(total_summary, whale_summary, fish_summary)
ratio_all$group <- factor(ratio_all$group,
                           levels = c("All predators",
                                      "Large baleen + minke whales",
                                      "All fishes"))

LINE_COLS <- c(
  "All predators"               = LINE_COL,
  "Large baleen + minke whales" = WHALE_COL,
  "All fishes"                  = FISH_COL
)
LINE_WIDTHS <- c(
  "All predators"               = 1.0,
  "Large baleen + minke whales" = 0.9,
  "All fishes"                  = 0.9
)

###############################################################################
# Plot — identical layout to krill_ratio_all_predators_timeseries_no_catch.R
###############################################################################
cat("Generating plot...\n")

p <- ggplot() +

  # ---- Reference line at ratio = 1 ----
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50",
             linewidth = 0.7) +
  annotate("text", x = YEAR_MIN, y = 1, label = "Unexploited",
           hjust = 0, vjust = -1.1, size = 3.0, colour = "grey50") +

  # ---- IQR ribbons ----
  geom_ribbon(data    = total_summary,
              mapping = aes(x = year, ymin = q25, ymax = q75),
              fill = FILL_COL, alpha = 0.55, colour = NA) +
  geom_ribbon(data    = whale_summary,
              mapping = aes(x = year, ymin = q25, ymax = q75),
              fill = WHALE_COL, alpha = 0.25, colour = NA) +
  geom_ribbon(data    = fish_summary,
              mapping = aes(x = year, ymin = q25, ymax = q75),
              fill = FISH_COL, alpha = 0.25, colour = NA) +

  # ---- Per-group ±1 SD natural-variability references (colour-matched) ----
  # Dashed horizontal lines at 1 ± CV of the unexploited 1841-2010 baseline.
  # A group's median ratio leaving its band = change beyond natural variability.
  geom_hline(yintercept = c(sd_total$sd_lo, sd_total$sd_hi),
             linetype = "dashed", colour = LINE_COL,  linewidth = 0.5) +
  geom_hline(yintercept = c(sd_whale$sd_lo, sd_whale$sd_hi),
             linetype = "dashed", colour = WHALE_COL, linewidth = 0.5) +
  geom_hline(yintercept = c(sd_fish$sd_lo,  sd_fish$sd_hi),
             linetype = "dashed", colour = FISH_COL,  linewidth = 0.5) +

  # ---- Exploitation event lines (grey, matching the SNR figure) ----
  # Onset/offset events: grey dashed, labels horizontal near the top.
  geom_vline(xintercept = ONSET_YEARS, linetype = "dashed",
             colour = EVENT_COL, linewidth = 0.6) +
  annotate("text", x = ONSET_YEARS, y = Inf, label = ONSET_LABELS,
           hjust = -0.06, vjust = 1.3, size = 2.9, colour = EVENT_TXTCOL) +
  # Peak-effort events: grey dotted, labels rotated 90° along each line.
  geom_vline(xintercept = PEAK_YEARS, linetype = "dotted",
             colour = EVENT_COL, linewidth = 0.6) +
  annotate("text", x = PEAK_YEARS - 1.2, y = -Inf, label = PEAK_LABELS,
           angle = 90, hjust = -0.05, vjust = 0.5, size = 2.6,
           colour = EVENT_TXTCOL) +

  # ---- Median lines ----
  geom_line(data    = ratio_all,
            mapping = aes(x = year, y = med,
                          colour    = group,
                          linewidth = group)) +
  scale_colour_manual(values = LINE_COLS,  name = "Predator group") +
  scale_linewidth_manual(values = LINE_WIDTHS, name = "Predator group") +

  # ---- Axes ----
  scale_x_continuous(
    name   = "Year",
    breaks = seq(YEAR_MIN, 2010, by = 10),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    name   = "Exploited / Unexploited Antarctic krill consumption (ratio)",
    breaks = seq(0, 1.25, by = 0.25),
    labels = function(x) sprintf("%.2f", x),
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.08))
  ) +

  # ---- Theme ----
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 40, hjust = 1),
    legend.position  = "bottom",
    legend.box       = "horizontal",
    legend.key.size  = unit(0.45, "cm"),
    legend.text      = element_text(size = 8.5),
    legend.title     = element_text(size = 9, face = "bold"),
    plot.margin      = margin(8, 12, 4, 4, "pt")
  ) +
  guides(
    colour    = guide_legend(order = 1, override.aes = list(linewidth = 1.1)),
    linewidth = "none"
  )

###############################################################################
# Save
###############################################################################
png_path <- file.path(OUTPUT_DIR, "krill_ratio_top10pct_timeseries.png")
pdf_path <- file.path(OUTPUT_DIR, "krill_ratio_top10pct_timeseries.pdf")

ggsave(png_path, p, width = 8, height = 5.5, dpi = 300)
ggsave(pdf_path, p, width = 8, height = 5.5)

cat(sprintf("Saved: %s\n", png_path))
cat(sprintf("Saved: %s\n", pdf_path))
cat("Done.\n")
