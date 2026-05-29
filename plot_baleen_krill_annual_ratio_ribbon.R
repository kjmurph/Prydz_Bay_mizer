###############################################################################
# Baleen whale Antarctic krill consumption: exploited / unexploited ratio
# Annual median line + IQR ribbon, 1901–2010
#
# Outputs:
#   whale_consumption_outputs/baleen_krill_annual_ratio_ribbon.png
#   whale_consumption_outputs/baleen_krill_annual_ratio_ribbon.pdf
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

OUTPUT_DIR <- "whale_consumption_outputs"

# ---------------------------------------------------------------------------
# Load per-simulation data (Antarctic krill only)
# ---------------------------------------------------------------------------
cat("Loading fishing ensemble baleen krill...\n")
fish_sims <- readRDS(file.path(OUTPUT_DIR, "fishing_baleen_krill_all_sims.rds"))
cat("Loading climate-only ensemble baleen krill...\n")
clim_sims <- readRDS(file.path(OUTPUT_DIR, "climate_only_baleen_krill_all_sims.rds"))

n_sims <- length(fish_sims)
cat(sprintf("  %d simulation pairs\n\n", n_sims))

# ---------------------------------------------------------------------------
# Compute per-simulation, per-year ratio then summarise across sims
# ---------------------------------------------------------------------------
cat("Computing annual ratios...\n")

ratio_rows <- vector("list", n_sims)

for (i in seq_len(n_sims)) {
  f <- fish_sims[[i]]
  c <- clim_sims[[i]]

  merged <- merge(f, c, by = "year", suffixes = c("_fish", "_clim"))
  merged$ratio <- ifelse(merged$total_consumption_clim > 0,
                         merged$total_consumption_fish / merged$total_consumption_clim,
                         NA_real_)
  ratio_rows[[i]] <- merged[, c("year", "ratio")]
}

ratio_df <- do.call(rbind, ratio_rows)
ratio_df <- ratio_df[ratio_df$year >= 1901, ]

annual_summary <- ratio_df %>%
  group_by(year) %>%
  summarise(
    q25 = quantile(ratio, 0.25, na.rm = TRUE),
    med = median(ratio,         na.rm = TRUE),
    q75 = quantile(ratio, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("  Years: %d - %d\n", min(annual_summary$year), max(annual_summary$year)))
cat(sprintf("  Median range: %.3f - %.3f\n\n",
            min(annual_summary$med), max(annual_summary$med)))

# ---------------------------------------------------------------------------
# Plot
# ---------------------------------------------------------------------------
cat("Generating ribbon plot...\n")

FILL_COL       <- "#4daf8d"
LINE_COL       <- "#1a4d38"
KRILL_START    <- 1974
KRILL_END      <- 1996
VLINE_COL      <- "#8c4a2f"

# Whaling era annotation bars
BALEEN_ERA_START <- 1930
BALEEN_ERA_END   <- 1973
MINKE_ERA_START  <- 1963
MINKE_ERA_END    <- 2010   # truncated to plot range (effort continues to 2019)
ERA_Y_BALEEN     <- 0.08   # y position of baleen era bar (bottom of plot)
ERA_Y_MINKE      <- 0.03   # y position of minke era bar (below baleen, bottom of plot)
ERA_TICK_H       <- 0.022  # half-height of bookend tick marks
ERA_COL_BALEEN   <- "#2166ac"
ERA_COL_MINKE    <- "#b2182b"

p <- ggplot(annual_summary, aes(x = year)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50",
             linewidth = 0.7) +
  annotate("text", x = 1901, y = 1, label = "Unexploited",
           hjust = 0, vjust = -0.5, size = 3.2, colour = "grey50") +
  # Krill fishing onset
  geom_vline(xintercept = KRILL_START, linetype = "dashed",
             colour = VLINE_COL, linewidth = 0.7) +
  annotate("text", x = KRILL_START, y = Inf, label = "Start of krill fishing",
           hjust = -0.07, vjust = 1.4, size = 3.0, colour = VLINE_COL,
           angle = 0) +
  # Krill fishing end
  geom_vline(xintercept = KRILL_END, linetype = "dashed",
             colour = VLINE_COL, linewidth = 0.7) +
  annotate("text", x = KRILL_END, y = Inf, label = "End of krill fishing",
           hjust = -0.07, vjust = 1.4, size = 3.0, colour = VLINE_COL,
           angle = 0) +
  # Baleen whale hunting era bar with bookend ticks
  annotate("segment",
           x = BALEEN_ERA_START, xend = BALEEN_ERA_END,
           y = ERA_Y_BALEEN, yend = ERA_Y_BALEEN,
           colour = ERA_COL_BALEEN, linewidth = 0.9) +
  annotate("segment",
           x = BALEEN_ERA_START, xend = BALEEN_ERA_START,
           y = ERA_Y_BALEEN - ERA_TICK_H, yend = ERA_Y_BALEEN + ERA_TICK_H,
           colour = ERA_COL_BALEEN, linewidth = 0.9) +
  annotate("segment",
           x = BALEEN_ERA_END, xend = BALEEN_ERA_END,
           y = ERA_Y_BALEEN - ERA_TICK_H, yend = ERA_Y_BALEEN + ERA_TICK_H,
           colour = ERA_COL_BALEEN, linewidth = 0.9) +
  annotate("text",
           x = (BALEEN_ERA_START + BALEEN_ERA_END) / 2,
           y = ERA_Y_BALEEN + ERA_TICK_H + 0.008,
           label = "Baleen whale exploitation",
           vjust = 0, hjust = 0.5, size = 2.7, colour = ERA_COL_BALEEN) +
  # Minke whale hunting era bar with bookend ticks
  annotate("segment",
           x = MINKE_ERA_START, xend = MINKE_ERA_END,
           y = ERA_Y_MINKE, yend = ERA_Y_MINKE,
           colour = ERA_COL_MINKE, linewidth = 0.9) +
  annotate("segment",
           x = MINKE_ERA_START, xend = MINKE_ERA_START,
           y = ERA_Y_MINKE - ERA_TICK_H, yend = ERA_Y_MINKE + ERA_TICK_H,
           colour = ERA_COL_MINKE, linewidth = 0.9) +
  annotate("segment",
           x = MINKE_ERA_END, xend = MINKE_ERA_END,
           y = ERA_Y_MINKE - ERA_TICK_H, yend = ERA_Y_MINKE + ERA_TICK_H,
           colour = ERA_COL_MINKE, linewidth = 0.9) +
  annotate("text",
           x = (MINKE_ERA_START + MINKE_ERA_END) / 2,
           y = ERA_Y_MINKE + ERA_TICK_H + 0.008,
           label = "Minke whale exploitation",
           vjust = 0, hjust = 0.5, size = 2.7, colour = ERA_COL_MINKE) +
  geom_ribbon(aes(ymin = q25, ymax = q75),
              fill = FILL_COL, alpha = 0.5) +
  geom_line(aes(y = med),
            colour = LINE_COL, linewidth = 0.8) +
  scale_x_continuous(
    name   = "Year",
    breaks = seq(1901, 2010, by = 10),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    name   = "Exploited / Unexploited Antarctic krill consumption (ratio)",
    breaks = seq(0, 1, by = 0.25),
    labels = function(x) sprintf("%.2f", x),
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 40, hjust = 1),
    plot.margin      = margin(8, 12, 4, 4, "pt")
  )

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------
png_path <- file.path(OUTPUT_DIR, "baleen_krill_annual_ratio_ribbon.png")
pdf_path <- file.path(OUTPUT_DIR, "baleen_krill_annual_ratio_ribbon.pdf")

ggsave(png_path, p, width = 9, height = 5, dpi = 300)
ggsave(pdf_path, p, width = 9, height = 5)

cat(sprintf("Saved: %s\n", png_path))
cat(sprintf("Saved: %s\n", pdf_path))
cat("Done.\n")
