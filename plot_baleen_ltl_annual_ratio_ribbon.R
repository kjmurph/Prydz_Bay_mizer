###############################################################################
# Baleen whale LTL consumption: exploited / unexploited ratio — annual
# Median line + IQR ribbon, 1901–2010
#
# Outputs:
#   whale_consumption_outputs/baleen_ltl_annual_ratio_ribbon.png
#   whale_consumption_outputs/baleen_ltl_annual_ratio_ribbon.pdf
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

OUTPUT_DIR <- "whale_consumption_outputs"

# ---------------------------------------------------------------------------
# Load per-simulation data
# ---------------------------------------------------------------------------
cat("Loading fishing ensemble baleen LTL...\n")
fish_sims <- readRDS(file.path(OUTPUT_DIR, "fishing_baleen_ltl_all_sims.rds"))
cat("Loading climate-only ensemble baleen LTL...\n")
clim_sims <- readRDS(file.path(OUTPUT_DIR, "climate_only_baleen_ltl_all_sims.rds"))

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

# Restrict to 1901–2010
ratio_df <- ratio_df[ratio_df$year >= 1901, ]

annual_summary <- ratio_df %>%
  group_by(year) %>%
  summarise(
    q25 = quantile(ratio, 0.25, na.rm = TRUE),
    med = median(ratio,         na.rm = TRUE),
    q75 = quantile(ratio, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("  Years: %d – %d\n", min(annual_summary$year), max(annual_summary$year)))
cat(sprintf("  Median range: %.3f – %.3f\n\n",
            min(annual_summary$med), max(annual_summary$med)))

# ---------------------------------------------------------------------------
# Plot
# ---------------------------------------------------------------------------
cat("Generating ribbon plot...\n")

FILL_COL <- "#4daf8d"
LINE_COL <- "#1a4d38"

p <- ggplot(annual_summary, aes(x = year)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50",
             linewidth = 0.7) +
  annotate("text", x = 1901, y = 1, label = "Unexploited",
           hjust = 0, vjust = -0.5, size = 3.2, colour = "grey50") +
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
    name   = "Exploited / Unexploited LTL consumption (ratio)",
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
png_path <- file.path(OUTPUT_DIR, "baleen_ltl_annual_ratio_ribbon.png")
pdf_path <- file.path(OUTPUT_DIR, "baleen_ltl_annual_ratio_ribbon.pdf")

ggsave(png_path, p, width = 9, height = 5, dpi = 300)
ggsave(pdf_path, p, width = 9, height = 5)

cat(sprintf("Saved: %s\n", png_path))
cat(sprintf("Saved: %s\n", pdf_path))
cat("Done.\n")
