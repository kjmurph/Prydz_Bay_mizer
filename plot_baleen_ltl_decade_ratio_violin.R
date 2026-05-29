###############################################################################
# Baleen whale LTL consumption: exploited / unexploited ratio by decade
#
# For each decade 1901-1910 through 2001-2010, computes the per-simulation
# ratio of baleen LTL consumption (fishing / climate-only), then plots the
# distribution across 2111 simulations as violin plots.
#
# Outputs:
#   whale_consumption_outputs/baleen_ltl_decade_ratio_violin.png
#   whale_consumption_outputs/baleen_ltl_decade_ratio_violin.pdf
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

OUTPUT_DIR <- "whale_consumption_outputs"

# ---------------------------------------------------------------------------
# Define decades
# ---------------------------------------------------------------------------
decade_starts <- seq(1901, 2001, by = 10)
decade_labels <- paste0(decade_starts, "\u2013", substr(decade_starts + 9, 3, 4))
# e.g. "1901-10", "1911-20", ..., "2001-10"

# ---------------------------------------------------------------------------
# Load per-simulation data
# ---------------------------------------------------------------------------
cat("Loading fishing ensemble baleen LTL...\n")
fish_sims  <- readRDS(file.path(OUTPUT_DIR, "fishing_baleen_ltl_all_sims.rds"))
cat("Loading climate-only ensemble baleen LTL...\n")
clim_sims  <- readRDS(file.path(OUTPUT_DIR, "climate_only_baleen_ltl_all_sims.rds"))

n_sims <- length(fish_sims)
cat(sprintf("  %d simulation pairs\n\n", n_sims))

# ---------------------------------------------------------------------------
# Compute per-decade, per-sim ratios
# ---------------------------------------------------------------------------
cat("Computing decade ratios...\n")

ratio_rows <- vector("list", n_sims * length(decade_starts))
idx <- 1L

for (i in seq_len(n_sims)) {
  f <- fish_sims[[i]]
  c <- clim_sims[[i]]

  for (d in seq_along(decade_starts)) {
    yr_start <- decade_starts[d]
    yr_end   <- yr_start + 9L

    f_mean <- mean(f$total_consumption[f$year >= yr_start & f$year <= yr_end],
                   na.rm = TRUE)
    c_mean <- mean(c$total_consumption[c$year >= yr_start & c$year <= yr_end],
                   na.rm = TRUE)

    # Ratio = NA if unexploited is zero or missing
    ratio <- if (!is.na(c_mean) && c_mean > 0) f_mean / c_mean else NA_real_

    ratio_rows[[idx]] <- data.frame(
      sim_id  = i,
      decade  = decade_labels[d],
      ratio   = ratio,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }
}

ratio_df <- do.call(rbind, ratio_rows)
ratio_df$decade <- factor(ratio_df$decade, levels = decade_labels)

cat(sprintf("  Ratio rows: %d\n", nrow(ratio_df)))
cat(sprintf("  Ratio range: %.3f - %.3f\n\n",
            min(ratio_df$ratio, na.rm = TRUE),
            max(ratio_df$ratio, na.rm = TRUE)))

# Per-decade median for annotation
decade_medians <- ratio_df %>%
  group_by(decade) %>%
  summarise(med = median(ratio, na.rm = TRUE), .groups = "drop")

# ---------------------------------------------------------------------------
# Plot
# ---------------------------------------------------------------------------
cat("Generating violin plot...\n")

FILL_COL   <- "#4daf8d"
BORDER_COL <- "#2d7a5e"

p <- ggplot(ratio_df, aes(x = decade, y = ratio)) +
  # Reference line at ratio = 1 (= unexploited)
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.7) +
  annotate("text", x = Inf, y = 1, label = "= Unexploited",
           hjust = 1.05, vjust = -0.5, size = 3, colour = "grey50") +
  # Violin
  geom_violin(fill = FILL_COL, colour = BORDER_COL, alpha = 0.75,
              linewidth = 0.4, trim = TRUE, scale = "width") +
  # Narrow boxplot overlay (IQR + median)
  geom_boxplot(width = 0.12, outlier.shape = NA,
               fill = "white", colour = BORDER_COL, linewidth = 0.5) +
  # Decade median label
  geom_text(data = decade_medians,
            aes(x = decade, y = med, label = sprintf("%.2f", med)),
            vjust = -0.6, size = 2.8, fontface = "bold", colour = BORDER_COL) +
  scale_y_continuous(
    name   = "Exploited / Unexploited LTL consumption (ratio)",
    breaks = seq(0, ceiling(max(ratio_df$ratio, na.rm = TRUE)), by = 0.25),
    labels = function(x) sprintf("%.2f", x)
  ) +
  scale_x_discrete(name = "Decade") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(angle = 40, hjust = 1),
    plot.margin        = margin(8, 12, 4, 4, "pt")
  )

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------
png_path <- file.path(OUTPUT_DIR, "baleen_ltl_decade_ratio_violin.png")
pdf_path <- file.path(OUTPUT_DIR, "baleen_ltl_decade_ratio_violin.pdf")

ggsave(png_path, p, width = 10, height = 5.5, dpi = 300)
ggsave(pdf_path, p, width = 10, height = 5.5)

cat(sprintf("Saved: %s\n", png_path))
cat(sprintf("Saved: %s\n", pdf_path))
cat("Done.\n")
