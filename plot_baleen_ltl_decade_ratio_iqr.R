###############################################################################
# Baleen whale LTL consumption: exploited / unexploited ratio by decade
# IQR-only version (Q25–Q75 box + median line, no whiskers)
#
# Outputs:
#   whale_consumption_outputs/baleen_ltl_decade_ratio_iqr.png
#   whale_consumption_outputs/baleen_ltl_decade_ratio_iqr.pdf
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

OUTPUT_DIR <- "whale_consumption_outputs"

# ---------------------------------------------------------------------------
# Define decades
# ---------------------------------------------------------------------------
decade_starts <- seq(1901, 2001, by = 10)
decade_labels <- paste0(decade_starts, "\u2013", substr(decade_starts + 9, 3, 4))

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

    ratio <- if (!is.na(c_mean) && c_mean > 0) f_mean / c_mean else NA_real_

    ratio_rows[[idx]] <- data.frame(
      sim_id = i,
      decade = decade_labels[d],
      ratio  = ratio,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }
}

ratio_df <- do.call(rbind, ratio_rows)
ratio_df$decade <- factor(ratio_df$decade, levels = decade_labels)

# ---------------------------------------------------------------------------
# Pre-compute IQR summary per decade
# ---------------------------------------------------------------------------
iqr_summary <- ratio_df %>%
  group_by(decade) %>%
  summarise(
    q25 = quantile(ratio, 0.25, na.rm = TRUE),
    med = median(ratio, na.rm = TRUE),
    q75 = quantile(ratio, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(x     = as.integer(decade),
         xmin  = x - 0.35,
         xmax  = x + 0.35)

cat("IQR summary:\n")
print(iqr_summary[, c("decade", "q25", "med", "q75")])
cat("\n")

# ---------------------------------------------------------------------------
# Plot — IQR box (geom_rect) + median line (geom_segment) + label
# ---------------------------------------------------------------------------
cat("Generating IQR plot...\n")

FILL_COL   <- "#4daf8d"
BORDER_COL <- "#2d7a5e"
MED_COL    <- "#1a4d38"

p <- ggplot() +
  # Reference line at ratio = 1
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50",
             linewidth = 0.7) +
  annotate("text", x = 0.4, y = 1, label = "= Unexploited",
           hjust = 0, vjust = -0.5, size = 3.2, colour = "grey50") +
  # IQR rectangle (Q25 – Q75)
  geom_rect(data = iqr_summary,
            aes(xmin = xmin, xmax = xmax, ymin = q25, ymax = q75),
            fill = FILL_COL, colour = BORDER_COL, alpha = 0.80,
            linewidth = 0.5) +
  # Median line
  geom_segment(data = iqr_summary,
               aes(x = xmin, xend = xmax, y = med, yend = med),
               colour = MED_COL, linewidth = 1.1) +
  # Median value label above box
  geom_text(data = iqr_summary,
            aes(x = x, y = q75, label = sprintf("%.2f", med)),
            vjust = -0.5, size = 2.9, fontface = "bold",
            colour = BORDER_COL) +
  scale_x_continuous(
    name   = "Decade",
    breaks = seq_along(decade_labels),
    labels = decade_labels
  ) +
  scale_y_continuous(
    name   = "Exploited / Unexploited LTL consumption (ratio)",
    breaks = seq(0, ceiling(max(iqr_summary$q75, na.rm = TRUE) + 0.1), by = 0.25),
    labels = function(x) sprintf("%.2f", x),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
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
png_path <- file.path(OUTPUT_DIR, "baleen_ltl_decade_ratio_iqr.png")
pdf_path <- file.path(OUTPUT_DIR, "baleen_ltl_decade_ratio_iqr.pdf")

ggsave(png_path, p, width = 10, height = 5.5, dpi = 300)
ggsave(pdf_path, p, width = 10, height = 5.5)

cat(sprintf("Saved: %s\n", png_path))
cat(sprintf("Saved: %s\n", pdf_path))
cat("Done.\n")
