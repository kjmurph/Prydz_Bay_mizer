# Create combined panel plot:
# Left panel: Pre-Whaling (1920-1929) - single period
# Right panel: Krill fishing trajectory (3 periods overlaid)

library(ggplot2)
library(dplyr)
library(patchwork)

cat("=== Creating Combined Pre-Whaling + Krill Fishing Panel ===\n\n")

output_dir <- "fishmip_outputs/temporal_comparison"

# Load the temporal ratio data
ratio_file <- file.path(output_dir, "temporal_ratio_stats.csv")
if (!file.exists(ratio_file)) {
  stop("Temporal ratio stats not found. Run compare_size_spectrum_temporal.R first.")
}

all_ratio_stats <- read.csv(ratio_file)
cat("Loaded ratio stats:", nrow(all_ratio_stats), "rows\n")

reference_name <- "Reference (2001-2010)"

# ------------------------------------------------------------------------------
# Panel A: Pre-Whaling (1920-1929) - faceted single panel style
# ------------------------------------------------------------------------------
cat("Creating Pre-Whaling panel...\n")

panel_a_data <- all_ratio_stats %>%
  filter(period == "pre_whaling") %>%
  filter(w >= 10000) %>%  # 10 kg = 10000 g
  mutate(period_label = "Pre-Whaling (1920-1929)")

p_panel_a <- ggplot(panel_a_data, aes(x = w)) +
  geom_ribbon(aes(ymin = ratio_q25, ymax = ratio_q75), fill = "steelblue", alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_line(aes(y = ratio_median), color = "steelblue", linewidth = 1.2) +
  scale_x_log10(
    labels = function(x) {
      dplyr::case_when(
        x >= 1000000 ~ paste0(x/1000000, " t"),
        TRUE ~ paste0(x/1000, " kg")
      )
    },
    breaks = c(10000, 100000, 1000000, 10000000, 100000000),
    limits = c(10000, 1e8)
  ) +
  scale_y_log10(
    breaks = c(0.1, 0.3, 1, 3, 10, 30, 100),
    labels = c("0.1", "0.3", "1", "3", "10", "30", "100")
  ) +
  coord_cartesian(ylim = c(0.1, 100)) +
  labs(
    title = "Pre-Whaling (1920-1929)",
    x = "Body mass",
    y = "Abundance ratio"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

# ------------------------------------------------------------------------------
# Panel B: Krill Fishing Trajectory (3 periods overlaid)
# ------------------------------------------------------------------------------
cat("Creating Krill Fishing Trajectory panel...\n")

panel_b_data <- all_ratio_stats %>%
  filter(period %in% c("pre_krill", "peak_krill", "post_peak_krill")) %>%
  filter(w >= 10000) %>%  # 10 kg = 10000 g
  mutate(period_label = factor(period_label, levels = c(
    "Pre-Krill Fishing (1964-1973)",
    "Peak Krill Fishing (1974-1984)",
    "Post-Peak Krill Fishing (1985-1995)"
  )))

p_panel_b <- ggplot(panel_b_data, aes(x = w, color = period_label, fill = period_label)) +
  geom_ribbon(aes(ymin = ratio_q25, ymax = ratio_q75), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_line(aes(y = ratio_median), linewidth = 1.2) +
  scale_x_log10(
    labels = function(x) {
      dplyr::case_when(
        x >= 1000000 ~ paste0(x/1000000, " t"),
        TRUE ~ paste0(x/1000, " kg")
      )
    },
    breaks = c(10000, 100000, 1000000, 10000000, 100000000),
    limits = c(10000, 1e8)
  ) +
  scale_y_log10(
    breaks = c(0.3, 0.5, 1, 2, 3, 5, 10),
    labels = c("0.3", "0.5", "1", "2", "3", "5", "10")
  ) +
  scale_color_manual(values = c(
    "Pre-Krill Fishing (1964-1973)" = "#762A83",
    "Peak Krill Fishing (1974-1984)" = "#E66101",
    "Post-Peak Krill Fishing (1985-1995)" = "#1B7837"
  )) +
  scale_fill_manual(values = c(
    "Pre-Krill Fishing (1964-1973)" = "#762A83",
    "Peak Krill Fishing (1974-1984)" = "#E66101",
    "Post-Peak Krill Fishing (1985-1995)" = "#1B7837"
  )) +
  coord_cartesian(ylim = c(0.3, 10)) +
  labs(
    title = "Whale Recovery & Krill Fishing (1964-1995)",
    x = "Body mass",
    y = "Abundance ratio",
    color = "Period",
    fill = "Period"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.9), color = "grey80"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

# ------------------------------------------------------------------------------
# Combine panels using patchwork with Nature-style panel tags
# ------------------------------------------------------------------------------
cat("Combining panels...\n")

p_combined <- p_panel_a + p_panel_b +
  plot_annotation(
    tag_levels = 'A',
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold")
    )
  )

ggsave(file.path(output_dir, "size_spectrum_combined_prewhaling_krill.png"), 
       p_combined, width = 16, height = 6, dpi = 300)
cat("  Saved: size_spectrum_combined_prewhaling_krill.png\n")

# Also save to climate_only_analysis for consistency
ggsave("climate_only_analysis/size_spectrum_combined_prewhaling_krill.png", 
       p_combined, width = 16, height = 6, dpi = 300)
cat("  Saved: climate_only_analysis/size_spectrum_combined_prewhaling_krill.png\n")

cat("\n=== Combined Panel Complete ===\n")
