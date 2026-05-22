###############################################################################
# FishMIP Output Plotting Script
# 
# Creates time series plots of FishMIP outputs (biomass and catch density)
# with ensemble uncertainty bands.
#
# Author: Generated for Prydz Bay mizer project
# Date: 2025
###############################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)

###############################################################################
# Configuration
###############################################################################

OUTPUT_DIR <- "fishmip_outputs"
PLOT_DIR <- "fishmip_outputs/plots"

# Color palette for size classes (from small to large)
SIZE_CLASS_COLORS <- c(
  "1g-10g" = "#440154",
  "10g-100g" = "#3b528b",
  "100g-1kg" = "#21908c",
  "1kg-10kg" = "#5dc863",
  "10kg-100kg" = "#addc30",
  ">100kg" = "#fde725"
)

SIZE_CLASS_ORDER <- c("1g-10g", "10g-100g", "100g-1kg", "1kg-10kg", "10kg-100kg", ">100kg")

# Reference lines
FISHING_START_YEAR <- 1961

###############################################################################
# Helper Functions
###############################################################################

# Smart log10 axis labels
smart_log10_labels <- function(x) {
  ifelse(x == 0, "0",
         ifelse(x >= 1, format(x, scientific = FALSE, big.mark = ","),
                format(x, scientific = TRUE, digits = 1)))
}

###############################################################################
# Create output directory
###############################################################################

if (!dir.exists(PLOT_DIR)) {
  dir.create(PLOT_DIR, recursive = TRUE)
}

###############################################################################
# Load Data
###############################################################################

cat("Loading FishMIP output data...\n")

tcblog10 <- read.csv(file.path(OUTPUT_DIR, "tcblog10_ensemble_stats.csv"))
tcb <- read.csv(file.path(OUTPUT_DIR, "tcb_ensemble_stats.csv"))
tclog10 <- read.csv(file.path(OUTPUT_DIR, "tclog10_ensemble_stats.csv"))
tc <- read.csv(file.path(OUTPUT_DIR, "tc_ensemble_stats.csv"))

# Ensure size class ordering
tcblog10$size_class <- factor(tcblog10$size_class, levels = SIZE_CLASS_ORDER)
tclog10$size_class <- factor(tclog10$size_class, levels = SIZE_CLASS_ORDER)

cat(sprintf("  Time range: %d - %d\n", min(tcb$year), max(tcb$year)))

###############################################################################
# Plot 1: Total Consumer Biomass Density (tcb) Time Series
###############################################################################

cat("Creating tcb time series plot...\n")

p_tcb <- ggplot(tcb, aes(x = year)) +
  # 90% CI ribbon
  geom_ribbon(aes(ymin = q05, ymax = q95), fill = "steelblue", alpha = 0.2) +
  # 50% CI ribbon
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "steelblue", alpha = 0.4) +
  # Median line
  geom_line(aes(y = median), color = "steelblue", linewidth = 1) +
  # Reference lines
  geom_vline(xintercept = FISHING_START_YEAR, linetype = "dashed", color = "red", alpha = 0.7) +
  # Labels
  labs(
    title = "Total Consumer Biomass Density (tcb)",
    subtitle = sprintf("Ensemble: 2111 simulations | Vertical line: fishing starts (%d)", FISHING_START_YEAR),
    x = "Year",
    y = expression("Biomass Density (g m"^-2*")")
  ) +
  scale_x_continuous(breaks = seq(1850, 2010, by = 20)) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(PLOT_DIR, "tcb_timeseries.png"), p_tcb, width = 10, height = 6, dpi = 300)
cat(sprintf("  Saved: %s\n", file.path(PLOT_DIR, "tcb_timeseries.png")))

###############################################################################
# Plot 2: Biomass Density by Size Class (tcblog10) - Stacked Area
###############################################################################

cat("Creating tcblog10 stacked area plot...\n")

# Prepare data for stacked area
tcblog10_wide <- tcblog10 %>%
  select(year, size_class, median) %>%
  pivot_wider(names_from = size_class, values_from = median)

tcblog10_stack <- tcblog10 %>%
  select(year, size_class, median)

p_tcblog10_stack <- ggplot(tcblog10_stack, aes(x = year, y = median, fill = size_class)) +
  geom_area(position = "stack", alpha = 0.8) +
  geom_vline(xintercept = FISHING_START_YEAR, linetype = "dashed", color = "black", alpha = 0.7) +
  scale_fill_manual(values = SIZE_CLASS_COLORS, name = "Size Class") +
  labs(
    title = "Total Consumer Biomass Density by Size Class (tcblog10)",
    subtitle = "Stacked area showing median across ensemble",
    x = "Year",
    y = expression("Biomass Density (g m"^-2*")")
  ) +
  scale_x_continuous(breaks = seq(1850, 2010, by = 20)) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

ggsave(file.path(PLOT_DIR, "tcblog10_stacked_area.png"), p_tcblog10_stack, width = 12, height = 6, dpi = 300)
cat(sprintf("  Saved: %s\n", file.path(PLOT_DIR, "tcblog10_stacked_area.png")))

###############################################################################
# Plot 3: Biomass Density by Size Class - Faceted with Uncertainty
###############################################################################

cat("Creating tcblog10 faceted plot with uncertainty...\n")

p_tcblog10_facet <- ggplot(tcblog10, aes(x = year)) +
  geom_ribbon(aes(ymin = q05, ymax = q95, fill = size_class), alpha = 0.2) +
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = size_class), alpha = 0.4) +
  geom_line(aes(y = median, color = size_class), linewidth = 0.8) +
  geom_vline(xintercept = FISHING_START_YEAR, linetype = "dashed", color = "red", alpha = 0.5) +
  facet_wrap(~size_class, scales = "free_y", ncol = 2) +
  scale_color_manual(values = SIZE_CLASS_COLORS, guide = "none") +
  scale_fill_manual(values = SIZE_CLASS_COLORS, guide = "none") +
  labs(
    title = "Biomass Density by Size Class with Ensemble Uncertainty",
    subtitle = "Shaded regions: 50% (dark) and 90% (light) credible intervals",
    x = "Year",
    y = expression("Biomass Density (g m"^-2*")")
  ) +
  scale_x_continuous(breaks = seq(1850, 2010, by = 40)) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray90")
  )

ggsave(file.path(PLOT_DIR, "tcblog10_faceted_uncertainty.png"), p_tcblog10_facet, width = 10, height = 10, dpi = 300)
cat(sprintf("  Saved: %s\n", file.path(PLOT_DIR, "tcblog10_faceted_uncertainty.png")))

###############################################################################
# Plot 4: Total Catch Density (tc) Time Series
###############################################################################

cat("Creating tc time series plot...\n")

# Filter to fishing years only for better visualization
tc_fishing <- tc %>% filter(year >= 1920)

p_tc <- ggplot(tc_fishing, aes(x = year)) +
  geom_ribbon(aes(ymin = q05, ymax = q95), fill = "darkred", alpha = 0.2) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "darkred", alpha = 0.4) +
  geom_line(aes(y = median), color = "darkred", linewidth = 1) +
  geom_vline(xintercept = FISHING_START_YEAR, linetype = "dashed", color = "black", alpha = 0.7) +
  labs(
    title = "Total Catch Density (tc)",
    subtitle = sprintf("Ensemble: 2111 simulations | Vertical line: reference period starts (%d)", FISHING_START_YEAR),
    x = "Year",
    y = expression("Catch Density (g m"^-2*" yr"^-1*")")
  ) +
  scale_x_continuous(breaks = seq(1920, 2010, by = 10)) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(PLOT_DIR, "tc_timeseries.png"), p_tc, width = 10, height = 6, dpi = 300)
cat(sprintf("  Saved: %s\n", file.path(PLOT_DIR, "tc_timeseries.png")))

###############################################################################
# Plot 5: Catch Density by Size Class (tclog10) - Stacked Area
###############################################################################

cat("Creating tclog10 stacked area plot...\n")

tclog10_fishing <- tclog10 %>% filter(year >= 1920)

p_tclog10_stack <- ggplot(tclog10_fishing, aes(x = year, y = median, fill = size_class)) +
  geom_area(position = "stack", alpha = 0.8) +
  geom_vline(xintercept = FISHING_START_YEAR, linetype = "dashed", color = "black", alpha = 0.7) +
  scale_fill_manual(values = SIZE_CLASS_COLORS, name = "Size Class") +
  labs(
    title = "Total Catch Density by Size Class (tclog10)",
    subtitle = "Stacked area showing median across ensemble",
    x = "Year",
    y = expression("Catch Density (g m"^-2*" yr"^-1*")")
  ) +
  scale_x_continuous(breaks = seq(1920, 2010, by = 10)) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

ggsave(file.path(PLOT_DIR, "tclog10_stacked_area.png"), p_tclog10_stack, width = 12, height = 6, dpi = 300)
cat(sprintf("  Saved: %s\n", file.path(PLOT_DIR, "tclog10_stacked_area.png")))

###############################################################################
# Plot 6: Catch Density by Size Class - Faceted with Uncertainty
###############################################################################

cat("Creating tclog10 faceted plot with uncertainty...\n")

tclog10_fishing <- tclog10 %>% filter(year >= 1920)

p_tclog10_facet <- ggplot(tclog10_fishing, aes(x = year)) +
  geom_ribbon(aes(ymin = q05, ymax = q95, fill = size_class), alpha = 0.2) +
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = size_class), alpha = 0.4) +
  geom_line(aes(y = median, color = size_class), linewidth = 0.8) +
  geom_vline(xintercept = FISHING_START_YEAR, linetype = "dashed", color = "red", alpha = 0.5) +
  facet_wrap(~size_class, scales = "free_y", ncol = 2) +
  scale_color_manual(values = SIZE_CLASS_COLORS, guide = "none") +
  scale_fill_manual(values = SIZE_CLASS_COLORS, guide = "none") +
  labs(
    title = "Catch Density by Size Class with Ensemble Uncertainty",
    subtitle = "Shaded regions: 50% (dark) and 90% (light) credible intervals",
    x = "Year",
    y = expression("Catch Density (g m"^-2*" yr"^-1*")")
  ) +
  scale_x_continuous(breaks = seq(1920, 2010, by = 20)) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray90")
  )

ggsave(file.path(PLOT_DIR, "tclog10_faceted_uncertainty.png"), p_tclog10_facet, width = 10, height = 10, dpi = 300)
cat(sprintf("  Saved: %s\n", file.path(PLOT_DIR, "tclog10_faceted_uncertainty.png")))

###############################################################################
# Plot 7: Combined Biomass and Catch Panel
###############################################################################

cat("Creating combined biomass and catch panel...\n")

# Peak fishing effort years
PEAK_BALEEN_YEAR <- 1933
PEAK_SPERM_YEAR <- 1948
PEAK_KRILL_YEAR <- 1979

# Model domain area for unit conversion
MODEL_DOMAIN_AREA <- 1.474341e+12  # m^2 (therMizer calibration domain, 05_therMizer_calibration_scale_model_domain.Rmd)
GRAMS_PER_TONNE <- 1e6

# Load observed catch data
yield_fg <- read.csv("monte_carlo_2111_summaries/yield_timeseries_summary_per_year_tonnes.csv")
obs_catch <- yield_fg %>%
  group_by(Year) %>%
  summarise(obs_total_t = sum(ObsYield_t, na.rm = TRUE), .groups = "drop") %>%
  mutate(obs_gm2 = obs_total_t * GRAMS_PER_TONNE / MODEL_DOMAIN_AREA) %>%
  filter(Year >= 1900)

# Filter to start from 1900 for cleaner visualization
tcb_filtered <- tcb %>% filter(year >= 1900)
tc_filtered <- tc %>% filter(year >= 1900)

# Create biomass panel (linear scale)
p_biomass <- ggplot(tcb_filtered, aes(x = year)) +
  geom_ribbon(aes(ymin = q05, ymax = q95), fill = "steelblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "steelblue", alpha = 0.4) +
  geom_line(aes(y = median), color = "steelblue", linewidth = 1) +
  geom_vline(xintercept = PEAK_BALEEN_YEAR, linetype = "dashed", color = "grey40", alpha = 0.8) +
  geom_vline(xintercept = PEAK_SPERM_YEAR, linetype = "dashed", color = "grey40", alpha = 0.8) +
  geom_vline(xintercept = PEAK_KRILL_YEAR, linetype = "dashed", color = "grey40", alpha = 0.8) +
  annotate("text", x = PEAK_BALEEN_YEAR, y = Inf, label = "Peak\nBaleen Whale", 
           vjust = 1.5, hjust = 0.5, size = 3.5, color = "grey30") +
  annotate("text", x = PEAK_SPERM_YEAR, y = Inf, label = "Peak\nSperm Whale", 
           vjust = 1.5, hjust = 0.5, size = 3.5, color = "grey30") +
  annotate("text", x = PEAK_KRILL_YEAR, y = Inf, label = "Peak\nKrill", 
           vjust = 1.5, hjust = 0.5, size = 3.5, color = "grey30") +
  labs(
    subtitle = "Biomass",
    y = expression("Density (g m"^-2*")")
  ) +
  scale_x_continuous(breaks = seq(1900, 2010, by = 20)) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13),
    axis.text = element_text(size = 12),
    plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 13)
  )

# Create catch panel (log scale)
# Add small offset to handle zeros in log scale
tc_filtered_log <- tc_filtered %>%
  mutate(
    median_log = pmax(median, 1e-6),
    q05_log = pmax(q05, 1e-6),
    q25_log = pmax(q25, 1e-6),
    q75_log = pmax(q75, 1e-6),
    q95_log = pmax(q95, 1e-6)
  )

# Also offset observed catch for log scale
obs_catch_log <- obs_catch %>%
  mutate(obs_gm2_log = pmax(obs_gm2, 1e-6))

p_catch <- ggplot(tc_filtered_log, aes(x = year)) +
  geom_ribbon(aes(ymin = q05_log, ymax = q95_log), fill = "darkred", alpha = 0.2) +
  geom_ribbon(aes(ymin = q25_log, ymax = q75_log), fill = "darkred", alpha = 0.4) +
  geom_line(aes(y = median_log), color = "darkred", linewidth = 1) +
  # Add observed catch as black line with points
  geom_line(data = obs_catch_log, aes(x = Year, y = obs_gm2_log), 
            color = "black", linewidth = 0.4) +
  geom_point(data = obs_catch_log, aes(x = Year, y = obs_gm2_log), 
             color = "black", size = 1.5, alpha = 0.8) +
  geom_vline(xintercept = PEAK_BALEEN_YEAR, linetype = "dashed", color = "grey40", alpha = 0.8) +
  geom_vline(xintercept = PEAK_SPERM_YEAR, linetype = "dashed", color = "grey40", alpha = 0.8) +
  geom_vline(xintercept = PEAK_KRILL_YEAR, linetype = "dashed", color = "grey40", alpha = 0.8) +
  labs(
    subtitle = "Catch",
    x = "Year",
    y = expression("Density (g m"^-2*", log scale)")
  ) +
  scale_y_log10(
    labels = scales::label_scientific(),
    limits = c(1e-6, 1)
  ) +
  scale_x_continuous(breaks = seq(1900, 2010, by = 20)) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 13)
  )

# Combine panels
library(patchwork)
p_combined <- p_biomass / p_catch +
  plot_annotation(
    title = "FishMIP Outputs: Total Consumer Biomass and Catch Density",
    subtitle = "Prydz Bay Mizer Model Ensemble (2111 simulations)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "grey40")
    )
  )

ggsave(file.path(PLOT_DIR, "combined_biomass_catch.png"), p_combined, width = 10, height = 8, dpi = 300)
cat(sprintf("  Saved: %s\n", file.path(PLOT_DIR, "combined_biomass_catch.png")))

###############################################################################
# Plot 8: Size Class Contribution (Proportional)
###############################################################################

cat("Creating size class proportion plot...\n")

# Calculate proportions for biomass
tcblog10_prop <- tcblog10 %>%
  group_by(year) %>%
  mutate(total = sum(median),
         proportion = median / total) %>%
  ungroup()

p_proportion <- ggplot(tcblog10_prop, aes(x = year, y = proportion, fill = size_class)) +
  geom_area(position = "stack", alpha = 0.9) +
  geom_vline(xintercept = FISHING_START_YEAR, linetype = "dashed", color = "white", linewidth = 1) +
  scale_fill_manual(values = SIZE_CLASS_COLORS, name = "Size Class") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Proportional Biomass by Size Class Over Time",
    subtitle = "Relative contribution of each log10 size class to total consumer biomass",
    x = "Year",
    y = "Proportion of Total Biomass"
  ) +
  scale_x_continuous(breaks = seq(1850, 2010, by = 20)) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

ggsave(file.path(PLOT_DIR, "size_class_proportions.png"), p_proportion, width = 12, height = 6, dpi = 300)
cat(sprintf("  Saved: %s\n", file.path(PLOT_DIR, "size_class_proportions.png")))

###############################################################################
# Summary
###############################################################################

cat("\n=============================================================\n")
cat("FishMIP Output Plots Complete\n")
cat("=============================================================\n")
cat(sprintf("Plots saved to: %s\n", PLOT_DIR))
cat("\nGenerated plots:\n")
cat("  1. tcb_timeseries.png - Total consumer biomass density\n")
cat("  2. tcblog10_stacked_area.png - Biomass by size class (stacked)\n")
cat("  3. tcblog10_faceted_uncertainty.png - Biomass by size class (faceted)\n")
cat("  4. tc_timeseries.png - Total catch density\n")
cat("  5. tclog10_stacked_area.png - Catch by size class (stacked)\n")
cat("  6. tclog10_faceted_uncertainty.png - Catch by size class (faceted)\n")
cat("  7. combined_biomass_catch.png - Combined panel\n")
cat("  8. size_class_proportions.png - Proportional biomass by size class\n")
cat("=============================================================\n")
