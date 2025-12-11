# Create FishMIP-style faceted plots for CLIMATE-ONLY ensemble:
# 1. Biomass by size class (tcblog10)
# 2. Total Consumer Biomass (TCB) timeseries

library(ggplot2)
library(dplyr)
library(tidyr)

# Set up output directory
output_dir <- "fishmip_outputs_climate_only/plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Define consistent styling
theme_fishmip <- function() {
  theme_bw(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      strip.background = element_rect(fill = "grey95"),
      strip.text = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 13),
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "grey40")
    )
}

# Define size class order for proper faceting
size_class_order <- c("1g-10g", "10g-100g", "100g-1kg", "1kg-10kg", "10kg-100kg", ">100kg")

# Color palette (using green for climate-only to distinguish from fished)
ribbon_color <- "#31a354"
line_color <- "#006d2c"

# ==============================================================================
# PLOT 1: Biomass by Size Class (tcblog10) - CLIMATE-ONLY
# ==============================================================================
cat("Creating biomass by size class plot (climate-only)...\n")

tcblog10 <- read.csv("fishmip_outputs_climate_only/tcblog10_ensemble_stats.csv")

# Filter to match combined plot x-axis (1900-2010)
tcblog10 <- tcblog10 %>% filter(year >= 1900)

# Order size classes properly
tcblog10$size_class <- factor(tcblog10$size_class, levels = size_class_order)

p1 <- ggplot(tcblog10, aes(x = year)) +
  # 5-95% ribbon (lighter)
  geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.2, fill = ribbon_color) +
  # 25-75% ribbon (darker)
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.4, fill = ribbon_color) +
  # Median line
  geom_line(aes(y = median), color = line_color, linewidth = 0.8) +
  # Facet by size class
  facet_wrap(~ size_class, scales = "free_y", ncol = 2) +
  # Labels
  labs(
    title = "Total Consumer Biomass by Size Class (Climate-Only)",
    subtitle = "FishMIP tcblog10 output | Unfished scenario | Ensemble of 2111 Monte Carlo simulations",
    x = "Year",
    y = expression("Biomass (g m"^{-2}*")")
  ) +
  scale_x_continuous(breaks = seq(1900, 2010, 20), limits = c(1900, 2010)) +
  theme_fishmip()

ggsave(file.path(output_dir, "tcblog10_biomass_by_size_class_climate_only.png"), p1,
       width = 10, height = 12, dpi = 300)
cat("Saved: tcblog10_biomass_by_size_class_climate_only.png\n")

# ==============================================================================
# PLOT 2: Total Consumer Biomass (TCB) Timeseries - CLIMATE-ONLY
# ==============================================================================
cat("Creating total consumer biomass timeseries (climate-only)...\n")

tcb <- read.csv("fishmip_outputs_climate_only/tcb_ensemble_stats.csv")

p2 <- ggplot(tcb, aes(x = year)) +
  # 5-95% ribbon (lighter)
  geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.2, fill = ribbon_color) +
  # 25-75% ribbon (darker)
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.4, fill = ribbon_color) +
  # Median line
  geom_line(aes(y = median), color = line_color, linewidth = 1) +
  # Labels
  labs(
    title = "Total Consumer Biomass (TCB) - Climate-Only",
    subtitle = "Unfished scenario | Sum of all 19 functional groups | Ensemble of 2111 Monte Carlo simulations",
    x = "Year",
    y = expression("Biomass (g m"^{-2}*")")
  ) +
  scale_x_continuous(breaks = seq(1840, 2010, 20)) +
  theme_fishmip() +
  theme(panel.grid.major.x = element_line(color = "grey85"))

ggsave(file.path(output_dir, "tcb_total_consumer_biomass_climate_only.png"), p2,
       width = 10, height = 6, dpi = 300)
cat("Saved: tcb_total_consumer_biomass_climate_only.png\n")

# ==============================================================================
# PLOT 3: Comparison Plot - TCB Fished vs Climate-Only
# ==============================================================================
cat("Creating comparison plot (fished vs climate-only)...\n")

# Load fished data
tcb_fished <- read.csv("fishmip_outputs/tcb_ensemble_stats.csv")
tcb_fished$scenario <- "Fished"

# Climate-only already loaded
tcb_climate <- tcb
tcb_climate$scenario <- "Climate-Only (Unfished)"

# Combine
tcb_combined <- bind_rows(tcb_fished, tcb_climate)
tcb_combined$scenario <- factor(tcb_combined$scenario, 
                                 levels = c("Climate-Only (Unfished)", "Fished"))

p3 <- ggplot(tcb_combined, aes(x = year, color = scenario, fill = scenario)) +
  # 5-95% ribbon (lighter)
  geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.15, color = NA) +
  # 25-75% ribbon (darker)
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.3, color = NA) +
  # Median line
  geom_line(aes(y = median), linewidth = 1) +
  # Manual colors
  scale_color_manual(values = c("Climate-Only (Unfished)" = "#006d2c", "Fished" = "#08306b")) +
  scale_fill_manual(values = c("Climate-Only (Unfished)" = "#31a354", "Fished" = "#1f78b4")) +
  # Labels
  labs(
    title = "Total Consumer Biomass: Fished vs Climate-Only",
    subtitle = "Comparison of fishing impact | Ensemble of 2111 Monte Carlo simulations",
    x = "Year",
    y = expression("Biomass (g m"^{-2}*")"),
    color = "Scenario",
    fill = "Scenario"
  ) +
  scale_x_continuous(breaks = seq(1840, 2010, 20)) +
  theme_fishmip() +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "grey70"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_line(color = "grey85")
  )

ggsave(file.path(output_dir, "tcb_comparison_fished_vs_climate_only.png"), p3,
       width = 12, height = 6, dpi = 300)
cat("Saved: tcb_comparison_fished_vs_climate_only.png\n")

# ==============================================================================
# PLOT 4: Size Class Comparison - Climate-Only vs Fished (small multiples)
# ==============================================================================
cat("Creating size class comparison plot...\n")

# Load fished data
tcblog10_fished <- read.csv("fishmip_outputs/tcblog10_ensemble_stats.csv")
tcblog10_fished <- tcblog10_fished %>% filter(year >= 1900)
tcblog10_fished$scenario <- "Fished"

# Climate-only
tcblog10_climate <- tcblog10
tcblog10_climate$scenario <- "Climate-Only (Unfished)"

# Combine
tcblog10_combined <- bind_rows(tcblog10_fished, tcblog10_climate)
tcblog10_combined$size_class <- factor(tcblog10_combined$size_class, levels = size_class_order)
tcblog10_combined$scenario <- factor(tcblog10_combined$scenario, 
                                      levels = c("Climate-Only (Unfished)", "Fished"))

p4 <- ggplot(tcblog10_combined, aes(x = year, color = scenario, fill = scenario)) +
  # 5-95% ribbon (lighter)
  geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.15, color = NA) +
  # 25-75% ribbon (darker)
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.3, color = NA) +
  # Median line
  geom_line(aes(y = median), linewidth = 0.7) +
  # Facet by size class
  facet_wrap(~ size_class, scales = "free_y", ncol = 2) +
  # Manual colors
  scale_color_manual(values = c("Climate-Only (Unfished)" = "#006d2c", "Fished" = "#08306b")) +
  scale_fill_manual(values = c("Climate-Only (Unfished)" = "#31a354", "Fished" = "#1f78b4")) +
  # Labels
  labs(
    title = "Biomass by Size Class: Fished vs Climate-Only",
    subtitle = "Comparison of fishing impact across size classes | Ensemble of 2111 Monte Carlo simulations",
    x = "Year",
    y = expression("Biomass (g m"^{-2}*")"),
    color = "Scenario",
    fill = "Scenario"
  ) +
  scale_x_continuous(breaks = seq(1900, 2010, 40), limits = c(1900, 2010)) +
  theme_fishmip() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  )

ggsave(file.path(output_dir, "tcblog10_comparison_fished_vs_climate_only.png"), p4,
       width = 11, height = 13, dpi = 300)
cat("Saved: tcblog10_comparison_fished_vs_climate_only.png\n")

cat("\nAll FishMIP climate-only plots created successfully!\n")
cat("Output directory:", normalizePath(output_dir), "\n")
