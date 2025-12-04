# Create FishMIP-style faceted plots for:
# 1. Biomass by size class (tcblog10)
# 2. Total Consumer Biomass (TCB) timeseries
# 3. Catch by size class (tclog10)

library(ggplot2)
library(dplyr)
library(tidyr)

# Set up output directory
output_dir <- "fishmip_outputs/plots"
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

# Color palette
ribbon_color <- "#1f78b4"
line_color <- "#08306b"

# ==============================================================================
# PLOT 1: Biomass by Size Class (tcblog10)
# ==============================================================================
cat("Creating biomass by size class plot...\n")

tcblog10 <- read.csv("fishmip_outputs/tcblog10_ensemble_stats.csv")

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
    title = "Total Consumer Biomass by Size Class",
    subtitle = "FishMIP tcblog10 output | Ensemble of 2111 Monte Carlo simulations",
    x = "Year",
    y = expression("Biomass (g m"^{-2}*")")
  ) +
  scale_x_continuous(breaks = seq(1900, 2010, 20), limits = c(1900, 2010)) +
  theme_fishmip()

ggsave(file.path(output_dir, "tcblog10_biomass_by_size_class.png"), p1,
       width = 10, height = 12, dpi = 300)
cat("Saved: tcblog10_biomass_by_size_class.png\n")

# ==============================================================================
# PLOT 2: Total Consumer Biomass (TCB) Timeseries
# ==============================================================================
cat("Creating total consumer biomass timeseries...\n")

tcb <- read.csv("fishmip_outputs/tcb_ensemble_stats.csv")

p2 <- ggplot(tcb, aes(x = year)) +
  # 5-95% ribbon (lighter)
  geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.2, fill = ribbon_color) +
  # 25-75% ribbon (darker)
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.4, fill = ribbon_color) +
  # Median line
  geom_line(aes(y = median), color = line_color, linewidth = 1) +
  # Labels
  labs(
    title = "Total Consumer Biomass (TCB)",
    subtitle = "Sum of all 19 functional groups | Ensemble of 2111 Monte Carlo simulations",
    x = "Year",
    y = expression("Biomass (g m"^{-2}*")")
  ) +
  scale_x_continuous(breaks = seq(1840, 2010, 20)) +
  theme_fishmip() +
  theme(panel.grid.major.x = element_line(color = "grey85"))

ggsave(file.path(output_dir, "tcb_total_consumer_biomass.png"), p2,
       width = 10, height = 6, dpi = 300)
cat("Saved: tcb_total_consumer_biomass.png\n")

# ==============================================================================
# PLOT 3: Catch by Size Class (tclog10)
# ==============================================================================
cat("Creating catch by size class plot...\n")

tclog10 <- read.csv("fishmip_outputs/tclog10_ensemble_stats.csv")

# Order size classes properly
tclog10$size_class <- factor(tclog10$size_class, levels = size_class_order)

p3 <- ggplot(tclog10, aes(x = year)) +
  # 5-95% ribbon (lighter)
  geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.2, fill = "#e31a1c") +
  # 25-75% ribbon (darker)
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.4, fill = "#e31a1c") +
  # Median line
  geom_line(aes(y = median), color = "#67000d", linewidth = 0.8) +
  # Facet by size class
  facet_wrap(~ size_class, scales = "free_y", ncol = 2) +
  # Labels
  labs(
    title = "Total Catch by Size Class",
    subtitle = "FishMIP tclog10 output | Ensemble of 2111 Monte Carlo simulations",
    x = "Year",
    y = expression("Catch (g m"^{-2}*" year"^{-1}*")")
  ) +
  scale_x_continuous(breaks = seq(1860, 2000, 40)) +
  theme_fishmip()

ggsave(file.path(output_dir, "tclog10_catch_by_size_class.png"), p3,
       width = 10, height = 12, dpi = 300)
cat("Saved: tclog10_catch_by_size_class.png\n")

# ==============================================================================
# PLOT 4: Total Catch (TC) Timeseries (if available)
# ==============================================================================
if (file.exists("fishmip_outputs/tc_ensemble_stats.csv")) {
  cat("Creating total catch timeseries...\n")
  
  tc <- read.csv("fishmip_outputs/tc_ensemble_stats.csv")
  
  p4 <- ggplot(tc, aes(x = year)) +
    # 5-95% ribbon (lighter)
    geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.2, fill = "#e31a1c") +
    # 25-75% ribbon (darker)
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.4, fill = "#e31a1c") +
    # Median line
    geom_line(aes(y = median), color = "#67000d", linewidth = 1) +
    # Labels
    labs(
      title = "Total Catch (TC)",
      subtitle = "Sum of all fishing mortality | Ensemble of 2111 Monte Carlo simulations",
      x = "Year",
      y = expression("Catch (g m"^{-2}*" year"^{-1}*")")
    ) +
    scale_x_continuous(breaks = seq(1840, 2010, 20)) +
    theme_fishmip() +
    theme(panel.grid.major.x = element_line(color = "grey85"))
  
  ggsave(file.path(output_dir, "tc_total_catch.png"), p4,
         width = 10, height = 6, dpi = 300)
  cat("Saved: tc_total_catch.png\n")
}

cat("\nAll FishMIP plots created successfully!\n")
cat("Output directory:", normalizePath(output_dir), "\n")
