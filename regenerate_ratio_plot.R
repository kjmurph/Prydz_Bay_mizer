# Regenerate the fishing impact ratio plot with improved x-axis labels
# Removes text annotation and adds clear labels for 1t, 10t, 100t

library(ggplot2)
library(dplyr)
library(scales)

cat("Regenerating spectrum ratio plot...\n")

output_dir <- "climate_only_analysis"

# Load the ratio data
ratio_spec <- read.csv(file.path(output_dir, "fished_vs_climate_spectrum_ratio.csv"))

# Ensure size_category is a factor with correct order
ratio_spec$size_category <- factor(ratio_spec$size_category, 
                                    levels = c("Small (<1g)", "Medium (1g-1kg)", 
                                               "Large (1kg-1t)", "Very Large (>1t)"))

# Define custom x-axis breaks and labels
# Include: 1mg, 1g, 1kg, 1t, 10t, 100t, 1000t
x_breaks <- c(1e-3, 1e-2, 1e-1, 1, 10, 100, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9)
x_labels <- c("1 mg", "10 mg", "100 mg", "1 g", "10 g", "100 g", "1 kg", "10 kg", "100 kg", "1 t", "10 t", "100 t", "1000 t")

# Ratio plot with improved labels
p_ratio <- ggplot(ratio_spec, aes(x = w, y = ratio)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1, color = "darkgreen") +
  geom_point(aes(color = size_category), size = 2) +
  scale_x_log10(
    breaks = x_breaks,
    labels = x_labels
  ) +
  scale_color_manual(
    values = c("Small (<1g)" = "#1b9e77", "Medium (1g-1kg)" = "#d95f02", 
               "Large (1kg-1t)" = "#7570b3", "Very Large (>1t)" = "#e7298a")
  ) +
  theme_bw() +
  labs(
    title = "Fishing Impact on Community Size Spectrum",
    subtitle = "Ratio: Fished / Climate-Only (Reference Period 2001-2010)",
    x = "Body mass",
    y = "Biomass Ratio (Fished / Unfished)",
    color = "Size Category"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
  )

ggsave(file.path(output_dir, "spectrum_ratio_reference_period.png"), 
       p_ratio, width = 10, height = 6, dpi = 300)
cat("Saved: spectrum_ratio_reference_period.png\n")
