###############################################################################
# Ecosystem Assessment Diagnostic Plots
# 
# Creates individual metric time series showing both ensembles with
# uncertainty bands for detailed comparison.
#
# Run after: ecosystem_assessment.R
###############################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Configuration
INPUT_DIR <- "ecosystem_assessment_outputs"
OUTPUT_DIR <- "ecosystem_assessment_outputs/diagnostics"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

###############################################################################
# Create Individual Metric Time Series Plots
###############################################################################

create_metric_timeseries <- function(fishing_stats, climate_stats, 
                                      metric_col, metric_name, y_label,
                                      output_path) {
  
  # Extract columns
  median_col <- paste0(metric_col, "_median")
  q25_col <- paste0(metric_col, "_q25")
  q75_col <- paste0(metric_col, "_q75")
  q05_col <- paste0(metric_col, "_q05")
  q95_col <- paste0(metric_col, "_q95")
  
  # Check columns exist
  if (!(median_col %in% names(fishing_stats))) {
    cat(sprintf("  Warning: %s not found in stats\n", metric_col))
    return(NULL)
  }
  
  # Prepare data
  fishing_data <- fishing_stats %>%
    select(decade, start_year, 
           median = !!sym(median_col),
           q25 = !!sym(q25_col), q75 = !!sym(q75_col),
           q05 = !!sym(q05_col), q95 = !!sym(q95_col)) %>%
    mutate(scenario = "Fishing/Whaling")
  
  climate_data <- climate_stats %>%
    select(decade, start_year, 
           median = !!sym(median_col),
           q25 = !!sym(q25_col), q75 = !!sym(q75_col),
           q05 = !!sym(q05_col), q95 = !!sym(q95_col)) %>%
    mutate(scenario = "Climate-only")
  
  plot_data <- bind_rows(fishing_data, climate_data)
  
  # Create plot
  p <- ggplot(plot_data, aes(x = start_year, color = scenario, fill = scenario)) +
    
    # 90% CI ribbon
    geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.15, color = NA) +
    
    # 50% CI ribbon
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.25, color = NA) +
    
    # Median line
    geom_line(aes(y = median), linewidth = 1) +
    geom_point(aes(y = median), size = 2) +
    
    # Vertical lines for key periods
    geom_vline(xintercept = 1904, linetype = "dashed", color = "grey50", 
               alpha = 0.7) +  # Start of whaling
    geom_vline(xintercept = 1930, linetype = "dashed", color = "grey50", 
               alpha = 0.7) +  # Peak whaling begins
    geom_vline(xintercept = 1965, linetype = "dashed", color = "grey50", 
               alpha = 0.7) +  # IWC regulations
    geom_vline(xintercept = 1977, linetype = "dashed", color = "grey50", 
               alpha = 0.7) +  # CCAMLR krill fishing
    
    # Color scale
    scale_color_manual(values = c("Fishing/Whaling" = "#d62728", 
                                   "Climate-only" = "#2ca02c")) +
    scale_fill_manual(values = c("Fishing/Whaling" = "#d62728", 
                                  "Climate-only" = "#2ca02c")) +
    
    labs(
      title = metric_name,
      subtitle = "Fishing vs. Climate-only scenarios (median with 50% and 90% CI)",
      x = "Year",
      y = y_label,
      color = "Scenario",
      fill = "Scenario"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(size = 11),
      legend.position = "bottom"
    )
  
  # Save
  ggsave(file.path(output_path, paste0(metric_col, "_timeseries.png")),
         p, width = 10, height = 6, dpi = 300)
  ggsave(file.path(output_path, paste0(metric_col, "_timeseries.pdf")),
         p, width = 10, height = 6)
  
  return(p)
}


###############################################################################
# Create All Diagnostic Plots
###############################################################################

create_all_diagnostics <- function() {
  
  cat("=============================================================\n")
  cat("CREATING DIAGNOSTIC PLOTS\n")
  cat("=============================================================\n\n")
  
  # Load statistics files
  fishing_stats <- read.csv(file.path(INPUT_DIR, "fishing_metrics_stats.csv"))
  climate_stats <- read.csv(file.path(INPUT_DIR, "climate_only_metrics_stats.csv"))
  
  # Define metrics to plot
  metrics_to_plot <- list(
    list(col = "total_biomass", name = "Total Community Biomass", 
         label = "Biomass (g/m²)"),
    list(col = "baleen_biomass", name = "Baleen Whale Biomass", 
         label = "Biomass (g/m²)"),
    list(col = "whale_biomass", name = "All Whale Biomass", 
         label = "Biomass (g/m²)"),
    list(col = "seal_biomass", name = "Seal Biomass", 
         label = "Biomass (g/m²)"),
    list(col = "fish_biomass", name = "Fish Biomass", 
         label = "Biomass (g/m²)"),
    list(col = "krill_biomass", name = "Antarctic Krill Biomass", 
         label = "Biomass (g/m²)"),
    list(col = "ltl_biomass", name = "Lower Trophic Level Biomass", 
         label = "Biomass (g/m²)"),
    list(col = "apex_biomass", name = "Apex Predator Biomass", 
         label = "Biomass (g/m²)"),
    list(col = "spectrum_slope", name = "Community Size Spectrum Slope", 
         label = "Slope"),
    list(col = "mean_tl", name = "Mean Trophic Level (Biomass-weighted)", 
         label = "Trophic Level"),
    list(col = "htl_indicator", name = "High Trophic Level Indicator", 
         label = "Proportion"),
    list(col = "large_fish_indicator", name = "Large Fish Indicator", 
         label = "Proportion"),
    list(col = "mean_weight", name = "Mean Individual Weight", 
         label = "Weight (g)"),
    list(col = "predator_prey_ratio", name = "Predator-Prey Biomass Ratio", 
         label = "Ratio")
  )
  
  # Generate plots
  plots <- list()
  for (metric in metrics_to_plot) {
    cat(sprintf("Creating plot for: %s\n", metric$name))
    p <- create_metric_timeseries(
      fishing_stats, climate_stats,
      metric$col, metric$name, metric$label,
      OUTPUT_DIR
    )
    if (!is.null(p)) {
      plots[[metric$col]] <- p
    }
  }
  
  # Create combined panel for key biomass metrics
  cat("\nCreating combined biomass panel...\n")
  
  key_biomass <- list(
    list(col = "baleen_biomass", name = "Baleen Whales"),
    list(col = "whale_biomass", name = "All Whales"),
    list(col = "krill_biomass", name = "Antarctic Krill"),
    list(col = "apex_biomass", name = "Apex Predators")
  )
  
  panel_plots <- list()
  
  for (metric in key_biomass) {
    median_col <- paste0(metric$col, "_median")
    q25_col <- paste0(metric$col, "_q25")
    q75_col <- paste0(metric$col, "_q75")
    
    fishing_data <- fishing_stats %>%
      select(start_year, 
             median = !!sym(median_col),
             q25 = !!sym(q25_col), q75 = !!sym(q75_col)) %>%
      mutate(scenario = "Fishing")
    
    climate_data <- climate_stats %>%
      select(start_year, 
             median = !!sym(median_col),
             q25 = !!sym(q25_col), q75 = !!sym(q75_col)) %>%
      mutate(scenario = "Climate-only")
    
    plot_data <- bind_rows(fishing_data, climate_data)
    
    p <- ggplot(plot_data, aes(x = start_year, color = scenario, fill = scenario)) +
      geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.25, color = NA) +
      geom_line(aes(y = median), linewidth = 1) +
      scale_color_manual(values = c("Fishing" = "#d62728", "Climate-only" = "#2ca02c")) +
      scale_fill_manual(values = c("Fishing" = "#d62728", "Climate-only" = "#2ca02c")) +
      labs(title = metric$name, x = "Year", y = "Biomass (g/m²)") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        legend.position = "none",
        axis.title.x = element_blank()
      )
    
    panel_plots[[metric$col]] <- p
  }
  
  # Combine with patchwork
  combined <- (panel_plots[[1]] | panel_plots[[2]]) / 
              (panel_plots[[3]] | panel_plots[[4]]) +
    plot_annotation(
      title = "Key Ecosystem Biomass Indicators",
      subtitle = "Fishing/Whaling (red) vs Climate-only (green) scenarios",
      caption = "Shaded areas show 50% credible intervals"
    ) &
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  ggsave(file.path(OUTPUT_DIR, "key_biomass_panel.png"), 
         combined, width = 12, height = 8, dpi = 300)
  ggsave(file.path(OUTPUT_DIR, "key_biomass_panel.pdf"), 
         combined, width = 12, height = 8)
  
  cat("\n=============================================================\n")
  cat("DIAGNOSTIC PLOTS COMPLETE\n")
  cat("=============================================================\n")
  cat(sprintf("Output directory: %s\n", OUTPUT_DIR))
  
  return(plots)
}


###############################################################################
# Create Period Comparison Barplot
###############################################################################

create_period_comparison <- function() {
  
  cat("Creating period comparison barplots...\n")
  
  # Load heatmap data
  heatmap_data <- read.csv(file.path(INPUT_DIR, "heatmap_scores.csv"))
  
  # Define key periods
  key_periods <- c("1841-1850", "1931-1940", "1961-1970", "2001-2010")
  period_labels <- c("Pre-whaling", "Peak Whaling", "Post-moratorium", "Modern")
  
  period_data <- heatmap_data %>%
    filter(decade %in% key_periods) %>%
    mutate(period_label = factor(decade, levels = key_periods, 
                                  labels = period_labels))
  
  # Focus on key biomass metrics
  biomass_metrics <- c("Baleen Whale Biomass", "All Whale Biomass", 
                       "Krill Biomass", "Apex Predator Biomass",
                       "Total Biomass", "Fish Biomass")
  
  plot_data <- period_data %>%
    filter(metric_name %in% biomass_metrics) %>%
    mutate(metric_name = factor(metric_name, levels = biomass_metrics))
  
  p <- ggplot(plot_data, aes(x = metric_name, y = depletion_ratio, 
                              fill = period_label)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
             width = 0.7) +
    geom_hline(yintercept = 1.0, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0.75, linetype = "dotted", color = "orange") +
    geom_hline(yintercept = 0.54, linetype = "dotted", color = "red") +
    
    scale_fill_viridis_d(option = "viridis", direction = -1) +
    
    labs(
      title = "Ecosystem Depletion Across Key Historical Periods",
      subtitle = "Ratio of Fishing scenario to Climate-only baseline",
      x = "Metric",
      y = "Depletion Ratio",
      fill = "Period"
    ) +
    
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "right"
    ) +
    
    annotate("text", x = 0.5, y = 1.05, label = "No difference", 
             hjust = 0, size = 3, color = "black") +
    annotate("text", x = 0.5, y = 0.78, label = "Krill pred. target", 
             hjust = 0, size = 3, color = "orange") +
    annotate("text", x = 0.5, y = 0.57, label = "IWC lower target", 
             hjust = 0, size = 3, color = "red")
  
  ggsave(file.path(OUTPUT_DIR, "period_comparison_barplot.png"),
         p, width = 12, height = 7, dpi = 300)
  ggsave(file.path(OUTPUT_DIR, "period_comparison_barplot.pdf"),
         p, width = 12, height = 7)
  
  return(p)
}


###############################################################################
# Run Diagnostics
###############################################################################

# Check if assessment has been run
if (!file.exists(file.path(INPUT_DIR, "fishing_metrics_stats.csv"))) {
  cat("Error: Run ecosystem_assessment.R first to generate statistics.\n")
} else {
  plots <- create_all_diagnostics()
  create_period_comparison()
}
