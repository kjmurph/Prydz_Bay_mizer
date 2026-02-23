###############################################################################
# Ecosystem Assessment Diagnostic Plots (v2)
# 
# Creates diagnostic visualizations for the paired ensemble assessment.
# Handles biomass metrics (ratio panels) and structural metrics (difference
# panels with B0 envelope bands) differently.
#
# Run after: ecosystem_assessment_v2.R
###############################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Configuration
INPUT_DIR <- "ecosystem_assessment_outputs"
OUTPUT_DIR <- "ecosystem_assessment_outputs/diagnostics"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

B0_PERIOD <- c(1841, 1860)
WHALE_METRICS <- c("baleen_biomass", "whale_biomass")

# Source category definitions if not already loaded
if (!exists("BIOMASS_METRICS")) {
  BIOMASS_METRICS <- c("total_biomass", "whale_biomass", "baleen_biomass",
                       "seal_biomass", "fish_biomass", "krill_biomass",
                       "ltl_biomass", "apex_biomass")
}
if (!exists("STRUCTURAL_METRICS")) {
  STRUCTURAL_METRICS <- c("spectrum_slope", "spectrum_intercept", "mean_tl",
                          "htl_indicator", "large_fish_indicator", "fish_lfi",
                          "shannon_diversity", "w_statistic",
                          "production_biomass_ratio",
                          "mean_weight", "mean_max_weight",
                          "predator_prey_ratio", "consumer_ltl_ratio")
}

###############################################################################
# Biomass Time Series (ratio panel)
###############################################################################

create_biomass_timeseries <- function(fishing_raw, climate_raw, b0_reference,
                                      metric_col, metric_name, y_label,
                                      output_path) {
  if (!(metric_col %in% names(fishing_raw))) {
    cat(sprintf("  Warning: %s not found\n", metric_col)); return(NULL)
  }
  summarise_scenario <- function(raw, scenario) {
    raw %>% group_by(decade, start_year) %>%
      summarise(median = median(.data[[metric_col]], na.rm = TRUE),
                q25 = quantile(.data[[metric_col]], 0.25, na.rm = TRUE),
                q75 = quantile(.data[[metric_col]], 0.75, na.rm = TRUE),
                q05 = quantile(.data[[metric_col]], 0.05, na.rm = TRUE),
                q95 = quantile(.data[[metric_col]], 0.95, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(scenario = scenario)
  }
  plot_data <- bind_rows(summarise_scenario(fishing_raw, "Fishing/Whaling"),
                         summarise_scenario(climate_raw, "Climate-only"))

  b0_med <- b0_q25 <- b0_q75 <- NA
  if (metric_col %in% names(b0_reference)) {
    b0_vals <- b0_reference[[metric_col]][!is.na(b0_reference[[metric_col]])]
    if (length(b0_vals) > 0) {
      b0_med <- median(b0_vals); b0_q25 <- quantile(b0_vals, 0.25)
      b0_q75 <- quantile(b0_vals, 0.75)
    }
  }

  # Panel A: absolute values
  p_abs <- ggplot(plot_data, aes(x = start_year, color = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.12, color = NA) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.25, color = NA) +
    geom_line(aes(y = median), linewidth = 1) +
    geom_point(aes(y = median), size = 1.8)
  if (!is.na(b0_med)) {
    p_abs <- p_abs +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = b0_q25, ymax = b0_q75,
               fill = "grey50", alpha = 0.15) +
      geom_hline(yintercept = b0_med, linetype = "dashed", color = "grey40", linewidth = 0.6)
  }
  p_abs <- p_abs +
    geom_vline(xintercept = c(1904, 1930, 1965, 1977), linetype = "dotted",
               color = "grey60", alpha = 0.6) +
    scale_color_manual(values = c("Fishing/Whaling" = "#d62728", "Climate-only" = "#2ca02c")) +
    scale_fill_manual(values = c("Fishing/Whaling" = "#d62728", "Climate-only" = "#2ca02c")) +
    labs(title = metric_name, x = NULL, y = y_label, color = "Scenario", fill = "Scenario") +
    theme_minimal() +
    theme(plot.title = element_text(size = 13, face = "bold"),
          legend.position = "bottom", axis.title.x = element_blank())

  # Panel B: paired ratio
  paired <- fishing_raw %>%
    inner_join(climate_raw, by = c("sim_id", "decade", "start_year", "end_year"),
               suffix = c("_fish", "_clim"))
  fish_col <- paste0(metric_col, "_fish")
  clim_col <- paste0(metric_col, "_clim")

  if (fish_col %in% names(paired) && clim_col %in% names(paired)) {
    paired$ratio <- ifelse(paired[[clim_col]] > 0,
                           paired[[fish_col]] / paired[[clim_col]], NA)
    ratio_stats <- paired %>% group_by(decade, start_year) %>%
      summarise(median = median(ratio, na.rm = TRUE),
                q25 = quantile(ratio, 0.25, na.rm = TRUE),
                q75 = quantile(ratio, 0.75, na.rm = TRUE),
                q05 = quantile(ratio, 0.05, na.rm = TRUE),
                q95 = quantile(ratio, 0.95, na.rm = TRUE),
                .groups = "drop")

    p_ratio <- ggplot(ratio_stats, aes(x = start_year)) +
      geom_hline(yintercept = 1.0, linetype = "solid", color = "grey40") +
      geom_hline(yintercept = 0.75, linetype = "dashed", color = "#e6550d", linewidth = 0.5) +
      geom_hline(yintercept = 0.40, linetype = "dashed", color = "#e31a1c", linewidth = 0.5) +
      geom_hline(yintercept = 0.20, linetype = "dashed", color = "#67000d", linewidth = 0.5)

    if (metric_col %in% WHALE_METRICS) {
      p_ratio <- p_ratio +
        geom_hline(yintercept = 0.54, linetype = "longdash", color = "#6a3d9a", linewidth = 0.5) +
        annotate("text", x = min(ratio_stats$start_year), y = 0.56,
                 label = "IWC RMP 0.54", hjust = 0, size = 2.5, color = "#6a3d9a")
    }
    p_ratio <- p_ratio +
      geom_ribbon(aes(ymin = q05, ymax = q95), fill = "#7570b3", alpha = 0.15) +
      geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#7570b3", alpha = 0.3) +
      geom_line(aes(y = median), color = "#7570b3", linewidth = 1) +
      geom_point(aes(y = median), color = "#7570b3", size = 1.8) +
      geom_vline(xintercept = c(1904, 1930, 1965, 1977), linetype = "dotted",
                 color = "grey60", alpha = 0.6) +
      annotate("text", x = min(ratio_stats$start_year), y = 0.77,
               label = "CCAMLR g2 0.75", hjust = 0, size = 2.5, color = "#e6550d") +
      annotate("text", x = min(ratio_stats$start_year), y = 0.42,
               label = "BMSY 0.40", hjust = 0, size = 2.5, color = "#e31a1c") +
      annotate("text", x = min(ratio_stats$start_year), y = 0.22,
               label = "CCAMLR g1 0.20", hjust = 0, size = 2.5, color = "#67000d") +
      labs(x = "Year", y = "Fishing / Climate-only ratio") +
      theme_minimal() + theme(axis.title = element_text(size = 10))

    combined <- p_abs / p_ratio + plot_layout(heights = c(2, 1))
  } else {
    combined <- p_abs
  }

  ggsave(file.path(output_path, paste0(metric_col, "_timeseries.png")),
         combined, width = 11, height = 8, dpi = 300)
  ggsave(file.path(output_path, paste0(metric_col, "_timeseries.pdf")),
         combined, width = 11, height = 8)
  return(combined)
}


###############################################################################
# Structural Time Series (difference panel with B0 envelope)
###############################################################################

create_structural_timeseries <- function(fishing_raw, climate_raw, b0_reference,
                                         empirical_thresholds,
                                         metric_col, metric_name, y_label,
                                         output_path) {
  if (!(metric_col %in% names(fishing_raw))) {
    cat(sprintf("  Warning: %s not found\n", metric_col)); return(NULL)
  }

  summarise_scenario <- function(raw, scenario) {
    raw %>% group_by(decade, start_year) %>%
      summarise(median = median(.data[[metric_col]], na.rm = TRUE),
                q25 = quantile(.data[[metric_col]], 0.25, na.rm = TRUE),
                q75 = quantile(.data[[metric_col]], 0.75, na.rm = TRUE),
                q05 = quantile(.data[[metric_col]], 0.05, na.rm = TRUE),
                q95 = quantile(.data[[metric_col]], 0.95, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(scenario = scenario)
  }
  plot_data <- bind_rows(summarise_scenario(fishing_raw, "Fishing/Whaling"),
                         summarise_scenario(climate_raw, "Climate-only"))

  # B0 envelope from empirical thresholds
  th <- empirical_thresholds[[metric_col]]
  b0_med <- if (!is.null(th)) th$median else NA
  b0_q05 <- if (!is.null(th)) th$q05 else NA
  b0_q95 <- if (!is.null(th)) th$q95 else NA
  b0_q25 <- if (!is.null(th)) th$q25 else NA
  b0_q75 <- if (!is.null(th)) th$q75 else NA

  # Panel A: absolute values with B0 envelope
  p_abs <- ggplot(plot_data, aes(x = start_year, color = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.12, color = NA) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.25, color = NA) +
    geom_line(aes(y = median), linewidth = 1) +
    geom_point(aes(y = median), size = 1.8)

  if (!is.na(b0_q05)) {
    p_abs <- p_abs +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = b0_q05, ymax = b0_q95,
               fill = "grey50", alpha = 0.12) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = b0_q25, ymax = b0_q75,
               fill = "grey50", alpha = 0.15) +
      geom_hline(yintercept = b0_med, linetype = "dashed", color = "grey40", linewidth = 0.6)
  }
  p_abs <- p_abs +
    geom_vline(xintercept = c(1904, 1930, 1965, 1977), linetype = "dotted",
               color = "grey60", alpha = 0.6) +
    scale_color_manual(values = c("Fishing/Whaling" = "#d62728", "Climate-only" = "#2ca02c")) +
    scale_fill_manual(values = c("Fishing/Whaling" = "#d62728", "Climate-only" = "#2ca02c")) +
    labs(title = paste0(metric_name, " (B0 envelope: grey bands)"),
         x = NULL, y = y_label, color = "Scenario", fill = "Scenario") +
    theme_minimal() +
    theme(plot.title = element_text(size = 13, face = "bold"),
          legend.position = "bottom", axis.title.x = element_blank())

  # Panel B: paired difference (fishing - climate-only)
  paired <- fishing_raw %>%
    inner_join(climate_raw, by = c("sim_id", "decade", "start_year", "end_year"),
               suffix = c("_fish", "_clim"))
  fish_col <- paste0(metric_col, "_fish")
  clim_col <- paste0(metric_col, "_clim")

  if (fish_col %in% names(paired) && clim_col %in% names(paired)) {
    paired$diff <- paired[[fish_col]] - paired[[clim_col]]
    diff_stats <- paired %>% group_by(decade, start_year) %>%
      summarise(median = median(diff, na.rm = TRUE),
                q25 = quantile(diff, 0.25, na.rm = TRUE),
                q75 = quantile(diff, 0.75, na.rm = TRUE),
                q05 = quantile(diff, 0.05, na.rm = TRUE),
                q95 = quantile(diff, 0.95, na.rm = TRUE),
                .groups = "drop")

    p_diff <- ggplot(diff_stats, aes(x = start_year)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "grey40") +
      geom_ribbon(aes(ymin = q05, ymax = q95), fill = "#7570b3", alpha = 0.15) +
      geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#7570b3", alpha = 0.3) +
      geom_line(aes(y = median), color = "#7570b3", linewidth = 1) +
      geom_point(aes(y = median), color = "#7570b3", size = 1.8) +
      geom_vline(xintercept = c(1904, 1930, 1965, 1977), linetype = "dotted",
                 color = "grey60", alpha = 0.6) +
      labs(x = "Year", y = paste0("\u0394 (Fishing - Climate-only)")) +
      theme_minimal() + theme(axis.title = element_text(size = 10))

    combined <- p_abs / p_diff + plot_layout(heights = c(2, 1))
  } else {
    combined <- p_abs
  }

  ggsave(file.path(output_path, paste0(metric_col, "_timeseries.png")),
         combined, width = 11, height = 8, dpi = 300)
  ggsave(file.path(output_path, paste0(metric_col, "_timeseries.pdf")),
         combined, width = 11, height = 8)
  return(combined)
}


###############################################################################
# Key Biomass Panel
###############################################################################

create_key_biomass_panel <- function(fishing_raw, climate_raw, b0_reference,
                                     output_path) {
  cat("Creating key biomass panel...\n")
  key_metrics <- list(
    list(col = "baleen_biomass", name = "Baleen Whales"),
    list(col = "whale_biomass",  name = "All Whales"),
    list(col = "krill_biomass",  name = "Antarctic Krill"),
    list(col = "apex_biomass",   name = "Apex Predators"))

  panel_plots <- list()
  for (metric in key_metrics) {
    make_stats <- function(raw, scenario) {
      raw %>% group_by(start_year) %>%
        summarise(median = median(.data[[metric$col]], na.rm = TRUE),
                  q25 = quantile(.data[[metric$col]], 0.25, na.rm = TRUE),
                  q75 = quantile(.data[[metric$col]], 0.75, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(scenario = scenario)
    }
    plot_data <- bind_rows(make_stats(fishing_raw, "Fishing"),
                           make_stats(climate_raw, "Climate-only"))
    b0_med <- NA
    if (metric$col %in% names(b0_reference)) {
      b0_vals <- b0_reference[[metric$col]][!is.na(b0_reference[[metric$col]])]
      if (length(b0_vals) > 0) b0_med <- median(b0_vals)
    }
    p <- ggplot(plot_data, aes(x = start_year, color = scenario, fill = scenario)) +
      geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.25, color = NA) +
      geom_line(aes(y = median), linewidth = 1)
    if (!is.na(b0_med))
      p <- p + geom_hline(yintercept = b0_med, linetype = "dashed", color = "grey40", linewidth = 0.5)
    p <- p +
      scale_color_manual(values = c("Fishing" = "#d62728", "Climate-only" = "#2ca02c")) +
      scale_fill_manual(values = c("Fishing" = "#d62728", "Climate-only" = "#2ca02c")) +
      labs(title = metric$name, x = "Year",
           y = expression(paste("Biomass (g/m"^2, ")"))) +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold"),
            legend.position = "none", axis.title.x = element_blank())
    panel_plots[[metric$col]] <- p
  }

  combined <- (panel_plots[[1]] | panel_plots[[2]]) /
    (panel_plots[[3]] | panel_plots[[4]]) +
    plot_annotation(
      title = "Key Ecosystem Biomass Indicators (Paired Ensemble)",
      subtitle = paste0("Fishing/Whaling (red) vs Climate-only (green); ",
                        "dashed = B0 median (1841-1860)"),
      caption = "Shaded: 50% credible intervals") &
    theme(plot.title = element_text(size = 14, face = "bold"))

  ggsave(file.path(output_path, "key_biomass_panel.png"), combined, width = 12, height = 8, dpi = 300)
  ggsave(file.path(output_path, "key_biomass_panel.pdf"), combined, width = 12, height = 8)
  return(combined)
}


###############################################################################
# Attribution Panel
###############################################################################

create_attribution_panel <- function(biomass_summaries, structural_summaries,
                                     output_path) {
  cat("Creating attribution panel...\n")
  key_metrics <- c("total_biomass", "baleen_biomass", "krill_biomass", "apex_biomass")
  metric_labels <- c("Total Biomass", "Baleen Whales", "Krill", "Apex Predators")

  panel_plots <- list()
  for (idx in seq_along(key_metrics)) {
    metric <- key_metrics[idx]
    gather_comp <- function(comp_name, comp_label) {
      biomass_summaries[[comp_name]] %>%
        filter(metric_name == metric) %>%
        select(start_year, ratio_median, ratio_q25, ratio_q75) %>%
        mutate(comparison = comp_label)
    }
    plot_data <- bind_rows(
      gather_comp("absolute", "Total (Fishing / B0)"),
      gather_comp("climate", "Climate (Climate-only / B0)"),
      gather_comp("exploit", "Exploitation (Fishing / Climate-only)"))

    p <- ggplot(plot_data, aes(x = start_year, color = comparison, fill = comparison)) +
      geom_hline(yintercept = 1.0, linetype = "solid", color = "grey50") +
      geom_hline(yintercept = 0.75, linetype = "dotted", color = "grey40") +
      geom_ribbon(aes(ymin = ratio_q25, ymax = ratio_q75), alpha = 0.15, color = NA) +
      geom_line(aes(y = ratio_median), linewidth = 0.9)
    if (metric %in% WHALE_METRICS) {
      p <- p + geom_hline(yintercept = 0.54, linetype = "longdash",
                           color = "#6a3d9a", linewidth = 0.4, alpha = 0.7)
    }
    p <- p +
      scale_color_manual(values = c("Total (Fishing / B0)" = "#d62728",
                                     "Climate (Climate-only / B0)" = "#2ca02c",
                                     "Exploitation (Fishing / Climate-only)" = "#7570b3")) +
      scale_fill_manual(values = c("Total (Fishing / B0)" = "#d62728",
                                    "Climate (Climate-only / B0)" = "#2ca02c",
                                    "Exploitation (Fishing / Climate-only)" = "#7570b3")) +
      labs(title = metric_labels[idx], x = "Year", y = "Ratio") +
      theme_minimal() +
      theme(plot.title = element_text(size = 11, face = "bold"),
            legend.position = "none", axis.title.x = element_blank())
    panel_plots[[idx]] <- p
  }

  combined <- (panel_plots[[1]] | panel_plots[[2]]) /
    (panel_plots[[3]] | panel_plots[[4]]) +
    plot_annotation(
      title = "Impact Attribution: Climate vs Exploitation",
      subtitle = "Red = total, Green = climate, Purple = exploitation",
      caption = "Dotted: 0.75 (CCAMLR g2) | Purple dashed on whale panels: 0.54 (IWC RMP)") &
    theme(plot.title = element_text(size = 14, face = "bold"))

  ggsave(file.path(output_path, "attribution_panel.png"), combined, width = 13, height = 8, dpi = 300)
  ggsave(file.path(output_path, "attribution_panel.pdf"), combined, width = 13, height = 8)
  return(combined)
}


###############################################################################
# Period Comparison Barplot
###############################################################################

create_period_comparison <- function(biomass_summaries, output_path) {
  cat("Creating period comparison barplots...\n")
  key_decades <- c("1841-1850", "1931-1940", "1961-1970", "2001-2010")
  period_labels <- c("Pre-exploitation", "Peak Whaling", "Post-moratorium", "Modern")
  biomass_metrics <- c("baleen_biomass", "whale_biomass", "krill_biomass",
                       "apex_biomass", "total_biomass", "fish_biomass")
  biomass_labels <- c("Baleen Whales", "All Whales", "Krill",
                      "Apex Predators", "Total Biomass", "Fish")

  plot_data <- biomass_summaries$absolute %>%
    filter(decade %in% key_decades, metric_name %in% biomass_metrics) %>%
    mutate(period_label = factor(decade, levels = key_decades, labels = period_labels),
           metric_label = factor(metric_name, levels = biomass_metrics, labels = biomass_labels))

  p <- ggplot(plot_data, aes(x = metric_label, y = ratio_median, fill = period_label)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = ratio_q25, ymax = ratio_q75),
                  position = position_dodge(width = 0.8), width = 0.2) +
    geom_hline(yintercept = 1.0, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0.75, linetype = "dotted", color = "#e6550d") +
    geom_hline(yintercept = 0.40, linetype = "dotted", color = "#e31a1c") +
    scale_fill_viridis_d(option = "viridis", direction = -1) +
    labs(title = "Absolute Ecosystem Health Across Key Periods",
         subtitle = sprintf("Ratio: Fishing / B0 (%d-%d)", B0_PERIOD[1], B0_PERIOD[2]),
         x = "Metric", y = "Ratio (Fishing / B0)", fill = "Period") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10),
          plot.title = element_text(size = 14, face = "bold"),
          legend.position = "right") +
    annotate("text", x = 0.5, y = 0.78, label = "0.75 (CCAMLR g2)",
             hjust = 0, size = 2.8, color = "#e6550d") +
    annotate("text", x = 0.5, y = 0.43, label = "0.40 (BMSY proxy)",
             hjust = 0, size = 2.8, color = "#e31a1c")

  ggsave(file.path(output_path, "period_comparison_barplot.png"), p, width = 12, height = 7, dpi = 300)
  ggsave(file.path(output_path, "period_comparison_barplot.pdf"), p, width = 12, height = 7)
  return(p)
}


###############################################################################
# Run All Diagnostics
###############################################################################

create_all_diagnostics <- function() {
  cat("=============================================================\n")
  cat("CREATING DIAGNOSTIC PLOTS (v2)\n")
  cat("=============================================================\n\n")

  fishing_raw  <- readRDS(file.path(INPUT_DIR, "fishing_metrics_raw.rds"))
  climate_raw  <- readRDS(file.path(INPUT_DIR, "climate_only_metrics_raw.rds"))
  b0_reference <- readRDS(file.path(INPUT_DIR, "b0_reference.rds"))
  empirical_thresholds <- readRDS(file.path(INPUT_DIR, "empirical_thresholds.rds"))

  biomass_summaries <- list(
    exploit  = read.csv(file.path(INPUT_DIR, "summary_biomass_exploit.csv")),
    absolute = read.csv(file.path(INPUT_DIR, "summary_biomass_absolute.csv")),
    climate  = read.csv(file.path(INPUT_DIR, "summary_biomass_climate.csv")))

  # --- Biomass time series (Category A) ---
  biomass_to_plot <- list(
    list(col = "total_biomass", name = "Total Community Biomass",
         label = expression(paste("Biomass (g/m"^2, ")"))),
    list(col = "baleen_biomass", name = "Baleen Whale Biomass",
         label = expression(paste("Biomass (g/m"^2, ")"))),
    list(col = "whale_biomass", name = "All Whale Biomass",
         label = expression(paste("Biomass (g/m"^2, ")"))),
    list(col = "seal_biomass", name = "Seal Biomass",
         label = expression(paste("Biomass (g/m"^2, ")"))),
    list(col = "fish_biomass", name = "Fish Biomass",
         label = expression(paste("Biomass (g/m"^2, ")"))),
    list(col = "krill_biomass", name = "Antarctic Krill Biomass",
         label = expression(paste("Biomass (g/m"^2, ")"))),
    list(col = "ltl_biomass", name = "Lower Trophic Level Biomass",
         label = expression(paste("Biomass (g/m"^2, ")"))),
    list(col = "apex_biomass", name = "Apex Predator Biomass",
         label = expression(paste("Biomass (g/m"^2, ")"))))

  plots <- list()
  cat("Biomass time series (Category A - ratio panels):\n")
  for (metric in biomass_to_plot) {
    cat(sprintf("  %s\n", metric$name))
    plots[[metric$col]] <- create_biomass_timeseries(
      fishing_raw, climate_raw, b0_reference,
      metric$col, metric$name, metric$label, OUTPUT_DIR)
  }

  # --- Structural time series (Category B) ---
  structural_to_plot <- list(
    list(col = "spectrum_slope",           name = "Size Spectrum Slope",              label = "Slope"),
    list(col = "spectrum_intercept",       name = "Size Spectrum Intercept",          label = "Intercept"),
    list(col = "mean_tl",                  name = "Mean Trophic Level (Consumers)",   label = "Trophic Level"),
    list(col = "htl_indicator",            name = "High Trophic Level Indicator",     label = "Proportion"),
    list(col = "large_fish_indicator",     name = "Fish LFI (100g)",              label = "Proportion"),
    list(col = "fish_lfi",                 name = "Fish LFI (1000g)",                label = "Proportion"),
    list(col = "shannon_diversity",        name = "Shannon Diversity (H')",           label = "H'"),
    list(col = "w_statistic",              name = "W-Statistic (ABC Curves)",         label = "W"),
    list(col = "production_biomass_ratio", name = "Production:Biomass Ratio (P/B)",   label = "P/B (yr^-1)"),
    list(col = "mean_weight",              name = "Mean Individual Weight",           label = "Weight (g)"),
    list(col = "mean_max_weight",          name = "Mean Max Weight",                  label = "Weight (g)"),
    list(col = "predator_prey_ratio",      name = "Predator-Prey Ratio (Apex:Mid-TL)", label = "Ratio"),
    list(col = "consumer_ltl_ratio",       name = "Consumer:LTL Biomass Ratio",      label = "Ratio"))

  cat("Structural time series (Category B - difference panels with B0 envelope):\n")
  for (metric in structural_to_plot) {
    cat(sprintf("  %s\n", metric$name))
    plots[[metric$col]] <- create_structural_timeseries(
      fishing_raw, climate_raw, b0_reference, empirical_thresholds,
      metric$col, metric$name, metric$label, OUTPUT_DIR)
  }

  # Multi-panel summaries
  create_key_biomass_panel(fishing_raw, climate_raw, b0_reference, OUTPUT_DIR)
  create_attribution_panel(biomass_summaries, NULL, OUTPUT_DIR)
  create_period_comparison(biomass_summaries, OUTPUT_DIR)

  cat("\n=============================================================\n")
  cat("DIAGNOSTIC PLOTS COMPLETE\n")
  cat(sprintf("Output: %s\n", OUTPUT_DIR))
  cat("=============================================================\n")
  return(plots)
}

# Auto-run if inputs exist
required_files <- c("fishing_metrics_raw.rds", "climate_only_metrics_raw.rds",
                    "b0_reference.rds", "empirical_thresholds.rds")
missing <- required_files[!file.exists(file.path(INPUT_DIR, required_files))]
if (length(missing) > 0) {
  cat("Run ecosystem_assessment_v2.R first. Missing:\n")
  cat(paste("  -", missing, collapse = "\n"), "\n")
} else {
  plots <- create_all_diagnostics()
}
