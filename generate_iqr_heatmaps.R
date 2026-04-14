###############################################################################
# Generate IQR-based structural deviation heatmaps
#
# Uses the B0 interquartile range (25th-75th percentile) as the reference
# envelope instead of the 90% CI (5th-95th).
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(viridis)
  library(patchwork)
  library(ggtext)
})

setwd("C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/Prydz_Bay_mizer")

cat("Sourcing ecosystem_assessment_v2.R for functions & constants...\n")
source("ecosystem_assessment_v2.R")

data_dir   <- "Output_large_files/ecosystem_assessment"
output_dir <- "ecosystem_assessment_outputs"

# -------------------------------------------------------------------------
# Load data
# -------------------------------------------------------------------------
cat("Loading data files...\n")
paired   <- readRDS(file.path(data_dir, "paired_data.rds"))
thresh   <- readRDS(file.path(data_dir, "empirical_thresholds.rds"))

cat(sprintf("  paired_data: %d rows (%d sims)\n",
            nrow(paired), length(unique(paired$sim_id))))

# -------------------------------------------------------------------------
# Structural envelope flags: use ORIGINAL unpaired approach from paired_data.rds
# (raw value vs B0 global quantiles). Null-subtraction is applied at plot time.
# -------------------------------------------------------------------------
cat("\nUsing original structural envelope flags from cached paired_data...\n")

# -------------------------------------------------------------------------
# Summarize with IQR envelope
# -------------------------------------------------------------------------
cat("\nSummarizing structural deviations with IQR envelope...\n")
diff_suffix    <- c(exploit = "_exploit_diff", absolute = "_absolute_diff",
                    climate = "_climate_diff")
envelope_prefix <- c(exploit = "", absolute = "", climate = "_clim")

all_summaries <- list()
for (comp in c("exploit", "absolute", "climate")) {
  summary_rows <- list()
  for (metric in STRUCTURAL_METRICS) {
    diff_col <- paste0(metric, diff_suffix[comp])
    fish_col <- paste0(metric, "_fish")
    if (!(diff_col %in% names(paired))) next

    th <- thresh[[metric]]
    b0_median <- if (!is.null(th)) th$median else NA
    b0_sd     <- if (!is.null(th)) th$sd     else NA

    env_suffix     <- envelope_prefix[comp]
    outside_50_col <- paste0(metric, env_suffix, "_outside_b0_50")
    outside_90_col <- paste0(metric, env_suffix, "_outside_b0_90")
    outside_98_col <- paste0(metric, env_suffix, "_outside_b0_98")
    below_q05_col  <- paste0(metric, "_below_b0_q05")
    above_q95_col  <- paste0(metric, "_above_b0_q95")

    decade_summaries <- paired %>%
      group_by(decade, start_year, end_year) %>%
      summarise(
        metric_name       = metric,
        metric_category   = "structural",
        comparison        = comp,
        n_sims            = n(),
        diff_median       = median(.data[[diff_col]], na.rm = TRUE),
        diff_mean         = mean(.data[[diff_col]], na.rm = TRUE),
        diff_sd           = sd(.data[[diff_col]], na.rm = TRUE),
        diff_q05          = quantile(.data[[diff_col]], 0.05, na.rm = TRUE),
        diff_q25          = quantile(.data[[diff_col]], 0.25, na.rm = TRUE),
        diff_q75          = quantile(.data[[diff_col]], 0.75, na.rm = TRUE),
        diff_q95          = quantile(.data[[diff_col]], 0.95, na.rm = TRUE),
        z_median = if (!is.na(b0_sd) && b0_sd > 0) {
          median(.data[[diff_col]], na.rm = TRUE) / b0_sd
        } else NA_real_,
        value_median = if (fish_col %in% names(paired)) {
          median(.data[[fish_col]], na.rm = TRUE)
        } else NA_real_,
        value_q25 = if (fish_col %in% names(paired)) {
          quantile(.data[[fish_col]], 0.25, na.rm = TRUE)
        } else NA_real_,
        value_q75 = if (fish_col %in% names(paired)) {
          quantile(.data[[fish_col]], 0.75, na.rm = TRUE)
        } else NA_real_,
        n_valid = sum(!is.na(.data[[diff_col]])),
        prop_outside_b0_50 = if (outside_50_col %in% names(paired)) {
          mean(.data[[outside_50_col]], na.rm = TRUE)
        } else NA_real_,
        prop_outside_b0_90 = if (outside_90_col %in% names(paired)) {
          mean(.data[[outside_90_col]], na.rm = TRUE)
        } else NA_real_,
        prop_outside_b0_98 = if (outside_98_col %in% names(paired)) {
          mean(.data[[outside_98_col]], na.rm = TRUE)
        } else NA_real_,
        prop_below_b0_q05 = if (below_q05_col %in% names(paired) && comp != "climate") {
          mean(.data[[below_q05_col]], na.rm = TRUE)
        } else NA_real_,
        prop_above_b0_q95 = if (above_q95_col %in% names(paired) && comp != "climate") {
          mean(.data[[above_q95_col]], na.rm = TRUE)
        } else NA_real_,
        b0_median = b0_median,
        b0_sd     = b0_sd,
        .groups   = "drop"
      )
    summary_rows[[length(summary_rows) + 1]] <- decade_summaries
  }
  all_summaries[[comp]] <- bind_rows(summary_rows)
  cat(sprintf("  %s: %d records\n", comp, nrow(all_summaries[[comp]])))
}

# -------------------------------------------------------------------------
# Plot IQR heatmaps (same function as 90% but with IQR column)
# -------------------------------------------------------------------------
cat("\nGenerating IQR heatmaps...\n")

plot_iqr_heatmap <- function(structural_summary, envelope_col, envelope_label,
                             title, output_path, filename,
                             null_expected = STRUCTURAL_NULL_90CI) {
  plot_data <- filter_decades(structural_summary) %>%
    filter(metric_name %in% STRUCTURAL_METRICS_PLOT) %>%
    mutate(display_name = sapply(metric_name, get_display_name),
           prop_value = pmax((.data[[envelope_col]] - null_expected) / (1 - null_expected), 0),
           conf_level = bin_ipcc_confidence(prop_value))
  if (all(is.na(plot_data$prop_value))) {
    cat(sprintf("  Skipping %s (no valid data)\n", filename)); return(NULL)
  }
  str_names <- sapply(STRUCTURAL_METRICS_PLOT, get_display_name)
  valid_names <- str_names[str_names %in% plot_data$display_name]
  decade_order <- unique(plot_data$decade[order(plot_data$start_year)])
  plot_data$decade <- factor(plot_data$decade, levels = decade_order)
  plot_data$display_name <- factor(plot_data$display_name, levels = rev(valid_names))
  if (nrow(plot_data) == 0) { cat(sprintf("  Skipping %s\n", filename)); return(NULL) }

  p <- ggplot(plot_data, aes(x = decade, y = display_name, fill = conf_level)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", prop_value * 100)), size = 2.8, color = "black") +
    scale_fill_manual(
      values = IPCC_CONF_COLOURS, drop = FALSE, na.value = "grey80",
      name = "Confidence\nin change") +
    labs(title = title,
         subtitle = sprintf("Skill-score normalised confidence (null = %.0f%%)", null_expected * 100),
         x = "Decade", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 9.5, hjust = 0.5),
          legend.position = "right", panel.grid = element_blank())

  ggsave(file.path(output_path, paste0(filename, ".png")), p, width = 15, height = 7, dpi = 300)
  cat(sprintf("  Saved: %s.png\n", filename))
  return(p)
}

# Absolute (fishing vs B0)
plot_iqr_heatmap(
  all_summaries$absolute, "prop_outside_b0_50",
  "25th\u201375th percentile (IQR, empirical B0 envelope, 1841\u20131860)",
  "Structural Metrics: Outside B0 IQR",
  output_dir, "heatmap_structural_outside_b0_iqr")

# Exploitation (fishing vs climate-only)
plot_iqr_heatmap(
  all_summaries$exploit, "prop_outside_b0_50",
  "25th\u201375th percentile (IQR, B0 envelope)",
  "Structural Metrics: Exploitation-Driven Departure (IQR)",
  output_dir, "heatmap_structural_outside_b0_iqr_exploitation")

# Climate impact (climate-only vs B0)
plot_iqr_heatmap(
  all_summaries$climate, "prop_outside_b0_50",
  "25th\u201375th percentile (IQR, B0 envelope)",
  "Structural Metrics: Climate-Driven Departure (IQR)",
  output_dir, "heatmap_structural_outside_b0_iqr_climate")

# -------------------------------------------------------------------------
# Combined heatmaps: single-stacked (biomass + skill-score normalised structural)
# Uses plot_combined_heatmap() from ecosystem_assessment_v2.R
# -------------------------------------------------------------------------
cat("\nGenerating combined single-stacked heatmaps (skill-score normalised structural)...\n")

# We need the biomass summaries too
biomass_summaries <- summarize_biomass_ratios(paired)

plot_combined_heatmap(
  biomass_summaries$absolute, all_summaries$absolute,
  "Ecosystem Assessment: Prydz Bay (Absolute)",
  output_dir, "heatmap_combined_absolute")
plot_combined_heatmap(
  biomass_summaries$exploit, all_summaries$exploit,
  "Ecosystem Assessment: Exploitation Impact",
  output_dir, "heatmap_combined_exploitation")
plot_combined_heatmap(
  biomass_summaries$climate, all_summaries$climate,
  "Ecosystem Assessment: Climate Impact",
  output_dir, "heatmap_combined_climate")

# -------------------------------------------------------------------------
# Triple-stacked combined heatmaps (biomass + structural + exploitation)
# Single-column layout with skill-score normalised structural
# -------------------------------------------------------------------------
cat("\nGenerating triple-stacked combined heatmaps (biomass + structural + exploitation)...\n")

# Load species exploitation summary (cached)
species_exploit_summary <- compute_species_exploitation_summary()

plot_triple_combined_stacked <- function(biomass_summary, structural_summary,
                                          species_exploit_summary,
                                          title, output_path, filename) {
  fill_colours <- c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027")
  fill_values  <- c(0, 0.1, 0.25, 0.5, 0.75, 1)

  # --- Row 1: Biomass panel ---
  bio_d <- filter_decades(biomass_summary) %>%
    mutate(
      threshold = BIOMASS_THRESHOLD_MAP[metric_name],
      prop_value = case_when(
        threshold == 0.90 ~ prop_below_090,
        threshold == 0.75 ~ prop_below_075,
        threshold == 0.54 ~ prop_below_054,
        threshold == 0.40 ~ prop_below_040,
        TRUE ~ prop_below_075),
      display_name = sapply(metric_name, get_biomass_rich_label)
    ) %>% filter(!is.na(prop_value))
  bio_rich <- sapply(BIOMASS_METRICS, get_biomass_rich_label)
  bio_valid <- bio_rich[bio_rich %in% bio_d$display_name]
  decade_order <- unique(bio_d$decade[order(bio_d$start_year)])
  bio_d$decade <- factor(bio_d$decade, levels = decade_order)
  bio_d$display_name <- factor(bio_d$display_name, levels = rev(bio_valid))

  p_bio <- ggplot(bio_d, aes(x = decade, y = display_name, fill = prop_value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", prop_value * 100)), size = 2.8) +
    scale_fill_gradientn(colours = fill_colours, values = fill_values,
      limits = c(0, 1), na.value = "grey80",
      name = "Proportion\naltered", labels = scales::percent) +
    labs(x = "", y = "", tag = "Biomass") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 8),
          plot.tag = element_text(size = 9, face = "bold", angle = 90),
          plot.tag.position = "left",
          legend.position = "none", panel.grid = element_blank(),
          plot.margin = margin(5, 2, 0, 5))

  # --- Row 2: Structural panel (skill-score normalised) ---
  str_d <- filter_decades(structural_summary) %>%
    filter(metric_name %in% STRUCTURAL_METRICS_PLOT) %>%
    filter(!is.na(prop_outside_b0_90)) %>%
    mutate(display_name = sapply(metric_name, get_display_name),
           prop_value = pmax((prop_outside_b0_90 - STRUCTURAL_NULL_90CI) / (1 - STRUCTURAL_NULL_90CI), 0),
           conf_level = bin_ipcc_confidence(prop_value))
  str_names <- sapply(STRUCTURAL_METRICS_PLOT, get_display_name)
  str_valid <- str_names[str_names %in% str_d$display_name]
  str_d$decade <- factor(str_d$decade, levels = decade_order)
  str_d$display_name <- factor(str_d$display_name, levels = rev(str_valid))

  p_str <- ggplot(str_d, aes(x = decade, y = display_name, fill = conf_level)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", prop_value * 100)), size = 2.8) +
    scale_fill_manual(values = IPCC_CONF_COLOURS,
      drop = FALSE, na.value = "grey80",
      name = "Confidence\nin change") +
    labs(x = "", y = "", tag = "Structural") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 8),
          plot.tag = element_text(size = 9, face = "bold", angle = 90),
          plot.tag.position = "left",
          legend.position = "none", panel.grid = element_blank(),
          plot.margin = margin(0, 2, 0, 5))

  # --- Row 3: Exploitation panel ---
  exploit_data <- filter_decades(species_exploit_summary) %>%
    filter(species %in% names(FISHED_SPECIES_DISPLAY))
  max_F <- max(exploit_data$F_q95, na.rm = TRUE)
  scale_max <- max(0.2, ceiling(max_F * 10) / 10)

  p_exp <- make_exploit_panel(species_exploit_summary, "F_median", scale_max,
                               show_y = TRUE, show_x = TRUE, text_size = 2.8) +
    labs(tag = "Exploitation") +
    theme(plot.tag = element_text(size = 9, face = "bold", angle = 90),
          plot.tag.position = "left",
          plot.margin = margin(0, 2, 5, 5))

  # --- Shared legends ---
  p_for_prop_legend <- ggplot(data.frame(x = rep("a", 5), y = rep("b", 5),
                                          conf = factor(IPCC_CONF_LEVELS, levels = IPCC_CONF_LEVELS)),
                               aes(x, y, fill = conf)) +
    geom_tile() +
    scale_fill_manual(values = IPCC_CONF_COLOURS,
      drop = FALSE, na.value = "grey80",
      name = "Confidence\nin change") +
    theme(legend.position = "right")
  prop_legend <- cowplot::get_legend(p_for_prop_legend)

  p_for_f_legend <- ggplot(data.frame(x = "a", y = "b", v = scale_max / 2),
                            aes(x, y, fill = v)) +
    geom_tile() +
    scale_fill_gradientn(
      colours = c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027"),
      values = scales::rescale(c(0, 0.005, 0.02, 0.05, 0.15, scale_max), to = c(0, 1)),
      limits = c(0, scale_max), na.value = "grey90",
      name = "F\n(Yield/Biomass)",
      labels = scales::label_number(accuracy = 0.01)) +
    theme(legend.position = "right")
  f_legend <- cowplot::get_legend(p_for_f_legend)

  # --- Assemble: 3x2 grid with legends right-aligned per row ---
  bio_legend <- cowplot::get_legend(
    ggplot(data.frame(x = "a", y = "b", v = 0.5), aes(x, y, fill = v)) +
      geom_tile() +
      scale_fill_gradientn(colours = fill_colours, values = fill_values,
        limits = c(0, 1), na.value = "grey80",
        name = "Proportion\naltered", labels = scales::percent) +
      theme(legend.position = "right"))
  n_bio <- length(bio_valid)
  n_str <- length(str_valid)
  n_exp <- length(unique(exploit_data$species))

  p_combined <- (p_bio + wrap_elements(bio_legend) +
                 p_str + wrap_elements(prop_legend) +
                 p_exp + wrap_elements(f_legend)) +
    plot_layout(
      ncol = 2,
      nrow = 3,
      widths = c(1, 0.18),
      heights = c(n_bio, n_str, n_exp)
    ) +
    plot_annotation(
      caption = paste0(
        "Figure. Triple-stacked ecosystem assessment for the Prydz Bay mizer model. ",
        "Row 1 (Biomass): proportion of Monte Carlo simulations (n = 2,111) in which ",
        "biomass fell below the metric-specific depletion threshold relative to B\u2080 (1841\u20131860). ",
        "Symbols: \u2020 CCAMLR \u03b3\u2082 (B/B\u2080 < 0.75); ",
        "\u2021 IWC RMP (B/B\u2080 < 0.54); \u00a7 B\u2098\u209b\u2099 proxy (B/B\u2080 < 0.40). ",
        "Row 2 (Structural): skill-score normalised confidence in departure from B\u2080 90% envelope. ",
        "Row 3 (Exploitation): median fishing mortality rate (F = Yield/Biomass)."),
      theme = theme(
        plot.caption = element_text(size = 7, hjust = 0, color = "grey30",
                                    lineheight = 1.2),
        plot.caption.position = "plot")
    )

  ggsave(file.path(output_path, paste0(filename, ".png")), p_combined,
         width = 14, height = 12, dpi = 300)
  cat(sprintf("  Saved: %s.png\n", filename))
  return(p_combined)
}

# Generate triple-stacked for absolute scenario
plot_triple_combined_stacked(
  biomass_summaries$absolute, all_summaries$absolute,
  species_exploit_summary,
  "Ecosystem Assessment: Prydz Bay (Absolute)",
  output_dir, "heatmap_triple_combined_absolute")

# Generate triple-stacked for exploitation impact
plot_triple_combined_stacked(
  biomass_summaries$exploit, all_summaries$exploit,
  species_exploit_summary,
  "Ecosystem Assessment: Exploitation Impact",
  output_dir, "heatmap_triple_combined_exploitation")

# Generate triple-stacked for climate impact
plot_triple_combined_stacked(
  biomass_summaries$climate, all_summaries$climate,
  species_exploit_summary,
  "Ecosystem Assessment: Climate Impact",
  output_dir, "heatmap_triple_combined_climate")

# Save updated summary CSVs
cat("\nSaving IQR summary CSVs...\n")
for (comp in c("exploit", "absolute", "climate")) {
  write.csv(all_summaries[[comp]],
            file.path(output_dir, sprintf("summary_structural_%s_iqr.csv", comp)),
            row.names = FALSE)
}

cat("\n=== Done ===\n")
cat("Heatmaps saved to: ecosystem_assessment_outputs/\n")
cat("Files:\n")
cat("  heatmap_structural_outside_b0_iqr.png                (IQR structural, absolute)\n")
cat("  heatmap_structural_outside_b0_iqr_exploitation.png\n")
cat("  heatmap_structural_outside_b0_iqr_climate.png\n")
cat("  heatmap_combined_absolute.png                        (single-stacked biomass+structural)\n")
cat("  heatmap_combined_exploitation.png\n")
cat("  heatmap_combined_climate.png\n")
cat("  heatmap_triple_combined_absolute.png                 (biomass+structural+exploitation)\n")
cat("  heatmap_triple_combined_exploitation.png\n")
cat("  heatmap_triple_combined_climate.png\n")
