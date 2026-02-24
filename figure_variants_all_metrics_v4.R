###############################################################################
# Ecosystem Assessment — Comprehensive Figure Variants
#
# Generates five figure versions for EVERY biomass and structural metric:
#   1. Combined (ensemble + ratio/difference panel), 1900 onward
#   2. Ensemble panel only, 1900 onward
#   3. Ratio/difference panel only, 1900 onward
#   4. Ensemble with single ribbon (50% CI only), free y-axis, 1900 onward
#   5. Three-panel (ensemble + ratio/diff + breach probability timeseries)
#
# Features:
#   - Species/metric-specific peak-effort vertical lines
#     (peak years from effort_array_1841_2010.rds)
#   - Total biomass: all species peaks colour-coded
#   - Silhouette support for species-specific figures (PNG overlays)
#   - Biomass metrics: ratio panel (fishing / climate-only)
#   - Structural metrics: difference panel (fishing - climate-only)
#   - B0 reference: grey dashed line = pre-exploitation median,
#     grey band = B0 IQR (25th-75th percentile), labelled "B0"
#   - 50% CI variant: y-axis scales to q25/q75 data range (free_y)
#   - Breach probability panel (Variant 5):
#       Biomass: P(ratio < threshold) for CCAMLR g1/g2, BMSY, IWC RMP
#       Structural: P(outside B0 5th-95th) with directional breakdown
#
# Run after: ecosystem_assessment_v2.R
###############################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(grid)
library(png)
library(scales)

# =============================================================================
# CONFIGURATION
# =============================================================================

INPUT_DIR  <- "ecosystem_assessment_outputs"
OUTPUT_DIR <- "ecosystem_assessment_outputs/figure_variants_v4"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

YEAR_MIN <- 1900
WHALE_METRICS <- c("baleen_biomass", "whale_biomass")

# =============================================================================
# HISTORICAL EVENT LINES
# =============================================================================

# Species/metric-specific peak effort events
# Peak years derived from effort_array_1841_2010.rds (year of max effort)
# Each entry: year, label, color, linetype
PEAK_EVENTS <- list(

  baleen_biomass = list(
    list(year = 1933, label = "Peak baleen effort",
         color = "#1f78b4", linetype = "dashed"),
    list(year = 1948, label = "Peak sperm whale effort",
         color = "#6a3d9a", linetype = "dashed"),
    list(year = 1973, label = "Peak minke effort",
         color = "#33a02c", linetype = "dashed"),
    list(year = 1979, label = "Peak krill effort",
         color = "#e31a1c", linetype = "dashed")
  ),

  whale_biomass = list(
    list(year = 1933, label = "Peak baleen effort",
         color = "#1f78b4", linetype = "dashed"),
    list(year = 1948, label = "Peak sperm whale effort",
         color = "#6a3d9a", linetype = "dashed"),
    list(year = 1973, label = "Peak minke effort",
         color = "#33a02c", linetype = "dashed"),
    list(year = 1980, label = "Peak orca effort",
         color = "#e31a1c", linetype = "dashed")
  ),

  seal_biomass = list(
    # No major modern commercial sealing in Prydz Bay region;
    # incidental/historical only. Add events if relevant.
  ),

  fish_biomass = list(
    list(year = 2008, label = "Peak toothfish effort",
         color = "#ff7f00", linetype = "dashed")
  ),

  krill_biomass = list(
    list(year = 1979, label = "Peak krill effort",
         color = "#e31a1c", linetype = "dashed")
  ),

  ltl_biomass = list(
    # No direct exploitation of LTL; krill peak shown for context
    list(year = 1979, label = "Peak krill effort",
         color = "#e31a1c", linetype = "dashed")
  ),

  apex_biomass = list(
    list(year = 1948, label = "Peak sperm whale effort",
         color = "#6a3d9a", linetype = "dashed"),
    list(year = 1980, label = "Peak orca effort",
         color = "#e31a1c", linetype = "dashed")
  ),

  # Total biomass: show all major species peaks, colour-coded
  total_biomass = list(
    list(year = 1933, label = "Peak baleen effort",
         color = "#1f78b4", linetype = "dashed"),
    list(year = 1948, label = "Peak sperm whale effort",
         color = "#6a3d9a", linetype = "dashed"),
    list(year = 1979, label = "Peak krill effort",
         color = "#e31a1c", linetype = "dashed"),
    list(year = 2008, label = "Peak toothfish effort",
         color = "#ff7f00", linetype = "dashed")
  )
)

# Structural metrics: inherit event lines from the most relevant driver.
# Most structural metrics respond to the dominant perturbation (whaling),
# so default to baleen/whale events. Override individually as needed.
STRUCTURAL_DEFAULT_EVENTS <- list(
  list(year = 1933, label = "Peak baleen effort",
       color = "#1f78b4", linetype = "dashed"),
  list(year = 1979, label = "Peak krill effort",
       color = "#e31a1c", linetype = "dashed")
)

# =============================================================================
# SILHOUETTE CONFIGURATION
# =============================================================================
# Set file paths to species silhouette PNGs (transparent background).
# Placement: top-right of biomass ensemble panels.
# Set to NULL to disable for a given metric.
#
# Trial: baleen_biomass and apex_biomass only for now.
# Add additional entries as needed.

SILHOUETTES <- list(
  baleen_biomass = list(
    path = NULL,   # <- SET PATH e.g. "silhouettes/baleen_whale.png"
    x = 0.88,      # normalised x position (0-1, right side)
    y = 0.85,      # normalised y position (0-1, upper area)
    width = 0.18,  # width as fraction of plot
    alpha = 0.3    # transparency (0 = invisible, 1 = opaque)
  ),
  apex_biomass = list(
    path = NULL,   # <- SET PATH e.g. "silhouettes/orca.png"
    x = 0.88,
    y = 0.85,
    width = 0.18,
    alpha = 0.3
  )
)

# =============================================================================
# METRIC DEFINITIONS
# =============================================================================

BIOMASS_METRICS_CONFIG <- list(
  total_biomass  = list(name = "Total Community Biomass",
                        label = expression(paste("Biomass (g/m"^2, ")"))),
  baleen_biomass = list(name = "Baleen Whale Biomass",
                        label = expression(paste("Biomass (g/m"^2, ")"))),
  whale_biomass  = list(name = "All Whale Biomass",
                        label = expression(paste("Biomass (g/m"^2, ")"))),
  seal_biomass   = list(name = "Seal Biomass",
                        label = expression(paste("Biomass (g/m"^2, ")"))),
  fish_biomass   = list(name = "Fish Biomass",
                        label = expression(paste("Biomass (g/m"^2, ")"))),
  krill_biomass  = list(name = "Antarctic Krill Biomass",
                        label = expression(paste("Biomass (g/m"^2, ")"))),
  ltl_biomass    = list(name = "Lower Trophic Level Biomass",
                        label = expression(paste("Biomass (g/m"^2, ")"))),
  apex_biomass   = list(name = "Apex Predator Biomass",
                        label = expression(paste("Biomass (g/m"^2, ")")))
)

STRUCTURAL_METRICS_CONFIG <- list(
  spectrum_slope           = list(name = "Size Spectrum Slope",
                                  label = "Slope"),
  spectrum_intercept       = list(name = "Size Spectrum Intercept",
                                  label = "Intercept"),
  mean_tl                  = list(name = "Mean Trophic Level (Consumers)",
                                  label = "Trophic Level"),
  htl_indicator            = list(name = "High Trophic Level Indicator",
                                  label = "Proportion"),
  large_fish_indicator     = list(name = "Fish LFI (100g)",
                                  label = "Proportion"),
  fish_lfi                 = list(name = "Fish LFI (1000g)",
                                  label = "Proportion"),
  shannon_diversity        = list(name = "Shannon Diversity (H')",
                                  label = "H'"),
  w_statistic              = list(name = "W-Statistic (ABC Curves)",
                                  label = "W"),
  production_biomass_ratio = list(name = "Production:Biomass Ratio (P/B)",
                                  label = expression("P/B (yr"^{-1}*")")),
  mean_weight              = list(name = "Mean Individual Weight",
                                  label = "Weight (g)"),
  mean_max_weight          = list(name = "Mean Max Weight",
                                  label = "Weight (g)"),
  predator_prey_ratio      = list(name = "Predator-Prey Ratio (Apex:Mid-TL)",
                                  label = "Ratio"),
  consumer_ltl_ratio       = list(name = "Consumer:LTL Biomass Ratio",
                                  label = "Ratio")
)

# =============================================================================
# DATA LOADING & SUMMARISATION
# =============================================================================

load_assessment_data <- function() {
  cat("Loading assessment data...\n")
  out <- list(
    fishing  = readRDS(file.path(INPUT_DIR, "fishing_metrics_raw.rds")),
    climate  = readRDS(file.path(INPUT_DIR, "climate_only_metrics_raw.rds")),
    b0       = readRDS(file.path(INPUT_DIR, "b0_reference.rds"))
  )
  # Empirical thresholds for structural B0 envelope breach calculations
  th_file <- file.path(INPUT_DIR, "empirical_thresholds.rds")
  if (file.exists(th_file)) {
    out$empirical_thresholds <- readRDS(th_file)
    cat("  Loaded empirical thresholds for structural breach analysis\n")
  } else {
    out$empirical_thresholds <- NULL
    cat("  Warning: empirical_thresholds.rds not found; structural breach panels skipped\n")
  }
  out
}

summarise_scenario <- function(raw, scenario, metric_col,
                               year_min = YEAR_MIN) {
  raw %>%
    filter(start_year >= year_min) %>%
    group_by(decade, start_year) %>%
    summarise(
      median = median(.data[[metric_col]], na.rm = TRUE),
      q25 = quantile(.data[[metric_col]], 0.25, na.rm = TRUE),
      q75 = quantile(.data[[metric_col]], 0.75, na.rm = TRUE),
      q05 = quantile(.data[[metric_col]], 0.05, na.rm = TRUE),
      q95 = quantile(.data[[metric_col]], 0.95, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(scenario = scenario)
}

compute_ratio_stats <- function(fishing_raw, climate_raw, metric_col,
                                year_min = YEAR_MIN) {
  paired <- fishing_raw %>%
    inner_join(climate_raw,
               by = c("sim_id", "decade", "start_year", "end_year"),
               suffix = c("_fish", "_clim"))
  fish_col <- paste0(metric_col, "_fish")
  clim_col <- paste0(metric_col, "_clim")
  paired$ratio <- ifelse(paired[[clim_col]] > 0,
                         paired[[fish_col]] / paired[[clim_col]], NA)
  paired %>%
    filter(start_year >= year_min) %>%
    group_by(decade, start_year) %>%
    summarise(
      median = median(ratio, na.rm = TRUE),
      q25 = quantile(ratio, 0.25, na.rm = TRUE),
      q75 = quantile(ratio, 0.75, na.rm = TRUE),
      q05 = quantile(ratio, 0.05, na.rm = TRUE),
      q95 = quantile(ratio, 0.95, na.rm = TRUE),
      .groups = "drop"
    )
}

compute_diff_stats <- function(fishing_raw, climate_raw, metric_col,
                               year_min = YEAR_MIN) {
  paired <- fishing_raw %>%
    inner_join(climate_raw,
               by = c("sim_id", "decade", "start_year", "end_year"),
               suffix = c("_fish", "_clim"))
  fish_col <- paste0(metric_col, "_fish")
  clim_col <- paste0(metric_col, "_clim")
  paired$diff <- paired[[fish_col]] - paired[[clim_col]]
  paired %>%
    filter(start_year >= year_min) %>%
    group_by(decade, start_year) %>%
    summarise(
      median = median(diff, na.rm = TRUE),
      q25 = quantile(diff, 0.25, na.rm = TRUE),
      q75 = quantile(diff, 0.75, na.rm = TRUE),
      q05 = quantile(diff, 0.05, na.rm = TRUE),
      q95 = quantile(diff, 0.95, na.rm = TRUE),
      .groups = "drop"
    )
}

get_b0_stats <- function(b0_reference, metric_col) {
  if (!(metric_col %in% names(b0_reference)))
    return(list(med = NA, q25 = NA, q75 = NA))
  b0_vals <- b0_reference[[metric_col]][!is.na(b0_reference[[metric_col]])]
  if (length(b0_vals) == 0)
    return(list(med = NA, q25 = NA, q75 = NA))
  list(
    med = median(b0_vals),
    q25 = quantile(b0_vals, 0.25),
    q75 = quantile(b0_vals, 0.75)
  )
}

get_b0_envelope <- function(metric_col) {
  # Try loading empirical thresholds for structural metric B0 envelope
  th_file <- file.path(INPUT_DIR, "empirical_thresholds.rds")
  if (!file.exists(th_file)) return(list(med = NA, q05 = NA, q95 = NA,
                                          q25 = NA, q75 = NA))
  thresholds <- readRDS(th_file)
  th <- thresholds[[metric_col]]
  if (is.null(th)) return(list(med = NA, q05 = NA, q95 = NA,
                                q25 = NA, q75 = NA))
  list(
    med = th$median %||% NA,
    q05 = th$q05 %||% NA,
    q95 = th$q95 %||% NA,
    q25 = th$q25 %||% NA,
    q75 = th$q75 %||% NA
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# =============================================================================
# BREACH PROBABILITY COMPUTATIONS
# =============================================================================

# Biomass: P(ratio < threshold) per decade for each reference point
compute_biomass_breach_probs <- function(fishing_raw, climate_raw, metric_col,
                                          year_min = YEAR_MIN) {
  paired <- fishing_raw %>%
    inner_join(climate_raw,
               by = c("sim_id", "decade", "start_year", "end_year"),
               suffix = c("_fish", "_clim"))
  fish_col <- paste0(metric_col, "_fish")
  clim_col <- paste0(metric_col, "_clim")
  paired$ratio <- ifelse(paired[[clim_col]] > 0,
                         paired[[fish_col]] / paired[[clim_col]], NA)

  # Define thresholds (whale metrics get IWC RMP)
  thresholds <- c("CCAMLR \u03B3\u2082 (0.75)" = 0.75,
                  "BMSY (0.40)" = 0.40,
                  "CCAMLR \u03B3\u2081 (0.20)" = 0.20)
  if (metric_col %in% WHALE_METRICS) {
    thresholds <- c("CCAMLR \u03B3\u2082 (0.75)" = 0.75,
                    "IWC RMP (0.54)" = 0.54,
                    "BMSY (0.40)" = 0.40,
                    "CCAMLR \u03B3\u2081 (0.20)" = 0.20)
  }

  paired_filt <- paired %>% filter(start_year >= year_min)

  # Compute P(ratio < threshold) for each threshold and decade
  results <- list()
  for (th_name in names(thresholds)) {
    th_val <- thresholds[[th_name]]
    res <- paired_filt %>%
      group_by(decade, start_year) %>%
      summarise(
        prob = mean(ratio < th_val, na.rm = TRUE),
        n_valid = sum(!is.na(ratio)),
        .groups = "drop"
      ) %>%
      mutate(threshold_name = th_name, threshold_value = th_val)
    results[[th_name]] <- res
  }
  bind_rows(results)
}

# Structural: P(value outside B0 envelope) per decade
# Uses empirical B0 5th-95th percentile from pre-exploitation ensemble
compute_structural_breach_probs <- function(fishing_raw, climate_raw,
                                             metric_col, emp_thresholds,
                                             year_min = YEAR_MIN) {
  if (is.null(emp_thresholds)) return(NULL)
  th <- emp_thresholds[[metric_col]]
  if (is.null(th) || is.na(th$q05) || is.na(th$q95)) return(NULL)

  paired <- fishing_raw %>%
    inner_join(climate_raw,
               by = c("sim_id", "decade", "start_year", "end_year"),
               suffix = c("_fish", "_clim"))
  fish_col <- paste0(metric_col, "_fish")
  clim_col <- paste0(metric_col, "_clim")

  paired_filt <- paired %>% filter(start_year >= year_min)

  # Compute breach probabilities for fishing scenario
  breach_data <- paired_filt %>%
    group_by(decade, start_year) %>%
    summarise(
      prob_outside_90  = mean(.data[[fish_col]] < th$q05 |
                              .data[[fish_col]] > th$q95, na.rm = TRUE),
      prob_below_q05   = mean(.data[[fish_col]] < th$q05, na.rm = TRUE),
      prob_above_q95   = mean(.data[[fish_col]] > th$q95, na.rm = TRUE),
      # Climate-only breach for comparison
      prob_clim_outside_90 = mean(.data[[clim_col]] < th$q05 |
                                  .data[[clim_col]] > th$q95, na.rm = TRUE),
      n_valid = sum(!is.na(.data[[fish_col]])),
      .groups = "drop"
    )

  # Pivot to long format for plotting
  breach_data %>%
    tidyr::pivot_longer(
      cols = c(prob_outside_90, prob_below_q05, prob_above_q95,
               prob_clim_outside_90),
      names_to = "breach_type", values_to = "prob"
    ) %>%
    mutate(breach_label = case_when(
      breach_type == "prob_outside_90"      ~ "Fishing: outside B0 5th-95th",
      breach_type == "prob_below_q05"       ~ "Fishing: below B0 5th",
      breach_type == "prob_above_q95"       ~ "Fishing: above B0 95th",
      breach_type == "prob_clim_outside_90" ~ "Climate-only: outside B0 5th-95th",
      TRUE ~ breach_type
    ))
}

# =============================================================================
# EVENT LINE HELPERS
# =============================================================================

get_events_for_metric <- function(metric_col, metric_type = "biomass") {
  events <- list()

  # Add metric-specific events
  if (metric_col %in% names(PEAK_EVENTS)) {
    events <- c(events, PEAK_EVENTS[[metric_col]])
  } else if (metric_type == "structural") {
    events <- c(events, STRUCTURAL_DEFAULT_EVENTS)
  }

  # Filter to post-YEAR_MIN (guard against empty list)
  if (length(events) > 0) {
    keep <- vapply(events, function(e) e$year >= YEAR_MIN, logical(1))
    events <- events[keep]
  }
  return(events)
}

add_event_lines <- function(p, events, y_label_pos = NULL,
                            label_size = 2.3, show_labels = TRUE) {
  for (event in events) {
    p <- p + geom_vline(xintercept = event$year,
                        linetype = event$linetype,
                        color = event$color,
                        alpha = 0.7, linewidth = 0.5)
  }
  # Add small legend text at bottom of plot for event labels
  if (show_labels && length(events) > 0) {
    label_text <- paste(sapply(events, function(e)
      paste0(e$year, ": ", e$label)), collapse = "  |  ")
    p <- p + labs(caption = label_text) +
      theme(plot.caption = element_text(size = 7, color = "grey40",
                                        hjust = 0.5))
  }
  return(p)
}

# =============================================================================
# SILHOUETTE HELPERS
# =============================================================================

load_silhouette <- function(metric_col) {
  sil <- SILHOUETTES[[metric_col]]
  if (is.null(sil) || is.null(sil$path)) return(NULL)
  if (!file.exists(sil$path)) {
    cat(sprintf("    [Silhouette not found: %s]\n", sil$path))
    return(NULL)
  }
  tryCatch({
    img <- readPNG(sil$path)
    list(img = img, x = sil$x, y = sil$y,
         width = sil$width, alpha = sil$alpha)
  }, error = function(e) {
    cat(sprintf("    [Silhouette load error: %s]\n", e$message))
    NULL
  })
}

add_silhouette <- function(p, sil_data) {
  if (is.null(sil_data)) return(p)
  grob <- rasterGrob(
    sil_data$img,
    interpolate = TRUE,
    width = unit(sil_data$width, "npc"),
    height = unit(sil_data$width *
                    nrow(sil_data$img) / ncol(sil_data$img), "npc"),
    gp = gpar(alpha = sil_data$alpha)
  )
  # Use patchwork inset_element for precise normalised positioning
  p + inset_element(
    grob,
    left   = sil_data$x - sil_data$width / 2,
    bottom = sil_data$y - sil_data$width / 2,
    right  = sil_data$x + sil_data$width / 2,
    top    = sil_data$y + sil_data$width / 2,
    align_to = "panel"
  )
}

# =============================================================================
# PANEL BUILDERS — BIOMASS METRICS
# =============================================================================

build_biomass_ensemble <- function(plot_data, b0, events, sil_data = NULL,
                                   metric_name, y_label,
                                   dual_ribbon = TRUE,
                                   include_legend = TRUE,
                                   show_event_labels = FALSE,
                                   free_y = FALSE) {
  p <- ggplot(plot_data, aes(x = start_year, color = scenario,
                             fill = scenario))

  if (dual_ribbon) {
    # 50% + 90% CI
    p <- p +
      geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.12, color = NA) +
      geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.25, color = NA)
  } else {
    # 50% CI only
    p <- p +
      geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.25, color = NA)
  }

  p <- p +
    geom_line(aes(y = median), linewidth = 1) +
    geom_point(aes(y = median), size = 1.8)

  # B0 reference: median line always shown; band only if reasonably narrow
  if (!is.na(b0$med)) {
    # Sanity check: only draw B0 band if IQR is < 2x the data median range
    data_range <- diff(range(plot_data$median, na.rm = TRUE))
    b0_iqr <- abs(b0$q75 - b0$q25)
    if (b0_iqr < 2 * data_range && b0_iqr > 0) {
      p <- p +
        annotate("rect", xmin = -Inf, xmax = Inf,
                 ymin = b0$q25, ymax = b0$q75,
                 fill = "grey50", alpha = 0.15)
    }
    p <- p +
      geom_hline(yintercept = b0$med, linetype = "dashed",
                 color = "grey40", linewidth = 0.6) +
      annotate("text", x = max(plot_data$start_year, na.rm = TRUE),
               y = b0$med, label = " B0", hjust = 0, vjust = -0.5,
               size = 2.8, color = "grey40", fontface = "italic")
  }

  # Event lines
  p <- add_event_lines(p, events, show_labels = show_event_labels)

  p <- p +
    scale_color_manual(values = c("Fishing/Whaling" = "#d62728",
                                  "Climate-only" = "#2ca02c")) +
    scale_fill_manual(values = c("Fishing/Whaling" = "#d62728",
                                 "Climate-only" = "#2ca02c")) +
    labs(title = metric_name, x = NULL, y = y_label,
         color = "Scenario", fill = "Scenario") +
    theme_minimal() +
    theme(plot.title = element_text(size = 13, face = "bold"),
          axis.title.x = element_blank())

  if (include_legend) {
    p <- p + theme(legend.position = "bottom")
  } else {
    p <- p + theme(legend.position = "none")
  }

  # Free y-axis: constrain to visible data range only (exclude B0 band)
  if (free_y) {
    y_vals <- c(plot_data$q25, plot_data$q75, plot_data$median)
    y_range <- range(y_vals, na.rm = TRUE)
    y_pad <- diff(y_range) * 0.08
    p <- p + coord_cartesian(ylim = c(y_range[1] - y_pad, y_range[2] + y_pad))
  }

  # Silhouette overlay
  if (!is.null(sil_data)) {
    p <- add_silhouette(p, sil_data)
  }

  return(p)
}

build_biomass_ratio <- function(ratio_stats, events, metric_col,
                                include_title = FALSE, metric_name = NULL,
                                show_event_labels = TRUE) {
  p <- ggplot(ratio_stats, aes(x = start_year)) +
    # Reference thresholds
    geom_hline(yintercept = 1.0,  linetype = "solid",  color = "grey40") +
    geom_hline(yintercept = 0.75, linetype = "dashed",  color = "#e6550d",
               linewidth = 0.5) +
    geom_hline(yintercept = 0.40, linetype = "dashed",  color = "#e31a1c",
               linewidth = 0.5) +
    geom_hline(yintercept = 0.20, linetype = "dashed",  color = "#67000d",
               linewidth = 0.5)

  # IWC RMP line for whale metrics

  if (metric_col %in% WHALE_METRICS) {
    p <- p +
      geom_hline(yintercept = 0.54, linetype = "longdash",
                 color = "#6a3d9a", linewidth = 0.5) +
      annotate("text", x = YEAR_MIN, y = 0.56,
               label = "IWC RMP 0.54", hjust = 0, size = 2.5,
               color = "#6a3d9a")
  }

  p <- p +
    # Ensemble ribbons
    geom_ribbon(aes(ymin = q05, ymax = q95), fill = "#7570b3", alpha = 0.15) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#7570b3", alpha = 0.3) +
    geom_line(aes(y = median), color = "#7570b3", linewidth = 1) +
    geom_point(aes(y = median), color = "#7570b3", size = 1.8) +
    # Threshold labels
    annotate("text", x = YEAR_MIN, y = 0.77,
             label = "CCAMLR g2 0.75", hjust = 0, size = 2.5,
             color = "#e6550d") +
    annotate("text", x = YEAR_MIN, y = 0.42,
             label = "BMSY 0.40", hjust = 0, size = 2.5,
             color = "#e31a1c") +
    annotate("text", x = YEAR_MIN, y = 0.22,
             label = "CCAMLR g1 0.20", hjust = 0, size = 2.5,
             color = "#67000d")

  # Event lines
  p <- add_event_lines(p, events, show_labels = show_event_labels)

  p <- p +
    labs(x = "Year", y = "Fishing / Climate-only ratio") +
    theme_minimal() +
    theme(axis.title = element_text(size = 10))

  if (include_title && !is.null(metric_name)) {
    p <- p +
      labs(title = paste0(metric_name, " \u2014 Exploitation Ratio")) +
      theme(plot.title = element_text(size = 13, face = "bold"))
  }

  return(p)
}

# =============================================================================
# PANEL BUILDERS — STRUCTURAL METRICS
# =============================================================================

build_structural_ensemble <- function(plot_data, b0_env, events,
                                      metric_name, y_label,
                                      dual_ribbon = TRUE,
                                      include_legend = TRUE,
                                      show_event_labels = FALSE,
                                      free_y = FALSE) {
  p <- ggplot(plot_data, aes(x = start_year, color = scenario,
                             fill = scenario))

  if (dual_ribbon) {
    p <- p +
      geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.12, color = NA) +
      geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.25, color = NA)
  } else {
    p <- p +
      geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.25, color = NA)
  }

  p <- p +
    geom_line(aes(y = median), linewidth = 1) +
    geom_point(aes(y = median), size = 1.8)

  # B0 envelope: median line always shown; bands only if reasonably narrow
  if (!is.na(b0_env$q05)) {
    data_range <- diff(range(plot_data$median, na.rm = TRUE))
    b0_90_range <- abs(b0_env$q95 - b0_env$q05)
    b0_50_range <- abs(b0_env$q75 - b0_env$q25)
    if (b0_90_range < 2 * data_range && b0_90_range > 0) {
      p <- p +
        annotate("rect", xmin = -Inf, xmax = Inf,
                 ymin = b0_env$q05, ymax = b0_env$q95,
                 fill = "grey50", alpha = 0.12)
    }
    if (b0_50_range < 2 * data_range && b0_50_range > 0) {
      p <- p +
        annotate("rect", xmin = -Inf, xmax = Inf,
                 ymin = b0_env$q25, ymax = b0_env$q75,
                 fill = "grey50", alpha = 0.15)
    }
    p <- p +
      geom_hline(yintercept = b0_env$med, linetype = "dashed",
                 color = "grey40", linewidth = 0.6) +
      annotate("text", x = max(plot_data$start_year, na.rm = TRUE),
               y = b0_env$med, label = " B0", hjust = 0, vjust = -0.5,
               size = 2.8, color = "grey40", fontface = "italic")
  }

  # Event lines
  p <- add_event_lines(p, events, show_labels = show_event_labels)

  p <- p +
    scale_color_manual(values = c("Fishing/Whaling" = "#d62728",
                                  "Climate-only" = "#2ca02c")) +
    scale_fill_manual(values = c("Fishing/Whaling" = "#d62728",
                                 "Climate-only" = "#2ca02c")) +
    labs(title = paste0(metric_name, " (B0 envelope: grey bands)"),
         x = NULL, y = y_label,
         color = "Scenario", fill = "Scenario") +
    theme_minimal() +
    theme(plot.title = element_text(size = 13, face = "bold"),
          axis.title.x = element_blank())

  if (include_legend) {
    p <- p + theme(legend.position = "bottom")
  } else {
    p <- p + theme(legend.position = "none")
  }

  # Free y-axis: constrain to visible data range only (exclude B0 band)
  if (free_y) {
    y_vals <- c(plot_data$q25, plot_data$q75, plot_data$median)
    y_range <- range(y_vals, na.rm = TRUE)
    y_pad <- diff(y_range) * 0.08
    p <- p + coord_cartesian(ylim = c(y_range[1] - y_pad, y_range[2] + y_pad))
  }

  return(p)
}

build_structural_diff <- function(diff_stats, events,
                                  include_title = FALSE,
                                  metric_name = NULL,
                                  show_event_labels = TRUE) {
  p <- ggplot(diff_stats, aes(x = start_year)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "grey40") +
    geom_ribbon(aes(ymin = q05, ymax = q95), fill = "#7570b3", alpha = 0.15) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#7570b3", alpha = 0.3) +
    geom_line(aes(y = median), color = "#7570b3", linewidth = 1) +
    geom_point(aes(y = median), color = "#7570b3", size = 1.8)

  # Event lines
  p <- add_event_lines(p, events, show_labels = show_event_labels)

  p <- p +
    labs(x = "Year", y = "\u0394 (Fishing \u2212 Climate-only)") +
    theme_minimal() +
    theme(axis.title = element_text(size = 10))

  if (include_title && !is.null(metric_name)) {
    p <- p +
      labs(title = paste0(metric_name, " \u2014 Exploitation Effect")) +
      theme(plot.title = element_text(size = 13, face = "bold"))
  }

  return(p)
}

# =============================================================================
# PANEL BUILDERS — BREACH PROBABILITY
# =============================================================================

# Biomass: P(ratio < threshold) timeseries
# Colours matched to ratio panel threshold colours for visual continuity
build_biomass_breach_panel <- function(breach_probs, events, metric_col,
                                       include_title = FALSE,
                                       metric_name = NULL,
                                       show_event_labels = TRUE) {
  threshold_colours <- c(
    "CCAMLR \u03B3\u2082 (0.75)" = "#e6550d",
    "IWC RMP (0.54)"              = "#6a3d9a",
    "BMSY (0.40)"                 = "#e31a1c",
    "CCAMLR \u03B3\u2081 (0.20)" = "#67000d"
  )
  # Filter to thresholds present in this metric
  present_th <- unique(breach_probs$threshold_name)
  threshold_colours <- threshold_colours[names(threshold_colours) %in% present_th]

  p <- ggplot(breach_probs, aes(x = start_year, y = prob,
                                 color = threshold_name)) +
    geom_hline(yintercept = 0.5, linetype = "dotted", color = "grey60",
               linewidth = 0.4) +
    annotate("text", x = -Inf, y = 0.52, label = "50% of ensemble",
             hjust = -0.05, size = 2.3, color = "grey50") +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8)

  # Event lines
  p <- add_event_lines(p, events, show_labels = show_event_labels)

  p <- p +
    scale_color_manual(values = threshold_colours, name = "Threshold") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(),
                       breaks = seq(0, 1, 0.25)) +
    labs(x = "Year", y = "P(ratio < threshold)") +
    theme_minimal() +
    theme(axis.title = element_text(size = 10),
          legend.position = "bottom",
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 9))

  if (include_title && !is.null(metric_name)) {
    p <- p +
      labs(title = paste0(metric_name, " \u2014 Probability of Breach")) +
      theme(plot.title = element_text(size = 13, face = "bold"))
  }

  return(p)
}

# Structural: P(outside B0 envelope) timeseries
# Shows fishing (total outside + directional) and climate-only for comparison
build_structural_breach_panel <- function(breach_probs, events,
                                           include_title = FALSE,
                                           metric_name = NULL,
                                           show_event_labels = TRUE) {
  breach_colours <- c(
    "Fishing: outside B0 5th-95th"      = "#d62728",
    "Fishing: below B0 5th"             = "#ff7f0e",
    "Fishing: above B0 95th"            = "#2ca02c",
    "Climate-only: outside B0 5th-95th" = "#7f7f7f"
  )
  present_labels <- unique(breach_probs$breach_label)
  breach_colours <- breach_colours[names(breach_colours) %in% present_labels]

  p <- ggplot(breach_probs, aes(x = start_year, y = prob,
                                 color = breach_label)) +
    geom_hline(yintercept = 0.05, linetype = "dotted", color = "grey60",
               linewidth = 0.4) +
    annotate("text", x = -Inf, y = 0.07, label = "Expected 5%",
             hjust = -0.05, size = 2.3, color = "grey50") +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8)

  # Event lines
  p <- add_event_lines(p, events, show_labels = show_event_labels)

  p <- p +
    scale_color_manual(values = breach_colours, name = "Breach type") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(),
                       breaks = seq(0, 1, 0.25)) +
    labs(x = "Year", y = "P(outside B0 envelope)") +
    theme_minimal() +
    theme(axis.title = element_text(size = 10),
          legend.position = "bottom",
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 9))

  if (include_title && !is.null(metric_name)) {
    p <- p +
      labs(title = paste0(metric_name, " \u2014 B0 Envelope Breach")) +
      theme(plot.title = element_text(size = 13, face = "bold"))
  }

  return(p)
}

# =============================================================================
# SAVE HELPER
# =============================================================================

save_variant <- function(plot_obj, base_name, variant_suffix,
                         width, height) {
  png_path <- file.path(OUTPUT_DIR, paste0(base_name, "_", variant_suffix, ".png"))
  ggsave(png_path, plot_obj, width = width, height = height, dpi = 300)
}

# =============================================================================
# GENERATE VARIANTS — BIOMASS METRICS
# =============================================================================

generate_biomass_variants <- function(data, metric_col, config) {
  cat(sprintf("  %s\n", config$name))

  # Check metric exists
  if (!(metric_col %in% names(data$fishing))) {
    cat(sprintf("    Warning: %s not found, skipping\n", metric_col))
    return(NULL)
  }

  # Prepare data
  plot_data <- bind_rows(
    summarise_scenario(data$fishing, "Fishing/Whaling", metric_col),
    summarise_scenario(data$climate, "Climate-only", metric_col)
  )
  ratio_stats <- compute_ratio_stats(data$fishing, data$climate, metric_col)
  b0 <- get_b0_stats(data$b0, metric_col)
  events <- get_events_for_metric(metric_col, "biomass")
  sil_data <- load_silhouette(metric_col)

  # Variant 1: Combined (ensemble + ratio)
  p_ens <- build_biomass_ensemble(
    plot_data, b0, events, sil_data,
    config$name, config$label,
    dual_ribbon = TRUE, include_legend = TRUE,
    show_event_labels = FALSE
  )
  p_rat <- build_biomass_ratio(
    ratio_stats, events, metric_col,
    include_title = FALSE, show_event_labels = TRUE
  )
  v1 <- p_ens / p_rat + plot_layout(heights = c(2, 1))
  save_variant(v1, metric_col, "combined", 11, 8)

  # Variant 2: Ensemble only
  v2 <- build_biomass_ensemble(
    plot_data, b0, events, sil_data,
    config$name, config$label,
    dual_ribbon = TRUE, include_legend = TRUE,
    show_event_labels = TRUE
  ) + labs(x = "Year") + theme(axis.title.x = element_text(size = 10))
  save_variant(v2, metric_col, "ensemble_only", 11, 5.5)

  # Variant 3: Ratio only
  v3 <- build_biomass_ratio(
    ratio_stats, events, metric_col,
    include_title = TRUE, metric_name = config$name,
    show_event_labels = TRUE
  )
  save_variant(v3, metric_col, "ratio_only", 11, 5)

  # Variant 4: Ensemble, 50% CI ribbon only (free y-axis)
  v4 <- build_biomass_ensemble(
    plot_data, b0, events, sil_data,
    config$name, config$label,
    dual_ribbon = FALSE, include_legend = TRUE,
    show_event_labels = TRUE, free_y = TRUE
  ) + labs(x = "Year") + theme(axis.title.x = element_text(size = 10))
  save_variant(v4, metric_col, "ensemble_50ci", 11, 5.5)

  # Variant 5: Three-panel (ensemble + ratio + breach probability)
  breach_probs <- compute_biomass_breach_probs(
    data$fishing, data$climate, metric_col
  )
  p_ens5 <- build_biomass_ensemble(
    plot_data, b0, events, sil_data,
    config$name, config$label,
    dual_ribbon = TRUE, include_legend = TRUE,
    show_event_labels = FALSE
  )
  p_rat5 <- build_biomass_ratio(
    ratio_stats, events, metric_col,
    include_title = FALSE, show_event_labels = FALSE
  )
  p_breach5 <- build_biomass_breach_panel(
    breach_probs, events, metric_col,
    include_title = FALSE, show_event_labels = TRUE
  )
  v5 <- p_ens5 / p_rat5 / p_breach5 + plot_layout(heights = c(2, 1, 1))
  save_variant(v5, metric_col, "three_panel", 11, 10.5)

  invisible(list(combined = v1, ensemble = v2, ratio = v3,
                 single_ribbon = v4, three_panel = v5))
}

# =============================================================================
# GENERATE VARIANTS — STRUCTURAL METRICS
# =============================================================================

generate_structural_variants <- function(data, metric_col, config) {
  cat(sprintf("  %s\n", config$name))

  if (!(metric_col %in% names(data$fishing))) {
    cat(sprintf("    Warning: %s not found, skipping\n", metric_col))
    return(NULL)
  }

  # Prepare data
  plot_data <- bind_rows(
    summarise_scenario(data$fishing, "Fishing/Whaling", metric_col),
    summarise_scenario(data$climate, "Climate-only", metric_col)
  )
  diff_stats <- compute_diff_stats(data$fishing, data$climate, metric_col)
  b0_env <- get_b0_envelope(metric_col)
  events <- get_events_for_metric(metric_col, "structural")

  # Variant 1: Combined (ensemble + difference)
  p_ens <- build_structural_ensemble(
    plot_data, b0_env, events,
    config$name, config$label,
    dual_ribbon = TRUE, include_legend = TRUE,
    show_event_labels = FALSE
  )
  p_diff <- build_structural_diff(
    diff_stats, events,
    include_title = FALSE, show_event_labels = TRUE
  )
  v1 <- p_ens / p_diff + plot_layout(heights = c(2, 1))
  save_variant(v1, metric_col, "combined", 11, 8)

  # Variant 2: Ensemble only
  v2 <- build_structural_ensemble(
    plot_data, b0_env, events,
    config$name, config$label,
    dual_ribbon = TRUE, include_legend = TRUE,
    show_event_labels = TRUE
  ) + labs(x = "Year") + theme(axis.title.x = element_text(size = 10))
  save_variant(v2, metric_col, "ensemble_only", 11, 5.5)

  # Variant 3: Difference only
  v3 <- build_structural_diff(
    diff_stats, events,
    include_title = TRUE, metric_name = config$name,
    show_event_labels = TRUE
  )
  save_variant(v3, metric_col, "diff_only", 11, 5)

  # Variant 4: Ensemble, 50% CI ribbon only (free y-axis)
  v4 <- build_structural_ensemble(
    plot_data, b0_env, events,
    config$name, config$label,
    dual_ribbon = FALSE, include_legend = TRUE,
    show_event_labels = TRUE, free_y = TRUE
  ) + labs(x = "Year") + theme(axis.title.x = element_text(size = 10))
  save_variant(v4, metric_col, "ensemble_50ci", 11, 5.5)

  # Variant 5: Three-panel (ensemble + difference + breach probability)
  breach_probs <- compute_structural_breach_probs(
    data$fishing, data$climate, metric_col, data$empirical_thresholds
  )
  if (!is.null(breach_probs) && nrow(breach_probs) > 0) {
    p_ens5 <- build_structural_ensemble(
      plot_data, b0_env, events,
      config$name, config$label,
      dual_ribbon = TRUE, include_legend = TRUE,
      show_event_labels = FALSE
    )
    p_diff5 <- build_structural_diff(
      diff_stats, events,
      include_title = FALSE, show_event_labels = FALSE
    )
    p_breach5 <- build_structural_breach_panel(
      breach_probs, events,
      include_title = FALSE, show_event_labels = TRUE
    )
    v5 <- p_ens5 / p_diff5 / p_breach5 + plot_layout(heights = c(2, 1, 1))
    save_variant(v5, metric_col, "three_panel", 11, 10.5)
  } else {
    cat(sprintf("    [Skipping three-panel: no empirical thresholds for %s]\n",
                metric_col))
    v5 <- NULL
  }

  invisible(list(combined = v1, ensemble = v2, diff = v3,
                 single_ribbon = v4, three_panel = v5))
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

generate_all_figure_variants <- function() {
  cat("=============================================================\n")
  cat("GENERATING ALL FIGURE VARIANTS\n")
  cat(sprintf("Time range: %d onward\n", YEAR_MIN))
  cat(sprintf("Output: %s\n", OUTPUT_DIR))
  cat("=============================================================\n\n")

  data <- load_assessment_data()

  # --- Biomass metrics (Category A) ---
  cat("Category A — Biomass metrics (5 variants each):\n")
  for (metric_col in names(BIOMASS_METRICS_CONFIG)) {
    generate_biomass_variants(data, metric_col,
                              BIOMASS_METRICS_CONFIG[[metric_col]])
  }

  # --- Structural metrics (Category B) ---
  cat("\nCategory B — Structural metrics (5 variants each):\n")
  for (metric_col in names(STRUCTURAL_METRICS_CONFIG)) {
    generate_structural_variants(data, metric_col,
                                 STRUCTURAL_METRICS_CONFIG[[metric_col]])
  }

  # Summary
  n_biomass <- length(BIOMASS_METRICS_CONFIG)
  n_struct  <- length(STRUCTURAL_METRICS_CONFIG)
  n_total   <- (n_biomass + n_struct) * 5
  cat("\n=============================================================\n")
  cat("COMPLETE\n")
  cat(sprintf("  Biomass metrics:    %d x 5 variants = %d figures\n",
              n_biomass, n_biomass * 5))
  cat(sprintf("  Structural metrics: %d x 5 variants = %d figures\n",
              n_struct, n_struct * 5))
  cat(sprintf("  Total:              up to %d figures\n", n_total))
  cat(sprintf("  Output directory:   %s/\n", OUTPUT_DIR))
  cat("\nVariant naming:\n")
  cat("  *_combined       — ensemble + ratio/difference panel\n")
  cat("  *_ensemble_only  — ensemble panel standalone\n")
  cat("  *_ratio_only     — ratio panel standalone (biomass)\n")
  cat("  *_diff_only      — difference panel standalone (structural)\n")
  cat("  *_ensemble_50ci  — ensemble with 50%% CI ribbon only\n")
  cat("  *_three_panel    — ensemble + ratio/diff + breach probability\n")
  cat("\nSilhouette status:\n")
  for (m in names(SILHOUETTES)) {
    sil <- SILHOUETTES[[m]]
    status <- if (is.null(sil$path)) "PATH NOT SET" else {
      if (file.exists(sil$path)) "OK" else "FILE NOT FOUND"
    }
    cat(sprintf("  %-20s: %s\n", m, status))
  }
  cat("=============================================================\n")
}

# Run
generate_all_figure_variants()
