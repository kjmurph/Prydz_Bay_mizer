###############################################################################
# Whale Consumption Plotting Script
# 
# Creates plots from pre-extracted whale consumption data without re-running
# the full diet extraction. Also extracts and compares with climate-only 
# (unfished) ensemble for relative comparison.
#
# Requires: whale_consumption_outputs/ directory with RDS files from
#           extract_whale_consumption.R
#
# Author: Generated for Prydz Bay mizer project
# Date: 2025
###############################################################################

library(therMizer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

###############################################################################
# Configuration
###############################################################################

OUTPUT_DIR <- "whale_consumption_outputs"
G_TO_TONNES <- 1e-6
MODEL_DOMAIN_AREA <- 1.474341e+12  # m^2 (therMizer calibration domain, 05_therMizer_calibration_scale_model_domain.Rmd)

# Analysis periods
ANALYSIS_PERIODS <- list(
  pre_whaling = 1841:1880,
  early_whaling = 1900:1920,
  peak_whaling = 1930:1965,
  post_whaling = 1970:1976,
  krill_fishing = 1977:1995,
  modern = 2001:2010
)

# Species
WHALE_SPECIES <- list(
  baleen = c("baleen whales", "minke whales"),
  toothed = c("sperm whales", "orca"),
  all_whales = c("baleen whales", "minke whales", "sperm whales", "orca")
)

LTL_PREY <- c("antarctic krill", "other krill", "mesozooplankton", 
              "other macrozooplankton", "salps")

###############################################################################
# Load existing data
###############################################################################

cat("=============================================================\n")
cat("Loading pre-extracted whale consumption data...\n")
cat("=============================================================\n\n")

# Load krill consumption by baleen whales (from fishing scenario)
krill_baleen_sims <- readRDS(file.path(OUTPUT_DIR, "krill_consumption_baleen_all_sims.rds"))
krill_whale_sims <- readRDS(file.path(OUTPUT_DIR, "krill_consumption_all_whales_all_sims.rds"))

cat(sprintf("Loaded krill consumption data for %d simulations\n", length(krill_baleen_sims)))

###############################################################################
# Calculate ensemble statistics function
###############################################################################

calc_ensemble_stats <- function(df_list, value_col = "total_consumption") {
  if (length(df_list) == 0) return(NULL)
  
  for (i in seq_along(df_list)) {
    df_list[[i]]$sim_id <- i
  }
  combined <- do.call(rbind, df_list)
  
  stats <- combined %>%
    group_by(year) %>%
    summarise(
      n_sims = n(),
      median = median(!!sym(value_col), na.rm = TRUE),
      mean = mean(!!sym(value_col), na.rm = TRUE),
      q05 = quantile(!!sym(value_col), 0.05, na.rm = TRUE),
      q25 = quantile(!!sym(value_col), 0.25, na.rm = TRUE),
      q75 = quantile(!!sym(value_col), 0.75, na.rm = TRUE),
      q95 = quantile(!!sym(value_col), 0.95, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(year)
  
  return(stats)
}

# Calculate fishing scenario stats
krill_baleen_stats <- calc_ensemble_stats(krill_baleen_sims)
krill_whale_stats <- calc_ensemble_stats(krill_whale_sims)

###############################################################################
# Check for and extract climate-only consumption if needed
###############################################################################

cat("\n=============================================================\n")
cat("Checking for climate-only whale consumption data...\n")
cat("=============================================================\n\n")

climate_krill_file <- file.path(OUTPUT_DIR, "climate_only_krill_consumption_baleen.rds")

if (file.exists(climate_krill_file)) {
  cat("Loading existing climate-only krill consumption data...\n")
  climate_krill_baleen_sims <- readRDS(climate_krill_file)
  cat(sprintf("Loaded %d climate-only simulations\n", length(climate_krill_baleen_sims)))
} else {
  cat("Climate-only consumption not yet extracted. Extracting now...\n")
  cat("(This will only need to be done once)\n\n")
  
  # Load climate-only ensemble
  climate_file <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
  
  if (!file.exists(climate_file)) {
    cat("WARNING: Climate-only ensemble file not found!\n")
    climate_krill_baleen_sims <- NULL
  } else {
    climate_mc <- readRDS(climate_file)
    
    # Handle structure
    if ("simulations" %in% names(climate_mc)) {
      climate_sims <- climate_mc$simulations
    } else if (is.list(climate_mc) && inherits(climate_mc[[1]], "MizerSim")) {
      climate_sims <- climate_mc
    } else {
      stop("Unrecognized climate-only structure")
    }
    
    n_climate <- length(climate_sims)
    cat(sprintf("Processing %d climate-only simulations...\n", n_climate))
    
    # Get species
    first_sim <- climate_sims[[1]]
    all_species <- dimnames(first_sim@n)$sp
    baleen_species <- WHALE_SPECIES$baleen[WHALE_SPECIES$baleen %in% all_species]
    
    # Extract consumption function (simplified for krill only)
    extract_krill_timeseries <- function(sim, predator_species) {
      params <- sim@params
      times <- as.numeric(dimnames(sim@n)$time)
      dw <- params@dw
      
      results <- list()
      
      for (t_idx in seq_along(times)) {
        t <- times[t_idx]
        
        n <- sim@n[t_idx, , , drop = FALSE]
        dim(n) <- dim(sim@n)[2:3]
        dimnames(n) <- dimnames(sim@n)[2:3]
        
        n_pp <- sim@n_pp[t_idx, ]
        n_other <- sim@n_other[t_idx, ]
        if (!is.null(dimnames(sim@n_other))) {
          names(n_other) <- dimnames(sim@n_other)$component
        }
        
        diet <- tryCatch({
          getDiet(params, n = n, n_pp = n_pp, n_other = n_other, proportion = FALSE)
        }, error = function(e) NULL)
        
        if (is.null(diet)) next
        
        total_krill <- 0
        for (pred in predator_species) {
          pred_idx <- which(dimnames(diet)$predator == pred)
          if (length(pred_idx) == 0) next
          
          pred_diet <- diet[pred_idx, , , drop = FALSE]
          dim(pred_diet) <- dim(diet)[2:3]
          dimnames(pred_diet) <- dimnames(diet)[2:3]
          
          n_pred <- n[pred, ]
          
          krill_idx <- which(colnames(pred_diet) == "antarctic krill")
          if (length(krill_idx) > 0) {
            total_krill <- total_krill + sum(pred_diet[, krill_idx] * n_pred * dw)
          }
        }
        
        results[[length(results) + 1]] <- data.frame(
          year = t,
          total_consumption = total_krill,
          consumption_antarctic_krill = total_krill
        )
      }
      
      if (length(results) == 0) return(NULL)
      return(do.call(rbind, results))
    }
    
    # Process climate-only simulations
    climate_krill_baleen_sims <- list()
    pb <- txtProgressBar(min = 0, max = n_climate, style = 3)
    
    for (i in seq_len(n_climate)) {
      setTxtProgressBar(pb, i)
      
      sim <- tryCatch(climate_sims[[i]], error = function(e) NULL)
      if (is.null(sim)) next
      
      ts <- tryCatch({
        extract_krill_timeseries(sim, baleen_species)
      }, error = function(e) NULL)
      
      if (!is.null(ts)) {
        climate_krill_baleen_sims[[length(climate_krill_baleen_sims) + 1]] <- ts
      }
    }
    close(pb)
    
    cat(sprintf("\n  Extracted %d climate-only simulations\n", length(climate_krill_baleen_sims)))
    
    # Save for future use
    saveRDS(climate_krill_baleen_sims, climate_krill_file)
    cat(sprintf("  Saved to: %s\n", climate_krill_file))
  }
}

# Calculate climate-only stats
if (!is.null(climate_krill_baleen_sims) && length(climate_krill_baleen_sims) > 0) {
  climate_krill_baleen_stats <- calc_ensemble_stats(climate_krill_baleen_sims)
  cat(sprintf("Climate-only stats calculated: %d years\n", nrow(climate_krill_baleen_stats)))
} else {
  climate_krill_baleen_stats <- NULL
}

###############################################################################
# Generate plots
###############################################################################

cat("\n=============================================================\n")
cat("Generating plots (in tonnes/year)...\n")
cat("=============================================================\n\n")

# Plot 1: Krill consumption time series - Baleen whales (in tonnes/year)
cat("Creating Plot 1: Krill consumption time series...\n")

plot_data <- krill_baleen_stats %>%
  mutate(
    median_t = median * G_TO_TONNES,
    mean_t = mean * G_TO_TONNES,
    q05_t = q05 * G_TO_TONNES,
    q25_t = q25 * G_TO_TONNES,
    q75_t = q75 * G_TO_TONNES,
    q95_t = q95 * G_TO_TONNES
  )

p1 <- ggplot(plot_data, aes(x = year)) +
  geom_ribbon(aes(ymin = q05_t, ymax = q95_t), alpha = 0.2, fill = "steelblue") +
  geom_ribbon(aes(ymin = q25_t, ymax = q75_t), alpha = 0.4, fill = "steelblue") +
  geom_line(aes(y = median_t, linetype = "Median"), color = "steelblue", linewidth = 1) +
  geom_line(aes(y = mean_t, linetype = "Mean"), color = "darkblue", linewidth = 0.8) +
  geom_vline(xintercept = c(1930, 1965), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = 1977, linetype = "dashed", color = "orange", alpha = 0.5) +
  annotate("text", x = 1947, y = max(plot_data$q95_t) * 0.95, 
           label = "Peak Whaling", size = 3, color = "red") +
  annotate("text", x = 1990, y = max(plot_data$q95_t) * 0.95,
           label = "Krill Fishing", size = 3, color = "orange") +
  scale_y_continuous(labels = comma) +
  scale_linetype_manual(values = c("Median" = "solid", "Mean" = "dashed"),
                        name = "Statistic") +
  labs(
    title = "Antarctic Krill Consumption by Baleen Whales",
    subtitle = "Prydz Bay region, Monte Carlo ensemble (n = 2111)",
    x = "Year",
    y = "Krill consumption (tonnes/year)",
    caption = "Solid line: median, Dashed line: mean. Shaded areas: 50% and 90% credible intervals"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 11),
    legend.position = "bottom"
  )

ggsave(file.path(OUTPUT_DIR, "krill_consumption_baleen_timeseries.png"),
       p1, width = 10, height = 6, dpi = 300)
ggsave(file.path(OUTPUT_DIR, "krill_consumption_baleen_timeseries.pdf"),
       p1, width = 10, height = 6)
cat("  Saved: krill_consumption_baleen_timeseries plots\n")

# Plot 2: Fishing vs Climate-Only comparison
if (!is.null(climate_krill_baleen_stats)) {
  cat("Creating Plot 2: Fishing vs Climate-Only comparison...\n")
  
  # Prepare data
  fishing_data <- krill_baleen_stats %>%
    mutate(
      median_t = median * G_TO_TONNES,
      q05_t = q05 * G_TO_TONNES,
      q95_t = q95 * G_TO_TONNES,
      scenario = "With Fishing"
    ) %>%
    select(year, median_t, q05_t, q95_t, scenario)
  
  climate_data <- climate_krill_baleen_stats %>%
    mutate(
      median_t = median * G_TO_TONNES,
      q05_t = q05 * G_TO_TONNES,
      q95_t = q95 * G_TO_TONNES,
      scenario = "Climate-Only (Unfished)"
    ) %>%
    select(year, median_t, q05_t, q95_t, scenario)
  
  combined_data <- rbind(fishing_data, climate_data)
  combined_data$scenario <- factor(combined_data$scenario, 
                                    levels = c("Climate-Only (Unfished)", "With Fishing"))
  
  p2 <- ggplot(combined_data, aes(x = year, color = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin = q05_t, ymax = q95_t), alpha = 0.15, color = NA) +
    geom_line(aes(y = median_t), linewidth = 1) +
    geom_vline(xintercept = c(1930, 1965), linetype = "dashed", color = "gray40", alpha = 0.5) +
    geom_vline(xintercept = 1977, linetype = "dashed", color = "gray40", alpha = 0.5) +
    scale_color_manual(values = c("Climate-Only (Unfished)" = "forestgreen", 
                                   "With Fishing" = "steelblue")) +
    scale_fill_manual(values = c("Climate-Only (Unfished)" = "forestgreen", 
                                  "With Fishing" = "steelblue")) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Baleen Whale Krill Consumption: Fishing vs Climate-Only Scenarios",
      subtitle = "Prydz Bay region, Monte Carlo ensembles",
      x = "Year",
      y = "Krill consumption (tonnes/year)",
      color = "Scenario",
      fill = "Scenario",
      caption = "Shaded areas: 90% credible intervals. Dashed lines: Peak whaling (1930-1965), Krill fishing (1977+)"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 11),
      legend.position = "bottom"
    )
  
  ggsave(file.path(OUTPUT_DIR, "krill_consumption_fishing_vs_climate_only.png"),
         p2, width = 10, height = 6, dpi = 300)
  ggsave(file.path(OUTPUT_DIR, "krill_consumption_fishing_vs_climate_only.pdf"),
         p2, width = 10, height = 6)
  cat("  Saved: krill_consumption_fishing_vs_climate_only plots\n")
  
  # Plot 3: Relative to climate-only (ratio plot)
  cat("Creating Plot 3: Fishing relative to climate-only...\n")
  
  # Join and calculate ratio
  ratio_data <- fishing_data %>%
    select(year, fishing_median = median_t) %>%
    left_join(
      climate_data %>% select(year, climate_median = median_t),
      by = "year"
    ) %>%
    mutate(
      ratio = fishing_median / climate_median * 100,
      difference = fishing_median - climate_median
    )
  
  p3 <- ggplot(ratio_data, aes(x = year)) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "darkred", linewidth = 0.8) +
    geom_line(aes(y = ratio), color = "steelblue", linewidth = 1) +
    geom_vline(xintercept = c(1930, 1965), linetype = "dashed", color = "gray40", alpha = 0.5) +
    geom_vline(xintercept = 1977, linetype = "dashed", color = "gray40", alpha = 0.5) +
    annotate("text", x = 1860, y = 102, label = "Climate-only baseline", 
             size = 3, color = "darkred", hjust = 0) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "Baleen Whale Krill Consumption: Fishing Impact",
      subtitle = "Consumption with fishing as percentage of climate-only (unfished) scenario",
      x = "Year",
      y = "Percent of climate-only consumption",
      caption = "Values below 100% indicate reduced consumption due to fishing impacts on whale populations"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 11)
    )
  
  ggsave(file.path(OUTPUT_DIR, "krill_consumption_ratio_to_climate_only.png"),
         p3, width = 10, height = 6, dpi = 300)
  cat("  Saved: krill_consumption_ratio_to_climate_only.png\n")
  
  # Save ratio data
  write.csv(ratio_data, 
            file.path(OUTPUT_DIR, "krill_consumption_fishing_vs_climate_ratio.csv"),
            row.names = FALSE)
  cat("  Saved: krill_consumption_fishing_vs_climate_ratio.csv\n")
}

# Plot 4: Period comparison bar plot (in tonnes/year)
cat("Creating Plot 4: Period comparison...\n")

calc_period_stats <- function(stats_df, periods) {
  if (is.null(stats_df)) return(NULL)
  
  period_stats <- list()
  
  for (period_name in names(periods)) {
    period_years <- periods[[period_name]]
    period_data <- stats_df[stats_df$year %in% period_years, ]
    
    if (nrow(period_data) == 0) next
    
    period_stats[[period_name]] <- data.frame(
      period = period_name,
      start_year = min(period_years),
      end_year = max(period_years),
      n_years = nrow(period_data),
      mean_median = mean(period_data$median, na.rm = TRUE),
      mean_mean = mean(period_data$mean, na.rm = TRUE),
      min_q05 = min(period_data$q05, na.rm = TRUE),
      max_q95 = max(period_data$q95, na.rm = TRUE)
    )
  }
  
  return(do.call(rbind, period_stats))
}

krill_baleen_periods <- calc_period_stats(krill_baleen_stats, ANALYSIS_PERIODS)

if (!is.null(krill_baleen_periods)) {
  # Add relative change from pre-whaling (for both median and mean)
  baseline_median <- krill_baleen_periods$mean_median[krill_baleen_periods$period == "pre_whaling"]
  baseline_mean <- krill_baleen_periods$mean_mean[krill_baleen_periods$period == "pre_whaling"]
  
  if (length(baseline_median) > 0 && !is.na(baseline_median)) {
    krill_baleen_periods$median_pct_of_baseline <- krill_baleen_periods$mean_median / baseline_median * 100
  }
  if (length(baseline_mean) > 0 && !is.na(baseline_mean)) {
    krill_baleen_periods$mean_pct_of_baseline <- krill_baleen_periods$mean_mean / baseline_mean * 100
  }
  
  krill_baleen_periods$period <- factor(krill_baleen_periods$period, 
                                         levels = names(ANALYSIS_PERIODS))
  krill_baleen_periods$median_tonnes <- krill_baleen_periods$mean_median * G_TO_TONNES
  krill_baleen_periods$mean_tonnes <- krill_baleen_periods$mean_mean * G_TO_TONNES
  krill_baleen_periods$min_q05_t <- krill_baleen_periods$min_q05 * G_TO_TONNES
  krill_baleen_periods$max_q95_t <- krill_baleen_periods$max_q95 * G_TO_TONNES
  
  # Reshape for grouped bar plot
  period_long <- krill_baleen_periods %>%
    select(period, median_tonnes, mean_tonnes) %>%
    pivot_longer(cols = c(median_tonnes, mean_tonnes),
                 names_to = "statistic", values_to = "consumption") %>%
    mutate(statistic = ifelse(statistic == "median_tonnes", "Median", "Mean"))
  
  p4 <- ggplot(period_long, aes(x = period, y = consumption, fill = statistic)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.8, width = 0.7) +
    scale_fill_manual(values = c("Median" = "steelblue", "Mean" = "darkblue"),
                      name = "Statistic") +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Krill Consumption by Baleen Whales: Period Comparison",
      subtitle = "Comparing median and mean across Monte Carlo ensemble (n = 2111)",
      x = "Historical Period",
      y = "Annual krill consumption (tonnes/year)"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom"
    )
  
  ggsave(file.path(OUTPUT_DIR, "krill_consumption_period_comparison.png"),
         p4, width = 8, height = 6, dpi = 300)
  cat("  Saved: krill_consumption_period_comparison.png\n")
}

# Plot 5: Relative change from pre-whaling (both median and mean)
if (!is.null(krill_baleen_periods) && "median_pct_of_baseline" %in% names(krill_baleen_periods)) {
  # Reshape for grouped bar plot
  pct_long <- krill_baleen_periods %>%
    select(period, median_pct_of_baseline, mean_pct_of_baseline) %>%
    pivot_longer(cols = c(median_pct_of_baseline, mean_pct_of_baseline),
                 names_to = "statistic", values_to = "percent") %>%
    mutate(statistic = ifelse(statistic == "median_pct_of_baseline", "Median", "Mean"))
  
  p5 <- ggplot(pct_long, aes(x = period, y = percent, fill = statistic)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.8, width = 0.7) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "darkred", linewidth = 0.8) +
    scale_fill_manual(values = c("Median" = "steelblue", "Mean" = "darkblue"),
                      name = "Statistic") +
    labs(
      title = "Krill Consumption Relative to Pre-Whaling Baseline",
      subtitle = "Baleen whales, Prydz Bay - comparing median and mean",
      x = "Historical Period",
      y = "Percent of pre-whaling consumption (%)"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom"
    )
  
  ggsave(file.path(OUTPUT_DIR, "krill_consumption_relative_change.png"),
         p5, width = 8, height = 6, dpi = 300)
  cat("  Saved: krill_consumption_relative_change.png\n")
}

###############################################################################
# Summary Statistics
###############################################################################

cat("\n=============================================================\n")
cat("SUMMARY STATISTICS (tonnes/year)\n")
cat("=============================================================\n\n")

cat("Krill Consumption by Baleen Whales (tonnes/year):\n")
cat("--------------------------------------------------\n")
if (!is.null(krill_baleen_periods)) {
  summary_df <- data.frame(
    period = krill_baleen_periods$period,
    median_tonnes_yr = round(krill_baleen_periods$median_tonnes, 1),
    mean_tonnes_yr = round(krill_baleen_periods$mean_tonnes, 1),
    median_pct = round(krill_baleen_periods$median_pct_of_baseline, 1),
    mean_pct = round(krill_baleen_periods$mean_pct_of_baseline, 1)
  )
  print(summary_df, row.names = FALSE)
  
  # Save summary table
  write.csv(summary_df, file.path(OUTPUT_DIR, "krill_consumption_summary_median_mean.csv"), 
            row.names = FALSE)
  cat("\n  Saved: krill_consumption_summary_median_mean.csv\n")
}

if (!is.null(climate_krill_baleen_stats)) {
  cat("\n\nComparison with Climate-Only (Unfished) Scenario:\n")
  cat("-------------------------------------------------\n")
  
  # Calculate period stats for climate-only
  climate_periods <- calc_period_stats(climate_krill_baleen_stats, ANALYSIS_PERIODS)
  climate_periods$median_tonnes <- climate_periods$mean_median * G_TO_TONNES
  climate_periods$mean_tonnes <- climate_periods$mean_mean * G_TO_TONNES
  
  # Build comparison using both median and mean
  climate_comparison <- data.frame(
    period = krill_baleen_periods$period,
    fishing_median = round(krill_baleen_periods$median_tonnes, 1),
    fishing_mean = round(krill_baleen_periods$mean_tonnes, 1),
    climate_median = round(climate_periods$median_tonnes[match(krill_baleen_periods$period, climate_periods$period)], 1),
    climate_mean = round(climate_periods$mean_tonnes[match(krill_baleen_periods$period, climate_periods$period)], 1)
  )
  climate_comparison$median_pct_of_climate <- round(climate_comparison$fishing_median / 
                                                     climate_comparison$climate_median * 100, 1)
  climate_comparison$mean_pct_of_climate <- round(climate_comparison$fishing_mean / 
                                                   climate_comparison$climate_mean * 100, 1)
  climate_comparison$median_difference <- climate_comparison$climate_median - climate_comparison$fishing_median
  climate_comparison$mean_difference <- climate_comparison$climate_mean - climate_comparison$fishing_mean
  
  print(climate_comparison, row.names = FALSE)
  
  # Save comparison
  write.csv(climate_comparison, 
            file.path(OUTPUT_DIR, "fishing_vs_climate_period_comparison.csv"),
            row.names = FALSE)
  cat("\n  Saved: fishing_vs_climate_period_comparison.csv\n")
}

cat("\n=============================================================\n")
cat("Plots saved to:", OUTPUT_DIR, "\n")
cat("=============================================================\n")
