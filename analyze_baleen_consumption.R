###############################################################################
# Baleen Whale Consumption Analysis
# 
# Analyzes LTL and Antarctic krill consumption by baleen whales
# Calculates:
#   1. Absolute consumption (median & mean) for each period
#   2. Relative change (modern as % of pre-whaling)
#   3. Fishing impact (fishing vs climate-only unfished scenario)
#
# Author: Generated for Prydz Bay mizer project
# Date: February 2026
###############################################################################

library(dplyr)
library(tidyr)
library(ggplot2)

# Configuration
OUTPUT_DIR <- "whale_consumption_outputs"
G_TO_TONNES <- 1e-6

# Time periods for analysis
PERIODS <- list(
  pre_whaling = list(start = 1841, end = 1880, label = "Pre-whaling"),
  early_whaling = list(start = 1900, end = 1920, label = "Early whaling"),
  peak_whaling = list(start = 1930, end = 1965, label = "Peak whaling"),
  post_whaling = list(start = 1970, end = 1976, label = "Post-whaling"),
  krill_fishing = list(start = 1977, end = 1995, label = "Krill fishing"),
  modern = list(start = 2001, end = 2010, label = "Modern")
)

###############################################################################
# Load data
###############################################################################

cat("=============================================================\n")
cat("BALEEN WHALE CONSUMPTION ANALYSIS\n")
cat("=============================================================\n\n")

cat("Loading consumption data...\n")

# Load LTL consumption
fishing_ltl <- read.csv(file.path(OUTPUT_DIR, "fishing_baleen_ltl_stats.csv"))
climate_ltl <- read.csv(file.path(OUTPUT_DIR, "climate_only_baleen_ltl_stats.csv"))

# Load krill consumption
fishing_krill <- read.csv(file.path(OUTPUT_DIR, "fishing_baleen_krill_stats.csv"))
climate_krill <- read.csv(file.path(OUTPUT_DIR, "climate_only_baleen_krill_stats.csv"))

cat(sprintf("  Loaded %d years of data (year %d to %d)\n", 
            nrow(fishing_ltl), min(fishing_ltl$year), max(fishing_ltl$year)))

###############################################################################
# Function to calculate period statistics
###############################################################################

calculate_period_stats <- function(data, period_def, metric_name) {
  period_data <- data %>%
    filter(year >= period_def$start & year <= period_def$end) %>%
    summarise(
      period = period_def$label,
      start_year = period_def$start,
      end_year = period_def$end,
      n_years = n(),
      median_mean = mean(median_tonnes),
      median_sd = sd(median_tonnes),
      mean_mean = mean(mean_tonnes),
      mean_sd = sd(mean_tonnes),
      median_min = min(median_tonnes),
      median_max = max(median_tonnes),
      mean_min = min(mean_tonnes),
      mean_max = max(mean_tonnes)
    ) %>%
    mutate(metric = metric_name)
  
  return(period_data)
}

###############################################################################
# Calculate period-based statistics for each metric
###############################################################################

cat("\n=============================================================\n")
cat("CALCULATING PERIOD STATISTICS\n")
cat("=============================================================\n\n")

# Initialize results
fishing_ltl_periods <- list()
climate_ltl_periods <- list()
fishing_krill_periods <- list()
climate_krill_periods <- list()

for (period_name in names(PERIODS)) {
  period <- PERIODS[[period_name]]
  
  fishing_ltl_periods[[period_name]] <- calculate_period_stats(fishing_ltl, period, "LTL")
  climate_ltl_periods[[period_name]] <- calculate_period_stats(climate_ltl, period, "LTL")
  fishing_krill_periods[[period_name]] <- calculate_period_stats(fishing_krill, period, "Krill")
  climate_krill_periods[[period_name]] <- calculate_period_stats(climate_krill, period, "Krill")
}

# Combine into data frames
fishing_ltl_summary <- bind_rows(fishing_ltl_periods)
climate_ltl_summary <- bind_rows(climate_ltl_periods)
fishing_krill_summary <- bind_rows(fishing_krill_periods)
climate_krill_summary <- bind_rows(climate_krill_periods)

###############################################################################
# 1. ABSOLUTE CONSUMPTION BY PERIOD
###############################################################################

cat("1. ABSOLUTE CONSUMPTION (tonnes/year)\n")
cat("======================================\n\n")

# LTL consumption
cat("BALEEN WHALE LTL CONSUMPTION:\n")
cat("-----------------------------\n")
cat("Fishing scenario:\n")
print(fishing_ltl_summary %>% 
        select(period, start_year, end_year, median_mean, mean_mean) %>%
        mutate(across(c(median_mean, mean_mean), ~round(.x, 0))))

cat("\nClimate-only (unfished) scenario:\n")
print(climate_ltl_summary %>% 
        select(period, start_year, end_year, median_mean, mean_mean) %>%
        mutate(across(c(median_mean, mean_mean), ~round(.x, 0))))

cat("\n\nBALEEN WHALE KRILL CONSUMPTION:\n")
cat("-------------------------------\n")
cat("Fishing scenario:\n")
print(fishing_krill_summary %>% 
        select(period, start_year, end_year, median_mean, mean_mean) %>%
        mutate(across(c(median_mean, mean_mean), ~round(.x, 0))))

cat("\nClimate-only (unfished) scenario:\n")
print(climate_krill_summary %>% 
        select(period, start_year, end_year, median_mean, mean_mean) %>%
        mutate(across(c(median_mean, mean_mean), ~round(.x, 0))))

###############################################################################
# 2. RELATIVE CHANGE (modern as % of pre-whaling)
###############################################################################

cat("\n\n=============================================================\n")
cat("2. RELATIVE CHANGE: Modern as % of Pre-whaling\n")
cat("=============================================================\n\n")

# Function to calculate relative change
calc_relative_change <- function(summary_df) {
  pre <- summary_df %>% filter(period == "Pre-whaling")
  mod <- summary_df %>% filter(period == "Modern")
  
  data.frame(
    metric = unique(summary_df$metric),
    median_modern = mod$median_mean,
    median_prewhaling = pre$median_mean,
    median_percent = (mod$median_mean / pre$median_mean) * 100,
    median_change = mod$median_mean - pre$median_mean,
    mean_modern = mod$mean_mean,
    mean_prewhaling = pre$mean_mean,
    mean_percent = (mod$mean_mean / pre$mean_mean) * 100,
    mean_change = mod$mean_mean - pre$mean_mean
  )
}

# Calculate relative changes
fishing_ltl_change <- calc_relative_change(fishing_ltl_summary)
climate_ltl_change <- calc_relative_change(climate_ltl_summary)
fishing_krill_change <- calc_relative_change(fishing_krill_summary)
climate_krill_change <- calc_relative_change(climate_krill_summary)

cat("LTL CONSUMPTION:\n")
cat("----------------\n")
cat(sprintf("Fishing scenario (median): Modern = %.0f tonnes/yr (%.1f%% of pre-whaling, change = %.0f)\n",
            fishing_ltl_change$median_modern, fishing_ltl_change$median_percent, 
            fishing_ltl_change$median_change))
cat(sprintf("Fishing scenario (mean):   Modern = %.0f tonnes/yr (%.1f%% of pre-whaling, change = %.0f)\n",
            fishing_ltl_change$mean_modern, fishing_ltl_change$mean_percent, 
            fishing_ltl_change$mean_change))
cat(sprintf("Climate-only (median):     Modern = %.0f tonnes/yr (%.1f%% of pre-whaling, change = %.0f)\n",
            climate_ltl_change$median_modern, climate_ltl_change$median_percent, 
            climate_ltl_change$median_change))
cat(sprintf("Climate-only (mean):       Modern = %.0f tonnes/yr (%.1f%% of pre-whaling, change = %.0f)\n",
            climate_ltl_change$mean_modern, climate_ltl_change$mean_percent, 
            climate_ltl_change$mean_change))

cat("\n\nKRILL CONSUMPTION:\n")
cat("------------------\n")
cat(sprintf("Fishing scenario (median): Modern = %.0f tonnes/yr (%.1f%% of pre-whaling, change = %.0f)\n",
            fishing_krill_change$median_modern, fishing_krill_change$median_percent, 
            fishing_krill_change$median_change))
cat(sprintf("Fishing scenario (mean):   Modern = %.0f tonnes/yr (%.1f%% of pre-whaling, change = %.0f)\n",
            fishing_krill_change$mean_modern, fishing_krill_change$mean_percent, 
            fishing_krill_change$mean_change))
cat(sprintf("Climate-only (median):     Modern = %.0f tonnes/yr (%.1f%% of pre-whaling, change = %.0f)\n",
            climate_krill_change$median_modern, climate_krill_change$median_percent, 
            climate_krill_change$median_change))
cat(sprintf("Climate-only (mean):       Modern = %.0f tonnes/yr (%.1f%% of pre-whaling, change = %.0f)\n",
            climate_krill_change$mean_modern, climate_krill_change$mean_percent, 
            climate_krill_change$mean_change))

###############################################################################
# 3. FISHING IMPACT (fishing vs climate-only)
###############################################################################

cat("\n\n=============================================================\n")
cat("3. FISHING IMPACT: Fishing vs Climate-Only (Unfished)\n")
cat("=============================================================\n\n")

# Calculate fishing impact for each period
fishing_impact_ltl <- fishing_ltl_summary %>%
  select(period, start_year, end_year, fishing_median = median_mean, fishing_mean = mean_mean) %>%
  left_join(
    climate_ltl_summary %>% select(period, climate_median = median_mean, climate_mean = mean_mean),
    by = "period"
  ) %>%
  mutate(
    median_diff = fishing_median - climate_median,
    median_ratio = (fishing_median / climate_median) * 100,
    mean_diff = fishing_mean - climate_mean,
    mean_ratio = (fishing_mean / climate_mean) * 100,
    metric = "LTL"
  )

fishing_impact_krill <- fishing_krill_summary %>%
  select(period, start_year, end_year, fishing_median = median_mean, fishing_mean = mean_mean) %>%
  left_join(
    climate_krill_summary %>% select(period, climate_median = median_mean, climate_mean = mean_mean),
    by = "period"
  ) %>%
  mutate(
    median_diff = fishing_median - climate_median,
    median_ratio = (fishing_median / climate_median) * 100,
    mean_diff = fishing_mean - climate_mean,
    mean_ratio = (fishing_mean / climate_mean) * 100,
    metric = "Krill"
  )

cat("LTL CONSUMPTION - FISHING IMPACT:\n")
cat("---------------------------------\n")
print(fishing_impact_ltl %>%
        select(period, start_year, end_year, 
               fishing_median, climate_median, median_ratio, median_diff) %>%
        mutate(across(c(fishing_median, climate_median, median_diff), ~round(.x, 0)),
               median_ratio = round(median_ratio, 1)))

cat("\n\nKRILL CONSUMPTION - FISHING IMPACT:\n")
cat("-----------------------------------\n")
print(fishing_impact_krill %>%
        select(period, start_year, end_year, 
               fishing_median, climate_median, median_ratio, median_diff) %>%
        mutate(across(c(fishing_median, climate_median, median_diff), ~round(.x, 0)),
               median_ratio = round(median_ratio, 1)))

###############################################################################
# Save results
###############################################################################

cat("\n\n=============================================================\n")
cat("SAVING RESULTS\n")
cat("=============================================================\n\n")

# Save absolute consumption summaries
write.csv(fishing_ltl_summary, 
          file.path(OUTPUT_DIR, "baleen_ltl_fishing_period_summary.csv"), 
          row.names = FALSE)
write.csv(climate_ltl_summary, 
          file.path(OUTPUT_DIR, "baleen_ltl_climate_only_period_summary.csv"), 
          row.names = FALSE)
write.csv(fishing_krill_summary, 
          file.path(OUTPUT_DIR, "baleen_krill_fishing_period_summary.csv"), 
          row.names = FALSE)
write.csv(climate_krill_summary, 
          file.path(OUTPUT_DIR, "baleen_krill_climate_only_period_summary.csv"), 
          row.names = FALSE)

# Save relative change summaries
relative_changes <- bind_rows(
  fishing_ltl_change %>% mutate(scenario = "Fishing"),
  climate_ltl_change %>% mutate(scenario = "Climate-only"),
  fishing_krill_change %>% mutate(scenario = "Fishing"),
  climate_krill_change %>% mutate(scenario = "Climate-only")
)
write.csv(relative_changes, 
          file.path(OUTPUT_DIR, "baleen_relative_changes_modern_vs_prewhaling.csv"), 
          row.names = FALSE)

# Save fishing impact
fishing_impact_combined <- bind_rows(fishing_impact_ltl, fishing_impact_krill)
write.csv(fishing_impact_combined, 
          file.path(OUTPUT_DIR, "baleen_fishing_impact_all_periods.csv"), 
          row.names = FALSE)

cat("Saved:\n")
cat("  - baleen_ltl_fishing_period_summary.csv\n")
cat("  - baleen_ltl_climate_only_period_summary.csv\n")
cat("  - baleen_krill_fishing_period_summary.csv\n")
cat("  - baleen_krill_climate_only_period_summary.csv\n")
cat("  - baleen_relative_changes_modern_vs_prewhaling.csv\n")
cat("  - baleen_fishing_impact_all_periods.csv\n")

###############################################################################
# Create summary visualization data
###############################################################################

cat("\n\n=============================================================\n")
cat("KEY FINDINGS SUMMARY\n")
cat("=============================================================\n\n")

cat("MODERN (2001-2010) vs PRE-WHALING (1841-1880):\n")
cat("-----------------------------------------------\n\n")

cat("LTL Consumption (using median values):\n")
cat(sprintf("  Fishing scenario:   %.0f tonnes/yr (%.1f%% of pre-whaling)\n",
            fishing_ltl_change$median_modern, fishing_ltl_change$median_percent))
cat(sprintf("  Climate-only:       %.0f tonnes/yr (%.1f%% of pre-whaling)\n",
            climate_ltl_change$median_modern, climate_ltl_change$median_percent))
cat(sprintf("  Fishing impact:     %.0f tonnes/yr 'missing' consumption\n",
            fishing_impact_ltl %>% filter(period == "Modern") %>% pull(median_diff)))
cat(sprintf("  Fishing as %% of unfished: %.1f%%\n",
            fishing_impact_ltl %>% filter(period == "Modern") %>% pull(median_ratio)))

cat("\nKrill Consumption (using median values):\n")
cat(sprintf("  Fishing scenario:   %.0f tonnes/yr (%.1f%% of pre-whaling)\n",
            fishing_krill_change$median_modern, fishing_krill_change$median_percent))
cat(sprintf("  Climate-only:       %.0f tonnes/yr (%.1f%% of pre-whaling)\n",
            climate_krill_change$median_modern, climate_krill_change$median_percent))
cat(sprintf("  Fishing impact:     %.0f tonnes/yr 'missing' consumption\n",
            fishing_impact_krill %>% filter(period == "Modern") %>% pull(median_diff)))
cat(sprintf("  Fishing as %% of unfished: %.1f%%\n",
            fishing_impact_krill %>% filter(period == "Modern") %>% pull(median_ratio)))

cat("\n\nKrill as % of LTL consumption:\n")
cat(sprintf("  Pre-whaling (fishing):  %.1f%%\n",
            (fishing_krill_summary %>% filter(period == "Pre-whaling") %>% pull(median_mean)) /
            (fishing_ltl_summary %>% filter(period == "Pre-whaling") %>% pull(median_mean)) * 100))
cat(sprintf("  Modern (fishing):       %.1f%%\n",
            (fishing_krill_summary %>% filter(period == "Modern") %>% pull(median_mean)) /
            (fishing_ltl_summary %>% filter(period == "Modern") %>% pull(median_mean)) * 100))

cat("\n=============================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("=============================================================\n")
