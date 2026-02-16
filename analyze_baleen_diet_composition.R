###############################################################################
# Baleen Whale Diet Composition Analysis
# 
# Extracts and visualizes diet proportions (including Resource) for baleen
# whales across the Monte Carlo ensemble.
#
# Uses getDiet(proportion = TRUE) to get diet composition as proportions
# of total consumption, aggregated across ensemble with uncertainty.
#
# Author: Generated for Prydz Bay mizer project
# Date: February 2026
###############################################################################

library(therMizer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

# Configuration
OUTPUT_DIR <- "whale_consumption_outputs"
MODEL_DOMAIN_AREA <- 1.95e+13

# Baleen whale species
BALEEN_SPECIES <- c("baleen whales", "minke whales")

###############################################################################
# Extract diet proportions for baleen whales
###############################################################################

extract_diet_proportions <- function(sim, species_list, times_to_extract = NULL) {
  # Extract diet proportions for specified species across time
  # 
  # Args:
  #   sim: MizerSim object
  #   species_list: Vector of species names to aggregate
  #   times_to_extract: Specific time indices to extract (NULL = all)
  # 
  # Returns:
  #   Data frame with year, prey, and proportion columns
  
  params <- sim@params
  times <- as.numeric(dimnames(sim@n)$time)
  
  if (is.null(times_to_extract)) {
    times_to_extract <- seq_along(times)
  }
  
  # Storage for results
  results_list <- list()
  
  for (t_idx in times_to_extract) {
    year <- times[t_idx]
    
    # Get state at this time
    n <- sim@n[t_idx, , , drop = FALSE]
    dim(n) <- dim(sim@n)[2:3]
    dimnames(n) <- dimnames(sim@n)[2:3]
    
    n_pp <- sim@n_pp[t_idx, ]
    n_other <- sim@n_other[t_idx, ]
    if (!is.null(dimnames(sim@n_other))) {
      names(n_other) <- dimnames(sim@n_other)$component
    }
    
    # Get diet proportions
    diet_prop <- tryCatch({
      getDiet(params, n = n, n_pp = n_pp, n_other = n_other, proportion = TRUE)
    }, error = function(e) NULL)
    
    if (is.null(diet_prop)) next
    
    # Filter to baleen species and sum across size classes
    valid_species <- species_list[species_list %in% dimnames(diet_prop)$predator]
    if (length(valid_species) == 0) next
    
    # Sum diet proportions across predator species and size classes
    # Weighted by abundance
    total_consumption <- 0
    weighted_diet <- rep(0, dim(diet_prop)[3])
    names(weighted_diet) <- dimnames(diet_prop)[[3]]
    
    dw <- params@dw
    
    for (sp in valid_species) {
      n_pred <- n[sp, ]
      # For each prey, sum consumption across sizes
      for (prey_idx in seq_len(dim(diet_prop)[3])) {
        # diet_prop gives proportion, but we need to weight by actual consumption
        # consumption rate * abundance for each size
        consumption_by_size <- diet_prop[sp, , prey_idx] * n_pred * dw
        weighted_diet[prey_idx] <- weighted_diet[prey_idx] + sum(consumption_by_size)
        total_consumption <- total_consumption + sum(consumption_by_size)
      }
    }
    
    # Convert to proportions
    if (total_consumption > 0) {
      diet_proportions <- weighted_diet / total_consumption
    } else {
      diet_proportions <- weighted_diet * 0
    }
    
    # Store results
    results_list[[length(results_list) + 1]] <- data.frame(
      year = year,
      prey = names(weighted_diet),
      proportion = as.numeric(diet_proportions),
      stringsAsFactors = FALSE
    )
  }
  
  return(bind_rows(results_list))
}

###############################################################################
# Process ensemble
###############################################################################

process_diet_ensemble <- function(ensemble_file, scenario_name, 
                                   sample_interval = 10, max_sims = NULL) {
  # Extract diet composition across ensemble
  # 
  # Args:
  #   ensemble_file: Path to ensemble RDS
  #   scenario_name: Name for labeling
  #   sample_interval: Extract every Nth year (default 10 for efficiency)
  #   max_sims: Maximum simulations to process (NULL = all)
  
  cat(sprintf("\n=============================================================\n"))
  cat(sprintf("Processing %s ensemble\n", scenario_name))
  cat(sprintf("=============================================================\n\n"))
  
  # Load ensemble
  cat("Loading ensemble...\n")
  mc <- readRDS(ensemble_file)
  
  if ("simulations" %in% names(mc)) {
    simulations_list <- mc$simulations
  } else if (is.list(mc) && inherits(mc[[1]], "MizerSim")) {
    simulations_list <- mc
  } else {
    stop("Unrecognized ensemble structure")
  }
  
  n_sims <- length(simulations_list)
  if (!is.null(max_sims)) {
    n_sims <- min(n_sims, max_sims)
    simulations_list <- simulations_list[1:n_sims]
  }
  
  cat(sprintf("  Processing %d simulations\n", n_sims))
  
  # Get time structure from first simulation
  first_sim <- simulations_list[[1]]
  all_times <- as.numeric(dimnames(first_sim@n)$time)
  
  # Sample time indices
  if (sample_interval > 1) {
    time_indices <- seq(1, length(all_times), by = sample_interval)
  } else {
    time_indices <- seq_along(all_times)
  }
  
  cat(sprintf("  Extracting %d time points (every %d years)\n", 
              length(time_indices), sample_interval))
  
  # Process each simulation
  all_results <- list()
  pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
  
  for (i in seq_len(n_sims)) {
    setTxtProgressBar(pb, i)
    
    sim <- tryCatch(simulations_list[[i]], error = function(e) NULL)
    if (is.null(sim)) next
    
    diet_df <- tryCatch({
      extract_diet_proportions(sim, BALEEN_SPECIES, time_indices)
    }, error = function(e) NULL)
    
    if (!is.null(diet_df) && nrow(diet_df) > 0) {
      diet_df$sim_id <- i
      all_results[[length(all_results) + 1]] <- diet_df
    }
  }
  close(pb)
  
  cat(sprintf("\n  Successfully processed %d simulations\n", length(all_results)))
  
  return(bind_rows(all_results))
}

###############################################################################
# Calculate ensemble statistics
###############################################################################

calculate_diet_stats <- function(diet_data) {
  # Calculate median, mean, and quantiles for diet proportions
  
  stats <- diet_data %>%
    group_by(year, prey) %>%
    summarise(
      n_sims = n(),
      median = median(proportion, na.rm = TRUE),
      mean = mean(proportion, na.rm = TRUE),
      sd = sd(proportion, na.rm = TRUE),
      q05 = quantile(proportion, 0.05, na.rm = TRUE),
      q25 = quantile(proportion, 0.25, na.rm = TRUE),
      q75 = quantile(proportion, 0.75, na.rm = TRUE),
      q95 = quantile(proportion, 0.95, na.rm = TRUE),
      .groups = 'drop'
    )
  
  return(stats)
}

###############################################################################
# Main execution
###############################################################################

cat("=============================================================\n")
cat("BALEEN WHALE DIET COMPOSITION ANALYSIS\n")
cat("=============================================================\n\n")

# Process fishing ensemble
fishing_diet <- process_diet_ensemble(
  "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds",
  "Fishing",
  sample_interval = 5,  # Every 5 years for better resolution
  max_sims = NULL       # Process all simulations
)

# Save raw data
saveRDS(fishing_diet, file.path(OUTPUT_DIR, "baleen_diet_proportions_fishing_raw.rds"))

# Calculate statistics
cat("\nCalculating ensemble statistics...\n")
fishing_stats <- calculate_diet_stats(fishing_diet)

# Save statistics
write.csv(fishing_stats, 
          file.path(OUTPUT_DIR, "baleen_diet_proportions_fishing_stats.csv"),
          row.names = FALSE)

cat("Saved:\n")
cat("  - baleen_diet_proportions_fishing_raw.rds\n")
cat("  - baleen_diet_proportions_fishing_stats.csv\n")

###############################################################################
# Create visualizations
###############################################################################

cat("\n=============================================================\n")
cat("CREATING VISUALIZATIONS\n")
cat("=============================================================\n\n")

# Identify major prey groups (median proportion > 0.01 in any year)
major_prey <- fishing_stats %>%
  group_by(prey) %>%
  summarise(max_median = max(median)) %>%
  filter(max_median > 0.01) %>%
  pull(prey)

cat(sprintf("Major prey groups (>1%% in any year): %d\n", length(major_prey)))
print(major_prey)

# Group minor prey as "Other"
plot_data <- fishing_stats %>%
  mutate(
    prey_group = ifelse(prey %in% major_prey, prey, "Other")
  ) %>%
  group_by(year, prey_group) %>%
  summarise(
    median = sum(median),
    mean = sum(mean),
    q25 = sum(q25),
    q75 = sum(q75),
    .groups = 'drop'
  )

# Order prey groups by overall importance
prey_order <- plot_data %>%
  group_by(prey_group) %>%
  summarise(total = mean(median)) %>%
  arrange(desc(total)) %>%
  pull(prey_group)

plot_data$prey_group <- factor(plot_data$prey_group, levels = prey_order)

# 1. Stacked area plot (median proportions)
cat("\n1. Creating stacked area plot...\n")

p1 <- ggplot(plot_data, aes(x = year, y = median, fill = prey_group)) +
  geom_area(position = "stack", alpha = 0.8) +
  scale_fill_viridis_d(option = "turbo", direction = -1) +
  scale_y_continuous(labels = scales::percent_format(), 
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  labs(
    title = "Baleen Whale Diet Composition Over Time",
    subtitle = "Prydz Bay Mizer Model - Fishing Scenario (Median Ensemble Values)",
    x = "Year",
    y = "Proportion of Total Consumption",
    fill = "Prey Group"
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(OUTPUT_DIR, "baleen_diet_stacked_area.png"),
       p1, width = 12, height = 6, dpi = 300)
ggsave(file.path(OUTPUT_DIR, "baleen_diet_stacked_area.pdf"),
       p1, width = 12, height = 6)

# 2. Time series for major prey groups with uncertainty
cat("2. Creating time series with uncertainty ribbons...\n")

major_plot_data <- plot_data %>%
  filter(prey_group %in% major_prey)

p2 <- ggplot(major_plot_data, 
             aes(x = year, y = median, color = prey_group, fill = prey_group)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(option = "turbo", direction = -1) +
  scale_fill_viridis_d(option = "turbo", direction = -1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Major Prey Groups in Baleen Whale Diet",
    subtitle = "Lines = median, ribbons = 25th-75th percentile (2,111 simulations)",
    x = "Year",
    y = "Proportion of Total Consumption",
    color = "Prey Group",
    fill = "Prey Group"
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.minor.x = element_blank()
  )

ggsave(file.path(OUTPUT_DIR, "baleen_diet_major_prey_timeseries.png"),
       p2, width = 12, height = 6, dpi = 300)
ggsave(file.path(OUTPUT_DIR, "baleen_diet_major_prey_timeseries.pdf"),
       p2, width = 12, height = 6)

# 3. Focus on Resource vs other prey
cat("3. Creating Resource vs biological prey comparison...\n")

resource_comparison <- plot_data %>%
  mutate(
    category = ifelse(prey_group == "Resource", "Resource (background plankton)", 
                     "Biological prey (species)")
  ) %>%
  group_by(year, category) %>%
  summarise(
    median = sum(median),
    q25 = sum(q25),
    q75 = sum(q75),
    .groups = 'drop'
  )

p3 <- ggplot(resource_comparison, 
             aes(x = year, y = median, color = category, fill = category)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("Resource (background plankton)" = "#E64B35",
                                 "Biological prey (species)" = "#4DBBD5")) +
  scale_fill_manual(values = c("Resource (background plankton)" = "#E64B35",
                                "Biological prey (species)" = "#4DBBD5")) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 1)) +
  labs(
    title = "Resource vs Biological Prey in Baleen Whale Diet",
    subtitle = "Lines = median, ribbons = 25th-75th percentile",
    x = "Year",
    y = "Proportion of Total Consumption",
    color = "Prey Category",
    fill = "Prey Category"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(OUTPUT_DIR, "baleen_diet_resource_vs_biological.png"),
       p3, width = 10, height = 6, dpi = 300)
ggsave(file.path(OUTPUT_DIR, "baleen_diet_resource_vs_biological.pdf"),
       p3, width = 10, height = 6)

# 4. Period comparison bar plot
cat("4. Creating period comparison...\n")

period_data <- fishing_stats %>%
  mutate(
    period = case_when(
      year >= 1841 & year <= 1880 ~ "Pre-whaling",
      year >= 1900 & year <= 1920 ~ "Early whaling",
      year >= 1930 & year <= 1965 ~ "Peak whaling",
      year >= 1970 & year <= 1976 ~ "Post-whaling",
      year >= 1977 & year <= 1995 ~ "Krill fishing",
      year >= 2001 & year <= 2010 ~ "Modern",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(period, prey) %>%
  summarise(median = mean(median), .groups = 'drop') %>%
  mutate(
    prey_group = ifelse(prey %in% major_prey, prey, "Other"),
    period = factor(period, levels = c("Pre-whaling", "Early whaling", "Peak whaling",
                                       "Post-whaling", "Krill fishing", "Modern"))
  ) %>%
  group_by(period, prey_group) %>%
  summarise(median = sum(median), .groups = 'drop')

period_data$prey_group <- factor(period_data$prey_group, levels = prey_order)

p4 <- ggplot(period_data, aes(x = period, y = median, fill = prey_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "turbo", direction = -1) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0, 0)) +
  labs(
    title = "Baleen Whale Diet Composition by Historical Period",
    subtitle = "Average composition within each period (median ensemble values)",
    x = "Period",
    y = "Proportion of Total Consumption",
    fill = "Prey Group"
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

ggsave(file.path(OUTPUT_DIR, "baleen_diet_period_comparison.png"),
       p4, width = 10, height = 6, dpi = 300)
ggsave(file.path(OUTPUT_DIR, "baleen_diet_period_comparison.pdf"),
       p4, width = 10, height = 6)

cat("\nSaved visualizations:\n")
cat("  - baleen_diet_stacked_area.png/pdf\n")
cat("  - baleen_diet_major_prey_timeseries.png/pdf\n")
cat("  - baleen_diet_resource_vs_biological.png/pdf\n")
cat("  - baleen_diet_period_comparison.png/pdf\n")

###############################################################################
# Summary statistics
###############################################################################

cat("\n=============================================================\n")
cat("DIET COMPOSITION SUMMARY\n")
cat("=============================================================\n\n")

# Overall average composition
overall_avg <- fishing_stats %>%
  group_by(prey) %>%
  summarise(
    avg_median = mean(median),
    avg_mean = mean(mean)
  ) %>%
  arrange(desc(avg_median))

cat("Overall average diet composition (all years):\n")
cat("--------------------------------------------\n")
print(overall_avg %>% 
        filter(avg_median > 0.001) %>%
        mutate(
          avg_median_pct = round(avg_median * 100, 2),
          avg_mean_pct = round(avg_mean * 100, 2)
        ) %>%
        select(prey, avg_median_pct, avg_mean_pct),
      n = Inf)

# Resource proportion over time
resource_summary <- fishing_stats %>%
  filter(prey == "Resource") %>%
  summarise(
    min_median = min(median),
    max_median = max(median),
    mean_median = mean(median)
  )

cat(sprintf("\nResource contribution:\n"))
cat(sprintf("  Range (median): %.1f%% - %.1f%%\n", 
            resource_summary$min_median * 100,
            resource_summary$max_median * 100))
cat(sprintf("  Average (median): %.1f%%\n", 
            resource_summary$mean_median * 100))

cat("\n=============================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("=============================================================\n")
