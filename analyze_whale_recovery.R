# Whale Recovery and Krill Fishing Impact Analysis
# Examines how krill fishing affected whale recovery rates
# Includes: Recovery trajectories, Feeding levels, Krill-Whale biomass ratios

library(mizer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

cat("=== Whale Recovery and Krill Fishing Impact Analysis ===\n\n")

# ------------------------------------------------------------------------------
# Define analysis periods and species groups
# ------------------------------------------------------------------------------

# Key periods for analysis
periods <- list(
  pre_whaling = 1841:1860,
  peak_whaling = 1930:1950,
  early_recovery = 1960:1976,  # Post-whaling, pre-krill fishing
  krill_fishing = 1977:1995,   # Peak krill fishing period
  late_recovery = 1996:2010    # Reduced krill fishing
)

# Whale species (based on typical Antarctic ecosystem model)
whale_species <- c("minke whales", "baleen whales", "sperm whales", "orca")

# Krill species
krill_species <- c("antarctic krill", "other krill")

# All marine mammals (for broader analysis)
mammal_species <- c("minke whales", "baleen whales", "sperm whales", "orca", 
                    "leopard seals", "small divers", "medium divers", "large divers")

# ------------------------------------------------------------------------------
# Load MC simulation results
# ------------------------------------------------------------------------------
cat("Loading MC simulation results...\n")

mc_results_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/combined_rerun_successful_sims_20250923_122211.rds"
mc_results <- readRDS(mc_results_file)

simulations_list <- mc_results$simulations
n_sims <- length(simulations_list)
cat("  Found", n_sims, "simulations\n")

# Get structure from first simulation
first_sim <- simulations_list[[1]]
sim_times <- as.numeric(dimnames(first_sim@n)$time)
all_species <- dimnames(first_sim@n)$sp
cat("  Time range:", min(sim_times), "-", max(sim_times), "\n")
cat("  Species:", paste(all_species, collapse = ", "), "\n\n")

# Check which whale/krill species exist in the model
whale_species <- whale_species[whale_species %in% all_species]
krill_species <- krill_species[krill_species %in% all_species]
mammal_species <- mammal_species[mammal_species %in% all_species]

cat("  Whale species found:", paste(whale_species, collapse = ", "), "\n")
cat("  Krill species found:", paste(krill_species, collapse = ", "), "\n")
cat("  All mammal species found:", paste(mammal_species, collapse = ", "), "\n\n")

# ------------------------------------------------------------------------------
# ANALYSIS 1: Recovery Trajectory - Biomass through time
# ------------------------------------------------------------------------------
cat("=== Analysis 1: Recovery Trajectories ===\n")

# Extract annual biomass for each simulation
extract_biomass_timeseries <- function(sim, species_list) {
  params <- sim@params
  w <- params@w
  dw <- params@dw
  times <- as.numeric(dimnames(sim@n)$time)
  
  # Calculate biomass for each species at each time
  biomass_ts <- matrix(NA, nrow = length(times), ncol = length(species_list))
  colnames(biomass_ts) <- species_list
  rownames(biomass_ts) <- times
  
  for (sp in species_list) {
    sp_idx <- which(dimnames(sim@n)$sp == sp)
    if (length(sp_idx) > 0) {
      # Biomass = sum of n * w * dw across size bins
      for (t in seq_along(times)) {
        biomass_ts[t, sp] <- sum(sim@n[t, sp_idx, ] * w * dw)
      }
    }
  }
  
  return(biomass_ts)
}

cat("Extracting biomass time series for all simulations...\n")

# Storage for all simulations
all_whale_biomass <- list()
all_krill_biomass <- list()
all_mammal_biomass <- list()

pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
skipped <- 0

for (i in seq_len(n_sims)) {
  setTxtProgressBar(pb, i)
  
  sim <- tryCatch(simulations_list[[i]], error = function(e) NULL)
  
  if (is.null(sim)) {
    skipped <- skipped + 1
    next
  }
  
  # Extract biomass for different groups
  whale_bio <- tryCatch(extract_biomass_timeseries(sim, whale_species), error = function(e) NULL)
  krill_bio <- tryCatch(extract_biomass_timeseries(sim, krill_species), error = function(e) NULL)
  mammal_bio <- tryCatch(extract_biomass_timeseries(sim, mammal_species), error = function(e) NULL)
  
  if (!is.null(whale_bio)) {
    all_whale_biomass[[length(all_whale_biomass) + 1]] <- whale_bio
  }
  if (!is.null(krill_bio)) {
    all_krill_biomass[[length(all_krill_biomass) + 1]] <- krill_bio
  }
  if (!is.null(mammal_bio)) {
    all_mammal_biomass[[length(all_mammal_biomass) + 1]] <- mammal_bio
  }
}
close(pb)

cat("\n  Processed", n_sims - skipped, "simulations\n\n")

# Calculate total whale biomass for each simulation
total_whale_biomass <- do.call(cbind, lapply(all_whale_biomass, function(x) rowSums(x, na.rm = TRUE)))
total_krill_biomass <- do.call(cbind, lapply(all_krill_biomass, function(x) rowSums(x, na.rm = TRUE)))
total_mammal_biomass <- do.call(cbind, lapply(all_mammal_biomass, function(x) rowSums(x, na.rm = TRUE)))

# Calculate ensemble statistics
years <- as.numeric(rownames(all_whale_biomass[[1]]))

whale_stats <- data.frame(
  year = years,
  median = apply(total_whale_biomass, 1, median, na.rm = TRUE),
  mean = apply(total_whale_biomass, 1, mean, na.rm = TRUE),
  q25 = apply(total_whale_biomass, 1, quantile, 0.25, na.rm = TRUE),
  q75 = apply(total_whale_biomass, 1, quantile, 0.75, na.rm = TRUE),
  q05 = apply(total_whale_biomass, 1, quantile, 0.05, na.rm = TRUE),
  q95 = apply(total_whale_biomass, 1, quantile, 0.95, na.rm = TRUE)
)

krill_stats <- data.frame(
  year = years,
  median = apply(total_krill_biomass, 1, median, na.rm = TRUE),
  mean = apply(total_krill_biomass, 1, mean, na.rm = TRUE),
  q25 = apply(total_krill_biomass, 1, quantile, 0.25, na.rm = TRUE),
  q75 = apply(total_krill_biomass, 1, quantile, 0.75, na.rm = TRUE)
)

# Normalize to pre-whaling baseline
baseline_whale <- mean(whale_stats$median[whale_stats$year %in% periods$pre_whaling])
baseline_krill <- mean(krill_stats$median[krill_stats$year %in% periods$pre_whaling])

whale_stats$relative <- whale_stats$median / baseline_whale
whale_stats$relative_q25 <- whale_stats$q25 / baseline_whale
whale_stats$relative_q75 <- whale_stats$q75 / baseline_whale

krill_stats$relative <- krill_stats$median / baseline_krill
krill_stats$relative_q25 <- krill_stats$q25 / baseline_krill
krill_stats$relative_q75 <- krill_stats$q75 / baseline_krill

# ------------------------------------------------------------------------------
# Calculate recovery rates for different periods
# ------------------------------------------------------------------------------
cat("Calculating recovery rates...\n")

calculate_recovery_rate <- function(biomass_matrix, period_years, years) {
  # Get indices for the period
  period_idx <- which(years %in% period_years)
  
  if (length(period_idx) < 2) return(NA)
  
  # Calculate annual growth rate for each simulation
  rates <- apply(biomass_matrix[period_idx, ], 2, function(bio) {
    if (all(is.na(bio)) || length(bio) < 2) return(NA)
    # Log-linear regression to get exponential growth rate
    t <- seq_along(bio)
    if (any(bio <= 0, na.rm = TRUE)) return(NA)
    fit <- tryCatch(lm(log(bio) ~ t), error = function(e) NULL)
    if (is.null(fit)) return(NA)
    return(coef(fit)[2])  # Slope = growth rate
  })
  
  return(rates)
}

# Calculate recovery rates for each period
recovery_rates <- list()
for (period_name in names(periods)) {
  rates <- calculate_recovery_rate(total_whale_biomass, periods[[period_name]], years)
  recovery_rates[[period_name]] <- rates[!is.na(rates)]
}

# Summary statistics for recovery rates
rate_summary <- data.frame(
  period = names(periods),
  n_sims = sapply(recovery_rates, length),
  median_rate = sapply(recovery_rates, median, na.rm = TRUE),
  mean_rate = sapply(recovery_rates, mean, na.rm = TRUE),
  q25_rate = sapply(recovery_rates, quantile, 0.25, na.rm = TRUE),
  q75_rate = sapply(recovery_rates, quantile, 0.75, na.rm = TRUE),
  pct_positive = sapply(recovery_rates, function(x) mean(x > 0, na.rm = TRUE) * 100)
)

# Convert to annual % change
rate_summary$median_pct_per_year <- (exp(rate_summary$median_rate) - 1) * 100
rate_summary$mean_pct_per_year <- (exp(rate_summary$mean_rate) - 1) * 100

cat("\nWhale Recovery Rates by Period:\n")
print(rate_summary %>% select(period, n_sims, median_pct_per_year, pct_positive))

# ------------------------------------------------------------------------------
# ANALYSIS 2: Feeding Levels
# ------------------------------------------------------------------------------
cat("\n=== Analysis 2: Feeding Levels ===\n")

# Extract feeding level time series
extract_feeding_level <- function(sim, species_list) {
  times <- as.numeric(dimnames(sim@n)$time)
  
  # Get feeding level for each time step
  feeding_ts <- matrix(NA, nrow = length(times), ncol = length(species_list))
  colnames(feeding_ts) <- species_list
  rownames(feeding_ts) <- times
  
  for (t in seq_along(times)) {
    # Get params at this time step (for time-varying models)
    fl <- tryCatch({
      getFeedingLevel(sim, time_range = times[t])
    }, error = function(e) NULL)
    
    if (!is.null(fl) && is.matrix(fl)) {
      for (sp in species_list) {
        sp_idx <- which(rownames(fl) == sp)
        if (length(sp_idx) > 0) {
          # Average feeding level across size classes (weighted by abundance)
          feeding_ts[t, sp] <- mean(fl[sp_idx, ], na.rm = TRUE)
        }
      }
    }
  }
  
  return(feeding_ts)
}

cat("Extracting feeding levels (this may take a while)...\n")

# Sample a subset of simulations for feeding level (computationally intensive)
n_sample <- min(100, n_sims)
sample_idx <- sample(seq_len(n_sims), n_sample)

all_whale_feeding <- list()

pb <- txtProgressBar(min = 0, max = n_sample, style = 3)

for (i in seq_along(sample_idx)) {
  setTxtProgressBar(pb, i)
  
  sim <- tryCatch(simulations_list[[sample_idx[i]]], error = function(e) NULL)
  
  if (is.null(sim)) next
  
  feeding <- tryCatch(extract_feeding_level(sim, whale_species), error = function(e) NULL)
  
  if (!is.null(feeding)) {
    all_whale_feeding[[length(all_whale_feeding) + 1]] <- feeding
  }
}
close(pb)

cat("\n  Extracted feeding levels from", length(all_whale_feeding), "simulations\n")

# Calculate mean feeding level across whale species for each simulation
if (length(all_whale_feeding) > 0) {
  mean_whale_feeding <- do.call(cbind, lapply(all_whale_feeding, function(x) rowMeans(x, na.rm = TRUE)))
  
  feeding_stats <- data.frame(
    year = years,
    median = apply(mean_whale_feeding, 1, median, na.rm = TRUE),
    mean = apply(mean_whale_feeding, 1, mean, na.rm = TRUE),
    q25 = apply(mean_whale_feeding, 1, quantile, 0.25, na.rm = TRUE),
    q75 = apply(mean_whale_feeding, 1, quantile, 0.75, na.rm = TRUE)
  )
} else {
  feeding_stats <- NULL
  cat("  Warning: Could not extract feeding levels\n")
}

# ------------------------------------------------------------------------------
# ANALYSIS 3: Krill-Whale Biomass Ratio
# ------------------------------------------------------------------------------
cat("\n=== Analysis 3: Krill-Whale Biomass Ratio ===\n")

# Calculate ratio for each simulation
krill_whale_ratio <- total_krill_biomass / total_whale_biomass

ratio_stats <- data.frame(
  year = years,
  median = apply(krill_whale_ratio, 1, median, na.rm = TRUE),
  mean = apply(krill_whale_ratio, 1, mean, na.rm = TRUE),
  q25 = apply(krill_whale_ratio, 1, quantile, 0.25, na.rm = TRUE),
  q75 = apply(krill_whale_ratio, 1, quantile, 0.75, na.rm = TRUE)
)

# Calculate period averages
ratio_by_period <- data.frame(
  period = names(periods),
  median_ratio = sapply(periods, function(p) {
    idx <- which(years %in% p)
    median(ratio_stats$median[idx], na.rm = TRUE)
  }),
  mean_ratio = sapply(periods, function(p) {
    idx <- which(years %in% p)
    mean(ratio_stats$median[idx], na.rm = TRUE)
  })
)

cat("\nKrill-to-Whale Biomass Ratio by Period:\n")
print(ratio_by_period)

# ------------------------------------------------------------------------------
# Create output directory
# ------------------------------------------------------------------------------
output_dir <- "fishmip_outputs/whale_recovery_analysis"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ------------------------------------------------------------------------------
# PLOT 1: Whale Biomass Recovery Trajectory
# ------------------------------------------------------------------------------
cat("\nCreating plots...\n")

# Add period shading data
period_shading <- data.frame(
  xmin = c(1930, 1977),
  xmax = c(1960, 1995),
  ymin = -Inf,
  ymax = Inf,
  label = c("Peak Whaling", "Peak Krill Fishing")
)

p_trajectory <- ggplot(whale_stats, aes(x = year)) +
  # Period shading
  annotate("rect", xmin = 1930, xmax = 1960, ymin = -Inf, ymax = Inf, 
           fill = "red", alpha = 0.1) +
  annotate("rect", xmin = 1977, xmax = 1995, ymin = -Inf, ymax = Inf, 
           fill = "orange", alpha = 0.1) +
  # Uncertainty ribbon
  geom_ribbon(aes(ymin = relative_q25, ymax = relative_q75), fill = "steelblue", alpha = 0.3) +
  # Median line
  geom_line(aes(y = relative), color = "steelblue", linewidth = 1) +
  # Reference line
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  # Period labels

  annotate("text", x = 1945, y = max(whale_stats$relative_q75) * 0.95, 
           label = "Whaling Era", color = "darkred", size = 3.5, fontface = "italic") +
  annotate("text", x = 1986, y = max(whale_stats$relative_q75) * 0.95, 
           label = "Krill Fishing", color = "darkorange", size = 3.5, fontface = "italic") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(
    title = "Whale Population Recovery Trajectory",
    subtitle = paste0("Relative to pre-whaling baseline (1841-1860) | Median with 50% CI (n=", n_sims, " simulations)"),
    x = "Year",
    y = "Whale biomass (% of pre-whaling)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    axis.title = element_text(size = 12)
  )

ggsave(file.path(output_dir, "whale_recovery_trajectory.png"), 
       p_trajectory, width = 12, height = 7, dpi = 300)
cat("  Saved: whale_recovery_trajectory.png\n")

# ------------------------------------------------------------------------------
# PLOT 2: Recovery Rates by Period
# ------------------------------------------------------------------------------
rate_plot_data <- rate_summary %>%
  mutate(period = factor(period, levels = c("pre_whaling", "peak_whaling", "early_recovery", 
                                             "krill_fishing", "late_recovery"))) %>%
  mutate(period_label = c("Pre-Whaling\n(1841-1860)", "Peak Whaling\n(1930-1950)", 
                          "Early Recovery\n(1960-1976)", "Krill Fishing\n(1977-1995)", 
                          "Late Recovery\n(1996-2010)")[as.numeric(period)])

p_rates <- ggplot(rate_plot_data, aes(x = period_label, y = median_pct_per_year)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_col(aes(fill = median_pct_per_year > 0), width = 0.7) +
  geom_errorbar(aes(ymin = (exp(q25_rate) - 1) * 100, ymax = (exp(q75_rate) - 1) * 100), 
                width = 0.2) +
  scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "firebrick"), guide = "none") +
  labs(
    title = "Whale Population Growth Rates by Period",
    subtitle = "Median annual growth rate with interquartile range",
    x = "",
    y = "Annual growth rate (%)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10)
  )

ggsave(file.path(output_dir, "whale_recovery_rates_by_period.png"), 
       p_rates, width = 10, height = 7, dpi = 300)
cat("  Saved: whale_recovery_rates_by_period.png\n")

# ------------------------------------------------------------------------------
# PLOT 3: Feeding Levels Through Time
# ------------------------------------------------------------------------------
if (!is.null(feeding_stats)) {
  p_feeding <- ggplot(feeding_stats, aes(x = year)) +
    # Period shading
    annotate("rect", xmin = 1930, xmax = 1960, ymin = -Inf, ymax = Inf, 
             fill = "red", alpha = 0.1) +
    annotate("rect", xmin = 1977, xmax = 1995, ymin = -Inf, ymax = Inf, 
             fill = "orange", alpha = 0.1) +
    # Uncertainty ribbon
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = "purple", alpha = 0.3) +
    # Median line
    geom_line(aes(y = median), color = "purple", linewidth = 1) +
    # Labels
    annotate("text", x = 1945, y = max(feeding_stats$q75, na.rm = TRUE) * 0.98, 
             label = "Whaling Era", color = "darkred", size = 3.5, fontface = "italic") +
    annotate("text", x = 1986, y = max(feeding_stats$q75, na.rm = TRUE) * 0.98, 
             label = "Krill Fishing", color = "darkorange", size = 3.5, fontface = "italic") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    labs(
      title = "Whale Feeding Level Through Time",
      subtitle = paste0("Mean feeding level across whale species | Median with 50% CI (n=", 
                        length(all_whale_feeding), " simulations)"),
      x = "Year",
      y = "Feeding level (% of maximum intake)"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "grey40"),
      axis.title = element_text(size = 12)
    )
  
  ggsave(file.path(output_dir, "whale_feeding_level_timeseries.png"), 
         p_feeding, width = 12, height = 7, dpi = 300)
  cat("  Saved: whale_feeding_level_timeseries.png\n")
}

# ------------------------------------------------------------------------------
# PLOT 4: Krill-Whale Biomass Ratio
# ------------------------------------------------------------------------------
p_ratio <- ggplot(ratio_stats, aes(x = year)) +
  # Period shading
  annotate("rect", xmin = 1930, xmax = 1960, ymin = -Inf, ymax = Inf, 
           fill = "red", alpha = 0.1) +
  annotate("rect", xmin = 1977, xmax = 1995, ymin = -Inf, ymax = Inf, 
           fill = "orange", alpha = 0.1) +
  # Uncertainty ribbon
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "darkgreen", alpha = 0.3) +
  # Median line
  geom_line(aes(y = median), color = "darkgreen", linewidth = 1) +
  # Labels
  annotate("text", x = 1945, y = max(ratio_stats$q75, na.rm = TRUE) * 0.95, 
           label = "Whaling Era", color = "darkred", size = 3.5, fontface = "italic") +
  annotate("text", x = 1986, y = max(ratio_stats$q75, na.rm = TRUE) * 0.95, 
           label = "Krill Fishing", color = "darkorange", size = 3.5, fontface = "italic") +
  labs(
    title = "Krill-to-Whale Biomass Ratio Through Time",
    subtitle = paste0("Higher ratio = more krill per unit whale biomass | Median with 50% CI (n=", n_sims, " simulations)"),
    x = "Year",
    y = "Krill biomass / Whale biomass"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    axis.title = element_text(size = 12)
  )

ggsave(file.path(output_dir, "krill_whale_ratio_timeseries.png"), 
       p_ratio, width = 12, height = 7, dpi = 300)
cat("  Saved: krill_whale_ratio_timeseries.png\n")

# ------------------------------------------------------------------------------
# PLOT 5: Combined Whale and Krill Trajectories
# ------------------------------------------------------------------------------
combined_data <- bind_rows(
  whale_stats %>% mutate(group = "Whales") %>% select(year, relative, relative_q25, relative_q75, group),
  krill_stats %>% mutate(group = "Krill") %>% select(year, relative, relative_q25, relative_q75, group)
)

p_combined <- ggplot(combined_data, aes(x = year, color = group, fill = group)) +
  # Period shading
  annotate("rect", xmin = 1930, xmax = 1960, ymin = -Inf, ymax = Inf, 
           fill = "red", alpha = 0.1) +
  annotate("rect", xmin = 1977, xmax = 1995, ymin = -Inf, ymax = Inf, 
           fill = "orange", alpha = 0.1) +
  # Uncertainty ribbons
  geom_ribbon(aes(ymin = relative_q25, ymax = relative_q75), alpha = 0.2, color = NA) +
  # Median lines
  geom_line(aes(y = relative), linewidth = 1) +
  # Reference line
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("Whales" = "steelblue", "Krill" = "coral")) +
  scale_fill_manual(values = c("Whales" = "steelblue", "Krill" = "coral")) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(
    title = "Whale and Krill Population Dynamics",
    subtitle = "Relative to pre-whaling baseline (1841-1860)",
    x = "Year",
    y = "Biomass (% of pre-whaling)",
    color = "Group",
    fill = "Group"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "grey80"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    axis.title = element_text(size = 12)
  )

ggsave(file.path(output_dir, "whale_krill_combined_trajectory.png"), 
       p_combined, width = 12, height = 7, dpi = 300)
cat("  Saved: whale_krill_combined_trajectory.png\n")

# ------------------------------------------------------------------------------
# Save data
# ------------------------------------------------------------------------------
cat("\nSaving data...\n")

write.csv(whale_stats, file.path(output_dir, "whale_biomass_timeseries.csv"), row.names = FALSE)
write.csv(krill_stats, file.path(output_dir, "krill_biomass_timeseries.csv"), row.names = FALSE)
write.csv(ratio_stats, file.path(output_dir, "krill_whale_ratio_timeseries.csv"), row.names = FALSE)
write.csv(rate_summary, file.path(output_dir, "whale_recovery_rates.csv"), row.names = FALSE)
if (!is.null(feeding_stats)) {
  write.csv(feeding_stats, file.path(output_dir, "whale_feeding_level_timeseries.csv"), row.names = FALSE)
}

cat("  Data files saved\n")

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------
cat("\n=== Analysis Summary ===\n\n")

cat("WHALE RECOVERY RATES:\n")
cat("  Early Recovery (1960-1976, pre-krill fishing):", 
    round(rate_summary$median_pct_per_year[rate_summary$period == "early_recovery"], 2), "% per year\n")
cat("  During Krill Fishing (1977-1995):", 
    round(rate_summary$median_pct_per_year[rate_summary$period == "krill_fishing"], 2), "% per year\n")
cat("  Late Recovery (1996-2010):", 
    round(rate_summary$median_pct_per_year[rate_summary$period == "late_recovery"], 2), "% per year\n")

cat("\nKRILL-WHALE RATIO:\n")
cat("  Pre-whaling:", round(ratio_by_period$median_ratio[1], 2), "\n")
cat("  Peak whaling:", round(ratio_by_period$median_ratio[2], 2), "\n")
cat("  During krill fishing:", round(ratio_by_period$median_ratio[4], 2), "\n")
cat("  Recent (1996-2010):", round(ratio_by_period$median_ratio[5], 2), "\n")

cat("\n=== Analysis Complete ===\n")
cat("Output directory:", output_dir, "\n")
