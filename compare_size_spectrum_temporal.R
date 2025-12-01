# Temporal Comparison of Community Size Spectrum Within MC Ensemble
# Compares different historical periods to the 2001-2010 reference period
# Uses within-simulation comparisons to assess fishing impacts on size structure

library(mizer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

cat("=== Temporal Size Spectrum Comparison (Within-Ensemble) ===\n\n")

# ------------------------------------------------------------------------------
# Define periods for comparison
# ------------------------------------------------------------------------------
# Reference period (denominator in all ratios)
reference_period <- 2001:2010
reference_name <- "Reference (2001-2010)"

# Comparison periods (numerator in ratios)
# Whaling history: first year 1930 (baleen whales)
# Krill fishing history: first year 1974, peak year 1979, last year 1996
# Pre-whaling: decade prior to first whaling (1930)
# Pre-krill: decade prior to first krill fishing (1974)
# Peak krill: ±5 years around peak effort year (1979)
# Post-peak krill: decade after peak
comparison_periods <- list(
  pre_whaling = 1920:1929,
  pre_krill = 1964:1973,
  peak_krill = 1974:1984,
  post_peak_krill = 1985:1995
)

period_labels <- c(
  pre_whaling = "Pre-Whaling (1920-1929)",
  pre_krill = "Pre-Krill Fishing (1964-1973)",
  peak_krill = "Peak Krill Fishing (1974-1984)",
  post_peak_krill = "Post-Peak Krill Fishing (1985-1995)"
)

# Model domain area for density conversion
model_domain_area <- 1.95e+13  # m^2

# ------------------------------------------------------------------------------
# Load MC simulation results
# ------------------------------------------------------------------------------
cat("Loading MC simulation results...\n")

mc_results_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/combined_rerun_successful_sims_20250923_122211.rds"
mc_results <- readRDS(mc_results_file)
cat("  Loaded:", mc_results_file, "\n")

simulations_list <- mc_results$simulations
n_sims <- length(simulations_list)
cat("  Found", n_sims, "simulations\n")

# Get structure from first simulation
first_sim <- simulations_list[[1]]
sim_times <- as.numeric(dimnames(first_sim@n)$time)
w_bins <- first_sim@params@w
n_w <- length(w_bins)
cat("  Time range:", min(sim_times), "-", max(sim_times), "\n")
cat("  Size bins:", n_w, "\n\n")

# ------------------------------------------------------------------------------
# Function to extract community size spectrum for a time period
# ------------------------------------------------------------------------------
extract_community_spectrum <- function(sim, time_range) {
  params <- sim@params
  w <- params@w
  dw <- params@dw
  
  sim_times <- as.numeric(dimnames(sim@n)$time)
  time_idx <- which(sim_times %in% time_range)
  
  if (length(time_idx) == 0) {
    return(NULL)
  }
  
  # Extract n array for the time range: [time, species, size]
  n_subset <- sim@n[time_idx, , , drop = FALSE]
  
  # Sum across species to get community spectrum for each time step
  community_spectrum <- apply(n_subset, c(1, 3), sum)  # [time, size]
  
  # Time-average
  mean_spectrum <- colMeans(community_spectrum)
  
  return(mean_spectrum)
}

# ------------------------------------------------------------------------------
# Extract spectra and calculate ratios for each simulation
# ------------------------------------------------------------------------------
cat("Processing simulations and calculating temporal ratios...\n")

# Storage for all ratios
all_ratios <- list()
for (period_name in names(comparison_periods)) {
  all_ratios[[period_name]] <- matrix(NA, nrow = n_sims, ncol = n_w)
}

# Storage for absolute spectra
reference_spectra <- matrix(NA, nrow = n_sims, ncol = n_w)

pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
skipped <- 0

for (i in seq_len(n_sims)) {
  setTxtProgressBar(pb, i)
  
  sim <- tryCatch({
    simulations_list[[i]]
  }, error = function(e) NULL)
  
  if (is.null(sim)) {
    skipped <- skipped + 1
    next
  }
  
  # Extract reference period spectrum
  ref_spectrum <- tryCatch({
    extract_community_spectrum(sim, reference_period)
  }, error = function(e) NULL)
  
  if (is.null(ref_spectrum) || any(is.na(ref_spectrum)) || all(ref_spectrum == 0)) {
    skipped <- skipped + 1
    next
  }
  
  reference_spectra[i, ] <- ref_spectrum
  
  # Extract comparison period spectra and calculate ratios
  for (period_name in names(comparison_periods)) {
    period_years <- comparison_periods[[period_name]]
    
    comp_spectrum <- tryCatch({
      extract_community_spectrum(sim, period_years)
    }, error = function(e) NULL)
    
    if (!is.null(comp_spectrum)) {
      # Calculate ratio (comparison / reference)
      # Add small value to avoid division by zero
      ratio <- comp_spectrum / (ref_spectrum + 1e-30)
      all_ratios[[period_name]][i, ] <- ratio
    }
  }
}
close(pb)

cat("\n  Processed", n_sims - skipped, "simulations\n")
cat("  Skipped", skipped, "simulations due to errors\n\n")

# ------------------------------------------------------------------------------
# Calculate ensemble statistics for ratios
# ------------------------------------------------------------------------------
cat("Calculating ensemble statistics...\n")

ratio_stats <- list()

for (period_name in names(comparison_periods)) {
  ratio_matrix <- all_ratios[[period_name]]
  
  stats <- data.frame(
    w = w_bins,
    period = period_name,
    period_label = period_labels[period_name],
    ratio_mean = apply(ratio_matrix, 2, mean, na.rm = TRUE),
    ratio_median = apply(ratio_matrix, 2, median, na.rm = TRUE),
    ratio_q05 = apply(ratio_matrix, 2, quantile, 0.05, na.rm = TRUE),
    ratio_q25 = apply(ratio_matrix, 2, quantile, 0.25, na.rm = TRUE),
    ratio_q75 = apply(ratio_matrix, 2, quantile, 0.75, na.rm = TRUE),
    ratio_q95 = apply(ratio_matrix, 2, quantile, 0.95, na.rm = TRUE),
    n_valid = apply(ratio_matrix, 2, function(x) sum(!is.na(x)))
  )
  
  ratio_stats[[period_name]] <- stats
  cat("  Period:", period_name, "- mean ratio range:", 
      round(min(stats$ratio_mean, na.rm = TRUE), 3), "-", 
      round(max(stats$ratio_mean, na.rm = TRUE), 3), "\n")
}

# Combine all periods
all_ratio_stats <- bind_rows(ratio_stats)

# Also calculate reference period absolute spectrum statistics
reference_stats <- data.frame(
  w = w_bins,
  n_mean = apply(reference_spectra, 2, mean, na.rm = TRUE),
  n_median = apply(reference_spectra, 2, median, na.rm = TRUE),
  n_q05 = apply(reference_spectra, 2, quantile, 0.05, na.rm = TRUE),
  n_q95 = apply(reference_spectra, 2, quantile, 0.95, na.rm = TRUE)
)

# ------------------------------------------------------------------------------
# Create output directory
# ------------------------------------------------------------------------------
output_dir <- "fishmip_outputs/temporal_comparison"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ------------------------------------------------------------------------------
# PLOT 1: Ratio plot - All periods relative to reference
# ------------------------------------------------------------------------------
cat("\nCreating plots...\n")

# Color palette for periods
period_colors <- c(
  "Pre-Whaling (1920-1929)" = "#2166AC",
  "Pre-Krill Fishing (1964-1973)" = "#762A83",
  "Peak Krill Fishing (1974-1984)" = "#E66101",
  "Post-Peak Krill Fishing (1985-1995)" = "#1B7837"
)

# Filter to w >= 0.01g for cleaner plots
ratio_plot_data <- all_ratio_stats %>%
  filter(w >= 0.01)

p_ratio <- ggplot(ratio_plot_data, aes(x = w, color = period_label, fill = period_label)) +
  # Uncertainty ribbons - 50th percentile only (IQR)
  geom_ribbon(aes(ymin = ratio_q25, ymax = ratio_q75), alpha = 0.25, color = NA) +
  # Reference line
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  # Median lines
  geom_line(aes(y = ratio_median), linewidth = 1) +
  scale_x_log10(
    labels = function(x) {
      dplyr::case_when(
        x >= 1000000 ~ paste0(x/1000000, " t"),
        TRUE ~ paste0(x, " g")
      )
    },
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000),
    limits = c(0.01, 1e8)
  ) +
  scale_y_log10(
    breaks = c(0.1, 0.3, 0.5, 1, 2, 3, 5, 10, 30, 100),
    labels = c("0.1", "0.3", "0.5", "1", "2", "3", "5", "10", "30", "100")
  ) +
  scale_color_manual(values = period_colors) +
  scale_fill_manual(values = period_colors) +
  coord_cartesian(ylim = c(0.1, 100)) +
  labs(
    title = "Community Size Spectrum: Pre-Exploitation Periods vs Modern Reference",
    subtitle = paste0("Ratio relative to ", reference_name, " | Median with 50% credible interval (n=", n_sims, " simulations)"),
    x = "Body mass",
    y = paste0("Abundance ratio\n(Historical / ", reference_name, ")"),
    color = "Period",
    fill = "Period"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = "grey80"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

ggsave(file.path(output_dir, "size_spectrum_ratio_vs_reference.png"), 
       p_ratio, width = 12, height = 7, dpi = 300)
cat("  Saved: size_spectrum_ratio_vs_reference.png\n")

# ------------------------------------------------------------------------------
# PLOT 2: Faceted panel - Pre-whaling and Pre-krill only
# ------------------------------------------------------------------------------
# Filter to just pre-whaling and pre-krill for the original faceted plot
facet_data <- all_ratio_stats %>%
  filter(period %in% c("pre_whaling", "pre_krill")) %>%
  filter(w >= 0.01) %>%
  mutate(period_label = factor(period_label, levels = c("Pre-Whaling (1920-1929)", "Pre-Krill Fishing (1964-1973)")))

p_facet <- ggplot(facet_data, aes(x = w)) +
  geom_ribbon(aes(ymin = ratio_q25, ymax = ratio_q75), fill = "steelblue", alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_line(aes(y = ratio_median), color = "steelblue", linewidth = 1) +
  facet_wrap(~period_label, ncol = 2) +
  scale_x_log10(
    labels = function(x) {
      dplyr::case_when(
        x >= 1000000 ~ paste0(x/1000000, " t"),
        TRUE ~ paste0(x, " g")
      )
    },
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000),
    limits = c(0.01, 1e8)
  ) +
  scale_y_log10(
    breaks = c(0.05, 0.1, 0.3, 1, 3, 10, 30, 100),
    labels = c("0.05", "0.1", "0.3", "1", "3", "10", "30", "100")
  ) +
  coord_cartesian(ylim = c(0.05, 100)) +
  labs(
    title = "Community Size Spectrum: Pre-Exploitation vs Modern Recovery",
    subtitle = paste0("Each panel shows ratio relative to ", reference_name),
    x = "Body mass",
    y = "Abundance ratio"
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "grey95"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

ggsave(file.path(output_dir, "size_spectrum_ratio_faceted.png"), 
       p_facet, width = 14, height = 6, dpi = 300)
cat("  Saved: size_spectrum_ratio_faceted.png\n")

# ------------------------------------------------------------------------------
# PLOT 2a-zoom: Faceted panel zoomed to whale sizes (>= 10 kg)
# ------------------------------------------------------------------------------
facet_data_whales <- all_ratio_stats %>%
  filter(period %in% c("pre_whaling", "pre_krill")) %>%
  filter(w >= 10000) %>%  # 10 kg = 10000 g
  mutate(period_label = factor(period_label, levels = c("Pre-Whaling (1920-1929)", "Pre-Krill Fishing (1964-1973)")))

p_facet_whales <- ggplot(facet_data_whales, aes(x = w)) +
  geom_ribbon(aes(ymin = ratio_q25, ymax = ratio_q75), fill = "steelblue", alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_line(aes(y = ratio_median), color = "steelblue", linewidth = 1.2) +
  facet_wrap(~period_label, ncol = 2) +
  scale_x_log10(
    labels = function(x) {
      dplyr::case_when(
        x >= 1000000 ~ paste0(x/1000000, " t"),
        TRUE ~ paste0(x/1000, " kg")
      )
    },
    breaks = c(10000, 100000, 1000000, 10000000, 100000000),
    limits = c(10000, 1e8)
  ) +
  scale_y_log10(
    breaks = c(0.1, 0.3, 1, 3, 10, 30, 100),
    labels = c("0.1", "0.3", "1", "3", "10", "30", "100")
  ) +
  coord_cartesian(ylim = c(0.1, 100)) +
  labs(
    title = "Large Organism Size Spectrum: Pre-Exploitation vs Modern Recovery",
    subtitle = paste0("Zoomed to \u2265 10 kg | Each panel shows ratio relative to ", reference_name),
    x = "Body mass",
    y = "Abundance ratio"
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "grey95"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

ggsave(file.path(output_dir, "size_spectrum_ratio_faceted_whales.png"), 
       p_facet_whales, width = 14, height = 6, dpi = 300)
cat("  Saved: size_spectrum_ratio_faceted_whales.png\n")

# ------------------------------------------------------------------------------
# PLOT 2b: Whale Recovery Trajectory - Size spectrum during krill fishing periods
# ------------------------------------------------------------------------------
cat("  Creating whale recovery trajectory plot...\\n")

# Filter to whale recovery periods only
recovery_data <- all_ratio_stats %>%
  filter(period %in% c("pre_krill", "peak_krill", "post_peak_krill")) %>%
  filter(w >= 0.01) %>%
  mutate(period_label = factor(period_label, levels = c(
    "Pre-Krill Fishing (1964-1973)",
    "Peak Krill Fishing (1974-1984)",
    "Post-Peak Krill Fishing (1985-1995)"
  )))

p_recovery <- ggplot(recovery_data, aes(x = w, color = period_label, fill = period_label)) +
  # Uncertainty ribbons - 50th percentile only (IQR)
  geom_ribbon(aes(ymin = ratio_q25, ymax = ratio_q75), alpha = 0.2, color = NA) +
  # Reference line
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  # Median lines
  geom_line(aes(y = ratio_median), linewidth = 1) +
  scale_x_log10(
    labels = function(x) {
      dplyr::case_when(
        x >= 1000000 ~ paste0(x/1000000, " t"),
        TRUE ~ paste0(x, " g")
      )
    },
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000),
    limits = c(0.01, 1e8)
  ) +
  scale_y_log10(
    breaks = c(0.1, 0.3, 0.5, 1, 2, 3, 5, 10),
    labels = c("0.1", "0.3", "0.5", "1", "2", "3", "5", "10")
  ) +
  scale_color_manual(values = c(
    "Pre-Krill Fishing (1964-1973)" = "#762A83",
    "Peak Krill Fishing (1974-1984)" = "#E66101",
    "Post-Peak Krill Fishing (1985-1995)" = "#1B7837"
  )) +
  scale_fill_manual(values = c(
    "Pre-Krill Fishing (1964-1973)" = "#762A83",
    "Peak Krill Fishing (1974-1984)" = "#E66101",
    "Post-Peak Krill Fishing (1985-1995)" = "#1B7837"
  )) +
  coord_cartesian(ylim = c(0.1, 10)) +
  labs(
    title = "Whale Recovery and Krill Fishing: Size Spectrum Trajectory",
    subtitle = paste0("Ratio relative to ", reference_name, " | Shows how size spectrum changed during whale recovery"),
    x = "Body mass",
    y = paste0("Abundance ratio (Period / ", reference_name, ")"),
    color = "Period",
    fill = "Period"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.9), color = "grey80"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

ggsave(file.path(output_dir, "size_spectrum_whale_recovery_trajectory.png"), 
       p_recovery, width = 12, height = 7, dpi = 300)
cat("  Saved: size_spectrum_whale_recovery_trajectory.png\n")

# ------------------------------------------------------------------------------
# PLOT 2b-zoom: Whale Recovery Trajectory zoomed to whale sizes (>= 10 kg)
# ------------------------------------------------------------------------------
recovery_data_whales <- all_ratio_stats %>%
  filter(period %in% c("pre_krill", "peak_krill", "post_peak_krill")) %>%
  filter(w >= 10000) %>%  # 10 kg = 10000 g
  mutate(period_label = factor(period_label, levels = c(
    "Pre-Krill Fishing (1964-1973)",
    "Peak Krill Fishing (1974-1984)",
    "Post-Peak Krill Fishing (1985-1995)"
  )))

p_recovery_whales <- ggplot(recovery_data_whales, aes(x = w, color = period_label, fill = period_label)) +
  geom_ribbon(aes(ymin = ratio_q25, ymax = ratio_q75), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_line(aes(y = ratio_median), linewidth = 1.2) +
  scale_x_log10(
    labels = function(x) {
      dplyr::case_when(
        x >= 1000000 ~ paste0(x/1000000, " t"),
        TRUE ~ paste0(x/1000, " kg")
      )
    },
    breaks = c(10000, 100000, 1000000, 10000000, 100000000),
    limits = c(10000, 1e8)
  ) +
  scale_y_log10(
    breaks = c(0.3, 0.5, 1, 2, 3, 5, 10),
    labels = c("0.3", "0.5", "1", "2", "3", "5", "10")
  ) +
  scale_color_manual(values = c(
    "Pre-Krill Fishing (1964-1973)" = "#762A83",
    "Peak Krill Fishing (1974-1984)" = "#E66101",
    "Post-Peak Krill Fishing (1985-1995)" = "#1B7837"
  )) +
  scale_fill_manual(values = c(
    "Pre-Krill Fishing (1964-1973)" = "#762A83",
    "Peak Krill Fishing (1974-1984)" = "#E66101",
    "Post-Peak Krill Fishing (1985-1995)" = "#1B7837"
  )) +
  coord_cartesian(ylim = c(0.3, 10)) +
  labs(
    title = "Whale Recovery and Krill Fishing: Large Organism Trajectory",
    subtitle = paste0("Zoomed to \u2265 10 kg | Ratio relative to ", reference_name),
    x = "Body mass",
    y = paste0("Abundance ratio (Period / ", reference_name, ")"),
    color = "Period",
    fill = "Period"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.9), color = "grey80"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

ggsave(file.path(output_dir, "size_spectrum_whale_recovery_trajectory_whales.png"), 
       p_recovery_whales, width = 12, height = 7, dpi = 300)
cat("  Saved: size_spectrum_whale_recovery_trajectory_whales.png\n")

# ------------------------------------------------------------------------------
# PLOT 3: Absolute spectra for reference period
# ------------------------------------------------------------------------------
p_reference <- ggplot(reference_stats, aes(x = w)) +
  geom_ribbon(aes(ymin = n_q05, ymax = n_q95), fill = "forestgreen", alpha = 0.3) +
  geom_line(aes(y = n_median), color = "forestgreen", linewidth = 1) +
  scale_x_log10(
    labels = function(x) ifelse(x >= 1000000, paste0(x/1000000, " t"), paste0(x, " g")),
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000)
  ) +
  scale_y_log10(labels = scientific) +
  labs(
    title = paste0("Reference Period Community Size Spectrum (", reference_name, ")"),
    subtitle = paste0("Median with 90% credible interval (n=", n_sims, " simulations)"),
    x = "Body mass",
    y = "Abundance (numbers per size bin)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    axis.title = element_text(size = 12)
  )

ggsave(file.path(output_dir, "size_spectrum_reference_absolute.png"), 
       p_reference, width = 10, height = 7, dpi = 300)
cat("  Saved: size_spectrum_reference_absolute.png\n")

# ------------------------------------------------------------------------------
# Save data
# ------------------------------------------------------------------------------
cat("\nSaving data...\n")

write.csv(all_ratio_stats, file.path(output_dir, "temporal_ratio_stats.csv"), row.names = FALSE)
cat("  Saved: temporal_ratio_stats.csv\n")

write.csv(reference_stats, file.path(output_dir, "reference_spectrum_stats.csv"), row.names = FALSE)
cat("  Saved: reference_spectrum_stats.csv\n")

# Summary statistics
summary_stats <- all_ratio_stats %>%
  group_by(period, period_label) %>%
  summarise(
    median_ratio_overall = median(ratio_median, na.rm = TRUE),
    mean_ratio_overall = mean(ratio_median, na.rm = TRUE),
    pct_above_1 = mean(ratio_median > 1, na.rm = TRUE) * 100,
    pct_below_1 = mean(ratio_median < 1, na.rm = TRUE) * 100,
    max_ratio = max(ratio_median, na.rm = TRUE),
    min_ratio = min(ratio_median, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== Summary Statistics ===\n")
print(summary_stats)

write.csv(summary_stats, file.path(output_dir, "temporal_ratio_summary.csv"), row.names = FALSE)
cat("\n  Saved: temporal_ratio_summary.csv\n")

# ------------------------------------------------------------------------------
# Marine mammal size class analysis
# ------------------------------------------------------------------------------
cat("\n=== Marine Mammal Size Class Analysis ===\n")

# Define marine mammal size range (approximately 50 kg to 100,000 kg = 50,000 g to 100,000,000 g)
# This covers seals (~50-500 kg), small cetaceans, and great whales
mammal_min_mass <- 50 * 1000   # 50 kg in grams
mammal_max_mass <- 100000 * 1000  # 100 tonnes in grams

# Identify size bins that fall within marine mammal range
mammal_bins <- which(w_bins >= mammal_min_mass & w_bins <= mammal_max_mass)
cat("  Marine mammal size range:", mammal_min_mass/1000, "kg to", mammal_max_mass/1000, "kg\n")
cat("  Number of size bins in range:", length(mammal_bins), "\n")
cat("  Actual mass range covered:", round(min(w_bins[mammal_bins])/1000, 1), "kg to", 
    round(max(w_bins[mammal_bins])/1000, 1), "kg\n\n")

# Calculate % of marine mammal size bins where historical > reference
mammal_stats <- all_ratio_stats %>%
  filter(w >= mammal_min_mass & w <= mammal_max_mass) %>%
  group_by(period, period_label) %>%
  summarise(
    n_bins = n(),
    pct_above_1 = mean(ratio_median > 1, na.rm = TRUE) * 100,
    pct_below_1 = mean(ratio_median < 1, na.rm = TRUE) * 100,
    mean_ratio = mean(ratio_median, na.rm = TRUE),
    median_ratio = median(ratio_median, na.rm = TRUE),
    .groups = "drop"
  )

cat("Marine Mammal Size Class Results:\n")
cat("(Size bins from", round(mammal_min_mass/1000), "kg to", round(mammal_max_mass/1000), "kg)\n\n")
print(mammal_stats)

# Save marine mammal stats
write.csv(mammal_stats, file.path(output_dir, "marine_mammal_size_class_stats.csv"), row.names = FALSE)
cat("\n  Saved: marine_mammal_size_class_stats.csv\n")

# Also analyze by finer size categories
cat("\n=== Size Category Breakdown ===\n")

size_categories <- list(
  "Zooplankton (< 1g)" = c(0, 1),
  "Small organisms (1g - 100g)" = c(1, 100),
  "Medium organisms (100g - 10kg)" = c(100, 10000),
  "Large organisms (10kg - 100kg)" = c(10000, 100000),
  "Seals & small cetaceans (100kg - 1t)" = c(100000, 1000000),
  "Medium cetaceans (1t - 10t)" = c(1000000, 10000000),
  "Great whales (> 10t)" = c(10000000, Inf)
)

size_category_stats <- list()

for (cat_name in names(size_categories)) {
  range <- size_categories[[cat_name]]
  
  cat_stats <- all_ratio_stats %>%
    filter(w >= range[1] & w < range[2]) %>%
    group_by(period, period_label) %>%
    summarise(
      n_bins = n(),
      pct_above_1 = mean(ratio_median > 1, na.rm = TRUE) * 100,
      mean_ratio = mean(ratio_median, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(size_category = cat_name)
  
  size_category_stats[[cat_name]] <- cat_stats
}

size_category_combined <- bind_rows(size_category_stats)

# Pivot wider for easier reading
size_category_wide <- size_category_combined %>%
  select(size_category, period_label, pct_above_1, mean_ratio) %>%
  pivot_wider(
    names_from = period_label,
    values_from = c(pct_above_1, mean_ratio),
    names_sep = " - "
  )

cat("\nPercentage of size bins where historical abundance > modern reference:\n\n")
print(size_category_combined %>% 
        select(size_category, period_label, n_bins, pct_above_1, mean_ratio) %>%
        arrange(desc(size_category == "Great whales (> 10t)")))

write.csv(size_category_combined, file.path(output_dir, "size_category_stats.csv"), row.names = FALSE)
cat("\n  Saved: size_category_stats.csv\n")

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------
cat("\n=== Analysis Complete ===\n")
cat("Output directory:", output_dir, "\n")
cat("\nPlots created:\n")
cat("  1. size_spectrum_ratio_vs_reference.png - Both periods with 50% credible interval\n")
cat("  2. size_spectrum_ratio_faceted.png - Faceted panel view\n")
cat("  3. size_spectrum_reference_absolute.png - Reference period absolute spectrum\n")
cat("\nInterpretation:\n")
cat("  - Ratio > 1: Higher abundance in historical period than reference (recovery incomplete)\n")
cat("  - Ratio < 1: Lower abundance in historical period than reference (recovery achieved)\n")
cat("  - Uncertainty bands show ensemble variation (50% credible interval)\n")
