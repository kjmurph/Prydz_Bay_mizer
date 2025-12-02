# Compare Fished vs Unfished Community Size Spectrum
# Preliminary analysis using completed climate-only simulations
# This script compares matched pairs of fished and unfished simulations

library(mizer)
library(ggplot2)
library(dplyr)
library(tidyr)

cat("=== Fished vs Unfished Size Spectrum Comparison ===\n\n")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
output_dir <- "fishmip_outputs/fished_vs_unfished"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Time periods for comparison
periods <- list(
  "Pre-Whaling (1920-1929)" = 1920:1929,
  "Pre-Krill (1964-1973)" = 1964:1973,
  "Peak Krill (1974-1984)" = 1974:1984,
  "Post-Peak (1985-1995)" = 1985:1995,
  "Reference (2001-2010)" = 2001:2010
)

# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------
cat("Loading data...\n")

# Load fished ensemble
mc_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
mc_results <- readRDS(mc_file)
fished_sims <- mc_results$simulations
cat("  Loaded fished ensemble:", length(fished_sims), "simulations\n")

# Load unfished (climate-only) simulations
unfished_dir <- "Output_large_files/climate_only_ensemble/individual_sims"
unfished_files <- list.files(unfished_dir, pattern = "\\.rds$", full.names = TRUE)
cat("  Found unfished simulations:", length(unfished_files), "\n")

# Extract indices from filenames
get_idx <- function(f) {
  as.integer(gsub(".*sim_(\\d+)_climate_only\\.rds", "\\1", basename(f)))
}
unfished_indices <- sapply(unfished_files, get_idx)
names(unfished_files) <- unfished_indices

cat("  Matched pairs available:", length(unfished_indices), "\n\n")

# ------------------------------------------------------------------------------
# Helper function: Get community spectrum from simulation
# ------------------------------------------------------------------------------
get_community_spectrum <- function(sim, years) {
  times <- as.numeric(dimnames(sim@n)$time)
  year_idx <- which(times %in% years)
  
  if (length(year_idx) == 0) return(NULL)
  
  # Get size bins
  w <- sim@params@w
  
  # Sum across species and average across years
  # n has dimensions [time, species, size]
  n_subset <- sim@n[year_idx, , , drop = FALSE]
  
  # Sum across species for each time and size
  community_n <- apply(n_subset, c(1, 3), sum)
  
  # Average across years
  mean_n <- colMeans(community_n)
  
  data.frame(
    w = w,
    abundance = mean_n,
    log_w = log10(w),
    log_abundance = log10(mean_n + 1e-30)
  )
}

# ------------------------------------------------------------------------------
# Calculate size spectra for matched pairs
# ------------------------------------------------------------------------------
cat("Calculating size spectra for matched pairs...\n")

# Use reference period for main comparison
ref_years <- 2001:2010

fished_spectra <- list()
unfished_spectra <- list()

pb <- txtProgressBar(min = 0, max = length(unfished_indices), style = 3)

for (i in seq_along(unfished_indices)) {
  idx <- unfished_indices[i]
  
  tryCatch({
    # Get fished simulation
    fished_sim <- fished_sims[[idx]]
    if (is.list(fished_sim) && !inherits(fished_sim, "MizerSim")) {
      fished_sim <- fished_sim[[1]]
    }
    
    # Get unfished simulation
    unfished_sim <- readRDS(unfished_files[as.character(idx)])
    
    # Calculate spectra
    fished_spec <- get_community_spectrum(fished_sim, ref_years)
    unfished_spec <- get_community_spectrum(unfished_sim, ref_years)
    
    if (!is.null(fished_spec) && !is.null(unfished_spec)) {
      fished_spectra[[as.character(idx)]] <- fished_spec
      unfished_spectra[[as.character(idx)]] <- unfished_spec
    }
    
  }, error = function(e) {
    # Skip failed simulations
  })
  
  setTxtProgressBar(pb, i)
}
close(pb)

cat("\nSuccessfully processed:", length(fished_spectra), "matched pairs\n")

# ------------------------------------------------------------------------------
# Aggregate spectra across simulations
# ------------------------------------------------------------------------------
cat("\nAggregating spectra...\n")

# Combine all fished spectra
fished_df <- bind_rows(lapply(names(fished_spectra), function(idx) {
  fished_spectra[[idx]] %>% mutate(sim_idx = idx, scenario = "Fished")
}))

# Combine all unfished spectra
unfished_df <- bind_rows(lapply(names(unfished_spectra), function(idx) {
  unfished_spectra[[idx]] %>% mutate(sim_idx = idx, scenario = "Unfished")
}))

# Combine both
all_spectra <- bind_rows(fished_df, unfished_df)

# Calculate summary statistics
spectrum_summary <- all_spectra %>%
  group_by(scenario, w, log_w) %>%
  summarise(
    mean_abundance = mean(abundance, na.rm = TRUE),
    median_abundance = median(abundance, na.rm = TRUE),
    sd_abundance = sd(abundance, na.rm = TRUE),
    q05 = quantile(abundance, 0.05, na.rm = TRUE),
    q25 = quantile(abundance, 0.25, na.rm = TRUE),
    q75 = quantile(abundance, 0.75, na.rm = TRUE),
    q95 = quantile(abundance, 0.95, na.rm = TRUE),
    n_sims = n(),
    .groups = "drop"
  ) %>%
  mutate(
    log_mean = log10(mean_abundance + 1e-30),
    log_median = log10(median_abundance + 1e-30),
    log_q05 = log10(q05 + 1e-30),
    log_q25 = log10(q25 + 1e-30),
    log_q75 = log10(q75 + 1e-30),
    log_q95 = log10(q95 + 1e-30)
  )

# ------------------------------------------------------------------------------
# Calculate fishing impact ratio
# ------------------------------------------------------------------------------
cat("Calculating fishing impact...\n")

# Pivot to calculate ratio
ratio_df <- spectrum_summary %>%
  select(scenario, w, log_w, mean_abundance) %>%
  pivot_wider(names_from = scenario, values_from = mean_abundance) %>%
  mutate(
    ratio = Fished / Unfished,
    log_ratio = log10(ratio),
    pct_change = (Fished - Unfished) / Unfished * 100
  )

# Size categories
ratio_df <- ratio_df %>%
  mutate(
    size_category = case_when(
      w < 1 ~ "Small (<1g)",
      w < 1000 ~ "Medium (1g-1kg)",
      w < 1e6 ~ "Large (1kg-1t)",
      TRUE ~ "Very Large (>1t)"
    ),
    size_category = factor(size_category, levels = c("Small (<1g)", "Medium (1g-1kg)", 
                                                      "Large (1kg-1t)", "Very Large (>1t)"))
  )

# ------------------------------------------------------------------------------
# Create plots
# ------------------------------------------------------------------------------
cat("Creating plots...\n")

# Custom theme
theme_spectrum <- theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  )

# Size labels for x-axis
size_breaks <- c(1e-3, 1e-1, 1e1, 1e3, 1e5, 1e7)
size_labels <- c("1 mg", "100 mg", "10 g", "1 kg", "100 kg", "10 t")

# Plot 1: Fished vs Unfished comparison with uncertainty
p1 <- ggplot(spectrum_summary, aes(x = w, y = mean_abundance, color = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.3, color = NA) +
  geom_line(size = 1) +
  scale_x_log10(breaks = size_breaks, labels = size_labels) +
  scale_y_log10() +
  scale_color_manual(values = c("Fished" = "#E41A1C", "Unfished" = "#377EB8")) +
  scale_fill_manual(values = c("Fished" = "#E41A1C", "Unfished" = "#377EB8")) +
  labs(
    title = "Community Size Spectrum: Fished vs Unfished",
    subtitle = paste0("Reference period 2001-2010 | ", length(fished_spectra), " matched simulation pairs"),
    x = "Body mass",
    y = expression("Abundance (individuals m"^-2*")"),
    caption = "Shaded region: interquartile range across simulations"
  ) +
  theme_spectrum +
  annotation_logticks(sides = "bl")

ggsave(file.path(output_dir, "size_spectrum_fished_vs_unfished.png"), p1, 
       width = 10, height = 7, dpi = 300)
cat("  Saved: size_spectrum_fished_vs_unfished.png\n")

# Plot 2: Ratio plot (fishing impact)
p2 <- ggplot(ratio_df %>% filter(is.finite(log_ratio)), aes(x = w, y = ratio)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", size = 0.8) +
  geom_line(size = 1, color = "#E41A1C") +
  geom_area(alpha = 0.2, fill = "#E41A1C") +
  scale_x_log10(breaks = size_breaks, labels = size_labels) +
  scale_y_log10() +
  labs(
    title = "Fishing Impact on Community Size Spectrum",
    subtitle = paste0("Ratio of Fished to Unfished abundance | ", length(fished_spectra), " matched pairs"),
    x = "Body mass",
    y = "Fished / Unfished ratio",
    caption = "Values <1 indicate depletion by fishing; >1 indicates increase"
  ) +
  theme_spectrum +
  annotation_logticks(sides = "bl")

ggsave(file.path(output_dir, "fishing_impact_ratio.png"), p2, 
       width = 10, height = 7, dpi = 300)
cat("  Saved: fishing_impact_ratio.png\n")

# Plot 3: Percent change by size
p3 <- ggplot(ratio_df %>% filter(is.finite(pct_change) & abs(pct_change) < 500), 
             aes(x = w, y = pct_change)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", size = 0.8) +
  geom_area(aes(fill = pct_change > 0), alpha = 0.4) +
  geom_line(size = 1, color = "black") +
  scale_x_log10(breaks = size_breaks, labels = size_labels) +
  scale_fill_manual(values = c("TRUE" = "#4DAF4A", "FALSE" = "#E41A1C"), guide = "none") +
  labs(
    title = "Percent Change in Abundance Due to Fishing",
    subtitle = paste0("(Fished - Unfished) / Unfished × 100 | ", length(fished_spectra), " matched pairs"),
    x = "Body mass",
    y = "Percent change (%)",
    caption = "Negative = depleted by fishing; Positive = increased (e.g., prey release)"
  ) +
  theme_spectrum +
  annotation_logticks(sides = "b")

ggsave(file.path(output_dir, "fishing_impact_percent_change.png"), p3, 
       width = 10, height = 7, dpi = 300)
cat("  Saved: fishing_impact_percent_change.png\n")

# Plot 4: Summary by size category
size_summary <- ratio_df %>%
  filter(is.finite(ratio)) %>%
  group_by(size_category) %>%
  summarise(
    mean_ratio = mean(ratio, na.rm = TRUE),
    median_ratio = median(ratio, na.rm = TRUE),
    mean_pct_change = mean(pct_change, na.rm = TRUE),
    .groups = "drop"
  )

p4 <- ggplot(size_summary, aes(x = size_category, y = mean_ratio, fill = size_category)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", size = 0.8) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = sprintf("%.2f", mean_ratio)), vjust = -0.5, size = 4) +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Mean Fishing Impact by Size Category",
    subtitle = paste0("Fished / Unfished ratio | ", length(fished_spectra), " matched pairs"),
    x = "Size category",
    y = "Mean ratio (Fished / Unfished)"
  ) +
  theme_spectrum +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave(file.path(output_dir, "fishing_impact_by_size_category.png"), p4, 
       width = 9, height = 7, dpi = 300)
cat("  Saved: fishing_impact_by_size_category.png\n")

# ------------------------------------------------------------------------------
# Print summary statistics
# ------------------------------------------------------------------------------
cat("\n=== Summary Statistics ===\n\n")

cat("Fishing impact by size category (Fished/Unfished ratio):\n")
print(size_summary)

# Large organism impact
large_impact <- ratio_df %>%
  filter(w >= 1e6) %>%  # >1 tonne
  summarise(
    mean_ratio = mean(ratio, na.rm = TRUE),
    min_ratio = min(ratio, na.rm = TRUE),
    max_ratio = max(ratio, na.rm = TRUE)
  )

cat("\nLarge organisms (>1 tonne):\n")
cat("  Mean ratio:", round(large_impact$mean_ratio, 3), "\n")
cat("  This means fishing reduced large organism abundance to", 
    round(large_impact$mean_ratio * 100, 1), "% of unfished levels\n")

cat("\n=== Preliminary Analysis Complete ===\n")
cat("Plots saved to:", output_dir, "\n")
cat("\nNote: This analysis uses", length(fished_spectra), "of 2111 simulations.\n")
cat("Run the full sequential processing to get complete results.\n")
