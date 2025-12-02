# Compare Fished vs Unfished Size Spectrum Across Multiple Periods
# Shows fishing impact ratio by size category for key historical periods

library(mizer)
library(ggplot2)
library(dplyr)
library(tidyr)

cat("=== Fished vs Unfished Size Spectrum: Multi-Period Comparison ===\n\n")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
output_dir <- "fishmip_outputs/fished_vs_unfished"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Key periods for comparison
periods <- list(
  "Pre-Whaling\n(1920-1929)" = 1920:1929,           # Before whaling started (1930)
  "Peak Whaling\n(1955-1965)" = 1955:1965,          # Peak baleen/sperm whale catches
  "Pre-Krill\n(1964-1973)" = 1964:1973,             # Before krill fishing (1974)
  "Peak Krill\n(1974-1984)" = 1974:1984,            # Peak krill fishing around 1979
  "Post-Krill\n(1985-1995)" = 1985:1995,            # After peak krill fishing
  "Reference\n(2001-2010)" = 2001:2010              # Modern reference period
)

# Size categories with clearer labels
size_breaks <- c(0, 1, 1000, 1e6, Inf)
size_labels <- c("Small\n(<1g)", "Medium\n(1g-1kg)", "Large\n(1kg-1t)", "Very Large\n(>1t)")

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
  
  w <- sim@params@w
  n_subset <- sim@n[year_idx, , , drop = FALSE]
  community_n <- apply(n_subset, c(1, 3), sum)
  mean_n <- colMeans(community_n)
  
  data.frame(w = w, abundance = mean_n)
}

# ------------------------------------------------------------------------------
# Calculate ratios for each period
# ------------------------------------------------------------------------------
cat("Calculating fishing impact ratios for each period...\n\n")

all_results <- list()

for (period_name in names(periods)) {
  period_years <- periods[[period_name]]
  cat("Processing:", gsub("\n", " ", period_name), "...\n")
  
  ratios_by_sim <- list()
  
  for (idx in unfished_indices) {
    tryCatch({
      # Get fished simulation
      fished_sim <- fished_sims[[idx]]
      if (is.list(fished_sim) && !inherits(fished_sim, "MizerSim")) {
        fished_sim <- fished_sim[[1]]
      }
      
      # Get unfished simulation
      unfished_sim <- readRDS(unfished_files[as.character(idx)])
      
      # Calculate spectra
      fished_spec <- get_community_spectrum(fished_sim, period_years)
      unfished_spec <- get_community_spectrum(unfished_sim, period_years)
      
      if (!is.null(fished_spec) && !is.null(unfished_spec)) {
        # Combine and calculate ratio
        combined <- data.frame(
          w = fished_spec$w,
          fished = fished_spec$abundance,
          unfished = unfished_spec$abundance
        ) %>%
          mutate(
            ratio = fished / unfished,
            size_category = cut(w, breaks = size_breaks, labels = size_labels, right = FALSE)
          )
        
        # Summarise by size category
        size_summary <- combined %>%
          group_by(size_category) %>%
          summarise(
            mean_ratio = mean(ratio, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(sim_idx = idx)
        
        ratios_by_sim[[as.character(idx)]] <- size_summary
      }
    }, error = function(e) {
      # Skip failed simulations
    })
  }
  
  # Combine all simulations for this period
  if (length(ratios_by_sim) > 0) {
    period_df <- bind_rows(ratios_by_sim) %>%
      mutate(period = period_name)
    all_results[[period_name]] <- period_df
  }
  
  cat("  Processed", length(ratios_by_sim), "simulation pairs\n")
}

# Combine all periods
all_ratios <- bind_rows(all_results)

# ------------------------------------------------------------------------------
# Calculate summary statistics
# ------------------------------------------------------------------------------
cat("\nCalculating summary statistics...\n")

ratio_summary <- all_ratios %>%
  group_by(period, size_category) %>%
  summarise(
    mean_ratio = mean(mean_ratio, na.rm = TRUE),
    median_ratio = median(mean_ratio, na.rm = TRUE),
    sd_ratio = sd(mean_ratio, na.rm = TRUE),
    q25 = quantile(mean_ratio, 0.25, na.rm = TRUE),
    q75 = quantile(mean_ratio, 0.75, na.rm = TRUE),
    n_sims = n(),
    .groups = "drop"
  ) %>%
  mutate(
    # Order periods chronologically
    period = factor(period, levels = names(periods)),
    # Calculate percent change
    pct_change = (mean_ratio - 1) * 100
  )

# ------------------------------------------------------------------------------
# Create plots
# ------------------------------------------------------------------------------
cat("Creating plots...\n")

# Custom theme
theme_comparison <- theme_bw() +
  theme(
    text = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 9),
    legend.position = "right",
    legend.title = element_text(size = 10),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 10, face = "bold")
  )

# Plot 1: Heatmap of ratios by period and size category
p1 <- ggplot(ratio_summary, aes(x = period, y = size_category, fill = mean_ratio)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", mean_ratio)), color = "black", size = 4) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B", 
    midpoint = 1, 
    limits = c(0.5, 1.5),
    oob = scales::squish,
    name = "Fished/Unfished\nRatio"
  ) +
  labs(
    title = "Fishing Impact on Community Size Spectrum Across Time",
    subtitle = paste0("Ratio of Fished to Unfished abundance | ", 
                      length(unfished_indices), " matched simulation pairs"),
    x = NULL,
    y = "Size Category",
    caption = "Blue (<1) = depletion by fishing | White (=1) = no change | Red (>1) = increase"
  ) +
  theme_comparison +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, lineheight = 0.9))

ggsave(file.path(output_dir, "fishing_impact_heatmap_periods.png"), p1, 
       width = 12, height = 6, dpi = 300)
cat("  Saved: fishing_impact_heatmap_periods.png\n")

# Plot 2: Bar chart by period with error bars
p2 <- ggplot(ratio_summary, aes(x = size_category, y = mean_ratio, fill = size_category)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0.2, color = "gray30") +
  geom_text(aes(label = sprintf("%.2f", mean_ratio)), vjust = -0.5, size = 3) +
  facet_wrap(~period, nrow = 1) +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  scale_y_continuous(limits = c(0, 1.6), breaks = seq(0, 1.5, 0.5)) +
  labs(
    title = "Fishing Impact by Size Category Across Historical Periods",
    subtitle = paste0("Mean ratio (Fished/Unfished) with interquartile range | ", 
                      length(unfished_indices), " matched pairs"),
    x = "Size Category",
    y = "Fished / Unfished Ratio",
    caption = "Dashed line = no fishing impact (ratio = 1)"
  ) +
  theme_comparison +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(size = 8)
  )

ggsave(file.path(output_dir, "fishing_impact_bars_by_period.png"), p2, 
       width = 14, height = 6, dpi = 300)
cat("  Saved: fishing_impact_bars_by_period.png\n")

# Plot 3: Line plot showing temporal trajectory by size category
p3 <- ggplot(ratio_summary, aes(x = period, y = mean_ratio, color = size_category, group = size_category)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = size_category), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_viridis_d(option = "plasma", name = "Size Category") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Temporal Trajectory of Fishing Impacts by Size Category",
    subtitle = paste0("Ratio of Fished to Unfished abundance | ", 
                      length(unfished_indices), " matched pairs"),
    x = NULL,
    y = "Fished / Unfished Ratio",
    caption = "Shaded region: interquartile range | Dashed line = no fishing impact"
  ) +
  theme_comparison +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(output_dir, "fishing_impact_trajectory_by_size.png"), p3, 
       width = 11, height = 7, dpi = 300)
cat("  Saved: fishing_impact_trajectory_by_size.png\n")

# Plot 4: Focus on Very Large organisms (>1t) - whale-size
very_large_trajectory <- ratio_summary %>%
  filter(size_category == "Very Large\n(>1t)")

p4 <- ggplot(very_large_trajectory, aes(x = period, y = mean_ratio, group = 1)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#E41A1C", alpha = 0.3) +
  geom_line(color = "#E41A1C", linewidth = 1.5) +
  geom_point(color = "#E41A1C", size = 4) +
  geom_text(aes(label = sprintf("%.2f", mean_ratio)), vjust = -1.5, size = 4, fontface = "bold") +
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1.2, 0.2)) +
  labs(
    title = "Fishing Impact on Very Large Organisms (>1 tonne)",
    subtitle = paste0("Whales and large fish | ", length(unfished_indices), " matched simulation pairs"),
    x = NULL,
    y = "Fished / Unfished Ratio",
    caption = "Values <1 indicate depletion due to fishing | Shaded: interquartile range"
  ) +
  theme_comparison +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 11))

ggsave(file.path(output_dir, "fishing_impact_very_large_trajectory.png"), p4, 
       width = 10, height = 7, dpi = 300)
cat("  Saved: fishing_impact_very_large_trajectory.png\n")

# ------------------------------------------------------------------------------
# Print summary table
# ------------------------------------------------------------------------------
cat("\n=== Summary Table: Fished/Unfished Ratio by Period and Size ===\n\n")

# Wide format for printing
summary_wide <- ratio_summary %>%
  select(period, size_category, mean_ratio) %>%
  pivot_wider(names_from = size_category, values_from = mean_ratio)

print(summary_wide, n = 20)

cat("\n=== Key Findings ===\n\n")

# Pre-whaling validation
pre_whaling <- ratio_summary %>% filter(grepl("Pre-Whaling", period))
cat("Pre-Whaling (1920-1929) - Validation period:\n")
cat("  All ratios should be ~1.0 (no fishing yet)\n")
for (i in 1:nrow(pre_whaling)) {
  cat("  ", gsub("\n", " ", as.character(pre_whaling$size_category[i])), ": ", 
      round(pre_whaling$mean_ratio[i], 3), "\n", sep = "")
}

# Peak whaling impact
peak_whaling <- ratio_summary %>% filter(grepl("Peak Whaling", period) & grepl("Very Large", size_category))
cat("\nPeak Whaling (1955-1965) - Very Large organisms:\n")
cat("  Ratio:", round(peak_whaling$mean_ratio, 3), "\n")
cat("  This represents", round((1 - peak_whaling$mean_ratio) * 100, 1), "% depletion\n")

# Reference period
reference <- ratio_summary %>% filter(grepl("Reference", period) & grepl("Very Large", size_category))
cat("\nReference (2001-2010) - Very Large organisms:\n")
cat("  Ratio:", round(reference$mean_ratio, 3), "\n")
cat("  Shows", round((1 - reference$mean_ratio) * 100, 1), "% still depleted relative to unfished\n")

cat("\n=== Multi-Period Analysis Complete ===\n")
cat("Plots saved to:", output_dir, "\n")
cat("\nNote: Preliminary analysis using", length(unfished_indices), "of 2111 simulations.\n")
