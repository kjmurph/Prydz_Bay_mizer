# Create Fished vs Unfished Size Spectrum Ratio Plot
# Reference Period (2001-2010)
# Same style as the combined pre-whaling + krill fishing panel

library(mizer)
library(ggplot2)
library(dplyr)
library(tidyr)

cat("=== Fished vs Unfished Size Spectrum (Reference Period) ===\n\n")

output_dir <- "climate_only_analysis"

# Reference period
ref_years <- 2001:2010
reference_name <- "Reference (2001-2010)"

# ------------------------------------------------------------------------------
# Load ensembles
# ------------------------------------------------------------------------------
cat("Loading ensembles...\n")

# Climate-only (unfished) ensemble
climate_file <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
climate_ensemble <- readRDS(climate_file)
cat("  Climate-only:", climate_ensemble$n_simulations, "simulations\n")

# Fished ensemble
fished_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
fished_ensemble <- readRDS(fished_file)
n_fished <- length(fished_ensemble$simulations)
cat("  Fished:", n_fished, "simulations\n")

# Get size bins from first simulation
first_sim <- fished_ensemble$simulations[[1]]
w_bins <- first_sim@params@w
n_w <- length(w_bins)
cat("  Size bins:", n_w, "\n\n")

# ------------------------------------------------------------------------------
# Function to extract community size spectrum for a time period
# ------------------------------------------------------------------------------
extract_community_spectrum <- function(sim, time_range) {
  params <- sim@params
  w <- params@w
  
  sim_times <- as.numeric(dimnames(sim@n)$time)
  time_idx <- which(sim_times %in% time_range)
  
  if (length(time_idx) == 0) {
    return(NULL)
  }
  
  # Extract n array for the time range: [time, species, size]
  n_subset <- sim@n[time_idx, , , drop = FALSE]
  
  # Sum across species to get community spectrum at each time
  community_spectrum <- apply(n_subset, c(1, 3), sum, na.rm = TRUE)
  
  # Time-average
  mean_spectrum <- colMeans(community_spectrum, na.rm = TRUE)
  
  return(mean_spectrum)
}

# ------------------------------------------------------------------------------
# Extract spectra for reference period from both ensembles
# ------------------------------------------------------------------------------
cat("Extracting community spectra for reference period...\n")

# Storage for spectra
fished_spectra <- matrix(NA, nrow = n_fished, ncol = n_w)
climate_spectra <- matrix(NA, nrow = climate_ensemble$n_simulations, ncol = n_w)

# Process fished simulations
cat("  Processing fished simulations...\n")
pb <- txtProgressBar(min = 0, max = n_fished, style = 3)
for (i in seq_len(n_fished)) {
  setTxtProgressBar(pb, i)
  sim <- tryCatch(fished_ensemble$simulations[[i]], error = function(e) NULL)
  if (is.null(sim)) next
  
  spec <- tryCatch(extract_community_spectrum(sim, ref_years), error = function(e) NULL)
  if (!is.null(spec) && length(spec) == n_w) {
    fished_spectra[i, ] <- spec
  }
}
close(pb)

# Process climate-only simulations
cat("  Processing climate-only simulations...\n")
pb <- txtProgressBar(min = 0, max = climate_ensemble$n_simulations, style = 3)
for (i in seq_len(climate_ensemble$n_simulations)) {
  setTxtProgressBar(pb, i)
  sim <- tryCatch(climate_ensemble$simulations[[i]], error = function(e) NULL)
  if (is.null(sim)) next
  
  spec <- tryCatch(extract_community_spectrum(sim, ref_years), error = function(e) NULL)
  if (!is.null(spec) && length(spec) == n_w) {
    climate_spectra[i, ] <- spec
  }
}
close(pb)

cat("\n  Fished spectra extracted:", sum(!is.na(fished_spectra[,1])), "\n")
cat("  Climate-only spectra extracted:", sum(!is.na(climate_spectra[,1])), "\n")

# ------------------------------------------------------------------------------
# Calculate ratio: Fished / Unfished (Climate-Only)
# For each simulation pair (matched by index)
# ------------------------------------------------------------------------------
cat("\nCalculating fished/unfished ratios...\n")

# Calculate ratio for each simulation pair
ratio_matrix <- fished_spectra / climate_spectra

# Calculate statistics across simulations
ratio_stats <- data.frame(
  w = w_bins,
  ratio_median = apply(ratio_matrix, 2, median, na.rm = TRUE),
  ratio_mean = apply(ratio_matrix, 2, mean, na.rm = TRUE),
  ratio_q05 = apply(ratio_matrix, 2, quantile, probs = 0.05, na.rm = TRUE),
  ratio_q25 = apply(ratio_matrix, 2, quantile, probs = 0.25, na.rm = TRUE),
  ratio_q75 = apply(ratio_matrix, 2, quantile, probs = 0.75, na.rm = TRUE),
  ratio_q95 = apply(ratio_matrix, 2, quantile, probs = 0.95, na.rm = TRUE)
)

# Save data
write.csv(ratio_stats, file.path(output_dir, "fished_vs_unfished_ratio_stats.csv"), row.names = FALSE)
cat("  Saved: fished_vs_unfished_ratio_stats.csv\n")

# ------------------------------------------------------------------------------
# Create plot in the same style as the combined panel
# (matching the Pre-Whaling / Krill Fishing plot)
# ------------------------------------------------------------------------------
cat("\nCreating plot...\n")

# Filter to large organisms (>= 10 kg)
ratio_large <- ratio_stats %>%
  filter(w >= 10000)  # 10 kg = 10000 g

# Plot: Fished vs Unfished ratio (zoomed to >= 10 kg)
# Same style as the Pre-Whaling panel
p_ratio_large <- ggplot(ratio_large, aes(x = w)) +
  geom_ribbon(aes(ymin = ratio_q25, ymax = ratio_q75), fill = "#e41a1c", alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_line(aes(y = ratio_median), color = "#e41a1c", linewidth = 1.2) +
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
  scale_y_continuous(
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2),
    limits = c(0, 1.3)
  ) +
  labs(
    title = "Fishing Impact on Large Organism Size Spectrum",
    subtitle = paste0("Ratio: Fished / Unfished (Climate-Only) | ", reference_name, " | Zoomed to \u2265 10 kg"),
    x = "Body mass",
    y = "Abundance ratio (Fished / Unfished)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

ggsave(file.path(output_dir, "fished_vs_unfished_ratio_large_styled.png"), 
       p_ratio_large, width = 10, height = 6, dpi = 300)
cat("  Saved: fished_vs_unfished_ratio_large_styled.png\n")

# Also save to fishmip_outputs for consistency
fishmip_dir <- "fishmip_outputs/temporal_comparison"
if (!dir.exists(fishmip_dir)) dir.create(fishmip_dir, recursive = TRUE)
ggsave(file.path(fishmip_dir, "fished_vs_unfished_ratio_large_styled.png"), 
       p_ratio_large, width = 10, height = 6, dpi = 300)
cat("  Saved: fishmip_outputs/temporal_comparison/fished_vs_unfished_ratio_large_styled.png\n")

# ------------------------------------------------------------------------------
# Summary statistics
# ------------------------------------------------------------------------------
cat("\n=== Summary Statistics ===\n")

# Size categories for large organisms
ratio_large_summary <- ratio_large %>%
  mutate(
    size_cat = case_when(
      w < 1e5 ~ "10 kg - 100 kg",
      w < 1e6 ~ "100 kg - 1 t",
      w < 1e7 ~ "1 t - 10 t",
      w < 1e8 ~ "10 t - 100 t",
      TRUE ~ "> 100 t"
    )
  ) %>%
  group_by(size_cat) %>%
  summarise(
    mean_ratio = mean(ratio_median, na.rm = TRUE),
    min_ratio = min(ratio_median, na.rm = TRUE),
    max_ratio = max(ratio_median, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\nFishing impact by size category (Fished/Unfished ratio):\n")
print(ratio_large_summary)

# Overall whale impact
whale_impact <- ratio_large %>%
  filter(w >= 1e6) %>%  # >= 1 tonne
  summarise(
    mean_ratio = mean(ratio_median, na.rm = TRUE),
    min_ratio = min(ratio_median, na.rm = TRUE),
    max_ratio = max(ratio_median, na.rm = TRUE)
  )

cat("\nWhale-sized organisms (>= 1 tonne):\n")
cat("  Mean ratio:", round(whale_impact$mean_ratio, 3), "\n")
cat("  Range:", round(whale_impact$min_ratio, 3), "-", round(whale_impact$max_ratio, 3), "\n")
cat("  Interpretation: Fishing reduced whale-sized organisms to", 
    round(whale_impact$mean_ratio * 100, 1), "% of unfished levels\n")

cat("\n=== Analysis Complete ===\n")
