###############################################################################
# Community Size Spectrum Comparison Script
# 
# Creates community size spectrum plots comparing different historical periods:
# - Pre-whaling, Peak-whaling, Post-whaling
# - Pre-krill fishing, Peak-krill fishing, Post-peak krill fishing
#
# Author: Generated for Prydz Bay mizer project
# Date: 2025
###############################################################################

library(mizer)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

###############################################################################
# Configuration
###############################################################################

# File paths
MC_RESULTS_FILE <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/combined_rerun_successful_sims_20250923_122211.rds"
PLOT_DIR <- "fishmip_outputs/comparison_plots"

# Model domain area
MODEL_DOMAIN_AREA <- 1.95e+13  # m^2

# Define periods for comparison
PERIODS <- list(
  # Whaling comparison
  whaling = list(
    "Pre-whaling (1920-1929)" = 1920:1929,
    "Peak whaling (1930-1939)" = 1930:1939,
    "Post-whaling (1970-1979)" = 1970:1979
  ),
  # Krill fishing comparison
  krill = list(
    "Pre-krill fishing (1965-1974)" = 1965:1974,
    "Peak krill fishing (1977-1986)" = 1977:1986,
    "Post-peak krill (2001-2010)" = 2001:2010
  )
)

# Color palettes
WHALING_COLORS <- c(
  "Pre-whaling (1920-1929)" = "#2166ac",
  "Peak whaling (1930-1939)" = "#b2182b", 
  "Post-whaling (1970-1979)" = "#1b7837"
)

KRILL_COLORS <- c(
  "Pre-krill fishing (1965-1974)" = "#2166ac",
  "Peak krill fishing (1977-1986)" = "#b2182b",
  "Post-peak krill (2001-2010)" = "#1b7837"
)

###############################################################################
# Create output directory
###############################################################################

if (!dir.exists(PLOT_DIR)) {
  dir.create(PLOT_DIR, recursive = TRUE)
}

###############################################################################
# Load Data
###############################################################################

cat("Loading Monte Carlo results...\n")
mc <- readRDS(MC_RESULTS_FILE)
n_sims <- mc$n_successful
cat(sprintf("  Loaded %d simulations\n", n_sims))

# Get reference simulation for weight bins
sim1 <- mc$simulations[[1]]
w_vec <- w(sim1@params)
dw_vec <- sim1@params@dw
n_w <- length(w_vec)
log10_w <- log10(w_vec)

cat(sprintf("  Weight bins: %d (log10 range: %.2f to %.2f)\n", 
            n_w, min(log10_w), max(log10_w)))

###############################################################################
# Helper Functions
###############################################################################

#' Extract community size spectrum for a given time period
#' @param sim MizerSim object
#' @param years Vector of years to average over
#' @param w_vec Weight bin centers
#' @param dw_vec Weight bin widths
#' @return Vector of biomass density per weight bin (summed across species)
extract_community_spectrum <- function(sim, years, w_vec, dw_vec) {
  n_array <- sim@n
  time_names <- as.numeric(dimnames(n_array)$time)
  
  # Find indices for requested years
  year_idx <- which(time_names %in% years)
  
  if (length(year_idx) == 0) {
    return(rep(NA, length(w_vec)))
  }
  
  # Extract N for these years and average
  n_subset <- n_array[year_idx, , , drop = FALSE]
  n_mean <- apply(n_subset, c(2, 3), mean)  # Average over time, keep species x weight
  
  # Calculate biomass density: N * w (abundance * weight)
  # Sum across species to get community spectrum
  biomass_per_w <- rep(0, length(w_vec))
  for (i in seq_along(w_vec)) {
    biomass_per_w[i] <- sum(n_mean[, i] * w_vec[i])
  }
  
  return(biomass_per_w)
}

#' Process all simulations for a set of periods
#' @param mc Monte Carlo results object
#' @param periods List of named vectors of years
#' @param w_vec Weight bin centers
#' @param dw_vec Weight bin widths
#' @return Data frame with ensemble statistics for each period
process_periods <- function(mc, periods, w_vec, dw_vec) {
  n_sims <- mc$n_successful
  n_w <- length(w_vec)
  n_periods <- length(periods)
  
  # Initialize storage
  all_spectra <- array(NA, dim = c(n_sims, n_periods, n_w))
  
  cat("  Processing simulations...\n")
  pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
  
  for (i in 1:n_sims) {
    sim <- tryCatch({
      mc$simulations[[i]]
    }, error = function(e) NULL)
    
    if (!is.null(sim)) {
      for (p in 1:n_periods) {
        years <- periods[[p]]
        spectrum <- extract_community_spectrum(sim, years, w_vec, dw_vec)
        all_spectra[i, p, ] <- spectrum
      }
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # Calculate ensemble statistics
  result_list <- list()
  
  for (p in 1:n_periods) {
    period_name <- names(periods)[p]
    
    stats_df <- data.frame(
      period = period_name,
      log10_w = log10(w_vec),
      w = w_vec,
      median = apply(all_spectra[, p, ], 2, median, na.rm = TRUE),
      q05 = apply(all_spectra[, p, ], 2, quantile, 0.05, na.rm = TRUE),
      q25 = apply(all_spectra[, p, ], 2, quantile, 0.25, na.rm = TRUE),
      q75 = apply(all_spectra[, p, ], 2, quantile, 0.75, na.rm = TRUE),
      q95 = apply(all_spectra[, p, ], 2, quantile, 0.95, na.rm = TRUE)
    )
    
    result_list[[p]] <- stats_df
  }
  
  return(do.call(rbind, result_list))
}

###############################################################################
# Process Whaling Periods
###############################################################################

cat("\nProcessing whaling period comparisons...\n")
whaling_spectra <- process_periods(mc, PERIODS$whaling, w_vec, dw_vec)

# Convert to density (g m^-2 per log10 weight bin)
whaling_spectra <- whaling_spectra %>%
  mutate(
    median_density = median / MODEL_DOMAIN_AREA,
    q05_density = q05 / MODEL_DOMAIN_AREA,
    q25_density = q25 / MODEL_DOMAIN_AREA,
    q75_density = q75 / MODEL_DOMAIN_AREA,
    q95_density = q95 / MODEL_DOMAIN_AREA
  )

# Set factor levels for proper ordering
whaling_spectra$period <- factor(whaling_spectra$period, 
                                  levels = names(PERIODS$whaling))

###############################################################################
# Process Krill Periods
###############################################################################

cat("\nProcessing krill fishing period comparisons...\n")
krill_spectra <- process_periods(mc, PERIODS$krill, w_vec, dw_vec)

# Convert to density
krill_spectra <- krill_spectra %>%
  mutate(
    median_density = median / MODEL_DOMAIN_AREA,
    q05_density = q05 / MODEL_DOMAIN_AREA,
    q25_density = q25 / MODEL_DOMAIN_AREA,
    q75_density = q75 / MODEL_DOMAIN_AREA,
    q95_density = q95 / MODEL_DOMAIN_AREA
  )

krill_spectra$period <- factor(krill_spectra$period, 
                                levels = names(PERIODS$krill))

###############################################################################
# Plot 1: Whaling Era Size Spectrum Comparison
###############################################################################

cat("\nCreating whaling era size spectrum plot...\n")

p_whaling <- ggplot(whaling_spectra, aes(x = log10_w, color = period, fill = period)) +
  # Uncertainty ribbons
  geom_ribbon(aes(ymin = q05_density, ymax = q95_density), alpha = 0.15, color = NA) +
  geom_ribbon(aes(ymin = q25_density, ymax = q75_density), alpha = 0.3, color = NA) +
  # Median lines
  geom_line(aes(y = median_density), linewidth = 1.2) +
  # Styling
  scale_color_manual(values = WHALING_COLORS, name = "Period") +
  scale_fill_manual(values = WHALING_COLORS, name = "Period") +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  labs(
    title = "Community Size Spectrum: Whaling Era Comparison",
    subtitle = "Ensemble median with 50% (dark) and 90% (light) credible intervals",
    x = expression("Body mass (log"[10]*" grams)"),
    y = expression("Biomass density (g m"^-2*")"),
    caption = "2,111 Monte Carlo simulations"
  ) +
  # Add reference lines for key size classes
  geom_vline(xintercept = c(0, 2, 4, 6), linetype = "dotted", alpha = 0.3) +
  annotate("text", x = 1, y = max(whaling_spectra$q95_density, na.rm = TRUE) * 0.5, 
           label = "1-100g", size = 3, alpha = 0.5) +
  annotate("text", x = 3, y = max(whaling_spectra$q95_density, na.rm = TRUE) * 0.5, 
           label = "100g-10kg", size = 3, alpha = 0.5) +
  annotate("text", x = 5, y = max(whaling_spectra$q95_density, na.rm = TRUE) * 0.5, 
           label = "10kg-1t", size = 3, alpha = 0.5) +
  annotate("text", x = 7, y = max(whaling_spectra$q95_density, na.rm = TRUE) * 0.5, 
           label = ">1t", size = 3, alpha = 0.5) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(PLOT_DIR, "community_size_spectrum_whaling_eras.png"), 
       p_whaling, width = 12, height = 8, dpi = 300)
cat(sprintf("  Saved: %s\n", file.path(PLOT_DIR, "community_size_spectrum_whaling_eras.png")))

###############################################################################
# Plot 2: Krill Era Size Spectrum Comparison
###############################################################################

cat("Creating krill fishing era size spectrum plot...\n")

p_krill <- ggplot(krill_spectra, aes(x = log10_w, color = period, fill = period)) +
  geom_ribbon(aes(ymin = q05_density, ymax = q95_density), alpha = 0.15, color = NA) +
  geom_ribbon(aes(ymin = q25_density, ymax = q75_density), alpha = 0.3, color = NA) +
  geom_line(aes(y = median_density), linewidth = 1.2) +
  scale_color_manual(values = KRILL_COLORS, name = "Period") +
  scale_fill_manual(values = KRILL_COLORS, name = "Period") +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  labs(
    title = "Community Size Spectrum: Krill Fishing Era Comparison",
    subtitle = "Ensemble median with 50% (dark) and 90% (light) credible intervals",
    x = expression("Body mass (log"[10]*" grams)"),
    y = expression("Biomass density (g m"^-2*")"),
    caption = "2,111 Monte Carlo simulations"
  ) +
  geom_vline(xintercept = c(0, 2, 4, 6), linetype = "dotted", alpha = 0.3) +
  annotate("text", x = 1, y = max(krill_spectra$q95_density, na.rm = TRUE) * 0.5, 
           label = "1-100g", size = 3, alpha = 0.5) +
  annotate("text", x = 3, y = max(krill_spectra$q95_density, na.rm = TRUE) * 0.5, 
           label = "100g-10kg", size = 3, alpha = 0.5) +
  annotate("text", x = 5, y = max(krill_spectra$q95_density, na.rm = TRUE) * 0.5, 
           label = "10kg-1t", size = 3, alpha = 0.5) +
  annotate("text", x = 7, y = max(krill_spectra$q95_density, na.rm = TRUE) * 0.5, 
           label = ">1t", size = 3, alpha = 0.5) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(PLOT_DIR, "community_size_spectrum_krill_eras.png"), 
       p_krill, width = 12, height = 8, dpi = 300)
cat(sprintf("  Saved: %s\n", file.path(PLOT_DIR, "community_size_spectrum_krill_eras.png")))

###############################################################################
# Plot 3: Normalized Sheldon Spectrum (N * w^2)
###############################################################################

cat("Creating normalized Sheldon spectrum plots...\n")

# Sheldon spectrum: multiply by w to get abundance, then by w again for normalization
# This should give a flatter spectrum that's easier to compare across sizes

whaling_sheldon <- whaling_spectra %>%
  mutate(
    sheldon_median = median_density * w,
    sheldon_q05 = q05_density * w,
    sheldon_q25 = q25_density * w,
    sheldon_q75 = q75_density * w,
    sheldon_q95 = q95_density * w
  )

p_whaling_sheldon <- ggplot(whaling_sheldon, aes(x = log10_w, color = period, fill = period)) +
  geom_ribbon(aes(ymin = sheldon_q05, ymax = sheldon_q95), alpha = 0.15, color = NA) +
  geom_ribbon(aes(ymin = sheldon_q25, ymax = sheldon_q75), alpha = 0.3, color = NA) +
  geom_line(aes(y = sheldon_median), linewidth = 1.2) +
  scale_color_manual(values = WHALING_COLORS, name = "Period") +
  scale_fill_manual(values = WHALING_COLORS, name = "Period") +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  labs(
    title = "Normalized Size Spectrum (Sheldon): Whaling Era Comparison",
    subtitle = "Biomass × weight to flatten spectrum; Ensemble with credible intervals",
    x = expression("Body mass (log"[10]*" grams)"),
    y = expression("Normalized biomass (g"^2*" m"^-2*")"),
    caption = "2,111 Monte Carlo simulations"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(PLOT_DIR, "sheldon_spectrum_whaling_eras.png"), 
       p_whaling_sheldon, width = 12, height = 8, dpi = 300)
cat(sprintf("  Saved: %s\n", file.path(PLOT_DIR, "sheldon_spectrum_whaling_eras.png")))

# Krill era Sheldon spectrum
krill_sheldon <- krill_spectra %>%
  mutate(
    sheldon_median = median_density * w,
    sheldon_q05 = q05_density * w,
    sheldon_q25 = q25_density * w,
    sheldon_q75 = q75_density * w,
    sheldon_q95 = q95_density * w
  )

p_krill_sheldon <- ggplot(krill_sheldon, aes(x = log10_w, color = period, fill = period)) +
  geom_ribbon(aes(ymin = sheldon_q05, ymax = sheldon_q95), alpha = 0.15, color = NA) +
  geom_ribbon(aes(ymin = sheldon_q25, ymax = sheldon_q75), alpha = 0.3, color = NA) +
  geom_line(aes(y = sheldon_median), linewidth = 1.2) +
  scale_color_manual(values = KRILL_COLORS, name = "Period") +
  scale_fill_manual(values = KRILL_COLORS, name = "Period") +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  labs(
    title = "Normalized Size Spectrum (Sheldon): Krill Fishing Era Comparison",
    subtitle = "Biomass × weight to flatten spectrum; Ensemble with credible intervals",
    x = expression("Body mass (log"[10]*" grams)"),
    y = expression("Normalized biomass (g"^2*" m"^-2*")"),
    caption = "2,111 Monte Carlo simulations"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(PLOT_DIR, "sheldon_spectrum_krill_eras.png"), 
       p_krill_sheldon, width = 12, height = 8, dpi = 300)
cat(sprintf("  Saved: %s\n", file.path(PLOT_DIR, "sheldon_spectrum_krill_eras.png")))

###############################################################################
# Plot 4: Ratio plots (change relative to baseline period)
###############################################################################

cat("Creating ratio plots...\n")

# Whaling: ratio relative to pre-whaling
whaling_wide <- whaling_spectra %>%
  select(period, log10_w, median_density) %>%
  pivot_wider(names_from = period, values_from = median_density)

names(whaling_wide)[2:4] <- c("pre", "peak", "post")

whaling_ratios <- whaling_wide %>%
  mutate(
    peak_vs_pre = peak / pre,
    post_vs_pre = post / pre,
    post_vs_peak = post / peak
  ) %>%
  select(log10_w, peak_vs_pre, post_vs_pre, post_vs_peak) %>%
  pivot_longer(cols = -log10_w, names_to = "comparison", values_to = "ratio") %>%
  mutate(comparison = case_when(
    comparison == "peak_vs_pre" ~ "Peak whaling / Pre-whaling",
    comparison == "post_vs_pre" ~ "Post-whaling / Pre-whaling",
    comparison == "post_vs_peak" ~ "Post-whaling / Peak whaling"
  ))

p_whaling_ratio <- ggplot(whaling_ratios, aes(x = log10_w, y = ratio, color = comparison)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1.2) +
  scale_color_brewer(palette = "Set1", name = "Comparison") +
  scale_y_log10() +
  labs(
    title = "Biomass Ratio by Size Class: Whaling Era Changes",
    subtitle = "Values > 1 indicate increase; < 1 indicate decrease",
    x = expression("Body mass (log"[10]*" grams)"),
    y = "Biomass ratio (log scale)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(PLOT_DIR, "size_spectrum_ratio_whaling.png"), 
       p_whaling_ratio, width = 12, height = 7, dpi = 300)
cat(sprintf("  Saved: %s\n", file.path(PLOT_DIR, "size_spectrum_ratio_whaling.png")))

# Krill: ratio relative to pre-krill
krill_wide <- krill_spectra %>%
  select(period, log10_w, median_density) %>%
  pivot_wider(names_from = period, values_from = median_density)

names(krill_wide)[2:4] <- c("pre", "peak", "post")

krill_ratios <- krill_wide %>%
  mutate(
    peak_vs_pre = peak / pre,
    post_vs_pre = post / pre,
    post_vs_peak = post / peak
  ) %>%
  select(log10_w, peak_vs_pre, post_vs_pre, post_vs_peak) %>%
  pivot_longer(cols = -log10_w, names_to = "comparison", values_to = "ratio") %>%
  mutate(comparison = case_when(
    comparison == "peak_vs_pre" ~ "Peak krill / Pre-krill",
    comparison == "post_vs_pre" ~ "Post-peak / Pre-krill",
    comparison == "post_vs_peak" ~ "Post-peak / Peak krill"
  ))

p_krill_ratio <- ggplot(krill_ratios, aes(x = log10_w, y = ratio, color = comparison)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1.2) +
  scale_color_brewer(palette = "Set1", name = "Comparison") +
  scale_y_log10() +
  labs(
    title = "Biomass Ratio by Size Class: Krill Fishing Era Changes",
    subtitle = "Values > 1 indicate increase; < 1 indicate decrease",
    x = expression("Body mass (log"[10]*" grams)"),
    y = "Biomass ratio (log scale)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(PLOT_DIR, "size_spectrum_ratio_krill.png"), 
       p_krill_ratio, width = 12, height = 7, dpi = 300)
cat(sprintf("  Saved: %s\n", file.path(PLOT_DIR, "size_spectrum_ratio_krill.png")))

###############################################################################
# Save Data
###############################################################################

cat("\nSaving spectrum data to CSV...\n")

write.csv(whaling_spectra, file.path(PLOT_DIR, "community_spectrum_whaling_eras.csv"), 
          row.names = FALSE)
write.csv(krill_spectra, file.path(PLOT_DIR, "community_spectrum_krill_eras.csv"), 
          row.names = FALSE)

cat(sprintf("  Saved: %s\n", file.path(PLOT_DIR, "community_spectrum_whaling_eras.csv")))
cat(sprintf("  Saved: %s\n", file.path(PLOT_DIR, "community_spectrum_krill_eras.csv")))

###############################################################################
# Summary
###############################################################################

cat("\n=============================================================\n")
cat("Community Size Spectrum Plots Complete\n")
cat("=============================================================\n")
cat(sprintf("Plots saved to: %s\n", PLOT_DIR))
cat("\nGenerated plots:\n")
cat("  1. community_size_spectrum_whaling_eras.png\n")
cat("  2. community_size_spectrum_krill_eras.png\n")
cat("  3. sheldon_spectrum_whaling_eras.png\n")
cat("  4. sheldon_spectrum_krill_eras.png\n")
cat("  5. size_spectrum_ratio_whaling.png\n")
cat("  6. size_spectrum_ratio_krill.png\n")
cat("=============================================================\n")
