# Compare Fished vs Unfished Community Size Spectra
# Compares time-averaged size spectra for peak and post krill fishing periods
# between the MC ensemble (fished) and the unfished (climate-only) baseline

library(mizer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

cat("=== Comparing Fished vs Unfished Community Size Spectra ===\n\n")

# ------------------------------------------------------------------------------
# Define periods for comparison
# ------------------------------------------------------------------------------
periods <- list(
  pre_whaling = 1841:1860,
  peak_whaling = 1930:1950,
  post_whaling = 1970:1980,
  peak_krill = 1977:1986,
  post_krill = 2001:2010
)

# Model domain area for density conversion
model_domain_area <- 1.95e+13  # m^2

# ------------------------------------------------------------------------------
# Load the unfished simulation
# ------------------------------------------------------------------------------
cat("Loading unfished simulation...\n")

# First try the full 1841-2010 unfished simulation
unfished_sim_file <- "sim_objects/sim_1841_2010_unfished_climate_only.rds"

if (!file.exists(unfished_sim_file)) {
  # Fall back to 1961-2010 version
  unfished_sim_file <- "sim_objects/ISIMIP3a_1961_2010_no_fishing_v2.rds"
  if (!file.exists(unfished_sim_file)) {
    unfished_sim_file <- "sim_objects/ISIMIP3a_1961_2010_no_fishing.rds"
  }
}

if (!file.exists(unfished_sim_file)) {
  stop("Could not find unfished simulation file in sim_objects/")
}

unfished_sim <- readRDS(unfished_sim_file)
cat("  Loaded:", unfished_sim_file, "\n")

# Check the time range
unfished_times <- as.numeric(dimnames(unfished_sim@n)$time)
cat("  Time range:", min(unfished_times), "-", max(unfished_times), "\n\n")

# Get size bins from unfished simulation
w_bins <- unfished_sim@params@w
w_full <- unfished_sim@params@w_full

# ------------------------------------------------------------------------------
# Load one MC simulation to get the structure (for fished comparison)
# ------------------------------------------------------------------------------
cat("Loading MC simulation results...\n")

# Use the combined rerun results which have actual simulations
mc_results_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/combined_rerun_successful_sims_20250923_122211.rds"
mc_results <- readRDS(mc_results_file)
cat("  Loaded:", mc_results_file, "\n")

# Get simulations list
simulations_list <- mc_results$simulations
n_sims <- length(simulations_list)
cat("  Found", n_sims, "simulations\n\n")

# Get structure from first simulation
first_sim <- simulations_list[[1]]
sim_times <- as.numeric(dimnames(first_sim@n)$time)
cat("  MC simulation time range:", min(sim_times), "-", max(sim_times), "\n\n")

# ------------------------------------------------------------------------------
# Function to extract community size spectrum for a time period
# ------------------------------------------------------------------------------
extract_community_spectrum <- function(sim, time_range, normalize_by_area = FALSE) {
  # Get params
  params <- sim@params
  w <- params@w
  dw <- params@dw
  
  # Get time indices that fall within the requested range
  sim_times <- as.numeric(dimnames(sim@n)$time)
  time_idx <- which(sim_times %in% time_range)
  
  if (length(time_idx) == 0) {
    warning("No time steps found in range ", min(time_range), "-", max(time_range))
    return(NULL)
  }
  
  # Extract n array for the time range: [time, species, size]
  n_subset <- sim@n[time_idx, , , drop = FALSE]
  
  # Sum across species to get community spectrum for each time step
  # n is in numbers per size bin
  community_spectrum <- apply(n_subset, c(1, 3), sum)  # [time, size]
  
  # Time-average
  mean_spectrum <- colMeans(community_spectrum)
  
  # Convert to density (numbers per m^2 per size bin)
  if (normalize_by_area) {
    mean_spectrum <- mean_spectrum / model_domain_area
  }
  
  # Return as data frame
  data.frame(
    w = w,
    dw = dw,
    n = mean_spectrum,
    n_density = mean_spectrum / dw,  # numbers per gram per m^2 (if normalized)
    biomass = mean_spectrum * w,     # biomass per size bin
    biomass_density = mean_spectrum * w / dw  # biomass per gram per m^2
  )
}

# ------------------------------------------------------------------------------
# Extract unfished spectra for each period
# ------------------------------------------------------------------------------
cat("Extracting unfished spectra for each period...\n")

unfished_spectra <- list()
for (period_name in names(periods)) {
  period_years <- periods[[period_name]]
  cat("  Period:", period_name, "(", min(period_years), "-", max(period_years), ")\n")
  
  spectrum <- extract_community_spectrum(unfished_sim, period_years, normalize_by_area = TRUE)
  if (!is.null(spectrum)) {
    spectrum$period <- period_name
    spectrum$scenario <- "unfished"
    unfished_spectra[[period_name]] <- spectrum
  }
}

cat("\n")

# ------------------------------------------------------------------------------
# Extract fished spectra from MC ensemble
# ------------------------------------------------------------------------------
cat("Extracting fished spectra from MC ensemble...\n")

# Initialize storage for each period
fished_spectra <- list()
for (period_name in names(periods)) {
  fished_spectra[[period_name]] <- list()
}

# Process each simulation
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
  
  # Extract spectrum for each period
  for (period_name in names(periods)) {
    period_years <- periods[[period_name]]
    
    spectrum <- tryCatch({
      extract_community_spectrum(sim, period_years, normalize_by_area = TRUE)
    }, error = function(e) NULL)
    
    if (!is.null(spectrum)) {
      spectrum$sim_id <- i
      fished_spectra[[period_name]][[length(fished_spectra[[period_name]]) + 1]] <- spectrum
    }
  }
}
close(pb)

cat("\n  Skipped", skipped, "simulations due to errors\n\n")

# ------------------------------------------------------------------------------
# Combine fished spectra and calculate ensemble statistics
# ------------------------------------------------------------------------------
cat("Calculating ensemble statistics...\n")

fished_ensemble_stats <- list()

for (period_name in names(periods)) {
  # Combine all simulations for this period
  period_data <- bind_rows(fished_spectra[[period_name]])
  
  # Calculate ensemble statistics
  stats <- period_data %>%
    group_by(w, dw) %>%
    summarise(
      n_mean = mean(n, na.rm = TRUE),
      n_median = median(n, na.rm = TRUE),
      n_q05 = quantile(n, 0.05, na.rm = TRUE),
      n_q25 = quantile(n, 0.25, na.rm = TRUE),
      n_q75 = quantile(n, 0.75, na.rm = TRUE),
      n_q95 = quantile(n, 0.95, na.rm = TRUE),
      n_density_mean = mean(n_density, na.rm = TRUE),
      n_density_median = median(n_density, na.rm = TRUE),
      n_density_q05 = quantile(n_density, 0.05, na.rm = TRUE),
      n_density_q95 = quantile(n_density, 0.95, na.rm = TRUE),
      biomass_mean = mean(biomass, na.rm = TRUE),
      biomass_median = median(biomass, na.rm = TRUE),
      biomass_q05 = quantile(biomass, 0.05, na.rm = TRUE),
      biomass_q95 = quantile(biomass, 0.95, na.rm = TRUE),
      biomass_density_mean = mean(biomass_density, na.rm = TRUE),
      biomass_density_median = median(biomass_density, na.rm = TRUE),
      biomass_density_q05 = quantile(biomass_density, 0.05, na.rm = TRUE),
      biomass_density_q95 = quantile(biomass_density, 0.95, na.rm = TRUE),
      n_sims = n(),
      .groups = "drop"
    )
  
  stats$period <- period_name
  stats$scenario <- "fished"
  fished_ensemble_stats[[period_name]] <- stats
  
  cat("  Period:", period_name, "- processed", max(stats$n_sims), "simulations\n")
}

# ------------------------------------------------------------------------------
# Create comparison data frames
# ------------------------------------------------------------------------------
cat("\nPreparing comparison data...\n")

# Combine unfished data
unfished_combined <- bind_rows(unfished_spectra)

# Combine fished statistics
fished_combined <- bind_rows(fished_ensemble_stats)

# Create comparison data with ratio (fished / unfished)
comparison_data <- list()

for (period_name in names(periods)) {
  unfished_period <- unfished_spectra[[period_name]]
  fished_period <- fished_ensemble_stats[[period_name]]
  
  # Merge by size
  merged <- inner_join(
    unfished_period %>% select(w, n_unfished = n, n_density_unfished = n_density, 
                                biomass_unfished = biomass, biomass_density_unfished = biomass_density),
    fished_period %>% select(w, n_mean, n_median, n_q05, n_q95, n_density_mean, 
                             biomass_mean, biomass_median, biomass_q05, biomass_q95, biomass_density_mean),
    by = "w"
  )
  
  # Calculate ratios
  merged <- merged %>%
    mutate(
      period = period_name,
      n_ratio = n_mean / n_unfished,
      n_ratio_median = n_median / n_unfished,
      biomass_ratio = biomass_mean / biomass_unfished,
      biomass_ratio_median = biomass_median / biomass_unfished,
      n_diff = n_mean - n_unfished,
      biomass_diff = biomass_mean - biomass_unfished,
      log_n_ratio = log10(n_mean / n_unfished),
      log_biomass_ratio = log10(biomass_mean / biomass_unfished)
    )
  
  comparison_data[[period_name]] <- merged
}

comparison_combined <- bind_rows(comparison_data)

# ------------------------------------------------------------------------------
# Create output directory
# ------------------------------------------------------------------------------
output_dir <- "fishmip_outputs/comparison_plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ------------------------------------------------------------------------------
# PLOT 1: Community Size Spectrum - Fished vs Unfished (by period)
# ------------------------------------------------------------------------------
cat("\nCreating comparison plots...\n")

# Define nice period labels
period_labels <- c(
  pre_whaling = "Pre-Whaling (1841-1860)",
  peak_whaling = "Peak Whaling (1930-1950)",
  post_whaling = "Post-Peak Whaling (1970-1980)",
  peak_krill = "Peak Krill Fishing (1977-1986)",
  post_krill = "Post Krill Fishing (2001-2010)"
)

# Plot for each period
for (period_name in names(periods)) {
  cat("  Creating plot for", period_name, "...\n")
  
  unfished_period <- unfished_spectra[[period_name]]
  fished_period <- fished_ensemble_stats[[period_name]]
  
  p <- ggplot() +
    # Fished ensemble uncertainty ribbon
    geom_ribbon(data = fished_period,
                aes(x = w, ymin = n_density_q05, ymax = n_density_q95),
                fill = "steelblue", alpha = 0.3) +
    # Unfished line
    geom_line(data = unfished_period,
              aes(x = w, y = n_density, color = "Unfished (Climate only)"),
              linewidth = 1.2, linetype = "dashed") +
    # Fished median
    geom_line(data = fished_period,
              aes(x = w, y = n_density_median, color = "Fished (Ensemble median)"),
              linewidth = 1) +
    # Fished mean
    geom_line(data = fished_period,
              aes(x = w, y = n_density_mean, color = "Fished (Ensemble mean)"),
              linewidth = 1, linetype = "dotted") +
    scale_x_log10(labels = function(x) {
      ifelse(x >= 1000, paste0(x/1000, " kg"), paste0(x, " g"))
    }) +
    scale_y_log10(labels = scientific) +
    scale_color_manual(values = c(
      "Unfished (Climate only)" = "darkred",
      "Fished (Ensemble median)" = "steelblue",
      "Fished (Ensemble mean)" = "navy"
    )) +
    labs(
      title = paste("Community Size Spectrum:", period_labels[period_name]),
      subtitle = "Fished (MC ensemble, n=2111) vs Unfished (climate forcing only)",
      x = "Body mass",
      y = expression("Abundance density (ind g"^-1*" m"^-2*")"),
      color = "Scenario"
    ) +
    theme_classic() +
    theme(
      legend.position = c(0.75, 0.85),
      legend.background = element_rect(fill = "white", color = "grey80"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "grey40"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  
  ggsave(file.path(output_dir, paste0("spectrum_fished_vs_unfished_", period_name, ".png")),
         p, width = 10, height = 7, dpi = 300)
}

# ------------------------------------------------------------------------------
# PLOT 2: Combined panel - Both periods side by side
# ------------------------------------------------------------------------------
cat("  Creating combined panel plot...\n")

# Prepare data for faceted plot
plot_data_unfished <- unfished_combined %>%
  mutate(period_label = period_labels[period])

plot_data_fished <- fished_combined %>%
  mutate(period_label = period_labels[period])

p_combined <- ggplot() +
  # Fished ensemble uncertainty ribbon
  geom_ribbon(data = plot_data_fished,
              aes(x = w, ymin = n_density_q05, ymax = n_density_q95),
              fill = "steelblue", alpha = 0.3) +
  # Unfished line
  geom_line(data = plot_data_unfished,
            aes(x = w, y = n_density, color = "Unfished (Climate only)"),
            linewidth = 1.2, linetype = "dashed") +
  # Fished median
  geom_line(data = plot_data_fished,
            aes(x = w, y = n_density_median, color = "Fished (Ensemble median)"),
            linewidth = 1) +
  facet_wrap(~period_label, ncol = 2) +
  scale_x_log10(labels = function(x) {
    ifelse(x >= 1000, paste0(x/1000, " kg"), paste0(x, " g"))
  }) +
  scale_y_log10(labels = scientific) +
  scale_color_manual(values = c(
    "Unfished (Climate only)" = "darkred",
    "Fished (Ensemble median)" = "steelblue"
  )) +
  labs(
    title = "Community Size Spectrum: Fished vs Unfished",
    subtitle = "Comparing MC ensemble (n=2111) to climate-only baseline",
    x = "Body mass",
    y = expression("Abundance density (ind g"^-1*" m"^-2*")"),
    color = "Scenario"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(file.path(output_dir, "spectrum_fished_vs_unfished_combined.png"),
       p_combined, width = 14, height = 7, dpi = 300)

# ------------------------------------------------------------------------------
# PLOT 3: Ratio plot (Fished / Unfished)
# ------------------------------------------------------------------------------
cat("  Creating ratio plot...\n")

comparison_combined <- comparison_combined %>%
  mutate(period_label = period_labels[period])

p_ratio <- ggplot(comparison_combined, aes(x = w, y = n_ratio, color = period_label)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1) +
  scale_x_log10(labels = function(x) {
    ifelse(x >= 1000, paste0(x/1000, " kg"), paste0(x, " g"))
  }) +
  scale_y_log10() +
  scale_color_manual(values = c(
    "Pre-Whaling (1841-1860)" = "darkblue",
    "Peak Whaling (1930-1950)" = "red",
    "Post-Peak Whaling (1970-1980)" = "purple",
    "Peak Krill Fishing (1977-1986)" = "orange",
    "Post Krill Fishing (2001-2010)" = "forestgreen"
  )) +
  labs(
    title = "Size-Specific Fishing Impact: Ratio of Fished to Unfished Abundance",
    subtitle = "Values < 1 indicate fishing reduced abundance; values > 1 indicate fishing increased abundance",
    x = "Body mass",
    y = "Ratio (Fished / Unfished)",
    color = "Period"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = "grey80"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(file.path(output_dir, "spectrum_fishing_impact_ratio.png"),
       p_ratio, width = 10, height = 7, dpi = 300)

# ------------------------------------------------------------------------------
# PLOT 4: Biomass spectrum comparison
# ------------------------------------------------------------------------------
cat("  Creating biomass spectrum comparison...\n")

p_biomass <- ggplot() +
  # Fished ensemble uncertainty ribbon
  geom_ribbon(data = plot_data_fished,
              aes(x = w, ymin = biomass_q05, ymax = biomass_q95),
              fill = "steelblue", alpha = 0.3) +
  # Unfished line
  geom_line(data = plot_data_unfished,
            aes(x = w, y = biomass, color = "Unfished (Climate only)"),
            linewidth = 1.2, linetype = "dashed") +
  # Fished median
  geom_line(data = plot_data_fished,
            aes(x = w, y = biomass_median, color = "Fished (Ensemble median)"),
            linewidth = 1) +
  facet_wrap(~period_label, ncol = 2) +
  scale_x_log10(labels = function(x) {
    ifelse(x >= 1000, paste0(x/1000, " kg"), paste0(x, " g"))
  }) +
  scale_y_log10(labels = scientific) +
  scale_color_manual(values = c(
    "Unfished (Climate only)" = "darkred",
    "Fished (Ensemble median)" = "steelblue"
  )) +
  labs(
    title = "Community Biomass Spectrum: Fished vs Unfished",
    subtitle = "Comparing MC ensemble (n=2111) to climate-only baseline",
    x = "Body mass",
    y = "Biomass per size bin (g)",
    color = "Scenario"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(file.path(output_dir, "biomass_spectrum_fished_vs_unfished.png"),
       p_biomass, width = 14, height = 7, dpi = 300)

# ------------------------------------------------------------------------------
# PLOT 5: Biomass ratio by size
# ------------------------------------------------------------------------------
cat("  Creating biomass ratio plot...\n")

p_biomass_ratio <- ggplot(comparison_combined, aes(x = w, y = biomass_ratio, color = period_label)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1) +
  scale_x_log10(labels = function(x) {
    ifelse(x >= 1000, paste0(x/1000, " kg"), paste0(x, " g"))
  }) +
  scale_y_log10() +
  scale_color_manual(values = c(
    "Pre-Whaling (1841-1860)" = "darkblue",
    "Peak Whaling (1930-1950)" = "red",
    "Post-Peak Whaling (1970-1980)" = "purple",
    "Peak Krill Fishing (1977-1986)" = "orange",
    "Post Krill Fishing (2001-2010)" = "forestgreen"
  )) +
  labs(
    title = "Size-Specific Fishing Impact on Biomass",
    subtitle = "Ratio of fished to unfished biomass by size class",
    x = "Body mass",
    y = "Biomass ratio (Fished / Unfished)",
    color = "Period"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = "grey80"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(file.path(output_dir, "biomass_fishing_impact_ratio.png"),
       p_biomass_ratio, width = 10, height = 7, dpi = 300)

# ------------------------------------------------------------------------------
# Save comparison data
# ------------------------------------------------------------------------------
cat("\nSaving comparison data...\n")

# Save detailed comparison
write.csv(comparison_combined, file.path(output_dir, "fished_vs_unfished_comparison.csv"), 
          row.names = FALSE)

# Save unfished spectra
write.csv(unfished_combined, file.path(output_dir, "unfished_spectra_by_period.csv"),
          row.names = FALSE)

# Save fished ensemble stats
write.csv(fished_combined, file.path(output_dir, "fished_ensemble_spectra_by_period.csv"),
          row.names = FALSE)

cat("\n=== Analysis Complete ===\n")
cat("Plots saved to:", output_dir, "\n")
cat("Files created:\n")
cat("  - spectrum_fished_vs_unfished_peak_krill.png\n")
cat("  - spectrum_fished_vs_unfished_post_krill.png\n")
cat("  - spectrum_fished_vs_unfished_combined.png\n")
cat("  - spectrum_fishing_impact_ratio.png\n")
cat("  - biomass_spectrum_fished_vs_unfished.png\n")
cat("  - biomass_fishing_impact_ratio.png\n")
cat("  - fished_vs_unfished_comparison.csv\n")
cat("  - unfished_spectra_by_period.csv\n")
cat("  - fished_ensemble_spectra_by_period.csv\n")
