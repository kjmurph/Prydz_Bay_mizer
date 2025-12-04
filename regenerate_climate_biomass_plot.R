# Regenerate climate-only biomass plots: median only, 25-75%, and 5-95% versions
# Also creates biomass density spectra plots using plotSpectra()

library(mizer)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

cat("=== Regenerating Climate-Only Plots ===\n\n")

# ============================================================================
# PART 1: Biomass Timeseries Plots (3 versions)
# ============================================================================
cat("--- Part 1: Biomass Timeseries Plots ---\n")

# Load data
output_dir <- "climate_only_analysis"
biomass_summary_climate <- read.csv(file.path(output_dir, "climate_only_biomass_timeseries_summary.csv"))

species_order <- c(
  "mesozooplankton", "other krill", "other macrozooplankton", "antarctic krill",
  "salps", "mesopelagic fishes", "bathypelagic fishes", "shelf and coastal fishes",
  "flying birds", "small divers", "squids", "toothfishes", "leopard seals",
  "medium divers", "large divers", "minke whales", "orca", "sperm whales", "baleen whales"
)

biomass_summary_climate$Species <- factor(biomass_summary_climate$Species, levels = species_order)

n_sims <- 2111

# Observation data for reference period
obs_data <- biomass_summary_climate %>% 
  filter(!is.na(ObsBiomass_t) & Year >= 2001 & Year <= 2020)

# --- Version 1: Median only ---
cat("Creating median-only version...\n")
p_median <- ggplot(biomass_summary_climate, aes(x = Year, y = median_t, color = Species)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = obs_data,
             aes(x = Year, y = ObsBiomass_t), 
             inherit.aes = FALSE, color = "black", size = 1) +
  facet_wrap(~Species, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(legend.position = "none", 
        strip.text = element_text(face = "bold")) +
  labs(
    title = sprintf("Monte Carlo: Climate-Only Biomass vs Observations (%d sims)", n_sims),
    subtitle = "Median only",
    x = "Year", 
    y = "Biomass [t]"
  )

ggsave(file.path(output_dir, "climate_only_biomass_timeseries_median.png"), 
       p_median, width = 12, height = 10, dpi = 300)
cat("  Saved: climate_only_biomass_timeseries_median.png\n")

# --- Version 2: 25-75 percentile ---
cat("Creating 25-75 percentile version...\n")
p_q50 <- ggplot(biomass_summary_climate, aes(x = Year, y = median_t, color = Species, fill = Species)) +
  geom_ribbon(aes(ymin = q25_t, ymax = q75_t), alpha = 0.3, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(data = obs_data,
             aes(x = Year, y = ObsBiomass_t), 
             inherit.aes = FALSE, color = "black", size = 1) +
  facet_wrap(~Species, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(legend.position = "none", 
        strip.text = element_text(face = "bold")) +
  labs(
    title = sprintf("Monte Carlo: Climate-Only Biomass vs Observations (%d sims)", n_sims),
    subtitle = "Median with 25-75th percentile range (interquartile range)",
    x = "Year", 
    y = "Biomass [t]"
  )

ggsave(file.path(output_dir, "climate_only_biomass_timeseries_q25_75.png"), 
       p_q50, width = 12, height = 10, dpi = 300)
cat("  Saved: climate_only_biomass_timeseries_q25_75.png\n")

# --- Version 3: 5-95 percentile ---
cat("Creating 5-95 percentile version...\n")
p_q90 <- ggplot(biomass_summary_climate, aes(x = Year, y = median_t, color = Species, fill = Species)) +
  geom_ribbon(aes(ymin = q05_t, ymax = q95_t), alpha = 0.3, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(data = obs_data,
             aes(x = Year, y = ObsBiomass_t), 
             inherit.aes = FALSE, color = "black", size = 1) +
  facet_wrap(~Species, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(legend.position = "none", 
        strip.text = element_text(face = "bold")) +
  labs(
    title = sprintf("Monte Carlo: Climate-Only Biomass vs Observations (%d sims)", n_sims),
    subtitle = "Median with 5-95th percentile range (90% credible interval)",
    x = "Year", 
    y = "Biomass [t]"
  )

ggsave(file.path(output_dir, "climate_only_biomass_timeseries_q05_95.png"), 
       p_q90, width = 12, height = 10, dpi = 300)
cat("  Saved: climate_only_biomass_timeseries_q05_95.png\n")

# ============================================================================
# PART 2: Biomass Density Spectra using plotSpectra()
# ============================================================================
cat("\n--- Part 2: Biomass Density Spectra ---\n")

# Load ensembles
cat("Loading ensembles for spectra plots...\n")
climate_ensemble <- readRDS("Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds")
fished_ensemble <- readRDS("Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds")

# Reference period for spectra
ref_years <- 2001:2010

# Function to get averaged simulation for a period
# We'll use the median simulation (by total biomass) as representative
get_representative_sim <- function(sim_list, years) {
  # Calculate total biomass at reference period for each simulation
  total_biomass <- sapply(seq_along(sim_list), function(i) {
    sim <- sim_list[[i]]
    if (is.null(sim)) return(NA)
    bm <- tryCatch(getBiomass(sim), error = function(e) NULL)
    if (is.null(bm)) return(NA)
    year_rows <- as.numeric(rownames(bm)) %in% years
    if (!any(year_rows)) return(NA)
    sum(bm[year_rows, ], na.rm = TRUE)
  })
  
  # Find median simulation
  valid <- which(!is.na(total_biomass))
  if (length(valid) == 0) return(NULL)
  
  median_idx <- valid[which.min(abs(total_biomass[valid] - median(total_biomass[valid], na.rm = TRUE)))]
  return(sim_list[[median_idx]])
}

cat("Selecting representative simulations...\n")
climate_rep <- get_representative_sim(climate_ensemble$simulations, ref_years)
fished_rep <- get_representative_sim(fished_ensemble$simulations, ref_years)

if (is.null(climate_rep) || is.null(fished_rep)) {
  cat("WARNING: Could not find representative simulations\n")
} else {
  cat("  Found representative simulations for spectra plots\n")
  
  # Create spectra plots using plotSpectra with different power values
  
  # --- Power = 1: Biomass density ---
  cat("Creating biomass density spectrum (power=1)...\n")
  
  # For climate-only
  p_spec_climate_p1 <- plotSpectra(climate_rep, time_range = ref_years, power = 1, total = TRUE) +
    labs(
      title = "Climate-Only: Biomass Density Spectrum",
      subtitle = sprintf("Reference Period (%d-%d), power=1", min(ref_years), max(ref_years)),
      y = "Biomass density (g/m²)"
    ) +
    theme_bw()
  
  ggsave(file.path(output_dir, "climate_only_spectra_power1.png"), 
         p_spec_climate_p1, width = 10, height = 6, dpi = 300)
  cat("  Saved: climate_only_spectra_power1.png\n")
  
  # For fished
  p_spec_fished_p1 <- plotSpectra(fished_rep, time_range = ref_years, power = 1, total = TRUE) +
    labs(
      title = "Fished: Biomass Density Spectrum",
      subtitle = sprintf("Reference Period (%d-%d), power=1", min(ref_years), max(ref_years)),
      y = "Biomass density (g/m²)"
    ) +
    theme_bw()
  
  ggsave(file.path(output_dir, "fished_spectra_power1.png"), 
         p_spec_fished_p1, width = 10, height = 6, dpi = 300)
  cat("  Saved: fished_spectra_power1.png\n")
  
  # --- Power = 2: Biomass density w.r.t. logarithmic size bins ---
  cat("Creating biomass density spectrum (power=2)...\n")
  
  # For climate-only
  p_spec_climate_p2 <- plotSpectra(climate_rep, time_range = ref_years, power = 2, total = TRUE) +
    labs(
      title = "Climate-Only: Biomass Density Spectrum (log bins)",
      subtitle = sprintf("Reference Period (%d-%d), power=2", min(ref_years), max(ref_years)),
      y = "Biomass density w.r.t. log size bins"
    ) +
    theme_bw()
  
  ggsave(file.path(output_dir, "climate_only_spectra_power2.png"), 
         p_spec_climate_p2, width = 10, height = 6, dpi = 300)
  cat("  Saved: climate_only_spectra_power2.png\n")
  
  # For fished
  p_spec_fished_p2 <- plotSpectra(fished_rep, time_range = ref_years, power = 2, total = TRUE) +
    labs(
      title = "Fished: Biomass Density Spectrum (log bins)",
      subtitle = sprintf("Reference Period (%d-%d), power=2", min(ref_years), max(ref_years)),
      y = "Biomass density w.r.t. log size bins"
    ) +
    theme_bw()
  
  ggsave(file.path(output_dir, "fished_spectra_power2.png"), 
         p_spec_fished_p2, width = 10, height = 6, dpi = 300)
  cat("  Saved: fished_spectra_power2.png\n")
  
  # --- Combined comparison plots ---
  cat("Creating combined comparison spectra...\n")
  
  # Extract data from plotSpectra for custom combined plot
  # We'll create side-by-side comparison
  
  # Power = 1 comparison
  p1_climate <- plotSpectra(climate_rep, time_range = ref_years, power = 1, total = TRUE) +
    labs(title = "Climate-Only", subtitle = "power=1") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  p1_fished <- plotSpectra(fished_rep, time_range = ref_years, power = 1, total = TRUE) +
    labs(title = "Fished", subtitle = "power=1") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  library(patchwork)
  p_combined_p1 <- p1_climate + p1_fished + 
    plot_layout(guides = "collect") +
    plot_annotation(
      title = "Community Biomass Density Spectrum: Climate-Only vs Fished",
      subtitle = sprintf("Reference Period (%d-%d), power=1", min(ref_years), max(ref_years))
    ) & theme(legend.position = "bottom")
  
  ggsave(file.path(output_dir, "spectra_comparison_power1.png"), 
         p_combined_p1, width = 14, height = 6, dpi = 300)
  cat("  Saved: spectra_comparison_power1.png\n")
  
  # Power = 2 comparison
  p2_climate <- plotSpectra(climate_rep, time_range = ref_years, power = 2, total = TRUE) +
    labs(title = "Climate-Only", subtitle = "power=2") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  p2_fished <- plotSpectra(fished_rep, time_range = ref_years, power = 2, total = TRUE) +
    labs(title = "Fished", subtitle = "power=2") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  p_combined_p2 <- p2_climate + p2_fished + 
    plot_layout(guides = "collect") +
    plot_annotation(
      title = "Community Biomass Density Spectrum (log bins): Climate-Only vs Fished",
      subtitle = sprintf("Reference Period (%d-%d), power=2", min(ref_years), max(ref_years))
    ) & theme(legend.position = "bottom")
  
  ggsave(file.path(output_dir, "spectra_comparison_power2.png"), 
         p_combined_p2, width = 14, height = 6, dpi = 300)
  cat("  Saved: spectra_comparison_power2.png\n")
}

# ============================================================================
# Summary
# ============================================================================
cat("\n=== All Plots Generated ===\n")
cat("Output files in:", output_dir, "\n\n")
cat("Biomass Timeseries:\n")
cat("  1. climate_only_biomass_timeseries_median.png (median only)\n")
cat("  2. climate_only_biomass_timeseries_q25_75.png (25-75th percentile)\n")
cat("  3. climate_only_biomass_timeseries_q05_95.png (5-95th percentile)\n\n")
cat("Biomass Density Spectra:\n")
cat("  4. climate_only_spectra_power1.png (biomass density)\n")
cat("  5. fished_spectra_power1.png (biomass density)\n")
cat("  6. climate_only_spectra_power2.png (log bins)\n")
cat("  7. fished_spectra_power2.png (log bins)\n")
cat("  8. spectra_comparison_power1.png (side-by-side)\n")
cat("  9. spectra_comparison_power2.png (side-by-side)\n")
