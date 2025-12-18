# ============================================================================
# Comprehensive Analysis of Climate-Only Ensemble
# Compares fished vs unfished (climate-only) simulations
# Creates: biomass timeseries, spectrum ratios, slope/intercept timeseries
# ============================================================================

library(mizer)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(scales)
library(patchwork)

cat("=== Climate-Only Ensemble Analysis ===\n")
cat("Started:", as.character(Sys.time()), "\n\n")

# ============================================================================
# Configuration
# ============================================================================
output_dir <- "climate_only_analysis"
dir.create(output_dir, showWarnings = FALSE)

# Key periods for analysis
periods <- list(
  "Pre-Whaling" = 1920:1929,
  "Peak-Whaling" = 1955:1965,
  "Pre-Krill" = 1964:1973,
  "Peak-Krill" = 1974:1984,
  "Post-Krill" = 1985:1995,
  "Reference" = 2001:2010
)

# Species order (smallest to largest)
species_order <- c(
  "mesozooplankton", "other krill", "other macrozooplankton", "antarctic krill",
  "salps", "mesopelagic fishes", "bathypelagic fishes", "shelf and coastal fishes",
  "flying birds", "small divers", "squids", "toothfishes", "leopard seals",
  "medium divers", "large divers", "minke whales", "orca", "sperm whales", "baleen whales"
)

# ============================================================================
# Load Data
# ============================================================================
cat("Loading ensembles...\n")

# Climate-only ensemble
climate_file <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
if (!file.exists(climate_file)) {
  stop("Climate-only ensemble not found. Run compile_climate_only_ensemble.R first.")
}
climate_ensemble <- readRDS(climate_file)
n_climate <- climate_ensemble$n_successful
cat("  Climate-only: ", n_climate, " simulations\n")

# Fished ensemble
fished_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
if (!file.exists(fished_file)) {
  stop("Fished ensemble not found.")
}
fished_ensemble <- readRDS(fished_file)
n_fished <- length(fished_ensemble$simulations)
cat("  Fished: ", n_fished, " simulations\n")

# Observed biomass data
obs_biomass_data <- data.frame(
  Species = species_order,
  ObsBiomass_g = c(1.297420e+13, 2.801248e+12, 1.474341e+13, 5.897364e+12, 9.612703e+11, 
                   1.769209e+12, 1.769209e+12, 4.027900e+12, 4.423023e+09, 2.358946e+10, 
                   2.211512e+11, 1.105756e+12, 2.948682e+09, 3.907004e+11, 1.621775e+10, 
                   2.064077e+10, 8.846046e+09, 1.621775e+10, 1.872413e+11)
)
obs_biomass_data$ObsBiomass_t <- obs_biomass_data$ObsBiomass_g / 1e6

# ============================================================================
# PART 1: Extract Biomass Timeseries
# ============================================================================
cat("\n--- Part 1: Biomass Timeseries ---\n")

extract_biomass_df <- function(sim_list, scenario_name) {
  all_bm <- list()
  n <- length(sim_list)
  
  cat("  Extracting biomass from", n, "simulations...")
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  
  for (i in seq_len(n)) {
    sim <- sim_list[[i]]
    if (is.null(sim)) next
    
    bm <- tryCatch(getBiomass(sim), error = function(e) NULL)
    if (is.null(bm)) next
    
    df <- melt(bm)
    names(df) <- c("Year", "Species", "Biomass")
    df$Year <- as.numeric(as.character(df$Year))
    df$sim_id <- i
    df$scenario <- scenario_name
    all_bm[[length(all_bm) + 1]] <- df
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  result <- bind_rows(all_bm)
  result$Biomass_t <- result$Biomass / 1e6
  cat(" done\n")
  return(result)
}

# Extract biomass from both ensembles
climate_biomass <- extract_biomass_df(climate_ensemble$simulations, "Climate-Only")
fished_biomass <- extract_biomass_df(fished_ensemble$simulations, "Fished")

# Combine for comparison
all_biomass <- bind_rows(climate_biomass, fished_biomass)

cat("  Climate-only biomass rows:", nrow(climate_biomass), "\n")
cat("  Fished biomass rows:", nrow(fished_biomass), "\n")

# ============================================================================
# PART 1a: Summary statistics for climate-only
# ============================================================================
cat("\nCalculating per-year biomass statistics...\n")

biomass_summary_climate <- climate_biomass %>%
  group_by(Year, Species) %>%
  summarise(
    n = n(),
    min_t = min(Biomass_t, na.rm = TRUE),
    q05_t = quantile(Biomass_t, 0.05, na.rm = TRUE),
    q25_t = quantile(Biomass_t, 0.25, na.rm = TRUE),
    median_t = median(Biomass_t, na.rm = TRUE),
    q75_t = quantile(Biomass_t, 0.75, na.rm = TRUE),
    q95_t = quantile(Biomass_t, 0.95, na.rm = TRUE),
    max_t = max(Biomass_t, na.rm = TRUE),
    .groups = 'drop'
  )

# Add observed biomass for reference period
obs_years <- 2001:2020
obs_biomass_ts <- crossing(Year = obs_years, obs_biomass_data %>% select(Species, ObsBiomass_t))
biomass_summary_climate <- biomass_summary_climate %>%
  left_join(obs_biomass_ts, by = c("Year", "Species"))

# Save summary CSV
write.csv(biomass_summary_climate, 
          file.path(output_dir, "climate_only_biomass_timeseries_summary.csv"), 
          row.names = FALSE)
cat("  Saved: climate_only_biomass_timeseries_summary.csv\n")

# ============================================================================
# PART 1b: Create Biomass Timeseries Plot (matching attached figure)
# ============================================================================
cat("\nCreating biomass timeseries plot...\n")

# Set species as ordered factor
biomass_summary_climate$Species <- factor(biomass_summary_climate$Species, levels = species_order)

n_sims <- climate_ensemble$n_simulations

p_biomass <- ggplot(biomass_summary_climate, aes(x = Year, y = median_t, color = Species, fill = Species)) +
  # 5-95 percentile ribbon (90% credible interval)
  geom_ribbon(aes(ymin = q05_t, ymax = q95_t), alpha = 0.2, color = NA) +
  # 25-75 percentile ribbon (50% credible interval)
  geom_ribbon(aes(ymin = q25_t, ymax = q75_t), alpha = 0.3, color = NA) +
  # Median line
  geom_line(linewidth = 0.8) +
  # Observation points (reference period)
  geom_point(data = biomass_summary_climate %>% 
               filter(!is.na(ObsBiomass_t) & Year >= 2001 & Year <= 2020),
             aes(x = Year, y = ObsBiomass_t), 
             inherit.aes = FALSE, color = 'black', size = 1) +
  facet_wrap(~Species, scales = 'free_y') +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(legend.position = 'none', 
        strip.text = element_text(face = 'bold')) +
  labs(
    x = 'Year', 
    y = 'Biomass [t]'
  )

ggsave(file.path(output_dir, "climate_only_biomass_timeseries.png"), 
       p_biomass, width = 12, height = 10, dpi = 300)
cat("  Saved: climate_only_biomass_timeseries.png\n")

# ============================================================================
# PART 2: Community Size Spectrum Ratio Analysis
# ============================================================================
cat("\n--- Part 2: Size Spectrum Ratio Analysis ---\n")

# Define size bins (log scale)
w_breaks <- 10^seq(-3, 9, by = 0.5)  # 1 mg to 1000 tonnes
w_labels <- paste0("10^", seq(-3, 8.5, by = 0.5))

# Function to get community spectrum for a simulation at a given time
get_community_spectrum <- function(sim, years) {
  # Get species params
  sp <- sim@params@species_params
  w <- sim@params@w
  
  # Average across years
  n_array <- sim@n[as.character(years), , , drop = FALSE]
  n_mean <- apply(n_array, c(2, 3), mean, na.rm = TRUE)
  
  # Sum across species to get community spectrum
  community_n <- colSums(n_mean, na.rm = TRUE)
  
  return(data.frame(
    w = w,
    abundance = community_n,
    biomass = community_n * w
  ))
}

# Calculate spectrum for reference period (2001-2010)
ref_years <- periods$Reference

cat("Calculating community spectra for reference period...\n")

# For fished simulations
fished_spectra <- list()
for (i in seq_along(fished_ensemble$simulations)) {
  if (i %% 500 == 0) cat("  Fished:", i, "/", n_fished, "\n")
  sim <- fished_ensemble$simulations[[i]]
  if (is.null(sim)) next
  spec <- tryCatch(get_community_spectrum(sim, ref_years), error = function(e) NULL)
  if (!is.null(spec)) {
    spec$sim_id <- i
    fished_spectra[[length(fished_spectra) + 1]] <- spec
  }
}

# For climate-only simulations
climate_spectra <- list()
for (i in seq_along(climate_ensemble$simulations)) {
  if (i %% 500 == 0) cat("  Climate-only:", i, "/", climate_ensemble$n_simulations, "\n")
  sim <- climate_ensemble$simulations[[i]]
  if (is.null(sim)) next
  spec <- tryCatch(get_community_spectrum(sim, ref_years), error = function(e) NULL)
  if (!is.null(spec)) {
    spec$sim_id <- i
    climate_spectra[[length(climate_spectra) + 1]] <- spec
  }
}

fished_spec_df <- bind_rows(fished_spectra)
climate_spec_df <- bind_rows(climate_spectra)

cat("  Fished spectra:", length(unique(fished_spec_df$sim_id)), "simulations\n")
cat("  Climate-only spectra:", length(unique(climate_spec_df$sim_id)), "simulations\n")

# Calculate mean spectra
fished_mean_spec <- fished_spec_df %>%
  group_by(w) %>%
  summarise(
    mean_abundance = mean(abundance, na.rm = TRUE),
    mean_biomass = mean(biomass, na.rm = TRUE),
    q25_biomass = quantile(biomass, 0.25, na.rm = TRUE),
    q75_biomass = quantile(biomass, 0.75, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(scenario = "Fished")

climate_mean_spec <- climate_spec_df %>%
  group_by(w) %>%
  summarise(
    mean_abundance = mean(abundance, na.rm = TRUE),
    mean_biomass = mean(biomass, na.rm = TRUE),
    q25_biomass = quantile(biomass, 0.25, na.rm = TRUE),
    q75_biomass = quantile(biomass, 0.75, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(scenario = "Climate-Only")

# Calculate ratio (Fished / Climate-only)
ratio_spec <- fished_mean_spec %>%
  select(w, fished_biomass = mean_biomass) %>%
  inner_join(climate_mean_spec %>% select(w, climate_biomass = mean_biomass), by = "w") %>%
  mutate(
    ratio = fished_biomass / climate_biomass,
    log10_w = log10(w)
  )

# Define size categories
ratio_spec <- ratio_spec %>%
  mutate(
    size_category = case_when(
      w < 1 ~ "Small (<1g)",
      w < 1000 ~ "Medium (1g-1kg)",
      w < 1e6 ~ "Large (1kg-1t)",
      TRUE ~ "Very Large (>1t)"
    )
  )

# Save ratio data
write.csv(ratio_spec, file.path(output_dir, "fished_vs_climate_spectrum_ratio.csv"), row.names = FALSE)
cat("  Saved: fished_vs_climate_spectrum_ratio.csv\n")

# ============================================================================
# PART 2a: Plot spectrum comparison and ratio
# ============================================================================
cat("\nCreating spectrum comparison plots...\n")

# Combined spectrum comparison
spec_combined <- bind_rows(fished_mean_spec, climate_mean_spec)

p_spec_compare <- ggplot(spec_combined, aes(x = w, y = mean_biomass, color = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = q25_biomass, ymax = q75_biomass), alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values = c("Fished" = "red", "Climate-Only" = "blue")) +
  scale_fill_manual(values = c("Fished" = "red", "Climate-Only" = "blue")) +
  theme_bw() +
  labs(
    title = "Community Size Spectrum: Fished vs Climate-Only",
    subtitle = paste0("Reference Period (", min(ref_years), "-", max(ref_years), "), ", n_sims, " simulations"),
    x = "Body mass (g)",
    y = "Biomass density (g/m²)",
    color = "Scenario",
    fill = "Scenario"
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "spectrum_comparison_reference_period.png"), 
       p_spec_compare, width = 10, height = 6, dpi = 300)
cat("  Saved: spectrum_comparison_reference_period.png\n")

# Ratio plot
p_ratio <- ggplot(ratio_spec, aes(x = w, y = ratio)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_line(size = 1, color = "darkgreen") +
  geom_point(aes(color = size_category), size = 2) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(
    values = c("Small (<1g)" = "#1b9e77", "Medium (1g-1kg)" = "#d95f02", 
               "Large (1kg-1t)" = "#7570b3", "Very Large (>1t)" = "#e7298a")
  ) +
  theme_bw() +
  labs(
    title = "Fishing Impact on Community Size Spectrum",
    subtitle = paste0("Ratio: Fished / Climate-Only (Reference Period ", min(ref_years), "-", max(ref_years), ")"),
    x = "Body mass (g)",
    y = "Biomass Ratio (Fished / Unfished)",
    color = "Size Category"
  ) +
  theme(legend.position = "bottom") +
  annotate("text", x = 1e8, y = 0.7, label = "Fishing depletes\nlarger sizes", 
           hjust = 1, fontface = "italic", color = "gray40")

ggsave(file.path(output_dir, "spectrum_ratio_reference_period.png"), 
       p_ratio, width = 10, height = 6, dpi = 300)
cat("  Saved: spectrum_ratio_reference_period.png\n")

# ============================================================================
# PART 3: Size Spectrum Slope and Intercept Timeseries
# ============================================================================
cat("\n--- Part 3: Slope and Intercept Timeseries ---\n")

# Function to fit log-linear regression to community spectrum
fit_spectrum <- function(sim, year) {
  # Get abundance at specific year
  year_char <- as.character(year)
  if (!(year_char %in% dimnames(sim@n)$time)) return(NULL)
  
  n_matrix <- sim@n[year_char, , , drop = FALSE]
  n_sum <- apply(n_matrix, 3, sum, na.rm = TRUE)  # Sum across species
  
  w <- sim@params@w
  
  # Filter to positive values
  valid <- n_sum > 0 & is.finite(n_sum)
  if (sum(valid) < 5) return(NULL)
  
  # Fit log-linear regression: log(N) ~ log(w)
  log_w <- log10(w[valid])
  log_n <- log10(n_sum[valid])
  
  fit <- tryCatch(lm(log_n ~ log_w), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  
  return(data.frame(
    year = year,
    slope = coef(fit)[2],
    intercept = coef(fit)[1],
    r_squared = summary(fit)$r.squared
  ))
}

# Extract slope/intercept for all years and simulations
years <- 1841:2010

cat("Calculating slope/intercept for fished simulations...\n")
fished_slopes <- list()
pb <- txtProgressBar(min = 0, max = n_fished, style = 3)
for (i in seq_along(fished_ensemble$simulations)) {
  sim <- fished_ensemble$simulations[[i]]
  if (is.null(sim)) next
  
  for (yr in years) {
    result <- tryCatch(fit_spectrum(sim, yr), error = function(e) NULL)
    if (!is.null(result)) {
      result$sim_id <- i
      fished_slopes[[length(fished_slopes) + 1]] <- result
    }
  }
  setTxtProgressBar(pb, i)
}
close(pb)

cat("\nCalculating slope/intercept for climate-only simulations...\n")
climate_slopes <- list()
pb <- txtProgressBar(min = 0, max = n_climate, style = 3)
for (i in seq_along(climate_ensemble$simulations)) {
  sim <- climate_ensemble$simulations[[i]]
  if (is.null(sim)) next
  
  for (yr in years) {
    result <- tryCatch(fit_spectrum(sim, yr), error = function(e) NULL)
    if (!is.null(result)) {
      result$sim_id <- i
      climate_slopes[[length(climate_slopes) + 1]] <- result
    }
  }
  setTxtProgressBar(pb, i)
}
close(pb)

fished_slopes_df <- bind_rows(fished_slopes) %>% mutate(scenario = "Fished")
climate_slopes_df <- bind_rows(climate_slopes) %>% mutate(scenario = "Climate-Only")

cat("\n  Fished slope/intercept records:", nrow(fished_slopes_df), "\n")
cat("  Climate-only slope/intercept records:", nrow(climate_slopes_df), "\n")

# Combine and calculate summary statistics
all_slopes <- bind_rows(fished_slopes_df, climate_slopes_df)

slope_summary <- all_slopes %>%
  group_by(year, scenario) %>%
  summarise(
    n = n(),
    slope_median = median(slope, na.rm = TRUE),
    slope_q25 = quantile(slope, 0.25, na.rm = TRUE),
    slope_q75 = quantile(slope, 0.75, na.rm = TRUE),
    slope_q05 = quantile(slope, 0.05, na.rm = TRUE),
    slope_q95 = quantile(slope, 0.95, na.rm = TRUE),
    intercept_median = median(intercept, na.rm = TRUE),
    intercept_q25 = quantile(intercept, 0.25, na.rm = TRUE),
    intercept_q75 = quantile(intercept, 0.75, na.rm = TRUE),
    intercept_q05 = quantile(intercept, 0.05, na.rm = TRUE),
    intercept_q95 = quantile(intercept, 0.95, na.rm = TRUE),
    r_squared_median = median(r_squared, na.rm = TRUE),
    .groups = 'drop'
  )

# Save slope/intercept data
write.csv(slope_summary, file.path(output_dir, "slope_intercept_timeseries_summary.csv"), row.names = FALSE)
cat("  Saved: slope_intercept_timeseries_summary.csv\n")

# ============================================================================
# PART 3a: Plot Slope/Intercept Timeseries
# ============================================================================
cat("\nCreating slope/intercept timeseries plots...\n")

# Slope plot
p_slope <- ggplot(slope_summary, aes(x = year, y = slope_median, color = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = slope_q05, ymax = slope_q95), alpha = 0.15, color = NA) +
  geom_ribbon(aes(ymin = slope_q25, ymax = slope_q75), alpha = 0.3, color = NA) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c("Fished" = "#e41a1c", "Climate-Only" = "#377eb8")) +
  scale_fill_manual(values = c("Fished" = "#e41a1c", "Climate-Only" = "#377eb8")) +
  geom_vline(xintercept = c(1920, 1961, 2010), linetype = "dashed", alpha = 0.5) +
  theme_bw() +
  labs(
    title = "Community Size Spectrum Slope Over Time",
    subtitle = paste0("Ribbons: 25-75% and 5-95% quantiles (", n_sims, " simulations)"),
    x = "Year",
    y = "Slope (log₁₀ abundance vs log₁₀ body mass)",
    color = "Scenario",
    fill = "Scenario"
  ) +
  theme(legend.position = "bottom") +
  annotate("text", x = 1920, y = min(slope_summary$slope_q05, na.rm = TRUE), 
           label = "Whaling\nstarts", hjust = 0.5, vjust = 1.2, size = 3) +
  annotate("text", x = 1961, y = min(slope_summary$slope_q05, na.rm = TRUE), 
           label = "Krill\nfishing", hjust = 0.5, vjust = 1.2, size = 3)

ggsave(file.path(output_dir, "slope_timeseries.png"), 
       p_slope, width = 12, height = 6, dpi = 300)
cat("  Saved: slope_timeseries.png\n")

# Intercept plot
p_intercept <- ggplot(slope_summary, aes(x = year, y = intercept_median, color = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = intercept_q05, ymax = intercept_q95), alpha = 0.15, color = NA) +
  geom_ribbon(aes(ymin = intercept_q25, ymax = intercept_q75), alpha = 0.3, color = NA) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c("Fished" = "#e41a1c", "Climate-Only" = "#377eb8")) +
  scale_fill_manual(values = c("Fished" = "#e41a1c", "Climate-Only" = "#377eb8")) +
  geom_vline(xintercept = c(1920, 1961, 2010), linetype = "dashed", alpha = 0.5) +
  theme_bw() +
  labs(
    title = "Community Size Spectrum Intercept Over Time",
    subtitle = paste0("Ribbons: 25-75% and 5-95% quantiles (", n_sims, " simulations)"),
    x = "Year",
    y = "Intercept (log₁₀ abundance at log₁₀(w) = 0)",
    color = "Scenario",
    fill = "Scenario"
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "intercept_timeseries.png"), 
       p_intercept, width = 12, height = 6, dpi = 300)
cat("  Saved: intercept_timeseries.png\n")

# Combined slope and intercept
p_combined <- p_slope / p_intercept + 
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Community Size Spectrum Dynamics: Fished vs Climate-Only",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  ) & theme(legend.position = "bottom")

ggsave(file.path(output_dir, "slope_intercept_combined.png"), 
       p_combined, width = 12, height = 10, dpi = 300)
cat("  Saved: slope_intercept_combined.png\n")

# ============================================================================
# PART 4: Summary Statistics
# ============================================================================
cat("\n--- Summary ---\n")

# Calculate mean ratio by size category
ratio_by_category <- ratio_spec %>%
  group_by(size_category) %>%
  summarise(
    mean_ratio = mean(ratio, na.rm = TRUE),
    min_ratio = min(ratio, na.rm = TRUE),
    max_ratio = max(ratio, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\nFishing Impact on Size Spectrum (Reference Period):\n")
print(ratio_by_category)

# Slope difference
slope_diff <- slope_summary %>%
  filter(year %in% 2001:2010) %>%
  select(year, scenario, slope_median) %>%
  pivot_wider(names_from = scenario, values_from = slope_median) %>%
  mutate(slope_diff = Fished - `Climate-Only`)

cat("\nSlope difference (Fished - Climate-Only) during reference period:\n")
cat("  Mean:", round(mean(slope_diff$slope_diff, na.rm = TRUE), 4), "\n")
cat("  (Positive = fishing steepens slope = depletes larger organisms)\n")

# ============================================================================
# Final Output Summary
# ============================================================================
cat("\n=== Analysis Complete ===\n")
cat("Finished:", as.character(Sys.time()), "\n\n")
cat("Output files in:", output_dir, "\n")
cat("  1. climate_only_biomass_timeseries_summary.csv\n")
cat("  2. climate_only_biomass_timeseries.png\n")
cat("  3. fished_vs_climate_spectrum_ratio.csv\n")
cat("  4. spectrum_comparison_reference_period.png\n")
cat("  5. spectrum_ratio_reference_period.png\n")
cat("  6. slope_intercept_timeseries_summary.csv\n")
cat("  7. slope_timeseries.png\n")
cat("  8. intercept_timeseries.png\n")
cat("  9. slope_intercept_combined.png\n")
