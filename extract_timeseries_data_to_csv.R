# Extract Time-Series Resolved Biomass and Yield Data
# Creates CSV files with min, median, max for each year and species

cat("Extracting Time-Series Resolved Data...\n\n")

library(dplyr)
library(tidyr)
library(mizer)
library(reshape2)
library(ggplot2)

# ==============================================================================
# 1. Load the Monte Carlo results (all 2112 simulations)
# ==============================================================================
cat("Loading Monte Carlo results...\n")

mc_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/combined_rerun_successful_sims_20250923_122211.rds"

if (!file.exists(mc_file)) {
  mc_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/combined_rerun_successful_sims_20250917_103731.rds"
}

mc_results <- readRDS(mc_file)

cat("  Loaded file:", basename(mc_file), "\n")
cat("  Total simulations:", mc_results$n_successful, "\n")

# ==============================================================================
# 2. Extract biomass time series from all simulations
# ==============================================================================
cat("\nExtracting biomass time series from all simulations...\n")

extract_biomass_from_sim <- function(sim) {
  bm <- try(getBiomass(sim), silent = TRUE)
  if (inherits(bm, "try-error") || is.null(bm)) return(NULL)
  
  bm_df <- reshape2::melt(bm, varnames = c("Year", "Species"), value.name = "Biomass")
  bm_df$Year <- as.numeric(as.character(bm_df$Year))
  bm_df$Species <- as.character(bm_df$Species)
  return(bm_df)
}

all_biomass <- list()
for (i in seq_along(mc_results$simulations)) {
  if (i %% 100 == 0) cat("  Processing simulation", i, "of", length(mc_results$simulations), "\n")
  
  sim_data <- extract_biomass_from_sim(mc_results$simulations[[i]])
  if (!is.null(sim_data)) {
    sim_data$sim_id <- i
    all_biomass[[i]] <- sim_data
  }
}

biomass_df <- bind_rows(all_biomass)
cat("  Extracted biomass from", length(unique(biomass_df$sim_id)), "simulations\n")

# ==============================================================================
# 3. Calculate yearly min, median, max biomass by species
# ==============================================================================
cat("\nCalculating yearly biomass statistics...\n")

biomass_timeseries <- biomass_df %>%
  group_by(Year, Species) %>%
  summarise(
    min_biomass_t = min(Biomass, na.rm = TRUE) / 1e6,
    median_biomass_t = median(Biomass, na.rm = TRUE) / 1e6,
    max_biomass_t = max(Biomass, na.rm = TRUE) / 1e6,
    q05_biomass_t = quantile(Biomass, 0.05, na.rm = TRUE) / 1e6,
    q95_biomass_t = quantile(Biomass, 0.95, na.rm = TRUE) / 1e6,
    .groups = 'drop'
  )

cat("  Years covered:", min(biomass_timeseries$Year), "to", max(biomass_timeseries$Year), "\n")
cat("  Total rows:", nrow(biomass_timeseries), "\n")

# ==============================================================================
# 4. Add observed biomass data
# ==============================================================================
cat("\nAdding observed biomass data...\n")

obs_biomass_data <- data.frame(
  Species = c("mesozooplankton","other krill","other macrozooplankton","antarctic krill",
              "salps","mesopelagic fishes","bathypelagic fishes","shelf and coastal fishes",
              "flying birds","small divers","squids","toothfishes","leopard seals",
              "medium divers","large divers","minke whales","orca","sperm whales","baleen whales"),
  ObsBiomass_t = c(1.297420e+13, 2.801248e+12, 1.474341e+13, 5.897364e+12, 9.612703e+11, 
                   1.769209e+12, 1.769209e+12, 4.027900e+12, 4.423023e+09, 2.358946e+10, 
                   2.211512e+11, 1.105756e+12, 2.948682e+09, 3.907004e+11, 1.621775e+10, 
                   2.064077e+10, 8.846046e+09, 1.621775e+10, 1.872413e+11) / 1e6
)

# Merge observed data (applies to 2010-2020 period)
biomass_timeseries <- biomass_timeseries %>%
  left_join(obs_biomass_data, by = "Species")

# ==============================================================================
# 5. Extract yield time series from all simulations
# ==============================================================================
cat("\nExtracting yield time series from all simulations...\n")

extract_yield_from_sim <- function(sim) {
  yld <- try(getYield(sim), silent = TRUE)
  if (inherits(yld, "try-error") || is.null(yld)) return(NULL)
  
  yld_df <- reshape2::melt(yld, varnames = c("Year", "Species"), value.name = "Yield")
  yld_df$Year <- as.numeric(as.character(yld_df$Year))
  yld_df$Species <- as.character(yld_df$Species)
  return(yld_df)
}

all_yield <- list()
for (i in seq_along(mc_results$simulations)) {
  if (i %% 100 == 0) cat("  Processing simulation", i, "of", length(mc_results$simulations), "\n")
  
  sim_data <- extract_yield_from_sim(mc_results$simulations[[i]])
  if (!is.null(sim_data)) {
    sim_data$sim_id <- i
    all_yield[[i]] <- sim_data
  }
}

yield_df <- bind_rows(all_yield)
cat("  Extracted yield from", length(unique(yield_df$sim_id)), "simulations\n")

# ==============================================================================
# 6. Calculate yearly min, median, max yield by species
# ==============================================================================
cat("\nCalculating yearly yield statistics...\n")

# Filter to species with actual yields
species_with_yield <- yield_df %>%
  group_by(Species) %>%
  summarise(max_yield = max(Yield, na.rm = TRUE), .groups = 'drop') %>%
  filter(max_yield > 0) %>%
  pull(Species)

cat("  Species with yields:", paste(species_with_yield, collapse = ", "), "\n")

yield_timeseries <- yield_df %>%
  filter(Species %in% species_with_yield) %>%
  group_by(Year, Species) %>%
  summarise(
    min_yield_t = min(Yield, na.rm = TRUE) / 1e6,
    median_yield_t = median(Yield, na.rm = TRUE) / 1e6,
    max_yield_t = max(Yield, na.rm = TRUE) / 1e6,
    q05_yield_t = quantile(Yield, 0.05, na.rm = TRUE) / 1e6,
    q95_yield_t = quantile(Yield, 0.95, na.rm = TRUE) / 1e6,
    .groups = 'drop'
  )

cat("  Years covered:", min(yield_timeseries$Year), "to", max(yield_timeseries$Year), "\n")
cat("  Total rows:", nrow(yield_timeseries), "\n")

# ==============================================================================
# 7. Add observed yield data
# ==============================================================================
cat("\nAdding observed yield data...\n")

yield_ts_tidy <- readRDS("yield_observed_timeseries_tidy.RDS")

# Convert to tonnes
obs_yield_ts <- yield_ts_tidy %>%
  mutate(ObsYield_t = Yield / 1e6) %>%
  select(Year, Species, ObsYield_t)

# Merge observed data
yield_timeseries <- yield_timeseries %>%
  left_join(obs_yield_ts, by = c("Year", "Species"))

# ==============================================================================
# 8. Save time-series data to CSV files
# ==============================================================================
cat("\nSaving time-series data to CSV files...\n")

dir.create("monte_carlo_2111_summaries", showWarnings = FALSE)

write.csv(biomass_timeseries, 
          "monte_carlo_2111_summaries/biomass_timeseries_min_median_max.csv",
          row.names = FALSE)
cat("  ✓ Saved: biomass_timeseries_min_median_max.csv\n")

write.csv(yield_timeseries,
          "monte_carlo_2111_summaries/yield_timeseries_min_median_max.csv",
          row.names = FALSE)
cat("  ✓ Saved: yield_timeseries_min_median_max.csv\n")

# ==============================================================================
# 9. Recreate biomass figure to verify
# ==============================================================================
cat("\nRecreating biomass figure from CSV data...\n")

# Species order (smallest to largest)
species_order <- c(
  "mesozooplankton", "other krill", "other macrozooplankton", 
  "antarctic krill", "salps", "mesopelagic fishes", "bathypelagic fishes", 
  "shelf and coastal fishes", "flying birds", "small divers", "squids", 
  "toothfishes", "leopard seals", "medium divers", "large divers", 
  "minke whales", "orca", "sperm whales", "baleen whales"
)

biomass_plot_data <- biomass_timeseries %>%
  mutate(Species = factor(Species, levels = species_order))

# Create observed data for 2010-2020
obs_biomass_ts <- obs_biomass_data %>%
  mutate(Species = factor(Species, levels = species_order)) %>%
  crossing(Year = 2010:2020) %>%
  mutate(
    Lower = ObsBiomass_t * 0.75,
    Upper = ObsBiomass_t * 1.25
  )

# Create plot
p_biomass <- ggplot() +
  geom_ribbon(data = biomass_plot_data, 
              aes(x = Year, ymin = q05_biomass_t, ymax = q95_biomass_t, fill = Species),
              alpha = 0.2) +
  geom_ribbon(data = biomass_plot_data,
              aes(x = Year, ymin = min_biomass_t, ymax = max_biomass_t, fill = Species),
              alpha = 0.1) +
  geom_line(data = biomass_plot_data,
            aes(x = Year, y = median_biomass_t, color = Species),
            linewidth = 1.1) +
  geom_ribbon(data = obs_biomass_ts,
              aes(x = Year, ymin = Lower, ymax = Upper),
              alpha = 0.2, fill = "gray50") +
  geom_point(data = obs_biomass_ts,
             aes(x = Year, y = ObsBiomass_t, color = Species),
             size = 0.8) +
  geom_point(data = obs_biomass_ts,
             aes(x = Year, y = ObsBiomass_t),
             shape = 1, size = 0.8, colour = "black") +
  facet_wrap(~Species, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  labs(
    title = paste("Monte Carlo: Biomass vs Observations (", mc_results$n_successful, " sims)"),
    x = "Year",
    y = "Biomass [t]"
  )

ggsave("monte_carlo_2111_summaries/biomass_timeseries_verification.png", 
       p_biomass, 
       width = 12, 
       height = 10, 
       dpi = 300)
cat("  ✓ Saved: biomass_timeseries_verification.png\n")

# ==============================================================================
# 10. Recreate yield figure to verify
# ==============================================================================
cat("\nRecreating yield figure from CSV data...\n")

# Custom label function for yield
label_dynamic_decimals <- function(x) {
  sci <- scales::label_scientific(digits = 1)
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (v <= 0) return("0")
    if (v >= 1) paste0(formatC(v, format = "f", digits = 0, big.mark = ","), " t")
    else if (v < 1e-6) paste0(sci(v), " t")
    else {
      dp <- max(1, ceiling(-log10(v)))
      dp <- min(dp, 6)
      paste0(formatC(v, format = "f", digits = dp), " t")
    }
  }, character(1))
}

yield_plot_data <- yield_timeseries %>%
  mutate(Species = factor(Species, levels = species_order))

# Filter observed yield to positive values
obs_yield_filtered <- obs_yield_ts %>%
  filter(Species %in% species_with_yield, ObsYield_t > 0) %>%
  mutate(Species = factor(Species, levels = species_order))

# Determine year range
baleen_start <- obs_yield_filtered %>%
  filter(Species == "baleen whales") %>%
  summarise(min_year = min(Year, na.rm = TRUE)) %>%
  pull(min_year)

start_year <- if (!is.null(baleen_start) && length(baleen_start) == 1 && is.finite(baleen_start)) {
  baleen_start
} else {
  min(obs_yield_filtered$Year, na.rm = TRUE)
}

end_year <- max(yield_plot_data$Year, na.rm = TRUE)

# Create plot
p_yield <- ggplot() +
  geom_ribbon(data = yield_plot_data,
              aes(x = Year, ymin = q05_yield_t, ymax = q95_yield_t, fill = Species),
              alpha = 0.2) +
  geom_ribbon(data = yield_plot_data,
              aes(x = Year, ymin = min_yield_t, ymax = max_yield_t, fill = Species),
              alpha = 0.1) +
  geom_line(data = yield_plot_data,
            aes(x = Year, y = median_yield_t, color = Species),
            linewidth = 1.0, linetype = "solid", lineend = "round") +
  geom_point(data = obs_yield_filtered,
             aes(x = Year, y = ObsYield_t, colour = Species),
             size = 1) +
  geom_point(data = obs_yield_filtered,
             aes(x = Year, y = ObsYield_t),
             shape = 1, size = 1, colour = "black") +
  geom_vline(xintercept = 1961, linetype = "dashed") +
  geom_vline(xintercept = 2010, linetype = "dashed") +
  scale_y_log10(breaks = scales::log_breaks(n = 6), labels = label_dynamic_decimals) +
  coord_cartesian(xlim = c(start_year, end_year)) +
  facet_wrap(~Species, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none", strip.text = element_text(face = "bold")) +
  labs(
    x = "Year", 
    y = "Yield [t/year]", 
    title = paste("Monte Carlo: Yield vs Observations (", mc_results$n_successful, " sims)")
  )

ggsave("monte_carlo_2111_summaries/yield_timeseries_verification.png", 
       p_yield, 
       width = 12, 
       height = 10, 
       dpi = 300)
cat("  ✓ Saved: yield_timeseries_verification.png\n")

# ==============================================================================
# 11. Summary
# ==============================================================================
cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   TIME-SERIES EXTRACTION COMPLETE!                           ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

cat("Summary of extracted data:\n")
cat("  - Number of simulations:", mc_results$n_successful, "\n")
cat("  - Biomass years:", min(biomass_timeseries$Year), "-", max(biomass_timeseries$Year), "\n")
cat("  - Biomass species:", length(unique(biomass_timeseries$Species)), "\n")
cat("  - Biomass rows:", nrow(biomass_timeseries), "\n\n")
cat("  - Yield years:", min(yield_timeseries$Year), "-", max(yield_timeseries$Year), "\n")
cat("  - Yield species:", length(unique(yield_timeseries$Species)), "\n")
cat("  - Yield rows:", nrow(yield_timeseries), "\n\n")

cat("Files created:\n")
cat("  1. biomass_timeseries_min_median_max.csv\n")
cat("  2. yield_timeseries_min_median_max.csv\n")
cat("  3. biomass_timeseries_verification.png\n")
cat("  4. yield_timeseries_verification.png\n\n")

cat("Each CSV contains:\n")
cat("  - Year: Year of the time series\n")
cat("  - Species: Species name\n")
cat("  - min_*_t: Minimum across all simulations (tonnes)\n")
cat("  - median_*_t: Median across all simulations (tonnes)\n")
cat("  - max_*_t: Maximum across all simulations (tonnes)\n")
cat("  - q05_*_t: 5th percentile (tonnes)\n")
cat("  - q95_*_t: 95th percentile (tonnes)\n")
cat("  - Obs*_t: Observed values (tonnes)\n\n")

cat("✓ Verification plots created - compare to original figures!\n")
cat("✓ All data in tonnes units only.\n")
