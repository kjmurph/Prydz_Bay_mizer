# Create publication-ready biomass and yield figures
# - Individual plots without titles
# - Combined multipanel figure with panel tags (A, B)
# Output to: publication_figures/

library(ggplot2)
library(dplyr)
library(scales)
library(patchwork)

cat("=== Creating Biomass and Yield Figures for Publication ===\n\n")

# Create output directory
output_dir <- "publication_figures"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------
cat("Loading data...\n")
outdir <- "monte_carlo_2111_summaries"
biomass_csv <- file.path(outdir, "biomass_timeseries_summary_per_year_tonnes.csv")
yield_csv <- file.path(outdir, "yield_timeseries_summary_per_year_tonnes.csv")

if (!file.exists(biomass_csv) || !file.exists(yield_csv)) {
  stop("Summary CSVs not found. Run extract_timeseries_data_to_csv.R first.")
}

biomass <- read.csv(biomass_csv, stringsAsFactors = FALSE)
yield <- read.csv(yield_csv, stringsAsFactors = FALSE)

# Species order (smallest to largest)
species_order <- c(
  "mesozooplankton","other krill","other macrozooplankton","antarctic krill",
  "salps","mesopelagic fishes","bathypelagic fishes","shelf and coastal fishes",
  "flying birds","small divers","squids","toothfishes","leopard seals",
  "medium divers","large divers","minke whales","orca","sperm whales","baleen whales"
)

biomass$Species <- factor(biomass$Species, levels = species_order)

# Climate-only biomass summary
clim_biomass_csv <- "climate_only_analysis/climate_only_biomass_timeseries_summary.csv"
if (!file.exists(clim_biomass_csv)) {
  stop("Climate-only biomass summary not found: ", clim_biomass_csv,
       "\nRun analyze_climate_only_ensemble.R first.")
}
clim_biomass <- read.csv(clim_biomass_csv, stringsAsFactors = FALSE)
clim_biomass$Species <- factor(clim_biomass$Species, levels = species_order)
cat(sprintf("  Climate-only data: %d rows\n", nrow(clim_biomass)))

# Observed biomass range per species for 2001-2010 (for grey ribbon)
obs_range <- biomass %>%
  filter(!is.na(ObsBiomass_t), Year >= 2001, Year <= 2010) %>%
  group_by(Species) %>%
  summarise(
    xmin = min(Year) - 0.5,
    xmax = max(Year) + 0.5,
    ymin = min(ObsBiomass_t, na.rm = TRUE),
    ymax = max(ObsBiomass_t, na.rm = TRUE),
    ymed = median(ObsBiomass_t, na.rm = TRUE),
    .groups = "drop"
  )

# Number of simulations
n_sims <- max(biomass$n, na.rm = TRUE)
cat(sprintf("  Loaded data for %d simulations\n", n_sims))

# ------------------------------------------------------------------------------
# Prepare yield data
# ------------------------------------------------------------------------------
cat("Preparing yield data...\n")

# Filter to species with actual yields
species_with_yield <- unique(yield$Species[yield$max_t > 0])
yield <- yield %>% filter(Species %in% species_with_yield)
yield$Species <- factor(yield$Species, levels = species_order)

# Build indicator: has_obs_pos = TRUE when ObsYield_t > 0
yield <- yield %>%
  mutate(has_obs_pos = !is.na(ObsYield_t) & ObsYield_t > 0)

# Separate frames for ribbons and line
yield_ribbon <- yield %>%
  filter(!(has_obs_pos == FALSE & (is.na(q95_t) | q95_t <= 0))) %>%
  mutate(
    q05_t = ifelse(q05_t > 0, q05_t, NA_real_),
    q25_t = ifelse(q25_t > 0, q25_t, NA_real_),
    q75_t = ifelse(q75_t > 0, q75_t, NA_real_),
    q95_t = ifelse(q95_t > 0, q95_t, NA_real_)
  ) %>%
  filter(!(is.na(q25_t) & is.na(q75_t) & is.na(q05_t) & is.na(q95_t)))

yield_line <- yield %>%
  filter(!(has_obs_pos == FALSE & (is.na(median_t) | median_t <= 0))) %>%
  mutate(median_t = ifelse(median_t > 0, median_t, NA_real_))

yield_obs <- yield %>% filter(has_obs_pos) %>% select(Year, Species, ObsYield_t)

# Determine x-axis start year
baleen_start_obs <- yield_obs %>%
  filter(Species == "baleen whales") %>%
  summarise(min_year = min(Year, na.rm = TRUE)) %>%
  pull(min_year)

start_year <- if (length(baleen_start_obs) == 1 && is.finite(baleen_start_obs)) {
  baleen_start_obs
} else {
  1930
}
end_year <- max(yield$Year, na.rm = TRUE)

# Custom y-axis label function for yield
label_dynamic_decimals <- function(x) {
  sci <- scales::label_scientific(digits = 1)
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (v <= 0) return("0")
    vt <- v
    if (vt >= 1) {
      paste0(formatC(vt, format = "f", digits = 0, big.mark = ","), " t")
    } else if (vt < 1e-6) {
      paste0(sci(vt), " t")
    } else {
      dp <- max(1, ceiling(-log10(vt)))
      dp <- min(dp, 6)
      paste0(formatC(vt, format = "f", digits = dp), " t")
    }
  }, character(1))
}

# ------------------------------------------------------------------------------
# Create Biomass Plot (no title)
# ------------------------------------------------------------------------------
cat("Creating biomass plot...\n")

p_biomass <- ggplot(biomass, aes(x = Year, y = median_t, color = Species, fill = Species)) +
  geom_ribbon(aes(ymin = q05_t, ymax = q95_t), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = q25_t, ymax = q75_t), alpha = 0.3, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(data = biomass %>% filter(!is.na(ObsBiomass_t) & Year >= 2010 & Year <= 2020), 
             aes(x = Year, y = ObsBiomass_t), 
             inherit.aes = FALSE, color = 'black', size = 1) +
  facet_wrap(~Species, scales = 'free_y') +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(legend.position = 'none', 
        strip.text = element_text(face = 'bold'),
        plot.margin = margin(5, 5, 5, 5)) +
  labs(x = 'Year', y = 'Biomass [t]')

ggsave(file.path(output_dir, "biomass_timeseries.png"), 
       p_biomass, width = 12, height = 10, dpi = 300)
cat("  Saved: biomass_timeseries.png\n")

# ------------------------------------------------------------------------------
# Create Yield Plot (no title)
# ------------------------------------------------------------------------------
cat("Creating yield plot...\n")

p_yield <- ggplot() +
  geom_ribbon(data = yield_ribbon,
              aes(x = Year, ymin = q05_t, ymax = q95_t, fill = Species),
              alpha = 0.2) +
  geom_ribbon(data = yield_ribbon,
              aes(x = Year, ymin = q25_t, ymax = q75_t, fill = Species),
              alpha = 0.3) +
  geom_line(data = yield_line,
            aes(x = Year, y = median_t, color = Species),
            linewidth = 1.0, linetype = "solid", lineend = "round") +
  geom_point(data = yield_obs,
             aes(x = Year, y = ObsYield_t, colour = Species),
             size = 1) +
  geom_point(data = yield_obs,
             aes(x = Year, y = ObsYield_t),
             shape = 1, size = 1, colour = "black") +
  geom_vline(xintercept = 1961, linetype = "dashed") +
  geom_vline(xintercept = 2010, linetype = "dashed") +
  scale_y_log10(breaks = scales::log_breaks(n = 6), labels = label_dynamic_decimals) +
  coord_cartesian(xlim = c(start_year, end_year)) +
  facet_wrap(~Species, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none", 
        strip.text = element_text(face = "bold"),
        plot.margin = margin(5, 5, 5, 5)) +
  labs(x = "Year", y = "Yield [t/year]")

ggsave(file.path(output_dir, "yield_timeseries.png"), 
       p_yield, width = 12, height = 10, dpi = 300)
cat("  Saved: yield_timeseries.png\n")

# ------------------------------------------------------------------------------
# Create Combined Multipanel Figure with panel tags
# ------------------------------------------------------------------------------
cat("Creating combined multipanel figure...\n")

p_combined <- p_biomass + p_yield +
  plot_layout(ncol = 1) +
  plot_annotation(
    tag_levels = 'A',
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold")
    )
  )

ggsave(file.path(output_dir, "biomass_yield_combined.png"), 
       p_combined, width = 12, height = 18, dpi = 300)
cat("  Saved: biomass_yield_combined.png\n")

cat("\n=== Figures Complete ===\n")
cat(sprintf("Output directory: %s\n", output_dir))
cat("Files created:\n")
cat("  1. biomass_timeseries.png (individual)\n")
cat("  2. yield_timeseries.png (individual)\n")
cat("  3. biomass_yield_combined.png (multipanel A+B)\n")
