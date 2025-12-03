# Regenerate climate-only biomass plot with only 25-75 percentile ribbon

library(mizer)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

cat("Regenerating climate-only biomass plot...\n")

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

p_biomass <- ggplot(biomass_summary_climate, aes(x = Year, y = median_t, color = Species, fill = Species)) +
  # Only 25-75 percentile ribbon
  geom_ribbon(aes(ymin = q25_t, ymax = q75_t), alpha = 0.3, color = NA) +
  # Median line
  geom_line(linewidth = 0.8) +
  # Observation points
  geom_point(data = biomass_summary_climate %>% 
               filter(!is.na(ObsBiomass_t) & Year >= 2001 & Year <= 2020),
             aes(x = Year, y = ObsBiomass_t), 
             inherit.aes = FALSE, color = "black", size = 1) +
  facet_wrap(~Species, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(legend.position = "none", 
        strip.text = element_text(face = "bold")) +
  labs(
    title = sprintf("Monte Carlo: Climate-Only Biomass vs Observations (%d sims)", n_sims),
    x = "Year", 
    y = "Biomass [t]"
  )

ggsave(file.path(output_dir, "climate_only_biomass_timeseries.png"), 
       p_biomass, width = 12, height = 10, dpi = 300)
cat("Saved: climate_only_biomass_timeseries.png\n")
