###############################################################################
# Biomass timeseries plot — model ensemble only (no observed biomass overlay)
#
# Plots fished ensemble (coloured median + IQR ribbon) and climate-only
# ensemble (grey median + IQR ribbon/dashed bounds) without observed
# biomass points, shaded reference box, or ±25% bounds.
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

message("=== Generating biomass plot (no observed biomass) ===")

species_order <- c(
  "mesozooplankton", "other krill", "other macrozooplankton",
  "antarctic krill", "salps", "mesopelagic fishes", "bathypelagic fishes",
  "shelf and coastal fishes", "flying birds", "small divers", "squids",
  "toothfishes", "leopard seals", "medium divers", "large divers",
  "minke whales", "orca", "sperm whales", "baleen whales"
)

# ---------------------------------------------------------------------------
# Fished ensemble biomass (from pre-computed CSV)
# ---------------------------------------------------------------------------
biomass_precomp_csv <- "monte_carlo_2111_summaries/biomass_timeseries_summary_per_year_tonnes.csv"
if (!file.exists(biomass_precomp_csv)) stop("Fished biomass CSV not found: ", biomass_precomp_csv)

message("Loading fished biomass summary from CSV...")
biomass_unc <- read.csv(biomass_precomp_csv, stringsAsFactors = FALSE) %>%
  mutate(
    Species = factor(Species, levels = species_order),
    median = median_t * 1e6,
    q25    = q25_t    * 1e6,
    q75    = q75_t    * 1e6,
    q05    = q05_t    * 1e6,
    q95    = q95_t    * 1e6
  )
message("  Fished biomass: ", nrow(biomass_unc), " rows")

# ---------------------------------------------------------------------------
# Climate-only ensemble biomass (from pre-computed CSV)
# ---------------------------------------------------------------------------
clim_csv <- "climate_only_analysis/climate_only_biomass_timeseries_summary.csv"
if (file.exists(clim_csv)) {
  clim_biomass_unc <- read.csv(clim_csv, stringsAsFactors = FALSE) %>%
    mutate(
      Species = factor(Species, levels = species_order),
      q05    = q05_t    * 1e6,
      q25    = q25_t    * 1e6,
      median = median_t * 1e6,
      q75    = q75_t    * 1e6,
      q95    = q95_t    * 1e6
    )
  message("Loaded climate-only biomass summary: ", nrow(clim_biomass_unc), " rows")
} else {
  clim_biomass_unc <- NULL
  message("Climate-only biomass summary not found; skipping climate-only overlay")
}

# ---------------------------------------------------------------------------
# Plot
# ---------------------------------------------------------------------------
message("Generating plot...")

p_bio <- ggplot() +
  # Climate-only: IQR ribbon (light grey, drawn first)
  { if (!is.null(clim_biomass_unc))
      geom_ribbon(data = clim_biomass_unc,
                  aes(x = Year, ymin = q25, ymax = q75, group = Species),
                  fill = "grey82", alpha = 0.6, colour = NA) } +
  # Climate-only: IQR bounds as dashed grey lines
  { if (!is.null(clim_biomass_unc))
      geom_line(data = clim_biomass_unc,
                aes(x = Year, y = q25, group = Species),
                colour = "grey62", linewidth = 0.3, linetype = "dashed") } +
  { if (!is.null(clim_biomass_unc))
      geom_line(data = clim_biomass_unc,
                aes(x = Year, y = q75, group = Species),
                colour = "grey62", linewidth = 0.3, linetype = "dashed") } +
  # Climate-only: median (thin solid grey)
  { if (!is.null(clim_biomass_unc))
      geom_line(data = clim_biomass_unc,
                aes(x = Year, y = median, group = Species),
                colour = "grey38", linewidth = 0.4) } +
  # Fished ensemble: IQR ribbon (coloured)
  geom_ribbon(data = biomass_unc,
              aes(x = Year, ymin = q25, ymax = q75, fill = Species),
              alpha = 0.3) +
  # Fished ensemble: median line (coloured, thicker)
  geom_line(data = biomass_unc,
            aes(x = Year, y = median, colour = Species),
            linewidth = 1.1) +
  facet_wrap(~Species, scales = "free_y", ncol = 4) +
  scale_y_continuous(labels = function(x) scales::number(x / 1e12)) +
  theme_bw(base_size = 14.4) +
  theme(
    legend.position  = "none",
    strip.text       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "Year", y = expression(Biomass~(10^6~t)))

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------
out_png <- "montecarlo_biomass_no_obs.png"
out_pdf <- "montecarlo_biomass_no_obs.pdf"

ggsave(out_png, p_bio, width = 16, height = 9, dpi = 300)
message("Saved: ", out_png)
ggsave(out_pdf, p_bio, width = 16, height = 9)
message("Saved: ", out_pdf)

message("=== Done ===")
