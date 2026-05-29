# Verification plots for species-level FishMIP outputs — CORRECTED MODEL DOMAIN AREA
# Reads from fishmip_outputs_species_test/ (corrected area = 1.474341e+12 m^2)
# Output filenames appended with _corrected_area for side-by-side comparison with old plots.

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

MODEL_DOMAIN_AREA <- 1.474341e+12  # m^2 (therMizer calibration domain)
INPUT_DIR <- "fishmip_outputs_species_test"  # corrected-area outputs

species_order <- c(
  "mesozooplankton", "other krill", "other macrozooplankton",
  "antarctic krill", "salps", "mesopelagic fishes", "bathypelagic fishes",
  "shelf and coastal fishes", "flying birds", "small divers", "squids",
  "toothfishes", "leopard seals", "medium divers", "large divers",
  "minke whales", "orca", "sperm whales", "baleen whales"
)

read_fishmip_csv <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE) %>%
    mutate(
      Year    = round(time / 365) + 1841,
      Species = factor(species, levels = species_order)
    )
  message("  Read: ", path, " | ", nrow(df), " rows | years ",
          min(df$Year), "-", max(df$Year))
  df
}

message("Loading FishMIP species CSVs (corrected area)...")
bsp_histsoc <- read_fishmip_csv(
  file.path(INPUT_DIR, "histsoc/mizer_gfdl-mom6-cobalt2_obsclim_histsoc_default_bsp_prydz-bay_annual_1841_2010.csv")
)
csp_histsoc <- read_fishmip_csv(
  file.path(INPUT_DIR, "histsoc/mizer_gfdl-mom6-cobalt2_obsclim_histsoc_default_csp_prydz-bay_annual_1841_2010.csv")
)
bsp_nat <- read_fishmip_csv(
  file.path(INPUT_DIR, "nat/mizer_gfdl-mom6-cobalt2_obsclim_nat_default_bsp_prydz-bay_annual_1841_2010.csv")
)

# Sanity check: print median biomass of baleen whales in 1900 to confirm scale
bw_1900 <- bsp_histsoc %>% filter(species == "baleen whales", Year == 1900)
message(sprintf("  Baleen whales biomass (histsoc, 1900): %.4f g m-2  [old was ~13x smaller]", bw_1900$median))

# --- Observed biomass (grams -> g m^-2) ---
obs_biomass_data <- data.frame(
  Species = factor(
    c("mesozooplankton","other krill","other macrozooplankton","antarctic krill",
      "salps","mesopelagic fishes","bathypelagic fishes","shelf and coastal fishes",
      "flying birds","small divers","squids","toothfishes","leopard seals",
      "medium divers","large divers","minke whales","orca","sperm whales","baleen whales"),
    levels = species_order),
  ObsBiomass = c(1.297420e+13, 2.801248e+12, 1.474341e+13, 5.897364e+12,
                 9.612703e+11, 1.769209e+12, 1.769209e+12, 4.027900e+12,
                 4.423023e+09, 2.358946e+10, 2.211512e+11, 1.105756e+12,
                 2.948682e+09, 3.907004e+11, 1.621775e+10, 2.064077e+10,
                 8.846046e+09, 1.621775e+10, 1.872413e+11) / MODEL_DOMAIN_AREA
)
obs_biomass_ts <- obs_biomass_data %>%
  mutate(Lower = ObsBiomass * 0.75, Upper = ObsBiomass * 1.25) %>%
  tidyr::crossing(Year = 2000:2010)

# ============================================================
# PLOT 1: Biomass (histsoc vs nat overlay) — corrected area
# ============================================================
message("Building biomass verification plot (g m^-2, corrected area)...")

p_bio <- ggplot() +
  geom_ribbon(data = bsp_nat,
              aes(x = Year, ymin = q25, ymax = q75, group = Species),
              fill = "grey82", alpha = 0.6, color = NA) +
  geom_line(data = bsp_nat,
            aes(x = Year, y = q25, group = Species),
            color = "grey62", linewidth = 0.3, linetype = "dashed") +
  geom_line(data = bsp_nat,
            aes(x = Year, y = q75, group = Species),
            color = "grey62", linewidth = 0.3, linetype = "dashed") +
  geom_line(data = bsp_nat,
            aes(x = Year, y = median, group = Species),
            color = "grey38", linewidth = 0.4) +
  geom_ribbon(data = bsp_histsoc,
              aes(x = Year, ymin = q25, ymax = q75, fill = Species),
              alpha = 0.3) +
  geom_line(data = bsp_histsoc,
            aes(x = Year, y = median, color = Species), linewidth = 1.1) +
  geom_ribbon(data = obs_biomass_ts,
              aes(x = Year, ymin = Lower, ymax = Upper),
              alpha = 0.25, fill = "grey40", color = "black", linewidth = 0.5) +
  geom_point(data = obs_biomass_ts,
             aes(x = Year, y = ObsBiomass, color = Species), size = 1.2) +
  geom_point(data = obs_biomass_ts,
             aes(x = Year, y = ObsBiomass),
             shape = 1, size = 1.2, colour = "black") +
  facet_wrap(~Species, scales = "free_y", ncol = 4) +
  scale_y_continuous(labels = scales::label_scientific(digits = 2)) +
  theme_bw(base_size = 14.4) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Year",
    y = expression(Biomass~(g~m^{-2})),
    title = "Verification: bsp — CORRECTED area (1.474\u00d710\u00b9\u00b2 m\u00b2)",
    subtitle = "Grey: climate-only nat | Colour: fished histsoc | Black points: observed (\u00b125%)"
  )

ggsave("verify_bsp_biomass_corrected_area.png", p_bio, width = 16, height = 9, dpi = 300)
message("Saved: verify_bsp_biomass_corrected_area.png")

# ============================================================
# PLOT 2: Catch (histsoc csp) — corrected area
# ============================================================
message("Building catch verification plot (g m^-2 yr^-1, corrected area)...")

species_with_catch <- csp_histsoc %>%
  group_by(Species) %>%
  summarise(max_med = max(median, na.rm = TRUE), .groups = "drop") %>%
  filter(max_med > 0) %>%
  pull(Species)
message("  Species with non-zero catch: ",
        paste(as.character(species_with_catch), collapse = ", "))

csp_plot <- csp_histsoc %>%
  filter(Species %in% species_with_catch) %>%
  mutate(
    median = ifelse(median > 0, median, NA_real_),
    q25    = ifelse(q25    > 0, q25,    NA_real_),
    q75    = ifelse(q75    > 0, q75,    NA_real_)
  )

# Observed yield: try ../yield_observed_timeseries_tidy.RDS (project root)
obs_yield_layer <- list()
obs_yield_file <- "../yield_observed_timeseries_tidy.RDS"
if (file.exists(obs_yield_file)) {
  obs_yield <- readRDS(obs_yield_file) %>%
    filter(Species %in% as.character(species_with_catch), Yield > 0) %>%
    mutate(
      Species       = factor(Species, levels = species_order),
      Yield_density = Yield / MODEL_DOMAIN_AREA  # g yr^-1 -> g m^-2 yr^-1
    )
  message(sprintf("  Loaded observed yield: %d non-zero rows", nrow(obs_yield)))
  obs_yield_layer <- list(
    geom_point(data = obs_yield,
               aes(x = Year, y = Yield_density, color = Species),
               size = 1.5, alpha = 0.8),
    geom_point(data = obs_yield,
               aes(x = Year, y = Yield_density),
               shape = 1, size = 1.5, colour = "black")
  )
} else {
  message("  yield_observed_timeseries_tidy.RDS not found — catch plot will have no observed overlay")
}

p_catch <- ggplot() +
  geom_ribbon(data = csp_plot,
              aes(x = Year, ymin = q25, ymax = q75, fill = Species),
              alpha = 0.3, na.rm = TRUE) +
  geom_line(data = csp_plot,
            aes(x = Year, y = median, color = Species),
            linewidth = 1.1, na.rm = TRUE) +
  obs_yield_layer +
  facet_wrap(~Species, scales = "free_y", ncol = 3) +
  scale_y_continuous(
    trans  = scales::pseudo_log_trans(sigma = 1e-6, base = 10),
    labels = scales::label_scientific(digits = 1)
  ) +
  theme_bw(base_size = 14.4) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Year",
    y = expression(Catch~(g~m^{-2}~yr^{-1})),
    title = "Verification: csp — CORRECTED area (1.474\u00d710\u00b9\u00b2 m\u00b2)",
    subtitle = "Colour: modelled catch | Black open circles: observed catch"
  )

ggsave("verify_csp_catch_corrected_area.png", p_catch, width = 14, height = 7, dpi = 300)
message("Saved: verify_csp_catch_corrected_area.png")

message("=== Done ===")
message("verify_bsp_biomass_corrected_area.png : biomass g m^-2 (corrected area)")
message("verify_csp_catch_corrected_area.png   : catch g m^-2 yr^-1 (corrected area)")
message(sprintf("MODEL_DOMAIN_AREA used: %.6e m^2", MODEL_DOMAIN_AREA))
