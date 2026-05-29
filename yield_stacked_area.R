###############################################################################
# yield_stacked_area.R
#
# Stacked area plots of yield timeseries (1900-2010)
#   - Modelled: ensemble median yield per species per year
#   - Observed: from yield_observed_timeseries.csv
#
# Both panels use the same species colours and kt y-axis scale.
# Outputs:
#   yield_stacked_modelled.png
#   yield_stacked_observed.png
###############################################################################

suppressPackageStartupMessages({
  library(mizer)
  library(tidyverse)
  library(reshape2)
  library(scales)
})

message("=== Yield stacked area plots ===")

PLOT_YEARS <- 1900:2010

# Species order (subset that actually fish will be retained automatically)
species_order <- c(
  "mesozooplankton", "other krill", "other macrozooplankton",
  "antarctic krill", "salps", "mesopelagic fishes", "bathypelagic fishes",
  "shelf and coastal fishes", "flying birds", "small divers", "squids",
  "toothfishes", "leopard seals", "medium divers", "large divers",
  "minke whales", "orca", "sperm whales", "baleen whales"
)

# ---------------------------------------------------------------------------
# Load MC ensemble and extract median yield per year per species
# ---------------------------------------------------------------------------
mc_path <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
if (!file.exists(mc_path)) stop("MC ensemble RDS not found: ", mc_path)

message("Loading MC ensemble (this will take a moment)...")
mc <- readRDS(mc_path)
sims_all <- mc$simulations
valid_sims <- Filter(function(x) {
  inherits(x, "MizerSim") && !any(is.nan(x@n)) && !any(is.infinite(x@n))
}, sims_all)
message("  Valid sims: ", length(valid_sims), " / ", length(sims_all))

message("Extracting yield from ensemble (slow step)...")
mc_yield_raw <- do.call(rbind, lapply(seq_along(valid_sims), function(i) {
  mat <- getYield(valid_sims[[i]])
  df  <- reshape2::melt(mat)
  names(df) <- c("Year", "Species", "Yield_g")
  df$Year    <- as.numeric(as.character(df$Year))
  df$Species <- as.character(df$Species)
  df
}))

# Species that have any positive yield across the ensemble
species_with_yield <- mc_yield_raw %>%
  group_by(Species) %>%
  summarise(max_yield = max(Yield_g, na.rm = TRUE), .groups = "drop") %>%
  filter(max_yield > 0) %>%
  pull(Species)
# Preserve species_order ordering
species_with_yield <- intersect(species_order, species_with_yield)
message("  Species with yield: ", paste(species_with_yield, collapse = ", "))

# Median yield in tonnes per year per species
yield_med <- mc_yield_raw %>%
  filter(Species %in% species_with_yield) %>%
  mutate(Yield_g = pmax(Yield_g, 0)) %>%
  group_by(Year, Species) %>%
  summarise(median_t = median(Yield_g, na.rm = TRUE) / 1e6, .groups = "drop")

# ---------------------------------------------------------------------------
# Effort array: zero-pad outside each species' effort window
# ---------------------------------------------------------------------------
effort_arr <- readRDS("effort_array_1841_2010.rds")
effort_windows <- do.call(rbind, lapply(colnames(effort_arr), function(sp) {
  yrs <- as.numeric(rownames(effort_arr))[effort_arr[, sp] > 0]
  if (length(yrs) > 0)
    data.frame(Species = sp, first_year = min(yrs), last_year = max(yrs),
               stringsAsFactors = FALSE)
})) %>%
  filter(Species %in% species_with_yield)

# Full year x species grid, values zero outside effort window
yield_modelled <- tidyr::expand_grid(
  Year    = PLOT_YEARS,
  Species = species_with_yield
) %>%
  left_join(yield_med, by = c("Year", "Species")) %>%
  left_join(effort_windows, by = "Species") %>%
  mutate(
    median_t = case_when(
      is.na(first_year)                    ~ 0,
      Year < first_year | Year > last_year ~ 0,
      TRUE                                 ~ coalesce(median_t, 0)
    ),
    Species = factor(Species, levels = species_order)
  )

# ---------------------------------------------------------------------------
# Observed yield
# ---------------------------------------------------------------------------
obs_raw  <- read.csv("yield_observed_timeseries.csv")

obs_long <- obs_raw %>%
  filter(Year %in% PLOT_YEARS) %>%
  reshape2::melt(id.vars = "Year", variable.name = "Species", value.name = "Yield_g") %>%
  mutate(
    Species = gsub("\\.", " ", as.character(Species)),
    Yield_g = pmax(coalesce(as.numeric(Yield_g), 0), 0),
    Yield_t = Yield_g / 1e6
  ) %>%
  filter(Species %in% species_with_yield) %>%
  # Fill any missing year x species rows with zero
  tidyr::complete(Year = PLOT_YEARS, Species, fill = list(Yield_g = 0, Yield_t = 0)) %>%
  mutate(Species = factor(Species, levels = species_order))

# ---------------------------------------------------------------------------
# Colour palette — consistent across both plots
# ---------------------------------------------------------------------------
sp_palette <- setNames(
  scales::hue_pal()(length(species_with_yield)),
  species_with_yield
)

# Y-axis label formatter: tonnes -> thousand tonnes (kt)
kt_formatter <- function(x) {
  ifelse(x == 0, "0", paste0(comma(x / 1e3), " kt"))
}

# ---------------------------------------------------------------------------
# Modelled stacked area
# ---------------------------------------------------------------------------
message("Building modelled stacked area plot...")

p_mod <- ggplot(yield_modelled, aes(x = Year, y = median_t, fill = Species)) +
  geom_area(position = "stack", alpha = 0.85, colour = NA) +
  geom_vline(xintercept = 1961, linetype = "dashed", linewidth = 0.5, colour = "grey30") +
  geom_vline(xintercept = 2010, linetype = "dashed", linewidth = 0.5, colour = "grey30") +
  scale_fill_manual(values = sp_palette) +
  scale_x_continuous(
    breaks = seq(1900, 2010, by = 10),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    labels = kt_formatter,
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position  = "right",
    axis.text.x      = element_text(angle = 40, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Modelled yield — ensemble median",
    x     = "Year",
    y     = expression(Yield~(10^3~t~y^{-1})),
    fill  = "Species"
  )

ggsave("yield_stacked_modelled.png", p_mod, width = 12, height = 6, dpi = 300)
message("Saved yield_stacked_modelled.png")

# ---------------------------------------------------------------------------
# Observed stacked area
# ---------------------------------------------------------------------------
message("Building observed stacked area plot...")

p_obs <- ggplot(obs_long, aes(x = Year, y = Yield_t, fill = Species)) +
  geom_area(position = "stack", alpha = 0.85, colour = NA) +
  geom_vline(xintercept = 1961, linetype = "dashed", linewidth = 0.5, colour = "grey30") +
  geom_vline(xintercept = 2010, linetype = "dashed", linewidth = 0.5, colour = "grey30") +
  scale_fill_manual(values = sp_palette) +
  scale_x_continuous(
    breaks = seq(1900, 2010, by = 10),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    labels = kt_formatter,
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position  = "right",
    axis.text.x      = element_text(angle = 40, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Observed yield",
    x     = "Year",
    y     = expression(Yield~(10^3~t~y^{-1})),
    fill  = "Species"
  )

ggsave("yield_stacked_observed.png", p_obs, width = 12, height = 6, dpi = 300)
message("Saved yield_stacked_observed.png")

message("=== Done ===")
