# Standalone script to generate Monte Carlo biomass and yield plots from saved results
suppressPackageStartupMessages({
  library(mizer)
  library(tidyverse)
  library(reshape2)
  library(scales)
})

message("=== Generating Monte Carlo plots from saved results ===")

# Choose saved results (prefer 1000-sim; fallback to 10-sim)
mc_candidates <- c(
  "monte_carlo_results_SD2_tol0.005_tmax1000_nsims1000.rds",
  "monte_carlo_results_SD2_tol0.005_tmax1000_nsims10.rds"
)
mc_path <- mc_candidates[which(file.exists(mc_candidates))[1]]
if (is.na(mc_path)) {
  stop("No saved Monte Carlo results file found in working directory.")
}
message("Loading Monte Carlo results from: ", mc_path)
mc <- readRDS(mc_path)

# Extract and validate simulations
sims_all <- mc$simulations
if (length(sims_all) == 0) stop("Saved results do not contain simulations.")
valid_sims <- Filter(function(x) {
  inherits(x, "MizerSim") && !any(is.nan(x@n)) && !any(is.infinite(x@n))
}, sims_all)
message("Valid simulations: ", length(valid_sims), " of ", length(sims_all))
if (length(valid_sims) == 0) stop("No valid MizerSim objects available.")

# Species order to match baseline
species_order <- c(
  "mesozooplankton", "other krill", "other macrozooplankton", 
  "antarctic krill", "salps", "mesopelagic fishes", "bathypelagic fishes", 
  "shelf and coastal fishes", "flying birds", "small divers", "squids", 
  "toothfishes", "leopard seals", "medium divers", "large divers", 
  "minke whales", "orca", "sperm whales", "baleen whales"
)

# Observed biomass (2010–2020 means) for ribbons and points
obs_biomass_data <- data.frame(
  Species = factor(c("mesozooplankton","other krill","other macrozooplankton","antarctic krill",
                     "salps","mesopelagic fishes","bathypelagic fishes","shelf and coastal fishes",
                     "flying birds","small divers","squids","toothfishes","leopard seals",
                     "medium divers","large divers","minke whales","orca","sperm whales","baleen whales"),
                   levels = species_order),
  ObsBiomass = c(1.297420e+13, 2.801248e+12, 1.474341e+13, 5.897364e+12, 9.612703e+11, 
                 1.769209e+12, 1.769209e+12, 4.027900e+12, 4.423023e+09, 2.358946e+10, 
                 2.211512e+11, 1.105756e+12, 2.948682e+09, 3.907004e+11, 1.621775e+10, 
                 2.064077e+10, 8.846046e+09, 1.621775e+10, 1.872413e+11)
)

# Observed yield time series
yield_ts_tidy <- readRDS("yield_observed_timeseries_tidy.RDS")

# Helpers to extract data from sims
extract_mc_biomass <- function(sim_list) {
  do.call(rbind, lapply(seq_along(sim_list), function(i) {
    sim <- sim_list[[i]]
    mat <- getBiomass(sim)
    df <- reshape2::melt(mat)
    names(df) <- c("Year", "Species", "Biomass")
    df$Year <- as.numeric(as.character(df$Year))
    df$Species <- as.character(df$Species)
    df$sim_id <- i
    df
  }))
}

extract_mc_yield <- function(sim_list) {
  do.call(rbind, lapply(seq_along(sim_list), function(i) {
    sim <- sim_list[[i]]
    mat <- getYield(sim)
    df <- reshape2::melt(mat)
    names(df) <- c("Year", "Species", "Yield")
    df$Year <- as.numeric(as.character(df$Year))
    df$Species <- as.character(df$Species)
    df$sim_id <- i
    df
  }))
}

# Build biomass uncertainty
mc_biomass <- extract_mc_biomass(valid_sims)
biomass_unc <- mc_biomass %>%
  group_by(Year, Species) %>%
  summarise(
    median = median(Biomass, na.rm = TRUE),
    q25 = quantile(Biomass, 0.25, na.rm = TRUE),
    q75 = quantile(Biomass, 0.75, na.rm = TRUE),
    q05 = quantile(Biomass, 0.05, na.rm = TRUE),
    q95 = quantile(Biomass, 0.95, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(Species = factor(Species, levels = species_order))

# Observation time series for ribbons/points over 2000–2010 (as in MC chunk)
obs_biomass_ts <- obs_biomass_data %>%
  mutate(Lower = ObsBiomass * 0.75, Upper = ObsBiomass * 1.25) %>%
  tidyr::crossing(Year = 2000:2010)

# Plot biomass
p_bio <- ggplot() +
  geom_ribbon(data = biomass_unc, aes(x = Year, ymin = q05, ymax = q95, fill = Species), alpha = 0.2) +
  geom_ribbon(data = biomass_unc, aes(x = Year, ymin = q25, ymax = q75, fill = Species), alpha = 0.3) +
  geom_line(data = biomass_unc, aes(x = Year, y = median, color = Species), linewidth = 1.1) +
  geom_ribbon(data = obs_biomass_ts, aes(x = Year, ymin = Lower, ymax = Upper), alpha = 0.2, fill = "gray50") +
  geom_point(data = obs_biomass_ts, aes(x = Year, y = ObsBiomass, color = Species), size = 0.8) +
  geom_point(data = obs_biomass_ts, aes(x = Year, y = ObsBiomass), shape = 1, size = 0.8, colour = "black") +
  facet_wrap(~Species, scales = "free_y") +
  scale_y_continuous(labels = function(x) scales::number(x/1e6)) +
  theme_bw() +
  theme(legend.position = "none", strip.text = element_text(face = "bold")) +
  labs(x = "Year", y = "Biomass [t]", title = paste0("Monte Carlo: Biomass vs Observations (", length(valid_sims), " sims)"))

ggsave("montecarlo_biomass.png", p_bio, width = 12, height = 8, dpi = 300)
message("Saved montecarlo_biomass.png")

# Build yield uncertainty
mc_yield <- extract_mc_yield(valid_sims)
# Guard against negative yields and determine species with any positive yields
mc_yield <- mc_yield %>% mutate(Yield = pmax(Yield, 0))
species_with_yield <- mc_yield %>% group_by(Species) %>% summarise(max_yield = max(Yield, na.rm = TRUE), .groups = 'drop') %>% filter(max_yield > 0) %>% pull(Species)
message("Species with fishing yields: ", paste(species_with_yield, collapse = ", "))
message("Species with fishing yields: ", paste(species_with_yield, collapse = ", "))

yield_unc <- mc_yield %>%
  filter(Species %in% species_with_yield) %>%
  group_by(Year, Species) %>%
  summarise(
    median = median(Yield, na.rm = TRUE),
    q25 = quantile(Yield, 0.25, na.rm = TRUE),
    q75 = quantile(Yield, 0.75, na.rm = TRUE),
    q05 = quantile(Yield, 0.05, na.rm = TRUE),
    q95 = quantile(Yield, 0.95, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(Species = factor(Species, levels = species_order))

# Observed positive-yield indicator by year/species
obs_pos_by_year <- yield_ts_tidy %>%
  mutate(has_obs_pos = Yield > 0) %>%
  group_by(Year, Species) %>%
  summarise(has_obs_pos = any(has_obs_pos, na.rm = TRUE), .groups = 'drop')

# Filter modeled zeros only when there is no observed positive yield (proxy for zero effort)
yield_unc <- yield_unc %>%
  left_join(obs_pos_by_year, by = c("Year", "Species")) %>%
  mutate(has_obs_pos = coalesce(has_obs_pos, FALSE))

yield_unc_ribbon <- yield_unc %>%
  filter(!(has_obs_pos == FALSE & (is.na(q95) | q95 <= 0))) %>%
  mutate(
    q05 = ifelse(q05 > 0, q05, NA_real_),
    q25 = ifelse(q25 > 0, q25, NA_real_),
    q75 = ifelse(q75 > 0, q75, NA_real_),
    q95 = ifelse(q95 > 0, q95, NA_real_)
  ) %>%
  filter(!(is.na(q25) & is.na(q75) & is.na(q05) & is.na(q95)))

yield_unc_line <- yield_unc %>%
  filter(!(has_obs_pos == FALSE & (is.na(median) | median <= 0))) %>%
  mutate(median = ifelse(median > 0, median, NA_real_))

yield_obs_filtered <- yield_ts_tidy %>% filter(Species %in% species_with_yield, Yield > 0) %>%
  mutate(Species = factor(Species, levels = species_order))

p_yield <- ggplot() +
  geom_ribbon(data = yield_unc_ribbon, aes(x = Year, ymin = q05, ymax = q95, fill = Species), alpha = 0.2) +
  geom_ribbon(data = yield_unc_ribbon, aes(x = Year, ymin = q25, ymax = q75, fill = Species), alpha = 0.3) +
  geom_line(data = yield_unc_line, aes(x = Year, y = median, color = Species), linewidth = 1.0, linetype = "solid", lineend = "round") +
  geom_point(data = yield_obs_filtered, aes(x = Year, y = Yield, colour = Species), size = 1) +
  geom_point(data = yield_obs_filtered, aes(x = Year, y = Yield), shape = 1, size = 1, colour = "black") +
  geom_vline(xintercept = 1961, linetype = "dashed") +
  geom_vline(xintercept = 2010, linetype = "dashed") +
  scale_y_log10(
    breaks = scales::log_breaks(n = 6),
    labels = (function() {
      sci <- scales::label_scientific(digits = 1)
      function(x) vapply(x, function(v) {
        if (is.na(v)) return(NA_character_)
        if (v <= 0) return("0")
        vt <- v / 1e6  # convert grams -> tonnes
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
    })()
  ) +
  coord_cartesian(xlim = {
    baleen_start_obs <- yield_obs_filtered %>% filter(Species == "baleen whales") %>% summarise(min_year = min(Year, na.rm = TRUE)) %>% pull(min_year)
    global_start_obs <- yield_obs_filtered %>% summarise(min_year = min(Year, na.rm = TRUE)) %>% pull(min_year)
    start_year <- if (!is.null(baleen_start_obs) && length(baleen_start_obs) == 1 && is.finite(baleen_start_obs)) baleen_start_obs else if (!is.null(global_start_obs) && length(global_start_obs) == 1 && is.finite(global_start_obs)) global_start_obs else 1930
    c(start_year, max(yield_unc$Year, na.rm = TRUE))
  }) +
  facet_wrap(~Species, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none", strip.text = element_text(face = "bold")) +
  labs(x = "Year", y = "Yield [t/year]", title = paste0("Monte Carlo: Yield vs Observations (", length(valid_sims), " sims)"))

tryCatch({
  ggsave("montecarlo_yield.png", p_yield, width = 12, height = 8, dpi = 300)
  message("Saved montecarlo_yield.png")
}, error = function(e) {
  message("Failed to save montecarlo_yield.png: ", e$message)
  stop(e)
})

message("=== Done ===")
