# Standalone script to generate Monte Carlo biomass and yield plots from saved results
#
# FAST_BIOMASS_ONLY = TRUE  : use pre-computed CSV summaries (seconds; biomass plot only)
# FAST_BIOMASS_ONLY = FALSE : load full ensemble RDS (slow; biomass + yield plots)
FAST_BIOMASS_ONLY <- TRUE

suppressPackageStartupMessages({
  if (!FAST_BIOMASS_ONLY) library(mizer)
  library(tidyverse)
  library(reshape2)
  library(scales)
})

message("=== Generating Monte Carlo plots from saved results ===")
if (FAST_BIOMASS_ONLY) message("Mode: FAST_BIOMASS_ONLY (using pre-computed CSV summaries)")

# Choose saved results (prefer 1000-sim; fallback to 10-sim)
mc_candidates <- c(
  "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds",
  "monte_carlo_results_nsims_1250_SD_3_5_5_tol_0.0025_tmax_1500.rds",
  "monte_carlo_results_SD2_tol0.005_tmax1000_nsims1000.rds",
  "monte_carlo_results_SD2_tol0.005_tmax1000_nsims10.rds"
)
mc_path <- mc_candidates[which(file.exists(mc_candidates))[1]]
if (!FAST_BIOMASS_ONLY && is.na(mc_path)) {
  stop("No saved Monte Carlo results file found in working directory.")
}
if (!FAST_BIOMASS_ONLY) {
  message("Loading Monte Carlo results from: ", mc_path)
  mc <- readRDS(mc_path)
  sims_all <- mc$simulations
  if (length(sims_all) == 0) stop("Saved results do not contain simulations.")
  valid_sims <- Filter(function(x) {
    inherits(x, "MizerSim") && !any(is.nan(x@n)) && !any(is.infinite(x@n))
  }, sims_all)
  message("Valid simulations: ", length(valid_sims), " of ", length(sims_all))
  if (length(valid_sims) == 0) stop("No valid MizerSim objects available.")
}

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

# Build biomass uncertainty — fast path from pre-computed CSV, or slow path from ensemble
biomass_precomp_csv <- "monte_carlo_2111_summaries/biomass_timeseries_summary_per_year_tonnes.csv"
if (FAST_BIOMASS_ONLY && file.exists(biomass_precomp_csv)) {
  message("Loading fished biomass summary from CSV...")
  biomass_unc <- read.csv(biomass_precomp_csv, stringsAsFactors = FALSE) %>%
    mutate(
      Species = factor(Species, levels = species_order),
      # CSV is in tonnes; multiply by 1e6 for grams (matches plot scale_y /1e6)
      median = median_t * 1e6,
      q25    = q25_t    * 1e6,
      q75    = q75_t    * 1e6,
      q05    = q05_t    * 1e6,
      q95    = q95_t    * 1e6
    )
  message("  Fished biomass: ", nrow(biomass_unc), " rows")
} else {
  message("Extracting biomass from ensemble (slow path)...")
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
}

# Observation time series for ribbons/points over 2000–2010 (as in MC chunk)
obs_biomass_ts <- obs_biomass_data %>%
  mutate(Lower = ObsBiomass * 0.75, Upper = ObsBiomass * 1.25) %>%
  tidyr::crossing(Year = 2000:2010)

# Climate-only biomass summary (load from pre-computed CSV; units are tonnes -> convert to grams)
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

# Plot biomass
p_bio <- ggplot() +
  # Climate-only: IQR ribbon (light grey, drawn first so coloured layers sit on top)
  { if (!is.null(clim_biomass_unc))
      geom_ribbon(data = clim_biomass_unc,
                  aes(x = Year, ymin = q25, ymax = q75, group = Species),
                  fill = "grey82", alpha = 0.6, color = NA) } +
  # Climate-only: IQR bounds as dashed grey lines
  { if (!is.null(clim_biomass_unc))
      geom_line(data = clim_biomass_unc,
                aes(x = Year, y = q25, group = Species),
                color = "grey62", linewidth = 0.3, linetype = "dashed") } +
  { if (!is.null(clim_biomass_unc))
      geom_line(data = clim_biomass_unc,
                aes(x = Year, y = q75, group = Species),
                color = "grey62", linewidth = 0.3, linetype = "dashed") } +
  # Climate-only: median (thin solid grey)
  { if (!is.null(clim_biomass_unc))
      geom_line(data = clim_biomass_unc,
                aes(x = Year, y = median, group = Species),
                color = "grey38", linewidth = 0.4) } +
  # Fished ensemble: IQR ribbon (coloured)
  geom_ribbon(data = biomass_unc, aes(x = Year, ymin = q25, ymax = q75, fill = Species), alpha = 0.3) +
  # Fished ensemble: median line (coloured, thicker)
  geom_line(data = biomass_unc, aes(x = Year, y = median, color = Species), linewidth = 1.1) +
  # Observed biomass: slightly darker grey ribbon (±25% around point estimate, 2000–2010)
  geom_ribbon(data = obs_biomass_ts, aes(x = Year, ymin = Lower, ymax = Upper),
              alpha = 0.25, fill = "grey40", color = "black", linewidth = 0.5) +
  geom_point(data = obs_biomass_ts, aes(x = Year, y = ObsBiomass, color = Species), size = 1.2) +
  geom_point(data = obs_biomass_ts, aes(x = Year, y = ObsBiomass), shape = 1, size = 1.2, colour = "black") +
  facet_wrap(~Species, scales = "free_y", ncol = 4) +
  scale_y_continuous(labels = function(x) scales::number(x/1e12)) +
  theme_bw(base_size = 14.4) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Year", y = expression(Biomass~(10^6~t)))

ggsave("montecarlo_biomass.png", p_bio, width = 16, height = 9, dpi = 300)
message("Saved montecarlo_biomass.png")

if (FAST_BIOMASS_ONLY) {
  message("=== Done (biomass only) ===")
  quit(save = "no")
}

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

# Use the effort array to determine first/last year of effort per species
combined_effort_array <- readRDS("effort_array_1841_2010.rds")
effort_years_by_species <- do.call(rbind, lapply(colnames(combined_effort_array), function(sp) {
  yrs <- as.numeric(rownames(combined_effort_array))[combined_effort_array[, sp] > 0]
  if (length(yrs) > 0) data.frame(Species = sp, first_year = min(yrs), last_year = max(yrs), stringsAsFactors = FALSE)
  else NULL
})) %>%
  filter(Species %in% species_with_yield)

message("Effort periods per fished species:")
for (i in seq_len(nrow(effort_years_by_species))) {
  message(sprintf("  %-30s %d - %d", effort_years_by_species$Species[i],
                  effort_years_by_species$first_year[i], effort_years_by_species$last_year[i]))
}

# Create zero-catch rows for pre-effort (1900 to first_year - 1) per species
zero_catch_pre <- effort_years_by_species %>%
  rowwise() %>%
  do(data.frame(
    Species = .$Species,
    Year = 1900:(.$first_year - 1),
    Yield = 0,
    stringsAsFactors = FALSE
  )) %>%
  ungroup()

# Create zero-catch rows for post-effort (last_year + 1 to 2010) per species
zero_catch_post <- effort_years_by_species %>%
  filter(last_year < 2010) %>%
  rowwise() %>%
  do(data.frame(
    Species = .$Species,
    Year = (.$last_year + 1):2010,
    Yield = 0,
    stringsAsFactors = FALSE
  )) %>%
  ungroup()

# Combine zero-catch padding (kept separate for distinct styling)
zero_catch_all <- bind_rows(
  zero_catch_pre %>% mutate(Species = factor(Species, levels = species_order)),
  zero_catch_post %>% mutate(Species = factor(Species, levels = species_order))
)

# Observed zero-catch years within the effort window (for unfiltered plot only)
# yield_ts_tidy has zeros removed, so read from the raw CSV which retains all zeros
yield_obs_raw <- read.csv("yield_observed_timeseries.csv") %>%
  reshape2::melt(id.vars = "Year", variable.name = "Species", value.name = "Yield") %>%
  mutate(Species = gsub("\\.", " ", as.character(Species))) %>%
  filter(Species %in% species_with_yield) %>%
  mutate(Species = factor(Species, levels = species_order))

zero_catch_in_effort <- yield_obs_raw %>%
  left_join(effort_years_by_species, by = c("Species" = "Species")) %>%
  filter(!is.na(first_year), Year >= first_year, Year <= last_year, Yield == 0) %>%
  select(Year, Species, Yield) %>%
  mutate(Species = factor(Species, levels = species_order))

# Extend modelled median line to zero for pre- and post-effort periods per species
zero_line_pre <- effort_years_by_species %>%
  filter(first_year > 1900) %>%
  rowwise() %>%
  do(data.frame(
    Species = .$Species,
    Year    = 1900:(.$first_year - 1),
    median  = 0,
    stringsAsFactors = FALSE
  )) %>%
  ungroup() %>%
  mutate(Species = factor(Species, levels = species_order))

zero_line_post <- effort_years_by_species %>%
  filter(last_year < 2010) %>%
  rowwise() %>%
  do(data.frame(
    Species = .$Species,
    Year    = (.$last_year + 1):2010,
    median  = 0,
    stringsAsFactors = FALSE
  )) %>%
  ungroup() %>%
  mutate(Species = factor(Species, levels = species_order))

yield_unc_line_extended <- bind_rows(
  yield_unc_line %>% select(Year, Species, median),
  zero_line_pre,
  zero_line_post
) %>%
  arrange(Species, Year)

p_yield <- ggplot() +
  geom_ribbon(data = yield_unc_ribbon, aes(x = Year, ymin = q05, ymax = q95, fill = Species), alpha = 0.2) +
  geom_ribbon(data = yield_unc_ribbon, aes(x = Year, ymin = q25, ymax = q75, fill = Species), alpha = 0.3) +
  geom_line(data = yield_unc_line_extended, aes(x = Year, y = median, color = Species), linewidth = 1.0, linetype = "solid", lineend = "round") +
  geom_point(data = yield_obs_filtered, aes(x = Year, y = Yield, colour = Species), size = 1.2) +
  geom_point(data = yield_obs_filtered, aes(x = Year, y = Yield), shape = 1, size = 1.2, colour = "black") +
  geom_point(data = zero_catch_all, aes(x = Year, y = Yield, colour = Species), size = 0.48) +
  geom_point(data = zero_catch_all, aes(x = Year, y = Yield), shape = 1, size = 0.48, stroke = 0.36, colour = "black") +
  geom_vline(xintercept = 1961, linetype = "dashed") +
  geom_vline(xintercept = 2010, linetype = "dashed") +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(sigma = 1, base = 10),
    breaks = c(0, 10^seq(2, 14, 2)),
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
  coord_cartesian(xlim = c(1900, 2010)) +
  facet_wrap(~Species, scales = "free_y") +
  theme_bw(base_size = 14.4) +
  theme(legend.position = "none", strip.text = element_text(face = "bold")) +
  labs(x = "Year", y = expression(Yield~(t~y^{-1})))

tryCatch({
  ggsave("montecarlo_yield.png", p_yield, width = 12, height = 6.5, dpi = 300)
  message("Saved montecarlo_yield.png")
}, error = function(e) {
  message("Failed to save montecarlo_yield.png: ", e$message)
  stop(e)
})

# ==== UNFILTERED VARIANT: modelled zeros within effort period not suppressed ====
# Within-effort rows: keep all modelled medians as-is (including zeros), floored at 0.
# Outside the effort window the same zero-line extensions from the main plot are reused.

yield_unc_line_in_effort <- yield_unc %>%
  mutate(Species_chr = as.character(Species)) %>%
  left_join(effort_years_by_species, by = c("Species_chr" = "Species")) %>%
  filter(!is.na(first_year), Year >= first_year, Year <= last_year) %>%
  mutate(median = pmax(coalesce(median, 0), 0)) %>%
  select(Year, Species, median) %>%
  mutate(Species = factor(as.character(Species), levels = species_order))

yield_unc_ribbon_in_effort <- yield_unc %>%
  mutate(Species_chr = as.character(Species)) %>%
  left_join(effort_years_by_species, by = c("Species_chr" = "Species")) %>%
  filter(!is.na(first_year), Year >= first_year, Year <= last_year) %>%
  mutate(
    q05 = ifelse(q05 > 0, q05, NA_real_),
    q25 = ifelse(q25 > 0, q25, NA_real_),
    q75 = ifelse(q75 > 0, q75, NA_real_),
    q95 = ifelse(q95 > 0, q95, NA_real_)
  ) %>%
  mutate(Species = factor(as.character(Species), levels = species_order))

# Detect isolated positive rows (positive quantiles with all-NA neighbours on both sides).
# geom_ribbon cannot draw a polygon for a single-row group, so we handle these separately
# with narrow geom_rect bars that match the ribbon styling.
yield_unc_isolated <- yield_unc_ribbon_in_effort %>%
  group_by(Species) %>%
  arrange(Year) %>%
  mutate(
    has_data   = !is.na(q25) | !is.na(q75),
    prev_has   = lag(has_data, default = FALSE),
    next_has   = lead(has_data, default = FALSE)
  ) %>%
  filter(has_data & !prev_has & !next_has) %>%
  select(Year, Species, q05, q25, q75, q95) %>%
  ungroup()

yield_unc_line_unfiltered <- bind_rows(
  yield_unc_line_in_effort,
  zero_line_pre,
  zero_line_post
) %>%
  arrange(Species, Year)

y_labels_fn <- (function() {
  sci <- scales::label_scientific(digits = 1)
  function(x) vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (v <= 0) return("0")
    vt <- v / 1e6
    if (vt >= 1) {
      paste0(formatC(vt, format = "f", digits = 0, big.mark = ","), " t")
    } else if (vt < 1e-6) {
      paste0(sci(vt), " t")
    } else {
      dp <- min(max(1, ceiling(-log10(vt))), 6)
      paste0(formatC(vt, format = "f", digits = dp), " t")
    }
  }, character(1))
})()

p_yield_unfiltered <- ggplot() +
  geom_ribbon(data = yield_unc_ribbon_in_effort, aes(x = Year, ymin = q05, ymax = q95, fill = Species), alpha = 0.2) +
  geom_ribbon(data = yield_unc_ribbon_in_effort, aes(x = Year, ymin = q25, ymax = q75, fill = Species), alpha = 0.3) +
  geom_rect(data = yield_unc_isolated, aes(xmin = Year - 0.45, xmax = Year + 0.45, ymin = q05, ymax = q95, fill = Species), alpha = 0.2) +
  geom_rect(data = yield_unc_isolated, aes(xmin = Year - 0.45, xmax = Year + 0.45, ymin = q25, ymax = q75, fill = Species), alpha = 0.3) +
  geom_line(data = yield_unc_line_unfiltered, aes(x = Year, y = median, color = Species), linewidth = 1.0, linetype = "solid", lineend = "round") +
  geom_point(data = yield_obs_filtered, aes(x = Year, y = Yield, colour = Species), size = 1.2) +
  geom_point(data = yield_obs_filtered, aes(x = Year, y = Yield), shape = 1, size = 1.2, colour = "black") +
  geom_point(data = zero_catch_all, aes(x = Year, y = Yield, colour = Species), size = 0.48) +
  geom_point(data = zero_catch_all, aes(x = Year, y = Yield), shape = 1, size = 0.48, stroke = 0.36, colour = "black") +
  geom_point(data = zero_catch_in_effort, aes(x = Year, y = Yield, colour = Species), size = 0.48) +
  geom_point(data = zero_catch_in_effort, aes(x = Year, y = Yield), shape = 1, size = 0.48, stroke = 0.36, colour = "black") +
  geom_vline(xintercept = 1961, linetype = "dashed") +
  geom_vline(xintercept = 2010, linetype = "dashed") +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(sigma = 1, base = 10),
    breaks = c(0, 10^seq(2, 14, 2)),
    labels = y_labels_fn
  ) +
  coord_cartesian(xlim = c(1900, 2010)) +
  facet_wrap(~Species, scales = "free_y") +
  theme_bw(base_size = 14.4) +
  theme(legend.position = "none", strip.text = element_text(face = "bold")) +
  labs(x = "Year", y = expression(Yield~(t~y^{-1})))

tryCatch({
  ggsave("montecarlo_yield_unfiltered.png", p_yield_unfiltered, width = 12, height = 6.5, dpi = 300)
  message("Saved montecarlo_yield_unfiltered.png")
}, error = function(e) {
  message("Failed to save montecarlo_yield_unfiltered.png: ", e$message)
  stop(e)
})

# ==== OVERLAID VARIANT: all functional groups on one panel (no facets) ====
message("Building overlaid yield plot (all species, single panel)...")

p_yield_overlaid <- ggplot() +
  geom_ribbon(data = yield_unc_ribbon, aes(x = Year, ymin = q25, ymax = q75, fill = Species), alpha = 0.15) +
  geom_line(data = yield_unc_line_extended, aes(x = Year, y = median, color = Species), linewidth = 0.8, linetype = "solid", lineend = "round") +
  geom_point(data = yield_obs_filtered, aes(x = Year, y = Yield, colour = Species), size = 1.0) +
  geom_point(data = yield_obs_filtered, aes(x = Year, y = Yield), shape = 1, size = 1.0, colour = "black") +
  geom_point(data = zero_catch_all, aes(x = Year, y = Yield, colour = Species), size = 0.4) +
  geom_point(data = zero_catch_all, aes(x = Year, y = Yield), shape = 1, size = 0.4, stroke = 0.3, colour = "black") +
  geom_vline(xintercept = 1961, linetype = "dashed") +
  geom_vline(xintercept = 2010, linetype = "dashed") +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(sigma = 1, base = 10),
    breaks = c(0, 10^seq(2, 14, 2)),
    labels = y_labels_fn
  ) +
  coord_cartesian(xlim = c(1900, 2010)) +
  theme_bw(base_size = 14.4) +
  theme(legend.position = "right", strip.text = element_text(face = "bold")) +
  labs(x = "Year", y = expression(Yield~(t~y^{-1})),
       color = "Species", fill = "Species")

tryCatch({
  ggsave("montecarlo_yield_overlaid.png", p_yield_overlaid, width = 14, height = 7, dpi = 300)
  message("Saved montecarlo_yield_overlaid.png")
}, error = function(e) {
  message("Failed to save montecarlo_yield_overlaid.png: ", e$message)
  stop(e)
})

message("=== Done ===")
