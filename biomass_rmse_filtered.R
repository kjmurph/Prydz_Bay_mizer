###############################################################################
# biomass_rmse_filtered.R
#
# Recreates the biomass timeseries facet plot from generate_mc_plots.R but
# restricted to RMSE-ranked subsets of the 2111-sim ensemble:
#   1. Best single sim (rank 1 by yield RMSE)
#   2. Mean of top-10 sims
#   3. Mean of top-10% sims (~211 sims)
#
# The grey climate-only overlay uses the SAME sim indices as the fished subset
# in each plot — paired by position so both ensembles are always consistent.
#
# Requires yield_rmse_per_sim.csv (from yield_rmse_evaluation.R).
#
# Outputs:
#   biomass_rmse_best_sim.png
#   biomass_rmse_top10_mean.png
#   biomass_rmse_top10pct_mean.png
###############################################################################

suppressPackageStartupMessages({
  library(mizer)
  library(tidyverse)
  library(reshape2)
  library(scales)
})

message("=== Biomass timeseries — RMSE-filtered subsets ===")

# ---------------------------------------------------------------------------
# Constants shared with generate_mc_plots.R
# ---------------------------------------------------------------------------
species_order <- c(
  "mesozooplankton", "other krill", "other macrozooplankton",
  "antarctic krill", "salps", "mesopelagic fishes", "bathypelagic fishes",
  "shelf and coastal fishes", "flying birds", "small divers", "squids",
  "toothfishes", "leopard seals", "medium divers", "large divers",
  "minke whales", "orca", "sperm whales", "baleen whales"
)

obs_biomass_data <- data.frame(
  Species = factor(
    c("mesozooplankton","other krill","other macrozooplankton","antarctic krill",
      "salps","mesopelagic fishes","bathypelagic fishes","shelf and coastal fishes",
      "flying birds","small divers","squids","toothfishes","leopard seals",
      "medium divers","large divers","minke whales","orca","sperm whales","baleen whales"),
    levels = species_order),
  ObsBiomass = c(1.297420e+13, 2.801248e+12, 1.474341e+13, 5.897364e+12, 9.612703e+11,
                 1.769209e+12, 1.769209e+12, 4.027900e+12, 4.423023e+09, 2.358946e+10,
                 2.211512e+11, 1.105756e+12, 2.948682e+09, 3.907004e+11, 1.621775e+10,
                 2.064077e+10, 8.846046e+09, 1.621775e+10, 1.872413e+11)
)

obs_biomass_ts <- obs_biomass_data %>%
  mutate(Lower = ObsBiomass * 0.75, Upper = ObsBiomass * 1.25) %>%
  tidyr::crossing(Year = 2000:2010)

# ---------------------------------------------------------------------------
# Load RMSE rankings
# ---------------------------------------------------------------------------
if (!file.exists("yield_rmse_per_sim.csv"))
  stop("yield_rmse_per_sim.csv not found — run yield_rmse_evaluation.R first.")

rmse_df <- read.csv("yield_rmse_per_sim.csv") %>% arrange(rank)
n_sims  <- nrow(rmse_df)
n_top10pct <- ceiling(n_sims * 0.10)

message(sprintf("RMSE rankings loaded: %d sims", n_sims))
message(sprintf("  Best:       sim_index=%d, RMSE=%.4f", rmse_df$sim_index[1], rmse_df$rmse[1]))
message(sprintf("  Top-10 max: sim_index=%d, RMSE=%.4f", rmse_df$sim_index[10], rmse_df$rmse[10]))
message(sprintf("  Top-10%% max (n=%d): RMSE=%.4f", n_top10pct, rmse_df$rmse[n_top10pct]))

# ---------------------------------------------------------------------------
# Load MC ensemble
# ---------------------------------------------------------------------------
mc_path <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
if (!file.exists(mc_path)) stop("MC ensemble RDS not found: ", mc_path)

message("Loading MC fishing ensemble...")
mc       <- readRDS(mc_path)
sims_all <- mc$simulations
valid_sims <- Filter(function(x)
  inherits(x, "MizerSim") && !any(is.nan(x@n)) && !any(is.infinite(x@n)),
  sims_all)
message("  Valid fishing sims: ", length(valid_sims))

# ---------------------------------------------------------------------------
# Load climate-only ensemble (same sim ordering — matched by index)
# ---------------------------------------------------------------------------
clim_path <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
if (!file.exists(clim_path)) stop("Climate-only ensemble RDS not found: ", clim_path)

message("Loading climate-only ensemble...")
clim_mc       <- readRDS(clim_path)
# Support both list-of-sims and $simulations slot
clim_sims_all <- if (is.list(clim_mc) && !is.null(clim_mc$simulations)) clim_mc$simulations else clim_mc
clim_valid_sims <- Filter(function(x)
  inherits(x, "MizerSim") && !any(is.nan(x@n)) && !any(is.infinite(x@n)),
  clim_sims_all)
message("  Valid climate-only sims: ", length(clim_valid_sims))

if (length(clim_valid_sims) != length(valid_sims)) {
  warning(sprintf(
    "Climate-only ensemble has %d valid sims but fishing ensemble has %d. ",
    length(clim_valid_sims), length(valid_sims),
    "Indices will be used as-is; ensure ensembles were generated in the same order."
  ))
}

# ---------------------------------------------------------------------------
# Helper: extract biomass from a subset of sims and summarise
# ---------------------------------------------------------------------------
extract_biomass_summary <- function(sim_subset) {
  raw <- do.call(rbind, lapply(seq_along(sim_subset), function(i) {
    mat <- getBiomass(sim_subset[[i]])
    df  <- reshape2::melt(mat)
    names(df) <- c("Year", "Species", "Biomass")
    df$Year    <- as.numeric(as.character(df$Year))
    df$Species <- as.character(df$Species)
    df
  }))

  raw %>%
    group_by(Year, Species) %>%
    summarise(
      median = median(Biomass, na.rm = TRUE),
      q25    = quantile(Biomass, 0.25, na.rm = TRUE),
      q75    = quantile(Biomass, 0.75, na.rm = TRUE),
      q05    = quantile(Biomass, 0.05, na.rm = TRUE),
      q95    = quantile(Biomass, 0.95, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Species = factor(Species, levels = species_order))
}

# ---------------------------------------------------------------------------
# Helper: build the standard faceted biomass plot
# clim_unc: pre-computed summary from extract_biomass_summary() for the
#           matched climate-only sims (or NULL to skip the grey overlay)
# ---------------------------------------------------------------------------
make_biomass_plot <- function(biomass_unc, clim_unc = NULL, subtitle = NULL,
                              show_obs = TRUE, facet = TRUE) {
  ggplot() +
    # Climate-only: IQR ribbon (light grey)
    { if (!is.null(clim_unc))
        geom_ribbon(data = clim_unc,
                    aes(x = Year, ymin = q25, ymax = q75, group = Species),
                    fill = "grey82", alpha = 0.6, color = NA) } +
    # Climate-only: IQR bounds as dashed grey lines
    { if (!is.null(clim_unc))
        geom_line(data = clim_unc,
                  aes(x = Year, y = q25, group = Species),
                  color = "grey62", linewidth = 0.3, linetype = "dashed") } +
    { if (!is.null(clim_unc))
        geom_line(data = clim_unc,
                  aes(x = Year, y = q75, group = Species),
                  color = "grey62", linewidth = 0.3, linetype = "dashed") } +
    # Climate-only: median (thin solid grey)
    { if (!is.null(clim_unc))
        geom_line(data = clim_unc,
                  aes(x = Year, y = median, group = Species),
                  color = "grey38", linewidth = 0.4) } +
    # Fished subset: IQR ribbon (coloured)
    geom_ribbon(data = biomass_unc,
                aes(x = Year, ymin = q25, ymax = q75, fill = Species),
                alpha = 0.3) +
    # Fished subset: median line (coloured, thicker)
    geom_line(data = biomass_unc,
              aes(x = Year, y = median, color = Species),
              linewidth = 1.1) +
    # Observed biomass: ±25% ribbon + points, 2000–2010
    { if (show_obs)
        geom_ribbon(data = obs_biomass_ts,
                    aes(x = Year, ymin = Lower, ymax = Upper),
                    alpha = 0.25, fill = "grey40", color = "black", linewidth = 0.5) } +
    { if (show_obs)
        geom_point(data = obs_biomass_ts,
                   aes(x = Year, y = ObsBiomass, color = Species), size = 1.2) } +
    { if (show_obs)
        geom_point(data = obs_biomass_ts,
                   aes(x = Year, y = ObsBiomass), shape = 1, size = 1.2, colour = "black") } +
    { if (facet) facet_wrap(~Species, scales = "free_y", ncol = 4) } +
    scale_y_continuous(labels = function(x) scales::number(x / 1e12)) +
    theme_bw(base_size = 14.4) +
    theme(legend.position = if (facet) "none" else "right",
          strip.text      = element_text(face = "bold"),
          axis.text.x     = element_text(angle = 45, hjust = 1)) +
    labs(
      subtitle = subtitle,
      x        = "Year",
      y        = expression(Biomass~(10^6~t))
    )
}

# ---------------------------------------------------------------------------
# 1. Best single sim
# ---------------------------------------------------------------------------
best_i <- rmse_df$sim_index[1]
message(sprintf("Extracting biomass for best sim (fishing index=%d)...", best_i))
best_unc      <- extract_biomass_summary(list(valid_sims[[best_i]]))
best_clim_unc <- extract_biomass_summary(list(clim_valid_sims[[best_i]]))

p_best <- make_biomass_plot(
  best_unc,
  clim_unc = best_clim_unc,
  subtitle = sprintf("Best sim by yield RMSE (sim index %d, RMSE = %.4f)",
                     best_i, rmse_df$rmse[1])
)
ggsave("biomass_rmse_best_sim.png", p_best, width = 16, height = 9, dpi = 300)
message("Saved biomass_rmse_best_sim.png")

# ---------------------------------------------------------------------------
# 2. Top-10 sims — mean
# ---------------------------------------------------------------------------
top10_idx <- rmse_df$sim_index[1:10]
message("Extracting biomass for top-10 sims (fishing + matched climate-only)...")
top10_unc      <- extract_biomass_summary(valid_sims[top10_idx])
top10_clim_unc <- extract_biomass_summary(clim_valid_sims[top10_idx])

p_top10 <- make_biomass_plot(
  top10_unc,
  clim_unc = top10_clim_unc,
  subtitle = sprintf("Top-10 sims by yield RMSE — mean (RMSE range %.4f – %.4f)",
                     rmse_df$rmse[1], rmse_df$rmse[10])
)
ggsave("biomass_rmse_top10_mean.png", p_top10, width = 16, height = 9, dpi = 300)
message("Saved biomass_rmse_top10_mean.png")

# ---------------------------------------------------------------------------
# 3. Top-10% sims — mean
# ---------------------------------------------------------------------------
top10pct_idx <- rmse_df$sim_index[1:n_top10pct]
message(sprintf("Extracting biomass for top-10%% sims (n=%d, fishing + matched climate-only)...", n_top10pct))
top10pct_unc      <- extract_biomass_summary(valid_sims[top10pct_idx])
top10pct_clim_unc <- extract_biomass_summary(clim_valid_sims[top10pct_idx])

p_top10pct <- make_biomass_plot(
  top10pct_unc,
  clim_unc = top10pct_clim_unc,
  subtitle = sprintf("Top-10%% sims by yield RMSE — mean (n=%d, RMSE range %.4f – %.4f)",
                     n_top10pct, rmse_df$rmse[1], rmse_df$rmse[n_top10pct])
)
ggsave("biomass_rmse_top10pct_mean.png", p_top10pct, width = 16, height = 9, dpi = 300)
message("Saved biomass_rmse_top10pct_mean.png")

# ---------------------------------------------------------------------------
# No-observed-data variants (same subsets, obs overlay removed)
# ---------------------------------------------------------------------------
message("Generating no-obs variants...")

p_best_noobs <- make_biomass_plot(
  best_unc,
  clim_unc = best_clim_unc,
  show_obs = FALSE,
  subtitle = sprintf("Best sim by yield RMSE (sim index %d, RMSE = %.4f)",
                     best_i, rmse_df$rmse[1])
)
ggsave("biomass_rmse_best_sim_noobs.png", p_best_noobs, width = 16, height = 9, dpi = 300)
message("Saved biomass_rmse_best_sim_noobs.png")

p_top10_noobs <- make_biomass_plot(
  top10_unc,
  clim_unc = top10_clim_unc,
  show_obs = FALSE,
  subtitle = sprintf("Top-10 sims by yield RMSE \u2014 mean (RMSE range %.4f \u2013 %.4f)",
                     rmse_df$rmse[1], rmse_df$rmse[10])
)
ggsave("biomass_rmse_top10_mean_noobs.png", p_top10_noobs, width = 16, height = 9, dpi = 300)
message("Saved biomass_rmse_top10_mean_noobs.png")

p_top10pct_noobs <- make_biomass_plot(
  top10pct_unc,
  clim_unc = top10pct_clim_unc,
  show_obs = FALSE,
  subtitle = sprintf("Top-10%% sims by yield RMSE \u2014 mean (n=%d, RMSE range %.4f \u2013 %.4f)",
                     n_top10pct, rmse_df$rmse[1], rmse_df$rmse[n_top10pct])
)
ggsave("biomass_rmse_top10pct_mean_noobs.png", p_top10pct_noobs, width = 16, height = 9, dpi = 300)
message("Saved biomass_rmse_top10pct_mean_noobs.png")

# ---------------------------------------------------------------------------
# Overlaid (no-facet) variant: top-10%, no obs
# ---------------------------------------------------------------------------
message("Generating overlaid (no-facet) top-10pct no-obs plot...")

p_top10pct_noobs_overlaid <- make_biomass_plot(
  top10pct_unc,
  clim_unc = top10pct_clim_unc,
  show_obs = FALSE,
  facet    = FALSE,
  subtitle = sprintf("Top-10%% sims by yield RMSE \u2014 mean (n=%d, RMSE range %.4f \u2013 %.4f)",
                     n_top10pct, rmse_df$rmse[1], rmse_df$rmse[n_top10pct])
)
ggsave("biomass_rmse_top10pct_mean_noobs_overlaid.png",
       p_top10pct_noobs_overlaid, width = 16, height = 9, dpi = 300)
message("Saved biomass_rmse_top10pct_mean_noobs_overlaid.png")

# ---------------------------------------------------------------------------
# Aggregated multi-panel biomass plot (top-10%, no obs, 1 column × 7 rows)
#
# Panel layout:
#   1. Zooplankton         — meso + other krill + other macrozooplankton + salps (summed)
#   2. Pelagic fishes      — meso + bathy pelagic (summed)
#   3. Seabirds            — flying birds + small divers (summed)
#   4. Pinnipeds           — medium + large divers (summed)
#   5. Krill & demersal fishes — antarctic krill, toothfishes, shelf & coastal (individual)
#   6. Large marine mammals — minke, orca, sperm whales (individual)
#   7. Large baleen whales — baleen whales (individual)
#
# Grey IQR ribbon + dashed median shown for matched climate-only sims.
# ---------------------------------------------------------------------------
message("\nBuilding aggregated multi-panel biomass plot (top-10%)...")

agg_groups_def <- list(
  "Zooplankton"    = c("mesozooplankton", "other krill", "other macrozooplankton", "salps"),
  "Pelagic fishes" = c("mesopelagic fishes", "bathypelagic fishes"),
  "Seabirds"       = c("flying birds", "small divers"),
  "Pinnipeds"      = c("medium divers", "large divers")
)

panel_levels_agg <- c(
  "Zooplankton",
  "Pelagic fishes",
  "Seabirds",
  "Pinnipeds",
  "Krill & demersal fishes",
  "Large marine mammals",
  "Large baleen whales"
)

panel_map_agg <- data.frame(
  display_name = c(
    "Zooplankton",
    "Pelagic fishes",
    "Seabirds",
    "Pinnipeds",
    "Antarctic krill", "Toothfishes", "Shelf & coastal fishes",
    "Minke whales", "Orca", "Sperm whales",
    "Baleen whales"
  ),
  raw_name = c(
    NA,
    NA,
    NA,
    NA,
    "antarctic krill", "toothfishes", "shelf and coastal fishes",
    "minke whales", "orca", "sperm whales",
    "baleen whales"
  ),
  panel = c(
    "Zooplankton",
    "Pelagic fishes",
    "Seabirds",
    "Pinnipeds",
    "Krill & demersal fishes", "Krill & demersal fishes", "Krill & demersal fishes",
    "Large marine mammals", "Large marine mammals", "Large marine mammals",
    "Large baleen whales"
  ),
  stringsAsFactors = FALSE
)

display_levels_agg <- panel_map_agg$display_name

agg_colors <- c(
  "Zooplankton"            = "#4dac26",
  "Pelagic fishes"         = "#0072B2",
  "Seabirds"               = "#f46d43",
  "Pinnipeds"              = "#d6604d",
  "Antarctic krill"        = "#e8534a",
  "Toothfishes"            = "#1b7837",
  "Shelf & coastal fishes" = "#74c476",
  "Minke whales"           = "#3dbbd4",
  "Orca"                   = "#222222",
  "Sperm whales"           = "#9467bd",
  "Baleen whales"          = "#e878b8"
)

extract_aggregated_summary <- function(sim_subset) {
  n_sims <- length(sim_subset)

  # Raw biomass per sim
  all_rows <- lapply(seq_len(n_sims), function(i) {
    mat <- getBiomass(sim_subset[[i]])
    df  <- reshape2::melt(mat)
    names(df) <- c("Year", "Species", "Biomass")
    df$Year    <- as.numeric(as.character(df$Year))
    df$Species <- as.character(df$Species)
    df
  })

  # Aggregated groups: sum within each sim, then summarise across sims
  agg_summary <- do.call(rbind, lapply(names(agg_groups_def), function(gname) {
    sp_list <- agg_groups_def[[gname]]
    sim_totals <- do.call(rbind, lapply(seq_len(n_sims), function(i) {
      sub     <- all_rows[[i]][all_rows[[i]]$Species %in% sp_list, ]
      yr_sums <- tapply(sub$Biomass, sub$Year, sum)
      data.frame(Year = as.numeric(names(yr_sums)),
                 Biomass = as.numeric(yr_sums))
    }))
    sim_totals %>%
      group_by(Year) %>%
      summarise(
        median = median(Biomass, na.rm = TRUE),
        q25    = quantile(Biomass, 0.25, na.rm = TRUE),
        q75    = quantile(Biomass, 0.75, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(display_name = gname)
  }))

  # Individual species: summarise directly across sims
  ind_map  <- panel_map_agg[!is.na(panel_map_agg$raw_name), ]
  ind_rows <- do.call(rbind, all_rows)
  ind_summary <- ind_rows %>%
    filter(Species %in% ind_map$raw_name) %>%
    group_by(Year, Species) %>%
    summarise(
      median = median(Biomass, na.rm = TRUE),
      q25    = quantile(Biomass, 0.25, na.rm = TRUE),
      q75    = quantile(Biomass, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(ind_map[, c("raw_name", "display_name")],
              by = c("Species" = "raw_name")) %>%
    select(-Species)

  bind_rows(agg_summary, ind_summary) %>%
    left_join(panel_map_agg[, c("display_name", "panel")], by = "display_name") %>%
    mutate(
      panel        = factor(panel,        levels = panel_levels_agg),
      display_name = factor(display_name, levels = display_levels_agg)
    )
}

message("  Extracting aggregated fishing biomass (top-10%)...")
agg_fish <- extract_aggregated_summary(valid_sims[top10pct_idx])
message("  Extracting aggregated climate-only biomass (top-10%)...")
agg_clim <- extract_aggregated_summary(clim_valid_sims[top10pct_idx])

p_agg <- ggplot() +
  # Climate-only: IQR ribbon (light grey, per group)
  geom_ribbon(data    = agg_clim,
              mapping = aes(x = Year, ymin = q25, ymax = q75, group = display_name),
              fill = "grey82", alpha = 0.6, colour = NA) +
  # Climate-only: IQR bounds as dashed grey lines
  geom_line(data    = agg_clim,
            mapping = aes(x = Year, y = q25, group = display_name),
            colour = "grey62", linewidth = 0.3, linetype = "dashed") +
  geom_line(data    = agg_clim,
            mapping = aes(x = Year, y = q75, group = display_name),
            colour = "grey62", linewidth = 0.3, linetype = "dashed") +
  # Climate-only: median (thin solid grey)
  geom_line(data    = agg_clim,
            mapping = aes(x = Year, y = median, group = display_name),
            colour = "grey38", linewidth = 0.4) +
  # Fished: IQR ribbon + solid median line (coloured)
  geom_ribbon(data    = agg_fish,
              mapping = aes(x = Year, ymin = q25, ymax = q75,
                            fill = display_name, group = display_name),
              alpha = 0.3) +
  geom_line(data    = agg_fish,
            mapping = aes(x = Year, y = median,
                          colour = display_name, group = display_name),
            linewidth = 0.9) +
  facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = agg_colors, name = NULL) +
  scale_fill_manual(values   = agg_colors, name = NULL) +
  scale_y_continuous(labels = function(x) scales::number(x / 1e12)) +
  scale_x_continuous(breaks = seq(1841, 2010, by = 20)) +
  theme_bw(base_size = 11) +
  theme(
    strip.text       = element_text(face = "bold", size = 10),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "right",
    legend.key.size  = unit(0.45, "cm"),
    legend.text      = element_text(size = 8.5),
    panel.grid.major = element_line(colour = "grey92"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Year",
    y = expression(Biomass~(10^6~t))
  )

ggsave("biomass_rmse_top10pct_aggregated.png",
       p_agg, width = 9, height = 21, dpi = 300)
message("Saved biomass_rmse_top10pct_aggregated.png")

message("=== Done ===")
