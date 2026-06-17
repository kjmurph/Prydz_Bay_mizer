###############################################################################
# abundance_meanweight_rmse_top10pct_grid.R
#
# Two companion 3-column × 4-row grids to biomass_rmse_top10pct_grid.R:
#
#   1. Total abundance (individuals) per functional group
#   2. Mean individual body mass (g) per functional group
#
# Uses the same 12-panel layout and colour palette as the biomass grid.
# For aggregated groups (Zooplankton, Pelagic fishes & squid, etc.), mean
# weight is the abundance-weighted group mean (= group biomass / group N),
# computed per simulation before taking ensemble quantiles.
#
# Caches per-sim abundance + biomass to RDS on first run; subsequent runs
# load from cache and are fast.
#
# Outputs:
#   abundance_rmse_top10pct_grid.png
#   meanweight_rmse_top10pct_grid.png
#   abund_mw_top10pct_raw_fish.rds  (cache)
#   abund_mw_top10pct_raw_clim.rds  (cache)
###############################################################################

suppressPackageStartupMessages({
  library(mizer)
  library(tidyverse)
  library(reshape2)
  library(scales)
})

message("=== Abundance & mean weight RMSE top-10% grid (3 × 4) ===")

# ---------------------------------------------------------------------------
# Panel definitions — identical to biomass_rmse_top10pct_grid.R
# ---------------------------------------------------------------------------
group_defs <- list(
  "Zooplankton"            = c("mesozooplankton", "other krill",
                               "other macrozooplankton", "salps"),
  "Pelagic fishes & squid" = c("mesopelagic fishes", "bathypelagic fishes", "squids"),
  "Seabirds"               = c("flying birds", "small divers"),
  "Pinnipeds"              = c("medium divers", "large divers")
)

ind_to_panel <- c(
  "antarctic krill"          = "Antarctic krill",
  "toothfishes"              = "Toothfishes",
  "shelf and coastal fishes" = "Shelf & coastal fishes",
  "leopard seals"            = "Leopard seals",
  "minke whales"             = "Minke whales",
  "orca"                     = "Orca",
  "sperm whales"             = "Sperm whales",
  "baleen whales"            = "Large baleen whales"
)

panel_levels <- c(
  "Large baleen whales",    "Sperm whales",            "Minke whales",
  "Orca",                   "Leopard seals",            "Pinnipeds",
  "Seabirds",               "Toothfishes",              "Shelf & coastal fishes",
  "Pelagic fishes & squid", "Antarctic krill",          "Zooplankton"
)

panel_colors <- c(
  "Large baleen whales"     = "#FF61C3",
  "Minke whales"            = "#00B9E3",
  "Sperm whales"            = "#DB72FB",
  "Orca"                    = "#619CFF",
  "Leopard seals"           = "#E07B39",
  "Pinnipeds"               = "#2B6CB0",
  "Seabirds"                = "#9E9E9E",
  "Toothfishes"             = "#00C19F",
  "Shelf & coastal fishes"  = "#93AA00",
  "Pelagic fishes & squid"  = "#D39200",
  "Antarctic krill"         = "#F8766D",
  "Zooplankton"             = "#6A1B9A"
)

# ---------------------------------------------------------------------------
# Helper: extract per-sim abundance and biomass from a list of MizerSims
#
# Returns long-format data.frame:
#   Year, Species, Biomass (g), Abundance (individuals), sim_i
#
# Abundance = integral of n(w) * dw over the size grid (individuals per m^3
# or per unit area, matching the model's units).
# Mean weight is NOT stored here — it is derived as Biomass / Abundance
# inside the panel-summary functions.
# ---------------------------------------------------------------------------
extract_raw_abund_bm <- function(sim_list) {
  do.call(rbind, lapply(seq_along(sim_list), function(i) {
    sim <- sim_list[[i]]
    if (is.null(sim) || !inherits(sim, "MizerSim")) return(NULL)

    params <- sim@params

    # Biomass [time × species] — uses the standard mizer function
    mat_bm  <- getBiomass(sim)
    df_bm   <- reshape2::melt(mat_bm)
    names(df_bm) <- c("Year", "Species", "Biomass")
    df_bm$Year    <- as.numeric(as.character(df_bm$Year))
    df_bm$Species <- as.character(df_bm$Species)

    # Abundance [time × species]: sum n(w) * dw over size bins
    # sim@n is [time × sp × w]; sweep multiplies each w-slice by dw
    mat_ab  <- apply(sweep(sim@n, 3, params@dw, "*"), MARGIN = c(1, 2), FUN = sum)
    df_ab   <- reshape2::melt(mat_ab)
    names(df_ab) <- c("Year", "Species", "Abundance")
    df_ab$Year    <- as.numeric(as.character(df_ab$Year))
    df_ab$Species <- as.character(df_ab$Species)

    dplyr::inner_join(df_bm, df_ab, by = c("Year", "Species")) %>%
      dplyr::mutate(sim_i = i)
  }))
}

# ---------------------------------------------------------------------------
# Helper: build abundance panel summary
# Aggregated groups: sum N within each sim first, then quantile across sims
# ---------------------------------------------------------------------------
build_panel_summary_abundance <- function(raw_df) {
  agg_out <- do.call(rbind, lapply(names(group_defs), function(gname) {
    raw_df %>%
      dplyr::filter(Species %in% group_defs[[gname]]) %>%
      dplyr::group_by(sim_i, Year) %>%
      dplyr::summarise(Abundance = sum(Abundance, na.rm = TRUE), .groups = "drop") %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        median = median(Abundance, na.rm = TRUE),
        q25    = quantile(Abundance, 0.25, na.rm = TRUE),
        q75    = quantile(Abundance, 0.75, na.rm = TRUE),
        q05    = quantile(Abundance, 0.05, na.rm = TRUE),
        q95    = quantile(Abundance, 0.95, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(panel = gname)
  }))

  ind_out <- raw_df %>%
    dplyr::filter(Species %in% names(ind_to_panel)) %>%
    dplyr::group_by(Year, Species) %>%
    dplyr::summarise(
      median = median(Abundance, na.rm = TRUE),
      q25    = quantile(Abundance, 0.25, na.rm = TRUE),
      q75    = quantile(Abundance, 0.75, na.rm = TRUE),
      q05    = quantile(Abundance, 0.05, na.rm = TRUE),
      q95    = quantile(Abundance, 0.95, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(panel = ind_to_panel[Species]) %>%
    dplyr::select(-Species)

  dplyr::bind_rows(agg_out, ind_out) %>%
    dplyr::mutate(panel = factor(panel, levels = panel_levels))
}

# ---------------------------------------------------------------------------
# Helper: build mean weight panel summary
# For aggregated groups: group mean weight = group biomass / group abundance,
# computed per sim before taking ensemble quantiles (avoids the bias of
# averaging ratios across sims).
# ---------------------------------------------------------------------------
build_panel_summary_meanweight <- function(raw_df) {
  agg_out <- do.call(rbind, lapply(names(group_defs), function(gname) {
    raw_df %>%
      dplyr::filter(Species %in% group_defs[[gname]]) %>%
      dplyr::group_by(sim_i, Year) %>%
      dplyr::summarise(
        group_bm = sum(Biomass,   na.rm = TRUE),
        group_ab = sum(Abundance, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(
        MeanWeight = ifelse(group_ab > 0, group_bm / group_ab, NA_real_)
      ) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        median = median(MeanWeight, na.rm = TRUE),
        q25    = quantile(MeanWeight, 0.25, na.rm = TRUE),
        q75    = quantile(MeanWeight, 0.75, na.rm = TRUE),
        q05    = quantile(MeanWeight, 0.05, na.rm = TRUE),
        q95    = quantile(MeanWeight, 0.95, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(panel = gname)
  }))

  ind_out <- raw_df %>%
    dplyr::filter(Species %in% names(ind_to_panel)) %>%
    dplyr::mutate(
      MeanWeight = ifelse(Abundance > 0, Biomass / Abundance, NA_real_)
    ) %>%
    dplyr::group_by(Year, Species) %>%
    dplyr::summarise(
      median = median(MeanWeight, na.rm = TRUE),
      q25    = quantile(MeanWeight, 0.25, na.rm = TRUE),
      q75    = quantile(MeanWeight, 0.75, na.rm = TRUE),
      q05    = quantile(MeanWeight, 0.05, na.rm = TRUE),
      q95    = quantile(MeanWeight, 0.95, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(panel = ind_to_panel[Species]) %>%
    dplyr::select(-Species)

  dplyr::bind_rows(agg_out, ind_out) %>%
    dplyr::mutate(panel = factor(panel, levels = panel_levels))
}

# ---------------------------------------------------------------------------
# Load RMSE rankings and determine top-10% indices
# ---------------------------------------------------------------------------
if (!file.exists("yield_rmse_per_sim.csv"))
  stop("yield_rmse_per_sim.csv not found — run yield_rmse_evaluation.R first.")

rmse_df      <- read.csv("yield_rmse_per_sim.csv") %>% dplyr::arrange(rank)
n_sims       <- nrow(rmse_df)
n_top10pct   <- ceiling(n_sims * 0.10)
top10pct_idx <- rmse_df$sim_index[1:n_top10pct]
message(sprintf("Top-10%% subset: n=%d sims (RMSE %.4f – %.4f)",
                n_top10pct, rmse_df$rmse[1], rmse_df$rmse[n_top10pct]))

# ---------------------------------------------------------------------------
# Load / cache raw data — fishing ensemble
# ---------------------------------------------------------------------------
fish_cache <- "abund_mw_top10pct_raw_fish.rds"
if (file.exists(fish_cache)) {
  message("Loading cached fishing raw data...")
  fish_raw <- readRDS(fish_cache)
} else {
  mc_path <- paste0("Output_large_files/monte_carlo_results/",
                    "combined_simulation_results/rerun_results/",
                    "mc_ensemble_2111_cleaned.rds")
  if (!file.exists(mc_path)) stop("MC ensemble RDS not found: ", mc_path)
  message("Loading MC fishing ensemble...")
  mc       <- readRDS(mc_path)
  sims_all <- mc$simulations
  valid_sims <- Filter(function(x)
    inherits(x, "MizerSim") && !any(is.nan(x@n)) && !any(is.infinite(x@n)),
    sims_all)
  message("  Extracting abundance & biomass for top-10% sims...")
  fish_raw <- extract_raw_abund_bm(valid_sims[top10pct_idx])
  saveRDS(fish_raw, fish_cache)
  message("  Cached to ", fish_cache)
  rm(mc, sims_all, valid_sims); gc()
}

# ---------------------------------------------------------------------------
# Load / cache raw data — climate-only ensemble (matched indices)
# ---------------------------------------------------------------------------
clim_cache <- "abund_mw_top10pct_raw_clim.rds"
if (file.exists(clim_cache)) {
  message("Loading cached climate-only raw data...")
  clim_raw <- readRDS(clim_cache)
} else {
  clim_path <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
  if (!file.exists(clim_path)) stop("Climate-only ensemble RDS not found: ", clim_path)
  message("Loading climate-only ensemble...")
  clim_mc        <- readRDS(clim_path)
  clim_sims_all  <- if (is.list(clim_mc) && !is.null(clim_mc$simulations))
    clim_mc$simulations else clim_mc
  clim_valid_sims <- Filter(function(x)
    inherits(x, "MizerSim") && !any(is.nan(x@n)) && !any(is.infinite(x@n)),
    clim_sims_all)
  message("  Extracting abundance & biomass for top-10% climate-only sims...")
  clim_raw <- extract_raw_abund_bm(clim_valid_sims[top10pct_idx])
  saveRDS(clim_raw, clim_cache)
  message("  Cached to ", clim_cache)
  rm(clim_mc, clim_sims_all, clim_valid_sims); gc()
}

# ---------------------------------------------------------------------------
# Build panel summaries
# ---------------------------------------------------------------------------
message("Building panel summaries...")
fish_abund_summary <- build_panel_summary_abundance(fish_raw)
clim_abund_summary <- build_panel_summary_abundance(clim_raw)
fish_mw_summary    <- build_panel_summary_meanweight(fish_raw)
clim_mw_summary    <- build_panel_summary_meanweight(clim_raw)

# ---------------------------------------------------------------------------
# Shared grid-plot builder (styled to match biomass grid)
# ---------------------------------------------------------------------------
make_grid_plot <- function(fish_sum, clim_sum, y_label,
                           y_scale_fn = scales::label_scientific()) {
  ggplot() +
    # Climate-only: IQR ribbon (light grey)
    geom_ribbon(data    = clim_sum,
                mapping = aes(x = Year, ymin = q25, ymax = q75),
                fill = "grey82", alpha = 0.6, colour = NA) +
    # Climate-only: IQR bounds (dashed grey)
    geom_line(data    = clim_sum,
              mapping = aes(x = Year, y = q25),
              colour = "grey62", linewidth = 0.3, linetype = "dashed") +
    geom_line(data    = clim_sum,
              mapping = aes(x = Year, y = q75),
              colour = "grey62", linewidth = 0.3, linetype = "dashed") +
    # Climate-only: median (thin solid grey)
    geom_line(data    = clim_sum,
              mapping = aes(x = Year, y = median),
              colour = "grey38", linewidth = 0.4) +
    # Fished: IQR ribbon (coloured, transparent)
    geom_ribbon(data    = fish_sum,
                mapping = aes(x = Year, ymin = q25, ymax = q75, fill = panel),
                alpha = 0.3) +
    # Fished: median line (coloured, thicker)
    geom_line(data    = fish_sum,
              mapping = aes(x = Year, y = median, colour = panel),
              linewidth = 1.1) +
    facet_wrap(~panel, ncol = 3, scales = "free_y") +
    scale_fill_manual(values   = panel_colors) +
    scale_colour_manual(values = panel_colors) +
    scale_y_continuous(labels = y_scale_fn) +
    theme_bw(base_size = 13) +
    theme(
      legend.position  = "none",
      strip.text       = element_text(face = "bold"),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    ) +
    labs(x = "Year", y = y_label)
}

# ---------------------------------------------------------------------------
# 1. Abundance grid
# ---------------------------------------------------------------------------
message("Building abundance grid...")
p_abund <- make_grid_plot(
  fish_abund_summary,
  clim_abund_summary,
  y_label    = "Total abundance (individuals)",
  y_scale_fn = scales::label_scientific()
)
ggsave("abundance_rmse_top10pct_grid.png", p_abund,
       width = 16, height = 9, dpi = 300)
message("Saved abundance_rmse_top10pct_grid.png")

# ---------------------------------------------------------------------------
# 2. Mean weight grid
# ---------------------------------------------------------------------------
message("Building mean weight grid...")
p_mw <- make_grid_plot(
  fish_mw_summary,
  clim_mw_summary,
  y_label    = "Mean individual mass (g)",
  y_scale_fn = scales::label_scientific()
)
ggsave("meanweight_rmse_top10pct_grid.png", p_mw,
       width = 16, height = 9, dpi = 300)
message("Saved meanweight_rmse_top10pct_grid.png")

# ---------------------------------------------------------------------------
# 3. Mean weight percent change (Exploited vs Unexploited), paired by sim
#
# For each sim, compute group mean weight under both ensembles, then derive
# the per-sim percent change: (fished_mw / climate_mw - 1) * 100.
# Ensemble quantiles are taken over those paired differences, so the summary
# reflects genuine inter-sim uncertainty rather than the ratio of means.
# ---------------------------------------------------------------------------
message("Building mean weight percent-change figure...")

# Per-sim mean weight for all panels (groups and individual species)
compute_persim_mw <- function(raw_df) {
  agg <- do.call(rbind, lapply(names(group_defs), function(gname) {
    raw_df %>%
      dplyr::filter(Species %in% group_defs[[gname]]) %>%
      dplyr::group_by(sim_i, Year) %>%
      dplyr::summarise(
        group_bm = sum(Biomass,   na.rm = TRUE),
        group_ab = sum(Abundance, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(
        MeanWeight = ifelse(group_ab > 0, group_bm / group_ab, NA_real_),
        panel      = gname
      ) %>%
      dplyr::select(Year, sim_i, panel, MeanWeight)
  }))

  ind <- raw_df %>%
    dplyr::filter(Species %in% names(ind_to_panel)) %>%
    dplyr::mutate(
      MeanWeight = ifelse(Abundance > 0, Biomass / Abundance, NA_real_),
      panel      = ind_to_panel[Species]
    ) %>%
    dplyr::select(Year, sim_i, panel, MeanWeight)

  dplyr::bind_rows(agg, ind)
}

fish_persim_mw <- compute_persim_mw(fish_raw)
clim_persim_mw <- compute_persim_mw(clim_raw)

# Join matched pairs and compute % change
ratio_persim <- dplyr::inner_join(
  fish_persim_mw, clim_persim_mw,
  by     = c("Year", "sim_i", "panel"),
  suffix = c("_fish", "_clim")
) %>%
  dplyr::mutate(
    pct_change = (MeanWeight_fish / MeanWeight_clim - 1) * 100
  ) %>%
  dplyr::filter(is.finite(pct_change))

ratio_summary <- ratio_persim %>%
  dplyr::group_by(Year, panel) %>%
  dplyr::summarise(
    median = median(pct_change, na.rm = TRUE),
    q25    = quantile(pct_change, 0.25, na.rm = TRUE),
    q75    = quantile(pct_change, 0.75, na.rm = TRUE),
    q05    = quantile(pct_change, 0.05, na.rm = TRUE),
    q95    = quantile(pct_change, 0.95, na.rm = TRUE),
    n_pairs = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(panel = factor(panel, levels = panel_levels))

key_years  <- c(1930, 1974)
key_labels <- c("Whaling\nstarts", "Krill fishing\nstarts")
event_df   <- data.frame(Year = key_years, label = key_labels)

# Text labels for global events restricted to the top row of facets only;
# vlines (event_df) still span all panels for alignment reference.
top_row_panels <- panel_levels[1:3]  # Large baleen whales, Sperm whales, Minke whales
event_label_df <- tidyr::expand_grid(
  Year  = key_years,
  panel = factor(top_row_panels, levels = panel_levels)
) %>%
  dplyr::left_join(data.frame(Year = key_years, label = key_labels), by = "Year")

# Species-specific fishing/hunting start events, drawn only in the matching panel.
# Loaded from the effort array; panels are matched via the facet variable so
# ggplot2 automatically restricts each row to its own panel.
all_sp_to_panel <- c(
  ind_to_panel,
  setNames(rep(names(group_defs), lengths(group_defs)), unlist(group_defs))
)

sp_event_labels <- c(
  "baleen whales"            = "Peak baleen whaling",
  "sperm whales"             = "Peak sperm whaling",
  "minke whales"             = "Peak minke whaling",
  "orca"                     = "Peak orca hunting",
  "leopard seals"            = "Peak leopard seal hunting",
  "toothfishes"              = "Peak toothfish fishing",
  "shelf and coastal fishes" = "Peak coastal fishing",
  "antarctic krill"          = "Peak krill fishing",
  "flying birds"             = "Peak seabird harvest",
  "small divers"             = "Peak penguin harvest",
  "squids"                   = "Peak squid fishing",
  "mesopelagic fishes"       = "Peak mesopelagic fishing",
  "bathypelagic fishes"      = "Peak bathypelagic fishing",
  "medium divers"            = "Peak seal hunting",
  "large divers"             = "Peak seal hunting"
)

if (file.exists("effort_array_1841_2010.rds")) {
  effort_arr <- readRDS("effort_array_1841_2010.rds")
  fishing_peak <- do.call(rbind, lapply(colnames(effort_arr), function(sp) {
    if (any(effort_arr[, sp] > 0))
      data.frame(
        Species    = sp,
        first_year = as.numeric(rownames(effort_arr))[which.max(effort_arr[, sp])],
        stringsAsFactors = FALSE
      )
  }))
  species_events_df <- fishing_peak %>%
    dplyr::mutate(
      panel    = all_sp_to_panel[Species],
      sp_label = dplyr::coalesce(sp_event_labels[Species], Species)
    ) %>%
    dplyr::filter(!is.na(panel)) %>%
    dplyr::mutate(panel = factor(panel, levels = panel_levels))
  message(sprintf("  Species fishing start events loaded: %d rows", nrow(species_events_df)))
} else {
  warning("effort_array_1841_2010.rds not found — species-specific vlines will be omitted.")
  species_events_df <- data.frame(
    Species = character(), first_year = numeric(),
    panel   = factor(character(), levels = panel_levels),
    sp_label = character()
  )
}

# Panel y-midpoints for mean weight % change plot: mid of IQR range per panel
panel_ymid_mw <- ratio_summary %>%
  dplyr::group_by(panel) %>%
  dplyr::summarise(
    y_mid = (max(q75, na.rm = TRUE) + min(q25, na.rm = TRUE)) / 2,
    .groups = "drop"
  )
species_events_mw <- dplyr::left_join(species_events_df, panel_ymid_mw, by = "panel")

p_mw_pct <- ggplot(ratio_summary, aes(x = Year)) +
  # IQR ribbon
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = panel), alpha = 0.3) +
  # Zero reference (no fishing effect)
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.7) +
  # Median line
  geom_line(aes(y = median, colour = panel), linewidth = 1.0) +
  # Key exploitation events — use geom_text with a data frame so facet_wrap
  # repeats the labels across all 12 panels correctly
  geom_vline(data = event_df, aes(xintercept = Year), inherit.aes = FALSE,
             linetype = "dotted", colour = "grey50", linewidth = 0.6) +
  geom_text(data = event_label_df, aes(x = Year, label = label), inherit.aes = FALSE,
            y = -Inf, hjust = 0.5, vjust = -0.3, size = 2.8, colour = "grey35") +
  # Species-specific fishing start vlines — drawn only in the matching panel,
  # coloured to match the panel, left of the vline, centred at zero
  geom_vline(data = species_events_mw, aes(xintercept = first_year, colour = panel),
             inherit.aes = FALSE, linetype = "dashed", linewidth = 0.5, alpha = 0.8) +
  geom_text(data = species_events_mw, aes(x = first_year, y = y_mid, label = sp_label),
            inherit.aes = FALSE, angle = 90,
            hjust = 0.5, vjust = -0.2, size = 2.3, colour = "grey10") +
  facet_wrap(~panel, ncol = 3, scales = "free_y") +
  scale_fill_manual(values   = panel_colors) +
  scale_colour_manual(values = panel_colors) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_bw(base_size = 13) +
  theme(
    legend.position  = "none",
    strip.text       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Year",
    y = "Mean individual mass: % change from unexploited",
    caption = "Line: median; ribbon: IQR (Q25–Q75) across paired top-10% RMSE sims. Dashed = 0% (no fishing effect)."
  )

ggsave("meanweight_pctchange_rmse_top10pct_grid.png", p_mw_pct,
       width = 16, height = 9, dpi = 300)
message("Saved meanweight_pctchange_rmse_top10pct_grid.png")

# ---------------------------------------------------------------------------
# 4. Abundance percent change (Exploited vs Unexploited), paired by sim
# ---------------------------------------------------------------------------
message("Building abundance percent-change figure...")

compute_persim_abundance <- function(raw_df) {
  agg <- do.call(rbind, lapply(names(group_defs), function(gname) {
    raw_df %>%
      dplyr::filter(Species %in% group_defs[[gname]]) %>%
      dplyr::group_by(sim_i, Year) %>%
      dplyr::summarise(Abundance = sum(Abundance, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(panel = gname) %>%
      dplyr::select(Year, sim_i, panel, Abundance)
  }))

  ind <- raw_df %>%
    dplyr::filter(Species %in% names(ind_to_panel)) %>%
    dplyr::mutate(panel = ind_to_panel[Species]) %>%
    dplyr::select(Year, sim_i, panel, Abundance)

  dplyr::bind_rows(agg, ind)
}

fish_persim_ab <- compute_persim_abundance(fish_raw)
clim_persim_ab <- compute_persim_abundance(clim_raw)

ab_ratio_persim <- dplyr::inner_join(
  fish_persim_ab, clim_persim_ab,
  by     = c("Year", "sim_i", "panel"),
  suffix = c("_fish", "_clim")
) %>%
  dplyr::mutate(
    pct_change = (Abundance_fish / Abundance_clim - 1) * 100
  ) %>%
  dplyr::filter(is.finite(pct_change))

ab_ratio_summary <- ab_ratio_persim %>%
  dplyr::group_by(Year, panel) %>%
  dplyr::summarise(
    median  = median(pct_change, na.rm = TRUE),
    q25     = quantile(pct_change, 0.25, na.rm = TRUE),
    q75     = quantile(pct_change, 0.75, na.rm = TRUE),
    q05     = quantile(pct_change, 0.05, na.rm = TRUE),
    q95     = quantile(pct_change, 0.95, na.rm = TRUE),
    n_pairs = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(panel = factor(panel, levels = panel_levels))

# Panel y-midpoints for abundance % change plot: mid of IQR range per panel
panel_ymid_ab <- ab_ratio_summary %>%
  dplyr::group_by(panel) %>%
  dplyr::summarise(
    y_mid = (max(q75, na.rm = TRUE) + min(q25, na.rm = TRUE)) / 2,
    .groups = "drop"
  )
species_events_ab <- dplyr::left_join(species_events_df, panel_ymid_ab, by = "panel")

p_ab_pct <- ggplot(ab_ratio_summary, aes(x = Year)) +
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = panel), alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.7) +
  geom_line(aes(y = median, colour = panel), linewidth = 1.0) +
  geom_vline(data = event_df, aes(xintercept = Year), inherit.aes = FALSE,
             linetype = "dotted", colour = "grey50", linewidth = 0.6) +
  geom_text(data = event_label_df, aes(x = Year, label = label), inherit.aes = FALSE,
            y = -Inf, hjust = 0.5, vjust = -0.3, size = 2.8, colour = "grey35") +
  # Species-specific fishing start vlines — drawn only in the matching panel,
  # coloured to match the panel, left of the vline, centred at zero
  geom_vline(data = species_events_ab, aes(xintercept = first_year, colour = panel),
             inherit.aes = FALSE, linetype = "dashed", linewidth = 0.5, alpha = 0.8) +
  geom_text(data = species_events_ab, aes(x = first_year, y = y_mid, label = sp_label),
            inherit.aes = FALSE, angle = 90,
            hjust = 0.5, vjust = -0.2, size = 2.3, colour = "grey10") +
  facet_wrap(~panel, ncol = 3, scales = "free_y") +
  scale_fill_manual(values   = panel_colors) +
  scale_colour_manual(values = panel_colors) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_bw(base_size = 13) +
  theme(
    legend.position  = "none",
    strip.text       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Year",
    y = "Abundance: % change from unexploited",
    caption = "Line: median; ribbon: IQR (Q25–Q75) across paired top-10% RMSE sims. Dashed = 0% (no fishing effect)."
  )

ggsave("abundance_pctchange_rmse_top10pct_grid.png", p_ab_pct,
       width = 16, height = 9, dpi = 300)
message("Saved abundance_pctchange_rmse_top10pct_grid.png")

# ---------------------------------------------------------------------------
# 5. Combined: mean individual mass + abundance % change, median lines only
# ---------------------------------------------------------------------------
message("Building combined percent-change figure...")

combined_pct <- dplyr::bind_rows(
  ratio_summary %>%
    dplyr::select(Year, panel, median) %>%
    dplyr::mutate(metric = "Mean individual mass"),
  ab_ratio_summary %>%
    dplyr::select(Year, panel, median) %>%
    dplyr::mutate(metric = "Abundance")
) %>%
  dplyr::mutate(metric = factor(metric, levels = c("Mean individual mass", "Abundance")))

# y-midpoints from the combined data range for species event label placement
combined_ymid <- combined_pct %>%
  dplyr::group_by(panel) %>%
  dplyr::summarise(
    y_mid = mean(range(median, na.rm = TRUE)),
    .groups = "drop"
  )
species_events_combined <- dplyr::left_join(species_events_df, combined_ymid, by = "panel")

p_combined_pct <- ggplot(combined_pct,
                         aes(x = Year, y = median, colour = panel, linetype = metric)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.7) +
  geom_line(linewidth = 0.9) +
  geom_vline(data = event_df, aes(xintercept = Year), inherit.aes = FALSE,
             linetype = "dotted", colour = "grey50", linewidth = 0.6) +
  geom_text(data = event_label_df, aes(x = Year, label = label), inherit.aes = FALSE,
            y = -Inf, hjust = 0.5, vjust = -0.3, size = 2.8, colour = "grey35") +
  geom_vline(data = species_events_combined, aes(xintercept = first_year, colour = panel),
             inherit.aes = FALSE, linetype = "dashed", linewidth = 0.5, alpha = 0.8) +
  geom_text(data = species_events_combined, aes(x = first_year, y = y_mid, label = sp_label),
            inherit.aes = FALSE, angle = 90,
            hjust = 0.5, vjust = -0.2, size = 2.3, colour = "grey10") +
  facet_wrap(~panel, ncol = 3, scales = "free_y") +
  scale_colour_manual(values = panel_colors, guide = "none") +
  scale_linetype_manual(
    values = c("Mean individual mass" = "solid", "Abundance" = "longdash"),
    name   = NULL
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  guides(linetype = guide_legend(
    override.aes = list(linewidth = 1.5),
    keywidth     = unit(2.5, "cm")
  )) +
  theme_bw(base_size = 13) +
  theme(
    legend.position  = "bottom",
    legend.text      = element_text(size = 11),
    strip.text       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Year",
    y = "% change from unexploited"
  )

ggsave("pctchange_combined_rmse_top10pct_grid.png", p_combined_pct,
       width = 16, height = 10, dpi = 300)
message("Saved pctchange_combined_rmse_top10pct_grid.png")

# ---------------------------------------------------------------------------
# Variants: x-axis restricted to 1900–2010
# Applied via coord_cartesian so no data is dropped (ribbons/CIs intact).
# ---------------------------------------------------------------------------
message("Saving 1900-start variants...")

xlim_1900 <- coord_cartesian(xlim = c(1900, NA))

ggsave("abundance_rmse_top10pct_grid_1900.png",
       p_abund + xlim_1900, width = 16, height = 9,  dpi = 300)

ggsave("meanweight_rmse_top10pct_grid_1900.png",
       p_mw    + xlim_1900, width = 16, height = 9,  dpi = 300)

ggsave("meanweight_pctchange_rmse_top10pct_grid_1900.png",
       p_mw_pct + xlim_1900, width = 16, height = 9,  dpi = 300)

ggsave("abundance_pctchange_rmse_top10pct_grid_1900.png",
       p_ab_pct + xlim_1900, width = 16, height = 9,  dpi = 300)

ggsave("pctchange_combined_rmse_top10pct_grid_1900.png",
       p_combined_pct + xlim_1900, width = 16, height = 10, dpi = 300)

message("Saved 1900-start variants.")

# ---------------------------------------------------------------------------
# 6. SD reference variants (1900 onward)
#
# For each panel, ±1 SD of the unexploited metric during the pre-exploitation
# baseline (1901–1929) is expressed as a % of the baseline mean (i.e., the
# coefficient of variation %).  This gives a panel-specific detectability
# threshold: when the exploited timeseries crosses the red lines, the change
# exceeds the natural inter-sim variability of the unexploited ensemble.
# Method mirrors the 1 SD reference used on the NBSS paired-difference plot.
# ---------------------------------------------------------------------------
message("Building SD-reference variants (1900 onward)...")

PRE_EXPLOIT_YEARS <- 1901:1929

# Helper: ± CV% reference data frame from a per-sim per-year metric data frame
make_sd_ref <- function(df, value_col) {
  df %>%
    dplyr::filter(Year %in% PRE_EXPLOIT_YEARS) %>%
    dplyr::group_by(panel) %>%
    dplyr::summarise(
      bl_mean = mean(.data[[value_col]], na.rm = TRUE),
      bl_sd   = sd(.data[[value_col]],   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      ref_pct = bl_sd / bl_mean * 100,
      panel   = factor(panel, levels = panel_levels)
    ) %>%
    { dplyr::bind_rows(
        dplyr::select(., panel, ref_pct),
        dplyr::mutate(dplyr::select(., panel, ref_pct), ref_pct = -ref_pct)
    ) }
}

mw_ref <- make_sd_ref(clim_persim_mw, "MeanWeight")
ab_ref <- make_sd_ref(clim_persim_ab, "Abundance")

sd_ref_layer <- function(ref_df, lt = "dashed") {
  geom_hline(data = ref_df, aes(yintercept = ref_pct),
             colour = "red", linetype = lt, linewidth = 0.6, alpha = 0.8,
             inherit.aes = FALSE)
}

# --- Mean weight % change + SD reference ---
ggsave(
  "meanweight_pctchange_rmse_top10pct_grid_1900_sdref.png",
  p_mw_pct + sd_ref_layer(mw_ref) + coord_cartesian(xlim = c(1900, NA)) +
    labs(caption = "±1 SD of unexploited mean weight (1901–1929 baseline) as % of baseline mean."),
  width = 16, height = 9, dpi = 300
)
message("  Saved meanweight_pctchange_rmse_top10pct_grid_1900_sdref.png")

# --- Abundance % change + SD reference ---
ggsave(
  "abundance_pctchange_rmse_top10pct_grid_1900_sdref.png",
  p_ab_pct + sd_ref_layer(ab_ref) + coord_cartesian(xlim = c(1900, NA)) +
    labs(caption = "±1 SD of unexploited abundance (1901–1929 baseline) as % of baseline mean."),
  width = 16, height = 9, dpi = 300
)
message("  Saved abundance_pctchange_rmse_top10pct_grid_1900_sdref.png")

# --- Combined % change + SD references for both metrics ---
# Linetype matches the metric: solid red = mean weight, longdash red = abundance
ggsave(
  "pctchange_combined_rmse_top10pct_grid_1900_sdref.png",
  p_combined_pct +
    sd_ref_layer(mw_ref, lt = "solid") +
    sd_ref_layer(ab_ref, lt = "longdash") +
    coord_cartesian(xlim = c(1900, NA)) +
    labs(caption = paste0(
      "Red solid: ±1 SD mean individual mass (unexploited 1901–1929). ",
      "Red long-dashed: ±1 SD abundance (same baseline)."
    )),
  width = 16, height = 10, dpi = 300
)
message("  Saved pctchange_combined_rmse_top10pct_grid_1900_sdref.png")

message("SD-reference variants complete.")

# ---------------------------------------------------------------------------
# 7. Single-column stacked facets, side-by-side
#
# Both % change metrics as 1-column × 12-row stacked facets (facet_grid),
# combined side-by-side:
#   Left:  abundance % change           (no strip labels)
#   Right: mean individual mass % change (panel labels on right, horizontal)
#
# facet_grid behaviour: x-axis ticks and labels appear only on the bottom
# panel automatically — no extra work required.
# ---------------------------------------------------------------------------
message("Building 1-column stacked side-by-side plot...")

library(patchwork)

# Global event text: 1-column layout → restrict to the single top row only
# (vs. top-3 in the 3-col grid, where each label appeared once per column)
event_label_df_1col <- data.frame(
  Year  = key_years,
  label = key_labels,
  panel = factor(panel_levels[1], levels = panel_levels),
  stringsAsFactors = FALSE
)

# Shared base theme
theme_1col_base <- theme_bw(base_size = 10) +
  theme(
    legend.position  = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.y  = unit(0.05, "lines"),
    axis.title       = element_text(size = 11),
    axis.text        = element_text(size = 9)
  )

# Helper — builds one 1-column % change facet plot
build_1col_pct <- function(summ_df, events_sp_df, y_lab, show_strips, shared_ylims = NULL) {
  p <- ggplot() +
    geom_hline(yintercept = 0,
               linetype = "dashed", colour = "grey40", linewidth = 0.5) +
    geom_ribbon(data = summ_df,
                aes(x = Year, ymin = q25, ymax = q75, fill = panel),
                alpha = 0.3) +
    geom_line(data = summ_df,
              aes(x = Year, y = median, colour = panel),
              linewidth = 0.8) +
    geom_vline(data = event_df,
               aes(xintercept = Year), inherit.aes = FALSE,
               linetype = "dotted", colour = "grey50", linewidth = 0.5) +
    geom_text(data = event_label_df_1col,
              aes(x = Year, label = label), inherit.aes = FALSE,
              y = -Inf, hjust = 1, vjust = -0.3,
              size = 3.4, colour = "grey35") +
    geom_vline(data = events_sp_df,
               aes(xintercept = first_year, colour = panel), inherit.aes = FALSE,
               linetype = "dashed", linewidth = 0.45, alpha = 0.8) +
    geom_text(data = events_sp_df,
              aes(x = first_year, y = y_mid, label = sp_label), inherit.aes = FALSE,
              angle = 90, hjust = 0.5, vjust = 1.3,
              size = 3.2, colour = "grey10") +
    facet_grid(rows = vars(panel), scales = "free_y") +
    coord_cartesian(xlim = c(1900, NA)) +
    scale_fill_manual(values   = panel_colors, guide = "none") +
    scale_colour_manual(values = panel_colors, guide = "none") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    theme_1col_base +
    labs(x = "Year", y = y_lab)

  # Optional: invisible anchor points to synchronise y scale with the partner plot
  if (!is.null(shared_ylims)) {
    p <- p + geom_blank(data = shared_ylims, aes(x = Year, y = y),
                        inherit.aes = FALSE)
  }

  if (show_strips) {
    # Horizontal text on the right-side strip (facet_grid row default position)
    p + theme(strip.text.y.right = element_text(angle = -90, hjust = 0.5,
                                                 vjust = 0.5, size = 9.6))
  } else {
    # Remove strip entirely so no blank space appears on the left plot
    p + theme(strip.text.y   = element_blank(),
              strip.background = element_blank())
  }
}

p_ab_1col <- build_1col_pct(
  ab_ratio_summary, species_events_ab,
  y_lab       = "Abundance: % change from unexploited",
  show_strips = FALSE
)

p_mw_1col <- build_1col_pct(
  ratio_summary, species_events_mw,
  y_lab       = "Mean ind. mass: % change from unexploited",
  show_strips = TRUE
)

# Wider right panel to accommodate the strip labels without compressing the plot area.
# Bold lowercase panel tags (a, b) without parentheses.
p_1col_combined <- (p_ab_1col + p_mw_1col +
  plot_layout(ncol = 2, widths = c(1, 1.05)) +
  plot_annotation(tag_levels = "a")) &
  theme(plot.tag = element_text(face = "bold", size = 12))

ggsave(
  "pctchange_1col_sidebyside.png",
  p_1col_combined,
  width = 14, height = 20, dpi = 300
)
message("Saved pctchange_1col_sidebyside.png")

# ---------------------------------------------------------------------------
# Fixed-y variant: y-axis is synchronised between the two columns for each
# group row, so relative magnitudes of abundance vs mean ind. mass change are
# directly comparable when scanning horizontally across the figure.
# The range for each row is the union of the 5th–95th percentile envelopes
# across both metrics, computed before calling build_1col_pct.
# ---------------------------------------------------------------------------
message("Building 1-column fixed-y variant...")

panel_ylims_fixed <- dplyr::bind_rows(
  ratio_summary    %>% dplyr::select(panel, q25, q75),
  ab_ratio_summary %>% dplyr::select(panel, q25, q75)
) %>%
  dplyr::group_by(panel) %>%
  dplyr::summarise(
    ymin = min(q25, na.rm = TRUE),
    ymax = max(q75, na.rm = TRUE),
    .groups = "drop"
  )

# Midpoint of the shared fixed range — used to centre peak-exploitation text
# so labels sit in the middle of the actual displayed window in both columns.
panel_ymid_fixed <- panel_ylims_fixed %>%
  dplyr::mutate(y_mid = (ymin + ymax) / 2) %>%
  dplyr::select(panel, y_mid)

species_events_mw_fixed <- dplyr::left_join(
  species_events_df, panel_ymid_fixed, by = "panel"
)
species_events_ab_fixed <- dplyr::left_join(
  species_events_df, panel_ymid_fixed, by = "panel"
)

# Two anchor rows per panel (at ymin and ymax) that geom_blank uses to expand
# the trained scale range, ensuring both columns share the same y limits.
blank_ylims <- dplyr::bind_rows(
  panel_ylims_fixed %>% dplyr::mutate(y = ymin),
  panel_ylims_fixed %>% dplyr::mutate(y = ymax)
) %>%
  dplyr::transmute(
    panel = factor(panel, levels = panel_levels),
    y     = y,
    Year  = 1900L
  )

p_ab_1col_fixed <- build_1col_pct(
  ab_ratio_summary, species_events_ab_fixed,
  y_lab        = "Abundance: % change from unexploited",
  show_strips  = FALSE,
  shared_ylims = blank_ylims
)

p_mw_1col_fixed <- build_1col_pct(
  ratio_summary, species_events_mw_fixed,
  y_lab        = "Mean ind. mass: % change from unexploited",
  show_strips  = TRUE,
  shared_ylims = blank_ylims
)

p_1col_fixed <- (p_ab_1col_fixed + p_mw_1col_fixed +
  plot_layout(ncol = 2, widths = c(1, 1.05)) +
  plot_annotation(tag_levels = "a")) &
  theme(plot.tag = element_text(face = "bold", size = 12))

ggsave(
  "pctchange_1col_sidebyside_fixedy.png",
  p_1col_fixed,
  width = 14, height = 20, dpi = 300
)
message("Saved pctchange_1col_sidebyside_fixedy.png")

message("=== Done ===")
