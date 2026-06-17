#############################################################################
# biomass_rmse_top10pct_grid.R
#
# 3-column x 4-row biomass timeseries grid for the top-10% RMSE fishing
# ensemble vs. matched climate-only ensemble, with observed data overlay.
#
# Aggregated groups (biomass summed WITHIN each sim before computing quantiles):
#   Zooplankton            = mesozooplankton + other krill + other macrozooplankton + salps
#   Pelagic fishes & squid = mesopelagic fishes + bathypelagic fishes + squids
#   Seabirds               = flying birds + small divers
#   Pinnipeds              = medium divers + large divers
#
# Panel layout (3 cols x 4 rows, left-to-right, top-to-bottom):
#   Row 1: Minke whales          | Sperm whales          | Baleen whales
#   Row 2: Orca                  | Leopard seals          | Pinnipeds
#   Row 3: Seabirds              | Toothfishes            | Shelf & coastal fishes
#   Row 4: Pelagic fishes & squid| Antarctic krill        | Zooplankton
#
# Caches per-sim raw biomass to RDS on first run; subsequent runs are fast.
#
# Outputs:
#   biomass_rmse_top10pct_grid.png
###############################################################################

suppressPackageStartupMessages({
  library(mizer)
  library(tidyverse)
  library(reshape2)
  library(scales)
})

message("=== Biomass RMSE top-10% grid (3 x 4) ===")

# ---------------------------------------------------------------------------
# Panel definitions
# ---------------------------------------------------------------------------

# Groups whose biomass is summed within each sim before quantile computation
group_defs <- list(
  "Zooplankton"            = c("mesozooplankton", "other krill",
                               "other macrozooplankton", "salps"),
  "Pelagic fishes & squid" = c("mesopelagic fishes", "bathypelagic fishes", "squids"),
  "Seabirds"               = c("flying birds", "small divers"),
  "Pinnipeds"              = c("medium divers", "large divers")
)

# Individual species -> panel name
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

# All species -> panel name (used for observed biomass aggregation)
all_sp_to_panel <- c(
  "mesozooplankton"          = "Zooplankton",
  "other krill"              = "Zooplankton",
  "other macrozooplankton"   = "Zooplankton",
  "salps"                    = "Zooplankton",
  "mesopelagic fishes"       = "Pelagic fishes & squid",
  "bathypelagic fishes"      = "Pelagic fishes & squid",
  "squids"                   = "Pelagic fishes & squid",
  "flying birds"             = "Seabirds",
  "small divers"             = "Seabirds",
  "medium divers"            = "Pinnipeds",
  "large divers"             = "Pinnipeds",
  "antarctic krill"          = "Antarctic krill",
  "toothfishes"              = "Toothfishes",
  "shelf and coastal fishes" = "Shelf & coastal fishes",
  "leopard seals"            = "Leopard seals",
  "minke whales"             = "Minke whales",
  "orca"                     = "Orca",
  "sperm whales"             = "Sperm whales",
  "baleen whales"            = "Large baleen whales"
)

# Ordered panel levels for facet_wrap (left-to-right, top-to-bottom)
panel_levels <- c(
  "Large baleen whales",    "Sperm whales",            "Minke whales",
  "Orca",                   "Leopard seals",            "Pinnipeds",
  "Seabirds",               "Toothfishes",              "Shelf & coastal fishes",
  "Pelagic fishes & squid", "Antarctic krill",          "Zooplankton"
)

panel_colors <- c(
  "Large baleen whales"     = "#FF61C3",  # = baleen whales in yield plot
  "Minke whales"            = "#00B9E3",  # = minke whales in yield plot
  "Sperm whales"            = "#DB72FB",  # = sperm whales in yield plot
  "Orca"                    = "#619CFF",  # = orca in yield plot
  "Leopard seals"           = "#E07B39",  # warm amber-orange (not in yield)
  "Pinnipeds"               = "#2B6CB0",  # medium steel blue (not in yield)
  "Seabirds"                = "#9E9E9E",  # neutral grey (not in yield)
  "Toothfishes"             = "#00C19F",  # = toothfishes in yield plot
  "Shelf & coastal fishes"  = "#93AA00",  # = shelf & coastal fishes in yield plot
  "Pelagic fishes & squid"  = "#D39200",  # = bathypelagic fishes in yield plot
  "Antarctic krill"         = "#F8766D",  # = antarctic krill in yield plot
  "Zooplankton"             = "#6A1B9A"   # deep purple (not in yield)
)

# ---------------------------------------------------------------------------
# Helper: extract raw per-sim per-species biomass from a list of MizerSims
# ---------------------------------------------------------------------------
extract_raw_biomass <- function(sim_list) {
  do.call(rbind, lapply(seq_along(sim_list), function(i) {
    mat <- getBiomass(sim_list[[i]])
    df  <- reshape2::melt(mat)
    names(df) <- c("Year", "Species", "Biomass")
    df$Year    <- as.numeric(as.character(df$Year))
    df$Species <- as.character(df$Species)
    df$sim_i   <- i
    df
  }))
}

# ---------------------------------------------------------------------------
# Helper: aggregate raw biomass to panel-level summary
#   - Aggregated groups: sum within each sim first, then quantile across sims
#   - Individual panels: quantile directly across sims
# ---------------------------------------------------------------------------
build_panel_summary <- function(raw_df) {
  # Aggregated groups
  agg_out <- do.call(rbind, lapply(names(group_defs), function(gname) {
    sp_list <- group_defs[[gname]]
    raw_df %>%
      filter(Species %in% sp_list) %>%
      group_by(sim_i, Year) %>%
      summarise(Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
      group_by(Year) %>%
      summarise(
        median = median(Biomass, na.rm = TRUE),
        q25    = quantile(Biomass, 0.25, na.rm = TRUE),
        q75    = quantile(Biomass, 0.75, na.rm = TRUE),
        q05    = quantile(Biomass, 0.05, na.rm = TRUE),
        q95    = quantile(Biomass, 0.95, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(panel = gname)
  }))

  # Individual species panels
  ind_out <- raw_df %>%
    filter(Species %in% names(ind_to_panel)) %>%
    group_by(Year, Species) %>%
    summarise(
      median = median(Biomass, na.rm = TRUE),
      q25    = quantile(Biomass, 0.25, na.rm = TRUE),
      q75    = quantile(Biomass, 0.75, na.rm = TRUE),
      q05    = quantile(Biomass, 0.05, na.rm = TRUE),
      q95    = quantile(Biomass, 0.95, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(panel = ind_to_panel[Species]) %>%
    select(-Species)

  bind_rows(agg_out, ind_out) %>%
    mutate(panel = factor(panel, levels = panel_levels))
}

# ---------------------------------------------------------------------------
# Load RMSE rankings and determine top-10% indices
# ---------------------------------------------------------------------------
if (!file.exists("yield_rmse_per_sim.csv"))
  stop("yield_rmse_per_sim.csv not found — run yield_rmse_evaluation.R first.")

rmse_df    <- read.csv("yield_rmse_per_sim.csv") %>% arrange(rank)
n_sims     <- nrow(rmse_df)
n_top10pct <- ceiling(n_sims * 0.10)
top10pct_idx <- rmse_df$sim_index[1:n_top10pct]
message(sprintf("Top-10%% subset: n=%d sims (RMSE %.4f – %.4f)",
                n_top10pct, rmse_df$rmse[1], rmse_df$rmse[n_top10pct]))

# ---------------------------------------------------------------------------
# Load / cache raw biomass — fishing ensemble
# ---------------------------------------------------------------------------
fish_cache <- "biomass_top10pct_raw_fish.rds"
if (file.exists(fish_cache)) {
  message("Loading cached fishing raw biomass...")
  fish_raw <- readRDS(fish_cache)
} else {
  mc_path <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
  if (!file.exists(mc_path)) stop("MC ensemble RDS not found: ", mc_path)
  message("Loading MC fishing ensemble...")
  mc       <- readRDS(mc_path)
  sims_all <- mc$simulations
  valid_sims <- Filter(function(x)
    inherits(x, "MizerSim") && !any(is.nan(x@n)) && !any(is.infinite(x@n)),
    sims_all)
  message("  Extracting biomass for top-10% sims...")
  fish_raw <- extract_raw_biomass(valid_sims[top10pct_idx])
  saveRDS(fish_raw, fish_cache)
  message("  Cached to ", fish_cache)
  rm(mc, sims_all, valid_sims)
  gc()
}

# ---------------------------------------------------------------------------
# Load / cache raw biomass — climate-only ensemble (matched indices)
# ---------------------------------------------------------------------------
clim_cache <- "biomass_top10pct_raw_clim.rds"
if (file.exists(clim_cache)) {
  message("Loading cached climate-only raw biomass...")
  clim_raw <- readRDS(clim_cache)
} else {
  clim_path <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
  if (!file.exists(clim_path)) stop("Climate-only ensemble RDS not found: ", clim_path)
  message("Loading climate-only ensemble...")
  clim_mc   <- readRDS(clim_path)
  clim_sims_all <- if (is.list(clim_mc) && !is.null(clim_mc$simulations))
    clim_mc$simulations else clim_mc
  clim_valid_sims <- Filter(function(x)
    inherits(x, "MizerSim") && !any(is.nan(x@n)) && !any(is.infinite(x@n)),
    clim_sims_all)
  message("  Extracting biomass for top-10% matched climate-only sims...")
  clim_raw <- extract_raw_biomass(clim_valid_sims[top10pct_idx])
  saveRDS(clim_raw, clim_cache)
  message("  Cached to ", clim_cache)
  rm(clim_mc, clim_sims_all, clim_valid_sims)
  gc()
}

# ---------------------------------------------------------------------------
# Build panel summaries
# ---------------------------------------------------------------------------
message("Building panel summaries...")
fish_summary <- build_panel_summary(fish_raw)
clim_summary <- build_panel_summary(clim_raw)

# ---------------------------------------------------------------------------
# Plot
# ---------------------------------------------------------------------------
message("Building 3 x 4 biomass grid plot...")

p_grid <- ggplot() +
  # Climate-only: IQR ribbon (light grey)
  geom_ribbon(data    = clim_summary,
              mapping = aes(x = Year, ymin = q25, ymax = q75),
              fill = "grey82", alpha = 0.6, colour = NA) +
  # Climate-only: IQR bounds (dashed grey lines)
  geom_line(data    = clim_summary,
            mapping = aes(x = Year, y = q25),
            colour = "grey62", linewidth = 0.3, linetype = "dashed") +
  geom_line(data    = clim_summary,
            mapping = aes(x = Year, y = q75),
            colour = "grey62", linewidth = 0.3, linetype = "dashed") +
  # Climate-only: median (thin solid grey)
  geom_line(data    = clim_summary,
            mapping = aes(x = Year, y = median),
            colour = "grey38", linewidth = 0.4) +
  # Fished: IQR ribbon (coloured)
  geom_ribbon(data    = fish_summary,
              mapping = aes(x = Year, ymin = q25, ymax = q75, fill = panel),
              alpha = 0.3) +
  # Fished: median line (coloured, thicker)
  geom_line(data    = fish_summary,
            mapping = aes(x = Year, y = median, colour = panel),
            linewidth = 1.1) +
  facet_wrap(~panel, ncol = 3, scales = "free_y") +
  scale_fill_manual(values   = panel_colors) +
  scale_colour_manual(values = panel_colors) +
  scale_y_continuous(labels = function(x) scales::number(x / 1e12)) +
  theme_bw(base_size = 13) +
  theme(
    legend.position  = "none",
    strip.text       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Year",
    y = expression(Biomass~(10^6~t))
  )

ggsave("biomass_rmse_top10pct_grid.png", p_grid,
       width = 16, height = 9, dpi = 300)
message("Saved biomass_rmse_top10pct_grid.png")

message("=== Done ===")
