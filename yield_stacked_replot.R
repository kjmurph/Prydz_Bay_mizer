###############################################################################
# yield_stacked_replot.R
#
# Fast replot of yield_stacked_top10pct_mean_with_obs_inset.png WITHOUT
# rerunning the slow RMSE loop.  Requires:
#   yield_rmse_per_sim.csv
#   yield_observed_timeseries.csv
#   effort_array_1841_2010.rds
#   yield_cached_top10pct_df.rds   (created on first run and reused)
#   yield_cached_obs_stack.rds     (created on first run and reused)
#
# The first time this runs it will load the MC ensemble to build top10pct_df,
# then cache it so subsequent runs skip the ensemble entirely.
###############################################################################

suppressPackageStartupMessages({
  library(mizer)
  library(tidyverse)
  library(reshape2)
  library(scales)
  library(patchwork)
})

PLOT_YEARS <- 1900:2010

species_order <- c(
  "mesozooplankton", "other krill", "other macrozooplankton",
  "antarctic krill", "salps", "mesopelagic fishes", "bathypelagic fishes",
  "shelf and coastal fishes", "flying birds", "small divers", "squids",
  "toothfishes", "leopard seals", "medium divers", "large divers",
  "minke whales", "orca", "sperm whales", "baleen whales"
)

# ---------------------------------------------------------------------------
# RMSE rankings
# ---------------------------------------------------------------------------
if (!file.exists("yield_rmse_per_sim.csv"))
  stop("yield_rmse_per_sim.csv not found — run yield_rmse_evaluation.R first.")
rmse_df    <- read.csv("yield_rmse_per_sim.csv") %>% arrange(rank)
n_sims     <- nrow(rmse_df)
n_top10pct <- ceiling(n_sims * 0.10)
top10pct_idx <- rmse_df$sim_index[1:n_top10pct]
message(sprintf("Top-10%% subset: n=%d sims", n_top10pct))

# ---------------------------------------------------------------------------
# Effort windows
# ---------------------------------------------------------------------------
effort_arr <- readRDS("effort_array_1841_2010.rds")
effort_windows <- do.call(rbind, lapply(colnames(effort_arr), function(sp) {
  yrs <- as.numeric(rownames(effort_arr))[effort_arr[, sp] > 0]
  if (length(yrs) > 0)
    data.frame(Species = sp, first_year = min(yrs), last_year = max(yrs),
               stringsAsFactors = FALSE)
}))

# ---------------------------------------------------------------------------
# Observed stacked data (cheap — just the CSV)
# ---------------------------------------------------------------------------
obs_cache <- "yield_cached_obs_stack.rds"
if (file.exists(obs_cache)) {
  obs_stack <- readRDS(obs_cache)
  message("Loaded obs_stack from cache.")
} else {
  obs_raw <- read.csv("yield_observed_timeseries.csv")
  obs_long_all <- obs_raw %>%
    reshape2::melt(id.vars = "Year", variable.name = "Species", value.name = "Yield_g") %>%
    mutate(
      Species = gsub("\\.", " ", as.character(Species)),
      Yield_g = pmax(coalesce(as.numeric(Yield_g), 0), 0)
    ) %>%
    left_join(effort_windows, by = "Species") %>%
    filter(!is.na(first_year))
  fished_species_all <- unique(obs_long_all$Species)

  obs_stack <- obs_raw %>%
    reshape2::melt(id.vars = "Year", variable.name = "Species", value.name = "Yield_g") %>%
    mutate(
      Species = gsub("\\.", " ", as.character(Species)),
      Yield_g = pmax(coalesce(as.numeric(Yield_g), 0), 0),
      Yield_t = Yield_g / 1e6
    ) %>%
    filter(Species %in% fished_species_all, Year %in% PLOT_YEARS) %>%
    tidyr::complete(Year = PLOT_YEARS, Species, fill = list(Yield_g = 0, Yield_t = 0)) %>%
    mutate(Species = factor(Species, levels = species_order))
  saveRDS(obs_stack, obs_cache)
  message("Built and cached obs_stack.")
}
# Derive fished_species in CSV column order — matches yield_rmse_evaluation.R
obs_raw_cols   <- read.csv("yield_observed_timeseries.csv", nrows = 1)
fished_species <- gsub("\\.", " ", setdiff(names(obs_raw_cols), "Year"))
fished_species <- fished_species[fished_species %in% as.character(unique(obs_stack$Species))]

# ---------------------------------------------------------------------------
# Top-10% modelled stacked data
# ---------------------------------------------------------------------------
top10pct_cache <- "yield_cached_top10pct_df.rds"
if (file.exists(top10pct_cache)) {
  top10pct_df <- readRDS(top10pct_cache)
  message("Loaded top10pct_df from cache.")
} else {
  message("Cache not found — loading MC ensemble to build top10pct_df...")
  mc_path <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
  mc <- readRDS(mc_path)
  sims_all <- mc$simulations

  build_stack_df <- function(sim_subset, fished_sp, eff_windows, plot_years) {
    combined <- do.call(rbind, lapply(seq_along(sim_subset), function(i) {
      mat <- getYield(sim_subset[[i]])
      df  <- reshape2::melt(mat)
      names(df) <- c("Year", "Species", "Yield_mod")
      df$Year    <- as.numeric(as.character(df$Year))
      df$Species <- as.character(df$Species)
      df$Yield_mod <- pmax(df$Yield_mod, 0)
      df %>% filter(Species %in% fished_sp) %>% mutate(sim_i = i)
    }))
    avg <- combined %>%
      group_by(Year, Species) %>%
      summarise(median_t = mean(Yield_mod, na.rm = TRUE) / 1e6, .groups = "drop")
    tidyr::expand_grid(Year = plot_years, Species = fished_sp) %>%
      left_join(avg, by = c("Year", "Species")) %>%
      left_join(eff_windows, by = "Species") %>%
      mutate(
        median_t = case_when(
          is.na(first_year)                    ~ 0,
          Year < first_year | Year > last_year ~ 0,
          TRUE                                 ~ coalesce(median_t, 0)
        ),
        Species = factor(Species, levels = species_order)
      )
  }
  top10pct_df <- build_stack_df(sims_all[top10pct_idx], fished_species,
                                 effort_windows, PLOT_YEARS)
  saveRDS(top10pct_df, top10pct_cache)
  message("Built and cached top10pct_df.")
}

# ---------------------------------------------------------------------------
# Colour palette
# ---------------------------------------------------------------------------
sp_palette <- setNames(
  scales::hue_pal()(length(fished_species)),
  fished_species
)

kt_formatter <- function(x) ifelse(x == 0, "0", paste0(comma(x / 1e3), " kt"))

# ---------------------------------------------------------------------------
# Modelled stacked plot (top 10%)
# ---------------------------------------------------------------------------
p_top10pct <- ggplot(top10pct_df, aes(x = Year, y = median_t, fill = Species)) +
  geom_area(position = "stack", alpha = 0.85, colour = NA) +
  scale_fill_manual(values = sp_palette) +
  scale_x_continuous(breaks = seq(1900, 2010, by = 10),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = kt_formatter,
                     expand = expansion(mult = c(0, 0.05))) +
  theme_bw(base_size = 13) +
  theme(legend.position  = "right",
        axis.text.x      = element_text(angle = 40, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x    = "Year",
       y    = expression(Yield~(10^3~t~y^{-1})),
       fill = "Species")

# ---------------------------------------------------------------------------
# Observed base plot
# ---------------------------------------------------------------------------
p_obs_base <- ggplot(obs_stack, aes(x = Year, y = Yield_t, fill = Species)) +
  geom_area(position = "stack", alpha = 0.85, colour = NA) +
  scale_fill_manual(values = sp_palette) +
  scale_x_continuous(breaks = seq(1900, 2010, by = 10),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = kt_formatter,
                     expand = expansion(mult = c(0, 0.05))) +
  theme_bw(base_size = 13) +
  theme(legend.position  = "right",
        axis.text.x      = element_text(angle = 40, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x    = "Year",
       y    = expression(Yield~(10^3~t~y^{-1})),
       fill = "Species")

ggsave("yield_stacked_observed.png", p_obs_base, width = 12, height = 6, dpi = 300)
message("Saved yield_stacked_observed.png")

# ---------------------------------------------------------------------------
# Inset: no legend, no axis titles, "Observed yield" label, border
# ---------------------------------------------------------------------------
p_obs_inset <- p_obs_base +
  labs(title = "Observed yield") +
  theme(legend.position = "none",
        axis.title      = element_blank(),
        axis.text       = element_text(size = 6.5),
        plot.title      = element_text(size = 8, face = "bold", margin = margin(b = 2)),
        plot.margin     = margin(2, 2, 2, 2, "pt"),
        plot.background = element_rect(fill = "white", colour = "grey70", linewidth = 0.4))

# ---------------------------------------------------------------------------
# Composite
# ---------------------------------------------------------------------------
p_top10pct_inset <- p_top10pct +
  inset_element(p_obs_inset,
                left = 0.65, bottom = 0.59, right = 0.99, top = 0.97,
                align_to = "panel")

ggsave("yield_stacked_top10pct_mean_with_obs_inset.png",
       p_top10pct_inset, width = 12, height = 6, dpi = 300)
message("Saved yield_stacked_top10pct_mean_with_obs_inset.png")

message("=== Done ===")
