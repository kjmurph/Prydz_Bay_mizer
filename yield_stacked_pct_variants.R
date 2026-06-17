###############################################################################
# yield_stacked_pct_variants.R
#
# Stacked area plots of modelled yield for top 1%, 5%, and 10% RMSE ensembles
# with observed yield inset and 95% CI error bars at auto-detected yield peaks.
#
# Requires:
#   yield_rmse_per_sim.csv
#   yield_observed_timeseries.csv
#   effort_array_1841_2010.rds
#   yield_cached_obs_stack.rds                    (from yield_stacked_replot.R)
#   mc_ensemble_2111_cleaned.rds                  (loaded once if any cache missing)
#
# Per-percentile caches written on first run:
#   yield_cached_top{P}pct_stack.rds              mean yield per species per year
#   yield_cached_top{P}pct_total_per_sim.rds      total yield per year per sim
#
# Outputs (one per percentile):
#   yield_stacked_top1pct_with_errorbars.png
#   yield_stacked_top5pct_with_errorbars.png
#   yield_stacked_top10pct_with_errorbars.png
###############################################################################

suppressPackageStartupMessages({
  library(mizer)
  library(tidyverse)
  library(reshape2)
  library(scales)
  library(patchwork)
})

PLOT_YEARS  <- 1900:2010
PERCENTILES <- c(1, 5, 10)
PEAK_WINDOW <- 3     # half-window (years) for local-maxima detection
MIN_PEAK_KT <- 10    # minimum peak height in kt to count as a peak

species_order <- c(
  "mesozooplankton", "other krill", "other macrozooplankton",
  "antarctic krill", "salps", "mesopelagic fishes", "bathypelagic fishes",
  "shelf and coastal fishes", "flying birds", "small divers", "squids",
  "toothfishes", "leopard seals", "medium divers", "large divers",
  "minke whales", "orca", "sperm whales", "baleen whales"
)

# ---------------------------------------------------------------------------
# Local-maxima detector
# Peak must exceed all neighbours within ±PEAK_WINDOW years and exceed
# MIN_PEAK_KT threshold (values are in tonnes; threshold is in kt * 1000).
# ---------------------------------------------------------------------------
find_peaks <- function(total_t, years, window = PEAK_WINDOW,
                       min_t = MIN_PEAK_KT * 1000) {
  n       <- length(total_t)
  is_peak <- logical(n)
  for (i in seq_len(n)) {
    lo <- max(1L, i - window)
    hi <- min(n,  i + window)
    if (total_t[i] == max(total_t[lo:hi]) && total_t[i] >= min_t) {
      is_peak[i] <- TRUE
    }
  }
  years[is_peak]
}

# ---------------------------------------------------------------------------
# RMSE rankings
# ---------------------------------------------------------------------------
if (!file.exists("yield_rmse_per_sim.csv"))
  stop("yield_rmse_per_sim.csv not found — run yield_rmse_evaluation.R first.")
rmse_df <- read.csv("yield_rmse_per_sim.csv") %>% arrange(rank)
n_sims  <- nrow(rmse_df)
message(sprintf("Total sims in RMSE table: %d", n_sims))
for (p in PERCENTILES) {
  message(sprintf("  Top %d%%: n = %d sims", p, ceiling(n_sims * p / 100)))
}

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
# Observed stacked data
# ---------------------------------------------------------------------------
obs_cache <- "yield_cached_obs_stack.rds"
if (file.exists(obs_cache)) {
  obs_stack <- readRDS(obs_cache)
  message("Loaded obs_stack from cache.")
} else {
  message("Building obs_stack from CSV...")
  obs_raw <- read.csv("yield_observed_timeseries.csv")
  obs_long_all <- obs_raw %>%
    reshape2::melt(id.vars = "Year", variable.name = "Species", value.name = "Yield_g") %>%
    mutate(Species = gsub("\\.", " ", as.character(Species)),
           Yield_g = pmax(coalesce(as.numeric(Yield_g), 0), 0)) %>%
    left_join(effort_windows, by = "Species") %>%
    filter(!is.na(first_year))
  fished_species_all <- unique(obs_long_all$Species)
  obs_stack <- obs_raw %>%
    reshape2::melt(id.vars = "Year", variable.name = "Species", value.name = "Yield_g") %>%
    mutate(Species = gsub("\\.", " ", as.character(Species)),
           Yield_g = pmax(coalesce(as.numeric(Yield_g), 0), 0),
           Yield_t = Yield_g / 1e6) %>%
    filter(Species %in% fished_species_all, Year %in% PLOT_YEARS) %>%
    tidyr::complete(Year = PLOT_YEARS, Species,
                    fill = list(Yield_g = 0, Yield_t = 0)) %>%
    mutate(Species = factor(Species, levels = species_order))
  saveRDS(obs_stack, obs_cache)
  message("Cached obs_stack.")
}

obs_raw_cols   <- read.csv("yield_observed_timeseries.csv", nrows = 1)
fished_species <- gsub("\\.", " ", setdiff(names(obs_raw_cols), "Year"))
fished_species <- fished_species[fished_species %in%
                                   as.character(unique(obs_stack$Species))]

# ---------------------------------------------------------------------------
# Colour palette (consistent across all plots)
# ---------------------------------------------------------------------------
sp_palette    <- setNames(scales::hue_pal()(length(fished_species)), fished_species)
kt_formatter  <- function(x) ifelse(x == 0, "0", paste0(comma(x / 1e3), " kt"))

# ---------------------------------------------------------------------------
# Observed inset (shared across all percentile plots)
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
  labs(x = "Year", y = expression(Yield~(10^3~t~y^{-1})), fill = "Species")

p_obs_inset <- p_obs_base +
  labs(title = "Observed yield") +
  theme(legend.position = "none",
        axis.title      = element_blank(),
        axis.text       = element_text(size = 6.5),
        plot.title      = element_text(size = 8, face = "bold", margin = margin(b = 2)),
        plot.margin     = margin(2, 2, 2, 2, "pt"),
        plot.background = element_rect(fill = "white", colour = "grey70", linewidth = 0.4))

# ---------------------------------------------------------------------------
# Determine whether the MC ensemble needs to be loaded
# (needed if any stack or per-sim total cache is missing)
# ---------------------------------------------------------------------------
need_mc <- any(sapply(PERCENTILES, function(p) {
  !file.exists(sprintf("yield_cached_top%dpct_stack.rds", p)) ||
    !file.exists(sprintf("yield_cached_top%dpct_total_per_sim.rds", p)) ||
    !file.exists(sprintf("yield_cached_top%dpct_median_stack.rds", p))
}))

# Legacy cache from yield_stacked_replot.R can supply the top-10% stack data
legacy_top10_cache <- "yield_cached_top10pct_df.rds"

sims_all <- NULL
if (need_mc) {
  message("\nLoading MC ensemble (this will take a moment)...")
  mc_path <- paste0("Output_large_files/monte_carlo_results/",
                    "combined_simulation_results/rerun_results/",
                    "mc_ensemble_2111_cleaned.rds")
  if (!file.exists(mc_path)) stop("MC ensemble not found: ", mc_path)
  mc       <- readRDS(mc_path)
  sims_all <- mc$simulations
  message(sprintf("  Loaded %d simulations.", length(sims_all)))
}

# ---------------------------------------------------------------------------
# Build (or load) per-percentile data
# ---------------------------------------------------------------------------
results <- list()

for (pct in PERCENTILES) {
  message(sprintf("\n--- Top %d%% ---", pct))
  n_top   <- ceiling(n_sims * pct / 100)
  top_idx <- rmse_df$sim_index[1:n_top]

  stack_cache <- sprintf("yield_cached_top%dpct_stack.rds", pct)
  total_cache <- sprintf("yield_cached_top%dpct_total_per_sim.rds", pct)

  # ---- Mean yield per species per year (stacked area data) ----
  if (file.exists(stack_cache)) {
    stack_df <- readRDS(stack_cache)
    message("  Stack: loaded from cache.")
  } else if (pct == 10 && file.exists(legacy_top10_cache)) {
    stack_df <- readRDS(legacy_top10_cache)
    # Ensure Species column uses correct factor levels
    stack_df <- stack_df %>%
      mutate(Species = factor(as.character(Species), levels = species_order))
    saveRDS(stack_df, stack_cache)
    message("  Stack: migrated from legacy top10 cache.")
  } else {
    message(sprintf("  Stack: building from %d sims...", n_top))
    sim_subset <- sims_all[top_idx]
    combined <- do.call(rbind, lapply(seq_along(sim_subset), function(i) {
      mat <- getYield(sim_subset[[i]])
      df  <- reshape2::melt(mat)
      names(df) <- c("Year", "Species", "Yield_g")
      df$Year    <- as.numeric(as.character(df$Year))
      df$Species <- as.character(df$Species)
      df %>% filter(Species %in% fished_species) %>%
        mutate(Yield_g = pmax(Yield_g, 0))
    }))
    avg <- combined %>%
      group_by(Year, Species) %>%
      summarise(median_t = mean(Yield_g, na.rm = TRUE) / 1e6, .groups = "drop")
    stack_df <- tidyr::expand_grid(Year = PLOT_YEARS, Species = fished_species) %>%
      left_join(avg, by = c("Year", "Species")) %>%
      left_join(effort_windows, by = "Species") %>%
      mutate(
        median_t = case_when(
          is.na(first_year)                    ~ 0,
          Year < first_year | Year > last_year ~ 0,
          TRUE                                 ~ coalesce(median_t, 0)
        ),
        Species = factor(Species, levels = species_order)
      )
    saveRDS(stack_df, stack_cache)
    message("  Stack: built and cached.")
  }

  # ---- Per-sim total yield per year (for 95% CI error bars) ----
  if (file.exists(total_cache)) {
    total_per_sim <- readRDS(total_cache)
    message("  Per-sim totals: loaded from cache.")
  } else {
    message(sprintf("  Per-sim totals: building from %d sims...", n_top))
    sim_subset  <- sims_all[top_idx]
    total_per_sim <- do.call(rbind, lapply(seq_along(sim_subset), function(i) {
      mat <- getYield(sim_subset[[i]])
      df  <- reshape2::melt(mat)
      names(df) <- c("Year", "Species", "Yield_g")
      df$Year    <- as.numeric(as.character(df$Year))
      df$Species <- as.character(df$Species)
      df %>%
        filter(Species %in% fished_species, Year %in% PLOT_YEARS) %>%
        left_join(effort_windows, by = "Species") %>%
        filter(!is.na(first_year), Year >= first_year, Year <= last_year) %>%
        group_by(Year) %>%
        summarise(total_t = sum(pmax(Yield_g, 0), na.rm = TRUE) / 1e6,
                  .groups = "drop") %>%
        mutate(sim_i = i)
    }))
    saveRDS(total_per_sim, total_cache)
    message("  Per-sim totals: built and cached.")
  }

  # ---- Median yield per species per year (for median-variant stacked area) ----
  median_stack_cache <- sprintf("yield_cached_top%dpct_median_stack.rds", pct)
  if (file.exists(median_stack_cache)) {
    median_stack_df <- readRDS(median_stack_cache)
    message("  Median stack: loaded from cache.")
  } else {
    message(sprintf("  Median stack: building from %d sims...", n_top))
    sim_subset <- sims_all[top_idx]
    combined_med <- do.call(rbind, lapply(seq_along(sim_subset), function(i) {
      mat <- getYield(sim_subset[[i]])
      df  <- reshape2::melt(mat)
      names(df) <- c("Year", "Species", "Yield_g")
      df$Year    <- as.numeric(as.character(df$Year))
      df$Species <- as.character(df$Species)
      df %>% filter(Species %in% fished_species) %>%
        mutate(Yield_g = pmax(Yield_g, 0))
    }))
    med <- combined_med %>%
      group_by(Year, Species) %>%
      summarise(median_t = median(Yield_g, na.rm = TRUE) / 1e6, .groups = "drop")
    median_stack_df <- tidyr::expand_grid(Year = PLOT_YEARS, Species = fished_species) %>%
      left_join(med, by = c("Year", "Species")) %>%
      left_join(effort_windows, by = "Species") %>%
      mutate(
        median_t = case_when(
          is.na(first_year)                    ~ 0,
          Year < first_year | Year > last_year ~ 0,
          TRUE                                 ~ coalesce(median_t, 0)
        ),
        Species = factor(Species, levels = species_order)
      )
    saveRDS(median_stack_df, median_stack_cache)
    message("  Median stack: built and cached.")
  }

  results[[as.character(pct)]] <- list(
    stack_df        = stack_df,
    median_stack_df = median_stack_df,
    total_per_sim   = total_per_sim
  )
}

# ---------------------------------------------------------------------------
# Build and save plots per percentile (mean-stack variant + median-stack variant)
# ---------------------------------------------------------------------------

# Shared helper: IQR error bars with centre dot at column y_col
add_bars <- function(p, ci_df, y_col) {
  p +
    geom_errorbar(
      data = ci_df, aes(x = Year, ymin = lo25, ymax = hi75),
      inherit.aes = FALSE, width = 1.2, linewidth = 0.6, colour = "white"
    ) +
    geom_errorbar(
      data = ci_df, aes(x = Year, ymin = lo25, ymax = hi75),
      inherit.aes = FALSE, width = 1.0, linewidth = 0.4, colour = "grey20"
    ) +
    geom_point(
      data = ci_df, aes(x = Year, y = .data[[y_col]]),
      inherit.aes = FALSE, shape = 21, size = 2,
      fill = "white", colour = "grey20", stroke = 0.5
    )
}

# Shared helper: build peak CI table for a given stack data frame.
# Peaks are detected from the stack's own totals so the centre dot sits
# exactly at the top of the rendered stacked area.
make_peak_ci <- function(sdf, total_ps) {
  totals <- sdf %>%
    group_by(Year) %>%
    summarise(centre = sum(median_t, na.rm = TRUE), .groups = "drop") %>%
    arrange(Year)
  pk <- find_peaks(totals$centre, totals$Year)
  iqr <- total_ps %>%
    filter(Year %in% pk) %>%
    group_by(Year) %>%
    summarise(lo25 = quantile(total_t, 0.25, na.rm = TRUE),
              hi75 = quantile(total_t, 0.75, na.rm = TRUE),
              .groups = "drop")
  totals %>% filter(Year %in% pk) %>% left_join(iqr, by = "Year")
}

# Shared scale/theme list applied with Reduce("+", ...)
plot_scales <- function() list(
  scale_fill_manual(values = sp_palette),
  scale_x_continuous(breaks = seq(1900, 2010, by = 10),
                     expand = expansion(mult = c(0.01, 0.01))),
  scale_y_continuous(labels = kt_formatter,
                     expand = expansion(mult = c(0, 0.05))),
  theme_bw(base_size = 13),
  theme(legend.position  = "right",
        axis.text.x      = element_text(angle = 40, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()),
  labs(x = "Year", y = expression(Yield~(10^3~t~y^{-1})), fill = "Species")
)

for (pct in PERCENTILES) {
  message(sprintf("\nPlotting top %d%%...", pct))

  stack_df        <- results[[as.character(pct)]]$stack_df
  median_stack_df <- results[[as.character(pct)]]$median_stack_df
  total_per_sim   <- results[[as.character(pct)]]$total_per_sim

  obs_inset <- inset_element(p_obs_inset,
                             left = 0.65, bottom = 0.59,
                             right = 0.99, top = 0.97,
                             align_to = "panel")

  # --- Variant A: area shows MEAN per species; dot at sum-of-means (stack top) ---
  ci_mean <- make_peak_ci(stack_df, total_per_sim)
  message(sprintf("  Mean peaks:   %s", paste(ci_mean$Year, collapse = ", ")))
  p_mean  <- Reduce(`+`,
    c(list(ggplot(stack_df, aes(x = Year, y = median_t, fill = Species)) +
             geom_area(position = "stack", alpha = 0.85, colour = NA)),
      plot_scales()))
  p_mean  <- add_bars(p_mean, ci_mean, "centre") + obs_inset
  f_mean  <- sprintf("yield_stacked_top%dpct_mean_errorbars.png", pct)
  ggsave(f_mean, p_mean, width = 12, height = 6, dpi = 300)
  message(sprintf("  Saved: %s", f_mean))

  # --- Variant B: area shows MEDIAN per species; dot at sum-of-medians (stack top) ---
  ci_med  <- make_peak_ci(median_stack_df, total_per_sim)
  message(sprintf("  Median peaks: %s", paste(ci_med$Year, collapse = ", ")))
  p_med   <- Reduce(`+`,
    c(list(ggplot(median_stack_df, aes(x = Year, y = median_t, fill = Species)) +
             geom_area(position = "stack", alpha = 0.85, colour = NA)),
      plot_scales()))
  p_med   <- add_bars(p_med, ci_med, "centre") + obs_inset
  f_med   <- sprintf("yield_stacked_top%dpct_median_errorbars.png", pct)
  ggsave(f_med, p_med, width = 12, height = 6, dpi = 300)
  message(sprintf("  Saved: %s", f_med))
}

message("\n=== Done ===")
