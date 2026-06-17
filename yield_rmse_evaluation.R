###############################################################################
# yield_rmse_evaluation.R
#
# For each of the 2111 MC simulations, compute RMSE of modelled vs observed
# yield across all fished species and years within each species' effort window.
# RMSE is computed on log10(g + 1) scale to equalise species contributions
# (otherwise baleen whale / krill mass differences dominate).
#
# Outputs:
#   yield_rmse_per_sim.csv           — RMSE for all 2111 sims (ranked)
#   yield_rmse_corr_per_sim.csv      — RMSE + correlation metrics + pass/fail flag
#   yield_top10pct_corr_screen.csv   — correlation screen results for RMSE top-10% subset
#   yield_stacked_best_sim.png       — best single sim
#   yield_stacked_top1pct_mean.png   — mean of top 1% sims (from screened top-10% pool)
#   yield_stacked_top5pct_mean.png   — mean of top 5% sims (from screened top-10% pool)
#   yield_stacked_top10pct_mean.png  — mean of top 10% sims (from screened top-10% pool)
###############################################################################

suppressPackageStartupMessages({
  library(mizer)
  library(tidyverse)
  library(reshape2)
  library(scales)
})

message("=== Yield RMSE model evaluation ===")

# Correlation screening control (DBPM-style threshold).
# Screening is applied to the already RMSE-ranked top-10% subset.
CORR_THRESHOLD <- 0.5

PLOT_YEARS <- 1900:2010

species_order <- c(
  "mesozooplankton", "other krill", "other macrozooplankton",
  "antarctic krill", "salps", "mesopelagic fishes", "bathypelagic fishes",
  "shelf and coastal fishes", "flying birds", "small divers", "squids",
  "toothfishes", "leopard seals", "medium divers", "large divers",
  "minke whales", "orca", "sperm whales", "baleen whales"
)

# ---------------------------------------------------------------------------
# Load ensemble
# ---------------------------------------------------------------------------
mc_path <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
if (!file.exists(mc_path)) stop("MC ensemble RDS not found: ", mc_path)

message("Loading MC ensemble...")
mc        <- readRDS(mc_path)
sims_all  <- mc$simulations
valid_idx <- which(vapply(sims_all, function(x)
  inherits(x, "MizerSim") && !any(is.nan(x@n)) && !any(is.infinite(x@n)),
  logical(1)))
valid_sims <- sims_all[valid_idx]
n_sims <- length(valid_sims)
message("  Valid sims: ", n_sims)

# ---------------------------------------------------------------------------
# Effort windows (defines comparison window per species)
# ---------------------------------------------------------------------------
effort_arr <- readRDS("effort_array_1841_2010.rds")
effort_windows <- do.call(rbind, lapply(colnames(effort_arr), function(sp) {
  yrs <- as.numeric(rownames(effort_arr))[effort_arr[, sp] > 0]
  if (length(yrs) > 0)
    data.frame(Species = sp, first_year = min(yrs), last_year = max(yrs),
               stringsAsFactors = FALSE)
}))

# ---------------------------------------------------------------------------
# Observed yield — restricted to effort-window years per species
# ---------------------------------------------------------------------------
message("Loading observed yield...")
obs_raw <- read.csv("yield_observed_timeseries.csv")

obs_long <- obs_raw %>%
  reshape2::melt(id.vars = "Year", variable.name = "Species", value.name = "Yield_g") %>%
  mutate(
    Species = gsub("\\.", " ", as.character(Species)),
    Yield_g = pmax(coalesce(as.numeric(Yield_g), 0), 0)
  ) %>%
  left_join(effort_windows, by = "Species") %>%
  filter(!is.na(first_year), Year >= first_year, Year <= last_year) %>%
  select(Year, Species, Yield_obs = Yield_g)

fished_species <- unique(obs_long$Species)
message("  Fished species for RMSE: ", paste(fished_species, collapse = ", "))
message("  Comparison rows (year x species within effort windows): ", nrow(obs_long))

# ---------------------------------------------------------------------------
# Extract yield from every sim and compute RMSE
# ---------------------------------------------------------------------------
message("Extracting yield and computing RMSE per sim (slow step)...")

rmse_results <- vector("numeric", n_sims)
corr_results_raw <- vector("numeric", n_sims)
corr_results_log <- vector("numeric", n_sims)

# Accumulate per-sim yield for later plotting (stored as list of data.frames)
sim_yield_list <- vector("list", n_sims)

for (i in seq_len(n_sims)) {
  if (i %% 100 == 0) message("  Sim ", i, " / ", n_sims)

  mat <- getYield(valid_sims[[i]])
  df  <- reshape2::melt(mat)
  names(df) <- c("Year", "Species", "Yield_mod")
  df$Year    <- as.numeric(as.character(df$Year))
  df$Species <- as.character(df$Species)
  df$Yield_mod <- pmax(df$Yield_mod, 0)

  sim_yield_list[[i]] <- df

  # Join to observed on the effort-window rows only
  comp <- obs_long %>%
    left_join(df, by = c("Year", "Species")) %>%
    mutate(Yield_mod = coalesce(Yield_mod, 0))

  # log10(g + 1) RMSE
  rmse_results[i] <- sqrt(mean(
    (log10(comp$Yield_mod + 1) - log10(comp$Yield_obs + 1))^2,
    na.rm = TRUE
  ))

  # Correlation metrics across the same comparison rows.
  # Raw correlation is provided for transparency; log-scale is used for
  # DBPM-style screening to be consistent with the RMSE transform.
  corr_results_raw[i] <- suppressWarnings(cor(
    comp$Yield_mod,
    comp$Yield_obs,
    use = "complete.obs",
    method = "pearson"
  ))

  corr_results_log[i] <- suppressWarnings(cor(
    log10(comp$Yield_mod + 1),
    log10(comp$Yield_obs + 1),
    use = "complete.obs",
    method = "pearson"
  ))
}

# ---------------------------------------------------------------------------
# Rank and save
# ---------------------------------------------------------------------------
rmse_df <- data.frame(
  sim_index = seq_len(n_sims),
  valid_sim_index = valid_idx,
  rmse = rmse_results,
  cor_raw = corr_results_raw,
  cor_log = corr_results_log
) %>%
  arrange(rmse) %>%
  mutate(
    rank = seq_len(n()),
    pass_corr = !is.na(cor_log) & cor_log > CORR_THRESHOLD
  )

write.csv(rmse_df, "yield_rmse_per_sim.csv", row.names = FALSE)
message("Saved yield_rmse_per_sim.csv")

write.csv(rmse_df, "yield_rmse_corr_per_sim.csv", row.names = FALSE)
message("Saved yield_rmse_corr_per_sim.csv")

n_top10pct <- ceiling(n_sims * 0.10)
top10pct_screen <- rmse_df %>%
  slice(1:n_top10pct) %>%
  mutate(rank_within_top10pct = row_number())

write.csv(top10pct_screen, "yield_top10pct_corr_screen.csv", row.names = FALSE)
message("Saved yield_top10pct_corr_screen.csv")

top10pct_pass <- top10pct_screen %>%
  filter(pass_corr) %>%
  arrange(rmse)

message(sprintf("Correlation screen on RMSE top-10%%: cor_log > %.2f", CORR_THRESHOLD))
message(sprintf("  Top-10%% candidates: %d", n_top10pct))
message(sprintf("  Passing simulations: %d", nrow(top10pct_pass)))
message(sprintf("  Best sim rank 1: sim_index=%d, RMSE=%.4f",
                rmse_df$sim_index[1], rmse_df$rmse[1]))
message(sprintf("  Worst sim rank %d: sim_index=%d, RMSE=%.4f",
                nrow(rmse_df), rmse_df$sim_index[nrow(rmse_df)],
                rmse_df$rmse[nrow(rmse_df)]))

# ---------------------------------------------------------------------------
# Helper: build stacked-area data.frame from a list of yield data.frames
#   (averages across sims for multi-sim groups, or uses single sim directly)
# ---------------------------------------------------------------------------
build_stack_df <- function(yield_dfs, fished_sp, eff_windows, plot_years) {
  combined <- do.call(rbind, lapply(seq_along(yield_dfs), function(i) {
    yield_dfs[[i]] %>%
      filter(Species %in% fished_sp) %>%
      mutate(sim_i = i)
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

# ---------------------------------------------------------------------------
# Observed stacked data (same for all three plots — compute once)
# ---------------------------------------------------------------------------
obs_stack <- obs_raw %>%
  filter(Year %in% PLOT_YEARS) %>%
  reshape2::melt(id.vars = "Year", variable.name = "Species", value.name = "Yield_g") %>%
  mutate(
    Species = gsub("\\.", " ", as.character(Species)),
    Yield_g = pmax(coalesce(as.numeric(Yield_g), 0), 0),
    Yield_t = Yield_g / 1e6
  ) %>%
  filter(Species %in% fished_species) %>%
  tidyr::complete(Year = PLOT_YEARS, Species, fill = list(Yield_g = 0, Yield_t = 0)) %>%
  mutate(Species = factor(Species, levels = species_order))

# ---------------------------------------------------------------------------
# Colour palette
# ---------------------------------------------------------------------------
sp_palette <- setNames(
  scales::hue_pal()(length(fished_species)),
  fished_species
)

kt_formatter <- function(x) ifelse(x == 0, "0", paste0(comma(x / 1e3), " kt"))

make_stacked_plot <- function(mod_df, title_str = NULL) {
  ggplot(mod_df, aes(x = Year, y = median_t, fill = Species)) +
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
}

# ---------------------------------------------------------------------------
# 1. Best single sim
# ---------------------------------------------------------------------------
best_i    <- rmse_df$sim_index[1]
message("Building best-sim stacked plot (sim_index=", best_i, ", RMSE=",
        round(rmse_df$rmse[1], 4), ")...")

best_df <- build_stack_df(
  list(sim_yield_list[[best_i]]),
  fished_species, effort_windows, PLOT_YEARS
)

p_best <- make_stacked_plot(best_df)
ggsave("yield_stacked_best_sim.png", p_best, width = 12, height = 6, dpi = 300)
message("Saved yield_stacked_best_sim.png")

# ---------------------------------------------------------------------------
# 2-4. Top 1%, 5%, 10% (from correlation-passing RMSE top-10% pool)
# ---------------------------------------------------------------------------
if (nrow(top10pct_pass) == 0) {
  stop("No simulations passed correlation in the RMSE top-10% pool; cannot build top-1/5/10% figures.")
}

build_top_group_plot <- function(pct, file_stub) {
  n_target <- ceiling(n_sims * pct)
  n_use <- min(n_target, nrow(top10pct_pass))
  idx <- top10pct_pass$sim_index[1:n_use]

  message(sprintf("Building top-%.0f%% mean stacked plot (target=%d, used=%d)...",
                  pct * 100, n_target, n_use))

  out_df <- build_stack_df(
    sim_yield_list[idx],
    fished_species, effort_windows, PLOT_YEARS
  )

  p <- make_stacked_plot(out_df)
  out_file <- paste0("yield_stacked_", file_stub, "_mean.png")
  ggsave(out_file, p, width = 12, height = 6, dpi = 300)
  message("Saved ", out_file)
  p
}

p_top1pct <- build_top_group_plot(0.01, "top1pct")
p_top5pct <- build_top_group_plot(0.05, "top5pct")
p_top10pct <- build_top_group_plot(0.10, "top10pct")

# ---------------------------------------------------------------------------
# Observed plot — standalone + inset composite
# ---------------------------------------------------------------------------
message("Building observed stacked plot...")

# Clean standalone observed plot (no title, no vlines, no gridlines)
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

# Inset version: strip legend, axis titles, gridlines; add border + label
p_obs_inset <- p_obs_base +
  labs(title = "Observed yield") +
  theme(legend.position = "none",
        axis.title      = element_blank(),
        axis.text       = element_text(size = 6.5),
        plot.title      = element_text(size = 8, face = "bold", margin = margin(b = 2)),
        plot.margin     = margin(2, 2, 2, 2, "pt"),
        plot.background = element_rect(fill = "white", colour = "grey70", linewidth = 0.4))

# Composite: top-10% modelled yield with observed inset in top-right corner
# Reduced size to avoid overlapping the krill peak
library(patchwork)
p_top10pct_inset <- p_top10pct +
  inset_element(p_obs_inset,
                left = 0.65, bottom = 0.59, right = 0.99, top = 0.97,
                align_to = "panel")
ggsave("yield_stacked_top10pct_mean_with_obs_inset.png",
       p_top10pct_inset, width = 12, height = 6, dpi = 300)
message("Saved yield_stacked_top10pct_mean_with_obs_inset.png")

message("=== Done ===")
