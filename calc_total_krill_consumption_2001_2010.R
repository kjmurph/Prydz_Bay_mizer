###############################################################################
# Total Antarctic krill consumption by all modelled predators
# Exploited vs. unexploited comparison, 2001-2010
#
# Predator groups included:
#   - All whales (baleen whales, minke whales, sperm whales, orca)
#   - Fish (mesopelagic fishes, bathypelagic fishes, shelf and coastal fishes,
#            toothfishes)
#   - Seals (leopard seals, small divers, medium divers, large divers)
#
# Note: flying birds and squids are not included (not extracted separately).
#
# Outputs:
#   whale_consumption_outputs/total_krill_consumption_2001_2010.csv
#   whale_consumption_outputs/total_krill_consumption_2001_2010_by_group.csv
###############################################################################

suppressPackageStartupMessages(library(dplyr))

OUTPUT_DIR  <- "whale_consumption_outputs"
G_TO_MT     <- 1e-12   # grams -> million tonnes
YEAR_START  <- 2001
YEAR_END    <- 2010

# ---------------------------------------------------------------------------
# Load per-simulation krill consumption for each predator group
# ---------------------------------------------------------------------------
cat("Loading per-sim krill consumption data...\n")

groups <- list(
  whales = list(
    fish = readRDS(file.path(OUTPUT_DIR, "fishing_all_whales_krill_all_sims.rds")),
    clim = readRDS(file.path(OUTPUT_DIR, "climate_only_all_whales_krill_all_sims.rds"))
  ),
  fish   = list(
    fish = readRDS(file.path(OUTPUT_DIR, "fishing_fish_krill_all_sims.rds")),
    clim = readRDS(file.path(OUTPUT_DIR, "climate_only_fish_krill_all_sims.rds"))
  ),
  seals  = list(
    fish = readRDS(file.path(OUTPUT_DIR, "fishing_seals_krill_all_sims.rds")),
    clim = readRDS(file.path(OUTPUT_DIR, "climate_only_seals_krill_all_sims.rds"))
  )
)

n_sims <- length(groups$whales$fish)
cat(sprintf("  %d simulations per group\n\n", n_sims))

# ---------------------------------------------------------------------------
# Per-group: mean krill consumption over 2001-2010 per simulation
# ---------------------------------------------------------------------------
cat("Computing 2001-2010 means per simulation...\n")

period_mean_per_sim <- function(sim_list, year_start, year_end) {
  vapply(sim_list, function(df) {
    df_sub <- df[df$year >= year_start & df$year <= year_end, ]
    mean(df_sub$total_consumption, na.rm = TRUE)
  }, numeric(1))
}

results_by_group <- lapply(names(groups), function(grp) {
  fish_means <- period_mean_per_sim(groups[[grp]]$fish, YEAR_START, YEAR_END)
  clim_means <- period_mean_per_sim(groups[[grp]]$clim, YEAR_START, YEAR_END)
  data.frame(
    group    = grp,
    scenario = rep(c("Exploited", "Unexploited"), each = n_sims),
    mean_g_yr = c(fish_means, clim_means)
  )
})
group_df <- do.call(rbind, results_by_group)

# ---------------------------------------------------------------------------
# Total across all groups per simulation
# ---------------------------------------------------------------------------
cat("Summing across predator groups...\n")

# Align by sim_id (1:n_sims, same order for all groups)
fishing_total <- Reduce("+", lapply(groups, function(g) {
  period_mean_per_sim(g$fish, YEAR_START, YEAR_END)
}))
climate_total <- Reduce("+", lapply(groups, function(g) {
  period_mean_per_sim(g$clim, YEAR_START, YEAR_END)
}))

total_df <- data.frame(
  scenario  = rep(c("Exploited", "Unexploited"), each = n_sims),
  mean_g_yr = c(fishing_total, climate_total)
)

# ---------------------------------------------------------------------------
# Summarise: median, mean, IQR across simulations
# ---------------------------------------------------------------------------
summarise_sims <- function(df) {
  df %>%
    group_by(scenario) %>%
    summarise(
      n_sims      = n(),
      median_Mt   = median(mean_g_yr, na.rm = TRUE) * G_TO_MT,
      mean_Mt     = mean(mean_g_yr,   na.rm = TRUE) * G_TO_MT,
      q25_Mt      = quantile(mean_g_yr, 0.25, na.rm = TRUE) * G_TO_MT,
      q75_Mt      = quantile(mean_g_yr, 0.75, na.rm = TRUE) * G_TO_MT,
      q05_Mt      = quantile(mean_g_yr, 0.05, na.rm = TRUE) * G_TO_MT,
      q95_Mt      = quantile(mean_g_yr, 0.95, na.rm = TRUE) * G_TO_MT,
      .groups = "drop"
    )
}

summary_total <- summarise_sims(total_df)
summary_total$group <- "ALL (whales + fish + seals)"

summary_by_group <- group_df %>%
  group_by(group, scenario) %>%
  summarise(
    n_sims      = n(),
    median_Mt   = median(mean_g_yr, na.rm = TRUE) * G_TO_MT,
    mean_Mt     = mean(mean_g_yr,   na.rm = TRUE) * G_TO_MT,
    q25_Mt      = quantile(mean_g_yr, 0.25, na.rm = TRUE) * G_TO_MT,
    q75_Mt      = quantile(mean_g_yr, 0.75, na.rm = TRUE) * G_TO_MT,
    q05_Mt      = quantile(mean_g_yr, 0.05, na.rm = TRUE) * G_TO_MT,
    q95_Mt      = quantile(mean_g_yr, 0.95, na.rm = TRUE) * G_TO_MT,
    .groups = "drop"
  )

# ---------------------------------------------------------------------------
# Ratio: exploited / unexploited per simulation, then summarise
# ---------------------------------------------------------------------------
ratio_total <- fishing_total / climate_total
ratio_group <- lapply(names(groups), function(grp) {
  r <- period_mean_per_sim(groups[[grp]]$fish, YEAR_START, YEAR_END) /
       period_mean_per_sim(groups[[grp]]$clim, YEAR_START, YEAR_END)
  data.frame(group = grp, ratio = r)
})
ratio_group_df <- do.call(rbind, ratio_group)

cat("\n=== Total Antarctic krill consumption: 2001-2010 mean ===\n")
cat(sprintf("%-28s  %10s  %10s  %10s\n",
            "Group", "Exploited", "Unexploited", "Ratio"))
cat(strrep("-", 62), "\n")

# Print each group
for (grp in names(groups)) {
  fish_med <- median(period_mean_per_sim(groups[[grp]]$fish, YEAR_START, YEAR_END)) * G_TO_MT
  clim_med <- median(period_mean_per_sim(groups[[grp]]$clim, YEAR_START, YEAR_END)) * G_TO_MT
  rat_med  <- median(ratio_group_df[ratio_group_df$group == grp, "ratio"])
  cat(sprintf("%-28s  %9.3f   %9.3f   %8.3f\n",
              grp, fish_med, clim_med, rat_med))
}
cat(strrep("-", 62), "\n")

# Total
fish_tot_med <- median(fishing_total) * G_TO_MT
clim_tot_med <- median(climate_total) * G_TO_MT
rat_tot_med  <- median(ratio_total)
cat(sprintf("%-28s  %9.3f   %9.3f   %8.3f\n",
            "TOTAL", fish_tot_med, clim_tot_med, rat_tot_med))

# IQR
fish_q25 <- quantile(fishing_total, 0.25) * G_TO_MT
fish_q75 <- quantile(fishing_total, 0.75) * G_TO_MT
clim_q25 <- quantile(climate_total, 0.25) * G_TO_MT
clim_q75 <- quantile(climate_total, 0.75) * G_TO_MT
rat_q25  <- quantile(ratio_total, 0.25)
rat_q75  <- quantile(ratio_total, 0.75)
cat(sprintf("  IQR                          [%5.3f-%5.3f]  [%5.3f-%5.3f]  [%5.3f-%5.3f]\n",
            fish_q25, fish_q75, clim_q25, clim_q75, rat_q25, rat_q75))

cat("\nUnits: million tonnes per year (Mt yr-1)\n")
cat(sprintf("Period: %d-%d mean, n = %d simulations\n", YEAR_START, YEAR_END, n_sims))
cat("Note: flying birds and squids not included in totals.\n\n")

# ---------------------------------------------------------------------------
# Save CSVs
# ---------------------------------------------------------------------------
out_total <- file.path(OUTPUT_DIR, "total_krill_consumption_2001_2010.csv")
write.csv(summary_total, out_total, row.names = FALSE)
cat("Saved:", out_total, "\n")

out_group <- file.path(OUTPUT_DIR, "total_krill_consumption_2001_2010_by_group.csv")
write.csv(summary_by_group, out_group, row.names = FALSE)
cat("Saved:", out_group, "\n")

cat("\nDone.\n")
