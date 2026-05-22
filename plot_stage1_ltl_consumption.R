###############################################################################
# Stage 1 LTL Consumption Figures – 2001-2010 Period
#
# Produces three figures from the cached ensemble stats (Stage 1 outputs):
#
#  Fig 1 – Time series (1841-2010): median + IQR ribbon per group, both
#           scenarios overlaid, 2001-2010 window highlighted.
#
#  Fig 2 – Period comparison: violin / box plots of per-sim 2001-2010 means,
#           by predator group and scenario (fishing vs climate-only).
#
#  Fig 3 – Fishing-impact bar chart: median 2001-2010 consumption as % of
#           climate-only (unfished) baseline, for each group, with IQR error bars.
###############################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

CACHED_DIR <- "whale_consumption_outputs"
MODERN_YEARS <- 2001:2010
G_TO_TONNES  <- 1e-6

# ── colour / theme setup ─────────────────────────────────────────────────────
COL_FISHING  <- "#E64B35"   # red
COL_CLIMATE  <- "#4DBBD5"   # blue

theme_prydz <- function() {
  theme_classic(base_size = 7, base_family = "sans") +
    theme(
      strip.background = element_rect(fill = "white", colour = "grey75", linewidth = 0.5),
      strip.text       = element_text(face = "bold", size = 7, colour = "grey10"),
      panel.grid.major = element_line(colour = "grey93", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(colour = "grey75", fill = NA, linewidth = 0.5),
      axis.line        = element_blank(),
      axis.text        = element_text(size = 6, colour = "grey10"),
      axis.title       = element_text(size = 7),
      legend.position  = "bottom",
      legend.text      = element_text(size = 6)
    )
}

# ── group metadata ────────────────────────────────────────────────────────────
groups <- list(
  baleen_whales = list(
    label    = "Baleen whales (incl. minke)",
    fishing  = file.path(CACHED_DIR, "fishing_baleen_ltl_stats.csv"),
    climate  = file.path(CACHED_DIR, "climate_only_baleen_ltl_stats.csv"),
    fish_sim = file.path(CACHED_DIR, "fishing_baleen_ltl_all_sims.rds"),
    clim_sim = file.path(CACHED_DIR, "climate_only_baleen_ltl_all_sims.rds")
  ),
  all_whales = list(
    label    = "All whales\n(baleen + minke + sperm + orca)",
    fishing  = file.path(CACHED_DIR, "fishing_all_whales_ltl_stats.csv"),
    climate  = file.path(CACHED_DIR, "climate_only_all_whales_ltl_stats.csv"),
    fish_sim = file.path(CACHED_DIR, "fishing_all_whales_ltl_all_sims.rds"),
    clim_sim = file.path(CACHED_DIR, "climate_only_all_whales_ltl_all_sims.rds")
  ),
  seals = list(
    label    = "Seals & penguins",
    fishing  = file.path(CACHED_DIR, "fishing_seals_ltl_stats.csv"),
    climate  = file.path(CACHED_DIR, "climate_only_seals_ltl_stats.csv"),
    fish_sim = file.path(CACHED_DIR, "fishing_seals_ltl_all_sims.rds"),
    clim_sim = file.path(CACHED_DIR, "climate_only_seals_ltl_all_sims.rds")
  )
)

# ── helper: read annual stats CSV ─────────────────────────────────────────────
read_stats <- function(path, scenario, group) {
  df <- read.csv(path)
  df$scenario <- scenario
  df$group    <- group
  df
}

# ── helper: compute per-sim 2001-2010 period means from all_sims.rds ─────────
period_means_from_sims <- function(rds_path, scenario, group) {
  if (!file.exists(rds_path)) return(NULL)
  sim_list <- readRDS(rds_path)
  rows <- lapply(seq_along(sim_list), function(i) {
    df <- sim_list[[i]]
    sub <- df[df$year %in% MODERN_YEARS, ]
    if (nrow(sub) == 0) return(NULL)
    data.frame(sim_id = i,
               mean_tonnes = mean(sub$total_consumption, na.rm = TRUE) * G_TO_TONNES,
               scenario = scenario,
               group    = group)
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}

# ── load data ─────────────────────────────────────────────────────────────────
cat("Loading annual stats...\n")
ts_list  <- list()
sim_list <- list()

for (grp in names(groups)) {
  info <- groups[[grp]]
  for (scen in c("fishing", "climate")) {
    stats_path <- info[[scen]]
    if (file.exists(stats_path)) {
      ts_list[[paste(grp, scen, sep = "__")]] <- read_stats(stats_path, scen, grp)
    }
  }
  cat(sprintf("  Loading per-sim data for %s...\n", grp))
  fish_sims <- period_means_from_sims(info$fish_sim, "fishing",      grp)
  clim_sims <- period_means_from_sims(info$clim_sim, "climate_only", grp)
  if (!is.null(fish_sims)) sim_list[[paste(grp, "fishing",      sep = "__")]] <- fish_sims
  if (!is.null(clim_sims)) sim_list[[paste(grp, "climate_only", sep = "__")]] <- clim_sims
}

ts_df  <- do.call(rbind, ts_list)
sim_df <- do.call(rbind, sim_list)

# Tidy up labels
group_labels <- sapply(names(groups), function(g) groups[[g]]$label)
names(group_labels) <- names(groups)

ts_df$group_label    <- group_labels[ts_df$group]
sim_df$group_label   <- group_labels[sim_df$group]

ts_df$scenario_label  <- ifelse(ts_df$scenario == "fishing",  "Exploited",   "Unexploited")
sim_df$scenario_label <- ifelse(sim_df$scenario == "fishing", "Exploited",   "Unexploited")

# Force factor order: whales before seals, exploited before unexploited
ts_df$group_label  <- factor(ts_df$group_label,  levels = group_labels)
sim_df$group_label <- factor(sim_df$group_label, levels = group_labels)
ts_df$scenario_label  <- factor(ts_df$scenario_label,  levels = c("Exploited", "Unexploited"))
sim_df$scenario_label <- factor(sim_df$scenario_label, levels = c("Exploited", "Unexploited"))

###############################################################################
# Figure 1 – Time series with IQR ribbon
###############################################################################
cat("\nBuilding Figure 1 – time series...\n")

# Use median_tonnes, q25_tonnes, q75_tonnes from stats CSVs; also show q05/q95
fig1 <- ggplot(ts_df, aes(x = year, colour = scenario_label, fill = scenario_label)) +
  # outer ribbon: q05-q95
  geom_ribbon(aes(ymin = q05 * G_TO_TONNES, ymax = q95 * G_TO_TONNES),
              alpha = 0.12, colour = NA) +
  # inner ribbon: IQR
  geom_ribbon(aes(ymin = q25 * G_TO_TONNES, ymax = q75 * G_TO_TONNES),
              alpha = 0.25, colour = NA) +
  # median line
  geom_line(aes(y = median_tonnes), linewidth = 0.7) +
  # 2001-2010 highlight
  annotate("rect", xmin = 2001, xmax = 2010,
           ymin = -Inf, ymax = Inf,
           fill = "gold", alpha = 0.15) +
  annotate("text", x = 2005.5, y = Inf, label = "2001–2010",
           vjust = 1.5, size = 2.5, colour = "goldenrod4") +
  facet_wrap(~ group_label, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = c("Exploited" = COL_FISHING,
                                 "Unexploited" = COL_CLIMATE),
                      name = NULL) +
  scale_fill_manual(values = c("Exploited" = COL_FISHING,
                               "Unexploited" = COL_CLIMATE),
                    name = NULL) +
  scale_x_continuous(breaks = seq(1850, 2010, 20)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "LTL prey consumption \u2013 exploited vs unexploited ensemble",
    subtitle = "Median \u00b1 IQR (dark ribbon) and 5\u201395th percentile (light ribbon)",
    x        = "Year",
    y        = "LTL consumption (t yr\u207b\u00b9)"
  ) +
  theme_prydz()

ggsave(file.path(CACHED_DIR, "stage1_ltl_timeseries.png"),
       fig1, width = 10, height = 11, dpi = 200)
ggsave(file.path(CACHED_DIR, "stage1_ltl_timeseries.pdf"),
       fig1, width = 10, height = 11)
cat("  Saved: stage1_ltl_timeseries.png / .pdf\n")

###############################################################################
# Figure 2 – Violin + box plots of per-sim 2001-2010 period means
###############################################################################
cat("Building Figure 2 – violin plots (baleen whales and seals only)...\n")

# Filter to baleen whales group and seals group only
sim_df_violin <- sim_df %>% filter(group %in% c("baleen_whales", "seals"))

# Compute group-level summaries for annotation
sim_summary <- sim_df_violin %>%
  group_by(group_label, scenario_label) %>%
  summarise(
    med  = median(mean_tonnes, na.rm = TRUE),
    q25  = quantile(mean_tonnes, 0.25, na.rm = TRUE),
    q75  = quantile(mean_tonnes, 0.75, na.rm = TRUE),
    n    = n(),
    .groups = "drop"
  )

# Log scale needs positive values; replace zeros/negatives with a small floor
min_pos <- min(sim_df_violin$mean_tonnes[sim_df_violin$mean_tonnes > 0], na.rm = TRUE)
sim_df_log  <- sim_df_violin %>% mutate(mean_tonnes = pmax(mean_tonnes, min_pos * 0.1))
sim_sum_log <- sim_summary  # medians are positive so no adjustment needed

fig2 <- ggplot(sim_df_log, aes(x = scenario_label, y = mean_tonnes,
                                fill = scenario_label, colour = scenario_label)) +
  geom_violin(alpha = 0.35, trim = TRUE, scale = "width", linewidth = 0.4) +
  geom_boxplot(width = 0.18, alpha = 0.7, outlier.size = 0.6,
               outlier.alpha = 0.3, linewidth = 0.5,
               colour = "grey20", fill = "white") +
  facet_wrap(~ group_label, scales = "fixed", ncol = 2) +
  scale_fill_manual(values = c("Exploited" = COL_FISHING,
                               "Unexploited" = COL_CLIMATE),
                    guide = "none") +
  scale_colour_manual(values = c("Exploited" = COL_FISHING,
                                 "Unexploited" = COL_CLIMATE),
                      guide = "none") +
  scale_y_log10(labels = scales::label_comma()) +
  labs(
    title    = NULL,
    subtitle = NULL,
    x        = NULL,
    y        = "LTL Consumption in 2001\u20132010 (t yr\u207b\u00b9)"
  ) +
  theme_prydz() +
  theme(axis.text.x = element_text(size = 7, colour = "grey10"))

ggsave(file.path(CACHED_DIR, "stage1_ltl_violin_2001_2010.png"),
       fig2, width = 9, height = 5, dpi = 200)
ggsave(file.path(CACHED_DIR, "stage1_ltl_violin_2001_2010.pdf"),
       fig2, width = 9, height = 5)
cat("  Saved: stage1_ltl_violin_2001_2010.png / .pdf\n")

###############################################################################
# Figure 3 – Fishing impact: fished as % of unfished
###############################################################################
cat("Building Figure 3 – fishing impact...\n")

impact_df <- sim_df_violin %>%
  # per-sim: compute period mean tonnes already stored as mean_tonnes
  # pivot wide to get fished / unfished side-by-side per sim (baleen + seals only)
  pivot_wider(id_cols = c(sim_id, group, group_label),
              names_from = scenario,
              values_from = mean_tonnes) %>%
  # some sims may appear in one scenario but not the other (different n)
  filter(!is.na(fishing) & !is.na(climate_only)) %>%
  mutate(pct_of_unfished = fishing / climate_only * 100)

impact_summary <- impact_df %>%
  group_by(group_label) %>%
  summarise(
    n        = n(),
    median   = median(pct_of_unfished, na.rm = TRUE),
    q25      = quantile(pct_of_unfished, 0.25, na.rm = TRUE),
    q75      = quantile(pct_of_unfished, 0.75, na.rm = TRUE),
    q05      = quantile(pct_of_unfished, 0.05, na.rm = TRUE),
    q95      = quantile(pct_of_unfished, 0.95, na.rm = TRUE),
    .groups  = "drop"
  )

fig3 <- ggplot(impact_summary,
               aes(x = group_label, y = median, fill = group_label)) +
  geom_col(width = 0.55, alpha = 0.85, colour = "grey30", linewidth = 0.4) +
  # 5–95th percentile whisker
  geom_errorbar(aes(ymin = q05, ymax = q95), width = 0.18,
                colour = "grey20", linewidth = 0.6) +
  # IQR cap marks
  geom_crossbar(aes(ymin = q25, ymax = q75), width = 0.35,
                fill = NA, colour = "grey20", linewidth = 0.5) +
  geom_hline(yintercept = 100, linetype = "dashed",
             colour = "grey50", linewidth = 0.6) +
  annotate("text", x = 0.5, y = 101, label = "100% (= unexploited)",
           hjust = 0, vjust = 0, size = 2.5, colour = "grey50") +
  geom_text(aes(label = sprintf("%.0f%%", median)),
            vjust = -0.5, fontface = "bold", size = 2.5) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  scale_y_continuous(limits = c(0, NA), labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(
    title    = NULL,
    subtitle = NULL,
    x        = NULL,
    y        = "Exploited LTL consumption as % of unexploited (2001\u20132010)"
  ) +
  theme_prydz()

ggsave(file.path(CACHED_DIR, "stage1_ltl_fishing_impact_2001_2010.png"),
       fig3, width = 8, height = 5, dpi = 200)
ggsave(file.path(CACHED_DIR, "stage1_ltl_fishing_impact_2001_2010.pdf"),
       fig3, width = 8, height = 5)
cat("  Saved: stage1_ltl_fishing_impact_2001_2010.png / .pdf\n")

###############################################################################
# Figure 4 – Combined baleen-whale figure: Panel A (violin) + Panel B (impact)
###############################################################################
cat("Building Figure 4 – combined baleen whale figure...\n")

BALEEN_LABEL <- "Baleen whales (incl. minke)"

# --- Panel A: violin for baleen whales only ---
pA_df  <- sim_df_log  %>% filter(group == "baleen_whales")
pA_sum <- sim_sum_log %>% filter(group_label == BALEEN_LABEL)

panelA <- ggplot(pA_df, aes(x = scenario_label, y = mean_tonnes,
                             fill = scenario_label, colour = scenario_label)) +
  geom_violin(alpha = 0.35, trim = TRUE, scale = "width", linewidth = 0.4) +
  geom_boxplot(width = 0.18, alpha = 0.7, outlier.size = 0.6,
               outlier.alpha = 0.3, linewidth = 0.5,
               colour = "grey20", fill = "white") +
  scale_fill_manual(values = c("Exploited" = COL_FISHING,
                               "Unexploited" = COL_CLIMATE), guide = "none") +
  scale_colour_manual(values = c("Exploited" = COL_FISHING,
                                 "Unexploited" = COL_CLIMATE), guide = "none") +
  scale_y_log10(labels = scales::label_comma()) +
  labs(x = NULL,
       y = "LTL Consumption in 2001\u20132010 (t yr\u207b\u00b9)") +
  theme_prydz() +
  theme(axis.text.x = element_text(size = 7, colour = "grey10"))

# --- Panel B: impact bar for baleen whales only ---
pB_sum <- impact_summary %>% filter(group_label == BALEEN_LABEL)

panelB <- ggplot(pB_sum, aes(x = group_label, y = median, fill = group_label)) +
  geom_col(width = 0.5, alpha = 0.85, colour = "grey30", linewidth = 0.4,
           fill = "#66C2A5") +
  geom_errorbar(aes(ymin = q05, ymax = q95), width = 0.15,
                colour = "grey20", linewidth = 0.6) +
  geom_crossbar(aes(ymin = q25, ymax = q75), width = 0.30,
                fill = NA, colour = "grey20", linewidth = 0.5) +
  geom_hline(yintercept = 100, linetype = "dashed",
             colour = "grey50", linewidth = 0.6) +
  annotate("text", x = 0.55, y = 101, label = "100% (= unexploited)",
           hjust = 0, vjust = 0, size = 2.5, colour = "grey50") +
  geom_text(aes(label = sprintf("%.0f%%", median)),
            vjust = -0.5, fontface = "bold", size = 2.5) +
  scale_y_continuous(limits = c(0, NA), labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.10))) +
  labs(x = NULL,
       y = "Exploited LTL consumption as % of unexploited (2001\u20132010)") +
  theme_prydz()

fig4 <- panelA + panelB +
  plot_layout(widths = c(2, 1.5)) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "", tag_suffix = "",
                  theme = theme(plot.tag = element_text(face = "bold", size = 8)))

ggsave(file.path(CACHED_DIR, "stage1_ltl_baleen_combined.png"),
       fig4, width = 9, height = 5, dpi = 200)
ggsave(file.path(CACHED_DIR, "stage1_ltl_baleen_combined.pdf"),
       fig4, width = 9, height = 5)
cat("  Saved: stage1_ltl_baleen_combined.png / .pdf\n")

###############################################################################
# Print period summary table
###############################################################################
cat("\n=============================================================\n")
cat("2001-2010 Period Summary (Stage 1 groups)\n")
cat("=============================================================\n\n")

print_tbl <- sim_summary %>%
  arrange(group_label, scenario_label) %>%
  mutate(IQR_range = sprintf("[%s – %s]",
                             scales::comma(round(q25)),
                             scales::comma(round(q75))),
         Median = scales::comma(round(med)))
cat(sprintf("%-38s  %-26s  %14s  %26s\n",
            "Group", "Scenario", "Median (t/yr)", "IQR (t/yr)"))
cat(strrep("-", 108), "\n")
for (i in seq_len(nrow(print_tbl))) {
  cat(sprintf("%-38s  %-26s  %14s  %26s\n",
              as.character(print_tbl$group_label[i]),
              as.character(print_tbl$scenario_label[i]),
              print_tbl$Median[i],
              print_tbl$IQR_range[i]))
}

cat("\nFishing impact (% of unfished):\n")
cat(strrep("-", 60), "\n")
for (i in seq_len(nrow(impact_summary))) {
  cat(sprintf("  %-36s  %5.1f%%  IQR [%.1f%% – %.1f%%]\n",
              as.character(impact_summary$group_label[i]),
              impact_summary$median[i],
              impact_summary$q25[i],
              impact_summary$q75[i]))
}

cat("\nAll figures saved to:", CACHED_DIR, "\n")
cat("Done.\n")
