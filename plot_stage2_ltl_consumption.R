###############################################################################
# Stage 2 LTL Consumption Figures – Individual Species, 2001-2010
#
# Reads the per-sim period-mean RDS files from Stage 2 and produces:
#
#  Fig 4 – Grouped bar chart: individual species + group aggregates,
#           fished vs unfished, median ± IQR.
#
#  Fig 5 – Fishing-impact dot chart: fished as % of unfished per species/group.
#
#  Fig 6 – LTL prey breakdown: stacked bar showing which LTL category each
#           predator consumes most (from by_prey CSV).
###############################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

CACHED_DIR  <- "whale_consumption_outputs"
G_TO_TONNES <- 1e-6

COL_FISHING <- "#E64B35"
COL_CLIMATE <- "#4DBBD5"

theme_prydz <- function() {
  theme_bw(base_size = 11) +
    theme(strip.background  = element_rect(fill = "grey92", colour = NA),
          strip.text        = element_text(face = "bold"),
          panel.grid.minor  = element_blank(),
          legend.position   = "bottom")
}

###############################################################################
# Load Stage 2 summary CSV
###############################################################################
cat("Loading Stage 2 outputs...\n")

summary_df <- read.csv(file.path(CACHED_DIR, "ltl_consumption_2001_2010_summary.csv"),
                        stringsAsFactors = FALSE)
prey_df    <- read.csv(file.path(CACHED_DIR, "ltl_consumption_2001_2010_by_prey.csv"),
                        stringsAsFactors = FALSE)

# --- define display order and labels ----------------------------------------
# Individual species (not aggregates)
individual_species <- c("baleen whales", "minke whales", "sperm whales", "orca",
                         "leopard seals", "small divers", "medium divers", "large divers")

# Group aggregates derived from Stage 2 (bracket labels)
group_aggregates <- c("[baleen_whales] aggregate", "[toothed_whales] aggregate",
                       "[all_whales] aggregate",   "[seals] aggregate",
                       "[all_marine_mams] aggregate")

# Nice labels for plotting
nice_labels <- c(
  "baleen whales"              = "Baleen whales (sp.)",
  "minke whales"               = "Minke whales",
  "sperm whales"               = "Sperm whales",
  "orca"                       = "Orca",
  "leopard seals"              = "Leopard seals",
  "small divers"               = "Small divers",
  "medium divers"              = "Medium divers",
  "large divers"               = "Large divers",
  "[baleen_whales] aggregate"  = "▶ Baleen group total",
  "[toothed_whales] aggregate" = "▶ Toothed whale total",
  "[all_whales] aggregate"     = "▶ All whales total",
  "[seals] aggregate"          = "▶ All seals total",
  "[all_marine_mams] aggregate"= "▶ All large marine mammals"
)

# Species category for colouring facets
species_category <- c(
  "baleen whales"              = "Baleen whale",
  "minke whales"               = "Baleen whale",
  "sperm whales"               = "Toothed whale",
  "orca"                       = "Toothed whale",
  "leopard seals"              = "Seal / diver",
  "small divers"               = "Seal / diver",
  "medium divers"              = "Seal / diver",
  "large divers"               = "Seal / diver",
  "[baleen_whales] aggregate"  = "Aggregate",
  "[toothed_whales] aggregate" = "Aggregate",
  "[all_whales] aggregate"     = "Aggregate",
  "[seals] aggregate"          = "Aggregate",
  "[all_marine_mams] aggregate"= "Aggregate"
)

display_order <- c(individual_species, group_aggregates)

# Filter to Stage 2 rows only (exclude duplicated cached Stage 1 rows)
stage2_df <- summary_df %>%
  filter(predator %in% display_order) %>%
  mutate(
    nice_label = nice_labels[predator],
    category   = species_category[predator],
    nice_label = factor(nice_label, levels = nice_labels[display_order]),
    scenario_label = ifelse(scenario == "fishing",
                            "Fished", "Unfished (climate-only)"),
    scenario_label = factor(scenario_label,
                            levels = c("Fished", "Unfished (climate-only)"))
  )

###############################################################################
# Figure 4 – Grouped bar chart, individual + aggregates, log scale
###############################################################################
cat("Building Figure 4 – grouped bar chart...\n")

# Separate individual and aggregate for styling
stage2_ind  <- stage2_df %>% filter(predator %in% individual_species)
stage2_grp  <- stage2_df %>% filter(predator %in% group_aggregates)

# Colour by category
cat_colours <- c("Baleen whale"  = "#2166AC",
                 "Toothed whale" = "#762A83",
                 "Seal / diver"  = "#1B7837",
                 "Aggregate"     = "grey40")

fig4 <- ggplot(stage2_df,
               aes(x = fct_rev(nice_label), y = median_tonnes,
                   fill = category, alpha = scenario_label)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_errorbar(aes(ymin = q25_tonnes, ymax = q75_tonnes),
                position = position_dodge(width = 0.7),
                width = 0.3, linewidth = 0.5, colour = "grey30") +
  scale_fill_manual(values = cat_colours, name = "Group") +
  scale_alpha_manual(values = c("Fished" = 0.95, "Unfished (climate-only)" = 0.55),
                     name = NULL) +
  scale_y_log10(labels = scales::label_comma(),
                breaks = c(1, 10, 100, 1e3, 1e4, 1e5, 1e6),
                limits = c(0.5, NA)) +
  coord_flip() +
  labs(
    title    = "LTL prey consumption by predator group – 2001–2010",
    subtitle = "Median ± IQR (log₁₀ scale). Dark = fished; light = unfished (climate-only).",
    x        = NULL,
    y        = "LTL consumption (tonnes / year, log scale)"
  ) +
  guides(fill  = guide_legend(order = 1, nrow = 2),
         alpha = guide_legend(order = 2)) +
  theme_prydz() +
  theme(legend.box = "vertical")

ggsave(file.path(CACHED_DIR, "stage2_ltl_by_species_bar.png"),
       fig4, width = 11, height = 7, dpi = 200)
ggsave(file.path(CACHED_DIR, "stage2_ltl_by_species_bar.pdf"),
       fig4, width = 11, height = 7)
cat("  Saved: stage2_ltl_by_species_bar.png / .pdf\n")

###############################################################################
# Figure 5 – Fishing-impact dot chart (% of unfished)
###############################################################################
cat("Building Figure 5 – fishing impact dot chart...\n")

# Load raw per-sim data for proper pair-matched % calculation
fishing_sims    <- readRDS(file.path(CACHED_DIR, "per_species_period_means_fishing.rds"))
climate_sims    <- readRDS(file.path(CACHED_DIR, "per_species_period_means_climate_only.rds"))

# Compute group aggregates from raw per-sim data
group_members <- list(
  "[baleen_whales] aggregate"  = c("baleen whales", "minke whales"),
  "[toothed_whales] aggregate" = c("sperm whales", "orca"),
  "[all_whales] aggregate"     = c("baleen whales", "minke whales", "sperm whales", "orca"),
  "[seals] aggregate"          = c("leopard seals", "small divers", "medium divers", "large divers"),
  "[all_marine_mams] aggregate"= c("baleen whales", "minke whales", "sperm whales", "orca",
                                    "leopard seals", "small divers", "medium divers", "large divers")
)

add_aggregates <- function(ltl_df) {
  if (is.null(ltl_df)) return(NULL)
  agg_rows <- lapply(names(group_members), function(grp_name) {
    members <- group_members[[grp_name]]
    ltl_df %>%
      filter(predator %in% members) %>%
      group_by(sim_id) %>%
      summarise(ltl_total_g_yr = sum(ltl_total_g_yr, na.rm = TRUE), .groups = "drop") %>%
      mutate(predator = grp_name)
  })
  rbind(ltl_df, do.call(rbind, agg_rows))
}

f_ltl <- add_aggregates(fishing_sims$ltl_total)
c_ltl <- add_aggregates(climate_sims$ltl_total)

# Pair sims by sim_id and compute % of unfished
impact_raw <- inner_join(
  f_ltl %>% rename(fished = ltl_total_g_yr),
  c_ltl %>% rename(unfished = ltl_total_g_yr),
  by = c("sim_id", "predator")
) %>%
  mutate(pct = ifelse(unfished > 0, fished / unfished * 100, NA_real_)) %>%
  filter(predator %in% display_order)

# Remove toothed whales from the fishing impact plot – they consume negligible LTL
toothed_exclude <- c("sperm whales", "orca", "[toothed_whales] aggregate")

impact_summary <- impact_raw %>%
  filter(!predator %in% toothed_exclude) %>%
  group_by(predator) %>%
  summarise(
    n       = n(),
    median  = median(pct, na.rm = TRUE),
    q25     = quantile(pct, 0.25, na.rm = TRUE),
    q75     = quantile(pct, 0.75, na.rm = TRUE),
    q05     = quantile(pct, 0.05, na.rm = TRUE),
    q95     = quantile(pct, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    nice_label = factor(nice_labels[predator], levels = nice_labels[display_order]),
    category   = species_category[predator]
  ) %>%
  filter(!is.na(nice_label))

fig5 <- ggplot(impact_summary,
               aes(x = fct_rev(nice_label), y = median, colour = category)) +
  # 5-95 whisker
  geom_segment(aes(xend = fct_rev(nice_label), y = q05, yend = q95),
               linewidth = 0.7, alpha = 0.4) +
  # IQR bar
  geom_segment(aes(xend = fct_rev(nice_label), y = q25, yend = q75),
               linewidth = 2.5, alpha = 0.6) +
  # median dot
  geom_point(size = 3.5) +
  geom_text(aes(label = sprintf("%.0f%%", median)),
            hjust = -0.4, size = 3, fontface = "bold") +
  geom_hline(yintercept = 100, linetype = "dashed",
             colour = "grey50", linewidth = 0.6) +
  annotate("text", x = 0.6, y = 101, label = "100% (= unfished)",
           hjust = 0, vjust = 0, size = 3, colour = "grey50") +
  scale_colour_manual(values = cat_colours, name = "Group") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.05, 0.12))) +
  coord_flip() +
  labs(
    title    = "Fishing / whaling impact on LTL consumption – 2001–2010",
    subtitle = "Fished as % of unfished (climate-only). Dot = median; thick bar = IQR; thin bar = 5–95th %.",
    x        = NULL,
    y        = "Fished LTL consumption as % of unfished"
  ) +
  theme_prydz() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey88"))

ggsave(file.path(CACHED_DIR, "stage2_ltl_fishing_impact_by_species.png"),
       fig5, width = 11, height = 6, dpi = 200)
ggsave(file.path(CACHED_DIR, "stage2_ltl_fishing_impact_by_species.pdf"),
       fig5, width = 11, height = 6)
cat("  Saved: stage2_ltl_fishing_impact_by_species.png / .pdf\n")

###############################################################################
# Figure 6 – Prey composition: proportional stacked bars with total annotation
#
# Style matches Fig 4 (horizontal bars) but bars are split by LTL prey type.
# Includes individual species + baleen-group and seals-group aggregates.
# Excludes toothed whales (negligible LTL consumers).
###############################################################################
cat("Building Figure 6 – prey composition (proportional stacked bars)...\n")

# Prey labels and colours
prey_labels <- c(
  "antarctic krill"        = "Antarctic krill",
  "other krill"            = "Other krill",
  "mesozooplankton"        = "Mesozooplankton",
  "other macrozooplankton" = "Other macrozooplankton",
  "salps"                  = "Salps"
)
prey_colours <- c(
  "Antarctic krill"        = "#E64B35",
  "Other krill"            = "#F39B7F",
  "Mesozooplankton"        = "#4DBBD5",
  "Other macrozooplankton" = "#00A087",
  "Salps"                  = "#3C5488"
)

# Meaningful LTL consumers (no toothed whales)
meaningful_preds <- c("baleen whales", "minke whales",
                       "leopard seals", "small divers", "medium divers", "large divers")

# Compute aggregates by summing individual species medians per prey × scenario
build_agg_prey <- function(members, agg_name) {
  prey_df %>%
    filter(predator %in% members) %>%
    group_by(prey, scenario) %>%
    summarise(median_tonnes = sum(median_tonnes, na.rm = TRUE), .groups = "drop") %>%
    mutate(predator = agg_name)
}

prey_df_ext <- rbind(
  prey_df %>% filter(predator %in% meaningful_preds) %>%
    select(predator, prey, scenario, median_tonnes),
  build_agg_prey(c("baleen whales", "minke whales"),                                       "[baleen_whales] aggregate"),
  build_agg_prey(c("leopard seals", "small divers", "medium divers", "large divers"),     "[seals] aggregate"),
  build_agg_prey(c("baleen whales", "minke whales",
                   "leopard seals", "small divers", "medium divers", "large divers"),      "[all_marine_mams] aggregate")
)

# Display order: individuals first, then aggregates
prey_display_order <- c(meaningful_preds,
                         "[baleen_whales] aggregate", "[seals] aggregate",
                         "[all_marine_mams] aggregate")

prey_plot6 <- prey_df_ext %>%
  mutate(
    nice_label     = factor(nice_labels[predator], levels = rev(nice_labels[prey_display_order])),
    prey_label     = factor(prey_labels[prey],      levels = prey_labels),
    scenario_label = factor(ifelse(scenario == "fishing", "Fished", "Unfished (climate-only)"),
                            levels = c("Fished", "Unfished (climate-only)"))
  ) %>%
  filter(!is.na(nice_label), !is.na(prey_label)) %>%
  # Compute proportion within each predator × scenario
  group_by(predator, scenario, scenario_label, nice_label) %>%
  mutate(
    total_ltl  = sum(median_tonnes, na.rm = TRUE),
    proportion = median_tonnes / total_ltl * 100
  ) %>%
  ungroup()

# Label data for total tonnes annotation (right edge of each bar)
bar_labels6 <- prey_plot6 %>%
  distinct(nice_label, scenario_label, total_ltl)

fig6 <- ggplot(prey_plot6,
               aes(x = nice_label, y = proportion, fill = prey_label)) +
  geom_col(position = "stack", width = 0.68, colour = "white", linewidth = 0.25) +
  # annotate with total tonnes consumed
  geom_text(data = bar_labels6,
            aes(x = nice_label, y = 103,
                label = paste0(scales::comma(round(total_ltl)), " t/yr")),
            inherit.aes = FALSE, hjust = 0, size = 2.7, colour = "grey30") +
  facet_wrap(~ scenario_label, ncol = 2) +
  scale_fill_manual(values = prey_colours, name = "LTL prey") +
  scale_y_continuous(limits = c(0, 145),
                     breaks = c(0, 25, 50, 75, 100),
                     labels = function(x) paste0(x, "%")) +
  coord_flip() +
  labs(
    title    = "LTL prey composition by predator – 2001–2010",
    subtitle = "Proportion (%) of total LTL consumption by prey category. Annotation = total consumption (tonnes/year).",
    x        = NULL,
    y        = "% of total LTL consumption"
  ) +
  theme_prydz() +
  theme(legend.position = "right")

ggsave(file.path(CACHED_DIR, "stage2_ltl_prey_composition.png"),
       fig6, width = 13, height = 6, dpi = 200)
ggsave(file.path(CACHED_DIR, "stage2_ltl_prey_composition.pdf"),
       fig6, width = 13, height = 6)
cat("  Saved: stage2_ltl_prey_composition.png / .pdf\n")

###############################################################################
# Print clean summary table
###############################################################################
cat("\n=============================================================\n")
cat("STAGE 2 SUMMARY: 2001-2010 LTL Consumption (tonnes/year)\n")
cat("=============================================================\n\n")

for (scen in c("fishing", "climate_only")) {
  scen_lbl <- ifelse(scen == "fishing", "FISHED", "UNFISHED (CLIMATE-ONLY)")
  cat(sprintf("  %s\n", scen_lbl))
  cat(sprintf("  %-35s  %12s  %25s\n", "Predator", "Median t/yr", "IQR [q25 – q75]"))
  cat(sprintf("  %s\n", strrep("-", 78)))

  sub <- stage2_df %>% filter(scenario == scen) %>%
    arrange(match(predator, display_order))

  for (j in seq_len(nrow(sub))) {
    cat(sprintf("  %-35s  %12s  [%10s – %10s]\n",
                as.character(sub$nice_label[j]),
                scales::comma(round(sub$median_tonnes[j])),
                scales::comma(round(sub$q25_tonnes[j])),
                scales::comma(round(sub$q75_tonnes[j]))))
  }
  cat("\n")
}

cat("Fishing impact (median fished as % of unfished):\n")
cat(sprintf("  %-35s  %8s  %20s\n", "Predator", "% unfished", "IQR [q25 – q75]"))
cat(sprintf("  %s\n", strrep("-", 68)))
imp <- impact_summary %>% arrange(match(predator, display_order))
for (j in seq_len(nrow(imp))) {
  cat(sprintf("  %-35s  %7.1f%%  [%6.1f%% – %6.1f%%]\n",
              as.character(imp$nice_label[j]),
              imp$median[j], imp$q25[j], imp$q75[j]))
}

cat("\nFigures saved to:", CACHED_DIR, "\n")
cat("Done.\n")
