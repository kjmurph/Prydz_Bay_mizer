###############################################################################
# summarise_krill_consumption_top10pct.R
#
# Total Antarctic krill consumption (g yr-1, domain-wide) by each predator
# for the contemporary period (2001-2010), across both the fishing and
# matched climate-only top-10% RMSE ensembles (212 sims each).
#
# Reports:
#   1. Per-predator summary table (CSV)
#   2. Grouped bar chart comparing exploited vs unexploited
#
# Outputs (whale_consumption_outputs/):
#   krill_consumption_by_predator_top10pct.csv
#   krill_consumption_by_predator_top10pct.png
#   krill_consumption_by_predator_top10pct.pdf
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

OUTPUT_DIR <- "whale_consumption_outputs"
YEAR_START <- 2001
YEAR_END   <- 2010
G_TO_MT    <- 1e-12
G_TO_KT    <- 1e-9

PRED_DISPLAY <- c(
  "mesozooplankton"          = "Mesozooplankton",
  "other krill"              = "Other krill",
  "other macrozooplankton"   = "Other macrozooplankton",
  "antarctic krill"          = "Antarctic krill",
  "salps"                    = "Salps",
  "mesopelagic fishes"       = "Mesopelagic fishes",
  "bathypelagic fishes"      = "Bathypelagic fishes",
  "shelf and coastal fishes" = "Shelf & coastal fishes",
  "flying birds"             = "Flying birds",
  "small divers"             = "Small divers",
  "squids"                   = "Squids",
  "toothfishes"              = "Toothfishes",
  "leopard seals"            = "Leopard seals",
  "medium divers"            = "Medium divers",
  "large divers"             = "Large divers",
  "minke whales"             = "Minke whales",
  "orca"                     = "Orca",
  "sperm whales"             = "Sperm whales",
  "baleen whales"            = "Large baleen whales"
)

###############################################################################
# Functional groups for grouped bar chart
###############################################################################
FUNC_GROUP <- c(
  "mesozooplankton"          = "Zooplankton",
  "other krill"              = "Zooplankton",
  "other macrozooplankton"   = "Zooplankton",
  "antarctic krill"          = "Krill",
  "salps"                    = "Zooplankton",
  "mesopelagic fishes"       = "Fishes",
  "bathypelagic fishes"      = "Fishes",
  "shelf and coastal fishes" = "Fishes",
  "toothfishes"              = "Fishes",
  "flying birds"             = "Seabirds",
  "small divers"             = "Seabirds",
  "squids"                   = "Squids",
  "leopard seals"            = "Pinnipeds",
  "medium divers"            = "Pinnipeds",
  "large divers"             = "Pinnipeds",
  "minke whales"             = "Cetaceans",
  "orca"                     = "Cetaceans",
  "sperm whales"             = "Cetaceans",
  "baleen whales"            = "Cetaceans"
)

GROUP_COLS <- c(
  "Krill"       = "#e8534a",
  "Zooplankton" = "#6dbf6b",
  "Fishes"      = "#1a6faf",
  "Squids"      = "#e87c10",
  "Seabirds"    = "#9e9e9e",
  "Pinnipeds"   = "#a0522d",
  "Cetaceans"   = "#9467bd"
)

###############################################################################
# Load arrays and extract 2001-2010 mean per predator per sim
###############################################################################
cat("Loading top-10% diet arrays...\n")
fish_arrays <- readRDS(file.path(OUTPUT_DIR,
                       "full_diet_top10pct_fishing_all_sims.rds"))
clim_arrays <- readRDS(file.path(OUTPUT_DIR,
                       "full_diet_top10pct_climate_only_all_sims.rds"))
n_sims <- length(fish_arrays)
cat(sprintf("  %d sims per scenario\n\n", n_sims))

extract_contemporary_krill <- function(arr_list, scenario_label) {
  cat(sprintf("Extracting contemporary krill consumption (%s)...\n", scenario_label))
  preds <- dimnames(arr_list[[1]])$predator

  rows <- lapply(seq_len(n_sims), function(i) {
    arr   <- arr_list[[i]]
    years <- as.numeric(dimnames(arr)$year)
    t_idx <- years >= YEAR_START & years <= YEAR_END

    # For each predator: mean annual krill consumption across contemporary years
    krill_slice <- arr[t_idx, , "antarctic krill", drop = FALSE]
    means <- colMeans(krill_slice[, , 1, drop = TRUE])   # [pred]
    data.frame(predator = preds, mean_g_yr = as.numeric(means),
               sim = i, scenario = scenario_label,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

fish_raw <- extract_contemporary_krill(fish_arrays, "Exploited")
clim_raw <- extract_contemporary_krill(clim_arrays, "Unexploited")

all_raw <- bind_rows(fish_raw, clim_raw)

###############################################################################
# Ensemble summary per predator
###############################################################################
cat("\nSummarising across ensemble...\n")

summary_df <- all_raw %>%
  group_by(predator, scenario) %>%
  summarise(
    median_g_yr = median(mean_g_yr, na.rm = TRUE),
    q25_g_yr    = quantile(mean_g_yr, 0.25, na.rm = TRUE),
    q75_g_yr    = quantile(mean_g_yr, 0.75, na.rm = TRUE),
    mean_g_yr   = mean(mean_g_yr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    median_kt = median_g_yr * G_TO_KT,
    q25_kt    = q25_g_yr    * G_TO_KT,
    q75_kt    = q75_g_yr    * G_TO_KT,
    median_mt = median_g_yr * G_TO_MT,
    predator_label = PRED_DISPLAY[predator],
    func_group     = FUNC_GROUP[predator]
  ) %>%
  arrange(scenario, desc(median_kt))

###############################################################################
# Print results table
###############################################################################
cat("\n===== Contemporary Antarctic krill consumption (2001-2010 mean) =====\n")
cat("Units: kt yr-1 (thousand tonnes per year)\n\n")

for (scen in c("Exploited", "Unexploited")) {
  cat(sprintf("--- %s scenario ---\n", scen))
  sub <- summary_df[summary_df$scenario == scen, ]
  sub <- sub[order(-sub$median_kt), ]
  for (r in seq_len(nrow(sub))) {
    cat(sprintf("  %-30s  %8.3f kt/yr  [IQR: %.3f – %.3f]\n",
                sub$predator_label[r], sub$median_kt[r],
                sub$q25_kt[r], sub$q75_kt[r]))
  }
  tot <- summary_df[summary_df$scenario == scen, ] %>%
    group_by(func_group) %>%
    summarise(grp_kt = sum(median_kt), .groups = "drop") %>%
    arrange(-grp_kt)
  cat(sprintf("  Total: %.3f kt/yr\n\n", sum(sub$median_kt)))
}

###############################################################################
# Save CSV
###############################################################################
csv_path <- file.path(OUTPUT_DIR, "krill_consumption_by_predator_top10pct.csv")
write.csv(summary_df, csv_path, row.names = FALSE)
cat(sprintf("Saved: %s\n\n", csv_path))

###############################################################################
# Grouped bar chart
###############################################################################
cat("Generating bar chart...\n")

# Order predators by Exploited median (descending) for the y-axis
pred_order <- summary_df %>%
  filter(scenario == "Exploited") %>%
  arrange(median_kt) %>%
  pull(predator_label)

summary_df$predator_label <- factor(summary_df$predator_label, levels = pred_order)
summary_df$scenario       <- factor(summary_df$scenario,
                                     levels = c("Unexploited", "Exploited"))

# Use log scale — range spans many orders of magnitude
p <- ggplot(summary_df,
            aes(y = predator_label, x = median_kt,
                xmin = q25_kt, xmax = q75_kt,
                colour = func_group, fill = func_group,
                shape = scenario)) +
  geom_pointrange(
    aes(shape = scenario),
    position = position_dodge(width = 0.6),
    size = 0.55, linewidth = 0.55
  ) +
  scale_shape_manual(values = c("Unexploited" = 21, "Exploited" = 19),
                     name = "Scenario") +
  scale_colour_manual(values = GROUP_COLS, name = "Functional group") +
  scale_fill_manual(values   = GROUP_COLS, name = "Functional group") +
  scale_x_log10(
    name   = "Antarctic krill consumption (kt yr-1, log scale)",
    labels = scales::comma
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "right",
    axis.title.y       = element_blank()
  ) +
  labs(subtitle = sprintf("Contemporary period %d-%d, top-10%% RMSE ensemble (n=%d sims)",
                          YEAR_START, YEAR_END, n_sims))

png_path <- file.path(OUTPUT_DIR, "krill_consumption_by_predator_top10pct.png")
pdf_path <- file.path(OUTPUT_DIR, "krill_consumption_by_predator_top10pct.pdf")

ggsave(png_path, p, width = 10, height = 7, dpi = 300)
ggsave(pdf_path, p, width = 10, height = 7)

cat(sprintf("Saved: %s\n", png_path))
cat(sprintf("Saved: %s\n", pdf_path))
cat("Done.\n")
