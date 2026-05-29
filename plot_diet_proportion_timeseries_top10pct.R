###############################################################################
# plot_diet_proportion_timeseries_top10pct.R
#
# Stacked area chart of diet composition through time for each of the 19
# predators, using the top-10% RMSE fishing ensemble (212 sims, 1901-2010).
#
# Prey are collapsed into 8 functional groups for readability.
# Proportions are computed per-sim per-year, then ensemble median taken.
#
# Outputs (whale_consumption_outputs/):
#   diet_proportion_timeseries_top10pct.png   (4 x 5 panel grid)
#   diet_proportion_timeseries_top10pct.pdf
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

OUTPUT_DIR <- "whale_consumption_outputs"
YEAR_MIN   <- 1901   # trim spin-up years

###############################################################################
# Functional prey groups
###############################################################################
PREY_GROUPS <- list(
  "Antarctic krill" = "antarctic krill",
  "Other LTL"       = c("mesozooplankton", "other krill",
                        "other macrozooplankton", "salps"),
  "Fishes"          = c("mesopelagic fishes", "bathypelagic fishes",
                        "shelf and coastal fishes", "toothfishes"),
  "Squids"          = "squids",
  "Seabirds"        = c("flying birds", "small divers"),
  "Pinnipeds"       = c("leopard seals", "medium divers", "large divers"),
  "Cetaceans"       = c("minke whales", "orca", "sperm whales", "baleen whales"),
  "Plankton resource" = "Resource"
  # "External" deliberately excluded (external subsidies, usually near-zero)
)

PREY_GROUP_COLS <- c(
  "Antarctic krill"   = "#e8534a",
  "Other LTL"         = "#6dbf6b",
  "Fishes"            = "#1a6faf",
  "Squids"            = "#e87c10",
  "Seabirds"          = "#9e9e9e",
  "Pinnipeds"         = "#a0522d",
  "Cetaceans"         = "#9467bd",
  "Plankton resource" = "#c8e6a0"
)

PREY_GROUP_ORDER <- rev(names(PREY_GROUP_COLS))   # bottom of stack = first

###############################################################################
# Ordered display names for predator panels
###############################################################################
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
# Load per-sim diet arrays and compute per-sim proportions
###############################################################################
cat("Loading top-10% diet arrays...\n")
fish_arrays <- readRDS(file.path(OUTPUT_DIR,
                       "full_diet_top10pct_fishing_all_sims.rds"))
n_sims <- length(fish_arrays)
cat(sprintf("  %d sims loaded\n", n_sims))

# Build prey → group lookup from the actual prey names in the arrays
all_prey <- dimnames(fish_arrays[[1]])$prey
all_preds <- dimnames(fish_arrays[[1]])$predator

prey_to_group <- rep(NA_character_, length(all_prey))
names(prey_to_group) <- all_prey
for (grp in names(PREY_GROUPS)) {
  matched <- intersect(PREY_GROUPS[[grp]], all_prey)
  prey_to_group[matched] <- grp
}

cat(sprintf("  Prey mapped: %d / %d have a group assignment\n",
            sum(!is.na(prey_to_group)), length(all_prey)))
unmapped <- all_prey[is.na(prey_to_group)]
if (length(unmapped)) cat(sprintf("  Unmapped prey (excluded): %s\n",
                                   paste(unmapped, collapse = ", ")))

# Keep only prey with a group
prey_keep  <- all_prey[!is.na(prey_to_group)]
group_keep <- prey_to_group[prey_keep]

###############################################################################
# Compute per-sim per-year diet proportions, then take ensemble median
###############################################################################
cat("Computing diet proportions (per sim per year)...\n")

# Pre-allocate: for each sim, we'll create a long df of
# (year, predator, prey_group, proportion)
all_rows <- vector("list", n_sims)

for (i in seq_len(n_sims)) {
  arr    <- fish_arrays[[i]]    # [year × pred × prey]
  years  <- as.numeric(dimnames(arr)$year)
  yr_idx <- years >= YEAR_MIN

  # Subset to kept prey
  sub <- arr[yr_idx, , prey_keep, drop = FALSE]  # [yr × pred × prey_keep]

  # Sum within prey groups: [yr × pred × group]
  groups  <- unique(group_keep)
  grp_arr <- array(0, dim = c(sum(yr_idx), length(all_preds), length(groups)),
                   dimnames = list(year = years[yr_idx],
                                   predator = all_preds,
                                   prey_group = groups))

  for (grp in groups) {
    prey_in_grp <- prey_keep[group_keep == grp]
    if (length(prey_in_grp) == 1) {
      grp_arr[, , grp] <- sub[, , prey_in_grp]
    } else {
      grp_arr[, , grp] <- apply(sub[, , prey_in_grp, drop = FALSE], c(1, 2), sum)
    }
  }

  # Convert to proportions: divide by total per year × predator
  totals <- apply(grp_arr, c(1, 2), sum)  # [yr × pred]
  prop_arr <- sweep(grp_arr, c(1, 2), totals, "/")
  prop_arr[!is.finite(prop_arr)] <- 0

  # Melt to long
  rows_list <- vector("list", length(groups))
  for (g in seq_along(groups)) {
    mat    <- prop_arr[, , g]  # [yr × pred]
    yr_vec <- as.numeric(rownames(mat))
    for (pred in all_preds) {
      rows_list[[g]] <- rbind(rows_list[[g]],
        data.frame(year       = yr_vec,
                   predator   = pred,
                   prey_group = groups[g],
                   proportion = mat[, pred],
                   stringsAsFactors = FALSE))
    }
  }
  all_rows[[i]] <- do.call(rbind, rows_list)
  if (i %% 50 == 0) cat(sprintf("  Sim %d / %d\n", i, n_sims))
}

cat("  Combining and computing medians...\n")
all_df <- do.call(rbind, all_rows)

summary_df <- all_df %>%
  group_by(year, predator, prey_group) %>%
  summarise(med_prop = median(proportion, na.rm = TRUE), .groups = "drop")

# Renormalise medians so they sum to 1 per year×predator
summary_df <- summary_df %>%
  group_by(year, predator) %>%
  mutate(med_prop = med_prop / sum(med_prop, na.rm = TRUE)) %>%
  ungroup()

summary_df$prey_group <- factor(summary_df$prey_group, levels = PREY_GROUP_ORDER)
summary_df$predator_label <- PRED_DISPLAY[summary_df$predator]
summary_df$predator_label <- factor(summary_df$predator_label,
                                     levels = PRED_DISPLAY)

###############################################################################
# Plot
###############################################################################
cat("Generating plot...\n")

p <- ggplot(summary_df,
            aes(x = year, y = med_prop, fill = prey_group)) +
  geom_area(position = "stack", colour = NA) +
  facet_wrap(~predator_label, ncol = 4) +
  scale_fill_manual(values = PREY_GROUP_COLS, name = "Prey group") +
  scale_x_continuous(
    breaks = c(1920, 1960, 2000),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  theme_bw(base_size = 10) +
  theme(
    strip.text       = element_text(face = "bold", size = 8),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y      = element_text(size = 7),
    legend.position  = "bottom",
    legend.key.size  = unit(0.4, "cm"),
    legend.text      = element_text(size = 8),
    legend.title     = element_text(size = 9, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(x = "Year", y = "Diet proportion (median across ensemble)")

png_path <- file.path(OUTPUT_DIR, "diet_proportion_timeseries_top10pct.png")
pdf_path <- file.path(OUTPUT_DIR, "diet_proportion_timeseries_top10pct.pdf")

ggsave(png_path, p, width = 14, height = 18, dpi = 300)
ggsave(pdf_path, p, width = 14, height = 18)

cat(sprintf("Saved: %s\n", png_path))
cat(sprintf("Saved: %s\n", pdf_path))
cat("Done.\n")
