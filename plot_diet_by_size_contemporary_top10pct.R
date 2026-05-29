###############################################################################
# plot_diet_by_size_contemporary_top10pct.R
#
# Contemporary (2001-2010) diet proportion by predator body size for each of
# the 19 functional groups, using the best-RMSE fishing sim (rank 1).
#
# Uses therMizer's getDiet(proportion=TRUE) at each year 2001-2010 and
# averages across those 10 years. Produces one panel per predator arranged
# in a 4 x 5 grid.
#
# Outputs (whale_consumption_outputs/):
#   diet_by_size_contemporary_top10pct.png
#   diet_by_size_contemporary_top10pct.pdf
###############################################################################

suppressPackageStartupMessages({
  library(therMizer)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

OUTPUT_DIR <- "whale_consumption_outputs"
YEAR_START <- 2001
YEAR_END   <- 2010

###############################################################################
# Prey functional groups and colours (same as timeseries plot)
###############################################################################
PREY_GROUPS <- list(
  "Antarctic krill"   = "antarctic krill",
  "Other LTL"         = c("mesozooplankton", "other krill",
                          "other macrozooplankton", "salps"),
  "Fishes"            = c("mesopelagic fishes", "bathypelagic fishes",
                          "shelf and coastal fishes", "toothfishes"),
  "Squids"            = "squids",
  "Seabirds"          = c("flying birds", "small divers"),
  "Pinnipeds"         = c("leopard seals", "medium divers", "large divers"),
  "Cetaceans"         = c("minke whales", "orca", "sperm whales", "baleen whales"),
  "Plankton resource" = "Resource"
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

PREY_GROUP_ORDER <- rev(names(PREY_GROUP_COLS))

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
# Load RMSE ranking and best sim
###############################################################################
cat("Loading RMSE rankings...\n")
rmse_df <- read.csv("yield_rmse_per_sim.csv")
rmse_df <- rmse_df[order(rmse_df$rank), ]
best_idx <- rmse_df$sim_index[1]
cat(sprintf("  Best sim: index=%d, RMSE=%.4f\n", best_idx, rmse_df$rmse[1]))

cat("Loading MC fishing ensemble (this may take a moment)...\n")
mc_path <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
mc      <- readRDS(mc_path)
sims    <- mc$simulations
best_sim <- sims[[best_idx]]
rm(mc, sims); gc()
cat("  Best sim loaded.\n")

###############################################################################
# Extract diet proportions averaged over 2001-2010
###############################################################################
cat("Extracting diet by size (2001-2010)...\n")

times   <- as.numeric(dimnames(best_sim@n)$time)
t_idx_v <- which(times >= YEAR_START & times <= YEAR_END)
cat(sprintf("  %d time steps in period\n", length(t_idx_v)))

params <- best_sim@params

diet_list <- lapply(t_idx_v, function(ti) {
  n_slice <- best_sim@n[ti, , , drop = FALSE]
  dim(n_slice)      <- dim(best_sim@n)[2:3]
  dimnames(n_slice) <- dimnames(best_sim@n)[2:3]
  n_pp    <- best_sim@n_pp[ti, ]
  n_other <- best_sim@n_other[ti, ]
  if (!is.null(dimnames(best_sim@n_other)))
    names(n_other) <- dimnames(best_sim@n_other)$component

  tryCatch(
    getDiet(params, n = n_slice, n_pp = n_pp, n_other = n_other,
            proportion = TRUE),
    error = function(e) NULL
  )
})
diet_list <- Filter(Negate(is.null), diet_list)
cat(sprintf("  %d time steps successfully extracted\n", length(diet_list)))

# Average proportion arrays across years
diet_mean <- Reduce("+", diet_list) / length(diet_list)
# diet_mean: [predator × size × prey]

###############################################################################
# Build long-format data collapsed to functional prey groups
###############################################################################
preds      <- dimnames(diet_mean)$predator
sizes      <- as.numeric(dimnames(diet_mean)[[2]])
prey_names <- dimnames(diet_mean)[[3]]

# Map prey to groups
prey_to_group <- rep(NA_character_, length(prey_names))
names(prey_to_group) <- prey_names
for (grp in names(PREY_GROUPS)) {
  matched <- intersect(PREY_GROUPS[[grp]], prey_names)
  prey_to_group[matched] <- grp
}
prey_keep  <- prey_names[!is.na(prey_to_group)]
group_keep <- prey_to_group[prey_keep]

# Build long data.frame
rows <- do.call(rbind, lapply(preds, function(pred) {
  # Accumulate by functional group: sum proportions over constituent prey,
  # for each size class
  grp_mat <- matrix(0, nrow = length(sizes), ncol = length(unique(group_keep)),
                    dimnames = list(size = sizes,
                                    prey_group = unique(group_keep)))
  for (prey in prey_keep) {
    grp <- prey_to_group[prey]
    grp_mat[, grp] <- grp_mat[, grp] + diet_mean[pred, , prey]
  }

  # Renormalise each size class so proportions sum to 1
  row_tots <- rowSums(grp_mat)
  grp_mat  <- sweep(grp_mat, 1, pmax(row_tots, 1e-30), "/")

  # Long format
  data.frame(
    predator   = pred,
    size_g     = rep(sizes, length(unique(group_keep))),
    prey_group = rep(unique(group_keep), each = length(sizes)),
    proportion = as.vector(grp_mat),
    stringsAsFactors = FALSE
  )
}))

rows$predator_label <- PRED_DISPLAY[rows$predator]
rows$predator_label <- factor(rows$predator_label, levels = PRED_DISPLAY)
rows$prey_group     <- factor(rows$prey_group, levels = PREY_GROUP_ORDER)

###############################################################################
# Plot — one panel per predator, x = log10(body size in g)
###############################################################################
cat("Generating plot...\n")

p <- ggplot(rows,
            aes(x = log10(size_g), y = proportion, fill = prey_group)) +
  geom_area(position = "stack", colour = NA) +
  facet_wrap(~predator_label, ncol = 4, scales = "free_x") +
  scale_fill_manual(values = PREY_GROUP_COLS, name = "Prey group") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_continuous(
    name   = "Predator body size (log10 g)",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  theme_bw(base_size = 10) +
  theme(
    strip.text       = element_text(face = "bold", size = 8),
    axis.text.x      = element_text(size = 7),
    axis.text.y      = element_text(size = 7),
    legend.position  = "bottom",
    legend.key.size  = unit(0.4, "cm"),
    legend.text      = element_text(size = 8),
    legend.title     = element_text(size = 9, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    y        = "Diet proportion",
    subtitle = sprintf("Contemporary diet by predator body size (best RMSE sim, %d-%d average)",
                       YEAR_START, YEAR_END)
  )

png_path <- file.path(OUTPUT_DIR, "diet_by_size_contemporary_top10pct.png")
pdf_path <- file.path(OUTPUT_DIR, "diet_by_size_contemporary_top10pct.pdf")

ggsave(png_path, p, width = 14, height = 18, dpi = 300)
ggsave(pdf_path, p, width = 14, height = 18)

cat(sprintf("Saved: %s\n", png_path))
cat(sprintf("Saved: %s\n", pdf_path))
cat("Done.\n")
