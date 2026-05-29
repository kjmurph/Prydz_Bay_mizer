###############################################################################
# Plot: Antarctic krill and LTL prey consumption comparison
# Exploited vs. Unexploited, 2001-2010 mean
#
# Three-panel figure:
#   a  Antarctic krill consumed by ALL predators (dot-range plot, log10 scale)
#   b  Antarctic krill consumed by baleen + minke whales
#   c  Total LTL prey consumed by baleen + minke whales
#
# Output: whale_consumption_outputs/krill_ltl_consumption_comparison.png/pdf
###############################################################################

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(patchwork)
})

OUTPUT_DIR <- "whale_consumption_outputs"
G_TO_KT    <- 1e-9   # g yr-1 -> kt yr-1
G_TO_MT    <- 1e-12  # g yr-1 -> Mt yr-1
YEAR_START <- 2001
YEAR_END   <- 2010

SCN_COLS   <- c("Exploited" = "#d73027", "Unexploited" = "#4575b4")
SCN_SHAPES <- c("Exploited" = 19,        "Unexploited" = 1)
SCN_LEVELS <- c("Exploited", "Unexploited")

###############################################################################
# Helper: compute 2001-2010 period mean per simulation from a per-sim RDS list
###############################################################################
period_mean_per_sim <- function(rds_path, y1 = YEAR_START, y2 = YEAR_END) {
  dat <- readRDS(rds_path)
  vapply(dat, function(df) {
    sub <- df[df$year >= y1 & df$year <= y2, ]
    mean(sub$total_consumption, na.rm = TRUE)
  }, numeric(1))
}

###############################################################################
# PANEL A: All predators × Antarctic krill
###############################################################################
cat("Building panel a (all predators, Antarctic krill)...\n")

fish_sum <- read.csv(file.path(OUTPUT_DIR, "full_diet_contemporary_fishing_summary.csv"),
                     stringsAsFactors = FALSE)
clim_sum <- read.csv(file.path(OUTPUT_DIR, "full_diet_contemporary_climate_only_summary.csv"),
                     stringsAsFactors = FALSE)

# Functional group lookup
FUNC_GROUP <- c(
  "shelf and coastal fishes" = "Fish",
  "mesopelagic fishes"       = "Fish",
  "bathypelagic fishes"      = "Fish",
  "toothfishes"              = "Fish",
  "squids"                   = "Squid",
  "flying birds"             = "Seabirds",
  "small divers"             = "Pinnipeds",
  "medium divers"            = "Pinnipeds",
  "large divers"             = "Pinnipeds",
  "leopard seals"            = "Pinnipeds",
  "minke whales"             = "Baleen whales",
  "baleen whales"            = "Baleen whales",
  "sperm whales"             = "Toothed whales",
  "orca"                     = "Toothed whales",
  "other macrozooplankton"   = "LTL zooplankton"
)

# Exclude: self-predation and ecologically trivial LTL self-interactions
EXCL_PRED <- c("antarctic krill", "other krill", "salps", "mesozooplankton")

prep_krill <- function(df, scenario) {
  d <- df[df$prey == "antarctic krill" & !df$predator %in% EXCL_PRED, ]
  d$func_group <- FUNC_GROUP[d$predator]
  d <- d[!is.na(d$func_group), ]
  data.frame(
    predator   = d$predator,
    func_group = d$func_group,
    median_Mt  = d$median_g_yr * G_TO_MT,
    q25_Mt     = d$q25_g_yr    * G_TO_MT,
    q75_Mt     = d$q75_g_yr    * G_TO_MT,
    scenario   = scenario,
    stringsAsFactors = FALSE
  )
}

kA_fish <- prep_krill(fish_sum, "Exploited")
kA_clim <- prep_krill(clim_sum, "Unexploited")
kA      <- rbind(kA_fish, kA_clim)

# Drop predators where both scenarios have negligible/zero consumption
MIN_MT <- 1e-8
keep_pred <- kA %>%
  group_by(predator) %>%
  summarise(any_nonzero = any(median_Mt > MIN_MT), .groups = "drop") %>%
  filter(any_nonzero) %>%
  pull(predator)

kA <- kA[kA$predator %in% keep_pred, ]

# Order by unexploited median (ascending, so largest is at top of horizontal plot)
pred_order <- kA_clim[kA_clim$predator %in% keep_pred, ]
pred_order <- pred_order[order(pred_order$median_Mt), "predator"]
kA$predator_f <- factor(kA$predator,
                         levels = pred_order,
                         labels = tools::toTitleCase(pred_order))
kA$scenario   <- factor(kA$scenario, levels = SCN_LEVELS)

GROUP_COLS <- c(
  "Fish"            = "#0072B2",
  "Squid"           = "#CC79A7",
  "Seabirds"        = "#E69F00",
  "Pinnipeds"       = "#D55E00",
  "Baleen whales"   = "#e878b8",
  "Toothed whales"  = "#56B4E9",
  "LTL zooplankton" = "#999999"
)

# Map functional group to y-axis label colour via a lookup vector
grp_order  <- kA_clim[kA_clim$predator %in% keep_pred,
                       c("predator", "func_group")]
grp_order  <- grp_order[order(match(grp_order$predator, pred_order)), ]
axis_cols  <- GROUP_COLS[grp_order$func_group]

pA <- ggplot(kA,
             aes(y = predator_f, x = median_Mt,
                 colour = scenario, shape = scenario)) +
  geom_linerange(aes(xmin = q25_Mt, xmax = q75_Mt),
                 position = position_dodge(width = 0.55),
                 linewidth = 0.55, alpha = 0.75) +
  geom_point(position = position_dodge(width = 0.55), size = 2.8,
             stroke = 1.1) +
  scale_x_log10(
    name   = "Antarctic krill consumption (Mt yr-1)",
    breaks = 10^seq(-5, 1),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_colour_manual(values = SCN_COLS,   name = "Scenario") +
  scale_shape_manual( values = SCN_SHAPES, name = "Scenario") +
  labs(y = NULL,
       title = "a   Antarctic krill consumption by all predators, 2001-2010") +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(size = 9, colour = axis_cols),
    legend.position    = "top",
    legend.key.size    = unit(0.5, "cm"),
    legend.text        = element_text(size = 9),
    legend.box         = "horizontal",
    plot.title         = element_text(size = 10, face = "bold"),
    plot.margin        = margin(6, 6, 4, 4, "pt")
  )

# Add a minimal functional-group key as a text annotation caption
grp_key <- paste(
  sapply(names(GROUP_COLS)[names(GROUP_COLS) %in% unique(kA$func_group)], function(g) {
    sprintf('<span style="color:%s">\u25a0 %s</span>', GROUP_COLS[g], g)
  }),
  collapse = "  "
)
# Note: ggtext would render HTML; use plain annotation instead
func_legend_df <- data.frame(
  x = 1e-5,
  y = seq_along(names(GROUP_COLS)[names(GROUP_COLS) %in% unique(kA$func_group)]),
  label = names(GROUP_COLS)[names(GROUP_COLS) %in% unique(kA$func_group)],
  col   = GROUP_COLS[names(GROUP_COLS) %in% unique(kA$func_group)]
)

###############################################################################
# PANEL B: Baleen + minke whales × Antarctic krill
###############################################################################
cat("Building panel b (baleen whales, Antarctic krill)...\n")

bk_fish <- period_mean_per_sim(file.path(OUTPUT_DIR, "fishing_baleen_krill_all_sims.rds"))
bk_clim <- period_mean_per_sim(file.path(OUTPUT_DIR, "climate_only_baleen_krill_all_sims.rds"))

bk_df <- data.frame(
  scenario = factor(c(rep("Exploited",   length(bk_fish)),
                      rep("Unexploited", length(bk_clim))),
                    levels = SCN_LEVELS),
  value_kt = c(bk_fish, bk_clim) * G_TO_KT
)

bk_sum <- bk_df %>%
  group_by(scenario) %>%
  summarise(med = median(value_kt, na.rm = TRUE),
            q25 = quantile(value_kt, 0.25, na.rm = TRUE),
            q75 = quantile(value_kt, 0.75, na.rm = TRUE),
            .groups = "drop")

pB <- ggplot(bk_sum, aes(x = scenario, y = med, fill = scenario)) +
  geom_col(width = 0.5, alpha = 0.88, colour = "grey30", linewidth = 0.3) +
  geom_errorbar(aes(ymin = q25, ymax = q75),
                width = 0.15, linewidth = 0.75, colour = "grey20") +
  scale_fill_manual(values = SCN_COLS, guide = "none") +
  scale_y_continuous(
    name   = "Krill consumption (kt yr-1)",
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(x = NULL,
       title = "b   Baleen + minke whale\nAntarctic krill consumption") +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 9),
    plot.title         = element_text(size = 10, face = "bold"),
    plot.margin        = margin(6, 8, 4, 4, "pt")
  )

###############################################################################
# PANEL C: Baleen + minke whales × total LTL prey
###############################################################################
cat("Building panel c (baleen whales, total LTL prey)...\n")

bl_fish <- period_mean_per_sim(file.path(OUTPUT_DIR, "fishing_baleen_ltl_all_sims.rds"))
bl_clim <- period_mean_per_sim(file.path(OUTPUT_DIR, "climate_only_baleen_ltl_all_sims.rds"))

bl_df <- data.frame(
  scenario = factor(c(rep("Exploited",   length(bl_fish)),
                      rep("Unexploited", length(bl_clim))),
                    levels = SCN_LEVELS),
  value_kt = c(bl_fish, bl_clim) * G_TO_KT
)

bl_sum <- bl_df %>%
  group_by(scenario) %>%
  summarise(med = median(value_kt, na.rm = TRUE),
            q25 = quantile(value_kt, 0.25, na.rm = TRUE),
            q75 = quantile(value_kt, 0.75, na.rm = TRUE),
            .groups = "drop")

pC <- ggplot(bl_sum, aes(x = scenario, y = med, fill = scenario)) +
  geom_col(width = 0.5, alpha = 0.88, colour = "grey30", linewidth = 0.3) +
  geom_errorbar(aes(ymin = q25, ymax = q75),
                width = 0.15, linewidth = 0.75, colour = "grey20") +
  scale_fill_manual(values = SCN_COLS, guide = "none") +
  scale_y_continuous(
    name   = "LTL prey consumption (kt yr-1)",
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(x = NULL,
       title = "c   Baleen + minke whale\ntotal LTL prey consumption") +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 9),
    plot.title         = element_text(size = 10, face = "bold"),
    plot.margin        = margin(6, 8, 4, 4, "pt")
  )

###############################################################################
# Combine panels and save
###############################################################################
cat("Combining panels...\n")

# Add a functional group colour key as a caption below panel a
grp_present  <- names(GROUP_COLS)[names(GROUP_COLS) %in% unique(kA$func_group)]
grp_key_text <- paste(grp_present, collapse = "   ")

fig <- pA / (pB | pC) +
  plot_layout(heights = c(1.7, 1))

# Functional group caption (drawn separately, same width as fig)
func_cap_df <- data.frame(
  label = grp_present,
  col   = GROUP_COLS[grp_present],
  x     = seq_along(grp_present),
  y     = 1
)
func_cap <- ggplot(func_cap_df, aes(x = x, y = y)) +
  geom_point(aes(colour = label), shape = 15, size = 5) +
  geom_text(aes(label = label, colour = label), hjust = -0.15, size = 3.2) +
  scale_colour_manual(values = setNames(func_cap_df$col, func_cap_df$label),
                      guide = "none") +
  scale_x_continuous(limits = c(0.5, max(func_cap_df$x) + 1.5)) +
  theme_void() +
  theme(plot.margin = margin(2, 4, 2, 4, "pt"))

fig <- func_cap / pA / (pB | pC) +
  plot_layout(heights = c(0.12, 1.7, 1))

png_path <- file.path(OUTPUT_DIR, "krill_ltl_consumption_comparison.png")
pdf_path <- file.path(OUTPUT_DIR, "krill_ltl_consumption_comparison.pdf")

ggsave(png_path, fig, width = 9, height = 10, dpi = 300)
ggsave(pdf_path, fig, width = 9, height = 10)

cat(sprintf("\nSaved: %s\n", png_path))
cat(sprintf("Saved: %s\n", pdf_path))

# Print summary values for reference
cat("\n--- Panel b summary (kt yr-1) ---\n")
print(as.data.frame(bk_sum))
cat("\n--- Panel c summary (kt yr-1) ---\n")
print(as.data.frame(bl_sum))

cat("\nDone.\n")
