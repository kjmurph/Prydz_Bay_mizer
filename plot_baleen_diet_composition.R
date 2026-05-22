###############################################################################
# Baleen Whale Diet Composition – Publication Figures
#
# Reads from cached statistics (baleen_diet_proportions_fishing_stats.csv)
# and produces Nature-portfolio-formatted figures.
#
# Nature spec applied:
#   - Sans-serif font (Arial via "sans"), base 7 pt, max 7 pt text
#   - Wong (2011) colour-blind-safe palette
#   - No titles/subtitles (descriptions belong in figure legend)
#   - 180 mm double-column width (7.09 in)
###############################################################################

library(dplyr)
library(ggplot2)
library(scales)

OUTPUT_DIR <- "whale_consumption_outputs"

# ── Wong (2011) colour-blind-safe palette (recommended by Nature) ─────────────
WONG <- c(
  vermilion = "#D55E00",
  blue      = "#0072B2",
  sky_blue  = "#56B4E9",
  green     = "#009E73",
  orange    = "#E69F00",
  pink      = "#CC79A7",
  yellow    = "#F0E442",
  black     = "#000000"
)

# ── Nature portfolio theme ────────────────────────────────────────────────────
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

# ── load cached data ──────────────────────────────────────────────────────────
cat("Loading cached diet statistics...\n")
stats <- read.csv(file.path(OUTPUT_DIR, "baleen_diet_proportions_fishing_stats.csv"),
                  stringsAsFactors = FALSE)
cat(sprintf("  %d rows, years %d-%d, %d prey categories\n",
            nrow(stats), min(stats$year), max(stats$year), length(unique(stats$prey))))

# ── identify major prey groups (>1% median in any year) ──────────────────────
major_prey <- stats %>%
  group_by(prey) %>%
  summarise(max_median = max(median), .groups = "drop") %>%
  filter(max_median > 0.01) %>%
  pull(prey)

cat(sprintf("Major prey groups (>1%% in any year): %s\n",
            paste(sort(major_prey), collapse = ", ")))

# ── colour + label mapping ────────────────────────────────────────────────────
# Ordered by overall importance (Resource first = bottom of stack)
prey_importance <- stats %>%
  filter(prey %in% major_prey) %>%
  group_by(prey) %>%
  summarise(avg = mean(median), .groups = "drop") %>%
  arrange(desc(avg)) %>%
  pull(prey)

# Put Resource first (largest, sits at bottom of stack)
prey_levels <- c("Resource", prey_importance[prey_importance != "Resource"], "Other")

# Assign Wong colours: Resource = vermilion, rest in order
n_biol <- length(prey_levels) - 1  # excluding "Other"
wong_seq <- unname(WONG[c("vermilion", "blue", "sky_blue", "green",
                           "orange", "pink", "yellow", "black")])
prey_colours <- setNames(
  c(wong_seq[seq_len(n_biol)], "#999999"),
  prey_levels
)

# Human-readable labels
prey_labels <- c(
  "Resource"                 = "Background resource",
  "bathypelagic fishes"      = "Bathypelagic fishes",
  "mesopelagic fishes"       = "Mesopelagic fishes",
  "antarctic krill"          = "Antarctic krill",
  "salps"                    = "Salps",
  "other krill"              = "Other krill",
  "mesozooplankton"          = "Mesozooplankton",
  "other macrozooplankton"   = "Other macrozooplankton",
  "shelf and coastal fishes" = "Shelf & coastal fishes",
  "toothfishes"              = "Toothfishes",
  "Other"                    = "Other prey"
)

# ── aggregate into plot_data ──────────────────────────────────────────────────
plot_data <- stats %>%
  mutate(prey_group = ifelse(prey %in% major_prey, prey, "Other")) %>%
  group_by(year, prey_group) %>%
  summarise(
    median_prop = sum(median, na.rm = TRUE),
    q25         = sum(q25,    na.rm = TRUE),
    q75         = sum(q75,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(prey_group = factor(prey_group,
                             levels = prey_levels[prey_levels %in% prey_group]))

###############################################################################
# Figure 1 – Stacked area chart (median proportions)
###############################################################################
cat("\nBuilding Figure 1 - stacked area...\n")

p1 <- ggplot(plot_data, aes(x = year, y = median_prop, fill = prey_group)) +
  geom_area(position = "stack", alpha = 0.85, colour = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = prey_colours,
    labels = prey_labels,
    guide  = guide_legend(title = NULL, nrow = 2, byrow = TRUE)
  ) +
  scale_x_continuous(breaks = seq(1850, 2010, 20)) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.001), expand = c(0, 0)
  ) +
  labs(x = "Year",
       y = "Proportion of total consumption") +
  theme_prydz() +
  theme(
    legend.key.size  = unit(3, "mm"),
    legend.spacing.x = unit(1, "mm")
  )

ggsave(file.path(OUTPUT_DIR, "baleen_diet_stacked_area.png"),
       p1, width = 7, height = 4, dpi = 300)
ggsave(file.path(OUTPUT_DIR, "baleen_diet_stacked_area.pdf"),
       p1, width = 7, height = 4)
cat("  Saved: baleen_diet_stacked_area.png/.pdf\n")

###############################################################################
# Figure 2 – Time series with IQR ribbons per prey group
###############################################################################
cat("Building Figure 2 - time series with IQR...\n")

major_ts <- plot_data %>% filter(prey_group != "Other")

p2 <- ggplot(major_ts, aes(x = year, colour = prey_group, fill = prey_group)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.18, colour = NA) +
  geom_line(aes(y = median_prop), linewidth = 0.6) +
  scale_colour_manual(
    values = prey_colours,
    labels = prey_labels,
    guide  = guide_legend(title = NULL, nrow = 2, byrow = TRUE)
  ) +
  scale_fill_manual(
    values = prey_colours,
    labels = prey_labels,
    guide  = guide_legend(title = NULL, nrow = 2, byrow = TRUE)
  ) +
  scale_x_continuous(breaks = seq(1850, 2010, 20)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Year",
       y = "Proportion of total consumption") +
  theme_prydz() +
  theme(
    legend.key.size  = unit(3, "mm"),
    legend.spacing.x = unit(1, "mm")
  )

ggsave(file.path(OUTPUT_DIR, "baleen_diet_major_prey_timeseries.png"),
       p2, width = 7, height = 4, dpi = 300)
ggsave(file.path(OUTPUT_DIR, "baleen_diet_major_prey_timeseries.pdf"),
       p2, width = 7, height = 4)
cat("  Saved: baleen_diet_major_prey_timeseries.png/.pdf\n")

###############################################################################
# Figure 3 – Background resource vs biological prey (two-line summary)
###############################################################################
cat("Building Figure 3 - resource vs biological prey...\n")

res_vs_bio <- plot_data %>%
  mutate(
    category = ifelse(as.character(prey_group) == "Resource",
                      "Background resource",
                      "Biological prey (named species)")
  ) %>%
  group_by(year, category) %>%
  summarise(
    median_prop = sum(median_prop),
    q25         = sum(q25),
    q75         = sum(q75),
    .groups = "drop"
  )

CAT_COLS <- c(
  "Background resource"           = unname(WONG["vermilion"]),
  "Biological prey (named species)" = unname(WONG["blue"])
)

p3 <- ggplot(res_vs_bio,
             aes(x = year, colour = category, fill = category)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.18, colour = NA) +
  geom_line(aes(y = median_prop), linewidth = 0.8) +
  scale_colour_manual(values = CAT_COLS,
                      guide  = guide_legend(title = NULL)) +
  scale_fill_manual(values   = CAT_COLS,
                    guide    = guide_legend(title = NULL)) +
  scale_x_continuous(breaks = seq(1850, 2010, 20)) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1), expand = c(0, 0.01)
  ) +
  labs(x = "Year",
       y = "Proportion of total consumption") +
  theme_prydz() +
  theme(legend.key.size = unit(4, "mm"))

ggsave(file.path(OUTPUT_DIR, "baleen_diet_resource_vs_biological.png"),
       p3, width = 7, height = 4, dpi = 300)
ggsave(file.path(OUTPUT_DIR, "baleen_diet_resource_vs_biological.pdf"),
       p3, width = 7, height = 4)
cat("  Saved: baleen_diet_resource_vs_biological.png/.pdf\n")

###############################################################################
# Figure 4 – Period comparison stacked bar
###############################################################################
cat("Building Figure 4 - period comparison bar...\n")

PERIOD_LEVELS <- c("Pre-whaling", "Early whaling", "Peak whaling",
                   "Post-whaling", "Krill fishing", "Modern")

period_data <- stats %>%
  mutate(
    period = case_when(
      year >= 1841 & year <= 1880 ~ "Pre-whaling",
      year >= 1900 & year <= 1920 ~ "Early whaling",
      year >= 1930 & year <= 1965 ~ "Peak whaling",
      year >= 1970 & year <= 1976 ~ "Post-whaling",
      year >= 1977 & year <= 1995 ~ "Krill fishing",
      year >= 2001 & year <= 2010 ~ "Modern",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  mutate(prey_group = ifelse(prey %in% major_prey, prey, "Other")) %>%
  group_by(period, prey_group) %>%
  summarise(median_prop = mean(median, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    period     = factor(period, levels = PERIOD_LEVELS),
    prey_group = factor(prey_group,
                        levels = prey_levels[prey_levels %in% prey_group])
  )

p4 <- ggplot(period_data, aes(x = period, y = median_prop, fill = prey_group)) +
  geom_col(position = "stack", width = 0.75, alpha = 0.85,
           colour = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = prey_colours,
    labels = prey_labels,
    guide  = guide_legend(title = NULL, nrow = 2, byrow = TRUE)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.001), expand = c(0, 0)
  ) +
  labs(x = NULL,
       y = "Proportion of total consumption") +
  theme_prydz() +
  theme(
    axis.text.x      = element_text(size = 6, angle = 30, hjust = 1),
    legend.key.size  = unit(3, "mm"),
    legend.spacing.x = unit(1, "mm")
  )

ggsave(file.path(OUTPUT_DIR, "baleen_diet_period_comparison.png"),
       p4, width = 7, height = 4, dpi = 300)
ggsave(file.path(OUTPUT_DIR, "baleen_diet_period_comparison.pdf"),
       p4, width = 7, height = 4)
cat("  Saved: baleen_diet_period_comparison.png/.pdf\n")

cat("\nAll figures saved to:", OUTPUT_DIR, "\nDone.\n")
