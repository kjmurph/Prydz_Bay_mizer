###############################################################################
# FishMIP Output Plotting Script — CORRECTED MODEL DOMAIN AREA
#
# Reads FishMIP-format CSVs (time = days since 1841-01-01) produced with
# the corrected MODEL_DOMAIN_AREA = 1.474341e+12 m^2.
# Outputs plots with "_corrected_area" suffix for comparison with old outputs.
#
# Inputs:
#   fishmip_outputs/          — fished (histsoc) tcb, tcblog10, tc, tclog10
#   fishmip_outputs_climate_only/ — unfished (nat) tcb, tcblog10
###############################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)

###############################################################################
# Configuration
###############################################################################

HISTSOC_DIR <- "fishmip_outputs"
NAT_DIR     <- "fishmip_outputs_climate_only"
PLOT_DIR    <- "fishmip_outputs/plots"
SUFFIX      <- "_corrected_area"

MODEL_DOMAIN_AREA <- 1.474341e+12  # m^2 (corrected)
GRAMS_PER_TONNE  <- 1e6

SIZE_CLASS_COLORS <- c(
  "1g-10g"    = "#440154",
  "10g-100g"  = "#3b528b",
  "100g-1kg"  = "#21908c",
  "1kg-10kg"  = "#5dc863",
  "10kg-100kg"= "#addc30",
  ">100kg"    = "#fde725"
)
SIZE_CLASS_ORDER <- c("1g-10g", "10g-100g", "100g-1kg", "1kg-10kg", "10kg-100kg", ">100kg")

PEAK_BALEEN_YEAR <- 1933
PEAK_SPERM_YEAR  <- 1948
PEAK_KRILL_YEAR  <- 1979
FISHING_START_YEAR <- 1961

if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)

###############################################################################
# Helper: convert FishMIP time (days since 1841-01-01) to year
###############################################################################

days_to_year <- function(days) as.integer(1841L + days / 365L)

###############################################################################
# Load fished (histsoc) data
###############################################################################

cat("Loading fished (histsoc) outputs...\n")

tcb_raw <- read.csv(file.path(HISTSOC_DIR, "mizer_gfdl-mom6-cobalt2_obsclim_histsoc_default_tcb_prydz-bay_annual_1841_2010.csv"))
tcb <- tcb_raw %>% mutate(year = days_to_year(time))
cat(sprintf("  tcb: %d rows, years %d-%d, median range [%.4f, %.4f] g m-2\n",
    nrow(tcb), min(tcb$year), max(tcb$year), min(tcb$median), max(tcb$median)))

tcblog10_raw <- read.csv(file.path(HISTSOC_DIR, "mizer_gfdl-mom6-cobalt2_obsclim_histsoc_default_tcblog10_prydz-bay_annual_1841_2010.csv"))
tcblog10 <- tcblog10_raw %>%
  mutate(year = days_to_year(time),
         size_class = factor(size_class, levels = SIZE_CLASS_ORDER))

tc_raw <- read.csv(file.path(HISTSOC_DIR, "mizer_gfdl-mom6-cobalt2_obsclim_histsoc_default_tc_prydz-bay_annual_1841_2010.csv"))
tc <- tc_raw %>% mutate(year = days_to_year(time))
cat(sprintf("  tc: %d rows, non-zero years = %d, max median = %.4f g m-2 yr-1\n",
    nrow(tc), sum(tc$median > 0), max(tc$median)))

tclog10_raw <- read.csv(file.path(HISTSOC_DIR, "mizer_gfdl-mom6-cobalt2_obsclim_histsoc_default_tclog10_prydz-bay_annual_1841_2010.csv"))
tclog10 <- tclog10_raw %>%
  mutate(year = days_to_year(time),
         size_class = factor(size_class, levels = SIZE_CLASS_ORDER))

###############################################################################
# Load climate-only (nat) data
###############################################################################

cat("Loading climate-only (nat) outputs...\n")

nat_tcb_raw <- read.csv(file.path(NAT_DIR, "mizer_gfdl-mom6-cobalt2_obsclim_nat_default_tcb_prydz-bay_annual_1841_2010.csv"))
nat_tcb <- nat_tcb_raw %>% mutate(year = days_to_year(time))
cat(sprintf("  nat tcb: %d rows, median range [%.4f, %.4f] g m-2\n",
    nrow(nat_tcb), min(nat_tcb$median), max(nat_tcb$median)))

nat_tcblog10_raw <- read.csv(file.path(NAT_DIR, "mizer_gfdl-mom6-cobalt2_obsclim_nat_default_tcblog10_prydz-bay_annual_1841_2010.csv"))
nat_tcblog10 <- nat_tcblog10_raw %>%
  mutate(year = days_to_year(time),
         size_class = factor(size_class, levels = SIZE_CLASS_ORDER))

cat("\n")

###############################################################################
# Load observed catch for comparison
###############################################################################

yield_fg <- read.csv("monte_carlo_2111_summaries/yield_timeseries_summary_per_year_tonnes.csv")
obs_catch <- yield_fg %>%
  group_by(Year) %>%
  summarise(obs_total_t = sum(ObsYield_t, na.rm = TRUE), .groups = "drop") %>%
  mutate(obs_gm2 = obs_total_t * GRAMS_PER_TONNE / MODEL_DOMAIN_AREA) %>%
  filter(Year >= 1900)

###############################################################################
# Plot 1: Total Consumer Biomass (tcb) — fished vs unfished
###############################################################################

cat("Plot 1: TCB fished vs unfished overlay...\n")

p_tcb_both <- ggplot() +
  # Unfished (nat) — grey
  geom_ribbon(data = nat_tcb, aes(x = year, ymin = q05, ymax = q95), fill = "grey60", alpha = 0.15) +
  geom_ribbon(data = nat_tcb, aes(x = year, ymin = q25, ymax = q75), fill = "grey60", alpha = 0.35) +
  geom_line(data = nat_tcb, aes(x = year, y = median, linetype = "Unfished (nat)"), color = "grey40", linewidth = 0.9) +
  # Fished (histsoc) — steelblue
  geom_ribbon(data = tcb, aes(x = year, ymin = q05, ymax = q95), fill = "steelblue", alpha = 0.15) +
  geom_ribbon(data = tcb, aes(x = year, ymin = q25, ymax = q75), fill = "steelblue", alpha = 0.35) +
  geom_line(data = tcb, aes(x = year, y = median, linetype = "Fished (histsoc)"), color = "steelblue", linewidth = 0.9) +
  geom_vline(xintercept = c(PEAK_BALEEN_YEAR, PEAK_SPERM_YEAR, PEAK_KRILL_YEAR),
             linetype = "dashed", color = "grey40", alpha = 0.7) +
  annotate("text", x = PEAK_BALEEN_YEAR, y = Inf, label = "Peak\nBaleen", vjust = 1.5, hjust = 0.5, size = 3, color = "grey30") +
  annotate("text", x = PEAK_SPERM_YEAR,  y = Inf, label = "Peak\nSperm",  vjust = 1.5, hjust = 0.5, size = 3, color = "grey30") +
  annotate("text", x = PEAK_KRILL_YEAR,  y = Inf, label = "Peak\nKrill",  vjust = 1.5, hjust = 0.5, size = 3, color = "grey30") +
  scale_linetype_manual(values = c("Fished (histsoc)" = "solid", "Unfished (nat)" = "solid"), name = NULL) +
  labs(
    title = "Total Consumer Biomass Density — Corrected Area (1.474×10¹² m²)",
    subtitle = "Fished (histsoc, blue) vs unfished (nat, grey) | Shading: 50% and 90% CI",
    x = "Year",
    y = expression("Biomass Density (g m"^-2*")")
  ) +
  scale_x_continuous(breaks = seq(1850, 2010, by = 20)) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank(),
        legend.position = "bottom")

fname <- file.path(PLOT_DIR, paste0("tcb_fished_vs_unfished", SUFFIX, ".png"))
ggsave(fname, p_tcb_both, width = 11, height = 6, dpi = 300)
cat(sprintf("  Saved: %s\n", fname))

###############################################################################
# Plot 2: Fished TCB time series (standalone)
###############################################################################

cat("Plot 2: Fished TCB time series...\n")

p_tcb <- ggplot(tcb, aes(x = year)) +
  geom_ribbon(aes(ymin = q05, ymax = q95), fill = "steelblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "steelblue", alpha = 0.4) +
  geom_line(aes(y = median), color = "steelblue", linewidth = 1) +
  geom_vline(xintercept = FISHING_START_YEAR, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(
    title = "Total Consumer Biomass Density — Fished (histsoc) — Corrected Area",
    subtitle = "2111-simulation ensemble | Red dashed: 1961 reference period",
    x = "Year", y = expression("Biomass Density (g m"^-2*")")
  ) +
  scale_x_continuous(breaks = seq(1850, 2010, by = 20)) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

fname <- file.path(PLOT_DIR, paste0("tcb_histsoc_timeseries", SUFFIX, ".png"))
ggsave(fname, p_tcb, width = 10, height = 6, dpi = 300)
cat(sprintf("  Saved: %s\n", fname))

###############################################################################
# Plot 3: Biomass by size class — faceted with uncertainty (fished)
###############################################################################

cat("Plot 3: TCBlog10 faceted uncertainty (fished)...\n")

p_tcblog10_facet <- ggplot(tcblog10, aes(x = year)) +
  geom_ribbon(aes(ymin = q05, ymax = q95, fill = size_class), alpha = 0.2) +
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = size_class), alpha = 0.4) +
  geom_line(aes(y = median, color = size_class), linewidth = 0.8) +
  geom_vline(xintercept = FISHING_START_YEAR, linetype = "dashed", color = "red", alpha = 0.5) +
  facet_wrap(~size_class, scales = "free_y", ncol = 2) +
  scale_color_manual(values = SIZE_CLASS_COLORS, guide = "none") +
  scale_fill_manual(values = SIZE_CLASS_COLORS, guide = "none") +
  labs(
    title = "Biomass Density by Size Class — Fished (histsoc) — Corrected Area",
    subtitle = "50% (dark) and 90% (light) CI | Red dashed: 1961",
    x = "Year", y = expression("Biomass Density (g m"^-2*")")
  ) +
  scale_x_continuous(breaks = seq(1850, 2010, by = 40)) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray90"))

fname <- file.path(PLOT_DIR, paste0("tcblog10_histsoc_faceted", SUFFIX, ".png"))
ggsave(fname, p_tcblog10_facet, width = 10, height = 10, dpi = 300)
cat(sprintf("  Saved: %s\n", fname))

###############################################################################
# Plot 4: Biomass by size class — faceted with uncertainty (unfished)
###############################################################################

cat("Plot 4: TCBlog10 faceted uncertainty (unfished)...\n")

p_nat_tcblog10_facet <- ggplot(nat_tcblog10, aes(x = year)) +
  geom_ribbon(aes(ymin = q05, ymax = q95, fill = size_class), alpha = 0.2) +
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = size_class), alpha = 0.4) +
  geom_line(aes(y = median, color = size_class), linewidth = 0.8) +
  facet_wrap(~size_class, scales = "free_y", ncol = 2) +
  scale_color_manual(values = SIZE_CLASS_COLORS, guide = "none") +
  scale_fill_manual(values = SIZE_CLASS_COLORS, guide = "none") +
  labs(
    title = "Biomass Density by Size Class — Unfished/nat — Corrected Area",
    subtitle = "50% (dark) and 90% (light) CI",
    x = "Year", y = expression("Biomass Density (g m"^-2*")")
  ) +
  scale_x_continuous(breaks = seq(1850, 2010, by = 40)) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray90"))

fname <- file.path(PLOT_DIR, paste0("tcblog10_nat_faceted", SUFFIX, ".png"))
ggsave(fname, p_nat_tcblog10_facet, width = 10, height = 10, dpi = 300)
cat(sprintf("  Saved: %s\n", fname))

###############################################################################
# Plot 5: Catch density (tc) with observed overlay
###############################################################################

cat("Plot 5: TC catch with observed overlay...\n")

tc_log <- tc %>%
  mutate(median_log = pmax(median, 1e-6),
         q05_log    = pmax(q05, 1e-6),
         q25_log    = pmax(q25, 1e-6),
         q75_log    = pmax(q75, 1e-6),
         q95_log    = pmax(q95, 1e-6)) %>%
  filter(year >= 1900)

obs_log <- obs_catch %>% mutate(obs_gm2_log = pmax(obs_gm2, 1e-6))

p_tc <- ggplot(tc_log, aes(x = year)) +
  geom_ribbon(aes(ymin = q05_log, ymax = q95_log), fill = "darkred", alpha = 0.2) +
  geom_ribbon(aes(ymin = q25_log, ymax = q75_log), fill = "darkred", alpha = 0.4) +
  geom_line(aes(y = median_log), color = "darkred", linewidth = 1) +
  geom_line(data = obs_log, aes(x = Year, y = obs_gm2_log), color = "black", linewidth = 0.5) +
  geom_point(data = obs_log, aes(x = Year, y = obs_gm2_log), color = "black", size = 1.5, alpha = 0.8) +
  geom_vline(xintercept = c(PEAK_BALEEN_YEAR, PEAK_SPERM_YEAR, PEAK_KRILL_YEAR),
             linetype = "dashed", color = "grey40", alpha = 0.7) +
  annotate("text", x = PEAK_BALEEN_YEAR, y = Inf, label = "Peak\nBaleen", vjust = 1.5, hjust = 0.5, size = 3, color = "grey30") +
  annotate("text", x = PEAK_SPERM_YEAR,  y = Inf, label = "Peak\nSperm",  vjust = 1.5, hjust = 0.5, size = 3, color = "grey30") +
  annotate("text", x = PEAK_KRILL_YEAR,  y = Inf, label = "Peak\nKrill",  vjust = 1.5, hjust = 0.5, size = 3, color = "grey30") +
  scale_y_log10(labels = scales::label_scientific(), limits = c(1e-6, 1)) +
  scale_x_continuous(breaks = seq(1900, 2010, by = 10)) +
  labs(
    title = "Total Catch Density — Corrected Area (1.474×10¹² m²)",
    subtitle = "Red: model ensemble (log scale) | Black points: observed catch",
    x = "Year", y = expression("Catch Density (g m"^-2*" yr"^-1*", log scale)")
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

fname <- file.path(PLOT_DIR, paste0("tc_catch_with_observed", SUFFIX, ".png"))
ggsave(fname, p_tc, width = 11, height = 6, dpi = 300)
cat(sprintf("  Saved: %s\n", fname))

###############################################################################
# Plot 6: Catch by size class — faceted with uncertainty
###############################################################################

cat("Plot 6: TClog10 faceted (fished, 1920+)...\n")

tclog10_fishing <- tclog10 %>% filter(year >= 1920)

p_tclog10_facet <- ggplot(tclog10_fishing, aes(x = year)) +
  geom_ribbon(aes(ymin = q05, ymax = q95, fill = size_class), alpha = 0.2) +
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = size_class), alpha = 0.4) +
  geom_line(aes(y = median, color = size_class), linewidth = 0.8) +
  geom_vline(xintercept = FISHING_START_YEAR, linetype = "dashed", color = "red", alpha = 0.5) +
  facet_wrap(~size_class, scales = "free_y", ncol = 2) +
  scale_color_manual(values = SIZE_CLASS_COLORS, guide = "none") +
  scale_fill_manual(values = SIZE_CLASS_COLORS, guide = "none") +
  labs(
    title = "Catch Density by Size Class — Fished (histsoc) — Corrected Area",
    subtitle = "50% (dark) and 90% (light) CI | Red dashed: 1961",
    x = "Year", y = expression("Catch Density (g m"^-2*" yr"^-1*")")
  ) +
  scale_x_continuous(breaks = seq(1920, 2010, by = 20)) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray90"))

fname <- file.path(PLOT_DIR, paste0("tclog10_histsoc_faceted", SUFFIX, ".png"))
ggsave(fname, p_tclog10_facet, width = 10, height = 10, dpi = 300)
cat(sprintf("  Saved: %s\n", fname))

###############################################################################
# Plot 7: Combined biomass + catch panel
###############################################################################

cat("Plot 7: Combined biomass + catch panel...\n")

p_biomass <- ggplot(tcb %>% filter(year >= 1900), aes(x = year)) +
  geom_ribbon(aes(ymin = q05, ymax = q95), fill = "steelblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "steelblue", alpha = 0.4) +
  geom_line(aes(y = median), color = "steelblue", linewidth = 1) +
  geom_ribbon(data = nat_tcb %>% filter(year >= 1900),
              aes(x = year, ymin = q25, ymax = q75), fill = "grey60", alpha = 0.25) +
  geom_line(data = nat_tcb %>% filter(year >= 1900),
            aes(x = year, y = median), color = "grey40", linewidth = 0.8, linetype = "dashed") +
  geom_vline(xintercept = c(PEAK_BALEEN_YEAR, PEAK_SPERM_YEAR, PEAK_KRILL_YEAR),
             linetype = "dotted", color = "grey50") +
  labs(subtitle = "Biomass  [fished=blue solid, unfished=grey dashed]",
       y = expression("Density (g m"^-2*")")) +
  scale_x_continuous(breaks = seq(1900, 2010, by = 20)) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(), axis.title.x = element_blank(),
        plot.subtitle = element_text(face = "bold", hjust = 0.5))

p_catch_panel <- ggplot(tc_log, aes(x = year)) +
  geom_ribbon(aes(ymin = q05_log, ymax = q95_log), fill = "darkred", alpha = 0.2) +
  geom_ribbon(aes(ymin = q25_log, ymax = q75_log), fill = "darkred", alpha = 0.4) +
  geom_line(aes(y = median_log), color = "darkred", linewidth = 1) +
  geom_line(data = obs_log, aes(x = Year, y = obs_gm2_log), color = "black", linewidth = 0.5) +
  geom_point(data = obs_log, aes(x = Year, y = obs_gm2_log), color = "black", size = 1.5) +
  scale_y_log10(labels = scales::label_scientific()) +
  labs(subtitle = "Catch  [model=red, observed=black points]",
       x = "Year", y = expression("Density (g m"^-2*", log)")) +
  scale_x_continuous(breaks = seq(1900, 2010, by = 20)) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.subtitle = element_text(face = "bold", hjust = 0.5))

p_combined <- p_biomass / p_catch_panel +
  plot_annotation(
    title = "FishMIP Outputs — Corrected Model Domain Area (1.474×10¹² m²)",
    subtitle = "Prydz Bay therMizer | 2111-simulation ensemble",
    theme = theme(plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(size = 11, color = "grey40"))
  )

fname <- file.path(PLOT_DIR, paste0("combined_biomass_catch", SUFFIX, ".png"))
ggsave(fname, p_combined, width = 11, height = 9, dpi = 300)
cat(sprintf("  Saved: %s\n", fname))

###############################################################################
# Plot 8: Proportional biomass by size class (fished)
###############################################################################

cat("Plot 8: Proportional biomass by size class...\n")

tcblog10_prop <- tcblog10 %>%
  group_by(year) %>%
  mutate(total = sum(median), proportion = median / total) %>%
  ungroup()

p_proportion <- ggplot(tcblog10_prop, aes(x = year, y = proportion, fill = size_class)) +
  geom_area(position = "stack", alpha = 0.9) +
  geom_vline(xintercept = FISHING_START_YEAR, linetype = "dashed", color = "white", linewidth = 1) +
  scale_fill_manual(values = SIZE_CLASS_COLORS, name = "Size Class") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Proportional Biomass by Size Class — Fished (histsoc) — Corrected Area",
    subtitle = "Relative contribution of each log10 size class to total consumer biomass",
    x = "Year", y = "Proportion of Total Biomass"
  ) +
  scale_x_continuous(breaks = seq(1850, 2010, by = 20)) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

fname <- file.path(PLOT_DIR, paste0("size_class_proportions_histsoc", SUFFIX, ".png"))
ggsave(fname, p_proportion, width = 12, height = 6, dpi = 300)
cat(sprintf("  Saved: %s\n", fname))

###############################################################################
# Summary
###############################################################################

cat("\n=============================================================\n")
cat("Done. All plots use corrected MODEL_DOMAIN_AREA = 1.474341e+12 m^2\n")
cat(sprintf("Plots saved to: %s\n", PLOT_DIR))
cat("Files generated (all with '_corrected_area' suffix):\n")
cat("  1. tcb_fished_vs_unfished_corrected_area.png\n")
cat("  2. tcb_histsoc_timeseries_corrected_area.png\n")
cat("  3. tcblog10_histsoc_faceted_corrected_area.png\n")
cat("  4. tcblog10_nat_faceted_corrected_area.png\n")
cat("  5. tc_catch_with_observed_corrected_area.png\n")
cat("  6. tclog10_histsoc_faceted_corrected_area.png\n")
cat("  7. combined_biomass_catch_corrected_area.png\n")
cat("  8. size_class_proportions_histsoc_corrected_area.png\n")
cat("=============================================================\n")
