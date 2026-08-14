###############################################################################
# biomass_bb0_rmse_top10pct_grid.R
#
# Per-functional-group biomass depletion (B / B0) for the top-10% RMSE
# ensemble (212 paired members), 12-panel 3x4 grid.
#
# B0 = the *unexploited* (climate-only) counterpart of the SAME calibration.
# The ratio is formed WITHIN each of the 212 matched members first
#   ratio_i = B_fished_i / B_climate-only_i
# (so parameter/climate differences cancel within the pair), then summarised
# across members as the median (line) and 25-75th percentile IQR (ribbon).
# This is the biomass analogue of the paired abundance/mean-mass % change
# grids (abundance_meanweight_rmse_top10pct_grid.R) and the krill-consumption
# ratio (plot_krill_ratio_top10pct_timeseries.R).
#
# Two figures are produced:
#   1. RATIO grid  (biomass_bb0_ratio_rmse_top10pct_grid.png)
#        y = B/B0, with CCAMLR gamma2 0.75 / BMSY 0.40 / CCAMLR gamma1 0.20
#        depletion reference lines (+ IWC RMP 0.54 on the cetacean panels)
#        and the unexploited reference at 1.0.
#   2. % CHANGE grid (biomass_bb0_pctchange_rmse_top10pct_grid.png)
#        y = (B/B0 - 1) x 100 %, with a panel-specific +/-1 SD natural-
#        variability band (SNR detectability reference), matching the abundance
#        / mean-mass pctchange figures.
#
# Input caches (per-member biomass for the top-10% subset, built by
# abundance_meanweight_rmse_top10pct_grid.R):
#   abund_mw_top10pct_raw_fish.rds   (Year, Species, Biomass, Abundance, sim_i)
#   abund_mw_top10pct_raw_clim.rds
#
# Outputs:
#   biomass_bb0_ratio_rmse_top10pct_grid.png
#   biomass_bb0_pctchange_rmse_top10pct_grid.png
#   biomass_bb0_rmse_top10pct_summary.csv
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

message("=== Biomass B/B0 top-10% grid (3 x 4) ===")

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
FISH_CACHE <- "abund_mw_top10pct_raw_fish.rds"
CLIM_CACHE <- "abund_mw_top10pct_raw_clim.rds"

YEAR_MIN       <- 1900          # display window start
BASELINE_YEARS <- 1841:2010     # unexploited natural-variability baseline (SNR)

# ---------------------------------------------------------------------------
# Panel definitions - identical to biomass_rmse_top10pct_grid.R
# ---------------------------------------------------------------------------
group_defs <- list(
  "Zooplankton"            = c("mesozooplankton", "other krill",
                               "other macrozooplankton", "salps"),
  "Pelagic fishes & squid" = c("mesopelagic fishes", "bathypelagic fishes", "squids"),
  "Seabirds"               = c("flying birds", "small divers"),
  "Pinnipeds"              = c("medium divers", "large divers")
)

ind_to_panel <- c(
  "antarctic krill"          = "Antarctic krill",
  "toothfishes"              = "Toothfishes",
  "shelf and coastal fishes" = "Shelf & coastal fishes",
  "leopard seals"            = "Leopard seals",
  "minke whales"             = "Minke whales",
  "orca"                     = "Orca",
  "sperm whales"             = "Sperm whales",
  "baleen whales"            = "Large baleen whales"
)

panel_levels <- c(
  "Large baleen whales",    "Sperm whales",            "Minke whales",
  "Orca",                   "Leopard seals",            "Pinnipeds",
  "Seabirds",               "Toothfishes",              "Shelf & coastal fishes",
  "Pelagic fishes & squid", "Antarctic krill",          "Zooplankton"
)

panel_colors <- c(
  "Large baleen whales"     = "#FF61C3",
  "Minke whales"            = "#00B9E3",
  "Sperm whales"            = "#DB72FB",
  "Orca"                    = "#619CFF",
  "Leopard seals"           = "#E07B39",
  "Pinnipeds"               = "#2B6CB0",
  "Seabirds"                = "#9E9E9E",
  "Toothfishes"             = "#00C19F",
  "Shelf & coastal fishes"  = "#93AA00",
  "Pelagic fishes & squid"  = "#D39200",
  "Antarctic krill"         = "#F8766D",
  "Zooplankton"             = "#6A1B9A"
)

# Cetacean panels get the IWC RMP (0.54) reference line
whale_panels <- c("Large baleen whales", "Sperm whales", "Minke whales", "Orca")

# Regime-onset exploitation markers (shared across panels)
ONSET_YEARS  <- c(1930, 1974)
ONSET_LABELS <- c("Whaling starts", "Krill fishing starts")

# ---------------------------------------------------------------------------
# Helper: aggregate raw per-sim per-species biomass -> per-sim per-panel biomass
#   Aggregated groups: sum constituent species WITHIN each sim+year first.
#   Individual panels : passed through with panel label.
# Returns long df: sim_i, Year, panel, Biomass
# ---------------------------------------------------------------------------
to_panel_biomass <- function(raw_df) {
  agg <- do.call(rbind, lapply(names(group_defs), function(gname) {
    raw_df %>%
      filter(Species %in% group_defs[[gname]]) %>%
      group_by(sim_i, Year) %>%
      summarise(Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
      mutate(panel = gname)
  }))
  ind <- raw_df %>%
    filter(Species %in% names(ind_to_panel)) %>%
    mutate(panel = ind_to_panel[Species]) %>%
    select(sim_i, Year, panel, Biomass)
  bind_rows(agg, ind)
}

# ---------------------------------------------------------------------------
# Load caches
# ---------------------------------------------------------------------------
if (!file.exists(FISH_CACHE) || !file.exists(CLIM_CACHE))
  stop("Biomass caches not found. Run abundance_meanweight_rmse_top10pct_grid.R first ",
       "to build ", FISH_CACHE, " / ", CLIM_CACHE)

message("Loading top-10% biomass caches...")
fish_raw <- readRDS(FISH_CACHE)
clim_raw <- readRDS(CLIM_CACHE)

# Sanity: matched pairing by sim_i
stopifnot(setequal(unique(fish_raw$sim_i), unique(clim_raw$sim_i)))
n_members <- length(unique(fish_raw$sim_i))
message(sprintf("  %d matched members; year range %d-%d",
                n_members, min(fish_raw$Year), max(fish_raw$Year)))

fish_panel <- to_panel_biomass(fish_raw)
clim_panel <- to_panel_biomass(clim_raw)

# ---------------------------------------------------------------------------
# Paired B/B0 per member, then ensemble median + IQR
# ---------------------------------------------------------------------------
message("Computing paired B/B0 (fished / climate-only)...")

paired <- inner_join(fish_panel, clim_panel,
                     by = c("sim_i", "Year", "panel"),
                     suffix = c("_f", "_c")) %>%
  mutate(ratio = if_else(Biomass_c > 0, Biomass_f / Biomass_c, NA_real_)) %>%
  filter(is.finite(ratio))

summary_ratio <- paired %>%
  group_by(panel, Year) %>%
  summarise(
    med = median(ratio,          na.rm = TRUE),
    q25 = quantile(ratio, 0.25,  na.rm = TRUE),
    q75 = quantile(ratio, 0.75,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_med = (med - 1) * 100,
    pct_q25 = (q25 - 1) * 100,
    pct_q75 = (q75 - 1) * 100,
    panel   = factor(panel, levels = panel_levels)
  )

# ---------------------------------------------------------------------------
# +/-1 SD natural-variability band (SNR detectability reference)
#   noise = temporal SD, over 1841-2010, of the ensemble-MEAN unexploited
#           biomass trajectory; expressed as a CV (/ its baseline mean).
#   On the % axis the band is +/- CV*100 %; on the ratio axis it is 1 +/- CV.
# (matches METHODS_abundance_meanmass_pctchange.R section 5)
# ---------------------------------------------------------------------------
sd_band <- clim_panel %>%
  filter(Year %in% BASELINE_YEARS) %>%
  group_by(panel, Year) %>%
  summarise(mu = mean(Biomass, na.rm = TRUE), .groups = "drop") %>%
  group_by(panel) %>%
  summarise(
    ref_mean = mean(mu, na.rm = TRUE),
    noise    = sd(mu,   na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    cv      = noise / ref_mean,
    pct_ref = cv * 100,
    panel   = factor(panel, levels = panel_levels)
  )

# ---------------------------------------------------------------------------
# Save summary CSV
# ---------------------------------------------------------------------------
out_csv <- "biomass_bb0_rmse_top10pct_summary.csv"
summary_ratio %>%
  left_join(select(sd_band, panel, cv, pct_ref), by = "panel") %>%
  arrange(panel, Year) %>%
  write.csv(out_csv, row.names = FALSE)
message("Saved ", out_csv)

# ===========================================================================
# Shared 1-column x 12-row layout
#   Row order top->bottom = panel_levels (identical to the abundance /
#   mean-body-mass % change figures: Large baleen whales -> Zooplankton).
# ===========================================================================
facet_1col <- facet_grid(rows = vars(panel), scales = "free_y")

theme_1col <- theme_bw(base_size = 11) +
  theme(
    legend.position    = "bottom",
    legend.title       = element_text(size = 10, face = "bold"),
    legend.text        = element_text(size = 9),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing.y    = unit(0.35, "lines"),
    strip.background   = element_rect(fill = "grey92", colour = NA),
    strip.text.y.right = element_text(angle = 0, hjust = 0, size = 9,
                                      face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    axis.title         = element_text(size = 12)
  )

# ===========================================================================
# FIGURE 1 - B/B0 ratio grid with CCAMLR / BMSY / IWC depletion legend
# ===========================================================================
message("Building B/B0 ratio grid (1-col)...")

# Reference-value definitions (colour + linetype merged into one legend).
ref_levels <- c("Unexploited (B/B0 = 1)",
                "CCAMLR γ2 (0.75)",
                "IWC RMP (0.54)",
                "BMSY (0.40)",
                "CCAMLR γ1 (0.20)")
ref_col <- c("Unexploited (B/B0 = 1)"      = "grey40",
             "CCAMLR γ2 (0.75)"  = "#e6550d",
             "IWC RMP (0.54)"              = "#6a3d9a",
             "BMSY (0.40)"                 = "#e31a1c",
             "CCAMLR γ1 (0.20)"  = "#67000d")
ref_lty <- c("Unexploited (B/B0 = 1)"      = "solid",
             "CCAMLR γ2 (0.75)"  = "dashed",
             "IWC RMP (0.54)"              = "longdash",
             "BMSY (0.40)"                 = "dashed",
             "CCAMLR γ1 (0.20)"  = "dashed")

# Global references (drawn in every panel): all except the IWC RMP line.
global_ref_df <- data.frame(
  ref  = factor(c("Unexploited (B/B0 = 1)", "CCAMLR γ2 (0.75)",
                  "BMSY (0.40)", "CCAMLR γ1 (0.20)"),
                levels = ref_levels),
  yint = c(1.00, 0.75, 0.40, 0.20)
)
# IWC RMP 0.54 drawn only in the cetacean panels (carries the panel column).
whale_ref_df <- data.frame(
  panel = factor(whale_panels, levels = panel_levels),
  ref   = factor("IWC RMP (0.54)", levels = ref_levels),
  yint  = 0.54
)

# One combined colour scale spans group colours (median/ribbon) + reference
# colours; `breaks` restricts the legend to the reference levels only.
line_cols <- c(panel_colors, ref_col)

p_ratio <- ggplot(summary_ratio, aes(x = Year)) +
  # exploitation onset markers
  geom_vline(xintercept = ONSET_YEARS, linetype = "dotted",
             colour = "grey55", linewidth = 0.4) +
  # paired ensemble median + IQR
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = panel), alpha = 0.3) +
  # reference lines (mapped -> legend), drawn over the ribbon
  geom_hline(data = global_ref_df,
             aes(yintercept = yint, colour = ref, linetype = ref),
             linewidth = 0.5) +
  geom_hline(data = whale_ref_df,
             aes(yintercept = yint, colour = ref, linetype = ref),
             linewidth = 0.5) +
  geom_line(aes(y = med, colour = panel), linewidth = 1.0) +
  facet_1col +
  scale_fill_manual(values = panel_colors, guide = "none") +
  scale_colour_manual(values = line_cols, breaks = ref_levels,
                      name = "Reference level") +
  scale_linetype_manual(values = ref_lty, breaks = ref_levels,
                        name = "Reference level") +
  coord_cartesian(xlim = c(YEAR_MIN, 2010)) +
  guides(colour   = guide_legend(order = 1, nrow = 2),
         linetype = guide_legend(order = 1, nrow = 2)) +
  theme_1col +
  labs(
    x = "Year",
    y = expression(Biomass~depletion~~B / B[0]~~(fished / unexploited))
  )

ggsave("biomass_bb0_ratio_rmse_top10pct_grid.png", p_ratio,
       width = 9, height = 20, dpi = 300, limitsize = FALSE)
message("Saved biomass_bb0_ratio_rmse_top10pct_grid.png")

# ===========================================================================
# FIGURE 2 - B/B0 % change grid with +/-1 SD detectability band (legend)
# ===========================================================================
message("Building B/B0 % change grid (1-col)...")

BAND_LABEL <- "±1 SD natural variability"

band_lines <- sd_band %>%
  transmute(panel, lo = -pct_ref, hi = pct_ref) %>%
  tidyr::pivot_longer(c(lo, hi), values_to = "yint") %>%
  mutate(ref = factor(BAND_LABEL, levels = BAND_LABEL))

pct_cols <- c(panel_colors, setNames("#d62728", BAND_LABEL))

p_pct <- ggplot(summary_ratio, aes(x = Year)) +
  geom_vline(xintercept = ONSET_YEARS, linetype = "dotted",
             colour = "grey55", linewidth = 0.4) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5) +
  geom_ribbon(aes(ymin = pct_q25, ymax = pct_q75, fill = panel), alpha = 0.3) +
  # +/-1 SD natural-variability band (per panel) -> single-entry legend
  geom_hline(data = band_lines,
             aes(yintercept = yint, colour = ref, linetype = ref),
             linewidth = 0.45) +
  geom_line(aes(y = pct_med, colour = panel), linewidth = 1.0) +
  facet_1col +
  scale_fill_manual(values = panel_colors, guide = "none") +
  scale_colour_manual(values = pct_cols, breaks = BAND_LABEL, name = NULL) +
  scale_linetype_manual(values = setNames("dashed", BAND_LABEL),
                        breaks = BAND_LABEL, name = NULL) +
  coord_cartesian(xlim = c(YEAR_MIN, 2010)) +
  theme_1col +
  labs(
    x = "Year",
    y = expression(paste("Change in ", B / B[0], " (%)"))
  )

ggsave("biomass_bb0_pctchange_rmse_top10pct_grid.png", p_pct,
       width = 9, height = 20, dpi = 300, limitsize = FALSE)
message("Saved biomass_bb0_pctchange_rmse_top10pct_grid.png")

message("=== Done ===")