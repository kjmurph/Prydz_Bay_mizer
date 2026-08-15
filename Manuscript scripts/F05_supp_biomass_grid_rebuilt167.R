# =============================================================================
# SUPPLEMENTARY FIGURE -- 3 x 4 biomass timeseries grid, exploited vs
# unexploited, rebuilt ensemble 44, cut A (unweighted yield RMSE), 167 members.
#
# This recreates biomass_rmse_top10pct_grid.png on the current ensemble cut. The
# panel definitions, aggregation rule, colours, scaling and layout are
# transcribed unchanged from biomass_rmse_top10pct_grid.R at the repo root.
#
# WHAT IS DIFFERENT: the original loads the 1.9 GB legacy Monte Carlo ensemble and
# the separate climate-only ensemble, extracts getBiomass() from every member and
# caches the result. None of that is needed here --
# F00_build_rebuilt167_data.R already emits exactly the required columns
# (sim_index, Year, Species, Biomass) for both arms, from the SAME projections
# that produced Figures 1-4. Reading those keeps this figure consistent with the
# main ones by construction rather than by coincidence.
#
# Aggregated groups (biomass summed WITHIN each sim before computing quantiles):
#   Zooplankton            = mesozooplankton + other krill
#                          + other macrozooplankton + salps
#   Pelagic fishes & squid = mesopelagic fishes + bathypelagic fishes + squids
#   Seabirds               = flying birds + small divers
#   Pinnipeds              = medium divers + large divers
#
# Writes Manuscript figures/Supplemental figures/
#        biomass_rmse_grid_rebuilt167.{png,pdf}
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
})

DATA <- "Manuscript data"
FIGS <- file.path("Manuscript figures", "Supplemental figures")
dir.create(FIGS, recursive = TRUE, showWarnings = FALSE)
# Default is cut A (the manuscript membership). Set F05_SUF=nrmse167 to draw the
# same grid on the balanced NRMSE_sd cut, whose inputs are written in the same
# schema by R/wmin_test/50_nrmse167_figure_data.R. Panels, aggregation, colours,
# scaling and layout are identical, so the two variants are directly comparable.
SUF <- Sys.getenv("F05_SUF", "rebuilt167")
# FIG_SET / FIG_OUTER select the member set and whether the outer percentile
# band is drawn. q05/q95 were already computed here and simply not plotted; the
# defaults leave it that way, so an unset environment reproduces the published
# figure exactly.
source("Manuscript scripts/F00z_member_set.R")
guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE); f
}

# --- panel definitions, transcribed from biomass_rmse_top10pct_grid.R ---------
group_defs <- list(
  "Zooplankton"            = c("mesozooplankton", "other krill",
                               "other macrozooplankton", "salps"),
  "Pelagic fishes & squid" = c("mesopelagic fishes", "bathypelagic fishes", "squids"),
  "Seabirds"               = c("flying birds", "small divers"),
  "Pinnipeds"              = c("medium divers", "large divers"))
ind_to_panel <- c(
  "antarctic krill"          = "Antarctic krill",
  "toothfishes"              = "Toothfishes",
  "shelf and coastal fishes" = "Shelf & coastal fishes",
  "leopard seals"            = "Leopard seals",
  "minke whales"             = "Minke whales",
  "orca"                     = "Orca",
  "sperm whales"             = "Sperm whales",
  "baleen whales"            = "Large baleen whales")
panel_levels <- c(
  "Large baleen whales",    "Sperm whales",            "Minke whales",
  "Orca",                   "Leopard seals",           "Pinnipeds",
  "Seabirds",               "Toothfishes",             "Shelf & coastal fishes",
  "Pelagic fishes & squid", "Antarctic krill",         "Zooplankton")
panel_colors <- c(
  "Large baleen whales"     = "#FF61C3", "Minke whales"           = "#00B9E3",
  "Sperm whales"            = "#DB72FB", "Orca"                   = "#619CFF",
  "Leopard seals"           = "#E07B39", "Pinnipeds"              = "#2B6CB0",
  "Seabirds"                = "#9E9E9E", "Toothfishes"            = "#00C19F",
  "Shelf & coastal fishes"  = "#93AA00", "Pelagic fishes & squid" = "#D39200",
  "Antarctic krill"         = "#F8766D", "Zooplankton"            = "#6A1B9A")

# --- aggregate raw biomass to panel-level summary ----------------------------
#   - Aggregated groups: sum within each sim first, then quantile across sims
#   - Individual panels: quantile directly across sims
build_panel_summary <- function(raw_df) {
  qs <- function(d) d %>% summarise(
    median = median(Biomass, na.rm = TRUE),
    q25    = quantile(Biomass, 0.25, na.rm = TRUE),
    q75    = quantile(Biomass, 0.75, na.rm = TRUE),
    q05    = quantile(Biomass, fig_outer_probs()[1], na.rm = TRUE),
    q95    = quantile(Biomass, fig_outer_probs()[2], na.rm = TRUE),
    .groups = "drop")

  agg_out <- bind_rows(lapply(names(group_defs), function(gname) {
    raw_df %>% filter(Species %in% group_defs[[gname]]) %>%
      group_by(sim_index, Year) %>%
      summarise(Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
      group_by(Year) %>% qs() %>% mutate(panel = gname)
  }))

  ind_out <- raw_df %>% filter(Species %in% names(ind_to_panel)) %>%
    group_by(Year, Species) %>% qs() %>%
    mutate(panel = unname(ind_to_panel[Species])) %>% select(-Species)

  bind_rows(agg_out, ind_out) %>%
    mutate(panel = factor(panel, levels = panel_levels))
}

# --- data --------------------------------------------------------------------
fish_raw <- readRDS(file.path(DATA, sprintf("biomass_abund_fish_%s.rds", SUF)))
clim_raw <- readRDS(file.path(DATA, sprintf("biomass_abund_clim_%s.rds", SUF)))
meta <- readRDS(file.path(DATA, sprintf("meta_%s.rds", SUF)))
message("members: ", meta$n_members, " | cut: ", meta$cut)
KEEP <- fig_members(meta)
fish_raw <- fig_filter(fish_raw, KEEP, "exploited")
clim_raw <- fig_filter(clim_raw, KEEP, "unexploited")

fish_summary <- build_panel_summary(fish_raw)
clim_summary <- build_panel_summary(clim_raw)

# --- plot --------------------------------------------------------------------
p_grid <- ggplot() +
  # Outer percentile bands first, so both IQRs sit on top of them. Off unless
  # FIG_OUTER=1; the quantiles themselves were always computed.
  {if (fig_outer())
    geom_ribbon(data = clim_summary, aes(x = Year, ymin = q05, ymax = q95),
                fill = "grey82", alpha = 0.30, colour = NA)} +
  {if (fig_outer())
    geom_ribbon(data = fish_summary,
                aes(x = Year, ymin = q05, ymax = q95, fill = panel),
                alpha = 0.14)} +
  # Unexploited: IQR ribbon (light grey), its bounds, and a thin median
  geom_ribbon(data = clim_summary, aes(x = Year, ymin = q25, ymax = q75),
              fill = "grey82", alpha = 0.6, colour = NA) +
  geom_line(data = clim_summary, aes(x = Year, y = q25),
            colour = "grey62", linewidth = 0.3, linetype = "dashed") +
  geom_line(data = clim_summary, aes(x = Year, y = q75),
            colour = "grey62", linewidth = 0.3, linetype = "dashed") +
  geom_line(data = clim_summary, aes(x = Year, y = median),
            colour = "grey38", linewidth = 0.4) +
  # Exploited: coloured IQR ribbon and a thicker median
  geom_ribbon(data = fish_summary,
              aes(x = Year, ymin = q25, ymax = q75, fill = panel), alpha = 0.3) +
  geom_line(data = fish_summary, aes(x = Year, y = median, colour = panel),
            linewidth = 1.1) +
  facet_wrap(~panel, ncol = 3, scales = "free_y") +
  scale_fill_manual(values   = panel_colors) +
  scale_colour_manual(values = panel_colors) +
  scale_y_continuous(labels = function(x) scales::number(x / 1e12)) +
  theme_bw(base_size = 13) +
  theme(legend.position  = "none",
        strip.text       = element_text(face = "bold"),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()) +
  labs(x = "Year", y = expression(Biomass~(10^6~t)))

# Member set and band go in the STEM so the variants cannot overwrite each other.
STEM <- sprintf("biomass_rmse_grid_%s%s", SUF,
                if (identical(Sys.getenv("FIG_SET", "all"), "all")) ""
                else paste0("_", fig_set_tag()))
if (fig_outer()) STEM <- paste0(STEM, "_iqr", 100 * fig_outer_probs()[2])
png_out <- guard(file.path(FIGS, sprintf("%s.png", STEM)))
pdf_out <- guard(file.path(FIGS, sprintf("%s.pdf", STEM)))
ggsave(png_out, p_grid, width = 16, height = 9, dpi = 300)
ggsave(pdf_out, p_grid, width = 16, height = 9)
write.csv(bind_rows(fish_summary %>% mutate(arm = "exploited"),
                    clim_summary %>% mutate(arm = "unexploited")),
          guard(file.path(DATA, sprintf("supp_%s_series.csv", STEM))),
          row.names = FALSE)

cat("\n=== biomass at 2010, 10^6 t (median across members) ===\n")
cmp <- fish_summary %>% filter(Year == 2010) %>%
  select(panel, exploited = median) %>%
  left_join(clim_summary %>% filter(Year == 2010) %>%
              select(panel, unexploited = median), by = "panel") %>%
  mutate(ratio = exploited / unexploited,
         exploited = exploited / 1e12, unexploited = unexploited / 1e12) %>%
  arrange(ratio)
print(as.data.frame(cmp), digits = 4, row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")