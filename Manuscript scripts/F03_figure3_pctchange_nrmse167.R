# =============================================================================
# NRMSE_sd VARIANT. Identical to the cut A script of the same name except for the
# ensemble it reads: the top 10% (167 members) ranked by the balanced
# multi-species objective -- equal-weight mean over the nine fished species of
# RMSE_tonnes / sd(observed tonnes), effort-on rows -- built by
# R/wmin_test/49_rank_nrmse_sd.R and R/wmin_test/50_nrmse167_figure_data.R, with
# the slope / 1 g-cutoff / diet inputs from
# Manuscript scripts/F00f_build_nrmse167_extras.R.
#
# It shares only 38 of 167 members with cut A. Panels, methods, palettes and
# layout are unchanged, so the two versions are directly comparable. Outputs are
# written to Manuscript figures/Supplemental figures/ and tagged _nrmse167, so
# nothing belonging to the cut A manuscript set is touched.
# =============================================================================
# =============================================================================
# FIGURE 3 -- paired percentage change from unexploited, by functional group,
# rebuilt ensemble 44, cut A (unweighted yield RMSE), 167 members.
#
#   a  Abundance change      (numbers)
#   b  Body size change      (mean individual mass)
#
# METHOD, following Manuscript scripts/METHODS_abundance_meanmass_pctchange.md,
# and UNCHANGED from the previous build.
#
# PAIRED signal (S4 of the METHODS). Each exploited member is paired with its own
# unexploited counterpart by simulation index, the percentage difference is formed
# WITHIN each pair, and only then summarised across members. Forming it from
# across-member means instead would leak the cross-calibration spread into the
# signal.
#
# +/-1 SD DETECTABILITY BAND (S5). Red dashed lines. Same SNR framework as
# Figure 2, expressed on the percentage axis:
#   1. ensemble-mean UNEXPLOITED trajectory for the panel
#   2. noise = temporal SD of that trajectory over 1841-2010 (one scalar)
#   3. band  = +/- noise / mean(trajectory) * 100%
# The noise deliberately does NOT use the inter-calibration spread: the plotted
# signal is already paired, so that spread has been removed, and re-introducing
# it would inflate the band by 1-2 orders of magnitude and render even large
# declines "non-significant".
#
# FORMATTING FOLLOWS pctchange_1col_sidebyside.png, i.e.
# abundance_meanweight_rmse_top10pct_grid.R:875-991. What that changes relative
# to the previous version of this figure:
#   - facet_grid with a single-level COLUMN facet carrying the metric name, so
#     "Abundance change" / "Body size change" are header strips rather than plot
#     titles; row strips on the right-hand plot only
#   - row strip text ROTATED (angle -90) instead of horizontal
#   - onset vlines in every panel but their text on the TOP panel only, off the
#     bottom axis (y = -Inf, hjust = 1, vjust = -0.3)
#   - per-group PEAK-EFFORT vlines, coloured by panel and drawn only in their own
#     panel, labelled with rotated text at the panel's y midpoint. These are new
#     to this figure; the previous version drew onset lines only.
#   - x range starts at 1900, matching Figures 1, 2 and 4
#
# Writes Manuscript figures/fig3_pctchange_rebuilt167.{png,pdf}
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(grid)
})

DATA <- "Manuscript data"; FIGS <- file.path("Manuscript figures", "Supplemental figures")
dir.create(FIGS, showWarnings = FALSE)
SUF <- "nrmse167"; BASELINE <- 1841:2010
guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE); f
}

# --- panel definitions, identical to abundance_meanweight_rmse_top10pct_grid.R -
group_defs <- list(
  "Zooplankton"            = c("mesozooplankton", "other krill",
                               "other macrozooplankton", "salps"),
  "Pelagic fishes & squid" = c("mesopelagic fishes", "bathypelagic fishes", "squids"),
  "Seabirds"               = c("flying birds", "small divers"),
  "Pinnipeds"              = c("medium divers", "large divers"))
ind_to_panel <- c("antarctic krill" = "Antarctic krill", "toothfishes" = "Toothfishes",
  "shelf and coastal fishes" = "Shelf & coastal fishes",
  "leopard seals" = "Leopard seals", "minke whales" = "Minke whales",
  "orca" = "Orca", "sperm whales" = "Sperm whales",
  "baleen whales" = "Large baleen whales")
panel_levels <- c("Large baleen whales", "Sperm whales", "Minke whales", "Orca",
  "Leopard seals", "Pinnipeds", "Seabirds", "Toothfishes",
  "Shelf & coastal fishes", "Pelagic fishes & squid", "Antarctic krill",
  "Zooplankton")
panel_colors <- c("Large baleen whales" = "#FF61C3", "Minke whales" = "#00B9E3",
  "Sperm whales" = "#DB72FB", "Orca" = "#619CFF", "Leopard seals" = "#E07B39",
  "Pinnipeds" = "#2B6CB0", "Seabirds" = "#9E9E9E", "Toothfishes" = "#00C19F",
  "Shelf & coastal fishes" = "#93AA00", "Pelagic fishes & squid" = "#D39200",
  "Antarctic krill" = "#F8766D", "Zooplankton" = "#6A1B9A")

sp2panel <- c(ind_to_panel,
  setNames(rep(names(group_defs), lengths(group_defs)), unlist(group_defs)))

# --- data --------------------------------------------------------------------
bf <- readRDS(file.path(DATA, sprintf("biomass_abund_fish_%s.rds", SUF)))
bc <- readRDS(file.path(DATA, sprintf("biomass_abund_clim_%s.rds", SUF)))
meta <- readRDS(file.path(DATA, sprintf("meta_%s.rds", SUF)))
message("members: ", meta$n_members, " | cut: ", meta$cut)

# Aggregate to panels FIRST: for multi-species panels the abundance is summed and
# the mean mass is the biomass-weighted group mean (total biomass / total number),
# not the mean of per-species means.
to_panel <- function(d) d %>%
  mutate(panel = unname(sp2panel[Species])) %>% filter(!is.na(panel)) %>%
  group_by(sim_index, Year, panel) %>%
  summarise(Abundance = sum(Abundance), Biomass = sum(Biomass), .groups = "drop") %>%
  mutate(MeanMass = ifelse(Abundance > 0, Biomass / Abundance, NA_real_))
PF <- to_panel(bf); PC <- to_panel(bc)

# --- paired percentage change ------------------------------------------------
pair <- inner_join(PF, PC, by = c("sim_index", "Year", "panel"),
                   suffix = c("_f", "_c")) %>%
  mutate(pct_abund = 100 * (Abundance_f - Abundance_c) / Abundance_c,
         pct_mass  = 100 * (MeanMass_f  - MeanMass_c)  / MeanMass_c)

summ <- function(v) pair %>% group_by(panel, Year) %>%
  summarise(med = median(.data[[v]], na.rm = TRUE),
            lo = quantile(.data[[v]], 0.25, na.rm = TRUE),
            hi = quantile(.data[[v]], 0.75, na.rm = TRUE), .groups = "drop") %>%
  mutate(panel = factor(panel, levels = panel_levels))
S_ab <- summ("pct_abund"); S_mw <- summ("pct_mass")

# --- +/-1 SD detectability band, per panel and metric ------------------------
band <- function(metric) PC %>% group_by(panel, Year) %>%
  summarise(mu = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
  filter(Year %in% BASELINE) %>% group_by(panel) %>%
  summarise(ref_pct = 100 * sd(mu, na.rm = TRUE) / mean(mu, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(panel = factor(panel, levels = panel_levels))
B_ab <- band("Abundance"); B_mw <- band("MeanMass")

# --- era markers, transcribed from the reference -----------------------------
eff <- readRDS("effort_array_1841_2010.rds"); yr <- as.numeric(rownames(eff))
onset <- function(s){y <- yr[eff[,s]>0]; if(length(y)) min(y) else NA}
key_years  <- c(min(c(onset("baleen whales"), onset("sperm whales")), na.rm = TRUE),
                onset("antarctic krill"))
key_labels <- c("Whaling\nstarts", "Krill fishing\nstarts")
event_df   <- data.frame(Year = key_years, label = key_labels)

# 1-column layout -> the global event text goes on the SINGLE top row only
event_label_df_1col <- data.frame(
  Year = key_years, label = key_labels,
  panel = factor(panel_levels[1], levels = panel_levels),
  stringsAsFactors = FALSE)

sp_event_labels <- c(
  "baleen whales"            = "Peak baleen whaling",
  "sperm whales"             = "Peak sperm whaling",
  "minke whales"             = "Peak minke whaling",
  "orca"                     = "Peak orca hunting",
  "leopard seals"            = "Peak leopard seal hunting",
  "toothfishes"              = "Peak toothfish fishing",
  "shelf and coastal fishes" = "Peak coastal fishing",
  "antarctic krill"          = "Peak krill fishing",
  "flying birds"             = "Peak seabird harvest",
  "small divers"             = "Peak penguin harvest",
  "squids"                   = "Peak squid fishing",
  "mesopelagic fishes"       = "Peak mesopelagic fishing",
  "bathypelagic fishes"      = "Peak bathypelagic fishing",
  "medium divers"            = "Peak seal hunting",
  "large divers"             = "Peak seal hunting")

# Per-gear argmax of the effort array -> one vline per exploited group, drawn
# only in its own panel (ggplot restricts each row by the facet variable).
fishing_peak <- do.call(rbind, lapply(colnames(eff), function(sp) {
  if (any(eff[, sp] > 0))
    data.frame(Species = sp, first_year = yr[which.max(eff[, sp])],
               stringsAsFactors = FALSE)
}))
species_events_df <- fishing_peak %>%
  mutate(panel = unname(sp2panel[Species]),
         sp_label = coalesce(unname(sp_event_labels[Species]), Species)) %>%
  filter(!is.na(panel)) %>%
  mutate(panel = factor(panel, levels = panel_levels))
message("peak-effort events: ", nrow(species_events_df), " across ",
        n_distinct(species_events_df$panel), " panels")

# label height: mid of the plotted IQR range within each panel
ymid <- function(S) S %>% group_by(panel) %>%
  summarise(y_mid = (max(hi, na.rm = TRUE) + min(lo, na.rm = TRUE)) / 2,
            .groups = "drop")
ev_ab <- left_join(species_events_df, ymid(S_ab), by = "panel")
ev_mw <- left_join(species_events_df, ymid(S_mw), by = "panel")

# --- shared theme, transcribed from the reference ----------------------------
theme_1col_base <- theme_bw(base_size = 10) +
  theme(legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.y  = unit(0.4, "lines"),
        axis.title       = element_text(size = 11),
        axis.text        = element_text(size = 9))

# col_label becomes a single-level column facet, which is what draws the top
# header strip; show_strips controls the right-hand row strips.
build_1col_pct <- function(summ_df, ref_df, events_sp_df, y_lab, show_strips,
                           col_label) {
  summ_df <- mutate(summ_df, col_facet = factor(col_label, levels = col_label))
  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40",
               linewidth = 0.5) +
    geom_ribbon(data = summ_df, aes(x = Year, ymin = lo, ymax = hi, fill = panel),
                alpha = 0.3) +
    # +/-1 SD reference lines, drawn under the median so it stays legible
    geom_hline(data = ref_df, aes(yintercept = ref_pct), inherit.aes = FALSE,
               colour = "red", linetype = "dashed", linewidth = 0.45, alpha = 0.85) +
    geom_hline(data = ref_df, aes(yintercept = -ref_pct), inherit.aes = FALSE,
               colour = "red", linetype = "dashed", linewidth = 0.45, alpha = 0.85) +
    geom_line(data = summ_df, aes(x = Year, y = med, colour = panel),
              linewidth = 0.8) +
    geom_vline(data = event_df, aes(xintercept = Year), inherit.aes = FALSE,
               linetype = "dotted", colour = "grey50", linewidth = 0.5) +
    geom_text(data = event_label_df_1col, aes(x = Year, label = label),
              inherit.aes = FALSE, y = -Inf, hjust = 1, vjust = -0.3,
              size = 3.4, colour = "grey35") +
    geom_vline(data = events_sp_df, aes(xintercept = first_year, colour = panel),
               inherit.aes = FALSE, linetype = "dashed", linewidth = 0.45,
               alpha = 0.8) +
    geom_text(data = events_sp_df,
              aes(x = first_year, y = y_mid, label = sp_label), inherit.aes = FALSE,
              angle = 90, hjust = 0.5, vjust = 1.3, size = 3.2, colour = "grey10") +
    facet_grid(rows = vars(panel), cols = vars(col_facet), scales = "free_y") +
    coord_cartesian(xlim = c(1900, NA)) +
    scale_fill_manual(values = panel_colors, guide = "none") +
    scale_colour_manual(values = panel_colors, guide = "none") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    theme_1col_base +
    theme(strip.text.x = element_text(face = "bold", size = 11)) +
    labs(x = "Year", y = y_lab)

  if (show_strips) {
    # Rotated text on the right-side (row) strip; keep the top column strip
    p + theme(strip.text.y.right = element_text(angle = -90, hjust = 0.5,
                                                vjust = 0.5, size = 9.6))
  } else {
    # Hide only the row strip; the top column-header strip stays visible
    p + theme(strip.text.y = element_blank(),
              strip.background.y = element_blank())
  }
}

p_ab <- build_1col_pct(S_ab, B_ab, ev_ab, "% change from unexploited",
                       show_strips = FALSE, col_label = "Abundance change")
p_mw <- build_1col_pct(S_mw, B_mw, ev_mw, "% change from unexploited",
                       show_strips = TRUE,  col_label = "Body size change")

# Wider right panel so the strip labels do not compress its plot area.
fig <- (p_ab + p_mw +
  plot_layout(ncol = 2, widths = c(1, 1.05)) +
  plot_annotation(tag_levels = "a")) &
  theme(plot.tag = element_text(face = "bold", size = 12))

png_out <- guard(file.path(FIGS, sprintf("fig3_pctchange_%s.png", SUF)))
pdf_out <- guard(file.path(FIGS, sprintf("fig3_pctchange_%s.pdf", SUF)))
ggsave(png_out, fig, width = 14, height = 20, dpi = 300, limitsize = FALSE)
ggsave(pdf_out, fig, width = 14, height = 20, limitsize = FALSE)

out <- bind_rows(S_ab %>% mutate(metric = "abundance"),
                 S_mw %>% mutate(metric = "mean_mass")) %>%
  left_join(bind_rows(B_ab %>% mutate(metric = "abundance"),
                      B_mw %>% mutate(metric = "mean_mass")),
            by = c("panel", "metric"))
write.csv(out, guard(file.path(DATA, sprintf("fig3_pctchange_series_%s.csv", SUF))),
          row.names = FALSE)

cat("\n=== % change at 2010 (median across members) ===\n")
print(as.data.frame(out %>% filter(Year == 2010) %>%
  select(panel, metric, med, ref_pct) %>% arrange(metric, med)), digits = 4,
  row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")