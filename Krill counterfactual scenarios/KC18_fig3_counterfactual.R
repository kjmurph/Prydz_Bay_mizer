# =============================================================================
# KC18 -- Figure 3 with the KRILL COUNTERFACTUALS referenced to the OBSERVED
# HISTORY, rather than everything referenced to unexploited
#
# WHAT THIS CHANGES AND WHY. F03 plots one line per panel: the exploited arm as
# a percentage change from its own unexploited counterpart. That answers "what
# did exploitation do", and whaling dominates it -- baleen whales are down 54%,
# so a 0.02% krill-fishing effect is invisible on the same axis. This figure
# changes the DENOMINATOR to the observed history and plots the two krill
# counterfactuals against it, so the krill-fishing signal is the whole content
# of the panel instead of a rounding error on the whaling signal.
#
#   F03   : (exploited - unexploited) / unexploited
#   here  : (no_krill   - exploited)  / exploited
#           (peak_krill - exploited)  / exploited
#
# COLOUR MOVES FROM PANEL TO SCENARIO. F03 colours by functional group because
# it has one line per panel and the group is the only thing to encode. Here
# there are two lines per panel and the scenario is what has to be
# distinguishable, so it takes the counterfactual palette used throughout the
# KC figures -- blue <- ink -> red, diverging on krill-fishing intensity. The
# per-group colours are dropped; the row strip already names the group.
#
# THE +-1 SD BAND IS UNCHANGED, and deliberately so: it is still the temporal SD
# of the ensemble-mean UNEXPLOITED trajectory, which is the natural-variability
# yardstick used by Figures 2, 3 and 4. Keeping the same band means a reader can
# compare a bar height here directly against the published Figure 3. It is NOT
# rescaled to the exploited denominator -- doing that would make the two figures
# silently incomparable.
#
# RIBBONS ARE UNDERLAID, as in KC15: both scenario IQRs are drawn, peak_krill
# last so it sits on top, because no_krill is nearly coincident with the zero
# line everywhere and would otherwise be buried.
#
# GEOMETRY otherwise transcribed from F03_figure3_pctchange_rebuilt167.R -- the
# same panel definitions and order, facet_grid with a single-level column facet
# for the metric header, rotated right-hand row strips, era vlines with the text
# on the top row only, and per-group peak-effort vlines.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC18_fig3_counterfactual.R"
# ENV    KC18_IN, KC18_SUF (whres000), FIG_OUT, KC_FORCE
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(grid)
})

OUT_LARGE <- "Output_large_files/wmin_test"
KC_ROOT <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
ANA <- file.path(KC_ROOT, "analysis")
FIGS <- Sys.getenv("FIG_OUT", "Manuscript figures")
for (d in c(ANA, FIGS)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
IN <- Sys.getenv("KC18_IN", file.path(OUT_LARGE, "KC14_whres000_n167.rds"))
SUF <- Sys.getenv("KC18_SUF", "whres000")
FORCE <- nzchar(Sys.getenv("KC_FORCE"))
BASELINE <- 1841:2010
guard <- function(f) {
  if (file.exists(f) && !FORCE)
    stop("refusing to overwrite: ", f, "\n  set KC_FORCE=1", call. = FALSE)
  f
}
if (!file.exists(IN))
  stop("missing ", IN, " -- run KC14_ensemble_whres000.R first", call. = FALSE)

# --- panel definitions, identical to F03 --------------------------------------
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
sp2panel <- c(ind_to_panel,
  setNames(rep(names(group_defs), lengths(group_defs)), unlist(group_defs)))

# --- data ---------------------------------------------------------------------
D <- readRDS(IN); meta <- D$meta
# USABLE = stable AND admissible, matching KC16b. `stable` alone was a no-op on
# the phase-61 ensemble (stable == usable == 122) but is not on ensembles where
# the reproduction treatment pushes erepro >= 1: phase 88 is 521 stable of which
# only 427 are usable, and one inadmissible member can distort a median.
adm <- if ("n_erepro_ge1" %in% names(D$members)) D$members$n_erepro_ge1 == 0 else TRUE
keep <- D$members$sim_index[D$members$stable & adm]
cat("=== KC18: Figure 3, counterfactuals vs observed history ===\n")
cat("input:", basename(IN), "| base:", basename(meta$base), "\n")
cat("members:", nrow(D$members), "| stable:", sum(D$members$stable),
    "| stable AND admissible (USED):", length(keep), "\n")
if (!length(keep)) stop("no usable members", call. = FALSE)
B <- D$biomass %>% filter(sim_index %in% keep)
n_mem <- length(keep)

to_panel <- function(d) d %>%
  mutate(panel = unname(sp2panel[Species])) %>% filter(!is.na(panel)) %>%
  group_by(sim_index, arm, Year, panel) %>%
  summarise(Abundance = sum(Abundance), Biomass = sum(Biomass), .groups = "drop") %>%
  mutate(MeanMass = ifelse(Abundance > 0, Biomass / Abundance, NA_real_))
P <- to_panel(B)

# --- paired percentage change, referenced to the OBSERVED HISTORY -------------
ref <- P %>% filter(arm == "exploited") %>%
  select(sim_index, Year, panel, Ab_e = Abundance, Mm_e = MeanMass)
CF <- P %>% filter(arm %in% c("no_krill", "peak_krill")) %>%
  inner_join(ref, by = c("sim_index", "Year", "panel")) %>%
  mutate(pct_abund = 100 * (Abundance - Ab_e) / Ab_e,
         pct_mass  = 100 * (MeanMass  - Mm_e) / Mm_e)

SCEN <- c(no_krill = "No krill fishing",
          peak_krill = sprintf("Peak krill effort held from %d", meta$peak_year))
SCEN_COL <- setNames(c("#2a78d6", "#e34948"), unname(SCEN))

summ <- function(v) CF %>% group_by(panel, arm, Year) %>%
  summarise(med = median(.data[[v]], na.rm = TRUE),
            lo = quantile(.data[[v]], 0.25, na.rm = TRUE),
            hi = quantile(.data[[v]], 0.75, na.rm = TRUE), .groups = "drop") %>%
  mutate(panel = factor(panel, levels = panel_levels),
         scen = factor(SCEN[arm], levels = unname(SCEN)))
S_ab <- summ("pct_abund"); S_mw <- summ("pct_mass")

# --- +-1 SD band: UNCHANGED from F03, still off the unexploited arm -----------
PU <- P %>% filter(arm == "unexploited")
band <- function(metric) PU %>% group_by(panel, Year) %>%
  summarise(mu = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
  filter(Year %in% BASELINE) %>% group_by(panel) %>%
  summarise(ref_pct = 100 * sd(mu, na.rm = TRUE) / mean(mu, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(panel = factor(panel, levels = panel_levels))
B_ab <- band("Abundance"); B_mw <- band("MeanMass")

# --- era markers, transcribed from F03 ----------------------------------------
eff <- readRDS("effort_array_1841_2010.rds"); yr <- as.numeric(rownames(eff))
onset <- function(s) { y <- yr[eff[, s] > 0]; if (length(y)) min(y) else NA }
key_years  <- c(min(c(onset("baleen whales"), onset("sperm whales")), na.rm = TRUE),
                onset("antarctic krill"))
key_labels <- c("Whaling\nstarts", "Krill fishing\nstarts")
event_df   <- data.frame(Year = key_years, label = key_labels)
event_label_df_1col <- data.frame(
  Year = key_years, label = key_labels,
  panel = factor(panel_levels[1], levels = panel_levels))
sp_event_labels <- c(
  "baleen whales" = "Peak baleen whaling", "sperm whales" = "Peak sperm whaling",
  "minke whales" = "Peak minke whaling", "orca" = "Peak orca hunting",
  "leopard seals" = "Peak leopard seal hunting",
  "toothfishes" = "Peak toothfish fishing",
  "shelf and coastal fishes" = "Peak coastal fishing",
  "antarctic krill" = "Peak krill fishing", "flying birds" = "Peak seabird harvest",
  "small divers" = "Peak penguin harvest", "squids" = "Peak squid fishing",
  "mesopelagic fishes" = "Peak mesopelagic fishing",
  "bathypelagic fishes" = "Peak bathypelagic fishing",
  "medium divers" = "Peak seal hunting", "large divers" = "Peak seal hunting")
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
ymid <- function(S) S %>% group_by(panel) %>%
  summarise(y_mid = (max(hi, na.rm = TRUE) + min(lo, na.rm = TRUE)) / 2,
            .groups = "drop")
ev_ab <- left_join(species_events_df, ymid(S_ab), by = "panel")
ev_mw <- left_join(species_events_df, ymid(S_mw), by = "panel")

theme_1col_base <- theme_bw(base_size = 10) +
  theme(legend.position  = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.y  = unit(0.4, "lines"),
        axis.title       = element_text(size = 11),
        axis.text        = element_text(size = 9))

build_1col_pct <- function(summ_df, ref_df, events_sp_df, y_lab, show_strips,
                           col_label) {
  summ_df <- mutate(summ_df, col_facet = factor(col_label, levels = col_label))
  rib <- function(a) geom_ribbon(
    data = filter(summ_df, arm == a),
    aes(x = Year, ymin = lo, ymax = hi, fill = scen), alpha = 0.28, colour = NA)
  ln <- function(a, lw) geom_line(data = filter(summ_df, arm == a),
                                  aes(x = Year, y = med, colour = scen),
                                  linewidth = lw)
  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40",
               linewidth = 0.5) +
    # no_krill sits on the zero line almost everywhere, so peak_krill goes on
    # top of it rather than under it.
    rib("no_krill") + rib("peak_krill") +
    geom_hline(data = ref_df, aes(yintercept = ref_pct), inherit.aes = FALSE,
               colour = "grey35", linetype = "dashed", linewidth = 0.45) +
    geom_hline(data = ref_df, aes(yintercept = -ref_pct), inherit.aes = FALSE,
               colour = "grey35", linetype = "dashed", linewidth = 0.45) +
    ln("no_krill", 0.55) + ln("peak_krill", 0.9) +
    geom_vline(data = event_df, aes(xintercept = Year), inherit.aes = FALSE,
               linetype = "dotted", colour = "grey50", linewidth = 0.5) +
    geom_text(data = event_label_df_1col, aes(x = Year, label = label),
              inherit.aes = FALSE, y = -Inf, hjust = 1, vjust = -0.3,
              size = 3.4, colour = "grey35") +
    geom_vline(data = events_sp_df, aes(xintercept = first_year),
               inherit.aes = FALSE, linetype = "dashed", linewidth = 0.4,
               colour = "grey55") +
    geom_text(data = events_sp_df,
              aes(x = first_year, y = y_mid, label = sp_label),
              inherit.aes = FALSE, angle = 90, hjust = 0.5, vjust = 1.3,
              size = 3.2, colour = "grey25") +
    facet_grid(rows = vars(panel), cols = vars(col_facet), scales = "free_y") +
    coord_cartesian(xlim = c(1900, NA)) +
    scale_colour_manual(values = SCEN_COL, name = "Scenario",
                        limits = unname(SCEN),
                        guide = guide_legend(
                          override.aes = list(linewidth = 1.4))) +
    scale_fill_manual(values = SCEN_COL, guide = "none") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    theme_1col_base +
    theme(strip.text.x = element_text(face = "bold", size = 11)) +
    labs(x = "Year", y = y_lab)
  if (show_strips)
    p + theme(strip.text.y.right = element_text(angle = -90, hjust = 0.5,
                                                vjust = 0.5, size = 9.6))
  else
    p + theme(strip.text.y = element_blank(), strip.background.y = element_blank())
}

YLAB <- "% change from observed history"
p_ab <- build_1col_pct(S_ab, B_ab, ev_ab, YLAB, FALSE, "Abundance change")
p_mw <- build_1col_pct(S_mw, B_mw, ev_mw, YLAB, TRUE,  "Body size change")
fig <- (p_ab + p_mw + plot_layout(ncol = 2, widths = c(1, 1.05),
                                  guides = "collect") +
  plot_annotation(tag_levels = "a")) &
  theme(plot.tag = element_text(face = "bold", size = 12),
        legend.position = "bottom")

png_out <- guard(file.path(FIGS, sprintf("fig3_pctchange_vs_history_%s.png", SUF)))
pdf_out <- guard(file.path(FIGS, sprintf("fig3_pctchange_vs_history_%s.pdf", SUF)))
suppressWarnings({
  ggsave(png_out, fig, width = 13.5, height = 17.5, dpi = 300, limitsize = FALSE)
  ggsave(pdf_out, fig, width = 13.5, height = 17.5, limitsize = FALSE)
})

cat("\n=== change at 2010 vs observed history (%) ===\n")
tab <- bind_rows(mutate(S_ab, metric = "abundance"),
                 mutate(S_mw, metric = "mean_mass")) %>%
  filter(Year == 2010) %>%
  transmute(panel, metric, arm, med = round(med, 4)) %>%
  pivot_wider(names_from = arm, values_from = med) %>%
  arrange(metric, peak_krill)
print(as.data.frame(tab), row.names = FALSE)

OUTC <- bind_rows(mutate(S_ab, metric = "abundance"),
                  mutate(S_mw, metric = "mean_mass"))
write.csv(OUTC, file.path(ANA, sprintf("KC18_fig3_vs_history_%s.csv", SUF)),
          row.names = FALSE)
cat("\nwrote:\n  ", png_out, "\n   ", pdf_out, "\n")
cat("KC18 complete.\n")