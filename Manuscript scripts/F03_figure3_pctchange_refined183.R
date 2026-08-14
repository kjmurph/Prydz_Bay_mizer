# =============================================================================
# FIGURE 3 -- paired percentage change from unexploited, by functional group,
# refined 183-member fitted ensemble.
#
#   a  Abundance change      (numbers)
#   b  Body size change      (mean individual mass)
#
# METHOD, following Manuscript scripts/METHODS_abundance_meanmass_pctchange.md.
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
# Writes Manuscript figures/fig3_pctchange_refined183.{png,pdf}
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
})

DATA <- "Manuscript data"; FIGS <- "Manuscript figures"
dir.create(FIGS, showWarnings = FALSE)
SUF <- "refined183"; BASELINE <- 1841:2010
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
message("members: ", meta$n_members)

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
            hi = quantile(.data[[v]], 0.75, na.rm = TRUE), .groups = "drop")
S_ab <- summ("pct_abund"); S_mw <- summ("pct_mass")

# --- +/-1 SD detectability band, per panel and metric ------------------------
band <- function(metric) PC %>% group_by(panel, Year) %>%
  summarise(mu = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
  filter(Year %in% BASELINE) %>% group_by(panel) %>%
  summarise(ref = 100 * sd(mu, na.rm = TRUE) / mean(mu, na.rm = TRUE), .groups = "drop")
B_ab <- band("Abundance"); B_mw <- band("MeanMass")

# --- era markers --------------------------------------------------------------
eff <- readRDS("effort_array_1841_2010.rds"); yr <- as.numeric(rownames(eff))
onset <- function(s){y <- yr[eff[,s]>0]; if(length(y)) min(y) else NA}
peak <- function(s){v <- eff[,s]; if(any(v>0)) yr[which.max(v)] else NA}
whal <- min(c(onset("baleen whales"), onset("sperm whales")), na.rm = TRUE)
krl  <- onset("antarctic krill")

mk <- function(S, B, xlab_txt, tag) {
  S <- S %>% mutate(panel = factor(panel, levels = panel_levels))
  B <- B %>% mutate(panel = factor(panel, levels = panel_levels))
  ggplot(S, aes(Year, med)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40",
               linewidth = 0.4) +
    geom_hline(data = B, aes(yintercept = ref), inherit.aes = FALSE,
               colour = "red", linetype = "dashed", linewidth = 0.4, alpha = 0.85) +
    geom_hline(data = B, aes(yintercept = -ref), inherit.aes = FALSE,
               colour = "red", linetype = "dashed", linewidth = 0.4, alpha = 0.85) +
    geom_vline(xintercept = c(whal, krl), colour = "grey55",
               linetype = "dotted", linewidth = 0.4) +
    geom_ribbon(aes(ymin = lo, ymax = hi, fill = panel), alpha = 0.28) +
    geom_line(aes(colour = panel), linewidth = 0.75) +
    scale_fill_manual(values = panel_colors, guide = "none") +
    scale_colour_manual(values = panel_colors, guide = "none") +
    facet_wrap(~ panel, ncol = 1, scales = "free_y", strip.position = "right") +
    labs(x = "Year", y = "% change from unexploited", title = xlab_txt, tag = tag) +
    theme_bw(base_size = 9) +
    theme(panel.grid = element_blank(),
          strip.background = element_rect(fill = "grey92", colour = "grey60"),
          strip.text.y.right = element_text(angle = 0, size = 7),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
          plot.tag = element_text(face = "bold", size = 12))
}

fig <- mk(S_ab, B_ab, "Abundance change", "a") |
       mk(S_mw, B_mw, "Body size change", "b")

png_out <- guard(file.path(FIGS, sprintf("fig3_pctchange_%s.png", SUF)))
pdf_out <- guard(file.path(FIGS, sprintf("fig3_pctchange_%s.pdf", SUF)))
ggsave(png_out, fig, width = 11, height = 15.5, dpi = 300, limitsize = FALSE)
ggsave(pdf_out, fig, width = 11, height = 15.5, limitsize = FALSE)

out <- bind_rows(S_ab %>% mutate(metric = "abundance"),
                 S_mw %>% mutate(metric = "mean_mass")) %>%
  left_join(bind_rows(B_ab %>% mutate(metric = "abundance"),
                      B_mw %>% mutate(metric = "mean_mass")),
            by = c("panel", "metric"))
write.csv(out, guard(file.path(DATA, sprintf("fig3_pctchange_series_%s.csv", SUF))),
          row.names = FALSE)

cat("\n=== % change at 2010 (median across members) ===\n")
print(as.data.frame(out %>% filter(Year == 2010) %>%
  select(panel, metric, med, ref) %>% arrange(metric, med)), digits = 4,
  row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")
