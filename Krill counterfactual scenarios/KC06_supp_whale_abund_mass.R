# =============================================================================
# KC06 -- SUPPLEMENT: whale abundance and mean individual body size across the
# three exploitation scenarios
#
# A scenario-resolved extract of Figure 3
# (Manuscript scripts/F03_figure3_pctchange_rebuilt167.R). Same signal -- paired
# per-member percentage change from unexploited -- same two metric columns, same
# +-1 SD detectability band. Two changes: the rows are the two whale groups
# rather than all twelve functional groups, and COLOUR NOW ENCODES SCENARIO
# rather than panel, which is why the legend comes back (F03 sets
# legend.position = "none" because its colour is redundant with the strip).
#
# WHAT THIS FIGURE IS FOR, AND WHY THE ABUNDANCE COLUMN IS NEARLY FLAT.
# Whale BIOMASS is a numerical null across the three arms -- the per-member
# paired difference at 2010 is -1e-6 for large baleen and -1.9e-5 for minke,
# against a whaling effect of about -0.55. Krill fishing, even held at its 1979
# peak for 31 years, does not measurably impair modelled whale recovery. That is
# a result, not a rendering failure, and documenting it is this figure's job.
# The three lines are expected to superimpose; they are drawn at decreasing
# width so the overlap reads as concentric banding rather than a missing line.
#
# BODY SIZE IS NOT QUITE NULL. Mean individual mass does carry a small scenario
# signal, because whales feed on a krill stock that peak effort does deplete
# (2010 median krill standing stock: 1.031 of unexploited under observed
# history, 0.997 under sustained peak effort). Whale intake responds; whale
# numbers do not. The printed table below quantifies both.
#
# INPUT KC05_abund_mass_n162.rds -- run KC05 first. Its G1 gate proves the
# re-projection reproduces KC02's biomass exactly, which is what makes the
# abundance numbers (never stored by KC02) trustworthy.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC06_supp_whale_abund_mass.R"
# ENV    KC_SUF, KC_FORCE
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(grid)
})

OUT_LARGE <- "Output_large_files/wmin_test"
KC_ROOT <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
ANA <- file.path(KC_ROOT, "analysis"); FIG <- file.path(KC_ROOT, "figures")
for (d in c(ANA, FIG)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
SUF <- Sys.getenv("KC_SUF", ""); FORCE <- nzchar(Sys.getenv("KC_FORCE"))
# KC_IQR_LINES: draw the two counterfactual arms' interquartile range as
# colour-matched dashed bounds rather than additional filled ribbons. See KC04
# for the reasoning. Output gets its own "_iqr" stem.
IQR_LINES <- nzchar(Sys.getenv("KC_IQR_LINES"))
BASELINE <- 1841:2010

IN <- Sys.getenv("KC_IN5", file.path(OUT_LARGE, "KC05_abund_mass_n162.rds"))
if (!file.exists(IN))
  stop("missing ", IN, " -- run KC05_extract_abund_mass.R first", call. = FALSE)
K5 <- readRDS(IN)
# KC20 carries the same schema under different names ($biomass rather than
# $abund_mass), so a KC20 extraction can be read directly. The columns are
# identical -- sim_index, arm, Year, Species, Abundance, Biomass.
if (is.null(K5$abund_mass) && !is.null(K5$biomass)) {
  K5$abund_mass <- K5$biomass
  cat("  (reading a KC20 extraction: $biomass used as $abund_mass)\n")
}
AM <- K5$abund_mass; MEM <- K5$members; meta <- K5$meta
cat("=== KC06: supplementary whale abundance / body size by scenario ===\n")
# gate_G1_max_rel is a KC05 build diagnostic; a KC20 extraction does not carry it.
cat("input:", basename(IN),
    if (is.numeric(meta$gate_G1_max_rel))
      paste("| G1 max rel diff at build:", signif(meta$gate_G1_max_rel, 3))
    else "", "\n")

# USABLE members only -- one divergent member can own most of an across-member
# sum, and its paired change is meaningless rather than merely extreme.
# `stable` alone is a no-op where every member is admissible but not on
# ensembles where the reproduction treatment pushes erepro >= 1 (phase 88 is
# 521 stable of which 427 are usable). Matches KC15/KC16b/KC18.
adm <- if ("n_erepro_ge1" %in% names(MEM)) MEM$n_erepro_ge1 == 0 else TRUE
keep <- MEM$sim_index[MEM$stable & adm]
AM <- AM[AM$sim_index %in% keep, ]
n_mem <- length(unique(AM$sim_index))
cat("members:", nrow(MEM), "| stable:", n_mem, "\n")
# 156 is a regression check on the published phase-54 build only. A different
# reference model legitimately converges a different number of members, so the
# count is reported rather than enforced there.
CANON <- basename(IN) == "KC05_abund_mass_n162.rds"
if (n_mem != 156) {
  if (CANON) stop("expected 156 stable members, got ", n_mem, call. = FALSE)
  cat("  NOTE: the published build had 156 stable members\n")
}

# --- panels: the whale groups, named as in F03 -------------------------------
# KC_WHALES selects them. The default is the published pair; sperm whales are
# available because they are the third whaled group and the one the model
# depletes hardest (median 2010 biomass 0.003 of 1841 on the phase-88 ensemble),
# so leaving them out understates what whaling did.
ALL_WHALE_PANELS <- c("baleen whales" = "Large baleen whales",
                      "minke whales"  = "Minke whales",
                      "sperm whales"  = "Sperm whales")
WH_SEL <- trimws(strsplit(Sys.getenv("KC_WHALES",
                                     "baleen whales,minke whales"), ",")[[1]])
WH_SEL <- WH_SEL[nzchar(WH_SEL)]
if (length(setdiff(WH_SEL, names(ALL_WHALE_PANELS))))
  stop("KC_WHALES: unknown group(s) ",
       paste(setdiff(WH_SEL, names(ALL_WHALE_PANELS)), collapse = ", "),
       " -- choose from ", paste(names(ALL_WHALE_PANELS), collapse = ", "),
       call. = FALSE)
sp2panel <- ALL_WHALE_PANELS[WH_SEL]
panel_levels <- unname(sp2panel)
cat("whale groups:", paste(WH_SEL, collapse = ", "), "\n")

ARMS <- c("no_krill", "exploited", "peak_krill")   # diverging order
SCEN <- c(no_krill = "No krill fishing", exploited = "Observed history",
          peak_krill = sprintf("Peak krill effort held from %d", meta$peak_year))
# Same diverging encoding as KC04: two hues plus a neutral midpoint, because the
# arms bracket the observed history by krill-fishing intensity. The chromatic
# poles clear every hard gate on the all-pairs list against a white surface
# (OKLab dE x100: CVD 21.6 protan, normal vision 32.3).
SCEN_COL <- setNames(c("#2a78d6", "#0b0b0b", "#e34948"), unname(SCEN))
LW <- c(exploited = 2.2, peak_krill = 1.3, no_krill = 0.6)   # draw order below

# --- paired percentage change from unexploited (F03:100-111) -----------------
# Formed WITHIN each member and year, then summarised across members. Building
# it from across-member means instead would leak the cross-calibration spread
# into the signal.
P <- AM %>%
  mutate(panel = unname(sp2panel[Species])) %>% filter(!is.na(panel)) %>%
  group_by(sim_index, Year, panel, arm) %>%
  summarise(Abundance = sum(Abundance), Biomass = sum(Biomass),
            .groups = "drop") %>%
  mutate(MeanMass = ifelse(Abundance > 0, Biomass / Abundance, NA_real_))

UN <- P %>% filter(arm == "unexploited") %>%
  select(sim_index, Year, panel, Abundance_u = Abundance,
         MeanMass_u = MeanMass)
pair <- P %>% filter(arm %in% ARMS) %>%
  inner_join(UN, by = c("sim_index", "Year", "panel")) %>%
  mutate(pct_abund = 100 * (Abundance - Abundance_u) / Abundance_u,
         pct_mass  = 100 * (MeanMass  - MeanMass_u)  / MeanMass_u)

summ <- function(v) pair %>% group_by(panel, arm, Year) %>%
  summarise(med = median(.data[[v]], na.rm = TRUE),
            lo = quantile(.data[[v]], 0.25, na.rm = TRUE),
            hi = quantile(.data[[v]], 0.75, na.rm = TRUE), .groups = "drop") %>%
  mutate(panel = factor(panel, levels = panel_levels),
         scen = factor(SCEN[arm], levels = unname(SCEN)))
S_ab <- summ("pct_abund"); S_mw <- summ("pct_mass")

# --- +-1 SD detectability band, per panel and metric (F03:114-120) -----------
# NOTE ON COLOUR: F03 draws this band in red. Here red is the peak-krill
# scenario, so a red dashed reference line and a red dashed scenario bound would
# be the same ink meaning two different things. Drawn in grey35 instead, which
# also matches KC04's band.
# Temporal SD of the ensemble-mean UNEXPLOITED trajectory over 1841-2010,
# divided by its own mean, as a percentage. The noise deliberately does NOT use
# the inter-calibration spread: the plotted signal is already paired, so that
# spread has been removed, and re-introducing it would inflate the band by one
# to two orders of magnitude and render even large declines "non-significant".
band <- function(metric) P %>% filter(arm == "unexploited",
                                      Year %in% BASELINE) %>%
  group_by(panel, Year) %>%
  summarise(mu = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
  group_by(panel) %>%
  summarise(ref_pct = 100 * sd(mu, na.rm = TRUE) / mean(mu, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(panel = factor(panel, levels = panel_levels))
B_ab <- band("Abundance"); B_mw <- band("MeanMass")
cat("\n=== +-1 SD detectability bands (unexploited 1841-2010) ===\n")
print(as.data.frame(bind_rows(B_ab %>% mutate(metric = "abundance"),
                              B_mw %>% mutate(metric = "mean_mass"))),
      digits = 4, row.names = FALSE)

# --- era markers (F03:122-134 plus the event this figure is about) -----------
eff <- readRDS("effort_array_1841_2010.rds"); yr <- as.numeric(rownames(eff))
onset <- function(s) { y <- yr[eff[, s] > 0]; if (length(y)) min(y) else NA }
peak  <- function(s) { v <- eff[, s]; if (any(v > 0)) yr[which.max(v)] else NA }
event_df <- data.frame(
  Year = c(min(c(onset("baleen whales"), onset("sperm whales")), na.rm = TRUE),
           onset("antarctic krill")),
  label = c("Whaling\nstarts", "Krill fishing\nstarts"))
event_lab <- event_df %>%
  mutate(panel = factor(panel_levels[1], levels = panel_levels))
# F03 places a per-panel peak-effort line from its own gear. Peak krill fishing
# is added to BOTH rows -- it is the event this figure is about, and F03's
# species_events_df would never put it in a whale panel.
sp_ev <- bind_rows(
  data.frame(panel = "Large baleen whales", Year = peak("baleen whales"),
             lab = "Peak baleen whaling"),
  data.frame(panel = "Minke whales", Year = peak("minke whales"),
             lab = "Peak minke whaling"),
  data.frame(panel = panel_levels, Year = peak("antarctic krill"),
             lab = "Peak krill fishing")) %>%
  mutate(panel = factor(panel, levels = panel_levels))

ymid <- function(S) S %>% group_by(panel) %>%
  summarise(y_mid = (max(hi, na.rm = TRUE) + min(lo, na.rm = TRUE)) / 2,
            .groups = "drop")
ev_ab <- left_join(sp_ev, ymid(S_ab), by = "panel")
ev_mw <- left_join(sp_ev, ymid(S_mw), by = "panel")

# --- build, formatting transcribed from F03:176-235 --------------------------
theme_1col_base <- theme_bw(base_size = 10) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.y  = unit(0.4, "lines"),
        axis.title       = element_text(size = 11),
        axis.text        = element_text(size = 9))

build <- function(S, ref_df, ev_df, y_lab, show_strips, col_label) {
  S <- mutate(S, col_facet = factor(col_label, levels = col_label))
  RIB <- filter(S, arm == "exploited")
  # `group` keeps the lower and upper bound as two separate paths -- without it
  # ggplot joins hi back to lo at the series end and draws a closed loop.
  IQRL <- S %>% filter(IQR_LINES, arm %in% setdiff(ARMS, "exploited")) %>%
    tidyr::pivot_longer(c(lo, hi), names_to = "bound", values_to = "yb")
  # One layer per arm at decreasing width, in fixed order. Equal widths would
  # let the last-drawn arm silently hide the other two, which here coincide.
  lay <- function(a) geom_line(data = filter(S, arm == a),
                               aes(Year, med, colour = scen), linewidth = LW[[a]])
  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40",
               linewidth = 0.5) +
    # observed-arm envelope only: three overlapping IQR ribbons would read as
    # one cloud, and the scenario contrast is paired and far tighter anyway
    geom_ribbon(data = RIB, aes(Year, ymin = lo, ymax = hi), fill = "grey55",
                alpha = 0.22) +
    geom_hline(data = ref_df, aes(yintercept = ref_pct), colour = "grey35",
               linetype = "dashed", linewidth = 0.45, alpha = 0.9) +
    geom_hline(data = ref_df, aes(yintercept = -ref_pct), colour = "grey35",
               linetype = "dashed", linewidth = 0.45, alpha = 0.9) +
    geom_line(data = IQRL, aes(Year, yb, colour = scen,
                               group = interaction(scen, bound)),
              linetype = "dashed", linewidth = 0.4, show.legend = FALSE) +
    lay("exploited") + lay("peak_krill") + lay("no_krill") +
    geom_vline(data = event_df, aes(xintercept = Year), linetype = "dotted",
               colour = "grey50", linewidth = 0.5) +
    geom_text(data = event_lab, aes(x = Year, label = label), y = -Inf,
              hjust = 1, vjust = -0.3, size = 3.2, colour = "grey35") +
    geom_vline(data = ev_df, aes(xintercept = Year), linetype = "dashed",
               colour = "grey35", linewidth = 0.45, alpha = 0.8) +
    geom_text(data = ev_df, aes(x = Year, y = y_mid, label = lab), angle = 90,
              hjust = 0.5, vjust = 1.3, size = 3.1, colour = "grey10") +
    facet_grid(rows = vars(panel), cols = vars(col_facet), scales = "free_y") +
    coord_cartesian(xlim = c(1925, NA)) +
    scale_colour_manual(values = SCEN_COL, name = "Scenario",
                        limits = unname(SCEN),
                        guide = guide_legend(override.aes = list(linewidth = 1.4))) +
    scale_x_continuous(breaks = seq(1930, 2010, 20)) +
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

p_ab <- build(S_ab, B_ab, ev_ab, "% change from unexploited",
              show_strips = FALSE, col_label = "Abundance change")
p_mw <- build(S_mw, B_mw, ev_mw, "% change from unexploited",
              show_strips = TRUE,  col_label = "Body size change")

fig <- (p_ab + p_mw + plot_layout(ncol = 2, widths = c(1, 1.05),
                                  guides = "collect") +
  plot_annotation(tag_levels = "a")) &
  theme(plot.tag = element_text(face = "bold", size = 12),
        legend.position = "bottom", legend.title = element_text(face = "bold"))

stem <- paste0("KC_supp_whale_abund_mass", SUF, if (IQR_LINES) "_iqr" else "")
for (ext in c("png", "pdf")) {
  f <- file.path(FIG, paste0(stem, ".", ext))
  if (file.exists(f) && !FORCE)
    stop("refusing to overwrite: ", f, "\n  set KC_FORCE=1", call. = FALSE)
}
suppressWarnings({
  ggsave(file.path(FIG, paste0(stem, ".png")), fig, width = 11, height = 9,
         dpi = 300)
  ggsave(file.path(FIG, paste0(stem, ".pdf")), fig, width = 11, height = 9)
})
cat("\nwrote", paste0(stem, ".{png,pdf}"), "(", n_mem, "members )\n")

# =============================================================================
# the null, quantified
# =============================================================================
# Per-member paired contrasts against the observed history, 2001-2010. If the
# abundance column looks flat, this is what says how flat.
PC <- pair %>% filter(Year %in% 2001:2010) %>%
  select(sim_index, panel, arm, Abundance, MeanMass) %>%
  pivot_wider(names_from = arm, values_from = c(Abundance, MeanMass),
              values_fn = mean) %>%
  transmute(sim_index, panel,
            ab_nokr = 100 * (Abundance_no_krill / Abundance_exploited - 1),
            ab_peak = 100 * (Abundance_peak_krill / Abundance_exploited - 1),
            mm_nokr = 100 * (MeanMass_no_krill / MeanMass_exploited - 1),
            mm_peak = 100 * (MeanMass_peak_krill / MeanMass_exploited - 1))

TAB <- PC %>% group_by(panel) %>% summarise(
  `abund: no krill vs observed` = median(ab_nokr),
  `abund: peak vs observed`     = median(ab_peak),
  `abund: peak worst member`    = min(ab_peak),
  `size: no krill vs observed`  = median(mm_nokr),
  `size: peak vs observed`      = median(mm_peak),
  `size: peak worst member`     = min(mm_peak),
  `size: peak members < 0`      = sum(mm_peak < 0), n = n(), .groups = "drop")
cat("\n=== paired % change vs OBSERVED HISTORY, 2001-2010 (median across members) ===\n")
print(as.data.frame(TAB %>% mutate(across(where(is.numeric), ~signif(.x, 3)))),
      row.names = FALSE)

cat("\n=== paired % change vs UNEXPLOITED at 2010 (what the figure plots) ===\n")
print(as.data.frame(
  bind_rows(S_ab %>% mutate(metric = "abundance"),
            S_mw %>% mutate(metric = "mean_mass")) %>%
    filter(Year == 2010) %>%
    transmute(panel, metric, scen, med = round(med, 3),
              IQR = sprintf("%.2f to %.2f", lo, hi)) %>%
    arrange(metric, panel, scen)), row.names = FALSE)

write.csv(bind_rows(S_ab %>% mutate(metric = "abundance"),
                    S_mw %>% mutate(metric = "mean_mass")) %>%
            select(panel, metric, scen, arm, Year, med, lo, hi),
          file.path(ANA, paste0("KC06_whale_abund_mass_series", SUF, ".csv")),
          row.names = FALSE)
write.csv(TAB, file.path(ANA, paste0("KC06_paired_vs_observed", SUF, ".csv")),
          row.names = FALSE)
cat("\nKC06 complete.\n")