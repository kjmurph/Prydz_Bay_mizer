# =============================================================================
# KC04 -- Figure 4, faceted by PREDATOR GROUP, three exploitation scenarios
#
# SUPERSEDES KC03_figure4_faceted.R, which faceted by SCENARIO and kept colour =
# predator group. That layout does not work: each panel repeats the same
# three-group cluster at y ~ 1, and the comparison the figure exists to make --
# scenario against scenario -- has to be carried across panels by eye.
#
# This inverts it. Facet by predator group, colour by scenario, free y per
# panel. The comparison now happens inside a single panel, and the whale panel
# gets the room its 0.39-1.00 range needs without flattening the +-3% signal in
# the other two.
#
# THREE THINGS MEASURED ON THIS DATA THAT DICTATE THE GEOMETRY. All from the 156
# stable members of KC02_n167_from1841_scenarios.rds; the script re-checks each.
#
#   1. THE MARGINAL SPREAD SWAMPS THE SCENARIO SEPARATION. For whales the
#      across-member IQR is 0.30-0.68 (width 0.38) against a 0.07 gap between
#      scenario medians. Three overlapping IQR ribbons read as one cloud. So the
#      ribbon here is the OBSERVED ARM ONLY, in grey, as the ensemble envelope --
#      and it is NOT the uncertainty on the scenario contrast. That uncertainty
#      is paired and much tighter; it is written to KC04_paired_contrast.csv and
#      printed below, because it does not survive being drawn on this axis.
#
#   2. `no_krill` AND `exploited` COINCIDE ALMOST EXACTLY. Median per-member
#      no_krill / observed is 1.00001-1.00007 in every group. Two of the three
#      lines are superimposed, and that IS the result -- the observed krill
#      fishery left no trace. Drawn at equal linewidth the last one plotted would
#      silently hide the other two, so they are drawn at DECREASING width in a
#      fixed order (KC01_figures.R:136-138 uses the same device): observed 2.4 ->
#      peak 1.4 -> no_krill 0.7. Coincidence then reads as concentric banding
#      rather than a missing line.
#
#   3. THE PANELS ARE NOT ON A COMMON Y SCALE. Required -- the whale panel spans
#      0.18-1.06 and the predator panel 0.94-1.05 -- but it means a visually
#      bigger drop is not a bigger effect. The +-1 SD band in each panel is the
#      per-panel yardstick. Say so in the caption.
#
# COLOUR. The three arms are a DIVERGING structure -- no krill fishing <-
# observed history -> peak krill effort -- so they take the diverging form: two
# hues plus a neutral midpoint, not three categorical hues. Blue <- ink -> red.
# The two chromatic poles clear every hard gate on the all-pairs list against a
# white surface (OKLab dE x100: CVD 21.6 protan, normal vision 32.3; both >= 3:1
# contrast). The achromatic midpoint is correct by construction for a diverging
# encoding. This replaces KC01's green/red pair, which is a red/green confusion
# for deuteranopes.
#
# GEOMETRY OTHERWISE TRANSCRIBED FROM KC03 (which transcribes F04): the per
# member-year ratio, the era markers derived from the effort array, and the
# canonical +-1 SD natural-variability band.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC04_figure4_by_group.R"
# ENV    KC_IN (default KC02_n167_from1841_scenarios.rds), KC_SUF, KC_FORCE
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
})

KC_ROOT <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
ANA <- file.path(KC_ROOT, "analysis"); FIG <- file.path(KC_ROOT, "figures")
dir.create(FIG, recursive = TRUE, showWarnings = FALSE)
IN  <- Sys.getenv("KC_IN", "KC02_n167_from1841_scenarios.rds")
SUF <- Sys.getenv("KC_SUF", "")
FORCE <- nzchar(Sys.getenv("KC_FORCE"))
# KC_IQR_LINES adds the two counterfactual arms' interquartile range as
# colour-matched DASHED BOUNDS rather than a third and fourth filled ribbon.
# Filling them would read as one cloud -- the across-member spread is 3-5x the
# scenario separation -- but the outer bounds alone stay legible, and they show
# that the counterfactual spreads are wide and heavily overlapping, which the
# medians alone do not convey. Output gets its own "_iqr" stem.
IQR_LINES <- nzchar(Sys.getenv("KC_IQR_LINES"))

D <- readRDS(file.path(ANA, IN))
KR <- D$krill; meta <- D$meta
cat("=== KC04: Figure 4 faceted by predator group ===\n")
cat("input:", IN, "| params:", basename(meta$params), "\n")

# stable members only -- a divergent member's ratio is meaningless, not merely
# extreme, and one of them can own most of an across-member sum.
if (!is.null(D$members)) {
  keep <- D$members$sim_index[D$members$stable]
  cat("members:", nrow(D$members), "| stable:", length(keep), "\n")
  if (!length(keep)) stop("no stable members", call. = FALSE)
  KR <- KR[KR$sim_index %in% keep, ]
}
n_mem <- length(unique(KR$sim_index))

FISHES <- c("mesopelagic fishes", "bathypelagic fishes",
            "shelf and coastal fishes", "toothfishes")
WHALES <- c("baleen whales", "minke whales")
ALL_PRED <- sort(unique(KR$Species))
GRP_LEV <- c("All predators", "Large baleen + minke whales", "All fishes")
# Diverging order: less exploitation -> reference -> more exploitation. The
# legend then carries the ordering of krill-fishing intensity by itself.
ARMS <- c("no_krill", "exploited", "peak_krill")
SCEN <- c(no_krill = "No krill fishing", exploited = "Observed history",
          peak_krill = sprintf("Peak krill effort held from %d", meta$peak_year))
SCEN_COL <- c("No krill fishing" = "#2a78d6",   # cool pole
              "Observed history" = "#0b0b0b",   # neutral midpoint (primary ink)
              "Peak krill effort held from 1979" = "#e34948")  # warm pole
names(SCEN_COL)[3] <- unname(SCEN["peak_krill"])   # keep in step with peak_year

# --- era markers, derived from the effort array (KC03:66-79) ------------------
eff <- readRDS("effort_array_1841_2010.rds")
yr <- as.numeric(rownames(eff))
onset <- function(s) { y <- yr[eff[, s] > 0]; if (length(y)) min(y) else NA }
peak  <- function(s) { v <- eff[, s]; if (any(v > 0)) yr[which.max(v)] else NA }
last  <- function(s) { y <- yr[eff[, s] > 0]; if (length(y)) max(y) else NA }
whaling_start <- min(c(onset("baleen whales"), onset("sperm whales")),
                     na.rm = TRUE)
krill_start <- onset("antarctic krill"); krill_end <- last("antarctic krill")
pk <- c("Peak baleen whaling" = peak("baleen whales"),
        "Peak sperm whaling"  = peak("sperm whales"),
        "Peak minke whaling"  = peak("minke whales"),
        "Peak krill fishing"  = peak("antarctic krill"))
pk <- pk[!is.na(pk)]

# --- one wide table of RAW consumption, per member-year-group -----------------
# Everything downstream comes off this: the plotted ratios (arm / unexploited),
# and the paired contrasts (arm / observed). Aggregating once means the two can
# never drift apart.
grp_wide <- function(species, label) {
  KR %>% filter(Species %in% species) %>%
    group_by(sim_index, arm, Year) %>%
    summarise(cons = sum(krill_consumed), .groups = "drop") %>%
    pivot_wider(names_from = arm, values_from = cons) %>%
    mutate(group = label)
}
CW <- bind_rows(grp_wide(ALL_PRED, GRP_LEV[1]), grp_wide(WHALES, GRP_LEV[2]),
                grp_wide(FISHES, GRP_LEV[3])) %>%
  filter(is.finite(unexploited), unexploited > 0)

# ratio to that member's OWN unexploited arm -- paired within member, exactly as
# F04 and KC03 form it
R <- CW %>%
  pivot_longer(all_of(ARMS), names_to = "arm", values_to = "cons") %>%
  filter(is.finite(cons)) %>%
  mutate(ratio = cons / unexploited)

S <- R %>% group_by(arm, group, Year) %>%
  summarise(med = median(ratio, na.rm = TRUE),
            lo = quantile(ratio, 0.25, na.rm = TRUE),
            hi = quantile(ratio, 0.75, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = factor(group, levels = GRP_LEV),
         scen = factor(SCEN[arm], levels = unname(SCEN)))

# --- per-group +-1 SD natural-variability band (KC03:107-118, verbatim) -------
# The canonical SNR denominator: the temporal SD of the across-member MEAN
# unexploited trajectory, divided by that curve's own mean to put it on the
# multiplicative ratio scale. One scalar per group, scenario-independent by
# construction -- the unexploited arm is the same run in every scenario.
BASE_YEARS <- sort(unique(KR$Year))
sd_band <- function(species) {
  u <- KR %>% filter(Species %in% species, arm == "unexploited") %>%
    group_by(sim_index, Year) %>%
    summarise(cons = sum(krill_consumed), .groups = "drop") %>%
    group_by(Year) %>% summarise(mu = mean(cons), .groups = "drop")
  sigma <- sd(u$mu) / mean(u$mu)
  data.frame(lo = max(1 - sigma, 0), hi = 1 + sigma, sigma = sigma)
}
BAND <- bind_rows(cbind(group = GRP_LEV[1], sd_band(ALL_PRED)),
                  cbind(group = GRP_LEV[2], sd_band(WHALES)),
                  cbind(group = GRP_LEV[3], sd_band(FISHES))) %>%
  mutate(group = factor(group, levels = GRP_LEV))
MATCHES_PUB <- min(BASE_YEARS) == 1841 && max(BASE_YEARS) == 2010
cat(sprintf("\n+-1 SD bands from the unexploited arm over %d-%d -- %s\n",
            min(BASE_YEARS), max(BASE_YEARS),
            if (MATCHES_PUB) "the published window, like for like." else
              paste("NOT the published 1841-2010 window; rerun KC02 with",
                    "KC_DIET_FROM=1841 to match it.")))
print(as.data.frame(BAND), digits = 4, row.names = FALSE)

# =============================================================================
# FATAL GATES -- this is the same data through a new layout, so any disagreement
# with KC03 is a restructuring bug, not a result. Checked before anything is
# written.
# =============================================================================
# The reference values below describe the PUBLISHED phase-54 build. They are a
# regression gate for that input and must stay fatal there. Pointed at a
# different model -- e.g. the phase-55 whale-resource variant -- a disagreement
# is the result, not a bug, so the same numbers are printed as a comparison
# instead of aborting. CANON decides which.
CANON <- basename(IN) == "KC02_n167_from1841_scenarios.rds"
gate <- function(what, got, want, tol) {
  d <- max(abs(got - want))
  ok <- d <= tol
  cat(sprintf("  [%s] %-34s max |diff| %.2e (tol %.0e)\n",
              if (ok) "PASS" else if (CANON) "FAIL" else "DIFF", what, d, tol))
  if (!ok && CANON) stop("gate failed: ", what, call. = FALSE)
}
cat("\n=== ", if (CANON) "gates" else "comparison",
    " against the published phase-54 build ===\n", sep = "")
if (!CANON)
  cat("  input is not the canonical build -- differences are reported, not fatal\n")
if (n_mem != 156) {
  if (CANON) stop("expected 156 stable members, got ", n_mem, call. = FALSE)
  cat(sprintf("  [DIFF] %-34s %d (published build had 156)\n",
              "stable members", n_mem))
} else cat(sprintf("  [PASS] %-34s %d\n", "stable members", n_mem))
# The reference sigmas are quoted to 4 dp, so 5e-5 is exactly their rounding
# half-width -- not an arbitrary tolerance.
gate("+-1 SD sigma, three groups", BAND$sigma, c(0.0126, 0.0297, 0.0136), 5e-5)

# KC03's own 2001-2010 summary route: across-member median per year, then the
# mean over years. Reproduced here exactly so the gate is a real reproduction.
REF_2001 <- S %>% filter(Year %in% 2001:2010) %>%
  group_by(group, arm) %>% summarise(med = mean(med), .groups = "drop")
# Joined on (group, arm), never positional -- an expected vector in the wrong
# order fails as a numeric disagreement and looks like a real one. It already
# did once: arrange() sorts `arm` alphabetically, not in the diverging order the
# rest of this script uses.
KC03_REF <- tribble(
  ~group,                        ~arm,         ~want,
  "All predators",               "no_krill",   0.9965,
  "All predators",               "exploited",  0.9958,
  "All predators",               "peak_krill", 0.9660,
  "Large baleen + minke whales", "no_krill",   0.4656,
  "Large baleen + minke whales", "exploited",  0.4656,
  "Large baleen + minke whales", "peak_krill", 0.3935,
  "All fishes",                  "no_krill",   1.0223,
  "All fishes",                  "exploited",  1.0205,
  "All fishes",                  "peak_krill", 0.9858)
CHK <- REF_2001 %>% mutate(group = as.character(group)) %>%
  inner_join(KC03_REF, by = c("group", "arm"))
if (nrow(CHK) != nrow(KC03_REF))
  stop("gate failed: only ", nrow(CHK), " of ", nrow(KC03_REF),
       " group/arm combinations matched", call. = FALSE)
gate("2001-2010 difference of medians", CHK$med, CHK$want, 5e-4)

# =============================================================================
# THE PAIRED CONTRAST -- the statistic the figure cannot draw
# =============================================================================
# The arms are paired: same member, same draws, same spin-up, same unexploited
# denominator. So the scenario effect is a WITHIN-member contrast, and its
# uncertainty is the spread of that contrast, not the marginal spread of the
# levels. The two disagree badly for whales: the gap between the median lines is
# -0.0721 while the median member moves only -0.0098, because peak_krill
# RE-RANKS the ensemble (members with higher whale ratios take bigger absolute
# hits). Both are reported so the 7.4x gap can never be quoted by accident.
PC <- CW %>%
  transmute(sim_index, Year, group,
            rel_no_krill   = no_krill   / exploited,
            rel_peak_krill = peak_krill / exploited,
            dif_no_krill   = (no_krill   - exploited) / unexploited,
            dif_peak_krill = (peak_krill - exploited) / unexploited)

PC_2001 <- PC %>% filter(Year %in% 2001:2010) %>%
  group_by(sim_index, group) %>%
  summarise(across(starts_with(c("rel_", "dif_")), mean), .groups = "drop")

SUMM <- PC_2001 %>% group_by(group) %>% summarise(
  paired_diff_med  = median(dif_peak_krill),
  paired_diff_q25  = quantile(dif_peak_krill, 0.25),
  paired_diff_q75  = quantile(dif_peak_krill, 0.75),
  paired_ratio_med = median(rel_peak_krill),
  paired_ratio_q25 = quantile(rel_peak_krill, 0.25),
  paired_ratio_q75 = quantile(rel_peak_krill, 0.75),
  n_below_1        = sum(rel_peak_krill < 1), n = n(),
  nokrill_ratio_med = median(rel_no_krill),
  nokrill_gt_1.001 = sum(rel_no_krill > 1.001),
  nokrill_gt_1.05  = sum(rel_no_krill > 1.05), .groups = "drop") %>%
  mutate(group = factor(group, levels = GRP_LEV)) %>% arrange(group)

DOM <- REF_2001 %>% pivot_wider(names_from = arm, values_from = med) %>%
  transmute(group, diff_of_medians = peak_krill - exploited)

cat("\n=== peak krill effort vs observed history, 2001-2010, TWO summaries ===\n")
cat("    (the figure's median lines show the first; the second is the typical member)\n")
print(as.data.frame(
  left_join(DOM, SUMM, by = "group") %>%
    transmute(group,
              `difference of medians` = round(diff_of_medians, 4),
              `median paired diff` = round(paired_diff_med, 4),
              `median paired ratio` = round(paired_ratio_med, 4),
              `paired ratio IQR` = sprintf("%.3f-%.3f", paired_ratio_q25,
                                           paired_ratio_q75),
              `below 1` = sprintf("%d/%d", n_below_1, n))),
  row.names = FALSE)

cat("\n=== the observed krill fishery: no_krill / observed, 2001-2010 ===\n")
print(as.data.frame(SUMM %>% transmute(group,
  `median` = signif(nokrill_ratio_med, 6),
  `members > +0.1%` = sprintf("%d/%d", nokrill_gt_1.001, n),
  `members > +5%`   = sprintf("%d/%d", nokrill_gt_1.05, n))), row.names = FALSE)

# =============================================================================
# figure
# =============================================================================
X_MIN <- 1925; X_MAX <- 2012
# Per-panel limits, set with a geom_blank frame. free_y alone leaves no room for
# the rotated peak-effort labels, and lets the whale panel's floor float.
# Arms whose spread is actually drawn, and therefore has to fit inside the
# panel. With KC_IQR_LINES the counterfactual bounds reach well below the
# observed ribbon -- peak krill fishing bottoms out near 0.25 for whales -- so
# the published hand-tuned limits no longer apply and are derived instead.
ENV_ARMS <- if (IQR_LINES) ARMS else "exploited"
if (CANON && !IQR_LINES) {
  # Hand-tuned to the published build so the figure is reproduced exactly.
  LIMS <- data.frame(
    group = factor(rep(GRP_LEV, each = 2), levels = GRP_LEV),
    y = c(0.94, 1.05, 0.18, 1.06, 0.96, 1.09), Year = 1930)
} else {
  # A different model can plot outside the published frame. Derive the limits
  # from what is actually drawn -- medians, the observed ribbon, and the band --
  # leaving headroom below for the rotated peak-effort labels.
  LIMS <- S %>% left_join(BAND, by = "group", suffix = c("", ".b")) %>%
    group_by(group) %>%
    summarise(dlo = min(c(med, lo[arm %in% ENV_ARMS], lo.b), na.rm = TRUE),
              dhi = max(c(med, hi[arm %in% ENV_ARMS], hi.b), na.rm = TRUE),
              .groups = "drop") %>%
    # Extra headroom above when the dashed bounds are drawn: they reach much
    # closer to the panel top than the medians do, and the two-line event labels
    # hang off it. Without this the "End of krill fishing" label sits on the
    # no-krill upper bound.
    mutate(span = dhi - dlo,
           ylo = dlo - 0.22 * span,
           yhi = dhi + (if (IQR_LINES) 0.20 else 0.05) * span) %>%
    select(group, ylo, yhi) %>%
    pivot_longer(c(ylo, yhi), values_to = "y") %>%
    transmute(group, y, Year = 1930)
}
YR <- LIMS %>% group_by(group) %>%
  summarise(ylo = min(y), yhi = max(y), .groups = "drop")
# Nothing plotted may fall outside its panel: silent clipping would delete a
# scenario line and leave a figure that still looks plausible.
CLIP <- S %>% left_join(YR, by = "group") %>%
  filter(med < ylo | med > yhi |
           (arm %in% ENV_ARMS & (lo < ylo | hi > yhi)))
if (nrow(CLIP))
  stop("y limits clip ", nrow(CLIP), " plotted points, e.g. ",
       CLIP$group[1], " ", CLIP$Year[1], " med ", signif(CLIP$med[1], 4),
       " outside [", CLIP$ylo[1], ", ", CLIP$yhi[1], "]", call. = FALSE)

# Event text on the top facet only; the rotated peak-effort labels need a
# PER-PANEL y now that the scales differ (KC03 could use a fixed y = 0.02).
TOP <- factor(GRP_LEV[1], levels = GRP_LEV)
ev_lab <- data.frame(
  x = c(whaling_start, krill_start, krill_end),
  lab = c("Whaling\nstarts", "Krill fishing\nstarts", "End of\nkrill fishing"),
  group = TOP)
pk_lab <- YR %>% tidyr::crossing(data.frame(x = as.numeric(pk),
                                            lab = names(pk))) %>%
  mutate(y = ylo + 0.02 * (yhi - ylo))
band_lab <- BAND %>% filter(group == TOP) %>% mutate(x = X_MAX, lab = "±1 SD")

# The observed-arm envelope. Grey, under everything, and named in the legend as
# what it is so it cannot be mistaken for the scenario uncertainty.
RIB <- S %>% filter(arm == "exploited") %>%
  mutate(env = if (IQR_LINES)
    "Interquartile range across members (shaded: observed; dashed: counterfactuals)"
    else "Observed history, interquartile range across members")

# Counterfactual IQRs as colour-matched dashed bounds. `group` keeps the lower
# and upper bound as two separate paths -- without it ggplot joins hi back to lo
# at the series end and draws a closed loop.
IQRL <- S %>% filter(IQR_LINES, arm %in% setdiff(ARMS, "exploited")) %>%
  pivot_longer(c(lo, hi), names_to = "bound", values_to = "y")

# One geom_line per arm, in fixed draw order at decreasing width. override.aes
# forces a uniform key so the legend encodes COLOUR only -- linewidth here is an
# overplotting device, not a variable.
lay <- function(a, lw) geom_line(data = filter(S, arm == a),
                                 aes(Year, med, colour = scen), linewidth = lw)

p <- ggplot() +
  geom_blank(data = LIMS, aes(Year, y)) +
  geom_hline(yintercept = 1, colour = "grey45", linetype = "dashed",
             linewidth = 0.5) +
  geom_vline(xintercept = c(whaling_start, krill_start, krill_end),
             colour = "grey40", linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = as.numeric(pk), colour = "grey55",
             linetype = "dotted", linewidth = 0.4) +
  geom_ribbon(data = RIB, aes(Year, ymin = lo, ymax = hi, fill = env),
              alpha = 0.22, colour = NA) +
  # +-1 SD natural-variability reference, over the ribbon so it is not buried,
  # under the medians so those stay legible. One pair per panel now that each
  # panel holds a single group -- in the old layout three pairs clustered at y=1.
  geom_hline(data = BAND, aes(yintercept = lo), colour = "grey35",
             linetype = "dashed", linewidth = 0.45) +
  geom_hline(data = BAND, aes(yintercept = hi), colour = "grey35",
             linetype = "dashed", linewidth = 0.45) +
  geom_line(data = IQRL, aes(Year, y, colour = scen,
                             group = interaction(scen, bound)),
            linetype = "dashed", linewidth = 0.45, show.legend = FALSE) +
  lay("exploited", 2.4) + lay("peak_krill", 1.4) + lay("no_krill", 0.7) +
  geom_text(data = ev_lab, aes(x = x, y = Inf, label = lab),
            hjust = -0.06, vjust = 1.25, size = 3.1, colour = "grey30") +
  geom_text(data = pk_lab, aes(x = x, y = y, label = lab), angle = 90,
            hjust = 0, vjust = -0.3, size = 2.7, colour = "grey45") +
  geom_text(data = band_lab, aes(x = x, y = hi, label = lab), hjust = 1.15,
            vjust = -0.6, size = 2.9, colour = "grey35") +
  facet_wrap(~group, ncol = 1, scales = "free_y") +
  # `limits` is not redundant: each geom_line layer carries a single scenario,
  # so without it the legend takes the LAYER order (observed, peak, no krill)
  # rather than the diverging order the encoding depends on.
  scale_colour_manual(values = SCEN_COL, name = "Scenario",
                      limits = unname(SCEN),
                      guide = guide_legend(order = 1,
                        override.aes = list(linewidth = 1.4))) +
  scale_fill_manual(values = c("grey55"), name = NULL,
                    guide = guide_legend(order = 2)) +
  scale_x_continuous(breaks = seq(1930, 2010, 10), limits = c(X_MIN, X_MAX),
                     expand = expansion(mult = 0.005)) +
  labs(x = "Year",
       y = "Scenario / Unexploited Antarctic krill consumption (ratio)") +
  theme_classic(base_size = 13) +
  theme(legend.position = "bottom", legend.box = "vertical",
        legend.spacing.y = unit(1, "pt"), panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey96", colour = "grey70"),
        strip.text = element_text(face = "bold", size = 11))

stem <- paste0("KC_fig4_by_group", SUF, if (IQR_LINES) "_iqr" else "")
for (ext in c("png", "pdf")) {
  f <- file.path(FIG, paste0(stem, ".", ext))
  if (file.exists(f) && !FORCE)
    stop("refusing to overwrite: ", f, "\n  set KC_FORCE=1", call. = FALSE)
}
suppressWarnings({
  ggsave(file.path(FIG, paste0(stem, ".png")), p, width = 11, height = 12,
         dpi = 300)
  ggsave(file.path(FIG, paste0(stem, ".pdf")), p, width = 11, height = 12)
})
cat("\nwrote", paste0(stem, ".{png,pdf}"), "(", n_mem, "members )\n")

cat("\n=== ratio at 2010, by group and scenario ===\n")
print(as.data.frame(S %>% filter(Year == 2010) %>%
  select(group, scen, med, lo, hi) %>% arrange(group, scen)),
  digits = 4, row.names = FALSE)

write.csv(S %>% select(group, scen, arm, Year, med, lo, hi),
          file.path(ANA, paste0("KC04_fig4_by_group_series", SUF, ".csv")),
          row.names = FALSE)
write.csv(PC_2001, file.path(ANA, paste0("KC04_paired_contrast", SUF, ".csv")),
          row.names = FALSE)
write.csv(SUMM, file.path(ANA, paste0("KC04_paired_summary", SUF, ".csv")),
          row.names = FALSE)
cat("\nKC04 complete.\n")