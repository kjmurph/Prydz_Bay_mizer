# =============================================================================
# KC15 -- consumption of a chosen prey group under the four krill
# counterfactuals, faceted by predator group
#
# Generalises KC04_figure4_by_group.R along the PREY axis. KC04 is hard-wired to
# `krill_consumed` and carries fatal regression gates against the published
# phase-54 build; neither applies here, because this reads the phase-56 ensemble
# (whale interaction_resource = 0) and has to draw a second prey group.
#
# TWO FIGURES FROM ONE SCRIPT:
#   KC15_PREY=krill      Antarctic krill  -- the KC04 figure on the new base
#   KC15_PREY=other_ltl  the four non-krill zooplankton groups summed
#                        (mesozooplankton, other krill, other macrozooplankton,
#                        salps)
#
# WHY THE SECOND ONE EXISTS. Setting whale interaction_resource to 0 removes the
# undepletable background resource from whale diets, but it does NOT remove the
# switching pathway -- it redirects it. At the phase-56 calibrated state baleen
# whales take 72.1% krill and 27.0% other LTL, where at phase 54 they took 40.8%
# krill and 43.5% resource. The other-LTL groups ARE dynamic and depletable, so
# the krill figure alone can no longer show where the compensation goes.
#
# READ THE "All predators" PANEL OF THE OTHER-LTL FIGURE WITH CARE: the four LTL
# groups are themselves among the predators, and they dominate consumption of
# each other, so that panel is not a statement about whales. The whale panel is.
#
# GEOMETRY IS KC04 TRANSCRIBED -- per member-year ratio to that member's own
# unexploited arm, the three arms at decreasing linewidth so coincident lines
# read as concentric banding, the per-panel +-1 SD natural-variability band, era
# markers derived from the effort array, and the fatal clipping guard. ONE
# DEPARTURE: all three scenario IQRs are drawn as filled ribbons, with the two
# counterfactuals UNDERLAID beneath the observed one -- see the KC15_IQR block.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC15_consumption_by_prey.R"
# ENV    KC15_IN, KC15_PREY (krill|other_ltl), KC15_SUF, KC_FORCE,
#        KC15_IQR (1), KC15_ALPHA_CF (0.32), KC15_ALPHA_OBS (0.45)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
})

OUT_LARGE <- "Output_large_files/wmin_test"
KC_ROOT <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
ANA <- file.path(KC_ROOT, "analysis"); FIG <- file.path(KC_ROOT, "figures")
for (d in c(ANA, FIG)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
IN <- Sys.getenv("KC15_IN", file.path(OUT_LARGE, "KC14_whres000_n167.rds"))
PREY <- Sys.getenv("KC15_PREY", "krill")
SUF <- Sys.getenv("KC15_SUF", "_whres000")
FORCE <- nzchar(Sys.getenv("KC_FORCE"))
# THE COUNTERFACTUAL IQRs ARE UNDERLAID RIBBONS, NOT DASHED BOUNDS (Kieran's
# call, 2026-08-12). KC04 drew them as dashed outer bounds because three
# overlapping FILLS at equal depth read as one cloud -- the across-member spread
# is 3-5x the between-scenario separation. Underlaying fixes that without the
# dashed clutter: the counterfactual ribbons go down FIRST and the grey observed
# ribbon covers them, so wherever the arms coincide the reader sees grey, and
# colour appears only where a counterfactual spread reaches outside the observed
# one. Draw order within the counterfactuals is no_krill then peak_krill,
# because no_krill sits on top of exploited almost everywhere and peak_krill is
# the arm that actually diverges.
IQR_ALL <- Sys.getenv("KC15_IQR", "1") != "0"
A_CF <- as.numeric(Sys.getenv("KC15_ALPHA_CF", "0.32"))   # counterfactual fills
A_OBS <- as.numeric(Sys.getenv("KC15_ALPHA_OBS", "0.45")) # observed fill, on top

PREY_DEF <- list(
  krill     = list(col = "krill_consumed",
                   axis = "Antarctic krill",
                   stem = "krill"),
  other_ltl = list(col = "other_ltl_consumed",
                   axis = "other lower-trophic-level prey",
                   stem = "other_ltl"))
if (!PREY %in% names(PREY_DEF))
  stop("KC15_PREY must be one of: ", paste(names(PREY_DEF), collapse = ", "),
       call. = FALSE)
PD <- PREY_DEF[[PREY]]
if (!file.exists(IN))
  stop("missing ", IN, " -- run KC14_ensemble_whres000.R first", call. = FALSE)

D <- readRDS(IN); meta <- D$meta
KR <- D$consumption
if (!PD$col %in% names(KR))
  stop("column ", PD$col, " is not in the consumption table", call. = FALSE)
KR$cons_raw <- KR[[PD$col]]
cat("=== KC15: consumption of", PD$axis, "===\n")
cat("input:", basename(IN), "| base:", basename(meta$base), "\n")
cat("other LTL =", paste(meta$other_ltl, collapse = ", "), "\n")

# Stable members only, before any aggregation: a divergent member's ratio is
# meaningless rather than merely extreme, and one can own an across-member sum.
keep <- D$members$sim_index[D$members$stable]
cat("members:", nrow(D$members), "| stable:", length(keep), "\n")
if (!length(keep)) stop("no stable members", call. = FALSE)
KR <- KR[KR$sim_index %in% keep, ]
n_mem <- length(unique(KR$sim_index))

FISHES <- c("mesopelagic fishes", "bathypelagic fishes",
            "shelf and coastal fishes", "toothfishes")
WHALES <- meta$whales
ALL_PRED <- sort(unique(KR$Species))
GRP_LEV <- c("All predators", "Large baleen + minke whales", "All fishes")
ARMS <- c("no_krill", "exploited", "peak_krill")
SCEN <- c(no_krill = "No krill fishing", exploited = "Observed history",
          peak_krill = sprintf("Peak krill effort held from %d", meta$peak_year))
SCEN_COL <- c("#2a78d6", "#0b0b0b", "#e34948")
names(SCEN_COL) <- unname(SCEN)

# --- era markers, derived from the effort array (KC04:104-117) ----------------
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
grp_wide <- function(species, label) {
  KR %>% filter(Species %in% species) %>%
    group_by(sim_index, arm, Year) %>%
    summarise(cons = sum(cons_raw), .groups = "drop") %>%
    pivot_wider(names_from = arm, values_from = cons) %>%
    mutate(group = label)
}
CW <- bind_rows(grp_wide(ALL_PRED, GRP_LEV[1]), grp_wide(WHALES, GRP_LEV[2]),
                grp_wide(FISHES, GRP_LEV[3])) %>%
  filter(is.finite(unexploited), unexploited > 0)

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

# --- per-group +-1 SD natural-variability band (KC04:148-165) -----------------
BASE_YEARS <- sort(unique(KR$Year))
sd_band <- function(species) {
  u <- KR %>% filter(Species %in% species, arm == "unexploited") %>%
    group_by(sim_index, Year) %>%
    summarise(cons = sum(cons_raw), .groups = "drop") %>%
    group_by(Year) %>% summarise(mu = mean(cons), .groups = "drop")
  sigma <- sd(u$mu) / mean(u$mu)
  data.frame(lo = max(1 - sigma, 0), hi = 1 + sigma, sigma = sigma)
}
BAND <- bind_rows(cbind(group = GRP_LEV[1], sd_band(ALL_PRED)),
                  cbind(group = GRP_LEV[2], sd_band(WHALES)),
                  cbind(group = GRP_LEV[3], sd_band(FISHES))) %>%
  mutate(group = factor(group, levels = GRP_LEV))
cat(sprintf("\n+-1 SD bands from the unexploited arm over %d-%d\n",
            min(BASE_YEARS), max(BASE_YEARS)))
print(as.data.frame(BAND), digits = 4, row.names = FALSE)

# --- the paired contrast, the statistic the figure cannot draw ----------------
PC <- CW %>%
  transmute(sim_index, Year, group,
            rel_no_krill   = no_krill   / exploited,
            rel_peak_krill = peak_krill / exploited,
            dif_peak_krill = (peak_krill - exploited) / unexploited)
PC_2001 <- PC %>% filter(Year %in% 2001:2010) %>%
  group_by(sim_index, group) %>%
  summarise(across(starts_with(c("rel_", "dif_")), mean), .groups = "drop")
SUMM <- PC_2001 %>% group_by(group) %>% summarise(
  paired_diff_med = median(dif_peak_krill),
  paired_ratio_med = median(rel_peak_krill),
  paired_ratio_q25 = quantile(rel_peak_krill, 0.25),
  paired_ratio_q75 = quantile(rel_peak_krill, 0.75),
  n_below_1 = sum(rel_peak_krill < 1), n = n(),
  nokrill_ratio_med = median(rel_no_krill), .groups = "drop") %>%
  mutate(group = factor(group, levels = GRP_LEV)) %>% arrange(group)
REF_2001 <- S %>% filter(Year %in% 2001:2010) %>%
  group_by(group, arm) %>% summarise(med = mean(med), .groups = "drop")
DOM <- REF_2001 %>% pivot_wider(names_from = arm, values_from = med) %>%
  transmute(group, diff_of_medians = peak_krill - exploited)
cat("\n=== peak krill effort vs observed history, 2001-2010, TWO summaries ===\n")
cat("    (the figure's median lines show the first; the second is the typical member)\n")
print(as.data.frame(left_join(DOM, SUMM, by = "group") %>%
  transmute(group,
            `difference of medians` = round(diff_of_medians, 4),
            `median paired diff` = round(paired_diff_med, 4),
            `median paired ratio` = round(paired_ratio_med, 4),
            `paired ratio IQR` = sprintf("%.3f-%.3f", paired_ratio_q25,
                                         paired_ratio_q75),
            `below 1` = sprintf("%d/%d", n_below_1, n))), row.names = FALSE)
cat("\n=== the observed krill fishery: no_krill / observed, 2001-2010 ===\n")
print(as.data.frame(SUMM %>% transmute(group,
  median = signif(nokrill_ratio_med, 6))), row.names = FALSE)

# =============================================================================
# figure
# =============================================================================
X_MIN <- 1925; X_MAX <- 2012
ENV_ARMS <- if (IQR_ALL) ARMS else "exploited"
# Limits derived from what is actually drawn -- the published hand-tuned frame
# is for the phase-54 krill figure and does not apply to a new base or a new
# prey group.
LIMS <- S %>% left_join(BAND, by = "group", suffix = c("", ".b")) %>%
  group_by(group) %>%
  summarise(dlo = min(c(med, lo[arm %in% ENV_ARMS], lo.b), na.rm = TRUE),
            dhi = max(c(med, hi[arm %in% ENV_ARMS], hi.b), na.rm = TRUE),
            .groups = "drop") %>%
  mutate(span = pmax(dhi - dlo, 1e-6),
         ylo = dlo - 0.22 * span,
         yhi = dhi + (if (IQR_ALL) 0.20 else 0.08) * span) %>%
  select(group, ylo, yhi) %>%
  pivot_longer(c(ylo, yhi), values_to = "y") %>%
  transmute(group, y, Year = 1930)
YR <- LIMS %>% group_by(group) %>%
  summarise(ylo = min(y), yhi = max(y), .groups = "drop")
# Silent clipping would delete a scenario line and leave a plausible figure.
CLIP <- S %>% left_join(YR, by = "group") %>%
  filter(med < ylo | med > yhi |
           (arm %in% ENV_ARMS & (lo < ylo | hi > yhi)))
if (nrow(CLIP))
  stop("y limits clip ", nrow(CLIP), " plotted points, e.g. ",
       as.character(CLIP$group[1]), " ", CLIP$Year[1], call. = FALSE)

TOP <- factor(GRP_LEV[1], levels = GRP_LEV)
ev_lab <- data.frame(
  x = c(whaling_start, krill_start, krill_end),
  lab = c("Whaling\nstarts", "Krill fishing\nstarts", "End of\nkrill fishing"),
  group = TOP)
pk_lab <- YR %>% tidyr::crossing(data.frame(x = as.numeric(pk),
                                            lab = names(pk))) %>%
  mutate(y = ylo + 0.02 * (yhi - ylo))
band_lab <- BAND %>% filter(group == TOP) %>% mutate(x = X_MAX, lab = "±1 SD")
RIB <- S %>% filter(arm == "exploited") %>%
  mutate(env = if (IQR_ALL)
    "Interquartile range across members, one per scenario (observed on top)"
    else "Observed history, interquartile range across members")
# Unmapped fill: the scenario colours are already in the colour legend, and a
# second three-entry fill legend saying the same thing would be noise. The grey
# key alone documents that the shading is an across-member IQR.
cf_ribbon <- function(a) geom_ribbon(
  data = filter(S, IQR_ALL, arm == a),
  aes(Year, ymin = lo, ymax = hi),
  fill = unname(SCEN_COL[[unname(SCEN[[a]])]]), alpha = A_CF, colour = NA)
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
  # Counterfactual IQRs UNDERNEATH, then the observed IQR over them: where the
  # arms coincide the reader sees grey, and colour appears only on divergence.
  cf_ribbon("no_krill") + cf_ribbon("peak_krill") +
  geom_ribbon(data = RIB, aes(Year, ymin = lo, ymax = hi, fill = env),
              alpha = A_OBS, colour = NA) +
  geom_hline(data = BAND, aes(yintercept = lo), colour = "grey35",
             linetype = "dashed", linewidth = 0.45) +
  geom_hline(data = BAND, aes(yintercept = hi), colour = "grey35",
             linetype = "dashed", linewidth = 0.45) +
  lay("exploited", 2.4) + lay("peak_krill", 1.4) + lay("no_krill", 0.7) +
  geom_text(data = ev_lab, aes(x = x, y = Inf, label = lab),
            hjust = -0.06, vjust = 1.25, size = 3.1, colour = "grey30") +
  geom_text(data = pk_lab, aes(x = x, y = y, label = lab), angle = 90,
            hjust = 0, vjust = -0.3, size = 2.7, colour = "grey45") +
  geom_text(data = band_lab, aes(x = x, y = hi, label = lab), hjust = 1.15,
            vjust = -0.6, size = 2.9, colour = "grey35") +
  facet_wrap(~group, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = SCEN_COL, name = "Scenario",
                      limits = unname(SCEN),
                      guide = guide_legend(order = 1,
                        override.aes = list(linewidth = 1.4))) +
  scale_fill_manual(values = c("grey55"), name = NULL,
                    guide = guide_legend(order = 2)) +
  scale_x_continuous(breaks = seq(1930, 2010, 10), limits = c(X_MIN, X_MAX),
                     expand = expansion(mult = 0.005)) +
  labs(x = "Year",
       y = sprintf("Scenario / Unexploited %s consumption (ratio)", PD$axis)) +
  theme_classic(base_size = 13) +
  theme(legend.position = "bottom", legend.box = "vertical",
        legend.spacing.y = unit(1, "pt"), panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey96", colour = "grey70"),
        strip.text = element_text(face = "bold", size = 11))

stem <- paste0("KC_consumption_", PD$stem, SUF, if (IQR_ALL) "" else "_obsiqr")
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
          file.path(ANA, paste0("KC15_series_", PD$stem, SUF, ".csv")),
          row.names = FALSE)
write.csv(SUMM, file.path(ANA, paste0("KC15_paired_summary_", PD$stem, SUF,
                                      ".csv")), row.names = FALSE)
cat("\nKC15 complete.\n")