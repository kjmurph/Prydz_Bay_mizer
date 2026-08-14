# =============================================================================
# KC08 -- did halving whale access to the background resource change the
# counterfactual result?
#
# Reads KC07's paired pilot (10 members x 2 reference models x 4 arms) and
# answers, in order:
#
#   Q1 DID THE INTERVENTION BITE? Resource share of whale intake must fall well
#      below the phase-54 base's 43.5% (baleen) / 49.4% (minke). If it did not,
#      nothing downstream is worth reading.
#   Q2 ARE WHALES EVEN FOOD-LIMITED? Biomass-weighted feeding level. A group
#      sitting near 1 is satiated, and no prey change can move it whatever the
#      interaction is set to. This is the alternative explanation for the null
#      and it has to be ruled in or out before the resource share is blamed.
#   Q3 DOES THE KRILL-FISHING SIGNAL GROW? Paired peak-vs-observed contrast on
#      whale krill consumption, biomass, abundance and mean individual mass.
#      Under phase 54 these were -2.6%, -1e-6, -2.0e-5% and -0.006%.
#   Q4 IS THE FULL RE-RUN WORTH IT? An explicit decision rule, not a vibe.
#
# EVERYTHING IS PAIRED ON THE MEMBER. Both bases are run for the same 10 cut-A
# members, so each comparison is within-member and the ensemble spread -- which
# is 3-5x the effect being looked for -- cancels. Only members stable under BOTH
# bases are used; a member that converges under one and not the other is not a
# comparison.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC08_compare_whaleres05.R"
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  # mizer + therMizer are needed only for Q2b: getReproductionLevel() recomputes
  # the encounter rate, which resolves therMizerEncounter off the search path.
  library(mizer); library(therMizer)
})

OUT_LARGE <- "Output_large_files/wmin_test"
KC_ROOT <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
ANA <- file.path(KC_ROOT, "analysis"); FIG <- file.path(KC_ROOT, "figures")
for (d in c(ANA, FIG)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
FORCE <- nzchar(Sys.getenv("KC_FORCE"))
IN <- Sys.getenv("KC8_IN", file.path(OUT_LARGE, "KC07_whres05_n10.rds"))
if (!file.exists(IN))
  stop("missing ", IN, " -- run KC07_pilot_whaleres05.R first", call. = FALSE)

D <- readRDS(IN); meta <- D$meta
BOTH <- meta$members_both
cat("=== KC08: does the whale-resource change alter the counterfactual? ===\n")
cat("input:", basename(IN), "| members usable under both bases:", length(BOTH),
    "of", meta$n_requested, "\n")
if (!length(BOTH)) stop("no member is stable under both bases", call. = FALSE)

BASE_LEV <- c("phase54", "phase55")
BASE_LAB <- c(phase54 = "Resource interaction 1.0 (published)",
              phase55 = "Resource interaction 0.5")
WHALES <- c("baleen whales", "minke whales")
FISHES <- c("mesopelagic fishes", "bathypelagic fishes",
            "shelf and coastal fishes", "toothfishes")
ARMS <- c("no_krill", "exploited", "peak_krill")
SCEN <- c(no_krill = "No krill fishing", exploited = "Observed history",
          peak_krill = sprintf("Peak krill effort held from %d", meta$peak_year))
SCEN_COL <- setNames(c("#2a78d6", "#0b0b0b", "#e34948"), unname(SCEN))
LW <- c(exploited = 2.2, peak_krill = 1.3, no_krill = 0.6)
keepm <- function(d) d[d$sim_index %in% BOTH, ] %>%
  mutate(base = factor(base, levels = BASE_LEV))

BIO <- keepm(D$biomass); CONS <- keepm(D$consumption); FL <- keepm(D$feeding)

# =============================================================================
# Q1 -- did the intervention bite?
# =============================================================================
RS <- CONS %>% filter(Species %in% WHALES, arm == "unexploited") %>%
  group_by(base, Species, Year) %>%
  summarise(resource_pct = 100 * sum(resource_consumed) / sum(total_consumed),
            krill_pct = 100 * sum(krill_consumed) / sum(total_consumed),
            .groups = "drop")
Q1 <- RS %>% filter(Year %in% 2001:2010) %>%
  group_by(base, Species) %>%
  summarise(resource_pct = mean(resource_pct), krill_pct = mean(krill_pct),
            .groups = "drop") %>%
  pivot_wider(names_from = base, values_from = c(resource_pct, krill_pct))
cat("\n=== Q1. resource share of whale intake, unexploited arm, 2001-2010 (%) ===\n")
print(as.data.frame(Q1 %>% mutate(across(where(is.numeric), ~round(.x, 2)))),
      row.names = FALSE)
BIT <- all(Q1$resource_pct_phase55 < 0.8 * Q1$resource_pct_phase54)
cat(sprintf("  intervention bit (share fell >20%% for both groups): %s\n", BIT))

# =============================================================================
# Q2 -- are whales food-limited?
# =============================================================================
Q2 <- FL %>% filter(Species %in% WHALES, Year %in% 2001:2010) %>%
  group_by(base, Species, arm) %>%
  summarise(feeding_level = median(feeding_level, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = base, values_from = feeding_level)
cat("\n=== Q2. biomass-weighted feeding level, 2001-2010 (0 = starving, 1 = satiated) ===\n")
print(as.data.frame(Q2 %>% mutate(across(where(is.numeric), ~round(.x, 4)))),
      row.names = FALSE)
cat("  A group near 1 is satiated: prey changes cannot move it, whatever the\n",
    "  interaction is set to. That would be a SEPARATE cause of the null.\n")

# --- Q2b. the reproduction clamp ---------------------------------------------
# The decisive one. Reproduction level is RDD/RDI: at 1 recruitment is entirely
# density-dependent and pinned at R_max, so numbers cannot track food however
# hungry the animals are. It is an OUTCOME of the steady()/matchBiomasses
# ladder, not a setting -- steady() re-derives R_max while preserving erepro.
RL <- do.call(rbind, lapply(names(meta$bases), function(b) {
  f <- meta$bases[[b]]
  if (!file.exists(f)) return(NULL)
  p <- suppressWarnings(mizer::validParams(readRDS(f)))
  data.frame(base = b, species = names(mizer::getReproductionLevel(p)),
             repro_level = as.numeric(mizer::getReproductionLevel(p)))
}))
if (!is.null(RL)) {
  cat("\n=== Q2b. reproduction level (RDD/RDI; 1 = recruitment pinned at R_max) ===\n")
  print(as.data.frame(RL %>%
    filter(species %in% c(WHALES, "antarctic krill")) %>%
    pivot_wider(names_from = base, values_from = repro_level) %>%
    mutate(across(where(is.numeric), ~round(.x, 4)))), row.names = FALSE)
  cat("  Whales near 1 => numbers are set by R_max, not by food. No change to\n",
      "  the prey field -- resource interaction included -- can move them.\n")
  write.csv(RL, file.path(ANA, "KC08_reproduction_level.csv"), row.names = FALSE)
}

# --- Q2c. how much whale response survives a given krill depletion? ----------
# The transfer coefficient, measured rather than argued. Members differ ~200x in
# drawn krill catchability, so they span a wide range of realised depletion and
# the slope is identifiable from 10 members.
KD <- BIO %>% filter(Species == "antarctic krill", Year %in% 2001:2010) %>%
  group_by(sim_index, base, arm) %>%
  summarise(B = mean(Biomass), .groups = "drop") %>%
  pivot_wider(names_from = arm, values_from = B) %>%
  transmute(sim_index, base, krill_pct = 100 * (peak_krill / exploited - 1))
WD <- BIO %>% filter(Species %in% WHALES, Year %in% 2001:2010) %>%
  group_by(sim_index, base, arm, Year) %>%
  summarise(B = sum(Biomass), .groups = "drop") %>%
  group_by(sim_index, base, arm) %>% summarise(B = mean(B), .groups = "drop") %>%
  pivot_wider(names_from = arm, values_from = B) %>%
  transmute(sim_index, base, whale_pct = 100 * (peak_krill / exploited - 1))
TR <- inner_join(KD, WD, by = c("sim_index", "base")) %>%
  mutate(transfer = whale_pct / krill_pct)
cat("\n=== Q2c. transfer: whale biomass %% change per 1%% of krill stock removed ===\n")
print(as.data.frame(TR %>% group_by(base) %>% summarise(
  worst_krill_depletion_pct = round(min(krill_pct), 2),
  whale_response_there_pct = signif(whale_pct[which.min(krill_pct)], 3),
  median_transfer = signif(median(transfer), 3),
  buffer_factor = round(1 / median(transfer)), .groups = "drop")),
  row.names = FALSE)
cat("  buffer_factor is how many times smaller the whale response is than the\n",
    "  krill depletion driving it.\n")
write.csv(TR, file.path(ANA, "KC08_transfer_coefficient.csv"), row.names = FALSE)

# =============================================================================
# Q3 -- does the krill-fishing signal grow?
# =============================================================================
# Per-member paired contrast, peak_krill vs observed history, 2001-2010 mean.
pc <- function(d, value, species, label) {
  d %>% filter(Species %in% species, Year %in% 2001:2010) %>%
    group_by(sim_index, base, arm, Year) %>%
    summarise(v = sum(.data[[value]]), .groups = "drop") %>%
    group_by(sim_index, base, arm) %>%
    summarise(v = mean(v), .groups = "drop") %>%
    pivot_wider(names_from = arm, values_from = v) %>%
    transmute(sim_index, base, metric = label,
              pct_peak = 100 * (peak_krill / exploited - 1),
              pct_nokr = 100 * (no_krill / exploited - 1),
              pct_exploited_vs_unexp = 100 * (exploited / unexploited - 1))
}
MM <- BIO %>% filter(Species %in% WHALES) %>%
  group_by(sim_index, base, arm, Year) %>%
  summarise(Abundance = sum(Abundance), Biomass = sum(Biomass),
            .groups = "drop") %>%
  mutate(MeanMass = Biomass / Abundance, Species = "whales")

Q3 <- bind_rows(
  pc(CONS, "krill_consumed", WHALES, "whale krill consumption"),
  pc(CONS, "total_consumed", WHALES, "whale total intake"),
  pc(MM,   "Biomass",        "whales", "whale biomass"),
  pc(MM,   "Abundance",      "whales", "whale abundance"),
  pc(MM,   "MeanMass",       "whales", "whale mean body mass"),
  pc(CONS, "krill_consumed", FISHES,  "fish krill consumption"))

Q3S <- Q3 %>% group_by(metric, base) %>%
  summarise(peak_vs_obs = median(pct_peak), n_neg = sum(pct_peak < 0),
            n = n(), .groups = "drop") %>%
  pivot_wider(names_from = base, values_from = c(peak_vs_obs, n_neg)) %>%
  mutate(amplification = peak_vs_obs_phase55 / peak_vs_obs_phase54)
cat("\n=== Q3. paired peak-krill vs observed history, 2001-2010 (% change) ===\n")
print(as.data.frame(Q3S %>%
  transmute(metric,
            `phase54 %` = signif(peak_vs_obs_phase54, 3),
            `phase55 %` = signif(peak_vs_obs_phase55, 3),
            `x larger` = round(amplification, 1),
            `neg 54` = sprintf("%d/%d", n_neg_phase54, n),
            `neg 55` = sprintf("%d/%d", n_neg_phase55, n))), row.names = FALSE)

cat("\n=== whaling depletion itself (observed vs unexploited, 2001-2010 %) ===\n")
print(as.data.frame(Q3 %>% group_by(metric, base) %>%
  summarise(v = median(pct_exploited_vs_unexp), .groups = "drop") %>%
  pivot_wider(names_from = base, values_from = v) %>%
  mutate(across(where(is.numeric), ~signif(.x, 4)))), row.names = FALSE)

# =============================================================================
# figure -- three diagnostics, one row per reference model
# =============================================================================
mk <- function(d, ylab, ttl, hline = NULL) {
  lay <- function(a) geom_line(data = filter(d, arm == a),
                               aes(Year, med, colour = scen), linewidth = LW[[a]])
  p <- ggplot()
  if (!is.null(hline))
    p <- p + geom_hline(yintercept = hline, colour = "grey45",
                        linetype = "dashed", linewidth = 0.5)
  p + geom_vline(xintercept = c(1930, meta$peak_year), colour = "grey55",
                 linetype = "dotted", linewidth = 0.4) +
    lay("exploited") + lay("peak_krill") + lay("no_krill") +
    facet_wrap(~base, ncol = 1, labeller = labeller(base = BASE_LAB)) +
    scale_colour_manual(values = SCEN_COL, name = "Scenario",
                        limits = unname(SCEN),
                        guide = guide_legend(override.aes = list(linewidth = 1.4))) +
    coord_cartesian(xlim = c(1925, NA)) +
    scale_x_continuous(breaks = seq(1930, 2010, 20)) +
    theme_bw(base_size = 10) +
    theme(panel.grid = element_blank(), legend.position = "bottom",
          strip.background = element_rect(fill = "grey96", colour = "grey70"),
          strip.text = element_text(face = "bold", size = 9)) +
    labs(x = "Year", y = ylab, title = ttl)
}
med <- function(d, val, denom = NULL) {
  x <- d %>% filter(arm %in% ARMS)
  if (!is.null(denom)) x <- x %>% left_join(denom, by = c("sim_index", "base", "Year"))
  x %>% group_by(base, arm, Year) %>%
    summarise(med = median(.data[[val]], na.rm = TRUE), .groups = "drop") %>%
    mutate(scen = factor(SCEN[arm], levels = unname(SCEN)))
}
WCONS <- CONS %>% filter(Species %in% WHALES) %>%
  group_by(sim_index, base, arm, Year) %>%
  summarise(krill = sum(krill_consumed), resource = sum(resource_consumed),
            total = sum(total_consumed), .groups = "drop")
UNW <- WCONS %>% filter(arm == "unexploited") %>%
  select(sim_index, base, Year, krill_u = krill)
RAT <- WCONS %>% filter(arm %in% ARMS) %>%
  left_join(UNW, by = c("sim_index", "base", "Year")) %>%
  mutate(ratio = krill / krill_u, res_pct = 100 * resource / total)
UNA <- MM %>% filter(arm == "unexploited") %>%
  select(sim_index, base, Year, Ab_u = Abundance)
ABU <- MM %>% filter(arm %in% ARMS) %>%
  left_join(UNA, by = c("sim_index", "base", "Year")) %>%
  mutate(pct = 100 * (Abundance / Ab_u - 1))

pA <- mk(med(RAT, "res_pct"), "% of whale intake", "a  Resource share")
pB <- mk(med(RAT, "ratio"), "ratio to unexploited",
         "b  Whale krill consumption", hline = 1)
pC <- mk(med(ABU, "pct"), "% change from unexploited",
         "c  Whale abundance", hline = 0)
fig <- pA + pB + pC + plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")
stem <- "KC_whaleres05_pilot_comparison"
for (ext in c("png", "pdf")) {
  f <- file.path(FIG, paste0(stem, ".", ext))
  if (file.exists(f) && !FORCE)
    stop("refusing to overwrite: ", f, "\n  set KC_FORCE=1", call. = FALSE)
}
suppressWarnings({
  ggsave(file.path(FIG, paste0(stem, ".png")), fig, width = 13, height = 7.5,
         dpi = 300)
  ggsave(file.path(FIG, paste0(stem, ".pdf")), fig, width = 13, height = 7.5)
})
cat("\nwrote", paste0(stem, ".{png,pdf}"), "\n")

# =============================================================================
# Q4 -- is the full re-run worth it?
# =============================================================================
# Stated as a rule so the answer does not depend on who reads the table. The
# threshold on abundance is the one that matters: the published claim is that
# krill fishing leaves whale NUMBERS untouched, and the phase-54 value is
# -2.0e-5%, five orders of magnitude below anything reportable.
ab <- Q3S %>% filter(metric == "whale abundance")
kc <- Q3S %>% filter(metric == "whale krill consumption")
crit <- c(
  "intervention bit (resource share fell >20%)" = BIT,
  "whale abundance response now exceeds 0.1%" =
    abs(ab$peak_vs_obs_phase55) > 0.1,
  "whale abundance response grew >10x" =
    abs(ab$amplification) > 10,
  "krill-consumption response grew >1.5x" =
    abs(kc$amplification) > 1.5,
  "sign is consistent across members (>=80%)" =
    ab$n_neg_phase55 >= 0.8 * ab$n)
cat("\n=== Q4. decision rule ===\n")
for (nm in names(crit))
  cat(sprintf("  [%s] %s\n", if (isTRUE(crit[[nm]])) "YES" else " no", nm))
cat(sprintf("\n  %d of %d criteria met.\n", sum(unlist(crit)), length(crit)))
cat(if (sum(unlist(crit)) >= 3)
  "  -> The change bites and moves the result. The top-10% re-run and figure\n     rebuild are justified.\n"
  else if (isTRUE(crit[[1]]))
  "  -> The intervention worked but the counterfactual result barely moved.\n     The null is NOT explained by resource switching alone -- check Q2\n     (satiation) before spending the ensemble re-run.\n"
  else
  "  -> The intervention did not bite. Diagnose before running anything else.\n")

write.csv(Q3, file.path(ANA, "KC08_paired_contrasts.csv"), row.names = FALSE)
write.csv(Q3S, file.path(ANA, "KC08_summary.csv"), row.names = FALSE)
write.csv(RS, file.path(ANA, "KC08_resource_share.csv"), row.names = FALSE)
cat("\nKC08 complete.\n")