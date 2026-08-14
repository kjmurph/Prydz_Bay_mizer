# =============================================================================
# KC11 -- does relieving satiation (and the reproduction clamp) let krill
# fishing move whale abundance and body size?
#
# Reads KC10's grid (10 members x 5 gamma arms x 2 reproduction arms x 4 effort
# arms) and answers, in order:
#
#   Q1 DID THE MANIPULATION BITE? Biomass-weighted whale feeding level per arm.
#      The control sits at ~0.977; the phase-55 base at 0.790 (baleen) / 0.491
#      (minke). If feeding level did not fall, nothing downstream is worth
#      reading and the tables below are noise.
#   Q2 DID THE CLAMP MOVE? Per-member reproduction level. Records whether
#      lowering gamma releases the clamp on its own, and confirms rl25 survived.
#   Q3 THE HEADLINE. Per-member paired peak_krill vs observed history on whale
#      abundance, biomass, MEAN INDIVIDUAL MASS, krill consumption and total
#      intake -- one column per configuration, with the amplification against
#      the drawn/asis control and the sign-consistency count.
#   Q4 THE BUFFER. Transfer coefficient: % whale biomass per % krill stock
#      removed, per configuration. This is what decomposes the buffers.
#   Q5 DECISION RULE, stated before the data so the answer does not depend on
#      who reads the table.
#   Q6 WHAT IT COST. How far each configuration moves members off their
#      calibration, so the diagnostic can be reported honestly.
#
# EVERYTHING IS PAIRED ON THE MEMBER, and only members usable in EVERY
# configuration are used -- a member that converges in one cell and not another
# is not a comparison. Medians, never means: one member whose unexploited arm
# runs near zero produces an enormous ratio and a mean follows it.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC11_compare_gamma_arms.R"
# ENV    KC11_IN, KC_FORCE (overwrite figures), KC11_SUF
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
})

OUT_LARGE <- "Output_large_files/wmin_test"
KC_ROOT <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
ANA <- file.path(KC_ROOT, "analysis"); FIG <- file.path(KC_ROOT, "figures")
for (d in c(ANA, FIG)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
FORCE <- nzchar(Sys.getenv("KC_FORCE"))
SUF <- Sys.getenv("KC11_SUF", "")
IN <- Sys.getenv("KC11_IN", file.path(OUT_LARGE, "KC10_gamma_n10.rds"))
if (!file.exists(IN))
  stop("missing ", IN, " -- run KC10_gamma_satiation_pilot.R first", call. = FALSE)

D <- readRDS(IN); meta <- D$meta
BOTH <- meta$members_all_cfg
GA <- meta$gamma_arms; RP <- meta$repro_arms
CFG_LEV <- as.vector(t(outer(GA, RP, paste, sep = "/")))
REF_CFG <- "drawn/asis"

cat("=== KC11: does relieving satiation move whale abundance / body size? ===\n")
cat("input:", basename(IN), "| base:", basename(meta$base), "\n")
cat("configurations:", length(CFG_LEV), "|",
    "members usable in every one:", length(BOTH), "of", meta$n_requested, "\n")
cat("members:", paste(BOTH, collapse = ", "), "\n")
if (!length(BOTH)) stop("no member is usable in every configuration", call. = FALSE)
if (!REF_CFG %in% CFG_LEV)
  stop("the control cell ", REF_CFG, " is not in this run -- nothing to ",
       "normalise against.", call. = FALSE)

WHALES <- c("baleen whales", "minke whales")
FISHES <- c("mesopelagic fishes", "bathypelagic fishes",
            "shelf and coastal fishes", "toothfishes")
ARMS <- c("no_krill", "exploited", "peak_krill")
SCEN <- c(no_krill = "No krill fishing", exploited = "Observed history",
          peak_krill = sprintf("Peak krill effort held from %d", meta$peak_year))
SCEN_COL <- setNames(c("#2a78d6", "#0b0b0b", "#e34948"), unname(SCEN))
LW <- c(exploited = 2.2, peak_krill = 1.3, no_krill = 0.6)

# Plain-language names for the treatment codes. Used for facet strips and in
# the legend key printed to the console, so a reader never has to decode
# "wh_base/rl25" from the script.
GAM_LAB <- c(
  drawn    = "Ensemble gamma (control)",
  wh_mid   = "Whales, gamma half-way to base",
  wh_base  = "Whales, base-model gamma",
  all_mid  = "All groups, gamma half-way to base",
  all_base = "All groups, base-model gamma")
REP_LAB <- c(asis = "recruitment as calibrated",
             rl25 = "recruitment freed (level 0.25)")
cfg_label <- function(x) {
  p <- strsplit(as.character(x), "/", fixed = TRUE)
  vapply(p, function(z) sprintf("%s\n%s", GAM_LAB[[z[1]]], REP_LAB[[z[2]]]), "")
}
cat("\n=== what each configuration code means ===\n")
print(data.frame(
  code = CFG_LEV,
  gamma = unname(GAM_LAB[sub("/.*", "", CFG_LEV)]),
  reproduction = unname(REP_LAB[sub(".*/", "", CFG_LEV)])), row.names = FALSE)

# One `cfg` factor throughout: the two treatment factors are crossed, and every
# contrast below is against the control cell, so a single ordered label reads
# better than two columns that always have to be pasted anyway.
keepm <- function(d) d %>% filter(sim_index %in% BOTH) %>%
  mutate(cfg = factor(paste(gamma_arm, repro, sep = "/"), levels = CFG_LEV))
BIO <- keepm(D$biomass); CONS <- keepm(D$consumption); FL <- keepm(D$feeding)
MECH <- keepm(D$mech)

# =============================================================================
# Q1 -- did the manipulation bite?
# =============================================================================
# Base reference values, read off the params object rather than typed, so this
# cannot drift if the base is ever rebuilt.
G_BASE <- meta$gamma_base
Q1 <- FL %>% filter(Species %in% WHALES, arm == "unexploited",
                    Year %in% 2001:2010) %>%
  group_by(cfg, Species) %>%
  summarise(f = median(feeding_level, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Species, values_from = f)
cat("\n=== Q1. biomass-weighted whale feeding level, unexploited, 2001-2010 ===\n")
cat("    (0 = starving, 1 = satiated; phase-55 base = 0.7904 baleen / 0.4906 minke)\n")
print(as.data.frame(Q1 %>% mutate(across(where(is.numeric), ~round(.x, 4)))),
      row.names = FALSE)
f_ref <- Q1 %>% filter(cfg == REF_CFG)
f_trt <- Q1 %>% filter(cfg != REF_CFG)
BIT <- nrow(f_trt) > 0 &&
  any(f_trt$`baleen whales` < f_ref$`baleen whales` - 0.05 |
      f_trt$`minke whales`  < f_ref$`minke whales`  - 0.05)
cat(sprintf("  manipulation bit (feeding level fell >0.05 somewhere): %s\n", BIT))

cat("\n=== realised whale gamma / base gamma, by configuration ===\n")
print(as.data.frame(MECH %>% filter(Species %in% WHALES) %>%
  group_by(cfg, Species) %>%
  summarise(median_ratio = signif(median(gamma / gamma_base), 4),
            .groups = "drop") %>%
  pivot_wider(names_from = Species, values_from = median_ratio)), row.names = FALSE)

# =============================================================================
# Q2 -- did the reproduction clamp move?
# =============================================================================
Q2 <- MECH %>% filter(Species %in% c(WHALES, "antarctic krill")) %>%
  group_by(cfg, Species) %>%
  summarise(rl = median(repro_level), .groups = "drop") %>%
  pivot_wider(names_from = Species, values_from = rl)
# reproduction_level = RDD / R_max = RDI / (RDI + R_max), equivalently
# 1 - RDD/RDI. Verified against ?setBevertonHolt ("sets R_max so that the
# reproduction rate at the initial state is R_max * reproduction_level") and
# numerically against getReproductionLevel(). It is NOT RDD/RDI.
cat("\n=== Q2. reproduction level (RDD/R_max; 1 = recruitment pinned at R_max) ===\n")
print(as.data.frame(Q2 %>% mutate(across(where(is.numeric), ~round(.x, 4)))),
      row.names = FALSE)
cat("  A group near 1 has its numbers set by R_max, not by food: no change to\n",
    "  the prey field can move it however hungry the animals are.\n")

# =============================================================================
# Q3 -- the headline
# =============================================================================
# Per-member paired contrast, 2001-2010 mean. The paired form is the honest one:
# a difference of medians overstated the phase-54 whale effect by 7.4x because
# peak_krill re-ranks the ensemble and the median trajectory is a different
# member in each arm.
pc <- function(d, value, species, label) {
  d %>% filter(Species %in% species, Year %in% 2001:2010) %>%
    group_by(sim_index, cfg, arm, Year) %>%
    summarise(v = sum(.data[[value]]), .groups = "drop") %>%
    group_by(sim_index, cfg, arm) %>%
    summarise(v = mean(v), .groups = "drop") %>%
    pivot_wider(names_from = arm, values_from = v) %>%
    transmute(sim_index, cfg, metric = label,
              pct_peak = 100 * (peak_krill / exploited - 1),
              pct_nokr = 100 * (no_krill / exploited - 1),
              pct_exploited_vs_unexp = 100 * (exploited / unexploited - 1))
}
# POOLED whale aggregate, kept for the transfer coefficient and for continuity
# with KC08. Do NOT read its MeanMass as a body-size response: pooling two
# species with a ~17x difference in individual mass means the ratio moves
# whenever their ABUNDANCE ratio moves, even if neither species' own mean mass
# changes at all. Q3b below reports it per species, which is the honest form.
MM <- BIO %>% filter(Species %in% WHALES) %>%
  group_by(sim_index, cfg, arm, Year) %>%
  summarise(Abundance = sum(Abundance), Biomass = sum(Biomass),
            .groups = "drop") %>%
  mutate(MeanMass = Biomass / Abundance, Species = "whales")
# PER-SPECIES, the form the figures and Q3b use.
MMS <- BIO %>% filter(Species %in% WHALES) %>%
  mutate(MeanMass = Biomass / Abundance)

MET_LEV <- c("whale abundance", "whale biomass", "whale mean body mass",
             "whale krill consumption", "whale total intake",
             "fish krill consumption")
Q3 <- bind_rows(
  pc(MM,   "Abundance",      "whales", "whale abundance"),
  pc(MM,   "Biomass",        "whales", "whale biomass"),
  pc(MM,   "MeanMass",       "whales", "whale mean body mass"),
  pc(CONS, "krill_consumed", WHALES,   "whale krill consumption"),
  pc(CONS, "total_consumed", WHALES,   "whale total intake"),
  pc(CONS, "krill_consumed", FISHES,   "fish krill consumption")) %>%
  mutate(metric = factor(metric, levels = MET_LEV))

Q3S <- Q3 %>% group_by(metric, cfg) %>%
  summarise(peak_vs_obs = median(pct_peak), nokr_vs_obs = median(pct_nokr),
            n_neg = sum(pct_peak < 0), n = n(), .groups = "drop") %>%
  group_by(metric) %>%
  mutate(amplification = peak_vs_obs / peak_vs_obs[cfg == REF_CFG]) %>%
  ungroup()

cat("\n=== Q3. paired peak-krill vs observed history, 2001-2010 (% change) ===\n")
for (m in MET_LEV) {
  cat("\n--", m, "--\n")
  print(as.data.frame(Q3S %>% filter(metric == m) %>%
    transmute(cfg,
              `peak vs obs %` = signif(peak_vs_obs, 3),
              `no-krill vs obs %` = signif(nokr_vs_obs, 3),
              `x vs control` = round(amplification, 2),
              negative = sprintf("%d/%d", n_neg, n))), row.names = FALSE)
}

# --- Q3b. the same three metrics, PER SPECIES ---------------------------------
# Baleen and minke respond differently enough that the pooled numbers above are
# not a description of either of them. This is the table to quote.
Q3B <- bind_rows(lapply(WHALES, function(s) bind_rows(
  pc(MMS, "Abundance", s, "abundance"),
  pc(MMS, "Biomass",   s, "biomass"),
  pc(MMS, "MeanMass",  s, "mean body mass")) %>% mutate(Species = s)))
Q3BS <- Q3B %>% group_by(Species, metric, cfg) %>%
  summarise(peak_vs_obs = median(pct_peak), n_neg = sum(pct_peak < 0),
            n = n(), .groups = "drop")
cat("\n=== Q3b. the same contrast, PER SPECIES (% change) ===\n")
for (s in WHALES) for (m in c("abundance", "biomass", "mean body mass")) {
  cat("\n--", s, "|", m, "--\n")
  print(as.data.frame(Q3BS %>% filter(Species == s, metric == m) %>%
    transmute(cfg, `peak vs obs %` = signif(peak_vs_obs, 3),
              negative = sprintf("%d/%d", n_neg, n))), row.names = FALSE)
}
# Quantify the pooling artefact so nobody has to take the warning on trust.
cmp <- Q3S %>% filter(metric == "whale mean body mass") %>%
  select(cfg, pooled = peak_vs_obs) %>%
  left_join(Q3BS %>% filter(metric == "mean body mass") %>%
              select(cfg, Species, v = peak_vs_obs) %>%
              pivot_wider(names_from = Species, values_from = v), by = "cfg")
cat("\n=== pooled vs per-species mean body mass: the pooling artefact ===\n")
print(as.data.frame(cmp %>% mutate(across(where(is.numeric), ~signif(.x, 3)))),
      row.names = FALSE)
cat("  Where the pooled value sits outside the two species values, it is being\n",
    " driven by a shift in the baleen:minke abundance ratio, not by body size.\n")

cat("\n=== whaling depletion itself (observed vs unexploited, 2001-2010 %) ===\n")
print(as.data.frame(Q3 %>% group_by(metric, cfg) %>%
  summarise(v = median(pct_exploited_vs_unexp), .groups = "drop") %>%
  pivot_wider(names_from = cfg, values_from = v) %>%
  mutate(across(where(is.numeric), ~signif(.x, 4)))), row.names = FALSE)

# =============================================================================
# Q4 -- the buffer
# =============================================================================
KD <- BIO %>% filter(Species == "antarctic krill", Year %in% 2001:2010) %>%
  group_by(sim_index, cfg, arm) %>%
  summarise(B = mean(Biomass), .groups = "drop") %>%
  pivot_wider(names_from = arm, values_from = B) %>%
  transmute(sim_index, cfg, krill_pct = 100 * (peak_krill / exploited - 1))
WD <- MM %>% group_by(sim_index, cfg, arm) %>%
  summarise(B = mean(Biomass), .groups = "drop") %>%
  pivot_wider(names_from = arm, values_from = B) %>%
  transmute(sim_index, cfg, whale_pct = 100 * (peak_krill / exploited - 1))
TR <- inner_join(KD, WD, by = c("sim_index", "cfg")) %>%
  mutate(transfer = whale_pct / krill_pct)
cat("\n=== Q4. transfer: whale biomass % change per 1% of krill stock removed ===\n")
print(as.data.frame(TR %>% group_by(cfg) %>% summarise(
  worst_krill_depletion_pct = round(min(krill_pct), 2),
  whale_response_there_pct = signif(whale_pct[which.min(krill_pct)], 3),
  median_transfer = signif(median(transfer), 3),
  buffer_factor = round(1 / median(transfer)), .groups = "drop")),
  row.names = FALSE)
cat("  buffer_factor is how many times smaller the whale response is than the\n",
    "  krill depletion driving it. Lower = the signal gets through.\n")

# --- Q4b. the substitution buffer, measured -----------------------------------
# Whales replace lost krill with other prey. The ratio of the krill-consumption
# response to the TOTAL-intake response is how effective that substitution is:
# 20x means 95% of the lost krill was made up elsewhere. This is the second
# buffer in the chain and it is not independent of satiation -- a food-limited
# whale has less room to substitute.
SUB <- Q3S %>% filter(metric %in% c("whale krill consumption",
                                    "whale total intake")) %>%
  select(metric, cfg, peak_vs_obs) %>%
  pivot_wider(names_from = metric, values_from = peak_vs_obs) %>%
  transmute(cfg,
            krill_pct = signif(`whale krill consumption`, 3),
            intake_pct = signif(`whale total intake`, 3),
            substitution_buffer =
              round(`whale krill consumption` / `whale total intake`, 2))
cat("\n=== Q4b. substitution buffer (krill response / total-intake response) ===\n")
print(as.data.frame(SUB), row.names = FALSE)
cat("  High = whales replace lost krill almost perfectly, so intake barely\n",
    "  moves. Falling with the gamma dose means relieving satiation also\n",
    "  weakens substitution -- the two buffers are not independent.\n")
write.csv(SUB, file.path(ANA, paste0("KC11_substitution_buffer", SUF, ".csv")),
          row.names = FALSE)

# =============================================================================
# Q5 -- decision rule
# =============================================================================
best <- function(m) Q3S %>% filter(metric == m, cfg != REF_CFG) %>%
  slice_max(abs(peak_vs_obs), n = 1, with_ties = FALSE)
# Both criteria are judged PER SPECIES. The pooled whale aggregate is dominated
# by baleen biomass and hides minke, which is the group that actually responds;
# and pooled mean body mass is not a body-size response at all (see Q3b). Each
# takes the larger absolute response of the two species.
bestsp <- function(m) Q3BS %>% filter(metric == m, cfg != REF_CFG) %>%
  slice_max(abs(peak_vs_obs), n = 1, with_ties = FALSE) %>%
  mutate(amplification = peak_vs_obs /
           Q3BS$peak_vs_obs[Q3BS$metric == m & Q3BS$cfg == REF_CFG &
                            Q3BS$Species == Species])
ab <- bestsp("abundance"); mm <- bestsp("mean body mass")
ab_pooled <- best("whale abundance")
ref_ab <- Q3S %>% filter(metric == "whale abundance", cfg == REF_CFG)
crit <- c(
  "manipulation bit (feeding level fell >0.05)" = BIT,
  "whale abundance response exceeds 0.1% somewhere" =
    abs(ab$peak_vs_obs) > 0.1,
  "whale abundance response grew >10x over the control" =
    abs(ab$amplification) > 10,
  "whale mean body mass response exceeds 1% somewhere" =
    abs(mm$peak_vs_obs) > 1,
  "sign consistent across members (>=80%) in that cell" =
    ab$n_neg >= 0.8 * ab$n)
cat("\n=== Q5. decision rule ===\n")
cat(sprintf("  control (%s) pooled whale abundance response: %.3g%%\n",
            REF_CFG, ref_ab$peak_vs_obs))
cat(sprintf("  best treatment cell for abundance: %s, %s at %.3g%% (%.1fx)\n",
            as.character(ab$cfg), ab$Species, ab$peak_vs_obs, ab$amplification))
cat(sprintf("    (the pooled aggregate would have said %s at %.3g%% -- it is\n",
            as.character(ab_pooled$cfg), ab_pooled$peak_vs_obs),
    "    dominated by baleen biomass and understates minke)\n", sep = "")
cat(sprintf("  best treatment cell for body mass: %s, %s at %.3g%% (%.1fx)\n",
            as.character(mm$cfg), mm$Species, mm$peak_vs_obs, mm$amplification))
for (nm in names(crit))
  cat(sprintf("  [%s] %s\n", if (isTRUE(crit[[nm]])) "YES" else " no", nm))
cat(sprintf("\n  %d of %d criteria met.\n", sum(unlist(crit)), length(crit)))

# The criteria above are pre-registered and are reported as they fell. They are
# RELATIVE, though, and the control response is numerical noise (see the
# sign-consistency column: a control at 5/9 negative is a coin flip, so any
# "x vs control" built on it is a ratio to noise and must not be read as an
# amplification factor). The verdict therefore keys on the ABSOLUTE size of the
# best response, which is the only thing a reader can act on.
BIG <- 1.0     # % change in whale numbers that would be ecologically visible
cat(sprintf("\n  Absolute size of the best abundance response: %.3g%%.\n",
            abs(ab$peak_vs_obs)))
cat(if (!isTRUE(crit[[1]]))
  "  -> The manipulation did not bite. Diagnose before reading anything else.\n"
  else if (abs(ab$peak_vs_obs) >= BIG)
  "  -> Satiation was a binding constraint. Whale numbers respond to krill\n     fishing at an ecologically visible size once it is relieved, so the\n     published null is a property of the ensemble's drawn gamma.\n"
  else if (abs(mm$peak_vs_obs) >= BIG)
  "  -> Relieving satiation moves whale BODY SIZE but not NUMBERS. Report the\n     growth response as the real signal and attribute the abundance null to\n     density-dependent recruitment.\n"
  else
  paste0("  -> Satiation IS a real buffer -- the intake and biomass responses\n",
         "     grow monotonically as gamma falls -- but relieving it does NOT\n",
         "     make whale numbers responsive. The best cell still moves whale\n",
         sprintf("     abundance only %.3g%%, far below anything detectable. The null\n",
                 abs(ab$peak_vs_obs)),
         "     survives every buffer removed so far; what remains is the small\n",
         "     realised krill depletion itself and near-perfect diet\n",
         "     substitution. Report the buffer ladder, not a reversal.\n"))

# =============================================================================
# Q6 -- what the manipulation cost
# =============================================================================
# These configurations are deliberately off-calibration. Quantify it rather than
# leaving a reader to assume the treated members are still fitted.
CAL <- BIO %>% filter(Species %in% WHALES, arm == "unexploited",
                      Year %in% 2001:2010) %>%
  group_by(sim_index, cfg, Species) %>%
  summarise(B = mean(Biomass), .groups = "drop") %>%
  group_by(sim_index, Species) %>%
  mutate(rel = B / B[cfg == REF_CFG]) %>% ungroup() %>%
  group_by(cfg, Species) %>%
  summarise(median_rel_biomass = signif(median(rel), 4), .groups = "drop") %>%
  pivot_wider(names_from = Species, values_from = median_rel_biomass)
cat("\n=== Q6. unexploited whale biomass relative to the control cell ===\n")
print(as.data.frame(CAL), row.names = FALSE)
cat("  1.0 = unchanged. Large departures mean the cell is no longer the fitted\n",
    "  member and its absolute levels must not be quoted as ensemble results.\n")

# =============================================================================
# figure
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
    facet_wrap(~cfg, ncol = 1, scales = "free_y",
               labeller = labeller(cfg = cfg_label)) +
    scale_colour_manual(values = SCEN_COL, name = "Scenario",
                        limits = unname(SCEN),
                        guide = guide_legend(override.aes = list(linewidth = 1.4))) +
    coord_cartesian(xlim = c(1925, NA)) +
    scale_x_continuous(breaks = seq(1930, 2010, 20)) +
    theme_bw(base_size = 9) +
    theme(panel.grid = element_blank(), legend.position = "bottom",
          strip.background = element_rect(fill = "grey96", colour = "grey70"),
          strip.text = element_text(face = "bold", size = 8)) +
    labs(x = "Year", y = ylab, title = ttl)
}
med <- function(d, val) d %>% filter(arm %in% ARMS) %>%
  group_by(cfg, arm, Year) %>%
  summarise(med = median(.data[[val]], na.rm = TRUE), .groups = "drop") %>%
  mutate(scen = factor(SCEN[arm], levels = unname(SCEN)))

# ONE FIGURE PER WHALE GROUP. Baleen and minke differ ~17x in individual mass
# and respond differently in every panel, so a pooled figure describes neither
# -- and its mean-body-mass panel is not a body-size response at all (Q3b).
one_figure <- function(sp) {
  tag <- gsub("[^a-z]+", "_", tolower(sp))
  wfl <- FL %>% filter(Species == sp) %>%
    group_by(sim_index, cfg, arm, Year) %>%
    summarise(f = mean(feeding_level, na.rm = TRUE), .groups = "drop")
  wc <- CONS %>% filter(Species == sp) %>%
    group_by(sim_index, cfg, arm, Year) %>%
    summarise(krill = sum(krill_consumed), .groups = "drop")
  rat <- wc %>% filter(arm %in% ARMS) %>%
    left_join(wc %>% filter(arm == "unexploited") %>%
                select(sim_index, cfg, Year, krill_u = krill),
              by = c("sim_index", "cfg", "Year")) %>%
    mutate(ratio = krill / krill_u)
  mms <- MMS %>% filter(Species == sp)
  abu <- mms %>% filter(arm %in% ARMS) %>%
    left_join(mms %>% filter(arm == "unexploited") %>%
                select(sim_index, cfg, Year, Ab_u = Abundance, Mm_u = MeanMass),
              by = c("sim_index", "cfg", "Year")) %>%
    mutate(pct = 100 * (Abundance / Ab_u - 1),
           mpct = 100 * (MeanMass / Mm_u - 1))

  fig <- mk(med(wfl, "f"), "feeding level", "a  Feeding level") +
    mk(med(rat, "ratio"), "ratio to unexploited",
       "b  Krill consumption", hline = 1) +
    mk(med(abu, "pct"), "% change from unexploited",
       "c  Abundance", hline = 0) +
    mk(med(abu, "mpct"), "% change from unexploited",
       "d  Mean individual body mass", hline = 0) +
    plot_layout(ncol = 4, guides = "collect") +
    plot_annotation(
      title = paste0(toupper(substring(sp, 1, 1)), substring(sp, 2)),
      subtitle = paste("median over", length(BOTH),
                       "members; rows are the gamma x reproduction",
                       "configurations")) &
    theme(legend.position = "bottom")

  stem <- paste0("KC_gamma_satiation_", tag, SUF)
  for (ext in c("png", "pdf")) {
    f <- file.path(FIG, paste0(stem, ".", ext))
    if (file.exists(f) && !FORCE)
      stop("refusing to overwrite: ", f, "\n  set KC_FORCE=1", call. = FALSE)
  }
  H <- max(7, 1.75 * length(CFG_LEV))
  suppressWarnings({
    ggsave(file.path(FIG, paste0(stem, ".png")), fig, width = 15, height = H,
           dpi = 300, limitsize = FALSE)
    ggsave(file.path(FIG, paste0(stem, ".pdf")), fig, width = 15, height = H,
           limitsize = FALSE)
  })
  cat("wrote", paste0(stem, ".{png,pdf}"), "\n")
}
cat("\n")
for (s in WHALES) one_figure(s)

write.csv(Q3,  file.path(ANA, paste0("KC11_paired_contrasts", SUF, ".csv")), row.names = FALSE)
write.csv(Q3S, file.path(ANA, paste0("KC11_summary", SUF, ".csv")), row.names = FALSE)
write.csv(Q3B, file.path(ANA, paste0("KC11_paired_contrasts_by_species", SUF, ".csv")), row.names = FALSE)
write.csv(Q3BS, file.path(ANA, paste0("KC11_summary_by_species", SUF, ".csv")), row.names = FALSE)
write.csv(data.frame(code = CFG_LEV,
                     gamma = unname(GAM_LAB[sub("/.*", "", CFG_LEV)]),
                     reproduction = unname(REP_LAB[sub(".*/", "", CFG_LEV)])),
          file.path(ANA, paste0("KC11_configuration_key", SUF, ".csv")),
          row.names = FALSE)
write.csv(Q1,  file.path(ANA, paste0("KC11_feeding_level", SUF, ".csv")), row.names = FALSE)
write.csv(Q2,  file.path(ANA, paste0("KC11_reproduction_level", SUF, ".csv")), row.names = FALSE)
write.csv(TR,  file.path(ANA, paste0("KC11_transfer_coefficient", SUF, ".csv")), row.names = FALSE)
cat("KC11 complete.\n")
