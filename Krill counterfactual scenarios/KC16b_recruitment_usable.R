# =============================================================================
# KC16 -- annual recruitment (getRDD) by species and exploitation scenario
#
# THE QUESTION. Whale abundance and body size barely move under any krill
# counterfactual. Recruitment is the flux that sets numbers, so this shows it
# directly: eggs produced per year for every group, under each scenario, across
# the ensemble. It answers two things the biomass figures cannot -- whether
# recruitment responds to exploitation at all, and whether a group's numbers are
# pinned at R_max (in which case its RDD line is flat however the prey field
# moves).
#
# WHAT IS PLOTTED. Panel a: RDD as a ratio to that member's own UNEXPLOITED arm,
# which is the paired form used everywhere else in this pipeline and removes the
# ~200x across-species difference in absolute egg number. Panel b: the
# reproduction level, RDD / R_max -- 1 means recruitment is entirely
# density-dependent and cannot track food, 0 means it is free to. Reading them
# together separates "recruitment did not respond" from "recruitment could not
# respond".
#
# NOT RDD/RDI. mizer's reproduction_level is RDD / R_max = RDI / (RDI + R_max),
# equivalently 1 - RDD/RDI. Confirmed against ?setBevertonHolt and numerically
# against getReproductionLevel(). RDD/RDI is its complement and reads backwards.
#
# THE RDD ITSELF comes from ther_rdd() in the shim, not getRDD(), because
# getRDD() defaults to t = 0 and under therMizer that indexes ocean_temp at the
# wrong year -- the same defect as getDiet(). See the shim header for the two
# API traps it works around (mizerRDI() called directly returns zeros).
#
# USAGE  Rscript "Krill counterfactual scenarios/KC16_recruitment_timeseries.R"
# ENV    KC16_IN, KC16_SUF, KC16_SPECIES (default: all), KC_FORCE
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
})

OUT_LARGE <- "Output_large_files/wmin_test"
KC_ROOT <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
ANA <- file.path(KC_ROOT, "analysis"); FIG <- file.path(KC_ROOT, "figures")
for (d in c(ANA, FIG)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
IN <- Sys.getenv("KC16_IN", file.path(OUT_LARGE, "KC14_whres000_n167.rds"))
SUF <- Sys.getenv("KC16_SUF", "_whres000")
FORCE <- nzchar(Sys.getenv("KC_FORCE"))
if (!file.exists(IN))
  stop("missing ", IN, " -- run KC14_ensemble_whres000.R first", call. = FALSE)

D <- readRDS(IN); meta <- D$meta
if (is.null(D$recruit))
  stop("this input carries no `recruit` table -- it predates KC14", call. = FALSE)
cat("=== KC16: annual recruitment by scenario ===\n")
cat("input:", basename(IN), "| base:", basename(meta$base), "\n")

# KC16b: filter to USABLE = stable AND admissible, matching F00g. KC16 filters on
# `stable` alone, which was a no-op on the phase-61 ensemble (every member was
# admissible, so stable == usable == 122) but is NOT on ensembles where the
# reproduction treatment pushes erepro >= 1: phase 67 is 96 stable but only 32
# usable. A single inadmissible member is enough to distort a median.
adm <- if ("n_erepro_ge1" %in% names(D$members)) D$members$n_erepro_ge1 == 0 else TRUE
keep <- D$members$sim_index[D$members$stable & adm]
cat("members:", nrow(D$members), "| stable:", sum(D$members$stable),
    "| stable AND admissible (USED):", length(keep), "\n")
if (!length(keep)) stop("no usable members", call. = FALSE)
REC <- D$recruit %>% filter(sim_index %in% keep)
MECH <- D$mech %>% filter(sim_index %in% keep)
n_mem <- length(unique(REC$sim_index))

SPSEL <- Sys.getenv("KC16_SPECIES", "")
SP_ALL <- unique(REC$Species)
SPSEL <- if (nzchar(SPSEL)) trimws(strsplit(SPSEL, ",")[[1]]) else SP_ALL
if (length(setdiff(SPSEL, SP_ALL)))
  stop("unknown species: ", paste(setdiff(SPSEL, SP_ALL), collapse = ", "),
       call. = FALSE)
# Keep the model's own species order -- smallest to largest -- so the panel grid
# reads as a size gradient rather than alphabetically.
SPSEL <- SP_ALL[SP_ALL %in% SPSEL]

ARMS <- c("no_krill", "exploited", "peak_krill")
SCEN <- c(no_krill = "No krill fishing", exploited = "Observed history",
          peak_krill = sprintf("Peak krill effort held from %d", meta$peak_year))
SCEN_COL <- c("#2a78d6", "#0b0b0b", "#e34948"); names(SCEN_COL) <- unname(SCEN)
LW <- c(exploited = 2.0, peak_krill = 1.2, no_krill = 0.6)

eff <- readRDS("effort_array_1841_2010.rds")
yr <- as.numeric(rownames(eff))
onset <- function(s) { y <- yr[eff[, s] > 0]; if (length(y)) min(y) else NA }
whaling_start <- min(c(onset("baleen whales"), onset("sperm whales")),
                     na.rm = TRUE)
krill_start <- onset("antarctic krill")

# --- panel a: RDD relative to each member's own unexploited arm ---------------
W <- REC %>% filter(Species %in% SPSEL) %>%
  select(sim_index, arm, Year, Species, rdd) %>%
  pivot_wider(names_from = arm, values_from = rdd) %>%
  filter(is.finite(unexploited), unexploited > 0)
RAT <- W %>% pivot_longer(all_of(ARMS), names_to = "arm", values_to = "rdd") %>%
  filter(is.finite(rdd)) %>%
  mutate(ratio = rdd / unexploited)
SA <- RAT %>% group_by(Species, arm, Year) %>%
  summarise(med = median(ratio, na.rm = TRUE),
            lo = quantile(ratio, 0.25, na.rm = TRUE),
            hi = quantile(ratio, 0.75, na.rm = TRUE), .groups = "drop") %>%
  mutate(Species = factor(Species, levels = SPSEL),
         scen = factor(SCEN[arm], levels = unname(SCEN)))

# --- panel b: reproduction level, the ceiling that gates the response ---------
# RDD/R_max at the calibrated state, one value per member-species, so it is a
# distribution over members rather than a trajectory.
RL <- MECH %>% filter(Species %in% SPSEL) %>%
  mutate(Species = factor(Species, levels = SPSEL))

cat("\n=== reproduction level (RDD / R_max; 1 = pinned at R_max) ===\n")
print(as.data.frame(RL %>% group_by(Species) %>%
  summarise(median = round(median(repro_level, na.rm = TRUE), 4),
            q25 = round(quantile(repro_level, 0.25, na.rm = TRUE), 4),
            q75 = round(quantile(repro_level, 0.75, na.rm = TRUE), 4),
            .groups = "drop")), row.names = FALSE)

cat("\n=== RDD, peak krill vs observed, 2001-2010 paired median (% change) ===\n")
PC <- W %>% filter(Year %in% 2001:2010) %>%
  group_by(sim_index, Species) %>%
  summarise(across(all_of(ARMS), mean), .groups = "drop") %>%
  mutate(pct_peak = 100 * (peak_krill / exploited - 1),
         pct_nokr = 100 * (no_krill / exploited - 1))
print(as.data.frame(PC %>% group_by(Species) %>%
  summarise(`peak vs obs %` = signif(median(pct_peak), 3),
            `no-krill vs obs %` = signif(median(pct_nokr), 3),
            negative = sprintf("%d/%d", sum(pct_peak < 0), n()),
            .groups = "drop")), row.names = FALSE)

# --- absolute recruitment, for scale ------------------------------------------
cat("\n=== absolute RDD, unexploited arm, 2001-2010 median (eggs/yr) ===\n")
print(as.data.frame(REC %>%
  filter(Species %in% SPSEL, arm == "unexploited", Year %in% 2001:2010) %>%
  group_by(Species) %>%
  summarise(rdd = signif(median(rdd), 4), .groups = "drop")), row.names = FALSE)

# =============================================================================
# figure
# =============================================================================
lay <- function(a) geom_line(data = filter(SA, arm == a),
                             aes(Year, med, colour = scen), linewidth = LW[[a]])
pA <- ggplot() +
  geom_hline(yintercept = 1, colour = "grey45", linetype = "dashed",
             linewidth = 0.4) +
  geom_vline(xintercept = c(whaling_start, krill_start), colour = "grey55",
             linetype = "dotted", linewidth = 0.4) +
  geom_ribbon(data = filter(SA, arm == "exploited"),
              aes(Year, ymin = lo, ymax = hi), fill = "grey55", alpha = 0.20) +
  lay("exploited") + lay("peak_krill") + lay("no_krill") +
  facet_wrap(~Species, ncol = 4, scales = "free_y") +
  scale_colour_manual(values = SCEN_COL, name = "Scenario",
                      limits = unname(SCEN),
                      guide = guide_legend(override.aes = list(linewidth = 1.4))) +
  coord_cartesian(xlim = c(1925, 2012)) +
  scale_x_continuous(breaks = seq(1930, 2010, 40)) +
  theme_bw(base_size = 9) +
  theme(panel.grid = element_blank(), legend.position = "bottom",
        strip.background = element_rect(fill = "grey96", colour = "grey70"),
        strip.text = element_text(face = "bold", size = 7.5)) +
  labs(x = "Year", y = "Recruitment / unexploited recruitment (ratio)",
       title = "a  Annual recruitment (RDD) relative to the unexploited arm",
       subtitle = paste("median over", n_mem,
                        "stable members; grey = observed-history IQR"))

pB <- ggplot(RL, aes(x = repro_level, y = Species)) +
  geom_vline(xintercept = c(0, 1), colour = "grey75", linewidth = 0.3) +
  geom_boxplot(outlier.size = 0.4, linewidth = 0.35, fill = "grey88",
               colour = "grey25") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_discrete(limits = rev(SPSEL)) +
  theme_bw(base_size = 9) +
  theme(panel.grid.major.y = element_line(colour = "grey93"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Reproduction level  (RDD / R_max)", y = NULL,
       title = "b  How much recruitment is density-dependent",
       subtitle = "1 = pinned at R_max, so numbers cannot track food")

fig <- pA + pB + plot_layout(widths = c(2.6, 1))
stem <- paste0("KC_recruitment_timeseries", SUF)
for (ext in c("png", "pdf")) {
  f <- file.path(FIG, paste0(stem, ".", ext))
  if (file.exists(f) && !FORCE)
    stop("refusing to overwrite: ", f, "\n  set KC_FORCE=1", call. = FALSE)
}
H <- max(8, 1.5 * ceiling(length(SPSEL) / 4) + 3)
suppressWarnings({
  ggsave(file.path(FIG, paste0(stem, ".png")), fig, width = 16, height = H,
         dpi = 300, limitsize = FALSE)
  ggsave(file.path(FIG, paste0(stem, ".pdf")), fig, width = 16, height = H,
         limitsize = FALSE)
})
cat("\nwrote", paste0(stem, ".{png,pdf}"), "\n")

# =============================================================================
# second figure -- the ABSOLUTE numbers, historical arm only
# =============================================================================
# The figure above is deliberately a ratio, so the scenarios are comparable
# across groups that differ by 19 orders of magnitude in egg number. That
# removes the one thing a reader also wants: how many recruits there actually
# are. This is that, for the observed-history arm alone -- no scenario contrast,
# because the counterfactuals are already covered above and three superimposed
# lines would only obscure the level.
ABS <- REC %>% filter(Species %in% SPSEL, arm == "exploited") %>%
  group_by(Species, Year) %>%
  summarise(med = median(rdd, na.rm = TRUE),
            lo = quantile(rdd, 0.25, na.rm = TRUE),
            hi = quantile(rdd, 0.75, na.rm = TRUE), .groups = "drop") %>%
  mutate(Species = factor(Species, levels = SPSEL))

pC <- ggplot(ABS, aes(Year, med)) +
  geom_vline(xintercept = c(whaling_start, krill_start), colour = "grey55",
             linetype = "dotted", linewidth = 0.4) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey50", alpha = 0.28) +
  geom_line(linewidth = 0.8, colour = "#0b0b0b") +
  facet_wrap(~Species, ncol = 4, scales = "free_y") +
  coord_cartesian(xlim = c(1925, 2012)) +
  scale_x_continuous(breaks = seq(1930, 2010, 20)) +
  # free_y per panel: egg number spans ~20 orders of magnitude across groups,
  # so a shared axis would flatten every panel except mesozooplankton.
  # 7 significant digits. Several groups vary by far less than 1% of their own
  # level -- toothfishes and squids by ~0.02% -- so a short label collapses
  # every break to the same string and the axis says nothing. Measured over all
  # 19 panels: 2 digits collapses 17 of them, 4 digits 4, 6 digits none, 7 none.
  # 7 is chosen over 6 because format "g" drops trailing zeros, so it is both
  # complete AND shorter at the top end (9 vs 11 characters).
  scale_y_continuous(labels = function(v) formatC(v, format = "g", digits = 7)) +
  theme_bw(base_size = 9) +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey96", colour = "grey70"),
        strip.text = element_text(face = "bold", size = 7.5)) +
  labs(x = "Year", y = expression("Recruitment  (eggs "*yr^-1*")"),
       title = "Annual recruitment under the observed exploitation history",
       subtitle = paste("median over", n_mem,
                        "stable members, interquartile range shaded;",
                        "note the per-panel y scale"))

stem2 <- paste0("KC_recruitment_absolute", SUF)
for (ext in c("png", "pdf")) {
  f <- file.path(FIG, paste0(stem2, ".", ext))
  if (file.exists(f) && !FORCE)
    stop("refusing to overwrite: ", f, "\n  set KC_FORCE=1", call. = FALSE)
}
H2 <- max(7, 1.7 * ceiling(length(SPSEL) / 4) + 1.5)
suppressWarnings({
  ggsave(file.path(FIG, paste0(stem2, ".png")), pC, width = 12, height = H2,
         dpi = 300, limitsize = FALSE)
  ggsave(file.path(FIG, paste0(stem2, ".pdf")), pC, width = 12, height = H2,
         limitsize = FALSE)
})
cat("wrote", paste0(stem2, ".{png,pdf}"), "\n")
write.csv(ABS, file.path(ANA, paste0("KC16_rdd_absolute_exploited", SUF, ".csv")),
          row.names = FALSE)

write.csv(SA, file.path(ANA, paste0("KC16_rdd_series", SUF, ".csv")),
          row.names = FALSE)
write.csv(PC, file.path(ANA, paste0("KC16_rdd_paired", SUF, ".csv")),
          row.names = FALSE)
cat("KC16 complete.\n")