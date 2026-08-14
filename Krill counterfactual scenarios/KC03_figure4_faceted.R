# =============================================================================
# KC03 -- Figure 4, faceted by exploitation scenario
#
# The published Figure 4 (Manuscript scripts/F04_figure4_krill_ratio_*.R) plots
# exploited / unexploited Antarctic krill consumption by predator group, one
# panel. This is the same figure with one panel per SCENARIO, so the two krill
# counterfactuals can be read against the observed history.
#
# GEOMETRY TRANSCRIBED FROM F04 so the panels are directly comparable to the
# published figure: IQR ribbons, median lines, the same three-group palette, the
# same era markers derived from the effort array, and the same per-group +-1 SD
# natural-variability reference lines.
#
# THREE DEPARTURES, all requested:
#   1. faceted by scenario rather than a single panel;
#   2. x axis starts at 1925, first tick and label at 1930, decades thereafter;
#   3. the "Unexploited" inset text label is removed.
#
# ONE DEPARTURE THAT IS NOT COSMETIC AND IS NOT REQUESTED -- READ THIS.
#   F04 builds its +-1 SD band from krill_baseline_1841_unexploited_*.rds, over
#   the full 1841-2010 window. KC02 extracts diet from 1900 only, so the band
#   here is computed over 1900-2010. F04's own header measured that window
#   change on the rebuilt data: 1.11x for whales, 0.93x for the other two
#   groups. Small, but the bands here are NOT numerically identical to the
#   published ones and should not be quoted as if they were. Extending KC02's
#   DIET_FROM to 1841 would close this.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC03_figure4_faceted.R"
# ENV    KC_IN (default KC02_pilot_scenarios.rds), KC_SUF, KC_FORCE
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
})

KC_ROOT <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
ANA <- file.path(KC_ROOT, "analysis"); FIG <- file.path(KC_ROOT, "figures")
dir.create(FIG, recursive = TRUE, showWarnings = FALSE)
IN  <- Sys.getenv("KC_IN", "KC02_pilot_scenarios.rds")
SUF <- Sys.getenv("KC_SUF", "")
FORCE <- nzchar(Sys.getenv("KC_FORCE"))

D <- readRDS(file.path(ANA, IN))
KR <- D$krill; meta <- D$meta
cat("=== KC03: Figure 4 faceted by scenario ===\n")
cat("input:", IN, "| params:", basename(meta$params), "\n")

# stable members only -- see KC01. A divergent member's ratio is meaningless.
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
SCEN <- c(no_krill = "No krill fishing", exploited = "Observed history",
          peak_krill = sprintf("Peak krill effort held from %d",
                               meta$peak_year))

# --- era markers, derived from the effort array (F04:58-70) -------------------
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

# --- ratio per member-year, per scenario, then across members (F04:74-90) -----
grp_ratio <- function(species, label) {
  KR %>% filter(Species %in% species) %>%
    group_by(sim_index, arm, Year) %>%
    summarise(cons = sum(krill_consumed), .groups = "drop") %>%
    pivot_wider(names_from = arm, values_from = cons) %>%
    filter(is.finite(unexploited), unexploited > 0) %>%
    pivot_longer(all_of(names(SCEN)), names_to = "scen", values_to = "cons") %>%
    filter(is.finite(cons)) %>%
    mutate(ratio = cons / unexploited, group = label)
}
R <- bind_rows(grp_ratio(ALL_PRED, GRP_LEV[1]), grp_ratio(WHALES, GRP_LEV[2]),
               grp_ratio(FISHES, GRP_LEV[3]))
S <- R %>% group_by(scen, group, Year) %>%
  summarise(med = median(ratio, na.rm = TRUE),
            lo = quantile(ratio, 0.25, na.rm = TRUE),
            hi = quantile(ratio, 0.75, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = factor(group, levels = GRP_LEV),
         scen = factor(SCEN[scen], levels = unname(SCEN)))

# --- per-group +-1 SD natural-variability band (F04:139-160) ------------------
# The canonical SNR denominator: the temporal SD of the across-member MEAN
# unexploited trajectory, divided by that curve's own mean to put it on the
# multiplicative ratio scale. One scalar per group, scenario-independent by
# construction -- the unexploited arm is the same run in every panel.
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

# --- annotation frames: event text on the TOP facet only ----------------------
TOP <- factor(unname(SCEN)[1], levels = unname(SCEN))
ev_lab <- data.frame(
  x = c(whaling_start, krill_start, krill_end),
  lab = c("Whaling\nstarts", "Krill fishing\nstarts", "End of\nkrill fishing"),
  scen = TOP)
pk_lab <- data.frame(x = as.numeric(pk), lab = names(pk), scen = TOP)

X_MIN <- 1925
cols <- c("black", "#FF3E96", "#D9A404"); names(cols) <- GRP_LEV

p <- ggplot(S, aes(Year, med, colour = group, fill = group)) +
  geom_hline(yintercept = 1, colour = "grey45", linetype = "dashed",
             linewidth = 0.5) +
  geom_vline(xintercept = c(whaling_start, krill_start, krill_end),
             colour = "grey40", linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = as.numeric(pk), colour = "grey55",
             linetype = "dotted", linewidth = 0.4) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.25, colour = NA) +
  geom_hline(data = BAND, aes(yintercept = lo, colour = group),
             linetype = "dashed", linewidth = 0.5, show.legend = FALSE,
             inherit.aes = FALSE) +
  geom_hline(data = BAND, aes(yintercept = hi, colour = group),
             linetype = "dashed", linewidth = 0.5, show.legend = FALSE,
             inherit.aes = FALSE) +
  geom_line(linewidth = 1) +
  geom_text(data = ev_lab, aes(x = x, y = Inf, label = lab), inherit.aes = FALSE,
            hjust = -0.06, vjust = 1.25, size = 3.1, colour = "grey30") +
  geom_text(data = pk_lab, aes(x = x, y = 0.02, label = lab), inherit.aes = FALSE,
            angle = 90, hjust = 0, vjust = -0.3, size = 2.7, colour = "grey45") +
  facet_wrap(~scen, ncol = 1) +
  scale_colour_manual(values = cols, name = "Predator group") +
  scale_fill_manual(values = cols, name = "Predator group") +
  scale_x_continuous(breaks = seq(1930, 2010, 10), limits = c(X_MIN, 2012),
                     expand = expansion(mult = 0.005)) +
  scale_y_continuous(limits = c(0, 1.15), breaks = seq(0, 1, 0.25)) +
  labs(x = "Year",
       y = "Scenario / Unexploited Antarctic krill consumption (ratio)") +
  theme_classic(base_size = 13) +
  theme(legend.position = "bottom", panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey96", colour = "grey70"),
        strip.text = element_text(face = "bold", size = 11))

stem <- paste0("KC_fig4_faceted_by_scenario", SUF)
for (ext in c("png", "pdf")) {
  f <- file.path(FIG, paste0(stem, ".", ext))
  if (file.exists(f) && !FORCE)
    stop("refusing to overwrite: ", f, "\n  set KC_FORCE=1", call. = FALSE)
}
suppressWarnings({
  ggsave(file.path(FIG, paste0(stem, ".png")), p, width = 11, height = 15,
         dpi = 300)
  ggsave(file.path(FIG, paste0(stem, ".pdf")), p, width = 11, height = 15)
})
cat("\nwrote", stem, ".png and .pdf (", n_mem, "members )\n")

cat("\n=== ratio at 2010, by scenario and group ===\n")
print(as.data.frame(S %>% filter(Year == 2010) %>%
  select(scen, group, med, lo, hi) %>% arrange(group, scen)),
  digits = 4, row.names = FALSE)

cat("\n=== 2001-2010 mean, and each scenario's departure from observed ===\n")
ref <- S %>% filter(Year %in% 2001:2010) %>%
  group_by(scen, group) %>% summarise(med = mean(med), .groups = "drop") %>%
  pivot_wider(names_from = scen, values_from = med)
names(ref) <- make.names(names(ref))
print(as.data.frame(ref %>% mutate(across(where(is.numeric), ~round(.x, 4)))),
      row.names = FALSE)

write.csv(S, file.path(ANA, paste0("KC03_fig4_series", SUF, ".csv")),
          row.names = FALSE)
cat("\nKC03 complete.\n")