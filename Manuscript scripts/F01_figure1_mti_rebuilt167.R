# =============================================================================
# FIGURE 1 (MEAN-TROPHIC-INDEX VERSION) -- rebuilt ensemble 44, cut A, 167 members.
#
#   A  Observed catch, stacked by species          (identical to F01_figure1_mmw)
#   B  Mean trophic level of the observed catch, same x axis as A
#
# WHAT THIS IS. A sibling of F01_figure1_mmw_rebuilt167.R with panel B's
# indicator swapped from asymptotic mass to trophic level. Panel A, the vlines,
# the colour key, the zero-catch handling and the era break are all unchanged;
# read that script's header for those decisions, they are not repeated here.
#
#  1. THE INDICATOR IS THE MARINE TROPHIC INDEX (Pauly et al. 1998, Science
#     279:860; the CBD's MTI indicator), the catch-weighted mean trophic level
#
#         MTI(y) = sum_i C_iy * TL_i / sum_i C_iy
#
#     Weighting is by catch BIOMASS and the mean is arithmetic, exactly as for
#     the mean maximum body mass in the sibling script, so the two panels differ
#     only in what is being averaged.
#
#  2. TL_i ARE THE ASSIGNED (NOT MODELLED) TROPHIC LEVELS from McCormack et al.
#     (2020) Prydz Bay Ecopath, Table 2. They are fixed per group and carry no
#     size or time dependence, which is what makes this an observational panel
#     like A -- nothing here comes from the ensemble. Transcribed from
#     ecosystem_assessment_v3.R:219-230 rather than sourced, because that file
#     runs a full assessment on load; RD02_trophic_level.R transcribes the same
#     block for the same reason.
#
#     The model's own trophic levels are NOT used and should not be substituted
#     casually: they run 0.01-1.59 above Ecopath by species (see
#     Reference model diet assessment/analysis/RD02_trophic_level_summary.csv),
#     and mizer::getTrophicLevel() is outright broken under therMizer.
#
#  3. THE PANEL IS LINEAR, NOT LOG. Trophic level is already a log-like index
#     (one unit is one transfer), so log10 of it means nothing. This is the one
#     structural difference from the sibling panel and it is why the reference
#     lines are so much harder to place: on the mass axis the exploited groups
#     span 7.4 decades, on the trophic axis they span 2.94 units and three
#     PAIRS sit closer than 0.09 units (sperm/orca 0.041, squids/shelf 0.055,
#     minke/baleen 0.088). Label sides and offsets are therefore hand-set per
#     taxon (REF_SIDE, REF_VJ) with the pairs split across opposite ends of the
#     panel, and panel B is given the same height as A rather than 0.7 -- below
#     that the minke and baleen whale labels cannot clear their own lines.
#
#  4. THE RESULT, for the manuscript text: the mean trophic level of the catch
#     is essentially FLAT. The 1930-1970 median is 4.00 and the 1971-2010 median
#     is 3.94 -- a fall of 0.065 trophic units, against the 16.5-fold (1.22
#     log10 units) fall in mean maximum body mass over the same break. Prydz Bay
#     was fished down the size spectrum without being fished down the food web,
#     because the group that replaced the large baleen whales (minke, TL 3.955)
#     feeds at the same level as the group it replaced (baleen, TL 3.867) while
#     being 17 times smaller. The MTI's excursions are transitions of dominance,
#     not of trophic structure: the 1970 peak (5.23) is the most sperm-whale-
#     dominated year in the record (92.1% of that year's 11.8 kt; sperm whaling
#     itself runs on to 1979) and the 1993 floor (2.40) is a 41-tonne total that
#     is 99.8% krill.
#
#     THIS IS THE KNOWN BLIND SPOT OF THE MTI, not a defect of the data. Panel A
#     carries the tonnage that says which years to trust; nothing here is
#     smoothed, thresholded or clipped.
#
# Writes Manuscript figures/fig1_mti_rebuilt167.{png,pdf}
#        Manuscript data/fig1_mti_series_rebuilt167.csv
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(scales); library(grid)
})

DATA <- "Manuscript data"; FIGS <- "Manuscript figures"
dir.create(FIGS, showWarnings = FALSE)
SUF <- "rebuilt167"
X_LIM <- c(1920, 2012)          # panel A and B share this exactly
X_BRK <- seq(1920, 2010, 10)
guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE); f
}
meta <- readRDS(file.path(DATA, sprintf("meta_%s.rds", SUF)))
message("members: ", meta$n_members, " | cut: ", meta$cut)

sp_cols <- c("baleen whales" = "#FF61C3", "sperm whales" = "#DB72FB",
  "minke whales" = "#00B9E3", "orca" = "#619CFF", "toothfishes" = "#00C19F",
  "shelf and coastal fishes" = "#93AA00", "antarctic krill" = "#F8766D",
  "leopard seals" = "#E07B39", "medium divers" = "#2B6CB0",
  "large divers" = "#4C8FD0", "flying birds" = "#9E9E9E",
  "small divers" = "#BDBDBD", "mesopelagic fishes" = "#D39200",
  "bathypelagic fishes" = "#B07A00", "squids" = "#E8B33C",
  "mesozooplankton" = "#6A1B9A", "other krill" = "#8E44AD",
  "other macrozooplankton" = "#9B59B6", "salps" = "#C39BD3")
nice <- c("baleen whales" = "Large baleen whales", "sperm whales" = "Sperm whales",
  "minke whales" = "Minke whales", "orca" = "Orca", "toothfishes" = "Toothfishes",
  "shelf and coastal fishes" = "Shelf & coastal fishes",
  "antarctic krill" = "Antarctic krill")
pretty_lab <- function(s) ifelse(s %in% names(nice), unname(nice[s]),
                                 paste0(toupper(substr(s, 1, 1)), substring(s, 2)))

# --- assigned trophic levels: McCormack et al. (2020) Prydz Bay Ecopath, Tab 2 -
# See header note 2. Fixed per group; no size or time dependence.
TROPHIC_LEVELS <- c(
  "mesozooplankton" = 3.272, "other krill" = 2.398,
  "other macrozooplankton" = 3.231, "antarctic krill" = 2.398,
  "salps" = 2.284, "mesopelagic fishes" = 3.539,
  "bathypelagic fishes" = 4.055, "shelf and coastal fishes" = 4.281,
  "toothfishes" = 4.966, "flying birds" = 4.103,
  "small divers" = 3.787, "medium divers" = 4.999,
  "large divers" = 5.075, "leopard seals" = 4.858,
  "squids" = 4.336, "minke whales" = 3.955,
  "orca" = 5.301, "sperm whales" = 5.342,
  "baleen whales" = 3.867
)

eff <- readRDS("effort_array_1841_2010.rds"); yv <- as.numeric(rownames(eff))
onset <- function(s){y <- yv[eff[,s]>0]; if(length(y)) min(y) else NA}
whal <- min(c(onset("baleen whales"), onset("sperm whales")), na.rm = TRUE)
krl  <- onset("antarctic krill")

# ------------------------------------------------------------------ PANEL A ---
# Byte-for-byte the panel A of F01_figure1_mmw_rebuilt167.R.
obs_raw <- read.csv("yield_observed_timeseries.csv", check.names = FALSE)
obs_all <- obs_raw %>%
  pivot_longer(-Year, names_to = "Species", values_to = "t") %>%
  mutate(t = pmax(coalesce(suppressWarnings(as.numeric(t)), 0), 0) / 1e9) %>%
  filter(Year >= X_LIM[1], Year <= X_LIM[2])

caught <- obs_all %>% group_by(Species) %>% summarise(s = sum(t), .groups = "drop") %>%
  filter(s > 0) %>% pull(Species)
obs <- obs_all %>% filter(Species %in% caught) %>%
  mutate(lab = pretty_lab(Species))
lev <- obs %>% group_by(lab) %>% summarise(s = sum(t), .groups = "drop") %>%
  arrange(desc(s)) %>% pull(lab)
lab2sp <- obs %>% distinct(lab, Species) %>% { setNames(.$Species, .$lab) }
obs$lab <- factor(obs$lab, levels = lev)

acols <- setNames(unname(sp_cols[lab2sp[lev]]), lev)
acols[is.na(acols)] <- "grey60"
stopifnot(!anyDuplicated(acols))

pA <- ggplot(obs, aes(Year, t, fill = lab)) +
  geom_area(colour = NA) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  annotate("text", x = whal, y = Inf, label = "Whaling\nstarts", hjust = 1.08,
           vjust = 1.3, size = 3, colour = "grey35") +
  annotate("text", x = krl, y = Inf, label = "Krill\nfishing starts", hjust = 1.08,
           vjust = 1.3, size = 3, colour = "grey35") +
  scale_fill_manual(values = acols, name = NULL) +
  scale_x_continuous(breaks = X_BRK, limits = X_LIM) +
  labs(x = NULL, y = expression("Catch (10"^3~"t y"^-1*")"), tag = "A") +
  theme_classic(base_size = 11) +
  theme(legend.position = "bottom", panel.grid = element_blank(),
        axis.text.x = element_blank(),
        plot.tag = element_text(face = "bold", size = 14)) +
  guides(fill = guide_legend(nrow = 2, order = 1))

# ------------------------------------------------------------------ PANEL B ---
# MTI(y) = sum_i C_iy * TL_i / sum_i C_iy, catch-biomass weighted, arithmetic.
spp <- setdiff(names(obs_raw), "Year")
stopifnot(all(spp %in% names(TROPHIC_LEVELS)))
C <- vapply(obs_raw[spp], function(v) {
  x <- suppressWarnings(as.numeric(v)); x[is.na(x)] <- 0; pmax(x, 0)
}, numeric(nrow(obs_raw)))
tot <- rowSums(C)

# Years with no reported catch leave the ratio undefined -- NA, not 0, so
# geom_line breaks. 1941-45 (the war) and 2005/2009/2011/2012 are blank.
MTI <- data.frame(Year = obs_raw$Year,
                  total_kt = tot / 1e9,
                  mti = ifelse(tot > 0, as.vector(C %*% TROPHIC_LEVELS[spp]) / tot,
                               NA)) %>%
  filter(Year >= X_LIM[1], Year <= X_LIM[2])

# --- summary statistics quoted in the panel -----------------------------------
# Same break as the sibling script: the last whaling-dominated year, 1970.
e1 <- MTI$Year <= 1970 & is.finite(MTI$mti)
e2 <- MTI$Year >= 1971 & MTI$Year <= 2010 & is.finite(MTI$mti)
m1 <- median(MTI$mti[e1]); m2 <- median(MTI$mti[e2])
k  <- is.finite(MTI$mti) & MTI$Year <= 2010
# LINEAR trend, not log: one trophic unit is one transfer, so the natural rate
# is trophic units per decade and log10(TL) has no interpretation.
fit <- lm(mti ~ Year, data = MTI[k, ])
rate <- coef(fit)[2] * 10
pval <- summary(fit)$coefficients[2, 4]

cat("\n=== mean trophic level of the catch (marine trophic index) ===\n")
cat(sprintf("  1930-1970 median  %.3f (n = %d)\n", m1, sum(e1)))
cat(sprintf("  1971-2010 median  %.3f (n = %d)\n", m2, sum(e2)))
cat(sprintf("  change            %+.3f trophic units\n", m2 - m1))
cat(sprintf("  linear trend      %+.4f trophic units per decade (p = %.2g, R2 = %.2f)\n",
            rate, pval, summary(fit)$r.squared))
cat(sprintf("  range             %.3f to %.3f (min %d, max %d)\n",
            min(MTI$mti, na.rm = TRUE), max(MTI$mti, na.rm = TRUE),
            MTI$Year[which.min(MTI$mti)], MTI$Year[which.max(MTI$mti)]))

# --- species trophic-level reference lines ------------------------------------
# EVERY EXPLOITED GROUP GETS ONE, so the panel's key is exactly panel A's legend.
# The set is read off the effort array and asserted against the catch record.
REF_TAXA <- names(which(colSums(eff) > 0))
stopifnot(setequal(REF_TAXA, caught))
REF_TAXA <- REF_TAXA[order(TROPHIC_LEVELS[REF_TAXA], decreasing = TRUE)]

# WHICH END OF THE PANEL EACH LABEL SITS AT, AND HOW FAR OFF ITS OWN LINE.
# Hand-set per taxon; see header note 3. The rules that produced these:
#   * the three close pairs are split across opposite ends, which is the only
#     thing that separates sperm/orca (0.041 units apart) at all;
#   * a label must clear every OTHER group's line as well as its own, since the
#     lines span the full width regardless of which end is labelled -- this is
#     what pushes shelf & coastal below its line (above it, it lands on the
#     squids line 0.055 units up);
#   * the left end is empty at all y before 1930, so anything can be labelled
#     there; the right end is empty except the 3.90-4.06 band held by the
#     1996-2010 points, which is why bathypelagic fishes is lifted further off
#     its line than the others (vj -0.9 rather than -0.45).
REF_SIDE <- c("sperm whales" = "left", "orca" = "right",
              "toothfishes" = "left", "squids" = "right",
              "shelf and coastal fishes" = "left",
              "bathypelagic fishes" = "right", "minke whales" = "left",
              "baleen whales" = "left", "antarctic krill" = "left")
REF_VJ   <- c("sperm whales" = -0.45, "orca" = 1.35,
              "toothfishes" = -0.45, "squids" = -0.45,
              "shelf and coastal fishes" = 1.35,
              "bathypelagic fishes" = -0.9, "minke whales" = -0.35,
              "baleen whales" = 1.35, "antarctic krill" = -0.45)
# If the exploited set ever changes, fail here rather than silently mislabel.
stopifnot(setequal(names(REF_SIDE), REF_TAXA), setequal(names(REF_VJ), REF_TAXA))

ref <- data.frame(Species = REF_TAXA,
                  y = unname(TROPHIC_LEVELS[REF_TAXA]),
                  lab = pretty_lab(REF_TAXA),
                  stringsAsFactors = FALSE) %>%
  mutate(side = unname(REF_SIDE[Species]),
         x  = ifelse(side == "left", X_LIM[1] + 1, X_LIM[2] - 1),
         hj = ifelse(side == "left", 0, 1),
         vj = unname(REF_VJ[Species]))
stopifnot(!any(is.na(sp_cols[ref$Species])))
cat("\n=== panel B trophic-level reference lines ===\n")
print(ref[, c("Species", "y", "side")], row.names = FALSE, digits = 4)

# The axis is NOT truncated. It runs from just below the krill line (2.398, the
# lowest exploited group and the 1993 floor) to just above the sperm whale line
# (5.342, the highest), so the panel shows the whole range the nine exploited
# groups can produce -- which is the point: the series only ever uses the middle
# of it. Expansion stays ON, as in the sibling script, so A and B's x axes align.
y_lim <- c(2.30, 5.45)

pB <- ggplot(MTI, aes(Year, mti)) +
  geom_hline(data = ref, aes(yintercept = y, colour = Species),
             linetype = "dotted", linewidth = 0.5, alpha = 0.85,
             show.legend = FALSE) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  geom_text(data = ref, aes(x = x, y = y, label = lab, hjust = hj, vjust = vj,
                            colour = Species),
            size = 2.6, show.legend = FALSE, inherit.aes = FALSE) +
  geom_line(linewidth = 0.55, colour = "grey30", na.rm = TRUE) +
  geom_point(size = 1.5, colour = "grey15", na.rm = TRUE) +
  scale_colour_manual(values = sp_cols, guide = "none") +
  scale_x_continuous(breaks = X_BRK, limits = X_LIM) +
  scale_y_continuous(breaks = seq(2.5, 5.5, 0.5)) +
  coord_cartesian(ylim = y_lim) +
  # NO IN-PANEL TEXT, as in the sibling script: the era medians and the trend go
  # to the console for the manuscript text, the dotted lines to the caption.
  labs(x = "Year", y = "Mean trophic level of catch", tag = "B") +
  theme_classic(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.tag = element_text(face = "bold", size = 14))

# B IS THE SAME HEIGHT AS A here, unlike the mean-maximum-mass build where it is
# 0.7. See header note 3: the minke (3.955) and large baleen whale (3.867) lines
# are 0.088 trophic units apart and both need a label that clears both lines, so
# the panel cannot be compressed the way a 7.4-decade log axis could be.
fig <- (pA / pB) + plot_layout(heights = c(1, 1), guides = "collect") &
  theme(legend.position = "bottom")
png_out <- guard(file.path(FIGS, sprintf("fig1_mti_%s.png", SUF)))
pdf_out <- guard(file.path(FIGS, sprintf("fig1_mti_%s.pdf", SUF)))
ggsave(png_out, fig, width = 11, height = 10.5, dpi = 300, limitsize = FALSE)
ggsave(pdf_out, fig, width = 11, height = 10.5, limitsize = FALSE)
write.csv(MTI, guard(file.path(DATA, sprintf("fig1_mti_series_%s.csv", SUF))),
          row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")