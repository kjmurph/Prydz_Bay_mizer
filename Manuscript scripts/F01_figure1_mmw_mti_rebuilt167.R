# =============================================================================
# FIGURE 1 (DUAL-INDICATOR VERSION) -- rebuilt ensemble 44, cut A, 167 members.
#
#   A  Observed catch, stacked by species          (identical to F01_figure1_mmw)
#   B  TWO indicators of the same catch on one panel, same x axis as A:
#        left  axis, log10   mean maximum body mass  (t)      -- grey, circles
#        right axis, linear  mean trophic level               -- blue, triangles
#
# WHAT THIS IS. The overlay of F01_figure1_mmw_rebuilt167.R's panel B and
# F01_figure1_mti_rebuilt167.R's panel B. Panel A, the vlines, the colour key,
# the zero-catch handling and the era break are unchanged from both; read the
# mean-maximum-mass script's header for those decisions. Both series are
# catch-biomass weighted arithmetic means over the same nine exploited groups
# and the same years, so they differ only in what is being averaged:
#
#     MMW(y) = sum_i C_iy * w_max,i / sum_i C_iy      w_max from the ensemble traits
#     MTI(y) = sum_i C_iy * TL_i    / sum_i C_iy      TL from McCormack et al. 2020
#
# THE POINT OF PUTTING THEM TOGETHER is that they disagree, and the disagreement
# is the result. Mean maximum body mass falls 16.5-fold across the 1970 break
# while mean trophic level falls 0.065 units. Prydz Bay was fished down the size
# spectrum without being fished down the food web: minke whales (TL 3.955)
# replaced large baleen whales (TL 3.867) at the same trophic level and one
# seventeenth of the asymptotic mass. This is the known blind spot of the marine
# trophic index, shown directly rather than argued.
#
# ------------------------------------------------------------------------------
# HOW THE TWO AXES ARE ANCHORED, AND WHY IT MATTERS
#
# A dual axis has one free choice -- the affine map from one scale to the other
# -- and it sets how the two curves appear to compare. Any choice is a
# convention; this one is stated rather than tuned by eye:
#
#     EACH AXIS SPANS EXACTLY THE RANGE ITS METRIC CAN TAKE ON THIS RECORD,
#     i.e. the range covered by the nine exploited groups themselves.
#
# The mass axis runs over [w_max(antarctic krill), w_max(baleen whales)] and the
# trophic axis over [TL(antarctic krill), TL(sperm whales)] -- the lowest and
# highest exploited group on each metric -- and the two are mapped onto the same
# vertical extent. So a curve's height on the panel reads as "how far through the
# available range of this indicator the catch has moved", which is comparable
# between the two, and the flatness of the trophic series is a statement about
# the exploited species pool rather than an artefact of a zoom level. The
# constants are derived at run time (see MASS_PER_TL / MASS_AT_TL0) and printed.
#
# WHAT THIS ANCHORING DOES NOT MEAN: the vertical GAP between the two curves
# carries no information -- it is fixed by the convention above, and so are the
# crossings from 1970 on; they are consequences of the anchoring, not events.
# Only the SHAPES are comparable. An alternative anchoring, both series
# normalised to their 1930 value so the panel reads as relative decline, was
# considered; it
# makes the contrast starker but costs the interpretable right-hand axis, so it
# is not used here.
#
# NO SPECIES REFERENCE LINES. Each single-indicator sibling keys its panel with
# nine dotted lines at the exploited groups' w_max / TL; both sets on one panel
# is eighteen lines behind two series. The two series are labelled directly and
# the axes are colour-coded to them instead. If the reference lines are what is
# wanted, use the sibling figures -- they carry the same series.
#
# Writes Manuscript figures/fig1_mmw_mti_rebuilt167.{png,pdf}
#        Manuscript data/fig1_mmw_mti_series_rebuilt167.csv
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
COL_W <- "grey25"               # mean maximum body mass: series, axis, ticks
COL_T <- "#1a6faf"              # mean trophic level:     series, axis, ticks
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
# Fixed per group, no size or time dependence -- which is what keeps panel B
# observational, like A. Transcribed from ecosystem_assessment_v3.R:219-230
# rather than sourced, because that file runs a full assessment on load.
# The model's own trophic levels are deliberately NOT used: they run 0.01-1.59
# above Ecopath by species (Reference model diet assessment/analysis/
# RD02_trophic_level_summary.csv) and mizer::getTrophicLevel() is broken under
# therMizer.
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
SP <- readRDS(file.path(DATA, sprintf("spectra_ref_period_%s.rds", SUF)))
wmax <- setNames(SP$traits$w_max, SP$traits$species)      # g

spp <- setdiff(names(obs_raw), "Year")
stopifnot(all(spp %in% names(wmax)), all(spp %in% names(TROPHIC_LEVELS)))
C <- vapply(obs_raw[spp], function(v) {
  x <- suppressWarnings(as.numeric(v)); x[is.na(x)] <- 0; pmax(x, 0)
}, numeric(nrow(obs_raw)))
tot <- rowSums(C)

# Years with no reported catch leave both ratios undefined -- NA, not 0, so both
# lines break together. 1941-45 (the war) and 2005/2009/2011/2012 are blank.
IND <- data.frame(
  Year     = obs_raw$Year,
  total_kt = tot / 1e9,
  mmw_t    = ifelse(tot > 0, as.vector(C %*% wmax[spp]) / tot, NA) / 1e6,
  mti      = ifelse(tot > 0, as.vector(C %*% TROPHIC_LEVELS[spp]) / tot, NA)
) %>% filter(Year >= X_LIM[1], Year <= X_LIM[2])

# --- the affine map between the two axes --------------------------------------
# See the header. Derived from the exploited set, not hand-tuned: the lowest and
# highest exploited group on each metric are pinned to the same two heights.
REF_TAXA <- names(which(colSums(eff) > 0))
stopifnot(setequal(REF_TAXA, caught))
w_rng  <- log10(range(wmax[REF_TAXA] / 1e6))       # krill .. large baleen whale
tl_rng <- range(TROPHIC_LEVELS[REF_TAXA])          # krill .. sperm whale
MASS_PER_TL <- diff(w_rng) / diff(tl_rng)          # log10 t per trophic unit
MASS_AT_TL0 <- w_rng[1] - MASS_PER_TL * tl_rng[1]  # log10 t at TL = 0
tl2mass <- function(tl) 10^(MASS_AT_TL0 + MASS_PER_TL * tl)  # TL   -> left axis
mass2tl <- function(m)  (log10(m) - MASS_AT_TL0) / MASS_PER_TL  # left axis -> TL
stopifnot(all.equal(mass2tl(tl2mass(tl_rng)), tl_rng))

cat("\n=== dual-axis anchoring ===\n")
cat(sprintf("  mass  axis pinned to %.3g - %.3g t   (antarctic krill - baleen whales)\n",
            10^w_rng[1], 10^w_rng[2]))
cat(sprintf("  troph axis pinned to %.3f - %.3f     (antarctic krill - sperm whales)\n",
            tl_rng[1], tl_rng[2]))
cat(sprintf("  map: log10(t) = %.4f + %.4f * TL\n", MASS_AT_TL0, MASS_PER_TL))

# --- summary statistics quoted in the manuscript text -------------------------
# Break at the last whaling-dominated year, 1970, as in both siblings.
e1 <- IND$Year <= 1970; e2 <- IND$Year >= 1971 & IND$Year <= 2010
k  <- is.finite(IND$mmw_t) & IND$Year <= 2010
sm <- function(v, sel) median(v[sel & is.finite(v)])
m1 <- sm(IND$mmw_t, e1); m2 <- sm(IND$mmw_t, e2)
t1 <- sm(IND$mti,   e1); t2 <- sm(IND$mti,   e2)
fw <- lm(log10(mmw_t) ~ Year, data = IND[k, ])   # log trend: mass spans decades
ft <- lm(mti          ~ Year, data = IND[k, ])   # linear:    one TL unit = one transfer

cat("\n=== the two indicators, same catch, same years ===\n")
cat(sprintf("  mean maximum body mass  %.3g t -> %.3g t   (%.1fx, %.2f log10 units)\n",
            m1, m2, m1 / m2, log10(m1 / m2)))
cat(sprintf("    trend %+.3f log10 units per decade (p = %.2g, R2 = %.2f), range %.3g to %.3g t\n",
            coef(fw)[2] * 10, summary(fw)$coefficients[2, 4], summary(fw)$r.squared,
            min(IND$mmw_t, na.rm = TRUE), max(IND$mmw_t, na.rm = TRUE)))
cat(sprintf("  mean trophic level      %.3f -> %.3f       (%+.3f trophic units)\n",
            t1, t2, t2 - t1))
cat(sprintf("    trend %+.4f units per decade (p = %.2g, R2 = %.2f), range %.3f to %.3f\n",
            coef(ft)[2] * 10, summary(ft)$coefficients[2, 4], summary(ft)$r.squared,
            min(IND$mti, na.rm = TRUE), max(IND$mti, na.rm = TRUE)))
cat(sprintf("  as a fraction of each indicator's exploited-species range:\n"))
cat(sprintf("    mass  %.1f%%   trophic %.1f%%\n",
            100 * log10(m1 / m2) / diff(w_rng), 100 * (t1 - t2) / diff(tl_rng)))

# The vertical range is the one the sibling mean-maximum-mass panel uses, which
# also holds the whole trophic axis: it is set by three near-zero-catch years
# (1971, 1988 and 1993 -- the last is a 41-tonne total that is 99.8% krill), not
# by the whaling-to-krill transition. Nothing is smoothed,
# thresholded or clipped, and panel A carries the tonnage that says which years
# to trust. Expansion stays ON, otherwise A and B's x axes misalign.
y_lim <- c(10^-5.5, 10^2.15)
key_x <- X_LIM[1] + 1          # bottom-left corner is empty for both series

pB <- ggplot(IND, aes(Year)) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  # trophic level first, so the mass series -- the panel's primary -- sits on top
  geom_line(aes(y = tl2mass(mti)), linewidth = 0.55, colour = COL_T, na.rm = TRUE) +
  geom_point(aes(y = tl2mass(mti)), size = 1.5, shape = 17, colour = COL_T,
             na.rm = TRUE) +
  geom_line(aes(y = mmw_t), linewidth = 0.55, colour = COL_W, na.rm = TRUE) +
  geom_point(aes(y = mmw_t), size = 1.5, shape = 16, colour = COL_W, na.rm = TRUE) +
  # direct series labels: the axes are colour-coded to the series, but the two
  # curves cross, so the key is stated as well as implied. Sits in the
  # bottom-left, which neither series enters before 1965.
  annotate("point", x = key_x, y = 10^-4.30, shape = 16, size = 1.5, colour = COL_W) +
  annotate("text",  x = key_x + 2.5, y = 10^-4.30, hjust = 0, size = 2.9,
           colour = COL_W, label = "Mean maximum body mass (left axis)") +
  annotate("point", x = key_x, y = 10^-4.95, shape = 17, size = 1.5, colour = COL_T) +
  annotate("text",  x = key_x + 2.5, y = 10^-4.95, hjust = 0, size = 2.9,
           colour = COL_T, label = "Mean trophic level (right axis)") +
  scale_x_continuous(breaks = X_BRK, limits = X_LIM) +
  scale_y_log10(breaks = 10^seq(-5, 2, 1),
                labels = trans_format("log10", math_format(10^.x)),
                sec.axis = sec_axis(transform = ~ mass2tl(.),
                                    name = "Mean trophic level of catch",
                                    breaks = seq(2.5, 5.0, 0.5))) +
  annotation_logticks(sides = "l", linewidth = 0.25, colour = COL_W) +
  coord_cartesian(ylim = y_lim) +
  labs(x = "Year",
       y = expression("Mean maximum body mass of catch, "*w[max]*" (t)"),
       tag = "B") +
  theme_classic(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y.left  = element_text(colour = COL_W),
        axis.text.y.left   = element_text(colour = COL_W),
        axis.line.y.left   = element_line(colour = COL_W),
        axis.ticks.y.left  = element_line(colour = COL_W),
        axis.title.y.right = element_text(colour = COL_T),
        axis.text.y.right  = element_text(colour = COL_T),
        axis.line.y.right  = element_line(colour = COL_T),
        axis.ticks.y.right = element_line(colour = COL_T),
        plot.tag = element_text(face = "bold", size = 14))

# B contributes no guide, so the collected legend is entirely A's. B can go back
# to the 0.7 height of the mean-maximum-mass build: unlike the trophic-only
# sibling it carries no reference-line labels, so nothing constrains it from
# below.
fig <- (pA / pB) + plot_layout(heights = c(1, 0.7), guides = "collect") &
  theme(legend.position = "bottom")
png_out <- guard(file.path(FIGS, sprintf("fig1_mmw_mti_%s.png", SUF)))
pdf_out <- guard(file.path(FIGS, sprintf("fig1_mmw_mti_%s.pdf", SUF)))
ggsave(png_out, fig, width = 11, height = 10.5, dpi = 300, limitsize = FALSE)
ggsave(pdf_out, fig, width = 11, height = 10.5, limitsize = FALSE)
write.csv(IND, guard(file.path(DATA, sprintf("fig1_mmw_mti_series_%s.csv", SUF))),
          row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")