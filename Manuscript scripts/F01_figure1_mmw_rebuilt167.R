# =============================================================================
# FIGURE 1 (MEAN-MAXIMUM-SIZE VERSION) -- rebuilt ensemble 44, cut A, 167 members.
#
#   A  Observed catch, stacked by species
#   B  Mean maximum body mass of the observed catch, same x axis as A
#
# CHANGES FROM F01_figure1_rebuilt167.R (Kieran, 2026-08-04):
#
#  1. X AXIS STARTS AT 1920 (was 1898). The record's first catch is 1930, so the
#     old panel spent a quarter of its width on empty years. 1920 keeps a short
#     run-in before the whaling vline rather than butting the area against the
#     axis.
#
#  1b. BUG FIX CARRIED INTO PANEL A, otherwise unchanged: the old script filtered
#     the long-format catch on t > 0, which removed the zero-catch years from the
#     data rather than plotting them as zero. geom_area then drew a straight
#     interpolation across 1941-1945 -- the wartime shutdown of Antarctic whaling
#     appeared as a smooth decline -- and across 2005, 2009, 2011 and 2012. Panel
#     B's gaps had no counterpart in A. Both panels now show the same blanks:
#     A drops to zero, B breaks (the ratio is undefined at zero catch).
#
#  2. THE BOTTOM ROW IS NOW A SINGLE PANEL. The realised size spectrum (old B)
#     and the lambda trajectories (old C) are both gone; the new B is a time
#     series on A's x axis, so the two panels read as one record.
#
#  3. THE NEW PANEL B IS THE MEAN MAXIMUM BODY MASS OF THE CATCH, the
#     catch-weighted mean of each group's asymptotic mass,
#
#         MMW(y) = sum_i C_iy * w_max,i / sum_i C_iy
#
#     after Cheung et al.'s mean-temperature-of-the-catch / assemblage-averaged
#     maximum body weight construction (Nature 497:365; Nature Clim. Change
#     3:254). Weighting is by catch BIOMASS, arithmetic, exactly as there -- not
#     by numbers, and not a geometric mean. The geometric version was computed
#     and is far noisier (7.4 decades of range against 6.8) because it lets a
#     small krill tonnage pull the mean down through several decades of mass;
#     the arithmetic mean is dominated by whatever carries the tonnage, which is
#     the quantity the stacked area in panel A shows.
#
#     IT IS AN OBSERVATIONAL PANEL, like A. The rebuilt-167 data products carry
#     no per-species modelled yield series (F00 saves biomass, abundance, NBSS
#     slope and krill consumption only), so there is no modelled counterpart to
#     overlay without re-projecting the ensemble.
#
#  4. THE PANEL IS KEYED BY SPECIES w_max REFERENCE LINES rather than by era
#     means. Drawing the era medians was tried and abandoned: the whaling-era
#     median (96.8 t) sits 0.03 log10 units from the large-baleen-whale w_max
#     (103 t) and the post-1971 median (5.87 t) sits 0.01 from the minke w_max
#     (6.0 t), so the segments were indistinguishable from the reference lines.
#     That coincidence IS the result -- the early catch was essentially pure
#     large baleen whale and the modern catch is essentially pure minke -- so it
#     is carried by the reference lines plus the text block, and the numbers are
#     quoted rather than drawn.
#
#     Orca is omitted from the reference lines. It is 0.03% of the record's
#     catch and its w_max is 0.25 decades below the sperm whale's, so its label
#     collides for no gain. Label sides are hand-set per taxon (REF_SIDE): the
#     two large-whale lines are labelled at the RIGHT, where the panel is empty
#     after 1971, and the rest at the LEFT, where it is empty before it.
#
# Writes Manuscript figures/fig1_mmw_rebuilt167.{png,pdf}
#        Manuscript data/fig1_mmw_series_rebuilt167.csv
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
# groups without a pretty name still get a capital, so neither the legend nor the
# panel B key is a mix of "Minke whales" and "squids"
pretty_lab <- function(s) ifelse(s %in% names(nice), unname(nice[s]),
                                 paste0(toupper(substr(s, 1, 1)), substring(s, 2)))

eff <- readRDS("effort_array_1841_2010.rds"); yv <- as.numeric(rownames(eff))
onset <- function(s){y <- yv[eff[,s]>0]; if(length(y)) min(y) else NA}
whal <- min(c(onset("baleen whales"), onset("sperm whales")), na.rm = TRUE)
krl  <- onset("antarctic krill")

# ------------------------------------------------------------------ PANEL A ---
# Unchanged from F01_figure1_rebuilt167.R apart from X_LIM/X_BRK and an explicit
# upper year filter (the old script let the scale limits drop 2013-2019, which
# warns).
obs_raw <- read.csv("yield_observed_timeseries.csv", check.names = FALSE)
obs_all <- obs_raw %>%
  pivot_longer(-Year, names_to = "Species", values_to = "t") %>%
  mutate(t = pmax(coalesce(suppressWarnings(as.numeric(t)), 0), 0) / 1e9) %>%
  filter(Year >= X_LIM[1], Year <= X_LIM[2])

# ZERO-CATCH YEARS ARE KEPT. F01_figure1_rebuilt167.R filtered on t > 0, which
# dropped 1941-1945 (Antarctic whaling stopped for the war) and 2005/2009/2011/
# 2012 from the data entirely -- geom_area then interpolated straight across
# them, so the panel showed a smooth decline through the war instead of the
# shutdown that actually happened. The species set is chosen on total catch
# instead, so groups that are never caught still stay out of the legend.
caught <- obs_all %>% group_by(Species) %>% summarise(s = sum(t), .groups = "drop") %>%
  filter(s > 0) %>% pull(Species)
obs <- obs_all %>% filter(Species %in% caught) %>%
  mutate(lab = pretty_lab(Species))
lev <- obs %>% group_by(lab) %>% summarise(s = sum(t), .groups = "drop") %>%
  arrange(desc(s)) %>% pull(lab)
lab2sp <- obs %>% distinct(lab, Species) %>% { setNames(.$Species, .$lab) }
obs$lab <- factor(obs$lab, levels = lev)

# Colours are looked up BY SPECIES. The old build looked them up through `nice`,
# so any group without a pretty name fell through to grey60 even though sp_cols
# has a colour for it -- squids and bathypelagic fishes both came out grey.
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
  # B sits directly below on an identical x axis and carries it for both, so A's
  # tick labels and title are dropped -- with them the two panels were separated
  # by a duplicated axis plus the legend band.
  theme(legend.position = "bottom", panel.grid = element_blank(),
        axis.text.x = element_blank(),
        plot.tag = element_text(face = "bold", size = 14)) +
  guides(fill = guide_legend(nrow = 2, order = 1))

# ------------------------------------------------------------------ PANEL B ---
# MMW(y) = sum_i C_iy * w_max,i / sum_i C_iy, catch-biomass weighted, arithmetic.
SP <- readRDS(file.path(DATA, sprintf("spectra_ref_period_%s.rds", SUF)))
wmax <- setNames(SP$traits$w_max, SP$traits$species)      # g

spp <- setdiff(names(obs_raw), "Year")
stopifnot(all(spp %in% names(wmax)))
C <- vapply(obs_raw[spp], function(v) {
  x <- suppressWarnings(as.numeric(v)); x[is.na(x)] <- 0; pmax(x, 0)
}, numeric(nrow(obs_raw)))
tot <- rowSums(C)

# Years with no reported catch leave the ratio undefined -- they are NA, not 0,
# so geom_line breaks rather than drawing a fall to the floor and back. 1941-45
# (the war) and 2005/2009/2011/2012 are blank in the record.
MMW <- data.frame(Year = obs_raw$Year,
                  total_kt = tot / 1e9,
                  mmw_t = ifelse(tot > 0, as.vector(C %*% wmax[spp]) / tot, NA) / 1e6) %>%
  filter(Year >= X_LIM[1], Year <= X_LIM[2])

# --- summary statistics quoted in the panel -----------------------------------
# The break is set at the last whaling-dominated year rather than at the krill
# vline (1974): the composition shifts with the end of large-whale catches in
# 1970, three years before krill enters the record.
e1 <- MMW$Year <= 1970 & is.finite(MMW$mmw_t)
e2 <- MMW$Year >= 1971 & MMW$Year <= 2010 & is.finite(MMW$mmw_t)
m1 <- median(MMW$mmw_t[e1]); m2 <- median(MMW$mmw_t[e2])
k  <- is.finite(MMW$mmw_t) & MMW$Year <= 2010
fit <- lm(log10(mmw_t) ~ Year, data = MMW[k, ])
rate <- coef(fit)[2] * 10                     # log10 units per decade
pval <- summary(fit)$coefficients[2, 4]

cat("\n=== mean maximum body mass of the catch ===\n")
cat(sprintf("  1930-1970 median  %.3g t (n = %d)\n", m1, sum(e1)))
cat(sprintf("  1971-2010 median  %.3g t (n = %d)\n", m2, sum(e2)))
cat(sprintf("  reduction         %.1fx (%.2f log10 units)\n", m1/m2, log10(m1/m2)))
cat(sprintf("  log10 trend       %.3f log10 units per decade (p = %.2g, R2 = %.2f)\n",
            rate, pval, summary(fit)$r.squared))
cat(sprintf("  range             %.3g to %.3g t\n",
            min(MMW$mmw_t, na.rm = TRUE), max(MMW$mmw_t, na.rm = TRUE)))

# --- species w_max reference lines --------------------------------------------
# EVERY EXPLOITED GROUP GETS ONE, so the panel's key is exactly panel A's legend.
# The set is read off the effort array rather than hand-listed, and asserted
# against the catch record -- the nine groups carrying nonzero effort are exactly
# the nine with nonzero reported catch, so either source defines "exploited".
REF_TAXA <- names(which(colSums(eff) > 0))
stopifnot(setequal(REF_TAXA, caught))
REF_TAXA <- REF_TAXA[order(wmax[REF_TAXA], decreasing = TRUE)]

# Which end of the panel each label sits at, and which side of its own line.
# Hand-set, and the only two entries needed: the large-whale lines run through
# the empty post-1971 half, everything else through the empty pre-1971 half.
# Orca (10.6 t) and minke (6.0 t) are 0.25 decades apart, so minke's label is
# dropped BELOW its line while orca's stays above; otherwise they touch.
REF_RIGHT <- c("baleen whales", "sperm whales")
REF_BELOW <- c("minke whales")
ref <- data.frame(Species = REF_TAXA,
                  y = unname(wmax[REF_TAXA]) / 1e6,
                  lab = pretty_lab(REF_TAXA),
                  stringsAsFactors = FALSE) %>%
  mutate(side = ifelse(Species %in% REF_RIGHT, "right", "left"),
         x  = ifelse(side == "left", X_LIM[1] + 1, X_LIM[2] - 1),
         hj = ifelse(side == "left", 0, 1),
         vj = ifelse(Species %in% REF_BELOW, 1.35, -0.45))
stopifnot(!any(is.na(sp_cols[ref$Species])))
cat("\n=== panel B w_max reference lines (t) ===\n")
print(ref[, c("Species", "y", "side")], row.names = FALSE, digits = 4)

# THE PANEL'S VERTICAL RANGE IS SET BY THREE NEAR-ZERO-CATCH YEARS, not by the
# whaling-to-krill transition. The indicator is a ratio, so 1971 (0.16 kt), 1988
# (1.1 kt) and 1993 (0.04 kt -- 41 tonnes of krill, the whole reported catch that
# year) drag it up to six decades below the whale line while the record's own
# peak is 500 kt. Nothing here is smoothed, thresholded or clipped, and the axis
# is NOT truncated to hide them; panel A carries the tonnage that tells the
# reader which years to trust. Points are a fixed size for that reason -- sizing
# them by catch was tried and only restated panel A.
y_lim <- c(10^-5.5, 10^2.15)      # widened ~0.4 decades each way by the default
                                  # 5% expansion, which must stay ON: turning it
                                  # off here and not in A misaligns the two x axes

pB <- ggplot(MMW, aes(Year, mmw_t)) +
  geom_hline(data = ref, aes(yintercept = y, colour = Species),
             linetype = "dotted", linewidth = 0.5, alpha = 0.85,
             show.legend = FALSE) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  # labels sit clear of their own line so the line is not overwritten
  geom_text(data = ref, aes(x = x, y = y, label = lab, hjust = hj, vjust = vj,
                            colour = Species),
            size = 2.7, show.legend = FALSE, inherit.aes = FALSE) +
  geom_line(linewidth = 0.55, colour = "grey30", na.rm = TRUE) +
  geom_point(size = 1.5, colour = "grey15", na.rm = TRUE) +
  scale_colour_manual(values = sp_cols, guide = "none") +
  scale_x_continuous(breaks = X_BRK, limits = X_LIM) +
  scale_y_log10(breaks = 10^seq(-5, 2, 1),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "l", linewidth = 0.25) +
  coord_cartesian(ylim = y_lim) +
  # NO IN-PANEL TEXT AT ALL (Kieran, 2026-08-04). The era medians and the decadal
  # rate are printed to the console above, for the manuscript text; the dotted
  # lines are explained in the caption, not on the panel.
  labs(x = "Year",
       y = expression("Mean maximum body mass of catch, "*w[max]*" (t)"),
       tag = "B") +
  theme_classic(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.tag = element_text(face = "bold", size = 14))

# B contributes no guide, so the collected legend is entirely A's.
#
# B IS THE SHORTER PANEL (Kieran, 2026-08-04). How far it can be compressed is
# set by the orca (10.6 t) and minke (6.0 t) reference labels, which are only 0.25
# decades apart: below roughly 0.65 they touch even with one set above its line
# and the other below, and orca has to move to the right-hand side -- where it
# then runs into the 2006-2010 points.
fig <- (pA / pB) + plot_layout(heights = c(1, 0.7), guides = "collect") &
  theme(legend.position = "bottom")
png_out <- guard(file.path(FIGS, sprintf("fig1_mmw_%s.png", SUF)))
pdf_out <- guard(file.path(FIGS, sprintf("fig1_mmw_%s.pdf", SUF)))
ggsave(png_out, fig, width = 11, height = 10.5, dpi = 300, limitsize = FALSE)
ggsave(pdf_out, fig, width = 11, height = 10.5, limitsize = FALSE)
write.csv(MMW, guard(file.path(DATA, sprintf("fig1_mmw_series_%s.csv", SUF))),
          row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")