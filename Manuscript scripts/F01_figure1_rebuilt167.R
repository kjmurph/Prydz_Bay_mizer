# =============================================================================
# FIGURE 1 -- rebuilt ensemble 44, cut A (unweighted yield RMSE), 167 members.
#
#   A  Observed catch, stacked by species (OBSERVATIONAL -- unchanged by any of
#      the ensemble work, carried over as-is)
#   B  Realised community size spectrum over the 2001-2010 reference period,
#      exploited vs unexploited, with per-group spectra
#   C  Size-spectrum slope lambda through time, and the paired difference
#
# PANEL B PLOTS NATIVE-RESOLUTION BIOMASS DENSITY n(w)*w over the 100 mizer bins,
# NOT the octave-binned LBNbiom spectrum. Octave binning merges the large-mammal
# structure the panel exists to show -- baleen whales occupy 2 native bins.
#
# CHANGES FROM F01_figure1_refined183.R (all agreed with Kieran, 2026-08-04):
#
#  1. Y-AXIS LABEL. The old "biomass density (g g-1)" invited a per-m2 reading.
#     The quantity is n(w)*w summed over species -- grams of biomass per gram of
#     body-mass interval, as a DOMAIN TOTAL. Labelled explicitly now.
#
#  2. THE LAMBDA FIT LINE IS GONE. A single slope cannot describe 15.5 decades of
#     a spectrum that plateaus below ~1e-2 g then steepens (residual sd 1.07,
#     range -2.9 to +1.6 log units). Both arms' lines were near-coincident and
#     fitted poorly; drawing them implied a description the data do not support.
#
#  3. THE LAMBDA VALUES STAY, and are now tied to panel C. They are the canonical
#     LBNbiom values from the octave-binned spectrum, not a fit to this panel, and
#     they are exactly what panel C plots through time. Panel C now carries a
#     shaded 2001-2010 band with the same two medians marked as ticks, so the two
#     numbers in B are locatable on C's y-axis.
#
#  4. SILHOUETTE PLACEMENT IS TRANSCRIBED FROM R/figures/fig1/03_panel_b.R:246-353.
#     Each one is anchored at the START of its group's line and set below and to
#     the left of it, sized by log10(w_max) so relative body size reads correctly.
#     No leader lines -- the offsets are small enough that each silhouette reads
#     against its own spectrum.
#
#     Two alternatives were trialled and rejected. Anchoring at w_mat packs five
#     large mammals into 1.5 decades of x where each silhouette is 1-3 decades
#     wide, so nearly every one ends up displaced and the anchor stops meaning
#     anything. An automatic de-overlap has to model the rendered box, which is
#     not knowable before the render; every estimate conservative enough to catch
#     the real collisions also cascaded the mid-panel fishes across the panel, and
#     it needed long leader lines to stay honest. The hand-tuned SIL_PLACE table
#     is what the earlier build settled on and it is stable.
#
#     PANEL B NOW PLOTS log10 ON A LINEAR AXIS rather than raw density under
#     scale_y_log10(). geom_phylopic()'s `height` is then exactly in decades,
#     which is what makes the placement table predictable, and the breaks read
#     0/5/10/15/20 to match the "log10 ..." axis title -- under scale_y_log10()
#     the title said log10 while the breaks said 10^n.
#
#  5. THE GREEN MEMBER IN PANEL C IS NOW REPRESENTATIVE, NOT EXTREME. It used to
#     be the member with the LARGEST |E - U| at 2010, which by construction sits
#     outside the ribbons and so read as an outlier rather than an illustration.
#     It is now the member whose two trajectories both stay inside the 25-75
#     ribbon of their own arm across 1841-2010, and whose 2010 gap is closest to
#     the ensemble median gap.
#
# Writes Manuscript figures/fig1_rebuilt167.{png,pdf}
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(scales); library(rphylopic); library(grid)
})

DATA <- "Manuscript data"; FIGS <- "Manuscript figures"
dir.create(FIGS, showWarnings = FALSE)
SUF <- "rebuilt167"; REF <- 2001:2010
guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE); f
}
meta <- readRDS(file.path(DATA, sprintf("meta_%s.rds", SUF)))
message("members: ", meta$n_members, " | cut: ", meta$cut)

# --- silhouette placement, transcribed from R/figures/fig1/03_panel_b.R:246-353
#
# Each silhouette is anchored at the START of its group's line and set below and
# to the LEFT of it, so it labels the line's origin rather than sitting on the
# data. No leader lines: the offset is small enough that each silhouette reads
# against its own spectrum.
#
# An automatic de-overlap was tried here and abandoned. It has to model the
# rendered box, which is not knowable before the render, and every estimate that
# was conservative enough to catch the real collisions also cascaded the
# mid-panel fishes across the panel. The hand-tuned table below is what the
# earlier build settled on and it is stable.
#
# Two forms per group:
#   c(dx, dy)          dx = decades to the LEFT of the line start,
#                      dy = fraction of the y range DOWN from it. dy is 0 for
#                      every group but one, so each silhouette sits directly
#                      left of its own spectrum at that spectrum's own height.
#   c(dx, NA, y_abs)   absolute y. Used only for the large baleen whales, which
#                      sit directly BELOW the large-marine-mammal spectra in the
#                      gap above the axis -- there is no room to their left,
#                      since their line starts at the extreme right edge.
#
# dx must clear the silhouette's own width: at these heights a silhouette is
# roughly one decade wide, so offsets below ~0.7 will overlap the line.
SIL_PLACE <- list(
  "mesozooplankton"          = c(-0.05, 0),
  "other krill"              = c( 0.90, 0),
  "other macrozooplankton"   = c( 0.90, 0),
  "antarctic krill"          = c( 0.90, 0),
  "salps"                    = c( 0.85, 0),
  "mesopelagic fishes"       = c( 0.85, 0),
  "bathypelagic fishes"      = c( 0.85, 0),
  "shelf and coastal fishes" = c( 0.90, 0),
  "flying birds"             = c( 0.85, 0),
  "small divers"             = c( 0.90, 0),
  "squids"                   = c( 0.95, 0),
  "toothfishes"              = c( 0.95, 0),
  "leopard seals"            = c( 0.90, 0),
  "medium divers"            = c( 1.05, 0),
  "large divers"             = c( 1.05, 0),
  # right and down, so it reads against its own line rather than the large
  # diver's, whose spectrum starts at a similar body mass
  "minke whales"             = c( 0.30, 0.030),
  "orca"                     = c( 1.60, 0),
  "sperm whales"             = c( 1.15, 0.025),
  # further right and lower than the 03_panel_b.R value of c(0.35, NA, -0.032):
  # the silhouettes here are larger, and at 0.35 the baleen whale ran into the
  # sperm whale, whose line starts only 0.16 decades to its right
  "baleen whales"            = c( 0.05, NA, NA))   # y_abs filled in below
SIL_DX_DEFAULT <- 0.45
SIL_DY_DEFAULT <- 0.040

# Height scales with log10(w_max) so relative body size reads correctly. Heavily
# compressed: the range spans 10 decades of mass, and silhouettes annotate the
# spectrum rather than compete with it. SIL_BASE/SIL_SPAN set the floor and the
# slope; both are up ~1.35x on the earlier build, which is the "larger across the
# board" Kieran asked for.
SIL_BASE <- 0.026
SIL_SPAN <- 0.019
# Height alone does not give equal apparent size: the PhyloPic images have very
# different aspect ratios and margins, so a compact leopard seal at the same
# height reads larger than an elongated whale. These restore the intended visual
# ordering -- baleen > sperm > orca > minke.
SIL_MULT <- c("leopard seals" = 0.70, "medium divers" = 0.80,
              "large divers" = 1.30, "minke whales" = 0.72,
              "orca" = 1.05, "sperm whales" = 1.02, "baleen whales" = 1.30)
# Clip used to find each line's left-hand end. Matches the plotted line: without
# it the anchor lands on a vanishing tail far to the left of the visible curve.
SIL_CLIP <- 1e-4

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

eff <- readRDS("effort_array_1841_2010.rds"); yv <- as.numeric(rownames(eff))
onset <- function(s){y <- yv[eff[,s]>0]; if(length(y)) min(y) else NA}
whal <- min(c(onset("baleen whales"), onset("sperm whales")), na.rm = TRUE)
krl  <- onset("antarctic krill")

# ------------------------------------------------------------------ PANEL A ---
obs <- read.csv("yield_observed_timeseries.csv") %>%
  pivot_longer(-Year, names_to = "Species", values_to = "t") %>%
  mutate(Species = gsub("\\.", " ", Species),
         t = pmax(coalesce(as.numeric(t), 0), 0) / 1e9) %>%   # g -> 10^3 tonnes
  filter(t > 0, Year >= 1900) %>%
  # groups without a pretty name still get a capital, so the legend is not a mix
  # of "Minke whales" and "squids"
  mutate(lab = ifelse(Species %in% names(nice), nice[Species],
                      paste0(toupper(substr(Species, 1, 1)), substring(Species, 2))))
lev <- obs %>% group_by(lab) %>% summarise(s = sum(t)) %>% arrange(desc(s)) %>% pull(lab)
obs$lab <- factor(obs$lab, levels = lev)
acols <- setNames(sp_cols[match(names(nice)[match(lev, nice)], names(sp_cols))], lev)
acols[is.na(acols)] <- "grey60"

pA <- ggplot(obs, aes(Year, t, fill = lab)) +
  geom_area(colour = NA) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  annotate("text", x = whal, y = Inf, label = "Whaling\nstarts", hjust = 1.08,
           vjust = 1.3, size = 3, colour = "grey35") +
  annotate("text", x = krl, y = Inf, label = "Krill\nfishing starts", hjust = 1.08,
           vjust = 1.3, size = 3, colour = "grey35") +
  scale_fill_manual(values = acols, name = NULL) +
  scale_x_continuous(breaks = seq(1900, 2010, 10), limits = c(1898, 2012)) +
  labs(x = "Year", y = expression("Catch (10"^3~"t y"^-1*")"), tag = "A") +
  theme_classic(base_size = 11) +
  theme(legend.position = "bottom", panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.tag = element_text(face = "bold", size = 14)) +
  guides(fill = guide_legend(nrow = 2))

# ------------------------------------------------------------------ PANEL B ---
SP <- readRDS(file.path(DATA, sprintf("spectra_ref_period_%s.rds", SUF)))
w <- SP$w
dens <- function(m) colMeans(m) * w        # n(w) * w, across-member mean
df_com <- bind_rows(
  data.frame(w = w, y = dens(SP$community_fished),  arm = "Exploited"),
  data.frame(w = w, y = dens(SP$community_clim),    arm = "Unexploited")) %>%
  filter(y > 0)

sp_mean_f <- apply(SP$species_fished, c(2, 3), mean)
df_sp <- bind_rows(lapply(seq_along(SP$sp_names), function(i) {
  v <- sp_mean_f[i, ] * w
  k <- which(v > 0)
  if (!length(k)) return(NULL)
  data.frame(w = w[k], y = v[k], Species = SP$sp_names[i])
}))

# canonical LBNbiom lambda, ensemble median at the reference period
sl <- readRDS(file.path(DATA, sprintf("nbss_slope_%s.rds", SUF)))
lam <- sl %>% filter(Year %in% REF) %>% group_by(arm) %>%
  summarise(lambda = median(slope, na.rm = TRUE), .groups = "drop")
lam_f <- lam$lambda[lam$arm == "exploited"]
lam_c <- lam$lambda[lam$arm == "unexploited"]

# Scenario colours are shared with panel C so the two panels read as one figure.
ARM_COLS <- c(Exploited = "#C0392B", Unexploited = "grey35")

# y is plotted as log10 on a LINEAR axis, not as raw density on scale_y_log10().
# Two reasons: geom_phylopic()'s `height` is then exactly in decades, which is
# what makes the placement table below predictable; and the axis then reads
# 0/5/10/15/20, which matches the "log10 ..." axis title. Under scale_y_log10()
# the title said log10 while the breaks said 10^n.
df_com$ly <- log10(df_com$y)
df_sp$ly  <- log10(df_sp$y)

# --- plotted window ----------------------------------------------------------
w_min_plot <- min(df_sp$w, df_com$w)
w_max_plot <- max(df_com$w) * 1.9     # right margin for the whale silhouettes
y_rng <- range(c(df_com$ly, df_sp$ly))
y_top <- y_rng[2] + diff(y_rng) * 0.10
# Headroom below the data for the large baleen whale silhouette, which sits
# under the large-marine-mammal spectra rather than to their left, and for the
# lambda annotation block.
y_bot <- y_rng[1] - diff(y_rng) * 0.165

# --- silhouettes, anchored at each group's LINE START -------------------------
# The cached objects are rphylopic images fetched by
# R/figures/fig1/00_provision_phylopic.R. Only 14 of the 19 groups have one; the
# rest are simply skipped rather than substituted with a different taxon.
PHYLOPIC_CACHE <- "R/figures/fig1/phylopic_cache"
sil_key <- function(nm) gsub("[^a-z0-9]+", "_", tolower(nm))

traits <- SP$traits
sp_wmax <- setNames(traits$w_max, traits$species)

anchors <- do.call(rbind, lapply(SP$sp_names, function(nm) {
  f <- file.path(PHYLOPIC_CACHE, paste0(sil_key(nm), ".rds"))
  if (!file.exists(f)) return(NULL)
  d <- df_sp[df_sp$Species == nm, ]
  d <- d[order(d$w), ]
  if (!nrow(d)) return(NULL)
  # left-hand end of the line, on the same clip the plotted line uses -- without
  # it the anchor lands on a vanishing tail far left of the visible curve
  k <- which(d$y > max(d$y) * SIL_CLIP)
  if (!length(k)) return(NULL)
  j <- min(k)
  data.frame(Species = nm, x0 = d$w[j], y0 = d$ly[j], stringsAsFactors = FALSE)
}))

getp <- function(k, j, default) {
  v <- SIL_PLACE[[k]]
  if (is.null(v) || length(v) < j || is.na(v[j])) default else v[j]
}
# baleen whales sit below the mammal spectra: there is no room to their left,
# since their line starts at the extreme right edge
SIL_PLACE[["baleen whales"]][3] <- y_rng[1] - diff(y_rng) * 0.085

anchors$x <- 10^(log10(anchors$x0) -
                   vapply(anchors$Species, getp, numeric(1), 1, SIL_DX_DEFAULT))
dyf <- vapply(anchors$Species, getp, numeric(1), 2, SIL_DY_DEFAULT)
yab <- vapply(anchors$Species, getp, numeric(1), 3, NA_real_)
anchors$y <- ifelse(is.finite(yab), yab, anchors$y0 - diff(y_rng) * dyf)
anchors$x <- pmax(anchors$x, w_min_plot * 1.15)     # keep inside the panel
anchors$colour <- unname(sp_cols[anchors$Species])
anchors$colour[is.na(anchors$colour)] <- "grey55"

# Height scales with log10(w_max) so relative body size reads correctly.
lw  <- log10(sp_wmax[anchors$Species])
rel <- (lw - min(log10(sp_wmax))) / diff(range(log10(sp_wmax)))
mult <- ifelse(anchors$Species %in% names(SIL_MULT),
               SIL_MULT[anchors$Species], 1)
anchors$h <- diff(y_rng) * (SIL_BASE + SIL_SPAN * rel) * mult
anchors$img <- I(lapply(anchors$Species, function(nm)
  readRDS(file.path(PHYLOPIC_CACHE, paste0(sil_key(nm), ".rds")))$img))

cat("\n=== silhouette placement (anchor: line start, no leaders) ===\n")
print(data.frame(species = anchors$Species,
                 log10_x_start = round(log10(anchors$x0), 2),
                 log10_x_drawn = round(log10(anchors$x), 2),
                 log10_y_start = round(anchors$y0, 2),
                 log10_y_drawn = round(anchors$y, 2),
                 height_dec = round(anchors$h, 2)), row.names = FALSE)
cat("  placed:", nrow(anchors), "of", n_distinct(df_sp$Species), "\n")

pB <- ggplot() +
  annotate("rect", xmin = 5e5, xmax = w_max_plot, ymin = -Inf, ymax = Inf,
           fill = "#CFE3F2", alpha = 0.55) +
  annotate("text", x = 6.5e6, y = y_top, label = "Large marine\nmammals",
           vjust = 1, size = 3.1, colour = "grey25", lineheight = 0.9) +
  geom_line(data = df_sp, aes(w, ly, colour = Species), linewidth = 0.95,
            alpha = 0.9, show.legend = FALSE) +
  geom_line(data = df_com, aes(w, ly, colour = arm, linetype = arm),
            linewidth = 0.85) +
  scale_colour_manual(values = c(sp_cols, ARM_COLS), name = NULL,
                      breaks = names(ARM_COLS)) +
  scale_linetype_manual(values = c(Exploited = "dashed", Unexploited = "solid"),
                        name = NULL) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)),
                breaks = 10^seq(-8, 8, 2)) +
  annotation_logticks(sides = "b", size = 0.25) +
  coord_cartesian(xlim = c(w_min_plot, w_max_plot), ylim = c(y_bot, y_top),
                  expand = FALSE) +
  labs(x = "Body mass (g)",
       y = expression(log[10]~"biomass per unit body mass, "*n(w)%.%w~"(g g"^-1*")"),
       tag = "B") +
  # The lambda VALUES stay although the fit line is gone: they are the canonical
  # octave-binned LBNbiom slopes, and they are what panel C plots through time.
  annotate("text", x = w_min_plot * 1.3, y = y_bot + diff(y_rng) * 0.132,
           hjust = 0, size = 3.2, colour = ARM_COLS[["Exploited"]],
           fontface = "bold",
           label = sprintf("lambda[exploited] == %.3f", lam_f), parse = TRUE) +
  annotate("text", x = w_min_plot * 1.3, y = y_bot + diff(y_rng) * 0.087,
           hjust = 0, size = 3.2, colour = ARM_COLS[["Unexploited"]],
           fontface = "bold",
           label = sprintf("lambda[unexploited] == %.3f", lam_c), parse = TRUE) +
  annotate("text", x = w_min_plot * 1.3, y = y_bot + diff(y_rng) * 0.045,
           hjust = 0, size = 2.8, colour = "grey35",
           label = "LBNbiom slope, 2001-2010 ensemble median (see panel C)") +
  theme_classic(base_size = 11) +
  # Lower-left, above the lambda block: that corner of the panel is empty (no
  # group's spectrum reaches below ~10^3 at these masses), and every other
  # position is taken -- upper left by the community line, right by the mammal
  # band and its silhouettes, centre by the fish spectra.
  theme(panel.grid = element_blank(), legend.position = c(0.13, 0.21),
        legend.background = element_blank(), legend.key.width = unit(1.4, "lines"),
        plot.tag = element_text(face = "bold", size = 14)) +
  guides(colour = guide_legend(override.aes = list(linewidth = 1)))

# One layer per group: height and fill must vary per silhouette, and a
# fixed-parameter layer is more predictable here than mapping them.
for (i in seq_len(nrow(anchors))) {
  pB <- pB + rphylopic::geom_phylopic(
    data = anchors[i, , drop = FALSE], aes(x = x, y = y, img = img),
    fill = anchors$colour[i], height = anchors$h[i], alpha = 1,
    inherit.aes = FALSE)
}

# ------------------------------------------------------------------ PANEL C ---
S <- sl %>% group_by(arm, Year) %>%
  summarise(med = median(slope, na.rm = TRUE),
            lo50 = quantile(slope, .25, na.rm = TRUE),
            hi50 = quantile(slope, .75, na.rm = TRUE),
            lo90 = quantile(slope, .05, na.rm = TRUE),
            hi90 = quantile(slope, .95, na.rm = TRUE), .groups = "drop") %>%
  mutate(Arm = ifelse(arm == "exploited", "Exploited", "Unexploited"))

# --- one REPRESENTATIVE within-pair member, drawn in green --------------------
# The ensemble ribbons show the SPREAD ACROSS members, which is much wider than
# the within-pair difference and so visually buries the actual effect. Tracing a
# single member's two trajectories makes the paired quantity legible: the gap
# between the two green lines IS the signal, and it is far narrower than the
# ribbons around it.
#
# The member is chosen to be REPRESENTATIVE, not extreme: both of its
# trajectories must sit inside the 25-75 ribbon of their own arm across the whole
# record, and among those it is the one whose 2010 gap is closest to the ensemble
# median gap. (The previous version picked the LARGEST |E - U|, which by
# construction lies outside the ribbons and reads as an outlier.)
pair_gap <- sl %>%
  select(sim_index, arm, Year, slope) %>%
  pivot_wider(names_from = arm, values_from = slope) %>%
  filter(Year == 2010) %>%
  mutate(gap = exploited - unexploited)
med_gap <- median(pair_gap$gap, na.rm = TRUE)

score <- sl %>%
  left_join(S %>% select(arm, Year, lo50, hi50, lo90, hi90),
            by = c("arm", "Year")) %>%
  group_by(sim_index) %>%
  summarise(f50 = mean(slope >= lo50 & slope <= hi50, na.rm = TRUE),
            f90 = mean(slope >= lo90 & slope <= hi90, na.rm = TRUE),
            .groups = "drop")

pick <- function(sc, col) {
  best <- max(sc[[col]], na.rm = TRUE)
  cand <- sc$sim_index[sc[[col]] >= best - 1e-12]
  g <- pair_gap %>% filter(sim_index %in% cand) %>%
    mutate(d = abs(gap - med_gap)) %>% arrange(d)
  list(sim = g$sim_index[1], gap = g$gap[1], score = best)
}
h <- pick(score, "f50")
crit <- "inside the 25-75 ribbon"
if (h$score < 1) {           # nothing stays in the IQR band all 170 years
  h2 <- pick(score, "f90")
  if (h2$score > h$score) { h <- h2; crit <- "inside the 5-95 ribbon" }
}
hero <- h$sim; hero_gap <- h$gap

cat("\n=== panel C representative member ===\n")
cat(sprintf("  member %d | %s for %.1f%% of years | E-U at 2010 = %.4f",
            hero, crit, 100 * h$score, hero_gap))
cat(sprintf(" (ensemble median %.4f)\n", med_gap))

HERO <- sl %>% filter(sim_index == hero) %>%
  mutate(Arm = ifelse(arm == "exploited", "Exploited", "Unexploited"))

# --- the two difference brackets at 2010 ---------------------------------------
# They measure different things and the distinction is the point of the panel:
#
#   ECOLOGICAL DEFICIT (grey)  median_i(E_i) - median_i(U_i), the gap between the
#                              two plotted ensemble medians -- a difference OF
#                              medians, read straight off the trajectories.
#   WITHIN-PAIR (green)        median_i(E_i - U_i), the median OF the paired
#                              differences -- each member differenced against its
#                              own unexploited twin before summarising.
#
# The second is the exploitation signal proper: it removes the cross-calibration
# spread, which is why 167 of 167 pairs agree on its sign while the ribbons
# overlap heavily. Both are anchored at the unexploited median so their lengths
# are directly comparable.
e2010 <- S$med[S$Arm == "Exploited"   & S$Year == 2010]
u2010 <- S$med[S$Arm == "Unexploited" & S$Year == 2010]
x_pair <- 2013      # within-pair, green
x_def  <- 2017.5    # ecological deficit, grey
x_lab  <- 2019.5
cat("\n=== the two 2010 differences ===\n")
cat(sprintf("  ecological deficit (median E - median U): %.5f\n", e2010 - u2010))
cat(sprintf("  within-pair        (median of E - U)    : %.5f\n", med_gap))

pC1 <- ggplot(S, aes(Year, med, colour = Arm, fill = Arm)) +
  # --- the panel B reference period, drawn first so it sits under everything --
  # This is the visual tie between the two lambda values printed in panel B and
  # the trajectories plotted here: the band is the window panel B averages over,
  # and the two ticks inside it are exactly the two numbers panel B reports.
  annotate("rect", xmin = min(REF), xmax = max(REF), ymin = -Inf, ymax = Inf,
           fill = "#CFE3F2", alpha = 0.5) +
  annotate("text", x = mean(REF), y = Inf, label = "Panel B\nreference period",
           vjust = 1.25, size = 2.7, colour = "grey30", lineheight = 0.95) +
  geom_ribbon(aes(ymin = lo90, ymax = hi90), alpha = 0.18, colour = NA) +
  geom_ribbon(aes(ymin = lo50, ymax = hi50), alpha = 0.32, colour = NA) +
  geom_line(aes(linetype = Arm), linewidth = 0.7) +
  geom_line(data = HERO, aes(Year, slope, group = Arm, linetype = Arm),
            colour = "#1B7F4B", linewidth = 0.55, inherit.aes = FALSE) +
  # The two lambda values panel B reports, marked ON this panel. They coincide
  # with the medians by construction -- that IS the tie -- so they are drawn
  # LAST, over the lines, and run a little past the band with an end dot, or
  # they vanish underneath the very curves they are meant to identify.
  annotate("segment", x = min(REF) - 3, xend = max(REF), y = lam_f, yend = lam_f,
           colour = ARM_COLS[["Exploited"]], linewidth = 1.2) +
  annotate("segment", x = min(REF) - 3, xend = max(REF), y = lam_c, yend = lam_c,
           colour = ARM_COLS[["Unexploited"]], linewidth = 1.2) +
  annotate("point", x = c(max(REF), max(REF)), y = c(lam_f, lam_c),
           colour = unname(ARM_COLS[c("Exploited", "Unexploited")]), size = 1.6) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  annotate("text", x = whal, y = Inf, label = "Whaling\nstarts", hjust = 1.07,
           vjust = 1.3, size = 2.9, colour = "grey35") +
  annotate("text", x = krl, y = Inf, label = "Krill\nfishing starts", hjust = 1.07,
           vjust = 1.3, size = 2.9, colour = "grey35") +
  # within-pair difference: same construction as the deficit bracket, anchored at
  # the same unexploited median so the two lengths can be read against each other
  annotate("segment", x = x_pair, xend = x_pair, y = u2010,
           yend = u2010 + med_gap,
           arrow = arrow(ends = "both", length = unit(0.055, "inches"),
                         type = "closed"),
           colour = "#1B7F4B", linewidth = 0.45) +
  # ecological deficit: double-headed arrow spanning the two medians at 2010
  annotate("segment", x = x_def, xend = x_def, y = u2010, yend = e2010,
           arrow = arrow(ends = "both", length = unit(0.055, "inches"),
                         type = "closed"),
           colour = "grey20", linewidth = 0.45) +
  # labels go outward from the brackets -- green above its top, grey below its
  # bottom -- because the two spans differ by only ~0.001 slope units and
  # centred labels would sit on top of each other
  annotate("text", x = x_lab, y = u2010, hjust = 0, vjust = 0.15, size = 3.0,
           colour = "#1B7F4B", lineheight = 0.95,
           label = sprintf("Within-pair\ndifference\n(%.4f)", med_gap)) +
  annotate("text", x = x_lab, y = e2010, hjust = 0, vjust = 0.85, size = 3.0,
           colour = "grey20", lineheight = 0.95,
           label = sprintf("Ecological\ndeficit\n(%.4f)", e2010 - u2010)) +
  annotate("text", x = 1852, y = min(S$lo90, na.rm = TRUE),
           label = sprintf("green: member %d (representative pair, %s)", hero, crit),
           hjust = 0, size = 2.7, colour = "#1B7F4B") +
  scale_colour_manual(values = c(Exploited = "#C0392B", Unexploited = "grey35")) +
  scale_fill_manual(values = c(Exploited = "#C0392B", Unexploited = "grey55")) +
  scale_linetype_manual(values = c(Exploited = "solid", Unexploited = "dotted")) +
  coord_cartesian(xlim = c(1841, 2010), clip = "off") +
  labs(x = NULL, y = expression("Size-spectrum slope "*lambda), tag = "C") +
  theme_classic(base_size = 11) +
  theme(panel.grid = element_blank(), legend.position = c(0.14, 0.22),
        legend.title = element_blank(), legend.background = element_blank(),
        plot.margin = margin(6, 108, 6, 6),
        plot.tag = element_text(face = "bold", size = 14))

d2010 <- sl %>% filter(Year == 2010) %>%
  select(sim_index, arm, slope) %>% pivot_wider(names_from = arm, values_from = slope)
base <- median(d2010$unexploited, na.rm = TRUE)
dd <- bind_rows(
  data.frame(v = d2010$exploited   - base, grp = "Exploited\n(across-member)"),
  data.frame(v = d2010$unexploited - base, grp = "Unexploited\n(across-member)"),
  data.frame(v = d2010$exploited - d2010$unexploited, grp = "E - U\n(within-pair)")) %>%
  mutate(grp = factor(grp, levels = c("Exploited\n(across-member)",
                                      "Unexploited\n(across-member)",
                                      "E - U\n(within-pair)")))
# Interval statistics are precomputed rather than left to stat_summary(): with a
# horizontal violin, stat_summary(geom = "errorbarh") receives no y and fails
# with "replacement has 1 row, data has 0".
dd_stat <- dd %>% group_by(grp) %>%
  summarise(mid = median(v, na.rm = TRUE),
            q25 = quantile(v, .25, na.rm = TRUE),
            q75 = quantile(v, .75, na.rm = TRUE),
            q05 = quantile(v, .05, na.rm = TRUE),
            q95 = quantile(v, .95, na.rm = TRUE), .groups = "drop")

# No within-pair marker here: this sub-plot already carries the paired quantity
# as its own row, with a median point and interval, so a vline would restate it.
# The green bracket lives in pC1, against the ecological deficit.
pC2 <- ggplot(dd, aes(v, grp, fill = grp, colour = grp)) +
  geom_violin(alpha = 0.45, colour = NA, scale = "width", width = 0.9) +
  geom_segment(data = dd_stat, inherit.aes = FALSE,
               aes(x = q05, xend = q95, y = grp, yend = grp, colour = grp),
               linewidth = 0.5) +
  geom_segment(data = dd_stat, inherit.aes = FALSE,
               aes(x = q25, xend = q75, y = grp, yend = grp, colour = grp),
               linewidth = 1.6) +
  geom_point(data = dd_stat, inherit.aes = FALSE,
             aes(x = mid, y = grp, colour = grp), size = 2.4) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey45") +
  scale_fill_manual(values = c("#C0392B", "grey55", "#5B8C7B"), guide = "none") +
  scale_colour_manual(values = c("#8B1A1A", "grey30", "#2F5D4E"), guide = "none") +
  labs(x = expression("Difference in "*lambda*" (slope units)"), y = NULL) +
  theme_classic(base_size = 10) +
  theme(panel.grid = element_blank(), axis.text.y = element_text(size = 8))

pC <- pC1 / pC2 + plot_layout(heights = c(2.1, 1))

cat("\n=== panel C key statistics ===\n")
cat("  lambda 2010: exploited", signif(median(d2010$exploited, na.rm=TRUE), 6),
    "| unexploited", signif(median(d2010$unexploited, na.rm=TRUE), 6), "\n")
pd <- d2010$exploited - d2010$unexploited
cat("  paired E-U at 2010: median", signif(median(pd, na.rm = TRUE), 6),
    sprintf("| %d of %d negative (%.1f%%)\n", sum(pd < 0, na.rm = TRUE),
            sum(is.finite(pd)), 100*mean(pd < 0, na.rm = TRUE)))
cat("  lambda over the reference period: exploited", signif(lam_f, 6),
    "| unexploited", signif(lam_c, 6), "\n")

fig <- (pA / (pB | pC)) + plot_layout(heights = c(1, 1.25))
png_out <- guard(file.path(FIGS, sprintf("fig1_%s.png", SUF)))
pdf_out <- guard(file.path(FIGS, sprintf("fig1_%s.pdf", SUF)))
ggsave(png_out, fig, width = 15, height = 15, dpi = 300, limitsize = FALSE)
ggsave(pdf_out, fig, width = 15, height = 15, limitsize = FALSE)
write.csv(S, guard(file.path(DATA, sprintf("fig1_slope_series_%s.csv", SUF))),
          row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")