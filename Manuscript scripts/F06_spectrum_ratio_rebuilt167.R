# =============================================================================
# COMBINED SPECTRUM + ABUNDANCE-RATIO FIGURE -- rebuilt ensemble 44, cut A,
# top 10% = 167 members.
#
#   A  Realised community size spectrum over the 2001-2010 reference period,
#      exploited vs unexploited, with per-group spectra and silhouettes.
#      (panel B of F01_figure1_rebuilt167.R)
#   B  Community abundance ratio, Exploited / Unexploited, over the same
#      reference period, with dominance bands.
#      (panel b of yield_ratio_combined_panels.R)
#
# Tags are UPPER CASE, as Science requires.
#
# BOTH PANELS NOW COME FROM THE SAME SOURCE, Manuscript data/
# spectra_ref_period_rebuilt167.rds -- the 167-member de-duplicated, re-run
# ensemble. The ratio panel previously read the legacy top-10% caches
# (spectra_cache_ref_period.rds, 2111 rows, indexed through
# yield_rmse_per_sim.csv); those are a different ensemble and are not read here.
# One consequence is visible and is NOT a plotting artefact: on the rebuilt
# ensemble the ratio sits within ~3% of 1 everywhere outside the large marine
# mammals, where the legacy caches reached 1.5 at the toothfishes. The y axis is
# scaled to the rebuilt data.
#
# CHANGES FROM F01_figure1_rebuilt167.R PANEL B (all requested by Kieran):
#
#  1. X AXIS CROPPED TO START AT 1 g, and both panels are given the SAME x scale
#     -- limits, breaks and labels -- so a body mass reads at the same horizontal
#     position in both. The window is the ratio panel's: [max(w < 1 g), 1.15 *
#     1.1 * w_max(baleen)]. Panel A therefore drops its x tick labels and title;
#     panel B carries the shared axis, as in F01_figure1_mmw_rebuilt167.R.
#
#  2. THE LIGHT-BLUE "Large marine mammals" BAND AND LABEL ARE GONE from panel A.
#     Panel B keeps its own mammal band -- that one is the last member of the
#     dominance-band series, not a repeat of panel A's annotation.
#
#  3. THE LAMBDA ANNOTATION BLOCK IS GONE. It pointed at panel C of Figure 1,
#     which does not exist here. (The lambda FIT LINE was already dropped in
#     F01_figure1_rebuilt167.R: a single slope cannot describe 15.5 decades of a
#     spectrum that plateaus then steepens.)
#
#  4. SILHOUETTE PLACEMENT IS RE-DERIVED FOR THE CROPPED WINDOW. The hand-tuned
#     table in F01 is expressed in decades of x and fractions of the y range, and
#     both change: x goes from 15.8 decades to 8.25, y from 17.2 to 12.8. Carried
#     over unscaled, every offset would be roughly twice too wide and every
#     silhouette a third too small. SIL_DX_SCALE / SIL_SIZE_SCALE below restore
#     the physical geometry, and six groups need a different ANCHOR: their
#     spectra start left of 1 g, so there is no line start inside the window to
#     sit beside and no room to their left. Those are anchored at the left edge
#     and set to its right (see SIL_EDGE).
#
# Reads   Manuscript data/spectra_ref_period_rebuilt167.rds
#         Manuscript data/meta_rebuilt167.rds
#         R/figures/fig1/phylopic_cache/*.rds
# Writes  Manuscript figures/fig_spectrum_ratio_rebuilt167.{png,pdf}
#
# Unlike the F0x scripts this one may overwrite its own outputs: it is expected
# to be re-run while the silhouette placement is tuned, and it owns both files.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(scales); library(rphylopic); library(grid)
})

DATA <- "Manuscript data"; FIGS <- "Manuscript figures"
dir.create(FIGS, showWarnings = FALSE)
SUF <- "rebuilt167"

meta <- readRDS(file.path(DATA, sprintf("meta_%s.rds", SUF)))
message("members: ", meta$n_members, " | cut: ", meta$cut)

SP <- readRDS(file.path(DATA, sprintf("spectra_ref_period_%s.rds", SUF)))
w  <- SP$w
# mizer builds w as a geometric grid and sets dw = (10^dx - 1) * w exactly;
# re-deriving it here matches params_sel_adj.rds@dw to 7e-16 relative, and saves
# loading a mizer 2.5.0 params object under mizer 3.x just to read one vector.
dw <- w * (w[2] / w[1] - 1)

sp_names <- SP$sp_names
sp_wmax  <- setNames(SP$traits$w_max, SP$traits$species)

# --- the shared x window ------------------------------------------------------
# Panel b's window, applied to both panels. w_min_plot is the last bin BELOW 1 g
# so the "1 g" break is not clipped off the axis.
W_LO <- max(w[w < 1])
W_HI <- unname(sp_wmax["baleen whales"]) * 1.15 * 1.1
LX   <- c(log10(W_LO), log10(W_HI))
cat(sprintf("shared x window: %.3g g to %.3g g (%.2f decades)\n",
            W_LO, W_HI, diff(LX)))

x_breaks_shared <- c(1, 10, 100, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8)
x_labels_shared <- function(x) dplyr::case_when(
  x >= 1e6 ~ paste0(x / 1e6, " t"),
  x >= 1e3 ~ paste0(x / 1e3, " kg"),
  TRUE     ~ paste0(round(x, 0), " g"))
x_scale_shared <- function()
  scale_x_log10(labels = x_labels_shared, breaks = x_breaks_shared,
                limits = c(W_LO, W_HI), oob = scales::squish,
                expand = expansion(mult = 0))

sp_cols <- c("baleen whales" = "#FF61C3", "sperm whales" = "#DB72FB",
  "minke whales" = "#00B9E3", "orca" = "#619CFF", "toothfishes" = "#00C19F",
  "shelf and coastal fishes" = "#93AA00", "antarctic krill" = "#F8766D",
  "leopard seals" = "#E07B39", "medium divers" = "#2B6CB0",
  "large divers" = "#4C8FD0", "flying birds" = "#9E9E9E",
  "small divers" = "#BDBDBD", "mesopelagic fishes" = "#D39200",
  "bathypelagic fishes" = "#B07A00", "squids" = "#E8B33C",
  "mesozooplankton" = "#6A1B9A", "other krill" = "#8E44AD",
  "other macrozooplankton" = "#9B59B6", "salps" = "#C39BD3")
ARM_COLS <- c(Exploited = "#C0392B", Unexploited = "grey35")

BASE_SIZE <- 11
shared_axis_theme <- theme(axis.title = element_text(size = 12),
                           axis.text  = element_text(size = 11))

# =============================================================================
# PANEL a -- community + per-group size spectrum, 2001-2010
# =============================================================================
dens <- function(m) colMeans(m) * w        # n(w) * w, across-member mean
df_com <- bind_rows(
  data.frame(w = w, y = dens(SP$community_fished), arm = "Exploited"),
  data.frame(w = w, y = dens(SP$community_clim),   arm = "Unexploited")) %>%
  filter(y > 0)

sp_mean_f <- apply(SP$species_fished, c(2, 3), mean)
df_sp <- bind_rows(lapply(seq_along(sp_names), function(i) {
  v <- sp_mean_f[i, ] * w
  k <- which(v > 0)
  if (!length(k)) return(NULL)
  data.frame(w = w[k], y = v[k], Species = sp_names[i])
}))

# y is plotted as log10 on a LINEAR axis, not raw density under scale_y_log10():
# geom_phylopic()'s `height` is then exactly in decades, which is what makes the
# placement table predictable, and the breaks read 0/5/10 to match the "log10 ..."
# axis title instead of contradicting it.
df_com$ly <- log10(df_com$y)
df_sp$ly  <- log10(df_sp$y)

# --- plotted window -----------------------------------------------------------
# y is set by the data INSIDE the cropped x window. Taken over the full grid it
# would run to 10^17 (mesozooplankton at 3e-8 g), which is now off-panel, and
# every silhouette offset -- all expressed as fractions of the y range -- would
# be scaled by a range that is not on screen.
in_win  <- function(d) d[d$w >= W_LO & d$w <= W_HI, ]
y_rng   <- range(c(in_win(df_com)$ly, in_win(df_sp)$ly))
y_span  <- diff(y_rng)
y_top   <- y_rng[2] + y_span * 0.06
y_bot   <- y_rng[1] - y_span * 0.135    # headroom for the baleen silhouette
cat(sprintf("panel a y range in window: %.2f to %.2f (%.2f decades)\n",
            y_rng[1], y_rng[2], y_span))

# --- silhouettes --------------------------------------------------------------
# Placement is transcribed from R/figures/fig1/03_panel_b.R:246-353 by way of
# F01_figure1_rebuilt167.R: each silhouette is anchored at the START of its
# group's line and set below and to the LEFT of it, with no leader lines.
#
#   c(dx, dy)          dx = decades LEFT of the anchor,
#                      dy = fraction of the y range DOWN from it
#   c(dx, NA, y_abs)   absolute y -- used only for the large baleen whales, which
#                      sit BELOW the large-mammal spectra in the gap above the
#                      axis, there being no room to their left.
#
# A NEGATIVE dx moves the silhouette RIGHT. That is new here and is what the
# six edge-anchored groups use.
PHYLOPIC_CACHE <- "R/figures/fig1/phylopic_cache"
sil_key <- function(nm) gsub("[^a-z0-9]+", "_", tolower(nm))

# Groups whose spectrum starts left of 1 g. The anchor search below takes the
# left-hand end of the VISIBLE line, so these are anchored at the 1 g edge at
# their own height there and set to its RIGHT; each then sits in the clear band
# between its own line and the next one down. This vector does not drive that --
# it only names them in the diagnostic, so the two anchoring regimes can be told
# apart when the placement is checked against the render.
SIL_EDGE <- c("antarctic krill", "salps", "mesopelagic fishes",
              "shelf and coastal fishes", "squids", "toothfishes")

# The six edge-anchored groups are staggered in x as well as y: at the 1 g edge
# the salps / pelagic-fish / commercial-fish lines are only 0.7-0.9 decades
# apart, which is less than a silhouette is tall, so each is slid right to where
# its own gap opens up.
SIL_PLACE <- list(
  # --- edge-anchored: dx NEGATIVE = to the RIGHT of the 1 g edge --------------
  "antarctic krill"          = c(-0.25,  0.060),
  "salps"                    = c(-1.14,  0),
  "mesopelagic fishes"       = c(-1.04,  0.028),
  "shelf and coastal fishes" = c(-0.29,  0.052),
  "toothfishes"              = c(-0.29,  0.052),
  "squids"                   = c(-0.29,  0.058),
  # --- start-anchored: F01's offsets, rescaled to the cropped window ----------
  # F01's dx is in decades and its window is 15.8 decades wide over a ~5.0 in
  # panel; this one is 8.25 decades over ~8.5 in, so the same physical offset is
  # 0.31x as many decades. Applied, then hand-checked against the render.
  "flying birds"             = c( 0.26,  0),
  "small divers"             = c( 0.62,  0.058),
  "leopard seals"            = c( 0.28,  0),
  "medium divers"            = c( 0.32,  0),
  "large divers"             = c( 0.32,  0),
  "minke whales"             = c( 0.09,  0.030),
  "orca"                     = c( 0.49,  0),
  "sperm whales"             = c( 0.55,  0.025),
  # F01 sets the baleen whale just LEFT of its line start. Here that puts it on
  # top of the sperm whale, whose line now starts only 0.16 decades away in a
  # window half as wide, so it goes to the RIGHT instead -- into the empty band
  # under the baleen spectrum's own right-hand rise.
  "baleen whales"            = c(-0.95,  NA, NA))  # y_abs filled in below
SIL_DX_DEFAULT <- 0.20
SIL_DY_DEFAULT <- 0.040

# Height scales with log10(w_max) so relative body size reads correctly.
# SIL_BASE/SIL_SPAN are F01's, and the y range they multiply has shrunk from 17.2
# decades to 12.8, which is about what the shorter panel needs -- so
# SIL_SIZE_SCALE stays at 1 and only the crowded mid-panel groups are cut back
# individually below.
SIL_BASE <- 0.026
SIL_SPAN <- 0.019
SIL_SIZE_SCALE <- 1.00
# Height alone does not give equal apparent size: the PhyloPic images have very
# different aspect ratios and margins. The large-mammal values restore the
# intended visual ordering -- baleen > sperm > orca > minke. The four small
# values are new, and are set by the vertical gap each silhouette has to fit
# into: 0.7-0.9 decades between the salps, pelagic-fish, commercial-fish and
# krill lines at the left of the cropped window.
SIL_MULT <- c("antarctic krill" = 0.85, "salps" = 0.70,
              "mesopelagic fishes" = 0.65, "shelf and coastal fishes" = 0.75,
              "leopard seals" = 0.70, "medium divers" = 0.80,
              "large divers" = 1.30, "minke whales" = 0.72,
              "orca" = 1.05, "sperm whales" = 1.02, "baleen whales" = 1.30)
# Clip used to find each line's end. Matches the plotted line: without it the
# anchor lands on a vanishing tail far to the left of the visible curve.
SIL_CLIP <- 1e-4

anchors <- do.call(rbind, lapply(sp_names, function(nm) {
  f <- file.path(PHYLOPIC_CACHE, paste0(sil_key(nm), ".rds"))
  if (!file.exists(f)) return(NULL)
  d <- df_sp[df_sp$Species == nm, ]
  d <- d[order(d$w), ]
  k <- which(d$y > max(d$y) * SIL_CLIP)
  if (!length(k)) return(NULL)
  d <- d[k, ]
  vis <- d[d$w >= W_LO & d$w <= W_HI, ]
  if (!nrow(vis)) return(NULL)          # nothing of this group is on screen
  j <- which.min(vis$w)                 # left-hand end of the VISIBLE line
  data.frame(Species = nm, x0 = vis$w[j], y0 = vis$ly[j],
             edge = nm %in% SIL_EDGE, stringsAsFactors = FALSE)
}))

getp <- function(k, j, default) {
  v <- SIL_PLACE[[k]]
  if (is.null(v) || length(v) < j || is.na(v[j])) default else v[j]
}
# baleen whales sit below the mammal spectra: there is no room to their left,
# since their line starts at the extreme right edge
SIL_PLACE[["baleen whales"]][3] <- y_rng[1] - y_span * 0.075

anchors$x <- 10^(log10(anchors$x0) -
                   vapply(anchors$Species, getp, numeric(1), 1, SIL_DX_DEFAULT))
dyf <- vapply(anchors$Species, getp, numeric(1), 2, SIL_DY_DEFAULT)
yab <- vapply(anchors$Species, getp, numeric(1), 3, NA_real_)
anchors$y <- ifelse(is.finite(yab), yab, anchors$y0 - y_span * dyf)
anchors$x <- pmin(pmax(anchors$x, W_LO * 1.05), W_HI * 0.98)   # keep on panel
anchors$colour <- unname(sp_cols[anchors$Species])
anchors$colour[is.na(anchors$colour)] <- "grey55"

lw   <- log10(sp_wmax[anchors$Species])
rel  <- (lw - min(log10(sp_wmax))) / diff(range(log10(sp_wmax)))
mult <- ifelse(anchors$Species %in% names(SIL_MULT), SIL_MULT[anchors$Species], 1)
anchors$h <- y_span * (SIL_BASE + SIL_SPAN * rel) * mult * SIL_SIZE_SCALE
anchors$img <- I(lapply(anchors$Species, function(nm)
  readRDS(file.path(PHYLOPIC_CACHE, paste0(sil_key(nm), ".rds")))$img))

cat("\n=== silhouette placement (cropped window) ===\n")
print(data.frame(species = anchors$Species,
                 anchor = ifelse(anchors$edge, "edge", "line start"),
                 log10_x_anchor = round(log10(anchors$x0), 2),
                 log10_x_drawn  = round(log10(anchors$x), 2),
                 log10_y_anchor = round(anchors$y0, 2),
                 log10_y_drawn  = round(anchors$y, 2),
                 height_dec = round(anchors$h, 2)), row.names = FALSE)
cat("  placed:", nrow(anchors), "of", n_distinct(in_win(df_sp)$Species),
    "groups visible in the window\n")

pA <- ggplot() +
  geom_line(data = df_sp, aes(w, ly, colour = Species), linewidth = 0.95,
            alpha = 0.9, show.legend = FALSE) +
  geom_line(data = df_com, aes(w, ly, colour = arm, linetype = arm),
            linewidth = 0.85) +
  scale_colour_manual(values = c(sp_cols, ARM_COLS), name = NULL,
                      breaks = names(ARM_COLS)) +
  scale_linetype_manual(values = c(Exploited = "dashed", Unexploited = "solid"),
                        name = NULL) +
  x_scale_shared() +
  coord_cartesian(xlim = c(W_LO, W_HI), ylim = c(y_bot, y_top), expand = FALSE) +
  labs(x = NULL,
       y = expression(log[10]~"biomass per unit body mass, "*n(w)%.%w~"(g g"^-1*")"),
       tag = "A") +
  theme_classic(base_size = BASE_SIZE) + shared_axis_theme +
  # Lower-left: that corner is empty in the cropped window -- no group's spectrum
  # reaches below ~10^4 at these masses -- and every other position is taken.
  theme(panel.grid = element_blank(), legend.position = c(0.11, 0.13),
        legend.background = element_blank(), legend.key.width = unit(1.4, "lines"),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        # tight against panel B: the two share an x axis and should read as one
        plot.margin = margin(5.5, 14, 2, 5.5),
        plot.tag = element_text(face = "bold", size = 14)) +
  guides(colour = guide_legend(override.aes = list(linewidth = 1)))

# One layer per group: height and fill must vary per silhouette, and a
# fixed-parameter layer is more predictable here than mapping them.
for (i in seq_len(nrow(anchors))) {
  pA <- pA + rphylopic::geom_phylopic(
    data = anchors[i, , drop = FALSE], aes(x = x, y = y, img = img),
    fill = anchors$colour[i], height = anchors$h[i], alpha = 1,
    inherit.aes = FALSE)
}

# =============================================================================
# PANEL b -- abundance ratio, Exploited / Unexploited, 2001-2010
# =============================================================================
# Ratio is formed WITHIN each member and then summarised across members, so the
# spread shown is the spread of the paired effect, not of the two levels.
ratio_matrix <- SP$community_fished / SP$community_clim
ratio_matrix[!is.finite(ratio_matrix)] <- NA
n_valid_per_bin <- apply(ratio_matrix, 2, function(x) sum(is.finite(x)))
min_valid_for_ribbon <- ceiling(nrow(ratio_matrix) * 0.50)

pair_ratio_stats <- data.frame(
  w            = w,
  n_valid      = n_valid_per_bin,
  ratio_median = apply(ratio_matrix, 2, median,   na.rm = TRUE),
  ratio_q25    = apply(ratio_matrix, 2, quantile, probs = 0.25, na.rm = TRUE),
  ratio_q75    = apply(ratio_matrix, 2, quantile, probs = 0.75, na.rm = TRUE)
) %>%
  mutate(ratio_q25 = ifelse(n_valid >= min_valid_for_ribbon, ratio_q25, NA_real_),
         ratio_q75 = ifelse(n_valid >= min_valid_for_ribbon, ratio_q75, NA_real_))

ratio_data <- pair_ratio_stats %>% filter(w >= W_LO)
cat(sprintf("\npanel b ratio: median %.3f-%.3f | ribbon %.3f-%.3f\n",
            min(ratio_data$ratio_median, na.rm = TRUE),
            max(ratio_data$ratio_median, na.rm = TRUE),
            min(ratio_data$ratio_q25, na.rm = TRUE),
            max(ratio_data$ratio_q75, na.rm = TRUE)))

# --- dominance bands ----------------------------------------------------------
# Each band names the functional group holding the most biomass in that octave
# of body mass, so the ratio curve can be read against who is actually there.
dominance_group_levels <- c(
  "Krill", "Salps", "Pelagic fishes", "Commercial fishes", "Squids",
  "Toothfishes", "Flying birds & penguins", "Seals", "Large marine mammals")

sp_to_dominance <- function(sp_name) dplyr::case_when(
  sp_name == "antarctic krill"                                ~ "Krill",
  sp_name == "salps"                                          ~ "Salps",
  sp_name %in% c("mesopelagic fishes", "bathypelagic fishes") ~ "Pelagic fishes",
  sp_name == "shelf and coastal fishes"                       ~ "Commercial fishes",
  sp_name == "squids"                                         ~ "Squids",
  sp_name == "toothfishes"                                    ~ "Toothfishes",
  sp_name %in% c("flying birds", "small divers")              ~ "Flying birds & penguins",
  sp_name %in% c("medium divers", "large divers")             ~ "Seals",
  sp_name %in% c("minke whales", "orca", "sperm whales",
                 "baleen whales")                             ~ "Large marine mammals",
  TRUE                                                        ~ NA_character_)

sp_dom <- setNames(vapply(sp_names, sp_to_dominance, character(1)), sp_names)
sp_in_dom <- sp_names[!is.na(sp_dom)]

# median n(w) across members, per species -> biomass in each bin
sp_median_n <- t(apply(SP$species_fished, c(2, 3), median))   # [w, species]
colnames(sp_median_n) <- sp_names
bm_mat <- sweep(sp_median_n, 1, w * dw, "*")

fg_bm <- sapply(dominance_group_levels, function(fg) {
  s <- sp_in_dom[sp_dom[sp_in_dom] == fg]
  if (!length(s)) return(rep(0, length(w)))
  rowSums(bm_mat[, s, drop = FALSE])
})
bm_dominance <- as.data.frame(fg_bm) %>% mutate(w = w) %>%
  pivot_longer(-w, names_to = "functional_group", values_to = "biomass") %>%
  mutate(functional_group = factor(functional_group, levels = dominance_group_levels))

shared_log_breaks <- seq(log10(min(w)), log10(max(w)), length.out = 61)
agg_dominance <- bm_dominance %>% filter(w > 0) %>%
  mutate(bin_id   = findInterval(log10(w), shared_log_breaks, all.inside = TRUE),
         bin_left = 10^shared_log_breaks[bin_id],
         bin_right = 10^shared_log_breaks[bin_id + 1]) %>%
  group_by(bin_id, bin_left, bin_right, functional_group) %>%
  summarise(biomass = sum(biomass, na.rm = TRUE), .groups = "drop")

dominant_per_bin <- agg_dominance %>%
  group_by(bin_id, bin_left, bin_right) %>%
  slice_max(biomass, n = 1, with_ties = FALSE) %>% ungroup() %>%
  arrange(bin_left) %>%
  select(bin_id, bin_left, bin_right, dominant_group = functional_group)

nn <- nrow(dominant_per_bin)
bands_raw <- dominant_per_bin %>%
  mutate(run_id = cumsum(c(1L, as.integer(dominant_group[-nn] != dominant_group[-1])))) %>%
  group_by(run_id, dominant_group) %>%
  summarise(band_xmin = min(bin_left), band_xmax = max(bin_right), .groups = "drop") %>%
  arrange(band_xmin)

# The large-mammal band runs from the first octave they dominate to the top of
# the plotted range in one piece: their spectra are spiky enough that a strict
# per-octave rule would break it into slivers.
mm_group     <- "Large marine mammals"
non_mm_bands <- bands_raw %>% filter(dominant_group != mm_group)
mm_bands_raw <- bands_raw %>% filter(dominant_group == mm_group)
if (nrow(mm_bands_raw) > 0) {
  mm_start <- min(mm_bands_raw$band_xmin)
  non_mm_bands <- non_mm_bands %>% mutate(band_xmax = pmin(band_xmax, mm_start)) %>%
    filter(band_xmax > band_xmin)
  bands_fixed <- bind_rows(non_mm_bands,
    data.frame(band_xmin = mm_start, band_xmax = W_HI,
               dominant_group = mm_group, stringsAsFactors = FALSE)) %>%
    arrange(band_xmin)
} else bands_fixed <- non_mm_bands
if (nrow(bands_fixed) > 0) {
  bands_fixed$band_xmin[1]                 <- W_LO
  bands_fixed$band_xmax[nrow(bands_fixed)] <- W_HI
}
cat("dominance bands:\n")
print(bands_fixed %>% mutate(across(where(is.numeric), ~signif(.x, 4))) %>%
        as.data.frame(), row.names = FALSE)

# --- ratio panel y window -----------------------------------------------------
# Set from the rebuilt data rather than inherited. On this ensemble the ribbon
# tops out near 1.08, so the legacy limit of 1.65 would leave the whole figure
# in its lower two-thirds.
Y_LIM <- c(0, max(1.10, ceiling(max(ratio_data$ratio_q75, na.rm = TRUE) * 20) / 20) + 0.10)
cat(sprintf("panel b y limits: %.2f to %.2f\n", Y_LIM[1], Y_LIM[2]))

mm_fill <- "#7bafd4"; mm_alpha <- 0.18
generate_band_annotations <- function(bands_df, y_label_pos, y_min = 0) {
  grey_light <- "grey85"; grey_dark <- "grey70"; band_alpha_val <- 0.40
  short_labels <- c(
    "Krill" = "Krill", "Salps" = "Salps", "Pelagic fishes" = "Pelagic\nfishes",
    "Commercial fishes" = "Commercial\nfishes", "Squids" = "Squids",
    "Toothfishes" = "Toothfishes", "Flying birds & penguins" = "Flying birds\n& penguins",
    "Seals" = "Seals", "Large marine mammals" = "Large marine\nmammals")
  rect_annots <- list(); text_annots <- list()
  for (i in seq_len(nrow(bands_df))) {
    grp  <- as.character(bands_df$dominant_group[i])
    xmin <- bands_df$band_xmin[i]; xmax <- bands_df$band_xmax[i]
    if (!grp %in% names(short_labels)) next
    if (grp == mm_group) { fill_col <- mm_fill; fill_alpha <- mm_alpha * 1.4
    } else { fill_col <- if (i %% 2 == 1) grey_light else grey_dark
             fill_alpha <- band_alpha_val }
    xmid  <- sqrt(max(xmin, W_LO) * xmax)
    y_off <- if (grp == "Flying birds & penguins") -y_label_pos * 0.065 else 0
    rect_annots <- c(rect_annots, list(
      annotate("rect", xmin = xmin, xmax = xmax, ymin = y_min, ymax = Inf,
               fill = fill_col, alpha = fill_alpha)))
    text_annots <- c(text_annots, list(
      annotate("text", x = xmid, y = y_label_pos + y_off, label = short_labels[grp],
               size = 3.2, colour = "grey25", hjust = 0.5)))
  }
  c(rect_annots, text_annots)
}

# --- the four whale troughs, labelled -----------------------------------------
compute_whale_annotations <- function(rs) {
  make_trough <- function(w_lo, w_hi) {
    idx <- which(rs$w > w_lo & rs$w < w_hi)
    if (!length(idx)) return(NULL)
    pos <- idx[which.min(rs$ratio_median[idx])]
    list(w = rs$w[pos], y = rs$ratio_median[pos])
  }
  mm_rows <- bands_fixed$dominant_group == mm_group
  if (!any(mm_rows)) return(list())
  whale_start <- min(bands_fixed$band_xmin[mm_rows])
  mk <- make_trough(whale_start * 0.8,           sp_wmax[["minke whales"]] * 1.5)
  ok <- make_trough(sp_wmax[["minke whales"]] * 1.5, sp_wmax[["orca"]] * 1.5)
  sp <- make_trough(sp_wmax[["orca"]] * 1.5,     sp_wmax[["sperm whales"]] * 1.5)
  bl <- make_trough(sp_wmax[["sperm whales"]] * 1.5, W_HI)
  a <- list()
  if (!is.null(mk)) { lx <- mk$w / 2.0; a <- c(a, list(
    annotate("segment", x = lx, xend = mk$w, y = mk$y, yend = mk$y,
             colour = "grey30", linewidth = 0.5),
    annotate("text", x = lx, y = mk$y, label = "Minke whales", size = 3.2,
             hjust = 1, vjust = 0.5, colour = "grey20"))) }
  if (!is.null(ok)) { rx <- ok$w * 1.5; ry <- ok$y - Y_LIM[2] * 0.075; a <- c(a, list(
    annotate("segment", x = ok$w, xend = rx, y = ok$y, yend = ry,
             colour = "grey30", linewidth = 0.5),
    annotate("text", x = rx, y = ry, label = "Orca", size = 3.2,
             hjust = 0, vjust = 0.5, colour = "grey20"))) }
  if (!is.null(sp)) { lx <- sp$w / 2.0; a <- c(a, list(
    annotate("segment", x = lx, xend = sp$w, y = sp$y, yend = sp$y,
             colour = "grey30", linewidth = 0.5),
    annotate("text", x = lx, y = sp$y, label = "Sperm whales", size = 3.2,
             hjust = 1, vjust = 0.5, colour = "grey20"))) }
  if (!is.null(bl)) {
    # one line in the lower right, with a near-vertical leader up to the trough
    lbl_x <- bl$w * 1.04; lbl_y <- Y_LIM[2] * 0.025
    a <- c(a, list(
      annotate("segment", x = lbl_x, xend = bl$w, y = lbl_y, yend = bl$y,
               colour = "grey30", linewidth = 0.5),
      annotate("text", x = lbl_x, y = lbl_y, label = "Large baleen whales",
               size = 3.2, hjust = 1, vjust = 0.5, colour = "grey20"))) }
  a
}

ribbon_colour <- "#e41a1c"
band_annots   <- generate_band_annotations(bands_fixed,
                   y_label_pos = Y_LIM[2] * 0.955, y_min = Y_LIM[1])
whale_annots  <- compute_whale_annotations(ratio_data)

pB <- ggplot(ratio_data, aes(x = w)) +
  band_annots +
  whale_annots +
  geom_ribbon(aes(ymin = ratio_q25, ymax = ratio_q75), fill = ribbon_colour,
              alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.8) +
  geom_line(aes(y = ratio_median), colour = ribbon_colour, linewidth = 1.2) +
  x_scale_shared() +
  scale_y_continuous(limits = Y_LIM, oob = scales::squish,
                     breaks = scales::pretty_breaks(n = 6),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Body mass", y = "Abundance ratio (Exploited / Unexploited)", tag = "B") +
  theme_classic(base_size = BASE_SIZE) + shared_axis_theme +
  theme(panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
        plot.margin = margin(2, 14, 5.5, 5.5),
        plot.tag = element_text(face = "bold", size = 14)) +
  coord_cartesian(clip = "off")

# =============================================================================
# COMPOSE
# =============================================================================
fig <- (pA / pB) + plot_layout(heights = c(1, 1))

png_out <- file.path(FIGS, sprintf("fig_spectrum_ratio_%s.png", SUF))
pdf_out <- file.path(FIGS, sprintf("fig_spectrum_ratio_%s.pdf", SUF))
ggsave(png_out, fig, width = 9.5, height = 11, dpi = 300, bg = "white",
       limitsize = FALSE)
ggsave(pdf_out, fig, width = 9.5, height = 11, bg = "white", limitsize = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")