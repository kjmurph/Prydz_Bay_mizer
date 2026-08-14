# =============================================================================
# FIGURE 1 (SMOOTHED MEAN-MAXIMUM-SIZE + DOMAIN MAP INSET) -- rebuilt 44, cut A,
# 167 members.
#
#   A  Observed catch, stacked by species, with a circumpolar Southern Ocean
#      inset in the top right showing the Prydz Bay model domain
#   B  Mean maximum body mass of the observed catch under a 3-year centred
#      rolling window -- the smoothed series ONLY, on a tightened y axis
#
# Sibling of F01_figure1_mmw_smooth3_rebuilt167.R, which stays as it is. Read its
# header for the smoothing: the window is applied to the CATCH, not the
# indicator, and a year with no catch of its own stays blank. That is unchanged
# here. What changes (Kieran, 2026-08-10):
#
# ------------------------------------------------------------------------------
#  1. THE MAP INSET. Circumpolar Southern Ocean, after the SOMEME Fig 1 recipe
#     (Fish-MIP/SOMEME, scripts/Fig1_Map_CCAMLR-MPA-mgt_MEASO.md, by Denisse
#     Fierro Arcos), which plots a south polar basemap with geom_sf, sets square
#     projected limits and lets coord_sf draw the graticule and its lat/long
#     labels. CCAMLR MPA planning domains and MEASO regions are replaced by the
#     one polygon this paper is about, and there is no legend.
#
#     THE DOMAIN COMES FROM THE PROJECT'S OWN SHAPEFILE,
#     model_domains/FishMIP_regional_models/FishMIP_regional_models.shp, row
#     region == "Prydz Bay" (the FishMIP regional model registry; the Prydz Bay
#     row is flagged "EwE, Mizer"). It is a single polygon carrying 172 rings --
#     one exterior boundary plus 171 island holes -- clipped to the coastline,
#     spanning 60-90 E and 69.8-56.7 S.
#
#     PROJECTION: EPSG:3031, Antarctic Polar Stereographic. The reference script
#     inherits its CRS from the CCAMLR shapefile, which this project does not
#     carry, so the standard Antarctic projection is used instead.
#
#     THE OUTLINE IS DENSIFIED IN LON/LAT BEFORE PROJECTING (densify_ll below).
#     The exterior ring is dense along the coast -- median vertex step 0.006
#     degrees -- but its three open-ocean edges are chords: the northern edge
#     carries a SINGLE vertex and the largest step in the ring is 9.98 degrees.
#     Projected as drawn, the northern boundary would cut across the 56.7 S
#     parallel as a straight chord roughly 127 km inside the true arc. sf's
#     st_segmentize does this properly but needs lwgeom, which is one more
#     dependency than this is worth, so the rings are interpolated directly.
#     Rings that are already dense gain nothing (9,238 vertices -> 9,402).
#
#     EXTENT is set from an explicit latitude cutoff (LAT_LIM) by projecting
#     that parallel and taking its radius, rather than from the reference
#     script's hard-coded +/-5774572.727594968, which is meaningless without the
#     CRS it was derived under. LAT_LIM = -55 (Kieran, 2026-08-11), matched to a
#     reference screenshot: Antarctica fills a little under 60% of the frame and
#     the domain runs close to the right-hand edge without touching it. THE
#     BINDING CONSTRAINT IS THE DOMAIN ITSELF -- its northern edge sits at
#     56.7 S, and because it lies around 75 E it meets the SQUARE panel's right
#     edge rather than the inscribed circle, so it survives any cutoff down to
#     about -57.5 and is clipped below that. -55 leaves roughly 9% margin.
#
#     Two earlier values and why they were wrong: -50 was too loose (the domain
#     sat well inside and the corners carried South America, New Zealand and
#     Australia), and -25 was looser still -- a WIDER view, not a tighter one.
#
#     The cutoff also sets the latitude labels. On a square polar panel the only
#     parallels reaching the edge are those north of the cutoff, and each
#     crosses every edge TWICE, so labels come in pairs. At -55 exactly one
#     round parallel (50 S) lies between the inscribed circle and the corners
#     (about 42 S), giving one symmetric pair; -25 put every parallel from 20 S
#     north on the edge and the labels repeated messily.
#
#     DO NOT set graticule spacing with scale_x/y_continuous(breaks = ...) here.
#     Without coord_sf(default_crs = ...) those breaks are read as PROJECTED
#     METRES, not degrees; doing so silently replaced the latitude labels with
#     the panel-corner value ("29.20407769 S") while leaving the longitude
#     labels looking correct. The automatic graticule is used instead, and
#     label_axes puts longitude on three sides so that the meridians bracketing
#     the domain (60 E and 120 E) are labelled on the right, next to it.
#
#  2. PANEL B SHOWS THE SMOOTHED SERIES ONLY. The sibling draws the unsmoothed
#     series faintly behind, to show what the window removed; that job is done
#     and this is the presentation version. The two-entry key goes with it.
#
#  3. THE Y AXIS IS TIGHTENED TO THE SMOOTHED SERIES, 0.8 to 130 t against the
#     sibling's 10^-5.5 to 10^2.15. The smoothed series spans 0.997 to 103 t, so
#     this is about 0.1 decades of headroom each side before the default 5%
#     expansion.
#
#     THE COST, AND IT IS NOT COSMETIC: five of the nine species reference lines
#     fall outside the new limits -- toothfishes (0.158 t), squids (0.0207 t),
#     shelf & coastal fishes (0.00242 t), bathypelagic fishes (6.04e-4 t) and
#     antarctic krill (4.17e-6 t). The panel's key is therefore NO LONGER
#     identical to panel A's legend, which the unsmoothed build was built around.
#     The four that survive (large baleen, sperm, orca, minke whales) are the
#     ones the series actually visits. The dropped set is filtered from the data
#     rather than hand-listed, and printed at run time, so it stays correct if
#     the limits are changed. Widening the floor to 0.1 t would recover the
#     toothfish line at the cost of a decade of empty panel.
#
#  4. THE SPECIES KEY IS AN INSET BOX IN PANEL A, above the map, instead of the
#     collected strip along the bottom of the figure (Kieran, 2026-08-10). It is
#     a real ggplot legend placed with legend.position = "inside", not a drawn
#     grob, so it still comes from the fill scale and cannot drift out of step
#     with panel A. plot_layout(guides = "collect") and the shared bottom strip
#     are both gone; panel B contributes no guide, so nothing is lost.
#
#     THE SHAPE OF THE KEY IS SET BY THE WIDTH IT HAS TO FIT INTO. Three rows,
#     hence three columns for nine groups, is about the map's width; the old
#     two-row/five-column arrangement is roughly 4.7 in and would collide with
#     the "Krill fishing starts" annotation, which is right-aligned on the 1974
#     vline and ends at 0.585 npc. Keys are 0.345 cm and labels 6.4 pt, down
#     from the ggplot defaults, which is what lets three columns clear that mark
#     -- the key is about 0.345 npc wide, so it starts around 0.655.
#
#     The figure is saved 0.9 in shorter (9.6 rather than 10.5) because the
#     bottom legend strip no longer needs the room; the panels themselves are
#     unchanged, and so is the map, whose size is pinned by its width fraction
#     rather than by the box it sits in.
#
# REQUIRES sf, rnaturalearth and rnaturalearthdata in addition to the usual set.
#
# Writes Manuscript figures/fig1_mmw_smooth3_map_rebuilt167.{png,pdf}
#        Manuscript figures/fig1_inset_map_prydz_domain.{png,pdf}  (map alone)
#        Manuscript data/fig1_mmw_smooth3_map_series_rebuilt167.csv
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(scales); library(grid); library(sf); library(rnaturalearth)
})

DATA <- "Manuscript data"; FIGS <- "Manuscript figures"
dir.create(FIGS, showWarnings = FALSE)
SUF <- "rebuilt167"
X_LIM <- c(1925, 2012)          # panel A and B share this exactly
X_BRK <- seq(1930, 2010, 10)    # first tick on the first catch year, then decadal
SMOOTH_K <- 3                   # centred window width, years
CRS_SO   <- "EPSG:3031"         # Antarctic Polar Stereographic
LAT_LIM  <- -55                 # northern edge of the inset; see header note 1
DOM_SHP  <- "model_domains/FishMIP_regional_models/FishMIP_regional_models.shp"
DOM_ROW  <- "Prydz Bay"
COL_DOM  <- "#D7263D"
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

eff <- readRDS("effort_array_1841_2010.rds"); yv <- as.numeric(rownames(eff))
onset <- function(s){y <- yv[eff[,s]>0]; if(length(y)) min(y) else NA}
whal <- min(c(onset("baleen whales"), onset("sperm whales")), na.rm = TRUE)
krl  <- onset("antarctic krill")

# ------------------------------------------------------- THE MAP INSET --------
# Interpolate every ring in lon/lat so edges that follow a parallel or meridian
# project as arcs rather than chords. See header note 1 for why this matters
# here and why st_segmentize is not used.
densify_ll <- function(g, max_deg = 0.25) {
  cr <- as.data.frame(st_coordinates(g))
  lc <- grep("^L", names(cr), value = TRUE)
  # one polygon, many rings: L1 indexes the ring, everything above it is constant
  stopifnot(all(vapply(cr[setdiff(lc, "L1")],
                       function(v) length(unique(v)) == 1L, logical(1))))
  rings <- lapply(split(cr[c("X", "Y")], factor(cr$L1, levels = unique(cr$L1))),
                  function(m) {
    m <- as.matrix(m)
    out <- do.call(rbind, lapply(seq_len(nrow(m) - 1), function(i) {
      n <- max(2, ceiling(max(abs(m[i + 1, ] - m[i, ])) / max_deg))
      cbind(seq(m[i, 1], m[i + 1, 1], length.out = n),
            seq(m[i, 2], m[i + 1, 2], length.out = n))[-n, , drop = FALSE]
    }))
    unname(rbind(out, m[nrow(m), ]))
  })
  st_sfc(st_multipolygon(list(rings)), crs = st_crs(g))
}

dom <- read_sf(DOM_SHP)
stopifnot(DOM_ROW %in% dom$region)
pb <- dom[dom$region == DOM_ROW, ]
stopifnot(nrow(pb) == 1)
pb_ll <- densify_ll(st_geometry(pb))
stopifnot(st_is_valid(pb_ll))
pb_p  <- st_transform(pb_ll, CRS_SO)
cat("\n=== inset map ===\n")
cat(sprintf("  domain '%s': %d -> %d vertices after densifying\n", DOM_ROW,
            nrow(st_coordinates(st_geometry(pb))), nrow(st_coordinates(pb_ll))))
print(st_bbox(st_geometry(pb)))

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(CRS_SO)

# Extent: the radius of the LAT_LIM parallel under this projection.
LIM <- max(abs(st_bbox(
  st_transform(st_sfc(st_multipoint(cbind(seq(-180, 180, 0.5), LAT_LIM)),
                      crs = 4326), CRS_SO))))
cat(sprintf("  extent: %g S -> +/-%.0f m in %s\n", LAT_LIM, LIM, CRS_SO))

# Graticule breaks are left automatic ON PURPOSE -- see header note 1. label_axes
# puts longitude on top/bottom/right so the meridians either side of the domain
# are labelled beside it, and latitude on the left.
p_map <- ggplot() +
  geom_sf(data = world, fill = "grey85", colour = "grey45", linewidth = 0.15) +
  geom_sf(data = pb_p, fill = COL_DOM, colour = "#8C1122", alpha = 0.75,
          linewidth = 0.3) +
  coord_sf(xlim = c(-LIM, LIM), ylim = c(-LIM, LIM), crs = CRS_SO,
           expand = FALSE,
           label_axes = list(top = "E", right = "E", bottom = "E", left = "N")) +
  theme_bw(base_size = 7) +
  # NO BOUNDING BOX (Kieran, 2026-08-11): the map floats on panel A's white
  # rather than sitting in a framed box. Ticks are dropped with it -- without a
  # border to sit on they read as stray marks -- so the graticule labels carry
  # the axes on their own.
  theme(panel.grid = element_line(colour = "grey80", linewidth = 0.2),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 5, colour = "grey25"),
        # opaque, so the inset sits cleanly over panel A's empty top right
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(1, 1, 1, 1))

map_png <- guard(file.path(FIGS, "fig1_inset_map_prydz_domain.png"))
map_pdf <- guard(file.path(FIGS, "fig1_inset_map_prydz_domain.pdf"))
ggsave(map_png, p_map, width = 3.2, height = 3.2, dpi = 300)
ggsave(map_pdf, p_map, width = 3.2, height = 3.2)

# ------------------------------------------------------------------ PANEL A ---
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
  # THE SPECIES KEY IS AN INSET BOX IN PANEL A, above the map -- see header
  # note 4. Three rows (so three columns for nine groups) keeps it about the
  # map's width; five columns would be 4.7 in and would run into the "Krill
  # fishing starts" annotation, which ends at 0.585 npc.
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification.inside = c(1, 1),
        # no outline (Kieran, 2026-08-11); the white fill stays, so the key
        # still masks whatever is behind it
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA),
        legend.key.size = unit(0.345, "cm"),
        legend.key.spacing.y = unit(0.5, "pt"),
        legend.text = element_text(size = 6.4),
        legend.margin = margin(3, 4, 3, 3),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        plot.tag = element_text(face = "bold", size = 14)) +
  guides(fill = guide_legend(nrow = 3, order = 1))

# The inset goes in panel A's right-hand side, which is empty after 1975 -- the
# last whaling-era catch peak is 1961 and the krill peak reaches only 12% of the
# panel height. It starts right of 0.66 npc so it clears the "Krill fishing
# starts" annotation, which is right-aligned on the 1974 vline (0.587 npc).
#
# THE MAP'S SIZE IS PINNED BY ITS WIDTH FRACTION, NOT THIS BOX. coord_sf holds it
# square, so it renders at min(box width, box height) and is letterboxed in the
# rest; with left = 0.67 unchanged the width is the binding constraint and the
# map stays the size it was before the legend moved in. `top` is therefore free
# to drop to make room for the key above it (header note 4), and `bottom` sits
# at 0.03 rather than 0 so the map does not touch the axis -- the 1982-2010
# catch it covers peaks at 3.8 kt in 2006, under 1% of the panel height.
pA_ins <- pA + inset_element(p_map, left = 0.67, bottom = 0.12,
                             right = 1.00, top = 0.86, align_to = "panel")

# ------------------------------------------------------------------ PANEL B ---
SP <- readRDS(file.path(DATA, sprintf("spectra_ref_period_%s.rds", SUF)))
wmax <- setNames(SP$traits$w_max, SP$traits$species)      # g

spp <- setdiff(names(obs_raw), "Year")
stopifnot(all(spp %in% names(wmax)))
C <- vapply(obs_raw[spp], function(v) {
  x <- suppressWarnings(as.numeric(v)); x[is.na(x)] <- 0; pmax(x, 0)
}, numeric(nrow(obs_raw)))
num <- as.vector(C %*% wmax[spp]); den <- rowSums(C)

# --- the 3-year centred rolling window ----------------------------------------
# Positional window, so a calendar window only on a contiguous year grid.
stopifnot(all(diff(obs_raw$Year) == 1))
roll_sum <- function(v, k = SMOOTH_K) {
  h <- (k - 1) / 2
  vapply(seq_along(v), function(i)
    sum(v[max(1, i - h):min(length(v), i + h)]), numeric(1))
}
# `den > 0`: a year with no catch of its own stays NA, window or no window.
MMW <- data.frame(Year = obs_raw$Year,
                  total_kt = den / 1e9,
                  mmw_t    = ifelse(den > 0, num / den, NA) / 1e6,
                  mmw_t_s3 = ifelse(den > 0, roll_sum(num) / roll_sum(den), NA) / 1e6) %>%
  filter(Year >= X_LIM[1], Year <= X_LIM[2])

e1 <- MMW$Year <= 1970; e2 <- MMW$Year >= 1971 & MMW$Year <= 2010
sm <- function(v, sel) median(v[sel & is.finite(v)])
k  <- is.finite(MMW$mmw_t_s3) & MMW$Year <= 2010
fit <- lm(log10(mmw_t_s3) ~ Year, data = MMW[k, ])
cat("\n=== mean maximum body mass, ", SMOOTH_K, "-year centred window ===\n", sep = "")
cat(sprintf("  1930-1970 median  %.3g t\n", sm(MMW$mmw_t_s3, e1)))
cat(sprintf("  1971-2010 median  %.3g t  (%.1fx)\n", sm(MMW$mmw_t_s3, e2),
            sm(MMW$mmw_t_s3, e1) / sm(MMW$mmw_t_s3, e2)))
cat(sprintf("  trend             %+.3f log10 units per decade (p = %.2g, R2 = %.2f)\n",
            coef(fit)[2] * 10, summary(fit)$coefficients[2, 4], summary(fit)$r.squared))
cat(sprintf("  range             %.3g to %.3g t\n",
            min(MMW$mmw_t_s3, na.rm = TRUE), max(MMW$mmw_t_s3, na.rm = TRUE)))

# --- species w_max reference lines --------------------------------------------
REF_TAXA <- names(which(colSums(eff) > 0))
stopifnot(setequal(REF_TAXA, caught))
REF_TAXA <- REF_TAXA[order(wmax[REF_TAXA], decreasing = TRUE)]
REF_RIGHT <- c("baleen whales", "sperm whales")
REF_BELOW <- c("minke whales")
ref_all <- data.frame(Species = REF_TAXA,
                      y = unname(wmax[REF_TAXA]) / 1e6,
                      lab = pretty_lab(REF_TAXA),
                      stringsAsFactors = FALSE) %>%
  mutate(side = ifelse(Species %in% REF_RIGHT, "right", "left"),
         x  = ifelse(side == "left", X_LIM[1] + 1, X_LIM[2] - 1),
         hj = ifelse(side == "left", 0, 1),
         vj = ifelse(Species %in% REF_BELOW, 1.35, -0.45))
stopifnot(!any(is.na(sp_cols[ref_all$Species])))

# Header note 3: the tightened axis cannot hold all nine. Filtered from the data,
# not hand-listed, so this stays correct if y_lim changes.
y_lim <- c(0.8, 130)
ref <- ref_all %>% filter(y >= y_lim[1], y <= y_lim[2])
cat("\n=== panel B reference lines ===\n")
cat("  drawn  :", paste(ref$lab, collapse = ", "), "\n")
cat("  dropped (outside the tightened y axis):\n")
print(ref_all %>% filter(!Species %in% ref$Species) %>%
        transmute(Species, w_max_t = signif(y, 3)), row.names = FALSE)

pB <- ggplot(MMW, aes(Year, mmw_t_s3)) +
  geom_hline(data = ref, aes(yintercept = y, colour = Species),
             linetype = "dotted", linewidth = 0.5, alpha = 0.85,
             show.legend = FALSE, inherit.aes = FALSE) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  geom_text(data = ref, aes(x = x, y = y, label = lab, hjust = hj, vjust = vj,
                            colour = Species),
            size = 2.7, show.legend = FALSE, inherit.aes = FALSE) +
  geom_line(linewidth = 0.7, colour = "grey20", na.rm = TRUE) +
  geom_point(size = 1.5, colour = "grey10", na.rm = TRUE) +
  # NO IN-PANEL TEXT (Kieran, 2026-08-10), as in the unsmoothed builds: there is
  # only one series, so nothing needs distinguishing, and the window belongs in
  # the caption. SMOOTH_K is still printed to the console with the statistics.
  scale_colour_manual(values = sp_cols, guide = "none") +
  scale_x_continuous(breaks = X_BRK, limits = X_LIM) +
  scale_y_log10(breaks = 10^seq(0, 2, 1),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "l", linewidth = 0.25) +
  coord_cartesian(ylim = y_lim) +
  labs(x = "Year",
       y = expression("Mean maximum body mass of catch, "*w[max]*" (t)"),
       tag = "B") +
  theme_classic(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.tag = element_text(face = "bold", size = 14))

# NO COLLECTED LEGEND (header note 4). The key now lives inside panel A, so
# guides = "collect" and the shared bottom strip both go; pB contributes no
# guide, so nothing is lost. The figure is 0.9 in shorter for the same panels.
fig <- (pA_ins / pB) + plot_layout(heights = c(1, 0.7))
png_out <- guard(file.path(FIGS, sprintf("fig1_mmw_smooth3_map_%s.png", SUF)))
pdf_out <- guard(file.path(FIGS, sprintf("fig1_mmw_smooth3_map_%s.pdf", SUF)))
ggsave(png_out, fig, width = 11, height = 9.6, dpi = 300, limitsize = FALSE)
ggsave(pdf_out, fig, width = 11, height = 9.6, limitsize = FALSE)
write.csv(MMW, guard(file.path(DATA,
          sprintf("fig1_mmw_smooth3_map_series_%s.csv", SUF))), row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n  ", map_png, "\n")