# =============================================================================
# FIGURE 1 (GEOMETRIC MEAN MAXIMUM BODY MASS + MEAN TROPHIC LEVEL, MAP INSET)
# -- inset-key revision of
#    F01_figure1_mmwgeo_mti_linear_map_rebuilt167_pubtext.R
#
#   A  Observed catch, stacked by species, with the species key and an
#      Antarctic inset showing the Prydz Bay model domain, both inside the
#      empty top-right of the panel
#   B  GEOMETRIC mean maximum body mass of the observed catch, ANNUAL (no
#      smoothing), on a LINEAR axis, shown as points and line only
#   C  Mean trophic level of the observed catch, ANNUAL (no smoothing), linear
#      axis, carrying the dotted species reference lines and species labels
#
# WHAT CHANGED FROM THE SOURCE SCRIPT (layout and labels only; no data or
# calculations are touched):
#
# 1. A: y title reads "thousand tonnes" instead of 10^3 t.
#
# 2. A: SPECIES KEY is back inside the panel, in the empty top-right corner
#    right of the krill-fishing line, at 7 pt (was 8 pt). In two columns the
#    full names fit, so nothing is abbreviated. LEGEND_POS <- "column" gives
#    the other layout: one column down the right edge, map to its left.
#
# 3. A: MAP INSET draws Antarctica alone (ne_countries(continent =
#    "Antarctica")) instead of every country cropped to 55 deg S, which removes
#    the tip of South America and the sub-Antarctic islands that sat at the
#    frame edges; Antarctic islands under MIN_ISLAND (2,500 km2) go too, as
#    the South Orkneys, South Shetlands and Balleny Islands printed as specks
#    on the frame edge. The frame is cropped to the continent + domain, so the
#    continent fills the inset, and the inset is sized in mm and parked
#    directly against the key (measured from the built legend), so it follows
#    the key if LEG, KEY_SZ or KEY_NCOL change. Key and map are both kept
#    right of the krill-fishing line. The antimeridian seam (the thin line
#    from the pole to the Ross Ice Shelf) is dissolved.
#
# 4. B: year labels and "Year" title dropped (shared with C), and the y title
#    shortened to "Mean max. weight of catch (t)", which sits within the axis.
#
# 5. C: species labels re-sided so none sits on another line or label: Squids
#    moved left, Shelf & coastal fishes moved right, Bathypelagic fishes sits
#    in the clear band directly above its own line, Minke whales in the same
#    band on the left.
#
# Writes Manuscript figures/fig1_mmwgeo_mti_linear_map_rebuilt167_insetkey.{png,pdf}
#        Manuscript data/fig1_mmwgeo_mti_linear_map_series_rebuilt167_insetkey.csv
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(scales); library(grid); library(sf); library(rnaturalearth)
})

DATA <- "Manuscript data"; FIGS <- "Manuscript figures"
dir.create(FIGS, showWarnings = FALSE)
SUF <- "rebuilt167"
X_LIM <- c(1929, 2010)
X_BRK <- seq(1930, 2010, 10)
CRS_SO   <- "EPSG:3031"
DOM_SHP  <- "model_domains/FishMIP_regional_models/FishMIP_regional_models.shp"
DOM_ROW  <- "Prydz Bay"
COL_DOM  <- "#D7263D"

# ---- output size and typography ---------------------------------------------
# Every size below is in POINTS ON THE PRINTED PAGE, because the canvas is the
# printed size. Geoms take millimetres, so geom text/point sizes are written as
# <points>/.pt rather than as bare numbers.
FIG_W  <- 180 / 25.4   # in -- 180 mm, standard double-column width
FIG_H  <- 8.60         # in -- 218 mm; slightly taller than a straight rescale
                       #       of 11 x 12.5 to give the larger text its room
BASE   <- 9            # pt -- axis text
TITLE  <- 10           # pt -- axis titles
TAG    <- 12           # pt -- panel letters A, B, C
LEG    <- 7            # pt -- legend entries (was 8; 7 lets the key sit in A)
KEY_SZ <- 0.32         # cm -- legend key squares (was 0.40, scaled with LEG)
ANNOT  <- 8            # pt -- "Whaling starts" / "Krill fishing starts"
REFLAB <- 7            # pt -- species reference labels in panel C
PT_SZ  <- 1.0          # mm -- plotted point radius (was 1.4/1.3 on the big
                       #       canvas, where the series had 1.55x the width to
                       #       spread across; kept smaller so the ~75 annual
                       #       points in B do not run together)
LN_SZ  <- 0.55         # mm -- series line width

LEGEND_POS <- "top"      # "top":    key in A's top-right corner, map under it
                         # "column": key as one column down A's right edge,
                         #           map immediately to its left
KEY_NCOL   <- 2          # key columns in "top" mode; two fit the full names
MAP_W      <- 56         # mm -- width of the map inset; its height follows
                         #       from the cropped frame (roughly 4:3). Capped
                         #       to the room right of the krill-fishing line,
                         #       so it comes out narrower in "column" mode
MAP_GAP    <- 1.5        # mm -- clearance between the key and the map
SHARE_X    <- TRUE       # drops B's duplicate year labels and x title, which
                         # C already carries

th_pub <- function() {
  theme_classic(base_size = BASE) +
    theme(panel.grid  = element_blank(),
          axis.text   = element_text(size = BASE, colour = "grey20"),
          axis.title  = element_text(size = TITLE),
          plot.tag    = element_text(face = "bold", size = TAG),
          plot.margin = margin(2, 4, 2, 2))
}

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

# --- assigned trophic levels: McCormack et al. (2020) Prydz Bay Ecopath, Tab 2 ---
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

# ------------------------------------------------------- THE MAP INSET --------
# Interpolate every ring in lon/lat so edges that follow a parallel or meridian
# project as arcs rather than chords.
densify_ll <- function(g, max_deg = 0.25) {
  cr <- as.data.frame(st_coordinates(g))
  lc <- grep("^L", names(cr), value = TRUE)
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

# Antarctica alone, so no other land can reach the frame edges. The 1 m
# buffer + union dissolves the antimeridian seam, which otherwise draws as a
# thin line from the pole to the Ross Ice Shelf; 1 m is far below anything the
# printed inset can show. The buffer leaves a pinhole at the pole (~0.2 km
# across, but its outline prints as a dot), so only exterior rings are kept.
# Islands under MIN_ISLAND are dropped: at this scale (~110 km per mm) they
# print as specks under half a millimetre, and the outermost of them (South
# Orkneys, South Shetlands, Balleny Is.) set the edges of the frame.
MIN_ISLAND <- 2500     # km2 -- 0 keeps every island
ant <- ne_countries(scale = "medium", continent = "Antarctica",
                    returnclass = "sf") %>%
  st_transform(CRS_SO) %>% st_geometry() %>% st_buffer(1) %>% st_union() %>%
  st_cast("POLYGON")
ant <- ant[as.numeric(st_area(ant)) >= MIN_ISLAND * 1e6]
ant <- st_sfc(lapply(ant, function(p) st_polygon(list(p[[1]]))), crs = CRS_SO)

# Frame = continent + domain plus a small pad, rather than a 55 deg S square,
# so the continent fills the inset instead of floating in open ocean. The
# frame is wider than tall (the domain reaches ~57 deg S at 90 E); MAP_ASP
# carries that shape through to the inset box so the map fills it exactly.
MAP_PAD <- 0.02        # fraction of the longer side
bb   <- st_bbox(c(ant, pb_p))
pad  <- MAP_PAD * max(bb[["xmax"]] - bb[["xmin"]], bb[["ymax"]] - bb[["ymin"]])
M_XL <- c(bb[["xmin"]] - pad, bb[["xmax"]] + pad)
M_YL <- c(bb[["ymin"]] - pad, bb[["ymax"]] + pad)
MAP_ASP <- diff(M_XL) / diff(M_YL)      # width / height of the map frame

p_map <- ggplot() +
  geom_sf(data = ant, fill = "grey85", colour = "grey45", linewidth = 0.15) +
  geom_sf(data = pb_p, fill = COL_DOM, colour = "#8C1122", alpha = 0.75,
          linewidth = 0.3) +
  coord_sf(xlim = M_XL, ylim = M_YL, crs = CRS_SO, expand = FALSE) +
  theme_void() +
  theme(panel.grid = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(0, 0, 0, 0))

# ----------------------------------------------------------- PANEL A --------
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

stopifnot(LEGEND_POS %in% c("top", "column"))
pA <- ggplot(obs, aes(Year, t, fill = lab)) +
  geom_area(colour = NA) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  annotate("text", x = 1930.25, y = Inf, label = "Whaling\nstarts", hjust = 0,
           vjust = 1.3, size = ANNOT / .pt, colour = "grey35") +
  annotate("text", x = krl, y = Inf, label = "Krill\nfishing starts", hjust = 1.08,
           vjust = 1.3, size = ANNOT / .pt, colour = "grey35") +
  scale_fill_manual(values = acols, name = NULL) +
  scale_x_continuous(breaks = X_BRK, limits = X_LIM, expand = c(0, 0)) +
  labs(x = NULL, y = expression("Catch (thousand tonnes y"^-1*")"), tag = "A") +
  th_pub() +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification.inside = c(1, 1),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA),
        legend.key.size = unit(KEY_SZ, "cm"),
        legend.key.spacing.x = unit(6, "pt"),
        legend.key.spacing.y = unit(0.5, "pt"),
        legend.text = element_text(size = LEG),
        legend.margin = margin(2, 2, 2, 2),
        axis.text.x = element_blank()) +
  guides(fill = guide_legend(ncol = if (LEGEND_POS == "top") KEY_NCOL else 1,
                             order = 1))

# ----------------------------------------------------------- PANEL B --------
# GEOMETRIC mean maximum body mass of catch (same construction as the source
# figure, but with no species reference lines / labels and the y-axis label
# shortened to fit within the axis).
SP <- readRDS(file.path(DATA, sprintf("spectra_ref_period_%s.rds", SUF)))
wmax <- setNames(SP$traits$w_max, SP$traits$species)

spp <- setdiff(names(obs_raw), "Year")
stopifnot(all(spp %in% names(wmax)))
C <- vapply(obs_raw[spp], function(v) {
  x <- suppressWarnings(as.numeric(v)); x[is.na(x)] <- 0; pmax(x, 0)
}, numeric(nrow(obs_raw)))
num <- as.vector(C %*% log(wmax[spp])); den <- rowSums(C)

mmw_mu <- rep(NA_real_, nrow(obs_raw))
mmw_lo <- rep(NA_real_, nrow(obs_raw))
mmw_hi <- rep(NA_real_, nrow(obs_raw))
for (i in seq_len(nrow(obs_raw))) {
  x <- suppressWarnings(as.numeric(obs_raw[i, spp]))
  x[is.na(x)] <- 0
  total <- sum(x)
  if (total > 0) {
    w <- x / total
    z <- log(wmax[spp])
    mu <- sum(w * z)
    sdlog <- sqrt(sum(w * (z - mu)^2))
    mmw_mu[i] <- exp(mu) / 1e6
    mmw_lo[i] <- exp(mu - sdlog) / 1e6
    mmw_hi[i] <- exp(mu + sdlog) / 1e6
  }
}
MMW <- data.frame(Year = obs_raw$Year,
                  mmw_t = mmw_mu,
                  mmw_lo = mmw_lo,
                  mmw_hi = mmw_hi) %>%
  filter(Year >= X_LIM[1], Year <= X_LIM[2])

Y_FLOOR <- -2
Y_CEIL <- max(MMW$mmw_t, na.rm = TRUE) * 1.12
y_lim <- c(Y_FLOOR, Y_CEIL)
y_breaks <- seq(0, ceiling(y_lim[2] / 25) * 25, 25)

pB <- ggplot(MMW, aes(Year)) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  geom_point(aes(y = mmw_t), size = PT_SZ, colour = "grey20", na.rm = TRUE) +
  geom_line(aes(y = mmw_t), linewidth = LN_SZ + 0.15, colour = "grey20",
            na.rm = TRUE) +
  scale_x_continuous(breaks = X_BRK, limits = X_LIM, expand = c(0, 0)) +
  scale_y_continuous(breaks = y_breaks, limits = y_lim, expand = c(0, 0)) +
  coord_cartesian(ylim = y_lim) +
  labs(x = "Year",
       y = "Mean max. weight of catch (t)",   # fallback: "Mean max. weight (t)"
       tag = "B") +
  th_pub() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

if (SHARE_X) {
  pB <- pB + labs(x = NULL) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
}

# ----------------------------------------------------------- PANEL C --------
# Mean trophic level panel from the MTI figure, with its dotted reference lines
# and labels. Labels on the left start just right of the 1930 reference line;
# labels on the right end just inside 2010.
C_t <- vapply(obs_raw[spp], function(v) {
  x <- suppressWarnings(as.numeric(v)); x[is.na(x)] <- 0; pmax(x, 0)
}, numeric(nrow(obs_raw)))
C_num <- as.vector(C_t %*% TROPHIC_LEVELS[spp])
C_den <- rowSums(C_t)

mti_mu <- rep(NA_real_, nrow(obs_raw))
mti_lo <- rep(NA_real_, nrow(obs_raw))
mti_hi <- rep(NA_real_, nrow(obs_raw))
for (i in seq_len(nrow(obs_raw))) {
  x <- suppressWarnings(as.numeric(obs_raw[i, spp]))
  x[is.na(x)] <- 0
  total <- sum(x)
  if (total > 0) {
    w <- x / total
    z <- TROPHIC_LEVELS[spp]
    mu <- sum(w * z)
    sdz <- sqrt(sum(w * (z - mu)^2))
    mti_mu[i] <- mu
    mti_lo[i] <- mu - sdz
    mti_hi[i] <- mu + sdz
  }
}
MTI <- data.frame(Year = obs_raw$Year,
                  mti = mti_mu,
                  mti_lo = mti_lo,
                  mti_hi = mti_hi) %>%
  filter(Year >= X_LIM[1], Year <= X_LIM[2])

# The reference lines come in two tight groups -- squids 4.336 over shelf &
# coastal 4.281, and bathypelagic 4.055 / minke 3.955 / baleen 3.867 -- so the
# labels go in the clear space around the groups rather than between lines:
#   above the upper pair  Squids (left), Shelf & coastal fishes (right)
#   band between groups   Minke whales (left), Bathypelagic fishes (right,
#                         directly on top of its own line)
#   below the lower trio  Large baleen whales (left)
# Each label is anchored at REF_AT (default: its own line) and offset by REF_VJ
# in text heights (negative = above the anchor, >1 = below it). The two band
# labels are centred in the band in data units, so they stay clear of both
# bounding lines whatever the panel height or font.
BAND <- mean(TROPHIC_LEVELS[c("bathypelagic fishes", "shelf and coastal fishes")])
REF_TAXA <- names(which(colSums(eff) > 0))
stopifnot(setequal(REF_TAXA, caught))
REF_TAXA <- REF_TAXA[order(TROPHIC_LEVELS[REF_TAXA], decreasing = TRUE)]
REF_SIDE <- c("sperm whales" = "left", "orca" = "right",
              "toothfishes" = "left", "squids" = "left",
              "shelf and coastal fishes" = "right",
              "bathypelagic fishes" = "right", "minke whales" = "left",
              "baleen whales" = "left", "antarctic krill" = "left")
REF_AT   <- c("shelf and coastal fishes" = unname(TROPHIC_LEVELS["squids"]),
              "bathypelagic fishes" = BAND, "minke whales" = BAND)
REF_VJ   <- c("sperm whales" = -0.45, "orca" = 1.35,
              "toothfishes" = -0.45, "squids" = -0.45,
              "shelf and coastal fishes" = -0.45,
              "bathypelagic fishes" = 0.5, "minke whales" = 0.5,
              "baleen whales" = 1.35, "antarctic krill" = -0.45)
ref <- data.frame(Species = REF_TAXA,
                  y = unname(TROPHIC_LEVELS[REF_TAXA]),
                  lab = pretty_lab(REF_TAXA),
                  stringsAsFactors = FALSE) %>%
  mutate(side  = unname(REF_SIDE[Species]),
         x     = ifelse(side == "left", 1932, X_LIM[2] - 1),
         y_lab = ifelse(Species %in% names(REF_AT), REF_AT[Species], y),
         hj    = ifelse(side == "left", 0, 1),
         vj    = unname(REF_VJ[Species])) %>%
  filter(Species %in% names(TROPHIC_LEVELS))

pC <- ggplot(MTI, aes(Year)) +
  geom_hline(data = ref, aes(yintercept = y, colour = Species),
             linetype = "dotted", linewidth = 0.5, alpha = 0.85,
             show.legend = FALSE) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  geom_text(data = ref, aes(x = x, y = y_lab, label = lab, hjust = hj, vjust = vj,
                            colour = Species),
            size = REFLAB / .pt, show.legend = FALSE, inherit.aes = FALSE) +
  geom_point(aes(y = mti), size = PT_SZ - 0.05, colour = "grey30", na.rm = TRUE) +
  geom_line(aes(y = mti), linewidth = LN_SZ, colour = "grey30", na.rm = TRUE) +
  scale_colour_manual(values = sp_cols, guide = "none") +
  scale_x_continuous(breaks = X_BRK, limits = X_LIM, expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(2.5, 5.5, 0.5), expand = c(0, 0)) +
  coord_cartesian(ylim = c(2.30, 5.55)) +
  labs(x = "Year", y = "Mean Trophic Index of catch", tag = "C") +
  th_pub() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------------------------------------------ PARK THE MAP AGAINST THE KEY ------
# All in printed mm. The key's size is read off the built legend, and the
# shared panel width off the assembled figure (patchwork aligns A, B and C, so
# it is the canvas less the widest axis furniture), both on a null device.
# ROOM is the width right of the krill-fishing line: the key and the map are
# kept inside it, so neither covers the line or its label. The map box carries
# the frame's own aspect ratio, so the map fills it exactly -- no letterboxing,
# and its edges line up with the key's.
key_mm <- function(p) {
  pdf(NULL); on.exit(dev.off())
  g  <- ggplotGrob(p)
  kb <- g$grobs[[which(g$layout$name == "guide-box-inside")]]
  c(w = convertWidth(sum(kb$widths), "mm", valueOnly = TRUE),
    h = convertHeight(sum(kb$heights), "mm", valueOnly = TRUE))
}
panel_w_mm <- function(fig) {
  pdf(NULL, width = FIG_W, height = FIG_H); on.exit(dev.off())
  w <- patchworkGrob(fig)$widths
  is_null <- vapply(seq_along(w), function(i) unitType(w[i]) == "null", logical(1))
  stopifnot(sum(is_null) == 1)
  FIG_W * 25.4 - sum(vapply(which(!is_null), function(i)
    convertWidth(w[i], "mm", valueOnly = TRUE), numeric(1)))
}
KEY  <- key_mm(pA)
ROOM <- panel_w_mm(pA / pB / pC) * (X_LIM[2] - krl) / diff(X_LIM)
if (KEY[["w"]] > ROOM - MAP_GAP)
  warning("the species key reaches past the krill-fishing line; ",
          "lower LEG or KEY_SZ", call. = FALSE)
map_w <- min(MAP_W, ROOM - MAP_GAP -
                    (if (LEGEND_POS == "column") KEY[["w"]] + MAP_GAP else 0))
map_h <- map_w / MAP_ASP
if (LEGEND_POS == "top") {
  m_right <- unit(1, "npc")
  m_top   <- unit(1, "npc") - unit(KEY[["h"]] + MAP_GAP, "mm")
} else {
  m_right <- unit(1, "npc") - unit(KEY[["w"]] + MAP_GAP, "mm")
  m_top   <- unit(1, "npc")
}
pA_ins <- pA + inset_element(p_map,
                             left = m_right - unit(map_w, "mm"),
                             bottom = m_top - unit(map_h, "mm"),
                             right = m_right, top = m_top,
                             align_to = "panel")

fig <- (pA_ins / pB / pC) + plot_layout(heights = c(0.8, 0.56, 0.56))

STEM <- "fig1_mmwgeo_mti_linear_map_rebuilt167_insetkey"
png_out <- guard(file.path(FIGS, paste0(STEM, ".png")))
pdf_out <- guard(file.path(FIGS, paste0(STEM, ".pdf")))
csv_out <- guard(file.path(DATA, paste0(
  "fig1_mmwgeo_mti_linear_map_series_rebuilt167_insetkey.csv")))

ggsave(png_out, fig, width = FIG_W, height = FIG_H, dpi = 600, limitsize = FALSE)
ggsave(pdf_out, fig, width = FIG_W, height = FIG_H, limitsize = FALSE)
write.csv(MMW, csv_out, row.names = FALSE)

cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n  ", csv_out, "\n")
cat(sprintf("  canvas %.2f x %.2f in (%.0f x %.0f mm) | base text %d pt\n",
            FIG_W, FIG_H, FIG_W * 25.4, FIG_H * 25.4, BASE))
cat(sprintf("  key %.1f x %.1f mm | map %.1f x %.1f mm | room right of krill line %.1f mm (%s layout)\n",
            KEY[["w"]], KEY[["h"]], map_w, map_h, ROOM, LEGEND_POS))
