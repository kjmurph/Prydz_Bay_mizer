# =============================================================================
# FIGURE 1 (SMOOTHED MEAN-TROPHIC-INDEX + DOMAIN MAP INSET) -- rebuilt 44,
# cut A, 167 members.
#
#   A  Observed catch, stacked by species, with the species key and a
#      circumpolar Southern Ocean inset showing the Prydz Bay model domain
#   B  Mean trophic level of the observed catch under a 3-year centred rolling
#      window -- the smoothed series ONLY, on a tightened y axis
#
# EXACTLY F01_figure1_mmw_smooth3_map_rebuilt167.R WITH PANEL B'S INDICATOR
# SWAPPED from asymptotic mass to trophic level. Panel A, the map, the inset
# key, the layout and the figure size are identical; read that script's header
# for all of them, and F01_figure1_mti_rebuilt167.R for the trophic-level
# source. Only the differences are recorded here.
#
#  1. THE INDICATOR. Same construction, TL_i in place of w_max,i, with the
#     window applied to the CATCH rather than to the indicator:
#
#         MTI_3(y) = sum_{y' in W(y)} sum_i C_iy' TL_i
#                    --------------------------------- ,  W(y) = {y-1, y, y+1}
#                    sum_{y' in W(y)} sum_i C_iy'
#
#     TL_i are the assigned trophic levels of McCormack et al. (2020) Prydz Bay
#     Ecopath, Table 2 -- fixed per group, no size or time dependence, which is
#     what keeps panel B observational like panel A. Transcribed from
#     ecosystem_assessment_v3.R:219-230 rather than sourced (that file runs a
#     full assessment on load). The model's own trophic levels are deliberately
#     not used: they run 0.01-1.59 above Ecopath by species, and
#     mizer::getTrophicLevel() is broken under therMizer.
#
#  2. THE AXIS IS LINEAR, not log. One trophic unit is one transfer, so log10 of
#     it means nothing; the y limits are 3.2 to 5.25 against the smoothed
#     series' 3.291 to 5.140, about 0.1 units of headroom each side before the
#     default 5% expansion. This is the same tightening rule the mass variant
#     applies, not a separate judgement.
#
#     SIX OF THE NINE REFERENCE LINES SURVIVE IT, against four on the mass
#     panel. Antarctic krill (TL 2.398) is far below the floor, but sperm whales
#     (5.342) and orca (5.301) are only 0.16 and 0.20 above the series maximum,
#     so raising the ceiling to about 5.42 would recover both for roughly 0.17
#     units of extra headroom. They are dropped here because the tightening rule
#     is the mass variant's and is applied without exception; the dropped set is
#     filtered from the data rather than hand-listed and is printed at run time.
#
#  3. THE LABEL PLACEMENT TABLE IS THE ONE FROM F01_figure1_mti_rebuilt167.R,
#     unchanged, and it has more room here than it did there: the trophic axis
#     now spans 2.05 units over a panel that carries roughly 1.6 in per unit,
#     against 1.2 in per unit in the full-range build. The two close pairs still
#     have to be split across opposite ends of the panel -- squids/shelf &
#     coastal are 0.055 units apart and minke/large baleen whales 0.088 -- and
#     shelf & coastal still sits BELOW its own line, because above it the label
#     lands on the squids line.
#
#  4. RESULT, for the manuscript text: 1930-1970 median 3.99, 1971-2010 median
#     3.86, a fall of 0.136 trophic units, against the 16.8-fold fall in mean
#     maximum body mass over the same break. Quote the smoothed figure only as
#     the smoothed figure -- the annual record gives -0.065, and the difference
#     is real, not rounding (see F01_figure1_mti_smooth3_rebuilt167.R).
#
# The standalone map is NOT re-saved here: it is byte-identical to the one
# F01_figure1_mmw_smooth3_map_rebuilt167.R writes to
# Manuscript figures/fig1_inset_map_prydz_domain.{png,pdf}.
#
# REQUIRES sf, rnaturalearth and rnaturalearthdata in addition to the usual set.
#
# Writes Manuscript figures/fig1_mti_smooth3_map_rebuilt167.{png,pdf}
#        Manuscript data/fig1_mti_smooth3_map_series_rebuilt167.csv
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
LAT_LIM  <- -55                 # northern edge of the inset
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

# --- assigned trophic levels: McCormack et al. (2020) Prydz Bay Ecopath, Tab 2 -
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
# Identical to the mass variant; see its header for the densification, the
# projection and the extent rule.
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
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(CRS_SO)
LIM <- max(abs(st_bbox(
  st_transform(st_sfc(st_multipoint(cbind(seq(-180, 180, 0.5), LAT_LIM)),
                      crs = 4326), CRS_SO))))
cat("\n=== inset map ===\n")
cat(sprintf("  domain '%s': %d -> %d vertices; extent %g S -> +/-%.0f m in %s\n",
            DOM_ROW, nrow(st_coordinates(st_geometry(pb))),
            nrow(st_coordinates(pb_ll)), LAT_LIM, LIM, CRS_SO))

# Graticule breaks stay automatic ON PURPOSE: without coord_sf(default_crs = ..)
# scale_x/y_continuous(breaks = ..) are read as projected METRES, which silently
# replaces the latitude labels with the panel-corner value.
p_map <- ggplot() +
  geom_sf(data = world, fill = "grey85", colour = "grey45", linewidth = 0.15) +
  geom_sf(data = pb_p, fill = COL_DOM, colour = "#8C1122", alpha = 0.75,
          linewidth = 0.3) +
  coord_sf(xlim = c(-LIM, LIM), ylim = c(-LIM, LIM), crs = CRS_SO,
           expand = FALSE,
           label_axes = list(top = "E", right = "E", bottom = "E", left = "N")) +
  theme_bw(base_size = 7) +
  # No bounding box, and ticks go with it -- see the mass script's header.
  theme(panel.grid = element_line(colour = "grey80", linewidth = 0.2),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 5, colour = "grey25"),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(1, 1, 1, 1))

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
  # Species key as an inset box above the map. Three rows -- hence three columns
  # for nine groups -- keeps it about the map's width and clear of the "Krill
  # fishing starts" annotation, which ends at 0.585 npc.
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification.inside = c(1, 1),
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

pA_ins <- pA + inset_element(p_map, left = 0.67, bottom = 0.12,
                             right = 1.00, top = 0.86, align_to = "panel")

# ------------------------------------------------------------------ PANEL B ---
spp <- setdiff(names(obs_raw), "Year")
stopifnot(all(spp %in% names(TROPHIC_LEVELS)))
C <- vapply(obs_raw[spp], function(v) {
  x <- suppressWarnings(as.numeric(v)); x[is.na(x)] <- 0; pmax(x, 0)
}, numeric(nrow(obs_raw)))
num <- as.vector(C %*% TROPHIC_LEVELS[spp]); den <- rowSums(C)

# --- the 3-year centred rolling window ----------------------------------------
stopifnot(all(diff(obs_raw$Year) == 1))
roll_sum <- function(v, k = SMOOTH_K) {
  h <- (k - 1) / 2
  vapply(seq_along(v), function(i)
    sum(v[max(1, i - h):min(length(v), i + h)]), numeric(1))
}
# `den > 0`: a year with no catch of its own stays NA, window or no window, so
# the panel's gaps are exactly panel A's -- 1941-45 and 2005/2009/2011/2012.
MTI <- data.frame(Year = obs_raw$Year,
                  total_kt = den / 1e9,
                  mti      = ifelse(den > 0, num / den, NA),
                  mti_s3   = ifelse(den > 0, roll_sum(num) / roll_sum(den), NA)) %>%
  filter(Year >= X_LIM[1], Year <= X_LIM[2])

e1 <- MTI$Year <= 1970; e2 <- MTI$Year >= 1971 & MTI$Year <= 2010
sm <- function(v, sel) median(v[sel & is.finite(v)])
k  <- is.finite(MTI$mti_s3) & MTI$Year <= 2010
fit <- lm(mti_s3 ~ Year, data = MTI[k, ])   # LINEAR: one unit is one transfer
cat("\n=== mean trophic level, ", SMOOTH_K, "-year centred window ===\n", sep = "")
cat(sprintf("  1930-1970 median  %.3f\n", sm(MTI$mti_s3, e1)))
cat(sprintf("  1971-2010 median  %.3f  (%+.3f trophic units)\n",
            sm(MTI$mti_s3, e2), sm(MTI$mti_s3, e2) - sm(MTI$mti_s3, e1)))
cat(sprintf("  trend             %+.4f units per decade (p = %.2g, R2 = %.2f)\n",
            coef(fit)[2] * 10, summary(fit)$coefficients[2, 4], summary(fit)$r.squared))
cat(sprintf("  range             %.3f to %.3f\n",
            min(MTI$mti_s3, na.rm = TRUE), max(MTI$mti_s3, na.rm = TRUE)))

# --- species trophic-level reference lines ------------------------------------
REF_TAXA <- names(which(colSums(eff) > 0))
stopifnot(setequal(REF_TAXA, caught))
REF_TAXA <- REF_TAXA[order(TROPHIC_LEVELS[REF_TAXA], decreasing = TRUE)]
# Header note 3: the table from the full-range trophic build, unchanged.
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
stopifnot(setequal(names(REF_SIDE), REF_TAXA), setequal(names(REF_VJ), REF_TAXA))

ref_all <- data.frame(Species = REF_TAXA,
                      y = unname(TROPHIC_LEVELS[REF_TAXA]),
                      lab = pretty_lab(REF_TAXA),
                      stringsAsFactors = FALSE) %>%
  mutate(side = unname(REF_SIDE[Species]),
         x  = ifelse(side == "left", X_LIM[1] + 1, X_LIM[2] - 1),
         hj = ifelse(side == "left", 0, 1),
         vj = unname(REF_VJ[Species]))
stopifnot(!any(is.na(sp_cols[ref_all$Species])))

# Header note 2. Filtered from the data, not hand-listed, so this stays correct
# if y_lim changes.
y_lim <- c(3.2, 5.25)
ref <- ref_all %>% filter(y >= y_lim[1], y <= y_lim[2])
cat("\n=== panel B reference lines ===\n")
cat("  drawn  :", paste(ref$lab, collapse = ", "), "\n")
cat("  dropped (outside the tightened y axis):\n")
print(ref_all %>% filter(!Species %in% ref$Species) %>%
        transmute(Species, trophic_level = y), row.names = FALSE)

pB <- ggplot(MTI, aes(Year, mti_s3)) +
  geom_hline(data = ref, aes(yintercept = y, colour = Species),
             linetype = "dotted", linewidth = 0.5, alpha = 0.85,
             show.legend = FALSE, inherit.aes = FALSE) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  geom_text(data = ref, aes(x = x, y = y, label = lab, hjust = hj, vjust = vj,
                            colour = Species),
            size = 2.7, show.legend = FALSE, inherit.aes = FALSE) +
  geom_line(linewidth = 0.7, colour = "grey20", na.rm = TRUE) +
  geom_point(size = 1.5, colour = "grey10", na.rm = TRUE) +
  # No in-panel text: one series, and the window belongs in the caption.
  scale_colour_manual(values = sp_cols, guide = "none") +
  scale_x_continuous(breaks = X_BRK, limits = X_LIM) +
  scale_y_continuous(breaks = seq(3.5, 5.0, 0.5)) +
  coord_cartesian(ylim = y_lim) +
  labs(x = "Year", y = "Mean trophic level of catch", tag = "B") +
  theme_classic(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.tag = element_text(face = "bold", size = 14))

# No collected legend: the key lives inside panel A, and pB contributes none.
fig <- (pA_ins / pB) + plot_layout(heights = c(1, 0.7))
png_out <- guard(file.path(FIGS, sprintf("fig1_mti_smooth3_map_%s.png", SUF)))
pdf_out <- guard(file.path(FIGS, sprintf("fig1_mti_smooth3_map_%s.pdf", SUF)))
ggsave(png_out, fig, width = 11, height = 9.6, dpi = 300, limitsize = FALSE)
ggsave(pdf_out, fig, width = 11, height = 9.6, limitsize = FALSE)
write.csv(MTI, guard(file.path(DATA,
          sprintf("fig1_mti_smooth3_map_series_%s.csv", SUF))), row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")