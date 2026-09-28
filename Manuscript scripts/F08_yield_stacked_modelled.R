# =============================================================================
# F08 -- MODELLED yield, stacked by species, with the observed yield, the
# species key and the Prydz Bay map as insets (drawn for Science, matching
# Figure 1).
#
# The p104 successor of yield_stacked_<cut>.png from R/wmin_test/47_plot_cut_yields.R
# (last drawn for "E NRMSE_sd", n = 167 on ensemble 44). THE STATISTICS ARE 47's:
#   - the stack is the across-member MEDIAN of each species' yield, per year
#   - bars at peak years are the IQR, across members, of each member's TOTAL
#     yield, centred on the stacked medians' sum (find_peaks(): the max within
#     +/-PEAK_WINDOW years and >= MIN_PEAK_KT)
# THE DRAWING IS FIGURE 1's (F01_figure1_mmwgeo_mti_linear_map_rebuilt167_insetkey.R):
# its species colours, names and stack order (by total observed catch), its
# in-panel key (no outline, 7 pt, 0.32 cm keys, two columns), its map inset
# (transcribed below), its x axis (1929-2010, 10-yr ticks at 45 deg), its type
# sizes, printed width 180 mm, and "thousand tonnes" on the y axis.
#
# INPUT. Per-member, per-species yield from R/wmin_test/80_yield_by_species.R
# for phase 104 at the fitted-whale, q <= 10 catchability. THAT FILE CARRIES
# 265 MEMBERS, the pre-drift usable definition (see p104 figure pipeline notes)
# -- so it is filtered here to the ranking's "FULL usable" cut, 203 members,
# or to its TOP cut with FIG_SET=top.
#
# THE MODELLED CATCH IS FRONT-LOADED, and this figure shows it rather than hides
# it: with whale q fitted, the first whaling years take the unexploited stock
# (1931: median stack 1,524 kt vs 335 kt observed; IQR of the total 534-2,641
# kt), and the 1950s-60s fall 5-9x below observed. Uncapped, the 1931 bar sets
# the y axis and squashes the rest; by default the axis is CAPPED at 600 kt
# (F08_YCAP) and the off-scale bar is arrowed, with its values above the panel.
#
# Canonical invocation (FIG_SUPP -> Manuscript figures/p104 figures/Supplemental figures):
#   Rscript run_p104q10.R "Manuscript scripts/F08_yield_stacked_modelled.R"
# The runner's --top does NOT reach this script (no per-script block); set
# FIG_SET=top in the environment for the top cut. F08_YCAP=0 for no cap.
#
# Writes FIG_SUPP/yield_stacked_<SUF>[_top][_ycap<cap>].{png,pdf}
#        Manuscript data/yield_stacked_<SUF>[_top]_series.csv  (shared by the
#        capped and uncapped drawings; verified byte-identical if present)
# guard() refuses to overwrite a figure.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(scales); library(grid); library(sf); library(rnaturalearth)
})

DATA <- "Manuscript data"
FIGS <- Sys.getenv("FIG_SUPP", file.path("Manuscript figures", "Supplemental figures"))
dir.create(FIGS, recursive = TRUE, showWarnings = FALSE)
SUF    <- Sys.getenv("FIG_SUF", "p104q10")
YIELD  <- Sys.getenv("F08_YIELD", "Output_large_files/wmin_test/80_yield_by_species_p104q10.rds")
RANK   <- Sys.getenv("F0_RANK",   "Output_large_files/wmin_test/104_q10_rerank.rds")
source("Manuscript scripts/F00z_member_set.R")
guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE); f
}

X_LIM <- c(1929, 2010)          # Figure 1's
X_BRK <- seq(1930, 2010, 10)
PEAK_WINDOW <- 3                # 47's
MIN_PEAK_KT <- 10
CRS_SO   <- "EPSG:3031"
DOM_SHP  <- "model_domains/FishMIP_regional_models/FishMIP_regional_models.shp"
DOM_ROW  <- "Prydz Bay"
COL_DOM  <- "#D7263D"

# --- output size and typography (Figure 1's values) ----------------------------
FIG_W  <- 180 / 25.4   # in
FIG_H  <- 115 / 25.4   # in
BASE   <- 9            # pt -- axis text
TITLE  <- 10           # pt -- axis titles
LEG    <- 7            # pt -- key entries
KEY_SZ <- 0.32         # cm -- key squares
INS_PT <- 7            # pt -- observed inset: title and tick labels
MAP_W  <- 56           # mm -- map width, as Figure 1; capped to the key width
GAP    <- 1.5          # mm -- between key, map and observed inset
OBS_X0 <- 1937         # observed inset's left edge (year), clear of the 1931 bar
OBS_H  <- 52           # mm -- observed inset height
# Y-AXIS CAP (kt), Kieran 2026-09-26. The front-loaded 1931 bar (IQR to ~2.6 Mt)
# otherwise sets the axis and squashes everything after 1936 into the bottom 7%.
# Capped near the observed peak (500 kt), the modelled series is readable and on
# the observed inset's scale. A peak whose bar runs past the cap is drawn to the
# panel edge with an arrowhead and labelled with its values -- the data are cut
# off, not hidden. F08_YCAP=0 draws the full axis. Capped outputs carry
# "_ycap<cap>" in their names.
Y_CAP <- as.numeric(Sys.getenv("F08_YCAP", "600"))
if (!is.finite(Y_CAP) || Y_CAP < 0) stop("F08_YCAP must be >= 0 (0 = no cap)",
                                         call. = FALSE)
CAPPED <- Y_CAP > 0

th_pub <- function() {
  theme_classic(base_size = BASE) +
    theme(panel.grid  = element_blank(),
          axis.text   = element_text(size = BASE, colour = "grey20"),
          axis.title  = element_text(size = TITLE),
          plot.margin = margin(2, 4, 2, 2))
}

# --- Figure 1's species colours and names ---------------------------------------
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

# --- observed yield: Figure 1 panel A's construction ---------------------------
obs_raw <- read.csv("yield_observed_timeseries.csv", check.names = FALSE)
obs_all <- obs_raw %>%
  pivot_longer(-Year, names_to = "Species", values_to = "g") %>%
  mutate(kt = pmax(coalesce(suppressWarnings(as.numeric(g)), 0), 0) / 1e9) %>%
  filter(Year >= X_LIM[1], Year <= X_LIM[2])
caught <- obs_all %>% group_by(Species) %>% summarise(s = sum(kt), .groups = "drop") %>%
  filter(s > 0) %>% pull(Species)
# Stack and key order = total observed catch, descending (Figure 1's `lev`), so
# the two figures key identically.
sp_order <- obs_all %>% filter(Species %in% caught) %>% group_by(Species) %>%
  summarise(s = sum(kt), .groups = "drop") %>% arrange(desc(s)) %>% pull(Species)
lev   <- pretty_lab(sp_order)
acols <- setNames(unname(sp_cols[sp_order]), lev)
stopifnot(!anyNA(acols), !anyDuplicated(acols))
obs <- obs_all %>% filter(Species %in% caught) %>%
  mutate(lab = factor(pretty_lab(Species), levels = lev))

# --- modelled yield, filtered to the usable members ------------------------------
Y <- readRDS(YIELD)
QMAX_ENV <- Sys.getenv("F0_QMAX", "")
if (nzchar(QMAX_ENV) && !isTRUE(all.equal(as.numeric(QMAX_ENV), Y$meta$qmax)))
  stop("F0_QMAX ", QMAX_ENV, " but ", basename(YIELD), " was extracted at qmax = ",
       Y$meta$qmax, call. = FALSE)
RR <- readRDS(RANK)
keep <- fig_top_intersect(as.integer(RR$cuts[["FULL usable"]]), rank_file = RANK)
miss <- setdiff(keep, unique(Y$raw$sim_index))
if (length(miss)) stop(length(miss), " selected members are absent from ",
                       basename(YIELD), call. = FALSE)
d <- Y$raw %>%
  filter(sim_index %in% keep, Year >= X_LIM[1], Year <= X_LIM[2],
         Species %in% caught) %>%
  mutate(kt = pmax(yield_t, 0) / 1e3)
N_MEM <- length(unique(d$sim_index))
message("members plotted: ", N_MEM, " (extraction carries ",
        length(unique(Y$raw$sim_index)), ")")

# 47 zeroed yields outside each species' effort window. With zero effort the
# model's yield is already exactly zero, so this is asserted, not applied.
eff <- readRDS("effort_array_1841_2010.rds"); yv <- as.numeric(rownames(eff))
win <- sapply(caught, function(s) range(yv[eff[, s] > 0]))
outside <- d %>% filter(Year < win[1, Species] | Year > win[2, Species], kt > 0)
if (nrow(outside)) stop(nrow(outside), " non-zero yields outside effort windows",
                        call. = FALSE)

stack_df <- d %>% group_by(Year, Species) %>%
  summarise(med = median(kt), .groups = "drop") %>%
  mutate(lab = factor(pretty_lab(Species), levels = lev))
totals <- stack_df %>% group_by(Year) %>%
  summarise(centre = sum(med), .groups = "drop") %>% arrange(Year)

find_peaks <- function(total, years, window = PEAK_WINDOW, min_kt = MIN_PEAK_KT) {
  n <- length(total); is_peak <- logical(n)
  for (i in seq_len(n)) {
    lo <- max(1L, i - window); hi <- min(n, i + window)
    if (total[i] == max(total[lo:hi]) && total[i] >= min_kt) is_peak[i] <- TRUE
  }
  years[is_peak]
}
pk <- find_peaks(totals$centre, totals$Year)
ci_df <- d %>% group_by(sim_index, Year) %>% summarise(t = sum(kt), .groups = "drop") %>%
  filter(Year %in% pk) %>% group_by(Year) %>%
  summarise(lo25 = quantile(t, .25), hi75 = quantile(t, .75), .groups = "drop") %>%
  left_join(totals, by = "Year")
# Split the peaks at the cap: a bar that stays inside is drawn as before; one
# that runs past it is drawn to the panel edge, arrowed, and labelled.
ci_df <- ci_df %>% mutate(off = CAPPED & hi75 > Y_CAP)
ci_in  <- ci_df %>% filter(!off)
ci_off <- ci_df %>% filter(off) %>%
  mutate(label = sprintf("%d: %s kt, IQR %s–%s", Year, comma(round(centre)),
                         comma(round(lo25)), comma(round(hi75))))
# The values go ABOVE the panel, as a subtitle over the arrow: inside it, the
# stack under the label differs by build (the top-20 1933 stack reaches ~570 kt,
# through a label drawn there), so no in-panel spot is clear for every build.
SUBT <- if (nrow(ci_off)) paste(ci_off$label, collapse = ";  ") else NULL
if (CAPPED) {
  over <- totals %>% filter(centre > Y_CAP, !Year %in% ci_off$Year)
  if (nrow(over))
    warning("stacked total exceeds the cap in unlabelled year(s): ",
            paste(over$Year, collapse = ", "), call. = FALSE)
}
cat("\n=== peak years: stacked-median total and IQR of member totals (kt) ===\n")
print(as.data.frame(ci_df %>% left_join(
  obs %>% group_by(Year) %>% summarise(observed = sum(kt), .groups = "drop"),
  by = "Year")), digits = 4, row.names = FALSE)

# Modelled / observed total catch over the window, median across members (47's).
ratio <- d %>% group_by(sim_index, Species) %>% summarise(m = sum(kt), .groups = "drop") %>%
  inner_join(obs %>% group_by(Species) %>% summarise(o = sum(kt), .groups = "drop"),
             by = "Species") %>%
  group_by(Species) %>% summarise(median_ratio = median(m / o), .groups = "drop")
cat("\n=== modelled / observed total catch", X_LIM[1], "-", X_LIM[2],
    "(median across members) ===\n")
print(as.data.frame(ratio), digits = 3, row.names = FALSE)

# =============================================================================
# DRAWING
# =============================================================================
x_sc <- scale_x_continuous(breaks = X_BRK, limits = X_LIM, expand = c(0, 0))

p_main <- ggplot(stack_df, aes(Year, med, fill = lab)) +
  geom_area(colour = NA) +
  # 47's peak bars, WITHOUT 47's white halo: the 1931 spike is one year wide,
  # so above ~1,400 kt it is narrower than the halo, which hid its tip and left
  # the median point floating above the area.
  geom_errorbar(data = ci_in, aes(x = Year, ymin = lo25, ymax = hi75),
                inherit.aes = FALSE, width = 1.0, linewidth = 0.4, colour = "grey20") +
  geom_point(data = ci_df %>% filter(!CAPPED | centre <= Y_CAP),
             aes(x = Year, y = centre), inherit.aes = FALSE,
             shape = 21, size = 1.6, fill = "white", colour = "grey20", stroke = 0.5) +
  # Off-scale bars: from the lower quartile to the cap, arrowed; a tick marks the
  # lower quartile when it is on the panel. When the whole bar is above the cap
  # (the top-20 1931 lower quartile is 763 kt) the arrow still spans the top 6%
  # of the axis -- a zero-length segment draws its head as a blob. The values
  # are the subtitle, directly above.
  {if (nrow(ci_off))
    geom_segment(data = ci_off, aes(x = Year, xend = Year,
                                    y = pmin(lo25, 0.94 * Y_CAP), yend = Y_CAP),
                 inherit.aes = FALSE, linewidth = 0.4, colour = "grey20",
                 arrow = arrow(length = unit(1.5, "mm"), angle = 25, type = "closed"))} +
  {if (nrow(ci_off))
    geom_segment(data = ci_off %>% filter(lo25 < Y_CAP),
                 aes(x = Year - 0.5, xend = Year + 0.5, y = lo25, yend = lo25),
                 inherit.aes = FALSE, linewidth = 0.4, colour = "grey20")} +
  scale_fill_manual(values = acols, name = NULL) +
  x_sc +
  scale_y_continuous(labels = label_comma(),
                     breaks = if (CAPPED) seq(0, Y_CAP, 100) else waiver(),
                     expand = expansion(mult = c(0, 0.03))) +
  # The cap is a COORD limit, so nothing is dropped from the stack: the areas and
  # the bar are clipped at the panel edge. expand = FALSE puts the edge exactly
  # at the cap, where the arrowheads end.
  {if (CAPPED) coord_cartesian(ylim = c(0, Y_CAP), expand = FALSE)} +
  labs(x = "Year", y = expression("Modelled yield (thousand tonnes y"^-1*")"),
       subtitle = SUBT) +
  th_pub() +
  theme(plot.title.position = "panel",       # subtitle starts at the panel's left
        plot.subtitle = element_text(size = LEG, colour = "grey20", hjust = 0,
                                     margin = margin(b = 2))) +
  # Figure 1's key: inside the panel, top right, white, no outline.
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification.inside = c(1, 1),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA),
        legend.key.size = unit(KEY_SZ, "cm"),
        legend.key.spacing.x = unit(6, "pt"),
        legend.key.spacing.y = unit(0.5, "pt"),
        legend.text = element_text(size = LEG),
        legend.margin = margin(2, 2, 2, 2)) +
  guides(fill = guide_legend(ncol = 2, order = 1))

# --- observed inset: Figure 1 panel A's stack, small -----------------------------
p_obs <- ggplot(obs, aes(Year, kt, fill = lab)) +
  geom_area(colour = NA) +
  scale_fill_manual(values = acols, guide = "none") +
  scale_x_continuous(breaks = seq(1930, 2010, 20), limits = X_LIM, expand = c(0, 0)) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Observed yield", x = NULL, y = NULL) +
  theme_classic(base_size = INS_PT) +
  theme(axis.text  = element_text(size = INS_PT, colour = "grey20"),
        plot.title = element_text(size = INS_PT, face = "bold", margin = margin(b = 2)),
        plot.margin = margin(3, 6, 2, 3),
        plot.background = element_rect(fill = "white", colour = "grey70",
                                       linewidth = 0.3))

# --- map inset: transcribed from Figure 1 ----------------------------------------
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
MIN_ISLAND <- 2500     # km2
ant <- ne_countries(scale = "medium", continent = "Antarctica",
                    returnclass = "sf") %>%
  st_transform(CRS_SO) %>% st_geometry() %>% st_buffer(1) %>% st_union() %>%
  st_cast("POLYGON")
ant <- ant[as.numeric(st_area(ant)) >= MIN_ISLAND * 1e6]
ant <- st_sfc(lapply(ant, function(p) st_polygon(list(p[[1]]))), crs = CRS_SO)
MAP_PAD <- 0.02
bb   <- st_bbox(c(ant, pb_p))
pad  <- MAP_PAD * max(bb[["xmax"]] - bb[["xmin"]], bb[["ymax"]] - bb[["ymin"]])
M_XL <- c(bb[["xmin"]] - pad, bb[["xmax"]] + pad)
M_YL <- c(bb[["ymin"]] - pad, bb[["ymax"]] + pad)
MAP_ASP <- diff(M_XL) / diff(M_YL)
p_map <- ggplot() +
  geom_sf(data = ant, fill = "grey85", colour = "grey45", linewidth = 0.15) +
  geom_sf(data = pb_p, fill = COL_DOM, colour = "#8C1122", alpha = 0.75,
          linewidth = 0.3) +
  coord_sf(xlim = M_XL, ylim = M_YL, crs = CRS_SO, expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(0, 0, 0, 0))

# --- place the insets, in printed mm (Figure 1's method) --------------------------
key_mm <- function(p) {
  pdf(NULL); on.exit(dev.off())
  g  <- ggplotGrob(p)
  kb <- g$grobs[[which(g$layout$name == "guide-box-inside")]]
  c(w = convertWidth(sum(kb$widths), "mm", valueOnly = TRUE),
    h = convertHeight(sum(kb$heights), "mm", valueOnly = TRUE))
}
panel_mm <- function(p) {
  pdf(NULL, width = FIG_W, height = FIG_H); on.exit(dev.off())
  g <- ggplotGrob(p)
  pn <- g$layout[g$layout$name == "panel", ]
  w <- g$widths; h <- g$heights
  fixed_w <- sum(vapply(setdiff(seq_along(w), pn$l), function(i)
    convertWidth(w[i], "mm", valueOnly = TRUE), numeric(1)))
  fixed_h <- sum(vapply(setdiff(seq_along(h), pn$t), function(i)
    convertHeight(h[i], "mm", valueOnly = TRUE), numeric(1)))
  c(w = FIG_W * 25.4 - fixed_w, h = FIG_H * 25.4 - fixed_h)
}
KEY <- key_mm(p_main)
PAN <- panel_mm(p_main)
map_w <- min(MAP_W, KEY[["w"]])          # the map sits under the key, same width
map_h <- map_w / MAP_ASP
obs_left <- (OBS_X0 - X_LIM[1]) / diff(X_LIM)          # npc
obs_w    <- PAN[["w"]] * (1 - obs_left) - KEY[["w"]] - 2 * GAP
if (KEY[["h"]] + GAP + map_h > PAN[["h"]])
  warning("key + map are taller than the panel", call. = FALSE)
if (obs_w < 40) warning("observed inset is only ", round(obs_w), " mm wide", call. = FALSE)

fig <- p_main +
  inset_element(p_map,
                left   = unit(1, "npc") - unit(map_w, "mm"),
                bottom = unit(1, "npc") - unit(KEY[["h"]] + GAP + map_h, "mm"),
                right  = unit(1, "npc"),
                top    = unit(1, "npc") - unit(KEY[["h"]] + GAP, "mm"),
                align_to = "panel") +
  inset_element(p_obs,
                left   = unit(obs_left, "npc"),
                bottom = unit(1, "npc") - unit(OBS_H, "mm"),
                right  = unit(obs_left, "npc") + unit(obs_w, "mm"),
                top    = unit(1, "npc"),
                align_to = "panel")

set_tag <- if (identical(Sys.getenv("FIG_SET", "all"), "all")) "" else
  paste0("_", fig_set_tag())
DSTEM <- sprintf("yield_stacked_%s%s", SUF, set_tag)          # the data
STEM  <- paste0(DSTEM, if (CAPPED) sprintf("_ycap%g", Y_CAP) else "")  # the drawing
png_out <- guard(file.path(FIGS, paste0(STEM, ".png")))
pdf_out <- guard(file.path(FIGS, paste0(STEM, ".pdf")))
ggsave(png_out, fig, width = FIG_W, height = FIG_H, dpi = 600)
ggsave(pdf_out, fig, width = FIG_W, height = FIG_H)
# The series does not depend on the cap, so capped and uncapped builds share one
# CSV. If it exists it must be byte-identical to what this run would write;
# anything else is a changed input, and stops.
series <- stack_df %>% select(Year, Species, median_kt = med) %>%
  left_join(ci_df %>% select(Year, peak_total_kt = centre, lo25, hi75),
            by = "Year") %>% mutate(n_members = N_MEM)
csv_out <- file.path(DATA, paste0(DSTEM, "_series.csv"))
tmp <- tempfile(fileext = ".csv"); write.csv(series, tmp, row.names = FALSE)
if (!file.exists(csv_out)) {
  file.copy(tmp, csv_out)
} else if (!identical(unname(tools::md5sum(tmp)), unname(tools::md5sum(csv_out)))) {
  stop(csv_out, " exists and differs from this run's series -- refusing to ",
       "overwrite", call. = FALSE)
} else message("series CSV unchanged: ", csv_out)

cat(sprintf("\npanel %.0f x %.0f mm | key %.1f x %.1f mm | map %.1f x %.1f mm | observed inset %.0f x %.0f mm\n",
            PAN[["w"]], PAN[["h"]], KEY[["w"]], KEY[["h"]], map_w, map_h, obs_w, OBS_H))
cat("Wrote:\n  ", png_out, "\n  ", pdf_out, "\n")
