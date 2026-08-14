# =============================================================================
# FIGURE 1 (GEOMETRIC MEAN MAXIMUM BODY MASS + MEAN TROPHIC LEVEL, MAP INSET)
# -- rebuilt 44, cut A, 167 members.
#
#   A  Observed catch, stacked by species, with the species key and a
#      circumpolar Southern Ocean inset showing the Prydz Bay model domain
#   B  GEOMETRIC mean maximum body mass of the observed catch, ANNUAL (no
#      smoothing), on a LINEAR axis
#   C  Mean trophic level of the observed catch, ANNUAL (no smoothing), linear
#      axis, carrying the dotted species reference lines and species labels
#
# This is a new figure built from the existing geometric MMW map figure and the
# MTI panel, without overwriting the source figures or their outputs.
#
# Writes Manuscript figures/fig1_mmwgeo_mti_linear_map_rebuilt167.{png,pdf}
#        Manuscript data/fig1_mmwgeo_mti_linear_map_series_rebuilt167.csv
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(scales); library(grid); library(sf); library(rnaturalearth)
})

DATA <- "Manuscript data"; FIGS <- "Manuscript figures"
dir.create(FIGS, showWarnings = FALSE)
SUF <- "rebuilt167"
X_LIM <- c(1930, 2010)
X_BRK <- seq(1930, 2010, 10)
CRS_SO   <- "EPSG:3031"
LAT_LIM  <- -55
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

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(CRS_SO)

LIM <- max(abs(st_bbox(
  st_transform(st_sfc(st_multipoint(cbind(seq(-180, 180, 0.5), LAT_LIM)),
                      crs = 4326), CRS_SO))))

p_map <- ggplot() +
  geom_sf(data = world, fill = "grey85", colour = "grey45", linewidth = 0.15) +
  geom_sf(data = pb_p, fill = COL_DOM, colour = "#8C1122", alpha = 0.75,
          linewidth = 0.3) +
  coord_sf(xlim = c(-LIM, LIM), ylim = c(-LIM, LIM), crs = CRS_SO,
           expand = FALSE,
           label_axes = list(top = "E", right = "E", bottom = "E", left = "N")) +
  theme_bw(base_size = 7) +
  theme(panel.grid = element_line(colour = "grey80", linewidth = 0.2),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 5, colour = "grey25"),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(1, 1, 1, 1))

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

pA <- ggplot(obs, aes(Year, t, fill = lab)) +
  geom_area(colour = NA) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  annotate("text", x = 1930.25, y = Inf, label = "Whaling\nstarts", hjust = 0,
           vjust = 1.3, size = 3, colour = "grey35") +
  annotate("text", x = krl, y = Inf, label = "Krill\nfishing starts", hjust = 1.08,
           vjust = 1.3, size = 3, colour = "grey35") +
  scale_fill_manual(values = acols, name = NULL) +
  scale_x_continuous(breaks = X_BRK, limits = X_LIM, expand = c(0, 0)) +
  labs(x = NULL, y = expression("Catch (10"^3~"t y"^-1*")"), tag = "A") +
  theme_classic(base_size = 11) +
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

pA_ins <- pA + inset_element(p_map, left = 0.58, bottom = 0.12,
                             right = 0.98, top = 0.86, align_to = "panel")

# ----------------------------------------------------------- PANEL B --------
# GEOMETRIC mean maximum body mass of catch (same construction as the source
# figure, but with no species reference lines / labels and the y-axis label
# shortened to omit the word "Geometric").
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

y_lim <- c(0, 108)

pB <- ggplot(MMW, aes(Year)) +
  geom_ribbon(aes(ymin = mmw_lo, ymax = mmw_hi), fill = "grey40", alpha = 0.18,
              na.rm = TRUE) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  geom_line(aes(y = mmw_t), linewidth = 0.7, colour = "grey20", na.rm = TRUE) +
  scale_x_continuous(breaks = X_BRK, limits = X_LIM, expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, 25), expand = c(0, 0)) +
  coord_cartesian(ylim = y_lim) +
  labs(x = "Year",
       y = expression("Mean maximum body mass of catch, "*w[max]*" (t)"),
       tag = "B") +
  theme_classic(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.tag = element_text(face = "bold", size = 14))

# ----------------------------------------------------------- PANEL C --------
# Mean trophic level panel from the MTI figure, with its dotted reference lines
# and labels retained, while the left-side labels are moved to the right of the
# 1930 reference line. This matches the requested x-axis adjustment.
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

REF_TAXA <- names(which(colSums(eff) > 0))
stopifnot(setequal(REF_TAXA, caught))
REF_TAXA <- REF_TAXA[order(TROPHIC_LEVELS[REF_TAXA], decreasing = TRUE)]
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
ref <- data.frame(Species = REF_TAXA,
                  y = unname(TROPHIC_LEVELS[REF_TAXA]),
                  lab = pretty_lab(REF_TAXA),
                  stringsAsFactors = FALSE) %>%
  mutate(side = unname(REF_SIDE[Species]),
         x  = ifelse(side == "left", 1932, X_LIM[2] - 1),
         hj = ifelse(side == "left", 0, 1),
         vj = unname(REF_VJ[Species])) %>%
  filter(Species %in% names(TROPHIC_LEVELS))

pC <- ggplot(MTI, aes(Year)) +
  geom_ribbon(aes(ymin = mti_lo, ymax = mti_hi), fill = "grey40", alpha = 0.18,
              na.rm = TRUE) +
  geom_hline(data = ref, aes(yintercept = y, colour = Species),
             linetype = "dotted", linewidth = 0.5, alpha = 0.85,
             show.legend = FALSE) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  geom_text(data = ref, aes(x = x, y = y, label = lab, hjust = hj, vjust = vj,
                            colour = Species),
            size = 2.6, show.legend = FALSE, inherit.aes = FALSE) +
  geom_line(aes(y = mti), linewidth = 0.55, colour = "grey30", na.rm = TRUE) +
  scale_colour_manual(values = sp_cols, guide = "none") +
  scale_x_continuous(breaks = X_BRK, limits = X_LIM, expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(2.5, 5.5, 0.5), expand = c(0, 0)) +
  coord_cartesian(ylim = c(2.30, 5.45)) +
  labs(x = "Year", y = "Mean trophic level of catch", tag = "C") +
  theme_classic(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.tag = element_text(face = "bold", size = 14))

fig <- (pA_ins / pB / pC) + plot_layout(heights = c(1, 0.56, 0.56))

png_out <- guard(file.path(FIGS, "fig1_mmwgeo_mti_linear_map_rebuilt167_sd_ribbon_trimmed1930.png"))
pdf_out <- guard(file.path(FIGS, "fig1_mmwgeo_mti_linear_map_rebuilt167_sd_ribbon_trimmed1930.pdf"))
csv_out <- guard(file.path(DATA, "fig1_mmwgeo_mti_linear_map_series_rebuilt167_sd_ribbon_trimmed1930.csv"))

ggsave(png_out, fig, width = 11, height = 12.5, dpi = 300, limitsize = FALSE)
ggsave(pdf_out, fig, width = 11, height = 12.5, limitsize = FALSE)
write.csv(MMW, csv_out, row.names = FALSE)

cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n  ", csv_out, "\n")
