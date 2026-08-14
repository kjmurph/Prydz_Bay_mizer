# =============================================================================
# FIGURE 1 -- refined 183-member fitted ensemble.
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
# lambda keeps its canonical LBNbiom value; only the INTERCEPT of the display
# line is refitted, because octave NBS and native density use different
# normalisations and the octave intercept does not transfer.
#
# The lambda line fits poorly and that is real, not a bug: the spectrum has a
# plateau below ~1e-2 g then a steeper decline, so no single slope describes 15.5
# decades. Both scenario lines are drawn and are near-coincident, which is why
# panel C exists.
#
# Writes Manuscript figures/fig1_refined183.{png,pdf}
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(scales); library(rphylopic); library(grid)
})

DATA <- "Manuscript data"; FIGS <- "Manuscript figures"
dir.create(FIGS, showWarnings = FALSE)
SUF <- "refined183"; REF <- 2001:2010
guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE); f
}
meta <- readRDS(file.path(DATA, sprintf("meta_%s.rds", SUF)))
message("members: ", meta$n_members, " | arm: ", meta$arm)

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

sp_mean <- function(a) apply(a, c(2, 3), mean)
df_sp <- bind_rows(lapply(seq_along(SP$sp_names), function(i) {
  v <- sp_mean(SP$species_fished)[i, ] * w
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

# refit only the INTERCEPT, holding the canonical slope (see header)
icept <- function(d, lambda) {
  mean(log10(d$y) - lambda * log10(d$w))
}
bf <- df_com %>% filter(arm == "Exploited"); bc <- df_com %>% filter(arm == "Unexploited")
ln <- bind_rows(
  data.frame(w = range(bf$w), arm = "Exploited",
             y = 10^(icept(bf, lam_f) + lam_f * log10(range(bf$w)))),
  data.frame(w = range(bc$w), arm = "Unexploited",
             y = 10^(icept(bc, lam_c) + lam_c * log10(range(bc$w)))))

# Scenario colours are shared with panel C so the two panels read as one figure:
# exploited red, unexploited grey. Previously panel B distinguished the scenarios
# by line type only, against a single dark-red lambda line, which did not match
# panel C's colour coding.
ARM_COLS <- c(Exploited = "#C0392B", Unexploited = "grey35")

# --- silhouettes, anchored at the LEFT-HAND END of each group's spectrum -------
# The cached objects are rphylopic images fetched by
# R/figures/fig1/00_provision_phylopic.R. Only 14 of the 19 groups have one; the
# rest are simply skipped rather than substituted with a different taxon.
PHYLOPIC_CACHE <- "R/figures/fig1/phylopic_cache"
sil_key <- function(nm) gsub("[^a-z0-9]+", "_", tolower(nm))
anchors <- do.call(rbind, lapply(unique(df_sp$Species), function(nm) {
  f <- file.path(PHYLOPIC_CACHE, paste0(sil_key(nm), ".rds"))
  if (!file.exists(f)) return(NULL)
  d <- df_sp[df_sp$Species == nm, ]
  d <- d[order(d$w), ]
  data.frame(Species = nm, x = d$w[1], y = d$y[1],
             colour = unname(sp_cols[nm]), stringsAsFactors = FALSE)
}))
if (!is.null(anchors)) {
  # the cached RDS is list(uuid, img, credit) -- geom_phylopic needs the $img
  # element, not the wrapper
  anchors$img <- I(lapply(anchors$Species, function(nm)
    readRDS(file.path(PHYLOPIC_CACHE, paste0(sil_key(nm), ".rds")))$img))
  # nudge left of the line start so the silhouette labels the origin rather than
  # sitting on the data; ~0.8 decades clears its own width at these heights
  anchors$x <- anchors$x * 10^-0.85
  message("silhouettes placed: ", nrow(anchors), " of ", n_distinct(df_sp$Species))
}

pB <- ggplot() +
  annotate("rect", xmin = 5e5, xmax = 1.2e8, ymin = min(df_com$y), ymax = Inf,
           fill = "#CFE3F2", alpha = 0.55) +
  annotate("text", x = 6.5e6, y = Inf, label = "Large marine\nmammals",
           vjust = 1.4, size = 3.1, colour = "grey25") +
  geom_line(data = df_sp, aes(w, y, colour = Species), linewidth = 0.95,
            alpha = 0.9, show.legend = FALSE) +
  geom_line(data = ln, aes(w, y, colour = arm), linewidth = 0.6,
            linetype = "solid", show.legend = FALSE) +
  geom_line(data = df_com, aes(w, y, colour = arm, linetype = arm),
            linewidth = 0.85) +
  scale_colour_manual(values = c(sp_cols, ARM_COLS), name = NULL,
                      breaks = names(ARM_COLS)) +
  scale_linetype_manual(values = c(Exploited = "dashed", Unexploited = "solid"),
                        name = NULL) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)),
                breaks = 10^seq(-8, 8, 2)) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "b", size = 0.25) +
  labs(x = "Body mass (g)",
       y = expression(log[10]~"biomass density (g g"^-1*")"), tag = "B") +
  annotate("text", x = 1e-7, y = min(df_com$y) * 3, hjust = 0, size = 3.2,
           colour = ARM_COLS[["Exploited"]], fontface = "bold",
           label = sprintf("lambda[exploited] == %.3f", lam_f), parse = TRUE) +
  annotate("text", x = 1e-7, y = min(df_com$y), hjust = 0, size = 3.2,
           colour = ARM_COLS[["Unexploited"]], fontface = "bold",
           label = sprintf("lambda[unexploited] == %.3f", lam_c), parse = TRUE) +
  theme_classic(base_size = 11) +
  # bottom-right: top-right collides with the "Large marine mammals" label,
  # bottom-left with the lambda annotations
  theme(panel.grid = element_blank(), legend.position = c(0.86, 0.13),
        legend.background = element_blank(), legend.key.width = unit(1.4, "lines"),
        plot.tag = element_text(face = "bold", size = 14)) +
  guides(colour = guide_legend(override.aes = list(linewidth = 1)))

if (!is.null(anchors) && nrow(anchors)) {
  for (i in seq_len(nrow(anchors))) {
    pB <- pB + rphylopic::geom_phylopic(
      data = anchors[i, , drop = FALSE], aes(x = x, y = y, img = img),
      fill = anchors$colour[i], height = 0.30, alpha = 1, inherit.aes = FALSE)
  }
}

# ------------------------------------------------------------------ PANEL C ---
S <- sl %>% group_by(arm, Year) %>%
  summarise(med = median(slope, na.rm = TRUE),
            lo50 = quantile(slope, .25, na.rm = TRUE),
            hi50 = quantile(slope, .75, na.rm = TRUE),
            lo90 = quantile(slope, .05, na.rm = TRUE),
            hi90 = quantile(slope, .95, na.rm = TRUE), .groups = "drop") %>%
  mutate(Arm = ifelse(arm == "exploited", "Exploited", "Unexploited"))

# --- one representative within-pair member, drawn in green ---------------------
# The ensemble ribbons show the SPREAD ACROSS members, which is much wider than
# the within-pair difference and so visually buries the actual effect. Tracing a
# single member's two trajectories makes the paired quantity legible: the gap
# between the two green lines IS the signal, and it is far narrower than the
# ribbons around it. The member shown is the one with the largest |E - U| at
# 2010, i.e. the clearest illustration rather than a typical one -- stated in the
# caption so it is not mistaken for the ensemble median.
pair_gap <- sl %>%
  select(sim_index, arm, Year, slope) %>%
  pivot_wider(names_from = arm, values_from = slope) %>%
  filter(Year == 2010) %>%
  mutate(gap = exploited - unexploited) %>%
  arrange(gap)
hero <- pair_gap$sim_index[1]
hero_gap <- pair_gap$gap[1]
message("highlighted member: ", hero, " (E-U at 2010 = ",
        signif(hero_gap, 4), ")")
HERO <- sl %>% filter(sim_index == hero) %>%
  mutate(Arm = ifelse(arm == "exploited", "Exploited", "Unexploited"))

# --- ecological deficit bracket ------------------------------------------------
# The vertical gap between the two ensemble medians at the end of the record.
e2010 <- S$med[S$Arm == "Exploited"   & S$Year == 2010]
u2010 <- S$med[S$Arm == "Unexploited" & S$Year == 2010]
x_def <- 2013

pC1 <- ggplot(S, aes(Year, med, colour = Arm, fill = Arm)) +
  geom_ribbon(aes(ymin = lo90, ymax = hi90), alpha = 0.18, colour = NA) +
  geom_ribbon(aes(ymin = lo50, ymax = hi50), alpha = 0.32, colour = NA) +
  geom_line(aes(linetype = Arm), linewidth = 0.7) +
  geom_line(data = HERO, aes(Year, slope, group = Arm, linetype = Arm),
            colour = "#1B7F4B", linewidth = 0.55, inherit.aes = FALSE) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  annotate("text", x = whal, y = Inf, label = "Whaling\nstarts", hjust = 1.07,
           vjust = 1.3, size = 2.9, colour = "grey35") +
  annotate("text", x = krl, y = Inf, label = "Krill\nfishing starts", hjust = 1.07,
           vjust = 1.3, size = 2.9, colour = "grey35") +
  # ecological deficit: double-headed arrow spanning the two medians at 2010
  annotate("segment", x = x_def, xend = x_def, y = u2010, yend = e2010,
           arrow = arrow(ends = "both", length = unit(0.055, "inches"),
                         type = "closed"),
           colour = "grey20", linewidth = 0.45) +
  annotate("text", x = x_def + 1.5, y = (u2010 + e2010) / 2,
           label = "Ecological\ndeficit", hjust = 0, size = 3.1,
           colour = "grey20", lineheight = 0.95) +
  annotate("text", x = 1852, y = min(S$lo90, na.rm = TRUE),
           label = sprintf("green: member %d (largest within-pair difference)", hero),
           hjust = 0, size = 2.7, colour = "#1B7F4B") +
  scale_colour_manual(values = c(Exploited = "#C0392B", Unexploited = "grey35")) +
  scale_fill_manual(values = c(Exploited = "#C0392B", Unexploited = "grey55")) +
  scale_linetype_manual(values = c(Exploited = "solid", Unexploited = "dotted")) +
  coord_cartesian(xlim = c(1841, 2010), clip = "off") +
  labs(x = NULL, y = expression("Size-spectrum slope "*lambda), tag = "C") +
  theme_classic(base_size = 11) +
  theme(panel.grid = element_blank(), legend.position = c(0.14, 0.22),
        legend.title = element_blank(), legend.background = element_blank(),
        plot.margin = margin(6, 62, 6, 6),
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

fig <- (pA / (pB | pC)) + plot_layout(heights = c(1, 1.25))
png_out <- guard(file.path(FIGS, sprintf("fig1_%s.png", SUF)))
pdf_out <- guard(file.path(FIGS, sprintf("fig1_%s.pdf", SUF)))
ggsave(png_out, fig, width = 15, height = 15, dpi = 300, limitsize = FALSE)
ggsave(pdf_out, fig, width = 15, height = 15, limitsize = FALSE)
write.csv(S, guard(file.path(DATA, sprintf("fig1_slope_series_%s.csv", SUF))),
          row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")
