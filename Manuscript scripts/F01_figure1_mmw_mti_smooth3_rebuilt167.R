# =============================================================================
# FIGURE 1 (DUAL-INDICATOR, 3-YEAR SMOOTHED) -- rebuilt 44, cut A, 167 members.
#
#   A  Observed catch, stacked by species          (identical to F01_figure1_mmw)
#   B  Both indicators of the same catch under a 3-year centred rolling window,
#      each with its unsmoothed series drawn faintly behind it:
#        left  axis, log10   mean maximum body mass  (t)      -- grey, circles
#        right axis, linear  mean trophic level               -- blue, triangles
#
# Sibling of F01_figure1_mmw_mti_rebuilt167.R; read that header for the dual-axis
# anchoring, which is unchanged and is the one thing here that is a convention
# rather than a measurement. Read F01_figure1_mmw_smooth3_rebuilt167.R's header
# for the smoothing. In brief: the window is applied to the CATCH, not to either
# indicator, so each year enters weighted by the tonnage it carries and the three
# near-zero-catch years (1971, 1988, 1993) stop dominating; a year with no catch
# of its own stays blank rather than borrowing a value from its neighbours.
#
# THIS IS THE VARIANT WHERE THE COMPARISON IS CLEANEST, because the excursions
# that the window removes were never a property of either indicator -- they were
# years in which both were barely determined, and they were the main thing
# distracting from the contrast the panel exists to show. After smoothing:
#
#     mean maximum body mass   97.1 t -> 5.78 t     16.8-fold, 1.23 log10 units
#     mean trophic level        3.99  -> 3.86       0.136 trophic units
#
#     as a fraction of the range the nine exploited groups span on each metric:
#     mass 16.6%, trophic 4.6%
#
# The mass trend also stops being noise-dominated: R2 goes from 0.34 on the raw
# series to 0.73 on the smoothed one, at essentially the same slope (-0.285 to
# -0.239 log10 units per decade). The trophic trend stays weak on both.
#
# ONE NUMBER MOVES ENOUGH TO MATTER. The trophic era shift doubles under
# smoothing, -0.065 to -0.136 units, because the raw 1971-2010 median sits on a
# run of minke-only years at exactly TL 3.955 while the pooled window mixes each
# with its krill-bearing neighbours. Quote the raw figure for the annual record
# and the smoothed figure for the smoothed one; do not quote either without
# saying which. The mass figures are insensitive to the choice (16.5x vs 16.8x).
#
# Writes Manuscript figures/fig1_mmw_mti_smooth3_rebuilt167.{png,pdf}
#        Manuscript data/fig1_mmw_mti_smooth3_series_rebuilt167.csv
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
SMOOTH_K        <- 3            # centred window width, years
SMOOTH_ON_RATIO <- FALSE        # TRUE = rolling mean of the indicators instead
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
# Fixed per group, no size or time dependence. Transcribed from
# ecosystem_assessment_v3.R:219-230 rather than sourced (that file runs a full
# assessment on load); the model's own trophic levels are deliberately not used.
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
num_w <- as.vector(C %*% wmax[spp])
num_t <- as.vector(C %*% TROPHIC_LEVELS[spp])
den   <- rowSums(C)

# --- the 3-year centred rolling window ----------------------------------------
# Positional window, so a calendar window only on a contiguous year grid --
# assert rather than assume. Taken on the full csv (1930-2019) before the
# 1920-2012 filter; the one partial window, 1930's, is exact because the record
# begins when whaling did.
stopifnot(all(diff(obs_raw$Year) == 1))
roll_sum <- function(v, k = SMOOTH_K) {
  h <- (k - 1) / 2
  vapply(seq_along(v), function(i)
    sum(v[max(1, i - h):min(length(v), i + h)]), numeric(1))
}
roll_mean_defined <- function(v, k = SMOOTH_K) {   # the rejected form
  h <- (k - 1) / 2
  vapply(seq_along(v), function(i) {
    w <- v[max(1, i - h):min(length(v), i + h)]
    if (all(is.na(w))) NA_real_ else mean(w, na.rm = TRUE)
  }, numeric(1))
}
# `den > 0`: a year with no catch of its own stays NA, window or no window, so
# both series break exactly where panel A does -- 1941-45, 2005/2009/2011/2012.
raw_w <- ifelse(den > 0, num_w / den, NA) / 1e6
raw_t <- ifelse(den > 0, num_t / den, NA)
pool_w <- ifelse(den > 0, roll_sum(num_w) / roll_sum(den), NA) / 1e6
pool_t <- ifelse(den > 0, roll_sum(num_t) / roll_sum(den), NA)
rat_w  <- ifelse(den > 0, roll_mean_defined(raw_w), NA)
rat_t  <- ifelse(den > 0, roll_mean_defined(raw_t), NA)

IND <- data.frame(Year = obs_raw$Year,
                  total_kt = den / 1e9,
                  mmw_t    = raw_w,
                  mti      = raw_t,
                  mmw_t_s3 = if (SMOOTH_ON_RATIO) rat_w else pool_w,
                  mti_s3   = if (SMOOTH_ON_RATIO) rat_t else pool_t,
                  mmw_t_s3_ratioform = rat_w,
                  mti_s3_ratioform   = rat_t) %>%
  filter(Year >= X_LIM[1], Year <= X_LIM[2])

# --- the affine map between the two axes --------------------------------------
# Unchanged from the unsmoothed dual build, and deliberately so: it is derived
# from the exploited species pool, not from the data, so smoothing the data must
# not move it. The two figures are therefore on identical axes.
REF_TAXA <- names(which(colSums(eff) > 0))
stopifnot(setequal(REF_TAXA, caught))
w_rng  <- log10(range(wmax[REF_TAXA] / 1e6))       # krill .. large baleen whale
tl_rng <- range(TROPHIC_LEVELS[REF_TAXA])          # krill .. sperm whale
MASS_PER_TL <- diff(w_rng) / diff(tl_rng)
MASS_AT_TL0 <- w_rng[1] - MASS_PER_TL * tl_rng[1]
tl2mass <- function(tl) 10^(MASS_AT_TL0 + MASS_PER_TL * tl)
mass2tl <- function(m)  (log10(m) - MASS_AT_TL0) / MASS_PER_TL
stopifnot(all.equal(mass2tl(tl2mass(tl_rng)), tl_rng))

cat("\n=== dual-axis anchoring (unchanged by smoothing) ===\n")
cat(sprintf("  map: log10(t) = %.4f + %.4f * TL\n", MASS_AT_TL0, MASS_PER_TL))

# --- summary statistics quoted in the manuscript text -------------------------
e1 <- IND$Year <= 1970; e2 <- IND$Year >= 1971 & IND$Year <= 2010
sm <- function(v, sel) median(v[sel & is.finite(v)])
say_w <- function(tag, v) {
  k <- is.finite(v) & IND$Year <= 2010
  f <- lm(log10(v[k]) ~ IND$Year[k])
  cat(sprintf("  %-16s %.3g -> %.3g t (%.1fx, %.2f log10) | %+.3f log10/decade (R2=%.2f) | %.2f decades of range\n",
              tag, sm(v, e1), sm(v, e2), sm(v, e1)/sm(v, e2), log10(sm(v, e1)/sm(v, e2)),
              coef(f)[2] * 10, summary(f)$r.squared,
              diff(log10(range(v, na.rm = TRUE)))))
}
say_t <- function(tag, v) {
  k <- is.finite(v) & IND$Year <= 2010
  f <- lm(v[k] ~ IND$Year[k])
  cat(sprintf("  %-16s %.3f -> %.3f (%+.3f units) | %+.4f units/decade (R2=%.2f) | range %.3f-%.3f\n",
              tag, sm(v, e1), sm(v, e2), sm(v, e2) - sm(v, e1),
              coef(f)[2] * 10, summary(f)$r.squared,
              min(v, na.rm = TRUE), max(v, na.rm = TRUE)))
}
cat("\n=== mean maximum body mass ===\n")
say_w("unsmoothed", IND$mmw_t); say_w("pooled catch *", IND$mmw_t_s3)
say_w("rolling mean", IND$mmw_t_s3_ratioform)
cat("\n=== mean trophic level ===\n")
say_t("unsmoothed", IND$mti); say_t("pooled catch *", IND$mti_s3)
say_t("rolling mean", IND$mti_s3_ratioform)
cat("  * plotted. Window applied to the catch, not the indicators.\n")
cat("\n=== the contrast, smoothed, as a fraction of each exploited-species range ===\n")
cat(sprintf("  mass  %.1f%%   trophic %.1f%%\n",
            100 * log10(sm(IND$mmw_t_s3, e1) / sm(IND$mmw_t_s3, e2)) / diff(w_rng),
            100 * (sm(IND$mti_s3, e1) - sm(IND$mti_s3, e2)) / diff(tl_rng)))

# The unsmoothed dual figure's axis, unchanged, so the two are directly
# comparable. It is not truncated to hide the raw excursions -- they are still
# drawn, just no longer the thing the eye follows.
y_lim <- c(10^-5.5, 10^2.15)
key_x <- X_LIM[1] + 1          # bottom-left corner is empty for all four series

pB <- ggplot(IND, aes(Year)) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  # unsmoothed, behind and faint: what the window removed, one per indicator
  geom_line(aes(y = tl2mass(mti)), linewidth = 0.3, colour = COL_T, alpha = 0.4,
            na.rm = TRUE) +
  geom_line(aes(y = mmw_t), linewidth = 0.3, colour = COL_W, alpha = 0.4,
            na.rm = TRUE) +
  # smoothed, in front; trophic first so mass -- the panel's primary -- is on top
  geom_line(aes(y = tl2mass(mti_s3)), linewidth = 0.7, colour = COL_T,
            na.rm = TRUE) +
  geom_point(aes(y = tl2mass(mti_s3)), size = 1.5, shape = 17, colour = COL_T,
             na.rm = TRUE) +
  geom_line(aes(y = mmw_t_s3), linewidth = 0.7, colour = COL_W, na.rm = TRUE) +
  geom_point(aes(y = mmw_t_s3), size = 1.5, shape = 16, colour = COL_W,
             na.rm = TRUE) +
  # three-line key: one per indicator, plus what the faint lines are
  annotate("point", x = key_x, y = 10^-3.75, shape = 16, size = 1.5, colour = COL_W) +
  annotate("text",  x = key_x + 2.5, y = 10^-3.75, hjust = 0, size = 2.9,
           colour = COL_W, label = "Mean maximum body mass (left axis)") +
  annotate("point", x = key_x, y = 10^-4.40, shape = 17, size = 1.5, colour = COL_T) +
  annotate("text",  x = key_x + 2.5, y = 10^-4.40, hjust = 0, size = 2.9,
           colour = COL_T, label = "Mean trophic level (right axis)") +
  annotate("text",  x = key_x, y = 10^-5.10, hjust = 0, size = 2.9,
           colour = "grey45",
           label = sprintf("Bold: %d-year centred window, catch-weighted.  Faint: annual.",
                           SMOOTH_K)) +
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

fig <- (pA / pB) + plot_layout(heights = c(1, 0.7), guides = "collect") &
  theme(legend.position = "bottom")
png_out <- guard(file.path(FIGS, sprintf("fig1_mmw_mti_smooth3_%s.png", SUF)))
pdf_out <- guard(file.path(FIGS, sprintf("fig1_mmw_mti_smooth3_%s.pdf", SUF)))
ggsave(png_out, fig, width = 11, height = 10.5, dpi = 300, limitsize = FALSE)
ggsave(pdf_out, fig, width = 11, height = 10.5, limitsize = FALSE)
write.csv(IND, guard(file.path(DATA,
          sprintf("fig1_mmw_mti_smooth3_series_%s.csv", SUF))), row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")