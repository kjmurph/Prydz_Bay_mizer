# =============================================================================
# FIGURE 1 (MEAN-MAXIMUM-SIZE, 3-YEAR SMOOTHED) -- rebuilt 44, cut A, 167 members.
#
#   A  Observed catch, stacked by species          (identical to F01_figure1_mmw)
#   B  Mean maximum body mass of the observed catch under a 3-year centred
#      rolling window, with the unsmoothed series drawn faintly behind it
#
# Sibling of F01_figure1_mmw_rebuilt167.R; read that header for panel A, the
# vlines, the colour key, the zero-catch handling, the era break and the
# reference-line placement, none of which change here. What changes:
#
# ------------------------------------------------------------------------------
#  1. THE WINDOW IS APPLIED TO THE CATCH, NOT TO THE INDICATOR. The smoothed
#     series is the catch-weighted mean over the three-year pooled catch,
#
#         MMW_3(y) = sum_{y' in W(y)} sum_i C_iy' w_max,i
#                    ---------------------------------------- ,  W(y)={y-1,y,y+1}
#                    sum_{y' in W(y)} sum_i C_iy'
#
#     i.e. numerator and denominator are smoothed separately and divided after.
#     This is NOT the same as a rolling mean of MMW(y), and the difference is
#     the whole point of the variant.
#
#     WHY. The excursions being smoothed are not noise in the indicator, they
#     are years in which the indicator is barely determined: 1971 (0.16 kt),
#     1988 (1.1 kt) and 1993 (0.04 kt) drag the raw series six decades below the
#     whale line while the record's own peak is 500 kt. A rolling mean of the
#     RATIO gives a 41-tonne year the same weight as a 500,000-tonne year and so
#     only dilutes the artefact to a third of itself -- at 1993 it returns
#     3.22 t against neighbours of 2.87 and 3.12, a visible notch that is still
#     pure sampling accident. Pooling the catch first weights each year by the
#     tonnage it actually carries, and 1993 returns 4.73 t, between its
#     neighbours' 4.46 and 4.57. Both were computed; the comparison is printed
#     to the console at run time, and SMOOTH_ON_RATIO below switches to the
#     rolling-mean form if it is ever wanted.
#
#  2. A YEAR WITH NO REPORTED CATCH STAYS BLANK, window or no window. The
#     indicator is undefined when nothing was caught, and smoothing must not
#     invent a value for such a year out of its neighbours' catch -- 1941 would
#     otherwise acquire a value from 1940 alone. The panel's gaps are therefore
#     exactly panel A's and exactly the unsmoothed figure's: 1941-45 (the
#     wartime shutdown of Antarctic whaling) and 2005, 2009, 2011, 2012.
#
#  3. THE WINDOW IS COMPLETE EVERYWHERE EXCEPT 1930, and there it is exact
#     rather than partial: the catch record begins in 1930 because whaling did,
#     so the missing 1929 half of that window is a true zero, not a gap. The
#     window is taken on the FULL csv (1930-2019) before the 1920-2012 filter,
#     so no year inside the panel is smoothed against a truncated neighbourhood.
#     The positional window equals the calendar window only on a contiguous year
#     grid, which is asserted rather than assumed.
#
#  4. THE UNSMOOTHED SERIES IS DRAWN FAINTLY BEHIND. The unsmoothed figure's
#     header states that nothing there is smoothed, thresholded or clipped; this
#     one is smoothed, so it shows what the smoothing removed rather than
#     presenting the result alone. It also justifies keeping the y axis at the
#     unsmoothed figure's full range -- the smoothed series alone spans 2.01
#     decades and would not otherwise need an axis that shows all nine species
#     reference lines. The two figures are on identical axes and can be compared
#     directly or overlaid.
#
#  5. THE SMOOTHING DOES NOT MOVE THE RESULT, which is the point of quoting it.
#     Era medians go 96.8 -> 97.1 t and 5.87 -> 5.78 t, a 16.5-fold fall
#     becoming 16.8-fold. What collapses is the range, 6.80 decades to 2.01:
#     the entire lower five decades of the raw panel were three near-zero-catch
#     years, and none of the trend was.
#
# Writes Manuscript figures/fig1_mmw_smooth3_rebuilt167.{png,pdf}
#        Manuscript data/fig1_mmw_smooth3_series_rebuilt167.csv
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
SMOOTH_K        <- 3            # centred window width, years
SMOOTH_ON_RATIO <- FALSE        # see header note 1; TRUE = rolling mean of MMW
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
stopifnot(all(spp %in% names(wmax)))
C <- vapply(obs_raw[spp], function(v) {
  x <- suppressWarnings(as.numeric(v)); x[is.na(x)] <- 0; pmax(x, 0)
}, numeric(nrow(obs_raw)))
num <- as.vector(C %*% wmax[spp]); den <- rowSums(C)

# --- the 3-year centred rolling window ----------------------------------------
# Header notes 1-3. The window is positional, so it is a calendar window only on
# a contiguous year grid -- assert rather than assume.
stopifnot(all(diff(obs_raw$Year) == 1))
roll_sum <- function(v, k = SMOOTH_K) {
  h <- (k - 1) / 2
  vapply(seq_along(v), function(i)
    sum(v[max(1, i - h):min(length(v), i + h)]), numeric(1))
}
roll_mean_defined <- function(v, k = SMOOTH_K) {   # the rejected form, note 1
  h <- (k - 1) / 2
  vapply(seq_along(v), function(i) {
    w <- v[max(1, i - h):min(length(v), i + h)]
    if (all(is.na(w))) NA_real_ else mean(w, na.rm = TRUE)
  }, numeric(1))
}
# `den > 0` is note 2: a year with no catch of its own stays NA either way.
raw_t   <- ifelse(den > 0, num / den, NA) / 1e6
pooled  <- ifelse(den > 0, roll_sum(num) / roll_sum(den), NA) / 1e6
ratiosm <- ifelse(den > 0, roll_mean_defined(raw_t), NA)

MMW <- data.frame(Year = obs_raw$Year,
                  total_kt  = den / 1e9,
                  mmw_t     = raw_t,
                  mmw_t_s3  = if (SMOOTH_ON_RATIO) ratiosm else pooled,
                  mmw_t_s3_ratioform = ratiosm) %>%
  filter(Year >= X_LIM[1], Year <= X_LIM[2])

# --- summary statistics quoted in the manuscript text -------------------------
# Break at the last whaling-dominated year, 1970, as in the unsmoothed build.
e1 <- MMW$Year <= 1970; e2 <- MMW$Year >= 1971 & MMW$Year <= 2010
sm <- function(v, sel) median(v[sel & is.finite(v)])
say <- function(tag, v) {
  k <- is.finite(v) & MMW$Year <= 2010
  f <- lm(log10(v[k]) ~ MMW$Year[k])
  cat(sprintf("  %-16s %.3g -> %.3g t (%.1fx) | %+.3f log10/decade (p=%.2g, R2=%.2f) | range %.3g-%.3g t (%.2f decades)\n",
              tag, sm(v, e1), sm(v, e2), sm(v, e1) / sm(v, e2),
              coef(f)[2] * 10, summary(f)$coefficients[2, 4], summary(f)$r.squared,
              min(v, na.rm = TRUE), max(v, na.rm = TRUE),
              diff(log10(range(v, na.rm = TRUE)))))
}
cat("\n=== mean maximum body mass of the catch, ", SMOOTH_K,
    "-year centred window ===\n", sep = "")
cat("                   1930-70 -> 1971-2010 median | trend | range\n")
say("unsmoothed",       MMW$mmw_t)
say("pooled catch *",   MMW$mmw_t_s3)
say("rolling mean",     MMW$mmw_t_s3_ratioform)
cat("  * plotted. See header note 1 for why the pooled-catch form is the one used.\n")
cat(sprintf("  the three near-zero-catch years, raw -> pooled -> rolling mean (t):\n"))
print(MMW %>% filter(Year %in% c(1971, 1988, 1993)) %>%
        transmute(Year, kt = round(total_kt, 3), raw = signif(mmw_t, 3),
                  pooled = round(mmw_t_s3, 3), rollmean = round(mmw_t_s3_ratioform, 3)),
      row.names = FALSE)

# --- species w_max reference lines --------------------------------------------
# Unchanged from the unsmoothed build, including the hand-set label sides: the
# unsmoothed series is still drawn (faintly), so it still occupies the same
# space and the same placement still clears it.
REF_TAXA <- names(which(colSums(eff) > 0))
stopifnot(setequal(REF_TAXA, caught))
REF_TAXA <- REF_TAXA[order(wmax[REF_TAXA], decreasing = TRUE)]
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

# THE AXIS IS THE UNSMOOTHED FIGURE'S, UNCHANGED (header note 4), so the two are
# directly comparable. It is not truncated to hide the raw excursions -- they are
# still drawn, just no longer the thing the eye follows.
y_lim <- c(10^-5.5, 10^2.15)
key_x <- X_LIM[1] + 1

pB <- ggplot(MMW, aes(Year)) +
  geom_hline(data = ref, aes(yintercept = y, colour = Species),
             linetype = "dotted", linewidth = 0.5, alpha = 0.85,
             show.legend = FALSE, inherit.aes = FALSE) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  geom_text(data = ref, aes(x = x, y = y, label = lab, hjust = hj, vjust = vj,
                            colour = Species),
            size = 2.7, show.legend = FALSE, inherit.aes = FALSE) +
  # unsmoothed, behind and faint: what the window removed
  geom_line(aes(y = mmw_t), linewidth = 0.3, colour = "grey45", alpha = 0.5,
            na.rm = TRUE) +
  geom_point(aes(y = mmw_t), size = 0.7, colour = "grey45", alpha = 0.5,
             na.rm = TRUE) +
  # smoothed, in front
  geom_line(aes(y = mmw_t_s3), linewidth = 0.7, colour = "grey20", na.rm = TRUE) +
  geom_point(aes(y = mmw_t_s3), size = 1.5, colour = "grey10", na.rm = TRUE) +
  # the key sits in the band between the bathypelagic-fish line (6.0e-4 t) and
  # the antarctic krill line (4.2e-6 t), which no series and no reference label
  # occupies
  annotate("point", x = key_x, y = 10^-4.00, size = 1.5, colour = "grey10") +
  annotate("text",  x = key_x + 2.5, y = 10^-4.00, hjust = 0, size = 2.9,
           colour = "grey10",
           label = sprintf("%d-year centred window, catch-weighted", SMOOTH_K)) +
  annotate("point", x = key_x, y = 10^-4.62, size = 0.7, colour = "grey45") +
  annotate("text",  x = key_x + 2.5, y = 10^-4.62, hjust = 0, size = 2.9,
           colour = "grey45", label = "Annual (unsmoothed)") +
  scale_colour_manual(values = sp_cols, guide = "none") +
  scale_x_continuous(breaks = X_BRK, limits = X_LIM) +
  scale_y_log10(breaks = 10^seq(-5, 2, 1),
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

fig <- (pA / pB) + plot_layout(heights = c(1, 0.7), guides = "collect") &
  theme(legend.position = "bottom")
png_out <- guard(file.path(FIGS, sprintf("fig1_mmw_smooth3_%s.png", SUF)))
pdf_out <- guard(file.path(FIGS, sprintf("fig1_mmw_smooth3_%s.pdf", SUF)))
ggsave(png_out, fig, width = 11, height = 10.5, dpi = 300, limitsize = FALSE)
ggsave(pdf_out, fig, width = 11, height = 10.5, limitsize = FALSE)
write.csv(MMW, guard(file.path(DATA,
          sprintf("fig1_mmw_smooth3_series_%s.csv", SUF))), row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")