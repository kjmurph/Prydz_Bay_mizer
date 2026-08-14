# =============================================================================
# FIGURE 1 (MEAN-TROPHIC-INDEX, 3-YEAR SMOOTHED) -- rebuilt 44, cut A, 167 members.
#
#   A  Observed catch, stacked by species          (identical to F01_figure1_mmw)
#   B  Mean trophic level of the observed catch under a 3-year centred rolling
#      window, with the unsmoothed series drawn faintly behind it
#
# Sibling of F01_figure1_mti_rebuilt167.R; read that header for the trophic-level
# source, the linear axis, the reference-line placement and the result. Read
# F01_figure1_mmw_smooth3_rebuilt167.R's header for the smoothing, which is
# identical in form -- the window is applied to the CATCH, not to the indicator:
#
#     MTI_3(y) = sum_{y' in W(y)} sum_i C_iy' TL_i
#                --------------------------------- ,   W(y) = {y-1, y, y+1}
#                sum_{y' in W(y)} sum_i C_iy'
#
# so each year enters weighted by the tonnage it actually carries, and a year
# with no catch of its own stays blank rather than borrowing a value from its
# neighbours. Both the pooled-catch and the rolling-mean-of-the-ratio forms are
# computed and compared on the console; SMOOTH_ON_RATIO switches which is drawn.
#
# WHAT THE SMOOTHING DOES HERE, and it is not what it does to the mass panel.
# On the mass panel the window removes three sampling artefacts and leaves the
# trend untouched (16.5-fold becomes 16.8-fold). On this panel it removes the
# same three years -- the range narrows from 2.402-5.226 to 3.291-5.140, and the
# 1993 floor, which was 41 tonnes of catch that happened to be 99.8% krill,
# disappears -- but it also DOUBLES the apparent era shift, from -0.065 to
# -0.136 trophic units. That is still 4.6% of the range the nine exploited
# groups span, against 16.5% for mass, so the conclusion survives: Prydz Bay was
# fished down the size spectrum, not down the food web. But the -0.065 figure is
# the one to quote for the unsmoothed record and -0.136 for the smoothed, and
# neither should be quoted without saying which.
#
# WHY THE SHIFT MOVES AT ALL: the raw 1971-2010 median sits on a run of
# minke-only years at exactly TL 3.955, while the pooled window mixes each of
# those years with its krill-bearing neighbours and pulls them down. The
# smoothed series is the better estimate of what was actually being removed from
# the ecosystem per unit time; the raw series is the better description of what
# a given year's fishery targeted. They answer different questions.
#
# Writes Manuscript figures/fig1_mti_smooth3_rebuilt167.{png,pdf}
#        Manuscript data/fig1_mti_smooth3_series_rebuilt167.csv
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
SMOOTH_ON_RATIO <- FALSE        # TRUE = rolling mean of MTI instead
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
spp <- setdiff(names(obs_raw), "Year")
stopifnot(all(spp %in% names(TROPHIC_LEVELS)))
C <- vapply(obs_raw[spp], function(v) {
  x <- suppressWarnings(as.numeric(v)); x[is.na(x)] <- 0; pmax(x, 0)
}, numeric(nrow(obs_raw)))
num <- as.vector(C %*% TROPHIC_LEVELS[spp]); den <- rowSums(C)

# --- the 3-year centred rolling window ----------------------------------------
# The window is positional, so it is a calendar window only on a contiguous year
# grid -- assert rather than assume. Taken on the full csv (1930-2019) before the
# 1920-2012 filter, so no year inside the panel sees a truncated neighbourhood;
# the one partial window, 1930's, is exact because the record begins when
# whaling did.
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
# the panel's gaps stay exactly panel A's -- 1941-45 and 2005/2009/2011/2012.
raw_tl  <- ifelse(den > 0, num / den, NA)
pooled  <- ifelse(den > 0, roll_sum(num) / roll_sum(den), NA)
ratiosm <- ifelse(den > 0, roll_mean_defined(raw_tl), NA)

MTI <- data.frame(Year = obs_raw$Year,
                  total_kt = den / 1e9,
                  mti      = raw_tl,
                  mti_s3   = if (SMOOTH_ON_RATIO) ratiosm else pooled,
                  mti_s3_ratioform = ratiosm) %>%
  filter(Year >= X_LIM[1], Year <= X_LIM[2])

# --- summary statistics quoted in the manuscript text -------------------------
e1 <- MTI$Year <= 1970; e2 <- MTI$Year >= 1971 & MTI$Year <= 2010
sm <- function(v, sel) median(v[sel & is.finite(v)])
say <- function(tag, v) {
  k <- is.finite(v) & MTI$Year <= 2010
  f <- lm(v[k] ~ MTI$Year[k])       # LINEAR: one trophic unit is one transfer
  cat(sprintf("  %-16s %.3f -> %.3f (%+.3f) | %+.4f units/decade (p=%.2g, R2=%.2f) | range %.3f-%.3f\n",
              tag, sm(v, e1), sm(v, e2), sm(v, e2) - sm(v, e1),
              coef(f)[2] * 10, summary(f)$coefficients[2, 4], summary(f)$r.squared,
              min(v, na.rm = TRUE), max(v, na.rm = TRUE)))
}
cat("\n=== mean trophic level of the catch, ", SMOOTH_K,
    "-year centred window ===\n", sep = "")
cat("                   1930-70 -> 1971-2010 median | trend | range\n")
say("unsmoothed",     MTI$mti)
say("pooled catch *", MTI$mti_s3)
say("rolling mean",   MTI$mti_s3_ratioform)
cat("  * plotted. Window applied to the catch, not the indicator.\n")
cat("  the three near-zero-catch years, raw -> pooled -> rolling mean:\n")
print(MTI %>% filter(Year %in% c(1971, 1988, 1993)) %>%
        transmute(Year, kt = round(total_kt, 3), raw = round(mti, 3),
                  pooled = round(mti_s3, 3), rollmean = round(mti_s3_ratioform, 3)),
      row.names = FALSE)

# --- species trophic-level reference lines ------------------------------------
# Unchanged from the unsmoothed build, sides and offsets included: the
# unsmoothed series is still drawn (faintly) and the smoothed one stays inside
# its envelope, so the same placement still clears both.
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
stopifnot(setequal(names(REF_SIDE), REF_TAXA), setequal(names(REF_VJ), REF_TAXA))

ref <- data.frame(Species = REF_TAXA,
                  y = unname(TROPHIC_LEVELS[REF_TAXA]),
                  lab = pretty_lab(REF_TAXA),
                  stringsAsFactors = FALSE) %>%
  mutate(side = unname(REF_SIDE[Species]),
         x  = ifelse(side == "left", X_LIM[1] + 1, X_LIM[2] - 1),
         hj = ifelse(side == "left", 0, 1),
         vj = unname(REF_VJ[Species]))
stopifnot(!any(is.na(sp_cols[ref$Species])))

# The unsmoothed figure's axis, unchanged, so the two are directly comparable.
y_lim <- c(2.30, 5.45)
key_x <- X_LIM[1] + 1

pB <- ggplot(MTI, aes(Year)) +
  geom_hline(data = ref, aes(yintercept = y, colour = Species),
             linetype = "dotted", linewidth = 0.5, alpha = 0.85,
             show.legend = FALSE, inherit.aes = FALSE) +
  geom_vline(xintercept = c(whal, krl), linetype = "dashed", colour = "grey45") +
  geom_text(data = ref, aes(x = x, y = y, label = lab, hjust = hj, vjust = vj,
                            colour = Species),
            size = 2.6, show.legend = FALSE, inherit.aes = FALSE) +
  # unsmoothed, behind and faint: what the window removed
  geom_line(aes(y = mti), linewidth = 0.3, colour = "grey45", alpha = 0.5,
            na.rm = TRUE) +
  geom_point(aes(y = mti), size = 0.7, colour = "grey45", alpha = 0.5,
             na.rm = TRUE) +
  # smoothed, in front
  geom_line(aes(y = mti_s3), linewidth = 0.7, colour = "grey20", na.rm = TRUE) +
  geom_point(aes(y = mti_s3), size = 1.5, colour = "grey10", na.rm = TRUE) +
  # the key sits below the krill reference line's label, in the band no series
  # reaches (the smoothed minimum is 3.29, the raw minimum 2.40 at 1993)
  annotate("point", x = key_x, y = 2.78, size = 1.5, colour = "grey10") +
  annotate("text",  x = key_x + 2.5, y = 2.78, hjust = 0, size = 2.9,
           colour = "grey10",
           label = sprintf("%d-year centred window, catch-weighted", SMOOTH_K)) +
  annotate("point", x = key_x, y = 2.60, size = 0.7, colour = "grey45") +
  annotate("text",  x = key_x + 2.5, y = 2.60, hjust = 0, size = 2.9,
           colour = "grey45", label = "Annual (unsmoothed)") +
  scale_colour_manual(values = sp_cols, guide = "none") +
  scale_x_continuous(breaks = X_BRK, limits = X_LIM) +
  scale_y_continuous(breaks = seq(2.5, 5.5, 0.5)) +
  coord_cartesian(ylim = y_lim) +
  labs(x = "Year", y = "Mean trophic level of catch", tag = "B") +
  theme_classic(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.tag = element_text(face = "bold", size = 14))

# Same height as A, as in the unsmoothed trophic build: the minke (3.955) and
# large baleen whale (3.867) lines are 0.088 trophic units apart and both need a
# label that clears both lines.
fig <- (pA / pB) + plot_layout(heights = c(1, 1), guides = "collect") &
  theme(legend.position = "bottom")
png_out <- guard(file.path(FIGS, sprintf("fig1_mti_smooth3_%s.png", SUF)))
pdf_out <- guard(file.path(FIGS, sprintf("fig1_mti_smooth3_%s.pdf", SUF)))
ggsave(png_out, fig, width = 11, height = 10.5, dpi = 300, limitsize = FALSE)
ggsave(pdf_out, fig, width = 11, height = 10.5, limitsize = FALSE)
write.csv(MTI, guard(file.path(DATA,
          sprintf("fig1_mti_smooth3_series_%s.csv", SUF))), row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")