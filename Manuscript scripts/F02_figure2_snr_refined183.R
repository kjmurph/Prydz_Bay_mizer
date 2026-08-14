# =============================================================================
# FIGURE 2 -- signal-to-noise ratio panels, refined 183-member fitted ensemble.
#
#   a  Biomass SNR
#   b  Biomass variability SNR (15 yr trailing rolling SD)
#   c  Size-spectrum slope SNR
#   d  Slope variability SNR (15 yr trailing rolling SD)
#
# CANONICAL SNR, transcribed from Plotting scripts/biomass_slope_snr_mean_med.R
# (~line 89). The denominator is the SD **across years 1841-2010 of the
# across-member mean UNEXPLOITED trajectory** -- a temporal SD of one curve,
# giving ONE scalar per metric. It is NOT a rolling SD, and it is not the
# across-member spread. The 15 yr window applies only to the *variability*
# panels (b, d); the level panels (a, c) are window-independent.
#
# Getting this wrong is the single easiest way to produce a plausible-looking
# but meaningless SNR, so the definition is reproduced here rather than
# re-derived.
#
# Data: Manuscript data/*_refined183.rds, built by F00_build_refined183_data.R
# from the corrected-w_min model over the refined 183 members.
#
# Writes Manuscript figures/fig2_snr_refined183.{png,pdf}
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(zoo)
})

DATA <- "Manuscript data"; FIGS <- "Manuscript figures"
dir.create(FIGS, showWarnings = FALSE)
SUF <- "refined183"
BASELINE_YEARS <- 1841:2010     # full unexploited record
WINDOW <- 15L                   # trailing rolling-SD window for panels b and d
PLOT_YEARS <- 1900:2010

guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE)
  f
}

# --- era markers (derived from the effort array, not hard-coded) --------------
eff <- readRDS("effort_array_1841_2010.rds")
yr <- as.numeric(rownames(eff))
onset <- function(sp) { y <- yr[eff[, sp] > 0]; if (length(y)) min(y) else NA }
peak  <- function(sp) { v <- eff[, sp]; if (any(v > 0)) yr[which.max(v)] else NA }
whaling_start <- min(c(onset("baleen whales"), onset("sperm whales"),
                       onset("minke whales")), na.rm = TRUE)
krill_start   <- onset("antarctic krill")
peaks <- c("Peak baleen" = peak("baleen whales"), "Peak sperm" = peak("sperm whales"),
           "Peak minke" = peak("minke whales"),   "Peak krill" = peak("antarctic krill"))
peaks <- peaks[!is.na(peaks)]

# --- data --------------------------------------------------------------------
bf <- readRDS(file.path(DATA, sprintf("biomass_abund_fish_%s.rds", SUF)))
bc <- readRDS(file.path(DATA, sprintf("biomass_abund_clim_%s.rds", SUF)))
sl <- readRDS(file.path(DATA, sprintf("nbss_slope_%s.rds", SUF)))
meta <- readRDS(file.path(DATA, sprintf("meta_%s.rds", SUF)))
message("members: ", meta$n_members, " | arm: ", meta$arm)

# community biomass per member-year
tot <- function(d) d %>% group_by(member = sim_index, Year) %>%
  summarise(val = sum(Biomass), .groups = "drop")
bio_f <- tot(bf); bio_c <- tot(bc)

slope_f <- sl %>% filter(arm == "exploited")   %>% transmute(member = sim_index, Year, val = slope)
slope_c <- sl %>% filter(arm == "unexploited") %>% transmute(member = sim_index, Year, val = slope)

# --- the canonical SNR -------------------------------------------------------
make_snr <- function(fish_m, clim_m) {
  rep_unexp <- clim_m %>% group_by(Year) %>%
    summarise(mu = mean(val, na.rm = TRUE), .groups = "drop")
  noise <- sd(rep_unexp$mu[rep_unexp$Year %in% BASELINE_YEARS], na.rm = TRUE)
  if (is.na(noise) || noise == 0) stop("baseline noise is NA or 0")
  inner_join(fish_m, clim_m, by = c("member", "Year"), suffix = c("_f", "_c")) %>%
    mutate(signal = val_f - val_c) %>%
    group_by(Year) %>%
    summarise(signal_med = median(signal, na.rm = TRUE),
              signal_q25 = quantile(signal, 0.25, na.rm = TRUE),
              signal_q75 = quantile(signal, 0.75, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(noise = noise, snr_med = signal_med / noise,
           snr_q25 = signal_q25 / noise, snr_q75 = signal_q75 / noise)
}

# rolling SD per member, then the same SNR machinery on the variability series
roll_sd <- function(d, k = WINDOW) d %>% arrange(member, Year) %>% group_by(member) %>%
  mutate(val = zoo::rollapplyr(val, k, sd, fill = NA)) %>% ungroup()

snr_bio   <- make_snr(bio_f, bio_c)
snr_slope <- make_snr(slope_f, slope_c)
snr_biov  <- make_snr(roll_sd(bio_f), roll_sd(bio_c))
snr_slov  <- make_snr(roll_sd(slope_f), roll_sd(slope_c))

cat("\n=== SNR at 2010 (compare against the published -0.920 biomass / -10.404 lambda) ===\n")
cat("  biomass SNR 2010:", signif(snr_bio$snr_med[snr_bio$Year == 2010], 4), "\n")
cat("  slope   SNR 2010:", signif(snr_slope$snr_med[snr_slope$Year == 2010], 4), "\n")

# --- plotting ----------------------------------------------------------------
th <- theme_classic(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        plot.tag = element_text(face = "bold", size = 13),
        plot.margin = margin(4, 8, 2, 6))

panel <- function(d, ylab, col, fill, tag, show_x = FALSE) {
  d <- d %>% filter(Year %in% PLOT_YEARS)
  p <- ggplot(d, aes(Year, snr_med)) +
    geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.3) +
    geom_hline(yintercept = c(-1, 1), colour = "red", linetype = "dashed",
               linewidth = 0.4) +
    geom_vline(xintercept = c(whaling_start, krill_start), colour = "grey45",
               linetype = "dashed", linewidth = 0.5) +
    geom_vline(xintercept = peaks, colour = "grey60", linetype = "dotted",
               linewidth = 0.35) +
    geom_ribbon(aes(ymin = snr_q25, ymax = snr_q75), fill = fill, alpha = 0.35) +
    geom_line(colour = col, linewidth = 0.7) +
    annotate("text", x = whaling_start, y = Inf, label = "Whaling\nstarts",
             hjust = 1.05, vjust = 1.2, size = 2.9, colour = "grey35") +
    annotate("text", x = krill_start, y = Inf, label = "Krill fishing\nstarts",
             hjust = 1.05, vjust = 1.2, size = 2.9, colour = "grey35") +
    labs(y = ylab, tag = tag) +
    scale_x_continuous(limits = range(PLOT_YEARS), expand = expansion(mult = 0.01)) +
    th
  if (show_x) p <- p + theme(axis.title.x = element_text()) + labs(x = "Year")
  p
}

pa <- panel(snr_bio,  "Biomass SNR", "#2C6FA6", "#9EC5E3", "a")
pb <- panel(snr_biov, sprintf("Biomass variability SNR\n( %dyr rolling SD )", WINDOW),
            "#1F4E79", "#9EC5E3", "b")
pc <- panel(snr_slope, "Size-spectrum slope SNR", "#C0392B", "#F0B7B2", "c")
pd <- panel(snr_slov, sprintf("Slope variability SNR\n( %dyr rolling SD )", WINDOW),
            "#7B1E14", "#F0B7B2", "d", show_x = TRUE)

fig <- pa / pb / pc / pd

png_out <- guard(file.path(FIGS, sprintf("fig2_snr_%s.png", SUF)))
pdf_out <- guard(file.path(FIGS, sprintf("fig2_snr_%s.pdf", SUF)))
ggsave(png_out, fig, width = 8, height = 13, dpi = 300)
ggsave(pdf_out, fig, width = 8, height = 13)

series <- bind_rows(
  snr_bio   %>% mutate(metric = "biomass"),
  snr_biov  %>% mutate(metric = "biomass_variability"),
  snr_slope %>% mutate(metric = "slope"),
  snr_slov  %>% mutate(metric = "slope_variability"))
write.csv(series, guard(file.path(DATA, sprintf("fig2_snr_series_%s.csv", SUF))),
          row.names = FALSE)

cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")
