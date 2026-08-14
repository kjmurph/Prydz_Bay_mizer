###############################################################################
# yield_ratio_combined_panels.R
#
# Combined publication figure pairing:
#   Panel a  Stacked-area yield (top-10% RMSE ensemble mean) + observed inset,
#            now with +/-1 SD error bars at the auto-detected yield peaks.
#   Panel b  Community size-spectrum ratio (Exploited / Unexploited), ref period
#            2001-2010 -- i.e. panel a of spectrum_ratio_slope_top10pct.R -- now
#            with a +/-1 SD natural-variability reference (horizontal dashed
#            lines) around the null ratio of 1.0.
#
# The reference on the ratio panel mirrors the SNR / paired-difference framing
# used in biomass_slope_snr.R and spectrum_ratio_slope_top10pct.R panel b:
#   noise(w) = across-ensemble SD of the UNEXPLOITED (climate-only) community
#              spectrum at body mass w, expressed as a CV so it maps onto the
#              multiplicative ratio scale.  The horizontal lines use a single
#              representative CV (median over the resolved spectrum).  Where the
#              ratio line leaves 1 +/- 1 SD the fishing effect at that size has
#              "emerged" beyond natural spread.
#
# Layouts written:
#   yield_ratio_combined_2col.png   (2 col x 1 row: yield left,  ratio right)
#   yield_ratio_combined_2row.png   (1 col x 2 row: yield top,   ratio bottom)
# Plus standalone panels for checking:
#   yield_stacked_top10pct_sd_errorbars.png
#   spectrum_ratio_panelA_sd_reference.png
#
# All inputs are pre-existing caches; the heavy MC / slope ensembles are NOT
# re-run.
###############################################################################

suppressPackageStartupMessages({
  library(mizer)
  library(tidyverse)
  library(reshape2)
  library(scales)
  library(patchwork)
})

cat("=== Combined yield + size-spectrum-ratio figure ===\n\n")

# ===========================================================================
# CONFIG
# ===========================================================================
PLOT_YEARS  <- 1900:2010
PCT         <- 10          # ensemble percentile used for both panels
PEAK_WINDOW <- 3           # half-window (yr) for local-maxima detection
MIN_PEAK_KT <- 10          # minimum peak height (kt) to annotate
# Display cap for the yield y-axis (tonnes). The total-yield ensemble is heavily
# right-skewed, so mean+1 SD reaches ~2900 kt at the 1931 peak; capping keeps the
# stacked area legible. The cap is set high enough to open a clear band across the
# top of the panel for the (wide) observed-yield inset. Whiskers that run past the
# cap are drawn to the cap and flagged with an up-arrow (see below).
YMAX_CAP <- 1.5e6          # 1,500 kt

species_order <- c(
  "mesozooplankton", "other krill", "other macrozooplankton",
  "antarctic krill", "salps", "mesopelagic fishes", "bathypelagic fishes",
  "shelf and coastal fishes", "flying birds", "small divers", "squids",
  "toothfishes", "leopard seals", "medium divers", "large divers",
  "minke whales", "orca", "sperm whales", "baleen whales"
)

output_dir <- "ecosystem_assessment_outputs/abundance_ratio_plots"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ===========================================================================
# PART 1 -- YIELD PANEL (mean stacked area + SD error bars at peaks)
# ===========================================================================
cat("--- Building yield panel ---\n")

stack_cache <- sprintf("yield_cached_top%dpct_stack.rds", PCT)          # mean/sp/yr
total_cache <- sprintf("yield_cached_top%dpct_total_per_sim.rds", PCT)  # total/yr/sim
obs_cache   <- "yield_cached_obs_stack.rds"
for (f in c(stack_cache, total_cache, obs_cache))
  if (!file.exists(f)) stop("Missing cache: ", f, " -- run yield_stacked_pct_variants.R first.")

stack_df      <- readRDS(stack_cache)
total_per_sim <- readRDS(total_cache)
obs_stack     <- readRDS(obs_cache)

# fished species in CSV column order (matches original palette exactly)
obs_raw_cols   <- read.csv("yield_observed_timeseries.csv", nrows = 1)
fished_species <- gsub("\\.", " ", setdiff(names(obs_raw_cols), "Year"))
fished_species <- fished_species[fished_species %in%
                                   as.character(unique(obs_stack$Species))]

sp_palette   <- setNames(scales::hue_pal()(length(fished_species)), fished_species)
# Tick labels in thousand-tonnes (unit is carried by the "10^3 t" axis title,
# so no "kt" suffix is repeated on the ticks).
kt_formatter <- function(x) comma(x / 1e3)

# Local-maxima detector (peak must top all neighbours within +/-window and >= min)
find_peaks <- function(total_t, years, window = PEAK_WINDOW,
                       min_t = MIN_PEAK_KT * 1000) {
  n <- length(total_t); is_peak <- logical(n)
  for (i in seq_len(n)) {
    lo <- max(1L, i - window); hi <- min(n, i + window)
    if (total_t[i] == max(total_t[lo:hi]) && total_t[i] >= min_t) is_peak[i] <- TRUE
  }
  years[is_peak]
}

# Peak table: centre = stack top (= mean total); bars = mean +/- 1 SD across sims.
# (sum over species of per-species mean == mean over sims of per-sim total, so the
#  centre dot sits exactly at the top of the rendered stacked area.)
make_peak_ci_sd <- function(sdf, total_ps) {
  totals <- sdf %>% group_by(Year) %>%
    summarise(centre = sum(median_t, na.rm = TRUE), .groups = "drop") %>%
    arrange(Year)
  pk  <- find_peaks(totals$centre, totals$Year)
  sds <- total_ps %>% filter(Year %in% pk) %>% group_by(Year) %>%
    summarise(mean_t = mean(total_t, na.rm = TRUE),
              sd_t   = sd(total_t,   na.rm = TRUE), .groups = "drop") %>%
    mutate(lo = pmax(mean_t - sd_t, 0), hi = mean_t + sd_t)
  totals %>% filter(Year %in% pk) %>% left_join(sds, by = "Year")
}

add_bars_sd <- function(p, ci_df) {
  p +
    geom_errorbar(data = ci_df, aes(x = Year, ymin = lo, ymax = hi),
                  inherit.aes = FALSE, width = 1.4, linewidth = 0.7, colour = "white") +
    geom_errorbar(data = ci_df, aes(x = Year, ymin = lo, ymax = hi),
                  inherit.aes = FALSE, width = 1.1, linewidth = 0.45, colour = "grey20") +
    geom_point(data = ci_df, aes(x = Year, y = centre), inherit.aes = FALSE,
               shape = 21, size = 2, fill = "white", colour = "grey20", stroke = 0.5)
}

ci_sd <- make_peak_ci_sd(stack_df, total_per_sim)
cat(sprintf("  Yield peaks (yr): %s\n", paste(ci_sd$Year, collapse = ", ")))

# Only the non-negligible species get a legend key (the small groups still fill
# the stack; they are just dropped from the legend). The legend is a single-column
# inset in the bottom-right of the yield panel.
legend_species <- c("baleen whales", "sperm whales", "orca",
                    "minke whales", "antarctic krill")

yield_scales <- list(
  scale_fill_manual(values = sp_palette, breaks = legend_species,
                    labels = c("Large baleen whales", "Sperm whales", "Orca",
                               "Minke whales", "Antarctic krill")),
  scale_x_continuous(breaks = seq(1900, 2010, by = 10),
                     expand = expansion(mult = c(0.01, 0.01))),
  # Main-panel y ticks in 500 (10^3 t) increments so they line up with the
  # observed-yield inset (whose max is ~500). Inset overrides its own breaks.
  scale_y_continuous(labels = kt_formatter, breaks = seq(0, 1.5e6, by = 2.5e5),
                     expand = expansion(mult = c(0, 0.03))),
  guides(fill = guide_legend(ncol = 1)),
  theme_bw(base_size = 13),
  theme(legend.position      = c(0.995, 0.03),
        legend.justification = c(1, 0),
        legend.background    = element_rect(fill = "white", colour = "grey70",
                                            linewidth = 0.3),
        legend.key           = element_rect(fill = "white", colour = NA),
        legend.key.size      = unit(0.42, "cm"),
        legend.title         = element_text(size = 10),
        legend.text          = element_text(size = 9),
        axis.text.x          = element_text(angle = 40, hjust = 1),
        panel.grid.major     = element_blank(),
        panel.grid.minor     = element_blank()),
  labs(x = "Year", y = expression(Yield~(10^3~t~y^{-1})), fill = "Species")
)

# Observed inset (no legend, boxed) -- same styling as the source figure
p_obs_inset <- Reduce(`+`, c(list(
  ggplot(obs_stack, aes(x = Year, y = Yield_t, fill = Species)) +
    geom_area(position = "stack", alpha = 0.85, colour = NA)), yield_scales)) +
  labs(title = "Observed yield") +
  # Inset y ticks: 0 and 500 only, matching the main panel's 500 increment.
  scale_y_continuous(labels = kt_formatter, breaks = c(0, 2.5e5, 5e5),
                     expand = expansion(mult = c(0, 0.02))) +
  theme(legend.position = "none", axis.title = element_blank(),
        axis.text = element_text(size = 6.5),
        plot.title = element_text(size = 8, face = "bold", margin = margin(b = 2)),
        plot.margin = margin(2, 2, 2, 2, "pt"),
        plot.background = element_rect(fill = "white", colour = "grey70", linewidth = 0.4))

# Observed inset spans the clear band across the top of the panel (opened up by
# the raised y-cap), running from ~1940 rightward so it stays above every whisker.
obs_inset <- inset_element(p_obs_inset, left = 0.58, bottom = 0.67,
                           right = 0.995, top = 0.98, align_to = "panel")

panel_yield <- Reduce(`+`, c(list(
  ggplot(stack_df, aes(x = Year, y = median_t, fill = Species)) +
    geom_area(position = "stack", alpha = 0.85, colour = NA)), yield_scales))
panel_yield <- add_bars_sd(panel_yield, ci_sd)

# Cap the axis; flag whiskers that run off-scale with an up-arrow.
clipped_sd <- ci_sd %>% dplyr::filter(hi > YMAX_CAP)
if (nrow(clipped_sd) > 0) {
  panel_yield <- panel_yield +
    geom_segment(data = clipped_sd,
                 aes(x = Year, xend = Year, y = YMAX_CAP * 0.90, yend = YMAX_CAP * 0.995),
                 inherit.aes = FALSE, colour = "grey20", linewidth = 0.45,
                 arrow = arrow(length = unit(0.18, "cm"), type = "closed"))
}
panel_yield <- panel_yield + coord_cartesian(ylim = c(0, YMAX_CAP), clip = "on")
cat(sprintf("  Off-scale peaks (whisker > %.0f kt): %s\n", YMAX_CAP / 1e3,
            if (nrow(clipped_sd)) paste(clipped_sd$Year, collapse = ", ") else "none"))

# Standalone yield figure (with inset) for checking
ggsave("yield_stacked_top10pct_sd_errorbars.png",
       panel_yield + obs_inset, width = 10.2, height = 4.8, dpi = 300, bg = "white")
cat("  Saved: yield_stacked_top10pct_sd_errorbars.png\n\n")

# ===========================================================================
# PART 2 -- SIZE-SPECTRUM RATIO PANEL (panel a of spectrum_ratio_slope*)
#           + natural-variability (+/-1 SD) reference
# ===========================================================================
cat("--- Building size-spectrum ratio panel ---\n")

spectra_cache_path <- file.path(output_dir, "spectra_cache_ref_period.rds")
species_cache_path <- file.path(output_dir, "spectra_cache_species_ref_period.rds")
temporal_cache_path <- file.path(output_dir,
  "climate_only_community_spectrum_by_year_top10pct.rds")
params_path        <- "params_sel_adj.rds"
rmse_path          <- "yield_rmse_per_sim.csv"
for (f in c(spectra_cache_path, species_cache_path, temporal_cache_path,
            params_path, rmse_path))
  if (!file.exists(f)) stop("Missing input: ", f)

# --- top-10% indices ---
rmse_df      <- read.csv(rmse_path) %>% arrange(rank)
n_sims       <- nrow(rmse_df)
n_top10pct   <- ceiling(n_sims * 0.10)
top10pct_idx <- rmse_df$sim_index[1:n_top10pct]

# --- community spectra cache -> matched ratio matrix ---
cache       <- readRDS(spectra_cache_path)
w_bins      <- cache$w_bins
n_w         <- cache$n_w
top_idx_valid <- top10pct_idx[top10pct_idx <= nrow(cache$fished_spectra)]
fished_top    <- cache$fished_spectra[top_idx_valid, , drop = FALSE]
climate_top   <- cache$climate_spectra[top_idx_valid, , drop = FALSE]
rm(cache); gc()

ratio_matrix <- fished_top / climate_top
ratio_matrix[!is.finite(ratio_matrix)] <- NA
n_valid_per_bin <- apply(ratio_matrix, 2, function(x) sum(is.finite(x)))
min_valid_for_ribbon <- ceiling(nrow(ratio_matrix) * 0.50)

# Natural-variability noise (SNR-consistent): temporal SD of the ENSEMBLE-MEAN
# unexploited (climate-only) community spectrum over 1841-2010, per body-mass bin,
# expressed as a CV so it lands on the multiplicative ratio scale (null = 1.0).
# Matches the detection-noise convention of biomass_slope_snr_mean_med.R (temporal
# SD of the ensemble-mean unexploited trajectory over the stationary record), NOT
# the across-member/inter-sim spread. Built by
# extract_climate_only_spectrum_by_year.R.
tcache  <- readRDS(temporal_cache_path)
cv_by_w <- apply(tcache$Nbar_clim, 2,
                 function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
# map the climate cache's size grid onto the ratio-panel grid (nearest bin)
cv_temporal <- cv_by_w[vapply(w_bins,
                              function(w) which.min(abs(tcache$w_bins - w)),
                              integer(1))]
cat(sprintf("  Temporal noise: %d climate members, years %d-%d\n",
            tcache$n_members, min(tcache$years), max(tcache$years)))

pair_ratio_stats <- data.frame(
  w            = w_bins,
  n_valid      = n_valid_per_bin,
  ratio_median = apply(ratio_matrix, 2, median,   na.rm = TRUE),
  ratio_mean   = apply(ratio_matrix, 2, mean,     na.rm = TRUE),
  ratio_sd     = apply(ratio_matrix, 2, sd,       na.rm = TRUE),
  ratio_q25    = apply(ratio_matrix, 2, quantile, probs = 0.25, na.rm = TRUE),
  ratio_q75    = apply(ratio_matrix, 2, quantile, probs = 0.75, na.rm = TRUE),
  cv_noise     = cv_temporal
) %>%
  mutate(
    ratio_q25   = ifelse(n_valid >= min_valid_for_ribbon, ratio_q25, NA_real_),
    ratio_q75   = ifelse(n_valid >= min_valid_for_ribbon, ratio_q75, NA_real_),
    ratio_sd_lo = ifelse(n_valid >= min_valid_for_ribbon, pmax(ratio_mean - ratio_sd, 0), NA_real_),
    ratio_sd_hi = ifelse(n_valid >= min_valid_for_ribbon, ratio_mean + ratio_sd,          NA_real_)
  )

# --- species spectra cache + params (for dominance bands & whale troughs) ---
sp_cache       <- readRDS(species_cache_path)
sp_names       <- sp_cache$sp_names
fished_sp_list <- lapply(sp_cache$fished_sp, function(m) m[top_idx_valid, , drop = FALSE])
rm(sp_cache); gc()

params_obj <- readRDS(params_path)
dw         <- params_obj@dw
sp_wmax    <- setNames(mizer::species_params(params_obj)$w_max,
                       as.character(mizer::species_params(params_obj)$species))
rm(params_obj); gc()

find_sp <- function(pattern) {
  hit <- grep(pattern, sp_names, ignore.case = TRUE, value = TRUE)
  if (length(hit) == 0) stop(paste("No species matching:", pattern)); hit[1]
}
sp_minke  <- find_sp("minke");  sp_orca   <- find_sp("orca")
sp_sperm  <- find_sp("sperm");  sp_baleen <- find_sp("baleen")
fg_minke_max_raw  <- sp_wmax[sp_minke];  fg_orca_max_raw  <- sp_wmax[sp_orca]
fg_sperm_max_raw  <- sp_wmax[sp_sperm];  fg_baleen_max_raw <- sp_wmax[sp_baleen]
fg_baleen_upper   <- fg_baleen_max_raw * 1.15

w_below_1g <- w_bins[w_bins < 1]
w_min_plot <- if (length(w_below_1g) > 0) max(w_below_1g) else min(w_bins)
w_max_plot <- fg_baleen_upper * 1.1

x_breaks_shared <- c(1, 10, 100, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8)
x_labels_shared <- function(x) dplyr::case_when(
  x >= 1e6 ~ paste0(x / 1e6, " t"),
  x >= 1e3 ~ paste0(x / 1e3, " kg"),
  TRUE     ~ paste0(round(x, 0), " g"))

# --- dominance bands (identical logic to spectrum_ratio_slope_top10pct.R) ---
dominance_group_levels <- c(
  "Krill", "Salps", "Pelagic fishes", "Commercial fishes", "Squids",
  "Toothfishes", "Flying birds & penguins", "Seals", "Large marine mammals")

sp_to_dominance <- function(sp_name) dplyr::case_when(
  sp_name == "antarctic krill"                               ~ "Krill",
  sp_name == "salps"                                         ~ "Salps",
  sp_name %in% c("mesopelagic fishes", "bathypelagic fishes")~ "Pelagic fishes",
  sp_name == "shelf and coastal fishes"                      ~ "Commercial fishes",
  sp_name == "squids"                                        ~ "Squids",
  sp_name == "toothfishes"                                   ~ "Toothfishes",
  sp_name %in% c("flying birds", "small divers")             ~ "Flying birds & penguins",
  sp_name %in% c("medium divers", "large divers")            ~ "Seals",
  sp_name %in% c(sp_minke, sp_orca, sp_sperm, sp_baleen)     ~ "Large marine mammals",
  TRUE                                                       ~ NA_character_)

sp_dominance_groups <- setNames(vapply(sp_names, sp_to_dominance, character(1)), sp_names)
sp_in_dom           <- sp_names[!is.na(sp_dominance_groups)]
sp_dom_factor_clean <- factor(sp_dominance_groups[sp_in_dom], levels = dominance_group_levels)
names(sp_dom_factor_clean) <- sp_in_dom

compute_fg_biomass_df <- function(sp_list, sp_names_sel, sp_groups_vec, w_bins, dw, fg_levels) {
  sp_median_n <- sapply(sp_names_sel, function(sp) apply(sp_list[[sp]], 2, median, na.rm = TRUE))
  bm_mat      <- sweep(sp_median_n, 1, w_bins * dw, "*")
  fg_bm <- sapply(fg_levels, function(fg) {
    sp_in <- sp_names_sel[as.character(sp_groups_vec) == fg]
    if (length(sp_in) == 0) return(rep(0, length(w_bins)))
    if (length(sp_in) == 1) return(bm_mat[, sp_in])
    rowSums(bm_mat[, sp_in, drop = FALSE])
  })
  as.data.frame(fg_bm) %>% dplyr::mutate(w = w_bins) %>%
    tidyr::pivot_longer(-w, names_to = "functional_group", values_to = "biomass") %>%
    dplyr::mutate(functional_group = factor(functional_group, levels = fg_levels))
}
aggregate_to_log_bins <- function(bm_df, log_breaks) {
  bm_df %>% dplyr::filter(w > 0) %>%
    dplyr::mutate(bin_id = findInterval(log10(w), log_breaks, all.inside = TRUE),
                  bin_left = 10^log_breaks[bin_id], bin_right = 10^log_breaks[bin_id + 1],
                  bin_mid = sqrt(bin_left * bin_right)) %>%
    dplyr::group_by(bin_id, bin_left, bin_right, bin_mid, functional_group) %>%
    dplyr::summarise(biomass = sum(biomass, na.rm = TRUE), .groups = "drop")
}
w_pos             <- w_bins[w_bins > 0]
shared_log_breaks <- seq(log10(min(w_pos)), log10(max(w_pos)), length.out = 61)
bm_dominance   <- compute_fg_biomass_df(fished_sp_list, sp_in_dom, sp_dom_factor_clean,
                                        w_bins, dw, fg_levels = dominance_group_levels)
agg_dominance  <- aggregate_to_log_bins(bm_dominance, shared_log_breaks)
dominant_per_bin <- agg_dominance %>%
  dplyr::group_by(bin_id, bin_left, bin_right) %>%
  dplyr::slice_max(biomass, n = 1, with_ties = FALSE) %>% dplyr::ungroup() %>%
  dplyr::arrange(bin_left) %>%
  dplyr::select(bin_id, bin_left, bin_right, dominant_group = functional_group)
nn <- nrow(dominant_per_bin)
bands_raw <- dominant_per_bin %>%
  dplyr::mutate(run_id = cumsum(c(1L, as.integer(dominant_group[-nn] != dominant_group[-1])))) %>%
  dplyr::group_by(run_id, dominant_group) %>%
  dplyr::summarise(band_xmin = min(bin_left), band_xmax = max(bin_right), .groups = "drop") %>%
  dplyr::arrange(band_xmin)
mm_group     <- "Large marine mammals"
non_mm_bands <- bands_raw %>% dplyr::filter(dominant_group != mm_group)
mm_bands_raw <- bands_raw %>% dplyr::filter(dominant_group == mm_group)
if (nrow(mm_bands_raw) > 0) {
  mm_start     <- min(mm_bands_raw$band_xmin)
  non_mm_bands <- non_mm_bands %>% dplyr::mutate(band_xmax = pmin(band_xmax, mm_start)) %>%
    dplyr::filter(band_xmax > band_xmin)
  mm_band <- data.frame(band_xmin = mm_start, band_xmax = fg_baleen_upper,
                        dominant_group = mm_group, stringsAsFactors = FALSE)
  bands_fixed <- dplyr::bind_rows(non_mm_bands, mm_band) %>% dplyr::arrange(band_xmin)
} else bands_fixed <- non_mm_bands
if (nrow(bands_fixed) > 0) {
  bands_fixed$band_xmin[1]                 <- w_min_plot
  bands_fixed$band_xmax[nrow(bands_fixed)] <- w_max_plot
}

mm_fill  <- "#7bafd4"; mm_alpha <- 0.18
generate_band_annotations <- function(bands_df, y_label_pos = 1.24, y_min = 0) {
  grey_light <- "grey85"; grey_dark <- "grey70"; band_alpha_val <- 0.40
  short_labels <- c(
    "Krill" = "Krill", "Salps" = "Salps", "Pelagic fishes" = "Pelagic\nfishes",
    "Commercial fishes" = "Commercial\nfishes", "Squids" = "Squids",
    "Toothfishes" = "Toothfishes", "Flying birds & penguins" = "Flying birds\n& penguins",
    "Seals" = "Seals", "Large marine mammals" = "Large marine\nmammals")
  rect_annots <- list(); text_annots <- list()
  for (i in seq_len(nrow(bands_df))) {
    grp  <- as.character(bands_df$dominant_group[i])
    xmin <- bands_df$band_xmin[i]; xmax <- bands_df$band_xmax[i]
    if (!grp %in% names(short_labels)) next
    if (grp == mm_group) { fill_col <- mm_fill; fill_alpha <- mm_alpha * 1.4
    } else { fill_col <- if (i %% 2 == 1) grey_light else grey_dark; fill_alpha <- band_alpha_val }
    xmin_vis <- max(xmin, w_min_plot); xmid <- sqrt(xmin_vis * xmax)
    y_off <- if (grp == "Flying birds & penguins") -y_label_pos * 0.065 else 0
    rect_annots <- c(rect_annots, list(
      annotate("rect", xmin = xmin, xmax = xmax, ymin = y_min, ymax = Inf,
               fill = fill_col, alpha = fill_alpha)))
    text_annots <- c(text_annots, list(
      annotate("text", x = xmid, y = y_label_pos + y_off, label = short_labels[grp],
               size = 3.2,
               colour = "grey25", hjust = 0.5)))
  }
  c(rect_annots, text_annots)
}

compute_whale_annotations <- function(ratio_stats) {
  make_trough <- function(w, med, w_lo, w_hi) {
    idx <- which(w > w_lo & w < w_hi); if (length(idx) == 0) return(NULL)
    pos <- idx[which.min(med[idx])]; list(w = w[pos], y = med[pos])
  }
  mm_rows <- bands_fixed$dominant_group == mm_group
  if (!any(mm_rows)) return(list())
  whale_start <- min(bands_fixed$band_xmin[mm_rows])
  mk <- make_trough(ratio_stats$w, ratio_stats$ratio_median, whale_start * 0.8,      fg_minke_max_raw * 1.5)
  ok <- make_trough(ratio_stats$w, ratio_stats$ratio_median, fg_minke_max_raw * 1.5, fg_orca_max_raw  * 1.5)
  sp <- make_trough(ratio_stats$w, ratio_stats$ratio_median, fg_orca_max_raw  * 1.5, fg_sperm_max_raw * 1.5)
  bl <- make_trough(ratio_stats$w, ratio_stats$ratio_median, fg_sperm_max_raw * 1.5, fg_baleen_upper  * 1.5)
  annots <- list()
  if (!is.null(mk)) { lx <- mk$w / 2.0; annots <- c(annots, list(
    annotate("segment", x = lx, xend = mk$w, y = mk$y, yend = mk$y, colour = "grey30", linewidth = 0.5),
    annotate("text", x = lx, y = mk$y, label = "Minke whales", size = 3.2, hjust = 1, vjust = 0.5, colour = "grey20"))) }
  if (!is.null(ok)) { rx <- ok$w * 1.5; ry <- ok$y - 0.12; annots <- c(annots, list(
    annotate("segment", x = ok$w, xend = rx, y = ok$y, yend = ry, colour = "grey30", linewidth = 0.5),
    annotate("text", x = rx, y = ry, label = "Orca", size = 3.2, hjust = 0, vjust = 0.5, colour = "grey20"))) }
  if (!is.null(sp)) { lx <- sp$w / 2.0; annots <- c(annots, list(
    annotate("segment", x = lx, xend = sp$w, y = sp$y, yend = sp$y, colour = "grey30", linewidth = 0.5),
    annotate("text", x = lx, y = sp$y, label = "Sperm whales", size = 3.2, hjust = 1, vjust = 0.5, colour = "grey20"))) }
  if (!is.null(bl)) {
    # one-line label in the lower-right (below/right of the sperm label), with a
    # near-vertical leader up to the baleen trough (slight right-to-left lean)
    lbl_x <- bl$w * 1.04
    lbl_y <- 0.03
    annots <- c(annots, list(
    annotate("segment", x = lbl_x, xend = bl$w, y = lbl_y, yend = bl$y,
             colour = "grey30", linewidth = 0.5),
    annotate("text", x = lbl_x, y = lbl_y, label = "Large baleen whales",
             size = 3.2, hjust = 1, vjust = 0.5, colour = "grey20"))) }
  annots
}

# --- assemble ratio panel with +/-1 SD reference ---
y_limits_a    <- c(0, 1.65)
ribbon_colour <- "#e41a1c"
sd_line_col   <- "red"

ratio_data_a <- pair_ratio_stats %>% dplyr::filter(w >= w_min_plot)
band_annots  <- generate_band_annotations(bands_fixed,
                  y_label_pos = y_limits_a[2] * 0.955, y_min = y_limits_a[1])
whale_annots <- compute_whale_annotations(ratio_data_a)

# Representative scalar SD for the horizontal reference lines: median temporal
# CV (of the ensemble-mean unexploited trajectory) over the resolved size range.
sigma_ref <- median(ratio_data_a$cv_noise[is.finite(ratio_data_a$cv_noise) &
                                          ratio_data_a$n_valid >= min_valid_for_ribbon],
                    na.rm = TRUE)
sd_hi_line <- 1 + sigma_ref
sd_lo_line <- max(1 - sigma_ref, 0)
cat(sprintf("  Temporal-SD noise (sigma_ref) = %.3f -> lines at %.3f / %.3f\n",
            sigma_ref, sd_lo_line, sd_hi_line))

panel_ratio <- ggplot(ratio_data_a, aes(x = w)) +
  band_annots +
  whale_annots +
  geom_ribbon(aes(ymin = ratio_q25, ymax = ratio_q75), fill = ribbon_colour, alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.8) +
  geom_line(aes(y = ratio_median), colour = ribbon_colour, linewidth = 1.2) +
  scale_x_log10(labels = x_labels_shared, breaks = x_breaks_shared,
                limits = c(w_min_plot, w_max_plot), oob = scales::squish,
                expand = expansion(mult = 0)) +
  scale_y_continuous(limits = y_limits_a, oob = scales::squish,
                     breaks = scales::pretty_breaks(n = 6),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Body mass", y = "Abundance ratio (Exploited / Unexploited)") +
  theme_classic() +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 11),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
        plot.margin = margin(5.5, 20, 5.5, 5.5)) +
  coord_cartesian(clip = "off")

ggsave("spectrum_ratio_panelA_sd_reference.png",
       panel_ratio, width = 11, height = 6, dpi = 300, bg = "white")
cat("  Saved: spectrum_ratio_panelA_sd_reference.png\n\n")

# ---------------------------------------------------------------------------
# 8a-bis. EXPLORATORY variant: MEAN +/- SD of the abundance ratio (instead of
#         median + IQR), so panels a (yield) and b (ratio) share a mean+/-SD
#         central+spread metric. The +/-1 SD horizontal lines remain the temporal
#         natural-variability (emergence) threshold -- a different SD from the
#         ribbon (which is the across-member spread of the ratio).
# ---------------------------------------------------------------------------
cat("Building mean+/-SD ratio panel (exploratory)...\n")
cat(sprintf("  ratio_mean range in plot: %.2f - %.2f (max mean+SD = %.2f)\n",
            min(ratio_data_a$ratio_mean, na.rm = TRUE),
            max(ratio_data_a$ratio_mean, na.rm = TRUE),
            max(ratio_data_a$ratio_sd_hi, na.rm = TRUE)))

whale_annots_mean <- compute_whale_annotations(
  ratio_data_a %>% dplyr::mutate(ratio_median = ratio_mean))

panel_ratio_meansd <- ggplot(ratio_data_a, aes(x = w)) +
  band_annots +
  whale_annots_mean +
  geom_ribbon(aes(ymin = ratio_sd_lo, ymax = ratio_sd_hi),
              fill = ribbon_colour, alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.8) +
  geom_hline(yintercept = c(sd_lo_line, sd_hi_line), linetype = "dashed",
             colour = sd_line_col, linewidth = 0.6) +
  geom_line(aes(y = ratio_mean), colour = ribbon_colour, linewidth = 1.2) +
  scale_x_log10(labels = x_labels_shared, breaks = x_breaks_shared,
                limits = c(w_min_plot, w_max_plot), oob = scales::squish,
                expand = expansion(mult = 0)) +
  scale_y_continuous(limits = y_limits_a, oob = scales::squish,
                     breaks = scales::pretty_breaks(n = 6),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Body mass", y = "Abundance ratio (Exploited / Unexploited)") +
  annotate("text", x = w_max_plot * 4.0, y = y_limits_a[2] * 0.955,
           label = "Dominant\nbiomass", size = 4.5, colour = "grey20",
           hjust = 0, vjust = 0.5, fontface = "italic") +
  annotate("text", x = w_max_plot * 4.0, y = sd_hi_line,
           label = "+1 SD", size = 3.2, colour = sd_line_col, hjust = 0, vjust = 0.5) +
  annotate("text", x = w_max_plot * 4.0, y = sd_lo_line,
           label = "-1 SD", size = 3.2, colour = sd_line_col, hjust = 0, vjust = 0.5) +
  theme_classic() +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 11),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
        plot.margin = margin(5.5, 100, 5.5, 5.5)) +
  coord_cartesian(clip = "off")

ggsave("spectrum_ratio_meanSD_panelA.png", panel_ratio_meansd,
       width = 11, height = 6, dpi = 300, bg = "white")
cat("  Saved: spectrum_ratio_meanSD_panelA.png\n\n")

# ---------------------------------------------------------------------------
# 8b. SNR variant (fully consistent metric with the biomass/slope SNR figures)
#     signal(w) = median_i [ Exploited_i(w) - Unexploited_i(w) ]  (paired diff)
#     noise(w)  = temporal SD of the ENSEMBLE-MEAN unexploited spectrum, 1841-2010
#     SNR(w)    = signal / noise ; emergence at |SNR| > 1 (also > 2).
#     Ref and temporal caches share the size grid, so absolute units are
#     directly comparable (verified: identical w).
# ---------------------------------------------------------------------------
cat("Building SNR panel...\n")
THRESH1 <- 1; THRESH2 <- 2

diff_matrix <- fished_top - climate_top                       # members x w (ref grid)
noise_abs   <- apply(tcache$Nbar_clim, 2, sd, na.rm = TRUE)   # temporal SD (temporal grid)
noise_ref   <- noise_abs[vapply(w_bins,
                          function(w) which.min(abs(tcache$w_bins - w)), integer(1))]
noise_ref[!is.finite(noise_ref) | noise_ref <= 0] <- NA

snr_stats <- data.frame(
  w       = w_bins,
  n_valid = n_valid_per_bin,
  snr_med = apply(diff_matrix, 2, median,   na.rm = TRUE)       / noise_ref,
  snr_q25 = apply(diff_matrix, 2, quantile, 0.25, na.rm = TRUE) / noise_ref,
  snr_q75 = apply(diff_matrix, 2, quantile, 0.75, na.rm = TRUE) / noise_ref
) %>%
  mutate(
    snr_q25 = ifelse(n_valid >= min_valid_for_ribbon, snr_q25, NA_real_),
    snr_q75 = ifelse(n_valid >= min_valid_for_ribbon, snr_q75, NA_real_)
  ) %>%
  dplyr::filter(w >= w_min_plot)

# y-limits span the full IQR ribbon so nothing plots off the margins (keeping
# +/-1 and +/-2 reference lines within range).
.rib   <- c(snr_stats$snr_q25, snr_stats$snr_q75)
.rib   <- .rib[is.finite(.rib)]
snr_lo <- min(floor(min(.rib)), -2.5)
snr_hi <- max(ceiling(max(.rib)), 2.5)
cat(sprintf("  SNR median range %.1f..%.1f ; plotted ylim %.0f..%.0f\n",
            min(snr_stats$snr_med, na.rm = TRUE), max(snr_stats$snr_med, na.rm = TRUE),
            snr_lo, snr_hi))

band_annots_snr <- generate_band_annotations(
  bands_fixed, y_label_pos = snr_hi - 0.06 * (snr_hi - snr_lo), y_min = snr_lo)

panel_snr <- ggplot(snr_stats, aes(x = w)) +
  band_annots_snr +
  geom_ribbon(aes(ymin = snr_q25, ymax = snr_q75), fill = ribbon_colour, alpha = 0.3) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
  geom_hline(yintercept = c(-THRESH1, THRESH1), linetype = "dashed",
             colour = sd_line_col, linewidth = 0.6) +
  geom_hline(yintercept = c(-THRESH2, THRESH2), linetype = "dotted",
             colour = "grey45", linewidth = 0.5) +
  geom_line(aes(y = snr_med), colour = ribbon_colour, linewidth = 1.2) +
  scale_x_log10(labels = x_labels_shared, breaks = x_breaks_shared,
                limits = c(w_min_plot, w_max_plot), oob = scales::squish,
                expand = expansion(mult = 0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
                     expand = expansion(mult = c(0.02, 0.02))) +
  labs(x = "Body mass",
       y = "Abundance SNR (Exploited - Unexploited)/SD") +
  annotate("text", x = w_max_plot * 4.0, y = snr_hi - 0.06 * (snr_hi - snr_lo),
           label = "Dominant\nbiomass", size = 4.5, colour = "grey20",
           hjust = 0, vjust = 0.5, fontface = "italic") +
  annotate("text", x = w_max_plot * 4.0, y = THRESH1,
           label = "+1 SD", size = 3.2, colour = sd_line_col, hjust = 0, vjust = 0.5) +
  annotate("text", x = w_max_plot * 4.0, y = -THRESH1,
           label = "-1 SD", size = 3.2, colour = sd_line_col, hjust = 0, vjust = 0.5) +
  theme_classic() +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 11),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
        plot.margin = margin(5.5, 100, 5.5, 5.5)) +
  coord_cartesian(ylim = c(snr_lo, snr_hi), clip = "off")

ggsave("spectrum_snr_panelA.png", panel_snr,
       width = 11, height = 6, dpi = 300, bg = "white")
cat("  Saved: spectrum_snr_panelA.png\n\n")

# direct side-by-side comparison: ratio (a, top) vs SNR (b, bottom), shared x
fig_ratio_vs_snr <- (panel_ratio + labs(tag = "a")) /
                    (panel_snr   + labs(tag = "b")) &
  theme(plot.tag = element_text(size = 14, face = "bold"))
ggsave("spectrum_ratio_vs_snr.png", fig_ratio_vs_snr,
       width = 12, height = 11, dpi = 300, bg = "white")
cat("  Saved: spectrum_ratio_vs_snr.png\n\n")

# ===========================================================================
# PART 3 -- COMBINED LAYOUTS
# ===========================================================================
cat("--- Composing combined layouts ---\n")

tag_theme <- theme(plot.tag = element_text(size = 14, face = "bold"))

# Manual tags on the two main panels only. (plot_annotation(tag_levels=) recurses
# into the inset and would mis-tag it, so we tag explicitly instead.)
panel_yield_a <- panel_yield + labs(tag = "a") + obs_inset
panel_ratio_b <- panel_ratio + labs(tag = "b")

# Species legend now lives inside panel a (bottom-right inset), so we do NOT
# collect guides -- each panel keeps its own (ratio panel has none).
# 2 col x 1 row: yield left, ratio right
fig_2col <- (panel_yield_a | panel_ratio_b) & tag_theme
ggsave("yield_ratio_combined_2col.png", fig_2col,
       width = 17.0, height = 6.0, dpi = 300, bg = "white")
cat("  Saved: yield_ratio_combined_2col.png\n")

# 1 col x 2 row: yield top, ratio bottom
fig_2row <- (panel_yield_a / panel_ratio_b) & tag_theme
ggsave("yield_ratio_combined_2row.png", fig_2row,
       width = 10.2, height = 10.4, dpi = 300, bg = "white")
cat("  Saved: yield_ratio_combined_2row.png\n")

# SNR-metric variants of the combined figure (yield a, size-spectrum SNR b)
panel_snr_b  <- panel_snr + labs(tag = "b")
fig_snr_2col <- (panel_yield_a | panel_snr_b) & tag_theme
ggsave("yield_snr_combined_2col.png", fig_snr_2col,
       width = 20, height = 7.5, dpi = 300, bg = "white")
cat("  Saved: yield_snr_combined_2col.png\n")
fig_snr_2row <- (panel_yield_a / panel_snr_b) & tag_theme
ggsave("yield_snr_combined_2row.png", fig_snr_2row,
       width = 12, height = 13, dpi = 300, bg = "white")
cat("  Saved: yield_snr_combined_2row.png\n")

# mean+/-SD ratio variants (yield mean+/-SD in a, ratio mean+/-SD in b)
panel_ratio_meansd_b <- panel_ratio_meansd + labs(tag = "b")
fig_msd_2col <- (panel_yield_a | panel_ratio_meansd_b) & tag_theme
ggsave("yield_ratio_meanSD_combined_2col.png", fig_msd_2col,
       width = 20, height = 7.5, dpi = 300, bg = "white")
cat("  Saved: yield_ratio_meanSD_combined_2col.png\n")
fig_msd_2row <- (panel_yield_a / panel_ratio_meansd_b) & tag_theme
ggsave("yield_ratio_meanSD_combined_2row.png", fig_msd_2row,
       width = 12, height = 13, dpi = 300, bg = "white")
cat("  Saved: yield_ratio_meanSD_combined_2row.png\n")

cat("\n=== Done ===\n")