# =============================================================================
# spectrum_ratio_slope_top10pct.R
#
# Two-panel publication figure for the top-10% RMSE fishing ensemble:
#
#   a) Community size spectrum ratio (Exploited / Unexploited),
#      Reference period 2001-2010. Styled to match v6 (dominance bands,
#      whale annotations, IQR ribbon across top-10% matched pairs).
#
#   b) Timeseries of biomass-weighted community size spectrum slope from
#      1901 onward. Mean line with min/max ribbon for both Exploited
#      (Fished) and Unexploited (Climate-only) top-10% ensembles.
#
# Caches:
#   - Reads  : ecosystem_assessment_outputs/abundance_ratio_plots/
#              spectra_cache_ref_period.rds  (community spectrum matrices)
#              spectra_cache_species_ref_period.rds  (per-species matrices)
#   - Reads  : Output_large_files/community_slope_analysis/
#              community_slope_full_2111_data.rds  (per-sim annual slopes)
#   - Reads  : yield_rmse_per_sim.csv  (RMSE rankings)
#   - Writes : ecosystem_assessment_outputs/abundance_ratio_plots/
#              spectrum_ratio_slope_top10pct.png
#              spectrum_ratio_slope_top10pct_ratio_data.csv
#              spectrum_ratio_slope_top10pct_slope_data.csv
# =============================================================================

suppressPackageStartupMessages({
  library(mizer)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

cat("=== Spectrum ratio + slope timeseries (top-10% RMSE ensemble) ===\n\n")

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
output_dir     <- "ecosystem_assessment_outputs/abundance_ratio_plots"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

spectra_cache_path  <- file.path(output_dir, "spectra_cache_ref_period.rds")
species_cache_path  <- file.path(output_dir, "spectra_cache_species_ref_period.rds")
slope_data_path     <- "Output_large_files/community_slope_analysis/nbss_slope_top10pct_data.rds"
rmse_path           <- "yield_rmse_per_sim.csv"
params_path         <- "params_sel_adj.rds"

# ---------------------------------------------------------------------------
# 1. Load RMSE rankings — determine top-10% indices
# ---------------------------------------------------------------------------
cat("Loading RMSE rankings...\n")
if (!file.exists(rmse_path)) stop("yield_rmse_per_sim.csv not found — run yield_rmse_evaluation.R first.")

rmse_df      <- read.csv(rmse_path) %>% arrange(rank)
n_sims       <- nrow(rmse_df)
n_top10pct   <- ceiling(n_sims * 0.10)
top10pct_idx <- rmse_df$sim_index[1:n_top10pct]
message(sprintf("  Top-10%% subset: n = %d sims (RMSE %.4f – %.4f)\n",
                n_top10pct, rmse_df$rmse[1], rmse_df$rmse[n_top10pct]))

# ---------------------------------------------------------------------------
# 2. Load community spectra cache — subset to top-10%
# ---------------------------------------------------------------------------
cat("Loading community spectra cache...\n")
if (!file.exists(spectra_cache_path))
  stop("spectra_cache_ref_period.rds not found — run create_fished_vs_unfished_ratio_plot_FIXED_v6.R first.")

cache        <- readRDS(spectra_cache_path)
w_bins       <- cache$w_bins
n_w          <- cache$n_w

# Subset rows to top-10% indices (clamp to valid range)
top_idx_valid <- top10pct_idx[top10pct_idx <= nrow(cache$fished_spectra)]
fished_top    <- cache$fished_spectra[top_idx_valid, , drop = FALSE]
climate_top   <- cache$climate_spectra[top_idx_valid, , drop = FALSE]
rm(cache); gc()

cat(sprintf("  Fished top-10%%:       %d rows × %d size bins\n", nrow(fished_top),  n_w))
cat(sprintf("  Climate-only top-10%%: %d rows × %d size bins\n", nrow(climate_top), n_w))

# Per-pair ratio matrix (matched: same row = same parameter draw)
ratio_matrix <- fished_top / climate_top
ratio_matrix[!is.finite(ratio_matrix)] <- NA

# Count valid (non-NA) pairs per size bin — used to gate the IQR ribbon
n_valid_per_bin <- apply(ratio_matrix, 2, function(x) sum(is.finite(x)))
cat(sprintf("  Valid pairs per bin: min=%d, median=%.0f, max=%d (of %d total)\n",
            min(n_valid_per_bin), median(n_valid_per_bin),
            max(n_valid_per_bin), nrow(ratio_matrix)))

# Minimum threshold for drawing the IQR ribbon (require >=50% valid pairs)
min_valid_for_ribbon <- ceiling(nrow(ratio_matrix) * 0.50)

pair_ratio_stats <- data.frame(
  w            = w_bins,
  n_valid      = n_valid_per_bin,
  ratio_median = apply(ratio_matrix, 2, median,   na.rm = TRUE),
  ratio_mean   = apply(ratio_matrix, 2, mean,     na.rm = TRUE),
  ratio_sd     = apply(ratio_matrix, 2, sd,        na.rm = TRUE),
  ratio_q05    = apply(ratio_matrix, 2, quantile, probs = 0.05, na.rm = TRUE),
  ratio_q25    = apply(ratio_matrix, 2, quantile, probs = 0.25, na.rm = TRUE),
  ratio_q75    = apply(ratio_matrix, 2, quantile, probs = 0.75, na.rm = TRUE),
  ratio_q95    = apply(ratio_matrix, 2, quantile, probs = 0.95, na.rm = TRUE)
) %>%
  # Suppress IQR ribbon where too few valid pairs exist
  dplyr::mutate(
    ratio_q25 = ifelse(n_valid >= min_valid_for_ribbon, ratio_q25, NA_real_),
    ratio_q75 = ifelse(n_valid >= min_valid_for_ribbon, ratio_q75, NA_real_)
  )

write.csv(pair_ratio_stats,
          file.path(output_dir, "spectrum_ratio_slope_top10pct_ratio_data.csv"),
          row.names = FALSE)
cat("  Ratio data saved.\n")

# ---------------------------------------------------------------------------
# 3. Load species spectra cache — subset to top-10%
# ---------------------------------------------------------------------------
cat("Loading species spectra cache...\n")
if (!file.exists(species_cache_path))
  stop("spectra_cache_species_ref_period.rds not found — run create_fished_vs_unfished_ratio_plot_FIXED_v6.R first.")

sp_cache       <- readRDS(species_cache_path)
sp_names       <- sp_cache$sp_names
fished_sp_list <- lapply(sp_cache$fished_sp, function(m) m[top_idx_valid, , drop = FALSE])
rm(sp_cache); gc()
cat(sprintf("  Species: %s\n", paste(sp_names, collapse = ", ")))

# ---------------------------------------------------------------------------
# 4. Load model params — w_max per species and dw
# ---------------------------------------------------------------------------
cat("Loading model params...\n")
if (!file.exists(params_path)) stop("params_sel_adj.rds not found.")
params_obj <- readRDS(params_path)
dw         <- params_obj@dw
sp_wmax    <- setNames(
  mizer::species_params(params_obj)$w_max,
  as.character(mizer::species_params(params_obj)$species)
)
rm(params_obj); gc()

find_sp <- function(pattern) {
  hit <- grep(pattern, sp_names, ignore.case = TRUE, value = TRUE)
  if (length(hit) == 0) stop(paste("No species matching:", pattern))
  hit[1]
}
sp_minke  <- find_sp("minke")
sp_orca   <- find_sp("orca")
sp_sperm  <- find_sp("sperm")
sp_baleen <- find_sp("baleen")

fg_minke_max_raw  <- sp_wmax[sp_minke]
fg_orca_max_raw   <- sp_wmax[sp_orca]
fg_sperm_max_raw  <- sp_wmax[sp_sperm]
fg_baleen_max_raw <- sp_wmax[sp_baleen]
fg_baleen_upper   <- fg_baleen_max_raw * 1.15

# Plot x-axis limits
w_below_1g   <- w_bins[w_bins < 1]
w_min_plot   <- if (length(w_below_1g) > 0) max(w_below_1g) else min(w_bins)
w_max_plot   <- fg_baleen_upper * 1.1

# Shared x-axis config
x_breaks_shared <- c(1, 100, 1e3, 1e5, 1e6, 1e7, 1e8)
x_labels_shared <- function(x) {
  dplyr::case_when(
    x >= 1e6 ~ paste0(x / 1e6, " t"),
    x >= 1e3 ~ paste0(x / 1e3, " kg"),
    TRUE     ~ paste0(round(x, 0), " g")
  )
}

# ---------------------------------------------------------------------------
# 5. Compute dominance bands from species spectra
# ---------------------------------------------------------------------------
cat("Computing data-driven dominance bands...\n")

dominance_group_levels <- c(
  "Krill", "Salps", "Pelagic fishes", "Commercial fishes",
  "Squids", "Toothfishes", "Flying birds & penguins", "Seals",
  "Large marine mammals"
)

sp_to_dominance <- function(sp_name) {
  dplyr::case_when(
    sp_name == "antarctic krill"                              ~ "Krill",
    sp_name == "salps"                                        ~ "Salps",
    sp_name %in% c("mesopelagic fishes", "bathypelagic fishes") ~ "Pelagic fishes",
    sp_name == "shelf and coastal fishes"                     ~ "Commercial fishes",
    sp_name == "squids"                                       ~ "Squids",
    sp_name == "toothfishes"                                  ~ "Toothfishes",
    sp_name %in% c("flying birds", "small divers")            ~ "Flying birds & penguins",
    sp_name %in% c("medium divers", "large divers")           ~ "Seals",
    sp_name %in% c(sp_minke, sp_orca, sp_sperm, sp_baleen)   ~ "Large marine mammals",
    TRUE                                                      ~ NA_character_
  )
}

sp_dominance_groups <- setNames(vapply(sp_names, sp_to_dominance, character(1)), sp_names)
sp_in_dom           <- sp_names[!is.na(sp_dominance_groups)]
sp_dom_factor_clean <- factor(sp_dominance_groups[sp_in_dom], levels = dominance_group_levels)
names(sp_dom_factor_clean) <- sp_in_dom

# Helper: biomass df per functional group
compute_fg_biomass_df <- function(sp_list, sp_names_sel, sp_groups_vec, w_bins, dw, fg_levels) {
  sp_median_n <- sapply(sp_names_sel, function(sp) apply(sp_list[[sp]], 2, median, na.rm = TRUE))
  bm_mat      <- sweep(sp_median_n, 1, w_bins * dw, "*")
  fg_bm <- sapply(fg_levels, function(fg) {
    sp_in <- sp_names_sel[as.character(sp_groups_vec) == fg]
    if (length(sp_in) == 0) return(rep(0, length(w_bins)))
    if (length(sp_in) == 1) return(bm_mat[, sp_in])
    rowSums(bm_mat[, sp_in, drop = FALSE])
  })
  as.data.frame(fg_bm) %>%
    dplyr::mutate(w = w_bins) %>%
    tidyr::pivot_longer(-w, names_to = "functional_group", values_to = "biomass") %>%
    dplyr::mutate(functional_group = factor(functional_group, levels = fg_levels))
}

# Helper: aggregate to log bins
aggregate_to_log_bins <- function(bm_df, log_breaks) {
  bm_df %>%
    dplyr::filter(w > 0) %>%
    dplyr::mutate(
      bin_id    = findInterval(log10(w), log_breaks, all.inside = TRUE),
      bin_left  = 10^log_breaks[bin_id],
      bin_right = 10^log_breaks[bin_id + 1],
      bin_mid   = sqrt(bin_left * bin_right)
    ) %>%
    dplyr::group_by(bin_id, bin_left, bin_right, bin_mid, functional_group) %>%
    dplyr::summarise(biomass = sum(biomass, na.rm = TRUE), .groups = "drop")
}

# Build log bins consistent with v6
w_pos             <- w_bins[w_bins > 0]
shared_log_breaks <- seq(log10(min(w_pos)), log10(max(w_pos)), length.out = 61)

bm_dominance   <- compute_fg_biomass_df(fished_sp_list, sp_in_dom, sp_dom_factor_clean,
                                         w_bins, dw, fg_levels = dominance_group_levels)
agg_dominance  <- aggregate_to_log_bins(bm_dominance, shared_log_breaks)

# Dominant group per bin
dominant_per_bin <- agg_dominance %>%
  dplyr::group_by(bin_id, bin_left, bin_right) %>%
  dplyr::slice_max(biomass, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(bin_left) %>%
  dplyr::select(bin_id, bin_left, bin_right, dominant_group = functional_group)

# Merge consecutive same-group bins
nn       <- nrow(dominant_per_bin)
bands_raw <- dominant_per_bin %>%
  dplyr::mutate(
    run_id = cumsum(c(1L, as.integer(dominant_group[-nn] != dominant_group[-1])))
  ) %>%
  dplyr::group_by(run_id, dominant_group) %>%
  dplyr::summarise(band_xmin = min(bin_left), band_xmax = max(bin_right), .groups = "drop") %>%
  dplyr::arrange(band_xmin)

# Collapse large marine mammals into one block
mm_group      <- "Large marine mammals"
non_mm_bands  <- bands_raw %>% dplyr::filter(dominant_group != mm_group)
mm_bands_raw  <- bands_raw %>% dplyr::filter(dominant_group == mm_group)

if (nrow(mm_bands_raw) > 0) {
  mm_start     <- min(mm_bands_raw$band_xmin)
  non_mm_bands <- non_mm_bands %>%
    dplyr::mutate(band_xmax = pmin(band_xmax, mm_start)) %>%
    dplyr::filter(band_xmax > band_xmin)
  mm_band <- data.frame(band_xmin = mm_start, band_xmax = fg_baleen_upper,
                         dominant_group = mm_group, stringsAsFactors = FALSE)
  bands_fixed <- dplyr::bind_rows(non_mm_bands, mm_band) %>% dplyr::arrange(band_xmin)
} else {
  bands_fixed <- non_mm_bands
}

# Extend edges to fill x-axis range
if (nrow(bands_fixed) > 0) {
  bands_fixed$band_xmin[1]                <- w_min_plot
  bands_fixed$band_xmax[nrow(bands_fixed)] <- w_max_plot
}

cat("  Dominance bands computed.\n")

# ---------------------------------------------------------------------------
# 6. Band annotation generator (identical to v6)
# ---------------------------------------------------------------------------
mm_fill      <- "#7bafd4"
mm_alpha     <- 0.18

generate_band_annotations <- function(bands_df, y_label_pos = 1.24, y_min = 0) {
  grey_light      <- "grey85"
  grey_dark       <- "grey70"
  band_alpha_val  <- 0.40

  short_labels <- c(
    "Krill"                   = "Krill",
    "Salps"                   = "Salps",
    "Pelagic fishes"          = "Pelagic\nfishes",
    "Commercial fishes"       = "Commercial\nfishes",
    "Squids"                  = "Squids",
    "Toothfishes"             = "Toothfishes",
    "Flying birds & penguins" = "Flying birds\n& penguins",
    "Seals"                   = "Seals",
    "Large marine mammals"    = "Large marine\nmammals"
  )

  rect_annots <- list()
  text_annots <- list()

  for (i in seq_len(nrow(bands_df))) {
    grp  <- as.character(bands_df$dominant_group[i])
    xmin <- bands_df$band_xmin[i]
    xmax <- bands_df$band_xmax[i]
    if (!grp %in% names(short_labels)) next

    if (grp == mm_group) {
      fill_col   <- mm_fill
      fill_alpha <- mm_alpha * 1.4
    } else {
      fill_col   <- if (i %% 2 == 1) grey_light else grey_dark
      fill_alpha <- band_alpha_val
    }

    xmin_vis <- max(xmin, w_min_plot)
    xmid     <- sqrt(xmin_vis * xmax)
    y_off    <- if (grp == "Flying birds & penguins") -y_label_pos * 0.065 else 0

    rect_annots <- c(rect_annots, list(
      annotate("rect", xmin = xmin, xmax = xmax, ymin = y_min, ymax = Inf,
               fill = fill_col, alpha = fill_alpha)
    ))
    text_annots <- c(text_annots, list(
      annotate("text", x = xmid, y = y_label_pos + y_off,
               label = short_labels[grp],
               size  = ifelse(grp %in% c("Large marine mammals", "Flying birds & penguins"), 2.7, 3.0),
               colour = "grey25", hjust = 0.5)
    ))
  }
  c(rect_annots, text_annots)
}

# ---------------------------------------------------------------------------
# 7. Whale trough annotations (identical to v6)
# ---------------------------------------------------------------------------
compute_whale_annotations <- function(ratio_stats) {
  make_trough <- function(w, med, w_lo, w_hi) {
    idx <- which(w > w_lo & w < w_hi)
    if (length(idx) == 0) return(NULL)
    pos <- idx[which.min(med[idx])]
    list(w = w[pos], y = med[pos])
  }

  mm_rows     <- bands_fixed$dominant_group == mm_group
  if (!any(mm_rows)) return(list())
  whale_start <- min(bands_fixed$band_xmin[mm_rows])

  mk <- make_trough(ratio_stats$w, ratio_stats$ratio_median,
                    whale_start * 0.8,        fg_minke_max_raw  * 1.5)
  ok <- make_trough(ratio_stats$w, ratio_stats$ratio_median,
                    fg_minke_max_raw  * 1.5,  fg_orca_max_raw   * 1.5)
  sp <- make_trough(ratio_stats$w, ratio_stats$ratio_median,
                    fg_orca_max_raw   * 1.5,  fg_sperm_max_raw  * 1.5)
  bl <- make_trough(ratio_stats$w, ratio_stats$ratio_median,
                    fg_sperm_max_raw  * 1.5,  fg_baleen_upper   * 1.5)

  annots <- list()

  if (!is.null(mk)) {
    lx <- mk$w / 2.0
    annots <- c(annots, list(
      annotate("segment", x = lx, xend = mk$w, y = mk$y, yend = mk$y,
               colour = "grey30", linewidth = 0.5),
      annotate("text", x = lx, y = mk$y, label = "Minke whales",
               size = 3.2, hjust = 1, vjust = 0.5, colour = "grey20")
    ))
  }
  if (!is.null(ok)) {
    rx <- ok$w * 1.5; ry <- ok$y + 0.08
    annots <- c(annots, list(
      annotate("segment", x = ok$w, xend = rx, y = ok$y, yend = ry,
               colour = "grey30", linewidth = 0.5),
      annotate("text", x = rx, y = ry, label = "Orca",
               size = 3.2, hjust = 0, vjust = 0.5, colour = "grey20")
    ))
  }
  if (!is.null(sp)) {
    lx <- sp$w / 2.0
    annots <- c(annots, list(
      annotate("segment", x = lx, xend = sp$w, y = sp$y, yend = sp$y,
               colour = "grey30", linewidth = 0.5),
      annotate("text", x = lx, y = sp$y, label = "Sperm whales",
               size = 3.2, hjust = 1, vjust = 0.5, colour = "grey20")
    ))
  }
  if (!is.null(bl)) {
    rx_line <- bl$w * 1.15; ry_line <- bl$y + 0.15
    rx_lbl  <- bl$w * 1.18; ry_lbl  <- bl$y + 0.17
    annots <- c(annots, list(
      annotate("segment", x = bl$w, xend = rx_line, y = bl$y, yend = ry_line,
               colour = "grey30", linewidth = 0.5),
      annotate("text", x = rx_lbl, y = ry_lbl, label = "Baleen\nwhales",
               size = 3.2, hjust = 0, vjust = 0.5, colour = "grey20")
    ))
  }

  if (!is.null(mk)) cat(sprintf("  Minke  trough: %.1f t  ratio = %.3f\n", mk$w/1e6, mk$y))
  if (!is.null(ok)) cat(sprintf("  Orca   trough: %.1f t  ratio = %.3f\n", ok$w/1e6, ok$y))
  if (!is.null(sp)) cat(sprintf("  Sperm  trough: %.1f t  ratio = %.3f\n", sp$w/1e6, sp$y))
  if (!is.null(bl)) cat(sprintf("  Baleen trough: %.1f t  ratio = %.3f\n", bl$w/1e6, bl$y))

  annots
}

# ---------------------------------------------------------------------------
# 8. Build Panel A — Ratio plot
# ---------------------------------------------------------------------------
cat("\nBuilding Panel A (ratio plot)...\n")

y_limits_a   <- c(0, 1.65)
ribbon_colour <- "#e41a1c"

ratio_data_a <- pair_ratio_stats %>% dplyr::filter(w >= w_min_plot)
band_annots  <- generate_band_annotations(bands_fixed,
                  y_label_pos = y_limits_a[2] * 0.955, y_min = y_limits_a[1])
whale_annots <- compute_whale_annotations(ratio_data_a)

panel_a <- ggplot(ratio_data_a, aes(x = w)) +
  band_annots +
  whale_annots +
  geom_ribbon(aes(ymin = ratio_q25, ymax = ratio_q75),
              fill = ribbon_colour, alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.8) +
  geom_line(aes(y = ratio_median), colour = ribbon_colour, linewidth = 1.2) +
  scale_x_log10(
    labels = x_labels_shared, breaks = x_breaks_shared,
    limits = c(w_min_plot, w_max_plot),
    oob    = scales::squish,
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    limits = y_limits_a,
    oob    = scales::squish,
    breaks = scales::pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(x = "Body mass", y = "Abundance ratio (Exploited / Unexploited)") +
  annotate("text",
           x = w_max_plot * 4.0, y = y_limits_a[2] * 0.955,
           label = "Dominant\nbiomass",
           size = 4.5, colour = "grey20",
           hjust = 0, vjust = 0.5, fontface = "italic") +
  theme_classic() +
  theme(
    axis.title         = element_text(size = 12),
    axis.text          = element_text(size = 11),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    plot.margin        = margin(5.5, 100, 5.5, 5.5)
  ) +
  coord_cartesian(clip = "off")

cat("  Panel A built.\n")

# ---------------------------------------------------------------------------
# 9. Load per-sim annual slope data — filter to top-10%
# ---------------------------------------------------------------------------
cat("\nLoading community slope data...\n")
if (!file.exists(slope_data_path))
  stop("nbss_slope_top10pct_data.rds not found — run run_nbss_slope_top10pct.R first.")

slope_cache  <- readRDS(slope_data_path)
all_slopes   <- slope_cache$all_slopes
rm(slope_cache); gc()

cat(sprintf("  Loaded %d slope records. Year range: %d – %d\n",
            nrow(all_slopes), min(all_slopes$time), max(all_slopes$time)))

# Filter: top-10% sims, years >= 1901, biomass spectrum
slope_top10 <- all_slopes %>%
  dplyr::filter(
    sim_id        %in% top10pct_idx,
    time          >= 1901,
    spectrum_type == "Biomass"
  )

cat(sprintf("  After filter: %d records across %d sims, years %d – %d\n",
            nrow(slope_top10),
            dplyr::n_distinct(slope_top10$sim_id),
            min(slope_top10$time), max(slope_top10$time)))

# Annual mean, min, max across top-10% sims
slope_summary_top10 <- slope_top10 %>%
  dplyr::group_by(ensemble, time) %>%
  dplyr::summarise(
    slope_mean   = mean(slope,   na.rm = TRUE),
    slope_median = median(slope, na.rm = TRUE),
    slope_sd     = sd(slope,     na.rm = TRUE),
    slope_min    = min(slope,    na.rm = TRUE),
    slope_max    = max(slope,    na.rm = TRUE),
    slope_q25    = quantile(slope, 0.25, na.rm = TRUE),
    slope_q75    = quantile(slope, 0.75, na.rm = TRUE),
    n_sims       = dplyr::n(),
    .groups      = "drop"
  )

write.csv(slope_summary_top10,
          file.path(output_dir, "spectrum_ratio_slope_top10pct_slope_data.csv"),
          row.names = FALSE)
cat("  Slope summary saved.\n")

# ---------------------------------------------------------------------------
# 9b. Paired differences: Exploited − Unexploited, matched by sim_id × time
# ---------------------------------------------------------------------------
cat("\nComputing paired slope differences...\n")

slope_paired <- slope_top10 %>%
  dplyr::select(sim_id, time, ensemble, slope) %>%
  tidyr::pivot_wider(names_from = ensemble, values_from = slope,
                     values_fn  = mean) %>%
  dplyr::mutate(slope_diff = Exploited - Unexploited) %>%
  dplyr::filter(!is.na(slope_diff))

slope_diff_summary <- slope_paired %>%
  dplyr::group_by(time) %>%
  dplyr::summarise(
    diff_median = median(slope_diff, na.rm = TRUE),
    diff_q25    = quantile(slope_diff, 0.25, na.rm = TRUE),
    diff_q75    = quantile(slope_diff, 0.75, na.rm = TRUE),
    diff_q05    = quantile(slope_diff, 0.05, na.rm = TRUE),
    diff_q95    = quantile(slope_diff, 0.95, na.rm = TRUE),
    n_pairs     = dplyr::n(),
    .groups     = "drop"
  )

cat(sprintf("  Paired diff: %d time points, median n per year = %d\n",
            nrow(slope_diff_summary), round(median(slope_diff_summary$n_pairs))))

write.csv(slope_diff_summary,
          file.path(output_dir, "spectrum_ratio_slope_top10pct_paired_diff_data.csv"),
          row.names = FALSE)
cat("  Paired diff data saved.\n")

# ---------------------------------------------------------------------------
# 10. Build Panel B — Slope timeseries
# ---------------------------------------------------------------------------
cat("\nBuilding Panel B (slope timeseries)...\n")

# Colour palette: Exploited = dark red, Unexploited = steel blue
pal_slope <- c("Exploited" = "#C0392B", "Unexploited" = "#2471A3")

# Key year annotations
key_years <- c(1930, 1974)
key_labels <- c("Whaling\nstarts", "Krill\nfishing\nstarts")

panel_b <- ggplot(slope_summary_top10, aes(x = time, colour = ensemble, fill = ensemble)) +
  # IQR ribbon (Q25–Q75)
  geom_ribbon(aes(ymin = slope_q25, ymax = slope_q75), alpha = 0.35, colour = NA) +
  # Median line
  geom_line(aes(y = slope_median), linewidth = 0.9) +
  # Key event lines
  geom_vline(xintercept = key_years, linetype = "dashed", colour = "grey50",
             linewidth = 0.6, alpha = 0.8) +
  scale_colour_manual(
    values = pal_slope,
    labels = c("Exploited" = "Exploited (Fished)", "Unexploited" = "Unexploited (Climate-only)")
  ) +
  scale_fill_manual(
    values = pal_slope,
    labels = c("Exploited" = "Exploited (Fished)", "Unexploited" = "Unexploited (Climate-only)")
  ) +
  scale_x_continuous(
    breaks = seq(1900, 2010, by = 20),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    x      = "Year",
    y      = expression(paste("NBSS slope (", log[10], " scale)")),
    colour = NULL,
    fill   = NULL,
    caption = "Line: median; ribbon: IQR (Q25\u2013Q75) across top-10% RMSE ensemble"
  ) +
  # Annotate key events
  annotate("text", x = key_years, y = Inf,
           label = key_labels,
           hjust = 0.5, vjust = 1.2, size = 3.0, colour = "grey35") +
  theme_classic() +
  theme(
    axis.title         = element_text(size = 12),
    axis.text          = element_text(size = 11),
    legend.position    = c(0.02, 0.05),
    legend.justification = c(0, 0),
    legend.text        = element_text(size = 10),
    legend.background  = element_rect(fill = "white", colour = NA),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

cat("  Panel B built.\n")

# ---------------------------------------------------------------------------
# 10b. Build Panel B_diff — Paired slope difference timeseries
# ---------------------------------------------------------------------------
cat("\nBuilding Panel B_diff (paired slope difference)...\n")

diff_colour <- "#5D4037"   # dark brown — neutral exploitation signal colour

panel_b_diff <- ggplot(slope_diff_summary, aes(x = time)) +
  geom_ribbon(aes(ymin = diff_q25, ymax = diff_q75),
              fill = diff_colour, alpha = 0.35, colour = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.8) +
  geom_line(aes(y = diff_median), colour = diff_colour, linewidth = 0.9) +
  geom_vline(xintercept = key_years, linetype = "dashed", colour = "grey50",
             linewidth = 0.6, alpha = 0.8) +
  annotate("text", x = key_years - 0.25, y = Inf,
           label = key_labels,
           hjust = 1, vjust = 1.2, size = 3.5, colour = "grey35") +
  annotate("text", x = 1901, y = Inf,
           label = "Exploitation steepens slope ↓",
           hjust = 0, vjust = 1.5, size = 3.25, colour = "grey45", fontface = "italic") +
  scale_x_continuous(
    breaks = seq(1900, 2010, by = 20),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    x = "Year",
    y = expression(paste(Delta, " NBSS slope (Exploited − Unexploited)"))
  ) +
  theme_classic() +
  theme(
    axis.title         = element_text(size = 12),
    axis.text          = element_text(size = 11),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

cat("  Panel B_diff built.\n")

# ---------------------------------------------------------------------------
# 11. Combine panels with patchwork and save
# ---------------------------------------------------------------------------
cat("\nAssembling combined figure...\n")

fig_combined <- panel_a / panel_b +
  plot_annotation(tag_levels = "a",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 14, face = "bold"))

out_path <- file.path(output_dir, "spectrum_ratio_slope_top10pct.png")
ggsave(out_path, fig_combined, width = 12, height = 11, dpi = 300, bg = "white")
cat(sprintf("Saved: %s\n", out_path))

# ---------------------------------------------------------------------------
# 12. Assemble and save paired-difference variant
# ---------------------------------------------------------------------------
cat("\nAssembling paired-difference figure...\n")

fig_paired_diff <- panel_a / panel_b_diff +
  plot_annotation(tag_levels = "a",
                  tag_suffix = "") &
  theme(plot.tag = element_text(size = 14, face = "bold"))

out_path_diff <- file.path(
  output_dir, "spectrum_ratio_slope_top10pct_paired_diff.png"
)
ggsave(out_path_diff, fig_paired_diff,
       width = 12, height = 11, dpi = 300, bg = "white")
cat(sprintf("Saved: %s\n", out_path_diff))

# ---------------------------------------------------------------------------
# 13. Mean ± SD variant: panel a ratio + panel b slope overlay
# ---------------------------------------------------------------------------
cat("\nBuilding mean/SD ratio figure...\n")

ratio_data_a_sd <- pair_ratio_stats %>% dplyr::filter(w >= w_min_plot)

panel_a_mean_sd <- ggplot(ratio_data_a_sd, aes(x = w)) +
  band_annots +
  whale_annots +
  geom_ribbon(aes(ymin = ratio_mean - 2 * ratio_sd,
                  ymax = ratio_mean + 2 * ratio_sd),
              fill = ribbon_colour, alpha = 0.15) +
  geom_ribbon(aes(ymin = ratio_mean - ratio_sd,
                  ymax = ratio_mean + ratio_sd),
              fill = ribbon_colour, alpha = 0.30) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40",
             linewidth = 0.8) +
  geom_line(aes(y = ratio_mean), colour = ribbon_colour, linewidth = 1.2) +
  scale_x_log10(
    labels = x_labels_shared, breaks = x_breaks_shared,
    limits = c(w_min_plot, w_max_plot),
    oob    = scales::squish,
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    limits = y_limits_a,
    oob    = scales::squish,
    breaks = scales::pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(x = "Body mass", y = "Abundance ratio (Exploited / Unexploited)") +
  annotate("text",
           x = w_max_plot * 4.0, y = y_limits_a[2] * 0.955,
           label = "Dominant\nbiomass",
           size = 4.5, colour = "grey20",
           hjust = 0, vjust = 0.5, fontface = "italic") +
  theme_classic() +
  theme(
    axis.title         = element_text(size = 12),
    axis.text          = element_text(size = 11),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    plot.margin        = margin(5.5, 100, 5.5, 5.5)
  ) +
  coord_cartesian(clip = "off")

panel_b_mean_sd <- ggplot(slope_summary_top10,
                          aes(x = time, colour = ensemble, fill = ensemble)) +
  geom_ribbon(aes(ymin = slope_mean - slope_sd,
                  ymax = slope_mean + slope_sd),
              alpha = 0.30, colour = NA) +
  geom_line(aes(y = slope_mean), linewidth = 0.9) +
  geom_vline(xintercept = key_years, linetype = "dashed", colour = "grey50",
             linewidth = 0.6, alpha = 0.8) +
  scale_colour_manual(
    values = pal_slope,
    labels = c("Exploited"   = "Exploited (Fished)",
               "Unexploited" = "Unexploited (Climate-only)")
  ) +
  scale_fill_manual(
    values = pal_slope,
    labels = c("Exploited"   = "Exploited (Fished)",
               "Unexploited" = "Unexploited (Climate-only)")
  ) +
  scale_x_continuous(
    breaks = seq(1900, 2010, by = 20),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    x      = "Year",
    y      = expression(paste("NBSS slope (", log[10], " scale)")),
    colour = NULL, fill = NULL,
    caption = "Line: mean; ribbon: ±1 SD"
  ) +
  annotate("text", x = key_years - 0.25, y = Inf,
           label = key_labels,
           hjust = 1, vjust = 1.2, size = 3.5, colour = "grey35") +
  theme_classic() +
  theme(
    axis.title           = element_text(size = 12),
    axis.text            = element_text(size = 11),
    legend.position      = c(0.02, 0.05),
    legend.justification = c(0, 0),
    legend.text          = element_text(size = 10),
    legend.background    = element_rect(fill = "white", colour = NA),
    panel.grid.major.y   = element_line(color = "grey90", linewidth = 0.3)
  )

fig_mean_sd <- panel_a / panel_b_mean_sd +
  plot_annotation(tag_levels = "a", tag_suffix = "") &
  theme(plot.tag = element_text(size = 14, face = "bold"))

out_path_mean_sd <- file.path(
  output_dir, "spectrum_ratio_slope_top10pct_mean_sd.png"
)
ggsave(out_path_mean_sd, fig_mean_sd,
       width = 12, height = 11, dpi = 300, bg = "white")
cat(sprintf("Saved: %s\n", out_path_mean_sd))

# ---------------------------------------------------------------------------
# 14. Paired diff + SD reference lines figure
# ---------------------------------------------------------------------------
cat("\nBuilding paired difference + SD reference lines figure...\n")

# Baseline SD: natural inter-sim variability of the unexploited slope
# before any exploitation begins (1901-1929). Uses slope values rather
# than paired differences, which are all zero pre-exploitation.
baseline_sd_val <- slope_top10 %>%
  dplyr::filter(ensemble == "Unexploited", time < 1930) %>%
  dplyr::pull(slope) %>%
  sd(na.rm = TRUE)
cat(sprintf("  Baseline SD (unexploited slope, 1901-1929): %.7f\n",
            baseline_sd_val))

sd_multiples <- c(1)
sd_yvals  <- -sd_multiples * baseline_sd_val
sd_labels <- "1 SD pre-exploitation"
sd_greys  <- "red"

panel_b_sd_lines <- ggplot(slope_diff_summary, aes(x = time)) +
  geom_ribbon(aes(ymin = diff_q25, ymax = diff_q75),
              fill = diff_colour, alpha = 0.35, colour = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40",
             linewidth = 0.8) +
  geom_hline(yintercept = sd_yvals,
             linetype  = "dashed",
             colour    = sd_greys,
             linewidth = 0.6) +
  geom_line(aes(y = diff_median), colour = diff_colour, linewidth = 0.9) +
  geom_vline(xintercept = key_years, linetype = "dashed", colour = "grey50",
             linewidth = 0.6, alpha = 0.8) +
  annotate("text", x = key_years - 0.25, y = Inf,
           label = key_labels,
           hjust = 1, vjust = 1.2, size = 3.5, colour = "grey35") +
  annotate("text", x = 1901, y = Inf,
           label = "Exploitation steepens slope ↓",
           hjust = 0, vjust = 1.5, size = 3.25, colour = "grey45",
           fontface = "italic") +
  annotate("text", x = 2011, y = sd_yvals,
           label = "1 SD\nfrom unexploited mean",
           hjust = 0, vjust = 1.2, size = 3.5, colour = "red") +
  scale_x_continuous(
    breaks = seq(1900, 2010, by = 20),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    x = "Year",
    y = expression(paste(Delta, " NBSS slope (Exploited − Unexploited)"))
  ) +
  theme_classic() +
  theme(
    axis.title         = element_text(size = 12),
    axis.text          = element_text(size = 11),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    plot.margin        = margin(5.5, 60, 5.5, 5.5)
  ) +
  coord_cartesian(clip = "off")

fig_paired_sd <- panel_a / panel_b_sd_lines +
  plot_annotation(tag_levels = "a", tag_suffix = "") &
  theme(plot.tag = element_text(size = 14, face = "bold"))

out_path_paired_sd <- file.path(
  output_dir, "spectrum_ratio_slope_top10pct_paired_diff_sd_lines.png"
)
ggsave(out_path_paired_sd, fig_paired_sd,
       width = 12, height = 11, dpi = 300, bg = "white")
cat(sprintf("Saved: %s\n", out_path_paired_sd))

cat("\n=== Done ===\n")
