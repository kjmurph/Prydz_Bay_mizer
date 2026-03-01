# ==============================================================================
# FIXED VERSION v6: Fished vs Unfished Size Spectrum Ratio Plot
# Reference Period (2001-2010) and Pre-industrial (1841-1860)
#
# Based on v4 (good panel b bars) with v5 panel a improvements:
#  - Data-driven 11-group dominance bands (computed from actual biomass)
#  - "Dominant biomass" label outside plot area (clip = "off")
#  - Whale trough annotations with improved search windows
#  - 4 colour palettes (original, highcontrast, viridis, trophic)
#  - Extended x-axis below 1g to include smallest dominance band
#  - Panel b bars unchanged from v4 (seamless, with thin outlines)
# ==============================================================================

library(mizer)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)

cat("=== Fished vs Unfished Size Spectrum — FIXED v6 ===\n\n")

output_dir <- "ecosystem_assessment_outputs/abundance_ratio_plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ref_years <- 2001:2010
reference_name <- "Reference (2001-2010)"

spectra_cache  <- file.path(output_dir, "spectra_cache_ref_period.rds")
species_cache  <- file.path(output_dir, "spectra_cache_species_ref_period.rds")

# --------------------------------------------------------------------------
# Helper functions
# --------------------------------------------------------------------------
extract_community_spectrum <- function(sim, time_range) {
  sim_times <- as.numeric(dimnames(sim@n)$time)
  time_idx  <- which(sim_times %in% time_range)
  if (length(time_idx) == 0) return(NULL)
  n_subset <- sim@n[time_idx, , , drop = FALSE]
  community_spectrum <- apply(n_subset, c(1, 3), sum, na.rm = TRUE)
  colMeans(community_spectrum, na.rm = TRUE)
}

extract_species_spectrum <- function(sim, time_range) {
  sim_times <- as.numeric(dimnames(sim@n)$time)
  time_idx  <- which(sim_times %in% time_range)
  if (length(time_idx) == 0) return(NULL)
  n_subset <- sim@n[time_idx, , , drop = FALSE]
  apply(n_subset, c(2, 3), mean, na.rm = TRUE)
}

# --------------------------------------------------------------------------
# Shared x-axis configuration
# --------------------------------------------------------------------------
x_breaks_shared <- c(1, 100, 1e3, 1e5, 1e6, 1e7, 1e8)
x_labels_shared <- function(x) {
  dplyr::case_when(
    x >= 1e6 ~ paste0(x / 1e6, " t"),
    x >= 1e3 ~ paste0(x / 1e3, " kg"),
    TRUE     ~ paste0(round(x, 0), " g")
  )
}

# --------------------------------------------------------------------------
# Community spectra cache
# --------------------------------------------------------------------------
if (file.exists(spectra_cache)) {
  cat("Loading cached community spectra matrices...\n")
  cache <- readRDS(spectra_cache)
  w_bins        <- cache$w_bins
  n_w           <- cache$n_w
  fished_spectra  <- cache$fished_spectra
  climate_spectra <- cache$climate_spectra
  n_fished  <- nrow(fished_spectra)
  n_climate <- nrow(climate_spectra)
  cat("  Loaded:", n_fished, "fished,", n_climate, "climate-only simulations\n\n")
  rm(cache)
} else {
  cat("No community cache — extracting from full ensembles.\n\n")

  fished_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
  fished_ensemble <- readRDS(fished_file)
  n_fished <- length(fished_ensemble$simulations)
  first_sim <- fished_ensemble$simulations[[1]]
  w_bins  <- first_sim@params@w
  n_w     <- length(w_bins)
  sp_names <- as.character(first_sim@params@species_params$species)
  n_sp    <- length(sp_names)
  rm(first_sim)

  fished_spectra <- matrix(NA, nrow = n_fished, ncol = n_w)
  fished_sp_list <- lapply(sp_names, function(s) matrix(NA, nrow = n_fished, ncol = n_w))
  names(fished_sp_list) <- sp_names

  cat("  Extracting fished spectra...\n")
  pb <- txtProgressBar(min = 0, max = n_fished, style = 3)
  for (i in seq_len(n_fished)) {
    setTxtProgressBar(pb, i)
    sim <- tryCatch(fished_ensemble$simulations[[i]], error = function(e) NULL)
    if (is.null(sim)) next
    comm <- tryCatch(extract_community_spectrum(sim, ref_years), error = function(e) NULL)
    if (!is.null(comm) && length(comm) == n_w) fished_spectra[i, ] <- comm
    sp_mat <- tryCatch(extract_species_spectrum(sim, ref_years), error = function(e) NULL)
    if (!is.null(sp_mat) && ncol(sp_mat) == n_w) {
      for (j in seq_len(n_sp)) fished_sp_list[[j]][i, ] <- sp_mat[j, ]
    }
  }
  close(pb); cat("\n"); rm(fished_ensemble); gc()

  climate_file <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
  climate_ensemble <- readRDS(climate_file)
  n_climate <- climate_ensemble$n_successful

  climate_spectra <- matrix(NA, nrow = n_climate, ncol = n_w)
  climate_sp_list <- lapply(sp_names, function(s) matrix(NA, nrow = n_climate, ncol = n_w))
  names(climate_sp_list) <- sp_names

  cat("  Extracting climate-only spectra...\n")
  pb <- txtProgressBar(min = 0, max = n_climate, style = 3)
  for (i in seq_len(n_climate)) {
    setTxtProgressBar(pb, i)
    sim <- tryCatch(climate_ensemble$simulations[[i]], error = function(e) NULL)
    if (is.null(sim)) next
    comm <- tryCatch(extract_community_spectrum(sim, ref_years), error = function(e) NULL)
    if (!is.null(comm) && length(comm) == n_w) climate_spectra[i, ] <- comm
    sp_mat <- tryCatch(extract_species_spectrum(sim, ref_years), error = function(e) NULL)
    if (!is.null(sp_mat) && ncol(sp_mat) == n_w) {
      for (j in seq_len(n_sp)) climate_sp_list[[j]][i, ] <- sp_mat[j, ]
    }
  }
  close(pb); cat("\n"); rm(climate_ensemble); gc()

  saveRDS(list(w_bins = w_bins, n_w = n_w,
               fished_spectra = fished_spectra, climate_spectra = climate_spectra),
          spectra_cache)
  saveRDS(list(sp_names = sp_names,
               fished_sp = fished_sp_list, climate_sp = climate_sp_list),
          species_cache)
  cat("  Caches saved.\n\n")
}

# --------------------------------------------------------------------------
# Species spectra cache
# --------------------------------------------------------------------------
if (file.exists(species_cache)) {
  cat("Loading cached species spectra...\n")
  sp_cache <- readRDS(species_cache)
  sp_names <- sp_cache$sp_names
  fished_sp_list  <- sp_cache$fished_sp
  climate_sp_list <- sp_cache$climate_sp
  cat("  Species:", paste(sp_names, collapse = ", "), "\n\n")
  rm(sp_cache)
} else {
  cat("No species cache — extracting...\n")
  fished_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
  fished_ensemble <- readRDS(fished_file)
  n_fished <- length(fished_ensemble$simulations)
  first_sim <- fished_ensemble$simulations[[1]]
  sp_names <- as.character(first_sim@params@species_params$species)
  n_sp <- length(sp_names); rm(first_sim)

  fished_sp_list <- lapply(sp_names, function(s) matrix(NA, nrow = n_fished, ncol = n_w))
  names(fished_sp_list) <- sp_names
  pb <- txtProgressBar(min = 0, max = n_fished, style = 3)
  for (i in seq_len(n_fished)) {
    setTxtProgressBar(pb, i)
    sim <- tryCatch(fished_ensemble$simulations[[i]], error = function(e) NULL)
    if (is.null(sim)) next
    sp_mat <- tryCatch(extract_species_spectrum(sim, ref_years), error = function(e) NULL)
    if (!is.null(sp_mat) && ncol(sp_mat) == n_w)
      for (j in seq_len(n_sp)) fished_sp_list[[j]][i, ] <- sp_mat[j, ]
  }
  close(pb); rm(fished_ensemble); gc()

  climate_file <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
  climate_ensemble <- readRDS(climate_file)
  n_climate <- climate_ensemble$n_successful
  climate_sp_list <- lapply(sp_names, function(s) matrix(NA, nrow = n_climate, ncol = n_w))
  names(climate_sp_list) <- sp_names
  pb <- txtProgressBar(min = 0, max = n_climate, style = 3)
  for (i in seq_len(n_climate)) {
    setTxtProgressBar(pb, i)
    sim <- tryCatch(climate_ensemble$simulations[[i]], error = function(e) NULL)
    if (is.null(sim)) next
    sp_mat <- tryCatch(extract_species_spectrum(sim, ref_years), error = function(e) NULL)
    if (!is.null(sp_mat) && ncol(sp_mat) == n_w)
      for (j in seq_len(n_sp)) climate_sp_list[[j]][i, ] <- sp_mat[j, ]
  }
  close(pb); rm(climate_ensemble); gc()

  saveRDS(list(sp_names = sp_names,
               fished_sp = fished_sp_list, climate_sp = climate_sp_list),
          species_cache)
  cat("  Species cache saved.\n\n")
}

# ==============================================================================
# Load model params
# ==============================================================================
cat("Loading model params...\n")
params_obj <- readRDS("params_sel_adj.RDS")
dw <- params_obj@dw
sp_wmax <- setNames(
  mizer::species_params(params_obj)$w_max,
  as.character(mizer::species_params(params_obj)$species)
)
rm(params_obj)

find_sp <- function(pattern) {
  hit <- grep(pattern, sp_names, ignore.case = TRUE, value = TRUE)
  if (length(hit) == 0) stop(paste("No species matching:", pattern))
  hit[1]
}
sp_minke  <- find_sp("minke")
sp_orca   <- find_sp("orca")
sp_sperm  <- find_sp("sperm")
sp_baleen <- find_sp("baleen")

# ==============================================================================
# Per-pair ratio statistics
# ==============================================================================
ratio_matrix <- fished_spectra / climate_spectra
ratio_matrix[!is.finite(ratio_matrix)] <- NA

pair_ratio_stats <- data.frame(
  w            = w_bins,
  ratio_median = apply(ratio_matrix, 2, median,   na.rm = TRUE),
  ratio_mean   = apply(ratio_matrix, 2, mean,     na.rm = TRUE),
  ratio_q05    = apply(ratio_matrix, 2, quantile, probs = 0.05, na.rm = TRUE),
  ratio_q25    = apply(ratio_matrix, 2, quantile, probs = 0.25, na.rm = TRUE),
  ratio_q75    = apply(ratio_matrix, 2, quantile, probs = 0.75, na.rm = TRUE),
  ratio_q95    = apply(ratio_matrix, 2, quantile, probs = 0.95, na.rm = TRUE)
)

write.csv(pair_ratio_stats,
          file.path(output_dir, "fished_vs_unfished_pair_ratio_stats.csv"),
          row.names = FALSE)


# ==============================================================================
# Whale w_max values and midpoint boundaries
# ==============================================================================
fg_minke_max_raw  <- sp_wmax[sp_minke]
fg_orca_max_raw   <- sp_wmax[sp_orca]
fg_sperm_max_raw  <- sp_wmax[sp_sperm]
fg_baleen_max_raw <- sp_wmax[sp_baleen]

# Whale midpoint boundaries (computed once, used for bands and trough detection)
fg_minke_orca_mid   <- sqrt(fg_minke_max_raw * fg_orca_max_raw)
fg_orca_sperm_mid   <- sqrt(fg_orca_max_raw * fg_sperm_max_raw)
fg_sperm_baleen_mid <- sqrt(fg_sperm_max_raw * fg_baleen_max_raw)
fg_baleen_upper     <- fg_baleen_max_raw * 1.15

mm_fill  <- "#7bafd4"
mm_alpha <- 0.18

# Axis limits — extend below 1g to include smallest dominance band
w_below_1g_bins <- w_bins[w_bins < 1]
w_min_plot <- if (length(w_below_1g_bins) > 0) max(w_below_1g_bins) else min(w_bins)
w_max_plot <- fg_baleen_upper * 1.1

cat(sprintf("  Plot range: %.4f g - %.0f t\n", w_min_plot, w_max_plot / 1e6))

# (x_breaks_shared and x_labels_shared already defined above)

# ==============================================================================
# 11-group dominance scheme
# ==============================================================================
dominance_group_levels <- c(
  "Krill", "Salps", "Pelagic fishes", "Commercial fishes",
  "Squids", "Toothfishes", "Flying birds & penguins", "Seals",
  "Large marine mammals"
)

sp_to_dominance <- function(sp_name) {
  dplyr::case_when(
    sp_name == "antarctic krill"          ~ "Krill",
    sp_name == "salps"                    ~ "Salps",
    sp_name %in% c("mesopelagic fishes",
                   "bathypelagic fishes")  ~ "Pelagic fishes",
    sp_name == "shelf and coastal fishes" ~ "Commercial fishes",
    sp_name == "squids"                   ~ "Squids",
    sp_name == "toothfishes"              ~ "Toothfishes",
    sp_name %in% c("flying birds",
                   "small divers")        ~ "Flying birds & penguins",
    sp_name %in% c("medium divers",
                   "large divers")        ~ "Seals",
    sp_name %in% c(sp_minke, sp_orca,
                   sp_sperm, sp_baleen)   ~ "Large marine mammals",
    TRUE                                  ~ NA_character_
  )
}

sp_dominance_groups <- setNames(
  vapply(sp_names, sp_to_dominance, character(1)),
  sp_names
)

cat("  Dominance group mapping:\n")
for (sp in sp_names) {
  grp <- sp_dominance_groups[sp]
  cat(sprintf("    %-30s -> %s\n", sp,
              ifelse(is.na(grp), "(not in dominance groups)", grp)))
}

# ==============================================================================
# Individual species setup for panel b
# ==============================================================================
fg_individual_levels <- sp_names
sp_groups_individual <- factor(sp_names, levels = fg_individual_levels)
names(sp_groups_individual) <- sp_names

# --- PALETTE 1: Original (warm → green → teal → pink/red) ---
fg_colours_original <- c(
  "mesozooplankton"          = "#E9777D", "other krill"              = "#F5956F",
  "other macrozooplankton"   = "#F9C440", "antarctic krill"          = "#C8D66A",
  "salps"                    = "#9EC44A", "mesopelagic fishes"       = "#72B248",
  "bathypelagic fishes"      = "#3D9A40", "shelf and coastal fishes" = "#3CB0A0",
  "flying birds"             = "#2A9490", "small divers"             = "#31BDC0",
  "squids"                   = "#78C8E2", "toothfishes"              = "#4BA8D8",
  "leopard seals"            = "#549AC0", "medium divers"            = "#697DB8",
  "large divers"             = "#9070B8", "minke whales"             = "#F5A0B0",
  "orca"                     = "#E83C90", "sperm whales"             = "#C85030",
  "baleen whales"            = "#FAC8C8"
)

# --- PALETTE 2: High-contrast (maximally separated hues) ---
fg_colours_highcontrast <- c(
  "mesozooplankton"          = "#D73027", "other krill"              = "#FC8D59",
  "other macrozooplankton"   = "#FEE090", "antarctic krill"          = "#E6F598",
  "salps"                    = "#ABDDA4", "mesopelagic fishes"       = "#1B7837",
  "bathypelagic fishes"      = "#66BD63", "shelf and coastal fishes" = "#00BFC4",
  "flying birds"             = "#1F78B4", "small divers"             = "#A6CEE3",
  "squids"                   = "#33A02C", "toothfishes"              = "#6A3D9A",
  "leopard seals"            = "#B2DF8A", "medium divers"            = "#CAB2D6",
  "large divers"             = "#FF7F00", "minke whales"             = "#E31A1C",
  "orca"                     = "#000000", "sperm whales"             = "#8C510A",
  "baleen whales"            = "#FB9A99"
)

# --- PALETTE 3: Viridis-inspired (sequential but distinct) ---
# Zooplankton in warm yellows, fish in greens, upper trophic in blues/purples
fg_colours_viridis <- c(
  "mesozooplankton"          = "#FDE725", "other krill"              = "#D8E219",
  "other macrozooplankton"   = "#B8DE29", "antarctic krill"          = "#95D840",
  "salps"                    = "#73D055", "mesopelagic fishes"       = "#55C667",
  "bathypelagic fishes"      = "#3CBB75", "shelf and coastal fishes" = "#29AF7F",
  "flying birds"             = "#20A386", "small divers"             = "#1F968B",
  "squids"                   = "#238A8D", "toothfishes"              = "#2D708E",
  "leopard seals"            = "#39568C", "medium divers"            = "#453781",
  "large divers"             = "#55186A", "minke whales"             = "#B63679",
  "orca"                     = "#D3436E", "sperm whales"             = "#F05F42",
  "baleen whales"            = "#FCA636"
)

# --- PALETTE 4: Trophic-themed (ecologically intuitive) ---
# Green for primary consumers, blue for fish, warm for top predators
fg_colours_trophic <- c(
  "mesozooplankton"          = "#A8D5A2", "other krill"              = "#6DBF6D",
  "other macrozooplankton"   = "#E8D44D", "antarctic krill"          = "#4DAF4A",
  "salps"                    = "#B2E2B2", "mesopelagic fishes"       = "#377EB8",
  "bathypelagic fishes"      = "#1A5276", "shelf and coastal fishes" = "#2CA02C",
  "flying birds"             = "#8C564B", "small divers"             = "#17BECF",
  "squids"                   = "#9467BD", "toothfishes"              = "#2171B5",
  "leopard seals"            = "#636363", "medium divers"            = "#969696",
  "large divers"             = "#BCBD22", "minke whales"             = "#E377C2",
  "orca"                     = "#1F1F1F", "sperm whales"             = "#D62728",
  "baleen whales"            = "#FF9896"
)

# Collect all palettes
palette_list <- list(
  original     = fg_colours_original,
  highcontrast = fg_colours_highcontrast,
  viridis      = fg_colours_viridis,
  trophic      = fg_colours_trophic
)

# Pad any missing species with grey
for (pal_name in names(palette_list)) {
  pal <- palette_list[[pal_name]]
  for (sp in sp_names) if (!sp %in% names(pal)) pal[sp] <- "grey70"
  palette_list[[pal_name]] <- pal
}

# Default palette
fg_colours_individual <- palette_list[["original"]]


# ==============================================================================
# Helper functions for biomass panels
# ==============================================================================
compute_fg_biomass_df <- function(sp_list, sp_names, sp_groups_vec, w_bins, dw,
                                  fg_levels) {
  sp_median_n <- sapply(sp_names, function(sp) {
    n_mat <- sp_list[[sp]]
    apply(n_mat, 2, median, na.rm = TRUE)
  })
  bm_mat <- sweep(sp_median_n, 1, w_bins * dw, "*")
  fg_bm <- sapply(fg_levels, function(fg) {
    sp_in_fg <- sp_names[as.character(sp_groups_vec) == fg]
    if (length(sp_in_fg) == 0) return(rep(0, length(w_bins)))
    if (length(sp_in_fg) == 1) return(bm_mat[, sp_in_fg])
    rowSums(bm_mat[, sp_in_fg, drop = FALSE])
  })
  as.data.frame(fg_bm) %>%
    dplyr::mutate(w = w_bins) %>%
    tidyr::pivot_longer(-w, names_to = "functional_group", values_to = "biomass") %>%
    dplyr::mutate(functional_group = factor(functional_group, levels = fg_levels))
}

aggregate_to_log_bins <- function(bm_df, n_bins = 60) {
  w_pos <- bm_df$w[bm_df$w > 0]
  w_range <- range(w_pos, na.rm = TRUE)
  log_breaks <- seq(log10(w_range[1]), log10(w_range[2]),
                    length.out = n_bins + 1)
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

# ==============================================================================
# Compute data-driven dominance bands from actual biomass
# ==============================================================================
cat("\nComputing data-driven dominance bands...\n")

sp_dom_factor <- factor(sp_dominance_groups, levels = dominance_group_levels)
names(sp_dom_factor) <- sp_names
sp_in_dom <- sp_names[!is.na(sp_dominance_groups)]
sp_dom_factor_clean <- sp_dom_factor[sp_in_dom]

bm_dominance <- compute_fg_biomass_df(
  fished_sp_list, sp_in_dom, sp_dom_factor_clean, w_bins, dw,
  fg_levels = dominance_group_levels
)

# Use v4's aggregate_to_log_bins (bins from full data range = seamless)
agg_dominance <- aggregate_to_log_bins(bm_dominance, n_bins = 80)

# For each bin, find which dominance group has the highest biomass
dominant_per_bin <- agg_dominance %>%
  dplyr::group_by(bin_id, bin_left, bin_right) %>%
  dplyr::slice_max(biomass, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(bin_left) %>%
  dplyr::select(bin_id, bin_left, bin_right, dominant_group = functional_group)

# Merge consecutive bins with same dominant group into contiguous bands
nn <- nrow(dominant_per_bin)
bands_data <- dominant_per_bin %>%
  dplyr::mutate(
    run_id = cumsum(c(1L, as.integer(
      dominant_group[-nn] != dominant_group[-1])))
  ) %>%
  dplyr::group_by(run_id, dominant_group) %>%
  dplyr::summarise(band_xmin = min(bin_left), band_xmax = max(bin_right),
                   .groups = "drop") %>%
  dplyr::arrange(band_xmin) %>%
  dplyr::select(band_xmin, band_xmax, dominant_group)

# Collapse whale bands into single "Large marine mammals" band
mm_group <- "Large marine mammals"
non_mm_bands <- bands_data %>% dplyr::filter(dominant_group != mm_group)
mm_bands_raw <- bands_data %>% dplyr::filter(dominant_group == mm_group)

if (nrow(mm_bands_raw) > 0) {
  mm_start <- min(mm_bands_raw$band_xmin)
  non_mm_bands <- non_mm_bands %>%
    dplyr::mutate(band_xmax = pmin(band_xmax, mm_start)) %>%
    dplyr::filter(band_xmax > band_xmin)

  mm_band <- data.frame(
    band_xmin = mm_start,
    band_xmax = fg_baleen_upper,
    dominant_group = mm_group,
    stringsAsFactors = FALSE
  )
  bands_fixed <- dplyr::bind_rows(non_mm_bands, mm_band) %>%
    dplyr::arrange(band_xmin)
} else {
  bands_fixed <- non_mm_bands
}

# Extend edge bands to fill full axis range
if (nrow(bands_fixed) > 0) {
  bands_fixed$band_xmin[1] <- w_min_plot
  bands_fixed$band_xmax[nrow(bands_fixed)] <- w_max_plot
}

cat("  Data-driven dominance bands:\n")
for (i in seq_len(nrow(bands_fixed))) {
  xmin_str <- ifelse(bands_fixed$band_xmin[i] >= 1e6,
    sprintf("%.1f t", bands_fixed$band_xmin[i] / 1e6),
    ifelse(bands_fixed$band_xmin[i] >= 1e3,
      sprintf("%.1f kg", bands_fixed$band_xmin[i] / 1e3),
      sprintf("%.1f g", bands_fixed$band_xmin[i])))
  xmax_str <- ifelse(bands_fixed$band_xmax[i] >= 1e6,
    sprintf("%.1f t", bands_fixed$band_xmax[i] / 1e6),
    ifelse(bands_fixed$band_xmax[i] >= 1e3,
      sprintf("%.1f kg", bands_fixed$band_xmax[i] / 1e3),
      sprintf("%.1f g", bands_fixed$band_xmax[i])))
  cat(sprintf("    %-25s : %s - %s\n",
              bands_fixed$dominant_group[i], xmin_str, xmax_str))
}

# Band annotation generator — data-driven, contiguous
# ==============================================================================
generate_band_annotations <- function(bands_df, y_label_pos = 1.24, y_min = 0) {
  # Fills: grey gradient for non-mammal, blue for large marine mammals (orca shade)
  band_fills <- c(
    "Krill" = "grey90", "Salps" = "grey82",
    "Pelagic fishes" = "grey74", "Commercial fishes" = "grey66",
    "Squids" = "grey58", "Toothfishes" = "grey50",
    "Flying birds & penguins" = "grey42", "Seals" = "grey34",
    "Large marine mammals" = mm_fill
  )
  band_alphas <- c(
    "Krill" = 0.6, "Salps" = 0.55,
    "Pelagic fishes" = 0.5, "Commercial fishes" = 0.45,
    "Squids" = 0.4, "Toothfishes" = 0.35,
    "Flying birds & penguins" = 0.3, "Seals" = 0.25,
    "Large marine mammals" = mm_alpha * 1.4   # orca shade
  )
  short_labels <- c(
    "Krill" = "Krill", "Salps" = "Salps",
    "Pelagic fishes" = "Pelagic\nfishes",
    "Commercial fishes" = "Commercial\nfishes",
    "Squids" = "Squids", "Toothfishes" = "Toothfishes",
    "Flying birds & penguins" = "Flying birds\n& penguins", "Seals" = "Seals",
    "Large marine mammals" = "Large marine\nmammals"
  )

  annots <- list()
  prev_log_x <- -Inf
  stagger_up <- FALSE

  for (i in seq_len(nrow(bands_df))) {
    grp  <- as.character(bands_df$dominant_group[i])
    xmin <- bands_df$band_xmin[i]; xmax <- bands_df$band_xmax[i]
    if (!grp %in% names(band_fills)) next

    # Centre label on visible portion (clamp xmin to w_min_plot for Krill etc.)
    xmin_vis <- max(xmin, w_min_plot)
    xmid <- sqrt(xmin_vis * xmax)
    log_xmid <- log10(xmid)

    # Add band rectangle
    annots <- c(annots, list(
      annotate("rect", xmin = xmin, xmax = xmax, ymin = y_min, ymax = Inf,
               fill = band_fills[grp], alpha = band_alphas[grp])
    ))

    # Detect overlap: stagger labels when midpoints are close in log space
    proximity <- log_xmid - prev_log_x
    if (proximity < 0.55 && prev_log_x > -Inf) {
      y_off <- if (stagger_up) y_label_pos * 0.065 else -y_label_pos * 0.065
      stagger_up <- !stagger_up
    } else {
      y_off <- 0
      stagger_up <- FALSE
    }

    annots <- c(annots, list(
      annotate("text", x = xmid, y = y_label_pos + y_off,
               label = short_labels[grp],
               size = ifelse(grp %in% c("Large marine mammals",
                                         "Flying birds & penguins"), 2.7, 3.0),
               colour = "grey25", hjust = 0.5)
    ))
    prev_log_x <- log_xmid
  }
  annots
}

# ==============================================================================
# Whale trough annotations — Baleen label now to the RIGHT of the plot
# ==============================================================================
compute_whale_annotations <- function(ratio_stats) {
  # Whale midpoint boundaries for trough search (already computed globally)
  make_trough <- function(w, med, w_lo, w_hi) {
    idx <- which(w > w_lo & w < w_hi)
    if (length(idx) == 0) return(NULL)
    pos <- idx[which.min(med[idx])]
    list(w = w[pos], y = med[pos])
  }

  # Find mammal start from bands
  mm_rows <- bands_fixed$dominant_group == "Large marine mammals"
  if (!any(mm_rows)) return(list())
  whale_start <- min(bands_fixed$band_xmin[mm_rows])

  # Trough search windows — use species w_max × 1.5 as boundaries
  # to keep each species' window non-overlapping:
  #   Minke  : whale_start → 1.5 × minke_w_max  (≈2.9t → 9t)
  #   Orca   : 1.5 × minke_w_max → 1.5 × orca_w_max  (≈9t → 16t)
  #   Sperm  : 1.5 × orca_w_max → 1.5 × sperm_w_max  (≈16t → 55t)
  #   Baleen : 1.5 × sperm_w_max → baleen_upper × 1.5 (≈55t → 178t)
  mk <- make_trough(ratio_stats$w, ratio_stats$ratio_median,
                    whale_start * 0.8, fg_minke_max_raw * 1.5)
  ok <- make_trough(ratio_stats$w, ratio_stats$ratio_median,
                    fg_minke_max_raw * 1.5, fg_orca_max_raw * 1.5)
  sp <- make_trough(ratio_stats$w, ratio_stats$ratio_median,
                    fg_orca_max_raw * 1.5, fg_sperm_max_raw * 1.5)
  bl <- make_trough(ratio_stats$w, ratio_stats$ratio_median,
                    fg_sperm_max_raw * 1.5, fg_baleen_upper * 1.5)

  annots <- list()

  # Minke — label to the left
  if (!is.null(mk)) {
    lx <- mk$w / 2.0
    annots <- c(annots, list(
      annotate("segment", x = lx, xend = mk$w, y = mk$y, yend = mk$y,
               colour = "grey30", linewidth = 0.5),
      annotate("text", x = lx, y = mk$y, label = "Minke whales",
               size = 3.2, hjust = 1, vjust = 0.5, colour = "grey20")
    ))
  }

  # Orca — label to the right (offset upward slightly to avoid overlap with minke)
  if (!is.null(ok)) {
    rx <- ok$w * 1.5
    ry <- ok$y + 0.08
    annots <- c(annots, list(
      annotate("segment", x = ok$w, xend = rx, y = ok$y, yend = ry,
               colour = "grey30", linewidth = 0.5),
      annotate("text", x = rx, y = ry, label = "Orca",
               size = 3.2, hjust = 0, vjust = 0.5, colour = "grey20")
    ))
  }

  # Sperm — label to the left
  if (!is.null(sp)) {
    lx <- sp$w / 2.0
    annots <- c(annots, list(
      annotate("segment", x = lx, xend = sp$w, y = sp$y, yend = sp$y,
               colour = "grey30", linewidth = 0.5),
      annotate("text", x = lx, y = sp$y, label = "Sperm whales",
               size = 3.2, hjust = 1, vjust = 0.5, colour = "grey20")
    ))
  }

  # Baleen — angled line to the RIGHT of the plot
  if (!is.null(bl)) {
    rx_line <- bl$w * 1.15
    ry_line <- bl$y + 0.15
    rx_lbl  <- bl$w * 1.18
    ry_lbl  <- bl$y + 0.17
    annots <- c(annots, list(
      annotate("segment", x = bl$w, xend = rx_line, y = bl$y, yend = ry_line,
               colour = "grey30", linewidth = 0.5),
      annotate("text", x = rx_lbl, y = ry_lbl, label = "Baleen\nwhales",
               size = 3.2, hjust = 0, vjust = 0.5, colour = "grey20")
    ))
  }

  cat(sprintf("  Trough positions:\n"))
  if (!is.null(mk)) cat(sprintf("    Minke  = %.1f t (ratio = %.3f)\n", mk$w/1e6, mk$y))
  if (!is.null(ok)) cat(sprintf("    Orca   = %.1f t (ratio = %.3f)\n", ok$w/1e6, ok$y))
  if (!is.null(sp)) cat(sprintf("    Sperm  = %.1f t (ratio = %.3f)\n", sp$w/1e6, sp$y))
  if (!is.null(bl)) cat(sprintf("    Baleen = %.1f t (ratio = %.3f)\n", bl$w/1e6, bl$y))

  annots
}

# ==============================================================================
# Build ratio panel
# ==============================================================================
build_ratio_panel <- function(ratio_stats, bands_df,
                              ribbon_colour = "#e41a1c",
                              y_label = "Abundance ratio (Fished / Unfished)",
                              y_limits = c(0, 1.3), y_breaks = NULL,
                              w_min = NULL) {
  if (is.null(w_min)) w_min <- w_min_plot
  rs <- ratio_stats %>% dplyr::filter(w >= w_min)
  band_annots  <- generate_band_annotations(bands_df,
                      y_label_pos = y_limits[2] * 0.955, y_min = y_limits[1])
  whale_annots <- compute_whale_annotations(rs)
  if (is.null(y_breaks)) y_breaks <- scales::pretty_breaks(n = 6)

  p <- ggplot(rs, aes(x = w)) +
    band_annots +
    whale_annots +
    geom_ribbon(aes(ymin = ratio_q25, ymax = ratio_q75),
                fill = ribbon_colour, alpha = 0.3) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.8) +
    geom_line(aes(y = ratio_median), colour = ribbon_colour, linewidth = 1.2) +
    scale_x_log10(labels = x_labels_shared, breaks = x_breaks_shared,
                  limits = c(w_min_plot, w_max_plot), oob = scales::squish) +
    scale_y_continuous(limits = y_limits, breaks = y_breaks,
                       expand = expansion(mult = c(0, 0.02))) +
    labs(x = "Body mass", y = y_label) +
    # "Dominant biomass" label — OUTSIDE plot on the right
    # Matches trough label font: size 3.2, grey20
    annotate("text",
             x = w_max_plot * 2.5,        # squished to right edge by oob
             y = y_limits[2] * 0.955,      # same height as band labels
             label = "Dominant\nbiomass",
             size = 4.5, colour = "grey20",
             hjust = 0, vjust = 0.5,        # left-aligned at right edge -> renders in margin
             fontface = "italic") +
    theme_classic() +
    theme(
      axis.title = element_text(size = 12),
      axis.text  = element_text(size = 11),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      plot.margin = margin(5.5, 80, 5.5, 5.5)   # wider right margin for larger label
    ) +
    coord_cartesian(clip = "off")                 # allow drawing outside panel

  p
}

# ==============================================================================
# Biomass bar panel — y-axis in TONNES
# ==============================================================================
build_biomass_bar_panel <- function(agg_df, panel_title = "", show_x_axis = TRUE,
                                   colour_map = fg_colours_individual,
                                   bar_alpha = 0.7,
                                   y_label = expression(paste("Biomass (t ", m^{-2}, ")"))) {

  # Convert grams to tonnes for display (1 t = 1e6 g)
  agg_df <- agg_df %>% dplyr::mutate(biomass = biomass / 1e6)

  y_floor <- min(agg_df$biomass[agg_df$biomass > 0], na.rm = TRUE) * 0.5

  stack_df <- agg_df %>%
    dplyr::arrange(bin_id, functional_group) %>%
    dplyr::group_by(bin_id) %>%
    dplyr::mutate(
      total         = sum(biomass, na.rm = TRUE),
      prop          = dplyr::if_else(total > 0, biomass / total, 0),
      cum_prop      = cumsum(prop),
      cum_prop_prev = cum_prop - prop,
      log_floor     = log10(y_floor),
      log_total     = log10(pmax(total, y_floor)),
      log_bottom    = log_floor + cum_prop_prev * (log_total - log_floor),
      log_top       = log_floor + cum_prop * (log_total - log_floor),
      cum_bottom    = 10^log_bottom,
      cum_top       = 10^log_top
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(biomass > 0)

  # Only show species with visible bars within x-axis range
  visible_species <- stack_df %>%
    dplyr::filter(bin_right >= w_min_plot & bin_left <= w_max_plot) %>%
    dplyr::pull(functional_group) %>%
    unique() %>%
    as.character()

  p <- ggplot(stack_df) +
    geom_rect(
      aes(xmin = bin_left, xmax = bin_right,
          ymin = cum_bottom, ymax = cum_top,
          fill = functional_group),
      alpha = bar_alpha,
      colour = "grey30", linewidth = 0.15
    ) +
    scale_fill_manual(
      values = colour_map,
      name   = "Species",
      limits = visible_species,
      drop   = TRUE
    ) +
    scale_x_log10(
      labels = x_labels_shared,
      breaks = x_breaks_shared,
      limits = c(w_min_plot, w_max_plot)
    ) +
    scale_y_log10(
      labels = scales::label_scientific()
    ) +
    labs(title = panel_title,
         x = if (show_x_axis) "Body mass" else NULL,
         y = y_label) +
    theme_classic() +
    theme(
      plot.title         = element_text(size = 14, face = "bold"),
      axis.title         = element_text(size = 11),
      axis.text          = element_text(size = 10),
      legend.position    = "right",
      legend.title       = element_text(size = 10, face = "bold"),
      legend.text        = element_text(size = 9),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
    )

  if (!show_x_axis)
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  p
}


# ############################################################################
#  FIGURE ASSEMBLY
# ############################################################################

cat("\n=== Assembling figures ===\n")

# Compute individual-species biomass (v4 aggregate — full data range, seamless bins)
bm_fished_ref <- compute_fg_biomass_df(
  fished_sp_list, sp_names, sp_groups_individual, w_bins, dw,
  fg_levels = fg_individual_levels
)
agg_fished_ref <- aggregate_to_log_bins(bm_fished_ref, n_bins = 60)

# ==============================================================================
# V1: Fished / Climate-only, Reference Period (2001-2010) — 2 panels
# ==============================================================================
cat("\n--- V1: Fished vs Climate-only, 2001-2010 ---\n")

p_top_v1 <- build_ratio_panel(
  pair_ratio_stats, bands_fixed,
  ribbon_colour = "#e41a1c",
  y_limits = c(0, 1.3),
  y_breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2)
) +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

for (pal_name in names(palette_list)) {
  cat(sprintf("  Saving V1 with palette: %s\n", pal_name))
  p_bm <- build_biomass_bar_panel(agg_fished_ref,
                                   colour_map = palette_list[[pal_name]],
                                   bar_alpha = 0.7)
  p_combined <- (p_top_v1 / p_bm) +
    plot_layout(heights = c(1, 1)) +
    plot_annotation(
      tag_levels = "a", tag_prefix = "", tag_suffix = ")",
      theme = theme(plot.tag = element_text(size = 13, face = "bold"))
    )

  suffix <- ifelse(pal_name == "original", "", paste0("_", pal_name))
  out <- file.path(output_dir,
    paste0("fishing_impact_fished_vs_climateonly_ref2001-2010_2panel", suffix, ".png"))
  ggsave(out, p_combined, width = 12, height = 11, dpi = 300)
  cat("    ->", out, "\n")
}

# ==============================================================================
# SECTION: Pre-industrial baseline (1841-1860)
# ==============================================================================
cat("\n=== Pre-industrial baseline (1841-1860) ===\n\n")

preindustrial_years <- 1841:1860

preindustrial_comm_cache    <- file.path(output_dir, "spectra_cache_preindustrial.rds")
preindustrial_species_cache <- file.path(output_dir, "spectra_cache_species_preindustrial.rds")

if (file.exists(preindustrial_comm_cache) &&
    file.exists(preindustrial_species_cache)) {
  pi_comm <- readRDS(preindustrial_comm_cache)
  preindustrial_spectra <- pi_comm$climate_spectra
  cat("  Loaded:", nrow(preindustrial_spectra), "pre-industrial sims\n")
  rm(pi_comm)
  pi_sp <- readRDS(preindustrial_species_cache)
  preindustrial_sp_list <- pi_sp$climate_sp
  rm(pi_sp)
} else {
  cat("  Extracting pre-industrial spectra...\n")
  climate_file <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
  climate_ensemble <- readRDS(climate_file)
  n_climate_pi <- climate_ensemble$n_successful
  preindustrial_spectra <- matrix(NA, nrow = n_climate_pi, ncol = n_w)
  pi_sp_list <- lapply(sp_names, function(s) matrix(NA, nrow = n_climate_pi, ncol = n_w))
  names(pi_sp_list) <- sp_names

  pb <- txtProgressBar(min = 0, max = n_climate_pi, style = 3)
  for (i in seq_len(n_climate_pi)) {
    setTxtProgressBar(pb, i)
    sim <- tryCatch(climate_ensemble$simulations[[i]], error = function(e) NULL)
    if (is.null(sim)) next
    comm <- tryCatch(extract_community_spectrum(sim, preindustrial_years), error = function(e) NULL)
    if (!is.null(comm) && length(comm) == n_w) preindustrial_spectra[i, ] <- comm
    sp_mat <- tryCatch(extract_species_spectrum(sim, preindustrial_years), error = function(e) NULL)
    if (!is.null(sp_mat) && ncol(sp_mat) == n_w)
      for (j in seq_along(sp_names)) pi_sp_list[[sp_names[j]]][i, ] <- sp_mat[j, ]
  }
  close(pb); rm(climate_ensemble); gc()

  saveRDS(list(w_bins = w_bins, n_w = n_w, climate_spectra = preindustrial_spectra),
          preindustrial_comm_cache)
  saveRDS(list(sp_names = sp_names, climate_sp = pi_sp_list),
          preindustrial_species_cache)
  preindustrial_sp_list <- pi_sp_list
}

# V2 ratio stats
ratio_v2_matrix <- fished_spectra / preindustrial_spectra
ratio_v2_matrix[!is.finite(ratio_v2_matrix)] <- NA

ratio_v2_stats <- data.frame(
  w            = w_bins,
  ratio_median = apply(ratio_v2_matrix, 2, median,   na.rm = TRUE),
  ratio_mean   = apply(ratio_v2_matrix, 2, mean,     na.rm = TRUE),
  ratio_q05    = apply(ratio_v2_matrix, 2, quantile, probs = 0.05, na.rm = TRUE),
  ratio_q25    = apply(ratio_v2_matrix, 2, quantile, probs = 0.25, na.rm = TRUE),
  ratio_q75    = apply(ratio_v2_matrix, 2, quantile, probs = 0.75, na.rm = TRUE),
  ratio_q95    = apply(ratio_v2_matrix, 2, quantile, probs = 0.95, na.rm = TRUE)
)

write.csv(ratio_v2_stats,
          file.path(output_dir, "fished_vs_preindustrial_ratio_stats.csv"),
          row.names = FALSE)

v2_y_max <- max(
  ceiling(quantile(ratio_v2_stats$ratio_q75, 0.99, na.rm = TRUE) * 10) / 10 + 0.1,
  1.32
)
v2_y_max <- min(v2_y_max, 3.0)

# Pre-industrial biomass
bm_preindustrial <- compute_fg_biomass_df(
  preindustrial_sp_list, sp_names, sp_groups_individual, w_bins, dw,
  fg_levels = fg_individual_levels
)
agg_preindustrial <- aggregate_to_log_bins(bm_preindustrial, n_bins = 60)

# ==============================================================================
# V2: Fished (2001-2010) / Pre-industrial (1841-1860) — 3 panels
# ==============================================================================
cat("\n--- V2: Fished vs Pre-industrial, 3-panel ---\n")

p_top_v2 <- build_ratio_panel(
  ratio_v2_stats, bands_fixed,
  ribbon_colour = "#4878A4",
  y_label  = "Abundance ratio\n(Fished 2001\u20132010 / Pre-industrial)",
  y_limits = c(0, v2_y_max)
) +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

for (pal_name in names(palette_list)) {
  cat(sprintf("  Saving V2 with palette: %s\n", pal_name))
  p_mid <- build_biomass_bar_panel(agg_fished_ref,
                                    colour_map = palette_list[[pal_name]],
                                    bar_alpha = 0.7) +
    theme(axis.title.x = element_blank(),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank())

  p_bot <- build_biomass_bar_panel(agg_preindustrial,
                                    colour_map = palette_list[[pal_name]],
                                    bar_alpha = 0.7)

  p_combined <- (p_top_v2 / p_mid / p_bot) +
    plot_layout(heights = c(1.5, 1, 1)) +
    plot_annotation(
      tag_levels = "a", tag_prefix = "", tag_suffix = ")",
      theme = theme(plot.tag = element_text(size = 13, face = "bold"))
    )

  suffix <- ifelse(pal_name == "original", "", paste0("_", pal_name))
  out <- file.path(output_dir,
    paste0("fishing_climate_impact_fished2001-2010_vs_preindustrial1841-1860_3panel",
           suffix, ".png"))
  ggsave(out, p_combined, width = 12, height = 14, dpi = 300)
  cat("    ->", out, "\n")
}


# ==============================================================================
# Silhouette code using rphylopic
# ==============================================================================
cat("\n=== Silhouette lookup using rphylopic ===\n")
cat("  To use silhouettes, ensure rphylopic is installed:\n")
cat("  install.packages('rphylopic')\n\n")

# Search terms for each dominance group
silhouette_search_terms <- c(
  "Krill"                    = "Euphausiacea",
  "Salps"                    = "Salpida",
  "Pelagic fishes"           = "Myctophidae",
  "Commercial fishes"        = "Nototheniidae",
  "Squids"                   = "Teuthida",
  "Toothfishes"              = "Dissostichus",
  "Flying birds & penguins"  = "Spheniscidae",
  "Seals"                    = "Mirounga",
  "Large marine mammals"     = "Balaenoptera musculus"
)

# Example code to fetch and add silhouettes to panel a:
#
# library(rphylopic)
#
# silhouette_uuids <- list()
# for (grp in names(silhouette_search_terms)) {
#   term <- silhouette_search_terms[grp]
#   cat(sprintf("  Searching PhyloPic for: %s (%s)...\n", grp, term))
#   result <- tryCatch(
#     rphylopic::get_uuid(name = term, n = 1),
#     error = function(e) { cat("    Not found\n"); NULL }
#   )
#   if (!is.null(result)) {
#     silhouette_uuids[[grp]] <- result
#     cat(sprintf("    UUID: %s\n", result))
#   }
# }
#
# add_silhouettes <- function(p, bands_df, uuid_list, y_pos = 0.5) {
#   for (i in seq_len(nrow(bands_df))) {
#     grp <- bands_df$dominant_group[i]
#     if (!grp %in% names(uuid_list)) next
#     xmid <- sqrt(bands_df$band_xmin[i] * bands_df$band_xmax[i])
#     p <- p + rphylopic::add_phylopic(
#       uuid = uuid_list[[grp]],
#       x = log10(xmid), y = y_pos,
#       ysize = 0.15, alpha = 0.3, color = "grey30"
#     )
#   }
#   p
# }

cat("\n=== Done! ===\n")
