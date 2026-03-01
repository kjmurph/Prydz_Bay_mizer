# Create Fished vs Unfished Size Spectrum Ratio Plot
# Reference Period (2001-2010)
# Same style as the combined pre-whaling + krill fishing panel

library(mizer)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)

cat("=== Fished vs Unfished Size Spectrum (Reference Period) ===\n\n")

output_dir <- "ecosystem_assessment_outputs/abundance_ratio_plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Reference period
ref_years <- 2001:2010
reference_name <- "Reference (2001-2010)"

# Cache file for extracted spectra (avoids re-loading full ensembles on re-runs)
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
  # Returns matrix: n_species x n_w (time-averaged per-species abundance)
  sim_times <- as.numeric(dimnames(sim@n)$time)
  time_idx  <- which(sim_times %in% time_range)
  if (length(time_idx) == 0) return(NULL)
  n_subset <- sim@n[time_idx, , , drop = FALSE]
  apply(n_subset, c(2, 3), mean, na.rm = TRUE)  # rows = species, cols = w bins
}

# ------------------------------------------------------------------------------
# Community spectra cache
# ------------------------------------------------------------------------------
if (file.exists(spectra_cache)) {
  cat("Loading cached community spectra matrices...\n")
  cache <- readRDS(spectra_cache)
  w_bins        <- cache$w_bins
  n_w           <- cache$n_w
  fished_spectra  <- cache$fished_spectra
  climate_spectra <- cache$climate_spectra
  n_fished  <- nrow(fished_spectra)
  n_climate <- nrow(climate_spectra)
  cat("  Loaded from cache:", n_fished, "fished,", n_climate, "climate-only simulations\n\n")
  rm(cache)
} else {
  cat("No community cache found — extracting from full ensembles.\n\n")

  cat("Loading fished ensemble...\n")
  fished_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
  fished_ensemble <- readRDS(fished_file)
  n_fished <- length(fished_ensemble$simulations)
  cat("  Fished:", n_fished, "simulations\n")

  first_sim <- fished_ensemble$simulations[[1]]
  w_bins  <- first_sim@params@w
  n_w     <- length(w_bins)
  sp_names <- as.character(first_sim@params@species_params$species)
  n_sp    <- length(sp_names)
  cat("  Size bins:", n_w, "| Species:", paste(sp_names, collapse = ", "), "\n")
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
  close(pb)
  cat("\n  Done.\n")
  rm(fished_ensemble); gc()

  cat("Loading climate-only ensemble...\n")
  climate_file <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
  climate_ensemble <- readRDS(climate_file)
  n_climate <- climate_ensemble$n_successful
  cat("  Climate-only:", n_climate, "simulations\n")

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
  close(pb)
  cat("\n  Done.\n")
  rm(climate_ensemble); gc()

  saveRDS(list(w_bins = w_bins, n_w = n_w,
               fished_spectra = fished_spectra,
               climate_spectra = climate_spectra),
          spectra_cache)
  saveRDS(list(sp_names = sp_names,
               fished_sp = fished_sp_list,
               climate_sp = climate_sp_list),
          species_cache)
  cat("  Caches saved.\n\n")
}

# ------------------------------------------------------------------------------
# Species spectra cache (may exist independently if community cache is fresh)
# ------------------------------------------------------------------------------
if (file.exists(species_cache)) {
  cat("Loading cached species spectra...\n")
  sp_cache    <- readRDS(species_cache)
  sp_names    <- sp_cache$sp_names
  fished_sp_list  <- sp_cache$fished_sp
  climate_sp_list <- sp_cache$climate_sp
  cat("  Species:", paste(sp_names, collapse = ", "), "\n\n")
  rm(sp_cache)
} else {
  cat("No species cache — extracting species spectra from ensembles...\n")

  fished_file <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
  fished_ensemble <- readRDS(fished_file)
  n_fished <- length(fished_ensemble$simulations)
  first_sim   <- fished_ensemble$simulations[[1]]
  sp_names    <- as.character(first_sim@params@species_params$species)
  n_sp        <- length(sp_names)
  rm(first_sim)

  fished_sp_list <- lapply(sp_names, function(s) matrix(NA, nrow = n_fished, ncol = n_w))
  names(fished_sp_list) <- sp_names

  pb <- txtProgressBar(min = 0, max = n_fished, style = 3)
  for (i in seq_len(n_fished)) {
    setTxtProgressBar(pb, i)
    sim <- tryCatch(fished_ensemble$simulations[[i]], error = function(e) NULL)
    if (is.null(sim)) next
    sp_mat <- tryCatch(extract_species_spectrum(sim, ref_years), error = function(e) NULL)
    if (!is.null(sp_mat) && ncol(sp_mat) == n_w) {
      for (j in seq_len(n_sp)) fished_sp_list[[j]][i, ] <- sp_mat[j, ]
    }
  }
  close(pb); rm(fished_ensemble); gc()

  climate_file <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
  climate_ensemble <- readRDS(climate_file)
  n_climate    <- climate_ensemble$n_successful
  climate_sp_list <- lapply(sp_names, function(s) matrix(NA, nrow = n_climate, ncol = n_w))
  names(climate_sp_list) <- sp_names

  pb <- txtProgressBar(min = 0, max = n_climate, style = 3)
  for (i in seq_len(n_climate)) {
    setTxtProgressBar(pb, i)
    sim <- tryCatch(climate_ensemble$simulations[[i]], error = function(e) NULL)
    if (is.null(sim)) next
    sp_mat <- tryCatch(extract_species_spectrum(sim, ref_years), error = function(e) NULL)
    if (!is.null(sp_mat) && ncol(sp_mat) == n_w) {
      for (j in seq_len(n_sp)) climate_sp_list[[j]][i, ] <- sp_mat[j, ]
    }
  }
  close(pb); rm(climate_ensemble); gc()

  saveRDS(list(sp_names = sp_names,
               fished_sp = fished_sp_list,
               climate_sp = climate_sp_list),
          species_cache)
  cat("  Species cache saved.\n\n")
}

# ==============================================================================
# PLOT 1: Per-pair Fished / Climate-Only Ratio (original approach)
# Each fished simulation divided by its matched climate-only simulation
# ==============================================================================
cat("\n--- Plot 1: Per-pair Fished / Climate-Only Ratio ---\n")

ratio_matrix <- fished_spectra / climate_spectra
ratio_matrix[!is.finite(ratio_matrix)] <- NA

pair_ratio_stats <- data.frame(
  w = w_bins,
  ratio_median = apply(ratio_matrix, 2, median,   na.rm = TRUE),
  ratio_mean   = apply(ratio_matrix, 2, mean,     na.rm = TRUE),
  ratio_q05    = apply(ratio_matrix, 2, quantile, probs = 0.05, na.rm = TRUE),
  ratio_q25    = apply(ratio_matrix, 2, quantile, probs = 0.25, na.rm = TRUE),
  ratio_q75    = apply(ratio_matrix, 2, quantile, probs = 0.75, na.rm = TRUE),
  ratio_q95    = apply(ratio_matrix, 2, quantile, probs = 0.95, na.rm = TRUE)
)

write.csv(pair_ratio_stats, file.path(output_dir, "fished_vs_unfished_pair_ratio_stats.csv"),
          row.names = FALSE)

# ------------------------------------------------------------------------------
# Annotation constants (verified from model species_params)
# ------------------------------------------------------------------------------

# Whale w_max values (grams)
w_max_minke  <- 6.000e+06   # Minke whale
w_max_sperm  <- 3.650e+07   # Sperm whale
w_max_baleen <- 1.030e+08   # Baleen whale

# Locate actual trough (lowest median ratio) within each whale's size window
trough_minke  <- with(pair_ratio_stats,
  w[w > 4e6 & w < 1.5e7][which.min(ratio_median[w > 4e6 & w < 1.5e7])])
trough_sperm  <- with(pair_ratio_stats,
  w[w > 2e7 & w < 8e7][which.min(ratio_median[w > 2e7 & w < 8e7])])
trough_baleen <- with(pair_ratio_stats,
  w[w >= 8e7][which.min(ratio_median[w >= 8e7])])

# Median ratio at each trough (y-anchor for pointer lines)
trough_minke_y  <- pair_ratio_stats$ratio_median[pair_ratio_stats$w == trough_minke]
trough_sperm_y  <- pair_ratio_stats$ratio_median[pair_ratio_stats$w == trough_sperm]
trough_baleen_y <- pair_ratio_stats$ratio_median[pair_ratio_stats$w == trough_baleen]

# x position for the label end of Minke/Sperm pointer lines (half the previous log offset)
trough_minke_lx  <- trough_minke  / 1.87
trough_sperm_lx  <- trough_sperm  / 1.87
# Baleen pointer goes upper-RIGHT from trough (flipped); compute line end and label position
trough_baleen_rx_line  <- trough_baleen * 1.05   # right end of angled line (~108 t, within 115 t limit)
trough_baleen_ry_line  <- trough_baleen_y + 0.15  # y at line end (angled up from trough)
trough_baleen_rx_label <- trough_baleen * 1.06   # label x (just past line tip)
trough_baleen_ry_label <- trough_baleen_y + 0.17  # label y (just above line tip)

cat(sprintf("  Trough positions: Minke=%.1f t (ratio=%.3f)  Sperm=%.1f t (ratio=%.3f)  Baleen=%.1f t (ratio=%.3f)\n",
            trough_minke/1e6,  trough_minke_y,
            trough_sperm/1e6,  trough_sperm_y,
            trough_baleen/1e6, trough_baleen_y))

# Functional group size boundaries (grams) — non-overlapping sequential bands
# Each band covers the size range up to that group's maximum body size
fg_krill_max     <- 4.173206   # Antarctic krill w_max
fg_pelagic_max   <- 603.7      # Bathypelagic fish w_max (largest other pelagic)
fg_comm_max      <- 1.5757e5   # Toothfish w_max (largest commercial fish)
# Marine mammal sequential bands (non-overlapping, starting from fg_comm_max)
fg_minke_max  <- 6.000e+06   # Minke whale w_max (6 t)
fg_orca_max   <- 1.063e+07   # Orca w_max (10.6 t)
fg_sperm_max  <- 3.650e+07   # Sperm whale w_max (36.5 t)
fg_baleen_max <- 1.030e+08   # Baleen whale w_max (103 t)
mm_fill  <- "#7bafd4"        # blue-grey fill for marine mammal bands
mm_alpha <- 0.18             # consistent alpha for all mammal bands

pair_large <- pair_ratio_stats %>% filter(w >= 10000)

# --- PLOT 1: Per-pair ratio, zoomed to >= 10 kg, annotated ---
p_pair <- ggplot(pair_large, aes(x = w)) +
  # Commercial fish band
  annotate("rect",
           xmin = 10000, xmax = fg_comm_max,
           ymin = -Inf,  ymax = Inf,
           fill = "grey70", alpha = 0.18) +
  annotate("text", x = sqrt(10000 * fg_comm_max), y = 1.24,
           label = "Commercial fish", size = 3.2, colour = "grey30", hjust = 0.5) +
  # Marine mammal sequential bands (Minke → Orca → Sperm → Baleen)
  annotate("rect",
           xmin = fg_comm_max, xmax = fg_minke_max,
           ymin = -Inf, ymax = Inf, fill = mm_fill, alpha = mm_alpha) +
  annotate("text", x = sqrt(fg_comm_max * fg_minke_max), y = 1.24,
           label = "Minke", size = 2.7, colour = "grey25", hjust = 0.5) +
  annotate("rect",
           xmin = fg_minke_max, xmax = fg_orca_max,
           ymin = -Inf, ymax = Inf, fill = mm_fill, alpha = mm_alpha * 1.4) +
  annotate("text", x = sqrt(fg_minke_max * fg_orca_max), y = 1.24,
           label = "Orca", size = 2.7, colour = "grey20", hjust = 0.5) +
  annotate("rect",
           xmin = fg_orca_max, xmax = fg_sperm_max,
           ymin = -Inf, ymax = Inf, fill = mm_fill, alpha = mm_alpha * 1.8) +
  annotate("text", x = sqrt(fg_orca_max * fg_sperm_max), y = 1.24,
           label = "Sperm", size = 2.7, colour = "grey15", hjust = 0.5) +
  annotate("rect",
           xmin = fg_sperm_max, xmax = fg_baleen_max,
           ymin = -Inf, ymax = Inf, fill = mm_fill, alpha = mm_alpha * 2.2) +
  annotate("text", x = sqrt(fg_sperm_max * fg_baleen_max), y = 1.24,
           label = "Baleen", size = 2.7, colour = "grey10", hjust = 0.5) +
  # Pointer lines (dark grey, thin): tip at trough, label at far end
  # Minke — horizontal, pointing right to trough
  annotate("segment",
           x = trough_minke_lx, xend = trough_minke,
           y = trough_minke_y,  yend = trough_minke_y,
           colour = "grey30", linewidth = 0.5) +
  # Sperm — horizontal, pointing right to trough
  annotate("segment",
           x = trough_sperm_lx, xend = trough_sperm,
           y = trough_sperm_y,  yend = trough_sperm_y,
           colour = "grey30", linewidth = 0.5) +
  # Baleen — angled upper-right from trough
  annotate("segment",
           x = trough_baleen,       xend = trough_baleen_rx_line,
           y = trough_baleen_y,     yend = trough_baleen_ry_line,
           colour = "grey30", linewidth = 0.5) +
  # Data ribbon and median line
  geom_ribbon(aes(ymin = ratio_q25, ymax = ratio_q75), fill = "#e41a1c", alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.8) +
  geom_line(aes(y = ratio_median), colour = "#e41a1c", linewidth = 1.2) +
  # Labels
  annotate("text", x = trough_minke_lx,      y = trough_minke_y,
           label = "Minke whales",  size = 3.2, hjust = 1, vjust = 0.5, colour = "grey20") +
  annotate("text", x = trough_sperm_lx,      y = trough_sperm_y,
           label = "Sperm whales",  size = 3.2, hjust = 1, vjust = 0.5, colour = "grey20") +
  annotate("text", x = trough_baleen_rx_label, y = trough_baleen_ry_label,
           label = "Baleen\nwhales", size = 3.2, hjust = 0, vjust = 0.5, colour = "grey20") +
  scale_x_log10(
    labels = function(x) {
      dplyr::case_when(
        x >= 1000000 ~ paste0(x/1000000, " t"),
        TRUE ~ paste0(x/1000, " kg")
      )
    },
    breaks = c(10000, 100000, 1000000, 10000000, 100000000),
    limits = c(10000, 1.15e8)   # extended to include baleen whale bin (103 t)
  ) +
  scale_y_continuous(
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2),
    limits = c(0, 1.3)
  ) +
  labs(
    title = "Fishing Impact on Large Organism Size Spectrum",
    subtitle = paste0("Per-pair ratio: Fished / Climate-Only | ", reference_name,
                      " | Zoomed to \u2265 10 kg"),
    x = "Body mass",
    y = "Abundance ratio (Fished / Unfished)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

ggsave(file.path(output_dir, "fished_vs_unfished_ratio_large_styled.png"),
       p_pair, width = 10, height = 6, dpi = 300)
cat("  Saved:", file.path(output_dir, "fished_vs_unfished_ratio_large_styled.png"), "\n")

# ==============================================================================
# PLOT 2: Full Size Spectrum (no size filter)
# Same per-pair ratio but across the entire size range
# ==============================================================================
cat("\n--- Plot 2: Full Size Spectrum Fished / Climate-Only Ratio ---\n")

w_min_plot <- 1  # 1 gram — minimum size for full-spectrum plot
pair_ratio_filt <- pair_ratio_stats %>% filter(w >= w_min_plot)

p_full <- ggplot(pair_ratio_filt, aes(x = w)) +
  # Grey shaded boxes: non-overlapping sequential functional-group bands
  # Band 1 — Krill
  annotate("rect",
           xmin = w_min_plot, xmax = fg_krill_max,
           ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.6) +
  annotate("text", x = sqrt(w_min_plot * fg_krill_max), y = 1.24,
           label = "Krill", size = 3.0, colour = "grey30", hjust = 0.5) +
  # Band 2 — Other pelagic fish
  annotate("rect",
           xmin = fg_krill_max, xmax = fg_pelagic_max,
           ymin = -Inf, ymax = Inf,
           fill = "grey75", alpha = 0.5) +
  annotate("text", x = sqrt(fg_krill_max * fg_pelagic_max), y = 1.24,
           label = "Other\npelagic fish", size = 3.0, colour = "grey30", hjust = 0.5) +
  # Band 3 — Commercial fish
  annotate("rect",
           xmin = fg_pelagic_max, xmax = fg_comm_max,
           ymin = -Inf, ymax = Inf,
           fill = "grey65", alpha = 0.4) +
  annotate("text", x = sqrt(fg_pelagic_max * fg_comm_max), y = 1.24,
           label = "Commercial\nfish", size = 3.0, colour = "grey20", hjust = 0.5) +
  # Marine mammal sequential bands (Minke → Orca → Sperm → Baleen)
  annotate("rect",
           xmin = fg_comm_max, xmax = fg_minke_max,
           ymin = -Inf, ymax = Inf, fill = mm_fill, alpha = mm_alpha) +
  annotate("text", x = sqrt(fg_comm_max * fg_minke_max), y = 1.24,
           label = "Minke", size = 2.7, colour = "grey25", hjust = 0.5) +
  annotate("rect",
           xmin = fg_minke_max, xmax = fg_orca_max,
           ymin = -Inf, ymax = Inf, fill = mm_fill, alpha = mm_alpha * 1.4) +
  annotate("text", x = sqrt(fg_minke_max * fg_orca_max), y = 1.24,
           label = "Orca", size = 2.7, colour = "grey20", hjust = 0.5) +
  annotate("rect",
           xmin = fg_orca_max, xmax = fg_sperm_max,
           ymin = -Inf, ymax = Inf, fill = mm_fill, alpha = mm_alpha * 1.8) +
  annotate("text", x = sqrt(fg_orca_max * fg_sperm_max), y = 1.24,
           label = "Sperm", size = 2.7, colour = "grey15", hjust = 0.5) +
  annotate("rect",
           xmin = fg_sperm_max, xmax = fg_baleen_max,
           ymin = -Inf, ymax = Inf, fill = mm_fill, alpha = mm_alpha * 2.2) +
  annotate("text", x = sqrt(fg_sperm_max * fg_baleen_max), y = 1.24,
           label = "Baleen", size = 2.7, colour = "grey10", hjust = 0.5) +
  # Pointer lines (dark grey, thin)
  annotate("segment",
           x = trough_minke_lx, xend = trough_minke,
           y = trough_minke_y,  yend = trough_minke_y,
           colour = "grey30", linewidth = 0.5) +
  annotate("segment",
           x = trough_sperm_lx, xend = trough_sperm,
           y = trough_sperm_y,  yend = trough_sperm_y,
           colour = "grey30", linewidth = 0.5) +
  annotate("segment",
           x = trough_baleen,       xend = trough_baleen_rx_line,
           y = trough_baleen_y,     yend = trough_baleen_ry_line,
           colour = "grey30", linewidth = 0.5) +
  # Data ribbon and median line
  geom_ribbon(aes(ymin = ratio_q25, ymax = ratio_q75), fill = "#e41a1c", alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.8) +
  geom_line(aes(y = ratio_median), colour = "#e41a1c", linewidth = 1.2) +
  # Labels
  annotate("text", x = trough_minke_lx,      y = trough_minke_y,
           label = "Minke whales",  size = 3.2, hjust = 1, vjust = 0.5, colour = "grey20") +
  annotate("text", x = trough_sperm_lx,      y = trough_sperm_y,
           label = "Sperm whales",  size = 3.2, hjust = 1, vjust = 0.5, colour = "grey20") +
  annotate("text", x = trough_baleen_rx_label, y = trough_baleen_ry_label,
           label = "Baleen\nwhales", size = 3.2, hjust = 0, vjust = 0.5, colour = "grey20") +
  scale_x_log10(
    labels = function(x) {
      dplyr::case_when(
        x >= 1e6 ~ paste0(x/1e6, " t"),
        x >= 1e3 ~ paste0(x/1e3, " kg"),
        TRUE     ~ paste0(round(x, 1), " g")
      )
    }
  ) +
  scale_y_continuous(
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2),
    limits = c(0, 1.3)
  ) +
  labs(
    title = "Fishing Impact Across Full Size Spectrum",
    subtitle = paste0("Per-pair ratio: Fished / Climate-Only | ", reference_name),
    x = "Body mass",
    y = "Abundance ratio (Fished / Unfished)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

ggsave(file.path(output_dir, "fished_vs_unfished_ratio_full_spectrum.png"),
       p_full, width = 12, height = 6, dpi = 300)
cat("  Saved:", file.path(output_dir, "fished_vs_unfished_ratio_full_spectrum.png"), "\n")

# ==============================================================================
# Summary statistics (based on per-pair ratio)
# ==============================================================================
cat("\n=== Summary Statistics (Per-pair ratio) ===\n")

pair_large_summary <- pair_large %>%
  mutate(
    size_cat = case_when(
      w < 1e5 ~ "10 kg - 100 kg",
      w < 1e6 ~ "100 kg - 1 t",
      w < 1e7 ~ "1 t - 10 t",
      w < 1e8 ~ "10 t - 100 t",
      TRUE ~ "> 100 t"
    )
  ) %>%
  group_by(size_cat) %>%
  summarise(
    mean_ratio = mean(ratio_median, na.rm = TRUE),
    min_ratio  = min(ratio_median,  na.rm = TRUE),
    max_ratio  = max(ratio_median,  na.rm = TRUE),
    .groups = 'drop'
  )

cat("\nFishing impact by size category (Fished/Unfished ratio):\n")
print(pair_large_summary)

whale_impact <- pair_large %>%
  filter(w >= 1e6) %>%
  summarise(
    mean_ratio = mean(ratio_median, na.rm = TRUE),
    min_ratio  = min(ratio_median,  na.rm = TRUE),
    max_ratio  = max(ratio_median,  na.rm = TRUE)
  )

cat("\nWhale-sized organisms (>= 1 tonne):\n")
cat("  Mean ratio:", round(whale_impact$mean_ratio, 3), "\n")
cat("  Range:", round(whale_impact$min_ratio, 3), "-", round(whale_impact$max_ratio, 3), "\n")
cat("  Interpretation: Fishing reduced whale-sized organisms to",
    round(whale_impact$mean_ratio * 100, 1), "% of unfished levels\n")

cat("\n=== Analysis Complete ===\n")

# ==============================================================================
# PLOT 3: Species-specific multi-panel (whole community + 4 whale species)
# 1 column x 5 rows, shared x axis (10 kg to 115 t)
# ==============================================================================
cat("\n--- Plot 3: Species-specific multipanel ---\n")

# Identify whale species names in the model (case-insensitive partial match)
find_sp <- function(pattern) {
  hit <- grep(pattern, sp_names, ignore.case = TRUE, value = TRUE)
  if (length(hit) == 0) stop(paste("No species matching:", pattern))
  hit[1]
}
sp_minke  <- find_sp("minke")
sp_orca   <- find_sp("orca")
sp_sperm  <- find_sp("sperm")
sp_baleen <- find_sp("baleen")
cat("  Matched species:", sp_minke, "|", sp_orca, "|", sp_sperm, "|", sp_baleen, "\n")

# Helper: compute per-pair ratio stats for a species
species_ratio_stats <- function(sp_name) {
  f_mat <- fished_sp_list[[sp_name]]
  c_mat <- climate_sp_list[[sp_name]]
  r_mat <- f_mat / c_mat
  r_mat[!is.finite(r_mat)] <- NA
  # Mask bins where <50% of simulations have non-zero abundance
  nonzero_frac <- colMeans(c_mat > 0, na.rm = TRUE)
  data.frame(
    w            = w_bins,
    ratio_median = ifelse(nonzero_frac >= 0.5, apply(r_mat, 2, median,   na.rm = TRUE), NA),
    ratio_q25    = ifelse(nonzero_frac >= 0.5, apply(r_mat, 2, quantile, probs = 0.25, na.rm = TRUE), NA),
    ratio_q75    = ifelse(nonzero_frac >= 0.5, apply(r_mat, 2, quantile, probs = 0.75, na.rm = TRUE), NA)
  )
}

stats_minke  <- species_ratio_stats(sp_minke)
stats_orca   <- species_ratio_stats(sp_orca)
stats_sperm  <- species_ratio_stats(sp_sperm)
stats_baleen <- species_ratio_stats(sp_baleen)

# Shared axis limits and formatting
x_lim_mp  <- c(1e4, 1.15e8)
x_breaks_mp <- c(1e4, 1e5, 1e6, 1e7, 1e8)
x_labels_mp <- function(x) dplyr::case_when(
  x >= 1e6 ~ paste0(x/1e6, " t"),
  TRUE     ~ paste0(x/1e3, " kg")
)
y_lim_mp <- c(0, 1.3)
y_breaks_mp <- c(0, 0.5, 1.0)

# Shared background annotation layers (marine mammal bands)
mm_bands <- list(
  annotate("rect", xmin = fg_comm_max, xmax = fg_minke_max,
            ymin = -Inf, ymax = Inf, fill = mm_fill, alpha = mm_alpha),
  annotate("rect", xmin = fg_minke_max, xmax = fg_orca_max,
            ymin = -Inf, ymax = Inf, fill = mm_fill, alpha = mm_alpha * 1.4),
  annotate("rect", xmin = fg_orca_max, xmax = fg_sperm_max,
            ymin = -Inf, ymax = Inf, fill = mm_fill, alpha = mm_alpha * 1.8),
  annotate("rect", xmin = fg_sperm_max, xmax = fg_baleen_max,
            ymin = -Inf, ymax = Inf, fill = mm_fill, alpha = mm_alpha * 2.2)
)

# Base theme for all panels
theme_mp <- theme_classic() +
  theme(
    plot.title   = element_text(size = 11, face = "bold"),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text    = element_text(size = 8),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    plot.margin  = margin(4, 8, 2, 4)
  )

# Function to build one ratio panel
build_panel <- function(stats_df, title_label, colour = "#e41a1c",
                        show_x_axis = FALSE) {
  df <- stats_df %>% filter(w >= x_lim_mp[1])
  p  <- ggplot(df, aes(x = w)) +
    annotate("rect", xmin = 1e4, xmax = fg_comm_max,
             ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.18) +
    mm_bands +
    geom_ribbon(aes(ymin = ratio_q25, ymax = ratio_q75), fill = colour, alpha = 0.3,
                na.rm = TRUE) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.6) +
    geom_line(aes(y = ratio_median), colour = colour, linewidth = 1.0, na.rm = TRUE) +
    scale_x_log10(labels = x_labels_mp, breaks = x_breaks_mp, limits = x_lim_mp) +
    scale_y_continuous(breaks = y_breaks_mp, limits = y_lim_mp) +
    labs(title = title_label,
         y = "Fished / Unfished",
         x = if (show_x_axis) "Body mass" else NULL) +
    theme_mp
  if (!show_x_axis) p <- p + theme(axis.text.x = element_blank(),
                                    axis.ticks.x = element_blank())
  p
}

p_comm_mp  <- build_panel(pair_ratio_stats,  "a) Whole community",  colour = "#333333")
p_minke_mp <- build_panel(stats_minke,       "b) Minke whales",      colour = "#2166ac")
p_orca_mp  <- build_panel(stats_orca,        "c) Orca",              colour = "#6a3d9a")
p_sperm_mp <- build_panel(stats_sperm,       "d) Sperm whales",      colour = "#e6550d")
p_baleen_mp <- build_panel(stats_baleen,     "e) Baleen whales",     colour = "#e41a1c",
                           show_x_axis = TRUE)

p_multipanel <- p_comm_mp / p_minke_mp / p_orca_mp / p_sperm_mp / p_baleen_mp

out_mp <- file.path(output_dir, "fished_vs_unfished_species_multipanel.png")
ggsave(out_mp, p_multipanel, width = 10, height = 14, dpi = 300)
cat("  Saved:", out_mp, "\n")

# ==============================================================================
# SECTION: Biomass functional group composition panels
# Bottom panels to pair with the size-spectrum ratio plots above.
# Each bar (stacked area) shows the PROPORTION of total biomass at each size
# bin contributed by each functional group, using ensemble-median abundances.
# ==============================================================================

cat("\n=== Biomass Functional Group Composition Panels ===\n\n")

# ---- Load model params for dw vector and species w_max ----
cat("Loading model params for dw and species w_max info...\n")
params_obj <- readRDS("params_sel_adj.RDS")
dw         <- params_obj@dw           # width of each w bin (g)
sp_wmax    <- setNames(
  mizer::species_params(params_obj)$w_max,
  as.character(mizer::species_params(params_obj)$species)
)
rm(params_obj)

# ---- Functional group assignment (by species w_max, matching shaded bands) ----
fg_levels_ordered <- c(
  "Krill & zooplankton", "Small pelagic fish", "Commercial fish",
  "Minke whales", "Orca", "Sperm whales", "Baleen whales"
)

fg_colours <- c(
  "Krill & zooplankton" = "#F9C74F",
  "Small pelagic fish"  = "#F3722C",
  "Commercial fish"     = "#7CAE7A",
  "Minke whales"        = "#C6DBF0",
  "Orca"                = "#84B4D4",
  "Sperm whales"        = "#4878A4",
  "Baleen whales"       = "#1A3F6B"
)

# Assign species to functional groups using the same w_max thresholds as the
# shaded bands in the ratio panels above
sp_groups <- cut(
  sp_wmax[sp_names],
  breaks        = c(0, fg_krill_max, fg_pelagic_max, fg_comm_max,
                    fg_minke_max, fg_orca_max, fg_sperm_max, Inf),
  labels        = fg_levels_ordered,
  include.lowest = TRUE
)

cat("  Species-to-functional-group assignment:\n")
for (sp in sp_names) {
  cat(sprintf("    %-35s -> %s\n", sp,
              as.character(sp_groups)[match(sp, sp_names)]))
}

# ---- Helper: compute functional-group median biomass from a species cache ----
# sp_list : named list of n_sims × n_w abundance matrices
# Returns a long data frame: w | functional_group | biomass
compute_fg_biomass_df <- function(sp_list, sp_names, sp_groups, w_bins, dw) {
  # Per-species ensemble-median abundance at each size bin
  sp_median_n <- sapply(sp_names, function(sp) {
    n_mat <- sp_list[[sp]]            # n_sims × n_w
    apply(n_mat, 2, median, na.rm = TRUE)
  })
  # sp_median_n is n_w × n_sp (sapply stacks column-wise)

  # Convert to biomass: N * w * dw
  bm_mat <- sweep(sp_median_n, 1, w_bins * dw, "*")  # n_w × n_sp

  # Aggregate by functional group
  fg_bm <- sapply(fg_levels_ordered, function(fg) {
    sp_in_fg <- sp_names[as.character(sp_groups) == fg]
    if (length(sp_in_fg) == 0) return(rep(0, length(w_bins)))
    if (length(sp_in_fg) == 1) return(bm_mat[, sp_in_fg])
    rowSums(bm_mat[, sp_in_fg, drop = FALSE])
  })
  # fg_bm is n_w × n_fg

  as.data.frame(fg_bm) %>%
    dplyr::mutate(w = w_bins) %>%
    tidyr::pivot_longer(-w, names_to = "functional_group", values_to = "biomass") %>%
    dplyr::mutate(functional_group = factor(functional_group,
                                            levels = fg_levels_ordered))
}

# ---- Helper: build proportional stacked area panel ----
build_biomass_panel <- function(bm_df, panel_title, show_x_axis = TRUE) {
  # Compute proportions (0-1) within each w bin, then cumulative for ribbons
  bm_prop <- bm_df %>%
    dplyr::group_by(w) %>%
    dplyr::mutate(
      total = sum(biomass, na.rm = TRUE),
      prop  = dplyr::if_else(total > 0, biomass / total, 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(w, functional_group) %>%
    dplyr::group_by(w) %>%
    dplyr::mutate(
      cum_upper = cumsum(prop),
      cum_lower = cum_upper - prop
    ) %>%
    dplyr::ungroup()

  # Background band annotations matching the ratio panels
  annot_bands <- list(
    annotate("rect", xmin = min(w_bins), xmax = fg_krill_max,
             ymin = 0, ymax = 1, fill = "grey85", alpha = 0.3),
    annotate("rect", xmin = fg_krill_max,   xmax = fg_pelagic_max,
             ymin = 0, ymax = 1, fill = "grey75", alpha = 0.25),
    annotate("rect", xmin = fg_pelagic_max, xmax = fg_comm_max,
             ymin = 0, ymax = 1, fill = "grey65", alpha = 0.2),
    annotate("rect", xmin = fg_comm_max,    xmax = fg_minke_max,
             ymin = 0, ymax = 1, fill = mm_fill, alpha = mm_alpha),
    annotate("rect", xmin = fg_minke_max,   xmax = fg_orca_max,
             ymin = 0, ymax = 1, fill = mm_fill, alpha = mm_alpha * 1.4),
    annotate("rect", xmin = fg_orca_max,    xmax = fg_sperm_max,
             ymin = 0, ymax = 1, fill = mm_fill, alpha = mm_alpha * 1.8),
    annotate("rect", xmin = fg_sperm_max,   xmax = fg_baleen_max,
             ymin = 0, ymax = 1, fill = mm_fill, alpha = mm_alpha * 2.2)
  )

  p <- ggplot(bm_prop, aes(x = w)) +
    annot_bands +
    geom_ribbon(
      aes(ymin = cum_lower, ymax = cum_upper, fill = functional_group),
      alpha = 0.92
    ) +
    scale_fill_manual(
      values = fg_colours,
      name   = "Functional group",
      drop   = FALSE
    ) +
    scale_x_log10(
      labels = function(x) {
        dplyr::case_when(
          x >= 1e6 ~ paste0(x / 1e6, " t"),
          x >= 1e3 ~ paste0(x / 1e3, " kg"),
          TRUE     ~ paste0(round(x, 1), " g")
        )
      }
    ) +
    scale_y_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1.0),
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1),
      expand = c(0, 0)
    ) +
    labs(
      title = panel_title,
      x     = if (show_x_axis) "Body mass" else NULL,
      y     = "Proportion of biomass"
    ) +
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

  if (!show_x_axis) {
    p <- p + theme(axis.text.x = element_blank(),
                   axis.ticks.x = element_blank())
  }
  p
}

# ---- Compute reference-period fished biomass by functional group ----
cat("Computing functional group biomass — fished reference period (2001-2010)...\n")
bm_fished_ref <- compute_fg_biomass_df(fished_sp_list, sp_names, sp_groups,
                                       w_bins, dw)
p_bm_ref <- build_biomass_panel(
  bm_fished_ref,
  panel_title  = "Biomass by functional group — Fished (2001-2010)",
  show_x_axis  = TRUE
)

out_bm_ref <- file.path(output_dir, "biomass_composition_ref_period.png")
ggsave(out_bm_ref, p_bm_ref, width = 12, height = 4.5, dpi = 300)
cat("  Saved:", out_bm_ref, "\n")

# ---- Combined Version 1: full-spectrum ratio + reference-period biomass ----
cat("Creating combined Version 1 figure (ratio 2001-2010 + biomass 2001-2010)...\n")

p_top_v1 <- p_full +
  labs(
    title    = "a) Fishing impact — Fished vs Climate-only (2001-2010)",
    subtitle = NULL
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

p_bottom_v1 <- p_bm_ref +
  labs(title = "b) Biomass by functional group — Fished (2001-2010)")

p_combined_v1 <- p_top_v1 / p_bottom_v1 +
  plot_layout(heights = c(2, 1))

out_v1 <- file.path(output_dir, "combined_v1_ref_period.png")
ggsave(out_v1, p_combined_v1, width = 12, height = 10.5, dpi = 300)
cat("  Saved:", out_v1, "\n")

# ==============================================================================
# SECTION: Pre-industrial baseline (1841-1860) extraction for Version 2
# ==============================================================================

cat("\n=== Pre-industrial baseline extraction (1841-1860) ===\n\n")

preindustrial_years <- 1841:1860
preindustrial_name  <- "Pre-industrial (1841-1860)"

preindustrial_comm_cache    <- file.path(output_dir,
                                         "spectra_cache_preindustrial.rds")
preindustrial_species_cache <- file.path(output_dir,
                                         "spectra_cache_species_preindustrial.rds")

if (file.exists(preindustrial_comm_cache) &&
    file.exists(preindustrial_species_cache)) {

  cat("Loading cached pre-industrial spectra...\n")
  pi_comm               <- readRDS(preindustrial_comm_cache)
  preindustrial_spectra <- pi_comm$climate_spectra
  cat("  Loaded:", nrow(preindustrial_spectra),
      "pre-industrial climate-only simulations\n\n")
  rm(pi_comm)

  pi_sp                 <- readRDS(preindustrial_species_cache)
  preindustrial_sp_list <- pi_sp$climate_sp
  cat("  Species cache loaded.\n\n")
  rm(pi_sp)

} else {
  cat("No pre-industrial cache found — extracting from climate-only ensemble.\n\n")

  climate_file     <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
  climate_ensemble <- readRDS(climate_file)
  n_climate_pi     <- climate_ensemble$n_successful
  cat("  Climate-only ensemble:", n_climate_pi, "simulations\n")

  preindustrial_spectra <- matrix(NA, nrow = n_climate_pi, ncol = n_w)
  pi_sp_list <- lapply(sp_names, function(s) matrix(NA, nrow = n_climate_pi, ncol = n_w))
  names(pi_sp_list) <- sp_names

  cat("  Extracting pre-industrial spectra (1841-1860)...\n")
  pb <- txtProgressBar(min = 0, max = n_climate_pi, style = 3)
  for (i in seq_len(n_climate_pi)) {
    setTxtProgressBar(pb, i)
    sim <- tryCatch(climate_ensemble$simulations[[i]], error = function(e) NULL)
    if (is.null(sim)) next
    comm <- tryCatch(extract_community_spectrum(sim, preindustrial_years),
                     error = function(e) NULL)
    if (!is.null(comm) && length(comm) == n_w)
      preindustrial_spectra[i, ] <- comm
    sp_mat <- tryCatch(extract_species_spectrum(sim, preindustrial_years),
                       error = function(e) NULL)
    if (!is.null(sp_mat) && ncol(sp_mat) == n_w) {
      for (j in seq_along(sp_names)) pi_sp_list[[sp_names[j]]][i, ] <- sp_mat[j, ]
    }
  }
  close(pb)
  rm(climate_ensemble); gc()
  cat("\n  Done.\n")

  saveRDS(
    list(w_bins = w_bins, n_w = n_w, climate_spectra = preindustrial_spectra),
    preindustrial_comm_cache
  )
  saveRDS(
    list(sp_names = sp_names, climate_sp = pi_sp_list),
    preindustrial_species_cache
  )
  cat("  Pre-industrial caches saved.\n\n")
  preindustrial_sp_list <- pi_sp_list
}

# ==============================================================================
# Version 2 top panel: fished (2001-2010) / pre-industrial (1841-1860) ratio
# Per-pair: fished sim_i at 2001-2010 divided by matched climate-only sim_i at
# 1841-1860 — removes parameter uncertainty while showing the combined effect
# of fishing AND climate change relative to pre-industrial baseline.
# ==============================================================================

cat("Computing fished (2001-2010) / pre-industrial (1841-1860) ratio...\n")

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

# Recompute whale-trough annotation positions for V2 ratio
trough_minke_v2  <- with(ratio_v2_stats,
  w[w > 4e6 & w < 1.5e7][which.min(ratio_median[w > 4e6 & w < 1.5e7])])
trough_sperm_v2  <- with(ratio_v2_stats,
  w[w > 2e7 & w < 8e7][which.min(ratio_median[w > 2e7 & w < 8e7])])
trough_baleen_v2 <- with(ratio_v2_stats,
  w[w >= 8e7][which.min(ratio_median[w >= 8e7])])

trough_minke_v2_y  <- ratio_v2_stats$ratio_median[ratio_v2_stats$w == trough_minke_v2]
trough_sperm_v2_y  <- ratio_v2_stats$ratio_median[ratio_v2_stats$w == trough_sperm_v2]
trough_baleen_v2_y <- ratio_v2_stats$ratio_median[ratio_v2_stats$w == trough_baleen_v2]

trough_minke_v2_lx       <- trough_minke_v2  / 1.87
trough_sperm_v2_lx       <- trough_sperm_v2  / 1.87
trough_baleen_v2_rx_line <- trough_baleen_v2 * 1.05
trough_baleen_v2_ry_line <- trough_baleen_v2_y + 0.15
trough_baleen_v2_rx_lbl  <- trough_baleen_v2 * 1.06
trough_baleen_v2_ry_lbl  <- trough_baleen_v2_y + 0.17

# Dynamic y-axis ceiling (ratio can exceed 1 if populations have grown)
v2_y_max <- max(
  ceiling(quantile(ratio_v2_stats$ratio_q75, 0.99, na.rm = TRUE) * 10) / 10 + 0.1,
  1.32
)
v2_y_max <- min(v2_y_max, 3.0)   # safety cap

p_v2_ratio <- ggplot(ratio_v2_stats %>% filter(w >= 1), aes(x = w)) +
  # Functional group bands
  annotate("rect", xmin = 1, xmax = fg_krill_max,
           ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.6) +
  annotate("text", x = sqrt(1 * fg_krill_max), y = v2_y_max * 0.96,
           label = "Krill", size = 3.0, colour = "grey30", hjust = 0.5) +
  annotate("rect", xmin = fg_krill_max,   xmax = fg_pelagic_max,
           ymin = -Inf, ymax = Inf, fill = "grey75", alpha = 0.5) +
  annotate("text", x = sqrt(fg_krill_max * fg_pelagic_max), y = v2_y_max * 0.96,
           label = "Other\npelagic fish", size = 3.0, colour = "grey30", hjust = 0.5) +
  annotate("rect", xmin = fg_pelagic_max, xmax = fg_comm_max,
           ymin = -Inf, ymax = Inf, fill = "grey65", alpha = 0.4) +
  annotate("text", x = sqrt(fg_pelagic_max * fg_comm_max), y = v2_y_max * 0.96,
           label = "Commercial\nfish", size = 3.0, colour = "grey20", hjust = 0.5) +
  annotate("rect", xmin = fg_comm_max,  xmax = fg_minke_max,
           ymin = -Inf, ymax = Inf, fill = mm_fill, alpha = mm_alpha) +
  annotate("text", x = sqrt(fg_comm_max * fg_minke_max), y = v2_y_max * 0.96,
           label = "Minke", size = 2.7, colour = "grey25", hjust = 0.5) +
  annotate("rect", xmin = fg_minke_max, xmax = fg_orca_max,
           ymin = -Inf, ymax = Inf, fill = mm_fill, alpha = mm_alpha * 1.4) +
  annotate("text", x = sqrt(fg_minke_max * fg_orca_max), y = v2_y_max * 0.96,
           label = "Orca", size = 2.7, colour = "grey20", hjust = 0.5) +
  annotate("rect", xmin = fg_orca_max,  xmax = fg_sperm_max,
           ymin = -Inf, ymax = Inf, fill = mm_fill, alpha = mm_alpha * 1.8) +
  annotate("text", x = sqrt(fg_orca_max * fg_sperm_max), y = v2_y_max * 0.96,
           label = "Sperm", size = 2.7, colour = "grey15", hjust = 0.5) +
  annotate("rect", xmin = fg_sperm_max, xmax = fg_baleen_max,
           ymin = -Inf, ymax = Inf, fill = mm_fill, alpha = mm_alpha * 2.2) +
  annotate("text", x = sqrt(fg_sperm_max * fg_baleen_max), y = v2_y_max * 0.96,
           label = "Baleen", size = 2.7, colour = "grey10", hjust = 0.5) +
  # Whale trough pointer lines
  annotate("segment",
           x    = trough_minke_v2_lx, xend = trough_minke_v2,
           y    = trough_minke_v2_y,  yend = trough_minke_v2_y,
           colour = "grey30", linewidth = 0.5) +
  annotate("segment",
           x    = trough_sperm_v2_lx, xend = trough_sperm_v2,
           y    = trough_sperm_v2_y,  yend = trough_sperm_v2_y,
           colour = "grey30", linewidth = 0.5) +
  annotate("segment",
           x    = trough_baleen_v2,        xend = trough_baleen_v2_rx_line,
           y    = trough_baleen_v2_y,      yend = trough_baleen_v2_ry_line,
           colour = "grey30", linewidth = 0.5) +
  # Data ribbon and median line (blue to distinguish from V1 red)
  geom_ribbon(aes(ymin = ratio_q25, ymax = ratio_q75),
              fill = "#4878A4", alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40",
             linewidth = 0.8) +
  geom_line(aes(y = ratio_median), colour = "#4878A4", linewidth = 1.2) +
  # Whale labels
  annotate("text", x = trough_minke_v2_lx,  y = trough_minke_v2_y,
           label = "Minke whales", size = 3.2, hjust = 1, vjust = 0.5,
           colour = "grey20") +
  annotate("text", x = trough_sperm_v2_lx,  y = trough_sperm_v2_y,
           label = "Sperm whales", size = 3.2, hjust = 1, vjust = 0.5,
           colour = "grey20") +
  annotate("text", x = trough_baleen_v2_rx_lbl, y = trough_baleen_v2_ry_lbl,
           label = "Baleen\nwhales", size = 3.2, hjust = 0, vjust = 0.5,
           colour = "grey20") +
  scale_x_log10(
    labels = function(x) {
      dplyr::case_when(
        x >= 1e6 ~ paste0(x / 1e6, " t"),
        x >= 1e3 ~ paste0(x / 1e3, " kg"),
        TRUE     ~ paste0(round(x, 1), " g")
      )
    }
  ) +
  scale_y_continuous(
    limits = c(0, v2_y_max),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    title    = "Fishing & Climate Impact on Size Spectrum",
    subtitle = paste0("Per-pair ratio: Fished (2001-2010) / ",
                      "Pre-industrial climate-only (1841-1860)"),
    x        = "Body mass",
    y        = "Abundance ratio (Fished 2001-2010 / Pre-industrial)"
  ) +
  theme_classic() +
  theme(
    plot.title         = element_text(size = 16, face = "bold"),
    plot.subtitle      = element_text(size = 12, color = "grey40"),
    axis.title         = element_text(size = 12),
    axis.text          = element_text(size = 11),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

ggsave(
  file.path(output_dir, "fished_vs_preindustrial_ratio_full_spectrum.png"),
  p_v2_ratio, width = 12, height = 6, dpi = 300
)
cat("  Saved:",
    file.path(output_dir, "fished_vs_preindustrial_ratio_full_spectrum.png"), "\n")

# ---- Pre-industrial biomass composition panel ----
cat("Computing functional group biomass — pre-industrial (1841-1860)...\n")
bm_preindustrial <- compute_fg_biomass_df(preindustrial_sp_list, sp_names,
                                          sp_groups, w_bins, dw)
p_bm_preindustrial <- build_biomass_panel(
  bm_preindustrial,
  panel_title = "Biomass by functional group — Pre-industrial climate-only (1841-1860)",
  show_x_axis = TRUE
)

out_bm_pi <- file.path(output_dir, "biomass_composition_preindustrial.png")
ggsave(out_bm_pi, p_bm_preindustrial, width = 12, height = 4.5, dpi = 300)
cat("  Saved:", out_bm_pi, "\n")

# ---- Combined Version 2: ratio + fished biomass + pre-industrial biomass ----
cat("Creating combined Version 2 figure (3 panels)...\n")

p_top_v2 <- p_v2_ratio +
  labs(
    title    = "a) Fishing & climate impact — Fished (2001-2010) vs Pre-industrial (1841-1860)",
    subtitle = NULL
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

p_mid_v2 <- p_bm_ref +
  labs(title = "b) Biomass by functional group — Fished (2001-2010)") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

p_bot_v2 <- p_bm_preindustrial +
  labs(title = "c) Biomass by functional group — Pre-industrial climate-only (1841-1860)")

p_combined_v2 <- p_top_v2 / p_mid_v2 / p_bot_v2 +
  plot_layout(heights = c(2, 1, 1))

out_v2 <- file.path(output_dir, "combined_v2_preindustrial.png")
ggsave(out_v2, p_combined_v2, width = 12, height = 14, dpi = 300)
cat("  Saved:", out_v2, "\n")

cat("\n=== All biomass composition figures complete ===\n")
cat("Figures saved to:", output_dir, "\n")
cat("  Standalone panels:\n")
cat("    biomass_composition_ref_period.png        (12 x 4.5 in)\n")
cat("    biomass_composition_preindustrial.png     (12 x 4.5 in)\n")
cat("    fished_vs_preindustrial_ratio_full_spectrum.png  (12 x 6 in)\n")
cat("  Combined multipanel:\n")
cat("    combined_v1_ref_period.png   (12 x 10.5 in) — ratio + fished biomass\n")
cat("    combined_v2_preindustrial.png (12 x 14 in)  — ratio + fished + pre-industrial\n")
