# =============================================================================
# Final validation: confirm all files are consistent post-fix
# =============================================================================
suppressPackageStartupMessages({ library(dplyr) })

base <- "Output_large_files/ecosystem_assessment"
out  <- "ecosystem_assessment_outputs"

cat("=== File timestamps (should all be recent) ===\n")
for (f in c("fishing_metrics_raw.rds", "climate_only_metrics_raw.rds",
            "b0_reference.rds", "paired_data.rds", "empirical_thresholds.rds")) {
  info <- file.info(file.path(base, f))
  cat(sprintf("  %-45s %s\n", f, format(info$mtime, "%Y-%m-%d %H:%M:%S")))
}

# Load all
fish_raw <- readRDS(file.path(base, "fishing_metrics_raw.rds"))
clim_raw <- readRDS(file.path(base, "climate_only_metrics_raw.rds"))
b0_ref   <- readRDS(file.path(base, "b0_reference.rds"))
paired   <- readRDS(file.path(base, "paired_data.rds"))
thresh   <- readRDS(file.path(base, "empirical_thresholds.rds"))

cat("\n=== Consistency checks ===\n")

# Check 1: paired_data spectrum_slope_fish matches fishing_metrics_raw
check1 <- fish_raw %>%
  select(sim_id, decade, spectrum_slope) %>%
  inner_join(paired %>% select(sim_id, decade, spectrum_slope_fish),
             by = c("sim_id", "decade")) %>%
  mutate(match = abs(spectrum_slope - spectrum_slope_fish) < 1e-10)

cat(sprintf("  Check 1: fishing_raw vs paired_data spectrum_slope:\n"))
cat(sprintf("    Total: %d, Match: %d, Mismatch: %d\n",
            nrow(check1), sum(check1$match), sum(!check1$match)))

# Check 2: paired_data spectrum_slope_clim matches climate_only_metrics_raw
check2 <- clim_raw %>%
  select(sim_id, decade, spectrum_slope) %>%
  inner_join(paired %>% select(sim_id, decade, spectrum_slope_clim),
             by = c("sim_id", "decade")) %>%
  mutate(match = abs(spectrum_slope - spectrum_slope_clim) < 1e-10)

cat(sprintf("  Check 2: climate_raw vs paired_data spectrum_slope:\n"))
cat(sprintf("    Total: %d, Match: %d, Mismatch: %d\n",
            nrow(check2), sum(check2$match), sum(!check2$match)))

# Check 3: b0_reference spectrum_slope matches empirical_thresholds
b0_slopes <- b0_ref$spectrum_slope[!is.na(b0_ref$spectrum_slope)]
cat(sprintf("  Check 3: B0 spectrum_slope median: %.6f, Threshold median: %.6f\n",
            median(b0_slopes), thresh$spectrum_slope$median))
cat(sprintf("    Match: %s\n",
            abs(median(b0_slopes) - thresh$spectrum_slope$median) < 1e-10))

# Check 4: values are in full-spectrum range (not old filtered range ~-1.36)
cat(sprintf("\n  Check 4: Value ranges (full-spectrum should be ~ -1.08, old was ~ -1.36)\n"))
cat(sprintf("    fishing_raw slope median:      %.4f\n", median(fish_raw$spectrum_slope, na.rm=TRUE)))
cat(sprintf("    climate_raw slope median:      %.4f\n", median(clim_raw$spectrum_slope, na.rm=TRUE)))
cat(sprintf("    paired_data fish slope median:  %.4f\n", median(paired$spectrum_slope_fish, na.rm=TRUE)))
cat(sprintf("    paired_data clim slope median:  %.4f\n", median(paired$spectrum_slope_clim, na.rm=TRUE)))
cat(sprintf("    b0_reference slope median:      %.4f\n", median(b0_ref$spectrum_slope, na.rm=TRUE)))

# Check 5: heatmap PNGs are newer than paired_data
cat("\n=== Heatmap timestamps (should be after paired_data) ===\n")
paired_time <- file.info(file.path(base, "paired_data.rds"))$mtime
for (f in c("heatmap_combined_absolute.png", "heatmap_structural_outside_b0_90.png",
            "heatmap_structural_zscore_absolute.png")) {
  info <- file.info(file.path(out, f))
  cat(sprintf("  %-50s %s  %s\n", f,
              format(info$mtime, "%Y-%m-%d %H:%M:%S"),
              ifelse(info$mtime >= paired_time, "OK", "STALE")))
}

cat("\n=== VALIDATION COMPLETE ===\n")
