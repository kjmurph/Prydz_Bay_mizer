# =============================================================================
# Verify whether paired_data.rds has been updated with full-spectrum values
# =============================================================================
suppressPackageStartupMessages({ library(dplyr) })

base <- "Output_large_files/ecosystem_assessment"

cat("=== File timestamps ===\n")
for (f in c("fishing_metrics_raw.rds", "climate_only_metrics_raw.rds",
            "b0_reference.rds", "paired_data.rds")) {
  info <- file.info(file.path(base, f))
  cat(sprintf("  %-40s  %s  (%.1f MB)\n", f,
              format(info$mtime, "%Y-%m-%d %H:%M:%S"), info$size / 1e6))
}

# Load files
fish_raw <- readRDS(file.path(base, "fishing_metrics_raw.rds"))
clim_raw <- readRDS(file.path(base, "climate_only_metrics_raw.rds"))
b0_ref   <- readRDS(file.path(base, "b0_reference.rds"))
paired   <- readRDS(file.path(base, "paired_data.rds"))

cat("\n=== Dimensions ===\n")
cat("  fishing_metrics_raw:", nrow(fish_raw), "rows\n")
cat("  climate_only_metrics_raw:", nrow(clim_raw), "rows\n")
cat("  b0_reference:", nrow(b0_ref), "rows\n")
cat("  paired_data:", nrow(paired), "rows\n")

cat("\n=== Check: does paired_data contain spectrum columns? ===\n")
spec_cols <- grep("spectrum", names(paired), value = TRUE)
cat("  Columns containing 'spectrum':", paste(spec_cols, collapse = ", "), "\n")

cat("\n=== Spot-check: fishing_metrics_raw vs paired_data for sim_id=1, decade='1841-1850' ===\n")
fr1 <- fish_raw %>% filter(sim_id == 1, decade == "1841-1850")
pd1 <- paired %>% filter(sim_id == 1, decade == "1841-1850")
cat("  fishing_metrics_raw spectrum_slope:", fr1$spectrum_slope, "\n")
cat("  fishing_metrics_raw spectrum_intercept:", fr1$spectrum_intercept, "\n")

# In paired_data, fish columns have _fish suffix
if ("spectrum_slope_fish" %in% names(pd1)) {
  cat("  paired_data spectrum_slope_fish:", pd1$spectrum_slope_fish, "\n")
  cat("  paired_data spectrum_intercept_fish:", pd1$spectrum_intercept_fish, "\n")
  cat("  MATCH:", abs(fr1$spectrum_slope - pd1$spectrum_slope_fish) < 1e-10, "\n")
} else {
  cat("  WARNING: spectrum_slope_fish not found in paired_data\n")
}

# Check for backup comparison
backup_path <- file.path(base, "fishing_metrics_raw_pre_fullspectrum_backup.rds")
if (file.exists(backup_path)) {
  fish_old <- readRDS(backup_path)
  fr1_old <- fish_old %>% filter(sim_id == 1, decade == "1841-1850")
  cat("\n=== Backup (pre-fullspectrum) values for sim_id=1, decade='1841-1850' ===\n")
  cat("  OLD spectrum_slope:", fr1_old$spectrum_slope, "\n")
  cat("  NEW spectrum_slope:", fr1$spectrum_slope, "\n")
  cat("  Values differ:", abs(fr1$spectrum_slope - fr1_old$spectrum_slope) > 1e-10, "\n")
}

# Check b0_reference
cat("\n=== b0_reference spectrum values (first 3 sims) ===\n")
print(b0_ref %>% select(sim_id, spectrum_slope, spectrum_intercept) %>% head(3))

cat("\n=== Summary: spectrum_slope distributions ===\n")
cat("  fishing_raw  slope: ", summary(fish_raw$spectrum_slope), "\n")
cat("  climate_raw  slope: ", summary(clim_raw$spectrum_slope), "\n")
cat("  b0_reference slope: ", summary(b0_ref$spectrum_slope), "\n")
if ("spectrum_slope_fish" %in% names(paired)) {
  cat("  paired fish slope: ", summary(paired$spectrum_slope_fish), "\n")
  cat("  paired clim slope: ", summary(paired$spectrum_slope_clim), "\n")
}
