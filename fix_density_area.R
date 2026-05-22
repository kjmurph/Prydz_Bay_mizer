###############################################################################
# Fix MODEL_DOMAIN_AREA bug in cached stats CSVs
#
# Bug: density columns were computed with 1.95e+13 m^2 (climate forcing grid
#      extent) instead of the correct therMizer calibration domain area of
#      1.474341e+12 m^2 — a factor of ~13.2x too large denominator.
#
# Fix: recompute median_density and mean_density from the raw gram columns
#      (median, mean) which are unaffected.
#
# Total/tonne values are NOT affected. Only *_density columns need correction.
###############################################################################

CORRECT_AREA   <- 1.474341e+12   # m^2 — therMizer calibration domain
INCORRECT_AREA <- 1.95e+13       # m^2 — was mistakenly used
CORRECTION_FACTOR <- INCORRECT_AREA / CORRECT_AREA

cat("=============================================================\n")
cat("Fixing MODEL_DOMAIN_AREA in cached stats CSVs\n")
cat("=============================================================\n")
cat(sprintf("  Wrong area:   %.6e m^2\n", INCORRECT_AREA))
cat(sprintf("  Correct area: %.6e m^2\n", CORRECT_AREA))
cat(sprintf("  Correction factor (density * ): %.4f\n\n", CORRECTION_FACTOR))

csv_files <- list.files("whale_consumption_outputs",
                         pattern = "_stats\\.csv$",
                         full.names = TRUE)

cat(sprintf("Found %d stats CSV files to check\n\n", length(csv_files)))

corrected <- 0
skipped   <- 0

for (f in csv_files) {
  d <- read.csv(f, stringsAsFactors = FALSE)

  has_density <- all(c("median_density", "mean_density") %in% names(d))
  has_raw     <- all(c("median", "mean") %in% names(d))

  if (!has_density) {
    cat(sprintf("  SKIP (no density cols): %s\n", basename(f)))
    skipped <- skipped + 1
    next
  }

  if (!has_raw) {
    cat(sprintf("  SKIP (no raw gram cols): %s\n", basename(f)))
    skipped <- skipped + 1
    next
  }

  old_med <- d$median_density[1]
  d$median_density <- d$median / CORRECT_AREA
  d$mean_density   <- d$mean   / CORRECT_AREA
  new_med <- d$median_density[1]

  write.csv(d, f, row.names = FALSE)

  cat(sprintf("  FIXED: %-58s  x%.2f\n",
              basename(f), new_med / old_med))
  corrected <- corrected + 1
}

cat(sprintf("\nResult: %d files corrected, %d skipped\n", corrected, skipped))
cat("\nNOTE: The following require re-running their extraction scripts\n")
cat("      to update NetCDF density outputs (FishMIP submissions):\n")
cat("        - extract_fishmip_outputs.R\n")
cat("        - extract_fishmip_outputs_climate_only.R\n")
cat("\nAll R scripts have been updated to use 1.474341e+12 m^2.\n")
