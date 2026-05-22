###############################################################################
# Compare old (wrong area) vs new (correct area) FishMIP bsp/csp outputs
###############################################################################

OLD <- "FishMIP_ISIMIP3a_submission/fishmip_outputs_species"
NEW <- "FishMIP_ISIMIP3a_submission/fishmip_outputs_species_test"

fname <- function(soc, var)
  sprintf("mizer_gfdl-mom6-cobalt2_obsclim_%s_default_%s_prydz-bay_annual_1841_2010.csv",
          soc, var)

expected_ratio <- 1.95e+13 / 1.474341e+12

cat("==========================================================\n")
cat("bsp/csp area correction comparison\n")
cat(sprintf("Expected ratio (old / new): %.6f\n", expected_ratio))
cat(sprintf("  old area: 1.95e+13 m^2 (wrong)\n"))
cat(sprintf("  new area: 1.474341e+12 m^2 (correct)\n"))
cat("==========================================================\n\n")

checks <- list(
  list(soc = "histsoc", var = "bsp"),
  list(soc = "histsoc", var = "csp"),
  list(soc = "nat",     var = "bsp")
)

all_ok <- TRUE

for (ch in checks) {
  tag     <- paste0(ch$soc, "/", ch$var)
  f_old   <- file.path(OLD, ch$soc, fname(ch$soc, ch$var))
  f_new   <- file.path(NEW, ch$soc, fname(ch$soc, ch$var))

  old <- read.csv(f_old, stringsAsFactors = FALSE)
  new <- read.csv(f_new, stringsAsFactors = FALSE)

  # Use only non-zero old values to avoid 0/0 division
  ok <- old$median > 0 & !is.na(old$median) & !is.na(new$median)

  ratios <- new$median[ok] / old$median[ok]

  cat(sprintf("--- %s ---\n", tag))
  cat(sprintf("  rows        : %d (old)  /  %d (new)\n", nrow(old), nrow(new)))
  cat(sprintf("  mean(old)   : %12.6e g/m^2\n", mean(old$median, na.rm = TRUE)))
  cat(sprintf("  mean(new)   : %12.6e g/m^2\n", mean(new$median, na.rm = TRUE)))
  cat(sprintf("  ratio mean  : %.6f  (expected %.6f)\n", mean(ratios), expected_ratio))
  cat(sprintf("  ratio range : %.6f – %.6f\n", min(ratios), max(ratios)))

  ok_flag <- abs(mean(ratios) - expected_ratio) / expected_ratio < 1e-6
  cat(sprintf("  RESULT      : %s\n\n", if (ok_flag) "PASS - ratio matches expected" else "FAIL - unexpected ratio"))
  all_ok <- all_ok & ok_flag
}

cat("==========================================================\n")
if (all_ok) {
  cat("ALL PASS: New outputs are exactly 13.23x larger than old outputs.\n")
  cat("The old bsp/csp files ARE affected and NEED updating.\n")
} else {
  cat("WARNING: Some ratios did not match - review above.\n")
}
cat("==========================================================\n")
