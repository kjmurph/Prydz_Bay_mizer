# A2_inventory.R -- static inventory of the candidate notebooks (read-only)
#
# Usage: Rscript --vanilla R/model_construction/A2_inventory.R [<repo> <out_dir>]
#   (defaults: ASSESS_REPO or "."; ASSESS_OUT or Output_large_files/model_construction)
#
# Tags every line inside an R chunk (Rmd) or script (R) that does I/O, a
# calibration step, or an interactive/random step, and records whether the
# line is live code or commented out. Writes inventory_lines.csv and
# inventory_summary.csv to <out_dir>.

args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args) >= 1) args[1] else Sys.getenv("ASSESS_REPO", ".")
out  <- if (length(args) >= 2) args[2] else Sys.getenv("ASSESS_OUT", "Output_large_files/model_construction")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

nb <- c("00_Tidying IWC Southern Hemisphere data.Rmd",
        "01_FishMIP_Fishing_Data.Rmd",
        "02_Preparing_Climate_Forcings.Rmd",
        "03_model_setup_pre_therMizer.rmd",
        "04_therMizer_calibration_scale_g_m2.Rmd",
        "05_therMizer_calibration_scale_model_domain.Rmd",
        "06_steady_state_therMizer.Rmd",
        "07_Calibrate_New_Reference_Period_pre_1961.Rmd",
        "08_ISIMIP3a_simulations_Prydz_Bay.Rmd",
        "09_Uncertainty_Analysis.Rmd",
        "optim_model_setup_old/model_setup_v4.Rmd",
        "model_setup_old/New_steady_state.Rmd",
        "model_setup_old/New_steady_state_07_06_2024.Rmd",
        "group params/1g_simplified_groups_params.Rmd",
        "interaction matrix/2g_trait_groups_interaction_matrix_vCWC.R")

pats <- list(
  read        = "readRDS\\(|read\\.csv\\(|read\\.table\\(|read_csv\\(|read_excel\\(|st_read\\(|read_sf\\(|nc_open\\(|load\\(",
  write       = "saveRDS\\(|write\\.csv\\(|write\\.table\\(|write_csv\\(|ggsave\\(|save\\(",
  build       = "newMultispeciesParams\\(|newTraitParams\\(|upgradeTherParams\\(|upgradeParams\\(|setPredKernel\\(|setResource\\(|setRateFunction\\(|scaleModel\\(",
  calibrate   = "steady\\(|projectToSteady\\(|matchBiomasses\\(|calibrateBiomass\\(|matchGrowth\\(|matchYields\\(|calibrateYield\\(|setBevertonHolt\\(",
  interactive = "tuneParams\\(|tuneGrowth\\(|shiny|runApp\\(",
  optimiser   = "optim\\(|optimParallel\\(|optimx\\(|calibrar|nloptr|DEoptim",
  random      = "set\\.seed\\(|runif\\(|rnorm\\(|rlnorm\\(|sample\\(",
  library     = "library\\(|require\\(|install_github\\(|pak::")

rows <- list(); summ <- list()
for (f in nb) {
  path <- file.path(repo, f)
  if (!file.exists(path)) { message("missing: ", f); next }
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  is_rmd <- grepl("\\.[Rr]md$", f)
  in_chunk <- !is_rmd
  chunk_open <- 0L
  for (i in seq_along(x)) {
    l <- x[i]
    if (is_rmd && grepl("^\\s*```\\s*\\{r", l)) { in_chunk <- TRUE; chunk_open <- chunk_open + 1L; next }
    if (is_rmd && grepl("^\\s*```\\s*$", l)) { in_chunk <- FALSE; next }
    if (!in_chunk) next
    commented <- grepl("^\\s*#", l)
    for (cat in names(pats)) {
      if (grepl(pats[[cat]], l)) {
        rows[[length(rows) + 1]] <- data.frame(
          file = f, line = i, category = cat,
          state = if (commented) "commented" else "live",
          text = trimws(substr(l, 1, 160)), stringsAsFactors = FALSE)
      }
    }
  }
  r <- if (length(rows)) do.call(rbind, rows) else data.frame()
  rf <- r[r$file == f, , drop = FALSE]
  cnt <- function(cat, st) sum(rf$category == cat & rf$state == st)
  summ[[f]] <- data.frame(
    file = f, lines = length(x), chunks = chunk_open,
    read_live = cnt("read", "live"), write_live = cnt("write", "live"),
    write_commented = cnt("write", "commented"),
    build_live = cnt("build", "live"), calibrate_live = cnt("calibrate", "live"),
    tuneParams_live = cnt("interactive", "live"),
    optimiser_live = cnt("optimiser", "live"), random_live = cnt("random", "live"),
    stringsAsFactors = FALSE)
}
lines <- do.call(rbind, rows)
summ <- do.call(rbind, summ); rownames(summ) <- NULL
write.csv(lines, file.path(out, "inventory_lines.csv"), row.names = FALSE)
write.csv(summ, file.path(out, "inventory_summary.csv"), row.names = FALSE)
options(width = 200)
print(summ, right = FALSE)
cat("\nlibraries (live):\n")
lib <- lines[lines$category == "library" & lines$state == "live", ]
pk <- unique(unlist(regmatches(lib$text, gregexpr("(?<=library\\()[A-Za-z0-9.]+|(?<=require\\()[A-Za-z0-9.]+", lib$text, perl = TRUE))))
print(sort(pk))
