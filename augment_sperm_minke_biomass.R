###############################################################################
# augment_sperm_minke_biomass.R
#
# Targeted extraction script: adds sperm_biomass and minke_biomass columns
# to the existing raw data and paired_data caches WITHOUT re-running the
# full ecosystem assessment pipeline.
#
# Loads the ensemble files once and extracts ONLY the two new biomass metrics
# for each simulation/decade, then patches the existing RDS files.
###############################################################################

cat("=============================================================\n")
cat("AUGMENTING DATA: Adding sperm_biomass & minke_biomass\n")
cat("=============================================================\n\n")

# Source ecosystem_assessment_v2.R for all function definitions and constants
# (it only defines functions, doesn't auto-execute)
source("ecosystem_assessment_v2.R")

start_time <- Sys.time()

# ---------------------------------------------------------------------------
# 1. Load ensemble files
# ---------------------------------------------------------------------------
cat("Loading fishing ensemble (this may take a few minutes)...\n")
mc_fish <- readRDS(ENSEMBLE_PATHS$fishing)
cat("Loading climate-only ensemble...\n")
mc_clim <- readRDS(ENSEMBLE_PATHS$climate_only)

extract_sims <- function(mc) {
  if ("simulations" %in% names(mc)) return(mc$simulations)
  if (is.list(mc) && inherits(mc[[1]], "MizerSim")) return(mc)
  stop("Unrecognized ensemble structure")
}

fish_sims <- extract_sims(mc_fish)
clim_sims <- extract_sims(mc_clim)
n_sims <- min(length(fish_sims), length(clim_sims))
cat(sprintf("  Found %d paired simulations\n\n", n_sims))

# ---------------------------------------------------------------------------
# 2. Extract sperm_biomass and minke_biomass for each sim/decade + B0
# ---------------------------------------------------------------------------
cat("Extracting sperm_biomass and minke_biomass per decade...\n")

fish_new <- list()
clim_new <- list()
b0_new <- list()
errors <- 0

pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
for (i in seq_len(n_sims)) {
  setTxtProgressBar(pb, i)
  
  # --- Per-decade metrics (fishing) ---
  tryCatch({
    sim <- fish_sims[[i]]
    times <- as.numeric(dimnames(sim@n)$time)
    for (d in seq_len(nrow(DECADES))) {
      decade <- DECADES[d, ]
      time_range <- which(times >= decade$start & times <= decade$end)
      if (length(time_range) == 0) next
      fish_new[[length(fish_new) + 1]] <- data.frame(
        sim_id = i, decade = decade$label,
        sperm_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$sperm_whales),
        minke_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$minke_whales),
        stringsAsFactors = FALSE
      )
    }
  }, error = function(e) { errors <<- errors + 1 })
  
  # --- Per-decade metrics (climate-only) ---
  tryCatch({
    sim <- clim_sims[[i]]
    times <- as.numeric(dimnames(sim@n)$time)
    for (d in seq_len(nrow(DECADES))) {
      decade <- DECADES[d, ]
      time_range <- which(times >= decade$start & times <= decade$end)
      if (length(time_range) == 0) next
      clim_new[[length(clim_new) + 1]] <- data.frame(
        sim_id = i, decade = decade$label,
        sperm_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$sperm_whales),
        minke_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$minke_whales),
        stringsAsFactors = FALSE
      )
    }
  }, error = function(e) { errors <<- errors + 1 })
  
  # --- B0 reference (from climate-only, 1841-1860) ---
  tryCatch({
    sim <- clim_sims[[i]]
    times <- as.numeric(dimnames(sim@n)$time)
    time_range <- which(times >= B0_PERIOD[1] & times <= B0_PERIOD[2])
    if (length(time_range) > 0) {
      b0_new[[length(b0_new) + 1]] <- data.frame(
        sim_id = i,
        sperm_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$sperm_whales),
        minke_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$minke_whales),
        stringsAsFactors = FALSE
      )
    }
  }, error = function(e) { errors <<- errors + 1 })
}
close(pb)
cat(sprintf("\n  Extraction complete. Errors: %d\n", errors))

# Free ensemble memory
rm(mc_fish, mc_clim, fish_sims, clim_sims); gc()

fish_new_df <- bind_rows(fish_new)
clim_new_df <- bind_rows(clim_new)
b0_new_df <- bind_rows(b0_new)
cat(sprintf("  New fishing rows: %d, climate rows: %d, B0 rows: %d\n\n",
            nrow(fish_new_df), nrow(clim_new_df), nrow(b0_new_df)))

# ---------------------------------------------------------------------------
# 3. Load existing cached data and merge new columns
# ---------------------------------------------------------------------------
cat("Loading existing cached data...\n")
fishing_raw <- readRDS(file.path(OUTPUT_DIR_LARGE, "fishing_metrics_raw.rds"))
climate_raw <- readRDS(file.path(OUTPUT_DIR_LARGE, "climate_only_metrics_raw.rds"))
b0_reference <- readRDS(file.path(OUTPUT_DIR_LARGE, "b0_reference.rds"))
empirical_thresholds <- readRDS(file.path(OUTPUT_DIR_LARGE, "empirical_thresholds.rds"))

cat(sprintf("  Existing fishing_raw: %d rows x %d cols\n", nrow(fishing_raw), ncol(fishing_raw)))

# Remove old sperm/minke columns if they exist (from any prior partial run)
for (col in c("sperm_biomass", "minke_biomass")) {
  if (col %in% names(fishing_raw)) fishing_raw[[col]] <- NULL
  if (col %in% names(climate_raw)) climate_raw[[col]] <- NULL
  if (col %in% names(b0_reference)) b0_reference[[col]] <- NULL
}

# Merge new columns into raw data
fishing_raw <- fishing_raw %>%
  left_join(fish_new_df, by = c("sim_id", "decade"))
climate_raw <- climate_raw %>%
  left_join(clim_new_df, by = c("sim_id", "decade"))
b0_reference <- b0_reference %>%
  left_join(b0_new_df, by = "sim_id")

cat(sprintf("  Updated fishing_raw: %d rows x %d cols\n", nrow(fishing_raw), ncol(fishing_raw)))
cat(sprintf("  Updated climate_raw: %d rows x %d cols\n", nrow(climate_raw), ncol(climate_raw)))
cat(sprintf("  Updated b0_reference: %d rows x %d cols\n\n", nrow(b0_reference), ncol(b0_reference)))

# Quick check
cat("  Sperm biomass range (fishing):", range(fishing_raw$sperm_biomass, na.rm = TRUE), "\n")
cat("  Minke biomass range (fishing):", range(fishing_raw$minke_biomass, na.rm = TRUE), "\n")
cat("  Sperm biomass range (B0):", range(b0_reference$sperm_biomass, na.rm = TRUE), "\n")
cat("  Minke biomass range (B0):", range(b0_reference$minke_biomass, na.rm = TRUE), "\n\n")

# ---------------------------------------------------------------------------
# 4. Recompute paired comparisons (uses all BIOMASS_METRICS including new ones)
# ---------------------------------------------------------------------------
cat("Recomputing paired comparisons...\n")
paired_data <- compute_paired_comparisons(fishing_raw, climate_raw, b0_reference, empirical_thresholds)
cat(sprintf("  Paired data: %d rows x %d cols\n", nrow(paired_data), ncol(paired_data)))

# Verify new ratio columns exist
new_cols <- grep("sperm_biomass|minke_biomass", names(paired_data), value = TRUE)
cat(sprintf("  New columns found: %s\n\n", paste(new_cols, collapse = ", ")))

# ---------------------------------------------------------------------------
# 5. Save updated data files
# ---------------------------------------------------------------------------
cat("Saving updated data files...\n")
saveRDS(fishing_raw, file.path(OUTPUT_DIR_LARGE, "fishing_metrics_raw.rds"))
saveRDS(climate_raw, file.path(OUTPUT_DIR_LARGE, "climate_only_metrics_raw.rds"))
saveRDS(b0_reference, file.path(OUTPUT_DIR_LARGE, "b0_reference.rds"))
saveRDS(paired_data, file.path(OUTPUT_DIR_LARGE, "paired_data.rds"))

# Also save to standard output dir
saveRDS(fishing_raw, file.path(OUTPUT_DIR, "fishing_metrics_raw.rds"))
saveRDS(climate_raw, file.path(OUTPUT_DIR, "climate_only_metrics_raw.rds"))
saveRDS(b0_reference, file.path(OUTPUT_DIR, "b0_reference.rds"))

end_time <- Sys.time()
cat(sprintf("\n=============================================================\n"))
cat(sprintf("AUGMENTATION COMPLETE (%.1f minutes)\n",
            difftime(end_time, start_time, units = "mins")))
cat(sprintf("New columns: sperm_biomass, minke_biomass (+ ratio derivatives)\n"))
cat("=============================================================\n")
cat("\nNow run replot_existing_outputs.R to regenerate heatmaps.\n")
