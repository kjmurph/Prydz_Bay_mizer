###############################################################################
# Quick test: verify parallel chunking works with 2 workers on 10 sims
###############################################################################

N_WORKERS <- 2
N_TEST_SIMS <- 10

suppressPackageStartupMessages({
  library(mizer)
  library(therMizer)
  library(parallel)
  library(dplyr)
})

source("ecosystem_assessment_v2.R")
ASSESSMENT_SCRIPT <- normalizePath("ecosystem_assessment_v2.R")

cat("=== PARALLEL MINI-TEST: 2 workers, 10 sims ===\n\n")
start_time <- Sys.time()

# Load ensembles
cat("Loading ensembles...\n")
mc_fish <- readRDS(ENSEMBLE_PATHS$fishing)
fish_sims <- if ("simulations" %in% names(mc_fish)) mc_fish$simulations else mc_fish
rm(mc_fish); gc(verbose = FALSE)

mc_clim <- readRDS(ENSEMBLE_PATHS$climate_only)
clim_sims <- if ("simulations" %in% names(mc_clim)) mc_clim$simulations else mc_clim
rm(mc_clim); gc(verbose = FALSE)
cat(sprintf("  Loaded %d fishing, %d climate sims\n", length(fish_sims), length(clim_sims)))

# Use only first N_TEST_SIMS
fish_sims <- fish_sims[1:N_TEST_SIMS]
clim_sims <- clim_sims[1:N_TEST_SIMS]

# Split into 2 chunks and save
chunk_dir  <- file.path(tempdir(), "test_parallel_chunks")
result_dir <- file.path(tempdir(), "test_parallel_results")
if (!dir.exists(chunk_dir))  dir.create(chunk_dir, recursive = TRUE)
if (!dir.exists(result_dir)) dir.create(result_dir, recursive = TRUE)

chunk_indices <- split(seq_len(N_TEST_SIMS),
                       cut(seq_len(N_TEST_SIMS), N_WORKERS, labels = FALSE))

work_items <- list()
for (k in seq_along(chunk_indices)) {
  idx <- chunk_indices[[k]]
  chunk_file  <- file.path(chunk_dir, sprintf("chunk_%02d.rds", k))
  result_file <- file.path(result_dir, sprintf("result_%02d.rds", k))
  saveRDS(list(fish_sims = fish_sims[idx], clim_sims = clim_sims[idx], sim_ids = idx),
          chunk_file)
  work_items[[k]] <- list(chunk_file = chunk_file, result_file = result_file)
  cat(sprintf("  Chunk %d: sims %d-%d\n", k, min(idx), max(idx)))
}

rm(fish_sims, clim_sims); gc(verbose = FALSE)

# Launch workers
cat(sprintf("\nLaunching %d workers...\n", N_WORKERS))
t0 <- Sys.time()
cl <- makeCluster(N_WORKERS)
clusterExport(cl, "ASSESSMENT_SCRIPT", envir = environment())

msgs <- parLapply(cl, work_items, function(item) {
  suppressPackageStartupMessages({
    library(mizer); library(therMizer); library(dplyr)
  })
  source(ASSESSMENT_SCRIPT)
  chunk <- readRDS(item$chunk_file)
  fish_sims <- chunk$fish_sims
  clim_sims <- chunk$clim_sims
  sim_ids   <- chunk$sim_ids
  rm(chunk); gc(verbose = FALSE)

  fishing_results <- vector("list", length(sim_ids))
  climate_results <- vector("list", length(sim_ids))
  b0_results      <- vector("list", length(sim_ids))

  for (i in seq_along(sim_ids)) {
    sid <- sim_ids[i]
    # Fishing
    fishing_results[[i]] <- tryCatch({
      sim <- fish_sims[[i]]; times <- as.numeric(dimnames(sim@n)$time)
      rows <- vector("list", nrow(DECADES))
      for (d in seq_len(nrow(DECADES))) {
        dec <- DECADES[d,]; tr <- which(times >= dec$start & times <= dec$end)
        if (length(tr) == 0) next
        spec <- calculate_spectrum_slope_intercept(sim, tr)
        rows[[d]] <- data.frame(sim_id=sid, decade=dec$label,
          start_year=dec$start, end_year=dec$end,
          spectrum_slope=unname(spec["slope"]),
          spectrum_intercept=unname(spec["intercept"]),
          production_biomass_ratio=calculate_production_biomass_ratio(sim, tr),
          spectrum_mle_exponent=calculate_spectrum_mle_exponent(sim, tr),
          fish_pb_ratio=calculate_production_biomass_ratio(sim, tr, species_group=SPECIES_GROUPS$fish),
          marine_mammal_pb_ratio=calculate_production_biomass_ratio(sim, tr, species_group=SPECIES_GROUPS$marine_mammals),
          sperm_biomass=calculate_group_biomass(sim, tr, SPECIES_GROUPS$sperm_whales),
          minke_biomass=calculate_group_biomass(sim, tr, SPECIES_GROUPS$minke_whales),
          stringsAsFactors=FALSE)
      }
      bind_rows(rows)
    }, error = function(e) NULL)
    # Climate
    climate_results[[i]] <- tryCatch({
      sim <- clim_sims[[i]]; times <- as.numeric(dimnames(sim@n)$time)
      rows <- vector("list", nrow(DECADES))
      for (d in seq_len(nrow(DECADES))) {
        dec <- DECADES[d,]; tr <- which(times >= dec$start & times <= dec$end)
        if (length(tr) == 0) next
        spec <- calculate_spectrum_slope_intercept(sim, tr)
        rows[[d]] <- data.frame(sim_id=sid, decade=dec$label,
          start_year=dec$start, end_year=dec$end,
          spectrum_slope=unname(spec["slope"]),
          spectrum_intercept=unname(spec["intercept"]),
          production_biomass_ratio=calculate_production_biomass_ratio(sim, tr),
          spectrum_mle_exponent=calculate_spectrum_mle_exponent(sim, tr),
          fish_pb_ratio=calculate_production_biomass_ratio(sim, tr, species_group=SPECIES_GROUPS$fish),
          marine_mammal_pb_ratio=calculate_production_biomass_ratio(sim, tr, species_group=SPECIES_GROUPS$marine_mammals),
          sperm_biomass=calculate_group_biomass(sim, tr, SPECIES_GROUPS$sperm_whales),
          minke_biomass=calculate_group_biomass(sim, tr, SPECIES_GROUPS$minke_whales),
          stringsAsFactors=FALSE)
      }
      bind_rows(rows)
    }, error = function(e) NULL)
    # B0
    b0_results[[i]] <- tryCatch({
      sim <- clim_sims[[i]]; times <- as.numeric(dimnames(sim@n)$time)
      tr <- which(times >= B0_PERIOD[1] & times <= B0_PERIOD[2])
      if (length(tr) == 0) return(NULL)
      spec <- calculate_spectrum_slope_intercept(sim, tr)
      data.frame(sim_id=sid,
        spectrum_slope=unname(spec["slope"]),
        spectrum_intercept=unname(spec["intercept"]),
        production_biomass_ratio=calculate_production_biomass_ratio(sim, tr),
        spectrum_mle_exponent=calculate_spectrum_mle_exponent(sim, tr),
        fish_pb_ratio=calculate_production_biomass_ratio(sim, tr, species_group=SPECIES_GROUPS$fish),
        marine_mammal_pb_ratio=calculate_production_biomass_ratio(sim, tr, species_group=SPECIES_GROUPS$marine_mammals),
        sperm_biomass=calculate_group_biomass(sim, tr, SPECIES_GROUPS$sperm_whales),
        minke_biomass=calculate_group_biomass(sim, tr, SPECIES_GROUPS$minke_whales),
        stringsAsFactors=FALSE)
    }, error = function(e) NULL)
  }

  result <- list(fishing=bind_rows(fishing_results), climate=bind_rows(climate_results),
                 b0=bind_rows(b0_results))
  saveRDS(result, item$result_file)
  sprintf("OK: %d fish, %d clim, %d B0 rows", nrow(result$fishing), nrow(result$climate), nrow(result$b0))
})

stopCluster(cl)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("Workers finished in %.1f sec\n", elapsed))
for (m in msgs) cat(sprintf("  %s\n", m))

# Merge and validate
rfiles <- list.files(result_dir, pattern="result_.*\\.rds$", full.names=TRUE)
all_f <- bind_rows(lapply(rfiles, function(f) readRDS(f)$fishing))
all_c <- bind_rows(lapply(rfiles, function(f) readRDS(f)$climate))
all_b <- bind_rows(lapply(rfiles, function(f) readRDS(f)$b0))

cat(sprintf("\nMerged: %d fishing rows, %d climate rows, %d B0 rows\n",
            nrow(all_f), nrow(all_c), nrow(all_b)))
cat(sprintf("Expected: %d rows (%d sims x %d decades), %d B0 rows\n",
            N_TEST_SIMS * nrow(DECADES), N_TEST_SIMS, nrow(DECADES), N_TEST_SIMS))

# Check all 8 columns present and non-NA
PATCH_COLS <- c("spectrum_slope", "spectrum_intercept", "production_biomass_ratio",
                "spectrum_mle_exponent", "fish_pb_ratio", "marine_mammal_pb_ratio",
                "sperm_biomass", "minke_biomass")
all_ok <- TRUE
for (col in PATCH_COLS) {
  n_na <- sum(is.na(all_f[[col]]))
  status <- if (n_na == 0) "OK" else sprintf("%d NAs", n_na)
  cat(sprintf("  %-30s %s\n", col, status))
  if (n_na > 0) all_ok <- FALSE
}

# Cleanup
unlink(chunk_dir, recursive = TRUE)
unlink(result_dir, recursive = TRUE)

total <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat(sprintf("\n%s — Total: %.0f sec, Parallel: %.0f sec\n",
            if (all_ok) "ALL TESTS PASSED" else "SOME NAs DETECTED", total, elapsed))

# Extrapolate
per_sim_parallel <- elapsed / N_TEST_SIMS
cat(sprintf("Per-sim (2 workers): %.1f sec → 2111 sims estimated: %.0f sec (%.1f min, %.1f hrs)\n",
            per_sim_parallel, per_sim_parallel * 2111, per_sim_parallel * 2111 / 60,
            per_sim_parallel * 2111 / 3600))
