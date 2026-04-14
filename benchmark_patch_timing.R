suppressPackageStartupMessages({library(mizer);library(therMizer)})
source("ecosystem_assessment_v2.R")

cat("Loading ensembles...\n")
mc_fish <- readRDS(ENSEMBLE_PATHS[["fishing"]])
fish_sims <- if ("simulations" %in% names(mc_fish)) mc_fish[["simulations"]] else mc_fish
mc_clim <- readRDS(ENSEMBLE_PATHS[["climate_only"]])
clim_sims <- if ("simulations" %in% names(mc_clim)) mc_clim[["simulations"]] else mc_clim
cat(sprintf("  %d fishing, %d climate sims loaded\n", length(fish_sims), length(clim_sims)))

# Patch extract function (same as in patch_2111_ensemble.R)
extract_patch_metrics <- function(sim) {
  times <- as.numeric(dimnames(sim@n)$time)
  results <- list()
  for (i in seq_len(nrow(DECADES))) {
    decade <- DECADES[i, ]
    time_range <- which(times >= decade$start & times <= decade$end)
    if (length(time_range) == 0) next
    spec <- calculate_spectrum_slope_intercept(sim, time_range)
    results[[i]] <- data.frame(
      decade = decade$label,
      spectrum_slope = unname(spec["slope"]),
      spectrum_intercept = unname(spec["intercept"]),
      production_biomass_ratio = calculate_production_biomass_ratio(sim, time_range),
      spectrum_mle_exponent = calculate_spectrum_mle_exponent(sim, time_range),
      fish_pb_ratio = calculate_production_biomass_ratio(sim, time_range,
                                                         species_group = SPECIES_GROUPS$fish),
      marine_mammal_pb_ratio = calculate_production_biomass_ratio(sim, time_range,
                                                                   species_group = SPECIES_GROUPS$marine_mammals),
      sperm_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$sperm_whales),
      minke_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$minke_whales),
      stringsAsFactors = FALSE
    )
  }
  return(dplyr::bind_rows(results))
}

extract_patch_b0 <- function(sim) {
  times <- as.numeric(dimnames(sim@n)$time)
  time_range <- which(times >= B0_PERIOD[1] & times <= B0_PERIOD[2])
  if (length(time_range) == 0) return(NULL)
  spec <- calculate_spectrum_slope_intercept(sim, time_range)
  c(
    spectrum_slope = unname(spec["slope"]),
    spectrum_intercept = unname(spec["intercept"]),
    production_biomass_ratio = calculate_production_biomass_ratio(sim, time_range),
    spectrum_mle_exponent = calculate_spectrum_mle_exponent(sim, time_range),
    fish_pb_ratio = calculate_production_biomass_ratio(sim, time_range,
                                                       species_group = SPECIES_GROUPS$fish),
    marine_mammal_pb_ratio = calculate_production_biomass_ratio(sim, time_range,
                                                                 species_group = SPECIES_GROUPS$marine_mammals),
    sperm_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$sperm_whales),
    minke_biomass = calculate_group_biomass(sim, time_range, SPECIES_GROUPS$minke_whales)
  )
}

# Benchmark 5 sims
N_BENCH <- 5
cat(sprintf("\nBenchmarking %d sims...\n", N_BENCH))
sim_times <- numeric(N_BENCH)
for (s in 1:N_BENCH) {
  t0 <- Sys.time()
  extract_patch_metrics(fish_sims[[s]])
  extract_patch_metrics(clim_sims[[s]])
  extract_patch_b0(clim_sims[[s]])
  sim_times[s] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("  Sim %d: %.1f sec\n", s, sim_times[s]))
}

mean_t <- mean(sim_times)
cat(sprintf("\n=== RESULTS ===\n"))
cat(sprintf("Per-sim (mean): %.2f sec\n", mean_t))
cat(sprintf("Per-sim (range): %.2f - %.2f sec\n", min(sim_times), max(sim_times)))
cat(sprintf("\n=== EXTRAPOLATION: 2111 sims ===\n"))
load_overhead <- 40  # measured above
total_compute <- mean_t * 2111
total_sec <- load_overhead + total_compute
cat(sprintf("Load overhead:   ~40 sec\n"))
cat(sprintf("Compute time:    %.0f sec (%.1f min, %.1f hours)\n", total_compute, total_compute/60, total_compute/3600))
cat(sprintf("Total estimated: %.0f sec (%.1f min, %.1f hours)\n", total_sec, total_sec/60, total_sec/3600))
