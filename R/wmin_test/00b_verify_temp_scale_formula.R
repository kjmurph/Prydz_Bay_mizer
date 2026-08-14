# Scratch: recover the exact therMizer temperature-effect formula by
# reproducing the stored species_params$encounterpred_scale.
# therMizer::setEncounterPredScale sets encounterpred_scale to the maximum of the
# unscaled polynomial over a 0.1-degree grid spanning [temp_min, temp_max].
suppressPackageStartupMessages(library(mizer))

ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
sp <- ens$fished_top10pct[[1]]@params@species_params

cands <- list(
  "K273_sqrt"    = function(T, tmin, tmax) T * (T - (tmin + 273)) * sqrt((tmax + 273) - T),
  "K273.15_sqrt" = function(T, tmin, tmax) T * (T - (tmin + 273.15)) * sqrt((tmax + 273.15) - T),
  "K273_lin"     = function(T, tmin, tmax) T * (T - (tmin + 273)) * ((tmax + 273) - T),
  "C_sqrt"       = function(T, tmin, tmax) T * (T - tmin) * sqrt(tmax - T),
  "C_lin"        = function(T, tmin, tmax) T * (T - tmin) * (tmax - T)
)
offsets <- c("K273_sqrt" = 273, "K273.15_sqrt" = 273.15, "K273_lin" = 273,
             "C_sqrt" = 0, "C_lin" = 0)

res <- data.frame(species = sp$species, stored = sp$encounterpred_scale)
for (nm in names(cands)) {
  f <- cands[[nm]]; off <- offsets[[nm]]
  res[[nm]] <- vapply(seq_len(nrow(sp)), function(i) {
    grid <- seq(sp$temp_min[i], sp$temp_max[i], by = 0.1) + off
    max(f(grid, sp$temp_min[i], sp$temp_max[i]))
  }, numeric(1))
}
print(res, digits = 8)

cat("\n--- max abs relative error per candidate ---\n")
for (nm in names(cands))
  cat(sprintf("%-14s %.3e\n", nm, max(abs(res[[nm]] - res$stored) / res$stored)))
