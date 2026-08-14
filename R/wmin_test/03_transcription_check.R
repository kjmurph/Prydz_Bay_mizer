# =============================================================================
# Close the one remaining validation gap: is ther_diet() a faithful
# transcription of mizer::getDiet()?
#
# mizer::getDiet() cannot run on these objects because params@rates_funcs$
# Encounter names "therMizerEncounter", which does not exist here. Define a stub
# that is exactly mizerEncounter (i.e. temperature effect = 1) and register it
# globally; then getDiet() runs, and the shim called with temp_eff = 1 must
# reproduce it bit for bit.
#
# Also prints the diet composition of baleen whales and small divers, as a
# sanity check on which prey these groups actually take in this model.
# =============================================================================

source("R/wmin_test/thermizer_shim.R")

# Stub: no temperature effect. Deliberately NOT used anywhere else.
therMizerEncounter <- function(params, n, n_pp, n_other, t, ...) {
  mizer::mizerEncounter(params, n = n, n_pp = n_pp, n_other = n_other, t = t, ...)
}
therMizerPredRate <- function(params, n, n_pp, n_other, t, feeding_level, ...) {
  mizer::mizerPredRate(params, n = n, n_pp = n_pp, n_other = n_other, t = t,
                       feeding_level = feeding_level, ...)
}

ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
sim <- ens$fished_top10pct[[1]]
p   <- sim@params
yr  <- 2005
st  <- sim_state_at(sim, yr)

one <- setNames(rep(1, nrow(p@species_params)), p@species_params$species)
fl_plain <- mizer::mizerFeedingLevel(
  p, n = st$n, n_pp = st$n_pp, n_other = st$n_other, t = 0,
  encounter = mizer::mizerEncounter(p, n = st$n, n_pp = st$n_pp,
                                    n_other = st$n_other, t = 0))
shim_plain <- ther_diet(p, st$n, st$n_pp, st$n_other, yr,
                        temp_eff = one, feeding_level = fl_plain)

mz <- mizer::getDiet(p, n = st$n, n_pp = st$n_pp, n_other = st$n_other,
                     proportion = FALSE)

cat("dim shim:", dim(shim_plain), " dim mizer:", dim(mz), "\n")
d   <- max(abs(shim_plain - mz))
rel <- d / max(abs(mz))
cat("max abs diff :", format(d, digits = 6), "\n")
cat("max rel diff :", format(rel, digits = 6),
    if (rel < 1e-12) "   -> TRANSCRIPTION VERIFIED\n" else "   -> DIFFERS\n")

# --- diet composition sanity check -------------------------------------------
te <- ther_temp_effect(p, yr)
fl <- ther_feeding_level(p, st$n, st$n_pp, st$n_other, yr, temp_eff = te)
diet <- ther_diet(p, st$n, st$n_pp, st$n_other, yr, temp_eff = te,
                  feeding_level = fl)

pop_diet <- function(pred) {
  v <- colSums(diet[pred, , ] * st$n[pred, ] * p@dw)
  data.frame(prey = names(v), g_per_yr = as.numeric(v),
             pct = 100 * as.numeric(v) / sum(v))
}
for (pred in c("baleen whales", "small divers")) {
  cat("\n--- population diet of", pred, "at", yr, "(member 1) ---\n")
  dd <- pop_diet(pred)
  print(dd[order(-dd$pct), ][1:8, ], row.names = FALSE, digits = 5)
}

cat("\n--- biomass at", yr, "(g), member 1 ---\n")
bm <- rowSums(sweep(st$n, 2, p@w * p@dw, "*"))
print(format(bm, digits = 4, scientific = TRUE), quote = FALSE)
