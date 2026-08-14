# =============================================================================
# KC00 -- krill-fishing counterfactual scenarios on the phase-54 reference model
#
# THE QUESTION
#   How did krill fishing shape Antarctic krill consumption by predators, and
#   what does that imply for whale recovery? Two counterfactuals bracket the
#   observed history: one with the krill fishery removed entirely, one with it
#   held at its peak intensity for the rest of the record.
#
# FOUR ARMS, all projected from the SAME unfished spin-up state, so every
# difference between them is attributable to the effort forcing alone.
#
#   unexploited   effort = 0 throughout. The denominator for every ratio.
#   exploited     the observed effort array, unchanged. The baseline.
#   no_krill      observed effort with the antarctic krill column set to 0 in
#                 every year. Every other fishery (whaling, toothfish, squid,
#                 finfish) runs exactly as observed.
#   peak_krill    observed effort up to and including 1978, then the PEAK krill
#                 effort (1.0, reached 1979) held every year from 1979 to 2010.
#                 Every other fishery unchanged.
#
#   Observed krill fishing is a brief 1974-1996 pulse: it rises to 1.0 in 1979
#   then collapses to ~0.019 the following year and never recovers. So the two
#   counterfactuals are not symmetric around it -- observed history is much
#   closer to no_krill (~2.5 cumulative effort-years) than to peak_krill (~33).
#   That asymmetry is the point, and the figures state it.
#
# PROTOCOL, following phase 54 / 44_rebuild_from_base.R:
#   NO steady() CALL. The phase-54 params is already a converged steady state at
#   tol 0.002, tighter than the 0.0025 the ensemble used. Re-entering steady()
#   on an already-calibrated object is the documented trap that halves the
#   unexploited baleen whale stock (docs/SNR_regression_FINDING.md); the
#   spin-up here is unfished, so going straight to it is both correct and exact.
#   Then: 118-year unfished spin-up (1 cycle, as the production ensemble used),
#   then project each arm with t_max omitted so mizer takes the horizon from the
#   effort array's own time dimension.
#
# KRILL CONSUMPTION is computed with ther_diet() from
# R/wmin_test/thermizer_shim.R, which takes `year` EXPLICITLY. mizer's own
# getDiet(proportion = FALSE) defaults to t = 0, indexes ocean_temp at
# 0 + t_idx = -1841, and returns absolute rates 13-80x too large -- the bug that
# invalidated whale_consumption_outputs/. Proportions are unaffected; absolute
# consumption, which is what this script needs, is not.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC00_run_scenarios.R"
# ENV    KC_PARAMS (default params_ref_sw2000_balror_mnkfish05.rds)
#        KC_DIET_FROM (default 1900), KC_OUT
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(dplyr)
})
source(file.path("R", "wmin_test", "thermizer_shim.R"))

KC_PARAMS <- Sys.getenv("KC_PARAMS", "params_ref_sw2000_balror_mnkfish05.rds")
KC_ROOT   <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
DIET_FROM <- as.integer(Sys.getenv("KC_DIET_FROM", "1900"))
ANA <- file.path(KC_ROOT, "analysis")
dir.create(ANA, recursive = TRUE, showWarnings = FALSE)

SPINUP_YEARS <- 118      # 09_Uncertainty_Analysis.Rmd production setting
KRILL <- "antarctic krill"
END_YEAR <- 2010

t0 <- proc.time()
cat("=== KC00: krill-fishing counterfactuals ===\n")
cat("params:", KC_PARAMS, "| mizer", as.character(packageVersion("mizer")),
    "| therMizer", as.character(packageVersion("therMizer")), "\n")

p <- suppressWarnings(validParams(readRDS(KC_PARAMS)))
stopifnot(identical(p@rates_funcs$Encounter, "therMizerEncounter"),
          identical(p@resource_dynamics, "plankton_forcing"),
          identical(p@second_order_w$flux, "upwind"))
SP <- p@species_params$species
i_kr <- which(SP == KRILL); stopifnot(length(i_kr) == 1)
cat("species:", length(SP), "| sperm beta", signif(p@species_params$beta[
  which(SP == "sperm whales")], 6), "| baleen beta",
  signif(p@species_params$beta[which(SP == "baleen whales")], 6), "\n")

# --- build the four effort arrays ---------------------------------------------
eff_obs <- readRDS("effort_array_1841_2010.rds")
stopifnot(identical(colnames(eff_obs), SP))
yrs_eff <- as.numeric(rownames(eff_obs))
kr_eff <- eff_obs[, KRILL]
peak_year <- yrs_eff[which.max(kr_eff)]
peak_val  <- max(kr_eff)
active <- range(yrs_eff[kr_eff > 0])
cat(sprintf("krill fishery: active %d-%d, peak %.5f in %d, cumulative %.3f effort-years\n",
            active[1], active[2], peak_val, peak_year, sum(kr_eff)))

eff_none <- eff_obs; eff_none[, KRILL] <- 0
eff_peak <- eff_obs
eff_peak[yrs_eff >= peak_year, KRILL] <- peak_val
cat(sprintf("counterfactual cumulative krill effort-years: no_krill %.3f, peak_krill %.3f (%.1fx observed)\n",
            sum(eff_none[, KRILL]), sum(eff_peak[, KRILL]),
            sum(eff_peak[, KRILL]) / sum(kr_eff)))
# every other gear must be untouched
stopifnot(identical(eff_none[, setdiff(SP, KRILL)], eff_obs[, setdiff(SP, KRILL)]),
          identical(eff_peak[, setdiff(SP, KRILL)], eff_obs[, setdiff(SP, KRILL)]),
          all(eff_peak[yrs_eff < peak_year, KRILL] ==
                eff_obs[yrs_eff < peak_year, KRILL]))

# --- unfished spin-up, shared by every arm ------------------------------------
cat("\nunfished spin-up:", SPINUP_YEARS, "years...\n")
s0 <- project(p, t_start = 1841, t_max = SPINUP_YEARS, effort = 0)
init <- s0@n[SPINUP_YEARS, , ]
cat(sprintf("  post-spin-up total biomass %.4g g | baleen %.4g g\n",
            sum(init %*% (p@w * p@dw)),
            (init %*% (p@w * p@dw))[which(SP == "baleen whales")]))

# --- project the four arms ----------------------------------------------------
ARMS <- list(unexploited = 0, exploited = eff_obs, no_krill = eff_none,
             peak_krill = eff_peak)
SIMS <- list()
for (nm in names(ARMS)) {
  cat("projecting", nm, "...")
  e <- ARMS[[nm]]
  SIMS[[nm]] <- if (identical(e, 0))
    project(p, initial_n = init, t_start = 1841, t_max = 169, effort = 0)
  else project(p, initial_n = init, t_start = 1841, effort = e)
  yr <- as.numeric(dimnames(SIMS[[nm]]@n)$time)
  cat(" years", min(yr), "-", max(yr), "\n")
}
# exploited runs end 2010, unexploited can carry an extra year -- clip both
keep_of <- function(s) which(as.numeric(dimnames(s@n)$time) <= END_YEAR)

# --- per-species biomass, every arm, every year -------------------------------
cat("\nextracting biomass...\n")
wdw <- p@w * p@dw
BIO <- do.call(rbind, lapply(names(SIMS), function(nm) {
  s <- SIMS[[nm]]; ki <- keep_of(s)
  yr <- as.numeric(dimnames(s@n)$time)[ki]
  do.call(rbind, lapply(seq_along(ki), function(j) data.frame(
    arm = nm, Year = yr[j], Species = SP,
    biomass_g = as.numeric(s@n[ki[j], , ] %*% wdw), stringsAsFactors = FALSE)))
}))

# --- krill consumption per predator, per year ---------------------------------
# transcribed from Manuscript scripts/F00_build_rebuilt167_data.R:213-227
cat("extracting krill consumption from", DIET_FROM, "...\n")
KR <- do.call(rbind, lapply(names(SIMS), function(nm) {
  s <- SIMS[[nm]]
  yr <- as.numeric(dimnames(s@n)$time)
  ys <- yr[yr >= DIET_FROM & yr <= END_YEAR]
  do.call(rbind, lapply(ys, function(y) {
    ti <- which(yr == y)
    n <- s@n[ti, , ]; npp <- s@n_pp[ti, ]
    d <- ther_diet(s@params, n = n, n_pp = npp,
                   n_other = s@params@initial_n_other, year = y)
    cons <- rowSums(d[, , i_kr, drop = TRUE] * n *
                      rep(p@dw, each = length(SP)))
    data.frame(arm = nm, Year = y, Species = SP, krill_consumed = cons,
               stringsAsFactors = FALSE)
  }))
}))

# --- krill catch actually taken in each arm -----------------------------------
cat("extracting yield...\n")
YLD <- do.call(rbind, lapply(names(SIMS), function(nm) {
  s <- SIMS[[nm]]; y <- getYield(s); ki <- keep_of(s)
  data.frame(arm = nm, Year = as.numeric(rownames(y))[ki],
             krill_yield_g = as.numeric(y[ki, KRILL]),
             stringsAsFactors = FALSE)
}))

# --- sanity gates -------------------------------------------------------------
cat("\n--- gates ---\n")
# 1. no_krill must take zero krill catch; unexploited must take none of anything
z1 <- max(YLD$krill_yield_g[YLD$arm == "no_krill"])
z2 <- max(YLD$krill_yield_g[YLD$arm == "unexploited"])
cat(sprintf("  krill yield in no_krill %.4g, in unexploited %.4g (both must be 0)\n",
            z1, z2))
stopifnot(z1 == 0, z2 == 0)
# 2. arms must be IDENTICAL to exploited before krill fishing starts
pre <- BIO$Year < active[1]
for (nm in c("no_krill", "peak_krill")) {
  a <- BIO$biomass_g[BIO$arm == nm & pre]
  b <- BIO$biomass_g[BIO$arm == "exploited" & pre]
  d <- max(abs(a / b - 1))
  cat(sprintf("  %-11s vs exploited before %d: max rel diff %.3g\n",
              nm, active[1], d))
  stopifnot(d < 1e-12)
}
# 3. peak_krill must remove strictly more krill than exploited
tot <- tapply(YLD$krill_yield_g, YLD$arm, sum)
cat(sprintf("  cumulative krill catch (t): exploited %.4g, peak_krill %.4g (%.1fx)\n",
            tot[["exploited"]] / 1e6, tot[["peak_krill"]] / 1e6,
            tot[["peak_krill"]] / tot[["exploited"]]))
stopifnot(tot[["peak_krill"]] > tot[["exploited"]])

# --- write --------------------------------------------------------------------
meta <- list(params = KC_PARAMS, spinup_years = SPINUP_YEARS,
             peak_year = peak_year, peak_val = peak_val,
             krill_active = active, diet_from = DIET_FROM,
             cum_effort = c(exploited = sum(kr_eff), no_krill = 0,
                            peak_krill = sum(eff_peak[, KRILL])),
             built = format(Sys.time()))
saveRDS(list(biomass = BIO, krill = KR, yield = YLD, meta = meta),
        file.path(ANA, "KC00_scenarios.rds"))
write.csv(BIO, file.path(ANA, "KC00_biomass.csv"), row.names = FALSE)
write.csv(KR,  file.path(ANA, "KC00_krill_consumption.csv"), row.names = FALSE)
write.csv(YLD, file.path(ANA, "KC00_krill_yield.csv"), row.names = FALSE)

cat("\nwrote KC00_scenarios.rds and 3 csvs in", ANA, "\n")
cat("elapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")