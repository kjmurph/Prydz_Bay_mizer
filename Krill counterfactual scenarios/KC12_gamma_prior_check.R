# =============================================================================
# KC12 -- do the ensemble's realised gamma multipliers respect the documented
# prior clamp?
#
# 09_Uncertainty_Analysis.Rmd:358-369 draws gamma as a lognormal multiplier and
# then clamps it to [0.8 x original, 100 x original]:
#
#     lower <- original_gamma[j] * 0.8   # do not reduce below 80% of baseline
#     upper <- original_gamma[j] * 100
#     sp_params$gamma[j] <- original_gamma[j] * mult
#     sp_params$gamma[j] <- min(upper, max(lower, sp_params$gamma[j]))
#
# But the top-10 cut-A members carry minke gamma at 1708x the base, and one
# species-member at 0.42x. This script measures the realised multiplier over all
# 1,848 draw-distinct members and reports how many fall outside the documented
# window, and for which species.
#
# It is a MEASUREMENT, not an accusation. The accepted ensemble came from 33
# Monte Carlo runs spanning nine prior-SD combinations, so the production clamp
# may simply have differed from the one in the chunk above. Report the numbers.
#
# gamma is byte-identical across the whole base lineage (biocal = phase 51 = 54
# = 55), verified below, so the choice of reference base cannot explain a
# discrepancy.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC12_gamma_prior_check.R"
# =============================================================================

suppressPackageStartupMessages({library(mizer); library(dplyr)})

OUT_LARGE <- "Output_large_files/wmin_test"
KC_ROOT <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
ANA <- file.path(KC_ROOT, "analysis")
dir.create(ANA, recursive = TRUE, showWarnings = FALSE)
LOWER <- 0.8; UPPER <- 100          # the documented clamp

# --- the base the draws were generated from ----------------------------------
BASES <- c(draw_base = "params_sel_adj_wmin_corrected_biocal.rds",
           phase51   = "params_whale_lognormal_kernel_bk05_mk05.rds",
           phase54   = "params_ref_sw2000_balror_mnkfish05.rds",
           phase55   = "params_ref_sw2000_balror_mnkfish05_whres05.rds")
BASES <- BASES[file.exists(BASES)]
gof <- function(f) {
  p <- suppressWarnings(validParams(readRDS(f)))
  setNames(species_params(p)$gamma, species_params(p)$species)
}
G <- lapply(BASES, gof)
cat("=== KC12: realised gamma multipliers vs the documented prior clamp ===\n")
cat("gamma identical across the base lineage:",
    all(vapply(G[-1], function(g) identical(g, G[[1]]), logical(1))), "\n")
g0 <- G[["draw_base"]]

# --- the draws ---------------------------------------------------------------
DR <- readRDS(file.path(OUT_LARGE, "43_member_draws.rds"))
mem <- DR$members_distinct
cat("draw-distinct members:", length(mem), "\n")
M <- vapply(mem, function(m) as.numeric(DR$draws[[as.character(m)]]$gamma),
            numeric(length(g0)))
rownames(M) <- names(g0); colnames(M) <- mem
stopifnot(identical(rownames(M), DR$species))
R <- M / as.numeric(g0)             # realised multiplier

cat(sprintf("\nrealised multiplier over %d members x %d species = %d values\n",
            ncol(R), nrow(R), length(R)))
cat(sprintf("  range %.4g to %.4g | documented clamp [%.1f, %.0f]\n",
            min(R), max(R), LOWER, UPPER))
n_lo <- sum(R < LOWER * (1 - 1e-9)); n_hi <- sum(R > UPPER * (1 + 1e-9))
cat(sprintf("  below the floor: %d (%.2f%%) | above the ceiling: %d (%.2f%%)\n",
            n_lo, 100 * n_lo / length(R), n_hi, 100 * n_hi / length(R)))
cat(sprintf("  members with at least one out-of-window species: %d of %d (%.1f%%)\n",
            sum(apply(R, 2, function(v) any(v < LOWER * (1 - 1e-9) |
                                            v > UPPER * (1 + 1e-9)))),
            ncol(R),
            100 * mean(apply(R, 2, function(v) any(v < LOWER * (1 - 1e-9) |
                                                   v > UPPER * (1 + 1e-9))))))

BY <- data.frame(
  species = rownames(R),
  min = apply(R, 1, min), median = apply(R, 1, median), max = apply(R, 1, max),
  n_below = apply(R, 1, function(v) sum(v < LOWER * (1 - 1e-9))),
  n_above = apply(R, 1, function(v) sum(v > UPPER * (1 + 1e-9))),
  n = ncol(R), row.names = NULL)
cat("\n=== by species ===\n")
print(as.data.frame(BY %>% mutate(across(c(min, median, max), ~signif(.x, 4)))),
      row.names = FALSE)

# Exactly-at-the-boundary counts distinguish "the clamp ran" from "it did not".
# A clamp that fired leaves a spike of values sitting exactly on the bound.
at_lo <- sum(abs(R - LOWER) < 1e-9); at_hi <- sum(abs(R - UPPER) < 1e-9)
cat(sprintf("\nvalues sitting EXACTLY on a bound: %d at %.1f, %d at %.0f\n",
            at_lo, LOWER, at_hi, UPPER))
cat("  A clamp that fired leaves a spike exactly on the bound. No spike plus\n",
    " out-of-window values means the clamp in 09:358-369 was not the one that\n",
    " produced these draws.\n")

write.csv(BY, file.path(ANA, "KC12_gamma_multiplier_by_species.csv"),
          row.names = FALSE)
LONG <- data.frame(sim_index = rep(as.integer(colnames(R)), each = nrow(R)),
                   species = rep(rownames(R), ncol(R)),
                   multiplier = as.numeric(R))
LONG$outside <- LONG$multiplier < LOWER * (1 - 1e-9) |
                LONG$multiplier > UPPER * (1 + 1e-9)
write.csv(LONG, file.path(ANA, "KC12_gamma_multipliers_long.csv"),
          row.names = FALSE)
cat("\nwrote KC12_gamma_multiplier_by_species.csv, KC12_gamma_multipliers_long.csv\n")
cat("KC12 complete.\n")