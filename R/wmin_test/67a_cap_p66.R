# =============================================================================
# Phase 67a -- cap the reproduction level of the phase-66 reference at 0.9
#
# The phase-58 operation, applied to params_ref_p66_growth_intres_balkrill075.rds.
# setBevertonHolt() preserves the steady state exactly (it selects a different
# curve through the same (E_R, R_dd) point), so BIOMASS AND GROWTH MUST NOT MOVE.
# The script asserts that rather than assuming it. What changes is the split
# between erepro and R_max, and the reproduction level itself.
#
# WHY. p66 came out of the growth recalibration at max erepro 0.9080 and max
# reproduction level 0.9489, with 9 groups above 0.9. The member protocol needs
# erepro < 1 and the phase-65 pilot already peaked at 1.09 building on p59's
# 0.7198 -- starting members from 0.9080 would eat the margin. Capping scales
# erepro by roughly (1 - 0.9489)/(1 - 0.9) = 0.51, to about 0.46, which is BETTER
# headroom than p59 had.
#
# USAGE  Rscript R/wmin_test/67a_cap_p66.R
# ENV    P67A_IN, P67A_OUT, P67A_CAP (0.9)
# =============================================================================

suppressPackageStartupMessages({library(mizer); library(therMizer)})

IN  <- Sys.getenv("P67A_IN", "params_ref_p66_recal_growth_intres_balkrill075.rds")
OUT <- Sys.getenv("P67A_OUT", "params_ref_p66_growth_cap09.rds")
CAP <- as.numeric(Sys.getenv("P67A_CAP", "0.9"))
if (file.exists(OUT) && !nzchar(Sys.getenv("P67A_FORCE")))
  stop("refusing to overwrite: ", OUT, " (set P67A_FORCE=1)", call. = FALSE)
if (!file.exists(IN)) stop("missing ", IN, call. = FALSE)

P0 <- suppressWarnings(validParams(readRDS(IN)))
SPN <- P0@species_params$species
EE0 <- P0@ext_encounter
obs <- P0@species_params$biomass_observed

bio <- function(p) as.numeric(getBiomass(p, use_cutoff = TRUE) / obs)
age <- function(p) as.numeric(mizer::age_mat(p))
rl  <- function(p) as.numeric(getReproductionLevel(p))

cat("=== Phase 67a: cap the p66 reproduction level at", CAP, "===\n")
cat("in :", IN, "\nout:", OUT, "\n\n")
cat(sprintf("before: max bio dev %.5f | max erepro %.4f | max rl %.4f (%d > %.2f)\n",
            max(abs(bio(P0) - 1)), max(P0@species_params$erepro),
            max(rl(P0)), sum(rl(P0) > CAP + 1e-9), CAP))

P1 <- suppressWarnings(setBevertonHolt(P0, reproduction_level = pmin(rl(P0), CAP)))
P1@ext_encounter <- EE0        # belt and braces; setBevertonHolt should not touch it

cat(sprintf("after : max bio dev %.5f | max erepro %.4f | max rl %.4f (%d > %.2f)\n\n",
            max(abs(bio(P1) - 1)), max(P1@species_params$erepro),
            max(rl(P1)), sum(rl(P1) > CAP + 1e-9), CAP))

# --- the cap must be free: state, biomass and growth all unchanged ------------
d_bio <- max(abs(bio(P1) / bio(P0) - 1))
d_age <- max(abs(age(P1) / age(P0) - 1))
d_n   <- max(abs(P1@initial_n / P0@initial_n - 1), na.rm = TRUE)
cat("--- the cap must preserve the steady state ---\n")
cat(sprintf("  max |biomass ratio change| : %.3g\n", d_bio))
cat(sprintf("  max |age_mat ratio change| : %.3g\n", d_age))
cat(sprintf("  max |initial_n change|     : %.3g\n", d_n))
cat("  ext_encounter identical    : ",
    identical(as.vector(P1@ext_encounter), as.vector(EE0)), "\n", sep = "")
cat("  therMizer rate funcs kept  : ",
    identical(P0@rates_funcs, P1@rates_funcs), "\n", sep = "")
stopifnot(d_bio < 1e-8, d_age < 1e-8, d_n < 1e-8)

rep_df <- data.frame(species = SPN,
                     erepro_before = signif(P0@species_params$erepro, 4),
                     erepro_after  = signif(P1@species_params$erepro, 4),
                     erepro_ratio  = signif(P1@species_params$erepro /
                                            P0@species_params$erepro, 3),
                     rl_before = round(rl(P0), 4), rl_after = round(rl(P1), 4),
                     R_max_ratio = signif(P1@species_params$R_max /
                                          P0@species_params$R_max, 4),
                     stringsAsFactors = FALSE)
print(rep_df, row.names = FALSE)
write.csv(rep_df, file.path("Output_large_files", "wmin_test",
                            "67a_cap_per_species.csv"), row.names = FALSE)

saveRDS(P1, OUT)
cat("\nWROTE ", OUT, "\n", sep = "")