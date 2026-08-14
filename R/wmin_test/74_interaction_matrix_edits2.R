# =============================================================================
# Phase 74 -- second round of pre-calibration edits, on top of phase 73
#
# Input is params_ref_p73_intmatrix.rds -- the PRE-calibration object, so the
# edits stack on phase 73's rather than on its recalibrated output. Both rounds
# are then absorbed by one ladder.
#
# 1. SMALL DIVERS interaction DOUBLED for six prey groups. Phase 73 raised these
#    to 0.2 and it changed nothing: small divers still equilibrated 16% above its
#    observed biomass (1.1513 -> 1.1623) and its erepro rose rather than fell
#    (0.908 -> 0.953). Doubling again tests whether the overshoot responds to
#    food access at all.
#      mesozooplankton, other krill, other macrozooplankton, salps,
#      mesopelagic fishes   0.2   -> 0.4
#      squids               0.025 -> 0.05
#    antarctic krill, shelf and coastal fishes, bathypelagic fishes and
#    toothfishes are NOT in the list and keep their phase-73 values.
#
# 2. MINKE PPMR a FURTHER 10%. Phase 73 took beta 5,042,016.8 -> 4,537,815.1
#    (x0.9). This applies x0.9 again, giving x0.81 of the original.
#
# Values are computed FROM THE INPUT (2 x current, 0.9 x current) rather than
# hardcoded, so "double" and "a further 10%" mean exactly that whatever the
# starting point.
#
# THE MATRIX IS SYMMETRIC BY CONSTRUCTION -- every edit is written to both halves
# and the asymmetric-cell count is asserted at 0.
#
# USAGE  Rscript R/wmin_test/74_interaction_matrix_edits2.R
# ENV    P74_IN, P74_OUT, P74_MINKE_BETA_FACTOR (0.9)
# =============================================================================

suppressPackageStartupMessages({library(mizer); library(therMizer)})

IN  <- Sys.getenv("P74_IN", "params_ref_p73_intmatrix.rds")
OUT <- Sys.getenv("P74_OUT", "params_ref_p74_intmatrix.rds")
BETA_FAC <- as.numeric(Sys.getenv("P74_MINKE_BETA_FACTOR", "0.9"))
if (file.exists(OUT) && !nzchar(Sys.getenv("P74_FORCE")))
  stop("refusing to overwrite: ", OUT, " (set P74_FORCE=1)", call. = FALSE)
if (!file.exists(IN)) stop("missing ", IN, call. = FALSE)

P0 <- suppressWarnings(validParams(readRDS(IN)))
SPN <- P0@species_params$species
EE0 <- P0@ext_encounter
I0 <- P0@interaction
SP0 <- P0@species_params

DOUBLE_PREY <- c("mesozooplankton", "other krill", "other macrozooplankton",
                 "salps", "mesopelagic fishes", "squids")
stopifnot(all(DOUBLE_PREY %in% SPN))

cat("=== Phase 74: second round of edits ===\n")
cat("in :", IN, "\nout:", OUT, "\n\n")
cat("asymmetric cells BEFORE:", sum(I0 != t(I0)), "\n\n")

I <- I0
cat("--- 1. small divers interaction DOUBLED ---\n")
cat(sprintf("%-26s %8s -> %8s\n", "prey", "old", "new"))
for (b in DOUBLE_PREY) {
  v <- 2 * I["small divers", b]
  cat(sprintf("%-26s %8.4f -> %8.4f\n", b, I["small divers", b], v))
  I["small divers", b] <- v
  I[b, "small divers"] <- v          # the mirror
}
interaction_matrix(P0) <- I
P0@ext_encounter <- EE0

I1 <- P0@interaction
changed <- which(I1 != I0, arr.ind = TRUE)
expected <- unique(rbind(
  cbind(match("small divers", SPN), match(DOUBLE_PREY, SPN)),
  cbind(match(DOUBLE_PREY, SPN), match("small divers", SPN))))
unexpected <- nrow(changed) -
  sum(paste(changed[,1], changed[,2]) %in% paste(expected[,1], expected[,2]))

# --- 2. minke PPMR, a further 10% --------------------------------------------
mi <- match("minke whales", SPN)
beta_old <- SP0$beta[mi]
ee <- P0@ext_encounter
sp <- species_params(P0); sp$beta[mi] <- beta_old * BETA_FAC
species_params(P0) <- sp
P0@ext_encounter <- ee
SP1 <- P0@species_params

cat(sprintf("\n--- 2. minke PPMR x %.2f (a further 10%%) ---\n", BETA_FAC))
cat(sprintf("  beta            %.4f -> %.4f\n", beta_old, SP1$beta[mi]))
cat(sprintf("  cumulative vs the phase-54 value 5042016.8067: x%.4f\n",
            SP1$beta[mi] / 5042016.8067))
cat(sprintf("  preferred prey at w_inf (%.3g g): %.3f g -> %.3f g\n",
            SP1$w_inf[mi], SP1$w_inf[mi] / beta_old, SP1$w_inf[mi] / SP1$beta[mi]))

moved <- names(SP1)[!vapply(names(SP1), function(cn)
  isTRUE(all.equal(SP0[[cn]], SP1[[cn]])), logical(1))]
cat("\n--- checks ---\n")
cat("asymmetric cells AFTER : ", sum(I1 != t(I1)), "\n", sep = "")
cat("matrix cells changed   : ", nrow(changed),
    " | outside the requested pairs: ", unexpected, "\n", sep = "")
cat("species_params changed : ",
    if (length(moved)) paste(moved, collapse = ", ") else "NONE", "\n", sep = "")
cat("ext_encounter identical: ",
    identical(as.vector(P0@ext_encounter), as.vector(EE0)), "\n", sep = "")
stopifnot(sum(I1 != t(I1)) == 0, unexpected == 0, identical(moved, "beta"),
          identical(as.vector(P0@ext_encounter), as.vector(EE0)))

cat("\n--- interaction row sums (all prey) ---\n")
for (sp_ in c("small divers", "minke whales"))
  cat(sprintf("  %-14s %.4f -> %.4f\n", sp_, sum(I0[sp_, ]), sum(I1[sp_, ])))

saveRDS(P0, OUT)
cat("\nWROTE ", OUT, "\n", sep = "")