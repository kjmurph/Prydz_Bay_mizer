# =============================================================================
# Phase 73 -- interaction matrix edits: minke prey breadth, small divers access
#
# Applied to params_ref_p62_intres_balkrill075.rds, i.e. BEFORE the growth
# recalibration, so the ladder absorbs them.
#
# THE MATRIX IS SYMMETRIC BY CONSTRUCTION. Every predator-prey pair carries the
# same value in both halves; the size kernel decides which direction actually
# feeds. Phase 54 wrote its changes symmetrically and verified 0 asymmetric
# cells. EVERY EDIT HERE IS WRITTEN TO BOTH HALVES and the count is asserted.
#
# ------------------------------------------------------------------ 1. MINKE
# Intent: reduce direct competition with baleen whales for antarctic krill by
# broadening minke's access to other prey.
#   mesopelagic fishes        0.5     -> 0.5     (UNCHANGED, phase-54 value kept)
#   shelf and coastal fishes  0.5     -> 0.25
#   bathypelagic fishes       0.0125  -> 0.1
#   salps                     0.05    -> 0.2
#
# ---------------------------------------------------------- 2. SMALL DIVERS
# Intent: relieve chronic food limitation. Small divers is the max-erepro group
# in every reference object (p59 0.720, p62 0.772, p63 0.802, p66 0.908) and its
# interaction with real prey summed to just 0.564 across 17 groups -- a
# dive-depth weighting that leaves it unable to feed itself.
#   all five LTL groups       -> 0.2   (mesozooplankton, other krill,
#                                       other macrozooplankton, antarctic krill,
#                                       salps)
#   mesopelagic fishes        -> 0.2
#   shelf and coastal fishes  -> 0.2
#   bathypelagic fishes       -> 0.1
#   toothfishes               -> 0.1
#   squids                    unchanged at 0.025
#
# --------------------------------------------------- 3. SYMMETRY REPAIR (BUG)
# Phase 62 set interaction["baleen whales", "antarctic krill"] <- 0.75 but NOT
# its mirror, leaving the matrix with 2 asymmetric cells for the first time --
# p59 has 0, p62/p63/p66 have 2. The mirror is set to 0.75 here. The practical
# effect was almost certainly nil (the mirror governs krill preying on baleen
# whales, which the size kernel forbids), but the invariant is repaired.
#
# USAGE  Rscript R/wmin_test/73_interaction_matrix_edits.R
# ENV    P73_IN, P73_OUT
# =============================================================================

suppressPackageStartupMessages({library(mizer); library(therMizer)})

IN  <- Sys.getenv("P73_IN", "params_ref_p62_intres_balkrill075.rds")
OUT <- Sys.getenv("P73_OUT", "params_ref_p73_intmatrix.rds")
if (file.exists(OUT) && !nzchar(Sys.getenv("P73_FORCE")))
  stop("refusing to overwrite: ", OUT, " (set P73_FORCE=1)", call. = FALSE)
if (!file.exists(IN)) stop("missing ", IN, call. = FALSE)

P0 <- suppressWarnings(validParams(readRDS(IN)))
SPN <- P0@species_params$species
EE0 <- P0@ext_encounter
I0 <- P0@interaction

LTL <- c("mesozooplankton", "other krill", "other macrozooplankton",
         "antarctic krill", "salps")

# predator, prey, new value -- each written to BOTH halves
EDITS <- rbind(
  data.frame(a = "minke whales", b = "shelf and coastal fishes", v = 0.25),
  data.frame(a = "minke whales", b = "bathypelagic fishes",      v = 0.10),
  data.frame(a = "minke whales", b = "salps",                    v = 0.20),
  data.frame(a = "small divers", b = LTL,                        v = 0.20),
  data.frame(a = "small divers", b = "mesopelagic fishes",       v = 0.20),
  data.frame(a = "small divers", b = "shelf and coastal fishes", v = 0.20),
  data.frame(a = "small divers", b = "bathypelagic fishes",      v = 0.10),
  data.frame(a = "small divers", b = "toothfishes",              v = 0.10),
  # symmetry repair for the phase-62 one-sided edit
  data.frame(a = "baleen whales", b = "antarctic krill",         v = 0.75))
stopifnot(all(c(EDITS$a, EDITS$b) %in% SPN))

cat("=== Phase 73: interaction matrix edits ===\n")
cat("in :", IN, "\nout:", OUT, "\n\n")
cat("asymmetric cells BEFORE:", sum(I0 != t(I0)), "\n\n")

I <- I0
cat(sprintf("%-14s %-26s %8s -> %8s\n", "predator", "prey", "old", "new"))
for (k in seq_len(nrow(EDITS))) {
  a <- EDITS$a[k]; b <- EDITS$b[k]; v <- EDITS$v[k]
  cat(sprintf("%-14s %-26s %8.4f -> %8.4f\n", a, b, I[a, b], v))
  I[a, b] <- v
  I[b, a] <- v                      # the mirror -- symmetric by construction
}
interaction_matrix(P0) <- I
P0@ext_encounter <- EE0

# --- assertions ---------------------------------------------------------------
I1 <- P0@interaction
n_asym <- sum(I1 != t(I1))
changed <- which(I1 != I0, arr.ind = TRUE)
expected <- unique(rbind(
  cbind(match(EDITS$a, SPN), match(EDITS$b, SPN)),
  cbind(match(EDITS$b, SPN), match(EDITS$a, SPN))))
unexpected <- nrow(changed) -
  sum(paste(changed[,1], changed[,2]) %in% paste(expected[,1], expected[,2]))

cat("\n--- checks ---\n")
cat("asymmetric cells AFTER : ", n_asym, "\n", sep = "")
cat("cells changed          : ", nrow(changed),
    " (both halves of ", nrow(EDITS), " pairs, minus any already correct)\n", sep = "")
cat("changed OUTSIDE the requested pairs: ", unexpected, "\n", sep = "")
cat("ext_encounter identical: ",
    identical(as.vector(P0@ext_encounter), as.vector(EE0)), "\n", sep = "")
cat("species_params untouched: ",
    identical(P0@species_params, suppressWarnings(
      validParams(readRDS(IN)))@species_params), "\n", sep = "")
stopifnot(n_asym == 0, unexpected == 0)

# --- what it does to food access ---------------------------------------------
real_prey <- function(M, sp) sum(M[sp, ])
cat("\n--- interaction row sums (all prey) ---\n")
for (sp in c("minke whales", "small divers", "baleen whales"))
  cat(sprintf("  %-14s %.4f -> %.4f\n", sp, real_prey(I0, sp), real_prey(I1, sp)))

# --- 4. MINKE PPMR -----------------------------------------------------------
# beta is the preferred predator/prey MASS RATIO, so LOWERING it moves minke's
# preferred prey to LARGER sizes -- the same direction as the interaction edits
# above, away from krill and toward fish. Phase 54 verified the current value as
# exactly 1 / 1.98333333333333e-07 from the source table, implying 1.19 g prey.
#
# species_params<- ZEROES ext_encounter (phase 57); restore by slot.
BETA_FAC <- as.numeric(Sys.getenv("P73_MINKE_BETA_FACTOR", "0.9"))
mi <- match("minke whales", SPN)
sp0 <- P0@species_params
beta_old <- sp0$beta[mi]
ee <- P0@ext_encounter
sp <- species_params(P0); sp$beta[mi] <- beta_old * BETA_FAC
species_params(P0) <- sp
P0@ext_encounter <- ee

sp1 <- P0@species_params
cat(sprintf("\n--- 4. minke PPMR x %.2f ---\n", BETA_FAC))
cat(sprintf("  beta            %.4f -> %.4f\n", beta_old, sp1$beta[mi]))
cat(sprintf("  preferred prey at w_inf (%.3g g): %.3f g -> %.3f g\n",
            sp1$w_inf[mi], sp1$w_inf[mi] / beta_old, sp1$w_inf[mi] / sp1$beta[mi]))
cat("  kernel type: ", as.character(sp1$pred_kernel_type[mi]),
    " | sigma ", sp1$sigma[mi], "\n", sep = "")

moved <- setdiff(names(sp1)[!vapply(names(sp1), function(cn)
  isTRUE(all.equal(sp0[[cn]], sp1[[cn]])), logical(1))], character(0))
cat("  species_params columns changed: ",
    if (length(moved)) paste(moved, collapse = ", ") else "NONE", "\n", sep = "")
cat("  ext_encounter identical: ",
    identical(as.vector(P0@ext_encounter), as.vector(EE0)), "\n", sep = "")
cat("  interaction matrix still symmetric: ",
    sum(P0@interaction != t(P0@interaction)) == 0, "\n", sep = "")
stopifnot(identical(moved, "beta"),
          identical(as.vector(P0@ext_encounter), as.vector(EE0)),
          sum(P0@interaction != t(P0@interaction)) == 0)

saveRDS(P0, OUT)
cat("\nWROTE ", OUT, "\n", sep = "")
