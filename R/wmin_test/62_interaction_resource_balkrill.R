# =============================================================================
# Phase 62 -- per-group interaction_resource, and baleen x antarctic krill 0.75
#
# ISOLATED. Reads params_ref_p59_cap09_tol001.rds and writes only new files.
#
# ------------------------------------------------------------- what changes
# 1. interaction_resource, set per group (all 19 were 1.0 in p59):
#      mesozooplankton, other krill, other macrozooplankton, antarctic krill,
#      salps                                              1.00  (unchanged)
#      mesopelagic / bathypelagic / shelf and coastal fishes, toothfishes  0.75
#      flying birds, small divers, squids, leopard seals,
#      medium divers, large divers, minke whales, baleen whales            0.25
#      orca, sperm whales                                                  0.00
#
# 2. interaction["baleen whales", "antarctic krill"]  0.500 -> 0.750
#
# Both values are operator-supplied. Nothing here is inferred.
#
# ------------------------------------------------- NOTE: p59 has no whres branch
# Phases 55/56 made the analogous whale edit (baleen + minke resource at 0.5 and
# 0.0) but that lives only in params_ref_sw2000_balror_mnkfish05_whres*.rds. The
# p57 subsidy branch was built from p54, so p59 carries all 19 groups at 1.0 and
# this is not a repeat of that work.
#
# ------------------------------------------------------------------- the traps
# A. species_params<- CALLS setParams(), WHICH DESTROYS ext_encounter. The
#    phase-57 out-of-domain subsidy lives there (900 non-zero entries in p59).
#    It is snapshotted before the edit, restored with ext_encounter<- after, and
#    asserted identical. Every other species_params column and every rate slot
#    is diffed too, so an unexpected recalculation is reported rather than
#    absorbed silently.
#
# B. steady() SIGNALS NON-CONVERGENCE WITH message(), NOT warning(). A guard
#    built on tryCatch(warning = ) is dead. steady_guarded() below is
#    transcribed from 59_reladder_tighter_tolerance.R:69-80, which is the
#    working version.
#
# C. THE EDIT MOVES THE MODEL OFF ITS CALIBRATION. Measured on the p59 state
#    before any equilibration, the instantaneous change in biomass-weighted
#    encounter is: baleen +13.8% (the krill theta rise outweighs the resource
#    cut), minke -20.1%, small divers -9.4%, mesopelagic -9.2%, bathypelagic
#    -8.1%, shelf fishes -6.3%, birds -2.2%, leopard seals -0.9%. orca and
#    sperm whales change by EXACTLY 0 -- their resource share is already zero,
#    so setting 0.0 is a formal no-op. squids, toothfishes and medium/large
#    divers move <= 0.15% for the same reason.
#    Antarctic krill predation mortality rises 6.2%, and other krill /
#    macrozooplankton / salps rise 1.7-3.9%: cutting interaction_resource lowers
#    the feeding level, and predation rate scales with (1 - f), so the predators
#    take more from the species spectrum. That is the phase-56 redirection.
#
# ------------------------------------------------------------- what "pass" means
# Stated before the run, following phase 59: steady() must (a) converge AND
# (b) leave every erepro < 1. Convergence alone is not a pass -- an inadmissible
# erepro is what sank phase 57. On a pass the object is written to OUT. On a
# failure it is written to the _NOTCONVERGED name instead and OUT is not
# created, so a failed equilibration can never be mistaken for a reference model.
#
# The pre-steady (edit-only) object is always written, so the parameter edit can
# be inspected separately from the equilibration.
#
# USAGE  Rscript R/wmin_test/62_interaction_resource_balkrill.R
# ENV    P62_IN, P62_OUT, P62_TOL (0.001), P62_PRESERVE (reproduction_level),
#        P62_TMAX (1000), P62_BAL_KRILL (0.75)
# =============================================================================

suppressPackageStartupMessages({library(mizer); library(therMizer)})

IN   <- Sys.getenv("P62_IN", "params_ref_p59_cap09_tol001.rds")
OUT  <- Sys.getenv("P62_OUT", "params_ref_p62_intres_balkrill075.rds")
TOL  <- as.numeric(Sys.getenv("P62_TOL", "0.001"))
PRES <- Sys.getenv("P62_PRESERVE", "reproduction_level")
STEADY_TMAX <- as.numeric(Sys.getenv("P62_TMAX", "1000"))
BAL_KRILL   <- as.numeric(Sys.getenv("P62_BAL_KRILL", "0.75"))
CAP <- 0.9

out_dir <- file.path("Output_large_files", "wmin_test")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
PRE_OUT  <- file.path(out_dir, "62_preSteady_intres_balkrill075.rds")
FAIL_OUT <- file.path(out_dir, "62_NOTCONVERGED_intres_balkrill075.rds")

# --- the operator-supplied values, matched BY NAME, never by position ---------
NEW_IR <- c(
  "mesozooplankton"          = 1.00,
  "other krill"              = 1.00,
  "other macrozooplankton"   = 1.00,
  "antarctic krill"          = 1.00,
  "salps"                    = 1.00,
  "mesopelagic fishes"       = 0.75,
  "bathypelagic fishes"      = 0.75,
  "shelf and coastal fishes" = 0.75,
  "flying birds"             = 0.25,
  "small divers"             = 0.25,
  "squids"                   = 0.25,
  "toothfishes"              = 0.75,
  "leopard seals"            = 0.25,
  "medium divers"            = 0.25,
  "large divers"             = 0.25,
  "minke whales"             = 0.25,
  "orca"                     = 0.00,
  "sperm whales"             = 0.00,
  "baleen whales"            = 0.25)

t0 <- proc.time()
cat("=== Phase 62: interaction_resource + baleen-krill interaction ===\n")
cat("input:", IN, "| output:", OUT, "\n")
cat("tol:", TOL, "| preserve:", PRES, "| t_max:", STEADY_TMAX, "\n\n")
if (!file.exists(IN)) stop("missing ", IN, call. = FALSE)

P0 <- suppressWarnings(validParams(readRDS(IN)))
SPN <- P0@species_params$species
if (!setequal(SPN, names(NEW_IR)))
  stop("species names do not match the supplied values", call. = FALSE)

# --- snapshot everything that setParams() could touch --------------------------
EE0        <- P0@ext_encounter
EE0_cmt    <- comment(P0@ext_encounter)
SP0        <- P0@species_params
SLOTS <- c("search_vol", "intake_max", "metab", "mu_b", "ft_pred_kernel_e",
           "ft_pred_kernel_p", "rr_pp", "cc_pp", "psi", "maturity", "w_min_idx",
           "initial_n", "initial_n_pp", "ext_mort")
snap <- function(p) {
  s <- lapply(SLOTS, function(nm)
    if (.hasSlot(p, nm)) slot(p, nm) else NULL)
  names(s) <- SLOTS
  s
}
S0 <- snap(P0)
RF0 <- P0@rates_funcs

cat("p59 ext_encounter: ", sum(EE0 != 0), " non-zero entries",
    if (is.null(EE0_cmt)) " (no comment -- unprotected)" else " (comment set)",
    "\n", sep = "")

# --- edit 1: interaction_resource ----------------------------------------------
P <- P0
sp <- species_params(P)
sp$interaction_resource <- unname(NEW_IR[match(SPN, names(NEW_IR))])
species_params(P) <- sp

# --- restore ext_encounter and prove it is back --------------------------------
wiped <- !isTRUE(all.equal(as.vector(P@ext_encounter), as.vector(EE0),
                           tolerance = 0))
cat("ext_encounter after species_params<- : ",
    if (wiped) "DESTROYED, restoring" else "survived", "\n", sep = "")
ext_encounter(P) <- EE0
if (!is.null(EE0_cmt)) comment(P@ext_encounter) <- EE0_cmt
stopifnot(identical(as.vector(P@ext_encounter), as.vector(EE0)))

# --- edit 2: the interaction matrix --------------------------------------------
I <- interaction_matrix(P)
old_bk <- I["baleen whales", "antarctic krill"]
I["baleen whales", "antarctic krill"] <- BAL_KRILL
interaction_matrix(P) <- I
cat(sprintf("interaction[baleen whales, antarctic krill]: %.3f -> %.3f\n",
            old_bk, interaction_matrix(P)["baleen whales", "antarctic krill"]))

# --- assert NOTHING ELSE moved --------------------------------------------------
sp1 <- P@species_params
other_cols <- setdiff(intersect(names(SP0), names(sp1)), "interaction_resource")
moved <- other_cols[!vapply(other_cols, function(cn)
  isTRUE(all.equal(SP0[[cn]], sp1[[cn]])), logical(1))]
S1 <- snap(P)
slots_moved <- SLOTS[!vapply(SLOTS, function(nm)
  isTRUE(all.equal(S0[[nm]], S1[[nm]])), logical(1))]
cat("\n--- collateral check ---\n")
cat("species_params columns changed besides interaction_resource: ",
    if (length(moved)) paste(moved, collapse = ", ") else "NONE", "\n", sep = "")
cat("rate slots changed: ",
    if (length(slots_moved)) paste(slots_moved, collapse = ", ") else "NONE",
    "\n", sep = "")
cat("therMizer rate functions intact: ", identical(RF0, P@rates_funcs), "\n",
    sep = "")
cat("interaction matrix cells changed: ", sum(P0@interaction != P@interaction),
    "\n", sep = "")

saveRDS(P, PRE_OUT)
cat("\npre-steady object written: ", PRE_OUT, "\n", sep = "")

# --- diagnostics, phase-59 definitions ------------------------------------------
bio_ratio <- function(p)
  as.numeric(getBiomass(p, use_cutoff = TRUE) / p@species_params$biomass_observed)
max_dev <- function(p) max(abs(bio_ratio(p) - 1))
rl_of   <- function(p) as.numeric(getReproductionLevel(p))

cat(sprintf("\np59 baseline : max biomass dev %.5f | max erepro %.4f | max rl %.4f\n",
            max_dev(P0), max(P0@species_params$erepro), max(rl_of(P0))))
cat(sprintf("after edit   : max biomass dev %.5f | max erepro %.4f | max rl %.4f\n",
            max_dev(P), max(P@species_params$erepro), max(rl_of(P))))

# --- steady(), guarded on message() [59_reladder_tighter_tolerance.R:69-80] -----
steady_guarded <- function(p, tol) {
  nc <- FALSE
  out <- withCallingHandlers(
    try(steady(p, tol = tol, t_max = STEADY_TMAX, preserve = PRES,
               progress_bar = FALSE), silent = TRUE),
    message = function(m) {
      if (grepl("did not converge", conditionMessage(m), ignore.case = TRUE))
        nc <<- TRUE
      invokeRestart("muffleMessage")
    }, warning = function(w) invokeRestart("muffleWarning"))
  list(params = out, converged = !nc, errored = inherits(out, "try-error"))
}

cat("\nrunning steady(tol = ", TOL, ") ...\n", sep = "")
flush.console()
st <- steady_guarded(P, TOL)
if (st$errored) stop("steady() errored: ", as.character(st$params), call. = FALSE)
PS <- st$params

er <- PS@species_params$erepro
rl <- rl_of(PS)
admissible <- max(er) < 1
pass <- st$converged && admissible

cat(sprintf("\nafter steady : max biomass dev %.5f | max erepro %.4f (%d >= 1) | max rl %.4f (%d > %.2f)\n",
            max_dev(PS), max(er), sum(er >= 1), max(rl), sum(rl > CAP + 1e-9), CAP))
cat("converged: ", st$converged, " | admissible: ", admissible,
    " | PASS: ", pass, "\n", sep = "")

# ext_encounter must have survived steady() too
cat("ext_encounter after steady(): ",
    if (isTRUE(all.equal(as.vector(PS@ext_encounter), as.vector(EE0))))
      "intact" else "CHANGED", "\n", sep = "")

# --- per-species report ----------------------------------------------------------
rep_df <- data.frame(
  species        = SPN,
  interaction_resource = PS@species_params$interaction_resource,
  bio_ratio_p59  = round(bio_ratio(P0), 4),
  bio_ratio_new  = round(bio_ratio(PS), 4),
  biomass_pct    = round(100 * (as.numeric(getBiomass(PS, use_cutoff = TRUE)) /
                                as.numeric(getBiomass(P0, use_cutoff = TRUE)) - 1), 2),
  erepro         = signif(er, 4),
  repro_level    = round(rl, 4),
  stringsAsFactors = FALSE)
print(rep_df, row.names = FALSE)
write.csv(rep_df, file.path(out_dir, "62_per_species.csv"), row.names = FALSE)

if (pass) {
  saveRDS(PS, OUT)
  cat("\nWROTE ", OUT, "\n", sep = "")
} else {
  saveRDS(PS, FAIL_OUT)
  cat("\nDID NOT PASS -- ", OUT, " was NOT written.\n", sep = "")
  cat("The steadied object is at ", FAIL_OUT, " for diagnosis only.\n", sep = "")
  cat("Next step if this is non-convergence: walk the phase-59 tolerance ladder\n")
  cat("  (0.1, 0.05, 0.01, 0.005, 0.002, 0.001) instead of a single tol.\n")
}
cat(sprintf("\nelapsed %.1f min\n", (proc.time() - t0)[["elapsed"]] / 60))