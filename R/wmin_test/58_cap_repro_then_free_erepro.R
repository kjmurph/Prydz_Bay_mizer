# =============================================================================
# Phase 58 -- cap the pinned reproduction levels, then re-ladder with erepro FREE
#
# THE PROBLEM THIS SOLVES. Phase 57's free-erepro arms failed on physiological
# admissibility: preserve = "reproduction_level" needed erepro 1.088 for orca,
# and preserve = "R_max" needed 2.14 (minke) and 17.05 (small divers). erepro > 1
# means converting more than 100% of reproductive energy into eggs.
#
# WHY IT FAILED, AND WHY A CAP FIXES IT. At a fixed steady state erepro and
# reproduction_level are ONE lever, not two: RDD is fixed, RDI is proportional to
# erepro, and reproduction_level = 1 - RDD/RDI. So a group inheriting a level of
# 0.999 REQUIRES a huge erepro to sustain it, and orca inherited 0.9989. Measured
# on this object, the number of groups needing erepro > 1 at a uniform target is
#     level 0.05-0.50 -> 0 of 19      0.75 -> 1      0.90 -> 2      0.95 -> 3
# so it is the HIGH levels that break erepro, not the low ones. Capping the three
# pinned groups before freeing erepro removes the violation at its source.
#
# WHY A CAP RATHER THAN A UNIFORM TARGET (Kieran's call, 2026-08-12). Only three
# groups sit above 0.9 -- toothfishes 0.9933, baleen whales 0.9974, orca 0.9989.
# A cap moves those three and leaves the other sixteen exactly as calibrated,
# where a uniform 0.25 or 0.5 would rewrite every group's reproduction for no
# stated reason. The clamped groups have the headroom for it: erepro sits
# 899x (orca), 392x (baleen) and 148x (toothfishes) above its floor.
#
# THIS IS FREE. setBevertonHolt holds the steady state exactly -- verified here
# at max relative biomass change 0 -- so the cap costs nothing against the
# phase-57 biomass fit of 0.00820. The re-ladder afterwards is the part that can
# move it, and both objects are written so the two can be compared.
#
# ONE HONEST CAVEAT, restated because it does not go away. Lowering a
# reproduction level lowers erepro, and for K-selected mammals that gets
# strange: orca at level 0.9 needs erepro 0.0106, i.e. ~1% of reproductive
# energy becoming viable calves. The tension is real rather than a defect -- at
# the observed orca biomass a population converting ~95% of reproductive energy
# into calves would produce far more recruits than the steady state can absorb,
# so something must remove them. Capping moves that regulation out of R_max and
# into a low erepro; it relocates the assumption, it does not remove it.
#
# USAGE  Rscript R/wmin_test/58_cap_repro_then_free_erepro.R
# ENV    P58_IN, P58_CAP (0.9), P58_PRESERVE (reproduction_level), P58_TOL (0.005)
# =============================================================================

suppressPackageStartupMessages({library(mizer); library(therMizer)})
source(file.path("R", "check_size_params.R"))

IN <- Sys.getenv("P58_IN", "params_ref_p57_subsidy_wcut1_erepro.rds")
CAP <- as.numeric(Sys.getenv("P58_CAP", "0.9"))
PRES <- Sys.getenv("P58_PRESERVE", "reproduction_level")
STEADY_TOL <- as.numeric(Sys.getenv("P58_TOL", "0.005"))
out_dir <- file.path("Output_large_files", "wmin_test")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
tag <- function(x) file.path(out_dir, paste0("58_", x))

STEADY_TMAX <- 1000
TOL_LADDER  <- c(0.1, 0.05, 0.01, STEADY_TOL)
MATCH_ROUNDS <- 6; TARGET_ROUNDS <- 14
OUT_CAP <- "params_ref_p58_cap09.rds"
OUT_LAD <- "params_ref_p58_cap09_reladdered.rds"

t0 <- proc.time()
cat("=== Phase 58: cap reproduction level at", CAP,
    "then re-ladder with erepro free ===\n")
cat("input:", IN, "| preserve:", PRES, "| target tol:", STEADY_TOL, "\n\n")

P <- suppressWarnings(validParams(readRDS(IN)))
stopifnot(identical(P@rates_funcs$Encounter, "therMizerEncounter"),
          identical(P@resource_dynamics, "plankton_forcing"))
SPN <- P@species_params$species
bio_ratio <- function(p)
  as.numeric(getBiomass(p, use_cutoff = TRUE) / p@species_params$biomass_observed)
max_dev <- function(p) max(abs(bio_ratio(p) - 1))
rl_of <- function(p) as.numeric(getReproductionLevel(p))

rl0 <- rl_of(P); er0 <- P@species_params$erepro; bio0 <- bio_ratio(P)
cat("--- input state ---\n")
cat(sprintf("  max biomass deviation %.5f | max erepro %.4f | median repro level %.4f\n",
            max_dev(P), max(er0), median(rl0)))
cat("  groups above the cap:",
    paste(sprintf("%s %.4f", SPN[rl0 > CAP], rl0[rl0 > CAP]), collapse = ", "),
    "\n")

# --- GATE: setting a level to its CURRENT value must be a no-op ---------------
# If it is not, setBevertonHolt is not the identity-preserving operation this
# phase assumes and the cap cannot be trusted to leave the other groups alone.
ident <- suppressWarnings(setBevertonHolt(P, reproduction_level = rl0))
d_rmax <- max(abs(ident@species_params$R_max / P@species_params$R_max - 1))
d_er <- max(abs(ident@species_params$erepro / P@species_params$erepro - 1))
cat(sprintf("\n  GATE identity: re-setting the CURRENT levels moves R_max by %.3g, erepro by %.3g\n",
            d_rmax, d_er))
if (max(d_rmax, d_er) > 1e-8)
  stop("setBevertonHolt is not identity at the current reproduction levels -- ",
       "the cap would silently perturb groups it should not touch.", call. = FALSE)

# --- the cap ------------------------------------------------------------------
target <- pmin(rl0, CAP)
capped <- suppressWarnings(setBevertonHolt(P, reproduction_level = target))
rl1 <- rl_of(capped); er1 <- capped@species_params$erepro

moved <- which(abs(rl1 - rl0) > 1e-9)
cat("\n--- the cap ---\n")
cat("  groups moved:", length(moved), "of", length(SPN), "\n")
print(data.frame(species = SPN[moved],
                 repro_level = sprintf("%.4f -> %.4f", rl0[moved], rl1[moved]),
                 erepro = sprintf("%.4g -> %.4g", er0[moved], er1[moved]),
                 R_max_x = signif(capped@species_params$R_max[moved] /
                                    P@species_params$R_max[moved], 4)),
      row.names = FALSE)
untouched <- setdiff(seq_along(SPN), moved)
cat("  the other", length(untouched), "groups: max |erepro change| =",
    signif(max(abs(er1[untouched] / er0[untouched] - 1)), 3), "\n")
cat(sprintf("  steady state held? max relative biomass change %.3g\n",
            max(abs(bio_ratio(capped) / bio0 - 1))))
cat(sprintf("  max biomass deviation %.5f (was %.5f) | max erepro %.4f\n",
            max_dev(capped), max_dev(P), max(er1)))
if (max(abs(bio_ratio(capped) / bio0 - 1)) > 1e-10)
  stop("the cap moved the steady state -- it should be exact", call. = FALSE)
if (any(er1 >= 1)) stop("the cap alone produced erepro >= 1", call. = FALSE)
saveRDS(capped, OUT_CAP)
cat("  wrote", OUT_CAP, "\n")

# --- the ladder, erepro free --------------------------------------------------
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
match_guarded <- function(p) {
  out <- withCallingHandlers(try(matchBiomasses(p), silent = TRUE),
    warning = function(w) invokeRestart("muffleWarning"))
  list(params = out, errored = inherits(out, "try-error"))
}

cat("\n--- re-ladder with preserve =", PRES, "(erepro free) ---\n")
p <- capped
st <- steady_guarded(p, TOL_LADDER[1])
if (st$errored) stop("steady() errored on the leading call", call. = FALSE)
p <- st$params
best <- NULL; best_d <- Inf; trace <- list()
for (tol in TOL_LADDER) {
  at <- isTRUE(all.equal(tol, STEADY_TOL))
  for (r in seq_len(if (at) TARGET_ROUNDS else MATCH_ROUNDS)) {
    mt <- match_guarded(p); if (mt$errored) stop("matchBiomasses() errored")
    p <- mt$params
    st <- steady_guarded(p, tol); if (st$errored) stop("steady() errored")
    p <- st$params
    d <- max_dev(p); me <- max(p@species_params$erepro)
    rl <- rl_of(p)
    # admissibility is the whole point of this phase, so it is tracked every
    # round rather than only at the end
    if (at && st$converged && me < 1 && d < best_d) { best_d <- d; best <- p }
    trace[[length(trace) + 1]] <- data.frame(
      tol = tol, round = r, max_dev = d, max_erepro = me,
      n_erepro_ge1 = sum(p@species_params$erepro >= 1),
      max_repro_level = max(rl), converged = st$converged)
    cat(sprintf("   tol=%-6.3g r=%-2d dev %.5f  max erepro %.4f (%d >= 1)  max rl %.4f  %s\n",
                tol, r, d, me, sum(p@species_params$erepro >= 1), max(rl),
                if (st$converged) "conv" else "NO-CONV"))
    flush.console()
  }
}
TR <- do.call(rbind, trace)
write.csv(TR, tag("ladder_trace.csv"), row.names = FALSE)

if (is.null(best)) {
  cat("\nNO admissible converged state at the target tolerance",
      "(every candidate had erepro >= 1 or failed to converge).\n")
  cat("The capped object", OUT_CAP, "still stands on its own.\n")
} else {
  saveRDS(best, OUT_LAD)
  cat(sprintf("\n  selected: max dev %.5f | max erepro %.4f\n", best_d,
              max(best@species_params$erepro)))
  cat("  wrote", OUT_LAD, "\n")
}

# --- comparison ---------------------------------------------------------------
arms <- list(`phase57 (input)` = P, `capped` = capped)
if (!is.null(best)) arms[["capped + re-laddered"]] <- best
CMP <- do.call(rbind, lapply(names(arms), function(nm) {
  q <- arms[[nm]]
  data.frame(arm = nm, max_dev = max_dev(q),
             max_erepro = max(q@species_params$erepro),
             n_erepro_ge1 = sum(q@species_params$erepro >= 1),
             median_repro_level = median(rl_of(q)),
             n_rl_above_0.99 = sum(rl_of(q) > 0.99),
             n_rl_above_cap = sum(rl_of(q) > CAP))
}))
cat("\n=== comparison ===\n"); print(CMP, row.names = FALSE, digits = 4)

PER <- do.call(rbind, lapply(names(arms), function(nm) {
  q <- arms[[nm]]
  data.frame(arm = nm, species = SPN, erepro = q@species_params$erepro,
             R_max = q@species_params$R_max, repro_level = rl_of(q),
             bio_ratio = bio_ratio(q))
}))
w <- function(v) {
  x <- reshape(PER[, c("arm", "species", v)], idvar = "species",
               timevar = "arm", direction = "wide")
  names(x) <- sub(paste0(v, "\\."), "", names(x))
  x[match(SPN, x$species), ]
}
cat("\n=== reproduction level ===\n"); print(w("repro_level"), row.names = FALSE,
                                             digits = 4)
cat("\n=== erepro ===\n"); print(w("erepro"), row.names = FALSE, digits = 4)
write.csv(PER, tag("per_species.csv"), row.names = FALSE)
write.csv(CMP, tag("comparison.csv"), row.names = FALSE)
cat("\nelapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")
cat("Phase 58 complete.\n")