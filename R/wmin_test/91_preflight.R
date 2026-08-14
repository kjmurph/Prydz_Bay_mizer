# =============================================================================
# Phase 91 -- PREFLIGHT for the 87 -> 88 -> 89 re-run. Run this FIRST.
#
# 41_vm_environment_check.R is the ancestor of this script and still the right
# check for the PHASE-40 job. It is the WRONG check for this one: it requires
# 39_params_cache/ and 39_full_ensemble_paired.rds, which this run does not use,
# and its package list predates tidyr, which phases 89 and 90 both need. Running
# it here fails on files that do not matter and passes without testing the ones
# that do.
#
# ------------------------------------------------------------- what it checks
#   1. working directory (every path in these scripts is repo-root-relative)
#   2. R, cores, memory, and DISK -- disk is the VM's tight resource
#   3. packages, including the loaded-vs-installed mismatch trap
#   4. the scripts and inputs THIS run needs
#   5. the reference model's invariants, the ones phase 88 asserts at run time
#   6. the draws file, and what phase 87 will actually do to it
#   7. a NUMERIC FINGERPRINT with a known answer
#
# ------------------------------------------------------- why the fingerprint
# Version strings do not tell you whether a machine reproduces the NUMBERS. This
# projects the reference model across the full 1841-2010 effort array and
# compares per-species yield totals against values committed to the repo. That
# exercises the whole stack the run depends on -- therMizer's temperature
# scaling, plankton_forcing, the gear parameters, the effort array and
# getYield() -- in about ten seconds.
#
# If the fingerprint fails, STOP. The run would produce numbers that cannot be
# compared with anything already reported.
#
# USAGE
#   Rscript R/wmin_test/91_preflight.R              # check (exit 1 on failure)
#   Rscript R/wmin_test/91_preflight.R fingerprint  # REGENERATE the expected file
#
# Regenerate the fingerprint ONLY when the reference model deliberately changes,
# and commit the result in the same commit as the new reference.
# =============================================================================

FINGERPRINT <- "docs/vm_preflight_fingerprint.csv"
BASE_FILE   <- Sys.getenv("P91_BASE", "params_ref_p86_agemat.rds")
TOL         <- as.numeric(Sys.getenv("P91_TOL", "1e-10"))
DISK_NEED_GB <- 1.0     # ~0.34 GB of states plus chunks and headroom
mode <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(mode)) mode <- "check"
stopifnot(mode %in% c("check", "fingerprint"))

fails <- character(0)
warns <- character(0)
note  <- function(ok, label, detail = "", warn_only = FALSE) {
  cat(sprintf("  %-52s %s%s\n", label,
              if (ok) "ok" else if (warn_only) "WARN" else "FAIL",
              if (nzchar(detail)) paste0("  ", detail) else ""))
  if (!ok) {
    if (warn_only) warns <<- c(warns, label) else fails <<- c(fails, label)
  }
  invisible(ok)   # called at top level: without this every check prints [1] TRUE
}
bail <- function(msg) {
  cat("\n==============================================================\n")
  cat(" PREFLIGHT FAILED --", msg, "\n")
  cat("==============================================================\n")
  if (interactive()) stop(msg, call. = FALSE)
  quit(save = "no", status = 1)
}

cat("==============================================================\n")
cat(" PHASE 91 PREFLIGHT -- the 87 -> 88 -> 89 re-run\n")
cat("==============================================================\n\n")

# --------------------------------------------------------- 1. working dir ---
cat("--- 1. working directory ---\n")
if (!dir.exists("Output_large_files") ||
    !file.exists("effort_array_1841_2010.rds")) {
  cat("  currently:", getwd(), "\n")
  cat("  expected : the repository root (containing Output_large_files/,\n")
  cat("             effort_array_1841_2010.rds and R/)\n")
  bail("wrong working directory")
}
note(TRUE, "repository root", getwd())

# ------------------------------------------------------- 2. machine, disk ---
cat("\n--- 2. machine ---\n")
cat("  R version   :", R.version.string, "\n")
cat("  platform    :", R.version$platform, "\n")
NCORE <- parallel::detectCores()
cat("  cores       :", NCORE, "-> this run should use", max(1L, NCORE - 2L), "\n")
mem <- try(suppressWarnings(system(
  "free -g 2>/dev/null | awk '/Mem:/{print $2}'", intern = TRUE)), silent = TRUE)
cat("  memory      :", if (!inherits(mem, "try-error") && length(mem))
  paste(mem, "GB") else "unknown", "\n")
dfree <- try(suppressWarnings(system(
  "df -BG --output=avail . 2>/dev/null | tail -1", intern = TRUE)), silent = TRUE)
gb <- suppressWarnings(as.numeric(gsub("[^0-9.]", "",
  if (!inherits(dfree, "try-error") && length(dfree)) dfree else NA)))
if (is.finite(gb)) {
  note(gb >= DISK_NEED_GB, "disk headroom",
       sprintf("%.0f GB free, need ~%.1f GB", gb, DISK_NEED_GB))
  if (is.finite(gb) && gb < 3)
    cat("     TIP: ~/40_states.tar is 727 MB and is duplicated locally.\n")
} else cat("  disk        : unknown (not Linux?)\n")

# ---------------------------------------------------------- 3. packages ----
cat("\n--- 3. packages ---\n")
want <- c(mizer = "3.1.0", therMizer = "1.0.0")
pkgs <- c("mizer", "therMizer", "dplyr", "tidyr", "parallel", "reshape2")
stale <- character(0)
for (p in pkgs) {
  v <- tryCatch(as.character(packageVersion(p)), error = function(e) NA_character_)
  lv <- if (p %in% loadedNamespaces())
    tryCatch(as.character(getNamespaceVersion(p)), error = function(e) NA_character_)
  else NA_character_
  mismatch <- !is.na(lv) && !is.na(v) && lv != v
  if (mismatch) stale <- c(stale, sprintf("%s (loaded %s, installed %s)", p, lv, v))
  detail <- if (is.na(v)) "not installed" else v
  if (!is.na(v) && p %in% names(want) && v != want[[p]])
    detail <- paste0(v, "  (validated on ", want[[p]], ")")
  note(!is.na(v) && !mismatch, paste("package", p), detail)
}
if (length(stale)) {
  cat("\n  Loaded-vs-installed mismatch:", paste(stale, collapse = "; "), "\n")
  cat("  These were loaded before being updated on disk; library() cannot swap\n")
  cat("  them. FIX: restart R (Rscript: just re-run) and check again.\n")
  bail("stale package namespace")
}
if (length(fails)) {
  cat('\n  install.packages(c("dplyr","tidyr","reshape2"))\n')
  cat('  remotes::install_version("mizer", "3.1.0")\n')
  cat('  pak::pak("sizespectrum/therMizer@v1.0.0")\n')
  bail("missing packages")
}
suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(dplyr); library(tidyr)
})
note("therMizerEncounter" %in% ls(asNamespace("therMizer")),
     "therMizer exports therMizerEncounter")

# -------------------------------------------------- 4. scripts and inputs ---
cat("\n--- 4. scripts ---\n")
for (f in c("R/wmin_test/87_substitute_draws.R",
            "R/wmin_test/88_full_ensemble.R",
            "R/wmin_test/89_catchability_refit_2004.R"))
  note(file.exists(f), basename(f))

cat("\n--- 5. inputs ---\n")
OL <- "Output_large_files/wmin_test"
inputs <- c(BASE_FILE,
            "effort_array_1841_2010.rds",
            "yield_observed_timeseries.csv",
            file.path(OL, "43_member_draws.rds"),
            file.path(OL, "45_refit_results.rds"),
            file.path(OL, "46_selection_cuts.rds"))
for (f in inputs)
  note(file.exists(f), f, if (file.exists(f))
    sprintf("%.1f MB", file.size(f) / 1e6) else "")
if (length(fails)) {
  cat("\n  Output_large_files/ is gitignored, so the three files under it do NOT\n")
  cat("  arrive with a git pull. They must be copied across preserving these\n")
  cat("  relative paths. See docs/VM_RERUN_PLAN.md.\n")
  bail("missing inputs")
}

# ------------------------------------------- 6. the reference model itself ---
cat("\n--- 6. reference model:", basename(BASE_FILE), "---\n")
BASE <- suppressWarnings(validParams(readRDS(BASE_FILE)))
SPN <- BASE@species_params$species
note(length(SPN) == 19, "19 functional groups", paste(length(SPN), "found"))
note(identical(BASE@rates_funcs$Encounter, "therMizerEncounter"),
     "Encounter is therMizerEncounter", BASE@rates_funcs$Encounter)
note(identical(BASE@resource_dynamics, "plankton_forcing"),
     "resource_dynamics is plankton_forcing", BASE@resource_dynamics)
note(identical(BASE@second_order_w$flux, "upwind"), "flux scheme is upwind")
note(!all(BASE@ext_encounter == 0),
     "out-of-domain subsidy present (ext_encounter)")
sp <- BASE@species_params
note("age_mat" %in% names(sp) && !anyNA(sp$age_mat),
     "age_mat column complete")
er <- max(sp$erepro)
note(er < 1, "max erepro below 1", signif(er, 4))
rl <- as.numeric(getReproductionLevel(BASE))
cat(sprintf("  %-52s %s\n", "reproduction level: median / max",
            paste(round(median(rl), 4), "/", round(max(rl), 4))))

# --------------------------------------------------------- 7. the draws -----
cat("\n--- 7. draws and member set ---\n")
DR <- readRDS(file.path(OL, "43_member_draws.rds"))
note(!is.null(DR$draws) && length(DR$draws) > 0, "draws present",
     paste(length(DR$draws), "members"))
sp_dr <- names(DR$draws[[1]]$abundance_scaling)
note(identical(sort(sp_dr), sort(SPN)),
     "draw species match the reference model")
RF <- readRDS(file.path(OL, "45_refit_results.rds"))
RANK <- RF$per_species %>% group_by(sim_index) %>%
  summarise(m = sqrt(sum(sse) / sum(n)), .groups = "drop") %>% arrange(m)
CUTS <- readRDS(file.path(OL, "46_selection_cuts.rds"))
cutA <- as.integer(CUTS$cuts[["A unweighted RMSE"]])
note(identical(sort(as.integer(head(RANK$sim_index, length(cutA)))), sort(cutA)),
     "top of the ranking reproduces cut A",
     sprintf("%d ranked, cut A = %d", nrow(RANK), length(cutA)))
note(!length(setdiff(as.character(RANK$sim_index), names(DR$draws))),
     "every ranked member has draws")

TREATED <- c("mesozooplankton", "baleen whales", "minke whales",
             "sperm whales", "orca")
cat("\n  phase 87 will substitute these draws below 1:\n")
for (s in TREATED) {
  v <- vapply(names(DR$draws), function(m)
    as.numeric(DR$draws[[m]]$abundance_scaling[s]), numeric(1))
  cat(sprintf("    %-18s %4d of %d below 1 (%.1f%%)\n", s, sum(v < 1),
              length(v), 100 * mean(v < 1)))
}
already <- file.path(OL, "87_member_draws_substituted.rds")
if (file.exists(already))
  note(FALSE, "87 output does not already exist",
       "present -- 87 will refuse unless P87_FORCE=1", warn_only = TRUE)

# ------------------------------------------------- 8. numeric fingerprint ---
cat("\n--- 8. numeric fingerprint ---\n")
effort_arr <- readRDS("effort_array_1841_2010.rds")
cat("  projecting the reference across 1841-2010 under the effort array...\n")
t0 <- proc.time()
sim <- try(project(BASE, t_start = 1841, effort = effort_arr,
                   progress_bar = FALSE), silent = TRUE)
if (inherits(sim, "try-error")) {
  cat("  project() FAILED:", trimws(as.character(sim)), "\n")
  bail("the reference model does not project on this machine")
}
Y <- getYield(sim)
got <- data.frame(species = colnames(Y),
                  yield_total = as.numeric(colSums(Y)),
                  stringsAsFactors = FALSE)
B <- getBiomass(sim)
got$biomass_final <- as.numeric(B[nrow(B), ])
cat(sprintf("  done in %.1f s\n", (proc.time() - t0)[["elapsed"]]))

if (mode == "fingerprint") {
  dir.create(dirname(FINGERPRINT), showWarnings = FALSE, recursive = TRUE)
  write.csv(got, FINGERPRINT, row.names = FALSE)
  cat("\n  WROTE", FINGERPRINT, "-- commit this with the reference model.\n")
  print(got, row.names = FALSE, digits = 10)
  quit(save = "no", status = 0)
}

if (!file.exists(FINGERPRINT)) {
  note(FALSE, "expected fingerprint file", paste("missing:", FINGERPRINT),
       warn_only = TRUE)
  cat("  Regenerate on a machine you trust:\n")
  cat("    Rscript R/wmin_test/91_preflight.R fingerprint\n")
} else {
  exp <- read.csv(FINGERPRINT, stringsAsFactors = FALSE)
  cmp <- merge(exp, got, by = "species", suffixes = c("_exp", "_got"))
  note(nrow(cmp) == nrow(exp), "fingerprint species line up",
       sprintf("%d of %d", nrow(cmp), nrow(exp)))
  rel <- function(a, b) ifelse(abs(b) > 0, abs(a - b) / abs(b), abs(a - b))
  cmp$rel_yield <- rel(cmp$yield_total_got, cmp$yield_total_exp)
  cmp$rel_biom  <- rel(cmp$biomass_final_got, cmp$biomass_final_exp)
  worst <- max(c(cmp$rel_yield, cmp$rel_biom), na.rm = TRUE)
  bad <- cmp[cmp$rel_yield > TOL | cmp$rel_biom > TOL, ]
  if (nrow(bad)) {
    cat("\n  species outside tolerance:\n")
    print(bad[, c("species", "rel_yield", "rel_biom")], row.names = FALSE,
          digits = 6)
  }
  note(worst < TOL, "reproduces the stored fingerprint",
       sprintf("worst relative difference %.3g (tol %.0e)", worst, TOL))
}

# ------------------------------------------------------------- verdict -----
cat("\n--- verdict ---\n")
if (length(fails)) {
  for (f in fails) cat("  FAIL:", f, "\n")
  bail(paste(length(fails), "check(s) failed"))
}
if (length(warns)) for (w in warns) cat("  WARN:", w, "\n")
cat("  PASS. This machine is ready for the re-run.\n\n")
cat("  Rscript R/wmin_test/87_substitute_draws.R\n")
cat("  P61_CORES=", max(1L, NCORE - 2L),
    " Rscript R/wmin_test/88_full_ensemble.R run\n", sep = "")
cat("  P89_CORES=", max(1L, NCORE - 2L),
    " Rscript R/wmin_test/89_catchability_refit_2004.R\n", sep = "")
cat("\n  Progress, without disturbing the run:\n")
cat("  Rscript R/wmin_test/88_full_ensemble.R status\n")
cat("\n==============================================================\n")