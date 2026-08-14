# =============================================================================
# VM ENVIRONMENT CHECK -- run this FIRST, before the 15-hour job.
#
# Version numbers alone do not tell you whether the VM will reproduce this work.
# What matters is whether it reproduces the NUMBERS. So this does both: it reports
# the environment, then runs an empirical test with a known answer.
#
# THE TEST. `initial_effort` is 0 for all 19 gears, so steady() and the spin-up are
# unfished and catchability enters only the 1841-2010 projection. A member's
# `params@initial_n` is bit-identical to `sim@n[1,,]`, the post-spin-up state.
# Therefore re-projecting a stored member's params under the effort array
# reproduces its stored yield exactly -- verified locally to 4.6e-15, and the
# recomputed RMSE matched the stored table to 4.9e-15 across all 2,111 members.
#
# So: project a handful of members here and compare against
# `Manuscript data/yield_rmse_per_sim.csv`. If the VM agrees to ~1e-12 the
# environment is equivalent and the long run is safe. If it does not, STOP --
# something in the toolchain changes the model, and the run would produce numbers
# that cannot be compared with anything already reported.
#
# This needs only the same ~325 MB of inputs as the main run. No steady(), no
# spin-up, so it takes about a minute.
#
# USAGE
#   Rscript R/wmin_test/41_vm_environment_check.R
#   # or, in the RStudio console:
#   source("R/wmin_test/41_vm_environment_check.R")
# =============================================================================

N_TEST <- as.integer(Sys.getenv("CHECK_N", "5"))
TOL    <- 1e-12                       # generous: local agreement is ~5e-15

cat("==============================================================\n")
cat(" VM ENVIRONMENT CHECK\n")
cat("==============================================================\n\n")

# Stop safely from BOTH contexts. `quit()` inside an interactive RStudio session
# terminates the session and RStudio restarts it, which looks like a crash and
# hides whatever the script was trying to report. Under Rscript we do want a
# non-zero exit code so a shell caller can branch on it.
bail <- function(msg = "environment check failed") {
  cat("==============================================================\n")
  if (interactive()) stop(msg, call. = FALSE)
  quit(save = "no", status = 1)
}

# --- working directory --------------------------------------------------------
# Every path in this script and in 40_vm_project_survivors.R is relative to the
# REPOSITORY ROOT. Sourcing from R/wmin_test/ silently breaks all of them.
if (!dir.exists("Output_large_files") || !file.exists("effort_array_1841_2010.rds")) {
  cat("STOP: wrong working directory.\n\n")
  cat("  currently:", getwd(), "\n")
  cat("  expected : the repository root (the folder containing\n")
  cat("             'Output_large_files/', 'effort_array_1841_2010.rds' and 'R/')\n\n")
  cat("  FIX:\n")
  cat('    setwd("/path/to/Prydz_Bay_mizer")\n')
  cat('    source("R/wmin_test/41_vm_environment_check.R")\n\n')
  cat("  All paths in these scripts are repo-root-relative, so the long run\n")
  cat("  would fail the same way ~1 minute in.\n")
  bail("set the working directory to the repository root")
}

# ---------------------------------------------------------------- 1. report ---
cat("--- 1. environment ---\n")
cat("R version    :", R.version.string, "\n")
cat("platform     :", R.version$platform, "\n")
cat("cores        :", parallel::detectCores(), "\n")
cat("memory       : ")
mem <- try(suppressWarnings(system("free -g 2>/dev/null | awk '/Mem:/{print $2\" GB\"}'",
                                   intern = TRUE)), silent = TRUE)
cat(if (!inherits(mem, "try-error") && length(mem)) mem else "unknown", "\n")
cat("locale       :", Sys.getlocale("LC_NUMERIC"), "\n\n")

want <- c(mizer = "3.1.0", therMizer = "1.0.0")
pkgs <- c("mizer", "therMizer", "dplyr", "reshape2", "parallel")
ok_pkg <- TRUE
stale <- character(0)
for (p in pkgs) {
  v <- tryCatch(as.character(packageVersion(p)), error = function(e) NA_character_)
  # A package can be LOADED at one version while a DIFFERENT version sits on
  # disk -- typically when the session predates an update. library() then tries
  # to unload the old one and fails if anything imports it, which surfaces as a
  # confusing stack trace rather than a clear message. Catch it here.
  lv <- if (p %in% loadedNamespaces())
    tryCatch(as.character(getNamespaceVersion(p)), error = function(e) NA_character_)
  else NA_character_
  mismatch <- !is.na(lv) && !is.na(v) && lv != v
  if (mismatch) stale <- c(stale, sprintf("%s (loaded %s, installed %s)", p, lv, v))
  tag <- if (is.na(v)) "MISSING"
  else if (mismatch) sprintf("** loaded %s != installed %s **", lv, v)
  else if (p %in% names(want) && v != want[[p]])
    sprintf("differs from validated %s", want[[p]]) else "ok"
  cat(sprintf("  %-10s %-10s %s\n", p, if (is.na(v)) "-" else v, tag))
  if (is.na(v)) ok_pkg <- FALSE
}

if (length(stale)) {
  cat("\n--------------------------------------------------------------\n")
  cat(" STOP: package version conflict in this R session\n")
  cat("--------------------------------------------------------------\n")
  for (s in stale) cat("  ", s, "\n")
  cat("\n  These packages were loaded before being updated on disk. library()\n")
  cat("  cannot swap them, because mizer/therMizer/tidyr/plotly import them.\n")
  cat("\n  FIX: restart R, then re-run this check.\n")
  cat("    RStudio : Session -> Restart R  (Ctrl+Shift+F10)\n")
  cat("    Rscript : just re-run -- a fresh process has no stale namespace\n")
  cat("\n  Nothing is wrong with the installation; only this session is stale.\n")
  bail("restart R first")
}

if (!ok_pkg) {
  cat("\nInstall the missing packages, then re-run:\n")
  cat('  install.packages(c("dplyr","reshape2"))\n')
  cat('  install.packages("pak"); pak::pak("sizespectrum/therMizer")\n')
  bail("missing packages")
}

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(dplyr); library(reshape2)
})
cat("\n  therMizer attached:", "therMizerEncounter" %in% ls(asNamespace("therMizer")), "\n")

# ------------------------------------------------------------ 2. input files --
cat("\n--- 2. input files ---\n")
need <- c("Output_large_files/wmin_test/39_params_cache",
          "Output_large_files/wmin_test/39_full_ensemble_paired.rds",
          "effort_array_1841_2010.rds",
          "yield_observed_timeseries.csv",
          "Manuscript data/yield_rmse_per_sim.csv")
miss <- need[!file.exists(need)]
for (f in need) cat(sprintf("  %-58s %s\n", f, if (file.exists(f)) "ok" else "MISSING"))
if (length(miss)) {
  cat("\n  ", length(miss), "input(s) missing. Copy them preserving the relative\n")
  cat("  paths shown above -- see docs/VM_RUN_MANIFEST.md. Total is ~325 MB;\n")
  cat("  'Output_large_files/wmin_test/39_params_cache/' (20 files, 324 MB) is\n")
  cat("  the bulk of it and replaces the 1.86 GB ensemble.\n")
  bail("missing input files")
}
nchunk <- length(list.files("Output_large_files/wmin_test/39_params_cache",
                            pattern = "^chunk_\\d+\\.rds$"))
cat("  params cache chunks:", nchunk, if (nchunk == 20) "(expected 20)" else
    "** expected 20 **", "\n")

# ------------------------------------------------- 3. the reproduction test ---
cat("\n--- 3. numeric reproduction test ---\n")
effort_arr <- readRDS("effort_array_1841_2010.rds")
ew <- do.call(rbind, lapply(colnames(effort_arr), function(s) {
  y <- as.numeric(rownames(effort_arr))[effort_arr[, s] > 0]
  if (length(y)) data.frame(Species = s, first_year = min(y), last_year = max(y),
                            stringsAsFactors = FALSE)
}))
obs <- read.csv("yield_observed_timeseries.csv") %>%
  reshape2::melt(id.vars = "Year", variable.name = "Species", value.name = "Yield_g") %>%
  mutate(Species = gsub("\\.", " ", as.character(Species)),
         Yield_g = pmax(coalesce(as.numeric(Yield_g), 0), 0)) %>%
  left_join(ew, by = "Species") %>%
  filter(!is.na(first_year), Year >= first_year, Year <= last_year) %>%
  select(Year, Species, Yield_obs = Yield_g)

stored <- read.csv("Manuscript data/yield_rmse_per_sim.csv")
z <- readRDS(file.path("Output_large_files/wmin_test/39_params_cache", "chunk_001.rds"))
take <- seq_len(min(N_TEST, length(z$params)))

cat("  testing", length(take), "members by re-projecting from stored params\n")
cat("  (no steady(), no spin-up -- catchability enters only the projection)\n\n")

res <- data.frame()
for (k in take) {
  p  <- z$params[[k]]
  si <- z$sim_index[k]
  vp <- try(suppressWarnings(validParams(p)), silent = TRUE)
  if (inherits(vp, "try-error")) {
    cat("  member", si, ": validParams() FAILED --", trimws(as.character(vp)), "\n")
    next
  }
  sim <- try(project(vp, t_start = 1841, effort = effort_arr), silent = TRUE)
  if (inherits(sim, "try-error")) {
    cat("  member", si, ": project() FAILED --", trimws(as.character(sim)), "\n")
    next
  }
  y <- try(getYield(sim), silent = TRUE)
  if (inherits(y, "try-error")) {
    cat("  member", si, ": getYield() FAILED --", trimws(as.character(y)), "\n")
    next
  }
  d <- reshape2::melt(y); names(d) <- c("Year", "Species", "Yield_mod")
  d$Year <- as.numeric(as.character(d$Year)); d$Species <- as.character(d$Species)
  d$Yield_mod <- pmax(d$Yield_mod, 0)
  cmp <- obs %>% left_join(d, by = c("Year", "Species")) %>%
    mutate(Yield_mod = coalesce(Yield_mod, 0))
  rmse <- sqrt(mean((log10(cmp$Yield_mod + 1) - log10(cmp$Yield_obs + 1))^2,
                    na.rm = TRUE))
  st <- stored$rmse[stored$sim_index == si]
  res <- rbind(res, data.frame(sim_index = si, stored = st, recomputed = rmse,
                               abs_diff = abs(rmse - st),
                               rel_diff = abs(rmse - st) / st))
}

if (!nrow(res)) {
  cat("\n  VERDICT: FAIL -- no member could be projected. Fix the errors above.\n")
  bail("no member could be projected")
}
print(res, digits = 12, row.names = FALSE)
worst <- max(res$rel_diff)
cat("\n  worst relative difference:", format(worst, digits = 4),
    "| tolerance", format(TOL, digits = 2), "\n")

# ------------------------------------------------------------- 4. verdict ----
cat("\n--- 4. verdict ---\n")
if (worst < TOL) {
  cat("  PASS. This VM reproduces the stored yield RMSE to", format(worst, digits = 3),
      "\n")
  cat("  The toolchain is equivalent to the one everything was validated on,\n")
  cat("  regardless of any version-string differences reported above.\n")
  cat("\n  Safe to run:\n")
  cat("    VM_CORES=30 VM_CHUNK=50 nohup Rscript \\\n")
  cat("      R/wmin_test/40_vm_project_survivors.R run > vm_run.log 2>&1 &\n")
} else {
  cat("  FAIL. Recomputed RMSE differs from stored by", format(worst, digits = 4),
      "\n")
  cat("  DO NOT run the 15-hour job. Something in this toolchain changes the\n")
  cat("  model, and the output could not be compared with anything already\n")
  cat("  reported. Most likely causes, in order:\n")
  cat("    * mizer is not 3.1.0 -- projection or selectivity internals differ\n")
  cat("    * therMizer is not 1.0.0 -- the temperature scaling differs\n")
  cat("    * a different BLAS/LAPACK giving a larger numerical drift\n")
  cat("  Pin the versions and re-run this check:\n")
  cat('    remotes::install_version("mizer", "3.1.0")\n')
  cat('    pak::pak("sizespectrum/therMizer@v1.0.0")\n')
}
cat("\n==============================================================\n")
