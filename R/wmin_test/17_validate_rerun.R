# =============================================================================
# VALIDATION GATE for the Stage 1 re-run, using the REAL therMizer package
# (v1.0.0, sizespectrum/therMizer, as the workflow Rmds install it).
#
# sim@params@initial_n is the post-spin-up state (verified identical to
# sim@n[1,,]), so re-projecting an UNMODIFIED member from it with its own stored
# effort must reproduce the stored trajectory. Nothing downstream is trusted
# until this passes.
#
# Also checks that the temperature scaling used for the Stage 0 numbers
# (R/wmin_test/thermizer_shim.R) agrees with therMizer's own scaled_temp_effect.
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer)
  library(mizer)
})
source("R/wmin_test/thermizer_shim.R")

out_dir <- "Output_large_files/wmin_test"
ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")

cat("mizer:", as.character(packageVersion("mizer")),
    " therMizer:", as.character(packageVersion("therMizer")), "\n\n")

# --- (1) does the Stage 0 shim's temperature scaling match therMizer? --------
cat("=== Stage 0 shim vs therMizer::scaled_temp_effect ===\n")
ste <- get("scaled_temp_effect", envir = asNamespace("therMizer"))
p1 <- ens$fished_top10pct[[1]]@params
worst <- 0
for (y in c(1841, 1900, 1950, 2005, 2010)) {
  a <- ther_temp_effect(p1, y)                 # my Stage 0 implementation
  b <- ste(p1, y)                              # therMizer's own
  b <- if (is.matrix(b)) b[, 1] else b
  worst <- max(worst, max(abs(a - as.numeric(b)) / as.numeric(b)))
}
cat("  max rel diff over 5 years:", format(worst, digits = 4),
    if (worst < 1e-12) "   -> Stage 0 scaling CONFIRMED\n" else "   -> DIFFERS\n")

# --- (2) re-run an unmodified member ----------------------------------------
cat("\n=== re-run member 1 (sim_index 446), unmodified ===\n")
sim <- ens$fished_top10pct[[1]]
p   <- sim@params
ref <- sim@n
wdw <- p@w * p@dw

t0 <- proc.time()
new <- project(p, t_start = 1841, effort = sim@effort)
cat("  projected in", round((proc.time() - t0)["elapsed"], 1), "s;",
    "dim(n) =", dim(new@n), "\n")

yrs <- intersect(as.numeric(dimnames(new@n)$time), as.numeric(dimnames(ref)$time))
dn <- sapply(as.character(yrs), function(y) {
  a <- new@n[y, , ]; b <- ref[y, , ]; sel <- b > 0
  max(abs(a[sel] - b[sel]) / b[sel])
})
dbm <- sapply(as.character(yrs), function(y) {
  a <- rowSums(sweep(new@n[y, , ], 2, wdw, "*"))
  b <- rowSums(sweep(ref[y, , ], 2, wdw, "*"))
  max(abs(a - b) / b)
})
show <- as.character(c(1841, 1842, 1850, 1900, 1930, 1950, 2000, 2010))
show <- show[show %in% names(dn)]
cat("  max rel diff in n      :",
    paste(sprintf("%s:%.2e", show, dn[show]), collapse = "  "), "\n")
cat("  max rel diff in biomass:",
    paste(sprintf("%s:%.2e", show, dbm[show]), collapse = "  "), "\n")

npp_rel <- {
  nt <- min(nrow(new@n_pp), nrow(sim@n_pp))
  a <- new@n_pp[1:nt, ]; b <- sim@n_pp[1:nt, ]; sel <- b > 0
  max(abs(a[sel] - b[sel]) / b[sel])
}
cat("  max rel diff in n_pp   :", format(npp_rel, digits = 4), "\n")

pass <- max(dbm) < 1e-6
cat("\n  VERDICT:", if (pass) "REPRODUCED -- re-run pipeline is trustworthy"
    else "NOT reproduced -- investigate before running the experiment", "\n")

saveRDS(list(dn = dn, dbm = dbm, npp_rel = npp_rel, pass = pass,
             mizer = as.character(packageVersion("mizer")),
             therMizer = as.character(packageVersion("therMizer"))),
        file.path(out_dir, "17_validate_rerun.rds"))
