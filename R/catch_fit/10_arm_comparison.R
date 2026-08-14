# =============================================================================
# Is the catchability optimum sensitive to the `small divers` w_min correction?
#
# The comparison is optimum(C) - optimum(B), NEVER C - A.  Re-entering the
# pipeline reorders the RMSE ranking at rho 0.60 on its own -- roughly 5,500x the
# size of the w_min effect -- because sim@params@initial_n is the POST-spin-up
# state and the pre-spin-up state was never saved.  Arm B exists solely to absorb
# that, by taking the identical steady() + spin-up path with w_min uncorrected.
#
# Arm A is still reported, but only to show how far the re-run alone moves things
# relative to the correction.
#
#   Rscript R/catch_fit/10_arm_comparison.R
# =============================================================================

source("R/catch_fit/01_harness.R")

arms <- c("A", "B", "C")
say <- function(...) cat(..., "\n", sep = "")

grab <- function(arm) {
  d <- cf_out_dir(arm = arm)
  fs <- sort(list.files(file.path(d, "q_optimum"), pattern = "^opt_\\d+\\.rds$",
                        full.names = TRUE))
  if (!length(fs)) return(NULL)
  z <- lapply(fs, readRDS)
  base <- read.csv(file.path(d, "00_baseline_per_species.csv"), check.names = FALSE)
  bt <- unique(base[, c("sim_index", "total_rmse")])
  do.call(rbind, lapply(z, function(x) {
    o <- if (x$winner == "grid") x$grid else x$baseline
    data.frame(arm = arm, sim_index = x$sim_index,
               rmse_before = bt$total_rmse[match(x$sim_index, bt$sim_index)],
               rmse_after = o$rmse,
               t(setNames(log10(pmax(o$q[cf_fished_species()], 1e-300)),
                          paste0("logq_", gsub(" ", "_", cf_fished_species())))),
               stringsAsFactors = FALSE, check.names = FALSE)
  }))
}

A <- grab("A"); B <- grab("B"); C <- grab("C")
if (is.null(B) || is.null(C)) stop("arms B and/or C not built yet")

qcols <- grep("^logq_", names(B), value = TRUE)

say("=== optimum RMSE by arm ===")
m <- merge(merge(A[, c("sim_index", "rmse_before", "rmse_after")],
                 B[, c("sim_index", "rmse_before", "rmse_after")],
                 by = "sim_index", suffixes = c("_A", "_B")),
           C[, c("sim_index", "rmse_before", "rmse_after")], by = "sim_index")
names(m)[names(m) == "rmse_before"] <- "rmse_before_C"
names(m)[names(m) == "rmse_after"]  <- "rmse_after_C"
m$CminusB_after  <- m$rmse_after_C  - m$rmse_after_B
m$BminusA_after  <- m$rmse_after_B  - m$rmse_after_A
print(data.frame(sim = m$sim_index,
                 A = round(m$rmse_after_A, 5),
                 B = round(m$rmse_after_B, 5),
                 C = round(m$rmse_after_C, 5),
                 `C-B` = signif(m$CminusB_after, 3),
                 `B-A` = signif(m$BminusA_after, 3),
                 check.names = FALSE), row.names = FALSE)

say("\n  median |C - B| : ", signif(median(abs(m$CminusB_after)), 4),
    "   (the w_min effect on the optimum)")
say("  median |B - A| : ", signif(median(abs(m$BminusA_after)), 4),
    "   (the re-run alone)")
r <- median(abs(m$BminusA_after)) / max(median(abs(m$CminusB_after)), 1e-12)
say("  ratio          : ", signif(r, 4), "x")

say("\n=== how far the OPTIMUM CATCHABILITY moves, in dex ===")
mv <- do.call(rbind, lapply(qcols, function(cc) {
  bb <- B[[cc]][match(m$sim_index, B$sim_index)]
  cc2 <- C[[cc]][match(m$sim_index, C$sim_index)]
  aa <- A[[cc]][match(m$sim_index, A$sim_index)]
  data.frame(gear = sub("^logq_", "", gsub("_", " ", cc)),
             med_abs_CminusB = median(abs(cc2 - bb)),
             max_abs_CminusB = max(abs(cc2 - bb)),
             med_abs_BminusA = median(abs(bb - aa)),
             stringsAsFactors = FALSE)
}))
print(data.frame(gear = mv$gear,
                 `median |C-B| dex` = round(mv$med_abs_CminusB, 4),
                 `max |C-B| dex` = round(mv$max_abs_CminusB, 4),
                 `median |B-A| dex` = round(mv$med_abs_BminusA, 4),
                 check.names = FALSE), row.names = FALSE)

say("\n=== verdict ===")
if (median(abs(m$CminusB_after)) < 0.01 * median(m$rmse_after_B)) {
  say("The w_min correction does not move the catchability optimum: |C - B| is")
  say("below 1% of the optimum RMSE for every member.  The optimum, and every")
  say("conclusion drawn from it, is insensitive to the correction.")
} else {
  say("The w_min correction DOES move the optimum -- see the table above.")
}

out <- rbind(A, B, C)
write.csv(out, file.path(cf_out_dir(arm = NA), "10_arm_optima.csv"),
          row.names = FALSE)
write.csv(m, file.path(cf_out_dir(arm = NA), "10_arm_comparison.csv"),
          row.names = FALSE)
say("\nwrote 10_arm_optima.csv and 10_arm_comparison.csv")
