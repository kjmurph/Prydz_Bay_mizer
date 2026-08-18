# =============================================================================
# Phase 93 -- the corrected ranking, and the two member sets the figures use
#
# Phase 46's replacement. Same selection rule, new ensemble, and two faults
# removed from what fed it.
#
# ------------------------------------------------------------------ what moved
# 1. THE ENSEMBLE. Members are the phase-88 rebuild on params_ref_p86_agemat,
#    not the ensemble-44 states. 1,668 built.
# 2. THE SCREEN. Phase 46 ranked every state file in the directory. Phase 88
#    writes a state for every member that converges on the tolerance ladder,
#    INCLUDING those that fail the stability screen or carry erepro >= 1, so
#    that set is 1,668 of which only 427 are usable. Phase 89 (CORRECTION 5)
#    now fits catchability on the 427; this ranks the same 427.
# 3. THE CATCHABILITY. The ranking is computed POST re-fit, from
#    89_refit_results.rds, on the ISIMIP3a-compliant window ending 2004.
#
# ------------------------------------------------------- the rule is UNCHANGED
# Cut A: unweighted pooled log10 yield RMSE, sqrt(sum(sse)/sum(n)) over the nine
# fished species, ascending. That is phase 46's `A unweighted RMSE` and the rule
# every published figure rests on. This phase does NOT re-open the choice of
# rule -- it re-applies it to a corrected ensemble.
#
# ------------------------------------------------------------- the TWO sets
#   FULL  all 427 usable members          -- figures get IQR *and* 95% bands
#   TOP10 the best 43 (ceiling of 10%)    -- figures get IQR
#
# Phase 46 emitted only the top 10%. Carrying the full usable set as well means
# the figures can show what the whole admissible ensemble does, with the
# selection shown against it rather than instead of it.
#
# ------------------------------------------------------------- NO PROJECTION
# Deliberate. Everything here is already in 89_refit_results.rds; the
# trajectories the SNR needs are built once by the F00 layer, which projects
# these members anyway. Phase 46 projected inside the selection script and the
# F00 build then projected the same members again.
#
# USAGE  Rscript R/wmin_test/93_rerank_p88.R
# ENV    P93_REFIT, P93_MEMBERS_RDS, P93_TOP_FRAC, P93_OUT
# =============================================================================

suppressPackageStartupMessages({ library(dplyr) })

OL <- "Output_large_files/wmin_test"
REFIT <- Sys.getenv("P93_REFIT", file.path(OL, "89_refit_results.rds"))
MEMBERS_RDS <- Sys.getenv("P93_MEMBERS_RDS", file.path(OL, "88_full.rds"))
TOP_FRAC <- as.numeric(Sys.getenv("P93_TOP_FRAC", "0.10"))
OUT <- Sys.getenv("P93_OUT", file.path(OL, "93_rerank_p88.rds"))
BAL <- "baleen whales"

for (f in c(REFIT, MEMBERS_RDS))
  if (!file.exists(f)) stop("missing input: ", f, call. = FALSE)

RF <- readRDS(REFIT)
P88 <- readRDS(MEMBERS_RDS)
MEM <- P88$members

cat("=== Phase 93: re-rank the phase-88 ensemble ===\n")
cat("refit  :", basename(REFIT), "\n")
cat("members:", basename(MEMBERS_RDS), "\n\n")

# --- the screen, re-derived here rather than trusted -------------------------
# The refit records which members it fitted. Re-deriving the screen and checking
# the two agree is what stops a refit built on the wrong member set from
# silently setting the ranking -- the exact failure CORRECTION 5 documents.
# `usable` FOLLOWS THE MEMBER TABLE'S OWN DEFINITION, exactly as phase 89 does.
# Phase 88 had no drift test; phase 104 adds one, and using the phase-88
# definition against a phase-104 table asks for 265 members where the run itself
# accepted 203. The guard below would then fire on a refit that was in fact
# correct. `drift_ok` is used when present and ignored when absent.
usable <- sort(as.integer(MEM$sim_index[
  MEM$stable & MEM$n_erepro_ge1 == 0 &
  (if ("drift_ok" %in% names(MEM)) MEM$drift_ok else TRUE)]))
cat(sprintf("phase 88: built %d | stable %d | admissible %d | USABLE %d\n",
            nrow(MEM), sum(MEM$stable), sum(MEM$n_erepro_ge1 == 0),
            length(usable)))

fitted <- sort(unique(as.integer(RF$per_species$sim_index)))
if (!identical(usable, fitted)) {
  extra <- setdiff(fitted, usable); miss <- setdiff(usable, fitted)
  stop("the refit was not run on the usable set -- refusing to rank.\n",
       "  fitted but not usable: ", length(extra),
       if (length(extra)) paste0(" (e.g. ", extra[1], ")") else "",
       "\n  usable but not fitted: ", length(miss),
       if (length(miss)) paste0(" (e.g. ", miss[1], ")") else "",
       "\n  Re-run phase 89 with P89_SCREEN=usable.", call. = FALSE)
}
if (!is.null(RF$meta$screen) && !identical(RF$meta$screen, "usable"))
  stop("the refit records screen='", RF$meta$screen,
       "' but this phase ranks the usable set", call. = FALSE)
cat("  refit screen agrees with the re-derived usable set (", length(usable),
    " members)\n", sep = "")
if (length(RF$held_at_one))
  cat("  catchability held at the drawn value for:",
      paste(RF$held_at_one, collapse = ", "), "\n")

# --- the ranking --------------------------------------------------------------
# Recomputed from per_species rather than taken from $summary, so the metric is
# visible here and cannot drift if phase 89's summary ever changes definition.
RANK <- RF$per_species %>% group_by(sim_index) %>%
  summarise(rmse = sqrt(sum(sse) / sum(n)), n_obs = sum(n), .groups = "drop") %>%
  arrange(rmse) %>% mutate(rank = row_number())
stopifnot(nrow(RANK) == length(usable))
if (!isTRUE(all.equal(RANK$rmse[order(RANK$sim_index)],
                      RF$summary$rmse[order(RF$summary$sim_index)])))
  stop("the recomputed RMSE disagrees with the refit's own summary",
       call. = FALSE)

bal <- RF$per_species %>% filter(Species == BAL) %>%
  transmute(sim_index, baleen_ratio = mod_tot / obs_tot)
RANK <- RANK %>% left_join(bal, by = "sim_index")

N <- nrow(RANK)
n_top <- ceiling(N * TOP_FRAC)
cat(sprintf("\nranking: %d members | RMSE %.4f .. %.4f | median %.4f\n",
            N, min(RANK$rmse), max(RANK$rmse), median(RANK$rmse)))

# --- the two sets -------------------------------------------------------------
cuts <- list(
  `FULL usable`  = as.integer(sort(RANK$sim_index)),
  `TOP unweighted RMSE` = as.integer(RANK$sim_index[seq_len(n_top)]))
names(cuts)[2] <- sprintf("TOP %g%% unweighted RMSE", 100 * TOP_FRAC)

cat(sprintf("\ncuts:\n  %-28s n = %d\n  %-28s n = %d  (RMSE <= %.4f)\n",
            names(cuts)[1], length(cuts[[1]]),
            names(cuts)[2], length(cuts[[2]]), RANK$rmse[n_top]))

summ <- do.call(rbind, lapply(names(cuts), function(nm) {
  d <- RANK %>% filter(sim_index %in% cuts[[nm]])
  data.frame(cut = nm, n = nrow(d),
             rmse_median = signif(median(d$rmse), 5),
             rmse_min = signif(min(d$rmse), 5),
             rmse_max = signif(max(d$rmse), 5),
             baleen_ratio_median = signif(median(d$baleen_ratio, na.rm = TRUE), 4),
             pct_baleen_ge_0.3 = round(100 * mean(d$baleen_ratio >= 0.3,
                                                  na.rm = TRUE), 1),
             stringsAsFactors = FALSE)
}))
cat("\n=== the two sets ===\n"); print(summ, row.names = FALSE)

# --- what changed against the ensemble-44 cut A -------------------------------
# Not a check, a record: the two ensembles are different models, so overlap is
# expected to be low and a low number is not a fault. It is reported because
# every figure that carries the `rebuilt167` suffix rests on the old membership.
old_f <- file.path(OL, "46_selection_cuts.rds")
if (file.exists(old_f)) {
  oldA <- as.integer(readRDS(old_f)$cuts[["A unweighted RMSE"]])
  shared_top <- length(intersect(cuts[[2]], oldA))
  cat(sprintf("\nagainst ensemble-44 cut A (n = %d):\n", length(oldA)))
  cat(sprintf("  shared with the new top %d : %d\n", n_top, shared_top))
  cat(sprintf("  old cut A members that are not even usable here: %d of %d\n",
              length(setdiff(oldA, usable)), length(oldA)))
}

cat("\n=== the ranked head ===\n")
print(as.data.frame(RANK %>% head(10) %>%
  transmute(rank, sim_index, rmse = signif(rmse, 5),
            baleen_ratio = signif(baleen_ratio, 3))), row.names = FALSE)

saveRDS(list(
  cuts = cuts, ranking = RANK, comparison = summ,
  usable = usable, n_top = n_top, top_frac = TOP_FRAC,
  rule = paste0("unweighted pooled log10 yield RMSE, sqrt(sum(sse)/sum(n)) ",
                "over the nine fished species, computed POST catchability ",
                "re-fit on the ISIMIP3a window ending 2004, over the members ",
                "passing the phase-88 usable screen (stable AND no erepro>=1)"),
  meta = list(refit = REFIT, members_rds = MEMBERS_RDS,
              screen = "usable", n_usable = length(usable),
              held_at_one = RF$held_at_one,
              catchability_M = RF$M,
              built = Sys.time())), OUT)

write.csv(RANK, file.path(OL, "93_rerank_p88_ranking.csv"), row.names = FALSE)
cat("\nWROTE", OUT, "and 93_rerank_p88_ranking.csv\n")
cat("NEXT: the F00 data build against these cuts, then the figure scripts.\n")
