# =============================================================================
# STAGE F -- deduplicate the fitted ensemble using the ENSEMBLE OBJECTS.
#
# WHY NOT THE PARAMETER SUMMARY CSV. The mechanism of duplication is settled
# (31_dedupe_parameter_draws.R + provenance probes):
#
#   * monte_carlo_2111_summaries/all_parameters_all_simulations.csv holds 2,112
#     simulations carrying only 1,849 distinct (gamma, catchability,
#     abundance_scaling) triples -- 263 redundant copies in 241 groups.
#   * combined_rerun_params_summary_20250923_122211.csv shows 241 of 241 (100%)
#     of those groups span MULTIPLE source_file entries. sim_ids 2082, 2085 and
#     2093 all carry sim_id_within_file = 36, stable = TRUE and
#     max_cv = 0.07491564 from the same run_id but three different output files.
#     The same Monte Carlo configuration was executed repeatedly and the outputs
#     concatenated; set.seed(20250907 + i) keys the draw on the index, not the
#     run, so every repeat re-draws identical parameters.
#
# BUT those two files (2,112 rows) do not align with yield_rmse_per_sim.csv
# (2,111 rows, member 1206 dropped by create_cleaned_ensemble.R) under any
# offset tried -- +0, +1, -1, or shifting indices >= 1206. Best agreement was
# 38%, where a correct mapping must give 100%, since identical parameters must
# give identical RMSE. The ensemble-object <-> RMSE mapping IS verified
# independently (Stage 0 matched recomputed biomass to relative difference 0),
# so the summary CSV sits in a different indexing and cannot be used to name
# WHICH members are duplicates.
#
# This script therefore reads the parameter vectors straight out of the saved
# MizerParams, which are keyed on the same indexing as the RMSE table.
#
# SIGNATURE. A duplicate requires ALL THREE varied quantities to match --
# matching on two while the third differs is still a unique parameter set:
#     gamma          species_params$gamma          (search rate)
#     catchability   gear_params$catchability
#     abundance      initial_n                     (the scaled abundances)
# erepro and R_max are carried too; they are set by steady() and so act as a
# further fingerprint of an identical run.
#
# Writes Output_large_files/wmin_test/32_dedupe_212.{rds,csv}
# =============================================================================

suppressPackageStartupMessages({ library(mizer); library(dplyr) })

out_dir <- "Output_large_files/wmin_test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== Stage F: deduplicating the fitted 212 from the ensemble objects ===\n\n")

ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
idx <- ens$metadata$top10pct_indices
n <- length(idx)
cat("members:", n, "\n")

num <- function(x) sprintf("%.17g", as.numeric(x))

comp <- lapply(seq_len(n), function(m) {
  p <- ens$fished_top10pct[[m]]@params
  sp <- p@species_params
  gp <- tryCatch(p@gear_params, error = function(e) NULL)
  list(gamma = paste(num(sp$gamma), collapse = "|"),
       catch = if (!is.null(gp) && "catchability" %in% names(gp))
         paste(num(gp$catchability), collapse = "|") else "",
       abund = paste(num(p@initial_n), collapse = "|"),
       repro = paste(c(num(sp$erepro), num(sp$R_max)), collapse = "|"))
})
g_gamma <- vapply(comp, `[[`, character(1), "gamma")
g_catch <- vapply(comp, `[[`, character(1), "catch")
g_abund <- vapply(comp, `[[`, character(1), "abund")
g_repro <- vapply(comp, `[[`, character(1), "repro")

# a duplicate must match on ALL THREE varied parameters
sig_all3 <- paste(g_gamma, g_catch, g_abund, sep = "##")
sig_full <- paste(sig_all3, g_repro, sep = "##")

rmse <- read.csv("Manuscript data/yield_rmse_per_sim.csv", stringsAsFactors = FALSE)
rm_top <- rmse[match(idx, rmse$sim_index), ]

df <- data.frame(position = seq_len(n), sim_index = idx,
                 rank = rm_top$rank, rmse = rm_top$rmse, cor_log = rm_top$cor_log,
                 grp = match(sig_all3, unique(sig_all3)),
                 grp_full = match(sig_full, unique(sig_full)),
                 stringsAsFactors = FALSE)

cat("\n=== distinct members ===\n")
cat("  matching on gamma only            :", n_distinct(g_gamma), "of", n, "\n")
cat("  matching on catchability only     :", n_distinct(g_catch), "of", n, "\n")
cat("  matching on abundance only        :", n_distinct(g_abund), "of", n, "\n")
cat("  matching on ALL THREE (the rule)  :", n_distinct(sig_all3), "of", n, "\n")
cat("  ...plus erepro/R_max              :", n_distinct(sig_full), "of", n, "\n")
cat("  identical rmse + cor_log          :",
    n_distinct(paste(df$rmse, df$cor_log)), "of", n, "\n")

cat("\n  (a member matching on only one or two of the three is NOT a duplicate;\n")
cat("   the per-parameter rows above are diagnostic only)\n")

# --- consistency: identical parameters must give identical rmse --------------
gs <- df %>% group_by(grp) %>% filter(n() > 1) %>%
  summarise(n = n(), n_rmse = n_distinct(rmse),
            sims = paste(sim_index, collapse = ","), .groups = "drop")
cat("\n=== consistency check ===\n")
cat("  duplicate groups (all three match):", nrow(gs), "\n")
if (nrow(gs)) {
  cat("  ...sharing exactly one rmse       :", sum(gs$n_rmse == 1),
      sprintf("(%.0f%%)\n", 100 * mean(gs$n_rmse == 1)))
  print(as.data.frame(gs %>% select(n, n_rmse, sims)), row.names = FALSE)
  if (all(gs$n_rmse == 1)) {
    cat("\n  PASS -- every duplicate group has a single rmse, as it must.\n")
  } else {
    cat("\n  WARNING -- inconsistent. Do NOT dedupe on this until explained.\n")
  }
} else {
  cat("  none: all", n, "fitted members are distinct parameter sets\n")
}

# --- deduplicated ranking ----------------------------------------------------
dedup <- df %>% arrange(rank) %>% distinct(grp, .keep_all = TRUE) %>%
  mutate(rank_dedup = row_number())
cat("\n=== deduplicated fitted ensemble ===\n")
cat("  before :", n, "\n  after  :", nrow(dedup), "\n  removed:", n - nrow(dedup), "\n")

removed <- df %>% filter(!position %in% dedup$position)
if (nrow(removed)) {
  rep_of <- dedup %>% select(grp, kept_sim = sim_index, kept_rank = rank)
  cat("\n  removed members and the representative kept:\n")
  print(as.data.frame(removed %>% left_join(rep_of, by = "grp") %>%
                        select(sim_index, rank, rmse, kept_sim, kept_rank) %>%
                        arrange(rank)), digits = 7, row.names = FALSE)
}

saveRDS(list(table = df, dedup = dedup,
             sig_all3 = sig_all3,
             note = "signatures read from ensemble objects; summary CSV misaligned"),
        file.path(out_dir, "32_dedupe_212.rds"))
write.csv(dedup, file.path(out_dir, "32_dedupe_212.csv"), row.names = FALSE)

cat("\n=== scope note ===\n")
cat("  This covers the fitted 212 only. The 212 was ceiling(2111 * 0.10), so a\n")
cat("  fully deduplicated top 10% needs the whole 2,111 deduplicated first --\n")
cat("  that requires mc_ensemble_2111_cleaned.rds (1.86 GB) and is the next step.\n")
cat("\nWrote", file.path(out_dir, "32_dedupe_212.csv"), "\n")
