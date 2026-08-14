# =============================================================================
# STAGE F (full) -- deduplicate the whole 2,111 ensemble, then recompute yield
# RMSE from scratch so the top 10% is derived from a clean, duplicate-free
# ranking.
#
# WHY THE WHOLE ENSEMBLE, NOT JUST THE 212. The fitted set was
# ceiling(2111 * 0.10) = 212 taken from a ranking over an ensemble that contains
# repeated draws. Deduplicating only the 212 leaves two problems: the cutoff N is
# computed from an inflated total, and members ranked just outside 212 that
# should be promoted once duplicates are removed never get considered. The clean
# order is dedupe -> re-rank -> re-cut.
#
# ESTABLISHED MECHANISM. combined_rerun_params_summary_20250923_122211.csv shows
# the same Monte Carlo configuration was executed repeatedly and every output
# concatenated: 241 of 241 duplicate-parameter groups span multiple source_file
# entries, and sim_ids 2082/2085/2093 all carry sim_id_within_file = 36 with
# identical stability diagnostics from three different output files.
# set.seed(20250907 + i) keys the draw on the index, not the run, so each repeat
# re-draws identical parameters.
#
# WHY NOT THE PARAMETER SUMMARY CSV. all_parameters_all_simulations.csv and the
# provenance file hold 2,112 rows; yield_rmse_per_sim.csv holds 2,111 (member
# 1206 dropped by create_cleaned_ensemble.R). They do not align under any offset
# tried (+0, +1, -1, or shifting indices >= 1206); best agreement was 38% where a
# correct mapping must give 100%, since identical parameters must give identical
# RMSE. Fingerprints are therefore read straight from the ensemble objects, which
# share the RMSE table's indexing (yield_rmse_evaluation.R sets
# sim_index = seq_len(n_sims) over the same valid-sim filter used here).
#
# DUPLICATE RULE. All THREE varied quantities must match. Matching on two while
# the third differs is a distinct parameter set and is kept.
#   gamma         species_params$gamma
#   catchability  gear_params$catchability
#   abundance     initial_n  (deterministic in the abundance draw)
#
# RMSE is recomputed with the exact formula from yield_rmse_evaluation.R:112-140
# -- log10(g+1), restricted per species to its effort window -- and cross-checked
# against the stored table, which also validates that table.
#
# Writes Output_large_files/wmin_test/33_dedupe_full.rds
#        Manuscript data/yield_rmse_per_sim_deduped.csv
# =============================================================================

# therMizer is REQUIRED even though nothing here projects: params@rates_funcs
# names therMizerEncounter/PredRate/EReproAndGrowth, and getYield() routes
# through projectRateFunctions, which fails with
# "object 'therMizerEncounter' not found" if the package is not attached.
suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(dplyr); library(reshape2)
})

out_dir <- "Output_large_files/wmin_test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
CORR_THRESHOLD <- 0.5

t0 <- proc.time()
cat("=== Stage F (full): dedupe 2,111 + clean RMSE ===\n")
cat("started", format(Sys.time()), "\n\n")

mc_path <- "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds"
stopifnot(file.exists(mc_path))
cat("loading ensemble (1.86 GB, 2-4 min)...\n")
mc <- readRDS(mc_path)
sims_all <- mc$simulations

# identical validity filter to yield_rmse_evaluation.R:51-55
valid_idx <- which(vapply(sims_all, function(x)
  inherits(x, "MizerSim") && !any(is.nan(x@n)) && !any(is.infinite(x@n)),
  logical(1)))
sims <- sims_all[valid_idx]
n <- length(sims)
rm(sims_all, mc); invisible(gc())
cat("valid sims:", n, "\n\n")

# --- fingerprints ------------------------------------------------------------
cat("building parameter fingerprints...\n")
fp <- lapply(seq_len(n), function(i) {
  p <- sims[[i]]@params
  gp <- tryCatch(p@gear_params, error = function(e) NULL)
  c(as.numeric(p@species_params$gamma),
    if (!is.null(gp) && "catchability" %in% names(gp))
      as.numeric(gp$catchability) else numeric(0),
    as.numeric(p@initial_n))
})
len <- vapply(fp, length, integer(1))
stopifnot(length(unique(len)) == 1)
M <- matrix(unlist(fp), nrow = n, byrow = TRUE)
rm(fp); invisible(gc())
cat("  fingerprint matrix:", nrow(M), "x", ncol(M), "\n")

# component-wise diagnostics (NOT the dedupe rule)
p1 <- sims[[1]]@params
n_gamma <- nrow(p1@species_params)
n_catch <- ncol(M) - n_gamma - length(as.numeric(p1@initial_n))
i_gamma <- seq_len(n_gamma)
i_catch <- if (n_catch > 0) n_gamma + seq_len(n_catch) else integer(0)
i_abund <- (n_gamma + n_catch + 1):ncol(M)

dup_all <- duplicated(M)
grp <- match(apply(M, 1, function(r) paste(sprintf("%.17g", r), collapse = "|")),
             unique(apply(M, 1, function(r) paste(sprintf("%.17g", r), collapse = "|"))))

cat("\n=== distinct members ===\n")
cat("  gamma only          :", nrow(unique(M[, i_gamma, drop = FALSE])), "of", n, "\n")
if (n_catch > 0)
  cat("  catchability only   :", nrow(unique(M[, i_catch, drop = FALSE])), "of", n, "\n")
cat("  abundance only      :", nrow(unique(M[, i_abund, drop = FALSE])), "of", n, "\n")
cat("  ALL THREE (the rule):", n - sum(dup_all), "of", n, "\n")
cat("  redundant copies    :", sum(dup_all), "\n")
cat("  duplicate groups    :", sum(table(grp) > 1), "\n")
if (any(table(grp) > 1))
  cat("  largest group size  :", max(table(grp)), "\n")

# --- recompute RMSE ----------------------------------------------------------
cat("\nrecomputing yield RMSE (exact formula from yield_rmse_evaluation.R)...\n")
effort_arr <- readRDS("effort_array_1841_2010.rds")
effort_windows <- do.call(rbind, lapply(colnames(effort_arr), function(sp) {
  yrs <- as.numeric(rownames(effort_arr))[effort_arr[, sp] > 0]
  if (length(yrs) > 0)
    data.frame(Species = sp, first_year = min(yrs), last_year = max(yrs),
               stringsAsFactors = FALSE)
}))
obs_raw <- read.csv("yield_observed_timeseries.csv")
obs_long <- obs_raw %>%
  reshape2::melt(id.vars = "Year", variable.name = "Species", value.name = "Yield_g") %>%
  mutate(Species = gsub("\\.", " ", as.character(Species)),
         Yield_g = pmax(coalesce(as.numeric(Yield_g), 0), 0)) %>%
  left_join(effort_windows, by = "Species") %>%
  filter(!is.na(first_year), Year >= first_year, Year <= last_year) %>%
  select(Year, Species, Yield_obs = Yield_g)
cat("  comparison rows:", nrow(obs_long), "\n")

# VERSION NOTE. These sims were written by mizer 2.5.0; the installed mizer is
# 3.1.0. getYield.MizerSim dereferences sim@params@second_order_w[["bin_average"]]
# before branching, and 2.5.0 objects have no such slot, so it errors outright.
# validParams() is mizer's own sanctioned upgrade -- the fix its warning
# recommends -- and it sets second_order_w = list(flux = "upwind",
# bin_average = FALSE). bin_average = FALSE means the upgraded object takes the
# same else-branch mizer 2.5.0 used, so the computation is unchanged.
#
# Verified rather than assumed (scratch test, 3 sims): validParams() + getYield()
# agrees with a hand-coded 2.5.0 else-branch to max relative difference 0, and
# both reproduce the stored yield_rmse_per_sim.csv values to ~3e-15. Downgrading
# mizer is therefore unnecessary; it would also break therMizer 1.0.0 and
# invalidate 17_validate_rerun.R, which pinned the re-run pipeline under 3.1.0.
upgrade_sim <- function(sim) {
  sim@params <- suppressWarnings(validParams(sim@params))
  sim
}

rmse_v <- numeric(n); cor_raw_v <- numeric(n); cor_log_v <- numeric(n)
for (i in seq_len(n)) {
  if (i %% 250 == 0) cat("   sim", i, "/", n, "\n")
  df <- reshape2::melt(getYield(upgrade_sim(sims[[i]])))
  names(df) <- c("Year", "Species", "Yield_mod")
  df$Year <- as.numeric(as.character(df$Year))
  df$Species <- as.character(df$Species)
  df$Yield_mod <- pmax(df$Yield_mod, 0)
  comp <- obs_long %>% left_join(df, by = c("Year", "Species")) %>%
    mutate(Yield_mod = coalesce(Yield_mod, 0))
  rmse_v[i] <- sqrt(mean((log10(comp$Yield_mod + 1) -
                            log10(comp$Yield_obs + 1))^2, na.rm = TRUE))
  cor_raw_v[i] <- suppressWarnings(cor(comp$Yield_mod, comp$Yield_obs,
                                       use = "complete.obs", method = "pearson"))
  cor_log_v[i] <- suppressWarnings(cor(log10(comp$Yield_mod + 1),
                                       log10(comp$Yield_obs + 1),
                                       use = "complete.obs", method = "pearson"))
}

all_df <- data.frame(sim_index = seq_len(n), valid_sim_index = valid_idx,
                     rmse = rmse_v, cor_raw = cor_raw_v, cor_log = cor_log_v,
                     grp = grp, is_dup = dup_all, stringsAsFactors = FALSE)

# --- cross-check against the stored table ------------------------------------
cat("\n=== cross-check vs stored yield_rmse_per_sim.csv ===\n")
stored <- read.csv("Manuscript data/yield_rmse_per_sim.csv", stringsAsFactors = FALSE)
m <- all_df %>% inner_join(stored %>% select(sim_index, rmse_stored = rmse),
                           by = "sim_index")
d <- abs(m$rmse - m$rmse_stored)
cat("  matched rows:", nrow(m), "| max |diff|:", format(max(d), digits = 4),
    "| n differing > 1e-8:", sum(d > 1e-8), "\n")
cat("  ->", if (max(d) < 1e-8) "stored table REPRODUCED" else
  "stored table DIFFERS -- investigate", "\n")

# --- consistency: identical parameters must give identical RMSE --------------
gs <- all_df %>% group_by(grp) %>% filter(n() > 1) %>%
  summarise(n = n(), n_rmse = n_distinct(round(rmse, 12)), .groups = "drop")
cat("\n=== consistency ===\n")
cat("  duplicate groups:", nrow(gs), "| sharing one rmse:", sum(gs$n_rmse == 1),
    sprintf("(%.0f%%)\n", 100 * mean(gs$n_rmse == 1)))
if (nrow(gs) && !all(gs$n_rmse == 1))
  cat("  WARNING: identical fingerprints with differing RMSE -- investigate\n")

# --- deduplicated ranking and the new top 10% --------------------------------
dedup <- all_df %>% filter(!is_dup) %>% arrange(rmse) %>%
  mutate(rank = row_number(),
         pass_corr = !is.na(cor_log) & cor_log > CORR_THRESHOLD)
n_u <- nrow(dedup)
n_top_new <- ceiling(n_u * 0.10)
n_top_old <- ceiling(n * 0.10)

cat("\n=== the new top 10% ===\n")
cat("  ensemble before dedupe :", n, " -> top 10% =", n_top_old, "\n")
cat("  ensemble after dedupe  :", n_u, " -> top 10% =", n_top_new, "\n")

new_top <- dedup$sim_index[seq_len(n_top_new)]
old_top <- stored %>% arrange(rank) %>% slice_head(n = n_top_old) %>% pull(sim_index)
cat("  retained from old top 10%:", sum(new_top %in% old_top), "of", n_top_new, "\n")
cat("  promoted from outside    :", sum(!new_top %in% old_top), "\n")
cat("  dropped from old top 10% :", sum(!old_top %in% new_top), "\n")
cat("  duplicates inside old top 10%:", sum(all_df$is_dup[all_df$sim_index %in% old_top]), "\n")
cat("  passing the cor_log > 0.5 screen:", sum(dedup$pass_corr[seq_len(n_top_new)]),
    "of", n_top_new, "\n")

saveRDS(list(all = all_df, dedup = dedup, new_top = new_top, old_top = old_top,
             n_unique = n_u, n_top_new = n_top_new),
        file.path(out_dir, "33_dedupe_full.rds"))
write.csv(dedup %>% select(sim_index, valid_sim_index, rmse, cor_raw, cor_log,
                           rank, pass_corr),
          "Manuscript data/yield_rmse_per_sim_deduped.csv", row.names = FALSE)
write.csv(all_df, file.path(out_dir, "33_all_sims_rmse_dedup_flags.csv"),
          row.names = FALSE)

cat("\nWrote Manuscript data/yield_rmse_per_sim_deduped.csv\n")
cat("elapsed:", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
