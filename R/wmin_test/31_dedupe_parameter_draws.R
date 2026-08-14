# =============================================================================
# STAGE F (run first) -- how many of the 2,111 accepted members are actually
# distinct parameter draws, and what is the top 10% once duplicates are removed?
#
# WHY THIS COMES FIRST. 25_rmse_context.R found 208 members sharing identical
# rmse AND cor_raw AND cor_log -- 94 groups -- concentrated at the top of the
# ranking (ranks 4-9 are one model). But identical fit statistics are only
# CIRCUMSTANTIAL evidence of a repeated draw. This script settles it against the
# parameter draws themselves: sim_id x 19 species x (gamma, catchability,
# abundance_scaling) = 57 numbers per simulation.
#
# The likely mechanism: the 2,111 is a concatenation of 33 Monte Carlo runs, and
# run_single_enhanced_sim seeds as set.seed(20250907 + i) -- a per-worker seed
# keyed on the draw INDEX, not on the run. Any two runs therefore re-draw
# identical parameter sets for overlapping i. Results are then collected by
# success order, so duplicates survive into the accepted set.
#
# Everything downstream is keyed on the top 10%, so the deduplicated count has to
# be settled before the corrected-w_min ensemble is rebuilt -- otherwise the
# duplication is baked into the new ensemble too.
#
# Writes Output_large_files/wmin_test/31_dedupe_{groups,unique_ranking}.csv
#        Manuscript data/yield_rmse_per_sim_deduped.csv
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

out_dir <- "Output_large_files/wmin_test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== Stage F: duplicate parameter draws in the 2,111 ensemble ===\n\n")

# --- 1. the parameter draws ---------------------------------------------------
pars <- read.csv("monte_carlo_2111_summaries/all_parameters_all_simulations.csv",
                 stringsAsFactors = FALSE)
cat("parameter rows:", nrow(pars), "| sims:", length(unique(pars$sim_id)),
    "| species:", length(unique(pars$species)), "\n")

# one row per sim: 19 species x 3 parameters, in a fixed species order
sp_order <- sort(unique(pars$species))
wide <- pars %>%
  mutate(species = factor(species, levels = sp_order)) %>%
  arrange(sim_id, species) %>%
  group_by(sim_id) %>%
  summarise(sig = paste(c(sprintf("%.17g", gamma),
                          sprintf("%.17g", catchability),
                          sprintf("%.17g", abundance_scaling)), collapse = "|"),
            .groups = "drop")
cat("sims with a full parameter signature:", nrow(wide), "\n\n")

dup_groups <- wide %>% group_by(sig) %>%
  summarise(n = n(), sims = paste(sort(sim_id), collapse = ","),
            first_sim = min(sim_id), .groups = "drop") %>%
  filter(n > 1) %>% arrange(desc(n), first_sim)

n_total <- nrow(wide)
n_unique <- n_distinct(wide$sig)
n_redundant <- n_total - n_unique

cat("=== EXACT duplicate parameter draws ===\n")
cat("  total sims           :", n_total, "\n")
cat("  distinct draws       :", n_unique, "\n")
cat("  redundant copies     :", n_redundant, "\n")
cat("  duplicate groups     :", nrow(dup_groups), "\n")
if (nrow(dup_groups)) {
  cat("  members involved     :", sum(dup_groups$n), "\n")
  cat("  largest group size   :", max(dup_groups$n), "\n\n")
  cat("  10 largest duplicate groups:\n")
  print(as.data.frame(dup_groups %>% select(n, sims) %>% head(10)),
        row.names = FALSE)
}

write.csv(dup_groups %>% select(n, first_sim, sims),
          file.path(out_dir, "31_dedupe_groups.csv"), row.names = FALSE)

# --- 2. cross-check against the RMSE-table evidence ---------------------------
cat("\n=== cross-check against identical fit statistics ===\n")
rmse <- read.csv("Manuscript data/yield_rmse_per_sim.csv", stringsAsFactors = FALSE)
cat("rmse rows:", nrow(rmse), "\n")

rmse_dup <- rmse %>% group_by(rmse, cor_raw, cor_log) %>%
  summarise(n = n(), sims = paste(sort(sim_index), collapse = ","), .groups = "drop") %>%
  filter(n > 1)
cat("groups sharing identical rmse+cor_raw+cor_log:", nrow(rmse_dup),
    "covering", sum(rmse_dup$n), "sims\n")

# do the two agree? map each sim to its parameter-signature group id
sig_id <- setNames(match(wide$sig, unique(wide$sig)), wide$sim_id)
rmse$param_group <- sig_id[as.character(rmse$sim_index)]
agree <- rmse %>% filter(!is.na(param_group)) %>%
  group_by(rmse, cor_raw, cor_log) %>%
  summarise(n = n(), n_param_groups = n_distinct(param_group), .groups = "drop") %>%
  filter(n > 1)
cat("of the", nrow(agree), "identical-fit groups,",
    sum(agree$n_param_groups == 1),
    "are also a single parameter draw\n")
if (any(agree$n_param_groups > 1))
  cat("  NOTE:", sum(agree$n_param_groups > 1),
      "group(s) share fit statistics but NOT parameters -- coincidence, not duplication\n")

# --- 3. deduplicated ranking and the new top 10% ------------------------------
cat("\n=== deduplicated ranking ===\n")
rmse_ok <- rmse %>% filter(!is.na(param_group))
cat("rmse rows with a parameter signature:", nrow(rmse_ok),
    "(", nrow(rmse) - nrow(rmse_ok), "without )\n")

# keep the best-ranked representative of each distinct parameter draw
dedup <- rmse_ok %>% arrange(rank) %>% distinct(param_group, .keep_all = TRUE) %>%
  mutate(rank_dedup = row_number())

n_dedup <- nrow(dedup)
n_top10_old <- ceiling(nrow(rmse) * 0.10)
n_top10_new <- ceiling(n_dedup * 0.10)
cat("  distinct members         :", n_dedup, "\n")
cat("  old top 10%              :", n_top10_old, "of", nrow(rmse), "\n")
cat("  NEW top 10%              :", n_top10_new, "of", n_dedup, "\n")

old_top <- rmse %>% arrange(rank) %>% slice_head(n = n_top10_old) %>% pull(sim_index)
new_top <- dedup %>% slice_head(n = n_top10_new) %>% pull(sim_index)
cat("  old top-10% distinct draws:", n_distinct(sig_id[as.character(old_top)]), "\n")
cat("  retained from old top 10% :", sum(new_top %in% old_top), "of", n_top10_new, "\n")
cat("  promoted from outside     :", sum(!new_top %in% old_top), "\n")
if (any(!new_top %in% old_top)) {
  promoted <- dedup %>% filter(sim_index %in% new_top[!new_top %in% old_top])
  cat("  promoted members (sim_index / original rank / rmse):\n")
  print(as.data.frame(promoted %>% select(sim_index, rank, rmse) %>%
                        arrange(rank)), row.names = FALSE)
}

write.csv(dedup, file.path(out_dir, "31_dedupe_unique_ranking.csv"), row.names = FALSE)
write.csv(dedup %>% select(-param_group),
          "Manuscript data/yield_rmse_per_sim_deduped.csv", row.names = FALSE)

cat("\n=== duplication by position in the ranking ===\n")
brk <- c(0, 50, 100, 212, 500, 1000, nrow(rmse))
rmse_r <- rmse %>% arrange(rank) %>% mutate(band = cut(rank, brk, include.lowest = TRUE))
bandtab <- rmse_r %>% filter(!is.na(param_group)) %>% group_by(band) %>%
  summarise(n = n(), distinct_draws = n_distinct(param_group),
            redundant = n - distinct_draws,
            pct_redundant = 100 * (n - distinct_draws) / n, .groups = "drop")
print(as.data.frame(bandtab), digits = 3, row.names = FALSE)

cat("\nWrote:\n  ", file.path(out_dir, "31_dedupe_groups.csv"),
    "\n  ", file.path(out_dir, "31_dedupe_unique_ranking.csv"),
    "\n   Manuscript data/yield_rmse_per_sim_deduped.csv\n")
cat("\nNEXT: Stage D runs the paired stability test on the", n_top10_new,
    "unique top-10% members.\n")
