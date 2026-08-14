# Would the treated RMSE values still fall inside the top 10%? And are members
# 4 and 5 (sim_index 2082 / 2085) actually duplicate parameter draws?
suppressPackageStartupMessages(library(dplyr))

rmse_all <- read.csv("Manuscript data/yield_rmse_per_sim.csv") %>% arrange(rank)
cat("=== RMSE distribution over the 2111-member ensemble ===\n")
for (k in c(1, 5, 10, 50, 100, 200, 212, 213, 250, 300)) {
  cat(sprintf("  rank %4d : sim_index %4d  rmse %.6f\n", k,
              rmse_all$sim_index[k], rmse_all$rmse[k]))
}
cutoff <- rmse_all$rmse[212]
cat("\ntop-10%% cutoff (rank 212) rmse =", sprintf("%.6f", cutoff), "\n\n")

tr <- read.csv("Output_large_files/wmin_test/23_yield_rmse.csv")
tr$rank_if_treated <- sapply(tr$treated, function(x) sum(rmse_all$rmse < x) + 1)
tr$still_top10 <- tr$treated <= cutoff
cat("=== treated RMSE placed back into the original ranking ===\n")
print(tr, digits = 6, row.names = FALSE)
cat("\n(rank_if_treated assumes the other 2110 members keep their stored RMSE,",
    "\n which they would not under a full re-run -- it is an indicative check only)\n")

cat("\n=== are members 4 and 5 duplicates? ===\n")
d <- rmse_all %>% filter(sim_index %in% c(2082, 2085))
print(d, row.names = FALSE)
dup <- rmse_all %>% group_by(rmse, cor_raw, cor_log) %>%
  summarise(n = n(), sims = paste(sim_index, collapse = ","), .groups = "drop") %>%
  filter(n > 1) %>% arrange(desc(n))
cat("\nrows of the RMSE table sharing identical rmse AND both correlations:",
    nrow(dup), "groups covering", sum(dup$n), "sims\n")
print(head(as.data.frame(dup), 10), row.names = FALSE)
