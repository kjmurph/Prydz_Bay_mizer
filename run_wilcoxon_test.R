suppressPackageStartupMessages(library(dplyr))

CACHED_DIR   <- "whale_consumption_outputs"
MODERN_YEARS <- 2001:2010
G_TO_TONNES  <- 1e-6

period_means <- function(rds_path, group) {
  sim_list <- readRDS(rds_path)
  rows <- lapply(seq_along(sim_list), function(i) {
    df  <- sim_list[[i]]
    sub <- df[df$year %in% MODERN_YEARS, ]
    data.frame(sim_id      = i,
               mean_tonnes = mean(sub$total_consumption, na.rm = TRUE) * G_TO_TONNES,
               group       = group)
  })
  do.call(rbind, rows)
}

# Use the exact same source files as the stage1 plot figures
bf <- period_means(file.path(CACHED_DIR, "fishing_baleen_ltl_all_sims.rds"),      "baleen whales")
bc <- period_means(file.path(CACHED_DIR, "climate_only_baleen_ltl_all_sims.rds"), "baleen whales")
sf <- period_means(file.path(CACHED_DIR, "fishing_seals_ltl_all_sims.rds"),       "seals")
sc <- period_means(file.path(CACHED_DIR, "climate_only_seals_ltl_all_sims.rds"),  "seals")

agg_f  <- bind_rows(bf, sf)
agg_cl <- bind_rows(bc, sc)

joined <- inner_join(agg_f, agg_cl, by = c("group", "sim_id"), suffix = c("_f", "_c")) %>%
  mutate(
    log_ratio = log(mean_tonnes_f / mean_tonnes_c),
    pct       = mean_tonnes_f / mean_tonnes_c * 100
  )

for (grp in c("baleen whales", "seals")) {
  d  <- filter(joined, group == grp, is.finite(log_ratio))
  wt <- wilcox.test(d[["log_ratio"]], mu = 0, conf.int = TRUE, conf.level = 0.95)
  hl <- exp(wt[["estimate"]])
  ci <- exp(wt[["conf.int"]])
  cat(sprintf("\n=== %s (n = %d) ===\n", grp, nrow(d)))
  cat(sprintf("  Median (t/yr):    %.0f\n", median(d[["mean_tonnes_f"]])))
  cat(sprintf("  Median %%:         %.1f%%  IQR [%.1f%% - %.1f%%]\n",
              median(d[["pct"]]), quantile(d[["pct"]], 0.25), quantile(d[["pct"]], 0.75)))
  cat(sprintf("  5th-95th %%ile:    %.1f%% - %.1f%%\n",
              quantile(d[["pct"]], 0.05), quantile(d[["pct"]], 0.95)))
  cat(sprintf("  Wilcoxon V:       %.0f\n", wt[["statistic"]]))
  cat(sprintf("  p-value:          %s\n", format.pval(wt[["p.value"]], digits = 3, eps = 1e-300)))
  cat(sprintf("  HL ratio:         %.4f  (95%% CI: %.4f - %.4f)\n", hl, ci[1], ci[2]))
}
