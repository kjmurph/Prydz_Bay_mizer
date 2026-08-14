# =============================================================================
# Verify that position m in top10pct_rmse_ensembles corresponds to sim_i = m in
# the biomass caches, and to original ensemble index top10pct_indices[m].
#
# The brief warns that joining on the bare number silently mispairs members, so
# this is checked directly rather than assumed: recompute biomass from the sim
# object and match it against Manuscript data/biomass_top10pct_raw_fish.rds.
# Also re-derives the fitted-ensemble membership from yield_rmse_per_sim.csv.
# =============================================================================

suppressPackageStartupMessages({
  library(mizer)
  library(dplyr)
})

ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
idx <- ens$metadata$top10pct_indices
bio <- readRDS("Manuscript data/biomass_top10pct_raw_fish.rds")

cat("biomass cache columns:", paste(names(bio), collapse = ", "), "\n")
cat("sim_i range:", range(bio$sim_i), "  Year range:", range(bio$Year), "\n\n")

# --- re-derive top-10% membership from the RMSE table ------------------------
rmse <- read.csv("Manuscript data/yield_rmse_per_sim.csv")
cat("rmse columns:", paste(names(rmse), collapse = ", "), "\n")
n_top <- ceiling(2111 * 0.10)
derived <- rmse %>% arrange(rank) %>% head(n_top) %>% pull(sim_index)
cat("n_top =", n_top, "\n")
cat("derived membership identical to metadata$top10pct_indices:",
    identical(as.integer(derived), as.integer(idx)), "\n")
cat("  (as sets:", setequal(derived, idx), ")\n")
cat("  first 8 derived :", paste(head(derived, 8), collapse = " "), "\n")
cat("  first 8 metadata:", paste(head(idx, 8), collapse = " "), "\n\n")

# --- biomass check -----------------------------------------------------------
yr <- 2005
check <- lapply(c(1, 7, 50, 212), function(m) {
  sim <- ens$fished_top10pct[[m]]
  p <- sim@params
  ti <- which(as.numeric(dimnames(sim@n)$time) == yr)
  n <- sim@n[ti, , ]
  bm_sim <- rowSums(sweep(n, 2, p@w * p@dw, "*"))

  cached <- bio %>% filter(sim_i == m, Year == yr) %>%
    select(Species, Biomass) %>% arrange(Species)
  common <- intersect(names(bm_sim), cached$Species)
  a <- bm_sim[common]
  b <- cached$Biomass[match(common, cached$Species)]
  data.frame(member = m, orig_index = idx[m], n_species_matched = length(common),
             max_rel_diff = max(abs(a - b) / b))
})
print(do.call(rbind, check), digits = 6)

cat("\nIf max_rel_diff ~ 0, then top10pct member m == biomass cache sim_i m.\n")
