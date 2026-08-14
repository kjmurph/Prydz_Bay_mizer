# =============================================================================
# Stage 0 robustness, two questions the reference-period snapshot cannot answer:
#
#  (1) Is 2001-2010 representative? The size-structure shares need no diet
#      calculation, so they are computed for ALL 170 years x 212 members x both
#      arms. The consumption and mortality shares are expensive (two FFTs per
#      member-year), so they are computed for a reproducible 30-member subsample
#      on a decadal year grid.
#
#  (2) Who actually dominates community krill consumption? The Stage 0 headline
#      "penguins are 0.3% of community krill consumption" is only interpretable
#      alongside the full predator breakdown.
#
# Writes 06_stage0_size_structure_all_years.rds, 06_stage0_consumption_subsample.rds,
#        06_krill_predator_breakdown.csv
# =============================================================================

source("R/wmin_test/thermizer_shim.R")
suppressPackageStartupMessages(library(dplyr))

out_dir <- "Output_large_files/wmin_test"
W_THR   <- 3626.667
PENGUIN <- "small divers"
KRILL   <- "antarctic krill"
FISH_PREY <- c("mesopelagic fishes", "bathypelagic fishes",
               "shelf and coastal fishes", "toothfishes")

t0 <- proc.time()
cat("=== Stage 0 robustness:", format(Sys.time()), "===\n")

ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
idx <- ens$metadata$top10pct_indices
arms <- list(Exploited = ens$fished_top10pct, Unexploited = ens$climate_top10pct)

p0 <- ens$fished_top10pct[[1]]@params
w  <- p0@w
dw <- p0@dw
frac_below <- pmin(1, pmax(0, (W_THR - w) / dw))

# --- (1a) size-structure shares, every year, every member --------------------
cat("\n[1a] size-structure shares over all years ...\n")
size_rows <- list()
for (arm in names(arms)) {
  sims <- arms[[arm]]
  for (m in seq_along(sims)) {
    sim <- sims[[m]]
    times <- as.numeric(dimnames(sim@n)$time)
    npen <- sim@n[, PENGUIN, ]                    # time x w
    num_bin  <- sweep(npen, 2, dw, "*")
    biom_bin <- sweep(npen, 2, w * dw, "*")
    size_rows[[length(size_rows) + 1]] <- data.frame(
      arm = arm, member = m, sim_index = idx[m], year = times,
      f_number_sub  = as.numeric(num_bin  %*% frac_below) / rowSums(num_bin),
      f_biomass_sub = as.numeric(biom_bin %*% frac_below) / rowSums(biom_bin)
    )
  }
  cat("  ", arm, "done, elapsed", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
}
size_all <- bind_rows(size_rows)
saveRDS(size_all, file.path(out_dir, "06_stage0_size_structure_all_years.rds"))

cat("\n--- sub-threshold share by era (median over members) ---\n")
era <- size_all %>%
  filter(year <= 2010) %>%
  mutate(era = cut(year, c(1840, 1900, 1930, 1960, 1990, 2010),
                   labels = c("1841-1900", "1901-1930", "1931-1960",
                              "1961-1990", "1991-2010"))) %>%
  group_by(arm, era) %>%
  summarise(f_number_sub = median(f_number_sub),
            f_biomass_sub = median(f_biomass_sub), .groups = "drop")
print(as.data.frame(era), digits = 4)

cat("\n--- extremes over the whole record (any member, any year <= 2010) ---\n")
ext <- size_all %>% filter(year <= 2010) %>% group_by(arm) %>%
  summarise(max_f_number = max(f_number_sub), max_f_biomass = max(f_biomass_sub),
            .groups = "drop")
print(as.data.frame(ext), digits = 4)

# --- (1b) consumption / mortality shares, subsample --------------------------
set.seed(20260728)
sub_members <- sort(sample(seq_along(ens$fished_top10pct), 30))
sub_years   <- seq(1850, 2010, by = 10)
cat("\n[1b] consumption subsample: 30 members (seed 20260728), years",
    min(sub_years), "-", max(sub_years), "by 10\n")
cat("  members:", paste(sub_members, collapse = " "), "\n")

cons_rows <- list()
for (arm in names(arms)) {
  sims <- arms[[arm]]
  for (m in sub_members) {
    sim <- sims[[m]]
    p <- sim@params
    for (y in sub_years) {
      st <- sim_state_at(sim, y)
      te <- ther_temp_effect(p, y)
      fl <- ther_feeding_level(p, st$n, st$n_pp, st$n_other, y, temp_eff = te)
      diet <- ther_diet(p, st$n, st$n_pp, st$n_other, y, temp_eff = te,
                        feeding_level = fl)
      pm <- ther_pred_mort(p, st$n, st$n_pp, st$n_other, y, temp_eff = te,
                           feeding_level = fl)
      n <- st$n
      npen <- n[PENGUIN, ]
      ck <- diet[PENGUIN, , KRILL] * npen * dw
      cf <- rowSums(diet[PENGUIN, , FISH_PREY, drop = FALSE]) * npen * dw
      cons_all <- function(prey) sum(diet[, , prey] * n * rep(dw, each = nrow(n)))
      mn <- pm[PENGUIN, ] * npen * dw
      cons_rows[[length(cons_rows) + 1]] <- data.frame(
        arm = arm, member = m, year = y,
        f_cons_krill_sub = sum(ck * frac_below) / sum(ck),
        f_cons_fish_sub  = sum(cf * frac_below) / sum(cf),
        pen_share_krill  = sum(ck) / cons_all(KRILL),
        subpen_share_krill = sum(ck * frac_below) / cons_all(KRILL),
        f_mort_number_sub = sum(mn * frac_below) / sum(mn)
      )
    }
  }
  cat("  ", arm, "done, elapsed", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
}
cons_sub <- bind_rows(cons_rows)
saveRDS(cons_sub, file.path(out_dir, "06_stage0_consumption_subsample.rds"))

cat("\n--- consumption shares by era (median over 30 members) ---\n")
cs <- cons_sub %>%
  mutate(era = cut(year, c(1840, 1900, 1930, 1960, 1990, 2010),
                   labels = c("1841-1900", "1901-1930", "1931-1960",
                              "1961-1990", "1991-2010"))) %>%
  group_by(arm, era) %>%
  summarise(across(c(f_cons_krill_sub, pen_share_krill, subpen_share_krill,
                     f_mort_number_sub), median), .groups = "drop")
print(as.data.frame(cs), digits = 4)

cat("\n--- worst case over the whole record ---\n")
print(as.data.frame(cons_sub %>% group_by(arm) %>%
  summarise(max_f_cons_krill_sub = max(f_cons_krill_sub),
            max_subpen_share_krill = max(subpen_share_krill),
            max_f_mort_number_sub = max(f_mort_number_sub), .groups = "drop")),
  digits = 4)

# --- (2) who eats the krill --------------------------------------------------
cat("\n[2] community krill-consumption breakdown by predator",
    "(Unexploited, 2005, median over 30 members)\n")
brk <- lapply(sub_members, function(m) {
  sim <- ens$climate_top10pct[[m]]
  p <- sim@params
  st <- sim_state_at(sim, 2005)
  te <- ther_temp_effect(p, 2005)
  fl <- ther_feeding_level(p, st$n, st$n_pp, st$n_other, 2005, temp_eff = te)
  diet <- ther_diet(p, st$n, st$n_pp, st$n_other, 2005, temp_eff = te,
                    feeding_level = fl)
  v <- rowSums(diet[, , KRILL] * st$n * rep(dw, each = nrow(st$n)))
  v / sum(v)
})
brk <- do.call(rbind, brk)
tab <- data.frame(predator = colnames(brk),
                  median_share_pct = 100 * apply(brk, 2, median)) %>%
  arrange(desc(median_share_pct))
print(tab, row.names = FALSE, digits = 4)
write.csv(tab, file.path(out_dir, "06_krill_predator_breakdown.csv"),
          row.names = FALSE)

cat("\nelapsed:", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
