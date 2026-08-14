# =============================================================================
# Threshold sensitivity.
#
# Section 2 of the brief flags an unresolved question: is the target w_min the
# historical 3626.667 g (= 0.85 x the OLD w_mat of 4266.667 g) or a biologically
# derived egg/hatchling mass, nearer 100 g for an Adelie penguin? The test
# design is the same either way, but the number changes what is at stake.
#
# This sweeps the threshold so the Stage 0 answer is available for whichever
# value is chosen, rather than having to re-run once the question is settled.
#
# Writes 07_threshold_sweep.rds / .csv
# =============================================================================

source("R/wmin_test/thermizer_shim.R")
suppressPackageStartupMessages(library(dplyr))

out_dir <- "Output_large_files/wmin_test"
PENGUIN <- "small divers"
KRILL   <- "antarctic krill"
FISH_PREY <- c("mesopelagic fishes", "bathypelagic fishes",
               "shelf and coastal fishes", "toothfishes")
THRESHOLDS <- c(1, 10, 100, 500, 1000, 2000, 3626.667)
REF_YEARS  <- 2001:2010

t0 <- proc.time()
cat("=== threshold sweep:", format(Sys.time()), "===\n")
cat("thresholds (g):", paste(THRESHOLDS, collapse = ", "), "\n\n")

ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
p0 <- ens$fished_top10pct[[1]]@params
w  <- p0@w
dw <- p0@dw
wt_list <- lapply(THRESHOLDS, function(thr) pmin(1, pmax(0, (thr - w) / dw)))
names(wt_list) <- as.character(THRESHOLDS)

# --- size structure: all 212 members, reference period, both arms ------------
cat("[1] size structure, all members ...\n")
size_rows <- list()
for (arm in c("Exploited", "Unexploited")) {
  sims <- if (arm == "Exploited") ens$fished_top10pct else ens$climate_top10pct
  for (m in seq_along(sims)) {
    sim <- sims[[m]]
    times <- as.numeric(dimnames(sim@n)$time)
    ti <- which(times %in% REF_YEARS)
    npen <- sim@n[ti, PENGUIN, , drop = FALSE]
    npen <- apply(npen, c(1, 3), identity)          # year x w
    num_bin  <- sweep(npen, 2, dw, "*")
    biom_bin <- sweep(npen, 2, w * dw, "*")
    for (thr in names(wt_list)) {
      wt <- wt_list[[thr]]
      size_rows[[length(size_rows) + 1]] <- data.frame(
        arm = arm, member = m, threshold = as.numeric(thr),
        f_number_sub  = mean(as.numeric(num_bin  %*% wt) / rowSums(num_bin)),
        f_biomass_sub = mean(as.numeric(biom_bin %*% wt) / rowSums(biom_bin))
      )
    }
  }
}
size_sweep <- bind_rows(size_rows)

# --- consumption + mortality: 30-member subsample, year 2005 ----------------
set.seed(20260728)
sub_members <- sort(sample(seq_along(ens$fished_top10pct), 30))
cat("[2] consumption/mortality, 30-member subsample at 2005 ...\n")
cons_rows <- list()
for (arm in c("Exploited", "Unexploited")) {
  sims <- if (arm == "Exploited") ens$fished_top10pct else ens$climate_top10pct
  for (m in sub_members) {
    sim <- sims[[m]]
    p <- sim@params
    st <- sim_state_at(sim, 2005)
    te <- ther_temp_effect(p, 2005)
    fl <- ther_feeding_level(p, st$n, st$n_pp, st$n_other, 2005, temp_eff = te)
    diet <- ther_diet(p, st$n, st$n_pp, st$n_other, 2005, temp_eff = te,
                      feeding_level = fl)
    pm <- ther_pred_mort(p, st$n, st$n_pp, st$n_other, 2005, temp_eff = te,
                         feeding_level = fl)
    n <- st$n
    npen <- n[PENGUIN, ]
    ck <- diet[PENGUIN, , KRILL] * npen * dw
    cf <- rowSums(diet[PENGUIN, , FISH_PREY, drop = FALSE]) * npen * dw
    mn <- pm[PENGUIN, ] * npen * dw
    mb <- pm[PENGUIN, ] * npen * w * dw
    krill_all <- sum(diet[, , KRILL] * n * rep(dw, each = nrow(n)))
    for (thr in names(wt_list)) {
      wt <- wt_list[[thr]]
      cons_rows[[length(cons_rows) + 1]] <- data.frame(
        arm = arm, member = m, threshold = as.numeric(thr),
        f_cons_krill_sub = sum(ck * wt) / sum(ck),
        f_cons_fish_sub  = sum(cf * wt) / sum(cf),
        subpen_share_krill = sum(ck * wt) / krill_all,
        f_mort_number_sub  = sum(mn * wt) / sum(mn),
        f_mort_biomass_sub = sum(mb * wt) / sum(mb)
      )
    }
  }
}
cons_sweep <- bind_rows(cons_rows)

sweep_out <- list(size = size_sweep, cons = cons_sweep,
                  sub_members = sub_members, thresholds = THRESHOLDS)
saveRDS(sweep_out, file.path(out_dir, "07_threshold_sweep.rds"))

# --- report ------------------------------------------------------------------
s <- size_sweep %>% group_by(arm, threshold) %>%
  summarise(f_number_sub = median(f_number_sub),
            f_biomass_sub = median(f_biomass_sub), .groups = "drop")
c_ <- cons_sweep %>% group_by(arm, threshold) %>%
  summarise(across(c(f_cons_krill_sub, f_cons_fish_sub, subpen_share_krill,
                     f_mort_number_sub, f_mort_biomass_sub), median),
            .groups = "drop")
tab <- left_join(s, c_, by = c("arm", "threshold"))
write.csv(tab, file.path(out_dir, "07_threshold_sweep.csv"), row.names = FALSE)

cat("\n=== median over members, by threshold ===\n")
cat("(size structure: 212 members, 2001-2010 mean; ",
    "consumption/mortality: 30 members, 2005)\n\n", sep = "")
for (a in unique(tab$arm)) {
  cat("--- ", a, " ---\n", sep = "")
  d <- as.data.frame(tab[tab$arm == a, -1])
  d[, -1] <- lapply(d[, -1], function(x) signif(100 * x, 3))
  names(d)[-1] <- paste0(names(d)[-1], "_%")
  print(d, row.names = FALSE)
  cat("\n")
}
cat("elapsed:", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
