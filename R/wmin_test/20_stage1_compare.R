# =============================================================================
# Stage 1 comparison: corrected w_min (treated) vs uncorrected (control), for
# the top 5 members by yield RMSE, both scenarios.
#
# Three series are carried for every metric:
#   stored   the existing ensemble member (the manuscript's current numbers)
#   control  re-run through the fresh pipeline WITHOUT the w_min change
#   treated  re-run through the same pipeline WITH w_min = 3626.667 g
#
# treated - control isolates the w_min effect; control - stored shows what the
# spin-up restart alone costs, so the two are never conflated.
#
# Slope uses the canonical LBNbiom estimator (Edwards et al. 2017 Method 5),
# ecosystem_assessment_v3.R:493, with spectrum_min_w = 3.16227766e-08.
# All comparisons are filtered to <= 2010 (the unexploited arm runs to 2011).
#
# Writes 20_stage1_comparison.rds / .csv
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer)
  library(mizer)
  library(dplyr)
})

out_dir <- "Output_large_files/wmin_test"
SPECTRUM_MIN_W <- 3.16227766e-08
PEN <- "small divers"
KRILL <- "antarctic krill"
FISHES <- c("mesopelagic fishes", "bathypelagic fishes",
            "shelf and coastal fishes", "toothfishes")

res <- readRDS(file.path(out_dir, "19_stage1_sims.rds"))

# --- canonical LBNbiom slope, per year --------------------------------------
lbnbiom_slope_series <- function(sim, min_w = SPECTRUM_MIN_W) {
  p <- sim@params; w <- p@w; dw <- p@dw
  times <- as.numeric(dimnames(sim@n)$time)
  w_filt <- w[w >= min_w]
  bin_breaks <- 2^(floor(log2(min(w_filt))):ceiling(log2(max(w_filt))))
  n_bins <- length(bin_breaks) - 1
  bin_idx <- findInterval(w, bin_breaks, rightmost.closed = TRUE)
  oct_width <- diff(bin_breaks)
  oct_mid <- sqrt(bin_breaks[-length(bin_breaks)] * bin_breaks[-1])
  keep <- w >= min_w
  vapply(seq_along(times), function(ti) {
    bpb <- colSums(sim@n[ti, , ]) * w * dw
    ob <- numeric(n_bins)
    for (j in which(keep)) {
      b <- bin_idx[j]
      if (b >= 1 && b <= n_bins) ob[b] <- ob[b] + bpb[j]
    }
    nbs <- ob / oct_width
    v <- which(nbs > 0)
    if (length(v) < 3) return(NA_real_)
    unname(coef(lm(log10(nbs[v]) ~ log10(oct_mid[v])))[2])
  }, numeric(1))
}

# --- krill consumption, population level, per year ---------------------------
krill_consumption <- function(sim, predators, years) {
  p <- sim@params; dw <- p@dw
  sapply(years, function(y) {
    ti <- which(as.numeric(dimnames(sim@n)$time) == y)
    n <- sim@n[ti, , ]
    diet <- getDiet(p, n = n, n_pp = sim@n_pp[ti, ],
                    n_other = p@initial_n_other, proportion = FALSE)
    d <- diet[predators, , KRILL, drop = FALSE]   # np x nw x 1
    dim(d) <- c(length(predators), length(dw))
    sum(d * n[predators, , drop = FALSE] * rep(dw, each = length(predators)))
  })
}

series_for <- function(sim, arm, cond, member, sim_index) {
  p <- sim@params
  times <- as.numeric(dimnames(sim@n)$time)
  keep <- times <= 2010
  wdw <- p@w * p@dw
  bm_sp <- t(apply(sim@n, 1, function(x) rowSums(sweep(x, 2, wdw, "*"))))
  data.frame(
    member = member, sim_index = sim_index, arm = arm, cond = cond,
    year = times[keep],
    total_biomass = rowSums(bm_sp)[keep],
    pen_biomass   = bm_sp[keep, PEN],
    pen_number    = apply(sim@n[keep, PEN, ], 1, function(x) sum(x * p@dw)),
    lambda        = lbnbiom_slope_series(sim)[keep]
  )
}

cat("=== building series ===\n")
rows <- list()
for (r in res) {
  for (arm in c("fished", "climate")) {
    rows[[length(rows) + 1]] <- series_for(r$control[[arm]], arm, "control",
                                           r$member, r$sim_index)
    rows[[length(rows) + 1]] <- series_for(r$treated[[arm]], arm, "treated",
                                           r$member, r$sim_index)
    stored <- if (arm == "fished") r$stored_fished else r$stored_climate
    rows[[length(rows) + 1]] <- series_for(stored, arm, "stored",
                                           r$member, r$sim_index)
  }
  cat("  member", r$member, "done\n")
}
series <- bind_rows(rows)
saveRDS(series, file.path(out_dir, "20_stage1_series.rds"))

# --- krill consumption at the 2001-2010 reference period ---------------------
cat("\n=== krill consumption (2001-2010 mean) ===\n")
krows <- list()
for (r in res) {
  for (arm in c("fished", "climate")) {
    for (cond in c("control", "treated", "stored")) {
      sim <- switch(cond, control = r$control[[arm]], treated = r$treated[[arm]],
                    stored = if (arm == "fished") r$stored_fished else r$stored_climate)
      krows[[length(krows) + 1]] <- data.frame(
        member = r$member, arm = arm, cond = cond,
        baleen_krill = mean(krill_consumption(sim, "baleen whales", 2001:2010)),
        fish_krill   = mean(krill_consumption(sim, FISHES, 2001:2010)),
        pen_krill    = mean(krill_consumption(sim, PEN, 2001:2010)))
    }
  }
  cat("  member", r$member, "done\n")
}
krill <- bind_rows(krows)
saveRDS(krill, file.path(out_dir, "20_stage1_krill.rds"))

# --- paired differences ------------------------------------------------------
pct <- function(a, b) 100 * (a - b) / b
wide <- series %>%
  filter(year == 2010) %>%
  select(member, arm, cond, total_biomass, pen_biomass, pen_number, lambda) %>%
  tidyr::pivot_wider(names_from = cond,
                     values_from = c(total_biomass, pen_biomass, pen_number, lambda))

cat("\n\n================ TERMINAL YEAR 2010 ================\n")
for (a in c("fished", "climate")) {
  d <- wide[wide$arm == a, ]
  cat("\n--- ", a, " ---\n", sep = "")
  cat(sprintf("  control vs stored : total biomass %+.4f%%  lambda %+.6f\n",
              median(pct(d$total_biomass_control, d$total_biomass_stored)),
              median(d$lambda_control - d$lambda_stored)))
  cat(sprintf("  TREATED vs control: total biomass %+.4f%%  lambda %+.6f\n",
              median(pct(d$total_biomass_treated, d$total_biomass_control)),
              median(d$lambda_treated - d$lambda_control)))
  cat(sprintf("  TREATED vs control: penguin biomass %+.2f%%  penguin numbers %+.2f%%\n",
              median(pct(d$pen_biomass_treated, d$pen_biomass_control)),
              median(pct(d$pen_number_treated, d$pen_number_control))))
  cat("  per-member lambda (treated - control): ",
      paste(sprintf("%+.6f", d$lambda_treated - d$lambda_control), collapse = "  "), "\n")
  cat("  per-member total biomass %% (treated - control): ",
      paste(sprintf("%+.4f", pct(d$total_biomass_treated, d$total_biomass_control)),
            collapse = "  "), "\n")
}

cat("\n================ KRILL CONSUMPTION (2001-2010) ================\n")
kw <- krill %>% tidyr::pivot_wider(names_from = cond,
                                   values_from = c(baleen_krill, fish_krill, pen_krill))
for (a in c("fished", "climate")) {
  d <- kw[kw$arm == a, ]
  cat("\n--- ", a, " ---\n", sep = "")
  cat(sprintf("  baleen krill  treated vs control: %+.3f%%  (control vs stored %+.4f%%)\n",
              median(pct(d$baleen_krill_treated, d$baleen_krill_control)),
              median(pct(d$baleen_krill_control, d$baleen_krill_stored))))
  cat(sprintf("  fish   krill  treated vs control: %+.3f%%  (control vs stored %+.4f%%)\n",
              median(pct(d$fish_krill_treated, d$fish_krill_control)),
              median(pct(d$fish_krill_control, d$fish_krill_stored))))
  cat(sprintf("  penguin krill treated vs control: %+.2f%%\n",
              median(pct(d$pen_krill_treated, d$pen_krill_control))))
}

# --- paired exploited-vs-unexploited statistic, the manuscript headline ------
cat("\n================ PAIRED EXPLOITED - UNEXPLOITED, 2010 ================\n")
for (cond in c("stored", "control", "treated")) {
  d <- series %>% filter(year == 2010, cond == !!cond) %>%
    select(member, arm, lambda, total_biomass) %>%
    tidyr::pivot_wider(names_from = arm, values_from = c(lambda, total_biomass))
  cat(sprintf("  %-8s lambda diff median %+.6f  (n negative %d/%d) | biomass diff %+.3f%%\n",
              cond, median(d$lambda_fished - d$lambda_climate),
              sum(d$lambda_fished - d$lambda_climate < 0), nrow(d),
              median(pct(d$total_biomass_fished, d$total_biomass_climate))))
}

# --- stability summary -------------------------------------------------------
cat("\n================ SPIN-UP STABILITY ================\n")
for (r in res)
  cat(sprintf("  member %d (sim %4d): control stable=%-5s max_cv=%.3f | treated stable=%-5s max_cv=%.3f  fail=[%s]\n",
              r$member, r$sim_index, r$control$stab$stable, r$control$stab$max_cv,
              r$treated$stab$stable, r$treated$stab$max_cv,
              paste(unique(c(r$treated$stab$fail_cv, r$treated$stab$fail_trend)),
                    collapse = ",")))

write.csv(series, file.path(out_dir, "20_stage1_series.csv"), row.names = FALSE)
write.csv(krill, file.path(out_dir, "20_stage1_krill.csv"), row.names = FALSE)
cat("\nSaved comparison outputs to", out_dir, "\n")
