# =============================================================================
# Stage 1 comparison, definitive run (full protocol incl. steady()).
#
# Three series per metric:
#   stored   the existing ensemble member (the manuscript's current numbers)
#   control  re-run through the full fresh pipeline WITHOUT the w_min change
#   treated  same pipeline WITH w_min = 3626.667 g
#
# treated - control isolates the w_min effect; control - stored shows the cost of
# re-running the pipeline at all, so the two are never conflated.
#
# Covers section 5 outputs 1 (biomass), 3 (lambda), 5/6 (krill consumption by
# baleen whales and fishes), 7 (stability rejection) and 8 (yield RMSE), plus
# the two headline paired statistics.
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(dplyr); library(tidyr)
})

out_dir <- "Output_large_files/wmin_test"
SPECTRUM_MIN_W <- 3.16227766e-08
PEN <- "small divers"; KRILL <- "antarctic krill"
FISHES <- c("mesopelagic fishes", "bathypelagic fishes",
            "shelf and coastal fishes", "toothfishes")

res <- readRDS(file.path(out_dir, "22_stage1_steady_sims.rds"))
res <- Filter(function(r) !is.null(r$control) && !is.null(r$treated), res)
cat("members with both conditions:", length(res), "\n\n")

lbnbiom <- function(sim, min_w = SPECTRUM_MIN_W) {
  p <- sim@params; w <- p@w; dw <- p@dw
  wf <- w[w >= min_w]
  br <- 2^(floor(log2(min(wf))):ceiling(log2(max(wf))))
  nb <- length(br) - 1
  bi <- findInterval(w, br, rightmost.closed = TRUE)
  ow <- diff(br); om <- sqrt(br[-length(br)] * br[-1]); keep <- which(w >= min_w)
  vapply(seq_len(dim(sim@n)[1]), function(ti) {
    bpb <- colSums(sim@n[ti, , ]) * w * dw
    ob <- numeric(nb)
    for (j in keep) { b <- bi[j]; if (b >= 1 && b <= nb) ob[b] <- ob[b] + bpb[j] }
    nbs <- ob / ow; v <- which(nbs > 0)
    if (length(v) < 3) return(NA_real_)
    unname(coef(lm(log10(nbs[v]) ~ log10(om[v])))[2])
  }, numeric(1))
}

krill_cons <- function(sim, predators, years) {
  p <- sim@params; dw <- p@dw
  sapply(years, function(y) {
    ti <- which(as.numeric(dimnames(sim@n)$time) == y)
    n <- sim@n[ti, , ]
    d <- getDiet(p, n = n, n_pp = sim@n_pp[ti, ],
                 n_other = p@initial_n_other, proportion = FALSE)[predators, , KRILL,
                                                                  drop = FALSE]
    dim(d) <- c(length(predators), length(dw))
    sum(d * n[predators, , drop = FALSE] * rep(dw, each = length(predators)))
  })
}

series_for <- function(sim, arm, cond, member, sim_index) {
  p <- sim@params; times <- as.numeric(dimnames(sim@n)$time); keep <- times <= 2010
  wdw <- p@w * p@dw
  bm <- t(apply(sim@n, 1, function(x) rowSums(sweep(x, 2, wdw, "*"))))
  data.frame(member = member, sim_index = sim_index, arm = arm, cond = cond,
             year = times[keep],
             total_biomass = rowSums(bm)[keep],
             pen_biomass = bm[keep, PEN],
             pen_number = apply(sim@n[keep, PEN, ], 1, function(x) sum(x * p@dw)),
             lambda = lbnbiom(sim)[keep])
}

get_sim <- function(r, cond, arm) switch(cond,
  control = r$control[[arm]], treated = r$treated[[arm]],
  stored = if (arm == "fished") r$stored_fished else r$stored_climate)

cat("=== series ===\n")
rows <- list(); krows <- list()
for (r in res) {
  for (arm in c("fished", "climate")) for (cond in c("stored", "control", "treated")) {
    s <- get_sim(r, cond, arm)
    rows[[length(rows) + 1]] <- series_for(s, arm, cond, r$member, r$sim_index)
    krows[[length(krows) + 1]] <- data.frame(
      member = r$member, arm = arm, cond = cond,
      baleen_krill = mean(krill_cons(s, "baleen whales", 2001:2010)),
      fish_krill = mean(krill_cons(s, FISHES, 2001:2010)),
      pen_krill = mean(krill_cons(s, PEN, 2001:2010)))
  }
  cat("  member", r$member, "done\n")
}
series <- bind_rows(rows); krill <- bind_rows(krows)
saveRDS(series, file.path(out_dir, "23_series.rds"))
saveRDS(krill, file.path(out_dir, "23_krill.rds"))

# --- yield RMSE, exactly as yield_rmse_evaluation.R computes it --------------
cat("\n=== yield RMSE ===\n")
effort_arr <- readRDS("effort_array_1841_2010.rds")
ew <- do.call(rbind, lapply(colnames(effort_arr), function(sp) {
  yrs <- as.numeric(rownames(effort_arr))[effort_arr[, sp] > 0]
  if (length(yrs)) data.frame(Species = sp, first_year = min(yrs),
                              last_year = max(yrs), stringsAsFactors = FALSE)
}))
obs_long <- read.csv("yield_observed_timeseries.csv") %>%
  reshape2::melt(id.vars = "Year", variable.name = "Species", value.name = "Yield_g") %>%
  mutate(Species = gsub("\\.", " ", as.character(Species)),
         Yield_g = pmax(coalesce(as.numeric(Yield_g), 0), 0)) %>%
  left_join(ew, by = "Species") %>%
  filter(!is.na(first_year), Year >= first_year, Year <= last_year) %>%
  select(Year, Species, Yield_obs = Yield_g)

rmse_of <- function(sim) {
  # Stored sims were written by mizer 2.5.0 and have no `second_order_w` slot,
  # which getYield() dereferences. Upgrade a copy; verified benign in
  # 17_validate_rerun.R, where an upgraded re-projection reproduced the stored
  # trajectory to ~3e-15.
  if (!tryCatch({ sim@params@second_order_w; TRUE }, error = function(e) FALSE))
    sim@params <- suppressWarnings(validParams(sim@params))
  df <- reshape2::melt(getYield(sim))
  names(df) <- c("Year", "Species", "Yield_mod")
  df$Year <- as.numeric(as.character(df$Year)); df$Species <- as.character(df$Species)
  df$Yield_mod <- pmax(df$Yield_mod, 0)
  comp <- obs_long %>% left_join(df, by = c("Year", "Species")) %>%
    mutate(Yield_mod = coalesce(Yield_mod, 0))
  sqrt(mean((log10(comp$Yield_mod + 1) - log10(comp$Yield_obs + 1))^2, na.rm = TRUE))
}
rmse_tab <- bind_rows(lapply(res, function(r) data.frame(
  member = r$member, sim_index = r$sim_index,
  stored = rmse_of(r$stored_fished), control = rmse_of(r$control$fished),
  treated = rmse_of(r$treated$fished))))
print(rmse_tab, digits = 6, row.names = FALSE)
write.csv(rmse_tab, file.path(out_dir, "23_yield_rmse.csv"), row.names = FALSE)

# --- report -----------------------------------------------------------------
pct <- function(a, b) 100 * (a - b) / b
w2010 <- series %>% filter(year == 2010) %>%
  select(member, arm, cond, total_biomass, pen_biomass, pen_number, lambda) %>%
  pivot_wider(names_from = cond,
              values_from = c(total_biomass, pen_biomass, pen_number, lambda))

cat("\n\n================ TERMINAL YEAR 2010 ================\n")
for (a in c("fished", "climate")) {
  d <- w2010[w2010$arm == a, ]
  cat("\n--- ", a, " ---\n", sep = "")
  cat(sprintf("  control vs stored : biomass %+8.4f%%   lambda %+.6f\n",
              median(pct(d$total_biomass_control, d$total_biomass_stored)),
              median(d$lambda_control - d$lambda_stored)))
  cat(sprintf("  TREATED vs control: biomass %+8.4f%%   lambda %+.6f\n",
              median(pct(d$total_biomass_treated, d$total_biomass_control)),
              median(d$lambda_treated - d$lambda_control)))
  cat(sprintf("  TREATED vs control: penguin biomass %+.2f%%  numbers %+.2f%%\n",
              median(pct(d$pen_biomass_treated, d$pen_biomass_control)),
              median(pct(d$pen_number_treated, d$pen_number_control))))
  cat("    per-member lambda diff:",
      paste(sprintf("%+.6f", d$lambda_treated - d$lambda_control), collapse = " "), "\n")
  cat("    per-member biomass %:",
      paste(sprintf("%+.4f", pct(d$total_biomass_treated, d$total_biomass_control)),
            collapse = " "), "\n")
}

cat("\n================ KRILL CONSUMPTION (2001-2010 mean) ================\n")
kw <- krill %>% pivot_wider(names_from = cond,
                            values_from = c(baleen_krill, fish_krill, pen_krill))
for (a in c("fished", "climate")) {
  d <- kw[kw$arm == a, ]
  cat("\n--- ", a, " ---\n", sep = "")
  cat(sprintf("  baleen  treated vs control %+8.3f%%   (control vs stored %+8.3f%%)\n",
              median(pct(d$baleen_krill_treated, d$baleen_krill_control)),
              median(pct(d$baleen_krill_control, d$baleen_krill_stored))))
  cat(sprintf("  fishes  treated vs control %+8.3f%%   (control vs stored %+8.3f%%)\n",
              median(pct(d$fish_krill_treated, d$fish_krill_control)),
              median(pct(d$fish_krill_control, d$fish_krill_stored))))
  cat(sprintf("  penguin treated vs control %+8.2f%%\n",
              median(pct(d$pen_krill_treated, d$pen_krill_control))))
}

cat("\n================ PAIRED EXPLOITED - UNEXPLOITED, 2010 ================\n")
cat("  (manuscript headline: lambda median -0.01404, 100% of 212 pairs negative)\n")
for (cond in c("stored", "control", "treated")) {
  d <- series %>% filter(year == 2010, cond == !!cond) %>%
    select(member, arm, lambda, total_biomass) %>%
    pivot_wider(names_from = arm, values_from = c(lambda, total_biomass))
  cat(sprintf("  %-8s lambda %+.6f (%d/%d negative) | biomass %+.3f%%\n", cond,
              median(d$lambda_fished - d$lambda_climate),
              sum(d$lambda_fished - d$lambda_climate < 0), nrow(d),
              median(pct(d$total_biomass_fished, d$total_biomass_climate))))
}

cat("\n================ STABILITY + RECALIBRATION ================\n")
for (r in res) {
  i <- which(r$control$params@species_params$species == PEN)
  cat(sprintf("  m%d (sim %4d) control stable=%-5s cv=%.3f | treated stable=%-5s cv=%.3f\n",
              r$member, r$sim_index, r$control$stab$stable, r$control$stab$max_cv,
              r$treated$stab$stable, r$treated$stab$max_cv))
  cat(sprintf("        penguin R_max  control %.4e  treated %.4e\n",
              r$control$params@species_params$R_max[i],
              r$treated$params@species_params$R_max[i]))
  cat(sprintf("        penguin erepro control %.4e  treated %.4e\n",
              r$control$params@species_params$erepro[i],
              r$treated$params@species_params$erepro[i]))
}
cat("\nSaved outputs to", out_dir, "\n")
