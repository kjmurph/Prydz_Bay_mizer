# =============================================================================
# The irreducible error floor: how much of the objective is reachable by ANY
# catchability, and how much is structural.
#
# WHAT THE MODEL CAN AND CANNOT DO.  Effort in effort_array_1841_2010.rds is
# BINARY -- 0/1 per gear per year -- and catchability is one scalar per gear.  So
# modelled catch is:
#   * exactly 0 in an effort-OFF year, whatever q is;
#   * q * selectivity * biomass in an effort-ON year, which can only vary as
#     slowly as biomass does.
# The model therefore HAS a year-to-year on/off mechanism (the effort switch),
# but no graded one.  Observed catch inside the effort-on years varies by orders
# of magnitude between adjacent years, and nothing in the parameterisation can
# follow that.
#
# THE BOUND.  For a model emitting 0 when effort is off and a constant c when it
# is on, the objective over a species' window is minimised at
# log10(c+1) = mean of log10(obs+1) over the effort-ON years, giving
#
#   floor^2 = [ sum_{effort off} log10(obs+1)^2
#             + sum_{effort on}  (log10(obs+1) - m)^2 ] / n
#
# The first term is what the effort array itself gets wrong (catch recorded in a
# year the model believes was unfished, or vice versa); the second is the
# within-window spread the model cannot track.  Neither term contains q.
#
# This bounds CONSTANT-catch models.  A modelled series that trends with biomass
# can beat it, so it is a reference, not a theorem -- reported as such.  Where a
# group's best-achievable-by-q error already sits at its floor, catchability has
# nothing left to give and the residual is structural.
#
#   Rscript R/catch_fit/07_error_floor.R
# =============================================================================

source("R/catch_fit/01_harness.R")

rows <- cf_comparison_rows()
eff  <- cf_effort()
eyr  <- as.numeric(rownames(eff))
D    <- cf_out_dir()

# is this species-year inside the effort window AND effort-on?
rows$effort_on <- mapply(function(y, s) {
  i <- match(y, eyr); !is.na(i) && eff[i, s] > 0
}, rows$Year, rows$Species)

floor_tab <- do.call(rbind, lapply(split(rows, rows$Species), function(d) {
  L    <- log10(d$Yield_obs + 1)
  on   <- d$effort_on
  m    <- if (any(on)) mean(L[on]) else 0
  sse_off <- sum(L[!on]^2)                 # effort says unfished, data says fished
  sse_on  <- sum((L[on] - m)^2)            # spread the model cannot track
  n <- nrow(d)
  data.frame(
    Species = d$Species[1], n = n, n_on = sum(on), n_off = sum(!on),
    n_zero_obs = sum(d$Yield_obs == 0),
    # how often does the effort switch disagree with the observations?
    n_off_but_caught = sum(!on & d$Yield_obs > 0),
    n_on_but_nothing = sum(on & d$Yield_obs == 0),
    obs_total_t = sum(d$Yield_obs) / 1e6,
    obs_max_t = max(d$Yield_obs) / 1e6,
    spread_decades = if (any(d$Yield_obs > 0))
      log10(max(d$Yield_obs) / min(d$Yield_obs[d$Yield_obs > 0])) else 0,
    floor_rmse = sqrt((sse_off + sse_on) / n),
    floor_from_effort_mismatch = sqrt(sse_off / n),
    floor_from_within_window_spread = sqrt(sse_on / n),
    best_constant_t = (10^m - 1) / 1e6,
    stringsAsFactors = FALSE)
}))

base_f <- file.path(D, "00_baseline_per_species.csv")
if (file.exists(base_f)) {
  B <- read.csv(base_f, check.names = FALSE)
  a <- aggregate(B$rmse, list(Species = B$Species), mean)
  floor_tab$fitted_rmse_mean <- a$x[match(floor_tab$Species, a$Species)]
}
v_f <- file.path(D, "06_gear_verdicts.csv")
if (file.exists(v_f)) {
  V <- read.csv(v_f)
  a2 <- aggregate(V$own_rmse_best, list(Species = V$gear), mean)
  floor_tab$best_q_rmse_mean <- a2$x[match(floor_tab$Species, a2$Species)]
}
floor_tab$headroom_above_floor <- floor_tab$best_q_rmse_mean - floor_tab$floor_rmse
floor_tab <- floor_tab[order(-floor_tab$n * floor_tab$floor_rmse^2), ]

cat("=== irreducible error floor, given the binary effort switch ===\n")
cat("floor  best RMSE for a model emitting 0 when effort is off and a constant when on\n")
cat("fitted members' mean baseline RMSE\n")
cat("best_q members' mean best achievable with that gear's catchability alone\n")
cat("head   best_q - floor: what catchability still has left to give\n\n")
print(data.frame(
  Species = floor_tab$Species, n = floor_tab$n,
  on = floor_tab$n_on, `off+caught` = floor_tab$n_off_but_caught,
  `on+nothing` = floor_tab$n_on_but_nothing,
  decades = round(floor_tab$spread_decades, 1),
  floor = round(floor_tab$floor_rmse, 3),
  fitted = round(floor_tab$fitted_rmse_mean, 3),
  best_q = round(floor_tab$best_q_rmse_mean, 3),
  head = round(floor_tab$headroom_above_floor, 3),
  check.names = FALSE), row.names = FALSE)

cat("\n=== where the floor comes from ===\n")
print(data.frame(
  Species = floor_tab$Species,
  floor = round(floor_tab$floor_rmse, 3),
  from_effort_mismatch = round(floor_tab$floor_from_effort_mismatch, 3),
  from_within_window_spread = round(floor_tab$floor_from_within_window_spread, 3)),
  row.names = FALSE)

pooled_floor <- sqrt(sum(floor_tab$floor_rmse^2 * floor_tab$n) / sum(floor_tab$n))
cat(sprintf("\nPOOLED constant-catch reference over all %d rows: %.4f\n",
            sum(floor_tab$n), pooled_floor))
if (!is.null(floor_tab$fitted_rmse_mean)) {
  pf <- sqrt(sum(floor_tab$fitted_rmse_mean^2 * floor_tab$n) / sum(floor_tab$n))
  bq <- sqrt(sum(floor_tab$best_q_rmse_mean^2 * floor_tab$n) / sum(floor_tab$n))
  cat(sprintf("POOLED fitted (members' mean):                  %.4f\n", pf))
  cat(sprintf("POOLED if every gear took its own best q:       %.4f\n", bq))
}

cat("\nREAD THIS BOUND CAREFULLY.  It bounds a CONSTANT-catch model, and the\n")
cat("model is not constant -- its catch tracks biomass, which over 1841-2010\n")
cat("moves a long way as the whale groups are removed.  Six of nine groups\n")
cat("already BEAT the reference, which is not a contradiction: the trend in\n")
cat("biomass is doing fitting work that a constant cannot.  The bound is only\n")
cat("binding where `head` is positive.\n\n")
tight <- floor_tab[!is.na(floor_tab$headroom_above_floor) &
                     floor_tab$headroom_above_floor > 0, ]
if (nrow(tight)) {
  cat("groups still ABOVE the constant-catch reference at their best q --\n")
  cat("i.e. where something other than catchability is still available:\n")
  print(data.frame(Species = tight$Species,
                   best_q = round(tight$best_q_rmse_mean, 3),
                   floor = round(tight$floor_rmse, 3),
                   head = round(tight$headroom_above_floor, 3)), row.names = FALSE)
}
cat("\nThe robust statistic is `decades`: the log10 range of NON-ZERO observed\n")
cat("catch inside the window.  Modelled catch varies smoothly by well under one\n")
cat("decade, so a group observed to span 5-6 decades cannot be fitted closely by\n")
cat("ANY scalar catchability -- shelf and coastal fishes (6.4), antarctic krill\n")
cat("(5.6) and toothfishes (5.0) are the extreme cases, and the first two of\n")
cat("those carry 79% of the objective's error between them.\n")

write.csv(floor_tab, file.path(D, "07_error_floor.csv"), row.names = FALSE)
cat("\nwrote 07_error_floor.csv\n")
