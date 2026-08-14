# =============================================================================
# Phase 90 -- A/B test of the catchability fitting rule, on cached states
#
# Phase 89 corrects the ISIMIP3a calibration window. Before it runs on the full
# ensemble it is worth checking on a handful of members what the correction
# actually does -- and, separately, that nothing ELSE changed with it.
#
# ------------------------------------------------------- why three arms, not two
# Comparing phase 45 with phase 89 directly would confound TWO changes, because
# phase 89 also altered the ESTIMATOR:
#
#   phase 45   step <- 1 / median(mod_tot / obs_tot)   # median ACROSS MEMBERS
#              step <- pmin(pmax(step, 1/50), 50)      # per-iteration step cap
#              M    <- M * step
#
#   phase 89   r <- sum(Yield_mod) / sum(Yield_obs)    # POOLED across members
#              M <- M / pmax(r, 1e-6)                  # no cap
#
# A pooled sum across members is the construction this project has already been
# burned by: a single divergent member once owned 83% of an across-member sum.
# The median is the robust choice and is what phase 45 used. Whether that matters
# HERE is an empirical question, so this script measures it rather than assuming.
#
#   A "phase45"  legacy window, zero-filled, median estimator   (the old fit)
#   B "window"   2004 window,   NA dropped,  median estimator   (the fix ALONE)
#   C "phase89"  2004 window,   NA dropped,  pooled estimator   (as committed)
#
# A -> B is the window correction. B -> C is the estimator change. Reporting both
# means neither can hide inside the other.
#
# ------------------------------------------------------------------ evaluation
# ALL arms are scored on the CAPPED (2004) window. Scoring the legacy arm on its
# own window would flatter it by construction -- it was fitted to phantom zeros,
# so including those zeros in its score rewards predicting them. Years 2005-2010
# are reported SEPARATELY as genuine out-of-sample, which is what the ISIMIP3a
# protocol reserves them for.
#
# ------------------------------------------------------------------ what it is not
# This is a METHOD TEST on 10 members, not a production fit. Phase 45 fitted on a
# 300-member subsample; a 10-member median is noisy and the multipliers here
# should not be carried anywhere. What transfers is the DIRECTION and rough SIZE
# of each effect, and the ranking of the arms.
#
# WHY RE-PROJECTING IS EXACT: initial_effort is 0, so steady() and the spin-up are
# UNFISHED and catchability cannot touch the initial condition. It enters only the
# 1841-2010 projection, so a cached state can be re-projected bit-exactly.
#
# USAGE  Rscript R/wmin_test/90_catchability_ab_test.R
# ENV    P90_STATE_DIR, P90_MEMBERS, P90_CORES, P90_ITERS, P90_OUT, P90_QMAX
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
  library(tidyr)
})

OL <- "Output_large_files/wmin_test"
STATE_DIR <- Sys.getenv("P90_STATE_DIR",
  file.path(OL, "77_rmaxpostcap_n500_nomg_recap09_K1_R_max_states"))
# the top 10 of the 166 usable (stable AND admissible) phase-77 members, in
# cut-A rank order
MEMBERS <- as.integer(trimws(strsplit(Sys.getenv("P90_MEMBERS",
  "384,71,446,173,757,1055,531,55,946,122"), ",")[[1]]))
CORES <- min(as.integer(Sys.getenv("P90_CORES", "10")),
             max(1L, parallel::detectCores() - 2L), length(MEMBERS))
ITERS <- as.integer(Sys.getenv("P90_ITERS", "6"))
QMAX  <- as.numeric(Sys.getenv("P90_QMAX", "1"))
CAP_YEAR <- 2004L
M_STEP_CAP <- 50            # phase 45's per-iteration change cap
OUT <- Sys.getenv("P90_OUT", file.path(OL, "90_catchability_ab_test.rds"))

if (!dir.exists(STATE_DIR)) stop("no state dir: ", STATE_DIR, call. = FALSE)
states <- file.path(STATE_DIR, sprintf("state_%05d.rds", MEMBERS))
if (!all(file.exists(states)))
  stop("missing states: ", paste(MEMBERS[!file.exists(states)], collapse = ", "),
       call. = FALSE)

effort_arr <- readRDS("effort_array_1841_2010.rds")
eff_yrs <- as.numeric(rownames(effort_arr))
SPN_EFF <- colnames(effort_arr)

cat("=== Phase 90: catchability A/B test ===\n")
cat("states :", basename(STATE_DIR), "\n")
cat("members:", length(MEMBERS), "|", paste(MEMBERS, collapse = ", "), "\n")
cat("cores  :", CORES, "| iterations:", ITERS, "| q ceiling:", QMAX, "\n\n")

# --- the two observed series --------------------------------------------------
obs_w <- read.csv("yield_observed_timeseries.csv", check.names = FALSE)
ocols <- setdiff(names(obs_w), "Year")

# window bounds per species, both rules side by side
win <- do.call(rbind, lapply(SPN_EFF, function(s) {
  cn <- ocols[match(make.names(s), make.names(ocols))]
  if (is.na(cn)) return(NULL)
  ey <- eff_yrs[effort_arr[, s] > 0]
  if (!length(ey)) return(NULL)
  oc <- obs_w[[cn]]; oy <- obs_w$Year
  reported <- oy[is.finite(oc) & oc > 0]
  if (!length(reported)) return(NULL)
  data.frame(Species = s, first_year = min(ey),
             legacy_last = max(ey),
             capped_last = min(CAP_YEAR, max(reported), max(ey)),
             reported_last = max(reported), stringsAsFactors = FALSE)
}))
win$years_dropped <- win$legacy_last - win$capped_last

obs_all <- obs_w %>%
  pivot_longer(-Year, names_to = "col", values_to = "Yield_g") %>%
  mutate(Species = SPN_EFF[match(make.names(col), make.names(SPN_EFF))]) %>%
  filter(!is.na(Species)) %>% left_join(win, by = "Species") %>%
  filter(!is.na(first_year))

# LEGACY: effort window to 2010, missing catch coerced to zero (phase 45)
obs_legacy <- obs_all %>%
  filter(Year >= first_year, Year <= legacy_last) %>%
  transmute(Year, Species,
            Yield_obs = pmax(coalesce(as.numeric(Yield_g), 0), 0))
# CAPPED: window ends at min(2004, last reported), missing catch DROPPED
obs_capped <- obs_all %>%
  filter(Year >= first_year, Year <= capped_last, is.finite(Yield_g)) %>%
  transmute(Year, Species, Yield_obs = pmax(Yield_g, 0))
# OUT OF SAMPLE: real reported catch after the calibration window
obs_oos <- obs_all %>%
  filter(Year > CAP_YEAR, is.finite(Yield_g), Yield_g > 0) %>%
  transmute(Year, Species, Yield_obs = Yield_g)

FIT_SP <- obs_capped %>% group_by(Species) %>%
  summarise(o = sum(Yield_obs), .groups = "drop") %>% filter(o > 0) %>%
  pull(Species)

# how much of the legacy objective is phantom
phantom <- obs_legacy %>%
  left_join(obs_capped %>% transmute(Year, Species, real = TRUE),
            by = c("Year", "Species")) %>%
  group_by(Species) %>%
  summarise(n_legacy = n(), n_capped = sum(!is.na(real)),
            n_phantom = sum(is.na(real)),
            n_phantom_zero = sum(is.na(real) & Yield_obs == 0),
            .groups = "drop")

cat("=== the fitting window, legacy vs capped ===\n")
print(as.data.frame(win %>% select(Species, first_year, legacy_last,
                                   capped_last, reported_last, years_dropped) %>%
                    arrange(desc(years_dropped))), row.names = FALSE)
cat("\n=== observations gained/lost, and how many were phantom zeros ===\n")
print(as.data.frame(phantom %>% arrange(desc(n_phantom))), row.names = FALSE)
cat("\nfitting", length(FIT_SP), "species |", nrow(obs_capped),
    "capped observations vs", nrow(obs_legacy), "legacy\n\n")

# ------------------------------------------------------------------ worker ---
project_one <- function(f, M) {
  suppressPackageStartupMessages({library(mizer); library(therMizer)})
  z <- readRDS(f); p <- z$params
  gp <- gear_params(p)
  m <- M[match(gp$species, names(M))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
  gear_params(p) <- gp
  s <- try(project(p, initial_n = z$initial_n, t_start = 1841,
                   effort = effort_arr, progress_bar = FALSE), silent = TRUE)
  if (inherits(s, "try-error")) return(NULL)
  y <- getYield(s); yr <- as.numeric(rownames(y))
  si <- as.integer(sub("^state_0*", "", sub("\\.rds$", "", basename(f))))
  data.frame(sim_index = si,
             Year = rep(yr, times = ncol(y)),
             Species = rep(colnames(y), each = length(yr)),
             Yield_mod = pmax(as.vector(y), 0), stringsAsFactors = FALSE)
}

cl <- makeCluster(CORES)
on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
clusterExport(cl, c("effort_arr", "QMAX", "project_one"), envir = environment())

run_batch <- function(M) {
  clusterExport(cl, "M", envir = environment())
  r <- parLapplyLB(cl, states, function(f) project_one(f, M))
  bad <- vapply(r, is.null, logical(1))
  if (all(bad)) stop("every member failed to project", call. = FALSE)
  if (any(bad)) cat("   ", sum(bad), "member(s) failed to project\n")
  bind_rows(r[!bad])
}

# --- the two update rules -----------------------------------------------------
# MEDIAN ACROSS MEMBERS, with phase 45's per-iteration step cap.
step_median <- function(J, M) {
  rt <- J %>% group_by(sim_index, Species) %>%
    summarise(mod = sum(Yield_mod), obs = sum(Yield_obs), .groups = "drop") %>%
    filter(obs > 0) %>% group_by(Species) %>%
    summarise(r = median(mod / obs, na.rm = TRUE), .groups = "drop")
  st <- pmin(pmax(1 / pmax(rt$r, 1e-12), 1 / M_STEP_CAP), M_STEP_CAP)
  M[rt$Species] <- M[rt$Species] * st
  list(M = M, r = setNames(rt$r, rt$Species))
}
# POOLED SUMS across members, uncapped -- phase 89 as committed.
step_pooled <- function(J, M) {
  rt <- J %>% group_by(Species) %>%
    summarise(mod = sum(Yield_mod), obs = sum(Yield_obs), .groups = "drop") %>%
    filter(obs > 0) %>% mutate(r = mod / obs)
  M[rt$Species] <- M[rt$Species] / pmax(rt$r, 1e-6)
  list(M = M, r = setNames(rt$r, rt$Species))
}

ARMS <- list(
  phase45 = list(obs = obs_legacy, step = step_median,
                 label = "legacy window, zero-filled, median estimator"),
  window  = list(obs = obs_capped, step = step_median,
                 label = "2004 window, NA dropped, median estimator"),
  phase89 = list(obs = obs_capped, step = step_pooled,
                 label = "2004 window, NA dropped, POOLED estimator"))

fits <- list(); traces <- list()
for (a in names(ARMS)) {
  A <- ARMS[[a]]
  cat("=== arm", a, "--", A$label, "===\n")
  M <- setNames(rep(1, length(FIT_SP)), FIT_SP)
  tr <- list()
  for (it in seq_len(ITERS)) {
    t1 <- proc.time()
    Y <- run_batch(M)
    J <- inner_join(Y, A$obs, by = c("Year", "Species")) %>%
      filter(Species %in% FIT_SP)
    up <- A$step(J, M)
    tr[[it]] <- data.frame(arm = a, iter = it, Species = names(up$r),
                           ratio = as.numeric(up$r),
                           M_before = as.numeric(M[names(up$r)]),
                           M_after = as.numeric(up$M[names(up$r)]),
                           stringsAsFactors = FALSE)
    cat(sprintf("  iter %d | median |log10 ratio| %.4f | %.1f min\n", it,
                median(abs(log10(pmax(up$r, 1e-12)))),
                (proc.time() - t1)[["elapsed"]] / 60))
    flush.console()
    M <- up$M
  }
  fits[[a]] <- M
  traces[[a]] <- bind_rows(tr)
  cat("  final multipliers:",
      paste(sprintf("%s=%.3g", names(M), M), collapse = ", "), "\n\n")
}

# --- reference multiplier sets -----------------------------------------------
# "drawn"    : no correction at all, the member's own drawn q
# "stored45" : the multipliers the CURRENT ensemble actually uses, off disk
RF <- readRDS(file.path(OL, "45_refit_results.rds"))
Msets <- c(list(drawn = setNames(rep(1, length(FIT_SP)), FIT_SP),
                stored45 = RF$M), fits)

cat("=== fitted multipliers, all arms ===\n")
MTAB <- data.frame(Species = FIT_SP, stringsAsFactors = FALSE)
for (nm in names(Msets)) {
  v <- Msets[[nm]][FIT_SP]; v[is.na(v)] <- 1
  MTAB[[nm]] <- signif(as.numeric(v), 4)
}
print(MTAB, row.names = FALSE)

# --- evaluate every set on the SAME honest window ----------------------------
cat("\n=== evaluating on the 2004 window (and 2005-2010 out of sample) ===\n")
EV <- list()
for (nm in names(Msets)) {
  Mn <- Msets[[nm]]
  Y <- run_batch(Mn)
  ins <- inner_join(Y, obs_capped, by = c("Year", "Species")) %>%
    filter(Species %in% FIT_SP)
  oos <- inner_join(Y, obs_oos, by = c("Year", "Species")) %>%
    filter(Species %in% FIT_SP)
  per_sp <- ins %>% group_by(sim_index, Species) %>%
    summarise(n = n(),
              sse = sum((log10(Yield_mod + 1) - log10(Yield_obs + 1))^2),
              obs_tot = sum(Yield_obs), mod_tot = sum(Yield_mod),
              .groups = "drop")
  oos_sp <- oos %>% group_by(sim_index, Species) %>%
    summarise(obs_tot = sum(Yield_obs), mod_tot = sum(Yield_mod),
              .groups = "drop")
  EV[[nm]] <- list(per_sp = per_sp, oos_sp = oos_sp,
                   rmse = per_sp %>% group_by(sim_index) %>%
                     summarise(rmse = sqrt(sum(sse) / sum(n)), .groups = "drop"))
  cat(sprintf("  %-9s median member RMSE %.4f\n", nm, median(EV[[nm]]$rmse$rmse)))
}

cat("\n=== modelled / observed catch, median over members, 2004 window ===\n")
RAT <- Reduce(function(x, y) full_join(x, y, by = "Species"),
  lapply(names(EV), function(nm) EV[[nm]]$per_sp %>% filter(obs_tot > 0) %>%
    group_by(Species) %>%
    summarise(!!nm := signif(median(mod_tot / obs_tot), 3), .groups = "drop")))
print(as.data.frame(RAT), row.names = FALSE)
cat("\n(1.00 is a perfect match; the current ensemble is the 'stored45' column)\n")

cat("\n=== the same, 2005-2010 OUT OF SAMPLE ===\n")
ROOS <- Reduce(function(x, y) full_join(x, y, by = "Species"),
  lapply(names(EV), function(nm) EV[[nm]]$oos_sp %>% filter(obs_tot > 0) %>%
    group_by(Species) %>%
    summarise(!!nm := signif(median(mod_tot / obs_tot), 3), .groups = "drop")))
if (!nrow(ROOS)) {
  cat("  NONE. No species has a reported catch > 0 after 2004 in this file,\n")
  cat("  so there is no out-of-sample catch to score against -- which is the\n")
  cat("  same fact that made the legacy window's post-2004 years phantom.\n")
} else {
  print(as.data.frame(ROOS), row.names = FALSE)
  cat("  (species absent here have no reported post-2004 catch)\n")
}

cat("\n=== share of the pooled objective, by species ===\n")
SH <- Reduce(function(x, y) full_join(x, y, by = "Species"),
  lapply(names(EV), function(nm) EV[[nm]]$per_sp %>% group_by(Species) %>%
    summarise(!!nm := round(100 * sum(sse) /
                sum(EV[[nm]]$per_sp$sse), 1), .groups = "drop")))
SH <- SH %>% left_join(EV[[1]]$per_sp %>% group_by(Species) %>%
  summarise(pct_n = round(100 * sum(n) / sum(EV[[1]]$per_sp$n), 1),
            .groups = "drop"), by = "Species")
print(as.data.frame(SH %>% arrange(desc(phase89))), row.names = FALSE)

saveRDS(list(multipliers = Msets, table = MTAB, traces = bind_rows(traces),
             eval = EV, ratio_in = RAT, ratio_oos = ROOS, share = SH,
             window = win, phantom = phantom,
             meta = list(state_dir = STATE_DIR, members = MEMBERS,
                         iters = ITERS, qmax = QMAX, cap_year = CAP_YEAR,
                         built = Sys.time())), OUT)
cat("\nWROTE", OUT, "\n")
