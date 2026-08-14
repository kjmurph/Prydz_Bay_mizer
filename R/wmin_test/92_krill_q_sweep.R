# =============================================================================
# Phase 92 -- is the krill catch limited by the q CEILING or by the STOCK?
#
# Phase 89 on the phase-88 states drove antarctic krill catchability to
# M = 2.615e5 and still reached only 0.204 of the observed catch. Verified: 200
# of 200 sampled members have krill q pinned at the QMAX = 1 ceiling, from drawn
# values spanning 0.00197-0.822. The fit is NON-IDENTIFIABLE -- the multiplier
# diverges while the ratio stays flat -- and it has destroyed the sampled spread
# in krill catchability, which is the same harm phase 45 documented for the
# whales. Krill now carries 48.5% of the objective on 7.6% of the observations.
#
# ------------------------------------------------------------- the question
# From 45_catchability_refit.R's own header:
#
#   "Catch is sub-linear in q, so: if the catch keeps rising as q goes past 1,
#    the cap was the binding limit; if it saturates, the STOCK is the limit and
#    no catchability can fix it. Run it both ways and report which."
#
# The [0, 1] clamp is an author's choice from 09_Uncertainty_Analysis.Rmd, NOT a
# mizer constraint: q is a rate in F = q * effort * selectivity, not a
# probability, and effort here is a normalised index, so q absorbs its scale.
# Values above 1 are admissible; whether they help is what this measures.
#
# ------------------------------------------------------------- what it does
# Sets krill catchability to each value on an ABSOLUTE grid (ignoring the drawn
# value, so every member is at the same q), holds every other species at the
# phase-89 fitted multiplier, projects, and reports two things per q:
#
#   catch ratio       median over members of modelled/observed krill catch
#   exploitation rate median over members and years of
#                     krill catch / krill standing stock, inside the window
#
# The second is the one that settles it. If matching the observed catch requires
# removing an implausible fraction of the standing stock each year, then the
# stock is the limit and no q is defensible -- the same argument that settled the
# whales, where observed catch is ~23.8x the calibrated stock.
#
# USAGE  Rscript R/wmin_test/92_krill_q_sweep.R
# ENV    P92_STATE_DIR, P92_N, P92_CORES, P92_GRID, P92_REFIT, P92_OUT
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
  library(tidyr)
})

OL <- "Output_large_files/wmin_test"
STATE_DIR <- Sys.getenv("P92_STATE_DIR", file.path(OL, "88_full_states"))
REFIT <- Sys.getenv("P92_REFIT", file.path(OL, "89_refit_results.rds"))
N     <- as.integer(Sys.getenv("P92_N", "20"))
CORES <- min(as.integer(Sys.getenv("P92_CORES", "20")),
             max(1L, parallel::detectCores() - 2L))
GRID  <- as.numeric(trimws(strsplit(
  Sys.getenv("P92_GRID", "0.01,0.05,0.1,0.25,0.5,1,2,5,10,25"), ",")[[1]]))
OUT   <- Sys.getenv("P92_OUT", file.path(OL, "92_krill_q_sweep.rds"))
SP    <- "antarctic krill"

if (!dir.exists(STATE_DIR)) stop("no state dir: ", STATE_DIR, call. = FALSE)
RF <- readRDS(REFIT)
M_FIT <- RF$M
effort_arr <- readRDS("effort_array_1841_2010.rds")

# the krill fitting window, straight from the phase-89 output so it cannot drift
win <- RF$window[RF$window$Species == SP, ]
YR <- seq(win$first_year, win$last_year)
obs <- RF$obs_used %>% filter(Species == SP)
OBS_TOT <- sum(obs$Yield_obs)

cat("=== Phase 92: krill q sweep -- ceiling or stock? ===\n")
cat("states :", basename(STATE_DIR), "\n")
cat("window :", win$first_year, "-", win$last_year, "|", nrow(obs), "observations\n")
cat(sprintf("observed krill catch over the window: %.4g g = %.0f t\n",
            OBS_TOT, OBS_TOT / 1e6))
cat("q grid :", paste(GRID, collapse = ", "), "\n")

# Prefer STABLE members -- an unstable one contributes a meaningless stock.
sel <- NULL
p88 <- file.path(OL, "88_full.rds")
if (file.exists(p88)) {
  MEM <- readRDS(p88)$members
  us <- MEM$sim_index[MEM$stable & MEM$n_erepro_ge1 == 0]
  cat("usable (stable AND admissible) members available:", length(us), "\n")
  sel <- head(us, N)
}
states <- if (!is.null(sel) && length(sel))
  file.path(STATE_DIR, sprintf("state_%05d.rds", sel)) else
  head(sort(list.files(STATE_DIR, pattern = "^state_\\d+\\.rds$",
                       full.names = TRUE)), N)
states <- states[file.exists(states)]
cat("members:", length(states), "| cores:", CORES, "\n\n")
CORES <- min(CORES, length(states))

# ------------------------------------------------------------------ worker ---
# Every species except krill keeps its drawn q times the phase-89 multiplier,
# clamped at 1 exactly as phase 89 does. Krill is SET to the grid value, so the
# sweep is not confounded by the drawn spread.
run_one <- function(f, qk) {
  suppressPackageStartupMessages({library(mizer); library(therMizer)})
  z <- readRDS(f); p <- z$params
  gp <- gear_params(p)
  m <- M_FIT[match(gp$species, names(M_FIT))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(1, pmax(0, gp$catchability * m))
  gp$catchability[gp$species == SP] <- qk          # absolute, uncapped
  gear_params(p) <- gp
  s <- try(project(p, initial_n = z$initial_n, t_start = 1841,
                   effort = effort_arr, progress_bar = FALSE), silent = TRUE)
  if (inherits(s, "try-error")) return(NULL)
  y <- getYield(s); b <- getBiomass(s)
  yr <- as.numeric(rownames(y))
  k <- yr %in% YR
  ycat <- as.numeric(y[k, SP]); bstk <- as.numeric(b[k, SP])
  data.frame(sim_index = as.integer(sub("^state_0*", "",
                                        sub("\\.rds$", "", basename(f)))),
             q = qk,
             mod_tot = sum(ycat),
             biomass_med = median(bstk),
             expl_rate = median(ycat / pmax(bstk, 1e-30)),
             stringsAsFactors = FALSE)
}

cl <- makeCluster(CORES)
on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
clusterExport(cl, c("effort_arr", "M_FIT", "SP", "YR", "run_one"),
              envir = environment())
res <- list()
for (qk in GRID) {
  t0 <- proc.time()
  clusterExport(cl, "qk", envir = environment())
  r <- parLapplyLB(cl, states, function(f) run_one(f, qk))
  d <- bind_rows(r[!vapply(r, is.null, logical(1))])
  res[[as.character(qk)]] <- d
  cat(sprintf("  q = %-6g ratio %.4f | expl rate %.4f | %d members | %.1f s\n",
              qk, median(d$mod_tot) / OBS_TOT, median(d$expl_rate),
              nrow(d), (proc.time() - t0)[["elapsed"]]))
  flush.console()
}
stopCluster(cl)
ALL <- bind_rows(res)

SUM <- ALL %>% group_by(q) %>%
  summarise(n = n(),
            catch_ratio = median(mod_tot) / OBS_TOT,
            expl_rate = median(expl_rate),
            biomass = median(biomass_med), .groups = "drop") %>%
  mutate(ratio_gain = catch_ratio / lag(catch_ratio),
         q_gain = q / lag(q),
         elasticity = log(ratio_gain) / log(q_gain))

cat("\n=== krill catch vs catchability ===\n")
print(as.data.frame(SUM %>% transmute(q,
  catch_ratio = signif(catch_ratio, 4),
  expl_rate = signif(expl_rate, 4),
  biomass_t = signif(biomass / 1e6, 4),
  elasticity = signif(elasticity, 3))), row.names = FALSE)

cat("\n  elasticity is d log(catch) / d log(q): 1 means catch scales with q,\n")
cat("  0 means the stock is the limit and more q buys nothing.\n")

top <- SUM %>% filter(q >= 1)
cat("\n=== verdict ===\n")
if (nrow(top) >= 2) {
  e <- tail(top$elasticity[is.finite(top$elasticity)], 1)
  best <- SUM$catch_ratio[which.max(SUM$catch_ratio)]
  cat(sprintf("  best catch ratio reached: %.4f at q = %g\n", best,
              SUM$q[which.max(SUM$catch_ratio)]))
  cat(sprintf("  elasticity above q = 1  : %.3f\n", e))
  if (is.finite(e) && e < 0.15) {
    cat("  SATURATED -- the STOCK is the binding limit. No catchability value\n")
    cat("  reaches the observed catch, so krill q should be HELD at the drawn\n")
    cat("  values, as baleen/sperm/minke already are.\n")
  } else {
    cat("  STILL RISING -- the q = 1 CEILING was binding, not the stock.\n")
    cat("  Re-run phase 89 with a higher P89_QMAX rather than holding krill.\n")
  }
}
saveRDS(list(all = ALL, summary = SUM, obs_total = OBS_TOT, window = win,
             members = basename(states), grid = GRID,
             meta = list(state_dir = STATE_DIR, refit = REFIT,
                         built = Sys.time())), OUT)
cat("\nWROTE", OUT, "\n")