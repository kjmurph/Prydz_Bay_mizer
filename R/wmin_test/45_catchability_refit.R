# =============================================================================
# PHASE 5 -- global per-species catchability re-fit, from the cached states.
#
# WHY. The base catchability was hand-set at 09_Uncertainty_Analysis.Rmd:1504-1512
# against the OLD ensemble, whose whale stocks were ~13x larger than the
# recalibrated model's (baleen whales 0.05). It is stale: with a biomass-calibrated
# stock the same catchability lands a small fraction of the observed catch. A sweep
# on member 1339 went from a catch ratio of 0.094 at its drawn q = 0.0145 to 0.72
# at q = 0.25 (docs/new_ensemble_44_results.md).
#
# METHOD -- the documented one, 08_ISIMIP3a_simulations_Prydz_Bay.Rmd:449-511:
# divide catchability by the modelled/observed yield ratio, re-project, iterate.
# Applied here to the PROJECTION yield rather than a steady-state yield, because
# the base params carry yield_observed = 0 (zeroed at 06:152) and the quantity we
# fit is the 1841-2010 catch.
#
# GLOBAL PER SPECIES, NOT PER MEMBER. One multiplier M_s per species is applied to
# every member's own drawn catchability:
#
#     q_new[i,s] = min(1, q_drawn[i,s] * M_s)
#
# so the Monte Carlo spread in catchability is preserved and only its centre moves.
# Fitting q per member would maximise the fit but turn catchability from a sampled
# uncertainty into an outcome.
#
# WHY THIS IS CHEAP AND EXACT. `initial_effort` is 0 for all gears, so steady() and
# the 118 yr spin-up are UNFISHED -- catchability cannot touch the initial
# condition and enters only the 1841-2010 projection. Re-projecting a cached state
# is therefore bit-exact, not an approximation (catch-fit-reprojection-exact).
#
# CATCHABILITY IS NOT BOUNDED AT 1. The [0, 1] clamp is an author's choice in
# run_single_enhanced_sim (09_Uncertainty_Analysis.Rmd:347, 351), not a mizer
# constraint: q is a rate in F = q * effort * selectivity, not a probability, and
# mizer accepts any non-negative value. REFIT_QMAX controls the ceiling here.
#
# Raising it is also the clean way to separate two explanations of the poor whale
# catch. Catch is sub-linear in q (member 1339: 17x q gave 7.7x catch), so:
#   - if the catch keeps rising as q goes past 1, the cap was the binding limit;
#   - if it saturates, the STOCK is the limit and no catchability can fix it --
#     the observed baleen total is 23.8x the calibrated standing stock.
# Run it both ways and report which. A boundary solution at the ceiling is a
# finding about the model, not something to tune away.
#
# USAGE
#   Rscript R/wmin_test/45_catchability_refit.R fit     # iterate on a subsample
#   Rscript R/wmin_test/45_catchability_refit.R apply   # re-project all, re-rank
#
# ENV: REFIT_CORES (14), REFIT_SUB (300), REFIT_ITERS (4),
#      REFIT_QMAX (1 = the original clamp; try 100 to let q exceed 1)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel)
  library(dplyr); library(reshape2)
})

out_dir   <- "Output_large_files/wmin_test"
state_dir <- file.path(out_dir, "44_states")
fit_path  <- file.path(out_dir, "45_catchability_multipliers.rds")

CORES <- as.integer(Sys.getenv("REFIT_CORES", "14"))
SUB   <- as.integer(Sys.getenv("REFIT_SUB", "300"))
ITERS <- as.integer(Sys.getenv("REFIT_ITERS", "4"))
QMAX  <- as.numeric(Sys.getenv("REFIT_QMAX", "1"))   # see the header: q is a rate
M_STEP_CAP <- 50      # per-iteration change cap, keeps the first step sane
mode <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(mode)) mode <- "fit"
stopifnot(mode %in% c("fit", "apply"))

effort_arr <- readRDS("effort_array_1841_2010.rds")
ew <- do.call(rbind, lapply(colnames(effort_arr), function(s) {
  y <- as.numeric(rownames(effort_arr))[effort_arr[, s] > 0]
  if (length(y)) data.frame(Species = s, first_year = min(y), last_year = max(y),
                            stringsAsFactors = FALSE)
}))
obs_long <- read.csv("yield_observed_timeseries.csv") %>%
  reshape2::melt(id.vars = "Year", variable.name = "Species", value.name = "Yield_g") %>%
  mutate(Species = gsub("\\.", " ", as.character(Species)),
         Yield_g = pmax(coalesce(as.numeric(Yield_g), 0), 0)) %>%
  left_join(ew, by = "Species") %>%
  filter(!is.na(first_year), Year >= first_year, Year <= last_year) %>%
  select(Year, Species, Yield_obs = Yield_g)

# species that are actually fished and have a non-zero observed catch
FIT_SP <- obs_long %>% group_by(Species) %>% summarise(o = sum(Yield_obs)) %>%
  filter(o > 0) %>% pull(Species)

states <- sort(list.files(state_dir, pattern = "^state_treated_\\d+\\.rds$",
                          full.names = TRUE))
if (!length(states)) stop("no cached states in ", state_dir)

# ------------------------------------------------------------------ worker ----
# Re-project one cached member under a per-species catchability multiplier and
# return its per-species modelled/observed totals plus the yield RMSE.
reproject <- function(f, M, qmax = 1) {
  suppressPackageStartupMessages({
    library(therMizer); library(mizer); library(dplyr); library(reshape2)
  })
  z <- readRDS(f)
  p <- z$params
  gp <- gear_params(p)
  m <- M[match(gp$species, names(M))]
  m[is.na(m)] <- 1
  gp$catchability <- pmin(qmax, pmax(0, gp$catchability * m))
  gear_params(p) <- gp
  q_at_cap <- sum(gp$catchability >= qmax * (1 - 1e-9) & m > 1)

  pr <- try(project(p, initial_n = z$initial_n, t_start = 1841, effort = EFF),
            silent = TRUE)
  if (inherits(pr, "try-error")) return(NULL)
  d <- reshape2::melt(getYield(pr))
  names(d) <- c("Year", "Species", "Yield_mod")
  d$Year <- as.numeric(as.character(d$Year)); d$Species <- as.character(d$Species)
  d$Yield_mod <- pmax(d$Yield_mod, 0)
  cmp <- OBS %>% left_join(d, by = c("Year", "Species")) %>%
    mutate(Yield_mod = coalesce(Yield_mod, 0))
  per_sp <- cmp %>% group_by(Species) %>%
    summarise(n = n(),
              sse = sum((log10(Yield_mod + 1) - log10(Yield_obs + 1))^2),
              obs_tot = sum(Yield_obs), mod_tot = sum(Yield_mod), .groups = "drop")
  list(sim_index = z$sim_index,
       rmse = sqrt(mean((log10(cmp$Yield_mod + 1) - log10(cmp$Yield_obs + 1))^2,
                        na.rm = TRUE)),
       cor_log = suppressWarnings(cor(log10(cmp$Yield_mod + 1),
                                      log10(cmp$Yield_obs + 1), use = "complete.obs")),
       q_at_cap = q_at_cap, per_sp = per_sp)
}

run_batch <- function(files, M) {
  cl <- makeCluster(CORES)
  on.exit(stopCluster(cl), add = TRUE)
  assign("EFF", effort_arr, envir = environment())
  assign("OBS", obs_long, envir = environment())
  clusterExport(cl, c("EFF", "OBS", "reproject"), envir = environment())
  # M must be passed as an ARGUMENT, not captured from the closure: parLapply
  # serialises the function but not the globals it references, so a closure over
  # M fails on every worker with "object 'M' not found" -- and if the failures are
  # swallowed to NULL it looks like an empty result rather than an error.
  r <- parLapplyLB(cl, files, function(f, MM, QQ)
    tryCatch(reproject(f, MM, QQ),
             error = function(e) paste("ERROR:", conditionMessage(e))),
    MM = M, QQ = QMAX)
  bad <- vapply(r, function(x) is.character(x) || is.null(x), logical(1))
  if (any(bad)) {
    msg <- unique(unlist(r[bad & vapply(r, is.character, logical(1))]))
    cat("  ", sum(bad), "member(s) failed to re-project\n")
    if (length(msg)) cat("   ", paste(head(msg, 3), collapse = " | "), "\n")
    if (all(bad)) stop("every member failed -- fix this before continuing")
  }
  r[!bad]
}

ratios <- function(res) {
  bind_rows(lapply(res, `[[`, "per_sp")) %>%
    filter(Species %in% FIT_SP, obs_tot > 0) %>%
    group_by(Species) %>%
    summarise(ratio = median(mod_tot / obs_tot, na.rm = TRUE), .groups = "drop")
}

# --------------------------------------------------------------------- fit ----
if (mode == "fit") {
  set.seed(20260803)
  sub <- sample(states, min(SUB, length(states)))
  cat("=== catchability re-fit ===\n")
  cat("started", format(Sys.time()), "| cores", CORES,
      "| subsample", length(sub), "of", length(states), "| iterations", ITERS, "\n")
  cat("fitting", length(FIT_SP), "species:", paste(FIT_SP, collapse = ", "), "\n\n")

  M <- setNames(rep(1, length(FIT_SP)), FIT_SP)
  trace <- list()
  for (it in seq_len(ITERS)) {
    t0 <- proc.time()
    res <- run_batch(sub, M)
    rt <- ratios(res)
    rm_med <- median(vapply(res, `[[`, numeric(1), "rmse"), na.rm = TRUE)
    n_cap <- sum(vapply(res, function(x) x$q_at_cap, numeric(1)))
    cat(sprintf("iteration %d | %d members | median RMSE %.4f | %.1f min | %d gear-rows at the q ceiling (%.4g)\n",
                it, length(res), rm_med, (proc.time() - t0)["elapsed"] / 60,
                n_cap, QMAX))
    print(as.data.frame(rt %>% mutate(ratio = signif(ratio, 4),
                                      M_now = signif(M[Species], 4))),
          row.names = FALSE)
    trace[[it]] <- rt %>% mutate(iter = it, M_before = M[Species], rmse_med = rm_med)
    if (it < ITERS) {
      # the 08:449-511 step: divide catchability by the modelled/observed ratio
      step <- 1 / pmax(rt$ratio, 1e-12)
      step <- pmin(pmax(step, 1 / M_STEP_CAP), M_STEP_CAP)
      M[rt$Species] <- M[rt$Species] * step
      cat("  -> new multipliers:",
          paste(sprintf("%s=%.3g", names(M), M), collapse = ", "), "\n\n")
    }
  }
  saveRDS(list(M = M, trace = bind_rows(trace), subsample = basename(sub),
               fit_species = FIT_SP, qmax = QMAX),
          fit_path)
  cat("\nWrote", fit_path, "\n")
  cat("Now: Rscript R/wmin_test/45_catchability_refit.R apply\n")
  quit(save = "no")
}

# ------------------------------------------------------------------- apply ----
stopifnot(file.exists(fit_path))
FIT <- readRDS(fit_path); M <- FIT$M
cat("=== applying the fitted multipliers to all", length(states), "members ===\n")
print(signif(M, 4))
t0 <- proc.time()
res <- run_batch(states, M)
cat(sprintf("re-projected %d members in %.1f min\n", length(res),
            (proc.time() - t0)["elapsed"] / 60))

S <- data.frame(sim_index = vapply(res, `[[`, numeric(1), "sim_index"),
                rmse = vapply(res, `[[`, numeric(1), "rmse"),
                cor_log = vapply(res, `[[`, numeric(1), "cor_log"))
P <- bind_rows(lapply(res, function(x) x$per_sp %>% mutate(sim_index = x$sim_index)))
saveRDS(list(summary = S, per_species = P, M = M),
        file.path(out_dir, "45_refit_results.rds"))
write.csv(S, file.path(out_dir, "45_refit_summary.csv"), row.names = FALSE)

old <- readRDS(file.path(out_dir, "44_rebuild_results.rds"))
oS <- old$summary %>% filter(treated_ok)
oB <- old$per_species %>% filter(arm == "treated", Species == "baleen whales") %>%
  transmute(sim_index, br_before = mod_tot / obs_tot)
nB <- P %>% filter(Species == "baleen whales") %>%
  transmute(sim_index, br_after = mod_tot / obs_tot)
cmp <- oS %>% select(sim_index, rmse_before = treated_rmse) %>%
  inner_join(S %>% select(sim_index, rmse_after = rmse), by = "sim_index") %>%
  inner_join(oB, by = "sim_index") %>% inner_join(nB, by = "sim_index")

cat("\n=== before vs after, n =", nrow(cmp), "===\n")
cat(sprintf("  yield RMSE   median %.4f -> %.4f  (best %.4f -> %.4f)\n",
            median(cmp$rmse_before), median(cmp$rmse_after),
            min(cmp$rmse_before), min(cmp$rmse_after)))
cat(sprintf("  members improved: %d of %d (%.1f%%)\n",
            sum(cmp$rmse_after < cmp$rmse_before), nrow(cmp),
            100 * mean(cmp$rmse_after < cmp$rmse_before)))
cat("\n  baleen whale catch ratio:\n")
cat("    before "); print(signif(quantile(cmp$br_before, c(0,.25,.5,.75,.9,1), na.rm=TRUE), 3))
cat("    after  "); print(signif(quantile(cmp$br_after,  c(0,.25,.5,.75,.9,1), na.rm=TRUE), 3))
for (thr in c(0.1, 0.3, 0.5, 0.69))
  cat(sprintf("    members >= %.2f : %4d -> %4d\n", thr,
              sum(cmp$br_before >= thr, na.rm = TRUE),
              sum(cmp$br_after  >= thr, na.rm = TRUE)))

cat("\n=== per-species modelled/observed totals after the re-fit ===\n")
print(as.data.frame(P %>% filter(obs_tot > 0) %>% group_by(Species) %>%
  summarise(median_ratio = signif(median(mod_tot / obs_tot), 4),
            pct_of_error = NA, .groups = "drop") %>%
  left_join(P %>% group_by(Species) %>% summarise(sse = mean(sse), .groups = "drop") %>%
              mutate(pct = round(100 * sse / sum(sse), 2)) %>% select(Species, pct),
            by = "Species")), row.names = FALSE)
cat("\nWrote 45_refit_results.rds and 45_refit_summary.csv\n")
