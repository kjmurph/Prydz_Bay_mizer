# =============================================================================
# PHASE 52 -- PILOT. Rebuild the top N ensemble members from the REVISED base
# params (lognormal whale feeding kernel, 51) and, paired, from the CURRENT base
# (params_sel_adj_wmin_corrected_biocal.rds, 42), following 44_rebuild_from_base.R
# exactly.
#
# THREE QUESTIONS THIS ANSWERS, and nothing else:
#   1. do the members still converge and pass the stability screen?
#   2. what happens to the manuscript's headline quantities -- yield RMSE, and the
#      Figure 4 exploited/unexploited krill consumption ratio?
#   3. how long would the full 1,848-member re-run take?
#
# IT IS A PILOT, NOT A RESULT. N = 10 is far too few for any ensemble statistic;
# medians and IQRs quoted from it mean nothing. Read it as a smoke test plus a
# timing measurement.
#
# THE PROCESS IS 44's, TRANSCRIBED. apply the three recovered draws
# (catchability, gamma, abundance_scaling) to the base -> steady(tol = 0.0025,
# t_max = 1500, preserve = "erepro") -> 118 yr unfished spin-up, 1 cycle ->
# stability screen -> 1841-2010 projection. Settings are 09_Uncertainty_Analysis
# .Rmd:1581-1591, the production values, not the function defaults. The
# non-convergence guard listens for message(), not warning() -- steady() signals
# non-convergence with message() and every legacy handler in this repo missed it.
#
# WHICH TOP N. The first N of cut A, i.e. the POST-catchability-refit unweighted
# pooled log10 yield RMSE ranking (46_selection_cuts.rds), which is the ranking the
# manuscript's 167-member decile is the head of. Note 44's own VM_LIMIT pilot
# ordered by the LEGACY deduplicated ranking instead
# (yield_rmse_per_sim_deduped.csv); those are different orderings and this script
# deliberately uses the manuscript one.
#
# BOTH CATCHABILITY BASES ARE PROJECTED, because they answer different questions:
#   drawn      -- the member's own drawn catchability. Comparable to
#                 44_rebuild_summary.csv, which is pre-refit.
#   refitted   -- drawn x the global per-species multipliers from 45, applied as
#                 46_selection_cuts.R:71-75. This is what F00/MA00 and every
#                 manuscript figure actually use.
# CAVEAT ON `refitted`: those multipliers were fitted to the CURRENT ensemble. A
# full re-run on the revised base would re-fit them (phase 45), so the refitted
# RMSE here is indicative, not final.
#
# PAIRED, ALWAYS. Only kernel - current is attributable to the feeding kernel.
#
# USAGE  Rscript R/wmin_test/52_kernel_pilot.R
# ENV: P52_N (default 10), P52_CORES (default 10),
#      P52_KERNEL_BASE (default params_whale_lognormal_kernel_bk05_mk05.rds --
#      the revised object to pilot; outputs and the state directory are tagged
#      from its filename, so each variant is kept separate)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel)
  library(dplyr); library(reshape2)
})
source("R/wmin_test/thermizer_shim.R")

out_dir <- "Output_large_files/wmin_test"   # state_dir is set with TAG, below

PEN <- "small divers"; BALEEN <- "baleen whales"; MINKE <- "minke whales"
KRILL <- "antarctic krill"
FISHES <- c("mesopelagic fishes", "bathypelagic fishes",
            "shelf and coastal fishes", "toothfishes")
# 09_Uncertainty_Analysis.Rmd:1581-1591 -- production settings, as 44 uses.
STEADY_TOL <- 0.0025; STEADY_TMAX <- 1500
SPINUP_YEARS <- 118; SPINUP_CYCLES <- 1
STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)
REF_YEARS <- 2001:2010
QMAX <- 1

N     <- as.integer(Sys.getenv("P52_N", "10"))
CORES <- as.integer(Sys.getenv("P52_CORES", "10"))
# Which revised-kernel object to pilot. The tag is derived from its filename so
# each variant gets its own outputs and its own state directory, and no run can
# silently overwrite another's.
KERNEL_BASE <- Sys.getenv("P52_KERNEL_BASE", "params_whale_lognormal_kernel_bk05_mk05.rds")
TAG <- sub("^params_whale_lognormal_kernel", "", sub("\\.rds$", "", KERNEL_BASE))
BASE_PATH <- c(current = "params_sel_adj_wmin_corrected_biocal.rds",
               kernel  = KERNEL_BASE)
ARMS <- names(BASE_PATH)
state_dir <- file.path(out_dir, sprintf("52%s_pilot_states", TAG))
dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)

guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite an existing file: ", f, call. = FALSE); f
}
OUT_RDS <- guard(file.path(out_dir, sprintf("52%s_kernel_pilot.rds", TAG)))
OUT_CSV <- guard(file.path(out_dir, sprintf("52%s_kernel_pilot_summary.csv", TAG)))

cat("=== 52 KERNEL PILOT ===\n")
cat("started", format(Sys.time()), "| N", N, "| cores", CORES,
    "| arms", paste(ARMS, collapse = ","), "\n")

# --- members: the head of cut A ----------------------------------------------
CUTS <- readRDS(file.path(out_dir, "46_selection_cuts.rds"))
cutA <- as.integer(CUTS$cuts[["A unweighted RMSE"]])
DR <- readRDS(file.path(out_dir, "43_member_draws.rds"))
members <- head(cutA, N)
missing <- setdiff(as.character(members), names(DR$draws))
if (length(missing))
  stop("no recovered draws for member(s): ", paste(missing, collapse = ", "))
if (!all(members %in% DR$members_distinct))
  stop("some pilot members are not in the draw-distinct set")
cat("members (head of cut A):", paste(members, collapse = ", "), "\n")

MULT <- readRDS(file.path(out_dir, "45_catchability_multipliers.rds"))$M
effort_arr <- readRDS("effort_array_1841_2010.rds")

# --- observed yield windows, identical to 44 / yield_rmse_evaluation.R --------
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

load_base <- function(arm) {
  p <- suppressWarnings(validParams(readRDS(BASE_PATH[[arm]])))
  stopifnot(identical(p@second_order_w$flux, "upwind"),
            isFALSE(p@second_order_w$bin_average),
            identical(p@rates_funcs$Encounter, "therMizerEncounter"),
            identical(p@resource_dynamics, "plankton_forcing"),
            p@species_params$w_min[p@species_params$species == PEN] > 1)
  kt <- as.character(p@species_params$pred_kernel_type)
  want <- if (arm == "kernel") "lognormal" else "box"
  got <- kt[p@species_params$species == BALEEN]
  if (!identical(got, want))
    stop("arm '", arm, "': baleen pred_kernel_type is '", got, "', expected '", want, "'")
  p
}
BASE <- lapply(setNames(ARMS, ARMS), load_base)
for (a in ARMS) {
  sp <- BASE[[a]]@species_params
  cat(sprintf("  base %-8s %-46s baleen kernel %-9s beta %.4g\n", a, BASE_PATH[[a]],
              as.character(sp$pred_kernel_type)[sp$species == BALEEN],
              sp$beta[sp$species == BALEEN]))
}

# ------------------------------------------------------------------ worker ----
worker <- function(j) {
  suppressPackageStartupMessages({
    library(therMizer); library(mizer); library(dplyr); library(reshape2)
  })
  source("R/wmin_test/thermizer_shim.R")
  si <- CHUNK_SIM[j]; dw_ <- CHUNK_DRAW[[j]]
  tk <- function(expr) { tt <- proc.time(); v <- force(expr)
    list(v = v, s = as.numeric((proc.time() - tt)["elapsed"])) }

  check_stability <- function(s) {
    bm <- getBiomass(s); yrs <- as.numeric(rownames(bm)); nT <- nrow(bm)
    if (nT < 5) return(list(stable = FALSE, max_cv = NA_real_))
    tm <- tail(bm, max(5, min(STAB$check_years_tail, nT)))
    hn <- max(5, min(STAB$trend_first_years, nT))
    hm <- head(bm, hn); hy <- head(yrs, hn)
    mt <- colMeans(tm, na.rm = TRUE)
    cv <- ifelse(mt > 0, apply(tm, 2, sd, na.rm = TRUE) / mt, Inf)
    rs <- setNames(rep(NA_real_, ncol(hm)), colnames(hm)); pv <- rs
    for (q in seq_len(ncol(hm))) {
      y <- hm[, q]; mu <- mean(y, na.rm = TRUE)
      if (!is.finite(mu) || mu < STAB$min_mean_biomass) { rs[q] <- 0; pv[q] <- 1; next }
      ft <- try(suppressWarnings(lm(b ~ t, data = data.frame(t = hy, b = as.numeric(y)))),
                silent = TRUE)
      if (inherits(ft, "try-error")) { rs[q] <- 0; pv[q] <- 1 } else {
        rs[q] <- as.numeric(coef(ft)[["t"]]) / mu
        pp <- try(summary(ft)$coefficients["t", "Pr(>|t|)"], silent = TRUE)
        pv[q] <- if (inherits(pp, "try-error")) 1 else as.numeric(pp)
      }
    }
    list(stable = !any((cv > STAB$cv_threshold) |
           ((abs(rs) > STAB$trend_rel_slope_max) & (pv < STAB$trend_pval_max)),
           na.rm = TRUE),
         max_cv = max(cv[is.finite(cv)], na.rm = TRUE))
  }
  yield_rmse <- function(sim) {
    d <- reshape2::melt(getYield(sim)); names(d) <- c("Year", "Species", "Yield_mod")
    d$Year <- as.numeric(as.character(d$Year)); d$Species <- as.character(d$Species)
    d$Yield_mod <- pmax(d$Yield_mod, 0)
    cmp <- obs_long %>% left_join(d, by = c("Year", "Species")) %>%
      mutate(Yield_mod = coalesce(Yield_mod, 0))
    sqrt(mean((log10(cmp$Yield_mod + 1) - log10(cmp$Yield_obs + 1))^2, na.rm = TRUE))
  }
  steady_guarded <- function(p) {
    no_conv <- FALSE
    out <- withCallingHandlers(
      try(steady(p, tol = STEADY_TOL, t_max = STEADY_TMAX,
                 preserve = c("erepro"), progress_bar = FALSE), silent = TRUE),
      message = function(m) {
        if (grepl("did not converge", conditionMessage(m), ignore.case = TRUE))
          no_conv <<- TRUE
        invokeRestart("muffleMessage")
      }, warning = function(w) invokeRestart("muffleWarning"))
    list(params = out, converged = !no_conv)
  }
  # 09:337-402, exactly as 44 applies them.
  apply_draws <- function(p) {
    gp <- gear_params(p)
    if (!identical(paste(gp$gear, gp$species), names(dw_$catchability)))
      stop("gear_params rows differ between the base and the stored member")
    gp$catchability <- as.numeric(dw_$catchability); gear_params(p) <- gp
    sp <- species_params(p)
    if (!identical(sp$species, names(dw_$gamma)))
      stop("species order differs between the base and the draw")
    sp$gamma <- as.numeric(dw_$gamma)
    species_params(p) <- sp        # rebuilds search_vol AND the pred kernel
    sc <- as.numeric(dw_$abundance_scaling[sp$species])
    for (k in seq_along(sc)) p@initial_n[k, ] <- p@initial_n[k, ] * sc[k]
    stopifnot(identical(p@rates_funcs$Encounter, "therMizerEncounter"))
    p
  }
  refit_q <- function(p) {           # 46_selection_cuts.R:71-75
    gp <- gear_params(p)
    m <- MULT[match(gp$species, names(MULT))]; m[is.na(m)] <- 1
    gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
    gear_params(p) <- gp; p
  }
  # krill consumption per predator, period mean -- MA00's protocol
  krill_cons <- function(sim, yrs) {
    p <- sim@params; spn <- p@species_params$species; tn <- as.numeric(dimnames(sim@n)$time)
    v <- rowMeans(vapply(intersect(yrs, tn), function(y) {
      ti <- which(tn == y); n <- sim@n[ti, , ]; npp <- sim@n_pp[ti, ]
      d <- ther_diet(p, n = n, n_pp = npp, n_other = p@initial_n_other, year = y)
      rowSums(d[, , KRILL] * n * rep(p@dw, each = length(spn)))
    }, numeric(length(spn))))
    setNames(v, spn)
  }

  run_arm <- function(arm) {
    tt <- list()
    p <- try(apply_draws(BASE[[arm]]), silent = TRUE)
    if (inherits(p, "try-error")) return(list(ok = FALSE, reason = "draw_error"))
    r <- tk(steady_guarded(p)); tt$steady <- r$s; st <- r$v
    if (inherits(st$params, "try-error")) return(list(ok = FALSE, reason = "steady_error"))
    if (!st$converged) return(list(ok = FALSE, reason = "steady_no_converge"))
    ps <- st$params

    r <- tk(try(project(ps, t_start = 1841, t_max = SPINUP_YEARS, effort = 0,
                        progress_bar = FALSE), silent = TRUE))
    tt$spinup <- r$s; s <- r$v
    if (inherits(s, "try-error")) return(list(ok = FALSE, reason = "spinup_error"))
    init <- s@n[SPINUP_YEARS, , ]; stab <- check_stability(s)

    r <- tk(try(project(ps, initial_n = init, t_start = 1841, effort = effort_arr,
                        progress_bar = FALSE), silent = TRUE))
    tt$proj_drawn <- r$s; pr_drawn <- r$v
    if (inherits(pr_drawn, "try-error")) return(list(ok = FALSE, reason = "projection_error"))

    pq <- refit_q(ps)
    r <- tk(try(project(pq, initial_n = init, t_start = 1841, effort = effort_arr,
                        progress_bar = FALSE), silent = TRUE))
    tt$proj_refit <- r$s; pr_refit <- r$v
    if (inherits(pr_refit, "try-error")) return(list(ok = FALSE, reason = "projection_refit_error"))

    r <- tk(try(project(pq, initial_n = init, t_start = 1841, t_max = 169,
                        effort = 0, progress_bar = FALSE), silent = TRUE))
    tt$proj_unexp <- r$s; pr_unexp <- r$v
    if (inherits(pr_unexp, "try-error")) return(list(ok = FALSE, reason = "unexploited_error"))

    r <- tk(list(e = krill_cons(pr_refit, REF_YEARS), u = krill_cons(pr_unexp, REF_YEARS)))
    tt$diet <- r$s; kc <- r$v

    saveRDS(list(sim_index = si, arm = arm, params = ps, initial_n = init),
            file.path(STATE_DIR, sprintf("state_%s_%05d.rds", arm, si)))

    spn <- ps@species_params$species; wdw <- ps@w * ps@dw
    bio_init <- as.numeric(init %*% wdw)
    grp <- function(x, g) sum(x[g])
    bm_e <- getBiomass(pr_refit); bm_u <- getBiomass(pr_unexp)
    ye <- as.numeric(rownames(bm_e)); yu <- as.numeric(rownames(bm_u))
    ab <- function(bm, y, s) mean(bm[y %in% REF_YEARS, s])

    list(ok = TRUE, reason = "ok",
         rmse_drawn = yield_rmse(pr_drawn), rmse_refit = yield_rmse(pr_refit),
         stable = isTRUE(stab$stable), max_cv = stab$max_cv,
         bw_1841 = bio_init[spn == BALEEN], total_1841 = sum(bio_init),
         kr_ratio_whales = grp(kc$e, c(BALEEN, MINKE)) / grp(kc$u, c(BALEEN, MINKE)),
         kr_ratio_all = sum(kc$e) / sum(kc$u),
         kr_ratio_fish = grp(kc$e, FISHES) / grp(kc$u, FISHES),
         kr_unexp_whales_t = grp(kc$u, c(BALEEN, MINKE)) / 1e6,
         kr_unexp_all_t = sum(kc$u) / 1e6,
         kr_whale_share_pct = 100 * grp(kc$u, c(BALEEN, MINKE)) / sum(kc$u),
         bw_ratio = ab(bm_e, ye, BALEEN) / ab(bm_u, yu, BALEEN),
         mw_ratio = ab(bm_e, ye, MINKE) / ab(bm_u, yu, MINKE),
         timings = tt)
  }

  res <- lapply(setNames(ARMS, ARMS), run_arm)
  g <- function(a, f, d = NA) if (isTRUE(res[[a]]$ok)) res[[a]][[f]] else d
  row <- data.frame(sim_index = si, stringsAsFactors = FALSE)
  for (a in ARMS) {
    row[[paste0(a, "_ok")]] <- isTRUE(res[[a]]$ok)
    row[[paste0(a, "_reason")]] <- res[[a]]$reason
    for (f in c("rmse_drawn", "rmse_refit", "stable", "max_cv", "bw_1841",
                "total_1841", "kr_ratio_whales", "kr_ratio_all", "kr_ratio_fish",
                "kr_unexp_whales_t", "kr_unexp_all_t", "kr_whale_share_pct",
                "bw_ratio", "mw_ratio"))
      row[[paste0(a, "_", f)]] <- g(a, f)
  }
  tim <- bind_rows(lapply(ARMS, function(a) {
    if (!isTRUE(res[[a]]$ok)) return(NULL)
    as.data.frame(c(list(sim_index = si, arm = a), res[[a]]$timings)) }))
  list(summary = row, timings = tim)
}

# --------------------------------------------------------------------- run ---
t0 <- proc.time()
CHUNK_SIM <- members; CHUNK_DRAW <- DR$draws[as.character(members)]
STATE_DIR <- state_dir
cl <- makeCluster(min(CORES, length(members)))
clusterExport(cl, c("PEN", "BALEEN", "MINKE", "KRILL", "FISHES", "STEADY_TOL",
                    "STEADY_TMAX", "SPINUP_YEARS", "STAB", "obs_long", "ARMS",
                    "BASE", "STATE_DIR", "CHUNK_SIM", "CHUNK_DRAW", "effort_arr",
                    "MULT", "QMAX", "REF_YEARS", "worker"), envir = environment())
R <- parLapplyLB(cl, seq_along(members), function(j)
  tryCatch(worker(j), error = function(e) {
    row <- data.frame(sim_index = CHUNK_SIM[j], stringsAsFactors = FALSE)
    for (a in ARMS) { row[[paste0(a, "_ok")]] <- FALSE
      row[[paste0(a, "_reason")]] <- paste("worker_error:", conditionMessage(e)) }
    list(summary = row, timings = NULL) }))
stopCluster(cl)
wall <- as.numeric((proc.time() - t0)["elapsed"])

S <- bind_rows(lapply(R, `[[`, "summary"))
TM <- bind_rows(lapply(R, `[[`, "timings"))
saveRDS(list(summary = S, timings = TM, members = members, arms = ARMS,
             base_path = BASE_PATH, wall_s = wall, cores = min(CORES, length(members)),
             settings = list(steady_tol = STEADY_TOL, steady_tmax = STEADY_TMAX,
                             spinup_years = SPINUP_YEARS, ref_years = REF_YEARS),
             built = Sys.time()), OUT_RDS)
write.csv(S, OUT_CSV, row.names = FALSE)

cat("\n=== outcome ===\n")
for (a in ARMS)
  cat(sprintf("  %-8s ok %d/%d | stable %d | reasons: %s\n", a,
              sum(S[[paste0(a, "_ok")]]), nrow(S),
              sum(S[[paste0(a, "_stable")]], na.rm = TRUE),
              paste(unique(S[[paste0(a, "_reason")]]), collapse = ", ")))

cat("\n=== per-member comparison ===\n")
show <- function(f, lab, d = 4) {
  x <- S[[paste0("current_", f)]]; y <- S[[paste0("kernel_", f)]]
  cat(sprintf("  %-26s current %s | kernel %s | median ratio %.4f\n", lab,
              paste(formatC(x, format = "g", digits = d), collapse = " "),
              paste(formatC(y, format = "g", digits = d), collapse = " "),
              median(y / x, na.rm = TRUE)))
}
for (v in list(c("rmse_drawn", "yield RMSE (drawn q)"),
               c("rmse_refit", "yield RMSE (refit q)"),
               c("kr_ratio_whales", "Fig4 whale E/U ratio"),
               c("kr_ratio_all", "Fig4 all-predator ratio"),
               c("kr_whale_share_pct", "whale % of krill predation"),
               c("bw_ratio", "baleen biomass E/U"),
               c("bw_1841", "baleen 1841 biomass (g)")))
  show(v[1], v[2])

cat("\n=== timing, seconds per member per arm ===\n")
st <- TM %>% group_by(arm) %>%
  summarise(across(c(steady, spinup, proj_drawn, proj_refit, proj_unexp, diet),
                   ~ round(mean(.x), 2)), .groups = "drop") %>%
  mutate(total = steady + spinup + proj_drawn + proj_refit + proj_unexp + diet)
print(as.data.frame(st), row.names = FALSE)
cat(sprintf("\nwall clock for %d members x %d arms on %d cores: %.1f s\n",
            length(members), length(ARMS), min(CORES, length(members)), wall))

per_member_1arm <- st$total[st$arm == "kernel"]
core44 <- with(st[st$arm == "kernel", ], steady + spinup + proj_drawn)
cat("\n=== extrapolation to the full draw-distinct ensemble (1,848 members) ===\n")
for (cfg in list(list("44-equivalent (steady + spinup + 1 projection)", core44),
                 list("this pilot's full set (3 projections + diet)", per_member_1arm)))
  for (nc in c(10, 14, 30))
    cat(sprintf("  %-46s %2d cores -> %5.1f h\n", cfg[[1]], nc,
                1848 * cfg[[2]] / nc / 3600))
cat("\n(30 cores is the VM, per docs/VM_RUN_MANIFEST_44.md; 14 is the local cap.)\n")
cat("\nWrote", OUT_RDS, "and", OUT_CSV, "\n")
cat("elapsed:", round(wall / 60, 1), "min\n")