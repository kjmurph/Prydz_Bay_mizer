# =============================================================================
# Phase 103 -- WHERE to scale R_max, and WHAT reproduction level to choose
#
# Two questions the phase-88 replacement cannot be written without answering.
# Both are measured on FULL 19-species member draws, not the single-species
# probe that motivated them.
#
# ---------------------------------------------------------------- background
# Measured 2026-08-16 on a clean baleen x5 probe (p100 reference, ramp at the
# FIXED budget tol 0.001 / t_max 1000):
#
#   placement       realised/drawn  repro level  R_max x  drift 200yr
#   postcap (now)       0.772          0.6453     5.457      1.0273
#   ramp                1.004          0.9030     5.028      1.0090
#   ramp_postcap        3.520          0.6444    25.140      1.0258
#   ramp_norecap        1.004          0.9030     5.028      1.0090
#
# THREE THINGS THAT PROBE SETTLED, and this phase tests whether they survive a
# full draw:
#   1. ramp + postcap DOUBLE-SCALES. R_max x 25.14 is exactly 5^2. Never do both.
#   2. ramp alone hits the draw almost exactly and leaves the steadiest state.
#   3. The recap is a NO-OP under ramp -- ramp and ramp_norecap were identical to
#      four significant figures -- because a correct ramp already leaves the
#      level at the cap.
#
# THE TRADE-OFF, and it cuts AGAINST the fix. The real difference is not the
# abundance, it is the reproduction level: 0.645 under postcap against 0.903
# under ramp. A LOWER level means recruitment is MORE sensitive to spawning
# stock (mizer's own setBevertonHolt documentation), so the current protocol's
# 0.645 -- an accident of where R_max was scaled -- is the MORE responsive
# configuration. Switching to `ramp` pins the level at RECAP and would make
# whales LESS recovery-capable, which is the opposite of the goal.
#
# What `ramp` buys is CONTROL: the level lands where RECAP puts it, by
# construction, for every member. That turns the reproduction level from an
# emergent artefact into a designed parameter -- which is why mode "recap"
# exists.
#
# ------------------------------------------------------------------ the modes
#   placement  four placements x N members, full 19-species draws. Reports
#              realised/drawn, reproduction level, R_max ratio, erepro and the
#              200-yr drift PER SPECIES.
#   recap      RECAP sweep under the `ramp` placement, then 1841-2100 under
#              three effort arms, reporting whale depletion and recovery. This
#              is the one that answers "what level actually gives recovery".
#
# A state that drifts is not steady, whatever steady() reported -- it reported
# convergence in every arm of the probe above, including the one that collapsed
# 79%. Drift is measured, never assumed.
#
# USAGE  Rscript R/wmin_test/103_rmax_placement_and_recap.R [placement|recap]
# ENV    P103_BASE, P103_N, P103_CORES, P103_DRAWS, P103_RECAPS, P103_OUT,
#        P103_RAMP_TOL, P103_RAMP_TMAX, P103_END
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OL   <- "Output_large_files/wmin_test"
BASE_FILE <- Sys.getenv("P103_BASE", "params_ref_p100_mort_kernel_diet.rds")
N     <- as.integer(Sys.getenv("P103_N", "5"))
CORES <- min(as.integer(Sys.getenv("P103_CORES", "12")),
             max(1L, parallel::detectCores() - 2L))
DRAWS_F <- Sys.getenv("P103_DRAWS",
                      file.path(OL, "87_member_draws_substituted.rds"))
OUT_DIR <- Sys.getenv("P103_OUT", file.path(OL, "103_placement"))
# the FIXED ramp budget: tol 0.01 / t_max 300 returns a state that collapses 79%
RAMP_TOL  <- as.numeric(Sys.getenv("P103_RAMP_TOL", "0.001"))
RAMP_TMAX <- as.integer(Sys.getenv("P103_RAMP_TMAX", "1000"))
LAD_TOL   <- 0.001; LAD_TMAX <- 1500L
SPINUP    <- 120L          # six whole ENSO cycles; index the LAST row
END       <- as.integer(Sys.getenv("P103_END", "2100"))
RECAPS <- as.numeric(trimws(strsplit(
  Sys.getenv("P103_RECAPS", "0.9,0.75,0.5,0.25,0.1"), ",")[[1]]))
DRIFT_YEARS <- 200L
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "placement"
stopifnot(mode %in% c("placement", "recap"))
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

t0 <- proc.time()
cat("=== Phase 103:", mode, "===\nbase:", BASE_FILE, "\n")
BASE <- suppressWarnings(validParams(readRDS(BASE_FILE)))
stopifnot(identical(BASE@rates_funcs$Encounter, "therMizerEncounter"),
          !all(BASE@ext_encounter == 0))
SPN <- BASE@species_params$species
WDW <- BASE@w * BASE@dw
REF_B    <- as.numeric(BASE@initial_n %*% WDW)
REF_RMAX <- BASE@species_params$R_max
WH <- c("minke whales", "sperm whales", "baleen whales")

DR <- readRDS(DRAWS_F)
RK <- readRDS(file.path(OL, "93_rerank_p88.rds"))$ranking
P88 <- readRDS(file.path(OL, "88_full.rds"))$members
usable <- as.integer(P88$sim_index[P88$stable & P88$n_erepro_ge1 == 0])
members <- head(as.integer(RK$sim_index[RK$sim_index %in% usable]), N)
cat("members:", paste(members, collapse = ", "), "\n")
cat("ramp budget: tol", RAMP_TOL, "t_max", RAMP_TMAX, "| cores", CORES, "\n\n")

# --- shared helpers -----------------------------------------------------------
pin_forcing <- function(p, n_years, ref_year = 1841L, t_start = 1841L) {
  o <- p@other_params$other
  k <- match(ref_year, as.numeric(rownames(o$ocean_temp)))
  yrs <- t_start + seq_len(n_years + 2L) - 1L
  p@other_params$other$ocean_temp <-
    `rownames<-`(o$ocean_temp[rep(k, length(yrs)), , drop = FALSE], yrs)
  p@other_params$other$n_pp_array <-
    `rownames<-`(o$n_pp_array[rep(k, length(yrs)), , drop = FALSE], yrs)
  p
}
# ENSO-cycle repeat for the future arms -- the protocol's own construction, and
# NOT a single pinned year, which is 0.043 degC above the cycle mean.
cycle_forcing <- function(p, fut_yrs, from = 1961L, to = 1980L) {
  o <- p@other_params$other
  ot <- o$ocean_temp; npp <- o$n_pp_array
  oyr <- as.numeric(rownames(ot))
  src <- which(oyr >= from & oyr <= to)
  idx <- src[((seq_along(fut_yrs) - 1L) %% length(src)) + 1L]
  ot2  <- rbind(ot,  ot[idx, , drop = FALSE])
  npp2 <- rbind(npp, npp[idx, , drop = FALSE])
  rownames(ot2) <- rownames(npp2) <- c(oyr, fut_yrs)
  p@other_params$other$ocean_temp <- ot2
  p@other_params$other$n_pp_array <- npp2
  p
}
scale_rmax <- function(p, f) {          # species_params<- can zero ext_encounter
  ee <- p@ext_encounter; s <- species_params(p)
  s$R_max <- s$R_max * f
  species_params(p) <- s; p@ext_encounter <- ee
  stopifnot(isTRUE(all.equal(p@ext_encounter, ee)))
  p
}
sg <- function(p, tol, tmax, pres) {    # mizer signals via message(), not warning()
  nc <- FALSE
  out <- withCallingHandlers(
    try(steady(p, tol = tol, t_max = tmax, preserve = pres,
               progress_bar = FALSE), silent = TRUE),
    message = function(m) {
      if (grepl("did not converge", conditionMessage(m), TRUE)) nc <<- TRUE
      invokeRestart("muffleMessage") })
  list(p = out, conv = !nc, err = inherits(out, "try-error"))
}
# THE MEMBER BUILD, parameterised by placement and recap level.
build <- function(si, placement, recap) {
  dw_ <- DR$draws[[as.character(si)]]
  p <- BASE
  gp <- gear_params(p)
  gp$catchability <- as.numeric(dw_$catchability); gear_params(p) <- gp
  sc <- as.numeric(dw_$abundance_scaling[SPN])
  if (any(!is.finite(sc)) || any(sc <= 0)) return(NULL)
  for (j in seq_along(sc)) p@initial_n[j, ] <- p@initial_n[j, ] * sc[j]
  if (placement %in% c("ramp", "ramp_postcap", "ramp_norecap"))
    p <- scale_rmax(p, sc)
  r <- sg(p, RAMP_TOL, RAMP_TMAX, "erepro")
  if (r$err) return(NULL)
  p <- r$p
  if (placement != "ramp_norecap" && is.finite(recap)) {
    rl <- as.numeric(getReproductionLevel(p))
    p2 <- try(suppressWarnings(setBevertonHolt(p,
      reproduction_level = pmin(rl, recap))), silent = TRUE)
    if (!inherits(p2, "try-error")) p <- p2
  }
  if (placement %in% c("postcap", "ramp_postcap")) p <- scale_rmax(p, sc)
  r2 <- sg(p, LAD_TOL, LAD_TMAX, "R_max")
  if (r2$err) return(NULL)
  list(p = r2$p, sc = sc, ramp_conv = r$conv, ladder_conv = r2$conv)
}

# ============================================================ mode: placement ==
if (mode == "placement") {
  PLACEMENTS <- c("postcap", "ramp", "ramp_postcap", "ramp_norecap")
  JOBS <- expand.grid(si = members, placement = PLACEMENTS,
                      stringsAsFactors = FALSE)
  worker <- function(k) {
    suppressPackageStartupMessages({library(mizer); library(therMizer)})
    si <- JOBS$si[k]; pl <- JOBS$placement[k]
    b <- try(build(si, pl, 0.9), silent = TRUE)
    if (inherits(b, "try-error") || is.null(b)) return(NULL)
    p <- b$p
    s <- try(project(pin_forcing(p, DRIFT_YEARS), t_start = 1841,
                     t_max = DRIFT_YEARS, effort = 0, progress_bar = FALSE),
             silent = TRUE)
    if (inherits(s, "try-error")) return(NULL)
    bm <- apply(s@n, 1, function(n) rowSums(sweep(n, 2, WDW, "*")))
    data.frame(sim_index = si, placement = pl, species = SPN,
      drawn = b$sc, realised = as.numeric(p@initial_n %*% WDW) / REF_B,
      repro_level = as.numeric(getReproductionLevel(p)),
      R_max_x = p@species_params$R_max / REF_RMAX,
      erepro = p@species_params$erepro,
      drift = bm[, ncol(bm)] / bm[, 1],
      ramp_conv = b$ramp_conv, ladder_conv = b$ladder_conv,
      stringsAsFactors = FALSE)
  }
  cl <- makeCluster(CORES)
  clusterEvalQ(cl, suppressPackageStartupMessages({
    library(mizer); library(therMizer) }))
  clusterExport(cl, c("BASE","SPN","WDW","REF_B","REF_RMAX","DR","JOBS",
                      "RAMP_TOL","RAMP_TMAX","LAD_TOL","LAD_TMAX","DRIFT_YEARS",
                      "pin_forcing","scale_rmax","sg","build","worker"))
  RES <- parLapplyLB(cl, seq_len(nrow(JOBS)), function(k)
    tryCatch(worker(k), error = function(e) NULL))
  stopCluster(cl)
  D <- bind_rows(RES)
  saveRDS(D, file.path(OUT_DIR, "103_placement.rds"))
  cat("=== realised / drawn, median over members ===\n")
  print(as.data.frame(D %>% group_by(placement, species) %>%
    summarise(ratio = round(median(realised / drawn), 3), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = placement, values_from = ratio) %>%
    slice(match(SPN, species))), row.names = FALSE)
  cat("\n=== the whale groups: level, R_max x, drift ===\n")
  print(as.data.frame(D %>% filter(species %in% WH) %>%
    group_by(placement, species) %>%
    summarise(repro_level = round(median(repro_level), 4),
              R_max_x = signif(median(R_max_x), 4),
              realised_over_drawn = round(median(realised / drawn), 3),
              drift = round(median(drift), 4), .groups = "drop")),
    row.names = FALSE)
  cat("\n=== admissibility and drift, all species ===\n")
  print(as.data.frame(D %>% group_by(placement) %>%
    summarise(members = n_distinct(sim_index),
              n_erepro_ge1 = sum(erepro >= 1),
              worst_drift_lo = round(min(drift), 3),
              worst_drift_hi = round(max(drift), 3),
              ramp_conv = sum(ramp_conv) / n() * 19,
              .groups = "drop")), row.names = FALSE)
}

# ================================================================ mode: recap ==
if (mode == "recap") {
  eff_obs <- readRDS("effort_array_1841_2010.rds")
  yrs_obs <- as.numeric(rownames(eff_obs)); fut <- (max(yrs_obs) + 1):END
  GEARS <- colnames(eff_obs)
  mk_eff <- function(v) {
    m <- rbind(eff_obs, matrix(rep(v, each = length(fut)), nrow = length(fut)))
    rownames(m) <- c(yrs_obs, fut); colnames(m) <- GEARS; m
  }
  ZERO <- setNames(rep(0, length(GEARS)), GEARS)
  EFF <- list(none = mk_eff(ZERO), unfished = { m <- mk_eff(ZERO); m[] <- 0; m })
  MULT <- readRDS(file.path(OL, "89_refit_results.rds"))$M
  JOBS <- expand.grid(si = members, recap = RECAPS)
  worker <- function(k) {
    suppressPackageStartupMessages({library(mizer); library(therMizer)})
    si <- JOBS$si[k]; rc <- JOBS$recap[k]
    b <- try(build(si, "ramp", rc), silent = TRUE)
    if (inherits(b, "try-error") || is.null(b)) return(NULL)
    p <- b$p
    gp <- gear_params(p)
    m <- MULT[match(gp$species, names(MULT))]; m[is.na(m)] <- 1
    gp$catchability <- pmin(1, pmax(0, gp$catchability * m)); gear_params(p) <- gp
    # SPIN-UP: 120 yr = six whole ENSO cycles, and index the LAST row. Phase 88
    # used t_max 118 and row 118, which is year 1958 -- 117 elapsed years landing
    # at cycle position 17 of 20.
    s0 <- try(project(p, t_start = 1841, t_max = SPINUP, effort = 0,
                      progress_bar = FALSE), silent = TRUE)
    if (inherits(s0, "try-error")) return(NULL)
    init <- s0@n[dim(s0@n)[1], , ]
    pf <- cycle_forcing(p, fut)
    out <- lapply(names(EFF), function(a) {
      s <- try(project(pf, initial_n = init, t_start = 1841, effort = EFF[[a]],
                       progress_bar = FALSE), silent = TRUE)
      if (inherits(s, "try-error")) return(NULL)
      bm <- apply(s@n, 1, function(n) rowSums(sweep(n, 2, WDW, "*")))
      yr <- as.numeric(dimnames(s@n)$time)
      data.frame(arm = a, Year = rep(yr, each = length(SPN)),
                 species = rep(SPN, times = length(yr)),
                 biomass = as.vector(bm), stringsAsFactors = FALSE)
    })
    if (any(vapply(out, is.null, logical(1)))) return(NULL)
    # do.call(rbind), NOT dplyr::bind_rows -- the workers load mizer and
    # therMizer only, and a dplyr call here fails every job silently.
    list(traj = cbind(sim_index = si, recap = rc, do.call(rbind, out)),
         pars = data.frame(sim_index = si, recap = rc, species = SPN,
           repro_level = as.numeric(getReproductionLevel(p)),
           erepro = p@species_params$erepro, stringsAsFactors = FALSE))
  }
  cl <- makeCluster(CORES)
  clusterEvalQ(cl, suppressPackageStartupMessages({
    library(mizer); library(therMizer) }))
  clusterExport(cl, c("BASE","SPN","WDW","REF_B","REF_RMAX","DR","JOBS","EFF",
                      "MULT","SPINUP","fut","RAMP_TOL","RAMP_TMAX","LAD_TOL",
                      "LAD_TMAX","cycle_forcing","scale_rmax","sg","build",
                      "worker"))
  RES <- parLapplyLB(cl, seq_len(nrow(JOBS)), function(k)
    tryCatch(worker(k), error = function(e) NULL))
  stopCluster(cl)
  RES <- RES[!vapply(RES, is.null, logical(1))]
  if (!length(RES)) stop("every recap job failed", call. = FALSE)
  TR <- bind_rows(lapply(RES, `[[`, "traj"))
  PA <- bind_rows(lapply(RES, `[[`, "pars"))
  saveRDS(list(traj = TR, pars = PA), file.path(OUT_DIR, "103_recap.rds"))
  U <- TR %>% filter(arm == "unfished") %>% select(sim_index, recap, Year,
                                                   species, unf = biomass)
  S <- TR %>% filter(arm == "none") %>% inner_join(U,
        by = c("sim_index","recap","Year","species")) %>%
    mutate(rel = biomass / unf) %>% filter(Year %in% c(2010, END)) %>%
    select(sim_index, recap, species, Year, rel) %>%
    tidyr::pivot_wider(names_from = Year, values_from = rel,
                       names_prefix = "y") %>%
    mutate(gap_closed = (.data[[paste0("y", END)]] - y2010) / (1 - y2010))
  cat("=== depletion at 2010 and recovery by", END, "under `ramp` ===\n")
  print(as.data.frame(S %>% filter(species %in% WH) %>%
    group_by(recap, species) %>%
    summarise(depletion_2010 = round(median(y2010), 4),
              rel_2100 = round(median(.data[[paste0("y", END)]]), 4),
              gap_closed_pct = round(100 * median(gap_closed), 1),
              .groups = "drop") %>% arrange(species, desc(recap))),
    row.names = FALSE)
  cat("\n=== realised reproduction level and erepro ===\n")
  print(as.data.frame(PA %>% filter(species %in% WH) %>%
    group_by(recap, species) %>%
    summarise(level = round(median(repro_level), 4),
              erepro = signif(median(erepro), 3),
              n_erepro_ge1 = sum(erepro >= 1), .groups = "drop") %>%
    arrange(species, desc(recap))), row.names = FALSE)
}
cat("\nelapsed", round((proc.time() - t0)[["elapsed"]] / 60, 2), "min\n")
cat("Phase 103 complete.\n")