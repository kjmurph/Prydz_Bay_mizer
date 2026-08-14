# =============================================================================
# Phase 71 -- WHALE-ONLY abundance multiplier experiment
#
# A deliberately simple contrast to the member ensemble: perturb ONLY the whale
# groups, by a known factor, with no Monte Carlo draws at all. If whale
# recruitment can be freed to sit below R_max and respond, this is where it
# should show, because nothing else in the system is being moved at the same
# time.
#
# ------------------------------------------------------------------ the design
# ONE reference model, ONE global catchability correction, TEN runs:
#
#   groups scaled : baleen whales, minke whales, sperm whales, orca
#   multipliers   : 10, 100, 1000, 10000, 100000
#   flows         : "cap"   initial_n x s -> steady(erepro) -> recap 0.9
#                           -> R_max x s -> steady(R_max) ladder
#                   "nocap" initial_n x s -> steady(erepro)
#                           -> R_max x s -> steady(R_max) ladder
#
# R_max IS SCALED AFTER THE RECAP. setBevertonHolt(reproduction_level = L) sets
# R_max = RDD / L, so a recap applied after the scaling DESTROYS it -- that is
# exactly what happened in phase 67 (whale levels pinned at 0.90, no change in
# recruitment). The "nocap" arm removes the recap entirely, which is the cleaner
# test of whether the scaled ceiling alone frees recruitment.
#
# CATCHABILITY. The GLOBAL correction (45_catchability_multipliers) is applied to
# the reference model's own catchability, capped at QMAX. The per-member DRAWN
# catchability is NOT used -- there are no members here.
#
# NO ABUNDANCE DRAW, NO GAMMA DRAW. Every run starts from the same reference.
# The only difference between runs is the whale multiplier and the flow.
#
# USAGE  Rscript R/wmin_test/71_whale_multiplier_experiment.R
# ENV    P71_BASE, P71_MULTS (10,100,1000,10000,100000), P71_FLOWS (cap,nocap),
#        P71_CORES (10), P71_RECAP (0.9), P71_TOLS, P71_TMAX (1500)
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OUT_LARGE <- "Output_large_files/wmin_test"
BASE_FILE <- Sys.getenv("P71_BASE", "params_ref_p66_growth_cap09.rds")
MULTS <- as.numeric(trimws(strsplit(
  Sys.getenv("P71_MULTS", "10,100,1000,10000,100000"), ",")[[1]]))
FLOWS <- trimws(strsplit(Sys.getenv("P71_FLOWS", "cap,nocap"), ",")[[1]])
CORES <- as.integer(Sys.getenv("P71_CORES", "10"))
RECAP <- as.numeric(Sys.getenv("P71_RECAP", "0.9"))
TOLS  <- as.numeric(trimws(strsplit(
  Sys.getenv("P71_TOLS", "0.01,0.005,0.002,0.001"), ",")[[1]]))
STEADY_TMAX <- as.integer(Sys.getenv("P71_TMAX", "1500"))
RAMP_TMAX   <- as.integer(Sys.getenv("P71_RAMP_TMAX", "300"))
SPINUP_YEARS <- 118
QMAX <- 1
WHALES <- c("baleen whales", "minke whales", "sperm whales", "orca")
STAB <- list(cv_threshold = 0.25, check_years_tail = 40, trend_first_years = 50,
             trend_rel_slope_max = 0.025, trend_pval_max = 0.05,
             min_mean_biomass = 1)

STEM <- "71_whale_mult"
STATE_DIR <- file.path(OUT_LARGE, paste0(STEM, "_states"))
dir.create(STATE_DIR, recursive = TRUE, showWarnings = FALSE)
OUT_RDS <- file.path(OUT_LARGE, paste0(STEM, ".rds"))

t0 <- proc.time()
cat("=== Phase 71: whale-only multiplier experiment ===\n")
cat("base:", BASE_FILE, "\n")
if (!file.exists(BASE_FILE)) stop("missing ", BASE_FILE, call. = FALSE)

BASE0 <- suppressWarnings(validParams(readRDS(BASE_FILE)))
SPN <- BASE0@species_params$species
stopifnot(all(WHALES %in% SPN))
if (all(BASE0@ext_encounter == 0))
  stop("base carries no out-of-domain subsidy -- wrong reference model",
       call. = FALSE)

# --- the GLOBAL catchability correction, applied once to the reference --------
MULTQ <- readRDS(file.path(OUT_LARGE, "45_catchability_multipliers.rds"))$M
BASE <- BASE0
ext0 <- BASE@ext_encounter
gp <- gear_params(BASE)
mq <- MULTQ[match(gp$species, names(MULTQ))]; mq[is.na(mq)] <- 1
gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * mq))
gear_params(BASE) <- gp
stopifnot(isTRUE(all.equal(BASE@ext_encounter, ext0)))
cat("global catchability correction applied; krill multiplier",
    signif(MULTQ[["antarctic krill"]], 5), "\n")
cat("whale groups:", paste(WHALES, collapse = ", "), "\n")
cat("multipliers:", paste(MULTS, collapse = ", "), "| flows:",
    paste(FLOWS, collapse = ", "), "\n\n")

WH_I <- match(WHALES, SPN)
BIOM_BASE <- as.numeric(BASE@initial_n %*% (BASE@w * BASE@dw))

grid <- expand.grid(mult = MULTS, flow = FLOWS, stringsAsFactors = FALSE)
grid$run_id <- seq_len(nrow(grid))
cat("runs:", nrow(grid), "| cores:", CORES, "\n\n")

worker <- function(k) {
  suppressPackageStartupMessages({library(mizer); library(therMizer)})
  s <- grid$mult[k]; flow <- grid$flow[k]
  lab <- sprintf("mult%g_%s", s, flow)
  fail <- function(r) list(run_id = k, mult = s, flow = flow, label = lab,
                           ok = FALSE, reason = r)

  rlev <- function(p) {
    r <- try(as.numeric(getReproductionLevel(p)), silent = TRUE)
    if (inherits(r, "try-error")) rep(NA_real_, length(SPN)) else r
  }
  steady_guarded <- function(p, tol, preserve, tmax = STEADY_TMAX) {
    nc <- FALSE
    out <- withCallingHandlers(
      try(steady(p, tol = tol, t_max = tmax, preserve = preserve,
                 progress_bar = FALSE), silent = TRUE),
      message = function(m) {
        if (grepl("did not converge", conditionMessage(m), ignore.case = TRUE))
          nc <<- TRUE
        invokeRestart("muffleMessage")
      }, warning = function(w) invokeRestart("muffleWarning"))
    list(params = out, converged = !nc, errored = inherits(out, "try-error"))
  }
  scale_rmax <- function(p, idx, fac) {
    e <- p@ext_encounter
    sp <- species_params(p); sp$R_max[idx] <- sp$R_max[idx] * fac
    species_params(p) <- sp
    p@ext_encounter <- e
    p
  }

  p <- BASE
  # 1. abundance multiplier on the whale groups only
  p@initial_n[WH_I, ] <- p@initial_n[WH_I, ] * s
  # 2. settle with erepro pinned
  st <- steady_guarded(p, 0.01, "erepro", RAMP_TMAX)
  if (st$errored) return(fail("steady_erepro_error"))
  p <- st$params
  rl_pre <- rlev(p)
  # 3. recap, only in the "cap" flow
  if (flow == "cap") {
    p2 <- try(suppressWarnings(
      setBevertonHolt(p, reproduction_level = pmin(rl_pre, RECAP))),
      silent = TRUE)
    if (inherits(p2, "try-error")) return(fail("recap_error"))
    p <- p2
  }
  # 4. R_max multiplier on the whale groups -- AFTER any recap
  p <- scale_rmax(p, WH_I, s)
  if (all(p@ext_encounter == 0)) return(fail("subsidy_lost"))
  # 5. ladder under preserve = "R_max" so the scaled ceiling is held
  keep_p <- NULL; keep_tol <- NA_real_
  for (tol in TOLS) {
    st <- steady_guarded(p, tol, "R_max")
    if (st$errored) break
    p <- st$params
    if (st$converged) { keep_p <- p; keep_tol <- tol }
  }
  if (is.null(keep_p)) return(fail("no_rung_converged"))
  ps <- keep_p

  # 6. unfished spin-up + the standard stability screen
  s0 <- try(project(ps, t_start = 1841, t_max = SPINUP_YEARS, effort = 0,
                    progress_bar = FALSE), silent = TRUE)
  if (inherits(s0, "try-error")) return(fail("spinup_error"))
  init <- s0@n[SPINUP_YEARS, , ]
  bm <- getBiomass(s0); yrs <- as.numeric(rownames(bm)); nT <- nrow(bm)
  tm <- tail(bm, max(5, min(STAB$check_years_tail, nT)))
  mt <- colMeans(tm, na.rm = TRUE)
  cv <- ifelse(mt > 0, apply(tm, 2, sd, na.rm = TRUE) / mt, Inf)
  stable <- !any(cv > STAB$cv_threshold, na.rm = TRUE)

  saveRDS(list(run_id = k, mult = s, flow = flow, label = lab,
               params = ps, initial_n = init),
          file.path(STATE_DIR, sprintf("state_%s.rds", lab)))

  spq <- species_params(ps)
  b_end <- as.numeric(init %*% (ps@w * ps@dw))
  list(run_id = k, mult = s, flow = flow, label = lab, ok = TRUE,
       tightest_tol = keep_tol, stable = stable,
       max_cv = max(cv[is.finite(cv)], na.rm = TRUE),
       max_erepro = max(spq$erepro), n_erepro_ge1 = sum(spq$erepro >= 1),
       per_species = data.frame(
         run_id = k, mult = s, flow = flow, species = SPN,
         erepro = spq$erepro, R_max = spq$R_max,
         repro_level = rlev(ps), repro_level_pre = rl_pre,
         biomass_spinup = b_end, biomass_base = BIOM_BASE,
         biomass_ratio = b_end / BIOM_BASE, stringsAsFactors = FALSE))
}

cl <- makeCluster(min(CORES, nrow(grid)))
on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
clusterExport(cl, c("BASE", "SPN", "WH_I", "WHALES", "grid", "RECAP", "TOLS",
                    "STEADY_TMAX", "RAMP_TMAX", "SPINUP_YEARS", "STAB",
                    "STATE_DIR", "BIOM_BASE"), envir = environment())
res <- parLapply(cl, seq_len(nrow(grid)), worker)
stopCluster(cl)

ok <- vapply(res, function(r) isTRUE(r$ok), logical(1))
cat("succeeded:", sum(ok), "of", length(res), "\n")
if (any(!ok))
  for (r in res[!ok]) cat("  FAILED", r$label, ":", r$reason, "\n")

SUM <- do.call(rbind, lapply(res[ok], function(r) data.frame(
  label = r$label, mult = r$mult, flow = r$flow, tol = r$tightest_tol,
  stable = r$stable, max_cv = round(r$max_cv, 4),
  max_erepro = signif(r$max_erepro, 4), n_erepro_ge1 = r$n_erepro_ge1,
  stringsAsFactors = FALSE)))
PS <- do.call(rbind, lapply(res[ok], `[[`, "per_species"))
print(SUM, row.names = FALSE)

cat("\n=== whale groups: reproduction level and spin-up biomass ratio ===\n")
w <- PS %>% filter(species %in% WHALES) %>%
  transmute(flow, mult, species, repro_level = round(repro_level, 4),
            pre_cap = round(repro_level_pre, 4),
            biomass_x = round(biomass_ratio, 3),
            erepro = signif(erepro, 3)) %>%
  arrange(flow, mult, species)
print(as.data.frame(w), row.names = FALSE)

saveRDS(list(summary = SUM, per_species = PS, grid = grid,
             meta = list(base = BASE_FILE, whales = WHALES, mults = MULTS,
                         flows = FLOWS, recap = RECAP, tols = TOLS,
                         spinup_years = SPINUP_YEARS, qmax = QMAX,
                         built = Sys.time())), OUT_RDS)
write.csv(SUM, file.path(OUT_LARGE, paste0(STEM, "_summary.csv")), row.names = FALSE)
write.csv(PS, file.path(OUT_LARGE, paste0(STEM, "_per_species.csv")), row.names = FALSE)
cat("\nWROTE", OUT_RDS, "\n")
cat(sprintf("elapsed %.1f min\n", (proc.time() - t0)[["elapsed"]] / 60))