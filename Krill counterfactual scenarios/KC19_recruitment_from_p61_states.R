# =============================================================================
# KC19 -- project the PHASE-61 member states through the four effort arms and
# extract annual recruitment, in the shape KC16 consumes
#
# Phase 61 stopped at steady() + spin-up: it saved params and the post-spin-up
# abundances but ran NO projections, so it carries no `recruit` table and KC16
# cannot read it. This script supplies exactly that missing step and nothing
# else -- it does not re-derive members, re-run steady(), or touch the states.
#
# WHY RE-PROJECTING A STORED STATE IS EXACT. The phase-61 spin-up is UNFISHED,
# so the stored initial_n does not depend on catchability. Applying refit_q()
# afterwards and projecting is bit-exact against having done it in one pass --
# the re-run trap that afflicts steady() does not apply here.
#
# THE CATCHABILITY TRAP. Phase-61 states carry the DRAWN catchability. refit_q()
# MUST be applied before projecting: the krill multiplier is 0.1904, so skipping
# it makes the krill fishery >5x too strong, and for krill counterfactuals that
# IS the result.
#
# THE RDD TRAP. Recruitment comes from ther_rdd() in the shim, not getRDD().
# getRDD() defaults to t = 0, which under therMizer indexes ocean_temp at the
# wrong year. Same defect as getDiet().
#
# SCOPE. Recruitment and mechanism only -- no diet, consumption or feeding
# level, which KC16 does not read. That is what makes this cheap.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC19_recruitment_from_p61_states.R"
# ENV    KC19_STEM (phase-61 stem, no _states suffix), KC19_CORES (10),
#        KC19_FROM (1841), KC19_OUT (defaults to <stem>_KC16.rds)
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OUT_LARGE <- "Output_large_files/wmin_test"
STEM  <- Sys.getenv("KC19_STEM", "61_ramp_n10_nomg_K1_reproduction_level")
CORES <- as.integer(Sys.getenv("KC19_CORES", "10"))
FROM  <- as.integer(Sys.getenv("KC19_FROM", "1841"))
END_YEAR <- 2010; QMAX <- 1; KRILL <- "antarctic krill"

STATE_DIR <- file.path(OUT_LARGE, paste0(STEM, "_states"))
SRC_RDS   <- file.path(OUT_LARGE, paste0(STEM, ".rds"))
OUT_RDS   <- Sys.getenv("KC19_OUT", file.path(OUT_LARGE, paste0(STEM, "_KC16.rds")))
if (!dir.exists(STATE_DIR)) stop("no state dir: ", STATE_DIR, call. = FALSE)
if (!file.exists(SRC_RDS))  stop("no phase-61 summary: ", SRC_RDS, call. = FALSE)

t0 <- proc.time()
D <- readRDS(SRC_RDS)
cat("=== KC19: recruitment from phase-61 states ===\n")
cat("stem:", STEM, "\n  base:", basename(D$meta$base),
    "| gamma draw:", if (isTRUE(D$meta$gamma_draw)) "ON" else "see stem",
    "| K:", D$meta$K, "\n")

BASE <- suppressWarnings(validParams(readRDS(D$meta$base)))
SP <- BASE@species_params$species
MULT <- readRDS(file.path(OUT_LARGE, "45_catchability_multipliers.rds"))$M

# --- effort arms, KC14:108-118 verbatim --------------------------------------
eff_obs <- readRDS("effort_array_1841_2010.rds")
stopifnot(identical(colnames(eff_obs), SP))
yrs_eff <- as.numeric(rownames(eff_obs)); kr_eff <- eff_obs[, KRILL]
peak_year <- yrs_eff[which.max(kr_eff)]; peak_val <- max(kr_eff)
eff_none <- eff_obs; eff_none[, KRILL] <- 0
eff_peak <- eff_obs; eff_peak[yrs_eff >= peak_year, KRILL] <- peak_val
stopifnot(identical(eff_none[, setdiff(SP, KRILL)], eff_obs[, setdiff(SP, KRILL)]),
          identical(eff_peak[, setdiff(SP, KRILL)], eff_obs[, setdiff(SP, KRILL)]))
ARM_EFF <- list(unexploited = NULL, exploited = eff_obs, no_krill = eff_none,
                peak_krill = eff_peak)
cat("  krill multiplier:", signif(MULT[[KRILL]], 5), "| peak year", peak_year, "\n")

states <- sort(list.files(STATE_DIR, pattern = "^state_\\d+\\.rds$",
                          full.names = TRUE))
cat("  member states:", length(states), "| cores:", min(CORES, length(states)),
    "\n\n")
if (!length(states)) stop("no member states found", call. = FALSE)

# ------------------------------------------------------------------ worker ---
worker <- function(f) {
  suppressPackageStartupMessages({library(mizer); library(therMizer)})
  source(file.path("R", "wmin_test", "thermizer_shim.R"))
  st <- readRDS(f); si <- st$sim_index
  fail <- function(r) list(sim_index = si, ok = FALSE, reason = r)

  ps <- st$params; init <- st$initial_n
  if (!identical(ps@rates_funcs$Encounter, "therMizerEncounter"))
    return(fail("not_thermizer"))
  # the phase-57 subsidy must have survived into the stored state
  if (all(ps@ext_encounter == 0)) return(fail("no_subsidy_in_state"))

  # refit_q -- KC14:172-177 verbatim
  gp <- gear_params(ps)
  m <- MULT[match(gp$species, names(MULT))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
  pq <- ps; gear_params(pq) <- gp
  q_krill <- gp$catchability[match(KRILL, gp$species)]

  REC <- list()
  for (nm in names(ARM_EFF)) {
    e <- ARM_EFF[[nm]]
    sim <- try(if (is.null(e))
      project(pq, initial_n = init, t_start = 1841, t_max = 169, effort = 0,
              progress_bar = FALSE)
      else project(pq, initial_n = init, t_start = 1841, effort = e,
                   progress_bar = FALSE), silent = TRUE)
    if (inherits(sim, "try-error")) return(fail(paste0("projection_error_", nm)))
    yr <- as.numeric(dimnames(sim@n)$time)
    ys <- yr[yr >= FROM & yr <= END_YEAR]
    REC[[nm]] <- do.call(rbind, lapply(ys, function(y) {
      ti <- which(yr == y)
      r <- ther_rdd(sim@params, n = sim@n[ti, , ], n_pp = sim@n_pp[ti, ],
                    n_other = sim@params@initial_n_other, year = y)
      data.frame(sim_index = si, arm = nm, Year = y, Species = SP,
                 rdd = as.numeric(r$rdd), rdi = as.numeric(r$rdi),
                 stringsAsFactors = FALSE)
    }))
  }

  spq <- species_params(ps)
  rl <- try(getReproductionLevel(ps), silent = TRUE)
  list(sim_index = si, ok = TRUE, reason = "ok", q_krill = q_krill,
       recruit = do.call(rbind, REC),
       mech = data.frame(sim_index = si, Species = SP, gamma = spq$gamma,
         erepro = spq$erepro, R_max = spq$R_max,
         repro_level = if (inherits(rl, "try-error")) NA_real_ else as.numeric(rl),
         stringsAsFactors = FALSE))
}

cl <- makeCluster(min(CORES, length(states)))
on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
clusterExport(cl, c("ARM_EFF", "SP", "MULT", "KRILL", "QMAX", "FROM",
                    "END_YEAR"), envir = environment())
res <- parLapply(cl, states, worker)
stopCluster(cl)

ok <- vapply(res, function(r) isTRUE(r$ok), logical(1))
cat("projected", length(res), "members |", sum(ok), "ok\n")
if (any(!ok)) cat("  failures:", paste(vapply(res[!ok], function(r)
  sprintf("%d(%s)", r$sim_index, r$reason), ""), collapse = ", "), "\n")
res <- res[ok]
if (!length(res)) stop("no member projected", call. = FALSE)

# `members` must carry the phase-61 stability verdict, not a fresh one -- the
# stability screen belongs to the spin-up, which this script does not repeat.
MEM <- D$members[D$members$sim_index %in% vapply(res, `[[`, 0, "sim_index"), ]
OUTL <- list(
  recruit = do.call(rbind, lapply(res, `[[`, "recruit")),
  mech    = do.call(rbind, lapply(res, `[[`, "mech")),
  members = MEM,
  meta = list(base = D$meta$base, peak_year = peak_year, source_stem = STEM,
              members = MEM$sim_index, members_stable = sum(MEM$stable),
              spinup_years = D$meta$spinup_years, diet_from = FROM,
              multipliers = MULT, q_krill = vapply(res, `[[`, 0, "q_krill"),
              p61_meta = D$meta, built = format(Sys.time())))
saveRDS(OUTL, OUT_RDS)

cat("\n=== recruitment table ===\n")
cat("  rows:", nrow(OUTL$recruit), "| arms:",
    paste(unique(OUTL$recruit$arm), collapse = ", "), "| years",
    paste(range(OUTL$recruit$Year), collapse = "-"), "\n")
cat("  stable members (from phase 61):", sum(MEM$stable), "of", nrow(MEM), "\n")
nz <- OUTL$recruit %>% group_by(Species) %>%
  summarise(n_zero_rdd = sum(rdd <= 0), .groups = "drop") %>% filter(n_zero_rdd > 0)
if (nrow(nz)) {
  cat("  WARNING -- species with non-positive RDD (mizerRDI can return zeros):\n")
  print(as.data.frame(nz), row.names = FALSE)
} else cat("  all RDD strictly positive\n")
cat("\nwrote", OUT_RDS, "\n")
cat("elapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")