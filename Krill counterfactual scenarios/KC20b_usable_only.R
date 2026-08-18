# =============================================================================
# KC20 -- FULL extraction from the phase-61 member states: biomass, abundance,
# consumption by prey, feeding level, recruitment and yield, over the four
# effort arms, in the shape KC15 / KC16 / KC18 consume
#
# KC19 deliberately extracted recruitment only, which is all KC16 reads. KC15
# needs `consumption` and KC18 needs `biomass`, so this is KC19 widened to
# KC14's full extraction. Same inputs, same arms, same traps -- it is KC14's
# worker body with the steady()/spin-up front end replaced by a stored state.
#
# WHY RE-PROJECTING A STORED STATE IS EXACT: the phase-61 spin-up is UNFISHED,
# so the stored initial_n does not depend on catchability. Applying refit_q()
# afterwards is bit-exact against having done it in one pass.
#
# THE CATCHABILITY TRAP. States carry DRAWN catchability. refit_q() MUST run
# before projecting -- the krill multiplier is 0.1904, and skipping it makes the
# krill fishery >5x too strong, which for krill counterfactuals IS the result.
#
# THE DIET/RDD TRAP. Consumption comes from ther_diet() and recruitment from
# ther_rdd(), not getDiet()/getRDD(): both default to t = 0, which under
# therMizer indexes ocean_temp at the wrong year. Encounter and feeding level
# are computed ONCE per year and shared between the two, as in KC14.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC20_full_extract_from_p61_states.R" [run|collect|status]
# ENV    KC20_STEM (phase-61 stem, no _states suffix), KC20_CORES (14),
#        KC20_FROM (1841), KC20_CHUNK
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OUT_LARGE <- "Output_large_files/wmin_test"
STEM  <- Sys.getenv("KC20_STEM", "61_ramp_n167_nomg_K1_reproduction_level")
CORES <- as.integer(Sys.getenv("KC20_CORES", "14"))
FROM  <- as.integer(Sys.getenv("KC20_FROM", "1841"))
END_YEAR <- 2010; QMAX <- 1; KRILL <- "antarctic krill"
OTHER_LTL <- c("mesozooplankton", "other krill", "other macrozooplankton",
               "salps")

STATE_DIR <- file.path(OUT_LARGE, paste0(STEM, "_states"))
SRC_RDS   <- file.path(OUT_LARGE, paste0(STEM, ".rds"))
CHUNK_DIR <- file.path(OUT_LARGE, paste0(STEM, "_KC20_chunks"))
OUT_RDS   <- file.path(OUT_LARGE, paste0(STEM, "_KC20.rds"))
dir.create(CHUNK_DIR, recursive = TRUE, showWarnings = FALSE)
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"
stopifnot(mode %in% c("run", "collect", "status"))
if (!dir.exists(STATE_DIR)) stop("no state dir: ", STATE_DIR, call. = FALSE)

t0 <- proc.time()
D <- readRDS(SRC_RDS)
cat("=== KC20: full extraction from phase-61 states ===\n")
cat("stem:", STEM, "| base:", basename(D$meta$base), "\n")

BASE <- suppressWarnings(validParams(readRDS(D$meta$base)))
SP <- BASE@species_params$species
stopifnot(!anyNA(match(OTHER_LTL, SP)), KRILL %in% SP)
# The catchability multipliers must match the ENSEMBLE being extracted. The
# phase-45 file was fitted on ensemble-44 states; a phase-104 extraction needs
# its own refit, or every arm is projected at the wrong q. KC20_MULT overrides.
MULT_F <- Sys.getenv("KC20_MULT",
                     file.path(OUT_LARGE, "45_catchability_multipliers.rds"))
if (!file.exists(MULT_F)) stop("no multiplier file: ", MULT_F, call. = FALSE)
MULT <- readRDS(MULT_F)$M
cat("multipliers:", basename(MULT_F), "\n")

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

# KC20b: process USABLE members only -- stable AND admissible, the same filter
# F00g and KC16b apply. KC20 globs every state in the directory, which on a run
# where most members fail the screens means projecting three times the data that
# any downstream figure will use. The member table travels with the run, so the
# filter is applied from the source of truth rather than from a pasted list.
if (!nzchar(Sys.getenv("KC20_ALL"))) {
  MEM <- D$members
  adm <- if ("n_erepro_ge1" %in% names(MEM)) MEM$n_erepro_ge1 == 0 else TRUE
  # follow the member table's own definition of usable: phase 104 adds a drift
  # screen, phase 88 and earlier carry no drift_ok column and are unaffected.
  drf <- if ("drift_ok" %in% names(MEM)) MEM$drift_ok else TRUE
  keep_si <- MEM$sim_index[MEM$stable & adm & drf]
  si_of <- as.integer(sub("^state_0*", "",
                          sub("\\.rds$", "", basename(states))))
  states <- states[si_of %in% keep_si]
  cat("  USABLE filter: ", nrow(MEM), " members | ", sum(MEM$stable),
      " stable | ", length(keep_si), " stable AND admissible\n", sep = "")
}
cat("  member states:", length(states), "| cores:", CORES, "\n\n")
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
  if (all(ps@ext_encounter == 0)) return(fail("no_subsidy_in_state"))

  gp <- gear_params(ps)
  m <- MULT[match(gp$species, names(MULT))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
  pq <- ps; gear_params(pq) <- gp
  q_krill <- gp$catchability[match(KRILL, gp$species)]

  dw <- pq@dw; wdw <- pq@w * pq@dw
  BIO <- CONS <- FL <- REC <- YL <- list()
  for (nm in names(ARM_EFF)) {
    e <- ARM_EFF[[nm]]
    sim <- try(if (is.null(e))
      project(pq, initial_n = init, t_start = 1841, t_max = 169, effort = 0,
              progress_bar = FALSE)
      else project(pq, initial_n = init, t_start = 1841, effort = e,
                   progress_bar = FALSE), silent = TRUE)
    if (inherits(sim, "try-error")) return(fail(paste0("projection_error_", nm)))
    yr <- as.numeric(dimnames(sim@n)$time); ki <- which(yr <= END_YEAR)

    BIO[[nm]] <- do.call(rbind, lapply(ki, function(j) data.frame(
      sim_index = si, arm = nm, Year = yr[j], Species = SP,
      Abundance = as.numeric(sim@n[j, , ] %*% dw),
      Biomass   = as.numeric(sim@n[j, , ] %*% wdw), stringsAsFactors = FALSE)))

    ys <- yr[yr >= FROM & yr <= END_YEAR]
    out <- lapply(ys, function(y) {
      ti <- which(yr == y)
      nn <- sim@n[ti, , ]; npp <- sim@n_pp[ti, ]
      no <- sim@params@initial_n_other
      te <- ther_temp_effect(sim@params, y)
      enc <- sweep(mizer::mizerEncounter(sim@params, n = nn, n_pp = npp,
                                         n_other = no, t = 0), 1, te, "*")
      flm <- mizer::mizerFeedingLevel(sim@params, n = nn, n_pp = npp,
                                      n_other = no, encounter = enc, t = 0)
      d <- ther_diet(sim@params, n = nn, n_pp = npp, n_other = no, year = y,
                     temp_eff = te, feeding_level = flm)
      E <- nn * rep(dw, each = length(SP))
      C <- apply(d, 3, function(mm) rowSums(mm * E))
      dimnames(C) <- list(SP, dimnames(d)$prey)
      r <- ther_rdd(sim@params, n = nn, n_pp = npp, n_other = no, year = y,
                    temp_eff = te, encounter = enc, feeding_level = flm)
      wt <- nn * rep(wdw, each = length(SP)); tot <- rowSums(wt)
      list(C = C, rdd = r$rdd, rdi = r$rdi,
           fl = ifelse(tot > 0, rowSums(flm * wt) / tot, NA_real_))
    })
    names(out) <- ys

    CONS[[nm]] <- do.call(rbind, lapply(ys, function(y) {
      C <- out[[as.character(y)]]$C
      data.frame(sim_index = si, arm = nm, Year = y, Species = SP,
                 total_consumed = rowSums(C),
                 krill_consumed = C[, KRILL],
                 other_ltl_consumed = rowSums(C[, OTHER_LTL, drop = FALSE]),
                 mesozoo_consumed = C[, "mesozooplankton"],
                 other_krill_consumed = C[, "other krill"],
                 other_macrozoo_consumed = C[, "other macrozooplankton"],
                 salps_consumed = C[, "salps"],
                 resource_consumed = C[, "Resource"],
                 stringsAsFactors = FALSE) }))
    FL[[nm]] <- do.call(rbind, lapply(ys, function(y) data.frame(
      sim_index = si, arm = nm, Year = y, Species = SP,
      feeding_level = out[[as.character(y)]]$fl, stringsAsFactors = FALSE)))
    REC[[nm]] <- do.call(rbind, lapply(ys, function(y) data.frame(
      sim_index = si, arm = nm, Year = y, Species = SP,
      rdd = out[[as.character(y)]]$rdd, rdi = out[[as.character(y)]]$rdi,
      stringsAsFactors = FALSE)))

    yy <- getYield(sim)
    YL[[nm]] <- data.frame(sim_index = si, arm = nm,
                           Year = as.numeric(rownames(yy))[ki],
                           krill_yield_g = as.numeric(yy[ki, KRILL]),
                           stringsAsFactors = FALSE)
  }

  spq <- species_params(ps)
  rl <- try(getReproductionLevel(ps), silent = TRUE)
  list(sim_index = si, ok = TRUE, reason = "ok", q_krill = q_krill,
       biomass = do.call(rbind, BIO), consumption = do.call(rbind, CONS),
       feeding = do.call(rbind, FL), recruit = do.call(rbind, REC),
       yield = do.call(rbind, YL),
       mech = data.frame(sim_index = si, Species = SP, gamma = spq$gamma,
         erepro = spq$erepro, R_max = spq$R_max,
         repro_level = if (inherits(rl, "try-error")) NA_real_ else as.numeric(rl),
         stringsAsFactors = FALSE))
}

# --------------------------------------------------------------------- run ---
CHUNK <- as.integer(Sys.getenv("KC20_CHUNK", as.character(CORES)))
chunks <- split(states, ceiling(seq_along(states) / CHUNK))
chunk_file <- function(ci) file.path(CHUNK_DIR, sprintf("res_%03d.rds", ci))
if (mode == "status") {
  cat(sprintf("chunks %d/%d complete\n",
              sum(vapply(seq_along(chunks), function(ci)
                file.exists(chunk_file(ci)), logical(1))), length(chunks)))
  quit(save = "no")
}
if (mode == "run") {
  todo <- which(!vapply(seq_along(chunks),
                        function(ci) file.exists(chunk_file(ci)), logical(1)))
  cat("chunks to run:", length(todo), "of", length(chunks), "\n\n")
  if (length(todo)) {
    cl <- makeCluster(CORES)
    on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
    clusterExport(cl, c("ARM_EFF", "SP", "MULT", "KRILL", "OTHER_LTL", "QMAX",
                        "FROM", "END_YEAR"), envir = environment())
    for (ci in todo) {
      tc <- proc.time()
      saveRDS(parLapply(cl, chunks[[ci]], worker), chunk_file(ci))
      cat(sprintf("[%s] chunk %d/%d done (%d members, %.1f min)\n",
                  format(Sys.time(), "%H:%M:%S"), ci, length(chunks),
                  length(chunks[[ci]]), (proc.time() - tc)[["elapsed"]] / 60))
      flush.console()
    }
    stopCluster(cl)
  }
}

res <- unlist(lapply(seq_along(chunks), function(ci) {
  f <- chunk_file(ci); if (file.exists(f)) readRDS(f) else NULL
}), recursive = FALSE)
if (!length(res)) stop("no chunk results in ", CHUNK_DIR)
ok <- vapply(res, function(r) isTRUE(r$ok), logical(1))
cat("\nextracted", length(res), "members |", sum(ok), "ok\n")
if (any(!ok)) cat("  failures:", paste(vapply(res[!ok], function(r)
  sprintf("%d(%s)", r$sim_index, r$reason), ""), collapse = ", "), "\n")
res <- res[ok]

# `members` carries the PHASE-61 stability verdict -- the screen belongs to the
# spin-up, which this script does not repeat.
MEM <- D$members[D$members$sim_index %in% vapply(res, `[[`, 0, "sim_index"), ]
saveRDS(list(
  biomass = do.call(rbind, lapply(res, `[[`, "biomass")),
  consumption = do.call(rbind, lapply(res, `[[`, "consumption")),
  feeding = do.call(rbind, lapply(res, `[[`, "feeding")),
  recruit = do.call(rbind, lapply(res, `[[`, "recruit")),
  yield = do.call(rbind, lapply(res, `[[`, "yield")),
  mech = do.call(rbind, lapply(res, `[[`, "mech")),
  members = MEM,
  meta = list(base = D$meta$base, peak_year = peak_year, source_stem = STEM,
              members = MEM$sim_index, members_stable = sum(MEM$stable),
              spinup_years = D$meta$spinup_years, diet_from = FROM,
              multipliers = MULT, other_ltl = OTHER_LTL,
              whales = c("baleen whales", "minke whales"),
              q_krill = vapply(res, `[[`, 0, "q_krill"),
              p61_meta = D$meta, built = format(Sys.time()))), OUT_RDS)

cat("\n=== tables ===\n")
cat("  stable members (from phase 61):", sum(MEM$stable), "of", nrow(MEM), "\n")
cat("  biomass rows:", nrow(do.call(rbind, lapply(res, `[[`, "biomass"))),
    "| consumption rows:",
    nrow(do.call(rbind, lapply(res, `[[`, "consumption"))), "\n")
cat("\nwrote", OUT_RDS, "\n")
cat("elapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")