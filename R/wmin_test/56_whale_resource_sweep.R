# =============================================================================
# Phase 56 -- how far can whale access to the background resource be reduced
# before the reference model stops calibrating?
#
# THE QUESTION. Whale abundance and body size are unmoved by krill fishing even
# with satiation and the reproduction clamp both relieved (KC10/KC11). getRDD()
# says recruitment numbers are not obviously unrealistic, which points at PREY
# SWITCHING: `plankton_forcing` PRESCRIBES n_pp from n_pp_array every step and
# never subtracts predation, so the background resource is an undepletable
# buffer, and the model has no prey-quality distinction to penalise switching
# onto it. Phase 55 halved whale access (1 -> 0.5) and the counterfactual result
# barely moved. This phase asks for the EXTREME case: can it go to ZERO?
#
# A SWEEP, NOT A SINGLE VALUE. Kieran's instruction: try 0, and if the model
# will not steady and recalibrate there, work down from the known-good 0.5 until
# it does. This runs the whole ladder in ascending order and reports the LOWEST
# theta that calibrates, so one run answers both questions and also gives the
# dose-response between them.
#
# PROTOCOL is 55_whale_resource_interaction.R transcribed verbatim -- leading
# steady(), then (matchBiomasses -> steady) over the tolerance ladder
# 0.1 -> 0.05 -> 0.01 -> 0.002, keeping the BEST converged state at the target
# tolerance rather than the last. Two differences, both deliberate:
#
#   1. The paired CONTROL arm runs ONCE, not once per theta. It is the same
#      unchanged phase-54 base through the same ladder every time, so running it
#      six times would cost six times as much and answer the same question.
#   2. A theta that fails is RECORDED AND SKIPPED rather than aborting the run,
#      because "it fails at 0" is the result, not an error.
#
# Every arm starts from the PHASE-54 base (interaction_resource all 1), never
# from phase 55, so the thetas are directly comparable to each other and to the
# published 0.5.
#
# USAGE  Rscript R/wmin_test/56_whale_resource_sweep.R
# ENV    P56_BASE, P56_THETAS (comma separated, ascending), P56_SKIP_CONTROL
# =============================================================================

suppressPackageStartupMessages({library(mizer); library(therMizer)})
source(file.path("R", "check_size_params.R"))
source(file.path("R", "wmin_test", "thermizer_shim.R"))

BASE <- Sys.getenv("P56_BASE", "params_ref_sw2000_balror_mnkfish05.rds")
THETAS <- as.numeric(trimws(strsplit(
  Sys.getenv("P56_THETAS", "0,0.05,0.1,0.2,0.3,0.4"), ",")[[1]]))
SKIP_CONTROL <- nzchar(Sys.getenv("P56_SKIP_CONTROL"))
out_dir <- file.path("Output_large_files", "wmin_test")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
tag <- function(x) file.path(out_dir, paste0("56_", x))
theta_file <- function(th)
  sprintf("params_ref_sw2000_balror_mnkfish05_whres%s.rds",
          sub("\\.", "", formatC(th, format = "f", digits = 2)))

BALEEN <- "baleen whales"; MINKE <- "minke whales"; KRILL <- "antarctic krill"
SPERM  <- "sperm whales"
WHALES <- c(BALEEN, MINKE)
ZOOP4  <- c("mesozooplankton", "other krill", "other macrozooplankton", "salps")
FISH4  <- c("mesopelagic fishes", "bathypelagic fishes",
            "shelf and coastal fishes", "toothfishes")

RORQUAL_RATIOS <- c(
  "blue whale"     = 1.15533980582524e-08,
  "fin whale"      = 2.58695652173913e-08,
  "sei whale"      = 5.28888888888889e-08,
  "humpback whale" = 4.40740740740741e-08)
EXP_BETA_BAL <- 1 / mean(RORQUAL_RATIOS)
EXP_BETA_SPM <- 2000
EXP_BETA_MNK <- 5042016.80672269

STEADY_TMAX     <- 1000
STEADY_PRESERVE <- c("erepro")
TOL_LADDER      <- c(0.1, 0.05, 0.01, 0.002)
STEADY_TOL      <- 0.002
MATCH_ROUNDS    <- 6
TARGET_ROUNDS   <- 14

t0 <- proc.time()
cat("=== Phase 56: whale x background-resource sweep ===\n")
cat("thetas:", paste(THETAS, collapse = ", "), "| base:", BASE, "\n")
cat("started", format(Sys.time()), "| mizer",
    as.character(packageVersion("mizer")), "| therMizer",
    as.character(packageVersion("therMizer")), "\n\n")

base <- suppressWarnings(validParams(readRDS(BASE)))
stopifnot(identical(base@second_order_w$flux, "upwind"),
          isFALSE(base@second_order_w$bin_average),
          identical(base@rates_funcs$Encounter, "therMizerEncounter"),
          identical(base@rates_funcs$RDD, "BevertonHoltRDD"),
          identical(base@resource_dynamics, "plankton_forcing"))
sp0 <- base@species_params; SPN <- sp0$species
idx <- function(nm) { i <- which(SPN == nm); stopifnot(length(i) == 1); i }
i_bal <- idx(BALEEN); i_mnk <- idx(MINKE); i_kr <- idx(KRILL); i_spm <- idx(SPERM)
i_wh <- c(i_bal, i_mnk)
stopifnot(isTRUE(all.equal(sp0$beta[i_bal], EXP_BETA_BAL)),
          isTRUE(all.equal(sp0$beta[i_spm], EXP_BETA_SPM)),
          isTRUE(all.equal(sp0$beta[i_mnk], EXP_BETA_MNK)))
if (!all(sp0$interaction_resource == 1))
  stop("base already has interaction_resource != 1 -- wrong base", call. = FALSE)
cat("base checks passed: all 19 groups at interaction_resource = 1\n\n")

# --- helpers, transcribed from 55:131-184 -------------------------------------
bio_ratio <- function(p)
  as.numeric(getBiomass(p, use_cutoff = TRUE) / p@species_params$biomass_observed)
max_dev <- function(p) max(abs(bio_ratio(p) - 1))

steady_guarded <- function(p, tol) {
  no_conv <- FALSE
  out <- withCallingHandlers(
    try(steady(p, tol = tol, t_max = STEADY_TMAX, preserve = STEADY_PRESERVE,
               progress_bar = FALSE), silent = TRUE),
    message = function(m) {
      if (grepl("did not converge", conditionMessage(m), ignore.case = TRUE))
        no_conv <<- TRUE
      invokeRestart("muffleMessage")
    }, warning = function(w) invokeRestart("muffleWarning"))
  list(params = out, converged = !no_conv,
       errored = inherits(out, "try-error"))
}
match_guarded <- function(p) {
  bumped <- character(0)
  out <- withCallingHandlers(
    try(matchBiomasses(p), silent = TRUE),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("smallest possible value|unrealistic value greater", msg))
        bumped <<- c(bumped, trimws(gsub("\\s+", " ", msg)))
      invokeRestart("muffleWarning")
    })
  list(params = out, bumped = bumped, errored = inherits(out, "try-error"))
}
# Returns a reason string on failure instead of stopping: at theta = 0 a failure
# IS the answer, so it must be recorded and the sweep must continue.
check_invariants <- function(p, theta) {
  sp <- p@species_params
  if (any(!(sp$erepro < 1)))
    return(paste0("erepro >= 1 for ",
                  paste(sp$species[!(sp$erepro < 1)], collapse = ", ")))
  chk <- check_size_params(p, quiet = TRUE)
  if (!all(chk$ordering_ok)) return("w_min < w_mat < w_max violated")
  if (!identical(p@rates_funcs$Encounter, "therMizerEncounter"))
    return("therMizer Encounter override lost")
  if (!identical(p@resource_dynamics, "plankton_forcing"))
    return("resource_dynamics override lost")
  if (!isTRUE(all.equal(sp$interaction_resource[i_wh], rep(theta, 2))))
    return("interaction_resource drifted")
  NA_character_
}

trace_rows <- list()
recalibrate <- function(p, label, theta) {
  cat("--- ", label, " ---\n", sep = "")
  cat(sprintf("   start: max dev %.5f\n", max_dev(p)))
  st <- steady_guarded(p, TOL_LADDER[1])
  if (st$errored) { cat("   steady() ERRORED on the leading call\n\n")
    return(list(params = NULL, reason = "steady_error_leading")) }
  p <- st$params
  best <- NULL; best_d <- Inf
  for (tol in TOL_LADDER) {
    at_target <- isTRUE(all.equal(tol, STEADY_TOL))
    for (r in seq_len(if (at_target) TARGET_ROUNDS else MATCH_ROUNDS)) {
      mt <- match_guarded(p)
      if (mt$errored) { cat("   matchBiomasses() ERRORED\n\n")
        return(list(params = NULL, reason = "match_error")) }
      p <- mt$params
      st <- steady_guarded(p, tol)
      if (st$errored) { cat("   steady() ERRORED\n\n")
        return(list(params = NULL, reason = "steady_error")) }
      p <- st$params
      bad <- check_invariants(p, theta)
      if (!is.na(bad)) { cat("   invariant broken: ", bad, "\n\n", sep = "")
        return(list(params = NULL, reason = bad)) }
      d <- max_dev(p)
      if (at_target && st$converged && d < best_d) { best_d <- d; best <- p }
      trace_rows[[length(trace_rows) + 1]] <<- data.frame(
        arm = label, theta = theta, tol = tol, round = r, max_dev = d,
        baleen_ratio = bio_ratio(p)[i_bal], minke_ratio = bio_ratio(p)[i_mnk],
        krill_ratio = bio_ratio(p)[i_kr],
        max_erepro = max(p@species_params$erepro),
        converged = st$converged, stringsAsFactors = FALSE)
      cat(sprintf(paste("   tol=%-6.3g r=%-2d max dev %.5f  baleen %.4f",
                        "minke %.4f krill %.4f  %s%s\n"),
                  tol, r, d, bio_ratio(p)[i_bal], bio_ratio(p)[i_mnk],
                  bio_ratio(p)[i_kr], if (st$converged) "conv" else "NO-CONV",
                  if (length(mt$bumped)) "  [erepro bumped]" else ""))
      flush.console()
    }
  }
  if (is.null(best)) {
    cat("   no state converged at the target tolerance\n\n")
    return(list(params = NULL, reason = "no_converged_state_at_target"))
  }
  cat(sprintf("   selected: max dev %.5f\n\n", best_d))
  list(params = best, reason = NA_character_, max_dev = best_d)
}

# --- the control, once --------------------------------------------------------
p_ctl <- NULL
if (!SKIP_CONTROL && !file.exists(tag("control_relad.rds"))) {
  r <- recalibrate(base, "control (re-ladder, no change)", 1)
  p_ctl <- r$params
  if (!is.null(p_ctl)) saveRDS(p_ctl, tag("control_relad.rds"))
} else if (file.exists(tag("control_relad.rds"))) {
  p_ctl <- readRDS(tag("control_relad.rds"))
  cat("control: reusing", tag("control_relad.rds"), "\n\n")
}

# --- the sweep ----------------------------------------------------------------
diet_of <- function(p, label, theta) {
  d <- ther_diet(p, n = p@initial_n, n_pp = p@initial_n_pp,
                 n_other = p@initial_n_other, year = 1841)
  E <- p@initial_n * rep(p@dw, each = nrow(p@initial_n))
  C <- apply(d, 3, function(mm) rowSums(mm * E))
  dimnames(C) <- list(predator = SPN, prey = dimnames(d)$prey)
  do.call(rbind, lapply(c(BALEEN, MINKE, SPERM), function(nm) {
    v <- C[nm, ]; tot <- sum(v)
    data.frame(state = label, theta = theta, predator = nm,
               total_intake_t_per_yr = tot / 1e6,
               krill_pct = 100 * v[[KRILL]] / tot,
               resource_pct = 100 * v[["Resource"]] / tot,
               other_ltl_pct = 100 * sum(v[ZOOP4]) / tot,
               fish_pct = 100 * sum(v[FISH4]) / tot, row.names = NULL) }))
}

RES <- list(); DIET <- list(); FEED <- list()
for (th in THETAS) {
  cat("############ theta =", th, "############\n")
  revised <- base
  spn <- species_params(revised)
  spn$interaction_resource[i_wh] <- th
  species_params(revised) <- spn
  stopifnot(identical(revised@rates_funcs$Encounter, "therMizerEncounter"),
            identical(revised@resource_dynamics, "plankton_forcing"))
  r <- recalibrate(revised, sprintf("revised (whale resource %g)", th), th)
  ok <- !is.null(r$params)
  RES[[length(RES) + 1]] <- data.frame(
    theta = th, calibrated = ok,
    reason = if (ok) "ok" else r$reason,
    max_dev = if (ok) r$max_dev else NA_real_,
    baleen_ratio = if (ok) bio_ratio(r$params)[i_bal] else NA_real_,
    minke_ratio  = if (ok) bio_ratio(r$params)[i_mnk] else NA_real_,
    max_erepro   = if (ok) max(r$params@species_params$erepro) else NA_real_,
    stringsAsFactors = FALSE)
  if (ok) {
    f <- theta_file(th)
    if (file.exists(f)) cat("  NOTE: not overwriting existing", f, "\n")
    else saveRDS(r$params, f)
    cat("  wrote", f, "\n")
    DIET[[length(DIET) + 1]] <- diet_of(r$params, sprintf("theta=%g", th), th)
    fl <- getFeedingLevel(r$params)
    wt <- r$params@initial_n * r$params@w * r$params@dw
    FEED[[length(FEED) + 1]] <- data.frame(
      theta = th, species = SPN,
      feeding_level = rowSums(fl * wt) / rowSums(wt),
      repro_level = as.numeric(getReproductionLevel(r$params)))
  }
  cat("\n")
}

SUM <- do.call(rbind, RES)
cat("=== sweep summary ===\n")
print(SUM, row.names = FALSE, digits = 4)
lowest <- if (any(SUM$calibrated)) min(SUM$theta[SUM$calibrated]) else NA
cat(sprintf("\nLOWEST theta that calibrates: %s\n",
            if (is.na(lowest)) "NONE in this sweep" else
              sprintf("%g  ->  %s", lowest, theta_file(lowest))))

if (length(DIET)) {
  D <- do.call(rbind, c(list(diet_of(base, "base (theta=1)", 1)), DIET))
  cat("\n=== diet at the calibrated state (% of intake) ===\n")
  print(as.data.frame(D[order(D$predator, D$theta), ]), row.names = FALSE,
        digits = 4)
  write.csv(D, tag("diet.csv"), row.names = FALSE)
}
if (length(FEED)) {
  FF <- do.call(rbind, FEED)
  cat("\n=== whale feeding and reproduction level by theta ===\n")
  print(as.data.frame(FF[FF$species %in% c(WHALES, KRILL), ]), row.names = FALSE,
        digits = 4)
  write.csv(FF, tag("feeding_repro.csv"), row.names = FALSE)
}
write.csv(SUM, tag("sweep_summary.csv"), row.names = FALSE)
write.csv(do.call(rbind, trace_rows), tag("ladder_trace.csv"), row.names = FALSE)
cat("\nelapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")
cat("Phase 56 complete.\n")