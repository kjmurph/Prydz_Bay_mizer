# =============================================================================
# Phase 55 -- whale interaction with the background resource reduced to 0.5
#
# THE HYPOTHESIS THIS TESTS. The krill counterfactuals (KC04/KC06) found whale
# abundance and biomass essentially unmoved by krill fishing -- the per-member
# paired difference at 2010 is -1e-6 for baleen biomass and -2e-5% for baleen
# abundance, even with peak effort held for 31 years. Kieran's proposed
# explanation: whales can switch onto the background resource at no cost.
#
# THE MODEL STRUCTURE SAYS THAT IS EXACTLY RIGHT. `plankton_forcing`
# (therMizer) PRESCRIBES n_pp from n_pp_array at every step and never subtracts
# predation, so the background resource is an undepletable buffer. And it is not
# a minor food source for whales -- measured on the phase-54 reference at its
# calibrated state, biomass-weighted over sizes:
#
#     baleen whales   Resource 43.5%   antarctic krill 40.8%
#     minke whales    Resource 49.4%   antarctic krill 43.5%
#     sperm whales    Resource  1.1%   (left alone -- nothing to gain)
#
# So roughly half of whale intake comes from a food source that no fishery can
# deplete. Halving their access to it is the intervention.
#
# THE CHANGE. species_params$interaction_resource 1 -> 0.5 for baleen whales and
# minke whales only. This enters mizerEncounter as a plain multiplier on the
# resource term (checked against the installed mizer 3.1.0 source, not from
# memory), and therMizerEncounter is a thin wrapper -- mizerEncounter * temp
# effect -- so the standard path is intact.
#
# NOT setParams(). Phase 54's set_betas() had to splice rates_funcs back after
# setParams() dropped the therMizer overrides. `species_params(p) <- sp` does
# not: verified here that Encounter stays therMizerEncounter, resource_dynamics
# stays plankton_forcing, interaction_resource is the ONLY column that moves,
# and the encounter rate of every other species is unchanged to exactly 0. The
# script asserts all four rather than trusting it.
#
# PROTOCOL, transcribed verbatim from 54_reference_model_recalibrate.R (itself
# from 51, from 42): leading steady(), then (matchBiomasses -> steady)
# alternating over the tolerance ladder 0.1 -> 0.05 -> 0.01 -> 0.002, ending on
# steady(), keeping the BEST converged state at the target tolerance rather than
# the last -- tightening the tolerance makes the biomass fit worse, so the
# per-round deviation oscillates rather than converging. A paired CONTROL arm
# puts the unchanged phase-54 reference through the identical ladder, because
# re-steadying is known to move the whale stock on its own and without the
# control there is no way to tell that apart from the interaction change.
#
# USAGE  Rscript R/wmin_test/55_whale_resource_interaction.R
# ENV    P55_BASE (default params_ref_sw2000_balror_mnkfish05.rds)
#        P55_OUT  (default params_ref_sw2000_balror_mnkfish05_whres05.rds)
#        P55_THETA_RES (default 0.5)
# =============================================================================

suppressPackageStartupMessages({library(mizer); library(therMizer)})
source(file.path("R", "check_size_params.R"))
source(file.path("R", "wmin_test", "thermizer_shim.R"))

BASE    <- Sys.getenv("P55_BASE", "params_ref_sw2000_balror_mnkfish05.rds")
OUT_RDS <- Sys.getenv("P55_OUT",
                      "params_ref_sw2000_balror_mnkfish05_whres05.rds")
THETA_RES <- as.numeric(Sys.getenv("P55_THETA_RES", "0.5"))
out_dir <- file.path("Output_large_files", "wmin_test")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
tag <- function(x) file.path(out_dir, paste0("55_", x))

BALEEN <- "baleen whales"; MINKE <- "minke whales"; KRILL <- "antarctic krill"
SPERM  <- "sperm whales"
WHALES <- c(BALEEN, MINKE)          # the two groups whose access is reduced

# Expected phase-54 values -- fail loudly on the wrong base. The baleen figure
# is 1/mean of the four rorqual ratios; it is derived here rather than typed, so
# a transcription slip cannot masquerade as a wrong base (it did on the first
# run of this script).
RORQUAL_RATIOS <- c(
  "blue whale"     = 1.15533980582524e-08,
  "fin whale"      = 2.58695652173913e-08,
  "sei whale"      = 5.28888888888889e-08,
  "humpback whale" = 4.40740740740741e-08)
EXP_BETA_BAL <- 1 / mean(RORQUAL_RATIOS)      # 29765021.620626152
EXP_BETA_SPM <- 2000
EXP_BETA_MNK <- 5042016.80672269

STEADY_TMAX     <- 1000
STEADY_PRESERVE <- c("erepro")
TOL_LADDER      <- c(0.1, 0.05, 0.01, 0.002)
STEADY_TOL      <- 0.002
MATCH_ROUNDS    <- 6
TARGET_ROUNDS   <- 14

guard <- function(f) {
  if (file.exists(f))
    stop("refusing to overwrite an existing file: ", f, call. = FALSE)
  f
}
invisible(guard(OUT_RDS)); invisible(guard(tag("control_relad.rds")))

t0 <- proc.time()
cat("=== Phase 55: whale x background-resource interaction ->", THETA_RES,
    "===\n")
cat("started", format(Sys.time()), "| mizer",
    as.character(packageVersion("mizer")), "| therMizer",
    as.character(packageVersion("therMizer")), "\n\n")

# --- load and assert the base is the phase-54 reference -----------------------
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
          isTRUE(all.equal(sp0$beta[i_mnk], EXP_BETA_MNK)),
          all(sp0$pred_kernel_type[c(i_bal, i_mnk, i_spm)] == "lognormal"))
# every group starts at 1; if the base already carries a reduced value this is
# not the object this script was written against
if (!all(sp0$interaction_resource == 1))
  stop("base already has interaction_resource != 1 for: ",
       paste(SPN[sp0$interaction_resource != 1], collapse = ", "),
       call. = FALSE)
cat("base:", BASE, "\n")
cat("  all 19 groups start at interaction_resource = 1\n")
cat(sprintf("  w_pp_cutoff %.4g g -- the resource spans %.3g to %.4g g\n\n",
            base@resource_params$w_pp_cutoff, min(base@w_full),
            max(base@w_full[base@w_full < base@resource_params$w_pp_cutoff])))

# --- helpers, transcribed from 54:145-192 -------------------------------------
bio_ratio <- function(p)
  as.numeric(getBiomass(p, use_cutoff = TRUE) / p@species_params$biomass_observed)
max_dev <- function(p) max(abs(bio_ratio(p) - 1))

steady_guarded <- function(p, tol, label) {
  no_conv <- FALSE
  out <- withCallingHandlers(
    try(steady(p, tol = tol, t_max = STEADY_TMAX, preserve = STEADY_PRESERVE,
               progress_bar = FALSE), silent = TRUE),
    message = function(m) {
      if (grepl("did not converge", conditionMessage(m), ignore.case = TRUE))
        no_conv <<- TRUE
      invokeRestart("muffleMessage")
    },
    warning = function(w) invokeRestart("muffleWarning"))
  if (inherits(out, "try-error"))
    stop("steady() errored for ", label, " at tol = ", tol, ": ",
         as.character(out))
  list(params = out, converged = !no_conv)
}
match_guarded <- function(p, label) {
  bumped <- character(0)
  out <- withCallingHandlers(
    try(matchBiomasses(p), silent = TRUE),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("smallest possible value|unrealistic value greater", msg))
        bumped <<- c(bumped, trimws(gsub("\\s+", " ", msg)))
      invokeRestart("muffleWarning")
    })
  if (inherits(out, "try-error"))
    stop("matchBiomasses() errored for ", label, ": ", as.character(out))
  list(params = out, bumped = bumped)
}
assert_invariants <- function(p, label, rung) {
  sp <- p@species_params
  bad_e <- sp$species[!(sp$erepro < 1)]
  if (length(bad_e))
    stop(label, " rung ", rung, ": erepro >= 1 for ",
         paste(bad_e, collapse = ", "), " -- abort.")
  chk <- check_size_params(p, quiet = TRUE)
  if (!all(chk$ordering_ok))
    stop(label, " rung ", rung, ": w_min < w_mat < w_max violated for ",
         paste(chk$species[!chk$ordering_ok], collapse = ", "))
  if (!identical(p@rates_funcs$Encounter, "therMizerEncounter"))
    stop(label, " rung ", rung, ": therMizer Encounter override was lost")
  if (!identical(p@resource_dynamics, "plankton_forcing"))
    stop(label, " rung ", rung, ": resource_dynamics override was lost")
  # the whole point of the phase -- it must survive every ladder rung
  if (!isTRUE(all.equal(sp$interaction_resource[i_wh],
                        p@species_params$interaction_resource[i_wh])))
    stop(label, " rung ", rung, ": interaction_resource drifted")
  invisible(TRUE)
}

# --- apply the change ---------------------------------------------------------
cat("--- parameter change ---\n")
revised <- base
sp_new <- species_params(revised)
sp_new$interaction_resource[i_wh] <- THETA_RES
species_params(revised) <- sp_new

# Assert what the setter did and did not do. species_params<- routes through
# setParams internally, and phase 54 learned the hard way that that path can
# drop the therMizer overrides.
stopifnot(identical(revised@rates_funcs$Encounter, "therMizerEncounter"),
          identical(revised@resource_dynamics, "plankton_forcing"))
a <- species_params(base); b <- species_params(revised)
moved <- intersect(names(a), names(b))
moved <- moved[vapply(moved, function(k) !isTRUE(all.equal(a[[k]], b[[k]])),
                      logical(1))]
if (!identical(moved, "interaction_resource"))
  stop("species_params<- moved more than interaction_resource: ",
       paste(moved, collapse = ", "), call. = FALSE)
cat(sprintf("  interaction_resource  %s: 1 -> %.3g\n",
            paste(WHALES, collapse = ", "), THETA_RES))
cat("  only column moved: interaction_resource | therMizer overrides intact\n")

# No other species may feel this. If one does, the change is not what it says.
e0 <- getEncounter(base); e1 <- getEncounter(revised)
d_other <- max(abs(e1[-i_wh, ] - e0[-i_wh, ]))
if (d_other != 0)
  stop("encounter changed for a non-whale species by ", d_other, call. = FALSE)
occ <- function(i) which(base@initial_n[i, ] > 0)
cat(sprintf("  encounter ratio over occupied sizes: baleen %.4f-%.4f, minke %.4f-%.4f\n",
            min(e1[i_bal, occ(i_bal)] / e0[i_bal, occ(i_bal)]),
            max(e1[i_bal, occ(i_bal)] / e0[i_bal, occ(i_bal)]),
            min(e1[i_mnk, occ(i_mnk)] / e0[i_mnk, occ(i_mnk)]),
            max(e1[i_mnk, occ(i_mnk)] / e0[i_mnk, occ(i_mnk)])))
cat(sprintf("  max |encounter change| for any other species: %g\n\n", d_other))

# --- the ladder (54:270-306, verbatim) ---------------------------------------
trace_rows <- list()
recalibrate <- function(p, label) {
  cat("--- ", label, " ---\n", sep = "")
  cat(sprintf("   start: max dev %.5f\n", max_dev(p)))
  st <- steady_guarded(p, TOL_LADDER[1], label); p <- st$params
  best <- NULL; best_d <- Inf
  for (tol in TOL_LADDER) {
    at_target <- isTRUE(all.equal(tol, STEADY_TOL))
    for (r in seq_len(if (at_target) TARGET_ROUNDS else MATCH_ROUNDS)) {
      mt <- match_guarded(p, label);       p <- mt$params
      st <- steady_guarded(p, tol, label); p <- st$params
      assert_invariants(p, label, sprintf("tol=%g r=%d", tol, r))
      d <- max_dev(p)
      if (at_target && st$converged && d < best_d) { best_d <- d; best <- p }
      trace_rows[[length(trace_rows) + 1]] <<- data.frame(
        arm = label, tol = tol, round = r, max_dev = d,
        baleen_ratio = bio_ratio(p)[i_bal], minke_ratio = bio_ratio(p)[i_mnk],
        krill_ratio = bio_ratio(p)[i_kr], sperm_ratio = bio_ratio(p)[i_spm],
        max_erepro = max(p@species_params$erepro),
        n_rmax_inf = sum(!is.finite(p@species_params$R_max)),
        converged = st$converged,
        bumped = if (length(mt$bumped)) paste(mt$bumped, collapse = " ") else "",
        stringsAsFactors = FALSE)
      cat(sprintf(paste("   tol=%-6.3g r=%-2d max dev %.5f  baleen %.4f",
                        "minke %.4f krill %.4f sperm %.4f  %s%s\n"),
                  tol, r, d, bio_ratio(p)[i_bal], bio_ratio(p)[i_mnk],
                  bio_ratio(p)[i_kr], bio_ratio(p)[i_spm],
                  if (st$converged) "conv" else "NO-CONV",
                  if (length(mt$bumped)) "  [erepro bumped]" else ""))
      flush.console()
    }
  }
  if (is.null(best))
    stop(label, ": no state converged at the target tolerance ", STEADY_TOL)
  cat(sprintf("   selected: max dev %.5f\n\n", best_d))
  best
}

p_ctl <- recalibrate(base,    "control (re-ladder, no change)")
p_rev <- recalibrate(revised, "revised (whale resource 0.5)")

# --- what changed -------------------------------------------------------------
diet_of <- function(p, label) {
  d <- ther_diet(p, n = p@initial_n, n_pp = p@initial_n_pp,
                 n_other = p@initial_n_other, year = 1841)
  E <- p@initial_n * rep(p@dw, each = nrow(p@initial_n))
  C <- apply(d, 3, function(mm) rowSums(mm * E))
  dimnames(C) <- list(predator = SPN, prey = dimnames(d)$prey)
  do.call(rbind, lapply(c(BALEEN, MINKE, SPERM), function(nm) {
    v <- C[nm, ]; tot <- sum(v)
    data.frame(state = label, predator = nm,
               total_intake_t_per_yr = tot / 1e6,
               krill_pct = 100 * v[[KRILL]] / tot,
               resource_pct = 100 * v[["Resource"]] / tot,
               zoop_pct = 100 * sum(v[c("mesozooplankton", "other krill",
                                        "other macrozooplankton", "salps")]) / tot,
               fish_pct = 100 * sum(v[c("mesopelagic fishes",
                                        "bathypelagic fishes",
                                        "shelf and coastal fishes",
                                        "toothfishes")]) / tot,
               row.names = NULL) }))
}
DIET <- rbind(diet_of(base, "base"), diet_of(p_ctl, "control"),
              diet_of(p_rev, "revised"))
cat("=== diet at the calibrated state (%) ===\n")
print(as.data.frame(DIET[order(DIET$predator, DIET$state), ]), row.names = FALSE,
      digits = 4)

BIO <- data.frame(species = SPN,
                  base_ratio = bio_ratio(base),
                  control_ratio = bio_ratio(p_ctl),
                  revised_ratio = bio_ratio(p_rev))
cat("\n=== modelled/observed biomass, cutoff basis (what matchBiomasses fits) ===\n")
print(BIO, row.names = FALSE, digits = 4)
cat(sprintf("\nmax deviation:  base %.5f  control %.5f  revised %.5f\n",
            max_dev(base), max_dev(p_ctl), max_dev(p_rev)))

REPRO <- data.frame(species = SPN,
                    base_erepro = base@species_params$erepro,
                    control_erepro = p_ctl@species_params$erepro,
                    revised_erepro = p_rev@species_params$erepro,
                    base_Rmax = base@species_params$R_max,
                    revised_Rmax = p_rev@species_params$R_max)
cat("\n=== reproduction (an OUTCOME of steady(), not a setting) ===\n")
print(REPRO, row.names = FALSE, digits = 4)

# The question this phase exists to answer, asked of the calibrated states:
# how much of whale intake is still the undepletable buffer?
cat("\n=== the headline: resource share of whale intake ===\n")
RS <- DIET[DIET$predator %in% WHALES, c("predator", "state", "resource_pct",
                                        "krill_pct")]
print(as.data.frame(RS[order(RS$predator, RS$state), ]), row.names = FALSE,
      digits = 4)
cat("\nIf the revised resource share has not fallen well below the base's",
    "\n43.5% / 49.4%, the intervention did not bite and the scenario re-run",
    "\nis not worth its cost.\n")

# --- write --------------------------------------------------------------------
saveRDS(p_rev, OUT_RDS)
saveRDS(p_ctl, tag("control_relad.rds"))
TRACE <- do.call(rbind, trace_rows)
write.csv(TRACE, tag("ladder_trace.csv"), row.names = FALSE)
write.csv(DIET,  tag("diet.csv"), row.names = FALSE)
write.csv(BIO,   tag("biomass_ratios.csv"), row.names = FALSE)
write.csv(REPRO, tag("reproduction.csv"), row.names = FALSE)
cat("\nwrote", OUT_RDS, "and", length(list.files(out_dir, "^55_")),
    "diagnostic files in", out_dir, "\n")
cat("elapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")