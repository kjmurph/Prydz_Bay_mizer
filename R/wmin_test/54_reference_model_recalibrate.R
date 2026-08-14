# =============================================================================
# Phase 54 -- reference model parameter corrections + recalibration
#
# Lives here rather than in "Reference model diet assessment/" because it builds
# a params object at the repo root and transcribes phase 51's ladder verbatim;
# any later pilot or ensemble rebuild (the 52/53 pattern) will look for it here.
# The diagnostics that motivated every change are in RD03/RD04/RD06/RD07.
#
# FOUR CHANGES, and one deliberate non-change.
#
# 1. SPERM WHALE beta 44082.1256 -> 2000
#    Moves preferred prey from 828 g to 18.25 kg at w_max. RD06 swept beta
#    against the fixed prey field and found the squid share of the sperm whale
#    diet peaks at 15.9% near beta = 2000, against 2.3% at the value in use and
#    a diet-literature benchmark of 50-90%. Note the sweep held everything else
#    fixed; what the recalibrated model actually does is what this script tests.
#    This DEPARTS from csvs/predator_parameters_updated.csv, which gives 44,082
#    (Evans & Hindell 2004, mean prey 828 g). It is a modelling choice, not a
#    data correction -- say so in the methods.
#
# 2. BALEEN WHALE beta 2.468e7 -> 1 / mean(four rorqual ratios) = 2.9765e7
#    This IS a data correction. The original group beta of 219,113 was the
#    arithmetic mean of FIVE ratios from csvs/predator_parameters_updated.csv,
#    one of which -- Southern Right Whales -- is a corrupted cell: it duplicates
#    the sperm whale ratio exactly (mean AND SD), carries no source where the
#    sperm whale row does, and implies a 1247.67 g prey mass for a copepod
#    feeder. See RD06 section 1. Dropping it and keeping the pipeline's own
#    aggregation (mean of ratios, inverted) gives 2.9765e7.
#    The value it replaces, 2.468e7, was w_max(baleen)/w_max(krill) -- a
#    geometric argument, not a measurement. The two agree to 21%, which is the
#    useful cross-check.
#
# 3./4. INTERACTION minke whales x {mesopelagic fishes, shelf and coastal
#    fishes}: 0.0125 and 0.0069 -> 0.5 each.
#    Written SYMMETRICALLY (both halves of each pair) at Kieran's instruction,
#    keeping the matrix symmetric as constructed -- verified at 0 asymmetric
#    cells before and after. The reverse direction (fish preying on minke) is
#    inert in practice: mesopelagic fishes top out at 240 g with beta 442, so
#    their kernel places no meaningful weight on 600 kg+ prey. The script
#    asserts this rather than assuming it.
#
# NOT CHANGED
#    - squid w_max stays at 20,718.35 g. It is data-driven -- max over three
#      cephalopod taxa, Onychoteuthiids at Lmax 115 cm (Phillips 2004) -- and
#      RD07 showed raising it alone would make squid LESS available, because
#      matchBiomasses pins total squid biomass. Be explicit in the methods that
#      the taxon list excludes Cranchiidae and Architeuthidae.
#    - minke beta stays at 5,042,016.8067. VERIFIED correct: it is exactly
#      1 / 1.98333333333333e-07 from the source table, implying 1.19 g prey,
#      the same krill mass every other baleen row encodes.
#
# PROTOCOL, transcribed from 51_whale_kernel_recalibrate.R (itself from 42)
#    Leading steady(), then (matchBiomasses -> steady) alternating over a
#    tolerance ladder 0.1 -> 0.05 -> 0.01 -> 0.002, ending on steady(), keeping
#    the BEST converged state at the target tolerance rather than the last.
#    Residual read after steady(), never after matchBiomasses. A paired CONTROL
#    arm puts the unchanged base through the identical ladder, so the report
#    separates what the parameter changes did from what re-running the ladder
#    does on its own -- which is not nothing (see docs/, re-steadying is known
#    to move the whale stock).
#
# USAGE  Rscript R/wmin_test/54_reference_model_recalibrate.R
# ENV    P54_BASE (default params_whale_lognormal_kernel_bk05_mk05.rds)
#        P54_OUT  (default params_ref_sw2000_balror_mnkfish05.rds)
# =============================================================================

suppressPackageStartupMessages({library(mizer); library(therMizer)})
source(file.path("R", "check_size_params.R"))       # as 51:84
source(file.path("R", "wmin_test", "thermizer_shim.R"))

BASE    <- Sys.getenv("P54_BASE", "params_whale_lognormal_kernel_bk05_mk05.rds")
OUT_RDS <- Sys.getenv("P54_OUT",  "params_ref_sw2000_balror_mnkfish05.rds")
out_dir <- file.path("Output_large_files", "wmin_test")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
tag <- function(x) file.path(out_dir, paste0("54_", x))

BALEEN <- "baleen whales"; MINKE <- "minke whales"; KRILL <- "antarctic krill"
SPERM  <- "sperm whales"
MESO   <- "mesopelagic fishes"; SHELF <- "shelf and coastal fishes"

# The four rorqual ratios, transcribed literally from
# csvs/predator_parameters_updated.csv "preferred PPMR mean".
RORQUAL_RATIOS <- c(
  "blue whale"     = 1.15533980582524e-08,
  "fin whale"      = 2.58695652173913e-08,
  "sei whale"      = 5.28888888888889e-08,
  "humpback whale" = 4.40740740740741e-08)
BETA_BALEEN <- 1 / mean(RORQUAL_RATIOS)
BETA_SPERM  <- 2000
THETA_MNK_FISH <- 0.5

# expected starting values -- the script must fail loudly on the wrong base
EXP_BETA_BAL <- 24680000
EXP_BETA_SPM <- 44082.1256038647
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
guard(OUT_RDS); guard(tag("control_relad.rds"))

t0 <- proc.time()
cat("=== Phase 54: reference model corrections + recalibration ===\n")
cat("started", format(Sys.time()), "| mizer",
    as.character(packageVersion("mizer")), "| therMizer",
    as.character(packageVersion("therMizer")), "\n\n")

# --- load and assert the base is the one we think it is -----------------------
base <- suppressWarnings(validParams(readRDS(BASE)))
stopifnot(identical(base@second_order_w$flux, "upwind"),
          isFALSE(base@second_order_w$bin_average),
          identical(base@rates_funcs$Encounter, "therMizerEncounter"),
          identical(base@rates_funcs$RDD, "BevertonHoltRDD"),
          identical(base@resource_dynamics, "plankton_forcing"))
sp0 <- base@species_params; SPN <- sp0$species
idx <- function(nm) { i <- which(SPN == nm); stopifnot(length(i) == 1); i }
i_bal <- idx(BALEEN); i_mnk <- idx(MINKE); i_kr <- idx(KRILL)
i_spm <- idx(SPERM);  i_mes <- idx(MESO);  i_shf <- idx(SHELF)

stopifnot(isTRUE(all.equal(sp0$beta[i_bal], EXP_BETA_BAL)),
          isTRUE(all.equal(sp0$beta[i_spm], EXP_BETA_SPM)),
          isTRUE(all.equal(sp0$beta[i_mnk], EXP_BETA_MNK)),
          all(sp0$pred_kernel_type[c(i_bal, i_mnk, i_spm)] == "lognormal"),
          isTRUE(all.equal(base@interaction, t(base@interaction),
                           check.attributes = FALSE)))
cat("base:", BASE, "\n")
cat(sprintf("  minke beta %.4f == 1/1.98333333333333e-07 : %s (prey %.3f g)\n",
            sp0$beta[i_mnk],
            isTRUE(all.equal(sp0$beta[i_mnk], 1 / 1.98333333333333e-07)),
            sp0$w_max[i_mnk] * 1.98333333333333e-07))
cat(sprintf("  squid w_max %.4f g -- UNCHANGED\n", sp0$w_max[idx("squids")]))
cat(sprintf("  interaction matrix symmetric, %d asymmetric cells\n\n",
            sum(abs(base@interaction - t(base@interaction)) > 1e-12)))

# --- helpers, transcribed from 51 ---------------------------------------------
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
  invisible(TRUE)
}

# setParams() rebuilds the kernel from species_params but RESETS rates_funcs
# (dropping therMizer) and resource_dynamics. Splice back only the therMizer
# entries -- restoring the whole list would drop mizer 3.x's Diffusion entry.
set_betas <- function(p, betas) {
  rf <- p@rates_funcs; rd <- p@resource_dynamics
  sp <- p@species_params
  for (nm in names(betas)) sp$beta[which(sp$species == nm)] <- betas[[nm]]
  p@species_params <- sp
  p <- suppressWarnings(setParams(p))
  ther <- intersect(c("Encounter", "PredRate", "EReproAndGrowth"), names(rf))
  p@rates_funcs[ther] <- rf[ther]
  p@resource_dynamics <- rd
  stopifnot(identical(p@rates_funcs$Encounter, "therMizerEncounter"),
            identical(p@resource_dynamics, "plankton_forcing"))
  p
}

# --- apply the changes --------------------------------------------------------
cat("--- parameter changes ---\n")
cat(sprintf("  baleen beta  %12.6g -> %12.6g  (log10 %.4f -> %.4f)\n",
            sp0$beta[i_bal], BETA_BALEEN, log10(sp0$beta[i_bal]),
            log10(BETA_BALEEN)))
cat(sprintf("     from 1/mean(%s)\n",
            paste(sprintf("%.6g", RORQUAL_RATIOS), collapse = ", ")))
cat(sprintf("  sperm beta   %12.6g -> %12.6g  (log10 %.4f -> %.4f)\n",
            sp0$beta[i_spm], BETA_SPERM, log10(sp0$beta[i_spm]),
            log10(BETA_SPERM)))
cat(sprintf("     preferred prey at w_max %.4g g -> %.4g g\n",
            sp0$w_max[i_spm] / sp0$beta[i_spm], sp0$w_max[i_spm] / BETA_SPERM))

revised <- set_betas(base, setNames(list(BETA_BALEEN, BETA_SPERM),
                                    c(BALEEN, SPERM)))
th_mes <- revised@interaction[i_mnk, i_mes]
th_shf <- revised@interaction[i_mnk, i_shf]
revised@interaction[i_mnk, i_mes] <- revised@interaction[i_mes, i_mnk] <-
  THETA_MNK_FISH
revised@interaction[i_mnk, i_shf] <- revised@interaction[i_shf, i_mnk] <-
  THETA_MNK_FISH
cat(sprintf("  theta[minke, mesopelagic]  %.4f -> %.4f  (both halves)\n",
            th_mes, THETA_MNK_FISH))
cat(sprintf("  theta[minke, shelf&coastal] %.4f -> %.4f  (both halves)\n",
            th_shf, THETA_MNK_FISH))
stopifnot(isTRUE(all.equal(revised@interaction, t(revised@interaction),
                           check.attributes = FALSE)))
cat("  matrix still symmetric: TRUE\n")

# The reverse direction must be inert, or the symmetric write has quietly made
# mesopelagic fishes into minke whale predators. Assert it, do not assume it.
K <- getPredKernel(revised)
mnk_bins <- which(revised@w_full >= revised@species_params$w_min[i_mnk] &
                    revised@w_full <= revised@species_params$w_max[i_mnk])
for (i in c(i_mes, i_shf)) {
  occ <- which(revised@initial_n[i, ] > 0)
  phi_max <- max(K[i, occ, mnk_bins])
  cat(sprintf("  reverse check: %-24s max kernel weight on minke-sized prey = %.3g\n",
              SPN[i], phi_max))
  if (phi_max > 1e-4)
    stop(SPN[i], " can reach minke-sized prey (phi = ", phi_max,
         "); the symmetric write is NOT inert -- reconsider.")
}

# kernel sanity: peak must sit at w/beta for the two groups whose beta moved
for (nm in c(BALEEN, SPERM)) {
  i <- idx(nm)
  kb <- which.min(abs(revised@w - revised@species_params$w_max[i]))
  row <- K[i, kb, ]
  peak <- revised@w_full[which.max(row)]
  expect <- revised@w[kb] / revised@species_params$beta[i]
  cat(sprintf("  kernel check %-14s peak %.4g g, expected %.4g g\n",
              nm, peak, expect))
  if (abs(log(peak / expect)) > log(revised@w_full[2] / revised@w_full[1]))
    stop(nm, ": kernel peak is not at w/beta")
}
cat("\n")

# --- the ladder ---------------------------------------------------------------
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
        squid_ratio = bio_ratio(p)[idx("squids")],
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
    }
  }
  if (is.null(best))
    stop(label, ": no state converged at the target tolerance ", STEADY_TOL)
  cat(sprintf("   selected: max dev %.5f\n\n", best_d))
  best
}

p_ctl <- recalibrate(base,    "control (re-ladder, no change)")
p_rev <- recalibrate(revised, "revised (54 corrections)")

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
               squid_pct = 100 * v[["squids"]] / tot,
               fish_pct = 100 * sum(v[c(MESO, "bathypelagic fishes", SHELF,
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

REPRO <- data.frame(species = SPN,
                    base_erepro = base@species_params$erepro,
                    revised_erepro = p_rev@species_params$erepro,
                    base_Rmax = base@species_params$R_max,
                    revised_Rmax = p_rev@species_params$R_max)
cat("\n=== reproduction (an OUTCOME of steady(), not a setting) ===\n")
print(REPRO, row.names = FALSE, digits = 4)

# --- write --------------------------------------------------------------------
saveRDS(p_rev, OUT_RDS)
saveRDS(p_ctl, tag("control_relad.rds"))
TRACE <- do.call(rbind, trace_rows)
write.csv(TRACE, tag("ladder_trace.csv"), row.names = FALSE)
write.csv(DIET,  tag("diet.csv"), row.names = FALSE)
write.csv(BIO,   tag("biomass_ratios.csv"), row.names = FALSE)
write.csv(REPRO, tag("reproduction.csv"), row.names = FALSE)
cat("\nwrote", OUT_RDS, "and", length(list.files(out_dir, "^54_")),
    "diagnostic files in", out_dir, "\n")
cat("elapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")