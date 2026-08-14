# =============================================================================
# PHASE 51 -- rebuild the base params with a LOGNORMAL feeding kernel for baleen
# and minke whales, a corrected baleen beta, and a stronger baleen-krill
# interaction, then recalibrate to the 2001-2010 observed biomass.
#
# WHY. Baleen and minke whales are the only two of the 19 groups carrying
# `pred_kernel_type = "box"`, with ppmr_min = 1e5 and ppmr_max = 5e6. That window
# is a MINKE calibration: applying the two natural rules to minke whales gives
# ppmr_max = w_max/1 g = 6.0e6 and ppmr_min = w_min/4.45 g = 1.35e5, which is
# essentially the implemented pair. Baleen whales have a w_max 17x larger, so the
# same fixed ratio slides their window a decade ABOVE the krill spectrum: a 103 t
# baleen whale feeds on 20.6-1030 g prey, and Antarctic krill tops out at 4.17 g.
# The consequence, measured on the 167-member fitted ensemble, is that baleen
# whales take 0.0013% of community krill predation and 0.014% of their own intake
# as krill -- see `Manuscript analysis/SUMMARY.md` sections A and F.
#
# THE CHANGES, all of them Kieran's decision:
#   1. baleen whales  pred_kernel_type  "box" -> "lognormal"
#   2. minke whales   pred_kernel_type  "box" -> "lognormal"  (beta unchanged)
#   3. baleen whales  beta  219112.6 -> 2.468e7
#   4. interaction[baleen whales, antarctic krill] -- K51_THETA_BK, default 0.2
#   5. interaction[minke whales,  antarctic krill] -- K51_THETA_MK, default 0.05
#      Both defaults are the base values, so by default the matrix is untouched
#      and the kernel is the only change. Both are written SYMMETRICALLY.
#
# WHY MINKE IS A SEPARATE KNOB, AND WHY ITS BASE VALUE IS SUSPECT. In the base
# object the ENTIRE minke interaction row is exactly one quarter of the baleen
# row -- every prey, ratio 4.000 -- because the matrix is a water-column overlap
# construction and minke whales carry max_depth = 50 m against baleen's 200 m.
# The result is that minke whales have the JOINT LOWEST krill interaction of all
# 19 groups (0.05, tied with flying birds), which inverts the real ordering:
# Antarctic minke whales are effectively obligate euphausiid feeders (Ichii &
# Kato 1991, Polar Biology 11:479-487). Raising baleen alone would make baleen
# 10x more krill-coupled than minke and deepen that inversion, so the two are
# exposed as separate parameters and Kieran set both to 0.5 (2026-08-09).
# The root cause is minke's 50 m max_depth; correcting THAT would rescale the
# whole row from the habitat logic rather than krill alone, and is not done here.
#
# HISTORY OF THE INTERACTION TERM. The first run of this script used baleen 0.5
# applied to [baleen, krill] ONLY, which broke the matrix's symmetry. Kieran
# reverted it on review. That variant is preserved as
# `params_whale_lognormal_kernel_theta05_asym.rds` with diagnostics under
# `51_theta05_asym_*`; it is NOT reproducible from this script as it now stands,
# because this version always writes both halves of every pair.
#
# WHERE 2.468e7 COMES FROM. It is w_max(baleen) / w_max(krill) =
# 1.03e8 / 4.173206 = 24,681,262, i.e. the value that puts the preferred prey mass
# of a maximum-size baleen whale exactly on the largest Antarctic krill. Under it
# the WHOLE baleen size range prefers prey inside the krill spectrum
# (0.091 g at w_min -> 4.17 g at w_max), which no other single beta achieves.
# Minke whales keep their stored beta of 5.042017e6, which already places their
# preferred prey at 0.119-1.19 g, squarely on krill.
#
# THE INTERACTION MATRIX IS KEPT SYMMETRIC. It was built from water-column
# overlap and is exactly symmetric in the base object, so both [baleen, krill] and
# [krill, baleen] are written. The second of those is dynamically irrelevant -- it
# governs krill preying on baleen whales, which the kernel forbids at any size --
# but writing it keeps the matrix's meaning intact as a habitat-overlap matrix.
#
# THE RECALIBRATION IS 42_recalibrate_base_biomass.R's LADDER, UNCHANGED.
# Leading steady(), then (matchBiomasses -> steady) alternating over a tolerance
# ladder 0.1 -> 0.05 -> 0.01 -> 0.002, ending on steady(), keeping the BEST
# converged state at the target tolerance rather than the last. The residual is
# read after steady(), never after matchBiomasses (which lands on the targets to
# ~1e-16 and would break the loop on round one). Tightening the tolerance makes
# the biomass fit WORSE, so the ladder does not converge monotonically -- see the
# note at 42:63-80.
#
# PAIRED, ALWAYS. The control arm is the same base object through the IDENTICAL
# ladder with no parameter change. The base is itself a product of that ladder, so
# control - base measures what re-laddering costs on its own, and revised -
# control is what the kernel change did. Re-running steady() alone is known to
# move biomasses 0.2-5.8%, which is why this separation is not optional.
#
# Writes  params_whale_lognormal_kernel.rds                    (revised)
#         Output_large_files/wmin_test/51_control_relad.rds     (control)
#         Output_large_files/wmin_test/51_{biomass_ratios,ladder_trace,diet}.csv
# Overwrites nothing that already exists.
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(dplyr)
})
source("R/check_size_params.R")
source("R/wmin_test/thermizer_shim.R")

out_dir <- "Output_large_files/wmin_test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
BASE    <- "params_sel_adj_wmin_corrected_biocal.rds"

BALEEN <- "baleen whales"; MINKE <- "minke whales"; KRILL <- "antarctic krill"
BETA_BALEEN <- 2.468e7       # w_max(baleen) / w_max(krill)
# interaction[whale, antarctic krill], each written SYMMETRICALLY. The defaults
# are the base values, so by default the matrix is left exactly as it was.
THETA_BK <- as.numeric(Sys.getenv("K51_THETA_BK", "0.2"))
THETA_MK <- as.numeric(Sys.getenv("K51_THETA_MK", "0.05"))
BK_BASE  <- 0.2; MK_BASE <- 0.05      # asserted against the loaded object below
CHANGED_THETA <- !isTRUE(all.equal(THETA_BK, BK_BASE)) ||
                 !isTRUE(all.equal(THETA_MK, MK_BASE))
fmt <- function(x) gsub("\\.", "", sprintf("%g", x))
SUF <- if (CHANGED_THETA) sprintf("_bk%s_mk%s", fmt(THETA_BK), fmt(THETA_MK)) else ""
OUT_RDS <- sprintf("params_whale_lognormal_kernel%s.rds", SUF)
tag     <- function(x) file.path(out_dir, sprintf("51%s_%s", SUF, x))

STEADY_TMAX     <- 1000      # 09_Uncertainty_Analysis.Rmd:1444
STEADY_PRESERVE <- c("erepro")
TOL_LADDER      <- c(0.1, 0.05, 0.01, 0.002)
STEADY_TOL      <- 0.002
MATCH_ROUNDS    <- 6
TARGET_ROUNDS   <- 14
ACCEPT_DEV      <- 0.0172    # the stored base's own worst deviation, as in 42

guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite an existing file: ", f, call. = FALSE); f
}
guard(OUT_RDS); guard(tag("control_relad.rds"))

t0 <- proc.time()
cat("=== Phase 51: lognormal whale kernel + recalibration ===\n")
cat("started", format(Sys.time()), "| mizer", as.character(packageVersion("mizer")),
    "| therMizer", as.character(packageVersion("therMizer")), "\n\n")

# --- load and upgrade --------------------------------------------------------
base <- readRDS(BASE)
base <- suppressWarnings(validParams(base))
stopifnot(identical(base@second_order_w$flux, "upwind"),
          isFALSE(base@second_order_w$bin_average),
          identical(base@rates_funcs$Encounter, "therMizerEncounter"),
          identical(base@rates_funcs$RDD, "BevertonHoltRDD"),
          identical(base@resource_dynamics, "plankton_forcing"))
sp0 <- base@species_params
SPN <- sp0$species
i_bal <- which(SPN == BALEEN); i_mnk <- which(SPN == MINKE); i_kr <- which(SPN == KRILL)
stopifnot(length(i_bal) == 1, length(i_mnk) == 1, length(i_kr) == 1)
cat("base:", BASE, "| small divers w_min =", sp0$w_min[SPN == "small divers"], "g\n")
cat("base kernels: baleen", as.character(sp0$pred_kernel_type)[i_bal],
    "beta", signif(sp0$beta[i_bal], 7), "| minke",
    as.character(sp0$pred_kernel_type)[i_mnk], "beta", signif(sp0$beta[i_mnk], 7), "\n")
cat("base interaction[baleen, krill] =", base@interaction[i_bal, i_kr],
    "| matrix symmetric:",
    isTRUE(all.equal(base@interaction, t(base@interaction), check.attributes = FALSE)),
    "\n\n")

# --- helpers, transcribed from 42_recalibrate_base_biomass.R ------------------
bio_ratio <- function(p)
  as.numeric(getBiomass(p, use_cutoff = TRUE) / p@species_params$biomass_observed)
bio_ratio_full <- function(p)
  as.numeric(getBiomass(p, use_cutoff = FALSE) / p@species_params$biomass_observed)
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
    stop("steady() errored for ", label, " at tol = ", tol, ": ", as.character(out))
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
    stop(label, " rung ", rung, ": erepro >= 1 for ", paste(bad_e, collapse = ", "),
         " -- abort. orca starts at 0.9509 with only 5% headroom.")
  chk <- check_size_params(p, quiet = TRUE)
  if (!all(chk$ordering_ok))
    stop(label, " rung ", rung, ": w_min < w_mat < w_max violated for ",
         paste(chk$species[!chk$ordering_ok], collapse = ", "))
  if (!identical(p@rates_funcs$Encounter, "therMizerEncounter"))
    stop(label, " rung ", rung, ": therMizer Encounter override was lost")
  invisible(TRUE)
}

# --- the kernel edit ----------------------------------------------------------
# setParams() rebuilds the predation kernel from species_params, but it also
# RESETS rates_funcs (dropping therMizer) and resource_dynamics. Splice back only
# the three therMizer entries -- restoring the whole list would drop mizer 3.x's
# Diffusion entry. Same pattern as 42's set_wmin() / 30_stageC:74-86.
set_lognormal <- function(p, betas) {
  rf <- p@rates_funcs; rd <- p@resource_dynamics
  sp <- p@species_params
  for (nm in names(betas)) {
    i <- which(sp$species == nm)
    sp$pred_kernel_type[i] <- "lognormal"
    sp$beta[i] <- betas[[nm]]
  }
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
stopifnot(isTRUE(all.equal(base@interaction[i_bal, i_kr], BK_BASE)),
          isTRUE(all.equal(base@interaction[i_kr, i_bal], BK_BASE)),
          isTRUE(all.equal(base@interaction[i_mnk, i_kr], MK_BASE)),
          isTRUE(all.equal(base@interaction[i_kr, i_mnk], MK_BASE)))
revised <- set_lognormal(base, setNames(list(BETA_BALEEN, base@species_params$beta[i_mnk]),
                                        c(BALEEN, MINKE)))
# Both halves of each pair, so the matrix stays symmetric as it was constructed.
revised@interaction[i_bal, i_kr] <- revised@interaction[i_kr, i_bal] <- THETA_BK
revised@interaction[i_mnk, i_kr] <- revised@interaction[i_kr, i_mnk] <- THETA_MK

# VERIFY the kernel is genuinely lognormal now, not still a box. Two independent
# checks: the peak must sit at w/beta, and the kernel must take values strictly
# between 0 and 1 (a box takes only 0 and 1).
K <- getPredKernel(revised)
for (nm in c(BALEEN, MINKE)) {
  i <- which(SPN == nm)
  wmax_bin <- which.min(abs(revised@w - revised@species_params$w_max[i]))
  row <- K[i, wmax_bin, ]
  peak_w <- revised@w_full[which.max(row)]
  expect <- revised@w[wmax_bin] / revised@species_params$beta[i]
  interior <- sum(row > 1e-6 & row < 0.999)
  cat(sprintf("kernel check %-14s peak at %.4g g, expected %.4g g (%.2f bins apart) | interior values: %d\n",
              nm, peak_w, expect,
              abs(log(peak_w / expect)) / log(revised@w_full[2] / revised@w_full[1]),
              interior))
  if (abs(log(peak_w / expect)) > log(revised@w_full[2] / revised@w_full[1]))
    stop(nm, ": kernel peak is not at w/beta -- the kernel did not switch")
  if (interior < 10) stop(nm, ": kernel still looks like a box (no interior values)")
}
stopifnot(revised@interaction[i_bal, i_kr] == THETA_BK,
          revised@interaction[i_kr, i_bal] == THETA_BK,
          revised@interaction[i_mnk, i_kr] == THETA_MK,
          revised@interaction[i_kr, i_mnk] == THETA_MK,
          isTRUE(all.equal(revised@interaction, t(revised@interaction),
                           check.attributes = FALSE)))
cat(sprintf("interaction[baleen, krill] %.4g -> %.4g | [minke, krill] %.4g -> %.4g",
            BK_BASE, THETA_BK, MK_BASE, THETA_MK),
    "\n  written symmetrically; matrix remains symmetric",
    if (!CHANGED_THETA) " -- NO CHANGE, kernel is the only edit" else "",
    "\n  baleen:minke krill ratio ", sprintf("%.2f -> %.2f", BK_BASE / MK_BASE,
                                             THETA_BK / THETA_MK),
    " (the whole minke ROW is 1/4 of baleen's in the base object)\n\n", sep = "")

# --- the ladder, transcribed from 42 -----------------------------------------
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
        krill_ratio = bio_ratio(p)[i_kr],
        max_erepro = max(p@species_params$erepro),
        n_rmax_inf = sum(!is.finite(p@species_params$R_max)),
        converged = st$converged,
        bumped = if (length(mt$bumped)) paste(mt$bumped, collapse = " ") else "",
        stringsAsFactors = FALSE)
      cat(sprintf("   tol=%-6.3g r=%-2d max dev %.5f  baleen %.4f krill %.4f  max erepro %.4f  %s%s\n",
                  tol, r, d, bio_ratio(p)[i_bal], bio_ratio(p)[i_kr],
                  max(p@species_params$erepro),
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
p_rev <- recalibrate(revised, "revised (lognormal kernel)")

# --- what the whales now actually eat ----------------------------------------
# Diet at the calibrated state, via the shim's ther_diet(), which takes `year`
# explicitly. 1841 is used because these are base params, pre-projection.
diet_of <- function(p, label) {
  d <- ther_diet(p, n = p@initial_n, n_pp = p@initial_n_pp,
                 n_other = p@initial_n_other, year = 1841)
  E <- p@initial_n * rep(p@dw, each = nrow(p@initial_n))
  C <- apply(d, 3, function(mm) rowSums(mm * E))
  dimnames(C) <- list(predator = SPN, prey = dimnames(d)$prey)
  do.call(rbind, lapply(c(BALEEN, MINKE), function(nm) {
    v <- C[nm, ]; tot <- sum(v)
    data.frame(state = label, predator = nm,
               total_intake_t_per_yr = tot / 1e6,
               krill_pct = 100 * v[[KRILL]] / tot,
               resource_pct = 100 * v[["Resource"]] / tot,
               otherLTL_pct = 100 * sum(v[c("mesozooplankton", "other krill",
                 "other macrozooplankton", "salps")]) / tot,
               fish_squid_pct = 100 * sum(v[c("mesopelagic fishes",
                 "bathypelagic fishes", "shelf and coastal fishes", "squids",
                 "toothfishes")]) / tot) }))
}
DIET <- bind_rows(diet_of(base, "base (box kernel)"),
                  diet_of(p_ctl, "control (re-laddered)"),
                  diet_of(p_rev, "revised (lognormal)"))
cat("=== whale diet composition at the calibrated state ===\n")
print(as.data.frame(DIET), digits = 4, row.names = FALSE)

# --- report -------------------------------------------------------------------
cmp <- data.frame(
  species = SPN, obs = sp0$biomass_observed,
  ratio_base = bio_ratio(base), ratio_control = bio_ratio(p_ctl),
  ratio_revised = bio_ratio(p_rev),
  biomass_base_t = as.numeric(getBiomass(base, use_cutoff = TRUE)) / 1e6,
  biomass_control_t = as.numeric(getBiomass(p_ctl, use_cutoff = TRUE)) / 1e6,
  biomass_revised_t = as.numeric(getBiomass(p_rev, use_cutoff = TRUE)) / 1e6,
  erepro_base = sp0$erepro, erepro_control = p_ctl@species_params$erepro,
  erepro_revised = p_rev@species_params$erepro,
  R_max_control = p_ctl@species_params$R_max,
  R_max_revised = p_rev@species_params$R_max,
  stringsAsFactors = FALSE)
cat("\n=== modelled/observed biomass, cutoff basis (what matchBiomasses fits) ===\n")
print(cmp[, c("species", "obs", "ratio_base", "ratio_control", "ratio_revised")],
      digits = 5, row.names = FALSE)

cat("\n=== hard constraints ===\n")
for (nm in c("control", "revised")) {
  p <- if (nm == "control") p_ctl else p_rev
  s <- p@species_params
  cat(sprintf("  %-8s erepro < 1: %s (max %.4f, %s) | R_max finite: %s | max dev %.5f  %s\n",
              nm, all(s$erepro < 1), max(s$erepro), s$species[which.max(s$erepro)],
              all(is.finite(s$R_max)), max_dev(p),
              if (max_dev(p) <= ACCEPT_DEV) "PASS" else "FAIL"))
}
cat(sprintf("  acceptance bar: max dev <= %.4f (the stored base's own worst)\n",
            ACCEPT_DEV))

# --- reproduction: erepro, R_max and the ACHIEVED reproduction level ---------
# The reproduction level is an OUTCOME, not a setting. steady(preserve = "erepro")
# ends by calling setBevertonHolt(erepro = old_erepro), which holds erepro and
# RE-DERIVES R_max from the required RDD at the equilibrated state. So erepro is
# carried through essentially unchanged while R_max moves, and the level lands
# where it lands. Always report the achieved value; never describe these runs as
# controlled at matched recruitment.
repro_of <- function(p, lab) {
  r <- getRates(p, n = p@initial_n, n_pp = p@initial_n_pp,
                n_other = p@initial_n_other, effort = 0, t = 1841)
  lvl <- tryCatch(as.numeric(getReproductionLevel(p)),
                  error = function(e) as.numeric(r$rdd / p@species_params$R_max))
  data.frame(state = lab, species = SPN,
             erepro = p@species_params$erepro, R_max = p@species_params$R_max,
             RDI = as.numeric(r$rdi), RDD = as.numeric(r$rdd),
             reproduction_level = lvl,
             rdd_over_rmax = as.numeric(r$rdd) / p@species_params$R_max,
             stringsAsFactors = FALSE)
}
REPRO <- bind_rows(repro_of(base, "base"), repro_of(p_ctl, "control"),
                   repro_of(p_rev, "revised"))
chk_lvl <- max(abs(REPRO$reproduction_level - REPRO$rdd_over_rmax), na.rm = TRUE)
cat(sprintf("\n=== reproduction (getReproductionLevel vs RDD/R_max agree to %.3g) ===\n",
            chk_lvl))
wide <- REPRO %>%
  select(species, state, erepro, R_max, reproduction_level) %>%
  tidyr::pivot_wider(names_from = state,
                     values_from = c(erepro, R_max, reproduction_level))
print(as.data.frame(wide), digits = 5, row.names = FALSE)
cat("\n  erepro identical base vs revised (preserve = 'erepro'):",
    isTRUE(all.equal(REPRO$erepro[REPRO$state == "base"],
                     REPRO$erepro[REPRO$state == "revised"], tolerance = 1e-8)), "\n")
rl <- REPRO$reproduction_level[REPRO$state == "revised"]
cat(sprintf("  revised reproduction level: min %.4f (%s), median %.4f, max %.4f (%s)\n",
            min(rl), SPN[which.min(rl)], median(rl), max(rl), SPN[which.max(rl)]))

write.csv(cmp, guard(tag("biomass_ratios.csv")), row.names = FALSE)
write.csv(bind_rows(trace_rows), guard(tag("ladder_trace.csv")), row.names = FALSE)
write.csv(DIET, guard(tag("diet.csv")), row.names = FALSE)
write.csv(REPRO, guard(tag("reproduction.csv")), row.names = FALSE)
saveRDS(p_rev, OUT_RDS)
saveRDS(p_ctl, tag("control_relad.rds"))

cat("\nWrote", OUT_RDS, "and", tag("control_relad.rds"), "\n")
cat("      ", tag("{biomass_ratios,ladder_trace,diet,reproduction}.csv"), "\n")
cat("elapsed:", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")

if (max_dev(p_rev) > ACCEPT_DEV)
  cat("\nNOTE: the revised arm did not reach the acceptance bar --",
      "inspect 51_ladder_trace.csv before using it.\n")
