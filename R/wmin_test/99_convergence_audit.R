# =============================================================================
# Phase 99 -- THE CONVERGENCE AUDIT: is the whale 1841 baseline a steady state?
#
# READ-ONLY. This phase builds no members and rewrites no reference model. It
# reads the STORED phase-88 states and measures how far they are from the steady
# state the protocol believes it found. Nothing here is a candidate for the
# ensemble; the outputs are diagnostics.
#
# ------------------------------------------------------------------- why now
# Measured 2026-08-16 on params_ref_p86_agemat.rds. Perturb baleen whales x5 and
# call steady() at the phase-88 RAMP settings (tol 0.01, t_max 300, preserve
# "erepro"):
#
#     steady() reports convergence : TRUE
#     of the 5x applied, surviving : 4.995
#
# Re-run the same perturbation at tol 1e-4, t_max 2000 and baleen falls to
# 1.398, with steady() reporting NON-convergence -- still not finished after two
# millennia. So the protocol overstates the baleen equilibrium by ~3.6x and
# reports success while doing it.
#
# TWO MECHANISMS, both in steady() itself, and they compound:
#
# 1. THE MULTIPLIER IS ERASED BEFORE THE PROJECTION STARTS. steady() sets
#    rates_funcs$RDD to "constantRDD" with constant_reproduction = getRDD() of
#    the state it is handed. Applying 23x to initial_n raises RDI 23x but raises
#    RDD only 1.106x, because RDD saturates at the un-scaled R_max (level ->
#    0.995). So steady() is asked to relax a 23x spectrum onto ~1.1x
#    recruitment, and the fixed point it is walking toward is ~1.1x, not 23x.
#
# 2. IT CANNOT WALK THERE, AND CANNOT TELL. Convergence is distanceMaxRelRDI
#    over t_per = 1.5 yr. Baleen total mortality is 0.00128/yr, so the
#    relaxation timescale is ~780 yr and RDI moves ~0.2% per period -- inside
#    tol = 0.01 at any abundance. The whales pass the convergence test
#    trivially. The phase-88 stability screen cannot catch it either: it flags
#    |slope/mean| > 0.025 and this drifts at 0.0013.
#
# If that holds on the stored states then the whale 1841 stock -- the
# denominator of every depletion and recovery statistic in the manuscript -- is
# a slowly decaying transient, not a baseline. That is what this phase measures.
# It is a prerequisite for reading phases 94/95 and for T1-T7.
#
# --------------------------------------------------------------- the modes
#   timescale  cheap. Per member and species: total mortality, relaxation time,
#              the fraction of a perturbation surviving each protocol horizon,
#              and a short CONSTANT-CLIMATE projection giving the MEASURED
#              drift rate. Then the blind-spot test: is that drift smaller than
#              the tolerance steady() judges itself by. Runs locally.
#   steady     re-steady the stored 1841 state at the protocol rungs and at a
#              long/tight rung, and report how far each species moves. This is
#              the decisive arm. The long rung is the expensive one.
#   drift      long constant-climate unfished projection from the stored state,
#              giving the empirical equilibrium and half-life directly, with no
#              steady() involved at all. VM.
#   collect    assemble whatever chunks exist.
#
# ------------------------------------------------------------ THE CLIMATE TRAP
# therMizer does not error outside its forcing record: plankton_forcing() and
# scaled_temp_effect() both open with
#     if (!floor(t) %in% <years>) t <- t %% <last year> + <first year>
# Two consequences this phase has to handle, and the second is not in phase 94:
#
#   a. Projections past 2010 wrap to 1842-. Phase 94 fixed this by extending the
#      arrays. Same fix here, but repeating the ISIMIP3a ENSO window rather than
#      pinning one year -- see P99_FORCING below.
#   b. steady() ALSO walks the record. projectToSteady starts at t = 0, which
#      wraps to 1841, and then t = 1 -> 1842 and so on -- so a steady() call
#      with t_max 5000 cycles the 1841-2010 climate roughly 30 times. Every arm
#      here therefore runs against forcing PINNED to a single year, so the
#      comparison is about relaxation and not about climate. P99_PIN=0 disables
#      it, and the `ramp_asrun` arm is always unpinned so the protocol's own
#      behaviour stays visible.
#
# Verified 2026-08-16: neither therMizerEncounter, therMizerPredRate nor
# therMizerEReproAndGrowth reads other_params$t_idx, so rewriting ocean_temp and
# n_pp_array is sufficient to control the forcing.
#
# MEMBER 0 IS THE REFERENCE MODEL, carried through every mode as the control. If
# the reference is itself unconverged for the whales then the defect predates
# the member protocol and no change to the protocol can fix it.
#
# USAGE  Rscript R/wmin_test/99_convergence_audit.R [timescale|steady|drift|collect]
# ENV    P99_STEM, P99_BASE, P99_N, P99_MEMBERS, P99_CORES, P99_PROBE_YEARS,
#        P99_DRIFT_YEARS, P99_REF_YEAR, P99_LONG_TMAX, P99_LONG_TOL, P99_ARMS,
#        P99_PIN, P99_OUT
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OL        <- "Output_large_files/wmin_test"
STEM      <- Sys.getenv("P99_STEM", "88_full")
BASE_FILE <- Sys.getenv("P99_BASE", "params_ref_p86_agemat.rds")
N         <- as.integer(Sys.getenv("P99_N", "20"))
CORES     <- min(as.integer(Sys.getenv("P99_CORES", "8")),
                 max(1L, parallel::detectCores() - 2L))
PROBE_YEARS <- as.integer(Sys.getenv("P99_PROBE_YEARS", "20"))
DRIFT_YEARS <- as.integer(Sys.getenv("P99_DRIFT_YEARS", "1000"))
# Store every DRIFT_KEEP-th year of the drift trajectory. The equilibrium and
# half-life below are computed from the FULL annual series regardless; this only
# thins what goes to disk, because 427 members x 2000 yr x 19 species is 16M
# rows and the VM runs at 87% disk. Centuries-long relaxations do not need
# annual resolution.
DRIFT_KEEP  <- max(1L, as.integer(Sys.getenv("P99_DRIFT_KEEP", "10")))
REF_YEAR    <- as.integer(Sys.getenv("P99_REF_YEAR", "1841"))
# FORCING CONTROL. "cycle" repeats the ISIMIP3a ENSO window; "pin" holds one
# year. Cycle is the DEFAULT and is the protocol-correct choice: FishMIP 2.0
# ISIMIP3a asks for ctrlclim 1961-1980 "on repeat for six cycles" over 1841-1960,
# chosen because they span a full ENSO cycle with no detectable trend before
# 1980, and says that models needing a longer spin-up should "repeat the ENSO
# cycle ... for as many times necessary". Verified against this model's arrays:
# 1841-1960 is EXACTLY six repeats of 1961-1980, bit for bit.
# Pinning a single year is therefore NOT neutral -- 1841 sits 0.043 degC above
# the cycle mean. The first drift pass used "pin"; this exists to close that.
FORCING  <- Sys.getenv("P99_FORCING", "cycle")
CYC_FROM <- as.integer(Sys.getenv("P99_CYCLE_FROM", "1961"))
CYC_TO   <- as.integer(Sys.getenv("P99_CYCLE_TO", "1980"))
stopifnot(FORCING %in% c("cycle", "pin"), CYC_TO > CYC_FROM)
LONG_TMAX   <- as.integer(Sys.getenv("P99_LONG_TMAX", "5000"))
LONG_TOL    <- as.numeric(Sys.getenv("P99_LONG_TOL", "1e-5"))
PIN         <- Sys.getenv("P99_PIN", "1") == "1"
ARMS        <- trimws(strsplit(Sys.getenv("P99_ARMS",
                 "ramp_asrun,ramp,ladder,long"), ",")[[1]])

# The protocol rungs, transcribed from 88_full_ensemble.R. These are the thing
# under test -- do not "improve" them here.
RAMP_TOL <- 0.01;  RAMP_TMAX <- 300L;  RAMP_PRESERVE <- "erepro"
LAD_TOL  <- 0.001; LAD_TMAX  <- 1500L; LAD_PRESERVE  <- "R_max"
T_PER    <- 1.5                      # steady()'s own convergence period

STATE_DIR <- file.path(OL, paste0(STEM, "_states"))
OUTDIR    <- Sys.getenv("P99_OUT", file.path(OL, "99_convergence"))
dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "timescale"
stopifnot(mode %in% c("timescale", "steady", "drift", "collect"),
          all(ARMS %in% c("ramp_asrun", "ramp", "ladder", "long")))

t0 <- proc.time()
cat("=== Phase 99: convergence audit | mode:", mode, "===\n")
if (!file.exists(BASE_FILE)) stop("missing base params: ", BASE_FILE, call. = FALSE)
if (!dir.exists(STATE_DIR))  stop("missing state dir: ", STATE_DIR, call. = FALSE)

BASE <- suppressWarnings(validParams(readRDS(BASE_FILE)))
stopifnot(identical(BASE@rates_funcs$Encounter, "therMizerEncounter"),
          identical(BASE@resource_dynamics, "plankton_forcing"))
if (all(BASE@ext_encounter == 0))
  stop("base carries no out-of-domain subsidy -- wrong reference model",
       call. = FALSE)
SPN <- BASE@species_params$species
WDW <- BASE@w * BASE@dw

# --- members: usable only, ordered by the phase-93 ranking --------------------
SRC <- readRDS(file.path(OL, paste0(STEM, ".rds")))
MEM <- SRC$members
adm <- if ("n_erepro_ge1" %in% names(MEM)) MEM$n_erepro_ge1 == 0 else TRUE
usable <- as.integer(MEM$sim_index[MEM$stable & adm])
RR_F <- file.path(OL, "93_rerank_p88.rds")
if (file.exists(RR_F)) {
  RK <- readRDS(RR_F)$ranking
  usable <- as.integer(RK$sim_index[RK$sim_index %in% usable])
  cat("  members ordered by the phase-93 ranking\n")
}
if (nzchar(Sys.getenv("P99_MEMBERS"))) {
  members <- as.integer(trimws(strsplit(Sys.getenv("P99_MEMBERS"), ",")[[1]]))
  if (length(setdiff(members, usable)))
    cat("  NOTE: P99_MEMBERS includes", length(setdiff(members, usable)),
        "member(s) outside the usable set\n")
} else {
  members <- head(usable, min(N, length(usable)))
}
# 0 is the reference model itself -- the control, and it goes first
members <- c(0L, members)
CORES <- min(CORES, max(1L, length(members)))
cat("  stem:", STEM, "| built", nrow(MEM), "-> usable", length(usable), "\n")
cat("  auditing", length(members) - 1L, "members + the reference model |",
    CORES, "cores of", parallel::detectCores(), "\n")
cat("  forcing:", if (FORCING == "cycle")
      sprintf("ENSO cycle %d-%d repeated (protocol)", CYC_FROM, CYC_TO)
    else sprintf("pinned at %d", REF_YEAR),
    "| applied:", PIN, "| arms:", paste(ARMS, collapse = ", "), "\n\n")

# --- forcing control ----------------------------------------------------------
# Every projected year gets its own label and every label carries the SAME year's
# forcing, so the wrap can never fire and the climate cannot move. Both arrays
# must share length and labels: plankton_forcing() indexes n_pp_array using
# dimnames(ocean_temp).
set_forcing <- function(p, ref_year, n_years, t_start = 1841) {
  o <- p@other_params$other
  ot <- o$ocean_temp; npp <- o$n_pp_array
  oyr <- as.numeric(rownames(ot))
  stopifnot(identical(oyr, as.numeric(rownames(npp))))
  yrs <- t_start + seq_len(n_years + 2L) - 1L   # +2: t can reach t_start+n_years
  if (FORCING == "cycle") {
    src <- which(oyr >= CYC_FROM & oyr <= CYC_TO)
    if (!length(src)) stop("ENSO window ", CYC_FROM, "-", CYC_TO,
                           " not in the forcing record", call. = FALSE)
    # phase-aligned: model year t_start is cycle position 0, exactly as the
    # protocol's 1841 == ctrlclim 1961.
    k <- src[((seq_along(yrs) - 1L) %% length(src)) + 1L]
  } else {
    k <- match(ref_year, oyr)
    if (is.na(k)) stop("P99_REF_YEAR ", ref_year, " not in the forcing record",
                       call. = FALSE)
    k <- rep(k, length(yrs))
  }
  ot2  <- ot[k, , drop = FALSE]
  npp2 <- npp[k, , drop = FALSE]
  rownames(ot2) <- rownames(npp2) <- yrs
  stopifnot(nrow(ot2) == nrow(npp2), identical(rownames(ot2), rownames(npp2)),
            all(seq(t_start, t_start + n_years) %in% as.numeric(rownames(ot2))))
  p@other_params$other$ocean_temp <- ot2
  p@other_params$other$n_pp_array <- npp2
  p
}

# steady() in mizer 3.1.0 signals non-convergence with message(), not warning().
steady_guarded <- function(p, tol, preserve, tmax) {
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

biom <- function(n) as.numeric(n %*% WDW)

# Load a member and return it AS THE 1841 STATE: the stored params carry the
# pre-spin-up ladder spectrum in initial_n, while initial_n from the state file
# is the post-spin-up state every projection actually starts from. Both are
# audited; `which` selects.
load_state <- function(si, which = c("spinup", "ladder")) {
  which <- match.arg(which)
  if (si == 0L) return(list(p = BASE, n = BASE@initial_n))
  z <- readRDS(file.path(STATE_DIR, sprintf("state_%05d.rds", si)))
  n <- if (which == "spinup") z$initial_n else z$params@initial_n
  list(p = z$params, n = n)
}

# ============================================================ mode: timescale ==
probe_worker <- function(si) {
  suppressPackageStartupMessages({library(mizer); library(therMizer)})
  out <- lapply(c("ladder", "spinup"), function(which) {
    st <- try(load_state(si, which), silent = TRUE)
    if (inherits(st, "try-error")) return(NULL)
    p <- st$p; p@initial_n <- st$n
    ext0 <- p@ext_encounter
    pp <- set_forcing(p, REF_YEAR, PROBE_YEARS)

    B  <- sweep(p@initial_n, 2, WDW, "*")
    tot <- rowSums(B)
    r <- try(mizer::getRates(pp, n = p@initial_n, n_pp = pp@initial_n_pp,
                             n_other = pp@initial_n_other, effort = 0,
                             t = REF_YEAR), silent = TRUE)
    if (inherits(r, "try-error")) return(NULL)
    M  <- rowSums(r$mort * B) / tot
    FL <- rowSums(r$feeding_level * B) / tot
    rdd <- as.numeric(getRDD(pp, t = REF_YEAR))
    rdi <- as.numeric(getRDI(pp, t = REF_YEAR))
    rl  <- as.numeric(getReproductionLevel(pp))

    # MEASURED drift: constant climate, unfished, from this exact state.
    s <- try(project(pp, initial_n = p@initial_n, t_start = 1841,
                     t_max = PROBE_YEARS, effort = 0, progress_bar = FALSE),
             silent = TRUE)
    if (inherits(s, "try-error")) return(NULL)
    bt  <- apply(s@n, 1, function(nn) rowSums(sweep(nn, 2, WDW, "*")))
    yr  <- as.numeric(dimnames(s@n)$time)
    drift <- (log(bt[, ncol(bt)]) - log(bt[, 1])) / (yr[length(yr)] - yr[1])

    data.frame(
      sim_index = si, state = which, species = SPN,
      biomass = tot, M = M, relax_years = 1 / M, feeding_level = FL,
      RDI = rdi, RDD = rdd, R_max = p@species_params$R_max,
      repro_level = rl, erepro = p@species_params$erepro,
      # what survives each horizon the protocol allows
      surv_ramp   = exp(-M * RAMP_TMAX),
      surv_ladder = exp(-M * LAD_TMAX),
      surv_spinup = exp(-M * 118),
      drift_per_yr = drift,
      # THE BLIND-SPOT TEST, and it needs BOTH halves. A drift below tolerance
      # is what a genuinely converged state looks like, so on its own it proves
      # nothing -- the reference model shows it for all 19 species, correctly.
      # The failure is a species whose perturbation SURVIVES the horizon
      # (surv_ramp large) while its drift is still too small for steady() to
      # register (drift_below_ramp_tol). That pair is unfalsifiable: steady()
      # will report convergence at any abundance you hand it.
      change_per_period = abs(drift) * T_PER,
      drift_below_ramp_tol   = abs(drift) * T_PER < RAMP_TOL,
      drift_below_ladder_tol = abs(drift) * T_PER < LAD_TOL,
      unfalsifiable_ramp   = (exp(-M * RAMP_TMAX) > 0.1) &
                             (abs(drift) * T_PER < RAMP_TOL),
      unfalsifiable_ladder = (exp(-M * LAD_TMAX) > 0.1) &
                             (abs(drift) * T_PER < LAD_TOL),
      subsidy_ok = isTRUE(all.equal(pp@ext_encounter, ext0)),
      stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# =============================================================== mode: steady ==
steady_worker <- function(si) {
  suppressPackageStartupMessages({library(mizer); library(therMizer)})
  st <- try(load_state(si, "spinup"), silent = TRUE)
  if (inherits(st, "try-error"))
    return(data.frame(sim_index = si, arm = NA_character_, species = SPN,
                      ratio = NA_real_, converged = NA, errored = TRUE,
                      stringsAsFactors = FALSE))
  p0 <- st$p; p0@initial_n <- st$n
  b_in <- biom(p0@initial_n)
  cfg <- list(
    ramp_asrun = list(tol = RAMP_TOL, tmax = RAMP_TMAX, pres = RAMP_PRESERVE, pin = FALSE),
    ramp       = list(tol = RAMP_TOL, tmax = RAMP_TMAX, pres = RAMP_PRESERVE, pin = TRUE),
    ladder     = list(tol = LAD_TOL,  tmax = LAD_TMAX,  pres = LAD_PRESERVE,  pin = TRUE),
    long       = list(tol = LONG_TOL, tmax = LONG_TMAX, pres = RAMP_PRESERVE, pin = TRUE))
  do.call(rbind, lapply(ARMS, function(a) {
    k <- cfg[[a]]
    p <- if (k$pin && PIN) set_forcing(p0, REF_YEAR, k$tmax) else p0
    ext0 <- p@ext_encounter
    el <- proc.time()
    r <- steady_guarded(p, k$tol, k$pres, k$tmax)
    el <- (proc.time() - el)[["elapsed"]]
    if (r$errored)
      return(data.frame(sim_index = si, arm = a, species = SPN, ratio = NA_real_,
                        erepro = NA_real_, repro_level = NA_real_,
                        converged = NA, errored = TRUE, elapsed_s = el,
                        subsidy_ok = NA, stringsAsFactors = FALSE))
    q <- r$params
    data.frame(sim_index = si, arm = a, species = SPN,
               ratio = biom(q@initial_n) / b_in,
               erepro = q@species_params$erepro,
               repro_level = as.numeric(getReproductionLevel(q)),
               converged = r$converged, errored = FALSE, elapsed_s = el,
               subsidy_ok = isTRUE(all.equal(q@ext_encounter, ext0)),
               stringsAsFactors = FALSE)
  }))
}

# ================================================================ mode: drift ==
# No steady() anywhere. Just: hold the climate still, remove all fishing, and
# see where the stored state goes. This is the measurement steady() is supposed
# to be an efficient substitute for.
drift_worker <- function(si) {
  suppressPackageStartupMessages({library(mizer); library(therMizer)})
  st <- try(load_state(si, "spinup"), silent = TRUE)
  if (inherits(st, "try-error")) return(NULL)
  p <- set_forcing(st$p, REF_YEAR, DRIFT_YEARS)
  s <- try(project(p, initial_n = st$n, t_start = 1841, t_max = DRIFT_YEARS,
                   effort = 0, progress_bar = FALSE), silent = TRUE)
  if (inherits(s, "try-error")) return(NULL)
  bt <- apply(s@n, 1, function(nn) rowSums(sweep(nn, 2, WDW, "*")))  # sp x time
  yr <- as.numeric(dimnames(s@n)$time)
  b0 <- bt[, 1]
  beq <- rowMeans(bt[, (ncol(bt) - min(50, ncol(bt) - 1)):ncol(bt), drop = FALSE])
  # time to close half the distance from the start to the end state
  half <- vapply(seq_len(nrow(bt)), function(j) {
    tgt <- b0[j] + 0.5 * (beq[j] - b0[j])
    hit <- which(if (beq[j] >= b0[j]) bt[j, ] >= tgt else bt[j, ] <= tgt)
    if (!length(hit)) NA_real_ else yr[hit[1]] - yr[1]
  }, 0)
  list(summary = data.frame(sim_index = si, species = SPN,
         b_start = b0, b_end = beq, ratio_end_start = beq / b0,
         half_life_years = half, still_moving_pct_per_yr =
           100 * ((bt[, ncol(bt)] / bt[, ncol(bt) - min(50, ncol(bt) - 1)])^
                    (1 / min(50, ncol(bt) - 1)) - 1),
         stringsAsFactors = FALSE),
       trajectory = local({
         k <- unique(c(seq(1L, length(yr), by = DRIFT_KEEP), length(yr)))
         data.frame(sim_index = si,
           Year = rep(yr[k], each = length(SPN)),
           species = rep(SPN, times = length(k)),
           biomass = as.vector(bt[, k, drop = FALSE]),
           stringsAsFactors = FALSE)
       }))
}

# ================================================================== dispatch ===
EXPORTS <- c("BASE", "SPN", "WDW", "STATE_DIR", "REF_YEAR", "PIN", "ARMS",
             "PROBE_YEARS", "DRIFT_YEARS", "DRIFT_KEEP", "LONG_TMAX",
             "LONG_TOL", "T_PER",
             "RAMP_TOL", "RAMP_TMAX", "RAMP_PRESERVE", "LAD_TOL", "LAD_TMAX",
             "LAD_PRESERVE", "set_forcing", "FORCING", "CYC_FROM", "CYC_TO",
             "steady_guarded", "biom",
             "load_state")
run_parallel <- function(fun, tag) {
  # force() before parLapply serialises the closure: passed lazily, `fun` is a
  # promise for a name that does not exist on the workers, and every member
  # fails with "object 'probe_worker' not found".
  force(fun)
  # FORCING is in the filename: a cycled run must not silently overwrite a
  # pinned one, and the two are not comparable.
  f <- file.path(OUTDIR, sprintf("99_%s_%s_%s.rds", tag, STEM, FORCING))
  if (CORES > 1L) {
    cl <- makeCluster(CORES)
    on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
    clusterExport(cl, EXPORTS, envir = environment())
    res <- parLapplyLB(cl, members, function(si)
      tryCatch(fun(si), error = function(e)
        list(sim_index = si, err = conditionMessage(e))))
    stopCluster(cl)
  } else {
    res <- lapply(members, function(si)
      tryCatch(fun(si), error = function(e)
        list(sim_index = si, err = conditionMessage(e))))
  }
  bad <- vapply(res, function(r) !is.null(r[["err"]]), logical(1))
  if (any(bad)) {
    cat("\n", sum(bad), "member(s) errored:\n")
    for (r in res[bad]) cat("  ", r$sim_index, ":", r$err, "\n")
  }
  saveRDS(list(result = res[!bad], members = members, mode = tag,
               meta = list(stem = STEM, base = BASE_FILE, ref_year = REF_YEAR,
                           pinned = PIN, forcing = FORCING,
                           enso_window = c(CYC_FROM, CYC_TO), arms = ARMS,
                           probe_years = PROBE_YEARS, drift_years = DRIFT_YEARS,
                           long = c(tmax = LONG_TMAX, tol = LONG_TOL),
                           protocol = c(ramp_tol = RAMP_TOL, ramp_tmax = RAMP_TMAX,
                                        lad_tol = LAD_TOL, lad_tmax = LAD_TMAX),
                           built = format(Sys.time()))), f)
  cat("\nWROTE", f, "\n")
  if (!length(res[!bad]))
    stop("every member errored -- nothing to summarise. See the messages above.",
         call. = FALSE)
  res[!bad]
}

WH <- c("minke whales", "orca", "sperm whales", "baleen whales")

if (mode == "timescale") {
  res <- run_parallel(probe_worker, "timescale")
  D <- bind_rows(res)
  cat("\n=== relaxation timescales, reference model (member 0), spin-up state ===\n")
  print(as.data.frame(D %>% filter(sim_index == 0, state == "spinup") %>%
    transmute(species, M = signif(M, 3), relax_years = round(relax_years),
              surv_ramp = round(surv_ramp, 3), surv_ladder = round(surv_ladder, 3),
              drift_pct_per_yr = round(100 * drift_per_yr, 4),
              unfalsifiable_ramp, unfalsifiable_ladder) %>%
    slice(match(SPN, species))), row.names = FALSE)
  cat("  surv_ramp = fraction of a perturbation still present after the ramp's\n")
  cat("  300 yr. Near 0 means steady() can clear it; near 1 means it cannot.\n")
  cat("\n=== the whale groups across members (spin-up state) ===\n")
  print(as.data.frame(D %>% filter(species %in% WH, state == "spinup",
                                   sim_index != 0) %>%
    group_by(species) %>%
    summarise(n = n(), med_relax_years = round(median(relax_years)),
              med_surv_ramp = round(median(surv_ramp), 3),
              med_drift_pct_per_yr = round(100 * median(drift_per_yr), 4),
              n_unfalsifiable_ramp = sum(unfalsifiable_ramp),
              n_unfalsifiable_ladder = sum(unfalsifiable_ladder),
              med_repro_level = round(median(repro_level, na.rm = TRUE), 4),
              .groups = "drop") %>% slice(match(WH, species))), row.names = FALSE)
  cat("\n  UNFALSIFIABLE in every member -- perturbation survives the ramp AND\n")
  cat("  the residual drift is below the tolerance steady() judges itself by:\n    ",
      paste(D %>% filter(state == "spinup", sim_index != 0) %>% group_by(species) %>%
              summarise(all = all(unfalsifiable_ramp), .groups = "drop") %>%
              filter(all) %>% pull(species), collapse = ", "), "\n")
  if (any(!D$subsidy_ok)) cat("\n  WARNING: ext_encounter changed in",
                              sum(!D$subsidy_ok), "row(s)\n")
}

if (mode == "steady") {
  res <- run_parallel(steady_worker, "steady")
  D <- bind_rows(res)
  cat("\n=== how far the stored 1841 state moves when actually re-steadied ===\n")
  cat("    (ratio = biomass after / biomass before; 1.0 = it was already steady)\n")
  print(as.data.frame(D %>% filter(species %in% WH, !errored) %>%
    group_by(arm, species) %>%
    summarise(n = n(), converged = sum(converged, na.rm = TRUE),
              med_ratio = round(median(ratio, na.rm = TRUE), 4),
              min_ratio = round(min(ratio, na.rm = TRUE), 4),
              max_ratio = round(max(ratio, na.rm = TRUE), 4),
              med_min = round(median(elapsed_s) / 60, 2), .groups = "drop") %>%
    arrange(match(arm, ARMS), match(species, WH))), row.names = FALSE)
  cat("\n=== convergence reported, by arm (all 19 species) ===\n")
  print(as.data.frame(D %>% filter(!errored, species == SPN[1]) %>%
    group_by(arm) %>% summarise(members = n(),
      reported_converged = sum(converged, na.rm = TRUE), .groups = "drop") %>%
    arrange(match(arm, ARMS))), row.names = FALSE)
  if (any(!D$subsidy_ok, na.rm = TRUE))
    cat("\n  WARNING: ext_encounter changed in", sum(!D$subsidy_ok, na.rm = TRUE),
        "row(s) -- steady() is not subsidy-safe here\n")
}

if (mode == "drift") {
  res <- run_parallel(drift_worker, "drift")
  S <- bind_rows(lapply(res, `[[`, "summary"))
  cat("\n=== where the stored state actually goes, constant climate, unfished ===\n")
  print(as.data.frame(S %>% filter(species %in% WH) %>% group_by(species) %>%
    summarise(n = n(), med_ratio_end_start = round(median(ratio_end_start), 4),
              med_half_life_yr = round(median(half_life_years, na.rm = TRUE)),
              n_still_moving = sum(abs(still_moving_pct_per_yr) > 0.01),
              .groups = "drop") %>% slice(match(WH, species))), row.names = FALSE)
  cat("\n  DRIFT_YEARS =", DRIFT_YEARS,
      "-- if n_still_moving is large the run is too short to see the equilibrium\n")
}

if (mode == "collect") {
  for (tag in c("timescale", "steady", "drift")) {
    f <- file.path(OUTDIR, sprintf("99_%s_%s_%s.rds", tag, STEM, FORCING))
    cat(sprintf("%-10s %s\n", tag, if (file.exists(f))
      paste(round(file.size(f) / 1e6, 1), "MB") else "-- not run --"))
  }
}

cat("\nelapsed", round((proc.time() - t0)[["elapsed"]] / 60, 2), "min\n")
cat("Phase 99 complete.\n")