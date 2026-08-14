# =============================================================================
# MA00 -- full prey-resolved, size-resolved consumption and mortality extraction
# for the krill attribution analysis. Rebuilt ensemble 44, cut A, top 10% = 167.
#
# WHAT THIS ADDS OVER F00_build_rebuilt167_data.R. F00 extracts consumption of
# ONE prey (antarctic krill), summed over predator size, for 1900-2010. The
# attribution analysis needs three things F00 does not carry:
#
#   1. ALL prey, not just krill        -> analysis A (partition, diet composition)
#   2. resolved by PREDATOR SIZE BIN   -> analysis B (size attribution)
#   3. predation mortality per predator-> analysis D (mortality decomposition)
#
# and it needs 1841-1860, which is before F00's DIET_YEARS window.
#
# THE PROTOCOL IS F00's, NOT A NEW ONE. Same states, same catchability
# multipliers applied the same way (46_selection_cuts.R:71-75), same projection
# call, same ther_diet(). Verified: this code path reproduces
# `Manuscript data/krill_consumption_rebuilt167.rds` for member 384 at 2005 to a
# max relative difference of EXACTLY 0. MA02 re-runs that check over the whole
# 2001-2010 overlap and refuses to report on a mismatch.
#
# WHY NO steady() IS RE-ENTERED. initial_effort is 0 for all 19 gears, so the
# steady() and the 118 yr spin-up that produced 44_states/ were UNFISHED and
# fishing enters only the 1841-2010 projection. Re-projecting a stored member is
# therefore bit-exact and the re-run reordering trap does not apply.
#
# DIET AND MORTALITY ARE THE SAME QUANTITY, MEASURED TWICE. Krill biomass killed
# per predator, computed from interaction * therMizer-scaled mizerPredRate, agrees
# with the diet-derived consumption to ~1e-16 relative. Both are stored so MA02
# can assert it rather than assume it.
#
# NOTHING EXISTING IS OVERWRITTEN. All outputs carry the MA00 prefix.
#
# USAGE
#   Rscript "Manuscript scripts/MA00_extract_full_167.R" run
#   Rscript "Manuscript scripts/MA00_extract_full_167.R" collect
# ENV: MA_CORES (default cores-2), MA_CHUNK (default 25), MA_LIMIT (0 = all)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})
source("R/wmin_test/thermizer_shim.R")

N_TOP     <- 167
OUT_LARGE <- "Output_large_files/wmin_test"
STATE_DIR <- file.path(OUT_LARGE, "44_states")
WORK_DIR  <- file.path(OUT_LARGE, "MA00_chunks")
OUT_RDS   <- file.path(OUT_LARGE, "MA00_full167.rds")
QMAX      <- 1                      # catchability ceiling, as 46_selection_cuts.R
KRILL     <- "antarctic krill"

# Periods. P0 is pre-exploitation; the other three are the ones the analysis
# brief names. P0 is carried into the size and mortality tables as well as the
# partition, because those years are extracted for analysis A regardless and it
# is what makes "the change since before exploitation" readable in B and D.
PERIODS <- list("1841-1860" = 1841:1860,   # pre-exploitation
                "1930-1970" = 1930:1970,   # whaling only, before krill fishing
                "1974-1990" = 1974:1990,   # krill fishery active
                "2001-2010" = 2001:2010)   # contemporary
# Spectrum periods for the compensation budget (analysis E). 1960-1970 is
# post-whaling / pre-krill-fishery; 2001-2010 duplicates the cached
# spectra_ref_period_rebuilt167.rds and is kept as an independent check on it.
SPEC_PERIODS <- list("1841-1860" = 1841:1860,
                     "1960-1970" = 1960:1970,
                     "2001-2010" = 2001:2010)
DIET_YEARS <- sort(unique(unlist(PERIODS, use.names = FALSE)))

dir.create(WORK_DIR, recursive = TRUE, showWarnings = FALSE)
CORES <- as.integer(Sys.getenv("MA_CORES", as.character(max(1, detectCores() - 2))))
CHUNK <- as.integer(Sys.getenv("MA_CHUNK", "25"))
LIMIT <- as.integer(Sys.getenv("MA_LIMIT", "0"))
mode  <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"

guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite an existing file: ", f, call. = FALSE); f
}

# --- membership ---------------------------------------------------------------
# TRAP: 44_selection_rules.rds$sel_unweighted is the PRE-refit ranking and shares
# only 71 of these 167. Re-derive from the post-refit SSE and demand a match.
CUTS <- readRDS(file.path(OUT_LARGE, "46_selection_cuts.rds"))
members <- CUTS$cuts[["A unweighted RMSE"]]
stopifnot(length(members) == N_TOP)
RF <- readRDS(file.path(OUT_LARGE, "45_refit_results.rds"))
chk <- RF$per_species %>% group_by(sim_index) %>%
  summarise(m = sqrt(sum(sse) / sum(n)), .groups = "drop") %>%
  arrange(m) %>% head(N_TOP) %>% pull(sim_index)
if (!identical(as.integer(chk), as.integer(members)))
  stop("cut A membership does not match the top-", N_TOP,
       " recomputed from 45_refit_results.rds -- refusing to proceed.")
message("cut A membership verified against the post-refit ranking (", N_TOP, " members)")

MULT <- readRDS(file.path(OUT_LARGE, "45_catchability_multipliers.rds"))$M
message("catchability: ", sum(MULT != 1), " species scaled | held at 1: ",
        paste(names(MULT)[MULT == 1], collapse = ", "))

if (LIMIT > 0) members <- head(members, LIMIT)
n_tot  <- length(members)
chunks <- split(seq_len(n_tot), ceiling(seq_len(n_tot) / CHUNK))
effort_arr <- readRDS("effort_array_1841_2010.rds")

# ------------------------------------------------------------------- worker ---
worker <- function(k) {
  suppressPackageStartupMessages({ library(therMizer); library(mizer) })
  source("R/wmin_test/thermizer_shim.R")
  si <- MEM[k]
  st <- readRDS(file.path(STATE_DIR, sprintf("state_treated_%05d.rds", si)))
  p  <- st$params

  # --- the re-fitted catchability, exactly as 46_selection_cuts.R:71-75 -------
  gp <- gear_params(p)
  m <- MULT[match(gp$species, names(MULT))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
  gear_params(p) <- gp

  spn  <- p@species_params$species
  nsp  <- length(spn)
  w    <- p@w; dw <- p@dw; wdw <- w * dw
  i_kr <- which(spn == KRILL)
  inter_kr <- p@interaction[, KRILL]           # predator -> krill interaction
  idx_sp <- (length(p@w_full) - length(p@w) + 1):length(p@w_full)
  DY <- sort(unique(unlist(PERIODS, use.names = FALSE)))

  # Fishing mortality on krill depends only on params and effort, never on the
  # state, so hoist it out of the year loop and out of the arm loop.
  FKR <- vapply(DY, function(y)
    mizer::getFMort(p, effort = effort_arr[as.character(y), ])[i_kr, ],
    numeric(length(w)))
  colnames(FKR) <- as.character(DY)

  proj <- function(eff) {
    if (identical(eff, 0))
      project(p, initial_n = st$initial_n, t_start = 1841, t_max = 169, effort = 0)
    else
      project(p, initial_n = st$initial_n, t_start = 1841, effort = eff)
  }
  sf <- try(proj(effort_arr), silent = TRUE)
  sc <- try(proj(0), silent = TRUE)
  if (inherits(sf, "try-error") || inherits(sc, "try-error"))
    return(list(sim_index = si, ok = FALSE, err = "projection failed"))

  # exploited runs end 2010, unexploited can carry an extra year -- clip both
  clip <- function(s) {
    yr <- as.numeric(dimnames(s@n)$time)
    list(sim = s, keep = which(yr <= 2010), yr = yr[yr <= 2010])
  }
  cf <- clip(sf); cc <- clip(sc)

  # --------------------------------------------------------------- per arm ---
  do_arm <- function(cl, fished) {
    s <- cl$sim; yrs <- cl$yr
    tnames <- as.numeric(dimnames(s@n)$time)

    cons <- ksize <- kmort <- list()
    prey_names <- NULL
    for (pn in names(PERIODS)) {
      py <- intersect(PERIODS[[pn]], yrs)
      if (!length(py)) next
      Csum  <- matrix(0, nsp, nsp + 2)
      Ksum  <- matrix(0, nsp, length(w))
      KMsum <- numeric(nsp)            # g/yr of krill killed, per predator
      Mwsum <- numeric(length(w))      # total predation mortality on krill, 1/yr by bin
      kb <- ka <- fmort_t <- bmort_t <- 0
      for (y in py) {
        ti  <- which(tnames == y)
        n   <- s@n[ti, , ]; npp <- s@n_pp[ti, ]
        te  <- ther_temp_effect(p, y)
        fl  <- ther_feeding_level(p, n = n, n_pp = npp,
                                  n_other = p@initial_n_other, year = y,
                                  temp_eff = te)
        d   <- ther_diet(p, n = n, n_pp = npp, n_other = p@initial_n_other,
                         year = y, temp_eff = te, feeding_level = fl)
        if (is.null(prey_names)) prey_names <- dimnames(d)$prey
        E   <- n * rep(dw, each = nsp)           # number of predators per bin
        # consumption of every prey by every predator, g/yr
        Csum <- Csum + apply(d, 3, function(mm) rowSums(mm * E))
        # krill consumption resolved by PREDATOR body-mass bin, g/yr
        Ksum <- Ksum + d[, , KRILL] * E

        # --- mortality: the same flux, measured in rate space ---------------
        pr <- sweep(mizer::mizerPredRate(p, n = n, n_pp = npp,
                                         n_other = p@initial_n_other, t = 0,
                                         feeding_level = fl), 1, te, "*")
        krate <- sweep(pr[, idx_sp, drop = FALSE], 1, inter_kr, "*")  # [pred, w_prey]
        nkr_b <- n[i_kr, ] * wdw                                      # krill biomass per bin
        KMsum <- KMsum + as.numeric(krate %*% nkr_b)
        Mwsum <- Mwsum + colSums(krate)

        kb <- kb + sum(nkr_b)
        ka <- ka + sum(n[i_kr, ] * dw)
        fm <- if (fished) FKR[, as.character(y)] else rep(0, length(w))
        fmort_t <- fmort_t + sum(fm * nkr_b)
        bmort_t <- bmort_t + sum(p@mu_b[i_kr, ] * nkr_b)
      }
      ny <- length(py)
      dimnames(Csum) <- list(predator = spn, prey = prey_names)
      rownames(Ksum) <- spn
      cons[[pn]]  <- Csum / ny
      ksize[[pn]] <- Ksum / ny
      kmort[[pn]] <- list(kill_g_per_yr = setNames(KMsum / ny, spn),
                          predmort_by_bin = Mwsum / ny,
                          krill_biomass_g = kb / ny,
                          krill_abundance = ka / ny,
                          fishing_g_per_yr = fmort_t / ny,
                          background_g_per_yr = bmort_t / ny,
                          n_years = ny)
    }

    # ---- period-mean species x w spectra, for the compensation budget -------
    spec <- lapply(SPEC_PERIODS, function(py) {
      ti <- cl$keep[yrs %in% py]
      Reduce(`+`, lapply(ti, function(j) s@n[j, , ])) / length(ti)
    })

    # ---- per-year, per-species biomass / abundance, as compact matrices -----
    bio <- t(vapply(cl$keep, function(ti) rowSums(sweep(s@n[ti, , ], 2, wdw, "*")),
                    numeric(nsp)))
    abu <- t(vapply(cl$keep, function(ti) rowSums(sweep(s@n[ti, , ], 2, dw, "*")),
                    numeric(nsp)))
    dimnames(bio) <- dimnames(abu) <- list(year = as.character(yrs), species = spn)

    list(cons = cons, ksize = ksize, kmort = kmort, spec = spec,
         bio = bio, abu = abu,
         n_pp = s@n_pp[cl$keep, , drop = FALSE], years = yrs)
  }

  A_f <- do_arm(cf, TRUE)
  A_c <- do_arm(cc, FALSE)

  # --- resource forcing: is it identical between the two arms? ---------------
  # plankton_forcing is exogenous, so this SHOULD be exactly 0. Measured, not
  # assumed, because it is the whole of analysis C's second question.
  den <- pmax(1e-300, abs(A_c$n_pp))
  npp_max_rel <- max(abs(A_f$n_pp - A_c$n_pp) / den)
  npp_mean <- lapply(SPEC_PERIODS, function(py)
    colMeans(A_f$n_pp[A_f$years %in% py, , drop = FALSE]))
  A_f$n_pp <- NULL; A_c$n_pp <- NULL      # 386 kB/member of exogenous forcing

  # --- kernel fields, for the constancy assertion in MA02 --------------------
  spp <- p@species_params
  kern <- data.frame(sim_index = si, species = spp$species,
                     pred_kernel_type = as.character(spp$pred_kernel_type),
                     beta = spp$beta, sigma = spp$sigma,
                     ppmr_min = spp$ppmr_min, ppmr_max = spp$ppmr_max,
                     w_min = spp$w_min, w_mat = spp$w_mat, w_max = spp$w_max,
                     stringsAsFactors = FALSE)

  list(sim_index = si, ok = TRUE,
       arms = list(exploited = A_f, unexploited = A_c),
       npp_max_rel = npp_max_rel, npp_mean = npp_mean, kern = kern)
}

# ---------------------------------------------------------------------- run ---
if (mode == "run") {
  cat("=== MA00: full consumption / mortality extraction, 167 members ===\n")
  cat("started", format(Sys.time()), "| members", n_tot, "| cores", CORES,
      "| diet years", length(DIET_YEARS), "\n")
  done <- list.files(WORK_DIR, pattern = "^ma00_\\d+\\.rds$")
  cat("chunks:", length(chunks), "| already done:", length(done), "\n\n")
  t0 <- proc.time()
  for (ci in names(chunks)) {
    rf <- file.path(WORK_DIR, sprintf("ma00_%03d.rds", as.integer(ci)))
    if (file.exists(rf)) { cat("chunk", ci, "done, skipping\n"); next }
    MEM <- members[chunks[[ci]]]
    cl <- makeCluster(CORES)
    clusterExport(cl, c("MEM", "STATE_DIR", "effort_arr", "PERIODS",
                        "SPEC_PERIODS", "KRILL", "MULT", "QMAX", "worker"),
                  envir = environment())
    r <- parLapplyLB(cl, seq_along(MEM), function(j)
      tryCatch(worker(j), error = function(e)
        list(sim_index = MEM[j], ok = FALSE, err = conditionMessage(e))))
    stopCluster(cl)
    saveRDS(r, rf)
    el <- (proc.time() - t0)["elapsed"] / 60
    nd <- length(list.files(WORK_DIR, pattern = "^ma00_\\d+\\.rds$"))
    cat(sprintf("chunk %s (%d/%d) | ok %d/%d | elapsed %.1f min | ETA %.1f min\n",
                ci, nd, length(chunks),
                sum(vapply(r, function(x) isTRUE(x$ok), logical(1))), length(r),
                el, (el / max(1, nd - length(done))) * (length(chunks) - nd)))
    rm(r); invisible(gc())
  }
  cat("\nrun complete.", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
  cat('Now: Rscript "Manuscript scripts/MA00_extract_full_167.R" collect\n')
  quit(save = "no")
}

# ------------------------------------------------------------------ collect ---
fs <- sort(list.files(WORK_DIR, pattern = "^ma00_\\d+\\.rds$", full.names = TRUE))
stopifnot(length(fs) > 0)
Z0 <- unlist(lapply(fs, readRDS), recursive = FALSE)
ok <- vapply(Z0, function(x) isTRUE(x$ok), logical(1))
cat("=== collected", sum(ok), "of", length(Z0), "members ===\n")
failed <- data.frame(sim_index = numeric(0), reason = character(0))
if (any(!ok)) {
  failed <- do.call(rbind, lapply(Z0[!ok], function(z) data.frame(
    sim_index = z$sim_index,
    reason = if (is.null(z$err)) "(no message)" else z$err)))
  cat("FAILED members:\n"); print(failed, row.names = FALSE)
}
Z <- Z0[ok]
stopifnot(length(Z) > 0)

meta <- list(n_members = length(Z),
             members = vapply(Z, function(z) z$sim_index, numeric(1)),
             n_requested = n_tot, n_top = N_TOP, failed = failed,
             cut = "A unweighted pooled log10 yield RMSE, top 10%",
             periods = PERIODS, spec_periods = SPEC_PERIODS,
             multipliers = MULT, qmax = QMAX, built = Sys.time(),
             units = c(consumption = "g/yr (domain total)",
                       biomass = "g (domain total)", body_mass = "g",
                       mortality = "1/yr"))
saveRDS(list(members = Z, meta = meta), guard(OUT_RDS))

cat("\nWrote", OUT_RDS, "\n")
cat("max relative |n_pp exploited - n_pp unexploited| over all members / years / bins:",
    max(vapply(Z, function(z) z$npp_max_rel, numeric(1))), "\n")
cat("collect complete.\n")
