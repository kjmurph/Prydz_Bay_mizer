# =============================================================================
# F00 -- build every derived dataset the four manuscript figures need, from the
# REFINED 183-member fitted ensemble.
#
# WHY THIS EXISTS. The published figures were built from a 212-member fitted set
# drawn from a 2,111-member ensemble that turned out to contain 114 exact
# duplicates (repeated Monte Carlo configurations concatenated) and 166 members
# with a degenerate reproduction parameterisation. The chain is now:
#
#   2,111 accepted -> 1,997 distinct -> 1,831 clean -> top 10% = 183
#
# and the model carries the corrected `small divers` w_min (3626.667 g) with its
# erepro/R_max pair recalibrated. See docs/small_divers_wmin_SYNTHESIS.md and
# docs/ensemble_deduplication.md.
#
# NOTE ON 183. ceiling(1831 * 0.10) = 184, which is the rule used elsewhere in
# this repository (212 from 2,111). 183 is used here at Kieran's instruction.
# If the methods describe the top 10% as a ceiling, that wording needs adjusting.
#
# WHICH ARM. The CORRECTED (treated) model. Exploited vs unexploited are compared
# within it -- unexploited is the same params with effort = 0, the true matched
# control used by run_climate_only_ensemble.R:63-67.
#
# WHY IT STARTS FROM CACHED STATES. `initial_effort` is 0 for all 19 gears, so
# steady() and the spin-up are UNFISHED and fishing enters only the 1841-2010
# projection. R/wmin_test/40_vm_project_survivors.R already did the steady() and
# spin-up for every member and cached the resulting params + abundances, so this
# only has to project. No steady() is re-entered, which also means the
# re-run reordering trap does not apply here.
#
# THE FIGURE 4 BUG, FIXED. extract_whale_consumption.R:144 calls
# getDiet(params, n, n_pp, n_other, proportion = FALSE) with no `t`, so
# getFeedingLevel defaults to t = 0 and indexes ocean_temp at 0 + t_idx = -1841.
# Its outputs disagree with a faithful recomputation by 13-80x. This uses
# ther_diet() from R/wmin_test/thermizer_shim.R, which takes `year` explicitly
# and was verified bit-identical to mizer::getDiet(proportion = FALSE) (max
# relative difference 0) once the temperature scaling is threaded through.
#
# NOTHING IS OVERWRITTEN. Every output carries the `_refined183` suffix and no
# such file exists.
#
# USAGE
#   Rscript "Manuscript scripts/F00_build_refined183_data.R" run
#   Rscript "Manuscript scripts/F00_build_refined183_data.R" collect
# ENV: F0_CORES (default cores-2), F0_CHUNK (default 25), F0_LIMIT (0 = all),
#      F0_DIET (1|0, skip the expensive diet extraction for a fast rebuild)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})
source("R/wmin_test/thermizer_shim.R")

N_TOP       <- 183
STATE_DIR   <- "Output_large_files/wmin_test/40_states"
OUT_DATA    <- "Manuscript data"
WORK_DIR    <- "Output_large_files/wmin_test/F00_chunks"
SUFFIX      <- "refined183"
REF_PERIOD  <- 2001:2010          # contemporary reference window for the spectra
DIET_YEARS  <- 1900:2010          # Figure 4's x-axis
SPECTRUM_MIN_W <- 3.16227766e-08  # canonical LBNbiom floor
KRILL       <- "antarctic krill"
dir.create(WORK_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DATA, showWarnings = FALSE)

CORES <- as.integer(Sys.getenv("F0_CORES", as.character(max(1, detectCores() - 2))))
CHUNK <- as.integer(Sys.getenv("F0_CHUNK", "25"))
LIMIT <- as.integer(Sys.getenv("F0_LIMIT", "0"))
DO_DIET <- as.integer(Sys.getenv("F0_DIET", "1")) == 1
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"

# --- membership ---------------------------------------------------------------
R <- readRDS("Output_large_files/wmin_test/40_refined_ranking.rds")
members <- R$ranking$sim_index[seq_len(N_TOP)]
if (LIMIT > 0) members <- head(members, LIMIT)
n_tot <- length(members)
chunks <- split(seq_len(n_tot), ceiling(seq_len(n_tot) / CHUNK))
effort_arr <- readRDS("effort_array_1841_2010.rds")

# --- canonical LBNbiom slope, transcribed from ---------------------------------
#     ecosystem_assessment_v3.R:493 (Edwards et al. 2017 Method 5).
#     Do NOT substitute a plain OLS on the native spectrum.
lbnbiom_slope_series <- function(sim, min_w = SPECTRUM_MIN_W) {
  p <- sim@params; w <- p@w; dw <- p@dw
  w_filt <- w[w >= min_w]
  if (length(w_filt) < 3) return(rep(NA_real_, dim(sim@n)[1]))
  bin_breaks <- 2^(floor(log2(min(w_filt))):ceiling(log2(max(w_filt))))
  n_bins <- length(bin_breaks) - 1
  bin_idx <- findInterval(w, bin_breaks, rightmost.closed = TRUE)
  oct_width <- diff(bin_breaks)
  oct_mid <- sqrt(bin_breaks[-length(bin_breaks)] * bin_breaks[-1])
  keep <- which(w >= min_w)
  vapply(seq_len(dim(sim@n)[1]), function(ti) {
    bpb <- colSums(sim@n[ti, , ]) * w * dw
    ob <- numeric(n_bins)
    for (j in keep) {
      b <- bin_idx[j]
      if (b >= 1 && b <= n_bins) ob[b] <- ob[b] + bpb[j]
    }
    nbs <- ob / oct_width
    v <- which(nbs > 0)
    if (length(v) < 3) return(NA_real_)
    unname(coef(lm(log10(nbs[v]) ~ log10(oct_mid[v])))[2])
  }, numeric(1))
}

# ------------------------------------------------------------------- worker ---
worker <- function(k) {
  suppressPackageStartupMessages({ library(therMizer); library(mizer) })
  source("R/wmin_test/thermizer_shim.R")
  si <- MEM[k]
  st <- readRDS(file.path(STATE_DIR, sprintf("state_treated_%05d.rds", si)))
  p <- st$params
  sp <- p@species_params$species
  w <- p@w; dw <- p@dw; wdw <- w * dw
  i_kr <- which(sp == KRILL)

  proj <- function(eff) {
    s <- if (identical(eff, 0))
      project(p, initial_n = st$initial_n, t_start = 1841, t_max = 169, effort = 0)
    else
      project(p, initial_n = st$initial_n, t_start = 1841, effort = eff)
    s
  }
  sf <- try(proj(effort_arr), silent = TRUE)
  sc <- try(proj(0), silent = TRUE)
  if (inherits(sf, "try-error") || inherits(sc, "try-error"))
    return(list(sim_index = si, ok = FALSE))

  # exploited runs end 2010, unexploited can carry an extra year -- clip both
  clip <- function(s) {
    yr <- as.numeric(dimnames(s@n)$time)
    list(sim = s, keep = which(yr <= 2010), yr = yr[yr <= 2010])
  }
  cf <- clip(sf); cc <- clip(sc)

  # --- per-year, per-species biomass / abundance / mean mass -----------------
  tab <- function(cl, arm) {
    s <- cl$sim
    do.call(rbind, lapply(cl$keep, function(ti) {
      n <- s@n[ti, , ]
      bm <- rowSums(sweep(n, 2, wdw, "*"))
      ab <- rowSums(sweep(n, 2, dw, "*"))
      data.frame(sim_index = si, arm = arm,
                 Year = as.numeric(dimnames(s@n)$time)[ti],
                 Species = sp, Biomass = bm, Abundance = ab,
                 MeanMass = ifelse(ab > 0, bm / ab, NA_real_),
                 stringsAsFactors = FALSE)
    }))
  }

  # --- size-spectrum slope ---------------------------------------------------
  slp <- function(cl, arm) data.frame(
    sim_index = si, arm = arm, Year = cl$yr,
    slope = lbnbiom_slope_series(cl$sim)[cl$keep], stringsAsFactors = FALSE)

  # --- community + per-species spectrum, mean over the reference period ------
  spec <- function(cl, arm) {
    s <- cl$sim
    yr <- as.numeric(dimnames(s@n)$time)
    idx <- which(yr %in% REF_PERIOD)
    nsp <- Reduce(`+`, lapply(idx, function(ti) s@n[ti, , ])) / length(idx)
    list(arm = arm, sim_index = si, w = w,
         community = colSums(nsp),      # n(w), community
         by_species = nsp)              # n(w) per species
  }

  out <- list(sim_index = si, ok = TRUE,
              tab = rbind(tab(cf, "exploited"), tab(cc, "unexploited")),
              slope = rbind(slp(cf, "exploited"), slp(cc, "unexploited")),
              spec_f = spec(cf, "exploited"), spec_c = spec(cc, "unexploited"))

  # --- krill consumption per predator, per year ------------------------------
  # ther_diet() takes `year` EXPLICITLY. This is the fix for the t = 0 bug in
  # extract_whale_consumption.R, which indexed ocean_temp at -1841.
  if (DO_DIET) {
    kc <- function(cl, arm) {
      s <- cl$sim
      yrs <- intersect(DIET_YEARS, cl$yr)
      do.call(rbind, lapply(yrs, function(y) {
        ti <- which(as.numeric(dimnames(s@n)$time) == y)
        n <- s@n[ti, , ]; npp <- s@n_pp[ti, ]
        d <- ther_diet(s@params, n = n, n_pp = npp,
                       n_other = s@params@initial_n_other, year = y)
        # consumption of krill by each predator: diet[pred, w, krill] * n * dw
        cons <- rowSums(d[, , i_kr, drop = TRUE] * n *
                          rep(dw, each = length(sp)))
        data.frame(sim_index = si, arm = arm, Year = y, Species = sp,
                   krill_consumed = cons, stringsAsFactors = FALSE)
      }))
    }
    out$krill <- rbind(kc(cf, "exploited"), kc(cc, "unexploited"))
  }
  out
}

# ---------------------------------------------------------------------- run ---
if (mode == "run") {
  cat("=== F00: building refined183 figure data ===\n")
  cat("started", format(Sys.time()), "| members", n_tot, "| cores", CORES,
      "| diet", DO_DIET, "\n")
  done <- list.files(WORK_DIR, pattern = "^f00_\\d+\\.rds$")
  cat("chunks:", length(chunks), "| done:", length(done), "\n\n")
  t0 <- proc.time()
  for (ci in names(chunks)) {
    rf <- file.path(WORK_DIR, sprintf("f00_%03d.rds", as.integer(ci)))
    if (file.exists(rf)) { cat("chunk", ci, "done, skipping\n"); next }
    MEM <- members[chunks[[ci]]]
    cl <- makeCluster(CORES)
    clusterExport(cl, c("MEM", "STATE_DIR", "effort_arr", "REF_PERIOD",
                        "DIET_YEARS", "SPECTRUM_MIN_W", "KRILL", "DO_DIET",
                        "lbnbiom_slope_series", "worker"), envir = environment())
    r <- parLapplyLB(cl, seq_along(MEM), function(j)
      tryCatch(worker(j), error = function(e)
        list(sim_index = MEM[j], ok = FALSE, err = conditionMessage(e))))
    stopCluster(cl)
    saveRDS(r, rf)
    el <- (proc.time() - t0)["elapsed"] / 60
    nd <- length(list.files(WORK_DIR, pattern = "^f00_\\d+\\.rds$"))
    cat(sprintf("chunk %s (%d/%d) | ok %d/%d | elapsed %.1f min | ETA %.1f min\n",
                ci, nd, length(chunks),
                sum(vapply(r, function(x) isTRUE(x$ok), logical(1))), length(r),
                el, (el / max(1, nd - length(done))) * (length(chunks) - nd)))
    rm(r); invisible(gc())
  }
  cat("\nrun complete.", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
  cat('Now: Rscript "Manuscript scripts/F00_build_refined183_data.R" collect\n')
  quit(save = "no")
}

# ------------------------------------------------------------------ collect ---
fs <- sort(list.files(WORK_DIR, pattern = "^f00_\\d+\\.rds$", full.names = TRUE))
stopifnot(length(fs) > 0)
Z <- unlist(lapply(fs, readRDS), recursive = FALSE)
ok <- vapply(Z, function(x) isTRUE(x$ok), logical(1))
cat("=== collected", sum(ok), "of", length(Z), "members ===\n")
if (any(!ok)) {
  cat("FAILED members:", paste(vapply(Z[!ok], function(x) x$sim_index, numeric(1)),
                               collapse = ", "), "\n")
  for (z in Z[!ok]) if (!is.null(z$err)) cat("  ", z$sim_index, ":", z$err, "\n")
}
Z <- Z[ok]
stopifnot(length(Z) > 0)

sfx <- function(base, ext = "rds") file.path(OUT_DATA, sprintf("%s_%s.%s", base, SUFFIX, ext))
guard <- function(f) if (file.exists(f))
  stop("refusing to overwrite an existing file: ", f, call. = FALSE) else f

TAB <- bind_rows(lapply(Z, `[[`, "tab"))
SLP <- bind_rows(lapply(Z, `[[`, "slope"))
cat("biomass/abundance rows:", nrow(TAB), "| slope rows:", nrow(SLP), "\n")

saveRDS(TAB %>% filter(arm == "exploited"),   guard(sfx("biomass_abund_fish")))
saveRDS(TAB %>% filter(arm == "unexploited"), guard(sfx("biomass_abund_clim")))
saveRDS(SLP, guard(sfx("nbss_slope")))

w_grid <- Z[[1]]$spec_f$w
mk_spec <- function(field) {
  m <- t(vapply(Z, function(z) z[[field]]$community, numeric(length(w_grid))))
  rownames(m) <- vapply(Z, function(z) as.character(z$sim_index), character(1))
  m
}
sp_names <- readRDS("Manuscript data/params_sel_adj.rds")@species_params$species
mk_sp <- function(field) {
  a <- array(NA_real_, dim = c(length(Z), length(sp_names), length(w_grid)),
             dimnames = list(sim = vapply(Z, function(z) as.character(z$sim_index),
                                          character(1)),
                             species = sp_names, w = NULL))
  for (k in seq_along(Z)) a[k, , ] <- Z[[k]][[field]]$by_species
  a
}
saveRDS(list(w = w_grid,
             community_fished = mk_spec("spec_f"),
             community_clim   = mk_spec("spec_c"),
             species_fished   = mk_sp("spec_f"),
             species_clim     = mk_sp("spec_c"),
             ref_period = REF_PERIOD, sp_names = sp_names),
        guard(sfx("spectra_ref_period")))

if (DO_DIET && !is.null(Z[[1]]$krill)) {
  KR <- bind_rows(lapply(Z, `[[`, "krill"))
  cat("krill consumption rows:", nrow(KR), "\n")
  saveRDS(KR, guard(sfx("krill_consumption")))
}

meta <- list(n_members = length(Z), members = vapply(Z, function(z) z$sim_index, numeric(1)),
             n_top = N_TOP, arm = "corrected w_min (treated)",
             ref_period = REF_PERIOD, diet_years = DIET_YEARS,
             spectrum_min_w = SPECTRUM_MIN_W, built = Sys.time(),
             provenance = "2111 accepted -> 1997 distinct -> 1831 clean -> top 183")
saveRDS(meta, guard(sfx("meta")))

cat("\nWrote to", OUT_DATA, "with suffix _", SUFFIX, ":\n", sep = "")
for (f in list.files(OUT_DATA, pattern = SUFFIX, full.names = FALSE))
  cat("  ", f, "\n")
cat("\ncollect complete.\n")
