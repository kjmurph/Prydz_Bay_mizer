# =============================================================================
# F00b -- arm-parameterised rebuild of the biomass + slope datasets needed for
# the SNR decomposition. A stripped-down sibling of F00_build_refined183_data.R.
#
# WHY THIS EXISTS. The biomass SNR at 2010 moved from -0.920 (published,
# 212 members) to -0.378 (refined, 183 members) and the change is entirely in
# the signal. Three candidates: membership, re-entering steady(), and the w_min
# correction. `Output_large_files/wmin_test/40_states/` holds BOTH arms for all
# 1,831 clean members, each produced by the identical steady + spin-up pipeline,
# so projecting the CONTROL arm through the same extraction isolates the
# correction (control vs treated) and, against the old cache, the re-run.
#
# DIFFERENCES FROM F00. Arm is a parameter (F0_ARM=control|treated); the diet
# extraction, the reference-period spectra and the per-species arrays are
# dropped -- the SNR needs only community biomass and the LBNbiom slope. The
# projection, the clipping and the slope routine are transcribed unchanged so
# the treated arm reproduces F00's numbers exactly.
#
# NOTHING IS OVERWRITTEN. Outputs carry the `_<arm>183` suffix.
#
# USAGE
#   Rscript "Manuscript scripts/F00b_build_arm_data.R" run
#   Rscript "Manuscript scripts/F00b_build_arm_data.R" collect
# ENV: F0_ARM (control|treated, default control), F0_CORES, F0_CHUNK, F0_LIMIT
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})
source("R/wmin_test/thermizer_shim.R")

ARM         <- Sys.getenv("F0_ARM", "control")
stopifnot(ARM %in% c("control", "treated"))
N_TOP       <- 183
STATE_DIR   <- "Output_large_files/wmin_test/40_states"
OUT_DATA    <- "Manuscript data"
WORK_DIR    <- file.path("Output_large_files/wmin_test", paste0("F00b_chunks_", ARM))
SUFFIX      <- paste0(ARM, "183")
SPECTRUM_MIN_W <- 3.16227766e-08  # canonical LBNbiom floor
dir.create(WORK_DIR, recursive = TRUE, showWarnings = FALSE)

CORES <- as.integer(Sys.getenv("F0_CORES", as.character(max(1, detectCores() - 2))))
CHUNK <- as.integer(Sys.getenv("F0_CHUNK", "25"))
LIMIT <- as.integer(Sys.getenv("F0_LIMIT", "0"))
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"

R <- readRDS("Output_large_files/wmin_test/40_refined_ranking.rds")
members <- R$ranking$sim_index[seq_len(N_TOP)]
if (LIMIT > 0) members <- head(members, LIMIT)
n_tot <- length(members)
chunks <- split(seq_len(n_tot), ceiling(seq_len(n_tot) / CHUNK))
effort_arr <- readRDS("effort_array_1841_2010.rds")

# --- canonical LBNbiom slope, transcribed from F00 (ecosystem_assessment_v3.R:493)
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
  st <- readRDS(file.path(STATE_DIR, sprintf("state_%s_%05d.rds", ARM, si)))
  p <- st$params
  sp <- p@species_params$species
  w <- p@w; dw <- p@dw; wdw <- w * dw

  proj <- function(eff) {
    if (identical(eff, 0))
      project(p, initial_n = st$initial_n, t_start = 1841, t_max = 169, effort = 0)
    else
      project(p, initial_n = st$initial_n, t_start = 1841, effort = eff)
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
  slp <- function(cl, arm) data.frame(
    sim_index = si, arm = arm, Year = cl$yr,
    slope = lbnbiom_slope_series(cl$sim)[cl$keep], stringsAsFactors = FALSE)

  list(sim_index = si, ok = TRUE,
       tab = rbind(tab(cf, "exploited"), tab(cc, "unexploited")),
       slope = rbind(slp(cf, "exploited"), slp(cc, "unexploited")))
}

# ---------------------------------------------------------------------- run ---
if (mode == "run") {
  cat("=== F00b: arm =", ARM, "| members", n_tot, "| cores", CORES, "===\n")
  cat("started", format(Sys.time()), "\n")
  t0 <- proc.time()
  for (ci in names(chunks)) {
    rf <- file.path(WORK_DIR, sprintf("f00b_%03d.rds", as.integer(ci)))
    if (file.exists(rf)) { cat("chunk", ci, "done, skipping\n"); next }
    MEM <- members[chunks[[ci]]]
    cl <- makeCluster(CORES)
    clusterExport(cl, c("MEM", "STATE_DIR", "ARM", "effort_arr", "SPECTRUM_MIN_W",
                        "lbnbiom_slope_series", "worker"), envir = environment())
    r <- parLapplyLB(cl, seq_along(MEM), function(j)
      tryCatch(worker(j), error = function(e)
        list(sim_index = MEM[j], ok = FALSE, err = conditionMessage(e))))
    stopCluster(cl)
    saveRDS(r, rf)
    el <- (proc.time() - t0)["elapsed"] / 60
    nd <- length(list.files(WORK_DIR, pattern = "^f00b_\\d+\\.rds$"))
    cat(sprintf("chunk %s (%d/%d) | ok %d/%d | elapsed %.1f min\n", ci, nd,
                length(chunks), sum(vapply(r, function(x) isTRUE(x$ok), logical(1))),
                length(r), el))
    rm(r); invisible(gc())
  }
  cat("\nrun complete.", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
  quit(save = "no")
}

# ------------------------------------------------------------------ collect ---
fs <- sort(list.files(WORK_DIR, pattern = "^f00b_\\d+\\.rds$", full.names = TRUE))
stopifnot(length(fs) > 0)
Z <- unlist(lapply(fs, readRDS), recursive = FALSE)
ok <- vapply(Z, function(x) isTRUE(x$ok), logical(1))
cat("=== collected", sum(ok), "of", length(Z), "members (arm ", ARM, ") ===\n")
if (any(!ok)) for (z in Z[!ok])
  cat("  FAILED", z$sim_index, ":", if (is.null(z$err)) "" else z$err, "\n")
Z <- Z[ok]

sfx <- function(base) file.path(OUT_DATA, sprintf("%s_%s.rds", base, SUFFIX))
guard <- function(f) if (file.exists(f))
  stop("refusing to overwrite an existing file: ", f, call. = FALSE) else f

TAB <- bind_rows(lapply(Z, `[[`, "tab"))
SLP <- bind_rows(lapply(Z, `[[`, "slope"))
cat("biomass rows:", nrow(TAB), "| slope rows:", nrow(SLP), "\n")
saveRDS(TAB %>% filter(arm == "exploited"),   guard(sfx("biomass_abund_fish")))
saveRDS(TAB %>% filter(arm == "unexploited"), guard(sfx("biomass_abund_clim")))
saveRDS(SLP, guard(sfx("nbss_slope")))
saveRDS(list(n_members = length(Z), arm = ARM,
             members = vapply(Z, function(z) z$sim_index, numeric(1)),
             n_top = N_TOP, built = Sys.time()), guard(sfx("meta")))
cat("collect complete.\n")
