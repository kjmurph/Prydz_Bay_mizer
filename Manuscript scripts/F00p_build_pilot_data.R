# =============================================================================
# F00p -- build the manuscript figure data products from the 10-MEMBER PILOT
# states (R/wmin_test/52_kernel_pilot.R), so Figures 2, 3 and 4 can be redrawn
# under the revised whale feeding kernel.
#
# THIS IS A PILOT PRODUCT. n = 10, against the manuscript's 167. Every ensemble
# statistic these figures draw -- the IQR ribbons, and above all the SNR
# denominators, which are temporal SDs of an ACROSS-MEMBER MEAN -- is far noisier
# than the published versions. Do not read a 10-member ribbon as an uncertainty
# estimate, and do not compare a 10-member SNR against the published -0.147.
#
# BOTH PILOT ARMS ARE BUILT, and that is the point. The pilot ran `current` (the
# box kernel, i.e. the status quo) and `kernel` (the revision) over the SAME 10
# members. Rendering both means the n = 10 versus n = 167 sample-size effect can
# be separated from the effect of the kernel change: compare kernel against
# current, not against the published figures.
#
# THE PROTOCOL IS F00_build_rebuilt167_data.R's, TRANSCRIBED. Same catchability
# multipliers applied the same way (46_selection_cuts.R:71-75), same projection
# calls, same LBNbiom slope, same ther_diet(). The pilot states carry the DRAWN
# catchability exactly as 44_states/ does, so the multiplier step is identical.
#
# ONE ADDITION OVER F00: krill consumption is extracted over 1841-2010 rather
# than F00's 1900-2010, so that Figure 4's +-1 SD baseline file (normally built
# separately by F00c) falls out of the same pass. The 1900-2010 subset is written
# as krill_consumption_*, the full unexploited window as
# krill_baseline_1841_unexploited_*, which is what F04 expects.
#
# ALSO SERVES THE FULL cut-A REBUILD. Point F0P_STATE_DIR and F0P_MANIFEST at
# 53_kernel_rebuild_cutA.R's outputs and it builds the same products for all 167
# members; the "pilot" caveats above then no longer apply, and the suffix should
# say so (F0P_SUFFIX).
#
# USAGE  Rscript "Manuscript scripts/F00p_build_pilot_data.R"
# ENV: F0P_TAG (default _bk05_mk05 -- which pilot's states to read)
#      F0P_CORES (default 10)
#      F0P_STATE_DIR / F0P_MANIFEST -- override to read another run's states
#      F0P_SUFFIX -- data-product suffix; default pilot<N><arm>
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})
source("R/wmin_test/thermizer_shim.R")

OUT_LARGE <- "Output_large_files/wmin_test"
OUT_DATA  <- "Manuscript data"
TAG   <- Sys.getenv("F0P_TAG", "_bk05_mk05")
CORES <- as.integer(Sys.getenv("F0P_CORES", "10"))
STATE_DIR <- Sys.getenv("F0P_STATE_DIR",
                        file.path(OUT_LARGE, sprintf("52%s_pilot_states", TAG)))
MANIFEST  <- Sys.getenv("F0P_MANIFEST",
                        file.path(OUT_LARGE, sprintf("52%s_kernel_pilot.rds", TAG)))
SUFFIX_OVERRIDE <- Sys.getenv("F0P_SUFFIX", "")
KRILL <- "antarctic krill"
QMAX  <- 1
DIET_YEARS     <- 1841:2010
SPECTRUM_MIN_W <- 3.16227766e-08

guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite an existing file: ", f, call. = FALSE); f
}
if (!dir.exists(STATE_DIR)) stop("no pilot states at ", STATE_DIR)

PILOT <- readRDS(MANIFEST)
members <- PILOT$members
ARMS <- PILOT$arms
cat("=== F00p: figure data from the", length(members), "-member pilot", TAG, "===\n")
cat("members:", paste(members, collapse = ", "), "\n")
cat("arms:", paste(ARMS, collapse = ", "), "| base:",
    paste(sprintf("%s=%s", names(PILOT$base_path), PILOT$base_path), collapse = " "), "\n")
for (a in ARMS) {
  ok <- file.exists(file.path(STATE_DIR, sprintf("state_%s_%05d.rds", a, members)))
  if (!all(ok)) stop("missing pilot states for arm ", a, ": ",
                     paste(members[!ok], collapse = ", "))
}

MULT <- readRDS(file.path(OUT_LARGE, "45_catchability_multipliers.rds"))$M
effort_arr <- readRDS("effort_array_1841_2010.rds")

# --- canonical LBNbiom slope, transcribed from F00:115-137 -------------------
#     (ecosystem_assessment_v3.R:493, Edwards et al. 2017 Method 5).
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
  si  <- JOBS$sim_index[k]; pa <- JOBS$pilot_arm[k]
  st <- readRDS(file.path(STATE_DIR, sprintf("state_%s_%05d.rds", pa, si)))
  p <- st$params

  # the re-fitted catchability, exactly as 46_selection_cuts.R:71-75
  gp <- gear_params(p)
  m <- MULT[match(gp$species, names(MULT))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
  gear_params(p) <- gp

  sp <- p@species_params$species
  w <- p@w; dw <- p@dw; wdw <- w * dw
  i_kr <- which(sp == KRILL)

  proj <- function(eff) {
    if (identical(eff, 0))
      project(p, initial_n = st$initial_n, t_start = 1841, t_max = 169, effort = 0,
              progress_bar = FALSE)
    else
      project(p, initial_n = st$initial_n, t_start = 1841, effort = eff,
              progress_bar = FALSE)
  }
  sf <- try(proj(effort_arr), silent = TRUE)
  sc <- try(proj(0), silent = TRUE)
  if (inherits(sf, "try-error") || inherits(sc, "try-error"))
    return(list(sim_index = si, pilot_arm = pa, ok = FALSE))

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
  kc <- function(cl, arm) {
    s <- cl$sim
    yrs <- intersect(DIET_YEARS, cl$yr)
    do.call(rbind, lapply(yrs, function(y) {
      ti <- which(as.numeric(dimnames(s@n)$time) == y)
      n <- s@n[ti, , ]; npp <- s@n_pp[ti, ]
      d <- ther_diet(s@params, n = n, n_pp = npp,
                     n_other = s@params@initial_n_other, year = y)
      data.frame(sim_index = si, arm = arm, Year = y, Species = sp,
                 krill_consumed = rowSums(d[, , i_kr, drop = TRUE] * n *
                                            rep(dw, each = length(sp))),
                 stringsAsFactors = FALSE)
    }))
  }
  list(sim_index = si, pilot_arm = pa, ok = TRUE,
       tab   = rbind(tab(cf, "exploited"), tab(cc, "unexploited")),
       slope = rbind(slp(cf, "exploited"), slp(cc, "unexploited")),
       krill = rbind(kc(cf, "exploited"),  kc(cc, "unexploited")))
}

# ---------------------------------------------------------------------- run ---
JOBS <- expand.grid(sim_index = members, pilot_arm = ARMS,
                    stringsAsFactors = FALSE)
cat("jobs:", nrow(JOBS), "| cores", CORES, "\n\n")
t0 <- proc.time()
cl <- makeCluster(min(CORES, nrow(JOBS)))
clusterExport(cl, c("JOBS", "STATE_DIR", "effort_arr", "MULT", "QMAX", "KRILL",
                    "DIET_YEARS", "SPECTRUM_MIN_W", "lbnbiom_slope_series",
                    "worker"), envir = environment())
Z <- parLapplyLB(cl, seq_len(nrow(JOBS)), function(j)
  tryCatch(worker(j), error = function(e)
    list(sim_index = JOBS$sim_index[j], pilot_arm = JOBS$pilot_arm[j],
         ok = FALSE, err = conditionMessage(e))))
stopCluster(cl)
ok <- vapply(Z, function(x) isTRUE(x$ok), logical(1))
cat("completed", sum(ok), "of", length(Z), "jobs in",
    round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
if (any(!ok)) {
  for (z in Z[!ok]) cat("  FAILED", z$pilot_arm, z$sim_index, ":",
                        if (is.null(z$err)) "(no message)" else z$err, "\n")
  stop("not all pilot jobs completed")
}

# --- write one set of data products per pilot arm ----------------------------
for (pa in ARMS) {
  ZZ <- Z[vapply(Z, function(z) z$pilot_arm == pa, logical(1))]
  # With one arm the suffix override is used verbatim; with several it is
  # extended by the arm name so the arms cannot collide.
  SUF <- if (nzchar(SUFFIX_OVERRIDE)) {
    if (length(ARMS) > 1) paste0(SUFFIX_OVERRIDE, pa) else SUFFIX_OVERRIDE
  } else sprintf("pilot%d%s", length(members), pa)
  sfx <- function(base) file.path(OUT_DATA, sprintf("%s_%s.rds", base, SUF))

  TAB <- bind_rows(lapply(ZZ, `[[`, "tab"))
  SLP <- bind_rows(lapply(ZZ, `[[`, "slope"))
  KRA <- bind_rows(lapply(ZZ, `[[`, "krill"))

  saveRDS(TAB %>% filter(arm == "exploited"),   guard(sfx("biomass_abund_fish")))
  saveRDS(TAB %>% filter(arm == "unexploited"), guard(sfx("biomass_abund_clim")))
  saveRDS(SLP, guard(sfx("nbss_slope")))
  # F04 reads the 1900-2010 window from krill_consumption_* and the full
  # 1841-2010 unexploited record from krill_baseline_1841_unexploited_*.
  saveRDS(KRA %>% filter(Year >= 1900), guard(sfx("krill_consumption")))
  saveRDS(KRA %>% filter(arm == "unexploited"),
          guard(sfx("krill_baseline_1841_unexploited")))

  saveRDS(list(n_members = length(members), members = members,
               n_top = length(members), pilot_arm = pa,
               base_params = unname(PILOT$base_path[[pa]]),
               cut = sprintf("PILOT n=%d, head of cut A, arm '%s'",
                             length(members), pa),
               multipliers = MULT, qmax = QMAX, diet_years = DIET_YEARS,
               spectrum_min_w = SPECTRUM_MIN_W, source_pilot = TAG,
               built = Sys.time(),
               provenance = paste("10-member pilot from R/wmin_test/52_kernel_pilot.R;",
                                  "NOT an ensemble -- n = 10, statistics are indicative only")),
          guard(sfx("meta")))
  cat(sprintf("  arm %-8s -> suffix '%s' | %d biomass rows, %d slope rows, %d krill rows\n",
              pa, SUF, nrow(TAB), nrow(SLP), nrow(KRA)))
}
cat("\nF00p complete.\n")