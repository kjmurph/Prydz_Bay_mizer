# =============================================================================
# F00f -- the remaining figure inputs for the NRMSE_sd top-10% cut (167 members),
# so Figures 2 (1 g variant), 3 and 4 can be rebuilt on it.
#
# Figure 3 needs only biomass / abundance / mean mass, which
# R/wmin_test/50_nrmse167_figure_data.R already wrote. This adds everything else,
# in ONE projection pass per member per arm:
#
#   nbss_slope_nrmse167.rds                       full-range LBNbiom slope
#   biomass_abund_{fish,clim}_1g_nrmse167.rds     1 g cutoff, Figure 2's inputs
#   nbss_slope_1g_nrmse167.rds                    1 g cutoff slope
#   meta_1g_nrmse167.rds
#   krill_consumption_nrmse167.rds                both arms, 1900-2010 (Fig 4 x-axis)
#   krill_baseline_1841_unexploited_nrmse167.rds  unexploited, 1841-2010 (Fig 4 band)
#
# WHY ONE SCRIPT RATHER THAN THREE. For cut A these products came from F00, F00c
# and F00d, each of which projects the same members again. Here the three
# reductions are taken off a single projection, which is both faster and removes
# any possibility of the three disagreeing.
#
# THE VALIDATION. Each worker also computes the FULL-RANGE biomass and abundance,
# which must reproduce 50_nrmse167_figure_data.R's tables exactly -- same states,
# same multipliers, same protocol. `collect` checks and STOPS above 1e-10. That
# is what makes the 1 g and diet numbers trustworthy: they are further reductions
# of a projection proven to reproduce an independent extraction. (Cut A's F00d
# checks against F00 the same way; there is no slope reference for this cut, so
# the full-range slope is written here rather than checked.)
#
# THE FIGURE 4 BUG IS AVOIDED, as in F00: extract_whale_consumption.R:144 calls
# getDiet() with no `t`, so ocean_temp is indexed at 0 + t_idx = -1841. ther_diet()
# from R/wmin_test/thermizer_shim.R takes `year` explicitly and was verified
# bit-identical to mizer::getDiet(proportion = FALSE).
#
# Usage:  Rscript "Manuscript scripts/F00f_build_nrmse167_extras.R"
#         Rscript "Manuscript scripts/F00f_build_nrmse167_extras.R" collect
# ENV: F0_CORES (default 14), F0_CHUNK (default 25), F0_LIMIT (0 = all)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})
source("R/wmin_test/thermizer_shim.R")

OUT_LARGE  <- "Output_large_files/wmin_test"
STATE_DIR  <- file.path(OUT_LARGE, "44_states")
OUT_DATA   <- "Manuscript data"
WORK_DIR   <- file.path(OUT_LARGE, "F00f_chunks_nrmse167")
SUFFIX     <- "nrmse167"
SUFFIX_1G  <- "1g_nrmse167"
MIN_W      <- 1                   # the cutoff, grams
FULL_MIN_W <- 3.16227766e-08      # F00's canonical LBNbiom floor
BASE_YEARS <- 1841:2010           # Figure 4's SD band window
DIET_YEARS <- 1900:2010           # Figure 4's x axis
KRILL      <- "antarctic krill"
QMAX       <- 1
TOL        <- 1e-10
dir.create(WORK_DIR, recursive = TRUE, showWarnings = FALSE)

CORES <- as.integer(Sys.getenv("F0_CORES", "14"))
CHUNK <- as.integer(Sys.getenv("F0_CHUNK", "25"))
LIMIT <- as.integer(Sys.getenv("F0_LIMIT", "0"))
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"

sfx   <- function(stem, s) file.path(OUT_DATA, sprintf("%s_%s.rds", stem, s))
guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE); f
}

# --- membership ---------------------------------------------------------------
# The NRMSE_sd cut, from R/wmin_test/49_rank_nrmse_sd.R. Re-derived here from the
# stored per-species table and required to match, so a stale or edited selection
# cannot pass silently -- the same guard F00/F00c/F00d apply to cut A.
NR <- readRDS(file.path(OUT_LARGE, "49_rank_nrmse_sd.rds"))
members <- NR$selected
N_TOP <- length(members)
chk <- NR$per_species %>% group_by(sim_index) %>%
  summarise(o = mean(nrmse_sd), .groups = "drop") %>%
  arrange(o) %>% head(N_TOP) %>% pull(sim_index)
if (!setequal(as.integer(chk), as.integer(members)))
  stop("NRMSE_sd membership does not match the top-", N_TOP,
       " recomputed from 49_rank_nrmse_sd.rds -- refusing to proceed.")
message("NRMSE_sd membership verified (", N_TOP, " members)")

MULT <- readRDS(file.path(OUT_LARGE, "45_catchability_multipliers.rds"))$M
if (LIMIT > 0) members <- head(members, LIMIT)
n_tot <- length(members)
chunks <- split(seq_len(n_tot), ceiling(seq_len(n_tot) / CHUNK))
effort_arr <- readRDS("effort_array_1841_2010.rds")

# --- canonical LBNbiom slope, transcribed from F00d:93-115 unchanged -----------
lbnbiom_slope_series <- function(sim, min_w) {
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

  gp <- gear_params(p)
  m <- MULT[match(gp$species, names(MULT))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
  gear_params(p) <- gp

  sp <- p@species_params$species
  w <- p@w; dw <- p@dw; wdw <- w * dw
  keep_w <- which(w >= MIN_W)
  i_kr <- which(sp == KRILL)

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

  clip <- function(s) {
    yr <- as.numeric(dimnames(s@n)$time)
    list(sim = s, keep = which(yr <= 2010), yr = yr[yr <= 2010])
  }
  cf <- clip(sf); cc <- clip(sc)

  tab <- function(cl, arm, cols) {
    s <- cl$sim
    do.call(rbind, lapply(cl$keep, function(ti) {
      n <- s@n[ti, , ]
      if (!is.null(cols)) n <- n[, cols, drop = FALSE]
      ww <- if (is.null(cols)) wdw else wdw[cols]
      dd <- if (is.null(cols)) dw  else dw[cols]
      bm <- rowSums(sweep(n, 2, ww, "*"))
      ab <- rowSums(sweep(n, 2, dd, "*"))
      data.frame(sim_index = si, arm = arm,
                 Year = as.numeric(dimnames(s@n)$time)[ti],
                 Species = sp, Biomass = bm, Abundance = ab,
                 MeanMass = ifelse(ab > 0, bm / ab, NA_real_),
                 stringsAsFactors = FALSE)
    }))
  }
  slp <- function(cl, arm, mw) data.frame(
    sim_index = si, arm = arm, Year = cl$yr,
    slope = lbnbiom_slope_series(cl$sim, mw)[cl$keep], stringsAsFactors = FALSE)

  # --- krill consumption per predator per year -------------------------------
  # ther_diet() takes `year` EXPLICITLY -- the fix for the t = 0 bug.
  kc <- function(cl, arm, yrs_want) {
    s <- cl$sim
    yrs <- intersect(yrs_want, cl$yr)
    do.call(rbind, lapply(yrs, function(y) {
      ti <- which(as.numeric(dimnames(s@n)$time) == y)
      n <- s@n[ti, , ]; npp <- s@n_pp[ti, ]
      d <- ther_diet(s@params, n = n, n_pp = npp,
                     n_other = s@params@initial_n_other, year = y)
      cons <- rowSums(d[, , i_kr, drop = TRUE] * n * rep(dw, each = length(sp)))
      data.frame(sim_index = si, arm = arm, Year = y, Species = sp,
                 krill_consumed = cons, stringsAsFactors = FALSE)
    }))
  }

  list(sim_index = si, ok = TRUE,
       tab_full = rbind(tab(cf, "exploited", NULL), tab(cc, "unexploited", NULL)),
       tab_1g   = rbind(tab(cf, "exploited", keep_w), tab(cc, "unexploited", keep_w)),
       slope_full = rbind(slp(cf, "exploited", FULL_MIN_W),
                          slp(cc, "unexploited", FULL_MIN_W)),
       slope_1g   = rbind(slp(cf, "exploited", MIN_W),
                          slp(cc, "unexploited", MIN_W)),
       # exploited arm only over the figure window; the unexploited arm is taken
       # over the full baseline window and Figure 4's window is a subset of it
       krill_f = kc(cf, "exploited",   DIET_YEARS),
       krill_c = kc(cc, "unexploited", BASE_YEARS))
}

# ---------------------------------------------------------------------- run ---
if (mode == "run") {
  cat("=== F00f: NRMSE_sd cut extras (slope, 1 g cutoff, diet) ===\n")
  cat("started", format(Sys.time()), "| members", n_tot, "| cores", CORES, "\n")
  cat("chunks:", length(chunks), "| done:",
      length(list.files(WORK_DIR, pattern = "^f00f_\\d+\\.rds$")), "\n\n")
  t0 <- proc.time()
  for (ci in names(chunks)) {
    rf <- file.path(WORK_DIR, sprintf("f00f_%03d.rds", as.integer(ci)))
    if (file.exists(rf)) { cat("chunk", ci, "done, skipping\n"); next }
    MEM <- members[chunks[[ci]]]
    cl <- makeCluster(CORES)
    clusterExport(cl, c("MEM", "STATE_DIR", "effort_arr", "MULT", "QMAX",
                        "MIN_W", "FULL_MIN_W", "BASE_YEARS", "DIET_YEARS",
                        "KRILL", "lbnbiom_slope_series", "worker"),
                  envir = environment())
    r <- parLapplyLB(cl, seq_along(MEM), function(j)
      tryCatch(worker(j), error = function(e)
        list(sim_index = MEM[j], ok = FALSE, err = conditionMessage(e))))
    stopCluster(cl)
    saveRDS(r, rf)
    nd <- length(list.files(WORK_DIR, pattern = "^f00f_\\d+\\.rds$"))
    el <- (proc.time() - t0)[["elapsed"]] / 60
    cat(sprintf("chunk %s (%d/%d): %d/%d ok | %.1f min | ETA %.1f min\n", ci,
                nd, length(chunks), sum(vapply(r, `[[`, logical(1), "ok")),
                length(r), el, el / nd * (length(chunks) - nd)))
    rm(r); invisible(gc())
  }
  cat("\nrun complete. now: Rscript ... collect\n")
  quit(save = "no")
}

# ------------------------------------------------------------------ collect ---
files <- sort(list.files(WORK_DIR, pattern = "^f00f_\\d+\\.rds$", full.names = TRUE))
if (!length(files)) stop("no chunk files in ", WORK_DIR, " -- run first")
Z <- unlist(lapply(files, readRDS), recursive = FALSE)
bad <- Z[!vapply(Z, `[[`, logical(1), "ok")]
if (length(bad)) {
  message("FAILED members: ",
          paste(vapply(bad, `[[`, numeric(1), "sim_index"), collapse = ", "))
  stop("refusing to write a partial rebuild")
}
if (length(Z) != n_tot)
  stop("collected ", length(Z), " members, expected ", n_tot)

TABF <- bind_rows(lapply(Z, `[[`, "tab_full"))
TAB1 <- bind_rows(lapply(Z, `[[`, "tab_1g"))
SLPF <- bind_rows(lapply(Z, `[[`, "slope_full"))
SLP1 <- bind_rows(lapply(Z, `[[`, "slope_1g"))
KRF  <- bind_rows(lapply(Z, `[[`, "krill_f"))
KRC  <- bind_rows(lapply(Z, `[[`, "krill_c"))
cat("rows: tab_full", nrow(TABF), "| tab_1g", nrow(TAB1),
    "| slope", nrow(SLPF), "| krill", nrow(KRF) + nrow(KRC), "\n")

# --- THE CHECK: full-range must reproduce 50_nrmse167_figure_data.R -----------
relmax <- function(new, old) {
  den <- pmax(abs(old), .Machine$double.eps)
  max(abs(new - old) / den, na.rm = TRUE)
}
MEMS <- sort(unique(TABF$sim_index))
chk_tab <- function(a, stem) {
  o <- readRDS(sfx(stem, SUFFIX)) %>% filter(sim_index %in% MEMS)
  j <- TABF %>% filter(arm == a) %>%
    select(sim_index, Year, Species, Bn = Biomass, An = Abundance) %>%
    inner_join(o %>% select(sim_index, Year, Species,
                            Bo = Biomass, Ao = Abundance),
               by = c("sim_index", "Year", "Species"))
  if (nrow(j) != nrow(o))
    stop(stem, ": join gave ", nrow(j), " rows, expected ", nrow(o))
  c(biomass = relmax(j$Bn, j$Bo), abundance = relmax(j$An, j$Ao))
}
d1 <- chk_tab("exploited",   "biomass_abund_fish")
d2 <- chk_tab("unexploited", "biomass_abund_clim")
cat("\n=== full-range rebuild vs 50_nrmse167_figure_data.R ===\n")
cat(sprintf("  exploited   biomass %.3g | abundance %.3g\n", d1[1], d1[2]))
cat(sprintf("  unexploited biomass %.3g | abundance %.3g\n", d2[1], d2[2]))
worst <- max(d1, d2)
if (worst > TOL)
  stop("full-range rebuild does not reproduce the extraction (max rel diff ",
       signif(worst, 3), " > ", TOL, ") -- refusing to write")
cat("reproduces it exactly. The 1 g and diet outputs come from the same runs.\n")

# --- the internal consistency of the two krill products ----------------------
# The unexploited arm is extracted once over 1841-2010; Figure 4's 1900-2010
# window is a strict subset, so the two files share those rows by construction
# rather than by a second extraction. Recorded so the equality is not mistaken
# for an independent check.
KR <- bind_rows(KRF, KRC %>% filter(Year %in% DIET_YEARS))
cat("krill_consumption rows:", nrow(KR), "| baseline rows:", nrow(KRC), "\n")

meta <- readRDS(sfx("meta", SUFFIX))
saveRDS(SLPF, guard(sfx("nbss_slope", SUFFIX)))
saveRDS(TAB1 %>% filter(arm == "exploited"),   guard(sfx("biomass_abund_fish", SUFFIX_1G)))
saveRDS(TAB1 %>% filter(arm == "unexploited"), guard(sfx("biomass_abund_clim", SUFFIX_1G)))
saveRDS(SLP1, guard(sfx("nbss_slope", SUFFIX_1G)))
meta1 <- meta
meta1$min_w_cutoff <- MIN_W
meta1$note <- paste("Figure 2 inputs recomputed with a", MIN_W,
                    "g minimum body-mass cutoff on both metrics")
saveRDS(meta1, guard(sfx("meta", SUFFIX_1G)))
saveRDS(KR,  guard(sfx("krill_consumption", SUFFIX)))
saveRDS(KRC, guard(file.path(OUT_DATA,
  sprintf("krill_baseline_1841_unexploited_%s.rds", SUFFIX))))
cat("\nWrote 7 files for the NRMSE_sd cut\n")

# --- headline numbers ---------------------------------------------------------
snr2010 <- function(tf, tc) {
  f <- tf %>% group_by(sim_index, Year) %>% summarise(v = sum(Biomass), .groups = "drop")
  cc <- tc %>% group_by(sim_index, Year) %>% summarise(v = sum(Biomass), .groups = "drop")
  mu <- cc %>% group_by(Year) %>% summarise(m = mean(v), .groups = "drop")
  noise <- sd(mu$m[mu$Year %in% BASE_YEARS])
  d <- inner_join(f, cc, by = c("sim_index", "Year"), suffix = c("_f", "_c")) %>%
    filter(Year == 2010) %>% mutate(s = v_f - v_c)
  median(d$s) / noise
}
snr2010_sl <- function(S) {
  wd <- S %>% tidyr::pivot_wider(names_from = arm, values_from = slope)
  mu <- S %>% filter(arm == "unexploited") %>% group_by(Year) %>%
    summarise(m = mean(slope), .groups = "drop")
  noise <- sd(mu$m[mu$Year %in% BASE_YEARS])
  median((wd$exploited - wd$unexploited)[wd$Year == 2010]) / noise
}
cat("\n=== biomass / slope SNR at 2010, NRMSE_sd cut ===\n")
cat(sprintf("  full range : biomass %.4f | slope %.3f\n",
            snr2010(TABF %>% filter(arm == "exploited"),
                    TABF %>% filter(arm == "unexploited")),
            snr2010_sl(SLPF)))
cat(sprintf("  >= %g g    : biomass %.4f | slope %.3f\n", MIN_W,
            snr2010(TAB1 %>% filter(arm == "exploited"),
                    TAB1 %>% filter(arm == "unexploited")),
            snr2010_sl(SLP1)))
cat("  cut A for comparison: full -0.1465 / -12.1 | >=1g -0.751 / -30.9\n")