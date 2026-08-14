# =============================================================================
# F00d -- rebuild Figure 2's four inputs with a 1 g MINIMUM BODY-MASS CUTOFF.
# Rebuilt ensemble 44, cut A, 167 members, both arms, 1841-2010.
#
# WHAT FIGURE 2 CURRENTLY USES: the WHOLE spectrum, in both metrics.
#   panels a/b  community biomass = rowSums(sweep(n, 2, w*dw, "*")) over all 100
#               bins (F00:181) -- no cutoff of any kind.
#   panels c/d  LBNbiom slope with min_w = 3.16227766e-08 (F00:73), which is
#               exactly the model's smallest w_min (mesozooplankton), so no bin
#               is excluded there either.
# Neither can be re-cut after the fact: the saved tables are already summed over
# w, and the saved slope series is already fitted. Hence this re-extraction.
#
# WHAT THE 1 g CUTOFF DOES. 52 of 100 bins survive, spanning 27 octaves -- ample
# for the octave regression. Two groups leave the calculation entirely
# (mesozooplankton, w_max 3.2e-3 g; other krill, w_max 0.34 g). Eight are
# partially trimmed, most consequentially antarctic krill (w_min 1.1e-6,
# w_max 4.2), which keeps only its largest bins. Nine groups are untouched --
# every bird, seal and whale, and flying birds from 40 g up.
#
# THE CUTOFF IS APPLIED IDENTICALLY TO BOTH METRICS: `w >= MIN_W` on the same
# bin grid, which is the convention lbnbiom_slope_series() already uses
# internally (w_filt <- w[w >= min_w]). So biomass and slope see the same bins.
#
# HOW THIS IS VALIDATED. Each worker computes BOTH the full-range and the 1 g
# version from the same projected sim. The full-range output must reproduce
# F00_build_rebuilt167_data.R's saved tables, since it is the same protocol on
# the same stored states; `collect` checks biomass, abundance and slope against
# them and STOPS if the max relative difference exceeds 1e-10. That is what makes
# the 1 g numbers trustworthy -- they are a different reduction of a projection
# proven to reproduce F00 exactly. The extra full-range computation is nearly
# free next to the projection itself.
#
# NOTHING EXISTING IS OVERWRITTEN. Outputs carry the suffix `1g_rebuilt167`, so
# F02's variant reads them by changing one constant.
#
# Usage:  Rscript "Manuscript scripts/F00d_build_1g_cutoff_rebuilt167.R"
#         Rscript "Manuscript scripts/F00d_build_1g_cutoff_rebuilt167.R" collect
# ENV: F0_CORES (default cores-2), F0_CHUNK (default 25), F0_LIMIT (0 = all)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})
source("R/wmin_test/thermizer_shim.R")

N_TOP      <- 167
OUT_LARGE  <- "Output_large_files/wmin_test"
STATE_DIR  <- file.path(OUT_LARGE, "44_states")
OUT_DATA   <- "Manuscript data"
WORK_DIR   <- file.path(OUT_LARGE, "F00d_chunks_rebuilt167")
SRC_SUF    <- "rebuilt167"        # what we validate against
SUFFIX     <- "1g_rebuilt167"     # what we write
MIN_W      <- 1                   # the cutoff, grams
FULL_MIN_W <- 3.16227766e-08      # F00's canonical LBNbiom floor
QMAX       <- 1
TOL        <- 1e-10
dir.create(WORK_DIR, recursive = TRUE, showWarnings = FALSE)

CORES <- as.integer(Sys.getenv("F0_CORES", as.character(max(1, detectCores() - 2))))
CHUNK <- as.integer(Sys.getenv("F0_CHUNK", "25"))
LIMIT <- as.integer(Sys.getenv("F0_LIMIT", "0"))
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"

sfx <- function(stem) file.path(OUT_DATA, sprintf("%s_%s.rds", stem, SUFFIX))
guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE); f
}

# --- membership, verified exactly as F00 does ---------------------------------
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
message("cut A membership verified against the post-refit ranking (", N_TOP,
        " members)")

MULT <- readRDS(file.path(OUT_LARGE, "45_catchability_multipliers.rds"))$M
if (LIMIT > 0) members <- head(members, LIMIT)
n_tot <- length(members)
chunks <- split(seq_len(n_tot), ceiling(seq_len(n_tot) / CHUNK))
effort_arr <- readRDS("effort_array_1841_2010.rds")

# --- canonical LBNbiom slope, transcribed from F00:115-137 unchanged -----------
# Already parameterised on min_w: bin_breaks start at 2^floor(log2(min_w)), so
# passing min_w = 1 gives octaves from 1 g up with no other change.
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

  # cols = NULL means every bin, i.e. F00's behaviour
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

  list(sim_index = si, ok = TRUE,
       tab_1g   = rbind(tab(cf, "exploited", keep_w),
                        tab(cc, "unexploited", keep_w)),
       slope_1g = rbind(slp(cf, "exploited", MIN_W),
                        slp(cc, "unexploited", MIN_W)),
       tab_full   = rbind(tab(cf, "exploited", NULL),
                          tab(cc, "unexploited", NULL)),
       slope_full = rbind(slp(cf, "exploited", FULL_MIN_W),
                          slp(cc, "unexploited", FULL_MIN_W)))
}

# ---------------------------------------------------------------------- run ---
if (mode == "run") {
  cat("=== F00d: Figure 2 inputs with a", MIN_W, "g cutoff ===\n")
  cat("started", format(Sys.time()), "| members", n_tot, "| cores", CORES, "\n")
  cat("chunks:", length(chunks), "| done:",
      length(list.files(WORK_DIR, pattern = "^f00d_\\d+\\.rds$")), "\n\n")
  t0 <- proc.time()
  for (ci in names(chunks)) {
    rf <- file.path(WORK_DIR, sprintf("f00d_%03d.rds", as.integer(ci)))
    if (file.exists(rf)) { cat("chunk", ci, "done, skipping\n"); next }
    MEM <- members[chunks[[ci]]]
    cl <- makeCluster(CORES)
    clusterExport(cl, c("MEM", "STATE_DIR", "effort_arr", "MULT", "QMAX",
                        "MIN_W", "FULL_MIN_W", "lbnbiom_slope_series", "worker"),
                  envir = environment())
    r <- parLapplyLB(cl, seq_along(MEM), function(j)
      tryCatch(worker(j), error = function(e)
        list(sim_index = MEM[j], ok = FALSE, err = conditionMessage(e))))
    stopCluster(cl)
    saveRDS(r, rf)
    cat(sprintf("chunk %s: %d/%d ok | %.1f min elapsed\n", ci,
                sum(vapply(r, `[[`, logical(1), "ok")), length(r),
                (proc.time() - t0)[["elapsed"]] / 60))
  }
  cat("\nrun complete. now: Rscript ... collect\n")
  quit(save = "no")
}

# ------------------------------------------------------------------ collect ---
files <- sort(list.files(WORK_DIR, pattern = "^f00d_\\d+\\.rds$", full.names = TRUE))
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

TAB1 <- bind_rows(lapply(Z, `[[`, "tab_1g"))
SLP1 <- bind_rows(lapply(Z, `[[`, "slope_1g"))
TABF <- bind_rows(lapply(Z, `[[`, "tab_full"))
SLPF <- bind_rows(lapply(Z, `[[`, "slope_full"))
cat("rows: tab", nrow(TAB1), "| slope", nrow(SLP1),
    "| years", min(SLP1$Year), "-", max(SLP1$Year), "\n")

# --- THE CHECK: the full-range rebuild must reproduce F00 ----------------------
relmax <- function(new, old) {
  den <- pmax(abs(old), .Machine$double.eps)
  max(abs(new - old) / den, na.rm = TRUE)
}
# Comparison is restricted to the members actually rebuilt, so the check is just
# as strict under F0_LIMIT as it is on the full run.
MEMS <- sort(unique(TABF$sim_index))
chk_tab <- function(a, path) {
  o <- readRDS(file.path(OUT_DATA, sprintf("%s_%s.rds", path, SRC_SUF))) %>%
    filter(sim_index %in% MEMS)
  j <- TABF %>% filter(arm == a) %>%
    select(sim_index, Year, Species, Bn = Biomass, An = Abundance) %>%
    inner_join(o %>% select(sim_index, Year, Species,
                            Bo = Biomass, Ao = Abundance),
               by = c("sim_index", "Year", "Species"))
  if (nrow(j) != nrow(o))
    stop(path, ": join gave ", nrow(j), " rows, expected ", nrow(o))
  c(biomass = relmax(j$Bn, j$Bo), abundance = relmax(j$An, j$Ao))
}
d1 <- chk_tab("exploited",   "biomass_abund_fish")
d2 <- chk_tab("unexploited", "biomass_abund_clim")
o_sl <- readRDS(file.path(OUT_DATA, sprintf("nbss_slope_%s.rds", SRC_SUF))) %>%
  filter(sim_index %in% MEMS)
js <- SLPF %>% rename(sn = slope) %>%
  inner_join(o_sl %>% rename(so = slope), by = c("sim_index", "arm", "Year"))
if (nrow(js) != nrow(o_sl))
  stop("slope join gave ", nrow(js), " rows, expected ", nrow(o_sl))
d3 <- relmax(js$sn, js$so)

cat("\n=== full-range rebuild vs F00 (max relative difference) ===\n")
cat(sprintf("  exploited   biomass %.3g | abundance %.3g\n", d1[1], d1[2]))
cat(sprintf("  unexploited biomass %.3g | abundance %.3g\n", d2[1], d2[2]))
cat(sprintf("  slope       %.3g  (%d values)\n", d3, nrow(js)))
worst <- max(d1, d2, d3)
if (worst > TOL)
  stop("full-range rebuild does not reproduce F00 (max rel diff ",
       signif(worst, 3), " > ", TOL, ") -- refusing to write the 1 g outputs")
cat("reproduces F00 exactly. The 1 g outputs come from the same projections.\n")

saveRDS(TAB1 %>% filter(arm == "exploited"),   guard(sfx("biomass_abund_fish")))
saveRDS(TAB1 %>% filter(arm == "unexploited"), guard(sfx("biomass_abund_clim")))
saveRDS(SLP1, guard(sfx("nbss_slope")))
src_meta <- readRDS(file.path(OUT_DATA, sprintf("meta_%s.rds", SRC_SUF)))
src_meta$min_w_cutoff <- MIN_W
src_meta$note <- paste("Figure 2 inputs recomputed with a", MIN_W,
                       "g minimum body-mass cutoff on both metrics")
saveRDS(src_meta, guard(sfx("meta")))
cat("\nWrote 4 files with suffix", SUFFIX, "\n")

# --- what the cutoff does to the headline numbers ------------------------------
BASE <- 1841:2010
snr2010 <- function(tf, tc) {
  f <- tf %>% group_by(sim_index, Year) %>% summarise(v = sum(Biomass), .groups = "drop")
  cc <- tc %>% group_by(sim_index, Year) %>% summarise(v = sum(Biomass), .groups = "drop")
  mu <- cc %>% group_by(Year) %>% summarise(m = mean(v), .groups = "drop")
  noise <- sd(mu$m[mu$Year %in% BASE])
  d <- inner_join(f, cc, by = c("sim_index", "Year"), suffix = c("_f", "_c")) %>%
    filter(Year == 2010) %>% mutate(s = v_f - v_c)
  median(d$s) / noise
}
snr2010_sl <- function(S) {
  wd <- S %>% tidyr::pivot_wider(names_from = arm, values_from = slope)
  mu <- S %>% filter(arm == "unexploited") %>% group_by(Year) %>%
    summarise(m = mean(slope), .groups = "drop")
  noise <- sd(mu$m[mu$Year %in% BASE])
  median((wd$exploited - wd$unexploited)[wd$Year == 2010]) / noise
}
cat("\n=== biomass / slope SNR at 2010 ===\n")
cat(sprintf("  full range : biomass %.4f | slope %.3f\n",
            snr2010(TABF %>% filter(arm == "exploited"),
                    TABF %>% filter(arm == "unexploited")),
            snr2010_sl(SLPF)))
cat(sprintf("  >= %g g    : biomass %.4f | slope %.3f\n", MIN_W,
            snr2010(TAB1 %>% filter(arm == "exploited"),
                    TAB1 %>% filter(arm == "unexploited")),
            snr2010_sl(SLP1)))