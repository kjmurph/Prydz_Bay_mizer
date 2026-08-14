# =============================================================================
# F00g -- Figure 2's four inputs, 1 g cutoff, from the PHASE-61 ensemble
#
# A copy of F00d_build_1g_cutoff_rebuilt167.R re-pointed at the phase-61 member
# states. The slope function, the cutoff, the two arms and the table shapes are
# transcribed unchanged, so F02b reads these by changing one suffix.
#
# FOUR DELIBERATE DEPARTURES FROM F00d, all forced by the new ensemble:
#
# 1. STATE SOURCE. `<stem>_states/state_%05d.rds` from phase 61, carrying
#    `params` + post-spin-up `initial_n`. F00d read `44_states/
#    state_treated_%05d.rds`. Re-projecting is exact either way because the
#    spin-up is unfished, so initial_n does not depend on catchability.
#
# 2. STABLE MEMBERS ONLY. F00d wrote all 167. Here the tables are restricted to
#    the members that PASSED THE PHASE-61 STABILITY SCREEN (122 of 167), because
#    F02b does not filter and a single divergent member is enough to wreck the
#    metric -- one previously owned 83% of the across-member sum and moved the
#    biomass SNR from 2.41 to 0.055. The stability verdict is inherited from
#    phase 61, not recomputed: it belongs to the spin-up, which this does not
#    repeat.
#
# 3. NO VALIDATION AGAINST F00. F00d's central check was that its full-range
#    rebuild reproduced F00's saved tables to 1e-10, which made its 1 g numbers
#    trustworthy. THAT CHECK CANNOT APPLY HERE -- this is a different ensemble on
#    a different base, so there is nothing it should reproduce. The full-range
#    tables are still computed (nearly free next to the projection) and reported
#    beside the 1 g ones, but as a full-vs-cutoff contrast, NOT as a gate.
#    These outputs therefore carry less validation than F00d's. Stated plainly
#    rather than papered over.
#
# 4. FRESH META. F00d amended `meta_rebuilt167.rds`; there is no phase-61
#    equivalent, so meta is constructed here.
#
# Usage:  Rscript "Manuscript scripts/F00g_build_1g_cutoff_p61.R" [run|collect]
# ENV: F0_CORES, F0_CHUNK (25), F0_LIMIT (0 = all), F0_STEM, F0_SUFFIX
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})
source("R/wmin_test/thermizer_shim.R")

OUT_LARGE  <- "Output_large_files/wmin_test"
STEM       <- Sys.getenv("F0_STEM", "61_ramp_n167_nomg_K1_reproduction_level")
STATE_DIR  <- file.path(OUT_LARGE, paste0(STEM, "_states"))
OUT_DATA   <- "Manuscript data"
SUFFIX     <- Sys.getenv("F0_SUFFIX", "1g_p61n122")
WORK_DIR   <- file.path(OUT_LARGE, paste0("F00g_chunks_", SUFFIX))
MIN_W      <- 1                   # the cutoff, grams
FULL_MIN_W <- 3.16227766e-08      # F00's canonical LBNbiom floor
QMAX       <- 1
dir.create(WORK_DIR, recursive = TRUE, showWarnings = FALSE)

CORES <- as.integer(Sys.getenv("F0_CORES", as.character(max(1, detectCores() - 2))))
CHUNK <- as.integer(Sys.getenv("F0_CHUNK", "25"))
LIMIT <- as.integer(Sys.getenv("F0_LIMIT", "0"))
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"

sfx <- function(stem) file.path(OUT_DATA, sprintf("%s_%s.rds", stem, SUFFIX))
guard <- function(f) {
  if (file.exists(f) && !nzchar(Sys.getenv("F0_FORCE")))
    stop("refusing to overwrite: ", f, " (set F0_FORCE=1)", call. = FALSE)
  f
}

# --- membership: phase-61 STABLE members --------------------------------------
SRC <- readRDS(file.path(OUT_LARGE, paste0(STEM, ".rds")))
MEMBERS_ALL <- SRC$members
members <- MEMBERS_ALL$sim_index[MEMBERS_ALL$stable]
n_unstable <- sum(!MEMBERS_ALL$stable)
n_inadm <- sum(MEMBERS_ALL$n_erepro_ge1 > 0)
# Inadmissible members must not slip through either. In the n=167 run all three
# were already unstable, so this is a no-op there -- assert rather than assume.
members <- intersect(members,
                     MEMBERS_ALL$sim_index[MEMBERS_ALL$n_erepro_ge1 == 0])
message("phase-61 ensemble: ", nrow(MEMBERS_ALL), " members | ", n_unstable,
        " unstable | ", n_inadm, " inadmissible | USING ", length(members))
if (!length(members)) stop("no usable members", call. = FALSE)

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
  st <- readRDS(file.path(STATE_DIR, sprintf("state_%05d.rds", si)))
  p <- st$params
  if (all(p@ext_encounter == 0))
    return(list(sim_index = si, ok = FALSE, err = "no_subsidy_in_state"))

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
    return(list(sim_index = si, ok = FALSE, err = "projection_error"))

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
  cat("=== F00g: Figure 2 inputs,", MIN_W, "g cutoff, phase-61 ensemble ===\n")
  cat("stem:", STEM, "\n")
  cat("started", format(Sys.time()), "| members", n_tot, "| cores", CORES, "\n")
  cat("chunks:", length(chunks), "| done:",
      length(list.files(WORK_DIR, pattern = "^f00g_\\d+\\.rds$")), "\n\n")
  t0 <- proc.time()
  for (ci in names(chunks)) {
    rf <- file.path(WORK_DIR, sprintf("f00g_%03d.rds", as.integer(ci)))
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
    flush.console()
  }
  cat("\nrun complete. now: Rscript ... collect\n")
  quit(save = "no")
}

# ------------------------------------------------------------------ collect ---
files <- sort(list.files(WORK_DIR, pattern = "^f00g_\\d+\\.rds$", full.names = TRUE))
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
cat("members written:", length(unique(TAB1$sim_index)), "\n")

saveRDS(TAB1 %>% filter(arm == "exploited"),   guard(sfx("biomass_abund_fish")))
saveRDS(TAB1 %>% filter(arm == "unexploited"), guard(sfx("biomass_abund_clim")))
saveRDS(SLP1, guard(sfx("nbss_slope")))
saveRDS(list(
  n_members = length(unique(TAB1$sim_index)),
  members = sort(unique(TAB1$sim_index)),
  n_top = nrow(MEMBERS_ALL),
  arm = paste("phase-61 ensemble, no gamma draw, pre-cap steady(preserve=erepro),",
              "reproduction level capped at 0.9"),
  cut = paste0("A unweighted pooled log10 yield RMSE, top 10%; ",
               "restricted to phase-61 stable AND admissible members"),
  multipliers = MULT, qmax = QMAX, ref_period = 2001:2010,
  spectrum_min_w = FULL_MIN_W, min_w_cutoff = MIN_W,
  base = SRC$meta$base, source_stem = STEM,
  n_unstable_excluded = n_unstable, n_inadmissible = n_inadm,
  note = paste("Figure 2 inputs with a", MIN_W, "g minimum body-mass cutoff on",
               "both metrics. NOT validated against F00 -- different ensemble."),
  built = Sys.time()), guard(sfx("meta")))
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
cat("\n=== biomass / slope SNR at 2010 (full range vs cutoff) ===\n")
cat(sprintf("  full range : biomass %.4f | slope %.3f\n",
            snr2010(TABF %>% filter(arm == "exploited"),
                    TABF %>% filter(arm == "unexploited")),
            snr2010_sl(SLPF)))
cat(sprintf("  >= %g g    : biomass %.4f | slope %.3f\n", MIN_W,
            snr2010(TAB1 %>% filter(arm == "exploited"),
                    TAB1 %>% filter(arm == "unexploited")),
            snr2010_sl(SLP1)))
cat("\nThese are a full-vs-cutoff contrast, NOT a validation gate.\n")
