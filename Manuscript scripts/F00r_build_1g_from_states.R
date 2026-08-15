# =============================================================================
# F00r -- Figure 2's four inputs with a 1 g MINIMUM BODY-MASS CUTOFF, computed
# from an ARBITRARY state directory (e.g. 53_kernel_rebuild_cutA.R's states).
#
# WHY THIS EXISTS RATHER THAN A CALL TO F00d. F00d does exactly this job but is
# hard-wired to `44_states/`, the `state_treated_%05d.rds` naming, cut-A
# membership of exactly 167, and a validation against the `rebuilt167` products.
# None of those hold for a variant build. The cutoff logic below is transcribed
# from F00d:95-182 verbatim so the two produce identical numbers on identical
# input; F00d itself is left untouched so the published pipeline stays exactly
# reproducible.
#
# WHY THE CUTOFF CANNOT BE APPLIED POST HOC. `biomass_abund_*` carries biomass
# already summed over every size bin, and the slope is fitted from
# min_w = 3.16227766e-08. Neither is re-cuttable after the fact -- the cutoff has
# to be applied to the size-resolved state, which means re-projecting. That is
# what this script does.
#
# THE MANUSCRIPT'S FIGURE 2 IS THE 1 g VARIANT. `fig2_snr_1g_rebuilt167` ends at
# a biomass SNR of -0.7506 and a slope SNR of -30.92 at 2010; the full-range
# `fig2_snr_rebuilt167` ends at -0.1465 and -12.08. The published figure shows
# the former.
#
# STABILITY FILTERING IS ON BY DEFAULT, AND THAT IS DELIBERATE. Figure 2's SNR
# denominator is the temporal SD of the ACROSS-MEMBER MEAN unexploited
# trajectory. A mean is not robust: on the revised-kernel build one unstable
# member (2055) carries an unexploited biomass 800x the ensemble norm and inflates
# that denominator ~40x, driving the community-biomass SNR from 2.41 to 0.055.
# Building this product on unfiltered members would silently reproduce that.
# Members are therefore restricted to those passing the 44/53 stability screen
# unless F0R_STABLE_ONLY=0 is set explicitly.
#
# USAGE  Rscript "Manuscript scripts/F00r_build_1g_from_states.R"
# ENV: F0R_STATE_DIR, F0R_MANIFEST, F0R_SUFFIX, F0R_ARM (state file arm token),
#      F0R_CORES (default 12), F0R_MIN_W (default 1),
#      F0R_SUMMARY (run summary carrying `stable`), F0R_STABLE_ONLY (default 1)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})
source("R/wmin_test/thermizer_shim.R")

OUT_LARGE <- "Output_large_files/wmin_test"
OUT_DATA  <- "Manuscript data"
STATE_DIR <- Sys.getenv("F0R_STATE_DIR",
               file.path(OUT_LARGE, "53_bk05_mk05_n167_states"))
MANIFEST  <- Sys.getenv("F0R_MANIFEST",
               file.path(OUT_LARGE, "53_bk05_mk05_n167_rebuild.rds"))
ARM       <- Sys.getenv("F0R_ARM", "kernel")
CORES     <- as.integer(Sys.getenv("F0R_CORES", "12"))
MIN_W     <- as.numeric(Sys.getenv("F0R_MIN_W", "1"))
SUMMARY   <- Sys.getenv("F0R_SUMMARY",
               sub("_rebuild\\.rds$", "_summary.csv", MANIFEST))
STABLE_ONLY <- Sys.getenv("F0R_STABLE_ONLY", "1") == "1"
QMAX <- 1

sfx <- function(stem) file.path(OUT_DATA, sprintf("%s_%s.rds", stem, SUFFIX))
guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE); f
}

# State file naming. Phase 44/53 states carry an arm token
# (state_<arm>_00148.rds); phase 88's do not (state_00148.rds). F0R_ARM="" picks
# the second form. The default is unchanged, so kernel158 still reproduces.
state_file <- function(si) file.path(STATE_DIR,
  if (nzchar(ARM)) sprintf("state_%s_%05d.rds", ARM, si)
  else sprintf("state_%05d.rds", si))

cat("=== F00r: 1 g-cutoff Figure 2 inputs ===\n")
# Membership: either a manifest carrying $members, or a phase-93-style cuts
# object named by F0R_CUTS_RDS / F0R_CUT.
CUTS_RDS <- Sys.getenv("F0R_CUTS_RDS", "")
MAN <- NULL
if (nzchar(CUTS_RDS)) {
  CR <- readRDS(CUTS_RDS)
  cut_nm <- Sys.getenv("F0R_CUT", "FULL usable")
  if (is.null(CR$cuts[[cut_nm]]))
    stop("no cut '", cut_nm, "' in ", CUTS_RDS, " -- have: ",
         paste(names(CR$cuts), collapse = " | "), call. = FALSE)
  members <- as.integer(CR$cuts[[cut_nm]])
  cat("membership:", basename(CUTS_RDS), "| cut '", cut_nm, "' |",
      length(members), "members\n")
} else {
  MAN <- readRDS(MANIFEST)
  members <- MAN$members
}
if (STABLE_ONLY) {
  S <- read.csv(SUMMARY)
  drop <- setdiff(members, S$sim_index[S$stable])
  members <- intersect(members, S$sim_index[S$stable])
  cat("stability filter ON:", length(drop), "dropped (",
      paste(drop, collapse = ", "), ")\n")
} else {
  # For a phase-93 cut this is CORRECT, not a risk: the usable screen is
  # `stable AND no erepro >= 1`, so the members are already stability-filtered
  # upstream and re-filtering here would need a summary this build has no
  # reason to carry. For a raw manifest it IS the outlier risk in the header.
  cat("stability filter OFF -- correct for a phase-93 cut (already screened);",
      "for a raw manifest, SNR denominators may be dominated by outliers\n")
}
SUFFIX <- Sys.getenv("F0R_SUFFIX", sprintf("1gkernel%d", length(members)))
MULT <- readRDS(Sys.getenv("F0R_MULT",
  file.path(OUT_LARGE, "45_catchability_multipliers.rds")))$M
effort_arr <- readRDS("effort_array_1841_2010.rds")
cat("states:", STATE_DIR, "| members", length(members), "| cutoff", MIN_W,
    "g | suffix", SUFFIX, "\n")
ok <- file.exists(vapply(members, state_file, character(1)))
if (!all(ok)) stop("missing states for: ", paste(members[!ok], collapse = ", "))

# --- LBNbiom slope, transcribed from F00d:93-115 -----------------------------
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

worker <- function(k) {
  suppressPackageStartupMessages({ library(therMizer); library(mizer) })
  source("R/wmin_test/thermizer_shim.R")
  si <- MEM[k]
  st <- readRDS(state_file(si))
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
      project(p, initial_n = st$initial_n, t_start = 1841, t_max = 169, effort = 0,
              progress_bar = FALSE)
    else
      project(p, initial_n = st$initial_n, t_start = 1841, effort = eff,
              progress_bar = FALSE)
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
      n <- s@n[ti, , ][, cols, drop = FALSE]
      bm <- rowSums(sweep(n, 2, wdw[cols], "*"))
      ab <- rowSums(sweep(n, 2, dw[cols], "*"))
      data.frame(sim_index = si, arm = arm,
                 Year = as.numeric(dimnames(s@n)$time)[ti],
                 Species = sp, Biomass = bm, Abundance = ab,
                 MeanMass = ifelse(ab > 0, bm / ab, NA_real_),
                 stringsAsFactors = FALSE)
    }))
  }
  slp <- function(cl, arm) data.frame(
    sim_index = si, arm = arm, Year = cl$yr,
    slope = lbnbiom_slope_series(cl$sim, MIN_W)[cl$keep], stringsAsFactors = FALSE)

  list(sim_index = si, ok = TRUE,
       tab = rbind(tab(cf, "exploited", keep_w), tab(cc, "unexploited", keep_w)),
       slope = rbind(slp(cf, "exploited"), slp(cc, "unexploited")))
}

t0 <- proc.time()
MEM <- members
cl <- makeCluster(min(CORES, length(members)))
clusterExport(cl, c("MEM", "STATE_DIR", "ARM", "state_file", "effort_arr",
                    "MULT", "QMAX",
                    "MIN_W", "lbnbiom_slope_series", "worker"), envir = environment())
Z <- parLapplyLB(cl, seq_along(MEM), function(j)
  tryCatch(worker(j), error = function(e)
    list(sim_index = MEM[j], ok = FALSE, err = conditionMessage(e))))
stopCluster(cl)
okz <- vapply(Z, function(x) isTRUE(x$ok), logical(1))
cat("completed", sum(okz), "of", length(Z), "in",
    round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
if (any(!okz)) stop("failed members: ",
                    paste(vapply(Z[!okz], function(z) z$sim_index, numeric(1)),
                          collapse = ", "))

TAB <- bind_rows(lapply(Z, `[[`, "tab")); SLP <- bind_rows(lapply(Z, `[[`, "slope"))
saveRDS(TAB %>% filter(arm == "exploited"),   guard(sfx("biomass_abund_fish")))
saveRDS(TAB %>% filter(arm == "unexploited"), guard(sfx("biomass_abund_clim")))
saveRDS(SLP, guard(sfx("nbss_slope")))
# base_path comes from the manifest, which a cuts-driven build does not have.
base_p <- if (!is.null(MAN)) unname(MAN$base_path[[ARM]]) else NA_character_
meta <- list(n_members = length(members), members = members,
             min_w_cutoff = MIN_W, state_dir = STATE_DIR,
             base_params = base_p,
             # Carried so the figure layer can filter to the selection without
             # re-deriving the ranking, exactly as F00_build_p88_data.R does.
             cuts = if (nzchar(CUTS_RDS)) {
               CR <- readRDS(CUTS_RDS)
               tn <- grep("^TOP ", names(CR$cuts), value = TRUE)[1]
               list(full = as.integer(members),
                    top = as.integer(intersect(CR$cuts[[tn]], members)))
             } else NULL,
             cut = if (!is.null(MAN))
               sprintf("cut A re-run under %s, %d g cutoff",
                       basename(base_p), MIN_W)
             else sprintf("%s from %s, %d g cutoff",
                          Sys.getenv("F0R_CUT", "FULL usable"),
                          basename(CUTS_RDS), MIN_W),
             built = Sys.time())
saveRDS(meta, guard(sfx("meta")))
cat("wrote 4 files with suffix", SUFFIX, "| biomass rows", nrow(TAB),
    "| slope rows", nrow(SLP), "\n")