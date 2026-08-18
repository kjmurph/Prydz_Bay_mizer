# =============================================================================
# F00 (phase 88) -- build every derived dataset the manuscript figures need,
# from the PHASE-88 ensemble, over ALL 427 USABLE members.
#
# A copy of F00_build_rebuilt167_data.R. The ANALYSIS CODE IS UNCHANGED and must
# stay so -- lbnbiom_slope_series(), tab(), spec() and the ther_diet() krill
# extraction are transcribed byte-for-byte, so figures built from this are
# comparable with the published ones. What differs is the SOURCE and the SCOPE.
#
# ------------------------------------------------------------------ the source
# A. STATES are Output_large_files/wmin_test/88_full_states, built on
#    params_ref_p86_agemat. File naming is `state_%05d.rds`, not phase 44's
#    `state_treated_%05d.rds`.
# B. CATCHABILITY comes from 89_refit_results.rds -- the ISIMIP3a-compliant fit
#    (window ends 2004, missing catch dropped) run on the 427 usable members
#    only. NOT 45_catchability_multipliers.rds, which was fitted on ensemble-44
#    states against a stock that has since moved, and NOT the first phase-89 run,
#    which fitted on all 1,668 and drove krill q to the QMAX ceiling in every
#    member.
# C. MEMBERSHIP comes from 93_rerank_p88.rds.
#
# ------------------------------------------------------------------ the scope
# ALL 427 USABLE MEMBERS ARE BUILT, not a top-N cut. The figures are wanted two
# ways -- the full usable set with IQR and 95% bands, and the top 10% with IQR --
# and the top 43 is a SUBSET of the 427. Building the full set therefore covers
# both, and the selection becomes a plot-time filter on sim_index rather than a
# second six-hour build. The F00 layer stores PER-MEMBER rows and the figure
# scripts take quantiles, so no band choice is baked in here.
#
# Both memberships are carried in meta$cuts so no figure script has to re-derive
# the ranking.
#
# WHY IT STARTS FROM CACHED STATES, unchanged from the original: initial_effort
# is 0 for all 19 gears, so steady() and the spin-up are UNFISHED and fishing
# enters only the 1841-2010 projection. Phase 88 already did the steady() and
# spin-up and cached params + abundances, so this only projects. No steady() is
# re-entered, so the re-run reordering trap does not apply and the projections
# are exact.
#
# THE getDiet t = 0 BUG, still fixed: ther_diet() takes `year` explicitly.
# mizer::getDiet() without `t` defaults to t = 0 and indexes ocean_temp at
# -1841. Do not substitute it back.
#
# NOTHING IS OVERWRITTEN. Every output carries the `_p88full427` suffix and
# collect refuses to write over an existing file.
#
# USAGE
#   Rscript "Manuscript scripts/F00_build_p88_data.R" run
#   Rscript "Manuscript scripts/F00_build_p88_data.R" collect
# ENV: F0_CORES (default cores-2), F0_CHUNK (default 25), F0_LIMIT (0 = all),
#      F0_DIET (1|0), F0_SUFFIX
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})
source("R/wmin_test/thermizer_shim.R")

OUT_LARGE   <- "Output_large_files/wmin_test"
# THE ENSEMBLE IS NOW SELECTABLE. Everything below defaulted to phase 88; a
# phase-104 build needs its own states, ranking, member table and multipliers,
# and its own catchability ceiling. Defaults are unchanged so existing calls
# reproduce exactly.
STATE_DIR   <- Sys.getenv("F0_STATE_DIR", file.path(OUT_LARGE, "88_full_states"))
RANK_F      <- Sys.getenv("F0_RANK", file.path(OUT_LARGE, "93_rerank_p88.rds"))
MEMBERS_F   <- Sys.getenv("F0_MEMBERS", file.path(OUT_LARGE, "88_full.rds"))
REFIT_F     <- Sys.getenv("F0_REFIT", file.path(OUT_LARGE, "89_refit_results.rds"))
OUT_DATA    <- "Manuscript data"
SUFFIX      <- Sys.getenv("F0_SUFFIX", "p88full427")
WORK_DIR    <- file.path(OUT_LARGE, paste0("F00_chunks_", SUFFIX))
REF_PERIOD  <- 2001:2010          # contemporary reference window for the spectra
DIET_YEARS  <- 1900:2010          # Figure 4's x-axis
SPECTRUM_MIN_W <- 3.16227766e-08  # canonical LBNbiom floor
KRILL       <- "antarctic krill"
# catchability ceiling. MUST match the ceiling the refit was run at, or the
# multipliers are applied against a different clamp than they were fitted under.
QMAX        <- as.numeric(Sys.getenv("F0_QMAX", "1"))
dir.create(WORK_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DATA, showWarnings = FALSE)

CORES <- as.integer(Sys.getenv("F0_CORES", as.character(max(1, detectCores() - 2))))
CHUNK <- as.integer(Sys.getenv("F0_CHUNK", "25"))
LIMIT <- as.integer(Sys.getenv("F0_LIMIT", "0"))
DO_DIET <- as.integer(Sys.getenv("F0_DIET", "1")) == 1
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"

# --- membership ---------------------------------------------------------------
RR <- readRDS(RANK_F)
FULL_NAME <- "FULL usable"
TOP_NAME  <- grep("^TOP ", names(RR$cuts), value = TRUE)[1]
members <- as.integer(RR$cuts[[FULL_NAME]])
top_members <- as.integer(RR$cuts[[TOP_NAME]])
stopifnot(length(members) == RR$meta$n_usable,
          all(top_members %in% members))

# Re-derive the usable screen from the phase-88 summary and require a match.
# The failure this guards is real and recent: phase 89's first run fitted every
# state file in the directory, 1,241 of which are rejected members.
P88 <- readRDS(MEMBERS_F)$members
# follow the member table's own definition: phase 104 adds a drift screen, and
# earlier tables carry no drift_ok column so this is a no-op on them.
usable <- sort(as.integer(P88$sim_index[
  P88$stable & P88$n_erepro_ge1 == 0 &
  (if ("drift_ok" %in% names(P88)) P88$drift_ok else TRUE)]))
if (!identical(sort(members), usable))
  stop("the ranking's FULL set is not the usable set of ", basename(MEMBERS_F),
       " -- refusing to proceed. Re-run R/wmin_test/93_rerank_p88.R against ",
       "the matching refit.", call. = FALSE)
message("membership verified: ", length(members), " usable | top cut '",
        TOP_NAME, "' = ", length(top_members))

# --- catchability multipliers -------------------------------------------------
RF <- readRDS(REFIT_F)
MULT <- RF$M
if (!identical(RF$meta$screen, "usable"))
  stop("89_refit_results.rds was fitted with screen='", RF$meta$screen,
       "', not 'usable' -- its multipliers do not belong to this member set",
       call. = FALSE)
message("catchability: ", sum(MULT != 1), " species scaled | held at 1: ",
        paste(names(MULT)[MULT == 1], collapse = ", "))

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
  st <- readRDS(file.path(STATE_DIR, sprintf("state_%05d.rds", si)))
  p <- st$params

  # --- apply the re-fitted catchability, exactly as 46_selection_cuts.R:71-75 -
  # gear_params(p) <- preserves @rates_funcs, @initial_n, @resource_dynamics.
  gp <- gear_params(p)
  m <- MULT[match(gp$species, names(MULT))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
  gear_params(p) <- gp

  sp <- p@species_params$species
  w <- p@w; dw <- p@dw; wdw <- w * dw
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
  cat("=== F00: building", SUFFIX, "figure data ===\n")
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
                        "MULT", "QMAX",
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
  cat('Now: Rscript "Manuscript scripts/F00_build_p88_data.R" collect\n')
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
# Species names come from the model that was actually run, not from a separate
# params file on disk.
sp_names <- readRDS(file.path(STATE_DIR,
  sprintf("state_%05d.rds", Z[[1]]$sim_index)))$params@species_params$species
mk_sp <- function(field) {
  a <- array(NA_real_, dim = c(length(Z), length(sp_names), length(w_grid)),
             dimnames = list(sim = vapply(Z, function(z) as.character(z$sim_index),
                                          character(1)),
                             species = sp_names, w = NULL))
  for (k in seq_along(Z)) a[k, , ] <- Z[[k]][[field]]$by_species
  a
}
# w_mat / w_max are needed by Figure 1's silhouette placement; carry them with
# the spectra so the figure script never has to open a 350 MB state directory.
trait <- readRDS(file.path(STATE_DIR,
  sprintf("state_%05d.rds", Z[[1]]$sim_index)))$params@species_params[
    , c("species", "w_min", "w_mat", "w_max")]

saveRDS(list(w = w_grid,
             community_fished = mk_spec("spec_f"),
             community_clim   = mk_spec("spec_c"),
             species_fished   = mk_sp("spec_f"),
             species_clim     = mk_sp("spec_c"),
             ref_period = REF_PERIOD, sp_names = sp_names, traits = trait),
        guard(sfx("spectra_ref_period")))

if (DO_DIET && !is.null(Z[[1]]$krill)) {
  KR <- bind_rows(lapply(Z, `[[`, "krill"))
  cat("krill consumption rows:", nrow(KR), "\n")
  saveRDS(KR, guard(sfx("krill_consumption")))
}

built <- vapply(Z, function(z) z$sim_index, numeric(1))
meta <- list(n_members = length(Z), members = built,
             # BOTH memberships travel with the data, so a figure script filters
             # rather than re-deriving the ranking from the refit.
             cuts = list(full = as.integer(built),
                         top  = as.integer(intersect(top_members, built))),
             n_top = length(intersect(top_members, built)),
             arm = "phase-88 ensemble on params_ref_p86_agemat, catchability re-fitted (phase 89, ISIMIP3a window, usable screen)",
             cut = paste0("FULL usable (", length(built), ") carried in full; ",
                          TOP_NAME, " (", length(intersect(top_members, built)),
                          ") flagged in meta$cuts$top"),
             multipliers = MULT, qmax = QMAX,
             ref_period = REF_PERIOD, diet_years = DIET_YEARS,
             spectrum_min_w = SPECTRUM_MIN_W, built = Sys.time(),
             ranking_rule = RR$rule,
             provenance = paste("1668 built -> 427 usable (stable AND no",
                                "erepro>=1) -> catchability re-fitted on those",
                                "427 -> ranked; top 10% = 43"))
saveRDS(meta, guard(sfx("meta")))

cat("\nWrote to", OUT_DATA, "with suffix _", SUFFIX, ":\n", sep = "")
for (f in list.files(OUT_DATA, pattern = SUFFIX, full.names = FALSE))
  cat("  ", f, "\n")
cat("\ncollect complete.\n")
