###############################################################################
# FM00_common.R -- shared constants and helpers for the FishMIP ISIMIP3a
#                  outputs built on the PHASE-104 ensemble.
#
# Protocol: https://github.com/Fish-MIP/FishMIP2.0_ISIMIP3a
#
# WHAT THIS PIPELINE REPLACES
# ---------------------------
# The outputs in `FishMIP_ISIMIP3a_submission/` and the root-level
# `fishmip_outputs*/` were built from the 2,111-member Monte Carlo ensemble
# (`mc_ensemble_2111_cleaned.rds`) whose catchability fit used a window the
# project has since declared protocol-non-compliant (docs/VM_RERUN_PLAN.md).
# Those trees are left untouched; this pipeline writes a parallel bundle.
#
# THE ENSEMBLE
# ------------
# Phase 104, arm q10: 203 usable members (stable AND drift_ok AND erepro < 1),
# catchability re-fitted on the ISIMIP3a window ending 2004 with a q <= 10
# ceiling. Members come from `105_export_member_sims.R`'s exported simulations,
# already projected 1841-2010 with catchability applied. Nothing here
# re-projects, re-steadies or re-applies a multiplier -- see AGENTS.md on why
# re-running the protocol does not reproduce the ensemble.
#
# THE DOMAIN AREA
# ---------------
#   MODEL_DOMAIN_AREA   1.474341e+12 m^2   (1,474,341 km^2)
#
# The Prydz Bay model domain: the study box 60-90 E, 80-60 S unioned with
# Atlantis Box21 and differenced against land and ice shelf, measured in a
# Lambert azimuthal equal-area projection centred on the region
# (model_domains/Stacey/BanzareBank.R). FM06_verify.R recomputes it from the
# stored polygon `BanzareBank.rds` and asserts the match, so this constant is
# verified rather than quoted.
#
# This is the area the model IS: observed densities in g m^-2 were multiplied
# by it at model construction, so the state variable is grams of wet mass over
# this polygon (05_therMizer_calibration_scale_model_domain.Rmd:45-58). Every
# biomass, catch and yield quantity in the project -- the manuscript, the
# consumption analyses, the calibration targets -- is on this basis.
#
# Dividing by it is therefore exactly invertible: it returns the density the
# model was fitted to, over the region the model represents. Any other divisor
# would report grams-over-this-polygon per square metre of somewhere else.
#
# Author: Prydz Bay mizer project
###############################################################################

suppressPackageStartupMessages({
  library(mizer)
  library(therMizer)
})

## --------------------------------------------------------------------------
## Domain area (see the header)
## --------------------------------------------------------------------------

MODEL_DOMAIN_AREA <- 1.474341e+12  # m^2, Prydz Bay model domain polygon

AREA_NOTE <- paste0(
  "Densities are model domain totals divided by MODEL_DOMAIN_AREA ",
  "(1.474341e+12 m2 = 1,474,341 km2), the Prydz Bay model domain polygon. ",
  "This is the area the model was built with: observed densities in g m-2 were ",
  "multiplied by it at model construction, so the model state variable is grams ",
  "of wet mass over this domain. Dividing by it returns the density basis the ",
  "model was calibrated to. Multiply by 1.474341e+12 to recover domain totals ",
  "in grams."
)

## --------------------------------------------------------------------------
## FishMIP naming and size classes
##   verbatim from extract_fishmip_outputs.R:43-50
## --------------------------------------------------------------------------

FISHMIP_MODEL    <- "mizer"
FISHMIP_FORCING  <- "gfdl-mom6-cobalt2"
FISHMIP_CLIMATE  <- "obsclim"
FISHMIP_SENS     <- "default"
FISHMIP_REGION   <- "prydz-bay"
FISHMIP_TIMESTEP <- "annual"
FISHMIP_START    <- "1841"
FISHMIP_END      <- "2010"

# FishMIP log10 weight class boundaries (in grams)
# Classes: 1-10g, 10-100g, 100g-1kg, 1-10kg, 10-100kg, >100kg
FISHMIP_BINS <- c(1, 10, 100, 1000, 10000, 100000, Inf)  # grams
FISHMIP_BIN_LOG10 <- log10(FISHMIP_BINS[-length(FISHMIP_BINS)])  # 0, 1, 2, 3, 4, 5
FISHMIP_BIN_NAMES <- c("1g-10g", "10g-100g", "100g-1kg", "1kg-10kg", "10kg-100kg", ">100kg")

# FishMIP missing value
FISHMIP_MISSING <- 1.0e+20

FISHMIP_STATS <- c("median", "q05", "q25", "q75", "q95")

## The four groups excluded from the fisheries-only catch variant. FishMIP's
## catch reconstruction covers fisheries, not whaling, so `tc` including these
## is not comparable with it. Excluded BY NAME, not by "is fished": the other
## ten groups carry catchability 0 and so contribute nothing either way.
WHALE_GROUPS <- c("minke whales", "orca", "sperm whales", "baleen whales")

## The `sens` slot token for the fisheries-only variant. The protocol slot order
## is <model>_<forcing>_<climate>_<soc>_<sens>_<var>_<region>_<timestep>_<start>_<end>;
## putting the variant in `sens` keeps `var` equal to the protocol variable name
## so an automated checker still matches it. Confirm the token with the FishMIP
## regional coordinators before upload -- a rename is this one line.
FISHMIP_SENS_NOWHALES <- "nowhales"

## --------------------------------------------------------------------------
## Paths
## --------------------------------------------------------------------------

OUT_LARGE <- "Output_large_files/wmin_test"
SIM_DIR   <- file.path(OUT_LARGE, "p104q10_sims")

FM_OUT_DIR <- Sys.getenv("FM_OUT_DIR", "FishMIP_ISIMIP3a_submission_p104")
FM_WORK    <- file.path(FM_OUT_DIR, "_work")

FM_RANK <- Sys.getenv("F0_RANK", file.path(OUT_LARGE, "104_q10_rerank.rds"))

FM_DIR_TOTALS    <- file.path(FM_OUT_DIR, "fishmip_outputs")
FM_DIR_SPECIES   <- file.path(FM_OUT_DIR, "fishmip_outputs_species")
FM_DIR_ANCILLARY <- file.path(FM_OUT_DIR, "fishmip_outputs_ancillary")

FM_N_MEMBERS_EXPECTED <- 203L

fm_dir <- function(...) {
  d <- file.path(...)
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(d))
      stop("could not create directory: ", d,
           "\n(", nchar(normalizePath(d, mustWork = FALSE)), " characters; ",
           "see fm_check_path() on the Windows path limit)", call. = FALSE)
  }
  d
}

## Windows MAX_PATH. The repository sits under a OneDrive path 90 characters
## long and the protocol filenames run to 88, which leaves very little room:
## staging once produced a 261-character path and failed mid-write. The
## protocol filenames cannot be shortened, so the directories must stay short,
## and every intended output path is checked BEFORE anything is written.
##
## `LongPathsEnabled` is 0 on this machine. Turning it on is a machine-wide
## registry change and is deliberately not done here.
FM_MAX_PATH <- 259L

#' Stop before writing if any intended path would exceed the Windows limit.
#'
#' @param paths character vector of intended output paths
#' @param what  label for the error message
fm_check_paths <- function(paths, what = "output") {
  full <- normalizePath(paths, winslash = "\\", mustWork = FALSE)
  n <- nchar(full)
  bad <- which(n > FM_MAX_PATH)
  if (length(bad))
    stop(length(bad), " ", what, " path(s) exceed the ", FM_MAX_PATH,
         "-character Windows limit; longest is ", max(n), ":\n  ",
         full[bad[which.max(n[bad])]],
         "\nShorten the directory names -- the protocol filenames must not change.",
         call. = FALSE)
  invisible(max(n))
}

## --------------------------------------------------------------------------
## Size-class and biomass helpers
##   moved verbatim from extract_fishmip_outputs.R:99-148. These were reviewed
##   and are correct; they are not rewritten.
## --------------------------------------------------------------------------

#' Assign mizer weight bins to FishMIP log10 size classes
#'
#' @param w_vec Vector of mizer weight bin centers (in grams)
#' @param fishmip_bins Vector of FishMIP bin boundaries (in grams)
#' @return Integer vector assigning each mizer bin to a FishMIP class (1-6, or NA if < 1g)
assign_fishmip_bins <- function(w_vec, fishmip_bins = FISHMIP_BINS) {
  bin_assignments <- cut(w_vec, breaks = fishmip_bins, labels = FALSE, include.lowest = TRUE, right = FALSE)
  return(bin_assignments)
}

#' Calculate biomass from abundance (N) array
#'
#' @param n_array Array of abundances (time x species x weight bins)
#' @param w_vec Vector of weight bin centers
#' @param dw_vec Vector of weight bin widths
#' @return Array of biomass (same dimensions as n_array)
calc_biomass_from_n <- function(n_array, w_vec, dw_vec) {
  # Biomass = N * w * dw (integrated over the width of each bin)
  # In mizer, N is typically in numbers per unit biomass per weight bin width
  # The formula is: Biomass_in_bin = N * w * dw
  n_dims <- dim(n_array)
  biomass_array <- array(0, dim = n_dims)

  for (i in seq_along(w_vec)) {
    biomass_array[, , i] <- n_array[, , i] * w_vec[i] * dw_vec[i]
  }

  return(biomass_array)
}

#' Aggregate biomass into FishMIP log10 size classes
#'
#' @param biomass_array Array of biomass (time x species x weight bins)
#' @param bin_assignments Vector assigning each weight bin to a FishMIP class
#' @param n_bins Number of FishMIP bins (6)
#' @return Array of biomass aggregated by size class (time x species x 6 size classes)
aggregate_to_fishmip_bins <- function(biomass_array, bin_assignments, n_bins = 6) {
  n_time <- dim(biomass_array)[1]
  n_species <- dim(biomass_array)[2]

  aggregated <- array(0, dim = c(n_time, n_species, n_bins))

  for (b in 1:n_bins) {
    bin_idx <- which(bin_assignments == b)
    if (length(bin_idx) > 0) {
      if (length(bin_idx) == 1) {
        aggregated[, , b] <- biomass_array[, , bin_idx]
      } else {
        aggregated[, , b] <- apply(biomass_array[, , bin_idx, drop = FALSE], c(1, 2), sum)
      }
    }
  }

  return(aggregated)
}

## --------------------------------------------------------------------------
## Time, naming and output helpers
##   year_to_days and the filename builder come from
##   FishMIP_ISIMIP3a_submission/extract_fishmip_outputs_species.R:58, 76-83,
##   generalised so `sens` is an argument.
## --------------------------------------------------------------------------

## `ref` is the reference year of the time axis. The protocol counts days from
## 1841-1-1 in files covering 1841-1960 and from 1901-1-1 in files covering
## 1961-2010; the 365-day calendar makes either an exact multiple of 365.
year_to_days <- function(year, ref = 1841L) (as.integer(year) - as.integer(ref)) * 365L

fishmip_filename <- function(soc, var,
                             sens  = FISHMIP_SENS,
                             start = FISHMIP_START,
                             end   = FISHMIP_END,
                             ext   = "csv") {
  sprintf(
    "%s_%s_%s_%s_%s_%s_%s_%s_%s_%s.%s",
    FISHMIP_MODEL, FISHMIP_FORCING, FISHMIP_CLIMATE,
    soc, sens, var,
    FISHMIP_REGION, FISHMIP_TIMESTEP, start, end, ext
  )
}

#' Write one protocol CSV and report it.
#'
#' Generated outputs are deterministic given the ensemble and live in a fresh
#' tree, so re-running overwrites rather than refusing -- unlike the
#' `Manuscript data/` builders, where a stale overwrite would be silent. The
#' overwrite that actually matters (clobbering the OLD submission bundle) is
#' blocked in FM05_stage_bundle.R.
fm_write_csv <- function(df, dir, soc, var, sens = FISHMIP_SENS,
                         start = FISHMIP_START, end = FISHMIP_END) {
  d <- fm_dir(dir, if (identical(sens, FISHMIP_SENS)) soc else paste0(soc, "_", sens))
  path <- file.path(d, fishmip_filename(soc, var, sens, start, end))
  write.csv(df, path, row.names = FALSE)
  message(sprintf("  wrote %s  (%d rows)", path, nrow(df)))
  invisible(path)
}

## --------------------------------------------------------------------------
## Ensemble statistics
##   replaces the four hand-rolled quantile loops at
##   extract_fishmip_outputs.R:322-378. Medians, per AGENTS.md: across-member
##   distributions here are heavy-tailed and means are not robust.
## --------------------------------------------------------------------------

#' Ensemble quantiles across the FIRST margin of `x` (members).
#'
#' @param x  member x ... array
#' @return array with the member margin replaced by a length-5 `stat` margin
#'         (median, q05, q25, q75, q95), other margins preserved and moved first
ens_stats <- function(x) {
  d <- dim(x)
  if (is.null(d)) stop("ens_stats() needs an array with a member margin", call. = FALSE)
  rest <- seq_along(d)[-1]
  f <- function(v) c(median(v, na.rm = TRUE),
                     unname(quantile(v, c(0.05, 0.25, 0.75, 0.95), na.rm = TRUE)))
  out <- apply(x, rest, f)                   # stat x rest...
  out <- aperm(out, c(seq_along(rest) + 1L, 1L))   # rest... x stat
  dn <- dimnames(x)
  dimnames(out) <- c(if (is.null(dn)) vector("list", length(rest)) else dn[rest],
                     list(stat = FISHMIP_STATS))
  out
}

#' time x stat matrix -> protocol long CSV frame
stats_to_df <- function(m, times) {
  data.frame(time = year_to_days(times),
             median = m[, "median"], q05 = m[, "q05"], q25 = m[, "q25"],
             q75 = m[, "q75"], q95 = m[, "q95"],
             row.names = NULL)
}

#' time x level x stat array -> protocol long CSV frame, keyed by `key_name`
stats_to_long_df <- function(a, times, key_name, key_levels) {
  df <- expand.grid(time = year_to_days(times), KEY = key_levels,
                    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  for (s in FISHMIP_STATS) df[[s]] <- as.vector(a[, , s])
  df$KEY <- factor(df$KEY, levels = key_levels)
  df <- df[order(df$time, df$KEY), ]
  df$KEY <- as.character(df$KEY)
  names(df)[names(df) == "KEY"] <- key_name
  df[, c("time", key_name, FISHMIP_STATS)]
}

## --------------------------------------------------------------------------
## Member access -- streams the per-chunk files, both arms per member
##
## The collected `sims_{exploited,unexploited}_p104q10_n203.rds` files are the
## authoritative product, but each holds one arm and expands to ~1 GB. The
## `_chunks/` files that built them hold BOTH arms for 14 members at a time, so
## streaming them peaks near 200 MB and reads each member's state once. Which
## source is used is asserted against the ranking either way, so the numbers
## cannot silently differ.
## --------------------------------------------------------------------------

fm_usable_members <- function(rank_file = FM_RANK) {
  if (!file.exists(rank_file))
    stop("ranking not found: ", rank_file,
         "\nSet F0_RANK, or run through run_p104q10.R.", call. = FALSE)
  RR <- readRDS(rank_file)
  nm <- "FULL usable"
  if (is.null(RR$cuts[[nm]]))
    stop("no '", nm, "' cut in ", rank_file, call. = FALSE)
  sort(as.integer(RR$cuts[[nm]]))
}

#' Locate the member source and validate it against the ranking.
#'
#' @return list(kind = "chunks"|"collected", files/paths, members, order)
fm_member_source <- function() {
  usable <- fm_usable_members()
  if (length(usable) != FM_N_MEMBERS_EXPECTED)
    stop("expected ", FM_N_MEMBERS_EXPECTED, " usable members, ranking gives ",
         length(usable), call. = FALSE)

  chunk_dir <- file.path(SIM_DIR, "_chunks")
  fs <- sort(list.files(chunk_dir, pattern = "^sims_\\d+\\.rds$", full.names = TRUE))
  if (length(fs) > 0) {
    idx <- unlist(lapply(fs, function(f) {
      z <- readRDS(f); vapply(z, function(e) as.integer(e$sim_index), integer(1))
    }), use.names = FALSE)
    if (identical(sort(unique(idx)), usable) && length(idx) == length(usable))
      return(list(kind = "chunks", files = fs, members = usable, order = idx))
    message("note: _chunks/ does not match the ranking; falling back to the ",
            "collected arm files")
  }

  ex <- file.path(SIM_DIR, "sims_exploited_p104q10_n203.rds")
  un <- file.path(SIM_DIR, "sims_unexploited_p104q10_n203.rds")
  if (!file.exists(ex) || !file.exists(un))
    stop("no member source found. Expected either\n  ", chunk_dir,
         "\nor\n  ", ex, "\n  ", un,
         "\nBuild them with R/wmin_test/105_export_member_sims.R.", call. = FALSE)
  list(kind = "collected", exploited = ex, unexploited = un,
       members = usable, order = usable)
}

#' Stream every member, calling FUN(sim_index, exploited, unexploited).
#'
#' FUN must return a small summary; the MizerSim objects are released as soon
#' as it returns, which is what keeps peak memory near 200 MB.
#'
#' @return list of FUN's returns, named by sim_index, in the source's order
fm_iterate <- function(FUN, src = fm_member_source(), progress = TRUE) {
  out <- list()
  n <- length(src$members)
  done <- 0L
  tick <- function() {
    done <<- done + 1L
    if (progress && (done %% 20L == 0L || done == n))
      message(sprintf("    %d/%d members", done, n))
  }

  if (identical(src$kind, "chunks")) {
    for (f in src$files) {
      z <- readRDS(f)
      for (e in z) {
        si <- as.integer(e$sim_index)
        out[[as.character(si)]] <- FUN(si, e$exploited, e$unexploited)
        tick()
      }
      rm(z); gc(verbose = FALSE)
    }
  } else {
    se <- readRDS(src$exploited)
    su <- readRDS(src$unexploited)
    stopifnot(identical(names(se), names(su)))
    for (nm in names(se)) {
      out[[nm]] <- FUN(as.integer(nm), se[[nm]], su[[nm]])
      tick()
    }
    rm(se, su); gc(verbose = FALSE)
  }

  if (!setequal(as.integer(names(out)), src$members))
    stop("member stream did not cover the usable set", call. = FALSE)
  out
}

#' The 19 functional groups, in model order.
#'
#' Prefers `Manuscript data/spectra_ref_period_p104q10.rds$traits`, which the
#' manuscript pipeline writes and which is a few kB, so the species list can be
#' had without opening a 24 MB member chunk. Falls back to the first chunk.
fm_species <- function() {
  tr <- file.path("Manuscript data", "spectra_ref_period_p104q10.rds")
  if (file.exists(tr)) {
    sp <- readRDS(tr)$traits$species
    if (length(sp) == 19L) return(as.character(sp))
  }
  fs <- sort(list.files(file.path(SIM_DIR, "_chunks"),
                        pattern = "^sims_\\d+\\.rds$", full.names = TRUE))
  if (!length(fs)) stop("cannot determine the species list", call. = FALSE)
  z <- readRDS(fs[1])
  dimnames(z[[1]]$exploited@n)[[2]]
}

## --------------------------------------------------------------------------
## Provenance, for the NetCDF attributes and the README
## --------------------------------------------------------------------------

fm_provenance <- function() {
  list(
    ensemble               = "phase-104 arm q10",
    n_ensemble_members     = FM_N_MEMBERS_EXPECTED,
    usable_rule            = "stable AND drift_ok AND no erepro >= 1",
    base_params            = "params_ref_p100_mort_kernel_diet.rds",
    member_states          = file.path(OUT_LARGE, "104_full_states"),
    catchability_refit     = file.path(OUT_LARGE, "104_refit_wh_q10.rds"),
    catchability_qmax      = 10,
    ranking_source         = FM_RANK,
    simulations            = SIM_DIR,
    model_domain_area_m2   = MODEL_DOMAIN_AREA,
    area_note              = AREA_NOTE,
    domain_polygon         = "model_domains/Stacey/BanzareBank.rds",
    mizer_version          = as.character(utils::packageVersion("mizer")),
    thermizer_version      = as.character(utils::packageVersion("therMizer")),
    built                  = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
}

message("FM00_common.R loaded | area ", format(MODEL_DOMAIN_AREA, scientific = TRUE),
        " m2 | out ", FM_OUT_DIR)
