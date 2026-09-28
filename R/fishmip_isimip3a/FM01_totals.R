###############################################################################
# FM01_totals.R -- tcb, tcblog10, tc, tclog10 and the fisheries-only catch
#                  variant, on the phase-104 (q10) ensemble.
#
# Run:  Rscript run_p104q10.R R/fishmip_isimip3a/FM01_totals.R
#
# Variables (all g m-2, ensemble median / q05 / q25 / q75 / q95 across the 203
# usable members):
#
#   tcb       Total Consumer Biomass Density.
#             ALL 19 functional groups over the FULL size range (3.16e-8 g up).
#             Every group is a consumer (TL > 1) -- mesozooplankton, krill and
#             salps included. The prescribed plankton resource (n_pp) is
#             EXCLUDED: it is built from phypico + phydiat + phydiaz (TL = 1)
#             and zmicro + zmeso, so including it would add primary producers
#             and double-count the explicit mesozooplankton group
#             (docs/manuscript_methods_supplement.md:425-455).
#             tcb > sum(tcblog10) by construction, because tcblog10 starts at 1 g.
#
#   tcblog10  The same biomass in the six FishMIP log10 classes, >= 1 g only.
#
#   tc        Total Catch Density, all 19 groups, all sizes.
#   tclog10   The same catch in the six log10 classes, >= 1 g only.
#
#   tc / tclog10 under sens = "nowhales"
#             The same two quantities with minke whales, orca, sperm whales and
#             baleen whales removed. FishMIP's catch reconstruction covers
#             fisheries, not whaling, and the whale groups dominate the catch
#             mass, so the full `tc` is not comparable with it. Excluded by
#             species name; the ten groups with catchability 0 are unaffected.
#
# Catch is F * N * w * dw summed over size, with F from getFMort() (already
# summed over gears). Verified identical to mizer's own getYield() to 2.2e-16
# -- FM06_verify.R re-runs that check.
#
# The nat (unfished) arm carries zero effort, so its catch is asserted to be
# exactly zero and no nat catch CSV is written, matching the previous
# submission. The zeros are not hardcoded the way
# extract_fishmip_outputs_climate_only.R:176-177 did it -- they are computed
# and then checked.
###############################################################################

source("R/fishmip_isimip3a/FM00_common.R")

src <- fm_member_source()
message("member source: ", src$kind, " | ", length(src$members), " members")

## ---------------------------------------------------------------------------
## Per-member extraction
## ---------------------------------------------------------------------------

BIN_ASSIGN <- NULL   # filled from the first member's weight grid
SPECIES    <- NULL
TIMES      <- NULL
NOWHALE    <- NULL

one_arm <- function(sim) {
  n_array <- sim@n
  w_vec   <- sim@params@w
  dw_vec  <- sim@params@dw

  ## ---- biomass ----
  biomass_array <- calc_biomass_from_n(n_array, w_vec, dw_vec)
  tcb      <- apply(biomass_array, 1, sum)
  tcblog10 <- apply(aggregate_to_fishmip_bins(biomass_array, BIN_ASSIGN), c(1, 3), sum)

  ## ---- catch: F * N * w * dw ----
  fmort <- getFMort(sim)                       # time x species x size
  catch_array <- sweep(fmort * n_array, 3, w_vec * dw_vec, "*")
  tc      <- apply(catch_array, 1, sum)
  tclog10 <- apply(aggregate_to_fishmip_bins(catch_array, BIN_ASSIGN), c(1, 3), sum)

  ## ---- catch excluding the whale groups ----
  ca_nw <- catch_array[, NOWHALE, , drop = FALSE]
  tc_nw      <- apply(ca_nw, 1, sum)
  tclog10_nw <- apply(aggregate_to_fishmip_bins(ca_nw, BIN_ASSIGN), c(1, 3), sum)

  list(tcb = tcb / MODEL_DOMAIN_AREA,
       tcblog10 = tcblog10 / MODEL_DOMAIN_AREA,
       tc = tc / MODEL_DOMAIN_AREA,
       tclog10 = tclog10 / MODEL_DOMAIN_AREA,
       tc_nw = tc_nw / MODEL_DOMAIN_AREA,
       tclog10_nw = tclog10_nw / MODEL_DOMAIN_AREA)
}

message("extracting totals...")
Z <- fm_iterate(function(si, exploited, unexploited) {
  if (is.null(BIN_ASSIGN)) {
    BIN_ASSIGN <<- assign_fishmip_bins(exploited@params@w)
    SPECIES    <<- dimnames(exploited@n)[[2]]
    TIMES      <<- as.numeric(dimnames(exploited@n)$time)
    NOWHALE    <<- !(SPECIES %in% WHALE_GROUPS)
    message("  size classes: ", sum(!is.na(BIN_ASSIGN)), " of ",
            length(BIN_ASSIGN), " mizer bins are >= 1 g")
    message("  fisheries-only catch keeps ", sum(NOWHALE), " of ",
            length(SPECIES), " groups (drops: ",
            paste(SPECIES[!NOWHALE], collapse = ", "), ")")
  }
  ## species order and time axis must not drift between members or arms
  stopifnot(identical(dimnames(exploited@n)[[2]], SPECIES),
            identical(dimnames(unexploited@n)[[2]], SPECIES),
            identical(as.numeric(dimnames(exploited@n)$time), TIMES),
            identical(as.numeric(dimnames(unexploited@n)$time), TIMES))
  list(histsoc = one_arm(exploited), nat = one_arm(unexploited))
}, src = src)

nT <- length(TIMES); nM <- length(Z); nB <- length(FISHMIP_BIN_NAMES)
message(sprintf("extracted %d members x %d years", nM, nT))

## ---------------------------------------------------------------------------
## Stack members -> member x time [x size class]
## ---------------------------------------------------------------------------

stack_vec <- function(arm, field) {
  m <- t(vapply(Z, function(z) z[[arm]][[field]], numeric(nT)))
  dimnames(m) <- list(member = names(Z), time = as.character(TIMES))
  m
}
stack_arr <- function(arm, field) {
  a <- array(NA_real_, dim = c(nM, nT, nB),
             dimnames = list(member = names(Z), time = as.character(TIMES),
                             size_class = FISHMIP_BIN_NAMES))
  for (i in seq_len(nM)) a[i, , ] <- Z[[i]][[arm]][[field]]
  a
}

RAW <- list(
  histsoc = list(tcb = stack_vec("histsoc", "tcb"),
                 tcblog10 = stack_arr("histsoc", "tcblog10"),
                 tc = stack_vec("histsoc", "tc"),
                 tclog10 = stack_arr("histsoc", "tclog10"),
                 tc_nowhales = stack_vec("histsoc", "tc_nw"),
                 tclog10_nowhales = stack_arr("histsoc", "tclog10_nw")),
  nat = list(tcb = stack_vec("nat", "tcb"),
             tcblog10 = stack_arr("nat", "tcblog10"),
             tc = stack_vec("nat", "tc"),
             tclog10 = stack_arr("nat", "tclog10"))
)

## ---------------------------------------------------------------------------
## Assertions before anything is written
## ---------------------------------------------------------------------------

## nat is unfished: catch must be exactly zero, not merely small
if (max(abs(RAW$nat$tc)) != 0 || max(abs(RAW$nat$tclog10)) != 0)
  stop("nat arm has non-zero catch (max ", max(abs(RAW$nat$tc)),
       ") -- the unexploited simulations are not unfished", call. = FALSE)
message("check: nat arm catch is identically zero -- ok")

## tcb spans all sizes, tcblog10 only >= 1 g
for (arm in c("histsoc", "nat")) {
  d <- RAW[[arm]]$tcb - apply(RAW[[arm]]$tcblog10, c(1, 2), sum)
  if (min(d) < 0)
    stop("tcb < sum(tcblog10) in arm ", arm, " (min ", min(d), ")", call. = FALSE)
}
message("check: tcb >= sum(tcblog10) in both arms -- ok")

## the fisheries-only subset cannot exceed the total
if (max(RAW$histsoc$tc_nowhales - RAW$histsoc$tc) > 0)
  stop("tc_nowhales exceeds tc", call. = FALSE)
message("check: tc_nowhales <= tc -- ok")

## ---------------------------------------------------------------------------
## Ensemble statistics and CSVs
## ---------------------------------------------------------------------------

STATS <- list()
for (arm in names(RAW)) STATS[[arm]] <- lapply(RAW[[arm]], ens_stats)

message("writing CSVs...")

# --- histsoc, default sens ---
fm_write_csv(stats_to_df(STATS$histsoc$tcb, TIMES), FM_DIR_TOTALS, "histsoc", "tcb")
fm_write_csv(stats_to_long_df(STATS$histsoc$tcblog10, TIMES, "size_class", FISHMIP_BIN_NAMES),
             FM_DIR_TOTALS, "histsoc", "tcblog10")
fm_write_csv(stats_to_df(STATS$histsoc$tc, TIMES), FM_DIR_TOTALS, "histsoc", "tc")
fm_write_csv(stats_to_long_df(STATS$histsoc$tclog10, TIMES, "size_class", FISHMIP_BIN_NAMES),
             FM_DIR_TOTALS, "histsoc", "tclog10")

# --- histsoc, fisheries-only ---
fm_write_csv(stats_to_df(STATS$histsoc$tc_nowhales, TIMES),
             FM_DIR_TOTALS, "histsoc", "tc", sens = FISHMIP_SENS_NOWHALES)
fm_write_csv(stats_to_long_df(STATS$histsoc$tclog10_nowhales, TIMES, "size_class", FISHMIP_BIN_NAMES),
             FM_DIR_TOTALS, "histsoc", "tclog10", sens = FISHMIP_SENS_NOWHALES)

# --- nat: biomass only (catch is identically zero, asserted above) ---
fm_write_csv(stats_to_df(STATS$nat$tcb, TIMES), FM_DIR_TOTALS, "nat", "tcb")
fm_write_csv(stats_to_long_df(STATS$nat$tcblog10, TIMES, "size_class", FISHMIP_BIN_NAMES),
             FM_DIR_TOTALS, "nat", "tcblog10")

## ---------------------------------------------------------------------------
## Per-member raw and the stats arrays FM04 needs
## ---------------------------------------------------------------------------

fm_dir(FM_WORK)
saveRDS(list(raw = RAW, stats = STATS, times = TIMES, species = SPECIES,
             size_classes = FISHMIP_BIN_NAMES, members = as.integer(names(Z)),
             provenance = fm_provenance()),
        file.path(FM_WORK, "FM01_totals.rds"))
message("  wrote ", file.path(FM_WORK, "FM01_totals.rds"))

## ---------------------------------------------------------------------------
## Summary
## ---------------------------------------------------------------------------

yr <- TIMES
sel <- yr >= 1961 & yr <= 2010
cat("\n--- FM01 summary (g m-2, ensemble median) ---\n")
cat(sprintf("tcb  histsoc  1841 %.4f | 2010 %.4f | 1961-2010 median %.4f\n",
            STATS$histsoc$tcb[1, "median"], STATS$histsoc$tcb[nT, "median"],
            median(STATS$histsoc$tcb[sel, "median"])))
cat(sprintf("tcb  nat      1841 %.4f | 2010 %.4f | 1961-2010 median %.4f\n",
            STATS$nat$tcb[1, "median"], STATS$nat$tcb[nT, "median"],
            median(STATS$nat$tcb[sel, "median"])))
cat(sprintf("tc            1961-2010 median %.6e | total over 1841-2010 %.6e\n",
            median(STATS$histsoc$tc[sel, "median"]), sum(STATS$histsoc$tc[, "median"])))
cat(sprintf("tc nowhales   1961-2010 median %.6e | total over 1841-2010 %.6e\n",
            median(STATS$histsoc$tc_nowhales[sel, "median"]),
            sum(STATS$histsoc$tc_nowhales[, "median"])))
cat(sprintf("whaling share of total catch mass: %.1f%%\n",
            100 * (1 - sum(STATS$histsoc$tc_nowhales[, "median"]) /
                       sum(STATS$histsoc$tc[, "median"]))))
cat("\ntcblog10 histsoc, median over 1961-2010 by size class:\n")
for (b in seq_len(nB))
  cat(sprintf("  %-11s %.6e\n", FISHMIP_BIN_NAMES[b],
              median(STATS$histsoc$tcblog10[sel, b, "median"])))
cat("\nFM01 done.\n")
