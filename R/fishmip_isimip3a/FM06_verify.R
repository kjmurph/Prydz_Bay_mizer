###############################################################################
# FM06_verify.R -- independent checks on the staged FishMIP outputs.
#
# Run:  Rscript run_p104q10.R R/fishmip_isimip3a/FM06_verify.R
#       (after FM01, FM02, FM03, and FM05 if the bundle is to be checked)
#
# Every CHECK is a hard assertion: the script stops on the first failure and
# names it. Every REPORT is printed for the record and asserts nothing, because
# the quantity is expected to move between ensembles.
#
# The point of check 6 is to tie these outputs to the manuscript pipeline: if
# `bsp` times the domain area does not reproduce `Manuscript data/
# biomass_abund_fish_p104q10.rds` exactly, then the submission and the paper
# are describing different ensembles.
###############################################################################

source("R/fishmip_isimip3a/FM00_common.R")

PASS <- 0L
ok <- function(msg) { PASS <<- PASS + 1L; cat(sprintf("  [ok]  %s\n", msg)) }
rep_ <- function(msg) cat(sprintf("  [--]  %s\n", msg))
need <- function(cond, msg) if (!isTRUE(cond)) stop("CHECK FAILED: ", msg, call. = FALSE) else ok(msg)

f1 <- file.path(FM_WORK, "FM01_totals.rds")
f2 <- file.path(FM_WORK, "FM02_species.rds")
f3 <- file.path(FM_WORK, "FM03_ancillary.rds")
for (f in c(f1, f2, f3))
  if (!file.exists(f)) stop("missing ", f, " -- run FM01-FM03 first", call. = FALSE)
T1 <- readRDS(f1); T2 <- readRDS(f2); T3 <- readRDS(f3)

TIMES <- T1$times; SPECIES <- T1$species; nT <- length(TIMES)

## ---------------------------------------------------------------------------
cat("\n1. Domain area, recomputed from the model domain polygon\n")
## ---------------------------------------------------------------------------
## Re-derives the area rather than restating the constant: reads the stored
## polygon, projects it to the same Lambert azimuthal equal-area projection
## BanzareBank.R used, and measures it.
POLY <- file.path("model_domains", "Stacey", "BanzareBank.rds")
LAEA <- "+proj=laea +lon_0=75 +lat_0=-65 +datum=WGS84"
if (file.exists(POLY) && requireNamespace("sf", quietly = TRUE)) {
  g <- readRDS(POLY)
  ## the stored polygon carries an old-style crs object, which sf notes on
  ## every transform -- harmless here, and quietened so it does not look like
  ## a check result
  a <- suppressMessages(suppressWarnings(
    sum(as.numeric(sf::st_area(sf::st_transform(g, LAEA))))))
  need(isTRUE(all.equal(signif(a, 7), signif(MODEL_DOMAIN_AREA, 7))),
       sprintf("polygon area = %.6e m2 matches MODEL_DOMAIN_AREA = %.6e (ratio %.9f)",
               a, MODEL_DOMAIN_AREA, a / MODEL_DOMAIN_AREA))
} else {
  rep_(paste("polygon or sf unavailable, area not re-derived:", POLY))
}

## The model state variable is grams over this polygon, so dividing by it is
## exactly invertible. Check 6 below is what actually proves that end to end.
rep_(sprintf("domain = %.6e m2 (%s km2), the basis the model was calibrated on",
             MODEL_DOMAIN_AREA,
             formatC(MODEL_DOMAIN_AREA / 1e6, format = "d", big.mark = ",")))

## ---------------------------------------------------------------------------
cat("\n2. Catch identity: sum_w(F * N * w * dw) == getYield(), on sampled members\n")
## ---------------------------------------------------------------------------
src <- fm_member_source()
set.seed(1L)
want <- sample(src$members, min(10L, length(src$members)))
worst <- 0
checked <- 0L
if (identical(src$kind, "chunks")) {
  for (f in src$files) {
    z <- readRDS(f)
    for (e in z) {
      if (!(as.integer(e$sim_index) %in% want)) next
      s <- e$exploited
      w <- s@params@w; dw <- s@params@dw
      mine <- apply(sweep(getFMort(s) * s@n, 3, w * dw, "*"), c(1, 2), sum)
      yy <- getYield(s)
      r <- abs(mine - yy) / pmax(abs(yy), 1e-300)
      worst <- max(worst, max(r[yy > 0])); checked <- checked + 1L
    }
    rm(z); gc(verbose = FALSE)
  }
} else {
  se <- readRDS(src$exploited)
  for (nm in as.character(want)) {
    s <- se[[nm]]
    w <- s@params@w; dw <- s@params@dw
    mine <- apply(sweep(getFMort(s) * s@n, 3, w * dw, "*"), c(1, 2), sum)
    yy <- getYield(s)
    r <- abs(mine - yy) / pmax(abs(yy), 1e-300)
    worst <- max(worst, max(r[yy > 0])); checked <- checked + 1L
  }
  rm(se); gc(verbose = FALSE)
}
need(checked == length(want), sprintf("sampled %d members", checked))
need(worst < 1e-10,
     sprintf("max relative difference vs getYield() = %.3e (< 1e-10)", worst))

## ---------------------------------------------------------------------------
cat("\n3. Membership\n")
## ---------------------------------------------------------------------------
usable <- fm_usable_members()
need(length(usable) == FM_N_MEMBERS_EXPECTED,
     sprintf("ranking 'FULL usable' holds %d members", length(usable)))
need(identical(sort(T1$members), usable), "FM01 covered exactly the usable set")
need(identical(sort(T2$members), usable), "FM02 covered exactly the usable set")

## ---------------------------------------------------------------------------
cat("\n4. Internal consistency\n")
## ---------------------------------------------------------------------------
for (arm in c("histsoc", "nat")) {
  d <- T1$raw[[arm]]$tcb - apply(T1$raw[[arm]]$tcblog10, c(1, 2), sum)
  need(min(d) >= 0, sprintf("tcb >= sum(tcblog10) in %s (min margin %.4e)", arm, min(d)))
}
need(max(T1$raw$histsoc$tc_nowhales - T1$raw$histsoc$tc) <= 0,
     "tc_nowhales <= tc for every member and year")

## tc must decompose into csp -- this is the check that ties FM01 to FM02
mm <- intersect(rownames(T1$raw$histsoc$tc), rownames(T2$raw$csp_histsoc))
need(length(mm) == FM_N_MEMBERS_EXPECTED, "FM01 and FM02 share all members")
csp_all <- apply(T2$raw$csp_histsoc[mm, , , drop = FALSE], c(1, 2), sum)
r1 <- max(abs(csp_all - T1$raw$histsoc$tc[mm, ]) /
          pmax(abs(T1$raw$histsoc$tc[mm, ]), 1e-300))
need(r1 < 1e-10, sprintf("tc == sum over groups of csp (max rel diff %.3e)", r1))

wi <- which(SPECIES %in% WHALE_GROUPS)
csp_wh <- apply(T2$raw$csp_histsoc[mm, , wi, drop = FALSE], c(1, 2), sum)
dif <- T1$raw$histsoc$tc[mm, ] - T1$raw$histsoc$tc_nowhales[mm, ]
r2 <- max(abs(csp_wh - dif) / pmax(abs(dif), 1e-300))
need(r2 < 1e-10,
     sprintf("tc - tc_nowhales == whale csp (max rel diff %.3e)", r2))

## ---------------------------------------------------------------------------
cat("\n5. The nat arm is unfished\n")
## ---------------------------------------------------------------------------
need(max(abs(T1$raw$nat$tc)) == 0, "nat tc is identically zero")
need(max(abs(T1$raw$nat$tclog10)) == 0, "nat tclog10 is identically zero")
need(max(abs(T2$raw$csp_nat)) == 0, "nat csp is identically zero")

## ---------------------------------------------------------------------------
cat("\n6. Tie to the manuscript pipeline\n")
## ---------------------------------------------------------------------------
MB <- file.path("Manuscript data", "biomass_abund_fish_p104q10.rds")
if (file.exists(MB)) {
  bm <- readRDS(MB)
  yrs <- c(1841, 1950, 2010)
  sub <- bm[bm$Year %in% yrs, , drop = FALSE]
  key <- paste(sub$sim_index, sub$Year, sub$Species)
  grid <- expand.grid(member = rownames(T2$raw$bsp_histsoc),
                      Year = yrs, Species = SPECIES,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  ti <- match(as.character(grid$Year), dimnames(T2$raw$bsp_histsoc)$time)
  si <- match(grid$Species, SPECIES)
  mi <- match(grid$member, rownames(T2$raw$bsp_histsoc))
  mine <- T2$raw$bsp_histsoc[cbind(mi, ti, si)] * MODEL_DOMAIN_AREA
  theirs <- sub$Biomass[match(paste(grid$member, grid$Year, grid$Species), key)]
  need(!anyNA(theirs), "every (member, year, group) found in the manuscript build")
  r <- abs(mine - theirs) / pmax(abs(theirs), 1e-300)
  need(max(r) < 1e-10,
       sprintf("bsp x area reproduces %s at 1841/1950/2010 (max rel diff %.3e, %d values)",
               basename(MB), max(r), length(r)))
} else {
  rep_(paste("manuscript build not present, tie not checked:", MB))
}

## ---------------------------------------------------------------------------
cat("\n7. Written CSV shapes\n")
## ---------------------------------------------------------------------------
expect_days <- year_to_days(1841:2010)
chk_csv <- function(dir, soc, var, sens, rows) {
  d <- file.path(dir, if (identical(sens, FISHMIP_SENS)) soc else paste0(soc, "_", sens))
  p <- file.path(d, fishmip_filename(soc, var, sens))
  need(file.exists(p), paste("exists:", basename(p)))
  df <- read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
  need(nrow(df) == rows, sprintf("%s has %d rows", basename(p), rows))
  need(identical(sort(unique(df$time)), expect_days),
       sprintf("%s time axis is 1841-2010 at 365 d/yr", basename(p)))
  invisible(df)
}
nS <- length(SPECIES); nB <- length(FISHMIP_BIN_NAMES)
chk_csv(FM_DIR_TOTALS, "histsoc", "tcb", FISHMIP_SENS, nT)
chk_csv(FM_DIR_TOTALS, "histsoc", "tcblog10", FISHMIP_SENS, nT * nB)
chk_csv(FM_DIR_TOTALS, "histsoc", "tc", FISHMIP_SENS, nT)
chk_csv(FM_DIR_TOTALS, "histsoc", "tclog10", FISHMIP_SENS, nT * nB)
chk_csv(FM_DIR_TOTALS, "histsoc", "tc", FISHMIP_SENS_NOWHALES, nT)
chk_csv(FM_DIR_TOTALS, "histsoc", "tclog10", FISHMIP_SENS_NOWHALES, nT * nB)
chk_csv(FM_DIR_TOTALS, "nat", "tcb", FISHMIP_SENS, nT)
chk_csv(FM_DIR_TOTALS, "nat", "tcblog10", FISHMIP_SENS, nT * nB)
chk_csv(FM_DIR_SPECIES, "histsoc", "bsp", FISHMIP_SENS, nT * nS)
chk_csv(FM_DIR_SPECIES, "histsoc", "csp", FISHMIP_SENS, nT * nS)
chk_csv(FM_DIR_SPECIES, "nat", "bsp", FISHMIP_SENS, nT * nS)
chk_csv(FM_DIR_ANCILLARY, "histsoc", "catchobs", FISHMIP_SENS, nT * nS)
chk_csv(FM_DIR_ANCILLARY, "histsoc", "catchobs", FISHMIP_SENS_NOWHALES, nT * nS)
chk_csv(FM_DIR_ANCILLARY, "histsoc", "effort", FISHMIP_SENS, nT * nS)

## quantiles must be ordered
tcb <- chk_csv(FM_DIR_TOTALS, "histsoc", "tcb", FISHMIP_SENS, nT)
need(all(tcb$q05 <= tcb$q25 & tcb$q25 <= tcb$median &
         tcb$median <= tcb$q75 & tcb$q75 <= tcb$q95),
     "tcb quantiles are monotone q05 <= q25 <= median <= q75 <= q95")

## ---------------------------------------------------------------------------
cat("\n7b. NetCDF round-trip\n")
## ---------------------------------------------------------------------------
NCP <- file.path(FM_DIR_TOTALS, "histsoc",
                 fishmip_filename("histsoc", "allvars", FISHMIP_SENS, ext = "nc"))
if (requireNamespace("ncdf4", quietly = TRUE) && file.exists(NCP)) {
  nc <- ncdf4::nc_open(NCP)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  need(setequal(names(nc$var), c("tcb", "tcblog10", "tc", "tclog10", "bsp", "csp")),
       "histsoc NetCDF holds all six variables")
  ## the NetCDF is float precision, so compare to single-precision tolerance
  v <- ncdf4::ncvar_get(nc, "tcb")                       # time x statistic
  ref <- T1$stats$histsoc$tcb[, "median"]
  r <- max(abs(v[, 1] - ref) / pmax(abs(ref), 1e-300))
  need(r < 1e-6, sprintf("NetCDF tcb median matches the CSV to float precision (%.3e)", r))
  a <- ncdf4::ncatt_get(nc, 0)
  need(isTRUE(all.equal(a$model_domain_area_m2, MODEL_DOMAIN_AREA)),
       "NetCDF records model_domain_area_m2")
  need(identical(a$model_domain_polygon, "model_domains/Stacey/BanzareBank.rds"),
       "NetCDF records the model domain polygon")
  need(identical(a$n_ensemble_members, FM_N_MEMBERS_EXPECTED),
       "NetCDF records n_ensemble_members = 203")
  need(identical(a$species_names, paste(SPECIES, collapse = ", ")),
       "NetCDF records the 19 group names in model order")
  need(nzchar(a$area_note), "NetCDF carries the area note")
} else {
  rep_("NetCDF not present or ncdf4 unavailable -- round-trip not checked")
}

## ---------------------------------------------------------------------------
cat("\n8. Staged bundle\n")
## ---------------------------------------------------------------------------
## split_csv/ is the upload set, so every file in it is checked against the
## full-span file it was cut from: named for the years it covers, on its own
## time reference (days since 1841-1-1 for 1841-1960, days since 1901-1-1 for
## 1961-2010), and carrying every value over unchanged.
sub_dir <- file.path(FM_OUT_DIR, "submission_csv")
if (dir.exists(sub_dir)) {
  split_dir <- file.path(FM_OUT_DIR, "split_csv")
  sp_dir <- file.path(split_dir, "temp2")
  ex_dir <- file.path(split_dir, "temp")
  full_span <- sprintf("_%s_%s.csv", FISHMIP_START, FISHMIP_END)
  sp_days <- year_to_days(1841:1960)
  ex_days <- year_to_days(1961:2010, ref = 1901L)
  rd <- function(p) read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
  n_full <- 0L
  for (sc in list.dirs(sub_dir, recursive = FALSE)) {
    fs <- list.files(sc, pattern = "\\.csv$")
    n_full <- n_full + length(fs)
    bad <- character(0)
    for (f in fs) {
      stem <- substr(f, 1L, nchar(f) - nchar(full_span))
      p_sp <- file.path(sp_dir, paste0(stem, "_1841_1960.csv"))
      p_ex <- file.path(ex_dir, paste0(stem, "_1961_2010.csv"))
      if (!endsWith(f, full_span) || !file.exists(p_sp) || !file.exists(p_ex)) {
        bad <- c(bad, f); next
      }
      full <- rd(file.path(sc, f)); a <- rd(p_sp); b <- rd(p_ex)
      back <- b; back$time <- back$time + year_to_days(1901L)
      back <- rbind(a, back)
      same <- identical(names(back), names(full)) && nrow(back) == nrow(full) &&
        all(mapply(identical, back, full))
      if (!identical(sort(unique(a$time)), sp_days) ||
          !identical(sort(unique(b$time)), ex_days) || !same)
        bad <- c(bad, f)
    }
    need(!length(bad),
         sprintf("%s: %d CSVs split to 1841_1960 (days since 1841) + 1961_2010 (days since 1901), values unchanged%s",
                 basename(sc), length(fs),
                 if (length(bad)) paste0(" -- FAILED: ", paste(bad, collapse = ", ")) else ""))
  }
  stray <- list.files(split_dir, recursive = TRUE)
  stray <- stray[!dirname(stray) %in% c("temp", "temp2")]
  need(!length(stray),
       sprintf("split_csv/ has no file outside temp/ and temp2/%s",
               if (length(stray)) paste0(" -- FOUND: ", paste(stray, collapse = ", ")) else ""))
  n_sp <- length(list.files(sp_dir, pattern = "\\.csv$"))
  n_ex <- length(list.files(ex_dir, pattern = "\\.csv$"))
  need(n_sp == n_full && n_ex == n_full,
       sprintf("temp2/ and temp/ hold %d and %d files, one per full-span CSV (%d), none stale",
               n_sp, n_ex, n_full))
  need(file.exists(file.path(FM_OUT_DIR, "README.md")), "bundle README.md written")
} else {
  rep_("bundle not staged yet -- run FM05_stage_bundle.R")
}

## ---------------------------------------------------------------------------
cat("\n9. Reports (no assertion -- these move with the ensemble)\n")
## ---------------------------------------------------------------------------
S1 <- T1$stats
sel <- TIMES >= 1961 & TIMES <= 2010

OLDTCB <- file.path("fishmip_outputs",
  "mizer_gfdl-mom6-cobalt2_obsclim_histsoc_default_tcb_prydz-bay_annual_1841_2010.csv")
if (file.exists(OLDTCB)) {
  old <- read.csv(OLDTCB)
  for (y in c(1841, 2010)) {
    o <- old$median[old$time == year_to_days(y)]
    n <- S1$histsoc$tcb[which(TIMES == y), "median"]
    ## the May 2026 root-level re-run used this same area, so the ratio below
    ## is the ensemble change alone -- 2111 members -> phase-104's 203
    rep_(sprintf("tcb %d: previous %.4f -> now %.4f (x%.4f). Same area, so this is the ensemble change.",
                 y, o, n, n / o))
  }
} else {
  rep_("previous root-level tcb not found, no comparison made")
}

rep_(sprintf("tc  1961-2010 median  %.6e g m-2 | 1841-2010 total %.6e",
             median(S1$histsoc$tc[sel, "median"]), sum(S1$histsoc$tc[, "median"])))
rep_(sprintf("tc  nowhales, same    %.6e g m-2 | 1841-2010 total %.6e",
             median(S1$histsoc$tc_nowhales[sel, "median"]),
             sum(S1$histsoc$tc_nowhales[, "median"])))
rep_(sprintf("whaling share of MODELLED catch mass  %.2f%%",
             100 * (1 - sum(S1$histsoc$tc_nowhales[, "median"]) /
                        sum(S1$histsoc$tc[, "median"]))))
rep_(sprintf("whaling share of RECORDED catch mass  %.2f%%",
             100 * T3$whale_share_of_recorded_catch))
rep_(sprintf("tcb histsoc 1841 %.4f -> 2010 %.4f g m-2",
             S1$histsoc$tcb[1, "median"], S1$histsoc$tcb[nT, "median"]))
rep_(sprintf("tcb nat     1841 %.4f -> 2010 %.4f g m-2",
             S1$nat$tcb[1, "median"], S1$nat$tcb[nT, "median"]))

cat(sprintf("\n=== %d checks passed ===\n", PASS))
cat("FM06 done.\n")
