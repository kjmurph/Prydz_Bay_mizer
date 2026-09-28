###############################################################################
# FM05_stage_bundle.R -- assemble the upload-ready bundle from the FM01-FM04
#                        outputs, and write its README.
#
# Run:  Rscript run_p104q10.R R/fishmip_isimip3a/FM05_stage_bundle.R
#       (after FM01, FM02, FM03 and -- if ncdf4 is available -- FM04)
#
# Produces, under FM_OUT_DIR:
#
#   submission_csv/<scenario>/                 every CSV for that scenario, 1841-2010
#   split_csv/
#       temp2/                                 1841-1960, every scenario
#       temp/                                  1961-2010, every scenario
#   optional_full/<scenario>/                  NetCDF + per-member RDS
#   README.md
#
# THE PERIOD SPLIT (protocol, "Output data" and "Path to output files on DKRZ")
# -----------------------------------------------------------------------------
# Each output is saved as two files, one per period, and each file is named for
# the years it covers and counts time from its own reference date:
#
#   split_csv/temp2/  ..._annual_1841_1960.csv  days since 1841-1-1  0 ... 43435
#       -> /work/bb0820/ISIMIP/ISIMIP3a/UploadArea/marine-fishery_regional/model_name/temp2
#   split_csv/temp/   ..._annual_1961_2010.csv  days since 1901-1-1  21900 ... 39785
#       -> /work/bb0820/ISIMIP/ISIMIP3a/UploadArea/marine-fishery_regional/model_name/temp
#
# The two folders mirror the DKRZ targets, so each is uploaded as one copy. The
# soc and sens slots keep the filenames unique across scenarios, and the split
# stops if two scenarios ever produce the same name.
#
# Calendar: 365-day, as the protocol text specifies and as the full-span files
# already use. The protocol's own time_axis_*.csv were generated with leap days
# (1961-01-01 = 21915 days since 1901, not 21900); the README records this.
#
# Windows MAX_PATH is 260 and long paths are disabled on this machine. The repo
# sits 90 characters deep under OneDrive and the protocol filenames run to 88;
# an earlier layout, `submission_csv_split/<scenario>/experiment_1961_2010/`,
# came to 261 characters and failed mid-write. fm_check_paths() refuses to
# start a stage that would not fit.
#
# This script will REFUSE to write into the previous submission directory. That
# tree was built from the retired 2,111-member ensemble on a domain area 13.23x
# too large, and it is kept as-is so the old submission stays auditable.
###############################################################################

source("R/fishmip_isimip3a/FM00_common.R")

## --- safety: never clobber the previous submission ---------------------------
FORBIDDEN <- c("FishMIP_ISIMIP3a_submission", "fishmip_outputs",
               "fishmip_outputs_climate_only", "fishmip_outputs_species")
if (basename(normalizePath(FM_OUT_DIR, mustWork = FALSE)) %in% FORBIDDEN)
  stop("refusing to stage into '", FM_OUT_DIR, "': that is a previous output ",
       "tree.\nSet FM_OUT_DIR to a new directory.", call. = FALSE)

SPINUP_LAST  <- year_to_days(1960)   # 43435
EXPERIMENT_1 <- year_to_days(1961)   # 43800

## Experiment files count days from 1901-1-1 rather than 1841-1-1: on the
## 365-day calendar that is a shift of exactly 60 years.
EXPERIMENT_REF   <- 1901L
EXPERIMENT_SHIFT <- year_to_days(EXPERIMENT_REF)   # 21900

SPLIT_DIR    <- file.path(FM_OUT_DIR, "split_csv")
SPLIT_SPINUP <- file.path(SPLIT_DIR, "temp2")   # DKRZ .../temp2, 1841-1960
SPLIT_EXPER  <- file.path(SPLIT_DIR, "temp")    # DKRZ .../temp,  1961-2010

FULL_SPAN <- sprintf("_%s_%s.csv", FISHMIP_START, FISHMIP_END)   # _1841_2010.csv

#' A full-span filename renamed for one period.
#'
#' Stops rather than passing a name through unchanged: a split file that kept
#' `_1841_2010` would be uploaded claiming years it does not contain.
period_name <- function(f, start, end) {
  if (!endsWith(f, FULL_SPAN))
    stop("'", f, "' does not end in ", FULL_SPAN, call. = FALSE)
  paste0(substr(f, 1L, nchar(f) - nchar(FULL_SPAN)),
         sprintf("_%s_%s.csv", start, end))
}
spinup_name <- function(f) period_name(f, FISHMIP_START, "1960")
exper_name  <- function(f) period_name(f, "1961", FISHMIP_END)

SCENARIOS <- c("histsoc", "nat", "histsoc_nowhales")

SRC_DIRS <- c(FM_DIR_TOTALS, FM_DIR_SPECIES, FM_DIR_ANCILLARY)

## ---------------------------------------------------------------------------
## 1. Gather CSVs per scenario
## ---------------------------------------------------------------------------

message("staging submission_csv/ ...")
staged <- list()

## Check every path this script will write BEFORE writing any of them, so a
## length failure is reported up front rather than halfway through a scenario.
planned <- unlist(lapply(SCENARIOS, function(sc) {
  fs <- unlist(lapply(SRC_DIRS, function(d) {
    p <- file.path(d, sc)
    if (dir.exists(p)) list.files(p, pattern = "\\.csv$") else character(0)
  }), use.names = FALSE)
  if (!length(fs)) return(character(0))
  c(file.path(FM_OUT_DIR, "submission_csv", sc, fs),
    file.path(SPLIT_SPINUP, vapply(fs, spinup_name, character(1), USE.NAMES = FALSE)),
    file.path(SPLIT_EXPER,  vapply(fs, exper_name,  character(1), USE.NAMES = FALSE)))
}), use.names = FALSE)
longest <- fm_check_paths(planned, "staged CSV")
message(sprintf("  path check: %d files, longest path %d of %d characters",
                length(planned), longest, FM_MAX_PATH))

for (sc in SCENARIOS) {
  files <- unlist(lapply(SRC_DIRS, function(d) {
    p <- file.path(d, sc)
    if (dir.exists(p)) list.files(p, pattern = "\\.csv$", full.names = TRUE) else character(0)
  }), use.names = FALSE)

  if (!length(files)) { message("  ", sc, ": nothing to stage"); next }

  out <- fm_dir(FM_OUT_DIR, "submission_csv", sc)
  ok <- file.copy(files, file.path(out, basename(files)), overwrite = TRUE)
  if (!all(ok)) stop("failed to copy: ",
                     paste(basename(files)[!ok], collapse = ", "), call. = FALSE)
  staged[[sc]] <- basename(files)
  message(sprintf("  %-18s %d files", sc, length(files)))
}

if (!length(staged)) stop("nothing was staged -- run FM01-FM03 first", call. = FALSE)

## ---------------------------------------------------------------------------
## 2. Split each CSV by period
## ---------------------------------------------------------------------------

message("staging split_csv/ ...")
## split_csv/ is written by nothing but this section, so it is emptied first: a
## file left over from an earlier naming or layout must not survive to be
## uploaded alongside the current one. What is checked is that no FILE remains.
## Under OneDrive the folders are read-only reparse points that unlink() cannot
## remove, but an empty folder cannot be uploaded by mistake.
if (dir.exists(SPLIT_DIR)) {
  unlink(SPLIT_DIR, recursive = TRUE)
  left <- list.files(SPLIT_DIR, recursive = TRUE, all.files = TRUE)
  if (length(left))
    stop("could not clear ", SPLIT_DIR, ": ", length(left), " file(s) remain, e.g. ",
         left[1], call. = FALSE)
}
d_sp <- fm_dir(SPLIT_SPINUP)
d_ex <- fm_dir(SPLIT_EXPER)
for (sc in names(staged)) {
  n_sp <- 0L; n_ex <- 0L
  for (f in staged[[sc]]) {
    df <- read.csv(file.path(FM_OUT_DIR, "submission_csv", sc, f),
                   stringsAsFactors = FALSE, check.names = FALSE)
    if (!"time" %in% names(df))
      stop("no `time` column in ", f, call. = FALSE)
    sp <- df[df$time <= SPINUP_LAST, , drop = FALSE]
    ex <- df[df$time >= EXPERIMENT_1, , drop = FALSE]
    if (nrow(sp) + nrow(ex) != nrow(df))
      stop("period split lost or duplicated rows in ", f, call. = FALSE)
    ex$time <- ex$time - EXPERIMENT_SHIFT
    p_sp <- file.path(d_sp, spinup_name(f))
    p_ex <- file.path(d_ex, exper_name(f))
    ## Both folders hold every scenario, so a name collision would silently
    ## overwrite one scenario with another.
    if (file.exists(p_sp) || file.exists(p_ex))
      stop("two scenarios map to the same split filename: ", basename(p_sp),
           call. = FALSE)
    write.csv(sp, p_sp, row.names = FALSE)
    write.csv(ex, p_ex, row.names = FALSE)
    n_sp <- n_sp + nrow(sp); n_ex <- n_ex + nrow(ex)
  }
  message(sprintf("  %-18s temp2 (1841-1960) %d rows | temp (1961-2010) %d rows",
                  sc, n_sp, n_ex))
}

## ---------------------------------------------------------------------------
## 3. Reference copies: NetCDF and per-member RDS
## ---------------------------------------------------------------------------

message("staging optional_full/ ...")
nc_found <- 0L
for (sc in SCENARIOS) {
  p <- file.path(FM_DIR_TOTALS, sc)
  ncs <- if (dir.exists(p)) list.files(p, pattern = "\\.nc$", full.names = TRUE) else character(0)
  if (!length(ncs)) next
  out <- fm_dir(FM_OUT_DIR, "optional_full", sc)
  file.copy(ncs, file.path(out, basename(ncs)), overwrite = TRUE)
  nc_found <- nc_found + length(ncs)
  message(sprintf("  %-18s %d NetCDF", sc, length(ncs)))
}
if (nc_found == 0L)
  message("  no NetCDF found -- run FM04_netcdf.R (needs the ncdf4 package). ",
          "The CSVs are the upload set, so the bundle is still complete without it.")

## Per-member arrays. Unlike the previous bundle's `*_all_sims.rds`, which
## indexed members 1..n and so lost their identity, these carry the real
## `sim_index` in their dimnames and can be joined to the ranking.
out_ref <- fm_dir(FM_OUT_DIR, "optional_full", "per_member")
for (f in c("FM01_totals.rds", "FM02_species.rds", "FM03_ancillary.rds")) {
  src <- file.path(FM_WORK, f)
  if (file.exists(src)) {
    file.copy(src, file.path(out_ref, f), overwrite = TRUE)
    message("  per_member/", f)
  }
}
rk <- file.path(SIM_DIR, "ranking_p104q10.csv")
if (file.exists(rk)) file.copy(rk, file.path(out_ref, basename(rk)), overwrite = TRUE)

## ---------------------------------------------------------------------------
## 4. README
## ---------------------------------------------------------------------------

PROV <- fm_provenance()
sc_line <- function(sc) if (!is.null(staged[[sc]]))
  sprintf("  - `%s/` — %d files\n", sc, length(staged[[sc]])) else ""

readme <- paste0(
"# Prydz Bay mizer — FishMIP ISIMIP3a submission bundle (phase-104 ensemble)

Outputs for the Prydz Bay mizer regional model, prepared for the FishMIP
ISIMIP3a protocol (<https://github.com/Fish-MIP/FishMIP2.0_ISIMIP3a>).

Built ", PROV$built, " by `R/fishmip_isimip3a/FM01`–`FM05`.

**This bundle supersedes `FishMIP_ISIMIP3a_submission/`**, which was built from
the retired 2,111-member Monte Carlo ensemble and divided by a domain area
13.23× too large. That tree is kept unchanged so the earlier submission stays
auditable; nothing in it should be uploaded.

## The ensemble

| | |
|---|---|
| ensemble | ", PROV$ensemble, " |
| members | ", PROV$n_ensemble_members, " |
| usable rule | ", PROV$usable_rule, " |
| reference model | `", PROV$base_params, "` |
| catchability | `", basename(PROV$catchability_refit), "`, re-fitted on the ISIMIP3a window ending 2004, ceiling q ≤ ", PROV$catchability_qmax, " |
| ranking | `", PROV$ranking_source, "` |
| simulations | `", PROV$simulations, "` |
| mizer / therMizer | ", PROV$mizer_version, " / ", PROV$thermizer_version, " |

Every value is an **ensemble quantile across the ", PROV$n_ensemble_members,
" members**: columns `median`, `q05`, `q25`, `q75`, `q95`. Take `median` as the
protocol value. Medians rather than means throughout — the across-member
distributions are heavy-tailed.

## Domain area

**`model_domain_area_m2` = ", format(MODEL_DOMAIN_AREA, scientific = TRUE),
" m² (1,474,341 km²)**

The Prydz Bay model domain: the study box 60–90°E, 80–60°S unioned with Atlantis
Box21 and differenced against land and ice shelf, measured in a Lambert
azimuthal equal-area projection centred on the region. The polygon is
`model_domains/Stacey/BanzareBank.rds`; `FM06_verify.R` recomputes its area and
asserts the match, so this figure is verified rather than quoted.

This is the area the model **is**. Observed densities in g m⁻² were multiplied
by it at model construction, so the state variable is grams of wet mass over
this polygon. Dividing by it is exactly invertible: it returns the density basis
the model was calibrated and fitted to, over the region the model represents.

Every quantity in the wider project — the manuscript, the calibration targets,
the consumption analyses — is on this same basis, so these outputs and the
paper agree exactly.

- to recover domain totals in grams: multiply by 1.474341e+12

## Variables

All densities are **g m⁻²**. Time is annual on the 365-day calendar, one value
per year at 1 January. In the full-span files `time` is **days since
1841-01-01** (0, 365, …, 61685). The period-split upload files use the
protocol's two reference dates; see Layout.

### Protocol variables

| variable | definition |
|---|---|
| `tcb` | Total Consumer Biomass Density. All 19 consumer functional groups over the **full** modelled size range (from 3.16 × 10⁻⁸ g). |
| `tcblog10` | The same biomass in the six log10 size classes — 1g-10g, 10g-100g, 100g-1kg, 1kg-10kg, 10kg-100kg, >100kg — i.e. **≥ 1 g only**. |
| `tc` | Total Catch Density, all groups, all sizes. |
| `tclog10` | The same catch in the six log10 size classes, ≥ 1 g only. |

`tcb` > `sum(tcblog10)` by construction, because `tcblog10` starts at 1 g and
`tcb` does not.

**What counts as a consumer.** All 19 modelled functional groups, invertebrates
included: mesozooplankton, other krill, other macrozooplankton, Antarctic krill,
salps, mesopelagic fishes, bathypelagic fishes, shelf and coastal fishes, flying
birds, small divers, squids, toothfishes, leopard seals, medium divers, large
divers, minke whales, orca, sperm whales, baleen whales. Every one has trophic
level > 1. The prescribed plankton **resource** is excluded: it is fitted to
`phypico` + `phydiat` + `phydiaz` (primary producers, TL = 1) together with
`zmicro` + `zmeso`, so including it would both add primary producers and
double-count the explicit mesozooplankton group.

### Fisheries-only catch — `histsoc_nowhales`

`tc` and `tclog10` recomputed with the four whale groups removed — **minke
whales, orca, sperm whales, baleen whales** — leaving Antarctic krill,
bathypelagic fishes, shelf and coastal fishes, squids and toothfishes.

This exists because FishMIP's catch reconstruction covers fisheries, not
whaling, and whaling accounts for **~96–98% of the catch mass here** (96.5% of
the modelled `tc`, 97.8% of the recorded catch). The full `tc` is therefore not
comparable with the FishMIP catch data; this variant is.

The variant is carried in the `sens` slot of the filename, leaving `var` equal
to the protocol variable name so an automated checker still matches it:

```
mizer_gfdl-mom6-cobalt2_obsclim_histsoc_nowhales_tc_prydz-bay_annual_1841_2010.csv
```

> **Confirm this token with the FishMIP regional coordinators before upload.**
> No slot in the ISIMIP3a vocabulary is a natural home for a catch subset; `sens`
> is the closest. Renaming it is a one-line change in `FM00_common.R`.

### Supporting variables (not protocol vocabulary)

| variable | definition |
|---|---|
| `bsp` | biomass density per functional group |
| `csp` | catch density per functional group |
| `catchobs` | **observed** catch density per group, from the historical record (IWC, CCAMLR, FAO). 1930–2010 recorded, 1841–1929 padded with zeros. Zero means no catch recorded, not missing. |
| `effort` | relative fishing effort per group, dimensionless 0–1 |

> `effort` is **model-internal relative effort**, not the protocol's `NomActive`
> (kW × days at sea). 1 is the maximum effort applied to any group in any year of
> the historical simulation. Converting to `NomActive` would need data this
> project does not hold.

## Scenarios

| directory | soc | sens | what |
|---|---|---|---|
| `histsoc` | histsoc | default | observed historical fishing and whaling effort |
| `nat` | nat | default | no fishing. Biomass only — catch is identically zero and was verified so, not assumed |
| `histsoc_nowhales` | histsoc | nowhales | the fisheries-only catch variant |

Climate forcing is GFDL-MOM6-COBALT2 `obsclim` throughout. The 1841–1960 spin-up
repeats the 1961–1980 forcing window six times (verified bit-for-bit). The
protocol names that repeated window `ctrlclim`; **no `ctrlclim` extraction exists
for this domain**, so the repeated window is built from the `obsclim` rows and
the files are named `obsclim` accordingly.

## Layout

- **split_csv/** — **the upload set.** Every CSV in `submission_csv/`, split into
  the protocol's two periods. Each folder holds all three scenarios and mirrors
  one DKRZ target, so it uploads as a single copy:

  | folder | covers | filenames end | `time` | DKRZ target |
  |---|---|---|---|---|
  | `temp2/` | 1841–1960 | `_annual_1841_1960.csv` | days since 1841-01-01, 0 … 43435 | `/work/bb0820/ISIMIP/ISIMIP3a/UploadArea/marine-fishery_regional/model_name/temp2` |
  | `temp/` | 1961–2010 | `_annual_1961_2010.csv` | days since 1901-01-01, 21900 … 39785 | `/work/bb0820/ISIMIP/ISIMIP3a/UploadArea/marine-fishery_regional/model_name/temp` |

  Values are identical to the full-span files. Only the filename years and, in
  `temp/`, the time reference change. `FM06_verify.R` checks both.

  > **Calendar.** The protocol text specifies a 365-day calendar, and that is
  > what these files use. The protocol's own `time_axis_spinup.csv` and
  > `time_axis_experment.csv` were generated with leap days, so they put
  > 1961-01-01 at 21915 days since 1901 rather than 21900. On this annual axis
  > the two differ by up to 27 days, reached in 2010. Mention this to the
  > coordinators when uploading.
- **submission_csv/** — the same CSVs as single full-span files, 1841–2010, one
  folder per scenario. **Not** in the protocol's two-file form; kept as the
  source of the split.
", sc_line("histsoc"), sc_line("nat"), sc_line("histsoc_nowhales"),
"- **optional_full/** — reference copies, not required for submission
  - `<scenario>/` — NetCDF bundles (`allvars`), which also carry every constant above as global attributes
  - `per_member/` — the per-member arrays behind the quantiles, keyed by the real `sim_index`, plus `ranking_p104q10.csv`
- **fishmip_outputs/**, **fishmip_outputs_species/**, **fishmip_outputs_ancillary/** — the generated originals the above are staged from
- **_work/** — intermediates

## Filename convention

```
<model>_<forcing>_<climate>_<soc>_<sens>_<var>_<region>_<timestep>_<start>_<end>.{csv,nc}
mizer_gfdl-mom6-cobalt2_obsclim_histsoc_default_tcb_prydz-bay_annual_1841_1960.csv   split_csv/temp2/
mizer_gfdl-mom6-cobalt2_obsclim_histsoc_default_tcb_prydz-bay_annual_1961_2010.csv   split_csv/temp/
mizer_gfdl-mom6-cobalt2_obsclim_histsoc_default_tcb_prydz-bay_annual_1841_2010.csv   submission_csv/histsoc/
```

`<start>_<end>` is the span the file actually covers. The spin-up files keep
`obsclim` in the climate slot: the protocol names every file after its
experiment, and the spin-up belongs to the `obsclim` experiment.

## Reproducing

```
Rscript run_p104q10.R R/fishmip_isimip3a/FM01_totals.R
Rscript run_p104q10.R R/fishmip_isimip3a/FM02_species.R
Rscript run_p104q10.R R/fishmip_isimip3a/FM03_ancillary.R
Rscript run_p104q10.R R/fishmip_isimip3a/FM04_netcdf.R      # needs ncdf4
Rscript run_p104q10.R R/fishmip_isimip3a/FM05_stage_bundle.R
Rscript run_p104q10.R R/fishmip_isimip3a/FM06_verify.R
```

`run_p104q10.R` is the single definition of the current ensemble; running these
any other way risks picking up a stale default.
")

writeLines(readme, file.path(FM_OUT_DIR, "README.md"))
message("  wrote ", file.path(FM_OUT_DIR, "README.md"))

cat("\n--- FM05 summary ---\n")
for (sc in names(staged))
  cat(sprintf("%-18s %2d CSVs staged and split\n", sc, length(staged[[sc]])))
cat(sprintf("NetCDF staged: %d\n", nc_found))
cat("\nFM05 done.\n")
