###############################################################################
# FM04_netcdf.R -- bundle the FM01/FM02 outputs into NetCDF.
#
# Run:  Rscript run_p104q10.R R/fishmip_isimip3a/FM04_netcdf.R
#       (after FM01_totals.R and FM02_species.R)
#
# Writes one file per scenario, holding every variable that scenario has:
#
#   <...>_histsoc_default_allvars_<...>.nc    tcb, tcblog10, tc, tclog10, bsp, csp
#   <...>_nat_default_allvars_<...>.nc        tcb, tcblog10, bsp
#   <...>_histsoc_nowhales_allvars_<...>.nc   tc, tclog10
#
# CSV is what is actually uploaded to DKRZ; these are the reference copies that
# go under optional_full/. Structure follows extract_fishmip_outputs.R:520-586,
# with two additions: the per-group variables gain a `species` dimension, and
# the global attributes now carry the full phase-104 provenance and BOTH domain
# areas, so a reader can undo the area choice without reading this repo.
#
# Dimensions
#   time        days since 1841-1-1 00:00:00, 365 d/yr (protocol convention)
#   size_class  1..6  -> see the size_class_names global attribute
#   species     1..19 -> see the species_names global attribute, model order
#   statistic   1..5  -> 1=median, 2=q05, 3=q25, 4=q75, 5=q95
#
# The `statistic` dimension is not part of the protocol variable definition; it
# carries the ensemble spread, which a single-member submission would not have.
# Take statistic 1 (median) for the protocol value.
###############################################################################

source("R/fishmip_isimip3a/FM00_common.R")

if (!requireNamespace("ncdf4", quietly = TRUE))
  stop("the ncdf4 package is required for FM04 and is not installed.\n",
       "  install.packages(\"ncdf4\")\n",
       "Every other FM step runs without it; the CSVs are the upload set.",
       call. = FALSE)
library(ncdf4)

f1 <- file.path(FM_WORK, "FM01_totals.rds")
f2 <- file.path(FM_WORK, "FM02_species.rds")
for (f in c(f1, f2))
  if (!file.exists(f))
    stop("missing ", f, " -- run FM01_totals.R and FM02_species.R first",
         call. = FALSE)

T1 <- readRDS(f1)
T2 <- readRDS(f2)

stopifnot(identical(T1$times, T2$times), identical(T1$species, T2$species),
          identical(T1$members, T2$members))

TIMES   <- T1$times
SPECIES <- T1$species
PROV    <- fm_provenance()
time_days <- year_to_days(TIMES)

time_dim <- ncdim_def("time", "days since 1841-1-1 00:00:00", time_days, unlim = FALSE)
size_dim <- ncdim_def("size_class", "log10_g", seq_along(FISHMIP_BIN_NAMES), unlim = FALSE)
sp_dim   <- ncdim_def("species", "", seq_along(SPECIES), unlim = FALSE)
stat_dim <- ncdim_def("statistic", "", seq_along(FISHMIP_STATS), unlim = FALSE)

vdef <- function(name, dims, longname)
  ncvar_def(name, "g m-2", dims, missval = FISHMIP_MISSING,
            longname = longname, prec = "float")

LONGNAME <- c(
  tcb      = "Total Consumer Biomass Density",
  tcblog10 = "Total Consumer Biomass Density in log10 Weight Bins",
  tc       = "Total Catch Density",
  tclog10  = "Total Catch Density in log10 Weight Bins",
  bsp      = "Biomass Density by Functional Group",
  csp      = "Catch Density by Functional Group"
)

#' Write one scenario file.
#' @param vars named list: name -> the stats array, dims inferred from its shape
write_nc <- function(soc, sens, vars) {
  defs <- list(); data <- list()
  for (nm in names(vars)) {
    a <- vars[[nm]]
    dims <- switch(as.character(length(dim(a))),
                   "2" = list(time_dim, stat_dim),
                   "3" = if (dim(a)[2] == length(FISHMIP_BIN_NAMES))
                           list(time_dim, size_dim, stat_dim)
                         else list(time_dim, sp_dim, stat_dim),
                   stop("unexpected shape for ", nm, call. = FALSE))
    defs[[nm]] <- vdef(nm, dims, unname(LONGNAME[nm]))
    data[[nm]] <- a
  }

  d <- fm_dir(FM_DIR_TOTALS, if (identical(sens, FISHMIP_SENS)) soc
                             else paste0(soc, "_", sens))
  path <- file.path(d, fishmip_filename(soc, "allvars", sens, ext = "nc"))
  if (file.exists(path)) unlink(path)
  nc <- nc_create(path, defs)
  for (nm in names(defs)) ncvar_put(nc, defs[[nm]], data[[nm]])

  ncatt_put(nc, 0, "title", "Prydz Bay mizer FishMIP ISIMIP3a outputs")
  ncatt_put(nc, 0, "institution", "University of Tasmania")
  ncatt_put(nc, 0, "source", "mizer size-spectrum model, phase-104 Monte Carlo ensemble")
  ncatt_put(nc, 0, "contact", "kieran.murphy137@gmail.com")
  ncatt_put(nc, 0, "scenario_soc", soc)
  ncatt_put(nc, 0, "scenario_sens", sens)
  if (identical(sens, FISHMIP_SENS_NOWHALES))
    ncatt_put(nc, 0, "sens_note",
              paste("Catch excluding the whale groups (",
                    paste(WHALE_GROUPS, collapse = ", "),
                    "). FishMIP's catch reconstruction covers fisheries, not",
                    "whaling; the whale groups dominate the modelled catch mass."))
  ncatt_put(nc, 0, "model_domain_area_m2", MODEL_DOMAIN_AREA)
  ncatt_put(nc, 0, "model_domain_polygon", PROV$domain_polygon)
  ncatt_put(nc, 0, "area_note", AREA_NOTE)
  ncatt_put(nc, 0, "ensemble", PROV$ensemble)
  ncatt_put(nc, 0, "n_ensemble_members", PROV$n_ensemble_members)
  ncatt_put(nc, 0, "usable_rule", PROV$usable_rule)
  ncatt_put(nc, 0, "base_params", PROV$base_params)
  ncatt_put(nc, 0, "catchability_refit", PROV$catchability_refit)
  ncatt_put(nc, 0, "catchability_qmax", PROV$catchability_qmax)
  ncatt_put(nc, 0, "ranking_source", PROV$ranking_source)
  ncatt_put(nc, 0, "member_simulations", PROV$simulations)
  ncatt_put(nc, 0, "mizer_version", PROV$mizer_version)
  ncatt_put(nc, 0, "thermizer_version", PROV$thermizer_version)
  ncatt_put(nc, 0, "size_class_names", paste(FISHMIP_BIN_NAMES, collapse = ", "))
  ncatt_put(nc, 0, "size_class_boundaries_g", paste(FISHMIP_BINS, collapse = ", "))
  ncatt_put(nc, 0, "species_names", paste(SPECIES, collapse = ", "))
  ncatt_put(nc, 0, "statistics", "1=median, 2=q05, 3=q25, 4=q75, 5=q95")
  ncatt_put(nc, 0, "tcb_definition",
            paste("All 19 consumer functional groups over the full modelled size",
                  "range; the prescribed plankton resource is excluded."))
  ncatt_put(nc, 0, "creation_date", as.character(Sys.time()))
  nc_close(nc)

  message(sprintf("  wrote %s  (%s)", path, paste(names(vars), collapse = ", ")))
  invisible(path)
}

message("writing NetCDF...")

write_nc("histsoc", FISHMIP_SENS, list(
  tcb      = T1$stats$histsoc$tcb,
  tcblog10 = T1$stats$histsoc$tcblog10,
  tc       = T1$stats$histsoc$tc,
  tclog10  = T1$stats$histsoc$tclog10,
  bsp      = T2$stats$bsp_histsoc,
  csp      = T2$stats$csp_histsoc))

write_nc("nat", FISHMIP_SENS, list(
  tcb      = T1$stats$nat$tcb,
  tcblog10 = T1$stats$nat$tcblog10,
  bsp      = T2$stats$bsp_nat))

write_nc("histsoc", FISHMIP_SENS_NOWHALES, list(
  tc      = T1$stats$histsoc$tc_nowhales,
  tclog10 = T1$stats$histsoc$tclog10_nowhales))

cat("\nFM04 done.\n")
