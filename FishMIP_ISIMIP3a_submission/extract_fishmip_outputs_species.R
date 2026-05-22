###############################################################################
# FishMIP ISIMIP3a Species-Level Output Extraction Script
#
# Produces species/functional-group level equivalents of the FishMIP standard
# tcb and tc variables, using pre-computed ensemble summary CSVs as a fast
# path (no ensemble loading required).
#
# Outputs (g m^-2, ensemble quantiles):
#   histsoc/
#     b_sp  : biomass density per species (fished ensemble)
#     c_sp  : catch density per species   (fished ensemble)
#   nat/
#     b_sp  : biomass density per species (climate-only / unfished ensemble)
#
# Input CSVs (in tonnes, pre-computed from 2111-simulation ensemble):
#   monte_carlo_2111_summaries/biomass_timeseries_summary_per_year_tonnes.csv
#   monte_carlo_2111_summaries/yield_timeseries_summary_per_year_tonnes.csv
#   climate_only_analysis/climate_only_biomass_timeseries_summary.csv
#
# Unit conversion:
#   g m^-2 = tonnes * 1e6 [g/t] / MODEL_DOMAIN_AREA [m^2]
#
# Time convention (FishMIP ISIMIP3a):
#   days since 1841-01-01 = (Year - 1841) * 365
#
# Author: Prydz Bay mizer project
###############################################################################

library(dplyr)

###############################################################################
# Configuration
###############################################################################

MODEL_DOMAIN_AREA <- 1.474341e+12  # m^2 (therMizer calibration domain, 05_therMizer_calibration_scale_model_domain.Rmd)

# FishMIP naming components
FISHMIP_MODEL    <- "mizer"
FISHMIP_FORCING  <- "gfdl-mom6-cobalt2"
FISHMIP_CLIMATE  <- "obsclim"
FISHMIP_SENS     <- "default"
FISHMIP_REGION   <- "prydz-bay"
FISHMIP_TIMESTEP <- "annual"
FISHMIP_START    <- "1841"
FISHMIP_END      <- "2010"

if (!exists("OUTPUT_DIR")) OUTPUT_DIR <- "fishmip_outputs_species"

# Input paths
BIOMASS_CSV      <- "monte_carlo_2111_summaries/biomass_timeseries_summary_per_year_tonnes.csv"
YIELD_CSV        <- "monte_carlo_2111_summaries/yield_timeseries_summary_per_year_tonnes.csv"
CLIM_BIOMASS_CSV <- "climate_only_analysis/climate_only_biomass_timeseries_summary.csv"

###############################################################################
# Helpers
###############################################################################

year_to_days <- function(year) (year - 1841L) * 365L

#' Convert a pre-computed summary CSV (tonnes) to g m^-2
#' Returns a data frame: time, species, median, q05, q25, q75, q95
tonnes_to_density <- function(df, area = MODEL_DOMAIN_AREA) {
  df %>%
    mutate(
      time   = year_to_days(Year),
      median = median_t * 1e6 / area,
      q05    = q05_t    * 1e6 / area,
      q25    = q25_t    * 1e6 / area,
      q75    = q75_t    * 1e6 / area,
      q95    = q95_t    * 1e6 / area
    ) %>%
    select(time, species = Species, median, q05, q25, q75, q95) %>%
    arrange(time, species)
}

fishmip_filename <- function(soc, var) {
  sprintf(
    "%s_%s_%s_%s_%s_%s_%s_%s_%s_%s.csv",
    FISHMIP_MODEL, FISHMIP_FORCING, FISHMIP_CLIMATE,
    soc, FISHMIP_SENS, var,
    FISHMIP_REGION, FISHMIP_TIMESTEP, FISHMIP_START, FISHMIP_END
  )
}

save_output <- function(df, soc, var) {
  dir <- file.path(OUTPUT_DIR, soc)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  path <- file.path(dir, fishmip_filename(soc, var))
  write.csv(df, path, row.names = FALSE)
  message(sprintf("  Saved: %s  (%d rows)", path, nrow(df)))
}

###############################################################################
# Load and convert
###############################################################################

cat("=============================================================\n")
cat("FishMIP Species-Level Output Extraction\n")
cat("=============================================================\n\n")

# --- histsoc: biomass (b_sp) ---
if (!file.exists(BIOMASS_CSV)) stop("Biomass CSV not found: ", BIOMASS_CSV)
cat("Loading fished biomass summary...\n")
b_raw <- read.csv(BIOMASS_CSV, stringsAsFactors = FALSE)
cat(sprintf("  %d rows, %d species, years %d-%d\n",
            nrow(b_raw), n_distinct(b_raw$Species),
            min(b_raw$Year), max(b_raw$Year)))

b_sp <- tonnes_to_density(b_raw)
save_output(b_sp, "histsoc", "bsp")

# --- histsoc: catch (c_sp) ---
if (!file.exists(YIELD_CSV)) stop("Yield CSV not found: ", YIELD_CSV)
cat("Loading fished yield (catch) summary...\n")
c_raw <- read.csv(YIELD_CSV, stringsAsFactors = FALSE)
cat(sprintf("  %d rows\n", nrow(c_raw)))

c_sp <- tonnes_to_density(c_raw)
save_output(c_sp, "histsoc", "csp")

# --- nat: biomass (b_sp) ---
if (!file.exists(CLIM_BIOMASS_CSV)) stop("Climate-only biomass CSV not found: ", CLIM_BIOMASS_CSV)
cat("Loading climate-only (unfished) biomass summary...\n")
clim_raw <- read.csv(CLIM_BIOMASS_CSV, stringsAsFactors = FALSE)
cat(sprintf("  %d rows (before trim)\n", nrow(clim_raw)))
# Trim to 1841-2010 to match fished ensemble and FishMIP protocol window
clim_raw <- clim_raw[clim_raw$Year <= 2010, ]
cat(sprintf("  %d rows (after trimming to 2010)\n", nrow(clim_raw)))

b_sp_nat <- tonnes_to_density(clim_raw)
save_output(b_sp_nat, "nat", "bsp")

###############################################################################
# Summary
###############################################################################

cat("\n=============================================================\n")
cat("Done.\n\n")
cat("Outputs written to: ", OUTPUT_DIR, "\n\n")
cat("Variables produced:\n")
cat("  histsoc/bsp  — species biomass density (g m^-2), fished ensemble\n")
cat("  histsoc/csp  — species catch density   (g m^-2), fished ensemble\n")
cat("  nat/bsp      — species biomass density (g m^-2), climate-only ensemble (trimmed to 2010)\n\n")
cat("Columns: time (days since 1841-01-01), species, median, q05, q25, q75, q95\n")
cat("Units  : g m^-2  (= input tonnes * 1e6 / 1.474341e12 m^2)\n")
cat("=============================================================\n")
