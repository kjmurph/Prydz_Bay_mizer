###############################################################################
# Prepare ancillary FishMIP outputs for Prydz Bay regional model
#
# Produces two supplementary files to accompany the species-level bsp/csp
# outputs, in the same time/species long format:
#
#   catchobs : Observed catch by species (g m^-2)
#              Derived from historical catch records (IWC, CCAMLR, FAO).
#              Covers 1930-2019 (years with available records, including
#              zero-catch years). All 19 model functional groups included;
#              species with no historical catch record have catch = 0.
#              Unit convention: g m^-2 (total catch mass per unit area for
#              each annual time step), consistent with FishMIP protocol
#              Table 9 catch variable conventions.
#
#   effort   : Model fishing effort by species (dimensionless, 0-1)
#              This is the relative fishing effort used as input to the
#              mizer model, NOT the FishMIP protocol NomActive effort
#              (kW x days at sea). Values are scaled 0-1 where 1 represents
#              the maximum effort applied to any species in any year of the
#              historical simulation. Covers 1841-2010. All 19 functional
#              groups included.
#
# Output folder: fishmip_outputs_ancillary/histsoc/
# File naming follows the same convention as bsp/csp files.
#
# Author: Prydz Bay mizer project
###############################################################################

library(dplyr)
library(tidyr)

MODEL_DOMAIN_AREA <- 1.474341e+12  # m^2 (therMizer calibration domain, 05_therMizer_calibration_scale_model_domain.Rmd)

# FishMIP naming components (same as extract_fishmip_outputs_species.R)
FISHMIP_MODEL    <- "mizer"
FISHMIP_FORCING  <- "gfdl-mom6-cobalt2"
FISHMIP_CLIMATE  <- "obsclim"
FISHMIP_SOC      <- "histsoc"
FISHMIP_SENS     <- "default"
FISHMIP_REGION   <- "prydz-bay"
FISHMIP_TIMESTEP <- "annual"

if (!exists("OUTPUT_DIR")) OUTPUT_DIR <- file.path("FishMIP_ISIMIP3a_submission", "fishmip_outputs_ancillary", "histsoc")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

year_to_days <- function(year) (as.integer(year) - 1841L) * 365L

fishmip_filename <- function(var, start_yr, end_yr) {
  file.path(OUTPUT_DIR, sprintf(
    "%s_%s_%s_%s_%s_%s_%s_%s_%s_%s.csv",
    FISHMIP_MODEL, FISHMIP_FORCING, FISHMIP_CLIMATE,
    FISHMIP_SOC, FISHMIP_SENS, var,
    FISHMIP_REGION, FISHMIP_TIMESTEP,
    start_yr, end_yr
  ))
}

###############################################################################
# 1. Observed catch by species (catchobs)
###############################################################################
message("Preparing observed catch (catchobs)...")

obs_raw <- read.csv("yield_observed_timeseries.csv", stringsAsFactors = FALSE)

# Fix dotted column names back to spaces
names(obs_raw) <- gsub("\\.", " ", names(obs_raw))

year_range <- range(obs_raw$Year)
message(sprintf("  Source: yield_observed_timeseries.csv | years %d-%d | %d rows",
                year_range[1], year_range[2], nrow(obs_raw)))

# Pivot to long format, convert grams -> g m^-2, build time column
catchobs_observed <- obs_raw %>%
  filter(Year <= 2010L) %>%
  pivot_longer(
    cols      = -Year,
    names_to  = "species",
    values_to = "catch_g"
  ) %>%
  mutate(
    time  = year_to_days(Year),
    catch = catch_g / MODEL_DOMAIN_AREA   # total g yr^-1 -> g m^-2
  ) %>%
  select(time, species, catch)

# Prepend zero-catch rows for 1841-1929 (pre-observation era) so the time
# dimension is consistent with the bsp/csp outputs (which start at 1841)
pre_obs_years <- 1841L:(year_range[1] - 1L)
all_species   <- unique(catchobs_observed$species)
pre_obs <- expand.grid(Year = pre_obs_years, species = all_species,
                       stringsAsFactors = FALSE)
pre_obs$time  <- year_to_days(pre_obs$Year)
pre_obs$catch <- 0
pre_obs <- pre_obs[, c("time", "species", "catch")]

catchobs <- bind_rows(pre_obs, catchobs_observed) %>%
  arrange(time, species)

full_year_range <- c(1841L, 2010L)
message(sprintf("  Pre-observation zeros added for 1841-%d", year_range[1] - 1L))

out_path_catch <- fishmip_filename("catchobs", full_year_range[1], full_year_range[2])
write.csv(catchobs, out_path_catch, row.names = FALSE)
message(sprintf("  Saved: %s  (%d rows)", out_path_catch, nrow(catchobs)))
message(sprintf("  Species: %d | Years: %d-%d | catch range: %.3e - %.3e g m^-2",
                length(unique(catchobs$species)),
                full_year_range[1], full_year_range[2],
                min(catchobs$catch), max(catchobs$catch)))

###############################################################################
# 2. Model fishing effort by species (effort)
###############################################################################
message("Preparing model fishing effort (effort)...")

effort_matrix <- readRDS("effort_array_1841_2010.rds")

# effort_matrix: rows = years, cols = species
effort_years <- as.integer(rownames(effort_matrix))
message(sprintf("  Source: effort_array_1841_2010.rds | years %d-%d | %d species",
                min(effort_years), max(effort_years), ncol(effort_matrix)))
message(sprintf("  Effort value range: %.4f - %.4f (dimensionless, 0-1 relative scale)",
                min(effort_matrix), max(effort_matrix)))

# Convert matrix to long data frame
effort_long <- as.data.frame(effort_matrix) %>%
  mutate(Year = effort_years) %>%
  pivot_longer(
    cols      = -Year,
    names_to  = "species",
    values_to = "effort"
  ) %>%
  mutate(time = year_to_days(Year)) %>%
  select(time, species, effort) %>%
  arrange(time, species)

out_path_effort <- fishmip_filename("effort",
                                    min(effort_years), max(effort_years))
write.csv(effort_long, out_path_effort, row.names = FALSE)
message(sprintf("  Saved: %s  (%d rows)", out_path_effort, nrow(effort_long)))
message(sprintf("  Species with non-zero effort: %s",
                paste(
                  unique(effort_long$species[effort_long$effort > 0]),
                  collapse = ", "
                )))

###############################################################################
# Summary
###############################################################################
message("\n=== Done ===")
message("catchobs : observed catch density (g m^-2) — same unit as FishMIP tc/csp")
message("           time convention: days since 1841-01-01 (= (Year-1841)*365)")
message("           zero = no observed catch in that year (not missing)")
message("effort   : relative fishing effort (dimensionless, 0-1)")
message("           0 = no fishing; 1 = maximum effort in the historical record")
message("           NOTE: this is model-internal relative effort, NOT FishMIP")
message("           protocol NomActive (kW x days at sea)")
