###############################################################################
# FM03_ancillary.R -- observed catch (catchobs) and model fishing effort
#                     (effort), in the same long format as bsp/csp.
#
# Run:  Rscript run_p104q10.R R/fishmip_isimip3a/FM03_ancillary.R
#
#   catchobs  Observed catch by group (g m-2), from the historical record
#             (IWC, CCAMLR, FAO) via `yield_observed_timeseries.csv`. Records
#             run 1930-2019; rows are trimmed to 2010 and 1841-1929 is padded
#             with zeros so the time axis matches bsp/csp. All 19 groups
#             appear; a group with no catch record carries 0, which is a real
#             zero, not a missing value.
#
#   catchobs under sens = "nowhales"
#             The same series with minke whales, orca, sperm whales and baleen
#             whales zeroed -- the observational companion to FM01's
#             fisheries-only `tc`, so the two can be compared directly.
#
#   effort    Relative fishing effort by group (dimensionless, 0-1), the array
#             used as input to the mizer runs. THIS IS NOT the FishMIP protocol
#             NomActive effort (kW x days at sea): it is model-internal
#             relative effort, where 1 is the maximum applied to any group in
#             any year of the historical simulation. Converting to NomActive
#             would need data the project does not hold. This caveat was
#             carried by the previous submission and is carried here unchanged.
#
# Both are ENSEMBLE-INDEPENDENT -- they are model input and observation, not
# model output, so the only thing the phase-104 rebuild changes about them is
# the domain area in the denominator.
#
# Differs from prepare_fishmip_ancillary_outputs.R:62-145 in two respects:
# the area, and the species-name handling. That script read with the default
# check.names = TRUE and then repaired names with gsub("\\.", " ", ...), which
# happens to work only because no group name contains a dot. Here the file is
# read with check.names = FALSE and the columns are asserted to be exactly the
# 19 model groups in model order.
###############################################################################

source("R/fishmip_isimip3a/FM00_common.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

SPECIES <- fm_species()
message("model groups: ", length(SPECIES))

## ---------------------------------------------------------------------------
## 1. Observed catch (catchobs)
## ---------------------------------------------------------------------------

OBS_CSV <- "yield_observed_timeseries.csv"
if (!file.exists(OBS_CSV)) stop("not found: ", OBS_CSV, call. = FALSE)

obs_raw <- read.csv(OBS_CSV, stringsAsFactors = FALSE, check.names = FALSE)

cols <- setdiff(names(obs_raw), "Year")
if (!identical(cols, SPECIES))
  stop("yield_observed_timeseries.csv columns do not match the model groups.\n",
       "  only in csv:   ", paste(setdiff(cols, SPECIES), collapse = ", "), "\n",
       "  only in model: ", paste(setdiff(SPECIES, cols), collapse = ", "),
       call. = FALSE)
message("check: observed-catch columns are the 19 model groups, in model order -- ok")

obs_years <- range(obs_raw$Year)
message(sprintf("  source: %s | years %d-%d | %d rows",
                OBS_CSV, obs_years[1], obs_years[2], nrow(obs_raw)))

## grams over the domain per year -> g m-2
catchobs_recorded <- obs_raw %>%
  filter(Year <= 2010L) %>%
  pivot_longer(cols = -Year, names_to = "species", values_to = "catch_g") %>%
  mutate(time = year_to_days(Year), catch = catch_g / MODEL_DOMAIN_AREA) %>%
  select(time, species, catch)

## pad 1841 to the first record year with zeros, so the time axis matches bsp/csp
pre_years <- 1841L:(as.integer(obs_years[1]) - 1L)
pre <- expand.grid(Year = pre_years, species = SPECIES,
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
pre$time <- year_to_days(pre$Year)
pre$catch <- 0
pre <- pre[, c("time", "species", "catch")]
message(sprintf("  padded 1841-%d with zeros", obs_years[1] - 1L))

catchobs <- bind_rows(pre, catchobs_recorded) %>%
  mutate(species = factor(species, levels = SPECIES)) %>%
  arrange(time, species) %>%
  mutate(species = as.character(species))

stopifnot(nrow(catchobs) == 170L * length(SPECIES),
          identical(sort(unique(catchobs$time)), year_to_days(1841:2010)))

fm_write_csv(catchobs, FM_DIR_ANCILLARY, "histsoc", "catchobs")

## fisheries-only companion: the whale groups zeroed, rows kept so the shape
## matches the full file exactly
catchobs_nw <- catchobs
catchobs_nw$catch[catchobs_nw$species %in% WHALE_GROUPS] <- 0
fm_write_csv(catchobs_nw, FM_DIR_ANCILLARY, "histsoc", "catchobs",
             sens = FISHMIP_SENS_NOWHALES)

whale_share <- 1 - sum(catchobs_nw$catch) / sum(catchobs$catch)
message(sprintf("  whaling share of recorded catch mass, 1841-2010: %.2f%%",
                100 * whale_share))

## ---------------------------------------------------------------------------
## 2. Model fishing effort (effort)
## ---------------------------------------------------------------------------

EFFORT_RDS <- "effort_array_1841_2010.rds"
if (!file.exists(EFFORT_RDS)) stop("not found: ", EFFORT_RDS, call. = FALSE)

effort_matrix <- readRDS(EFFORT_RDS)
effort_years <- as.integer(rownames(effort_matrix))

if (!identical(colnames(effort_matrix), SPECIES))
  stop("effort array columns do not match the model groups", call. = FALSE)
if (!identical(effort_years, 1841:2010))
  stop("effort array years are not 1841-2010", call. = FALSE)
message(sprintf("  source: %s | years %d-%d | %d groups | range %.4f-%.4f",
                EFFORT_RDS, min(effort_years), max(effort_years),
                ncol(effort_matrix), min(effort_matrix), max(effort_matrix)))

effort_long <- as.data.frame(effort_matrix, check.names = FALSE) %>%
  mutate(Year = effort_years) %>%
  pivot_longer(cols = -Year, names_to = "species", values_to = "effort") %>%
  mutate(time = year_to_days(Year),
         species = factor(species, levels = SPECIES)) %>%
  arrange(time, species) %>%
  mutate(species = as.character(species)) %>%
  select(time, species, effort)

fm_write_csv(effort_long, FM_DIR_ANCILLARY, "histsoc", "effort")

message("  groups with non-zero effort: ",
        paste(unique(effort_long$species[effort_long$effort > 0]), collapse = ", "))

## ---------------------------------------------------------------------------

fm_dir(FM_WORK)
saveRDS(list(catchobs = catchobs, catchobs_nowhales = catchobs_nw,
             effort = effort_long, species = SPECIES,
             whale_share_of_recorded_catch = whale_share,
             provenance = fm_provenance()),
        file.path(FM_WORK, "FM03_ancillary.rds"))
message("  wrote ", file.path(FM_WORK, "FM03_ancillary.rds"))

cat("\n--- FM03 summary ---\n")
cat("catchobs : observed catch density (g m-2), same unit as tc / csp\n")
cat("           time convention: days since 1841-01-01 (= (Year - 1841) * 365)\n")
cat("           zero = no catch recorded in that year, not missing\n")
cat("effort   : relative fishing effort (dimensionless, 0-1)\n")
cat("           0 = no fishing; 1 = maximum effort in the historical record\n")
cat("           NOTE: model-internal relative effort, NOT FishMIP protocol\n")
cat("           NomActive (kW x days at sea)\n")
cat(sprintf("\nrecorded catch totals over 1841-2010: all groups %.6e g m-2, ",
            sum(catchobs$catch)))
cat(sprintf("fisheries only %.6e g m-2\n", sum(catchobs_nw$catch)))
cat("\nFM03 done.\n")
