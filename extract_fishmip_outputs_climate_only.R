###############################################################################
# FishMIP ISIMIP3a Output Extraction Script - CLIMATE-ONLY ENSEMBLE
# 
# Extracts size-structured biomass and catch outputs from climate-only Monte Carlo ensemble 
# simulations and formats them according to FishMIP ISIMIP3a protocol requirements.
#
# This script processes the climate-only (unfished) ensemble to enable comparison
# with the fished ensemble. Each simulation matches its fished counterpart exactly
# except for fishing effort = 0.
#
# Outputs:
#   - tcblog10: Total Consumer Biomass Density in log10 Weight Bins (g m^-2)
#               Only includes sizes >= 1g (FishMIP bins: 1g-10g through >100kg)
#   - tcb: Total Consumer Biomass Density (g m^-2)
#          Includes ALL sizes (including organisms < 1g)
#   - tclog10: Total Catch Density in log10 Weight Bins (g m^-2) [All zeros - no fishing]
#   - tc: Total Catch Density (g m^-2) [All zeros - no fishing]
#   - Plus ensemble statistics (median, 5%, 25%, 75%, 95% quantiles)
#
# Author: Generated for Prydz Bay mizer project
# Date: 2025
###############################################################################

library(mizer)
library(dplyr)
library(tidyr)
library(reshape2)

# Optional: NetCDF support
if (!requireNamespace("ncdf4", quietly = TRUE)) {
  message("ncdf4 package not installed - will only save CSV outputs")
  has_ncdf4 <- FALSE
} else {
  library(ncdf4)
  has_ncdf4 <- TRUE
}

###############################################################################
# Configuration
###############################################################################

# Model domain area in m^2 (from 02_Preparing_Climate_Forcings.Rmd)
MODEL_DOMAIN_AREA <- 1.95e+13  # m^2

# FishMIP log10 weight class boundaries (in grams)
# Classes: 1-10g, 10-100g, 100g-1kg, 1-10kg, 10-100kg, >100kg
FISHMIP_BINS <- c(1, 10, 100, 1000, 10000, 100000, Inf)  # grams
FISHMIP_BIN_LOG10 <- log10(FISHMIP_BINS[-length(FISHMIP_BINS)])  # 0, 1, 2, 3, 4, 5
FISHMIP_BIN_NAMES <- c("1g-10g", "10g-100g", "100g-1kg", "1kg-10kg", "10kg-100kg", ">100kg")

# FishMIP missing value
FISHMIP_MISSING <- 1.0e+20

# Input/output paths - CLIMATE-ONLY ENSEMBLE
MC_RESULTS_FILE <- "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
OUTPUT_DIR <- "fishmip_outputs_climate_only"

# FishMIP naming convention components (climate-only = nat scenario)
FISHMIP_MODEL <- "mizer"
FISHMIP_FORCING <- "gfdl-mom6-cobalt2"
FISHMIP_CLIMATE <- "obsclim"
FISHMIP_SOC <- "nat"  # natural/no-fishing scenario
FISHMIP_SENS <- "default"
FISHMIP_REGION <- "prydz-bay"
FISHMIP_TIMESTEP <- "annual"
FISHMIP_START_YEAR <- "1841"
FISHMIP_END_YEAR <- "2010"

###############################################################################
# Helper Functions
###############################################################################

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

#' Extract FishMIP outputs from a single simulation
#' 
#' @param sim A MizerSim object
#' @return List with tcblog10 (time x 6 size classes), tcb (time vector),
#'         tclog10 (time x 6 size classes), tc (time vector)
extract_fishmip_from_sim <- function(sim) {
  # Get weight bins and widths
  w_vec <- w(sim@params)
  dw_vec <- sim@params@dw
  
  # Get N array (time x species x weight bins)
  n_array <- sim@n
  
  # Trim to 170 time points if needed (to match fished ensemble: 1841-2010)
  # project() with t_max=170 from t_start=1841 gives 171 points (includes both endpoints)
  # We want 1841-2010 (170 years), so keep first 170 points
  if (dim(n_array)[1] == 171) {
    n_array <- n_array[1:170, , ]
  }
  
  times <- as.numeric(dimnames(n_array)$time)
  n_times <- length(times)
  n_species <- dim(n_array)[2]
  n_w <- length(w_vec)
  
  # Calculate biomass
  biomass_array <- calc_biomass_from_n(n_array, w_vec, dw_vec)
  
  # tcb: total consumer biomass (sum across ALL weight bins and species)
  tcb <- apply(biomass_array, 1, sum)
  
  # Assign mizer bins to FishMIP classes
  bin_assignments <- assign_fishmip_bins(w_vec)
  
  # Aggregate to FishMIP size classes
  biomass_by_sizeclass <- aggregate_to_fishmip_bins(biomass_array, bin_assignments, n_bins = 6)
  
  # Sum across species to get total consumer biomass in each size class
  tcblog10 <- apply(biomass_by_sizeclass, c(1, 3), sum)
  
  # Convert to density (g m^-2)
  tcblog10_density <- tcblog10 / MODEL_DOMAIN_AREA
  tcb_density <- tcb / MODEL_DOMAIN_AREA
  
  # =========================================================================
  # Catch outputs (all zeros for climate-only/unfished simulations)
  # =========================================================================
  # No fishing in climate-only simulations, so all catch values are zero
  # We still create these arrays for consistency with fished ensemble format
  
  tclog10_density <- matrix(0, nrow = n_times, ncol = 6)  # time x 6 size classes, all zeros
  tc_density <- rep(0, n_times)  # time vector, all zeros
  
  return(list(
    times = times,
    tcblog10 = tcblog10_density,  # time x 6 size classes
    tcb = tcb_density,             # time vector
    tclog10 = tclog10_density,    # time x 6 size classes (all zeros)
    tc = tc_density                # time vector (all zeros)
  ))
}

###############################################################################
# Main Processing
###############################################################################

cat("=============================================================\n")
cat("FishMIP ISIMIP3a Output Extraction - CLIMATE-ONLY ENSEMBLE\n")
cat("=============================================================\n\n")

# Create output directory
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# Load Monte Carlo results (climate-only compiled ensemble)
cat("Loading climate-only Monte Carlo results...\n")
mc <- readRDS(MC_RESULTS_FILE)
n_sims <- length(mc$simulations)
cat(sprintf("  Loaded %d simulations\n", n_sims))

# Get time dimension from first simulation
sim1 <- mc$simulations[[1]]
times_raw <- as.numeric(dimnames(sim1@n)$time)
n_times_raw <- length(times_raw)
cat(sprintf("  Time range: %d - %d (%d time steps)\n", min(times_raw), max(times_raw), n_times_raw))

# We'll trim to 170 to match fished ensemble (1841-2010)
n_times <- 170
times <- times_raw[1:n_times]  # Use only first 170 years

# Get species names
species_names <- dimnames(sim1@n)$sp
n_species <- length(species_names)
cat(sprintf("  Number of species: %d\n", n_species))

# Check weight bins
w_vec <- w(sim1@params)
cat(sprintf("  Weight bins: %d bins (log10 range: %.2f to %.2f)\n", 
            length(w_vec), min(log10(w_vec)), max(log10(w_vec))))

# Show FishMIP bin assignments
bin_assignments <- assign_fishmip_bins(w_vec)
cat("\n  FishMIP size class bin assignments:\n")
for (b in 1:6) {
  bin_idx <- which(bin_assignments == b)
  if (length(bin_idx) > 0) {
    cat(sprintf("    %s: mizer bins %d-%d (log10 w: %.2f to %.2f)\n",
                FISHMIP_BIN_NAMES[b], min(bin_idx), max(bin_idx),
                log10(w_vec[min(bin_idx)]), log10(w_vec[max(bin_idx)])))
  }
}

# Initialize arrays to store all simulation results (using 170 time points)
cat("\nProcessing simulations...\n")
all_tcblog10 <- array(NA, dim = c(n_sims, n_times, 6))
all_tcb <- matrix(NA, nrow = n_sims, ncol = n_times)
# Note: No catch outputs for climate-only (no fishing)
failed_sims <- c()

# Process each simulation
pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
for (i in 1:n_sims) {
  sim <- mc$simulations[[i]]
  result <- tryCatch({
    extract_fishmip_from_sim(sim)
  }, error = function(e) {
    if (i <= 3) {  # Print first 3 errors for debugging
      cat("\n  Error in sim", i, ":", conditionMessage(e), "\n")
    }
    NULL
  })
  
  if (!is.null(result)) {
    all_tcblog10[i, , ] <- result$tcblog10
    all_tcb[i, ] <- result$tcb
    # No catch extraction needed (climate-only has no fishing)
  } else {
    failed_sims <- c(failed_sims, i)
  }
  
  setTxtProgressBar(pb, i)
}
close(pb)

if (length(failed_sims) > 0) {
  cat(sprintf("\n  Warning: %d simulations failed to process and were skipped\n", length(failed_sims)))
  cat(sprintf("  (Sim IDs: %s)\n", paste(head(failed_sims, 10), collapse = ", ")))
}

cat("\n\nCalculating ensemble statistics...\n")

# Calculate ensemble statistics for tcblog10
tcblog10_stats <- array(0, dim = c(n_times, 6, 5))
dimnames(tcblog10_stats) <- list(
  time = as.character(times),
  size_class = FISHMIP_BIN_NAMES,
  stat = c("median", "q05", "q25", "q75", "q95")
)

for (t in 1:n_times) {
  for (b in 1:6) {
    vals <- all_tcblog10[, t, b]
    tcblog10_stats[t, b, 1] <- median(vals, na.rm = TRUE)
    tcblog10_stats[t, b, 2] <- quantile(vals, 0.05, na.rm = TRUE)
    tcblog10_stats[t, b, 3] <- quantile(vals, 0.25, na.rm = TRUE)
    tcblog10_stats[t, b, 4] <- quantile(vals, 0.75, na.rm = TRUE)
    tcblog10_stats[t, b, 5] <- quantile(vals, 0.95, na.rm = TRUE)
  }
}

# Calculate ensemble statistics for tcb
tcb_stats <- matrix(0, nrow = n_times, ncol = 5)
colnames(tcb_stats) <- c("median", "q05", "q25", "q75", "q95")
rownames(tcb_stats) <- as.character(times)

for (t in 1:n_times) {
  vals <- all_tcb[, t]
  tcb_stats[t, 1] <- median(vals, na.rm = TRUE)
  tcb_stats[t, 2] <- quantile(vals, 0.05, na.rm = TRUE)
  tcb_stats[t, 3] <- quantile(vals, 0.25, na.rm = TRUE)
  tcb_stats[t, 4] <- quantile(vals, 0.75, na.rm = TRUE)
  tcb_stats[t, 5] <- quantile(vals, 0.95, na.rm = TRUE)
}

# Note: No catch statistics for climate-only (no fishing)

###############################################################################
# Save CSV Outputs
###############################################################################

cat("Saving CSV outputs...\n")

# Convert years to days since 1841-1-1 (FishMIP ISIMIP3a protocol)
# Our data spans 1841-2010 (includes spin-up and transition period)
# Format: "days since 1841-1-1 00:00:00"
convert_year_to_days <- function(year) {
  # Each year = 365 days (assuming no leap years for simplicity, consistent with model)
  (year - 1841) * 365
}

time_days <- convert_year_to_days(times)

# tcblog10 - reshape to long format for CSV
tcblog10_df <- expand.grid(
  time = time_days,
  size_class = FISHMIP_BIN_NAMES
)
tcblog10_df$median <- as.vector(tcblog10_stats[, , "median"])
tcblog10_df$q05 <- as.vector(tcblog10_stats[, , "q05"])
tcblog10_df$q25 <- as.vector(tcblog10_stats[, , "q25"])
tcblog10_df$q75 <- as.vector(tcblog10_stats[, , "q75"])
tcblog10_df$q95 <- as.vector(tcblog10_stats[, , "q95"])

tcblog10_df$size_class <- factor(tcblog10_df$size_class, levels = FISHMIP_BIN_NAMES)
tcblog10_df <- tcblog10_df[order(tcblog10_df$time, tcblog10_df$size_class), ]

filename <- sprintf("%s_%s_%s_%s_%s_tcblog10_%s_%s_%s_%s.csv",
                   FISHMIP_MODEL, FISHMIP_FORCING, FISHMIP_CLIMATE, FISHMIP_SOC, FISHMIP_SENS,
                   FISHMIP_REGION, FISHMIP_TIMESTEP, FISHMIP_START_YEAR, FISHMIP_END_YEAR)
write.csv(tcblog10_df, file.path(OUTPUT_DIR, filename), row.names = FALSE)
cat(sprintf("  Saved: %s\n", file.path(OUTPUT_DIR, filename)))

# tcb - simple data frame
tcb_df <- data.frame(
  time = time_days,
  median = tcb_stats[, "median"],
  q05 = tcb_stats[, "q05"],
  q25 = tcb_stats[, "q25"],
  q75 = tcb_stats[, "q75"],
  q95 = tcb_stats[, "q95"]
)

filename <- sprintf("%s_%s_%s_%s_%s_tcb_%s_%s_%s_%s.csv",
                   FISHMIP_MODEL, FISHMIP_FORCING, FISHMIP_CLIMATE, FISHMIP_SOC, FISHMIP_SENS,
                   FISHMIP_REGION, FISHMIP_TIMESTEP, FISHMIP_START_YEAR, FISHMIP_END_YEAR)
write.csv(tcb_df, file.path(OUTPUT_DIR, filename), row.names = FALSE)
cat(sprintf("  Saved: %s\n", file.path(OUTPUT_DIR, filename)))

# Note: No catch outputs for climate-only (no fishing)

# Also save per-simulation raw data for full transparency (compressed)
cat("Saving per-simulation raw data (this may take a moment)...\n")

# tcblog10 raw
raw_tcblog10_list <- list()
for (i in 1:n_sims) {
  df <- expand.grid(year = times, size_class = FISHMIP_BIN_NAMES)
  df$sim_id <- i
  df$tcblog10_gm2 <- as.vector(all_tcblog10[i, , ])
  raw_tcblog10_list[[i]] <- df
}
raw_tcblog10_df <- do.call(rbind, raw_tcblog10_list)
raw_tcblog10_df$size_class <- factor(raw_tcblog10_df$size_class, levels = FISHMIP_BIN_NAMES)

saveRDS(raw_tcblog10_df, file.path(OUTPUT_DIR, "tcblog10_all_sims.rds"))
cat(sprintf("  Saved: %s\n", file.path(OUTPUT_DIR, "tcblog10_all_sims.rds")))

# tcb raw
raw_tcb_df <- data.frame(
  sim_id = rep(1:n_sims, each = n_times),
  year = rep(times, n_sims),
  tcb_gm2 = as.vector(t(all_tcb))
)
saveRDS(raw_tcb_df, file.path(OUTPUT_DIR, "tcb_all_sims.rds"))
cat(sprintf("  Saved: %s\n", file.path(OUTPUT_DIR, "tcb_all_sims.rds")))

# Note: No catch raw data for climate-only (no fishing)

###############################################################################
# Save NetCDF Output
###############################################################################

if (has_ncdf4) {
  cat("Saving NetCDF outputs...\n")
  
  # FishMIP naming convention for NetCDF
  nc_filename <- sprintf("%s_%s_%s_%s_%s_allvars_%s_%s_%s_%s.nc",
                        FISHMIP_MODEL, FISHMIP_FORCING, FISHMIP_CLIMATE, FISHMIP_SOC, FISHMIP_SENS,
                        FISHMIP_REGION, FISHMIP_TIMESTEP, FISHMIP_START_YEAR, FISHMIP_END_YEAR)
  
  time_dim <- ncdim_def("time", "days since 1841-1-1 00:00:00", time_days, unlim = FALSE)
  size_class_dim <- ncdim_def("size_class", "log10_g", 1:6, unlim = FALSE)
  stat_dim <- ncdim_def("statistic", "", 1:5, unlim = FALSE)
  
  tcblog10_var <- ncvar_def("tcblog10", "g m-2", 
                             list(time_dim, size_class_dim, stat_dim),
                             missval = FISHMIP_MISSING,
                             longname = "Total Consumer Biomass Density in log10 Weight Bins",
                             prec = "float")
  
  tcb_var <- ncvar_def("tcb", "g m-2",
                        list(time_dim, stat_dim),
                        missval = FISHMIP_MISSING,
                        longname = "Total Consumer Biomass Density",
                        prec = "float")
  
  # Note: No catch variables for climate-only (no fishing)
  
  # Create NetCDF file with FishMIP naming convention
  nc_file <- file.path(OUTPUT_DIR, nc_filename)
  nc <- nc_create(nc_file, list(tcblog10_var, tcb_var))
  
  ncvar_put(nc, tcblog10_var, tcblog10_stats)
  ncvar_put(nc, tcb_var, tcb_stats)
  
  ncatt_put(nc, 0, "title", "Prydz Bay mizer FishMIP ISIMIP3a outputs - Climate-Only (Unfished)")
  ncatt_put(nc, 0, "institution", "University of Tasmania")
  ncatt_put(nc, 0, "source", "mizer size-spectrum model Monte Carlo ensemble - climate-only (no fishing)")
  ncatt_put(nc, 0, "model_domain_area_m2", MODEL_DOMAIN_AREA)
  ncatt_put(nc, 0, "n_ensemble_members", n_sims)
  ncatt_put(nc, 0, "size_class_names", paste(FISHMIP_BIN_NAMES, collapse = ", "))
  ncatt_put(nc, 0, "size_class_boundaries_g", paste(FISHMIP_BINS, collapse = ", "))
  ncatt_put(nc, 0, "statistics", "1=median, 2=q05, 3=q25, 4=q75, 5=q95")
  ncatt_put(nc, 0, "fishing_effort", "0 (climate-only, unfished scenario)")
  ncatt_put(nc, 0, "creation_date", as.character(Sys.time()))
  
  nc_close(nc)
  cat(sprintf("  Saved: %s\n", nc_file))
  
} else {
  cat("Skipping NetCDF output (ncdf4 package not installed)\n")
}

###############################################################################
# Summary Statistics
###############################################################################

cat("\n=============================================================\n")
cat("Summary Statistics - CLIMATE-ONLY (UNFISHED)\n")
cat("=============================================================\n\n")

cat("tcblog10 (Total Consumer Biomass Density by size class, g m^-2):\n")
cat("-----------------------------------------------------------------\n")
for (b in 1:6) {
  cat(sprintf("  %s: median = %.4e, 90%% CI = [%.4e, %.4e]\n",
              FISHMIP_BIN_NAMES[b],
              median(tcblog10_stats[, b, "median"]),
              median(tcblog10_stats[, b, "q05"]),
              median(tcblog10_stats[, b, "q95"])))
}

cat("\ntcb (Total Consumer Biomass Density, g m^-2):\n")
cat("----------------------------------------------\n")
cat(sprintf("  Overall median: %.4e\n", median(tcb_stats[, "median"])))
cat(sprintf("  Overall 90%% CI: [%.4e, %.4e]\n", 
            median(tcb_stats[, "q05"]), median(tcb_stats[, "q95"])))

cat("\nNote: No catch outputs for climate-only (no fishing scenario)\n")

cat("\n=============================================================\n")
cat("Output files saved to:", OUTPUT_DIR, "\n")
cat("=============================================================\n")
cat("\nThis climate-only (unfished) ensemble can be directly compared\n")
cat("to the fished ensemble to assess fishing impacts.\n")
cat(sprintf("Processed %d simulations successfully.\n", n_sims - length(failed_sims)))
