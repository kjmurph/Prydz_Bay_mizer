# Test Detailed Timing for Climate-Only Simulation
# Verifies full spinup + simulation workflow with timing breakdown

cat("=== Verifying Full Spinup + Simulation Workflow ===\n\n")

library(therMizer)
library(mizer)

# Load data
cat("Loading data...\n")
mc <- readRDS("Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds")
extended_ocean_temp <- readRDS("temperature_forcing_1841_2010.rds")
extended_n_pp_array <- readRDS("phytoplankton_forcing_1841_2010.rds")
cat("  Data loaded\n\n")

# Test ONE simulation with detailed timing
idx <- 1
fished_sim <- mc[["simulations"]][[idx]]
params_original <- fished_sim@params

cat("Step 1: Upgrading to therMizer params...\n")
t1 <- Sys.time()
params_climate <- upgradeTherParams(
  params_original,
  ocean_temp_array = extended_ocean_temp,
  n_pp_array = extended_n_pp_array,
  aerobic_effect = FALSE,
  metabolism_effect = TRUE
)
t2 <- Sys.time()
cat("  Time:", round(difftime(t2, t1, units = "secs"), 1), "seconds\n")

cat("\nStep 2: Running SPINUP (118 years, effort=0)...\n")
t3 <- Sys.time()
sim_spinup <- project(
  params_climate,
  t_start = 1841,
  t_max = 118,
  effort = 0
)
t4 <- Sys.time()
spinup_time <- difftime(t4, t3, units = "secs")
cat("  Time:", round(spinup_time, 1), "seconds\n")
spinup_times <- as.numeric(dimnames(sim_spinup@n)[["time"]])
cat("  Spinup time range:", min(spinup_times), "-", max(spinup_times), "\n")
cat("  Spinup timesteps:", length(spinup_times), "\n")

cat("\nStep 3: Running MAIN SIMULATION (170 years, effort=0)...\n")
cat("  Using spinup end state as initial_n\n")
t5 <- Sys.time()
sim_climate_only <- project(
  params_climate,
  initial_n = sim_spinup@n[118, , ],
  t_start = 1841,
  t_max = 170,
  effort = 0
)
t6 <- Sys.time()
main_time <- difftime(t6, t5, units = "secs")
cat("  Time:", round(main_time, 1), "seconds\n")
main_times <- as.numeric(dimnames(sim_climate_only@n)[["time"]])
cat("  Main sim time range:", min(main_times), "-", max(main_times), "\n")
cat("  Main sim timesteps:", length(main_times), "\n")

total_time <- difftime(t6, t1, units = "secs")
cat("\n=== TIMING SUMMARY ===\n")
cat("  therMizer upgrade:", round(difftime(t2, t1, units = "secs"), 1), "s\n")
cat("  Spinup (118 yr):  ", round(as.numeric(spinup_time), 1), "s\n")
cat("  Main sim (170 yr):", round(as.numeric(main_time), 1), "s\n")
cat("  --------------------------\n")
cat("  TOTAL:            ", round(as.numeric(total_time), 1), "s\n")

cat("\n=== EXTRAPOLATED ESTIMATES ===\n")
time_per_sim <- as.numeric(total_time)
cat("  Sequential (2111 sims):", round(2111 * time_per_sim / 3600, 1), "hours\n")
cat("  Parallel 30 cores:     ", round(2111 * time_per_sim / 30 / 3600, 2), "hours\n")
