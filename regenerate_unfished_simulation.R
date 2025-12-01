# Regenerate Unfished Simulation for 1841-2010
# Creates a climate-only (no fishing) baseline simulation for comparison
# with the Monte Carlo fished ensemble

library(therMizer)
library(mizer)

cat("=== Regenerating Unfished Simulation (1841-2010) ===\n\n")

# ------------------------------------------------------------------------------
# Load required data
# ------------------------------------------------------------------------------
cat("Loading model parameters and forcing data...\n")

# Load the steady-state parameters (same as used for MC ensemble)
params_new_v4 <- readRDS("params_steady_state_2011_2020_tol_0.00025.RDS")
cat("  Loaded: params_steady_state_2011_2020_tol_0.00025.RDS\n")

# Load climate forcings (temperature and phytoplankton)
extended_ocean_temp <- readRDS("temperature_forcing_1841_2010.rds")
extended_n_pp_array <- readRDS("phytoplankton_forcing_1841_2010.rds")
cat("  Loaded: temperature_forcing_1841_2010.rds\n")
cat("  Loaded: phytoplankton_forcing_1841_2010.rds\n")

# Check dimensions
cat("\nForcing data dimensions:\n")
cat("  Temperature array:", dim(extended_ocean_temp), "\n")
cat("  Phytoplankton array:", dim(extended_n_pp_array), "\n")

# ------------------------------------------------------------------------------
# Set up therMizer parameters with climate forcing
# ------------------------------------------------------------------------------
cat("\nSetting up therMizer parameters with climate forcing...\n")

params_1841_2010_climate_only <- upgradeTherParams(
  params_new_v4, 
  ocean_temp_array = extended_ocean_temp,
  n_pp_array = extended_n_pp_array,
  aerobic_effect = FALSE, 
  metabolism_effect = TRUE
)

cat("  therMizer parameters created successfully\n")

# ------------------------------------------------------------------------------
# Run spinup simulation
# ------------------------------------------------------------------------------
cat("\nRunning spinup simulation (118 years, unfished)...\n")
cat("  This establishes initial conditions from the steady state\n")

spinup_start <- Sys.time()

sim_spinup <- project(
  params_1841_2010_climate_only, 
  t_start = 1841, 
  t_max = 118,  
  effort = 0
)

spinup_end <- Sys.time()
cat("  Spinup completed in", round(difftime(spinup_end, spinup_start, units = "mins"), 2), "minutes\n")

# ------------------------------------------------------------------------------
# Run main climate-only simulation (unfished)
# ------------------------------------------------------------------------------
cat("\nRunning main simulation (1841-2010, unfished with climate forcing)...\n")
cat("  Using spinup end state as initial conditions\n")
cat("  Effort = 0 (no fishing)\n")

sim_start <- Sys.time()

sim_1841_2010_climate_only <- project(
  params_1841_2010_climate_only,
  initial_n = sim_spinup@n[118,,], 
  t_start = 1841, 
  t_max = 170,  # 1841 + 170 - 1 = 2010
  effort = 0
)

sim_end <- Sys.time()
cat("  Simulation completed in", round(difftime(sim_end, sim_start, units = "mins"), 2), "minutes\n")

# ------------------------------------------------------------------------------
# Verify simulation
# ------------------------------------------------------------------------------
cat("\nVerifying simulation output...\n")

# Check time range
sim_times <- as.numeric(dimnames(sim_1841_2010_climate_only@n)$time)
cat("  Time range:", min(sim_times), "-", max(sim_times), "\n")
cat("  Number of time steps:", length(sim_times), "\n")

# Check species
species_names <- dimnames(sim_1841_2010_climate_only@n)$sp
cat("  Number of species:", length(species_names), "\n")

# Check total biomass at start and end
biomass_start <- sum(sim_1841_2010_climate_only@n[1,,] * sim_1841_2010_climate_only@params@w * 
                     sim_1841_2010_climate_only@params@dw)
biomass_end <- sum(sim_1841_2010_climate_only@n[length(sim_times),,] * 
                   sim_1841_2010_climate_only@params@w * 
                   sim_1841_2010_climate_only@params@dw)

cat("  Total biomass at start (1841):", format(biomass_start, scientific = TRUE, digits = 3), "g\n")
cat("  Total biomass at end (2010):", format(biomass_end, scientific = TRUE, digits = 3), "g\n")
cat("  Biomass change:", round((biomass_end/biomass_start - 1) * 100, 1), "%\n")

# ------------------------------------------------------------------------------
# Save simulation object
# ------------------------------------------------------------------------------
cat("\nSaving simulation object...\n")

# Create output directory if needed
output_dir <- "sim_objects"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Save the main simulation
output_file <- file.path(output_dir, "sim_1841_2010_unfished_climate_only.rds")
saveRDS(sim_1841_2010_climate_only, output_file)
cat("  Saved:", output_file, "\n")

# Also save the spinup for reference
spinup_file <- file.path(output_dir, "sim_1841_spinup_unfished.rds")
saveRDS(sim_spinup, spinup_file)
cat("  Saved:", spinup_file, "\n")

# ------------------------------------------------------------------------------
# Create quick diagnostic plots
# ------------------------------------------------------------------------------
cat("\nCreating diagnostic plots...\n")

# Create plots directory if needed
plot_dir <- "fishmip_outputs/unfished_diagnostics"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

# Biomass plot
png(file.path(plot_dir, "unfished_biomass_timeseries.png"), width = 12, height = 8, units = "in", res = 300)
plot(sim_1841_2010_climate_only)
dev.off()
cat("  Saved: unfished_biomass_timeseries.png\n")

# Relative biomass plot (using mizer's built-in function if available)
tryCatch({
  png(file.path(plot_dir, "unfished_biomass_relative.png"), width = 12, height = 8, units = "in", res = 300)
  print(plotlyBiomass(sim_1841_2010_climate_only))
  dev.off()
  cat("  Saved: unfished_biomass_relative.png\n")
}, error = function(e) {
  cat("  Skipped relative biomass plot (function not available)\n")
})

# Size spectrum at different time periods
library(ggplot2)

# Function to extract spectrum for a time range
get_spectrum_data <- function(sim, time_range, label) {
  times <- as.numeric(dimnames(sim@n)$time)
  time_idx <- which(times %in% time_range)
  
  if (length(time_idx) == 0) return(NULL)
  
  # Sum across species and average across time
  n_avg <- apply(sim@n[time_idx, , , drop = FALSE], 3, mean)
  
  data.frame(
    w = sim@params@w,
    n = n_avg,
    period = label
  )
}

# Extract spectra for key periods
spectrum_data <- rbind(
  get_spectrum_data(sim_1841_2010_climate_only, 1841:1860, "1841-1860 (Pre-exploitation)"),
  get_spectrum_data(sim_1841_2010_climate_only, 1930:1950, "1930-1950 (Peak whaling)"),
  get_spectrum_data(sim_1841_2010_climate_only, 1977:1986, "1977-1986 (Peak krill fishing)"),
  get_spectrum_data(sim_1841_2010_climate_only, 2001:2010, "2001-2010 (Recent)")
)

p_spectrum <- ggplot(spectrum_data, aes(x = w, y = n, color = period)) +
  geom_line(linewidth = 1) +
  scale_x_log10(labels = function(x) ifelse(x >= 1000, paste0(x/1000, " kg"), paste0(x, " g"))) +
  scale_y_log10() +
  labs(
    title = "Unfished Community Size Spectrum (Climate Only)",
    subtitle = "Time-averaged spectra for different periods",
    x = "Body mass",
    y = "Abundance (numbers per size bin)",
    color = "Period"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.75, 0.85),
    legend.background = element_rect(fill = "white", color = "grey80"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )

ggsave(file.path(plot_dir, "unfished_size_spectrum_periods.png"), p_spectrum, 
       width = 10, height = 7, dpi = 300)
cat("  Saved: unfished_size_spectrum_periods.png\n")

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------
cat("\n=== Simulation Complete ===\n")
cat("\nOutput files:\n")
cat("  sim_objects/sim_1841_2010_unfished_climate_only.rds\n")
cat("  sim_objects/sim_1841_spinup_unfished.rds\n")
cat("  fishmip_outputs/unfished_diagnostics/unfished_biomass_timeseries.png\n")
cat("  fishmip_outputs/unfished_diagnostics/unfished_biomass_relative.png\n")
cat("  fishmip_outputs/unfished_diagnostics/unfished_size_spectrum_periods.png\n")

cat("\nThis unfished simulation can now be used for comparison with the\n")
cat("fished Monte Carlo ensemble to assess fishing impacts on the\n")
cat("community size spectrum.\n")
