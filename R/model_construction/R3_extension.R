# R3_extension.R -- replay 06_steady_state_therMizer.Rmd:887-1260: the
# 1841-2010 temperature and plankton forcing, the constant spin-up arrays,
# and the plankton anomaly built on params_steady_state_2011_2020_tol_0.00025.
# The two extension functions are copied verbatim from 06.
source(file.path(Sys.getenv("ASSESS_SRC", "R/model_construction"), "A0_helpers.R"))
suppressPackageStartupMessages(library(mizer))
id <- "R3"

# --- 06:887-915 (abind::abind(..., along = 2) on five vectors == cbind) ------
time_steps <- 1961:2010
tos <- readRDS(repo_path("tos_annual.rds")); t500m <- readRDS(repo_path("t500m_annual.rds"))
t1000m <- readRDS(repo_path("t1000m_annual.rds")); t1500m <- readRDS(repo_path("t1500m_annual.rds"))
tob <- readRDS(repo_path("tob_annual.rds"))
ocean_temp <- cbind(tos, t500m, t1000m, t1500m, tob)
realm_names <- c("tos", "t500m", "t1000m", "t1500m", "tob")
colnames(ocean_temp) <- realm_names
rownames(ocean_temp) <- time_steps

# --- 06:920-964, verbatim ---------------------------------------------------------
extend_climate_forcing <- function(data, start_year = 1841, end_existing_data = 1960,
                                   pattern_start = 1961, pattern_end = 1980) {
  pattern_data <- data[rownames(data) >= pattern_start & rownames(data) <= pattern_end, ]
  years_to_extend <- end_existing_data - start_year + 1
  pattern_length <- pattern_end - pattern_start + 1
  full_cycles_needed <- floor(years_to_extend / pattern_length)
  remaining_years <- years_to_extend %% pattern_length
  extended_data <- NULL
  for (i in 1:full_cycles_needed) {
    cycle_data <- pattern_data
    cycle_years <- start_year + ((i - 1) * pattern_length) + (0:(pattern_length - 1))
    rownames(cycle_data) <- cycle_years
    extended_data <- rbind(extended_data, cycle_data)
  }
  if (remaining_years > 0) {
    remaining_data <- pattern_data[1:remaining_years, ]
    remaining_years_values <- start_year + (full_cycles_needed * pattern_length) + (0:(remaining_years - 1))
    rownames(remaining_data) <- remaining_years_values
    extended_data <- rbind(extended_data, remaining_data)
  }
  full_data <- rbind(extended_data, data)
  full_data <- full_data[order(as.numeric(rownames(full_data))), ]
  return(full_data)
}
extended_ocean_temp <- extend_climate_forcing(ocean_temp, 1841, 1960, 1961, 1980)
st_temp <- readRDS(repo_path("temperature_forcing_1841_2010.rds"))
check(id, "temperature_forcing_1841_2010.rds rebuilt (06:909-1006)",
      identical(unname(extended_ocean_temp), unname(st_temp)) &&
        identical(dimnames(extended_ocean_temp), dimnames(st_temp)),
      paste("max abs diff", fmt(max(abs(extended_ocean_temp - st_temp))),
            "| dimnames identical:", identical(dimnames(extended_ocean_temp), dimnames(st_temp))))
check(id, "temperature 1841-1960 is 1961-1980 repeated exactly six times",
      identical(unname(st_temp[as.character(1841:1960), ]),
                unname(do.call(rbind, rep(list(st_temp[as.character(1961:1980), ]), 6)))))

# --- 06:1013-1060 constant spin-up arrays ---------------------------------------------
constant_time_steps <- 1961:2460
first_row_temp <- ocean_temp[1, ]
constant_array_temp <- do.call(rbind, replicate(500, first_row_temp, simplify = FALSE))
rownames(constant_array_temp) <- constant_time_steps; colnames(constant_array_temp) <- realm_names
constant_time_steps_1841 <- 1841:2340
constant_array_temp_1841 <- do.call(rbind, replicate(500, first_row_temp, simplify = FALSE))
rownames(constant_array_temp_1841) <- constant_time_steps_1841; colnames(constant_array_temp_1841) <- realm_names
check(id, "constant_array_temp.RDS rebuilt (06:1013-1035)",
      identical(constant_array_temp, readRDS(repo_path("constant_array_temp.RDS"))))
check(id, "constant_array_temp_1841.RDS rebuilt (06:1038-1060)",
      identical(constant_array_temp_1841, readRDS(repo_path("constant_array_temp_1841.RDS"))))

# --- 06:1065-1089 plankton anomaly on params_new_v4 -------------------------------
# raw slots, as the 2025 session (mizer 2.5.0) read them; validParams() is
# checked not to alter the two slots used
p_raw <- readRDS(repo_path("params_steady_state_2011_2020_tol_0.00025.RDS"))
p_val <- suppressWarnings(validParams(p_raw))
check(id, "validParams() leaves initial_n_pp and dw_full untouched",
      identical(p_raw@initial_n_pp, p_val@initial_n_pp) && identical(p_raw@dw_full, p_val@dw_full))
params <- p_raw
isimip_plankton <- read.table(repo_path("GFDL_resource_spectra_annual.dat"))
isimip_plankton <- as(isimip_plankton, "matrix")
sizes <- names(params@initial_n_pp)
n_pp_array <- array(NA, dim = c(length(time_steps), length(sizes)),
                    dimnames = list(time = time_steps, w = sizes))
n_pp_array[1, ] <- (isimip_plankton[1, ] - colMeans(isimip_plankton[, ])) + log10(params@initial_n_pp * params@dw_full)
for (t in seq(1, length(time_steps) - 1, 1)) {
  n_pp_array[t + 1, ] <- (isimip_plankton[t, ] - colMeans(isimip_plankton[, ])) + log10(params@initial_n_pp * params@dw_full)
}

# --- 06:1099-1158, verbatim -------------------------------------------------------------
extend_phytoplankton_data <- function(data_array, start_year = 1841, end_existing_data = 1960,
                                      pattern_start = 1961, pattern_end = 1980) {
  if (length(dim(data_array)) != 2) stop("Input data must be a 2D array")
  n_years <- dim(data_array)[1]
  n_size_classes <- dim(data_array)[2]
  orig_years <- pattern_start:(pattern_start + n_years - 1)
  pattern_indices <- which(orig_years >= pattern_start & orig_years <= pattern_end)
  pattern_years <- orig_years[pattern_indices]
  pattern_data <- data_array[pattern_indices, , drop = FALSE]
  years_to_extend <- end_existing_data - start_year + 1
  pattern_length <- length(pattern_years)
  full_cycles_needed <- floor(years_to_extend / pattern_length)
  remaining_years <- years_to_extend %% pattern_length
  extended_length <- years_to_extend + n_years
  extended_array <- array(NA, dim = c(extended_length, n_size_classes))
  extended_years <- start_year:(start_year + extended_length - 1)
  rownames(extended_array) <- extended_years
  if (!is.null(colnames(data_array))) colnames(extended_array) <- colnames(data_array)
  current_index <- 1
  for (i in 1:full_cycles_needed) {
    extended_array[current_index:(current_index + pattern_length - 1), ] <- pattern_data
    current_index <- current_index + pattern_length
  }
  if (remaining_years > 0) {
    extended_array[current_index:(current_index + remaining_years - 1), ] <- pattern_data[1:remaining_years, ]
    current_index <- current_index + remaining_years
  }
  extended_array[current_index:(current_index + n_years - 1), ] <- data_array
  return(extended_array)
}
extended_n_pp_array <- extend_phytoplankton_data(n_pp_array, 1841, 1960, 1961, 1980)
st_npp <- readRDS(repo_path("phytoplankton_forcing_1841_2010.rds"))
check(id, "phytoplankton_forcing_1841_2010.rds rebuilt (06:1065-1206)",
      identical(dim(extended_n_pp_array), dim(st_npp)) && max_rel(extended_n_pp_array, st_npp) == 0,
      paste("max rel diff", fmt(max_rel(extended_n_pp_array, st_npp)),
            "| identical():", identical(extended_n_pp_array, st_npp)))
check(id, "plankton 1841-1960 is 1961-1980 repeated exactly six times",
      identical(unname(st_npp[as.character(1841:1960), ]),
                unname(do.call(rbind, rep(list(st_npp[as.character(1961:1980), ]), 6)))))

# --- the one-year lag (06:1086-1088) -------------------------------------------------------
check(id, "plankton rows 1961 and 1962 are identical (both carry ESM year 1961)",
      identical(st_npp["1961", ], st_npp["1962", ]))
anom <- sweep(isimip_plankton, 2, colMeans(isimip_plankton))
lagged <- all(sapply(2:50, function(r) max(abs(st_npp[as.character(1960 + r), ] -
  (anom[r - 1, ] + log10(params@initial_n_pp * params@dw_full))), na.rm = TRUE) == 0))
check(id, "forcing year Y (1962-2010) carries ESM year Y-1; ESM 2010 is never used", lagged)
note(id, "temperature is not lagged: forcing year Y carries ESM year Y",
     paste("temperature row 1962 == tos_annual[2]:", st_temp["1962", "tos"] == unname(tos[2])))

# --- 06:1216-1260 constant plankton arrays --------------------------------------------------
first_row_n_pp <- n_pp_array[1, ]
constant_array_n_pp <- do.call(rbind, replicate(500, first_row_n_pp, simplify = FALSE))
rownames(constant_array_n_pp) <- constant_time_steps; colnames(constant_array_n_pp) <- sizes
constant_array_n_pp_1841 <- do.call(rbind, replicate(500, first_row_n_pp, simplify = FALSE))
rownames(constant_array_n_pp_1841) <- constant_time_steps_1841; colnames(constant_array_n_pp_1841) <- sizes
check(id, "constant_array_n_pp.RDS rebuilt (06:1216-1236)",
      identical(constant_array_n_pp, readRDS(repo_path("constant_array_n_pp.RDS"))))
check(id, "constant_array_n_pp_1841.RDS rebuilt (06:1240-1260)",
      identical(constant_array_n_pp_1841, readRDS(repo_path("constant_array_n_pp_1841.RDS"))))

# --- the forcing params_sel_adj actually carries ---------------------------------------------
sel <- readRDS(repo_path("params_sel_adj.rds"))
check(id, "params_sel_adj carries temperature_forcing_1841_2010.rds unchanged",
      isTRUE(all.equal(unname(sel@other_params$other$ocean_temp), unname(st_temp), tolerance = 0)))
check(id, "params_sel_adj carries phytoplankton_forcing_1841_2010.rds unchanged",
      max_rel(sel@other_params$other$n_pp_array, st_npp) == 0)
save_results(id)
