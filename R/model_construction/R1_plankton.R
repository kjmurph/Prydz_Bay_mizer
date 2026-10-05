# R1_plankton.R -- replay 02_Preparing_Climate_Forcings.Rmd:72-458 (annual
# plankton spectra) from the five raw GFDL-MOM6-COBALT2 obsclim CSVs and
# compare with the stored GFDL_resource_spectra_annual.dat.
#
# The code below is 02's, line for line, with only the file paths changed.
source(file.path(Sys.getenv("ASSESS_SRC", "R/model_construction"), "A0_helpers.R"))
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
})
id <- "R1"
pf <- function(f) repo_path("FishMIP_Plankton_Forcing", f)

# 02:74 (diatoms carry area_m2 and Mon_YYYY columns)
df_diat_raw <- read.csv(pf("gfdl-mom6-cobalt2_obsclim_phydiat-vint_15arcmin_prydz-bay_monthly_1961_2010.csv"))
# 02:144, 02:207 -- on disk these two are spelled "Prydz-Bay"; 02 spells them
# "prydz-bay", which resolves only on a case-insensitive file system
df_pico_raw <- read.csv(pf("gfdl-mom6-cobalt2_obsclim_phypico-vint_15arcmin_Prydz-Bay_monthly_1961_2010.csv"))
df_diaz_raw <- read.csv(pf("gfdl-mom6-cobalt2_obsclim_phydiaz-vint_15arcmin_Prydz-Bay_monthly_1961_2010.csv"))
df_zmicro_raw <- read.csv(pf("gfdl-mom6-cobalt2_obsclim_zmicro-vint_15arcmin_prydz-bay_monthly_1961_2010.csv"))
df_zmeso_raw <- read.csv(pf("gfdl-mom6-cobalt2_obsclim_zmeso-vint_15arcmin_prydz-bay_monthly_1961_2010.csv"))

# --- structural checks that 02 relies on but never tests --------------------
note(id, "grid cells per file", paste(
  sprintf("%s=%d", c("diat", "pico", "diaz", "zmicro", "zmeso"),
          c(nrow(df_diat_raw), nrow(df_pico_raw), nrow(df_diaz_raw),
            nrow(df_zmicro_raw), nrow(df_zmeso_raw))), collapse = " "))
note(id, "area_m2 column present", paste(
  sprintf("%s=%s", c("diat", "pico", "diaz", "zmicro", "zmeso"),
          c("area_m2" %in% names(df_diat_raw), "area_m2" %in% names(df_pico_raw),
            "area_m2" %in% names(df_diaz_raw), "area_m2" %in% names(df_zmicro_raw),
            "area_m2" %in% names(df_zmeso_raw))), collapse = " "))
same_cells <- function(a, b) isTRUE(all.equal(a$lat, b$lat, tolerance = 0)) &&
  isTRUE(all.equal(a$lon, b$lon, tolerance = 0))
check(id, "pico cells in the same order as diatoms (02:166 borrows diatom area by position)",
      same_cells(df_pico_raw, df_diat_raw))
check(id, "diaz cells in the same order as diatoms (02:227 borrows diatom area by position)",
      same_cells(df_diaz_raw, df_diat_raw))
check(id, "zmicro/zmeso area_m2 identical to diatom area_m2",
      identical(df_zmicro_raw$area_m2, df_diat_raw$area_m2) &&
        identical(df_zmeso_raw$area_m2, df_diat_raw$area_m2))
note(id, "sum of area_m2 over the extraction grid", fmt(sum(df_diat_raw$area_m2)))

# --- 02:113-132 diatoms -------------------------------------------------------
df_diat_long <- df_diat_raw %>%
  gather(Date, mol_C_m2, Jan_1961:Dec_2010) %>%
  mutate(C_g_m2 = mol_C_m2 * 12.001) %>%
  mutate(C_g = mol_C_m2 * area_m2) %>%
  mutate(C_gww = C_g * 10) %>%
  mutate(date = parse_date_time(Date, orders = "my"))
df_area <- df_diat_long$area_m2
df_date <- df_diat_long$date
df_diat_long <- df_diat_long %>% group_by(date) %>% summarise(total_C_gww = sum(C_gww))
df_diat_long_annual <- df_diat_long %>%
  group_by(year = lubridate::floor_date(date, "year")) %>%
  summarize(total_C_gww = mean(total_C_gww))

# --- 02:164-179 pico; 02:225-239 diaz ------------------------------------------
long_borrowed <- function(raw) {
  long <- raw %>%
    gather(Date, mol_C_m2, X1961.01.01.00.00.00:X2010.12.01.00.00.00) %>%
    mutate(area_m2 = df_area) %>%
    mutate(date = df_date) %>%
    mutate(C_g_m2 = mol_C_m2 * 12.001) %>%
    mutate(C_g = mol_C_m2 * area_m2) %>%
    mutate(C_gww = C_g * 10) %>%
    group_by(date) %>%
    summarise(total_C_gww = sum(C_gww))
  annual <- long %>% group_by(year = lubridate::floor_date(date, "year")) %>%
    summarize(total_C_gww = mean(total_C_gww))
  list(long = long, annual = annual)
}
pico <- long_borrowed(df_pico_raw); diaz <- long_borrowed(df_diaz_raw)

# --- 02:266-278 zmicro; 02:306-318 zmeso -----------------------------------------
long_own <- function(raw) {
  long <- raw %>%
    gather(Date, mol_C_m2, Jan_1961:Dec_2010) %>%
    mutate(C_g_m2 = mol_C_m2 * 12.001) %>%
    mutate(C_g = mol_C_m2 * area_m2) %>%
    mutate(C_gww = C_g * 10) %>%
    mutate(date = parse_date_time(Date, orders = "my")) %>%
    group_by(date) %>%
    summarise(total_C_gww = sum(C_gww))
  annual <- long %>% group_by(year = lubridate::floor_date(date, "year")) %>%
    summarize(total_C_gww = mean(total_C_gww))
  list(long = long, annual = annual)
}
zmicro <- long_own(df_zmicro_raw); zmeso <- long_own(df_zmeso_raw)

# --- 02:330-363 mid-points and abundances -----------------------------------
pico_mid <- (4/3) * pi * ((0.5 * 0.0001 * 5.1)^3)
large_mid <- (4/3) * pi * ((0.5 * 0.0001 * 105)^3)
micro_mid <- (4/3) * pi * ((0.5 * 0.0001 * 101)^3)
meso_mid <- (4/3) * pi * ((0.5 * 0.0001 * 10100)^3)
pico_abund_annual <- pico$annual[, 2] / pico_mid
large_abund_annual <- (df_diat_long_annual[, 2] + diaz$annual[, 2]) / large_mid
micro_abund_annual <- zmicro$annual[, 2] / micro_mid
meso_abund_annual <- zmeso$annual[, 2] / meso_mid
plankton_x <- log10(c(pico_mid, micro_mid, large_mid, meso_mid))

# --- 02:382 the size grid; 02 reads it from the repository root, where the file
# no longer is (it now lives in params/)
so_params <- readRDS(repo_path("params", "params_latest_xx.RDS"))
full_x <- log10(so_params@w_full)
note(id, "resource grid length (02:382-386)", length(full_x))

# --- 02:434-458 annual spectra ---------------------------------------------------
fit_annual <- function(scale = 1) {
  out <- array(numeric(), c(50, 142)); sl <- ic <- numeric(50)
  for (t in seq(1, 50, 1)) {
    yy <- log10(scale * c(pico_abund_annual$total_C_gww[t], micro_abund_annual$total_C_gww[t],
                          large_abund_annual$total_C_gww[t], meso_abund_annual$total_C_gww[t]))
    lmf <- lm(yy ~ plankton_x)
    out[t, ] <- lmf$coefficients[2] * full_x + lmf$coefficients[1]
    ic[t] <- lmf$coefficients[1]; sl[t] <- lmf$coefficients[2]
  }
  list(out = out, slope = sl, intercept = ic)
}
rep02 <- fit_annual()
stored <- as.matrix(read.table(repo_path("GFDL_resource_spectra_annual.dat")))
check(id, "rebuilt annual spectra vs GFDL_resource_spectra_annual.dat (50 x 142)",
      identical(dim(stored), dim(rep02$out)) && max_rel(rep02$out, stored) < 1e-12,
      paste("max rel diff", fmt(max_rel(rep02$out, stored)),
            "| max abs diff", fmt(max(abs(rep02$out - unname(stored))))))
note(id, "fitted annual slope, range", paste(fmt(range(rep02$slope)), collapse = " to "))

# --- the unapplied mol C -> g C conversion (02:115-116, used by S6.4) -------------
rep_g <- fit_annual(scale = 12.001)
shift <- rep_g$out - rep02$out
check(id, "x12.001 shifts every log10 spectrum by exactly log10(12.001)",
      max(abs(shift - log10(12.001))) < 1e-9,
      paste("shift", fmt(mean(shift)), "vs", fmt(log10(12.001))))
anom <- function(m) sweep(m, 2, colMeans(m))
check(id, "the shift cancels in the 1961-2010 anomaly that 06:1086-1088 forms",
      max(abs(anom(rep_g$out) - anom(rep02$out))) < 1e-9,
      paste("max abs diff", fmt(max(abs(anom(rep_g$out) - anom(rep02$out))))))
check(id, "slopes unchanged by the constant factor",
      max(abs(rep_g$slope - rep02$slope)) < 1e-12)

# --- the monthly product in FishMIP_Plankton_Forcing/ (02:399-424) ---------------
mon <- tryCatch(as.matrix(read.table(pf("GFDL_resource_spectra.dat"))), error = function(e) NULL)
note(id, "FishMIP_Plankton_Forcing/GFDL_resource_spectra.dat dims",
     if (is.null(mon)) "unreadable" else paste(dim(mon), collapse = " x "))

save_results(id)
