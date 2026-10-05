# R2_temperature.R -- replay 02_Preparing_Climate_Forcings.Rmd:733-792
# (annual surface temperature; the three interpolated depth realms) and
# compare with the stored *_annual.rds files. The monthly bottom-temperature
# CSV is missing, so tob_annual.rds itself cannot be rebuilt.
source(file.path(Sys.getenv("ASSESS_SRC", "R/model_construction"), "A0_helpers.R"))
suppressPackageStartupMessages({ library(dplyr); library(reshape2); library(lubridate) })
id <- "R2"

tos_st <- readRDS(repo_path("tos_annual.rds")); tob_st <- readRDS(repo_path("tob_annual.rds"))
t500_st <- readRDS(repo_path("t500m_annual.rds")); t1000_st <- readRDS(repo_path("t1000m_annual.rds"))
t1500_st <- readRDS(repo_path("t1500m_annual.rds"))
note(id, "stored annual files", paste0("length ", length(tos_st), ", names ", names(tos_st)[1], " .. ", tail(names(tos_st), 1)))

# 02:610 reads FishMIP_Temperature_Forcing/, which does not exist; the tos CSV
# sits in FishMIP_Plankton_Forcing/
df_gfdl_tos <- read.csv(repo_path("FishMIP_Plankton_Forcing",
  "gfdl-mom6-cobalt2_obsclim_tos_15arcmin_prydz-bay_monthly_1961_2010.csv"))
note(id, "tos grid cells", nrow(df_gfdl_tos))

# 02:733-741, verbatim
GFDL_tos_annual <- df_gfdl_tos %>%
  select(!c(lat, lon, area_m2)) %>%
  melt() %>%
  group_by(variable) %>%
  rename(date = variable) %>%
  summarise(tos = mean(value)) %>%
  mutate(date_tidy = parse_date_time(date, orders = "my")) %>%
  group_by(year = lubridate::floor_date(date_tidy, "year")) %>%
  summarize(tos_ = mean(tos))
check(id, "tos_annual rebuilt from the monthly CSV (unweighted cell mean, 02:723)",
      max_rel(GFDL_tos_annual$tos_, tos_st) < 1e-12,
      paste("max rel diff", fmt(max_rel(GFDL_tos_annual$tos_, tos_st))))

# the same, area-weighted, to size the effect of the unweighted mean
vals <- as.matrix(df_gfdl_tos[, !(names(df_gfdl_tos) %in% c("lat", "lon", "area_m2"))])
w_mon <- colSums(vals * df_gfdl_tos$area_m2) / sum(df_gfdl_tos$area_m2)
w_ann <- colMeans(matrix(w_mon, nrow = 12))
note(id, "area-weighted minus unweighted annual tos (degC), range",
     paste(fmt(range(w_ann - GFDL_tos_annual$tos_)), collapse = " to "))

# 02:789-792, from the stored surface and bottom series
t500 <- (tob_st - tos_st) / 4 + tos_st
t1000 <- (tob_st - tos_st) / 2 + tos_st
t1500 <- ((tob_st - tos_st) / 4) * 3 + tos_st
check(id, "t500m_annual = (tob - tos)/4 + tos", max_rel(t500, t500_st) < 1e-12,
      paste("max rel diff", fmt(max_rel(t500, t500_st))))
check(id, "t1000m_annual = (tob - tos)/2 + tos", max_rel(t1000, t1000_st) < 1e-12,
      paste("max rel diff", fmt(max_rel(t1000, t1000_st))))
check(id, "t1500m_annual = 3(tob - tos)/4 + tos", max_rel(t1500, t1500_st) < 1e-12,
      paste("max rel diff", fmt(max_rel(t1500, t1500_st))))

allt <- c(tos_st, t500_st, t1000_st, t1500_st, tob_st)
note(id, "all five realms 1961-2010: median, range (degC)",
     paste0(fmt(median(allt)), "; ", paste(fmt(range(allt)), collapse = " to ")))
note(id, "tos 1961-2010 mean; tob 1961-2010 mean (degC)",
     paste(fmt(mean(tos_st)), fmt(mean(tob_st)), sep = "; "))
save_results(id)
