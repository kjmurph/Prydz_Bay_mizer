# R4_effort.R -- (a) replay 06_steady_state_therMizer.Rmd:48-80 and 491-518
# (effort_array.csv -> effort_array_1841_2010.rds); (b) rebuild
# effort_array.csv itself from the FishMIP Prydz subset (01:464-499) and the
# IWC CPUE series in git history (03:156-161), combined as 04:780-839 does.
source(file.path(Sys.getenv("ASSESS_SRC", "R/model_construction"), "A0_helpers.R"))
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
id <- "R4"

# ---- (a) 06:48-80 ------------------------------------------------------------
time_steps_effort <- 1930:2010
isimip_effort <- read.csv(repo_path("effort_array.csv"))
years <- isimip_effort[, 1]
isimip_effort <- isimip_effort[, -1]
rownames(isimip_effort) <- years
for (i in 1:ncol(isimip_effort)) colnames(isimip_effort)[i] <- "knife_edge_gear"
effort_1930_2010 <- isimip_effort[-c(82:90), ]
isimip_effort_array <- as(effort_1930_2010, "matrix")
gear_name <- colnames(isimip_effort)
effort_array <- array(NA, c(length(time_steps_effort), length(gear_name)),
                      dimnames = list(time = time_steps_effort, gear = gear_name))
effort_array[1, ] <- isimip_effort_array[1, ]
for (t in seq(1, length(time_steps_effort) - 1, 1)) effort_array[t + 1, ] <- isimip_effort_array[t + 1, ]
effort_array[is.na(effort_array)] <- 0
# 06:491-518
new_years <- as.character(1841:1929)
new_data <- matrix(0, nrow = length(new_years), ncol = ncol(effort_array))
dimnames(new_data) <- list(time = new_years, gear = dimnames(effort_array)[[2]])
combined_effort_array <- rbind(new_data, effort_array)
species_names <- readRDS(repo_path("params_for_use.RDS"))@species_params$species
colnames(combined_effort_array) <- species_names
stored <- readRDS(repo_path("effort_array_1841_2010.rds"))
check(id, "effort_array_1841_2010.rds rebuilt from effort_array.csv (06:48-80, 491-518)",
      identical(combined_effort_array, stored),
      paste("values equal:", isTRUE(all.equal(unname(combined_effort_array), unname(stored), tolerance = 0)),
            "| dimnames equal:", identical(dimnames(combined_effort_array), dimnames(stored))))
check(id, "effort is zero for every gear before 1930", all(stored[as.character(1841:1929), ] == 0))
mx <- apply(stored, 2, max)
note(id, "per-gear maximum over 1841-2010 (gears with effort)",
     paste(sprintf("%s=%s", names(mx)[mx > 0], fmt(mx[mx > 0])), collapse = "; "))
note(id, "stored effort_array.csv has rows for", paste(range(years), collapse = "-"))

# ---- (b) upstream rebuild of effort_array.csv -----------------------------------
# FishMIP: 01:77-78 filter of the full regional file; the tracked Prydz subset is
# that filter (03:208-211)
df_Prydz_effort <- read.csv(repo_path("FishMIP_fishing_data", "effort_histsoc_1841_2010_regional_models_Prydz.Bay.csv"))
pre1950 <- df_Prydz_effort %>% filter(Year < 1950) %>% group_by(Year) %>% summarise(tot = sum(NomActive))
note(id, "FishMIP histsoc Prydz effort before 1950: max yearly total NomActive", fmt(max(pre1950$tot)))
post1950 <- df_Prydz_effort %>% filter(Year >= 1950) %>% group_by(Year) %>% summarise(tot = sum(NomActive))
note(id, "FishMIP histsoc Prydz effort 1950-2010: max yearly total NomActive", fmt(max(post1950$tot)))
# 01:464-499, verbatim
df_Prydz_effort_prepared <- df_Prydz_effort %>%
  dplyr::mutate(species = case_when(FGroup == "demersal<30cm" ~ "shelf and coastal fishes",
                                    FGroup == "rays<90cm" ~ "rays<90cm",
                                    FGroup == "benthopelagic30-90cm" ~ "shelf and coastal fishes",
                                    FGroup == "benthopelagic>=90cm" ~ "toothfishes",
                                    FGroup == "krill" ~ "antarctic krill",
                                    FGroup == "pelagic30-90cm" ~ "shelf and coastal fishes",
                                    FGroup == "bathydemersal>=90cm" ~ "toothfishes",
                                    FGroup == "pelagic<30cm" ~ "shelf and coastal fishes",
                                    FGroup == "lobsterscrab" ~ "lobsterscrab",
                                    FGroup == "cephalopods" ~ "squids",
                                    FGroup == "demersal30-90cm" ~ "shelf and coastal fishes",
                                    FGroup == "bathypelagic<30cm" ~ "bathypelagic fishes",
                                    FGroup == "bathydemersal30-90cm" ~ "shelf and coastal fishes")) %>%
  filter(!FGroup == "rays<90cm" & !FGroup == "lobsterscrab") %>%
  group_by(Year, species) %>%
  summarise(effort = sum(NomActive), .groups = "drop_last") %>%
  filter(!effort < 1e-15) %>%
  group_by(species) %>%
  mutate(max_effort = max(effort)) %>%
  ungroup() %>%
  group_by(species, Year) %>%
  mutate(effort_standard = effort / max_effort) %>%
  ungroup() %>%
  tidyr::drop_na()
# IWC: 03:156-161 on the CPUE series from git history (f54387b)
df_CPUE_kg_day <- readRDS(file.path(HIST, "catch_timeseries_BanzareBank_1930_2019_CPUE.rds"))
note(id, "IWC CPUE series columns", paste(names(df_CPUE_kg_day), collapse = ","))
df_plot <- df_CPUE_kg_day %>%
  group_by(Species) %>%
  mutate(max_effort = max(effort_days)) %>%
  group_by(Species, Year) %>%
  mutate(effort_standard = effort_days / max_effort)
# 04:780-809
effort_IWC_tidy <- df_plot %>% ungroup() %>%
  dplyr::mutate(species = Species) %>%
  dplyr::select(c(Year, species, effort_standard)) %>%
  dplyr::mutate(species = case_when(species == "Baleen" ~ "baleen whales",
                                    species == "Sperm" ~ "sperm whales",
                                    species == "Antarctic Minke" ~ "minke whales",
                                    species == "Killer" ~ "orca"))
effort_FishMIP_tidy <- df_Prydz_effort_prepared %>% dplyr::select(c(Year, species, effort_standard))
effort <- rbind(effort_IWC_tidy, effort_FishMIP_tidy) %>% arrange(Year)
# compare each rebuilt (species, year) with the stored csv
st <- read.csv(repo_path("effort_array.csv"), check.names = FALSE)
names(st)[1] <- "Year"
stl <- st %>% pivot_longer(-Year, names_to = "species", values_to = "stored") %>% filter(!is.na(stored))
cmpd <- full_join(effort %>% rename(rebuilt = effort_standard), stl, by = c("Year", "species"))
both <- cmpd %>% filter(!is.na(rebuilt) & !is.na(stored))
only_r <- cmpd %>% filter(!is.na(rebuilt) & is.na(stored))
only_s <- cmpd %>% filter(is.na(rebuilt) & !is.na(stored))
check(id, "effort_array.csv rebuilt from FishMIP subset + IWC CPUE (01:464-499, 03:156-161, 04:780-839)",
      nrow(only_r) == 0 && nrow(only_s) == 0 && max_rel(both$rebuilt, both$stored) < 1e-12,
      paste0(nrow(both), " cells compared, max rel diff ", fmt(max_rel(both$rebuilt, both$stored)),
             "; only rebuilt ", nrow(only_r), "; only stored ", nrow(only_s)))
if (nrow(both)) {
  bysp <- both %>% group_by(species) %>%
    summarise(n = n(), max_rel = max(abs(rebuilt - stored) / pmax(abs(rebuilt), abs(stored))), .groups = "drop")
  for (k in seq_len(nrow(bysp))) note(id, paste("  per-species agreement:", bysp$species[k]),
                                      paste0("n=", bysp$n[k], " max rel ", fmt(bysp$max_rel[k])))
}
if (nrow(only_r)) note(id, "cells only in the rebuild (first 6)",
                       paste(head(paste(only_r$species, only_r$Year)), collapse = "; "))
if (nrow(only_s)) note(id, "cells only in the stored csv (first 6)",
                       paste(head(paste(only_s$species, only_s$Year)), collapse = "; "))
save_results(id)
