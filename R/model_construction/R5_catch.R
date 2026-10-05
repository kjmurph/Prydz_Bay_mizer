# R5_catch.R -- replay 01_FishMIP_Fishing_Data.Rmd:42-391 (observed catch:
# FishMIP histsoc + IWC) and compare with yield_observed_timeseries.csv; test
# the per-row group-sum in 01:62-66; compare the tidy series 06:711-744.
source(file.path(Sys.getenv("ASSESS_SRC", "R/model_construction"), "A0_helpers.R"))
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(reshape2) })
id <- "R5"

df_FishMIP_catch <- read.csv(repo_path("FishMIP_fishing_data", "calibration_catch_histsoc_1850_2004_regional_models.csv"))
# 01:62-66, verbatim
df_Prydz_catch <- df_FishMIP_catch %>%
  dplyr::filter(region == "Prydz.Bay") %>%
  dplyr::group_by(Year, Sector, SAUP, FGroup) %>%
  dplyr::mutate(total_catch = sum(Reported, IUU, Discards),
                FishMIP_yield = sum(Reported, Discards))
gs <- df_Prydz_catch %>% summarise(n = n(), .groups = "drop")
note(id, "rows per (Year, Sector, SAUP, FGroup) group in the Prydz catch",
     paste0("groups ", nrow(gs), "; with >1 row ", sum(gs$n > 1), "; max rows ", max(gs$n)))
note(id, "Prydz catch columns", paste(names(df_FishMIP_catch), collapse = ","))

map_fg <- function(d) d %>% dplyr::mutate(species = case_when(
  FGroup == "demersal<30cm" ~ "shelf and coastal fishes", FGroup == "rays<90cm" ~ "rays<90cm",
  FGroup == "benthopelagic30-90cm" ~ "shelf and coastal fishes", FGroup == "benthopelagic>=90cm" ~ "toothfishes",
  FGroup == "krill" ~ "antarctic krill", FGroup == "pelagic30-90cm" ~ "shelf and coastal fishes",
  FGroup == "bathydemersal>=90cm" ~ "toothfishes", FGroup == "pelagic<30cm" ~ "shelf and coastal fishes",
  FGroup == "lobsterscrab" ~ "lobsterscrab", FGroup == "cephalopods" ~ "squids",
  FGroup == "demersal30-90cm" ~ "shelf and coastal fishes", FGroup == "bathypelagic<30cm" ~ "bathypelagic fishes",
  FGroup == "bathydemersal30-90cm" ~ "shelf and coastal fishes"))
# 01:150-171, verbatim
df_Prydz_catch_prepared <- map_fg(df_Prydz_catch) %>%
  filter(!FGroup == "rays<90cm" & !FGroup == "lobsterscrab") %>%
  group_by(Year, species) %>%
  summarise(catch = sum(total_catch), catch_g = catch * 1e6, .groups = "drop_last")
# the same with a per-row total (no group-sum repetition), for comparison
per_row <- map_fg(df_FishMIP_catch %>% dplyr::filter(region == "Prydz.Bay")) %>%
  filter(!FGroup == "rays<90cm" & !FGroup == "lobsterscrab") %>%
  mutate(rowtot = Reported + IUU + Discards) %>%
  group_by(Year, species) %>% summarise(catch_row = sum(rowtot), .groups = "drop")
inflate <- left_join(df_Prydz_catch_prepared, per_row, by = c("Year", "species")) %>%
  mutate(ratio = catch / catch_row)
note(id, "01:65 group-sum per row vs per-row sum: ratio range",
     paste(fmt(range(inflate$ratio, na.rm = TRUE)), collapse = " to "))
infl_sp <- inflate %>% group_by(species) %>%
  summarise(tot_coded = sum(catch), tot_row = sum(catch_row), .groups = "drop") %>%
  mutate(ratio = tot_coded / tot_row)
for (k in seq_len(nrow(infl_sp))) note(id, paste("  total-catch inflation,", infl_sp$species[k]),
                                       paste0("x", fmt(infl_sp$ratio[k])))

# IWC catch (01:220-240) from the 2023-04-21 git copy
df_IWC_catch <- readRDS(file.path(HIST, "catch_timeseries_BanzareBank_1930_2019_CPUE.rds"))
df_IWC_catch_prepared <- df_IWC_catch %>%
  dplyr::select(c(Year, Species, total_catch_kg)) %>%
  dplyr::mutate(species = case_when(Species == "Baleen" ~ "baleen whales", Species == "Sperm" ~ "sperm whales",
                                    Species == "Antarctic Minke" ~ "minke whales", Species == "Killer" ~ "orca")) %>%
  dplyr::select(c(Year, species, total_catch_kg)) %>%
  mutate(catch_g = total_catch_kg * 1000) %>%
  dplyr::select(c(Year, species, catch_g))

build_csv <- function(fishmip) {
  df_combined <- bind_rows(df_IWC_catch_prepared, fishmip)
  reshaped_df <- df_combined %>% pivot_wider(names_from = species, values_from = catch_g, values_fill = 0)
  ordered_df <- reshaped_df %>% arrange(Year)
  for (s in c("mesozooplankton", "other krill", "other macrozooplankton", "salps", "mesopelagic fishes",
              "flying birds", "small divers", "leopard seals", "medium divers", "large divers")) ordered_df[[s]] <- 0
  sp_order <- readRDS(repo_path("params", "params_latest_xx.RDS"))@species_params$species
  reordered_df <- ordered_df %>% dplyr::select(Year, all_of(sp_order))
  all_years <- seq(min(reordered_df$Year), max(reordered_df$Year), by = 1)
  merged_df <- merge(data.frame(Year = all_years), reordered_df, by = "Year", all.x = TRUE)
  clean <- merged_df; clean[is.na(clean)] <- 0
  list(merged = merged_df, clean = clean)
}
coded <- build_csv(df_Prydz_catch_prepared[, c("Year", "species", "catch_g")])
st <- read.csv(repo_path("yield_observed_timeseries.csv"), check.names = FALSE)
check(id, "same years and species columns as yield_observed_timeseries.csv",
      length(st$Year) == length(coded$clean$Year) && all(st$Year == coded$clean$Year) &&
        identical(names(st), names(coded$clean)),
      paste("years", paste(range(st$Year), collapse = "-")))
m_st <- as.matrix(st[, -1]); m_co <- as.matrix(coded$clean[, -1])
rel <- abs(m_co - m_st) / pmax(abs(m_co), abs(m_st)); rel[!is.finite(rel)] <- 0
check(id, "yield_observed_timeseries.csv rebuilt as coded (01:42-391)",
      max(rel) < 1e-5,
      paste("max rel diff", fmt(max(rel)), "| zero pattern identical:", identical(m_co == 0, m_st == 0)))
# precision of the stored file: how many significant digits survive?
raw_lines <- readLines(repo_path("yield_observed_timeseries.csv"), n = 5)
note(id, "stored csv number format (first data line)", substr(raw_lines[2], 1, 90))
bad <- which(rel > 1e-12, arr.ind = TRUE)
if (nrow(bad)) {
  worst <- bad[which.max(rel[bad]), , drop = FALSE]
  note(id, "largest coded-vs-stored difference",
       paste0(colnames(m_st)[worst[2]], " ", st$Year[worst[1]], ": rebuilt ",
              format(m_co[worst], digits = 12), " stored ", format(m_st[worst], digits = 12)))
}
rowv <- build_csv(per_row %>% transmute(Year, species, catch_g = catch_row * 1e6))
m_rw <- as.matrix(rowv$clean[, -1])
relr <- abs(m_rw - m_st) / pmax(abs(m_rw), abs(m_st)); relr[!is.finite(relr)] <- 0
note(id, "per-row-sum variant vs stored csv", paste("max rel diff", fmt(max(relr))))

# coverage per group (S7.1 table)
cov <- lapply(colnames(m_st), function(s) {
  y <- st$Year[m_st[, s] > 0]
  if (!length(y)) return(NULL)
  data.frame(group = s, years = paste(range(y), collapse = "-"), n_years = length(y), total_g = sum(m_st[, s]))
})
cov <- do.call(rbind, cov)
for (k in seq_len(nrow(cov))) note(id, paste("coverage", cov$group[k]),
                                   paste0(cov$years[k], ", n=", cov$n_years[k], ", total ", fmt(cov$total_g[k]), " g"))

# 06:711-744 tidy series (built from the never-committed catch_timeseries.csv,
# i.e. merged_df before the NA fill); rebuild from the coded merge
tidy_st <- readRDS(repo_path("yield_observed_timeseries_tidy.RDS"))
note(id, "stored tidy series", paste0(nrow(tidy_st), " rows; columns ", paste(names(tidy_st), collapse = ",")))
ym <- as.matrix(coded$merged[, -1]); rownames(ym) <- coded$merged$Year
yt <- reshape2::melt(ym); names(yt) <- c("Year", "Species", "Yield")
yt <- yt %>% filter(!Yield == 0)
key <- function(d) paste(d$Year, as.character(d$Species))
mt <- match(key(yt), key(tidy_st))
check(id, "yield_observed_timeseries_tidy.RDS reproduced from the coded merge (06:711-744)",
      nrow(yt) == nrow(tidy_st) && !anyNA(mt) && max_rel(yt$Yield, tidy_st$Yield[mt]) < 1e-5,
      paste0("rows rebuilt ", nrow(yt), " vs stored ", nrow(tidy_st),
             if (!anyNA(mt)) paste0("; max rel diff ", fmt(max_rel(yt$Yield, tidy_st$Yield[mt]))) else "; keys differ"))
save_results(id)
