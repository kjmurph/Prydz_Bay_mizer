# Item 2: year each metric first reaches AND HOLDS 100% sign consistency.
# Full 1841-2010 record, 212 matched pairs. For the Results, not Fig 1. READ ONLY.
options(width = 200)
setwd("c:/Users/uqkmur13/OneDrive - The University of Queensland/Documents/GitHub/Prydz_Bay_mizer")
suppressPackageStartupMessages({library(dplyr); library(tidyr)})
hdr <- function(x) cat("\n\n##########", x, "##########\n")

OUT <- "output/results"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# ---- paired difference series, both metrics -------------------------------
raw_fish <- readRDS("Manuscript data/biomass_top10pct_raw_fish.rds")
raw_clim <- readRDS("Manuscript data/biomass_top10pct_raw_clim.rds")
bl <- inner_join(
  raw_fish %>% group_by(sim_i, Year) %>% summarise(E = sum(Biomass, na.rm=TRUE), .groups="drop"),
  raw_clim %>% group_by(sim_i, Year) %>% summarise(U = sum(Biomass, na.rm=TRUE), .groups="drop"),
  by = c("sim_i","Year")) %>% transmute(member = sim_i, Year, d = E - U)
rm(raw_fish, raw_clim)

sl <- readRDS("Manuscript data/nbss_slope_top10pct_data.rds")$all_slopes %>%
  filter(spectrum_type == "Biomass") %>%
  select(sim_id, time, ensemble, slope) %>%
  pivot_wider(names_from = ensemble, values_from = slope, values_fn = mean) %>%
  filter(!is.na(Exploited), !is.na(Unexploited)) %>%
  transmute(member = sim_id, Year = time, d = Exploited - Unexploited)

# ---- per-year sign-consistency --------------------------------------------
consistency <- function(df, label) {
  df %>% group_by(Year) %>%
    summarise(n = n(),
              n_neg = sum(d < 0), n_pos = sum(d > 0), n_zero = sum(d == 0),
              frac_neg = mean(d < 0), median_d = median(d), .groups = "drop") %>%
    mutate(metric = label) %>% arrange(Year)
}
cb <- consistency(bl, "Total biomass")
cs <- consistency(sl, "Lambda")

# ---- first-reaches vs first-reaches-AND-HOLDS ------------------------------
summarise_emergence <- function(cc, label) {
  yrs <- cc$Year; f <- cc$frac_neg
  first_touch <- if (any(f == 1)) yrs[which(f == 1)[1]] else NA
  # permanent: earliest year y such that frac==1 for every year >= y
  ok_from <- rev(cumprod(rev(as.integer(f == 1)))) == 1
  permanent <- if (any(ok_from)) yrs[which(ok_from)[1]] else NA
  n_dropbacks <- if (!is.na(first_touch)) sum(f[yrs >= first_touch] < 1) else NA
  cat(sprintf("\n%-14s first year frac==1.00 : %s\n", label, first_touch))
  cat(sprintf("%-14s first year it reaches AND HOLDS to 2010 : %s\n", label, permanent))
  cat(sprintf("%-14s years after first touch that drop back below 1.00 : %s\n", label, n_dropbacks))
  if (!is.na(first_touch) && !is.na(permanent) && n_dropbacks > 0) {
    db <- cc %>% filter(Year >= first_touch, frac_neg < 1)
    cat(sprintf("%-14s drop-back years: %s\n", label,
                paste(sprintf("%d(%.3f)", db$Year, db$frac_neg), collapse=", ")))
  }
  c(first_touch = first_touch, permanent = permanent, n_dropbacks = n_dropbacks)
}

hdr("2A. FIRST REACHES vs FIRST REACHES-AND-HOLDS (full 1841-2010 record)")
cat("NOTE: before the first catch year the two runs are identical to machine precision,\n")
cat("      so the sign of d is float noise and frac_neg is meaningless there.\n")
eb <- summarise_emergence(cb, "Total biomass")
es <- summarise_emergence(cs, "Lambda")

hdr("2B. pre-divergence sanity: exact zeros and noise scale, 1841-1930")
for (nm in c("Total biomass","Lambda")) {
  cc <- if (nm == "Total biomass") cb else cs
  pre <- cc %>% filter(Year <= 1930)
  cat(sprintf("  %-14s years<=1930: n_zero(all yrs)=%d of %d obs; frac_neg range %.3f..%.3f\n",
              nm, sum(pre$n_zero), sum(pre$n), min(pre$frac_neg), max(pre$frac_neg)))
}
d1930 <- bl %>% filter(Year == 1930)
cat(sprintf("  biomass 1930: max|d| = %.4g g  (vs typical total ~1e14 g)\n", max(abs(d1930$d))))
d1930s <- sl %>% filter(Year == 1930)
cat(sprintf("  lambda  1930: max|d| = %.4g      (vs slope ~ -1.05)\n", max(abs(d1930s$d))))

hdr("2C. SERIES 1928-1990 (every year), both metrics")
ser <- bind_rows(cb, cs) %>% select(metric, Year, n, n_neg, frac_neg, median_d)
w <- ser %>% filter(Year >= 1928, Year <= 1990) %>%
  select(metric, Year, frac_neg) %>%
  pivot_wider(names_from = metric, values_from = frac_neg)
print(as.data.frame(w), row.names = FALSE, digits = 4)

hdr("2D. SERIES 1991-2010")
w2 <- ser %>% filter(Year >= 1991) %>% select(metric, Year, frac_neg) %>%
  pivot_wider(names_from = metric, values_from = frac_neg)
print(as.data.frame(w2), row.names = FALSE, digits = 4)

# ---- write full series -----------------------------------------------------
write.csv(ser, file.path(OUT, "sign_consistency_series_1841_2010.csv"), row.names = FALSE)
cat("\nFull series written:", file.path(OUT, "sign_consistency_series_1841_2010.csv"), "\n")

hdr("2E. cross-check: is test_community_slope_5members_data.rds also corrupted?")
p <- "test_community_slope_5members_data.rds"
if (file.exists(p)) {
  t5 <- readRDS(p)
  a5 <- if (!is.null(t5$all_slopes)) t5$all_slopes else NULL
  if (!is.null(a5)) {
    for (st in unique(a5$spectrum_type))
      cat(sprintf("  spectrum_type=%-10s time range %g .. %g  n_unique=%d\n",
                  st, min(a5$time), max(a5$time), length(unique(a5$time[a5$spectrum_type==st]))))
    cat("  -> corrupted:", any(a5$time > 2011), "\n")
  } else cat("  no $all_slopes element; names:", paste(names(t5), collapse=", "), "\n")
} else cat("  MISSING\n")
