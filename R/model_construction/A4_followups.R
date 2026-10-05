# A4_followups.R -- the follow-up diagnostics behind specific numbers in
# docs/model_construction_assessment.md that are not PASS/FAIL replays:
# ambiguous lineage edges, the thermal tolerances and scaling factors, the
# plankton level, therMizer's temperature functions and time indexing, the two
# observed-catch definitions, and the whale length-weight mismatch.
#
# Read-only. Run from the repository root AFTER A1_lineage.R (section (i) reads
# its species_param_changes.csv and section (m) its lineage_edges.csv):
#   Rscript --vanilla R/model_construction/A4_followups.R
# Prints only; writes nothing.
source(file.path(Sys.getenv("ASSESS_SRC", "R/model_construction"), "A0_helpers.R"))
OUT_ABS <- normalizePath(OUT, mustWork = FALSE)
setwd(REPO)
suppressPackageStartupMessages({ library(mizer); library(therMizer); library(dplyr); library(tidyr) })
options(width = 200)
sec <- function(x) cat("\n==================== ", x, " ====================\n", sep = "")

# ---- (a) edges 10 and 14: which parent, what forcing, which effort ----------------------
sec("(a) params_04_06_2024's parent; params_for_use's forcing and effort; params_07_06_2024's kernels")
ld <- function(f) attributes(readRDS(f))
v03 <- ld("params/therMizer_params_v03.RDS"); v04 <- ld("params/therMizer_params_v04.RDS")
ta2 <- ld("params/therMizer_params_total_area_v2.RDS"); ta <- ld("params/therMizer_params_total_area.RDS")
p0406 <- ld("params/params_04_06_2024.rds"); p0706 <- ld("params/params_07_06_2024.rds")
pfu <- ld("params_for_use.RDS"); pss <- ld("params_steady_state_2011_2020.RDS")
same <- function(x, y) isTRUE(all.equal(x, y, tolerance = 0, check.attributes = FALSE))
cat("== which of v03 / v04 / total_area_v2 does params_04_06_2024 match? ==\n")
for (fld in c("erepro","R_max","gamma","h","temp_min","temp_max","interaction_resource","beta","sigma","w_mat","w_min","w_max","ks","k","z0","biomass_observed")) {
  cat(sprintf("%-22s v03:%-5s v04:%-5s ta2:%-5s ta:%-5s\n", fld,
    same(p0406$species_params[[fld]], v03$species_params[[fld]]),
    same(p0406$species_params[[fld]], v04$species_params[[fld]]),
    same(p0406$species_params[[fld]], ta2$species_params[[fld]]),
    same(p0406$species_params[[fld]], ta$species_params[[fld]])))
}
cat("interaction  v03:", same(p0406$interaction, v03$interaction), " v04:", same(p0406$interaction, v04$interaction), " ta2:", same(p0406$interaction, ta2$interaction), "\n")
cat("other_params identical to v04:", same(p0406$other_params, v04$other_params), " to v03:", same(p0406$other_params, v03$other_params), "\n")
cat("v03 -> v04 differing species_params cols: ")
cat(names(v04$species_params)[sapply(names(v04$species_params), function(cn) !same(v04$species_params[[cn]], v03$species_params[[cn]]))], "\n")
cat("\n== params_for_use forcing arrays ==\n")
print(pfu$other_params$other$ocean_temp)
cat("n_pp rows:", rownames(pfu$other_params$other$n_pp_array), "\n")
cat("vertical_migration unique (for_use):", paste(signif(unique(as.vector(pfu$other_params$other$vertical_migration)), 4), collapse = ","), "\n")
cat("vertical_migration unique (07_06):", paste(signif(unique(as.vector(p0706$other_params$other$vertical_migration)), 4), collapse = ","), "\n")
cat("initial_effort 07_06:\n"); print(signif(p0706$initial_effort, 4))
cat("initial_effort for_use:\n"); print(signif(pfu$initial_effort, 4))
cat("initial_effort steady_2011_2020 all zero:", all(pss$initial_effort == 0), "\n")
cat("rate functions for_use:", unlist(pfu$rates_funcs[c("Encounter", "PredRate", "EReproAndGrowth")]), "\n")
sp7 <- p0706$species_params; sp4 <- ld("params/params_04_06_2024_v4.rds")$species_params
d <- which(sp7$pred_kernel_type != sp4$pred_kernel_type | is.na(sp7$ppmr_min) != is.na(sp4$ppmr_min))
print(data.frame(species = sp7$species[d], kernel_old = sp4$pred_kernel_type[d], kernel_new = sp7$pred_kernel_type[d],
                 ppmr_min_old = sp4$ppmr_min[d], ppmr_min_new = sp7$ppmr_min[d], ppmr_max_old = sp4$ppmr_max[d], ppmr_max_new = sp7$ppmr_max[d]))

# ---- (b) thermal tolerances and the domain-scaling factors ------------------------------
sec("(b) temp_min/temp_max at therMizer_params_v1, params_sel_adj, p100; v1 -> total_area scaling factors")
v1 <- ld("params/therMizer_params_v1.RDS"); sel <- ld("params_sel_adj.rds"); p100 <- ld("params_ref_p100_mort_kernel_diet.rds")
p86 <- ld("params_ref_p86_agemat.rds")
print(data.frame(species = sel$species_params$species,
  v1_min = v1$species_params$temp_min, sel_min = sel$species_params$temp_min, p100_min = p100$species_params$temp_min,
  v1_max = v1$species_params$temp_max, sel_max = sel$species_params$temp_max, p100_max = p100$species_params$temp_max), row.names = FALSE)
cat("identical temp cols sel vs p86:", identical(sel$species_params$temp_min, p86$species_params$temp_min), identical(sel$species_params$temp_max, p86$species_params$temp_max), "\n")
rat <- function(a, b) { r <- as.numeric(b) / as.numeric(a); r <- r[is.finite(r) & r != 0]; c(min = min(r), median = median(r), max = max(r)) }
print(rbind(
  biomass_observed = rat(v1$species_params$biomass_observed, ta$species_params$biomass_observed),
  yield_observed = rat(v1$species_params$yield_observed, ta$species_params$yield_observed),
  initial_n = rat(v1$initial_n, ta$initial_n), initial_n_pp = rat(v1$initial_n_pp, ta$initial_n_pp),
  kappa = rat(v1$resource_params$kappa, ta$resource_params$kappa), R_max = rat(v1$species_params$R_max, ta$species_params$R_max),
  inv_gamma = rat(ta$species_params$gamma, v1$species_params$gamma), inv_search_vol = rat(ta$search_vol, v1$search_vol)), digits = 10)
e <- ta$species_params$erepro / v1$species_params$erepro - 1
cat("erepro change v1 -> total_area: range", paste(signif(range(e) * 100, 3), collapse = " to "), "%\n")

# ---- (c) params_for_use against the candidate inputs -------------------------------------
sec("(c) params_for_use's temperature rows; params_07_06_2024's forcing vs the stored files")
ot <- pfu$other_params$other$ocean_temp
tos <- readRDS("tos_annual.rds"); t500 <- readRDS("t500m_annual.rds"); tob <- readRDS("tob_annual.rds")
cat("for_use tos:", signif(ot[, "tos"], 8), " | tos_annual[1:2]:", signif(head(as.numeric(tos), 2), 8), "\n")
cat("for_use t500m:", signif(ot[, "t500m"], 8), " | t500m_annual[1:2]:", signif(head(as.numeric(t500), 2), 8), "\n")
cat("for_use tob:", signif(ot[, "tob"], 8), " | tob_annual[1:2]:", signif(head(as.numeric(tob), 2), 8), "\n")
p7ot <- p0706$other_params$other$ocean_temp
cat("p07_06 ocean_temp == annual files (tos):", isTRUE(all.equal(as.numeric(p7ot[, "tos"]), as.numeric(tos), tolerance = 0)), "\n")
dat <- as.matrix(read.table("GFDL_resource_spectra_annual.dat"))
p7np <- p0706$other_params$other$n_pp_array
cat("p07_06 n_pp_array == .dat (absolute)?", isTRUE(all.equal(unname(p7np), unname(dat), tolerance = 0)), "\n")
eff <- read.csv("effort_array.csv"); rownames(eff) <- eff[, 1]; eff <- as.matrix(eff[, -1])
for (yrs in list(1951:1960, 2001:2010, 1961:1970, 1961:1980)) {
  cmy <- colMeans(eff[as.character(yrs), , drop = FALSE])
  cat("colMeans", range(yrs), "of effort_array.csv matches p07_06 effort:", isTRUE(all.equal(unname(cmy), unname(p0706$initial_effort), tolerance = 1e-6)), "\n")
}

# ---- (d, e) the plankton level the 2025 calibration used ----------------------------------
sec("(d, e) the resource level: params_for_use's forcing is the 06:1086 anomaly on params_07_06_2024")
datu <- unname(dat)
v4 <- ld("params_steady_state_2011_2020_tol_0.00025.RDS")
np <- unname(pfu$other_params$other$n_pp_array)
fin <- is.finite(np) & is.finite(datu[1:2, ])
cat("params_for_use n_pp rows vs .dat rows 1961-62 (finite cells): max abs diff", max(abs(np[fin] - datu[1:2, ][fin])), "\n")
below <- pfu$w_full < pfu$resource_params$w_pp_cutoff
lvl_v4 <- log10(v4$initial_n_pp * v4$dw_full)
cat("params_new_v4 log10(n_pp*dw) vs .dat row 1961 (w < cutoff): max abs diff", max(abs(lvl_v4[below] - datu[1, below])), "\n")
selnp <- unname(sel$other_params$other$n_pp_array)
cat("params_sel_adj forcing mean 1961-2010 minus params_new_v4 level (w < cutoff): range",
    paste(signif(range(colMeans(selnp[121:170, below]) - lvl_v4[below]), 4), collapse = " to "), "\n")
cat("params_for_use n_pp row 1961 identical to row 1962:", identical(np[1, ], np[2, ]), "\n")
L7 <- log10(p0706$initial_n_pp * p0706$dw_full)
an1 <- datu[1, ] - colMeans(datu) + L7
fin1 <- is.finite(an1) & is.finite(np[1, ])
cat("for_use row 1 vs 06:1086 anomaly on params_07_06_2024's level: finite cells", sum(fin1), " max abs diff", signif(max(abs(np[1, fin1] - an1[fin1])), 4), "\n")
cat("p07_06 resource dynamics:", p0706$resource_dynamics, " kappa:", signif(p0706$resource_params$kappa, 6), "\n")

# ---- (f) therMizer's temperature functions and time indexing; realised scalars -------------
sec("(f) therMizer 1.0.0 / mizer 3.1.0 source behind report section 2.4 and S6.3")
f <- deparse(therMizer::upgradeTherParams)
idx <- grep("aerobic|metabolism|setRateFunction|t_idx|plankton_forcing", f)
cat(paste(sprintf("%4d  %s", idx, f[idx]), collapse = "\n"), "\n")
cat("\n---- setMetabTher ----\n"); print(therMizer::setMetabTher)
cat("---- scaled_temp_effect ----\n"); print(therMizer::scaled_temp_effect)
cat("---- plankton_forcing ----\n"); print(therMizer::plankton_forcing)
m <- getS3method("projectToSteady", "MizerParams", optional = TRUE)
fm <- deparse(m)
idx <- grep("t <-|t = |project_simple", fm)
cat("\n---- mizer::projectToSteady.MizerParams, time lines ----\n")
cat(paste(sprintf("%4d  %s", idx, fm[idx]), collapse = "\n"), "\n")
selv <- suppressWarnings(validParams(readRDS("params_sel_adj.rds")))
sp <- species_params(selv)
Tm <- selv@other_params$other$ocean_temp
vm <- selv@other_params$other$vertical_migration; ex <- selv@other_params$other$exposure
fac <- function(T) {
  enc <- sapply(seq_len(nrow(sp)), function(i) { u <- sapply(T, function(t) { tk <- t + 273; v <- tk * (tk - (sp$temp_min[i] + 273)) * (sp$temp_max[i] - tk + 273)^0.5 / sp$encounterpred_scale[i]; if (t > sp$temp_max[i] | t < sp$temp_min[i]) 0 else v }); sum(u * vm[, i, 1] * ex[, i]) })
  met <- sapply(seq_len(nrow(sp)), function(i) { u <- sapply(T, function(t) { a <- exp(25.22 - 0.63 / (8.62e-05 * (273 + t))); v <- (a - sp$metab_min[i]) / sp$metab_range[i]; if (t > sp$temp_max[i] | t < sp$temp_min[i]) 0 else v }); sum(u * vm[, i, 1] * ex[, i]) })
  data.frame(species = sp$species, encounter_scalar = round(enc, 3), metabolism_scalar = round(met, 3))
}
cat("\nrealised therMizer scalars at model year 1841 (= climate 1961), equal residence across 5 realms:\n")
print(fac(Tm["1841", ]), row.names = FALSE)
r <- fac(colMeans(Tm[as.character(1961:2010), ]))
cat("at the 1961-2010 mean temperature: encounter", paste(range(r$encounter_scalar), collapse = " to "),
    "; metabolism", paste(range(r$metabolism_scalar), collapse = " to "), "\n")

# ---- (g, h) the two observed-catch definitions ----------------------------------------------
sec("(g, h) yield_observed_timeseries_tidy.RDS (VM MC) vs yield_observed_timeseries.csv")
st <- read.csv("yield_observed_timeseries.csv", check.names = FALSE)
tidy <- readRDS("yield_observed_timeseries_tidy.RDS")
stl <- st %>% pivot_longer(-Year, names_to = "Species", values_to = "csv") %>% filter(csv != 0)
j <- inner_join(tidy %>% mutate(Species = as.character(Species), Year = as.numeric(as.character(Year))), stl, by = c("Year", "Species")) %>%
  mutate(rel = abs(Yield - csv) / pmax(abs(Yield), abs(csv)))
print(as.data.frame(j %>% group_by(Species) %>% summarise(n = n(), n_diff = sum(rel > 1e-5), max_rel = max(rel), sum_tidy = sum(Yield), sum_csv = sum(csv))), row.names = FALSE)
fc <- read.csv("FishMIP_fishing_data/calibration_catch_histsoc_1850_2004_regional_models.csv") %>% filter(region == "Prydz.Bay")
map <- c("demersal<30cm"="shelf and coastal fishes","benthopelagic30-90cm"="shelf and coastal fishes","benthopelagic>=90cm"="toothfishes","krill"="antarctic krill","pelagic30-90cm"="shelf and coastal fishes","bathydemersal>=90cm"="toothfishes","pelagic<30cm"="shelf and coastal fishes","cephalopods"="squids","demersal30-90cm"="shelf and coastal fishes","bathypelagic<30cm"="bathypelagic fishes","bathydemersal30-90cm"="shelf and coastal fishes")
fc$species <- unname(map[fc$FGroup])
v <- fc %>% filter(!is.na(species)) %>% group_by(Year, species) %>%
  summarise(rep_disc = sum(Reported + Discards) * 1e6, all_three = sum(Reported + IUU + Discards) * 1e6, rep_only = sum(Reported) * 1e6, .groups = "drop")
jt <- inner_join(tidy %>% mutate(Species = as.character(Species), Year = as.numeric(as.character(Year))), v, by = c("Year", "Species" = "species"))
relmx <- function(a, b) max(abs(a - b) / pmax(abs(a), abs(b)))
cat("FishMIP cells joined:", nrow(jt), "\n")
cat("tidy vs Reported+Discards: max rel", signif(relmx(jt$Yield, jt$rep_disc), 4), "\n")
cat("tidy vs Reported+IUU+Discards: max rel", signif(relmx(jt$Yield, jt$all_three), 4), "\n")
cat("tidy vs Reported only: max rel", signif(relmx(jt$Yield, jt$rep_only), 4), "\n")
print(as.data.frame(v %>% group_by(species) %>% summarise(IUU_share_of_total = 1 - sum(rep_disc) / sum(all_three))), row.names = FALSE)

# ---- (i, j) edge 11's erepro; whale length-weight constants vs the IWC conversion ----------
sec("(i, j) erepro on params_04_06_2024 -> _v3; whale a, b and the mass at l50")
spc <- read.csv(file.path(OUT_ABS, "species_param_changes.csv"))
print(spc[spc$child == "params/params_04_06_2024_v3.rds" & spc$column == "erepro", c("species", "old", "new")], row.names = FALSE)
s <- sel$species_params
print(s[s$species %in% c("minke whales","orca","sperm whales","baleen whales","leopard seals"), c("species","a","b","beta","w_mat","w_max")], row.names = FALSE)
L <- c(baleen = 2230, sperm = 1520, orca = 640, minke = 870)
model <- c(baleen = 0.0067 * L[["baleen"]]^3, sperm = 0.0096 * L[["sperm"]]^3, orca = 0.006 * L[["orca"]]^3.2, minke = 0.0115 * L[["minke"]]^3)
iwc   <- c(baleen = 0.0061 * L[["baleen"]]^3, sperm = 0.0109 * L[["sperm"]]^3, orca = 0.2080 * L[["orca"]]^2.577, minke = 0.0115 * L[["minke"]]^3)
print(data.frame(l50_cm = L, model_w_t = signif(model / 1e6, 4), iwc_conversion_w_t = signif(iwc / 1e6, 4), model_over_iwc = signif(model / iwc, 4)))

# ---- (k) edge 12: kappa direction, gamma range --------------------------------------------
sec("(k) params_04_06_2024_v3 -> _v4")
a3 <- ld("params/params_04_06_2024_v3.rds"); b4 <- ld("params/params_04_06_2024_v4.rds")
cat("kappa v3 -> v4:", signif(a3$resource_params$kappa, 6), "->", signif(b4$resource_params$kappa, 6), " ratio", signif(b4$resource_params$kappa / a3$resource_params$kappa, 4), "\n")
g <- b4$species_params$gamma / a3$species_params$gamma
cat("gamma ratio v4/v3: range", paste(signif(range(g), 3), collapse = " to "), "; median", signif(median(g), 3), "\n")

# ---- (l) the interaction cells changed at construction -------------------------------------
sec("(l) interaction: v4 matrix (orca self-interaction 0) vs the first 19-group object")
stm <- as.matrix(readRDS("interaction matrix/trait_groups_interaction_matrix_vCWC_v4.rds")); stm[17, 17] <- 0
p0 <- readRDS("params/steady_phase1_group_params_v3.RDS"); pint <- p0@interaction
w <- which(abs(pint - stm) > 0, arr.ind = TRUE)
print(data.frame(predator = rownames(pint)[w[, 1]], prey = colnames(pint)[w[, 2]], v4 = round(stm[w], 4), built = round(pint[w], 4)), row.names = FALSE)

# ---- (m) the bridge: what changed into p86, p75, p76 -----------------------------------------
sec("(m) stored-field changes into params_ref_p86_agemat, p75, p76 (from A1's lineage_edges.csv)")
le <- read.csv(file.path(OUT_ABS, "lineage_edges.csv"))
for (cc in c("params_ref_p86_agemat.rds", "params_ref_p75_recal_capped.rds", "params_ref_p76_recal_rltargets.rds")) {
  x <- le[le$child == cc & !startsWith(le$key, "given_species_params"), c("parent", "key", "kind", "n_diff", "max_rel")]
  cat("----", cc, "<-", unique(x$parent), "\n"); print(x[, -1], row.names = FALSE)
}

# ---- (n) packages the notebooks need ----------------------------------------------------------
sec("(n) installed versions of the packages the notebooks load")
pk <- c("mizer","therMizer","mizerExperimental","mizerHowTo","mizerMR","solong","SOmap","openair","optimParallel","DEoptim","pbapply","abind","janitor","readxl","sf","lubridate","tidyverse","reshape2","Polychrome","magick","corrplot","tictoc","calibrar","plotly")
for (p in pk) cat(sprintf("%-18s %s\n", p, if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else "NOT INSTALLED"))
