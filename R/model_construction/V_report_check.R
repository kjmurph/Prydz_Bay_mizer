# V_report_check.R -- re-read every md5 and the cited line numbers in
# docs/model_construction_assessment.md from disk.
source(file.path(Sys.getenv("ASSESS_SRC", "R/model_construction"), "A0_helpers.R"))
id <- "V"
ln <- function(f, i) { x <- readLines(repo_path(f), warn = FALSE); if (i > length(x)) NA_character_ else x[i] }
lines_chk <- list(
  c("09_Uncertainty_Analysis.Rmd", 1105, "upgradeTherParams\\(params_new_v4"),
  c("09_Uncertainty_Analysis.Rmd", 1110, "aerobic_effect = FALSE"),
  c("09_Uncertainty_Analysis.Rmd", 1174, "yield_observed_timeseries_tidy.RDS"),
  c("09_Uncertainty_Analysis.Rmd", 1329, "catch_lengths.rds"),
  c("09_Uncertainty_Analysis.Rmd", 1444, "steady\\(params_sel_adj, tol = 0.002, t_max = 1000"),
  c("09_Uncertainty_Analysis.Rmd", 1477, "#saveRDS\\(params_sel_adj"),
  c("05_therMizer_calibration_scale_model_domain.Rmd", 54, "1.474341e\\+12"),
  c("05_therMizer_calibration_scale_model_domain.Rmd", 272, "2000:2010"),
  c("05_therMizer_calibration_scale_model_domain.Rmd", 302, "vertical_migration array set up"),
  c("05_therMizer_calibration_scale_model_domain.Rmd", 469, "aerobic_effect = TRUE"),
  c("05_therMizer_calibration_scale_model_domain.Rmd", 500, "temp_min <- c\\(-2"),
  c("05_therMizer_calibration_scale_model_domain.Rmd", 508, "-1.5, # \"flying birds\""),
  c("05_therMizer_calibration_scale_model_domain.Rmd", 546, "temp_min <- temp_min"),
  c("05_therMizer_calibration_scale_model_domain.Rmd", 574, "latest_therMizer_params.RDS"),
  c("05_therMizer_calibration_scale_model_domain.Rmd", 592, "tuneParams\\(params\\)"),
  c("05_therMizer_calibration_scale_model_domain.Rmd", 594, "therMizer_params_v03.RDS"),
  c("05_therMizer_calibration_scale_model_domain.Rmd", 647, "setBevertonHolt\\(params, reproduction_level"),
  c("05_therMizer_calibration_scale_model_domain.Rmd", 679, "therMizer_params_v04.RDS"),
  c("06_steady_state_therMizer.Rmd", 35, "params_for_use.RDS"),
  c("06_steady_state_therMizer.Rmd", 84, "effort_array_26_08_2024"),
  c("06_steady_state_therMizer.Rmd", 145, "initial_effort\\(params_new\\) <- 0"),
  c("06_steady_state_therMizer.Rmd", 152, "yield_observed <- 0"),
  c("06_steady_state_therMizer.Rmd", 173, "tuneParams\\(params_new_v2\\)"),
  c("06_steady_state_therMizer.Rmd", 189, "tuneParams\\(params_new_v3\\)"),
  c("06_steady_state_therMizer.Rmd", 312, "params_steady_state_2011_2020_tol_0.00025.RDS"),
  c("06_steady_state_therMizer.Rmd", 421, "initial_effort_1951_1960 <- c\\("),
  c("06_steady_state_therMizer.Rmd", 428, "0.2254233"),
  c("06_steady_state_therMizer.Rmd", 518, "effort_array_1841_2010.rds"),
  c("06_steady_state_therMizer.Rmd", 744, "yield_observed_timeseries_tidy.RDS"),
  c("06_steady_state_therMizer.Rmd", 909, "abind::abind"),
  c("06_steady_state_therMizer.Rmd", 1086, "isimip_plankton\\[1,\\] - colMeans"),
  c("06_steady_state_therMizer.Rmd", 1088, "isimip_plankton\\[t,\\]"),
  c("06_steady_state_therMizer.Rmd", 1091, "n_pp_array_26_08_2024"),
  c("06_steady_state_therMizer.Rmd", 1269, "aerobic_effect = FALSE"),
  c("06_steady_state_therMizer.Rmd", 2782, "params_pre_temp <- upgradeTherParams"),
  c("06_steady_state_therMizer.Rmd", 2785, "aerobic_effect = FALSE"),
  c("06_steady_state_therMizer.Rmd", 2796, "saveRDS\\(params_pre,\"params_for_use.RDS\"\\)"),
  c("02_Preparing_Climate_Forcings.Rmd", 115, "C_g_m2 = mol_C_m2 \\* 12.001"),
  c("02_Preparing_Climate_Forcings.Rmd", 116, "C_g = mol_C_m2\\*area_m2"),
  c("02_Preparing_Climate_Forcings.Rmd", 144, "phypico-vint_15arcmin_prydz-bay"),
  c("02_Preparing_Climate_Forcings.Rmd", 207, "phydiaz-vint_15arcmin_prydz-bay"),
  c("02_Preparing_Climate_Forcings.Rmd", 382, "w_full is 164"),
  c("02_Preparing_Climate_Forcings.Rmd", 576, "GFDL_resource_spectra_w_full_142.dat"),
  c("02_Preparing_Climate_Forcings.Rmd", 610, "FishMIP_Temperature_Forcing/gfdl-mom6-cobalt2_obsclim_tos"),
  c("02_Preparing_Climate_Forcings.Rmd", 612, "obsclim_tob"),
  c("02_Preparing_Climate_Forcings.Rmd", 723, "summarise\\(tos = mean\\(value\\)\\)"),
  c("02_Preparing_Climate_Forcings.Rmd", 790, "t500m = \\(tob_ - tos_\\)/4 \\+ tos_"),
  c("01_FishMIP_Fishing_Data.Rmd", 43, "effort_histsoc_1841_2010_regional_models.csv"),
  c("01_FishMIP_Fishing_Data.Rmd", 65, "total_catch = sum\\(Reported,IUU,Discards\\)"),
  c("01_FishMIP_Fishing_Data.Rmd", 325, "catch_timeseries.csv"),
  c("01_FishMIP_Fishing_Data.Rmd", 391, "yield_observed_timeseries.csv"),
  c("01_FishMIP_Fishing_Data.Rmd", 484, "filter\\(!effort < 1e-15\\)"),
  c("01_FishMIP_Fishing_Data.Rmd", 505, "effort_Prydz_Bay_1950_2010_resolution_v1.RDS"),
  c("03_model_setup_pre_therMizer.rmd", 156, "df_plot <- df_CPUE_kg_day"),
  c("03_model_setup_pre_therMizer.rmd", 164, "IWC_effort.csv"),
  c("04_therMizer_calibration_scale_g_m2.Rmd", 181, "params_fk <- setPredKernel"),
  c("04_therMizer_calibration_scale_g_m2.Rmd", 401, "Updating biomass_observed"),
  c("04_therMizer_calibration_scale_g_m2.Rmd", 403, "biomass_observed <- c\\(8.8"),
  c("04_therMizer_calibration_scale_g_m2.Rmd", 617, "params_latest_biomass_09_Aug_2023"),
  c("04_therMizer_calibration_scale_g_m2.Rmd", 702, "temp_min <- c\\(-2"),
  c("04_therMizer_calibration_scale_g_m2.Rmd", 756, "therMizer_params_v1.RDS"),
  c("04_therMizer_calibration_scale_g_m2.Rmd", 839, "effort_array.csv"),
  c("08_ISIMIP3a_simulations_Prydz_Bay.Rmd", 53, "params_07_06_2024.rds"),
  c("08_ISIMIP3a_simulations_Prydz_Bay.Rmd", 404, "gear_params\\(params\\)\\$gear <- gear_params\\(params\\)\\$species"),
  c("08_ISIMIP3a_simulations_Prydz_Bay.Rmd", 451, "catchability\\[c\\(7,8,12,16,19\\)\\]"),
  c("08_ISIMIP3a_simulations_Prydz_Bay.Rmd", 521, "params_04_06_2024_v3.rds"),
  c("08_ISIMIP3a_simulations_Prydz_Bay.Rmd", 542, "params_04_06_2024.rds"),
  c("optim_model_setup_old/model_setup_v4.Rmd", 104, "trait_groups_params_vCWC_v4.csv"),
  c("optim_model_setup_old/model_setup_v4.Rmd", 352, "trait_groups_interaction_matrix_vCWC_v4"),
  c("optim_model_setup_old/model_setup_v4.Rmd", 363, "theta\\[17,17\\] <- 0"),
  c("optim_model_setup_old/model_setup_v4.Rmd", 378, "newMultispeciesParams\\(species_params = groups"),
  c("optim_model_setup_old/model_setup_v4.Rmd", 380, "kappa = 2.63907e\\+13"),
  c("optim_model_setup_old/model_setup_v4.Rmd", 483, "ppmr_min.*baleen whales.*1e5"),
  c("optim_model_setup_old/model_setup_v4.Rmd", 506, "large divers.*0.9"),
  c("optim_model_setup_old/model_setup_v4.Rmd", 548, "kappa\\*params_guessed@species_params\\$w_max\\^-1.5"),
  c("optim_model_setup_old/model_setup_v4.Rmd", 651, "leopard seals.*<- 1000"),
  c("optim_model_setup_old/model_setup_v4.Rmd", 779, "theta\\[17,15\\] <- 0.6"),
  c("optim_model_setup_old/model_setup_v4.Rmd", 816, "539153.8"),
  c("optim_model_setup_old/model_setup_v4.Rmd", 846, "steady_phase1_group_params_v3.RDS"),
  c("optim_model_setup_old/model_setup_v4.Rmd", 1410, "steady_params_xx.RDS"),
  c("optim_model_setup_old/model_setup_v4.Rmd", 1423, "steady_params_x1.RDS"),
  c("optim_model_setup_old/model_setup_v4.Rmd", 1617, "params_latest_xx.RDS"),
  c("model_setup_old/New_steady_state.Rmd", 41, "params_04_06_2024_v4.rds"),
  c("model_setup_old/New_steady_state_07_06_2024.Rmd", 44, "erepro >= 1, 0.99"),
  c("model_setup_old/New_steady_state_07_06_2024.Rmd", 303, "tuneParams\\(params\\)"),
  c("model_setup_old/New_steady_state_07_06_2024.Rmd", 305, "saveRDS\\(params, \"params_07_06_2024.rds\"\\)"),
  c("group params/1g_simplified_groups_params.Rmd", 585, "trait_groups_params_vCWC_v4.csv"),
  c("interaction matrix/2g_trait_groups_interaction_matrix_vCWC.R", 161, "trait_groups_interaction_matrix_vCWC_v4.rds"),
  c("interaction matrix/2g_trait_groups_interaction_matrix_vCWC.R", 193, "set.seed\\(9641\\)"),
  c("00_Tidying IWC Southern Hemisphere data.Rmd", 160, "Ac==0"),
  c("00_Tidying IWC Southern Hemisphere data.Rmd", 187, "BanzareBank.rds"),
  c("00_Tidying IWC Southern Hemisphere data.Rmd", 193, "st_intersection"),
  c("00_Tidying IWC Southern Hemisphere data.Rmd", 343, "a = 0.0061"),
  c("00_Tidying IWC Southern Hemisphere data.Rmd", 447, "0.2080\\*\\(Length\\)\\^2.577"),
  c("00_Tidying IWC Southern Hemisphere data.Rmd", 522, "catch_timeseries_BanzareBank_1930_2019_CPUE.rds"))
nbad <- 0
for (k in lines_chk) {
  txt <- ln(k[1], as.integer(k[2]))
  ok <- !is.na(txt) && grepl(k[3], txt)
  if (!ok) { nbad <- nbad + 1; check(id, sprintf("%s:%s", k[1], k[2]), FALSE, substr(ifelse(is.na(txt), "<no line>", txt), 1, 100)) }
}
check(id, sprintf("cited line numbers: %d checked", length(lines_chk)), nbad == 0, paste(nbad, "mismatches"))

md5s <- list(
  c("params_sel_adj.rds", "88e6f9b2"), c("Manuscript data/params_sel_adj.rds", "88e6f9b2"),
  c("FishMIP_Plankton_Forcing/gfdl-mom6-cobalt2_obsclim_phydiat-vint_15arcmin_prydz-bay_monthly_1961_2010.csv", "e7c3ad9c"),
  c("FishMIP_Plankton_Forcing/gfdl-mom6-cobalt2_obsclim_phypico-vint_15arcmin_Prydz-Bay_monthly_1961_2010.csv", "865d3f11"),
  c("FishMIP_Plankton_Forcing/gfdl-mom6-cobalt2_obsclim_phydiaz-vint_15arcmin_Prydz-Bay_monthly_1961_2010.csv", "681007eb"),
  c("FishMIP_Plankton_Forcing/gfdl-mom6-cobalt2_obsclim_zmicro-vint_15arcmin_prydz-bay_monthly_1961_2010.csv", "5ae110b1"),
  c("FishMIP_Plankton_Forcing/gfdl-mom6-cobalt2_obsclim_zmeso-vint_15arcmin_prydz-bay_monthly_1961_2010.csv", "029ae357"),
  c("FishMIP_Plankton_Forcing/gfdl-mom6-cobalt2_obsclim_tos_15arcmin_prydz-bay_monthly_1961_2010.csv", "132736c6"),
  c("FishMIP_fishing_data/calibration_catch_histsoc_1850_2004_regional_models.csv", "5501b0aa"),
  c("FishMIP_fishing_data/effort_histsoc_1841_2010_regional_models_Prydz.Bay.csv", "50ff6cc8"),
  c("model_domains/Stacey/BanzareBank.rds", "7130f275"), c("IWC_data/catch_lengths.rds", "d9921024"),
  c("csvs/predator_parameters_updated.csv", "8950ee08"), c("csvs/fish_parameters_updated.csv", "566179c1"),
  c("csvs/squid_parameters_updated.csv", "7f489871"),
  c("tos_annual.rds", "fe6eb084"), c("t500m_annual.rds", "b0346f5e"), c("t1000m_annual.rds", "018b6d4d"),
  c("t1500m_annual.rds", "69278852"), c("tob_annual.rds", "f834e76e"),
  c("GFDL_resource_spectra_annual.dat", "1df9ba74"), c("effort_array.csv", "82922586"),
  c("yield_observed_timeseries.csv", "e48df259"), c("yield_observed_timeseries_tidy.RDS", "1c7ccb81"),
  c("group params/trait_groups_params_vCWC_v4.csv", "4749f46b"),
  c("interaction matrix/trait_groups_interaction_matrix_vCWC_v4.rds", "805f23c6"),
  c("temperature_forcing_1841_2010.rds", "94d60f87"), c("phytoplankton_forcing_1841_2010.rds", "32130254"),
  c("effort_array_1841_2010.rds", "f8f7f94c"))
mbad <- 0
for (k in md5s) {
  m <- unname(tools::md5sum(repo_path(k[1])))
  if (is.na(m) || !startsWith(m, k[2])) { mbad <- mbad + 1; check(id, paste("md5", k[1]), FALSE, paste(m, "vs", k[2])) }
}
hist_md5 <- unname(tools::md5sum(file.path(HIST, "catch_timeseries_BanzareBank_1930_2019_CPUE.rds")))
if (!startsWith(hist_md5, "b92bf815")) mbad <- mbad + 1
ms_repo <- Sys.getenv("ASSESS_MS_REPO", file.path(REPO, "..", "Prydz_Bay_mizer_manuscript"))
for (k in list(c("ensemble/inputs/mc_origin/params_sel_adj.rds", "88e6f9b2"), c("data/effort_array_1841_2010.rds", "f8f7f94c"),
               c("data/yield_observed_timeseries.csv", "e48df259"))) {
  m <- unname(tools::md5sum(file.path(ms_repo, k[1])))
  if (is.na(m) || !startsWith(m, k[2])) { mbad <- mbad + 1; check(id, paste("manuscript md5", k[1]), FALSE, paste(m, "vs", k[2])) }
}
check(id, sprintf("cited md5s: %d checked (incl. history copy and 3 manuscript-repo copies)", length(md5s) + 4), mbad == 0, paste(mbad, "mismatches"))
save_results(id)
