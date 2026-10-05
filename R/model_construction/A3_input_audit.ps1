# A3_input_audit.ps1 -- md5, size and git status of every input on the lineage
# of params_sel_adj.rds, plus the searches behind the report's "missing",
# "history only" and "no producer" statements. Read-only.
#
# Run from the repository root:
#   pwsh -NoProfile -ExecutionPolicy Bypass -File R/model_construction/A3_input_audit.ps1
# Writes input_audit.csv to $env:ASSESS_OUT (default Output_large_files/model_construction).

$Out = if ($env:ASSESS_OUT) { $env:ASSESS_OUT } else { 'Output_large_files/model_construction' }
New-Item -ItemType Directory -Force -Path $Out | Out-Null

# ---- 1. md5, size and git status of the inputs (report section 4) -------------
$paths = @(
 'FishMIP_Plankton_Forcing/gfdl-mom6-cobalt2_obsclim_phydiat-vint_15arcmin_prydz-bay_monthly_1961_2010.csv',
 'FishMIP_Plankton_Forcing/gfdl-mom6-cobalt2_obsclim_phypico-vint_15arcmin_Prydz-Bay_monthly_1961_2010.csv',
 'FishMIP_Plankton_Forcing/gfdl-mom6-cobalt2_obsclim_phydiaz-vint_15arcmin_Prydz-Bay_monthly_1961_2010.csv',
 'FishMIP_Plankton_Forcing/gfdl-mom6-cobalt2_obsclim_zmicro-vint_15arcmin_prydz-bay_monthly_1961_2010.csv',
 'FishMIP_Plankton_Forcing/gfdl-mom6-cobalt2_obsclim_zmeso-vint_15arcmin_prydz-bay_monthly_1961_2010.csv',
 'FishMIP_Plankton_Forcing/gfdl-mom6-cobalt2_obsclim_tos_15arcmin_prydz-bay_monthly_1961_2010.csv',
 'FishMIP_Plankton_Forcing/GFDL_resource_spectra.dat',
 'FishMIP_fishing_data/calibration_catch_histsoc_1850_2004_regional_models.csv',
 'FishMIP_fishing_data/effort_histsoc_1841_2010_regional_models_Prydz.Bay.csv',
 'model_domains/Stacey/BanzareBank.rds',
 'IWC_data/catch_lengths.rds',
 'csvs/predator_parameters_updated.csv','csvs/fish_parameters_updated.csv','csvs/squid_parameters_updated.csv',
 'group params/trait_groups_params_vCWC_v4.csv',
 'interaction matrix/trait_groups_interaction_matrix_vCWC_v4.rds',
 'tos_annual.rds','t500m_annual.rds','t1000m_annual.rds','t1500m_annual.rds','tob_annual.rds',
 'GFDL_resource_spectra_annual.dat','effort_array.csv','yield_observed_timeseries.csv','yield_observed_timeseries_tidy.RDS',
 'temperature_forcing_1841_2010.rds','phytoplankton_forcing_1841_2010.rds','constant_array_temp.RDS','constant_array_temp_1841.RDS','constant_array_n_pp.RDS','constant_array_n_pp_1841.RDS','effort_array_1841_2010.rds',
 'params/steady_phase1_group_params_v3.RDS','params/steady_params_xx.RDS','params/steady_params_x1.RDS','params/params_latest_xx.RDS','params/params_latest_biomass_09_Aug_2023.RDS','params/therMizer_params_v1.RDS','params/therMizer_params_total_area.RDS','params/latest_therMizer_params.RDS','params/therMizer_params_v03.RDS','params/therMizer_params_v04.RDS','params/params_04_06_2024.rds','params/params_04_06_2024_v3.rds','params/params_04_06_2024_v4.rds','params/params_07_06_2024.rds',
 'params_for_use.RDS','params_steady_state_2011_2020.RDS','params_steady_state_2011_2020_tol_0.00025.RDS','params_sel_adj.rds',
 'Manuscript data/params_sel_adj.rds','Manuscript data/yield_observed_timeseries.csv','data/effort_array_1841_2010.rds')
$rows = foreach ($p in $paths) {
  $exists = Test-Path -LiteralPath $p
  $md5 = ''; $size = ''
  if ($exists) { $md5 = (Get-FileHash -LiteralPath $p -Algorithm MD5).Hash.ToLower(); $size = (Get-Item -LiteralPath $p).Length }
  $tr = git ls-files -- "$p"
  $tracked = if ($tr) { 'tracked' } else { '' }
  $ignored = ''
  if ($exists) { git check-ignore -q -- "$p" 2>$null; if ($LASTEXITCODE -eq 0) { $ignored = 'ignored' } }
  $hist = git log --all --format='%h %ad' --date=short -1 -- "$p"
  [pscustomobject]@{ path = $p; exists = $exists; bytes = $size; md5 = $md5; git = (@($tracked, $ignored) | Where-Object { $_ }) -join '+'; last_commit = "$hist" }
}
$rows | Export-Csv -NoTypeInformation -Path (Join-Path $Out 'input_audit.csv')
$rows | ForEach-Object { "{0,-96} {1,-5} {2,11} {3} {4,-8} {5}" -f $_.path, $_.exists, $_.bytes, $_.md5, $_.git, $_.last_commit }
# (data/effort_array_1841_2010.rds lives in the manuscript worktree, not here;
#  its history hit is that branch's commit, because the two share one .git)

# ---- 2. inputs the notebooks name that exist nowhere on disk ----------------------
"`n==== named inputs: present anywhere on disk (excluding .git)?"
foreach ($n in @('catch_timeseries.csv','fish_catch.csv','IWC_effort.csv','effort_histsoc_1841_2010_regional_models.csv',
                 'effort_Prydz_Bay_1950_2010_resolution_v1.RDS','effort_array_26_08_2024.RDS','effort_array_full_26_08_2024.RDS',
                 'n_pp_array_26_08_2024.RDS','SHP1.csv','SHP2.csv','SHL.csv','SU.csv',
                 'gfdl-mom6-cobalt2_obsclim_tob_15arcmin_prydz-bay_monthly_1961_2010.csv')) {
  $hits = @(Get-ChildItem -Recurse -File -Filter $n -ErrorAction SilentlyContinue | Where-Object { $_.FullName -notmatch '\\\.git\\' })
  if ($hits.Count -eq 0) { "   {0,-75} MISSING" -f $n } else { $hits | ForEach-Object { "   {0,-75} {1}" -f $n, $_.FullName.Substring((Get-Location).Path.Length + 1) } }
}

# ---- 3. which of them ever existed in git history ----------------------------------
"`n==== paths ever in git history"
$pats = @('IWC data/','IWC_data/','FishMIP_Temperature_Forcing/','FishMIP_fishing_data/','catch_timeseries','fish_catch.csv','IWC_effort','tob_annual','temperature_forcing_1841_2010','phytoplankton_forcing_1841_2010','effort_array_1841_2010','params_steady_state_2011_2020','params_sel_adj.rds','GFDL_resource_spectra_annual')
$all = git log --all --name-only --format="" 2>$null | Sort-Object -Unique
foreach ($p in $pats) { $m = @($all | Where-Object { $_ -like "*$p*" }); "   ---- $p : $($m.Count) path(s)"; $m | Select-Object -First 15 | ForEach-Object { "      $_" } }

"`n==== add/delete history of the IWC and FishMIP intermediates"
foreach ($p in @('catch_timeseries_BanzareBank_1930_2019_CPUE.rds','catch_timeseries_BanzareBank_1930_2019.csv','fish_catch.csv','ind_catch_weight_BanzareBank_1930_2019_CPUE.rds','ind_catch_weight_BanzareBank_1930_2019.rds','FishMIP_fishing_data/catch_timeseries.csv','catch_timeseries.csv','FishMIP_fishing_data/IWC_effort.csv','IWC_effort.csv','FishMIP_fishing_data/effort_array.csv')) {
  "   ---- $p"
  git log --all --format='   %h %ad %s' --date=short --name-status -- "$p" | Select-String -Pattern '^\s+[0-9a-f]{7}|^[ADMR]\s' | Select-Object -First 8 | ForEach-Object { "      " + $_.Line.Trim() }
}

# ---- 4. the 30 -> 86 bridge: is there a producer for p75, p76 or p86? -------------------
"`n==== files named *86_* / *75_* / *76_* / *agemat* (excluding Output_large_files)"
Get-ChildItem -Recurse -File -Include '*86_*','*75_*','*76_*','*agemat*' -ErrorAction SilentlyContinue | Where-Object { $_.FullName -notmatch 'Output_large_files|\\\.git\\|- Copy' } | Select-Object -First 30 | ForEach-Object { "   " + $_.FullName.Substring((Get-Location).Path.Length + 1) }
"`n==== any script that saves p75 / p76 / p86"
$sv = @(Get-ChildItem -Recurse -File -Include '*.R','*.Rmd','*.rmd' -ErrorAction SilentlyContinue | Where-Object { $_.FullName -notmatch 'Output_large_files|\\\.git\\' } | Select-String -Pattern 'saveRDS\(.*(p86_agemat|p75_recal|p76_recal)')
if ($sv.Count -eq 0) { "   none" } else { $sv | ForEach-Object { "   " + $_.Path + ":" + $_.LineNumber } }
"`n==== paths in git history matching a phase-75/76/84/86 script or agemat"
git log --all --name-only --format="" | Sort-Object -Unique | Where-Object { $_ -match 'agemat|wmin_test/(75|76|84|86)_' } | ForEach-Object { "   $_" }
"`n==== first commit of params_ref_p86_agemat.rds"
git log --all --diff-filter=A --format='   %h %ad %s' --date=iso -- params_ref_p86_agemat.rds
