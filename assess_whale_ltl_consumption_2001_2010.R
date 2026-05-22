###############################################################################
# Whale LTL Consumption Assessment – 2001-2010 Period
#
# Assesses total lower trophic level (LTL) prey consumed by each whale group
# individually, and as aggregates (all whales, all large marine mammals) for
# the 2001-2010 period from both the exploited (fishing) and unexploited
# (climate-only) ensembles.
#
# Two-stage approach:
#   Stage 1 – Fast: Derive aggregated group results from existing cached
#             per-simulation time series (all_sims.rds files), filtering to
#             2001-2010. Covers: baleen group (baleen+minke combined),
#             all whales combined, seals combined.
#   Stage 2 – Targeted: Load raw ensembles and call getDiet only for
#             2001-2010 time steps to get individual species breakdowns:
#             "baleen whales" sp., "minke whales", "sperm whales", "orca",
#             and individual seal species.
#
# LTL prey categories (consistent with rest of codebase):
#   antarctic krill, other krill, mesozooplankton, other macrozooplankton, salps
#
# Outputs (saved to OUTPUT_DIR):
#   per_species_period_means_fishing.rds          – per-sim period means (fishing)
#   per_species_period_means_climate_only.rds     – per-sim period means (climate-only)
#   ltl_consumption_2001_2010_summary.csv         – ensemble statistics, all groups
#   ltl_consumption_2001_2010_by_prey.csv         – breakdown by individual LTL prey type
###############################################################################

library(therMizer)   # loads mizer; provides getDiet etc.
library(dplyr)
library(tidyr)

###############################################################################
# Configuration
###############################################################################

MODEL_DOMAIN_AREA <- 1.474341e+12  # m² (therMizer calibration domain, 05_therMizer_calibration_scale_model_domain.Rmd)
G_TO_TONNES       <- 1e-6

MODERN_YEARS <- 2001:2010

LTL_PREY <- c("antarctic krill", "other krill", "mesozooplankton",
               "other macrozooplankton", "salps")

# Individual predators to track (getDiet runs for all simultaneously, so no
# extra cost versus tracking fewer)
INDIVIDUAL_PREDATORS <- c(
  "baleen whales",   # large baleen – blue, fin, humpback etc.
  "minke whales",    # minke / Antarctic minke
  "sperm whales",
  "orca",
  "leopard seals",
  "small divers",    # e.g. fur seals / penguins
  "medium divers",
  "large divers"
)

# Convenient groupings for aggregation (evaluated after individual extraction)
PREDATOR_GROUPS <- list(
  baleen_whales    = c("baleen whales", "minke whales"),
  toothed_whales   = c("sperm whales", "orca"),
  all_whales       = c("baleen whales", "minke whales", "sperm whales", "orca"),
  seals            = c("leopard seals", "small divers", "medium divers", "large divers"),
  all_marine_mams  = c("baleen whales", "minke whales", "sperm whales", "orca",
                       "leopard seals", "small divers", "medium divers", "large divers")
)

# Ensemble file paths
ENSEMBLE_PATHS <- list(
  fishing      = "Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds",
  climate_only = "Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds"
)

# Existing cached data directory (Stage 1 reads from here)
CACHED_DIR <- "whale_consumption_outputs"

OUTPUT_DIR <- "whale_consumption_outputs"

###############################################################################
# Stage 1 – Derive grouped results from cached all_sims.rds files
#
# Each cached file is a list of data frames, one per simulation, with columns:
#   year, total_consumption  (consumption in g/year)
###############################################################################

#' Compute 2001-2010 period mean from a cached all_sims list
#'
#' @param rds_path  Path to the *_all_sims.rds file
#' @param years     Integer vector of years to average over (default MODERN_YEARS)
#' @return Data frame: sim_id, period_mean_g_yr (one row per simulation)
period_mean_from_cache <- function(rds_path, years = MODERN_YEARS) {
  if (!file.exists(rds_path)) {
    message("  Cached file not found: ", rds_path)
    return(NULL)
  }
  sim_list <- readRDS(rds_path)

  results <- lapply(seq_along(sim_list), function(i) {
    df <- sim_list[[i]]
    subset_df <- df[df$year %in% years, ]
    if (nrow(subset_df) == 0) return(NULL)
    data.frame(sim_id = i, period_mean_g_yr = mean(subset_df$total_consumption, na.rm = TRUE))
  })
  do.call(rbind, Filter(Negate(is.null), results))
}

#' Ensemble statistics from a vector of per-sim values
ensemble_stats <- function(values, n_sims_total = length(values)) {
  data.frame(
    n_sims     = length(values),
    median_g_yr = median(values, na.rm = TRUE),
    mean_g_yr   = mean(values, na.rm = TRUE),
    sd_g_yr     = sd(values, na.rm = TRUE),
    q05_g_yr    = quantile(values, 0.05, na.rm = TRUE),
    q25_g_yr    = quantile(values, 0.25, na.rm = TRUE),
    q75_g_yr    = quantile(values, 0.75, na.rm = TRUE),
    q95_g_yr    = quantile(values, 0.95, na.rm = TRUE),
    median_tonnes = median(values, na.rm = TRUE) * G_TO_TONNES,
    mean_tonnes   = mean(values, na.rm = TRUE)   * G_TO_TONNES,
    q05_tonnes    = quantile(values, 0.05, na.rm = TRUE) * G_TO_TONNES,
    q25_tonnes    = quantile(values, 0.25, na.rm = TRUE) * G_TO_TONNES,
    q75_tonnes    = quantile(values, 0.75, na.rm = TRUE) * G_TO_TONNES,
    q95_tonnes    = quantile(values, 0.95, na.rm = TRUE) * G_TO_TONNES
  )
}

cat("=============================================================\n")
cat("WHALE LTL CONSUMPTION – 2001-2010 PERIOD ASSESSMENT\n")
cat("=============================================================\n\n")

# --- Stage 1: aggregated groups from cache ------------------------------------
cat("Stage 1: Reading cached ensemble summaries...\n\n")

cached_groups <- list(
  baleen_whales   = list(label = "Baleen whales (baleen+minke combined)",
                          fishing      = file.path(CACHED_DIR, "fishing_baleen_ltl_all_sims.rds"),
                          climate_only = file.path(CACHED_DIR, "climate_only_baleen_ltl_all_sims.rds")),
  all_whales      = list(label = "All whales (baleen+minke+sperm+orca)",
                          fishing      = file.path(CACHED_DIR, "fishing_all_whales_ltl_all_sims.rds"),
                          climate_only = file.path(CACHED_DIR, "climate_only_all_whales_ltl_all_sims.rds")),
  seals           = list(label = "All seals (leopard+small+medium+large divers)",
                          fishing      = file.path(CACHED_DIR, "fishing_seals_ltl_all_sims.rds"),
                          climate_only = file.path(CACHED_DIR, "climate_only_seals_ltl_all_sims.rds"))
)

stage1_results <- list()

for (grp in names(cached_groups)) {
  info <- cached_groups[[grp]]
  cat(sprintf("  [%s]\n", info$label))

  for (scenario in c("fishing", "climate_only")) {
    df <- period_mean_from_cache(info[[scenario]])
    if (!is.null(df)) {
      key <- paste0(grp, "__", scenario)
      stage1_results[[key]] <- df
      stats <- ensemble_stats(df$period_mean_g_yr)
      cat(sprintf("    %-15s  n=%d  median=%8.0f t/yr  IQR=[%8.0f, %8.0f]\n",
                  scenario, stats$n_sims,
                  stats$median_tonnes, stats$q25_tonnes, stats$q75_tonnes))
    } else {
      cat(sprintf("    %-15s  SKIPPED (file not found)\n", scenario))
    }
  }
  cat("\n")
}

###############################################################################
# Stage 2 – Individual species extraction from raw ensemble
#
# For each simulation, extract getDiet only for 2001-2010 time steps.
# getDiet runs for ALL predators simultaneously so there is no per-species
# overhead – we simply slice the result by predator name.
#
# Returns: per-sim period means for every individual predator listed in
# INDIVIDUAL_PREDATORS, and also broken down by LTL prey category.
###############################################################################

#' Extract per-predator LTL consumption for 2001-2010 from one MizerSim
#'
#' @param sim        MizerSim object
#' @param years      Years to include (must exist in sim time axis)
#' @param predators  Character vector of predator names to track
#' @param ltl_prey   Character vector of prey names to sum over
#' @return Data frame: predator, prey, period_mean_g_yr
extract_individual_period_means <- function(sim, years = MODERN_YEARS,
                                             predators = INDIVIDUAL_PREDATORS,
                                             ltl_prey  = LTL_PREY) {
  params <- sim@params
  times  <- as.numeric(dimnames(sim@n)$time)
  dw     <- params@dw

  # Find time indices in this simulation matching the requested years
  t_indices <- which(times %in% years)
  if (length(t_indices) == 0) return(NULL)

  # Accumulate consumption across years, then divide by n_years at the end
  # (avoids storing year-by-year arrays)
  n_years <- length(t_indices)

  # Determine which predators exist in this model
  all_sp <- dimnames(sim@n)$sp
  valid_preds <- predators[predators %in% all_sp]
  if (length(valid_preds) == 0) return(NULL)

  # Accumulator: predator x ltl_prey  (sums over years)
  # Will be divided by n_years at the end
  accumulator <- matrix(0.0,
    nrow = length(valid_preds),
    ncol = length(ltl_prey),
    dimnames = list(predator = valid_preds, prey = ltl_prey)
  )

  for (t_idx in t_indices) {
    # Extract state at this time step
    n    <- sim@n[t_idx, , , drop = FALSE]
    dim(n) <- dim(sim@n)[2:3]
    dimnames(n) <- dimnames(sim@n)[2:3]

    n_pp    <- sim@n_pp[t_idx, ]
    n_other <- sim@n_other[t_idx, ]
    if (!is.null(dimnames(sim@n_other)))
      names(n_other) <- dimnames(sim@n_other)$component

    # getDiet returns g/yr per individual at each body-size bin
    # dimensions: predator x predator_size x prey
    diet <- tryCatch(
      getDiet(params, n = n, n_pp = n_pp, n_other = n_other, proportion = FALSE),
      error = function(e) NULL
    )
    if (is.null(diet)) next

    prey_in_model <- dimnames(diet)[[3]]
    valid_ltl     <- ltl_prey[ltl_prey %in% prey_in_model]

    for (pred in valid_preds) {
      pred_i <- which(dimnames(diet)$predator == pred)
      if (length(pred_i) == 0) next
      n_pred <- n[pred, ]

      # Integrate over predator size: sum_w( diet[pred,w,prey] * n[w] * dw )
      for (prey in valid_ltl) {
        prey_i <- which(prey_in_model == prey)
        accumulator[pred, prey] <- accumulator[pred, prey] +
          sum(diet[pred_i, , prey_i] * n_pred * dw)
      }
    }
  }

  # Convert accumulator to period means and tidy to data frame
  period_mean_mat <- accumulator / n_years

  df <- as.data.frame(as.table(period_mean_mat))
  names(df) <- c("predator", "prey", "period_mean_g_yr")
  df
}

#' Process one ensemble file and return per-sim, per-species period means
#'
#' @param ensemble_file  Path to the .rds ensemble file
#' @param label          Short label for progress messages
#' @return List with two elements:
#'   $by_prey   – data frame: sim_id, predator, prey, period_mean_g_yr
#'   $ltl_total – data frame: sim_id, predator, ltl_total_g_yr (summed over LTL prey)
process_ensemble_individual <- function(ensemble_file, label) {
  if (!file.exists(ensemble_file)) {
    message("Ensemble file not found: ", ensemble_file)
    return(NULL)
  }

  cat(sprintf("\n  Loading %s ensemble...\n", label))
  mc <- readRDS(ensemble_file)

  # Handle different ensemble structures (consistent with existing code)
  if ("simulations" %in% names(mc)) {
    sims <- mc$simulations
  } else if (is.list(mc) && inherits(mc[[1]], "MizerSim")) {
    sims <- mc
  } else {
    stop("Unrecognised ensemble structure in: ", ensemble_file)
  }

  n_sims <- length(sims)
  cat(sprintf("  %d simulations found. Extracting 2001-2010 only...\n", n_sims))

  by_prey_rows   <- list()
  ltl_total_rows <- list()
  skipped <- 0L

  pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
  for (i in seq_len(n_sims)) {
    setTxtProgressBar(pb, i)

    sim <- tryCatch(sims[[i]], error = function(e) NULL)
    if (is.null(sim) || !inherits(sim, "MizerSim")) { skipped <- skipped + 1L; next }

    result <- tryCatch(
      extract_individual_period_means(sim),
      error = function(e) NULL
    )
    if (is.null(result) || nrow(result) == 0) { skipped <- skipped + 1L; next }

    result$sim_id <- i
    by_prey_rows[[length(by_prey_rows) + 1]] <- result

    # Sum over LTL prey to get total LTL per predator
    ltl_totals <- result %>%
      group_by(predator) %>%
      summarise(ltl_total_g_yr = sum(period_mean_g_yr, na.rm = TRUE), .groups = "drop") %>%
      mutate(sim_id = i)
    ltl_total_rows[[length(ltl_total_rows) + 1]] <- ltl_totals
  }
  close(pb)
  cat(sprintf("\n  Processed %d / %d sims  (skipped %d)\n", n_sims - skipped, n_sims, skipped))

  list(
    by_prey   = do.call(rbind, by_prey_rows),
    ltl_total = do.call(rbind, ltl_total_rows)
  )
}

###############################################################################
# Run Stage 2 for both ensembles
###############################################################################

cat("\n=============================================================\n")
cat("Stage 2: Individual species extraction (2001-2010 only)...\n")
cat("=============================================================\n")

stage2 <- list()

for (scenario in c("fishing", "climate_only")) {
  label  <- ifelse(scenario == "fishing", "Fishing", "Climate-Only (Unfished)")
  result <- process_ensemble_individual(ENSEMBLE_PATHS[[scenario]], label)

  if (!is.null(result)) {
    stage2[[scenario]] <- result

    # Save raw per-sim period means
    saveRDS(result, file.path(OUTPUT_DIR, sprintf("per_species_period_means_%s.rds", scenario)))
    cat(sprintf("  Saved: per_species_period_means_%s.rds\n", scenario))
  }
}

###############################################################################
# Compute ensemble statistics
###############################################################################

cat("\n=============================================================\n")
cat("Computing ensemble statistics...\n")
cat("=============================================================\n\n")

# Build tidy summary table rows
summary_rows <- list()

add_summary_row <- function(scenario, group_label, species_label, values) {
  if (length(values) == 0 || all(is.na(values))) return()
  stats <- ensemble_stats(values)
  summary_rows[[length(summary_rows) + 1]] <<- data.frame(
    scenario       = scenario,
    predator_group = group_label,
    predator       = species_label,
    stringsAsFactors = FALSE
  ) %>% cbind(stats)
}

# --- Stage 1 groups (from cache) ---
for (grp in names(cached_groups)) {
  label <- cached_groups[[grp]]$label
  for (scenario in c("fishing", "climate_only")) {
    key <- paste0(grp, "__", scenario)
    if (!is.null(stage1_results[[key]])) {
      add_summary_row(scenario, grp, label, stage1_results[[key]]$period_mean_g_yr)
    }
  }
}

# --- Stage 2 individual species ---
for (scenario in names(stage2)) {
  ltl_df <- stage2[[scenario]]$ltl_total
  if (is.null(ltl_df)) next

  # Individual predator rows
  for (pred in unique(ltl_df$predator)) {
    vals <- ltl_df$ltl_total_g_yr[ltl_df$predator == pred]
    add_summary_row(scenario, "individual_species", pred, vals)
  }

  # Compute aggregated groups from Stage 2 individual data
  for (grp_name in names(PREDATOR_GROUPS)) {
    members <- PREDATOR_GROUPS[[grp_name]]
    grp_df  <- ltl_df %>%
      filter(predator %in% members) %>%
      group_by(sim_id) %>%
      summarise(ltl_total_g_yr = sum(ltl_total_g_yr, na.rm = TRUE), .groups = "drop")
    if (nrow(grp_df) > 0) {
      add_summary_row(scenario, grp_name,
                      paste0("[", grp_name, "] aggregate"),
                      grp_df$ltl_total_g_yr)
    }
  }
}

summary_df <- do.call(rbind, summary_rows)

# Print table
cat("LTL Consumption 2001-2010 (tonnes/year), ensemble statistics:\n")
cat("==============================================================\n")
for (scen in c("fishing", "climate_only")) {
  cat(sprintf("\n  Scenario: %s\n", toupper(scen)))
  cat(sprintf("  %-50s  %8s  %10s  [%10s – %10s]\n",
              "Predator", "n_sims", "Median t/yr", "q25", "q75"))
  cat(sprintf("  %s\n", strrep("-", 100)))
  sub <- summary_df[summary_df$scenario == scen, ]
  sub <- sub[order(sub$predator_group, sub$predator), ]
  for (j in seq_len(nrow(sub))) {
    cat(sprintf("  %-50s  %8d  %10.0f  [%10.0f – %10.0f]\n",
                sub$predator[j], sub$n_sims[j],
                sub$median_tonnes[j], sub$q25_tonnes[j], sub$q75_tonnes[j]))
  }
}

###############################################################################
# Prey-type breakdown table (by individual LTL category)
###############################################################################

prey_summary_rows <- list()

for (scenario in names(stage2)) {
  by_prey_df <- stage2[[scenario]]$by_prey
  if (is.null(by_prey_df)) next

  prey_stats <- by_prey_df %>%
    group_by(predator, prey, sim_id) %>%
    summarise(period_mean_g_yr = sum(period_mean_g_yr, na.rm = TRUE), .groups = "drop") %>%
    group_by(predator, prey) %>%
    summarise(
      n_sims        = n(),
      median_g_yr   = median(period_mean_g_yr, na.rm = TRUE),
      mean_g_yr     = mean(period_mean_g_yr, na.rm = TRUE),
      q25_g_yr      = quantile(period_mean_g_yr, 0.25, na.rm = TRUE),
      q75_g_yr      = quantile(period_mean_g_yr, 0.75, na.rm = TRUE),
      median_tonnes = median(period_mean_g_yr, na.rm = TRUE) * G_TO_TONNES,
      mean_tonnes   = mean(period_mean_g_yr, na.rm = TRUE)   * G_TO_TONNES,
      q25_tonnes    = quantile(period_mean_g_yr, 0.25, na.rm = TRUE) * G_TO_TONNES,
      q75_tonnes    = quantile(period_mean_g_yr, 0.75, na.rm = TRUE) * G_TO_TONNES,
      .groups = "drop"
    ) %>%
    mutate(scenario = scenario)

  prey_summary_rows[[scenario]] <- prey_stats
}

prey_summary_df <- do.call(rbind, prey_summary_rows)

###############################################################################
# Fishing impact: fished vs unfished comparison
###############################################################################

cat("\n\n  Fishing impact (fishing vs climate-only):\n")
cat(sprintf("  %-50s  %12s  %12s  %10s\n", "Predator group", "Fishing (t/yr)", "Unfished (t/yr)", "% of unfished"))
cat(sprintf("  %s\n", strrep("-", 90)))

for (pred_lbl in unique(summary_df$predator)) {
  f_row  <- summary_df[summary_df$predator == pred_lbl & summary_df$scenario == "fishing", ]
  co_row <- summary_df[summary_df$predator == pred_lbl & summary_df$scenario == "climate_only", ]
  if (nrow(f_row) == 0 || nrow(co_row) == 0) next
  pct <- if (co_row$median_tonnes > 0) f_row$median_tonnes / co_row$median_tonnes * 100 else NA
  cat(sprintf("  %-50s  %12.0f  %12.0f  %9.1f%%\n",
              pred_lbl, f_row$median_tonnes, co_row$median_tonnes, pct))
}

###############################################################################
# Save outputs
###############################################################################

cat("\n\nSaving outputs...\n")

# Main summary CSV
write.csv(summary_df, file.path(OUTPUT_DIR, "ltl_consumption_2001_2010_summary.csv"),
          row.names = FALSE)
cat("  Saved: ltl_consumption_2001_2010_summary.csv\n")

# Prey breakdown CSV
if (!is.null(prey_summary_df) && nrow(prey_summary_df) > 0) {
  write.csv(prey_summary_df, file.path(OUTPUT_DIR, "ltl_consumption_2001_2010_by_prey.csv"),
            row.names = FALSE)
  cat("  Saved: ltl_consumption_2001_2010_by_prey.csv\n")
}

cat("\n=============================================================\n")
cat("ASSESSMENT COMPLETE\n")
cat("=============================================================\n\n")
cat("Key outputs:\n")
cat("  ltl_consumption_2001_2010_summary.csv   - ensemble statistics per predator\n")
cat("  ltl_consumption_2001_2010_by_prey.csv   - breakdown by LTL prey category\n")
cat("  per_species_period_means_fishing.rds     - raw per-sim data (fishing)\n")
cat("  per_species_period_means_climate_only.rds- raw per-sim data (climate-only)\n")
