# ===== MONTE CARLO ANALYSIS HELPER FUNCTIONS =====
# Functions to analyze parameter distributions and model outputs from
# memory-efficient Monte Carlo simulations
# 
# Usage:
#   source("mc_analysis_helpers.R")
#   results <- load_and_prepare_mc_results("mc_10k_blocks")
#   plots <- analyze_mc_results(results, obs_biomass_data, yield_ts_tidy)

# ===== LOAD REQUIRED LIBRARIES =====
if (!require(mizer, quietly = TRUE)) stop("Package 'mizer' is required")
if (!require(ggplot2, quietly = TRUE)) stop("Package 'ggplot2' is required")
if (!require(dplyr, quietly = TRUE)) stop("Package 'dplyr' is required")
if (!require(tidyr, quietly = TRUE)) stop("Package 'tidyr' is required")
if (!require(scales, quietly = TRUE)) stop("Package 'scales' is required")
if (!require(reshape2, quietly = TRUE)) stop("Package 'reshape2' is required")

# ===== HELPER FUNCTION: LOAD PARAMETERS FROM BLOCKS =====

#' Load all parameters from block files
#' 
#' @param base_save_path Path to directory containing block files
#' @param type Type of parameters to load: "successful", "attempted", or "failed"
#' @return List of parameter sets
load_all_parameters <- function(base_save_path = "mc_blocks", type = "successful") {
  master_file <- file.path(base_save_path, "master_index.rds")
  master <- readRDS(master_file)
  
  if (master$run_info$memory_mode == "full") {
    # Data already in master index
    if (type == "successful") return(master$all_successful_parameters)
    if (type == "attempted") return(master$all_attempted_parameters)
    if (type == "failed") return(master$all_failed_parameters)
  }
  
  # Load from block files - reconstruct paths using current base_save_path
  cat("Loading", type, "parameters from", length(master$block_files), "block files...\n")
  all_params <- list()
  param_count <- 0
  
  for (block_file in master$block_files) {
    # Reconstruct path using current base_save_path and just the filename
    block_filename <- basename(block_file)
    actual_block_file <- file.path(base_save_path, block_filename)
    
    if (file.exists(actual_block_file)) {
      block_data <- readRDS(actual_block_file)
      
      target_data <- switch(type,
                            "successful" = block_data$successful_parameters,
                            "attempted" = block_data$all_attempted_parameters,
                            "failed" = block_data$failed_parameters
      )
      
      if (!is.null(target_data)) {
        for (param in target_data) {
          param_count <- param_count + 1
          all_params[[param_count]] <- param
        }
      }
    }
  }
  
  cat("Loaded", param_count, "parameter sets\n")
  return(all_params)
}

# ===== CORE DATA LOADING FUNCTIONS =====

#' Load and prepare Monte Carlo results from block files
#' 
#' @param base_save_path Path to directory containing master_index.rds and block files
#' @param load_simulations Whether to load full MizerSim objects (memory intensive)
#' @param verbose Print progress messages
#' @return List containing master index, parameters, and optionally simulations
load_and_prepare_mc_results <- function(base_save_path = "mc_10k_blocks",
                                        load_simulations = FALSE,
                                        verbose = TRUE) {
  
  if (verbose) cat("=== LOADING MONTE CARLO RESULTS ===\n")
  
  # Load master index
  master_file <- file.path(base_save_path, "master_index.rds")
  if (!file.exists(master_file)) {
    stop("Master index not found: ", master_file)
  }
  
  master <- readRDS(master_file)
  
  if (verbose) {
    cat("Run info:\n")
    cat("  Total simulations:", master$run_info$total_sims, "\n")
    cat("  Successful:", master$run_info$n_successful, "\n")
    cat("  Success rate:", round(master$run_info$success_rate * 100, 1), "%\n")
    cat("  Memory mode:", master$run_info$memory_mode, "\n")
    cat("  Number of blocks:", master$run_info$n_blocks_completed, "\n\n")
  }
  
  # Validate block files - check with corrected paths
  corrected_block_files <- sapply(master$block_files, function(f) {
    file.path(base_save_path, basename(f))
  })
  missing_files <- corrected_block_files[!file.exists(corrected_block_files)]
  if (length(missing_files) > 0) {
    warning("Missing ", length(missing_files), " block files: ", 
            paste(basename(missing_files), collapse = ", "))
  }
  
  # Load parameters
  if (verbose) cat("Loading successful parameters...\n")
  successful_params <- load_all_parameters(base_save_path, type = "successful")
  
  if (verbose) cat("Loading all attempted parameters...\n")
  all_attempted_params <- load_all_parameters(base_save_path, type = "attempted")
  
  if (verbose) cat("Loading failed simulation details...\n")
  failed_params <- load_all_parameters(base_save_path, type = "failed")
  
  result <- list(
    master = master,
    successful_params = successful_params,
    all_attempted_params = all_attempted_params,
    failed_params = failed_params,
    base_save_path = base_save_path
  )
  
  # Optionally load full simulations
  if (load_simulations) {
    if (verbose) {
      cat("\nLoading full MizerSim objects (this may take several minutes)...\n")
      cat("WARNING: This will use substantial memory (~", 
          round(master$run_info$n_successful * 50 / 1024, 1), "GB)\n")
    }
    
    result$simulations <- load_all_simulations(base_save_path)
    
    if (verbose) {
      cat("Loaded", length(result$simulations), "simulation objects\n")
    }
  }
  
  if (verbose) cat("\n=== LOADING COMPLETE ===\n\n")
  
  return(result)
}

#' Extract parameter distributions to tidy data frame
#' 
#' @param params_list List of parameter combinations from MC results
#' @param param_type Type of parameters ("successful", "attempted", or "failed")
#' @return Data frame with one row per species per simulation
extract_parameter_df <- function(params_list, param_type = "successful") {
  
  param_df_list <- lapply(seq_along(params_list), function(i) {
    p <- params_list[[i]]
    
    # Handle different structures for attempted vs successful parameters
    if (param_type == "attempted") {
      # Attempted parameters have nested structure
      if (is.null(p$species_names)) return(NULL)
      
      data.frame(
        sim_id = p$sim_id,
        species = p$species_names,
        gamma_change = if (!is.null(p$multiplicative_factors$gamma)) {
          p$multiplicative_factors$gamma
        } else rep(NA, length(p$species_names)),
        abundance_scaling = if (!is.null(p$multiplicative_factors$abundance_scaling)) {
          p$multiplicative_factors$abundance_scaling
        } else rep(NA, length(p$species_names)),
        # Catchability is per gear, not per species
        stringsAsFactors = FALSE
      )
      
    } else if (param_type == "failed") {
      # Failed params are in attempted_parameters sub-element
      if (is.null(p$attempted_parameters)) return(NULL)
      ap <- p$attempted_parameters
      
      if (is.null(ap$species_names)) return(NULL)
      
      data.frame(
        sim_id = ap$sim_id,
        species = ap$species_names,
        gamma_change = if (!is.null(ap$multiplicative_factors$gamma)) {
          ap$multiplicative_factors$gamma
        } else rep(NA, length(ap$species_names)),
        abundance_scaling = if (!is.null(ap$multiplicative_factors$abundance_scaling)) {
          ap$multiplicative_factors$abundance_scaling
        } else rep(NA, length(ap$species_names)),
        failure_reason = p$error,
        stringsAsFactors = FALSE
      )
      
    } else {
      # Successful parameters have direct structure
      if (is.null(p$species_names)) return(NULL)
      
      data.frame(
        sim_id = if (!is.null(p$sim_id)) p$sim_id else i,
        species = p$species_names,
        gamma_change = if (!is.null(p$gamma_change)) {
          p$gamma_change
        } else rep(NA, length(p$species_names)),
        abundance_scaling = if (!is.null(p$abundance_scaling)) {
          p$abundance_scaling
        } else rep(NA, length(p$species_names)),
        max_cv = if (!is.null(p$max_cv)) p$max_cv else NA,
        stringsAsFactors = FALSE
      )
    }
  })
  
  # Remove NULL entries and combine
  param_df_list <- param_df_list[!sapply(param_df_list, is.null)]
  
  if (length(param_df_list) == 0) {
    warning("No valid parameter data found")
    return(data.frame())
  }
  
  do.call(rbind, param_df_list)
}

#' Extract catchability parameters
#' 
#' @param params_list List of parameter combinations
#' @param param_type Type of parameters
#' @return Data frame with catchability by gear
extract_catchability_df <- function(params_list, param_type = "successful") {
  
  catch_df_list <- lapply(seq_along(params_list), function(i) {
    p <- params_list[[i]]
    
    # Get catchability change values
    if (param_type == "attempted") {
      if (is.null(p$multiplicative_factors$catchability)) return(NULL)
      catch_change <- p$multiplicative_factors$catchability
      sim_id <- p$sim_id
      
    } else if (param_type == "failed") {
      if (is.null(p$attempted_parameters)) return(NULL)
      if (is.null(p$attempted_parameters$multiplicative_factors$catchability)) return(NULL)
      catch_change <- p$attempted_parameters$multiplicative_factors$catchability
      sim_id <- p$attempted_parameters$sim_id
      
    } else {
      if (is.null(p$catchability_change)) return(NULL)
      catch_change <- p$catchability_change
      sim_id <- if (!is.null(p$sim_id)) p$sim_id else i
    }
    
    data.frame(
      sim_id = sim_id,
      gear_index = seq_along(catch_change),
      catchability_change = catch_change,
      stringsAsFactors = FALSE
    )
  })
  
  catch_df_list <- catch_df_list[!sapply(catch_df_list, is.null)]
  
  if (length(catch_df_list) == 0) {
    return(data.frame())
  }
  
  do.call(rbind, catch_df_list)
}

# ===== PARAMETER DISTRIBUTION ANALYSIS =====

#' Compare prior and posterior parameter distributions
#' 
#' @param mc_results Results from load_and_prepare_mc_results()
#' @param catchability_sd SD used for catchability in MC
#' @param abundance_sd SD used for abundance in MC
#' @param gamma_sd SD used for gamma in MC
#' @return List of data frames for plotting
analyze_parameter_distributions <- function(mc_results,
                                           catchability_sd = 2,
                                           abundance_sd = 4,
                                           gamma_sd = 2) {
  
  cat("=== ANALYZING PARAMETER DISTRIBUTIONS ===\n")
  
  # Extract parameter data frames
  cat("Extracting successful parameters...\n")
  successful_df <- extract_parameter_df(mc_results$successful_params, "successful")
  
  cat("Extracting all attempted parameters...\n")
  attempted_df <- extract_parameter_df(mc_results$all_attempted_params, "attempted")
  
  # Add distribution type labels
  successful_df$distribution <- "Posterior (Accepted)"
  attempted_df$distribution <- "Prior (All Attempted)"
  
  # Combine for comparison
  gamma_comparison <- rbind(
    successful_df[, c("sim_id", "species", "gamma_change", "distribution")],
    attempted_df[, c("sim_id", "species", "gamma_change", "distribution")]
  )
  
  abundance_comparison <- rbind(
    successful_df[, c("sim_id", "species", "abundance_scaling", "distribution")],
    attempted_df[, c("sim_id", "species", "abundance_scaling", "distribution")]
  )
  
  # Extract catchability
  cat("Extracting catchability parameters...\n")
  successful_catch <- extract_catchability_df(mc_results$successful_params, "successful")
  attempted_catch <- extract_catchability_df(mc_results$all_attempted_params, "attempted")
  
  successful_catch$distribution <- "Posterior (Accepted)"
  attempted_catch$distribution <- "Prior (All Attempted)"
  
  catchability_comparison <- rbind(successful_catch, attempted_catch)
  
  # Summary statistics
  cat("\n=== SUMMARY STATISTICS ===\n")
  
  # Gamma
  cat("\nGamma (search rate) changes:\n")
  gamma_summary <- successful_df %>%
    group_by(species) %>%
    summarise(
      median = median(gamma_change, na.rm = TRUE),
      mean = mean(gamma_change, na.rm = TRUE),
      q05 = quantile(gamma_change, 0.05, na.rm = TRUE),
      q95 = quantile(gamma_change, 0.95, na.rm = TRUE),
      .groups = 'drop'
    )
  print(gamma_summary)
  
  # Abundance
  cat("\nAbundance scaling:\n")
  abundance_summary <- successful_df %>%
    group_by(species) %>%
    summarise(
      median = median(abundance_scaling, na.rm = TRUE),
      mean = mean(abundance_scaling, na.rm = TRUE),
      q05 = quantile(abundance_scaling, 0.05, na.rm = TRUE),
      q95 = quantile(abundance_scaling, 0.95, na.rm = TRUE),
      .groups = 'drop'
    )
  print(abundance_summary)
  
  # Catchability
  if (nrow(successful_catch) > 0) {
    cat("\nCatchability changes:\n")
    catchability_summary <- successful_catch %>%
      group_by(gear_index) %>%
      summarise(
        median = median(catchability_change, na.rm = TRUE),
        mean = mean(catchability_change, na.rm = TRUE),
        q05 = quantile(catchability_change, 0.05, na.rm = TRUE),
        q95 = quantile(catchability_change, 0.95, na.rm = TRUE),
        .groups = 'drop'
      )
    print(catchability_summary)
  }
  
  list(
    gamma_comparison = gamma_comparison,
    abundance_comparison = abundance_comparison,
    catchability_comparison = catchability_comparison,
    gamma_summary = gamma_summary,
    abundance_summary = abundance_summary,
    catchability_summary = if (nrow(successful_catch) > 0) catchability_summary else NULL,
    successful_df = successful_df,
    attempted_df = attempted_df
  )
}

# ===== PLOTTING FUNCTIONS =====

#' Plot prior vs posterior parameter distributions
#' 
#' @param param_comparison Data frame from analyze_parameter_distributions()
#' @param param_name Name of parameter ("gamma_change" or "abundance_scaling")
#' @param title Plot title
#' @return ggplot object
plot_prior_posterior_distributions <- function(param_comparison, 
                                              param_name = "gamma_change",
                                              title = "Gamma Parameter Distribution") {
  
  require(ggplot2)
  require(dplyr)
  
  # Rename column for plotting
  if (param_name == "gamma_change") {
    plot_df <- param_comparison %>%
      rename(param_value = gamma_change)
  } else if (param_name == "abundance_scaling") {
    plot_df <- param_comparison %>%
      rename(param_value = abundance_scaling)
  } else {
    stop("param_name must be 'gamma_change' or 'abundance_scaling'")
  }
  
  # Calculate acceptance rate by species
  acceptance_rates <- plot_df %>%
    group_by(species) %>%
    summarise(
      acceptance_rate = sum(distribution == "Posterior (Accepted)") / n(),
      .groups = 'drop'
    )
  
  # Create plot
  p <- ggplot(plot_df, aes(x = param_value, fill = distribution, color = distribution)) +
    geom_density(alpha = 0.3, size = 0.8) +
    facet_wrap(~species, scales = "free_y", ncol = 4) +
    scale_x_log10(
      breaks = c(0.1, 0.5, 1, 2, 5, 10),
      labels = c("0.1×", "0.5×", "1×", "2×", "5×", "10×")
    ) +
    scale_fill_manual(
      values = c("Prior (All Attempted)" = "gray60", 
                 "Posterior (Accepted)" = "steelblue"),
      name = ""
    ) +
    scale_color_manual(
      values = c("Prior (All Attempted)" = "gray40", 
                 "Posterior (Accepted)" = "navy"),
      name = ""
    ) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
    labs(
      title = title,
      subtitle = paste("Prior (all attempted) vs Posterior (accepted simulations)"),
      x = "Multiplicative Change Factor (log scale)",
      y = "Density"
    ) +
    theme_bw() +
    theme(
      legend.position = "top",
      strip.text = element_text(size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}

#' Plot catchability prior vs posterior
#' 
#' @param catchability_comparison Data frame from analyze_parameter_distributions()
#' @param gear_names Optional vector of gear names for labeling
#' @return ggplot object
plot_catchability_distributions <- function(catchability_comparison, 
                                           gear_names = NULL) {
  
  require(ggplot2)
  
  if (is.null(gear_names)) {
    gear_names <- paste("Gear", unique(catchability_comparison$gear_index))
  }
  
  # Add gear labels
  catchability_comparison$gear_label <- factor(
    paste("Gear", catchability_comparison$gear_index),
    levels = gear_names
  )
  
  p <- ggplot(catchability_comparison, 
              aes(x = catchability_change, fill = distribution, color = distribution)) +
    geom_density(alpha = 0.3, size = 0.8) +
    facet_wrap(~gear_label, scales = "free_y", ncol = 3) +
    scale_x_log10(
      breaks = c(0.1, 0.5, 1, 2, 5, 10),
      labels = c("0.1×", "0.5×", "1×", "2×", "5×", "10×")
    ) +
    scale_fill_manual(
      values = c("Prior (All Attempted)" = "gray60", 
                 "Posterior (Accepted)" = "darkgreen"),
      name = ""
    ) +
    scale_color_manual(
      values = c("Prior (All Attempted)" = "gray40", 
                 "Posterior (Accepted)" = "darkgreen"),
      name = ""
    ) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
    labs(
      title = "Catchability Parameter Distribution",
      subtitle = "Prior (all attempted) vs Posterior (accepted simulations)",
      x = "Multiplicative Change Factor (log scale)",
      y = "Density"
    ) +
    theme_bw() +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}

# ===== BIOMASS ANALYSIS FUNCTIONS =====

#' Extract biomass time series from simulation summaries
#' 
#' @param mc_results Results from load_and_prepare_mc_results()
#' @param load_from_blocks If TRUE, loads from block files (slower but complete)
#' @return Data frame with biomass time series
extract_biomass_timeseries <- function(mc_results, load_from_blocks = FALSE) {
  
  cat("=== EXTRACTING BIOMASS TIME SERIES ===\n")
  
  if (load_from_blocks || is.null(mc_results$simulations)) {
    cat("Loading biomass from block files...\n")
    
    # Load from block files - use base_save_path from mc_results
    biomass_list <- list()
    block_files <- mc_results$master$block_files
    base_save_path <- mc_results$base_save_path
    
    pb <- txtProgressBar(min = 0, max = length(block_files), style = 3)
    
    for (i in seq_along(block_files)) {
      # Reconstruct correct path using current base_save_path
      block_filename <- basename(block_files[i])
      actual_block_file <- file.path(base_save_path, block_filename)
      
      if (file.exists(actual_block_file)) {
        block_data <- readRDS(actual_block_file)
        
        if (!is.null(block_data$successful_simulations)) {
          for (j in seq_along(block_data$successful_simulations)) {
            sim <- block_data$successful_simulations[[j]]
            sim_params <- block_data$successful_parameters[[j]]
            
            biomass_matrix <- getBiomass(sim)
            biomass_df <- reshape2::melt(biomass_matrix)
            names(biomass_df) <- c("Year", "Species", "Biomass")
            biomass_df$sim_id <- sim_params$sim_id
            
            biomass_list[[length(biomass_list) + 1]] <- biomass_df
          }
        }
      }
      setTxtProgressBar(pb, i)
    }
    close(pb)
    
  } else {
    cat("Extracting biomass from loaded simulations...\n")
    
    biomass_list <- lapply(seq_along(mc_results$simulations), function(i) {
      sim <- mc_results$simulations[[i]]
      
      biomass_matrix <- getBiomass(sim)
      biomass_df <- reshape2::melt(biomass_matrix)
      names(biomass_df) <- c("Year", "Species", "Biomass")
      biomass_df$sim_id <- i
      
      biomass_df
    })
  }
  
  cat("Combining data...\n")
  biomass_combined <- do.call(rbind, biomass_list)
  
  cat("Extracted biomass for", length(unique(biomass_combined$sim_id)), 
      "simulations\n")
  
  return(biomass_combined)
}

#' Plot biomass time series with observation comparison
#' 
#' @param biomass_df Data frame from extract_biomass_timeseries()
#' @param obs_biomass_data Observed biomass data frame
#' @param obs_years Years for observed data (default 2010:2020)
#' @param quantiles Quantiles to plot (default c(0.05, 0.25, 0.5, 0.75, 0.95))
#' @param highlight_species Optional vector of species to highlight
#' @return ggplot object
plot_biomass_uncertainty <- function(biomass_df,
                                    obs_biomass_data,
                                    obs_years = 2010:2020,
                                    quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                    highlight_species = NULL) {
  
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  
  cat("Calculating biomass quantiles...\n")
  
  # Species order from smallest to largest (based on observed biomass)
  species_order <- c(
    "mesozooplankton", "other krill", "other macrozooplankton", 
    "antarctic krill", "salps", "mesopelagic fishes", "bathypelagic fishes", 
    "shelf and coastal fishes", "flying birds", "small divers", "squids", 
    "toothfishes", "leopard seals", "medium divers", "large divers", 
    "minke whales", "orca", "sperm whales", "baleen whales"
  )
  
  # Calculate quantiles by year and species
  biomass_summary <- biomass_df %>%
    group_by(Year, Species) %>%
    summarise(
      median = median(Biomass, na.rm = TRUE),
      q05 = quantile(Biomass, 0.05, na.rm = TRUE),
      q25 = quantile(Biomass, 0.25, na.rm = TRUE),
      q75 = quantile(Biomass, 0.75, na.rm = TRUE),
      q95 = quantile(Biomass, 0.95, na.rm = TRUE),
      mean = mean(Biomass, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Species = factor(Species, levels = species_order))
  
  # Prepare observed data with bounds
  obs_biomass_ts <- obs_biomass_data %>%
    mutate(
      Lower = ObsBiomass * 0.75,
      Upper = ObsBiomass * 1.25,
      Species = factor(Species, levels = species_order)
    ) %>%
    tidyr::crossing(Year = obs_years)
  
  # Create plot
  p <- ggplot() +
    # 90% credible interval
    geom_ribbon(data = biomass_summary, 
                aes(x = Year, ymin = q05, ymax = q95, fill = Species),
                alpha = 0.2) +
    # 50% credible interval
    geom_ribbon(data = biomass_summary,
                aes(x = Year, ymin = q25, ymax = q75, fill = Species),
                alpha = 0.3) +
    # Median line
    geom_line(data = biomass_summary,
              aes(x = Year, y = median, color = Species),
              linewidth = 1.1) +
    # Observed data bounds (gray ribbon)
    geom_ribbon(data = obs_biomass_ts,
                aes(x = Year, ymin = Lower, ymax = Upper),
                alpha = 0.2, fill = "gray50") +
    # Observed data points (colored)
    geom_point(data = obs_biomass_ts,
               aes(x = Year, y = ObsBiomass, color = Species),
               size = 0.8) +
    # Observed data points (black outline)
    geom_point(data = obs_biomass_ts,
               aes(x = Year, y = ObsBiomass),
               shape = 1, size = 0.8, colour = "black") +
    facet_wrap(~Species, scales = "free_y") +
    # Convert grams to tonnes (divide by 1e6)
    scale_y_continuous(labels = function(x) scales::number(x/1e6)) +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    ) +
    labs(
      title = paste("Monte Carlo: Biomass vs Observations (", 
                   length(unique(biomass_df$sim_id)), " sims)"),
      x = "Year",
      y = "Biomass [t]"
    )
  
  return(p)
}

#' Plot biomass comparison for observation period
#' 
#' @param biomass_df Data frame from extract_biomass_timeseries()
#' @param obs_biomass_data Observed biomass data frame
#' @param obs_years Years for observed data (default 2010:2020)
#' @return ggplot object
plot_biomass_obs_period <- function(biomass_df,
                                   obs_biomass_data,
                                   obs_years = 2010:2020) {
  
  require(ggplot2)
  require(dplyr)
  
  cat("Calculating biomass for observation period...\n")
  
  # Filter to observation period and calculate mean
  biomass_obs_period <- biomass_df %>%
    filter(Year %in% obs_years) %>%
    group_by(sim_id, Species) %>%
    summarise(
      mean_biomass = mean(Biomass, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Calculate summary statistics
  biomass_summary <- biomass_obs_period %>%
    group_by(Species) %>%
    summarise(
      median = median(mean_biomass, na.rm = TRUE),
      q05 = quantile(mean_biomass, 0.05, na.rm = TRUE),
      q95 = quantile(mean_biomass, 0.95, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Join with observed data
  comparison <- biomass_summary %>%
    left_join(obs_biomass_data, by = "Species")
  
  # Create plot
  p <- ggplot(comparison, aes(x = Species)) +
    # Model uncertainty
    geom_errorbar(aes(ymin = q05, ymax = q95),
                  width = 0.3, color = "steelblue", size = 1) +
    geom_point(aes(y = median),
               color = "navy", size = 3) +
    # Observed data
    geom_point(aes(y = ObsBiomass),
               color = "red", size = 3, shape = 17) +
    scale_y_log10(labels = scales::scientific) +
    labs(
      title = paste("Biomass Comparison:", min(obs_years), "-", max(obs_years)),
      subtitle = "Model median (blue circle) with 90% CI vs Observed (red triangle)",
      x = "",
      y = "Biomass (g, log scale)"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
    )
  
  return(p)
}

# ===== YIELD ANALYSIS FUNCTIONS =====

#' Extract yield time series from simulations
#' 
#' @param mc_results Results from load_and_prepare_mc_results()
#' @param load_from_blocks If TRUE, loads from block files
#' @return Data frame with yield time series
extract_yield_timeseries <- function(mc_results, load_from_blocks = FALSE) {
  
  cat("=== EXTRACTING YIELD TIME SERIES ===\n")
  
  if (load_from_blocks || is.null(mc_results$simulations)) {
    cat("Loading yield from block files...\n")
    
    yield_list <- list()
    block_files <- mc_results$master$block_files
    base_save_path <- mc_results$base_save_path
    
    pb <- txtProgressBar(min = 0, max = length(block_files), style = 3)
    
    for (i in seq_along(block_files)) {
      # Reconstruct correct path using current base_save_path
      block_filename <- basename(block_files[i])
      actual_block_file <- file.path(base_save_path, block_filename)
      
      if (file.exists(actual_block_file)) {
        block_data <- readRDS(actual_block_file)
        
        if (!is.null(block_data$successful_simulations)) {
          for (j in seq_along(block_data$successful_simulations)) {
            sim <- block_data$successful_simulations[[j]]
            sim_params <- block_data$successful_parameters[[j]]
            
            yield_matrix <- getYield(sim)
            yield_df <- reshape2::melt(yield_matrix)
            names(yield_df) <- c("Year", "Species", "Yield")
            yield_df$sim_id <- sim_params$sim_id
            
            yield_list[[length(yield_list) + 1]] <- yield_df
          }
        }
      }
      setTxtProgressBar(pb, i)
    }
    close(pb)
    
  } else {
    cat("Extracting yield from loaded simulations...\n")
    
    yield_list <- lapply(seq_along(mc_results$simulations), function(i) {
      sim <- mc_results$simulations[[i]]
      
      yield_matrix <- getYield(sim)
      yield_df <- reshape2::melt(yield_matrix)
      names(yield_df) <- c("Year", "Species", "Yield")
      yield_df$sim_id <- i
      
      yield_df
    })
  }
  
  cat("Combining data...\n")
  yield_combined <- do.call(rbind, yield_list)
  
  cat("Extracted yield for", length(unique(yield_combined$sim_id)), 
      "simulations\n")
  
  return(yield_combined)
}

#' Plot yield time series with observations
#' 
#' @param yield_df Data frame from extract_yield_timeseries()
#' @param obs_yield_data Observed yield data (tidy format)
#' @param highlight_species Optional vector of species to plot separately
#' @return ggplot object
plot_yield_uncertainty <- function(yield_df,
                                  obs_yield_data,
                                  highlight_species = NULL) {
  
  require(ggplot2)
  require(dplyr)
  
  cat("Calculating yield quantiles...\n")
  
  # Species order
  species_order <- c(
    "mesozooplankton", "other krill", "other macrozooplankton", 
    "antarctic krill", "salps", "mesopelagic fishes", "bathypelagic fishes", 
    "shelf and coastal fishes", "flying birds", "small divers", "squids", 
    "toothfishes", "leopard seals", "medium divers", "large divers", 
    "minke whales", "orca", "sperm whales", "baleen whales"
  )
  
  # Guard against negative yields and identify species with any positive yield
  yield_df <- yield_df %>% mutate(Yield = pmax(Yield, 0))
  species_with_yield <- yield_df %>% 
    group_by(Species) %>% 
    summarise(max_yield = max(Yield, na.rm = TRUE), .groups = 'drop') %>% 
    filter(max_yield > 0) %>% 
    pull(Species)
  
  cat("  Species with yields:", paste(species_with_yield, collapse = ", "), "\n")
  
  # Calculate quantiles
  yield_summary <- yield_df %>%
    filter(Species %in% species_with_yield) %>%
    group_by(Year, Species) %>%
    summarise(
      median = median(Yield, na.rm = TRUE),
      q05 = quantile(Yield, 0.05, na.rm = TRUE),
      q25 = quantile(Yield, 0.25, na.rm = TRUE),
      q75 = quantile(Yield, 0.75, na.rm = TRUE),
      q95 = quantile(Yield, 0.95, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Species = factor(Species, levels = species_order))
  
  # Standardize column names in obs_yield_data
  obs_yield_plot <- obs_yield_data
  if ("species" %in% names(obs_yield_plot)) {
    obs_yield_plot <- obs_yield_plot %>% rename(Species = species)
  }
  if ("year" %in% names(obs_yield_plot)) {
    obs_yield_plot <- obs_yield_plot %>% rename(Year = year)
  }
  if ("yield" %in% names(obs_yield_plot)) {
    obs_yield_plot <- obs_yield_plot %>% rename(Yield = yield)
  }
  
  # Build observed positive-yield indicator by year/species
  obs_pos_by_year <- obs_yield_plot %>%
    mutate(has_obs_pos = Yield > 0) %>%
    group_by(Year, Species) %>%
    summarise(has_obs_pos = any(has_obs_pos, na.rm = TRUE), .groups = 'drop')
  
  # Filter modeled zeros only when there is no observed positive yield
  yield_summary <- yield_summary %>%
    left_join(obs_pos_by_year, by = c("Year", "Species")) %>%
    mutate(has_obs_pos = coalesce(has_obs_pos, FALSE))
  
  # Separate frames for ribbons and line
  yield_uncertainty_ribbon <- yield_summary %>%
    filter(!(has_obs_pos == FALSE & (is.na(q95) | q95 <= 0))) %>%
    mutate(
      q05 = ifelse(q05 > 0, q05, NA_real_),
      q25 = ifelse(q25 > 0, q25, NA_real_),
      q75 = ifelse(q75 > 0, q75, NA_real_),
      q95 = ifelse(q95 > 0, q95, NA_real_)
    ) %>%
    filter(!(is.na(q25) & is.na(q75) & is.na(q05) & is.na(q95)))
  
  yield_uncertainty_line <- yield_summary %>%
    filter(!(has_obs_pos == FALSE & (is.na(median) | median <= 0))) %>%
    mutate(median = ifelse(median > 0, median, NA_real_))
  
  # Filter observed yield to positive values only
  yield_obs_filtered <- obs_yield_plot %>%
    filter(Species %in% species_with_yield, Yield > 0) %>%
    mutate(Species = factor(Species, levels = species_order))
  
  # Determine x-axis range
  baleen_start_obs <- yield_obs_filtered %>%
    filter(Species == "baleen whales") %>%
    summarise(min_year = min(Year, na.rm = TRUE)) %>%
    pull(min_year)
  global_start_obs <- yield_obs_filtered %>% 
    summarise(min_year = min(Year, na.rm = TRUE)) %>% 
    pull(min_year)
  start_year <- if (!is.null(baleen_start_obs) && length(baleen_start_obs) == 1 && is.finite(baleen_start_obs)) {
    baleen_start_obs
  } else if (!is.null(global_start_obs) && length(global_start_obs) == 1 && is.finite(global_start_obs)) {
    global_start_obs
  } else { 1930 }
  end_year <- max(yield_summary$Year, na.rm = TRUE)
  
  # Custom label function for tonnes
  label_dynamic_decimals <- function(x) {
    sci <- scales::label_scientific(digits = 1)
    vapply(x, function(v) {
      if (is.na(v)) return(NA_character_)
      if (v <= 0) return("0")
      vt <- v / 1e6  # convert grams -> tonnes
      if (vt >= 1) {
        paste0(formatC(vt, format = "f", digits = 0, big.mark = ","), " t")
      } else if (vt < 1e-6) {
        paste0(sci(vt), " t")
      } else {
        dp <- max(1, ceiling(-log10(vt)))
        dp <- min(dp, 6)
        paste0(formatC(vt, format = "f", digits = dp), " t")
      }
    }, character(1))
  }
  
  # Create plot
  p <- ggplot() +
    geom_ribbon(data = yield_uncertainty_ribbon,
                aes(x = Year, ymin = q05, ymax = q95, fill = Species),
                alpha = 0.2) +
    geom_ribbon(data = yield_uncertainty_ribbon,
                aes(x = Year, ymin = q25, ymax = q75, fill = Species),
                alpha = 0.3) +
    geom_line(data = yield_uncertainty_line,
              aes(x = Year, y = median, color = Species),
              linewidth = 1.0, linetype = "solid", lineend = "round") +
    geom_point(data = yield_obs_filtered,
               aes(x = Year, y = Yield, colour = Species),
               size = 1) +
    geom_point(data = yield_obs_filtered,
               aes(x = Year, y = Yield),
               shape = 1, size = 1, colour = "black") +
    geom_vline(xintercept = 1961, linetype = "dashed") +
    geom_vline(xintercept = 2010, linetype = "dashed") +
    scale_y_log10(breaks = scales::log_breaks(n = 6), labels = label_dynamic_decimals) +
    coord_cartesian(xlim = c(start_year, end_year)) +
    facet_wrap(~Species, scales = "free_y") +
    theme_bw() +
    theme(legend.position = "none", strip.text = element_text(face = "bold")) +
    labs(
      x = "Year", 
      y = "Yield [t/year]", 
      title = paste("Monte Carlo: Yield vs Observations (", 
                   length(unique(yield_df$sim_id)), " sims)")
    )
  
  return(p)
}

# ===== COMPREHENSIVE ANALYSIS FUNCTION =====

#' Run complete Monte Carlo analysis and generate all plots
#' 
#' @param base_save_path Path to MC results directory
#' @param obs_biomass_data Observed biomass data frame
#' @param obs_yield_data Observed yield data frame (tidy format)
#' @param obs_years Years for biomass observations (default 2010:2020)
#' @param catchability_sd SD used in MC
#' @param abundance_sd SD used in MC
#' @param gamma_sd SD used in MC
#' @param load_simulations Whether to load full simulations (memory intensive)
#' @param save_plots Whether to save plots to files
#' @param output_dir Directory for saving plots
#' @return List containing all results and plots
analyze_mc_results <- function(base_save_path = "mc_10k_blocks",
                              obs_biomass_data,
                              obs_yield_data,
                              obs_years = 2010:2020,
                              catchability_sd = 2,
                              abundance_sd = 4,
                              gamma_sd = 2,
                              load_simulations = FALSE,
                              save_plots = TRUE,
                              output_dir = "mc_analysis_output") {
  
  cat("\n")
  cat("╔═══════════════════════════════════════════════════════════════╗\n")
  cat("║   COMPREHENSIVE MONTE CARLO ANALYSIS                          ║\n")
  cat("╚═══════════════════════════════════════════════════════════════╝\n\n")
  
  # Create output directory
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Step 1: Load data
  cat("\n[1/6] Loading Monte Carlo results...\n")
  mc_results <- load_and_prepare_mc_results(
    base_save_path = base_save_path,
    load_simulations = load_simulations,
    verbose = TRUE
  )
  
  # Step 2: Analyze parameter distributions
  cat("\n[2/6] Analyzing parameter distributions...\n")
  param_analysis <- analyze_parameter_distributions(
    mc_results = mc_results,
    catchability_sd = catchability_sd,
    abundance_sd = abundance_sd,
    gamma_sd = gamma_sd
  )
  
  # Step 3: Create parameter distribution plots
  cat("\n[3/6] Creating parameter distribution plots...\n")
  
  plot_gamma <- plot_prior_posterior_distributions(
    param_comparison = param_analysis$gamma_comparison,
    param_name = "gamma_change",
    title = "Gamma (Search Rate) Parameter Distribution"
  )
  
  if (save_plots) {
    ggsave(file.path(output_dir, "gamma_prior_posterior.png"),
           plot_gamma, width = 14, height = 10, dpi = 300)
    cat("  Saved: gamma_prior_posterior.png\n")
  }
  
  plot_abundance <- plot_prior_posterior_distributions(
    param_comparison = param_analysis$abundance_comparison,
    param_name = "abundance_scaling",
    title = "Abundance Scaling Parameter Distribution"
  )
  
  if (save_plots) {
    ggsave(file.path(output_dir, "abundance_prior_posterior.png"),
           plot_abundance, width = 14, height = 10, dpi = 300)
    cat("  Saved: abundance_prior_posterior.png\n")
  }
  
  if (nrow(param_analysis$catchability_comparison) > 0) {
    plot_catchability <- plot_catchability_distributions(
      catchability_comparison = param_analysis$catchability_comparison
    )
    
    if (save_plots) {
      ggsave(file.path(output_dir, "catchability_prior_posterior.png"),
             plot_catchability, width = 10, height = 8, dpi = 300)
      cat("  Saved: catchability_prior_posterior.png\n")
    }
  } else {
    plot_catchability <- NULL
  }
  
  # Step 4: Extract and plot biomass
  cat("\n[4/6] Extracting biomass time series...\n")
  biomass_df <- extract_biomass_timeseries(
    mc_results = mc_results,
    load_from_blocks = !load_simulations
  )
  
  cat("Creating biomass plots...\n")
  
  plot_biomass_ts <- plot_biomass_uncertainty(
    biomass_df = biomass_df,
    obs_biomass_data = obs_biomass_data,
    obs_years = obs_years
  )
  
  if (save_plots) {
    ggsave(file.path(output_dir, "biomass_timeseries_uncertainty.png"),
           plot_biomass_ts, width = 16, height = 12, dpi = 300)
    cat("  Saved: biomass_timeseries_uncertainty.png\n")
  }
  
  plot_biomass_obs <- plot_biomass_obs_period(
    biomass_df = biomass_df,
    obs_biomass_data = obs_biomass_data,
    obs_years = obs_years
  )
  
  if (save_plots) {
    ggsave(file.path(output_dir, "biomass_obs_period_comparison.png"),
           plot_biomass_obs, width = 12, height = 8, dpi = 300)
    cat("  Saved: biomass_obs_period_comparison.png\n")
  }
  
  # Step 5: Extract and plot yield
  cat("\n[5/6] Extracting yield time series...\n")
  yield_df <- extract_yield_timeseries(
    mc_results = mc_results,
    load_from_blocks = !load_simulations
  )
  
  cat("Creating yield plots...\n")
  
  plot_yield_ts <- plot_yield_uncertainty(
    yield_df = yield_df,
    obs_yield_data = obs_yield_data
  )
  
  if (save_plots) {
    ggsave(file.path(output_dir, "yield_timeseries_uncertainty.png"),
           plot_yield_ts, width = 12, height = 10, dpi = 300)
    cat("  Saved: yield_timeseries_uncertainty.png\n")
  }
  
  # Step 6: Save summary statistics
  cat("\n[6/6] Saving summary statistics...\n")
  
  if (save_plots) {
    # Save parameter summaries
    write.csv(param_analysis$gamma_summary,
              file.path(output_dir, "gamma_summary_stats.csv"),
              row.names = FALSE)
    
    write.csv(param_analysis$abundance_summary,
              file.path(output_dir, "abundance_summary_stats.csv"),
              row.names = FALSE)
    
    if (!is.null(param_analysis$catchability_summary)) {
      write.csv(param_analysis$catchability_summary,
                file.path(output_dir, "catchability_summary_stats.csv"),
                row.names = FALSE)
    }
    
    cat("  Saved summary statistics to CSV files\n")
  }
  
  cat("\n")
  cat("╔═══════════════════════════════════════════════════════════════╗\n")
  cat("║   ANALYSIS COMPLETE                                           ║\n")
  cat("╚═══════════════════════════════════════════════════════════════╝\n\n")
  
  if (save_plots) {
    cat("All outputs saved to:", output_dir, "\n\n")
  }
  
  # Return comprehensive results
  return(list(
    mc_results = mc_results,
    param_analysis = param_analysis,
    biomass_df = biomass_df,
    yield_df = yield_df,
    plots = list(
      gamma = plot_gamma,
      abundance = plot_abundance,
      catchability = plot_catchability,
      biomass_timeseries = plot_biomass_ts,
      biomass_obs_period = plot_biomass_obs,
      yield_timeseries = plot_yield_ts
    )
  ))
}

# ===== QUICK ANALYSIS WRAPPER =====

#' Quick analysis with default settings
#' 
#' @param base_save_path Path to MC results
#' @param obs_biomass_data Observed biomass data
#' @param obs_yield_data Observed yield data
#' @return Analysis results
quick_analyze_mc <- function(base_save_path = "mc_10k_blocks",
                            obs_biomass_data = obs_biomass_data,
                            obs_yield_data = yield_ts_tidy) {
  
  analyze_mc_results(
    base_save_path = base_save_path,
    obs_biomass_data = obs_biomass_data,
    obs_yield_data = obs_yield_data,
    obs_years = 2010:2020,
    catchability_sd = 2,
    abundance_sd = 4,
    gamma_sd = 2,
    load_simulations = FALSE,  # Use block files (memory efficient)
    save_plots = TRUE,
    output_dir = "mc_analysis_output"
  )
}

cat("Monte Carlo analysis helper functions loaded successfully!\n")
cat("\nQuick start:\n")
cat("  results <- analyze_mc_results(\n")
cat("    base_save_path = 'mc_10k_blocks',\n")
cat("    obs_biomass_data = obs_biomass_data,\n")
cat("    obs_yield_data = yield_ts_tidy\n")
cat("  )\n\n")
