###############################################################################
# PLOT FROM CHECKPOINTS
# 
# Purpose: Generate all ecosystem assessment plots and outputs from existing
#          checkpoint files (partial or complete run)
# 
# Usage:
#   1. Stop the main analysis at any point (checkpoints saved every 10 sims)
#   2. Run: Rscript plot_from_checkpoints.R
#   3. This will load all existing checkpoints and generate preliminary outputs
# 
# Notes:
#   - Requires at least one checkpoint file to exist
#   - All downstream analyses use the exact same functions as the main assessment
#   - Output files show N (number of simulations) in filenames for clarity
#   - Does NOT delete checkpoint files (preserves ability to resume main run)
###############################################################################

# Source the main assessment code (all functions, configurations)
source("ecosystem_assessment_v2.R")

###############################################################################
# Load and Merge Checkpoint Files
###############################################################################

load_checkpoints_for_plotting <- function(checkpoint_dir) {
  cat("=============================================================\n")
  cat("LOADING CHECKPOINT FILES FOR PLOTTING\n")
  cat("=============================================================\n\n")
  
  if (!dir.exists(checkpoint_dir)) {
    stop("Checkpoint directory does not exist: ", checkpoint_dir)
  }
  
  checkpoints <- find_checkpoints(checkpoint_dir)
  
  if (length(checkpoints) == 0) {
    stop("No checkpoint files found in: ", checkpoint_dir)
  }
  
  cat(sprintf("Found %d checkpoint files\n\n", length(checkpoints)))
  
  # Load and merge all checkpoints
  all_fishing <- list()
  all_climate <- list()
  all_b0 <- list()
  
  for (i in seq_along(checkpoints)) {
    cp_file <- checkpoints[i]
    cp_data <- readRDS(cp_file)
    
    cat(sprintf("  [%2d/%2d] Checkpoint sim %04d (saved %s)\n", 
                i, length(checkpoints), cp_data$last_sim,
                format(cp_data$timestamp, "%Y-%m-%d %H:%M:%S")))
    
    all_fishing <- c(all_fishing, cp_data$fishing_results)
    all_climate <- c(all_climate, cp_data$climate_results)
    all_b0 <- c(all_b0, cp_data$b0_results)
  }
  
  # Combine into data frames
  fishing_raw <- bind_rows(all_fishing)
  climate_raw <- bind_rows(all_climate)
  b0_reference <- bind_rows(all_b0)
  
  # Get total number of unique simulations
  n_sims_fishing <- length(unique(fishing_raw$sim_id))
  n_sims_climate <- length(unique(climate_raw$sim_id))
  n_sims_b0 <- length(unique(b0_reference$sim_id))
  
  cat(sprintf("\n=============================================================\n"))
  cat(sprintf("MERGED CHECKPOINT DATA:\n"))
  cat(sprintf("  Fishing simulations:     %d\n", n_sims_fishing))
  cat(sprintf("  Climate-only simulations: %d\n", n_sims_climate))
  cat(sprintf("  B0 references:           %d\n", n_sims_b0))
  cat(sprintf("=============================================================\n\n"))
  
  list(
    fishing_raw = fishing_raw,
    climate_raw = climate_raw,
    b0_reference = b0_reference,
    n_sims = min(n_sims_fishing, n_sims_climate)
  )
}

###############################################################################
# Main Execution: Generate All Outputs from Checkpoints
###############################################################################

main_plot_from_checkpoints <- function(checkpoint_dir = NULL) {
  
  if (is.null(checkpoint_dir)) {
    checkpoint_dir <- file.path(OUTPUT_DIR_LARGE, "checkpoints")
  }
  
  start_time <- Sys.time()
  
  cat("\n")
  cat("=============================================================\n")
  cat("PRYDZ BAY ECOSYSTEM ASSESSMENT - FROM CHECKPOINTS\n")
  cat(sprintf("B0 Reference Period: %d-%d (ISIMIP climate norm)\n", B0_PERIOD[1], B0_PERIOD[2]))
  cat("=============================================================\n\n")
  
  # 1. Load checkpoint data
  checkpoint_data <- load_checkpoints_for_plotting(checkpoint_dir)
  fishing_raw <- checkpoint_data$fishing_raw
  climate_raw <- checkpoint_data$climate_raw
  b0_reference <- checkpoint_data$b0_reference
  n_sims <- checkpoint_data$n_sims
  
  # Create output suffix to indicate partial run
  output_suffix <- sprintf("_n%04d", n_sims)
  
  # 2. Derive empirical thresholds
  cat("=============================================================\n")
  cat("DERIVING EMPIRICAL THRESHOLDS\n")
  cat("=============================================================\n")
  empirical_thresholds <- derive_empirical_thresholds(b0_reference)
  
  # 3. Compute paired comparisons
  paired_data <- compute_paired_comparisons(
    fishing_raw, climate_raw, b0_reference, empirical_thresholds)
  
  # 4. Summarize by category
  cat("\nSummarizing results by metric category...\n")
  biomass_summaries <- summarize_biomass_ratios(paired_data)
  structural_summaries <- summarize_structural_deviations(paired_data, empirical_thresholds)
  exploitation_summary <- summarize_exploitation(paired_data)
  
  # 5. Pre-whaling validation
  validation <- validate_pre_exploitation(biomass_summaries, structural_summaries)
  
  # 6. Generate visualizations
  cat("\n=============================================================\n")
  cat("GENERATING VISUALIZATIONS\n")
  cat("=============================================================\n\n")
  plots <- list()
  
  # 6a. Biomass ratio heatmaps
  cat("  Category A: Biomass ratio heatmaps...\n")
  plots$bio_exploit_ratio <- plot_biomass_ratio_heatmap(
    biomass_summaries$exploit, 
    sprintf("Exploitation Impact: Biomass Metrics (N=%d)", n_sims),
    "Median ratio: Fishing / Climate-only (paired)",
    OUTPUT_DIR, paste0("heatmap_biomass_exploitation_ratio", output_suffix))
  plots$bio_absolute_ratio <- plot_biomass_ratio_heatmap(
    biomass_summaries$absolute, 
    sprintf("Absolute Health: Biomass Metrics (N=%d)", n_sims),
    sprintf("Median ratio: Fishing / B0 (%d-%d)", B0_PERIOD[1], B0_PERIOD[2]),
    OUTPUT_DIR, paste0("heatmap_biomass_absolute_ratio", output_suffix))
  plots$bio_climate_ratio <- plot_biomass_ratio_heatmap(
    biomass_summaries$climate, 
    sprintf("Climate Impact: Biomass Metrics (N=%d)", n_sims),
    sprintf("Median ratio: Climate-only / B0 (%d-%d)", B0_PERIOD[1], B0_PERIOD[2]),
    OUTPUT_DIR, paste0("heatmap_biomass_climate_ratio", output_suffix))
  
  # Biomass proportion heatmaps
  cat("  Category A: Biomass proportion heatmaps...\n")
  plots$bio_prop_075 <- plot_biomass_proportion_heatmap(
    biomass_summaries$absolute, "prop_below_075", 0.75,
    "CCAMLR \u03B3\u2082 escapement; Constable et al. 2000",
    sprintf("Absolute Health: P(Biomass/B0 < 0.75) (N=%d)", n_sims),
    OUTPUT_DIR, paste0("heatmap_biomass_prop_below_075", output_suffix))
  plots$bio_prop_040 <- plot_biomass_proportion_heatmap(
    biomass_summaries$absolute, "prop_below_040", 0.40,
    "BMSY proxy; Restrepo et al. 1998",
    sprintf("Absolute Health: P(Biomass/B0 < 0.40) (N=%d)", n_sims),
    OUTPUT_DIR, paste0("heatmap_biomass_prop_below_040", output_suffix))
  plots$bio_prop_020 <- plot_biomass_proportion_heatmap(
    biomass_summaries$absolute, "prop_below_020", 0.20,
    "CCAMLR \u03B3\u2081 / MSST collapse; Constable et al. 2000",
    sprintf("Absolute Health: P(Biomass/B0 < 0.20) (N=%d)", n_sims),
    OUTPUT_DIR, paste0("heatmap_biomass_prop_below_020", output_suffix))
  plots$whale_prop_054 <- plot_biomass_proportion_heatmap(
    biomass_summaries$absolute, "prop_below_054", 0.54,
    "IWC RMP protection level; IWC 1994, Punt & Donovan 2007",
    sprintf("Whale Populations: P(Biomass/B0 < 0.54) (N=%d)", n_sims),
    OUTPUT_DIR, paste0("heatmap_whale_prop_below_054", output_suffix), 
    metrics_filter = WHALE_METRICS)
  plots$whale_exploit_054 <- plot_biomass_proportion_heatmap(
    biomass_summaries$exploit, "prop_below_054", 0.54,
    "IWC RMP protection level; IWC 1994, Punt & Donovan 2007",
    sprintf("Exploitation Impact on Whales: P(Fishing/Climate-only < 0.54) (N=%d)", n_sims),
    OUTPUT_DIR, paste0("heatmap_whale_exploit_prop_below_054", output_suffix), 
    metrics_filter = WHALE_METRICS)
  
  # 6b. Structural deviation heatmaps
  cat("  Category B: Structural deviation heatmaps...\n")
  plots$str_outside_90 <- plot_structural_deviation_heatmap(
    structural_summaries$absolute, "prop_outside_b0_90",
    "5th\u201395th percentile (empirical B0 envelope, 1841\u20131860)",
    sprintf("Structural Metrics: Outside B0 Natural Range (N=%d)", n_sims),
    OUTPUT_DIR, paste0("heatmap_structural_outside_b0_90", output_suffix))
  plots$str_outside_98 <- plot_structural_deviation_heatmap(
    structural_summaries$absolute, "prop_outside_b0_98",
    "1st\u201399th percentile (substantially altered)",
    sprintf("Structural Metrics: Substantially Altered (N=%d)", n_sims),
    OUTPUT_DIR, paste0("heatmap_structural_outside_b0_98", output_suffix))
  plots$str_zscore_abs <- plot_structural_zscore_heatmap(
    structural_summaries$absolute,
    sprintf("Structural Metrics: Standardized Departure from B0 (N=%d)", n_sims),
    OUTPUT_DIR, paste0("heatmap_structural_zscore_absolute", output_suffix))
  plots$str_zscore_exploit <- plot_structural_zscore_heatmap(
    structural_summaries$exploit,
    sprintf("Structural Metrics: Exploitation-Driven Departure (N=%d)", n_sims),
    OUTPUT_DIR, paste0("heatmap_structural_zscore_exploitation", output_suffix))
  
  # 6c. Combined heatmaps
  cat("  Combined heatmaps (biomass + structural)...\n")
  plots$combined_absolute <- plot_combined_heatmap(
    biomass_summaries$absolute, structural_summaries$absolute,
    sprintf("Ecosystem Assessment: Prydz Bay (Absolute) (N=%d)", n_sims),
    OUTPUT_DIR, paste0("heatmap_combined_absolute", output_suffix))
  plots$combined_exploit <- plot_combined_heatmap(
    biomass_summaries$exploit, structural_summaries$exploit,
    sprintf("Ecosystem Assessment: Exploitation Impact (N=%d)", n_sims),
    OUTPUT_DIR, paste0("heatmap_combined_exploitation", output_suffix))
  plots$combined_climate <- plot_combined_heatmap(
    biomass_summaries$climate, structural_summaries$climate,
    sprintf("Ecosystem Assessment: Climate Impact (N=%d)", n_sims),
    OUTPUT_DIR, paste0("heatmap_combined_climate", output_suffix))
  
  # 6d. Exploitation heatmap
  cat("  Category C: Exploitation rate heatmap...\n")
  # Need to modify for suffix - create custom version
  exploit_plot_data <- exploitation_summary %>%
    mutate(display_name = sapply(metric_name, function(m) {
      exploit_display <- list(
        exploitation_total = list(name = "Total Ecosystem", color = "#d73027"),
        exploitation_whale = list(name = "Whales", color = "#fc8d59"),
        exploitation_krill = list(name = "Krill", color = "#91bfdb")
      )
      if (m %in% names(exploit_display)) exploit_display[[m]]$name else m
    }))
  decade_order <- unique(exploit_plot_data$decade[order(exploit_plot_data$start_year)])
  exploit_plot_data$decade <- factor(exploit_plot_data$decade, levels = decade_order)
  name_order <- c("Total Ecosystem", "Whales", "Krill")
  exploit_plot_data$display_name <- factor(exploit_plot_data$display_name, levels = rev(name_order))
  
  p_exploit <- ggplot(exploit_plot_data, aes(x = decade, y = display_name, fill = value_median)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.3f", value_median)), size = 2.8, color = "black") +
    scale_fill_gradientn(
      colours = c("#1a9850", "#91cf60", "#fee08b", "#fc8d59", "#d73027"),
      values = c(0, 0.05, 0.2, 0.4, 1),
      limits = c(0, max(0.5, max(exploit_plot_data$value_median, na.rm = TRUE))),
      na.value = "grey80", name = "Median F") +
    labs(title = sprintf("Exploitation Rates (Fishing Scenario) (N=%d)", n_sims),
         subtitle = "Median fishing mortality rates across ensemble",
         x = "Decade", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5),
          legend.position = "right", panel.grid = element_blank())
  
  ggsave(file.path(OUTPUT_DIR, paste0("exploitation_heatmap", output_suffix, ".png")), 
         p_exploit, width = 15, height = 5, dpi = 300)
  ggsave(file.path(OUTPUT_DIR, paste0("exploitation_heatmap", output_suffix, ".pdf")), 
         p_exploit, width = 15, height = 5)
  plots$exploitation_F <- p_exploit
  
  # 7. Save summary outputs (NOT large RDS files - those come from full run only)
  cat("\n=============================================================\n")
  cat("SAVING SUMMARY OUTPUTS\n")
  cat("=============================================================\n\n")
  cat("Summary csvs/plots -> ecosystem_assessment_outputs/\n")
  for (comp in c("exploit", "absolute", "climate")) {
    write.csv(biomass_summaries[[comp]],
              file.path(OUTPUT_DIR, sprintf("summary_biomass_%s%s.csv", comp, output_suffix)), 
              row.names = FALSE)
    write.csv(structural_summaries[[comp]],
              file.path(OUTPUT_DIR, sprintf("summary_structural_%s%s.csv", comp, output_suffix)), 
              row.names = FALSE)
  }
  write.csv(exploitation_summary,
            file.path(OUTPUT_DIR, sprintf("summary_exploitation_rates%s.csv", output_suffix)), 
            row.names = FALSE)
  if (!is.null(validation)) {
    if (!is.null(validation$biomass))
      write.csv(validation$biomass, 
                file.path(OUTPUT_DIR, sprintf("validation_biomass%s.csv", output_suffix)), 
                row.names = FALSE)
    if (!is.null(validation$structural))
      write.csv(validation$structural, 
                file.path(OUTPUT_DIR, sprintf("validation_structural%s.csv", output_suffix)), 
                row.names = FALSE)
  }
  
  end_time <- Sys.time()
  cat(sprintf("\n=============================================================\n"))
  cat(sprintf("CHECKPOINT PLOTTING COMPLETE (%.1f minutes)\n", 
              difftime(end_time, start_time, units = "mins")))
  cat(sprintf("Based on %d paired simulations\n", n_sims))
  cat("=============================================================\n\n")
  cat("Output files (all with suffix:", output_suffix, "):\n")
  cat("  SUMMARY CSVS (ecosystem_assessment_outputs/):\n")
  cat("    - summary_biomass_{exploit,absolute,climate}.csv\n")
  cat("    - summary_structural_{exploit,absolute,climate}.csv\n")
  cat("    - summary_exploitation_rates.csv\n")
  cat("    - validation_{biomass,structural}.csv\n")
  cat("  HEATMAPS (ecosystem_assessment_outputs/):\n")
  cat("    Category A (Biomass - ratio-based):\n")
  cat("      - heatmap_biomass_{exploitation,absolute,climate}_ratio\n")
  cat("      - heatmap_biomass_prop_below_{075,040,020}\n")
  cat("      - heatmap_whale_{prop,exploit}_below_054\n")
  cat("    Category B (Structural - empirical deviation):\n")
  cat("      - heatmap_structural_outside_b0_{90,98}\n")
  cat("      - heatmap_structural_zscore_{absolute,exploitation}\n")
  cat("    Combined (A + B):\n")
  cat("      - heatmap_combined_{absolute,exploitation,climate}\n")
  cat("    Category C (Exploitation):\n")
  cat("      - exploitation_heatmap\n\n")
  cat("NOTE: Checkpoint files preserved in Output_large_files/ecosystem_assessment/checkpoints/\n")
  cat("      Main analysis can still resume from last checkpoint.\n")
  
  return(list(
    n_sims = n_sims,
    fishing_raw = fishing_raw, 
    climate_raw = climate_raw,
    b0_reference = b0_reference, 
    paired_data = paired_data,
    empirical_thresholds = empirical_thresholds,
    biomass_summaries = biomass_summaries,
    structural_summaries = structural_summaries,
    exploitation_summary = exploitation_summary,
    validation = validation, 
    plots = plots
  ))
}

###############################################################################
# Execute
###############################################################################

if (!interactive()) {
  results <- main_plot_from_checkpoints()
}
