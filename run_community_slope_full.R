# =============================================================================
# Full analysis: Community size spectrum slope & intercept — all 2111 members
# Extracts getCommunitySlope(biomass = TRUE/FALSE) for both ensembles.
# Saves raw data, summaries, and 8 publication-ready plots.
# =============================================================================

library(mizer)
library(dplyr)
library(ggplot2)
library(purrr)

setwd("C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/Prydz_Bay_mizer")

t_start <- proc.time()
cat("=== Full community slope analysis started:", format(Sys.time()), "===\n\n")

# --- Output directory --------------------------------------------------------
out_dir <- "Output_large_files/community_slope_analysis"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# --- Colour palette ----------------------------------------------------------
pal_col <- c("Exploited" = "#2166AC", "Unexploited" = "#D73027")

# --- Load ensembles ----------------------------------------------------------
cat("Loading Exploited (MC) ensemble...\n")
mc      <- readRDS("Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds")
mc_sims <- mc$simulations
cat("  n =", length(mc_sims), "simulations\n")

cat("Loading Unexploited (climate-only) ensemble...\n")
co      <- readRDS("Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds")
co_sims <- co$simulations
cat("  n =", length(co_sims), "simulations\n\n")

# --- Extraction helper -------------------------------------------------------
extract_slopes <- function(sim_list, ensemble_label) {
  n <- length(sim_list)
  cat("  Extracting slopes for", ensemble_label, "(n =", n, ")...\n")
  map_dfr(seq_along(sim_list), function(i) {
    if (i %% 100 == 0 || i == 1 || i == n)
      cat("    ", ensemble_label, "sim", i, "/", n,
          " elapsed:", round((proc.time() - t_start)["elapsed"] / 60, 1), "min\n")
    db <- getCommunitySlope(sim_list[[i]], biomass = TRUE)
    db$spectrum_type <- "Biomass"
    da <- getCommunitySlope(sim_list[[i]], biomass = FALSE)
    da$spectrum_type <- "Abundance"
    df <- rbind(db, da)
    df$time     <- as.numeric(sub("\\..*", "", rownames(df)))
    df$sim_id   <- i
    df$ensemble <- ensemble_label
    rownames(df) <- NULL
    df
  })
}

all_slopes <- bind_rows(
  extract_slopes(mc_sims, "Exploited"),
  extract_slopes(co_sims, "Unexploited")
)

cat("\nExtraction complete. Rows:", nrow(all_slopes), "\n")
cat("time range:", range(all_slopes$time), "\n\n")

# --- Quantile summaries ------------------------------------------------------
make_summary <- function(data, var) {
  data %>%
    group_by(ensemble, spectrum_type, time) %>%
    summarise(
      median = median(.data[[var]]),
      q05    = quantile(.data[[var]], 0.05),
      q25    = quantile(.data[[var]], 0.25),
      q75    = quantile(.data[[var]], 0.75),
      q95    = quantile(.data[[var]], 0.95),
      .groups = "drop"
    ) %>%
    mutate(variable = var)
}

slope_summary     <- make_summary(all_slopes, "slope")
intercept_summary <- make_summary(all_slopes, "intercept")

# --- Save data ---------------------------------------------------------------
saveRDS(
  list(
    all_slopes        = all_slopes,
    slope_summary     = slope_summary,
    intercept_summary = intercept_summary,
    n_sims            = length(mc_sims),
    run_date          = Sys.time()
  ),
  file.path(out_dir, "community_slope_full_2111_data.rds")
)
cat("Data saved to:", file.path(out_dir, "community_slope_full_2111_data.rds"), "\n\n")

# --- Plot functions ----------------------------------------------------------
make_facet_plot <- function(summary_df, spectrum, var_label, y_label, n) {
  df <- filter(summary_df, spectrum_type == spectrum)
  ggplot(df, aes(x = time)) +
    geom_ribbon(aes(ymin = q05, ymax = q95, fill = ensemble), alpha = 0.20) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = ensemble), alpha = 0.35) +
    geom_line(aes(y = median, colour = ensemble), linewidth = 0.9) +
    scale_colour_manual(values = pal_col) +
    scale_fill_manual(values = pal_col) +
    facet_wrap(~ ensemble, ncol = 2, scales = "fixed") +
    labs(
      title    = paste0(spectrum, " spectrum ", tolower(var_label), " (n = ", n, ")"),
      subtitle = "Ribbons: 90% and 50% CI | Line: median | Shared y-axis for direct comparison",
      x = "Year", y = y_label, colour = NULL, fill = NULL
    ) +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "none",
      strip.text      = element_text(face = "bold", size = 11),
      plot.title      = element_text(face = "bold"),
      plot.subtitle   = element_text(colour = "grey40", size = 9)
    )
}

make_overlay_plot <- function(summary_df, spectrum, var_label, y_label, n) {
  df <- filter(summary_df, spectrum_type == spectrum)
  ggplot(df, aes(x = time)) +
    geom_ribbon(aes(ymin = q05, ymax = q95, fill = ensemble), alpha = 0.15) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = ensemble), alpha = 0.25) +
    geom_line(aes(y = median, colour = ensemble), linewidth = 0.9) +
    scale_colour_manual(values = pal_col) +
    scale_fill_manual(values = pal_col) +
    labs(
      title    = paste0(spectrum, " spectrum ", tolower(var_label), " (n = ", n, ") \u2014 overlaid"),
      subtitle = "Ribbons: 90% and 50% CI | Line: median",
      x = "Year", y = y_label, colour = NULL, fill = NULL
    ) +
    theme_classic(base_size = 12) +
    theme(
      legend.position      = c(0.02, 0.02),
      legend.justification = c(0, 0),
      legend.background    = element_rect(fill = "white", colour = "grey80"),
      legend.key.size      = unit(0.8, "lines"),
      plot.title           = element_text(face = "bold"),
      plot.subtitle        = element_text(colour = "grey40", size = 9)
    )
}

# --- Build and save 8 plots --------------------------------------------------
n_sims <- length(mc_sims)

combos <- list(
  list(sdf = slope_summary,     spectrum = "Biomass",   var = "Slope",
       ylab = "Slope (log biomass ~ log size)",     tag = "slope_biomass"),
  list(sdf = slope_summary,     spectrum = "Abundance", var = "Slope",
       ylab = "Slope (log numbers ~ log size)",     tag = "slope_abundance"),
  list(sdf = intercept_summary, spectrum = "Biomass",   var = "Intercept",
       ylab = "Intercept (log biomass ~ log size)", tag = "intercept_biomass"),
  list(sdf = intercept_summary, spectrum = "Abundance", var = "Intercept",
       ylab = "Intercept (log numbers ~ log size)", tag = "intercept_abundance")
)

for (x in combos) {
  pf <- make_facet_plot(x$sdf, x$spectrum, x$var, x$ylab, n_sims)
  po <- make_overlay_plot(x$sdf, x$spectrum, x$var, x$ylab, n_sims)
  f_out <- file.path(out_dir, paste0("community_", x$tag, "_facet.png"))
  o_out <- file.path(out_dir, paste0("community_", x$tag, "_overlay.png"))
  ggsave(f_out, pf, width = 12, height = 5, dpi = 300)
  ggsave(o_out, po, width = 7,  height = 5, dpi = 300)
  cat("Saved:", basename(f_out), "\n")
  cat("Saved:", basename(o_out), "\n")
}

elapsed <- (proc.time() - t_start)["elapsed"]
cat("\n=== Analysis complete:", format(Sys.time()), "===\n")
cat("Total elapsed:", round(elapsed / 60, 1), "minutes\n")
