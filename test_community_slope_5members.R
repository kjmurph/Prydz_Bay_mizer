# =============================================================================
# Test script: Community size spectrum slope & intercept from 5 ensemble members
# Extracts getCommunitySlope(biomass = TRUE) and getCommunitySlope(biomass = FALSE)
# for both the fished MC ensemble and the climate-only (unfished) ensemble.
# Produces 4 plots: slope (biomass), slope (abundance), intercept (biomass),
# intercept (abundance) - each faceted fished vs climate-only side by side.
# =============================================================================

library(mizer)
library(dplyr)
library(ggplot2)
library(purrr)

setwd("C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/Prydz_Bay_mizer")

N_TEST <- 5  # number of ensemble members to test

# --- Colour palette ----------------------------------------------------------
pal_col <- c("Exploited" = "#2166AC", "Unexploited" = "#D73027")

# --- Load ensembles ----------------------------------------------------------
cat("Loading fished MC ensemble (first", N_TEST, "sims)...\n")
mc <- readRDS("Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds")
mc_sims <- mc$simulations[1:N_TEST]

cat("Loading climate-only ensemble (first", N_TEST, "sims)...\n")
co <- readRDS("Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds")
co_sims <- co$simulations[1:N_TEST]

# --- Extraction helper -------------------------------------------------------
# Returns a tidy data.frame with columns:
#   time, slope, intercept, r2, sim_id, ensemble, spectrum_type
extract_slopes <- function(sim_list, ensemble_label) {
  cat("  Extracting slopes for", ensemble_label, "...\n")
  map_dfr(seq_along(sim_list), function(i) {
    cat("    sim", i, "/", length(sim_list), "\n")
    # biomass spectrum (log biomass ~ log size)
    db <- getCommunitySlope(sim_list[[i]], biomass = TRUE)
    db$spectrum_type <- "Biomass"
    # abundance spectrum (log numbers ~ log size)
    da <- getCommunitySlope(sim_list[[i]], biomass = FALSE)
    da$spectrum_type <- "Abundance"
    df <- rbind(db, da)
    # row names encode year (e.g. "1841...1") - extract numeric year
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

cat("\nStructure of all_slopes:\n")
str(all_slopes)
cat("\nHead:\n")
print(head(all_slopes, 12))
cat("\nspectrum_type values:", unique(all_slopes$spectrum_type), "\n")
cat("ensemble values:", unique(all_slopes$ensemble), "\n")
cat("time range:", range(all_slopes$time), "\n")

# --- Quantile summaries for slope and intercept ------------------------------
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

# --- Faceted plot (fixed axes for direct comparison) -------------------------
make_facet_plot <- function(summary_df, spectrum, var_label, y_label, n_test) {
  df <- filter(summary_df, spectrum_type == spectrum)
  ggplot(df, aes(x = time)) +
    geom_ribbon(aes(ymin = q05, ymax = q95, fill = ensemble), alpha = 0.20) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = ensemble), alpha = 0.35) +
    geom_line(aes(y = median, colour = ensemble), linewidth = 0.9) +
    scale_colour_manual(values = pal_col) +
    scale_fill_manual(values = pal_col) +
    facet_wrap(~ ensemble, ncol = 2, scales = "fixed") +
    labs(
      title    = paste0(spectrum, " spectrum ", tolower(var_label),
                        " (n = ", n_test, " test members)"),
      subtitle = "Ribbons: 90% and 50% CI | Line: median | Shared y-axis for direct comparison",
      x        = "Year",
      y        = y_label,
      colour   = NULL, fill = NULL
    ) +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "none",
      strip.text      = element_text(face = "bold", size = 11),
      plot.title      = element_text(face = "bold"),
      plot.subtitle   = element_text(colour = "grey40", size = 9)
    )
}

# --- Overlaid plot (both ensembles on the same panel) ------------------------
make_overlay_plot <- function(summary_df, spectrum, var_label, y_label, n_test) {
  df <- filter(summary_df, spectrum_type == spectrum)
  ggplot(df, aes(x = time)) +
    geom_ribbon(aes(ymin = q05, ymax = q95, fill = ensemble), alpha = 0.15) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = ensemble), alpha = 0.25) +
    geom_line(aes(y = median, colour = ensemble), linewidth = 0.9) +
    scale_colour_manual(values = pal_col) +
    scale_fill_manual(values = pal_col) +
    labs(
      title    = paste0(spectrum, " spectrum ", tolower(var_label),
                        " (n = ", n_test, " test members) — overlaid"),
      subtitle = "Ribbons: 90% and 50% CI | Line: median",
      x        = "Year",
      y        = y_label,
      colour   = NULL, fill = NULL
    ) +
    theme_classic(base_size = 12) +
    theme(
      legend.position        = c(0.02, 0.02),
      legend.justification   = c(0, 0),
      legend.background      = element_rect(fill = "white", colour = "grey80"),
      legend.key.size        = unit(0.8, "lines"),
      strip.text             = element_text(face = "bold", size = 11),
      plot.title             = element_text(face = "bold"),
      plot.subtitle          = element_text(colour = "grey40", size = 9)
    )
}

# --- Create and save 8 plots -------------------------------------------------
plots_facet <- list(
  biomass_slope       = make_facet_plot(slope_summary,     "Biomass",   "Slope",
                                        "Slope (log biomass ~ log size)",     N_TEST),
  abundance_slope     = make_facet_plot(slope_summary,     "Abundance", "Slope",
                                        "Slope (log numbers ~ log size)",     N_TEST),
  biomass_intercept   = make_facet_plot(intercept_summary, "Biomass",   "Intercept",
                                        "Intercept (log biomass ~ log size)", N_TEST),
  abundance_intercept = make_facet_plot(intercept_summary, "Abundance", "Intercept",
                                        "Intercept (log numbers ~ log size)", N_TEST)
)

plots_overlay <- list(
  biomass_slope       = make_overlay_plot(slope_summary,     "Biomass",   "Slope",
                                          "Slope (log biomass ~ log size)",     N_TEST),
  abundance_slope     = make_overlay_plot(slope_summary,     "Abundance", "Slope",
                                          "Slope (log numbers ~ log size)",     N_TEST),
  biomass_intercept   = make_overlay_plot(intercept_summary, "Biomass",   "Intercept",
                                          "Intercept (log biomass ~ log size)", N_TEST),
  abundance_intercept = make_overlay_plot(intercept_summary, "Abundance", "Intercept",
                                          "Intercept (log numbers ~ log size)", N_TEST)
)

out_facet <- c(
  biomass_slope       = "test_community_slope_biomass_facet.png",
  abundance_slope     = "test_community_slope_abundance_facet.png",
  biomass_intercept   = "test_community_intercept_biomass_facet.png",
  abundance_intercept = "test_community_intercept_abundance_facet.png"
)

out_overlay <- c(
  biomass_slope       = "test_community_slope_biomass_overlay.png",
  abundance_slope     = "test_community_slope_abundance_overlay.png",
  biomass_intercept   = "test_community_intercept_biomass_overlay.png",
  abundance_intercept = "test_community_intercept_abundance_overlay.png"
)

for (nm in names(plots_facet)) {
  ggsave(out_facet[nm],   plots_facet[[nm]],   width = 12, height = 5, dpi = 150)
  cat("Saved:", out_facet[nm], "\n")
  ggsave(out_overlay[nm], plots_overlay[[nm]], width = 7,  height = 5, dpi = 150)
  cat("Saved:", out_overlay[nm], "\n")
}

# --- Save data ---------------------------------------------------------------
saveRDS(
  list(
    all_slopes        = all_slopes,
    slope_summary     = slope_summary,
    intercept_summary = intercept_summary
  ),
  "test_community_slope_5members_data.rds"
)
cat("Data saved to: test_community_slope_5members_data.rds\n")
cat("\nDone.\n")
