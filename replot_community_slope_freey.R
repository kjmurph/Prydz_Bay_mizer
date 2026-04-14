# Replot community slope/intercept from cached test data
# Faceted (shared y-axis) + overlaid versions for all 4 metrics
library(dplyr)
library(ggplot2)

setwd("C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/Prydz_Bay_mizer")

N_TEST  <- 5
pal_col <- c("Exploited" = "#2166AC", "Unexploited" = "#D73027")

dat               <- readRDS("test_community_slope_5members_data.rds")
slope_summary     <- dat$slope_summary
intercept_summary <- dat$intercept_summary

# Relabel ensembles to match new names
relabel <- function(df) {
  df$ensemble <- dplyr::recode(df$ensemble,
    "Fished (MC ensemble)"    = "Exploited",
    "Climate-only (unfished)" = "Unexploited"
  )
  df
}
slope_summary     <- relabel(slope_summary)
intercept_summary <- relabel(intercept_summary)

cat("Columns in slope_summary:", paste(names(slope_summary), collapse = ", "), "\n")
cat("spectrum_type values:", paste(unique(slope_summary$spectrum_type), collapse = ", "), "\n")

# --- Faceted plot (shared/fixed y-axis for direct comparison) ----------------
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

# --- Overlaid plot (both ensembles on one panel) -----------------------------
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
                        " (n = ", n_test, " test members) \u2014 overlaid"),
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

# --- Build and save all 8 plots ----------------------------------------------
combos <- list(
  list(sdf = slope_summary,     spectrum = "Biomass",    var = "Slope",
       ylab = "Slope (log biomass ~ log size)",     tag = "slope_biomass"),
  list(sdf = slope_summary,     spectrum = "Abundance",  var = "Slope",
       ylab = "Slope (log numbers ~ log size)",     tag = "slope_abundance"),
  list(sdf = intercept_summary, spectrum = "Biomass",    var = "Intercept",
       ylab = "Intercept (log biomass ~ log size)", tag = "intercept_biomass"),
  list(sdf = intercept_summary, spectrum = "Abundance",  var = "Intercept",
       ylab = "Intercept (log numbers ~ log size)", tag = "intercept_abundance")
)

for (x in combos) {
  pf <- make_facet_plot(x$sdf, x$spectrum, x$var, x$ylab, N_TEST)
  po <- make_overlay_plot(x$sdf, x$spectrum, x$var, x$ylab, N_TEST)
  f_out <- paste0("test_community_", x$tag, "_facet.png")
  o_out <- paste0("test_community_", x$tag, "_overlay.png")
  ggsave(f_out, pf, width = 12, height = 5, dpi = 150)
  ggsave(o_out, po, width = 7,  height = 5, dpi = 150)
  cat("Saved:", f_out, "\n")
  cat("Saved:", o_out, "\n")
}

cat("Done.\n")
