# Helper functions

plot_biomass_comparison <- function(sim_object, 
                                    obs_biomass_data = NULL,
                                    obs_years = 2010:2020,
                                    uncertainty = 0.1,
                                    species_labels = NULL,
                                    plot_title = "Modeled vs. Observed Biomass",
                                    plot_subtitle = NULL) {
  
  # Required libraries
  require(ggplot2)
  require(dplyr)
  require(reshape2)
  require(tidyr)
  
  # Extract modeled biomass time series
  mod_biomass_matrix <- getBiomass(sim_object)
  
  # Convert matrix to data frame
  df_mod_biomass <- mod_biomass_matrix %>% 
    melt()
  names(df_mod_biomass) <- c("Year", "Species", "Biomass")
  
  # Default observed biomass data if not provided
  if (is.null(obs_biomass_data)) {
    obs_biomass_data <- data.frame(
      Species = c("mesozooplankton","other krill","other macrozooplankton",
                  "antarctic krill","salps","mesopelagic fishes",
                  "bathypelagic fishes","shelf and coastal fishes","flying birds",
                  "small divers","squids","toothfishes","leopard seals",
                  "medium divers","large divers","minke whales","orca",
                  "sperm whales","baleen whales"),
      ObsBiomass = c(1.297420e+13, 2.801248e+12, 1.474341e+13, 5.897364e+12, 
                     9.612703e+11, 1.769209e+12, 1.769209e+12, 4.027900e+12, 
                     4.423023e+09, 2.358946e+10, 2.211512e+11, 1.105756e+12, 
                     2.948682e+09, 3.907004e+11, 1.621775e+10, 2.064077e+10, 
                     8.846046e+09, 1.621775e+10, 1.872413e+11)
    )
  }
  
  # Add uncertainty bounds
  obs_biomass <- obs_biomass_data %>%
    mutate(
      Lower = ObsBiomass * (1 - uncertainty),
      Upper = ObsBiomass * (1 + uncertainty)
    )
  
  # Create time series version for observation years
  obs_biomass_ts <- obs_biomass %>%
    crossing(Year = obs_years)
  
  # Define species order (use provided labels or default order)
  if (is.null(species_labels)) {
    species_order <- c("mesozooplankton", "other krill", "other macrozooplankton", 
                       "antarctic krill", "salps", "mesopelagic fishes", 
                       "bathypelagic fishes", "shelf and coastal fishes", 
                       "flying birds", "small divers", "squids", "toothfishes", 
                       "leopard seals", "medium divers", "large divers", 
                       "minke whales", "orca", "sperm whales", "baleen whales")
    species.labs <- species_order  # Use species names as labels
  } else {
    species_order <- names(species_labels)
    species.labs <- species_labels
  }
  
  # Convert Species to factors with specified order
  obs_biomass_ts$Species <- factor(obs_biomass_ts$Species, levels = species_order)
  df_mod_biomass$Species <- factor(df_mod_biomass$Species, levels = species_order)
  
  # Create subtitle if not provided
  if (is.null(plot_subtitle)) {
    plot_subtitle <- paste0("Observed values (", min(obs_years), "-", max(obs_years), 
                            ") shown as points with ±", uncertainty*100, "% uncertainty (shaded)")
  }
  
  # Create the plot
  p_biomass <- ggplot() +
    # Plot model biomass line
    geom_line(data = df_mod_biomass, 
              aes(x = Year, y = Biomass, color = Species), linewidth = 1.5) +
    # Plot observed biomass as points
    geom_point(data = obs_biomass_ts, 
               aes(x = Year, y = ObsBiomass, color = Species), 
               size = 0.8) +
    geom_point(data = obs_biomass_ts, 
               aes(x = Year, y = ObsBiomass), 
               shape = 1, size = 0.8, colour = "black") +
    # Add uncertainty ribbon
    geom_ribbon(data = obs_biomass_ts,
                aes(x = Year, ymin = Lower, ymax = Upper),
                alpha = 0.2) +
    # Facet by species with free y scales
    facet_wrap(~Species, scales = "free_y",
               labeller = labeller(Species = species.labs)) +
    # Theme and labels
    theme_bw() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 12)
    ) +
    labs(
      x = "Year",
      y = "Biomass",
      title = plot_title,
      subtitle = plot_subtitle
    )
  
  return(p_biomass)
}

# Example usage:
# Basic usage with default parameters
# p1 <- plot_biomass_comparison(sim_1841_2010_fishing_only)

# Usage with custom parameters
# p2 <- plot_biomass_comparison(
#   sim_object = sim_1841_2010_fishing_only,
#   obs_years = 2015:2020,
#   uncertainty = 0.15,
#   plot_title = "Custom Biomass Comparison",
#   plot_subtitle = "Custom subtitle here"
# )

# Usage with custom observed data
# custom_obs_data <- data.frame(
#   Species = c("species1", "species2"),
#   ObsBiomass = c(1e12, 2e12)
# )
# p3 <- plot_biomass_comparison(
#   sim_object = sim_1841_2010_fishing_only,
#   obs_biomass_data = custom_obs_data
# )


plot_yield_comparison <- function(sim_object,
                                  yield_obs_data,
                                  free_y_scale = TRUE,
                                  species_labels = NULL,
                                  vertical_lines = c(1961, 2010),
                                  plot_title = "Modeled vs. Observed Yield",
                                  show_legend = FALSE,
                                  point_size = 1) {
  
  # Required libraries
  require(ggplot2)
  require(dplyr)
  
  # Default species labels if not provided
  if (is.null(species_labels)) {
    species_labels <- c("Antarctic krill", "Bathypelagic fishes", 
                        "Shelf & Coastal fishes", "Squids", "Toothfishes", 
                        "Antarctic minke whales", "Orcas", "Sperm whales", 
                        "Baleen whales")
    names(species_labels) <- c("antarctic krill", "bathypelagic fishes", 
                               "shelf and coastal fishes", "squids", "toothfishes", 
                               "minke whales", "orca", "sperm whales", "baleen whales")
  }
  
  # Create base plot using plotYield
  p_yield <- plotYield(sim_object)
  
  # Set up facet wrap based on y-axis scale preference
  facet_scales <- if (free_y_scale) "free_y" else "fixed"
  
  p_yield <- p_yield + 
    facet_wrap(~Species, scales = facet_scales,
               labeller = labeller(Species = species_labels))
  
  # Add observed data points and styling
  p_yield <- p_yield +
    labs(fill = "Functional group", title = plot_title) +
    geom_point(data = yield_obs_data, 
               aes(x = Year, y = Yield, colour = Species), 
               size = point_size) +
    geom_point(data = yield_obs_data, 
               aes(x = Year, y = Yield), 
               shape = 1, size = point_size, colour = "black") +
    theme_bw() +
    theme(
      legend.title = element_text(size = 3), 
      legend.text = element_text(size = 3),
      legend.position = if(show_legend) "bottom" else "none"
    )
  
  # Add vertical lines if specified
  if (!is.null(vertical_lines) && length(vertical_lines) > 0) {
    for (line_year in vertical_lines) {
      p_yield <- p_yield + 
        geom_vline(aes(xintercept = line_year), linetype = "dashed")
    }
  }
  
  return(p_yield)
}

# Example usage:
# Basic usage with free y-axis (default)
# p_yield_free <- plot_yield_comparison(
#   sim_object = sim_1841_2010_fishing_only,
#   yield_obs_data = yield_ts_tidy
# )

# Usage with fixed y-axis scale
# p_yield_fixed <- plot_yield_comparison(
#   sim_object = sim_1841_2010_fishing_only,
#   yield_obs_data = yield_ts_tidy,
#   free_y_scale = FALSE
# )

# Usage with custom parameters
# p_yield_custom <- plot_yield_comparison(
#   sim_object = sim_1841_2010_fishing_only,
#   yield_obs_data = yield_ts_tidy,
#   free_y_scale = TRUE,
#   vertical_lines = c(1950, 1980, 2000),
#   plot_title = "Custom Yield Analysis",
#   show_legend = TRUE,
#   point_size = 1.5
# )

# Using convenience functions
# p_free <- plot_yield_free_scale(sim_1841_2010_fishing_only, yield_ts_tidy)
# p_fixed <- plot_yield_fixed_scale(sim_1841_2010_fishing_only, yield_ts_tidy)