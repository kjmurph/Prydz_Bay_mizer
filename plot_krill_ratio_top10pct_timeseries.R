###############################################################################
# Antarctic krill consumption ratio: exploited / unexploited
# Using the top-10% RMSE-filtered ensemble (212 paired sims)
#
# Three overlaid timeseries, 1901-2010:
#   - All predators combined (ribbon + line)  — total community signal
#   - Large baleen + minke whales (line)       — losers under exploitation
#   - Fishes (line)                            — winners under exploitation
#
# Data source: full_diet_top10pct_fishing_all_sims.rds
#              full_diet_top10pct_climate_only_all_sims.rds
# Each element is a 3D array [year × predator × prey] of domain-wide
# consumption (g yr-1).
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

OUTPUT_DIR <- "whale_consumption_outputs"

###############################################################################
# Colours (identical to original figure)
###############################################################################
FILL_COL  <- "#4daf8d"   # ribbon fill — total all predators
LINE_COL  <- "#1a4d38"   # median line — total all predators
WHALE_COL <- "#e878b8"   # large baleen + minke (losers)
FISH_COL  <- "#0072B2"   # fishes (winners)

KRILL_START <- 1974
KRILL_END   <- 1996
VLINE_COL   <- "#e8534a"

YEAR_MIN <- 1901    # plot start year (sims begin 1841)

###############################################################################
# Predator group definitions
###############################################################################
PREDS_BALEEN_MINKE <- c("baleen whales", "minke whales")
PREDS_TOOTHED      <- c("sperm whales", "orca")
PREDS_FISH         <- c("mesopelagic fishes", "bathypelagic fishes",
                        "shelf and coastal fishes", "toothfishes")
PREDS_SEALS        <- c("leopard seals", "small divers",
                        "medium divers", "large divers")
PREY_KRILL         <- "antarctic krill"

###############################################################################
# Helper: extract per-sim annual krill consumption for a predator group
# Returns a list of data.frames(year, total_consumption), one per sim
###############################################################################
extract_group_krill <- function(arr_list, pred_names) {
  lapply(arr_list, function(arr) {
    years       <- as.numeric(dimnames(arr)$year)
    valid_preds <- intersect(pred_names, dimnames(arr)$predator)
    if (length(valid_preds) == 0) {
      return(data.frame(year = years, total_consumption = 0))
    }
    slice <- arr[, valid_preds, PREY_KRILL, drop = FALSE]
    vals  <- if (length(valid_preds) == 1) as.numeric(slice) else rowSums(slice)
    data.frame(year = years, total_consumption = as.numeric(vals))
  })
}

###############################################################################
# Helper: element-wise sum of per-sim consumption lists
###############################################################################
sum_sim_lists <- function(...) {
  lists <- list(...)
  lapply(seq_along(lists[[1]]), function(i) {
    total <- lists[[1]][[i]]$total_consumption
    for (lst in lists[-1]) total <- total + lst[[i]]$total_consumption
    data.frame(year = lists[[1]][[i]]$year, total_consumption = total)
  })
}

###############################################################################
# Helper: compute annual exploited/unexploited ratio ensemble quantiles
###############################################################################
compute_annual_ratio <- function(fish_list, clim_list, year_min = YEAR_MIN) {
  rows <- lapply(seq_along(fish_list), function(i) {
    f   <- fish_list[[i]]
    cl  <- clim_list[[i]]
    mrg <- merge(f, cl, by = "year", suffixes = c("_f", "_c"))
    mrg$ratio <- ifelse(mrg$total_consumption_c > 0,
                        mrg$total_consumption_f / mrg$total_consumption_c,
                        NA_real_)
    mrg[mrg$year >= year_min, c("year", "ratio")]
  })

  do.call(rbind, rows) %>%
    group_by(year) %>%
    summarise(
      q25 = quantile(ratio, 0.25, na.rm = TRUE),
      med = median(ratio,         na.rm = TRUE),
      q75 = quantile(ratio, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
}

###############################################################################
# Load top-10% diet arrays
###############################################################################
cat("Loading top-10% RMSE diet arrays...\n")

fish_arrays <- readRDS(file.path(OUTPUT_DIR, "full_diet_top10pct_fishing_all_sims.rds"))
clim_arrays <- readRDS(file.path(OUTPUT_DIR, "full_diet_top10pct_climate_only_all_sims.rds"))

n_sims <- length(fish_arrays)
cat(sprintf("  %d simulation pairs loaded\n", n_sims))

# Report predator/prey names from first array
preds_available <- dimnames(fish_arrays[[1]])$predator
preys_available <- dimnames(fish_arrays[[1]])$prey
cat(sprintf("  Predators: %s\n", paste(preds_available, collapse = ", ")))
cat(sprintf("  Prey items: %s\n\n", paste(preys_available, collapse = ", ")))

###############################################################################
# Extract per-sim krill consumption by predator group
###############################################################################
cat("Extracting per-sim krill consumption by group...\n")

cat("  Baleen + minke whales (fishing)...\n")
f_baleen_minke <- extract_group_krill(fish_arrays, PREDS_BALEEN_MINKE)
c_baleen_minke <- extract_group_krill(clim_arrays, PREDS_BALEEN_MINKE)

cat("  Toothed whales (fishing)...\n")
f_toothed <- extract_group_krill(fish_arrays, PREDS_TOOTHED)
c_toothed <- extract_group_krill(clim_arrays, PREDS_TOOTHED)

cat("  Fishes (fishing)...\n")
f_fish <- extract_group_krill(fish_arrays, PREDS_FISH)
c_fish <- extract_group_krill(clim_arrays, PREDS_FISH)

cat("  Seals (fishing)...\n")
f_seals <- extract_group_krill(fish_arrays, PREDS_SEALS)
c_seals <- extract_group_krill(clim_arrays, PREDS_SEALS)

cat("  All predators (sum)...\n")
f_total <- sum_sim_lists(f_baleen_minke, f_toothed, f_fish, f_seals)
c_total <- sum_sim_lists(c_baleen_minke, c_toothed, c_fish, c_seals)

###############################################################################
# Compute ratios
###############################################################################
cat("\nComputing ratios...\n")

cat("  Total all predators...\n")
total_summary <- compute_annual_ratio(f_total, c_total)

cat("  Large baleen + minke whales...\n")
whale_summary <- compute_annual_ratio(f_baleen_minke, c_baleen_minke)

cat("  Fishes...\n")
fish_summary  <- compute_annual_ratio(f_fish, c_fish)

cat(sprintf("\n  Year range: %d – %d\n", min(total_summary$year), max(total_summary$year)))
cat(sprintf("  Total median range:  %.3f – %.3f\n", min(total_summary$med), max(total_summary$med)))
cat(sprintf("  Whale median range:  %.3f – %.3f\n", min(whale_summary$med), max(whale_summary$med)))
cat(sprintf("  Fish  median range:  %.3f – %.3f\n\n", min(fish_summary$med),  max(fish_summary$med)))

###############################################################################
# Build long-format ratio data
###############################################################################
total_summary$group <- "All predators"
whale_summary$group <- "Large baleen + minke whales"
fish_summary$group  <- "Fishes"

ratio_all <- bind_rows(total_summary, whale_summary, fish_summary)
ratio_all$group <- factor(ratio_all$group,
                           levels = c("All predators",
                                      "Large baleen + minke whales",
                                      "Fishes"))

LINE_COLS <- c(
  "All predators"               = LINE_COL,
  "Large baleen + minke whales" = WHALE_COL,
  "Fishes"                      = FISH_COL
)
LINE_WIDTHS <- c(
  "All predators"               = 1.0,
  "Large baleen + minke whales" = 0.9,
  "Fishes"                      = 0.9
)

###############################################################################
# Plot — identical layout to krill_ratio_all_predators_timeseries_no_catch.R
###############################################################################
cat("Generating plot...\n")

p <- ggplot() +

  # ---- Reference line at ratio = 1 ----
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50",
             linewidth = 0.7) +
  annotate("text", x = YEAR_MIN, y = 1, label = "Unexploited",
           hjust = 0, vjust = -0.45, size = 3.0, colour = "grey50") +

  # ---- Krill fishing period vlines ----
  geom_vline(xintercept = KRILL_START, linetype = "dashed",
             colour = VLINE_COL, linewidth = 0.6) +
  annotate("text", x = KRILL_START, y = Inf, label = "Start of krill fishing",
           hjust = -0.06, vjust = 1.4, size = 2.9, colour = VLINE_COL) +
  geom_vline(xintercept = KRILL_END, linetype = "dashed",
             colour = VLINE_COL, linewidth = 0.6) +
  annotate("text", x = KRILL_END, y = Inf, label = "End of krill fishing",
           hjust = -0.06, vjust = 1.4, size = 2.9, colour = VLINE_COL) +

  # ---- IQR ribbons ----
  geom_ribbon(data    = total_summary,
              mapping = aes(x = year, ymin = q25, ymax = q75),
              fill = FILL_COL, alpha = 0.55, colour = NA) +
  geom_ribbon(data    = whale_summary,
              mapping = aes(x = year, ymin = q25, ymax = q75),
              fill = WHALE_COL, alpha = 0.25, colour = NA) +
  geom_ribbon(data    = fish_summary,
              mapping = aes(x = year, ymin = q25, ymax = q75),
              fill = FISH_COL, alpha = 0.25, colour = NA) +

  # ---- Median lines ----
  geom_line(data    = ratio_all,
            mapping = aes(x = year, y = med,
                          colour    = group,
                          linewidth = group)) +
  scale_colour_manual(values = LINE_COLS,  name = "Predator group") +
  scale_linewidth_manual(values = LINE_WIDTHS, name = "Predator group") +

  # ---- Axes ----
  scale_x_continuous(
    name   = "Year",
    breaks = seq(YEAR_MIN, 2010, by = 10),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    name   = "Exploited / Unexploited Antarctic krill consumption (ratio)",
    breaks = seq(0, 1.25, by = 0.25),
    labels = function(x) sprintf("%.2f", x),
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.08))
  ) +

  # ---- Theme ----
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 40, hjust = 1),
    legend.position  = "bottom",
    legend.box       = "horizontal",
    legend.key.size  = unit(0.45, "cm"),
    legend.text      = element_text(size = 8.5),
    legend.title     = element_text(size = 9, face = "bold"),
    plot.margin      = margin(8, 12, 4, 4, "pt")
  ) +
  guides(
    colour    = guide_legend(order = 1, override.aes = list(linewidth = 1.1)),
    linewidth = "none"
  )

###############################################################################
# Save
###############################################################################
png_path <- file.path(OUTPUT_DIR, "krill_ratio_top10pct_timeseries.png")
pdf_path <- file.path(OUTPUT_DIR, "krill_ratio_top10pct_timeseries.pdf")

ggsave(png_path, p, width = 8, height = 5.5, dpi = 300)
ggsave(pdf_path, p, width = 8, height = 5.5)

cat(sprintf("Saved: %s\n", png_path))
cat(sprintf("Saved: %s\n", pdf_path))
cat("Done.\n")
