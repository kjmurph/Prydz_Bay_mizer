###############################################################################
# Antarctic krill consumption ratio: exploited / unexploited
# Three overlaid timeseries, 1901-2010:
#   - All predators combined (ribbon + line)  — total community signal
#   - Baleen + minke whales (line)             — losers under exploitation
#   - Fish (line)                              — winners under exploitation
#
# Secondary right axis: stacked observed catch (baleen + minke + krill) in kt
#
# Note: squids (~1%) and seabirds (~0.3%) excluded from "all predators" total
# (full-timeseries data not extracted for those groups).
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

OUTPUT_DIR <- "whale_consumption_outputs"

###############################################################################
# Colours
###############################################################################
FILL_COL    <- "#4daf8d"          # ribbon fill — total all predators
LINE_COL    <- "#1a4d38"          # median line — total all predators
WHALE_COL   <- "#e878b8"          # baleen + minke (losers)
FISH_COL    <- "#0072B2"          # fish (winners)

KRILL_START <- 1974
KRILL_END   <- 1996
VLINE_COL   <- "#e8534a"

CATCH_COLS <- c(
  "baleen.whales"   = "#e878b8",
  "minke.whales"    = "#3dbbd4",
  "antarctic.krill" = "#e8534a"
)
CATCH_LABELS <- c(
  "baleen.whales"   = "Baleen whale catch",
  "minke.whales"    = "Minke whale catch",
  "antarctic.krill" = "Krill catch"
)

###############################################################################
# Helper: compute per-sim annual ratio from paired fishing / climate-only lists
###############################################################################
compute_annual_ratio <- function(fish_list, clim_list, year_min = 1901) {
  n_sims <- length(fish_list)
  rows   <- vector("list", n_sims)
  for (i in seq_len(n_sims)) {
    f   <- fish_list[[i]]
    cl  <- clim_list[[i]]
    mrg <- merge(f, cl, by = "year", suffixes = c("_f", "_c"))
    mrg$ratio <- ifelse(mrg$total_consumption_c > 0,
                        mrg$total_consumption_f / mrg$total_consumption_c,
                        NA_real_)
    rows[[i]] <- mrg[mrg$year >= year_min, c("year", "ratio")]
  }
  ratio_df <- do.call(rbind, rows)

  ratio_df %>%
    group_by(year) %>%
    summarise(
      q25 = quantile(ratio, 0.25, na.rm = TRUE),
      med = median(ratio,         na.rm = TRUE),
      q75 = quantile(ratio, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
}

###############################################################################
# Helper: sum multiple per-sim lists element-wise (same year order assumed)
###############################################################################
sum_sim_lists <- function(...) {
  lists <- list(...)
  n <- length(lists[[1]])
  lapply(seq_len(n), function(i) {
    total <- lists[[1]][[i]]$total_consumption
    for (lst in lists[-1]) total <- total + lst[[i]]$total_consumption
    data.frame(year = lists[[1]][[i]]$year,
               total_consumption = total)
  })
}

###############################################################################
# Load data
###############################################################################
cat("Loading per-sim krill timeseries...\n")

fw  <- readRDS(file.path(OUTPUT_DIR, "fishing_all_whales_krill_all_sims.rds"))
ff  <- readRDS(file.path(OUTPUT_DIR, "fishing_fish_krill_all_sims.rds"))
fsl <- readRDS(file.path(OUTPUT_DIR, "fishing_seals_krill_all_sims.rds"))
cw  <- readRDS(file.path(OUTPUT_DIR, "climate_only_all_whales_krill_all_sims.rds"))
cf  <- readRDS(file.path(OUTPUT_DIR, "climate_only_fish_krill_all_sims.rds"))
csl <- readRDS(file.path(OUTPUT_DIR, "climate_only_seals_krill_all_sims.rds"))

# Baleen + minke whales only (for the loser line)
fbk <- readRDS(file.path(OUTPUT_DIR, "fishing_baleen_krill_all_sims.rds"))
cbk <- readRDS(file.path(OUTPUT_DIR, "climate_only_baleen_krill_all_sims.rds"))

n_sims <- length(fw)
cat(sprintf("  %d simulation pairs loaded\n\n", n_sims))

###############################################################################
# Compute ratios
###############################################################################
cat("Computing ratios...\n")

# All predators: sum whales + fish + seals per sim
fish_total <- sum_sim_lists(fw, ff, fsl)
clim_total <- sum_sim_lists(cw, cf, csl)

cat("  Total all predators...\n")
total_summary <- compute_annual_ratio(fish_total, clim_total)

cat("  Baleen + minke whales...\n")
whale_summary <- compute_annual_ratio(fbk, cbk)

cat("  Fish...\n")
fish_summary  <- compute_annual_ratio(ff, cf)

cat(sprintf("  Year range: %d - %d\n", min(total_summary$year),
            max(total_summary$year)))
cat(sprintf("  Total median range: %.3f - %.3f\n",
            min(total_summary$med), max(total_summary$med)))
cat(sprintf("  Whale median range: %.3f - %.3f\n",
            min(whale_summary$med), max(whale_summary$med)))
cat(sprintf("  Fish  median range: %.3f - %.3f\n\n",
            min(fish_summary$med),  max(fish_summary$med)))

###############################################################################
# Observed catch (secondary axis)
###############################################################################
cat("Loading observed catch...\n")
catch_raw <- read.csv("yield_observed_timeseries.csv")

all_years <- data.frame(Year = 1901:2010)
catch_sel <- catch_raw[catch_raw$Year <= 2010,
                        c("Year", "baleen.whales", "minke.whales", "antarctic.krill")]
catch_sel <- merge(all_years, catch_sel, by = "Year", all.x = TRUE)
catch_sel[is.na(catch_sel)] <- 0

G_TO_KT <- 1e-9
catch_sel$baleen.whales   <- catch_sel$baleen.whales   * G_TO_KT
catch_sel$minke.whales    <- catch_sel$minke.whales    * G_TO_KT
catch_sel$antarctic.krill <- catch_sel$antarctic.krill * G_TO_KT

MAX_CATCH_KT    <- max(rowSums(catch_sel[, -1]))
# Map max stacked catch to the top of the ratio y-axis
# Set based on combined axis range (whale ratio goes down to ~0, fish goes slightly >1)
CATCH_MAX_RATIO <- 1.0
SCALE_FACTOR    <- CATCH_MAX_RATIO / MAX_CATCH_KT

BREAK_STEP    <- 100
sec_breaks_kt <- seq(0, ceiling(MAX_CATCH_KT / BREAK_STEP) * BREAK_STEP, by = BREAK_STEP)

catch_long <- tidyr::pivot_longer(
  catch_sel,
  cols     = c("baleen.whales", "minke.whales", "antarctic.krill"),
  names_to = "group", values_to = "catch_kt"
)
catch_long$catch_scaled <- catch_long$catch_kt * SCALE_FACTOR
catch_long$group <- factor(catch_long$group,
                            levels = c("baleen.whales", "minke.whales", "antarctic.krill"))

###############################################################################
# Build long-format ratio data for multiple lines
###############################################################################
total_summary$group <- "All predators"
whale_summary$group <- "Large baleen + minke whales"
fish_summary$group  <- "Fishes"

# Combine — ribbon only for total; lines for all
ratio_all <- bind_rows(total_summary, whale_summary, fish_summary)
ratio_all$group <- factor(ratio_all$group,
                           levels = c("All predators",
                                      "Large baleen + minke whales",
                                      "Fishes"))

LINE_COLS <- c(
  "All predators"              = LINE_COL,
  "Large baleen + minke whales" = WHALE_COL,
  "Fishes"                      = FISH_COL
)
LINE_TYPES <- c(
  "All predators"              = "solid",
  "Large baleen + minke whales" = "solid",
  "Fishes"                      = "solid"
)
LINE_WIDTHS <- c(
  "All predators"              = 1.0,
  "Large baleen + minke whales" = 0.9,
  "Fishes"                      = 0.9
)

###############################################################################
# Plot
###############################################################################
cat("Generating plot...\n")

p <- ggplot() +

  # ---- Background: stacked observed catch ----
  geom_area(data    = catch_long,
            mapping = aes(x = Year, y = catch_scaled, fill = group),
            position = "stack", alpha = 0.45, colour = NA) +
  scale_fill_manual(
    values = CATCH_COLS,
    labels = CATCH_LABELS,
    name   = "Observed catch"
  ) +

  # ---- Reference line at ratio = 1 ----
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50",
             linewidth = 0.7) +
  annotate("text", x = 1901, y = 1, label = "Unexploited",
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

  # ---- Median lines for all three groups ----
  geom_line(data    = ratio_all,
            mapping = aes(x = year, y = med,
                          colour    = group,
                          linewidth = group)) +
  scale_colour_manual(values = LINE_COLS,  name = "Predator group") +
  scale_linewidth_manual(values = LINE_WIDTHS, name = "Predator group") +

  # ---- Axes ----
  scale_x_continuous(
    name   = "Year",
    breaks = seq(1901, 2010, by = 10),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    name   = "Exploited / Unexploited Antarctic krill consumption (ratio)",
    breaks = seq(0, 1.25, by = 0.25),
    labels = function(x) sprintf("%.2f", x),
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.08)),
    sec.axis = sec_axis(
      transform = ~ . / SCALE_FACTOR,
      name      = "Observed catch (thousand tonnes)",
      breaks    = sec_breaks_kt
    )
  ) +

  # ---- Theme ----
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(angle = 40, hjust = 1),
    axis.title.y.right = element_text(colour = "grey35"),
    axis.text.y.right  = element_text(colour = "grey35"),
    legend.position    = "bottom",
    legend.box         = "horizontal",
    legend.key.size    = unit(0.45, "cm"),
    legend.text        = element_text(size = 8.5),
    legend.title       = element_text(size = 9, face = "bold"),
    plot.margin        = margin(8, 12, 4, 4, "pt")
  ) +
  guides(
    colour    = guide_legend(order = 1, override.aes = list(linewidth = 1.1)),
    linewidth = "none",
    fill      = guide_legend(order = 2)
  )

###############################################################################
# Save
###############################################################################
png_path <- file.path(OUTPUT_DIR, "krill_ratio_all_predators_timeseries.png")
pdf_path <- file.path(OUTPUT_DIR, "krill_ratio_all_predators_timeseries.pdf")

ggsave(png_path, p, width = 10, height = 5.5, dpi = 300)
ggsave(pdf_path, p, width = 10, height = 5.5)

cat(sprintf("Saved: %s\n", png_path))
cat(sprintf("Saved: %s\n", pdf_path))
cat("Done.\n")
