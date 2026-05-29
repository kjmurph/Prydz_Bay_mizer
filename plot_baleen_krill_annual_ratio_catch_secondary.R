###############################################################################
# Baleen whale Antarctic krill consumption: exploited / unexploited ratio
# Annual median line + IQR ribbon, 1901-2010
# Secondary right axis: stacked observed catch (baleen + minke + krill) in kt
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

OUTPUT_DIR <- "whale_consumption_outputs"

# ---------------------------------------------------------------------------
# Load per-simulation ratio data
# ---------------------------------------------------------------------------
cat("Loading fishing ensemble baleen krill...\n")
fish_sims <- readRDS(file.path(OUTPUT_DIR, "fishing_baleen_krill_all_sims.rds"))
cat("Loading climate-only ensemble baleen krill...\n")
clim_sims <- readRDS(file.path(OUTPUT_DIR, "climate_only_baleen_krill_all_sims.rds"))

n_sims <- length(fish_sims)
cat(sprintf("  %d simulation pairs\n\n", n_sims))

cat("Computing annual ratios...\n")
ratio_rows <- vector("list", n_sims)
for (i in seq_len(n_sims)) {
  f <- fish_sims[[i]]
  c <- clim_sims[[i]]
  merged <- merge(f, c, by = "year", suffixes = c("_fish", "_clim"))
  merged$ratio <- ifelse(merged$total_consumption_clim > 0,
                         merged$total_consumption_fish / merged$total_consumption_clim,
                         NA_real_)
  ratio_rows[[i]] <- merged[, c("year", "ratio")]
}
ratio_df <- do.call(rbind, ratio_rows)
ratio_df <- ratio_df[ratio_df$year >= 1901, ]

annual_summary <- ratio_df %>%
  group_by(year) %>%
  summarise(
    q25 = quantile(ratio, 0.25, na.rm = TRUE),
    med = median(ratio,         na.rm = TRUE),
    q75 = quantile(ratio, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("  Years: %d - %d\n", min(annual_summary$year), max(annual_summary$year)))
cat(sprintf("  Median range: %.3f - %.3f\n\n",
            min(annual_summary$med), max(annual_summary$med)))

# ---------------------------------------------------------------------------
# Load and prepare observed catch data
# ---------------------------------------------------------------------------
cat("Loading observed catch data...\n")
# read.csv converts spaces -> dots in column names by default
catch_raw <- read.csv("yield_observed_timeseries.csv")
# Columns of interest: Year, baleen.whales, minke.whales, antarctic.krill

# Restrict to plot range; fill 1901-1929 (pre-data) with zeros
all_years   <- data.frame(Year = 1901:2010)
catch_sel   <- catch_raw[catch_raw$Year <= 2010, c("Year", "baleen.whales",
                                                     "minke.whales",
                                                     "antarctic.krill")]
catch_sel   <- merge(all_years, catch_sel, by = "Year", all.x = TRUE)
catch_sel[is.na(catch_sel)] <- 0

# Convert g -> thousand tonnes (kt)
G_TO_KT <- 1e-9
catch_sel$baleen.whales   <- catch_sel$baleen.whales   * G_TO_KT
catch_sel$minke.whales    <- catch_sel$minke.whales    * G_TO_KT
catch_sel$antarctic.krill <- catch_sel$antarctic.krill * G_TO_KT

# Scale factor: max stacked catch maps to CATCH_MAX_RATIO on the 0-1 ratio axis
MAX_CATCH_KT    <- max(rowSums(catch_sel[, -1]))
CATCH_MAX_RATIO <- 1.0    # max stacked catch maps to ratio = 1 (full axis)
SCALE_FACTOR    <- CATCH_MAX_RATIO / MAX_CATCH_KT  # ratio units per kt

cat(sprintf("  Max stacked catch: %.1f kt\n", MAX_CATCH_KT))
cat(sprintf("  Scale factor: %.6f ratio/kt\n\n", SCALE_FACTOR))

# Long format; stacking order: baleen (bottom), minke, krill (top)
catch_long <- tidyr::pivot_longer(
  catch_sel,
  cols      = c("baleen.whales", "minke.whales", "antarctic.krill"),
  names_to  = "group",
  values_to = "catch_kt"
)
catch_long$catch_scaled <- catch_long$catch_kt * SCALE_FACTOR
catch_long$group <- factor(catch_long$group,
                            levels = c("baleen.whales", "minke.whales",
                                       "antarctic.krill"))

# ---------------------------------------------------------------------------
# Colours
# ---------------------------------------------------------------------------
FILL_COL    <- "#4daf8d"
LINE_COL    <- "#1a4d38"
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

# Secondary axis breaks: round to nearest 100 kt
BREAK_STEP   <- 100
sec_breaks_kt <- seq(0, ceiling(MAX_CATCH_KT / BREAK_STEP) * BREAK_STEP,
                     by = BREAK_STEP)

# ---------------------------------------------------------------------------
# Plot
# ---------------------------------------------------------------------------
cat("Generating ribbon plot with secondary catch axis...\n")

p <- ggplot(annual_summary, aes(x = year)) +

  # Stacked catch areas (faded background) - drawn first so they sit behind
  geom_area(data    = catch_long,
            mapping = aes(x = Year, y = catch_scaled, fill = group),
            position = "stack", alpha = 0.45, colour = NA) +

  scale_fill_manual(
    values = CATCH_COLS,
    labels = CATCH_LABELS,
    name   = "Observed catch"
  ) +

  # Reference line at ratio = 1
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50",
             linewidth = 0.7) +
  annotate("text", x = 1901, y = 1, label = "Unexploited",
           hjust = 0, vjust = -0.5, size = 3.2, colour = "grey50") +

  # Krill fishing period vlines
  geom_vline(xintercept = KRILL_START, linetype = "dashed",
             colour = VLINE_COL, linewidth = 0.7) +
  annotate("text", x = KRILL_START, y = Inf, label = "Start of krill fishing",
           hjust = -0.07, vjust = 1.4, size = 3.0, colour = VLINE_COL) +
  geom_vline(xintercept = KRILL_END, linetype = "dashed",
             colour = VLINE_COL, linewidth = 0.7) +
  annotate("text", x = KRILL_END, y = Inf, label = "End of krill fishing",
           hjust = -0.07, vjust = 1.4, size = 3.0, colour = VLINE_COL) +

  # IQR ribbon and median line (foreground)
  geom_ribbon(aes(ymin = q25, ymax = q75),
              fill = FILL_COL, alpha = 0.6, colour = NA) +
  geom_line(aes(y = med),
            colour = LINE_COL, linewidth = 0.8) +

  scale_x_continuous(
    name   = "Year",
    breaks = seq(1901, 2010, by = 10),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    name   = "Exploited / Unexploited Antarctic krill consumption (ratio)",
    breaks = seq(0, 1, by = 0.25),
    labels = function(x) sprintf("%.2f", x),
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.08)),
    sec.axis = sec_axis(
      transform = ~ . / SCALE_FACTOR,
      name      = "Observed catch (thousand tonnes)",
      breaks    = sec_breaks_kt
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(angle = 40, hjust = 1),
    axis.title.y.right = element_text(colour = "grey35"),
    axis.text.y.right  = element_text(colour = "grey35"),
    legend.position    = "bottom",
    legend.key.size    = unit(0.45, "cm"),
    legend.text        = element_text(size = 8.5),
    legend.title       = element_text(size = 9, face = "bold"),
    plot.margin        = margin(8, 12, 4, 4, "pt")
  )

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------
png_path <- file.path(OUTPUT_DIR, "baleen_krill_annual_ratio_catch_secondary.png")
pdf_path <- file.path(OUTPUT_DIR, "baleen_krill_annual_ratio_catch_secondary.pdf")

ggsave(png_path, p, width = 10, height = 5.5, dpi = 300)
ggsave(pdf_path, p, width = 10, height = 5.5)

cat(sprintf("Saved: %s\n", png_path))
cat(sprintf("Saved: %s\n", pdf_path))
cat("Done.\n")
