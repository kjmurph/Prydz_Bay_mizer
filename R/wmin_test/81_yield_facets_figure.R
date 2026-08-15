# =============================================================================
# Phase 81 -- per-species yield facets, modelled vs observed
#
# Consumes phase 80's extraction. STYLING IS TRANSCRIBED FROM
# 47_plot_cut_yields.R rather than reinvented, so this figure keys to the rest
# of the manuscript set:
#   - two ribbons (5-95% at alpha .2, 25-75% at alpha .3) + median line
#   - observed points twice: filled in the species colour, then shape 21 in
#     black, so zero-catch years stay visible against the ribbon
#   - era vlines at 1961 and 2010, dashed
#   - PSEUDO-LOG y, not scale_y_log10: the observed series contain true zeros in
#     the unfished years and log10 silently DROPS them. sigma is set below the
#     smallest non-zero value so the compression is invisible where it matters.
#   - the canonical manuscript palette (sp_cols), NOT hue_pal()
#
# USAGE  Rscript R/wmin_test/81_yield_facets_figure.R
# ENV    P81_IN, P81_SUFFIX, P81_OUT
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(scales)
})

OUT_LARGE <- "Output_large_files/wmin_test"
SUFFIX <- Sys.getenv("P81_SUFFIX", "p77n166")
IN  <- Sys.getenv("P81_IN",
                  file.path(OUT_LARGE,
                            sprintf("80_yield_by_species_%s.rds", SUFFIX)))
FIG <- Sys.getenv("P81_OUT", file.path("Manuscript figures",
                                       "Supplemental figures"))
dir.create(FIG, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(IN)) stop("missing ", IN, " -- run phase 80 first", call. = FALSE)

D <- readRDS(IN)
Y <- D$raw; obs <- D$observed
cat("=== Phase 81: yield facets ===\n")
cat("input:", basename(IN), "| members:", D$meta$n_members,
    "| base:", basename(D$meta$base), "\n")

# --- optional member cut -----------------------------------------------------
# Phase 80 extracts every usable member. P81_CUTS_RDS/P81_CUT restrict the plot
# to a selection from a phase-93-style cuts object; unset, nothing is filtered
# and the figure is what it always was. P81_BANDS=iqr drops the outer 5-95%
# ribbon, which is what the top-10% variant wants -- a selected set has a
# spread narrow enough that two nested ribbons only crowd the panel.
CUTS_RDS <- Sys.getenv("P81_CUTS_RDS", "")
BANDS <- Sys.getenv("P81_BANDS", "both")
stopifnot(BANDS %in% c("both", "iqr"))
if (nzchar(CUTS_RDS)) {
  CR <- readRDS(CUTS_RDS)
  cut_nm <- Sys.getenv("P81_CUT", "FULL usable")
  if (is.null(CR$cuts[[cut_nm]]))
    stop("no cut '", cut_nm, "' in ", CUTS_RDS, " -- have: ",
         paste(names(CR$cuts), collapse = " | "), call. = FALSE)
  sel <- as.integer(CR$cuts[[cut_nm]])
  n0 <- length(unique(Y$sim_index))
  Y <- Y[Y$sim_index %in% sel, , drop = FALSE]
  if (!nrow(Y)) stop("the cut left no members in this extraction", call. = FALSE)
  cat("cut '", cut_nm, "': ", length(unique(Y$sim_index)), " of ", n0,
      " members\n", sep = "")
}
cat("bands:", if (BANDS == "both") "5-95% + IQR" else "IQR only", "\n")

fished <- sort(unique(obs$Species[obs$obs_t > 0]))
cat("fished species:", length(fished), "\n")

sp_cols <- c(
  "antarctic krill"          = "#F8766D", "bathypelagic fishes" = "#B07A00",
  "shelf and coastal fishes" = "#93AA00", "squids"              = "#E8B33C",
  "toothfishes"              = "#00C19F", "minke whales"        = "#00B9E3",
  "orca"                     = "#619CFF", "sperm whales"        = "#DB72FB",
  "baleen whales"            = "#FF61C3", "mesopelagic fishes"  = "#D39200",
  "leopard seals"            = "#E07B39", "medium divers"       = "#2B6CB0",
  "large divers"             = "#4C8FD0", "flying birds"        = "#9E9E9E",
  "small divers"             = "#BDBDBD", "mesozooplankton"     = "#6A1B9A",
  "other krill"              = "#8E44AD", "other macrozooplankton" = "#9B59B6",
  "salps"                    = "#C39BD3")
missing_col <- setdiff(fished, names(sp_cols))
if (length(missing_col))
  stop("no manuscript colour for: ", paste(missing_col, collapse = ", "),
       call. = FALSE)

# 47_plot_cut_yields.R:191-200, transcribed
label_dynamic_decimals <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (v == 0) return("0")
    if (abs(v) >= 1) return(paste0(comma(v, accuracy = 1), " t"))
    d <- max(0, ceiling(-log10(abs(v))))
    paste0(formatC(v, format = "f", digits = d), " t")
  }, character(1))
}

rib <- Y %>% filter(Species %in% fished) %>%
  group_by(Species, Year) %>%
  summarise(med = median(yield_t, na.rm = TRUE),
            q25 = quantile(yield_t, .25, na.rm = TRUE),
            q75 = quantile(yield_t, .75, na.rm = TRUE),
            q05 = quantile(yield_t, .05, na.rm = TRUE),
            q95 = quantile(yield_t, .95, na.rm = TRUE), .groups = "drop") %>%
  mutate(Species = factor(Species, levels = fished))
obs_pts <- obs %>% filter(Species %in% fished) %>%
  mutate(Species = factor(Species, levels = fished))

nz <- c(rib$med[rib$med > 0], obs_pts$obs_t[obs_pts$obs_t > 0])
sigma <- max(min(nz, na.rm = TRUE) / 10, 1e-8)
cat(sprintf("pseudo-log sigma %.3g (smallest non-zero %.3g)\n",
            sigma, min(nz, na.rm = TRUE)))

p <- ggplot() +
  {if (BANDS == "both")
    geom_ribbon(data = rib, aes(Year, ymin = q05, ymax = q95, fill = Species),
                alpha = .2)} +
  geom_ribbon(data = rib, aes(Year, ymin = q25, ymax = q75, fill = Species),
              alpha = .3) +
  geom_line(data = rib, aes(Year, med, colour = Species), linewidth = 1) +
  geom_point(data = obs_pts, aes(Year, obs_t, colour = Species), size = 1.1) +
  geom_point(data = obs_pts, aes(Year, obs_t), shape = 1, size = 1.1,
             colour = "black") +
  geom_vline(xintercept = c(1961, 2010), linetype = "dashed") +
  scale_colour_manual(values = sp_cols) +
  scale_fill_manual(values = sp_cols) +
  # EXPLICIT breaks on both axes. Letting pseudo_log_trans choose its own puts
  # several labels within a few pixels of each other on free_y facets, and
  # 10-year x breaks collide once nine panels are three across.
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = sigma, base = 10),
                     breaks = c(0, 10^seq(-4, 6, by = 2)),
                     labels = label_dynamic_decimals) +
  scale_x_continuous(breaks = seq(1920, 2010, by = 30)) +
  facet_wrap(~ Species, ncol = 3, scales = "free_y") +
  theme_bw(base_size = 13) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 40, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  labs(x = "Year", y = expression(Yield~(t~y^{-1})))

STEM <- sprintf("yield_facets_%s%s%s", SUFFIX,
                if (nzchar(CUTS_RDS)) "_top" else "",
                if (BANDS == "iqr") "_iqr" else "")
png_out <- file.path(FIG, sprintf("%s.png", STEM))
pdf_out <- file.path(FIG, sprintf("%s.pdf", STEM))
ggsave(png_out, p, width = 14, height = 8, dpi = 300)
ggsave(pdf_out, p, width = 14, height = 8)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n", sep = "")

# how well does the median track the observations, per species?
chk <- rib %>% inner_join(obs_pts, by = c("Species", "Year")) %>%
  filter(obs_t > 0) %>% group_by(Species) %>%
  summarise(n_obs_yr = n(),
            med_model_t = round(median(med), 2),
            med_obs_t = round(median(obs_t), 2),
            ratio = round(median(med) / median(obs_t), 3), .groups = "drop")
print(as.data.frame(chk), row.names = FALSE)