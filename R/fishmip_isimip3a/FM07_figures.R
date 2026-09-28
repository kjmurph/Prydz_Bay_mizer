###############################################################################
# FM07_figures.R -- figures of the submitted FishMIP outputs: yield and biomass
#                   per functional group, and the aggregates tcb and tc.
#
# Run:  Rscript run_p104q10.R R/fishmip_isimip3a/FM07_figures.R
#       (after FM05; reads the staged submission_csv/)
#
# Writes, under FM_OUT_DIR/figures/:
#   yield_species.{png,pdf}    csp for the nine fished or whaled groups, against
#                              observed catch (catchobs)
#   biomass_species.{png,pdf}  bsp for all 19 groups, histsoc against nat
#   tcb.{png,pdf}              tcb, histsoc against nat
#   tc.{png,pdf}               tc for all groups and for the fisheries-only
#                              variant, each against summed observed catch
#
# THE DATA ARE THE SUBMISSION. Every plotted value is read from the staged
# submission_csv/ files -- the 203-member quantiles in g m^-2, exactly as
# uploaded -- and nothing is recomputed, so each figure is a picture of what was
# submitted. FM06 check 8 asserts that split_csv/ carries the same values.
#
# STYLING IS TRANSCRIBED, not reinvented, so these key to the manuscript set:
#   yield    R/wmin_test/81_yield_facets_figure.R -- 5-95% and IQR ribbons and
#            the median; observed points drawn twice (filled in the group
#            colour, then a black outline) so zero-catch years stay visible;
#            dashed lines at 1961 and 2010; PSEUDO-LOG y, because log10 silently
#            drops the true zeros; the manuscript palette
#   biomass  Manuscript scripts/F05_supp_biomass_grid_rebuilt167.R -- nat as a
#            grey IQR with dashed bounds and a thin median, histsoc as a
#            coloured IQR and a thick median, linear free y
#
# WHAT DIFFERS FROM THOSE FIGURES
#   - units are the submitted g m^-2, not tonnes
#   - the biomass figure has one panel per submitted group (19), not F05's 12
#     with four summed aggregates, which appear in no submitted file
#   - observed catch is drawn from 1930, where the record starts. catchobs pads
#     1841-1929 with zeros that are not observations; phase 81's observed series
#     also starts in 1930
#   - each figure carries a one-line caption, since no manuscript caption goes
#     with it here
###############################################################################

source("R/fishmip_isimip3a/FM00_common.R")

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(scales)
})

SUB_DIR <- file.path(FM_OUT_DIR, "submission_csv")
FIG_DIR <- fm_dir(FM_OUT_DIR, "figures")

FIRST_OBS_YEAR <- 1930L   # catchobs before this is zero padding, not record

## One colour for the two aggregates, which have no group colour of their own.
AGG_COL <- "#2B6CB0"

## Transcribed from 81_yield_facets_figure.R:67-77 -- covers all 19 groups.
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

N_MEMBERS <- FM_N_MEMBERS_EXPECTED

## ---------------------------------------------------------------------------
## Reading the submission
## ---------------------------------------------------------------------------

#' One staged submission CSV, with a calendar Year added from `time`.
rd <- function(soc, var, sens = FISHMIP_SENS) {
  d <- if (identical(sens, FISHMIP_SENS)) soc else paste0(soc, "_", sens)
  p <- file.path(SUB_DIR, d, fishmip_filename(soc, var, sens))
  if (!file.exists(p))
    stop("missing ", p, " -- run FM05_stage_bundle.R first", call. = FALSE)
  df <- read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
  if (any(df$time %% 365L != 0L))
    stop("time in ", basename(p), " is not on whole 365-day years", call. = FALSE)
  df$Year <- as.integer(df$time %/% 365L) + 1841L
  df
}

## ---------------------------------------------------------------------------
## Shared pieces
## ---------------------------------------------------------------------------

#' Pseudo-log axis labels in powers of ten. Character, not expressions, so an
#' out-of-range (NA) break passes through harmlessly.
label_pow10 <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (v == 0) return("0")
    e <- as.integer(round(log10(v)))
    if (abs(e) <= 2) format(10^e, scientific = FALSE, drop0trailing = TRUE)
    else sprintf("1e%d", e)
  }, character(1))
}

#' Pseudo-log sigma set below the smallest non-zero value, so the compression is
#' invisible where the data are (81_yield_facets_figure.R:105-108), with phase
#' 81's floor of 1e-8 t converted to g m^-2.
SIGMA_FLOOR <- 1e-8 * 1e6 / MODEL_DOMAIN_AREA   # 1e-8 t, as g m^-2
pl_sigma <- function(...) {
  v <- c(...)
  v <- v[is.finite(v) & v > 0]
  if (!length(v)) stop("no non-zero values to scale the axis on", call. = FALSE)
  max(min(v) / 10, SIGMA_FLOOR)
}

#' Powers-of-ten breaks for a pseudo-log axis, plus 0. Breaks within a factor of
#' 100 of sigma sit in the compressed band just above zero and print on top of
#' the "0" label, so they are dropped. Phase 81 did the same by hand: its lowest
#' break, 1e-4 t, was fixed well clear of its sigma.
pl_breaks <- function(sigma) {
  b <- 10^seq(-16, 2, by = 2)
  c(0, b[b >= 100 * sigma])
}

save_fig <- function(p, stem, width, height) {
  png_out <- file.path(FIG_DIR, paste0(stem, ".png"))
  pdf_out <- file.path(FIG_DIR, paste0(stem, ".pdf"))
  ggsave(png_out, p, width = width, height = height, dpi = 300)
  ggsave(pdf_out, p, width = width, height = height)
  message("  wrote ", png_out, "\n  wrote ", pdf_out)
}

YIELD_CAPTION <- sprintf(paste0(
  "Median (line), 25-75%% and 5-95%% (ribbons) across %d ensemble members, ",
  "histsoc. Points: observed catch from %d. Dashed: 1961 and 2010. ",
  "Pseudo-log axis, so zero is shown."), N_MEMBERS, FIRST_OBS_YEAR)
BIOMASS_CAPTION <- sprintf(paste0(
  "Colour: histsoc median and 25-75%%. Grey: nat (no fishing) median and ",
  "25-75%%, dashed bounds. %d ensemble members."), N_MEMBERS)

## ===========================================================================
## 1. Yield by species -- phase 81
## ===========================================================================
message("1. yield by species")

csp <- rd("histsoc", "csp")
obs <- rd("histsoc", "catchobs")

fished <- sort(unique(obs$species[obs$catch > 0]))
missing_col <- setdiff(fished, names(sp_cols))
if (length(missing_col))
  stop("no manuscript colour for: ", paste(missing_col, collapse = ", "),
       call. = FALSE)
message("  fished or whaled groups: ", length(fished))

rib <- csp %>% filter(species %in% fished) %>%
  mutate(species = factor(species, levels = fished))
obs_pts <- obs %>% filter(species %in% fished, Year >= FIRST_OBS_YEAR) %>%
  mutate(species = factor(species, levels = fished))

sigma_y <- pl_sigma(rib$median, obs_pts$catch)
message(sprintf("  pseudo-log sigma %.3g g m-2", sigma_y))

p_yield <- ggplot() +
  geom_ribbon(data = rib, aes(Year, ymin = q05, ymax = q95, fill = species),
              alpha = .2) +
  geom_ribbon(data = rib, aes(Year, ymin = q25, ymax = q75, fill = species),
              alpha = .3) +
  geom_line(data = rib, aes(Year, median, colour = species), linewidth = 1) +
  geom_point(data = obs_pts, aes(Year, catch, colour = species), size = 1.1) +
  geom_point(data = obs_pts, aes(Year, catch), shape = 1, size = 1.1,
             colour = "black") +
  geom_vline(xintercept = c(1961, 2010), linetype = "dashed") +
  scale_colour_manual(values = sp_cols) +
  scale_fill_manual(values = sp_cols) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = sigma_y, base = 10),
                     breaks = pl_breaks(sigma_y),
                     labels = label_pow10) +
  scale_x_continuous(breaks = seq(1920, 2010, by = 30)) +
  facet_wrap(~ species, ncol = 3, scales = "free_y") +
  theme_bw(base_size = 13) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 40, hjust = 1),
        axis.text.y = element_text(size = 8),
        plot.caption = element_text(size = 9, hjust = 0)) +
  labs(x = "Year", y = expression(Catch~density~(g~m^{-2}~y^{-1})),
       caption = paste("csp.", YIELD_CAPTION))

save_fig(p_yield, "yield_species", width = 14, height = 8)

## How well the median tracks the record, per group (81_yield_facets_figure.R:
## 161-168). A ratio, so it is the same in g m^-2 as in tonnes.
chk <- rib %>%
  inner_join(obs_pts %>% select(species, Year, catch), by = c("species", "Year")) %>%
  filter(catch > 0) %>% group_by(species) %>%
  summarise(n_obs_yr = n(),
            ratio_model_obs = round(median(median) / median(catch), 3),
            .groups = "drop")
cat("\nmedian model / median observed, over years with recorded catch:\n")
print(as.data.frame(chk), row.names = FALSE)

## ===========================================================================
## 2. Biomass by species -- F05
## ===========================================================================
message("\n2. biomass by species")

bh <- rd("histsoc", "bsp")
bn <- rd("nat", "bsp")

## Model order is ascending w_inf; F05 reads largest first, so reverse it.
sp_levels <- rev(unique(bh$species))
if (length(sp_levels) != 19L || !setequal(sp_levels, bn$species))
  stop("expected the same 19 groups in both bsp files", call. = FALSE)
bh$species <- factor(bh$species, levels = sp_levels)
bn$species <- factor(bn$species, levels = sp_levels)

#' F05's two-arm panel: nat drawn first in grey, histsoc on top in colour.
#' `colour = NULL` colours histsoc by group; a colour fixes it.
f05_layers <- function(fish, clim, colour = NULL) {
  hist <- if (is.null(colour)) list(
    geom_ribbon(data = fish, aes(x = Year, ymin = q25, ymax = q75, fill = species),
                alpha = 0.3),
    geom_line(data = fish, aes(x = Year, y = median, colour = species),
              linewidth = 1.1))
  else list(
    geom_ribbon(data = fish, aes(x = Year, ymin = q25, ymax = q75),
                fill = colour, alpha = 0.3),
    geom_line(data = fish, aes(x = Year, y = median), colour = colour,
              linewidth = 1.1))
  c(list(
    geom_ribbon(data = clim, aes(x = Year, ymin = q25, ymax = q75),
                fill = "grey82", alpha = 0.6, colour = NA),
    geom_line(data = clim, aes(x = Year, y = q25),
              colour = "grey62", linewidth = 0.3, linetype = "dashed"),
    geom_line(data = clim, aes(x = Year, y = q75),
              colour = "grey62", linewidth = 0.3, linetype = "dashed"),
    geom_line(data = clim, aes(x = Year, y = median),
              colour = "grey38", linewidth = 0.4)),
    hist)
}
f05_theme <- theme_bw(base_size = 13) +
  theme(legend.position  = "none",
        strip.text       = element_text(face = "bold"),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.caption     = element_text(size = 9, hjust = 0))

p_bio <- ggplot() +
  f05_layers(bh, bn) +
  facet_wrap(~ species, ncol = 4, scales = "free_y") +
  scale_fill_manual(values = sp_cols) +
  scale_colour_manual(values = sp_cols) +
  f05_theme +
  labs(x = "Year", y = expression(Biomass~density~(g~m^{-2})),
       caption = paste("bsp.", BIOMASS_CAPTION))

save_fig(p_bio, "biomass_species", width = 16, height = 13)

## ===========================================================================
## 3. tcb -- F05, one panel
## ===========================================================================
message("\n3. tcb")

th <- rd("histsoc", "tcb") %>% mutate(panel = "tcb: total consumer biomass density")
tn <- rd("nat", "tcb")     %>% mutate(panel = "tcb: total consumer biomass density")

p_tcb <- ggplot() +
  f05_layers(th, tn, colour = AGG_COL) +
  facet_wrap(~ panel) +
  f05_theme +
  labs(x = "Year", y = expression(Biomass~density~(g~m^{-2})),
       caption = paste(BIOMASS_CAPTION, "All 19 groups, full size range."))

save_fig(p_tcb, "tcb", width = 9, height = 5.5)

## ===========================================================================
## 4. tc -- phase 81, all groups and fisheries only
## ===========================================================================
message("\n4. tc")

TC_ALL <- "tc: all groups"
TC_NOW <- "tc: fisheries only (histsoc_nowhales)"

tc_rib <- bind_rows(
  rd("histsoc", "tc") %>% mutate(panel = TC_ALL),
  rd("histsoc", "tc", FISHMIP_SENS_NOWHALES) %>% mutate(panel = TC_NOW)) %>%
  mutate(panel = factor(panel, levels = c(TC_ALL, TC_NOW)))

## Observed totals: catchobs summed over groups within each year.
obs_tot <- function(df, panel) df %>% filter(Year >= FIRST_OBS_YEAR) %>%
  group_by(Year) %>% summarise(catch = sum(catch), .groups = "drop") %>%
  mutate(panel = panel)
tc_obs <- bind_rows(
  obs_tot(obs, TC_ALL),
  obs_tot(rd("histsoc", "catchobs", FISHMIP_SENS_NOWHALES), TC_NOW)) %>%
  mutate(panel = factor(panel, levels = c(TC_ALL, TC_NOW)))

sigma_tc <- pl_sigma(tc_rib$median, tc_obs$catch)
message(sprintf("  pseudo-log sigma %.3g g m-2", sigma_tc))

p_tc <- ggplot() +
  geom_ribbon(data = tc_rib, aes(Year, ymin = q05, ymax = q95),
              fill = AGG_COL, alpha = .2) +
  geom_ribbon(data = tc_rib, aes(Year, ymin = q25, ymax = q75),
              fill = AGG_COL, alpha = .3) +
  geom_line(data = tc_rib, aes(Year, median), colour = AGG_COL, linewidth = 1) +
  geom_point(data = tc_obs, aes(Year, catch), colour = AGG_COL, size = 1.1) +
  geom_point(data = tc_obs, aes(Year, catch), shape = 1, size = 1.1,
             colour = "black") +
  geom_vline(xintercept = c(1961, 2010), linetype = "dashed") +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = sigma_tc, base = 10),
                     breaks = pl_breaks(sigma_tc),
                     labels = label_pow10) +
  scale_x_continuous(breaks = seq(1920, 2010, by = 30)) +
  facet_wrap(~ panel, ncol = 2, scales = "free_y") +
  theme_bw(base_size = 13) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 40, hjust = 1),
        axis.text.y = element_text(size = 8),
        plot.caption = element_text(size = 9, hjust = 0)) +
  labs(x = "Year", y = expression(Catch~density~(g~m^{-2}~y^{-1})),
       caption = paste0(YIELD_CAPTION, "\nObserved: catchobs summed over groups. ",
                        "Fisheries only drops minke whales, orca, sperm whales ",
                        "and baleen whales."))

save_fig(p_tc, "tc", width = 12, height = 5)

## ===========================================================================
## 5. Tie to the manuscript yield series (report only)
## ===========================================================================
## Phase 80 is what the manuscript's yield figure plots, in tonnes. Converted
## back to tonnes over the same 203 members, the submitted csp medians should
## reproduce its medians -- if they do, this figure and the manuscript's show
## the same catch.
cat("\n5. Tie to phase 80 (report only)\n")
F80 <- file.path(OUT_LARGE, "80_yield_by_species_p104q10.rds")
if (file.exists(F80)) {
  D80 <- readRDS(F80)
  mem <- fm_usable_members()
  y80 <- D80$raw %>%
    filter(sim_index %in% mem, Species %in% fished, Year <= 2010) %>%
    group_by(Species, Year) %>%
    summarise(n = n(), med_t = median(yield_t), .groups = "drop")
  j <- y80 %>% inner_join(
    rib %>% transmute(Species = as.character(species), Year = as.numeric(Year),
                      fm_t = median * MODEL_DOMAIN_AREA / 1e6),
    by = c("Species", "Year"))
  nzr <- j$med_t > 0
  cat(sprintf(paste0("  members matched in phase 80: %d of %d | %d group-years ",
                     "| max rel diff (non-zero) %.3e | max abs diff at zero %.3e t\n"),
              max(y80$n), length(mem), nrow(j),
              if (any(nzr)) max(abs(j$fm_t[nzr] / j$med_t[nzr] - 1)) else NA,
              if (any(!nzr)) max(abs(j$fm_t[!nzr])) else 0))
} else {
  cat("  ", F80, " not present -- tie not checked\n", sep = "")
}

cat("\nFM07 done. Figures in ", FIG_DIR, "\n", sep = "")
