# =============================================================================
# RD02 -- trophic level of every species in the reference model
#
# READ THIS BEFORE USING mizer::getTrophicLevel() ON ANY therMizer MODEL.
#
# Called directly on this params it returns orca 192.1, toothfishes 60.9, sperm
# whales 31.5, baleen whales 27.5 -- nonsense, but plausible-looking nonsense in
# the sense that it runs without warning and the ORDER of species is roughly
# right. The cause is an interaction between mizer's internals and any extension
# that rescales encounter:
#
#   mizer:::getTrophicLevel.MizerParams forms
#       cumA = sum_w (1 - f) * E_tl * dw/g          E_tl built DIRECTLY from
#                                                   params@search_vol %*% kernel
#       cumB = sum_w (1 - f) * encounter * dw/g     encounter from getEncounter(),
#                                                   which DOES dispatch to
#                                                   therMizerEncounter
#       TL   = 1 + cumA / cumB
#
# so the temperature factor sits in the denominator but not the numerator and
# fails to cancel. TL is inflated by ~1/temp_eff (0.09-0.71 here, by species) and
# the error compounds recursively, because each species' inflated TL feeds into
# prey_mass_tl for its predators. That is why orca -- top of the web, temp_eff
# 0.12 -- ends up worst.
#
# ther_trophic_level() in RD00_common.R is mizer's own function with E_tl scaled
# by the same factor. Section 1 below asserts that transcription is faithful by
# reproducing mizer's output BIT-IDENTICALLY once the scaling is switched off.
#
# This is worth reporting upstream: it affects every mizer extension that
# rescales encounter, not just therMizer.
#
# WHAT THIS SCRIPT PRODUCES
#   1. the demonstration above, printed
#   2. trophic_level_by_size          TL vs body size, all species
#   3. trophic_level_vs_ecopath       mean TL vs McCormack et al. (2020) Prydz
#                                     Bay Ecopath Table 2
#   4. trophic_level_sensitivity      TL over the w_R / beta_R grid. NOT
#                                     cosmetic: with mizer's defaults the
#                                     largest resource particles are assigned
#                                     TL 5.0, which lifts every consumer above
#                                     them. The model-Ecopath offset cannot be
#                                     read without this panel.
#
# USAGE  Rscript "Reference model diet assessment/RD02_trophic_level.R"
# =============================================================================

source(file.path("Reference model diet assessment", "RD00_common.R"))
cat("=== RD02: trophic level ===\n")

W_R_DEFAULT    <- 1e-10
BETA_R_DEFAULT <- 1000

# Trophic levels from McCormack et al. (2020) Prydz Bay Ecopath, Table 2.
# Transcribed from ecosystem_assessment_v3.R:219-230 rather than sourced,
# because that file runs a full assessment on load.
ECOPATH_TL <- c(
  "mesozooplankton" = 3.272, "other krill" = 2.398,
  "other macrozooplankton" = 3.231, "antarctic krill" = 2.398,
  "salps" = 2.284, "mesopelagic fishes" = 3.539,
  "bathypelagic fishes" = 4.055, "shelf and coastal fishes" = 4.281,
  "toothfishes" = 4.966, "flying birds" = 4.103,
  "small divers" = 3.787, "medium divers" = 4.999,
  "large divers" = 5.075, "leopard seals" = 4.858,
  "squids" = 4.336, "minke whales" = 3.955,
  "orca" = 5.301, "sperm whales" = 5.342,
  "baleen whales" = 3.867)

# =============================================================================
# 1. the incompatibility, demonstrated and the fix validated
# =============================================================================
cat("\n[1] mizer::getTrophicLevel() on this params\n")

tl_raw <- ther_trophic_level(PARAMS, temp_eff = NULL)          # corrected
tl_bad <- getTrophicLevel(PARAMS)                              # as-is

# the third route: strip the therMizer rate functions entirely. The temperature
# factor then appears in neither numerator nor denominator, so it also cancels,
# but the dw/g lifetime weighting now uses untempered growth.
PLAIN <- PARAMS
PLAIN@rates_funcs$Encounter       <- "mizerEncounter"
PLAIN@rates_funcs$PredRate        <- "mizerPredRate"
PLAIN@rates_funcs$EReproAndGrowth <- "mizerEReproAndGrowth"
tl_plain <- getTrophicLevel(PLAIN)

mean_tl <- function(a) {
  m <- as.matrix(a)
  setNames(round(apply(m, 1, mean, na.rm = TRUE), 3), SPECIES)
}
cmp <- data.frame(
  species        = SPECIES,
  getTrophicLevel_asis = mean_tl(tl_bad),
  corrected      = mean_tl(tl_raw),
  rates_reset    = mean_tl(tl_plain),
  ecopath        = ECOPATH_TL[SPECIES],
  temp_eff       = round(temp_effect_scalar(PARAMS, RD_YEAR), 3),
  row.names = NULL)
print(cmp, row.names = FALSE)
cat(sprintf("\ninflation factor implied by temp_eff (1/temp_eff): %.1f to %.1f\n",
            min(1 / cmp$temp_eff), max(1 / cmp$temp_eff)))
cat(sprintf("corrected vs rates_reset: max |diff| = %.4f  (the dw/g weighting)\n",
            max(abs(cmp$corrected - cmp$rates_reset))))

cat("\n[1b] assertion: the transcription is faithful\n")
tl_check <- ther_trophic_level(PLAIN, temp_eff = rep(1, NS))
d <- max(abs(as.matrix(tl_check) - as.matrix(tl_plain)), na.rm = TRUE)
same_na <- identical(is.na(as.matrix(tl_check)), is.na(as.matrix(tl_plain)))
cat(sprintf("  ther_trophic_level(scaling off) vs mizer::getTrophicLevel(): "))
cat(sprintf("max |diff| = %.3g, NA pattern identical = %s\n", d, same_na))
if (!(d == 0 && same_na))
  stop("ther_trophic_level() is not a faithful transcription of mizer's ",
       "function; do not trust its output.", call. = FALSE)

# =============================================================================
# 2. trophic level by body size
# =============================================================================
cat("\n[2] trophic level by size\n")
TL <- tl_raw
tl_df <- as.data.frame(as.table(as.matrix(TL)), stringsAsFactors = FALSE)
names(tl_df) <- c("species", "w_lab", "trophic_level")
tl_df$size_g <- PARAMS@w[match(tl_df$w_lab, dimnames(PARAMS@initial_n)$w)]
tl_df <- tl_df[!is.na(tl_df$trophic_level), ]
tl_df$species_label <- factor(PRED_DISPLAY[tl_df$species], levels = PRED_DISPLAY)

# mizer's own plot method for the returned ArraySpeciesBySize object
sv(plot(TL) + theme_rd() + labs(title = "Trophic level at size, reference model"),
   "trophic_level_by_size", 9, 6)

p_facet <- ggplot(tl_df, aes(log10(size_g), trophic_level)) +
  geom_hline(data = data.frame(
    species_label = factor(PRED_DISPLAY[names(ECOPATH_TL)],
                           levels = PRED_DISPLAY),
    tl = as.numeric(ECOPATH_TL)),
    aes(yintercept = tl), colour = "grey45", linetype = "dashed",
    linewidth = 0.4) +
  geom_line(colour = "#1a6faf", linewidth = 0.8) +
  facet_wrap(~species_label, ncol = 4, scales = "free_x") +
  theme_rd() +
  labs(x = "Body size (log10 g)", y = "Trophic level",
       subtitle = paste("Solid: reference model (temperature-consistent).",
                        "Dashed: McCormack et al. (2020) Prydz Bay Ecopath."))
sv(p_facet, "trophic_level_by_size_panels", 12, 12)

# =============================================================================
# 3. mean trophic level against Ecopath
# =============================================================================
cat("\n[3] model vs Ecopath\n")
mean_by_sp <- tl_df %>% group_by(species) %>%
  summarise(tl_model = mean(trophic_level), .groups = "drop") %>%
  mutate(tl_ecopath = as.numeric(ECOPATH_TL[species]),
         diff = tl_model - tl_ecopath,
         label = PRED_DISPLAY[species])
lim <- range(c(mean_by_sp$tl_model, mean_by_sp$tl_ecopath))
p_ec <- ggplot(mean_by_sp, aes(tl_ecopath, tl_model)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50",
              linetype = "dashed") +
  geom_point(size = 2.4, colour = "#1a6faf") +
  ggrepel::geom_text_repel(aes(label = label), size = 2.6,
                           max.overlaps = 30, seed = 1) +
  coord_equal(xlim = lim, ylim = lim) +
  theme_rd() +
  labs(x = "Trophic level, McCormack et al. (2020) Prydz Bay Ecopath",
       y = "Trophic level, reference model (mean over size)",
       subtitle = sprintf(
         "Model is higher for %d of %d groups; mean offset %+.2f",
         sum(mean_by_sp$diff > 0), nrow(mean_by_sp), mean(mean_by_sp$diff)))
sv(p_ec, "trophic_level_vs_ecopath", 7, 7)

# =============================================================================
# 4. sensitivity to the resource trophic-level parameters
# =============================================================================
cat("\n[4] w_R / beta_R sensitivity\n")
# mizer assigns the resource TL_R(w) = max(1, 1 + log(w / w_R) / log(beta_R)).
# With the defaults and this model's w_pp_cutoff = 100 g, the largest resource
# particles are TL 5.0 -- as high as a modelled top predator. Every consumer of
# the resource inherits that.
w_R_grid    <- c(1e-12, 1e-10, 1e-8)
beta_R_grid <- c(100, 1000, 10000)
grid <- expand.grid(w_R = w_R_grid, beta_R = beta_R_grid)

res_curve <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  tlr <- pmax(1, 1 + log(PARAMS@w_full / grid$w_R[i]) / log(grid$beta_R[i]))
  keep <- PARAMS@w_full <= PARAMS@resource_params$w_pp_cutoff
  data.frame(w = PARAMS@w_full[keep], tl_R = tlr[keep],
             w_R = grid$w_R[i], beta_R = grid$beta_R[i])
}))
p_res <- ggplot(res_curve, aes(log10(w), tl_R, colour = factor(beta_R))) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~sprintf("w_R = %g", w_R), nrow = 1) +
  scale_colour_viridis_d(name = expression(beta[R]), end = 0.85) +
  theme_rd() +
  labs(x = "Resource particle size (log10 g)",
       y = "Assigned trophic level of the resource",
       subtitle = paste("mizer default is w_R = 1e-10, beta_R = 1000, which",
                        "makes a 100 g 'plankton' particle trophic level 5.0"))

sens <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  a <- ther_trophic_level(PARAMS, w_R = grid$w_R[i], beta_R = grid$beta_R[i])
  data.frame(species = SPECIES,
             tl = apply(as.matrix(a), 1, mean, na.rm = TRUE),
             w_R = grid$w_R[i], beta_R = grid$beta_R[i], row.names = NULL)
}))
sens$label <- factor(PRED_DISPLAY[sens$species], levels = PRED_DISPLAY)
p_sens <- ggplot(sens, aes(factor(beta_R), tl, colour = factor(w_R),
                           group = factor(w_R))) +
  geom_hline(data = data.frame(
    label = factor(PRED_DISPLAY[names(ECOPATH_TL)], levels = PRED_DISPLAY),
    tl = as.numeric(ECOPATH_TL)),
    aes(yintercept = tl), colour = "grey45", linetype = "dashed",
    linewidth = 0.4) +
  geom_line(linewidth = 0.6) + geom_point(size = 1.4) +
  facet_wrap(~label, ncol = 5, scales = "free_y") +
  scale_colour_viridis_d(name = expression(w[R]), end = 0.85,
                         labels = function(x) sprintf("%g", as.numeric(x))) +
  theme_rd() +
  labs(x = expression(beta[R]), y = "Mean trophic level",
       subtitle = "Dashed: McCormack et al. (2020) Prydz Bay Ecopath")
sv(p_res / p_sens + plot_layout(heights = c(1, 3)),
   "trophic_level_sensitivity", 13, 13)

cat(sprintf("  TL range across the 9 (w_R, beta_R) settings, by species:\n"))
sw <- sens %>% group_by(species) %>%
  summarise(min = round(min(tl), 2), max = round(max(tl), 2),
            span = round(max(tl) - min(tl), 2), .groups = "drop") %>%
  mutate(ecopath = as.numeric(ECOPATH_TL[species])) %>%
  arrange(desc(span))
print(as.data.frame(sw), row.names = FALSE)

# Which setting sits closest to Ecopath? Not a calibration -- w_R and beta_R
# describe the microbial food web feeding the resource spectrum and should be
# argued from first principles -- but it puts a scale on the offset, showing
# whether the model-Ecopath gap is a real structural difference or is inside
# what this convention alone can move.
fit <- sens %>% mutate(ecopath = as.numeric(ECOPATH_TL[species])) %>%
  group_by(w_R, beta_R) %>%
  summarise(rmse = sqrt(mean((tl - ecopath)^2)),
            bias = mean(tl - ecopath), .groups = "drop") %>%
  arrange(rmse) %>%
  mutate(across(c(rmse, bias), ~round(.x, 3)))
cat("\n  distance to Ecopath over the (w_R, beta_R) grid, best first:\n")
print(as.data.frame(fit), row.names = FALSE)
cat(sprintf(paste("  mizer default (w_R = 1e-10, beta_R = 1000): RMSE %.3f,",
                  "bias %+.3f\n"),
            fit$rmse[fit$w_R == W_R_DEFAULT & fit$beta_R == BETA_R_DEFAULT],
            fit$bias[fit$w_R == W_R_DEFAULT & fit$beta_R == BETA_R_DEFAULT]))

# =============================================================================
# 5. tables
# =============================================================================
cat("\n[5] tables\n")
wcsv(tl_df[, c("species", "size_g", "trophic_level")], "RD02_trophic_level_by_size")
wcsv(cmp, "RD02_trophic_level_methods")
wcsv(sens[, c("species", "w_R", "beta_R", "tl")], "RD02_trophic_level_sensitivity")
wcsv(mean_by_sp %>%
       select(species, tl_model, tl_ecopath, diff) %>%
       mutate(across(where(is.numeric), ~round(.x, 3))),
     "RD02_trophic_level_summary")

cat("\n=== mean trophic level, reference model vs Ecopath ===\n")
print(as.data.frame(mean_by_sp %>%
        mutate(species = PRED_DISPLAY[species],
               across(where(is.numeric), ~round(.x, 2))) %>%
        select(species, tl_model, tl_ecopath, diff) %>%
        arrange(desc(tl_model))), row.names = FALSE)
cat("\nRD02 complete.\n")