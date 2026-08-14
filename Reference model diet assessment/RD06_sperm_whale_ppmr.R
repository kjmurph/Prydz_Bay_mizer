# =============================================================================
# RD06 -- what the sperm whale PPMR should be, and whether the Williams value
#         would make cephalopods more available
#
# THE FLAG THIS RESOLVES
#   RD00b noticed that "Sperm Whales" and "Southern Right Whales" carry an
#   identical preferred-PPMR mean AND SD in csvs/predator_parameters_updated.csv
#   and flagged the sperm whale row as suspect, because its note reads
#   "1/400 according to Williams in Estes whales book" while the value it
#   carries is 1/44082.
#
#   That flag pointed the wrong way. Section 1 below shows the duplication runs
#   sperm whale -> southern right whale, not the reverse:
#
#     - Every rorqual and minke row encodes the SAME prey mass, 1.19 g (an adult
#       E. superba), as 1.19 / w_max. They are prey-mass entries.
#     - The sperm whale row encodes 828 g, sourced to Evans & Hindell (2004).
#       Also a prey-mass entry, and an ecologically sensible one.
#     - The southern right whale row encodes NEITHER. Its implied prey mass is
#       1247.67 g, which is exactly 5.5e7 x the SPERM WHALE ratio -- the
#       signature of copying the ratio cell rather than a prey mass. Its source
#       cell is empty where the sperm whale's is filled. And 1.25 kg is absurd
#       for an animal that filters copepods.
#
#   So the sperm whale value is the original and is sound; the note records
#   Williams as a contrasting literature figure the compiler did not adopt.
#
#   The consequence lands on the BALEEN group, because the original baleen beta
#   was the arithmetic mean of the five large-baleen ratios and the corrupted
#   southern right whale entry is ~1000x the rorqual values, so it dominated
#   that mean. Section 2 quantifies it.
#
# THE SECOND QUESTION: would beta = 400 help the squid?
#   Yes, substantially -- squid go from 2.3% to 11.6% of the diet, a 5.1x rise.
#   That is NOT the intuitive answer and it is worth understanding why, because
#   the naive geometry says the opposite: beta = 400 puts the preferred prey at
#   3.65e7 / 400 = 91,250 g while the squid group tops out at 20,718 g, so the
#   kernel peak sits 4.4x ABOVE the largest squid in the model.
#
#   It helps anyway for two reasons the peak position hides:
#     - sigma = 2 is very wide (in natural-log units), so the kernel's lower
#       tail still blankets the whole squid spectrum even with the peak beyond
#       it; and
#     - squid biomass DENSITY rises steeply with size in this model (see
#       sperm_whale_prey_overlap.png), so pushing the kernel up moves it onto
#       the densest part of the squid spectrum while abandoning the background
#       plankton resource, which collapses from 16.9% of the diet to 0.07%.
#
#   But adopting it would still be wrong, for a reason separate from the squid.
#   Section 3 shows the cost: the preferred prey moves to 91 kg and toothfishes
#   take 83% of the diet. 91 kg is far above any plausible MEAN prey mass for a
#   sperm whale, and the value has no source behind it in this table, whereas
#   828 g does.
#
#   Section 3b is the useful result. Neither lever alone gets squid to the
#   50-90% the diet studies report -- beta peaks at ~16% (around beta = 2000,
#   not at 400), and 8x squid biomass at the current beta gives ~16% -- but the
#   two COMPOUND: 8x biomass with beta ~2000 gives ~60%. So the shortfall is
#   genuinely joint, and RD04's "74% biomass, 26% size" split describes the
#   current operating point rather than a fixed property of the model.
#
# SCOPE
#   This recomputes the sperm whale kernel against the CURRENT prey field. No
#   projection, no recalibration, and no saved object is modified. It answers
#   "what would this kernel see today", not "what would the model settle to".
#
# USAGE  Rscript "Reference model diet assessment/RD06_sperm_whale_ppmr.R"
# =============================================================================

source(file.path("Reference model diet assessment", "RD00_common.R"))
cat("=== RD06: sperm whale PPMR ===\n")

SRC <- file.path("csvs", "predator_parameters_updated.csv")
d <- read.csv(SRC, stringsAsFactors = FALSE)
val <- function(sp, par) d$value[d$group == sp & d$parameter == par]
src <- function(sp, par) d$`source.derivation`[d$group == sp & d$parameter == par]
note <- function(sp, par) d$notes[d$group == sp & d$parameter == par]

# =============================================================================
# 1. which row is the copy?
# =============================================================================
cat("\n[1] the duplicated cells, and what each row actually encodes\n")
SPP <- c("blue whale", "fin whale", "sei whale", "humpback whale",
         "minke whale", "Southern Right Whales", "Sperm Whales")
enc <- do.call(rbind, lapply(SPP, function(s) {
  wm <- val(s, "maximum weight"); r <- val(s, "preferred PPMR mean")
  sd_r <- val(s, "preferred PPMR SD")
  data.frame(species = s, w_max_g = wm, ratio = r, ppmr = 1 / r,
             implied_prey_g = wm * r, implied_prey_sd_g = wm * sd_r,
             source = ifelse(nzchar(src(s, "preferred PPMR mean")),
                             src(s, "preferred PPMR mean"), "(none)"),
             row.names = NULL)
}))
print(as.data.frame(enc %>%
  transmute(species, w_max_g = signif(w_max_g, 3), ratio = signif(ratio, 6),
            ppmr = signif(ppmr, 4), prey_g = signif(implied_prey_g, 6),
            prey_sd_g = signif(implied_prey_sd_g, 4), source)),
  row.names = FALSE)

krill_feeders <- enc[enc$species %in% c("blue whale", "fin whale", "sei whale",
                                        "humpback whale", "minke whale"), ]
cat(sprintf(paste("\n  the five krill feeders all encode the same prey mass:",
                  "%.4g g (sd %.4g), spread = %.3g\n"),
            mean(krill_feeders$implied_prey_g),
            mean(krill_feeders$implied_prey_sd_g),
            diff(range(krill_feeders$implied_prey_g))))
cat(sprintf("  sperm whale encodes %.4g g, sourced to '%s'\n",
            enc$implied_prey_g[enc$species == "Sperm Whales"],
            enc$source[enc$species == "Sperm Whales"]))
cat(sprintf("  sperm whale note reads: '%s'\n",
            note("Sperm Whales", "preferred PPMR mean")))
cat(sprintf(paste("  southern right whale encodes %.6g g with source '%s'",
                  "-- which is exactly\n    its own w_max x the SPERM WHALE",
                  "ratio (%.6g), not a prey mass anyone entered.\n"),
            enc$implied_prey_g[enc$species == "Southern Right Whales"],
            enc$source[enc$species == "Southern Right Whales"],
            enc$ratio[enc$species == "Sperm Whales"]))
stopifnot(identical(enc$ratio[enc$species == "Southern Right Whales"],
                    enc$ratio[enc$species == "Sperm Whales"]))
cat("  VERDICT: the southern right whale row is the copy. The sperm whale\n")
cat("           value stands at PPMR 44,082 (mean prey 828 g).\n")

# =============================================================================
# 2. what the corrupted cell did to the baleen group
# =============================================================================
cat("\n[2] consequence for the baleen whale group\n")
ror <- enc$ratio[enc$species %in% c("blue whale", "fin whale", "sei whale",
                                    "humpback whale")]
srw_bad <- enc$ratio[enc$species == "Southern Right Whales"]
srw_krill <- 1.19 / enc$w_max_g[enc$species == "Southern Right Whales"]
scen <- data.frame(
  scenario = c("as recorded (5 spp, corrupted SRW)",
               "rorquals only, SRW dropped",
               "5 spp, SRW given the same 1.19 g krill as the others"),
  beta = c(1 / mean(c(ror, srw_bad)), 1 / mean(ror),
           1 / mean(c(ror, srw_krill))))
scen$log10_beta <- log10(scen$beta)
BETA_BAL_P51 <- 24680000        # phase 51: w_max(baleen) / w_max(krill)
BETA_BAL_NOW <- PARAMS@species_params$beta[match("baleen whales", SPECIES)]
scen <- rbind(scen, data.frame(
  scenario = "phase 51 value (w_max baleen / w_max krill)",
  beta = BETA_BAL_P51, log10_beta = log10(BETA_BAL_P51)))
if (!isTRUE(all.equal(BETA_BAL_NOW, BETA_BAL_P51)))
  scen <- rbind(scen, data.frame(
    scenario = "beta in the loaded params object",
    beta = BETA_BAL_NOW, log10_beta = log10(BETA_BAL_NOW)))
print(as.data.frame(scen %>% mutate(beta = signif(beta, 6),
                                    log10_beta = round(log10_beta, 4))),
      row.names = FALSE)
cat(sprintf(paste("\n  One corrupted cell moved the original baleen beta by a",
                  "factor of %.0f.\n  Repaired, the compilation gives %.3g",
                  "(log10 %.3f) -- within %.0f%% of the %.3g\n  that phase 51",
                  "adopted on entirely independent grounds.\n"),
            scen$beta[2] / scen$beta[1], scen$beta[2], scen$log10_beta[2],
            100 * abs(scen$beta[2] - BETA_BAL_P51) / BETA_BAL_P51,
            BETA_BAL_P51))
if (!isTRUE(all.equal(BETA_BAL_NOW, BETA_BAL_P51)))
  cat(sprintf(paste("  The loaded params carries %.4g (log10 %.3f) -- the",
                    "repaired compilation value,\n  adopted in phase 54.\n"),
              BETA_BAL_NOW, log10(BETA_BAL_NOW)))

# =============================================================================
# 3. would the Williams value help the squid?
# =============================================================================
cat("\n[3] sweeping the sperm whale beta\n")
IF_   <- match("sperm whales", SPECIES)
PREYB <- prey_field(PARAMS)
INTER <- interaction_full(PARAMS)
PREY_NAMES <- c(SPECIES, "Resource")
K_OCC <- which(OCC[IF_, ])
WTS <- PARAMS@initial_n[IF_, K_OCC] * PARAMS@w[K_OCC] * PARAMS@dw[K_OCC]
WTS <- WTS / sum(WTS)
BETA_USED <- PARAMS@species_params$beta[IF_]
SIGMA_USED <- PARAMS@species_params$sigma[IF_]
BETA_WILLIAMS <- 400

#' Biomass-weighted diet share of the sperm whale under a given kernel.
share_at <- function(beta, sigma = SIGMA_USED) {
  ae <- t(sapply(K_OCC, function(k) {
    phi <- lognormal_pred_kernel(PARAMS@w[k] / PARAMS@w_full, beta = beta,
                                 sigma = sigma)
    as.numeric(phi %*% t(PREYB))
  }))
  contrib <- sweep(ae, 2, INTER[IF_, ], "*")
  sh <- sweep(contrib, 1, pmax(rowSums(contrib), 1e-300), "/")
  setNames(as.numeric(WTS %*% sh), PREY_NAMES)
}

# assertion: reconstructing the kernel at the stored beta must reproduce
# mizer's own getDiet() shares, or the sweep means nothing
gd <- getDiet(PARAMS, proportion = TRUE)[IF_, K_OCC, PREY_NAMES]
gd <- sweep(gd, 1, pmax(rowSums(gd), 1e-300), "/")
dmax <- max(abs(as.numeric(WTS %*% gd) - share_at(BETA_USED)))
cat(sprintf("assert reconstructed kernel reproduces getDiet(): max |diff| = %.3g\n",
            dmax))
# 1e-7 rather than machine precision: mizer builds the kernel by FFT
# convolution with a 1e-18 truncation, while this reconstructs it directly, so
# the two agree to roughly float precision scaled by the magnitudes involved.
# At small beta (large preferred prey) that residual grows to ~1e-8. It is
# numerical noise on a proportion in [0, 1], not a disagreement about the diet.
stopifnot(dmax < 1e-7)

betas <- 10^seq(1, 8, by = 0.05)
sw <- do.call(rbind, lapply(betas, function(b) {
  s <- share_at(b)
  data.frame(beta = b, prey = PREY_NAMES, share = as.numeric(s),
             row.names = NULL)
}))
sq <- sw[sw$prey == "squids", ]
best <- sq$beta[which.max(sq$share)]

key <- data.frame(
  setting = c("Williams 1/400", "as used (Evans & Hindell)",
              "squid-maximising"),
  beta = c(BETA_WILLIAMS, BETA_USED, best))
# nominal w_max, matching the convention the source compilation uses
# ("mean prey size / max body size"), not the top occupied grid bin
W_MAX_NOM <- PARAMS@species_params$w_max[IF_]
key$preferred_prey_g_at_wmax <- W_MAX_NOM / key$beta
key$squid_pct <- vapply(key$beta, function(b) 100 * share_at(b)[["squids"]], 0)
key$toothfish_pct <- vapply(key$beta,
                            function(b) 100 * share_at(b)[["toothfishes"]], 0)
key$small_fish_pct <- vapply(key$beta, function(b) {
  s <- share_at(b)
  100 * sum(s[c("mesopelagic fishes", "bathypelagic fishes",
                "shelf and coastal fishes")])
}, 0)
key$resource_pct <- vapply(key$beta,
                           function(b) 100 * share_at(b)[["Resource"]], 0)
key$mammals_birds_pct <- vapply(key$beta, function(b) {
  s <- share_at(b)
  100 * sum(s[c("leopard seals", "medium divers", "large divers",
                "minke whales", "orca", "flying birds", "small divers")])
}, 0)

SQUID_WMAX <- PARAMS@species_params$w_max[match("squids", SPECIES)]
cat(sprintf("\n  squid group spans %.4g to %.6g g\n",
            PARAMS@species_params$w_min[match("squids", SPECIES)], SQUID_WMAX))
print(as.data.frame(key %>%
  transmute(setting, beta = signif(beta, 6),
            pref_prey_g = signif(preferred_prey_g_at_wmax, 4),
            vs_squid_wmax = sprintf("%.2fx", preferred_prey_g_at_wmax /
                                      SQUID_WMAX),
            squid_pct = round(squid_pct, 3),
            toothfish_pct = round(toothfish_pct, 2),
            small_fish_pct = round(small_fish_pct, 2),
            resource_pct = round(resource_pct, 2),
            mam_bird_pct = round(mammals_birds_pct, 2))),
  row.names = FALSE)

cat(sprintf(paste("\n  Williams beta = 400 raises the squid share from %.2f%%",
                  "to %.2f%% -- a %.2fx rise, NOT a fall.\n"),
            key$squid_pct[2], key$squid_pct[1],
            key$squid_pct[1] / key$squid_pct[2]))
cat(sprintf(paste("  It works despite the kernel peaking %.1fx ABOVE the",
                  "largest squid because sigma = %.1f\n  is wide enough for",
                  "the lower tail to cover the squid spectrum, and because",
                  "moving up\n  abandons the background resource (%.1f%% ->",
                  "%.2f%% of the diet).\n"),
            key$preferred_prey_g_at_wmax[1] / SQUID_WMAX, SIGMA_USED,
            key$resource_pct[2], key$resource_pct[1]))
cat(sprintf(paste("  But the squid share PEAKS at only %.1f%% (beta = %.0f,",
                  "preferred prey %.3g g).\n  beta alone cannot reach the",
                  "50-90%% the diet literature reports, so the kernel is not\n",
                  " the binding constraint.\n"),
            max(sq$share) * 100, best, W_MAX_NOM / best))

# --- the two levers together --------------------------------------------------
# RD04 showed squid standing biomass is the dominant term. Does retuning beta
# on top of more squid compound, or do the two levers substitute?
cat("\n[3b] beta and squid biomass together\n")
B_ROW <- match("squids", PREY_NAMES)
share_at_scaled <- function(beta, f) {
  pb <- PREYB; pb[B_ROW, ] <- pb[B_ROW, ] * f
  ae <- t(sapply(K_OCC, function(k) {
    phi <- lognormal_pred_kernel(PARAMS@w[k] / PARAMS@w_full, beta = beta,
                                 sigma = SIGMA_USED)
    as.numeric(phi %*% t(pb))
  }))
  contrib <- sweep(ae, 2, INTER[IF_, ], "*")
  sh <- sweep(contrib, 1, pmax(rowSums(contrib), 1e-300), "/")
  as.numeric(WTS %*% sh)[B_ROW]
}
lev <- expand.grid(beta = c(BETA_USED, best, BETA_WILLIAMS),
                   squid_x = c(1, 8, 64))
lev$squid_pct <- 100 * mapply(share_at_scaled, lev$beta, lev$squid_x)
lev$beta_lab <- c("in use 44,082", "squid-max 1,995",
                  "Williams 400")[match(lev$beta,
                                        c(BETA_USED, best, BETA_WILLIAMS))]
print(as.data.frame(lev %>%
  transmute(beta = signif(beta, 6), beta_lab,
            squid_biomass_x = squid_x, squid_pct = round(squid_pct, 2)) %>%
  arrange(squid_biomass_x, beta)), row.names = FALSE)
g8 <- function(b) lev$squid_pct[lev$beta == b & lev$squid_x == 8]
cat(sprintf(paste("\n  The two levers COMPOUND. Neither alone reaches the",
                  "50-90%% the diet studies report:\n    beta alone (best",
                  "case)            %.1f%%\n    8x squid biomass alone",
                  "          %.1f%%\n  but together they clear it:\n",
                  "   8x squid biomass + beta ~2000   %.1f%%\n"),
            max(sq$share) * 100, g8(BETA_USED), g8(best)))
cat(paste("  8x is the factor that brings squid biomass level with the\n",
          " bathypelagic fishes (RD04's counterfactual), so it is a",
          "meaningful scale\n  rather than an arbitrary one.\n"))

# =============================================================================
# figure
# =============================================================================
cat("\n[4] figure\n")
grp <- setNames(rep(NA_character_, length(PREY_NAMES)), PREY_NAMES)
for (g in names(PREY_GROUPS))
  grp[intersect(PREY_GROUPS[[g]], PREY_NAMES)] <- g
sw$prey_group <- factor(grp[sw$prey], levels = PREY_GROUP_ORDER)
comp <- sw %>% group_by(beta, prey_group) %>%
  summarise(share = sum(share), .groups = "drop")

vl <- data.frame(beta = c(BETA_WILLIAMS, BETA_USED, best),
                 lab = c("Williams 1/400", "in use: 44,082",
                         sprintf("squid max: %.0f", best)))

p1 <- ggplot(sq, aes(log10(beta), 100 * share)) +
  geom_vline(data = vl, aes(xintercept = log10(beta)), colour = "grey55",
             linetype = "dashed", linewidth = 0.4) +
  geom_text(data = vl, aes(x = log10(beta), y = Inf, label = lab),
            angle = 90, vjust = -0.4, hjust = 1.05, size = 2.5,
            colour = "grey30") +
  geom_line(colour = "#e87c10", linewidth = 1) +
  theme_rd(10) +
  labs(x = NULL, y = "Squid share of sperm whale diet (%)",
       title = "Squid in the sperm whale diet against the feeding kernel",
       subtitle = paste("Prey field, interaction matrix and abundances held at",
                        "their current values. The Williams 1/400 value would",
                        "raise\nsquid 5.1x, from 2.3% to 11.6%, but the share",
                        "peaks at 15.9% near beta = 2000 and cannot reach the",
                        "50-90%\nthe diet studies report. beta is therefore",
                        "not the binding constraint on its own."))

p2 <- ggplot(comp, aes(log10(beta), share, fill = prey_group)) +
  geom_area(colour = NA) +
  geom_vline(data = vl, aes(xintercept = log10(beta)), colour = "grey20",
             linetype = "dashed", linewidth = 0.4) +
  scale_fill_manual(values = PREY_GROUP_COLS, name = "Prey group") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0))) +
  theme_rd(10) + theme(legend.position = "bottom") +
  labs(x = expression(Sperm~whale~log[10]~beta), y = "Diet share",
       subtitle = paste("The cost of the Williams value: at beta = 400",
                        "toothfishes take 83% of the diet and the preferred",
                        "prey is 91 kg,\nfar above any plausible mean prey",
                        "mass for a sperm whale."))

sv(p1 / p2 + plot_layout(heights = c(1, 1.25)),
   "sperm_whale_ppmr_sweep", 9, 8)

wcsv(enc, "RD06_ppmr_source_rows")
wcsv(scen, "RD06_baleen_beta_scenarios")
wcsv(key, "RD06_sperm_whale_key_settings")
wcsv(lev, "RD06_squid_two_levers")
wcsv(sw %>% select(beta, prey, share) %>%
       mutate(across(where(is.numeric), ~signif(.x, 6))),
     "RD06_sperm_whale_beta_sweep")

cat("\nRD06 complete. No parameter changed.\n")