# =============================================================================
# RD04 -- why do squids contribute so little to sperm whale diet?
#
# THE QUESTION
#   The interaction coefficient for sperm whales on squids is 0.5, the joint
#   highest of any prey along with toothfishes, yet squid are ~2% of the
#   modelled sperm whale diet. Is that a mismatch between the sperm whale
#   feeding kernel and squid sizes, or simply low squid biomass?
#
# HOW IT IS ANSWERED
#   A diet share is the product of exactly three things, so separate them:
#
#     share_j  proportional to  theta_ij  x  A_j(w)  x  B_j
#
#     theta_ij  interaction coefficient                      (a model choice)
#     A_j(w)    size availability PER GRAM of prey biomass:
#                 A_j(w) = [ sum_q phi_i(w,w_q) N_j(w_q) w_q dw_q ] / B_j
#               a pure kernel-overlap term, independent of how much prey exists
#     B_j       standing biomass of prey j                   (a model outcome)
#
#   A_j isolates the size question and B_j the abundance question, so the ratio
#   between two prey decomposes cleanly in logs into a size part and an
#   abundance part. A counterfactual then asks it directly: raise squid biomass
#   to that of bathypelagic fishes and see what the diet share becomes.
#
# VALIDATION
#   The decomposition is built from the kernel and the prey field, entirely
#   independently of mizer's getDiet(). Reproducing getDiet()'s shares from it
#   is therefore a real check on both, and the script stops if it fails.
#
# USAGE  Rscript "Reference model diet assessment/RD04_squid_availability.R"
# =============================================================================

source(file.path("Reference model diet assessment", "RD00_common.R"))
cat("=== RD04: prey availability decomposition ===\n")

PK    <- getPredKernel(PARAMS)
PREYB <- prey_field(PARAMS)
INTER <- interaction_full(PARAMS)
PREY_NAMES <- c(SPECIES, "Resource")
B     <- rowSums(PREYB)                       # standing biomass per prey, g
FOCUS <- "sperm whales"
IF_   <- match(FOCUS, SPECIES)

#' theta x A x B decomposition of one predator's diet, per predator size bin.
#' @return list(per_size = [w x prey] share, A = [w x prey], contrib = [w x prey])
decompose <- function(i, preyb = PREYB) {
  k_occ <- which(OCC[i, ])
  Bj <- rowSums(preyb)
  ae <- t(sapply(k_occ, function(k)
    as.numeric(PK[i, k, ] %*% t(preyb))))     # [w x prey], biomass encountered
  dimnames(ae) <- list(as.character(PARAMS@w[k_occ]), PREY_NAMES)
  A       <- sweep(ae, 2, pmax(Bj, 1e-300), "/")
  contrib <- sweep(ae, 2, INTER[i, ], "*")
  share   <- sweep(contrib, 1, pmax(rowSums(contrib), 1e-300), "/")
  list(share = share, A = A, contrib = contrib, Bj = Bj, k_occ = k_occ)
}

D <- decompose(IF_)

# --- validation against getDiet() ---------------------------------------------
gd <- getDiet(PARAMS, proportion = TRUE)[IF_, D$k_occ, , drop = FALSE]
gd <- gd[1, , PREY_NAMES]                     # drop "External" (zero here)
gd <- sweep(gd, 1, pmax(rowSums(gd), 1e-300), "/")
dmax <- max(abs(gd - D$share))
cat(sprintf("assert decomposition reproduces getDiet(): max |diff| = %.3g\n",
            dmax))
stopifnot(dmax < 1e-10)

# --- biomass-weighted table over the predator's ontogeny ----------------------
wts <- PARAMS@initial_n[IF_, D$k_occ] * PARAMS@w[D$k_occ] * PARAMS@dw[D$k_occ]
wts <- wts / sum(wts)
tab <- data.frame(
  prey        = PREY_NAMES,
  theta       = as.numeric(INTER[IF_, ]),
  biomass_g   = as.numeric(B),
  avail_per_g = as.numeric(wts %*% D$A),
  share       = as.numeric(wts %*% D$share),
  row.names = NULL)
tab$share_pct <- 100 * tab$share
tab <- tab[order(-tab$share_pct), ]

cat("\n=== sperm whale diet decomposition, biomass-weighted over ontogeny ===\n")
print(as.data.frame(tab %>%
  transmute(prey, theta = round(theta, 4),
            biomass_g = signif(biomass_g, 4),
            avail_per_g = signif(avail_per_g, 4),
            share_pct = round(share_pct, 3)) %>%
  filter(share_pct > 1e-4)), row.names = FALSE)

# --- the squid vs bathypelagic fish comparison, in logs -----------------------
MIN_LOG_GAP <- 0.3   # ~2-fold; below this the percentage split is meaningless
cmp_pair <- function(a, b) {
  ra <- tab[tab$prey == a, ]; rb <- tab[tab$prey == b, ]
  l_share <- log10(rb$share / ra$share)
  l_theta <- log10(rb$theta / ra$theta)
  l_avail <- log10(rb$avail_per_g / ra$avail_per_g)
  l_biom  <- log10(rb$biomass_g / ra$biomass_g)
  # The three logs sum to l_share, so each can be expressed as a share of it --
  # but only when l_share is comfortably non-zero. For two prey that end up at
  # nearly the same diet share the split is a ratio of small numbers and can
  # exceed 100% with opposing signs, which says nothing. Report NA instead.
  ok <- abs(l_share) >= MIN_LOG_GAP
  pct <- function(x) if (ok) 100 * x / l_share else NA_real_
  data.frame(focal = a, reference = b,
             share_gap_fold = rb$share / ra$share,
             pct_from_theta = pct(l_theta),
             pct_from_size  = pct(l_avail),
             pct_from_biomass = pct(l_biom),
             theta_fold = rb$theta / ra$theta,
             avail_fold = rb$avail_per_g / ra$avail_per_g,
             biomass_fold = rb$biomass_g / ra$biomass_g,
             split_reportable = ok)
}
pairs_df <- rbind(cmp_pair("squids", "bathypelagic fishes"),
                  cmp_pair("squids", "mesopelagic fishes"),
                  cmp_pair("squids", "toothfishes"))
cat("\n=== why squid lose: decomposing the share gap in log space ===\n")
cat("  (pct_* is NA where the two prey end up at similar shares, so the split\n")
cat("   would be a ratio of small numbers)\n")
print(as.data.frame(pairs_df %>%
  transmute(focal, reference,
            gap_fold = round(share_gap_fold, 2),
            theta_fold = round(theta_fold, 3),
            size_fold = round(avail_fold, 3),
            biomass_fold = round(biomass_fold, 2),
            pct_size = round(pct_from_size, 1),
            pct_biomass = round(pct_from_biomass, 1),
            pct_theta = round(pct_from_theta, 1))), row.names = FALSE)

# --- the counterfactual -------------------------------------------------------
cat("\n=== counterfactual: squid biomass raised to each reference ===\n")
cf <- do.call(rbind, lapply(c("bathypelagic fishes", "mesopelagic fishes"),
                            function(ref) {
  f <- B[[ref]] / B[["squids"]]
  pb <- PREYB
  pb["squids", ] <- pb["squids", ] * f          # same size spectrum, more of it
  d2 <- decompose(IF_, pb)
  s2 <- as.numeric(wts %*% d2$share)
  data.frame(scaled_to = ref, factor = f,
             squid_share_pct = 100 * s2[match("squids", PREY_NAMES)],
             bathy_share_pct = 100 * s2[match("bathypelagic fishes",
                                              PREY_NAMES)])
}))
cf <- rbind(data.frame(scaled_to = "(as modelled)", factor = 1,
                       squid_share_pct = tab$share_pct[tab$prey == "squids"],
                       bathy_share_pct =
                         tab$share_pct[tab$prey == "bathypelagic fishes"]),
            cf)
print(as.data.frame(cf %>% mutate(across(where(is.numeric), ~round(.x, 3)))),
      row.names = FALSE)

# --- every predator that is allowed to eat squid ------------------------------
cat("\n=== squid in the diet of every predator with theta > 0 on squid ===\n")
sq <- match("squids", PREY_NAMES)
eaters <- which(PARAMS@interaction[, "squids"] > 0)
all_sq <- do.call(rbind, lapply(eaters, function(i) {
  d <- decompose(i)
  w <- PARAMS@initial_n[i, d$k_occ] * PARAMS@w[d$k_occ] * PARAMS@dw[d$k_occ]
  w <- w / sum(w)
  data.frame(predator = PRED_DISPLAY[[SPECIES[i]]],
             theta_squid = PARAMS@interaction[i, "squids"],
             avail_per_g = as.numeric(w %*% d$A)[sq],
             squid_pct = 100 * as.numeric(w %*% d$share)[sq],
             squid_pct_at_wmax = 100 * d$share[nrow(d$share), sq])
}))
print(as.data.frame(all_sq %>%
  mutate(across(where(is.numeric), ~signif(.x, 4))) %>%
  arrange(desc(squid_pct))), row.names = FALSE)

# =============================================================================
# figures
# =============================================================================
cat("\n[1] the overlap, directly\n")
# The sperm whale kernel at two body sizes, over the biomass-density spectra of
# its main prey. This shows the size question without a summary statistic.
SHOW <- c("squids", "bathypelagic fishes", "mesopelagic fishes",
          "shelf and coastal fishes", "toothfishes", "Resource")
SHOW_COL <- c("squids" = "#e87c10", "bathypelagic fishes" = "#1a6faf",
              "mesopelagic fishes" = "#5aa5dd", "shelf and coastal fishes" =
                "#9ecae1", "toothfishes" = "#08306b", "Resource" = "#c8e6a0")
k_mat <- D$k_occ[which.min(abs(PARAMS@w[D$k_occ] -
                                 PARAMS@species_params$w_mat[IF_]))]
k_max <- D$k_occ[length(D$k_occ)]

kern <- do.call(rbind, lapply(list(c(k_mat, "at w_mat"), c(k_max, "at w_max")),
  function(z) data.frame(w = PARAMS@w_full,
                         phi = PK[IF_, as.integer(z[1]), ],
                         at = z[2])))
kern <- kern[kern$phi > 1e-4, ]

# Both panels must share an x range or the comparison the figure exists to make
# cannot be read off it. Use the span where the kernel is non-negligible, padded.
XL <- range(log10(kern$w)) + c(-1, 1)

spec <- do.call(rbind, lapply(SHOW, function(j) data.frame(
  prey = j, w = PARAMS@w_full,
  dens = PREYB[j, ] / PARAMS@dw_full)))
spec <- spec[spec$dens > 0, ]
p_spec <- ggplot(spec, aes(log10(w), log10(dens), colour = prey)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = SHOW_COL, name = "Prey") +
  coord_cartesian(xlim = XL,
                  ylim = range(log10(spec$dens[log10(spec$w) >= XL[1] &
                                                 log10(spec$w) <= XL[2]]))) +
  theme_rd(10) +
  labs(x = NULL, y = "Biomass density (log10 g per g bin)",
       title = "Prey biomass spectra",
       subtitle = paste(
         "Squid density sits ~3-4 orders below the mesopelagic and",
         "bathypelagic fishes across most of the window, but rises\nsteeply",
         "with size, which is why the squid share of the diet climbs with",
         "sperm whale body size. Note this\nis density: squid TOTAL biomass",
         "is 8x lower, and per gram of prey their size availability is only",
         "2.1x lower."))

p_kern <- ggplot(kern, aes(log10(w), phi, linetype = at)) +
  geom_line(linewidth = 0.8, colour = "grey20") +
  scale_linetype_manual(values = c("at w_mat" = "dashed",
                                   "at w_max" = "solid"), name = NULL) +
  coord_cartesian(xlim = XL) +
  theme_rd(10) +
  labs(x = "Prey mass (log10 g)", y = "Feeding kernel phi",
       title = "Sperm whale feeding kernel")

sv(p_spec / p_kern + plot_layout(heights = c(1.3, 1)),
   "sperm_whale_prey_overlap", 9, 7)

cat("\n[2] the three factors, side by side\n")
fac <- tab %>% filter(share_pct > 1e-3) %>%
  transmute(prey, Interaction = theta,
            `Size availability per g` = avail_per_g,
            `Standing biomass (g)` = biomass_g,
            `Diet share (%)` = share_pct) %>%
  pivot_longer(-prey, names_to = "quantity", values_to = "value") %>%
  mutate(quantity = factor(quantity,
    levels = c("Interaction", "Size availability per g",
               "Standing biomass (g)", "Diet share (%)")),
    prey = factor(prey, levels = rev(tab$prey[tab$share_pct > 1e-3])),
    is_squid = prey == "squids")
p_fac <- ggplot(fac, aes(value, prey, fill = is_squid)) +
  geom_col() +
  facet_wrap(~quantity, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = c("TRUE" = "#e87c10", "FALSE" = "grey65"),
                    guide = "none") +
  scale_x_continuous(trans = "log10", labels = label_number(drop0trailing = TRUE)) +
  theme_rd(9) +
  labs(x = "value (log scale)", y = NULL,
       subtitle = paste("Diet share is the product of the first three.",
                        "Squid (orange) are mid-ranked on size availability",
                        "and low on biomass."))
sv(p_fac, "sperm_whale_diet_factors", 12, 4.5)

cat("\n[3] squid share against predator size\n")
sq_size <- data.frame(size_g = PARAMS@w[D$k_occ],
                      squid_pct = 100 * D$share[, sq],
                      avail = D$A[, sq])
p_sz <- ggplot(sq_size, aes(log10(size_g), squid_pct)) +
  geom_line(colour = "#e87c10", linewidth = 1) +
  geom_point(colour = "#e87c10", size = 1.6) +
  theme_rd(10) +
  labs(x = "Sperm whale body size (log10 g)",
       y = "Squid share of diet (%)",
       subtitle = paste("Squid share rises steeply with sperm whale size as",
                        "the kernel moves onto larger squid."))
sv(p_sz, "sperm_whale_squid_share_by_size", 7, 4.5)

# =============================================================================
# tables
# =============================================================================
cat("\n[4] tables\n")
wcsv(tab %>% mutate(across(where(is.numeric), ~signif(.x, 6))),
     "RD04_prey_availability_decomposition")
wcsv(pairs_df %>% mutate(across(where(is.numeric), ~signif(.x, 6))),
     "RD04_squid_gap_decomposition")
wcsv(cf, "RD04_squid_counterfactual")
wcsv(all_sq, "RD04_squid_in_all_diets")
wcsv(data.frame(size_g = PARAMS@w[D$k_occ], D$share, check.names = FALSE) %>%
       mutate(across(where(is.numeric), ~signif(.x, 6))),
     "RD04_sperm_whale_share_by_size")

cat("\nRD04 complete.\n")