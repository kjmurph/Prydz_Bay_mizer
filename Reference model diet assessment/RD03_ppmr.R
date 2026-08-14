# =============================================================================
# RD03 -- realised ontogenetic predator-prey mass ratio
#
# THE QUESTION
#   Baleen whales should show little ontogenetic shift in prey size. Does the
#   revised lognormal kernel (beta = 2.468e7, sigma = 2) produce a flat realised
#   PPMR, or a shift, and is the shift outside what is observed?
#
# HOW PPMR IS COMPUTED
#   mizer 3.1.0 has no PPMR function -- checked against the bundled API index
#   and the package NAMESPACE -- so it comes from the encounter integrand. For
#   predator i at size w, the biomass encountered from prey bin q is
#
#     c_q = phi_i(w, w_q) * [ sum_j theta_ij N_j(w_q) + theta_iR N_R(w_q) ]
#                         * w_q * dw_q
#
#   and the numbers encountered are n_q = c_q / w_q. search_vol, the feeding
#   level and the therMizer temperature factor are constant in prey size and
#   cancel from every ratio below.
#
#   TWO MEANS, because the choice matters and the two disagree:
#     mean_arith  sum(c_q) / sum(n_q)   -- the abundance-weighted arithmetic
#                 mean prey mass. This is the convention in
#                 "PPMR_Plot_Example/Mean Prey Size vs Predator Size PPMR Plot
#                 Example.rmd" (Richards & Murphy) and it is what the empirical
#                 benchmark measures, so it is the PRIMARY value here.
#     mean_geom   10^( sum(c_q log10 w_q) / sum(c_q) ) -- biomass-weighted
#                 geometric mean, the natural centre of a lognormal kernel and
#                 the one directly comparable to log10(beta).
#
# THREE PREY SETS, reported side by side and never collapsed to one:
#   incl_resource   all prey including the background plankton spectrum -- what
#                   the model actually eats
#   species_only    the 19 modelled prey groups -- closer to what a stomach
#                   contents study measures, since the background resource is
#                   not an identifiable prey item
#   nominal         log10(beta), flat by construction: the reference against
#                   which any realised shift is read
#
# COMPARING TO THE BENCHMARK
#   The observed compilation defines PPMR as max body size / mean prey size, so
#   the like-for-like model value is the one AT w_max, not the ontogenetic mean.
#   Both are reported and the figures mark w_max explicitly.
#
# NO KERNEL PARAMETER IS CHANGED BY THIS SCRIPT. sweep_kernel() at the bottom is
# written and left uncalled for when that decision is taken.
#
# USAGE  Rscript "Reference model diet assessment/RD03_ppmr.R"
#        (run RD00b_build_ppmr_benchmark.R first for the observed bands)
# =============================================================================

source(file.path("Reference model diet assessment", "RD00_common.R"))
cat("=== RD03: realised ontogenetic PPMR ===\n")

PK    <- getPredKernel(PARAMS)            # [sp x w_pred x w_prey], w_prey = w_full
PREYB <- prey_field(PARAMS)               # [(NS+1) x NWF] biomass per bin
INTER <- interaction_full(PARAMS)         # [NS x (NS+1)]
WF    <- PARAMS@w_full
LWF   <- log10(WF)
cat("pred kernel:", paste(dim(PK), collapse = " x "), "\n")

#' Realised prey-size statistics for one predator against a given prey field.
#'
#' @param avail interaction-weighted prey biomass per w_full bin, numeric[NWF]
#' @return data frame, one row per occupied predator size bin
realised <- function(i, avail) {
  k_occ <- which(OCC[i, ])
  out <- lapply(k_occ, function(k) {
    cc <- PK[i, k, ] * avail                 # biomass encountered per prey bin
    s  <- sum(cc)
    if (s <= 0) return(NULL)
    nn <- cc / WF                            # numbers encountered
    cf <- cumsum(cc) / s                     # biomass CDF over prey size
    q  <- function(p) WF[which(cf >= p)[1]]
    data.frame(
      size_g      = PARAMS@w[k],
      mean_arith  = s / sum(nn),
      mean_geom   = 10^(sum(cc * LWF) / s),
      prey_q05    = q(0.05), prey_q50 = q(0.50), prey_q95 = q(0.95))
  })
  out <- do.call(rbind, out)
  if (is.null(out)) return(NULL)
  out$log10_ppmr_arith <- log10(out$size_g / out$mean_arith)
  out$log10_ppmr_geom  <- log10(out$size_g / out$mean_geom)
  out
}

KRILL <- match("antarctic krill", SPECIES)
variants <- list(
  incl_resource = function(i) as.numeric(INTER[i, ] %*% PREYB),
  species_only  = function(i) as.numeric(INTER[i, seq_len(NS)] %*%
                                           PREYB[seq_len(NS), , drop = FALSE]),
  # Antarctic krill alone. The observed compilation's "mean prey size" for
  # rorquals is the mean mass of a krill (blue whale: 1.155e-8 x ~1.5e8 g
  # = 1.7 g, an adult E. superba), so for the krill feeders this is the
  # sharpest like-for-like test of whether the kernel selects krill of a
  # realistic size.
  krill_only    = function(i) PREYB[KRILL, ] * PARAMS@interaction[i, KRILL])

# krill_only is only interpretable for predators that actually take krill;
# for the rest it describes a kernel window over prey they barely eat.
KRILL_SHARE <- local({
  d <- getDiet(PARAMS, proportion = TRUE)
  wts <- PARAMS@initial_n * outer(rep(1, NS), PARAMS@w * PARAMS@dw)
  wts <- wts / pmax(rowSums(wts), 1e-300)
  setNames(rowSums(wts * d[, , "antarctic krill"]), SPECIES)
})
KRILL_MIN <- 0.01
cat("krill_only variant restricted to the",
    sum(KRILL_SHARE >= KRILL_MIN), "groups taking >=1% krill:",
    paste(SPECIES[KRILL_SHARE >= KRILL_MIN], collapse = ", "), "\n")

ppmr <- do.call(rbind, lapply(names(variants), function(v)
  do.call(rbind, lapply(seq_len(NS), function(i) {
    if (v == "krill_only" && KRILL_SHARE[[i]] < KRILL_MIN) return(NULL)
    r <- realised(i, variants[[v]](i))
    if (is.null(r)) return(NULL)
    cbind(species = SPECIES[i], variant = v, r)
  }))))
ppmr$species_label <- factor(PRED_DISPLAY[ppmr$species], levels = PRED_DISPLAY)

NOMINAL <- setNames(log10(PARAMS@species_params$beta), SPECIES)

# assertion 4: against a broad smooth prey field the realised geometric value
# must sit near the nominal one. Antarctic krill feed almost entirely on the
# background resource, a clean power law, so they are the right anchor.
anchor <- ppmr[ppmr$species == "antarctic krill" &
                 ppmr$variant == "incl_resource", ]
cat(sprintf("assert krill realised (geom) vs nominal log10 PPMR: %.3f-%.3f vs %.3f\n",
            min(anchor$log10_ppmr_geom), max(anchor$log10_ppmr_geom),
            NOMINAL[["antarctic krill"]]))
stopifnot(abs(mean(anchor$log10_ppmr_geom) - NOMINAL[["antarctic krill"]]) < 0.3)

# --- observed benchmark -------------------------------------------------------
BEN_F <- file.path(RD_ROOT, "benchmarks", "ppmr_benchmark_groups.csv")
BAND <- NULL
if (file.exists(BEN_F)) {
  BAND <- read.csv(BEN_F, stringsAsFactors = FALSE)
  BAND <- BAND[BAND$metric == "log10_ppmr", ]
  BAND$rorquals_only <- grepl("RORQUALS ONLY", BAND$basis)
  cat("observed benchmark: ", nrow(BAND), " group bands\n", sep = "")
} else {
  cat("NOTE: benchmark not found; run RD00b_build_ppmr_benchmark.R first.\n")
}

# =============================================================================
# summary
# =============================================================================
summ <- ppmr %>% group_by(species, variant) %>%
  summarise(
    n_bins      = n(),
    pred_w_min  = min(size_g), pred_w_max = max(size_g),
    lppmr_at_wmin = log10_ppmr_arith[which.min(size_g)],
    lppmr_at_wmax = log10_ppmr_arith[which.max(size_g)],
    lppmr_range   = diff(range(log10_ppmr_arith)),
    slope         = if (n() > 2)
      unname(coef(lm(log10_ppmr_arith ~ log10(size_g)))[2]) else NA_real_,
    lppmr_geom_at_wmax = log10_ppmr_geom[which.max(size_g)],
    lppmr_geom_range   = diff(range(log10_ppmr_geom)),
    prey_g_at_wmin = mean_arith[which.min(size_g)],
    prey_g_at_wmax = mean_arith[which.max(size_g)],
    prey_fold      = mean_arith[which.max(size_g)] / mean_arith[which.min(size_g)],
    .groups = "drop") %>%
  mutate(nominal_log10_beta = as.numeric(NOMINAL[species]),
         sigma = PARAMS@species_params$sigma[match(species, SPECIES)])

cat("\n=== realised log10 PPMR, arithmetic mean prey mass, all prey ===\n")
print(as.data.frame(summ %>% filter(variant == "incl_resource") %>%
  transmute(species = PRED_DISPLAY[species], n = n_bins,
            at_wmin = round(lppmr_at_wmin, 2),
            at_wmax = round(lppmr_at_wmax, 2),
            range = round(lppmr_range, 2), slope = round(slope, 3),
            nominal = round(nominal_log10_beta, 2),
            prey_fold = round(prey_fold, 1))), row.names = FALSE)

WH <- c("baleen whales", "minke whales", "sperm whales")
cat("\n=== the whales, both prey sets and both means ===\n")
print(as.data.frame(summ %>% filter(species %in% WH) %>%
  transmute(species = PRED_DISPLAY[species], variant,
            arith_wmin = round(lppmr_at_wmin, 3),
            arith_wmax = round(lppmr_at_wmax, 3),
            arith_range = round(lppmr_range, 3),
            slope = round(slope, 3),
            geom_wmax = round(lppmr_geom_at_wmax, 3),
            prey_g_wmin = signif(prey_g_at_wmin, 3),
            prey_g_wmax = signif(prey_g_at_wmax, 3),
            fold = round(prey_fold, 1),
            nominal = round(nominal_log10_beta, 3)) %>%
  arrange(species, variant)), row.names = FALSE)

if (!is.null(BAND)) {
  cat("\n=== model at w_max against the observed band ===\n")
  cat("The compilation defines PPMR as max body size / mean prey size, where\n")
  cat("'mean prey size' is the mean mass of prey ITEMS -- an abundance-weighted\n")
  cat("arithmetic mean over identifiable prey. So the like-for-like model value\n")
  cat("is the arithmetic mean at w_max, and the prey set matters: the resource\n")
  cat("spectrum is not something a stomach-contents study can count.\n")
  cat("Margin is in log10 units; negative means outside the band.\n\n")
  chk <- summ %>%
    inner_join(BAND %>% filter(!rorquals_only) %>%
                 select(species = predator_group, value_low, value_mid,
                        value_high), by = "species") %>%
    transmute(species = PRED_DISPLAY[species], variant,
              arith_wmax = round(lppmr_at_wmax, 3),
              geom_wmax = round(lppmr_geom_at_wmax, 3),
              obs_lo = round(value_low, 3), obs_mid = round(value_mid, 3),
              obs_hi = round(value_high, 3),
              margin_arith = round(pmin(lppmr_at_wmax - value_low,
                                        value_high - lppmr_at_wmax), 3),
              margin_geom = round(pmin(lppmr_geom_at_wmax - value_low,
                                       value_high - lppmr_geom_at_wmax), 3)) %>%
    arrange(species, variant)
  print(as.data.frame(chk), row.names = FALSE)

  ror <- BAND[BAND$rorquals_only, ]
  bw  <- summ %>% filter(species == "baleen whales")
  cat(sprintf("\n  BALEEN vs RORQUALS ONLY (%.3f - %.3f, mid %.3f):\n",
              ror$value_low, ror$value_high, ror$value_mid))
  for (v in bw$variant)
    cat(sprintf("    %-14s arithmetic %.3f (margin %+.3f) | geometric %.3f (margin %+.3f)\n",
                v, bw$lppmr_at_wmax[bw$variant == v],
                min(bw$lppmr_at_wmax[bw$variant == v] - ror$value_low,
                    ror$value_high - bw$lppmr_at_wmax[bw$variant == v]),
                bw$lppmr_geom_at_wmax[bw$variant == v],
                min(bw$lppmr_geom_at_wmax[bw$variant == v] - ror$value_low,
                    ror$value_high - bw$lppmr_geom_at_wmax[bw$variant == v])))

  cat("\n  Mean mass of the ANTARCTIC KRILL each whale group takes, at w_max,\n")
  cat("  against ~1-2 g for an adult E. superba (the empirical prey mass):\n")
  kro <- ppmr %>% filter(variant == "krill_only", species %in% WH) %>%
    group_by(species) %>%
    summarise(krill_g_at_wmin = mean_arith[which.min(size_g)],
              krill_g_at_wmax = mean_arith[which.max(size_g)],
              fold = krill_g_at_wmax / krill_g_at_wmin, .groups = "drop")
  print(as.data.frame(kro %>%
    transmute(species = PRED_DISPLAY[species],
              krill_g_at_wmin = signif(krill_g_at_wmin, 3),
              krill_g_at_wmax = signif(krill_g_at_wmax, 3),
              fold = round(fold, 2))), row.names = FALSE)
}

# =============================================================================
# figures
# =============================================================================
VAR_COLS <- c(incl_resource = "#1a6faf", species_only = "#e8534a",
              krill_only = "#e8a33d")
VAR_LABS <- c(incl_resource = "All prey (incl. plankton resource)",
              species_only  = "Modelled prey species only",
              krill_only    = "Antarctic krill only")

cat("\n[1] mean prey size vs predator size, all groups\n")
# The canonical PPMR figure, following the layout in PPMR_Plot_Example/: prey
# mass against predator mass with diagonal constant-PPMR references.
ref <- expand.grid(c = 0:9, x = range(log10(ppmr$size_g)))
ref$y <- ref$x - ref$c
lab <- data.frame(c = 0:9, x = max(log10(ppmr$size_g)))
lab$y <- lab$x - lab$c
p0 <- ggplot(ppmr %>% filter(variant == "incl_resource"),
             aes(log10(size_g), log10(mean_arith))) +
  geom_line(data = ref, aes(x, y, group = c), inherit.aes = FALSE,
            colour = "grey80", linewidth = 0.3) +
  geom_text(data = lab, aes(x, y, label = sprintf("10^%d", c)),
            inherit.aes = FALSE, hjust = -0.05, size = 2.3,
            colour = "grey55") +
  geom_line(aes(colour = species_label), linewidth = 1) +
  scale_colour_viridis_d(name = NULL, option = "turbo", end = 0.95) +
  coord_cartesian(xlim = c(min(log10(ppmr$size_g)),
                           max(log10(ppmr$size_g)) + 0.9)) +
  theme_rd(10) +
  labs(x = "Predator body mass (log10 g)",
       y = "Mean prey mass (log10 g)",
       subtitle = paste("Grey diagonals are constant PPMR. A group running",
                        "parallel to them has no ontogenetic PPMR shift."))
sv(p0, "ppmr_prey_size_vs_predator_size", 10, 7)

nom_df <- data.frame(species_label = factor(PRED_DISPLAY[SPECIES],
                                            levels = PRED_DISPLAY),
                     nominal = as.numeric(NOMINAL))

# Each PPMR figure is emitted twice: once with the Antarctic-krill-only line and
# once without. The krill-only line is the sharpest test against the observed
# compilation for the krill feeders, but it is a different question from "what
# size is the average thing this predator eats", so the two should be readable
# separately.
VARIANT_SETS <- list(
  list(vars = names(VAR_COLS), suffix = "",
       note = paste("Orange: Antarctic krill alone, shown only for the",
                    "groups taking >=1% krill.")),
  list(vars = c("incl_resource", "species_only"), suffix = "_no_krill_only",
       note = ""))

cat("\n[2] realised PPMR, all species\n")
# Observed +-1 SD envelope as a ribbon around the nominal line. Built per
# species over that panel's own x range, because the facets use free scales.
# Only the 9 predator groups in csvs/predator_parameters_updated.csv have an
# empirical PPMR; the LTL, fish, squid and toothfish panels get no ribbon.
xr <- ppmr %>% group_by(species_label) %>%
  summarise(xmin = min(log10(size_g)), xmax = max(log10(size_g)),
            .groups = "drop")
ribbon_for <- function(rows) {
  if (is.null(rows) || !nrow(rows)) return(NULL)
  r <- rows %>%
    transmute(species_label = factor(PRED_DISPLAY[predator_group],
                                     levels = PRED_DISPLAY),
              lo = value_low, hi = value_high, mid = value_mid) %>%
    inner_join(xr, by = "species_label")
  if (!nrow(r)) return(NULL)
  rbind(transform(r, x = xmin), transform(r, x = xmax))
}
rib_all <- rib_ror <- NULL
if (!is.null(BAND)) {
  # Baleen whales get the rorquals-only band here and nothing else: their
  # all-species band reaches down to 3.97 because of the copepod-feeding
  # southern right whale, which would stretch the panel's free y scale over
  # four orders of magnitude and hide the curve. ppmr_whales.png shows both.
  rib_all <- ribbon_for(BAND[!BAND$rorquals_only &
                               BAND$predator_group != "baleen whales", ])
  rib_ror <- ribbon_for(BAND[BAND$rorquals_only, ])
}

for (vs in VARIANT_SETS) {
  dd <- ppmr %>% filter(variant %in% vs$vars)
  p1 <- ggplot(dd, aes(log10(size_g), log10_ppmr_arith, colour = variant))
  if (!is.null(rib_all))
    p1 <- p1 +
      geom_ribbon(data = rib_all, inherit.aes = FALSE,
                  aes(x = x, ymin = lo, ymax = hi), fill = "grey70",
                  alpha = 0.35) +
      geom_line(data = rib_all, inherit.aes = FALSE, aes(x = x, y = mid),
                colour = "grey30", linetype = "dotted", linewidth = 0.4)
  if (!is.null(rib_ror))
    p1 <- p1 +
      geom_ribbon(data = rib_ror, inherit.aes = FALSE,
                  aes(x = x, ymin = lo, ymax = hi), fill = "#7fbf7b",
                  alpha = 0.35) +
      geom_line(data = rib_ror, inherit.aes = FALSE, aes(x = x, y = mid),
                colour = "#1b7837", linetype = "dotted", linewidth = 0.4)
  p1 <- p1 +
    geom_hline(data = nom_df, aes(yintercept = nominal),
               colour = "grey20", linetype = "dashed", linewidth = 0.4) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~species_label, ncol = 4, scales = "free") +
    scale_colour_manual(values = VAR_COLS, labels = VAR_LABS, name = NULL) +
    theme_rd() + theme(legend.position = "bottom") +
    labs(x = "Predator body size (log10 g)",
         y = expression(Realised~log[10]~PPMR~(arithmetic~mean~prey~mass)),
         subtitle = trimws(paste(
           "Dashed: nominal log10(beta). Grey ribbon: observed +-1 SD across",
           "the group's constituent species,\ndotted at its mean (only the 9",
           "groups with an empirical PPMR). Green: rorquals only, for baleen",
           "whales.", vs$note)))
  sv(p1, paste0("ppmr_all_species", vs$suffix), 13, 14)
}

cat("\n[3] the whales against the observed benchmark\n")
if (!is.null(BAND)) {
  bd <- BAND %>% filter(predator_group %in% WH, !rorquals_only) %>%
    transmute(species_label = factor(PRED_DISPLAY[predator_group],
                                     levels = PRED_DISPLAY),
              lo = value_low, hi = value_high, mid = value_mid)
  rq <- BAND %>% filter(rorquals_only) %>%
    transmute(species_label = factor(PRED_DISPLAY["baleen whales"],
                                     levels = PRED_DISPLAY),
              lo = value_low, hi = value_high, mid = value_mid)
}
for (vs in VARIANT_SETS) {
  wh_df <- ppmr %>% filter(species %in% WH, variant %in% vs$vars)
  p2 <- ggplot(wh_df, aes(log10(size_g), log10_ppmr_arith, colour = variant))
  if (!is.null(BAND)) {
    p2 <- p2 +
      geom_rect(data = bd, inherit.aes = FALSE,
                aes(xmin = -Inf, xmax = Inf, ymin = lo, ymax = hi),
                fill = "grey80", alpha = 0.4) +
      geom_rect(data = rq, inherit.aes = FALSE,
                aes(xmin = -Inf, xmax = Inf, ymin = lo, ymax = hi),
                fill = "#7fbf7b", alpha = 0.30) +
      geom_hline(data = bd, inherit.aes = FALSE, aes(yintercept = mid),
                 colour = "grey30", linetype = "dotted", linewidth = 0.5) +
      geom_hline(data = rq, inherit.aes = FALSE, aes(yintercept = mid),
                 colour = "#1b7837", linetype = "dotted", linewidth = 0.5)
  }
  p2 <- p2 +
    geom_hline(data = nom_df[nom_df$species_label %in% wh_df$species_label, ],
               aes(yintercept = nominal), inherit.aes = FALSE,
               colour = "grey20", linetype = "dashed", linewidth = 0.5) +
    geom_line(linewidth = 1) + geom_point(size = 1.5) +
    facet_wrap(~species_label, nrow = 1, scales = "free_x") +
    scale_colour_manual(values = VAR_COLS, labels = VAR_LABS, name = NULL) +
    theme_rd(10) + theme(legend.position = "bottom") +
    labs(x = "Predator body size (log10 g)",
         y = expression(Realised~log[10]~PPMR),
         subtitle = paste0(
           "Grey band: observed +-1 SD across the group's constituent species. ",
           "Green (baleen only): rorquals\nalone (blue, fin, sei, humpback), ",
           "excluding copepod-feeding southern right whales. ",
           "Dashed: nominal log10(beta)."))
  sv(p2, paste0("ppmr_whales", vs$suffix), 12, 6)
}

cat("\n[4] prey size against predator size, the mechanism\n")
KR <- match("antarctic krill", SPECIES)
kr_ref <- data.frame(
  y = log10(c(PARAMS@species_params$w_min[KR], PARAMS@species_params$w_mat[KR],
              PARAMS@species_params$w_max[KR])),
  lab = c("krill w_min", "krill w_mat", "krill w_max"))
for (vs in VARIANT_SETS) {
  bm <- ppmr %>% filter(species %in% c("baleen whales", "minke whales"),
                        variant %in% vs$vars)
  bm$pref_w <- bm$size_g / PARAMS@species_params$beta[match(bm$species, SPECIES)]
  p3 <- ggplot(bm, aes(log10(size_g))) +
    geom_hline(data = kr_ref, aes(yintercept = y), inherit.aes = FALSE,
               colour = "grey55", linetype = "dotted", linewidth = 0.5) +
    geom_text(data = kr_ref, inherit.aes = FALSE,
              aes(x = -Inf, y = y, label = lab), hjust = -0.05, vjust = -0.5,
              size = 2.5, colour = "grey35") +
    geom_ribbon(data = bm %>% filter(variant == "incl_resource"),
                aes(ymin = log10(prey_q05), ymax = log10(prey_q95)),
                fill = "#1a6faf", alpha = 0.13) +
    geom_line(aes(y = log10(pref_w),
                  linetype = "Kernel preferred prey (w/beta)"),
              colour = "grey25", linewidth = 0.7) +
    geom_line(aes(y = log10(mean_arith), colour = variant,
                  linetype = "Realised mean prey"), linewidth = 0.9) +
    facet_wrap(~species_label, nrow = 1, scales = "free_x") +
    scale_colour_manual(values = VAR_COLS, labels = VAR_LABS, name = NULL) +
    scale_linetype_manual(
      values = c("Kernel preferred prey (w/beta)" = "dashed",
                 "Realised mean prey" = "solid"), name = NULL) +
    theme_rd(10) + theme(legend.position = "bottom", legend.box = "vertical") +
    labs(x = "Predator body size (log10 g)", y = "Prey mass (log10 g)",
         subtitle = paste("Shaded: 5th-95th percentile of the consumed",
                          "prey-size distribution (all prey).\nThe",
                          "preferred-prey line reaches krill w_max at the",
                          "largest whales; realised prey size cannot follow",
                          "it."))
  sv(p3, paste0("prey_size_vs_predator_size_whales", vs$suffix), 10, 6)
}

# =============================================================================
# tables
# =============================================================================
cat("\n[5] tables\n")
wcsv(ppmr %>% select(species, variant, size_g, mean_arith, mean_geom,
                     prey_q05, prey_q50, prey_q95,
                     log10_ppmr_arith, log10_ppmr_geom) %>%
       mutate(across(where(is.numeric), ~signif(.x, 6))),
     "RD03_ppmr_by_size")
wcsv(summ %>% mutate(across(where(is.numeric), ~signif(.x, 6))),
     "RD03_ppmr_summary")

# =============================================================================
# NOT RUN -- the kernel sweep, for when the parameterisation is revisited
# =============================================================================
#' Realised PPMR and krill diet share over a beta x sigma grid.
#'
#' Recomputes the kernel only; the prey field, interaction matrix and abundances
#' are held at their current values, so this needs no projection and no
#' recalibration and runs in seconds. That also means it answers a narrow
#' question -- "given today's prey field, what would this kernel see?" -- not
#' "what would the model settle to". Anything chosen here still has to go
#' through the steady()/matchBiomasses ladder in
#' R/wmin_test/51_whale_kernel_recalibrate.R.
sweep_kernel <- function(species, betas, sigmas, params = PARAMS) {
  i <- match(species, params@species_params$species)
  stopifnot(!is.na(i))
  pf <- prey_field(params)
  avail <- as.numeric(interaction_full(params)[i, ] %*% pf)
  ki <- match("antarctic krill", params@species_params$species)
  kb <- pf[ki, ] * params@interaction[i, ki]
  occ <- which(params@initial_n[i, ] > 0)
  g <- expand.grid(beta = betas, sigma = sigmas)
  do.call(rbind, lapply(seq_len(nrow(g)), function(z) {
    lp <- ks <- rep(NA_real_, length(occ))
    for (m in seq_along(occ)) {
      k <- occ[m]
      phi <- lognormal_pred_kernel(params@w[k] / params@w_full,
                                   beta = g$beta[z], sigma = g$sigma[z])
      cc <- phi * avail
      lp[m] <- log10(params@w[k]) - log10(sum(cc) / sum(cc / params@w_full))
      ks[m] <- sum(phi * kb) / sum(cc)
    }
    data.frame(species = species, beta = g$beta[z], sigma = g$sigma[z],
               lppmr_at_wmax = lp[length(lp)],
               lppmr_range = diff(range(lp)),
               slope = unname(coef(lm(lp ~ log10(params@w[occ])))[2]),
               krill_share = mean(ks))
  }))
}
# Example, when you come back to this:
#   sweep_kernel("baleen whales", betas = 10^seq(6.5, 8.5, by = 0.25),
#                sigmas = c(1.5, 2.0, 2.5, 3.0))

cat("\nRD03 complete. (sweep_kernel() defined but not run.)\n")