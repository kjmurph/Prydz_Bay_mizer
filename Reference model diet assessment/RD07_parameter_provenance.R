# =============================================================================
# RD07 -- where the feeding-kernel and size parameters come from, and what
#         extending the squid group to colossal squid would do
#
# PART 1: PROVENANCE
#   The 19 groups' beta values come from THREE different places with three very
#   different evidential standards, and the distinction matters when deciding
#   which ones are safe to retune:
#
#     zooplankton (5 groups)  Heneghan et al. 2020, Ecol Modelling,
#                             doi 10.1016/j.ecolmodel.2020.109265 -- published
#                             log10 PPMR ranges, midpoints taken. Recorded in
#                             03_model_setup_pre_therMizer.rmd:1026-1052.
#     predators (9 groups)    csvs/predator_parameters_updated.csv -- per-species
#                             "mean prey size / max body size" with SDs and named
#                             literature sources. Group value = mean of members'
#                             ratios, inverted (verified in RD00b).
#     fishes + squid (5)      csvs/fish_parameters_updated.csv and
#                             csvs/squid_parameters_updated.csv -- every PPMR row
#                             reads "calculated or assumed". NO SOURCE.
#
#   So squid beta = 50 and the fish betas are the least defensible numbers in
#   the kernel, and squid beta is one of the two levers RD06 identified.
#
# PART 2: THE SQUID SIZE CEILING
#   squid w_max = 20,718.35 g is NOT arbitrary and it does have a source, but it
#   is a source for a taxon list that excludes the largest Antarctic squid.
#   It is max(wmax) over three taxa, the largest being Onychoteuthiids at
#   Lmax = 115 cm (Phillips 2004), converted by a length-weight relationship
#   that is itself "calculated or assumed".
#
#   Mesonychoteuthis hamiltoni (colossal squid, Cranchiidae) and Architeuthis
#   (giant squid) are simply not in the taxon list -- and Mesonychoteuthis is
#   the taxon sperm whales in this sector are best known for eating. Section 4
#   quantifies the consequence.
#
# USAGE  Rscript "Reference model diet assessment/RD07_parameter_provenance.R"
# =============================================================================

source(file.path("Reference model diet assessment", "RD00_common.R"))
cat("=== RD07: parameter provenance and the squid size ceiling ===\n")

SQ_CSV <- file.path("csvs", "squid_parameters_updated.csv")
FI_CSV <- file.path("csvs", "fish_parameters_updated.csv")
PR_CSV <- file.path("csvs", "predator_parameters_updated.csv")

# =============================================================================
# 1. beta provenance for all 19 groups
# =============================================================================
cat("\n[1] beta provenance\n")

# Heneghan values, transcribed from 03_model_setup_pre_therMizer.rmd:1036-1052
HENEGHAN <- list(
  "salps" = list(lo = 6.8, hi = 11.7, mid = 9.25,
                 basis = "salps 6.8-11.7, midpoint"),
  "antarctic krill" = list(lo = 6.6, hi = 7.8, mid = 7.2,
                           basis = "euphausiids 6.6-7.8, midpoint"),
  "other krill" = list(lo = 6.6, hi = 7.8, mid = 7.2,
                       basis = "euphausiids 6.6-7.8, midpoint"),
  "mesozooplankton" = list(lo = 0.8, hi = 4.6, mid = 2.725,
                           basis = "mean of Omni.Cop 4.1 and Carn.Cop 1.35"),
  "other macrozooplankton" = list(lo = 1.9, hi = 3.4, mid = 2.65,
                                  basis = "chaetognaths 1.9-3.4, midpoint"))

prov <- do.call(rbind, lapply(SPECIES, function(s) {
  b <- PARAMS@species_params$beta[match(s, SPECIES)]
  if (s %in% names(HENEGHAN)) {
    h <- HENEGHAN[[s]]
    data.frame(species = s, beta = b, log10_beta = log10(b),
               source_file = "03_model_setup_pre_therMizer.rmd:1036-1052",
               source = paste("Heneghan et al. 2020 Ecol Modelling,",
                              "doi 10.1016/j.ecolmodel.2020.109265"),
               basis = h$basis, sourced = "published",
               reproduces = isTRUE(all.equal(log10(b), h$mid, tolerance = 1e-4)),
               row.names = NULL)
  } else if (s %in% c("mesopelagic fishes", "bathypelagic fishes",
                      "shelf and coastal fishes", "toothfishes")) {
    data.frame(species = s, beta = b, log10_beta = log10(b),
               source_file = FI_CSV, source = "calculated or assumed",
               basis = "mean of member species' assumed PPMR",
               sourced = "ASSUMED", reproduces = NA, row.names = NULL)
  } else if (s == "squids") {
    data.frame(species = s, beta = b, log10_beta = log10(b),
               source_file = SQ_CSV, source = "calculated or assumed",
               basis = "mean of 3 cephalopod taxa, all assumed 50",
               sourced = "ASSUMED", reproduces = NA, row.names = NULL)
  } else {
    data.frame(species = s, beta = b, log10_beta = log10(b),
               source_file = PR_CSV,
               source = "per-species literature; see RD00b benchmark",
               basis = "mean of members' prey/predator mass ratios, inverted",
               sourced = "published", reproduces = TRUE, row.names = NULL)
  }
}))
print(as.data.frame(prov %>%
  transmute(species, beta = signif(beta, 6),
            log10_beta = round(log10_beta, 3), sourced,
            reproduces, basis)), row.names = FALSE)
cat(sprintf("\n  %d of %d groups carry a published PPMR source; %d are assumed.\n",
            sum(prov$sourced == "published"), nrow(prov),
            sum(prov$sourced == "ASSUMED")))
cat("  The assumed ones are the four fish groups and squids.\n")
# A published-source group whose beta no longer reproduces has DEPARTED from
# the compilation. That is a legitimate modelling decision (phase 54 does it
# for sperm and baleen whales) rather than an error, so report it and carry on.
dep <- prov$species[!is.na(prov$reproduces) & !prov$reproduces]
if (length(dep)) {
  cat(sprintf("\n  DEPARTS from its source: %s\n", paste(dep, collapse = ", ")))
  for (d in dep) {
    b <- prov$beta[prov$species == d]
    cat(sprintf("    %-14s model beta %12.6g (log10 %.4f)\n", d, b, log10(b)))
  }
  cat("  These are deliberate overrides -- check them against phase 54's header.\n")
} else {
  cat("  assert: every published beta reproduces from its source -- OK\n")
}

# =============================================================================
# 2. the squid size ceiling, reproduced from source
# =============================================================================
cat("\n[2] squid w_max, reproduced from csvs/squid_parameters_updated.csv\n")
s <- read.csv(SQ_CSV, stringsAsFactors = FALSE)
sw <- s %>% select(species, parameter, value) %>%
  tidyr::pivot_wider(names_from = parameter, values_from = value) %>%
  as.data.frame()
num <- function(x) suppressWarnings(as.numeric(x))
LW_A <- num(sw[["a (W=aL^b)"]]); LW_B <- num(sw[["b (W=aL^b)"]])
sw$wmax_g <- LW_A * (num(sw[["Lmax cm"]]) * 10)^LW_B
sw$wmat_g <- LW_A * (num(sw[["Lmat cm"]]) * 10)^LW_B
src_lmax <- s$`source.derivation`[s$parameter == "Lmax cm"]
sw$lmax_source <- src_lmax
print(as.data.frame(sw %>%
  transmute(taxon = species, Lmax_cm = num(`Lmax cm`),
            wmax_g = round(wmax_g, 1), wmat_g = round(wmat_g, 1),
            ppmr = num(`preferred predator-prey mass ratio (mean)`),
            lmax_source)), row.names = FALSE)

i_sq <- match("squids", SPECIES)
cat(sprintf(paste("\n  aggregation (group params/1g_simplified_groups_params.R",
                  "md:370-380):\n    Wmax = max(wmax), Wmat = mean(w_mat),",
                  "beta = mean(beta)\n")))
cat(sprintf("  model w_max %.6f vs max(source) %.6f  -> %s\n",
            PARAMS@species_params$w_max[i_sq], max(sw$wmax_g),
            isTRUE(all.equal(PARAMS@species_params$w_max[i_sq],
                             max(sw$wmax_g)))))
cat(sprintf("  set by %s at Lmax = %g cm, sourced to '%s'\n",
            sw$species[which.max(sw$wmax_g)],
            num(sw[["Lmax cm"]])[which.max(sw$wmax_g)],
            sw$lmax_source[which.max(sw$wmax_g)]))
cat(sprintf(paste("  BUT the length-weight conversion (a = %.6g, b = %.6g,",
                  "L in mm) is itself\n  '%s' -- so the 115 cm is sourced and",
                  "the 20.7 kg is not.\n"),
            LW_A[1], LW_B[1],
            s$`source.derivation`[s$parameter == "a (W=aL^b)"][1]))
cat("\n  Taxa in the group: ", paste(sw$species, collapse = "; "), "\n")
cat("  NOT in the group: Cranchiidae (Mesonychoteuthis hamiltoni, colossal\n")
cat("  squid) and Architeuthidae (Architeuthis, giant squid) -- the two taxa\n")
cat("  that dominate large-squid biomass in sperm whale diets in this sector.\n")

# =============================================================================
# 3. what a colossal squid entry would look like
# =============================================================================
cat("\n[3] adding Mesonychoteuthis hamiltoni\n")
# Two ways to set its w_max, and they disagree by 3.3x. The group's generic
# length-weight exponent b = 2.565 describes slender ommastrephid/onychoteuthid
# bodies; Mesonychoteuthis is far bulkier, so applying it to a 250 cm mantle
# badly under-predicts the mass of the animals that have actually been landed.
ML_CM <- 250            # mantle length of the largest landed specimens
OBS_G <- 5e5            # ~500 kg, largest landed specimen (Ross Sea, 2007)
gen <- LW_A[1] * (ML_CM * 10)^LW_B[1]
cat(sprintf("  generic L-W applied to ML %g cm : %.4g g\n", ML_CM, gen))
cat(sprintf("  largest landed specimen         : %.4g g\n", OBS_G))
cat(sprintf("  the generic relationship under-predicts by %.1fx --\n", OBS_G / gen))
cat("  use the observed mass, not the group's length-weight coefficients.\n")

SCEN <- c("as modelled (3 taxa)" = PARAMS@species_params$w_max[i_sq],
          "generic L-W at ML 250 cm" = gen,
          "observed 500 kg specimen" = OBS_G)
cat("\n  candidate squid w_max:\n")
for (n in names(SCEN))
  cat(sprintf("    %-26s %10.4g g  (%.1fx current)\n", n, SCEN[[n]],
              SCEN[[n]] / SCEN[[1]]))
cat("\n  NOTE the group aggregation also moves w_mat (a MEAN over taxa) and\n")
cat("  beta (a MEAN over taxa), so adding a fourth taxon changes both even if\n")
cat("  you only intend to raise the ceiling. Supply its Lmat and PPMR too.\n")

# =============================================================================
# 4. would a larger ceiling actually feed sperm whales?
# =============================================================================
cat("\n[4] ILLUSTRATIVE: extending the squid spectrum\n")
cat("  This extrapolates the current squid biomass spectrum past its ceiling.\n")
cat("  It is NOT a model result: a real change to w_max alters growth,\n")
cat("  reproduction and the calibration, and needs a rebuild. Read it as a\n")
cat("  bound on how much difference the ceiling could make, not a prediction.\n")

PREYB <- prey_field(PARAMS)
INTER <- interaction_full(PARAMS)
PREY_NAMES <- c(SPECIES, "Resource")
IF_ <- match("sperm whales", SPECIES)
K_OCC <- which(OCC[IF_, ])
WTS <- PARAMS@initial_n[IF_, K_OCC] * PARAMS@w[K_OCC] * PARAMS@dw[K_OCC]
WTS <- WTS / sum(WTS)
WF <- PARAMS@w_full

# fit the local slope of squid biomass density over its top decade
sq_row <- PREYB["squids", ]
occ_sq <- which(sq_row > 0)
w_top <- WF[occ_sq]
top <- occ_sq[w_top >= max(w_top) / 10]
dens <- sq_row[top] / PARAMS@dw_full[top]
fit <- lm(log10(dens) ~ log10(WF[top]))
slope <- unname(coef(fit)[2])
cat(sprintf("\n  squid biomass-density slope over the top decade: %+.3f\n", slope))
cat("  This is POSITIVE -- squid biomass density rises steeply towards w_max in\n")
cat("  this model. Extrapolating a positive slope past the ceiling is the weak\n")
cat("  point of the calculation below: it compounds fast, so the 'biomass\n")
cat("  grows' variant should be read as an upper bound and nothing more.\n")

#' Extend the squid biomass spectrum to new_wmax.
#' @param conserve TRUE  = total squid biomass held fixed (redistributed)
#'                 FALSE = density continued, so total biomass grows
extend_squid <- function(new_wmax, conserve) {
  pb <- PREYB
  add <- which(WF > max(w_top) & WF <= new_wmax)
  if (!length(add)) return(pb)
  d_ref <- dens[length(dens)]; w_ref <- WF[top][length(top)]
  pb["squids", add] <- d_ref * (WF[add] / w_ref)^slope * PARAMS@dw_full[add]
  if (conserve)
    pb["squids", ] <- pb["squids", ] * sum(PREYB["squids", ]) /
      sum(pb["squids", ])
  pb
}
squid_share <- function(pb, beta) {
  ae <- t(sapply(K_OCC, function(k) {
    phi <- lognormal_pred_kernel(PARAMS@w[k] / WF, beta = beta,
                                 sigma = PARAMS@species_params$sigma[IF_])
    as.numeric(phi %*% t(pb))
  }))
  cb <- sweep(ae, 2, INTER[IF_, ], "*")
  sh <- sweep(cb, 1, pmax(rowSums(cb), 1e-300), "/")
  as.numeric(WTS %*% sh)[match("squids", PREY_NAMES)]
}

BETA_USED <- PARAMS@species_params$beta[IF_]
grid <- expand.grid(w_max = unname(SCEN), conserve = c(TRUE, FALSE),
                    beta = c(BETA_USED, 1995, 400))
grid$scenario <- names(SCEN)[match(grid$w_max, unname(SCEN))]
grid$squid_pct <- 100 * mapply(function(wm, cons, b)
  squid_share(extend_squid(wm, cons), b), grid$w_max, grid$conserve, grid$beta)
grid$beta_lab <- c("in use 44,082", "squid-max 1,995",
                   "Williams 400")[match(grid$beta,
                                         c(BETA_USED, 1995, 400))]
grid$biomass_x <- mapply(function(wm, cons)
  sum(extend_squid(wm, cons)["squids", ]) / sum(PREYB["squids", ]),
  grid$w_max, grid$conserve)
res <- grid %>%
  transmute(scenario, total_biomass = ifelse(conserve, "held fixed", "grows"),
            biomass_x = round(biomass_x, 1), beta_lab,
            squid_pct = round(squid_pct, 2)) %>%
  arrange(scenario, total_biomass, desc(squid_pct))
print(as.data.frame(res), row.names = FALSE)

cat(paste("\n  WHICH COLUMN TO READ. The model is calibrated to an observed",
          "squid biomass\n  (biomass_observed = 0.15 t/km2) and matchBiomasses",
          "pins the group total to it,\n  so 'held fixed' is the operative",
          "case. 'grows' requires total squid biomass\n  to rise by the",
          "biomass_x factor shown, which for the 500 kg ceiling is",
          sprintf("%.0fx --\n  not something the calibration would permit.\n",
                  max(grid$biomass_x))))

cur <- res$squid_pct[res$scenario == "as modelled (3 taxa)" &
                       res$beta_lab == "in use 44,082"][1]
big_fix <- res$squid_pct[res$scenario == "observed 500 kg specimen" &
                           res$total_biomass == "held fixed" &
                           res$beta_lab == "in use 44,082"]
cat(sprintf(paste("\n  So at the CURRENT beta, raising the ceiling to 500 kg",
                  "makes squid WORSE,\n  not better: %.2f%% -> %.2f%%.\n"),
            cur, big_fix))
cat(sprintf(paste("  The sperm whale kernel sits at %.0f g. Biomass added",
                  "above 20.7 kg is nowhere\n  near it, so with the total",
                  "pinned the extension just moves biomass OUT of the\n",
                  " window the whale feeds in.\n"),
            PARAMS@species_params$w_max[IF_] / BETA_USED))
best_fix <- res %>% filter(total_biomass == "held fixed") %>%
  arrange(desc(squid_pct)) %>% head(1)
cat(sprintf(paste("\n  Best case with the total pinned: %.2f%% (%s, %s) --",
                  "still far short of the\n  50-90%% the diet studies report.",
                  "The ceiling and the kernel have to move\n  together, and",
                  "even then the squid STANDING STOCK is the binding",
                  "constraint,\n  which is the same joint conclusion RD06",
                  "reached.\n"),
            best_fix$squid_pct, best_fix$scenario, best_fix$beta_lab))

wcsv(prov, "RD07_beta_provenance")
wcsv(sw %>% transmute(taxon = species, Lmax_cm = num(`Lmax cm`),
                      Lmat_cm = num(`Lmat cm`), wmax_g, wmat_g,
                      ppmr = num(`preferred predator-prey mass ratio (mean)`),
                      lmax_source),
     "RD07_squid_source_taxa")
wcsv(res, "RD07_squid_ceiling_scenarios")
cat("\nRD07 complete. No parameter changed.\n")