# =============================================================================
# STAGE 0 -- analytic bound on the `small divers` w_min artefact. No simulation.
#
# Question: how much of what penguins DO in the model is done by individuals
# below the proposed corrected w_min of 3626.667 g? Three quantities, per
# member, both scenarios, over the 2001-2010 reference period:
#
#   (a) share of penguin BIOMASS and ABUNDANCE below the threshold
#   (b) PREDATION EXERTED by sub-threshold penguins -- their share of penguin
#       consumption of Antarctic krill and of the fish groups, and penguins'
#       share of total community consumption of those prey
#   (c) their share of the PREDATION MORTALITY falling on penguins
#
# Rates are computed with R/wmin_test/thermizer_shim.R, whose diet routine is a
# verified bit-identical transcription of mizer::getDiet() (03_transcription_
# check.R) plus a temperature scaling whose functional form is pinned exactly by
# the stored encounterpred_scale (_verify_tempscale.R).
#
# Bin proration: the mizer grid is 100 log bins over ~15.5 decades, so a bin
# spans a factor of ~1.43 and the threshold falls strictly inside one. Shares
# are reported both prorated (linear within the straddling bin) and with the
# whole-bin lower-edge rule, to show the convention does not drive the answer.
#
# Writes Output_large_files/wmin_test/05_stage0_per_member.rds  (+ csv summary)
# =============================================================================

source("R/wmin_test/thermizer_shim.R")
suppressPackageStartupMessages(library(dplyr))

out_dir <- "Output_large_files/wmin_test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

W_THR      <- 3626.667                 # proposed corrected w_min, g
REF_YEARS  <- 2001:2010
PENGUIN    <- "small divers"
KRILL      <- "antarctic krill"
FISH_PREY  <- c("mesopelagic fishes", "bathypelagic fishes",
                "shelf and coastal fishes", "toothfishes")

t0 <- proc.time()
cat("=== Stage 0 started:", format(Sys.time()), "===\n")
cat("threshold =", W_THR, "g ; reference period", min(REF_YEARS), "-",
    max(REF_YEARS), "\n\n")

ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
idx <- ens$metadata$top10pct_indices
arms <- list(Exploited = ens$fished_top10pct, Unexploited = ens$climate_top10pct)

p0 <- ens$fished_top10pct[[1]]@params
w  <- p0@w
dw <- p0@dw

# fraction of each bin lying below the threshold
frac_below <- pmin(1, pmax(0, (W_THR - w) / dw))
edge_below <- as.numeric(w < W_THR)     # whole-bin lower-edge rule
cat("bins fully below threshold:", sum(frac_below == 1),
    "| straddling bin index:", which(frac_below > 0 & frac_below < 1),
    "| w of that bin:", signif(w[which(frac_below > 0 & frac_below < 1)], 6), "\n")
frac_below_1g <- pmin(1, pmax(0, (1 - w) / dw))   # brief's stated 1 g cross-check
cat("penguin w_min in these params:",
    p0@species_params$w_min[p0@species_params$species == PENGUIN], "g\n\n")

one_member <- function(sim, year) {
  p  <- sim@params
  st <- sim_state_at(sim, year)
  n  <- st$n
  te <- ther_temp_effect(p, year)
  fl <- ther_feeding_level(p, st$n, st$n_pp, st$n_other, year, temp_eff = te)
  diet <- ther_diet(p, st$n, st$n_pp, st$n_other, year, temp_eff = te,
                    feeding_level = fl)
  pm <- ther_pred_mort(p, st$n, st$n_pp, st$n_other, year, temp_eff = te,
                       feeding_level = fl)

  npen <- n[PENGUIN, ]

  # --- (a) size structure ---
  num_bin  <- npen * dw
  biom_bin <- npen * w * dw

  # --- (b) predation exerted ---
  # consumption of prey j by penguins in bin k, g/yr
  cons_krill_bin <- diet[PENGUIN, , KRILL] * npen * dw
  cons_fish_bin  <- rowSums(diet[PENGUIN, , FISH_PREY, drop = FALSE]) * npen * dw
  # community-wide consumption of the same prey, all predators, g/yr
  cons_all <- function(prey) sum(diet[, , prey] * n * rep(dw, each = nrow(n)))
  krill_all <- cons_all(KRILL)
  fish_all  <- sum(vapply(FISH_PREY, cons_all, numeric(1)))
  krill_baleen <- sum(diet["baleen whales", , KRILL] * n["baleen whales", ] * dw)

  # --- (c) predation mortality falling on penguins ---
  mort_num_bin  <- pm[PENGUIN, ] * npen * dw
  mort_biom_bin <- pm[PENGUIN, ] * npen * w * dw

  wsum <- function(x, wt) sum(x * wt)
  data.frame(
    year = year,
    pen_biomass          = sum(biom_bin),
    pen_number           = sum(num_bin),
    pen_biomass_sub      = wsum(biom_bin, frac_below),
    pen_number_sub       = wsum(num_bin,  frac_below),
    pen_biomass_sub_edge = wsum(biom_bin, edge_below),
    pen_number_sub_edge  = wsum(num_bin,  edge_below),
    pen_biomass_sub_1g   = wsum(biom_bin, frac_below_1g),
    pen_number_sub_1g    = wsum(num_bin,  frac_below_1g),

    pen_cons_krill       = sum(cons_krill_bin),
    pen_cons_krill_sub   = wsum(cons_krill_bin, frac_below),
    pen_cons_krill_sub_edge = wsum(cons_krill_bin, edge_below),
    pen_cons_fish        = sum(cons_fish_bin),
    pen_cons_fish_sub    = wsum(cons_fish_bin, frac_below),
    community_cons_krill = krill_all,
    community_cons_fish  = fish_all,
    baleen_cons_krill    = krill_baleen,

    pen_mort_number      = sum(mort_num_bin),
    pen_mort_number_sub  = wsum(mort_num_bin, frac_below),
    pen_mort_biomass     = sum(mort_biom_bin),
    pen_mort_biomass_sub = wsum(mort_biom_bin, frac_below)
  )
}

rows <- list()
for (arm in names(arms)) {
  sims <- arms[[arm]]
  for (m in seq_along(sims)) {
    if (m %% 25 == 0 || m == 1)
      cat(sprintf("  %-11s member %3d/%d   elapsed %.1f min\n", arm, m,
                  length(sims), (proc.time() - t0)["elapsed"] / 60))
    for (y in REF_YEARS) {
      r <- one_member(sims[[m]], y)
      r$arm <- arm; r$member <- m; r$sim_index <- idx[m]
      rows[[length(rows) + 1]] <- r
    }
  }
}
per_year <- bind_rows(rows)
saveRDS(per_year, file.path(out_dir, "05_stage0_per_member_year.rds"))

# --- collapse to the reference-period mean per member ------------------------
per_member <- per_year %>%
  group_by(arm, member, sim_index) %>%
  summarise(across(-year, mean), .groups = "drop") %>%
  mutate(
    f_biomass_sub        = pen_biomass_sub / pen_biomass,
    f_number_sub         = pen_number_sub  / pen_number,
    f_biomass_sub_edge   = pen_biomass_sub_edge / pen_biomass,
    f_number_sub_edge    = pen_number_sub_edge  / pen_number,
    f_biomass_sub_1g     = pen_biomass_sub_1g / pen_biomass,
    f_number_sub_1g      = pen_number_sub_1g  / pen_number,
    f_cons_krill_sub     = pen_cons_krill_sub / pen_cons_krill,
    f_cons_krill_sub_edge= pen_cons_krill_sub_edge / pen_cons_krill,
    f_cons_fish_sub      = pen_cons_fish_sub  / pen_cons_fish,
    pen_share_krill      = pen_cons_krill / community_cons_krill,
    pen_share_fish       = pen_cons_fish  / community_cons_fish,
    subpen_share_krill   = pen_cons_krill_sub / community_cons_krill,
    subpen_share_fish    = pen_cons_fish_sub  / community_cons_fish,
    pen_vs_baleen_krill  = pen_cons_krill / baleen_cons_krill,
    f_mort_number_sub    = pen_mort_number_sub / pen_mort_number,
    f_mort_biomass_sub   = pen_mort_biomass_sub / pen_mort_biomass
  )
saveRDS(per_member, file.path(out_dir, "05_stage0_per_member.rds"))
write.csv(per_member, file.path(out_dir, "05_stage0_per_member.csv"),
          row.names = FALSE)

# --- report ------------------------------------------------------------------
q <- function(x) sprintf("%.4g [%.4g, %.4g]", median(x), quantile(x, .25),
                         quantile(x, .75))
cat("\n\n================ STAGE 0 RESULT  (median [IQR] over 212 members) ",
    "================\n")
metrics <- c(
  "f_biomass_sub"      = "penguin BIOMASS below 3626.667 g",
  "f_number_sub"       = "penguin ABUNDANCE below 3626.667 g",
  "f_biomass_sub_edge" = "  same, whole-bin lower-edge rule (biomass)",
  "f_number_sub_edge"  = "  same, whole-bin lower-edge rule (abundance)",
  "f_biomass_sub_1g"   = "penguin BIOMASS below 1 g (brief cross-check)",
  "f_number_sub_1g"    = "penguin ABUNDANCE below 1 g (brief cross-check)",
  "f_cons_krill_sub"   = "share of PENGUIN krill consumption by sub-threshold",
  "f_cons_fish_sub"    = "share of PENGUIN fish consumption by sub-threshold",
  "pen_share_krill"    = "penguins' share of COMMUNITY krill consumption",
  "subpen_share_krill" = "sub-threshold penguins' share of COMMUNITY krill",
  "pen_share_fish"     = "penguins' share of COMMUNITY fish consumption",
  "subpen_share_fish"  = "sub-threshold penguins' share of COMMUNITY fish",
  "pen_vs_baleen_krill"= "penguin : baleen whale krill consumption ratio",
  "f_mort_number_sub"  = "share of penguin NUMBERS killed that are sub-thr.",
  "f_mort_biomass_sub" = "share of penguin BIOMASS killed that is sub-thr."
)
for (a in unique(per_member$arm)) {
  cat("\n--- ", a, " ---\n", sep = "")
  d <- per_member[per_member$arm == a, ]
  for (k in names(metrics))
    cat(sprintf("  %-52s %s\n", metrics[[k]], q(d[[k]])))
}

cat("\nelapsed:", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
cat("Saved:", file.path(out_dir, "05_stage0_per_member.rds"), "\n")
