# =============================================================================
# RD05 -- modelled diet against an empirical Southern Ocean benchmark
#
# THE BENCHMARK IS HAND-CURATED AND UNVERIFIED.
#   benchmarks/diet_benchmark_southern_ocean.csv was transcribed from the
#   literature by an LLM, not read off the primary sources. Every row carries
#   its claimed source and a confidence flag, and every note ends in
#   "VERIFY BEFORE QUOTING". Treat the ranges as a sanity screen, not as data.
#   Nothing in the manuscript should cite them until they have been checked.
#
#   The PPMR benchmark used by RD03 is a different matter: it is derived
#   mechanically from csvs/predator_parameters_updated.csv, which is already in
#   the repository with its own sources, and RD00b verifies the group mapping by
#   reproducing the model's own beta values.
#
#   If you later want the SCAR Southern Ocean Diet and Energetics Database via
#   the sohungry package, it replaces this CSV without touching this script --
#   the schema is metric / predator_group / prey_group / value_low / value_mid /
#   value_high / units / confidence / source / note.
#
# ONE STRUCTURAL POINT THE COMPARISON WILL RAISE
#   The model gives the whales a large share of the background PLANKTON
#   RESOURCE, which is not a prey category a stomach-contents study can report.
#   Modelled shares are therefore reported twice: raw, and renormalised over
#   identifiable prey only (resource excluded). The second is the like-for-like
#   number and it is the one the figure marks.
#
# USAGE  Rscript "Reference model diet assessment/RD05_benchmark_comparison.R"
#        (run RD01 first -- this reads its diet summary)
# =============================================================================

source(file.path("Reference model diet assessment", "RD00_common.R"))
cat("=== RD05: model against the empirical diet benchmark ===\n")

# The diet benchmark is a static literature file, not a per-params product, so
# it does NOT live under RD_OUT. Fall back to the canonical folder when RD_OUT
# has been pointed somewhere else (as it is when re-running on a rebuilt params).
BEN <- file.path(RD_ROOT, "benchmarks", "diet_benchmark_southern_ocean.csv")
if (!file.exists(BEN))
  BEN <- file.path("Reference model diet assessment", "benchmarks",
                   "diet_benchmark_southern_ocean.csv")
if (!file.exists(BEN)) stop("benchmark not found: ", BEN, call. = FALSE)
bm <- read.csv(BEN, stringsAsFactors = FALSE)
bm <- bm[bm$metric == "diet_pct", ]
cat("benchmark rows:", nrow(bm), "| predators:",
    length(unique(bm$predator_group)), "\n")
cat("ALL VALUES ARE UNVERIFIED TRANSCRIPTIONS -- see the script header.\n")

# --- modelled diet, grouped, biomass-weighted over the ontogeny ---------------
DIET <- getDiet(PARAMS, proportion = TRUE)
PREY <- dimnames(DIET)$prey
prey_to_group <- setNames(rep(NA_character_, length(PREY)), PREY)
for (g in names(PREY_GROUPS))
  prey_to_group[intersect(PREY_GROUPS[[g]], PREY)] <- g
keep <- PREY[!is.na(prey_to_group)]
groups <- names(PREY_GROUPS)

GS <- array(0, dim = c(NS, NW, length(groups)),
            dimnames = list(SPECIES, NULL, groups))
for (g in groups)
  GS[, , g] <- apply(DIET[, , keep[prey_to_group[keep] == g], drop = FALSE],
                     c(1, 2), sum)
GS <- sweep(GS, c(1, 2), pmax(apply(GS, c(1, 2), sum), 1e-300), "/")

model <- do.call(rbind, lapply(seq_len(NS), function(i) {
  k <- which(OCC[i, ])
  w <- PARAMS@initial_n[i, k] * PARAMS@w[k] * PARAMS@dw[k]; w <- w / sum(w)
  data.frame(predator_group = SPECIES[i], prey_group = groups,
             model_pct = 100 * as.numeric(w %*% GS[i, k, , drop = FALSE][1, , ]),
             row.names = NULL)
}))
# renormalise over identifiable prey -- the resource is not something a diet
# study can count, so this is the like-for-like number
ident <- model %>% filter(prey_group != "Plankton resource") %>%
  group_by(predator_group) %>%
  mutate(model_pct_ident = 100 * model_pct / sum(model_pct)) %>% ungroup()
model <- model %>% left_join(ident %>%
  select(predator_group, prey_group, model_pct_ident),
  by = c("predator_group", "prey_group"))

res_share <- model %>% filter(prey_group == "Plankton resource") %>%
  select(predator_group, resource_pct = model_pct)

cmp <- bm %>%
  select(predator_group, prey_group, value_low, value_mid, value_high,
         confidence, source) %>%
  inner_join(model, by = c("predator_group", "prey_group")) %>%
  left_join(res_share, by = "predator_group") %>%
  mutate(inside_raw   = model_pct       >= value_low & model_pct       <= value_high,
         inside_ident = model_pct_ident >= value_low & model_pct_ident <= value_high,
         resid_ident  = model_pct_ident - value_mid,
         label = PRED_DISPLAY[predator_group])

cat("\n=== modelled vs benchmark diet share (%) ===\n")
cat("model_pct        : raw modelled share, resource included as a prey\n")
cat("model_pct_ident  : renormalised over identifiable prey only\n")
cat("resource_pct     : how much of the raw diet is the background resource\n\n")
print(as.data.frame(cmp %>%
  transmute(predator = label, prey = prey_group,
            model_pct = round(model_pct, 1),
            model_ident = round(model_pct_ident, 1),
            resource_pct = round(resource_pct, 1),
            obs = sprintf("%g-%g (%g)", value_low, value_high, value_mid),
            inside_ident, confidence) %>%
  arrange(predator, desc(model_ident))), row.names = FALSE)

cat(sprintf("\ninside the benchmark range: raw %d/%d, identifiable-prey %d/%d\n",
            sum(cmp$inside_raw), nrow(cmp),
            sum(cmp$inside_ident), nrow(cmp)))

cat("\n=== the biggest departures (identifiable-prey basis) ===\n")
print(as.data.frame(cmp %>% filter(!inside_ident) %>%
  transmute(predator = label, prey = prey_group,
            model_ident = round(model_pct_ident, 1),
            obs_lo = value_low, obs_hi = value_high,
            shortfall = round(ifelse(model_pct_ident < value_low,
                                     model_pct_ident - value_low,
                                     model_pct_ident - value_high), 1),
            confidence) %>%
  arrange(shortfall)), row.names = FALSE)

# --- figure -------------------------------------------------------------------
cat("\n[1] model vs benchmark\n")
pd <- cmp %>%
  mutate(label = factor(label, levels = PRED_DISPLAY),
         prey_group = factor(prey_group, levels = PREY_GROUP_ORDER))
p <- ggplot(pd, aes(y = prey_group)) +
  geom_segment(aes(x = value_low, xend = value_high,
                   yend = prey_group, colour = confidence),
               linewidth = 3, alpha = 0.35) +
  geom_point(aes(x = value_mid, colour = confidence), shape = 124, size = 4) +
  geom_point(aes(x = model_pct), shape = 21, size = 2.4, fill = "white",
             colour = "grey40", stroke = 0.7) +
  geom_point(aes(x = model_pct_ident), size = 2.6, colour = "#1a6faf") +
  facet_wrap(~label, ncol = 3, scales = "free_y") +
  scale_colour_manual(values = c(high = "#1b7837", medium = "#e8a33d",
                                 low = "#b2182b"),
                      name = "Benchmark confidence") +
  scale_x_continuous(limits = c(0, 100), name = "Diet share (%)") +
  theme_rd() + theme(legend.position = "bottom") +
  labs(y = NULL,
       subtitle = paste("Bars: unverified literature range, tick at the",
                        "central value. Filled blue: modelled share over",
                        "identifiable\nprey. Hollow: raw modelled share with",
                        "the background plankton resource counted as prey."))
sv(p, "model_vs_benchmark_diet", 12, 9)

cat("\n[2] how much of each diet is background resource\n")
rs <- model %>% filter(prey_group == "Plankton resource") %>%
  mutate(label = factor(PRED_DISPLAY[predator_group], levels = PRED_DISPLAY))
p2 <- ggplot(rs, aes(model_pct, reorder(label, model_pct))) +
  geom_col(fill = "#c8e6a0", colour = "grey55", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.0f%%", model_pct)), hjust = -0.15,
            size = 2.8) +
  scale_x_continuous(limits = c(0, 108), expand = expansion(mult = c(0, 0))) +
  theme_rd() +
  labs(x = "Share of modelled diet from the background plankton resource (%)",
       y = NULL,
       subtitle = paste("The resource spans", signif(min(PARAMS@w_full), 2),
                        "to", PARAMS@resource_params$w_pp_cutoff,
                        "g and is not a prey category a diet study reports."))
sv(p2, "background_resource_share_of_diet", 8, 6)

wcsv(cmp %>% select(-label) %>%
       mutate(across(where(is.numeric), ~round(.x, 4))),
     "RD05_model_vs_benchmark")
cat("\nRD05 complete. Benchmark values remain UNVERIFIED.\n")