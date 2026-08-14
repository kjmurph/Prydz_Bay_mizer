# =============================================================================
# F07 -- the three whale_consumption_outputs/ diet-composition figures, rebuilt
# on the revised-kernel ensemble.
#
#   1. baleen_diet_stacked_area            -- baleen whale diet over time
#   2. diet_proportion_timeseries          -- 19-panel diet over time
#   3. diet_by_size_contemporary           -- 19-panel diet vs predator body size
#
# Prey groupings, palettes, labels, themes and layouts are transcribed verbatim
# from the originals (plot_baleen_diet_composition.R,
# plot_diet_proportion_timeseries_top10pct.R,
# plot_diet_by_size_contemporary_top10pct.R) so the only thing that changes is
# the underlying ensemble.
#
# TWO DELIBERATE DEPARTURES FROM THE ORIGINALS, both stated on the figures:
#
#  1. The diet-by-size panel was built from the SINGLE best-RMSE simulation. Here
#     it is the ENSEMBLE MEDIAN across all members, matching the other two
#     figures and what "rebuilt on the ensemble" should mean. Medians are taken
#     per predator-size-prey cell and renormalised, exactly as the time-series
#     figure does.
#  2. Figure 1's prey palette was assigned by rank of mean proportion, so the
#     colour of a given prey depends on the data. That would silently recolour
#     the figure between builds and make it uncomparable to the published one, so
#     prey are given FIXED colours here by name.
#
# Source data: Manuscript data/diet_composition_<SUF>.rds, from
# F00s_diet_composition_from_states.R -- ther_diet() with an explicit year, so it
# does not carry the getDiet(t = 0) bug the legacy arrays do.
#
# USAGE  Rscript "Manuscript scripts/F07_diet_composition_figures.R"
# ENV: F07_SUF (default kernel158), F07_OUT (default the supplemental folder)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(scales); library(grid)
})

DATA <- "Manuscript data"
SUF  <- Sys.getenv("F07_SUF", "kernel158")
OUT  <- Sys.getenv("F07_OUT", file.path("Manuscript figures", "Supplemental figures"))
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
YEAR_MIN <- 1901
guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE); f
}
sv <- function(p, stem, w, h) {
  png <- guard(file.path(OUT, sprintf("%s_%s.png", stem, SUF)))
  pdf <- guard(file.path(OUT, sprintf("%s_%s.pdf", stem, SUF)))
  ggsave(png, p, width = w, height = h, dpi = 300)
  ggsave(pdf, p, width = w, height = h)
  cat("  wrote", basename(png), "and .pdf\n")
}

D <- readRDS(file.path(DATA, sprintf("diet_composition_%s.rds", SUF)))
cat("=== F07: diet composition figures,", SUF, "===\n")
cat("members:", length(D$members), "| years", min(D$years), "-", max(D$years),
    "| base:", basename(D$base_params), "\n")

CONS <- D$cons          # [sim x year x predator x prey], g/yr
SZ   <- D$size_prop     # [sim x predator x w x prey], proportion, 2001-2010 mean
SPN  <- D$species; PREY <- D$prey; WV <- D$w

# --- groupings and palettes, transcribed from the originals -------------------
PREY_GROUPS <- list(
  "Antarctic krill" = "antarctic krill",
  "Other LTL"       = c("mesozooplankton", "other krill",
                        "other macrozooplankton", "salps"),
  "Fishes"          = c("mesopelagic fishes", "bathypelagic fishes",
                        "shelf and coastal fishes", "toothfishes"),
  "Squids"          = "squids",
  "Seabirds"        = c("flying birds", "small divers"),
  "Pinnipeds"       = c("leopard seals", "medium divers", "large divers"),
  "Cetaceans"       = c("minke whales", "orca", "sperm whales", "baleen whales"),
  "Plankton resource" = "Resource")     # "External" deliberately excluded
PREY_GROUP_COLS <- c("Antarctic krill" = "#e8534a", "Other LTL" = "#6dbf6b",
  "Fishes" = "#1a6faf", "Squids" = "#e87c10", "Seabirds" = "#9e9e9e",
  "Pinnipeds" = "#a0522d", "Cetaceans" = "#9467bd",
  "Plankton resource" = "#c8e6a0")
PREY_GROUP_ORDER <- rev(names(PREY_GROUP_COLS))
PRED_DISPLAY <- c(
  "mesozooplankton" = "Mesozooplankton", "other krill" = "Other krill",
  "other macrozooplankton" = "Other macrozooplankton",
  "antarctic krill" = "Antarctic krill", "salps" = "Salps",
  "mesopelagic fishes" = "Mesopelagic fishes",
  "bathypelagic fishes" = "Bathypelagic fishes",
  "shelf and coastal fishes" = "Shelf & coastal fishes",
  "flying birds" = "Flying birds", "small divers" = "Small divers",
  "squids" = "Squids", "toothfishes" = "Toothfishes",
  "leopard seals" = "Leopard seals", "medium divers" = "Medium divers",
  "large divers" = "Large divers", "minke whales" = "Minke whales",
  "orca" = "Orca", "sperm whales" = "Sperm whales",
  "baleen whales" = "Large baleen whales")

prey_to_group <- setNames(rep(NA_character_, length(PREY)), PREY)
for (g in names(PREY_GROUPS))
  prey_to_group[intersect(PREY_GROUPS[[g]], PREY)] <- g
keep <- PREY[!is.na(prey_to_group)]
cat("prey mapped:", length(keep), "of", length(PREY),
    "| excluded:", paste(setdiff(PREY, keep), collapse = ", "), "\n")

theme_panels <- function() theme_bw(base_size = 10) + theme(
  strip.text = element_text(face = "bold", size = 8),
  axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7),
  legend.position = "bottom", legend.key.size = unit(0.4, "cm"),
  legend.text = element_text(size = 8),
  legend.title = element_text(size = 9, face = "bold"),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# =============================================================================
# 1. Baleen whale diet over time -- stacked area of median proportions
# =============================================================================
cat("\n[1] baleen diet stacked area\n")
WONG <- c(vermilion = "#D55E00", blue = "#0072B2", sky_blue = "#56B4E9",
          green = "#009E73", orange = "#E69F00", pink = "#CC79A7",
          yellow = "#F0E442", black = "#000000")
theme_prydz <- function() theme_classic(base_size = 7, base_family = "sans") +
  theme(strip.background = element_rect(fill = "white", colour = "grey75",
                                        linewidth = 0.5),
        strip.text = element_text(face = "bold", size = 7, colour = "grey10"),
        panel.grid.major = element_line(colour = "grey93", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey75", fill = NA, linewidth = 0.5),
        axis.line = element_blank(),
        axis.text = element_text(size = 6, colour = "grey10"),
        axis.title = element_text(size = 7), legend.position = "bottom",
        legend.text = element_text(size = 6))

# per member per year, baleen diet proportion by prey; then the across-member median
bal <- CONS[, , "baleen whales", keep, drop = FALSE]     # [sim x year x 1 x prey]
dim(bal) <- dim(bal)[c(1, 2, 4)]
dimnames(bal) <- list(sim = dimnames(CONS)$sim, year = dimnames(CONS)$year,
                      prey = keep)
tot <- apply(bal, c(1, 2), sum)
prop <- sweep(bal, c(1, 2), pmax(tot, 1e-300), "/")
med <- apply(prop, c(2, 3), median, na.rm = TRUE)        # [year x prey]

major <- keep[apply(med, 2, max) > 0.01]
cat("  major prey (>1% median in any year):", paste(sort(major), collapse = ", "), "\n")
cat("  pooled into 'Other prey':", paste(sort(setdiff(keep, major)), collapse = ", "), "\n")

# GROUP WITHIN EACH MEMBER, THEN take the median, THEN renormalise.
# Doing it the other way round -- median per prey, then summing the sub-1% medians
# -- is what the original script did, and it leaves the stack short of 100%: a
# median is not additive, so the summed medians of the minor prey recover only a
# fraction of the diet they actually represent (0.4% against a real ~8%). Pooling
# first makes "Other prey" a genuine group; renormalising then closes the residual
# median non-additivity across the retained groups, which is the same treatment
# the 19-panel figure applies.
grp_names <- c(major, "Other")
G1 <- array(0, dim = c(dim(prop)[1], dim(prop)[2], length(grp_names)),
            dimnames = list(sim = NULL, year = dimnames(prop)$year,
                            prey_group = grp_names))
for (nm in major) G1[, , nm] <- prop[, , nm]
minor <- setdiff(keep, major)
if (length(minor))
  G1[, , "Other"] <- apply(prop[, , minor, drop = FALSE], c(1, 2), sum)
med <- apply(G1, c(2, 3), median, na.rm = TRUE)          # [year x group]
pre <- rowSums(med)
med <- sweep(med, 1, pmax(pre, 1e-300), "/")
cat(sprintf("  stack completeness before renormalising: %.1f%%-%.1f%% of diet\n",
            100 * min(pre), 100 * max(pre)))
# FIXED colours by prey name -- see the header note. The original ranked by mean
# proportion, which makes the palette data-dependent and the figure uncomparable.
PREY_COLS_FIXED <- c(
  "Resource" = unname(WONG["vermilion"]),
  "bathypelagic fishes" = unname(WONG["blue"]),
  "salps" = unname(WONG["sky_blue"]),
  "mesopelagic fishes" = unname(WONG["green"]),
  "antarctic krill" = unname(WONG["orange"]),
  "shelf and coastal fishes" = unname(WONG["pink"]),
  "other macrozooplankton" = unname(WONG["yellow"]),
  "toothfishes" = unname(WONG["black"]),
  "other krill" = "#666666", "mesozooplankton" = "#B3B3B3",
  "Other" = "#999999")
PREY_LABELS <- c("Resource" = "Background resource",
  "bathypelagic fishes" = "Bathypelagic fishes", "salps" = "Salps",
  "mesopelagic fishes" = "Mesopelagic fishes",
  "antarctic krill" = "Antarctic krill",
  "shelf and coastal fishes" = "Shelf & coastal fishes",
  "other macrozooplankton" = "Other macrozooplankton",
  "toothfishes" = "Toothfishes", "other krill" = "Other krill",
  "mesozooplankton" = "Mesozooplankton", "Other" = "Other prey")

# `med` already carries the final groups as its columns, so no further pooling.
pd <- as.data.frame(as.table(med), stringsAsFactors = FALSE)
names(pd) <- c("year", "prey_group", "median_prop")
pd$year <- as.numeric(pd$year)
lev <- c("Resource", setdiff(names(PREY_COLS_FIXED),
                             c("Resource", "Other")), "Other")
pd$prey_group <- factor(pd$prey_group, levels = lev[lev %in% pd$prey_group])

p1 <- ggplot(pd, aes(year, median_prop, fill = prey_group)) +
  geom_area(position = "stack", alpha = 0.85, colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = PREY_COLS_FIXED, labels = PREY_LABELS,
                    guide = guide_legend(title = NULL, nrow = 2, byrow = TRUE)) +
  scale_x_continuous(breaks = seq(1850, 2010, 20)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1.001), expand = c(0, 0)) +
  labs(x = "Year", y = "Proportion of total consumption") +
  theme_prydz() +
  theme(legend.key.size = unit(3, "mm"), legend.spacing.x = unit(1, "mm"))
sv(p1, "baleen_diet_stacked_area", 7, 4)

# =============================================================================
# 2. Diet proportion time series, all 19 predators
# =============================================================================
cat("\n[2] diet proportion time series\n")
groups <- unique(prey_to_group[keep])
yrs <- as.numeric(dimnames(CONS)$year)
yi <- which(yrs >= YEAR_MIN)
# per member: collapse prey to groups, convert to proportions, then take the
# across-member median and renormalise -- the original's exact sequence.
G <- array(0, dim = c(dim(CONS)[1], length(yi), length(SPN), length(groups)),
           dimnames = list(sim = NULL, year = dimnames(CONS)$year[yi],
                           predator = SPN, prey_group = groups))
for (g in groups) {
  pg <- keep[prey_to_group[keep] == g]
  G[, , , g] <- apply(CONS[, yi, , pg, drop = FALSE], c(1, 2, 3), sum)
}
tot <- apply(G, c(1, 2, 3), sum)
P <- sweep(G, c(1, 2, 3), pmax(tot, 1e-300), "/")
P[!is.finite(P)] <- 0
MED <- apply(P, c(2, 3, 4), median, na.rm = TRUE)        # [year x pred x group]
MED <- sweep(MED, c(1, 2), pmax(apply(MED, c(1, 2), sum), 1e-300), "/")

df2 <- as.data.frame(as.table(MED), stringsAsFactors = FALSE)
names(df2) <- c("year", "predator", "prey_group", "med_prop")
df2$year <- as.numeric(df2$year)
df2$prey_group <- factor(df2$prey_group, levels = PREY_GROUP_ORDER)
df2$predator_label <- factor(PRED_DISPLAY[df2$predator], levels = PRED_DISPLAY)

p2 <- ggplot(df2, aes(year, med_prop, fill = prey_group)) +
  geom_area(position = "stack", colour = NA) +
  facet_wrap(~predator_label, ncol = 4) +
  scale_fill_manual(values = PREY_GROUP_COLS, name = "Prey group") +
  scale_x_continuous(breaks = c(1920, 1960, 2000),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  theme_panels() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
  labs(x = "Year", y = "Diet proportion (median across ensemble)")
sv(p2, "diet_proportion_timeseries", 14, 18)

# =============================================================================
# 3. Contemporary diet by predator body size
# =============================================================================
cat("\n[3] diet by predator body size\n")
# The w dimension must carry explicit labels: as.table() on a NULL dimname
# invents letters, and as.integer() on those silently yields NA.
GS <- array(0, dim = c(dim(SZ)[1], length(SPN), length(WV), length(groups)),
            dimnames = list(sim = NULL, predator = SPN,
                            w = as.character(seq_along(WV)),
                            prey_group = groups))
for (g in groups) {
  pg <- keep[prey_to_group[keep] == g]
  GS[, , , g] <- apply(SZ[, , , pg, drop = FALSE], c(1, 2, 3), sum)
}
MEDS <- apply(GS, c(2, 3, 4), median, na.rm = TRUE)      # [pred x w x group]
MEDS <- sweep(MEDS, c(1, 2), pmax(apply(MEDS, c(1, 2), sum), 1e-300), "/")

df3 <- as.data.frame(as.table(MEDS), stringsAsFactors = FALSE)
names(df3) <- c("predator", "wi", "prey_group", "proportion")
df3$size_g <- WV[as.integer(df3$wi)]
df3$prey_group <- factor(df3$prey_group, levels = PREY_GROUP_ORDER)
df3$predator_label <- factor(PRED_DISPLAY[df3$predator], levels = PRED_DISPLAY)
# drop size bins the predator does not occupy, so panels span only real sizes
occ <- df3 %>% group_by(predator, wi) %>%
  summarise(any = sum(proportion) > 1e-12, .groups = "drop") %>% filter(any)
df3 <- df3 %>% semi_join(occ, by = c("predator", "wi"))

p3 <- ggplot(df3, aes(log10(size_g), proportion, fill = prey_group)) +
  geom_area(position = "stack", colour = NA) +
  facet_wrap(~predator_label, ncol = 4, scales = "free_x") +
  scale_fill_manual(values = PREY_GROUP_COLS, name = "Prey group") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_x_continuous(name = "Predator body size (log10 g)",
                     expand = expansion(mult = c(0.01, 0.01))) +
  theme_panels() +
  labs(y = "Diet proportion",
       subtitle = sprintf(paste("Contemporary diet by predator body size",
                                "(median across %d members, %d-%d average)"),
                          length(D$members), min(D$ref_years), max(D$ref_years)))
sv(p3, "diet_by_size_contemporary", 14, 18)

# --- what the manuscript will want to quote ----------------------------------
cat("\n=== baleen whale diet, 2001-2010 median proportion ===\n")
b <- pd %>% filter(year %in% 2001:2010) %>% group_by(prey_group) %>%
  summarise(prop = mean(median_prop), .groups = "drop") %>% arrange(desc(prop))
print(as.data.frame(b %>% mutate(pct = 100 * prop) %>% select(prey_group, pct)),
      digits = 4, row.names = FALSE)
cat("\n=== krill share of diet, 2001-2010, by predator ===\n")
kk <- df2 %>% filter(prey_group == "Antarctic krill", year %in% 2001:2010) %>%
  group_by(predator_label) %>%
  summarise(krill_pct = 100 * mean(med_prop), .groups = "drop") %>%
  arrange(desc(krill_pct))
print(as.data.frame(kk), digits = 4, row.names = FALSE)
cat("\nF07 complete.\n")