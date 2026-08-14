# =============================================================================
# RD01 -- emergent diet composition by predator body size, reference model
#
# WHAT
#   1. diet_by_size_reference      19-panel stacked area, 8 prey groups. The
#                                  single-model analogue of the published
#                                  diet_by_size_contemporary_kernel158 figure.
#   2. per_species/diet_<slug>     one figure per species, two panels:
#                                    (a) the same 8-group stacked area
#                                    (b) ALL 21 prey resolved, as lines on a
#                                        log10 y-axis
#                                  Panel (b) is where the finer detail is: a
#                                  stacked area cannot show a 1% prey, and
#                                  mizer's own plotDiet suppresses anything
#                                  below 1 permille.
#   3. diet_by_size_mizer_native   plotDiet(params) unmodified, as an
#                                  independent check on the aggregation above.
#
# Diet comes from mizer::getDiet(proportion = TRUE). That is safe on this
# therMizer params -- see the note in RD00_common.R, and the assertion that runs
# on every source(). Absolute consumption rates would NOT be safe.
#
# USAGE  Rscript "Reference model diet assessment/RD01_diet_by_size.R"
# ENV    see RD00_common.R
# =============================================================================

source(file.path("Reference model diet assessment", "RD00_common.R"))
cat("=== RD01: diet by predator body size ===\n")

DIET <- getDiet(PARAMS, proportion = TRUE)          # [predator x w x prey]
PREY <- dimnames(DIET)$prey
cat("diet array:", paste(dim(DIET), collapse = " x "), "\n")

# --- map prey to the 8 groups; "External" is dropped, as F07 does -------------
prey_to_group <- setNames(rep(NA_character_, length(PREY)), PREY)
for (g in names(PREY_GROUPS))
  prey_to_group[intersect(PREY_GROUPS[[g]], PREY)] <- g
keep <- PREY[!is.na(prey_to_group)]
dropped <- setdiff(PREY, keep)
cat("prey mapped:", length(keep), "of", length(PREY),
    "| dropped:", paste(dropped, collapse = ", "), "\n")
cat(sprintf("  dropped prey carry at most %.3g of any predator's diet\n",
            if (length(dropped))
              max(DIET[, , dropped, drop = FALSE]) else 0))

groups <- names(PREY_GROUPS)
GS <- array(0, dim = c(NS, NW, length(groups)),
            dimnames = list(predator = SPECIES,
                            w = dimnames(PARAMS@initial_n)$w,
                            prey_group = groups))
for (g in groups) {
  pg <- keep[prey_to_group[keep] == g]
  GS[, , g] <- apply(DIET[, , pg, drop = FALSE], c(1, 2), sum)
}
tot <- apply(GS, c(1, 2), sum)
GS  <- sweep(GS, c(1, 2), pmax(tot, 1e-300), "/")     # renormalise after drops

# assertion 3: every occupied stack sums to 1
resid <- abs(apply(GS, c(1, 2), sum) - 1)[OCC]
cat(sprintf("assert stacks sum to 1: max |resid| = %.3g\n", max(resid)))
stopifnot(max(resid) < 1e-10)

# --- long frames --------------------------------------------------------------
occ_df <- as.data.frame(as.table(OCC), stringsAsFactors = FALSE)
names(occ_df) <- c("predator", "w_lab", "occupied")
occ_df <- occ_df[occ_df$occupied, c("predator", "w_lab")]

to_long <- function(A, value) {
  d <- as.data.frame(as.table(A), stringsAsFactors = FALSE)
  names(d) <- c("predator", "w_lab", "prey", value)
  d$size_g <- PARAMS@w[match(d$w_lab, dimnames(PARAMS@initial_n)$w)]
  merge(d, occ_df, by = c("predator", "w_lab"))
}
grp_long  <- to_long(GS, "proportion")
names(grp_long)[names(grp_long) == "prey"] <- "prey_group"
full_long <- to_long(DIET, "proportion")

grp_long$prey_group     <- factor(grp_long$prey_group, levels = PREY_GROUP_ORDER)
grp_long$predator_label <- factor(PRED_DISPLAY[grp_long$predator],
                                  levels = PRED_DISPLAY)

# --- 1. the 19-panel figure ---------------------------------------------------
cat("\n[1] 19-panel diet by size\n")
p1 <- ggplot(grp_long, aes(log10(size_g), proportion, fill = prey_group)) +
  geom_area(position = "stack", colour = NA) +
  facet_wrap(~predator_label, ncol = 4, scales = "free_x") +
  scale_fill_manual(values = PREY_GROUP_COLS, name = "Prey group") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_x_continuous(name = "Predator body size (log10 g)",
                     expand = expansion(mult = c(0.01, 0.01))) +
  theme_panels() +
  labs(y = "Diet proportion",
       subtitle = sprintf(paste("Emergent diet by predator body size,",
                                "reference model (%s)"), basename(RD_PARAMS)))
sv(p1, "diet_by_size_reference", 14, 18)

# --- 2. one figure per species ------------------------------------------------
cat("\n[2] per-species figures\n")

# Colours for the full 21-prey panel: each prey takes a shade of its group's
# colour, so the two panels of a figure read as the same palette.
shade <- function(hex, f) {
  v <- as.numeric(col2rgb(hex)) / 255
  v <- if (f <= 1) v * f else v + (1 - v) * (f - 1)
  rgb(v[1], v[2], v[3])
}
PREY_COLS <- unlist(lapply(names(PREY_GROUPS), function(g) {
  m <- intersect(PREY_GROUPS[[g]], keep)
  f <- if (length(m) == 1) 1 else seq(0.55, 1.55, length.out = length(m))
  setNames(vapply(f, shade, "", hex = PREY_GROUP_COLS[[g]]), m)
}))
# shade alone does not separate four greens, so vary linetype within group too
PREY_LTY <- unlist(lapply(names(PREY_GROUPS), function(g) {
  m <- intersect(PREY_GROUPS[[g]], keep)
  setNames(c("solid", "22", "44", "1343")[seq_along(m)], m)
}))
PREY_LABEL <- c(PRED_DISPLAY, "Resource" = "Plankton resource")
FLOOR <- 1e-6      # log axis floor; prey below this are not plotted
PCT_BREAKS <- 10^(-6:0)
PCT_LABELS <- c("0.0001%", "0.001%", "0.01%", "0.1%", "1%", "10%", "100%")

for (s in SPECIES) {
  gd <- grp_long[grp_long$predator == s, ]
  fd <- full_long[full_long$predator == s & full_long$prey %in% keep, ]
  fd <- fd[fd$proportion >= FLOOR, ]
  fd$prey_label <- factor(PREY_LABEL[fd$prey],
                          levels = PREY_LABEL[names(PREY_COLS)])
  cols <- setNames(PREY_COLS, PREY_LABEL[names(PREY_COLS)])
  ltys <- setNames(PREY_LTY, PREY_LABEL[names(PREY_LTY)])

  rng <- range(log10(gd$size_g))
  sub <- sprintf("%s  |  %.3g to %.3g g  (w_mat %.3g g)  |  %d size bins",
                 s, 10^rng[1], 10^rng[2],
                 PARAMS@species_params$w_mat[match(s, SPECIES)],
                 length(unique(gd$w_lab)))

  pa <- ggplot(gd, aes(log10(size_g), proportion, fill = prey_group)) +
    geom_area(position = "stack", colour = NA) +
    scale_fill_manual(values = PREY_GROUP_COLS, name = "Prey group") +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.02))) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
    theme_rd() +
    labs(x = NULL, y = "Diet proportion",
         title = PRED_DISPLAY[[s]], subtitle = sub)

  pb <- ggplot(fd, aes(log10(size_g), proportion, colour = prey_label,
                       linetype = prey_label)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 0.8, show.legend = FALSE) +
    scale_colour_manual(values = cols, name = "Prey", drop = TRUE) +
    scale_linetype_manual(values = ltys, name = "Prey", drop = TRUE) +
    scale_y_log10(breaks = PCT_BREAKS, labels = PCT_LABELS,
                  limits = c(FLOOR, 1)) +
    scale_x_continuous(limits = rng,
                       expand = expansion(mult = c(0.01, 0.01))) +
    annotation_logticks(sides = "l", linewidth = 0.2) +
    theme_rd() +
    labs(x = "Predator body size (log10 g)",
         y = "Diet proportion (log scale)",
         caption = sprintf("All prey resolved; prey below %g omitted.", FLOOR))

  sv(pa / pb + plot_layout(heights = c(1, 1.15)),
     paste0("diet_", slugify(s)), 7.5, 8, subdir = "per_species")
}

# --- 3. mizer's own plotDiet, as an independent check -------------------------
cat("\n[3] mizer-native plotDiet\n")
sv(plotDiet(PARAMS) + theme(legend.position = "bottom"),
   "diet_by_size_mizer_native", 14, 18)

# --- CSVs ---------------------------------------------------------------------
cat("\n[4] tables\n")
wcsv(grp_long[, c("predator", "size_g", "prey_group", "proportion")] %>%
       arrange(predator, size_g, prey_group),
     "RD01_diet_by_size_grouped")
wcsv(full_long[, c("predator", "size_g", "prey", "proportion")] %>%
       arrange(predator, size_g, prey),
     "RD01_diet_by_size_full")

# composition at three reference sizes per predator, plus a biomass-weighted
# whole-ontogeny mean (the number to quote when a single figure is wanted)
sp <- PARAMS@species_params
ref_rows <- lapply(seq_len(NS), function(i) {
  k <- which(OCC[i, ])
  wts <- PARAMS@initial_n[i, k] * PARAMS@w[k] * PARAMS@dw[k]
  wts <- wts / sum(wts)
  pick <- c(w_min = k[1],
            w_mat = k[which.min(abs(PARAMS@w[k] - sp$w_mat[i]))],
            w_max = k[length(k)])
  do.call(rbind, c(
    lapply(names(pick), function(nm) data.frame(
      predator = SPECIES[i], at = nm, size_g = PARAMS@w[pick[[nm]]],
      prey_group = groups, proportion = GS[i, pick[[nm]], ])),
    list(data.frame(
      predator = SPECIES[i], at = "biomass_weighted", size_g = NA_real_,
      prey_group = groups,
      proportion = as.numeric(wts %*% GS[i, k, , drop = FALSE][1, , ])))))
})
summ <- do.call(rbind, ref_rows)
wcsv(summ, "RD01_diet_summary")

# --- what to quote ------------------------------------------------------------
cat("\n=== biomass-weighted diet composition, reference model (%) ===\n")
wide <- summ %>% filter(at == "biomass_weighted") %>%
  mutate(pct = round(100 * proportion, 2)) %>%
  select(predator, prey_group, pct) %>%
  pivot_wider(names_from = prey_group, values_from = pct)
wide$predator <- PRED_DISPLAY[wide$predator]
print(as.data.frame(wide), row.names = FALSE)

cat("\n=== Antarctic krill share of diet, by predator (%) ===\n")
kr <- summ %>% filter(at == "biomass_weighted",
                      prey_group == "Antarctic krill") %>%
  mutate(krill_pct = round(100 * proportion, 2),
         predator = PRED_DISPLAY[predator]) %>%
  select(predator, krill_pct) %>% arrange(desc(krill_pct))
print(as.data.frame(kr), row.names = FALSE)

cat("\nRD01 complete.\n")
