# =============================================================================
# GF01 -- growth curves and feeding levels for the REFERENCE MODEL
#
# Two figures from params_ref_p100_mort_kernel_diet.rds (the phase-104 base):
#   growth_curves_reference   19 panels, size at age, w_max and w_mat marked
#   feeding_level_reference   19 panels, feeding level against body mass
#
# Both are evaluated at t = 0, which therMizer resolves to model year 1841 --
# see the note in GF00_common.R. Growth uses the repaired integrator there;
# mizer's own getGrowthCurves() errors on this model (minke whales).
#
# USAGE  Rscript R/growth_feeding/GF01_reference.R
# ENV    GF_BASE, GF_OUT, GF_MAX_AGE, GF_T
# =============================================================================

source("R/growth_feeding/GF00_common.R")

BASE    <- Sys.getenv("GF_BASE", "params_ref_p100_mort_kernel_diet.rds")
OUT     <- Sys.getenv("GF_OUT", file.path("Manuscript figures", "p104 figures",
                                          "Growth and feeding"))
MAX_AGE <- as.numeric(Sys.getenv("GF_MAX_AGE", "20"))
TT      <- as.numeric(Sys.getenv("GF_T", "0"))
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

ref <- suppressWarnings(validParams(readRDS(BASE)))
sp  <- sp_order(ref)
cat("=== GF01: reference model ===\n")
cat("base:", basename(BASE), "| species:", length(sp), "| max_age:", MAX_AGE,
    "| t:", TT, "(model year 1841)\n")

# --- growth ------------------------------------------------------------------
G <- suppressWarnings(growth_curves(ref, max_age = MAX_AGE, t = TT))
if (!all(is.finite(G))) stop("non-finite growth curve", call. = FALSE)
gd <- gc_long(G) |> mutate(Species = factor(Species, levels = sp))

ann <- data.frame(Species = factor(sp, levels = sp),
                  w_max = ref@species_params$w_max,
                  w_mat = ref@species_params$w_mat)

VB <- vb_curve(ref, unique(gd$Age))
if (is.null(VB)) stop("species_params lacks a/b/k_vb/w_inf -- no vB curve",
                      call. = FALSE)
VB$Species <- factor(VB$Species, levels = sp)
n_round <- length(unique(VB$Species[VB$k_vb_round]))

p_g <- ggplot(mapping = aes(Age, w)) +
  geom_hline(data = ann, aes(yintercept = w_max), linetype = "dashed",
             colour = "grey45", linewidth = .4) +
  geom_hline(data = ann, aes(yintercept = w_mat), linetype = "dotted",
             colour = "grey45", linewidth = .4) +
  # vB drawn UNDER the model line: it is the comparison, not the result
  geom_line(data = VB, aes(y = w_vb, linetype = vb_class), colour = "grey25",
            linewidth = .7) +
  geom_line(data = gd, aes(colour = Species), linewidth = 1) +
  facet_wrap(~ Species, scales = "free_y", ncol = 4) +
  scale_colour_manual(values = sp_cols, guide = "none") +
  scale_linetype_manual(values = c("von Bertalanffy (fitted k_vb)" = "dashed",
                                   "von Bertalanffy (round-default k_vb)" = "dotted"),
                        name = NULL) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si("g"))) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank()) +
  labs(x = "Age (years)", y = "Body mass",
       title = "Reference model growth curves",
       subtitle = paste0(basename(BASE),
         " | coloured = modelled, grey = von Bertalanffy | grey dashed h-line = w_max, dotted = w_mat",
         "\nk_vb is a ROUND DEFAULT for ", n_round, " of ", length(sp),
         " groups (dotted vB): for those the vB curve is a placeholder, not independent data",
         " | model year 1841"))

ggsave(file.path(OUT, "growth_curves_reference.png"), p_g,
       width = 13, height = 10, dpi = 300)
ggsave(file.path(OUT, "growth_curves_reference.pdf"), p_g, width = 13, height = 10)

# --- feeding level ------------------------------------------------------------
FL <- feeding_level(ref, t = TT)
fd <- fl_long(FL, ref) |> filter(!is.na(feeding_level)) |>
  mutate(Species = factor(Species, levels = sp))

CF <- cfl_long(ref) |> mutate(Species = factor(Species, levels = sp))

p_f <- ggplot(mapping = aes(w)) +
  # f_crit: intake exactly covers metabolism, so growth is zero. The gap between
  # the realised and critical lines IS the surplus available for growth and
  # reproduction; where they meet, the individual is not growing.
  geom_line(data = CF, aes(y = f_crit, linetype = "Critical feeding level"),
            colour = "grey25", linewidth = .7) +
  geom_line(data = fd, aes(y = feeding_level, colour = Species), linewidth = 1) +
  facet_wrap(~ Species, scales = "free_x", ncol = 4) +
  scale_colour_manual(values = sp_cols, guide = "none") +
  scale_linetype_manual(values = c("Critical feeding level" = "dashed"),
                        name = NULL) +
  # label_log() emits fractional exponents (10^2.48) on the groups whose mass
  # range spans well under a decade -- small divers, leopard seals, minke. SI
  # units read correctly at any range and match the growth figure's y axis.
  scale_x_log10(labels = scales::label_number(scale_cut = scales::cut_si("g"))) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank()) +
  labs(x = "Body mass (g)", y = "Feeding level",
       title = "Reference model feeding levels",
       subtitle = paste0(basename(BASE),
         " | coloured = realised, grey dashed = critical (growth zero below it)",
         " | drawn between w_min and w_max only | model year 1841"))

ggsave(file.path(OUT, "feeding_level_reference.png"), p_f,
       width = 13, height = 10, dpi = 300)
ggsave(file.path(OUT, "feeding_level_reference.pdf"), p_f, width = 13, height = 10)

# --- what the numbers say ------------------------------------------------------
summ <- gd |> group_by(Species) |> summarise(w_at_max_age = max(w), .groups = "drop") |>
  left_join(ann, by = "Species") |>
  mutate(pct_of_w_max = round(100 * w_at_max_age / w_max, 1)) |>
  left_join(fd |> group_by(Species) |>
              summarise(fl_median = round(median(feeding_level), 3),
                        fl_min = round(min(feeding_level), 3),
                        fl_max = round(max(feeding_level), 3), .groups = "drop"),
            by = "Species") |>
  left_join(CF |> group_by(Species) |>
              summarise(fcrit_median = round(median(f_crit), 3), .groups = "drop"),
            by = "Species") |>
  # how close to not growing at all: <= 0 means intake does not cover metabolism
  mutate(margin_median = round(fl_median - fcrit_median, 3)) |>
  left_join(VB |> group_by(Species) |>
              summarise(k_vb = first(k_vb), k_vb_round = first(k_vb_round),
                        w_vb_at_max_age = max(w_vb), .groups = "drop"),
            by = "Species") |>
  mutate(model_vs_vB = round(w_at_max_age / w_vb_at_max_age, 3))
print(as.data.frame(summ |> select(Species, pct_of_w_max, fl_median, fcrit_median,
                                   margin_median, k_vb, k_vb_round, model_vs_vB)),
      row.names = FALSE)
write.csv(summ, file.path(OUT, "GF01_reference_summary.csv"), row.names = FALSE)
cat("\nWrote 2 figures + summary to", OUT, "\n")