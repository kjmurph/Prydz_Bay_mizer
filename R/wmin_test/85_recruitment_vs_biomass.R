# =============================================================================
# Phase 85 -- recruitment against biomass for the whale groups
#
# THE QUESTION. Does modelled recruitment respond to the stock, or is it
# buffered? Two views of the same data, because they answer it differently:
#
#   panel a  biomass and recruitment as trajectories, plus their RATIO
#            (recruitment per unit biomass). A rising ratio IS the buffer: it
#            says recruitment held up while the stock fell.
#   panel b  the STOCK-RECRUITMENT RELATIONSHIP -- recruitment plotted AGAINST
#            biomass, both relative to 1841, coloured by year, with a 1:1 line.
#            Points on the 1:1 line mean recruitment tracks the stock; a flat
#            cloud means it does not. This is the conventional diagnostic and
#            carries the argument without needing the ratio explained.
#
# Everything is relative to each member's OWN 1841 value before summarising, so
# the ~200x across-member spread in absolute egg number cancels and the median
# describes a typical member rather than being dominated by the largest.
#
# BIOMASS IS TOTAL, not spawning stock: KC20 stores biomass per species without
# a maturity split. For the whale groups w_mat is 0.49-0.90 of w_inf and ~98% of
# numbers sit in the top size bin, so total and mature biomass are close -- but
# it is an approximation and should be said out loud.
#
# RECRUITMENT is RDD, the density-DEPENDENT rate, i.e. what actually enters the
# smallest size class. RDI (the density-independent rate) is also in the table
# and moves very differently; do not mix them up.
#
# USAGE  Rscript R/wmin_test/85_recruitment_vs_biomass.R
# ENV    P85_STEM, P85_SUFFIX, P85_SPECIES, P85_OUT
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(tidyr); library(patchwork)
})

OL <- "Output_large_files/wmin_test"
STEM <- Sys.getenv("P85_STEM", "77_rmaxpostcap_n500_nomg_recap09_K1_R_max")
SUFFIX <- Sys.getenv("P85_SUFFIX", "p77n166")
SPSEL <- trimws(strsplit(Sys.getenv("P85_SPECIES", "baleen whales"), ",")[[1]])
FIG <- Sys.getenv("P85_OUT", file.path("Manuscript figures",
                                       "Supplemental figures"))
dir.create(FIG, recursive = TRUE, showWarnings = FALSE)

IN <- file.path(OL, paste0(STEM, "_KC20.rds"))
if (!file.exists(IN)) stop("missing ", IN, " -- run KC20b first", call. = FALSE)
Z <- readRDS(IN)
keep <- Z$members$sim_index[Z$members$stable &
                            (if ("n_erepro_ge1" %in% names(Z$members))
                               Z$members$n_erepro_ge1 == 0 else TRUE) &
                            (if ("drift_ok" %in% names(Z$members))
                               Z$members$drift_ok else TRUE)]
# OPTIONAL TOP-N SUBSET, so a "best members" figure is reproducible from the
# stored ranking rather than a pasted list. P85_RANK is a phase-93 style object
# with $ranking$sim_index in ascending yield RMSE; P85_TOP_N takes its head,
# intersected with the usable set so it can never reintroduce a rejected member.
RANK_F <- Sys.getenv("P85_RANK", "")
TOP_N  <- as.integer(Sys.getenv("P85_TOP_N", "0"))
subset_label <- sprintf("all usable (n = %d)", length(keep))
if (nzchar(RANK_F) && TOP_N > 0) {
  if (!file.exists(RANK_F)) stop("missing P85_RANK: ", RANK_F, call. = FALSE)
  RK <- readRDS(RANK_F)$ranking
  ord <- as.integer(RK$sim_index[RK$sim_index %in% keep])
  if (length(ord) < TOP_N)
    stop("P85_TOP_N=", TOP_N, " but only ", length(ord),
         " ranked members are usable", call. = FALSE)
  keep <- head(ord, TOP_N)
  subset_label <- sprintf("top %d of %d by yield RMSE", TOP_N, length(ord))
}
cat("=== Phase 85: recruitment vs biomass ===\n")
cat("stem:", STEM, "| members:", length(keep), "|", subset_label, "\n")
cat("species:", paste(SPSEL, collapse = ", "), "\n")

B <- Z$biomass %>% filter(sim_index %in% keep, Species %in% SPSEL,
                          arm == "exploited") %>%
  select(sim_index, Year, Species, Biomass)
R <- Z$recruit %>% filter(sim_index %in% keep, Species %in% SPSEL,
                          arm == "exploited") %>%
  select(sim_index, Year, Species, rdd)
D <- inner_join(B, R, by = c("sim_index", "Year", "Species")) %>%
  group_by(sim_index, Species) %>%
  arrange(Year) %>%
  mutate(b_rel = Biomass / Biomass[Year == 1841],
         r_rel = rdd / rdd[Year == 1841],
         rpb   = r_rel / b_rel) %>% ungroup()

S <- D %>% group_by(Species, Year) %>%
  summarise(across(c(b_rel, r_rel, rpb),
                   list(med = ~median(.x), lo = ~quantile(.x, .25),
                        hi = ~quantile(.x, .75)), .names = "{.col}_{.fn}"),
            .groups = "drop")

cat("\nmedian at 2010, relative to 1841:\n")
print(as.data.frame(S %>% filter(Year == 2010) %>%
        transmute(Species, biomass = round(b_rel_med, 3),
                  recruitment = round(r_rel_med, 3),
                  recruit_per_biomass = round(rpb_med, 3))), row.names = FALSE)

# --- panel a: trajectories + the ratio ---------------------------------------
long <- S %>%
  select(Species, Year, b_rel_med, b_rel_lo, b_rel_hi,
         r_rel_med, r_rel_lo, r_rel_hi, rpb_med, rpb_lo, rpb_hi) %>%
  pivot_longer(-c(Species, Year),
               names_to = c("var", ".value"), names_pattern = "(.*)_(med|lo|hi)") %>%
  mutate(var = recode(var, b_rel = "Biomass", r_rel = "Recruitment (RDD)",
                      rpb = "Recruitment / biomass"),
         var = factor(var, levels = c("Biomass", "Recruitment (RDD)",
                                      "Recruitment / biomass")))
# The trajectory panel starts at P85_YEAR_MIN: everything before whaling is a
# flat line at 1 by construction, so the pre-1920 stretch is dead space. The
# RELATIVISATION IS STILL TO 1841 -- only the drawn window is clipped, so the
# values are unchanged. Panel b is unaffected; it has no time axis.
YEAR_MIN <- as.numeric(Sys.getenv("P85_YEAR_MIN", "1920"))
long <- long %>% filter(Year >= YEAR_MIN)

pa <- ggplot(long, aes(Year, med, colour = var, fill = var)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = .18, colour = NA) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("Biomass" = "#FF61C3",
                                 "Recruitment (RDD)" = "#00B9E3",
                                 "Recruitment / biomass" = "grey20")) +
  scale_fill_manual(values = c("Biomass" = "#FF61C3",
                               "Recruitment (RDD)" = "#00B9E3",
                               "Recruitment / biomass" = "grey20")) +
  facet_wrap(~ Species, scales = "free_y", ncol = 1) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Year", y = "Relative to 1841",
       subtitle = "a  trajectories and their ratio; median and IQR")

# --- panel b: the stock-recruitment relationship ------------------------------
pb <- ggplot(S, aes(b_rel_med, r_rel_med, colour = Year)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
  geom_path(linewidth = .8) + geom_point(size = 1.4) +
  scale_colour_viridis_c(option = "plasma", end = .92) +
  facet_wrap(~ Species, scales = "free", ncol = 1) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Biomass, relative to 1841", y = "Recruitment (RDD), relative to 1841",
       subtitle = paste("b  stock-recruitment; the dashed 1:1 line is",
                        "recruitment tracking the stock"))

fig <- pa | pb
n_sp <- length(unique(S$Species))
png_out <- file.path(FIG, sprintf("recruitment_vs_biomass_%s.png", SUFFIX))
pdf_out <- file.path(FIG, sprintf("recruitment_vs_biomass_%s.pdf", SUFFIX))
ggsave(png_out, fig, width = 12, height = 4.2 * n_sp + 1, dpi = 300,
       limitsize = FALSE)
ggsave(pdf_out, fig, width = 12, height = 4.2 * n_sp + 1, limitsize = FALSE)
write.csv(S, file.path(OL, sprintf("85_recruit_vs_biomass_%s.csv", SUFFIX)),
          row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n", sep = "")