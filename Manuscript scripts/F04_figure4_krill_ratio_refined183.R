# =============================================================================
# FIGURE 4 -- Exploited / Unexploited Antarctic krill consumption, by predator
# group, refined 183-member fitted ensemble.
#
# THE BUG THIS FIXES. The published version came from
# extract_whale_consumption.R:144, which calls
#     getDiet(params, n = n, n_pp = n_pp, n_other = n_other, proportion = FALSE)
# with NO `t` argument. getFeedingLevel then defaults to t = 0 and indexes
# ocean_temp at 0 + t_idx = -1841. Its stored outputs disagree with a faithful
# recomputation by **13-80x**, varying with year (so not a units constant), and
# span 10 orders of magnitude across members at a single year. The member/index
# mapping was verified independently, so it is not an indexing error.
#
# The consumption used here comes from F00_build_refined183_data.R, which uses
# ther_diet(). NOTE: despite the `ther_` prefix, that is NOT a therMizer function
# -- it is local project code at R/wmin_test/thermizer_shim.R:97. It takes `year`
# EXPLICITLY and was verified bit-identical to mizer::getDiet(proportion = FALSE)
# (max relative difference 0) once the temperature scaling is threaded through.
#
# HOW MUCH THIS ACTUALLY CHANGES THE FIGURE -- less than the bug size suggests.
# The stored values are 13-80x too LARGE (the recomputation is the smaller
# number). But the bug inflates the exploited and unexploited arms similarly, so
# it **largely cancels in the ratio** plotted here: the rebuilt whale ratio at
# 2010 is ~0.38 against a published ~0.31-0.33. So the published Figure 4 is
# probably approximately right despite resting on wrong absolute values.
#
# What the bug definitely does invalidate is any ABSOLUTE krill-consumption
# quantity drawn from whale_consumption_outputs/ -- tonnes per year, per-predator
# totals, or consumption compared across predator groups. Those are 13-80x too
# high and should not be quoted.
#
# Writes Manuscript figures/fig4_krill_ratio_refined183.{png,pdf}
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
})

DATA <- "Manuscript data"; FIGS <- "Manuscript figures"
dir.create(FIGS, showWarnings = FALSE)
SUF <- "refined183"
guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE); f
}

KR <- readRDS(file.path(DATA, sprintf("krill_consumption_%s.rds", SUF)))
meta <- readRDS(file.path(DATA, sprintf("meta_%s.rds", SUF)))
message("members: ", meta$n_members, " | years ", min(KR$Year), "-", max(KR$Year))

FISHES <- c("mesopelagic fishes", "bathypelagic fishes",
            "shelf and coastal fishes", "toothfishes")
WHALES <- c("baleen whales", "minke whales")

# --- era markers, derived from the effort array -------------------------------
eff <- readRDS("effort_array_1841_2010.rds")
yr <- as.numeric(rownames(eff))
onset <- function(s) { y <- yr[eff[, s] > 0]; if (length(y)) min(y) else NA }
peak  <- function(s) { v <- eff[, s]; if (any(v > 0)) yr[which.max(v)] else NA }
last  <- function(s) { y <- yr[eff[, s] > 0]; if (length(y)) max(y) else NA }
whaling_start <- min(c(onset("baleen whales"), onset("sperm whales")), na.rm = TRUE)
krill_start   <- onset("antarctic krill")
krill_end     <- last("antarctic krill")
pk <- c("Peak baleen whaling" = peak("baleen whales"),
        "Peak sperm whaling"  = peak("sperm whales"),
        "Peak minke whaling"  = peak("minke whales"),
        "Peak krill fishing"  = peak("antarctic krill"))
pk <- pk[!is.na(pk)]

# --- ratio per member-year, then summarise across members ---------------------
grp_ratio <- function(species, label) {
  KR %>% filter(Species %in% species) %>%
    group_by(sim_index, arm, Year) %>%
    summarise(cons = sum(krill_consumed), .groups = "drop") %>%
    pivot_wider(names_from = arm, values_from = cons) %>%
    filter(is.finite(exploited), is.finite(unexploited), unexploited > 0) %>%
    mutate(ratio = exploited / unexploited, group = label)
}
ALL_PRED <- sort(unique(KR$Species))
R <- bind_rows(
  grp_ratio(ALL_PRED, "All predators"),
  grp_ratio(WHALES,   "Large baleen + minke whales"),
  grp_ratio(FISHES,   "All fishes"))

S <- R %>% group_by(group, Year) %>%
  summarise(med = median(ratio, na.rm = TRUE),
            lo  = quantile(ratio, 0.25, na.rm = TRUE),
            hi  = quantile(ratio, 0.75, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = factor(group, levels = c("All predators",
                                          "Large baleen + minke whales",
                                          "All fishes")))

cat("\n=== ratio at 2010 (exploited / unexploited krill consumption) ===\n")
print(as.data.frame(S %>% filter(Year == 2010) %>%
                      select(group, med, lo, hi)), digits = 4, row.names = FALSE)

cols <- c("All predators" = "black",
          "Large baleen + minke whales" = "#FF3E96",
          "All fishes" = "#D9A404")

p <- ggplot(S, aes(Year, med, colour = group, fill = group)) +
  geom_hline(yintercept = 1, colour = "grey45", linetype = "dashed",
             linewidth = 0.5) +
  annotate("text", x = 1903, y = 1.02, label = "Unexploited", hjust = 0,
           size = 3.2, colour = "grey40") +
  geom_vline(xintercept = c(whaling_start, krill_start, krill_end),
             colour = "grey40", linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = pk, colour = "grey55", linetype = "dotted",
             linewidth = 0.4) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.25, colour = NA) +
  geom_line(linewidth = 1) +
  annotate("text", x = whaling_start, y = 1.22, label = "Whaling\nstarts",
           hjust = -0.06, size = 3.3, colour = "grey30") +
  annotate("text", x = krill_start, y = 1.22, label = "Krill fishing\nstarts",
           hjust = -0.06, size = 3.3, colour = "grey30") +
  annotate("text", x = krill_end, y = 1.22, label = "End of\nkrill fishing",
           hjust = -0.06, size = 3.3, colour = "grey30") +
  lapply(seq_along(pk), function(i)
    annotate("text", x = pk[i], y = 0.02, label = names(pk)[i], angle = 90,
             hjust = 0, vjust = -0.3, size = 2.9, colour = "grey45")) +
  scale_colour_manual(values = cols, name = "Predator group") +
  scale_fill_manual(values = cols, name = "Predator group") +
  scale_x_continuous(breaks = seq(1900, 2010, 10), limits = c(1898, 2012),
                     expand = expansion(mult = 0.005)) +
  scale_y_continuous(limits = c(0, 1.28), breaks = seq(0, 1.25, 0.25)) +
  labs(x = "Year",
       y = "Exploited / Unexploited Antarctic krill consumption (ratio)") +
  theme_classic(base_size = 13) +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"))

png_out <- guard(file.path(FIGS, sprintf("fig4_krill_ratio_%s.png", SUF)))
pdf_out <- guard(file.path(FIGS, sprintf("fig4_krill_ratio_%s.pdf", SUF)))
ggsave(png_out, p, width = 11, height = 7.5, dpi = 300)
ggsave(pdf_out, p, width = 11, height = 7.5)
write.csv(S, guard(file.path(DATA, sprintf("fig4_krill_ratio_series_%s.csv", SUF))),
          row.names = FALSE)
cat("\nWrote:\n  ", png_out, "\n  ", pdf_out, "\n")
