# =============================================================================
# KC01 -- figures for the krill-fishing counterfactuals
#
# Reads KC00_scenarios.rds. Works for either a single-params pilot (one
# deterministic line per arm) or an ensemble build (adds a ribbon), detected
# from whether the data carry a sim_index column.
#
# THE HEADLINE FIGURE reproduces the published Figure 4 layout -- exploited /
# unexploited Antarctic krill consumption by predator group -- but with one
# line per SCENARIO rather than one per ensemble build, so the counterfactuals
# can be read against the observed history directly.
#
# A NOTE ON WHAT THE RATIO CAN AND CANNOT SHOW. Every arm is divided by the
# SAME unexploited run, so the ratio isolates the effect of fishing from the
# climate forcing that both arms share. It does not isolate krill fishing from
# whaling: the exploited, no_krill and peak_krill arms all carry the observed
# whaling history, so the large mid-century fall in whale krill consumption is
# whaling in all three. The krill fishery's effect is the SEPARATION between
# the three lines, not the fall itself.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC01_figures.R"
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
  library(patchwork)
})

KC_ROOT <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
ANA <- file.path(KC_ROOT, "analysis"); FIG <- file.path(KC_ROOT, "figures")
dir.create(FIG, recursive = TRUE, showWarnings = FALSE)
FORCE <- nzchar(Sys.getenv("KC_FORCE"))
sv <- function(p, stem, w, h) {
  stem <- paste0(stem, Sys.getenv("KC_SUF", ""))
  for (ext in c("png", "pdf")) {
    f <- file.path(FIG, paste0(stem, ".", ext))
    if (file.exists(f) && !FORCE)
      stop("refusing to overwrite: ", f, "\n  set KC_FORCE=1", call. = FALSE)
  }
  suppressWarnings({
    ggsave(file.path(FIG, paste0(stem, ".png")), p, width = w, height = h,
           dpi = 300)
    ggsave(file.path(FIG, paste0(stem, ".pdf")), p, width = w, height = h)
  })
  cat("  wrote", stem, "\n")
}

IN <- Sys.getenv("KC_IN", "KC00_scenarios.rds")
SUF <- Sys.getenv("KC_SUF", "")
D <- readRDS(file.path(ANA, IN))
BIO <- D$biomass; KR <- D$krill; YLD <- D$yield; meta <- D$meta
ENSEMBLE <- "sim_index" %in% names(KR)
cat("=== KC01: krill counterfactual figures ===\n")
cat("input:", IN, "| params:", basename(meta$params), "| ensemble:", ENSEMBLE,
    "| krill years", meta$krill_active[1], "-", meta$krill_active[2],
    "| peak", meta$peak_year, "\n")

# FILTER TO STABLE MEMBERS BEFORE ANY MEAN. A single divergent member can own
# most of an across-member sum; the published pipeline filters for exactly this
# reason (F00q_filter_stable.R). Medians are more robust but the filter is not
# optional -- an unstable member's ratio is meaningless, not merely extreme.
if (ENSEMBLE && !is.null(D$members)) {
  keep <- D$members$sim_index[D$members$stable]
  cat("members:", nrow(D$members), "| stable:", length(keep), "-> using stable only\n")
  if (!length(keep)) stop("no stable members", call. = FALSE)
  BIO <- BIO[BIO$sim_index %in% keep, ]
  KR  <- KR[KR$sim_index %in% keep, ]
  YLD <- YLD[YLD$sim_index %in% keep, ]
}

ARM_LAB <- c(exploited  = "Observed history",
             no_krill   = "No krill fishing",
             peak_krill = sprintf("Peak krill effort held from %d",
                                  meta$peak_year))
ARM_COL <- c(exploited = "#111111", no_krill = "#1b7837",
             peak_krill = "#b2182b")
FISHES <- c("mesopelagic fishes", "bathypelagic fishes",
            "shelf and coastal fishes", "toothfishes")
WHALES <- c("baleen whales", "minke whales")
GRP <- list("All predators" = NULL, "Large baleen + minke whales" = WHALES,
            "All fishes" = FISHES)

theme_kc <- function(base = 10) theme_bw(base_size = base) + theme(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(colour = "grey93", linewidth = 0.3),
  strip.background = element_rect(fill = "grey96", colour = "grey75"),
  strip.text = element_text(face = "bold", size = base - 1),
  legend.position = "bottom", legend.key.size = unit(0.5, "cm"))

# events, as annotated on the published figure
EV <- data.frame(
  year = c(1930, meta$krill_active[1], meta$krill_active[2], meta$peak_year),
  lab  = c("Whaling starts", "Krill fishing starts", "End of krill fishing",
           "Peak krill fishing"),
  kind = c("solid", "solid", "solid", "dotted"))

# =============================================================================
# 1. krill consumption ratio, by predator group and scenario
# =============================================================================
grp_sum <- function(df, sps) {
  d <- if (is.null(sps)) df else df[df$Species %in% sps, ]
  keys <- if (ENSEMBLE) c("sim_index", "arm", "Year") else c("arm", "Year")
  d %>% group_by(across(all_of(keys))) %>%
    summarise(cons = sum(krill_consumed), .groups = "drop")
}
RAT <- do.call(rbind, lapply(names(GRP), function(g) {
  s <- grp_sum(KR, GRP[[g]])
  den <- s %>% filter(arm == "unexploited") %>%
    select(-arm) %>% rename(cons_un = cons)
  join_by <- if (ENSEMBLE) c("sim_index", "Year") else "Year"
  s %>% filter(arm != "unexploited") %>%
    left_join(den, by = join_by) %>%
    mutate(ratio = cons / cons_un, group = g)
}))
RAT$group <- factor(RAT$group, levels = names(GRP))
RAT$arm <- factor(RAT$arm, levels = names(ARM_LAB))

p1 <- ggplot(RAT, aes(Year, ratio, colour = arm))
if (ENSEMBLE) {
  band <- RAT %>% group_by(group, arm, Year) %>%
    summarise(lo = quantile(ratio, 0.1), md = median(ratio),
              hi = quantile(ratio, 0.9), .groups = "drop")
  p1 <- ggplot(band, aes(Year, md, colour = arm)) +
    geom_ribbon(aes(ymin = lo, ymax = hi, fill = arm), alpha = 0.18,
                colour = NA) +
    scale_fill_manual(values = ARM_COL, labels = ARM_LAB, name = NULL)
}
p1 <- p1 +
  geom_hline(yintercept = 1, colour = "grey55", linetype = "dashed",
             linewidth = 0.4) +
  geom_vline(data = EV, aes(xintercept = year, linetype = kind),
             colour = "grey45", linewidth = 0.4, show.legend = FALSE) +
  # Arms can coincide to the width of the line -- for whale biomass they very
  # nearly do. Draw them at decreasing width so an overlap is visible as a
  # halo rather than silently hiding every arm but the last.
  geom_line(aes(linewidth = arm)) +
  scale_linewidth_manual(values = c(exploited = 2.2, no_krill = 1.2,
                                    peak_krill = 0.6), guide = "none") +
  facet_wrap(~group, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = ARM_COL, labels = ARM_LAB, name = NULL) +
  scale_linetype_identity() +
  scale_x_continuous(breaks = seq(1900, 2010, 10)) +
  theme_kc() +
  labs(x = "Year",
       y = "Scenario / Unexploited Antarctic krill consumption (ratio)",
       subtitle = paste0(
         "Every arm divided by the SAME unexploited run, so the ratio removes",
         " the shared climate forcing.\nAll three arms carry the observed",
         " whaling history -- the krill fishery's effect is the SEPARATION",
         " between\nthe lines, not the mid-century fall, which is whaling."))
sv(p1, "KC_krill_consumption_ratio", 10, 11)

# =============================================================================
# 2. whale biomass -- the recovery question
# =============================================================================
wb <- BIO %>% filter(Species %in% WHALES)
keys <- if (ENSEMBLE) c("sim_index", "arm", "Year", "Species") else
  c("arm", "Year", "Species")
den <- wb %>% filter(arm == "unexploited") %>% select(-arm) %>%
  rename(b_un = biomass_g)
jb <- setdiff(keys, "arm")
WB <- wb %>% filter(arm != "unexploited") %>% left_join(den, by = jb) %>%
  mutate(rel = biomass_g / b_un,
         arm = factor(arm, levels = names(ARM_LAB)),
         Species = factor(Species, levels = WHALES,
                          labels = c("Large baleen whales", "Minke whales")))

p2 <- ggplot(WB, aes(Year, rel, colour = arm)) +
  geom_hline(yintercept = 1, colour = "grey55", linetype = "dashed",
             linewidth = 0.4) +
  geom_vline(data = EV, aes(xintercept = year, linetype = kind),
             colour = "grey45", linewidth = 0.4, show.legend = FALSE) +
  # Arms can coincide to the width of the line -- for whale biomass they very
  # nearly do. Draw them at decreasing width so an overlap is visible as a
  # halo rather than silently hiding every arm but the last.
  geom_line(aes(linewidth = arm)) +
  scale_linewidth_manual(values = c(exploited = 2.2, no_krill = 1.2,
                                    peak_krill = 0.6), guide = "none") +
  facet_wrap(~Species, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = ARM_COL, labels = ARM_LAB, name = NULL) +
  scale_linetype_identity() +
  scale_x_continuous(breaks = seq(1900, 2010, 10)) +
  theme_kc() +
  labs(x = "Year", y = "Scenario / Unexploited whale biomass (ratio)",
       subtitle = paste("Whale depletion is whaling in all three arms. Any",
                        "vertical gap between the lines after 1974 is what",
                        "krill\nfishing did to whale recovery."))
sv(p2, "KC_whale_biomass_ratio", 10, 8)

# =============================================================================
# 3. krill itself: standing stock and what the fishery removed
# =============================================================================
kb <- BIO %>% filter(Species == "antarctic krill")
den_k <- kb %>% filter(arm == "unexploited") %>%
  select(-arm, -Species) %>% rename(b_un = biomass_g)
jk <- if (ENSEMBLE) c("sim_index", "Year") else "Year"
KB <- kb %>% filter(arm != "unexploited") %>% left_join(den_k, by = jk) %>%
  mutate(rel = biomass_g / b_un, arm = factor(arm, levels = names(ARM_LAB)))

p3a <- ggplot(KB, aes(Year, rel, colour = arm)) +
  geom_hline(yintercept = 1, colour = "grey55", linetype = "dashed",
             linewidth = 0.4) +
  geom_vline(data = EV, aes(xintercept = year, linetype = kind),
             colour = "grey45", linewidth = 0.4, show.legend = FALSE) +
  # Arms can coincide to the width of the line -- for whale biomass they very
  # nearly do. Draw them at decreasing width so an overlap is visible as a
  # halo rather than silently hiding every arm but the last.
  geom_line(aes(linewidth = arm)) +
  scale_linewidth_manual(values = c(exploited = 2.2, no_krill = 1.2,
                                    peak_krill = 0.6), guide = "none") +
  scale_colour_manual(values = ARM_COL, labels = ARM_LAB, name = NULL) +
  scale_linetype_identity() +
  scale_x_continuous(breaks = seq(1900, 2010, 10)) +
  theme_kc() +
  labs(x = NULL, y = "Krill biomass / unexploited",
       title = "Antarctic krill standing stock")

yk <- YLD %>% filter(arm != "unexploited") %>%
  mutate(arm = factor(arm, levels = names(ARM_LAB)))
if (ENSEMBLE)
  yk <- yk %>% group_by(arm, Year) %>%
    summarise(krill_yield_g = median(krill_yield_g), .groups = "drop")
p3b <- ggplot(yk, aes(Year, krill_yield_g / 1e6, colour = arm)) +
  # Arms can coincide to the width of the line -- for whale biomass they very
  # nearly do. Draw them at decreasing width so an overlap is visible as a
  # halo rather than silently hiding every arm but the last.
  geom_line(aes(linewidth = arm)) +
  scale_linewidth_manual(values = c(exploited = 2.2, no_krill = 1.2,
                                    peak_krill = 0.6), guide = "none") +
  scale_colour_manual(values = ARM_COL, labels = ARM_LAB, name = NULL) +
  scale_x_continuous(breaks = seq(1900, 2010, 10)) +
  theme_kc() +
  labs(x = "Year", y = "Krill catch (t / yr)",
       title = "What each scenario actually removed")
sv(p3a / p3b + plot_layout(guides = "collect") &
     theme(legend.position = "bottom"), "KC_krill_stock_and_catch", 10, 8)

# =============================================================================
# 4. numbers for the text
# =============================================================================
# ACROSS MEMBERS, USE THE MEDIAN, NOT THE MEAN. The consumption ratio has a long
# right tail -- a member whose unexploited arm runs near zero produces an
# enormous ratio, and the across-member mean follows it. On this ensemble the
# mean puts All fishes at 3.86 against a median of 1.02. The figures above
# already use medians; these tables must agree with them, and with KC03.
ctr <- function(x) median(x, na.rm = TRUE)

cat("\n=== krill consumption ratio, decadal MEDIANS across members ===\n")
dec <- RAT %>% mutate(decade = 10 * (Year %/% 10)) %>%
  filter(decade >= 1970) %>%
  group_by(group, arm, decade) %>%
  summarise(ratio = ctr(ratio), .groups = "drop") %>%
  pivot_wider(names_from = decade, values_from = ratio)
print(as.data.frame(dec %>% mutate(across(where(is.numeric), ~round(.x, 3)))),
      row.names = FALSE)

cat("\n=== 2001-2010, median across members then mean over years ===\n")
ref <- RAT %>% filter(Year %in% 2001:2010) %>%
  group_by(group, arm, Year) %>% summarise(r = ctr(ratio), .groups = "drop") %>%
  group_by(group, arm) %>% summarise(ratio = mean(r), .groups = "drop") %>%
  pivot_wider(names_from = arm, values_from = ratio) %>%
  mutate(krill_fishery_effect = exploited - no_krill,
         peak_vs_observed = peak_krill - exploited)
print(as.data.frame(ref %>% mutate(across(where(is.numeric), ~round(.x, 4)))),
      row.names = FALSE)

cat("\n=== whale biomass 2001-2010, scenario / unexploited (median) ===\n")
wref <- WB %>% filter(Year %in% 2001:2010) %>%
  group_by(Species, arm, Year) %>% summarise(r = ctr(rel), .groups = "drop") %>%
  group_by(Species, arm) %>% summarise(rel = mean(r), .groups = "drop") %>%
  pivot_wider(names_from = arm, values_from = rel) %>%
  mutate(krill_fishery_effect = exploited - no_krill,
         peak_vs_observed = peak_krill - exploited)
print(as.data.frame(wref %>% mutate(across(where(is.numeric), ~round(.x, 4)))),
      row.names = FALSE)

cat("\n=== cumulative krill catch per member (t) ===\n")
# summing across members would just multiply by the member count
print(as.data.frame(YLD %>%
  group_by(across(any_of(c("sim_index", "arm")))) %>%
  summarise(total_t = sum(krill_yield_g) / 1e6, .groups = "drop") %>%
  group_by(arm) %>%
  summarise(median_t = signif(ctr(total_t), 5),
            min_t = signif(min(total_t), 5),
            max_t = signif(max(total_t), 5), .groups = "drop")),
  row.names = FALSE)

write.csv(RAT, file.path(ANA, "KC01_krill_ratio.csv"), row.names = FALSE)
write.csv(WB,  file.path(ANA, "KC01_whale_biomass_ratio.csv"), row.names = FALSE)
cat("\nKC01 complete.\n")
