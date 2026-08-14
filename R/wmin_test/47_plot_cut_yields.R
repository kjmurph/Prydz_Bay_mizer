# =============================================================================
# PHASE 6b -- plot the modelled yield of each candidate selection cut, in the two
# manuscript figure styles, so the cuts can be compared visually.
#
# Styling is taken from the existing figures rather than reinvented:
#   Figure 1 (stacked area + observed inset + IQR bars at peaks)
#       yield_stacked_pct_variants.R  -- species_order, sp_palette, kt_formatter,
#       find_peaks(), add_bars(), plot_scales(), the inset construction
#   Figure 2 (per-species facets, log y, ribbons, observed points, era vlines)
#       create_biomass_yield_figures_for_publication.R:161-190
#
# Figure 2 uses a pseudo-log y axis rather than scale_y_log10: the observed series
# contain true zeros in the unfished years, which log10 silently drops. sigma is
# set below the smallest non-zero observation so the compression is invisible in
# the plotted range.
#
# INPUT is the cut memberships from 46_selection_cuts.R plus the cached
# post-spin-up states, re-projected under the fitted catchability multipliers
# (45_catchability_multipliers.rds). Re-projection is exact -- the spin-up is
# unfished so catchability enters only the projection.
#
# USAGE   Rscript R/wmin_test/47_plot_cut_yields.R
# ENV     PLOT_CORES (14)
# OUT     Output_large_files/wmin_test/47_cut_yields.rds
#         Manuscript figures/Supplemental figures/yield_stacked_<cut>.png
#         Manuscript figures/Supplemental figures/yield_facets_<cut>.png
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel)
  library(tidyverse); library(reshape2); library(scales); library(patchwork)
})

out_dir   <- "Output_large_files/wmin_test"
state_dir <- file.path(out_dir, "44_states")
# The repo already has "Manuscript figures/Supplemental figures"; use it rather
# than create a near-duplicate "Supplement figures" beside it.
fig_dir   <- file.path("Manuscript figures", "Supplemental figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
CORES <- as.integer(Sys.getenv("PLOT_CORES", "14"))
PLOT_YEARS  <- 1900:2010
PEAK_WINDOW <- 3
MIN_PEAK_KT <- 10
# getYield() returns GRAMS, and yield_observed_timeseries.csv is in grams too
# (44/45 compare them directly). Both axis formatters below label tonnes, so
# convert once, here, for modelled and observed alike. Without this the axis
# reads "1,500,000,000 kt" instead of "600 kt".
G_TO_T <- 1e6

species_order <- c(
  "mesozooplankton", "other krill", "other macrozooplankton",
  "antarctic krill", "salps", "mesopelagic fishes", "bathypelagic fishes",
  "shelf and coastal fishes", "flying birds", "small divers", "squids",
  "toothfishes", "leopard seals", "medium divers", "large divers",
  "minke whales", "orca", "sperm whales", "baleen whales")

CUTS <- readRDS(file.path(out_dir, "46_selection_cuts.rds"))$cuts
M    <- readRDS(file.path(out_dir, "45_catchability_multipliers.rds"))$M
effort_arr <- readRDS("effort_array_1841_2010.rds")

# Cut A is the manuscript's selection rule, and its facet plot is a supplementary
# FIGURE rather than a diagnostic: the caption identifies it, so it is drawn with
# no on-plot subtitle. B/C/D keep theirs -- they exist to be compared against each
# other, and without the subtitle the filename would be the only thing telling
# them apart.
MANUSCRIPT_CUT <- c("A unweighted RMSE", "E NRMSE_sd")

# The balanced NRMSE_sd cut (R/wmin_test/49_rank_nrmse_sd.R) is appended as a
# fifth cut so its facets are drawn by the same code, with the same styling, as
# the four it is meant to be compared against. Its yields are in their own cache
# (50_cut_yields_nrmse167.rds) because it shares only 38 of 167 members with
# cut A and so is barely covered by 47_cut_yields.rds.
nr_sel <- file.path(out_dir, "49_rank_nrmse_sd.rds")
if (file.exists(nr_sel)) {
  CUTS[["E NRMSE_sd"]] <- readRDS(nr_sel)$selected
  message("appended cut E (NRMSE_sd): ", length(CUTS[["E NRMSE_sd"]]), " members")
}

# --- observed yield, and the effort windows that gate it ---------------------
obs_raw <- read.csv("yield_observed_timeseries.csv")
obs_stack <- obs_raw %>%
  reshape2::melt(id.vars = "Year", variable.name = "Species", value.name = "Yield_t") %>%
  mutate(Species = gsub("\\.", " ", as.character(Species)),
         Yield_t = pmax(coalesce(as.numeric(Yield_t), 0), 0) / G_TO_T) %>%
  filter(Year %in% PLOT_YEARS) %>%
  mutate(Species = factor(Species, levels = species_order))
effort_windows <- do.call(rbind, lapply(colnames(effort_arr), function(s) {
  y <- as.numeric(rownames(effort_arr))[effort_arr[, s] > 0]
  if (length(y)) data.frame(Species = s, first_year = min(y), last_year = max(y),
                            stringsAsFactors = FALSE)
}))
fished_species <- intersect(species_order,
                            as.character(unique(obs_stack$Species[obs_stack$Yield_t > 0])))
obs_stack <- obs_stack %>% filter(Species %in% fished_species) %>%
  mutate(Species = factor(as.character(Species), levels = fished_species))

# Canonical manuscript colours, NOT scales::hue_pal(). The palette used to be
# hue_pal() over the CSV column order, which happened to land close to the
# manuscript scheme for the whales but put squids on a green and bathypelagic
# fishes on an orange that belong to other groups. These are the values used by
# F01_figure1_rebuilt167.R (`sp_cols`) and F05_supp_biomass_grid_rebuilt167.R
# (`panel_colors`), so the yield facets now key to the biomass grid.
sp_cols <- c(
  "antarctic krill"          = "#F8766D", "bathypelagic fishes" = "#B07A00",
  "shelf and coastal fishes" = "#93AA00", "squids"              = "#E8B33C",
  "toothfishes"              = "#00C19F", "minke whales"        = "#00B9E3",
  "orca"                     = "#619CFF", "sperm whales"        = "#DB72FB",
  "baleen whales"            = "#FF61C3", "mesopelagic fishes"  = "#D39200",
  "leopard seals"            = "#E07B39", "medium divers"       = "#2B6CB0",
  "large divers"             = "#4C8FD0", "flying birds"        = "#9E9E9E",
  "small divers"             = "#BDBDBD", "mesozooplankton"     = "#6A1B9A",
  "other krill"              = "#8E44AD", "other macrozooplankton" = "#9B59B6",
  "salps"                    = "#C39BD3")
missing_col <- setdiff(fished_species, names(sp_cols))
if (length(missing_col)) stop("no manuscript colour for: ",
                              paste(missing_col, collapse = ", "))
sp_palette   <- sp_cols[fished_species]
kt_formatter <- function(x) ifelse(x == 0, "0", paste0(comma(x / 1e3), " kt"))

find_peaks <- function(total_t, years, window = PEAK_WINDOW,
                       min_t = MIN_PEAK_KT * 1000) {
  n <- length(total_t); is_peak <- logical(n)
  for (i in seq_len(n)) {
    lo <- max(1L, i - window); hi <- min(n, i + window)
    if (total_t[i] == max(total_t[lo:hi]) && total_t[i] >= min_t) is_peak[i] <- TRUE
  }
  years[is_peak]
}

# --- re-project the union of the cuts, keeping per-species per-year yield -----
members <- sort(unique(unlist(CUTS)))
cache <- file.path(out_dir, "47_cut_yields.rds")
extra <- file.path(out_dir, "50_cut_yields_nrmse167.rds")
if (file.exists(cache)) {
  Y <- readRDS(cache)
  # Top up from the NRMSE_sd extraction rather than re-projecting: both were
  # produced from the same states under the same re-fitted catchability, so the
  # union is consistent. Members present in both keep the original rows.
  if (file.exists(extra)) {
    E <- readRDS(extra)
    Y <- bind_rows(Y, E %>% filter(!sim_index %in% unique(Y$sim_index)))
  }
  message("loaded cached yields for ", length(unique(Y$sim_index)), " members")
} else {
  message("projecting ", length(members), " members on ", CORES, " cores...")
  get_yield <- function(si, M, qmax = 1) {
    suppressPackageStartupMessages({
      library(therMizer); library(mizer); library(reshape2) })
    z <- readRDS(file.path(SDIR, sprintf("state_treated_%05d.rds", si)))
    p <- z$params
    gp <- gear_params(p)
    m <- M[match(gp$species, names(M))]; m[is.na(m)] <- 1
    gp$catchability <- pmin(qmax, pmax(0, gp$catchability * m))
    gear_params(p) <- gp
    pr <- try(project(p, initial_n = z$initial_n, t_start = 1841, effort = EFF),
              silent = TRUE)
    if (inherits(pr, "try-error")) return(NULL)
    d <- reshape2::melt(getYield(pr))
    names(d) <- c("Year", "Species", "Yield_t")
    d$Year <- as.numeric(as.character(d$Year))
    d$Species <- as.character(d$Species)
    d$Yield_t <- pmax(d$Yield_t, 0)
    d$sim_index <- si
    d[d$Year %in% PLOT_YEARS, ]
  }
  cl <- makeCluster(CORES)
  assign("EFF", effort_arr, envir = environment())
  assign("SDIR", state_dir, envir = environment())
  assign("PLOT_YEARS", PLOT_YEARS, envir = environment())
  clusterExport(cl, c("EFF", "SDIR", "PLOT_YEARS", "get_yield"), envir = environment())
  # M passed as an argument, never captured from the closure -- parLapply does not
  # ship globals (this bit us in 45_catchability_refit.R).
  r <- parLapplyLB(cl, members, function(si, MM, QQ)
    tryCatch(get_yield(si, MM, QQ), error = function(e) NULL), MM = M, QQ = 1)
  stopCluster(cl)
  r <- r[!vapply(r, is.null, logical(1))]
  Y <- bind_rows(r)
  saveRDS(Y, cache)
  message("  projected ", length(r), " members")
}

# grams -> tonnes (see G_TO_T), and yields outside a species' effort window are
# zero by construction
Y <- Y %>% mutate(Yield_t = Yield_t / G_TO_T) %>%
  left_join(effort_windows, by = "Species") %>%
  mutate(Yield_t = ifelse(is.na(first_year) | Year < first_year | Year > last_year,
                          0, Yield_t)) %>%
  filter(Species %in% fished_species) %>%
  mutate(Species = factor(Species, levels = fished_species))

label_dynamic_decimals <- function(x) {
  sci <- scales::label_scientific(digits = 1)
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (v <= 0) return("0")
    if (v >= 1) paste0(formatC(v, format = "f", digits = 0, big.mark = ","), " t")
    else if (v < 1e-6) paste0(sci(v), " t")
    else paste0(formatC(v, format = "f", digits = min(max(1, ceiling(-log10(v))), 6)), " t")
  }, character(1))
}

p_obs_inset <- ggplot(obs_stack, aes(x = Year, y = Yield_t, fill = Species)) +
  geom_area(position = "stack", alpha = 0.85, colour = NA) +
  scale_fill_manual(values = sp_palette) +
  scale_x_continuous(breaks = seq(1900, 2010, by = 10),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = kt_formatter, expand = expansion(mult = c(0, 0.05))) +
  theme_bw(base_size = 13) + labs(title = "Observed yield") +
  theme(legend.position = "none", axis.title = element_blank(),
        axis.text = element_text(size = 6.5),
        axis.text.x = element_text(angle = 40, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(size = 8, face = "bold", margin = margin(b = 2)),
        plot.margin = margin(2, 2, 2, 2, "pt"),
        plot.background = element_rect(fill = "white", colour = "grey70", linewidth = 0.4))

# --------------------------------------------------------------- per cut ----
summ <- list()
for (nm in names(CUTS)) {
  sel <- CUTS[[nm]]
  tag <- gsub("[^A-Za-z0-9]+", "_", nm)
  d <- Y %>% filter(sim_index %in% sel)
  if (!nrow(d)) { message("no yields for ", nm); next }
  message(sprintf("\n--- %s (n = %d members) ---", nm, length(unique(d$sim_index))))

  stack_df <- d %>% group_by(Year, Species) %>%
    summarise(median_t = median(Yield_t, na.rm = TRUE), .groups = "drop")
  total_ps <- d %>% group_by(sim_index, Year) %>%
    summarise(total_t = sum(Yield_t, na.rm = TRUE), .groups = "drop")

  totals <- stack_df %>% group_by(Year) %>%
    summarise(centre = sum(median_t, na.rm = TRUE), .groups = "drop") %>% arrange(Year)
  pk  <- find_peaks(totals$centre, totals$Year)
  ci  <- total_ps %>% filter(Year %in% pk) %>% group_by(Year) %>%
    summarise(lo25 = quantile(total_t, .25, na.rm = TRUE),
              hi75 = quantile(total_t, .75, na.rm = TRUE), .groups = "drop")
  ci_df <- totals %>% filter(Year %in% pk) %>% left_join(ci, by = "Year")

  p1 <- ggplot(stack_df, aes(x = Year, y = median_t, fill = Species)) +
    geom_area(position = "stack", alpha = 0.85, colour = NA) +
    geom_errorbar(data = ci_df, aes(x = Year, ymin = lo25, ymax = hi75),
                  inherit.aes = FALSE, width = 1.2, linewidth = 0.6, colour = "white") +
    geom_errorbar(data = ci_df, aes(x = Year, ymin = lo25, ymax = hi75),
                  inherit.aes = FALSE, width = 1.0, linewidth = 0.4, colour = "grey20") +
    geom_point(data = ci_df, aes(x = Year, y = centre), inherit.aes = FALSE,
               shape = 21, size = 2, fill = "white", colour = "grey20", stroke = 0.5) +
    scale_fill_manual(values = sp_palette) +
    scale_x_continuous(breaks = seq(1900, 2010, by = 10),
                       expand = expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(labels = kt_formatter, expand = expansion(mult = c(0, 0.05))) +
    theme_bw(base_size = 13) +
    theme(legend.position = "right", axis.text.x = element_text(angle = 40, hjust = 1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(x = "Year", y = expression(Yield~(10^3~t~y^{-1})), fill = "Species",
         subtitle = sprintf("%s  (n = %d)", nm, length(sel))) +
    inset_element(p_obs_inset, left = .52, bottom = .55, right = .99, top = .99)
  ggsave(file.path(fig_dir, sprintf("yield_stacked_%s.png", tag)), p1,
         width = 12, height = 6, dpi = 200)

  # ---- figure 2: per-species facets ----
  rib <- d %>% group_by(Year, Species) %>%
    summarise(q05 = quantile(Yield_t, .05, na.rm = TRUE),
              q25 = quantile(Yield_t, .25, na.rm = TRUE),
              med = median(Yield_t, na.rm = TRUE),
              q75 = quantile(Yield_t, .75, na.rm = TRUE),
              q95 = quantile(Yield_t, .95, na.rm = TRUE), .groups = "drop")
  obs_pts <- obs_stack %>% filter(Species %in% fished_species)
  nz <- c(rib$med[rib$med > 0], obs_pts$Yield_t[obs_pts$Yield_t > 0])
  sigma <- max(min(nz, na.rm = TRUE) / 10, 1e-8)

  p2 <- ggplot() +
    geom_ribbon(data = rib, aes(Year, ymin = q05, ymax = q95, fill = Species), alpha = .2) +
    geom_ribbon(data = rib, aes(Year, ymin = q25, ymax = q75, fill = Species), alpha = .3) +
    geom_line(data = rib, aes(Year, med, colour = Species), linewidth = 1) +
    geom_point(data = obs_pts, aes(Year, Yield_t, colour = Species), size = 1.1) +
    geom_point(data = obs_pts, aes(Year, Yield_t), shape = 1, size = 1.1, colour = "black") +
    geom_vline(xintercept = c(1961, 2010), linetype = "dashed") +
    scale_colour_manual(values = sp_palette) + scale_fill_manual(values = sp_palette) +
    scale_y_continuous(trans = scales::pseudo_log_trans(sigma = sigma, base = 10),
                       labels = label_dynamic_decimals,
                       breaks = c(0, 1e-4, 1e-2, 1, 1e2, 1e4, 1e6)) +
    facet_wrap(~Species, scales = "free_y") +
    theme_bw(base_size = 12) +
    theme(legend.position = "none", strip.text = element_text(face = "bold")) +
    labs(x = "Year", y = expression(Yield~(t~y^{-1})),
         subtitle = if (nm %in% MANUSCRIPT_CUT) NULL
                    else sprintf("%s  (n = %d)", nm, length(sel)))
  ggsave(file.path(fig_dir, sprintf("yield_facets_%s.png", tag)), p2,
         width = 12, height = 6.5, dpi = 200)

  # MEDIAN of the per-member ratio, not the ratio of means: the per-member totals
  # are heavy-tailed, and a mean is dominated by a handful of members. On cut D
  # the mean puts toothfishes at 75.6x observed while the median is 0.39.
  obs_tot <- obs_stack %>% group_by(Species) %>%
    summarise(obs = sum(Yield_t), .groups = "drop") %>% filter(obs > 0)
  tot <- d %>% group_by(sim_index, Species) %>%
    summarise(mod = sum(Yield_t), .groups = "drop") %>%
    inner_join(obs_tot, by = "Species") %>%
    group_by(Species) %>%
    summarise(ratio = median(mod / obs), .groups = "drop")
  summ[[nm]] <- tot %>% mutate(cut = nm)
  cat("  modelled/observed total catch by species (median across members):\n")
  print(as.data.frame(tot %>% transmute(Species, ratio = signif(ratio, 3))), row.names = FALSE)
  cat(sprintf("  species within a factor of 2 of observed: %d of %d\n",
              sum(tot$ratio >= 0.5 & tot$ratio <= 2, na.rm = TRUE), nrow(tot)))
  cat("  wrote yield_stacked_", tag, ".png and yield_facets_", tag, ".png\n", sep = "")
}

saveRDS(bind_rows(summ), file.path(out_dir, "47_cut_yield_summary.rds"))
cat("\nFigures in", fig_dir, "\n")