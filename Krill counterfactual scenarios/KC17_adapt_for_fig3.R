# =============================================================================
# KC17 -- reshape the KC14 phase-56 ensemble into the three files Figure 3 reads
#
# F03_figure3_pctchange_rebuilt167.R is already parameterised by FIG_SUF, and
# takes exactly three inputs from Manuscript data/:
#
#   biomass_abund_fish_<SUF>.rds   the EXPLOITED arm   (sim_index, Year,
#   biomass_abund_clim_<SUF>.rds   the UNEXPLOITED arm  Species, Abundance,
#   meta_<SUF>.rds                 n_members, cut       Biomass)
#
# So Figure 3 needs no editing at all -- only a translation of KC14's long
# `biomass` table into that shape. Writing the adapter rather than a second copy
# of F03 keeps one figure implementation, which matters because F03's paired
# construction and its +-1 SD band are the parts that must not drift.
#
# WHICH ARM IS WHICH. "fish" is the fished/exploited arm and "clim" the
# climate-only unexploited arm, matching the published naming. KC14's arms are
# named `exploited` and `unexploited`; the mapping is asserted below rather than
# assumed, because silently swapping them would flip every sign in the figure
# and still produce a plausible-looking plot.
#
# STABLE MEMBERS ONLY, filtered before anything is written -- F03 forms
# across-member means for its noise band, and one divergent member can own that
# statistic.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC17_adapt_for_fig3.R"
#        then   FIG_SUF=whres000 Rscript "Manuscript scripts/F03_figure3_pctchange_rebuilt167.R"
# ENV    KC17_IN, KC17_SUF (default whres000), KC17_FORCE
# =============================================================================

suppressPackageStartupMessages({library(dplyr)})

OUT_LARGE <- "Output_large_files/wmin_test"
DATA <- "Manuscript data"
dir.create(DATA, showWarnings = FALSE)
IN <- Sys.getenv("KC17_IN", file.path(OUT_LARGE, "KC14_whres000_n167.rds"))
SUF <- Sys.getenv("KC17_SUF", "whres000")
FORCE <- nzchar(Sys.getenv("KC17_FORCE"))
if (!file.exists(IN))
  stop("missing ", IN, " -- run KC14_ensemble_whres000.R first", call. = FALSE)

D <- readRDS(IN); meta <- D$meta
cat("=== KC17: reshaping KC14 for Figure 3 ===\n")
cat("input:", basename(IN), "| base:", basename(meta$base), "| suffix:", SUF, "\n")

B <- D$biomass
if (!all(c("exploited", "unexploited") %in% unique(B$arm)))
  stop("KC14 output is missing the exploited/unexploited arms", call. = FALSE)
keep <- D$members$sim_index[D$members$stable]
cat("members:", nrow(D$members), "| stable:", length(keep), "\n")
if (!length(keep)) stop("no stable members", call. = FALSE)
B <- B[B$sim_index %in% keep, ]

cols <- c("sim_index", "Year", "Species", "Abundance", "Biomass")
bf <- B[B$arm == "exploited",   cols]     # "fish" = fished
bc <- B[B$arm == "unexploited", cols]     # "clim" = climate only

# The arms must not be swapped. Whaling is in the effort array, so by 2010 the
# exploited arm MUST hold less baleen whale biomass than the unexploited one.
# This is the cheapest possible check that the mapping is the right way round.
chk <- function(d, y) median(d$Biomass[d$Species == "baleen whales" &
                                       d$Year == y], na.rm = TRUE)
bf10 <- chk(bf, 2010); bc10 <- chk(bc, 2010)
cat(sprintf("baleen whale biomass at 2010: exploited %.4g, unexploited %.4g (ratio %.3f)\n",
            bf10, bc10, bf10 / bc10))
if (!(bf10 < bc10))
  stop("the exploited arm does not hold less baleen whale biomass than the ",
       "unexploited arm at 2010 -- the arm mapping is wrong.", call. = FALSE)

stopifnot(nrow(bf) == nrow(bc), identical(sort(unique(bf$sim_index)),
                                          sort(unique(bc$sim_index))))
cat("rows per arm:", nrow(bf), "| species:", length(unique(bf$Species)),
    "| years:", min(bf$Year), "-", max(bf$Year), "\n")

MET <- list(n_members = length(keep), cut = paste0("KC14 phase-56 (", SUF, ")"),
            base = meta$base, source = basename(IN), built = format(Sys.time()))

out <- c(fish = file.path(DATA, sprintf("biomass_abund_fish_%s.rds", SUF)),
         clim = file.path(DATA, sprintf("biomass_abund_clim_%s.rds", SUF)),
         meta = file.path(DATA, sprintf("meta_%s.rds", SUF)))
for (f in out) if (file.exists(f) && !FORCE)
  stop("refusing to overwrite: ", f, "\n  set KC17_FORCE=1", call. = FALSE)
saveRDS(bf, out[["fish"]]); saveRDS(bc, out[["clim"]]); saveRDS(MET, out[["meta"]])
cat("\nwrote:\n"); for (f in out) cat("  ", f, "\n")
cat("\nnow run:  FIG_SUF=", SUF,
    " Rscript \"Manuscript scripts/F03_figure3_pctchange_rebuilt167.R\"\n",
    sep = "")
cat("KC17 complete.\n")