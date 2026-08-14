# =============================================================================
# F00q -- filter a figure-data build to the members that PASSED the stability
# screen, and rewrite the products under a new suffix.
#
# WHY THIS IS NECESSARY, and it is not optional for the revised-kernel build.
# Figure 2's SNR and Figure 4's +-1 SD band are both built on the ACROSS-MEMBER
# MEAN of the unexploited trajectory. A mean is not robust, and under the revised
# whale kernel one member (2055) diverges catastrophically: its unexploited total
# biomass at 1841 is 5.64e16 g against a typical 7e13 -- 800x the ensemble norm --
# and it then collapses by a factor of 570 over the record (temporal CV 2.72
# against a typical 0.023). Measured on the unfiltered build, the three largest
# members hold 82.9% of the across-member sum where an equal share would be 1.8%.
#
# The consequence is that the SNR denominator is set almost entirely by that one
# member. On the community-biomass SNR, like for like:
#
#     all 164 members            SNR  0.055   (noise 5.86e13, inflated ~40x)
#     stable members only (158)  SNR  2.408   (noise 1.46e12)
#     temporal CV < 0.05  (162)  SNR  2.358   (noise 1.45e12)
#     published rebuilt167       SNR -0.520   (noise 1.45e12)
#
# The two filters agree to 2%, and both land the noise on the published build's
# value, so the result is not sensitive to which one is used -- it is sensitive
# only to whether the divergent members are excluded at all. This is the same
# trap recorded for the legacy consumption arrays: any across-member MEAN is set
# by a handful of divergent members; medians are nearly sound.
#
# `stable` is used rather than an ad-hoc CV cut because it is the ensemble
# pipeline's own criterion, recorded per member by 44/53 (CV of the last 40 yr
# > 0.25, or a significant trend in the first 50 yr).
#
# USAGE  Rscript "Manuscript scripts/F00q_filter_stable.R"
# ENV: F0Q_IN (default kernel164), F0Q_OUT (default kernel158),
#      F0Q_SUMMARY (default the 53 run summary carrying the `stable` flag)
# =============================================================================

suppressPackageStartupMessages(library(dplyr))
DATA <- "Manuscript data"
OUT_LARGE <- "Output_large_files/wmin_test"
IN  <- Sys.getenv("F0Q_IN",  "kernel164")
SUMMARY <- Sys.getenv("F0Q_SUMMARY",
                      file.path(OUT_LARGE, "53_bk05_mk05_n167_summary.csv"))

S <- read.csv(SUMMARY)
keep <- S$sim_index[S$stable]
OUT <- Sys.getenv("F0Q_OUT", sprintf("kernel%d", length(keep)))
cat("=== F00q: filtering '", IN, "' to stable members -> '", OUT, "' ===\n", sep = "")
cat("  ", nrow(S), " members in, ", length(keep), " stable, ",
    nrow(S) - length(keep), " dropped: ",
    paste(setdiff(S$sim_index, keep), collapse = ", "), "\n", sep = "")

guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite an existing file: ", f, call. = FALSE); f
}
pin  <- function(b) file.path(DATA, sprintf("%s_%s.rds", b, IN))
pout <- function(b) file.path(DATA, sprintf("%s_%s.rds", b, OUT))

for (b in c("biomass_abund_fish", "biomass_abund_clim", "nbss_slope",
            "krill_consumption", "krill_baseline_1841_unexploited")) {
  d <- readRDS(pin(b))
  n0 <- length(unique(d$sim_index))
  d2 <- d %>% filter(sim_index %in% keep)
  saveRDS(d2, guard(pout(b)))
  cat(sprintf("  %-34s %7d -> %7d rows | members %3d -> %3d\n", b, nrow(d), nrow(d2),
              n0, length(unique(d2$sim_index))))
}

meta <- readRDS(pin("meta"))
meta$n_members <- length(keep)
meta$members   <- keep
meta$dropped_unstable <- setdiff(S$sim_index, keep)
meta$cut <- paste0(meta$cut, " | filtered to members passing the stability screen")
meta$filtered_by <- "44/53 stability screen (`stable` flag)"
meta$filtered_on <- Sys.time()
saveRDS(meta, guard(pout("meta")))
cat("  meta updated: n_members", meta$n_members, "\n\nF00q complete.\n")
