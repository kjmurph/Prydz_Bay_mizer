# =============================================================================
# Shared member-set and band helpers for the phase-88 figure variants.
#
# WHY THIS EXISTS. The phase-88 data build carries ALL 427 usable members, and
# the figures are wanted two ways: the full usable set with an IQR and an outer
# percentile band, and the top 10% (43 members) with an IQR alone. Both are
# plot-time choices over the same per-member data, so no figure needs a second
# six-hour build -- it needs a filter and one extra ribbon.
#
# EVERY DEFAULT REPRODUCES THE PUBLISHED BEHAVIOUR. With no environment set,
# fig_members() keeps every member in the file and fig_outer() is FALSE, so a
# script that sources this and is then run unchanged draws exactly what it drew
# before. The new behaviour is opt-in, which is what makes it safe to patch the
# existing figure scripts rather than fork them.
#
#   FIG_SET    "all" (default) | "top"   -- which members to plot
#   FIG_OUTER  "0" (default) | "1"       -- draw the outer percentile band
#   FIG_OUTER_PROBS  "0.05,0.95"         -- what that band is
#
# THE OUTER BAND IS THE 5th-95th PERCENTILE by default, not a 95% CI. That
# matches F05_supp_biomass_grid, which already computes q05/q95, so the two
# figures mean the same thing by "the outer band". Set FIG_OUTER_PROBS to
# "0.025,0.975" for a central 95% interval instead.
# =============================================================================

# Which members to plot. `meta` is the F00 meta object; it carries meta$cuts$top
# for builds that have a selection, and older builds have no $cuts at all.
fig_members <- function(meta, set = Sys.getenv("FIG_SET", "all")) {
  set <- tolower(trimws(set))
  if (!set %in% c("all", "full", "top"))
    stop("FIG_SET must be 'all', 'full' or 'top', not '", set, "'", call. = FALSE)
  if (set %in% c("all", "full")) return(NULL)          # NULL = keep everything
  if (is.null(meta$cuts$top))
    stop("FIG_SET='top' but this data build carries no meta$cuts$top -- it was ",
         "built before the selection was attached. Rebuild with ",
         "F00_build_p88_data.R.", call. = FALSE)
  as.integer(meta$cuts$top)
}

# Apply it. Keeps the call site to one line and reports what it did, so a figure
# built from the wrong set is visible in the log rather than only in the pixels.
fig_filter <- function(d, keep, what = "rows") {
  if (is.null(keep)) return(d)
  n0 <- length(unique(d$sim_index))
  d <- d[d$sim_index %in% keep, , drop = FALSE]
  message("  FIG_SET=top: ", what, " filtered to ",
          length(unique(d$sim_index)), " of ", n0, " members")
  d
}

fig_outer <- function() identical(Sys.getenv("FIG_OUTER", "0"), "1")

fig_outer_probs <- function() {
  p <- as.numeric(trimws(strsplit(
    Sys.getenv("FIG_OUTER_PROBS", "0.05,0.95"), ",")[[1]]))
  if (length(p) != 2 || any(!is.finite(p)) || p[1] >= p[2] ||
      any(p < 0) || any(p > 1))
    stop("FIG_OUTER_PROBS must be two increasing probabilities in [0,1]",
         call. = FALSE)
  p
}

# A label for the caption / filename, so the band shown and the band named
# cannot drift apart.
fig_outer_label <- function() {
  p <- fig_outer_probs()
  sprintf("%g-%gth percentile", 100 * p[1], 100 * p[2])
}

fig_set_tag <- function(set = Sys.getenv("FIG_SET", "all")) {
  set <- tolower(trimws(set))
  if (set == "top") "top" else "full"
}
