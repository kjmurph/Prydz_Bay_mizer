# =============================================================================
# Two questions about the recalibrated (treated) runs:
#  1. Are penguins at a stable new low equilibrium, or still collapsing at 2010?
#  2. How many size bins does the w grid give each species? A species with
#     w_min = 3626.667 g and w_max = 6000 g spans 0.22 decades against a grid
#     resolution of 0.155 decades/bin, so the answer bears directly on whether
#     the corrected parameter is even representable on this grid.
# =============================================================================

suppressPackageStartupMessages({ library(therMizer); library(mizer) })

PEN <- "small divers"
res <- readRDS("Output_large_files/wmin_test/22_stage1_steady_sims.rds")
series <- readRDS("Output_large_files/wmin_test/23_series.rds")

r <- res[[1]]
p <- r$treated$params
w <- p@w; dw <- p@dw

cat("=== grid resolution ===\n")
cat("  100 bins over", signif(log10(max(w) / min(w)), 4), "decades =",
    signif(log10(max(w) / min(w)) / 99, 4), "decades per bin\n\n")
sp <- p@species_params
tab <- data.frame(species = sp$species, w_min = sp$w_min, w_max = sp$w_max,
                  decades = log10(sp$w_max / sp$w_min))
tab$n_bins <- vapply(seq_len(nrow(sp)), function(i)
  sum(w >= w[max(which(w <= sp$w_min[i]))] & w <= sp$w_max[i]), integer(1))
print(tab[order(tab$n_bins), ], digits = 5, row.names = FALSE)

cat("\n=== penguin biomass trajectory, treated vs control (member 1, fished) ===\n")
s <- series[series$member == 1 & series$arm == "fished", ]
for (y in c(1841, 1850, 1900, 1950, 1980, 2000, 2010)) {
  row <- s[s$year == y, ]
  cat(sprintf("  %d  control %.4e   treated %.4e   ratio %.5f\n", y,
              row$pen_biomass[row$cond == "control"],
              row$pen_biomass[row$cond == "treated"],
              row$pen_biomass[row$cond == "treated"] /
                row$pen_biomass[row$cond == "control"]))
}

cat("\n=== is the treated penguin population still declining? ===\n")
for (m in unique(series$member)) {
  d <- series[series$member == m & series$arm == "fished" &
                series$cond == "treated", ]
  d <- d[order(d$year), ]
  last30 <- d[d$year >= 1981, ]
  fit <- lm(log(pen_biomass) ~ year, data = last30)
  cat(sprintf("  member %d: log-slope over 1981-2010 = %+.5f /yr  (half-life %s)\n",
              m, coef(fit)[2],
              if (coef(fit)[2] < 0) sprintf("%.0f yr", log(2) / -coef(fit)[2]) else "n/a"))
}

cat("\n=== treated penguin size distribution at 2010 (member 1, fished) ===\n")
sim <- r$treated$fished
n <- sim@n[as.character(2010), PEN, ]
nz <- which(n > 0)
cat("  occupied bins:", paste(nz, collapse = " "), "\n")
cat("  w at those bins:", paste(signif(w[nz], 5), collapse = " "), "\n")
cat("  N per bin:", paste(signif(n[nz] * dw[nz], 4), collapse = " "), "\n")

cat("\n=== control penguin size distribution at 2010 for contrast ===\n")
nc <- r$control$fished@n[as.character(2010), PEN, ]
cat("  occupied bins:", min(which(nc > 0)), "-", max(which(nc > 0)),
    "(", sum(nc > 0), "bins )\n")
