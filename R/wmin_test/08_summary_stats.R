# Extract the exact figures quoted in docs/small_divers_wmin_stage0_results.md.
suppressPackageStartupMessages(library(dplyr))

pm <- readRDS("Output_large_files/wmin_test/05_stage0_per_member.rds")

f <- function(x) sprintf("%.4g [%.4g, %.4g]  (min %.4g, max %.4g)",
                         median(x), quantile(x, .25), quantile(x, .75),
                         min(x), max(x))

cat("=== per-member, 2001-2010 mean, n =", sum(pm$arm == "Exploited"),
    "per arm ===\n")
for (a in unique(pm$arm)) {
  d <- pm[pm$arm == a, ]
  cat("\n--- ", a, " ---\n", sep = "")
  for (k in c("f_biomass_sub", "f_number_sub", "f_cons_krill_sub",
              "f_cons_fish_sub", "pen_share_krill", "subpen_share_krill",
              "subpen_share_fish", "f_mort_number_sub", "f_mort_biomass_sub",
              "pen_vs_baleen_krill"))
    cat(sprintf("  %-22s %s\n", k, f(d[[k]])))
}

cat("\n=== exceedance counts (n of 212) ===\n")
for (a in unique(pm$arm)) {
  d <- pm[pm$arm == a, ]
  cat("--- ", a, " ---\n", sep = "")
  cat("  sub-threshold penguins > 0.1% of community krill consumption:",
      sum(d$subpen_share_krill > 0.001), "\n")
  cat("  sub-threshold penguins > 1%   of community krill consumption:",
      sum(d$subpen_share_krill > 0.01), "\n")
  cat("  sub-threshold penguins > 1%   of community fish  consumption:",
      sum(d$subpen_share_fish > 0.01), "\n")
  cat("  penguin sub-threshold BIOMASS share > 5%:",
      sum(d$f_biomass_sub > 0.05), "\n")
  cat("  penguin sub-threshold ABUNDANCE share > 25%:",
      sum(d$f_number_sub > 0.25), "\n")
}

# exploited vs unexploited paired difference in the shares themselves
w <- pm %>% select(arm, member, f_number_sub, f_biomass_sub, subpen_share_krill) %>%
  tidyr::pivot_wider(names_from = arm,
                     values_from = c(f_number_sub, f_biomass_sub, subpen_share_krill))
cat("\n=== paired Exploited - Unexploited difference in the shares ===\n")
cat("  f_number_sub  median diff:",
    signif(median(w$f_number_sub_Exploited - w$f_number_sub_Unexploited), 4), "\n")
cat("  f_biomass_sub median diff:",
    signif(median(w$f_biomass_sub_Exploited - w$f_biomass_sub_Unexploited), 4), "\n")
