# =============================================================================
# STAGE A -- audit every group's size parameters against the empirical source,
# and detect any OTHER silent mizer clamp.
#
# WHY. `small divers` w_min was never authored as 0.001 g.
# mizer:::validGivenSpeciesParams rewrites w_min to pmin(0.001, w_mat/10)
# whenever w_min >= w_mat, and only warns. That warning was emitted at
# newMultispeciesParams() time and missed. This script asks: did it happen to
# anything else, and does anything else disagree with its source?
#
# Scope agreed with Kieran: audit ALL groups, change only `small divers`.
#
# Compares three layers:
#   source   group params/trait_groups_params_vCWC_v4.csv  (the empirical table)
#   built    params/base_params.rds                        (first post-mizer object)
#   ensemble Manuscript data/params_sel_adj.rds             (what the MC actually ran)
#
# Writes docs/size_parameter_audit.md + Output_large_files/wmin_test/28_audit.csv
# =============================================================================

suppressPackageStartupMessages({
  library(mizer)
  library(dplyr)
})

out_dir <- "Output_large_files/wmin_test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("docs", showWarnings = FALSE)

CLAMP <- 0.001   # mizer's hard-coded fallback

cat("=== Stage A: size parameter audit ===\n")
cat("mizer", as.character(packageVersion("mizer")), "\n\n")

# --- layer 1: the empirical source table -------------------------------------
src <- read.csv("group params/trait_groups_params_vCWC_v4.csv",
                stringsAsFactors = FALSE)
names(src)[1] <- "row"
# v4 names the max-size column w_inf; earlier versions call it w_max
if ("w_inf" %in% names(src)) src$w_max <- src$w_inf
src <- src %>% select(species, w_min, w_mat, w_max)

# --- layer 3: what the ensemble actually ran ---------------------------------
ens <- readRDS("Manuscript data/params_sel_adj.rds")
esp <- ens@species_params %>% select(species, w_min, w_mat, w_max, erepro, R_max)

# --- layer 2: the first object built through newMultispeciesParams -----------
bse <- tryCatch({
  b <- readRDS("params/base_params.rds")
  b@species_params %>% select(species, w_min, w_mat, w_max)
}, error = function(e) NULL)

aud <- src %>%
  rename(src_w_min = w_min, src_w_mat = w_mat, src_w_max = w_max) %>%
  full_join(esp %>% rename(ens_w_min = w_min, ens_w_mat = w_mat,
                           ens_w_max = w_max), by = "species")

if (!is.null(bse))
  aud <- aud %>% left_join(bse %>% rename(bse_w_min = w_min, bse_w_mat = w_mat,
                                          bse_w_max = w_max), by = "species")

aud <- aud %>% mutate(
  # would mizer clamp this row, given the SOURCE values?
  src_violates   = !is.na(src_w_min) & !is.na(src_w_mat) & src_w_min >= src_w_mat,
  src_would_clamp_to = ifelse(src_violates, pmin(CLAMP, src_w_mat / 10), NA_real_),
  # did the ensemble value actually land on the clamp?
  ens_at_clamp   = !is.na(ens_w_min) & abs(ens_w_min - CLAMP) < 1e-12,
  # does the ensemble disagree with source beyond float noise?
  w_min_ratio    = ens_w_min / src_w_min,
  w_min_changed  = !is.na(w_min_ratio) & abs(log10(w_min_ratio)) > 1e-6,
  # ordering valid in what the model actually ran?
  ens_valid      = !is.na(ens_w_min) & ens_w_min < ens_w_mat & ens_w_mat < ens_w_max,
  span_decades   = log10(ens_w_max / ens_w_min),
  w_mat_over_max = ens_w_mat / ens_w_max)

# size bins occupied on the model's own grid
w <- ens@w
aud$bins <- vapply(seq_len(nrow(aud)), function(k) {
  i <- which(ens@species_params$species == aud$species[k])
  if (!length(i)) return(NA_integer_)
  as.integer(sum(w >= w[ens@w_min_idx[[i]]] & w <= ens@species_params$w_max[i]))
}, integer(1))

cat("=== groups whose SOURCE values violate w_min < w_mat (mizer would clamp) ===\n")
v <- aud %>% filter(src_violates) %>%
  select(species, src_w_min, src_w_mat, src_would_clamp_to, ens_w_min)
if (nrow(v)) print(as.data.frame(v), digits = 6, row.names = FALSE) else
  cat("  none\n")

cat("\n=== groups sitting exactly on mizer's 0.001 g fallback ===\n")
c1 <- aud %>% filter(ens_at_clamp) %>% select(species, src_w_min, ens_w_min, bins)
if (nrow(c1)) print(as.data.frame(c1), digits = 6, row.names = FALSE) else
  cat("  none\n")

cat("\n=== ensemble w_min differing from source ===\n")
d <- aud %>% filter(w_min_changed) %>%
  select(species, src_w_min, ens_w_min, w_min_ratio)
if (nrow(d)) print(as.data.frame(d), digits = 6, row.names = FALSE) else
  cat("  none\n")

cat("\n=== ordering validity in the model that actually ran ===\n")
iv <- aud %>% filter(!ens_valid) %>% select(species, ens_w_min, ens_w_mat, ens_w_max)
if (nrow(iv)) print(as.data.frame(iv), digits = 6, row.names = FALSE) else
  cat("  all 19 groups satisfy w_min < w_mat < w_max\n")

cat("\n=== size span and grid occupancy, all groups ===\n")
sp_tab <- aud %>% arrange(span_decades) %>%
  select(species, ens_w_min, ens_w_mat, ens_w_max, span_decades, bins,
         w_mat_over_max)
print(as.data.frame(sp_tab), digits = 5, row.names = FALSE)

cat("\n=== w_mat/w_max ratios at round values ===\n")
r <- aud %>% filter(abs(w_mat_over_max - 0.9) < 1e-6 |
                      abs(w_mat_over_max - 0.01) < 1e-6) %>%
  select(species, ens_w_mat, ens_w_max, w_mat_over_max)
if (nrow(r)) print(as.data.frame(r), digits = 6, row.names = FALSE) else
  cat("  none\n")

# --- the SECOND silent rewrite ----------------------------------------------
# validGivenSpeciesParams applies TWO cascading fixes, w_mat first:
#     w_mat >= w_inf  ->  w_mat <- w_inf/4      (25%!)
#     w_min >= w_mat  ->  w_min <- pmin(0.001, w_mat/10)
# So a w_mat violation silently shrinks maturation size to a quarter of w_max
# before the w_min rule is even reached. Check whether that fired too.
src_v4 <- read.csv("group params/trait_groups_params_vCWC_v4.csv",
                   stringsAsFactors = FALSE)
wmat_bad <- src_v4 %>%
  filter(!is.na(w_mat) & !is.na(w_inf) & w_mat >= w_inf) %>%
  select(species, w_mat, w_inf) %>%
  left_join(esp %>% select(species, ens_w_mat = w_mat, ens_w_max = w_max),
            by = "species") %>%
  mutate(mizer_would_give = w_inf / 4,
         rule_0.9_of_ens_w_max = 0.9 * ens_w_max,
         matches_0.9_rule = abs(ens_w_mat - 0.9 * ens_w_max) < 1e-6 * ens_w_max,
         matches_mizer_25 = abs(ens_w_mat - w_inf / 4) < 1e-6 * w_inf,
         w_max_was_raised = abs(ens_w_max - w_inf) > 1e-6 * w_inf)

cat("\n=== groups whose SOURCE violates w_mat < w_inf (mizer's 25% rule) ===\n")
if (nrow(wmat_bad)) {
  print(as.data.frame(wmat_bad %>%
    select(species, w_mat, w_inf, ens_w_mat, ens_w_max, mizer_would_give,
           matches_0.9_rule, matches_mizer_25, w_max_was_raised)),
    digits = 6, row.names = FALSE)
  cat("\nInterpretation: where matches_0.9_rule is TRUE the manual",
      "`w_mat <- 0.9 * w_max`\ncorrection was applied and PREVENTED mizer's much",
      "harsher 25% rewrite. Where\nw_max_was_raised is TRUE the violation was",
      "fixed by updating w_max instead.\nNeither is a defaulted value.\n")
} else cat("  none\n")

aud$src_wmat_violates <- aud$species %in% wmat_bad$species

write.csv(aud, file.path(out_dir, "28_audit.csv"), row.names = FALSE)

# --- markdown report ---------------------------------------------------------
fmt <- function(x, d = 4) ifelse(is.na(x), "--", formatC(x, format = "g", digits = d))
L <- c(
  "# Size parameter audit -- all 19 functional groups",
  "",
  paste0("Generated by `R/wmin_test/28_size_param_audit.R` on ", Sys.Date(),
         ". mizer ", as.character(packageVersion("mizer")), "."),
  "",
  "## Why this exists",
  "",
  "`small divers` `w_min` was never authored as 0.001 g.",
  "`mizer:::validGivenSpeciesParams` silently rewrites `w_min` to",
  "`pmin(0.001, w_mat/10)` whenever `w_min >= w_mat`, emitting only a warning:",
  "",
  "```r",
  "wrong <- !is.na(sp$w_min) & !is.na(sp$w_mat) & sp$w_min >= sp$w_mat",
  "sp$w_min[wrong] <- pmin(0.001, sp$w_mat[wrong]/10)",
  "```",
  "",
  "That warning was emitted at `newMultispeciesParams()`",
  "(`optim_model_setup_old/model_setup_v4.Rmd:378`) and missed. This audit checks",
  "whether it happened anywhere else, and whether any group's realised parameters",
  "disagree with the empirical source table.",
  "",
  "Scope: **audit all groups, change only `small divers`.**",
  "",
  "## Layers compared",
  "",
  "| layer | object |",
  "|---|---|",
  "| source | `group params/trait_groups_params_vCWC_v4.csv` |",
  "| built | `params/base_params.rds` |",
  "| ensemble | `Manuscript data/params_sel_adj.rds` |",
  "",
  "## Finding 1 -- source rows that violate mizer's ordering constraint",
  "")
if (nrow(v)) {
  L <- c(L, "| species | source w_min | source w_mat | mizer clamps to | ensemble w_min |",
         "|---|---|---|---|---|")
  for (k in seq_len(nrow(v)))
    L <- c(L, sprintf("| %s | %s | %s | %s | %s |", v$species[k],
                      fmt(v$src_w_min[k]), fmt(v$src_w_mat[k]),
                      fmt(v$src_would_clamp_to[k]), fmt(v$ens_w_min[k])))
} else L <- c(L, "None.")

L <- c(L, "", "## Finding 2 -- groups sitting on the 0.001 g fallback", "")
if (nrow(c1)) {
  L <- c(L, "| species | source w_min | ensemble w_min | bins occupied |",
         "|---|---|---|---|")
  for (k in seq_len(nrow(c1)))
    L <- c(L, sprintf("| %s | %s | %s | %s |", c1$species[k],
                      fmt(c1$src_w_min[k]), fmt(c1$ens_w_min[k]), c1$bins[k]))
} else L <- c(L, "None.")

L <- c(L, "", "## Finding 3 -- size span and grid occupancy", "",
       paste0("Grid: ", length(w), " bins over ",
              formatC(log10(max(w)/min(w)), format = "f", digits = 2),
              " decades = ",
              formatC(log10(max(w)/min(w))/length(w), format = "f", digits = 3),
              " decades/bin."),
       "",
       "| species | w_min | w_mat | w_max | span (dec) | bins | w_mat/w_max |",
       "|---|---|---|---|---|---|---|")
for (k in seq_len(nrow(sp_tab)))
  L <- c(L, sprintf("| %s | %s | %s | %s | %s | %s | %s |",
                    sp_tab$species[k], fmt(sp_tab$ens_w_min[k]),
                    fmt(sp_tab$ens_w_mat[k]), fmt(sp_tab$ens_w_max[k]),
                    fmt(sp_tab$span_decades[k], 3), sp_tab$bins[k],
                    fmt(sp_tab$w_mat_over_max[k], 3)))

L <- c(L, "", "## Finding 4 -- the SECOND silent rewrite, and why it did NOT bite",
       "",
       "`validGivenSpeciesParams` applies **two** cascading fixes, `w_mat` first:",
       "",
       "| condition | mizer's silent fix |",
       "|---|---|",
       "| `w_mat >= w_inf` | `w_mat <- w_inf/4` (25%) |",
       "| `w_min >= w_mat` | `w_min <- pmin(0.001, w_mat/10)` |",
       "",
       "Four groups violate the first rule in the source table, all with",
       "`w_mat == w_inf` exactly. Left alone, mizer would have shrunk their",
       "maturation size to a quarter of `w_max` -- minke whales to 1.5e6 g instead of",
       "5.4e6 g, a 3.6x distortion.",
       "")
if (nrow(wmat_bad)) {
  L <- c(L, "| species | source w_mat | source w_inf | ensemble w_mat | ensemble w_max | mizer would give | how it was fixed |",
         "|---|---|---|---|---|---|---|")
  for (k in seq_len(nrow(wmat_bad))) {
    how <- if (isTRUE(wmat_bad$matches_0.9_rule[k])) "manual `w_mat <- 0.9 * w_max`"
    else if (isTRUE(wmat_bad$w_max_was_raised[k])) "`w_max` raised from source"
    else if (isTRUE(wmat_bad$matches_mizer_25[k])) "**mizer's 25% rewrite -- NOT caught**"
    else "other"
    L <- c(L, sprintf("| %s | %s | %s | %s | %s | %s | %s |",
                      wmat_bad$species[k], fmt(wmat_bad$w_mat[k]),
                      fmt(wmat_bad$w_inf[k]), fmt(wmat_bad$ens_w_mat[k]),
                      fmt(wmat_bad$ens_w_max[k]), fmt(wmat_bad$mizer_would_give[k]),
                      how))
  }
}
L <- c(L, "",
       "**These were handled correctly.** The `w_mat <- 0.9 * w_max` rule at",
       "`03_model_setup_pre_therMizer.rmd:378-381` /",
       "`optim_model_setup_old/model_setup_v4.Rmd:520-546` was *applied*, and it",
       "prevented the harsher 25% rewrite. `leopard seals` was fixed instead by",
       "raising `w_max` to the observed 450 kg. So the exact 0.900 ratios are a",
       "deliberate, necessary correction -- not a defaulted or fitted value, and not",
       "evidence of a second bug.",
       "",
       "This narrows the blast radius considerably. In `model_setup_v4.Rmd` the",
       "`w_mat` fix lived in `params_v1` and the `w_min` fix in `params_v2`; line 545",
       "continues from `params_v1`, so **the `w_mat` half survived and only the",
       "`w_min` half was lost**. `small divers` does not violate `w_mat < w_inf`",
       "(ratio 0.711) -- its only problem is `w_min`.",
       "",
       "The three groups at `w_mat/w_max` = exactly 0.010 (mesozooplankton, other",
       "macrozooplankton, salps) do not violate either rule; their round ratio is",
       "carried straight from the source table and is out of scope here.",
       "")

L <- c(L, "",
       "## The `small divers` compromise",
       "",
       "`w_min` for predator groups derives as `W0 = min(w_indep)` -- weight at",
       "**independence** (`group params/1g_simplified_groups_params.Rmd:442-447`).",
       "That is conceptually correct: mizer's `w_min` is the size at which an",
       "individual feeds for itself, since prey selection scales with body size. A",
       "dependent chick is provisioned by its parents and the model has no",
       "parental-energy-transfer mechanism, so entering at hatching mass would create",
       "a phantom independent forager eating prey it could never catch.",
       "",
       "The constraint is unsatisfiable with the empirical values, and that is real",
       "biology. Penguins fledge heavier than adult breeding mass and then slim down,",
       "so `w_indep > w_mat` for **all three** constituent species individually",
       "(`csvs/predator_parameters_updated.csv`):",
       "",
       "| species | w_mat | w_indep (fledging) | w_max | w_birth |",
       "|---|---|---|---|---|",
       "| Adelie Penguin | 4000 | 6000 | 6000 | 100 |",
       "| Crested Penguin | 4300 | 4500 | 4300 | 94 |",
       "| Gentoo Penguin | 4500 | 6700 | 4500 | 135 |",
       "",
       "Group aggregates are `min(w_indep) = 4500` against `mean(w_mat) = 4266.667`.",
       "No aggregation choice fixes this.",
       "",
       "**`w_min = 0.85 * w_mat = 3626.667 g` is therefore a deliberate compromise:**",
       "the largest value that respects mizer's ordering constraint while keeping",
       "recruits feeding independently. Hatchling mass (94 g) was considered and",
       "rejected on the feeding-mechanism grounds above.",
       "",
       "Known limitation to report: at 3626.667 g the group occupies **2 bins** on",
       "this grid against 44 under the erroneous value, so it is effectively",
       "unstructured in size. That is inherent to a species whose independent-feeding",
       "mass is 85% of its maturation mass, and is a caveat on any penguin-specific",
       "size-structure result.",
       "",
       "## Recommendation",
       "",
       "1. Correct `small divers` `w_min` to 3626.667 g and recalibrate its",
       "   reproduction pair (`erepro`, `R_max`) -- see",
       "   `R/wmin_test/27_recalibrate_penguin_repro.R`. Changing `w_min` alone leaves",
       "   the group uncalibrated and drives it extinct.",
       "2. Add a build-time assertion wherever `newMultispeciesParams()` is called:",
       "   check `w_min < w_mat < w_max` before, and compare realised `w_min` against",
       "   the input after. mizer's warning is too easy to miss.",
       "3. Leave the other groups unchanged, but record the round-number",
       "   `w_mat/w_max` ratios above so the manuscript can state what was checked.",
       "")

writeLines(L, "docs/size_parameter_audit.md")
cat("\nWrote docs/size_parameter_audit.md and", file.path(out_dir, "28_audit.csv"), "\n")
