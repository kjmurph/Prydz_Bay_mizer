# =============================================================================
# STAGE A -- emit trait_groups_params_vCWC_v5: an AS-BUILT, clamp-free record of
# the model's size parameters, with the `small divers` w_min compromise applied.
#
# WHY THE FIRST VERSION OF THIS SCRIPT WAS WRONG. It changed only the one cell
# (`small divers` w_min 4500 -> 3626.667) and left everything else at v4. An
# independent review showed the resulting table is still unsafe to build from,
# and mizer's own validator confirms it:
#
#   WARN: For the species leopard seals ... `w_min` is not smaller than `w_mat`.
#         I have reduced the values.        ->  leopard seals w_min 200000 -> 0.001
#
# The cause is a CASCADE. validGivenSpeciesParams applies two rules in order, and
# the first mutates the value the second tests:
#     1. w_mat >= w_inf  ->  w_mat <- w_inf/4
#     2. w_min >= w_mat  ->  w_min <- pmin(0.001, w_mat/10)     (NEW w_mat)
# `leopard seals` has w_mat == w_inf == 348000 in v4, so rule 1 rewrites w_mat to
# 87000, and its perfectly valid w_min of 200000 then trips rule 2. It escaped in
# the real build ONLY because an in-script override raised its w_max to 450000
# before the params were constructed (`model_setup_v4.Rmd:272`). v4 and the first
# v5 both fail to record that, so neither table is self-sufficient.
#
# THE FIX. v5 now carries the **as-built** size parameters, read directly from the
# object the ensemble actually ran (`Manuscript data/params_sel_adj.rds`), which
# already incorporates every documented in-script override:
#   * leopard seals w_max raised 348000 -> 450000 (sealifebase observed max)
#   * w_mat <- 0.9 * w_max for large divers, minke, orca, sperm whales
#   * orca w_max updated from sealifebase
# then the one substantive correction on top: `small divers` w_min -> 0.85 * w_mat.
#
# This is NOT a scientific change to groups other than `small divers`. Every other
# value is transcribed from what the model already ran. The point is that the table
# now reproduces the built model instead of silently diverging from it.
#
# v4 is retained unchanged as the provenance of the existing 2,111-member ensemble.
#
# Writes group params/trait_groups_params_vCWC_v5.csv
#        group params/trait_groups_params_vCWC_v5_PROVENANCE.md
# =============================================================================

suppressPackageStartupMessages(library(mizer))
source("R/check_size_params.R")

PEN  <- "small divers"
SRC  <- "group params/trait_groups_params_vCWC_v4.csv"
DEST <- "group params/trait_groups_params_vCWC_v5.csv"
BUILT <- "Manuscript data/params_sel_adj.rds"

v4 <- read.csv(SRC, stringsAsFactors = FALSE, check.names = FALSE)
built <- readRDS(BUILT)@species_params
cat("read", SRC, "-", nrow(v4), "groups\n")
cat("read as-built size params from", BUILT, "\n\n")

# --- confirm the as-built object is itself clamp-free ------------------------
cat("=== is the as-built model clamp-free? ===\n")
chk_built <- data.frame(species = built$species, w_min = built$w_min,
                        w_mat = built$w_mat, w_max = built$w_max)
cl <- simulate_mizer_clamps(chk_built$w_min, chk_built$w_mat, chk_built$w_max)
cat("  groups mizer would rewrite:",
    if (any(cl$mat_clamped | cl$min_clamped))
      paste(chk_built$species[cl$mat_clamped | cl$min_clamped], collapse = ", ")
    else "NONE", "\n")
cat("  (small divers is expected here -- it carries the erroneous 0.001 g)\n\n")

# --- build v5 -----------------------------------------------------------------
v5 <- v4
m <- match(v5$species, built$species)
stopifnot(!anyNA(m))

changes <- list()
note <- function(sp, field, from, to, why)
  changes[[length(changes) + 1]] <<- data.frame(species = sp, field = field,
    v4 = from, v5 = to, reason = why, stringsAsFactors = FALSE)

for (k in seq_len(nrow(v5))) {
  j <- m[k]
  for (fld in c("w_min", "w_mat", "w_inf")) {
    src_fld <- if (fld == "w_inf") "w_max" else fld
    old <- v5[[fld]][k]; new <- built[[src_fld]][j]
    if (!isTRUE(all.equal(old, new, tolerance = 1e-9))) {
      v5[[fld]][k] <- new
      note(v5$species[k], fld, old, new, "as-built (in-script override)")
    }
  }
}

# the one substantive correction
i <- which(v5$species == PEN)
old_wmin <- v5$w_min[i]
new_wmin <- 0.85 * v5$w_mat[i]
v5$w_min[i] <- new_wmin
note(PEN, "w_min", old_wmin, new_wmin, "CORRECTION: 0.85 * w_mat compromise")

chg <- do.call(rbind, changes)
cat("=== every cell changed relative to v4 ===\n")
print(chg, digits = 10, row.names = FALSE)

# --- validate -----------------------------------------------------------------
cat("\n=== validating v5 (cascade-aware) ===\n")
chk <- v5; chk$w_max <- chk$w_inf
res <- try(assert_size_params(chk), silent = TRUE)
if (inherits(res, "try-error")) {
  cat("FAILED -- v5 is still not safe to build from:\n")
  cat(conditionMessage(attr(res, "condition")), "\n")
  stop("refusing to write an unsafe v5", call. = FALSE)
}

cl5 <- simulate_mizer_clamps(v5$w_min, v5$w_mat, v5$w_inf)
stopifnot(!any(cl5$mat_clamped), !any(cl5$min_clamped))
cat("  no group is rewritten by either mizer rule, including the cascade\n")

# --- independent confirmation: let mizer itself decide -----------------------
cat("\n=== mizer's own validator on v5 ===\n")
vg <- get("validGivenSpeciesParams", envir = asNamespace("mizer"))
nwarn <- 0
out <- withCallingHandlers(vg(v5),
  warning = function(w) { nwarn <<- nwarn + 1
                          cat("  WARN:", conditionMessage(w), "\n")
                          invokeRestart("muffleWarning") })
moved <- which(abs(out$w_min - v5$w_min) > 1e-9 * pmax(v5$w_min, 1e-30) |
               abs(out$w_mat - v5$w_mat) > 1e-9 * pmax(v5$w_mat, 1e-30))
cat("  warnings:", nwarn, "| groups rewritten:",
    if (length(moved)) paste(out$species[moved], collapse = ", ") else "NONE", "\n")
stopifnot(nwarn == 0, length(moved) == 0)

write.csv(v5, DEST, row.names = FALSE)
cat("\nwrote", DEST, "\n")

# --- provenance ---------------------------------------------------------------
fm <- function(x) formatC(x, format = "g", digits = 10)
L <- c(
  "# trait_groups_params_vCWC_v5 - provenance",
  "",
  paste0("Generated by `R/wmin_test/35_emit_trait_table_v5.R` on ", Sys.Date(), "."),
  "",
  "## What v5 is",
  "",
  "An **as-built, clamp-free** record of the model's size parameters: the values",
  "the 2,111-member ensemble actually ran, plus the one substantive correction to",
  "`small divers` `w_min`. Verified against mizer's own validator to trigger",
  "**zero** warnings and **zero** rewrites.",
  "",
  "**v4 is retained unchanged** as the provenance of the existing ensemble.",
  "",
  "## Why v4 is not safe to build from",
  "",
  "`validGivenSpeciesParams` applies two rewrites in order, and the first mutates",
  "the value the second tests against:",
  "",
  "| order | condition | silent fix |",
  "|---|---|---|",
  "| 1 | `w_mat >= w_inf` | `w_mat <- w_inf/4` |",
  "| 2 | `w_min >= w_mat` | `w_min <- pmin(0.001, w_mat/10)` |",
  "",
  "In v4 that cascade clamps **two** groups, not one:",
  "",
  "| group | how it is clamped |",
  "|---|---|",
  "| `small divers` | directly: `w_min` 4500 >= `w_mat` 4266.667 -> **0.001 g** |",
  "| `leopard seals` | **by cascade**: `w_mat` 348000 == `w_inf` -> rewritten to 87000, after which its perfectly valid `w_min` of 200000 trips rule 2 -> **0.001 g** |",
  "",
  "`leopard seals` escaped in the real build only because an in-script override",
  "raised its `w_max` to 450000 before the params were constructed",
  "(`optim_model_setup_old/model_setup_v4.Rmd:272`). Neither v4 nor the first",
  "version of v5 recorded that, so neither table was self-sufficient. This was",
  "found by independent review, not by the original audit, whose Finding 1 tested",
  "the two rules separately and so missed the cascade.",
  "",
  "## Every cell that differs from v4",
  "",
  "| group | field | v4 | v5 | reason |",
  "|---|---|---|---|---|")
for (k in seq_len(nrow(chg)))
  L <- c(L, sprintf("| `%s` | `%s` | %s | %s | %s |", chg$species[k],
                    chg$field[k], fm(chg$v4[k]), fm(chg$v5[k]), chg$reason[k]))
L <- c(L, "",
  "Rows marked *as-built* are **transcriptions, not new science** -- they are the",
  "documented in-script overrides that were already applied to construct the",
  "model: `leopard seals` `w_max` raised to the observed 450 kg, and",
  "`w_mat <- 0.9 * w_max` for `large divers`, `minke whales`, `orca` and",
  "`sperm whales`. Only the `small divers` `w_min` row is a substantive change.",
  "",
  "## The `small divers` compromise",
  "",
  "`w_min` for predator groups derives as `W0 = min(w_indep)` -- weight at",
  "**independence** (`group params/1g_simplified_groups_params.Rmd:442-447`). That",
  "is conceptually correct: mizer's `w_min` is the size at which an individual",
  "feeds for itself, prey selection scales with body size, and the model has no",
  "parental-energy-transfer mechanism, so a provisioned chick cannot be",
  "represented.",
  "",
  "But penguins fledge heavier than adult breeding mass, so `w_indep > w_mat` for",
  "**all three** constituent species individually",
  "(`csvs/predator_parameters_updated.csv`):",
  "",
  "| species | w_mat | w_indep (fledging) | w_max | w_birth |",
  "|---|---|---|---|---|",
  "| Adelie Penguin | 4000 | 6000 | 6000 | 100 |",
  "| Crested Penguin | 4300 | 4500 | 4300 | 94 |",
  "| Gentoo Penguin | 4500 | 6700 | 4500 | 135 |",
  "",
  "Group aggregates are `min(w_indep) = 4500` against `mean(w_mat) = 4266.667`. No",
  "aggregation choice resolves it, so **`0.85 * w_mat` is a deliberate compromise,",
  "not a derived value** -- recorded here so it is never mistaken for one.",
  "Hatchling mass (94 g) was considered and rejected on the feeding-mechanism",
  "grounds above. The value is fixed; no intermediate value, finer `w` grid, or",
  "group split is in scope.",
  "",
  "## Two things this table cannot fix",
  "",
  "1. **The realised `w_min` is not 3626.667 g.** mizer sets",
  paste0("   `w_min_idx = max(which(w <= w_min))`, so on this grid the value snaps down to"),
  "   **2942.204 g = 0.690 x `w_mat`**. Quote the realised value, not the nominal one.",
  "2. **Correcting `w_min` alone is insufficient.** The penguin `erepro`/`R_max`",
  "   pair was calibrated against 0.001 g and must be recalibrated at the same",
  "   time, or the group goes extinct. The 2019 author hit exactly this and",
  "   abandoned the repair with `Error in setBevertonHolt(...): Some species have",
  "   no reproduction` (`model_setup_v4.Rmd:541`). See",
  "   `docs/small_divers_wmin_stageB_results.md`.",
  "",
  "## Reach",
  "",
  "Nothing in the current `00`-`09` workflow reads this file:",
  "`newMultispeciesParams()` is never called in the therMizer-era chain, which",
  "loads saved `MizerParams` objects instead. v5 exists so any future rebuild",
  "starts from a table that reproduces the built model without silent rewriting.",
  "")
writeLines(L, "group params/trait_groups_params_vCWC_v5_PROVENANCE.md")
cat("wrote group params/trait_groups_params_vCWC_v5_PROVENANCE.md\n")