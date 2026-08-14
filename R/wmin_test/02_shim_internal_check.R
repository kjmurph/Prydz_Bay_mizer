# =============================================================================
# Two things:
#
#  (A) INTERNAL validation of the shim -- the part Stage 0 actually depends on.
#      1. With temp_eff = 1 the shim must reproduce mizer::getDiet(proportion =
#         FALSE) exactly. That validates the transcription of getDiet.
#      2. rowSums(diet) must equal (1 - f) * encounter exactly. That validates
#         that the temperature scaling has been threaded through consistently.
#
#  (B) DIAGNOSIS of why the legacy whale-consumption artefact does not match.
#      Search every one of the 2111 stored series for the value the shim
#      computes, to test whether the mismatch is an indexing offset rather than
#      a methodological difference.
#
# Writes Output_large_files/wmin_test/02_shim_internal_check.rds
# =============================================================================

source("R/wmin_test/thermizer_shim.R")

out_dir <- "Output_large_files/wmin_test"
res <- list()

ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
idx <- ens$metadata$top10pct_indices

cat("=== metadata source files ===\n")
cat("fished :", ens$metadata$source_fished_file, "\n")
cat("climate:", ens$metadata$source_climate_file, "\n")
cat("rmse   :", ens$metadata$source_rmse_file, "\n\n")

sim <- ens$fished_top10pct[[1]]
p   <- sim@params
yr  <- 2005
st  <- sim_state_at(sim, yr)

# --- (A1) shim with temp_eff = 1 vs mizer::getDiet -----------------------------
one <- setNames(rep(1, nrow(p@species_params)), p@species_params$species)
fl_plain <- mizer::mizerFeedingLevel(
  p, n = st$n, n_pp = st$n_pp, n_other = st$n_other, t = 0,
  encounter = mizer::mizerEncounter(p, n = st$n, n_pp = st$n_pp,
                                    n_other = st$n_other, t = 0))
shim_plain <- ther_diet(p, st$n, st$n_pp, st$n_other, yr,
                        temp_eff = one, feeding_level = fl_plain)

mz <- tryCatch(
  mizer::getDiet(p, n = st$n, n_pp = st$n_pp, n_other = st$n_other,
                 proportion = FALSE),
  error = function(e) { cat("mizer::getDiet errored:", conditionMessage(e), "\n"); NULL })

if (!is.null(mz)) {
  d <- max(abs(shim_plain - mz))
  rel <- d / max(abs(mz))
  cat("(A1) shim(temp_eff=1) vs mizer::getDiet : max abs diff", format(d, digits = 4),
      " max rel", format(rel, digits = 4),
      if (rel < 1e-12) "  -> IDENTICAL\n" else "  -> DIFFERS\n")
  res$A1_max_rel_diff <- rel
}

# --- (A2) rowSums(diet) == (1 - f) * encounter --------------------------------
te <- ther_temp_effect(p, yr)
fl <- ther_feeding_level(p, st$n, st$n_pp, st$n_other, yr, temp_eff = te)
dietA <- ther_diet(p, st$n, st$n_pp, st$n_other, yr, temp_eff = te,
                   feeding_level = fl)
enc_scaled <- sweep(
  mizer::mizerEncounter(p, n = st$n, n_pp = st$n_pp, n_other = st$n_other, t = 0),
  1, te, "*")
lhs <- rowSums(dietA, dims = 2)
rhs <- (1 - fl) * enc_scaled * (st$n > 0)
rel2 <- max(abs(lhs - rhs)) / max(abs(rhs))
cat("(A2) rowSums(diet) vs (1-f)*encounter   : max rel", format(rel2, digits = 4),
    if (rel2 < 1e-12) "  -> CONSISTENT\n" else "  -> INCONSISTENT\n")
res$A2_max_rel_diff <- rel2

# --- (B) hunt the legacy artefact ---------------------------------------------
cat("\n=== (B) legacy artefact diagnosis ===\n")
ref <- readRDS("whale_consumption_outputs/fishing_baleen_krill_all_sims.rds")

krill_cols <- c("antarctic krill", "other krill")
mine <- c(
  ant_only  = sum(dietA["baleen whales", , "antarctic krill"] *
                    st$n["baleen whales", ] * p@dw),
  both_krill = sum(rowSums(dietA["baleen whales", , krill_cols, drop = FALSE],
                           dims = 1) * st$n["baleen whales", ] * p@dw)
)
cat("shim, member 1 (orig idx ", idx[1], "), year ", yr, ":\n", sep = "")
print(format(mine, big.mark = ",", scientific = FALSE))

ref_at <- vapply(ref, function(d) {
  v <- d$total_consumption[d$year == yr]
  if (length(v) == 1) v else NA_real_
}, numeric(1))
cat("stored series at", yr, ": range [", format(min(ref_at, na.rm = TRUE), digits = 4),
    ",", format(max(ref_at, na.rm = TRUE), digits = 4), "]\n")
cat("stored value at declared index", idx[1], ":",
    format(ref_at[idx[1]], big.mark = ",", scientific = FALSE), "\n")

for (nm in names(mine)) {
  rr <- abs(ref_at - mine[[nm]]) / mine[[nm]]
  best <- order(rr)[1:5]
  cat("\nclosest stored series to shim '", nm, "':\n", sep = "")
  print(data.frame(ref_index = best, ref_value = ref_at[best],
                   rel_err = rr[best]), digits = 5)
}
res$mine <- mine
res$ref_at_declared_index <- ref_at[idx[1]]

saveRDS(res, file.path(out_dir, "02_shim_internal_check.rds"))
cat("\nSaved:", file.path(out_dir, "02_shim_internal_check.rds"), "\n")
