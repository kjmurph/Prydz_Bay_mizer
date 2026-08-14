# =============================================================================
# Validate the therMizer shim against outputs produced when therMizer WAS
# installed: whale_consumption_outputs/fishing_baleen_krill_all_sims.rds, which
# extract_whale_consumption.R built with getDiet(proportion = FALSE).
#
# extract_whale_consumption.R used mizer's stock getDiet(), which applies the
# temperature scaling ONLY through the feeding level (via rates_funcs$Encounter)
# and not to the encounter numerator. So the reference numbers correspond to the
# shim run with temp_eff = 1 in the numerator. We therefore compare three
# variants and report which reproduces the stored series:
#   A  full therMizer scaling (numerator + feeding level)   <- self-consistent
#   B  scaling in the feeding level only                    <- stock getDiet
#   C  no temperature scaling at all
#
# The point of the exercise is to bound how much the choice matters, not to
# crown a winner: Stage 0's headline number is a WITHIN-species size share, from
# which a per-species scalar cancels.
#
# Writes Output_large_files/wmin_test/01_shim_validation.rds
# =============================================================================

source("R/wmin_test/thermizer_shim.R")

out_dir <- "Output_large_files/wmin_test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== shim validation:", format(Sys.time()), "===\n")

ens  <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
idx  <- ens$metadata$top10pct_indices          # sim_i -> original ensemble index
ref  <- readRDS("whale_consumption_outputs/fishing_baleen_krill_all_sims.rds")

# `ref` is an unnamed list of 2111 data frames (year, total_consumption), one
# per ORIGINAL ensemble index -- not a wide frame.
get_ref_series <- function(sim_index) {
  if (sim_index > length(ref) || is.null(ref[[sim_index]])) return(NULL)
  d <- ref[[sim_index]]
  data.frame(year = d$year, ref_consumption = d$total_consumption)
}

test_members <- c(1, 5, 20)                    # positions in the top-10% list
test_years   <- c(1900, 1950, 2005)

rows <- list()
for (m in test_members) {
  sim <- ens$fished_top10pct[[m]]
  rs  <- get_ref_series(idx[m])
  if (is.null(rs)) { cat("no reference column for member", m, "\n"); next }
  p <- sim@params
  for (y in test_years) {
    st <- sim_state_at(sim, y)
    te <- ther_temp_effect(p, y)

    # Variant A: full scaling
    fl_A   <- ther_feeding_level(p, st$n, st$n_pp, st$n_other, y, temp_eff = te)
    dietA  <- ther_diet(p, st$n, st$n_pp, st$n_other, y, temp_eff = te,
                        feeding_level = fl_A)
    # Variant B: scaling in feeding level only (stock getDiet behaviour)
    dietB  <- ther_diet(p, st$n, st$n_pp, st$n_other, y,
                        temp_eff = setNames(rep(1, length(te)), names(te)),
                        feeding_level = fl_A)
    # Variant C: no scaling anywhere
    one    <- setNames(rep(1, length(te)), names(te))
    fl_C   <- ther_feeding_level(p, st$n, st$n_pp, st$n_other, y, temp_eff = one)
    dietC  <- ther_diet(p, st$n, st$n_pp, st$n_other, y, temp_eff = one,
                        feeding_level = fl_C)

    krill_by <- function(diet, pred) sum(diet[pred, , "antarctic krill"] *
                                         st$n[pred, ] * p@dw)
    rows[[length(rows) + 1]] <- data.frame(
      member = m, sim_index = idx[m], year = y,
      ref  = rs$ref_consumption[rs$year == y],
      A_full = krill_by(dietA, "baleen whales"),
      B_flonly = krill_by(dietB, "baleen whales"),
      C_none = krill_by(dietC, "baleen whales"),
      temp_eff_baleen = te[["baleen whales"]],
      temp_eff_smalldivers = te[["small divers"]]
    )
  }
}
res <- do.call(rbind, rows)
res$ratio_A <- res$A_full / res$ref
res$ratio_B <- res$B_flonly / res$ref
res$ratio_C <- res$C_none / res$ref
print(res, digits = 6)

cat("\n--- ratio of each variant to the stored reference ---\n")
for (v in c("ratio_A", "ratio_B", "ratio_C"))
  cat(sprintf("  %-8s median %.6f   range [%.6f, %.6f]\n", v,
              median(res[[v]]), min(res[[v]]), max(res[[v]])))

cat("\n--- per-species temperature effect (member 1, year 2005) ---\n")
te1 <- ther_temp_effect(ens$fished_top10pct[[1]]@params, 2005)
print(round(te1, 4))

saveRDS(res, file.path(out_dir, "01_shim_validation.rds"))
cat("\nSaved:", file.path(out_dir, "01_shim_validation.rds"), "\n")
