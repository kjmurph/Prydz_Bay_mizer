# =============================================================================
# What do the STORED params of the candidate members actually look like against
# the hard constraints?
#
# This matters because 39_full_ensemble_paired.R measures erepro / R_max on
# `out@species_params` -- the state AFTER steady(preserve="erepro"), which ends
# by calling setBevertonHolt() and re-derives R_max.  A member can therefore pass
# that filter while its STORED params carry R_max = Inf, which is exactly what
# member 1776 does (mesozooplankton).
#
#   Rscript R/catch_fit/tests_baseline_admissibility.R [n_candidates]
# =============================================================================

source("R/catch_fit/01_harness.R")

args <- commandArgs(trailingOnly = TRUE)
n_extra <- if (length(args)) as.integer(args[1]) else 12L

rank <- read.csv(CF_RANKING)
x    <- readRDS(CF_ENSEMBLE)
idx  <- x$metadata$top10pct_indices

# candidates: the deduped ranking, restricted to what the 212-member file holds
cand <- rank$sim_index[rank$sim_index %in% idx][seq_len(n_extra)]
cand_rank <- rank$rank[match(cand, rank$sim_index)]

# the 39-run flags, for comparison
fs <- list.files("Output_large_files/wmin_test/39_results", pattern = "^res_\\d+\\.rds$",
                 full.names = TRUE)
D <- do.call(rbind, lapply(fs, readRDS))

out <- do.call(rbind, lapply(seq_along(cand), function(k) {
  si <- cand[k]
  s  <- x$fished_top10pct[[match(si, idx)]]
  p  <- suppressWarnings(validParams(s@params))
  sp <- p@species_params
  d39 <- D[D$sim_index == si, ]
  data.frame(
    rank = cand_rank[k], sim_index = si,
    rmse = rank$rmse[match(si, rank$sim_index)],
    stored_max_erepro = max(sp$erepro),
    stored_n_erepro_ge1 = sum(sp$erepro >= 1),
    stored_n_rmax_inf = sum(!is.finite(sp$R_max)),
    stored_rmax_inf_groups = paste(sp$species[!is.finite(sp$R_max)], collapse = ";"),
    f39_ctl_rmax_inf = if (nrow(d39)) d39$ctl_n_rmax_inf[1] else NA_integer_,
    f39_trt_rmax_inf = if (nrow(d39)) d39$trt_n_rmax_inf[1] else NA_integer_,
    f39_ctl_max_erepro = if (nrow(d39)) d39$ctl_max_erepro[1] else NA_real_,
    f39_repro_infeasible = if (nrow(d39)) d39$repro_infeasible[1] else NA,
    f39_clean = if (nrow(d39)) with(d39[1, ],
      trt_ok && trt_pass && trt_n_erepro_ge1 == 0 && trt_n_rmax_inf == 0 &&
        !repro_infeasible) else NA,
    stringsAsFactors = FALSE)
}))

cat("=== stored-params admissibility vs the 39-run filter ===\n\n")
print(data.frame(rank = out$rank, sim = out$sim_index, rmse = round(out$rmse, 4),
                 stored_rmaxInf = out$stored_n_rmax_inf,
                 stored_groups = substr(out$stored_rmax_inf_groups, 1, 34),
                 stored_maxErepro = round(out$stored_max_erepro, 4),
                 f39_ctlInf = out$f39_ctl_rmax_inf,
                 f39_trtInf = out$f39_trt_rmax_inf,
                 f39_clean = out$f39_clean), row.names = FALSE)

cat("\n=== members clean on BOTH the 39 filter and their stored params ===\n")
both <- out[isTRUE_vec <- (!is.na(out$f39_clean) & out$f39_clean) &
              out$stored_n_rmax_inf == 0 & out$stored_n_erepro_ge1 == 0, ]
print(data.frame(rank = both$rank, sim = both$sim_index,
                 rmse = round(both$rmse, 4)), row.names = FALSE)

write.csv(out, file.path(cf_out_dir(arm = NA), "00_baseline_admissibility.csv"),
          row.names = FALSE)
cat("\nsaved 00_baseline_admissibility.csv\n")
