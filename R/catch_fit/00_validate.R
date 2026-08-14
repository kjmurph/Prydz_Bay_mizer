# =============================================================================
# STAGE 0 -- harness validation.  Nothing downstream runs until this passes.
#
# Five gates, per member:
#   1  re-projecting the stored params reproduces the stored yield
#   2  a NULL treatment through gear_params<- reproduces it too
#   3  @rates_funcs / @initial_n / @resource_dynamics / @other_dynamics survive
#   4  the 1929-cached start equals the full 1841 projection (else cache off)
#   5  the recomputed pooled RMSE matches yield_rmse_per_sim_deduped.csv
#
# Gate 4 is advisory: failing it disables the cache, it does not fail the run.
# Gates 1, 2, 3, 5 are hard.
#
# USAGE
#   Rscript R/catch_fit/00_validate.R          # all 5 members
#   Rscript R/catch_fit/00_validate.R 446      # smoke test on one
# =============================================================================

source("R/catch_fit/01_harness.R")

args <- commandArgs(trailingOnly = TRUE)
members_wanted <- if (length(args)) as.integer(args) else CF_MEMBERS

cf_log("=== Stage 0 validation: members ", paste(members_wanted, collapse = ", "))
cf_log("mizer ", as.character(packageVersion("mizer")),
       " / therMizer ", as.character(packageVersion("therMizer")))

t0 <- proc.time()
members <- cf_load_members(members_wanted)
cf_log("loaded ", length(members), " members in ",
       round((proc.time() - t0)[["elapsed"]], 1), " s")

rows <- cf_comparison_rows()
cf_log("comparison rows: ", nrow(rows), " across ",
       length(unique(rows$Species)), " species")

res <- cf_validate_harness(members)

cat("\n")
print(res, row.names = FALSE)

d <- cf_out_dir()
saveRDS(list(gates = res, use_cache = attr(res, "use_cache"),
             members = members_wanted, when = Sys.time()),
        file.path(d, "00_validation.rds"))
write.csv(res, file.path(d, "00_validation.csv"), row.names = FALSE)

# ---- baseline per-species decomposition, the "before" half of the deliverable
# Arm A reads the stored sim directly.  Arms B and C have no stored sim -- their
# starting state was rebuilt by 05_wmin_arms.R -- so their baseline is the same
# member projected at its own unmodified catchability, which is the mult = 1
# point of the response surface.
cf_log("\nbaseline per-species decomposition")
base <- do.call(rbind, lapply(members, function(m) {
  y <- if (is.null(m$stored_sim)) {
    cf_project_yield(m$params, NULL)$yield
  } else {
    yy <- getYield(m$stored_sim)
    yy[as.numeric(rownames(yy)) <= CF_LAST_YEAR, , drop = FALSE]
  }
  sc <- cf_score_yield_matrix(y)
  cbind(sim_index = m$sim_index, total_rmse = sc$rmse, sc$per_species)
}))
write.csv(base, file.path(d, "00_baseline_per_species.csv"), row.names = FALSE)

for (si in members_wanted) {
  b <- base[base$sim_index == si, ]
  b <- b[order(-b$sse_pct), ]
  cat(sprintf("\n--- member %d, total RMSE %.4f ---\n", si, b$total_rmse[1]))
  print(data.frame(Species = b$Species, n = b$n,
                   rmse = round(b$rmse, 3),
                   sse_pct = round(b$sse_pct, 1),
                   mod_obs = signif(b$mod_obs_ratio, 3)), row.names = FALSE)
}

if (!isTRUE(attr(res, "pass")))
  stop("STAGE 0 FAILED -- do not proceed")
cf_log("\nStage 0 PASS. cache = ", attr(res, "use_cache"))
