# =============================================================================
# PHASE 2 -- recover the Monte Carlo draws for every distinct ensemble member, so
# each member can be rebuilt from the RECALIBRATED BASE params rather than
# re-steadied from its stored post-spin-up state.
#
# WHY. run_single_enhanced_sim (09_Uncertainty_Analysis.Rmd:337-402) perturbs
# exactly three things off the calibrated seed:
#
#   catchability       gear_params$catchability  * lognormal draw
#   gamma              species_params$gamma      * lognormal draw, clamped
#   abundance_scaling  initial_n rows            * lognormal draw, floored at 0.5
#
# then runs steady() -> 118 yr unfished spin-up -> 1841-2010 projection. Scripts 39
# and 40 skipped all of that and re-entered steady() on the STORED post-spin-up
# params, which halves the baleen whale stock and worsens the yield fit in 89% of
# members (docs/SNR_regression_FINDING.md). Recovering the three draws lets us
# replicate the original procedure instead.
#
# THE SOURCE: the ensemble's OWN `parameters` list, not the parameter CSV.
# run_single_enhanced_sim records `abundance_scaling` per accepted member
# (09:521-536) and the ensemble stores it alongside `simulations`. That is the
# authoritative record and it is indexed identically.
#
# WHY NOT monte_carlo_2111_summaries/all_parameters_all_simulations.csv --
# MEASURED, NOT ASSUMED. Two independent defects, both fatal:
#
#   1. The offset is PIECEWISE, not the uniform +1 that was previously recorded.
#      Over all 100 members of 39_params_cache/chunk_001.rds, 58 align at offset 0
#      (up to sim_index 1101) and 42 at offset +1 (from 1296) -- one extra CSV row
#      is inserted between them, which is why there are 2,112 sim_ids for 2,111
#      members. A fixed offset mispairs every member below the break.
#
#   2. Worse, and decisive: gamma is NOT a unique key. 59 of those 100 members
#      match more than one CSV sim_id, and in 36 of them the candidate rows
#      DISAGREE on abundance_scaling (spread up to 2.49x). Ensemble members 446
#      and 1512 are distinct -- they differ in initial_n -- yet BOTH match exactly
#      the CSV pair {446, 1513}, whose two rows carry identical abundance_scaling.
#      The CSV simply does not contain what distinguishes them. (Consistent with
#      set.seed(20250907 + i) being keyed on the draw index: the same draw index
#      re-run under a different abundance_sd gives identical gamma and different
#      abundance scaling.)
#
# So the CSV can support neither the join nor the value. It is not used here.
#
# USAGE
#   Rscript R/wmin_test/43_recover_member_draws.R extract   # loads the 1.86 GB
#                                                           # ensemble ONCE
#   Rscript R/wmin_test/43_recover_member_draws.R build     # validate + assemble
#
# Writes  Output_large_files/wmin_test/43_raw_draws.rds       (extract)
#         Output_large_files/wmin_test/43_member_draws.rds    (build)
#         Output_large_files/wmin_test/43_member_draws_map.csv
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(dplyr)
})

out_dir   <- "Output_large_files/wmin_test"
cache_dir <- file.path(out_dir, "39_params_cache")
raw_path  <- file.path(out_dir, "43_raw_draws.rds")
ENSEMBLE  <- file.path("Output_large_files/monte_carlo_results",
                       "combined_simulation_results/rerun_results",
                       "mc_ensemble_2111_cleaned.rds")
ABUND_FLOOR <- 0.5     # pmax(0.5, .) at 09_Uncertainty_Analysis.Rmd:379,395
GAMMA_TOL   <- 1e-10   # cross-validation tolerance; matches are seen at ~1e-15

mode <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(mode)) mode <- "build"
stopifnot(mode %in% c("extract", "build"))

t0 <- proc.time()

# --------------------------------------------------------------- extract -----
if (mode == "extract") {
  cat("=== Phase 2 (extract): pulling draws from the ensemble ===\n")
  cat("started", format(Sys.time()), "\n")
  cat("loading", basename(ENSEMBLE), "(1.86 GB, takes a few minutes)...\n")
  mc <- readRDS(ENSEMBLE)
  cat("top-level names:", paste(names(mc), collapse = ", "), "\n")
  if (!"parameters" %in% names(mc))
    stop("no `parameters` element -- inspect the object before proceeding")

  sims <- mc$simulations
  # identical validity filter to 39_full_ensemble_paired.R:84-86, so `vidx` maps
  # sim_index -> position in the raw lists exactly as the params cache did
  vidx <- which(vapply(sims, function(x)
    inherits(x, "MizerSim") && !any(is.nan(x@n)) && !any(is.infinite(x@n)),
    logical(1)))
  cat("valid sims:", length(vidx), "of", length(sims), "\n")

  pars <- mc$parameters
  cat("parameters list length:", length(pars),
      "| matches simulations:", length(pars) == length(sims), "\n")
  cat("fields on parameters[[1]]:", paste(names(pars[[1]]), collapse = ", "), "\n")
  if (!"abundance_scaling" %in% names(pars[[1]]))
    stop("`abundance_scaling` is not recorded on the parameters entries")

  keep <- lapply(vidx, function(v) {
    z <- pars[[v]]
    list(sim_id = z$sim_id,
         species = z$species_names,
         gamma = z$gamma_values,
         catchability = z$catchability,
         abundance_scaling = z$abundance_scaling)
  })
  saveRDS(list(vidx = vidx, draws = keep, n_sims = length(sims)), raw_path)
  rm(mc, sims, pars); invisible(gc())
  cat("\nWrote", raw_path, "\n")
  cat("elapsed:", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
  cat("Now: Rscript R/wmin_test/43_recover_member_draws.R build\n")
  quit(save = "no")
}

# ----------------------------------------------------------------- build -----
cat("=== Phase 2 (build): validate and assemble ===\n")
if (!file.exists(raw_path))
  stop("run the `extract` mode first -- ", raw_path, " does not exist")
RAW <- readRDS(raw_path)

dd <- readRDS(file.path(out_dir, "33_dedupe_full.rds"))
members <- dd$dedup$sim_index
cat("distinct members from 33_dedupe_full.rds:", length(members), "\n")
stopifnot(length(RAW$vidx) >= max(members))

# --- cross-validate against the params cache ---------------------------------
# The decisive check: the recorded gamma for member m must equal the gamma
# actually carried by that member's stored params. steady() does not change
# gamma, so agreement to ~1e-15 proves the parameters list is aligned with the
# simulations list and that these draws belong to these members.
files <- sort(list.files(cache_dir, pattern = "^chunk_\\d+\\.rds$",
                         full.names = TRUE))
if (!length(files)) stop("no params cache in ", cache_dir)

want <- setNames(seq_along(members), as.character(members))
draws <- vector("list", length(members)); names(draws) <- as.character(members)
map_rows <- list(); n_done <- 0L; sp_names <- NULL

for (f in files) {
  z <- readRDS(f)
  for (k in seq_along(z$sim_index)) {
    si <- z$sim_index[k]
    if (is.na(want[as.character(si)])) next
    p <- z$params[[k]]
    if (is.null(sp_names)) sp_names <- p@species_params$species
    stopifnot(identical(p@species_params$species, sp_names))

    d <- RAW$draws[[si]]              # RAW$draws is indexed by sim_index
    g_rec <- as.numeric(d$gamma)[match(sp_names, d$species)]
    g_obj <- p@species_params$gamma
    rel_g <- max(abs(g_rec - g_obj) / g_obj)
    if (rel_g > GAMMA_TOL)
      stop("sim_index ", si, ": recorded gamma disagrees with the stored params ",
           "by ", signif(rel_g, 3), " -- the parameters list is NOT aligned")

    ca_rec <- as.numeric(d$catchability)
    ca_obj <- p@gear_params$catchability
    rel_c <- if (length(ca_rec) == length(ca_obj)) {
      nz <- ca_obj > 0
      if (any(nz)) max(abs(ca_rec[nz] - ca_obj[nz]) / ca_obj[nz]) else 0
    } else NA_real_

    as_vec <- setNames(as.numeric(d$abundance_scaling)[match(sp_names, d$species)],
                       sp_names)
    ca <- setNames(ca_obj, paste(p@gear_params$gear, p@gear_params$species))

    draws[[as.character(si)]] <- list(
      sim_index = si, ens_sim_id = d$sim_id,
      gamma = setNames(g_obj, sp_names),     # from the params: full precision
      catchability = ca,                     # from the params: authoritative
      abundance_scaling = as_vec)            # from the record: only source

    map_rows[[length(map_rows) + 1]] <- data.frame(
      sim_index = si, ens_sim_id = d$sim_id,
      gamma_rel_diff = rel_g, catch_rel_diff = rel_c,
      abund_min = min(as_vec), abund_max = max(as_vec),
      n_gear = length(ca), catch_sum = sum(ca),
      stringsAsFactors = FALSE)
    n_done <- n_done + 1L
  }
  rm(z); invisible(gc())
  cat(sprintf("  %s -> %d/%d\n", basename(f), n_done, length(members)))
}

missing <- members[vapply(draws, is.null, logical(1))]
if (length(missing))
  stop(length(missing), " member(s) not found in the params cache: ",
       paste(head(missing, 10), collapse = ", "))

M <- bind_rows(map_rows) %>% arrange(sim_index)
stopifnot(nrow(M) == length(members), !anyDuplicated(M$sim_index))

cat("\n=== cross-validation against the stored params ===\n")
cat("  max gamma relative difference        :", signif(max(M$gamma_rel_diff), 3),
    "  (must be ~1e-15)\n")
cat("  max catchability relative difference :",
    signif(max(M$catch_rel_diff, na.rm = TRUE), 3), "\n")

cat("\n=== abundance_scaling sanity ===\n")
cat("  floor observed (expect exactly 0.5)  :", signif(min(M$abund_min), 12), "\n")
cat("  members whose min hits the floor     :",
    sum(abs(M$abund_min - ABUND_FLOOR) < 1e-12), "of", nrow(M), "\n")
cat("  max scaling observed                 :", signif(max(M$abund_max), 6), "\n")

# The draws must actually distinguish the members the deduplication kept apart.
# 446 and 1512 are the known pair that the parameter CSV could not separate.
chk <- c(446, 1512)
if (all(as.character(chk) %in% names(draws))) {
  a1 <- draws[[as.character(chk[1])]]$abundance_scaling
  a2 <- draws[[as.character(chk[2])]]$abundance_scaling
  cat("\n  members 446 vs 1512 (the pair the CSV could not separate):\n")
  cat("    identical gamma            :", isTRUE(all.equal(
    draws[[as.character(chk[1])]]$gamma,
    draws[[as.character(chk[2])]]$gamma)), "\n")
  cat("    abundance_scaling DIFFERS  :", !isTRUE(all.equal(a1, a2)),
      " (max relative gap ", signif(max(abs(a1 - a2) / pmax(a1, a2)), 4), ")\n", sep = "")
}

if (abs(min(M$abund_min) - ABUND_FLOOR) > 1e-9)
  warning("abundance_scaling floor is ", min(M$abund_min), ", not ", ABUND_FLOOR)

# --- DEDUPLICATE ON THE DRAWS, NOT ON THE OUTCOME ----------------------------
# 33_dedupe_full_and_rmse.R required gamma, catchability AND initial_n to match,
# where initial_n is the POST-SPIN-UP state. Two members carrying the identical
# Monte Carlo draw but run under different steady()/spin-up settings land on
# different post-spin-up states, so that rule kept both. The accepted 2,111 came
# from 33 runs with varying tol/t_max, so this happens often: members 446 and 1512
# have byte-identical gamma, catchability and abundance_scaling, and the pairs
# below share one ensemble sim_id (e.g. 1847/1852 -> sim_id 98).
#
# For a REBUILD that matters, because the same draw applied to the same base with
# the same settings is deterministic: duplicates would produce identical results
# and re-inflate the ensemble with repeated draws -- the exact defect the original
# deduplication set out to remove. Run the draw-distinct set.
draw_key <- vapply(as.character(M$sim_index), function(m) {
  d <- draws[[m]]
  paste(paste(sprintf("%.17g", d$gamma), collapse = "|"),
        paste(sprintf("%.17g", d$catchability), collapse = "|"),
        paste(sprintf("%.17g", d$abundance_scaling), collapse = "|"), sep = "#")
}, character(1))
M$draw_key_id <- match(draw_key, unique(draw_key))
keep <- !duplicated(draw_key)
members_distinct <- M$sim_index[keep]

cat("\n=== deduplication on the DRAWS ===\n")
cat("  members distinct by post-spin-up state (the old rule):", nrow(M), "\n")
cat("  members distinct by DRAW                              :",
    length(members_distinct), "\n")
cat("  further redundant copies                              :",
    nrow(M) - length(members_distinct), "\n")
cat("  implied top 10% = ceiling(", length(members_distinct), "* 0.10) =",
    ceiling(length(members_distinct) * 0.10), "\n")

saveRDS(list(draws = draws, map = M, species = sp_names,
             members_distinct = members_distinct,
             source = list(ensemble = ENSEMBLE, cache = cache_dir,
                           members = "33_dedupe_full.rds")),
        file.path(out_dir, "43_member_draws.rds"))
write.csv(M, file.path(out_dir, "43_member_draws_map.csv"), row.names = FALSE)

cat("\nWrote 43_member_draws.rds (", length(draws), " members)\n", sep = "")
cat("elapsed:", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
