# =============================================================================
# PHASE 6c -- rank the whole accepted ensemble on the RAW-TONNES yield RMSE, with
# no log and no offset, as a counterpart to the published log10(g+1) rule.
#
# WHY THIS NEEDS A RE-PROJECTION. Nothing on disk carries per-year modelled
# yields for the full ensemble. 44_rebuild_results.rds, 44_results/*.rds and
# 45_refit_results.rds all store per-species AGGREGATES -- n, sse, obs_tot,
# mod_tot -- where sse is already summed over years. log10 is invertible; the
# squaring and summing is not, and it happens before anything is written. For
# baleen whales that is four scalars covering 81 years, so the 81 modelled values
# cannot be recovered. 47_cut_yields.rds does hold per-year yields, but only for
# the 320 members that are the union of the four candidate cuts -- a sample
# already conditioned on the log-based rules this is meant to be compared
# against, so it cannot supply an ensemble ranking.
#
# WHY THE METRIC IS WORTH COMPUTING, AND WHY IT IS NOT PROPOSED AS A RULE. The
# published objective prices a zero-catch year at log10(1 g) = 0, so a modelled
# 14 t in a year reported as zero scores 7.15 log units. Only three groups have
# observed-zero rows (squids, shelf and coastal, toothfishes) and those rows
# carry 88-96% of their SSE; move the offset from 1 g to 1 t and the per-species
# ranking inverts. Raw tonnes removes the offset question entirely -- and
# introduces the opposite bias, since a squared error in tonnes is dominated by
# whichever fishery is largest. Both rankings are written out; the comparison is
# the point, not a replacement.
#
# WHAT IS HELD FIXED. Everything except the error measure: the same 1668 accepted
# members, the same cached post-spin-up states, the same re-fitted catchability
# multipliers at qmax = 1 (45_catchability_multipliers.rds), the same effort
# array, observed series and per-species fitting windows.
#
# ROWS. Restricted to effort > 0 (257 of the 344 window rows). The 87 gap rows
# have modelled = observed = 0 exactly, so they contribute nothing to either SSE
# and change only the divisor.
#
# VERIFICATION. Each member's per-species log10(g+1) SSE over the full window is
# recomputed here and compared against 45_refit_results.rds. That is an
# independent path to the same quantity -- if the projection, the multipliers or
# the row set were wrong it would not match -- and the script refuses to write a
# ranking if it does not.
#
# COST. 28 members took 24.2 s on 14 cores, so 1668 is ~24 min.
#
# USAGE   Rscript R/wmin_test/48_rank_raw_tonnes.R          # project + collect
#         Rscript R/wmin_test/48_rank_raw_tonnes.R collect  # collect only
# ENV     RANK_CORES (14 -- leave two free; saturating 16 risks a hard shutdown)
#         RANK_CHUNK (100), RANK_LIMIT (0 = all)
# OUT     Output_large_files/wmin_test/48_raw_tonnes_chunks/*.rds
#         Output_large_files/wmin_test/48_rank_raw_tonnes.{rds,csv}
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel)
  library(dplyr); library(reshape2)
})

od        <- "Output_large_files/wmin_test"
state_dir <- file.path(od, "44_states")
work_dir  <- file.path(od, "48_raw_tonnes_chunks")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
QMAX <- 1

CORES <- as.integer(Sys.getenv("RANK_CORES", "14"))
CHUNK <- as.integer(Sys.getenv("RANK_CHUNK", "100"))
LIMIT <- as.integer(Sys.getenv("RANK_LIMIT", "0"))
mode  <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"

# --- the fitting rows ---------------------------------------------------------
effort_arr <- readRDS("effort_array_1841_2010.rds")
eff_long <- reshape2::melt(effort_arr, varnames = c("Year", "Species"),
                           value.name = "effort") %>%
  mutate(Year = as.numeric(as.character(Year)), Species = as.character(Species))
ew <- eff_long %>% filter(effort > 0) %>% group_by(Species) %>%
  summarise(first_year = min(Year), last_year = max(Year), .groups = "drop")

OBS <- read.csv("yield_observed_timeseries.csv") %>%
  reshape2::melt(id.vars = "Year", variable.name = "Species", value.name = "obs_g") %>%
  mutate(Species = gsub("\\.", " ", as.character(Species)),
         obs_g = pmax(coalesce(as.numeric(obs_g), 0), 0)) %>%
  inner_join(ew, by = "Species") %>%
  filter(Year >= first_year, Year <= last_year) %>%
  group_by(Species) %>% filter(sum(obs_g) > 0) %>% ungroup() %>%
  left_join(eff_long, by = c("Year", "Species")) %>%
  select(Year, Species, obs_g, effort)
message("fitting rows: ", nrow(OBS), " | effort-on: ", sum(OBS$effort > 0),
        " | species: ", n_distinct(OBS$Species))

MULT <- readRDS(file.path(od, "45_catchability_multipliers.rds"))$M
RF   <- readRDS(file.path(od, "45_refit_results.rds"))
members <- sort(unique(RF$summary$sim_index))
if (LIMIT > 0) members <- head(members, LIMIT)
message("members: ", length(members), " | cores: ", CORES)

# ------------------------------------------------------------------- worker ---
# One row per species: raw-tonnes SSE over effort-on rows, and log10(g+1) SSE
# over both row sets. The window one is the verification hook.
score <- function(si, M, qmax) {
  suppressPackageStartupMessages({
    library(therMizer); library(mizer); library(dplyr); library(reshape2) })
  z <- readRDS(file.path(SDIR, sprintf("state_treated_%05d.rds", si)))
  p <- z$params
  gp <- gear_params(p)
  m <- M[match(gp$species, names(M))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(qmax, pmax(0, gp$catchability * m))
  gear_params(p) <- gp

  pr <- try(project(p, initial_n = z$initial_n, t_start = 1841, effort = EFF),
            silent = TRUE)
  if (inherits(pr, "try-error")) return(NULL)
  d <- reshape2::melt(getYield(pr))            # GRAMS
  names(d) <- c("Year", "Species", "mod_g")
  d$Year <- as.numeric(as.character(d$Year))
  d$Species <- as.character(d$Species)
  d$mod_g <- pmax(d$mod_g, 0)

  cmp <- ROWS %>% left_join(d, by = c("Year", "Species")) %>%
    mutate(mod_g = coalesce(mod_g, 0),
           on    = effort > 0,
           r_log = log10(mod_g + 1) - log10(obs_g + 1),
           r_t   = (mod_g - obs_g) / 1e6)
  cmp %>% group_by(Species) %>%
    summarise(n_win = n(), n_on = sum(on),
              sse_log_win = sum(r_log^2),
              sse_log_on  = sum(r_log[on]^2),
              sse_t_on    = sum(r_t[on]^2),
              obs_tot_t   = sum(obs_g) / 1e6,
              mod_tot_t   = sum(mod_g) / 1e6,
              .groups = "drop") %>%
    mutate(sim_index = si)
}

# ---------------------------------------------------------------------- run ---
if (mode != "collect") {
  chunks <- split(members, ceiling(seq_along(members) / CHUNK))
  t0 <- proc.time()
  for (ci in names(chunks)) {
    rf <- file.path(work_dir, sprintf("chunk_%03d.rds", as.integer(ci)))
    if (file.exists(rf)) next
    cl <- makeCluster(CORES)
    assign("EFF",  effort_arr, envir = environment())
    assign("SDIR", state_dir,  envir = environment())
    assign("ROWS", OBS,        envir = environment())
    clusterExport(cl, c("EFF", "SDIR", "ROWS", "score"), envir = environment())
    # M and qmax as ARGUMENTS: parLapply serialises the function, not the globals
    # it closes over (this cost a debugging cycle in 45_catchability_refit.R).
    r <- parLapplyLB(cl, chunks[[ci]], function(si, MM, QQ)
      tryCatch(score(si, MM, QQ), error = function(e) NULL), MM = MULT, QQ = QMAX)
    stopCluster(cl)
    saveRDS(bind_rows(r), rf)
    nd <- length(list.files(work_dir, pattern = "^chunk_"))
    el <- (proc.time() - t0)["elapsed"] / 60
    message(sprintf("chunk %s (%d/%d) | ok %d/%d | %.1f min elapsed | ETA %.1f min",
                    ci, nd, length(chunks),
                    sum(!vapply(r, is.null, logical(1))), length(r), el,
                    el / nd * (length(chunks) - nd)))
  }
  message("projection complete in ",
          round((proc.time() - t0)["elapsed"] / 60, 1), " min")
}

# ------------------------------------------------------------------ collect ---
fs <- sort(list.files(work_dir, pattern = "^chunk_\\d+\\.rds$", full.names = TRUE))
stopifnot(length(fs) > 0)
PS <- bind_rows(lapply(fs, readRDS))
message("collected ", n_distinct(PS$sim_index), " members, ", nrow(PS), " rows")

# --- verification against 45_refit_results ------------------------------------
chk <- RF$per_species %>%
  select(sim_index, Species, n_ref = n, sse_ref = sse) %>%
  inner_join(PS %>% select(sim_index, Species, n_win, sse_log_win),
             by = c("sim_index", "Species"))
rel <- max(abs(chk$sse_log_win - chk$sse_ref) / pmax(chk$sse_ref, 1e-12))
message(sprintf("VERIFY vs 45_refit_results: %d rows | n identical %s | max rel diff %.3g",
                nrow(chk), identical(chk$n_win, chk$n_ref), rel))
if (!identical(chk$n_win, chk$n_ref) || rel > 1e-8)
  stop("verification failed -- refusing to publish a ranking from these yields")

# --- the rankings -------------------------------------------------------------
# fold_err = 10^RMSE_log, the geometric-mean factor the model is wrong by. It is
# a re-expression of the log metric, NOT a third ranking -- identical order.
RANK <- PS %>% group_by(sim_index) %>%
  summarise(rmse_t      = sqrt(sum(sse_t_on)    / sum(n_on)),
            rmse_log    = sqrt(sum(sse_log_win) / sum(n_win)),
            rmse_log_on = sqrt(sum(sse_log_on)  / sum(n_on)),
            .groups = "drop") %>%
  mutate(fold_err = 10^rmse_log,
         rank_t   = rank(rmse_t,   ties.method = "first"),
         rank_log = rank(rmse_log, ties.method = "first")) %>%
  arrange(rank_t)

saveRDS(list(per_species = PS, rank = RANK, rows = OBS, mult = MULT, qmax = QMAX),
        file.path(od, "48_rank_raw_tonnes.rds"))
write.csv(RANK, file.path(od, "48_rank_raw_tonnes.csv"), row.names = FALSE)
message("wrote 48_rank_raw_tonnes.{rds,csv}")