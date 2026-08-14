# =============================================================================
# PHASE 6 -- cut the re-tuned ensemble several ways and compare, so the selection
# rule is chosen on evidence rather than in the abstract.
#
# The yield RMSE the published work selects on barely responds to the baleen whale
# catch: a 7.7x change moves it 0.3% (docs/new_ensemble_44_results.md). And the
# catchability re-fit established that the whale catch cannot be fixed by
# catchability at all -- the observed baleen total is 23.8x the calibrated standing
# stock, so the shortfall traces to the contemporary-biomass calibration target.
# The selection rule therefore has to be chosen knowing what each option buys.
#
# CUTS COMPARED (all from the same run, no extra simulation of new members):
#   A  unweighted pooled log10 yield RMSE, top 10%      -- the published rule
#   B  same, with baleen whale rows weighted x10        -- responds to whales
#   C  gate: baleen catch ratio >= 0.10, then rank by A
#   D  gate: baleen catch ratio >= 0.30, then rank by A
#
# METRICS PER CUT: membership overlap, baleen whale catch, 1841 whale stock, and
# the community biomass SNR at 2010.
#
# THE SNR IS THE CANONICAL ONE (`Plotting scripts/biomass_slope_snr_mean_med.R`,
# make_snr()): numerator median_i(E_i - U_i), denominator the SD ACROSS YEARS
# 1841-2010 of the across-member MEAN UNEXPLOITED trajectory -- a temporal SD of a
# single curve, one scalar, the same at every year. It is NOT the across-member SD
# of the paired differences; that answers a parameter-uncertainty question and must
# never be called SNR in this manuscript. Published value at 2010: -0.920.
#
# USAGE
#   Rscript R/wmin_test/46_selection_cuts.R
# ENV: CUT_CORES (14)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})

out_dir   <- "Output_large_files/wmin_test"
state_dir <- file.path(out_dir, "44_states")
BAL <- "baleen whales"
CORES <- as.integer(Sys.getenv("CUT_CORES", "14"))

RF <- readRDS(file.path(out_dir, "45_refit_results.rds"))
S  <- RF$summary; P <- RF$per_species; M <- RF$M
bal <- P %>% filter(Species == BAL) %>% transmute(sim_index, br = mod_tot / obs_tot)
S <- S %>% left_join(bal, by = "sim_index")
N <- nrow(S); n_top <- ceiling(N * 0.10)
cat("=== re-tuned ensemble:", N, "members | top 10% =", n_top, "===\n\n")

wscore <- function(w) P %>% mutate(wt = ifelse(Species == BAL, w, 1)) %>%
  group_by(sim_index) %>%
  summarise(m = sqrt(sum(wt * sse) / sum(wt * n)), .groups = "drop")

A <- wscore(1); B <- wscore(10)
cuts <- list(
  `A unweighted RMSE`      = A %>% arrange(m) %>% head(n_top) %>% pull(sim_index),
  `B RMSE, baleen x10`     = B %>% arrange(m) %>% head(n_top) %>% pull(sim_index),
  `C gate 0.10 + RMSE`     = A %>% filter(sim_index %in% bal$sim_index[bal$br >= 0.10]) %>%
                              arrange(m) %>% head(n_top) %>% pull(sim_index),
  `D gate 0.30 + RMSE`     = A %>% filter(sim_index %in% bal$sim_index[bal$br >= 0.30]) %>%
                              arrange(m) %>% head(n_top) %>% pull(sim_index))
for (nm in names(cuts)) cat(sprintf("  %-22s n = %d\n", nm, length(cuts[[nm]])))

# --- project the union once, fished and unfished -----------------------------
members <- sort(unique(unlist(cuts)))
cat("\nprojecting the union of all cuts:", length(members), "members, both arms\n")
effort_arr <- readRDS("effort_array_1841_2010.rds")

traj <- function(si, M, qmax = 1) {
  suppressPackageStartupMessages({ library(therMizer); library(mizer) })
  z <- readRDS(file.path(SDIR, sprintf("state_treated_%05d.rds", si)))
  p <- z$params
  gp <- gear_params(p)
  m <- M[match(gp$species, names(M))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(qmax, pmax(0, gp$catchability * m))
  gear_params(p) <- gp
  ex <- try(project(p, initial_n = z$initial_n, t_start = 1841, effort = EFF), silent = TRUE)
  un <- try(project(p, initial_n = z$initial_n, t_start = 1841,
                    t_max = nrow(EFF) - 1, effort = 0), silent = TRUE)
  if (inherits(ex, "try-error") || inherits(un, "try-error")) return(NULL)
  bx <- rowSums(getBiomass(ex)); bu <- rowSums(getBiomass(un))
  n <- min(length(bx), length(bu))
  list(sim_index = si, years = as.numeric(names(bx))[1:n],
       exploited = unname(bx[1:n]), unexploited = unname(bu[1:n]))
}

cl <- makeCluster(CORES)
assign("EFF", effort_arr, envir = environment())
assign("SDIR", state_dir, envir = environment())
clusterExport(cl, c("EFF", "SDIR", "traj"), envir = environment())
# M and qmax as ARGUMENTS: parLapply serialises the function, not the globals it
# closes over (this cost a debugging cycle in 45_catchability_refit.R).
res <- parLapplyLB(cl, members, function(si, MM, QQ)
  tryCatch(traj(si, MM, QQ), error = function(e) NULL), MM = M, QQ = 1)
stopCluster(cl)
res <- res[!vapply(res, is.null, logical(1))]
cat("  projected", length(res), "of", length(members), "\n")
names(res) <- vapply(res, `[[`, numeric(1), "sim_index")
saveRDS(res, file.path(out_dir, "46_trajectories.rds"))

# --- the canonical SNR -------------------------------------------------------
snr_at <- function(sel, year = 2010) {
  r <- res[as.character(sel)]; r <- r[!vapply(r, is.null, logical(1))]
  if (length(r) < 5) return(c(snr = NA, signal = NA, noise = NA))
  yrs <- r[[1]]$years
  E <- do.call(rbind, lapply(r, `[[`, "exploited"))
  U <- do.call(rbind, lapply(r, `[[`, "unexploited"))
  noise  <- sd(colMeans(U))                    # temporal SD of the mean unexploited curve
  signal <- apply(E - U, 2, median)            # per-year median paired difference
  i <- which.min(abs(yrs - year))
  c(snr = signal[i] / noise, signal = signal[i], noise = noise)
}

cat("\n=== comparison ===\n")
out <- do.call(rbind, lapply(names(cuts), function(nm) {
  sel <- cuts[[nm]]
  d <- S %>% filter(sim_index %in% sel)
  s <- snr_at(sel)
  data.frame(cut = nm, n = length(sel),
             median_rmse = signif(median(d$rmse, na.rm = TRUE), 5),
             median_catch = signif(median(d$br, na.rm = TRUE), 4),
             pct_catch_ge_0.3 = round(100 * mean(d$br >= 0.3, na.rm = TRUE), 1),
             biomass_SNR_2010 = signif(unname(s["snr"]), 4),
             signal_2010 = signif(unname(s["signal"]), 4),
             overlap_A = sum(sel %in% cuts[[1]]),
             stringsAsFactors = FALSE)
}))
print(out, row.names = FALSE)
cat("\n  published biomass SNR at 2010: -0.920 (212 members, stored pipeline)\n")
cat("  stored-ranking rebuilds gave -0.9393 (deduped 200) and -0.9432 (clean 184)\n")

saveRDS(list(cuts = cuts, comparison = out, snr_fn = "canonical temporal-SD"),
        file.path(out_dir, "46_selection_cuts.rds"))
write.csv(out, file.path(out_dir, "46_selection_cuts.csv"), row.names = FALSE)
cat("\nWrote 46_selection_cuts.{rds,csv} and 46_trajectories.rds\n")
