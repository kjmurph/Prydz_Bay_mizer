# =============================================================================
# PHASE 6d -- rank the accepted ensemble on a BALANCED multi-species objective:
# the equal-weight mean across species of the per-species NRMSE, normalised by
# the standard deviation of that species' observed catch.
#
#   NRMSE_sd(species) = sqrt( SSE_tonnes / n ) / sd(observed tonnes)
#   objective(member) = mean over the 9 fished species of NRMSE_sd
#
# WHY. The published log10(g+1) rule and the raw-tonnes rule are both dominated
# by a minority of the data, in opposite directions -- median within-member SSE
# share is 36% toothfishes / 35% shelf-and-coastal under the log rule, and 99%
# baleen whales under raw tonnes. Dividing each species' RMSE by the SD of its
# own observations makes the nine terms dimensionless and comparable, so they can
# be averaged without one swamping the rest. Shares under this objective run
# 4.8% to 18.9%.
#
# NO RE-PROJECTION IS NEEDED. The normalising constant is a per-species scalar
# that does not vary across years, and a constant commutes with the sum of
# squares, so NRMSE = sqrt(SSE/n)/s exactly. The per-year comparison was already
# made and stored as sse_t_on by 48_rank_raw_tonnes.R. (This is precisely what
# the log metric could NOT do -- log10 is applied per year, before squaring, and
# a nonlinear per-observation map does not commute with the aggregation.)
#
# WHAT THIS OBJECTIVE DOES NOT DO. It does not improve the whale catch: dividing
# by each species' own SD removes exactly the scale that made baleen whales
# visible to the raw-tonnes rule. Median baleen modelled/observed comes out at
# 0.029 against cut A's 0.041. And NSE is negative for six of nine groups under
# every one of these rules -- the model has no squared-error skill against the
# observed mean for those groups. Balanced is not the same as good.
#
# SNR. The canonical one, as R/wmin_test/46_selection_cuts.R and
# Plotting scripts/biomass_slope_snr_mean_med.R: numerator median_i(E_i - U_i),
# denominator the SD ACROSS YEARS 1841-2010 of the across-member MEAN UNEXPLOITED
# total-biomass trajectory -- a temporal SD of one curve, one scalar, the same at
# every year. NOT the across-member SD of the paired differences.
#
# USAGE   Rscript R/wmin_test/49_rank_nrmse_sd.R
# ENV     NR_CORES (14), NR_SNR (1|0 -- skip the both-arm projection)
# OUT     Output_large_files/wmin_test/49_rank_nrmse_sd.{rds,csv}
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})

od <- "Output_large_files/wmin_test"
state_dir <- file.path(od, "44_states")
CORES  <- as.integer(Sys.getenv("NR_CORES", "14"))
DO_SNR <- as.integer(Sys.getenv("NR_SNR", "1")) == 1

Z    <- readRDS(file.path(od, "48_rank_raw_tonnes.rds"))
PS   <- Z$per_species          # per species per member: n_on, sse_t_on, ...
ROWS <- Z$rows                 # Year, Species, obs_g, effort
cutA <- readRDS(file.path(od, "46_selection_cuts.rds"))$cuts[["A unweighted RMSE"]]
NTOP <- length(cutA)

# --- observation-side scale, effort-on rows, tonnes ---------------------------
SC <- ROWS %>% filter(effort > 0) %>% mutate(obs_t = obs_g / 1e6) %>%
  group_by(Species) %>%
  summarise(n_obs = n(), sd_obs = sd(obs_t),
            ss_tot = sum((obs_t - mean(obs_t))^2), .groups = "drop")
message("species: ", nrow(SC), " | rows: ", sum(SC$n_obs))

NR <- PS %>% select(sim_index, Species, n_on, sse_t_on, obs_tot_t, mod_tot_t) %>%
  left_join(SC, by = "Species") %>%
  mutate(rmse_t  = sqrt(sse_t_on / n_on),
         nrmse_sd = rmse_t / sd_obs,
         nse      = 1 - sse_t_on / ss_tot)

# --- the objective, and two sensitivities ------------------------------------
# mean is the headline. median pools more robustly against one bad species;
# dropping bathypelagic fishes tests the n = 2 group, whose SD rests on two
# observations yet carries a full ninth of the weight.
OBJ <- NR %>% group_by(sim_index) %>%
  summarise(obj      = mean(nrmse_sd),
            obj_med  = median(nrmse_sd),
            obj_nobp = mean(nrmse_sd[Species != "bathypelagic fishes"]),
            .groups = "drop") %>%
  mutate(rank_nrmse = rank(obj, ties.method = "first")) %>%
  arrange(rank_nrmse)

RANK <- Z$rank %>% select(sim_index, rmse_t, rank_t, rmse_log, rank_log) %>%
  left_join(OBJ, by = "sim_index") %>% arrange(rank_nrmse)
sel <- RANK$sim_index[RANK$rank_nrmse <= NTOP]

message("top-", NTOP, " selected | overlap with cut A: ",
        length(intersect(sel, cutA)),
        " | with the raw-tonnes cut: ",
        length(intersect(sel, Z$rank$sim_index[Z$rank$rank_t <= NTOP])))
message("pooling sensitivity -- membership shared with the mean-pooled cut: median ",
        length(intersect(sel, RANK$sim_index[rank(RANK$obj_med,  ties.method = "first") <= NTOP])),
        " | excl. bathypelagic ",
        length(intersect(sel, RANK$sim_index[rank(RANK$obj_nobp, ties.method = "first") <= NTOP])))

# --- the canonical biomass SNR at 2010 for the new cut ------------------------
snr <- NULL
if (DO_SNR) {
  message("projecting the selected ", length(sel), " members, both arms...")
  MULT <- readRDS(file.path(od, "45_catchability_multipliers.rds"))$M
  effort_arr <- readRDS("effort_array_1841_2010.rds")
  traj <- function(si, M, qmax = 1) {
    suppressPackageStartupMessages({ library(therMizer); library(mizer) })
    z <- readRDS(file.path(SDIR, sprintf("state_treated_%05d.rds", si)))
    p <- z$params
    gp <- gear_params(p)
    m <- M[match(gp$species, names(M))]; m[is.na(m)] <- 1
    gp$catchability <- pmin(qmax, pmax(0, gp$catchability * m))
    gear_params(p) <- gp
    ex <- try(project(p, initial_n = z$initial_n, t_start = 1841, effort = EFF),
              silent = TRUE)
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
  res <- parLapplyLB(cl, sel, function(si, MM, QQ)
    tryCatch(traj(si, MM, QQ), error = function(e) NULL), MM = MULT, QQ = 1)
  stopCluster(cl)
  res <- res[!vapply(res, is.null, logical(1))]
  message("  projected ", length(res), " of ", length(sel))

  yrs <- res[[1]]$years
  E <- do.call(rbind, lapply(res, `[[`, "exploited"))
  U <- do.call(rbind, lapply(res, `[[`, "unexploited"))
  noise  <- sd(colMeans(U))
  signal <- apply(E - U, 2, median)
  i <- which.min(abs(yrs - 2010))
  snr <- c(snr = signal[i] / noise, signal = signal[i], noise = noise,
           n = length(res))
  message(sprintf("biomass SNR at 2010: %.4f (signal %.4g, noise %.4g)",
                  snr["snr"], snr["signal"], snr["noise"]))
  message("  cut A, same definition, from 46_selection_cuts.csv: -0.1465")
}

saveRDS(list(per_species = NR, rank = RANK, selected = sel, scale = SC,
             snr_2010 = snr, n_top = NTOP,
             objective = "equal-weight mean over species of RMSE_tonnes / sd(observed tonnes), effort-on rows"),
        file.path(od, "49_rank_nrmse_sd.rds"))
write.csv(RANK, file.path(od, "49_rank_nrmse_sd.csv"), row.names = FALSE)
message("wrote 49_rank_nrmse_sd.{rds,csv}")