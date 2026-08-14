# =============================================================================
# Figure data for the NRMSE_sd top-10% cut (167 members), so the two
# supplementary figures can be redrawn on it.
#
# WHY A SEPARATE EXTRACTION. Nothing cached covers this membership. 49 kept only
# TOTAL biomass (for the SNR) and 48 kept only per-species SSE aggregates;
# 47_cut_yields.rds holds per-year yields but for the 320-member union of the
# log/whale-weighted cuts, of which this cut shares 38 + 34. And
# F00_build_rebuilt167_data.R is hard-wired to cut A -- it asserts that
# membership and would have to be unpicked to reuse. So this projects the 167
# once and emits exactly the two products the figures need, in the SAME schemas
# the existing figure scripts already read:
#
#   Manuscript data/biomass_abund_{fish,clim}_nrmse167.rds
#       sim_index, arm, Year, Species, Biomass, Abundance, MeanMass  (F00 schema,
#       biomass in GRAMS) -- read by F05_supp_biomass_grid_rebuilt167.R
#   Output_large_files/wmin_test/50_cut_yields_nrmse167.rds
#       Year, Species, Yield_t, sim_index  (47 schema, yield in GRAMS despite the
#       column name) -- read by 47_plot_cut_yields.R
#
# Same states, same re-fitted catchability at qmax = 1, same effort array as
# every other phase, so these are comparable with the cut A products by
# construction.
#
# USAGE  Rscript R/wmin_test/50_nrmse167_figure_data.R
# ENV    NR2_CORES (14)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel)
  library(dplyr); library(reshape2)
})

od        <- "Output_large_files/wmin_test"
state_dir <- file.path(od, "44_states")
DATA      <- "Manuscript data"
SUFFIX    <- "nrmse167"
CORES     <- as.integer(Sys.getenv("NR2_CORES", "14"))
QMAX      <- 1

N   <- readRDS(file.path(od, "49_rank_nrmse_sd.rds"))
sel <- N$selected
message("members: ", length(sel), " | objective: ", N$objective)

MULT       <- readRDS(file.path(od, "45_catchability_multipliers.rds"))$M
effort_arr <- readRDS("effort_array_1841_2010.rds")

worker <- function(si, M, qmax) {
  suppressPackageStartupMessages({
    library(therMizer); library(mizer); library(reshape2) })
  z <- readRDS(file.path(SDIR, sprintf("state_treated_%05d.rds", si)))
  p <- z$params
  gp <- gear_params(p)
  m <- M[match(gp$species, names(M))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(qmax, pmax(0, gp$catchability * m))
  gear_params(p) <- gp

  sp <- p@species_params$species
  w  <- p@w; dw <- p@dw; wdw <- w * dw

  ex <- try(project(p, initial_n = z$initial_n, t_start = 1841, effort = EFF),
            silent = TRUE)
  un <- try(project(p, initial_n = z$initial_n, t_start = 1841,
                    t_max = nrow(EFF) - 1, effort = 0), silent = TRUE)
  if (inherits(ex, "try-error") || inherits(un, "try-error"))
    return(list(sim_index = si, ok = FALSE))

  # exploited runs end 2010, unexploited can carry an extra year -- clip both
  tab <- function(s, arm) {
    yr <- as.numeric(dimnames(s@n)$time)
    keep <- which(yr <= 2010)
    do.call(rbind, lapply(keep, function(ti) {
      n  <- s@n[ti, , ]
      bm <- rowSums(sweep(n, 2, wdw, "*"))
      ab <- rowSums(sweep(n, 2, dw,  "*"))
      data.frame(sim_index = si, arm = arm, Year = yr[ti], Species = sp,
                 Biomass = bm, Abundance = ab,
                 MeanMass = ifelse(ab > 0, bm / ab, NA_real_),
                 stringsAsFactors = FALSE)
    }))
  }
  y <- reshape2::melt(getYield(ex))
  names(y) <- c("Year", "Species", "Yield_t")     # GRAMS, as 47 expects
  y$Year <- as.numeric(as.character(y$Year))
  y$Species <- as.character(y$Species)
  y$Yield_t <- pmax(y$Yield_t, 0)
  y$sim_index <- si

  list(sim_index = si, ok = TRUE,
       fish = tab(ex, "exploited"), clim = tab(un, "unexploited"),
       yield = y[y$Year %in% 1900:2010, ])
}

t0 <- proc.time()
cl <- makeCluster(CORES)
assign("EFF",  effort_arr, envir = environment())
assign("SDIR", state_dir,  envir = environment())
clusterExport(cl, c("EFF", "SDIR", "worker"), envir = environment())
res <- parLapplyLB(cl, sel, function(si, MM, QQ)
  tryCatch(worker(si, MM, QQ), error = function(e)
    list(sim_index = si, ok = FALSE, err = conditionMessage(e))), MM = MULT, QQ = QMAX)
stopCluster(cl)
ok <- vapply(res, function(x) isTRUE(x$ok), logical(1))
message("projected ", sum(ok), " of ", length(sel), " in ",
        round((proc.time() - t0)["elapsed"] / 60, 1), " min")
if (any(!ok)) message("FAILED: ", paste(vapply(res[!ok], function(x) x$sim_index,
                                               numeric(1)), collapse = ", "))
res <- res[ok]

dir.create(DATA, showWarnings = FALSE)
saveRDS(bind_rows(lapply(res, `[[`, "fish")),
        file.path(DATA, sprintf("biomass_abund_fish_%s.rds", SUFFIX)))
saveRDS(bind_rows(lapply(res, `[[`, "clim")),
        file.path(DATA, sprintf("biomass_abund_clim_%s.rds", SUFFIX)))
saveRDS(bind_rows(lapply(res, `[[`, "yield")),
        file.path(od, sprintf("50_cut_yields_%s.rds", SUFFIX)))
saveRDS(list(n_members = length(res),
             members = vapply(res, function(x) x$sim_index, numeric(1)),
             n_top = length(sel),
             cut = "NRMSE_sd, equal-weight mean over species, top 10%",
             multipliers = MULT, qmax = QMAX, built = Sys.time()),
        file.path(DATA, sprintf("meta_%s.rds", SUFFIX)))
message("wrote biomass_abund_{fish,clim}_", SUFFIX, ".rds, 50_cut_yields_",
        SUFFIX, ".rds and meta_", SUFFIX, ".rds")