# =============================================================================
# Phase 94 -- PILOT: project to 2100 under repeated climate, with and without
# continued exploitation
#
# THE QUESTION. On the phase-88 ensemble whale recruitment now tracks the stock
# far more closely than before -- minke sits essentially on the 1:1
# stock-recruitment line, and baleen's recruitment-per-biomass ratio fell from
# ~1.8 to 1.27. Yet baleen biomass has been FLAT at 0.60 of its 1841 level since
# about 1960 (0.605 in 1980, 0.605 in 1990, 0.603 in 2010: -0.27% over the last
# two decades) despite whaling having effectively stopped. Better compensation
# has not produced recovery.
#
# Two readings, and they are distinguishable:
#   (a) SLOW DYNAMICS -- recovery is happening but on a timescale longer than
#       the 1841-2010 record. Then a century of zero fishing should show it.
#   (b) AN ALTERNATIVE STATE -- the depleted configuration is itself an
#       equilibrium, held there by whatever replaced the whales on the krill.
#       Then zero fishing changes nothing and the flat line continues.
#
# ------------------------------------------------------------------ the arms
#   none  ALL gears at zero effort from 2011
#   hold  every gear held at its own MEAN 2001-2010 effort from 2011
#
# 2001-2010 is the reference decade for both the climate repeat and the effort
# hold, so the two are consistent: "the recent decade, continued".
#
# ------------------------------------------- THE FORCING TRAP, why this exists
# therMizer does NOT error on years outside its forcing record. Both
# scaled_temp_effect() and plankton_forcing() open with
#
#     if (!floor(t) %in% <years>) t <- t %% <last year> + <first year>
#
# so projecting to 2011-2100 against the stock 1841-2010 arrays would SILENTLY
# wrap to 1842-1931 -- the early historical climate, not a repeat of the recent
# one, and no warning. This script therefore EXTENDS the arrays explicitly so
# every projected year has its own label and the wrap can never fire.
#
# plankton_forcing() takes its row index from dimnames(ocean_temp), not from
# n_pp_array's own dimnames, so the two arrays MUST be extended to identical
# length and identical year labels. Asserted below.
#
# --------------------------------------------------------------- exactness
# Members are re-projected from their stored post-spin-up state, which is
# UNFISHED, so catchability cannot touch the initial condition and the
# 1841-2010 segment reproduces the phase-88 run exactly. The re-run trap that
# afflicts steady() does not apply.
#
# USAGE  Rscript R/wmin_test/94_future_projection.R [run|collect]
# ENV    P94_STEM, P94_N, P94_CORES, P94_REF_FROM, P94_REF_TO, P94_END,
#        P94_MULT, P94_OUT
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OL <- "Output_large_files/wmin_test"
STEM   <- Sys.getenv("P94_STEM", "88_full")
N      <- as.integer(Sys.getenv("P94_N", "20"))          # pilot default
CORES  <- min(as.integer(Sys.getenv("P94_CORES", "10")),
              max(1L, parallel::detectCores() - 2L))
REF_FROM <- as.integer(Sys.getenv("P94_REF_FROM", "2001"))
REF_TO   <- as.integer(Sys.getenv("P94_REF_TO", "2010"))
END      <- as.integer(Sys.getenv("P94_END", "2100"))
QMAX <- 1
STATE_DIR <- file.path(OL, paste0(STEM, "_states"))
OUT <- Sys.getenv("P94_OUT", file.path(OL, sprintf("94_future_%s.rds", STEM)))
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"

SRC <- readRDS(file.path(OL, paste0(STEM, ".rds")))
MEM <- SRC$members
adm <- if ("n_erepro_ge1" %in% names(MEM)) MEM$n_erepro_ge1 == 0 else TRUE
usable <- as.integer(MEM$sim_index[MEM$stable & adm])
cat("=== Phase 94: projection to", END, "===\n")
cat("stem:", STEM, "| built", nrow(MEM), "-> usable", length(usable), "\n")

# Rank by the corrected yield fit so a pilot takes the best-fitting members
# rather than an arbitrary slice.
RR_F <- file.path(OL, "93_rerank_p88.rds")
if (file.exists(RR_F)) {
  RK <- readRDS(RR_F)$ranking
  usable <- as.integer(RK$sim_index[RK$sim_index %in% usable])
  cat("  ordered by the phase-93 ranking\n")
}
members <- head(usable, min(N, length(usable)))
cat("  members this run:", length(members), "\n")

MULT <- readRDS(Sys.getenv("P94_MULT",
  file.path(OL, "89_refit_results.rds")))$M

# --- effort: observed to 2010, then the arm ----------------------------------
eff_obs <- readRDS("effort_array_1841_2010.rds")
yrs_obs <- as.numeric(rownames(eff_obs))
SPN <- colnames(eff_obs)
fut_yrs <- (max(yrs_obs) + 1):END
ref_rows <- yrs_obs >= REF_FROM & yrs_obs <= REF_TO
hold_vec <- colMeans(eff_obs[ref_rows, , drop = FALSE])
mk_eff <- function(future_row) {
  m <- rbind(eff_obs,
             matrix(rep(future_row, each = length(fut_yrs)),
                    nrow = length(fut_yrs), dimnames = list(fut_yrs, SPN)))
  rownames(m) <- c(yrs_obs, fut_yrs); m
}
ZERO <- setNames(rep(0, length(SPN)), SPN)
# THE THIRD ARM IS THE CONTROL, and without it the other two cannot be read.
# `none` and `hold` both carry the observed 1841-2010 fishing, so a statement
# like "baleen is at 0.303 of its 1841 level" mixes whaling with 170 years of
# climate change plus the repeated future. `unfished` is zero effort for the
# WHOLE 1841-2100 span under the same climate, so it isolates the climate path
# and supplies the contemporaneous denominator: how much recovery is available
# is (unfished - none), not (1841 - none).
# KRILL ARM. `hold` carries essentially NO krill fishing, because the fishery
# ended in 1996 and mean 2001-2010 krill effort is exactly 0 -- so nothing in
# the other arms tests a krill fishery at all. `krill_era` is `hold` with krill
# put back at its own mean over the years it actually operated (1974-1996, mean
# 0.10947, against a peak of 1.0). Everything else stays at the `hold` level, so
# the contrast against `hold` isolates the krill fishery on top of a continuation
# of present fishing rather than confounding it with the other gears.
KRILL <- "antarctic krill"
kr_eff <- eff_obs[, KRILL]
era_mean <- mean(kr_eff[kr_eff > 0])
krill_vec <- hold_vec; krill_vec[KRILL] <- era_mean

EFF <- list(unfished = { m <- mk_eff(ZERO); m[] <- 0; m },
            none = mk_eff(ZERO),
            hold = mk_eff(hold_vec),
            krill_era = mk_eff(krill_vec))
stopifnot(hold_vec[[KRILL]] == 0, era_mean > 0)
stopifnot(all(EFF$unfished == 0),
          # `none` must still carry the historical fishery, or it is not a
          # "stop in 2011" arm at all
          any(EFF$none[as.numeric(rownames(EFF$none)) <= 2010, ] > 0),
          all(EFF$none[as.numeric(rownames(EFF$none)) > 2010, ] == 0))
cat(sprintf("\neffort arms: unfished (0 throughout) | none (0 from %d) | hold (mean %d-%d) | krill_era\n",
            min(fut_yrs), REF_FROM, REF_TO))
print(round(hold_vec[hold_vec > 0], 5))
cat(sprintf("  krill_era: %s at %.5f (mean over %d-%d, its %d operating years; peak %.3f)\n",
            KRILL, era_mean, min(yrs_obs[kr_eff > 0]), max(yrs_obs[kr_eff > 0]),
            sum(kr_eff > 0), max(kr_eff)))

# --- climate: repeat the reference decade ------------------------------------
# EXPLICIT extension, so therMizer's silent wrap to t %% 2010 + 1841 can never
# fire. Both arrays get identical year labels because plankton_forcing() indexes
# n_pp_array using dimnames(ocean_temp).
extend_forcing <- function(p) {
  o <- p@other_params$other
  ot <- o$ocean_temp; npp <- o$n_pp_array
  oyr <- as.numeric(rownames(ot))
  stopifnot(identical(oyr, as.numeric(rownames(npp))))
  src <- which(oyr >= REF_FROM & oyr <= REF_TO)
  if (!length(src)) stop("reference window not in the forcing record",
                         call. = FALSE)
  idx <- src[((seq_along(fut_yrs) - 1) %% length(src)) + 1]
  ot2  <- rbind(ot,  ot[idx, , drop = FALSE])
  npp2 <- rbind(npp, npp[idx, , drop = FALSE])
  rownames(ot2) <- rownames(npp2) <- c(oyr, fut_yrs)
  stopifnot(nrow(ot2) == nrow(npp2),
            identical(rownames(ot2), rownames(npp2)),
            all(fut_yrs %in% as.numeric(rownames(ot2))))
  p@other_params$other$ocean_temp <- ot2
  p@other_params$other$n_pp_array <- npp2
  p
}

worker <- function(si) {
  suppressPackageStartupMessages({library(mizer); library(therMizer)})
  z <- readRDS(file.path(STATE_DIR, sprintf("state_%05d.rds", si)))
  p <- z$params
  gp <- gear_params(p)
  m <- MULT[match(gp$species, names(MULT))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
  gear_params(p) <- gp
  p <- extend_forcing(p)
  sp <- p@species_params$species; wdw <- p@w * p@dw
  out <- lapply(names(EFF), function(arm) {
    s <- try(project(p, initial_n = z$initial_n, t_start = 1841,
                     effort = EFF[[arm]], progress_bar = FALSE), silent = TRUE)
    if (inherits(s, "try-error")) return(NULL)
    yr <- as.numeric(dimnames(s@n)$time)
    do.call(rbind, lapply(seq_along(yr), function(ti) {
      n <- s@n[ti, , ]
      data.frame(sim_index = si, arm = arm, Year = yr[ti], Species = sp,
                 Biomass = rowSums(sweep(n, 2, wdw, "*")),
                 Abundance = rowSums(sweep(n, 2, p@dw, "*")),
                 stringsAsFactors = FALSE)
    }))
  })
  if (any(vapply(out, is.null, logical(1))))
    return(list(sim_index = si, ok = FALSE))
  list(sim_index = si, ok = TRUE, biomass = do.call(rbind, out))
}

if (mode == "run") {
  cat("\nprojecting", length(members), "members x", length(EFF), "arms to", END,
      "on", CORES, "cores\n")
  t0 <- proc.time()
  cl <- makeCluster(CORES)
  on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
  clusterExport(cl, c("STATE_DIR", "MULT", "QMAX", "EFF", "extend_forcing",
                      "REF_FROM", "REF_TO", "fut_yrs", "worker"),
                envir = environment())
  res <- parLapplyLB(cl, members, function(si)
    tryCatch(worker(si), error = function(e)
      list(sim_index = si, ok = FALSE, err = conditionMessage(e))))
  stopCluster(cl)
  ok <- vapply(res, function(r) isTRUE(r$ok), logical(1))
  cat("projected", sum(ok), "of", length(res), "|",
      round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")
  if (any(!ok)) for (r in res[!ok])
    cat("  FAILED", r$sim_index, ":", if (is.null(r$err)) "project error" else r$err, "\n")
  B <- do.call(rbind, lapply(res[ok], `[[`, "biomass"))
  saveRDS(list(biomass = B, members = members[ok],
               meta = list(stem = STEM, end = END, ref = c(REF_FROM, REF_TO),
                           hold_effort = hold_vec, multipliers = MULT,
                           climate = sprintf("repeat %d-%d", REF_FROM, REF_TO),
                           built = Sys.time())), OUT)
  cat("WROTE", OUT, "\n")
}