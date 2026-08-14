# =============================================================================
# PHASE 7 (TRIAL) -- per-member catchability calibration on cut D (31 members).
#
# WHAT THIS TESTS. The global per-species re-fit (45) moves the CENTRE of the
# catchability distribution and keeps the Monte Carlo spread. This instead gives
# every member its own optimum -- catchability stops being a sampled uncertainty
# and becomes a fitted outcome. Run as a trial before deciding whether to adopt it.
#
# THE PREDICTION TO CHECK, from docs/catch_fit_RESULTS.md §5 (7 members of the OLD
# ensemble): per-member optimisation makes members nearly indistinguishable. All 7
# beat the best of 1,997 random draws (1.5389); the worst, 1.5070, would rank
# FIRST in the published ranking; ranks 6, 8 and 16 optimised to 1.399/1.405/1.419.
# Seven different starting vectors converged to the SAME per-species error.
#
# If that reproduces here, per-member calibration does not solve the selection
# problem -- it DISSOLVES it, by removing the variation the top-10% cut reads.
# That is the result to look for, not the RMSE improvement.
#
# WHY THE OLD NUMBERS DO NOT TRANSFER. catch_fit ran on
# top10pct_rmse_ensembles.rds, whose whale stocks are ~13x this ensemble's. It
# found the model OVER-catches sperm whales by 3.2x; here they are UNDER-caught by
# 17x. Same method, different regime -- re-derive, do not assume.
#
# METHOD. Coordinate descent on log10(q) over the 9 fished gears, warm-started
# from the GLOBAL fit (45_catchability_multipliers.rds applied to each member's
# own drawn q), refined at +-0.5, +-0.25, +-0.125 dex, 5 points per coordinate per
# sweep. Final resolution ~0.06 dex, about 15% in q. Objective is the existing one,
# unchanged: RMSE of log10(g+1) modelled vs observed yield, each species only
# within its own effort window.
#
# Box q in (0, QCAP], QCAP = 1 by default -- the pipeline's own clamp
# (09:347,351) and catch_fit's admissible box. Gears whose optimum lands ON the
# cap are reported: there the box binds and the fit cannot distinguish "out of
# reach of the clamp" from "unattainable at any q".
#
# CHEAP AND EXACT: the spin-up is unfished, so catchability enters only the
# projection and re-projecting a cached state is bit-exact.
#
# USAGE  Rscript R/wmin_test/48_per_member_q.R
# ENV    Q_CORES (14), Q_CAP (1), Q_CUT ("D gate 0.30 + RMSE")
# COST   ~140 evaluations/member at ~13 core-s -> ~30 core-min/member
#        31 members ~= 15 core-hours ~= 65 min on 14 cores.
#        (A further 2x is available by caching 1841-1929, which is
#        catchability-independent -- not implemented here, see catch_fit/01.)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel)
  library(dplyr); library(reshape2)
})

out_dir   <- "Output_large_files/wmin_test"
state_dir <- file.path(out_dir, "44_states")
CORES <- as.integer(Sys.getenv("Q_CORES", "14"))
QCAP  <- as.numeric(Sys.getenv("Q_CAP", "1"))
CUT   <- Sys.getenv("Q_CUT", "D gate 0.30 + RMSE")
REFINE <- c(0.5, 0.25, 0.125)      # dex half-widths, one per sweep
NPT    <- 5                        # points per coordinate per sweep

CUTS <- readRDS(file.path(out_dir, "46_selection_cuts.rds"))$cuts
if (!CUT %in% names(CUTS)) stop("unknown cut: ", CUT, " | have: ",
                                paste(names(CUTS), collapse = " | "))
members <- CUTS[[CUT]]
LIMIT <- as.integer(Sys.getenv("Q_LIMIT", "0"))
if (LIMIT > 0) members <- head(members, LIMIT)
M_glob  <- readRDS(file.path(out_dir, "45_catchability_multipliers.rds"))$M
effort_arr <- readRDS("effort_array_1841_2010.rds")

ew <- do.call(rbind, lapply(colnames(effort_arr), function(s) {
  y <- as.numeric(rownames(effort_arr))[effort_arr[, s] > 0]
  if (length(y)) data.frame(Species = s, first_year = min(y), last_year = max(y),
                            stringsAsFactors = FALSE)
}))
obs_long <- read.csv("yield_observed_timeseries.csv") %>%
  reshape2::melt(id.vars = "Year", variable.name = "Species", value.name = "Yield_g") %>%
  mutate(Species = gsub("\\.", " ", as.character(Species)),
         Yield_g = pmax(coalesce(as.numeric(Yield_g), 0), 0)) %>%
  left_join(ew, by = "Species") %>%
  filter(!is.na(first_year), Year >= first_year, Year <= last_year) %>%
  select(Year, Species, Yield_obs = Yield_g)
FIT_SP <- obs_long %>% group_by(Species) %>% summarise(o = sum(Yield_obs)) %>%
  filter(o > 0) %>% pull(Species)

cat("=== per-member catchability calibration (TRIAL) ===\n")
cat("cut:", CUT, "|", length(members), "members | cores", CORES, "| q cap", QCAP, "\n")
cat("gears fitted:", length(FIT_SP), "->", paste(FIT_SP, collapse = ", "), "\n\n")

# ------------------------------------------------------------------ worker ----
optimise_member <- function(si) {
  suppressPackageStartupMessages({
    library(therMizer); library(mizer); library(dplyr); library(reshape2)
  })
  z <- readRDS(file.path(SDIR, sprintf("state_treated_%05d.rds", si)))
  p0 <- z$params; init <- z$initial_n
  gp0 <- gear_params(p0)
  idx <- which(gp0$species %in% FITSP & gp0$catchability > 0)
  if (!length(idx)) return(NULL)

  # objective: the existing yield RMSE, evaluated at a catchability vector
  evaluate <- function(qvec) {
    p <- p0
    gp <- gear_params(p); gp$catchability <- qvec; gear_params(p) <- gp
    pr <- try(project(p, initial_n = init, t_start = 1841, effort = EFF), silent = TRUE)
    if (inherits(pr, "try-error")) return(list(rmse = Inf, per_sp = NULL))
    d <- reshape2::melt(getYield(pr)); names(d) <- c("Year", "Species", "Yield_mod")
    d$Year <- as.numeric(as.character(d$Year)); d$Species <- as.character(d$Species)
    d$Yield_mod <- pmax(d$Yield_mod, 0)
    cmp <- OBS %>% left_join(d, by = c("Year", "Species")) %>%
      mutate(Yield_mod = coalesce(Yield_mod, 0))
    per_sp <- cmp %>% group_by(Species) %>%
      summarise(rmse_sp = sqrt(mean((log10(Yield_mod + 1) - log10(Yield_obs + 1))^2)),
                mod_tot = sum(Yield_mod), obs_tot = sum(Yield_obs), .groups = "drop")
    list(rmse = sqrt(mean((log10(cmp$Yield_mod + 1) - log10(cmp$Yield_obs + 1))^2,
                          na.rm = TRUE)), per_sp = per_sp)
  }

  q_drawn <- gp0$catchability
  # warm start: the member's own drawn q under the fitted GLOBAL multipliers
  m <- MGLOB[match(gp0$species, names(MGLOB))]; m[is.na(m)] <- 1
  q_start <- pmin(QC, pmax(0, q_drawn * m))

  e_drawn <- evaluate(q_drawn)
  e_start <- evaluate(q_start)
  q_best <- q_start; best <- e_start$rmse
  n_eval <- 2L

  for (h in REFINE) {
    for (g in idx) {
      base <- log10(max(q_best[g], 1e-12))
      cand <- unique(pmin(log10(QC), base + seq(-h, h, length.out = NPT)))
      cand <- cand[cand > -12]
      for (lq in cand) {
        if (isTRUE(all.equal(lq, base))) next
        qt <- q_best; qt[g] <- 10^lq
        r <- evaluate(qt); n_eval <- n_eval + 1L
        if (is.finite(r$rmse) && r$rmse < best) { best <- r$rmse; q_best <- qt }
      }
    }
  }
  e_best <- evaluate(q_best)

  at_cap <- gp0$species[idx][q_best[idx] >= QC * (1 - 1e-9)]
  list(sim_index = si,
       rmse_drawn = e_drawn$rmse, rmse_start = e_start$rmse, rmse_opt = best,
       n_eval = n_eval,
       q_drawn = setNames(q_drawn[idx], gp0$species[idx]),
       q_start = setNames(q_start[idx], gp0$species[idx]),
       q_opt   = setNames(q_best[idx], gp0$species[idx]),
       at_cap  = at_cap,
       per_sp_start = e_start$per_sp, per_sp_opt = e_best$per_sp)
}

t0 <- proc.time()
cl <- makeCluster(CORES)
assign("EFF", effort_arr, envir = environment())
assign("OBS", obs_long, envir = environment())
assign("SDIR", state_dir, envir = environment())
assign("FITSP", FIT_SP, envir = environment())
assign("MGLOB", M_glob, envir = environment())
assign("QC", QCAP, envir = environment())
# everything the worker reads must be exported or passed -- parLapply ships the
# function but NOT the globals it closes over.
clusterExport(cl, c("EFF", "OBS", "SDIR", "FITSP", "MGLOB", "QC",
                    "REFINE", "NPT", "optimise_member"), envir = environment())
res <- parLapplyLB(cl, members, function(si)
  tryCatch(optimise_member(si),
           error = function(e) paste("ERROR:", conditionMessage(e))))
stopCluster(cl)

bad <- vapply(res, function(x) is.character(x) || is.null(x), logical(1))
if (any(bad)) {
  cat(sum(bad), "member(s) failed:\n")
  print(unique(unlist(res[bad & vapply(res, is.character, logical(1))])))
}
res <- res[!bad]
cat(sprintf("\noptimised %d members in %.1f min (%.0f evaluations each)\n",
            length(res), (proc.time() - t0)["elapsed"] / 60,
            mean(vapply(res, `[[`, numeric(1), "n_eval"))))

S <- data.frame(
  sim_index  = vapply(res, `[[`, numeric(1), "sim_index"),
  rmse_drawn = vapply(res, `[[`, numeric(1), "rmse_drawn"),
  rmse_start = vapply(res, `[[`, numeric(1), "rmse_start"),
  rmse_opt   = vapply(res, `[[`, numeric(1), "rmse_opt"))
S$gain_vs_start <- S$rmse_start - S$rmse_opt

cat("\n=== yield RMSE ===\n")
for (nm in c("rmse_drawn", "rmse_start", "rmse_opt"))
  cat(sprintf("  %-11s min %.4f  median %.4f  max %.4f  sd %.4f\n", nm,
              min(S[[nm]]), median(S[[nm]]), max(S[[nm]]), sd(S[[nm]])))
cat(sprintf("\n  best of all 1,997 random draws (stored ranking): 1.5389\n"))
cat(sprintf("  members optimising BELOW that: %d of %d\n",
            sum(S$rmse_opt < 1.5389), nrow(S)))

cat("\n=== DOES THE RANKING SURVIVE? ===\n")
cat(sprintf("  spread (max-min) before %.4f -> after %.4f  (%.2fx compression)\n",
            diff(range(S$rmse_start)), diff(range(S$rmse_opt)),
            diff(range(S$rmse_start)) / diff(range(S$rmse_opt))))
cat(sprintf("  sd before %.4f -> after %.4f\n", sd(S$rmse_start), sd(S$rmse_opt)))
cat(sprintf("  Spearman rho, start vs optimised ranking: %.4f\n",
            cor(S$rmse_start, S$rmse_opt, method = "spearman")))
cat("  (catch_fit on the OLD ensemble found 7/7 members converging to within\n")
cat("   0.02 of each other -- if that reproduces, the ranking is not informative)\n")

cat("\n=== gears pinned at the q cap ===\n")
cap <- table(unlist(lapply(res, `[[`, "at_cap")))
if (length(cap)) print(cap) else cat("  none\n")

PS <- bind_rows(lapply(res, function(x)
  x$per_sp_opt %>% mutate(sim_index = x$sim_index)))
cat("\n=== per-species after optimisation ===\n")
print(as.data.frame(PS %>% group_by(Species) %>%
  summarise(rmse_sp = signif(median(rmse_sp), 3),
            ratio = signif(median(mod_tot / obs_tot), 3),
            sd_rmse = signif(sd(rmse_sp), 3), .groups = "drop")), row.names = FALSE)

saveRDS(list(summary = S, per_species = PS, results = res, cut = CUT, qcap = QCAP),
        file.path(out_dir, "48_per_member_q.rds"))
write.csv(S, file.path(out_dir, "48_per_member_q_summary.csv"), row.names = FALSE)
cat("\nWrote 48_per_member_q.{rds,csv}\n")