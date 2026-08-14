# =============================================================================
# STAGE C -- regenerate the calibrated steady state with the corrected w_min.
#
# RE-ENTRY POINT. 06_steady_state_therMizer.Rmd interleaves scripted
# steady()/matchBiomasses() ladders with interactive tuneParams() Shiny sessions
# (lines 173, 189), so it cannot be reproduced non-interactively. Agreed re-entry
# is 09_Uncertainty_Analysis.Rmd's own steady() call, which is fully scripted:
#
#   :1080  params_new_v4 <- readRDS("params_steady_state_2011_2020_tol_0.00025.RDS")
#   :1105  params_1841_2010_climate_only <- upgradeTherParams(params_new_v4, ...)
#   :1403  params_sel_adj <- params_1841_2010_climate_only   + gear selectivity
#   :1444  params_sel_adj <- steady(params_sel_adj, tol = 0.002, t_max = 1000,
#                                   preserve = c("erepro"))
#   :1477  saveRDS(params_sel_adj, "params_sel_adj.rds")
#
# We start from the SAVED params_sel_adj rather than replaying :1403-1444,
# because the gear-selectivity block derives l50/l25 from observed whale catch
# lengths (IWC_data/catch_lengths.rds) and depends on Rmd chunk state. Those
# values are functions of observed catch only -- they do not depend on w_min --
# so re-deriving them would reproduce the same numbers with more ways to go
# wrong. The final steady() call is then replayed exactly.
#
# CONTROL vs TREATED. The stored params_sel_adj was produced by a steady() run we
# cannot bit-reproduce, so the control is the stored object put through the SAME
# fresh steady() call. treated - control isolates the w_min correction;
# control - stored shows what re-running steady() alone costs.
#
# THE CORRECTION (settled in Stages A/B):
#   w_min 0.001 -> 3626.667 g      (0.85 * w_mat; see docs/size_parameter_audit.md)
#   penguin erepro/R_max recalibrated to reproduction level 0.50
#                                  (see docs/small_divers_wmin_stageB_results.md)
# Changing w_min alone leaves the group uncalibrated and drives it extinct --
# that was Stage 1's error.
#
# Writes params_sel_adj_wmin_corrected.rds  (NOT over either stored copy)
#        Output_large_files/wmin_test/30_stageC_comparison.csv
#        docs/small_divers_wmin_stageC_results.md
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(dplyr)
})
source("R/check_size_params.R")

out_dir <- "Output_large_files/wmin_test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

PEN         <- "small divers"
W_NEW       <- 3626.667
REPRO_LEVEL <- 0.50          # agreed setting: 2x the feasibility floor
# 09_Uncertainty_Analysis.Rmd:1444 exactly
STEADY_TOL  <- 0.002
STEADY_TMAX <- 1000
STEADY_PRESERVE <- c("erepro")
# approach the target tolerance through a ladder; a single strict call is where
# the treated params are most likely to fail (Stage B protocol note)
TOL_LADDER  <- c(0.1, 0.05, 0.01, STEADY_TOL)

t0 <- proc.time()
cat("=== Stage C: corrected steady-state params ===\n")
cat("started", format(Sys.time()), "| mizer", as.character(packageVersion("mizer")),
    "| therMizer", as.character(packageVersion("therMizer")), "\n")
cat("w_min ->", W_NEW, "g | penguin reproduction level ->", REPRO_LEVEL, "\n\n")

stored <- readRDS("Manuscript data/params_sel_adj.rds")
i_pen <- which(stored@species_params$species == PEN)
stopifnot(length(i_pen) == 1)

cat("stored params: w_min =", stored@species_params$w_min[i_pen],
    "| erepro =", signif(stored@species_params$erepro[i_pen], 5),
    "| R_max =", signif(stored@species_params$R_max[i_pen], 5), "\n\n")

# --- helpers (identical to Stage B) ------------------------------------------
set_wmin <- function(p, species, w_new) {
  rf <- p@rates_funcs; rd <- p@resource_dynamics
  i <- which(p@species_params$species == species)
  sp <- p@species_params; sp$w_min[i] <- w_new; p@species_params <- sp
  p <- suppressWarnings(setParams(p))
  ther <- intersect(c("Encounter", "PredRate", "EReproAndGrowth"), names(rf))
  p@rates_funcs[ther] <- rf[ther]
  p@resource_dynamics <- rd
  p@initial_n[i, p@w < w_new] <- 0
  stopifnot(p@w_min_idx[[i]] == max(which(p@w <= w_new)),
            identical(p@rates_funcs$Encounter, "therMizerEncounter"))
  p
}

rates_at_init <- function(p) {
  getRates(p, n = p@initial_n, n_pp = p@initial_n_pp,
           n_other = p@initial_n_other, effort = 0, t = 1841)
}

steady_ladder <- function(p, label) {
  conv <- rep(NA, length(TOL_LADDER))
  for (k in seq_along(TOL_LADDER)) {
    tol <- TOL_LADDER[k]; warned <- FALSE
    out <- withCallingHandlers(
      try(steady(p, tol = tol, t_max = STEADY_TMAX, preserve = STEADY_PRESERVE),
          silent = TRUE),
      warning = function(w) {
        if (grepl("did not converge", conditionMessage(w), ignore.case = TRUE))
          warned <<- TRUE
        invokeRestart("muffleWarning")
      })
    if (inherits(out, "try-error"))
      stop("steady() errored for ", label, " at tol = ", tol)
    conv[k] <- !warned
    p <- out
    cat(sprintf("   %-8s tol=%-7.4g %s\n", label, tol,
                if (warned) "did NOT converge" else "converged"))
  }
  list(params = p, converged = conv)
}

# --- CONTROL: stored params through the same fresh steady() ------------------
cat("--- control ---\n")
ctl <- steady_ladder(stored, "control")
p_ctl <- ctl$params

# --- TREATED: corrected w_min + recalibrated penguin reproduction ------------
cat("\n--- treated ---\n")
p_trt <- set_wmin(stored, PEN, W_NEW)

r_ctl0 <- rates_at_init(stored)
r_trt0 <- rates_at_init(p_trt)
rdd_target <- r_ctl0$rdd[i_pen]
erepro_floor <- stored@species_params$erepro[i_pen] * rdd_target / r_trt0$rdi[i_pen]
erepro_new <- erepro_floor / (1 - REPRO_LEVEL)
cat(sprintf("   erepro floor %.5g -> set %.5g at reproduction level %.2f\n",
            erepro_floor, erepro_new, REPRO_LEVEL))
stopifnot(erepro_new < 1)

rdi_target <- r_trt0$rdi[i_pen] * erepro_new / p_trt@species_params$erepro[i_pen]
R_new <- if (rdi_target > rdd_target)
  1 / (1 / rdd_target - 1 / rdi_target) else Inf
cat(sprintf("   R_max %.5g -> %.5g\n", stored@species_params$R_max[i_pen], R_new))
stopifnot(is.finite(R_new))

p_trt@species_params$erepro[i_pen] <- erepro_new
p_trt@species_params$R_max[i_pen]  <- R_new
# confirm the direct species_params assignment really moved RDI
stopifnot(isTRUE(all.equal(unname(rates_at_init(p_trt)$rdi[i_pen]),
                           unname(rdi_target), tolerance = 1e-8)))

trt <- steady_ladder(p_trt, "treated")
p_trt <- trt$params

# --- hard constraints --------------------------------------------------------
cat("\n=== hard constraints ===\n")
chk <- check_size_params(p_trt, quiet = TRUE)
cat("  w_min < w_mat < w_max, all 19 :",
    if (all(chk$ordering_ok)) "PASS" else "FAIL", "\n")
cat("  erepro < 1, all 19            :",
    if (all(chk$erepro_ok)) sprintf("PASS (max %.4g, %s)",
                                    max(chk$erepro),
                                    chk$species[which.max(chk$erepro)]) else "FAIL", "\n")
cat("  penguin R_max finite          :",
    if (is.finite(p_trt@species_params$R_max[i_pen])) "PASS" else "FAIL", "\n")
nf_ctl <- p_ctl@species_params$species[!is.finite(p_ctl@species_params$R_max)]
nf_trt <- p_trt@species_params$species[!is.finite(p_trt@species_params$R_max)]
cat("  R_max = Inf, control          :",
    if (length(nf_ctl)) paste(nf_ctl, collapse = ", ") else "none", "\n")
cat("  R_max = Inf, treated          :",
    if (length(nf_trt)) paste(nf_trt, collapse = ", ") else "none", "\n")
cat("  no NEW degeneracy             :",
    if (length(nf_trt) <= length(nf_ctl)) "PASS" else "FAIL", "\n")

stopifnot(all(chk$ordering_ok), all(chk$erepro_ok),
          is.finite(p_trt@species_params$R_max[i_pen]),
          length(nf_trt) <= length(nf_ctl))

# --- per-group comparison ----------------------------------------------------
bio <- function(p) {
  n <- p@initial_n; wdw <- p@w * p@dw
  rowSums(sweep(n, 2, wdw, "*"))
}
fl <- function(p) {
  r <- rates_at_init(p)
  rowSums(r$feeding_level * p@initial_n * rep(p@dw, each = nrow(p@initial_n))) /
    pmax(rowSums(p@initial_n * rep(p@dw, each = nrow(p@initial_n))), .Machine$double.xmin)
}
gr <- function(p) {
  r <- rates_at_init(p)
  i_mat <- vapply(seq_len(nrow(p@species_params)), function(i)
    which.min(abs(p@w - p@species_params$w_mat[i])), integer(1))
  vapply(seq_along(i_mat), function(i) r$e_growth[i, i_mat[i]], numeric(1))
}

sp_s <- stored@species_params; sp_c <- p_ctl@species_params; sp_t <- p_trt@species_params
cmp <- data.frame(
  species = sp_s$species,
  bio_stored = bio(stored), bio_control = bio(p_ctl), bio_treated = bio(p_trt),
  pct_treated_vs_control = 100 * (bio(p_trt) - bio(p_ctl)) / bio(p_ctl),
  pct_control_vs_stored  = 100 * (bio(p_ctl) - bio(stored)) / bio(stored),
  erepro_control = sp_c$erepro, erepro_treated = sp_t$erepro,
  R_max_control = sp_c$R_max, R_max_treated = sp_t$R_max,
  fl_control = fl(p_ctl), fl_treated = fl(p_trt),
  growth_at_wmat_control = gr(p_ctl), growth_at_wmat_treated = gr(p_trt),
  stringsAsFactors = FALSE)
cmp$growth_pct <- 100 * (cmp$growth_at_wmat_treated - cmp$growth_at_wmat_control) /
  cmp$growth_at_wmat_control

write.csv(cmp, file.path(out_dir, "30_stageC_comparison.csv"), row.names = FALSE)

cat("\n=== steady-state biomass, treated vs control (all 19 groups) ===\n")
print(as.data.frame(cmp %>% arrange(desc(abs(pct_treated_vs_control))) %>%
  select(species, bio_control, bio_treated, pct_treated_vs_control,
         pct_control_vs_stored)), digits = 4, row.names = FALSE)

cat("\n=== feeding level and growth at w_mat ===\n")
print(as.data.frame(cmp %>% select(species, fl_control, fl_treated,
                                   growth_at_wmat_control,
                                   growth_at_wmat_treated, growth_pct) %>%
  arrange(desc(abs(growth_pct)))), digits = 4, row.names = FALSE)

nonpen <- cmp[cmp$species != PEN, ]
cat("\n=== summary ===\n")
cat(sprintf("  total community biomass  control %.6e  treated %.6e  (%+.4f%%)\n",
            sum(cmp$bio_control), sum(cmp$bio_treated),
            100 * (sum(cmp$bio_treated) - sum(cmp$bio_control)) / sum(cmp$bio_control)))
cat(sprintf("  penguin biomass          %+.2f%% vs control\n",
            cmp$pct_treated_vs_control[cmp$species == PEN]))
cat(sprintf("  largest NON-penguin shift %+.4f%% (%s)\n",
            nonpen$pct_treated_vs_control[which.max(abs(nonpen$pct_treated_vs_control))],
            nonpen$species[which.max(abs(nonpen$pct_treated_vs_control))]))
cat(sprintf("  median |shift| over the other 18 groups: %.4f%%\n",
            median(abs(nonpen$pct_treated_vs_control))))

saveRDS(p_trt, "params_sel_adj_wmin_corrected.rds")
saveRDS(p_ctl, file.path(out_dir, "30_params_sel_adj_control_resteadied.rds"))
cat("\nWrote params_sel_adj_wmin_corrected.rds (stored copies untouched)\n")
cat("elapsed:", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
