# =============================================================================
# Why does member 446 lose 6.7% of community biomass?
#
# THE ANOMALY. Across 30 members the correction shifts community biomass by a
# median of -0.005%, and 29 of 30 sit within 1%. Member 446 sits at **-6.7%** --
# 20x the next worst (-0.91%). It is also **rank 1**, the best-fitting member in
# the ensemble, which is why it cannot be waved away.
#
# WHAT IS ALREADY RULED OUT. Recruitment. Across 30 members rdd_ratio explains
# penguin biomass almost completely (r = 0.859) but the community effect not at
# all (r = -0.111), and 446 has near-restored recruitment (rdd_ratio 0.959) with
# the worst community outcome. Penguins are only 0.06% of community biomass, and
# Stage 0 put them at 0.47% of community krill consumption -- far too small to
# move the community 6.7% through any trophic pathway.
#
# The shift is also already present at the post-steady() point (-7.16%) before the
# spin-up (-6.71%), so it is created by steady(), not by the projection.
#
# THE HYPOTHESIS TO TEST: basin-hopping. If 446's steady state sits near a
# bifurcation, ANY small perturbation flips it to a different attractor and the
# -6.7% is not a dose-response to w_min at all. Two ways to tell:
#
#   1. DOSE-RESPONSE. Run the treated arm at reproduction level 0.25 / 0.50 / 0.75.
#      A real mechanism gives a monotonic, graded response. Basin-hopping gives a
#      jump that barely depends on the dose.
#   2. IRRELEVANT PERTURBATION. Nudge penguin initial abundance by +/-10% in the
#      CONTROL, leaving w_min at 0.001 g. That is a 10% change to 0.06% of
#      community biomass -- physically trivial. If it also moves the community by
#      percent, the member is fragile and w_min is not the cause.
#
# Member 173 is carried as a reference: it showed +0.003% under the correction, so
# it is what a well-behaved member looks like under the identical treatment.
#
# Writes Output_large_files/wmin_test/38_member446.{rds,csv}
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})

out_dir <- "Output_large_files/wmin_test"
PEN <- "small divers"
W_NEW <- 3626.667
EREPRO_CAP <- 0.95
STEADY_TMAX <- 1500
TOL_LADDER <- c(0.1, 0.05, 0.01, 0.0025)
SPINUP_YEARS <- 118
MEMBERS <- c(446, 173)          # anomaly + well-behaved reference

t0 <- proc.time()
cat("=== diagnosing member 446 (rank 1, community -6.7%) ===\n")
cat("started", format(Sys.time()), "\n\n")

ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
idx <- ens$metadata$top10pct_indices
pos <- match(MEMBERS, idx)
stopifnot(!anyNA(pos))
plist <- lapply(pos, function(p) ens$fished_top10pct[[p]]@params)
sp_names <- plist[[1]]@species_params$species
rm(ens); invisible(gc())

# --- one job = (member, condition) -------------------------------------------
jobs <- list()
for (k in seq_along(MEMBERS)) {
  jobs[[length(jobs) + 1]] <- list(k = k, cond = "control", rl = NA, nudge = 1)
  for (rl in c(0.25, 0.50, 0.75))
    jobs[[length(jobs) + 1]] <- list(k = k, cond = sprintf("treated_rl%.2f", rl),
                                     rl = rl, nudge = 1)
  for (nu in c(0.90, 1.10))
    jobs[[length(jobs) + 1]] <- list(k = k, cond = sprintf("ctl_pen_x%.2f", nu),
                                     rl = NA, nudge = nu)
}
cat("jobs:", length(jobs), "(", length(MEMBERS), "members x",
    length(jobs) / length(MEMBERS), "conditions )\n\n")

worker <- function(job) {
  suppressPackageStartupMessages({ library(therMizer); library(mizer) })
  p <- plist[[job$k]]
  i <- which(p@species_params$species == PEN)
  rates0 <- function(q) getRates(q, n = q@initial_n, n_pp = q@initial_n_pp,
                                 n_other = q@initial_n_other, effort = 0, t = 1841)

  set_wmin <- function(q, w_new) {
    rf <- q@rates_funcs; rd <- q@resource_dynamics
    sp <- q@species_params; sp$w_min[i] <- w_new; q@species_params <- sp
    q <- suppressWarnings(setParams(q))
    ther <- intersect(c("Encounter", "PredRate", "EReproAndGrowth"), names(rf))
    q@rates_funcs[ther] <- rf[ther]; q@resource_dynamics <- rd
    q@initial_n[i, q@w < w_new] <- 0
    q
  }

  # build the requested condition
  if (!is.na(job$rl)) {
    p <- set_wmin(p, W_NEW)
    r0 <- rates0(plist[[job$k]]); rtt <- rates0(p)
    fl <- plist[[job$k]]@species_params$erepro[i] * r0$rdd[i] / rtt$rdi[i]
    e_new <- min(fl / (1 - job$rl), EREPRO_CAP)
    rdi_t <- rtt$rdi[i] * e_new / p@species_params$erepro[i]
    R_new <- if (rdi_t > r0$rdd[i]) 1/(1/r0$rdd[i] - 1/rdi_t) else Inf
    p@species_params$erepro[i] <- e_new
    p@species_params$R_max[i] <- R_new
  } else if (job$nudge != 1) {
    p@initial_n[i, ] <- p@initial_n[i, ] * job$nudge
  }

  # steady() ladder, recording convergence years per rung
  yrs <- numeric(0); warned <- FALSE
  for (tol in TOL_LADDER) {
    msg <- character(0)
    out <- withCallingHandlers(
      try(steady(p, tol = tol, t_max = STEADY_TMAX, preserve = c("erepro")),
          silent = TRUE),
      message = function(m) { msg <<- c(msg, conditionMessage(m))
                              invokeRestart("muffleMessage") },
      warning = function(w) {
        if (grepl("did not converge", conditionMessage(w), ignore.case = TRUE))
          warned <<- TRUE
        invokeRestart("muffleWarning") })
    if (inherits(out, "try-error")) return(NULL)
    y <- suppressWarnings(as.numeric(sub(".*in ([0-9.]+) years.*", "\\1",
                                         grep("Convergence", msg, value = TRUE)[1])))
    yrs <- c(yrs, if (length(y) && is.finite(y)) y else NA_real_)
    p <- out
  }

  sim <- try(project(p, t_start = 1841, t_max = SPINUP_YEARS, effort = 0),
             silent = TRUE)
  if (inherits(sim, "try-error")) return(NULL)
  bm_spin <- getBiomass(sim); bm_spin <- bm_spin[nrow(bm_spin), ]
  bm_steady <- rowSums(sweep(p@initial_n, 2, p@w * p@dw, "*"))
  r <- rates0(p)
  fl_sp <- rowSums(r$feeding_level * p@initial_n * rep(p@dw, each = nrow(p@initial_n))) /
    pmax(rowSums(p@initial_n * rep(p@dw, each = nrow(p@initial_n))), 1e-300)

  list(meta = data.frame(member = MEMBERS[job$k], cond = job$cond,
         steady_converged = !warned,
         conv_years_last = yrs[length(yrs)],
         conv_years_total = sum(yrs, na.rm = TRUE),
         total_steady = sum(bm_steady), total_spin = sum(bm_spin),
         pen_steady = bm_steady[i], pen_spin = bm_spin[i],
         erepro = p@species_params$erepro[i], R_max = p@species_params$R_max[i],
         rdd = r$rdd[i], stringsAsFactors = FALSE),
       groups = data.frame(member = MEMBERS[job$k], cond = job$cond,
         species = sp_names, bio_steady = bm_steady, bio_spin = bm_spin,
         feeding_level = fl_sp, stringsAsFactors = FALSE))
}

cl <- makeCluster(min(length(jobs), max(1, detectCores() - 2)))
on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
clusterExport(cl, c("plist", "MEMBERS", "PEN", "W_NEW", "EREPRO_CAP",
                    "STEADY_TMAX", "TOL_LADDER", "SPINUP_YEARS", "sp_names"),
              envir = environment())
res <- parLapplyLB(cl, jobs, worker)
stopCluster(cl)

ok <- !vapply(res, is.null, logical(1))
cat("completed:", sum(ok), "/", length(res), "\n\n")
res <- res[ok]
M <- bind_rows(lapply(res, `[[`, "meta"))
G <- bind_rows(lapply(res, `[[`, "groups"))
saveRDS(list(meta = M, groups = G), file.path(out_dir, "38_member446.rds"))
write.csv(M, file.path(out_dir, "38_member446.csv"), row.names = FALSE)

pct <- function(a, b) 100 * (a - b) / b
for (mb in MEMBERS) {
  Mi <- M %>% filter(member == mb)
  base <- Mi %>% filter(cond == "control")
  cat("################ member", mb,
      if (mb == 446) "(THE ANOMALY, rank 1)" else "(reference)", "################\n")
  Mi <- Mi %>% mutate(
    tot_vs_ctl_steady = pct(total_steady, base$total_steady),
    tot_vs_ctl_spin   = pct(total_spin,   base$total_spin),
    pen_vs_ctl_spin   = pct(pen_spin,     base$pen_spin),
    rdd_ratio         = rdd / base$rdd)
  print(as.data.frame(Mi %>% select(cond, steady_converged, conv_years_total,
    tot_vs_ctl_steady, tot_vs_ctl_spin, pen_vs_ctl_spin, rdd_ratio, erepro)),
    digits = 4, row.names = FALSE)

  cat("\n  --- TEST 1: is the treated response DOSE-DEPENDENT? ---\n")
  tr <- Mi %>% filter(grepl("^treated", cond)) %>% arrange(cond)
  cat("    reproduction level 0.25 / 0.50 / 0.75 -> community shift:",
      paste(sprintf("%+.4f%%", tr$tot_vs_ctl_spin), collapse = " / "), "\n")
  sprd <- diff(range(tr$tot_vs_ctl_spin))
  cat("    spread across the 3 doses:", signif(sprd, 4), "pp",
      if (sprd < 0.2 * max(abs(tr$tot_vs_ctl_spin)))
        " -> FLAT: the dose barely matters (basin-hop signature)\n" else
        " -> GRADED: responds to the dose (mechanism signature)\n")

  cat("\n  --- TEST 2: does an IRRELEVANT perturbation move it too? ---\n")
  nu <- Mi %>% filter(grepl("^ctl_pen_x", cond))
  cat("    penguin abundance x0.90 / x1.10 in the CONTROL -> community shift:",
      paste(sprintf("%+.4f%%", nu$tot_vs_ctl_spin), collapse = " / "), "\n")
  worst_nudge <- max(abs(nu$tot_vs_ctl_spin))
  worst_treat <- max(abs(tr$tot_vs_ctl_spin))
  cat("    |largest nudge effect| =", signif(worst_nudge, 4), "pp vs",
      "|largest treated effect| =", signif(worst_treat, 4), "pp\n")
  cat("    ratio:", signif(worst_nudge / worst_treat, 3),
      if (worst_nudge > 0.25 * worst_treat)
        " -> FRAGILE: a trivial nudge reproduces much of the effect\n" else
        " -> ROBUST: the nudge does nothing; the effect is real\n")

  cat("\n  --- which groups carry the change (treated rl=0.50, post-spin) ---\n")
  gb <- G %>% filter(member == mb, cond == "control") %>% select(species, ctl = bio_spin)
  gt <- G %>% filter(member == mb, cond == "treated_rl0.50") %>%
    select(species, trt = bio_spin, fl_trt = feeding_level)
  gj <- gb %>% inner_join(gt, by = "species") %>%
    mutate(delta_g = trt - ctl, pct = pct(trt, ctl),
           share_of_total_change = 100 * (trt - ctl) / (sum(trt) - sum(ctl)))
  print(as.data.frame(gj %>% arrange(desc(abs(delta_g))) %>%
    select(species, ctl, trt, pct, share_of_total_change) %>% head(8)),
    digits = 4, row.names = FALSE)
  cat("\n")
}

cat("\n================ INTERPRETATION ================\n")
cat("TEST 1 flat + TEST 2 fragile  -> 446 sits near a bifurcation; the -6.7% is\n")
cat("   an alternative steady state, not a dose-response to w_min. Report it as a\n")
cat("   numerical property of that member, not an effect of the correction.\n")
cat("TEST 1 graded + TEST 2 robust -> the effect is a genuine w_min consequence\n")
cat("   and needs a mechanism.\n")
cat("\nelapsed:", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")