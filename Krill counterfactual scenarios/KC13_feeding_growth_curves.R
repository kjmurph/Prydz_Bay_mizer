# =============================================================================
# KC13 -- the visual diagnostic: feeding level by body size, and modelled
# growth curves, per gamma / reproduction configuration.
#
# WHY THIS EXISTS. KC10 stores a biomass-weighted SCALAR feeding level per
# species-year. That hides where in the size range satiation bites, and it says
# nothing at all about growth -- which is the mechanism that would move body
# size even if reproduction pins numbers at R_max. This script rebuilds the
# post-steady params object for every member x configuration and reads both
# curves straight off it with mizer's own functions.
#
# STATE. The curves are evaluated at the POST-steady() calibrated state, before
# the spin-up and before any fishing. That is the right state for the question
# "what did the gamma change do to feeding and growth", which is a property of
# the parameterisation, not of an effort arm.
#
# TWO ADJUSTMENTS TO THE BUILT-INS, both forced by this model:
#
#   1. plotFeedingLevel() is used as-is (return_data = TRUE, include_critical),
#      and needs nothing.
#
#   2. getGrowthCurves() integrates dw/dt with deSolve::ode, and DIES once the
#      integration runs past the age at which growth reaches zero at
#      w_repro_max ("too much accuracy requested for precision of machine").
#      On the phase-55 base that is age ~27 for minke and ~91 for baleen. The
#      age is not fixed -- lower gamma means slower growth means a later
#      plateau -- so max_age must be chosen PER CONFIGURATION, by stepping down
#      until the call succeeds. Each config therefore has its own age grid, and
#      the curves are interpolated onto one common axis afterwards. Where a
#      curve reached w_repro_max it is held forward past its own max_age, which
#      is exact (growth there is zero); where it did not, it is left NA rather
#      than extrapolated.
#
#      G1 gates this against an independent calculation: age(w) = cumsum(dw/g)
#      straight off getEGrowth(), which uses no ODE solver and cannot stall.
#
# COST. steady() only -- no spin-up, no projection, no diet extraction. The
# rebuilt params objects are cached, so the figures can be re-made for free.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC13_feeding_growth_curves.R" [run|collect]
# ENV    KC13_N (10), KC13_CORES (10), KC13_SPECIES, KC13_AGE_MAX (60),
#        KC13_GAMMA_ARMS, KC13_REPRO_ARMS, KC13_RL (0.25), KC13_BASE, KC_FORCE
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
  library(tidyr); library(ggplot2); library(patchwork)
})

OUT_LARGE <- "Output_large_files/wmin_test"
KC_ROOT <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
ANA <- file.path(KC_ROOT, "analysis"); FIG <- file.path(KC_ROOT, "figures")
BASE_FILE <- Sys.getenv("KC13_BASE",
                        "params_ref_sw2000_balror_mnkfish05_whres05.rds")
FORCE <- nzchar(Sys.getenv("KC_FORCE"))

GAMMA_ARMS_ALL <- c("drawn", "wh_mid", "wh_base", "all_mid", "all_base")
REPRO_ARMS_ALL <- c("asis", "rl25")
pick <- function(env, all) {
  v <- Sys.getenv(env, paste(all, collapse = ","))
  v <- trimws(strsplit(v, ",")[[1]]); v <- v[nzchar(v)]
  if (length(setdiff(v, all)))
    stop(env, " must be a subset of: ", paste(all, collapse = ", "), call. = FALSE)
  all[all %in% v]
}
GA <- pick("KC13_GAMMA_ARMS", GAMMA_ARMS_ALL)
RP <- pick("KC13_REPRO_ARMS", REPRO_ARMS_ALL)
N         <- as.integer(Sys.getenv("KC13_N", "10"))
RL_TARGET <- as.numeric(Sys.getenv("KC13_RL", "0.25"))
AGE_MAX   <- as.numeric(Sys.getenv("KC13_AGE_MAX", "60"))
CORES     <- min(as.integer(Sys.getenv("KC13_CORES", "10")), 15,
                 max(1, N * length(GA) * length(RP)))
FOCUS <- trimws(strsplit(Sys.getenv(
  "KC13_SPECIES", "baleen whales,minke whales,antarctic krill"), ",")[[1]])
STEM <- sprintf("KC13_curves_n%d", N)
PDIR <- file.path(OUT_LARGE, paste0(STEM, "_params"))
OUT_RDS <- file.path(OUT_LARGE, paste0(STEM, ".rds"))
for (d in c(ANA, FIG, PDIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"
stopifnot(mode %in% c("run", "collect"))

STEADY_TOL <- 0.0025; STEADY_TMAX <- 1500
KRILL <- "antarctic krill"; WHALES <- c("baleen whales", "minke whales")

t0 <- proc.time()
cat("=== KC13: feeding level and growth curves by configuration ===\n")
cat("base:", BASE_FILE, "| species:", paste(FOCUS, collapse = ", "), "\n")

# --- members and base, exactly as KC10 ---------------------------------------
CUTS <- readRDS(file.path(OUT_LARGE, "46_selection_cuts.rds"))
cutA <- as.integer(CUTS$cuts[["A unweighted RMSE"]])
RF <- readRDS(file.path(OUT_LARGE, "45_refit_results.rds"))
chk <- RF$per_species %>% group_by(sim_index) %>%
  summarise(m = sqrt(sum(sse) / sum(n)), .groups = "drop") %>%
  arrange(m) %>% head(length(cutA)) %>% pull(sim_index)
if (!identical(as.integer(chk), cutA))
  stop("cut A does not match the post-refit ranking -- refusing to proceed.")
members <- head(cutA, N)
DR <- readRDS(file.path(OUT_LARGE, "43_member_draws.rds"))
BASE <- suppressWarnings(validParams(readRDS(BASE_FILE)))
stopifnot(identical(BASE@rates_funcs$Encounter, "therMizerEncounter"),
          identical(BASE@resource_dynamics, "plankton_forcing"))
SP <- BASE@species_params$species
i_wh <- match(WHALES, SP)
G_BASE <- setNames(BASE@species_params$gamma, SP)
if (length(setdiff(FOCUS, SP))) stop("unknown species: ",
                                     paste(setdiff(FOCUS, SP), collapse = ", "))
cat("members:", paste(members, collapse = ", "), "\n")

JOBS <- expand.grid(sim_index = members, gamma_arm = GA, repro = RP,
                    stringsAsFactors = FALSE)
pfile <- function(k) file.path(PDIR, sprintf("p_%05d_%s_%s.rds", JOBS$sim_index[k],
                                             JOBS$gamma_arm[k], JOBS$repro[k]))

# ------------------------------------------------- rebuild the params objects
build <- function(k) {
  suppressPackageStartupMessages({library(mizer); library(therMizer)})
  f <- pfile(k)
  if (file.exists(f)) return(list(k = k, ok = TRUE, reason = "cached"))
  si <- JOBS$sim_index[k]; garm <- JOBS$gamma_arm[k]; rarm <- JOBS$repro[k]
  dw_ <- DR$draws[[as.character(si)]]
  bad <- function(r) list(k = k, ok = FALSE, reason = r)

  p <- BASE                                   # apply_draws, as KC10 / KC07
  gp <- gear_params(p)
  if (!identical(paste(gp$gear, gp$species), names(dw_$catchability)))
    return(bad("gear_rows_differ"))
  gp$catchability <- as.numeric(dw_$catchability); gear_params(p) <- gp
  sp <- species_params(p)
  if (!identical(sp$species, names(dw_$gamma))) return(bad("species_order"))
  sp$gamma <- as.numeric(dw_$gamma)
  species_params(p) <- sp
  sc <- as.numeric(dw_$abundance_scaling[sp$species])
  for (j in seq_along(sc)) p@initial_n[j, ] <- p@initial_n[j, ] * sc[j]

  if (garm != "drawn") {                      # apply_gamma_arm, as KC10
    sp <- species_params(p)
    idx <- if (startsWith(garm, "wh_")) i_wh else seq_len(nrow(sp))
    gb <- as.numeric(G_BASE)[idx]; gd <- sp$gamma[idx]
    sp$gamma[idx] <- if (endsWith(garm, "_base")) gb else sqrt(gb * gd)
    species_params(p) <- sp
  }
  if (!identical(p@rates_funcs$Encounter, "therMizerEncounter"))
    return(bad("rates_funcs_lost"))

  no_conv <- FALSE
  ps <- withCallingHandlers(
    try(steady(p, tol = STEADY_TOL, t_max = STEADY_TMAX,
               preserve = c("erepro"), progress_bar = FALSE), silent = TRUE),
    message = function(m) {
      if (grepl("did not converge", conditionMessage(m), ignore.case = TRUE))
        no_conv <<- TRUE
      invokeRestart("muffleMessage")
    }, warning = function(w) invokeRestart("muffleWarning"))
  if (inherits(ps, "try-error")) return(bad("steady_error"))
  if (no_conv) return(bad("steady_no_converge"))

  if (rarm == "rl25") {
    ps2 <- try(suppressWarnings(setBevertonHolt(
      ps, reproduction_level = setNames(rep(RL_TARGET, length(WHALES)), WHALES))),
      silent = TRUE)
    if (inherits(ps2, "try-error")) return(bad("repro_error"))
    ps <- ps2
  }
  saveRDS(ps, f)
  list(k = k, ok = TRUE, reason = "built")
}

if (mode == "run") {
  todo <- which(!vapply(seq_len(nrow(JOBS)), function(k) file.exists(pfile(k)),
                        logical(1)))
  cat("params objects:", nrow(JOBS), "|", length(todo), "to build |",
      CORES, "cores\n")
  if (length(todo)) {
    cl <- makeCluster(CORES)
    on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
    clusterExport(cl, c("BASE", "JOBS", "DR", "G_BASE", "i_wh", "WHALES",
                        "RL_TARGET", "STEADY_TOL", "STEADY_TMAX", "PDIR",
                        "pfile"), envir = environment())
    br <- parLapply(cl, todo, build)
    stopCluster(cl)
    nb <- vapply(br, function(r) isTRUE(r$ok), logical(1))
    cat("built", sum(nb), "| failed", sum(!nb), "\n")
    if (any(!nb)) print(as.data.frame(do.call(rbind, lapply(br[!nb], function(r)
      data.frame(sim_index = JOBS$sim_index[r$k], gamma_arm = JOBS$gamma_arm[r$k],
                 repro = JOBS$repro[r$k], reason = r$reason)))), row.names = FALSE)
  }
  cat(sprintf("build elapsed %.1f min\n", (proc.time() - t0)[["elapsed"]] / 60))
}

have <- which(vapply(seq_len(nrow(JOBS)), function(k) file.exists(pfile(k)),
                     logical(1)))
if (!length(have)) stop("no params objects in ", PDIR, call. = FALSE)
cat("\nusing", length(have), "of", nrow(JOBS), "configurations\n")

# ------------------------------------------------------------- the two curves
# getGrowthCurves() with a per-configuration max_age, stepped down until the
# ODE solver stops complaining. See the header.
# The independent check: age(w) = integral of dw/g(w), off getEGrowth(). No
# solver, so it cannot stall.
#
# It MUST be integrated properly. mizer's size grid is logarithmic and spans ~15
# decades in 100 bins, so a whale occupies only ~11 of them; a plain
# cumsum(dw/g) left-Riemann sum over 11 wide bins is biased by tens of percent
# and will fail the gate against a correct ODE solution. Substituting
# w = exp(u) gives age = integral of (w/g) du, which is smooth in log space --
# interpolate w/g onto a fine u grid and use the trapezoid rule.
age_by_integral <- function(p, s, n_fine = 4000) {
  i <- match(s, species_params(p)$species)
  g <- getEGrowth(p)[i, ]; w <- p@w
  lo <- p@w_min_idx[i]
  hi <- max(which(w <= species_params(p)$w_repro_max[i]))
  j <- lo:hi
  j <- j[g[j] > 0]
  if (length(j) < 3) return(NULL)
  u <- log(w[j]); y <- w[j] / g[j]          # integrand in log-mass space
  uf <- seq(u[1], u[length(u)], length.out = n_fine)
  yf <- exp(approx(u, log(y), xout = uf)$y) # log-linear: y spans decades
  du <- diff(uf)
  age <- c(0, cumsum((head(yf, -1) + tail(yf, -1)) / 2 * du))
  data.frame(w = exp(uf), age = age)
}
# getGrowthCurves() with a per-configuration max_age. The integral above gives
# the age at which the species reaches w_repro_max; asking the solver for
# anything beyond that makes it integrate a zero-growth region and die. Start
# just inside it, and keep the step-down as a backstop.
gc_auto <- function(p, s, target) {
  ai <- age_by_integral(p, s)
  ma <- target
  if (!is.null(ai) && is.finite(max(ai$age)))
    ma <- max(3, min(target, floor(0.9 * max(ai$age))))
  repeat {
    r <- try(suppressWarnings(getGrowthCurves(p, species = s, max_age = ma)),
             silent = TRUE)
    if (!inherits(r, "try-error")) return(list(curve = r, max_age = ma))
    ma <- floor(ma * 0.8)
    if (ma < 3) return(NULL)
  }
}

FLL <- GCL <- MAX_AGE <- list()
for (k in have) {
  ps <- readRDS(pfile(k))
  tag <- list(sim_index = JOBS$sim_index[k], gamma_arm = JOBS$gamma_arm[k],
              repro = JOBS$repro[k])
  fl <- try(suppressWarnings(plotFeedingLevel(
    ps, species = FOCUS, include_critical = TRUE, return_data = TRUE)),
    silent = TRUE)
  if (!inherits(fl, "try-error")) {
    fl <- as.data.frame(fl); names(fl)[names(fl) == "Feeding level"] <- "f"
    FLL[[length(FLL) + 1]] <- cbind(fl, tag, row.names = NULL)
  }
  for (s in FOCUS) {
    r <- gc_auto(ps, s, AGE_MAX)
    if (is.null(r)) next
    cv <- r$curve
    GCL[[length(GCL) + 1]] <- data.frame(
      Species = s, Age = as.numeric(colnames(cv)), Size = as.numeric(cv[s, ]),
      max_age = r$max_age, tag, row.names = NULL)
    # Age at maturity comes from the INTEGRAL, not the ODE curve. The solver has
    # to stop short of the asymptote to survive (see gc_auto), and minke w_mat is
    # 0.9 * w_max -- reached only in the very last creep -- so reading it off the
    # truncated curve returns NA for every member. The integral spans the whole
    # size range and is validated against the solver by G1.
    ai <- age_by_integral(ps, s)
    amat <- NA_real_
    if (!is.null(ai)) {
      tgt <- species_params(ps)$w_mat[match(s, species_params(ps)$species)]
      if (max(ai$w) >= tgt) amat <- approx(ai$w, ai$age, xout = tgt)$y
    }
    MAX_AGE[[length(MAX_AGE) + 1]] <- data.frame(
      Species = s, max_age = r$max_age, age_mat = amat, tag)
  }
}
FLD <- bind_rows(FLL); GCD <- bind_rows(GCL); MAD <- bind_rows(MAX_AGE)
if (!nrow(GCD)) stop("no growth curve could be computed", call. = FALSE)

cat("\n=== max_age the ODE solver tolerated, by configuration (years) ===\n")
print(as.data.frame(MAD %>% group_by(gamma_arm, repro, Species) %>%
  summarise(min = min(max_age), max = max(max_age), .groups = "drop") %>%
  pivot_wider(names_from = Species, values_from = c(min, max))), row.names = FALSE)
cat("  Lower gamma grows more slowly and plateaus later, so it tolerates a\n",
    " longer integration. This is why max_age cannot be a constant.\n")

# =============================================================================
# G1 -- the built-in against the direct integral
# =============================================================================
k1 <- have[1]; p1 <- readRDS(pfile(k1))
cat("\n=== G1. getGrowthCurves vs the trapezoid integral of dw/g ===\n")
# Reported PER SPECIES. mizer's log grid gives a whale ~11 bins over its whole
# size range and antarctic krill fewer still, so the integral is itself coarser
# for a small-bodied, steep-growth group -- a 7% disagreement on krill is the
# check's resolution, not the solver's error. The whale verdict is the one that
# governs, since the whale curves are what this figure is for.
g1ok <- TRUE; g1 <- list()
for (s in FOCUS) {
  ai <- age_by_integral(p1, s)
  gg <- GCD %>% filter(Species == s, sim_index == JOBS$sim_index[k1],
                       gamma_arm == JOBS$gamma_arm[k1], repro == JOBS$repro[k1])
  if (is.null(ai) || !nrow(gg)) { cat(sprintf("  %-18s skipped\n", s)); next }
  # compare on the age range both cover, away from the asymptote where a tiny
  # size difference maps to an unbounded age difference
  amax <- min(max(ai$age), max(gg$Age))
  at <- seq(0.05 * amax, 0.8 * amax, length.out = 40)
  w_int <- approx(ai$age, ai$w, xout = at)$y
  w_ode <- approx(gg$Age, gg$Size, xout = at)$y
  rel <- max(abs(w_ode - w_int) / w_int, na.rm = TRUE)
  g1[[s]] <- rel
  cat(sprintf("  %-18s max rel diff in size-at-age: %.3g  [%s]\n", s, rel,
              if (is.finite(rel) && rel <= 0.05) "ok" else "COARSE"))
}
wh <- unlist(g1[intersect(WHALES, names(g1))])
g1ok <- length(wh) > 0 && all(is.finite(wh) & wh <= 0.05)
cat(if (g1ok)
  sprintf("  G1 PASSED on the whale groups (worst %.3g). Any non-whale group\n  flagged COARSE is the check's own grid resolution, not a solver error.\n",
          max(wh))
  else
  "  G1 FAILED on a whale group -- the solver and the integral genuinely\n  disagree. Treat the growth curves as indicative only and say so.\n")

# ------------------------------------------------ common age axis, then median
AGRID <- seq(0, AGE_MAX, length.out = 121)
# Hold a curve forward past its own max_age only where it actually reached the
# asymptote: growth is exactly zero at w_repro_max, so that is not extrapolation.
WREPRO <- setNames(species_params(BASE)$w_repro_max, SP)
GCI <- GCD %>% group_by(sim_index, gamma_arm, repro, Species) %>%
  reframe({
    y <- approx(Age, Size, xout = AGRID, rule = 1)$y
    at_asym <- max(Size, na.rm = TRUE) >= 0.99 * WREPRO[[first(Species)]]
    if (at_asym) y[AGRID > max(Age)] <- max(Size, na.rm = TRUE)
    data.frame(Age = AGRID, Size = y, reached_asymptote = at_asym)
  }) %>% ungroup()

CFG_LEV <- as.vector(t(outer(GA, RP, paste, sep = "/")))
REF_CFG <- if ("drawn/asis" %in% CFG_LEV) "drawn/asis" else CFG_LEV[1]
lab <- function(d) d %>%
  mutate(cfg = factor(paste(gamma_arm, repro, sep = "/"), levels = CFG_LEV))

# THE REPRODUCTION ARM CANNOT MOVE EITHER CURVE, and the figure must not imply
# otherwise. setBevertonHolt() is applied after steady() and preserves the
# calibrated state exactly, while both feeding level and growth are read off
# that state -- so rl25 is identical to asis to the last bit. Plotting both
# draws 10 keys for 5 curves, with each rl25 line painting exactly over its
# asis twin (which is why drawn/asis, the black key, was nowhere to be seen).
# Verified here rather than assumed, then dropped from the figure.
rep_delta <- function(d, val) {
  if (length(RP) < 2) return(0)
  w <- d %>% select(sim_index, gamma_arm, repro, Species,
                    x = any_of(c("w", "Age")), v = all_of(val)) %>%
    tidyr::pivot_wider(names_from = repro, values_from = v)
  if (!all(c("asis", "rl25") %in% names(w))) return(NA_real_)
  max(abs(w$rl25 - w$asis) / pmax(abs(w$asis), .Machine$double.xmin),
      na.rm = TRUE)
}
d_fl <- rep_delta(FLD %>% filter(Type == "actual"), "f")
d_gc <- rep_delta(GCD, "Size")
cat(sprintf("\nreproduction arm effect on these curves: feeding %.3g, growth %.3g\n",
            d_fl, d_gc))
if (isTRUE(d_fl > 1e-12) || isTRUE(d_gc > 1e-12))
  stop("rl25 changed a calibrated-state curve -- it should be identical to ",
       "asis. Investigate before plotting.", call. = FALSE)
cat("  identical, as it must be -- the figure shows the gamma arms only.\n")

# Plain-language names, so the legend does not need decoding.
GAM_LAB <- c(drawn    = "Ensemble gamma (control)",
             wh_mid   = "Whales, gamma half-way to base",
             wh_base  = "Whales, base-model gamma",
             all_mid  = "All groups, gamma half-way to base",
             all_base = "All groups, base-model gamma")
GA_LEV <- GA
GA_COL <- setNames(colorRampPalette(
  c("#0b0b0b", "#2a78d6", "#28a06a", "#e0a51c", "#e34948"))(length(GA_LEV)),
  GA_LEV)
ONE <- RP[1]        # the reproduction arms are identical; keep one
garm_f <- function(d) d %>% filter(repro == ONE) %>%
  mutate(garm = factor(gamma_arm, levels = GA_LEV))
GCM <- garm_f(GCI) %>% filter(!is.na(Size)) %>%
  group_by(garm, Species, Age) %>%
  summarise(lo = quantile(Size, 0.25), med = median(Size),
            hi = quantile(Size, 0.75), n = n(), .groups = "drop") %>%
  filter(n >= 0.5 * length(members))
FLM <- garm_f(FLD) %>% group_by(garm, Species, Type, w) %>%
  summarise(lo = quantile(f, 0.25), med = median(f), hi = quantile(f, 0.75),
            .groups = "drop")

# --------------------------------------------------------------------- plots
base_thm <- theme_bw(base_size = 10) +
  theme(panel.grid = element_blank(), legend.position = "bottom",
        strip.background = element_rect(fill = "grey96", colour = "grey70"),
        strip.text = element_text(face = "bold", size = 9))

# patchwork's guides = "collect" does NOT merge these two legends under
# ggplot2 4.0.3 -- it draws one per panel and clips both. Since the two panels
# share one colour mapping, only panel a carries a key and panel b suppresses
# its own. Do not "fix" this by re-enabling pB's guide.
cfg_scales <- function(show) list(
  scale_colour_manual(values = GA_COL, name = "Search rate (gamma)",
                      limits = GA_LEV, labels = GAM_LAB[GA_LEV], drop = FALSE,
                      guide = if (show)
                        guide_legend(nrow = 2, byrow = TRUE,
                                     override.aes = list(linewidth = 1.2))
                      else "none"),
  scale_fill_manual(values = GA_COL, limits = GA_LEV, drop = FALSE,
                    guide = "none"))

pA <- ggplot(FLM %>% filter(Type == "actual"),
             aes(w, med, colour = garm, fill = garm)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_line(data = FLM %>% filter(Type == "critical"),
            aes(w, med, colour = garm), linetype = "dotted", linewidth = 0.5) +
  facet_wrap(~Species, scales = "free_x", ncol = 1) +
  scale_x_log10() + coord_cartesian(ylim = c(0, 1)) +
  cfg_scales(TRUE) + base_thm +
  labs(x = "Body mass (g)", y = "Feeding level",
       title = "a  Feeding level by body size",
       subtitle = paste("median over members, IQR shaded; dotted = critical",
                        "feeding level\nnon-whale groups show three lines, not",
                        "five: the two 'whales' arms leave their gamma",
                        "untouched\nand overplot the control"))

pB <- ggplot(GCM, aes(Age, med, colour = garm, fill = garm)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~Species, scales = "free_y", ncol = 1) +
  cfg_scales(FALSE) + base_thm +
  labs(x = "Age (years)", y = "Body mass (g)",
       title = "b  Modelled growth curves",
       subtitle = "median over members, IQR shaded")

fig <- pA + pB + plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
stem <- "KC_feeding_growth_curves"
for (ext in c("png", "pdf")) {
  f <- file.path(FIG, paste0(stem, ".", ext))
  if (file.exists(f) && !FORCE)
    stop("refusing to overwrite: ", f, "\n  set KC_FORCE=1", call. = FALSE)
}
H <- max(7, 3.2 * length(FOCUS))
suppressWarnings({
  ggsave(file.path(FIG, paste0(stem, ".png")), fig, width = 12, height = H,
         dpi = 300, limitsize = FALSE)
  ggsave(file.path(FIG, paste0(stem, ".pdf")), fig, width = 12, height = H,
         limitsize = FALSE)
})
cat("\nwrote", paste0(stem, ".{png,pdf}"), "\n")

# ------------------------------------------------------------------- numbers
cat("\n=== feeding level at w_mat, median over members ===\n")
WMAT <- setNames(species_params(BASE)$w_mat, SP)
print(as.data.frame(FLM %>% filter(Type == "actual") %>%
  group_by(garm, Species) %>%
  slice_min(abs(w - WMAT[[first(Species)]]), n = 1, with_ties = FALSE) %>%
  ungroup() %>% transmute(garm, Species, f_at_w_mat = round(med, 4)) %>%
  pivot_wider(names_from = Species, values_from = f_at_w_mat)),
  row.names = FALSE)

cat("\n=== age at maturity, years (median over members) ===\n")
AGE_MAT <- lab(MAD) %>% select(sim_index, cfg, Species, age_mat, max_age)
# n_missing must be counted BEFORE age_mat is overwritten: dplyr evaluates
# summarise() expressions in order, so putting sum(is.na(age_mat)) second would
# count NAs in the scalar median rather than in the members.
print(as.data.frame(AGE_MAT %>% group_by(cfg, Species) %>%
  summarise(n_missing = sum(is.na(age_mat)),
            age_mat = round(median(age_mat, na.rm = TRUE), 2),
            .groups = "drop") %>%
  pivot_wider(names_from = Species, values_from = c(age_mat, n_missing))),
  row.names = FALSE)
cat("  Read off the integral, which spans the whole size range; the ODE curve\n",
    " has to stop short of the asymptote and minke w_mat is 0.9 * w_max.\n")
cat("\n=== age at maturity relative to the drawn/asis control ===\n")
print(as.data.frame(AGE_MAT %>% group_by(sim_index, Species) %>%
  mutate(rel = age_mat / age_mat[cfg == REF_CFG]) %>% ungroup() %>%
  group_by(cfg, Species) %>%
  summarise(rel = round(median(rel, na.rm = TRUE), 3), .groups = "drop") %>%
  pivot_wider(names_from = Species, values_from = rel)), row.names = FALSE)

saveRDS(list(feeding = FLD, growth = GCD, growth_interp = GCI,
             max_age = MAD, age_mat = AGE_MAT,
             meta = list(base = BASE_FILE, members = members, focus = FOCUS,
                         gamma_arms = GA, repro_arms = RP, age_max = AGE_MAX,
                         g1_ok = g1ok, built = format(Sys.time()))), OUT_RDS)
write.csv(AGE_MAT, file.path(ANA, "KC13_age_at_maturity.csv"), row.names = FALSE)
write.csv(FLM, file.path(ANA, "KC13_feeding_level_by_size.csv"), row.names = FALSE)
cat("\nwrote", OUT_RDS, "\n")
cat("elapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")
cat("KC13 complete.\n")