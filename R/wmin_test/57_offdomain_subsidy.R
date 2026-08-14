# =============================================================================
# Phase 57 -- out-of-domain feeding as an ext_encounter subsidy, then
# recalibration with erepro FREE
#
# ISOLATED TRIAL. This changes the energy budget of nine groups and the
# reproduction calibration at once. It is not a refinement of phases 54-56; it
# is a different model, and it writes only new files.
#
# ------------------------------------------------------- why ext_encounter
# The first design extended the resource to 50 t and rescaled
# interaction_resource. It was dropped, because ONE interaction_resource scalar
# multiplies the WHOLE resource spectrum while the resource plays two physically
# different roles that are size-segregated. Measured on the phase-54 state, the
# share of each group's resource encounter coming from ABOVE 79.75 g -- prey it
# could only meet outside the domain -- is:
#
#   orca 100.0   sperm whales 99.3   medium divers 85.3   toothfishes 80.2
#   large divers 74.4   squids 64.5   leopard seals 47.6   flying birds 23.2
#   small divers 16.4   BALEEN 2.4   MINKE 1.0   fishes and LTL ~0
#
# So no single rule works: toothfishes and squids are RESIDENT yet feed at sizes
# where an extended resource is out-of-domain prey, while baleen and minke are
# MIGRATORY yet draw 97.6% and 99.0% of their resource encounter from in-domain
# plankton. The two roles are therefore separated (Kieran's call, 2026-08-12):
#
#   interaction_resource  IN-DOMAIN plankton access -- left at 1, and the
#                         resource array and w_pp_cutoff are NOT touched
#   ext_encounter         OUT-OF-DOMAIN feeding, scaled per group
#
# Residents get zero out-of-domain food by construction; no undepletable 50 t
# prey pool appears inside the domain for every kernel tail to reach; and the
# subsidy is explicitly a subsidy, so it cannot be misread as a food-web link.
#
# ------------------------------------------- time away is NOT feeding away
# p_feed_outside = (time outside the domain) x (fraction of THAT time actually
# feeding). Hauled-out and fasting time contributes ZERO: it is negative energy
# balance funded by reserves, not food from elsewhere. Reasoned per group from
# the actual taxon composition in csvs/predator_group_names_catch_calibration.csv:
#
#  group          taxa                                  out   feed  p_feed_out
#  baleen whales  blue,fin,sei,humpback,S.right         0.50  0.30  0.15
#                 capital breeders; largely fast on migration and breeding grounds
#  minke whales   minke                                 0.25  0.60  0.15
#                 many overwinter in pack ice; migrants feed little
#  sperm whales   sperm                                 0.50  1.00  0.50
#                 year-round deep forager; mesopelagic squid at all latitudes
#  orca           orca                                  0.67  0.90  0.60
#                 year-round feeder; brief non-feeding moult migrations
#  leopard seals  leopard                               0.25  1.00  0.25
#                 hunts actively at subantarctic islands; its moult haul-out is
#                 on ice INSIDE the domain
#  large divers   ELEPHANT SEALS ONLY                   0.67  0.67  0.45
#                 breeding haul-out (F ~4 wk, M up to 3 mo) + moult (~4-5 wk),
#                 all ashore fasting on subantarctic islands, ~0.2 of the year
#  medium divers  crabeater,Ross,Weddell,fur seal,      0.464 0.75  0.35
#                 emperor+king penguin,dolphins,ziphiids
#                 pack-ice seals feed year round; fur seals fast through the
#                 breeding haul-out; king penguins have long adult fasts
#  small divers   Adelie,crested,gentoo                 0.39  0.80  0.31
#                 winter dispersal is feeding; crested fast ~25 d for the moult
#  flying birds   small,med,large,coastal               0.6125 0.95 0.58
#                 pelagic seabirds feed year round; partners alternate incubation
#  residents      fishes, squids, toothfishes, all LTL  0     --    0
#
# NOT REPO DATA. The source sheet (excel data/predator_parameter_collating_v1.xlsx,
# sheet interaction_factors) carries time-in-area and dive depth only. These
# feeding fractions are literature-informed judgement and are an ASSUMPTION
# LAYER that must be declared in the methods.
#
# TWO KNOWN WEAKNESSES, neither introduced here. Emperor penguin's ~115 d
# incubation fast and Adelie incubation shifts happen IN-domain, so they do not
# reduce out-of-domain feeding -- but the model has no seasonality, so those
# groups are over-fed in-domain regardless. And `medium divers` pools eight taxa
# spanning pack-ice seals, a fur seal, two penguins, dolphins and beaked whales;
# its 0.5356 p_time is a plain unweighted mean and any single number for it is a
# compromise.
#
# WHAT THIS CAN AND CANNOT DO. The largest subsidies land on orca and sperm
# whales, which also carry the worst reproduction clamps (0.9986, 0.9896).
# BALEEN AND MINKE GET LITTLE by construction. If the aim is to loosen the whale
# reproduction calibration specifically, expect this to disappoint.
#
# erepro IS FREE. Every previous phase ran steady(preserve = "erepro"), which is
# why reproduction level is pinned near 1 almost everywhere. `preserve` takes ONE
# of "erepro"/"R_max"/"reproduction_level"; all three run, with "erepro" as the
# like-for-like control.
#
# USAGE  Rscript R/wmin_test/57_offdomain_subsidy.R
# ENV    P57_BASE, P57_WREF (5e7), P57_PRESERVE, P57_FEEDOUT (csv override)
# =============================================================================

suppressPackageStartupMessages({library(mizer); library(therMizer)})
source(file.path("R", "check_size_params.R"))
source(file.path("R", "wmin_test", "thermizer_shim.R"))

BASE <- Sys.getenv("P57_BASE", "params_ref_sw2000_balror_mnkfish05.rds")
# The out-of-domain environment is assumed to have the SAME size-spectrum
# productivity as the domain, extended to WREF so it covers the preferred prey
# of the largest predators (orca prefer 17-19 t). Used ONLY to measure an
# encounter rate; the model's own resource is never changed.
WREF <- as.numeric(Sys.getenv("P57_WREF", "5e7"))
# IN-DOMAIN resource ceiling. The default sweeps the 1 g trial against the
# current 100 g as a paired control. At 100 g the resource runs to 79.75 g and
# so DUPLICATES prey the dynamic groups already represent -- antarctic krill
# w_max 4.17 g, salps 25.1 g, other macrozooplankton 1 g -- i.e. it is partly
# double-counting the modelled community. Cutting at 1 g leaves it as what it
# should be: phytoplankton and the small plankton the dynamic groups do not
# cover. It is a LARGE cut: baleen and minke take 43.5% and 49.7% of intake from
# the resource and their preferred prey is 1.7-3.5 g, i.e. above 1 g.
# plankton_forcing applies the cutoff itself, so no array edit is needed.
WCUTS <- as.numeric(trimws(strsplit(Sys.getenv("P57_WCUT", "1,100"), ",")[[1]]))
PRESERVE <- trimws(strsplit(Sys.getenv(
  "P57_PRESERVE", "erepro,R_max,reproduction_level"), ",")[[1]])
FEEDOUT_CSV <- Sys.getenv("P57_FEEDOUT", "")
out_dir <- file.path("Output_large_files", "wmin_test")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
tag <- function(x) file.path(out_dir, paste0("57_", x))

STEADY_TMAX <- 1000
TOL_LADDER  <- c(0.1, 0.05, 0.01, 0.005)
STEADY_TOL  <- 0.005
MATCH_ROUNDS <- 6; TARGET_ROUNDS <- 14

# One line per group with its basis and an indicative source, written out to
# 57_p_feed_outside.csv for review. Override wholesale with a csv (columns:
# species, p_feed_outside) via P57_FEEDOUT.
#
# ############################################################################
# # THE SOURCES BELOW ARE INDICATIVE AND UNVERIFIED. They are recalled, not
# # checked against the papers, and at least some will be wrong in detail
# # (year, exact figure, or the claim attributed). EVERY ONE MUST BE CHECKED
# # before it is cited. They are here to point the review at the right
# # literature, not to stand as citations.
# ############################################################################
FEED_DEF <- data.frame(stringsAsFactors = FALSE, rbind(
  c("mesozooplankton", 0, "resident", "-"),
  c("other krill", 0, "resident", "-"),
  c("other macrozooplankton", 0, "resident", "-"),
  c("antarctic krill", 0, "resident", "-"),
  c("salps", 0, "resident", "-"),
  c("mesopelagic fishes", 0, "resident", "-"),
  c("bathypelagic fishes", 0, "resident", "-"),
  c("shelf and coastal fishes", 0, "resident", "-"),
  c("flying birds", 0.58,
    "0.6125 outside x ~0.95 feeding; pelagic seabirds forage year round at sea, partners alternate incubation shifts",
    "Warham 1990 The Petrels; Weimerskirch 1998 on foraging/incubation alternation"),
  c("small divers", 0.31,
    "0.39 outside x ~0.80 feeding; winter dispersal is foraging, but crested penguins fast ~25 d for the catastrophic moult",
    "Brown 1985 (penguin moult fasting); Ballard et al. 2010 (Adelie winter dispersal)"),
  c("squids", 0, "resident", "-"),
  c("toothfishes", 0, "resident", "-"),
  c("leopard seals", 0.25,
    "0.25 outside x ~1.0 feeding; hunts actively at subantarctic islands, and its moult/rest haul-out is on ice INSIDE the domain",
    "Boveng et al. 1998 (predation on fur seal pups, Seal Island); Rogers 2009"),
  c("medium divers", 0.35,
    "0.464 outside x ~0.75 feeding; pack-ice seals feed year round, but Antarctic fur seals fast through the breeding haul-out and king penguins have long adult fasts. WEAKEST NUMBER IN THE TABLE - 8 taxa pooled",
    "Boyd et al. 1991 (fur seal male breeding fast); Cherel et al. 1993 (king penguin fasting); Bengtson & Stewart 1992 (pack-ice seals)"),
  c("large divers", 0.45,
    "0.67 outside x ~0.67 feeding; ELEPHANT SEALS ONLY - breeding haul-out (F ~4 wk, M up to 3 mo) plus moult (~4-5 wk) ashore fasting on subantarctic islands, ~0.2 of the year",
    "Hindell et al. 1991; Boyd et al. 1993; McMahon et al. 2005 (southern elephant seal haul-out and fasting)"),
  c("minke whales", 0.15,
    "0.25 outside x ~0.60 feeding; many Antarctic minke overwinter in the pack ice, and migrants feed little",
    "Thiele et al. 2004; Ainley et al. 2007 (minke in winter pack ice)"),
  c("orca", 0.60,
    "0.67 outside x ~0.90 feeding; year-round feeder, but makes rapid non-feeding round trips to subtropical water for skin moult",
    "Durban & Pitman 2012 Biol. Lett. (physiological maintenance migrations)"),
  c("sperm whales", 0.50,
    "0.50 outside x ~1.0 feeding; year-round deep forager on mesopelagic squid, available at all latitudes; only mature males reach high latitudes",
    "Whitehead 2003 Sperm Whales: Social Evolution in the Ocean"),
  c("baleen whales", 0.15,
    "0.50 outside x ~0.30 feeding; capital breeders that acquire most annual energy in the polar summer and largely fast on migration and breeding grounds",
    "Lockyer 1981 FAO (energy budgets of Southern Hemisphere baleen whales); Braithwaite et al. 2015 (humpback fasting)")))
names(FEED_DEF) <- c("species", "p_feed_outside", "basis", "source_UNVERIFIED")
FEED_DEF$p_feed_outside <- as.numeric(FEED_DEF$p_feed_outside)
FEED_OUT <- setNames(FEED_DEF$p_feed_outside, FEED_DEF$species)

t0 <- proc.time()
cat("=== Phase 57: out-of-domain feeding subsidy + free erepro ===\n")
cat("base:", BASE, "| reference prey ceiling:", WREF,
    "g | preserve arms:", paste(PRESERVE, collapse = ", "), "\n\n")

base <- suppressWarnings(validParams(readRDS(BASE)))
stopifnot(identical(base@rates_funcs$Encounter, "therMizerEncounter"),
          identical(base@resource_dynamics, "plankton_forcing"),
          identical(base@second_order_w$flux, "upwind"))
SPN <- base@species_params$species
if (!all(base@species_params$interaction_resource == 1))
  stop("base must have interaction_resource = 1 for all groups", call. = FALSE)
if (any(base@ext_encounter != 0))
  stop("base already carries a non-zero ext_encounter", call. = FALSE)

if (nzchar(FEEDOUT_CSV)) {
  ov <- read.csv(FEEDOUT_CSV, stringsAsFactors = FALSE)
  FEED_OUT <- setNames(ov$p_feed_outside, ov$species)
  cat("p_feed_outside overridden from", FEEDOUT_CSV, "\n")
}
if (length(setdiff(SPN, names(FEED_OUT))))
  stop("no p_feed_outside for: ",
       paste(setdiff(SPN, names(FEED_OUT)), collapse = ", "), call. = FALSE)
p_out <- unname(FEED_OUT[SPN])
stopifnot(all(p_out >= 0), all(p_out < 1))

# --- the out-of-domain encounter field ----------------------------------------
# A THROWAWAY params whose resource reaches WREF, with the modelled prey zeroed
# so only the resource contributes. The result is the encounter each group would
# experience in an out-of-domain environment of the same productivity across its
# whole preferred prey range.
extend_ref <- function(p, wcut) {
  a <- p@other_params$other$n_pp_array; w <- p@w_full
  top <- max(which(is.finite(a[1, ])))
  ext <- which(w > w[top] & w <= wcut)
  for (i in seq_len(nrow(a))) {
    fi <- which(is.finite(a[i, ])); sel <- fi[w[fi] > 1e-4]
    sl <- unname(coef(lm(a[i, sel] ~ log10(w[sel])))[2])
    a[i, ext] <- a[i, top] + sl * (log10(w[ext]) - log10(w[top]))
  }
  p@other_params$other$n_pp_array <- a
  p@resource_params$w_pp_cutoff <- wcut
  p@initial_n_pp <- plankton_forcing(p, t = 1841)
  p
}
pref <- extend_ref(base, WREF)
n0 <- pref@initial_n; n0[] <- 0
E_ref <- mizer::mizerEncounter(pref, n = n0, n_pp = pref@initial_n_pp,
                               n_other = pref@initial_n_other, t = 0)
stopifnot(identical(dim(E_ref), dim(base@ext_encounter)))

EXT <- sweep(E_ref, 1, p_out, "*")
# The out-of-domain reference spans the FULL size range up to WREF even when the
# in-domain resource is cut to 1 g. That is deliberate, not an inconsistency:
# in-domain the model represents everything above ~1 g explicitly as dynamic
# groups, so the resource there would double-count; out-of-domain it represents
# nothing, so the subsidy has to cover the whole prey range.
build <- function(wcut) {
  p <- base
  p@ext_encounter <- EXT
  p@resource_params$w_pp_cutoff <- wcut
  p@initial_n_pp <- plankton_forcing(p, t = 1841)
  # what this design promises NOT to touch
  stopifnot(identical(p@other_params$other$n_pp_array,
                      base@other_params$other$n_pp_array),
            all(p@species_params$interaction_resource == 1),
            identical(p@rates_funcs$Encounter, "therMizerEncounter"),
            identical(p@resource_dynamics, "plankton_forcing"))
  p
}
share <- function(p) {
  d <- ther_diet(p, n = p@initial_n, n_pp = p@initial_n_pp,
                 n_other = p@initial_n_other, year = 1841)
  E <- p@initial_n * rep(p@dw, each = nrow(p@initial_n))
  C <- apply(d, 3, function(mm) rowSums(mm * E))
  dimnames(C) <- list(SPN, dimnames(d)$prey)
  data.frame(total = rowSums(C),
             res_pct = 100 * C[, "Resource"] / rowSums(C),
             ext_pct = 100 * C[, "External"] / rowSums(C))
}
s0 <- share(base)
cat("\n--- effect of each in-domain resource ceiling, before recalibration ---\n")
PRE <- do.call(rbind, lapply(WCUTS, function(wc) {
  p <- build(wc); s <- share(p)
  nz <- p@initial_n_pp > 0
  cat(sprintf("\n  w_pp_cutoff = %g g -> resource tops out at %.4g g\n", wc,
              max(p@w_full[nz])))
  print(data.frame(species = SPN, p_feed_outside = p_out,
                   res_pct_base = round(s0$res_pct, 1),
                   res_pct_now = round(s$res_pct, 1),
                   subsidy_pct = round(s$ext_pct, 1),
                   intake_x = round(s$total / s0$total, 3)), row.names = FALSE)
  data.frame(w_pp_cutoff = wc, species = SPN, res_pct = s$res_pct,
             ext_pct = s$ext_pct, intake_x = s$total / s0$total)
}))
write.csv(PRE, tag("precalibration_intake.csv"), row.names = FALSE)

# --- the ladder ---------------------------------------------------------------
bio_ratio <- function(p)
  as.numeric(getBiomass(p, use_cutoff = TRUE) / p@species_params$biomass_observed)
max_dev <- function(p) max(abs(bio_ratio(p) - 1))
steady_guarded <- function(p, tol, pres) {
  nc <- FALSE
  out <- withCallingHandlers(
    try(steady(p, tol = tol, t_max = STEADY_TMAX, preserve = pres,
               progress_bar = FALSE), silent = TRUE),
    message = function(m) {
      if (grepl("did not converge", conditionMessage(m), ignore.case = TRUE))
        nc <<- TRUE
      invokeRestart("muffleMessage")
    }, warning = function(w) invokeRestart("muffleWarning"))
  list(params = out, converged = !nc, errored = inherits(out, "try-error"))
}
match_guarded <- function(p) {
  out <- withCallingHandlers(try(matchBiomasses(p), silent = TRUE),
    warning = function(w) invokeRestart("muffleWarning"))
  list(params = out, errored = inherits(out, "try-error"))
}
recalibrate <- function(p, pres) {
  cat("\n########  preserve = ", pres, "  ########\n", sep = "")
  cat(sprintf("   start: max dev %.5f\n", max_dev(p)))
  st <- steady_guarded(p, TOL_LADDER[1], pres)
  if (st$errored) { cat("   steady() ERRORED on the leading call\n")
    return(list(params = NULL, reason = "steady_error_leading")) }
  p <- st$params
  best <- NULL; best_d <- Inf
  for (tol in TOL_LADDER) {
    at <- isTRUE(all.equal(tol, STEADY_TOL))
    for (r in seq_len(if (at) TARGET_ROUNDS else MATCH_ROUNDS)) {
      mt <- match_guarded(p)
      if (mt$errored) { cat("   matchBiomasses() ERRORED\n")
        return(list(params = NULL, reason = "match_error")) }
      p <- mt$params
      st <- steady_guarded(p, tol, pres)
      if (st$errored) { cat("   steady() ERRORED\n")
        return(list(params = NULL, reason = "steady_error")) }
      p <- st$params
      d <- max_dev(p)
      if (at && st$converged && d < best_d) { best_d <- d; best <- p }
      cat(sprintf("   tol=%-6.3g r=%-2d max dev %.5f  max erepro %.4f  %s\n",
                  tol, r, d, max(p@species_params$erepro),
                  if (st$converged) "conv" else "NO-CONV"))
      flush.console()
    }
  }
  if (is.null(best)) { cat("   no state converged at the target tolerance\n")
    return(list(params = NULL, reason = "no_converged_state")) }
  cat(sprintf("   selected: max dev %.5f\n", best_d))
  list(params = best, reason = NA_character_, max_dev = best_d)
}

RES <- list(); REP <- list()
for (wc in WCUTS) {
  revised <- build(wc)
  saveRDS(revised, tag(sprintf("subsidised_precal_wcut%g.rds", wc)))
  for (pres in PRESERVE) {
    cat("\n=================  w_pp_cutoff =", wc, "g  =================")
    r <- recalibrate(revised, pres)
    ok <- !is.null(r$params)
    RES[[length(RES) + 1]] <- data.frame(
      w_pp_cutoff = wc, preserve = pres, calibrated = ok,
      reason = if (ok) "ok" else r$reason,
      max_dev = if (ok) r$max_dev else NA_real_,
      max_erepro = if (ok) max(r$params@species_params$erepro) else NA_real_,
      n_erepro_ge1 = if (ok) sum(r$params@species_params$erepro >= 1) else NA_integer_,
      stringsAsFactors = FALSE)
    if (ok) {
      f <- sprintf("params_ref_p57_subsidy_wcut%g_%s.rds", wc, pres)
      if (file.exists(f)) cat("  NOTE: kept existing", f, "\n") else
        saveRDS(r$params, f)
      cat("  wrote", f, "\n")
      rl <- try(getReproductionLevel(r$params), silent = TRUE)
      REP[[length(REP) + 1]] <- data.frame(
        arm = sprintf("wcut%g_%s", wc, pres), w_pp_cutoff = wc,
        preserve = pres, species = SPN,
        erepro = r$params@species_params$erepro,
        R_max = r$params@species_params$R_max,
        repro_level = if (inherits(rl, "try-error")) NA_real_ else as.numeric(rl),
        bio_ratio = bio_ratio(r$params))
    }
  }
}

SUM <- do.call(rbind, RES)
cat("\n=== phase 57 summary ===\n"); print(SUM, row.names = FALSE, digits = 4)
if (length(REP)) {
  RP <- do.call(rbind, REP)
  wide <- function(v) {
    w <- reshape(RP[, c("arm", "species", v)], idvar = "species",
                 timevar = "arm", direction = "wide")
    names(w) <- sub(paste0(v, "\\."), "", names(w)); w
  }
  b_rl <- try(as.numeric(getReproductionLevel(base)), silent = TRUE)
  cat("\n=== reproduction level (RDD / R_max; 1 = pinned at R_max) ===\n")
  rw <- wide("repro_level")
  if (!inherits(b_rl, "try-error"))
    rw <- cbind(rw[1], phase54 = round(b_rl[match(rw$species, SPN)], 4), rw[-1])
  print(rw, row.names = FALSE, digits = 4)
  cat("\n=== erepro ===\n")
  ew <- wide("erepro")
  print(cbind(ew[1], phase54 = round(base@species_params$erepro[
    match(ew$species, SPN)], 4), ew[-1]), row.names = FALSE, digits = 4)
  write.csv(RP, tag("reproduction.csv"), row.names = FALSE)
}
write.csv(SUM, tag("summary.csv"), row.names = FALSE)
write.csv(FEED_DEF[match(SPN, FEED_DEF$species), ], tag("p_feed_outside.csv"),
          row.names = FALSE)
cat("\nassumption table with sources -> ", tag("p_feed_outside.csv"), "\n",
    "  SOURCES ARE UNVERIFIED -- check every one before citing.\n", sep = "")
cat("\nelapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")
cat("Phase 57 complete.\n")
