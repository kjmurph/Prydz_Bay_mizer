# =============================================================================
# KC05 -- abundance (numbers) and mean individual mass for the four krill
# counterfactual arms
#
# WHY THIS EXISTS. KC02 recorded biomass only (KC02_ensemble_pilot.R:216-218).
# Numbers -- and therefore mean individual mass, which is biomass / numbers --
# are not recoverable from KC02_n167_from1841_scenarios.rds. KC06 needs both to
# build the scenario-resolved version of Figure 3.
#
# WHY IT IS CHEAP. The 162 converged member states are already on disk, each
# holding the post-steady params, the REFIT-catchability params, and the state
# at the end of the 118-year unfished spin-up. Re-projecting from a stored state
# skips steady(), the spin-up and ther_diet() -- the three things that made KC02
# cost 58.6 min. This is projection only.
#
# NO CATCHABILITY REFIT HERE. The stored `params_refit` already carries the
# 45_catchability_multipliers.rds multipliers (KC02:198-201). Applying refit_q()
# again would square the krill multiplier of 0.1904 and quietly make the fishery
# 5x too weak. Use the object as stored.
#
# NO thermizer_shim.R EITHER. KC02's worker sources it for ther_diet(), which is
# local project code. Every rate function these params actually name --
# therMizerEncounter, plankton_forcing -- is exported by therMizer itself, so
# projection needs the package and nothing else. Checked at startup.
#
# TWO FATAL GATES, the F00/MA02 pattern:
#   G1 the recomputed Biomass must reproduce KC02's stored biomass to a max
#      relative difference of ~0. A stored member re-projects deterministically
#      because the spin-up is unfished, so anything else means the protocol has
#      drifted and nothing derived from Abundance can be trusted.
#   G2 the member set and the stable flags are CARRIED from KC02, not
#      recomputed, so KC04 and KC06 rest on the same 156 members.
#
# OUTPUT goes to Output_large_files/ (gitignored), not analysis/ -- this object
# is ~60 MB and analysis/ is a tracked directory.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC05_extract_abund_mass.R" [run|collect|status]
# ENV    KC_CORES (default 14), KC_CHUNK, KC_IN
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OUT_LARGE <- "Output_large_files/wmin_test"
KC_ROOT   <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
ANA       <- file.path(KC_ROOT, "analysis")
IN        <- Sys.getenv("KC_IN", "KC02_n167_from1841_scenarios.rds")
STEM      <- "KC05_abund_mass"
STATE_DIR <- file.path(OUT_LARGE, "KC02_n167_from1841_states")
CHUNK_DIR <- file.path(OUT_LARGE, paste0(STEM, "_chunks"))
OUT_RDS   <- file.path(OUT_LARGE, "KC05_abund_mass_n162.rds")
# 16 cores on this machine; never commit all of them -- saturating them risks a
# hard shutdown that kills the in-flight run too.
CORES <- min(as.integer(Sys.getenv("KC_CORES", "14")), 15)
mode  <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(mode)) mode <- "run"
stopifnot(mode %in% c("run", "collect", "status"))
dir.create(CHUNK_DIR, recursive = TRUE, showWarnings = FALSE)

KRILL <- "antarctic krill"; END_YEAR <- 2010; SPINUP_YEARS <- 118

t0 <- proc.time()
cat("=== KC05: abundance + mean mass for the four counterfactual arms ===\n")

# every rate function these params name must come from the installed therMizer
for (fn in c("therMizerEncounter", "plankton_forcing"))
  if (!exists(fn)) stop("rate function not on the search path: ", fn,
                        " -- is therMizer installed?", call. = FALSE)

D <- readRDS(file.path(ANA, IN))
MEM <- D$members; meta <- D$meta
members <- MEM$sim_index
cat("members from", IN, ":", nrow(MEM), "| stable:", sum(MEM$stable), "\n")

state_file <- function(si) file.path(STATE_DIR, sprintf("kc02_state_%05d.rds", si))
missing <- members[!file.exists(vapply(members, state_file, ""))]
if (length(missing))
  stop("no stored state for member(s): ", paste(missing, collapse = ", "),
       call. = FALSE)

# --- the four effort arms, rebuilt exactly as KC02:96-110 --------------------
eff_obs <- readRDS("effort_array_1841_2010.rds")
yrs_eff <- as.numeric(rownames(eff_obs)); kr_eff <- eff_obs[, KRILL]
peak_year <- yrs_eff[which.max(kr_eff)]; peak_val <- max(kr_eff)
if (peak_year != meta$peak_year)
  stop("peak year disagrees with KC02's meta: ", peak_year, " vs ",
       meta$peak_year, call. = FALSE)
eff_none <- eff_obs; eff_none[, KRILL] <- 0
eff_peak <- eff_obs; eff_peak[yrs_eff >= peak_year, KRILL] <- peak_val
ARM_EFF <- list(unexploited = NULL, exploited = eff_obs, no_krill = eff_none,
                peak_krill = eff_peak)
cat("arms:", paste(names(ARM_EFF), collapse = ", "), "| peak", peak_year,
    "| cores", CORES, "\n")

# ------------------------------------------------------------------ worker ---
worker <- function(si) {
  suppressPackageStartupMessages({ library(mizer); library(therMizer) })
  st <- try(readRDS(file.path(STATE_DIR, sprintf("kc02_state_%05d.rds", si))),
            silent = TRUE)
  if (inherits(st, "try-error"))
    return(list(sim_index = si, ok = FALSE, reason = "state_read_error"))
  pq <- st$params_refit          # ALREADY refit -- do not touch catchability
  init <- st$initial_n
  SP <- pq@species_params$species
  dw <- pq@dw; wdw <- pq@w * pq@dw

  out <- list()
  for (nm in names(ARM_EFF)) {
    e <- ARM_EFF[[nm]]
    sim <- try(if (is.null(e))
      project(pq, initial_n = init, t_start = 1841, t_max = 169, effort = 0,
              progress_bar = FALSE)
      else project(pq, initial_n = init, t_start = 1841, effort = e,
                   progress_bar = FALSE), silent = TRUE)
    if (inherits(sim, "try-error"))
      return(list(sim_index = si, ok = FALSE,
                  reason = paste0("projection_error_", nm)))
    yr <- as.numeric(dimnames(sim@n)$time); ki <- which(yr <= END_YEAR)
    out[[nm]] <- do.call(rbind, lapply(ki, function(j) data.frame(
      sim_index = si, arm = nm, Year = yr[j], Species = SP,
      Abundance = as.numeric(sim@n[j, , ] %*% dw),
      Biomass   = as.numeric(sim@n[j, , ] %*% wdw),
      stringsAsFactors = FALSE)))
  }
  list(sim_index = si, ok = TRUE, reason = "ok",
       data = do.call(rbind, out))
}

# --------------------------------------------------------------------- run ---
# Chunked and RESUMABLE, transcribed from KC02:253-302. Each chunk writes its
# own file; a re-run skips what is already on disk. `status` touches nothing and
# is safe to call while a run is in flight.
CHUNK <- as.integer(Sys.getenv("KC_CHUNK", as.character(CORES)))
chunks <- split(members, ceiling(seq_along(members) / CHUNK))
chunk_file <- function(ci) file.path(CHUNK_DIR, sprintf("res_%03d.rds", ci))

if (mode == "status") {
  done <- vapply(seq_along(chunks), function(ci) file.exists(chunk_file(ci)),
                 logical(1))
  cat(sprintf("chunks %d/%d complete | members %d/%d\n", sum(done),
              length(chunks),
              sum(vapply(seq_along(chunks)[done],
                         function(ci) length(readRDS(chunk_file(ci))), integer(1))),
              length(members)))
  if (sum(done) < length(chunks))
    cat("  next incomplete chunk:", which(!done)[1], "of", length(chunks), "\n")
  quit(save = "no")
}

if (mode == "run") {
  todo <- which(!vapply(seq_along(chunks),
                        function(ci) file.exists(chunk_file(ci)), logical(1)))
  cat("chunks:", length(chunks), "of", CHUNK, "members |", length(todo),
      "to run\n\n")
  if (length(todo)) {
    cl <- makeCluster(CORES)
    on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
    clusterExport(cl, c("ARM_EFF", "STATE_DIR", "END_YEAR"),
                  envir = environment())
    for (k in seq_along(todo)) {
      ci <- todo[k]; tc <- proc.time()
      r <- parLapply(cl, chunks[[ci]], worker)
      saveRDS(r, chunk_file(ci))
      el <- (proc.time() - tc)[["elapsed"]] / 60
      tot <- (proc.time() - t0)[["elapsed"]] / 60
      cat(sprintf("[%s] chunk %d/%d done (%d members, %.1f min) | elapsed %.1f min | ETA %.1f min\n",
                  format(Sys.time(), "%H:%M:%S"), ci, length(chunks),
                  length(chunks[[ci]]), el, tot, tot / k * (length(todo) - k)))
      flush.console()
    }
    stopCluster(cl)
  }
}

res <- unlist(lapply(seq_along(chunks), function(ci) {
  f <- chunk_file(ci); if (file.exists(f)) readRDS(f) else NULL
}), recursive = FALSE)
if (!length(res)) stop("no chunk results found in ", CHUNK_DIR)
ok <- vapply(res, function(r) isTRUE(r$ok), logical(1))
cat("\nassembled", length(res), "member results |", sum(ok), "ok\n")
if (any(!ok))
  cat("  failures:", paste(vapply(res[!ok], function(r)
    sprintf("%d(%s)", r$sim_index, r$reason), ""), collapse = ", "), "\n")
if (!all(ok)) stop("some members failed to re-project -- refusing to write",
                   call. = FALSE)

AM <- do.call(rbind, lapply(res, `[[`, "data"))
cat("rows:", format(nrow(AM), big.mark = ","), "| species:",
    length(unique(AM$Species)), "| years", min(AM$Year), "-", max(AM$Year), "\n")

# =============================================================================
# GATES
# =============================================================================
cat("\n=== gates ===\n")

# G2 -- membership carried, not recomputed
if (!setequal(unique(AM$sim_index), members))
  stop("G2 FAILED: re-projected member set differs from KC02's", call. = FALSE)
cat(sprintf("  [PASS] G2 membership                 %d members, %d stable, identical to %s\n",
            length(members), sum(MEM$stable), IN))

# G1 -- bit-exactness against KC02's stored biomass
J <- inner_join(AM, D$biomass, by = c("sim_index", "arm", "Year", "Species"))
if (nrow(J) != nrow(D$biomass))
  stop("G1 FAILED: joined ", nrow(J), " rows against ", nrow(D$biomass),
       " stored -- the key does not line up", call. = FALSE)
den <- pmax(abs(J$biomass_g), .Machine$double.xmin)
rel <- max(abs(J$Biomass - J$biomass_g) / den)
abs_norm <- max(abs(J$Biomass - J$biomass_g)) / max(abs(J$biomass_g))
cat(sprintf("  [%s] G1 biomass vs KC02             max rel %.3g | max abs / scale %.3g (%s rows)\n",
            if (rel < 1e-12) "PASS" else "FAIL", rel, abs_norm,
            format(nrow(J), big.mark = ",")))
if (rel >= 1e-12)
  stop("G1 FAILED: the re-projection does not reproduce KC02's biomass, so ",
       "nothing derived from Abundance is trustworthy", call. = FALSE)

# =============================================================================
# write
# =============================================================================
meta5 <- list(source = IN, params = meta$params, states = STATE_DIR,
              n_members = length(members), n_stable = sum(MEM$stable),
              peak_year = peak_year, spinup_years = SPINUP_YEARS,
              arms = names(ARM_EFF), gate_G1_max_rel = rel,
              built = format(Sys.time()))
saveRDS(list(abund_mass = AM, members = MEM, meta = meta5), OUT_RDS)
cat("\nwrote", OUT_RDS, sprintf("(%.0f MB)\n",
    file.info(OUT_RDS)$size / 1024^2))

# small human-readable summary alongside the tracked analysis outputs
SUMM <- AM %>% filter(sim_index %in% MEM$sim_index[MEM$stable],
                      Species %in% c("baleen whales", "minke whales"),
                      Year %in% 2001:2010) %>%
  group_by(sim_index, arm, Species) %>%
  summarise(Abundance = mean(Abundance), Biomass = mean(Biomass),
            .groups = "drop") %>%
  mutate(MeanMass = Biomass / Abundance) %>%
  group_by(arm, Species) %>%
  summarise(across(c(Abundance, Biomass, MeanMass), median), .groups = "drop")
write.csv(SUMM, file.path(ANA, "KC05_whale_abund_mass_2001_2010.csv"),
          row.names = FALSE)
cat("\n=== whales, 2001-2010 median across stable members ===\n")
print(as.data.frame(SUMM %>%
  mutate(across(where(is.numeric), ~signif(.x, 5)))), row.names = FALSE)
cat("\nelapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")
cat("KC05 complete.\n")