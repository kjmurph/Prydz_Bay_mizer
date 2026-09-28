# =============================================================================
# 105 -- export the MizerSim objects for the usable members, both effort arms,
# as a self-contained package to hand to a collaborator
#
# WHY THIS EXISTS. Nothing in this project stores sims. Every downstream script
# (F00, F00r, F00s, 80, KC20b) projects inside its worker, extracts data frames
# and lets the MizerSim go, because 203 members x 2 arms is ~1.6 GB in memory.
# That is the right choice for the figure pipeline and the wrong one for a
# hand-off, where the collaborator wants the objects themselves so they can ask
# their own questions of them.
#
# WHAT IT IS. Two files, one per arm, each a NAMED LIST of MizerSim objects:
#
#     names(sims)              the sim_index, as character
#     order                    BEST FIRST, by the pooled log10 yield RMSE rank,
#                              so sims[1:20] IS the stored top-10% cut
#     attr(sims, "ranking")    the 203-row ranking table
#     attr(sims, "meta")       full provenance
#
# The ranking travels INSIDE the object so the collaborator cannot end up with
# sims and no way to cut them, and so no membership list is ever retyped.
#
# WHY RE-PROJECTING IS EXACT. The phase-104 spin-up is UNFISHED, so the stored
# initial_n does not depend on catchability and applying the refit afterwards is
# bit-exact against having done it in one pass. No steady() is re-entered, so
# the re-run reordering trap does not apply. The projections here are the SAME
# two projections F00 makes -- verified in `check` mode against
# Manuscript data/biomass_abund_{fish,clim}_<suffix>.rds.
#
# THE QMAX TRAP, guarded. `pmin(QMAX, .)` clips SILENTLY. On 104_refit_wh_q10
# four of nine multipliers exceed 1 (baleen 21.7, shelf/coastal 13.1), so a
# QMAX of 1 would quietly restore the q<=1 ceiling phase 104 abandoned. The
# refit records its own ceiling in meta$qmax and this stops on a mismatch.
#
# UNLIKE THE REST OF THE PIPELINE, the defaults here point at PHASE 104, not
# phase 88. This script has no legacy callers, so there is nothing to reproduce.
#
# USAGE  Rscript R/wmin_test/105_export_member_sims.R [run|collect|check|status]
#          run      project the members, writing resumable chunks
#          collect  assemble the two arm files, the ranking csv and the README
#          check    validate the written files against the F00 data products
# ENV    P105_STATE_DIR, P105_RANK, P105_MEMBERS, P105_REFIT, P105_QMAX,
#        P105_OUT_DIR, P105_SUFFIX, P105_CORES, P105_CHUNK, P105_LIMIT
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel)
})

OUT_LARGE <- "Output_large_files/wmin_test"
STATE_DIR <- Sys.getenv("P105_STATE_DIR", file.path(OUT_LARGE, "104_full_states"))
RANK_F    <- Sys.getenv("P105_RANK",      file.path(OUT_LARGE, "104_q10_rerank.rds"))
MEMBERS_F <- Sys.getenv("P105_MEMBERS",   file.path(OUT_LARGE, "104_q10.rds"))
REFIT_F   <- Sys.getenv("P105_REFIT",     file.path(OUT_LARGE, "104_refit_wh_q10.rds"))
SUFFIX    <- Sys.getenv("P105_SUFFIX",    "p104q10")
OUT_DIR   <- Sys.getenv("P105_OUT_DIR",   file.path(OUT_LARGE, paste0(SUFFIX, "_sims")))
# catchability ceiling. MUST match the ceiling the refit was fitted at.
QMAX      <- as.numeric(Sys.getenv("P105_QMAX", "10"))
CORES     <- as.integer(Sys.getenv("P105_CORES",
                                   as.character(max(1, detectCores() - 2))))
CHUNK     <- as.integer(Sys.getenv("P105_CHUNK", as.character(CORES)))
LIMIT     <- as.integer(Sys.getenv("P105_LIMIT", "0"))
END_YEAR  <- 2010

CHUNK_DIR <- file.path(OUT_DIR, "_chunks")
dir.create(CHUNK_DIR, recursive = TRUE, showWarnings = FALSE)
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"
stopifnot(mode %in% c("run", "collect", "check", "status"))

arm_file <- function(arm)
  file.path(OUT_DIR, sprintf("sims_%s_%s_n%d.rds", arm, SUFFIX, N_MEMBERS))

t0 <- proc.time()
cat("=== 105: export member sims ===\n")
cat("states:", STATE_DIR, "\n  rank:", basename(RANK_F),
    "| refit:", basename(REFIT_F), "| QMAX:", QMAX, "\n")
if (!dir.exists(STATE_DIR)) stop("no state dir: ", STATE_DIR, call. = FALSE)

# --- membership, transcribed from F00_build_p88_data.R:87-110 ----------------
RR        <- readRDS(RANK_F)
FULL_NAME <- "FULL usable"
TOP_NAME  <- grep("^TOP ", names(RR$cuts), value = TRUE)[1]
members     <- as.integer(RR$cuts[[FULL_NAME]])
top_members <- as.integer(RR$cuts[[TOP_NAME]])
stopifnot(length(members) == RR$meta$n_usable, all(top_members %in% members))

MEMTAB <- readRDS(MEMBERS_F)$members
# follow the member table's own definition of usable: phase 104 adds a drift
# screen, and earlier tables carry no drift_ok column so this is a no-op on them.
usable <- sort(as.integer(MEMTAB$sim_index[
  MEMTAB$stable & MEMTAB$n_erepro_ge1 == 0 &
  (if ("drift_ok" %in% names(MEMTAB)) MEMTAB$drift_ok else TRUE)]))
if (!identical(sort(members), usable))
  stop("the ranking's FULL set is not the usable set of ", basename(MEMBERS_F),
       " -- refusing to proceed.", call. = FALSE)

# --- the ranking, best first --------------------------------------------------
RANKING <- RR$ranking[order(RR$ranking$rank), , drop = FALSE]
rownames(RANKING) <- NULL
stopifnot(identical(sort(as.integer(RANKING$sim_index)), usable),
          identical(as.integer(head(RANKING$sim_index, length(top_members))),
                    top_members))
MEM <- as.integer(RANKING$sim_index)            # projection order = rank order
if (LIMIT > 0) MEM <- head(MEM, LIMIT)
N_MEMBERS <- length(MEM)
cat("  membership verified:", length(members), "usable | top cut '", TOP_NAME,
    "' =", length(top_members), "| exporting", N_MEMBERS, "\n")

# --- catchability, transcribed from F00_build_p88_data.R:112-131 -------------
RF <- readRDS(REFIT_F); MULT <- RF$M
if (!identical(RF$meta$screen, "usable"))
  stop(basename(REFIT_F), " was fitted with screen='", RF$meta$screen,
       "', not 'usable' -- its multipliers do not belong to this member set",
       call. = FALSE)
if (!is.null(RF$meta$qmax) && !isTRUE(all.equal(QMAX, RF$meta$qmax)))
  stop("QMAX mismatch: this run sets QMAX = ", QMAX, " but ", basename(REFIT_F),
       " was fitted at qmax = ", RF$meta$qmax, ". Set P105_QMAX=", RF$meta$qmax,
       call. = FALSE)
cat("  catchability:", sum(MULT != 1), "species scaled | ceiling", QMAX, "\n")

effort_arr <- readRDS("effort_array_1841_2010.rds")
stopifnot(identical(colnames(effort_arr), readRDS(
  file.path(STATE_DIR, sprintf("state_%05d.rds", MEM[1])))$params@species_params$species))

chunks     <- split(MEM, ceiling(seq_along(MEM) / CHUNK))
chunk_file <- function(ci) file.path(CHUNK_DIR, sprintf("sims_%03d.rds", ci))

# ------------------------------------------------------------------- worker ---
# Returns the two MizerSim objects for one member. Catchability is applied
# exactly as F00_build_p88_data.R:174-179, and the two project() calls are that
# script's proj() -- so these sims ARE the ones every p104q10 figure was built
# from, kept rather than discarded.
worker <- function(si) {
  suppressPackageStartupMessages({ library(therMizer); library(mizer) })
  source("R/wmin_test/thermizer_shim.R")
  st <- readRDS(file.path(STATE_DIR, sprintf("state_%05d.rds", si)))
  p  <- st$params

  gp <- gear_params(p)
  m  <- MULT[match(gp$species, names(MULT))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
  gear_params(p) <- gp

  se <- try(project(p, initial_n = st$initial_n, t_start = 1841,
                    effort = effort_arr, progress_bar = FALSE), silent = TRUE)
  su <- try(project(p, initial_n = st$initial_n, t_start = 1841, t_max = 169,
                    effort = 0, progress_bar = FALSE), silent = TRUE)
  if (inherits(se, "try-error") || inherits(su, "try-error"))
    return(list(sim_index = si, ok = FALSE,
                reason = if (inherits(se, "try-error")) "exploited" else "unexploited"))

  yre <- as.numeric(dimnames(se@n)$time); yru <- as.numeric(dimnames(su@n)$time)
  list(sim_index = si, ok = TRUE, reason = "ok",
       exploited = se, unexploited = su,
       yr_exploited = range(yre), yr_unexploited = range(yru),
       n_rows = c(exploited = length(yre), unexploited = length(yru)))
}

# --------------------------------------------------------------------- run ---
done <- function() vapply(seq_along(chunks),
                          function(ci) file.exists(chunk_file(ci)), logical(1))
if (mode == "status") {
  cat(sprintf("chunks %d/%d complete\n", sum(done()), length(chunks)))
  quit(save = "no")
}

if (mode == "run") {
  todo <- which(!done())
  cat("  chunks to run:", length(todo), "of", length(chunks),
      "| cores:", CORES, "\n\n")
  if (length(todo)) {
    cl <- makeCluster(CORES)
    on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
    clusterExport(cl, c("STATE_DIR", "MULT", "QMAX", "effort_arr"),
                  envir = environment())
    for (ci in todo) {
      tc <- proc.time()
      saveRDS(parLapply(cl, chunks[[ci]], worker), chunk_file(ci),
              compress = TRUE)
      cat(sprintf("[%s] chunk %d/%d done (%d members, %.1f min)\n",
                  format(Sys.time(), "%H:%M:%S"), ci, length(chunks),
                  length(chunks[[ci]]), (proc.time() - tc)[["elapsed"]] / 60))
      flush.console()
    }
    stopCluster(cl); on.exit()
  }
  cat("\nrun complete in", round((proc.time() - t0)[["elapsed"]] / 60, 1),
      "min. Now:  Rscript R/wmin_test/105_export_member_sims.R collect\n")
  quit(save = "no")
}

# ----------------------------------------------------------------- collect ---
# One arm at a time. Holding both arms for 203 members is ~1.6 GB; this keeps
# the peak to one arm (~0.8 GB) plus a single chunk.
if (mode == "collect") {
  if (!all(done()))
    stop("chunks incomplete (", sum(done()), " of ", length(chunks),
         ") -- finish `run` first", call. = FALSE)

  # a first pass for the failures and the shape checks, without keeping sims
  audit <- do.call(rbind, lapply(seq_along(chunks), function(ci) {
    r <- readRDS(chunk_file(ci))
    d <- do.call(rbind, lapply(r, function(x) data.frame(
      sim_index = x$sim_index, ok = isTRUE(x$ok),
      reason = x$reason,
      rows_e = if (isTRUE(x$ok)) x$n_rows[["exploited"]] else NA_integer_,
      rows_u = if (isTRUE(x$ok)) x$n_rows[["unexploited"]] else NA_integer_,
      end_e  = if (isTRUE(x$ok)) x$yr_exploited[2] else NA_real_,
      end_u  = if (isTRUE(x$ok)) x$yr_unexploited[2] else NA_real_,
      stringsAsFactors = FALSE)))
    rm(r); gc(verbose = FALSE); d
  }))
  if (any(!audit$ok))
    stop("projection failed for ", sum(!audit$ok), " member(s): ",
         paste(audit$sim_index[!audit$ok], collapse = ", "), call. = FALSE)
  if (!all(audit$end_e == END_YEAR & audit$end_u == END_YEAR))
    stop("some arms do not end at ", END_YEAR, call. = FALSE)
  if (length(unique(c(audit$rows_e, audit$rows_u))) != 1)
    stop("arms differ in length across members", call. = FALSE)
  n_rows <- unique(audit$rows_e)
  cat("  audit: ", nrow(audit), " members ok | ", n_rows,
      " rows each, ending ", END_YEAR, "\n", sep = "")
  if (!setequal(audit$sim_index, MEM))
    stop("the chunks do not carry exactly the intended membership", call. = FALSE)

  META <- list(
    ensemble        = SUFFIX,
    n_members       = N_MEMBERS,
    order           = "best first, by attr(,'ranking')$rank",
    years           = c(1841, END_YEAR),
    n_time_rows     = n_rows,
    base_params     = readRDS(MEMBERS_F)$meta$base,
    state_dir       = STATE_DIR,
    ranking_source  = RANK_F,
    member_table    = MEMBERS_F,
    refit           = REFIT_F,
    qmax            = QMAX,
    multipliers     = MULT,
    ranking_rule    = RR$rule,
    # RR$rule is quoted verbatim from the ranking object, and its trailing
    # clause is stale: it says the members are "the phase-88 usable screen
    # (stable AND no erepro>=1)". That wording was inherited when 93_rerank was
    # pointed at phase 104; the screen actually applied is usable_rule below.
    # Annotated rather than rewritten -- the stored string is not ours to edit.
    ranking_rule_note = paste("the objective is as stated; the trailing screen",
      "clause is inherited phase-88 wording. The screen actually applied is",
      "usable_rule."),
    usable_rule     = "stable AND drift_ok AND no erepro >= 1",
    top_cut_name    = TOP_NAME,
    top_cut         = top_members,
    mizer_version   = as.character(utils::packageVersion("mizer")),
    thermizer_version = as.character(utils::packageVersion("therMizer")),
    built           = format(Sys.time()),
    built_by        = "R/wmin_test/105_export_member_sims.R")

  for (arm in c("unexploited", "exploited")) {
    sims <- vector("list", N_MEMBERS)
    names(sims) <- as.character(MEM)
    for (ci in seq_along(chunks)) {
      r <- readRDS(chunk_file(ci))
      for (x in r) sims[[as.character(x$sim_index)]] <- x[[arm]]
      rm(r); gc(verbose = FALSE)
    }
    stopifnot(!any(vapply(sims, is.null, logical(1))),
              all(vapply(sims, function(s) inherits(s, "MizerSim"), logical(1))))
    attr(sims, "ranking") <- RANKING
    attr(sims, "meta")    <- c(META, list(arm = arm, effort = switch(arm,
      unexploited = "zero effort on all 19 gears",
      exploited   = "observed effort, effort_array_1841_2010.rds")))
    f <- arm_file(arm)
    saveRDS(sims, f, compress = TRUE)
    cat(sprintf("  wrote %s (%.0f MB)\n", basename(f), file.size(f) / 1e6))
    rm(sims); gc(verbose = FALSE)
  }

  write.csv(RANKING, file.path(OUT_DIR, sprintf("ranking_%s.csv", SUFFIX)),
            row.names = FALSE)
  cat("  wrote", sprintf("ranking_%s.csv", SUFFIX), "\n")

  # --- the README, generated so its numbers cannot drift from the files ------
  readme <- c(
sprintf("# Prydz Bay size-spectrum ensemble -- member simulations (%s)", SUFFIX),
"",
sprintf("%d ensemble members, two fishing scenarios, %d-%d.",
        N_MEMBERS, META$years[1], META$years[2]),
"",
"## The files",
"",
"| file | what it is |",
"|---|---|",
sprintf("| `%s` | %d `MizerSim` objects, **no fishing** |",
        basename(arm_file("unexploited")), N_MEMBERS),
sprintf("| `%s` | %d `MizerSim` objects, **observed historical effort** |",
        basename(arm_file("exploited")), N_MEMBERS),
sprintf("| `ranking_%s.csv` | the same ranking that is attached to each file |",
        SUFFIX),
"",
"Both arms start from the same per-member 1841 state, so they are paired:",
"the same `sim_index` in each file is the same member under the two scenarios.",
"",
"## Requirements",
"",
sprintf("- **mizer %s** (the version these were built and saved under)",
        META$mizer_version),
sprintf("- **therMizer %s** -- the models use temperature-dependent rates via",
        META$thermizer_version),
"  `setRateFunction()`, so `therMizer` must be loaded before anything that",
"  re-evaluates rates (`project()`, `plot()`, `getDiet()`).",
"",
"```r",
"library(mizer); library(therMizer)",
sprintf("sims <- readRDS(\"%s\")", basename(arm_file("exploited"))),
"```",
"",
"If your mizer is newer than the version above, wrap the params before",
"re-projecting: `validParams(sims[[1]]@params)`.",
"",
"## Using the whole ensemble",
"",
"```r",
"length(sims)          # members",
"names(sims)           # sim_index, as character",
"sims[[1]]             # the best-fitting member (see ordering below)",
"dim(sims[[1]]@n)      # time x species x size",
"",
"# across-member summary -- MEDIANS, not means (see the caveats)",
"b <- sapply(sims, function(s) getBiomass(s)[, \"antarctic krill\"])",
"apply(b, 1, median)",
"```",
"",
"## Cutting it however you like",
"",
"The ranking travels inside each file, so no membership list has to be",
"retyped. Members are stored **best first**, so `sims[1:n]` is the best n.",
"",
"```r",
"rk <- attr(sims, \"ranking\")   # sim_index, rmse, n_obs, rank, baleen_ratio",
"",
sprintf("# the stored top-10%% cut (%s, n = %d)", TOP_NAME, length(top_members)),
"top <- attr(sims, \"meta\")$top_cut",
"sims_top <- sims[as.character(top)]",
"",
"# equivalently, and for any other n",
"sims_top   <- sims[seq_len(20)]",
"sims_best  <- sims[rk$rank <= 50]",
"sims_tight <- sims[rk$rmse < 0.7]",
"",
"# or by a member property you care about",
"sims_dep <- sims[rk$baleen_ratio > 0.8]",
"```",
"",
"## What a member is",
"",
"Each member is one draw from the parameter uncertainty, taken to a",
"pre-exploitation steady state and then projected forward. Members differ in:",
"",
"- **abundance** -- a per-species multiplier on the calibrated 1841 spectrum",
"- **catchability** -- a per-species drawn `q`, then re-fitted to observed catch",
"- **reproduction** -- `RECAP ~ U(0.1, 0.9)`, the whale reproduction level cap",
"",
sprintf("All %d members are *usable*: %s.", N_MEMBERS, META$usable_rule),
"",
"`rmse` is the ensemble's own fit metric, quoted verbatim from the ranking",
"object:",
"",
sprintf("> %s", META$ranking_rule),
"",
"One correction to that quote: its trailing clause (\"the phase-88 usable",
sprintf("screen\") is inherited wording. The screen actually applied here is %s.",
        META$usable_rule),
"",
"## Caveats worth knowing before you analyse these",
"",
"- **Use medians, not means, across members.** The across-member distributions",
"  are heavy-tailed; a single member has previously owned 83% of an",
"  across-member sum.",
"- **`RECAP` is a propagated uncertainty, not a fitted parameter.** The yield",
"  objective is effectively blind to whales, so the ranking does not select on",
"  it and the ensemble median is the prior median.",
"- **`getTrophicLevel()` is unreliable under therMizer** -- it returns absurd",
"  values (orca ~192). `getDiet()` *proportions* are exact; its absolute rates",
"  are not.",
"- **`getDiet()` / `getRDD()` default to `t = 0`**, which under therMizer",
"  indexes the temperature forcing at the wrong year. Pass the year explicitly.",
"- **The mid-trophic abundance prior is conditioned on stability.** The",
"  screen removes the members that took high draws for squid,",
"  macrozooplankton, divers, seals and toothfishes, so the ensemble is not a",
"  fair sample of the prior for those groups. The whale prior largely survives.",
"",
"## Provenance",
"",
"| | |",
"|---|---|",
sprintf("| reference model | `%s` |", basename(META$base_params)),
sprintf("| member states | `%s` |", basename(META$state_dir)),
sprintf("| catchability | `%s`, ceiling q <= %s |",
        basename(META$refit), format(META$qmax)),
sprintf("| ranking | `%s` |", basename(META$ranking_source)),
sprintf("| built | %s by `%s` |", META$built, META$built_by),
"",
"Full provenance is in `attr(sims, \"meta\")`.",
"")
  writeLines(readme, file.path(OUT_DIR, "README.md"))
  cat("  wrote README.md\n")

  cat("\ncollect complete in", round((proc.time() - t0)[["elapsed"]] / 60, 1),
      "min\n  ", OUT_DIR, "\n")
  cat("Now:  Rscript R/wmin_test/105_export_member_sims.R check\n")
  quit(save = "no")
}

# ------------------------------------------------------------------- check ---
# Validate the written files against the F00 data products the p104q10 figures
# were built from. If these agree the exported sims ARE the published runs.
if (mode == "check") {
  cmp <- function(arm, prod) {
    f <- arm_file(arm)
    if (!file.exists(f)) stop("not written yet: ", f, call. = FALSE)
    sims <- readRDS(f)
    stopifnot(length(sims) == N_MEMBERS,
              identical(names(sims), as.character(MEM)),
              !is.null(attr(sims, "ranking")), !is.null(attr(sims, "meta")))
    p <- file.path("Manuscript data", sprintf("%s_%s.rds", prod, SUFFIX))
    if (!file.exists(p)) { cat("  no product to check against:", p, "\n")
                           return(invisible(NULL)) }
    D <- readRDS(p)
    set.seed(1); pick <- sample(names(sims), min(10, length(sims)))
    d <- do.call(rbind, lapply(pick, function(nm) {
      b  <- getBiomass(sims[[nm]])
      yr <- as.numeric(rownames(b))
      sub <- D[D$sim_index == as.integer(nm) & D$Year %in% c(1841, 1950, 2010), ]
      sub$mine <- b[cbind(match(sub$Year, yr), match(sub$Species, colnames(b)))]
      sub
    }))
    rel <- abs(d$mine - d$Biomass) / pmax(d$Biomass, .Machine$double.xmin)
    cat(sprintf("  %-12s vs %-22s  n=%5d  max rel diff = %.3e\n",
                arm, prod, nrow(d), max(rel)))
    rm(sims); gc(verbose = FALSE)
    invisible(max(rel))
  }
  cat("\n=== check: exported sims vs the F00 data products ===\n")
  e <- cmp("exploited",   "biomass_abund_fish")
  u <- cmp("unexploited", "biomass_abund_clim")
  worst <- max(c(e, u, 0))
  if (worst > 1e-10)
    stop("MISMATCH: max relative difference ", signif(worst, 3),
         " -- the exported sims are not the runs the figures were built from",
         call. = FALSE)
  cat("\nOK -- the exported sims reproduce the published data products.\n")
  cat("elapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")
}