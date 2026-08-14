# =============================================================================
# MA01 -- contemporary (2001-2010) krill consumption and abundance for the WHOLE
# accepted ensemble 44 (n = 1,668), so the headline ratios can be recomputed at
# four RMSE-ranking thresholds. Supplies analysis H (and re-derives G).
#
# WHY THE WHOLE ENSEMBLE. Analysis G is quoted on the best-fitting decile
# (n = 167). H asks whether that choice sets the answer. The quartile (417), half
# (834) and full (1,668) subsets are prefixes of the SAME post-refit RMSE
# ranking, so one pass over all 1,668 members answers all four thresholds and no
# member is projected twice.
#
#   n=167  ceiling(1668 * 0.10)   the manuscript's cut A
#   n=417  ceiling(1668 * 0.25)
#   n=834  ceiling(1668 * 0.50)
#   n=1668 the full accepted ensemble
#
# THE PROTOCOL IS MA00's, WHICH IS F00's. Same states, same catchability
# multipliers, same projection call, same ther_diet(). The n = 167 prefix of this
# run must therefore reproduce MA00 and the cached
# krill_consumption_rebuilt167.rds exactly; MA02 asserts it and refuses to report
# on a mismatch. That is the check that the extension to 1,668 has not quietly
# changed the code path.
#
# PER-YEAR, NOT JUST THE PERIOD MEAN. Figure 4 is a per-year ratio and its
# published contemporary value is quoted at 2010, so the ten years are kept
# separately rather than averaged in the worker. MA02 needs both the year-2010
# ratio (to reproduce the figure) and the 2001-2010 mean (the period statistic).
#
# USAGE
#   Rscript "Manuscript scripts/MA01_extract_contemporary_1668.R" run
#   Rscript "Manuscript scripts/MA01_extract_contemporary_1668.R" collect
# ENV: MA_CORES (default cores-2), MA_CHUNK (default 50), MA_LIMIT (0 = all)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})
source("R/wmin_test/thermizer_shim.R")

OUT_LARGE <- "Output_large_files/wmin_test"
STATE_DIR <- file.path(OUT_LARGE, "44_states")
WORK_DIR  <- file.path(OUT_LARGE, "MA01_chunks")
OUT_RDS   <- file.path(OUT_LARGE, "MA01_contemp1668.rds")
QMAX      <- 1
KRILL     <- "antarctic krill"
YEARS     <- 2001:2010

dir.create(WORK_DIR, recursive = TRUE, showWarnings = FALSE)
CORES <- as.integer(Sys.getenv("MA_CORES", as.character(max(1, detectCores() - 2))))
CHUNK <- as.integer(Sys.getenv("MA_CHUNK", "50"))
LIMIT <- as.integer(Sys.getenv("MA_LIMIT", "0"))
mode  <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"

guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite an existing file: ", f, call. = FALSE); f
}

# --- the post-refit ranking over the full accepted ensemble -------------------
# Same statistic that defines cut A: sqrt(sum(sse)/sum(n)) per member over the
# post-catchability-refit per-species SSE. Ranked ascending, cut A is its first
# 167 -- asserted here so the ordering used for H is provably the same one the
# manuscript's decile came from.
RF <- readRDS(file.path(OUT_LARGE, "45_refit_results.rds"))
RANK <- RF$per_species %>% group_by(sim_index) %>%
  summarise(rmse = sqrt(sum(sse) / sum(n)), .groups = "drop") %>%
  arrange(rmse) %>% mutate(rank = row_number())
CUTS <- readRDS(file.path(OUT_LARGE, "46_selection_cuts.rds"))
cutA <- CUTS$cuts[["A unweighted RMSE"]]
if (!identical(as.integer(head(RANK$sim_index, length(cutA))), as.integer(cutA)))
  stop("the first ", length(cutA), " of the post-refit ranking are not cut A -- ",
       "refusing to proceed.")
message("post-refit ranking verified: n = ", nrow(RANK),
        ", its first ", length(cutA), " are cut A")

MULT <- readRDS(file.path(OUT_LARGE, "45_catchability_multipliers.rds"))$M
members <- RANK$sim_index
missing <- members[!file.exists(file.path(STATE_DIR,
             sprintf("state_treated_%05d.rds", members)))]
if (length(missing))
  stop("no cached state for ", length(missing), " ranked members: ",
       paste(head(missing, 10), collapse = ", "))

if (LIMIT > 0) members <- head(members, LIMIT)
n_tot  <- length(members)
chunks <- split(seq_len(n_tot), ceiling(seq_len(n_tot) / CHUNK))
effort_arr <- readRDS("effort_array_1841_2010.rds")

# ------------------------------------------------------------------- worker ---
worker <- function(k) {
  suppressPackageStartupMessages({ library(therMizer); library(mizer) })
  source("R/wmin_test/thermizer_shim.R")
  si <- MEM[k]
  st <- readRDS(file.path(STATE_DIR, sprintf("state_treated_%05d.rds", si)))
  p  <- st$params

  gp <- gear_params(p)
  m <- MULT[match(gp$species, names(MULT))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
  gear_params(p) <- gp

  spn <- p@species_params$species; nsp <- length(spn)
  w <- p@w; dw <- p@dw; wdw <- w * dw

  proj <- function(eff) {
    if (identical(eff, 0))
      project(p, initial_n = st$initial_n, t_start = 1841, t_max = 169, effort = 0)
    else
      project(p, initial_n = st$initial_n, t_start = 1841, effort = eff)
  }
  sf <- try(proj(effort_arr), silent = TRUE)
  sc <- try(proj(0), silent = TRUE)
  if (inherits(sf, "try-error") || inherits(sc, "try-error"))
    return(list(sim_index = si, ok = FALSE, err = "projection failed"))

  arm <- function(s) {
    tn <- as.numeric(dimnames(s@n)$time)
    yy <- intersect(YEARS, tn)
    cons <- matrix(0, length(yy), nsp, dimnames = list(as.character(yy), spn))
    bio  <- abu <- cons
    for (j in seq_along(yy)) {
      y <- yy[j]; ti <- which(tn == y)
      n <- s@n[ti, , ]; npp <- s@n_pp[ti, ]
      te <- ther_temp_effect(p, y)
      fl <- ther_feeding_level(p, n = n, n_pp = npp,
                               n_other = p@initial_n_other, year = y, temp_eff = te)
      d  <- ther_diet(p, n = n, n_pp = npp, n_other = p@initial_n_other,
                      year = y, temp_eff = te, feeding_level = fl)
      cons[j, ] <- rowSums(d[, , KRILL] * n * rep(dw, each = nsp))
      bio[j, ]  <- rowSums(sweep(n, 2, wdw, "*"))
      abu[j, ]  <- rowSums(sweep(n, 2, dw, "*"))
    }
    list(krill_cons = cons, biomass = bio, abundance = abu, years = yy)
  }

  if (!all(YEARS %in% as.numeric(dimnames(sf@n)$time)) ||
      !all(YEARS %in% as.numeric(dimnames(sc@n)$time)))
    return(list(sim_index = si, ok = FALSE, err = "2001-2010 not present in both arms"))

  list(sim_index = si, ok = TRUE,
       exploited = arm(sf), unexploited = arm(sc), species = spn)
}

# ---------------------------------------------------------------------- run ---
if (mode == "run") {
  cat("=== MA01: contemporary extraction, full accepted ensemble ===\n")
  cat("started", format(Sys.time()), "| members", n_tot, "| cores", CORES,
      "| years", min(YEARS), "-", max(YEARS), "\n")
  done <- list.files(WORK_DIR, pattern = "^ma01_\\d+\\.rds$")
  cat("chunks:", length(chunks), "| already done:", length(done), "\n\n")
  t0 <- proc.time()
  for (ci in names(chunks)) {
    rf <- file.path(WORK_DIR, sprintf("ma01_%03d.rds", as.integer(ci)))
    if (file.exists(rf)) { cat("chunk", ci, "done, skipping\n"); next }
    MEM <- members[chunks[[ci]]]
    cl <- makeCluster(CORES)
    clusterExport(cl, c("MEM", "STATE_DIR", "effort_arr", "YEARS", "KRILL",
                        "MULT", "QMAX", "worker"), envir = environment())
    r <- parLapplyLB(cl, seq_along(MEM), function(j)
      tryCatch(worker(j), error = function(e)
        list(sim_index = MEM[j], ok = FALSE, err = conditionMessage(e))))
    stopCluster(cl)
    saveRDS(r, rf)
    el <- (proc.time() - t0)["elapsed"] / 60
    nd <- length(list.files(WORK_DIR, pattern = "^ma01_\\d+\\.rds$"))
    cat(sprintf("chunk %s (%d/%d) | ok %d/%d | elapsed %.1f min | ETA %.1f min\n",
                ci, nd, length(chunks),
                sum(vapply(r, function(x) isTRUE(x$ok), logical(1))), length(r),
                el, (el / max(1, nd - length(done))) * (length(chunks) - nd)))
    rm(r); invisible(gc())
  }
  cat("\nrun complete.", round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
  cat('Now: Rscript "Manuscript scripts/MA01_extract_contemporary_1668.R" collect\n')
  quit(save = "no")
}

# ------------------------------------------------------------------ collect ---
fs <- sort(list.files(WORK_DIR, pattern = "^ma01_\\d+\\.rds$", full.names = TRUE))
stopifnot(length(fs) > 0)
Z0 <- unlist(lapply(fs, readRDS), recursive = FALSE)
ok <- vapply(Z0, function(x) isTRUE(x$ok), logical(1))
cat("=== collected", sum(ok), "of", length(Z0), "members ===\n")
failed <- data.frame(sim_index = numeric(0), reason = character(0))
if (any(!ok)) {
  failed <- do.call(rbind, lapply(Z0[!ok], function(z) data.frame(
    sim_index = z$sim_index,
    reason = if (is.null(z$err)) "(no message)" else z$err)))
  cat("FAILED members:\n"); print(failed, row.names = FALSE)
}
Z <- Z0[ok]
stopifnot(length(Z) > 0)

# Flatten to arrays [member x year x species], with the ranking carried along so
# every threshold in H is a prefix rather than a re-derivation.
spn <- Z[[1]]$species
mk <- function(a, f) {
  out <- array(NA_real_, dim = c(length(Z), length(YEARS), length(spn)),
               dimnames = list(sim = vapply(Z, function(z) as.character(z$sim_index),
                                            character(1)),
                               year = as.character(YEARS), species = spn))
  for (k in seq_along(Z)) out[k, , ] <- Z[[k]][[a]][[f]]
  out
}
res <- list(
  krill_cons_exploited   = mk("exploited",   "krill_cons"),
  krill_cons_unexploited = mk("unexploited", "krill_cons"),
  biomass_exploited      = mk("exploited",   "biomass"),
  biomass_unexploited    = mk("unexploited", "biomass"),
  abundance_exploited    = mk("exploited",   "abundance"),
  abundance_unexploited  = mk("unexploited", "abundance"),
  ranking = RANK, members = vapply(Z, function(z) z$sim_index, numeric(1)),
  years = YEARS, species = spn, failed = failed,
  thresholds = c("n=167" = 167, "n=417" = 417, "n=834" = 834,
                 "n=1668" = nrow(RANK)),
  units = c(krill_cons = "g/yr (domain total)", biomass = "g (domain total)",
            abundance = "individuals (domain total)"),
  built = Sys.time())
saveRDS(res, guard(OUT_RDS))
cat("\nWrote", OUT_RDS, "\n")
cat("members retained:", length(Z), "| thresholds:",
    paste(names(res$thresholds), res$thresholds, sep = "="), "\n")
cat("collect complete.\n")
