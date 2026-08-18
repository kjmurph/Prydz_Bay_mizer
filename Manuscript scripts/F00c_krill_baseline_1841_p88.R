# =============================================================================
# F00c (phase 88) -- UNEXPLOITED krill consumption over 1841-2010, all 427
# usable members. Supplies Figure 4's +-1 SD natural-variability baseline.
#
# A copy of F00c_krill_baseline_1841_rebuilt167.R. The WORKER IS TRANSCRIBED
# UNCHANGED; only the source, the membership and the suffix differ, exactly as
# in F00_build_p88_data.R.
#
# WHY THIS EXISTS. F00_build_p88_data.R sets DIET_YEARS <- 1900:2010, which is
# Figure 4's x axis, so krill_consumption_p88full427.rds starts at 1900. The SD
# band is a different quantity from the plotted ratio: it is the temporal SD of
# the unexploited baseline over the manuscript's 1841-2010 window, the same one
# the SNR figure and the abundance / body-size series use. This extracts the
# missing years.
#
# ONLY THE UNEXPLOITED ARM IS RUN. With effort = 0 the catchability multipliers
# cannot influence the result -- they are applied anyway, so the protocol is
# identical to F00's rather than merely equivalent.
#
# THE CORRECTNESS CHECK IS THE OVERLAP. 1900-2010 is extracted here as well as
# in F00, by the same code path from the same stored states, so the two must
# agree to floating-point noise. The script computes the max relative difference
# over all 427 x 111 x 19 overlapping values and STOPS above 1e-10. That is what
# makes the new 1841-1899 years trustworthy.
#
# Usage:  Rscript "Manuscript scripts/F00c_krill_baseline_1841_p88.R"
#         Rscript "Manuscript scripts/F00c_krill_baseline_1841_p88.R" collect
# ENV: F0_CORES (default cores-2), F0_CHUNK (default 25), F0_LIMIT (0 = all)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})
source("R/wmin_test/thermizer_shim.R")

OUT_LARGE  <- "Output_large_files/wmin_test"
# Selectable, matching F00_build_p88_data.R. Defaults are the phase-88 values so
# existing calls reproduce exactly.
STATE_DIR  <- Sys.getenv("F0_STATE_DIR", file.path(OUT_LARGE, "88_full_states"))
RANK_F     <- Sys.getenv("F0_RANK", file.path(OUT_LARGE, "93_rerank_p88.rds"))
MEMBERS_F  <- Sys.getenv("F0_MEMBERS", file.path(OUT_LARGE, "88_full.rds"))
REFIT_F    <- Sys.getenv("F0_REFIT", file.path(OUT_LARGE, "89_refit_results.rds"))
OUT_DATA   <- "Manuscript data"
SUFFIX     <- Sys.getenv("F0_SUFFIX", "p88full427")
WORK_DIR   <- file.path(OUT_LARGE, paste0("F00c_chunks_", SUFFIX))
DIET_YEARS <- 1841:2010            # the manuscript baseline window
KRILL      <- "antarctic krill"
QMAX       <- as.numeric(Sys.getenv("F0_QMAX", "1"))  # must match the refit
OVERLAP    <- 1900:2010            # what F00 already has, used as the check
TOL        <- 1e-10
dir.create(WORK_DIR, recursive = TRUE, showWarnings = FALSE)

CORES <- as.integer(Sys.getenv("F0_CORES", as.character(max(1, detectCores() - 2))))
CHUNK <- as.integer(Sys.getenv("F0_CHUNK", "25"))
LIMIT <- as.integer(Sys.getenv("F0_LIMIT", "0"))
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"

OUT_RDS <- file.path(OUT_DATA, sprintf("krill_baseline_1841_unexploited_%s.rds",
                                       SUFFIX))
guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite: ", f, call. = FALSE); f
}

# --- membership, verified exactly as F00_build_p88_data.R does ----------------
RR <- readRDS(RANK_F)
members <- as.integer(RR$cuts[["FULL usable"]])
P88 <- readRDS(MEMBERS_F)$members
usable <- sort(as.integer(P88$sim_index[
  P88$stable & P88$n_erepro_ge1 == 0 &
  (if ("drift_ok" %in% names(P88)) P88$drift_ok else TRUE)]))
if (!identical(sort(members), usable))
  stop("the phase-93 FULL set is not the phase-88 usable set -- refusing to ",
       "proceed. Re-run R/wmin_test/93_rerank_p88.R", call. = FALSE)
message("membership verified: ", length(members), " usable members")

RF <- readRDS(REFIT_F)
if (!identical(RF$meta$screen, "usable"))
  stop("89_refit_results.rds was fitted with screen='", RF$meta$screen,
       "', not 'usable'", call. = FALSE)
MULT <- RF$M

if (LIMIT > 0) members <- head(members, LIMIT)
n_tot <- length(members)
chunks <- split(seq_len(n_tot), ceiling(seq_len(n_tot) / CHUNK))

# ------------------------------------------------------------------- worker ---
# Transcribed from F00_build_p88_data.R, unexploited arm only.
worker <- function(k) {
  suppressPackageStartupMessages({ library(therMizer); library(mizer) })
  source("R/wmin_test/thermizer_shim.R")
  si <- MEM[k]
  st <- readRDS(file.path(STATE_DIR, sprintf("state_%05d.rds", si)))
  p <- st$params

  # Applied for protocol identity with F00, not because it can matter: effort is
  # 0 throughout this arm, so fishing mortality is 0 for any catchability.
  gp <- gear_params(p)
  m <- MULT[match(gp$species, names(MULT))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
  gear_params(p) <- gp

  sp <- p@species_params$species
  dw <- p@dw
  i_kr <- which(sp == KRILL)

  s <- try(project(p, initial_n = st$initial_n, t_start = 1841, t_max = 169,
                   effort = 0), silent = TRUE)
  if (inherits(s, "try-error")) return(list(sim_index = si, ok = FALSE))

  # the unexploited run can carry an extra year past 2010 -- clip, as F00 does
  yr_all <- as.numeric(dimnames(s@n)$time)
  yrs <- intersect(DIET_YEARS, yr_all[yr_all <= 2010])

  kc <- do.call(rbind, lapply(yrs, function(y) {
    ti <- which(yr_all == y)
    n <- s@n[ti, , ]; npp <- s@n_pp[ti, ]
    d <- ther_diet(s@params, n = n, n_pp = npp,
                   n_other = s@params@initial_n_other, year = y)
    cons <- rowSums(d[, , i_kr, drop = TRUE] * n * rep(dw, each = length(sp)))
    data.frame(sim_index = si, arm = "unexploited", Year = y, Species = sp,
               krill_consumed = cons, stringsAsFactors = FALSE)
  }))
  list(sim_index = si, ok = TRUE, krill = kc)
}

# ---------------------------------------------------------------------- run ---
if (mode == "run") {
  cat("=== F00c: unexploited krill consumption,", min(DIET_YEARS), "-",
      max(DIET_YEARS), "===\n")
  cat("started", format(Sys.time()), "| members", n_tot, "| cores", CORES, "\n")
  cat("chunks:", length(chunks), "| done:",
      length(list.files(WORK_DIR, pattern = "^f00c_\\d+\\.rds$")), "\n\n")
  t0 <- proc.time()
  for (ci in names(chunks)) {
    rf <- file.path(WORK_DIR, sprintf("f00c_%03d.rds", as.integer(ci)))
    if (file.exists(rf)) { cat("chunk", ci, "done, skipping\n"); next }
    MEM <- members[chunks[[ci]]]
    cl <- makeCluster(CORES)
    clusterExport(cl, c("MEM", "STATE_DIR", "DIET_YEARS", "KRILL", "MULT",
                        "QMAX", "worker"), envir = environment())
    r <- parLapplyLB(cl, seq_along(MEM), function(j)
      tryCatch(worker(j), error = function(e)
        list(sim_index = MEM[j], ok = FALSE, err = conditionMessage(e))))
    stopCluster(cl)
    saveRDS(r, rf)
    el <- (proc.time() - t0)[["elapsed"]]
    cat(sprintf("chunk %s: %d/%d ok | %.1f min elapsed\n", ci,
                sum(vapply(r, `[[`, logical(1), "ok")), length(r), el / 60))
  }
  cat("\nrun complete. now: Rscript ... collect\n")
  quit(save = "no")
}

# ------------------------------------------------------------------ collect ---
files <- sort(list.files(WORK_DIR, pattern = "^f00c_\\d+\\.rds$",
                         full.names = TRUE))
if (!length(files)) stop("no chunk files in ", WORK_DIR, " -- run first")
Z <- unlist(lapply(files, readRDS), recursive = FALSE)
bad <- Z[!vapply(Z, `[[`, logical(1), "ok")]
if (length(bad)) {
  message("FAILED members: ",
          paste(vapply(bad, `[[`, numeric(1), "sim_index"), collapse = ", "))
  stop("refusing to write a partial baseline")
}
Z <- Z[vapply(Z, `[[`, logical(1), "ok")]
if (length(Z) != n_tot)
  stop("collected ", length(Z), " members, expected ", n_tot)

KB <- bind_rows(lapply(Z, `[[`, "krill"))
cat("rows:", nrow(KB), "| members:", n_distinct(KB$sim_index),
    "| years", min(KB$Year), "-", max(KB$Year),
    "| species:", n_distinct(KB$Species), "\n")
stopifnot(nrow(KB) == n_tot * length(DIET_YEARS) * n_distinct(KB$Species))

# --- THE CHECK: 1900-2010 must reproduce F00 -----------------------------------
# Same protocol, same stored states, so this is a reproduction, not a comparison
# of two estimates. Anything above floating-point noise means the transcription
# diverged and the new 1841-1899 years cannot be trusted either.
KR <- readRDS(file.path(OUT_DATA, sprintf("krill_consumption_%s.rds", SUFFIX)))
cmp <- KR %>% filter(arm == "unexploited", Year %in% OVERLAP) %>%
  select(sim_index, Year, Species, old = krill_consumed) %>%
  inner_join(KB %>% filter(Year %in% OVERLAP) %>%
               select(sim_index, Year, Species, new = krill_consumed),
             by = c("sim_index", "Year", "Species"))
n_exp <- n_tot * length(OVERLAP) * n_distinct(KB$Species)
if (nrow(cmp) != n_exp)
  stop("overlap join gave ", nrow(cmp), " rows, expected ", n_exp,
       " -- membership or indexing differs from F00")
den <- pmax(abs(cmp$old), .Machine$double.eps)
rel <- abs(cmp$new - cmp$old) / den
cat(sprintf("\noverlap check %d-%d: %d values | max rel diff %.3g\n",
            min(OVERLAP), max(OVERLAP), nrow(cmp), max(rel)))
if (max(rel) > TOL)
  stop("overlap does not reproduce F00 (max rel diff ", signif(max(rel), 3),
       " > ", TOL, ") -- refusing to write")
cat("reproduces F00 exactly over the overlap.\n")

saveRDS(KB, guard(OUT_RDS))
cat("WROTE", OUT_RDS, "\n")