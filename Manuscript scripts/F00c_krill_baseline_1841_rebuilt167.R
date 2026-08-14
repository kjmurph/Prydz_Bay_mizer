# =============================================================================
# F00c -- UNEXPLOITED krill consumption over 1841-2010, rebuilt ensemble 44,
# cut A, 167 members. Supplies Figure 4's +-1 SD natural-variability baseline.
#
# WHY THIS EXISTS. F00_build_rebuilt167_data.R sets DIET_YEARS <- 1900:2010,
# which is Figure 4's x axis, so krill_consumption_rebuilt167.rds starts at 1900.
# The SD band is a different quantity from the plotted ratio: it is the temporal
# SD of the unexploited baseline, and the manuscript's baseline window is
# 1841-2010 -- the same one the SNR figure and the abundance / body-size series
# use. Restricting it to 1900-2010 made Figure 4 the only place with a different
# baseline. This script extracts the missing years.
#
# ONLY THE UNEXPLOITED ARM IS RUN. The band is built from that arm alone; the
# plotted ratio still comes from krill_consumption_rebuilt167.rds and is
# untouched. That halves the projections and removes a whole class of error:
# with effort = 0 the catchability multipliers cannot influence the result (they
# are applied anyway, so the protocol is identical to F00's, not merely
# equivalent).
#
# NOTHING EXISTING IS OVERWRITTEN. This writes a new file. F00's outputs, and
# therefore every other figure, are left exactly as they are.
#
# THE CORRECTNESS CHECK IS THE OVERLAP. 1900-2010 is extracted here as well as
# in F00, by the same protocol from the same stored states, so the two must agree
# to floating-point noise. The script computes the max relative difference over
# all 167 x 111 x 19 overlapping values and STOPS if it exceeds 1e-10. That is
# what makes the new 1841-1899 years trustworthy: they come from the same code
# path that reproduces F00 exactly.
#
# Usage:  Rscript "Manuscript scripts/F00c_krill_baseline_1841_rebuilt167.R"
#         Rscript "Manuscript scripts/F00c_krill_baseline_1841_rebuilt167.R" collect
# ENV: F0_CORES (default cores-2), F0_CHUNK (default 25), F0_LIMIT (0 = all)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})
source("R/wmin_test/thermizer_shim.R")

N_TOP      <- 167
OUT_LARGE  <- "Output_large_files/wmin_test"
STATE_DIR  <- file.path(OUT_LARGE, "44_states")
OUT_DATA   <- "Manuscript data"
WORK_DIR   <- file.path(OUT_LARGE, "F00c_chunks_rebuilt167")
SUFFIX     <- "rebuilt167"
DIET_YEARS <- 1841:2010            # the manuscript baseline window
KRILL      <- "antarctic krill"
QMAX       <- 1
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

# --- membership, verified exactly as F00 does ---------------------------------
# TRAP: 44_selection_rules.rds$sel_unweighted is the PRE-refit ranking and shares
# only 71 of these 167. Re-derive from the post-refit SSE and demand a match.
CUTS <- readRDS(file.path(OUT_LARGE, "46_selection_cuts.rds"))
members <- CUTS$cuts[["A unweighted RMSE"]]
stopifnot(length(members) == N_TOP)
RF <- readRDS(file.path(OUT_LARGE, "45_refit_results.rds"))
chk <- RF$per_species %>% group_by(sim_index) %>%
  summarise(m = sqrt(sum(sse) / sum(n)), .groups = "drop") %>%
  arrange(m) %>% head(N_TOP) %>% pull(sim_index)
if (!identical(as.integer(chk), as.integer(members)))
  stop("cut A membership does not match the top-", N_TOP,
       " recomputed from 45_refit_results.rds -- refusing to proceed.")
message("cut A membership verified against the post-refit ranking (", N_TOP,
        " members)")

MULT <- readRDS(file.path(OUT_LARGE, "45_catchability_multipliers.rds"))$M

if (LIMIT > 0) members <- head(members, LIMIT)
n_tot <- length(members)
chunks <- split(seq_len(n_tot), ceiling(seq_len(n_tot) / CHUNK))

# ------------------------------------------------------------------- worker ---
# Transcribed from F00_build_rebuilt167_data.R:140-233, unexploited arm only.
worker <- function(k) {
  suppressPackageStartupMessages({ library(therMizer); library(mizer) })
  source("R/wmin_test/thermizer_shim.R")
  si <- MEM[k]
  st <- readRDS(file.path(STATE_DIR, sprintf("state_treated_%05d.rds", si)))
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
cat("\nWrote:", OUT_RDS, "\n")

# --- what this changes for Figure 4's band -------------------------------------
FISHES <- c("mesopelagic fishes", "bathypelagic fishes",
            "shelf and coastal fishes", "toothfishes")
WHALES <- c("baleen whales", "minke whales")
sig <- function(species, yrs) {
  u <- KB %>% filter(Species %in% species, Year %in% yrs) %>%
    group_by(sim_index, Year) %>%
    summarise(cons = sum(krill_consumed), .groups = "drop") %>%
    group_by(Year) %>% summarise(mu = mean(cons), .groups = "drop")
  sd(u$mu) / mean(u$mu)
}
cat("\n=== sigma by baseline window (unexploited, mean trajectory) ===\n")
for (g in list(list("All predators", sort(unique(KB$Species))),
               list("Large baleen + minke whales", WHALES),
               list("All fishes", FISHES))) {
  a <- sig(g[[2]], 1841:2010); b <- sig(g[[2]], 1900:2010)
  cat(sprintf("  %-28s 1841-2010 %.4f [%.3f, %.3f] | 1900-2010 %.4f | %.2fx\n",
              g[[1]], a, max(1 - a, 0), 1 + a, b, a / b))
}