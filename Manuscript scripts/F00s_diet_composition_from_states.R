# =============================================================================
# F00s -- full prey-resolved diet composition from a state directory, for the
# three whale_consumption_outputs/ diet figures.
#
# WHAT IT PRODUCES, and why both forms are needed:
#   cons       [member x year x predator x prey]  consumption, g/yr, summed over
#              predator size. Serves the baleen stacked area and the 19-panel
#              diet time series.
#   size_prop  [member x predator x size x prey]  diet PROPORTION at each
#              predator size bin, averaged over 2001-2010. Serves the
#              diet-by-body-size figure. This is mizer's getDiet(proportion =
#              TRUE) contract: normalised across prey within each predator-size.
#
# EXPLOITED ARM ONLY. All three source figures are built on the fishing run
# (`*_fishing_*` / the MC fishing ensemble), so only that arm is projected.
#
# THE t = 0 BUG IS NOT REPRODUCED. The original extraction
# (extract_whale_consumption.R:144) called getDiet() with no `t`, so
# getFeedingLevel defaulted to t = 0 and indexed ocean_temp at -1841; its stored
# outputs disagree with a faithful recomputation by 13-80x. This uses ther_diet()
# from R/wmin_test/thermizer_shim.R, which takes `year` explicitly and was
# verified bit-identical to mizer::getDiet(proportion = FALSE). Absolute
# consumption from the legacy arrays must not be compared against this; diet
# PROPORTIONS largely survive the bug, so the figures should look broadly similar.
#
# STABILITY FILTERING IS ON BY DEFAULT, for the same reason as F00r: these
# figures take an across-member median (robust) but the underlying build carries
# members that diverge by ~800x, and there is no reason to carry them.
#
# USAGE  Rscript "Manuscript scripts/F00s_diet_composition_from_states.R"
# ENV: F0S_STATE_DIR, F0S_MANIFEST, F0S_SUFFIX, F0S_ARM, F0S_CORES (default 12),
#      F0S_SUMMARY, F0S_STABLE_ONLY (default 1), F0S_YEAR_MIN (default 1901)
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer); library(mizer); library(parallel); library(dplyr)
})
source("R/wmin_test/thermizer_shim.R")

OUT_LARGE <- "Output_large_files/wmin_test"
OUT_DATA  <- "Manuscript data"
STATE_DIR <- Sys.getenv("F0S_STATE_DIR",
               file.path(OUT_LARGE, "53_bk05_mk05_n167_states"))
MANIFEST  <- Sys.getenv("F0S_MANIFEST",
               file.path(OUT_LARGE, "53_bk05_mk05_n167_rebuild.rds"))
ARM       <- Sys.getenv("F0S_ARM", "kernel")
CORES     <- as.integer(Sys.getenv("F0S_CORES", "12"))
SUMMARY   <- Sys.getenv("F0S_SUMMARY",
               sub("_rebuild\\.rds$", "_summary.csv", MANIFEST))
STABLE_ONLY <- Sys.getenv("F0S_STABLE_ONLY", "1") == "1"
YEAR_MIN  <- as.integer(Sys.getenv("F0S_YEAR_MIN", "1901"))
YEAR_MAX  <- 2010
REF_YEARS <- 2001:2010          # the contemporary window for the size figure
QMAX <- as.numeric(Sys.getenv("F0_QMAX", "1"))  # must match the refit

MAN <- readRDS(MANIFEST)
members <- MAN$members
cat("=== F00s: prey-resolved diet composition ===\n")
if (STABLE_ONLY) {
  S <- read.csv(SUMMARY)
  drop <- setdiff(members, S$sim_index[S$stable])
  members <- intersect(members, S$sim_index[S$stable])
  cat("stability filter ON:", length(drop), "dropped (",
      paste(drop, collapse = ", "), ")\n")
}
SUFFIX <- Sys.getenv("F0S_SUFFIX", sprintf("kernel%d", length(members)))
OUT_RDS <- file.path(OUT_DATA, sprintf("diet_composition_%s.rds", SUFFIX))
if (file.exists(OUT_RDS))
  stop("refusing to overwrite an existing file: ", OUT_RDS, call. = FALSE)

# must match the ensemble being extracted; the phase-45 file was fitted on
# ensemble-44 states.
MULT_F <- Sys.getenv("F0S_MULT",
                     file.path(OUT_LARGE, "45_catchability_multipliers.rds"))
if (!file.exists(MULT_F)) stop("no multiplier file: ", MULT_F, call. = FALSE)
MULT <- readRDS(MULT_F)$M
cat("multipliers:", basename(MULT_F), "
")
effort_arr <- readRDS("effort_array_1841_2010.rds")
YEARS <- YEAR_MIN:YEAR_MAX
cat("states:", STATE_DIR, "| members", length(members),
    "| years", YEAR_MIN, "-", YEAR_MAX, "| suffix", SUFFIX, "\n")
# Phase 44/53 states carry an arm token (state_<arm>_00148.rds); phase 88 and
# later do not (state_00148.rds). F0S_ARM="" picks the second form, exactly as
# F00r_build_1g_from_states.R already did. The default is unchanged.
state_file <- function(si) file.path(STATE_DIR,
  if (nzchar(ARM)) sprintf("state_%s_%05d.rds", ARM, si)
  else sprintf("state_%05d.rds", si))
ok <- file.exists(vapply(members, state_file, character(1)))
if (!all(ok)) stop("missing states for: ", paste(members[!ok], collapse = ", "))

worker <- function(k) {
  suppressPackageStartupMessages({ library(therMizer); library(mizer) })
  source("R/wmin_test/thermizer_shim.R")
  si <- MEM[k]
  st <- readRDS(state_file(si))
  p <- st$params
  gp <- gear_params(p)
  m <- MULT[match(gp$species, names(MULT))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
  gear_params(p) <- gp

  s <- try(project(p, initial_n = st$initial_n, t_start = 1841,
                   effort = effort_arr, progress_bar = FALSE), silent = TRUE)
  if (inherits(s, "try-error")) return(list(sim_index = si, ok = FALSE))

  spn <- p@species_params$species; nsp <- length(spn); dw <- p@dw
  tn <- as.numeric(dimnames(s@n)$time)
  prey_names <- NULL
  CONS <- NULL; PSUM <- NULL; npref <- 0

  for (y in YEARS) {
    ti <- which(tn == y); if (!length(ti)) next
    n <- s@n[ti, , ]; npp <- s@n_pp[ti, ]
    d <- ther_diet(p, n = n, n_pp = npp, n_other = p@initial_n_other, year = y)
    if (is.null(prey_names)) {
      prey_names <- dimnames(d)$prey
      CONS <- array(0, dim = c(length(YEARS), nsp, length(prey_names)),
                    dimnames = list(year = as.character(YEARS), predator = spn,
                                    prey = prey_names))
      PSUM <- array(0, dim = c(nsp, length(p@w), length(prey_names)),
                    dimnames = list(predator = spn, w = dimnames(p@initial_n)$w,
                                    prey = prey_names))
    }
    # consumption, g/yr: diet (g/yr per individual) x numbers in the bin
    E <- n * rep(dw, each = nsp)
    CONS[as.character(y), , ] <- apply(d, 3, function(mm) rowSums(mm * E))
    # size-resolved diet PROPORTION -- mizer's getDiet(proportion = TRUE)
    if (y %in% REF_YEARS) {
      tot <- apply(d, c(1, 2), sum)
      pr <- sweep(d, c(1, 2), pmax(tot, 1e-300), "/")
      pr[!is.finite(pr)] <- 0
      PSUM <- PSUM + pr; npref <- npref + 1
    }
  }
  list(sim_index = si, ok = TRUE, cons = CONS, size_prop = PSUM / max(npref, 1),
       species = spn, prey = prey_names, w = p@w)
}

t0 <- proc.time()
MEM <- members
cl <- makeCluster(min(CORES, length(members)))
clusterExport(cl, c("MEM", "STATE_DIR", "ARM", "effort_arr", "MULT", "QMAX",
                    # state_file is called BY NAME inside worker(); the workers
                    # start bare, so omitting it fails every member.
                    "state_file",
                    "YEARS", "REF_YEARS", "worker"), envir = environment())
Z <- parLapplyLB(cl, seq_along(MEM), function(j)
  tryCatch(worker(j), error = function(e)
    list(sim_index = MEM[j], ok = FALSE, err = conditionMessage(e))))
stopCluster(cl)
okz <- vapply(Z, function(x) isTRUE(x$ok), logical(1))
cat("completed", sum(okz), "of", length(Z), "in",
    round((proc.time() - t0)["elapsed"] / 60, 1), "min\n")
if (any(!okz)) stop("failed: ", paste(vapply(Z[!okz], function(z) z$sim_index,
                                             numeric(1)), collapse = ", "))

spn <- Z[[1]]$species; prey <- Z[[1]]$prey; wv <- Z[[1]]$w
CONS <- array(NA_real_, dim = c(length(Z), length(YEARS), length(spn), length(prey)),
              dimnames = list(sim = as.character(members), year = as.character(YEARS),
                              predator = spn, prey = prey))
SZ <- array(NA_real_, dim = c(length(Z), length(spn), length(wv), length(prey)),
            dimnames = list(sim = as.character(members), predator = spn,
                            w = NULL, prey = prey))
for (k in seq_along(Z)) { CONS[k, , , ] <- Z[[k]]$cons; SZ[k, , , ] <- Z[[k]]$size_prop }

saveRDS(list(cons = CONS, size_prop = SZ, members = members, years = YEARS,
             ref_years = REF_YEARS, species = spn, prey = prey, w = wv,
             arm = "exploited (fishing)", state_dir = STATE_DIR,
             # MAN$base_path is keyed by arm; a manifest without one, or an
             # empty ARM, yields NULL and F07 then fails on basename(NULL).
             # Fall back to the manifest's own meta, then to the string "unknown".
             base_params = {
               bp <- if (!is.null(MAN$base_path) && nzchar(ARM))
                       unname(MAN$base_path[[ARM]]) else NULL
               if (is.null(bp)) bp <- MAN$meta$base
               if (is.null(bp)) bp <- MAN$base
               if (is.null(bp) || !nzchar(bp)) "unknown" else as.character(bp)
             },
             units = c(cons = "g/yr (domain total)", size_prop = "proportion"),
             built = Sys.time()), OUT_RDS)
cat("wrote", OUT_RDS, "\n")
cat("  cons dim:", dim(CONS), "| size_prop dim:", dim(SZ), "\n")