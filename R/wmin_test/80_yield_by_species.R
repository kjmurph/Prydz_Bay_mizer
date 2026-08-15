# =============================================================================
# Phase 80 -- modelled vs observed YIELD by species, across the usable members
#
# WHY A NEW EXTRACTION. Nothing in the current pipeline carries per-species
# yield. KC20's `yield` table has a single column, krill_yield_g -- krill only.
# And yield is not recoverable from the stored biomass: it needs fishing
# mortality against the SIZE STRUCTURE, not the total. So this re-projects.
#
# The projection is the exploited arm only, 1841-2010, which is a quarter of
# KC20's work.
#
# THE CATCHABILITY TRAP, as in KC20/F00g: member states carry DRAWN
# catchability. The global refit (45_catchability_multipliers) MUST be applied
# before projecting, capped at QMAX -- the krill multiplier is 0.1904 and
# skipping it makes the krill fishery >5x too strong.
#
# USABLE MEMBERS ONLY -- stable AND admissible, the F00g/KC16b filter.
#
# USAGE  Rscript R/wmin_test/80_yield_by_species.R [run|collect]
# ENV    P80_STEM, P80_SUFFIX, P80_CORES, P80_CHUNK
# =============================================================================

suppressPackageStartupMessages({
  library(mizer); library(therMizer); library(parallel); library(dplyr)
})

OUT_LARGE <- "Output_large_files/wmin_test"
STEM   <- Sys.getenv("P80_STEM", "77_rmaxpostcap_n500_nomg_recap09_K1_R_max")
SUFFIX <- Sys.getenv("P80_SUFFIX", "p77n166")
CORES  <- as.integer(Sys.getenv("P80_CORES", "14"))
CHUNK  <- as.integer(Sys.getenv("P80_CHUNK", as.character(CORES)))
QMAX <- 1
STATE_DIR <- file.path(OUT_LARGE, paste0(STEM, "_states"))
WORK_DIR  <- file.path(OUT_LARGE, paste0("80_yield_chunks_", SUFFIX))
OUT_RDS   <- file.path(OUT_LARGE, sprintf("80_yield_by_species_%s.rds", SUFFIX))
dir.create(WORK_DIR, recursive = TRUE, showWarnings = FALSE)
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "run"

SRC <- readRDS(file.path(OUT_LARGE, paste0(STEM, ".rds")))
MEM <- SRC$members
adm <- if ("n_erepro_ge1" %in% names(MEM)) MEM$n_erepro_ge1 == 0 else TRUE
members <- MEM$sim_index[MEM$stable & adm]
cat("=== Phase 80: yield by species ===\n")
cat("stem:", STEM, "| members", nrow(MEM), "-> usable", length(members), "\n")
if (!length(members)) stop("no usable members", call. = FALSE)

# P80_MULT: the refit whose multipliers are applied. The phase-45 default belongs
# to ensemble 44; a phase-88 run must pass 89_refit_results.rds, which was fitted
# on the ISIMIP3a window over the usable screen. Getting this wrong is silent --
# both files carry a $M of the same shape.
MULT <- readRDS(Sys.getenv("P80_MULT",
  file.path(OUT_LARGE, "45_catchability_multipliers.rds")))$M
effort_arr <- readRDS("effort_array_1841_2010.rds")
chunks <- split(members, ceiling(seq_along(members) / CHUNK))
chunk_file <- function(ci) file.path(WORK_DIR, sprintf("y_%03d.rds", ci))

worker <- function(si) {
  suppressPackageStartupMessages({library(mizer); library(therMizer)})
  z <- readRDS(file.path(STATE_DIR, sprintf("state_%05d.rds", si)))
  p <- z$params
  gp <- gear_params(p)
  m <- MULT[match(gp$species, names(MULT))]; m[is.na(m)] <- 1
  gp$catchability <- pmin(QMAX, pmax(0, gp$catchability * m))
  gear_params(p) <- gp
  s <- try(project(p, initial_n = z$initial_n, t_start = 1841,
                   effort = effort_arr, progress_bar = FALSE), silent = TRUE)
  if (inherits(s, "try-error")) return(NULL)
  y <- getYield(s)                      # time x species, grams per year
  yr <- as.numeric(rownames(y)); keep <- yr <= 2010
  data.frame(sim_index = si,
             Year = rep(yr[keep], times = ncol(y)),
             Species = rep(colnames(y), each = sum(keep)),
             yield_t = as.vector(y[keep, ]) / 1e6,
             stringsAsFactors = FALSE)
}

if (mode == "run") {
  todo <- which(!vapply(seq_along(chunks),
                        function(ci) file.exists(chunk_file(ci)), logical(1)))
  cat("chunks:", length(chunks), "| to run:", length(todo),
      "| cores:", CORES, "\n\n")
  t0 <- proc.time()
  for (ci in todo) {
    cl <- makeCluster(CORES)
    clusterExport(cl, c("STATE_DIR", "MULT", "QMAX", "effort_arr", "worker"),
                  envir = environment())
    r <- parLapplyLB(cl, chunks[[ci]], function(si)
      tryCatch(worker(si), error = function(e) NULL))
    stopCluster(cl)
    saveRDS(r, chunk_file(ci))
    cat(sprintf("[%s] chunk %d/%d (%d members, %.1f min)\n",
                format(Sys.time(), "%H:%M:%S"), ci, length(chunks),
                length(chunks[[ci]]), (proc.time() - t0)[["elapsed"]] / 60))
    flush.console()
  }
  cat("\nrun complete. now: Rscript ... collect\n")
  quit(save = "no")
}

# ------------------------------------------------------------------ collect ---
files <- sort(list.files(WORK_DIR, pattern = "^y_\\d+\\.rds$", full.names = TRUE))
if (!length(files)) stop("no chunks -- run first", call. = FALSE)
Z <- unlist(lapply(files, readRDS), recursive = FALSE)
Z <- Z[!vapply(Z, is.null, logical(1))]
Y <- bind_rows(Z)
cat("collected", length(unique(Y$sim_index)), "members |", nrow(Y), "rows\n")

# yield_observed_timeseries.csv is WIDE: Year + one column per species, in
# GRAMS per year. read.csv turns spaces into dots, so the names are mapped back
# against the model's own species vector rather than un-dotted by hand.
obs_w <- read.csv("yield_observed_timeseries.csv", stringsAsFactors = FALSE,
                  check.names = FALSE)
SPN <- suppressWarnings(validParams(readRDS(SRC$meta$base)))@species_params$species
nm <- setdiff(names(obs_w), "Year")
back <- SPN[match(make.names(SPN), make.names(nm))]
if (anyNA(back))
  stop("cannot map observed columns to model species: ",
       paste(nm[is.na(back)], collapse = ", "), call. = FALSE)
obs <- obs_w %>%
  tidyr::pivot_longer(-Year, names_to = "col", values_to = "obs_g") %>%
  mutate(Species = SPN[match(make.names(col), make.names(nm))],
         obs_t = obs_g / 1e6) %>%
  select(Species, Year, obs_t)
fished <- sort(unique(obs$Species[obs$obs_t > 0]))
cat("fished species with observations:", length(fished), "\n")

S <- Y %>% filter(Species %in% fished) %>%
  group_by(Species, Year) %>%
  summarise(med = median(yield_t), q25 = quantile(yield_t, .25),
            q75 = quantile(yield_t, .75), .groups = "drop")
saveRDS(list(series = S, raw = Y, observed = obs, members = members,
             meta = list(stem = STEM, base = SRC$meta$base, qmax = QMAX,
                         n_members = length(unique(Y$sim_index)),
                         built = Sys.time())), OUT_RDS)
write.csv(S, file.path(OUT_LARGE, sprintf("80_yield_series_%s.csv", SUFFIX)),
          row.names = FALSE)
cat("WROTE", OUT_RDS, "\n")