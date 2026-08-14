# =============================================================================
# Phase 82 -- write the FULL-SIZE-RANGE biomass/slope tables from F00g's chunks
#
# NO RE-PROJECTION. F00g's worker already computes BOTH the 1 g-cutoff tables
# (tab_1g, slope_1g) and the full-range ones (tab_full, slope_full) -- see
# F00g_build_1g_cutoff_p61.R:168-176 -- but its collect step saves only the 1 g
# pair and uses the full-range pair solely for the printed SNR contrast, then
# discards it. Everything needed is already sitting in the chunk files.
#
# The full range is F00's canonical LBNbiom floor, w >= 3.16227766e-08 g, not a
# cutoff of any kind: it is every size bin the model carries.
#
# Writes the same three files F03/F05 read, under a distinct suffix, so the 1 g
# versions are untouched and the two can be compared.
#
# USAGE  Rscript R/wmin_test/82_collect_full_range_tables.R
# ENV    P82_CHUNKS, P82_SUFFIX, P82_SRC_SUFFIX
# =============================================================================

suppressPackageStartupMessages({library(dplyr)})

OUT_LARGE <- "Output_large_files/wmin_test"
DATA <- "Manuscript data"
SRC_SUF <- Sys.getenv("P82_SRC_SUFFIX", "1g_p77n166")
SUFFIX  <- Sys.getenv("P82_SUFFIX", "full_p77n166")
WORK_DIR <- Sys.getenv("P82_CHUNKS",
                       file.path(OUT_LARGE, paste0("F00g_chunks_", SRC_SUF)))
if (!dir.exists(WORK_DIR)) stop("no chunk dir: ", WORK_DIR, call. = FALSE)

files <- sort(list.files(WORK_DIR, pattern = "^f00g_\\d+\\.rds$",
                         full.names = TRUE))
if (!length(files)) stop("no chunk files in ", WORK_DIR, call. = FALSE)
cat("=== Phase 82: full-range tables from F00g chunks ===\n")
cat("chunks:", length(files), "| source suffix:", SRC_SUF,
    "-> writing:", SUFFIX, "\n")

Z <- unlist(lapply(files, readRDS), recursive = FALSE)
bad <- Z[!vapply(Z, `[[`, logical(1), "ok")]
if (length(bad))
  stop("chunk set contains ", length(bad), " failed members -- refusing to ",
       "write a partial rebuild", call. = FALSE)

TABF <- bind_rows(lapply(Z, `[[`, "tab_full"))
SLPF <- bind_rows(lapply(Z, `[[`, "slope_full"))
TAB1 <- bind_rows(lapply(Z, `[[`, "tab_1g"))
n_mem <- length(unique(TABF$sim_index))
cat("members:", n_mem, "| full-range rows:", nrow(TABF),
    "| years", min(SLPF$Year), "-", max(SLPF$Year), "\n")

# the full range must carry MORE biomass than the 1 g cutoff for every species
chk <- TABF %>% group_by(Species) %>% summarise(full = sum(Biomass), .groups = "drop") %>%
  inner_join(TAB1 %>% group_by(Species) %>%
               summarise(cut1g = sum(Biomass), .groups = "drop"), by = "Species") %>%
  mutate(ratio = full / cut1g)
if (any(chk$ratio < 1 - 1e-9))
  stop("full range carries LESS biomass than the 1 g cutoff for: ",
       paste(chk$Species[chk$ratio < 1 - 1e-9], collapse = ", "), call. = FALSE)
cat("\nfull-range / 1 g biomass ratio, the five largest:\n")
print(as.data.frame(chk %>% arrange(desc(ratio)) %>%
                      transmute(Species, ratio = round(ratio, 3)) %>% head(5)),
      row.names = FALSE)

sfx <- function(stem) file.path(DATA, sprintf("%s_%s.rds", stem, SUFFIX))
meta_src <- file.path(DATA, sprintf("meta_%s.rds", SRC_SUF))
MET <- if (file.exists(meta_src)) readRDS(meta_src) else list()
MET$min_w_cutoff <- NA
MET$spectrum_min_w <- 3.16227766e-08
MET$note <- paste("FULL SIZE RANGE (no cutoff), re-collected from the",
                  SRC_SUF, "F00g chunks by phase 82. Same members, same",
                  "projection; only the size range of the summation differs.")
MET$built <- Sys.time()

saveRDS(TABF %>% filter(arm == "exploited"),   sfx("biomass_abund_fish"))
saveRDS(TABF %>% filter(arm == "unexploited"), sfx("biomass_abund_clim"))
saveRDS(SLPF, sfx("nbss_slope"))
saveRDS(MET, sfx("meta"))
cat("\nWrote 4 files with suffix", SUFFIX, "\n")