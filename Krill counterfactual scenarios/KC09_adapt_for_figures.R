# =============================================================================
# KC09 -- reshape a KC07 run into the two shapes the figure scripts read
#
# KC04 (main figure) reads a KC02-shaped object: list(krill, members, meta).
# KC06 (supplement)  reads a KC05-shaped object: list(abund_mass, members, meta).
# KC07 stores a superset of both, plus a `base` column. This writes the two
# views rather than editing KC04/KC06, which are gated against the published
# phase-54 numbers and should not be touched to accommodate a new input.
#
# KC07 was verified to reproduce the published extractors exactly on the 10
# shared phase-54 members: krill consumption to 4.06e-16 against KC02 (pure
# round-off from a different aggregation order) and Abundance/Biomass to exactly
# 0 against KC05. So the views written here are like-for-like with the published
# build and the figures can be compared directly.
#
# USAGE  Rscript "Krill counterfactual scenarios/KC09_adapt_for_figures.R"
# ENV    KC9_IN  (default Output_large_files/wmin_test/KC07_whres05_n167_phase55.rds)
#        KC9_TAG (default whres05) -- names the outputs and the figure suffix
# =============================================================================

suppressPackageStartupMessages({library(dplyr)})

OUT_LARGE <- "Output_large_files/wmin_test"
KC_ROOT <- Sys.getenv("KC_OUT", "Krill counterfactual scenarios")
ANA <- file.path(KC_ROOT, "analysis")
dir.create(ANA, recursive = TRUE, showWarnings = FALSE)
IN  <- Sys.getenv("KC9_IN",
                  file.path(OUT_LARGE, "KC07_whres05_n167_phase55.rds"))
TAG <- Sys.getenv("KC9_TAG", "whres05")
if (!file.exists(IN)) stop("missing ", IN, call. = FALSE)

D <- readRDS(IN); meta <- D$meta
bases <- unique(D$members$base)
if (length(bases) != 1)
  stop("KC07 input carries ", length(bases), " bases (",
       paste(bases, collapse = ", "), "); adapt one at a time via KC9_IN",
       call. = FALSE)
cat("=== KC09: adapting", basename(IN), "for the figure scripts ===\n")
cat("base:", bases, "|", basename(meta$bases[[bases]]), "\n")

MEM <- D$members %>% select(-base)
cat("members:", nrow(MEM), "| stable:", sum(MEM$stable), "\n")
if (min(D$biomass$Year) != 1841 || max(D$biomass$Year) != 2010)
  stop("biomass years are ", min(D$biomass$Year), "-", max(D$biomass$Year),
       "; the +-1 SD band needs the full 1841-2010 window", call. = FALSE)
if (min(D$consumption$Year) != 1841)
  stop("consumption starts at ", min(D$consumption$Year),
       "; KC04's band window requires 1841 -- rerun KC07 with KC7_DIET_FROM=1841",
       call. = FALSE)

meta_out <- list(params = meta$bases[[bases]], base = bases,
                 n_members = nrow(MEM), members = MEM$sim_index,
                 spinup_years = meta$spinup_years, peak_year = meta$peak_year,
                 diet_from = meta$diet_from, multipliers = meta$multipliers,
                 source = basename(IN), built = format(Sys.time()))

# --- KC02-shaped view for KC04 -----------------------------------------------
kc02 <- list(
  krill = D$consumption %>%
    select(sim_index, arm, Year, Species, krill_consumed) %>%
    as.data.frame(),
  members = MEM, meta = meta_out)
f2 <- file.path(ANA, sprintf("KC09_%s_scenarios.rds", TAG))
saveRDS(kc02, f2)
cat("\nwrote", basename(f2), sprintf("(%.0f MB) -- KC04 input\n",
    file.info(f2)$size / 1024^2))

# --- KC05-shaped view for KC06 -----------------------------------------------
kc05 <- list(
  abund_mass = D$biomass %>%
    select(sim_index, arm, Year, Species, Abundance, Biomass) %>%
    as.data.frame(),
  members = MEM,
  # gate_G1_max_rel is KC05's bit-exactness receipt. KC07 re-projects from
  # freshly rebuilt members rather than from stored states, so there is no such
  # receipt here; NA records that honestly instead of implying one.
  meta = c(meta_out, list(gate_G1_max_rel = NA_real_)))
f5 <- file.path(OUT_LARGE, sprintf("KC09_abund_mass_%s.rds", TAG))
saveRDS(kc05, f5)
cat("wrote", basename(f5), sprintf("(%.0f MB) -- KC06 input\n",
    file.info(f5)$size / 1024^2))

cat("\nnext:\n")
cat(sprintf("  KC_IN=%s KC_SUF=_%s Rscript \"%s/KC04_figure4_by_group.R\"\n",
            basename(f2), TAG, KC_ROOT))
cat(sprintf("  KC_IN5=%s KC_SUF=_%s Rscript \"%s/KC06_supp_whale_abund_mass.R\"\n",
            f5, TAG, KC_ROOT))
cat("\nKC09 complete.\n")