# =============================================================================
# Phase 87 -- substitute the downward draws, ONCE, and write them to disk
#
# WHY THIS IS A SEPARATE SCRIPT. The existing ensemble carries a standing caveat
# that it CANNOT be regenerated: re-running the protocol reorders the ranking
# because several members are bistable. That is a consequence of the draws being
# re-made inside each run. Here the substitution happens once, under a recorded
# seed, and is WRITTEN TO DISK; the member build then reads a fixed file. The
# ensemble becomes reproducible from a stored input rather than from a hope, and
# the seed is documentation rather than a claim.
#
# ------------------------------------------------------------------ the rule
# MESOZOOPLANKTON, draw < 1 -> U(MZ_MIN, MZ_MAX), default U(1, 10)
#   There is no hypothesis under which a member should hold LESS
#   mesozooplankton: it takes 79% of its encounter from the prescribed resource,
#   which is identical across members, so a member-specific reduction is an
#   artefact of applying one random draw to all 19 groups. The range samples
#   possibilities rather than asserting a single value; note it is CONSERVATIVE
#   against the draws themselves, whose >1 median is 11.19 and upper quartile
#   60.5.
#
# WHALE GROUPS, draw < 1 -> U(WH_MIN, WH_MAX), default U(5, 50)
#   baleen, minke, sperm whales AND orca. All four were direct targets of
#   extensive exploitation, so the hypothesis is that their pre-exploitation
#   populations were much larger than the contemporary-calibrated reference.
#   A DIFFERENT rationale from mesozooplankton and a different range; say so in
#   the methods rather than presenting them as one rule.
#
# Draws ABOVE 1 are never touched, for any group. Every other species is
# untouched entirely.
#
# USAGE  Rscript R/wmin_test/87_substitute_draws.R
# ENV    P87_IN, P87_OUT, P87_SEED, P87_MZ_MIN/MAX, P87_WH_MIN/MAX, P87_WHALES
# =============================================================================

suppressPackageStartupMessages({library(dplyr)})

OL  <- "Output_large_files/wmin_test"
IN  <- Sys.getenv("P87_IN",  file.path(OL, "43_member_draws.rds"))
OUT <- Sys.getenv("P87_OUT", file.path(OL, "87_member_draws_substituted.rds"))
SEED <- as.integer(Sys.getenv("P87_SEED", "20260814"))
MZ <- c(as.numeric(Sys.getenv("P87_MZ_MIN", "1")),
        as.numeric(Sys.getenv("P87_MZ_MAX", "10")))
WH <- c(as.numeric(Sys.getenv("P87_WH_MIN", "5")),
        as.numeric(Sys.getenv("P87_WH_MAX", "50")))
WHALES <- trimws(strsplit(Sys.getenv("P87_WHALES",
  "baleen whales,minke whales,sperm whales,orca"), ",")[[1]])
MESO <- "mesozooplankton"
if (file.exists(OUT) && !nzchar(Sys.getenv("P87_FORCE")))
  stop("refusing to overwrite: ", OUT, " (set P87_FORCE=1)", call. = FALSE)
if (!file.exists(IN)) stop("missing ", IN, call. = FALSE)

DR <- readRDS(IN)
members <- names(DR$draws)
sp_all <- names(DR$draws[[1]]$abundance_scaling)
stopifnot(all(c(WHALES, MESO) %in% sp_all))

cat("=== Phase 87: substitute downward draws ===\n")
cat("in :", IN, "\nout:", OUT, "\nseed:", SEED, "| members:", length(members), "\n")
cat(sprintf("mesozooplankton  <1 -> U(%g, %g)\n", MZ[1], MZ[2]))
cat(sprintf("whales           <1 -> U(%g, %g) : %s\n", WH[1], WH[2],
            paste(WHALES, collapse = ", ")))

# One draw per member per treated species, made for EVERY member so the random
# stream does not depend on which members happen to be below 1. Only members
# actually below 1 consume their value; the rest are recorded but unused.
set.seed(SEED)
treat <- c(MESO, WHALES)
RND <- matrix(NA_real_, nrow = length(members), ncol = length(treat),
              dimnames = list(members, treat))
for (s in treat) {
  rng <- if (s == MESO) MZ else WH
  RND[, s] <- runif(length(members), rng[1], rng[2])
}

log_rows <- list()
for (m in members) {
  sc <- DR$draws[[m]]$abundance_scaling
  for (s in treat) {
    drawn <- as.numeric(sc[s])
    if (is.finite(drawn) && drawn < 1) {
      DR$draws[[m]]$abundance_scaling[s] <- RND[m, s]
      log_rows[[length(log_rows) + 1]] <- data.frame(
        sim_index = as.integer(m), species = s, drawn = drawn,
        substituted = RND[m, s], stringsAsFactors = FALSE)
    }
  }
}
SUBS <- if (length(log_rows)) bind_rows(log_rows) else
  data.frame(sim_index = integer(), species = character(),
             drawn = numeric(), substituted = numeric())

cat("\n--- substitutions made ---\n")
print(as.data.frame(SUBS %>% group_by(species) %>%
  summarise(n_substituted = n(),
            pct_of_members = round(100 * n() / length(members), 1),
            med_drawn = round(median(drawn), 3),
            med_substituted = round(median(substituted), 2),
            .groups = "drop")), row.names = FALSE)

# nothing above 1 may have been touched, and nothing outside the treated set
chk <- readRDS(IN)
moved <- do.call(rbind, lapply(members, function(m) {
  a <- chk$draws[[m]]$abundance_scaling; b <- DR$draws[[m]]$abundance_scaling
  d <- which(abs(as.numeric(a) - as.numeric(b)) > 0)
  if (!length(d)) return(NULL)
  data.frame(sim_index = as.integer(m), species = sp_all[d],
             was = as.numeric(a)[d], stringsAsFactors = FALSE)
}))
cat("\n--- checks ---\n")
cat("cells changed        : ", nrow(moved), " (log says ", nrow(SUBS), ")\n", sep = "")
cat("any changed species outside the treated set: ",
    any(!moved$species %in% treat), "\n", sep = "")
cat("any changed cell that was >= 1             : ", any(moved$was >= 1), "\n",
    sep = "")
stopifnot(nrow(moved) == nrow(SUBS), !any(!moved$species %in% treat),
          !any(moved$was >= 1))

DR$substitution <- list(seed = SEED, mesozoo_range = MZ, whale_range = WH,
                        whales = WHALES, source = IN, log = SUBS,
                        built = Sys.time())
saveRDS(DR, OUT)
write.csv(SUBS, file.path(OL, "87_substitution_log.csv"), row.names = FALSE)
cat("\nWROTE ", OUT, "\n", sep = "")