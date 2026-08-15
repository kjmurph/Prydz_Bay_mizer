# =============================================================================
# Phase 96 -- a NEW substitution: lift the lower-trophic groups, and widen the
# whale range
#
# Phase 87 is NOT modified. It produced the draws the phase-88 ensemble rests on
# and must keep reproducing them; this writes a separate file.
#
# ----------------------------------------------------------------- why now
# The phase-88 priors are asymmetric in a way that phase 95 showed to be
# load-bearing. Whales were substituted UP (medians 9.2-23.2) while the lower
# groups were left on their original draws, where the 0.5 floor is a PILE-UP
# rather than a distribution: 59.7% of antarctic krill draws sit at exactly 0.50
# and 68.4% are below 1. Phase 95 then found that scaling whales further makes
# them eat the krill to extinction (krill at 2.4e-05 of calibrated by x5), which
# is what caps the whale stock at 2.8x and the catch ratio at 0.375. Raising the
# prey base is the other side of that lever.
#
# ------------------------------------------------------------- the three rules
# 1. THE SPLIT, for the lower groups (shelf and coastal fishes and smaller in
#    w_inf order, excluding the two handled by rule 2):
#      50% of members -> REPLACED by U(2, 10)
#      25% of members -> left exactly as drawn, and drawn from the members whose
#                        value is ALREADY below 1, so this quarter keeps the
#                        real prior rather than a synthetic low value
#      25% of members -> set to exactly 1
#    Note that REPLACE, not multiply, is deliberate: it matches how phase 87
#    already treats mesozooplankton and the whales, and it bounds the result. A
#    multiplicative rule would be unbounded above -- krill's largest draw of 75.8
#    would reach 758.
#
# 2. NO DRAW BELOW 1, for mesozooplankton and ANTARCTIC KRILL: draw < 1 is
#    replaced by U(1, 10), which is phase 87's mesozooplankton rule. Krill is
#    excluded from rule 1 and treated this way instead, so its distribution keeps
#    the shape of the original prior above 1 while losing the floor pile-up.
#
# 3. WHALES: draw < 1 replaced by U(5, 500), where phase 87 used U(5, 50).
#
# ------------------------------------------------------------------ ordering
# The species order is taken from the draws themselves and the lower set is
# defined by NAME, not by position, so a reordering upstream cannot silently
# shift which groups are treated.
#
# USAGE  Rscript R/wmin_test/96_substitute_draws_ltl.R
# ENV    P96_IN, P96_OUT, P96_SEED, P96_LTL_MIN/MAX, P96_MZ_MIN/MAX,
#        P96_WH_MIN/MAX, P96_P_UP, P96_P_LOW
# =============================================================================

suppressPackageStartupMessages({library(dplyr)})

OL  <- "Output_large_files/wmin_test"
IN  <- Sys.getenv("P96_IN",  file.path(OL, "43_member_draws.rds"))
OUT <- Sys.getenv("P96_OUT", file.path(OL, "96_member_draws_ltl.rds"))
SEED <- as.integer(Sys.getenv("P96_SEED", "20260815"))
LTL <- c(as.numeric(Sys.getenv("P96_LTL_MIN", "2")),
         as.numeric(Sys.getenv("P96_LTL_MAX", "10")))
MZ  <- c(as.numeric(Sys.getenv("P96_MZ_MIN", "1")),
         as.numeric(Sys.getenv("P96_MZ_MAX", "10")))
WH  <- c(as.numeric(Sys.getenv("P96_WH_MIN", "5")),
         as.numeric(Sys.getenv("P96_WH_MAX", "500")))
P_UP  <- as.numeric(Sys.getenv("P96_P_UP",  "0.50"))   # -> U(LTL)
P_LOW <- as.numeric(Sys.getenv("P96_P_LOW", "0.25"))   # -> kept, already <1
stopifnot(P_UP > 0, P_LOW > 0, P_UP + P_LOW < 1)

WHALES <- trimws(strsplit(Sys.getenv("P96_WHALES",
  "baleen whales,minke whales,sperm whales,orca"), ",")[[1]])
MESO  <- "mesozooplankton"
KRILL <- "antarctic krill"
# Rule 1 set: shelf and coastal fishes and everything smaller, minus the two
# groups that rule 2 handles. By NAME so upstream reordering cannot shift it.
LTL_SPLIT <- c("other krill", "other macrozooplankton", "salps",
               "mesopelagic fishes", "bathypelagic fishes",
               "shelf and coastal fishes")

if (file.exists(OUT) && !nzchar(Sys.getenv("P96_FORCE")))
  stop("refusing to overwrite: ", OUT, " (set P96_FORCE=1)", call. = FALSE)
if (!file.exists(IN)) stop("missing ", IN, call. = FALSE)

DR <- readRDS(IN)
members <- names(DR$draws)
sp_all <- names(DR$draws[[1]]$abundance_scaling)
NM <- length(members)
stopifnot(all(c(WHALES, MESO, KRILL, LTL_SPLIT) %in% sp_all))

cat("=== Phase 96: lift the lower groups, widen the whales ===\n")
cat("in :", IN, "\nout:", OUT, "\nseed:", SEED, "| members:", NM, "\n\n")
cat(sprintf("rule 1  split  %.0f%% -> U(%g, %g) | %.0f%% kept (already <1) | %.0f%% -> 1\n",
            100*P_UP, LTL[1], LTL[2], 100*P_LOW, 100*(1 - P_UP - P_LOW)))
cat("        ", paste(LTL_SPLIT, collapse = ", "), "\n")
cat(sprintf("rule 2  <1 -> U(%g, %g)   | %s, %s\n", MZ[1], MZ[2], MESO, KRILL))
cat(sprintf("rule 3  <1 -> U(%g, %g) | %s\n\n", WH[1], WH[2],
            paste(WHALES, collapse = ", ")))

set.seed(SEED)
log_rows <- list()
note <- function(m, s, was, now, rule)
  data.frame(sim_index = as.integer(m), species = s, drawn = was,
             substituted = now, rule = rule, stringsAsFactors = FALSE)

# --- rule 1: the three-way split ---------------------------------------------
alloc <- list()
for (s in LTL_SPLIT) {
  v <- vapply(members, function(m) as.numeric(DR$draws[[m]]$abundance_scaling[s]),
              numeric(1))
  low_pool <- members[is.finite(v) & v < 1]
  n_low <- round(P_LOW * NM); n_up <- round(P_UP * NM)
  if (length(low_pool) < n_low)
    stop("species '", s, "': only ", length(low_pool), " members are below 1 but ",
         n_low, " are needed for the keep-low quarter. Lower P96_P_LOW.",
         call. = FALSE)
  keep_low <- sample(low_pool, n_low)
  rest <- setdiff(members, keep_low)
  up <- sample(rest, n_up)
  at_one <- setdiff(rest, up)
  for (m in up) {
    was <- as.numeric(DR$draws[[m]]$abundance_scaling[s])
    now <- runif(1, LTL[1], LTL[2])
    DR$draws[[m]]$abundance_scaling[s] <- now
    log_rows[[length(log_rows) + 1]] <- note(m, s, was, now, "up")
  }
  for (m in at_one) {
    was <- as.numeric(DR$draws[[m]]$abundance_scaling[s])
    DR$draws[[m]]$abundance_scaling[s] <- 1
    log_rows[[length(log_rows) + 1]] <- note(m, s, was, 1, "one")
  }
  alloc[[s]] <- data.frame(species = s, n_up = length(up),
                           n_keep_low = length(keep_low),
                           n_at_one = length(at_one),
                           pool_below_1 = length(low_pool))
}
cat("--- rule 1 allocation ---\n")
print(bind_rows(alloc), row.names = FALSE)

# --- rules 2 and 3: conditional, phase-87 style -------------------------------
# One draw per member per species so the stream does not depend on which members
# happen to be below 1, exactly as phase 87 does.
cond <- c(MESO, KRILL, WHALES)
RND <- matrix(NA_real_, nrow = NM, ncol = length(cond),
              dimnames = list(members, cond))
for (s in cond) {
  rng <- if (s %in% c(MESO, KRILL)) MZ else WH
  RND[, s] <- runif(NM, rng[1], rng[2])
}
for (m in members) for (s in cond) {
  was <- as.numeric(DR$draws[[m]]$abundance_scaling[s])
  if (is.finite(was) && was < 1) {
    DR$draws[[m]]$abundance_scaling[s] <- RND[m, s]
    log_rows[[length(log_rows) + 1]] <- note(m, s, was, RND[m, s],
      if (s %in% c(MESO, KRILL)) "no_below_1" else "whale")
  }
}
SUBS <- bind_rows(log_rows)

cat("\n--- resulting distribution, treated groups ---\n")
print(as.data.frame(do.call(rbind, lapply(c(LTL_SPLIT, MESO, KRILL, WHALES),
  function(s) {
    v <- vapply(members, function(m)
      as.numeric(DR$draws[[m]]$abundance_scaling[s]), numeric(1))
    data.frame(species = s, min = signif(min(v), 3),
               median = signif(median(v), 3), max = signif(max(v), 3),
               pct_below_1 = round(100 * mean(v < 1), 1),
               pct_at_1 = round(100 * mean(v == 1), 1))
  }))), row.names = FALSE)

# --- checks -------------------------------------------------------------------
chk <- readRDS(IN)
moved <- do.call(rbind, lapply(members, function(m) {
  a <- chk$draws[[m]]$abundance_scaling; b <- DR$draws[[m]]$abundance_scaling
  d <- which(abs(as.numeric(a) - as.numeric(b)) > 0)
  if (!length(d)) return(NULL)
  data.frame(sim_index = as.integer(m), species = sp_all[d],
             was = as.numeric(a)[d], stringsAsFactors = FALSE)
}))
treated <- c(LTL_SPLIT, MESO, KRILL, WHALES)
untouched <- setdiff(sp_all, treated)
cat("\n--- checks ---\n")
cat("cells changed                              : ", nrow(moved), "\n", sep = "")
cat("any changed species outside the treated set: ",
    any(!moved$species %in% treated), "\n", sep = "")
cat("untouched groups (must be unchanged)       : ",
    paste(untouched, collapse = ", "), "\n", sep = "")
stopifnot(!any(!moved$species %in% treated))
# rules 2 and 3 may only ever raise a value that was below 1
c23 <- SUBS %>% filter(rule %in% c("no_below_1", "whale"))
stopifnot(all(c23$drawn < 1), all(c23$substituted >= 1))
# no treated-for-no-below-1 group may be left below 1
for (s in c(MESO, KRILL)) {
  v <- vapply(members, function(m)
    as.numeric(DR$draws[[m]]$abundance_scaling[s]), numeric(1))
  if (any(v < 1)) stop(s, " still has ", sum(v < 1), " draws below 1",
                       call. = FALSE)
}
cat("mesozooplankton / antarctic krill below 1  : 0 (asserted)\n")

DR$substitution <- list(
  seed = SEED, source = IN,
  ltl_split = list(species = LTL_SPLIT, range = LTL,
                   p_up = P_UP, p_low = P_LOW, p_one = 1 - P_UP - P_LOW),
  no_below_1 = list(species = c(MESO, KRILL), range = MZ),
  whales = WHALES, whale_range = WH,
  # phase 88 asserts on these names; keep them so the draws file is a
  # drop-in replacement for 87's
  mesozoo_range = MZ, log = SUBS, built = Sys.time())
saveRDS(DR, OUT)
write.csv(SUBS, file.path(OL, "96_substitution_log.csv"), row.names = FALSE)
cat("\nWROTE ", OUT, "\n", sep = "")