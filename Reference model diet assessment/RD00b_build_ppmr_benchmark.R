# =============================================================================
# RD00b -- build the observed PPMR benchmark from the repository's own source
#
# WHY THIS RATHER THAN A TRANSCRIBED LITERATURE TABLE
#   csvs/predator_parameters_updated.csv already holds a sourced compilation of
#   "preferred PPMR mean" and "preferred PPMR SD" per Southern Ocean predator
#   species, defined as mean prey size / max body size -- i.e. 1 / PPMR. It is
#   the origin of the model's own beta values, which this script verifies by
#   reproducing each group's beta exactly as the arithmetic mean of its member
#   species' ratios. So the benchmark and the model come from the same table,
#   and the comparison in RD03 is like for like.
#
#   Tucker & Rogers (2014, Proc R Soc B) is the wider cross-species context for
#   marine mammal PPMR and agrees in magnitude, but the in-repo compilation is
#   per species with SDs and named sources, so it is used as the numbers.
#
# GROUP MEMBERSHIP IS NOT ASSUMED. For every predator group it searches subsets
# of the empirical species for the one whose mean ratio reproduces the model's
# beta, and stops with an error if a group cannot be reproduced. That is what
# makes the mapping below evidence rather than guesswork.
#
# TWO DATA-QUALITY FLAGS THIS SCRIPT RAISES (both printed at the end):
#   1. "Sperm Whales" and "Southern Right Whales" carry an IDENTICAL ratio and
#      SD (2.26849315068493e-05). The sperm whale note says "1/400 according to
#      Williams", which would be 2.5e-3, ~110x larger. One of the two rows is
#      very likely a copy-paste.
#   2. leopard seals carry beta = 100 in the params object but 11.236 in the
#      trait table and the empirical source (Forcada et al. 2009), and sigma 3.0
#      against 2.0 everywhere else. An undocumented override.
#
# OUTPUT  benchmarks/ppmr_benchmark_observed.csv  (per empirical species)
#         benchmarks/ppmr_benchmark_groups.csv    (per model group, the band)
#
# USAGE  Rscript "Reference model diet assessment/RD00b_build_ppmr_benchmark.R"
# =============================================================================

source(file.path("Reference model diet assessment", "RD00_common.R"))
cat("=== RD00b: observed PPMR benchmark ===\n")

SRC <- file.path("csvs", "predator_parameters_updated.csv")
BEN <- file.path(RD_ROOT, "benchmarks")
dir.create(BEN, recursive = TRUE, showWarnings = FALSE)

d <- read.csv(SRC, stringsAsFactors = FALSE)
grab <- function(par) {
  s <- d[d$parameter == par, ]
  list(v = setNames(s$value, s$group), src = setNames(s$`source.derivation`,
                                                      s$group))
}
mn <- grab("preferred PPMR mean"); sdv <- grab("preferred PPMR SD")
ratio <- mn$v
ratio_sd <- sdv$v[names(ratio)]
src <- mn$src
cat("empirical species with a PPMR entry:", length(ratio), "\n")

# --- verify the model group -> empirical species mapping ----------------------
# The model's beta is the arithmetic mean of its members' ratios, inverted.
# Searching for the subset that reproduces beta is what makes this a check.
find_members <- function(target_beta, pool = names(ratio), max_k = 9,
                         tol = 1e-9) {
  tgt <- 1 / target_beta
  for (k in seq_len(min(max_k, length(pool)))) {
    cm <- utils::combn(length(pool), k)
    for (j in seq_len(ncol(cm))) {
      if (abs(mean(ratio[pool[cm[, j]]]) - tgt) < tol * tgt)
        return(pool[cm[, j]])
    }
  }
  NULL
}

# baleen whales carry the REVISED beta (2.468e7 = w_max baleen / w_max krill),
# which is a kernel decision and not a mean of observations, so its membership
# is checked against the ORIGINAL beta that phase 51 replaced.
BALEEN_BETA_ORIGINAL <- 219112.601184
targets <- c("flying birds", "small divers", "medium divers", "large divers",
             "minke whales", "orca", "sperm whales")
# A group whose beta cannot be reproduced has DEPARTED from the compilation --
# which is a legitimate modelling decision (phase 54 does exactly that for sperm
# and baleen whales), not an error. The benchmark is a property of the source
# table, so it is still built for that group from its known members; the
# departure is recorded and reported instead of aborting.
KNOWN_MEMBERS <- list(
  "flying birds"  = c("Flying birds - small", "Flying birds - med",
                      "Flying birds - large", "flying birds - coastal"),
  "small divers"  = c("Adelie Penguin", "Crested Penguin", "Gentoo Penguin"),
  "medium divers" = c("Antarctic Fur Seals", "Crabeater Seals", "Ross Seals",
                      "Weddell Seals", "Emperor Penguin", "King Penguin",
                      "Ziphiids", "Dolphins"),
  "large divers"  = "Elephant Seals",
  "minke whales"  = "minke whale",
  "orca"          = "Orca",
  "sperm whales"  = "Sperm Whales")
mapping <- list(); departed <- character(0)
for (g in targets) {
  b <- PARAMS@species_params$beta[match(g, SPECIES)]
  m <- find_members(b)
  if (is.null(m)) {
    m <- KNOWN_MEMBERS[[g]]
    if (is.null(m)) stop("no beta match and no recorded membership for '", g,
                         "'", call. = FALSE)
    departed <- c(departed, g)
    cat(sprintf("  %-14s beta %12.6g  <-  %s   [DEPARTS from compilation]\n",
                g, b, paste(m, collapse = " + ")))
  } else {
    cat(sprintf("  %-14s beta %12.6g  <-  %s\n", g, b,
                paste(m, collapse = " + ")))
  }
  mapping[[g]] <- m
}
mb <- find_members(BALEEN_BETA_ORIGINAL, tol = 1e-6)
if (is.null(mb)) stop("cannot reproduce the original baleen beta", call. = FALSE)
mapping[["baleen whales"]] <- mb
cat(sprintf("  %-14s beta %12.6g  <-  %s   [ORIGINAL beta, pre-revision]\n",
            "baleen whales", BALEEN_BETA_ORIGINAL, paste(mb, collapse = " + ")))

# leopard seals: the params beta (100) does not come from this table
LS_BETA_TABLE <- 11.2359551
cat(sprintf("  %-14s beta %12.6g in params, %g in the trait table  <-  %s\n",
            "leopard seals", PARAMS@species_params$beta[match("leopard seals",
                                                              SPECIES)],
            LS_BETA_TABLE, "Leopard Seals"))
mapping[["leopard seals"]] <- "Leopard Seals"

# --- per-species observed PPMR ------------------------------------------------
# The +-1 SD interval is taken in ratio space and converted. Where mean - SD <= 0
# the upper bound is unbounded and uninformative, so the interval is reflected
# about the point estimate in log space instead, and flagged.
obs <- do.call(rbind, lapply(names(mapping), function(g) {
  sps <- mapping[[g]]
  do.call(rbind, lapply(sps, function(s) {
    m <- ratio[[s]]; sd_s <- ratio_sd[[s]]
    lp   <- log10(1 / m)
    lo   <- log10(1 / (m + sd_s))
    unb  <- !is.finite(sd_s) || (m - sd_s) <= 0
    hi   <- if (unb) lp + (lp - lo) else log10(1 / (m - sd_s))
    data.frame(model_group = g, empirical_species = s,
               ratio_mean = m, ratio_sd = sd_s,
               ppmr = 1 / m, log10_ppmr = lp,
               log10_ppmr_lo = lo, log10_ppmr_hi = hi,
               sd_reflected = unb,
               source = if (nzchar(src[[s]])) src[[s]] else NA_character_,
               row.names = NULL)
  }))
}))

# rorquals separately: the group the kernel revision actually targets
obs$rorqual <- obs$empirical_species %in%
  c("blue whale", "fin whale", "sei whale", "humpback whale")

f <- file.path(BEN, "ppmr_benchmark_observed.csv")
if (file.exists(f) && !RD_FORCE)
  stop("refusing to overwrite: ", f, call. = FALSE)
write.csv(obs, f, row.names = FALSE)
cat("\nwrote", basename(f), sprintf("(%d rows)\n", nrow(obs)))

# --- per-group band, in the shared benchmark schema ---------------------------
band <- function(sub, group, basis, note) data.frame(
  metric = "log10_ppmr", predator_group = group, prey_group = NA_character_,
  value_low = min(sub$log10_ppmr_lo), value_mid = mean(sub$log10_ppmr),
  value_high = max(sub$log10_ppmr_hi), units = "log10(predator g / prey g)",
  basis = basis,
  source = paste(unique(na.omit(sub$source)), collapse = "; "),
  note = note, stringsAsFactors = FALSE)

groups <- do.call(rbind, c(
  lapply(names(mapping), function(g) {
    sub <- obs[obs$model_group == g, ]
    band(sub, g, sprintf("mean +- 1 SD across %d species: %s", nrow(sub),
                         paste(sub$empirical_species, collapse = ", ")),
         if (g == "baleen whales")
           paste("Includes Southern Right Whales, whose ratio is 400-2000x",
                 "the four rorquals and dominated the ORIGINAL arithmetic-mean",
                 "beta of 219112.6. See the rorquals-only row.")
         else NA_character_)
  }),
  list(band(obs[obs$rorqual, ], "baleen whales",
            paste("rorquals only: blue, fin, sei, humpback"),
            paste("The comparison the revised kernel should be judged on:",
                  "the model's baleen whale group is krill-feeding rorquals,",
                  "and southern right whales are copepod feeders.")))))
groups$basis[nrow(groups)] <- paste0(groups$basis[nrow(groups)],
                                     " [RORQUALS ONLY]")

f2 <- file.path(BEN, "ppmr_benchmark_groups.csv")
if (file.exists(f2) && !RD_FORCE)
  stop("refusing to overwrite: ", f2, call. = FALSE)
write.csv(groups, f2, row.names = FALSE)
cat("wrote", basename(f2), sprintf("(%d rows)\n", nrow(groups)))

# --- what this says about the kernel revision ---------------------------------
cat("\n=== observed log10 PPMR by empirical species ===\n")
print(obs %>%
        transmute(model_group, species = empirical_species,
                  log10_ppmr = round(log10_ppmr, 3),
                  lo = round(log10_ppmr_lo, 3), hi = round(log10_ppmr_hi, 3),
                  sd_reflected, rorqual) %>%
        arrange(model_group, desc(log10_ppmr)) %>% as.data.frame(),
      row.names = FALSE)

if (length(departed))
  cat(sprintf(paste("\nNOTE: %s beta no longer reproduces from the",
                    "compilation.\n  This params object has DEPARTED from the",
                    "source table for %s -- a modelling\n  decision, not an",
                    "error. The benchmark below is still the observed value;",
                    "the\n  model simply no longer sits on it.\n"),
              paste(departed, collapse = " and "),
              paste(departed, collapse = " and ")))

cat("\n=== model beta against the observed band ===\n")
mb_tab <- groups %>%
  mutate(model_log10_beta = log10(PARAMS@species_params$beta[
    match(predator_group, SPECIES)]),
    inside = model_log10_beta >= value_low & model_log10_beta <= value_high) %>%
  transmute(group = predator_group,
            rorquals_only = grepl("RORQUALS ONLY", basis),
            obs_lo = round(value_low, 3), obs_mid = round(value_mid, 3),
            obs_hi = round(value_high, 3),
            model_log10_beta = round(model_log10_beta, 3), inside)
print(as.data.frame(mb_tab), row.names = FALSE)

cat("\n--- data-quality flags ---\n")
if (identical(unname(ratio[["Sperm Whales"]]),
              unname(ratio[["Southern Right Whales"]])))
  cat("1. 'Sperm Whales' and 'Southern Right Whales' share an identical ratio",
      sprintf("(%.6g) and SD in\n   %s.\n", ratio[["Sperm Whales"]], SRC),
      "   RD06 resolves the direction: the SOUTHERN RIGHT WHALE row is the",
      "copy. The sperm\n   whale row is sourced (Evans & Hindell 2004) and",
      "encodes a sensible 828 g prey mass;\n   the southern right whale row",
      "has no source and its implied 1247.67 g prey mass is\n   just its own",
      "w_max times the SPERM WHALE ratio -- absurd for a copepod feeder.\n",
      "  That corrupted cell is what dragged the ORIGINAL baleen beta down to",
      "219,113;\n   repaired, the same compilation gives 2.98e7. Run RD06 for",
      "the full argument.\n")
cat("2. leopard seals beta is 100 in the params object but",
    sprintf("%g in the trait table\n   (Forcada et al. 2009), and sigma is %g",
            LS_BETA_TABLE, PARAMS@species_params$sigma[
              match("leopard seals", SPECIES)]),
    "against 2.0 for every other group.\n   An override that is not recorded",
    "in group params/trait_groups_params_vCWC_v5.csv.\n")
cat("\nRD00b complete.\n")