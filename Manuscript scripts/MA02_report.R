# =============================================================================
# MA02 -- krill consumption attribution report. Reads MA00 (167 members, full
# prey- and size-resolved extraction) and MA01 (1,668 members, contemporary),
# plus the cached Figure 3/4 data products, and writes every analysis A-H to
# `Manuscript analysis/` as tidy CSVs plus one markdown summary.
#
# NO PROJECTION HAPPENS HERE. This script only summarises.
#
# THE FOUR VALIDATION GATES RUN FIRST AND ARE FATAL. Nothing is written unless
# all four pass:
#   V1  the published Figure 4 contemporary median is reproduced from MA01
#   V2  per-member krill consumption shares sum to 100%
#   V3  exactly 167 matched pairs, same membership, in MA00 and MA01's prefix
#   V4  MA00 reproduces the cached F00 products (biomass, krill consumption,
#       reference-period spectra) and the diet/mortality identity holds
#
# EVERY COMPARISON IS FORMED WITHIN A MATCHED MEMBER FIRST and only then
# summarised across the ensemble, as median / IQR / 5th-95th.
#
# UNITS are carried in every column name: _t_per_yr, _t, _g, _pct, _per_yr.
# Consumption and biomass are DOMAIN TOTALS -- the model domain (1.474341e12 m2)
# is encoded in the initial abundances, so no area normalisation is applied.
#
# USAGE  Rscript "Manuscript scripts/MA02_report.R"
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
options(stringsAsFactors = FALSE)

OUT_LARGE <- "Output_large_files/wmin_test"
DATA      <- "Manuscript data"
# MA_OUTDIR exists so the script can be dry-run into a scratch directory without
# tripping the no-overwrite guard on the real outputs.
OUTDIR    <- Sys.getenv("MA_OUTDIR", "Manuscript analysis")
MA1_PATH  <- Sys.getenv("MA_MA01", file.path(OUT_LARGE, "MA01_contemp1668.rds"))
dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)
DOMAIN_M2 <- 1.474341e12

guard <- function(f) {
  if (file.exists(f)) stop("refusing to overwrite an existing file: ", f, call. = FALSE); f
}
wcsv <- function(d, name) {
  f <- guard(file.path(OUTDIR, name)); write.csv(d, f, row.names = FALSE)
  cat("  wrote", name, "(", nrow(d), "rows )\n"); invisible(f)
}

MA0 <- readRDS(file.path(OUT_LARGE, "MA00_full167.rds"))
MA1 <- readRDS(MA1_PATH)
Z <- MA0$members; META <- MA0$meta
NM <- length(Z)
SPN <- Z[[1]]$kern$species
PERIODS <- META$periods; SPEC_PERIODS <- META$spec_periods

KRILL  <- "antarctic krill"
LTL    <- c("antarctic krill", "other krill", "mesozooplankton",
            "other macrozooplankton", "salps")
FISHES <- c("mesopelagic fishes", "bathypelagic fishes",
            "shelf and coastal fishes", "toothfishes")
WHALES <- c("baleen whales", "minke whales")
SEABIRDS  <- c("flying birds", "small divers")
PINNIPEDS <- c("medium divers", "large divers")

# grouped aggregates reported alongside the per-species rows
AGG <- list("Large baleen + minke whales" = WHALES,
            "All great whales"            = c("baleen whales", "minke whales",
                                              "sperm whales"),
            "All marine mammals + birds"  = c("baleen whales", "minke whales",
                                              "sperm whales", "orca",
                                              "leopard seals", SEABIRDS, PINNIPEDS),
            "Seabirds"                    = SEABIRDS,
            "Pinnipeds"                   = PINNIPEDS,
            "All fishes"                  = FISHES,
            "All fishes + squids"         = c(FISHES, "squids"),
            "Invertebrate predators"      = c("mesozooplankton", "other krill",
                                              "other macrozooplankton",
                                              "antarctic krill", "salps", "squids"),
            "All predators"               = SPN)

# --- summary helpers ----------------------------------------------------------
qs <- function(v) {
  v <- v[is.finite(v)]
  if (!length(v)) return(c(median = NA, q25 = NA, q75 = NA, p05 = NA, p95 = NA, n = 0))
  c(median = median(v), q25 = unname(quantile(v, .25)), q75 = unname(quantile(v, .75)),
    p05 = unname(quantile(v, .05)), p95 = unname(quantile(v, .95)), n = length(v))
}
# M is [member x item]; returns one tidy row per item with the five statistics
summ_mat <- function(M, idcol = "item", prefix = "") {
  d <- as.data.frame(t(apply(M, 2, qs)))
  names(d) <- paste0(prefix, c("median", "q25", "q75", "p05", "p95", "n_members"))
  d[[idcol]] <- colnames(M)
  d[, c(idcol, setdiff(names(d), idcol))]
}
md_table <- function(d, digits = 4) {
  f <- function(x) if (is.numeric(x)) formatC(x, format = "g", digits = digits) else as.character(x)
  d2 <- as.data.frame(lapply(d, f)); names(d2) <- names(d)
  paste(c(paste0("| ", paste(names(d2), collapse = " | "), " |"),
          paste0("|", paste(rep("---", ncol(d2)), collapse = "|"), "|"),
          apply(d2, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))),
        collapse = "\n")
}
G <- 1e6   # g -> t

# per-member accessor: [member x species] matrix of some per-period quantity
mm <- function(f) t(vapply(Z, f, numeric(length(SPN))))

MD <- c()        # markdown accumulator
say <- function(...) { MD <<- c(MD, paste0(...)); invisible(NULL) }

cat("=== MA02: krill consumption attribution report ===\n")
cat("MA00 members:", NM, "| MA01 members:", length(MA1$members), "\n\n")

# =============================================================================
# VALIDATION GATES
# =============================================================================
cat("--- validation gates ---\n")
VAL <- list()

## V3 -- membership -----------------------------------------------------------
CUTS <- readRDS(file.path(OUT_LARGE, "46_selection_cuts.rds"))
cutA <- as.integer(CUTS$cuts[["A unweighted RMSE"]])
ma0_mem <- as.integer(vapply(Z, function(z) z$sim_index, numeric(1)))
rank_all <- MA1$ranking
prefix_of <- function(n) as.integer(head(rank_all$sim_index[
  rank_all$sim_index %in% MA1$members], n))
ma1_167 <- prefix_of(167)
stopifnot(length(ma0_mem) == 167, setequal(ma0_mem, cutA),
          length(ma1_167) == 167, setequal(ma1_167, cutA))
VAL$V3 <- sprintf("PASS -- 167 matched pairs in MA00 and in MA01's n=167 prefix; both sets identical to cut A. Members dropped: %d (MA00), %d (MA01).",
                  nrow(META$failed), nrow(MA1$failed))
cat("V3 membership: PASS\n")

## V4 -- MA00 reproduces the cached F00 products ------------------------------
BAc <- readRDS(file.path(DATA, "biomass_abund_clim_rebuilt167.rds"))
BAf <- readRDS(file.path(DATA, "biomass_abund_fish_rebuilt167.rds"))
KRc <- readRDS(file.path(DATA, "krill_consumption_rebuilt167.rds"))
SPC <- readRDS(file.path(DATA, "spectra_ref_period_rebuilt167.rds"))
rel <- function(a, b) max(abs(a - b) / pmax(1e-300, abs(b)))

d_bio <- max(vapply(Z, function(z) {
  r <- BAc[BAc$sim_index == z$sim_index, ]
  b <- z$arms$unexploited$bio
  max(vapply(rownames(b), function(y) {
    rr <- r[r$Year == as.numeric(y), ]
    rel(b[y, ], rr$Biomass[match(colnames(b), rr$Species)]) }, numeric(1)))
}, numeric(1)))
d_kr <- max(vapply(Z, function(z) {
  r <- KRc[KRc$sim_index == z$sim_index & KRc$arm == "unexploited" &
             KRc$Year %in% 2001:2010, ]
  a <- tapply(r$krill_consumed, r$Species, mean)
  rel(z$arms$unexploited$cons[["2001-2010"]][, KRILL], a[SPN]) }, numeric(1)))
k_sp <- match(as.character(ma0_mem), dimnames(SPC$species_clim)$sim)
d_spec <- max(vapply(seq_along(Z), function(k)
  rel(Z[[k]]$arms$unexploited$spec[["2001-2010"]], SPC$species_clim[k_sp[k], , ]),
  numeric(1)))
# Diet vs predation mortality: the same flux measured two independent ways.
# Judged on an ABSOLUTE scale, normalised by the total krill flux, not per group.
# A per-group RELATIVE test is meaningless here: groups such as salps and orca
# carry ~1e-8 of the total krill flux, where FFT round-off is the whole signal
# and the relative disagreement reaches 1e-7 while the absolute disagreement is
# ~1e-15 of anything that matters. Both statistics are computed; the gate is on
# the normalised one.
id_stats <- vapply(Z, function(z) {
  o <- vapply(names(PERIODS), function(pn)
    vapply(c("exploited", "unexploited"), function(a) {
      v <- z$arms[[a]]$cons[[pn]][, KRILL]
      k <- z$arms[[a]]$kmort[[pn]]$kill_g_per_yr
      big <- v > 1e-6 * sum(v)
      c(max(abs(k - v)) / sum(v),
        if (any(big)) rel(k[big], v[big]) else 0)
    }, numeric(2)), numeric(4))
  c(max(o[c(1, 3), ]), max(o[c(2, 4), ]))
}, numeric(2))
d_id     <- max(id_stats[1, ])     # normalised by total krill flux -- the gate
d_id_rel <- max(id_stats[2, ])     # per-group relative, groups > 1e-6 of total
# MA00 vs MA01 over the shared 167
k1 <- match(as.character(ma0_mem), dimnames(MA1$krill_cons_unexploited)$sim)
d_x <- max(vapply(seq_along(Z), function(k)
  rel(Z[[k]]$arms$unexploited$cons[["2001-2010"]][, KRILL],
      colMeans(MA1$krill_cons_unexploited[k1[k], , ])), numeric(1)))
tol <- 1e-9
if (max(d_bio, d_kr, d_spec, d_id, d_x) > tol)
  stop(sprintf("V4 FAILED: bio %.3g, krill %.3g, spectra %.3g, diet/mort %.3g, MA00-MA01 %.3g",
               d_bio, d_kr, d_spec, d_id, d_x))
VAL$V4 <- sprintf(paste("PASS -- max relative difference against the cached F00 products:",
  "biomass %.3g, krill consumption %.3g, 2001-2010 spectra %.3g;",
  "MA00 vs MA01 over the shared 167 %.3g. Diet-vs-predation-mortality identity:",
  "max per-predator disagreement is %.3g of the total krill flux (and %.3g in",
  "per-group relative terms, over groups carrying more than 1e-6 of the total)."),
  d_bio, d_kr, d_spec, d_x, d_id, d_id_rel)
cat(sprintf("V4 reproduction: PASS (max rel diff %.3g)\n",
            max(d_bio, d_kr, d_spec, d_id, d_x)))

## V1 -- reproduce the published Figure 4 contemporary median -----------------
FIG4 <- read.csv(file.path(DATA, "fig4_krill_ratio_series_rebuilt167.csv"))
ratio_at <- function(idx, species, yr) {
  ce <- MA1$krill_cons_exploited[idx, as.character(yr), species, drop = FALSE]
  cu <- MA1$krill_cons_unexploited[idx, as.character(yr), species, drop = FALSE]
  e <- apply(ce, 1, sum); u <- apply(cu, 1, sum)
  r <- e / u; r[is.finite(r) & u > 0]
}
idx167 <- match(as.character(ma1_167), dimnames(MA1$krill_cons_exploited)$sim)
f4 <- do.call(rbind, lapply(
  list(c("All predators", "SPN"), c("Large baleen + minke whales", "WHALES"),
       c("All fishes", "FISHES")), function(p) {
    sset <- switch(p[2], SPN = SPN, WHALES = WHALES, FISHES = FISHES)
    mine <- median(ratio_at(idx167, sset, 2010))
    pub  <- FIG4$med[FIG4$group == p[1] & FIG4$Year == 2010]
    data.frame(group = p[1], recomputed = mine, published_fig4 = pub,
               rel_diff = abs(mine - pub) / pub) }))
if (max(f4$rel_diff) > 1e-9)
  stop("V1 FAILED: Figure 4 contemporary medians not reproduced:\n",
       paste(capture.output(print(f4)), collapse = "\n"))
VAL$V1 <- sprintf(paste("PASS -- the 2010 ensemble-median exploited/unexploited krill",
  "consumption ratio recomputed from MA01 matches `fig4_krill_ratio_series_rebuilt167.csv`",
  "for all three groups to a max relative difference of %.3g",
  "(baleen+minke %.5f vs %.5f published)."),
  max(f4$rel_diff), f4$recomputed[2], f4$published_fig4[2])
cat("V1 Figure 4 reproduction: PASS\n")

## V2 -- shares sum to 100% ---------------------------------------------------
KR1841 <- mm(function(z) z$arms$unexploited$cons[["1841-1860"]][, KRILL])
colnames(KR1841) <- SPN
SH1841 <- 100 * KR1841 / rowSums(KR1841)
d_sh <- max(abs(rowSums(SH1841) - 100))
if (d_sh > 1e-9) stop("V2 FAILED: per-member shares do not sum to 100 (max dev ", d_sh, ")")
med_sum <- sum(apply(SH1841, 2, median))
VAL$V2 <- sprintf(paste("PASS -- per-member krill consumption shares sum to 100%% for all",
  "%d members (max deviation %.3g). NOTE the ensemble MEDIAN of those shares sums to",
  "%.4f%%, not exactly 100%%, because a median is not additive; no renormalisation",
  "has been applied."), NM, d_sh, med_sum)
cat(sprintf("V2 shares: PASS (median-of-shares sums to %.4f%%)\n\n", med_sum))

# =============================================================================
# A -- pre-exploitation krill and LTL consumption partition, 1841-1860
# =============================================================================
cat("--- A: pre-exploitation partition (unexploited, 1841-1860) ---\n")
CONS0 <- lapply(Z, function(z) z$arms$unexploited$cons[["1841-1860"]])
PREY <- colnames(CONS0[[1]])

partition <- function(prey_set, label) {
  V <- t(vapply(CONS0, function(C) rowSums(C[, prey_set, drop = FALSE]), numeric(length(SPN))))
  colnames(V) <- SPN
  # aggregates are formed WITHIN member, then summarised
  A <- vapply(AGG, function(g) rowSums(V[, g, drop = FALSE]), numeric(nrow(V)))
  tot <- rowSums(V)
  bind_rows(
    cbind(level = "species",   summ_mat(V / G, "predator_group", "cons_t_per_yr_"),
          summ_mat(100 * V / tot, "predator_group", "share_pct_")[, -1]),
    cbind(level = "aggregate", summ_mat(A / G, "predator_group", "cons_t_per_yr_"),
          summ_mat(100 * A / tot, "predator_group", "share_pct_")[, -1])) %>%
    mutate(prey_group = label, .before = 1) %>%
    arrange(level, desc(share_pct_median))
}
A_krill <- partition(KRILL, "Antarctic krill")
A_ltl   <- bind_rows(partition(LTL, "LTL combined"),
                     do.call(rbind, lapply(LTL, function(p) partition(p, p))))
wcsv(A_krill, "A_krill_partition_1841_1860.csv")
wcsv(A_ltl,   "A_ltl_partition_1841_1860.csv")

# per-predator diet composition: what each predator eats, as % of its own intake
DC <- do.call(rbind, lapply(seq_along(SPN), function(i) {
  P <- t(vapply(CONS0, function(C) C[i, ], numeric(length(PREY))))
  colnames(P) <- PREY
  tot <- rowSums(P)
  cbind(predator_group = SPN[i],
        summ_mat(P / G, "prey_group", "cons_t_per_yr_"),
        summ_mat(100 * P / tot, "prey_group", "diet_pct_")[, -1],
        total_intake_t_per_yr_median = median(tot) / G) }))
wcsv(DC, "A_predator_diet_composition_1841_1860.csv")

TOT_KR <- rowSums(KR1841)
qs_tot <- qs(TOT_KR / G)
whale_share <- 100 * rowSums(KR1841[, WHALES, drop = FALSE]) / TOT_KR

# =============================================================================
# B -- size-resolved attribution of the change in krill predation
# =============================================================================
cat("--- B: size-resolved paired change in krill consumption ---\n")
WG <- readRDS(file.path(OUT_LARGE, "44_states/state_treated_00001.rds"))$params
Wv <- WG@w; DWv <- WG@dw
B_rows <- list(); Bd_rows <- list()
for (pn in names(PERIODS)) {
  # [member x (predator x bin)] paired delta, formed within member
  D <- t(vapply(Z, function(z)
    as.vector((z$arms$exploited$ksize[[pn]] - z$arms$unexploited$ksize[[pn]]) / G),
    numeric(length(SPN) * length(Wv))))
  Ef <- t(vapply(Z, function(z) as.vector(z$arms$exploited$ksize[[pn]] / G),
                 numeric(length(SPN) * length(Wv))))
  Uf <- t(vapply(Z, function(z) as.vector(z$arms$unexploited$ksize[[pn]] / G),
                 numeric(length(SPN) * length(Wv))))
  key <- expand.grid(predator_group = SPN, bin_index = seq_along(Wv),
                     stringsAsFactors = FALSE)
  colnames(D) <- colnames(Ef) <- colnames(Uf) <-
    paste(key$predator_group, key$bin_index, sep = "||")
  s <- cbind(period = pn, key,
             predator_w_g = Wv[key$bin_index], predator_dw_g = DWv[key$bin_index],
             summ_mat(D, "k", "delta_t_per_yr_")[, -1],
             exploited_t_per_yr_median   = apply(Ef, 2, median),
             unexploited_t_per_yr_median = apply(Uf, 2, median))
  B_rows[[pn]] <- s[s$exploited_t_per_yr_median > 0 |
                    s$unexploited_t_per_yr_median > 0, ]
  # log10 decade aggregation, summed WITHIN member first
  dec <- floor(log10(Wv))
  agg_by_dec <- function(M) {
    a <- array(as.vector(t(M)), dim = c(length(SPN), length(Wv), nrow(M)))
    out <- NULL
    for (d in sort(unique(dec))) {
      v <- t(apply(a[, dec == d, , drop = FALSE], c(1, 3), sum))
      colnames(v) <- paste(SPN, d, sep = "||"); out <- cbind(out, v) }
    out
  }
  Dd <- agg_by_dec(D); Ed <- agg_by_dec(Ef); Ud <- agg_by_dec(Uf)
  kk <- do.call(rbind, strsplit(colnames(Dd), "\\|\\|"))
  sd_ <- cbind(period = pn, predator_group = kk[, 1],
               log10_w_decade = as.numeric(kk[, 2]),
               summ_mat(Dd, "k", "delta_t_per_yr_")[, -1],
               exploited_t_per_yr_median   = apply(Ed, 2, median),
               unexploited_t_per_yr_median = apply(Ud, 2, median))
  Bd_rows[[pn]] <- sd_[sd_$exploited_t_per_yr_median > 0 |
                       sd_$unexploited_t_per_yr_median > 0, ]
}
B_bin <- bind_rows(B_rows); B_dec <- bind_rows(Bd_rows)
wcsv(B_bin, "B_krill_delta_by_size_bin.csv")
wcsv(B_dec, "B_krill_delta_by_decade_bin.csv")

# Sign-filtered: a period can have fewer than 10 genuinely positive contributions,
# and padding the list with the least-negative rows would label a -1e-16 entry
# "largest positive". Only rows of the matching sign are listed.
B_top <- B_dec %>% filter(delta_t_per_yr_median > 0) %>% group_by(period) %>%
  arrange(desc(delta_t_per_yr_median)) %>%
  mutate(direction = "largest positive", rank_in_period = row_number()) %>%
  filter(rank_in_period <= 10) %>% bind_rows(
    B_dec %>% filter(delta_t_per_yr_median < 0) %>% group_by(period) %>%
      arrange(delta_t_per_yr_median) %>%
      mutate(direction = "largest negative", rank_in_period = row_number()) %>%
      filter(rank_in_period <= 10)) %>%
  ungroup() %>% arrange(period, direction, rank_in_period) %>%
  select(period, direction, rank_in_period, predator_group, log10_w_decade,
         delta_t_per_yr_median, delta_t_per_yr_q25, delta_t_per_yr_q75,
         delta_t_per_yr_p05, delta_t_per_yr_p95,
         exploited_t_per_yr_median, unexploited_t_per_yr_median)
wcsv(B_top, "B_top_contributions.csv")

# null check: 1841-1860 predates all effort, so the paired delta must be exactly 0
B_null <- max(abs(B_bin$delta_t_per_yr_median[B_bin$period == "1841-1860"]))

# =============================================================================
# C -- lower-trophic-level disaggregation, and the background resource
# =============================================================================
cat("--- C: LTL disaggregation and resource spectrum ---\n")
YRS <- as.numeric(rownames(Z[[1]]$arms$exploited$bio))
pct <- function(f, c_) 100 * (f - c_) / c_
C_ts <- list()
for (g in LTL) {
  bf <- t(vapply(Z, function(z) z$arms$exploited$bio[, g],   numeric(length(YRS))))
  bc <- t(vapply(Z, function(z) z$arms$unexploited$bio[, g], numeric(length(YRS))))
  af <- t(vapply(Z, function(z) z$arms$exploited$abu[, g],   numeric(length(YRS))))
  ac <- t(vapply(Z, function(z) z$arms$unexploited$abu[, g], numeric(length(YRS))))
  mf <- bf / af; mc <- bc / ac
  for (nm in c("biomass", "abundance", "mean_individual_mass")) {
    P <- switch(nm, biomass = pct(bf, bc), abundance = pct(af, ac),
                mean_individual_mass = pct(mf, mc))
    colnames(P) <- as.character(YRS)
    C_ts[[paste(g, nm)]] <- cbind(ltl_group = g, metric = nm,
                                  summ_mat(P, "year", "pct_change_")) }
}
C_ts <- bind_rows(C_ts) %>% mutate(year = as.numeric(year)) %>%
  filter(year >= 1900) %>% arrange(ltl_group, metric, year)
wcsv(C_ts, "C_ltl_pctchange_timeseries_1900_2010.csv")

C_per <- list()
for (g in LTL) for (pn in names(PERIODS)) {
  ii <- YRS %in% PERIODS[[pn]]
  gm <- function(a, f) rowMeans(t(vapply(Z, function(z) z$arms[[a]][[f]][ii, g],
                                         numeric(sum(ii)))))
  bf <- gm("exploited", "bio"); bc <- gm("unexploited", "bio")
  af <- gm("exploited", "abu"); ac <- gm("unexploited", "abu")
  C_per[[paste(g, pn)]] <- data.frame(ltl_group = g, period = pn, metric =
      c("biomass", "abundance", "mean_individual_mass"),
    rbind(qs(pct(bf, bc)), qs(pct(af, ac)), qs(pct(bf / af, bc / ac))),
    exploited_median = c(median(bf) / G, median(af), median(bf / af)),
    unexploited_median = c(median(bc) / G, median(ac), median(bc / ac)),
    unit = c("t", "individuals", "g"))
}
C_per <- bind_rows(C_per)
names(C_per)[4:9] <- paste0("pct_change_", c("median", "q25", "q75", "p05", "p95", "n_members"))
wcsv(C_per, "C_ltl_pctchange_period_summary.csv")

# background resource forcing
npp_rel <- vapply(Z, function(z) z$npp_max_rel, numeric(1))
w_full <- WG@w_full; dw_full <- WG@dw_full
C_res <- bind_rows(lapply(names(SPEC_PERIODS), function(pn) {
  M <- t(vapply(Z, function(z) z$npp_mean[[pn]], numeric(length(w_full))))
  data.frame(period = pn, bin_index = seq_along(w_full),
             w_g = w_full, dw_g = dw_full,
             n_pp_mean_across_members = colMeans(M),
             n_pp_max_rel_spread_across_members =
               apply(M, 2, function(v) diff(range(v)) / pmax(1e-300, mean(v))),
             resource_biomass_t = colMeans(M) * w_full * dw_full / G,
             in_resolved_species_range = w_full >= min(Wv)) }))
wcsv(C_res, "C_resource_spectrum_check.csv")

# =============================================================================
# D -- krill mortality decomposition
# =============================================================================
cat("--- D: krill mortality decomposition ---\n")
D_rows <- list()
for (pn in names(PERIODS)) for (a in c("exploited", "unexploited")) {
  KM <- t(vapply(Z, function(z) z$arms[[a]]$kmort[[pn]]$kill_g_per_yr, numeric(length(SPN))))
  colnames(KM) <- SPN
  KB <- vapply(Z, function(z) z$arms[[a]]$kmort[[pn]]$krill_biomass_g, numeric(1))
  FI <- vapply(Z, function(z) z$arms[[a]]$kmort[[pn]]$fishing_g_per_yr, numeric(1))
  BG <- vapply(Z, function(z) z$arms[[a]]$kmort[[pn]]$background_g_per_yr, numeric(1))
  X <- cbind(KM, "FISHING" = FI, "BACKGROUND" = BG,
             "TOTAL predation" = rowSums(KM),
             "TOTAL mortality" = rowSums(KM) + FI + BG)
  D_rows[[paste(pn, a)]] <- cbind(
    period = pn, arm = a,
    summ_mat(X / G, "mortality_source", "removal_t_per_yr_"),
    summ_mat(X / KB, "mortality_source", "mort_rate_per_yr_")[, -1],
    krill_biomass_t_median = median(KB) / G)
}
D_dec <- bind_rows(D_rows) %>% arrange(period, arm, desc(removal_t_per_yr_median))
wcsv(D_dec, "D_krill_mortality_decomposition.csv")

D_pair <- list()
for (pn in names(PERIODS)) {
  get <- function(a) {
    KM <- t(vapply(Z, function(z) z$arms[[a]]$kmort[[pn]]$kill_g_per_yr, numeric(length(SPN))))
    colnames(KM) <- SPN
    KB <- vapply(Z, function(z) z$arms[[a]]$kmort[[pn]]$krill_biomass_g, numeric(1))
    FI <- vapply(Z, function(z) z$arms[[a]]$kmort[[pn]]$fishing_g_per_yr, numeric(1))
    BG <- vapply(Z, function(z) z$arms[[a]]$kmort[[pn]]$background_g_per_yr, numeric(1))
    list(X = cbind(KM, FISHING = FI, BACKGROUND = BG,
                   `TOTAL predation` = rowSums(KM),
                   `TOTAL mortality` = rowSums(KM) + FI + BG), KB = KB) }
  f <- get("exploited"); c_ <- get("unexploited")
  D_pair[[pn]] <- cbind(period = pn,
    summ_mat((f$X - c_$X) / G, "mortality_source", "delta_removal_t_per_yr_"),
    summ_mat(f$X / f$KB - c_$X / c_$KB, "mortality_source", "delta_rate_per_yr_")[, -1],
    delta_krill_biomass_pct_median = median(100 * (f$KB - c_$KB) / c_$KB))
}
D_pair <- bind_rows(D_pair) %>% arrange(period, desc(abs(delta_removal_t_per_yr_median)))
wcsv(D_pair, "D_krill_mortality_paired_delta.csv")

# =============================================================================
# E -- compensation biomass budget
# =============================================================================
cat("--- E: compensation biomass budget ---\n")
THR <- 1e6                                    # 1 tonne, in grams
# The straddling bin is split linearly, transcribed from
# R/wmin_test/06_stage0_robustness.R:36. An unsplit variant is reported alongside.
frac_below <- pmin(1, pmax(0, (THR - Wv) / DWv)); frac_above <- 1 - frac_below
frac_below_hard <- as.numeric(Wv < THR);        frac_above_hard <- 1 - frac_below_hard

E_out <- list(); E_bud <- list()
for (pn in c("1960-1970", "2001-2010")) {
  bm <- function(a, fr) t(vapply(Z, function(z)
    as.numeric((z$arms[[a]]$spec[[pn]] * rep(Wv * DWv * fr, each = length(SPN)))
               %*% rep(1, length(Wv))), numeric(length(SPN))))
  for (v in c("split", "hard")) {
    fa <- if (v == "split") frac_above else frac_above_hard
    fb <- if (v == "split") frac_below else frac_below_hard
    Af <- bm("exploited", fa); Ac <- bm("unexploited", fa)
    Bf <- bm("exploited", fb); Bc <- bm("unexploited", fb)
    colnames(Af) <- colnames(Ac) <- colnames(Bf) <- colnames(Bc) <- SPN
    E_out[[paste(pn, v)]] <- bind_rows(
      cbind(period = pn, boundary_bin = v, mass_class = "above 1 t",
            summ_mat((Af - Ac) / G, "species", "delta_biomass_t_"),
            exploited_t_median = apply(Af, 2, median) / G,
            unexploited_t_median = apply(Ac, 2, median) / G),
      cbind(period = pn, boundary_bin = v, mass_class = "below 1 t",
            summ_mat((Bf - Bc) / G, "species", "delta_biomass_t_"),
            exploited_t_median = apply(Bf, 2, median) / G,
            unexploited_t_median = apply(Bc, 2, median) / G))
    # budget: formed within member, then summarised
    la <- rowSums(Af - Ac) / G; gb <- rowSums(Bf - Bc) / G
    E_bud[[paste(pn, v)]] <- data.frame(period = pn, boundary_bin = v,
      quantity = c("delta biomass above 1 t", "delta biomass below 1 t",
                   "residual (above + below)", "offset fraction (gain below / |loss above|)"),
      rbind(qs(la), qs(gb), qs(la + gb), qs(gb / pmax(1e-300, -la))),
      unit = c("t", "t", "t", "fraction"))
  }
}
E_sp <- bind_rows(E_out) %>% arrange(period, boundary_bin, mass_class, delta_biomass_t_median)
E_budget <- bind_rows(E_bud)
names(E_budget)[4:9] <- paste0(c("median", "q25", "q75", "p05", "p95", "n_members"))
wcsv(E_sp %>% filter(period == "2001-2010"), "E_compensation_budget_2001_2010.csv")
wcsv(E_sp %>% filter(period == "1960-1970"), "E_compensation_budget_1960_1970.csv")
wcsv(E_budget, "E_compensation_budget_summary.csv")

# =============================================================================
# F -- feeding kernel parameters
# =============================================================================
cat("--- F: feeding kernel parameters ---\n")
KERN <- do.call(rbind, lapply(Z, function(z) z$kern))
kf <- c("pred_kernel_type", "beta", "sigma", "ppmr_min", "ppmr_max",
        "w_min", "w_mat", "w_max")
kconst <- vapply(kf, function(f) {
  s <- split(KERN[[f]], KERN$species)
  all(vapply(s, function(v) length(unique(v[!is.na(v)])) <= 1 &&
               all(is.na(v)) == any(is.na(v)), logical(1))) }, logical(1))
if (!all(kconst))
  stop("kernel parameters vary across members: ", paste(kf[!kconst], collapse = ", "))
K1 <- KERN[KERN$sim_index == KERN$sim_index[1], ]
K1 <- K1[match(SPN, K1$species), ]
kmax <- K1$w_max[K1$species == KRILL]

F_tab <- K1 %>% transmute(
  species, operative_kernel = pred_kernel_type,
  w_min_g = w_min, w_mat_g = w_mat, w_max_g = w_max,
  beta_stored = beta, sigma_stored = sigma,
  beta_is_used = pred_kernel_type == "lognormal",
  ppmr_min = ppmr_min, ppmr_max = ppmr_max,
  # lognormal geometry: peak at w/beta, +-1 sigma at w/beta * exp(-+sigma)
  lognormal_pref_prey_at_w_max_g   = w_max / beta,
  lognormal_pref_prey_at_w_mat_g   = w_mat / beta,
  lognormal_prey_minus1sd_at_w_max_g = w_max / beta * exp(-sigma),
  lognormal_prey_plus1sd_at_w_max_g  = w_max / beta * exp(sigma),
  # box geometry: hard window [w/ppmr_max, w/ppmr_min]
  box_prey_lo_at_w_max_g = w_max / ppmr_max,
  box_prey_hi_at_w_max_g = w_max / ppmr_min,
  box_prey_lo_at_w_min_g = w_min / ppmr_max,
  box_prey_hi_at_w_min_g = w_min / ppmr_min,
  # how far Antarctic krill sits from the preferred prey mass, in kernel sigmas
  krill_sd_from_pref_at_w_max = log((w_max / beta) / kmax) / sigma,
  max_predator_mass_that_can_eat_krill_g = ifelse(
    pred_kernel_type == "box", kmax * ppmr_max, NA_real_))
wcsv(F_tab, "F_feeding_kernel_parameters.csv")

# =============================================================================
# G / H -- headline ratios, at four ranking thresholds
# =============================================================================
cat("--- G/H: headline ratios and threshold sensitivity ---\n")
head_stats <- function(n) {
  ids <- prefix_of(n); ix <- match(as.character(ids), dimnames(MA1$krill_cons_exploited)$sim)
  rows <- list()
  for (g in list(c("Large baleen + minke whales", "W"), c("All predators", "A"),
                 c("All fishes", "F"))) {
    ss <- switch(g[2], W = WHALES, A = SPN, F = FISHES)
    e10 <- rowSums(MA1$krill_cons_exploited[ix, "2010", ss, drop = FALSE])
    u10 <- rowSums(MA1$krill_cons_unexploited[ix, "2010", ss, drop = FALSE])
    ep  <- rowMeans(apply(MA1$krill_cons_exploited[ix, , ss, drop = FALSE], c(1, 2), sum))
    up  <- rowMeans(apply(MA1$krill_cons_unexploited[ix, , ss, drop = FALSE], c(1, 2), sum))
    rows[[length(rows) + 1]] <- data.frame(
      subset = paste0("n=", n), quantity = c(
        sprintf("krill consumption ratio, %s, 2010", g[1]),
        sprintf("krill consumption ratio, %s, 2001-2010 mean", g[1]),
        sprintf("implied fold increase to close the gap, %s, 2001-2010", g[1]),
        sprintf("absolute deficit (unexploited - exploited), %s, 2001-2010", g[1])),
      rbind(qs(e10 / u10), qs(ep / up), qs(up / ep), qs((up - ep) / G)),
      unit = c("ratio", "ratio", "factor", "t/yr"))
  }
  for (s in c("baleen whales", "sperm whales", "minke whales", "orca")) {
    ae <- rowMeans(MA1$abundance_exploited[ix, , s]);
    au <- rowMeans(MA1$abundance_unexploited[ix, , s])
    a10 <- MA1$abundance_exploited[ix, "2010", s] / MA1$abundance_unexploited[ix, "2010", s]
    rows[[length(rows) + 1]] <- data.frame(subset = paste0("n=", n), quantity = c(
      sprintf("abundance ratio, %s, 2001-2010 mean", s),
      sprintf("abundance ratio, %s, 2010", s)),
      rbind(qs(ae / au), qs(a10)), unit = "ratio")
  }
  bind_rows(rows)
}
H_all <- bind_rows(lapply(MA1$thresholds, head_stats))
names(H_all)[3:8] <- c("median", "q25", "q75", "p05", "p95", "n_members")
G_tab <- H_all %>% filter(subset == "n=167")
wcsv(G_tab, "G_headline_ratios_n167.csv")
wcsv(H_all, "H_threshold_sensitivity.csv")
saveRDS(list(validation = VAL, A_krill = A_krill, A_ltl = A_ltl, B_dec = B_dec,
             C_per = C_per, D = D_dec, E = E_budget, F = F_tab, H = H_all),
        guard(file.path(OUTDIR, "MA02_all_tables.rds")))
cat("\nall CSVs written to", OUTDIR, "\n")
source("Manuscript scripts/MA02_summary.R", local = TRUE)
