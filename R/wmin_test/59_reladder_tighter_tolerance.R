# =============================================================================
# Phase 59 -- re-ladder the capped reference at tighter steady() tolerance
#
# Phase 58 capped reproduction level at 0.9 and then re-laddered with erepro
# free at a target tolerance of 0.005. The cap itself was a free, strict
# improvement (biomass deviation held at 0.00820, max erepro 0.9509 -> 0.7328,
# three groups came off level > 0.99), but the RE-LADDER was a regression: it
# landed at 0.00966, never beat the capped object at its target rung, and let
# two groups drift back over the cap.
#
# THE QUESTION HERE. Kieran wants the re-laddered object rather than the
# cap-only one, but at a tighter tolerance -- 0.002, and 0.001 if that passes.
# A pass at 0.002 is acceptable.
#
# A PROPER GRADED LADDER, NOT A SUBSTITUTED RUNG. Phase 58 built its ladder as
# c(0.1, 0.05, 0.01, TARGET), so asking for 0.002 there would have SKIPPED the
# 0.005 rung and jumped an extra factor straight from 0.01. Here the full rung
# set is fixed and truncated at the target, so 0.002 runs
# 0.1 -> 0.05 -> 0.01 -> 0.005 -> 0.002 and 0.001 adds one more.
#
# WHAT "PASS" MEANS, stated before the run: at the TARGET rung, at least one
# round must (a) converge and (b) leave every erepro < 1. Among those the round
# with the lowest max biomass deviation is selected. Convergence alone is not a
# pass -- an inadmissible erepro is what sank phase 57.
#
# EXPECT THE FIT TO GET WORSE, NOT BETTER. Tightening steady()'s tolerance makes
# the biomass fit worse in this pipeline, which is why phases 54-58 all keep the
# BEST converged state rather than the last and why the per-round deviation
# oscillates. A tighter tolerance is a stronger statement that the state really
# is a steady state; it is not a better calibration. Both numbers are reported
# against the cap-only baseline of 0.00820 so the trade is visible.
#
# USAGE  Rscript R/wmin_test/59_reladder_tighter_tolerance.R
# ENV    P59_IN, P59_TOLS (0.002,0.001), P59_PRESERVE, P59_CAP (0.9)
# =============================================================================

suppressPackageStartupMessages({library(mizer); library(therMizer)})

IN <- Sys.getenv("P59_IN", "params_ref_p58_cap09.rds")
TOLS <- as.numeric(trimws(strsplit(Sys.getenv("P59_TOLS", "0.002,0.001"),
                                   ",")[[1]]))
PRES <- Sys.getenv("P59_PRESERVE", "reproduction_level")
CAP <- as.numeric(Sys.getenv("P59_CAP", "0.9"))
RUNGS <- c(0.1, 0.05, 0.01, 0.005, 0.002, 0.001)
STEADY_TMAX <- as.numeric(Sys.getenv("P59_TMAX", "1000"))
MATCH_ROUNDS <- 6; TARGET_ROUNDS <- 14
out_dir <- file.path("Output_large_files", "wmin_test")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
tag <- function(x) file.path(out_dir, paste0("59_", x))

t0 <- proc.time()
cat("=== Phase 59: re-ladder the capped reference at tighter tolerance ===\n")
cat("input:", IN, "| targets:", paste(TOLS, collapse = ", "),
    "| preserve:", PRES, "\n\n")
if (!file.exists(IN))
  stop("missing ", IN, " -- run 58_cap_repro_then_free_erepro.R first",
       call. = FALSE)

P0 <- suppressWarnings(validParams(readRDS(IN)))
SPN <- P0@species_params$species
bio_ratio <- function(p)
  as.numeric(getBiomass(p, use_cutoff = TRUE) / p@species_params$biomass_observed)
max_dev <- function(p) max(abs(bio_ratio(p) - 1))
rl_of <- function(p) as.numeric(getReproductionLevel(p))
base_dev <- max_dev(P0); base_er <- max(P0@species_params$erepro)
cat(sprintf("cap-only baseline: max dev %.5f | max erepro %.4f | max level %.4f\n\n",
            base_dev, base_er, max(rl_of(P0))))

steady_guarded <- function(p, tol) {
  nc <- FALSE
  out <- withCallingHandlers(
    try(steady(p, tol = tol, t_max = STEADY_TMAX, preserve = PRES,
               progress_bar = FALSE), silent = TRUE),
    message = function(m) {
      if (grepl("did not converge", conditionMessage(m), ignore.case = TRUE))
        nc <<- TRUE
      invokeRestart("muffleMessage")
    }, warning = function(w) invokeRestart("muffleWarning"))
  list(params = out, converged = !nc, errored = inherits(out, "try-error"))
}
match_guarded <- function(p) {
  out <- withCallingHandlers(try(matchBiomasses(p), silent = TRUE),
    warning = function(w) invokeRestart("muffleWarning"))
  list(params = out, errored = inherits(out, "try-error"))
}

run_ladder <- function(target) {
  ladder <- RUNGS[RUNGS >= target]
  cat("\n########  target tolerance", target, " ########\n")
  cat("  rungs:", paste(ladder, collapse = " -> "), "\n")
  p <- P0
  st <- steady_guarded(p, ladder[1])
  if (st$errored) return(list(pass = FALSE, reason = "steady_error_leading"))
  p <- st$params
  best <- NULL; best_d <- Inf; tr <- list()
  n_conv_at_target <- 0; n_adm_at_target <- 0
  for (tol in ladder) {
    at <- isTRUE(all.equal(tol, target))
    for (r in seq_len(if (at) TARGET_ROUNDS else MATCH_ROUNDS)) {
      mt <- match_guarded(p)
      if (mt$errored) return(list(pass = FALSE, reason = "match_error"))
      p <- mt$params
      st <- steady_guarded(p, tol)
      if (st$errored) return(list(pass = FALSE, reason = "steady_error"))
      p <- st$params
      d <- max_dev(p); er <- p@species_params$erepro
      me <- max(er); nbad <- sum(er >= 1); rl <- rl_of(p)
      adm <- me < 1
      if (at) {
        n_conv_at_target <- n_conv_at_target + st$converged
        n_adm_at_target <- n_adm_at_target + (st$converged && adm)
        if (st$converged && adm && d < best_d) { best_d <- d; best <- p }
      }
      tr[[length(tr) + 1]] <- data.frame(
        target = target, tol = tol, round = r, max_dev = d, max_erepro = me,
        n_erepro_ge1 = nbad, max_repro_level = max(rl),
        n_above_cap = sum(rl > CAP + 1e-9), converged = st$converged)
      cat(sprintf("   tol=%-6.3g r=%-2d dev %.5f  max erepro %.4f (%d>=1)  max rl %.4f (%d>cap)  %s\n",
                  tol, r, d, me, nbad, max(rl), sum(rl > CAP + 1e-9),
                  if (st$converged) "conv" else "NO-CONV"))
      flush.console()
    }
  }
  list(pass = !is.null(best), params = best, max_dev = best_d,
       trace = do.call(rbind, tr), n_conv = n_conv_at_target,
       n_adm = n_adm_at_target,
       reason = if (is.null(best)) "no converged+admissible round at target" else NA)
}

RES <- list(); TRACE <- list(); KEEP <- list()
for (tg in TOLS) {
  r <- run_ladder(tg)
  TRACE[[length(TRACE) + 1]] <- r$trace
  ok <- isTRUE(r$pass)
  row <- data.frame(target = tg, pass = ok,
                    reason = if (ok) "ok" else r$reason,
                    max_dev = if (ok) r$max_dev else NA_real_,
                    max_erepro = if (ok) max(r$params@species_params$erepro) else NA_real_,
                    n_above_cap = if (ok) sum(rl_of(r$params) > CAP + 1e-9) else NA_integer_,
                    rounds_converged = if (is.null(r$n_conv)) NA else r$n_conv,
                    rounds_admissible = if (is.null(r$n_adm)) NA else r$n_adm,
                    stringsAsFactors = FALSE)
  RES[[length(RES) + 1]] <- row
  cat(sprintf("\n  -> target %g: %s", tg, if (ok) "PASS" else "FAIL"))
  if (ok) {
    f <- sprintf("params_ref_p59_cap09_tol%s.rds",
                 sub("^0\\.", "", format(tg, scientific = FALSE)))
    saveRDS(r$params, f); KEEP[[as.character(tg)]] <- r$params
    cat(sprintf("  (dev %.5f, max erepro %.4f) -> %s\n", r$max_dev,
                max(r$params@species_params$erepro), f))
  } else {
    cat("  --", r$reason, "\n")
    # a tighter target cannot succeed where a looser one failed
    if (tg == min(TOLS[TOLS >= tg])) {
      rem <- TOLS[TOLS < tg]
      if (length(rem)) {
        cat("  skipping the tighter target(s)", paste(rem, collapse = ", "),
            "-- they can only be harder\n")
        for (s in rem) RES[[length(RES) + 1]] <- data.frame(
          target = s, pass = FALSE, reason = "skipped after a looser target failed",
          max_dev = NA_real_, max_erepro = NA_real_, n_above_cap = NA_integer_,
          rounds_converged = NA_integer_, rounds_admissible = NA_integer_,
          stringsAsFactors = FALSE)
      }
      break
    }
  }
}

SUM <- do.call(rbind, RES)
cat("\n=== phase 59 summary ===\n")
print(SUM, row.names = FALSE, digits = 4)
cat(sprintf("\ncap-only baseline for comparison: max dev %.5f, max erepro %.4f\n",
            base_dev, base_er))
cat("phase 58 re-ladder at 0.005:      max dev 0.00966, max erepro 0.7290\n")

if (length(KEEP)) {
  arms <- c(list(`cap only` = P0), KEEP)
  names(arms)[-1] <- paste0("re-laddered tol ", names(KEEP))
  CMP <- do.call(rbind, lapply(names(arms), function(nm) {
    q <- arms[[nm]]
    data.frame(arm = nm, max_dev = max_dev(q),
               max_erepro = max(q@species_params$erepro),
               n_erepro_ge1 = sum(q@species_params$erepro >= 1),
               median_repro_level = median(rl_of(q)),
               max_repro_level = max(rl_of(q)),
               n_above_cap = sum(rl_of(q) > CAP + 1e-9))
  }))
  cat("\n=== comparison ===\n"); print(CMP, row.names = FALSE, digits = 4)
  write.csv(CMP, tag("comparison.csv"), row.names = FALSE)
  PER <- do.call(rbind, lapply(names(arms), function(nm) {
    q <- arms[[nm]]
    data.frame(arm = nm, species = SPN, erepro = q@species_params$erepro,
               repro_level = rl_of(q), bio_ratio = bio_ratio(q))
  }))
  wide <- function(v) {
    x <- reshape(PER[, c("arm", "species", v)], idvar = "species",
                 timevar = "arm", direction = "wide")
    names(x) <- sub(paste0(v, "\\."), "", names(x)); x[match(SPN, x$species), ]
  }
  cat("\n=== reproduction level ===\n")
  print(wide("repro_level"), row.names = FALSE, digits = 4)
  cat("\n=== erepro ===\n")
  print(wide("erepro"), row.names = FALSE, digits = 4)
  write.csv(PER, tag("per_species.csv"), row.names = FALSE)
}
write.csv(do.call(rbind, TRACE), tag("ladder_trace.csv"), row.names = FALSE)
write.csv(SUM, tag("summary.csv"), row.names = FALSE)
cat("\nelapsed", round((proc.time() - t0)[["elapsed"]] / 60, 1), "min\n")
cat("Phase 59 complete.\n")