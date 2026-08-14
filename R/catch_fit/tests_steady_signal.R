# =============================================================================
# How does mizer 3.1.0's steady() signal non-convergence?
#
# This matters beyond this analysis.  Both 09_Uncertainty_Analysis.Rmd:431 and
# R/wmin_test/39_full_ensemble_paired.R:154 detect non-convergence with a
# WARNING handler:
#
#     tryCatch(steady(...), warning = function(w)
#       if (grepl("did not converge", w$message)) <reject>)
#
# If steady() emits a message() rather than a warning(), that handler never
# fires and every non-converged draw is silently ACCEPTED -- which would mean the
# "non-convergence is an outright rejection" fidelity rule is not actually being
# enforced under the installed mizer.
#
#   Rscript R/catch_fit/tests_steady_signal.R
# =============================================================================

suppressPackageStartupMessages({ library(mizer) })

b <- deparse(body(mizer:::steady.MizerParams))
hits <- grep("converge", b, value = TRUE)
cat("=== lines in steady.MizerParams mentioning 'converge' ===\n")
for (h in hits) cat("  ", trimws(h), "\n", sep = "")

cat("\n=== which condition class carries it? ===\n")
p <- suppressMessages(suppressWarnings(
  newMultispeciesParams(NS_species_params_gears, no_w = 30)))

got <- list(message = FALSE, warning = FALSE)
res <- withCallingHandlers(
  suppressWarnings(try(steady(p, t_max = 1, tol = 1e-12, progress_bar = FALSE),
                       silent = TRUE)),
  message = function(m) {
    if (grepl("did not converge", conditionMessage(m), ignore.case = TRUE))
      got$message <<- TRUE
    invokeRestart("muffleMessage")
  },
  warning = function(w) {
    if (grepl("did not converge", conditionMessage(w), ignore.case = TRUE))
      got$warning <<- TRUE
    invokeRestart("muffleWarning")
  })

cat("  signalled as message():", got$message, "\n")
cat("  signalled as warning():", got$warning, "\n\n")

if (got$message && !got$warning) {
  cat("CONFIRMED: steady() uses message().  A `warning =` handler cannot see it.\n")
  cat("Consequence: the non-convergence rejection in 09_Uncertainty_Analysis.Rmd\n")
  cat("and in 39_full_ensemble_paired.R does not fire under mizer 3.1.0, so\n")
  cat("non-converged draws pass through as if they had converged.\n")
} else if (got$warning) {
  cat("steady() warns; the existing handlers are correct.\n")
} else {
  cat("INCONCLUSIVE: this toy model converged, so nothing was signalled.\n")
  cat("Re-run against a real member if a definitive answer is needed.\n")
}
