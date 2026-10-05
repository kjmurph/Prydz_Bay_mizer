# A0_helpers.R -- shared comparison helpers for the replay scripts (R1-R7, W5, V)
#
# Every replay compares a rebuilt object with the stored one and prints one
# PASS/FAIL line per check. Run the scripts from the repository root. They
# read the repository and write only to ASSESS_OUT (default
# Output_large_files/model_construction, which git ignores).

REPO <- Sys.getenv("ASSESS_REPO", ".")
OUT  <- Sys.getenv("ASSESS_OUT", "Output_large_files/model_construction")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
HIST <- file.path(OUT, "history")

repo_path <- function(...) file.path(REPO, ...)

# max relative difference over finite, paired values; Inf where one side is
# non-finite and the other is not
max_rel <- function(x, y) {
  x <- as.numeric(x); y <- as.numeric(y)
  stopifnot(length(x) == length(y))
  both_inf <- !is.finite(x) & !is.finite(y) & (x == y | (is.na(x) & is.na(y)))
  both_inf[is.na(both_inf)] <- FALSE
  one_bad <- xor(is.finite(x), is.finite(y)) & !both_inf
  if (any(one_bad, na.rm = TRUE)) return(Inf)
  ok <- is.finite(x) & is.finite(y)
  if (!any(ok)) return(0)
  den <- pmax(abs(x[ok]), abs(y[ok]))
  r <- ifelse(den == 0, 0, abs(x[ok] - y[ok]) / den)
  max(r)
}

results <- list()
check <- function(id, what, pass, detail = "") {
  line <- sprintf("%-4s %-4s %s%s", id, if (isTRUE(pass)) "PASS" else "FAIL",
                  what, if (nzchar(detail)) paste0("  [", detail, "]") else "")
  cat(line, "\n")
  results[[length(results) + 1]] <<- data.frame(
    replay = id, result = if (isTRUE(pass)) "PASS" else "FAIL", check = what,
    detail = detail, stringsAsFactors = FALSE)
  invisible(pass)
}
note <- function(id, what, detail = "") {
  cat(sprintf("%-4s NOTE %s%s\n", id, what,
              if (nzchar(detail)) paste0("  [", detail, "]") else ""))
  results[[length(results) + 1]] <<- data.frame(
    replay = id, result = "NOTE", check = what, detail = detail,
    stringsAsFactors = FALSE)
}
save_results <- function(id) {
  if (length(results)) {
    write.csv(do.call(rbind, results),
              file.path(OUT, paste0(id, "_results.csv")), row.names = FALSE)
  }
}
fmt <- function(x) formatC(x, format = "g", digits = 3)
