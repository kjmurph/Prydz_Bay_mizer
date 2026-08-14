# =============================================================================
# Guards against silent size-parameter rewriting by mizer.
#
# THE BUG THIS PREVENTS. `mizer:::validGivenSpeciesParams` rewrites w_min to
# pmin(0.001, w_mat/10) whenever w_min >= w_mat, and only warns:
#
#   wrong <- !is.na(sp$w_min) & !is.na(sp$w_mat) & sp$w_min >= sp$w_mat
#   sp$w_min[wrong] <- pmin(0.001, sp$w_mat[wrong]/10)
#   warning("... `w_min` is not smaller than that of `w_mat`. I have reduced the values.")
#
# In this project that warning fired for `small divers` at
# newMultispeciesParams() (optim_model_setup_old/model_setup_v4.Rmd:378), was
# missed, and w_min ran at 0.001 g -- four orders of magnitude below its
# empirical value -- through the entire calibration, the 2,111-member Monte Carlo
# and the 212-member fitted ensemble. A warning is too easy to lose in Rmd
# output. These are errors.
#
# USAGE
#   source("R/check_size_params.R")
#   assert_size_params(groups)                     # BEFORE newMultispeciesParams
#   params <- newMultispeciesParams(groups, ...)
#   assert_no_clamp(params, groups)                # AFTER
#   check_size_params(params)                      # audit any existing object
# =============================================================================

MIZER_CLAMP_VALUE <- 0.001

#' Replicate mizer's two size-parameter rewrites IN ORDER, including the cascade.
#'
#' `validGivenSpeciesParams` applies them sequentially, and the first mutates the
#' value the second tests against:
#'
#'   1. `w_mat >= w_inf`  ->  `w_mat <- w_inf/4`
#'   2. `w_min >= w_mat`  ->  `w_min <- pmin(0.001, w_mat/10)`   (using the NEW w_mat)
#'
#' Checking them independently misses the cascade. In this project's own trait
#' table, `leopard seals` passes rule 2 on its authored values (w_min 200000 <
#' w_mat 348000) but FAILS it after rule 1 rewrites w_mat to 87000 — so it would
#' be clamped to 0.001 g even though nothing about its w_min is wrong. It escaped
#' in the real build only because an in-script override raised its `w_max` to
#' 450000 before the params were constructed.
#'
#' @return list(w_mat, w_min, mat_clamped, min_clamped) after both rules.
simulate_mizer_clamps <- function(w_min, w_mat, w_inf) {
  mat_bad <- !is.na(w_mat) & !is.na(w_inf) & w_mat >= w_inf
  w_mat2 <- w_mat
  w_mat2[mat_bad] <- w_inf[mat_bad] / 4
  min_bad <- !is.na(w_min) & !is.na(w_mat2) & w_min >= w_mat2
  w_min2 <- w_min
  w_min2[min_bad] <- pmin(MIZER_CLAMP_VALUE, w_mat2[min_bad] / 10)
  list(w_mat = w_mat2, w_min = w_min2,
       mat_clamped = mat_bad, min_clamped = min_bad)
}

#' Validate a species_params table before it is handed to mizer.
#'
#' @param sp data.frame with columns species, w_min, w_mat and w_max (or w_inf).
#' @param strict if TRUE (default) an ordering violation is an error. Set FALSE
#'   to downgrade to a message when you are deliberately inspecting a bad table.
#' @return `sp`, invisibly, so the call can be piped or wrapped around an argument.
assert_size_params <- function(sp, strict = TRUE) {
  stopifnot(is.data.frame(sp))
  if (!"species" %in% names(sp)) stop("assert_size_params(): no `species` column")
  wmax_col <- if ("w_max" %in% names(sp)) "w_max" else
    if ("w_inf" %in% names(sp)) "w_inf" else NA_character_
  if (is.na(wmax_col)) stop("assert_size_params(): no `w_max` or `w_inf` column")
  if (!all(c("w_min", "w_mat") %in% names(sp)))
    stop("assert_size_params(): need both `w_min` and `w_mat`")

  w_min <- sp$w_min; w_mat <- sp$w_mat; w_max <- sp[[wmax_col]]

  # Apply mizer's rules IN ORDER so the cascade is caught: a w_mat violation
  # rewrites w_mat, which can then trip the w_min test on a group whose authored
  # w_min was perfectly valid. Testing the two rules independently misses this.
  cl <- simulate_mizer_clamps(w_min, w_mat, w_max)
  bad_mat <- which(cl$mat_clamped)
  bad_min <- which(cl$min_clamped)
  bad_min_direct <- which(!is.na(w_min) & !is.na(w_mat) & w_min >= w_mat)
  bad_min_cascade <- setdiff(bad_min, bad_min_direct)
  bad_na  <- which(is.na(w_min) | is.na(w_mat) | is.na(w_max))
  bad_pos <- which(!is.na(w_min) & w_min <= 0)

  msgs <- character(0)
  if (length(bad_mat))
    msgs <- c(msgs, paste0(
      "w_mat >= w_inf for: ",
      paste(sprintf("%s (w_mat=%g -> mizer would rewrite to %g, i.e. 25%% of w_inf)",
                    sp$species[bad_mat], w_mat[bad_mat], cl$w_mat[bad_mat]),
            collapse = "; ")))
  if (length(bad_min_direct))
    msgs <- c(msgs, paste0(
      "w_min >= w_mat for: ",
      paste(sprintf("%s (w_min=%g, w_mat=%g -> mizer would clamp to %g)",
                    sp$species[bad_min_direct], w_min[bad_min_direct],
                    w_mat[bad_min_direct], cl$w_min[bad_min_direct]),
            collapse = "; ")))
  if (length(bad_min_cascade))
    msgs <- c(msgs, paste0(
      "w_min clamped BY CASCADE (authored w_min is valid; the w_mat rewrite ",
      "above causes it) for: ",
      paste(sprintf("%s (w_min=%g valid against authored w_mat=%g, but %g >= rewritten w_mat=%g -> clamped to %g)",
                    sp$species[bad_min_cascade], w_min[bad_min_cascade],
                    w_mat[bad_min_cascade], w_min[bad_min_cascade],
                    cl$w_mat[bad_min_cascade], cl$w_min[bad_min_cascade]),
            collapse = "; ")))
  if (length(bad_na))
    msgs <- c(msgs, paste0("missing size parameter for: ",
                           paste(sp$species[bad_na], collapse = ", ")))
  if (length(bad_pos))
    msgs <- c(msgs, paste0("non-positive w_min for: ",
                           paste(sp$species[bad_pos], collapse = ", ")))

  if (length(msgs)) {
    txt <- paste0("Size parameters are invalid and mizer will silently rewrite ",
                  "them:\n  - ", paste(msgs, collapse = "\n  - "))
    if (strict) stop(txt, call. = FALSE) else message(txt)
  } else {
    message("assert_size_params(): OK -- w_min < w_mat < w_max for all ",
            nrow(sp), " groups")
  }
  invisible(sp)
}

#' Confirm mizer did not rewrite w_min while building a MizerParams.
#'
#' @param params the object returned by newMultispeciesParams()/setParams().
#' @param sp_input the species_params data.frame that was passed in.
#' @param tol relative tolerance; mizer legitimately re-derives some values at
#'   ~1e-8 relative, so only larger discrepancies are treated as rewrites.
assert_no_clamp <- function(params, sp_input, tol = 1e-6) {
  got <- params@species_params
  idx <- match(sp_input$species, got$species)
  if (anyNA(idx))
    stop("assert_no_clamp(): species missing after build: ",
         paste(sp_input$species[is.na(idx)], collapse = ", "), call. = FALSE)

  before <- sp_input$w_min
  after  <- got$w_min[idx]
  rel <- ifelse(before > 0, abs(after - before) / before, NA_real_)
  moved <- which(!is.na(rel) & rel > tol)

  if (length(moved))
    stop("mizer rewrote w_min for ", length(moved), " group(s):\n  - ",
         paste(sprintf("%s: %g -> %g%s", sp_input$species[moved],
                       before[moved], after[moved],
                       ifelse(abs(after[moved] - MIZER_CLAMP_VALUE) < 1e-12,
                              "  [this is mizer's 0.001 fallback]", "")),
               collapse = "\n  - "),
         "\nFix the input table; do not accept the rewritten value.",
         call. = FALSE)

  message("assert_no_clamp(): OK -- all ", length(idx),
          " w_min values survived the build")
  invisible(params)
}

#' Audit an existing MizerParams object. Returns a data.frame, prints a summary.
check_size_params <- function(params, quiet = FALSE) {
  sp <- params@species_params
  w <- params@w
  out <- data.frame(
    species = sp$species,
    w_min = sp$w_min, w_mat = sp$w_mat, w_max = sp$w_max,
    ordering_ok = sp$w_min < sp$w_mat & sp$w_mat < sp$w_max,
    at_mizer_clamp = abs(sp$w_min - MIZER_CLAMP_VALUE) < 1e-12,
    span_decades = log10(sp$w_max / sp$w_min),
    stringsAsFactors = FALSE)
  out$bins <- vapply(seq_len(nrow(sp)), function(i)
    as.integer(sum(w >= w[params@w_min_idx[[i]]] & w <= sp$w_max[i])), integer(1))
  if ("erepro" %in% names(sp)) {
    out$erepro <- sp$erepro
    out$erepro_ok <- sp$erepro < 1
  }
  if ("R_max" %in% names(sp)) {
    out$R_max <- sp$R_max
    out$R_max_finite <- is.finite(sp$R_max)
  }

  if (!quiet) {
    bad <- out[!out$ordering_ok, ]
    if (nrow(bad))
      message("ordering violated for: ", paste(bad$species, collapse = ", "))
    if (any(out$at_mizer_clamp))
      message("sitting on mizer's 0.001 fallback (verify against source): ",
              paste(out$species[out$at_mizer_clamp], collapse = ", "))
    if (!is.null(out$erepro_ok) && any(!out$erepro_ok))
      message("erepro >= 1 for: ",
              paste(out$species[!out$erepro_ok], collapse = ", "))
    if (!is.null(out$R_max_finite) && any(!out$R_max_finite))
      message("R_max non-finite for: ",
              paste(out$species[!out$R_max_finite], collapse = ", "))
  }
  invisible(out)
}
