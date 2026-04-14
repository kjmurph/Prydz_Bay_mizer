library(mizer)
setwd("C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/Prydz_Bay_mizer")

inspect_list <- function(x, prefix="", max_depth=3, depth=0) {
  if (depth > max_depth) return(invisible(NULL))
  nms <- names(x)
  if (is.null(nms)) nms <- paste0("[[", seq_along(x), "]]")
  for (i in seq_along(x)) {
    el <- x[[i]]
    nm <- nms[i]
    d <- if (!is.null(dim(el))) paste(dim(el), collapse="x") else length(el)
    cl <- paste(class(el), collapse="/")
    cat(sprintf("%s$%s : %s  [%s]\n", prefix, nm, cl, d))
    if (is.list(el) && depth < max_depth && length(el) > 0 && length(el) <= 20) {
      inspect_list(el, prefix=paste0(prefix, "  "), max_depth=max_depth, depth=depth+1)
    } else if (is.list(el) && length(el) > 20) {
      cat(sprintf("%s  (list of length %d, not expanded)\n", prefix, length(el)))
    }
  }
}

cat("=== mc_ensemble_2111_cleaned.rds ===\n")
mc <- readRDS("Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds")
cat("Top-level: class=", class(mc), " length=", length(mc), "\n")
cat("Names:", paste(names(mc), collapse=", "), "\n\n")

# Explore each top-level item
for (nm in names(mc)) {
  el <- mc[[nm]]
  d <- if (!is.null(dim(el))) paste(dim(el), collapse="x") else length(el)
  cl <- paste(class(el), collapse="/")
  cat(sprintf("$%s : %s  [%s]\n", nm, cl, d))
  if (nm == "simulations") {
    cat("  (Showing structure of simulations[[1]]):\n")
    s1 <- el[[1]]
    cat("  Class:", class(s1), "\n")
    if (!is.null(names(s1))) {
      for (snm in names(s1)) {
        sx <- s1[[snm]]
        sd <- if (!is.null(dim(sx))) paste(dim(sx), collapse="x") else length(sx)
        scl <- paste(class(sx), collapse="/")
        cat(sprintf("    $%s : %s  [%s]\n", snm, scl, sd))
        if (is.list(sx) && length(sx) <= 10) {
          for (snm2 in names(sx)) {
            sx2 <- sx[[snm2]]
            sd2 <- if (!is.null(dim(sx2))) paste(dim(sx2), collapse="x") else length(sx2)
            cat(sprintf("      $%s : %s  [%s]\n", snm2, paste(class(sx2), collapse="/"), sd2))
          }
        }
      }
    }
  } else if (nm == "parameters_table" && is.data.frame(el)) {
    cat("  (data.frame: ", nrow(el), "rows x", ncol(el), "cols)\n")
    cat("  Columns:", paste(names(el), collapse=", "), "\n")
  } else if (nm == "flat_summary" && is.data.frame(el)) {
    cat("  (data.frame: ", nrow(el), "rows x", ncol(el), "cols)\n")
    cat("  Columns:", paste(names(el), collapse=", "), "\n")
  } else if (nm == "settings" && is.list(el)) {
    cat("  Settings:", paste(names(el), collapse=", "), "\n")
    for (snm in names(el)) {
      cat(sprintf("    $%s = %s\n", snm, paste(el[[snm]], collapse=", ")))
    }
  }
}

cat("\n\n=== climate_only_ensemble_compiled.rds ===\n")
co <- readRDS("Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds")
cat("Top-level: class=", class(co), " length=", length(co), "\n")
cat("Names:", paste(names(co), collapse=", "), "\n\n")

for (nm in names(co)) {
  el <- co[[nm]]
  d <- if (!is.null(dim(el))) paste(dim(el), collapse="x") else length(el)
  cl <- paste(class(el), collapse="/")
  cat(sprintf("$%s : %s  [%s]\n", nm, cl, d))
  if (nm == "simulations") {
    cat("  (Showing structure of simulations[[1]]):\n")
    s1 <- el[[1]]
    cat("  Class:", class(s1), "\n")
    if (!is.null(names(s1))) {
      for (snm in names(s1)) {
        sx <- s1[[snm]]
        sd <- if (!is.null(dim(sx))) paste(dim(sx), collapse="x") else length(sx)
        scl <- paste(class(sx), collapse="/")
        cat(sprintf("    $%s : %s  [%s]\n", snm, scl, sd))
        if (is.list(sx) && length(sx) <= 10) {
          for (snm2 in names(sx)) {
            sx2 <- sx[[snm2]]
            sd2 <- if (!is.null(dim(sx2))) paste(dim(sx2), collapse="x") else length(sx2)
            cat(sprintf("      $%s : %s  [%s]\n", snm2, paste(class(sx2), collapse="/"), sd2))
          }
        }
      }
    }
  } else if (nm == "settings" && is.list(el)) {
    cat("  Settings:", paste(names(el), collapse=", "), "\n")
    for (snm in names(el)) {
      cat(sprintf("    $%s = %s\n", snm, paste(el[[snm]], collapse=", ")))
    }
  }
}

cat("\nDone.\n")
