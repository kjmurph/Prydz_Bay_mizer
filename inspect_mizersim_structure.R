library(mizer)
setwd("C:/Users/kjmurphy/OneDrive - University of Tasmania/Documents/GitHub/Prydz_Bay_mizer")

cat("=== Inspecting MizerSim structure from mc_ensemble_2111_cleaned.rds ===\n\n")
mc <- readRDS("Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds")

sim1 <- mc$simulations[[1]]
cat("sim1 class:", class(sim1), "\n")
cat("sim1 isS4:", isS4(sim1), "\n")
cat("sim1 slotNames:", paste(slotNames(sim1), collapse=", "), "\n\n")

for (sl in slotNames(sim1)) {
  x <- slot(sim1, sl)
  d <- if (!is.null(dim(x))) paste(dim(x), collapse="x") else length(x)
  cl <- paste(class(x), collapse="/")
  cat(sprintf("  @%s : %s  [%s]\n", sl, cl, d))
  
  # For n slot (species abundance over time)
  if (sl == "n") {
    cat("    dimnames: time x", paste(names(dimnames(x)), collapse=" x "), "\n")
    cat("    time range:", dimnames(x)$time[1], "to", tail(dimnames(x)$time, 1), "\n")
    cat("    n_species:", dim(x)[2], "  n_size_bins:", dim(x)[3], "\n")
  }
  if (sl == "n_pp") {
    cat("    dim: time x size =", paste(dim(x), collapse="x"), "\n")
  }
  if (sl == "effort") {
    cat("    dim:", paste(dim(x), collapse="x"), "\n")
    if (!is.null(dimnames(x))) cat("    gear names:", paste(dimnames(x)[[2]], collapse=", "), "\n")
  }
  if (sl == "params") {
    cat("    params class:", class(x), "\n")
    cat("    params slotNames:", paste(slotNames(x)[1:min(10,length(slotNames(x)))], collapse=", "), "...\n")
    sp_params <- x@species_params
    cat("    species_params nrow:", nrow(sp_params), "\n")
    cat("    species:", paste(rownames(sp_params), collapse=", "), "\n")
  }
}

cat("\n\n=== Parameter space summary (parameters_table) ===\n")
pt <- mc$parameters_table
cat("Dimensions:", nrow(pt), "rows x", ncol(pt), "cols\n")
cat("Columns:", paste(names(pt), collapse=", "), "\n")
cat("\nFirst 3 rows:\n")
print(head(pt, 3))

cat("\n\n=== Flat summary ===\n")
fs <- mc$flat_summary
cat("Dimensions:", nrow(fs), "rows x", ncol(fs), "cols\n")
cat("Columns:", paste(names(fs), collapse=", "), "\n")
cat("\nSummary:\n")
print(summary(fs[, c("stable", "max_cv", "mean_gamma_mult", "mean_abund_mult", "mean_catchability")]))

cat("\nn_successful:", mc$n_successful, "\n")
cat("n_attempts:", mc$n_attempts, "\n")

cat("\n\n=== Climate only: simulations[[1]] structure ===\n")
co <- readRDS("Output_large_files/climate_only_ensemble/climate_only_ensemble_compiled.rds")
sim1_co <- co$simulations[[1]]
cat("Class:", class(sim1_co), "\n")
cat("slotNames:", paste(slotNames(sim1_co), collapse=", "), "\n")

for (sl in slotNames(sim1_co)) {
  x <- slot(sim1_co, sl)
  d <- if (!is.null(dim(x))) paste(dim(x), collapse="x") else length(x)
  cl <- paste(class(x), collapse="/")
  cat(sprintf("  @%s : %s  [%s]\n", sl, cl, d))
  if (sl == "n") {
    cat("    time range:", dimnames(x)$time[1], "to", tail(dimnames(x)$time, 1), "\n")
    cat("    n_species:", dim(x)[2], "  n_size_bins:", dim(x)[3], "\n")
  }
  if (sl == "effort") {
    if (!is.null(dim(x))) cat("    dim:", paste(dim(x), collapse="x"), "\n")
    if (!is.null(dimnames(x))) cat("    gear names:", paste(dimnames(x)[[2]], collapse=", "), "\n")
  }
  if (sl == "params") {
    sp_params <- x@species_params
    cat("    n_species:", nrow(sp_params), "\n")
    cat("    species:", paste(rownames(sp_params), collapse=", "), "\n")
  }
}

cat("\nn_successful:", co$n_successful, "\n")
cat("n_failed:", co$n_failed, "\n")
cat("settings$effort:", co$settings$effort, "\n")
cat("settings$note:", co$settings$note, "\n")

cat("\nDone.\n")
