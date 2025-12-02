# Validate Climate-Only Simulations
library(mizer)

sims_dir <- "Output_large_files/climate_only_ensemble/individual_sims"
files <- list.files(sims_dir, pattern = "\\.rds$", full.names = TRUE)

cat("=== Validating", length(files), "climate-only simulations ===\n\n")

valid <- 0
invalid <- 0
errors <- c()

for (f in files) {
  tryCatch({
    sim <- readRDS(f)
    
    # Check class
    if (!inherits(sim, "MizerSim")) {
      invalid <- invalid + 1
      errors <- c(errors, paste(basename(f), "- Not a MizerSim object"))
      next
    }
    
    # Check time range
    times <- as.numeric(dimnames(sim@n)$time)
    if (min(times) != 1841 || max(times) != 2010) {
      invalid <- invalid + 1
      errors <- c(errors, paste(basename(f), "- Wrong time range:", min(times), "-", max(times)))
      next
    }
    
    # Check for NAs
    n_na <- sum(is.na(sim@n))
    if (n_na > 0) {
      invalid <- invalid + 1
      errors <- c(errors, paste(basename(f), "- Contains", n_na, "NA values"))
      next
    }
    
    # Check timesteps
    if (length(times) != 170) {
      invalid <- invalid + 1
      errors <- c(errors, paste(basename(f), "- Wrong timesteps:", length(times)))
      next
    }
    
    valid <- valid + 1
    
  }, error = function(e) {
    invalid <<- invalid + 1
    errors <<- c(errors, paste(basename(f), "- Error:", e$message))
  })
}

cat("Valid simulations:", valid, "/", length(files), "\n")
cat("Invalid simulations:", invalid, "\n")

if (length(errors) > 0) {
  cat("\nErrors found:\n")
  for (e in errors) cat("  ", e, "\n")
} else {
  cat("\n*** All simulations are valid! ***\n")
}

# Show sample details from first valid file
cat("\nSample simulation details:\n")
sim1 <- readRDS(files[1])
times <- as.numeric(dimnames(sim1@n)$time)
cat("  Time range:", min(times), "-", max(times), "\n")
cat("  Timesteps:", length(times), "\n")
cat("  Species:", nrow(sim1@params@species_params), "\n")
cat("  Size bins:", ncol(sim1@n[1,,]), "\n")

# Check progress file
progress_file <- "Output_large_files/climate_only_ensemble/progress_sequential.rds"
if (file.exists(progress_file)) {
  p <- readRDS(progress_file)
  cat("\nProgress file status:\n")
  cat("  Completed:", length(p$completed), "\n")
  cat("  Failed:", length(p$failed), "\n")
  cat("  Last update:", as.character(p$last_update), "\n")
}

cat("\n=== Summary ===\n")
cat("Simulations completed:", valid, "/ 2111\n")
cat("Remaining:", 2111 - valid, "\n")
cat("Percent complete:", round(valid/2111*100, 1), "%\n")
