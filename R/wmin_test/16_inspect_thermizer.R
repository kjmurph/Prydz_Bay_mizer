# What upstream therMizer actually does -- settles whether the sqrt form and the
# t_idx indexing in my reconstruction matched the real package.
suppressPackageStartupMessages(library(therMizer))
cat("therMizer version:", as.character(packageVersion("therMizer")), "\n\n")
for (f in c("therMizerEncounter", "therMizerPredRate", "therMizerEReproAndGrowth",
            "plankton_forcing", "setEncounterPredScale", "setMetabTher")) {
  cat("================", f, "================\n")
  obj <- tryCatch(get(f, envir = asNamespace("therMizer")), error = function(e) NULL)
  if (is.null(obj)) { cat("  NOT EXPORTED / NOT FOUND\n\n"); next }
  print(obj)
  cat("\n")
}
cat("=== exported ===\n")
print(ls(asNamespace("therMizer")))
