# Standalone re-run of the params/ directory scan with case-insensitive
# matching (params/ holds 18 *.rds and 28 *.RDS = 46 objects).
suppressPackageStartupMessages(library(mizer))

pfiles <- list.files("params", pattern = "\\.rds$", full.names = TRUE,
                     ignore.case = TRUE)
cat("objects in params/:", length(pfiles), "\n\n")

tab <- do.call(rbind, lapply(pfiles, function(f) {
  o <- try(readRDS(f), silent = TRUE)
  if (inherits(o, "try-error")) return(NULL)
  sp <- try(o@species_params, silent = TRUE)
  if (inherits(sp, "try-error") || is.null(sp$species)) return(NULL)
  i <- which(sp$species == "small divers")
  if (!length(i)) return(NULL)
  data.frame(file = basename(f), n_sp = nrow(sp), w_min = sp$w_min[i],
             w_mat = sp$w_mat[i], stringsAsFactors = FALSE)
}))

cat("objects containing a 'small divers' row:", nrow(tab), "\n\n")
print(tab, digits = 10, row.names = FALSE)
cat("\ntabulation of small divers w_min:\n")
print(table(signif(tab$w_min, 7)))
write.csv(tab, "Output_large_files/wmin_test/00d_params_dir_scan.csv",
          row.names = FALSE)
