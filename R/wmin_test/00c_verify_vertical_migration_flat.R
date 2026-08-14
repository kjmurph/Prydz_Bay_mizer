# Scratch: does the therMizer temperature effect vary with PREDATOR SIZE?
# If vertical_migration is flat in w for a species, the temp effect is a
# species-level scalar and cannot change within-species size shares.
suppressPackageStartupMessages(library(mizer))

ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
p <- ens$fished_top10pct[[1]]@params
op <- p@other_params$other

vm <- op$vertical_migration   # realm x species x w
ex <- op$exposure             # realm x species
ot <- op$ocean_temp           # time x realm

cat("dim vertical_migration:", dim(vm), "\n")
cat("dimnames realm:", dimnames(vm)[[1]], "\n")
cat("dim exposure:", dim(ex), "\n")
cat("dim ocean_temp:", dim(ot), " realms:", colnames(ot), "\n")
cat("ocean_temp time range:", head(rownames(ot),1), "-", tail(rownames(ot),1), "\n\n")

# Does vertical_migration vary with w, per species?
cat("--- vertical_migration variation across w, by species ---\n")
for (sp in dimnames(vm)[[2]]) {
  m <- vm[, sp, ]              # realm x w
  varies <- max(apply(m, 1, function(r) diff(range(r))))
  cat(sprintf("%-26s max range across w = %.6g%s\n", sp, varies,
              if (varies > 1e-12) "   <-- VARIES WITH SIZE" else ""))
}

cat("\n--- exposure matrix ---\n")
print(ex)

cat("\n--- small divers vertical_migration (realm x selected w) ---\n")
wsel <- c(1, 25, 50, 60, 70, 80, 90, 100)
print(round(vm[, "small divers", wsel], 4))
cat("w values at those indices:", signif(p@w[wsel], 4), "\n")

cat("\n--- thermal params ---\n")
sp <- p@species_params
print(data.frame(species = sp$species, temp_min = sp$temp_min, temp_max = sp$temp_max,
                 encounterpred_scale = sp$encounterpred_scale,
                 metab_min = sp$metab_min, metab_range = sp$metab_range))

cat("\n--- ocean_temp summary by realm ---\n")
print(summary(ot))
