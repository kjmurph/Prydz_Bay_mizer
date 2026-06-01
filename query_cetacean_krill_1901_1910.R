G_TO_KT <- 1e-9
CETS <- c("minke whales", "orca", "sperm whales", "baleen whales")
Y0 <- 1901; Y1 <- 1910

fish <- readRDS("whale_consumption_outputs/full_diet_top10pct_fishing_all_sims.rds")
clim <- readRDS("whale_consumption_outputs/full_diet_top10pct_climate_only_all_sims.rds")

extract <- function(arr_list, label) {
  vals <- sapply(arr_list, function(arr) {
    years <- as.numeric(dimnames(arr)[["year"]])
    t_idx <- years >= Y0 & years <= Y1
    slice <- arr[t_idx, CETS, "antarctic krill"]
    mean(rowSums(slice))
  })
  cat(sprintf("%s  n=%d  median=%.4f kt/yr  IQR=[%.4f, %.4f]\n",
    label, length(vals),
    median(vals)*G_TO_KT,
    quantile(vals, 0.25)*G_TO_KT,
    quantile(vals, 0.75)*G_TO_KT))
  cat("  By species (median across sims):\n")
  for (sp in CETS) {
    sp_vals <- sapply(arr_list, function(arr) {
      years <- as.numeric(dimnames(arr)[["year"]])
      t_idx <- years >= Y0 & years <= Y1
      mean(arr[t_idx, sp, "antarctic krill"])
    })
    cat(sprintf("    %-20s  %.4f kt/yr\n", sp, median(sp_vals)*G_TO_KT))
  }
  cat("\n")
}

extract(fish, "Exploited   (1901-1910)")
extract(clim, "Unexploited (1901-1910)")
