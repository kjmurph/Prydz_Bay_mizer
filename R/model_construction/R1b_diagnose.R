# R1b_diagnose.R -- explain the R1 mismatch: decompose stored - rebuilt into
# per-row intercept and slope differences, and test candidate variants.
source(file.path(Sys.getenv("ASSESS_SRC", "R/model_construction"), "A0_helpers.R"))
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(lubridate) })
id <- "R1b"
pf <- function(f) repo_path("FishMIP_Plankton_Forcing", f)
stored <- unname(as.matrix(read.table(repo_path("GFDL_resource_spectra_annual.dat"))))
so <- readRDS(repo_path("params", "params_latest_xx.RDS"))
full_x <- log10(so@w_full)

# per-row linear fit of each stored row on full_x: is it exactly linear?
st_fit <- t(apply(stored, 1, function(r) coef(lm(r ~ full_x))))
resid_max <- max(abs(stored - (st_fit[, 1] + outer(st_fit[, 2], full_x))))
note(id, "stored rows are exact lines in log10(w_full)", paste("max residual", fmt(resid_max)))
note(id, "stored slope range", paste(fmt(range(st_fit[, 2])), collapse = " to "))
note(id, "stored intercept range", paste(fmt(range(st_fit[, 1])), collapse = " to "))

# rebuild the four annual class totals, as R1 does, with switches
read_all <- function() {
  list(diat = read.csv(pf("gfdl-mom6-cobalt2_obsclim_phydiat-vint_15arcmin_prydz-bay_monthly_1961_2010.csv")),
       pico = read.csv(pf("gfdl-mom6-cobalt2_obsclim_phypico-vint_15arcmin_Prydz-Bay_monthly_1961_2010.csv")),
       diaz = read.csv(pf("gfdl-mom6-cobalt2_obsclim_phydiaz-vint_15arcmin_Prydz-Bay_monthly_1961_2010.csv")),
       zmicro = read.csv(pf("gfdl-mom6-cobalt2_obsclim_zmicro-vint_15arcmin_prydz-bay_monthly_1961_2010.csv")),
       zmeso = read.csv(pf("gfdl-mom6-cobalt2_obsclim_zmeso-vint_15arcmin_prydz-bay_monthly_1961_2010.csv")))
}
raw <- read_all()
area <- raw$diat$area_m2
# monthly domain totals of mol C (area-weighted), 600 months, per field
mon_tot <- function(df, weight = TRUE) {
  vals <- as.matrix(df[, !(names(df) %in% c("lat", "lon", "area_m2"))])
  if (weight) colSums(vals * area) else colSums(vals)
}
annual <- function(v) colMeans(matrix(v, nrow = 12))
tot <- lapply(raw, mon_tot)
ann <- lapply(tot, annual)
mids <- c(pico = (4/3)*pi*((0.5*0.0001*5.1)^3), micro = (4/3)*pi*((0.5*0.0001*101)^3),
          large = (4/3)*pi*((0.5*0.0001*105)^3), meso = (4/3)*pi*((0.5*0.0001*10100)^3))
px <- log10(mids)
build <- function(pico, micro, large, meso, conv = 10, grid = full_x) {
  out <- matrix(NA, 50, length(grid)); s <- numeric(50); ic <- numeric(50)
  for (t in 1:50) {
    y <- log10(conv * c(pico[t] / mids["pico"], micro[t] / mids["micro"],
                        large[t] / mids["large"], meso[t] / mids["meso"]))
    f <- lm(y ~ px); out[t, ] <- f$coefficients[2] * grid + f$coefficients[1]
    s[t] <- f$coefficients[2]; ic[t] <- f$coefficients[1]
  }
  list(out = out, s = s, ic = ic)
}
base <- build(ann$pico, ann$zmicro, ann$diat + ann$diaz, ann$zmeso)
note(id, "R1 rebuild reproduced here", paste("max abs diff vs stored", fmt(max(abs(base$out - stored)))))
d_ic <- st_fit[, 1] - base$ic; d_s <- st_fit[, 2] - base$s
note(id, "stored - rebuilt: intercept diff range", paste(fmt(range(d_ic)), collapse = " to "))
note(id, "stored - rebuilt: slope diff range", paste(fmt(range(d_s)), collapse = " to "))

cands <- list(
  "x12.001 (mol->g applied)" = build(ann$pico, ann$zmicro, ann$diat + ann$diaz, ann$zmeso, conv = 120.01),
  "unweighted cell sums" = {
    a2 <- lapply(lapply(raw, mon_tot, weight = FALSE), annual)
    build(a2$pico, a2$zmicro, a2$diat + a2$diaz, a2$zmeso)
  },
  "large = diatoms only" = build(ann$pico, ann$zmicro, ann$diat, ann$zmeso),
  "micro/large mids swapped" = {
    m2 <- mids; m2[c("micro", "large")] <- m2[c("large", "micro")]
    y <- NULL; out <- matrix(NA, 50, 142)
    for (t in 1:50) {
      yy <- log10(10 * c(ann$pico[t] / m2["pico"], ann$zmicro[t] / m2["micro"],
                         (ann$diat[t] + ann$diaz[t]) / m2["large"], ann$zmeso[t] / m2["meso"]))
      f <- lm(yy ~ px); out[t, ] <- f$coefficients[2] * full_x + f$coefficients[1]
    }
    list(out = out)
  })
for (nm in names(cands)) {
  note(id, paste("candidate:", nm), paste("max abs diff vs stored", fmt(max(abs(cands[[nm]]$out - stored)))))
}
# Is the stored series a fit to annual means of the MONTHLY fits? (i.e. fit
# per month on the 600 monthly totals, then average the 12 spectra per year)
fit_monthly <- function(conv = 10) {
  out <- matrix(NA, 600, 142)
  for (t in 1:600) {
    y <- log10(conv * c(tot$pico[t] / mids["pico"], tot$zmicro[t] / mids["micro"],
                        (tot$diat[t] + tot$diaz[t]) / mids["large"], tot$zmeso[t] / mids["meso"]))
    f <- lm(y ~ px); out[t, ] <- f$coefficients[2] * full_x + f$coefficients[1]
  }
  out
}
fm <- fit_monthly()
ann_of_monthly <- t(sapply(1:50, function(y) colMeans(fm[((y - 1) * 12 + 1):(y * 12), ])))
note(id, "candidate: annual mean of the 600 monthly log-spectra",
     paste("max abs diff vs stored", fmt(max(abs(ann_of_monthly - stored)))))
fm12 <- fit_monthly(conv = 120.01)
ann_of_monthly12 <- t(sapply(1:50, function(y) colMeans(fm12[((y - 1) * 12 + 1):(y * 12), ])))
note(id, "candidate: annual mean of monthly log-spectra, x12.001",
     paste("max abs diff vs stored", fmt(max(abs(ann_of_monthly12 - stored)))))
# anomaly comparison: does the stored file carry the same anomaly as the rebuild?
anom <- function(m) sweep(m, 2, colMeans(m))
note(id, "anomaly (row - 1961-2010 mean): stored vs R1 rebuild",
     paste("max abs diff", fmt(max(abs(anom(stored) - anom(base$out))))))
note(id, "anomaly: stored vs annual-mean-of-monthly",
     paste("max abs diff", fmt(max(abs(anom(stored) - anom(ann_of_monthly))))))
save_results(id)
