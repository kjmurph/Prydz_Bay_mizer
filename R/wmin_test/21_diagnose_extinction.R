# =============================================================================
# The treated runs lose the penguin group entirely. Establish whether that is a
# genuine consequence of the corrected w_min or an artefact of how the parameter
# was changed, before any of the Stage 1 numbers are interpreted.
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer)
  library(mizer)
})

PEN <- "small divers"
W_NEW <- 3626.667
res <- readRDS("Output_large_files/wmin_test/19_stage1_sims.rds")
r <- res[[1]]

pc <- r$control$spin
pt <- r$treated$spin
p_c <- pc@params
p_t <- pt@params
i <- which(p_c@species_params$species == PEN)
w <- p_c@w; dw <- p_c@dw

cat("=== grid geometry for", PEN, "===\n")
cat("  w_min control:", p_c@species_params$w_min[i],
    " w_min_idx:", p_c@w_min_idx[[i]], " w at that idx:", signif(w[p_c@w_min_idx[[i]]], 6), "\n")
cat("  w_min treated:", p_t@species_params$w_min[i],
    " w_min_idx:", p_t@w_min_idx[[i]], " w at that idx:", signif(w[p_t@w_min_idx[[i]]], 6), "\n")
cat("  w_mat:", p_c@species_params$w_mat[i], " w_max:", p_c@species_params$w_max[i], "\n")
cat("  bins from w_min_idx to w_max: control",
    sum(w >= w[p_c@w_min_idx[[i]]] & w <= p_c@species_params$w_max[i]),
    "| treated",
    sum(w >= w[p_t@w_min_idx[[i]]] & w <= p_t@species_params$w_max[i]), "\n")
cat("  w around the new w_min:", signif(w[69:75], 6), "\n\n")

cat("=== penguin biomass through the 118 yr spin-up ===\n")
bt <- function(s) {
  n <- s@n[, PEN, ]
  rowSums(sweep(n, 2, w * dw, "*"))
}
bc <- bt(pc); btr <- bt(pt)
yrs <- as.numeric(dimnames(pc@n)$time)
show <- c(1, 2, 3, 5, 10, 20, 40, 80, 119)
show <- show[show <= length(yrs)]
cat("  year   :", paste(sprintf("%8d", yrs[show]), collapse = ""), "\n")
cat("  control:", paste(sprintf("%8.2e", bc[show]), collapse = ""), "\n")
cat("  treated:", paste(sprintf("%8.2e", btr[show]), collapse = ""), "\n\n")

cat("=== penguin size distribution, treated, first years ===\n")
for (ti in c(1, 2, 3, 5, 20)) {
  n <- pt@n[ti, PEN, ]
  nz <- which(n > 0)
  cat(sprintf("  t=%d (%d): nonzero bins %s | total N %.3e\n", ti, yrs[ti],
              if (length(nz)) paste(range(nz), collapse = "-") else "NONE",
              sum(n * dw)))
}

cat("\n=== reproduction diagnostics at t = 1841 ===\n")
for (nm in c("control", "treated")) {
  p <- if (nm == "control") p_c else p_t
  s <- if (nm == "control") pc else pt
  n0 <- s@n[1, , ]
  npp0 <- s@n_pp[1, ]
  rates <- getRates(p, n = n0, n_pp = npp0, n_other = p@initial_n_other,
                    effort = 0, t = 1841)
  cat(sprintf("  %-8s rdi=%.4e  rdd=%.4e  R_max=%.4e  erepro=%.4e  w[w_min_idx]=%.4g\n",
              nm, rates$rdi[i], rates$rdd[i],
              p@species_params$R_max[i], p@species_params$erepro[i],
              p@w[p@w_min_idx[[i]]]))
  cat(sprintf("           recruit BIOMASS influx = rdd * w_min = %.4e g/yr\n",
              rates$rdd[i] * p@w[p@w_min_idx[[i]]]))
  eg <- rates$e_growth[i, ]
  cat(sprintf("           growth rate at w_min_idx: %.4e g/yr ; psi there = %.4g\n",
              eg[p@w_min_idx[[i]]], p@psi[i, p@w_min_idx[[i]]]))
  mort <- rates$mort[i, ]
  cat(sprintf("           total mortality at w_min_idx: %.4g /yr\n",
              mort[p@w_min_idx[[i]]]))
  # time to grow one bin vs mortality
  k <- p@w_min_idx[[i]]
  cat(sprintf("           dw/growth = %.4g yr to cross bin %d ; survival over that = %.4g\n",
              dw[k] / eg[k], k, exp(-mort[k] * dw[k] / eg[k])))
}

cat("\n=== psi / maturity around the new w_min (treated) ===\n")
cat("  bin :", paste(sprintf("%10d", 69:75), collapse = ""), "\n")
cat("  w   :", paste(sprintf("%10.4g", w[69:75]), collapse = ""), "\n")
cat("  psi :", paste(sprintf("%10.4g", p_t@psi[i, 69:75]), collapse = ""), "\n")
cat("  mat :", paste(sprintf("%10.4g", p_t@maturity[i, 69:75]), collapse = ""), "\n")
cat("  initial_n treated:", paste(sprintf("%10.3e", p_t@initial_n[i, 69:75]),
                                  collapse = ""), "\n")
cat("  initial_n control:", paste(sprintf("%10.3e", p_c@initial_n[i, 69:75]),
                                  collapse = ""), "\n")
