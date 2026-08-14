# =============================================================================
# Establish the correct way to change small divers' w_min on these params.
#
# w_min feeds several derived slots (w_min_idx, ft_mask, initial_n support).
# Rather than assume setParams() rebuilds them, check empirically which slots
# move and which have to be set by hand.
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer)
  library(mizer)
})

W_NEW <- 3626.667
PEN   <- "small divers"

ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
p <- ens$fished_top10pct[[1]]@params
i_pen <- which(p@species_params$species == PEN)

cat("=== slots that mention w_min ===\n")
cat("w_min_idx (stored):\n"); print(p@w_min_idx)
cat("\nft_mask present:", "ft_mask" %in% slotNames(p),
    " dim:", tryCatch(paste(dim(p@ft_mask), collapse = "x"),
                      error = function(e) "absent"), "\n")

gwi <- get("get_w_min_idx", envir = asNamespace("mizer"))
cat("\nrecomputed from stored species_params:", gwi(p@species_params, p@w)[i_pen],
    " (stored:", p@w_min_idx[i_pen], ")\n")

cat("\ninitial_n support for", PEN, ": first nonzero bin =",
    min(which(p@initial_n[i_pen, ] > 0)),
    " w =", signif(p@w[min(which(p@initial_n[i_pen, ] > 0))], 5), "g\n")

# --- try setParams -----------------------------------------------------------
cat("\n=== does setParams() rebuild w_min_idx / ft_mask? ===\n")
p2 <- p
sp <- p2@species_params
sp$w_min[i_pen] <- W_NEW
p2@species_params <- sp
p3 <- try(suppressWarnings(setParams(p2)), silent = TRUE)
if (inherits(p3, "try-error")) {
  cat("  setParams failed:", as.character(p3), "\n")
} else {
  cat("  w_min_idx after setParams:", p3@w_min_idx[i_pen],
      " (want", gwi(sp, p@w)[i_pen], ")\n")
  cat("  ft_mask changed by setParams:",
      !isTRUE(all.equal(p@ft_mask, p3@ft_mask)), "\n")
  cat("  search_vol changed:", !isTRUE(all.equal(p@search_vol, p3@search_vol)), "\n")
  cat("  metab changed:", !isTRUE(all.equal(p@metab, p3@metab)), "\n")
  cat("  psi changed:", !isTRUE(all.equal(p@psi, p3@psi)), "\n")
  cat("  mu_b changed:", !isTRUE(all.equal(p@mu_b, p3@mu_b)), "\n")
  cat("  initial_n changed:", !isTRUE(all.equal(p@initial_n, p3@initial_n)), "\n")
  cat("  rates_funcs preserved:", identical(p@rates_funcs, p3@rates_funcs), "\n")
  cat("  other_params preserved:",
      isTRUE(all.equal(p@other_params, p3@other_params)), "\n")
  cat("  resource_dynamics preserved:",
      identical(p@resource_dynamics, p3@resource_dynamics), "\n")
}

# --- how is ft_mask built? ---------------------------------------------------
cat("\n=== ft_mask structure ===\n")
if ("ft_mask" %in% slotNames(p)) {
  cat("  dim:", dim(p@ft_mask), "\n")
  cat("  for", PEN, ": first nonzero column =",
      min(which(p@ft_mask[i_pen, ] != 0)),
      " of", ncol(p@ft_mask), "\n")
  cat("  w_full at that index:",
      signif(p@w_full[min(which(p@ft_mask[i_pen, ] != 0))], 5), "g\n")
  cat("  (species w_min =", p@species_params$w_min[i_pen], "g)\n")
  cat("  unique values:", paste(head(unique(as.numeric(p@ft_mask)), 5),
                                collapse = " "), "\n")
}
