# =============================================================================
# Stage 0, step 0: verify the w_min claims in docs/small_divers_wmin_test_brief.md
#
# The brief asserts (section 1) that w_min for `small divers` is 0.001 g in the
# parameter set behind the 2,111-member ensemble, and that w_min is identical
# across all 212 exploited and all 212 unexploited simulation objects. It also
# asks that this be verified rather than trusted.
#
# Writes: Output_large_files/wmin_test/00_wmin_verification.rds  (+ console log)
# =============================================================================

suppressPackageStartupMessages({
  library(mizer)
})

if (!dir.exists("Output_large_files"))
  stop("Run this script from the repository root: 'Output_large_files/' not found.")

out_dir <- "Output_large_files/wmin_test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== w_min verification:", format(Sys.time()), "===\n\n")

res <- list()

# --- 1. The two parameter generations ---------------------------------------
cat("--- 1. Parameter-set comparison ---\n")

p_old <- readRDS("params/params_16_March_2023.rds")
p_new <- readRDS("Manuscript data/params_sel_adj.rds")

sp_old <- p_old@species_params
sp_new <- p_new@species_params

cat("params_16_March_2023.rds : n groups =", nrow(sp_old), "\n")
cat("params_sel_adj.rds       : n groups =", nrow(sp_new), "\n")

common <- intersect(sp_old$species, sp_new$species)
cat("Groups common to both    :", length(common), "\n\n")

cmp <- data.frame(
  species    = common,
  w_min_old  = sp_old$w_min[match(common, sp_old$species)],
  w_min_new  = sp_new$w_min[match(common, sp_new$species)],
  w_mat_new  = sp_new$w_mat[match(common, sp_new$species)],
  w_max_new  = if (!is.null(sp_new$w_max)) sp_new$w_max[match(common, sp_new$species)]
               else sp_new$w_inf[match(common, sp_new$species)],
  stringsAsFactors = FALSE
)
cmp$ratio_new_over_old <- cmp$w_min_new / cmp$w_min_old
cmp$changed <- !isTRUE(all.equal(cmp$w_min_old, cmp$w_min_new)) &
               abs(cmp$ratio_new_over_old - 1) > 1e-10
print(cmp, digits = 6)
cat("\n")
res$param_generation_cmp <- cmp

# Size span, all groups in the current parameter set
span <- data.frame(
  species = sp_new$species,
  w_min   = sp_new$w_min,
  w_mat   = sp_new$w_mat,
  w_max   = if (!is.null(sp_new$w_max)) sp_new$w_max else sp_new$w_inf,
  stringsAsFactors = FALSE
)
span$decades      <- log10(span$w_max / span$w_min)
span$wmin_over_wmat <- span$w_min / span$w_mat
cat("--- Size span (log10 w_max/w_min), current parameter set ---\n")
print(span[order(-span$decades), ], digits = 6)
cat("\n")
res$size_span <- span

# The claimed target value
w_min_target <- 3626.667
sd_row <- which(sp_new$species == "small divers")
cat("small divers  w_min (current) :", sp_new$w_min[sd_row], "g\n")
cat("small divers  w_mat           :", sp_new$w_mat[sd_row], "g\n")
cat("0.85 * w_mat                  :", 0.85 * sp_new$w_mat[sd_row], "g\n")
cat("brief's target                :", w_min_target, "g\n\n")

# --- 2. w_min across the 212 paired simulation objects -----------------------
cat("--- 2. w_min across the 212 paired simulation objects ---\n")
cat("Loading top10pct_rmse_ensembles.rds (375 MB) ...\n")
ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
cat("Top-level names:", paste(names(ens), collapse = ", "), "\n")
str(ens, max.level = 1)

# Identify the two arms without assuming names.
arm_names <- names(ens)
cat("\n")

collect_wmin <- function(sim_list, label) {
  n <- length(sim_list)
  cat("  ", label, ": n =", n, "\n")
  m <- t(vapply(seq_len(n), function(i) {
    sp <- sim_list[[i]]@params@species_params
    setNames(sp$w_min, sp$species)
  }, numeric(nrow(sim_list[[1]]@params@species_params))))
  m
}

res$arms <- list()
for (a in arm_names) {
  x <- ens[[a]]
  # the arm may be a list of sims, or a list containing $simulations
  sims <- if (is.list(x) && !is.null(x$simulations)) x$simulations else x
  if (!is.list(sims) || !inherits(sims[[1]], "MizerSim")) {
    cat("  ", a, ": not a list of MizerSim -- skipping\n")
    next
  }
  m <- collect_wmin(sims, a)
  n_unique_rows <- nrow(unique(m))
  cat("     unique w_min rows across members:", n_unique_rows,
      if (n_unique_rows == 1) " (identical everywhere)" else " (VARIES)", "\n")
  cat("     small divers w_min: range [",
      format(min(m[, "small divers"]), digits = 10), ",",
      format(max(m[, "small divers"]), digits = 10), "]\n")
  res$arms[[a]] <- list(wmin_matrix_unique = unique(m),
                        n_members = nrow(m),
                        n_unique_rows = n_unique_rows,
                        small_divers_range = range(m[, "small divers"]))
}

# Cross-arm check
if (length(res$arms) == 2) {
  u1 <- res$arms[[1]]$wmin_matrix_unique
  u2 <- res$arms[[2]]$wmin_matrix_unique
  cat("\n  Exploited vs unexploited w_min identical:",
      isTRUE(all.equal(u1, u2, check.attributes = FALSE)), "\n")
}

# --- 3. w_min across the params/ directory -----------------------------------
cat("\n--- 3. w_min for 'small divers' across params/ ---\n")
# NB: params/ holds 18 *.rds AND 28 *.RDS -- 46 objects in total. A
# case-sensitive pattern silently sees only 18 of them.
pfiles <- list.files("params", pattern = "\\.rds$", full.names = TRUE,
                     ignore.case = TRUE)
scan_tab <- do.call(rbind, lapply(pfiles, function(f) {
  o <- try(readRDS(f), silent = TRUE)
  if (inherits(o, "try-error")) return(NULL)
  sp <- try(o@species_params, silent = TRUE)
  if (inherits(sp, "try-error") || is.null(sp$species)) return(NULL)
  i <- which(sp$species == "small divers")
  if (!length(i)) return(NULL)
  data.frame(file = basename(f), n_sp = nrow(sp),
             w_min = sp$w_min[i], w_mat = sp$w_mat[i],
             stringsAsFactors = FALSE)
}))
if (!is.null(scan_tab)) {
  print(scan_tab, digits = 10)
  cat("\n  tabulation of small divers w_min:\n")
  print(table(signif(scan_tab$w_min, 7)))
}
res$params_dir_scan <- scan_tab

saveRDS(res, file.path(out_dir, "00_wmin_verification.rds"))
cat("\nSaved:", file.path(out_dir, "00_wmin_verification.rds"), "\n")
cat("=== done:", format(Sys.time()), "===\n")
