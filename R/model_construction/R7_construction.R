# R7_construction.R -- data-only checks of the 2023 construction inputs.
# (a) replay interaction matrix/2g_trait_groups_interaction_matrix_vCWC.R:40-156
#     from the stored trait distributions and compare with the stored v4 matrix,
#     the first 19-group object and params_sel_adj;
# (b) apply model_setup_v4.Rmd:104-322's overrides to trait_groups_params_vCWC_v4.csv
#     and compare with the first 19-group object; re-run mizer's two size
#     clamps on the table with R/check_size_params.R. Builds no model.
source(file.path(Sys.getenv("ASSESS_SRC", "R/model_construction"), "A0_helpers.R"))
suppressPackageStartupMessages({ library(dplyr) })
id <- "R7"

# ---- (a) 2g:40-156, verbatim ------------------------------------------------
all.prm <- readRDS(repo_path("group params", "trait_groups_params_distributions_vCWC_v4.rds"))
ips <- all.prm %>% select(species, max_depth, min_depth, water.column.use)
ips$adj <- 1
ips[ips$species == "flying birds", ]$max_depth <- 50
ips[ips$species == "other krill", ]$max_depth <- 500
ips[ips$species == "antarctic krill", ]$max_depth <- 500
ips[ips$max_depth > 2000, ]$max_depth <- 2000
ips$min_depth[is.na(ips$min_depth)] <- 0
ol.fun <- function(sp1, sp2, dat) {
  ol.p <- 1; ol.mod <- 1
  range1 <- c(dat[dat$species == sp1, ]$min_depth, dat[dat$species == sp1, ]$max_depth)
  range2 <- c(dat[dat$species == sp2, ]$min_depth, dat[dat$species == sp2, ]$max_depth)
  tot.r <- diff(range(range1, range2))
  if (length(intersect(seq(range1[1], range1[2]), seq(range2[1], range2[2]))) == 0) { ol.p = 0 } else {
    ol.r <- diff(range(intersect(seq(range1[1], range1[2]), seq(range2[1], range2[2]))))
    ol.p <- ol.r / tot.r
    dvms <- dat[dat$species %in% c(sp1, sp2), ]$water.column.use
    ol.mod <- 1
    if ("non DVM" %in% dvms & "DVM" %in% dvms) { ol.mod <- ol.mod * 0.5 }
    if (("diving" %in% dvms) & length(unique(dvms)) == 1) {
      ol.mod <- ol.mod * 4
      if (!(any(c(sp1, sp2) %in% c("orca", "leopard seals")))) ol.mod <- 0
      if (any(grepl("flying", c(sp1, sp2), ignore.case = T))) ol.mod <- 0
    }
    if (("diving" %in% dvms) & length(unique(dvms)) > 1) { ol.mod <- ol.mod * 0.5 }
    if (any(c(sp1, sp2) %in% "phytoplankton")) {
      if (any(c(sp1, sp2) %in% c("euphausiids", "salps", "other macrozooplankton", "mesozooplankton", "microzooplankton"))) {
        ol.mod <- ol.mod * 4 } else ol.mod <- 0
    }
    if (any(grepl("shelf", c(sp1, sp2), ignore.case = T)) & length(unique(c(sp1, sp2))) > 1) { ol.mod <- ol.mod * 0.2 }
    ol.adj <- prod(dat[dat$species %in% c(sp1, sp2), ]$adj)
    ol.mod <- ol.mod * ol.adj
    ol.p <- ol.p * ol.mod
    if (ol.p > 1) ol.p <- 1
  }
  ol.p
}
int.calc <- function(dat) {
  int.l <- expand.grid(dat$species, dat$species)
  int.l$ol.p <- NA
  for (i in 1:nrow(int.l)) int.l$ol.p[i] <- ol.fun(as.character(int.l[i, 1]), as.character(int.l[i, 2]), dat)
  data.frame(matrix(int.l$ol.p, nrow = nrow(dat), ncol = nrow(dat), dimnames = (list(dat$species, dat$species))))
}
int.m <- int.calc(ips)
st_int <- readRDS(repo_path("interaction matrix", "trait_groups_interaction_matrix_vCWC_v4.rds"))
check(id, "2g replay reproduces trait_groups_interaction_matrix_vCWC_v4.rds",
      isTRUE(all.equal(unname(as.matrix(int.m)), unname(as.matrix(st_int)), tolerance = 0)),
      paste("max abs diff", fmt(max(abs(as.matrix(int.m) - as.matrix(st_int))))))
theta <- as.matrix(st_int); theta[17, 17] <- 0                 # model_setup_v4.Rmd:363
p0 <- readRDS(repo_path("params", "steady_phase1_group_params_v3.RDS"))
check(id, "first 19-group object's interaction = v4 matrix with orca self-interaction 0 (model_setup_v4:352-363)",
      isTRUE(all.equal(unname(p0@interaction), unname(theta), tolerance = 0)),
      paste("max abs diff", fmt(max(abs(unname(p0@interaction) - unname(theta))))))
sel <- readRDS(repo_path("params_sel_adj.rds"))
check(id, "params_sel_adj carries the same interaction matrix (unchanged along the spine)",
      isTRUE(all.equal(unname(sel@interaction), unname(p0@interaction), tolerance = 0)))
note(id, "species order identical (2g distributions vs model)",
     identical(as.character(all.prm$species), as.character(p0@species_params$species)))

# ---- (b) model_setup_v4.Rmd:104-322 overrides on the v4 trait table -------------
groups_raw <- read.csv(repo_path("group params", "trait_groups_params_vCWC_v4.csv"))[, -1]
names(groups_raw)[4] <- "w_max"
groups_raw$beta[1:5] <- c(530.8844, 15848932, 446.6836, 15848932, 1778279410)
groups_raw$k_vb <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.3608333, 0.1825, 0.15, 1, 0.2, 0.5, 0.06,
                     0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
groups <- groups_raw %>% select(!h)
groups$w_max[17] <- 10628034; groups$w_mat[17] <- 3198855; groups$w_max[13] <- 450000
sp0 <- p0@species_params
for (col in c("w_min", "w_mat", "w_max", "beta", "sigma", "k_vb", "biomass_observed", "interaction_resource", "alpha", "n", "p")) {
  if (!col %in% names(groups) || !col %in% names(sp0)) { note(id, paste("column absent:", col)); next }
  a <- as.numeric(groups[[col]]); b <- as.numeric(sp0[[col]])
  d <- which(!(abs(a - b) <= 1e-9 * pmax(abs(a), abs(b)) | (is.na(a) & is.na(b))))
  if (!length(d)) note(id, paste("table+overrides vs first object:", col), "identical")
  else note(id, paste("table+overrides vs first object:", col),
            paste(sprintf("%s %s->%s", sp0$species[d], signif(a[d], 6), signif(b[d], 6)), collapse = "; "))
}
# mizer's silent size rewrites, simulated on the table the build actually used
source(repo_path("R", "check_size_params.R"))
cl <- simulate_mizer_clamps(groups$w_min, groups$w_mat, groups$w_max)
note(id, "mizer w_mat clamp (w_mat >= w_inf -> w_inf/4) fires for", paste(groups$species[cl$mat_clamped], collapse = ", "))
note(id, "mizer w_min clamp (w_min >= w_mat -> 0.001) fires for", paste(groups$species[cl$min_clamped], collapse = ", "))
save_results(id)
