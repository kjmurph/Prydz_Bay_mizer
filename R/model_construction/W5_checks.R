# W5_checks.R -- quantitative checks of methods claims (supplement S1, S2,
# S6, S7, S8; main text 2.2) against params_sel_adj and its inputs.
source(file.path(Sys.getenv("ASSESS_SRC", "R/model_construction"), "A0_helpers.R"))
suppressPackageStartupMessages({ library(mizer); library(therMizer); library(sf) })
id <- "W5"
sel <- suppressWarnings(validParams(readRDS(repo_path("params_sel_adj.rds"))))
sp <- species_params(sel)

# S1: domain area
bb <- readRDS(repo_path("model_domains", "Stacey", "BanzareBank.rds"))
note(id, "BanzareBank.rds class", paste(class(bb), collapse = ","))
ar <- tryCatch(sum(as.numeric(sf::st_area(sf::st_as_sf(bb)))), error = function(e) NA)
note(id, "S1 domain: area of BanzareBank.rds polygon (m2)", fmt(ar))
note(id, "S1 domain: biomass_observed / McCormack g m-2 (implied area)",
     fmt(unique(signif(sp$biomass_observed / c(8.8, 1.9, 10, 4, 0.652, 1.2, 1.2, 2.732, 0.003, 0.016,
                                               0.15, 0.75, 0.002, 0.265, 0.011, 0.014, 0.006, 0.011, 0.127), 7))))

# S2: size grid
note(id, "S2 consumer grid", paste0(length(w(sel)), " bins, ", fmt(min(w(sel))), " to ", fmt(max(w(sel))),
                                    " g, ", fmt(log10(max(w(sel)) / min(w(sel)))), " decades"))
note(id, "S2 resource grid", paste0(length(w_full(sel)), " classes from ", fmt(min(w_full(sel))), " g"))

# S6.3: realised temperature statistics, several definitions
Tm <- sel@other_params$other$ocean_temp
yrs <- as.character(1961:2010)
note(id, "S6.3 median/range, all realms, 1961-2010",
     paste0(fmt(median(Tm[yrs, ])), "; ", paste(fmt(range(Tm[yrs, ])), collapse = " to ")))
note(id, "S6.3 median/range, all realms, 1841-2010",
     paste0(fmt(median(Tm)), "; ", paste(fmt(range(Tm)), collapse = " to ")))
note(id, "S6.3 median of tos 1961-2010; of tob", paste(fmt(median(Tm[yrs, "tos"])), fmt(median(Tm[yrs, "tob"])), sep = "; "))
note(id, "S6.3 vertical residence used", paste("unique values", paste(unique(as.vector(sel@other_params$other$vertical_migration)), collapse = ",")))

# S6.4: initialisation of the resource, and the level of the forcing
npp <- sel@other_params$other$n_pp_array
below <- w_full(sel) < resource_params(sel)$w_pp_cutoff
m0010 <- 10^colMeans(npp[as.character(2000:2010), below]) / sel@dw_full[below]
note(id, "S6.4 initial_n_pp vs 10^mean(forcing 2000-2010)/dw, max rel diff",
     fmt(max(abs(initialNResource(sel)[below] / m0010 - 1))))
m6110 <- 10^colMeans(npp[yrs, below]) / sel@dw_full[below]
note(id, "S6.4 initial_n_pp vs 10^mean(forcing 1961-2010)/dw, max rel diff",
     fmt(max(abs(initialNResource(sel)[below] / m6110 - 1))))
dat <- as.matrix(read.table(repo_path("GFDL_resource_spectra_annual.dat")))
a1961 <- dat[1, ] - colMeans(dat)
note(id, "S6.4 size of the ESM-1961 anomaly that the calibrated level carries (log10, w < cutoff)",
     paste(fmt(range(a1961[below])), collapse = " to "))
note(id, "S6.4 resource cutoff w_pp_cutoff (g)", resource_params(sel)$w_pp_cutoff)

# S7.2: selectivity of the non-whale gears
gp <- gear_params(sel)
ke <- gp$sel_func == "knife_edge"
wm <- sp$w_mat[match(gp$species, sp$species)]
kd <- ke & abs(gp$knife_edge_size / wm - 1) >= 1e-12
note(id, "S7.2 knife_edge_size == w_mat for every knife-edge gear",
     paste(!any(kd), "| fished knife-edge gears:",
           paste(gp$species[ke & gp$catchability > 0], collapse = ", ")))
if (any(kd)) note(id, "S7.2 knife-edge gears where knife_edge_size != w_mat",
                  paste(sprintf("%s: knife %s vs w_mat %s (rel %s)", gp$species[kd], fmt(gp$knife_edge_size[kd]),
                                fmt(wm[kd]), fmt(gp$knife_edge_size[kd] / wm[kd] - 1)), collapse = "; "))
note(id, "S7.3 catchability carried by params_sel_adj (fished gears)",
     paste(sprintf("%s=%s", gp$species[gp$catchability > 0], fmt(gp$catchability[gp$catchability > 0])), collapse = "; "))
note(id, "initial_effort carried by params_sel_adj (all zero?)", all(initial_effort(sel) == 0))

# S8: biomass targets in params_sel_adj (g m-2) vs the published list
pub <- c(8.8, 1.9, 10, 4, 0.652, 1.2, 1.2, 2.732, 0.003, 0.016, 0.15, 0.75, 0.002, 0.265, 0.011, 0.014, 0.006, 0.011, 0.127)
note(id, "S8 biomass_observed / 1.474341e12 equals the published g m-2 list",
     paste("max rel diff", fmt(max(abs(sp$biomass_observed / 1.474341e12 / pub - 1)))))
bm <- getBiomass(sel, use_cutoff = TRUE)
note(id, "params_sel_adj biomass fit on the cutoff basis: max |model/observed - 1|",
     paste0(fmt(max(abs(bm / sp$biomass_observed - 1))), " (", names(bm)[which.max(abs(bm / sp$biomass_observed - 1))], ")"))
note(id, "params_sel_adj max erepro", paste0(fmt(max(sp$erepro)), " (", sp$species[which.max(sp$erepro)], ")"))
note(id, "params_sel_adj reproduction level range", paste(fmt(range(getReproductionLevel(sel))), collapse = " to "))
note(id, "params_sel_adj kernel types", paste(sprintf("%s=%s", sp$species[sp$pred_kernel_type != "lognormal"],
                                                     sp$pred_kernel_type[sp$pred_kernel_type != "lognormal"]), collapse = "; "))
save_results(id)
