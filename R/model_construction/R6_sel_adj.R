# R6_sel_adj.R -- replay 09_Uncertainty_Analysis.Rmd:1104-1444, the step that
# made params_sel_adj.rds (the Monte Carlo base), from its stored parent
# params_steady_state_2011_2020_tol_0.00025.RDS, the stored 1841-2010 forcing
# and IWC_data/catch_lengths.rds.
#
# Part (a) is arithmetic and must match exactly: upgradeTherParams() and the
# whale selectivity. Part (b) is the steady() solve, which ran under mizer
# 2.5.0 on 2025-09-07 and is replayed here under mizer 3.1.0: its difference is
# measured and reported, not passed or failed on a tight tolerance.
source(file.path(Sys.getenv("ASSESS_SRC", "R/model_construction"), "A0_helpers.R"))
suppressPackageStartupMessages({ library(mizer); library(therMizer) })
id <- "R6"

parent <- suppressWarnings(validParams(readRDS(repo_path("params_steady_state_2011_2020_tol_0.00025.RDS"))))
stored <- readRDS(repo_path("params_sel_adj.rds"))
extended_ocean_temp <- readRDS(repo_path("temperature_forcing_1841_2010.rds"))
extended_n_pp_array <- readRDS(repo_path("phytoplankton_forcing_1841_2010.rds"))

# 09:1105-1110
params_1841_2010_climate_only <- upgradeTherParams(parent,
  ocean_temp_array = extended_ocean_temp, n_pp_array = extended_n_pp_array,
  aerobic_effect = FALSE, metabolism_effect = TRUE)

# 09:1329, 1360-1400
catch_lengths <- readRDS(repo_path("IWC_data", "catch_lengths.rds"))
weighted_quantile <- function(x, w = NULL, probs = c(0.5), na.rm = TRUE) {
  if (is.null(w)) return(stats::quantile(x, probs = probs, na.rm = na.rm))
  if (all(is.na(w)) || length(w) != length(x)) return(stats::quantile(x, probs = probs, na.rm = na.rm))
  o <- order(x); x <- x[o]; w <- w[o]
  w <- ifelse(is.finite(w) & w >= 0, w, 0)
  cw <- cumsum(w)
  if (cw[length(cw)] == 0) return(stats::quantile(x, probs = probs, na.rm = na.rm))
  cw <- cw / cw[length(cw)]
  sapply(probs, function(p) { idx <- which(cw >= p)[1]; x[idx] })
}
whale_species <- c("baleen whales", "sperm whales", "orca", "minke whales")
species_group_col <- if ("species" %in% names(catch_lengths)) "species" else if ("Species" %in% names(catch_lengths)) "Species" else stop("no species column")
length_col <- intersect(names(catch_lengths), c("length", "Length", "L", "len"))[1]
count_col <- intersect(names(catch_lengths), c("catch", "Catch", "n", "N", "count", "Count", "numbers", "Numbers", "Number"))
count_col <- if (length(count_col) > 0) count_col[1] else NULL
note(id, "catch_lengths columns used", paste0("species=", species_group_col, " length=", length_col,
                                              " weight=", if (is.null(count_col)) "none (unweighted)" else count_col))
proposed_sel <- do.call(rbind, lapply(whale_species, function(sp) {
  df <- catch_lengths[catch_lengths[[species_group_col]] == sp, , drop = FALSE]
  if (!nrow(df)) return(NULL)
  L <- df[[length_col]]; W <- if (!is.null(count_col)) df[[count_col]] else NULL
  if (all(!is.finite(L))) return(data.frame(Species = sp, l50 = NA_real_, l25 = NA_real_))
  l50 <- as.numeric(weighted_quantile(L, W, probs = 0.6))
  gap <- max(0.02 * l50, 0.5)
  l25 <- max(suppressWarnings(min(L, na.rm = TRUE)), l50 - gap)
  data.frame(Species = sp, l50 = l50, l25 = l25)
}))
note(id, "proposed whale selectivity (cm)", paste(sprintf("%s l50=%g l25=%g", proposed_sel$Species,
                                                         proposed_sel$l50, proposed_sel$l25), collapse = "; "))

# 09:1403-1441
params_sel_adj <- params_1841_2010_climate_only
gp <- gear_params(params_sel_adj)
if (!all(c("l50", "l25") %in% colnames(gp))) {
  if (!"l50" %in% colnames(gp)) gp$l50 <- NA_real_
  if (!"l25" %in% colnames(gp)) gp$l25 <- NA_real_
}
has_species_col <- any(tolower(colnames(gp)) == "species")
species_col <- if (has_species_col) colnames(gp)[tolower(colnames(gp)) == "species"][1] else NULL
if (!"sel_func" %in% colnames(gp)) gp$sel_func <- if ("knife_edge_size" %in% colnames(gp)) "knife_edge" else NA_character_
for (i in seq_len(nrow(proposed_sel))) {
  sp <- proposed_sel$Species[i]; new_l50 <- proposed_sel$l50[i]; new_l25 <- proposed_sel$l25[i]
  if (is.na(new_l50) || is.na(new_l25)) next
  rows <- which(gp[[species_col]] == sp)
  if (length(rows) > 0) {
    gp$l50[rows] <- new_l50; gp$l25[rows] <- new_l25
    if ("sel_func" %in% colnames(gp)) gp$sel_func[rows] <- "sigmoid_length"
  }
}
gear_params(params_sel_adj) <- gp

# ---- (a) arithmetic part -------------------------------------------------------
sg <- stored@gear_params
for (col in c("sel_func", "l50", "l25", "catchability", "knife_edge_size")) {
  a <- gp[[col]]; b <- sg[[col]]
  ok <- if (is.numeric(a)) isTRUE(all.equal(a, b, tolerance = 0)) else identical(as.character(a), as.character(b))
  check(id, paste0("gear_params$", col, " identical to stored params_sel_adj"), ok)
}
so <- stored@other_params$other; ro <- params_sel_adj@other_params$other
for (nm in c("ocean_temp", "n_pp_array", "vertical_migration", "exposure", "t_idx")) {
  check(id, paste0("other_params$", nm, " identical to stored"),
        isTRUE(all.equal(unname(ro[[nm]]), unname(so[[nm]]), tolerance = 0)))
}
check(id, "rate functions identical to stored (Encounter, PredRate, EReproAndGrowth)",
      identical(params_sel_adj@rates_funcs[c("Encounter", "PredRate", "EReproAndGrowth")],
                stored@rates_funcs[c("Encounter", "PredRate", "EReproAndGrowth")]),
      paste(unlist(params_sel_adj@rates_funcs[c("Encounter", "PredRate", "EReproAndGrowth")]), collapse = ", "))
note(id, "aerobic_effect = FALSE at 09:1110, yet therMizerEncounter/PredRate remain (inherited from the parent)",
     paste(parent@rates_funcs$Encounter, parent@rates_funcs$PredRate))
check(id, "selectivity array identical to stored (before steady)",
      isTRUE(all.equal(unname(params_sel_adj@selectivity), unname(stored@selectivity), tolerance = 1e-14)),
      paste("max abs diff", fmt(max(abs(params_sel_adj@selectivity - stored@selectivity)))))

# ---- (b) steady() under mizer 3.1.0 -------------------------------------------------
t0 <- Sys.time()
msgs <- character(0)
replay <- withCallingHandlers(
  steady(params_sel_adj, tol = 0.002, t_max = 1000, preserve = c("erepro"), progress_bar = FALSE),
  message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
note(id, "steady() messages", paste(trimws(msgs), collapse = " | "))
note(id, "steady() wall time (min)", fmt(as.numeric(difftime(Sys.time(), t0, units = "mins"))))
check(id, "erepro preserved (identical to stored)",
      isTRUE(all.equal(replay@species_params$erepro, stored@species_params$erepro, tolerance = 0)))
note(id, "initial_n: replay vs stored, max rel diff", fmt(max_rel(replay@initial_n, stored@initial_n)))
note(id, "R_max: replay vs stored, max rel diff", fmt(max_rel(replay@species_params$R_max, stored@species_params$R_max)))
b_rep <- getBiomass(replay); b_st <- getBiomass(suppressWarnings(validParams(stored)))
rb <- abs(b_rep / b_st - 1)
note(id, "biomass: replay vs stored, max rel diff (species)",
     paste0(fmt(max(rb)), " (", names(rb)[which.max(rb)], ")"))
note(id, "stored params_sel_adj vs its parent: initial_n max rel diff (the 2025 steady() moved it by)",
     fmt(max_rel(stored@initial_n, parent@initial_n)))
saveRDS(replay, file.path(OUT, "R6_params_sel_adj_replay_mizer310.rds"))
save_results(id)
