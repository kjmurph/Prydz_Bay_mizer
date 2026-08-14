# =============================================================================
# STAGE B, part 1 -- is a physically valid penguin recalibration even reachable
# at w_min = 3626.667 g? Analytic, no steady(), runs in seconds.
#
# WHY. Stage 1 (scripts 19-24) ran steady() with the workflow's
# preserve = "erepro", which holds erepro fixed and lets R_max absorb the change.
# For small divers that drove R_max to Inf -- density dependence removed
# entirely -- and the group still decayed with a 128-404 yr half-life. That is a
# recalibration failure, not a model result.
#
# The correct fix is to recalibrate the penguin reproduction PAIR (erepro and
# R_max) at the new w_min. Two hard constraints:
#   1. erepro < 1 for ALL 19 groups. It is an efficiency, not a rate.
#   2. R_max must stay finite, or the group has no density dependence.
#
# Headroom is tight elsewhere: orca already sits at erepro = 0.9509.
#
# THE ARITHMETIC. mizer computes
#     RDI = 0.5 * (E_repro %*% dw) * erepro / w[w_min_idx]
# so at fixed reproductive output RDI scales as erepro / w_min. Raising w_min
# from bin 29 (7.72e-4 g) to bin 71 (2942 g) divides RDI by ~3.8e6. The question
# is whether the lost factor can be bought back with erepro <= 1.
#
# For each pilot member this reports:
#   erepro_needed   erepro that restores the CONTROL recruitment RDD at the new
#                   w_min with no density dependence -- the floor. Must be < 1.
#   repro_level_max density dependence still affordable at erepro = 1 -- the
#                   ceiling on how much of the calibrated clamp can be retained.
#
# Pilot members are DISTINCT models: 208 of the 2111 are exact duplicates
# (script 25), so the earlier "top 5" pilot was really n = 3.
#
# Writes Output_large_files/wmin_test/26_repro_feasibility.{rds,csv}
# =============================================================================

suppressPackageStartupMessages({
  library(therMizer)
  library(mizer)
  library(dplyr)
})

out_dir <- "Output_large_files/wmin_test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

PEN       <- "small divers"
W_NEW     <- 3626.667
N_MEMBERS <- 6

cat("=== Stage B part 1: reproduction feasibility ===\n")
cat("mizer", as.character(packageVersion("mizer")),
    "| therMizer", as.character(packageVersion("therMizer")), "\n")
cat("target w_min:", W_NEW, "g\n\n")

ens <- readRDS("Output_large_files/community_slope_analysis/top10pct_rmse_ensembles.rds")
idx <- ens$metadata$top10pct_indices

# --- distinct pilot members --------------------------------------------------
rmse_all <- read.csv("Manuscript data/yield_rmse_per_sim.csv") %>% arrange(rank)
top212 <- rmse_all[seq_along(idx), ]
stopifnot(identical(as.integer(top212$sim_index), as.integer(idx)))
top212$dup_key <- paste(top212$rmse, top212$cor_raw, top212$cor_log)
distinct_first <- top212[!duplicated(top212$dup_key), ]
cat("top-10% members:", nrow(top212), "| distinct models:", nrow(distinct_first),
    "| duplicates removed:", nrow(top212) - nrow(distinct_first), "\n")

pilot <- head(distinct_first, N_MEMBERS)
pilot$position <- match(pilot$sim_index, idx)
cat("pilot (position / sim_index / rmse):\n")
for (k in seq_len(nrow(pilot)))
  cat(sprintf("   m=%3d  sim=%4d  rmse=%.6f\n",
              pilot$position[k], pilot$sim_index[k], pilot$rmse[k]))
cat("\n")

# --- set the corrected w_min -------------------------------------------------
# setParams() rebuilds w_min_idx but RESETS rates_funcs (dropping therMizer) and
# does not re-zero initial_n below the new w_min. Restore ONLY the three
# therMizer entries -- replacing the whole list drops mizer 3.x's Diffusion entry
# and projection fails later. (18_test_wmin_change.R)
set_wmin <- function(p, species, w_new) {
  rf <- p@rates_funcs; rd <- p@resource_dynamics
  i <- which(p@species_params$species == species)
  sp <- p@species_params; sp$w_min[i] <- w_new; p@species_params <- sp
  p <- suppressWarnings(setParams(p))
  ther <- intersect(c("Encounter", "PredRate", "EReproAndGrowth"), names(rf))
  p@rates_funcs[ther] <- rf[ther]
  p@resource_dynamics <- rd
  p@initial_n[i, p@w < w_new] <- 0
  stopifnot(p@w_min_idx[[i]] == max(which(p@w <= w_new)),
            identical(p@rates_funcs$Encounter, "therMizerEncounter"))
  p
}

rates_at_init <- function(p) {
  getRates(p, n = p@initial_n, n_pp = p@initial_n_pp,
           n_other = p@initial_n_other, effort = 0, t = 1841)
}

rows <- list()
for (k in seq_len(nrow(pilot))) {
  m <- pilot$position[k]; si <- pilot$sim_index[k]
  p_c <- ens$fished_top10pct[[m]]@params
  i <- which(p_c@species_params$species == PEN)

  p_t <- set_wmin(p_c, PEN, W_NEW)

  r_c <- rates_at_init(p_c)
  r_t <- rates_at_init(p_t)

  erepro_c <- p_c@species_params$erepro[i]
  R_max_c  <- p_c@species_params$R_max[i]
  rdi_c <- r_c$rdi[i]; rdd_c <- r_c$rdd[i]
  rdi_t <- r_t$rdi[i]            # at the SAME erepro, new w_min

  # RDI is linear in erepro, so scale directly.
  erepro_needed   <- erepro_c * rdd_c / rdi_t          # floor: restores RDD, R_max = Inf
  rdi_at_erepro1  <- rdi_t / erepro_c                  # ceiling on RDI
  repro_level_max <- 1 - rdd_c / rdi_at_erepro1        # density dependence affordable

  rows[[k]] <- data.frame(
    member = k, position = m, sim_index = si,
    w_min_idx_ctrl = p_c@w_min_idx[[i]],
    w_min_idx_trt  = p_t@w_min_idx[[i]],
    w_ctrl = p_c@w[p_c@w_min_idx[[i]]],
    w_trt  = p_t@w[p_t@w_min_idx[[i]]],
    bins_ctrl = sum(p_c@w >= p_c@w[p_c@w_min_idx[[i]]] &
                      p_c@w <= p_c@species_params$w_max[i]),
    bins_trt  = sum(p_t@w >= p_t@w[p_t@w_min_idx[[i]]] &
                      p_t@w <= p_t@species_params$w_max[i]),
    erepro_ctrl = erepro_c, R_max_ctrl = R_max_c,
    rdi_ctrl = rdi_c, rdd_ctrl = rdd_c,
    repro_level_ctrl = 1 - rdd_c / rdi_c,
    rdi_trt_same_erepro = rdi_t,
    rdi_ratio = rdi_t / rdi_c,
    erepro_needed = erepro_needed,
    feasible = erepro_needed < 1,
    repro_level_max = repro_level_max)

  cat(sprintf("m=%d (sim %4d): bins %d -> %d | RDI x%.3e | erepro %.4e -> %.4e needed %s | max repro_level %.4f (was %.4f)\n",
              k, si, rows[[k]]$bins_ctrl, rows[[k]]$bins_trt,
              rows[[k]]$rdi_ratio, erepro_c, erepro_needed,
              if (erepro_needed < 1) "OK" else "INFEASIBLE",
              repro_level_max, rows[[k]]$repro_level_ctrl))
}

feas <- bind_rows(rows)
saveRDS(feas, file.path(out_dir, "26_repro_feasibility.rds"))
write.csv(feas, file.path(out_dir, "26_repro_feasibility.csv"), row.names = FALSE)

cat("\n================ VERDICT ================\n")
cat("members where erepro_needed < 1 :", sum(feas$feasible), "/", nrow(feas), "\n")
cat("erepro_needed  median", signif(median(feas$erepro_needed), 4),
    " range [", signif(min(feas$erepro_needed), 4), ",",
    signif(max(feas$erepro_needed), 4), "]\n")
cat("repro_level_max median", signif(median(feas$repro_level_max), 4),
    " (control", signif(median(feas$repro_level_ctrl), 4), ")\n")
cat("penguin size bins:", unique(feas$bins_ctrl), "->", unique(feas$bins_trt), "\n")

if (all(feas$feasible)) {
  cat("\nA valid recalibration EXISTS for every pilot member: erepro stays below 1\n",
      "with room to retain some density dependence. Proceed to the steady() run.\n")
} else {
  cat("\nNOT reachable for", sum(!feas$feasible), "member(s): sustaining the calibrated\n",
      "recruitment at this w_min would need erepro > 1, which is unphysical.\n",
      "The group cannot be recalibrated at 3626.667 g on this w grid.\n")
}

# --- erepro headroom for the other 18 groups ---------------------------------
cat("\n=== erepro headroom, all 19 groups (member 1) ===\n")
sp1 <- ens$fished_top10pct[[pilot$position[1]]]@params@species_params
h <- data.frame(species = sp1$species, erepro = signif(sp1$erepro, 4)) %>%
  arrange(desc(erepro))
print(head(h, 6), row.names = FALSE)
cat("max erepro across groups:", signif(max(sp1$erepro), 4),
    "(", sp1$species[which.max(sp1$erepro)], ") -- headroom to 1:",
    signif(1 - max(sp1$erepro), 4), "\n")
cat("\nHolding preserve = 'erepro' keeps these 18 untouched; only the penguin\n",
    "pair is retuned. That is the design that cannot breach erepro >= 1 elsewhere.\n")
