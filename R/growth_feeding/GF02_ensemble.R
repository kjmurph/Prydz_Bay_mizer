# =============================================================================
# GF02 -- growth curves and feeding levels ACROSS THE ENSEMBLE
#
# Median and IQR across members, per species. Two figures:
#   growth_curves_<SUF>    size at age, median line + 25-75% ribbon
#   feeding_level_<SUF>    feeding level vs body mass, median + 25-75% ribbon
#
# ----------------------------------------------------------------- MEMBER SET
# Members come from the RANKING object's cuts, so the set is the same one every
# other figure uses and cannot drift: `FULL usable` (203) by default, the TOP
# cut (20) with GF_SET=top. Never the raw state directory listing -- that
# includes members which failed the stability, drift or admissibility screens.
#
# ------------------------------------------------------------------- MEDIANS
# MEDIAN AND IQR, NOT MEAN AND SD. A single divergent member has owned 83% of an
# across-member sum in this ensemble before; the median is the robust summary
# and the one the rest of the manuscript set uses.
#
# ------------------------------------------------------------------- THE GRID
# Members share the base model's size grid, so feeding levels stack columnwise.
# That is CHECKED, not assumed -- a mismatch would silently average across
# different body masses.
#
# Growth curves are integrated per member on a common age grid, so they stack
# directly. Each member's own w_max is used, as in GF00.
#
# USAGE  Rscript R/growth_feeding/GF02_ensemble.R
# ENV    GF_STATE_DIR, GF_RANK, GF_SET (full|top), GF_SUF, GF_OUT,
#        GF_MAX_AGE, GF_T, GF_CORES, GF_LIMIT
# =============================================================================

source("R/growth_feeding/GF00_common.R")
suppressPackageStartupMessages(library(parallel))

OL        <- "Output_large_files/wmin_test"
STATE_DIR <- Sys.getenv("GF_STATE_DIR", file.path(OL, "104_full_states"))
RANK_F    <- Sys.getenv("GF_RANK", file.path(OL, "104_q10_rerank.rds"))
SET       <- tolower(Sys.getenv("GF_SET", "full"))
MAX_AGE   <- as.numeric(Sys.getenv("GF_MAX_AGE", "20"))
TT        <- as.numeric(Sys.getenv("GF_T", "0"))
LIMIT     <- as.integer(Sys.getenv("GF_LIMIT", "0"))
OUT       <- Sys.getenv("GF_OUT", file.path("Manuscript figures", "p104 figures",
                                            "Growth and feeding"))
# Never commit all 16 cores -- saturating them risks a hard shutdown that kills
# the in-flight run too.
CORES <- as.integer(Sys.getenv("GF_CORES",
                               as.character(max(1, detectCores() - 2))))
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
stopifnot(SET %in% c("full", "top"))

if (!dir.exists(STATE_DIR))
  stop("state directory absent:\n  ", STATE_DIR,
       "\nThe phase-104 states are VM-only (~300 MB, see AGENTS.md). Run this ",
       "on the VM, or point GF_STATE_DIR at a local copy.", call. = FALSE)
if (!file.exists(RANK_F)) stop("missing ranking: ", RANK_F, call. = FALSE)

RR <- readRDS(RANK_F)
cut_nm <- if (SET == "top") grep("^TOP ", names(RR$cuts), value = TRUE)[1] else "FULL usable"
members <- as.integer(RR$cuts[[cut_nm]])
if (LIMIT > 0) members <- head(members, LIMIT)

SUF <- Sys.getenv("GF_SUF", sprintf("p104q10%s", if (SET == "top") "top20" else "n203"))

cat("=== GF02: ensemble growth and feeding ===\n")
cat("states :", STATE_DIR, "\n")
cat("cut    : '", cut_nm, "' -> ", length(members), " members\n", sep = "")
cat("suffix :", SUF, "| max_age:", MAX_AGE, "| t:", TT, "| cores:", CORES, "\n")

have <- file.exists(vapply(members, function(s) state_path(STATE_DIR, s), character(1)))
if (!all(have))
  stop("missing states for ", sum(!have), " members, e.g. ",
       paste(head(members[!have], 3), collapse = ", "), call. = FALSE)

# --- per-member extraction -----------------------------------------------------
worker <- function(si) {
  p <- load_member(state_path(STATE_DIR, si))
  list(sim_index = si,
       gc = suppressWarnings(growth_curves(p, max_age = MAX_AGE, t = TT)),
       fl = feeding_level(p, t = TT),
       w  = p@w,
       species = p@species_params$species)
}

t0 <- Sys.time()
if (CORES > 1) {
  cl <- makeCluster(CORES)
  on.exit(stopCluster(cl), add = TRUE)
  clusterEvalQ(cl, suppressPackageStartupMessages({
    library(mizer); library(therMizer); library(deSolve)
  }))
  clusterExport(cl, c("STATE_DIR", "MAX_AGE", "TT", "growth_curves",
                      "feeding_level", "load_member", "state_path"),
                envir = environment())
  Z <- parLapply(cl, members, worker)
} else Z <- lapply(members, worker)
cat("extracted", length(Z), "members in",
    round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")

# --- the grid must be common ----------------------------------------------------
w_ref <- Z[[1]]$w; sp <- Z[[1]]$species
same <- vapply(Z, function(z) identical(z$w, w_ref) && identical(z$species, sp),
               logical(1))
if (!all(same))
  stop("members ", paste(members[!same], collapse = ", "),
       " carry a different size grid or species order -- refusing to stack",
       call. = FALSE)

# --- growth: median and IQR across members -------------------------------------
age <- as.numeric(dimnames(Z[[1]]$gc)$Age)
GA <- array(unlist(lapply(Z, `[[`, "gc")),
            dim = c(length(sp), length(age), length(Z)))
qs <- function(A, probs) apply(A, c(1, 2), quantile, probs = probs, na.rm = TRUE)
gd <- expand.grid(Species = sp, Age = age, stringsAsFactors = FALSE)
gd$med <- as.vector(qs(GA, .5)); gd$lo <- as.vector(qs(GA, .25))
gd$hi  <- as.vector(qs(GA, .75))
gd$Species <- factor(gd$Species, levels = sp)

# vB depends only on a/b/k_vb/w_inf, which the draws do not perturb -- VERIFIED
# identical across members below -- so it is ONE line, not a ribbon.
p1 <- load_member(state_path(STATE_DIR, members[1]))
VB <- vb_curve(p1, age)
if (is.null(VB)) stop("species_params lacks a/b/k_vb/w_inf", call. = FALSE)
vb_same <- vapply(members[seq_len(min(10, length(members)))], function(s) {
  identical(vb_curve(load_member(state_path(STATE_DIR, s)), age)$w_vb, VB$w_vb)
}, logical(1))
if (!all(vb_same))
  stop("von Bertalanffy parameters differ across members -- a single curve ",
       "would misrepresent them", call. = FALSE)
VB$Species <- factor(VB$Species, levels = sp)
n_round <- length(unique(VB$Species[VB$k_vb_round]))

p_g <- ggplot(mapping = aes(Age)) +
  geom_ribbon(data = gd, aes(ymin = lo, ymax = hi, fill = Species),
              alpha = .25, colour = NA) +
  geom_line(data = VB, aes(y = w_vb, linetype = vb_class), colour = "grey25",
            linewidth = .7) +
  geom_line(data = gd, aes(y = med, colour = Species), linewidth = 1) +
  facet_wrap(~ Species, scales = "free_y", ncol = 4) +
  scale_colour_manual(values = sp_cols, guide = "none") +
  scale_fill_manual(values = sp_cols, guide = "none") +
  scale_linetype_manual(values = c("von Bertalanffy (fitted k_vb)" = "dashed",
                                   "von Bertalanffy (round-default k_vb)" = "dotted"),
                        name = NULL) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si("g"))) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank()) +
  labs(x = "Age (years)", y = "Body mass",
       title = "Ensemble growth curves",
       subtitle = sprintf(paste0("%s | %d members (%s) | coloured = median with IQR",
         " ribbon, grey = von Bertalanffy (identical across members)",
         "\nk_vb is a ROUND DEFAULT for %d of %d groups (dotted vB): for those the",
         " vB curve is a placeholder, not independent data | model year 1841"),
         SUF, length(Z), cut_nm, n_round, length(sp)))

ggsave(file.path(OUT, sprintf("growth_curves_%s.png", SUF)), p_g,
       width = 13, height = 10, dpi = 300)
ggsave(file.path(OUT, sprintf("growth_curves_%s.pdf", SUF)), p_g,
       width = 13, height = 10)

# --- feeding level: median and IQR ---------------------------------------------
FA <- array(unlist(lapply(Z, `[[`, "fl")),
            dim = c(length(sp), length(w_ref), length(Z)))
fd <- expand.grid(Species = sp, w = w_ref, stringsAsFactors = FALSE)
fd$med <- as.vector(qs(FA, .5)); fd$lo <- as.vector(qs(FA, .25))
fd$hi  <- as.vector(qs(FA, .75))
# mask outside [w_min, w_max], as GF01 does
wmin <- p1@w[p1@w_min_idx]; wmax <- p1@species_params$w_max
fd <- fd |>
  mutate(keep = w >= wmin[match(Species, sp)] & w <= wmax[match(Species, sp)]) |>
  filter(keep) |> mutate(Species = factor(Species, levels = sp))

# f_crit is metab/intake_max/alpha -- physiology only, untouched by the draws, so
# it too is ONE line. Checked against members rather than assumed.
CF <- cfl_long(p1) |> mutate(Species = factor(Species, levels = sp))
cf_ref <- critical_feeding_level(p1)
cf_same <- vapply(members[seq_len(min(10, length(members)))], function(s)
  max(abs(critical_feeding_level(load_member(state_path(STATE_DIR, s))) - cf_ref)),
  numeric(1))
if (max(cf_same) > 0)
  stop("critical feeding level differs across members (max ", max(cf_same),
       ") -- it needs a ribbon, not a single line", call. = FALSE)

p_f <- ggplot(mapping = aes(w)) +
  geom_ribbon(data = fd, aes(ymin = lo, ymax = hi, fill = Species),
              alpha = .25, colour = NA) +
  geom_line(data = CF, aes(y = f_crit, linetype = "Critical feeding level"),
            colour = "grey25", linewidth = .7) +
  geom_line(data = fd, aes(y = med, colour = Species), linewidth = 1) +
  facet_wrap(~ Species, scales = "free_x", ncol = 4) +
  scale_colour_manual(values = sp_cols, guide = "none") +
  scale_fill_manual(values = sp_cols, guide = "none") +
  scale_linetype_manual(values = c("Critical feeding level" = "dashed"),
                        name = NULL) +
  scale_x_log10(labels = scales::label_number(scale_cut = scales::cut_si("g"))) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank()) +
  labs(x = "Body mass (g)", y = "Feeding level",
       title = "Ensemble feeding levels",
       subtitle = sprintf(paste0("%s | %d members (%s) | coloured = median with",
         " IQR ribbon\ngrey dashed = critical feeding level, below which growth",
         " is zero (identical across members) | model year 1841"),
         SUF, length(Z), cut_nm))

ggsave(file.path(OUT, sprintf("feeding_level_%s.png", SUF)), p_f,
       width = 13, height = 10, dpi = 300)
ggsave(file.path(OUT, sprintf("feeding_level_%s.pdf", SUF)), p_f,
       width = 13, height = 10)

saveRDS(list(growth = gd, feeding = fd, vb = VB, critical = CF,
             members = members, cut = cut_nm,
             state_dir = STATE_DIR, max_age = MAX_AGE, t = TT,
             built = Sys.time()),
        file.path(OL, sprintf("GF02_ensemble_%s.rds", SUF)))
cat("\nWrote 2 figures to", OUT, "and GF02_ensemble_", SUF, ".rds\n", sep = "")