# =============================================================================
# Unit checks for the parameter levers.  No projections -- pure arithmetic and
# constraint checks, so this runs in seconds and can be re-run after any edit.
#
#   Rscript R/catch_fit/tests_levers.R [sim_index]
# =============================================================================

source("R/catch_fit/01_harness.R")

args <- commandArgs(trailingOnly = TRUE)
si <- if (length(args)) as.integer(args[1]) else 446L

m  <- cf_load_members(si)[[1]]
p  <- m$params
sp <- p@species_params
G  <- "minke whales"
other <- setdiff(sp$species, G)
i1 <- match(other, sp$species)

untouched <- function(p2) {
  i2 <- match(other, p2@species_params$species)
  identical(sp$erepro[i1], p2@species_params$erepro[i2]) &&
    identical(sp$R_max[i1],  p2@species_params$R_max[i2]) &&
    identical(p@initial_n,   p2@initial_n) &&
    identical(p@rates_funcs, p2@rates_funcs)
}

fail <- 0L
chk <- function(ok, msg) {
  cat(if (ok) "  ok   " else "  FAIL ", msg, "\n", sep = "")
  if (!ok) fail <<- fail + 1L
}

cat("=== member ", si, " ===\n", sep = "")
cat("\nreproduction state, all 19 groups:\n")
print(data.frame(species = sp$species,
                 erepro = signif(sp$erepro, 4),
                 R_max = signif(sp$R_max, 4),
                 repro_level = round(getReproductionLevel(p), 5),
                 RDI = signif(getRDI(p), 4),
                 RDD = signif(getRDD(p), 4)), row.names = FALSE)

cat("\n=== LEVER B: reproduction_level at fixed RDD ===\n")
rdd0 <- unname(getRDD(p)[G])
for (rl in c(0.75, 0.5, 0.25)) {
  p2 <- cf_set_repro_level(p, G, rl)
  st <- cf_repro_state(p2, G)
  cat(sprintf("rl=%.2f -> achieved %.6f | erepro %.4g (was %.4g) | R_max %.4g (was %.4g) | RDD %.6g (was %.6g)\n",
              rl, st[["repro_level"]], st[["erepro"]], sp$erepro[sp$species == G],
              st[["R_max"]], sp$R_max[sp$species == G], st[["rdd"]], rdd0))
  chk(abs(st[["repro_level"]] - rl) < 1e-8, sprintf("achieved level == %.2f", rl))
  chk(abs(st[["rdd"]] / rdd0 - 1) < 1e-8,  "RDD preserved")
  chk(st[["erepro"]] < sp$erepro[sp$species == G], "erepro falls (constraint-safe)")
  chk(st[["max_erepro_all"]] < 1, "erepro < 1 for all 19")
  chk(st[["n_rmax_inf_all"]] == 0, "R_max finite for all 19")
  chk(untouched(p2), "other 18 groups + initial_n + rates_funcs bit-identical")
}

cat("\n=== LEVER A: R_max multiplier at fixed erepro ===\n")
for (k in c(1, 2, 5, 10, 100, 1000)) {
  p2 <- cf_scale_rmax(p, G, k)
  st <- cf_repro_state(p2, G)
  cat(sprintf("k=%-6g R_max %.5g | RDD %.6g (x%.5f) | repro_level %.5f | erepro %.4g\n",
              k, st[["R_max"]], st[["rdd"]], st[["rdd"]] / rdd0,
              st[["repro_level"]], st[["erepro"]]))
  chk(abs(st[["erepro"]] - sp$erepro[sp$species == G]) < 1e-15, "erepro untouched")
  chk(st[["max_erepro_all"]] < 1, "erepro < 1 for all 19")
  chk(st[["n_rmax_inf_all"]] == 0, "R_max finite for all 19")
  chk(untouched(p2), "other 18 groups + initial_n + rates_funcs bit-identical")
  if (k == 1) chk(abs(st[["rdd"]] / rdd0 - 1) < 1e-12, "k=1 is the exact control")
}

cat("\n=== the RDD ceiling: RDD -> RDI as R_max -> Inf ===\n")
for (g in c("minke whales", "sperm whales", "baleen whales", "orca",
            "toothfishes", "shelf and coastal fishes")) {
  cat(sprintf("%-26s RDI %.5g  RDD %.5g  headroom RDI/RDD = %.4g\n",
              g, getRDI(p)[[g]], getRDD(p)[[g]], getRDI(p)[[g]] / getRDD(p)[[g]]))
}

cat("\n=== the setBevertonHolt trap (documented, must still bite) ===\n")
rl_all <- getReproductionLevel(p)
p_bad <- suppressWarnings(setBevertonHolt(p, reproduction_level = rl_all))
cat(sprintf("passing the UNCHANGED full-length level vector back: max erepro %.5f (was %.5f), orca %.5f -> %.5f\n",
            max(p_bad@species_params$erepro), max(sp$erepro),
            sp$erepro[sp$species == "orca"],
            p_bad@species_params$erepro[p_bad@species_params$species == "orca"]))
chk(max(p_bad@species_params$erepro) >= 1,
    "confirms the trap: the full-vector call breaches erepro < 1")

cat("\n", if (fail == 0L) "ALL CHECKS PASSED" else paste(fail, "CHECKS FAILED"), "\n")
if (fail > 0L) quit(save = "no", status = 1L)
