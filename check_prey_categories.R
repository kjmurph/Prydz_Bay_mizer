library(therMizer)

mc <- readRDS('Output_large_files/monte_carlo_results/combined_simulation_results/rerun_results/mc_ensemble_2111_cleaned.rds')

if ('simulations' %in% names(mc)) {
  sim <- mc$simulations[[1]]
} else {
  sim <- mc[[1]]
}

params <- sim@params
n <- sim@n[1,,]
n_pp <- sim@n_pp[1,]
n_other <- sim@n_other[1,]
if (!is.null(dimnames(sim@n_other))) {
  names(n_other) <- dimnames(sim@n_other)$component
}

diet <- getDiet(params, n=n, n_pp=n_pp, n_other=n_other, proportion=FALSE)

cat('Prey categories in getDiet():\n')
print(dimnames(diet)[[3]])
cat('\nNumber of prey categories:', length(dimnames(diet)[[3]]), '\n')
