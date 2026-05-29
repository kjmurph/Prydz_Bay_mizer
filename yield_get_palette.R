obs_stack <- readRDS("yield_cached_obs_stack.rds")
obs_raw_cols <- read.csv("yield_observed_timeseries.csv", nrows = 1)
fished_species <- gsub(".", " ", setdiff(names(obs_raw_cols), "Year"), fixed = TRUE)
fished_species <- fished_species[fished_species %in% as.character(unique(obs_stack[["Species"]]))]
cat("n fished species:", length(fished_species), "\n")
sp_pal <- setNames(scales::hue_pal()(length(fished_species)), fished_species)
cat(paste(names(sp_pal), sp_pal, sep = " = "), sep = "\n")
