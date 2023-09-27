library(mizer)
params <- NS_params
# works only if predation kernel is supplied at the moment. If it's not run this first:
pred_kernel(params) <- pred_kernel(params)
getDietComp <- function (params, n = initialN(params), n_pp = initialNResource(params),
                      n_other = initialNOther(params), proportion = TRUE)
{
  params <- validParams(params)
  species <- params@species_params$species
  no_sp <- length(species)
  no_w <- length(params@w)
  no_w_full <- length(params@w_full)
  no_other <- length(params@other_encounter)
  other_names <- names(params@other_encounter)
  assertthat::assert_that(identical(dim(n), c(no_sp, no_w)), length(n_pp) ==
                no_w_full)

  idx_sp <- (no_w_full - no_w + 1):no_w_full
  if (!is.null(comment(params@pred_kernel))) {
    ## code to keep prey size | works only without other backgrounds now
    n_tot <- sweep(n, 2, params@w * params@dw, "*")
    diet_comp <- array(0, dim = c(dim(n_tot),no_sp + 1, no_w_full),
                       dimnames = list("Predator" = species,
                                       "wPredator" = params@w,
                                       "Prey" = c(as.character(species), "Resource"),#, other_names),
                                       "wPrey" = params@w_full))
    for(iPredator in 1:dim(diet_comp)[1])
    {
      for(wPredator in 1:dim(diet_comp)[2])
      {
        for(iPrey in 1:no_sp)
        {
          for(wPrey in 1:no_w) # assuming that w is the tail of w_full, won't work if w_full gets larger than w
          {
            diet_comp[iPredator,wPredator,iPrey,wPrey] <- params@pred_kernel[iPredator,wPredator, (idx_sp[wPrey])] *
              n_tot[iPrey,wPrey]
            
          }
        }
      }
    }
    diet_comp[,,no_sp+1,] <- sweep(params@pred_kernel,
                                   3, params@dw_full * params@w_full * n_pp, "*")
    
    ###

  }
  else {
    prey <- matrix(0, nrow = no_sp + 1, ncol = no_w_full)
    prey[1:no_sp, idx_sp] <- sweep(n, 2, params@w * params@dw,
                                   "*")
    prey[no_sp + 1, ] <- n_pp * params@w_full * params@dw_full
    ft <- array(rep(params@ft_pred_kernel_e, times = no_sp +
                      1) * rep(mvfft(t(prey)), each = no_sp), dim = c(no_sp,
                                                                      no_w_full, no_sp + 1))
    ft <- matrix(aperm(ft, c(2, 1, 3)), nrow = no_w_full)
    ae <- array(Re(mvfft(ft, inverse = TRUE)/no_w_full),
                dim = c(no_w_full, no_sp, no_sp + 1))
    ae <- ae[idx_sp, , , drop = FALSE]
    ae <- aperm(ae, c(2, 1, 3))
    ae[ae < 1e-18] <- 0
    diet[, , 1:(no_sp + 1)] <- ae
  }
  
  inter <- cbind(params@interaction, params@species_params$interaction_resource)

  diet_comp[, , 1:(no_sp + 1),] <- sweep(sweep(diet_comp[, , 1:(no_sp + 1),, drop = FALSE], c(1, 3), inter, "*"), c(1, 2), params@search_vol, "*")
  
  for (i in seq_along(params@other_encounter)) {
    diet[, , no_sp + 1 + i] <- do.call(params@other_encounter[[i]],
                                       list(params = params, n = n, n_pp = n_pp, n_other = n_other,
                                            component = names(params@other_encounter)[[i]]))
  }
  f <- getFeedingLevel(params, n, n_pp)
  fish_mask <- n > 0
  diet_comp <- sweep(diet_comp, c(1, 2), (1 - f) * fish_mask, "*")
  if (proportion) {
    total <- rowSums(diet_comp, dims = 2)
    diet_comp <- sweep(diet_comp, c(1, 2), total, "/")
    diet_comp[is.nan(diet_comp)] <- 0
  }
  return(diet_comp)
}


diet <- getDietComp(params)




### what can we plot with that?
plotDietComp <- function()
{
  
  
}





# some old code from chapter 3
  dietDat <- readRDS("romainZone/rawDiets.rds")
  totalDat <- NULL

   
        simSample <- simDat[[iPos]]
        tempSimDf <- NULL
        for(iSpecies in SpIdx) # for each species
        {
          speciesDat <- diet[iSpecies,,,] 
          speciesDat<- apply(speciesDat,c(1,3),sum) # sum prey identity, keep size class
          speciesPPMR <- NULL
          for(iSize in dimnames(speciesDat)$pred_size) # for each size class need PPMR value
          {
            if(sum(speciesDat[iSize,])) # if there is at least one diet data
            {
              sizeDat <- speciesDat[iSize,] # select the size
              sizeDat <- sizeDat / as.numeric(as.character(names(sizeDat)))# adjust biomass > density
              
              as.numeric(as.character(dimnames(speciesDat)$pred_size))
              
              PreferredSizeClass <- which(sizeDat == max(sizeDat)) # which size class is most feed upon
              sizePPMR <- as.numeric(iSize)/as.numeric(names(PreferredSizeClass)) # calculate PPMR
              speciesPPMR <- c(speciesPPMR,sizePPMR)
            }
          }
          tempSpeciesDf <- data.frame("species" = rep(iSpecies,length(speciesPPMR)), "rPPMR" = speciesPPMR)
          tempSimDf <- rbind(tempSimDf,tempSpeciesDf) # create a df of species
        }

    totalDat <- rbind(totalDat,tempKappaDf)
  
  saveRDS(totalDat,"romainZone/realisedPPMRdensity.rds")


plot_dat <- readRDS("romainZone/realisedPPMRdensity.rds")
plot_dat$log10PPMR <- log10(plot_dat$rPPMR)
plot_dat$species <- as.factor(plot_dat$species)

# empirical data
# grouping the data per size class
# data subset:
indDat<- readRDS("romainZone/empiricalPPMR.rds")
spDat <- read.csv("romainZone/Wmax_Romain.csv")
indDat <- indDat[,c("Predator.mass","Predator","lnPPMR")]
indDat$winf <- NA
# need to rewrite some of the species in the individual data
indDat$Predator[which(indDat$Predator == "Arnoglossus imperialis  ")] <- "Arnoglossus imperialis"
indDat$Predator[which(indDat$Predator == "Galeorhinus galeus  ")] <- "Galeorhinus galeus"
indDat$Predator[which(indDat$Predator == "Leucoraja  fullonica   ")] <- "Leucoraja fullonica"
indDat$Predator[which(indDat$Predator == "Scyliorhinus canicula ")] <- "Scyliorhinus canicula"

for(i in 1:dim(indDat)[1])
  indDat$winf[i] <- spDat$w_inf[which(spDat$Predator == indDat$Predator[i])]

indDat <- indDat[-c(which(is.na(indDat$winf))),] 
indDat$species <- as.factor(round(log10(indDat$winf))-1)

#merging data
indDat$data <- "empirical"
plot_dat$data <- "modelled"

plot_dat1 <- indDat[,-c(1,2,4)]
colnames(plot_dat1)[1] <- "log10PPMR"
plot_dat1$log10PPMR <- log10(exp(plot_dat1$log10PPMR))
plot_dat2 <- plot_dat[,c(5,1,6)]
plot_dat <- rbind(plot_dat1,plot_dat2)

dat_text <- data.frame(
  label = c("a)", "b)"),
  data   = c("empirical","modelled"))


p <- ggplot(plot_dat)+
  # geom_violin(aes(x = Predator.mass, y = lnPPMR, fill = species),scale = "count", alpha = .2)+
  geom_violin(aes(x = species, y = log10PPMR, fill = species),scale = "area",draw_quantiles = .5)+
  scale_fill_manual(name = "Species", values = colGrad)+ # color gradient
  scale_y_continuous(name = "log10 PPMR") +
  facet_wrap(.~data, nrow = 2)+
  theme(legend.title=element_text(),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.minor = element_line(colour = "grey92"), 
        legend.position="bottom",
        strip.background = element_blank(),
        legend.key = element_rect(fill = "white"))+
  geom_text(size = 5, data = dat_text, mapping = aes(x = - Inf, y = Inf, label = label), hjust = -0.75, vjust = 1.5) +
  guides(color = guide_legend(nrow=1))+
  ggtitle(NULL)

ggsave(p, file = "romainZone/figures/comparisonViolin2.png", units = "cm", width = 20, height = 20)