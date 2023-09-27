


#Function to process all the scenarios in a batch 

runSims<-function(scenList= scen_list, 
                  params=params, 
                  effort=effort, 
                  calibrateSetup=FALSE, 
                  initial_n=initial_n, 
                  initial_n_pp = initial_n_pp, 
                  diet_steps=10,
                  dt=1,
                  getDiet4D=TRUE){
  
  resultList<- scenList
  
  params_scen<- params
  
  for(i in 1:length(scenList)){
    
    params_scen@species_params$ca_met<- scenList[[i]]["ca_met"]
    params_scen@species_params$ca_mat<- scenList[[i]]["ca_mat"]
    params_scen@species_params$ca_mor<- 0
    params_scen@species_params$ca_int<- 0
    
    params_scen@species_params$ea_met<- scenList[[i]]["ea_met"]
    params_scen@species_params$ea_mat<- 0
    params_scen@species_params$ea_mor<- scenList[[i]]["ea_mor"]
    params_scen@species_params$ea_int<- scenList[[i]]["ea_int"]
    
    m_scen<-projectSetup(params_scen, effort=modscaled.INT_round2_fit@effort, 
                         dt=dt, 
                         calibrateSetup=FALSE, 
                         initial_n = initial_n, 
                         initial_n_pp = initial_n_pp, 
                         temperature_dt=scenList[[i]]["temperature"],
                         temperatureRef_dt=scenList[[i]]["temperatureRef"], 
                         diet_steps=diet_steps,
                         getDiet4D=getDiet4D)
    
    resultList[[i]]<-m_scen
    print(i)
  }
  return(resultList)
}



# Once the scenarios are run and stored as a list, we can use the below functions 
# with lapply to grab our favorite summary statistic or value from each model scenario 
# Note, the TL functions aren't correct just yet - working on that today (1/31/2020)


#############################################
#           MODELS as a list 
#############################################



#############################
# get community indicators 
#############################



getCommInd<-function(modls){
  
  #Indicators, apply to fish greater than 10 g
  # (1) LFI<- Large fish indicator: Biomass fish > 40 cm / Total biomass of all fish  (Blanchard et al. 2014) 
  # (2) MIW<- Mean individual weight 
  # (3) MMW<- Mean maximum weight (See Blanchard et al. 2014) 
  # (4) Slope<- Slope of community size spectrum 
  
  modN<-length(modls)
  modnam<- names(modls)
  
  
  species<-unique(modls[[1]]@params@species_params$species1)
  species<- species[-which(species=="Benthos")]
  species<- species[-which(species=="Detritus")]
  
  #Data frames to hold indivudal variables, array to hold output from all models 
  
  years<- as.numeric(as.character(dimnames(modls[[1]]@n)$time))
  predLFI<-expand.grid(list(year=years))
  
  predLFI$sim<-0
  
  predMIW<- predLFI
  predMMW<- predLFI
  predSlope<- predLFI
  
  
  predLFI$ts<-"lfi"
  predMIW$ts<- "miw"
  predMMW$ts<- "mmw"
  predSlope$ts<- "slope"
  
  #Data frame to hold time series output 
  datnew<- data.frame(year=0, sim=0, ts=0, model=0)
  datnew<-datnew[0,]
  
  
  
  #Extract LFI, MIW, MMW, and slope from previously fitted models   
  
  for( i in 1:modN){
    mod_preds<- modls[[i]]
    
    #Predict from model 
    
    time<- mod_preds@t_dimnames
    
    for (j in 1:length(time)){
      
      abspec<-getSpectra(mod_preds, time_range=time[j], biomass=FALSE) #Abundance spectra 
      abspec<- abspec[(abspec$w>=10), ]
      
      #Mean Individual weight 
      absFC<-subset(abspec, Species=="Fish_Crabs")
      
      predMIW$sim[j]<- weighted.mean(absFC$w,absFC$value )
      
      #Slope 
      abspectot<- aggregate(value~w, FUN=sum, data=abspec)
      predSlope$sim[j]<- coef(lm(log10(abspectot$value / mod_preds@params@dw_full[ mod_preds@params@w_full>10]) ~ log10(abspectot$w)))[2]
      
      
      #Mean maximum weight (treat sex as separate species)
      biospec<-getSpectra(mod_preds, time_range=time[j], biomass=TRUE) #Abundance spectra 
      biospec<- biospec[(biospec$w>=10), ]
      biospec<- biospec[!(biospec$Species=="Benthos"), ]
      biospec<- biospec[!(biospec$Species=="Background"), ]
      biospec<- biospec[!(biospec$Species=="Fish_Crabs"), ]
      
      
      ma<-match( biospec$Species, mod_preds@params@species_params$species)
      biospec$wmax<- mod_preds@params@species_params$w_inf[ma]
      biosum<-aggregate(value~wmax,data=biospec,FUN=sum)
      
      predMMW$sim[j]<- weighted.mean( biosum$wmax, biosum$value)
      
      #Large Fish indicator (crabs are excluded)
      
      biospec<- biospec[!(biospec$Species=="TANNER_m"), ]
      biospec<- biospec[!(biospec$Species=="TANNER_f"), ]
      biospec<- biospec[!(biospec$Species=="SNOWCRAB_m"), ]
      biospec<- biospec[!(biospec$Species=="SNOWCRAB_f"), ]
      biospec<- biospec[!(biospec$Species=="RKC_m"), ]
      biospec<- biospec[!(biospec$Species=="RKC_f"), ]
      
      ma<-match( biospec$Species, mod_preds@params@species_params$species)
      
      a<-mod_preds@params@species_params$a
      b<-mod_preds@params@species_params$b
      biospec$length<- exp( log(biospec$w/a[ma])/b[ma])
      
      predLFI$sim[j]<- sum(biospec$value[biospec$length>40])/ sum(biospec$value)
      
    }
    
    predLFI$model<-names(modls)[i]
    predMMW$model<-names(modls)[i]
    predMIW$model<-names(modls)[i]
    predSlope$model<-names(modls)[i]
    
    datnew<-rbind(datnew,predLFI, predMMW, predMIW, predSlope)
    
  } 
  return(datnew)
}







##########################################
#
# Get trophic level for each predator  
#
########################################


#modls<- allmodsF_350yrs
#        time_range=c(2339, 2364)
#        pelTLslope=.1
#        pelTLint=2.5
#        benTLslope=0
#        benTLint=2.5

#test
#modls<-modList
#mod<- modList[[1]]

getTLbyPred<-function(mod, 
                      time_range=max(as.numeric(dimnames(mod@n)$time)), 
                      zoopTL=0,
                      pelTLslope=0, pelTLint=0,
                      benTLslope=0, benTLint=0){ 
  
  
  
  time_elements <- get_time_elements(mod,time_range)
  
  n <- apply(mod@n[time_elements,,,drop=FALSE],c(2,3), mean)   # get n for weighting community mean TL 
  
  #Get biomass density     
  
  size_range <- get_size_range_array(mod@params)
  
  n_dw <- sweep(n*size_range,2,mod@params@dw, "*")
  b_wdw <- sweep(n_dw,2,mod@params@w, "*")
  
  
  n1<-n_dw
  n1[n1>0]<-1
  
  
  #Pull out diet composition 
  dietcomp<-mod@diet_comp4D
  
  tlsum<-dietcomp[1,1,,]
  tlsum<-drop(tlsum)
  names(dimnames(tlsum))<-c("predator","pred_size")
  
  predator<- dimnames(dietcomp)$predator
  prey<-dimnames(dietcomp)$prey
  pred_size<- dimnames(dietcomp)$pred_size
  prey_size<- dimnames(dietcomp)$prey_size
  
  tlsum[]<-0
  tlsum[dimnames(tlsum)$predator=="Benthos", ]<- benTLint + benTLslope * log10(as.numeric(dimnames(tlsum)$pred_size))
  tlsum[dimnames(tlsum)$predator=="background", ]<- pelTLint + pelTLslope * log10(as.numeric(dimnames(tlsum)$pred_size))
  
  #Load in specific zoop TL if supplied 
  if(all(!is.na(zoopTL))){ tlsum[dimnames(tlsum)$predator=="background", ] <- zoopTL }
  
  
  predator<- predator[!predator=="Detritus"]
  predator<- predator[!predator=="Benthos"]
  predator<- predator[!predator=="background"]
  
  #Calculate the weighted mean community TL 
  
  tlcomm<- rep(NA,length(prey_size))
  
  dimnames(n)[[2]]<-  dimnames(dietcomp)$pred_size
  for(i in 1:length(pred_size)){      #Predator size 
    for (j in 1:length(predator)){  #Predator species
      
      tl<- 1 + weighted.mean(tlsum,  dietcomp[dimnames(dietcomp)$predator==predator[j], dimnames(dietcomp)$pred_size==pred_size[i], ,] ) 
      tlsum[dimnames(tlsum)$predator==predator[j], dimnames(tlsum)$pred_size==pred_size[i]]<-tl
      
      #Remove tl where predator abundance is zero 
      
    }
    
    #Get community weighted mean tl:  sum(tl * n) /sum(n) 
    if(sum(n_dw[dimnames(n)$sp %in%predator, i]) == 0){
      tlcomm[dimnames(tlsum)$pred_size==pred_size[i]]<- NA
    } else {   
      tlcomm[dimnames(tlsum)$pred_size==pred_size[i]]<- sum((tlsum[ dimnames(tlsum)$predator  %in% predator, dimnames(tlsum)$pred_size==pred_size[i]] * n_dw[dimnames(n)$sp %in%predator, i] )) / sum(n_dw[dimnames(n)$sp %in%predator, i])
    } #End if                 
  } #End loop 
  
  
  
  #Remove tl of species if predator abundance is zero 
  for(i in 1:length(pred_size)){      #Predator size 
    for (j in 1:length(predator)){  #Predator species
      
      
      if( as.numeric(pred_size[i]) > as.numeric(min(dimnames(n)$w))  ){
        if(n[dimnames(n)$sp==predator[j], dimnames(n)$w==pred_size[i]]==0) {
          tlsum[dimnames(tlsum)$predator==predator[j], dimnames(tlsum)$pred_size==pred_size[i]]<- NA
        }
      }
    }
  }
  
  
  
  #Add community on to species matrix 
  tlcomm<- matrix(tlcomm, nrow=1)
  rownames(tlcomm)<- "community"
  tlsum<-rbind(tlsum, tlcomm)
  
  dimnames(tlsum)<-list( predator=dimnames(tlsum)[[1]], 
                         pred_size=dimnames(tlsum)[[2]])
  
  #Get rid of Benthos, Detritus, background
  tlsum<-tlsum[ !dimnames(tlsum)$predator %in% c("Benthos","Detritus","background"), ]
  
  
  #Melt the arrays to make data frame with species, wt, biomass, num, tl 
  tldat<-melt(tlsum)
  colnames(tldat)<- c("predator", "wt","tl")
  
  
  
  return(tldat)
  
} #end function 







##########################################
#
# Get Community mean TL by body size class (weighted av. TL within body size class)
#
########################################


modls<- mod_ls


pelTLslope=0; pelTLint=0
benTLslope=0; benTLint=0
zoopTL=0


modls<- modList

getCommMeanTL_List<-function(modls, 
                             time_range=max(as.numeric(dimnames(modls[[1]]@n)$time)),
                             zoopTL=NA,
                             pelTLslope=0, pelTLint=0,
                             benTLslope=0, benTLint=0){ 
  
  #data frame to recieve results 
  dat<-data.frame(wt=0, tls=0, model=0, totbio=0, totnum=0)
  dat<-dat[-1,]
  
  
  
  for (h in 1:length(modls)){
    
    time_elements <- get_time_elements(modls[[h]],time_range)
    
    n <- apply(modls[[h]]@n[time_elements,,,drop=FALSE],c(2,3), mean)   # get n for getting weighted community mean TL 
    
    #Get biomass density     
    
    size_range <- get_size_range_array(modls[[h]]@params)
    
    b <- sweep(n*size_range,2,modls[[h]]@params@w * modls[[h]]@params@dw, "*")
    n <- sweep(n*size_range,2,modls[[h]]@params@dw, "*")
    
    n1<-n
    n1[n1>0]<-1
    
    
    #Pull out diet composition 
    dietcomp<-modls[[h]]@diet_comp4D
    
    tlsum<-dietcomp[1,1,,]
    tlsum<-drop(tlsum)
    names(dimnames(tlsum))<-c("predator","pred_size")
    
    predator<- dimnames(dietcomp)$predator
    prey<-dimnames(dietcomp)$prey
    pred_size<- dimnames(dietcomp)$pred_size
    prey_size<- dimnames(dietcomp)$prey_size
    
    tlsum[]<-0
    tlsum[dimnames(tlsum)$predator=="Benthos", ]<- benTLint + benTLslope * log10(as.numeric(dimnames(tlsum)$pred_size))
    tlsum[dimnames(tlsum)$predator=="background", ]<- pelTLint + pelTLslope * log10(as.numeric(dimnames(tlsum)$pred_size))
    
    #Load in specific zoop TL if supplied 
    if(all(!is.na(zoopTL))){ tlsum[dimnames(tlsum)$predator=="background", ] <- zoopTL }
    
    predator<- predator[!predator=="Detritus"]
    predator<- predator[!predator=="Benthos"]
    predator<- predator[!predator=="background"]
    
    
    
    for(i in 1:length(pred_size)){    #Predator size 
      for (j in 1:length(predator)){ #Predator species
        
        tl<- 1 + weighted.mean(tlsum,  dietcomp[dimnames(dietcomp)$predator==predator[j], dimnames(dietcomp)$pred_size==pred_size[i], ,] ) 
        tlsum[dimnames(tlsum)$predator==predator[j], dimnames(tlsum)$pred_size==pred_size[i]]<-tl
      }
    }
    
    
    tlsum<-tlsum[1:25, 31:130] * n1
    
    n[dimnames(n)$sp=="Benthos", ]<-0
    n[dimnames(n)$sp=="Detritus", ]<-0
    
    b[dimnames(b)$sp=="Benthos", ]<-0
    b[dimnames(b)$sp=="Detritus", ]<-0
    
    tlsum<-tlsum[, as.numeric(dimnames(tlsum)$pred_size)>.001]
    n<- n[, as.numeric(dimnames(n)$w)>.001]
    b<- b[, as.numeric(dimnames(b)$w)>.001]
    
    
    tlcomm<- data.frame(  wt= as.numeric(dimnames(n)$w), 
                          tls=rep(0, dim(tlsum)[2])) 
    
    
    #Calculate community mean tl 
    for(i in 1:dim(tlsum)[2]){
      tlcomm$tls[i]<- weighted.mean( tlsum[,i], n[,i]) 
    }
    
    tlcomm$totbio<-apply(b, 2, FUN="sum") 
    tlcomm$totnum<-apply(n, 2, FUN="sum") 
    
    tlcomm$model<- names(modls)[h]
    
    dat<- rbind(dat, tlcomm)
    
  } #end h 
  
  return(dat)
  
} #end function 







####Version for lapply 




getCommMeanTL<-function(object, 
                        time_range=max(as.numeric(dimnames(object@n)$time)),
                        zoopTL=0,
                        pelTLslope=0, pelTLint=0,
                        benTLslope=0, benTLint=0){ 
  
  #data frame to recieve results 
  dat<-data.frame(wt=0, tls=0, model=0, totbio=0, totnum=0)
  dat<-dat[-1,]
  
  
  time_elements <- get_time_elements(object,time_range)
  
  n <- apply(object@n[time_elements,,,drop=FALSE],c(2,3), mean)   # get n for getting weighted community mean TL 
  
  #Get biomass density     
  
  size_range <- get_size_range_array(object@params)
  
  b <- sweep(n*size_range,2, object@params@w * object@params@dw, "*")
  n <- sweep(n*size_range,2, object@params@dw, "*")
  
  n1<-n
  n1[n1>0]<-1
  
  
  #Pull out diet composition 
  dietcomp<-object@diet_comp4D
  
  tlsum<-dietcomp[1,1,,]
  tlsum<-drop(tlsum)
  names(dimnames(tlsum))<-c("predator","pred_size")
  
  predator<- dimnames(dietcomp)$predator
  prey<-dimnames(dietcomp)$prey
  pred_size<- dimnames(dietcomp)$pred_size
  prey_size<- dimnames(dietcomp)$prey_size
  
  tlsum[]<-0
  tlsum[dimnames(tlsum)$predator=="Benthos", ]<- benTLint + benTLslope * log10(as.numeric(dimnames(tlsum)$pred_size))
  tlsum[dimnames(tlsum)$predator=="background", ]<- pelTLint + pelTLslope * log10(as.numeric(dimnames(tlsum)$pred_size))
  
  #Load in specific zoop TL if supplied 
  if(all(!is.na(zoopTL))){ tlsum[dimnames(tlsum)$predator=="background", ] <- zoopTL }
  
  predator<- predator[!predator=="Detritus"]
  predator<- predator[!predator=="Benthos"]
  predator<- predator[!predator=="background"]
  
  
  
  for(i in 1:length(pred_size)){    #Predator size 
    for (j in 1:length(predator)){ #Predator species
      
      tl<- 1 + weighted.mean(tlsum,  dietcomp[dimnames(dietcomp)$predator==predator[j], dimnames(dietcomp)$pred_size==pred_size[i], ,] ) 
      tlsum[dimnames(tlsum)$predator==predator[j], dimnames(tlsum)$pred_size==pred_size[i]]<-tl
    }
  }
  
  
  tlsum<-tlsum[1:25, 31:130] * n1
  
  n[dimnames(n)$sp=="Benthos", ]<-0
  n[dimnames(n)$sp=="Detritus", ]<-0
  
  b[dimnames(b)$sp=="Benthos", ]<-0
  b[dimnames(b)$sp=="Detritus", ]<-0
  
  tlsum<-tlsum[, as.numeric(dimnames(tlsum)$pred_size)>.001]
  n<- n[, as.numeric(dimnames(n)$w)>.001]
  b<- b[, as.numeric(dimnames(b)$w)>.001]
  
  
  tlcomm<- data.frame(  w= as.numeric(dimnames(n)$w), 
                        tls=rep(0, dim(tlsum)[2])) 
  
  
  #Calculate community mean tl 
  for(i in 1:dim(tlsum)[2]){
    tlcomm$tls[i]<- weighted.mean( tlsum[,i], n[,i]) 
  }
  
  tlcomm$totbio<-apply(b, 2, FUN="sum") 
  tlcomm$totnum<-apply(n, 2, FUN="sum") 
  
  dat<- rbind(dat, tlcomm)
  
  
  return(dat)
  
} #end function 














################################################
#
# Get TL by body size for individual species 
#
##################################################

getSpTLBodySize<-function(modls, 
                          time_range=max(as.numeric(dimnames(modls[[1]]@n)$time)),
                          zoopTL=NA,
                          pelTLslope=.1, pelTLint=2.5,
                          benTLslope=0, benTLint=2.5){ 
  
  #data frame to recieve results 
  dat<-data.frame(wt=0, tls=0, model=0, species=0)
  dat<-dat[-1,]
  
  
  
  for (h in 1:length(modls)){
    
    time_elements <- get_time_elements(modls[[h]],time_range)
    
    n <- apply(modls[[h]]@n[time_elements,,,drop=FALSE],c(2,3), mean)   # get n for getting weighted community mean TL 
    
    #Get biomass density     
    
    size_range <- get_size_range_array(modls[[h]]@params)
    
    b <- sweep(n*size_range,2,modls[[h]]@params@w * modls[[h]]@params@dw, "*")
    n <- sweep(n*size_range,2,modls[[h]]@params@dw, "*")
    
    n1<-n
    n1[n1>0]<-1
    
    #Pull out diet composition 
    dietcomp<-modls[[h]]@diet_comp4D
    
    tlsum<-dietcomp[1,1,,]
    tlsum<-drop(tlsum)
    names(dimnames(tlsum))<-c("predator","pred_size")
    
    predator<- dimnames(dietcomp)$predator
    prey<-dimnames(dietcomp)$prey
    pred_size<- dimnames(dietcomp)$pred_size
    prey_size<- dimnames(dietcomp)$prey_size
    
    tlsum[]<-0
    tlsum[dimnames(tlsum)$predator=="Benthos", ]<- benTLint + benTLslope * log10(as.numeric(dimnames(tlsum)$pred_size))
    tlsum[dimnames(tlsum)$predator=="background", ]<- pelTLint + pelTLslope * log10(as.numeric(dimnames(tlsum)$pred_size))
    
    #Load in specific zoop TL if supplied 
    if(all(!is.na(zoopTL))){ tlsum[dimnames(tlsum)$predator=="background", ] <- zoopTL }
    
    predator<- predator[!predator=="Detritus"]
    predator<- predator[!predator=="Benthos"]
    predator<- predator[!predator=="background"]
    
    
    
    for(i in 1:length(pred_size)){    #Predator size 
      for (j in 1:length(predator)){ #Predator species
        
        tl<- 1 + weighted.mean(tlsum,  dietcomp[dimnames(dietcomp)$predator==predator[j], dimnames(dietcomp)$pred_size==pred_size[i], ,] ) 
        tlsum[dimnames(tlsum)$predator==predator[j], dimnames(tlsum)$pred_size==pred_size[i]]<-tl
      }
    }
    
    
    tlsumSub<-tlsum[1:25, 31:130]
    
    tlsumSub[n1==0]<- 0 
    
    out<-melt(tlsumSub)
    out$value[out$value==0]<-NA
    
    colnames(out$value)
    colnames(out)[which(colnames(out)=="value")]<- "tls"
    colnames(out)[which(colnames(out)=="predator")]<- "species"
    colnames(out)[which(colnames(out)=="pred_size")]<- "wt"
    
    out$model<- names(modls)[h]
    
    dat<-rbind(dat, out)
  } #end h 
  
  return(dat)
  
} #end function 




####Get the PPMR 

getPPMRsp<-function(object=object, prey=dimnames(object@diet_comp)$prey, 
                    predator=dimnames(object@diet_comp)$predator){
  
  prey_nam<-prey
  pred_nam<-predator
  
  out<-object@diet_comp4D 
  
  prey<-apply(out, c(1,2,4), FUN=sum) #Sum across size classess with in prey 
  tot<-apply(prey, c(1,2), FUN=sum) #Sum across prey weight size classes 
  
  prop_prey<-sweep(prey, 1:2, tot, "/" ) #proportion of prey weight in each each prey size class
  prop_prey[is.na(prop_prey)]<-0
  
  #make matrix of realized PPMR
  ppmr_mat<-outer(object@params@w, object@params@w_full, FUN="/")
  ppmr_frac<-sweep(prop_prey, 2:3, ppmr_mat, "*")
  ppmr_tot<-apply(ppmr_frac, c(1,2), FUN=sum) #Sum across prey weight size classes 
  
  plot_dat<-expand.grid(dimnames(ppmr_tot)[[1]], dimnames(ppmr_tot)[[2]])
  colnames(plot_dat)<-c("predator","predsize")
  plot_dat$predsize<-as.numeric(as.character(log10(as.numeric(as.character(plot_dat$predsize)))))
  
  plot_dat$value<- as.vector(ppmr_tot)
  
  
  species<-object@params@species_params$species
  wmin<-object@params@w[object@params@species_params$w_min_idx]
  wmax<-object@params@w[object@params@species_params$w_max_idx]
  
  for ( i in 1:length(species)){
    plot_dat$value[plot_dat$predator==species[i] & plot_dat$predsize < log10(wmin[i])]<- 0
    plot_dat$value[plot_dat$predator==species[i] & plot_dat$predsize > log10(wmax[i])]<- 0
  }
  
  plot_dat$value[plot_dat$value==0]<-NA
  
  plot_dat<-plot_dat[!plot_dat$predator=="Benthos",]
  plot_dat<-plot_dat[!plot_dat$predator=="Detritus",]
  
  return(na.omit(plot_dat))
}

c("Benthos","Detritus")  %in% dimnames(out)["predator"]





object<- modList[[1]]

getPPMRcomm<-function(object=object){
  
  out<-object@diet_comp4D #This is g / m3 / yr, so we're working in biomass-weighted PPMR
  
  #Remove detritus as predator and prey, and benthos as predator 
  out<- out[!dimnames(out)$predator %in% c("Benthos","Detritus"),,!dimnames(out)$prey %in% c("Detritus"),]
  
  
  preyBiomass<-apply(out, c(2,4), FUN=sum) #Sum up prey encountred in predator size class by prey size class 
  prey_wt<-as.numeric(as.character(dimnames(preyBiomass)$prey_size))
  
  preyNumbers<-sweep(preyBiomass, 2, prey_wt, "/")  #convert g prey to individual prey 
  
  
  weighted.meanFunc<-function(bio) {
    weighted.mean(prey_wt , w=bio)
  }
  
  
  wtpreyBiomass<- apply(preyBiomass, c(1), weighted.meanFunc) #prey bodymass weighted by biomass in predator diet 
  wtpreyNumber<- apply(preyNumbers, c(1), weighted.meanFunc) #prey bodymass weighted by biomass in predator diet 
  
  #Predator to prey body mass ratio; PPMRbio 
  
  
  df<-data.frame(ppmrN =log10(as.numeric(names(wtpreyNumber)) / wtpreyNumber),
                 ppmrB = log10(as.numeric(names(wtpreyBiomass)) / wtpreyBiomass))
  
  return(as.matrix(df))
}





##################Function to get TLsmooth 

TLsmooth<-function(biomass, bins, tlsd=.1, binwid=.05){
  biosum<- rep(0,length(biomass))
  
  for(i in 1:length(biomass)){  # Pull out biomass in bin to distribute
    bio<-biomass[i]
    
    if(bio>0){
      for(j in 1:length(biomass)){ # distribute biomass from bin to surrounding bins
        biosum[j]<- biosum[j] + dnorm(bins[j],bins[i],tlsd)* bio * binwid
      }
    }
  }
  return(biosum)
}




######################
# Average age of SSB #




library(splines)

getMeanSSBAge(object)


getMeanSSBAge<-function(object=model, time_range=dim(object@n)[1]-1, 
                        print_it = TRUE, dt=300, use.kinf=FALSE){
  
  e_growth<- object@e_growth[-dim(object@n)[1],,]
  
  index<- c(1:I(dim(object@n)[1]-1))
  
  if(length(time_range)>1){
    time_elements <- (index >= time_range[1]) & (index <= time_range[2])
    spec_e_growth <- apply(e_growth[time_elements,,],c(2,3), mean)
  } else {
    time_elements <- (index == time_range[1])
    spec_e_growth <- e_growth[time_elements,,]
  }
  
  
  
  #get SSB for same mean age at maturation calc 
  
  if(length(time_range)>1){
    time_elements <- (index >= time_range[1]) & (index <= time_range[2])
    spec_spawner_ab <- sweep(apply(object@n[time_elements,,],c(2,3), mean), 2, object@params@dw, "*") * object@params@psi
  } else {
    time_elements <- (index == time_range[1])
    spec_spawner_ab <- sweep(object@n[time_elements,,], 2, object@params@dw, "*") * object@params@psi
  }
  
  
  
  #Pick range of WT to get growth rates for
  
  edat <- data.frame(e_gr = c(spec_e_growth), Species = dimnames(spec_e_growth)[[1]], w = rep(object@params@w, each=dim(spec_e_growth)[1]))
  
  species<-object@params@species_params$species
  
  wmin<-object@params@w[object@params@species_params$w_min_idx]
  wmax<-object@params@w[object@params@species_params$w_max_idx]
  
  for ( i in 1:length(species)){
    edat$e_gr[edat$Species==species[i] & edat$w < wmin[i]]<- NA
    edat$e_gr[edat$Species==species[i] & edat$w > wmax[i]]<- NA
  }
  
  edat<-edat[!is.na(edat$e_gr),]
  
  #Estimate functions to get growth 
  modls<-list()
  
  #Make smooth function of growth for each species 
  for(i in 1:length(species)){
    
    dat<-subset(edat, Species==species[i])
    
    if(dim(dat)[1]>2){
      dat=subset(edat, Species==species[i])
      spf<-splinefun(dat$w,dat$e_gr) #Spline interpolation for calculating w+dt 
      modls[[i]]<-spf               
    } else {
      modls[[i]]<-NA
    } 
  }
  
  
  
  #Data frame to hold age at weight calculations
  age<-c(0:100)
  
  adat<-expand.grid(unique(edat$Species), c(0:max(age)))        
  colnames(adat)<-c("Species", "age")
  adat$e_wt<-0
  adat$obs_wt<-0
  adat$age_norm<-0
  
  
  
  
  k<-object@params@species_params$k_vb
  
  if(use.kinf==TRUE){
    w_inf<-object@params@species_params$w_kinf
  } else {
    w_inf<-object@params@species_params$w_inf
  }
  
  
  t0<-object@params@species_params$t0
  b<-object@params@species_params$b
  
  
  for(i in 1:length(species)){
    adat$obs_wt[adat$Species==species[i]]<- (w_inf[i]* (1-exp(-k[i]*(adat$age[adat$Species==species[i]]-t0[i])))^b[i] )/w_inf[i]
  }
  
  adat$obs_wt[adat$obs_wt<=0]<-NA
  
  
  #adat$age_norm[adat$Species==species[i]]<- adat$age[adat$Species==species[i]] / (a*object@params@species_params$w_mat[i]^(1-n)) #n is exponent for maximum food intake (Hartvig et al. 2011) 
  
  
  for(i in 1:length(species)){
    
    incdat<-rep(NA, dt)
    incdat[1]<-object@params@species_params$w_min[i]
    adat$e_wt[adat$Species==species[i] & adat$age==age[1]]<-object@params@species_params$w_min[i] 
    
    if(!is.na(modls[[i]])){
      
      for (ii in 2:length(age)){
        
        for (j in 1:(dt-1)){
          spfun<-modls[[i]]
          gr<-incdat[j]+ spfun(incdat[j]) *(1/dt)
          
          if (gr<w_inf[i]){
            incdat[1+j]<- gr
          } else {
            incdat[1+j]<-w_inf[i]
          }
        }
        adat$e_wt[adat$Species==species[i] & adat$age==age[ii]]<-incdat[dt] 
        incdat[1]<-incdat[dt]
      }
    } else { adat$e_wt[adat$Species==species[i]]<-0}
    
  }
  # Get wt at 50%
  # Make function of Age ~ wt 
  
  modls_afunc<-list()
  modls_psifunc<-list()
  
  for(i in 1:length(species)){
    
    dat<-subset(adat, Species==species[i])
    
    if( species[i]%in%c("Benthos","Detritus")) {  #to deal with benthos
      modls_afunc[[i]]<-function(x){return(0)}
      modls_psifunc[[i]]<-function(x){return(0)}
    } else { 
      
      spf<-splinefun(dat$e_wt,dat$age) #Spline interpolation for calculating w+dt 
      modls_afunc[[i]]<-spf               
      
      wt<-as.numeric(dimnames(spec_spawner_ab)[[2]])
      xx<-object@params@psi[i,]
      
      spf_psi<-splinefun(x=xx[xx>0 & xx<1], y=wt[xx>0 & xx<1], method="monoH.FC")
      modls_psifunc[[i]]<- spf_psi
    }
  }
  
  
  
  #Biuld matrix of age vs. weight. 
  
  age_weight<- spec_e_growth
  
  age_weight[]<-0
  
  wt<-as.numeric(dimnames(age_weight)[[2]])
  
  for(i in 1:length(modls_afunc)){
    age_weight[i,]<- modls_afunc[[i]](wt)
  }
  
  age_weight[age_weight<0]<-0
  
  
  avedat<-data.frame(aveMatwt=rep(NA, length(modls_afunc)),
                     aveMatAge=rep(NA, length(modls_afunc)), 
                     aveAge_50=rep(NA, length(modls_afunc)))
  
  
  
  rownames(avedat)<-species
  
  #Add average age of spanwers back in (psi * n*dw * age~w)
  
  for(i in 1:length(modls_afunc)){
    
    avedat$aveMatwt[i]<- weighted.mean( as.numeric(dimnames(spec_spawner_ab)[[2]]), spec_spawner_ab[i,])
    avedat$aveMatAge[i]<-    modls_afunc[[i]](avedat$aveMatwt[i]) #the average age of spawners
    
    #get wt at 0.5 maturation 
    wtM<- modls_psifunc[[i]](0.5)
    #get age at weight 
    avedat$aveAge_50[i]<- modls_afunc[[i]](wtM) #the averaget age when psi is 50% 
    
  }
  
  
  avedat<-avedat[!rownames(avedat)%in%c("Benthos", "Detritus"), ]
  return(avedat)
}










