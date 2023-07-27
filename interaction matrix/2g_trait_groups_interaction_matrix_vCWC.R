# 2c_simplified_groups_interaction_matrix.R
# interaction matrix calculation for simplified groups - for functional size spectrum models

# interaction depends on
		# depth
		# water column use
		# other adjustment factors

# Scheme below accounts for effects of depth range and DVM, but not ontogenetic change
# need to consider if there is some way of accounting for this
# Decision rules
	# 'overlap' defined by
		# % overlap of depth range
			# this is the total depth range of the 2, divided by the depth range where the 2 overlap
				# generate df: olm <- data.frame(matrix(nrow=20, ncol=20, dimnames=list(prm.w$species, prm.w$species)))
		# modified by DVM
			# species with same DVM (both DVM, both non-DVM); no effect
			# species with opposing DVM: overlap halved (/2)
			# currently no modifier for ontogenetic DVM
		
		# # overlap function requires a table that has
		# 	species
		# 	min depth
		# 	max depth
		# 	distribution
		# 	water.column.use


library(readxl)
library(tidyverse)

# usr <- Sys.info()["user"]
# d<- paste0("/Users/", usr, "/GitHub/hawke_SBM")
# setwd(d)

# all.prm<- readRDS("all_groups_params_distributions.rds")
# all.prm<- readRDS("simplified_groups_params_distributions.rds")
# all.prm<- readRDS("trait_groups_params_distributions_vCWC_v2.rds")
all.prm<- readRDS("trait_groups_params_distributions_vCWC_v3.rds")


# for the interaction matrix: add phytoplankton - restricted to surface mixed layer (assumed at 50m)

# pp <- data_frame(species="phytoplankton",max_depth=50,min_depth=0, water.column.use="non-DVM")

ips <- all.prm %>% select(species, max_depth,min_depth,water.column.use)
# ips <- bind_rows(ips, pp)
ips$adj <- 1

# 

# adjustments before interaction calculation
	# small nectonic squid minimum depth to 0
	# ips[ips$species=="small nektonic squids",]$min_depth<- 0
# ips[ips$species=="squids",]$min_depth<- 0
	# flying birds maximum depth to 50 to enable access to resources
	ips[ips$species=="flying birds",]$max_depth<- 50
	ips[ips$species=="other krill",]$max_depth<- 500 # euphausids are mainly in top 500m; constrain this to ensure interaction with PP and surface predators
	ips[ips$species=="antarctic krill",]$max_depth<- 500 # euphausids are mainly in top 500m; constrain this to ensure interaction with PP and surface predators
	ips[ips$max_depth>2000,]$max_depth<- 2000 # cap max depths, as very deep ranges artificially down-weighting interactions
	ips$min_depth[is.na(ips$min_depth)]<-0

ol.fun <- function(sp1, sp2, dat){
	
	ol.p <- 1
	ol.mod <- 1

	range1 <- c(dat[dat$species==sp1,]$min_depth, dat[dat$species==sp1,]$max_depth)
	range2 <- c(dat[dat$species==sp2,]$min_depth, dat[dat$species==sp2,]$max_depth)
	tot.r <- diff(range(range1, range2))
	# test for range overlap: overlap of 0 if no range overlap
	if(length(intersect(seq(range1[1],range1[2]),seq(range2[1], range2[2])))==0){ol.p=0}else{
			
			ol.r  <- diff(range(intersect(seq(range1[1],range1[2]),seq(range2[1], range2[2]))))
			ol.p  <- ol.r/tot.r

			dvms  <- dat[dat$species%in%c(sp1, sp2),]$water.column.use

			# overlap of 0 if subantarctic and antarctic species
			# not currently implemented

			ol.mod <- 1
			
			# dvm modifier of overlap
			if("non DVM" %in% dvms & "DVM" %in% dvms){ol.mod<- ol.mod*0.5}
			#
			# Diving predator seasonality and DVM interaction modifiers
			#
			# only orca and leopard seals eat other diving predators

			if(("diving" %in% dvms) & length(unique(dvms))==1){ # both diving preds
				# up-weight diver/diver interactions for those that make it through these filters
				ol.mod<- ol.mod*4
				# interaction of 0 if Orca/Leopard aren't one of the predators
				# if(!(any(c(sp1, sp2) %in% c("apex predators")))) ol.mod <- 0 # Rowan original
				if(!(any(c(sp1, sp2) %in% c("orca", "leopard seals")))) ol.mod <- 0 # Kieran Edit
				# interaction of zero between diving and flying
				if(any(grepl("flying", c(sp1,sp2), ignore.case = T))) ol.mod <- 0
			}

			# diving predators are all present seasonally; vs other groups that are "resident"
			# down-weight interactions accordingly
			if(("diving" %in% dvms) & length(unique(dvms))>1){# diving predator and "resident"
			ol.mod<- ol.mod*0.5
			}


			# phytoplankton only interacting with zooplankton
			if(any(c(sp1,sp2) %in% "phytoplankton")){
				if(any(c(sp1,sp2) %in% c("euphausiids", "salps", "other macrozooplankton", "mesozooplankton",
"microzooplankton"))){
				ol.mod<- ol.mod*4 # up-weight interactions between phytoplankton and zooplankton
				}else ol.mod<-0
			} 


			#if("DVM" %in% dvms){ol.mod<- ol.mod*0.5} # turn this on to reduce interation of diurnal vertical migrators
			
			# if("diving" %in% dvms & "DVM" %in% dvms){ol.mod<- ol.mod*0.5}
			#
			# TODO: could potentially down-weight interactions between divers and DVM prey further

			# down weight interactions of shelf/slope dwellers to % of model domain that is >1000 m ==.2
			if(any(grepl("shelf", c(sp1,sp2), ignore.case = T)) & length(unique(c(sp1,sp2)))>1){ol.mod<- ol.mod*0.2}

			# down weight cannibalism
			# if(sp1==sp2){ol.mod <- ol.mod*.4}

			ol.adj<- prod(dat[dat$species%in%c(sp1, sp2),]$adj)
			ol.mod <- ol.mod*ol.adj
			#
			# TODO: consider making baleen whales zooplankton specialists... and probably exclude salps from diets

			

			ol.p <- ol.p*ol.mod
			if(ol.p>1) ol.p<- 1 # max is 1 for overlap
		}
	ol.p			
}

int.calc <- function(dat){
	int.l <- expand.grid(dat$species, dat$species)
	int.l$ol.p <- NA
	for(i in 1:nrow(int.l)){
		int.l$ol.p[i] <- ol.fun(as.character(int.l[i,1]), as.character(int.l[i,2]), dat)
	}

	int.m <- data.frame(matrix(int.l$ol.p, nrow=nrow(dat), ncol=nrow(dat), dimnames=(list(dat$species, dat$species))))

	int.m
}

int.m<- int.calc(ips)

# int.m %>% saveRDS("trait_groups_interaction_matrix_vCWC_v3.rds")
# int.m %>% write_csv("trait_groups_interaction_matrix_vCWC_v3.csv")


# visualise interactions as grid plot
		# all.inter <- read.csv("./csvs/interaction_matrix.csv")
		imo<- rownames(int.m)
		int.m$sp1 <- imo
		im <- reshape::melt(int.m)
		# has added . to names for sum reason
		im$sp2<- gsub("\\.", " ", im$variable)

		im$sp1<- factor(im$sp1, levels=imo)
		im$sp2<- factor(im$sp2, levels=imo)

		ggplot(im, aes(sp1, sp2, fill = value)) + geom_tile() + 

		scale_fill_gradient(low = "white",  high = "black") +
		theme_bw() +
		theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("interaction_matrix_trait_groups_vCWC_v3.png")


# phytoplankton mass bins
	ppmin <- -14.5
	ppmax <- -7.5

# plot to show sizes and depths

library(Polychrome)
# cols
set.seed(9641)
Dark24 <- createPalette(24, c("#2A95E8", "#E5629C"), range = c(10, 60), M = 100000)
Dark15 <- createPalette(15, c("#2A95E8", "#E5629C"), range = c(10, 60), M = 100000)
Dark22 <- Dark24[-c(3,6)]
ppcol<- Dark24[3]

# groupings to plot
all.prm$grp<- "fishes and squids"
# all.prm$grp[1]<- "euphausiids"
all.prm$grp[5:6]<- "marine mammals and birds"
all.prm$grp[9:15]<- "marine mammals and birds"
all.prm$grp<- factor(all.prm$grp, levels=c("plankton and jellies","fishes and squids", "marine mammals and birds"))

# # alternative grouping to reduce overlap between all the fish groups
# all.prm$grp2<- "pelagic fishes"
# all.prm$grp2[1:6]<- "plankton and jellies"
# all.prm$grp2[c(7:12, 18, 20:22)]<- "mammals, birds, squids, other fish"
# all.prm$grp2<- factor(all.prm$grp2, levels=c("plankton and jellies","pelagic fishes", "mammals, birds, squids, other fish"))

all.prm$pcol<- Dark15

# pdf("SO_SBM_sizes_depths.pdf", height=5, width=5)
# pdf("SO_SBM_sizes_depths_vCWC.pdf", height=5, width=5)
par(mfrow=c(3,1), mar=c(0,0,0,0), oma=c(3,4,1,1), las=1, fg="gray40", cex.axis=.7)
for(i in 1:3){
	plot(c(-12.5,8.5),c(0, -4000), type="n", col.axis="gray40", tcl=-.2, xlab="",ylab="", las=1, axes=F)
	axis(2)
	if(i==3) axis(1, at=c(-13:8), labels=c(10^(-13:8)))
	pdat<- all.prm[as.numeric(all.prm$grp)==i,]
	# pdat<- all.prm[as.numeric(all.prm$grp2)==i,]
	if(i==1) rect(ppmin, -50, ppmax, 0, col=ppcol, border=NA)

	# rect(xleft=pdat$w_min,ybottom=-pdat$max_depth, xright=pdat$w_max, ytop=-pdat$min_depth, col=adjustcolor(pdat$pcol, alpha.f =0.2),border = adjustcolor(pdat$pcol, alpha.f=.8))
	rect(xleft=pdat$w_min,ybottom=-pdat$max_depth, xright=pdat$w_inf, ytop=-pdat$min_depth, col=adjustcolor(pdat$pcol, alpha.f =0.2),border = adjustcolor(pdat$pcol, alpha.f=.8)) # adjustment for no w_max currently

	# text(x=rowMeans(pdat[c("w_min","w_max")]), y=-rowMeans(pdat[c("min_depth","max_depth")]), labels=pdat$species, cex=0.8, col=pdat$pcol)
	text(x=rowMeans(pdat[c("w_min","w_inf")]), y=-rowMeans(pdat[c("min_depth","max_depth")]), labels=pdat$species, cex=0.8, col=pdat$pcol) # adjustment for no w_max currently
	box()
}
dev.off()

