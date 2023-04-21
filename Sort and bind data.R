###IWC whaling data, read and sort files

rm(list=ls())
library(ggplot2);library(reshape2)
# library(datamart)
library(gridExtra)
world<-map_data("world")
##make adata frame to sort out the whale codes to text
# whale.nos<-c(1:20)
whale.nos<-c(1:22) # Kieran Edit
# whale.names<-c("Pilot", "Bottlenose", "Killer", "Blue", "Fin",
# 			"Sperm", "Humpback", "Sei", "Common Minke",
# 			"Byrdes", "Right", "Gray", "Bairds Beaked",
# 			"Baleen", "Pygmy Blue", "Pygmy Right", 
# 			"Cuviers Beaked", "Bowhead", "Beaked (unspec)",
# 			"Antarctic Minke")
whale.names<-c("Pilot", "Bottlenose", "Killer", "Blue", "Fin",
               "Sperm", "Humpback", "Sei", "Common Minke",
               "Byrdes", "Right", "Gray", "Bairds Beaked",
               "Baleen", "Pygmy Blue", "Pygmy Right", 
               "Cuviers Beaked", "Bowhead", "Beaked (unspec)",
               "Antarctic Minke", "Sei/Brydes", "Dolphin")

whale.codes<-data.frame(whale.names, whale.nos)

##columns to select from the data sets (we dont need them all)
# columns<-c("Year", "Sp", "Len", "L.u", "Sx", "Lat", "Mn", "Lon","Mn.1", "Mat", "Ac", "N.S", "E.W")
# columns<-c("Year", "Sp", "Len", "L-u", "Sx", "Lat", "Mn", "Lon","Mn.1", "Mat", "Ac", "N.S", "E.W")
columns<-c("Year", "Sp", "Len", "L.u", "Sx", "Lat", "Mn", "Lon","Mn.1", "Mat", "Ac", "N.S", "E.W")


# ##read in the Indian Ocean Data
# # IO<-read.csv("IWC-DBv7.1/IndivData-CSVfmt/IO-a.csv")
# IO<-read.csv("IWC-DBv7.1/IndivData-CSVfmt/IO.csv") # Kieran
# 
# head(IO, 1)
# IO<-IO[, columns]
# IO$area<-"Indian ocean"
# 
# ##read in North atlantic 
# df_NA <-read.csv("IWC-DBv7.1/IndivData-CSVfmt/NA.csv")
# head(df_NA, 1)
# df_NA<-df_NA[, columns]
# df_NA$area<-"North atlantic"
# 
# ##read in a second North atlantic file (check with Cherry)
# # NA.2<-read.csv("~/Desktop/Whaling EWS/Data/IWC data/Individual data csvs/NA.csv")
# # head(NA.2, 1)
# # NA.2<-NA.2[, columns
# # NA.2$area<-"North atlantic"
# 
# ##read in the North Pacific 
# NP <-read.csv("~/IWC-DBv7.1/IndivData-CSVfmt/NP.csv")
# NP.a<-NP.a[, columns]
# NP.a$area<-"North Pacific"
# 
# ##read in south atlantic
# SA.a<-read.csv("~/IWC-DBv7.1/IndivData-CSVfmt/SA-a.csv")
# SA.a<-SA.a[, columns]
# SA.a$area<-"South atlantic"
# 
# ##read in south georgia
# Sg<-read.csv("~/IWC-DBv7.1/IndivData-CSVfmt/SHSG-a.csv")
# Sg<-Sg[, columns]
# Sg$area<-"South georgia"
# 
# ##read in south shetland 
# Ss<-read.csv("~/IWC-DBv7.1/IndivData-CSVfmt/SHSS-a.csv")
# Ss<-Ss[, columns]
# Ss$area<-"South shetland"
# 
# ##read in south pacific 
# Sp<-read.csv("~/IWC-DBv7.1/IndivData-CSVfmt/SP-a.csv")
# Sp<-Sp[, columns]
# Sp$area<-"South pacific"

##read in the two southern hemisphere data set:
SH1<-read.csv("SHP1_edit.csv")
# SH2<-read.csv("~/IWC-DBv7.1/IndivData-CSVfmt/SHP2.csv")
# SH<-rbind(SH1, SH2)
SH<-SH1
SH<-SH[, columns]
SH$area<-"Southern Hemisphere"

##bind the data together
dd<-rbind(IO, NA.a, NP.a, SA.a, Sg, Ss, Sp, SH);head(dd, 3)
dd<- SH
####################################################################################################################
####################################################################################################################
##just for southern hemisphere:
##dats<-c("Southern Hemisphere", "South pacific", "South shetland", "South georgia", "South atlantic")

dd<-dd[which(dd$area%in%dats),]

##number of caught whales:
length(dd[,1])

##correct positive lat and long
dd$Lat[which(dd$N.S=="S")]<-dd$Lat[which(dd$N.S=="S")]*-1
dd$Lon[which(dd$E.W=="W")]<-dd$Lon[which(dd$E.W=="W")]*-1

##convert to decimal
dd$dec.Lat<-dd$Lat+(dd$Mn/60)
dd$dec.Lon<-dd$Lon+(dd$Mn.1/60)

##NA out the locational data that isnt certain:
dd$Lat[which(dd$Ac==0)]<-NA;dd$Lon[which(dd$Ac==0)]<-NA

##add in the species data
dd$Sp.names<-whale.codes$whale.names[match(dd$Sp, whale.codes$whale.nos)]

##map the data 
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="white") # create a layer of borders
whales<-c("Humpback", "Sei", "Blue", "Sperm", "Fin")
sub.dd<-dd[dd$Sp.names%in%whales, ]

 png("Plots/world map.png", width=1600, height=5000)
  ggplot()+mapWorld +geom_point(data=sub.dd, aes(x=dec.Lon, y=dec.Lat, col=area), size=0.5)+scale_colour_brewer(palette = "Set2", name = "Species")+coord_equal(ratio=1)+theme_bw()+theme(legend.position="top")+ xlab("")+ylab("")+theme(axis.ticks = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank())+facet_wrap(~Sp.names, ncol=1)
 dev.off()

##look at rough time series:
head(dd)
d1<-(melt(tapply(dd$Sp.names, list(dd$Year, dd$Sp.names), length)))
names(d1)<-c("Year", "Species", "Landings")
##remove spp with hardly any data:
omit<-c("Cuviers Beaked", "Baleen", "Pygmy Blue")
d1<-d1[!d1$Species%in%omit,]

##plot the rough time series:
thm<-theme(
    legend.background = element_rect(fill = "white"),
    panel.grid.major.y=element_blank(), 
    panel.grid.major = element_line(colour = "grey40"),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill = "grey95")
  )

##body size stuff
head(dd)

##function to convert ft and inches to cm
ft.in.2cm<-function(x){
	return(((as.numeric(substring(as.character(x), 1, 2))*12)+as.numeric(substring(as.character(x), 3, 4)))*2.54)
}

ft2cm<-function(x){
	return((as.numeric(substring(as.character(x), 1, 2))*12)*2.54)
}

##convert the ft and inches to cm
dd$Len[which(dd$Len==0000)]<-NA
dd$Len[which(dd$L.u==4)]<-ft.in.2cm(dd$Len[which(dd$L.u==4)])
dd$Len[which(dd$L.u==2)]<-ft2cm(dd$Len[which(dd$L.u==2)])

##remove shitty data (eg. massive individuals)
dd$Len[which(dd$Len>3400)]<-NA

##calculate means per spp per yr
szs<-(melt(tapply(dd$Len, list(dd$Year, dd$Sp.names), mean, na.rm=T)))
szs<-szs[!szs$Var2%in%omit,]
d1$Mean.size<-szs$value

##calculate size sd per yr
szs.sd<-(melt(tapply(dd$Len, list(dd$Year, dd$Sp.names), sd, na.rm=T)))
szs.sd<-szs.sd[!szs.sd$Var2%in%omit,]
d1$SD.size<-szs.sd$value

##calculate the 95 percentile 
q<-function(x){quantile(na.omit(x), 0.95)}
szs.95<-(melt(tapply(dd$Len, list(dd$Year, dd$Sp.names), q)))
szs.95<-szs.95[!szs.95$Var2%in%omit,]
d1$size.95<-szs.95$value

##calculate L-MAX
lmx<-function(x){max(na.omit(x))}
lmax<-(melt(tapply(dd$Len, list(dd$Year, dd$Sp.names), lmx)))
lmax<-lmax[!lmax$Var2%in%omit,]
d1$lmax<-lmax$value

ggplot(na.omit(d1), aes(x=Year, y=size.95))+geom_point(col="maroon")+geom_line(col="red")+theme_bw()+facet_wrap(~Species, scales="free")+theme(legend.position="none")+geom_vline(xintercept=1985)+stat_smooth(col="black")+xlab("Year")+ylab("Mean size (cm)")+thm+xlim(1900, 1985)



sum.dat<-read.csv("~/Desktop/Work/Whaling EWS/Data/IWC data/All-Feb-13.csv")

sum.dat<-subset(sum.dat, Year>=1900)

melt.sum<-melt(sum.dat, id=c(1:12))

melt.fin<-melt(tapply(melt.sum$value, list(melt.sum$Year, melt.sum$variable), sum))
names(melt.fin)<-c("Year", "Species", "Landings")

melt.fin$unique.code<-paste(melt.fin$Species, melt.fin$Year)
d1$unique.code<-paste(d1$Species, d1$Year)

## add the individual data to the summary data
melt.fin $Mean.size <-d1$Mean.size[match(melt.fin $unique.code, d1$unique.code)]
melt.fin $SD.size <-d1$SD.size[match(melt.fin $unique.code, d1$unique.code)]
melt.fin $size.95 <-d1$size.95[match(melt.fin $unique.code, d1$unique.code)]
melt.fin $lmax <-d1$lmax[match(melt.fin $unique.code, d1$unique.code)]

##remove the unique column
melt.sum$unique.code<-NULL
d1$unique.code<-NULL
names(melt.sum)<-names(d1)

p1<-ggplot(na.omit(melt.fin), aes(x=Year, y=Landings))+geom_point(col="maroon")+geom_line(col="red")+theme_bw()+facet_grid(Species~., scales="free")+theme(legend.position="none")+geom_vline(xintercept=1985)+thm+ylab("Landings (number of whales)")+xlim(1900, 1985)

p2<-ggplot(na.omit(melt.fin), aes(x=Year, y=size.95))+geom_point(col="maroon")+geom_line(col="red")+theme_bw()+facet_grid(Species~., scales="free")+theme(legend.position="none")+geom_vline(xintercept=1985)+stat_smooth(col="black")+xlab("Year")+ylab("95th percentile of mean body size (cm)")+thm+xlim(1900, 1985)

ggplot(na.omit(melt.fin), aes(x=Year, y=lmax))+geom_point(col="maroon")+geom_line(col="red")+theme_bw()+facet_grid(Species~., scales="free")+theme(legend.position="none")+geom_vline(xintercept=1985)+stat_smooth(col="black")+xlab("Year")+ylab("95th percentile of mean body size (cm)")+thm+xlim(1900, 1985)

pdf("~/Desktop/Work/Whaling EWS/Plots/all specis time series.pdf", width=8, height=20)
grid.arrange(p1, p2, ncol=2)
dev.off()

##write out data
##write.csv(dd, "~/Desktop/Work/Whaling EWS/Data/IWC bound data/IWC ind. data Clapham response.csv")

write.csv(melt.fin, "~/Desktop/Work/Whaling EWS/Data/Combined data sets/IWC agg. data Clapham response.csv")

