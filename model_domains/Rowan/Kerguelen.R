
### Rowan
llprj <- "+proj=longlat +datum=WGS84"
crp <- cbind(c(60, 60, 90, 90, 60), c(-65, -45, -45, -65, -65))

studybox <- sf::st_sfc(sf::st_polygon(list(crp)), crs = llprj)
## create area
library(sp)
#atlantis <- rbgm::boxSpatial(rbgm::read_bgm(bgmfiles::bgmfiles("antarctica_28")))
#box21 <- subset(atlantis, label == "Box21")
library(dplyr)
south <- readRDS("/home/shared/data/ace-ecostats_model_domains_0/model_domains/Stacey/BanzareBank.rds")

domain <- sf::st_cast(sf::st_difference(studybox, 
                                        south))



sf::st_area(sf::st_transform(domain, "+proj=laea +lon_0=75 +lat_0=-65 +datum=WGS84"))
## 2.968938e+12 [m^2]

saveRDS(domain, "/home/shared/data/ace-ecostats_model_domains_0/model_domains/Rowan/Kerguelen.rds")
