## domains

### Stacey
llprj <- "+proj=longlat +datum=WGS84"
crp <- cbind(c(60, 60, 90, 90, 60), c(-80, -60, -60, -80, -80))
studybox <- sf::st_sfc(sf::st_polygon(list(crp)), crs = llprj)
## create area
library(sp)
atlantis <- rbgm::boxSpatial(rbgm::read_bgm(bgmfiles::bgmfiles("antarctica_28")))
box21 <- subset(atlantis, label == "Box21")
library(dplyr)
ant <- sf::read_sf("/home/shared/data/layers/add_ver6/cst01_polygon.shp") %>% dplyr::select(cst01srf) %>% 
  dplyr::filter(cst01srf %in% c("land", "ice shelf"))

domain <- sf::st_cast(sf::st_difference(sf::st_transform( sf::st_union(studybox, sf::st_transform(sf::st_as_sf(box21), sf::st_crs(studybox))), 
                                                          sf::st_crs(ant)), sf::st_union(ant)))

sf::st_area(sf::st_transform(domain, "+proj=laea +lon_0=75 +lat_0=-65 +datum=WGS84"))
## 1.474341e+12 [m^2]
domain <- sf::st_transform(domain, 4326)
saveRDS(domain, "/home/shared/data/ace-ecostats_model_domains_0/model_domains/Stacey/BanzareBank.rds")

# https://stackoverflow.com/questions/45231578/how-to-convert-rds-format-data-into-shp-format-in-r

# library(rgdal)
# library(sp)

x <- readRDS("model_domains/Stacey/BanzareBank.rds") 

# rgdal::writeOGR(x, "model_domains/Stacey", "BanzareBank", driver = "ESRI Shapefile")

# https://community.rstudio.com/t/how-to-convert-rds-file-list-with-four-elements-all-lists-to-an-esri-shapefile/124101

# library(sf)

# st_write(x, "model_domains/Stacey/Banzare_Bank.shp")

