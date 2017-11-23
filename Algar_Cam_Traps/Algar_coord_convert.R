#######################################
# Algar_coord_convert.R
# Converting Station coordinates from UTM to lat long (Req'd for Camelot)
# Started by Erin T., Oct. 24, 2017
#######################################

library(dplyr)
library(rgdal)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
Stat <- read.csv("Station_data/AlgarStations_SiteDesc.csv")
head(Stat)
colnames(Stat)

##Separating out new stations, only utm data
Stat <- Stat[61:73,]


coords <- Stat %>% select(utmE, utmN)

sputm <- SpatialPoints(coords, 
                       proj4string=CRS("+proj=utm +zone=12V +datum=WGS84"))  
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
class(spgeo)
longlat <- as.data.frame(spgeo)
colnames(longlat) <- c("Longitude", "Latitude")
Stat <- cbind(coords, longlat)
head(Stat)
colnames(Stat)

write.csv(Stat, "Station_data/OffLine_coordinates.csv")


## For Camelot Bulk Import prep
Site.data <- Stat %>% select(CamStation, utmE, utmN, Longitude, Latitude, DeployDate, DeployTime_24h, Treatment, Elev_m, CamHeight_cm, CamDirection)
colnames(Site.data)
glimpse(Site.data)
write.csv(Site.data, "Station_data/Algar_DeployData.csv") ## Further modified and saved over to standardize formats in Camelot_bulk_import.R
