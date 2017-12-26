############################
# Algar_scale_analysis.R
# Determining which buffer sizes to use when modeling landscape variables for seismic line restoration analysis
# Started by Erin T. Dec. 26, 2017
# Code, advice provided by Gillian Fraser, Fisher et al., 2011
############################

library(rgdal) #For loading and working with spatial data
library(ggmap) #Extends ggplot2 for maps
library(dplyr)
library(tidyr) # For data manipulation
library(tmap)  # For making pretty maps
library(sp)    # Functions for working with spatial data 

# Spatial data for Algar located on Algar Project Google Drive
setwd("C:/Users/ETattersall/Google Drive/Algar Seismic Restoration Project/GIS data")
list.files() #Google Drive is syncing, may take a while to show up...


#Algar Camera stations (Lat-long coordinates retrieved from AlgarStations60.csv)
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data/Station_data")
Stat <- read.csv("AlgarStations60.csv")
summary(Stat$Longitude) #Min = -112.6, Max = -112.4
summary(Stat$Latitude) #Min = 56.17, Max = 56.48

#### Determining distances between points ####

#Uses sp function spDists

pts <- as.matrix(cbind(Stat$Longitude, Stat$Latitude)) #Create matrix of coordinates, where col1 = Long and col2 = Lat
row.names(pts) <- Stat$CamStation
colnames(pts) <- c("Longitude", "Latitude")
pts

dist <- spDists(x = pts, y = pts, longlat = TRUE) #Returns distances from all points to all points in km
class(dist) #matrix
dist <- as.vector(dist) #Coerces into vector
dist <- sort(dist) #Sorting distances into ascending order
mean(dist) #12.73439 km

max(dist) #36.60532 km
min(dist) #0 Distance between station and itself
hist(dist)
#Remove 0's
dist %in% 0 #Returns logical value for which elements are 0
dist <- dist[!dist== 0]
0 %in% dist #Returns logical value for whether ANY 0s are in dist --> FALSE
min(dist) #0.3382544km -->338m
head(dist)
tail(dist)
hist(dist) #Mode occurs ~8km
median(dist) #11.6673
dist.5000 <- dist[which(dist<5.0)] #540 distances of 3540 that are <5km
hist(dist.5000)
dist.4000 <- dist[which(dist<4.0)]
dist.2000 <- dist[which(dist<2.0)]
dist.1000 <- dist[which(dist<1.0)]
540/3540 # 15.25% of distances
388/3540 # 10.96
104/3540 # 2.94% of distances
26/3540  # 0.73% of distances
