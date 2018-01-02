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
library(raster)# Manipulating raster data
library(rgeos)

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
540/3540 # 15.25% of distances are less that 5km
388/3540 # 10.96 of distances are less than 4km
104/3540 # 2.94% of distances are less than 2km
26/3540  # 0.73% of distances are less than 1km
# I will test 8 buffer sizes from 250m - 2000m at intervals of 250m

###### Drawing buffers on AVI habitat data ####
# Load AVIE data
AVIE <- readOGR("GIS", "AVIE_Veg_simple")

summary(AVIE) # Spatial polygons data frame, projected in NAD83. Lowland, Non-forest, and upland categories
class(AVIE)
AVIE



#Camera station data
Algcoord <- readOGR("GIS", "AlgarSites_April2017")
summary(Algcoord)
plot(Algcoord, pch = 18, col = "red")
Cams <- Algcoord@coords #matrix of coordinates
Algcoord@data


## Visualizing features ##
plot(AVIE, col = "lightgreen") 
# Addin colour to lowland areas 
low <- AVIE$Veg_simple == "Lowland"
plot(AVIE[ low, ], col = "darkgreen", add = TRUE)
# Adding colour to nonforest areas
nofo <- AVIE$Veg_simple == "Non-forest"
plot(AVIE[ nofo, ], col = "lightblue", add = TRUE)

## I want % lowland habitat (AVIE$Veg_simple == "Lowland")

# Creating buffer around Camera stations
b100 <- gBuffer(Algcoord, width = 100)
summary(b100)#SpatialPolygons class, projected in NAD83

# Creating the 8 buffers to be used (250-2000)
b250 <- gBuffer(Algcoord, width = 250)
b500 <- gBuffer(Algcoord, width = 500)
b750 <- gBuffer(Algcoord, width = 750)
b1000 <- gBuffer(Algcoord, width = 1000)
b1250 <- gBuffer(Algcoord, width = 1250)
b1500 <- gBuffer(Algcoord, width = 1500)
b1750 <- gBuffer(Algcoord, width = 1750)
b2000 <- gBuffer(Algcoord, width = 2000)



#Extracting landcover data for that buffer

# Need to first convert AVIE into class raster
r <- raster(ncol = 100, nrow = 100)
AVIEraster <- rasterize(AVIE, r, fun = "first", update = TRUE, updateValue = "all")
summary(AVIEraster) #Returns object of NA :(
AVIEraster@data


#Tried method for x = Raster, y = SpatialPolygons
# AVIE data is a spatial polygons dataframe
lw100 <- extract(x=AVIE@polygons, 
                 y = b100, 
                 fun=NULL, 
                 na.rm=FALSE,
                 weights=FALSE,
                 normalizeWeights=TRUE,
                 cellnumbers=FALSE,
                 small=TRUE,
                 df=FALSE, 
                 layer, 
                 nl, 
                 factors=FALSE, 
                 sp=FALSE)
summary(b100) # Returns an empty matrix for poly.ID and Veg_simple
