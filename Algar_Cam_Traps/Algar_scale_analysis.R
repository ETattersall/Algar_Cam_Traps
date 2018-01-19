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
dist.5000 <- dist[which(dist<1.0)]
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
b500 <- gBuffer(Algcoord, width = 500, byid = TRUE) #byid=TRUE to apply function to subgeometries
summary(b500)#SpatialPolygons class, projected in NAD83, retains all station data

# Creating the 8 buffers to be used (250-2000m)
b250 <- gBuffer(Algcoord, width = 250, byid = TRUE)
b500 <- gBuffer(Algcoord, width = 500, byid = TRUE)
b750 <- gBuffer(Algcoord, width = 750, byid = TRUE)
b1000 <- gBuffer(Algcoord, width = 1000, byid = TRUE)
b1250 <- gBuffer(Algcoord, width = 1250, byid = TRUE)
b1500 <- gBuffer(Algcoord, width = 1500, byid = TRUE)
b1750 <- gBuffer(Algcoord, width = 1750, byid = TRUE)
b2000 <- gBuffer(Algcoord, width = 2000, byid = TRUE)
# Confirming buffers were drawn
plot(Algcoord)
plot(b500)
plot(b2000)


#### Extracting landcover data for that buffer ####
# comparing CRS between layers
proj4string(b500)# NAD83 utm zone 12
proj4string(AVIE)# NAD83 tmerc

# Converting AVIE to UTM
AVIE_UTM <- spTransform(AVIE, CRSobj = CRS(proj4string(Algcoord)))
proj4string(AVIE_UTM)
AVIE <- AVIE_UTM #Overwrite original AVIE CRS


#Clipping landcover data for buffers using rater::intersect
#over and gIntersects didn't work for clipping a polygon by a polygon
#gIntersection clipped the layer but did not retain attribute data



int500 <- intersect(AVIE, b500)
plot(int500)
int500@data
int250 <- intersect(AVIE, b250)
int500 <- intersect(AVIE, b500)
int750 <- intersect(AVIE, b750)
int5000 <- intersect(AVIE, b5000)
int1250 <- intersect(AVIE, b1250)
int1500 <- intersect(AVIE, b1500)
int1750 <- intersect(AVIE, b1750)
int2000 <- intersect(AVIE, b2000)

plot(int100)
plot(int250)
plot(int500)
plot(int750)
plot(int1000)
plot(int1250)
plot(int1500)
plot(int2000)

#### Writing for loop to extract AVIE polygons? ####
#Create a vector of buffers
Buffers <- c(b500,b250,b500,b750,b5000,b1250,b1500,b1750,b2000)
lnd <- rep(NA,9)# Empty vector to receive landcover data
for (i in 1:9){
  lnd[i] <- gIntersection(AVIE, Buffers[i], byid = T)
}
# Error in RGEOSBinTopoFunc(spgeom1, spgeom2, byid, id, drop_lower_td, unaryUnion_if_byid_false,  : 
# trying to get slot "proj4string" from an object of a 
# basic class ("list") with no slots
#####

#### Calculate area of each land cover class ####

#500m buffer
summary(int500) #Lowland occurs at all 60 sites, non-forest at 22, upland at 27
data500 <- int500@data #Repeats station data for each landcover type found at a site
# First 60 rows = all sites with lowland
# 61-82 = sites with non-forest
# 83-109 = sites with upland
A500 <- gArea(int500, byid = TRUE)#Calculates area for each landcover type at each station 
class(A500) #Returns numeric object containing 109 areas
data500$AREA_m <- A500 # Adding cover area to data
#Function for calculating total area of a buffer
bufferArea <- function(r){
  Area <- pi*(r)^2
  print(Area)
}

data500$TOTAREA <- bufferArea(500) #Adding total area 
data500$Percent_cover <-  data500$AREA_m/data500$TOTAREA #Calculating percent landcover

#250m buffer
summary(int250) #Lowland occurs at all 60 sites, non-forest at 34, upland at 34
data250 <- int250@data
A250 <- gArea(int250, byid = TRUE)#Calculates area for each landcover type at each station 
data250$AREA_m <- A250 # Adding cover area to data
data250$TOTAREA <- bufferArea(250) #Adding total area 
data250$Percent_cover <-  data250$AREA_m/data250$TOTAREA #Calculating percent landcover

#500m buffer
summary(int500) #Lowland occurs at all 60 sites, non-forest at 49, upland at 43
data500 <- int500@data
A500 <- gArea(int500, byid = TRUE)#Calculates area for each landcover type at each station 
data500$AREA_m <- A500 # Adding cover area to data
data500$TOTAREA <- bufferArea(500) #Adding total area 
data500$Percent_cover <-  data500$AREA_m/data500$TOTAREA #Calculating percent landcover

#750m buffer
summary(int750) #Lowland occurs at all 60 sites, non-forest at 54, upland at 46
data750 <- int750@data
A750 <- gArea(int750, byid = TRUE)#Calculates area for each landcover type at each station 
data750$AREA_m <- A750 # Adding cover area to data
data750$TOTAREA <- bufferArea(750) #Adding total area 
data750$Percent_cover <-  data750$AREA_m/data750$TOTAREA #Calculating percent landcover

#5000m buffer
summary(int5000) #Lowland occurs at all 60 sites, non-forest at 56, upland at 50
data5000 <- int5000@data
A5000 <- gArea(int5000, byid = TRUE)#Calculates area for each landcover type at each station 
data5000$AREA_m <- A5000 # Adding cover area to data
data5000$TOTAREA <- bufferArea(5000) #Adding total area 
data5000$Percent_cover <-  data5000$AREA_m/data5000$TOTAREA #Calculating percent landcover

#1250m buffer
summary(int1250) #Lowland occurs at all 60 sites, non-forest at 59, upland at 51
data1250 <- int1250@data
A1250 <- gArea(int1250, byid = TRUE)#Calculates area for each landcover type at each station 
data1250$AREA_m <- A1250 # Adding cover area to data
data1250$TOTAREA <- bufferArea(1250) #Adding total area 
data1250$Percent_cover <-  data1250$AREA_m/data1250$TOTAREA #Calculating percent landcover

#1500m buffer
summary(int1500) #Lowland occurs at all 60 sites, non-forest at 60, upland at 53
data1500 <- int1500@data
A1500 <- gArea(int1500, byid = TRUE)#Calculates area for each landcover type at each station 
data1500$AREA_m <- A1500 # Adding cover area to data
data1500$TOTAREA <- bufferArea(1500) #Adding total area 
data1500$Percent_cover <-  data1500$AREA_m/data1500$TOTAREA #Calculating percent landcover

#1750m buffer
summary(int1750) #Lowland occurs at all 60 sites, non-forest at 60, upland at 54
data1750 <- int1750@data
A1750 <- gArea(int1750, byid = TRUE)#Calculates area for each landcover type at each station 
data1750$AREA_m <- A1750 # Adding cover area to data
data1750$TOTAREA <- bufferArea(1750) #Adding total area 
data1750$Percent_cover <-  data1750$AREA_m/data1750$TOTAREA #Calculating percent landcover

#2000m buffer
summary(int2000) #Lowland occurs at all 60 sites, non-forest at 60, upland at 54
data2000 <- int2000@data
A2000 <- gArea(int2000, byid = TRUE)#Calculates area for each landcover type at each station 
data2000$AREA_m <- A2000 # Adding cover area to data
data2000$TOTAREA <- bufferArea(2000) #Adding total area 
data2000$Percent_cover <-  data2000$AREA_m/data2000$TOTAREA #Calculating percent landcover

#### Compiling a dataset of % lowland cover at different buffer sizes

lowland <- data500[1:60,]
lowland$AREA_m <- NULL
lowland$TOTAREA <- NULL
lowland$Percent_cover <- NULL

lowland$A500 <- data500$AREA_m[1:60]
lowland$TA500 <- data500$TOTAREA[1:60]
lowland$Prop500 <- data500$Percent_cover[1:60]

lowland$A250 <- data250$AREA_m[1:60]
lowland$TA250 <- data250$TOTAREA[1:60]
lowland$Prop250 <- data250$Percent_cover[1:60]

lowland$A500 <- data500$AREA_m[1:60]
lowland$TA500 <- data500$TOTAREA[1:60]
lowland$Prop500 <- data500$Percent_cover[1:60]

lowland$A750 <- data750$AREA_m[1:60]
lowland$TA750 <- data750$TOTAREA[1:60]
lowland$Prop750 <- data750$Percent_cover[1:60]

lowland$A1000 <- data1000$AREA_m[1:60]
lowland$TA1000 <- data1000$TOTAREA[1:60]
lowland$Prop1000 <- data1000$Percent_cover[1:60]

lowland$A1250 <- data1250$AREA_m[1:60]
lowland$TA1250 <- data1250$TOTAREA[1:60]
lowland$Prop1250 <- data1250$Percent_cover[1:60]

lowland$A1500 <- data1500$AREA_m[1:60]
lowland$TA1500 <- data1500$TOTAREA[1:60]
lowland$Prop1500 <- data1500$Percent_cover[1:60]

lowland$A1750 <- data1750$AREA_m[1:60]
lowland$TA1750 <- data1750$TOTAREA[1:60]
lowland$Prop1750 <- data1750$Percent_cover[1:60]

lowland$A2000 <- data2000$AREA_m[1:60]
lowland$TA2000 <- data2000$TOTAREA[1:60]
lowland$Prop2000 <- data2000$Percent_cover[1:60]

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

write.csv(lowland,"Lowlandcover_9buffersizes.csv")
