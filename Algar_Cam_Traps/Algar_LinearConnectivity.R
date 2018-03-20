######################################
# Linear_connectivity.R
# Exploring options for linear connectivity as a model covariate
# Started 20 March, 2018 by Erin T.
######################################

library(rgdal) #For loading and working with spatial data
library(tidyr) # For data manipulation
library(sp)    # Functions for working with spatial data 
library(rgeos)
library(raster) #For intersect function

setwd("C:/Users/ETattersall/Google Drive/Algar Seismic Restoration Project/GIS data")

## Algar camera sites
Algcoord <- readOGR("GIS", "AlgarSites_April2017")
summary(Algcoord) #utm

##Algar human footprint (ABMI, 2014; clipped to Algar site in Landcover_data_explore.R)
AlgarHF <- readOGR("GIS", "AlgarSites_HF_15kmbuffer")
summary(AlgarHF)
summary(AlgarHF$PUBLIC_COD) # Linear disturbances = Seismic lines (by far the most), pipelines, transmission lines, Road/Trail (Vegetated)

## Connectivity metric options:
#1. Line density (total length of lines within buffer/buffer area)
#2. Number of intersections
#3. Distance to intersections
#### All need to be measured at a few scales


#### Take 2: Re-clipping ABMI HF layer to desired scales ####
ABMI_HF <- readOGR("GIS", "CameraArray_ABMI_HFI_2014")
plot(ABMI_HF) ## HF for Christina Lakes, Algar, and Richardson
summary(ABMI_HF)

## Convert to UTM
HF_UTM <- spTransform(ABMI_HF, CRSobj = CRS(proj4string(Algcoord)))
proj4string(HF_UTM)
ABMI_HF <- HF_UTM #Overwrite original HF CRS
summary(ABMI_HF)
plot(ABMI_HF)

## Step 1: Subset HF layer for linear disturbances only
## Only interested in area <2km around cam stations at most. Most prevalent linear disturbances will be seismic line, with a few pipelines (1-2 cams are on pipelines)
## Subset HF for seismic and pipe lines in Algar
Lines <- ABMI_HF[ABMI_HF$PUBLIC_COD=="Seismic line" | ABMI_HF$PUBLIC_COD=="Pipeline" , ]
summary(Lines)

## Clip ABMI_HF to 8 scales around Algar cameras only
# Creating the 8 buffers to be used (250-2000m)
b250 <- gBuffer(Algcoord, width = 250, byid = TRUE)
b500 <- gBuffer(Algcoord, width = 500, byid = TRUE)
b750 <- gBuffer(Algcoord, width = 750, byid = TRUE)
b1000 <- gBuffer(Algcoord, width = 1000, byid = TRUE)
b1250 <- gBuffer(Algcoord, width = 1250, byid = TRUE)
b1500 <- gBuffer(Algcoord, width = 1500, byid = TRUE)
b1750 <- gBuffer(Algcoord, width = 1750, byid = TRUE)
b2000 <- gBuffer(Algcoord, width = 2000, byid = TRUE)

## Algcoord and Lines are in same UTM projection already, move on to clip
int250 <- raster::intersect(Lines, b250)
plot(int250) #worked correctly
summary(int250) #attribute data retained
data250 <- int250@data #131 lines, each unique (checked with unique(data250))


## Length is total length of line segment, not length within buffer. Verify by calculating length and comparing
int250$Length250<- gLength(int250, byid = TRUE)
data250 <- int250@data
#Order by Station and Length
data250 <- data250[with(data250, order(CamStation, Length_m)), ] ### Calculated lengths are not = to original lengths, but seem too long. Compare as buffer size increases


int500 <- raster::intersect(Lines, b500)
int500$Length500<- gLength(int500, byid = TRUE)
data500 <- int500@data
#Order by Station and Length
data500 <- data500[with(data500, order(CamStation, Length_m)), ]


int750 <- raster::intersect(Lines, b750)
int750$Length750<- gLength(int750, byid = TRUE)
data750 <- int750@data
#Order by Station and Length
data750 <- data750[with(data750, order(CamStation, Length_m)), ]

int1000 <- raster::intersect(Lines, b1000)
int1000$Length1000<- gLength(int1000, byid = TRUE)
data1000 <- int1000@data
#Order by Station and Length
data1000 <- data1000[with(data1000, order(CamStation, Length_m)), ]

int1250 <- raster::intersect(Lines, b1250)
int1250$Length1250<- gLength(int1250, byid = TRUE)
data1250 <- int1250@data
#Order by Station and Length
data1250 <- data1250[with(data1250, order(CamStation, Length_m)), ]

int1500 <- raster::intersect(Lines, b1500)
int1500$Length1500<- gLength(int1500, byid = TRUE)
data1500 <- int1500@data
#Order by Station and Length
data1500 <- data1500[with(data1500, order(CamStation, Length_m)), ]

int1750 <- raster::intersect(Lines, b1750)
int1750$Length1750<- gLength(int1750, byid = TRUE)
data1750 <- int1750@data
#Order by Station and Length
data1750 <- data1750[with(data1750, order(CamStation, Length_m)), ]


int2000 <- raster::intersect(Lines, b2000)
int2000$Length2000<- gLength(int2000, byid = TRUE)
data2000 <- int2000@data
#Order by Station and Length
data2000 <- data2000[with(data2000, order(CamStation, Length_m)), ] ## Number of lines per site increases, total length increases
