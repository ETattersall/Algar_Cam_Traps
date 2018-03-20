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

## Step 1: Subset HF layer for linear disturbances only
## Only interested in area <2km around cam stations at most. Most prevalent linear disturbances will be seismic line, with a few pipelines (1-2 cams are on pipelines)
## Subset HF for seismic and pipe lines in Algar
Lines <- AlgarHF[AlgarHF$PUBLIC_COD=="Seismic line" | AlgarHF$PUBLIC_COD=="Pipeline" , ]
summary(Lines) ## UTM. Also includes cam station data, which is not necessary (will be added by clip with Algcoord)
str(Lines@data) #keep columns 1:3 only (just need linear attribute data)
Linesdata <- Lines@data
Lines <- Lines[,1:3]
Linesdata <- Lines@data
summary(Lines) #retain spatial and line attributes
summary(Linesdata)
hist(Linesdata$Length_m)

## Step 2: Create buffers around cam sites and extract line data within buffers (from Algar_scale_analysis.R)

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

int500 <- raster::intersect(Lines, b500)
int750 <- raster::intersect(Lines, b750)
int1000 <- raster::intersect(Lines, b1000)
int1250 <- raster::intersect(Lines, b1250)
int1500 <- raster::intersect(Lines, b1500)
int1750 <- raster::intersect(Lines, b1750)
int2000 <- raster::intersect(Lines, b2000)

data250 <- int250@data

data500 <- int500@data
## Length_m = total line's length. But may not accurately depict functional linear length, as some seismic lines are linked to pipelines and other seismic lines
## Trying to figure out what each row is in int250, int500, etc
table(data250$Length_m) 
table(data250$CamStation)
## Order data250 by Station
data250 <- data250[with(data250, order(CamStation, Length_m)), ]
## Many rows are duplicated. 
data250 <- unique(data250) #Remove duplicates
data250 <- data250[with(data250, order(CamStation, Length_m)), ] #Now number of lines at each station matches what is shown when layer is plotted (roughly; some lines that appear continuous may actually be 2 separate lines)
table(data250$CamStation)

## Removing duplicates in dataframes for all
plot(int500)
data500 <- int500@data
data500 <- unique(data500) #Remove duplicates
data500 <- data500[with(data500, order(CamStation, Length_m)), ]
table(data500$CamStation)

data750 <- int750@data
data750 <- unique(data750) #Remove duplicates
data750 <- data750[with(data750, order(CamStation, Length_m)), ]

data1000 <- int1000@data
data1000 <- unique(data1000) #Remove duplicates
data1000 <- data1000[with(data1000, order(CamStation, Length_m)), ]
table(data1000$CamStation)

data1250 <- int1250@data
data1250 <- unique(data1250) #Remove duplicates
data1250 <- data1250[with(data1250, order(CamStation, Length_m)), ]
table(data1250$CamStation)

data1500 <- int1500@data
data1500 <- unique(data1500) #Remove duplicates
data1500 <- data1500[with(data1500, order(CamStation, Length_m)), ]
table(data1500$CamStation)

data1750 <- int1750@data
data1750 <- unique(data1750) #Remove duplicates
data1750 <- data1750[with(data1750, order(CamStation, Length_m)), ]
table(data1750$CamStation)

data2000 <- int2000@data
data2000 <- unique(data2000) #Remove duplicates
data2000 <- data2000[with(data2000, order(CamStation, Length_m)), ]
table(data2000$CamStation)

## BUT calculating length

##3. Calculate line density
#a. is calculated length the same as length_m?
int250$Length250<- gLength(int250, byid = TRUE)

summary(data250)

int500$Length500 <- gLength(int500, byid = TRUE)
data500 <- int500@data
data500 <- unique(data500) #Remove duplicates
data500 <- data500[with(data500, order(CamStation, Length_m)), ]
summary(data500)
