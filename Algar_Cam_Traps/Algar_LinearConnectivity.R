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
plot(Lines)

######### HF needs to be polylines, not polygons. Try using LinearFeatEastNTS_Algar10kClip from LinearFeat folder


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


##### Now have linear data clipped to 8 buffer sizes and length calculated for each segment within that buffer.
#### Line Density #### 
## 1) add up length of segments around each camera b) Calculate line density by dividing total length by buffer area 3) Compare across scales

## Line density unit = m/m^2

#Function for calculating total area of a buffer
bufferArea <- function(r){
  Area <- pi*(r)^2
  print(Area)
}

### 1) Sum length for each station
Length250 <- cbind.data.frame(data250$CamStation, data250$Length250)
colnames(Length250) <- c("CamStation", "Length250")
Length250 <- aggregate(.~CamStation, data = Length250, sum)

## 2) Line density
Length250$BufferArea <- bufferArea(250)
Length250$LineDensity <- Length250[ ,2]/ Length250[ , 3]


Length500 <- cbind.data.frame(data500$CamStation, data500$Length500)
colnames(Length500) <- c("CamStation", "Length500")
Length500 <- aggregate(.~CamStation, data = Length500, sum)
## 2) Line density
Length500$BufferArea <- bufferArea(500)
Length500$LineDensity <- Length500[ ,2]/ Length500[ , 3]

Length750 <- cbind.data.frame(data750$CamStation, data750$Length750)
colnames(Length750) <- c("CamStation", "Length750")
Length750 <- aggregate(.~CamStation, data = Length750, sum)
## 2) Line density
Length750$BufferArea <- bufferArea(750)
Length750$LineDensity <- Length750[ ,2]/ Length750[ , 3]

Length1000 <- cbind.data.frame(data1000$CamStation, data1000$Length1000)
colnames(Length1000) <- c("CamStation", "Length1000")
Length1000 <- aggregate(.~CamStation, data = Length1000, sum)
## 2) Line density
Length1000$BufferArea <- bufferArea(1000)
Length1000$LineDensity <- Length1000[ ,2]/ Length1000[ , 3]

Length1250 <- cbind.data.frame(data1250$CamStation, data1250$Length1250)
colnames(Length1250) <- c("CamStation", "Length1250")
Length1250 <- aggregate(.~CamStation, data = Length1250, sum)
## 2) Line density
Length1250$BufferArea <- bufferArea(1250)
Length1250$LineDensity <- Length1250[ ,2]/ Length1250[ , 3]

Length1500 <- cbind.data.frame(data1500$CamStation, data1500$Length1500)
colnames(Length1500) <- c("CamStation", "Length1500")
Length1500 <- aggregate(.~CamStation, data = Length1500, sum)
## 2) Line density
Length1500$BufferArea <- bufferArea(1500)
Length1500$LineDensity <- Length1500[ ,2]/ Length1500[ , 3]

Length1750 <- cbind.data.frame(data1750$CamStation, data1750$Length1750)
colnames(Length1750) <- c("CamStation", "Length1750")
Length1750 <- aggregate(.~CamStation, data = Length1750, sum)
## 2) Line density
Length1750$BufferArea <- bufferArea(1750)
Length1750$LineDensity <- Length1750[ ,2]/ Length1750[ , 3]

Length2000 <- cbind.data.frame(data2000$CamStation, data2000$Length2000)
colnames(Length2000) <- c("CamStation", "Length2000")
Length2000 <- aggregate(.~CamStation, data = Length2000, sum)
## 2) Line density
Length2000$BufferArea <- bufferArea(2000)
Length2000$LineDensity <- Length2000[ ,2]/ Length2000[ , 3]

#### 3) Compare across scales
Density_8scales <- cbind.data.frame(Length250$CamStation, Length250$LineDensity, Length500$LineDensity,Length750$LineDensity,Length1000$LineDensity, Length1250$LineDensity, Length1500$LineDensity,Length1750$LineDensity, Length2000$LineDensity )
colnames(Density_8scales) <- c("CamStation", "250m", "500m", "750m","1000m", "1250m", "1500m", "1750m", "2000m")

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
write.csv(Density_8scales, "AlgarStationLineDensity_8scales.csv")

# Rename columns (need to be numeric for plotting)
colnames(Density_8scales) <- c("CamStation", "250", "500", "750","1000", "1250", "1500", "1750", "2000")
#Gather Scales into one colum
Density_8scales <- gather(data = Density_8scales, key = Scale, value = LineDensity, 2:9)
str(Density_8scales) #Scale is character class, convert to numeric
Density_8scales$Scale <- as.numeric(Density_8scales$Scale)
str(Density_8scales)

## Plot LineDensity against Scale to compare
hist(Density_8scales$LineDensity) ##Skewed to lower densities
# Convert to km/km^2 (Multiply LineDensity by 1000)
Density_8scales$LineDensity <- Density_8scales$LineDensity*1000

require(ggplot2)
fig.a <- ggplot(data = Density_8scales, aes(x = Scale, y = LineDensity, fill = Scale)) + geom_point()
fig.a + geom_smooth(method = "auto") #Line density seems to exponentially decrease with increasing scale





#### Number of intersections ####
## Figure out how to find line intersections, count within buffer

## Take 1: (did not work) Use raster::intersect() to isolate intersections of lines
# Try with entire Lines layer (prior to clipping for camera stations) --> contains much more linear feature data than I need here--> File too big (3.1)
## Try with int500
#1. To avoid duplicating attribute data, copy layer and remove data from copy
int5001 <- int500
int5001 <- as(int5001,'SpatialPolygons')
summary(int5001)
plot(int5001)

Intersections <- raster::intersect(int500, int5001)
plot(int500, axes = T, xlab = "utmE", ylab = "utmN")
plot(Intersections, add= T,col = "red")
summary(Intersections)
Intdata500 <- Intersections@data
Intdata500 <- Intdata500[with(Intdata500, order(CamStation, Length_m)), ] ##240 observations, compared to 200 lines in 500m buffered data --> should be fewer observations (not every line intersects with another within the buffer)

###Take2: (worked) Try with gIntersects (no attribute data necessary)
Intersections <- gIntersects(int500, byid=T) #returns logical vector: TRUE if polygons have points in common
#Cannot add directly to data500 b/c it has been re-ordered
#Return to original order and visually validate Intersections
data500 <- int500@data
data500$Intersections <- gIntersects(int500, byid=T) 
## Now order
data500 <- data500[with(data500, order(CamStation, Length_m)), ]
## Check Intersections for stations that are easy to find: Algar01, 02, 46 --> does not indicate where intersections are

## Export lines with 500m buffer as shp file and explore in Arc
writeOGR(int500, dsn = "GIS", layer = "AlgarLines_500mbuffer", driver = "ESRI Shapefile")
## Problem could be that line segments aren't strictly linear; some polygons include intersecting segments --> therefore overlapping them won't show intersections
## Could count manually, but need to decide what scale to do this at....on hold


##### Scale analysis: deciding which linear density scale best predicts mammal detections with GLMs ####
## Create models where Species~low[scale] and compare with model selection
## Focus on scales between 250m - 1250m (greatest differences)
## Use glmmTMB, no random effects or ZI
require(glmmTMB)
require(bbmle)
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

#Load detection data
det <- read.csv("MonthlyDetections_nov2015-nov2017.csv")

#Load linedensity data
LineDens <- read.csv("AlgarStationLineDensity_8scales.csv")
LineDens$X <- NULL

## Densities are currently in m/m^2. Convert to km/km^2 (multiply by 1000)
LineDens_km <- LineDens[ , 2:9]*1000
LineDens_km <- cbind.data.frame(LineDens$CamStation, LineDens_km)
colnames(LineDens_km) <- c("CamStation", "X250m", "X500m", "X750m", "X1000m", "X1250m", "X1500m", "X1750m", "X2000m")
#Save LineDensities as km/km^2
write.csv(LineDens_km, "AlgarStationLineDensity_8scales.csv")



## Combine datasets (only for 5 scales)
det$LD250 <- LineDens$X250m[match(det$Site, LineDens$CamStation)]
det$LD500 <- LineDens$X500m[match(det$Site, LineDens$CamStation)]
det$LD750 <- LineDens$X750m[match(det$Site, LineDens$CamStation)]
det$LD1000 <- LineDens$X1000m[match(det$Site, LineDens$CamStation)]
det$LD1250 <- LineDens$X1250m[match(det$Site, LineDens$CamStation)]

### Wolf models
Wolf.0 <- glmmTMB(Wolf~1, data = det, family = nbinom2)
Wolf.250 <- glmmTMB(Wolf~LD250, data = det, family = nbinom2)
Wolf.500 <- glmmTMB(Wolf~LD500, data = det, family = nbinom2)
Wolf.750 <- glmmTMB(Wolf~LD750, data = det, family = nbinom2)
Wolf.1000 <- glmmTMB(Wolf~LD1000, data = det, family = nbinom2)
Wolf.1250 <- glmmTMB(Wolf~LD1250, data = det, family = nbinom2)

## Model selection with AICctab (bbmle) --> exluding VegHt
modnames <- c("NULL","250m", "500m", "750m", "1000m", "1250m")
wolftab <- ICtab(Wolf.0,Wolf.250,Wolf.500,Wolf.750,Wolf.1000,Wolf.1250, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
wolftab

#     dLogLik dAIC df weight
# NULL  0.0     0.0  2  0.30  
# 1250m 0.6     0.9  3  0.19  
# 750m  0.2     1.6  3  0.14  
# 1000m 0.2     1.6  3  0.13  
# 250m  0.1     1.9  3  0.12  
# 500m  0.0     2.0  3  0.11

summary(W.1250)
summary(W.750)
### No scale significantly better than others for wolves

## Caribou models
Caribou.0 <- glmmTMB(Caribou~1, data = det, family = nbinom2)
Caribou.250 <- glmmTMB(Caribou~LD250, data = det, family = nbinom2)
Caribou.500 <- glmmTMB(Caribou~LD500, data = det, family = nbinom2)
Caribou.750 <- glmmTMB(Caribou~LD750, data = det, family = nbinom2)
Caribou.1000 <- glmmTMB(Caribou~LD1000, data = det, family = nbinom2)
Caribou.1250 <- glmmTMB(Caribou~LD1250, data = det, family = nbinom2)

## Model selection with AICctab (bbmle) --> exluding VegHt
modnames <- c("NULL","250m", "500m", "750m", "1000m", "1250m")
cabtab <- ICtab(Caribou.0,Caribou.250,Caribou.500,Caribou.750,Caribou.1000,Caribou.1250, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
cabtab

#     dLogLik dAIC df weight
# 250m  2.9     0.0  3  0.661 
# NULL  0.0     3.8  2  0.100 
# 1250m 0.9     3.9  3  0.092 
# 1000m 0.6     4.6  3  0.066 
# 750m  0.1     5.5  3  0.042 
# 500m  0.0     5.7  3  0.038 

## 250m linear density strongest predictor
summary(Caribou.250)

## WTDeer models
WTDeer.0 <- glmmTMB(WTDeer~1, data = det, family = nbinom2)
WTDeer.250 <- glmmTMB(WTDeer~LD250, data = det, family = nbinom2)
WTDeer.500 <- glmmTMB(WTDeer~LD500, data = det, family = nbinom2)
WTDeer.750 <- glmmTMB(WTDeer~LD750, data = det, family = nbinom2)
WTDeer.1000 <- glmmTMB(WTDeer~LD1000, data = det, family = nbinom2)
WTDeer.1250 <- glmmTMB(WTDeer~LD1250, data = det, family = nbinom2)

## Model selection with AICctab (bbmle) 
modnames <- c("NULL","250m", "500m", "750m", "1000m", "1250m")
WTDtab <- ICtab(WTDeer.0,WTDeer.250,WTDeer.500,WTDeer.750,WTDeer.1000,WTDeer.1250, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
WTDtab

#      dLogLik dAIC df weight
# 750m  11.0     0.0 3  0.9703
# 1000m  7.4     7.3 3  0.0252
# 500m   5.4    11.2 3  0.0037
# 1250m  3.8    14.4 3  <0.001
# NULL   0.0    20.0 2  <0.001
# 250m   0.8    20.5 3  <0.001
summary(WTDeer.750)

## Moose models
Moose.0 <- glmmTMB(Moose~1, data = det, family = nbinom2)
Moose.250 <- glmmTMB(Moose~LD250, data = det, family = nbinom2)
Moose.500 <- glmmTMB(Moose~LD500, data = det, family = nbinom2)
Moose.750 <- glmmTMB(Moose~LD750, data = det, family = nbinom2)
Moose.1000 <- glmmTMB(Moose~LD1000, data = det, family = nbinom2)
Moose.1250 <- glmmTMB(Moose~LD1250, data = det, family = nbinom2)

## Model selection with AICctab (bbmle) --> exluding VegHt
modnames <- c("NULL","250m", "500m", "750m", "1000m", "1250m")
MOOSEtab <- ICtab(Moose.0,Moose.250,Moose.500,Moose.750,Moose.1000,Moose.1250, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
MOOSEtab
#     dLogLik dAIC df weight
# NULL  0.0     0.0  2  0.27  
# 750m  0.5     0.9  3  0.17  
# 1250m 0.4     1.1  3  0.16  
# 1000m 0.4     1.2  3  0.15  
# 500m  0.4     1.2  3  0.15  
# 250m  0.0     2.0  3  0.10  


### Bears: need truncated season
# Check black bear detections by yr_month
plot(x = det$Yr_Month, y = det$Blackbear) #Active April- November, both years
#Check against Snow
plot(x = det$SnowDays, y = det$Blackbear) # Predictably, more in months with fewer snow days

hist(det$Blackbear)

### Cropping data for active bear months only -- April - October -- use months rather than Yr_Months
unique(det$Month)
class(det$Month)



bear <- det %>% filter(Month >= 4 & Month <= 10) %>% select(Site, Treatment,Yr_Month, Site_ym, Blackbear, SnowDays, Year, Month, Dist2water_km, LD250,LD500,LD750,LD1000,LD1250)


plot(bear$Yr_Month, bear$Blackbear)
plot(bear$Month, bear$Blackbear)
hist(bear$Month)
hist(bear$Blackbear)

## Bear models
Blackbear.0 <- glmmTMB(Blackbear~1, data = bear, family = nbinom2)
Blackbear.250 <- glmmTMB(Blackbear~LD250, data = bear, family = nbinom2)
Blackbear.500 <- glmmTMB(Blackbear~LD500, data = bear, family = nbinom2)
Blackbear.750 <- glmmTMB(Blackbear~LD750, data = bear, family = nbinom2)
Blackbear.1000 <- glmmTMB(Blackbear~LD1000, data = bear, family = nbinom2)
Blackbear.1250 <- glmmTMB(Blackbear~LD1250, data = bear, family = nbinom2)

## Model selection with AICctab (bbmle) --> exluding VegHt
modnames <- c("NULL","250m", "500m", "750m", "1000m", "1250m")
Blackbeartab <- ICtab(Blackbear.0,Blackbear.250,Blackbear.500,Blackbear.750,Blackbear.1000,Blackbear.1250, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
Blackbeartab
#     dLogLik dAIC df weight
# 500m  3.2     0.0  3  0.368 
# 750m  3.1     0.2  3  0.334 
# 1000m 2.1     2.2  3  0.120 
# 250m  1.9     2.6  3  0.100 
# NULL  0.0     4.4  2  0.041 
# 1250m 0.9     4.6  3  0.037
