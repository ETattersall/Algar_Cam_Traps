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
library(dplyr)

## Connectivity metric options:
#1. Line density (total length of lines within buffer/buffer area)
#2. Number of intersections
#3. Distance to intersections
#### All need to be measured at a few scales

setwd("C:/Users/ETattersall/Google Drive/Algar Seismic Restoration Project/GIS data")

## Algar camera sites
Algcoord <- readOGR("GIS", "AlgarSites_April2017")
summary(Algcoord) #utm

##Algar human footprint (ABMI, 2014; clipped to Algar site in Landcover_data_explore.R)
AlgarHF <- readOGR("GIS", "AlgarSites_HF_15kmbuffer")
summary(AlgarHF)
summary(AlgarHF$PUBLIC_COD) # Linear disturbances = Seismic lines (by far the most), pipelines, transmission lines, Road/Trail (Vegetated)

AlgLines <- readOGR("LinearFeat", "LinearFeatNTS_Algar10kClip")
summary(AlgLines) #FeatureTyp = 3D, cutline, Electrical Transmission Line, Trail. Should keep all
#In tmerc, units = m
## Convert to UTM
HF_UTM <- spTransform(AlgLines, CRSobj = CRS(proj4string(Algcoord)))
proj4string(HF_UTM)
AlgLines <- HF_UTM #Overwrite original HF CRS
summary(AlgLines)
plot(AlgLines)

#####Take 3: HF needs to be polylines, not polygons. Try using LinearFeatEastNTS_Algar10kClip from LinearFeat folder ####


## Write function for calculating Line Density at desired buffer sizes, using rgeos, raster, and dplyr packages
#Function for calculating total area of a buffer
bufferArea <- function(r){
  Area <- pi*(r)^2
  print(Area)
}
LineDens <- function(buffer){
  a <- gBuffer(Algcoord, width = buffer, byid = TRUE) #Create buffer
  b <- raster::intersect(AlgLines, a)#Clip Line data
  b$Length <- gLength(b, byid = TRUE)
  data <- b@data #Isolate dataframe
  data <- data[with(data, order(CamStation, Length)), ] #Order by CamStation and Length
  Length <- cbind.data.frame(data$CamStation, data$Length)#Create dataframe for length & line density
  colnames(Length) <- c("CamStation", "Length")
  Length <- aggregate(.~CamStation, data = Length, sum) #Sum total length for each station
  Length$BufferArea <- bufferArea(buffer)
  Length$LineDensity <- Length[ ,2]/Length[ ,3] #LineDensity in m/m^2
  Length$LineDensity_km <- Length$LineDensity*1000 #Converting to km/km^2
  Length <- Length[1:60,]
}

### LineDensity at 8 buffer sizes
LD250 <- LineDens(250)
LD500 <- LineDens(500)
LD750 <- LineDens(750)
LD1000 <- LineDens(1000)
LD1250 <- LineDens(1250)
LD1500 <- LineDens(1500)
LD1750 <- LineDens(1750)
LD2000 <- LineDens(2000)

LD_Lines <- cbind.data.frame(LD250$CamStation, LD250$LineDensity_km, LD500$LineDensity_km,LD750$LineDensity_km,LD1000$LineDensity_km, LD1250$LineDensity_km, LD1500$LineDensity_km,LD1750$LineDensity_km, LD2000$LineDensity_km )
colnames(LD_Lines) <- c("CamStation", "250m", "500m", "750m","1000m", "1250m", "1500m", "1750m", "2000m")

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
write.csv(LD_Lines, "AlgarStationsLD_Lines.csv")

# Rename columns (need to be numeric for plotting)
colnames(LD_Lines) <- c("CamStation", "250", "500", "750","1000", "1250", "1500", "1750", "2000")
#Gather Scales into one colum
LD_Lines <- gather(data = LD_Lines, key = Scale, value = LineDensity, 2:9)
str(LD_Lines) #Scale is character class, convert to numeric
LD_Lines$Scale <- as.numeric(LD_Lines$Scale)
str(LD_Lines)

## Plot LineDensity against Scale to compare
hist(LD_Lines$LineDensity) ##Skewed to lower densities


require(ggplot2)
fig.a <- ggplot(data = LD_Lines, aes(x = Scale, y = LineDensity, fill = Scale)) + geom_point()
fig.a + geom_smooth(method = "auto") #Line density seems to exponentially decrease with increasing scale

#Compare to earlier calculations from polygons
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
LD_polygons <- read.csv("AlgarStationLineDensity_8scales.csv")
LD_polygons$X <- NULL

# Rename columns (need to be numeric for plotting)
colnames(LD_polygons) <- c("CamStation", "250", "500", "750","1000", "1250", "1500", "1750", "2000")
#Gather Scales into one colum
LD_polygons <- gather(data = LD_polygons, key = Scale, value = LineDensity, 2:9)
str(LD_polygons) #Scale is character class, convert to numeric
LD_polygons$Scale <- as.numeric(LD_polygons$Scale)
str(LD_polygons)

## Plot LineDensity against Scale to compare
hist(LD_polygons$LineDensity) ##Skewed to lower densities


require(ggplot2)
fig.a <- ggplot(data = LD_polygons, aes(x = Scale, y = LineDensity, fill = Scale)) + geom_point()
fig.a + geom_smooth(method = "auto")

#### Same pattern, just less length (absolute line density vs relative)

#### Number of intersections using polylines ####
#Function for creating a buffer around points and clipping spatial data around that buffer
SpClip <- function(points, SpData, buffer){
  a <- gBuffer(points, width = buffer, byid = TRUE) #Create buffer
  b <- raster::intersect(SpData, a) #Clipping data within the buffer
}

a250 <- SpClip(Algcoord, AlgLines, buffer = 250)
ogrDrivers()
writeOGR(a250, dsn = "LinearFeat", driver= "ESRI Shapefile", "LinearFeat_250mbuffer")
plot(a250)

# Find coordinates for points where lines overlap at each station, then filter for unique points
# gIntersection(), then unique()??

#Create unique identifiers for line segments at each camstations
a250$LineID <- paste(a250$CamStation, a250$OBJECTID_1, sep = "_")
summary(a250)
data250 <- a250@data

Intersections <- as.data.frame(gIntersects(a250, byid=T)) # matrix of all 132 lines indicating lines they intersect. BUT lines are not individually ID'd in a recognizable way
#Convert to numeric
Intersections <- Intersections + 0 # + 0 converts logical into numeric

#Name rows and columns with unique identifiers (can't be sure they are the same, but a 750 hasn't been re-ordered so there's no reason it shouldn't be)
colnames(Intersections) <- data250$LineID
row.names(Intersections) <- data250$LineID

#Sum across Intersections
sum(Intersections) #224 --> Subtract 132 duplicates = 92 intersections


## Find cells that are 1's
Inter1 <- as.data.frame(which(Intersections !=0, arr.ind = T)) #arr.ind = T --> each row gives row and column location of the non-zero value
class(Inter1)
## Remove if row value = column value (duplicate, line matched with itself)
Inter2 <- Inter1[which(!Inter1$row == Inter1$col),]
## Add CamStations to In




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
LineDens <- read.csv("AlgarStationsLD_Lines.csv")
LineDens$X <- NULL




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
# NULL  0.0     0.0  2  0.34  
# 1250m 0.2     1.6  3  0.15  
# 250m  0.1     1.9  3  0.13  
# 500m  0.0     1.9  3  0.13  
# 750m  0.0     2.0  3  0.13  
# 1000m 0.0     2.0  3  0.13

summary(Wolf.1250)
summary(Wolf.750)
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

#      dLogLik dAIC df weight
# 250m  1.9     0.0  3  0.431 
# NULL  0.0     1.8  2  0.177 
# 1250m 0.8     2.1  3  0.148 
# 1000m 0.4     3.1  3  0.093 
# 500m  0.3     3.2  3  0.085 
# 750m  0.0     3.8  3  0.066 

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
# 750m  13.7     0.0 3  0.986 
# 1000m  9.4     8.6 3  0.014 
# 500m   6.2    15.0 3  <0.001
# 1250m  5.2    16.9 3  <0.001
# NULL   0.0    25.4 2  <0.001
# 250m   0.7    25.9 3  <0.001
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
# NULL  0.0     0.0  2  0.29  
# 1250m 0.4     1.2  3  0.16  
# 1000m 0.3     1.3  3  0.15  
# 750m  0.3     1.4  3  0.15  
# 500m  0.3     1.5  3  0.14  
# 250m  0.1     1.9  3  0.11  


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
# 750m  4.3     0.0  3  0.440 
# 500m  3.9     0.8  3  0.299 
# 1000m 3.2     2.2  3  0.149 
# 250m  2.3     4.0  3  0.059 
# 1250m 1.8     5.0  3  0.037 
# NULL  0.0     6.6  2  0.017


#### Linear density with ABMI HF: Re-clipping ABMI HF layer to desired scales (works, but wrong layer) ####
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
#####

#### Checking collinearity between intersections and LD ####
## Manually extracting intersections for 12 camera stations
summary(Algcoord)
## Subsetting points for 12 stations (3 of each treatment, spread across landscape)
s1 <- Algcoord[Algcoord$CamStation == "Algar01" | Algcoord$CamStation == "Algar54" | Algcoord$CamStation == "Algar32" | Algcoord$CamStation == "Algar28" | Algcoord$CamStation == "Algar12" | Algcoord$CamStation == "Algar59" | Algcoord$CamStation == "Algar31" | Algcoord$CamStation == "Algar24" | Algcoord$CamStation == "Algar08" | Algcoord$CamStation == "Algar34" | Algcoord$CamStation == "Algar47" | Algcoord$CamStation == "Algar16", ]
summary(s1)
plot(s1)

### Clip AlgLines to 12 points for buffers between 250m -1250m
b250 <- SpClip(s1, AlgLines, 250)
b500 <- SpClip(s1, AlgLines, 500)
b750 <- SpClip(s1, AlgLines, 750)
b1000 <- SpClip(s1, AlgLines, 1000)
b1250 <- SpClip(s1, AlgLines, 1250)

## Plot each to count intersections
plot(b250)
plot(b500)
plot(b750)
plot(b1000)
plot(b1250)

## Recorded in Line_Int_Collinearities.csv (intersection count rough - difficult to determine at low resolution)
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
Col <- read.csv("Line_Int_Collinearities.csv")

plot(Col$LD_250, Col$Int_250)
plot(Col$LD_500, Col$Int_500)
plot(Col$LD_750, Col$Int_750)
plot(Col$LD_1000, Col$Int_1000)
plot(Col$LD_1250, Col$Int_1250)

## Positively correlated. Will therefore remove intersections from analysis