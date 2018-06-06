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

## Algar study area
Algar <- readOGR("GIS", "AVIE_Veg_simple")
plot(Algar)
summary(Algar)
gArea(Algar)

#### Linear density of total area
# Clip AlgLines to Algar area
AL1 <- raster::intersect(AlgLines, Algar)
plot(AL1)
summary(AL1)
gLength(AL1)

# Lengths of linear features
Seismic <- gLength(AL1[AL1$FeatureTyp == "Cutline", ])/1000 #523.6 km
Pipeline <- gLength(AL1[AL1$FeatureTyp == "Pipeline", ])/1000 #63.14 km
Trail <- gLength(AL1[AL1$FeatureTyp == "Trail", ])/1000 #65.60 km
plot(AL1[AL1$FeatureTyp=="Cutline", ])
plot(AL1[AL1$FeatureTyp=="Trail", ])
plot(AL1[AL1$FeatureTyp=="Pipeline", ])

# Compare to other layer of lines
AL2 <- readOGR("GIS", "Algar_StudyArea_Seismic_line")
plot(AL2)
summary(AL2)

## Calculate linear density
LDtotal <- (gLength(AL1)/gArea(Algar))*1000 ## 1.147 km/km^2

# Compare to other layer of lines
AL2 <- readOGR("GIS", "Algar_StudyArea_Seismic_line")
plot(AL2)
summary(AL2)
gLength(AL2)
LDtotal2 <- (gLength(AL2)/gArea(Algar))*1000 ## 2.01 km/km^2 --> higher because of overlap and line object fragmentation?


## Convert AlgLines to UTM
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
LD2250 <- LineDens(2250)
LD2500 <- LineDens(2500)
LD2750 <- LineDens(2750)
LD3000 <- LineDens(3000)
LD3250 <- LineDens(3250)
LD3500 <- LineDens(3500)
LD3750 <- LineDens(3750)
LD4000 <- LineDens(4000)
LD4250 <- LineDens(4250)
LD4500 <- LineDens(4500)
LD4750 <- LineDens(4750)
LD5000 <- LineDens(5000)

LD_Lines <- cbind.data.frame(LD250$CamStation, LD250$LineDensity_km, LD500$LineDensity_km,LD750$LineDensity_km,LD1000$LineDensity_km, LD1250$LineDensity_km, LD1500$LineDensity_km,LD1750$LineDensity_km, LD2000$LineDensity_km, LD2250$LineDensity_km, LD2500$LineDensity_km, LD2750$LineDensity_km, LD3000$LineDensity_km, LD3250$LineDensity_km,LD3500$LineDensity_km,LD3750$LineDensity_km,LD4000$LineDensity_km,LD4250$LineDensity_km,LD4500$LineDensity_km,LD4750$LineDensity_km,LD5000$LineDensity_km )
colnames(LD_Lines) <- c("CamStation", "250m", "500m", "750m","1000m", "1250m", "1500m", "1750m", "2000m", "2250m", "2500m", "2750m", "3000m", "3250m", "3500m", "3750m","4000m","4250m","4500m","4750m","5000m")

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
write.csv(LD_Lines, "AlgarStationsLD_Lines.csv")

# Rename columns (need to be numeric for plotting)
colnames(LD_Lines) <- c("CamStation", "250", "500", "750","1000", "1250", "1500", "1750", "2000", "2250", "2500", "2750", "3000", "3250", "3500", "3750","4000","4250","4500","4750","5000")
#Gather Scales into one colum
LD_Lines <- gather(data = LD_Lines, key = Scale, value = LineDensity, 2:21)
str(LD_Lines) #Scale is character class, convert to numeric
LD_Lines$Scale <- as.numeric(LD_Lines$Scale)
str(LD_Lines)

## Plot LineDensity against Scale to compare
hist(LD_Lines$LineDensity) ##Skewed to lower densities


require(ggplot2)
fig.a <- ggplot(data = LD_Lines, aes(x = Scale, y = LineDensity, fill = Scale)) + geom_point()
fig.a + geom_smooth(method = "auto") #Line density seems to exponentially decrease with increasing scale

##### Compare to earlier calculations from polygons ####
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




## Combine datasets
det$LD250 <- LineDens$X250m[match(det$Site, LineDens$CamStation)]
det$LD500 <- LineDens$X500m[match(det$Site, LineDens$CamStation)]
det$LD750 <- LineDens$X750m[match(det$Site, LineDens$CamStation)]
det$LD1000 <- LineDens$X1000m[match(det$Site, LineDens$CamStation)]
det$LD1250 <- LineDens$X1250m[match(det$Site, LineDens$CamStation)]
det$LD1500 <- LineDens$X1500m[match(det$Site, LineDens$CamStation)]
det$LD1750 <- LineDens$X1750m[match(det$Site, LineDens$CamStation)]
det$LD2000 <- LineDens$X2000m[match(det$Site, LineDens$CamStation)]
det$LD2250 <- LineDens$X2250m[match(det$Site, LineDens$CamStation)]
det$LD2500 <- LineDens$X2500m[match(det$Site, LineDens$CamStation)]
det$LD2750 <- LineDens$X2750m[match(det$Site, LineDens$CamStation)]
det$LD3000 <- LineDens$X3000m[match(det$Site, LineDens$CamStation)]
det$LD3250 <- LineDens$X3250m[match(det$Site, LineDens$CamStation)]
det$LD3500 <- LineDens$X3500m[match(det$Site, LineDens$CamStation)]
det$LD3750 <- LineDens$X3750m[match(det$Site, LineDens$CamStation)]
det$LD4000 <- LineDens$X4000m[match(det$Site, LineDens$CamStation)]
det$LD4250 <- LineDens$X4250m[match(det$Site, LineDens$CamStation)]
det$LD4500 <- LineDens$X4500m[match(det$Site, LineDens$CamStation)]
det$LD4750 <- LineDens$X4750m[match(det$Site, LineDens$CamStation)]
det$LD5000 <- LineDens$X5000m[match(det$Site, LineDens$CamStation)]


### Wolf models
Wolf.0 <- glmmTMB(Wolf~1, data = det, family = nbinom2)
Wolf.250 <- glmmTMB(Wolf~LD250, data = det, family = nbinom2)
Wolf.500 <- glmmTMB(Wolf~LD500, data = det, family = nbinom2)
Wolf.750 <- glmmTMB(Wolf~LD750, data = det, family = nbinom2)
Wolf.1000 <- glmmTMB(Wolf~LD1000, data = det, family = nbinom2)
Wolf.1250 <- glmmTMB(Wolf~LD1250, data = det, family = nbinom2)
Wolf.1500 <- glmmTMB(Wolf~LD1500, data = det, family = nbinom2)
Wolf.1750 <- glmmTMB(Wolf~LD1750, data = det, family = nbinom2)
Wolf.2000 <- glmmTMB(Wolf~LD2000, data = det, family = nbinom2)
Wolf.2250 <- glmmTMB(Wolf~LD2250, data = det, family = nbinom2)
Wolf.2500 <- glmmTMB(Wolf~LD2500, data = det, family = nbinom2)
Wolf.2750 <- glmmTMB(Wolf~LD2750, data = det, family = nbinom2)
Wolf.3000 <- glmmTMB(Wolf~LD3000, data = det, family = nbinom2)
Wolf.3250 <- glmmTMB(Wolf~LD3250, data = det, family = nbinom2)
Wolf.3500 <- glmmTMB(Wolf~LD3500, data = det, family = nbinom2)
Wolf.3750 <- glmmTMB(Wolf~LD3750, data = det, family = nbinom2)
Wolf.4000 <- glmmTMB(Wolf~LD4000, data = det, family = nbinom2)
Wolf.4250 <- glmmTMB(Wolf~LD4250, data = det, family = nbinom2)
Wolf.4500 <- glmmTMB(Wolf~LD4500, data = det, family = nbinom2)
Wolf.4750 <- glmmTMB(Wolf~LD4750, data = det, family = nbinom2)
Wolf.5000 <- glmmTMB(Wolf~LD5000, data = det, family = nbinom2)

## Model selection with AICctab (bbmle) --> exluding VegHt
modnames <- c("NULL","250m", "500m", "750m", "1000m", "1250m", "1500m", "1750m", "2000m")
wolftab <- ICtab(Wolf.0,Wolf.250,Wolf.500,Wolf.750,Wolf.1000,Wolf.1250, Wolf.1500, Wolf.1750, Wolf.2000,  mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
wolftab

#     dLogLik dAIC df weight
# 5000m 1.5     0.0  3  0.133 
# 4750m 1.5     0.2  3  0.122 
# 4500m 1.3     0.6  3  0.100 
# NULL  0.0     1.1  2  0.077 
# 4250m 0.9     1.3  3  0.070 
# 4000m 0.4     2.4  3  0.041 
# 1250m 0.2     2.7  3  0.034 
# 3750m 0.2     2.7  3  0.034 
# 3500m 0.2     2.8  3  0.033 
# 2750m 0.1     2.9  3  0.031 
# 1500m 0.1     2.9  3  0.031 
# 250m  0.1     3.0  3  0.030 
# 2500m 0.1     3.0  3  0.030 
# 2000m 0.0     3.0  3  0.029 
# 500m  0.0     3.0  3  0.029 
# 1750m 0.0     3.0  3  0.029 
# 3000m 0.0     3.0  3  0.029 
# 3250m 0.0     3.1  3  0.029 
# 2250m 0.0     3.1  3  0.029 
# 750m  0.0     3.1  3  0.029 
# 1000m 0.0     3.1  3  0.029


# Plotting model weights for each scale to determine best scale

class(wolftab) <- "data.frame"


wolftab$scale <- c("NULL",1250,1500,250,2000,500,1750,750,1000) #ordered according to ICtab 

#Remove NULL row
tab <- wolftab %>% filter(scale != "NULL")
str(tab)
#Convert to numeric
tab$scale <- as.numeric(tab$scale)
# Plot weight for each scale
op <- par(mar = c(5,5,4,2) + 0.1)
plot(tab$scale, tab$weight, xlim=range(tab$scale), ylim=c(0,1), xlab = "Scale (m)", ylab = "AICweight", main = "Wolf", pch=16, cex = 1, cex.axis = 2, cex.lab = 2, cex.main = 2)
lines(tab$scale[order(tab$scale)], tab$weight[order(tab$scale)], xlim=range(tab$scale), ylim=c(0,1))


### 5km comes out on top, but only slightly

### Caribou models
Caribou.0 <- glmmTMB(Caribou~1, data = det, family = nbinom2)
Caribou.250 <- glmmTMB(Caribou~LD250, data = det, family = nbinom2)
Caribou.500 <- glmmTMB(Caribou~LD500, data = det, family = nbinom2)
Caribou.750 <- glmmTMB(Caribou~LD750, data = det, family = nbinom2)
Caribou.1000 <- glmmTMB(Caribou~LD1000, data = det, family = nbinom2)
Caribou.1250 <- glmmTMB(Caribou~LD1250, data = det, family = nbinom2)
Caribou.1500 <- glmmTMB(Caribou~LD1500, data = det, family = nbinom2)
Caribou.1750 <- glmmTMB(Caribou~LD1750, data = det, family = nbinom2)
Caribou.2000 <- glmmTMB(Caribou~LD2000, data = det, family = nbinom2)
Caribou.2250 <- glmmTMB(Caribou~LD2250, data = det, family = nbinom2)
Caribou.2500 <- glmmTMB(Caribou~LD2500, data = det, family = nbinom2)
Caribou.2750 <- glmmTMB(Caribou~LD2750, data = det, family = nbinom2)
Caribou.3000 <- glmmTMB(Caribou~LD3000, data = det, family = nbinom2)
Caribou.3250 <- glmmTMB(Caribou~LD3250, data = det, family = nbinom2)
Caribou.3500 <- glmmTMB(Caribou~LD3500, data = det, family = nbinom2)
Caribou.3750 <- glmmTMB(Caribou~LD3750, data = det, family = nbinom2)
Caribou.4000 <- glmmTMB(Caribou~LD4000, data = det, family = nbinom2)
Caribou.4250 <- glmmTMB(Caribou~LD4250, data = det, family = nbinom2)
Caribou.4500 <- glmmTMB(Caribou~LD4500, data = det, family = nbinom2)
Caribou.4750 <- glmmTMB(Caribou~LD4750, data = det, family = nbinom2)
Caribou.5000 <- glmmTMB(Caribou~LD5000, data = det, family = nbinom2)

## Model selection with AICctab (bbmle) --> exluding VegHt
modnames <- c("NULL","250m", "500m", "750m", "1000m", "1250m", "1500m", "1750m", "2000m")
Cariboutab <- ICtab(Caribou.0,Caribou.250,Caribou.500,Caribou.750,Caribou.1000,Caribou.1250, Caribou.1500, Caribou.1750, Caribou.2000, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
Cariboutab

#      dLogLik dAIC df weight
# 5000m 4.1     0.0  3  0.4098
# 4750m 2.9     2.5  3  0.1204
# 4500m 2.5     3.3  3  0.0802
# 1750m 2.4     3.4  3  0.0739
# 4250m 1.9     4.3  3  0.0474
# 250m  1.9     4.4  3  0.0449
# 2000m 1.5     5.1  3  0.0319
# 4000m 1.4     5.4  3  0.0279
# 1500m 1.4     5.5  3  0.0264
# 3750m 1.1     5.9  3  0.0211
# NULL  0.0     6.2  2  0.0184
# 2250m 0.8     6.5  3  0.0156
# 1250m 0.8     6.6  3  0.0154
# 3500m 0.4     7.3  3  0.0104
# 1000m 0.4     7.5  3  0.0097
# 2500m 0.3     7.6  3  0.0091
# 500m  0.3     7.7  3  0.0089
# 3250m 0.1     7.9  3  0.0077
# 2750m 0.1     8.0  3  0.0073
# 750m  0.0     8.2  3  0.0068
# 3000m 0.0     8.2  3  0.0068

# Plotting model weights for each scale to determine best scale
class(Cariboutab) <- "data.frame"


Cariboutab$scale <- c(1750,250,2000,1500,"NULL",1250,1000,500,750) #ordered according to ICtab 

#Remove NULL row
tab <- Cariboutab %>% filter(scale != "NULL")
str(tab)
#Convert to numeric
tab$scale <- as.numeric(tab$scale)
# Plot weight for each scale
op <- par(mar = c(5,5,4,2) + 0.1)
plot(tab$scale, tab$weight, xlim=range(tab$scale), ylim=c(0,1), xlab = "Scale (m)", ylab = "AICweight", main = "Caribou", pch=16, cex = 1, cex.axis = 2, cex.lab = 2, cex.main = 2)
lines(tab$scale[order(tab$scale)], tab$weight[order(tab$scale)], xlim=range(tab$scale), ylim=c(0,1))


### WTDeer models
WTDeer.0 <- glmmTMB(WTDeer~1, data = det, family = nbinom2)
WTDeer.250 <- glmmTMB(WTDeer~LD250, data = det, family = nbinom2)
WTDeer.500 <- glmmTMB(WTDeer~LD500, data = det, family = nbinom2)
WTDeer.750 <- glmmTMB(WTDeer~LD750, data = det, family = nbinom2)
WTDeer.1000 <- glmmTMB(WTDeer~LD1000, data = det, family = nbinom2)
WTDeer.1250 <- glmmTMB(WTDeer~LD1250, data = det, family = nbinom2)
WTDeer.1500 <- glmmTMB(WTDeer~LD1500, data = det, family = nbinom2)
WTDeer.1750 <- glmmTMB(WTDeer~LD1750, data = det, family = nbinom2)
WTDeer.2000 <- glmmTMB(WTDeer~LD2000, data = det, family = nbinom2)
WTDeer.2250 <- glmmTMB(WTDeer~LD2250, data = det, family = nbinom2)
WTDeer.2500 <- glmmTMB(WTDeer~LD2500, data = det, family = nbinom2)
WTDeer.2750 <- glmmTMB(WTDeer~LD2750, data = det, family = nbinom2)
WTDeer.3000 <- glmmTMB(WTDeer~LD3000, data = det, family = nbinom2)
WTDeer.3250 <- glmmTMB(WTDeer~LD3250, data = det, family = nbinom2)
WTDeer.3500 <- glmmTMB(WTDeer~LD3500, data = det, family = nbinom2)
WTDeer.3750 <- glmmTMB(WTDeer~LD3750, data = det, family = nbinom2)
WTDeer.4000 <- glmmTMB(WTDeer~LD4000, data = det, family = nbinom2)
WTDeer.4250 <- glmmTMB(WTDeer~LD4250, data = det, family = nbinom2)
WTDeer.4500 <- glmmTMB(WTDeer~LD4500, data = det, family = nbinom2)
WTDeer.4750 <- glmmTMB(WTDeer~LD4750, data = det, family = nbinom2)
WTDeer.5000 <- glmmTMB(WTDeer~LD5000, data = det, family = nbinom2)

## Model selection with AICctab (bbmle) --> exluding VegHt
modnames <- c("NULL","250m", "500m", "750m", "1000m", "1250m", "1500m", "1750m", "2000m")
WTDeertab <- ICtab(WTDeer.0,WTDeer.250,WTDeer.500,WTDeer.750,WTDeer.1000,WTDeer.1250, WTDeer.1500, WTDeer.1750, WTDeer.2000, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
WTDeertab

#      dLogLik dAIC df weight
# 750m  13.7     0.0 3  0.984 
# 1000m  9.4     8.6 3  0.014 
# 500m   6.2    15.0 3  <0.001
# 1750m  6.1    15.2 3  <0.001
# 2000m  6.0    15.3 3  <0.001
# 1250m  5.2    16.9 3  <0.001
# 2250m  4.4    18.6 3  <0.001
# 1500m  4.2    18.9 3  <0.001
# 2500m  3.3    20.8 3  <0.001
# 2750m  3.1    21.2 3  <0.001
# 3000m  2.2    22.9 3  <0.001
# 3250m  1.4    24.5 3  <0.001
# 5000m  1.2    24.9 3  <0.001
# NULL   0.0    25.4 2  <0.001
# 250m   0.7    25.9 3  <0.001
# 3500m  0.7    26.0 3  <0.001
# 4750m  0.7    26.0 3  <0.001
# 3750m  0.4    26.6 3  <0.001
# 4500m  0.2    27.0 3  <0.001
# 4000m  0.1    27.2 3  <0.001
# 4250m  0.0    27.3 3  <0.001

summary(WTDeer.750)
# Plotting model weights for each scale to determine best scale
class(WTDeertab) <- "data.frame"


WTDeertab$scale <- c(750,1000,500,1750,2000,1250,1500, "NULL", 250) #ordered according to ICtab 

#Remove NULL row
tab <- WTDeertab %>% filter(scale != "NULL")
str(tab)
#Convert to numeric
tab$scale <- as.numeric(tab$scale)
# Plot weight for each scale
op <- par(mar = c(5,5,4,2) + 0.1)
plot(tab$scale, tab$weight, xlim=range(tab$scale), ylim=c(0,1), xlab = "Scale (m)", ylab = "AICweight", main = "WT Deer", pch=16, cex = 1, cex.axis = 2, cex.lab = 2, cex.main = 2)
lines(tab$scale[order(tab$scale)], tab$weight[order(tab$scale)], xlim=range(tab$scale), ylim=c(0,1))



### Moose models
Moose.0 <- glmmTMB(Moose~1, data = det, family = nbinom2)
Moose.250 <- glmmTMB(Moose~LD250, data = det, family = nbinom2)
Moose.500 <- glmmTMB(Moose~LD500, data = det, family = nbinom2)
Moose.750 <- glmmTMB(Moose~LD750, data = det, family = nbinom2)
Moose.1000 <- glmmTMB(Moose~LD1000, data = det, family = nbinom2)
Moose.1250 <- glmmTMB(Moose~LD1250, data = det, family = nbinom2)
Moose.1500 <- glmmTMB(Moose~LD1500, data = det, family = nbinom2)
Moose.1750 <- glmmTMB(Moose~LD1750, data = det, family = nbinom2)
Moose.2000 <- glmmTMB(Moose~LD2000, data = det, family = nbinom2)
Moose.2250 <- glmmTMB(Moose~LD2250, data = det, family = nbinom2)
Moose.2500 <- glmmTMB(Moose~LD2500, data = det, family = nbinom2)
Moose.2750 <- glmmTMB(Moose~LD2750, data = det, family = nbinom2)
Moose.3000 <- glmmTMB(Moose~LD3000, data = det, family = nbinom2)
Moose.3250 <- glmmTMB(Moose~LD3250, data = det, family = nbinom2)
Moose.3500 <- glmmTMB(Moose~LD3500, data = det, family = nbinom2)
Moose.3750 <- glmmTMB(Moose~LD3750, data = det, family = nbinom2)
Moose.4000 <- glmmTMB(Moose~LD4000, data = det, family = nbinom2)
Moose.4250 <- glmmTMB(Moose~LD4250, data = det, family = nbinom2)
Moose.4500 <- glmmTMB(Moose~LD4500, data = det, family = nbinom2)
Moose.4750 <- glmmTMB(Moose~LD4750, data = det, family = nbinom2)
Moose.5000 <- glmmTMB(Moose~LD5000, data = det, family = nbinom2)

## Model selection with AICctab (bbmle) --> exluding VegHt
modnames <- c("NULL","250m", "500m", "750m", "1000m", "1250m", "1500m", "1750m", "2000m")
Moosetab <- ICtab(Moose.0,Moose.250,Moose.500,Moose.750,Moose.1000,Moose.1250, Moose.1500, Moose.1750, Moose.2000, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
Moosetab

#      dLogLik dAIC df weight
# 5000m 2.3     0.0  3  0.157 
# 4750m 1.8     1.0  3  0.095 
# 4250m 1.8     1.0  3  0.093 
# 4000m 1.7     1.1  3  0.089 
# 4500m 1.6     1.3  3  0.082 
# 3750m 1.6     1.4  3  0.077 
# 3500m 1.0     2.5  3  0.045 
# NULL  0.0     2.6  2  0.043 
# 2500m 0.7     3.1  3  0.033 
# 2250m 0.6     3.3  3  0.030 
# 3250m 0.6     3.5  3  0.028 
# 2750m 0.5     3.5  3  0.028 
# 3000m 0.4     3.7  3  0.025 
# 1250m 0.4     3.7  3  0.024 
# 2000m 0.4     3.7  3  0.024 
# 1000m 0.3     3.9  3  0.022 
# 1750m 0.3     3.9  3  0.022 
# 750m  0.3     4.0  3  0.022 
# 1500m 0.3     4.0  3  0.021 
# 500m  0.3     4.1  3  0.021 
# 250m  0.1     4.5  3  0.017

summary(Moose.750)
# Plotting model weights for each scale to determine best scale
class(Moosetab) <- "data.frame"


Moosetab$scale <- c("NULL", 1250,2000,1000,1750,750,1500,500,250) #ordered according to ICtab 

#Remove NULL row
tab <- Moosetab %>% filter(scale != "NULL")
str(tab)
#Convert to numeric
tab$scale <- as.numeric(tab$scale)
# Plot weight for each scale
op <- par(mar = c(5,5,4,2) + 0.1)
plot(tab$scale, tab$weight, xlim=range(tab$scale), ylim=c(0,1), xlab = "Scale (m)", ylab = "AICweight", main = "Moose", pch=16, cex = 1, cex.axis = 2, cex.lab = 2, cex.main = 2)
lines(tab$scale[order(tab$scale)], tab$weight[order(tab$scale)], xlim=range(tab$scale), ylim=c(0,1))



### Bears: need truncated season
# Check black bear detections by yr_month
plot(x = det$Yr_Month, y = det$Blackbear) #Active April- November, both years
#Check against Snow
plot(x = det$SnowDays, y = det$Blackbear) # Predictably, more in months with fewer snow days

hist(det$Blackbear)

### Cropping data for active bear months only -- April - October -- use months rather than Yr_Months
unique(det$Month)
class(det$Month)



bear <- det %>% filter(Month >= 4 & Month <= 10) %>% select(Site, Treatment,Yr_Month, Site_ym, Blackbear, SnowDays, Year, Month, Dist2water_km, LD250,LD500,LD750,LD1000,LD1250, LD1500, LD1750, LD2000)


plot(bear$Yr_Month, bear$Blackbear)
plot(bear$Month, bear$Blackbear)
hist(bear$Month)
hist(bear$Blackbear)

### Blackbear models
Blackbear.0 <- glmmTMB(Blackbear~1, data = bear, family = nbinom2)
Blackbear.250 <- glmmTMB(Blackbear~LD250, data = bear, family = nbinom2)
Blackbear.500 <- glmmTMB(Blackbear~LD500, data = bear, family = nbinom2)
Blackbear.750 <- glmmTMB(Blackbear~LD750, data = bear, family = nbinom2)
Blackbear.1000 <- glmmTMB(Blackbear~LD1000, data = bear, family = nbinom2)
Blackbear.1250 <- glmmTMB(Blackbear~LD1250, data = bear, family = nbinom2)
Blackbear.1500 <- glmmTMB(Blackbear~LD1500, data = bear, family = nbinom2)
Blackbear.1750 <- glmmTMB(Blackbear~LD1750, data = bear, family = nbinom2)
Blackbear.2000 <- glmmTMB(Blackbear~LD2000, data = bear, family = nbinom2)
Blackbear.2250 <- glmmTMB(Blackbear~LD2250, data = bear, family = nbinom2)
Blackbear.2500 <- glmmTMB(Blackbear~LD2500, data = bear, family = nbinom2)
Blackbear.2750 <- glmmTMB(Blackbear~LD2750, data = bear, family = nbinom2)
Blackbear.3000 <- glmmTMB(Blackbear~LD3000, data = bear, family = nbinom2)
Blackbear.3250 <- glmmTMB(Blackbear~LD3250, data = bear, family = nbinom2)
Blackbear.3500 <- glmmTMB(Blackbear~LD3500, data = bear, family = nbinom2)
Blackbear.3750 <- glmmTMB(Blackbear~LD3750, data = bear, family = nbinom2)
Blackbear.4000 <- glmmTMB(Blackbear~LD4000, data = bear, family = nbinom2)
Blackbear.4250 <- glmmTMB(Blackbear~LD4250, data = bear, family = nbinom2)
Blackbear.4500 <- glmmTMB(Blackbear~LD4500, data = bear, family = nbinom2)
Blackbear.4750 <- glmmTMB(Blackbear~LD4750, data = bear, family = nbinom2)
Blackbear.5000 <- glmmTMB(Blackbear~LD5000, data = bear, family = nbinom2)

## Model selection with AICctab (bbmle) --> exluding VegHt
modnames <- c("NULL","250m", "500m", "750m", "1000m", "1250m", "1500m", "1750m", "2000m")
Blackbeartab <- ICtab(Blackbear.0,Blackbear.250,Blackbear.500,Blackbear.750,Blackbear.1000,Blackbear.1250, Blackbear.1500, Blackbear.1750, Blackbear.2000, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
Blackbeartab

#      dLogLik dAIC df weight
# 750m  4.3     0.0  3  0.2830
# 500m  3.9     0.8  3  0.1921
# 1000m 3.2     2.2  3  0.0958
# 2000m 3.0     2.5  3  0.0805
# 1750m 2.7     3.1  3  0.0586
# 2250m 2.5     3.5  3  0.0485
# 250m  2.3     4.0  3  0.0383
# 2750m 2.1     4.3  3  0.0331
# 2500m 2.1     4.4  3  0.0316
# 3000m 1.9     4.7  3  0.0272
# 1250m 1.8     5.0  3  0.0236
# 3250m 1.6     5.4  3  0.0191
# 1500m 1.6     5.4  3  0.0186
# NULL  0.0     6.6  2  0.0107
# 3500m 0.7     7.1  3  0.0082
# 4000m 0.5     7.5  3  0.0066
# 3750m 0.5     7.5  3  0.0065
# 4250m 0.3     7.9  3  0.0054
# 4500m 0.2     8.2  3  0.0047
# 4750m 0.0     8.5  3  0.0041
# 5000m 0.0     8.6  3  0.0039

summary(Blackbear.750)
# Plotting model weights for each scale to determine best scale
class(Blackbeartab) <- "data.frame"


Blackbeartab$scale <- c(750,500,1000,2000,1750,250,1250,1500, "NULL") #ordered according to ICtab 

#Remove NULL row
tab <- Blackbeartab %>% filter(scale != "NULL")
str(tab)
#Convert to numeric
tab$scale <- as.numeric(tab$scale)
# Plot weight for each scale
op <- par(mar = c(5,5,4,2) + 0.1)
plot(tab$scale, tab$weight, xlim=range(tab$scale), ylim=c(0,1), xlab = "Scale (m)", ylab = "AICweight", main = "Black bear", pch=16, cex = 1, cex.axis = 2, cex.lab = 2, cex.main = 2)
lines(tab$scale[order(tab$scale)], tab$weight[order(tab$scale)], xlim=range(tab$scale), ylim=c(0,1))



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