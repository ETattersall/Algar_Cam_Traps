##############
# Landcover_data_explore.R
# Exploring ABMI's landcover data
# Jan. 4, 2018
#########

library(rgdal) #For loading and working with spatial data
library(tidyr) # For data manipulation
library(sp)    # Functions for working with spatial data 
library(rgeos)
library(raster) #For intersect function

#Read in Algar camera sites
setwd("C:/Users/ETattersall/Google Drive/Algar Seismic Restoration Project/GIS data")
Algcoord <- readOGR("GIS", "AlgarSites_April2017")
plot(Algcoord)
summary(Algcoord) #Projected in UTM

# Read in AVIE
AVIE <- readOGR("GIS", "AVIE_Veg_simple")
plot(AVIE)
summary(AVIE) #Projected in tmerc

#Read in Jo's HF layer
HF_camArrays <- readOGR("GIS", "CameraArray_ABMI_HFI_2014")
summary(HF_camArrays) #Projected in tmerc

## Clip HF layer to 15km surrounding Algar
# Create buffer around Algar Study area, using Algcoord data as area (large enough buffer to cover entire area)
ESAR <- gBuffer(Algcoord, width = 15000, byid= TRUE) #byid needs to be included for intersect() to work
plot(ESAR)

summary(ESAR)

# Converting HF layer to UTM
HF_UTM <- spTransform(HF_camArrays, CRSobj = CRS(proj4string(Algcoord)))
proj4string(HF_UTM)
HF_camArrays <- HF_UTM #Overwrite original HF CRS
summary(HF_camArrays)
plot(HF_camArrays)


#Clipping landcover data for buffers using raster::intersect (make sure raster package is loaded!!)
#over and gIntersects didn't work for clipping a polygon by a polygon
#gIntersection clipped the layer but did not retain attribute data
# int15000 <- gIntersection(spgeom1 = HF_camArrays, spgeom2 =  ESAR, byid = c(TRUE, FALSE))

int15000 <- intersect(HF_camArrays,ESAR)


plot(int15000)
summary(int15000)

writeOGR(int15000, dsn = "GIS", layer = "AlgarSites_HF_15kmbuffer", driver = "ESRI Shapefile")











##### Read in ABMI landcover data (clipped in ArcGIS)
lndcov <- readOGR("GIS", "ABMI_2010Landcover_Algar10kClip")

summary(lndcov) #4 attributes: Landcover class codes (see data overview)
# [+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992
# +x_0=500000 +y_0=0 +datum=NAD83 +units=m
# +no_defs +ellps=GRS80 +towgs84=0,0,0]
### converting projection to NAD83 UTM (same as Algcoord)
str(lndcov@data) ##LC_class = factor, not integer

lndcov_UTM <- spTransform(lndcov, CRSobj = CRS(proj4string(Algcoord)))
proj4string(lndcov_UTM)
lndcov <- lndcov_UTM #Overwrite original CRS
proj4string(lndcov)

plot(lndcov)
lnd_data <- lndcov@data
unique(lndcov@data$MOD_TY) #Burnt, Cutblock, Tailing pond mining
unique(lnd_data$LC_class)
#11 unique land cover types
# From data overview:
# 20 = Water
# 31 = Snow/Ice
# 32 = Rock/Rubble
# 33 = Exposed Land
# 34 = Developed
# 50 = Shrubland
# 110 = Grassland
# 120 = Agriculture
# 210 = Coniferous forest
# 220 = Broadleaf forest
# 230 = Mixed forest

### Use for distance to water, where water is LC_class = 20
## Calculate minimum distance between camera stations (Algcoord, SpatialPoints) and landcover polygons (lndcov, SpatialPolygons)

#1. Try subsetting spatialpolygons dataframe for water only (LC_class == 20)
water <- lndcov[lndcov$LC_class == "20",]
str(water@data) #91 observations of 6 variables
summary(water) #still in utm (units = metres), only LC_class==20

#2. Calculate minimum distance with rgeos gDistance
Algcoord$Dist2Water_m <- apply(gDistance(Algcoord, water,byid=TRUE), 2, min)
summary(Algcoord)

##3. Add distance to water to detection data
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
dat <- read.csv("monthlydetections_nov2015-nov2017.csv")

dat$Dist2water_m <- Algcoord$Dist2Water_m[match(dat$Site, Algcoord$CamStation)]

write.csv(dat, "monthlydetections_nov2015-nov2017.csv")
