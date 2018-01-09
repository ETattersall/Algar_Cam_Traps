########
# Landcover_data_explore.R
# Exploring ABMI's landcover data
# Jan. 4, 2018
#########

library(rgdal) #For loading and working with spatial data
library(tidyr) # For data manipulation
library(sp)    # Functions for working with spatial data 
library(rgeos)

#Read in Algar camera sites
setwd("C:/Users/ETattersall/Google Drive/Algar Seismic Restoration Project/GIS data")
Algcoord <- readOGR("GIS", "AlgarSites_April2017")

# Read in ABMI landcover data
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
lndcov <- readOGR("2010LanCoverShapeFiles", "Lancover_Polygons_2010")

##Isolate ESAR region of interest
# Create buffer around Algar Study area, using AVIE data as area

ESAR <- gBuffer(AVIE, width = 5000)




summary(lndcov) #4 attributes: Landcover class codes (see data overview)
# [+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992
# +x_0=500000 +y_0=0 +datum=NAD83 +units=m
# +no_defs +ellps=GRS80 +towgs84=0,0,0]
### converting projection to NAD83 UTM (same as Algcoord)
lndcov_UTM <- spTransform(lndcov, CRSobj = CRS(proj4string(Algcoord)))
proj4string(lndcov_UTM)
lndcov <- lndcov_UTM #Overwrite original CRS

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

##Isolate ESAR region of interest
# Need coordinates of area of interest in UTM