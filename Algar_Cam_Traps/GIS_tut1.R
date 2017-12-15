####################################
# GIS_tut1.R
# Tutorial for GIS analysis in R, 
# http://pakillo.github.io/R-GIS-tutorial/
# Started Dec 14 2017 by Erin T.
###################################

library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
library(rgdal)
library(ggmap)
library(tmap)
library(dplyr)
library(tidyr)
# and their dependencies

#### 1. Retrieving base maps from Google ####
# Can access country maps from Google Earth

library(dismo)

mymap <- gmap("Canada")
plot(mymap)
#Change map type
mymap <- gmap("Canada", type = "satellite")
plot(mymap)
#Change zoom level
mymap <- gmap("Canada", type = "satellite", exp = 1)
plot(mymap)

#Save in working directory
mymap <- gmap("Canada", type = "satellite", filename = "Canada.gmap")

# Selecting specific areas
mymap <- gmap("Alberta")
plot(mymap)


select.area <- drawExtent()
# now click 2 times on the map to select your region
mymap <- gmap(select.area)
plot(mymap)
# See ?gmap for many other possibilities


####2. Map data onto Google Map tiles with RgoogleMaps ####
library(RgoogleMaps)

# Retrieve base maps from Google
newmap <- GetMap(center = c(36.7, -5.9), #indicates map center in lat - long
                 zoom = 10, 
                 destfile = "newmap.png", 
                 maptype = "satellite") 


# Now using bounding box instead of center coordinates:
newmap2 <- GetMap.bbox(lonR = c(-5, -6), 
                       latR = c(35, 37), 
                       destfile = "newmap2.png", 
                       maptype = "terrain")


# Try different maptypes
newmap3 <- GetMap.bbox(lonR = c(-5, -6), 
                       latR = c(36, 37), 
                       destfile = "newmap3.png", 
                       maptype = "satellite")
#Plotting points onto map
PlotOnStaticMap(newmap2,
                lat = c(36.3, 35.8, 36.4), 
                lon = c(-5.5, -5.6, -5.8), 
                zoom = 30, 
                cex = 1, 
                pch = 19, 
                col = "red", 
                FUN = points, 
                add = F)

## Try with Algar data
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data/Station_data")
Stat <- read.csv("AlgarStations60.csv")
summary(Stat$Longitude) #Min = -112.6, Max = -112.4
summary(Stat$Latitude) #Min = 56.17, Max = 56.48
Algarmap <- GetMap.bbox(lonR = c(-112.4, -112.6),
                        latR = c(56.17, 56.48),
                        destfile = "Algarmap.png", 
                        maptype = "satellite")

## Map camera points
PlotOnStaticMap(Algarmap,
                lat = Stat$Latitude, 
                lon = Stat$Longitude, 
                zoom = 30, 
                cex = 1, 
                pch = 20, 
                col = "white", 
                FUN = points, 
                add = F)


#### Spatial Vector Data (Points, lines, polygons) ####
# Using example data from Global Biodiversity Information Facility

library(dismo)  # check also the nice 'rgbif' package! 
laurus <- gbif("Laurus", "nobilis")

# get data frame with spatial coordinates (points)
locs <- subset(laurus, select = c("country", "lat", "lon"))
head(locs)  # a simple data frame with coordinates


# Discard data with errors in coordinates:
locs <- subset(locs, locs$lat < 90)

#Specifying that lat and long are spatial
coordinates(locs) <- c("lon", "lat")  # set spatial coordinates
plot(locs)

#Define the geographical projection, consult descriptions at http://www.spatialreference.org/

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data
summary(locs)

## Quickly plot points on map
plot(locs, pch = 20, col = "steelblue")
library(rworldmap)
# library rworldmap provides different types of global maps, e.g:
data(coastsCoarse)
data(countriesLow)
plot(coastsCoarse, add = T)

##Subsetting, re-mapping
table(locs$country)


locs.gb <- subset(locs, locs$country == "United Kingdom")  # select only locs in UK
plot(locs.gb, pch = 20, cex = 1, col = "steelblue")
title("Laurus nobilis occurrences in UK")
plot(countriesLow, add = T)

## Mapping vectorial data using gmap from dismo
gbmap <- gmap(locs.gb, type = "satellite")
locs.gb.merc <- Mercator(locs.gb)  # Google Maps are in Mercator projection. 
# This function projects the points to that projection to enable mapping
plot(gbmap)
points(locs.gb.merc, pch = 20, col = "red")


#### Using raster data ####
vignette("Raster")
