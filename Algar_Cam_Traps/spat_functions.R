##############################################
# spat_functions.R
# Functions for Spatial analysis
############################################

# Packages 
library(rgdal) #For loading and working with spatial data
library(ggmap) #Extends ggplot2 for maps
library(tmap)  # For making pretty maps
library(sp)    # Functions for working with spatial data 
library(raster)# Manipulating raster data
library(rgeos)

#### Loading in spatial data ####
AVIE <- readOGR(dsn = "GIS", layer = "AVIE_Veg_simple") #where dsn = data source name (folder or file), layer = layer name

summary(AVIE) # Spatial polygons data frame, projected in NAD83. Lowland, Non-forest, and upland categories
class(AVIE)
plot(AVIE)

AVIE@data #querying the attribute data associated with layer

#### Calculating distances (spDists is specific to points data, may need to adjust or use different function if one object is spatial lines, polygons, etc.) ####
#Uses data.frame Stat, which contains cam station data
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
dist.5000 <- dist[which(dist<5.0)] #540 distances out of 3540 that are <5km
hist(dist.5000)

#### Drawing buffers ####
b250 <- gBuffer(Algcoord, width = 250, byid = TRUE) #where Algcoord = spatial points data frame, width = 250m (because Algcoord has a UTM projection with base unit of metres)
# byid = TRUE treats every point as independent and will associate buffe with that individual point


#### Converting between projections ####
# comparing CRS between layers
proj4string(b250)# NAD83 utm zone 12
proj4string(AVIE)# NAD83 tmerc

# Converting AVIE to UTM
AVIE_UTM <- spTransform(AVIE, CRSobj = CRS(proj4string(Algcoord)))
proj4string(AVIE_UTM)
AVIE <- AVIE_UTM #Overwrite original AVIE CRS

#### Clipping polygons ####
#Clipping landcover data for buffers using raster::intersect
#Functions over and gIntersects didn't work for clipping a polygon by a polygon
#gIntersection clipped the layer but did not retain attribute data

int250 <- intersect(AVIE, b250)
plot(int250)
int250@data 

#### Clipping and extracting mean raster data for a specified buffer around a point (here using wet areas mapping raster - WAM) ####
#### Test 4: Cropping raster to spatial polygons for buffered area within function ####
# Loading original WAM file (not yet clipped to desired buffer)
r2 <- raster("AlgarWAM.tif")

# Function
#spatial.df = dataframe to add WAM data to
# wam_rast = original WAM raster layer
# buffer = desired buffer area

AddWAM<-function(spatial.df, wam_rast, buffer){
  
  start.time<-paste("start time:", Sys.time())
  
  for(i in 1:nrow(spatial.df)) {
    
    tryCatch({ #tryCatch allows the rest of the function to run even if one line encounters an error
      
      b <- gBuffer(spatial.df, width = buffer, byid=TRUE) #Creating buffer around points
      
      clip1 <- crop(x = wam_rast, y = extent(b[i,])) #crop raster to extent of station buffer
      
      ext <- getValues(clip1) #much faster than extract()
      
      spatial.df@data[i,paste("wam_mean", buffer, sep ="")]<- mean(ext, na.rm=T) #add 'mean' attribute to station data
      
      spatial.df@data[i,paste("wam_min", buffer, sep ="")]<- min(ext, na.rm=T)#add 'min' attribute to station data
      
      spatial.df[i,paste("wam_max", buffer, sep ="")]<- max(ext, na.rm=T)#add 'max' attribute to station data
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }
  
  end.time<-paste("end time:", Sys.time())
  
  print(start.time)
  
  print(end.time)
  
  return(spatial.df)
  
}

#Test
AddWAM4 <- AddWAM(spatial.df = cams, wam_rast = r2, buffer = 100)



plot(r2)

plot(AddWAM4,add=T)

plot(AddWAM4[is.na(AddWAM4@data$wam_mean),],add=T, col="red")
head(AddWAM4@data)
tail(AddWAM4@data)
hist(AddWAM4$wam_mean) # slightly skewed normal distrib
hist(AddWAM4$wam_max) # max is all 7, min is all 1

#Try for much smaller buffer to see if it changes
AddWAM30 <- AddWAM(spatial.df = cams, wam_rast = r2, buffer = 30)
head(AddWAM30@data)
hist(AddWAM30$wam_max) #mostly 7 but some lower

