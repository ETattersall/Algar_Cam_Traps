################################
## Ch3_ForestDensity.R
## Sep. 18,2018
## Extracting variable for proportion of dense forest cover
## Dense forest = >50% ground area covered by forest crown
## AVI divides forest density into 4 classes: A (6-30%), B(31-50%), C(51-70%), and D (71-100%)
## I will aggregate these into 'Dense' and 'Open', with a 50% cutoff, then calculate p.Dense around camera sites
#################################

require(raster)

require(sp)

require(rgdal)

require(rgeos)

require(dplyr) # for glimpse function

require(tidyr) #for gather function

require(ggplot2) #for plotting data across scales

setwd("C:/Users/ETattersall/Google Drive/Algar Seismic Restoration Project/3. Data")
AVIE <- readOGR(dsn = "3.1 GIS", layer = "AVIE_10k_Habitat")
summary(AVIE) #tmerc, Habitat = Habitat_1


### Create a data frame indicating whether polygon is 'dense' or 'open' (Density classes C and D or A and B, respectively)

Dense <- AVIE@data %>% filter(DENSITY == "C" | DENSITY == "D") %>% select(POLY_NUM)
Dense$DENSE <- rep("Dense", nrow(Dense))

Open <- AVIE@data %>% filter(DENSITY == "A" | DENSITY == "B") %>% select(POLY_NUM)
Open$DENSE <- rep("Open", nrow(Open))

DENSE <- rbind.data.frame(Dense, Open) ## row numbers does not match original data frame because of DENSITY obs. that are NA

## Add DENSE variable to spatial data frame
AVIE@data$DENSE <- DENSE$DENSE[match(AVIE@data$POLY_NUM, DENSE$POLY_NUM)]

colnames(AVIE@data)
str(AVIE@data$DENSE)
table(AVIE@data$DENSE)

### Extracting prop. Dense forest around cameras
## Loading in Algar camera station points
Algcoord <- readOGR("3.1 GIS", "AlgarSites_April2017")
summary(Algcoord) #in utm

# comparing CRS between layers
proj4string(Algcoord) # NAD83 utm zone 12
proj4string(AVIE)# NAD83 tmerc

# Converting AVIE to UTM
AVIE_UTM <- spTransform(AVIE, CRSobj = CRS(proj4string(Algcoord)))
proj4string(AVIE_UTM)
AVIE <- AVIE_UTM #Overwrite original AVIE CRS

### Extracting pDense at 8 different buffer sizes around cams: create buffer, clip AVIE to that buffer, calculate area of each polygon rel. to total area of buffer

#Function for calculating total area of a buffer
bufferArea <- function(r){
  Area <- pi*(r)^2
  print(Area)
}

## Creating general function to extract proportions DENSE at a given buffer size
## spPoints = sp. points object to use for buffering
## spDF = spatial dataframe to take cover values from
## buffer = radius of buffer

## returns a list of 60 proportions of Open forest (b/c at 250 m, not all sites have Dense forest)
propDENSE <- function(spPoints, spDF, buffer){
  start.time<-paste("start time:", Sys.time())
      b <- gBuffer(spPoints, width = buffer, byid = TRUE) ## creating buffer around spPoints
      i <- raster::intersect(spDF,b) ## clipping sp object to buffer
      i.data <- i@data %>% select(CamStation, utmE,utmN, Treatment, lat_decdeg,lon_decdeg, DENSE) ## extracting camera data and cover variable
      i.data$Area <- gArea(i, byid= TRUE) ## Adding polygon area to dataset
      i.data$Percent_Cover <- i.data$Area/bufferArea(buffer) ## calculating percent cover based on total area of buffer
      i.data <- aggregate(.~ CamStation + utmE + utmN + Treatment + lat_decdeg + lon_decdeg + DENSE, data = i.data, sum) # Aggregate same land cover polygons at each site
      i.data <- i.data[with(i.data, order(CamStation)), ] ## Order dataset by CamStation
      pDense <- i.data %>% filter(DENSE == "Open") %>% select(Percent_Cover)
  end.time<-paste("end time:", Sys.time())
  
  print(start.time)
  
  print(end.time)
  
  return(pDense)
}




## test function
Dense.data <- propDENSE(spPoints = Algcoord, spDF = AVIE, buffer = 250)

### (Prior to revising function to select only for propOpen)
## Check if all stations have proportions (either Dense or Open)
unique(Dense.data$CamStation) ## YES
## Do all stations have a proportion of Dense forest?
Denseforest <- Dense.data %>% filter(DENSE=="Dense") ## NO. Only 39 stations have prop.Dense
Openforest <- Dense.data %>% filter(DENSE=="Open") ## BUT all stations have a certain prop. of open forest. None are 100% (many have a certain prop of non-forest)


#### Components of function ####
b <- gBuffer(Algcoord, width = 250, byid = TRUE)
i <- raster::intersect(AVIE,b)
plot(i)
summary(i)

i.data <- i@data %>% select(CamStation, utmE,utmN, Treatment, lat_decdeg,lon_decdeg, DENSE)
i.data$Area <- gArea(i, byid= TRUE) ## Adding polygon area to dataset

## Order dataset by dense
i.data <- i.data[with(i.data, order(DENSE, CamStation)), ]
head(i.data)


i.data$Percent_Cover <- i.data$Area/bufferArea(250)
# Aggregate same land cover polygons at each site
i.data <- aggregate(.~ CamStation + utmE + utmN + Treatment + lat_decdeg + lon_decdeg + DENSE, data = i.data, sum)
####


##### Cycle through a loop to extract pDense at 8 different buffer sizes

scale <- c(250,500,750,1000,1250,1500,1750,2000)

## Create empty data frame to populate with pOpen (not pDense because not all Sites have pDense values)
df <- data.frame(matrix(data = NA, nrow = 60, ncol = length(scale)+1))
colnames(df) <- c("Site", "pOpen250", "pOpen500", "pOpen750", "pOpen1000", "pOpen1250", "pOpen1500", "pOpen1750", "pOpen2000")
df$Site <- Algcoord@data$CamStation
df <- df[order(df$Site), ]

for(i in 1:length(scale)){
  df[ , i+1] <- propDENSE(Algcoord,AVIE, buffer = scale[i])
}

### Save density data as csv
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

write.csv(df, "propOpenForest_Algar60.csv")

### Scale analysis -- assessing daily occurrences of target spp. (lynx, coyote, black bear) as a function of prop.Open forest