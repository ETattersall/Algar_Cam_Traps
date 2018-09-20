############################################
## Ch3_habitatvariables.R
## Creating habitat variables based on dominant tree species and moisture regime
## Extract habitat types around stations at 8 different buffer scales (250-2000 m)

require(raster)

require(sp)

require(rgdal)

require(rgeos)

require(dplyr) # for glimpse function

require(tidyr) #for gather function

require(ggplot2) #for plotting data across scales

# Spatial data for Algar located on Algar Project Google Drive
setwd("C:/Users/ETattersall/Google Drive/Algar Seismic Restoration Project/3. Data/3.1 GIS")

## Load in DeerTypes to explore data
DeerTypes <- readOGR(dsn = "Algar_data_DPan/data.gdb", layer = "AVIE_with_DeerTypes")
DeerData <- DeerTypes@data ## attribute table
class(DeerData)
colnames(DeerData)
unique(DeerData$FC_DOM) ## 40 Levels
unique(DeerData$Deer_Type) ## 9 Levels: Nonforest, Upland deciduous, Lowland spruce, Tamarack, Lowland mixwood, Upland Spruce, Upland mixwood, Pine

## Create rough lookup table, looking for which attributes comprise DeerType
LookUp <- cbind.data.frame(DeerData$FC_DOM, DeerData$SP1, DeerData$SP1_PER, DeerData$SP2, DeerData$SP2_PER, DeerData$MOIST_REG, DeerData$Deer_Type)
## Remove repeated rows
LookUp <- LookUp[!duplicated(LookUp), ]
colnames(LookUp) <- c("FC_DOM", "SP1", "SP1_PER", "SP2", "SP2_PER","MOIST_REG","DeerType")
LookUp <- LookUp[order(LookUp$FC_DOM, LookUp$SP1, LookUp$SP1_PER), ]
## DeerTypes based on moisture regime and percent cover of tree species. Deciduous = 80% decid species, mixedwood = >20% decid in total AND >20% conifer in total

#### Creating habitat classes using Fisher & Burton 2018 Web Table 1 ####
## Load in AVIE shapefile
AVIE <- readOGR(dsn = "AVIE_InnotechMar2018", layer = "AVIE_Algar_10k_Clip")
AVIEData <- AVIE@data
colnames(AVIEData)
head(AVIEData)
str(AVIEData)
class(AVIE@data)
class(AVIE)

## Checking mean percent classes for Sp1, Sp2, and Sp3
summary(AVIEData) # sP1_PER mean = 7.9 (med = 9), SP2_PER mean = 0.9 (med = 0), SP3_PER mean = 0.14 (med = 0)
## Can safely use SP1 and SP2 only


### Populate Habitat attribute according to Fisher and Burton's reclassifications (with alterations for simplicity, noted here)

### Simpler solution: Habitat classes based on Dominant tree species, moisture regime: Upland and Lowland classes for spruce and deciduous, (plus pine and tamarack separate?) 
## Clear Habitat field to start fresh
colnames(AVIEData)
AVIEData$Habitat <- rep(NA, nrow(AVIEData))

## What moisture classes are expressed in Pine dominated stands?
Pjm <- AVIEData %>% filter(SP1== "Pj") %>% select(MOIST_REG)
table(Pjm) ## 960 mesic, 43 wet --> keep as standalone without moisture class


## What moisture classes are expressed in Tamarack dominated stands?
Ltm <- AVIEData %>% filter(SP1== "Lt") %>% select(MOIST_REG)
table(Ltm) ## 1504 wet, 1 mesic LOL --> Tamarack also without Moisture class


### Populate Habitat field if criteria are met by creating vectors of POLY_NUMS for each class and assigning class names to those cells?

## 1. Create dataframe of POLY_NUMs and associated Habitat classes 
## 2. Combine all dataframes into Habitat data frame (check that unique poly_nums matches number of rows in AVIEData)
## 3. Match POLY_NUMS in Habitat data frame to POLY_NUMs in AVIEData to create AVIEData$Habitat


## Tamarack
Tamarack <- AVIEData %>% filter(SP1== "Lt") %>% select(POLY_NUM)
Tamarack$Habitat <- rep("Tamarack", nrow(Tamarack))
head(Tamarack)

## UpCon: SP1 = Sb, Sw, Pj, or Fb and moist = m or d
UpCon <- AVIEData %>% filter(SP1== "Sb" | SP1 == "Sw" | SP1== "Fb"| SP1== "Pj") %>% filter(MOIST_REG == "m" | MOIST_REG == "d") %>% select(POLY_NUM)
UpCon$Habitat <- rep("UpCon", nrow(UpCon))
head(UpCon)

## LowCon: SP1 = Sb, Sw, Pj, or Fb and moist = a or w
LowCon <- AVIEData %>% filter(SP1== "Sb" | SP1 == "Sw" | SP1== "Fb" | SP1== "Pj") %>% filter(MOIST_REG == "a" | MOIST_REG == "w") %>% select(POLY_NUM)
LowCon$Habitat <- rep("LowCon", nrow(LowCon))
head(LowCon)

## LowDecid: SP1 == Aw, Pb or Bw and and moist == a or w
LowDecid <- AVIEData %>% filter(SP1== "Aw" | SP1 == "Pb" | SP1== "Bw") %>% filter(MOIST_REG == "a" | MOIST_REG == "w") %>% select(POLY_NUM)
LowDecid$Habitat <- rep("LowDecid", nrow(LowDecid))
head(LowDecid)

## UpDecid: SP1 == Aw, Pb or Bw and and moist == m or d
UpDecid <- AVIEData %>% filter(SP1== "Aw" | SP1 == "Pb" | SP1== "Bw") %>% filter(MOIST_REG == "m" | MOIST_REG == "d") %>% select(POLY_NUM)
UpDecid$Habitat <- rep("UpDecid", nrow(UpDecid))
head(UpDecid)

## Non forest: SP1 == NA
Nonforest <- AVIEData %>% filter(is.na(SP1)) %>% select(POLY_NUM)
Nonforest$Habitat <- rep("Nonforest", nrow(Nonforest))
head(Nonforest)

### Does sum of all dataframes == rows in AVIEData?
sum(nrow(Tamarack), nrow(UpDecid), nrow(LowDecid), nrow(UpCon), nrow(LowCon), nrow(Nonforest)) ##YES IT DOES

## combine all in dataframe
Habitat <- rbind.data.frame(Tamarack,LowCon,UpCon,LowDecid,UpDecid,Nonforest)
length(unique(Habitat$POLY_NUM)) ## unique numbers of POLY_NUM == rows in AVIEData
table(Habitat$Habitat)
head(Habitat)
## Order by Poly_Num
Habitat <- Habitat[order(habitat$POLY_NUM),]
head(Habitat)

###Add habitat covariate to AVIE data
summary(AVIE@data$Habitat) ## Buncha NAs
AVIE@data$Habitat <- Habitat$Habitat[match(Habitat$POLY_NUM,AVIE@data$POLY_NUM)]
summary(AVIE@data$Habitat)
table(AVIE@data$Habitat)
head(AVIE@data)


## Write onto shapefile for visualization in Arc
ogrDrivers()
writeOGR(AVIE, dsn = "AVIE_InnotechMar2018", layer = "AVIE_Habitatclasses_Algar", driver = "ESRI Shapefile")

############ matching did not work. Need to go back to spatial join method in Arc
## Export habitat as look up table
write.csv(Habitat, "Habitat_LookUp.csv")

##### Sep. 18, 2018--> Measuring prop.habitat at various buffer sizes around cameras
## Re-writing AVIE shapefile in R env. with the one with  habitat classes
setwd("C:/Users/ETattersall/Google Drive/Algar Seismic Restoration Project/3. Data")
AVIE <- readOGR(dsn = "3.1 GIS", layer = "AVIE_10k_Habitat")
summary(AVIE) #tmerc, Habitat = Habitat_1

## Loading in Algar camera station points
Algcoord <- readOGR("3.1 GIS", "AlgarSites_April2017")
summary(Algcoord) #in utm

#### Extracting landcover data for that buffer ####
# comparing CRS between layers
proj4string(Algcoord) # NAD83 utm zone 12
proj4string(AVIE)# NAD83 tmerc

# Converting AVIE to UTM
AVIE_UTM <- spTransform(AVIE, CRSobj = CRS(proj4string(Algcoord)))
proj4string(AVIE_UTM)
AVIE <- AVIE_UTM #Overwrite original AVIE CRS

#### Extracting proportions of 6 habitat types at 8 buffer sizes
## returns a list of 60 proportions of Open forest (b/c at 250 m, not all sites have Dense forest)
propHab <- function(spPoints, spDF, buffer){
  start.time<-paste("start time:", Sys.time())
  b <- gBuffer(spPoints, width = buffer, byid = TRUE) ## creating buffer around spPoints
  i <- raster::intersect(spDF,b) ## clipping sp object to buffer
  i.data <- i@data %>% select(CamStation, utmE,utmN, Treatment, lat_decdeg,lon_decdeg, Habitat_1) ## extracting camera data and cover variable
  i.data$Area <- gArea(i, byid= TRUE) ## Adding polygon area to dataset
  i.data$Percent_Cover <- i.data$Area/bufferArea(buffer) ## calculating percent cover based on total area of buffer
  i.data <- aggregate(.~ CamStation + utmE + utmN + Treatment + lat_decdeg + lon_decdeg + Habitat_1, data = i.data, sum) # Aggregate same land cover polygons at each site
  i.data <- i.data[with(i.data, order(CamStation)), ] ## Order dataset by CamStation
  end.time<-paste("end time:", Sys.time())
  
  print(start.time)
  
  print(end.time)
  
  return(i.data)
}

## Test function at 250 m buffer
Hab.data <- propHab(Algcoord, AVIE, 250)

## Check which habitat types are NOT represented at every camera station with a 250m buffer 
LowSpruce <- Hab.data %>% filter(Habitat_1 == "LowSpruce") ## 58 stations
unique(LowSpruce$CamStation) ## missing Algar15, Algar 49
UpDecid <- Hab.data %>% filter(Habitat_1 == "UpDecid") ## 8 stations
Tamarack <- Hab.data %>% filter(Habitat_1 == "Tamarack") ## 35 stations
UpSpruce <- Hab.data %>% filter(Habitat_1 == "UpSpruce") ## 18 stations
LowDecid <- Hab.data %>% filter(Habitat_1 == "LowDecid") ## 0 stations -- would be easy to drop then...
Pine <- Hab.data %>% filter(Habitat_1 == "Pine") ## 17 stations

## None are represented at all stations. In order of decreasing abundance: LowSpruce, Tamarack, UpSpruce, Pine, UpDecid, LowDecid

## Do the same for largest scale (overwriting is fine for now)
Hab.data <- Hab.data <- propHab(Algcoord, AVIE, 2000)

## Check which habitat types are NOT represented at every camera station with a 250m buffer 
LowSpruce <- Hab.data %>% filter(Habitat_1 == "LowSpruce") ## 60 stations
UpDecid <- Hab.data %>% filter(Habitat_1 == "UpDecid") ## 51 stations
Tamarack <- Hab.data %>% filter(Habitat_1 == "Tamarack") ## 59 stations
UpSpruce <- Hab.data %>% filter(Habitat_1 == "UpSpruce") ## 54 stations
LowDecid <- Hab.data %>% filter(Habitat_1 == "LowDecid") ## 22 stations 
Pine <- Hab.data %>% filter(Habitat_1 == "Pine") ## 45 stations

## In order of decreasing abundance: LowSpruce, Tamarack, UpSpruce, UpDecid, Pine, LowDecid
