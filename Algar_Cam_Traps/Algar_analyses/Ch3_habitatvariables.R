############################################
## Ch3_habitatvariables.R
## Creating habitat variables based on Fisher & Burton habitat classes (species, % canopy, and moisture regime)
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
class(AVIE@data)
class(AVIE)

## Create new vector = to number of polygons in AVIE (22746)
AVIEData$Habitat <- rep(NA, 22746)

### Populate Habitat attribute according to Fisher and Burton's reclassifications

## Nest ifelse statement: Tamarack = all Lt >= 70%
## One of Columns 5,7,9,11,13 has to be Lt, and Per column has to be >=70%

