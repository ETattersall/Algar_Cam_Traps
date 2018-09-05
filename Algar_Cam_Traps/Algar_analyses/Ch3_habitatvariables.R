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
str(AVIEData)
class(AVIE@data)
class(AVIE)

## Checking mean percent classes for Sp1, Sp2, and Sp3
summary(AVIEData) # sP1_PER mean = 7.9 (med = 9), SP2_PER mean = 0.9 (med = 0), SP3_PER mean = 0.14 (med = 0)
## Can safely use SP1 and SP2 only


### Populate Habitat attribute according to Fisher and Burton's reclassifications (with alterations for simplicity, noted here)

## Nest ifelse statement-- Tamarack: Lt >= 70%, Pine: Pj >=70%, upland decid: Aw, Pb, Bw >= 70%
## Dominant species (SP1, column 5) has to be Lt, and Per column has to be >=70%


AVIEData$Habitat <- ifelse(test = is.na(AVIEData[ , 5]), yes = "Non-forest", 
                           no = ifelse(test = AVIEData[ , 5] == "Lt" & AVIEData[ , 6] >= 7, yes = "Tamarack", 
            no = ifelse(test = AVIEData[ , 5] == "Pj" & AVIEData[ , 6] >= 7,yes = "Pine", 
                                      no = ifelse(test = AVIEData[ , 5] == "Aw" & AVIEData[ , 6] >= 7| AVIEData[ , 5] == "Pb" & AVIEData[ , 6] >= 7 | AVIEData[ , 5] == "Bw" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d", yes = "UpDecid", 
                                                  no = ifelse(test = AVIEData[ , 5] == "Aw" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Pb" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Bw" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w", yes = "LowDecid", 
                                                              no = ifelse(test = AVIEData[ , 5] == "Sb" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Sw" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Fb" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d", yes = "UpSpruce", no = ifelse(test = AVIEData[ , 5] == "Sb" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Sw" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Fb" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w", yes = "LowSpruce", 
                           no = ifelse(test = AVIEData[ , 5] == "Sb" & AVIEData[ , 7] == "Aw" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Aw" & AVIEData[ , 7] == "Sb" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Sw" & AVIEData[ , 7] == "Aw" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Aw" & AVIEData[ , 7] == "Sw" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Pj" & AVIEData[ , 7] == "Aw" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Aw" & AVIEData[ , 7] == "Pj" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Bw" & AVIEData[ , 7] == "Lt" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Lt" & AVIEData[ , 7] == "Bw" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Bw" & AVIEData[ , 7] == "Sw" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Sw" & AVIEData[ , 7] == "Bw" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Pb" & AVIEData[ , 7] == "Sw" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Sw" & AVIEData[ , 7] == "Pb" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d", yes = "UpMix", 
                                       no = ifelse(test = AVIEData[ , 5] == "Sb" & AVIEData[ , 7] == "Aw" &  AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Aw" & AVIEData[ , 7] == "Sb" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Sw" & AVIEData[ , 7] == "Aw" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Aw" & AVIEData[ , 7] == "Sw" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Pj" & AVIEData[ , 7] == "Aw" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Aw" & AVIEData[ , 7] == "Pj" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w"  | AVIEData[ , 5] == "Bw" & AVIEData[ , 7] == "Lt" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Lt" & AVIEData[ , 7] == "Bw" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Bw" & AVIEData[ , 7] == "Sw" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Sw" & AVIEData[ , 7] == "Bw" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Pb" & AVIEData[ , 7] == "Sw" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Sw" & AVIEData[ , 7] == "Pb" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w", yes = "LowMix", no = NA)))))))))

table(AVIEData$Habitat) ## --> LowMix not coming out?
AVIEData[which(AVIEData$Habitat == "LowMix"), ] ## ZERO LowMixes

## Checking that ifelse statement is working properly 
print(head(AVIEData[ , c(2,5,6,7,8,154)], 100))
table(is.na(AVIEData$Habitat)) #840 after adding common mixedwoods

## Checking remaining mixedwood classes
AVIEData[which(is.na(AVIEData$Habitat)), c(5,6,7,8,154)]


## Code chunks for each class
## Tamarack: Lt >= 7
ifelse(test = AVIEData[ , 5] == "Lt" & AVIEData[ , 6] >= 7, yes = "Tamarack", no = NA)

## Pine: Pj >= 7
ifelse(test = AVIEData[ , 5] == "Pj" & AVIEData[ , 6] >= 7,yes = "Pine", 
       no = NA)

## Non-forest = SP1 == NA
ifelse(test = is.na(AVIEData[ , 5]), yes = "Non-forest", no = NA)

## Up decid: Sp1 and Sp2 == Aw, Pb or Bw and SP1_PER >= 70) and moist == m or d
ifelse(test = AVIEData[ , 5] == "Aw" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Pb" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Bw" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d", yes = "UpDecid", no = NA)

## Low decid: same as up decid except moisture regime is w or a
ifelse(test = AVIEData[ , 5] == "Aw" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Pb" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Bw" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w", yes = "LowDecid", no = NA)

## Up. spruce: SP1 = Sb, Sw, or Fb >= 7 and moist = m or d
ifelse(test = AVIEData[ , 5] == "Sb" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Sw" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Fb" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d", yes = "UpSpruce", no = NA)

## Low. spruce: same as UpSpruce except moist = a or w
ifelse(test = AVIEData[ , 5] == "Sb" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Sw" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Fb" & AVIEData[ , 6] >= 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w", yes = "LowSpruce", no = NA)



## Up. Mixedwood: 30 possible combinations of conifers and decid. starting with the most common, with SP1 canopy cover between 4 and 6
UpMix <- ifelse(test = AVIEData[ , 5] == "Sb" & AVIEData[ , 7] == "Aw" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Aw" & AVIEData[ , 7] == "Sb" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Sw" & AVIEData[ , 7] == "Aw" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Aw" & AVIEData[ , 7] == "Sw" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Pj" & AVIEData[ , 7] == "Aw" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Aw" & AVIEData[ , 7] == "Pj" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Bw" & AVIEData[ , 7] == "Lt" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Lt" & AVIEData[ , 7] == "Bw" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Bw" & AVIEData[ , 7] == "Sw" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Sw" & AVIEData[ , 7] == "Bw" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Pb" & AVIEData[ , 7] == "Sw" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Sw" & AVIEData[ , 7] == "Pb" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d", yes = "UpMix", no = NA)

head(UpMix, 200) ## Working
table(is.na(UpMix))

## LowMix: same as up except moist. regime
LowMix <- ifelse(test = AVIEData[ , 5] == "Sb" & AVIEData[ , 7] == "Aw" &  AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Aw" & AVIEData[ , 7] == "Sb" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Sw" & AVIEData[ , 7] == "Aw" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Aw" & AVIEData[ , 7] == "Sw" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Pj" & AVIEData[ , 7] == "Aw" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Aw" & AVIEData[ , 7] == "Pj" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w"  | AVIEData[ , 5] == "Bw" & AVIEData[ , 7] == "Lt" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Lt" & AVIEData[ , 7] == "Bw" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Bw" & AVIEData[ , 7] == "Sw" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Sw" & AVIEData[ , 7] == "Bw" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Pb" & AVIEData[ , 7] == "Sw" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Sw" & AVIEData[ , 7] == "Pb" & AVIEData[ , 6] < 7 & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w", yes = "LowMix", no = NA)
head(LowMix, 200)
table(is.na(LowMix))


#### For some reason Lowland mixedwood is not included when code is run (appears to be working correctly)
## Mixedwood overall makes up very little of Algar landscape -- exclude completely and incorporate into other categories?
## Habitat covariates therefore based on Dominant forest cover and moisture regime
## Removing canopy cover requirements and mixedwood
AVIEData$Habitat <- ifelse(test = is.na(AVIEData[ , 5]), yes = "Non-forest", 
                           no = ifelse(test = AVIEData[ , 5] == "Lt", yes = "Tamarack", 
                           no = ifelse(test = AVIEData[ , 5] == "Pj",yes = "Pine", 
                           no = ifelse(test = AVIEData[ , 5] == "Aw"| AVIEData[ , 5] == "Pb" | AVIEData[ , 5] == "Bw" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d", yes = "UpDecid", 
                           no = ifelse(test = AVIEData[ , 5] == "Aw" & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Pb" & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Bw" & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w", yes = "LowDecid", 
                           no = ifelse(test = AVIEData[ , 5] == "Sb" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Sw"  & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d" | AVIEData[ , 5] == "Fb" & AVIEData$MOIST_REG == "m" | AVIEData$MOIST_REG == "d", yes = "UpSpruce", 
                           no = ifelse(test = AVIEData[ , 5] == "Sb" & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Sw" & AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w" | AVIEData[ , 5] == "Fb"& AVIEData$MOIST_REG == "a" | AVIEData$MOIST_REG == "w", yes = "LowSpruce", no = NA)))))))

table(AVIEData$Habitat)


## Checking that ifelse statement is working properly 
print(head(AVIEData[ , c(2,5,6,7,8,154)], 100))
table(is.na(AVIEData$Habitat)) ## No NAs


### Merge AVIEData$Habitat with AVIE spatialpolygons dataframe
class(AVIE)

AVIE.hab <- sp::merge(AVIE,AVIEData$Habitat, by = "POLY_NUM") ## ran out of memory space
