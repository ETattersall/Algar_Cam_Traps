################################################
# Camelot_bulk_import.R
# Preparing a CSV for bulk import into Camelot
# Started by Erin T., Oct. 25, 2017
################################################

library(dplyr)
library(tidyr)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

#### 1. Prepare Camera Station metadata ####
## Done in Algar_coord_convert.R --> converted UTM to Lat-long and gathered relevant columns

CamDat <- read.csv("Station_data/Algar_DeployData.csv")
colnames(CamDat)
CamDat$X <- NULL

## Need to  standardize Treatment column
unique(CamDat$Treatment)

## for loop standardizing treatment column--> doesn't work :( Fix manually
# for(row in 1:60){
#  if(CamDat$Treatment[row]== "Control") {
#   CamDat$Treat[row]== "Control"
#  } else if(CamDat$Treatment[row]== "Human Use"){
#    CamDat$Treat[row]== "HumanUse"
#  } else if(CamDat$Treatment[row]== "Natural Regeneration"){
#    CamDat$Treat[row]== "NatRegen"
#  } else CamDat$Treat[row]== "SPP"
#}

unique(CamDat$Treat)
fix(CamDat) # Manually change Treatment values. Also need to remove symbols from Time and height columns

write.csv(CamDat, "Station_data/Algar_DeployData.csv") ## Save standardization steps
CamDat <- read.csv("Station_data/Algar_DeployData.csv")
CamDat$X <- NULL

unique(CamDat$Treatment)

glimpse(CamDat)

# DeployDate, Deploy Time need to be POSIX class (in %Y - %m - %d format)
CamDat$DeployDate <- as.POSIXct(strptime(CamDat$DeployDate, format = "%d/%m/%Y"))

glimpse(CamDat)
str(CamDat)

## Session End Date in the future... 30/12/2020
## Redult: Camelot didn't like this :(
# CamDat$EndDate <- as.factor("30/12/2020")
# str(CamDat$EndDate)
# CamDat$EndDate <- as.POSIXct(strptime(CamDat$EndDate, format = "%d/%m/%Y"))
# str(CamDat$EndDate)

## First deployment --> only use first 24 cams (test Camelot with first deployment)
Cam24 <- CamDat[1:24,]

## Need to add Retrieval/Check Dates. Take from Algar_stationdata_apr2017.csv
Cam.apr <- read.csv("Station_data/Algar_stationdata_apr2017.csv")
str(Cam.apr)
CheckDate1 <- Cam.apr$CheckDate1
Cam24$CheckDate1 <- CheckDate1[1:24]
glimpse(Cam24)
Cam24$CheckDate1 <- as.POSIXct(strptime(Cam24$CheckDate1, format = "%d/%m/%Y"))
glimpse(Cam24)



#### 2.  Retrieve image processing data from Timelapse CSVs ####
ImgDat <- read.csv("2015.01CSVs_12Dec_2016.csv")
colnames(ImgDat)
ImgDat$X.1 <- NULL
ImgDat$X <- NULL

unique(ImgDat$Other_specify) # [1] NA  1  2  3  4  5
class(ImgDat$Other_specify) # integer. Disregard for now

## Gathering Species count columns into a species (name 'Sp_code' - need to add a Species column later) and a Count column, keep only rows with an animal present
ImgDat <- ImgDat %>% gather(key = Sp_code, 
                            value = SpCount, 
                            O_virginianus, R_tarandus, C_elavus, A_alces, C_lupus, C_latrans, U_americanus, L_canadensis, G_gulo, M_americana, M_pennanti, V_vulpes, T_hudsonicus, L_americanus, H_sapiens, G_canadensis, Other_birds, Other) %>% 
                            filter(SpCount>0)
colnames(ImgDat)
unique(ImgDat$Sp_code)

## Add Genus, Species, and common name columns
ImgDat$Genus <- rep(NA, 5741)
ImgDat$Species <- rep(NA, 5741)
ImgDat$ComNam <- rep(NA, 5741)

# Populating Genus, species, and common name columns according to Timelapse code
for(i in 1:nrow(ImgDat)){
  if(ImgDat$Sp_code[i] == "O_virginianus") {
    ImgDat$Genus[i] = "Odocoileus"
    ImgDat$Species[i] = "virginianus"
    ImgDat$ComNam[i] = "White-Tailed Deer"
  } else if(ImgDat$Sp_code[i] == "R_tarandus"){
    ImgDat$Genus[i] = "Rangifer"
    ImgDat$Species[i] = "tarandus"
    ImgDat$ComNam[i] = "Caribou"
  } else if(ImgDat$Sp_code[i] == "A_alces"){
    ImgDat$Genus[i] = "Alces"
    ImgDat$Species[i] = "alces"
    ImgDat$ComNam[i] = "Moose"
  } else if(ImgDat$Sp_code[i] == "C_lupus"){
    ImgDat$Genus[i] = "Canis"
    ImgDat$Species[i] = "lupus"
    ImgDat$ComNam[i] = "Gray Wolf"
  } else if(ImgDat$Sp_code[i] == "C_latrans"){
    ImgDat$Genus[i] = "Canis"
    ImgDat$Species[i] = "latrans"
    ImgDat$ComNam[i] = "Coyote"
  } else if(ImgDat$Sp_code[i] == "U_americanus"){
    ImgDat$Genus[i] = "Ursus"
    ImgDat$Species[i] = "americanus"
    ImgDat$ComNam[i] = "American Black Bear"
  }  else if(ImgDat$Sp_code[i] == "L_canadensis"){
    ImgDat$Genus[i] = "Lynx"
    ImgDat$Species[i] = "canadensis"
    ImgDat$ComNam[i] = "Canadian Lynx"
  } else if(ImgDat$Sp_code[i] == "G_gulo"){
    ImgDat$Genus[i] = "Gulo"
    ImgDat$Species[i] = "gulo"
    ImgDat$ComNam[i] = "Wolverine"
  } else if(ImgDat$Sp_code[i] == "M_americana"){
    ImgDat$Genus[i] = "Martes"
    ImgDat$Species[i] = "americana"
    ImgDat$ComNam[i] = "American Marten"
  } else if(ImgDat$Sp_code[i] == "M_pennanti"){
    ImgDat$Genus[i] = "Martes"
    ImgDat$Species[i] = "pennanti"
    ImgDat$ComNam[i] = "Fisher"
  } else if(ImgDat$Sp_code[i] == "V_vulpes"){
    ImgDat$Genus[i] = "Vulpes"
    ImgDat$Species[i] = "vulpes"
    ImgDat$ComNam[i] = "Red Fox"
  }  else if(ImgDat$Sp_code[i] == "T_hudsonicus"){
    ImgDat$Genus[i] = "Tamiasciurus"
    ImgDat$Species[i] = "hudsonicus"
    ImgDat$ComNam[i] = "Red Squirrel"
  }  else if(ImgDat$Sp_code[i] == "L_americanus"){
    ImgDat$Genus[i] = "Lepus"
    ImgDat$Species[i] = "americanus"
    ImgDat$ComNam[i] = "Snowshoe Hare"
  } else if(ImgDat$Sp_code[i] == "H_sapiens"){
    ImgDat$Genus[i] = "Homo"
    ImgDat$Species[i] = "sapiens"
    ImgDat$ComNam[i] = "Human"
  } else if(ImgDat$Sp_code[i] == "G_canadensis"){
    ImgDat$Genus[i] = "Grus"
    ImgDat$Species[i] = "canadensis"
    ImgDat$ComNam[i] = "Sandhill Crane"
  } else if (ImgDat$Sp_code[i] == "Other_birds"){
    ImgDat$Genus[i] = "Other bird"
    ImgDat$Species[i] = "Other bird"
    ImgDat$ComNam[i] = "Other bird"
  } else if(ImgDat$Sp_code[i] == "Other"){
    ImgDat$Genus[i] = "Other"
    ImgDat$Species[i] = "Other"
    ImgDat$ComNam[i] = "Other"
  }}





    
unique(ImgDat$Genus)
head(ImgDat$Genus)
table(ImgDat$Genus)
unique(ImgDat$ComNam)

# Selecting relevant columns
ImgDat <- ImgDat %>% select(File, RelativePath, Folder, Date, Time, Temperature, MoonPhase, ComNam, Genus, Species, SpCount, Sex, Age)
glimpse(ImgDat)
unique(ImgDat$Sex)

## To make Folder name match Station in Station dataframe, need to use revalue (restart R and load plyr)
library(plyr)

ImgDat$Folder <- revalue(ImgDat$Folder, replace = c("Algar1" = "Algar01", "Algar2" = "Algar02", "Algar3" = "Algar03", "Algar4" = "Algar04", "Algar5" = "Algar05", "Algar6" = "Algar06", "Algar7" = "Algar07", "Algar8" = "Algar08", "Algar9" = "Algar09"))




#### 3. Bulk Import CSV from Camelot ####
## Merge image data LAST (after station data)
Camelot <- read.csv("Camelot_BulkImport_2015.01.csv")
head(Camelot) #Path.Component.9 = Station names. Add new Station column with same names, revalued to add 0's
Camelot$Station <- Camelot$Path.Component.9

Camelot$Station <- revalue(Camelot$Station, replace = c("Algar1" = "Algar01", "Algar2" = "Algar02", "Algar3" = "Algar03", "Algar4" = "Algar04", "Algar5" = "Algar05", "Algar6" = "Algar06", "Algar7" = "Algar07", "Algar8" = "Algar08", "Algar9" = "Algar09"))
head(Camelot)



## Combining station data to Camelot data by matching Cam24$CamStation to Camelot$Path.Component.9
## merge function
IS <- merge(Camelot, Cam24, by.x = "Station", by.y = "CamStation")
colnames(IS)

## Combining Timelapse image data with others by matching file names
ISC <- merge(IS, ImgDat, by.x = "File.Name", by.y = "File", all = TRUE) #all = TRUE ensure images without a match are appended
head(ISC)

## Adding a Site name (just Algar)
ISC$Site.Name <- "Algar"
str(ISC)



##### Sighting fields that Camelot has an issue with:
# Sighting quantity (SpCount): all should be an integer... change NAs to 0?
summary(ISC$SpCount)
ISC$SpCount[is.na(ISC$SpCount)] <- 0
summary(ISC$SpCount)

## Should be 18036 rows but there are 18039...check if Camelot import works?
write.csv(ISC, "2015.01Camelot_Import.csv")



