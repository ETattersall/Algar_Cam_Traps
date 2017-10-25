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
for(row in 1:60){
  if(CamDat$Treatment[row]== "Control") {
    CamDat$Treat[row]== "Control"
  } else if(CamDat$Treatment[row]== "Human Use"){
    CamDat$Treat[row]== "HumanUse"
  } else if(CamDat$Treatment[row]== "Natural Regeneration"){
    CamDat$Treat[row]== "NatRegen"
  } else CamDat$Treat[row]== "SPP"
}
unique(CamDat$Treat)
fix(CamDat) # Manually change Treatment values

unique(CamDat$Treatment)



## First deployment --> only use first 24 cams (test Camelot with first deployment)
Cam24 <- CamDat[1:24,]

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

## Add Genus and Species columns
ImgDat$Genus <- rep(NA, 5741)
ImgDat$Species <- rep(NA, 5741)

# Populating Genus, species columns according to Timelapse code
for(i in 1:nrow(ImgDat)){
  if(ImgDat$Sp_code[i] == "O_virginianus") {
    ImgDat$Genus[i] = "Odocoileus"
    ImgDat$Species[i] = "virginianus"
  } else if(ImgDat$Sp_code[i] == "R_tarandus"){
    ImgDat$Genus[i] = "Rangifer"
    ImgDat$Species[i] = "tarandus"
  } else if(ImgDat$Sp_code[i] == "A_alces"){
    ImgDat$Genus[i] = "Alces"
    ImgDat$Species[i] = "alces"
  } else if(ImgDat$Sp_code[i] == "C_lupus"){
    ImgDat$Genus[i] = "Canis"
    ImgDat$Species[i] = "lupus"
  } else if(ImgDat$Sp_code[i] == "C_latrans"){
    ImgDat$Genus[i] = "Canis"
    ImgDat$Species[i] = "latrans"
  } else if(ImgDat$Sp_code[i] == "U_americanus"){
    ImgDat$Genus[i] = "Ursus"
    ImgDat$Species[i] = "americanus"
  }  else if(ImgDat$Sp_code[i] == "L_canadensis"){
    ImgDat$Genus[i] = "Lynx"
    ImgDat$Species[i] = "canadensis"
  } else if(ImgDat$Sp_code[i] == "G_gulo"){
    ImgDat$Genus[i] = "Gulo"
    ImgDat$Species[i] = "gulo"
  } else if(ImgDat$Sp_code[i] == "M_americana"){
    ImgDat$Genus[i] = "Martes"
    ImgDat$Species[i] = "americana"
  } else if(ImgDat$Sp_code[i] == "M_pennanti"){
    ImgDat$Genus[i] = "Martes"
    ImgDat$Species[i] = "pennanti"
  } else if(ImgDat$Sp_code[i] == "V_vulpes"){
    ImgDat$Genus[i] = "Vulpes"
    ImgDat$Species[i] = "vulpes"
  }  else if(ImgDat$Sp_code[i] == "T_hudsonicus"){
    ImgDat$Genus[i] = "Tamiasciurus"
    ImgDat$Species[i] = "hudsonicus"
  }  else if(ImgDat$Sp_code[i] == "L_americanus"){
    ImgDat$Genus[i] = "Lepus"
    ImgDat$Species[i] = "americanus"
  } else if(ImgDat$Sp_code[i] == "H_sapiens"){
    ImgDat$Genus[i] = "Homo"
    ImgDat$Species[i] = "sapiens"
  } else if(ImgDat$Sp_code[i] == "G_canadensis"){
    ImgDat$Genus[i] = "Grus"
    ImgDat$Species[i] = "canadensis"
  } else if (ImgDat$Sp_code[i] == "Other_birds"){
    ImgDat$Genus[i] = "Other bird"
    ImgDat$Species[i] = "Other bird"
  } else if(ImgDat$Sp_code[i] == "Other"){
    ImgDat$Genus[i] = "Other"
    ImgDat$Species[i] = "Other"
  }}

    
unique(ImgDat$Genus)
head(ImgDat$Genus)
table(ImgDat$Genus)


# Selecting relevant columns
ImgDat <- ImgDat %>% select(File, RelativePath, Folder, Date, Time, Temperature, MoonPhase, Genus, Species, SpCount, Sex, Age)


#### 3. Bulk Import CSV from Camelot ####
Camelot <- read.csv("Camelot_BulkImport_2015.01.csv")
head(Camelot)
colnames(Camelot)

