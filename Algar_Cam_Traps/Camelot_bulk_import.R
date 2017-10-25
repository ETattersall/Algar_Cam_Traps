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
fix(CamDat)

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

ImgDat <- ImgDat %>% gather(key = Species, value = SpeciesCount, O_virginianus, R_tarandus, C_elavus, A_alces, C_lupus, C_latrans, U_americanus, L_canadensis, G_gulo, M_americana, M_pennanti, V_vulpes, T_hudsonicus, L_americanus, H_sapiens, G_canadensis, Other_birds, Other)
colnames(ImgDat)

ImgDat <- ImgDat %>% select(File, RelativePath, Folder, Date, Time, Temperature, MoonPhase, Species, SpeciesCount, Sex, Age)


