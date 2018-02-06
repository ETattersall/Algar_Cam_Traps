##################################
# Algar_fieldData.R
# Exploring and extracting useful data from field measurements
# Measurements taken Apr 2017, Nov 2017
#################################

library(dplyr)
library(tidyr)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data/Station_data")
apr <- read.csv("Algar_CameraFieldData_April2017.csv")
nov <- read.csv("Algar_CameraStationData_Nov2017.csv")

#Subset nov for 60 cams on lines
nov <- nov[1:60,]

#Compare variables

str(apr) #Line veg height, mounds, only measured once. Do not use these measurements
str(nov)
unique(nov$TreeSp_Dom1) # 4 : BlackSpruce, Tamarack, Aspen, JackPine
unique(apr$TreeSp_Dom1) # 5 levels: tamarack, black spruce, Aspen, birch, JackPine

#See if line dom. tree species, line widths match between Apr. and Nov.
ifelse(apr$TreeSp_Dom1==nov$TreeSp_Dom1,"Y", "N") # error because the level sets of factors are different
table(apr$TreeSp_Dom1) # Aspen-1, birch-1, black spruce-49, jackpine-1, tamarack-8
table(nov$TreeSp_Dom1) # Aspen-1, black spruce-52, jackpine-2, tamarack-5
table(ifelse(apr$Line_Width_m==nov$Line_Width_m,"Y", "N")) #49 do not agree
width.diff <- ifelse(apr$Line_Width_m==nov$Line_Width_m,"Y", apr$Line_Width_m - nov$Line_Width_m)
## Widths are very different. in places


## Possible variables of interest + hypotheses (general):
# Line veg height: Increase in veg height decreases pred. detections (movement barrier)
# Line width: Increase in line width increases pred. detections, dec. caribou detections
# Dom. tree species