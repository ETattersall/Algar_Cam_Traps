### Copying Timelapse images into folders for video clips
## Need to read Timelapse images from master CSV, pull into a spearate folder (unique to that camera station)
 
library(dplyr) ## Load for use of pipes



## Access data directory to read in csvs
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data") 

##Read in master CSV for 2016.01 deployment (Winter, snow only)
CSVs.2016.01 <- read.csv("2016.01CSVs_06June_2017.csv", header = TRUE)

## Subset data frame for Timelapse images for single camera station
Time.14 <- CSVs.2016.01 %>%
  filter(Folder == "ALGAR14" & TriggerMode == "T") %>% 
  select(File)

## Set working directory to camera station
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images/Algar14")

##Creates a vector of Timelapse image file names from list structure (list produced during subsetting above)
vec14 <- unlist(Time.14)

## Create a folder named "Algar13_timelapse" (done manually)
## Copy Timelapse images into new folder (within that camera station)
file.copy(vec14,"Algar14_timelapse")
