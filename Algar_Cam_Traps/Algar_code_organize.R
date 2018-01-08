###########################
# Dat summaries, modified for each deployment
# .RData Workspace in Algar_git
# Started by Erin T., Feb 7, 2017...June 2, 2017 modified for 2016.01 deployment
# Apr 2017 - Nov2017 deployment summaries started Dec. 7, 2017 (some steps aren't necessary, Camelot creates data summaries)
# Code organization for Algar Camera Trap Data
############################

## Station Folders created (createStationFolders), Images already renamed (see camtrapR_imageRename.R)
library(camtrapR)
library(tidyr)
library(dplyr)
library(ggplot2)


####### Cole's code
### Load camera station data for the deployed cameras
images_wd <- "E:/Algar_Apr-Nov2017/Renamed_Images"
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data/Station_data")
camdata <- read.csv("AlgarStations_DeploymentData.csv")

## plotting cam stations
with(camdata,plot(utmE,utmN))



# specify folder with renamed images
setwd(images_wd)
 

# check number of JPG images [this stuff isn't needed now that we have tagged images]
length(list.files(images_wd, pattern = "JPG", recursive = TRUE)) # [1] 63840 --> includes stations with misfiring cameras

# try this for each station (Image count per Station)
stations <- as.character(camdata$CamStation[1:60]) ## Need to exclude offline sites, which currently have no data


stationDir <- paste("E:/Algar_Apr-Nov2017/Renamed_Images",stations,sep="/")

stn.img <- rep(NA,length(stations))
for (i in 1:length(stations)) {
  stn.img[i] <- length(list.files(stationDir[i], pattern = "JPG"))
}
sum(stn.img)

## Create a station summary data frame for this deployment
stn.sum <- camdata[1:60,] %>% select(CamStation, utmE, utmN, Treatment, Session3Start, Problem2_from, Problem2_to,Session4Start)
## Ad station images to stn.sum
stn.sum$StationImages <- stn.img

# tally survey effort


## Added columns "Problem1_from" and "Problem1_to" to CTtable ('Algar_stationdata.2016.01.csv')

camEff <- cameraOperation(stn.sum, 
                          stationCol = "CamStation", 
                          setupCol = "Session3Start", 
                          retrievalCol = "Session4Start",
                          hasProblems = TRUE,
                          dateFormat = "%d/%m/%Y", 
                          writecsv = FALSE)

sum(camEff,na.rm=T) ## 7703

# days per station
summary(rowSums(camEff,na.rm=T)) # mean = 128.4, median = 203, range = 0 - 206

# add to camera station table
cams2016$trapdays <- rowSums(camEff,na.rm=T)

#### Done by Camelot, not necessary for apr 2017 - nov 2017 ####
# subtract days from total images for estimate of motion triggers [not needed, see image data below]
## Not done for 2016.01 deployment
cams2016$EstTrig <- cams2016$img.cnt - cams2016$trapdays

# quick-and-dirty plot of number of motion triggers by treatment 
boxplot(cams2015$EstTrig ~ cams2015$Treatment,col=c("orange","purple"),ylab="Number of Motion Triggers",
        cex.lab=1.5,cex.axis=1.5)
##Above plots each image

################################

##### Erin's code
### Merging csvs (copied from script 'file_rename_csvs.R')

setwd("C:/Users/ETattersall/Documents/Sync/Algar/2016.01")

## Creating objects for csv original and destination folders
csvs_2016.01 <- paste(cams2016.01, "csv", sep = ".")
from_csvs <- paste(cams2016.01, csvs_2016.01, sep = "/")
to_csvs <- paste("CSVs_2016.01", csvs_2016.01, sep = "/")

### Copying csvs into one folder
file.copy(from = from_csvs, to = to_csvs, overwrite = FALSE, recursive = TRUE, copy.mode = TRUE, copy.date = TRUE)
## Removing csvs from original folder
file.remove(from_csvs)

############ Original Algar code (Dec. 9, 2016), modified for 2016.01 deployment (June 1, 2017)
## Renaming CSVs with date
setwd("C:/Users/ETattersall/Documents/Sync/Algar/2016.01/CSvs_2016.01")
n.name <- paste(cams2016.01,"01June_2017", sep = "_")
name.csv <- paste(n.name, "csv", sep = ".")

file.rename(csvs_2016.01,name.csv)

library(plyr)
library(dplyr)
setwd("C:/Users/ETattersall/Documents/Sync/Algar/2016.01/CSvs_2016.01")
Algar08_2016.01 <- read.csv("ALGAR08_01June_2017.csv")

### rbind.fill (in plyr) merges csvs and fills missing columns with NA
filenames <- list.files()
x2016.01 <- do.call("rbind.fill", lapply(filenames, read.csv, header = TRUE, stringsAsFactors = FALSE)) #### stringsAsFactors req. to prevent comments (character class) from turning into integer class

### rbind.fill (in plyr) merges csvs and fills missing columns with NA


write.csv(x2016.01, "2016.01CSVs_06June_2017.csv") ### Needs to be edited to delete Algar49 wolf detection on 28/03/2017 (during malfunction period -- how?)

## Checking data classes
class(x2016.01$File)
class(x2016.01$Animal)
### Coerce File names to factors (to prevent it from being affected by turning all characters upper case later)
### Need Folders to be factors too!!!
x2016.01$File <- as.factor(x2016.01$File)
class(x2016.01$File)

#### Cole's code

## Make all lowercase letters uppercase for true/false data only!!!
## THIS function makes all characters Upper case
x2016.01 <- data.frame(lapply(x2016.01, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
})) 




# check for NA's in counts (shouldn't be any)
apply(apply(x2016.01[,"SpeciesCount", drop = F],2,is.na),2,sum) ## 3


# need to fix NA values in species counts ... [missing values in csv files]
## Searching for NA's
x2016.01 %>% filter(is.na(SpeciesCount)) %>% select(File, Animal, Unknown, Species, SpeciesCount) 

### Examine and edit where necessary
fix(x2016.01, file = "2016.01CSVs_06June_2017.csv")



# how many photos are of animals 
table(x2016.01$Animal) ##3044 animal images
table(x2016.01$TriggerMode) ##5064




# subset data to only animal photos that are not classified as unknown


A2 <- subset(x2016.01, Animal == TRUE & Unknown == FALSE)
A2$X <- NULL


# Checking for 0's in SpeciesCount
A2.0counts <- A2[which(A2$SpeciesCount==0),] ## 40 Counts. Fix manually

fix(x2016.01, file = "C:/Users/ETattersall/Documents/Sync/Algar/2016.01/CSvs_2016.01/2016.01CSVs_06June_2017.csv")



#############For old Algar Timelapse template #### Probably faster to use fix()
# add missing counts of snowshoe hare for others (Algar16__2016-09-25__21-40-28(1).JPG and subsequent)
A2[which(A2$count==0)[c(1:8)],"L_americanus"] <- 1

# add missing counts for 2 bird photos (Algar4__2016-09-06__19-46-06(1).JPG and Algar4__2016-09-18__08-52-16(2).JPG 
A2[which(A2$count==0)[c(9,10)],"Other_birds"] <- 1

# drop an image that didn't have an animal (checked, Algar6__2016-01-22__08-13-15(5).JPG)
A2 <- A2[-which(A2$count==0)[11],] 

### when changing count data use row number returned by A2[which(A2$count==0),] (ex. 1,2,3 not 2323, 6345, 9224)

# update the count variable
A2$count <- apply(A2[,14:31],1,max, na.rm=T)

summary(A2$count) # all zeros now gone

A2[which(A2$count==0),]

## Have to drop image without animal again...
A2 <- A2[-which(A2$count==0)[1],]

# update the count variable
A2$count <- apply(A2[,14:31],1,max, na.rm=T)

summary(A2$count) # all zeros now gone

A2[which(A2$count==0),]

# create variable with species name (Adding Species column)
for(i in 1:nrow(A2)) {
  A2$Species[i] <- names(which.max(A2[i,14:31]))
}

################################# Old Timelapse Template dataframe stuff above

# some summaries
nrow(A2) ### 3007 Animal images (including humans)

table(A2$Species)

# check the Other class
A2$OtherSpecify[which(A2$Species=="OTHER")] 

A2 %>% filter(Species == "OTHER") %>% select(File,Species,Unknown, Review,OtherSpecify,SpeciesCount, Comments) ##Reviewed wolf
#fix
fix(x2016.01, file = "C:/Users/ETattersall/Documents/Sync/Algar/2016.01/CSvs_2016.01/2016.01CSVs_06June_2017.csv")

##Done working with x2016.01. Write to csv
write.csv(x2016.01,"2016.01CSVs_06June_2017.csv")

A2$OtherSpecify[which(A2$Species=="OTHER_BIRDS")] ## Grouse, greyjays, great grey owl

table(A2$Folder)
# Add Treatment to detections 

A2$Treatment <- cams2016$TreatmentType[match(A2$Folder,cams2016$CamStation)] ##Isn't working because Folders are upper case...
##Try
cams2016$CamStation <- toupper(cams2016$CamStation) ## Works now

table(A2$Treatment)
###--- DETECTION RATE INDEX

# I will start with a liberal index of allowing 1 photo per minute (rather than hour)
# to set up, follow code used for Boreal Deer dataset (e.g. from SpatialCountDensity.R)

# specify date formats 
A2$Date.Time <- paste(A2$Date,A2$Time,sep = " ")
A2$Date.Time <- as.POSIXct(strptime(A2$Date.Time, "%d-%b-%Y %H:%M:%S", tz="MST"))

# calculate a unique day for each day of study
# taking straight difference will include partial days, so is really 24-hour periods from time first camera set
# using "floor" so it doesn't round up to next day
A2$StudyDay <- floor(as.numeric(difftime(max(A2$Date.Time),min(A2$Date.Time),units="days")))

### THERE ARE INCONSISTENCIES IN HOW YEAR IS SPECIFIED IN THESE DATES -- NEED TO FIX

###################### Erin's code

### Organizing by species
## Starting with A2, which has a count column and Species column

#### Vectors of all species to input into loop
unique(A2$Species)

species <- as.character(unique(A2$Species))


### Duplicate Raw_images folder and rename it "Species_images" (Did this manually...)
### In future, probably makes sense to remain in Raw_images and have backup raw images elsewhere


setwd(images_wd)

### Create species folders in duplicated folder

SpecFolderCreate1 <- createSpeciesFolders (inDir               = "2016.01/Species_Images",
                                           species             = species,
                                           hasCameraFolders    = FALSE,
                                           removeFolders       = FALSE)



#### Code works for looping all species in one folder, for now just repeat this 24 times (Can select all and run at once) ####
## Loop for organizing photos into species folders (1 station only)
##Still have to remove other images from each folder manually...ALSO removed wolf photo from Algar49 during misfiring period


## Algar 01
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar01")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR01" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
} 

## Algar 02
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar02")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR02" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 03
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar03")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR03" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 04
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar04")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR04" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
############################################### RUN 12:42PM JUNE 6, 2017
## Algar 05
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar05")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR05" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 06
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar06")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR06" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 07
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar07")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR07" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 08
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar08")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR08" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 09
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar09")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR09" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 10
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar10")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR10" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 11
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar11")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR11" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 12
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar12")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR12" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 13
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar13")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR13" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 14
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar14")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR14" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 15
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar15")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR15" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 16
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar16")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR16" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 17
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar17")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR17" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 18
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar18")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR18" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 19
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar19")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR19" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 20
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar20")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR20" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 21
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar21")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR21" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 22
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar22")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR22" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 23
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar23")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR23" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 24
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar24")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR24" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 25
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar25")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR25" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 26
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar26")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR26" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 27
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar27")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR27" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 28
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar28")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR28" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 29
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar29")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR29" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 30
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar30")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR30" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 31
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar31")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR31" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 32
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar32")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR32" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 33
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar33")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR33" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 34
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar34")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR34" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 35
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar35")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR35" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 36
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar36")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR36" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 37
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar37")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR37" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 38
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar38")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR38" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 39
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar39")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR39" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 40
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar40")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR40" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}


## Algar 41
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar41")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR41" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 42
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar42")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR42" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 43
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar43")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR43" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 44
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar44")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR44" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 45
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar45")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR45" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 46
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar46")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR46" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 47
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar47")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR47" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 48
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar48")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR48" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 49
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar49")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR49" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 50
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar50")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR50" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 51
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar51")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR51" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 52
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar52")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR52" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 53
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar53")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR53" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 54
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar54")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR54" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 55
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar55")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR55" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}


## Algar 56
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar56")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR56" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 57
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar57")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR57" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 58
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar58")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR58" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 59
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar59")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR59" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

## Algar 60
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar60")
for (sp in species) {
  f_sp_fol <- A2 %>%
    filter(Folder == "ALGAR60" & Species == sp) %>% 
    select(File)
  vec_sp_fol <- unlist(f_sp_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}





setwd(images_wd)

### May want to remove images from Station folders (keep only those organized into species folders)--> do this manually?
## Need to clean up to run recordTable()

###### Image data extraction


###1. recordTable --> need it to run a number of different functions
### generates a dataframe of events


exiftool_dir<-"C:/Users/ETattersall/Desktop"
exiftoolPath(exiftoolDir = exiftool_dir) 
grepl(exiftool_dir, Sys.getenv("PATH"))

##If you want to add other metadata to record table, 
##you can view metadata with exifTagNames, choose what to include
species_wd <- "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images"
exifTagNames(inDir = species_wd, whichSubDir = 1,
             returnMetadata = TRUE)




rec.2016.01 <- recordTable(inDir                  = species_wd,
                            IDfrom                 = "directory",
                            minDeltaTime           = 30,
                            deltaTimeComparedTo    = "lastRecord",
                            timeZone               = "Canada/Mountain",
                            metadataSpeciesTag     = "TriggerMode")




write.csv(rec.2016.01, "2016.01_recordTable.csv")

### Each event = 30 minutes after last record. MAKE recordTable with 
## deltaTimeComparedTo = lastRecord (30 minutes after last event ended)

#delta.time indicates time since last occurrance of that species at that site

### NOTE: recordTable extracts 1 file from each independent event. This means that 
### it is used for getSpeciesImages, only 1 file is extracted from each event (1st ### file). Only a problem when multiple individuals are in one event (wolf pack)


###2. recordTableIndividual--> record table for one species (both getSpeciesImages and recordTableIndividual only take one species at a time)


####Folder of images organized by species (still 2015.01 deployment)
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2015.01")
image_identify <- unique(rec.spec$Species)

for (id in image_identify) {
  getSpeciesImages(species                 = id,
                   recordTable             = rec.spec,
                   speciesCol              = "Species",
                   stationCol              = "Station",
                   outDir                  = "Species_org",
                   createStationSubfolders = TRUE)
}

exiftool_dir<-"C:/Users/ETattersall/Desktop"
exiftoolPath(exiftoolDir = exiftool_dir) 
grepl(exiftool_dir, Sys.getenv("PATH"))

bear.tab <- recordTableIndividual(inDir = "Species_org/U_americanus",
                                  hasStationFolders = TRUE,
                                  IDfrom = "directory",
                                  minDeltaTime = 30,
                                  deltaTimeComparedTo = "lastRecord",
                                  timeZone = "Canada/Mountain",
                                  writecsv = FALSE,
                                  additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))

#### Removing 'No Animal' images from record table ####

## Import recordTables created by Camelot
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
det_data <- read.csv("2017.01_recordTable.csv")

## Remove Species = No Animal (Timelapse and Misfires)
det_data <- det_data[!det_data$Species == "No Animal", ] # 1247 obs., fewer than the 1410 reported in other Camelot summaries
unique(det_data$Species)
summary(det_data$Species)
# Camelot summary output determines independent obs. bade on specific criteria. For consistency with early datam use detections from record table

write.csv(det_data, "2017.01_recordTable.csv")

### Data summarising using camtrapR record tables (rec.spec), graphing results
### mostly Cole's code

## No. of detections by species and station
sp_detect <- det_data$Species
st_detect <- det_data$Station

### Frequency histograms

par(mfrow = c(1,1))## Multiple plots on same page (2 rows, 1 column)

sp.plot <- rev(sort(table(sp_detect))) 
xvals <- barplot(sp.plot,names.arg = NA,col="royalblue4",ylab = "Camera detections",cex.lab=1.5,ylim=c(0,350))
text(xvals,par("usr")[3]-0.25,srt=45,adj=1.2,labels=names(sp.plot),xpd=TRUE)

## Remove humans, mustelid spp and other birds (in the most roundabout way ever...)
ani.rec <- det_data[!det_data$Species == "Homo sapiens", ]
ani.rec <- ani.rec[!ani.rec$Species == "Unknown species", ] ## Omits unknowns
 ## Gather all birds except sandhill crane into 1 category "Bird spp."
ani.rec$Species <- gsub("Perisoreus canadensis", "Bird spp.", ani.rec$Species)
ani.rec$Species <- gsub("Colaptes auratus", "Bird spp.", ani.rec$Species)
ani.rec$Species <- gsub("Tympanuchus phasianellus", "Bird spp.", ani.rec$Species)
ani.rec$Species <- gsub("Strix nebulosa", "Bird spp.", ani.rec$Species)
ani.rec$Species <- gsub("Canachites canadensis", "Bird spp.", ani.rec$Species)
ani.rec$Species <- gsub("Branta canadensis", "Bird spp.", ani.rec$Species)

unique(ani.rec$Species)




sp_detect <- ani.rec$Species
st_detect <- ani.rec$Station

sp.plot1 <- rev(sort(table(sp_detect)))
sp.plot1 <- as.data.frame(sp.plot1) ##data frame summing detections --> fix scientific names to common
fix(sp.plot1)
colnames(sp.plot1) <- c("Species", "Total Detections")
write.csv(sp.plot1, "SpDetectionSummary_nov2017.csv")


### Frequency histograms

par(mfrow = c(1,1))## Multiple plots on same page (2 rows, 1 column)

ggplot(data = sp.plot1, aes(x = sp_detect, y = Freq)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Total Detections") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("Sandhill crane", "Black bear", "White-tailed deer", "Grey wolf", "Snowshoe hare", "Bird spp.", "Woodland caribou", "Moose", "Coyote", "Canada lynx", "Red squirrel", "Red fox", "American marten", "River otter", "Beaver"))

###########Do same for 2015.01 deployment, i.e. removing humans and other birds#######################
no.hum <- rec.spec[!rec.spec$Species == "H_sapiens", ]
ani.2015 <- no.hum[!no.hum$Species == "Other_birds", ]

sp_detect <- ani.2015$Species
st_detect <- ani.2015$Station

### Frequency histograms

par(mfrow = c(1,1))## Multiple plots on same page (2 rows, 1 column)

sp.plot <- rev(sort(table(sp_detect))) 
xvals <- barplot(sp.plot,names.arg = NA,col="royalblue4",ylab = "Camera detections",cex.lab=1.5,ylim=c(0,250))
text(xvals,par("usr")[3]-0.25,srt=45,adj=1.2,labels=names(sp.plot),xpd=TRUE)
#### Species detection graph including all deployments ####
## Uses full record table, recordTable_nov2015-nov2017.csv, currently read in as All.rec

## Add Treatments to All.rec
StatData <- read.csv("Station_data/AlgarStations_DeploymentData.csv")
colnames(StatData)
colnames(All.rec)
All.rec$Treatment <- StatData$Treatment[match(All.rec$Station,StatData$CamStation)]
colnames(All.rec)


sp_detect <- All.rec$Species

sp.plot1 <- rev(sort(table(sp_detect)))
sp.plot1 <- as.data.frame(sp.plot1) ##data frame summing detections --> fix scientific names to common
fix(sp.plot1)

write.csv(sp.plot1, "SpDetectionSummary_nov2015-nov2017.csv")



### Frequency histograms

par(mfrow = c(1,1))## Multiple plots on same page (2 rows, 1 column)

ggplot(data = sp.plot1, aes(x = sp_detect, y = Freq)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Total Detections") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("White-tailed deer", "Sandhill crane", "Black bear", "Grey wolf", "Snowshoe hare", "Bird spp.", "Woodland caribou", "Human", "Canada lynx", "Red squirrel", "Red fox", "American marten", "Cougar", "Fisher", "Wolverine", "River otter", "Beaver"))


#### create Site x Species matrix ####
All.rec <- read.csv("recordTable_nov2015-nov2017.csv") #Data already cleaned, just loading in
sp_detect <- All.rec$Species
 # All.rec$Station <- toupper(All.rec$Station) ### Need to do to match CamStations in cam2016
st_detect <- All.rec$Station ##Will need to add row for Algar32 at some point (no detections)
S <- as.data.frame.matrix(table(st_detect,sp_detect))

# species totals to compare with table(sp_detect)
apply(S,2,sum)
# number of sites per species
sp.sites <- apply(S,2,function(x) sum(ifelse(x>0,1,0)))
sp.plot2 <- as.data.frame(rev(sort(sp.sites)))
colnames(sp.plot2) <- c("NumSites")
fix(sp.plot2) #Changing scientific names to common

# xvals <- barplot(sp.plot2,names.arg = NA,col="lightblue",ylab = "Number of sites",cex.lab=1.5, ylim=c(0,70))
# text(xvals,par("usr")[3]-0.25,srt=45,adj=1,labels=names(sp.plot2),xpd=TRUE)

## ggplot way (using dataframe)
ggplot(data = sp.plot2, aes(x = row.names(sp.plot2), y = NumSites))+ geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Number of Sites") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("Black bear", "Grey wolf", "Sandhill crane", "Bird spp.", "Moose", "White-tailed deer", "Woodland caribou", "Canada lynx", "Snowshoe hare", "Coyote", "Human", "Red fox", "Red squirrel", "American marten", "Cougar", "Fisher","River otter", "Wolverine", "Beaver"))



# add total count and total species (richness) for each site
S$Total <- apply(S,1,sum)

S$Richness <- apply(S[,1:19],1,function(x) sum(ifelse(x>0,1,0)))
## Edit to Cole's code: S$Richness included total in Richness count. Adding [,1:19] to specify counting only species 
## columns

# add coordinates and treatment to dataframe
S$utmE <- camdata$utmE[match(row.names(S),camdata$CamStation)]
S$utmN <- camdata$utmN[match(row.names(S),camdata$CamStation)]

S$Treatment <- camdata$Treatment[match(row.names(S),camdata$CamStation)]
str(S)
unique(S$Treatment)
fix(S) #Species columns need to be one word

## Add row for station with no detections (Algar32)
#### Don't actually need to do this: Algar32 was inactive, sostation cannot contribute data regardless ####

# create a one-row matrix the same length as data

temprow <- matrix(c(rep.int(NA,length(S))),nrow=1,ncol=length(S))

# make it a data.frame and give cols the same names as data

newrow <- data.frame(temprow)
colnames(newrow) <- colnames(S)

# rbind the empty row to data

S <- rbind(S,newrow)
## Fill in new rows with Station data for Algar32
fix(S)

#######################################################################

# setwd(images_wd)
write.csv(S, "detectionsByStation.csv")


#### NOTE: Graphs done in ggplot in script 'Algar_report(Erin).R' ####
# plot spatial variation by station [note: need to specify species columns now that other variables added]
with(S, symbols(x=utmE, y=utmN, circles=Total, inches=2/3, bg="royalblue3", fg="darkblue", 
                main = "Total animal detections by camera station"))

with(S, symbols(x=utmE, y=utmN, circles=Richness, inches=1/3, bg="royalblue3", fg="darkblue", 
                main = "Species richness by camera station"))

# look at detections relative to treatment [quick-and-dirty for exploration, not really appropriate tests]
# total detections
boxplot(S$Total~S$Treatment,cex.axis=1.3,ylab="Total detections",cex.lab=1.6,col=c("royalblue","orange","green", "red"),boxwex=0.6)
tapply(S$Total,S$Treatment,mean)
tapply(S$Total,S$Treatment,sd)
t.test(S$Total~S$Treatment)

# richness
boxplot(S$Richness~S$Treatment,cex.axis=1.3,ylab="Species Richness",cex.lab=1.6,col=c("royalblue","orange","green", "red"),boxwex=0.6)
tapply(S$Richness,S$Treatment,mean)
tapply(S$Richness,S$Treatment,sd)
t.test(S$Richness~S$Treatment)

# treatment effects by species

# caribou
boxplot(S$R_tarandus~S$Treatment,cex.axis=1.3,ylab="Caribou detections",cex.lab=1.6,col=c("royalblue","orange","green", "red"),boxwex=0.6)
tapply(S$R_tarandus,S$Treatment,mean)
tapply(S$R_tarandus,S$Treatment,sd)
tapply(S$R_tarandus,S$Treatment,median)
t.test(S$R_tarandus~S$Treatment)

# wolf
boxplot(S$C_lupus~S$Treatment,cex.axis=1.3,ylab="Wolf detections",cex.lab=1.6,col=c("royalblue","orange","green", "red"),boxwex=0.6)
tapply(S$C_lupus,S$Treatment,mean)
tapply(S$C_lupus,S$Treatment,sd)
t.test(S$C_lupus~S$Treatment)
tapply(S$C_lupus,S$Treatment,median)

# bear
boxplot(S$U_americanus~S$Treatment,cex.axis=1.3,ylab="Black bear detections",cex.lab=1.6,col=c("orange","purple"),boxwex=0.6)
tapply(S$U_americanus,S$Treatment,mean)
tapply(S$U_americanus,S$Treatment,sd)
tapply(S$U_americanus,S$Treatment,median)
t.test(S$U_americanus~S$Treatment)

# deer
boxplot(S$O_virginianus~S$Treatment,cex.axis=1.3,ylab="White-tailed deer detections",cex.lab=1.6,
        col=c("royalblue","orange","green", "red"),boxwex=0.6)
tapply(S$O_virginianus,S$Treatment,mean)
tapply(S$O_virginianus,S$Treatment,sd)
tapply(S$O_virginianus,S$Treatment,median)
t.test(S$O_virginianus~S$Treatment)

# moose
boxplot(S$A_alces~S$Treatment,cex.axis=1.3,ylab="Moose detections",cex.lab=1.6,col=c("royalblue","orange","green", "red"),boxwex=0.6)
tapply(S$A_alces,S$Treatment,mean)
tapply(S$A_alces,S$Treatment,sd)
tapply(S$A_alces,S$Treatment,median)
t.test(S$A_alces~S$Treatment)


### Quick spatial variation by station in species
par(mfrow = c(1,2))## Multiple plots on same page (1 row, 2 columns)
par(mfrow = c(1,1))
# Caribou
with(S, symbols(x=utmE, y=utmN, circles=Caribou, inches=2/3, bg="royalblue3", fg="darkblue", 
                main = "Caribou detections"))
# Wolves
with(S, symbols(x=utmE, y=utmN, circles=Wolf, inches=1/3, bg="royalblue3", fg="darkblue", 
                main = "Total wolf detections by camera station"))

# Black bears
with(S, symbols(x=utmE, y=utmN, circles=Bear, inches=2/3, bg="royalblue3", fg="darkblue", 
                main = "Total black bear detections by camera station"))

# Coyote
with(S, symbols(x=utmE, y=utmN, circles=Coyote, inches=1/3, bg="royalblue3", fg="darkblue", 
                main = "Total coyote detections by camera station"))
# Lynx
with(S, symbols(x=utmE, y=utmN, circles=Lynx, inches=1/3, bg="royalblue3", fg="darkblue", 
                main = "Total lynx detections by camera station"))
# Moose
with(S, symbols(x=utmE, y=utmN, circles=Moose, inches=1/3, bg="royalblue3", fg="darkblue", 
                main = "Total moose detections by camera station"))

# WT deer
with(S, symbols(x=utmE, y=utmN, circles=WTDeer, inches=1/3, bg="royalblue3", fg="darkblue", 
                main = "Total WT deer detections by camera station"))





################## Basic summary stat comparisons
summary(ani.rec$Species)
table(ani.rec$Species)
table(ani.2015$Species)
