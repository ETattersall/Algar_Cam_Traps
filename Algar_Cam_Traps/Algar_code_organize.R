###########################
# .RData Workspace in Algar_git
# Started by Erin T., Feb 7, 2017...June 2, 2017 modified for 2016.01 deployment
# Code organization for Algar Camera Trap Data
############################

## Station Folders created (createStationFolders), Images already renamed (see camtrapR_imageRename.R)
library(camtrapR)
library(tidyr)
library(dplyr)


####### Cole's code
### Load camera station data for the 24 cameras set in November 2015, checked in November 2016
images_wd <- "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps"
setwd(images_wd)
cams2016 <- read.csv("Algar_stationdata.2016.01.csv")
with(cams2016,plot(utmE,utmN))

# specify folder with renamed images
imgDir <- "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images" 

# check number of JPG images [this stuff isn't needed now that we have tagged images]
length(list.files(imgDir, pattern = "JPG", recursive = TRUE)) # [1] 30632 --> includes stations with misfiring cameras

# try this for each station (Image count per Station)
stations <- as.character(cams2016$CamStation)

stationDir <- paste("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Renamed_Images",stations,sep="/")

stn.img <- rep(NA,length(stations))
for (i in 1:length(stations)) {
  stn.img[i] <- length(list.files(stationDir[i], pattern = "JPG"))
}
sum(stn.img)

# add to camera station table
cams2016$img.cnt <- stn.img

# tally survey effort
### cams2016 is not updated CSV with retrieval info: RESOLVED (June 2), continued June 5

camEff <- cameraOperation(cams2016, 
                          stationCol = "CamStation", 
                          setupCol = "CheckDate1", 
                          retrievalCol = "CheckDate2",
                          hasProblems = FALSE,
                          dateFormat = "%d/%m/%Y", 
                          writecsv = FALSE)

# total camera days
sum(camEff,na.rm=T) #9612 --> Inaccurate because of malfunctioning cameras. Need to add "Problems" to CTtable to account for issues with 4 cameras

## Added columns "Problem1_from" and "Problem1_to" to CTtable ('Algar_stationdata.2016.01.csv')

camEff <- cameraOperation(cams2016, 
                          stationCol = "CamStation", 
                          setupCol = "CheckDate1", 
                          retrievalCol = "CheckDate2",
                          hasProblems = TRUE,
                          dateFormat = "%d/%m/%Y", 
                          writecsv = FALSE)

sum(camEff,na.rm=T) ## 9025

# days per station
summary(rowSums(camEff,na.rm=T)) # median = 160, range = 4-162

# add to camera station table
cams2016$trapdays <- rowSums(camEff,na.rm=T)

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



###Code works for looping all species in one folder, for now just repeat this 24 times (Can select all and run at once)
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




write.csv(rec.spec, "2016.01_recordTable.csv")

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


wolf.tab <- recordTableIndividual(inDir = "Species_org/C_lupus",
                                  hasStationFolders = TRUE,
                                  IDfrom = "directory",
                                  minDeltaTime = 30,
                                  deltaTimeComparedTo = "lastRecord",
                                  timeZone = "Canada/Mountain",
                                  writecsv = FALSE,
                                  additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))

coyote.tab <- recordTableIndividual(inDir = "Species_org/C_latrans",
                                    hasStationFolders = TRUE,
                                    IDfrom = "directory",
                                    minDeltaTime = 30,
                                    deltaTimeComparedTo = "lastRecord",
                                    timeZone = "Canada/Mountain",
                                    writecsv = FALSE,
                                    additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))

lynx.tab <- recordTableIndividual(inDir = "Species_org/L_canadensis",
                                  hasStationFolders = TRUE,
                                  IDfrom = "directory",
                                  minDeltaTime = 30,
                                  deltaTimeComparedTo = "lastRecord",
                                  timeZone = "Canada/Mountain",
                                  writecsv = FALSE,
                                  additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))

caribou.tab <- recordTableIndividual(inDir = "Species_org/R_tarandus",
                                     hasStationFolders = TRUE,
                                     IDfrom = "directory",
                                     minDeltaTime = 30,
                                     deltaTimeComparedTo = "lastRecord",
                                     timeZone = "Canada/Mountain",
                                     writecsv = FALSE,
                                     additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))

WTdeer.tab <- recordTableIndividual(inDir = "Species_org/O_virginianus",
                                    hasStationFolders = TRUE,
                                    IDfrom = "directory",
                                    minDeltaTime = 30,
                                    deltaTimeComparedTo = "lastRecord",
                                    timeZone = "Canada/Mountain",
                                    writecsv = FALSE,
                                    additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))

moose.tab <- recordTableIndividual(inDir = "Species_org/A_alces",
                                   hasStationFolders = TRUE,
                                   IDfrom = "directory",
                                   minDeltaTime = 30,
                                   deltaTimeComparedTo = "lastRecord",
                                   timeZone = "Canada/Mountain",
                                   writecsv = FALSE,
                                   additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))


### Data summarising using camtrapR record tables (rec.spec), graphing results
### mostly Cole's code

## No. of detections by species and station
sp_detect <- rec.2016.01$Species
st_detect <- rec.2016.01$Station

### Frequency histograms

par(mfrow = c(1,1))## Multiple plots on same page (2 rows, 1 column)

sp.plot <- rev(sort(table(sp_detect))) 
xvals <- barplot(sp.plot,names.arg = NA,col="royalblue4",ylab = "Camera detections",cex.lab=1.5,ylim=c(0,100))
text(xvals,par("usr")[3]-0.25,srt=45,adj=1.2,labels=names(sp.plot),xpd=TRUE)

## Remove humans, mustelid spp and other birds (in the most roundabout way ever...)
no.hum <- rec.2016.01[!rec.2016.01$Species == "H_sapiens", ]
no.oth <- no.hum[!no.hum$Species == "Mustelid spp", ]
ani.rec <- no.mus[!no.mus$Species == "Other_birds", ]

sp_detect <- ani.rec$Species
st_detect <- ani.rec$Station

### Frequency histograms

par(mfrow = c(1,1))## Multiple plots on same page (2 rows, 1 column)

sp.plot <- rev(sort(table(sp_detect))) 
xvals <- barplot(sp.plot,names.arg = NA,col="royalblue4",ylab = "Camera detections",cex.lab=1.5,ylim=c(0,100))
text(xvals,par("usr")[3]-0.25,srt=45,adj=1.2,labels=names(sp.plot),xpd=TRUE)

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
###################################################################################

# create Site x Species matrix
sp_detect <- ani.rec$Species
ani.rec$Station <- toupper(ani.rec$Station) ### Need to do to match CamStations in cam2016
st_detect <- ani.rec$Station
S <- as.data.frame.matrix(table(st_detect,sp_detect))

# species totals to compare with table(sp_detect)
apply(S,2,sum)
# number of sites per species
sp.sites <- apply(S,2,function(x) sum(ifelse(x>0,1,0)))
sp.plot2 <- rev(sort(sp.sites)) 
xvals <- barplot(sp.plot2,names.arg = NA,col="royalblue4",ylab = "Number of sites",cex.lab=1.5, ylim=c(0,30))
text(xvals,par("usr")[3]-0.25,srt=45,adj=1,labels=names(sp.plot2),xpd=TRUE)

# add total count and total species (richness) for each site
S$Total <- apply(S,1,sum)

S$Richness <- apply(S[,1:14],1,function(x) sum(ifelse(x>0,1,0)))
## Edit to Cole's code: S$Richness included total in Richness count. Adding [,1:17] to specify counting only species 
## columns

# add coordinates and treatment to dataframe
S$utmE <- cams2016$utmE[match(row.names(S),cams2016$CamStation)]
S$utmN <- cams2016$utmN[match(row.names(S),cams2016$CamStation)]

S$Treatment <- cams2016$TreatmentType[match(row.names(S),cams2016$CamStation)]
setwd(images_wd)
write.csv(S, "2016.01/detectionsByStation.csv")

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
with(S, symbols(x=utmE, y=utmN, circles=R_tarandus, inches=2/3, bg="royalblue3", fg="darkblue", 
                main = "Total caribou detections by camera station"))
# Wolves
with(S, symbols(x=utmE, y=utmN, circles=C_lupus, inches=1/3, bg="royalblue3", fg="darkblue", 
                main = "Total wolf detections by camera station"))

# Black bears
with(S, symbols(x=utmE, y=utmN, circles=U_americanus, inches=2/3, bg="royalblue3", fg="darkblue", 
                main = "Total black bear detections by camera station"))

# Coyote
with(S, symbols(x=utmE, y=utmN, circles=C_latrans, inches=1/3, bg="royalblue3", fg="darkblue", 
                main = "Total coyote detections by camera station"))
# Lynx
with(S, symbols(x=utmE, y=utmN, circles=L_canadensis, inches=1/3, bg="royalblue3", fg="darkblue", 
                main = "Total lynx detections by camera station"))
# Moose
with(S, symbols(x=utmE, y=utmN, circles=A_alces, inches=1/3, bg="royalblue3", fg="darkblue", 
                main = "Total moose detections by camera station"))

# WT deer
with(S, symbols(x=utmE, y=utmN, circles=O_virginianus, inches=1/3, bg="royalblue3", fg="darkblue", 
                main = "Total WT deer detections by camera station"))





################## Basic summary stat comparisons
summary(ani.rec$Species)
table(ani.rec$Species)
table(ani.2015$Species)
