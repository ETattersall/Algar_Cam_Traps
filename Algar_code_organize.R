###########################
# .RData Workspace in Algar_git
# Started by Erin T., Feb 7, 2017
# Code organization for Algar Camera Trap Data
############################

## Station Folders created (createStationFolders), Images already renamed (see camtrapR_imageRename.R)
library(camtrapR)
library(tidyr)
library(dplyr)


####### Cole's code
### Load camera station data for the 24 cameras set in November 2015, checked in November 2016
setwd(images_wd)
cams2015 <- read.csv("Algar2015StationData.csv")
with(cams2015,plot(utmE,utmN))

# specify folder with renamed images
imgDir <- "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images" 

# check number of JPG images [this stuff isn't needed now that we have tagged images]
length(list.files(imgDir, pattern = "JPG", recursive = TRUE)) # [1] 18036

# try this for each station (Image count per Station)
stations <- as.character(cams2015$CamStation)
stationDir <- rep(NA,length(stations))
stationDir <- paste("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Raw_images",stations,sep="/")

stn.img <- rep(NA,length(stations))
for (i in 1:length(stations)) {
  stn.img[i] <- length(list.files(stationDir[i], pattern = "JPG"))
}
sum(stn.img)

# add to camera station table
cams2015$img.cnt <- stn.img

# tally survey effort

camEff <- cameraOperation(cams2015, 
                          stationCol = "CamStation", 
                          setupCol = "DeployDate", 
                          retrievalCol = "CheckDate1", 
                          hasProblems = FALSE,
                          dateFormat = "%d/%m/%Y", 
                          writecsv = FALSE)

# total camera days
sum(camEff,na.rm=T) # 8911

# days per station
summary(rowSums(camEff,na.rm=T)) # median = 371, range = 370-373

# add to camera station table
cams2015$trapdays <- rowSums(camEff,na.rm=T)

# subtract days from total images for estimate of motion triggers [not needed, see image data below]
cams2015$EstTrig <- cams2015$img.cnt - cams2015$trapdays

# quick-and-dirty plot of number of motion triggers by treatment 
boxplot(cams2015$EstTrig ~ cams2015$Treatment,col=c("orange","purple"),ylab="Number of Motion Triggers",
        cex.lab=1.5,cex.axis=1.5)
##Above plots each image

################################

##### Erin's code
### Merging csvs
library(plyr)
setwd("ColeTimelapseExports12Dec2016")

### rbind.fill (in plyr) merges csvs and fills missing columns with NA
filenames <- list.files()
Alg_master <- do.call("rbind.fill", lapply(filenames, read.csv, header = TRUE))

### rbind.fill (in plyr) merges csvs and fills missing columns with NA


write.csv(Alg_master, "Master_csv_Cole.csv")

#### Cole's code
# check for NA's in counts (shouldn't be any)
apply(apply(Alg_master[,14:31],2,is.na),2,sum)

# need to fix NA values in species counts ... [missing values in csv files]

# how many photos are of animals 
table(Alg_master$Animal)
table(Alg_master$Trigger_Mode) 


# subset data to only animal photos that are not classified as unknown
## Problem: only subsets for images with Animal == TRUE and not Animal == "true" So lots of images are lost
## Make all lowercase letters uppercase

Alg_master <- data.frame(lapply(Alg_master, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
})) 


A2 <- subset(Alg_master, Animal == TRUE & Unknown == FALSE)
A2$X <- NULL

# make a count variable for each detection
A2$count <- apply(A2[,14:31],1,max, na.rm=T)

# there are still a few zero counts to fix (should go back in csv files eventually ...)
A2[which(A2$count==0),]

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

# some summaries
nrow(A2)

table(A2$Species)

# check the Other class
A2$Other_specify[which(A2$Species=="Other")]
A2[which(A2$Species=="Other"),]
A2[which(A2$Species=="Other"),c(32,39)]
## 2 cougar events, 2 ermine, 1 unknown weasel species

table(A2$Folder)
# Add Treatment to detections 

A2$Treatment <- cams2015$Treatment[match(A2$Folder,cams2015$CamStation)]

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

species <- c("O_virginianus","R_tarandus", "C_elavus", "A_alces", "C_lupus", "C_latrans", "U_americanus", "L_canadensis", "G_gulo", "M_americana", "M_pennanti", "V_vulpes", "T_hudsonicus", "L_americanus", "H_sapiens", "G_canadensis", "Other_birds", "Other")


### Duplicate Raw_images folder and rename it "Species_images" (Did this manually...)
### In future, probably makes sense to remain in Raw_images and have backup raw images elsewhere


setwd(images_wd)

### Create species folders in duplicated folder

SpecFolderCreate1 <- createSpeciesFolders (inDir               = "Species_images",
                                           species             = species,
                                           hasCameraFolders = FALSE,
                                           removeFolders       = FALSE)



###Code works for looping all species in one folder, for now just repeat this 24 times
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar1")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar1") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar2")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar2") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar3")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar3") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar4")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar4") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar5")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar5") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar6")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar6") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar7")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar7") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar8")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar8") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar9")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar9") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar10")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar10") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar11")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar11") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar12")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar12") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar13")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar13") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar14")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar14") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar15")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar15") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar16")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar16") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar17")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar17") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar18")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar18") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar19")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar19") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar20")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar20") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar21")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar21") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar22")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar22") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar23")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar23") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images/Algar24")
for (sp in species) {
  sp_x_fol <- A2 %>% filter(Species == sp & Folder == "Algar24") %>% 
    select(File)
  vec_sp_fol <- unlist(sp_x_fol)
  copySpeciesFiles <- file.copy(vec_sp_fol, sp)
}

setwd(images_wd)

