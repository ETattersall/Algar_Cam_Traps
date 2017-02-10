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



###Code works for looping all species in one folder, for now just repeat this 24 times (Can select all and run at once)
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
species_wd <- "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Species_images"
exifTagNames(inDir = species_wd, whichSubDir = 1,
             returnMetadata = TRUE)




rec.spec <- recordTable(inDir                  = species_wd,
                            IDfrom                 = "directory",
                            minDeltaTime           = 30,
                            deltaTimeComparedTo    = "lastIndependentRecord",
                            timeZone               = "Canada/Mountain",
                            metadataSpeciesTag     = "TriggerMode")


### Each event = 30 minutes after last record. MAKE recordTable with 
## deltaTimeComparedTo = lastIndependentRecord (30 minutes after last record of same species)

#delta.time indicates time since last occurrance of that species at that site

### NOTE: recordTable extracts 1 file from each independent event. This means that 
### it is used for getSpeciesImages, only 1 file is extracted from each event (1st ### file). Only a problem when multiple individuals are in one event (wolf pack)

###2. recordTableIndividual--> record table for one species (both getSpeciesImages and recordTableIndividual only take one species at a time)


####Folder of images organized by species
setwd(species_wd)
image_identify <- unique(rec.spec$Species)

for (id in image_identify) {
  getSpeciesImages(species                 = id,
                   recordTable             = rec.spec,
                   speciesCol              = "Species",
                   stationCol              = "Station",
                   outDir                  = "Species_org",
                   createStationSubfolders = TRUE)
}



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
sp_detect <- rec.spec$Species
st_detect <- rec.spec$Station

### Frequency histograms

sp.plot <- rev(sort(table(sp_detect))) 
xvals <- barplot(sp.plot,names.arg = NA,col="royalblue4",ylab = "Camera detections",cex.lab=1.4,ylim=c(0,250))
text(xvals,par("usr")[3]-0.25,srt=45,adj=1,labels=names(sp.plot),xpd=TRUE)

# create Site x Species matrix
S <- as.data.frame.matrix(table(st_detect,sp_detect))

# species totals to compare with table(sp_detect)
apply(S,2,sum)
# number of sites per species
sp.sites <- apply(S,2,function(x) sum(ifelse(x>0,1,0)))
sp.plot2 <- rev(sort(sp.sites)) 
xvals <- barplot(sp.plot2,names.arg = NA,col="royalblue4",ylab = "Number of sites",cex.lab=1.4)
text(xvals,par("usr")[3]-0.25,srt=45,adj=1,labels=names(sp.plot2),xpd=TRUE)

# add total count and total species (richness) for each site
S$Total <- apply(S,1,sum)

S$Richness <- apply(S[,1:17],1,function(x) sum(ifelse(x>0,1,0)))
## Edit to Cole's code: S$Richness included total in Richness count. Adding [,1:17] to specify counting only species 
## columns

# add coordinates and treatment to dataframe
S$utmE <- cams2015$utmE[match(row.names(S),cams2015$CamStation)]
S$utmN <- cams2015$utmN[match(row.names(S),cams2015$CamStation)]

S$Treatment <- cams2015$Treatment[match(row.names(S),cams2015$CamStation)]

# look at detections relative to treatment [quick-and-dirty for exploration, not really appropriate tests]
# total detections
boxplot(S$Total~S$Treatment,cex.axis=1.3,ylab="Total detections",cex.lab=1.6,col=c("orange","purple"),boxwex=0.6)
tapply(S$Total,S$Treatment,mean)
tapply(S$Total,S$Treatment,sd)
t.test(S$Total~S$Treatment)

# richness
boxplot(S$Richness~S$Treatment,cex.axis=1.3,ylab="Species Richness",cex.lab=1.6,col=c("orange","purple"),boxwex=0.6)
tapply(S$Richness,S$Treatment,mean)
tapply(S$Richness,S$Treatment,sd)
t.test(S$Richness~S$Treatment)

# treatment effects by species

# caribou
boxplot(S$R_tarandus~S$Treatment,cex.axis=1.3,ylab="Caribou detections",cex.lab=1.6,col=c("orange","purple"),boxwex=0.6)
tapply(S$R_tarandus,S$Treatment,mean)
tapply(S$R_tarandus,S$Treatment,sd)
tapply(S$R_tarandus,S$Treatment,median)
t.test(S$R_tarandus~S$Treatment)

# wolf
boxplot(S$C_lupus~S$Treatment,cex.axis=1.3,ylab="Wolf detections",cex.lab=1.6,col=c("orange","purple"),boxwex=0.6)
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
        col=c("orange","purple"),boxwex=0.6)
tapply(S$O_virginianus,S$Treatment,mean)
tapply(S$O_virginianus,S$Treatment,sd)
tapply(S$O_virginianus,S$Treatment,median)
t.test(S$O_virginianus~S$Treatment)

# moose
boxplot(S$A_alces~S$Treatment,cex.axis=1.3,ylab="Moose detections",cex.lab=1.6,col=c("orange","purple"),boxwex=0.6)
tapply(S$A_alces,S$Treatment,mean)
tapply(S$A_alces,S$Treatment,sd)
tapply(S$A_alces,S$Treatment,median)
t.test(S$A_alces~S$Treatment)