###########################
# .RData Workspace in Algar_git
# Started by Erin T., Feb 7, 2017
# Code organization for Algar Camera Trap Data
############################

## Station Folders created (createStationFolders), Images already renamed (see camtrapR_imageRename.R)
library(camtrapR)

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
