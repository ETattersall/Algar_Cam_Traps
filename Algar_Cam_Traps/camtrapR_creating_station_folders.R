###################################
# camtrapR_creating_station_folders.R
# Using createStationFolders to... create station folders :P
# Started Oct. 2016, Modified April 2018
###################################

library(camtrapR)

## Set WD
setwd("D:/CameraTrap_Images/Algar_Apr-Nov2018")

## List of station names
Stations <- paste("Algar", formatC(1:73, width=2, flag="0"), sep="") #Formats numbers as fixed width of 2

## Create station folders
createStationFolders(inDir = "Raw_Images", 
                     stations = Stations,
                     createinDir = TRUE) #Creates Raw_Images folder




###### Oct 2016 practice code: does not need to be run ####
##Default working directory ##

OriginalWD<-"C:/Users/ETattersall/Documents"

# Working directory for Algar project #
AlgarWD<-"C:/Users/ETattersall/OneDrive/Algar Camera Traps"
setwd("AlgarWD")

#When working on personal computer
AlgarWDhome<-"c:/Users/Erin/OneDrive/Algar Camera Traps"
setwd(AlgarWDhome)



#Creating raw image Station Folders#
AlgarFoldercreate<-createStationFolders("Raw_Images", 
                                        c("Control","Treatment"),
                                        createinDir = TRUE)
AlgarFoldercreate

#But how to make multiple folders at once?!?  Use levels()

#Step 1: Load csv file (abridged CSV only contains critical Station info)
AlgarPilotStations<-read.csv( "AlgarCameraStations_abridged.csv", header = TRUE)

#Step 2: Isolate CamStation names
AlgarCamStations<-AlgarPilotStations$CamStation
AlgarCamStations

#Step 3: Creating Station Folders in directory "Algar Camera Traps/Raw_Images"


#Creating Control and Treatment subdirectories
ControlStations<-AlgarCamStations[-(13:24)]

ControlStations


ControlFoldercreate<-createStationFolders("Raw_Images/Control", 
                                          as.character(factor(ControlStations)),
                                          createinDir = FALSE)
                                          
                                  
ControlFoldercreate


#Separating Treatment from Control
TreatmentStations<-AlgarCamStations[13:24]
TreatmentStations

TreatmentFoldercreate<-createStationFolders("Raw_Images/Treatment", 
                                            as.character(factor(TreatmentStations)),
                                            createinDir=TRUE)
#factor() function only recalls levels that appear in subset


#Need folder for Timelapse files when working in camtrapR

XtraTimelapseFoldercreate<-createStationFolders("Xtra Timelapse Files", 
                                                c("Control","Treatment"),
                                                createinDir = TRUE)

ControlFoldercreate_Xtra<-createStationFolders("Xtra Timelapse Files/Control", 
                                               as.character(factor(ControlStations)),
                                               createinDir = FALSE)

TreatmentFoldercreate_Xtra<-createStationFolders("Xtra Timelapse Files/Treatment", 
                                                 as.character(factor(TreatmentStations)),
                                                 createinDir=TRUE)

#For vignettes
vignette("camtrapR")
vignette

### Creating station folders for shared Sync folder
setwd(images_wd)
cams2016 <- read.csv("AlgarCameraStations_Nov2016.csv", header = T)
setwd("C:/Users/ETattersall/My Documents/Sync/Algar")
cams2016.01 <- cams2016$CamStation
createStationFolders(inDir       = "2016.01", 
                     stations    = as.character(cams2016.01),
                     createinDir = FALSE)
########


