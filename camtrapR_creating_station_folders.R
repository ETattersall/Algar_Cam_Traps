# Working in camtrapR #
library(camtrapR)

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




