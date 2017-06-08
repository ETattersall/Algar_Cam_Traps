##### Individual record tables ###
### Started 8 June, 2017 by Erin
library(camtrapR)
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2015.01")
image_identify <- unique(rec.spec$Species)


## Need species directory
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

car.tab <- recordTableIndividual(inDir = "Species_org/R_tarandus",
                                  hasStationFolders = TRUE,
                                  IDfrom = "directory",
                                  minDeltaTime = 30,
                                  deltaTimeComparedTo = "lastRecord",
                                  timeZone = "Canada/Mountain",
                                  writecsv = FALSE,
                                  additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))
car.day <- car.tab$Date
car.site <- car.tab$Station
m.car<- as.data.frame.matrix(table(car.site,car.day))

###m.car (and m.bear) = detection matrices of detection days and sites (i.e. excludes sites and days when animals were not detected - need to add these?)