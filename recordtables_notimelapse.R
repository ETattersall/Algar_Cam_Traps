#### Record tables without Timelapse triggered images


library(camtrapR)
library(dplyr)
library(tidyr)

setwd(images_wd)

exiftool_dir<-"C:/Users/ETattersall/Desktop"
exiftoolPath(exiftoolDir = exiftool_dir) 
grepl(exiftool_dir, Sys.getenv("PATH"))


### Identical code to rec.spec.ind, but all timelapse folders have been removed from Species_images folder

rec.noTimelapse <- recordTable(inDir                  = species_wd,
                               IDfrom                 = "directory",
                               minDeltaTime           = 30,
                               deltaTimeComparedTo    = "lastIndependentRecord",
                               timeZone               = "Canada/Mountain",
                               metadataSpeciesTag     = "TriggerMode")

species_NT <- unique(rec.noTimelapse$Species)

for (sp in species_NT) {
  getSpeciesImages(species                 = sp,
                   recordTable             = rec.noTimelapse,
                   speciesCol              = "Species",
                   stationCol              = "Station",
                   outDir                  = "Species_org_02",
                   createStationSubfolders = TRUE)
}


### No difference between rec.spec.ind and rec.noTimelapse --> no need to alter individual record tables