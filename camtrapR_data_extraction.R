###### Image data extraction: recordTables and recordTableIndividual
## Modified by Erin T. May 26, 2017

## Number of total images
length(list.files("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2015.01/Raw_images", pattern = "JPG", recursive = TRUE)) 
## [1] 18036

species_wd <- "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2015.01/Species_images"
## Number of total species images 
length(list.files(species_wd, pattern = "JPG", recursive = TRUE)) ## [1] 5662


###1. recordTable --> need it to run a number of different functions
### generates a dataframe of events


exiftool_dir<-"C:/Users/ETattersall/Desktop"
exiftoolPath(exiftoolDir = exiftool_dir) 
grepl(exiftool_dir, Sys.getenv("PATH"))

##If you want to add other metadata to record table, 
##you can view metadata with exifTagNames, choose what to include

exifTagNames(inDir = species_wd, whichSubDir = 1,
             returnMetadata = TRUE)


### Each event = 30 minutes after last record ended.

rec.spec <- recordTable(inDir                  = species_wd,
                        IDfrom                 = "directory",
                        minDeltaTime           = 30,
                        deltaTimeComparedTo    = "lastRecord",
                        timeZone               = "Canada/Mountain",
                        metadataSpeciesTag     = "TriggerMode")

## rec.spec = 2113 obs. of 11 variables

## MAKE recordTable with 
## deltaTimeComparedTo = lastIndependentRecord (first detection 30 min after last on began?)


rec.spec.ind <- recordTable(inDir                  = species_wd,
                        IDfrom                 = "directory",
                        minDeltaTime           = 30,
                        deltaTimeComparedTo    = "lastIndependentRecord",
                        timeZone               = "Canada/Mountain",
                        metadataSpeciesTag     = "TriggerMode")
## rec.spec.ind = 2121 obs. of 11 variables
                        
### Compare to deltaTime of 60 minutes

rec.spec60 <- recordTable(inDir                  = species_wd,
                        IDfrom                 = "directory",
                        minDeltaTime           = 60,
                        deltaTimeComparedTo    = "lastRecord",
                        timeZone               = "Canada/Mountain",
                        metadataSpeciesTag     = "TriggerMode")
## rec.spec60 = 2056 obs. of 11 variables (57 fewer detections than 30 minutes)

rec.spec.ind60 <- recordTable(inDir                  = species_wd,
                            IDfrom                 = "directory",
                            minDeltaTime           = 60,
                            deltaTimeComparedTo    = "lastIndependentRecord",
                            timeZone               = "Canada/Mountain",
                            metadataSpeciesTag     = "TriggerMode")
## rec.spec.ind60 = 2061 obs. of 11 variables




#delta.time indicates time since last occurrance of that species at that site


###2. recordTableIndividual--> record table for one species (both getSpeciesImages and recordTableIndividual only take one species at a time)


####Folder of images organized by species

specImagecopy <- getSpeciesImages(species                 = "A_alces",
                                  recordTable             = rec.spec,
                                  speciesCol              = "Species",
                                  stationCol              = "Station",
                                  outDir                  = "Species_org",
                                  createStationSubfolders = TRUE)

setwd(images_wd)

bear.tab <- recordTableIndividual(inDir = "Species_org_rec.spec.ind/U_americanus",
                                     hasStationFolders = TRUE,
                                     IDfrom = "directory",
                                     minDeltaTime = 30,
                                     deltaTimeComparedTo = "lastRecord",
                                     timeZone = "Canada/Mountain",
                                     writecsv = FALSE,
                                     additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))

wolf.tab <- recordTableIndividual(inDir = "Species_org_rec.spec.ind/C_lupus",
                                  hasStationFolders = TRUE,
                                  IDfrom = "directory",
                                  minDeltaTime = 30,
                                  deltaTimeComparedTo = "lastRecord",
                                  timeZone = "Canada/Mountain",
                                  writecsv = FALSE,
                                  additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))

coyote.tab <- recordTableIndividual(inDir = "Species_org_rec.spec.ind/C_latrans",
                                  hasStationFolders = TRUE,
                                  IDfrom = "directory",
                                  minDeltaTime = 30,
                                  deltaTimeComparedTo = "lastRecord",
                                  timeZone = "Canada/Mountain",
                                  writecsv = FALSE,
                                  additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))

lynx.tab <- recordTableIndividual(inDir = "Species_org_rec.spec.ind/L_canadensis",
                                    hasStationFolders = TRUE,
                                    IDfrom = "directory",
                                    minDeltaTime = 30,
                                    deltaTimeComparedTo = "lastRecord",
                                    timeZone = "Canada/Mountain",
                                    writecsv = FALSE,
                                    additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))

caribou.tab <- recordTableIndividual(inDir = "Species_org_rec.spec.ind/R_tarandus",
                                    hasStationFolders = TRUE,
                                    IDfrom = "directory",
                                    minDeltaTime = 30,
                                    deltaTimeComparedTo = "lastRecord",
                                    timeZone = "Canada/Mountain",
                                    writecsv = FALSE,
                                    additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))

WTdeer.tab <- recordTableIndividual(inDir = "Species_org_rec.spec.ind/O_virginianus",
                                    hasStationFolders = TRUE,
                                    IDfrom = "directory",
                                    minDeltaTime = 30,
                                    deltaTimeComparedTo = "lastRecord",
                                    timeZone = "Canada/Mountain",
                                    writecsv = FALSE,
                                    additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))

moose.tab <- recordTableIndividual(inDir = "Species_org_rec.spec.ind/A_alces",
                                    hasStationFolders = TRUE,
                                    IDfrom = "directory",
                                    minDeltaTime = 30,
                                    deltaTimeComparedTo = "lastRecord",
                                    timeZone = "Canada/Mountain",
                                    writecsv = FALSE,
                                    additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))


#### Re-organizing individual record tables by date
bear.date <- bear.tab[order(bear.tab$Date),]
wolf.date <- wolf.tab[order(wolf.tab$Date),]
coyote.date <- coyote.tab[order(coyote.tab$Date),]
lynx.date <- lynx.tab[order(lynx.tab$Date),]
caribou.date <- caribou.tab[order(caribou.tab$Date),]
WTdeer.date <- WTdeer.tab[order(WTdeer.tab$Date),]
moose.date <- moose.tab[order(moose.tab$Date),]
