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
                        deltaTimeComparedTo    = "lastRecord",
                        timeZone               = "Canada/Mountain",
                        metadataSpeciesTag     = "TriggerMode")

### Each event = 30 minutes after last record. MAKE recordTable with 
## deltaTimeComparedTo = lastIndependentRecord (30 minutes after last record of same species)

rec.spec.ind <- recordTable(inDir                  = species_wd,
                        IDfrom                 = "directory",
                        minDeltaTime           = 30,
                        deltaTimeComparedTo    = "lastIndependentRecord",
                        timeZone               = "Canada/Mountain",
                        metadataSpeciesTag     = "TriggerMode")
                        

### Difference between rec.spec and rec.spec.ind MIGHT be due to an event being interrupted by a timelapse image triggered at noon rather than actually depicting individual events
### Would be good to filter out timelapse images before building record tables?

rec.noTimelapse <- recordTable(inDir                  = species_wd,
                               IDfrom                 = "directory",
                               minDeltaTime           = 30,
                               deltaTimeComparedTo    = "lastIndependentRecord",
                               timeZone               = "Canada/Mountain",
                               metadataSpeciesTag     = "TriggerMode")



#delta.time indicates time since last occurrance of that species at that site

### NOTE: recordTable extracts 1 file from each independent event. This means that 
### it is used for getSpeciesImages, only 1 file is extracted from each event (1st ### file). Only a problem when multiple individuals are in one event (wolf pack)

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
