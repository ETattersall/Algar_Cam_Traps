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
                        
View(rec.spec)

#delta.time indicates time since last occurrance of that species at that site

###2. recordTableIndividual--> record table for one species (both getSpeciesImages and recordTableIndividual only take one species at a time)


####Folder of images organized by species

specImagecopy <- getSpeciesImages(species                 = "A_alces",
                                  recordTable             = rec.spec,
                                  speciesCol              = "Species",
                                  stationCol              = "Station",
                                  outDir                  = "Species_org",
                                  createStationSubfolders = TRUE)

bear.tab <- recordTableIndividual(inDir = "Species_org/U_americanus",
                                     hasStationFolders = TRUE,
                                     IDfrom = "directory",
                                     minDeltaTime = 30,
                                     deltaTimeComparedTo = "lastRecord",
                                     timeZone = "Canada/Mountain",
                                     writecsv = FALSE,
                                     additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))

