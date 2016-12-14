###### Image data extraction

###1. recordTable --> need it to run a number of different functions
### generates a dataframe of events


exiftool_dir<-"C:/Users/ETattersall/Desktop"
exiftoolPath(exiftoolDir = exiftool_dir) 
grepl(exiftool_dir, Sys.getenv("PATH"))

##If you want to add other metadata to record table, 
##you can view metadata with exifTagNames, choose what to include

exifTagNames(inDir = Renamed_Dir,
             returnMetadata = TRUE)



rec.test <- recordTable(inDir                  = Renamed_Dir,
                        IDfrom                 = "directory",
                        minDeltaTime           = 40,
                        deltaTimeComparedTo    = "lastRecord",
                        writecsv               = FALSE,
                        additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))
View(rec.test)

#delta.time indicates time since last occurrance of that species at that site

###2. recordTableIndividual--> record table for one species

rec.test.Ind <- recordTableIndividual(inDir = "Test images/Renamed_Test/Site 3/Black Bear",
                                      hasStationFolders = FALSE,
                                      IDfrom = "directory",
                                      camerasIndependent = TRUE,
                                      minDeltaTime = 20,
                                      deltaTimeComparedTo = "lastRecord",
                                      writecsv = FALSE,
                                      additionalMetadataTags = c("AmbientTemperature", "MoonPhase"))

###Notes: For Algar data, may want writecsv = TRUE
### Will have all species images across stations in a folder, so hasStationFolders = TRUE