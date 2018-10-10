################
## camtrapR_activityPatterns.R

library(camtrapR)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

Rec <- read.csv("AlgarRecordTable_nov2015-apr2018.csv")
str(Rec)

bear.act <- activityDensity(recordTable = Rec,
                            species = "Ursus americanus",
                            speciesCol = "Species",
                            recordDateTimeCol = "DateTimeOriginal",
                            recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                            plotR= TRUE)
## largely diurnal, with low activity between midnight and 5am (confounded by seasonal effects? Least of all spp. probably because of hibernation)

lynx.act <- activityDensity(recordTable = Rec,
                            species = "Lynx canadensis",
                            speciesCol = "Species",
                            recordDateTimeCol = "DateTimeOriginal",
                            recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                            plotR= TRUE)
Coyote.act <-  activityDensity(recordTable = Rec,
                               species = "Canis latrans",
                               speciesCol = "Species",
                               recordDateTimeCol = "DateTimeOriginal",
                               recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                               plotR= TRUE)

Wolf.act <- activityDensity(recordTable = Rec,
                species = "Canis lupus",
                speciesCol = "Species",
                recordDateTimeCol = "DateTimeOriginal",
                recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                plotR= TRUE)
