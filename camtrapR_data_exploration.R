#### R script for data exploration
### Started Dec. 15, 2016 by Erin Tattersall

library(camtrapR)

###Need record tables created in camtrapR_data_extraction

##Activity histograms
Wolf_hist <- activityHistogram (recordTable = rec.spec.ind,
                   species = "C_lupus",
                   allSpecies = FALSE,
                   speciesCol = "Species",
                   recordDateTimeCol = "DateTimeOriginal",
                   recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                   plotR = TRUE,
                   writePNG = FALSE)

Lynx_hist <- activityHistogram (recordTable = rec.spec.ind,
                                species = "L_canadensis",
                                allSpecies = FALSE,
                                speciesCol = "Species",
                                recordDateTimeCol = "DateTimeOriginal",
                                recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                plotR = TRUE,
                                writePNG = FALSE)

Caribou_hist <- activityHistogram (recordTable = rec.spec.ind,
                                species = "R_tarandus",
                                allSpecies = FALSE,
                                speciesCol = "Species",
                                recordDateTimeCol = "DateTimeOriginal",
                                recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                plotR = TRUE,
                                writePNG = FALSE)
### activityDensity over 24 hr period --> not super useful?

caribou.dens <- activityDensity(rec.spec.ind, 
                "R_tarandus",
                allSpecies = FALSE,
                speciesCol = "Species",
                recordDateTimeCol = "DateTimeOriginal",
                recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                plotR = TRUE, 
                writePNG = FALSE, 
                plotDirectory, 
                createDir = FALSE, 
                pngMaxPix = 1000,
                add.rug = TRUE)