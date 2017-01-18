#### Target predator data exploration #####
#### Created 17 January, 2017 by Erin Tattersall



library(camtrapR)
library(dplyr)
library(tidyr)

### Uses rec.spec (record table of all species)
### SEE NOTE from camtrapR_data_extraction.R re:recordTable


##Activity histograms
Wolf_hist <- activityHistogram (recordTable = rec.spec,
                                species = "C_lupus",
                                allSpecies = FALSE,
                                speciesCol = "Species",
                                recordDateTimeCol = "DateTimeOriginal",
                                recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                plotR = TRUE,
                                writePNG = FALSE)

Lynx_hist <- activityHistogram (recordTable = rec.spec,
                                species = "L_canadensis",
                                allSpecies = FALSE,
                                speciesCol = "Species",
                                recordDateTimeCol = "DateTimeOriginal",
                                recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                plotR = TRUE,
                                writePNG = FALSE)

Caribou_hist <- activityHistogram (recordTable = rec.spec,
                                   species = "R_tarandus",
                                   allSpecies = FALSE,
                                   speciesCol = "Species",
                                   recordDateTimeCol = "DateTimeOriginal",
                                   recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                   plotR = TRUE,
                                   writePNG = FALSE)
### activityDensity over 24 hr period --> not super useful?

caribou.dens <- activityDensity(rec.spec, 
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


##### Differences between Control (Algar 1-12) and Treatment (Algar 13-24)???

