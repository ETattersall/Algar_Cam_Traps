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
#### Remember that event =/= individual!! For wolves and bears esp.

## No. of wolf events
## Control lines = 34
## Treat lines   = 74

## No. of bear events
## Control lines = 58
## Treat lines   = 92

## No. of lynx events
## Control lines = 13
## Treat lines   = 18

## No. of coyote events
## Control lines = 10
## Treat lines   = 30

## No. of Caribou events
## Control lines = 8
## Treat lines   = 37