##############################
## 01.Co-occurrences_detectionsExplore.R
## Basic exploratory plots for co-occurrence analysis
## Started 24-Aug-2018
##############################

library(camtrapR)
library(reshape2)	# for formatting data frames
# library(plyr) need for renaming Treatments for consistency, but conflicts with dplyr
library(dplyr)		# only load after using revalue function, for applying functions to subsets of data frames
library(ggplot2)	# for data visualization
library(stringr)	# for working with character strings
library(tidyr)		# for data formatting functions




setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")


daily <- read.csv("Algar_DailyDetections_30months.csv") #Algar daily detections 30 months
rec <- read.csv("AlgarRecordTable_nov2015-apr2018.csv")
str(daily)
str(rec)

## Remove offline sites from recordTable
# Offline sites
offline <- rec[which(rec$Station == "Algar61" | rec$Station == "Algar62" | rec$Station == "Algar63" | rec$Station == "Algar64" | rec$Station == "Algar65" | rec$Station == "Algar66" | rec$Station == "Algar67" | rec$Station == "Algar68" | rec$Station == "Algar69" | rec$Station == "Algar70" | rec$Station == "Algar71" | rec$Station == "Algar72"), ]

## Remove offline from record table and daily detections
seismic.rec <- anti_join(rec, offline, by = "FileName")
daily <- daily %>% filter(Treatment != "OffLine")
unique(seismic.rec$Station)
unique(daily$Treatment)
str(rec)

## Convert # detections to occurrences
Occ <- as.data.frame(ifelse(daily[ , 6:15] > 0, 1, 0))
str(Occ)
## Add columns from daily (all non-spp columns)
Occ <- cbind.data.frame(daily[ , c(2:5,16:18)], Occ)

## daily and occ are both measured DAILY. If CamActive = 0, occurrence/detection would be 0. CamActive == 0 needs to be removed
Occ <- Occ %>% filter(CamActive==1)


#### Plotting occurrences by species of interest: wolf, lynx, coyote, black bear
Occ$Date <- as.Date(Occ$Datep) ## For plotting on x-axis


### Using Gillian's code for plotting occurrences

# Wolf
wolf <- ggplot(Occ)

wolf <- wolf + geom_point(aes(x = Date, y = jitter(Wolf), colour = Site), size = 2)

wolf <- wolf + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) # remove background

wolf <- wolf + labs(x = "Date", y = "Probability of wolf occurrence")

## Adding smoother -- estimates probability of occurrence based on model --> can't use yet
# binomial_smooth <- function(...) {
#  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
# } # Use loess smoother for binomial distributions
# wolf <- wolf + binomial_smooth(aes(x = Date, y = fit1, colour = Site), size = 0.5, se = FALSE)

print(wolf)

## Black bear
bear <- ggplot(Occ)

bear <- bear + geom_point(aes(x = Date, y = jitter(Blackbear), colour = Site), size = 2)

bear <- bear + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) # remove background

bear <- bear + labs(x = "Date", y = "Probability of bear occurrence")

print(bear)


## Lynx
Lynx <- ggplot(Occ)

Lynx <- Lynx + geom_point(aes(x = Date, y = jitter(Lynx), colour = Site), size = 2)

Lynx <- Lynx + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) # remove background

Lynx <- Lynx + labs(x = "Date", y = "Probability of Lynx occurrence")

print(Lynx)

## Coyote
Coyote <- ggplot(Occ)

Coyote <- Coyote + geom_point(aes(x = Date, y = jitter(Coyote), colour = Site), size = 2)

Coyote <- Coyote + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) # remove background

Coyote <- Coyote + labs(x = "Date", y = "Probability of Coyote occurrence")

print(Coyote)


## Difficult to determine temporal patterns from this, except for bears

## Spatial separation of occurrences


