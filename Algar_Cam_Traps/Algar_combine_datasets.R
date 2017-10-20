#################################################
# Combining Algar survey data sets
# Pilot data (Algar01 - Algar 24) with Winter 2016-17 data (Algar01-60)
# Started by Erin T. on Oct. 19, 2017
#################################################

library(dplyr)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

# No. detections/ month data
pilot <- read.csv("2015.01_monthlydetections.csv")
pilot$X <- NULL
win60 <- read.csv("2016.01_monthlydetections.csv")
win60$X <- NULL

# Full dataset will have NAs for the months that Algar25-60 were inactive (Nov. 2015-Oct.2016)
# 12 months, 36 cameras = 432 rows of NAs
# 6 months, 36 cameras = 216
# 18 months, 24 cameras = 432
# Should have 1080 rows of data (inactive and active cameras for entire survey up to April 2017)

#### First 24 cameras only ####
# Algar01 - 24 from 2nd deployment
Alg24 <- win60[1:138, ]

# Combine all 24 cameras for full survey
alldat <- rbind(pilot,Alg24)

## Double counts for Nov. 2016 (counted in both surveys)?
nov2016 <- alldat %>% filter(Yr_Month == "2016-11") ## Yes. Need to be combined

## Use aggregate function. Tested on data frame with only duplicated months
nov2016 <- aggregate(. ~ Site + Treatment + Yr_Month + Site_ym, data = nov2016, sum) 

### Combining data for 24 cams for full survey
alldat <- alldat[!alldat$Yr_Month == "2016-11", ] #Remove duplicated rows

alldat <- rbind(alldat,nov2016) #Combine fixed 2016-11 data to other months
alldat <- alldat[with(alldat, order(Site, Yr_Month)), ] ## Ordering by Site and Yr_month

## Algar18 failed in second deployment. Need to add NA rows?


#### Full data set for all 60 cameras across all survey months ####
## alldat = First 24 cameras for all survey months

win36 <- win60[139:330, ] #Active cams. 192 rows --> Of the 36 additional cameras, 3 failed and 2 had no detections

## Algar52 and Algar56 had no detections. Need to add rows for months they were active (Nov. 2016 - Apr. 2017)
##Dataframe of 12x11
# Site column
Algar52 <- rep("Algar52",6)
Algar56 <- rep("Algar56",6)
Site <- append(Algar52,Algar56)

# Treatment and Yr_Month columns
Treatment <- rep("NatRegen", 12)
Yr_Month <- rep(c("2016-11", "2016-12", "2017-01", "2017-02", "2017-03", "2017-04"),2)

# Combining as a dataframe
nodet <- as.data.frame(cbind(Site, Treatment, Yr_Month))
# Site_ym column
nodet$Site_ym <- paste(nodet$Site, nodet$Yr_Month)

#Detection matrix full of 0's
detections <- matrix(data = 0, nrow = 12, ncol = 7)
colnames(detections) <- c("Blackbear", "Wolf", "Coyote", "Lynx", "Caribou", "WTDeer", "Moose")
detections <- as.data.frame(detections)

#Full data.frame for 0 detection sites
nodet <- cbind(nodet,detections)

#Add nodet to win36
win36 <- as.data.frame(rbind(win36, nodet))
win36 <- with(win36, win36[order(as.factor(as.character(Site)), Yr_Month), ]) ## Ordering by Site and Yr_month. as.factor(as.character()) rearranges the levels in Site
tail(win36)



## Combine all ACTIVE cameras.
active <- as.data.frame(rbind(alldat, win36))
head(active)
tail(active)

## Still missing:
# 1. Algar25 - 60 - Inactive between Nov. 2015 - Nov. 2016
# 2. Algar18, 32, 50 - Inactive between Nov. 2016- Apr.2017
# 3. Algar49 - Inactive between Jan. 2017 - Apr. 2017 (currently just 0's in those months, can be edited with fix())

## 1. Adding inactive rows for 25-60 between Nov. 2015 - Nov. 2016
# Creating Site names
Algar <- rep("Algar", 36)
Algarnum <- 25:60
Site <- paste(Algar, Algarnum, sep="")
class(Site)
Site <- as.factor(Site)
Site <- rep(Site, 12)

inac36 <- as.data.frame(Site)
head(inac36)
str(inac36)

## Adding Treatments
inac36$Treatment <- win60$Treatment[match(inac36$Site, win60$Site)]
head(inac36)
inac36 <- with(inac36, inac36[order(Site), ]) ## Reorder by Site
head(inac36)

## Adding Yr_Month
Yr_Month <- c("2015-11", "2015-12", "2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06", "2016-07", "2016-08", "2016-09", "2016-10")
Yr_Month <- rep(Yr_Month, 36)
inac36$Yr_Month <- Yr_Month 
head(inac36)


## Adding Site_ym
inac36$Site_ym <- paste(inac36$Site, inac36$Yr_Month)
head(inac36)

## Adding NAs for detection data
detections <- as.data.frame(matrix(NA, nrow = 432, ncol = 7))
colnames(detections) <- c("Blackbear", "Wolf", "Coyote", "Lynx", "Caribou", "WTDeer", "Moose")
head(detections)

## Combine to Deployment data
inac36 <- cbind.data.frame(inac36,detections)
head(inac36)                 

## Combine inac36 to active
Alg.data <- rbind(active, inac36)
head(Alg.data)
tail(Alg.data)
Alg.data <- with(Alg.data, Alg.data[order(as.factor(as.character(Site)), Yr_Month), ])
head(Alg.data)
tail(Alg.data)
