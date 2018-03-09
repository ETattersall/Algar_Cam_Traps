############################################################################
## cameraInactiveDays.R
## Accounting for failed and inactive cameras in 3-deployment dataset 
## Started 7-Mar, 2018 by Erin
###########################################################################

library(dplyr)
library(tidyr)
library(camtrapR)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")


## Exploring monthly detections for 3 deployments Nov 2015- 2017
dep3 <- read.csv("MonthlyDetections_nov2015-nov2017.csv")
# Examining detection data shows that currently, months when camera was inactive just lists detections as 0's --> error
# Should be listed as NA

## Need list of problem cameras and months of inactivity --> should exclude entire month if camera failed before the 15th of the month?
## Deployment data for Algar sites (full survey)
stat <- read.csv("Station_data/AlgarStations_DeploymentData.csv")
str(stat)

#Isolate problem sites for Apr-Nov2017 deployment
prob2017 <- stat %>% filter(Problem2_from != "") %>% select(CamStation, Session2Start, Problem1_from, Problem1_to, Session3Start, Problem2_from, Problem2_to, Session4Start, Treatment) ##28 stations had inactive periods, 2 of those had inactive periods in previous deployment

##Used prob2017 to assess partial months (124/1475 site-months) --> not enough to include active days as an effect in model (<10% of data)
## Instead, inactive site-months will be NA, with partial site-months > 15 active days will be considered as full months
# Starting months will therefore be included, ending months will be excluded (until further deployment data is added later)


#### Adding in inactive months of 3rd deployment, starting from camtrapR record table and camera effort table ####

####Compare to Jo's example matrix in script 'ACTWS_CameraData.R'
ACTWS <- read.csv("Station_data/cameff_ACTWSexample.csv") #matrix where rows = cameras, columns = study day
glimpse(ACTWS)
str(ACTWS)
View(ACTWS)


#### Attempting to check camera effort for full 3 deployments
## On-line stations only
stat1 <- stat[1:60, ]
## setupCol needs to include setup dates for Session1 and new cameras in Session2
setupdates1 <- cbind.data.frame(stat$CamStation[1:24], stat$Session1Start[1:24])
colnames(setupdates1) <- c("CamStation", "setupDate")
setupdates2 <- cbind.data.frame(stat$CamStation[25:60], stat$Session2Start[25:60])
colnames(setupdates2) <- c("CamStation", "setupDate")
setupCol <- bind_rows(setupdates1,setupdates2)

stat1$setupCol <- setupCol$setupDate[match(stat1$CamStation, setupCol$CamStation)] #Deployment data now has setupCol indicating initial deployment date

class(stat1$Session4Start)

camEff <- as.data.frame(cameraOperation(stat1, 
                          stationCol = "CamStation", 
                          setupCol = "setupCol", 
                          retrievalCol = "Session4Start",
                          hasProblems = TRUE,
                          dateFormat = "%d/%m/%Y", 
                          writecsv = FALSE))
## where NA = camera not set up, 1 = camera was operational, 0 = camera not operational 

sum(camEff,na.rm=T) ## Total active days for entire survey = 25 585 active days

# days per station
summary(rowSums(camEff,na.rm=T)) # Min = 8, Max = 735, mean 426.4

Stat.ActDays <- rowSums(camEff, na.rm = T)
stat1$ActiveDays <- Stat.ActDays ## Adding ActiveDays to deployment data

glimpse(camEff) # check for correct loading of data - to see data classes and initial values
summary(camEff) # overview of data and check for NAs
View(camEff)


###--- Add columns for the Date/Time in POSIX format (using setupCol from stat1 - deployment data):
## Converting dates to POSIX format
str(stat1)
stat1$Session1Start <- as.POSIXct(strptime(stat1$Session1Start, format = "%d/%m/%Y"))
stat1$Session2Start <- as.POSIXct(strptime(stat1$Session2Start, format = "%d/%m/%Y"))
stat1$Problem1_from <- as.POSIXct(strptime(stat1$Problem1_from, format = "%d/%m/%Y"))
stat1$Problem1_to <- as.POSIXct(strptime(stat1$Problem1_to, format = "%d/%m/%Y"))
stat1$Session3Start <- as.POSIXct(strptime(stat1$Session3Start, format = "%d/%m/%Y"))
stat1$Problem2_from <- as.POSIXct(strptime(stat1$Problem2_from, format = "%d/%m/%Y"))
stat1$Problem2_to <- as.POSIXct(strptime(stat1$Problem2_to, format = "%d/%m/%Y"))
stat1$Session4Start <- as.POSIXct(strptime(stat1$Session4Start, format = "%d/%m/%Y"))
stat1$setupCol <- as.POSIXct(strptime(stat1$setupCol, format = "%d/%m/%Y"))
str(stat1) ## NAs for Problem from/to

## Start Date
camEff$DateStartp <- as.POSIXct(strptime(stat1$setupCol, format = "%d/%m/%Y"))

#Last working day..need to create column for this in stat1

#LastActiveDay will be Problem2_to or Session4End
stat1$LastActiveDay <- ifelse(stat1$Problem2_to == "", stat1$Session4Start, stat1$Problem2_to) ## returns integers 1-4
stat1$LastActiveDay <- ifelse(stat1$Problem2_to == "", as.factor(stat1$Session4Start), as.factor(stat1$Problem2_to)) #same result

LastActiveDay <- numeric(60) #empty vector to receive Last active day for each camera
LastActiveDay[1] <- stat1$Session4Start[1] #returns a 1
stat1$Session4Start[1]

for(i in 1:60){
  if(stat1$Problem2_to[i] == ""){
    LastActiveDay[i] == stat1$Session4Start[i]
  } else LastActiveDay[i] == stat1$Problem2_to[i]
}

LastActiveDay


camEff$DateLWp <- as.POSIXct(strptime(camEff$DateLastWorking, format = "%d/%m/%Y"))

##### Create a station summary data frame for Apr- Nov 2017 deployment ####
rec2017 <- read.csv("2017.01_recordTable.csv") #record Table, Apr- Nov 2017

stat2 <- stat[1:60,] %>% select(CamStation, utmE, utmN, Treatment, Session3Start, Problem2_from, Problem2_to,Session4Start)

## Calculating camera effort (camtrapR)
camEff <- cameraOperation(stat2, 
                          stationCol = "CamStation", 
                          setupCol = "Session3Start", 
                          retrievalCol = "Session4Start",
                          hasProblems = TRUE,
                          dateFormat = "%d/%m/%Y", 
                          writecsv = FALSE)

## where NA = camera not set up, 1 = camera was operational, 0 = camera not operational





sum(camEff,na.rm=T) ## 7703

# days per station
summary(rowSums(camEff,na.rm=T)) # mean = 128.4, median = 203, range = 0 - 206

# add to camera station table
cams2016$trapdays <- rowSums(camEff,na.rm=T)


