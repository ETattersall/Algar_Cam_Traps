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

str(stat1)

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

dim(camEff) # 60 Sites x 737 days

camEff[camEff == 0] <- NA # changes all 0 to NA, as 0 is not operational

###camEff now = matrix of 1's and NAs for active Days of entire study


##### Aggregate into active months? Or easier to do manually for monthly detection data?
# Edit ~31 inactive months within study period + Nov.2017 for all stations (currently partial month)
# will edit manually, but before removing Nov. 2017s, save copy of 2017.01 monthly detection data (no current copy for only this deployment)

## dep3 does not contain any inactive periods - not for Algar25-60 during Nov2015-2016, nor for camera failures
## Will extract Apr-Nov2017 detection data from dep 3, add in inactive months, then add this to Nov2015 - Apr2017 dataset

## monthly detections for Nov 2015- Apr 2017
## When combining with Apr-Nov 2017 data, remove April-2017 from dep2 (included in dep3)
dep2 <- read.csv("monthlydetections_nov2015-apr2017.csv")


month2017 <- dep3 %>% filter(Yr_Month == "2017-04" | Yr_Month ==  "2017-05" | Yr_Month ==  "2017-06" | Yr_Month == "2017-07" | Yr_Month == "2017-08" | Yr_Month == "2017-09" | Yr_Month == "2017-10" | Yr_Month == "2017-11")

## Manually change with fix (do so stepwise according to inactive periods to avoid mistakes)
## 1. Exchange NA's for 0's for Algar 35 and 41 --> Inactive between Apr-Nov 2017 (2017-04 - 2017-11)
fix(month2017) #Completed 13/03/2018, 10:21am

##2. Algar07,13,14,21,43,51 --> Inactive between May-Nov 2017
fix(month2017) #Completed 13/03/2018, 10:29am

##3. Algar 09,11,12,18,19,23,42,44,48,52,55,60 --> June-Nov2017
fix(month2017) # Completed 10:41am

##4. Algar 30,40,57,59
fix(month2017) # Completed 10:45am

##5. Algar 58 - Aug- Nov2017
fix(month2017) #10:46am

##6. Algar 06,27 --> Oct -Nov 2017
fix(month2017) ## 10:49am

month2017$var13 <- NULL

##Save month2017 as 2017.01_monthlydetections.csv
write.csv(month2017, "2017.01_monthlydetections.csv")

## Add month2017 to dep2 --> Will need to aggregate duplicate April 2017, remove partial month Nov. 2017







###--- Add columns for the Date/Time in POSIX format (to deployment data):

## Start Date
stat1$DateStartp <- as.POSIXct(strptime(stat1$setupCol, format = "%d/%m/%Y"))

#Last working day = Retrieval day (or most current check date)
stat1$DateLWp <- as.POSIXct(strptime(stat1$Session4Start, format = "%d/%m/%Y"))




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


