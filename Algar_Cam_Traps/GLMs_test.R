### Playing with GLMMs
##Using pilot Algar data and examining GLMM output (based on code from Bolker, 2008 and EMD)

#### Setup ####
library(lme4)
library(dplyr)
library(camtrapR)

## Access data directory to read in csvs
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data") 

#Read in CSV for Algar pilot data
test.data <- read.csv("2015.01_recordTable.csv", header = TRUE)

#Read in CSV for Pilot Station data
stations <- read.csv("Station_data/Algar2015StationData.csv", header = TRUE)

#### camtrapR detection history matrix for wolves ####

## First, need a matrix of camera operability 
camOp <- cameraOperation(CTtable = stations, 
                          stationCol = "CamStation", 
                          setupCol = "DeployDate", 
                          retrievalCol = "CheckDate1", 
                          hasProblems = FALSE,
                          dateFormat = "%d/%m/%Y", 
                          writecsv = FALSE)

##Include in detection history matrix with occasion length 30 days (1 month)
wolf.mat <- as.data.frame(detectionHistory(recordTable = test.data,
                             species = "C_lupus",
                             camOp = camOp,
                             stationCol = "Station",
                             speciesCol = "Species",
                             recordDateTimeCol = "DateTimeOriginal",
                             recordDateTimeFormat = "%d/%m/%Y %H:%M",
                             occasionLength = 30,
                             day1= "station",
                             includeEffort = FALSE,
                             timeZone = "Canada/Mountain",
                             writecsv = FALSE))
##Output = matrix of wolf detections, organized by station (row) and detection occasion (column)


## detection history just gives presence/absence data, not no.detections/occasion. Confirm?
wolf.mat$Total <- apply(wolf.mat[,1:13], 1, sum, na.rm = T) ## Sum of presence recordings = 64
## Total detections = 99

## occasion = 1
wolf.mat1 <- as.data.frame(detectionHistory(recordTable = test.data,
                                           species = "C_lupus",
                                           camOp = camOp,
                                           stationCol = "Station",
                                           speciesCol = "Species",
                                           recordDateTimeCol = "DateTimeOriginal",
                                           recordDateTimeFormat = "%d/%m/%Y %H:%M",
                                           occasionLength = 1,
                                           day1= "station",
                                           includeEffort = FALSE,
                                           timeZone = "Canada/Mountain",
                                           writecsv = FALSE))
##Count no. presences recorded
wolf.mat1$Total <- apply(wolf.mat1[,1:373], 1, sum, na.rm = T) ## 91. Some detections may be on same day

#### detectionHistory != number detections/occasion length ####


#### Add Treatment column to pilot data ####
test.data$Treatment <- stations$Treatment[match(test.data$Station, stations$CamStation)]

## Checking classes of data
class(test.data$Treatment) #factor
class(test.data$Station) #factor
class(test.data$Date) #Date


##Subset record table for wolves only, keeping station, species, Date, Time, and Treatment data
wolf <- test.data %>% filter(Species == "C_lupus") %>% 
        select(Station, Species, DateTimeOriginal, Date, Time, Treatment)

#### Detection matrices ####
## Need to count number of detections per month. Divide data by station and by month?

##1. Divide Date into Day, Month, Year columns
class(wolf$Date)
wolf$Date <- as.Date(wolf$Date, "%d/%m/%Y") ## Convert to date class
## Split into 3 columns (day, month, year)
df <- data.frame(date = wolf$Date,
                       day = as.numeric(format(wolf$Date, format = "%d")),
                       month = as.numeric(format(wolf$Date, format = "%m")),
                       year = as.numeric(format(wolf$Date, format = "%Y"))) 

## Want to combine year and month columns (to avoid Nov. 2015 and Nov. 2016 being considered the same)
df <-  within(df, year.month <- sprintf("%d-%02d", year, month))
## Combine new date data to wolf dataframe
wolf$year.month <- df$year.month

unique(wolf$year.month)

## Arrange into matrix of detection counts by station and year.month
## Ask Jo how to do this...
