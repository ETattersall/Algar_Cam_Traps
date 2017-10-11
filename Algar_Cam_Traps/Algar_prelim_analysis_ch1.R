###########################################################
# Algar_prelim_analysis_ch1.R
# Preliminary data analysis, modifying Jo's code (Algar_prelim_analysis_BayesianPA.R)
# Script modifying Jo's Bayesian analysis code for max. likelihood
# created by Erin T., 11-Oct-2017
###########################################################

library(reshape2)	# for formatting data frames
library(dplyr)		# for applying functions to subsets of data frames
library(ggplot2)	# for data visualization
library(stringr)	# for working with character strings
library(tidyr)		# for data formatting functions
library(knitr)		# for the "kable" function for formatting tables

###--- set directory, load files, check that they loaded correctly
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
data <- read.csv("2015.01_recordTable.csv",header=T, row.names=1)
glimpse(data)
summary(data)

###--- Add columns for the Date.Time and Date in POSIX format:
data$Date.Time <- as.POSIXct(strptime(data$DateTimeOriginal, format = "%d/%m/%Y %H:%M"))
data$Datep <- as.POSIXct(strptime(data$Date, format = "%d/%m/%Y"))
str(data)

###--- all cameras were operational for entire year - range of detection timing depend on site
# first date of detection per site - ranges from 2015-11-05 to 2016-05-06 
mindate <- aggregate(Date.Time~Station, data, function(x) min(x))
mindate.ord <- mindate[order(mindate$Date.Time, mindate$Station),] ## Order first detections by date
mindate.ord <- mindate[order(mindate$Station, mindate$Date.Time),] ## Order first detections by Station
mindate.ord 

maxdate <- aggregate(Date.Time~Station, data, function(x) max(x))
maxdate.ord <- maxdate[order(maxdate$Date.Time, maxdate$Station),]
maxdate.ord <- maxdate[order(maxdate$Station, maxdate$Date.Time),]
maxdate.ord
# last date of detection per site - ranges from 2016-08-09 to 2016-11-10

maxdate.ord[,2] - mindate.ord[,2] # (Needs to be ordered by Station here)
# Time differences in days
#[1] 300.56166 362.92569  98.13536 352.76646 216.12815 315.89721 262.92764 362.68677 366.22271 359.82296 241.57117
#[12] 368.61998 183.88051 342.17454 364.64853 307.09172 280.61439 359.25888 360.36235 345.03010 356.48046 265.69171
#[23] 315.82545 305.37284


# convert DateStart (but note that this doesn't have time set, so will treat as midnight)

data$DateStart <- min(mindate.ord[,2]) #Adding the date of first detection to record Table
data$DateStart <- (strptime(data$DateStart, "%Y-%m-%d", tz="MST")) ##Adding a column for the first day of the study
DateStart <- min(data$DateStart) # The first detection from all stations - 2015-11-05 - first day of deployment
str(data)

# calculate a unique day for each day of study
# taking straight difference will include partial days, so is really 24-hour periods from time first camera set
# since there is no start time entered, this should work from hour 0, so should be same as calendar day
# using "floor" so it doesn't round up to next day
data$StudyDay <- floor(as.numeric(difftime(data$Date.Time,min(data$DateStart),units="days"))) ##Takes difference between the detection date and start date, without rounding up
data$StudyDay <- data$StudyDay+1 #Turns start date into day 1, not day 0
summary(data$StudyDay) # 1-372 study days

####################################################################
##Adding Treatment Column (naming Stations 1-12 as Control and everything else as SPP)
data$Treatment <- as.factor(ifelse(data$Station=="Algar01"|data$Station=="Algar02"|data$Station=="Algar03"|data$Station=="Algar04"|
                                     data$Station=="Algar05"|data$Station=="Algar06"|data$Station=="Algar07"|data$Station=="Algar08"|
                                     data$Station=="Algar09"|data$Station=="Algar10"|data$Station=="Algar11"|data$Station=="Algar12",
                                   "Control", "SPP"))

glimpse(data)
summary(data)


