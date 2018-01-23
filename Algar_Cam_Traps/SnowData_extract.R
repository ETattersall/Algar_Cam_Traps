###############################
# SnowData_extract.R
# Collecting snow data from camera traps
# Data extracted by Nisha Raghukumar
# Merge csvs, extract Snow column, convert to numeric (0 = False, 1 = True),
# Collect monthly average, add to monthly detection data
# Started Jan. 19, 2018 by Erin T.
################################

library(dplyr)
library(tidyr)

# working directory for Nisha's csvs
setwd('C:/Users/ETattersall/OneDrive/ALGAR/Snow_csvs(Nisha)')
list.files() #24 csvs


csv.names <- list.files() #vector of all file names


#### Attempt 1: Merge all csvs and fix discrepancies (Date is easier to standaradize separately) ####
#Read csv files into a list
csv.list <- lapply(csv.names, read.csv, sep = ",")
summary(csv.list) # First 12 csvs have 29 variables, last 12 have 30
str(csv.list) # Additional variable is 'X' column of NAs. In addition, last 12 csvs show case discrepancies in logical variables (TRUE/FALSE is lower case)
head(csv.list) # Each element is a data frame, one from each station
tail(csv.list)

# Try using bind_rows. Missing columns should be filled by NA
all <- bind_rows(csv.list) # Generates error: column (x) can't be converted from logical to factor

# Try converting factors to characters then back again
all <- csv.list %>%
  lapply(function(x) mutate_each(x, funs('as.character'))) %>%
  bind_rows() %>% 
  mutate_each(funs('as.factor')) # Worked with error `mutate_each()` is deprecated. Use `mutate_all()`, `mutate_at()` or `mutate_if()` instead.

str(all) #24 levels for  folders, logicals are mix of upper and lower case

Snow <- all$Snow
Snow <- toupper(Snow) #All characters to upper case
unique(Snow)
table(Snow) #4950 FALSE, 3937 TRUE


#Convert to numeric
Snow[which(Snow==TRUE)] <- 1
Snow[which(Snow==FALSE)] <- 0
table(Snow) #4950 FALSE, 3937 TRUE

# Create data.frame of station, date, Snow data
# Station == all$Folder
# date == all$Date
class(all$Date) #factor
head(all$Date) #743 levels indicates that dates have differing formats
#Confirm that dataframe is only Timelapse photos
unique(all$TriggerMode) #1 level TRUE (character is T for Timelapse, which is read as logical TRUE)
class(all$TriggerMode)
# all$TriggerMode[is.na(all$TriggerMode)] <- "T"
# unique(all$TriggerMode) Results in NAs. Leave as is (i.e. return to TRUE)

Date <- as.character(all$Date)
Date <- as.Date(Date, format = c("%d-%b-%y", "%d-%b-%Y")) # dates formatted as 01-Apr-15 are now 0015-04-01

#### Attempt 2: Revising dat format before merging into one csv ####
first <- csv.names[1:11]
first.list <- lapply(first, read.csv, sep = ",")
str(first.list) #Date has 370 levels, %d-%b-%y format (15, not 2015)
#Merge first 11 csvs
dfirst <- first.list %>%
  lapply(function(x) mutate_each(x, funs('as.character'))) %>%
  bind_rows() %>% 
  mutate_each(funs('as.factor'))
dfirst$Date <- as.Date(dfirst$Date, format = "%d-%b-%y")
str(dfirst$Date)

last <- csv.names[12:24]
last.list <- lapply(last, read.csv, sep = ",")
str(last.list) #369 levels, %d-%b-%Y format
#Merge last 13 csvs
dlast <- last.list %>%
  lapply(function(x) mutate_each(x, funs('as.character'))) %>%
  bind_rows() %>% 
  mutate_each(funs('as.factor'))
dlast$Date <- as.Date(dlast$Date, format = "%d-%b-%Y")
str(dlast$Date)
dlast$X <- NULL

#Now dates are in same format,  dfirst and dlast can be merged
all <- rbind(dfirst,dlast)
str(all) #24 levels for  folders, Snow variable is a mix of upper and lower case. TriggerMode 'T' read as TRUE

Snow <- all$Snow
Snow <- toupper(Snow) #All characters to upper case
unique(Snow)
table(Snow) #4950 FALSE, 3937 TRUE


#Convert to numeric
Snow[which(Snow==TRUE)] <- 1
Snow[which(Snow==FALSE)] <- 0
table(Snow) #4950 FALSE, 3937 TRUE

# Create data.frame of stations (Folder column), Date, Snow
S <- as.data.frame(all$Folder)
colnames(S) <- "Station"
S$Date <- all$Date
S$Snow <- Snow
str(S) #Factor, Date, character

#### Extracting Snow data from Camelot full export (April 2017 deployment) ####
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
fexp <- read.csv("2017.01_fullexport(Camelot).csv")
rt <- read.csv("2017.01_rawrecordTable.csv") #Other record table has already had 'No Animal' detections removed

str(fexp) #Both Timelapse and Snow have 3 levels: "", "false", "true"
table(fexp$Timelapse) #True = 7492 --> does not match study days, but close (7703)

str(rt)
Nan <- rt %>% filter(Species == "No Animal") %>% select(Station,Species, DateTimeOriginal,Date,Time)
TL <- rt %>% filter(Time == "12:00:00") #7640 (26 of which are marked as a Species), closer than 7492 but not all
wtf <- rt %>% filter(Time == "12:00:00" & Species != "No Animal") # Mostly cranes and bird, one coyote, one bear, 2 WTDeer...need to be resolved first?

### Check number of Timelapse images with camtrapR metadata extraction? ###
library(camtrapR)
EXIF <- exifTagNames(inDir = "D:/Algar_Apr-Nov2017/Renamed_Images", returnMetadata = TRUE) #File: FileName, MakerNotes:TriggerMode--> only returns metadata for one image



A <- fexp %>% select(Date.Time, Trap.Station.Name, Timelapse, Snow) %>% filter(Timelapse == "true")
A1 <- fexp %>% select(Date.Time, Trap.Station.Name, Timelapse, Snow) %>% filter(Timelapse == "")
#Of A1 images, could try to extract all images taken at 12:00:00?
str(A1$Date.Time)

#### Ordering data by date and station, creating study days ####
# Adapted from Jo (and Algar_prelim_analysis_ch1.R)


###--- all cameras were operational for entire year - range of detection timing depend on site
# first date of detection per site - ranges from 2015-11-05 to 2016-05-06 
mindate <- aggregate(Date~Station, S, function(x) min(x))
head(mindate) #2015-11-05 to 2015-11-08


maxdate <- aggregate(Date~Station, S, function(x) max(x)) #Ordered by station, not by date



maxdate[,2] - mindate[,2] # (Needs to be ordered by Station here)
# Time differences in days


# convert DateStart (but note that this doesn't have time set, so will treat as midnight)

S$DateStart <- min(mindate[,2]) #Adding the date of first detection to S dataframe
S$DateStart <- (strptime(S$DateStart, "%Y-%m-%d", tz="MST")) ##Adding a column for the first day of the study
DateStart <- min(S$DateStart) # The first detection from all stations - 2015-11-05 - first detection of the deployment
str(S)

# calculate a unique day for each day of study
# since there is no start time entered, this should work from hour 0, so should be same as calendar day
# using "floor" so it doesn't round up to next day
S$StudyDay <- floor(as.numeric(difftime(S$Date,min(S$DateStart),units="days"))) ##Takes difference between the detection date and start date, without rounding up
S$StudyDay <- S$StudyDay+2 #Turns start date into day 1, not day 0 --> Nov. 5 was -1
summary(S$StudyDay) # 372 study days


#### Aggregating snow data into average per month (again, adapting Jo's code) ####
S$Station_SD <- paste(S$Station, S$StudyDay)
S$Year <- as.factor(format(S$Date, "%Y"))
S$Month <- as.factor(format(S$Date, "%m"))
S$Yr_month <- as.factor(format(S$Date, "%Y-%m"))

head(S)
summary(S) #snow is character, convert to numeric
S$Snow <- as.numeric(S$Snow)
summary(S)
levels(S$Yr_month)
sum(S$Snow) #3937 snow days total

#Subsetting for Station, StudyDay, Snow, Yr_Month columns (group_by has issue with POSIXct format from S)
S1 <- as.data.frame(S$Station)
colnames(S1) <- "Station"
S1$StudyDay <- S$StudyDay
S1$Snow <- S$Snow
S1$Yr_month <- S$Yr_month

## Total snow days by month
m.snow <- S1 %>%
  group_by(Station, Yr_month) %>% 
  summarise(sum(Snow, na.rm = TRUE))
colnames(m.snow) <- c("Station","Yr_Month","Snow")
summary(m.snow) #max 31 snow days in a month, min 0, mean 12.6, median 6.5
sum(m.snow$Snow) #3937 snow days

plot(x = m.snow$Yr_Month, y = m.snow$Snow, xlab = "Year-Month",ylab = "Snow Days")

#### Aggregate with detection data ####
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
#Load in monthly detection data for 1st deployment (Nov. 2015 - Nov. 2016)
dep1 <- read.csv("2015.01_monthlydetections.csv") #312 obs, same as m.snow (total months x cameras)

#Add unique identifier Site_ym to snow dataframe
m.snow$Site_ym <- paste(m.snow$Station, m.snow$Yr_Month)

dep1$SnowDays <- m.snow$Snow[match(dep1$Site_ym, m.snow$Site_ym)]

write.csv(dep1, "2015.01_monthlydetections.csv")

plot(x = dep1$SnowDays, y = dep1$Blackbear) # Highest detections in months without snow - unsurprising
plot(x = dep1$SnowDays, y = dep1$Wolf) # No clear trends
plot(x = dep1$SnowDays, y = dep1$Caribou) # More with less snow
plot(x = dep1$Treatment, y = dep1$SnowDays) #same same - good

#### Daily detection data ####
day1 <- read.csv("2015.01_detections_day.csv")

S$Site_SD <- paste(S$Station, S$StudyDay)
day1$SnowDays <- S$Snow[match(day1$Site_SD, S$Site_SD)] # Results in NAs --> could be because daily detection data and daily snow data have different n.rows (by 41)
#Snowdata is missing days (372*24 = 8928) --> because of different start days for different cameras?
#Come back to later