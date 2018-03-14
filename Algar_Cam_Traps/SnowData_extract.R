###############################
# SnowData_extract.R
# Collecting snow data from camera traps
# Data extracted by Nisha Raghukumar
# Merge csvs, extract Snow column, convert to numeric (0 = False, 1 = True),
# Collect monthly average, add to monthly detection data
# Started Jan. 19, 2018 by Erin T.
# Updated Jan. 30 for 2016-2017 Winter deployment
# Updated March 13 for Apr-Nov2017 deployment
################################

library(dplyr)
library(tidyr)

# working directory for Nisha's csvs
setwd('C:/Users/ETattersall/OneDrive/ALGAR/Snow_csvs(Nisha)/2017-2017 CSV Snow data')
list.files() #57 csvs --> Missing Algar06 (file too large), Algar21 (stolen), Algar43 (nothing on card) --> NAs will be added


csv.names <- list.files() #vector of all file names


#### Attempt 1: Merge all csvs and fix discrepancies (Date is easier to standaradize on separate CSVs first) ####
#Read csv files into a list
csv.list <- lapply(csv.names, read.csv, sep = ",")
summary(csv.list) # All have 30 variables
str(csv.list) # Appear to all be the same. Check possible problem variables?
head(csv.list) # Each element is a data frame, one from each station
tail(csv.list)

## All dates appear to be standard

# Try using bind_rows. Missing columns should be filled by NA
all <- bind_rows(csv.list) # Generates error: column (x) can't be converted from logical to factor

# Try converting factors to characters then back again
all <- csv.list %>%
  lapply(function(x) mutate_each(x, funs('as.character'))) %>%
  bind_rows() %>% 
  mutate_all(funs('as.factor')) # Worked with error `mutate_each()` is deprecated. Use `mutate_all()`, `mutate_at()` or `mutate_if()` instead.

str(all) #57 levels for folders: addressed above

Snow <- all$Snow
Snow
Snow <- toupper(Snow) #All characters to upper case
unique(Snow)
table(Snow) #6958 FALSE, 665 TRUE


#Convert to numeric
Snow[which(Snow==TRUE)] <- 1
Snow[which(Snow==FALSE)] <- 0
table(Snow) #6958 = 0, 665 = 1

# Create data.frame of station, date, Snow data
# Station == all$Folder
# date == all$Date
class(all$Date) #factor
head(all$Date) #162 levels, indicating all Dates are in same format
#Confirm that dataframe is only Timelapse photos
unique(all$TriggerMode) #1 level TRUE (character is T for Timelapse, which is read as logical TRUE)
class(all$TriggerMode)
# all$TriggerMode[is.na(all$TriggerMode)] <- "T"
# unique(all$TriggerMode) Results in NAs. Leave as is (i.e. return to TRUE)

#### If Date format needs to be standardized, otherwise just convert to Date class (skip to line 108) ####
Date <- as.character(all$Date)
Date <- as.Date(Date, format = c("%d-%b-%y", "%d-%b-%Y")) # dates formatted as 01-Apr-15 are now 0015-04-01 
head(Date)

#### Attempt 2: Revising date format before merging into one csv ####
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

#### Create data.frame of stations (Folder column), Date, Snow ###
all$Date <- as.Date(all$Date, format = "%d-%b-%Y")
S <- as.data.frame(all$Folder)
colnames(S) <- "Station"
S$Date <- all$Date
S$Snow <- Snow
glimpse(S)
str(S) #Factor, Date, character

#### Extracting Snow data from Camelot full export (April 2017 deployment): Incomplete, having Nisha do in Timelapse ####
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
head(mindate)


maxdate <- aggregate(Date~Station, S, function(x) max(x)) #Ordered by station, not by date



maxdate[,2] - mindate[,2] # (Needs to be ordered by Station here)
# Time differences in days


# convert DateStart (but note that this doesn't have time set, so will treat as midnight)

S$DateStart <- min(mindate[,2]) #Adding the date of first detection to S dataframe
S$DateStart <- (strptime(S$DateStart, "%Y-%m-%d", tz="MST")) ##Adding a column for the first day of the study
DateStart <- min(S$DateStart) # The first detection from all stations - 2017-04-19 - first detection of the deployment
str(S)

# calculate a unique day for each day of study
# since there is no start time entered, this should work from hour 0, so should be same as calendar day
# using "floor" so it doesn't round up to next day
S$StudyDay <- floor(as.numeric(difftime(S$Date,min(S$DateStart),units="days"))) ##Takes difference between the detection date and start date, without rounding up
S$StudyDay <- S$StudyDay+2 #Turns start date into day 1, not day -1
summary(S$StudyDay) # 205 study days


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
sum(S$Snow) #665 snow days total

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
summary(m.snow) #max 13 snow days in a month, min 0, mean 2.065, median 0
sum(m.snow$Snow) #665 snow days in deployment

plot(x = m.snow$Yr_Month, y = m.snow$Snow, xlab = "Year-Month",ylab = "Snow Days")

#### Aggregate with detection data ####
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
#Load in monthly detection data for 3rd deployment (Apr-Nov 2017)
dep1 <- read.csv("2017.01_monthlydetections.csv") #472 observations, as opposed to 322 in m.snow
dep1$X <- NULL
##m.snow does not include NA months for failed cameras

## Add in 8 NA months for cameras with no snow data: Algar06, Algar21, Algar43
#Station column
Algar06 <- rep("Algar06",8)
Algar21 <- rep("Algar21",8)
Algar43 <- rep("Algar43",8)
Station <- append(x = Algar06, values = c(Algar21, Algar43))

#Yr_Month column
Yr_Month <- rep(c("2017-04", "2017-05", "2017-06", "2017-07", "2017-08", "2017-09", "2017-10", "2017-11"),3)
#Snow column
Snow <- rep(NA,24)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)
m.snow <- with(m.snow, m.snow[order(as.factor(as.character(Station)), Yr_Month), ])

## Remove Algar32: 2017-04,  (no data, only active for 3 days)
m.snow[192,] #confirming row number (row num. in data frame does not match)
m.snow <- m.snow[-192, ]

## If snow data was collected in partial months --> keep if active days > 15 days (same rules as detections)
# Partial months during which snow data was collected (need to be switched to NAs):
# Algar11: 2017-06, Algar12:2017-06, Algar18: 2017-04, Algar19: 2017-06, Algar23: 2017-06, Algar35: 2017-06, 
# Algar40: 2017-07, Algar42: 2017-06, Algar44: 2017-06, Algar48:2017-06
#Algar27: active full time, but tree fell in Sep. Change 2017-10 and 2017-11 to NA
# Change manually
fix(m.snow)

## 1. Add NA months for failed cameras May - Nov (7 months): Algar07, Algar13, Algar14, Algar51
Algar07 <- rep("Algar07", 7)
Algar13 <- rep("Algar13", 7)
Algar14 <- rep("Algar14", 7)
Algar51 <- rep("Algar51", 7)
Station <- append(x = Algar07, values = c(Algar13, Algar14, Algar51))
#Yr_Month column
Yr_Month <- rep(c("2017-05", "2017-06", "2017-07", "2017-08", "2017-09", "2017-10", "2017-11"),4)
#Snow column
Snow <- rep(NA,28)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)
m.snow <- with(m.snow, m.snow[order(as.factor(as.character(Station)), Yr_Month), ])

##2. Add NA months for failed cameras Jun - Nov (6 months): Algar09, Algar18, Algar52, Algar55, Algar60
Algar09 <- rep("Algar09", 6)
Algar18 <- rep("Algar18", 6)
Algar52 <- rep("Algar52", 6)
Algar55 <- rep("Algar55", 6)
Algar60 <- rep("Algar60", 6)

Station <- append(x = Algar09, values = c(Algar18, Algar52, Algar55, Algar60))
#Yr_Month column
Yr_Month <- rep(c("2017-06", "2017-07", "2017-08", "2017-09", "2017-10", "2017-11"),5)
#Snow column
Snow <- rep(NA,30)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)
m.snow <- with(m.snow, m.snow[order(as.factor(as.character(Station)), Yr_Month), ])

##3. Add NA months for failed cameras Jul - Nov (5 months): Algar11, Algar12, Algar19, Algar23, Algar35, Algar42, Algar44, Algar48, Algar57, Algar59
Algar11 <- rep("Algar11", 5)
Algar12 <- rep("Algar12", 5)
Algar19 <- rep("Algar19", 5)
Algar23 <- rep("Algar23", 5)
Algar35 <- rep("Algar35", 5)
Algar42 <- rep("Algar42", 5)
Algar44 <- rep("Algar44", 5)
Algar48 <- rep("Algar48", 5)
Algar57 <- rep("Algar57", 5)
Algar59 <- rep("Algar59", 5)

Station <- append(x = Algar11, values = c(Algar12, Algar19, Algar23, Algar35, Algar42, Algar44, Algar48, Algar57, Algar59))
#Yr_Month column
Yr_Month <- rep(c("2017-07", "2017-08", "2017-09", "2017-10", "2017-11"),10)
#Snow column
Snow <- rep(NA,50)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)
m.snow <- with(m.snow, m.snow[order(as.factor(as.character(Station)), Yr_Month), ])

##4. Add NA months for failed cameras Aug - Nov. (4 months): Algar40, Algar58
Algar40 <- rep("Algar40", 4)
Algar58 <- rep("Algar58", 4)

Station <- append(x = Algar40, values = Algar58)
#Yr_Month column
Yr_Month <- rep(c("2017-08", "2017-09", "2017-10", "2017-11"), 2)
#Snow column
Snow <- rep(NA,8)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)
m.snow <- with(m.snow, m.snow[order(as.factor(as.character(Station)), Yr_Month), ])

##5. Algar30 (inactive Jul-Nov 2017 - 5 months --> missed before)
Station <- rep("Algar30", 5)
#Yr_Month column
Yr_Month <- c("2017-07", "2017-08", "2017-09", "2017-10", "2017-11")
#Snow column
Snow <- rep(NA,5)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)
m.snow <- with(m.snow, m.snow[order(as.factor(as.character(Station)), Yr_Month), ])

## Still a 6 row discrepancy...
## Also ordered incorrectly... shouldn't be a problem in long run but difficult to see missing data
str(m.snow)
table(m.snow$Station) # Algar30 has 9, Algar41 has 1
# Algar30 2017-07 is repeated, with one row Snow = NA, other Snow = 0 --> remove 0
m.snow[234, ] # Found with trial and error :P
m.snow <- m.snow[-234, ]

##Algar 41 missed: need NAs for May-Nov
Station <- rep("Algar41", 7)
#Yr_Month column
Yr_Month <- c("2017-05", "2017-06","2017-07", "2017-08", "2017-09", "2017-10", "2017-11")
#Snow column
Snow <- rep(NA,7)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)
m.snow <- with(m.snow, m.snow[order(as.factor(as.character(Station)), Yr_Month), ])



################
#(2nd deployment)m.snow has 6 months of data for 52 and 56, which do not have detections
## Empty detection rows need to be added to dep1 for 52 and 56 (copy code from Algar_combine_datasets.R)
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

#Add nodet to dep1
dep1 <- as.data.frame(rbind(dep1, nodet))
dep1 <- with(dep1, dep1[order(as.factor(as.character(Site)), Yr_Month), ]) ## Ordering by Site and Yr_month. as.factor(as.character()) rearranges the levels in Site
tail(dep1)



#### Combining SnowDays data to detection data ####
## Add unique identifier Site_ym to snow dataframe ####
m.snow$Site_ym <- paste(m.snow$Station, m.snow$Yr_Month)

dep1$SnowDays <- m.snow$Snow[match(dep1$Site_ym, m.snow$Site_ym)]

write.csv(dep1, "2017.01_monthlydetections.csv")

plot(x = dep1$SnowDays, y = dep1$Blackbear) # Highest detections in months with less snow - unsurprising
plot(x = dep1$SnowDays, y = dep1$Wolf) # No clear trends
plot(x = dep1$SnowDays, y = dep1$Caribou) # More with less snow
plot(x = dep1$Treatment, y = dep1$SnowDays) #same same - good

## Feb.5 - plotting SnowDays per month for Nov. 2015 - Apr. 2017 (read in as dat)

plot(x = dat$Yr_Month, y = dat$SnowDays) # Shows 2 winters, 1 summer, as expected. Nov. and April are more variable

#### Daily detection data ####
day1 <- read.csv("2016.01_detections_day.csv")

S$Site_SD <- paste(S$Station, S$StudyDay)
day1$SnowDays <- S$Snow[match(day1$Site_SD, S$Site_SD)] # Results in NAs --> could be because daily detection data and daily snow data have different n.rows (by 41)
#Nov2016-Apr2017 daily detections CSV is off by 1 in its StudyDay count (missing Nov 10 2016) --> resolve later if I end up using this
# nov2015-nov2016 Snowdata is missing days (372*24 = 8928) --> because of different start days for different cameras?
#Come back to later
