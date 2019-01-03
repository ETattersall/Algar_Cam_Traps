###############################
# SnowData_extract.R
# Collecting snow data from camera traps
# Data extracted by Nisha Raghukumar
# Merge csvs, extract Snow column, convert to numeric (0 = False, 1 = True),
# Collect monthly average, add to monthly detection data
# Started Jan. 19, 2018 by Erin T.
# Updated Jan. 30 for 2016-2017 Winter deployment
# Updated March 13 for Apr-Nov2017 deployment
# Updated May 9 for 2017-2018 winter deployment -- extract both snow (T/F) and SnowDepth
################################

### Issues when trying to address partial months, adding check months (e.g. April or November) together, combining Algar43 data with appropriate time periods
## Will attempt to add ALL csvs from all deployments together in SnowData_allDeps.R

library(dplyr)
library(tidyr)

# working directory for Nisha's csvs
setwd('C:/Users/ETattersall/OneDrive/ALGAR/Snow_csvs(Nisha)/2018-2018 CSV Snow data')
list.files() #67 csvs --> Missing  53-56, 59 and 60 because Timelapse was not set


csv.names <- list.files() #vector of all file names


#### Attempt 1: Merge all csvs and fix discrepancies (Date is easier to standaradize on separate CSVs first) ####
#Read csv files into a list
csv.list <- lapply(csv.names, read.csv, sep = ",")
summary(csv.list) # All have 30 variables
head(csv.list) # Each element is a data frame, one from each station
tail(csv.list)

## All dates appear to be standard


# Converting factors to characters then back again
all <- csv.list %>%
  lapply(function(x) mutate_each(x, funs('as.character'))) %>%
  bind_rows() %>% 
  mutate_all(funs('as.factor')) # Worked with error `mutate_each()` is deprecated. Use `mutate_all()`, `mutate_at()` or `mutate_if()` instead.

str(all) #67 levels for folders: addressed above
head(all)

Snow <- all$Snow
table(Snow)
Snow <- toupper(Snow) #All characters to upper case
unique(Snow)
table(Snow) #10460 FALSE, 1814 TRUE


#Convert to numeric
Snow[which(Snow==TRUE)] <- 1
Snow[which(Snow==FALSE)] <- 0
table(Snow) #10460 = 0, 1814 = 1

# Create data.frame of station, date, Snow data
# Station == all$Folder
# date == all$Date
class(all$Date) #factor
head(all$Date) #225 levels, starting at Apr 7, 2018
#Confirm that dataframe is only Timelapse photos (2017-2018 data is ONLY timelapse photos -ignore this step)
unique(all$TriggerMode) #1 level TRUE (character is T for Timelapse, which is read as logical TRUE) --> Not extracted, ignore
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
table(Snow) #336 FALSE, 8153 TRUE

## 


#Convert to numeric
Snow[which(Snow==TRUE)] <- 1
Snow[which(Snow==FALSE)] <- 0
table(Snow) #4950 FALSE, 3937 TRUE

#### Create data.frame of stations (Folder column), Date, Snow ###
all$Date <- as.Date(all$Date, format = "%d-%b-%Y")
head(all$Date)
S <- as.data.frame(all$Folder)
colnames(S) <- "Station"
S$Date <- all$Date
S$Snow <- Snow
glimpse(S)
str(S) #Factor, Date, character
table(S$Snow)

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
DateStart <- min(S$DateStart) # The first detection from all stations - 2018-04-07 - first detection of the deployment
str(S)

# calculate a unique day for each day of study
# since there is no start time entered, this should work from hour 0, so should be same as calendar day
# using "floor" so it doesn't round up to next day
S$StudyDay <- floor(as.numeric(difftime(S$Date,min(S$DateStart),units="days")))
##Takes difference between the detection date and start date, without rounding up
head(S$StudyDay)
S$StudyDay <- S$StudyDay+2 #Turns start date into day 1, not day -1
summary(S$StudyDay) # 225 study days


#### Aggregating snow data into average per month (again, adapting Jo's code) ####
S$Station_SD <- paste(S$Station, S$StudyDay)
S$Year <- as.factor(format(S$Date, "%Y"))
S$Month <- as.factor(format(S$Date, "%m"))
S$Yr_month <- as.factor(format(S$Date, "%Y-%m"))

## There appear to be duplicate rows in S--> remove
S <- S[!duplicated(S), ]

head(S)
summary(S) #snow is character, convert to numeric
S$Snow <- as.numeric(S$Snow)
summary(S)
levels(S$Yr_month)
sum(S$Snow) #1814 snow days total

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
summary(m.snow) #max 19
## If there are duplicates, find out where
m.snow[which(m.snow$Snow==37),] #Returned to S to remove duplicates --> fixed

sum(m.snow$Snow) #1814 snow days in deployment

plot(x = m.snow$Yr_Month, y = m.snow$Snow, xlab = "Year-Month",ylab = "Snow Days")

## Low number of Snow Days in April because there are low number of ActiveDays in April...

### NA rows omitted from analysis regardless --> DO NOT ADD THEM
## Skip to line 422

## m.snow also needs to include NA months for failed cameras 

## Some data lost previously because NAs replaced partial months

#### Aggregate with Snow Data from earlier deployments ####
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
## Load in detection data for all 3 previous deployments
dep2yr <- read.csv("MonthlyDetections_nov2015-nov2017.csv") #Contains NAs for all Nov 2017 rows --> removed because they were partial

#Load in monthly detection data for all deployments individually
dep3 <- read.csv("2017.01_monthlydetections.csv") #472 observations, as opposed to 322 in m.snow
dep3$X <- NULL


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


### Need to add Algar32 (included in other dataset, so should be here as well)
dep1 <- read.csv("2017.01_monthlydetections.csv")
# Vectors of data for Algar32
Site <- rep("Algar32", 8)
Alg32 <- as.data.frame(Site)
Alg32$Treatment <- rep("HumanUse", 8)
Alg32$Yr_Month <- c("2017-04", "2017-05", "2017-06", "2017-07", "2017-08", "2017-09", "2017-10", "2017-11")
Alg32$Site_ym <- paste("Algar32", Yr_Month)

#NAs for detections and SnowDays
detections <- as.data.frame(matrix(NA, nrow = 8, ncol = 8))
colnames(detections) <- c("Blackbear", "Wolf", "Coyote", "Lynx", "Caribou", "WTDeer", "Moose", "SnowDays")
head(detections)

Alg32 <- cbind.data.frame(Alg32, detections)

## Combine with rest of data
dep1$X <- NULL
dep1 <- rbind.data.frame(Alg32,dep1)
dep1 <- with(dep1, dep1[order(as.factor(as.character(Site)), Yr_Month), ])
write.csv(dep1, "2017.01_monthlydetections.csv")



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
m.snow <- as.data.frame(m.snow)

## Adding Nov2018 snow data to all previous snow data
dep1 <- read.csv("MonthlyDetections_nov2015-apr2018.csv")

dep1$X.3 <- NULL
dep1$X.1 <- NULL
dep1$X.2 <- NULL
dep1$X <- NULL

str(dep1) #ActiveDays, SnowDays, pSnow
summary(dep1) ## NAs present

## Query what SnowDays is when pSnow is NA
dep1[is.na(dep1$pSnow),] ## Always 0


## Need to add new snow data to old snow data, adding together duplicated Apr months
old.sno <- cbind.data.frame(dep1$Site_ym, dep1$SnowDays)
colnames(old.sno) <- c("Site_ym", "SnowDays")
str(old.sno)
head(old.sno)
str(m.snow)
new.sno <- cbind.data.frame(m.snow$Site_ym,m.snow$Snow)
colnames(new.sno) <- c("Site_ym", "SnowDays")

Allsno <- rbind.data.frame(old.sno,new.sno) #2640 obs.
## Sum duplicated year-months (i.e. Apr 2018)
Allsno <- Allsno %>% group_by(Site_ym) %>% summarise(sum(SnowDays, na.rm=TRUE)) #2573 obs: not 2701 (73*37), but there are missing months when cam was inactive. Will become NA
colnames(Allsno) <- c("Site_ym", "SnowDays")
summary(Allsno)

## Bind Allsno to full survey
data.month <- read.csv("MonthlyDetections_nov2015-2018.csv")
str(data.month)

data.month$SnowDays <- Allsno$SnowDays[match(data.month$Site_ym, Allsno$Site_ym)]
summary(data.month) ## Only 128 Nas, because I didn't add any?

plot(x = data.month$SnowDays, y = data.month$Blackbear) # Highest detections in months with less snow - unsurprising
plot(x = data.month$SnowDays, y = data.month$Wolf) # No clear trends
plot(x = data.month$SnowDays, y = data.month$Caribou) # More with less snow
plot(x = data.month$Treatment, y = data.month$SnowDays) #same same - good

## Feb.5 - plotting SnowDays per month for Nov. 2015 - Apr. 2017 (read in as dat)

plot(x = data.month$Yr_Month, y = data.month$SnowDays) # Shows 3 winters, 2 summer, as expected. Nov. and April are more variable

### Calculate pSnow
data.month$pSnow <- data.month$SnowDays/data.month$ActiveDays
summary(data.month$pSnow) ## 1074 NAs, and MAX is > 1...

str(data.month)

data.month[which(data.month$pSnow>1),] ## 4 cases in which SnowDays = ActiveDays + 1 --> all either April or November, so must have something to do with check date and time

## Change so SnowDays = ActiveDays (subtract 1)

data.month$SnowDays <- ifelse(data.month$SnowDays>1, data.month$SnowDays - 1, data.month$SnowDays)
## recalculate pSnow
data.month$pSnow <- data.month$SnowDays/data.month$ActiveDays
summary(data.month$pSnow)
## Check that problem rows are now fixed (should now be pSnow == 1)
data.month[which(data.month$pSnow < 1),]
head(data.month$pSnow, 100)

## Checking NA months
data.month[is.na(data.month$pSnow),] ## Some when SnowDays and ActiveDays are both 0, some when ActiveDays == 0 and SnowDays == NA
data.month[is.na(data.month$SnowDays),]
## Not sure why SnowDays = 0 if ActiveDays is 0 (should be NA), but effectively they are the same


### Save monthly detection data
write.csv(data.month, "MonthlyDetections_nov2015-2018.csv")

### Remove offline sites for thesis analyses
seismic <- data.month %>% filter(Treatment != "Offline")

head(seismic)
tail(seismic)
write.csv(seismic, "Seismic_nov2015-2018.csv")

#### Daily detection data ####
day1 <- read.csv("2016.01_detections_day.csv")

S$Site_SD <- paste(S$Station, S$StudyDay)
day1$SnowDays <- S$Snow[match(day1$Site_SD, S$Site_SD)] # Results in NAs --> could be because daily detection data and daily snow data have different n.rows (by 41)
#Nov2016-Apr2017 daily detections CSV is off by 1 in its StudyDay count (missing Nov 10 2016) --> resolve later if I end up using this
# nov2015-nov2016 Snowdata is missing days (372*24 = 8928) --> because of different start days for different cameras?
#Come back to later
