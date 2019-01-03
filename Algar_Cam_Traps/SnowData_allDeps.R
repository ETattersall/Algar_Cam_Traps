################################################
## SnowData_allDeps.R
## Snow data extraction: attempting to extract snow data for all deployments and save all together
## Modifying code from SnowDate_extract.R
## Started May 9, 2018
################################################

library(dplyr)
library(tidyr)

# working directory for Nisha's csvs
setwd('C:/Users/ETattersall/OneDrive/ALGAR/Snow_csvs(Nisha)')
list.files(recursive = TRUE) #Should have 24 + 60 + 60 + 73 = 217 CSVs
# Only have 209. Algar43 apr2017 and nov2017 are combined, Algar06apr2017 was too large, Algar21apr2017 and Algar27nov2017 were from stolen cameras, Algar17nov2017 failed before collecting Snow data, Algar30nov2017 needs to be added manually, Algar32nov2016 failed in 4 days, Algar50nov2016 failed in 4 days also

csv.names <- list.files(recursive=TRUE)

#### Attempt 1: Merge all csvs and fix discrepancies (Date is easier to standaradize on separate CSVs first) ####
#Read csv files into a list
csv.list <- lapply(csv.names, read.csv, sep = ",")
summary(csv.list) # All have 30 variables
head(csv.list) # Each element is a data frame, one from each station
tail(csv.list)

## All dates appear to be standard


# Try converting factors to characters then back again
all <- csv.list %>%
  lapply(function(x) mutate_all(x, funs('as.character'))) %>%
  bind_rows() %>% 
  mutate_all(funs('as.factor')) # Worked with error `mutate_each()` is deprecated. Use `mutate_all()`, `mutate_at()` or `mutate_if()` instead.

str(all) #70 levels for folders: addressed above. Date had 1257 levels --> likely different formats
head(all)

#### Revising date format before merging into one csv ####
## CSVs Algar01 - Algar11 from nov2016 all have different Date structure

first <- csv.names[1:11]
first.list <- lapply(first, read.csv, sep = ",")
str(first.list) #Date has 370 levels, %d-%b-%y format (15, not 2015)
#Merge first 11 csvs
dfirst <- first.list %>%
  lapply(function(x) mutate_all(x, funs('as.character'))) %>%
  bind_rows() %>% 
  mutate_all(funs('as.factor'))
dfirst$Date <- as.Date(dfirst$Date, format = "%d-%b-%y")
str(dfirst$Date)

last <- csv.names[12:209]
last.list <- lapply(last, read.csv, sep = ",")
str(last.list) # %d-%b-%Y format
#Merge last 198 csvs
dlast <- last.list %>%
  lapply(function(x) mutate_all(x, funs('as.character'))) %>%
  bind_rows() %>% 
  mutate_all(funs('as.factor'))
dlast$Date <- as.Date(dlast$Date, format = "%d-%b-%Y")
str(dlast$Date)
dlast$X <- NULL


#Now dates are in same format,  dfirst and dlast can be merged
all <- rbind(dfirst,dlast)
str(all) #24 levels for  folders, Snow variable is a mix of upper and lower case. TriggerMode 'T' read as TRUE

Snow <- all$Snow
table(Snow) #lower and upper case
Snow <- toupper(Snow)
unique(Snow)
table(Snow) #13836 FALSE, 20215 TRUE


#Convert to numeric
Snow[which(Snow==TRUE)] <- 1
Snow[which(Snow==FALSE)] <- 0
table(Snow) #13836 = 0, 20215 = 1

#### Create data.frame of stations (Folder column), Date, Snow ###
all$Date <- as.Date(all$Date, format = "%d-%b-%Y")
head(all$Date)
S <- as.data.frame(all$Folder)
colnames(S) <- "Station"
S$Date <- all$Date
S$Snow <- Snow
glimpse(S)
str(S) #Factor, Date, character

head(S)
tail(S) ##Not ordered by station and date

#### Ordering data by date and station, creating study days ####
S <- S[order(S$Station, S$Date),]
head(S)
tail(S) #Now ordered by Station and by date




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

## Formatting date in POSIXlt
S$Date <- (strptime(S$Date, "%Y-%m-%d", tz="MST"))
str(S)

# calculate a unique day for each day of study
# since there is no start time entered, this should work from hour 0, so should be same as calendar day
# using "floor" so it doesn't round up to next day
S$StudyDay <- floor(as.numeric(difftime(S$Date,min(S$DateStart),units="days"))) ##Takes difference between the detection date and start date, without rounding up
S$StudyDay <- S$StudyDay+1 #Turns start date into day 1, not day 0
summary(S$StudyDay) # 886 study days

table(S$Snow) ## Only 0's and 1's. Does NOT contain inactive days


##### Save Snow presence-absence table for use in occurrence models. 
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
write.csv(S, "SnowPresence_daily.csv")


#### Aggregating snow data into average per month (again, adapting Jo's code) ####
S$Station_SD <- paste(S$Station, S$StudyDay)
S$Year <- as.factor(format(S$Date, "%Y"))
S$Month <- as.factor(format(S$Date, "%m"))
S$Yr_month <- as.factor(format(S$Date, "%Y-%m"))

##Are there duplicated rows? If so, remove
S[duplicated(S),] #Algar14, 2018-03-26 to 2018-04-04 is duplicated
S <- S[!duplicated(S), ]

head(S)
summary(S) #snow is character, convert to numeric
S$Snow <- as.numeric(S$Snow)
summary(S)
levels(S$Yr_month) #30 --> full study period
sum(S$Snow) #20206 snow days total

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
summary(m.snow) 


plot(x = m.snow$Yr_Month, y = m.snow$Snow, xlab = "Year-Month",ylab = "Snow Days")


##### Dealing with partial months, adding NAs for missing site-months. End data frame should have 2190 rows (30 months * 73 cams) ####
## 2017-2018 winter has numerous partial months. Address by first checking to see is Active Days and SnowDays are correlated. If so, divide SnowDays by ActiveDays
##m.snow has 1214 site-months. Should have (24*30) + (36*18) + (13*6) = 1446, + NAs for months when camera were not deployed = 2190. Discrepancy from failed cameras and CSV errors

### IMPORTANT NOTE: Code inactive months as 0s instead of NAs --> consistent with how camera detection data is coded and models are more likely to run with fewer NAs

## I. Add NAs for months when cameras were not active
# a. Second deployment (in lab notebook, June 2, 2017): 
# Algar18,32,49,50 inactive 2016-12 to 2017-03
Algar18 <- rep("Algar18", 4)
Algar32 <- rep("Algar32", 4)
Algar50 <- rep("Algar50", 4)
Station <- append(x = Algar18, values = c(Algar32, Algar50))

#Yr_Month column
Yr_Month <- rep(c("2016-12", "2017-01", "2017-02", "2017-03"), 3)
#Snow column
Snow <- rep(NA,12)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

# Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

# Algar50 also needs 2016-11, where no snow was present that month
Station <- "Algar50"
Yr_Month <- "2016-11"
Snow <- 0
# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

# Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)



# b. Third deployment (see partial months): 
## 1. Add NA months for failed cameras May - Oct (6 months): Algar07, Algar13, Algar14, Algar21, Algar32, Algar41, Algar51
Algar07 <- rep("Algar07", 6)
Algar13 <- rep("Algar13", 6)
Algar14 <- rep("Algar14", 6)
Algar21 <- rep("Algar21", 6)
Algar32 <- rep("Algar32", 6)
Algar41 <- rep("Algar41", 6)
Algar51 <- rep("Algar51", 6)
Station <- append(x = Algar07, values = c(Algar13, Algar14, Algar21, Algar32, Algar41, Algar51))
#Yr_Month column
Yr_Month <- rep(c("2017-05", "2017-06", "2017-07", "2017-08", "2017-09", "2017-10"),7)
#Snow column
Snow <- rep(NA,42)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

##2. Add NA months for failed cameras Jun - Oct (5 months): Algar09, Algar18, Algar43, Algar52, Algar55
Algar09 <- rep("Algar09", 5)
Algar18 <- rep("Algar18", 5)
Algar43 <- rep("Algar43", 5)
Algar52 <- rep("Algar52", 5)
Algar55 <- rep("Algar55", 5)


Station <- append(x = Algar09, values = c(Algar18,  Algar43, Algar52, Algar55))
#Yr_Month column
Yr_Month <- rep(c("2017-06", "2017-07", "2017-08", "2017-09", "2017-10"),5)
#Snow column
Snow <- rep(NA,25)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

##3. Add NA months for failed cameras Jul - Oct (4 months): Algar11, Algar12, Algar19, Algar23, Algar35, Algar42, Algar44, Algar48, Algar57, Algar59, Algar60
Algar11 <- rep("Algar11", 4)
Algar12 <- rep("Algar12", 4)
Algar19 <- rep("Algar19", 4)
Algar23 <- rep("Algar23", 4)
Algar35 <- rep("Algar35", 4)
Algar42 <- rep("Algar42", 4)
Algar44 <- rep("Algar44", 4)
Algar48 <- rep("Algar48", 4)
Algar57 <- rep("Algar57", 4)
Algar59 <- rep("Algar59", 4)
Algar60 <- rep("Algar60", 4)

Station <- append(x = Algar11, values = c(Algar12, Algar19, Algar23, Algar35, Algar42, Algar44, Algar48, Algar57, Algar59, Algar60))
#Yr_Month column
Yr_Month <- rep(c("2017-07", "2017-08", "2017-09", "2017-10"),11)
#Snow column
Snow <- rep(NA,44)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

##4. Add NA months for failed cameras Aug - Oct. (3 months): Algar40, Algar58
Algar30 <- rep("Algar30", 3)
Algar40 <- rep("Algar40", 3)
Algar58 <- rep("Algar58", 3)

Station <- append(x = Algar30, values = c(Algar40, Algar58))
#Yr_Month column
Yr_Month <- rep(c("2017-08", "2017-09", "2017-10"), 3)
#Snow column
Snow <- rep(NA,9)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)



## Order to check progress
head(m.snow) # NAs added to top
tail(m.snow)
m.snow <- with(m.snow, m.snow[order(as.factor(as.character(Station)), as.factor(as.character(Yr_Month))), ])
head(m.snow)
tail(m.snow)

# Algar06-2017-05 to 2017-10
Station <- rep("Algar06", 6)
#Yr_Month column
Yr_Month <- c("2017-05", "2017-06", "2017-07", "2017-08", "2017-09", "2017-10")
#Snow column
Snow <- rep(NA,6)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)


m.snow <- with(m.snow, m.snow[order(as.factor(as.character(Station)), as.factor(as.character(Yr_Month))), ])
head(m.snow)
tail(m.snow)
table(is.na(m.snow$Snow))

# c. Fourth deployment (see partial months):
# 1. Stations inactive between 2017-11 to 2018-04: Algar10, 17,18,21,27,40,46,48,59
## Adding NA rows for 2017-12 to 2018-04 (5 months)
Algar10 <- rep("Algar10", 5)
Algar17 <- rep("Algar17", 5)
Algar18 <- rep("Algar18", 5)
Algar21 <- rep("Algar21", 5)
Algar27 <- rep("Algar27", 5)
Algar40 <- rep("Algar40", 5)
Algar46 <- rep("Algar46", 5)
Algar48 <- rep("Algar48", 5)
Algar59 <- rep("Algar59", 5)


Station <- append(x= Algar10, values=c(Algar17, Algar18, Algar21, Algar27, Algar40, Algar46, Algar48, Algar59))
#Yr_Month column
Yr_Month <- rep(c("2017-12", "2018-01", "2018-02", "2018-03", "2018-04"), 9)
#Snow column
Snow <- rep(NA,45)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

# 2. Stations inactive between 2017-12 to 2018-04: Algar05, 06, 07, 15, 34, 38, 42


## Adding NA rows for 2018-01 to 2018-04 (4 months)
Algar05 <- rep("Algar05", 4)
Algar06 <- rep("Algar06", 4)
Algar07 <- rep("Algar07", 4)
Algar15 <- rep("Algar15", 4)
Algar34 <- rep("Algar34", 4)
Algar38 <- rep("Algar38", 4)



Station <- append(x= Algar05, values=c(Algar06, Algar07, Algar15, Algar34, Algar38))
#Yr_Month column
Yr_Month <- rep(c("2018-01", "2018-02", "2018-03", "2018-04"), 6)
#Snow column
Snow <- rep(NA,24)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

# 3. Stations inactive between 2018-01 to 2018-04: Algar49, 36
## Adding NA rows for 2018-02 to 2018-04 (3 months)
Algar36 <- rep("Algar36", 3)
Algar49 <- rep("Algar49", 3)


Station <- append(x= Algar36, values=Algar49)
#Yr_Month column
Yr_Month <- rep(c("2018-02", "2018-03", "2018-04"), 2)
#Snow column
Snow <- rep(NA,6)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

m.snow <- with(m.snow, m.snow[order(as.factor(as.character(Station)), as.factor(as.character(Yr_Month))), ])
# 4. Stations inactive between 2018-02 to 2018-04: Algar30, Algar42, Algar50

## Adding NA rows for 2018-03 to 2018-04 (2 months)
Station <- rep("Algar42", 2)


#Yr_Month column
Yr_Month <- c("2018-03", "2018-04")
#Snow column
Snow <- rep(NA,2)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

#5. Stations inactive between 2018-03 to 2018-04: Algar 30,50
## Adding NA rows for 2018-03 to 2018-04 (2 months) for 30 and 50
Algar30 <- rep("Algar30", 2)
Algar50 <- rep("Algar50", 2)


Station <- append(x= Algar30, values=Algar50)
#Yr_Month column
Yr_Month <- rep(c("2018-03", "2018-04"), 2)
#Snow column
Snow <- rep(NA,4)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

#6. Stations inactive during 2018-04: Algar62,71
## Adding NA rows for 2018-03 to 2018-04 (2 months) for 30 and 50
Station <- c("Algar62", "Algar71")


#Yr_Month column
Yr_Month <- rep("2018-04", 2)
#Snow column
Snow <- rep(NA,2)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

m.snow <- with(m.snow, m.snow[order(as.factor(as.character(Station)), as.factor(as.character(Yr_Month))), ])
head(m.snow)
tail(m.snow)
table(is.na(m.snow$Snow))

## II. Some stations (noted at top) need to be addressed individually.
## Algar30 nov2017: 4 months of full snow, failed in Feb. (23 days in Nov., 30 in Dec., 30 in Jan., 26 in Feb.). 2 NA months for failing
## Algar30nov2017 needs to be added manually, Algar32nov2016 failed in 4 days; Algar32nov2017 reading incorrect Snowdata (23, 31, 31, 28, 31, 7)

#1. Algar30 --> manually add in Snow data for 2017-11 to 2018-04
## Algar30 nov2017: 4 months of full snow, failed in Feb. -->  (23 days in Nov., 31 in Dec., 31 in Jan., 26 in Feb.). NA, 30, 30, 26, NA , NA
## Add in 2017-11 to 2018-02 (2018-03 and 2018-04 added in earlier): 23, 31, 31, 26
Station <- rep("Algar30", 4)
Yr_Month <- c("2017-11", "2017-12", "2018-01", "2018-02")
Snow <- c(23, 31,31,26)
# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

## Algar32 - Add 2016-11 (0), correct 2017-11 to 2018-04
Station <- "Algar32"
Yr_Month <- "2016-11"
Snow <- 0
# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

# Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

## Correcting incorrect entries(2017-11=23, 2017-12=31, 2018-01=31, 2018-02=28, 2018-03=31, 2018-04=7)
m.snow$Snow[m.snow$Station=="Algar32" & m.snow$Yr_Month == "2017-11"] <- 23
m.snow$Snow[m.snow$Station=="Algar32" & m.snow$Yr_Month == "2017-12"] <- 31
m.snow$Snow[m.snow$Station=="Algar32" & m.snow$Yr_Month == "2018-01"] <- 31
m.snow$Snow[m.snow$Station=="Algar32" & m.snow$Yr_Month == "2018-02"] <- 28
m.snow$Snow[m.snow$Station=="Algar32" & m.snow$Yr_Month == "2018-03"] <- 31
m.snow$Snow[m.snow$Station=="Algar32" & m.snow$Yr_Month == "2018-04"] <- 7

m.snow <- with(m.snow, m.snow[order(as.factor(as.character(Station)), as.factor(as.character(Yr_Month))), ])
head(m.snow)
tail(m.snow)
table(is.na(m.snow$Snow))


str(m.snow)
summary(m.snow$Yr_Month)

m.snow$Station[which(m.snow$Yr_Month=="2018-04")] #45 missing
m.snow$Station[which(m.snow$Yr_Month=="2018-01")] #45 missing
m.snow$Station[which(m.snow$Yr_Month=="2017-06")] #60 missing


## 2018-01 to 2018-04: Algar45 is missing (failed on Dec.28)
Station <- rep("Algar45", 4)
Yr_Month <- c("2018-01", "2018-02", "2018-03", "2018-04")
Snow <- rep(NA, 4)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

## 2017-06: Algar60 missing (Active for 9 days in May, but addressed as if it failed in June)
Station <- "Algar60"
Yr_Month <- "2017-06"
Snow <- NA
# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

m.snow <- with(m.snow, m.snow[order(as.factor(as.character(Station)), as.factor(as.character(Yr_Month))), ])
head(m.snow)
tail(m.snow)
table(is.na(m.snow$Snow))


## Now have correct NUMBER of rows (excluding months when cameras were not deployed)

m.snow[duplicated(m.snow), ] #no duplicates


## III. Add 0s for months when camera was not deployed (12 for Algar25-60, 24 for Algar61-73)
##a. Algar25:60
Algar <- rep("Algar", 36)
Algarnum <- 25:60
Station <- paste(Algar, Algarnum, sep="")
class(Station)
Station <- as.factor(Station)
Station <- rep(Station, 12) # 12 = no. months
#Order Station to avoid causing duplicate rows (e.g. matching all Algar25 with all 2016-11)
Station <- sort(Station)
Station
Yr_Month <- c("2015-11", "2015-12", "2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06", "2016-07", "2016-08", "2016-09", "2016-10")
Yr_Month <- rep(Yr_Month, 36) #36 = no. of cameras
Snow <- rep(NA,432)
# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)
## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

m.snow <- with(m.snow, m.snow[order(as.factor(as.character(Station)), as.factor(as.character(Yr_Month))), ])
m.snow[duplicated(m.snow),]
head(m.snow)
tail(m.snow)

## Algar61:73
Algar <- rep("Algar", 13)
Algarnum <- 61:73
Station <- paste(Algar, Algarnum, sep="")
class(Station)
Station <- as.factor(Station)
Station <- rep(Station, 24) # 24 = no. months
#Order Station to avoid causing duplicate rows (e.g. matching all Algar25 with all 2016-11)
Station <- sort(Station)
Station
Yr_Month <- c("2015-11", "2015-12", "2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06", "2016-07", "2016-08", "2016-09", "2016-10", "2016-11", "2016-12", "2017-01", "2017-02", "2017-03", "2017-04", "2017-05", "2017-06", "2017-07", "2017-08", "2017-09", "2017-10")
Yr_Month <- rep(Yr_Month, 13) #13 = no. of cameras
Snow <- rep(NA,312)
# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)
## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

m.snow <- with(m.snow, m.snow[order(as.factor(as.character(Station)), as.factor(as.character(Yr_Month))), ])
m.snow[duplicated(m.snow), ]
head(m.snow)
tail(m.snow)
table(is.na(m.snow$Snow))
m.snow[duplicated(m.snow), ]

#### Add to monthly detection data sets - MonthlyDetections_nov2015-apr2018.csv AND Seismic_nov2015-apr2018.csv ####
## Check correlation with ActiveDays
getwd()
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
DF <- read.csv("MonthlyDetections_nov2015-apr2018.csv")
seis <- read.csv("Seismic_nov2015-apr2018.csv")
summary(DF)

## Create Site_ym column in m.snow
m.snow$Site_ym <- paste(m.snow$Station,m.snow$Yr_Month, sep=" ")

## Add ActiveDays to m.snow, calculate, proportion of snowdays, save, then re-script m.snow with NAs instead of zeroes to compare
m.snow$ActiveDays <- DF$ActiveDays[match(m.snow$Site_ym, DF$Site_ym)]

m.snow$pSnow <- m.snow$Snow/m.snow$ActiveDays
summary(m.snow$pSnow) #977 NAs
write.csv(m.snow, "SnowDays_permonth_NAs.csv")
table(m.snow$pSnow) #shouldn't have values >1, but do (rectified)
m.snow[which(m.snow$pSnow>1), ] #Counts that are different by 1 is an accounting error, ActiveDays should be adjusted (in some cases SnowDays; need to assess each case independently. See lab notebook for May 11,2018) ---> Rectified in dataframes

#Converting NAs in Snow to 0s
ms <- m.snow[ ,1:5]
table(is.na(ms$Snow)) #970 TRUE
summary(ms$Snow)
ms$Snow[is.na(ms$Snow)] <- 0
summary(ms$Snow) #No NAs, but summer months have equivalent value to fail months
table(is.na(ms$Snow)) #FALSE 2190
table(ms$Snow==0) ## 1299 TRUE--> Both No Snow months and NA months
m.snow$Site_ym[duplicated(m.snow$Site_ym)]

ms$pSnow <- ms$Snow/ms$ActiveDays
ms[duplicated(ms),]
summary(ms$pSnow) #968 NAs --> only 9 fewer than coding as NAs

plot(ms$ActiveDays,ms$pSnow) ## No correlation


#### Test model with pSnow from NA-coded snow
## Add SnowDays to DF
DF$SnowDays <- m.snow$Snow[match(DF$Site_ym, m.snow$Site_ym)]
summary(DF$SnowDays)

plot(DF$ActiveDays,DF$SnowDays)
A <- lm(SnowDays~ActiveDays,data=DF)
summary(A) #low R^2, but ActiveDays has significantly positive effect on SnowDays.

## Add pSnow to DF
DF$pSnow <- m.snow$pSnow[match(DF$Site_ym, m.snow$Site_ym)]
summary(DF$pSnow)

## Add SnowDays and pSnow to seis
seis$SnowDays <- m.snow$Snow[match(seis$Site_ym, m.snow$Site_ym)]
seis$pSnow <- m.snow$pSnow[match(seis$Site_ym, m.snow$Site_ym)]

library(glmmTMB)
library(bbmle)
#Compare results with pSnow and SnowDays
L13 <- glmmTMB(Caribou~ Treatment + pSnow + (1|Site) + (1|Month), data = seis, zi=~ActiveDays, family = nbinom1)
summary(L13)
L14 <- glmmTMB(Caribou~ Treatment + SnowDays + (1|Site) + (1|Month), data = seis, zi=~ActiveDays, family = nbinom1)
summary(L14)
## Same significance level, different parameter estimates


#### Test model with pSnow from 0-coded snow
## Add SnowDays to DF
DF$SnowDays <- ms$Snow[match(DF$Site_ym, ms$Site_ym)]
summary(DF$SnowDays)

plot(DF$ActiveDays,DF$SnowDays)
A <- lm(SnowDays~ActiveDays,data=DF)
summary(A) # Higher R^2 than when coded as NAs (makes sense as 0s in Snow correlate with 0s in ActiveDays)

## Add pSnow to DF
DF$pSnow <- ms$pSnow[match(DF$Site_ym, ms$Site_ym)]
summary(DF$pSnow)
A1 <- lm(pSnow~ActiveDays, data=DF)
summary(A1) # R^2= 0.007
plot(DF$ActiveDays,DF$pSnow)

## Add SnowDays and pSnow to seis
seis$SnowDays <- ms$Snow[match(seis$Site_ym, ms$Site_ym)]
seis$pSnow <- ms$pSnow[match(seis$Site_ym, ms$Site_ym)]


#Compare results with pSnow and SnowDays
L15 <- glmmTMB(Caribou~ Treatment + pSnow + (1|Site) + (1|Month), data = seis, zi=~ActiveDays, family = nbinom1)
summary(L13)
L16 <- glmmTMB(Caribou~ Treatment + SnowDays + (1|Site) + (1|Month), data = seis, zi=~ActiveDays, family = nbinom1)
summary(L14)

## Models run with both NA-coded fail months and 0-coded fail months, when Snow is measured both as a total number of days and as a proportion of ActiveDays
## I will continue to measure Snow as a proportion because it accounts for unequal days per month and accounts for differences in ActiveDays (removes correlation)
## I will continue to code fail months as 0s instead of NA to remain consistent with camera detection data

summary(DF$SnowDays) #No NAs
summary(DF$pSnow)
summary(seis$SnowDays)
summary(seis$pSnow)

write.csv(DF, "MonthlyDetections_nov2015-apr2018.csv")
write.csv(seis, "Seismic_nov2015-apr2018.csv")
