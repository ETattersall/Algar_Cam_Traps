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

#### Attempt 1: Merge all csvs and fix discrepancies (Date is easier to standaradize on separate CSVs first) ####
#Read csv files into a list
csv.list <- lapply(csv.names, read.csv, sep = ",")
summary(csv.list) # All have 30 variables
head(csv.list) # Each element is a data frame, one from each station
tail(csv.list)

## All dates appear to be standard

csv.names <- list.files(recursive = TRUE) #vector of all file names


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

## Formatting date in POSIXlt
S$Date <- (strptime(S$Date, "%Y-%m-%d", tz="MST"))


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
S$StudyDay <- S$StudyDay+1 #Turns start date into day 1, not day 0
summary(S$StudyDay) # 886 study days


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
## 2017-2018 winter has numerous partial months. Will need to address.
##m.snow has 1214 site-months. Should have (24*30) + (36*18) + (13*6) = 1446. Discrepancy from failed cameras and CSV errors

## 1. Add NAs for months when cameras were not active (inactive during WHOLE MONTH)
# a. Second deployment (in lab notebook, June 2, 2017): 
# Algar18 - inactive 2016 - Dec to 2017 - Apr; value present for 2017-Apr and 2016-Nov are partial and will need to be changed
# Algar32 - inactive 2016 - Dec to 2017 - Apr; value present for 2017-Apr and 2016-Nov are partial and will need to be changed
# Algar49 - inactive 2016 - Dec to 2017 - Apr; value present for 2017-Apr and 2016-Nov are partial and will need to be changed
# Algar50 - inactive 2016 - Dec to 2017 - Apr; value present for 2017-Apr and 2016-Nov are partial and will need to be changed
Algar18 <- rep("Algar18", 4)
Algar32 <- rep("Algar32", 4)
Algar49 <- rep("Algar49", 4)
Algar50 <- rep("Algar50", 4)
Station <- append(x = Algar18, values = c(Algar32, Algar49, Algar50))

#Yr_Month column
Yr_Month <- rep(c("2016-12", "2017-01", "2017-02", "2017-03"), 4)
#Snow column
Snow <- rep(NA,16)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

# Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

# Changing partial months
m.snow$Snow[m.snow$Station=="Algar18" & m.snow$Yr_Month == "2016-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar18" & m.snow$Yr_Month == "2017-04"] <- NA
m.snow$Snow[m.snow$Station=="Algar32" & m.snow$Yr_Month == "2016-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar32" & m.snow$Yr_Month == "2017-04"] <- NA
m.snow$Snow[m.snow$Station=="Algar49" & m.snow$Yr_Month == "2016-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar49" & m.snow$Yr_Month == "2017-04"] <- NA
m.snow$Snow[m.snow$Station=="Algar50" & m.snow$Yr_Month == "2016-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar50" & m.snow$Yr_Month == "2017-04"] <- NA

# b. Third deployment (see Problem Sites): Nov-2017 should be present in data frame and will be addressed later
## 1. Add NA months for failed cameras May - Oct (6 months): Algar07, Algar13, Algar14, Algar51
Algar07 <- rep("Algar07", 6)
Algar13 <- rep("Algar13", 6)
Algar14 <- rep("Algar14", 6)
Algar51 <- rep("Algar51", 6)
Station <- append(x = Algar07, values = c(Algar13, Algar14, Algar51))
#Yr_Month column
Yr_Month <- rep(c("2017-05", "2017-06", "2017-07", "2017-08", "2017-09", "2017-10"),4)
#Snow column
Snow <- rep(NA,24)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

##2. Add NA months for failed cameras Jun - Oct (5 months): Algar09, Algar18, Algar52, Algar55, Algar60
Algar09 <- rep("Algar09", 5)
Algar18 <- rep("Algar18", 5)
Algar52 <- rep("Algar52", 5)
Algar55 <- rep("Algar55", 5)
Algar60 <- rep("Algar60", 5)

Station <- append(x = Algar09, values = c(Algar18, Algar52, Algar55, Algar60))
#Yr_Month column
Yr_Month <- rep(c("2017-06", "2017-07", "2017-08", "2017-09", "2017-10"),5)
#Snow column
Snow <- rep(NA,25)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

##3. Add NA months for failed cameras Jul - Oct (4 months): Algar11, Algar12, Algar19, Algar23, Algar35, Algar42, Algar44, Algar48, Algar57, Algar59
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

Station <- append(x = Algar11, values = c(Algar12, Algar19, Algar23, Algar35, Algar42, Algar44, Algar48, Algar57, Algar59))
#Yr_Month column
Yr_Month <- rep(c("2017-07", "2017-08", "2017-09", "2017-10"),10)
#Snow column
Snow <- rep(NA,40)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

##4. Add NA months for failed cameras Aug - Oct. (3 months): Algar40, Algar58
Algar40 <- rep("Algar40", 3)
Algar58 <- rep("Algar58", 3)

Station <- append(x = Algar40, values = Algar58)
#Yr_Month column
Yr_Month <- rep(c("2017-08", "2017-09", "2017-10"), 2)
#Snow column
Snow <- rep(NA,6)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)


##5. Algar30 (inactive Jul-Oct 2017 - 4 months --> missed before)
Station <- rep("Algar30", 4)
#Yr_Month column
Yr_Month <- c("2017-07", "2017-08", "2017-09", "2017-10")
#Snow column
Snow <- rep(NA,4)

# Combine into data frame of NA snow months
NAmonths <- cbind.data.frame(Station,Yr_Month, Snow)

## Add NA months to m.snow
m.snow <- rbind.data.frame(NAmonths,m.snow)

## Convert all partial 2017-Nov to NA
m.snow$Snow[m.snow$Station=="Algar07" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar13" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar14" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar51" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar09" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar18" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar52" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar55" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar60" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar11" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar12" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar19" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar23" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar35" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar42" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar44" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar48" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar57" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar59" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar40" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar58" & m.snow$Yr_Month == "2017-11"] <- NA
m.snow$Snow[m.snow$Station=="Algar30" & m.snow$Yr_Month == "2017-11"] <- NA

## Order to check progress
head(m.snow) # NAs added to top
tail(m.snow)
m.snow <- with(m.snow, m.snow[order(as.factor(as.character(Station)), as.factor(as.character(Yr_Month))), ])
head(m.snow)
tail(m.snow)
# c. Fourth deployment (see Problem Sites):

##2. Add NAs for ALL partial months (regardless of when camera fails). This will include  deployment month and 2018-04 (2015-11 for 1-24, 2016-11 for 25-60, 2017-11 for 61-73)
# a. In third deployment, if partial month was previously kept because ActiveDays >15, will need to be excluded (See problem sites)

##3. Some stations (noted at top) need to be addressed individually.
## Algar30 nov2017: 4 months of full snow, failed in Feb. (23 days in Nov., 30 in Dec., 30 in Jan., 26 in Feb.). 2 NA months for failing
