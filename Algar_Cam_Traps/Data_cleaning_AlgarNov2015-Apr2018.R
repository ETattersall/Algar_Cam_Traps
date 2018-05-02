##############################################
# Data_cleaning_AlgarNov2015-Apr2018.R
## Raw data clean-up, Nov 2017 - April 2018
## camtrapR recordTable exported from Camelot, includes both summer 2017 and winter 2017-18 deployments
## Started April 30, 2018
#############################################

library(plyr)
library(dplyr) #only load after using revalue function
library(camtrapR)
library(reshape2)	# for formatting data frames
# library(plyr) need for renaming Treatments for consistency, but conflicts with dplyr
library(dplyr)		# for applying functions to subsets of data frames
library(ggplot2)	# for data visualization
library(stringr)	# for working with character strings
library(tidyr)		# for data formatting functions
library(knitr)		# for the "kable" function for formatting tables


setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

# Camelot record table
rc <- read.csv("recordTable_apr2017-2018_raw.csv")
str(rc)
head(rc)
tail(rc)
summary(rc)

## Algar deployment data
cams <- read.csv("Station_data/AlgarStations_DeploymentData.csv")
str(cams) ## Session4, Problem3s
head(cams)

## Camera active days - should match number of Timelapse images
## Recorded in SurveyEffort_summaries.docx (as of Apr. 30, in Data Analysis, Protocols --> Figures)
# First re-calculate for Apr-Nov 2017 (Algar 43 added): to do this need to isolate first 60 cams
cams60 <- cams[1:60,] %>% select(CamStation, utmE, utmN, Treatment, Session3Start, Problem2_from, Problem2_to,Session4Start)
camEff <- cameraOperation(cams60, 
                          stationCol = "CamStation", 
                          setupCol = "Session3Start", 
                          retrievalCol = "Session4Start",
                          hasProblems = TRUE,
                          dateFormat = "%d/%m/%Y", 
                          writecsv = FALSE)

sum(camEff,na.rm=T) ##7767

# days per station
summary(rowSums(camEff,na.rm=T)) # mean = 129.4, median = 203, range = 0 - 206

# Nov 2017 - Apr 2018
cams2018 <- cams %>% select(CamStation, utmE, utmN, Treatment, Session4Start, Problem3_from, Problem3_to,Session5Start)
camEff2 <- cameraOperation(cams2018, 
                          stationCol = "CamStation", 
                          setupCol = "Session4Start", 
                          retrievalCol = "Session5Start",
                          hasProblems = TRUE,
                          dateFormat = "%d/%m/%Y", 
                          writecsv = FALSE)

sum(camEff2,na.rm=T) ##8717 --> discrepancy with effort from Timelapse images (8555) could be from time of day when cameras were checked (affects camera being recorded as ACTIVE in Timelapse estimation)

# days per station
summary(rowSums(camEff2,na.rm=T))

#### Total survey effort calculations
cams_survey <- read.csv("Station_data/AlgarStations_FullSurveyDeploymentData.csv")
camEff3 <- cameraOperation(cams_survey, 
                           stationCol = "CamStation", 
                           setupCol = "Session1Start", 
                           retrievalCol = "Session5Start",
                           hasProblems = TRUE,
                           dateFormat = "%d/%m/%Y", 
                           writecsv = FALSE)

sum(camEff3,na.rm=T) ## 34, 277 camera days
summary(rowSums(camEff3,na.rm=T)) ## mean = 469.5, median = 432.0, range = 121 - 886

## Cams on lines only

seiscams <- cams_survey[1:60,]
camEff4 <- cameraOperation(seiscams, 
                           stationCol = "CamStation", 
                           setupCol = "Session1Start", 
                           retrievalCol = "Session5Start",
                           hasProblems = TRUE,
                           dateFormat = "%d/%m/%Y", 
                           writecsv = FALSE)

sum(camEff4,na.rm=T) ## 32, 371 camera days
summary(rowSums(camEff4,na.rm=T)) #mean = 539.5 median = 513, range = 158 - 886

#### Data cleaning: recordTable ####
table(rc$Species)
summary(rc$Date)
### Current Camelot recordTable includes 2 sessions: Apr - Nov 2017, and Nov 2017 - Apr 2018
### Need recordTable for Nov 2017-Apr2018 only to count animal detections in this session
### remove entries if they match the earlier Nov - Apr 2017 recordTable
rc2017 <- read.csv("2017.01_rawrecordTable.csv")

## Nov 2017 - Apr 2018 recordTable 
rc2017.02 <- anti_join(rc, rc2017, by = "FileName") #FileName is unique to the detection event --> assigned in Camelot
# rc2017.02 entries = rc - rc2017

str(rc2017.02)
summary(rc2017.02$DateTimeOriginal)
rc2017.02 <- rc2017.02[order(rc2017.02$DateTimeOriginal),]
head(rc2017.02)
tail(rc2017.02)
# Still includes Algar43 and a few other files not present on first recordTable
# First 41 detections occur before session4
## Save rc2017.02, including events occuring in Session3
write.csv(rc2017.02, "2017.02_rawrecordTable.csv")

# 685 - 41 = 644 independent detections in Session4
rc2017.02 <- rc2017.02[42:685,]
table(rc2017.02$Species) ## 11 non-human mammal species detected

## Continue working with rc --> recordTable for Apr 2017 - Apr 2018. This ensures that Algar43 detections are included
rc <- rc[order(rc$Station, rc$DateTimeOriginal), ]
str(rc)
summary(rc$Species)

## Remove No Animal and Unknown
rc <- rc[!rc$Species == "No Animal", ]
rc <- rc[!rc$Species == "Unknown species", ]


## Change known birds to bird spp in 2017
rc$Species <- gsub("Perisoreus canadensis", "Bird spp.", rc$Species)
rc$Species <- gsub("Colaptes auratus", "Bird spp.", rc$Species)
rc$Species <- gsub("Tympanuchus phasianellus", "Bird spp.", rc$Species)
rc$Species <- gsub("Strix nebulosa", "Bird spp.", rc$Species)
rc$Species <- gsub("Canachites canadensis", "Bird spp.", rc$Species)
rc$Species <- gsub("Branta canadensis", "Bird spp.", rc$Species)
rc$Species <- gsub("Melospiza melodia", "Bird spp.", rc$Species)
rc$Species <- gsub("Haemorhous purpureus", "Bird spp.", rc$Species)

table(rc$Species)

## Remove additional rows from Camelot record table (Camera, CameraName, TrapAndCamera)
rc$Camera <- NULL
rc$CameraName <- NULL
rc$TrapAndCamera <- NULL

## Combining to first 2 deployments (recordTable from Timelapse = rt)
rt <- read.csv("recordTable_nov2015-apr2017.csv")
str(rt)
rt$X.1 <- NULL
rt$X <- NULL
str(rt)

## Bind all record tables together
All.rec <- rbind.data.frame(rt, rc, deparse.level = 0)
unique(All.rec$Species) ## 19 categories, 16 non-human mammals
unique(All.rec$Station) ## Algar73 contained no wildlife detections, though it was active

## Date. Time conversions to POSIX
All.rec$Date.Time <- as.POSIXct(strptime(All.rec$DateTimeOriginal, format = "%Y-%m-%d %H:%M:%S"))
All.rec$Datep <- as.POSIXct(strptime(All.rec$Date, format = "%Y-%m-%d"))
str(All.rec)

##Save copy of full record table
write.csv(All.rec, "AlgarRecordTable_nov2015-apr2018.csv")

# Keeping in mind that for my Ch.1 analysis I will need to exclude off line sites
# Offline sites
offline <- All.rec[which(All.rec$Station == "Algar61" | All.rec$Station == "Algar62" | All.rec$Station == "Algar63" | All.rec$Station == "Algar64" | All.rec$Station == "Algar65" | All.rec$Station == "Algar66" | All.rec$Station == "Algar67" | All.rec$Station == "Algar68" | All.rec$Station == "Algar69" | All.rec$Station == "Algar70" | All.rec$Station == "Algar71" | All.rec$Station == "Algar72"), ]

## Remove offline from record table 
seismic.rec <- anti_join(All.rec, offline, by = "FileName")
table(seismic.rec$Species)

### Frequency histogram for species detections
sp_detect <- All.rec$Species
st_detect <- All.rec$Station

sp.plot1 <- rev(sort(table(sp_detect)))
sp.plot1 <- as.data.frame(sp.plot1) ##data frame summing detections --> fix scientific names to common
fix(sp.plot1)
colnames(sp.plot1) <- c("Species", "Total Detections")



### Frequency histograms of mammal species

par(mfrow = c(1,1))## Multiple plots on same page (2 rows, 1 column)

ggplot(data = sp.plot1, aes(x = sp_detect, y = Freq)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Total Detections") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("White-tailed deer", "Black bear", "Grey wolf", "Snowshoe hare",  "Moose", "Coyote", "Woodland caribou",  "Red squirrel", "Canada lynx","American marten", "Red fox", "River otter", "Wolverine", "Fisher", "Beaver"))

########### Convert to monthly detections ####
## Use recordTable of FULL SURVEY (All.rec)
## includes seismic and offline --> will remove offline as last step, prior to modelling


All.rec$Date.Time <- as.POSIXct(strptime(All.rec$DateTimeOriginal, format = "%Y-%m-%d %H:%M:%S"))
All.rec$Datep <- as.POSIXct(strptime(All.rec$Date, format = "%Y-%m-%d"))
str(All.rec)

## Copied code uses object 'data' for record table (copied from 'Algar_prelim_analysis_ch1.R)
data <- All.rec
data <- read.csv("AlgarRecordTable_nov2015-apr2018.csv")
str(data)
data$Date.Time <- as.POSIXct(strptime(data$DateTimeOriginal, format = "%Y-%m-%d %H:%M:%S"))
data$Datep <- as.POSIXct(strptime(data$Date, format = "%Y-%m-%d"))


# first date of detection per site - ranges from 2015-11-05 to 2018-03-19 (offline site)
mindate <- aggregate(Date.Time~Station, data, function(x) min(x))
head(mindate)
mindate.ord <- mindate[order(mindate$Date.Time, mindate$Station),] ## Order first detections by date
head(mindate.ord)
tail(mindate.ord)
mindate.ord <- mindate[order(mindate$Station, mindate$Date.Time),] ## Order first detections by Station
mindate.ord 

maxdate <- aggregate(Date.Time~Station, data, function(x) max(x))
maxdate.ord <- maxdate[order(maxdate$Date.Time, maxdate$Station),] # last date of detection per site - ranges from 2017-04-19 to 2018-04-08
head(maxdate.ord)
tail(maxdate.ord)
maxdate.ord <- maxdate[order(maxdate$Station, maxdate$Date.Time),]
maxdate.ord


maxdate.ord[,2] - mindate.ord[,2] # (Needs to be ordered by Station here)
# Time differences in secs


# convert DateStart (but note that this doesn't have time set, so will treat as midnight)

data$DateStart <- min(mindate.ord[,2]) #Adding the date of first detection to record Table
data$DateStart <- (strptime(data$DateStart, "%Y-%m-%d", tz="MST")) ##Adding a column for the first day of the study
DateStart <- min(data$DateStart) # The first detection from all stations - 2015-11-05 - first detection of the deployment
str(data)

# calculate a unique day for each day of study
# taking straight difference will include partial days, so is really 24-hour periods from time first camera set
# since there is no start time entered, this should work from hour 0, so should be same as calendar day
# using "floor" so it doesn't round up to next day
data$StudyDay <- floor(as.numeric(difftime(data$Date.Time,min(data$DateStart),units="days"))) ##Takes difference between the detection date and start date, without rounding up
head(data)
summary(data$StudyDay) #1 NA
data[is.na(data$StudyDay),] ## NA StudyDay occurs on 2018-03-11...Date.Time is NA, unknown why. Fix manually
data$Date.Time[is.na(data$Date.Time)] <- "2018-03-11 02:26:52" #re-ran code to calculate StudyDay, no NAs
data[which(data$StudyDay==0),]
data$StudyDay <- data$StudyDay+1 #Turns start date into day 1, not day 0
summary(data$StudyDay) #

### Adding Treatment Column by matching with station data (IMPORTANT NOTE: Algar07 and Algar34 lines were cleared, but cameras had already failed, so this did not affect the Nov - Apr 2018 deployment. Treatments will need to be adjusted in November 2018)
data$Treatment <- cams$Treatment[match(data$Station, cams$CamStation)]
str(data)

### use reshape2 package to create vector of all study days per site
# sum the number of independent-detections for each Site on each StudyDay
# this creates dataframe, so Site is first column
# only includes sites and days with species detections

max(data$StudyDay)
study.days <- 1:max(data$StudyDay) #886 study days
sites <- unique(data$Station)
sites <- sort(sites, decreasing=FALSE)
length(sites) #71 sites - Algar64, and 73 have no detections

Site_rep <- rep(sites,886) ## Repeat site names 355 times (# days in study period)
length(Site_rep)
head(Site_rep)

length(unique(data$Station)) #71 stations with detections
study.days_rep <- rep(study.days, 71) ## Repeat study days 70 times (# of camera sites with detections)
study.days_rep <- sort(study.days_rep, decreasing=FALSE)
length(study.days_rep)

d <- cbind(as.data.frame(Site_rep),study.days_rep) ## Creates a data frame of sites and study day
d$Site_SD <- as.factor(paste(d$Site_rep, d$study.days_rep)) ##New column with both site and study day
summary(d)


data$count <- 1 #add count column - 1 detection per row (does not take multiple individuals/detection into consideration)
names(data)
data2 <- data[c(1,2,15:17)] ## new data frame only containing site, species, study day, treatment, and detection count
head(data2)
str(data2)
# data2$Species <- as.factor(data2$Species) # If Species is listed as character

levels(data2$Species) ##Names of different species detected

#### Create individual species data frames --> # detections per day #### 
## if summarise function only gives a 1 by 1 output that sums the count column: close R and reload with dplyr loaded only (not plyr)

d.deer <- data2 %>%
  filter(Species == "Odocoileus virginianus") %>% ##Select deer data only
  group_by(Station, StudyDay) %>%        ## Arrange data first by Station, then by StudyDay (both in ascending order)
  summarise(sum(count, na.rm = TRUE))    ## Summarise the detection count for that day, add to new column in data frame
colnames(d.deer) <- c("Site","StudyDay","WTDeer") ##Renaming columns (ignore Warnings, function will add colname)
glimpse(d.deer) #262 days of deer detections (where some days have multiple detections)
plot(d.deer$StudyDay, d.deer$WTDeer, xlim=c(0,900)) ##Mostly 1 detection/day, up to 7

d.bear <- data2 %>%
  filter(Species == "Ursus americanus") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.bear) <- c("Site","StudyDay","Blackbear")
glimpse(d.bear) #178 obs
plot(d.bear$StudyDay, d.bear$Blackbear, xlim=c(0,900)) #Up to 4 detections in a day, 2 summer seasons



d.caribou <- data2 %>%
  filter(Species == "Rangifer tarandus") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.caribou) <- c("Site","StudyDay","Caribou")
glimpse(d.caribou) #76
plot(d.caribou$StudyDay, d.caribou$Caribou, xlim=c(0,900)) # As many as 3 in a day (once)

d.moose <- data2 %>%
  filter(Species == "Alces alces") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.moose) <- c("Site","StudyDay","Moose")
glimpse(d.moose) #112 obs
plot(d.moose$StudyDay, d.moose$Moose, xlim=c(0,900)) # As many as 3 in a day

d.coyote <- data2 %>%
  filter(Species == "Canis latrans") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.coyote) <- c("Site","StudyDay","Coyote")
glimpse(d.coyote) #40 obs
plot(d.coyote$StudyDay, d.coyote$Coyote, xlim=c(0,900)) #1 3-det. and 1 4-det. data point

d.lynx <- data2 %>%
  filter(Species == "Lynx canadensis") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.lynx) <- c("Site","StudyDay","Lynx")
glimpse(d.lynx) # 15 obs
plot(d.lynx$StudyDay, d.lynx$Lynx, xlim=c(0,900)) # only 1-detection days

d.wolf <- data2 %>%
  filter(Species == "Canis lupus") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.wolf) <- c("Site","StudyDay","Wolf")
glimpse(d.wolf) # 121 obs
plot(d.wolf$StudyDay, d.wolf$Wolf, xlim=c(0,900)) #2 3-det. days, many 1 and 2

## Add in other species of interest --> Humans, snowshoe hare?
d.hare <- data2 %>%
  filter(Species == "Lepus americanus") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.hare) <- c("Site","StudyDay","Hare")
glimpse(d.hare) # 121 obs
plot(d.hare$StudyDay, d.hare$Hare, xlim=c(0,900)) #up to 4 detections/day

d.human <- data2 %>%
  filter(Species == "Homo sapiens") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.human) <- c("Site","StudyDay","Human")
glimpse(d.human) # 121 obs
plot(d.human$StudyDay, d.human$Human, xlim=c(0,900)) #up to 4 detections/day

#### Adding species detections counts to a master data frame ####

names(d)

##Creating column of Station and StudyDay in data2 dataframe
data2$Site_SD <- as.factor(paste(data2$Station,data2$StudyDay))
data3 <- d
colnames(data3) <- c("Site","StudyDay","Site_SD")
data3$Treatment <- data2$Treatment[match(data3$Site,data2$Station)]
str(data3)
head(data3)

d.deer$Site_SD <- paste(d.deer$Site,d.deer$StudyDay) ## Adding Site/studyDay column to relate detection data between data frames
data3$WTDeer <- d.deer$WTDeer[match(data3$Site_SD,d.deer$Site_SD)] ##Matching Site_SD between data frames 
##NAs on Sites/Days with no detections

d.bear$Site_SD <- paste(d.bear$Site,d.bear$StudyDay)
data3$Blackbear <- d.bear$Blackbear[match(data3$Site_SD,d.bear$Site_SD)]

d.caribou$Site_SD <- paste(d.caribou$Site,d.caribou$StudyDay)
data3$Caribou <- d.caribou$Caribou[match(data3$Site_SD,d.caribou$Site_SD)]

d.moose$Site_SD <- paste(d.moose$Site,d.moose$StudyDay)
data3$Moose <- d.moose$Moose[match(data3$Site_SD,d.moose$Site_SD)]

d.coyote$Site_SD <- paste(d.coyote$Site,d.coyote$StudyDay)
data3$Coyote <- d.coyote$Coyote[match(data3$Site_SD,d.coyote$Site_SD)]

d.lynx$Site_SD <- paste(d.lynx$Site,d.lynx$StudyDay)
data3$Lynx <- d.lynx$Lynx[match(data3$Site_SD,d.lynx$Site_SD)]

d.wolf$Site_SD <- paste(d.wolf$Site,d.wolf$StudyDay)
data3$Wolf <- d.wolf$Wolf[match(data3$Site_SD,d.wolf$Site_SD)]

d.hare$Site_SD <- paste(d.hare$Site,d.hare$StudyDay)
data3$Hare <- d.hare$Hare[match(data3$Site_SD,d.hare$Site_SD)]

d.human$Site_SD <- paste(d.human$Site,d.human$StudyDay)
data3$Human <- d.human$Human[match(data3$Site_SD,d.human$Site_SD)]

summary(data3)

data3[is.na(data3)] <- 0 ## Converting NAs to 0's
summary(data3)
nrow(data3)

sum(data3$WTDeer) #687
sum(data3$Blackbear) #360
sum(data3$Caribou) #149
sum(data3$Coyote) #154
sum(data3$Lynx) #73
sum(data3$Wolf) #334
sum(data3$Moose) #196
sum(data3$Hare) #211
sum(data3$Human) #94

####--- aggregate species detection data by month ####
data$Site_SD <- paste(data$Station,data$StudyDay)
data3$StudyDay.date <- as.Date(data3$StudyDay,origin = "2015-11-04") # Origin = day before first detection

data3$Year <- as.factor(format(as.Date(data3$StudyDay.date), "%Y")) ##Separating date info into year, month, and year_month (to distinguish Nov.2015 from Nov. 2016)
data3$Month <- as.factor(format(as.Date(data3$StudyDay.date), "%b"))
data3$Yr_Month <- as.factor(format(as.Date(data3$StudyDay.date), "%Y-%m"))

head(data3)
summary(data3)
levels(data3$Yr_Month)

sum(data3$WTDeer)
sum(data3$Blackbear)
sum(data3$Caribou) 
sum(data3$Coyote) 
sum(data3$Lynx) 
sum(data3$Wolf) 
sum(data3$Moose) 

m.deer <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(WTDeer, na.rm = TRUE))
colnames(m.deer) <- c("Site","Treatment","Yr_Month","WTDeer")
summary(m.deer) 
sum(m.deer$WTDeer)

m.bear <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Blackbear, na.rm = TRUE))
colnames(m.bear) <- c("Site","Treatment","Yr_Month","Blackbear")
summary(m.bear) 
sum(m.bear$Blackbear)

m.caribou <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Caribou, na.rm = TRUE))
colnames(m.caribou) <- c("Site","Treatment","Yr_Month","Caribou")
summary(m.caribou) 
sum(m.caribou$Caribou) 

m.moose <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Moose, na.rm = TRUE))
colnames(m.moose) <- c("Site","Treatment","Yr_Month","Moose")
summary(m.moose) 
sum(m.moose$Moose) 


m.coyote <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Coyote, na.rm = TRUE))
colnames(m.coyote) <- c("Site","Treatment","Yr_Month","Coyote")
summary(m.coyote) 
sum(m.coyote$Coyote) 

m.lynx <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Lynx, na.rm = TRUE))
colnames(m.lynx) <- c("Site","Treatment","Yr_Month","Lynx")
summary(m.lynx) 
sum(m.lynx$Lynx) 

m.wolf <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Wolf, na.rm = TRUE))
colnames(m.wolf) <- c("Site","Treatment","Yr_Month","Wolf")
summary(m.wolf) # max 19 obs in one month
sum(m.wolf$Wolf) 

m.hare <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Hare, na.rm = TRUE))
colnames(m.hare) <- c("Site","Treatment","Yr_Month","Hare")
summary(m.hare) 
sum(m.hare$Hare)

m.human <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Human, na.rm = TRUE))
colnames(m.human) <- c("Site","Treatment","Yr_Month","Human")
summary(m.human) # max 19 obs in one month
sum(m.human$Human) 

#### Aggregating all monthly detection data into one data frame ####
data.month <- m.wolf ##Starting new dataframe with rows for all stations in increments by yr_month
data.month$Wolf <- NULL #Removing wolf column for now (add back in later with all other species detections)
data.month$Site_ym <- paste(data.month$Site,data.month$Yr_Month) ##Create a site and year/month column for matching with monthly species detections

m.bear$Site_ym <- paste(m.bear$Site,m.bear$Yr_Month)
data.month$Blackbear <- m.bear$Blackbear[match(data.month$Site_ym,m.bear$Site_ym)]

m.wolf$Site_ym <- paste(m.wolf$Site,m.wolf$Yr_Month)
data.month$Wolf <- m.wolf$Wolf[match(data.month$Site_ym,m.wolf$Site_ym)]

m.coyote$Site_ym <- paste(m.coyote$Site,m.coyote$Yr_Month)
data.month$Coyote <- m.coyote$Coyote[match(data.month$Site_ym,m.coyote$Site_ym)]

m.lynx$Site_ym <- paste(m.lynx$Site,m.lynx$Yr_Month)
data.month$Lynx <- m.lynx$Lynx[match(data.month$Site_ym,m.lynx$Site_ym)]

m.caribou$Site_ym <- paste(m.caribou$Site,m.caribou$Yr_Month)
data.month$Caribou <- m.caribou$Caribou[match(data.month$Site_ym,m.caribou$Site_ym)]

m.deer$Site_ym <- paste(m.deer$Site,m.deer$Yr_Month)
data.month$WTDeer <- m.deer$WTDeer[match(data.month$Site_ym,m.deer$Site_ym)]

m.moose$Site_ym <- paste(m.moose$Site,m.moose$Yr_Month)
data.month$Moose <- m.moose$Moose[match(data.month$Site_ym,m.moose$Site_ym)]

m.hare$Site_ym <- paste(m.hare$Site,m.hare$Yr_Month)
data.month$Hare <- m.hare$Hare[match(data.month$Site_ym,m.hare$Site_ym)]

m.human$Site_ym <- paste(m.human$Site,m.human$Yr_Month)
data.month$Human <- m.human$Human[match(data.month$Site_ym,m.human$Site_ym)]

sum(data.month$WTDeer)
sum(data.month$Blackbear) 
sum(data.month$Caribou) 
sum(data.month$Coyote) 
sum(data.month$Lynx) 
sum(data.month$Wolf) 
sum(data.month$Moose) 
sum(data.month$Hare)
sum(data.month$Human)
summary(data.month)
glimpse(data.month)

head(data.month)
str(data.month)

## data.month is missing counts for cameras that had no detections--> Algar64 and 73
## Algar64 and Algar73 had no detections. Need to add rows for months they weren't set up (Nov. 2015 -Nov. 2017) and months they were active (Nov. 2017 - Apr. 2018)
##Dataframe of 12x11
# Site column
Algar64 <- rep("Algar64",30)
Algar73 <- rep("Algar73",30)
Site <- append(Algar64,Algar73)

# Treatment and Yr_Month columns
Treatment <- rep("OffLine", "60")
Yr_Month <- rep(c("2015-11", "2015-12", "2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06", "2016-07", "2016-08", "2016-09", "2016-10", "2016-11", "2016-12", "2017-01", "2017-02", "2017-03", "2017-04", "2017-05", "2017-06", "2017-07", "2017-08", "2017-09", "2017-10", "2017-11", "2017-12", "2018-01", "2018-02", "2018-03", "2018-04"),2)

# Combining as a dataframe
nodet <- as.data.frame(cbind(Site, Treatment, Yr_Month))
# Site_ym column
nodet$Site_ym <- paste(nodet$Site, nodet$Yr_Month)

#Detection matrix full of 0's
detections <- matrix(data = 0, nrow = 60, ncol = 9)
colnames(detections) <- c("Blackbear", "Wolf", "Coyote", "Lynx", "Caribou", "WTDeer", "Moose", "Hare", "Human")
detections <- as.data.frame(detections)

#Full data.frame for 0 detection sites
nodet <- cbind(nodet,detections)

#Add nodet to data.month
data.month <- bind_rows(data.month, nodet)
data.month <- with(data.month, data.month[order(as.factor(as.character(Site)), Yr_Month), ]) ## Ordering by Site and Yr_month. as.factor(as.character()) rearranges the levels in Site
tail(data.month)
str(data.month)
data.month$Site <- as.factor(data.month$Site)
data.month$Treatment <- as.factor(data.month$Treatment)
data.month$Site_ym <- as.factor(data.month$Site_ym)
str(data.month)
class(data.month$Treatment)

## Separating elements from Yr_Month
data.month$Yr_Month2 <- data.month$Yr_Month #replicating Yr_Month for separation
data.month <- cbind(data.month[,1:12],
                    colsplit(data.month$Yr_Month2, "[-]", names=c("Year", "Month")))

write.csv(data.month, "MonthlyDetections_nov2015-apr2018.csv")


######## Need to account for months during which cameras were inactive -- currently entered as 0's
## Convert to NA
## Previously done for Apr - Nov data in cameraInactiveDays.R, but this method risked losing data in partial months
## Add number of active days to monthly detection data instead

### Camera operability matrix for full survey = camEff3 (from cams_survey, see above)
dim(camEff3) #73 cameras, 889 days

camEff3[camEff3 == 0] <- NA # changes all 0 to NA, as 0 is not operational

camEff3 <- camEff3[order(rownames(camEff3)), ]  # order alphabetically by Location (same as camdf)
cams_survey <- cams_survey[order(cams_survey$CamStation), ]   # order alphabetically by Location (same as camEff3)

sum(camEff3, na.rm=TRUE) #34 277 camera trap days - total effort for survey to date
sum.active.camera <- rowSums(camEff3, na.rm = TRUE)
min(sum.active.camera) #121
max(sum.active.camera) #886
mean(sum.active.camera) #469.4

#############################################################
###--- not all cameras were operational for entire study period
###--- range of detection timing depend on location

## Converting to POSIXct dates
cams_survey$Session1Start <- as.POSIXct(strptime(cams_survey$Session1Start, format = "%d/%m/%Y"))
cams_survey$Session5Start <- as.POSIXct(strptime(cams_survey$Session5Start, format = "%d/%m/%Y"))

max(cams_survey$Session5Start - cams_survey$Session1Start) # 887 max number of days cameras were out

## StudyDay recorded in 'data' (full sruvey recordTable)
summary(data$StudyDay)# 1 - 886

## Creating a Day Look-up matching StudyDay to date
DayLookup <- array(0,dim=c(max(data$StudyDay),2))
colnames(DayLookup) <- c("StudyDay", "Datep")
dim(DayLookup)

DayLookup <- as.data.frame(DayLookup)
DayLookup$StudyDay <- seq.int(max(data$StudyDay))
DayLookup$Datep <- seq(min(data$DateStart), max(data$Datep), by="days")

DayLookup$Datep <- as.Date(DayLookup$Datep, format="%Y-%m-%d")

DayLookup$Year <- as.factor(format(as.Date(DayLookup$Datep),"%Y"))
DayLookup$Month <-  as.factor(format(as.Date(DayLookup$Datep), "%m"))
DayLookup$Week <- as.factor(format(as.Date(DayLookup$Datep),"%V"))

DayLookup$YrMonth <- as.factor(paste(DayLookup$Year, DayLookup$Month, sep="-"))
DayLookup$YrWeek <- as.factor(paste(DayLookup$Year, DayLookup$Week, sep="-"))

summary(DayLookup)
glimpse(DayLookup)
write.csv(DayLookup,"DayLookup.csv")


#############################################################
###--- Sampling effort (# of hours per camera) per year, per month, per week, per day
glimpse(DayLookup)

# create vectors for grouping variables, all with length 886 to go with study days
#DayLookup correlates StudyDay to date, year, month, week, and year_month
Year <- DayLookup$Year
YrMonth <- DayLookup$YrMonth
YrWeek <- DayLookup$YrWeek
Day <- DayLookup$StudyDay


camEff3t <- t(camEff3[,1:886]) # limit camEff3 to same date range as study day (camera image) data
rownames(camEff3t) <- seq(1:nrow(camEff3t)) # change row names from date to study day
dim(camEff3t) # 886 study days by 73 cameras


## Summing columns according to the time period specified
Year.eff <- t(rowsum(camEff3t, Year, na.rm=TRUE)) # daily effort
YrMonth.eff <- t(rowsum(camEff3t, YrMonth, na.rm=TRUE))
YrWeek.eff <- t(rowsum(camEff3t, YrWeek, na.rm=TRUE))
Day.eff <- t(rowsum(camEff3t, Day, na.rm=TRUE))

## For now, only concernced with Yr.Month.eff
class(YrMonth.eff)
head(YrMonth.eff)
YrMonth.eff <- as.data.frame(YrMonth.eff)
str(YrMonth.eff)
table(is.na(YrMonth.eff)) #No NAs
 
head(YrMonth.eff)

### Gather by Yr_Month to create 2 columns
YrMonth_Active <- as.data.frame(row.names(YrMonth.eff)) #Need column of site names to match Yr_Months after gathering
colnames(YrMonth_Active) <- "Site"
YrMonth_Active <- cbind(YrMonth_Active,YrMonth.eff)
YrMonth_Active <- gather(data = YrMonth_Active, key = "Yr_Month", value = "ActiveDays", -Site, factor_key = TRUE)
str(YrMonth_Active)

### Add Active days to data.month dataframe
YrMonth_Active$Site_ym <- paste(YrMonth_Active$Site, YrMonth_Active$Yr_Month)
data.month$ActiveDays <- YrMonth_Active$ActiveDays[match(data.month$Site_ym, YrMonth_Active$Site_ym)]

## Check in basic plots
plot(data.month$ActiveDays, data.month$WTDeer)
plot(data.month$ActiveDays, data.month$Caribou)
plot(data.month$ActiveDays, data.month$Blackbear)
plot(data.month$ActiveDays, data.month$Wolf)
#Only adding in a few data points that would have been thrown out in inactive months
#data.month should have 30 months for each 73 cameras=2190 rows - yes

data.month$X <- NULL
write.csv(data.month, "MonthlyDetections_nov2015-apr2018.csv")

### Remove offline sites for thesis analyses
seismic <- data.month %>% filter(Treatment != "OffLine")
#13*30=390, 2190-390=1800
tail(seismic)
write.csv(seismic, "Seismic_nov2015-apr2018.csv")
