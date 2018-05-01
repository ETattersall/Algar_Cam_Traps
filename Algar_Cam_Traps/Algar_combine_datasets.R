#################################################
# Combining Algar survey data sets
# Pilot data (Algar01 - Algar 24) with Winter 2016-17 data (Algar01-60)
# Dec. 8, 2017: Combining record tables from Nov 2016, Apr 2017, and Nov 2017
# Started by Erin T. on Oct. 19, 2017
#################################################

library(plyr)
library(dplyr) #only load after using revalue function


setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

#### Combining record tables for three deployments ####
rec.2015 <- read.csv("2015.01_recordTable.csv")
rec.2016 <- read.csv("2016.01_recordTable.csv")
rec.2017 <- read.csv("2017.01_recordTable.csv")


str(rec.2015) #12variables. Need to change Station names and Date format
str(rec.2016) #12 variables
str(rec.2017) #15 variables
## 1. Rename 2015.01's stations to include 0's (usine plyr function revalue)

rec.2015$Station <- revalue(rec.2015$Station, replace = c("Algar1" = "Algar01", "Algar2" = "Algar02", "Algar3" = "Algar03", "Algar4" = "Algar04", "Algar5" = "Algar05", "Algar6" = "Algar06", "Algar7" = "Algar07", "Algar8" = "Algar08", "Algar9" = "Algar09"))
unique(rec.2015$Station)


# Altering DateTimeOriginal to POSIXct then to Factor to match other 2
rec.2015$DateTimeOriginal <- as.factor(as.POSIXct(strptime(rec.2015$DateTimeOriginal, format = "%d/%m/%Y %H:%M")))
rec.2015$Date <- as.factor(as.Date(rec.2015$Date, "%d/%m/%Y"))
str(rec.2015)


##3. Check and standardize species names
unique(rec.2015$Species)
unique(rec.2016$Species)
unique(rec.2017$Species)

## Combining 2015 and 2016 datasets
All.rec <- rbind.data.frame(rec.2015, rec.2016,deparse.level = 0)
unique(All.rec$Species)

# Change species names in 2015-2016 datasets
All.rec$Species <- gsub("A_alces","Alces alces", All.rec$Species)
All.rec$Species <- gsub("O_virginianus", "Odocoileus virginianus", All.rec$Species)
All.rec$Species <- gsub("U_americanus", "Ursus americanus", All.rec$Species)
All.rec$Species <- gsub("C_latrans","Canis latrans", All.rec$Species)
All.rec$Species <- gsub("C_lupus", "Canis lupus", All.rec$Species)
All.rec$Species <- gsub("G_canadensis", "Grus canadensis", All.rec$Species)
All.rec$Species <- gsub("L_canadensis", "Lynx canadensis", All.rec$Species)
All.rec$Species <- gsub("Other_birds", "Bird spp.", All.rec$Species)
All.rec$Species <- gsub("R_tarandus", "Rangifer tarandus", All.rec$Species)
All.rec$Species <- gsub("V_vulpes","Vulpes vulpes", All.rec$Species)
All.rec$Species <- gsub("L_americanus","Lepus americanus", All.rec$Species)
All.rec$Species <- gsub("M_pennanti","Martes pennanti", All.rec$Species)
All.rec$Species <- gsub("T_hudsonicus","Tamiasciurus hudsonicus", All.rec$Species)
All.rec$Species <- gsub("H_sapiens","Homo sapiens", All.rec$Species)
All.rec$Species <- gsub("G_gulo","Gulo gulo", All.rec$Species)
All.rec$Species <- gsub("P_concolor","Puma concolor", All.rec$Species)
All.rec$Species <- gsub("M_americana","Martes americana", All.rec$Species)

unique(All.rec$Species)

## Remove unknowns (here, just mustelid spp.)
All.rec <- All.rec[!All.rec$Species == "Mustelid spp", ]

str(All.rec)

write.csv(All.rec, "recordTable_nov2015-apr2017.csv")

## Change known birds to bird spp in 2017
rec.2017$Species <- gsub("Perisoreus canadensis", "Bird spp.", rec.2017$Species)
rec.2017$Species <- gsub("Colaptes auratus", "Bird spp.", rec.2017$Species)
rec.2017$Species <- gsub("Tympanuchus phasianellus", "Bird spp.", rec.2017$Species)
rec.2017$Species <- gsub("Strix nebulosa", "Bird spp.", rec.2017$Species)
rec.2017$Species <- gsub("Canachites canadensis", "Bird spp.", rec.2017$Species)
rec.2017$Species <- gsub("Branta canadensis", "Bird spp.", rec.2017$Species)

## Remove Unknown spp
rec.2017 <- rec.2017[!rec.2017$Species == "Unknown species",]

unique(rec.2017$Species)
unique(All.rec$Species)

##4. Remove additional rows from Camelot record table (Camera, CameraName, TrapAndCamera)
rec.2017$Camera <- NULL
rec.2017$CameraName <- NULL
rec.2017$TrapAndCamera <- NULL

## 5. Bind all record tables together
All.rec <- rbind.data.frame(All.rec, rec.2017,deparse.level = 0)
unique(All.rec$Species)
unique(All.rec$Station) ## Missing Algar32 --> malfunctioned both deployments so no detections

write.csv(All.rec, "recordTable_nov2015-nov2017.csv")

str(All.rec)

##2.Standardize date formats (done in Excel :P Need to pasted Date and time to do DateTime Original)
#str(All.rec) ## Date and Time info is all factor format for each record table. Not consistent format
#str(rec.2015) 
#str(rec.2016)## Different format
#str(rec.2017)

All.rec <- read.csv("recordTable_nov2015-nov2017.csv")
All.rec$X.1 <- NULL
All.rec$X <- NULL
str(All.rec)
head(All.rec$DateTimeOriginal)
tail(All.rec$DateTimeOriginal) # same format

##Convert to POSIXct time
All.rec$Date.Time <- as.POSIXct(strptime(All.rec$DateTimeOriginal, format = "%Y-%m-%d %H:%M:%S"))
All.rec$Datep <- as.POSIXct(strptime(All.rec$Date, format = "%Y-%m-%d"))
str(All.rec)

write.csv(All.rec, "recordTable_nov2015-nov2017.csv")

#### No. detections/ month data. FIRST 2 DEPLOYMENTS ONLY ####
pilot <- read.csv("2015.01_monthlydetections.csv")
pilot$X <- NULL
pilot$X.1 <- NULL
#Jan. 23: pilot data contains snow days. Drop for now and re-add later
pilot$SnowDays <- NULL
win60 <- read.csv("2016.01_monthlydetections.csv") 

win60$X <- NULL

# Full dataset will have NAs for the months that Algar25-60 were inactive (Nov. 2015-Oct.2016)
# 12 months, 36 cameras = 432 rows of NAs
# 6 months, 36 cameras = 216
# 18 months, 24 cameras = 432
# Should have 1080 rows of data (inactive and active cameras for entire survey up to April 2017)

#### First 24 cameras only ####
# Algar01 - 24 from 2nd deployment
Alg24 <- win60[1:138, ]

# Combine all 24 cameras for full survey
alldat <- rbind(pilot,Alg24)

## Double counts for Nov. 2016 (counted in both surveys)?
nov2016 <- alldat %>% filter(Yr_Month == "2016-11") ## Yes. Need to be combined

## Use aggregate function. Tested on data frame with only duplicated months
nov2016 <- aggregate(. ~ Site + Treatment + Yr_Month + Site_ym, data = nov2016, sum) 

### Combining data for 24 cams for full survey
alldat <- alldat[!alldat$Yr_Month == "2016-11", ] #Remove duplicated rows

alldat <- rbind(alldat,nov2016) #Combine fixed 2016-11 data to other months
alldat <- alldat[with(alldat, order(Site, Yr_Month)), ] ## Ordering by Site and Yr_month

## Algar18 failed in second deployment. Need to add NA rows?


#### Full data set for all 60 cameras across all survey months ####
## alldat = First 24 cameras for all survey months

win36 <- win60[139:330, ] #Active cams. 192 rows --> Of the 36 additional cameras, 3 failed and 2 had no detections

## Algar52 and Algar56 had no detections. Need to add rows for months they were active (Nov. 2016 - Apr. 2017)
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

#Add nodet to win36
win36 <- as.data.frame(rbind(win36, nodet))
win36 <- with(win36, win36[order(as.factor(as.character(Site)), Yr_Month), ]) ## Ordering by Site and Yr_month. as.factor(as.character()) rearranges the levels in Site
tail(win36)



## Combine all ACTIVE cameras.
active <- as.data.frame(rbind(alldat, win36))
head(active)
tail(active)

## Still missing:
# 1. Algar25 - 60 - Inactive between Nov. 2015 - Nov. 2016
# 2. Algar18, 32, 50 - Inactive between Nov. 2016- Apr.2017
# 3. Algar49 - Inactive between Jan. 2017 - Apr. 2017 (currently just 0's in those months, can be edited with fix())

## 1. Adding inactive rows for 25-60 between Nov. 2015 - Nov. 2016
# Creating Site names
Algar <- rep("Algar", 36)
Algarnum <- 25:60
Site <- paste(Algar, Algarnum, sep="")
class(Site)
Site <- as.factor(Site)
Site <- rep(Site, 12) # 12 = no. months

inac36 <- as.data.frame(Site)
head(inac36)
str(inac36)

## Adding Treatments
inac36$Treatment <- win60$Treatment[match(inac36$Site, win60$Site)]
head(inac36)
inac36 <- with(inac36, inac36[order(Site), ]) ## Reorder by Site
head(inac36)

## Adding Yr_Month
Yr_Month <- c("2015-11", "2015-12", "2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06", "2016-07", "2016-08", "2016-09", "2016-10")
Yr_Month <- rep(Yr_Month, 36) #36 = no. of cameras
inac36$Yr_Month <- Yr_Month 
head(inac36)


## Adding Site_ym
inac36$Site_ym <- paste(inac36$Site, inac36$Yr_Month)
head(inac36)

## Adding NAs for detection data
detections <- as.data.frame(matrix(NA, nrow = 432, ncol = 7)) # 432 = 12*36
colnames(detections) <- c("Blackbear", "Wolf", "Coyote", "Lynx", "Caribou", "WTDeer", "Moose")
head(detections)

## Combine to Deployment data
inac36 <- cbind.data.frame(inac36,detections)
head(inac36)                 

## Combine inac36 to active
Alg.data <- rbind(active, inac36)
head(Alg.data)
tail(Alg.data)
Alg.data <- with(Alg.data, Alg.data[order(as.factor(as.character(Site)), Yr_Month), ])
head(Alg.data)
tail(Alg.data)

#### 2. Adding NA rows for failed cameras: Algar 18, 32, 50 ####
## Algar18 has 2016-11 data for first deployment but not for 2nd --> mark as inactive for 2016-12 to 2017-04 but not 2016-11

# Do Algar 18 separately, first
Algar18 <- as.data.frame(rep("Algar18", 5))
colnames(Algar18) <- "Site"
head(Algar18)
Algar18$Treatment <- rep("SPP", 5)
Algar18$Yr_Month <- c("2016-12", "2017-01", "2017-02", "2017-03", "2017-04")
head(Algar18)
Algar18$Site_ym <- paste(Algar18$Site, Algar18$Yr_Month)

## Adding NAs for detection data
detections <- as.data.frame(matrix(NA, nrow = 5, ncol = 7))
colnames(detections) <- c("Blackbear", "Wolf", "Coyote", "Lynx", "Caribou", "WTDeer", "Moose")
head(detections)

Algar18 <- cbind.data.frame(Algar18, detections)

## Algar 32, 50
Algar32 <- rep("Algar32", 6)
Algar50 <- rep("Algar50", 6)
Site <- append(Algar32, Algar50)
inac <- as.data.frame(Site)
inac
inac$Treatment <- as.factor(ifelse(inac$Site == "Algar32", "HumanUse", "SPP"))
inac

Yr_Month <- rep(c("2016-11", "2016-12", "2017-01", "2017-02", "2017-03", "2017-04"), 2)
inac$Yr_Month <- as.factor(Yr_Month)
inac

inac$Site_ym <- paste(inac$Site, inac$Yr_Month)

## Adding NAs for detection data
detections <- as.data.frame(matrix(NA, nrow = 12, ncol = 7))
colnames(detections) <- c("Blackbear", "Wolf", "Coyote", "Lynx", "Caribou", "WTDeer", "Moose")
head(detections)

inac <- cbind.data.frame(inac, detections)
inac

inac <- rbind(Algar18, inac)
inac

Alg.data <- rbind(Alg.data, inac)
head(Alg.data)
tail(Alg.data)
Alg.data <- with(Alg.data, Alg.data[order(as.factor(as.character(Site)), Yr_Month), ])
head(Alg.data)
tail(Alg.data)

#### 3. Fix Algar49 ####
Alg.data %>% filter(Site == "Algar49") %>% print()
fix(Alg.data) ## Added NAs to Algar49 for 2017-01 to 2017-04. Also Added Treatments to Algar32 and 50 for first deployment months (skipped because they aren't present in first data.frame)
summary(Alg.data$Treatment) ##Still 24 NA's

Alg.data[which(is.na(Alg.data$Treatment)), ] ##Algar52 and 56, active no detection sites
Alg.data[which(is.na(Alg.data$Treatment))[c(1:24)], "Treatment"] <- "NatRegen" #Reassigning NAs to NatRegen. Rows called by c(1:24), column called by "Treatment"

summary(Alg.data$Treatment) #NAs gone
Alg.data %>% filter(Site == "Algar52") %>% print() #NAs gone
Alg.data %>% filter(Site == "Algar56") %>% print() #NAs gone
summary(Alg.data)
glimpse(Alg.data)

getwd()
write.csv(Alg.data, "monthlydetections_alldata.csv")

#### Add Snow data for Nov. 2015 - Nov.2016 ####
Alg.data <- read.csv("monthlydetections_nov2015-apr2017.csv") #I changed name for clarity, Jan. 23, 2018
Alg.data$X <- NULL 
dep1 <- read.csv("2015.01_monthlydetections.csv")
dep1$X.1 <- NULL
dep1$X <- NULL
dep2 <- read.csv("2016.01_monthlydetections.csv")
dep2$X <- NULL


#### 1st attempt: First add SnowDays from 1st deployment (DOES NOT WORK) ####
Alg.data$SnowDays <- dep1$SnowDays[match(Alg.data$Site_ym, dep1$Site_ym)]
#Add 2nd deployment SnowDays to new column
Alg.data$SnowDays2 <- dep2$SnowDays[match(Alg.data$Site_ym, dep2$Site_ym)]
#Nov 2016 has SnowDay counts for both deployments. Can add together, as the Check Date will be recorded in one deployment or the other, depending on the time of the camera check. It will not be recorded twice.

#Combine SnowDays and SnowDays2 in Alg.data, summing cells with 2 counts
# Duplicate data for insurance
A2 <- Alg.data 
unique(colnames(A2))

#rename SnowDays2 to match SnowDays (done manually)
fix(A2)
unique(colnames(A2))
str(A2) #SnowDays are both numeric

# sapply(unique(colnames(A2)), function(x) rowSums(A2[,grepl(x, colnames(A2))])) --> hasn't worked yet

Alg.data[which(is.na(Alg.data$SnowDays)), "SnowDays"] <- dep2$SnowDays[match(Alg.data$Site_ym, dep2$Site_ym)]


#### 2nd attempt: combine snow data from both deployments (WORKS) ####
b1 <- dep1 %>% select(Site, Site_ym, Yr_Month, SnowDays)
b2 <- dep2 %>% select(Site, Site_ym, Yr_Month, SnowDays)
str(b1) 
str(b2) 
b <- bind_rows(b1, b2)
str(b) #coerces factors into characters
b$Site <- as.factor(b$Site)
b$Site_ym <- as.factor(b$Site_ym)
b$Yr_Month <- as.factor(b$Yr_Month)
head(b)
tail(b)
#Order by Site
b <- b[with(b, order(Site, Yr_Month )), ] ## Ordering by Site and Yr_month

## Double counts for Nov. 2016 (counted in both surveys) (copied from above)
nov2016 <- b %>% filter(Yr_Month == "2016-11") ## 81 (24 cameras from 1st deployment + active cameras from second = 24 + 57 = 81)

## Use aggregate function. Tested on data frame with only duplicated months
nov2016 <- aggregate(. ~ Site + Yr_Month + Site_ym, data = nov2016, sum) 

### Combining data for 24 cams for full survey
b <- b[!b$Yr_Month == "2016-11", ] #Remove duplicated rows

b <- rbind(b,nov2016) #Combine fixed 2016-11 data to other months
b <- b[with(b, order(Site, Yr_Month)), ] ## Ordering by Site and Yr_month

#Algar49 has 0's where NA's should be:2017-01, 2017-02, 2017-03 (camera effectively inactive)
fix(b)#fix manually

Alg.data$SnowDays <- b$SnowDays[match(Alg.data$Site_ym, b$Site_ym)]

write.csv(Alg.data, "monthlydetections_nov2015-apr2017.csv")


######### Combining monthlydetections for Nov2015 - Apr2017 with Apr-Nov2017 (both including SnowData) #######
dep2 <- read.csv("monthlydetections_nov2015-apr2017.csv")
dep2017 <- read.csv("2017.01_monthlydetections.csv")
dep3 <- read.csv("MonthlyDetections_nov2015-nov2017.csv") #will be saved over, but needed for comparison

## Need to think about what I want to do when adding together the April counts from each dataset...
#Cameras inactive in April from first dataset: Algar18, Algar32, Algar49, Algar50
#Cameras inactive in April from second dataset: Algar18, Algar32, Algar41
# Desired value: Algar18,32, 49, 50 = NA, Algar41 =  (0's for detections, sum for Snow = 8)
# When adding duplicates, if either is NA, return NA --> fix Algar41 manually
#Snow Data can be summed (see comment on line 332)

all <- rbind.data.frame(dep2, dep2017) 
all <- with(all, all[order(as.factor(as.character(Site)), Yr_Month), ])

## April 2017 is duplicated, need to be combined
apr17 <- all %>% filter(Yr_Month == "2017-04") ##120 observations --> 2 rows for apr2017 per station

## Use aggregate function. Tested on data frame with only duplicated months
apr17 <- aggregate(. ~ Site + Treatment + Yr_Month + Site_ym, data = apr17, sum, na.action = na.pass) #na.pass ensures NA values are kept

apr17$X <- NULL

## Fixing Algar41 as desired
fix(apr17)


### Remove all Apr2017 rows from dataset
all <- all[!all$Yr_Month == "2017-04", ]
all$X <- NULL

all <- rbind(all,apr17) #Combine fixed 2017-04 data to other months
all <- all[with(all, order(Site, Yr_Month)), ] ## Ordering by Site and Yr_month

table(all$Site)

### all now = monthly detections and snow data between Nov 2015-Nov2017
## "2017-11" detection data should be NA for all sites, as activedays in this period < 15 days
# Exclude 2017-11
#Duplicate data in case of screw up
all2 <- all

all2[which(all2$Yr_Month=="2017-11"), 5:12] <- NA
#Compare the all2 to all
all2[which(all2$Yr_Month=="2017-11"), ]
all[which(all$Yr_Month=="2017-11"), ] ## all2 converted detections and SnowDays to NA for all end months

#Compare all2 to dep3 - 1500 obs. vs 1475
unique(dep3$Yr_Month) #25 levels
unique(all2$Yr_Month) #25 levels
unique(dep3$Site) #missing Algar32 --> No detections
unique(all2$Site)

## Save all2 as 3-deployment detection data, overwriting MonthlyDetections
write.csv(all2, "MonthlyDetections_nov2015-nov2017.csv")

