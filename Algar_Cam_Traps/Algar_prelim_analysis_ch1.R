###########################################################
# Algar_prelim_analysis_ch1.R
# Preliminary data analysis, modifying Jo's code (Algar_prelim_analysis_BayesianPA.R)
# Script modifying Jo's Bayesian analysis code for max. likelihood
# created by Erin T., 11-Oct-2017
# Modified for 3-deployment record table, Dec. 13, 2017
###########################################################

library(reshape2)	# for formatting data frames
# library(plyr) need for renaming Treatments for consistency, but conflicts with dplyr
library(dplyr)		# for applying functions to subsets of data frames
library(ggplot2)	# for data visualization
library(stringr)	# for working with character strings
library(tidyr)		# for data formatting functions
library(knitr)		# for the "kable" function for formatting tables

###--- set directory, load files, check that they loaded correctly
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
data <- read.csv("recordTable_nov2015-nov2017.csv",header=T, row.names=1)
glimpse(data)
summary(data)


###--- Add columns for the Date.Time and Date in POSIX format:
### 2016.01 data --> date data already in correct format, but needs to be converted to POSIX data type
data$Date.Time <- as.POSIXct(data$Date.Time)
data$Datep <- as.POSIXct(data$Datep)
str(data)
## If format also needs to be changed
# data$Date.Time <- as.POSIXct(strptime(data$DateTimeOriginal, format = "%d/%m/%Y %H:%M"))
# data$Datep <- as.POSIXct(strptime(data$Date, format = "%d/%m/%Y"))


###--- all cameras were operational for entire year - range of detection timing depend on site
# first date of detection per site - ranges from 2015-11-05 to 2016-05-06 
mindate <- aggregate(Date.Time~Station, data, function(x) min(x))
head(mindate)
mindate.ord <- mindate[order(mindate$Date.Time, mindate$Station),] ## Order first detections by date - first date of detection ranges from 2015-11-05 to 2017-05-12
head(mindate.ord)
tail(mindate.ord)
mindate.ord <- mindate[order(mindate$Station, mindate$Date.Time),] ## Order first detections by Station
mindate.ord 

maxdate <- aggregate(Date.Time~Station, data, function(x) max(x))
maxdate.ord <- maxdate[order(maxdate$Date.Time, maxdate$Station),] # last date of detection per site - ranges from 2016-12-11 to 2017-11-06
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
data$StudyDay <- data$StudyDay+1 #Turns start date into day 1, not day 0
summary(data$StudyDay) # 1-734 study days

####################################################################
##Adding Treatment Column (naming Stations 1-12 as Control and everything else as SPP)
## Not necessary with nov2015-2017 record table
data$Treatment <- as.factor(ifelse(data$Station=="Algar1"|data$Station=="Algar2"|data$Station=="Algar3"|data$Station=="Algar4"|
                                     data$Station=="Algar5"|data$Station=="Algar6"|data$Station=="Algar7"|data$Station=="Algar8"|
                                     data$Station=="Algar9"|data$Station=="Algar10"|data$Station=="Algar11"|data$Station=="Algar12",
                                   "Control", "SPP")) ### Station names 1-9 can't have a 0 to match pilot data!
## Rename pilot stations to stay consistent with following data (revalue is a plyr function)
data$Station <- revalue(data$Station, replace = c("Algar1" = "Algar01", "Algar2" = "Algar02", "Algar3" = "Algar03", "Algar4" = "Algar04", "Algar5" = "Algar05", "Algar6" = "Algar06", "Algar7" = "Algar07", "Algar8" = "Algar08", "Algar9" = "Algar09"))


### Adding Treatment Column by matching with station data
Stations <- read.csv("Station_data/AlgarStations60.csv")
data$Treatment <- Stations$Treatment[match(data$Station, Stations$CamStation)]
## Renaming to maintatin consistency with pilot data (SPP, not SP+P)
unique(data$Treatment) 

data$Treatment <- revalue(data$Treatment, replace = c("SP+P" = "SPP", "Nat Regen" = "NatRegen", "Human Use" = "HumanUse")) ## Not necessary with nov2015-2017 record table

glimpse(data)
summary(data)

#############################################################
###--- Transform data to have counts of species detections per site
###--- Need to think about how you want to structure this - detections per site per day, per week, per month?
###--- Data is in date format so relatively simple to group by date frequencies once decided

### use reshape2 package to create vector of all study days per site
# sum the number of independent-detections for each Site on each StudyDay
# this creates dataframe, so Site is first column
# only includes sites and days with species detections

study.days <- 1:max(data$StudyDay)
sites <- unique(data$Station)
sites <- sort(sites, decreasing=FALSE)

Site_rep <- rep(sites,734) ## Repeat site names 734 times (# days in study period)
length(Site_rep)
head(Site_rep)

unique(data$Station) #59 stations with detections
study.days_rep <- rep(study.days, 59) ## Repeat study days 59 times (# of camera sites with detections)
study.days_rep <- sort(study.days_rep, decreasing=FALSE)

d <- cbind(as.data.frame(Site_rep),study.days_rep) ## Creates a data frame of sites and study day
d$Site_SD <- as.factor(paste(d$Site_rep, d$study.days_rep)) ##New column with both site and study day
summary(d)


data$count <- 1 #add count column - 1 detection per row (does not take multiple individuals/detection into consideration)
names(data)
data2 <- data[c(1,2,15:17)] ## new data frame only containing site, species, study day, treatment, and detection count
head(data2)

levels(data$Species) ##Names of different species detected

#### Create individual species data frames --> # detections per day #### 
## if summarise function only gives a 1 by 1 output that sums the count column: close R and reload with dplyr loaded only (not plyr)

d.deer <- data2 %>%
  filter(Species == "Odocoileus virginianus") %>% ##Select deer data only
  group_by(Station, StudyDay) %>%        ## Arrange data first by Station, then by StudyDay (both in ascending order)
  summarise(sum(count, na.rm = TRUE))    ## Summarise the detection count for that day, add to new column in data frame
colnames(d.deer) <- c("Site","StudyDay","WTDeer") ##Renaming columns (ignore Warnings, function will add colname)
glimpse(d.deer) #464 days of deer detections (where some days have multiple detections)
plot(d.deer$StudyDay, d.deer$WTDeer, xlim=c(0,750)) ##Mostly 1 detection/day, up to 5

d.bear <- data2 %>%
  filter(Species == "Ursus americanus") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.bear) <- c("Site","StudyDay","Blackbear")
glimpse(d.bear) #315 obs
plot(d.bear$StudyDay, d.bear$Blackbear, xlim=c(0,750)) #Up to 4 detections in a day, detections concentrated into 2 summer seasons (between days ~180-360 and days ~520-700)
d.bear <- d.bear[order(d.bear$StudyDay),]
head(d.bear) ##First detections of bears within this deployment - day 159
tail(d.bear) ##Last detections of bears within this deployment - day 712
# Seasons: day 159 - 349, day 523 - 712


d.caribou <- data2 %>%
  filter(Species == "Rangifer tarandus") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.caribou) <- c("Site","StudyDay","Caribou")
glimpse(d.caribou) #125 obs
plot(d.caribou$StudyDay, d.caribou$Caribou, xlim=c(0,750)) # As many as 3 in a day (once)

d.moose <- data2 %>%
  filter(Species == "Alces alces") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.moose) <- c("Site","StudyDay","Moose")
glimpse(d.moose) #131 obs
plot(d.moose$StudyDay, d.moose$Moose, xlim=c(0,750)) # As many as 3 in a day (once)

d.coyote <- data2 %>%
  filter(Species == "Canis latrans") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.coyote) <- c("Site","StudyDay","Coyote")
glimpse(d.coyote) #126 obs
plot(d.coyote$StudyDay, d.coyote$Coyote, xlim=c(0,750)) #1 3-det. and 1 4-det. data point

d.lynx <- data2 %>%
  filter(Species == "Lynx canadensis") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.lynx) <- c("Site","StudyDay","Lynx")
glimpse(d.lynx) # 67 obs
plot(d.lynx$StudyDay, d.lynx$Lynx, xlim=c(0,750)) # 2 data pts for 2-det. days

d.wolf <- data2 %>%
  filter(Species == "Canis lupus") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.wolf) <- c("Site","StudyDay","Wolf")
glimpse(d.wolf) # 273 obs
plot(d.wolf$StudyDay, d.wolf$Wolf, xlim=c(0,750)) #3 3-det. days, many 1 and 2

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

summary(data3)

data3[is.na(data3)] <- 0 ## Converting NAs to 0's
summary(data3)
nrow(data3)

sum(data3$WTDeer) #539
sum(data3$Blackbear) #360
sum(data3$Caribou) #133
sum(data3$Coyote) #148
sum(data3$Lynx) #69
sum(data3$Wolf) #309
sum(data3$Moose) #138



####--- aggregate species detection data by month ####
data$Site_SD <- paste(data$Station,data$StudyDay)
data3$StudyDay.date <- as.Date(data3$StudyDay,origin = "2015-11-04") # Origin = day before first detection

data3$Year <- as.factor(format(as.Date(data3$StudyDay.date), "%Y")) ##Separating date info into year, month, and year_month (to distinguish Nov.2015 from Nov. 2016)
data3$Month <- as.factor(format(as.Date(data3$StudyDay.date), "%b"))
data3$Yr_Month <- as.factor(format(as.Date(data3$StudyDay.date), "%Y-%m"))

head(data3)
summary(data3)
levels(data3$Yr_Month)

sum(data3$WTDeer)#539
sum(data3$Blackbear) #360
sum(data3$Caribou) #133
sum(data3$Coyote) #148
sum(data3$Lynx) #69
sum(data3$Wolf) #309
sum(data3$Moose) #138

write.csv(data3, "detectionsByDay_nov2015-nov2017.csv")

m.deer <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(WTDeer, na.rm = TRUE))
colnames(m.deer) <- c("Site","Treatment","Yr_Month","WTDeer")
summary(m.deer) #max 21 obs/ station in one month
sum(m.deer$WTDeer) #541 - matches

m.bear <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Blackbear, na.rm = TRUE))
colnames(m.bear) <- c("Site","Treatment","Yr_Month","Blackbear")
summary(m.bear) # max 20 obs/station in one month
sum(m.bear$Blackbear) #361 - matches

m.caribou <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Caribou, na.rm = TRUE))
colnames(m.caribou) <- c("Site","Treatment","Yr_Month","Caribou")
summary(m.caribou) # max 9 obs/station in one month
sum(m.caribou$Caribou) #133 - matches

m.moose <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Moose, na.rm = TRUE))
colnames(m.moose) <- c("Site","Treatment","Yr_Month","Moose")
summary(m.moose) # max 6 obs/ station in one month
sum(m.moose$Moose) #138 - matches


m.coyote <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Coyote, na.rm = TRUE))
colnames(m.coyote) <- c("Site","Treatment","Yr_Month","Coyote")
summary(m.coyote) # max 34 obs in one month
sum(m.coyote$Coyote) # 150 - matches

m.lynx <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Lynx, na.rm = TRUE))
colnames(m.lynx) <- c("Site","Treatment","Yr_Month","Lynx")
summary(m.lynx) # max 4 obs in one month
sum(m.lynx$Lynx) #69 - matches

m.wolf <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Wolf, na.rm = TRUE))
colnames(m.wolf) <- c("Site","Treatment","Yr_Month","Wolf")
summary(m.wolf) # max 19 obs in one month
sum(m.wolf$Wolf) # 310 - matches

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

sum(data.month$WTDeer)
sum(data.month$Blackbear) 
sum(data.month$Caribou) 
sum(data.month$Coyote) 
sum(data.month$Lynx) 
sum(data.month$Wolf) 
sum(data.month$Moose) 

summary(data.month)
glimpse(data.month)


write.csv(data.month, "MonthlyDetections_nov2015-nov2017.csv")

#### Checking data for errors + data exploration (pilot data so it's okay)
hist(data.month$Wolf)
hist(data.month$Blackbear)
hist(data.month$Coyote)
hist(data.month$Lynx)
hist(data.month$Caribou)
hist(data.month$WTDeer)
hist(data.month$Moose)

plot(data.month$Treatment, data.month$Blackbear)
plot(data.month$Yr_Month, data.month$Blackbear) ##Active only in 2017-04

plot(data.month$Treatment, data.month$Wolf)
plot(data.month$Yr_Month, data.month$Wolf)

plot(data.month$Treatment, data.month$Coyote)
plot(data.month$Yr_Month, data.month$Coyote)

plot(data.month$Treatment, data.month$Lynx)
plot(data.month$Yr_Month, data.month$Lynx)

plot(data.month$Treatment, data.month$Caribou)
plot(data.month$Yr_Month, data.month$Caribou) 

plot(data.month$Treatment, data.month$WTDeer)
plot(data.month$Yr_Month, data.month$WTDeer)

plot(data.month$Treatment, data.month$Moose)
plot(data.month$Yr_Month, data.month$Moose)

#### Linear regressions (differ from Jo's Bayesian analysis) ####
### Check if there is a difference in species use of Control and SPP lines ##
### Still only modelling with pilot data (3:25pm Oct. 16, 2017)
pilot.month <- read.csv("2015.01_monthlydetections.csv")
data.month <- read.csv("2016.01_monthlydetections.csv")
## Editing pilot.month Stations for consistency with 2nd deployment data

pilot.month$X <- NULL
data.month$X <- NULL

### Pilot data csv's have mismatched sites and treatments. Need to go back and fix.... Fixed 4:45pm, Oct. 16
### 4:54 pm starting re-run of pilot data prep again --> Site and Site_SD columns need to match... fixed 5:10pm, Oct.16



library(lme4)
library(MASS)
library(AICcmodavg)

## glmmADMB conflicts with lme4 (Oct. 16 --> lme4 models already run for wolves, bears, coyotes)
## For zero-inflated glmms --> not yet tested, Oct. 17, 2017
install.packages("R2admb")
install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         getOption("repos")),
                 type="source")
library(R2admb)
library(glmmADMB)

#### Wolf modelling: Compare glmm to glm, adding 2 random effects, checking out zero-inflated Poisson and Neg. binomial GLMs ####
##Poisson glm
m0.wolf <- glm(Wolf~Treatment, family = poisson, data = pilot.month)
## Negative binomial glm
m1.wolf <- glm.nb(Wolf~Treatment, link = "log", data = pilot.month) # Use log link to compare with poisson

#Comparing Poisson and Neg. binomial with anova (lieklihood ratio test)
anova(m0.wolf, m1.wolf)
summary(m0.wolf)
summary(m1.wolf)

## Poisson GLMM
m2.wolf <- glmer(Wolf~Treatment + (1|Site), family = poisson, data = pilot.month)

## Negative binomial GLMM
m3.wolf <- glmer.nb(Wolf~Treatment + (1|Site), data = pilot.month)

anova(m2.wolf, m3.wolf)


anova(m0.wolf, m2.wolf)##m2 has lower AIC score, higher logLikelihood --> enough additional variance explained to justify more complexity of random effect

AIC(m0.wolf, m1.wolf, m2.wolf, m3.wolf) ## m3 has lowest AIC. Not sure if AIC is an appropriate comparison
anova(m2.wolf, m3.wolf) ## m3 AIC significantly lower than m2 AIC
summary(m3.wolf)

### Negative binomial GLMM has  highest explanatory power



## Intercept-free model, should be same as m1.wolf, just comparing 2 treatment estimates to zero rather than to each other
## Not run for pilot data
# m2.wolf <- glmer(Wolf~Treatment + (1|Site) -1, family = poisson, data = pilot.month) 


### Adding second random effect of time
m4.wolf <- glmer(Wolf~Treatment + (1|Site) + (1|Yr_Month), family = poisson, data = pilot.month)
summary(m4.wolf) ## Yr_Month random variable doesn't account for much variance....
anova(m2.wolf, m3.wolf, m4.wolf) ##m3 still wins
m5.wolf <- glmer.nb(Wolf~Treatment + (1 |Site) + (1|Yr_Month), data = pilot.month)
anova(m2.wolf,m3.wolf, m4.wolf, m5.wolf) ## m5 < m3
summary(m5.wolf)

## Time as a fixed effect
m6.wolf <- glmer.nb(Wolf~Treatment + Yr_Month + (1|Site), data = pilot.month)
summary(m6.wolf)
## Failed to converge. Might not be the strongest way to account for season anyway

##Zero-inflated GLMs
library(pscl)
f1 <- formula(Wolf~Treatment | 1) #where the count distribution is modelled as a function of treatment, binomial distrib. modelled as constant
# ZIP
Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = pilot.month) 
summary(Zip1) ## SPP estimate is sig. different from Control in the count model
# ZINB
Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = pilot.month)
summary(Nb1)

## Zero-inflated GLMMs?



# Model selection
AIC(m0.wolf, m1.wolf, m2.wolf, m3.wolf, m4.wolf, m5.wolf, m6.wolf, Zip1, Nb1) ## Still m5 < m3 < m4... Negative binomial distribution decreases AIC more than adding 2nd random variable.
#Adding Yr_Month as fixed effect dec. AIC to same as m3, but lots of degrees of freedom. Would only do this to account for effect of season/time, in which case the Yr_Month variable isn't most appropriate anyway
cand.set.wolf <- c(m2.wolf, m3.wolf, m4.wolf, m5.wolf)
names <- c("glmm.Pois", "glmm.nb", "glmm2.Pois", "glmm2.nb")
aictab(cand.set.wolf, modnames = names, second.ord = TRUE, nobs = NULL,
       sort = TRUE) ## Function doesn't support a mixture of model classes. Check glmms only

### Zero-inflated GLMMs
## Need to restart R session first to unload lme4
## For zero-inflated glmms --> not yet tested, Oct. 17, 2017
install.packages("R2admb")
install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         getOption("repos")),
                 type="source")
library(R2admb)
library(glmmADMB)
ZIPmer.wolf <- glmmadmb(Wolf~Treatment + (1| Site), data = pilot.month, family = "poisson", link = "log", zeroInflation = TRUE)
summary(ZIPmer.wolf)
AIC(m2.wolf, ZIPmer.wolf)

ZINBmer.wolf <- glmmadmb(Wolf~Treatment + (1| Site), data = pilot.month, family = "nbinom", link = "log", zeroInflation = TRUE) ##Parameters estimated, but standard errors were not. Function maximiser failed. Re-run with save.dir specified as getwd()

ZINBmer.wolf <- glmmadmb(Wolf~Treatment + (1| Site), data = pilot.month, family = "nbinom", link = "log", zeroInflation = TRUE, save.dir = getwd()) ## saves an output I can't open...

#re-run with debug=TRUE
ZINBmer.wolf <- glmmadmb(Wolf~Treatment + (1| Site), data = pilot.month, family = "nbinom", link = "log", zeroInflation = TRUE, debug = TRUE) # Can't understand output.

## run with different admbControl (suggest by Richard Shuster on https://groups.google.com/a/admb-project.org/forum/#!topic/users/DXHfeLEHiAQ)
ZINBmer.wolf <- glmmadmb(Wolf~Treatment + (1| Site), data = pilot.month, family = "nbinom", link = "log", zeroInflation = TRUE, admb.opts=admbControl(shess=FALSE,noinit=FALSE)) 

##admbControl take 2...took longer to run, same error messages
ZINBmer.wolf <- glmmadmb(Wolf~Treatment + (1| Site), data = pilot.month, family = "nbinom", link = "log", zeroInflation = TRUE, admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))


summary(ZINBmer.wolf)

## Comparing nb glmms to ZINBglmms, pois glmms to ZIPglmms
AIC(m2.wolf, m3.wolf, ZIPmer.wolf) ## Still no lower AIC than nbin.glmm

#### Blackbear modelling: Compare glmm to glm, adding 2 random effects, checking out zero-inflated Poisson and Neg. binomial GLMs ####
##Poisson glm
m0.bear <- glm(Blackbear~Treatment, family = poisson, data = pilot.month)
## Negative binomial glm
m1.bear <- glm.nb(Blackbear~Treatment, link = "log", data = pilot.month) # Use log link to compare with poisson

## Poisson GLMM
m2.bear <- glmer(Blackbear~Treatment + (1|Site), family = poisson, data = pilot.month)

## Negative binomial GLMM
m3.bear <- glmer.nb(Blackbear~Treatment + (1|Site), data = pilot.month)

anova(m2.bear, m3.bear)

anova(m0.bear, m2.bear)##m2 has lower AIC score, higher logLikelihood --> enough additional variance explained to justify more complexity
AIC(m0.bear, m1.bear, m2.bear, m3.bear) ## m3 has lowest AIC, followed by m1
anova(m2.bear, m3.bear) ## m3 AIC significantly lower than m2 AIC, Can't compare glm.nb to glmm.nb

summary(m3.bear)

### Negative binomial GLMM has  highest explanatory power



## Intercept-free model, should be same as m1.bear, just comparing 2 treatment estimates to zero rather than to each other
## Not run for pilot data
# m2.bear <- glmer(Blackbear~Treatment + (1|Site) -1, family = poisson, data = pilot.month) 


### Adding second random effect of time
m4.bear <- glmer(Blackbear~Treatment + (1|Site) + (1|Yr_Month), family = poisson, data = pilot.month)
summary(m4.bear) ## Yr_Month random variable accounts for LOTS of variance
anova(m2.bear, m3.bear, m4.bear) ##m3 and m4 significantly lower than m2, m4 = 445 vs. 503
m5.bear <- glmer.nb(Blackbear~Treatment + (1 |Site) + (1|Yr_Month), data = pilot.month)
anova(m2.bear, m3.bear, m4.bear, m5.bear)
summary(m5.bear)

##Zero-inflated GLMs
library(pscl)
f1 <- formula(Blackbear~Treatment | 1) #where the count distribution is modelled as a function of treatment, binomial distrib. modelled as constant
# ZIP
Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = pilot.month) 
summary(Zip1) ## SPP estimate is sig. different from Control in the count model
# ZINB
Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = pilot.month)
summary(Nb1)

# Model selection
AIC(m0.bear, m1.bear, m2.bear, m3.bear, m4.bear, m5.bear, Zip1, Nb1) ## m5 < m4 < m3

## Negative binomial with 2 random variables wins


### Zero-inflated GLMMs
## Need to restart R session first to unload lme4
ZIPmer.bear <- glmmadmb(Blackbear~Treatment + (1| Site), data = pilot.month, family = "poisson", link = "log", zeroInflation = TRUE)
ZINBmer.bear <- glmmadmb(Blackbear~Treatment + (1| Site), data = pilot.month, family = "nbinom", link = "log", zeroInflation = TRUE) 
summary(ZIPmer.bear)
anova(ZIPmer.bear, ZINBmer.bear)
AIC(ZIPmer.bear, ZINBmer.bear)

### Comparing results of glmmADMB to glmer with neg. bin. glmm
glmm.bear <- glmmadmb(Blackbear~Treatment + (1| Site), data = pilot.month, family = "nbinom", link = "log", zeroInflation = FALSE)
AIC(m3.bear,glmm.bear) ## Comparable AIC values and summaries
summary(m3.bear)
summary(glmm.bear)

AIC(m0.bear, m1.bear, m2.bear, m3.bear, m4.bear, m5.bear, Zip1, Nb1, ZIPmer.bear) ## Still no lower AIC than nbin.glmm






#### Caribou modelling: Compare glmm to glm, adding 2 random effects, checking out zero-inflated Poisson and Neg. binomial GLMs ####
##Poisson glm
m0.caribou <- glm(Caribou~Treatment, family = poisson, data = pilot.month)
## Negative binomial glm
m1.caribou <- glm.nb(Caribou~Treatment, link = "log", data = pilot.month) # Use log link to compare with poisson

## Poisson GLMM
m2.caribou <- glmer(Caribou~Treatment + (1|Site), family = poisson, data = pilot.month) ## Failed to converge
summary(m2.caribou)

### Testing convergence issues
relgrad <- with(m2.caribou@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad)) #5.001e-05

## Will re-fit with current parameter estimates set (suggested by lme4 documentation)

m2.caribou@theta #current estimate = 2.381955

m2.caribou <- glmer(Caribou~Treatment + (1|Site), family = poisson, data = pilot.month, start = m2.caribou@theta) #same convergence error
summary(m2.caribou)

#Starting with estimate in m2. summary
m2.caribou <- glmer(Caribou~Treatment + (1|Site), family = poisson, data = pilot.month, start =  1.734731) #can only have one theta component
summary(m2.caribou)
#Still failing to converge, same results
# Try new optimizer using control = glmerControl() argument
?glmerControl
m2.caribou <- glmer(Caribou~Treatment + (1|Site), family = poisson, data = pilot.month, control = glmerControl(optimizer = "Nelder_Mead")) #Same warning
summary(m2.caribou) # Consistent results. lme4 documentation indicates there is not really a problem



## Negative binomial GLMM
m3.caribou <- glmer.nb(Caribou~Treatment + (1|Site), data = pilot.month)
summary(m3.caribou)

### Testing convergence issues
relgrad <- with(m3.caribou@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad)) #2.655e-06

## Will re-fit with current parameter estimates set (suggested by lme4 documentation)

m3.caribou@theta #current estimate = 2.200174

m3.caribou <- glmer.nb(Caribou~Treatment + (1|Site), family = poisson, data = pilot.month, start = m3.caribou@theta) #same convergence error
summary(m3.caribou)

#Starting with estimate in m2. summary
m3.caribou <- glmer.nb(Caribou~Treatment + (1|Site), family = poisson, data = pilot.month, start =  1.734731) #can only have one theta component
summary(m3.caribou)
#Still failing to converge, same results
# Try new optimizer using control = glmerControl() argument
?glmerControl
m3.caribou <- glmer.nb(Caribou~Treatment + (1|Site), family = poisson, data = pilot.month, control = glmerControl(optimizer = "Nelder_Mead")) #Same warning
summary(m3.caribou) # Consistent results. lme4 documentation indicates there is not really a problem

anova(m2.caribou, m3.caribou)

AIC(m0.caribou, m1.caribou, m2.caribou, m3.caribou) ## m3 > m2 > m1
anova(m2.caribou, m3.caribou) ## m3 significantly less than m2, but both models failed to converge
# Warning message:
#  In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                 Model failed to converge with max|grad| = 0.0487246 (tol = 0.001, component 1)



## Intercept-free model, should be same as m1.caribou, just comparing 2 treatment estimates to zero rather than to each other
## Not run for pilot
## m2.caribou <- glmer(Caribou~Treatment + (1|Site) -1, family = poisson, data = pilot.month) 


### Adding second random effect of time
m4.caribou <- glmer(Caribou~Treatment + (1|Site) + (1|Yr_Month), family = poisson, data = pilot.month)
summary(m4.caribou) ## Yr_Month random variable accounts for LOTS of variance
anova(m2.caribou, m3.caribou, m4.caribou) 
m5.caribou <- glmer.nb(Caribou~Treatment + (1 |Site) + (1|Yr_Month), data = pilot.month)
anova(m2.caribou, m3.caribou, m4.caribou, m5.caribou) 


##Zero-inflated GLMs
library(pscl)
f1 <- formula(Caribou~Treatment | 1) #where the count distribution is modelled as a function of treatment, binomial distrib. modelled as constant
# ZIP
Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = pilot.month) 
summary(Zip1) 
# ZINB
Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = pilot.month)
summary(Nb1)

# Model selection
AIC(m0.caribou, m1.caribou, m2.caribou, m3.caribou, m4.caribou, m5.caribou, Zip1, Nb1) ## m5<m4<m3. m2 and m3 didn't converge though

### Zero-inflated glmms with glmmADMB
ZIPmer.caribou <- glmmadmb(Caribou~Treatment + (1| Site), data = pilot.month, family = "poisson", link = "log", zeroInflation = TRUE)
anova(m2.caribou, ZIPmer.caribou)

ZINBmer.caribou <- glmmadmb(Caribou~Treatment + (1| Site), data = pilot.month, family = "nbinom", link = "log", zeroInflation = TRUE)
#Function maximizer failed

AIC(m2.caribou, ZIPmer.caribou)



#### WTDeer modelling: Compare glmm to glm, adding 2 random effects, checking out zero-inflated Poisson and Neg. binomial GLMs ####
##Poisson glm
m0.WTDeer <- glm(WTDeer~Treatment, family = poisson, data = pilot.month)
## Negative binomial glm
m1.WTDeer <- glm.nb(WTDeer~Treatment, link = "log", data = pilot.month) # Use log link to compare with poisson

## Poisson GLMM
m2.WTDeer <- glmer(WTDeer~Treatment + (1|Site), family = poisson, data = pilot.month)
summary(m2.WTDeer)

## Negative binomial GLMM
m3.WTDeer <- glmer.nb(WTDeer~Treatment + (1|Site), data = pilot.month)
summary(m3.WTDeer)


AIC(m0.WTDeer, m1.WTDeer, m2.WTDeer, m3.WTDeer) ## m3 > m2 > m1
anova(m2.WTDeer, m3.WTDeer) ## m3 significantly less than m2

##Treatment has a significant effect on WT deer detections in all models





## Intercept-free model, should be same as m1.WTDeer, just comparing 2 treatment estimates to zero rather than to each other
## Not run for pilot
## m2.WTDeer <- glmer(WTDeer~Treatment + (1|Site) -1, family = poisson, data = pilot.month) 


### Adding second random effect of time
m4.WTDeer <- glmer(WTDeer~Treatment + (1|Site) + (1|Yr_Month), family = poisson, data = pilot.month)
summary(m4.WTDeer) ## Yr_Month random variable accounts for LOTS of variance
anova(m2.WTDeer, m3.WTDeer, m4.WTDeer) ##m3  significantly lower than m2
m5.WTDeer <- glmer.nb(WTDeer~Treatment + (1 |Site) + (1|Yr_Month), data = pilot.month)
anova(m2.WTDeer, m3.WTDeer, m4.WTDeer, m5.WTDeer) ## m5 has significantly more explanatory power


##Zero-inflated GLMs
library(pscl)
f1 <- formula(WTDeer~Treatment | 1) #where the count distribution is modelled as a function of treatment, binomial distrib. modelled as constant
# ZIP
Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = pilot.month) 
summary(Zip1) 
# ZINB
Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = pilot.month)
summary(Nb1)



# Model selection
AIC(m0.WTDeer, m1.WTDeer, m2.WTDeer, m3.WTDeer, m4.WTDeer, m5.WTDeer, Zip1, Nb1) ## m5<m3<m4. Negative binomial distributions best

### Zero-inflated glmms with glmmADMB
ZIPmer.WTDeer <- glmmadmb(WTDeer~Treatment + (1| Site), data = pilot.month, family = "poisson", link = "log", zeroInflation = TRUE)
anova(m2.WTDeer, ZIPmer.WTDeer)

ZINBmer.WTDeer <- glmmadmb(WTDeer~Treatment + (1| Site), data = pilot.month, family = "nbinom", link = "log", zeroInflation = TRUE)
anova(ZIPmer.WTDeer, ZINBmer.WTDeer)
AIC(ZIPmer.WTDeer, ZINBmer.WTDeer)
summary(ZINBmer.WTDeer)

plot(pilot.month$Treatment,pilot.month$WTDeer)


#### Moose modelling: Compare glmm to glm, adding 2 random effects, checking out zero-inflated Poisson and Neg. binomial GLMs ####
##Poisson glm
m0.Moose <- glm(Moose~Treatment, family = poisson, data = pilot.month)
## Negative binomial glm
m1.Moose <- glm.nb(Moose~Treatment, link = "log", data = pilot.month) # Use log link to compare with poisson

## Poisson GLMM
m2.Moose <- glmer(Moose~Treatment + (1|Site), family = poisson, data = pilot.month) 
summary(m2.Moose)

## Negative binomial GLMM
m3.Moose <- glmer.nb(Moose~Treatment + (1|Site), data = pilot.month)
summary(m3.Moose)
anova(m2.Moose, m3.Moose)

AIC(m0.Moose, m1.Moose, m2.Moose, m3.Moose) ## m3<m1<m2. Negative binomial stronger explanatory distribution than glmer Poisson 
anova(m2.Moose, m3.Moose) ## m3 significantly less than m2







## Intercept-free model, should be same as m1.Moose, just comparing 2 treatment estimates to zero rather than to each other
## Not run for pilot
## m2.Moose <- glmer(Moose~Treatment + (1|Site) -1, family = poisson, data = pilot.month) 


### Adding second random effect of time
m4.Moose <- glmer(Moose~Treatment + (1|Site) + (1|Yr_Month), family = poisson, data = pilot.month)
summary(m4.Moose) ## Yr_Month random variable accounts for LOTS of variance
anova(m2.Moose, m3.Moose, m4.Moose) ##m3  significantly lower than m2, 
m5.Moose <- glmer.nb(Moose~Treatment + (1 |Site) + (1|Yr_Month), data = pilot.month)
anova(m2.Moose, m3.Moose, m4.Moose, m5.Moose) ## m5 has significantly more explanatory power


##Zero-inflated GLMs
library(pscl)
f1 <- formula(Moose~Treatment | 1) #where the count distribution is modelled as a function of treatment, binomial distrib. modelled as constant
# ZIP
Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = pilot.month) 
summary(Zip1) 
# ZINB
Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = pilot.month)
summary(Nb1)

# Model selection
AIC(m0.Moose, m1.Moose, m2.Moose, m3.Moose, m4.Moose, m5.Moose, Zip1, Nb1) ## m5<m3<m4. Negative binomial distributions best

### Zero-inflated glmms with glmmADMB
ZIPmer.Moose <- glmmadmb(Moose~Treatment + (1| Site), data = pilot.month, family = "poisson", link = "log", zeroInflation = TRUE)
anova(m2.Moose, ZIPmer.Moose)

ZINBmer.Moose <- glmmadmb(Moose~Treatment + (1| Site), data = pilot.month, family = "nbinom", link = "log", zeroInflation = TRUE)
anova(ZIPmer.Moose, ZINBmer.Moose)
AIC(ZIPmer.Moose, ZINBmer.Moose)
summary(ZINBmer.Moose)


### Disregard mesocarnivores for ch. 1 analyses (no a priori hypotheses stated yet)
#### Coyote modelling: Compare glmm to glm, adding 2 random effects, checking out zero-inflated Poisson and Neg. binomial GLMs ####
##Poisson glm
m0.coyote <- glm(Coyote~Treatment, family = poisson, data = pilot.month)
## Negative binomial glm
m1.coyote <- glm.nb(Coyote~Treatment, link = "log", data = pilot.month) # Use log link to compare with poisson

## Poisson GLMM
m2.coyote <- glmer(Coyote~Treatment + (1|Site), family = poisson, data = pilot.month)

## Negative binomial GLMM
m3.coyote <- glmer.nb(Coyote~Treatment + (1|Site), data = pilot.month)


AIC(m0.coyote, m1.coyote, m2.coyote, m3.coyote) ## m3 < m2 < m1
anova(m2.coyote, m3.coyote) ## m3 AIC significantly lower than m2 AIC, Can't compare glm.nb to glmm.nb

summary(m3.coyote)

### Negative binomial GLMM has  highest explanatory power



## Intercept-free model, should be same as m1.coyote, just comparing 2 treatment estimates to zero rather than to each other
## Not run for pilot data
# m2.coyote <- glmer(Coyote~Treatment + (1|Site) -1, family = poisson, data = pilot.month) 

### Adding second random effect of time
m4.coyote <- glmer(Coyote~Treatment + (1|Site) + (1|Yr_Month), family = poisson, data = pilot.month)
summary(m4.coyote) ## Yr_Month random variable accounts for LOTS of variance
anova(m2.coyote, m3.coyote, m4.coyote) ##m3  significantly lower than m2, m3 = 235 and m4 = 236
m5.coyote <- glmer.nb(Coyote~Treatment + (1 |Site) + (1|Yr_Month), data = pilot.month) ## Warning message:
#                                                                                        In theta.ml(Y, mu, weights = object@resp$weights, limit = #                                                                                        limit,:iteration limit reached
anova(m2.coyote, m3.coyote, m4.coyote, m5.coyote)
summary(m5.coyote)

##Zero-inflated GLMs
library(pscl)
f1 <- formula(Coyote~Treatment | 1) #where the count distribution is modelled as a function of treatment, binomial distrib. modelled as constant
# ZIP
Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = pilot.month) 
summary(Zip1) ## SPP estimate is sig. different from Control in the count model
# ZINB
Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = pilot.month)
summary(Nb1)

# Model selection
AIC(m0.coyote, m1.coyote, m2.coyote, m3.coyote, m4.coyote, m5.coyote, Zip1, Nb1) ## m5 < m3 < m4, but both negbin models failed to converge (m5 and m3)

### Zero-inflated glmms with glmmADMB
ZIPmer.coyote <- glmmadmb(Coyote~Treatment + (1| Site), data = pilot.month, family = "poisson", link = "log", zeroInflation = TRUE)
ZINBmer.coyote <- glmmadmb(Coyote~Treatment + (1| Site), data = pilot.month, family = "nbinom", link = "log", zeroInflation = TRUE) 
summary(ZIPmer.coyote)
summary(ZINBmer.coyote)

### Comparing results of glmmADMB to glmer with neg. bin. glmm
glmm.coyote <- glmmadmb(Coyote~Treatment + (1| Site), data = pilot.month, family = "nbinom", link = "log", zeroInflation = FALSE)
AIC(m3.coyote,glmm.coyote) ## Comparable AIC values and summaries
summary(m3.coyote)
summary(glmm.coyote)

AIC(m0.coyote, m1.coyote, m2.coyote, m3.coyote, m4.coyote, m5.coyote, Zip1, Nb1, ZIPmer.coyote) ### Neg. bin. glmms still lowest



#### Lynx modelling: Compare glmm to glm, adding 2 random effects, checking out zero-inflated Poisson and Neg. binomial GLMs ####
##Poisson glm
m0.lynx <- glm(Lynx~Treatment, family = poisson, data = pilot.month)
## Negative binomial glm
m1.lynx <- glm.nb(Lynx~Treatment, link = "log", data = pilot.month) # Use log link to compare with poisson

## Poisson GLMM
m2.lynx <- glmer(Lynx~Treatment + (1|Site), family = poisson, data = pilot.month)

## Negative binomial GLMM
m3.lynx <- glmer.nb(Lynx~Treatment + (1|Site), data = pilot.month)


AIC(m0.lynx, m1.lynx, m2.lynx, m3.lynx) ## m3 > m2 > m1
anova(m2.lynx, m3.lynx) ## m3 AIC not significantly less than m2. Poisson has one less df, so default to that one

summary(m2.lynx)




## Intercept-free model, should be same as m1.lynx, just comparing 2 treatment estimates to zero rather than to each other
## Not run for pilot
## m2.lynx <- glmer(Lynx~Treatment + (1|Site) -1, family = poisson, data = pilot.month) 


### Adding second random effect of time
m4.lynx <- glmer(Lynx~Treatment + (1|Site) + (1|Yr_Month), family = poisson, data = pilot.month)
summary(m4.lynx) ## Yr_Month random variable accounts for LOTS of variance
anova(m2.lynx, m3.lynx, m4.lynx) ##m3  significantly lower than m2
m5.lynx <- glmer.nb(Lynx~Treatment + (1 |Site) + (1|Yr_Month), data = pilot.month)
anova(m2.lynx, m3.lynx, m4.lynx, m5.lynx) ## m2 lowest AIC with fewest df


##Zero-inflated GLMs
library(pscl)
f1 <- formula(Lynx~Treatment | 1) #where the count distribution is modelled as a function of treatment, binomial distrib. modelled as constant
# ZIP
Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = pilot.month) 
summary(Zip1) 
# ZINB
Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = pilot.month)
summary(Nb1)

# Model selection
AIC(m0.lynx, m1.lynx, m2.lynx, m3.lynx, m4.lynx, m5.lynx, Zip1, Nb1) ## m2 has relatively greatest expl. power

### Zero-inflated glmms with glmmADMB
ZIPmer.lynx <- glmmadmb(Lynx~Treatment + (1| Site), data = pilot.month, family = "poisson", link = "log", zeroInflation = TRUE)
# Function maximiser failed
ZINBmer.lynx <- glmmadmb(Lynx~Treatment + (1| Site), data = pilot.month, family = "nbinom", link = "log", zeroInflation = TRUE)
# Function maximiser failed



### For all species except lynx, negative binomial glmm with a random effect of site has lowest AIC (including random effect of Yr_Month decresases further but may not make sense)
