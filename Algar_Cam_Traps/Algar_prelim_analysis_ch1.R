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

Site_rep <- rep(sites,372) ## Repeat site names 372 times (# days in study period)
length(Site_rep)
head(Site_rep)

study.days_rep <- rep(study.days, 24) ## Repeat study days 24 times (# of camera sites)
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
d.deer <- data2 %>%
  filter(Species == "O_virginianus") %>% ##Select deer data only
  group_by(Station, StudyDay) %>%        ## Arrange data first by Station, then by StudyDay (both in ascending order)
  summarise(sum(count, na.rm = TRUE))    ## Summarise the detection count for that day, add to new column in data frame
colnames(d.deer) <- c("Site","StudyDay","WTDeer") ##Renaming columns (ignore Warnings, function will add colname)
glimpse(d.deer) #207 days of deer detections (where some days have multiple detections)
plot(d.deer$StudyDay, d.deer$WTDeer, xlim=c(0,350)) ##Most days have 1 detection, some 2, 2 days have 3 and 2 days have 5

d.bear <- data2 %>%
  filter(Species == "U_americanus") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.bear) <- c("Site","StudyDay","Blackbear")
glimpse(d.bear) #134 obs
plot(d.bear$StudyDay, d.bear$Blackbear, xlim=c(0,350))
head(d.bear[order(d.bear$StudyDay),]) ##First detections of bears within this deployment
tail(d.bear[order(d.bear$StudyDay),]) ##Last detections of bears within this deployment

d.caribou <- data2 %>%
  filter(Species == "R_tarandus") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.caribou) <- c("Site","StudyDay","Caribou")
glimpse(d.caribou) #41 obs
plot(d.caribou$StudyDay, d.caribou$Caribou, xlim=c(0,350))

d.moose <- data2 %>%
  filter(Species == "A_alces") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.moose) <- c("Site","StudyDay","Moose")
glimpse(d.moose) #28 obs
plot(d.moose$StudyDay, d.moose$Moose, xlim=c(0,350))

d.coyote <- data2 %>%
  filter(Species == "C_latrans") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.coyote) <- c("Site","StudyDay","Coyote")
glimpse(d.coyote) #46 obs
plot(d.coyote$StudyDay, d.coyote$Coyote, xlim=c(0,350))

d.lynx <- data2 %>%
  filter(Species == "L_canadensis") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.lynx) <- c("Site","StudyDay","Lynx")
glimpse(d.lynx) # 30 obs
plot(d.lynx$StudyDay, d.lynx$Lynx, xlim=c(0,350))

d.wolf <- data2 %>%
  filter(Species == "C_lupus") %>%
  group_by(Station, StudyDay) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.wolf) <- c("Site","StudyDay","Wolf")
glimpse(d.wolf) # 91 obs
plot(d.wolf$StudyDay, d.wolf$Wolf, xlim=c(0,350))

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

data3[is.na(data3)] <- 0 ##Converting NAs to 0's
summary(data3)
nrow(data3)

sum(data3$WTDeer) #244
sum(data3$Blackbear) #149
sum(data3$Caribou) #44
sum(data3$Coyote) #51
sum(data3$Lynx) #31
sum(data3$Wolf) #99
sum(data3$Moose) #32

####--- aggregate species detection data by month ####
data$Site_SD <- paste(data$Station,data$StudyDay)
data3$StudyDay.date <- as.Date(data3$StudyDay,origin = "2015-11-04")
data3$Year <- as.factor(format(as.Date(data3$StudyDay.date), "%Y")) ##Separating date info into year, month, and year_month (to distinguish Nov.2015 from Nov. 2016)
data3$Month <- as.factor(format(as.Date(data3$StudyDay.date), "%b"))
data3$Yr_Month <- as.factor(format(as.Date(data3$StudyDay.date), "%Y-%m"))

head(data3)
summary(data3)
levels(data3$Yr_Month)

sum(data3$WTDeer)#244
sum(data3$Blackbear) #149
sum(data3$Caribou) #44
sum(data3$Coyote) #51
sum(data3$Lynx) #31
sum(data3$Wolf) #99
sum(data3$Moose) #32

write.csv(data3, "2015.01_detections_day.csv")

m.deer <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(WTDeer, na.rm = TRUE))
colnames(m.deer) <- c("Site","Treatment","Yr_Month","WTDeer")
summary(m.deer) #max 21 obs in one month
sum(m.deer$WTDeer) #244 - matches

m.bear <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Blackbear, na.rm = TRUE))
colnames(m.bear) <- c("Site","Treatment","Yr_Month","Blackbear")
summary(m.bear) # max 12 obs in one month
sum(m.bear$Blackbear) #149 - matches

m.caribou <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Caribou, na.rm = TRUE))
colnames(m.caribou) <- c("Site","Treatment","Yr_Month","Caribou")
summary(m.caribou) # max 6 obs in one month
sum(m.caribou$Caribou) #44 - matches

m.moose <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Moose, na.rm = TRUE))
colnames(m.moose) <- c("Site","Treatment","Yr_Month","Moose")
summary(m.moose) # max 4 obs in one month
sum(m.moose$Moose) #32 - matches


m.coyote <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Coyote, na.rm = TRUE))
colnames(m.coyote) <- c("Site","Treatment","Yr_Month","Coyote")
summary(m.coyote) # max 9 obs in one month
sum(m.coyote$Coyote) #51 - matches

m.lynx <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Lynx, na.rm = TRUE))
colnames(m.lynx) <- c("Site","Treatment","Yr_Month","Lynx")
summary(m.lynx) # max 3 obs in one month
sum(m.lynx$Lynx) #31 - matches

m.wolf <- data3 %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Wolf, na.rm = TRUE))
colnames(m.wolf) <- c("Site","Treatment","Yr_Month","Wolf")
summary(m.wolf) # max 9 obs in one month
sum(m.wolf$Wolf) #99 - matches

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

sum(data.month$WTDeer)#244
sum(data.month$Blackbear) #149
sum(data.month$Caribou) #44
sum(data.month$Coyote) #51
sum(data.month$Lynx) #31
sum(data.month$Wolf) #99
sum(data.month$Moose) #32

summary(data.month)


write.csv(data.month, "2015.01_monthlydetections.csv")

#### Checking data for errors + data exploration (pilot data so it's okay)
hist(data.month$Wolf)
hist(data.month$Blackbear)
hist(data.month$Coyote)
hist(data.month$Lynx)
hist(data.month$Caribou)
hist(data.month$WTDeer)
hist(data.month$Moose)

plot(data.month$Treatment, data.month$Blackbear)
plot(data.month$Yr_Month, data.month$Blackbear) ##Active between 2016-04 and 2016-10

plot(data.month$Treatment, data.month$Wolf)
plot(data.month$Yr_Month, data.month$Wolf)

plot(data.month$Treatment, data.month$Coyote)
plot(data.month$Yr_Month, data.month$Coyote)

plot(data.month$Treatment, data.month$Lynx)
plot(data.month$Yr_Month, data.month$Lynx)

plot(data.month$Treatment, data.month$Caribou)
plot(data.month$Yr_Month, data.month$Caribou) ##more detections between 2016-04 and 2016-09

plot(data.month$Treatment, data.month$WTDeer)
plot(data.month$Yr_Month, data.month$WTDeer)

plot(data.month$Treatment, data.month$Moose)
plot(data.month$Yr_Month, data.month$Moose)

#### Linear regressions (differ from Jo's Bayesian analysis) ####
### Check if there is a difference in species use of Control and SPP lines
library(lme4)
library(MASS)

#### Wolf modelling: Compare glmm to glm, adding 2 random effects, checking out zero-inflated Poisson and Neg. binomial GLMs ####
##Poisson glm
m0.wolf <- glm(Wolf~Treatment, family = poisson, data = data.month)
## Negative binomial glm
m1.wolf <- glm.nb(Wolf~Treatment, link = "log", data = data.month) # Use log link to compare with poisson

## Poisson GLMM
m2.wolf <- glmer(Wolf~Treatment + (1|Site), family = poisson, data = data.month)

## Negative binomial GLMM
m3.wolf <- glmer.nb(Wolf~Treatment + (1|Site), data = data.month)

anova(m0.wolf, m2.wolf)##m2 has lower AIC score, higher logLikelihood --> enough additional variance explained to justify more complexity
AIC(m0.wolf, m1.wolf, m2.wolf, m3.wolf) ## m3 has lowest AIC
anova(m2.wolf, m3.wolf) ## m3 AIC significantly lower than m2 AIC
summary(m3.wolf)

### Negative binomial GLMM has  highest explanatory power



## Intercept-free model, should be same as m1.wolf, just comparing 2 treatment estimates to zero rather than to each other
m2.wolf <- glmer(Wolf~Treatment + (1|Site) -1, family = poisson, data = data.month) 
summary(m2.wolf)

### Adding second random effect of time
m4.wolf <- glmer(Wolf~Treatment + (1|Site) + (1|Yr_Month), family = poisson, data = data.month)
summary(m4.wolf) ## Yr_Month random variable doesn't account for much variance....
anova(m2.wolf, m3.wolf, m4.wolf) ##m3 still wins
m5.wolf <- glmer.nb(Wolf~Treatment + (1 |Site) + (1|Yr_Month), data = data.month)
anova(m2.wolf,m3.wolf, m4.wolf, m5.wolf) ## m5 < m3
summary(m5.wolf)

##Zero-inflated GLMs
library(pscl)
f1 <- formula(Wolf~Treatment | 1) #where the count distribution is modelled as a function of treatment, binomial distrib. modelled as constant
# ZIP
Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = data.month) 
summary(Zip1) ## SPP estimate is sig. different from Control in the count model
# ZINB
Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = data.month)
summary(Nb1)


# Model selection
AIC(m0.wolf, m1.wolf, m2.wolf, m3.wolf, m4.wolf, m5.wolf, Zip1, Nb1) ## Still m5 < m3 < m4... Negative binomial distribution decreases AIC more than adding 2nd random variable


#### Blackbear modelling: Compare glmm to glm, adding 2 random effects, checking out zero-inflated Poisson and Neg. binomial GLMs ####
##Poisson glm
m0.bear <- glm(Blackbear~Treatment, family = poisson, data = data.month)
## Negative binomial glm
m1.bear <- glm.nb(Blackbear~Treatment, link = "log", data = data.month) # Use log link to compare with poisson

## Poisson GLMM
m2.bear <- glmer(Blackbear~Treatment + (1|Site), family = poisson, data = data.month)

## Negative binomial GLMM
m3.bear <- glmer.nb(Blackbear~Treatment + (1|Site), data = data.month)

anova(m0.bear, m2.bear)##m2 has lower AIC score, higher logLikelihood --> enough additional variance explained to justify more complexity
AIC(m0.bear, m1.bear, m2.bear, m3.bear) ## m3 has lowest AIC, followed by m1
anova(m2.bear, m3.bear) ## m3 AIC significantly lower than m2 AIC, Can't compare glm.nb to glmm.nb

summary(m3.bear)

### Negative binomial GLMM has  highest explanatory power



## Intercept-free model, should be same as m1.bear, just comparing 2 treatment estimates to zero rather than to each other
m2.bear <- glmer(Blackbear~Treatment + (1|Site) -1, family = poisson, data = data.month) 
summary(m2.bear)

### Adding second random effect of time
m4.bear <- glmer(Blackbear~Treatment + (1|Site) + (1|Yr_Month), family = poisson, data = data.month)
summary(m4.bear) ## Yr_Month random variable accounts for LOTS of variance
anova(m2.bear, m3.bear, m4.bear) ##m3 and m4 significantly lower than m2, m4 = 445 vs. 503
m5.bear <- glmer.nb(Blackbear~Treatment + (1 |Site) + (1|Yr_Month), data = data.month)
anova(m2.bear, m3.bear, m4.bear, m5.bear)
summary(m5.bear)

##Zero-inflated GLMs
library(pscl)
f1 <- formula(Blackbear~Treatment | 1) #where the count distribution is modelled as a function of treatment, binomial distrib. modelled as constant
# ZIP
Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = data.month) 
summary(Zip1) ## SPP estimate is sig. different from Control in the count model
# ZINB
Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = data.month)
summary(Nb1)

# Model selection
AIC(m0.bear, m1.bear, m2.bear, m3.bear, m4.bear, m5.bear, Zip1, Nb1) ## m5 < m4 < m3

## Negative binomial with 2 random variables wins

#### Coyote modelling: Compare glmm to glm, adding 2 random effects, checking out zero-inflated Poisson and Neg. binomial GLMs ####
##Poisson glm
m0.coyote <- glm(Coyote~Treatment, family = poisson, data = data.month)
## Negative binomial glm
m1.coyote <- glm.nb(Coyote~Treatment, link = "log", data = data.month) # Use log link to compare with poisson

## Poisson GLMM
m2.coyote <- glmer(Coyote~Treatment + (1|Site), family = poisson, data = data.month)

## Negative binomial GLMM
m3.coyote <- glmer.nb(Coyote~Treatment + (1|Site), data = data.month)


AIC(m0.coyote, m1.coyote, m2.coyote, m3.coyote) ## m3 < m2 < m1
anova(m2.coyote, m3.coyote) ## m3 AIC significantly lower than m2 AIC, Can't compare glm.nb to glmm.nb

summary(m3.coyote)

### Negative binomial GLMM has  highest explanatory power



## Intercept-free model, should be same as m1.coyote, just comparing 2 treatment estimates to zero rather than to each other
m2.coyote <- glmer(Coyote~Treatment + (1|Site) -1, family = poisson, data = data.month) 
summary(m2.coyote)

### Adding second random effect of time
m4.coyote <- glmer(Coyote~Treatment + (1|Site) + (1|Yr_Month), family = poisson, data = data.month)
summary(m4.coyote) ## Yr_Month random variable accounts for LOTS of variance
anova(m2.coyote, m3.coyote, m4.coyote) ##m3  significantly lower than m2, m3 = 235 and m4 = 236
m5.coyote <- glmer.nb(Coyote~Treatment + (1 |Site) + (1|Yr_Month), data = data.month) ## Warning message:
#                                                                                        In theta.ml(Y, mu, weights = object@resp$weights, limit = #                                                                                        limit,:iteration limit reached
anova(m2.coyote, m3.coyote, m4.coyote, m5.coyote)
summary(m5.coyote)

##Zero-inflated GLMs
library(pscl)
f1 <- formula(Coyote~Treatment | 1) #where the count distribution is modelled as a function of treatment, binomial distrib. modelled as constant
# ZIP
Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = data.month) 
summary(Zip1) ## SPP estimate is sig. different from Control in the count model
# ZINB
Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit", data = data.month)
summary(Nb1)

# Model selection
AIC(m0.coyote, m1.coyote, m2.coyote, m3.coyote, m4.coyote, m5.coyote, Zip1, Nb1) ## m5 < m3 < m4, but both negbin models failed to converge (m5 and m3)
