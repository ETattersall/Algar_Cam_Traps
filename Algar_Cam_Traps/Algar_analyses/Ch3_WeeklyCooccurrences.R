####################################
## Ch3_MonthlyCoocurrences.R
## Started 13 Dec.2018 by Erin
###################################

library(ggplot2)
library(bbmle)
library(glmmTMB)
library(dplyr)
library(tidyr)
library(camtrapR)
library(data.table)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

### 1. Read in necessary data: 
#### daily detections to sum as weeks
day <- read.csv("Algar_DailyDetections_30months.csv")
####  habitat variables, linear density
hab <- read.csv("Algar_HabitatData_8scales.csv")
lin <- read.csv("AlgarStationLineDensity_8scales.csv")
str(hab)
hab$X <- NULL
str(lin)
lin$X <- NULL


str(day)
day[,1:2] <- NULL
head(day)
day$Datep <- as.POSIXct(strptime(day$Datep, format = "%Y-%m-%d"))
str(day)

### 1. Aggregating by month and week
day$Yr_Month <- as.factor(format(as.Date(day$Datep), "%Y-%m"))
day$Yr_Week <- as.factor(format(as.Date(day$Datep), "%Y-%V")) ## Problem is that some january dates are still marked as week 53, but in the new year. Change with ifelse statement
day$Mon_Week <- as.factor(format(as.Date(day$Datep), "%m-%V"))
day$Year <- as.factor(format(as.Date(day$Datep), "%Y"))
day$Month <- as.factor(format(as.Date(day$Datep), "%m"))
day$Week <- as.factor(format(as.Date(day$Datep), "%V"))
str(day)

day$Yr_Week2 <- day$Yr_Week
## No week 53 in Dec 2016, so all 2016-53 can be switched to 2015-03
day$Yr_Week2[day$Yr_Week2 == "2016-53"] <- "2015-53"
## 2017: Yr_Week == 2017-52 in both January and December
day$Yr_Week2[day$Yr_Week2 == "2017-52" & day$Mon_Week == "01-52"] <- "2016-52"
## 2018: No 2018-52; Jan1 is 2018-01

day$Yr_Week <- day$Yr_Week2
str(day)
day[,19:23] <- NULL

summary(day$Yr_Week)
head(day)

### Condense to seismic data only
day <- day %>% filter(Treatment != "OffLine")

## 2. Add snow data
sno <- read.csv("Algar60_speciesOccurrence.csv") ## Taking corrected snow data from here
str(sno)

day$Snow <- sno$Snow[match(day$Site_SD,sno$Site_SD)]
str(day)
summary(day$Snow)
table(day$Snow)


## 3. Aggregating by Yr_Week for weekly occurrences: sum detections over the week, take mean CamActive and mean Snow
WTDWK <- aggregate(WTDeer~Site+Yr_Week, data = day, sum)
BBWK <- aggregate(Blackbear~Site+Yr_Week, data = day, sum)
CaribouWK <- aggregate(Caribou~Site+Yr_Week, data = day, sum)
MooseWK <- aggregate(Moose~Site+Yr_Week, data = day, sum)
CoyoteWK <- aggregate(Coyote~Site+Yr_Week, data = day, sum)
LynxWK <- aggregate(Lynx~Site+Yr_Week, data = day, sum)
WolfWK <- aggregate(Wolf~Site+Yr_Week, data = day, sum)
HareWK <- aggregate(Hare~Site+Yr_Week, data = day, sum)
HumanWK <- aggregate(Human~Site+Yr_Week, data = day, sum)
SquirrelWK <- aggregate(Squirrel~Site+Yr_Week, data = day, sum)

## Mean of CamActive and Snow
CamActiveWK <- aggregate(CamActive~Site+Yr_Week, data = day, mean)
SnowWK <- aggregate(Snow~Site+Yr_Week, data = day, mean, na=TRUE) #4904 observations
str(SnowWK)
summary(SnowWK)

Week <- cbind.data.frame(WTDWK, BBWK[,3], CaribouWK[,3], MooseWK[,3], CoyoteWK[,3], LynxWK[,3], WolfWK[,3], HareWK[,3], SquirrelWK[,3], HumanWK[,3], CamActiveWK[,3]) ## 7620 observations, but SnowWK only has 4643

summary(Week)

## Match snow to combine
Week$Site_Ywk <- paste(Week$Site, Week$Yr_Week)
SnowWK$Site_Ywk <- paste(SnowWK$Site, SnowWK$Yr_Week)

Week$Snow <- SnowWK$Snow[match(Week$Site_Ywk, SnowWK$Site_Ywk)]
summary(Week) ## 2977 NAs

head(Week, 200)
tail(Week,200)
## NAs in Snow correspond to weeks when camera was inactive. Remove those rows by converting to NA then na.omit
Week$CamActive[Week$CamActive==0] <- NA
summary(Week) ## Still 63 rows where Snow = NA but CamActive does not. Omit anyway (Will be excluded from analyses anyhow)

Week <- na.omit(Week)

summary(Week)
str(Week)
colnames(Week) <- c("Site", "Yr_Week", "WTDeer", "Blackbear", "Caribou", "Moose", "Coyote", "Lynx", "Wolf", "Hare", "Squirrel", "Human", "CamActive", "SiteYwk", "Snow")
str(Week) ## 4643 observations

### 4. Determine distributions of response variables (sampling unit = Detections/site/week currently)
hist(Week$Blackbear)
sum(Week$Blackbear) ## 323
hist(Week$Lynx)
sum(Week$Lynx) ## 72 --> total occurrences is 71 (i.e. one day of 2 detections)
hist(Week$Coyote)
sum(Week$Coyote) ## 149

## Changing to occurrences does not get rid of much data, plus will avoid overdispersion in the variables (Bernoulli distrib cannot be overdispersed)
WeekOcc <- as.data.frame(ifelse(Week[ , 3:12] > 0, 1, 0))
WeekOcc <- cbind(Week[,1:2], WeekOcc, Week[,13:15])
summary(WeekOcc)


#### 5. Create 'Prey' for all 3
WeekOcc$Prey_Blackbear <- ifelse(WeekOcc$WTDeer==1 | WeekOcc$Caribou==1 | WeekOcc$Moose==1, 1, 0)
WeekOcc$Prey_Lynx <- ifelse(WeekOcc$Hare==1 | WeekOcc$Squirrel==1, 1, 0)
WeekOcc$Prey_Coyote <- ifelse(WeekOcc$Hare==1 | WeekOcc$Squirrel==1 | WeekOcc$WTDeer, 1, 0)

summary(WeekOcc)
sum(WeekOcc$Prey_Blackbear)
sum(WeekOcc$Prey_Coyote)
sum(WeekOcc$Prey_Lynx)

### Will use binomial glmms with random effect for site. Also include one for week (would assume that detections in the same week were more similar than those in others)?? 
## NO: Week is short enough that there shouldn't be any biological reason why hetergeneity would differ within and across weeks
## Verify with plots
plot(WeekOcc$Yr_Week, WeekOcc$Coyote)
plot(WeekOcc$Yr_Week, WeekOcc$Lynx)
plot(WeekOcc$Yr_Week, WeekOcc$Blackbear) ## Need to determine week cut offs for bears...

### 6. Combining spatial data by Site (Site column needs to be same across data sets, and the other two have CamStation)
colnames(WeekOcc)[1] <- "CamStation"
str(WeekOcc)
str(hab) #columns 7-61
hab2 <- cbind(hab$CamStation, hab[, 7:61])
str(hab2)
colnames(hab2)[1] <- "CamStation"
str(hab2)
str(lin)

WeekOcc2 <- merge(WeekOcc, hab2, by = "CamStation")
str(WeekOcc2)
WeekOcc2 <- merge(WeekOcc2, lin, by = "CamStation")
str(WeekOcc2)
## Remove SiteYwk
WeekOcc2$SiteYwk <- NULL

## 7. Scaling covariates --> scale all except co-occurrences
covscale <- function(covariate){
  centre <- (covariate - mean(covariate, na.rm = TRUE)) # where covariate is a vector of numeric input values for one covariate
  standardize <- centre/(2*sd(covariate, na.rm = TRUE)) # Divide centred value by 2 SD
}

WeekOcc_sc <- cbind.data.frame(WeekOcc2[,1:12], lapply(WeekOcc2[,13:14], covscale), WeekOcc2[,15:17], lapply(WeekOcc2[,18:80], covscale)) ## combining scaled columns with non-scaled columns
str(WeekOcc_sc)
summary(WeekOcc_sc)

### 9. Truncate Bear data-- roughly week 19 and 45 correspond to beginning of April/ end of October?
day$Yr_Week[which(day$Datep== "2016-04-01")] #13
day$Yr_Week[which(day$Datep== "2017-10-31")] #44

## Yr_Week needs to be date format
class(day$Yr_Week)
WeekOcc_sc <- separate(WeekOcc_sc, Yr_Week, into = c("Year", "Week"), sep = "-", remove = FALSE)
class(WeekOcc_sc$Week)
WeekOcc_sc$Week <- as.numeric(WeekOcc_sc$Week)

str(WeekOcc_sc)
head(WeekOcc_sc)
summary(WeekOcc_sc$Week)

## Order by Station and Yr_Week
WeekOcc_sc <- WeekOcc_sc[order(WeekOcc_sc$CamStation, WeekOcc_sc$Yr_Week), ]
head(WeekOcc_sc)

BearWeek_sc <- WeekOcc_sc %>% filter(WeekOcc_sc$Week > 12 & WeekOcc_sc$Week < 45)
  #subset(WeekOcc_sc, WeekOcc_sc$Week >=13 & WeekOcc_sc$Week <= 44)
summary(BearWeek_sc$Yr_Week)


### 8. Scale Analysis
## A. Blackbear
Blackbearhab0 <- glmmTMB(Blackbear~ CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
Blackbearhab250 <- glmmTMB(Blackbear~LowCon250 + UpCon250 + UpDecid250 + Tamarack250 + pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
Blackbearhab500 <- glmmTMB(Blackbear~LowCon500 + UpCon500 + UpDecid500 + Tamarack500 + LowDecid500 + pOpen500 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
Blackbearhab750 <- glmmTMB(Blackbear~LowCon750 + UpCon750 + UpDecid750 + Tamarack750 + LowDecid750 + pOpen750 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
Blackbearhab1000 <- glmmTMB(Blackbear~LowCon1000 + UpCon1000 + UpDecid1000 + Tamarack1000 + LowDecid1000 + pOpen1000 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
Blackbearhab1250 <- glmmTMB(Blackbear~LowCon1250 + UpCon1250 + UpDecid1250 + Tamarack1250 + LowDecid1250 + pOpen1250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
Blackbearhab1500 <- glmmTMB(Blackbear~LowCon1500 + UpCon1500 + UpDecid1500 + Tamarack1500 + LowDecid1500 + pOpen1500 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
Blackbearhab1750 <- glmmTMB(Blackbear~LowCon1750 + UpCon1750 + UpDecid1750 + Tamarack1750 + LowDecid1750 + pOpen1750 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
Blackbearhab2000 <- glmmTMB(Blackbear~LowCon2000 + UpCon2000 + UpDecid2000 + Tamarack2000 + LowDecid2000 + pOpen2000 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)

Blackbear.scale <- ICtab(Blackbearhab0, Blackbearhab250, Blackbearhab500, Blackbearhab750, Blackbearhab1000, Blackbearhab1250, Blackbearhab1500, Blackbearhab1750, Blackbearhab2000, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

Blackbear.scale
###                dLogLik dAIC df weight
# Blackbearhab250  6.9     0.0  8  0.578 
# Blackbearhab0    0.0     3.8  3  0.088 
# Blackbearhab2000 5.7     4.4  9  0.065 
# Blackbearhab750  5.6     4.6  9  0.059 
# Blackbearhab1750 5.5     4.8  9  0.053 
# Blackbearhab500  5.4     5.1  9  0.046 
# Blackbearhab1000 5.3     5.2  9  0.044 
# Blackbearhab1500 5.1     5.5  9  0.036 
# Blackbearhab1250 5.0     5.9  9  0.031

summary(Blackbearhab250) #pOpen is most significant but all other habitat variables are close

## A. Lynx
Lynxhab0 <- glmmTMB(Lynx~ CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
Lynxhab250 <- glmmTMB(Lynx~LowCon250 + UpCon250 + UpDecid250 + Tamarack250 + pOpen250 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
Lynxhab500 <- glmmTMB(Lynx~LowCon500 + UpCon500 + UpDecid500 + Tamarack500 + LowDecid500 + pOpen500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
Lynxhab750 <- glmmTMB(Lynx~LowCon750 + UpCon750 + UpDecid750 + Tamarack750 + LowDecid750 + pOpen750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
Lynxhab1000 <- glmmTMB(Lynx~LowCon1000 + UpCon1000 + UpDecid1000 + Tamarack1000 + LowDecid1000 + pOpen1000 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
Lynxhab1250 <- glmmTMB(Lynx~LowCon1250 + UpCon1250 + UpDecid1250 + Tamarack1250 + LowDecid1250 + pOpen1250 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
Lynxhab1500 <- glmmTMB(Lynx~LowCon1500 + UpCon1500 + UpDecid1500 + Tamarack1500 + LowDecid1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
Lynxhab1750 <- glmmTMB(Lynx~LowCon1750 + UpCon1750 + UpDecid1750 + Tamarack1750 + LowDecid1750 + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
Lynxhab2000 <- glmmTMB(Lynx~LowCon2000 + UpCon2000 + UpDecid2000 + Tamarack2000 + LowDecid2000 + pOpen2000 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)

Lynx.scale <- ICtab(Lynxhab0, Lynxhab250, Lynxhab500, Lynxhab750, Lynxhab1000, Lynxhab1250, Lynxhab1500, Lynxhab1750, Lynxhab2000, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

Lynx.scale
##            dLogLik dAIC df weight
# Lynxhab1500 19.5     0.0 9  0.5089
# Lynxhab1750 19.0     1.1 9  0.2949
# Lynxhab2000 18.0     3.0 9  0.1110
# Lynxhab1250 17.0     5.0 9  0.0428
# Lynxhab1000 16.0     7.0 9  0.0155
# Lynxhab250  14.7     7.6 8  0.0113
# Lynxhab750  15.5     7.9 9  0.0096
# Lynxhab500  15.1     8.8 9  0.0061
# Lynxhab0     0.0    27.0 3  <0.001

summary(Lynxhab1500) ## LowCon, UpCon, pOpen

## A. Coyote
Coyotehab0 <- glmmTMB(Coyote~ CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
Coyotehab250 <- glmmTMB(Coyote~LowCon250 + UpCon250 + UpDecid250 + Tamarack250 + pOpen250 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
Coyotehab500 <- glmmTMB(Coyote~LowCon500 + UpCon500 + UpDecid500 + Tamarack500 + LowDecid500 + pOpen500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
Coyotehab750 <- glmmTMB(Coyote~LowCon750 + UpCon750 + UpDecid750 + Tamarack750 + LowDecid750 + pOpen750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
Coyotehab1000 <- glmmTMB(Coyote~LowCon1000 + UpCon1000 + UpDecid1000 + Tamarack1000 + LowDecid1000 + pOpen1000 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
Coyotehab1250 <- glmmTMB(Coyote~LowCon1250 + UpCon1250 + UpDecid1250 + Tamarack1250 + LowDecid1250 + pOpen1250 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
Coyotehab1500 <- glmmTMB(Coyote~LowCon1500 + UpCon1500 + UpDecid1500 + Tamarack1500 + LowDecid1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
Coyotehab1750 <- glmmTMB(Coyote~LowCon1750 + UpCon1750 + UpDecid1750 + Tamarack1750 + LowDecid1750 + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
Coyotehab2000 <- glmmTMB(Coyote~LowCon2000 + UpCon2000 + UpDecid2000 + Tamarack2000 + LowDecid2000 + pOpen2000 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)

Coyote.scale <- ICtab(Coyotehab0, Coyotehab250, Coyotehab500, Coyotehab750, Coyotehab1000, Coyotehab1250, Coyotehab1500, Coyotehab1750, Coyotehab2000, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

Coyote.scale
##             dLogLik dAIC df weight
# Coyotehab1750 17.6     0.0 9  0.4973
# Coyotehab2000 17.5     0.3 9  0.4336
# Coyotehab1500 15.4     4.4 9  0.0538
# Coyotehab1250 13.6     8.0 9  0.0090
# Coyotehab250  11.7     9.8 8  0.0036
# Coyotehab1000 12.2    10.8 9  0.0022
# Coyotehab750  10.4    14.6 9  <0.001
# Coyotehab500   8.7    17.8 9  <0.001
# Coyotehab0     0.0    23.3 3  <0.001


summary(Coyotehab1750) ## pOpen

## All same scales and variables as daily analysis!!

### 9. Linear Density
## A. Blackbear
BlackbearLD0 <- glmmTMB(Blackbear~ pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
BlackbearLD250 <- glmmTMB(Blackbear~ X250m + pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
BlackbearLD500 <- glmmTMB(Blackbear~X500m + pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
BlackbearLD750 <- glmmTMB(Blackbear~X750m + pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
BlackbearLD1000 <- glmmTMB(Blackbear~X1000m + pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
BlackbearLD1250 <- glmmTMB(Blackbear~X1250m + pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
BlackbearLD1500 <- glmmTMB(Blackbear~X1500m + pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
BlackbearLD1750 <- glmmTMB(Blackbear~X1750m + pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
BlackbearLD2000 <- glmmTMB(Blackbear~X2000m + pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)

BlackbearLD <- ICtab(BlackbearLD0, BlackbearLD250, BlackbearLD500, BlackbearLD750, BlackbearLD1000, BlackbearLD1250, BlackbearLD1500, BlackbearLD1750, BlackbearLD2000, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

BlackbearLD
##                dLogLik dAIC df weight
# BlackbearLD1500 2.9     0.0  5  0.271 
# BlackbearLD1750 2.4     1.0  5  0.163 
# BlackbearLD2000 2.3     1.1  5  0.158 
# BlackbearLD1250 2.2     1.3  5  0.138 
# BlackbearLD1000 1.8     2.1  5  0.097 
# BlackbearLD750  1.4     2.9  5  0.064 
# BlackbearLD0    0.0     3.8  4  0.041 
# BlackbearLD500  0.8     4.1  5  0.035 
# BlackbearLD250  0.8     4.2  5  0.032

## B. Lynx
LynxLD0 <- glmmTMB(Lynx~LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxLD250 <- glmmTMB(Lynx~ X250m + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxLD500 <- glmmTMB(Lynx~X500m + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxLD750 <- glmmTMB(Lynx~X750m + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxLD1000 <- glmmTMB(Lynx~X1000m + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxLD1250 <- glmmTMB(Lynx~X1250m + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxLD1500 <- glmmTMB(Lynx~X1500m + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxLD1750 <- glmmTMB(Lynx~X1750m + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxLD2000 <- glmmTMB(Lynx~X2000m + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)

LynxLD <- ICtab(LynxLD0, LynxLD250, LynxLD500, LynxLD750, LynxLD1000, LynxLD1250, LynxLD1500, LynxLD1750, LynxLD2000, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

LynxLD
##          LogLik dAIC df weight
# LynxLD1750 3.5     0.0  7  0.271 
# LynxLD2000 3.2     0.6  7  0.197 
# LynxLD1000 2.8     1.5  7  0.130 
# LynxLD1250 2.7     1.6  7  0.119 
# LynxLD1500 2.7     1.7  7  0.113 
# LynxLD750  2.6     1.9  7  0.103 
# LynxLD500  1.4     4.3  7  0.031 
# LynxLD0    0.0     5.1  6  0.021 
# LynxLD250  0.5     6.1  7  0.013

## C. Coyote
CoyoteLD0 <- glmmTMB(Coyote~ pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteLD250 <- glmmTMB(Coyote~ X250m + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteLD500 <- glmmTMB(Coyote~X500m + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteLD750 <- glmmTMB(Coyote~X750m + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteLD1000 <- glmmTMB(Coyote~X1000m + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteLD1250 <- glmmTMB(Coyote~X1250m + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteLD1500 <- glmmTMB(Coyote~X1500m + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteLD1750 <- glmmTMB(Coyote~X1750m + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteLD2000 <- glmmTMB(Coyote~X2000m + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)

CoyoteLD <- ICtab(CoyoteLD0, CoyoteLD250, CoyoteLD500, CoyoteLD750, CoyoteLD1000, CoyoteLD1250, CoyoteLD1500, CoyoteLD1750, CoyoteLD2000, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

CoyoteLD

##            dLogLik dAIC df weight
# CoyoteLD1500 10.4     0.0 5  0.334 
# CoyoteLD1750 10.3     0.1 5  0.312 
# CoyoteLD2000  9.7     1.4 5  0.164 
# CoyoteLD1250  9.6     1.6 5  0.148 
# CoyoteLD1000  8.1     4.6 5  0.034 
# CoyoteLD750   6.5     7.7 5  0.007 
# CoyoteLD500   3.1    14.6 5  <0.001
# CoyoteLD0     0.0    18.8 4  <0.001
# CoyoteLD250   0.9    19.0 5  <0.001

### Same scales again

### 10. Co-occurrence models
## A. Blackbears
# Raw co-occurrence
nrow(BearWeek_sc[BearWeek_sc$Blackbear>0 & BearWeek_sc$Wolf>0, ]) ## 33 co-occurrences of 2122 observations
nrow(BearWeek_sc[BearWeek_sc$Blackbear>0 & BearWeek_sc$Prey_Blackbear>0, ]) ## 55 co-occurrences

BlackbearCo0 <- glmmTMB(Blackbear ~ pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
BlackbearCo0LD <- glmmTMB(Blackbear ~ X1500m + pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
BlackbearCo1 <- glmmTMB(Blackbear~ Wolf + pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
BlackbearCo1LD1 <- glmmTMB(Blackbear ~ Wolf + X1500m + pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
BlackbearCo1LD2 <- glmmTMB(Blackbear ~ Wolf * X1500m + pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
BlackbearCo2 <- glmmTMB(Blackbear ~ Prey_Blackbear + pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
BlackbearCo2LD1 <- glmmTMB(Blackbear ~ Prey_Blackbear + X1500m + pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)
BlackbearCo2LD2 <- glmmTMB(Blackbear~Prey_Blackbear * X1500m + pOpen250 + CamActive + (1|CamStation), data = BearWeek_sc, family = binomial)

BlackbearCo <- ICtab(BlackbearCo0, BlackbearCo0LD, BlackbearCo1, BlackbearCo1LD1, BlackbearCo1LD2, BlackbearCo2, BlackbearCo2LD1, BlackbearCo2LD2,  type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

BlackbearCo
##               dLogLik dAIC df weight
# BlackbearCo1LD1  7.2     0.0 6  0.5338
# BlackbearCo1LD2  7.5     1.2 7  0.2870
# BlackbearCo1     4.3     3.7 5  0.0840
# BlackbearCo2LD1  4.7     5.0 6  0.0439
# BlackbearCo2LD2  4.9     6.5 7  0.0212
# BlackbearCo0LD   2.9     6.6 5  0.0200
# BlackbearCo2     1.8     8.6 5  0.0071
# BlackbearCo0     0.0    10.3 4  0.0031

summary(BlackbearCo1LD1) ## Wolf +ve, LD -ve
summary(BlackbearCo1LD2) ## Same, non-sig. interaction
summary(BlackbearCo2LD1) ## Prey almost significant (p = 0.05)

## B. Lynx
nrow(WeekOcc_sc[WeekOcc_sc$Lynx>0 & WeekOcc_sc$Wolf>0, ]) # 5 co-occurrences
nrow(WeekOcc_sc[WeekOcc_sc$Lynx>0 & WeekOcc_sc$Coyote>0, ])## 6 co-occurrences of 2122 observations
nrow(WeekOcc_sc[WeekOcc_sc$Lynx>0 & WeekOcc_sc$Prey_Lynx>0, ]) ## 10 co-occurrences

LynxCo0 <- glmmTMB(Lynx~LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxCo0LD <- glmmTMB(Lynx~ X1750m + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxCo0Sn <- glmmTMB(Lynx~ Snow + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxCo1 <- glmmTMB(Lynx~Wolf + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxCo1Sn1 <- glmmTMB(Lynx~Wolf + Snow + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxCo1Sn2 <- glmmTMB(Lynx~Wolf * Snow + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxCo1LD1 <- glmmTMB(Lynx~Wolf + X1750m + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxCo1LD2 <- glmmTMB(Lynx~Wolf * X1750m + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxCo2 <- glmmTMB(Lynx~Prey_Lynx + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxCo2Sn1 <- glmmTMB(Lynx~Prey_Lynx + Snow + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxCo2Sn2 <- glmmTMB(Lynx~Prey_Lynx * Snow + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxCo2LD1 <- glmmTMB(Lynx~Prey_Lynx + X1750m + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxCo2LD2 <- glmmTMB(Lynx~Prey_Lynx * X1750m + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxCo3 <- glmmTMB(Lynx~Coyote + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxCo3Sn1 <- glmmTMB(Lynx~Coyote + Snow + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxCo3Sn2 <- glmmTMB(Lynx~Coyote * Snow + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxCo3LD1 <- glmmTMB(Lynx~Coyote + X1750m + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
LynxCo3LD2 <- glmmTMB(Lynx~Coyote * X1750m + LowCon1500 + UpCon1500 + pOpen1500 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)

LynxCo <- ICtab(LynxCo0, LynxCo0LD, LynxCo1, LynxCo1LD1, LynxCo1LD2, LynxCo2, LynxCo2LD1, LynxCo2LD2, LynxCo3, LynxCo3LD1, LynxCo3LD2, LynxCo1Sn1, LynxCo2Sn1, LynxCo3Sn1, LynxCo1Sn2, LynxCo2Sn2, LynxCo3Sn2, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

LynxCo
##           dLogLik dAIC df weight
# LynxCo0LD   3.5     0.0 7  0.3044
# LynxCo3LD1  4.0     1.1 8  0.1748
# LynxCo2LD1  3.6     1.8 8  0.1228
# LynxCo1LD1  3.5     2.0 8  0.1121
# LynxCo3LD2  4.3     2.4 9  0.0918
# LynxCo2LD2  3.8     3.5 9  0.0531
# LynxCo1LD2  3.8     3.5 9  0.0523
# LynxCo0     0.0     5.1 6  0.0241
# LynxCo3     0.7     5.6 7  0.0187
# LynxCo3Sn1  1.1     6.9 8  0.0097
# LynxCo2     0.0     7.0 7  0.0091
# LynxCo1     0.0     7.1 7  0.0089
# LynxCo2Sn1  0.4     8.3 8  0.0047
# LynxCo1Sn1  0.4     8.3 8  0.0047
# LynxCo3Sn2  1.3     8.5 9  0.0043
# LynxCo1Sn2  0.7     9.6 9  0.0025
# LynxCo2Sn2  0.4    10.2 9  0.0018

summary(LynxCo0LD) ## LD sig. +ve
summary(LynxCo3LD1) ## Coyote not sig.
summary(LynxCo2LD1) ## Prey not sig. either
summary(LynxCo1LD1) ## Not Wolf

## C. Coyote
nrow(WeekOcc_sc[WeekOcc_sc$Coyote>0 & WeekOcc_sc$Wolf>0, ]) # 15 co-occurrences
nrow(WeekOcc_sc[WeekOcc_sc$Lynx>0 & WeekOcc_sc$Coyote>0, ])## 6 co-occurrences of 2122 observations
nrow(WeekOcc_sc[WeekOcc_sc$Coyote>0 & WeekOcc_sc$Prey_Coyote>0, ]) # 12

CoyoteCo0 <- glmmTMB(Coyote~ pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteCo0LD <- glmmTMB(Coyote~ X1500m + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteCo0Sn <- glmmTMB(Coyote~ Snow + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteCo1 <- glmmTMB(Coyote~Wolf + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteCo1Sn1 <- glmmTMB(Coyote~Wolf + Snow + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteCo1Sn2 <- glmmTMB(Coyote~Wolf * Snow + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteCo1LD1 <- glmmTMB(Coyote~Wolf + X1500m + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteCo1LD2 <- glmmTMB(Coyote~Wolf * X1500m + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteCo2 <- glmmTMB(Coyote~Prey_Coyote + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteCo2Sn1 <- glmmTMB(Coyote~Prey_Coyote + Snow + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteCo2Sn2 <- glmmTMB(Coyote~Prey_Coyote * Snow + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteCo2LD1 <- glmmTMB(Coyote~Prey_Coyote + X1500m + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteCo2LD2 <- glmmTMB(Coyote~Prey_Coyote * X1500m + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteCo3 <- glmmTMB(Coyote~Coyote + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteCo3Sn1 <- glmmTMB(Coyote~Lynx + Snow + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteCo3Sn2 <- glmmTMB(Coyote~Lynx * Snow + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteCo3LD1 <- glmmTMB(Coyote~Lynx + X1500m + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)
CoyoteCo3LD2 <- glmmTMB(Coyote~Lynx * X1500m + pOpen1750 + CamActive + (1|CamStation), data = WeekOcc_sc, family = binomial)

CoyoteCo <- ICtab(CoyoteCo0, CoyoteCo0LD, CoyoteCo1, CoyoteCo1LD1, CoyoteCo1LD2, CoyoteCo2, CoyoteCo2LD1, CoyoteCo2LD2, CoyoteCo3, CoyoteCo3LD1, CoyoteCo3LD2, CoyoteCo1Sn1, CoyoteCo2Sn1, CoyoteCo3Sn1, CoyoteCo1Sn2, CoyoteCo2Sn2, CoyoteCo3Sn2, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

CoyoteCo
##            dLogLik dAIC df weight
# CoyoteCo1LD1 13.1     0.0 6  0.419 
# CoyoteCo1LD2 13.5     1.2 7  0.226 
# CoyoteCo3LD2 13.4     1.5 7  0.197 
# CoyoteCo0LD  10.4     3.5 5  0.071 
# CoyoteCo3LD1 10.9     4.5 6  0.043 
# CoyoteCo2LD1 10.4     5.5 6  0.027 
# CoyoteCo2LD2 10.9     6.5 7  0.016 
# CoyoteCo1Sn2  5.1    18.1 7  <0.001
# CoyoteCo1     2.4    19.5 5  <0.001
# CoyoteCo1Sn1  2.5    21.3 6  <0.001
# CoyoteCo0     0.0    22.3 4  <0.001
# CoyoteCo3     0.0    22.3 4  <0.001
# CoyoteCo2Sn2  2.9    22.4 7  <0.001
# CoyoteCo2     0.0    24.2 5  <0.001
# CoyoteCo3Sn1  0.7    24.9 6  <0.001
# CoyoteCo2Sn1  0.2    26.0 6  <0.001
# CoyoteCo3Sn2  0.8    26.7 7  <0.001

summary(CoyoteCo1LD1) ## Wolf sig. +ve, LD sig. +ve
summary(CoyoteCo1LD2) ## Wolf not significant when interaction is tested
summary(CoyoteCo3LD2) ## Lynx *interaction* is near sig. +ve (p = 0.07) -- Lynx alone not +ve
