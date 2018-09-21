########################################
## Ch3_ResponseVariable.R
## Exploring occasion lengths and model forms for Chapter 3 hypotheses
## Started August 1, 2018
########################################


library(camtrapR)
library(reshape2)	# for formatting data frames
# library(plyr) need for renaming Treatments for consistency, but conflicts with dplyr
library(dplyr)		# only load after using revalue function, for applying functions to subsets of data frames
library(ggplot2)	# for data visualization
library(stringr)	# for working with character strings
library(tidyr)		# for data formatting functions
library(glmmTMB)
library(bbmle)



setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")


daily <- read.csv(file.choose()) #Algar daily detections 30 months
rec <- read.csv(file.choose()) # record table

str(daily) # Number of detections each day. Also included binary 'cam active'
summary(daily) ## Total number of detections per site per day, for 886 days (not every camera active for whole time)
summary(rec)
## Rec includes offline sites?
unique(rec$Station) ## Yes. Will need to exclude

colnames(daily) ## Remove X's
daily$X <- NULL
daily$X.1 <- NULL

### Exclude OffLine sites
daily <- daily %>% filter(Treatment != "OffLine")
summary(daily)


### Total number of detections summed for each species?
colSums(daily[, 5:14]) ## Matches total number of detections for each species

### Converting detections to binary occurrences

Occ <- as.data.frame(ifelse(daily[ , 5:14] > 0, 1, 0))
str(Occ)
summary(Occ)
colSums(Occ) ## Only decreases total counts slightly (moreso for deer, wolves, hare)
Occ <- cbind.data.frame(daily[,1:4], Occ, daily[,15:16]) ## Adding non-detection rows
str(Occ)


## Histograms of data
hist(Occ$Wolf) ## Mainly zeroes, unsurprisingly
hist(Occ$WTDeer)

## Try basic binomial glm
f1 <- glmmTMB(Coyote~Wolf, data = Occ, family = 'binomial')
summary(f1)

f2 <- glmmTMB(Coyote~Lynx, data = Occ, family = 'binomial')
summary(f2)

## Try with Site random effect
f1r <- glmmTMB(Coyote~Wolf + (1| Site), data = Occ, family = 'binomial')
summary(f1r) ## Reduces deviance and neg logLik significantly, no longer strongly correlated

## Add CamActive
start.time <- Sys.time()
f1C <- glmmTMB(Coyote~Wolf + CamActive + (1| Site), data = Occ, family = 'binomial')
end.time <- Sys.time()
end.time - start.time
summary(f1C)

## ... Can I use zero-inflation with binomial?
f1z <- glmmTMB(Coyote~Wolf, data = Occ, zi = ~1, family = 'binomial')

ICtab(f1, f1r, f1C, f1z, type= "AIC", weights = TRUE, delta = TRUE, base = TRUE, logLik = TRUE, sort = TRUE) ## Random effect and CamActive greatly improve model fit, zero-inflation is equivalent

## How do zero-inflation estimates compare?
summary(f1r)
summary(f1z) ## ZI estimates are very different

#### Will 1-day occasion length work for our least-detected species?
f3r <- glmmTMB(Lynx~Hare + Squirrel + (1|Site), data = Occ, family = 'binomial')
summary(f3r) 
## Yes, but no strong effect detected

### Including Cam Active in model
f4 <- glmmTMB(Lynx~Hare + Squirrel + CamActive + (1|Site), data = Occ, family = 'binomial')

f4z<- glmmTMB(Lynx~Hare + Squirrel + CamActive + (1|Site), data = Occ, zi = ~1, family = 'binomial')
summary(f4z)
### Model convergence issues for both ZI and non ZI Lynx-prey models when including CamActive

## Coyote (more non-zeroes)
start.time <- Sys.time()
f5z <- glmmTMB(Coyote~Wolf + Hare + WTDeer + CamActive, data = Occ, zi = ~1, family = 'binomial')
end.time <- Sys.time()
end.time - start.time


#### Occasion length of 1 day works for models with few parameters, but not with >5

### Could remove days when camera was inactive: CamActive is either a 1 or 0 when occasion length is a day, and 0 - Cam Active hugely increases # zeroes --> confirm
plot(Occ$CamActive, Occ$Lynx) ## Cam Active and Lynx are both 0's or 1's

ActCams <- Occ %>% filter(CamActive == 1) ## Cuts observations almost in half
str(ActCams)

## Try lynx model again without CamActive (ZI -- go big!!)
start.time <- Sys.time()
f4az<- glmmTMB(Lynx~Hare + Squirrel + (1|Site), data = ActCams, zi = ~1, family = 'binomial') 
## Errors prevented it from running, likely because covariates aren't standardized
end.time <- Sys.time()
end.time - start.time ### 2.6 minutes, no warnings or errors
summary(f4az)

## Conclusions: Need to run full model at 1-day occasion length to see if those models will run. Remove CamActive = 0 cuts out loads of zeroes so models actually run (with 4 parameters anyway). Should also help to standardize and center covariates


##### Sep 21, 2018: Testing 1-day occasion length in modelling habitat variables ####
### 1- occasion length - ActCams

## Habitat variables
Hab <- read.csv("Algar_HabitatData_8scales.csv")

### There are 54 columns of interest in Hab -- too many to 'match' over to ActCams at one time. Match each scale separately?

## Register each scale as separate data frame
## Occasion length 1 day, Scale 250 m == Day1.250
Day1.250 <- ActCams


Day1.250$LowCon250 <- Hab$LowCon250[match(Day1.250$Site, Hab$CamStation)]
Day1.250$Tamarack250 <- Hab$Tamarack250[match(Day1.250$Site, Hab$CamStation)]
Day1.250$UpCon250 <- Hab$UpCon250[match(Day1.250$Site, Hab$CamStation)]
Day1.250$UpDecid250 <- Hab$UpDecid250[match(Day1.250$Site, Hab$CamStation)]
Day1.250$pOpen250 <- Hab$pOpen250[match(Day1.250$Site, Hab$CamStation)]


## 500 m 
Day1.500 <- ActCams


Day1.500$LowCon500 <- Hab$LowCon500[match(Day1.500$Site, Hab$CamStation)]
Day1.500$Tamarack500 <- Hab$Tamarack500[match(Day1.500$Site, Hab$CamStation)]
Day1.500$UpCon500 <- Hab$UpCon500[match(Day1.500$Site, Hab$CamStation)]
Day1.500$UpDecid500 <- Hab$UpDecid500[match(Day1.500$Site, Hab$CamStation)]
Day1.500$pOpen500 <- Hab$pOpen500[match(Day1.500$Site, Hab$CamStation)]
Day1.500$LowDecid500 <- Hab$LowDecid500[match(Day1.500$Site, Hab$CamStation)]

## Before registering all scales, check first to see if full model will run
## Scaling input variables
covscale <- function(covariate){
  centre <- (covariate - mean(covariate, na.rm = TRUE)) # where covariate is a vector of numeric input values for one covariate
  standardize <- centre/(2*sd(covariate, na.rm = TRUE)) # Divide centred value by 2 SD
}

Day1.250$LowCon250_sc <- covscale(Day1.250$LowCon250)
Day1.250$Tamarack250_sc <- covscale(Day1.250$Tamarack250)
Day1.250$UpCon250_sc <- covscale(Day1.250$UpCon250)
Day1.250$UpDecid250_sc <- covscale(Day1.250$UpDecid250)
Day1.250$pOpen250_sc <- covscale(Day1.250$pOpen250)

## Full model = all standardized habitat variables, random effect of Site and treatment, zero inflation. Will test Lynx first because they have fewest detections
L1.250 <- glmmTMB(Lynx~LowCon250_sc + Tamarack250_sc + UpCon250_sc + UpDecid250_sc + pOpen250_sc + (1|Site), data = Day1.250, zi = ~1, family = 'binomial')
summary(L1.250) ### MODEL ACTUALLY RAN SUCCESSFULLY!!!!

## Try 500 --> including LowDecid
## Standardize input variables

Day1.500$LowCon500_sc <- covscale(Day1.500$LowCon500)
Day1.500$Tamarack500_sc <- covscale(Day1.500$Tamarack500)
Day1.500$UpCon500_sc <- covscale(Day1.500$UpCon500)
Day1.500$UpDecid500_sc <- covscale(Day1.500$UpDecid500)
Day1.500$LowDecid500_sc <- covscale(Day1.500$LowDecid500)
Day1.500$pOpen500_sc <- covscale(Day1.500$pOpen500)

print(paste("start time", Sys.time()))
L1.500.1 <- glmmTMB(Lynx~LowCon500_sc + Tamarack500_sc + UpCon500_sc + UpDecid500_sc + LowDecid500_sc + pOpen500_sc + (1|Site), data = Day1.500, zi = ~1, family = 'binomial')
print(paste("end time", Sys.time()))

summary(L1.500.1)

###Warning message:
## In doTryCatch(return(expr), name, parentenv, handler) :
##  restarting interrupted promise evaluation

### Try 500 m (includes LowDecid) plus Treatment as random effect
print(paste("start time", Sys.time()))
L1.500.2 <- glmmTMB(Lynx~LowCon500_sc + Tamarack500_sc + UpCon500_sc + UpDecid500_sc + LowDecid500_sc + pOpen500_sc + (1|Site) + (1|Treatment), data = Day1.500, zi = ~1, family = 'binomial')
print(paste("end time", Sys.time()))

summary(L1.500.2)

### Compare above to model without any random effects
print(paste("start time", Sys.time()))
L1.500.0 <- glmmTMB(Lynx~LowCon500_sc + Tamarack500_sc + UpCon500_sc + UpDecid500_sc + LowDecid500_sc + pOpen500_sc, data = Day1.500, zi = ~1, family = 'binomial')
print(paste("end time", Sys.time()))

summary(L1.500.0)

ICtab(L1.500.0, L1.500.1, L1.500.2, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
### Site random effect has lowest AIC


#### Coyotes --> does 1 day occasion length work?
## No random variables
print(paste("start time", Sys.time()))
C1.500.0 <- glmmTMB(Coyote~LowCon500_sc + Tamarack500_sc + UpCon500_sc + UpDecid500_sc + LowDecid500_sc + pOpen500_sc, data = Day1.500, zi = ~1, family = 'binomial')
print(paste("end time", Sys.time()))



## 1 random variable
print(paste("start time", Sys.time()))
C1.500.1 <- glmmTMB(Coyote~LowCon500_sc + Tamarack500_sc + UpCon500_sc + UpDecid500_sc + LowDecid500_sc + pOpen500_sc + (1|Site), data = Day1.500, zi = ~1, family = 'binomial')
print(paste("end time", Sys.time()))



## 2 random variables
print(paste("start time", Sys.time()))
C1.500.2 <- glmmTMB(Coyote~LowCon500_sc + Tamarack500_sc + UpCon500_sc + UpDecid500_sc + LowDecid500_sc + pOpen500_sc + (1|Site) + (1|Treatment), data = Day1.500, zi = ~1, family = 'binomial')
print(paste("end time", Sys.time()))


ICtab(C1.500.0, C1.500.1, C1.500.2, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

summary(C1.500.1)


#### Black bears --> does 1 day occasion length work?

### Need to remove Winter days to account for hibernation! First separate month from Datep
Day1.500$Month <- as.factor(format(as.Date(Day1.500$Datep), "%b")) ## Month abbreviation used because idk if 'filter works on numbers, even if they're factors
class(Day1.500$Month)
unique(Day1.500$Month)

## Extract summer months -- Apr - Oct
Day1.500 <- Day1.500 %>% filter(Month == "Apr" | Month == "May" | Month == "Jun" | Month == "Jul" | Month == "Aug" | Month == "Sep" | Month == "Oct")

## No random variables
print(paste("start time", Sys.time()))
B1.500.0 <- glmmTMB(Blackbear~LowCon500_sc + Tamarack500_sc + UpCon500_sc + UpDecid500_sc + LowDecid500_sc + pOpen500_sc, data = Day1.500, zi = ~1, family = 'binomial')
print(paste("end time", Sys.time()))



## 1 random variable
print(paste("start time", Sys.time()))
B1.500.1 <- glmmTMB(Blackbear~LowCon500_sc + Tamarack500_sc + UpCon500_sc + UpDecid500_sc + LowDecid500_sc + pOpen500_sc + (1|Site), data = Day1.500, zi = ~1, family = 'binomial')
print(paste("end time", Sys.time()))



## 2 random variables
print(paste("start time", Sys.time()))
B1.500.2 <- glmmTMB(Blackbear~LowCon500_sc + Tamarack500_sc + UpCon500_sc + UpDecid500_sc + LowDecid500_sc + pOpen500_sc + (1|Site) + (1|Treatment), data = Day1.500, zi = ~1, family = 'binomial')
print(paste("end time", Sys.time()))
summary(B1.500.2)



ICtab(B1.500.0, B1.500.1, B1.500.2, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
summary(B1.500.1)

#### All species' models ran successfully with 1 day occasion length!! Single random effect of Site performed best with model selection
###### Test if zero inflation is appropriate. Use ActCams

## Proportion of zeros
table(is.na(ActCams$Lynx)) # no NAs
table(is.na(ActCams$Coyote)) # no NAs

### Bears - extract summer months only
ActCams$Month <- as.factor(format(as.Date(ActCams$Datep), "%b")) ## Month abbreviation used because idk if 'filter works on numbers, even if they're factors
class(ActCams$Month)
unique(ActCams$Month)

## Extract summer months -- Apr - Oct
Bears <- ActCams %>% filter(Month == "Apr" | Month == "May" | Month == "Jun" | Month == "Jul" | Month == "Aug" | Month == "Sep" | Month == "Oct")

##
table(is.na(Bears$Blackbear)) ## no NAs

table(ActCams$Lynx == 0) ## 32324 0's --> super frickin zero-inflated
table(ActCams$Coyote == 0) ## 32264 0's --> same
table(Bears$Blackbear == 0) ## 13669 0's

sum(ActCams$Lynx==0, na.rm = TRUE)/nrow(ActCams) ## 99.8% zeros -- zero-inflated
sum(ActCams$Coyote==0, na.rm = TRUE)/nrow(ActCams) ## 99.6% zeros -- zero-inflated
sum(Bears$Blackbear==0, na.rm = TRUE)/nrow(Bears) ## 97.8% zeros -- zero-inflated

### Save occurrence data --> ActCams and Bears
write.csv(ActCams, "Algar60_speciesOccurrence.csv")
write.csv(Bears, "Algar60_speciesOccurrence_SUMMER.csv")

