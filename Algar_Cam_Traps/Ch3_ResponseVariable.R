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

### Total number of detections summed for each species?
colSums(daily[, 6:15]) ## Matches total number of detections

### Converting detections to binary occurrences

Occ <- as.data.frame(ifelse(daily[ , 6:15] > 0, 1, 0))
str(Occ)
summary(Occ)
colSums(Occ) ## Only decreases total counts slightly (moreso for deer, wolves, hare)
Occ <- cbind.data.frame(daily[,1:5], Occ, daily[,16:17]) ## Adding non-detection rows
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
plot(Occ$CamActive, Occ$Lynx) ## Cam Active and Lynx are bot 0's or 1's

ActCams <- Occ %>% filter(CamActive == 1) ## Cuts observations almost in half

## Try lynx model again without CamActive (ZI -- go big!!)
start.time <- Sys.time()
f4az<- glmmTMB(Lynx~Hare + Squirrel + (1|Site), data = ActCams, zi = ~1, family = 'binomial') 
## Errors prevented it from running, likely because covariates aren't standardized
end.time <- Sys.time()
end.time - start.time ### 2.6 minutes, no warnings or errors
summary(f4az)

## Conclusions: Need to run full model at 1-day occasion length to see if those models will run. Remove CamActive = 0 cuts out loads of zeroes so models actually run (with 4 parameters anyway). Should also help to standardize and center covariates

