###############################
# InterceptFree_modelling.R
# Comparing outputs of intercept and intercept free models, changing the intercept
# Started Feb 22, 2018 by Erin
##############################

#### Loading packages and data ####
### Modelling with glmmTMB
library(glmmTMB)
library(bbmle) #AICtab function
library(ggplot2)
library(reshape)
library(dplyr)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
dat <- read.csv("monthlydetections_nov2015-apr2017.csv") # First 2 deployments monthly detection data + snow days
head(dat)
dat$X <- NULL
head(dat)

#Loading % lowland data
low <- read.csv("Lowlandcover_9buffersizes.csv")
##Add 250m and 500m to monthly detections
dat$low250 <- low$Prop250[match(dat$Site, low$CamStation)]
dat$low500 <- low$Prop500[match(dat$Site, low$CamStation)]

## Adding column for month only
str(dat) #Yr_Month is a factor
dat$Month <- as.Date(dat$Yr_Month, format = "%Y-%m") #NAs
dat$Month <- as.factor(format(as.Date(dat$Yr_Month, format = "%Y-%m-%d"), "%b")) #NAs
dat$Month <- NULL

## Separating elements from Yr_Month
dat$Yr_Month2 <- dat$Yr_Month #replicating Yr_Month for separation
dat <- cbind(dat[,1:15],
             colsplit(dat$Yr_Month2, "[-]", names=c("Year", "Month")))

#### Test model: top treatment model for caribou ####
# Original model ( Caribou = # detections per month, Treatment is a factor with 4 levels, low500 and Snow are continuous. Site and Month are both factors)
cabzinb4 <- glmmTMB(Caribou~Treatment + low500 + SnowDays + (1| Site)+ (1|Month), zi = ~1, data = dat, family = nbinom2)

# Intercept free
cabIF <- glmmTMB(Caribou~Treatment + low500 + SnowDays + (1| Site)+ (1|Month) - 1, zi = ~1, data = dat, family = nbinom2)

## Compare summaries
summary(cabzinb4) # Model with intercept compares all categorical treatments to reference (control), all continuous to 0
summary(cabIF)  # Model without intercept gives true effect estimate of all categorical treatments. Control = Intercept = same estimate, but all other categorical treatments have true estimates rather than differences from control

## When plotting estimates for categorical variables, the reference category (the constant, the intercept) is, for all intents and purposes, valued at estimate = 0, allowing other categories to compare

## Changing the intercept
str(dat$Treatment)
# Factor with 4 levels: "Control", "HumanUse", "NatRegen", "SPP" (1,2,3,4)
dat$Treatment <- factor(dat$Treatment, levels = c("HumanUse", "Control", "SPP", "NatRegen"))
str(dat$Treatment)
# Factor with 4 levels: "HumanUse", "Control", "SPP", "NatRegen"
# Now, re-running model with intercept will compare all categories to HumanUse