#################################
# Algar_bear_glmms.R
# Extracting bear data--> using only active months for first 2 deployments
# Started Feb 5., 2018
#################################

library(dplyr)
library(tidyr)
library(lmtest)
library(glmmTMB)
library(bbmle)


setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
dat <- read.csv("monthlydetections_nov2015-apr2017.csv") # First 2 deployments monthly detection data + snow days
head(dat)
dat$X <- NULL
head(dat)
str(dat)

#Loading % lowland data
low <- read.csv("Lowlandcover_9buffersizes.csv")
##Add 250m and 500m to monthly detections
dat$low250 <- low$Prop250[match(dat$Site, low$CamStation)]
dat$low500 <- low$Prop500[match(dat$Site, low$CamStation)]

# Check black bear detections by yr_month
plot(x = dat$Yr_Month, y = dat$Blackbear) #Active April- November
#Check against Snow
plot(x = dat$SnowDays, y = dat$Blackbear) # Predictably, more in months with fewer snow days
# Probably does not really make sense to include Snow as a covariate in models, since it will definitely be a factor
# Could consider including Yr_month as a random effect, as we know spring and fall months will have fewer detections
hist(dat$Blackbear)

### Cropping data for active bear months only -- April - October
unique(dat$Yr_Month)


bear <- dat %>% filter(Yr_Month == "2016-04" | Yr_Month == "2016-05" | Yr_Month == "2016-06"| Yr_Month == "2016-07"| Yr_Month == "2016-08"| Yr_Month == "2016-09"| Yr_Month == "2016-10"| Yr_Month == "2017-04") %>% select(Site, Treatment,Yr_Month, Site_ym, Blackbear, SnowDays, low250,low500)

plot(bear$Yr_Month, bear$Blackbear)
hist(bear$Blackbear)

### Testing Null model, zero-inflated vs not
b0.ZINB <- glmmTMB(Blackbear~1 + (1|Site), zi = ~1, data = bear, family = nbinom2)
b0.NB <- glmmTMB(Blackbear~1 + (1|Site), data = bear, family = nbinom2)

AICctab(b0.ZINB,b0.NB)
#       dAICc df
# b0.NB   0.0   3 
# b0.ZINB 2.1   4 

lrtest(b0.ZINB,b0.NB)
#     Df LogLik Df Chisq Pr(>Chisq)
# 1   4 -226.9                    
# 2   3 -226.9 -1     0     0.9998

summary(b0.NB) #Overdispersion parameter = 0.598. Not necessary to use ZI

# Model 1. Blackbear as a function of treatment
b1.NB <- glmmTMB(Blackbear~Treatment + (1|Site), data = bear, family = nbinom2)

# Model 2: Blackbear as a function of treatment and %Low
b2.NB <- glmmTMB(Blackbear~Treatment +low500 + (1|Site), data = bear, family = nbinom2)

# Model 3: Blackbear as a function of treatment, low, and interaction
b3.NB <- glmmTMB(Blackbear~Treatment*low500 + (1|Site), data = bear, family = nbinom2) #convergence issue. Try different distribution
b3.NB1 <- glmmTMB(Blackbear~Treatment*low500 + (1|Site), data = bear, family = nbinom1) #Still not fixed

b4.NB <- glmmTMB(Blackbear~low500 + (1|Site), data = bear, family = nbinom2)

## Model Selection
# Including models that did not converge
AICctab(b0.NB, b1.NB, b2.NB,b3.NB, b4.NB)
#       dAICc df
# b2.NB  0.0  7 
# b3.NB  4.6  10
# b1.NB 10.7  6 
# b4.NB 12.5  4 
# b0.NB 17.6  3 
lrtest(b2.NB,b3.NB)
#     #Df  LogLik Df  Chisq Pr(>Chisq)
#   1   7 -213.87                     
#   2  10 -212.92  3 1.9026     0.5929

summary(b3.NB) #NA values for SE. Cannot include

## Excluding non-converging models
AICctab(b0.NB, b1.NB, b2.NB, b4.NB)
#     dAICc df
# b2.NB  0.0  7 
# b1.NB 10.7  6 
# b4.NB 12.5  4 
# b0.NB 17.6  3 

lrtest(b2.NB, b1.NB)
# Model 1: Blackbear ~ Treatment + low500 + (1 | Site)
# Model 2: Blackbear ~ Treatment + (1 | Site)
#  #  Df  LogLik Df  Chisq Pr(>Chisq)    
# 1   7 -213.87                         
# 2   6 -220.27 -1 12.796  0.0003473 ***

summary(b2.NB)

# Comparing %lowland and bear detections
plot(x = bear$low500, y = bear$Blackbear) #Pretty clear that detections decrease with ince in %lowland
