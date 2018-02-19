#################################
# Algar_bear_glmms.R
# Extracting bear data--> using only active months for first 2 deployments
# Started Feb 5., 2018
#################################

require(dplyr)
require(tidyr)
require(lmtest)
require(glmmTMB)
require(bbmle)


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

# Including a random effect of Yr_Month
b5.NB <- glmmTMB(Blackbear~Treatment +low500 + (1|Site) + (1|Yr_Month), data = bear, family = nbinom2)



AICctab(b0.NB, b1.NB, b2.NB, b5.NB)
#     dAICc df
# b5.NB  0.0  8 
# b2.NB 19.1  7 
# b1.NB 29.8  6 
# b0.NB 36.8  3

lrtest(b5.NB, b2.NB)
# Model 1: Blackbear ~ Treatment + low500 + (1 | Site) + (1 | Yr_Month)
# Model 2: Blackbear ~ Treatment + low500 + (1 | Site)
#    #Df  LogLik Df  Chisq Pr(>Chisq)    
# 1   8 -203.23                         
# 2   7 -213.87 -1 21.284   3.96e-06 ***

summary(b2.NB)
summary(b5.NB) #Yr_Month accounts for more variance than site

#Include (1|Yr_Month) for all models
# Model 0: Null
b0.NB <- glmmTMB(Blackbear~1 + (1|Site)+ (1|Yr_Month), data = bear, family = nbinom2)
# Model 1. Blackbear as a function of treatment
b1.NB <- glmmTMB(Blackbear~Treatment + (1|Site) + (1|Yr_Month), data = bear, family = nbinom2)

# Model 2: Blackbear as a function of treatment and %Low
b2.NB <- glmmTMB(Blackbear~Treatment +low500 + (1|Site)+ (1|Yr_Month), data = bear, family = nbinom2)

# Model 3: Blackbear as a function of treatment, low, and interaction
b3.NB <- glmmTMB(Blackbear~Treatment*low500 + (1|Site)+ (1|Yr_Month), data = bear, family = nbinom2) #convergence issue still. Exclude from analysis

b4.NB <- glmmTMB(Blackbear~low500 + (1|Site)+ (1|Yr_Month), data = bear, family = nbinom2)

modnames <- c("Null", "Treat", "Treat + Low500", "Low500")
beartab <- ICtab(b0.NB, b1.NB, b2.NB,b4.NB, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
beartab
#             dLogLik dAIC df weight
# Treat + Low500 6.9     0.0  8  0.644 
# Low500         3.2     1.5  5  0.308 
# Null           0.0     5.8  4  0.035 
# Treat          2.0     7.8  7  0.013 

lrtest(b2.NB, b4.NB)
# Model 1: Blackbear ~ Treatment + low500 + (1 | Site) + (1 | Yr_Month)
# Model 2: Blackbear ~ low500 + (1 | Site) + (1 | Yr_Month)
#   #Df  LogLik Df  Chisq Pr(>Chisq)  
# 1   8 -203.23                       
# 2   5 -206.97 -3 7.4729    0.05826 .

summary(b2.NB)
summary(b4.NB)

# Comparing %lowland and bear detections
plot(x = bear$low500, y = bear$Blackbear) #Pretty clear that detections decrease with ince in %lowland


bbear <- ggplot(data = bear, aes(x = Treatment, y = Blackbear, fill = Treatment)) + geom_boxplot()
bbear + theme_classic() + xlab("Sampling Strata") + ylab("Black bear detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

## Without HumanUse (no detections)
bbear + theme_classic() + xlab("Sampling Strata") + ylab("Black bear detections/month") + scale_x_discrete(limits=c("Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

bear.low <- ggplot(data = bear, aes(x = low500, y = Blackbear)) + geom_point() + theme_classic() + xlab("Proportion lowland habitat (500m buffer)") + ylab("Black bear detections/month")
bear.low

plot(Blackbear~low500,
     data = bear,
     col = alpha("azure4", 1),
     pch = 16, cex = 1.0,
     xlab = "Snow Days/month",
     ylab = "Wolf detections/month")


#### Box plot for restricted bear season, updated
updat <- read.csv("MonthlyDetections_nov2015-nov2017.csv")

bear2 <- updat %>% filter(Yr_Month == "2016-04" | Yr_Month == "2016-05" | Yr_Month == "2016-06"| Yr_Month == "2016-07"| Yr_Month == "2016-08"| Yr_Month == "2016-09"| Yr_Month == "2016-10"| Yr_Month == "2017-04" | Yr_Month == "2017-05" | Yr_Month == "2017-06"| Yr_Month == "2017-07"| Yr_Month == "2017-08"| Yr_Month == "2017-09"| Yr_Month == "2017-10") %>% select(Site, Treatment,Yr_Month, Site_ym, Blackbear)
unique(bear2$Yr_Month)

bbear <- ggplot(data = bear2, aes(x = Treatment, y = Blackbear, fill = Treatment)) + geom_boxplot()
bbear + theme_classic() + xlab("Sampling Strata") + ylab("Black bear detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))
