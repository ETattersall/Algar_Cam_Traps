##########################################
# ZINBglmms_glmmtmb.R
# Testing out ZINBglmms with glmmTMB
# Started feb. 3, 2018 by Erin T.
###########################################

library("glmmTMB")
library("ggplot2")
theme_set(theme_bw())
library("ggstance")
library(lme4)

setwd("F:/Modelling")
dat <- read.csv("monthlydetections_nov2015-apr2017.csv") # First 2 deployments monthly detection data + snow days
head(dat)
dat$X <- NULL
head(dat)
#Loading % lowland data
low <- read.csv("Lowlandcover_9buffersizes.csv")

##Add 250m and 500m to monthly detections
dat$low250 <- low$Prop250[match(dat$Site, low$CamStation)]
dat$low500 <- low$Prop500[match(dat$Site, low$CamStation)]

### Fitting ZINBglmms for wolves with glmmTMB
wzinb1 <- glmmTMB(Wolf~Treatment + (1| Site), zi = ~1, data = dat, family = nbinom2)
summary(wzinb1)

## Comparing nb.glmm fit with glmmTMB to  one fit with glmer
#### Wolf Model 5: Wolf detections vary as a function of treatment and snow
m5.wolf <- glmer.nb(Wolf~Treatment + SnowDays + (1|Site), data = dat)
summary(m5.wolf)

##glmmTMB equivalent
wnblmm <- glmmTMB(Wolf~Treatment + SnowDays + (1|Site), data = dat, family = nbinom2)
summary(wnbglmm)

### Slightly different parameter estimates but very similar

##Model selection with glmmTMB?
wzinb0 <- glmmTMB(Wolf~1 + (1|Site), zi = ~1, data = dat, family = nbinom2)

wzinb2 <- glmmTMB(Wolf~Treatment + low500 + (1| Site), zi = ~1, data = dat, family = nbinom2)

wzinb3 <- glmmTMB(Wolf~Treatment + SnowDays + (1|Site), zi = ~1, data = dat, family = nbinom2)


## Model Selection with AICC tables (use package bblme)
library(bbmle) 
cand.set.wolf <- c(wzinb0, wzinb1, wzinb2, wzinb3, m5.wolf)
names <- c("NULL", "TREAT", "TREAT+LOW", "TREAT+SNOW", "TREAT+SNOWglmer")
AICtab(cand.set.wolf, modnames = names, second.ord = TRUE, nobs = NULL,
       sort = TRUE) ### Error in UseMethod("logLik") : 
                    ## no applicable method for 'logLik' applied to an object of class "list"
AICctab(wzinb0, wzinb1, wzinb2, wzinb3, m5.wolf, wnblmm)
AICtab(wzinb0, wzinb1, wzinb2, wzinb3, m5.wolf,wnblmm) #wzinb3 comes out on top

# dAIC df
# wzinb3   0.0 8 
# m5.wolf  0.8 7 
# wnblmm   1.1 7 
# wzinb0  10.8 4 
# wzinb2  11.6 8 
# wzinb1  12.3 7 

### Compare ZINBglmm to NBglmm, both run in glmmTMB (lme4 model ranked higher than glmmTMB)
summary(wzinb3)
summary(wnblmm)
# Zero-inflated has lower AIC, but barely. Compare simpler models

ZI.w0 <- glmmTMB(Wolf~Treatment +(1|Site), zi = ~1, data = dat, family = nbinom2)
w0 <- glmmTMB(Wolf~Treatment +(1|Site), data = dat, family = nbinom2)
w0glmer <- glmer.nb(Wolf~Treatment + (1|Site), data = dat)

AICtab(ZI.w0, w0, w0glmer) 
# dAIC df
# w0glmer 0.0  6 
# w0      0.5  6 
# ZI.w0   1.9  7 

### Makes sense  that data should have a zero-inflated distribution, but AIC scores suggest that non-ZI are marginally better
### LRT
library(lmtest)
lrtest(ZI.w0, w0glmer)

#     Df  LogLik Df  Chisq Pr(>Chisq)
# 1   7 -366.24                     
# 2   6 -366.29 -1 0.1063     0.7444

lrtest(ZI.w0, w0)
#Df  LogLik Df  Chisq Pr(>Chisq)
# 1   7 -366.24                     
# 2   6 -366.54 -1 0.6083     0.4354

### Essentially the same. Will continue modelling using glmmTMB with zero-inflation!!!
