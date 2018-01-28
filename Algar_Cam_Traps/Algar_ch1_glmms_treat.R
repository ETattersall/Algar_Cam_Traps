#############################
# Algar_ch1_glmms_treat.R
# GLMMs modeling species detections/month as a function of SL treatment
# All Algar data between Nov. 2015 and April 2017
# Started by Erin T., Oct. 24, 2017
##############################

library(lme4)
library(MASS)
library(AICcmodavg)
library(ggplot2)

getwd()
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
dat <- read.csv("Monthlydetections_nov2015-nov2017.csv")
head(dat)
dat$X <- NULL
head(dat)

#### Wolf detections as a function of Treatment ####
# GLMM framework, test Poisson, Neg.Bin., Zero-inflation

#Null model: Wolf detections are not explained by Treatment, random effect of Site
m0.wolf <- glmer(Wolf~1 + (1|Site), family = poisson, data = dat)
m0nb.wolf <- glmer.nb(Wolf~1 + (1|Site), data = dat)

cand.set.wolf <- c(m0.wolf, m0nb.wolf)
names <- c("NULLglmm.Pois", "NULLglmm.nb")
aictab(cand.set.wolf, modnames = names, second.ord = TRUE, nobs = NULL,
       sort = TRUE)

## Should check overdispersion with a qqplot...

# Adding Treatment explanatory variable
m1.wolf <- glmer.nb(Wolf~Treatment + (1|Site), data = dat)

cand.set.wolf <- c(m0.wolf, m0nb.wolf, m1.wolf)
names <- c("NULLglmm.Pois", "NULLglmm.nb", "TREATglmm.nb")
aictab(cand.set.wolf, modnames = names, second.ord = TRUE, nobs = NULL,
       sort = TRUE)


summary(m0nb.wolf)

## Zero-inflated model (restart R to load glmmADMB)
library(glmmADMB)
mZI.null.wolf <- glmmadmb(Wolf~1 + (1| Site), data = dat, family = "nbinom", link = "log", zeroInflation = TRUE)
# Error in II[, ii] <- II[, ii] + REmat$codes[[i]] : 
#  number of items to replace is not a multiple of replacement length
# In addition: Warning messages:
#  1: In glmmadmb(Wolf ~ Treatment + (1 | Site), data = dat, family = "nbinom",  :
#                   NAs removed in constructing fixed-effect model frame: you should probably # remove them manually, e.g. with na.omit()
#                 2: In II[, ii] + REmat$codes[[i]] :
#                  longer object length is not a multiple of shorter object length
mZI.wolf <- glmmadmb(Wolf~Treatment + (1| Site), data = dat, family = "nbinom", link = "log", zeroInflation = TRUE)
