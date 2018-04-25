##########################################
# Ch1_models.R
# ZINB GLMMs testing species line use as a function of line characteristics and env. variables
# Started Apr. 19, 2018
##########################################

library(glmmTMB)
library(bbmle) #AICtab function
library(ggplot2)
library(lmtest)
library(reshape)
library(dplyr)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

#Load detection data
det <- read.csv("MonthlyDetections_nov2015-nov2017.csv")
det$X.1 <- NULL
det$X <- NULL

#Load lowland data
low <- read.csv("newAVIE_lowland8buffersizes.csv")

#Combine data
det$low250 <- low$low250[match(det$Site, low$CamStation)]
det$low500 <- low$low500[match(det$Site, low$CamStation)]
det$low750 <- low$low750[match(det$Site, low$CamStation)]
det$low1000 <- low$low1000[match(det$Site, low$CamStation)]
det$low1250 <- low$low1250[match(det$Site, low$CamStation)]
det$low1500 <- low$low1500[match(det$Site, low$CamStation)]
det$low1750 <- low$low1750[match(det$Site, low$CamStation)]
det$low2000 <- low$low2000[match(det$Site, low$CamStation)]

#Load linedensity data
LineDens <- read.csv("AlgarStationsLD_Lines.csv")

# Combine datasets
det$LD250 <- LineDens$X250m[match(det$Site, LineDens$CamStation)]
det$LD500 <- LineDens$X500m[match(det$Site, LineDens$CamStation)]
det$LD750 <- LineDens$X750m[match(det$Site, LineDens$CamStation)]
det$LD1000 <- LineDens$X1000m[match(det$Site, LineDens$CamStation)]
det$LD1250 <- LineDens$X1250m[match(det$Site, LineDens$CamStation)]
det$LD1500 <- LineDens$X1500m[match(det$Site, LineDens$CamStation)]
det$LD1750 <- LineDens$X1750m[match(det$Site, LineDens$CamStation)]
det$LD2000 <- LineDens$X2000m[match(det$Site, LineDens$CamStation)]


#Load line width and veg height
sp <- read.csv("Spatial_covariates.csv")

det$VegHt <- sp$VegHt[match(det$Site, sp$Site)]
det$LineWidth <- sp$LineWidth[match(det$Site, sp$Site)]

hist(det$Caribou)
hist(det$SnowDays)
hist(det$low1750)
hist(det$LD1750)
hist(det$VegHt)
hist(det$LineWidth)


## Visualising patterns using predict function (predicting new data, given the best-fit model)
## Need to first remove NAs from explanatory variables
newdat <-  det[ , c(1,2,12:34)] #Naming columns in df
newdat <- na.omit(newdat) ## Exluding NA rows, which causes lengths to differ

#### Caribou models ####
## Lowland measured at 1750m, LD measured at 1750m
# Not including Dist2Water --> no corresponding hypothesis

#### Finding random structure (using 'beyond optimal' - global- model)
r0 <- glmmTMB(Caribou~ LineWidth + LD1750 + VegHt + low1750*Treatment + SnowDays, zi = ~1, data = det, family = nbinom2)
rSite <- glmmTMB(Caribou~ LineWidth + LD1750 + VegHt + low1750*Treatment + SnowDays + (1|Site), zi = ~1, data = det, family = nbinom2) 
rMonth <- glmmTMB(Caribou~ LineWidth + LD1750 + VegHt + low1750*Treatment + SnowDays + (1|Month), zi = ~1, data = det, family = nbinom2)
r2 <- glmmTMB(Caribou~ LineWidth + LD1750 + VegHt + low1750*Treatment + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)
r3 <- glmmTMB(Caribou~ LineWidth + LD1750 + VegHt + low1750*Treatment + SnowDays + (Treatment|Site) + (1|Month), zi = ~1, data = det, family = nbinom2) #Will not converge
summary(r3)#NAs

ICtab(r0, rSite, rMonth, r2, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
#       dLogLik dAIC df weight
# r2      8.0     0.0 16 0.6817
# rSite   6.2     1.5 15 0.3141
# rMonth  1.3    11.3 15 0.0025
# r0      0.0    11.9 14 0.0018

### Continue with 2 random effects

#### I. Environmental models ####
#1. Null model
C.1 <- glmmTMB(Caribou~1 + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

#2. Global model
C.2 <- glmmTMB(Caribou~low1750 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

#3. Lowland only
C.3 <- glmmTMB(Caribou~low1750 + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

#4. Snow only
C.4 <- glmmTMB(Caribou~SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

modnames <- c("NULL", "Low + Snow", "Low", "Snow")
cabtab <- ICtab(C.1, C.2, C.3, C.4, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
cabtab

#         dLogLik dAIC df weight
# Low + Snow 21.1     0.0 7  0.924 
# Low        17.6     5.0 6  0.076 
# Snow        4.0    32.2 6  <0.001
# NULL        0.0    38.2 5  <0.001

## Low + Snow in top model, continue hypothesis testing with both

#### Line characteristic models ####
#0. Null model (using top model from environmental models)
CH0 <- glmmTMB(Caribou~low1750 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

#1. Treatment
CH1 <- glmmTMB(Caribou~Treatment + low1750 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

#2. VegHt
CH2 <- glmmTMB(Caribou~ VegHt + low1750 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

#3. Line density
CH3 <- glmmTMB(Caribou~ LD1750 + low1750 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

#4. Line Width
CH4 <- glmmTMB(Caribou~ LineWidth + low1750 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

#5. Treatment + LineWidth
CH5 <- glmmTMB(Caribou~ Treatment + LineWidth + low1750 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

#6. LineWidth + VegHt
CH6 <- glmmTMB(Caribou~ LineWidth + VegHt + low1750 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

#7. Treatment + Line density
CH7 <- glmmTMB(Caribou~ Treatment + LD1750 + low1750 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

#8. VegHt + Treatment
CH8 <- glmmTMB(Caribou~ Treatment + VegHt + low1750 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

#9. Treatment, LineWidth, VegHt
CH9 <- glmmTMB(Caribou~ Treatment + LineWidth + VegHt + low1750 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

#10. Treatment, LineWidth, Line density
CH10 <- glmmTMB(Caribou~ Treatment + LineWidth + LD1750 + low1750 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

#11. Treatment, Line density, VegHt
CH11 <- glmmTMB(Caribou~ Treatment + VegHt + LD1750 + low1750 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

#12. Global model - Treatment, Line density, VegHt, LineWidth
CH12 <-  glmmTMB(Caribou~ Treatment + LineWidth + LD1750 + VegHt + low1750 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

## Interaction terms
# Treatment*low1750
CH13 <-  glmmTMB(Caribou~ LineWidth + LD1750 + VegHt + low1750*Treatment + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)


modnames <- c("NULL", "CH1", "CH2", "CH3", "CH4", "CH5", "CH6", "CH7", "CH8", "CH9", "CH10", "CH11", "CH12", "CH13")
cabtab <- ICtab(CH0, CH1, CH2, CH3, CH4, CH5, CH6, CH7, CH8, CH9, CH10, CH11, CH12, CH13, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
cabtab
#     dLogLik dAIC df weight
# CH2  13.8     0.0 8  0.156 
# CH10 17.8     0.1 12 0.152 
# CH11 17.6     0.3 12 0.131 
# CH12 18.6     0.4 13 0.126 
# CH5  16.4     0.9 11 0.100 
# CH4  13.3     1.0 8  0.094 
# CH8  16.2     1.2 11 0.084 
# CH9  17.1     1.4 12 0.076 
# CH6  14.0     1.6 9  0.070 
# CH13 19.1     5.4 16 0.010 
# CH7   5.5    22.7 11 <0.001
# CH3   2.3    23.1 8  <0.001
# CH1   3.5    24.7 10 <0.001
# NULL  0.0    25.6 7  <0.001

summary(CH12)
summary(CH3) 

# Comparing all models together (environmental and linear characteristic)
modnames <- c("NULL", "C.2", "C.3", "C.4", "CH0", "CH1", "CH2", "CH3", "CH4", "CH5", "CH6", "CH7", "CH8", "CH9", "CH10", "CH11", "CH12", "CH13")
cabtab <- ICtab(C.1, C.2, C.3, C.4, CH0, CH1, CH2, CH3, CH4, CH5, CH6, CH7, CH8, CH9, CH10, CH11, CH12, CH13, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
cabtab

lrtest(CH2,CH10)

# Model 1: Caribou ~ VegHt + low1750 + SnowDays + (1 | Site) + (1 | Month)
# Model 2: Caribou ~ Treatment + LineWidth + LD1750 + low1750 + SnowDays + 
#  (1 | Site) + (1 | Month)
 #   Df  LogLik Df  Chisq Pr(>Chisq)  
# 1   8 -265.60                       
# 2  12 -261.63  4 7.9497    0.09344 .

lrtest(CH10,CH11) #Line width significantly better than veg ht, but only a tiny tiny (0.14) difference in neg.LogLik

summary(CH10)
summary(CH2)
summary(CH12)


## Residual plot for global model(CH12)
op <- par(mfrow = c(1,2))
pred <- predict(CH12, zitype = "response")
resid <- residuals(CH12)

## Plot residuals against predicted --> diagonal lines characteristic of discrete data
plot(pred, resid, main = "Residuals vs. Predicted", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, lty = 2)  
lines(smooth.spline(pred, resid))
plot(fitted(CH12), residuals(CH12), main= "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)

#### Residual plot for most parsimonious model (CH2)
op <- par(mfrow = c(1,2))
pred <- predict(CH2, zitype = "response")
resid <- residuals(CH2)

## Plot residuals against predicted --> diagonal lines characteristic of discrete data
plot(pred, resid, main = "Residuals vs. Predicted", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, lty = 2)  
lines(smooth.spline(pred, resid))
plot(fitted(CH2), residuals(CH2), main= "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)

## Global model is a better fit, which is expected given that it hase more parameters


## Standardizing estimates? Evidence ratios? Model averaging?
