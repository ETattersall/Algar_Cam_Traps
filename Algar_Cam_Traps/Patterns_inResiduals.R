###################################
# Patterns_inResiduals.R
# Exploring nestedness and patterns in residuals
# Started Apr. 24, 2018
###################################

library(glmmTMB)
library(bbmle) #AICtab function
library(ggplot2)
library(lmtest)
library(reshape)
library(dplyr)

#### Setting up environment ####
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


plot(det$Treatment, det$Caribou)

## Visualising patterns using predict function (predicting new data, given the best-fit model)
## Need to first remove NAs from explanatory variables
newdat <-  det[ , c(1,2,12:34)] #Naming columns in df
newdat <- na.omit(newdat) ## Exluding NA rows, which causes lengths to differ



## Simple caribou models
#1. Null model
C.1 <- glmmTMB(Caribou~1 + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

#2. Env. covariates
C.2 <- glmmTMB(Caribou~low1750 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

#3. Env. covariates + treatment
C.3 <- glmmTMB(Caribou~Treatment + low1750 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)

# With random effects as fixed effects
C.4 <- glmmTMB(Caribou ~Month, zi=~1, data = det, family = nbinom2)
summary(C.4)

C.5 <- glmmTMB(Caribou ~Site, zi=~1, data = det, family = nbinom2) #Too many sites to model as a fixed effect

## Model selection with AIC and model weight
modnames <- c("NULL", "ENV", "TREAT")
ICtab(C.1, C.2, C.3, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

summary(C.2)
summary(C.3)

## Residual analysis of C.2
op <- par(mfrow = c(1,2))
pred <- predict(C.2, zitype = "response")
resid <- residuals(C.2)
# Residuals vs. Predicted plot
plot(pred, resid, main = "Residuals vs. Predicted", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, lty = 2)  
lines(smooth.spline(pred, resid)) ## Still patterns in residuals

#Residuals vs. Fitted plot
plot(newdat$low1750, residuals(C.2), main= "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals") #x and y lengths differ
length(newdat$low1750) #833
length(residuals(C.2)) #845

## Residual analysis of C.1
op <- par(mfrow = c(1,2))
pred <- predict(C.1, zitype = "response")
resid <- residuals(C.1)
# Residuals vs. Predicted plot
plot(pred, resid, main = "Residuals vs. Predicted", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, lty = 2)  
lines(smooth.spline(pred, resid))





##### All plots have diagonal lines --> according to Stack Exchange, this is a product of the response being count data --> discrete data intervals

#### Accounting for Nestedness (???) ####
# Including a random effect of Site on the Treatment (Site affects Treatment)
C.6 <- glmmTMB(Caribou~Treatment + (1|Site) + (1|Month) + (Treatment|Site), zi = ~1, data = det, family = nbinom2)
summary(C.6)

## Compare to model without (Treatment|Site)
C.7 <- glmmTMB(Caribou~Treatment + (1|Site) + (1|Month), zi = ~1, data = det, family = nbinom2)
summary(C.7) ## Lower AIC without random effect on slope --> do not include

## Residual analysis of C.6
op <- par(mfrow = c(1,2))
pred <- predict(C.6, zitype = "response")
resid <- residuals(C.6)
# Residuals vs. Predicted plot
plot(pred, resid, main = "Residuals vs. Predicted", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, lty = 2)  
lines(smooth.spline(pred, resid))
### Even with adding random effect on slope, pattern in residual still present