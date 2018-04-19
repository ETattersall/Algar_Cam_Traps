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


#### Caribou models ####
## Lowland measured at 1750m, LD measured at 1750m
# Not including Dist2Water --> no corresponding hypothesis

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


modnames <- c("NULL", "CH1", "CH2", "CH3", "CH4", "CH5", "CH6", "CH7", "CH8", "CH9", "CH10", "CH11", "CH12")
cabtab <- ICtab(CH0, CH1, CH2, CH3, CH4, CH5, CH6, CH7, CH8, CH9, CH10, CH11, CH12, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
cabtab
#     dLogLik dAIC df weight
# CH2  13.8     0.0 8  0.157 
# CH10 17.8     0.1 12 0.153 
# CH11 17.6     0.3 12 0.133 
# CH12 18.6     0.4 13 0.127 
# CH5  16.4     0.9 11 0.101 
# CH4  13.3     1.0 8  0.095 
# CH8  16.2     1.2 11 0.085 
# CH9  17.1     1.4 12 0.077 
# CH6  14.0     1.6 9  0.070 
# CH7   5.5    22.7 11 <0.001
# CH3   2.3    23.1 8  <0.001
# CH1   3.5    24.7 10 <0.001
# NULL  0.0    25.6 7  <0.001

summary(CH12)


