#######################################
# Lowland_scaleAnalysis.R
# Running GLMs to determine appropriate scale for lowland data
# Started Apr.18, 2018 by Erin
######################################

library(dplyr)
require(glmmTMB)
require(bbmle)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

#Load detection data
det <- read.csv("MonthlyDetections_nov2015-nov2017.csv")
#Load lowland data
low <- read.csv("newAVIE_lowland8buffersizes.csv")
low$X <- NULL

## Combine datasets
det$low250 <- low$low250[match(det$Site, low$CamStation)]
det$low500 <- low$low500[match(det$Site, low$CamStation)]
det$low750 <- low$low750[match(det$Site, low$CamStation)]
det$low1000 <- low$low1000[match(det$Site, low$CamStation)]
det$low1250 <- low$low1250[match(det$Site, low$CamStation)]
det$low1500 <- low$low1500[match(det$Site, low$CamStation)]
det$low1750 <- low$low1750[match(det$Site, low$CamStation)]
det$low2000 <- low$low2000[match(det$Site, low$CamStation)]

### Wolf models
Wolf.0 <- glmmTMB(Wolf~1, data = det, family = nbinom2)
Wolf.250 <- glmmTMB(Wolf~low250, data = det, family = nbinom2)
Wolf.500 <- glmmTMB(Wolf~low500, data = det, family = nbinom2)
Wolf.750 <- glmmTMB(Wolf~low750, data = det, family = nbinom2)
Wolf.1000 <- glmmTMB(Wolf~low1000, data = det, family = nbinom2)
Wolf.1250 <- glmmTMB(Wolf~low1250, data = det, family = nbinom2)
Wolf.1500 <- glmmTMB(Wolf~low1500, data = det, family = nbinom2)
Wolf.1750 <- glmmTMB(Wolf~low1750, data = det, family = nbinom2)
Wolf.2000 <- glmmTMB(Wolf~low2000, data = det, family = nbinom2)

## Model selection with AICctab (bbmle) --> exluding VegHt
modnames <- c("NULL","250m", "500m", "750m", "1000m", "1250m", "1500", "1750", "2000")
wolftab <- ICtab(Wolf.0,Wolf.250,Wolf.500,Wolf.750,Wolf.1000,Wolf.1250, Wolf.1500, Wolf.1750, Wolf.2000, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
wolftab

#    dLogLik dAIC df weight
# 500m  4.6     0.0  3  0.7125
# 2000  2.7     3.9  3  0.1033
# 750m  2.3     4.6  3  0.0703
# 1000m 1.8     5.7  3  0.0412
# 1750  1.2     6.9  3  0.0223
# NULL  0.0     7.3  2  0.0190
# 1500  0.6     8.1  3  0.0122
# 1250m 0.5     8.2  3  0.0116
# 250m  0.1     9.0  3  0.0077

# Plotting model weights for each scale to determine best scale
class(wolftab) <- "data.frame"


wolftab$scale <- c(500,2000,750,1000,1750,"NULL",1500,1250,250) #ordered according to ICtab 

#Remove NULL row
tab <- wolftab %>% filter(scale != "NULL")
str(tab)
#Convert to numeric
tab$scale <- as.numeric(tab$scale)
# Plot weight for each scale
op <- par(mar = c(5,5,4,2) + 0.1)
plot(tab$scale, tab$weight, xlim=range(tab$scale), ylim=c(0,1), xlab = "Scale (m)", ylab = "AICweight", main = "Wolf", pch=16, cex = 1, cex.axis = 2, cex.lab = 2, cex.main = 2)
lines(tab$scale[order(tab$scale)], tab$weight[order(tab$scale)], xlim=range(tab$scale), ylim=c(0,1))

### Caribou models
Caribou.0 <- glmmTMB(Caribou~1, data = det, family = nbinom2)
Caribou.250 <- glmmTMB(Caribou~low250, data = det, family = nbinom2)
Caribou.500 <- glmmTMB(Caribou~low500, data = det, family = nbinom2)
Caribou.750 <- glmmTMB(Caribou~low750, data = det, family = nbinom2)
Caribou.1000 <- glmmTMB(Caribou~low1000, data = det, family = nbinom2)
Caribou.1250 <- glmmTMB(Caribou~low1250, data = det, family = nbinom2)
Caribou.1500 <- glmmTMB(Caribou~low1500, data = det, family = nbinom2)
Caribou.1750 <- glmmTMB(Caribou~low1750, data = det, family = nbinom2)
Caribou.2000 <- glmmTMB(Caribou~low2000, data = det, family = nbinom2)

## Model selection with AICctab (bbmle) --> exluding VegHt
modnames <- c("NULL","250m", "500m", "750m", "1000m", "1250m", "1500", "1750", "2000")
Cariboutab <- ICtab(Caribou.0,Caribou.250,Caribou.500,Caribou.750,Caribou.1000,Caribou.1250, Caribou.1500, Caribou.1750, Caribou.2000, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
Cariboutab

#    dLogLik dAIC df weight
# 1750  30.9     0.0 3  0.758 
# 2000  29.2     3.3 3  0.144 
# 1500  28.7     4.3 3  0.086 
# 1250m 26.6     8.5 3  0.011 
# 1000m 22.2    17.3 3  <0.001
# 750m  18.0    25.7 3  <0.001
# 500m  12.4    37.0 3  <0.001
# 250m   3.1    55.5 3  <0.001
# NULL   0.0    59.8 2  <0.001

# Plotting model weights for each scale to determine best scale
class(Cariboutab) <- "data.frame"


Cariboutab$scale <- c(1750,2000,1500,1250,1000,750,500,250,"NULL") #ordered according to ICtab 

#Remove NULL row
tab <- Cariboutab %>% filter(scale != "NULL")
str(tab)
#Convert to numeric
tab$scale <- as.numeric(tab$scale)
# Plot weight for each scale
op <- par(mar = c(5,5,4,2) + 0.1)
plot(tab$scale, tab$weight, xlim=range(tab$scale), ylim=c(0,1), xlab = "Scale (m)", ylab = "AICweight", main = "Caribou", pch=16, cex = 1, cex.axis = 2, cex.lab = 2, cex.main = 2)
lines(tab$scale[order(tab$scale)], tab$weight[order(tab$scale)], xlim=range(tab$scale), ylim=c(0,1))

### WTDeer models
WTDeer.0 <- glmmTMB(WTDeer~1, data = det, family = nbinom2)
WTDeer.250 <- glmmTMB(WTDeer~low250, data = det, family = nbinom2)
WTDeer.500 <- glmmTMB(WTDeer~low500, data = det, family = nbinom2)
WTDeer.750 <- glmmTMB(WTDeer~low750, data = det, family = nbinom2)
WTDeer.1000 <- glmmTMB(WTDeer~low1000, data = det, family = nbinom2)
WTDeer.1250 <- glmmTMB(WTDeer~low1250, data = det, family = nbinom2)
WTDeer.1500 <- glmmTMB(WTDeer~low1500, data = det, family = nbinom2)
WTDeer.1750 <- glmmTMB(WTDeer~low1750, data = det, family = nbinom2)
WTDeer.2000 <- glmmTMB(WTDeer~low2000, data = det, family = nbinom2)

## Model selection with AICctab (bbmle) --> exluding VegHt
modnames <- c("NULL","250m", "500m", "750m", "1000m", "1250m", "1500", "1750", "2000")
WTDeertab <- ICtab(WTDeer.0,WTDeer.250,WTDeer.500,WTDeer.750,WTDeer.1000,WTDeer.1250, WTDeer.1500, WTDeer.1750, WTDeer.2000, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
WTDeertab

#    dLogLik dAIC df weight
# 2000  41.3     0.0 3  1     
# 1750  33.6    15.3 3  <0.001
# 1500  25.8    30.9 3  <0.001
# 500m  21.1    40.4 3  <0.001
# 1250m 21.0    40.6 3  <0.001
# 1000m 20.4    41.9 3  <0.001
# 250m  20.0    42.6 3  <0.001
# 750m  18.4    45.8 3  <0.001
# NULL   0.0    80.6 2  <0.001

# Plotting model weights for each scale to determine best scale
class(WTDeertab) <- "data.frame"


WTDeertab$scale <- c(2000,1750,1500,500,1250,1000,250,750,"NULL") #ordered according to ICtab 

#Remove NULL row
tab <- WTDeertab %>% filter(scale != "NULL")
str(tab)
#Convert to numeric
tab$scale <- as.numeric(tab$scale)
# Plot weight for each scale
op <- par(mar = c(5,5,4,2) + 0.1)
plot(tab$scale, tab$weight, xlim=range(tab$scale), ylim=c(0,1), xlab = "Scale (m)", ylab = "AICweight", main = "WT Deer", pch=16, cex = 1, cex.axis = 2, cex.lab = 2, cex.main = 2)
lines(tab$scale[order(tab$scale)], tab$weight[order(tab$scale)], xlim=range(tab$scale), ylim=c(0,1))

### Moose models
Moose.0 <- glmmTMB(Moose~1, data = det, family = nbinom2)
Moose.250 <- glmmTMB(Moose~low250, data = det, family = nbinom2)
Moose.500 <- glmmTMB(Moose~low500, data = det, family = nbinom2)
Moose.750 <- glmmTMB(Moose~low750, data = det, family = nbinom2)
Moose.1000 <- glmmTMB(Moose~low1000, data = det, family = nbinom2)
Moose.1250 <- glmmTMB(Moose~low1250, data = det, family = nbinom2)
Moose.1500 <- glmmTMB(Moose~low1500, data = det, family = nbinom2)
Moose.1750 <- glmmTMB(Moose~low1750, data = det, family = nbinom2)
Moose.2000 <- glmmTMB(Moose~low2000, data = det, family = nbinom2)

## Model selection with AICctab (bbmle) --> exluding VegHt
modnames <- c("NULL","250m", "500m", "750m", "1000m", "1250m", "1500", "1750", "2000")
Moosetab <- ICtab(Moose.0,Moose.250,Moose.500,Moose.750,Moose.1000,Moose.1250, Moose.1500, Moose.1750, Moose.2000, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
Moosetab

#    dLogLik dAIC df weight
# 250m  2.4     0.0  3  0.438 
# 500m  1.4     2.0  3  0.164 
# NULL  0.0     2.8  2  0.110 
# 750m  0.5     3.8  3  0.067 
# 1000m 0.2     4.3  3  0.051 
# 1250m 0.1     4.5  3  0.046 
# 1500  0.1     4.6  3  0.043 
# 1750  0.0     4.7  3  0.041 
# 2000  0.0     4.8  3  0.041

# Plotting model weights for each scale to determine best scale
class(Moosetab) <- "data.frame"


Moosetab$scale <- c(250,500,"NULL",750,1000,1250,1500,1750,2000) #ordered according to ICtab 

#Remove NULL row
tab <- Moosetab %>% filter(scale != "NULL")
str(tab)
#Convert to numeric
tab$scale <- as.numeric(tab$scale)
# Plot weight for each scale
op <- par(mar = c(5,5,4,2) + 0.1)
plot(tab$scale, tab$weight, xlim=range(tab$scale), ylim=c(0,1), xlab = "Scale (m)", ylab = "AICweight", main = "Moose", pch=16, cex = 1, cex.axis = 2, cex.lab = 2, cex.main = 2)
lines(tab$scale[order(tab$scale)], tab$weight[order(tab$scale)], xlim=range(tab$scale), ylim=c(0,1))


## Blackbear, truncated season
bear <- det %>% filter(Month >= 4 & Month <= 10) %>% select(Site, Treatment,Yr_Month, Site_ym, Blackbear, SnowDays, Year, Month, Dist2water_km, low250,low500,low750,low1000,low1250, low1500, low1750, low2000)

### Blackbear models
Blackbear.0 <- glmmTMB(Blackbear~1, data = bear, family = nbinom2)
Blackbear.250 <- glmmTMB(Blackbear~low250, data = bear, family = nbinom2)
Blackbear.500 <- glmmTMB(Blackbear~low500, data = bear, family = nbinom2)
Blackbear.750 <- glmmTMB(Blackbear~low750, data = bear, family = nbinom2)
Blackbear.1000 <- glmmTMB(Blackbear~low1000, data = bear, family = nbinom2)
Blackbear.1250 <- glmmTMB(Blackbear~low1250, data = bear, family = nbinom2)
Blackbear.1500 <- glmmTMB(Blackbear~low1500, data = bear, family = nbinom2)
Blackbear.1750 <- glmmTMB(Blackbear~low1750, data = bear, family = nbinom2)
Blackbear.2000 <- glmmTMB(Blackbear~low2000, data = bear, family = nbinom2)

## Model selection with AICctab (bbmle) --> exluding VegHt
modnames <- c("NULL","250m", "500m", "750m", "1000m", "1250m", "1500m", "1750m", "2000m")
Blackbeartab <- ICtab(Blackbear.0,Blackbear.250,Blackbear.500,Blackbear.750,Blackbear.1000,Blackbear.1250, Blackbear.1500, Blackbear.1750, Blackbear.2000, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
Blackbeartab

#    dLogLik dAIC df weight
# 500m  2.6     0.0  3  0.295 
# 2000m 1.9     1.4  3  0.150 
# 250m  1.5     2.2  3  0.097 
# 1000m 1.4     2.3  3  0.091 
# 1750m 1.4     2.4  3  0.087 
# 1500m 1.3     2.6  3  0.081 
# 750m  1.2     2.8  3  0.073 
# 1250m 1.1     2.9  3  0.068 
# NULL  0.0     3.2  2  0.059 


# Plotting model weights for each scale to determine best scale
class(Blackbeartab) <- "data.frame"


Blackbeartab$scale <- c(500,2000,250,1000,1750,1500,750,1250, "NULL") #ordered according to ICtab 

#Remove NULL row
tab <- Blackbeartab %>% filter(scale != "NULL")
str(tab)
#Convert to numeric
tab$scale <- as.numeric(tab$scale)
# Plot weight for each scale
op <- par(mar = c(5,5,4,2) + 0.1)
plot(tab$scale, tab$weight, xlim=range(tab$scale), ylim=c(0,1), xlab = "Scale (m)", ylab = "AICweight", main = "Black bear", pch=16, cex = 1, cex.axis = 2, cex.lab = 2, cex.main = 2)
lines(tab$scale[order(tab$scale)], tab$weight[order(tab$scale)], xlim=range(tab$scale), ylim=c(0,1))

###################################################################################
## Double checking scale analysis, using multivariate models rather than univariate
## In particular, scale analyses were inconclusive for moose and wolves when assessing effects of linear density
## Also using updated data
library(MuMIn) #streamlining model selection

det <- read.csv("Seismic_nov2015-apr2018.csv")
det$X.3 <- NULL
det$X.1 <- NULL
det$X.2 <- NULL
det$X <- NULL



sp <- read.csv("Spatial_covariates.csv")

det$VegHt <- sp$VegHt[match(det$Site, sp$Site)]
det$LineWidth <- sp$LineWidth[match(det$Site, sp$Site)]

LineDens <- read.csv("AlgarStationsLD_Lines.csv")

det$low250 <- low$low250[match(det$Site, low$CamStation)]
det$low500 <- low$low500[match(det$Site, low$CamStation)]
det$low750 <- low$low750[match(det$Site, low$CamStation)]
det$low1000 <- low$low1000[match(det$Site, low$CamStation)]
det$low1250 <- low$low1250[match(det$Site, low$CamStation)]
det$low1500 <- low$low1500[match(det$Site, low$CamStation)]
det$low1750 <- low$low1750[match(det$Site, low$CamStation)]
det$low2000 <- low$low2000[match(det$Site, low$CamStation)]

det$LD250 <- LineDens$X250m[match(det$Site, LineDens$CamStation)]
det$LD500 <- LineDens$X500m[match(det$Site, LineDens$CamStation)]
det$LD750 <- LineDens$X750m[match(det$Site, LineDens$CamStation)]
det$LD1000 <- LineDens$X1000m[match(det$Site, LineDens$CamStation)]
det$LD1250 <- LineDens$X1250m[match(det$Site, LineDens$CamStation)]
det$LD1500 <- LineDens$X1500m[match(det$Site, LineDens$CamStation)]
det$LD1750 <- LineDens$X1750m[match(det$Site, LineDens$CamStation)]
det$LD2000 <- LineDens$X2000m[match(det$Site, LineDens$CamStation)]

## Standardize and centre covariates
covscale <- function(covariate){
  centre <- (covariate - mean(covariate, na.rm = TRUE)) # where covariate is a vector of numeric input values for one covariate
  standardize <- centre/(2*sd(covariate, na.rm = TRUE)) # Divide centred value by 2 SD
}
det$low250_sc <- covscale(det$low250)
det$low500_sc <- covscale(det$low500)
det$low750_sc <- covscale(det$low750)
det$low1000_sc <- covscale(det$low1000)
det$low1250_sc <- covscale(det$low1250)
det$low1500_sc <- covscale(det$low1500)
det$low1750_sc <- covscale(det$low1750)
det$low2000_sc <- covscale(det$low2000)
det$LD250_sc <- covscale(det$LD250)
det$LD500_sc <- covscale(det$LD500)
det$LD750_sc <- covscale(det$LD750)
det$LD1000_sc <- covscale(det$LD1000)
det$LD1250_sc <- covscale(det$LD1250)
det$LD1500_sc <- covscale(det$LD1500)
det$LD1750_sc <- covscale(det$LD1750)
det$LD2000_sc <- covscale(det$LD2000)
det$LineWidth_sc <- covscale(det$LineWidth)
det$VegHt_sc <- covscale(det$VegHt)
det$pSnow_sc <- covscale(det$pSnow)
det$ActiveDays_sc <- covscale(det$ActiveDays)

### Check multivariate models with Moose, Wolf ~ spatial covariates measured at scales between 250 - 2000 m
## Include known influential covariates, but not global model
# Moose --> Line width & veght included
Moose.0 <- glmmTMB(Moose~1, data = det, family = nbinom2)
Moose.250 <- glmmTMB(Moose~low250_sc + LD250_sc + LineWidth_sc + VegHt_sc, data = det, family = nbinom2)
Moose.500 <- glmmTMB(Moose~low500_sc + LD500_sc + LineWidth_sc + VegHt_sc, data = det, family = nbinom2)
Moose.750 <- glmmTMB(Moose~low750_sc + LD750_sc + LineWidth_sc + VegHt_sc, data = det, family = nbinom2)
Moose.1000 <- glmmTMB(Moose~low1000_sc + LD1000_sc + LineWidth_sc + VegHt_sc, data = det, family = nbinom2)
Moose.1250 <- glmmTMB(Moose~low1250_sc + LD1250_sc + LineWidth_sc + VegHt_sc, data = det, family = nbinom2)
Moose.1500 <- glmmTMB(Moose~low1500_sc + LD1500_sc + LineWidth_sc + VegHt_sc, data = det, family = nbinom2)
Moose.1750 <- glmmTMB(Moose~low1750_sc + LD1750_sc + LineWidth_sc + VegHt_sc, data = det, family = nbinom2)
Moose.2000 <- glmmTMB(Moose~low2000_sc + LD2000_sc + LineWidth_sc + VegHt_sc, data = det, family = nbinom2)

modnames <- c("NULL","250m", "500m", "750m", "1000m", "1250m", "1500", "1750", "2000")
Moosetab <- ICtab(Moose.0,Moose.250,Moose.500,Moose.750,Moose.1000,Moose.1250, Moose.1500, Moose.1750, Moose.2000, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
Moosetab

## 2000 m scale wins
summary(Moose.2000) ## All effects are in same direction and relatively same sizes, 

## Use full, zero- inflated model with 2000 m scale
MooseZI <- glmmTMB(Moose~Treatment + low2000_sc + LD2000_sc + LineWidth_sc + VegHt_sc + pSnow_sc + ActiveDays_sc + (1|Site) + (1|Month), zi=~1, data = det, family = nbinom2)

summary(MooseZI) ##Effect sizes are comparable to model averaged effects --> keep the original scales


Wolf.0 <- glmmTMB(Wolf~1, data = det, family = nbinom2)
Wolf.250 <- glmmTMB(Wolf~low250_sc + LD250_sc + pSnow_sc + Treatment, data = det, family = nbinom2)
Wolf.500 <- glmmTMB(Wolf~low500_sc + LD500_sc + pSnow_sc + Treatment, data = det, family = nbinom2)
Wolf.750 <- glmmTMB(Wolf~low750_sc + LD750_sc +  pSnow_sc + Treatment, data = det, family = nbinom2)
Wolf.1000 <- glmmTMB(Wolf~low1000_sc + LD1000_sc + pSnow_sc + Treatment, data = det, family = nbinom2)
Wolf.1250 <- glmmTMB(Wolf~low1250_sc + LD1250_sc +  pSnow_sc + Treatment, data = det, family = nbinom2)
Wolf.1500 <- glmmTMB(Wolf~low1500_sc + LD1500_sc + pSnow_sc + Treatment, data = det, family = nbinom2)
Wolf.1750 <- glmmTMB(Wolf~low1750_sc + LD1750_sc + pSnow_sc + Treatment, data = det, family = nbinom2)
Wolf.2000 <- glmmTMB(Wolf~low2000_sc + LD2000_sc + pSnow_sc + Treatment, data = det, family = nbinom2)

modnames <- c("NULL","250m", "500m", "750m", "1000m", "1250m", "1500", "1750", "2000")
Wolftab <- ICtab(Wolf.0,Wolf.250,Wolf.500,Wolf.750,Wolf.1000,Wolf.1250, Wolf.1500, Wolf.1750, Wolf.2000, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
Wolftab
## Effects of lowland drive model selection --> 500m beats other with ~20% more model weight

## Full ZI model with LD and low measured at 500m
WolfZI <- glmmTMB(Wolf~Treatment + low500_sc + LD500_sc + pSnow_sc + ActiveDays_sc + (1|Site) + (1|Month), zi=~1, data = det, family = nbinom2)

summary(WolfZI) ## LD has

#### Given that lowland and linear density aren't strong effects for either of these species, and effect estimates don't change much when the scale is changed, I will keep the original scale analysis results