################################
# ModelDataCombine.R
# Preparing data for GLMMs from monthly detection data, GIS data, field data...
## Also checking collinearities between explanatory variables
# March 15, 2018
################################
library(dplyr)
library(reshape)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

# Read in data from cameras
det <- read.csv("MonthlyDetections_nov2015-nov2017.csv")
head(det)
det$X <- NULL
head(det)

## Adding column for month only
## Separating elements from Yr_Month
det$Yr_Month2 <- det$Yr_Month #replicating Yr_Month for separation
det <- cbind(det[,1:12],
             colsplit(det$Yr_Month2, "[-]", names=c("Year", "Month")))



## Including mean VegHt (measured in Nov 2017)
nov <- read.csv("Station_data/Algar_CameraStationData_Nov2017.csv", header = T)

#Subset nov for 60 cams on lines
nov <- nov[1:60,]
#### Veg. Height - from Nov. 2017 ####
## Avg. Veg Height measured 
nov$LineVeg_Ht_avg <- rowMeans(nov[ , 24:26], na.rm = TRUE)
## Examining offline vegetation
plot(nov$TreatmentType, nov$LineVeg_Ht_avg)
OL <- nov %>% filter(TreatmentType == "OffLine")
summary(OL$LineVeg_Ht_avg)

det$VegHt <- nov$LineVeg_Ht_avg[match(det$Site, nov$SiteID)]

#### Add Line Width ####
# Review line width data
hist(nov$Line_Width_m) #outlier of 70, must be error
table(nov$Line_Width_m)
nov[nov$Line_Width_m == 70, ] #Algar09, should be 7.0
nov[which(nov$Line_Width_m == 70), 28] <- 7.0
hist(nov$Line_Width_m)


det$LineWidth <- nov$Line_Width_m[match(det$Site, nov$SiteID)]

write.csv(det, "GLMMdata_3deployments.csv")

## Checking collinearities between covariates measured in field and treatment
hist(det$VegHt)
plot(det$Treatment, det$VegHt)
veg.lm <- lm(VegHt ~ Treatment, data  = det)
summary(veg.lm) #All measured veghts significantly different from control

# Change levels for better visualisation
det$Treatment <- factor(det$Treatment, levels = c("HumanUse", "Control", "SPP", "NatRegen"))
veg.lm <- lm(VegHt ~ Treatment, data  = det)
summary(veg.lm) # Increasing veg height across treatments, as expected
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)        0.70643    0.05057  13.970  < 2e-16 ***
#  TreatmentControl   0.50524    0.07444   6.788 1.65e-11 ***
#  TreatmentSPP       0.28373    0.06528   4.346 1.48e-05 ***
#  TreatmentNatRegen  1.04163    0.07444  13.994  < 2e-16 ***

plot(det$Treatment, det$VegHt, ylab = "VegHt (m)", xlab = "Treatments")


# Line Width
plot(det$Treatment, det$LineWidth, ylab = "LineWidth (m)", xlab = "Treatments") ## Control and SPP medians slightly less than other 2, but large variance

# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)        7.39286    0.07296 101.330  < 2e-16 ***
#   TreatmentControl  -0.93452    0.10739  -8.702  < 2e-16 ***
#   TreatmentSPP      -1.03571    0.09419 -10.996  < 2e-16 ***
#   TreatmentNatRegen -0.60119    0.10739  -5.598 2.58e-08 ***

wid.lm <- lm(LineWidth ~ Treatment, data = det)
summary(wid.lm) ## LineWidth differences across treatments are <1m. Significant differences between treatments BUT given the low-precision of measurement method, small differences are acceptable

# Line Width with VegHt
plot(det$LineWidth, det$VegHt, ylab = "VegHt (m)", xlab = "LineWidth (m)")
abline(a = 0.165, b = 0.013, col = "red")
vegwid.lm <- lm(VegHt ~ LineWidth, data = det)
summary(vegwid.lm)

# Coefficients:
#               Estimate Std. Error t value   Pr(>|t|)    
#  (Intercept)  0.01278    0.12339   0.104    0.918    
#  LineWidth    0.16527    0.01798   9.190   <2e-16 ***

######## March 22, 2018: Collinearity of covariates extracted so far --> using pairplots
## det already contains Treatment, Line Densities for 250-1250m scales (currently; not saved together), dWater, and SnowDays
## Need to add new lowland at 500m.
## Also include random effects: Site and Month

low500 <- read.csv("proplowland_500mbuffer_newAVIE.csv")

## Data frame of covariates only
covar <- det %>% select(Site, Treatment, SnowDays, Month, Dist2water_km, LD250,LD500, LD750, LD1000, LD1250)
covar$low500 <- low500$Percent_cover[match(covar$Site, low500$CamStation)]

## pairplot of covariates
pairs(covar)

### Add line width, veg height, plot spatial covariates only with spatial covariates
sp.covar <- as.data.frame(unique(det$Site))
colnames(sp.covar) <- "Site"
sp.covar$Treatment <- nov$TreatmentType[match(sp.covar$Site, nov$SiteID)]
sp.covar$VegHt <- nov$LineVeg_Ht_avg[match(sp.covar$Site, nov$SiteID)]
sp.covar$LineWidth <- nov$Line_Width_m[match(sp.covar$Site, nov$SiteID)]
sp.covar$Dist2Water_km <- det$Dist2water_km[match(sp.covar$Site, det$Site)]
sp.covar$LD250 <- det$LD250[match(sp.covar$Site, det$Site)]
sp.covar$LD500 <- det$LD500[match(sp.covar$Site, det$Site)]
sp.covar$LD750 <- det$LD750[match(sp.covar$Site, det$Site)]
sp.covar$LD1000 <- det$LD1000[match(sp.covar$Site, det$Site)]
sp.covar$LD1250 <- det$LD1250[match(sp.covar$Site, det$Site)]
sp.covar$low500 <- low500$Percent_cover[match(sp.covar$Site, low500$CamStation)]

pairs(sp.covar)
summary(sp.covar)

write.csv(sp.covar, "Spatial_covariates.csv")


### Apr. 4, 2018: Spatial covariates are likely collinear with Wolf detections, which could affect their use as a covariate ####
covar <- read.csv("Spatial_covariates.csv") 
GLMMdata <- read.csv("GLMMdata_3deployments.csv") ## Only differs from MonthlyDetections by not have distance to Water

###### LOS ####
apr2018 <- read.csv("Station_data/Algar_CameraStationData_Apr2018.csv")
str(apr2018)

## LOS averages for both directions
apr2018$Dir1_MaxMean <- rowMeans(apr2018[ , 29:31], na.rm = TRUE)
summary(apr2018$Dir1_MaxMean)
hist(apr2018$Dir1_MaxMean) #roughly poisson distributed

apr2018$Dir2_MaxMean <- rowMeans(apr2018[ , 37:39], na.rm = TRUE)
summary(apr2018$Dir2_MaxMean)
hist(apr2018$Dir2_MaxMean)

##Dir1 and Dir2 are arbitrarily assigned --> average those for an overall site score
apr2018$LOS_mean <- rowMeans(apr2018[, 43:44], na.rm = TRUE)
table(apr2018$LOS_mean) #4 sites have 600 as value --> indicates that rangefinder maxed out. How to address?
summary(apr2018$LOS_mean)
hist(apr2018$LOS_mean)

### Save to csv
write.csv(apr2018, "Station_data/Algar_CameraStationData_Apr2018.csv")