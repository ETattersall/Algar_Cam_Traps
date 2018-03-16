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
