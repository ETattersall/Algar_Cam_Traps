##################################
# Algar_fieldData.R
# Exploring and extracting useful data from field measurements
# Measurements taken Apr 2017, Nov 2017
#################################

library(dplyr)
library(tidyr)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data/Station_data")
apr <- read.csv("Algar_CameraFieldData_April2017.csv")
nov <- read.csv("Algar_CameraStationData_Nov2017.csv")

#Subset nov for 60 cams on lines
nov <- nov[1:60,]

#Compare variables

str(apr) #Line veg height, mounds, only measured once. Do not use these measurements
str(nov)
unique(nov$TreeSp_Dom1) # 4 : BlackSpruce, Tamarack, Aspen, JackPine
unique(apr$TreeSp_Dom1) # 5 levels: tamarack, black spruce, Aspen, birch, JackPine

#See if line dom. tree species, line widths match between Apr. and Nov.
ifelse(apr$TreeSp_Dom1==nov$TreeSp_Dom1,"Y", "N") # error because the level sets of factors are different
table(apr$TreeSp_Dom1) # Aspen-1, birch-1, black spruce-49, jackpine-1, tamarack-8
table(nov$TreeSp_Dom1) # Aspen-1, black spruce-52, jackpine-2, tamarack-5
table(ifelse(apr$Line_Width_m==nov$Line_Width_m,"Y", "N")) #49 do not agree
width.diff <- ifelse(apr$Line_Width_m==nov$Line_Width_m,"Y", apr$Line_Width_m - nov$Line_Width_m)
## Widths are very different. in places


## Possible variables of interest + hypotheses (general):
# Line veg height: Increase in veg height decreases pred. detections (movement barrier)
# Line width: Increase in line width increases pred. detections, dec. caribou detections
# Dom. tree species


#### Veg. Height - from Nov. 2017 ####
## Avg. Veg Height measured 
nov$LineVeg_Ht_avg <- rowMeans(nov[ , 24:26], na.rm = TRUE)

### Line Veg data frame

LineVeg <- cbind.data.frame(nov$SiteID, nov$TreatmentType, nov$LineVeg_GS,nov$LineVeg_M, nov$LineVeg_S, nov$LineVeg_T, nov$Seedlings, nov$LineVeg_Ht_avg)
colnames(LineVeg) <- c("SiteID", "TreatmentType", "Grasses.Sedges", "Mosses", "Shrubs", "Trees", "Seedlings", "Avg_VegHt")

### Add lowland data
#Loading % lowland data
low <- read.csv("Lowlandcover_9buffersizes.csv")
##Add 250m and 500m to monthly detections

LineVeg$Low500 <- low$Prop500[match(LineVeg$SiteID, low$CamStation)]
LineVeg$Low250 <- low$Prop250[match(LineVeg$SiteID, low$CamStation)]

## Line Veg now contains available site-specific covariates

### Collinearities between Veg height and treatment?
plot(LineVeg$TreatmentType, LineVeg$Avg_VegHt)
Veg.Treat <- lm(data = LineVeg, formula = Avg_VegHt~TreatmentType - 1)
summary(Veg.Treat) ## Line Veg not significantly different

### Collinearities between Ht and %low
plot(LineVeg$Low500, LineVeg$Avg_VegHt)
Veg.low <- lm(data = LineVeg, formula = Avg_VegHt~Low500)
summary(Veg.low)

### Plot against response variables
dat <- read.csv("monthlydetections_nov2015-apr2017.csv") # First 2 deployments monthly detection data + snow days
head(dat)
dat$X <- NULL
head(dat)

dat$VegHt <- LineVeg$Avg_VegHt[match(dat$Site, LineVeg$SiteID)]

plot(dat$VegHt,dat$Wolf)
plot(dat$VegHt, dat$Caribou) 
plot(dat$VegHt, dat$Blackbear)
plot(dat$VegHt, dat$WTDeer)
plot(dat$VegHt, dat$Moose)
## declines with veght for wolf, caribou, not for blackbear and deer. Moose less clear

plot(dat$SnowDays, dat$VegHt)
Sno.veg <- lm(data = dat, VegHt~SnowDays)
summary(Sno.veg) # As expected, no relationship
