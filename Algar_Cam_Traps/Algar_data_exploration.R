######################################
# Algar_data_exploration.R
# Checking for collinearities, outliers, 0's, etc (see Zuur et al., 2010)
# Started Jan. 30, 2018
# Updated May10, 2018 with full dataset
######################################


library(ggplot2) #for plotting magic
library(dplyr) #for data management convenience
library(tidyr) #for data management (gather function)

## Data available so far: Detections (up to Nov. 2017), treatments, AVIE at 500m and 250m (imperfect - some stations cut off), snow data up to Nov. 2016
# Detection data - explore pilot, then full separately

## Pilot data (24 cameras, Nov. 2015-2016)
getwd()

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
det <- read.csv("Seismic_nov2015-apr2018.csv") # Detection deta for 30 months of surveying
head(det)
det$X.2 <- NULL
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


#### Step 1. Check outliers ####

#### Species: gather species together for boxplot for every species ####
d1 <- gather(data = det, key = Species, value = Detections, 5:11)

#ggplot code for boxplot
bp <- ggplot(data = d1, aes(x = Species, y = Detections, fill = Species)) #Coyote has one major outlier, probably Algar17 2017-08
bp + geom_boxplot() + theme_classic() + theme(legend.position = "none")

#dot plot
dp <- bp + geom_dotplot(binaxis='y', binwidth = 0.25, stackdir='center')# Species overlap, but clear that data is skewed to 0
dp
#Shows means on plot
dp + stat_summary(fun.y=mean, geom="point", shape=18,
                    size=3, color="black") + theme_classic() + theme(legend.position = "none") 

### Outcome: Data is heavily zero-inflated. Graphically, this presents all non-zero points as outliers. 

#### Step 2: Checking for homogeneity of variance ####
# If we are interested in effect of treatment on species detections, plot this in box plot. Is variation from mean similar across groups?

ggplot(data = d1, aes(x = Treatment, y = Detections, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Detections/Month") + scale_x_discrete(limits=c("Control", "HumanUse", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange","red", "purple", "green")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species, ncol = 2, scales = "free_y") + theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + theme(strip.text = element_text(colour = "black", size = 14)) + theme(aspect.ratio=1)


#### Levene's test for equal variance. Tests the null hypothesis that variance are equal ####

install.packages("Rcmdr")
library(Rcmdr)

wolf.lm <- lm(data = det, Wolf~Treatment)
leveneTest(wolf.lm) #p = 0.234 --> equal variance

#Blackbear
Blackbear.lm <- lm(data = det, Blackbear~Treatment)
leveneTest(Blackbear.lm) #p = 0.0024--> unequal variance

#Caribou
Caribou.lm <- lm(data = det, Caribou~Treatment)
leveneTest(Caribou.lm) #p = 1.51e-06

#WTDeer
WTDeer.lm <- lm(data = det, WTDeer~Treatment)
leveneTest(WTDeer.lm) # p = 2.2e-16

#Moose
Moose.lm <- lm(data = det, Moose~Treatment)
leveneTest(Moose.lm) #p = 0.0080

#Coyote
Coyote.lm <- lm(data = det, Coyote~Treatment)
leveneTest(Coyote.lm) #p = 0.117

#Lynx
Lynx.lm <- lm(data = det, Lynx~Treatment)
leveneTest(Lynx.lm) #p = 0.0038

#### Residuals plotting for explanatory variables that are continuous (SnowDays, low500) ####
Wolf.snow <- lm(Wolf~SnowDays, data = det)
plot(Wolf.snow)  #Residuals not centred around zero --> unequal variance. Mean residuals = slightly negative. Makes sense because the mean detection is slightly above zero, but most points are zero
qqnorm(det$Wolf) #We know data is non-normal, as it is count data

Bear.snow <- lm(Blackbear~SnowDays, data = det)
plot(Bear.snow)

Caribou.snow <- lm(Caribou~SnowDays, data = dat)
plot(Caribou.snow)

WTDeer.snow <- lm(WTDeer~SnowDays, data = dat)
plot(WTDeer.snow)

Moose.snow <- lm(Moose~SnowDays, data = dat)
plot(Moose.snow)

Coyote.snow <- lm(Coyote~SnowDays, data = dat)
plot(Coyote.snow)

Lynx.snow <- lm(Lynx~SnowDays, data = dat)
plot(Lynx.snow)

#Simply plotting data
plot(x=dat$SnowDays, y=dat$Wolf)
plot(x=dat$SnowDays, y=dat$Blackbear)
plot(x=dat$SnowDays, y=dat$Caribou)
plot(x=dat$SnowDays, y=dat$WTDeer)
plot(x=dat$SnowDays, y=dat$Moose)
plot(x=dat$SnowDays, y=dat$Coyote)
plot(x=dat$SnowDays, y=dat$Lynx)

## Plotting low500 data against detections
plot(x=dat$low500, y=dat$Wolf)
plot(x=dat$low500, y=dat$Blackbear)
plot(x=dat$low500, y=dat$Caribou)
plot(x=dat$low500, y=dat$WTDeer)
plot(x=dat$low500, y=dat$Moose)
plot(x=dat$low500, y=dat$Coyote)
plot(x=dat$low500, y=dat$Lynx)
#Will hold off on further exploration until we receive updated AVIE data


#### Step 3: Normality of data ####
hist(dat$Wolf)
hist(dat$Blackbear)
hist(dat$Coyote)
hist(dat$Lynx)
hist(dat$Caribou)
hist(dat$WTDeer)
hist(dat$Moose)
hist(dat$SnowDays)
hist(dat$low500)

#### Step 4: Zeroes in data: addressed in previous steps ####

