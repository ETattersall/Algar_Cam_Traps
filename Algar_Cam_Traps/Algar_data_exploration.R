######################################
# Algar_data_exploration.R
# Checking for collinearities, outliers, 0's, etc (see Zuur et al., 2010)
# Started Jan. 30, 2018
######################################


library(ggplot2) #for plotting magic
library(dplyr) #for data management convenience
library(tidyr) #for data management (gather function)

## Data available so far: Detections (up to Nov. 2017), treatments, AVIE at 500m and 250m (imperfect - some stations cut off), snow data up to Nov. 2016
# Detection data - explore pilot, then full separately

## Pilot data (24 cameras, Nov. 2015-2016)
getwd()

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
dat <- read.csv("2015.01_monthlydetections.csv") # First deployment monthly detection data
head(dat)
dat$X.1 <- NULL
dat$X <- NULL

#Loading % lowland data
low <- read.csv("Lowlandcover_9buffersizes.csv")
# Forgot to include 750m scale. Not really imp. as there is little diff. across scales
prop4areas <- cbind.data.frame(low$Prop250, low$Prop500, low$Prop1000, low$Prop2000)
colnames(prop4areas) <- c("250m", "500m", "1000m", "2000m")
prop4areas$Station <- low$CamStation
summary(prop4areas) #approximately the same

##Add 250m and 500m to monthly detections
dat$low250 <- low$Prop250[match(dat$Site, low$CamStation)]
dat$low500 <- low$Prop500[match(dat$Site, low$CamStation)]

#### Step 1. Check outliers ####

#### Species: gather species together for boxplot for every species ####
d1 <- gather(data = dat, key = Species, value = Detections, 5:11)

#ggplot code for boxplot
bp <- ggplot(data = d1, aes(x = Species, y = Detections, fill = Species))
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

# For pilot data
ggplot(data = d1, aes(x = Treatment, y = Detections, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Detections/Month") + scale_x_discrete(limits=c("Control", "SPP")) + scale_fill_manual(values=c("orange", "purple")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species, ncol = 2, scales = "free_y") + theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + theme(strip.text = element_text(colour = "black", size = 14)) + theme(aspect.ratio=1)

# For all detection data
full <- read.csv("MonthlyDetections_nov2015-nov2017.csv")
full$X <- NULL
f1 <- gather(data = full, key = Species, value = Detections, 5:11)

ggplot(data = f1, aes(x = Treatment, y = Detections, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Detections/Month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("red", "light green", "orange", "purple")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species, nrow=3, scales = "free_y") + theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + theme(strip.text = element_text(colour = "black", size = 14)) + theme(aspect.ratio=1)






#### Levene's test for equal variance. Tests the null hypothesis that variance are equal ####

install.packages("Rcmdr")
library(Rcmdr)

wolf.lm <- lm(data = dat, Wolf~Treatment)
leveneTest(wolf.lm) #p = 0.044 --> unequal variance

#Blackbear
Blackbear.lm <- lm(data = dat, Blackbear~Treatment)
leveneTest(Blackbear.lm) #p = 0.191 --> equal variance

#Caribou
Caribou.lm <- lm(data = dat, Caribou~Treatment)
leveneTest(Caribou.lm) #p = 0.010

#WTDeer
WTDeer.lm <- lm(data = dat, WTDeer~Treatment)
leveneTest(WTDeer.lm) # p = 1.98e-05

#Moose
Moose.lm <- lm(data = dat, Moose~Treatment)
leveneTest(Moose.lm) #p = 0.322

#Coyote
Coyote.lm <- lm(data = dat, Coyote~Treatment)
leveneTest(Coyote.lm) #p = 0.028

#Lynx
Lynx.lm <- lm(data = dat, Lynx~Treatment)
leveneTest(Lynx.lm) #p = 0.430

#### Residuals plotting for explanatory variables that are continuous (SnowDays, low500) ####
Wolf.snow <- lm(Wolf~SnowDays, data = dat)
plot(Wolf.snow)  #Residuals not centred around zero --> unequal variance. Mean residuals = slightly negative. Makes sense because the mean detection is slightly above zero, but most points are zero
qqnorm(dat$Wolf) #We know data is non-normal, as it is count data

Bear.snow <- lm(Blackbear~SnowDays, data = dat)
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

