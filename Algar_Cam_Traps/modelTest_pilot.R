############################################
# modelTest_pilot.R
# Model testing with data up to April 2017
# Started Jan. 28, 2018 by Erin T.
############################################

library(lme4)
library(MASS)
library(AICcmodavg)
library(ggplot2)

getwd()

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
dat <- read.csv("Monthlydetections_nov2015-apr2017.csv") #Monthly detection data
head(dat)
dat$X <- NULL
head(dat)

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

#### Wolf Model 0: null glmm with neg. binomial distribution ####
m0.wolf <- glmer.nb(Wolf~1 + (1|Site), data = dat)
summary(m0.wolf)

#### Wolf Model 1: Treatment GLMM
m1.wolf <- glmer.nb(Wolf~Treatment + (1|Site), data = dat)
summary(m1.wolf)


#### Wolf Model 2: Treatment + %lowland 500m GLMM
m2.wolf <- glmer.nb(Wolf~Treatment + low500 + (1|Site), data = dat)
summary(m2.wolf) #Human use sig. different from Control

#### Wolf Model 3: Treat + %lowland 500m +interactions
m3.wolf <- glmer.nb(Wolf~Treatment + low500 + Treatment*low500 + (1|Site), data = dat)
summary(m3.wolf) #Model failed to converge

## Model Selection with AICC tables
cand.set.wolf <- c(m0.wolf, m1.wolf, m2.wolf, m3.wolf)
names <- c("NULL", "TREAT", "TREAT+LOW", "TREAT+LOW INTERACT")
aictab(cand.set.wolf, modnames = names, second.ord = TRUE, nobs = NULL,
       sort = TRUE)

# Model selection based on AICc:
  
#   K   AICc Delta_AICc AICcWt Cum.Wt      LL
# NULL                3 743.17       0.00   0.45   0.45 -368.57
# TREAT+LOW           7 743.97       0.81   0.30   0.75 -364.90
# TREAT               6 744.72       1.55   0.21   0.96 -366.29
# TREAT+LOW INTERACT 10 748.05       4.88   0.04   1.00 -363.84
