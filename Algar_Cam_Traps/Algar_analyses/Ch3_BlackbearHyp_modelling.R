## Ch3_BlackbearHyp_modelling.R
## Modelling co-occurrence hypotheses for Coyote

library(glmmTMB)
library(bbmle)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

Bears <- read.csv("Algar60_speciesOccurrence_SUMMER.csv")
Bears$X <- NULL

#### Standardize and scale input variables
## covscale function
covscale <- function(covariate){
  centre <- (covariate - mean(covariate, na.rm = TRUE)) # where covariate is a vector of numeric input values for one covariate
  standardize <- centre/(2*sd(covariate, na.rm = TRUE)) # Divide centred value by 2 SD
}

Bears_sc <- cbind.data.frame(Bears[ ,1:17], lapply(Bears[ , 18:19], covscale)) ## Exclude Snow from standardizing --> Essentially comparing summer (Snow == 0) to Winter (Snow == 1), so no need for scaling
Bears_sc$Snow <- Bears$Snow[match(Bears$Site_SD, Bears_sc$Site_SD)]

## Lumping Caribou, Moose, Deer into Prey category (also test one with Prey separate)
Bears_sc$Prey <- rowSums(Bears_sc[ , c(5,7,8)], na.rm = T)
table(Bears_sc$Prey) ## 1 case of 2 occurrences in one day
Bears_sc$Prey <- ifelse(Bears_sc$Prey>0,1,0)
table(Bears_sc$Prey) ## 519 occurrences of Prey

##B0 Core Model
B0 <- glmmTMB(Blackbear~pOpen250 + (1|Site), data = Bears_sc, zi~1, family = "binomial")

## B1 Competition/ Facilitation hypotheses
B1 <- glmmTMB(Blackbear~Wolf + pOpen250 + (1|Site), data = Bears_sc, zi~1, family = "binomial")

## B1 with LD
B1.LD1500 <- glmmTMB(Blackbear~Wolf*LD1500 + pOpen250 + (1|Site), data = Bears_sc, zi~1, family = "binomial")

## B2 Predation
B2 <- glmmTMB(Blackbear~Prey + pOpen250 + (1|Site), data = Bears_sc, zi~1, family = "binomial")

## B2 with LD
B2.LD1500 <- glmmTMB(Blackbear~Prey*LD1500 + pOpen250 + (1|Site), data = Bears_sc, zi~1, family = "binomial")

##B3 Predation (separated)
B3 <- glmmTMB(Blackbear~WTDeer + Moose + Caribou + pOpen250 + (1|Site), data = Bears_sc, zi~1, family = "binomial")

## B3 with LD??
