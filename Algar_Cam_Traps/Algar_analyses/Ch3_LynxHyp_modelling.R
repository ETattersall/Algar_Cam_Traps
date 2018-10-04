#################################
## Ch3_LynxHyp_modelling.R
## Modelling co-occurrence hypotheses for lynx

library(glmmTMB)
library(bbmle)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

Occ <- read.csv("Algar60_speciesOccurrence.csv")
Occ$X <- NULL

#### Standardize and scale input variables
## covscale function
covscale <- function(covariate){
  centre <- (covariate - mean(covariate, na.rm = TRUE)) # where covariate is a vector of numeric input values for one covariate
  standardize <- centre/(2*sd(covariate, na.rm = TRUE)) # Divide centred value by 2 SD
}

Occ_sc <- cbind.data.frame(Occ[ ,1:17], lapply(Occ[ , 18:23], covscale)) ## Exclude Snow from standardizing ---> COME BACK TO THIS
Occ_sc$Snow <- Occ$Snow[match(Occ$Site_SD, Occ_sc$Site_SD)]

#Core model
Sys.time()
L0 <- glmmTMB(Lynx~ LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, zi = ~1, family = "binomial")

## L1 = Intraguild competition/ facilitation
L1 <- glmmTMB(Lynx~ Wolf + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, zi = ~1, family = "binomial")

## L1 with seasonal effects
L1.season <- glmmTMB(Lynx~ Wolf + Snow + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, zi = ~1, family = "binomial")


## L1 with LD
L1.LD1750 <- glmmTMB(Lynx~ Wolf + LD175 + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, zi = ~1, family = "binomial")

## L2 = Resource competition --> which could also be framed as intraguild comp./facilitation with another mesocarnivore
L2 <- glmmTMB(Lynx~Coyote + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, zi = ~1, family = "binomial")

## L2 with seasonal effects
L2.season <- glmmTMB(Lynx~ Coyote + Snow + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, zi = ~1, family = "binomial")

## L2 with LD
L2.LD1750 <- glmmTMB(Lynx~ Coyote + LD175 + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, zi = ~1, family = "binomial")

## L3 = Prey resources
L3 <- glmmTMB(Lynx~ Prey + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, zi = ~1, family = "binomial")

## L3 with seasonal effects
L3.season <- glmmTMB(Lynx~ Prey + Snow + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, zi = ~1, family = "binomial")

## L3 with LD
L3.LD1750 <- glmmTMB(Lynx~ Prey + LD1750 + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, zi = ~1, family = "binomial")

## L4 = Hare only
L4 <- glmmTMB(Lynx~ Hare + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, zi = ~1, family = "binomial")

## L4 with seasonal effects
L4.season <- glmmTMB(Lynx~ Hare + Snow + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, zi = ~1, family = "binomial")

## L4 with LD
L4.LD1750 <- glmmTMB(Lynx~ Hare + LD1750 + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, zi = ~1, family = "binomial")
Sys.time()

### AIC model selection
Lynx.tab <- ICtab(L0, L1, L1.season, L1.LD1750, L2, L2.season, L2.LD1750, L3, L3.season, L3.LD1750, L4, L4.season, L4.LD1750, 
                  type = "AIC",
                  weights = TRUE,
                  delta = TRUE,
                  logLik = TRUE)
Lynx.tab

### Hare + Squirrel models did not converge --> 1) Try with JUST hare 2) Lump into Prey species
L3.Hare <- glmmTMB(Lynx~ Hare + Snow + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, zi = ~1, family = "binomial")
summary(L3.Hare)


## Lumping Hare + Squirrel into 'Prey' category
Occ_sc$Prey <- rowSums(Occ_sc[ , c("Hare", "Squirrel")], na.rm=T)
table(Occ_sc$Prey) ## 241 single occurrences now, 2  occurrences of both in one day --> convert to ones
Occ_sc$Prey <- ifelse(Occ_sc$Prey > 0 , 1, 0)
table(Occ_sc$Prey) ## 243 occurrences

## 
