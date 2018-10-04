## Ch3_CoyoteHyp_modelling.R
## Modelling co-occurrence hypotheses for Coyote

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

Occ_sc <- cbind.data.frame(Occ[ ,1:17], lapply(Occ[ , 18:23], covscale)) ## Exclude Snow from standardizing --> Essentially comparing summer (Snow == 0) to Winter (Snow == 1), so no need for scaling
Occ_sc$Snow <- Occ$Snow[match(Occ$Site_SD, Occ_sc$Site_SD)]

#Core model
Sys.time()
C0 <- glmmTMB(Coyote~ pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

## C1 = Intraguild competition/ facilitation
C1 <- glmmTMB(Coyote~ Wolf + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

## C1 with seasonal effects
C1.season <- glmmTMB(Coyote~ Wolf*Snow + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")


## C1 with LD
C1.LD1500 <- glmmTMB(Coyote~ Wolf*LD1500 + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

## C2 = Resource competition --> which could also be framed as intraguild comp./facilitation with another mesocarnivore
C2 <- glmmTMB(Coyote~Lynx + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

## C2 with seasonal effects
C2.season <- glmmTMB(Coyote~ Lynx*Snow + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

## C2 with LD
C2.LD1500 <- glmmTMB(Coyote~ Lynx*LD1500 + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

## C3 = Prey resources
C3 <- glmmTMB(Coyote~ Prey + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

## C3 with seasonal effects
C3.season <- glmmTMB(Coyote~ Prey*Snow + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

## C3 with LD
C3.LD1500 <- glmmTMB(Coyote~ Prey*LD1500 + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

Sys.time()

### AIC model selection
Coyote.tab <- ICtab(C0, C1, C1.season, C1.LD1500, C2, C2.season, C2.LD1500, C3, C3.season, C3.LD1500, 
                  type = "AIC",
                  weights = TRUE,
                  delta = TRUE,
                  logLik = TRUE)
Coyote.tab

summary(C2.season)
