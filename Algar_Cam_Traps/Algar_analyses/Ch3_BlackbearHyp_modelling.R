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
B0 <- glmmTMB(Blackbear~pOpen250 + (1|Site), data = Bears_sc, family = "binomial")

## B1 Competition/ Facilitation hypotheses
B1 <- glmmTMB(Blackbear~Wolf + pOpen250 + (1|Site), data = Bears_sc, family = "binomial")

## B1 with LD
B1.LD1500 <- glmmTMB(Blackbear~Wolf*LD1500 + pOpen250 + (1|Site), data = Bears_sc, family = "binomial")

## B2 Predation
B2 <- glmmTMB(Blackbear~Prey + pOpen250 + (1|Site), data = Bears_sc,  family = "binomial")

## B2 with LD
B2.LD1500 <- glmmTMB(Blackbear~Prey*LD1500 + pOpen250 + (1|Site), data = Bears_sc, family = "binomial")

##B3 Predation (separated)
B3 <- glmmTMB(Blackbear~WTDeer + Moose + Caribou + pOpen250 + (1|Site), data = Bears_sc, family = "binomial")

## B3 with LD??


#### Model validation with dHARMa
devtools::install_github(repo = "florianhartig/DHARMa", subdir = "DHARMa", 
                         dependencies = T, build_vignettes = T)

library(DHARMa)


## Simulate residuals with DHARMa
res <- simulateResiduals(B1)
res.LD <- simulateResiduals(B1.LD1500)

## Qualtitative check with plots
plot(res) ## qq plot shows deviation from expected, residuals vs. predicted with quantile lines --> ideally lines are straight, horizontal, and at 0.25, 0.5, 0.75. BUT deviations can be expected even in good models

## Plot against other predictors
plotResiduals() ## objects of different lengths...

recalc.Occ <- recalculateResiduals(res, group = (Occ_sc$Site), aggregateBy = sum)

## residuals with glmmTMB
res.glmmTMB <- residuals(B1)
length(res.glmmTMB)

plot(fitted(B1), resid(B1), xlab = "Fitted values", ylab = "Bear residuals")

## base R plot of residuals
Bears_naomit <- na.omit(Bears_sc) ## now same number of observations as residuals
plot(Bears_naomit$Coyote, res.glmmTMB, xlab = "Wolf Occurrence", ylab = "Bear residuals") ## uneven spread of residuals -- should there be roughly equal points in all corners?


plot(Bears_naomit$pOpen250, res.glmmTMB, xlab = "Open forest", ylab = "Bear residuals")


recalc.Bears <- recalculateResiduals(res, group = (Bears_sc$Site), aggregateBy = sum)


## Formal goodness of fit tests
#Kolmorgorov-Smirnov test for uniformity
testUniformity(res) # D = 0.0088 , max. absolute difference between sim. and observed, p-value = prob. of finding D if observed and sim. are same
## p = 0.229 --> simulated values are not significantly different from observed


## Checking temporal autocorrelation
library(ggplot2)

Bears_sc$Datep <- Bears$Datep[match(Bears_sc$StudyDay, Bears$StudyDay)]
class(Bears_sc$Datep)

Bears_sc$Datep <- as.Date(Bears_sc$Datep)

time <- ggplot(data = Bears_sc, aes(x = Datep, y = Blackbear, color = Site)) + geom_point() + scale_x_date()
time

## Difficult to see relationships when plotting all together -- check a few individually?
