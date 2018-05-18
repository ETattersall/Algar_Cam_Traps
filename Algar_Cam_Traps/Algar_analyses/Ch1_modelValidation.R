########################################
## Ch1_modelValidation.R
## Model validation using DHARMa
## DHARMa only recently added support for glmmTMB in the develoment package, which does not load in Rmarkdown
## May 17, 2018
########################################

devtools::install_github(repo = "florianhartig/DHARMa", subdir = "DHARMa", 
                         dependencies = T, build_vignettes = T)
library(glmmTMB)

det <- read.csv("Seismic_nov2015-apr2018.csv")

##### Top WTD model ( model 30, formed and run with MuMIn's dredge) ####
WTD.30 <- glmmTMB(WTDeer~LD750_sc + VegHt_sc + low2000_sc + Treatment + pSnow_sc + (1|Site) + (1|Month), data = det, zi=~ActiveDays_sc, family = nbinom1)
summary(WTD.30)

## Simulate residuals with DHARMa
res <- simulateResiduals(WTD.30)

## Qualtitative check with plots
plot(res) ## qq plot shows deviation from expected, residuals vs. predicted with quantile lines --> ideally lines are straight, horizontal, and at 0.25, 0.5, 0.75. BUT deviations can be expected even in good models

## Plot against other predictors --> residuals and predictor do not have same length
plotResiduals(det$LD750_sc, res$scaledResiduals)

## Formal goodness of fit tests
#Kolmorgorov-Smirnov test for uniformity
testUniformity(res) # D = 0.024, max. absolute difference between sim. and observed, p-value = prob. of finding D if observed and sim. are same
testZeroInflation(res) #compares observed number of zeroes to zeros expected in simulations
testTemporalAutocorrelation(res) #Durbin-Watson test for uncorrelated residuals --> no patterns in residuals over time
testSpatialAutocorrelation(res) #Moran's I test for spatial autocorrelation, based on random coordinate values and tests for a null of no spatial autocorrelation

