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

mean(Occ_sc$pOpen1500)
mean(Occ$pOpen1500)
sd(Occ_sc$pOpen1500)
sd(Occ$pOpen1500)

#Core model
Sys.time()
L0 <- glmmTMB(Lynx~ LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, family = "binomial")

## L1 = Intraguild competition/ facilitation
L1 <- glmmTMB(Lynx~ Wolf + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc,  family = "binomial")

## L1 with seasonal effects
L1.season <- glmmTMB(Lynx~ Wolf*Snow + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, family = "binomial")


## L1 with LD
L1.LD1750 <- glmmTMB(Lynx~ Wolf*LD1750 + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, family = "binomial")

## L2 = Resource competition --> which could also be framed as intraguild comp./facilitation with another mesocarnivore
L2 <- glmmTMB(Lynx~Coyote + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, family = "binomial")

## L2 with seasonal effects
L2.season <- glmmTMB(Lynx~ Coyote*Snow + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, family = "binomial")

## L2 with LD
L2.LD1750 <- glmmTMB(Lynx~ Coyote*LD1750 + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, family = "binomial")

## L3 = Prey resources
L3 <- glmmTMB(Lynx~ Prey*LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, family = "binomial")

## L3 with seasonal effects
L3.season <- glmmTMB(Lynx~ Prey*Snow + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, family = "binomial")

## L3 with LD
L3.LD1750 <- glmmTMB(Lynx~ Prey*LD1750 + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, family = "binomial")

## L4 = Hare only
L4 <- glmmTMB(Lynx~ Hare + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, family = "binomial")

## L4 with seasonal effects
L4.season <- glmmTMB(Lynx~ Hare*Snow + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, family = "binomial")

## L4 with LD
L4.LD1750 <- glmmTMB(Lynx~ Hare*LD1750 + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, family = "binomial")
Sys.time()

### Additive models for L2.season and L1.season
L1.s.Add <- glmmTMB(Lynx~ Wolf + Snow + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, family = "binomial")

L2.s.Add <- glmmTMB(Lynx~ Coyote + Snow + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, family = "binomial")


### AIC model selection
Lynx.tab <- ICtab(L0, L1, L1.season, L1.LD1750, L2, L2.season, L2.LD1750, L3, L3.season, L3.LD1750, L4, L4.season, L4.LD1750, L1.s.Add, L2.s.Add,
                  type = "AIC",
                  weights = TRUE,
                  delta = TRUE,
                  logLik = TRUE)
Lynx.tab


summary(L1.season)
summary(L2.s.Add)

### Hare + Squirrel models did not converge --> 1) Try with JUST hare 2) Lump into Prey species
L3.Hare <- glmmTMB(Lynx~ Hare + Snow + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_sc, zi = ~1, family = "binomial")
summary(L3.Hare)


## Lumping Hare + Squirrel into 'Prey' category
Occ_sc$Prey <- rowSums(Occ_sc[ , c("Hare", "Squirrel")], na.rm=T)
table(Occ_sc$Prey) ## 241 single occurrences now, 2  occurrences of both in one day --> convert to ones
Occ_sc$Prey <- ifelse(Occ_sc$Prey > 0 , 1, 0)
table(Occ_sc$Prey) ## 243 occurrences

#### Examining random effects of top model
## Best Linear Unbiased Predictors are conditional estimates of the variance of each it from the mean effect of site (conditional on the maximum likelihood estimate of the mean)
L2.BLUP <- ranef(L2.season)
class(L2.BLUP)
L2.ranef <- L2.BLUP$cond$Site


#### Model validation with dHARMa
devtools::install_github(repo = "florianhartig/DHARMa", subdir = "DHARMa", 
                         dependencies = T, build_vignettes = T)

library(DHARMa)


## Simulate residuals with DHARMa
res <- simulateResiduals(L2.season)

## residuals with glmmTMB
res.glmmTMB <- residuals(L2.season)
length(res.glmmTMB)

plot(fitted(L2.season), resid(L2.season), xlab = "Fitted values", ylab = "Lynx*snow residuals")

## Qualtitative check with plots
plot(res) ## qq plot shows deviation from expected, residuals vs. predicted with quantile lines --> ideally lines are straight, horizontal, and at 0.25, 0.5, 0.75. BUT deviations can be expected even in good models



## Plot against other predictors
plotResiduals(res$observedResponse, res$scaledResiduals, asFactor = T) ## differing length
length(res$scaledResiduals)
summary(res$observedResponse)
length(Occ_sc$Coyote)

## base R plot of residuals
Occ_naomit <- na.omit(Occ_sc) ## now same number of observations as residuals
plot(Occ_naomit$Coyote, res.glmmTMB, xlab = "Coyote occurrence", ylab = "Lynx*snow residuals") ## uneven spread of residuals -- should there be roughly equal points in all corners?
plot(Occ_naomit$LowCon1500, res.glmmTMB,xlab = "Lowland coniferous forest", ylab = "Lynx*snow residuals") ## variance is not constant about mean of LowCon (0) --> fewer points < 0 (accurate for LowCon values)
plot(Occ_naomit$UpCon1500, res.glmmTMB, xlab = "Upland coniferous forest", ylab = "Lynx*snow residuals")

plot(Occ_naomit$pOpen1500, res.glmmTMB, xlab = "Open forest", ylab = "Lynx*snow residuals")

plot(Occ_naomit$Snow, res.glmmTMB, xlab = "Snow presence", ylab = "Lynx*snow residuals")

recalc.Occ <- recalculateResiduals(res, group = (Occ_sc$Site), aggregateBy = sum)


## Formal goodness of fit tests
#Kolmorgorov-Smirnov test for uniformity
testUniformity(res) # D = 0.009 , max. absolute difference between sim. and observed, p-value = prob. of finding D if observed and sim. are same
## p = 0.0109 --> observed and simulated are DIFFERENT --> Lynx model is no good



### testing temporal autocorrelation
library(lme4)

testData = createData(sampleSize = 100, family = poisson(), temporalAutocorrelation = 5)

fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson() )

simulationOutput <- simulateResiduals(fittedModel = fittedModel)

testTemporalAutocorrelation(simulationOutput = simulationOutput, time = testData$time)

### Plotting each site over time?
library(ggplot2)

Occ_sc$Datep <- Occ$Datep[match(Occ_sc$StudyDay, Occ$StudyDay)]
class(Occ_sc$Datep)

Occ_sc$Datep <- as.Date(Occ_sc$Datep)

time <- ggplot(data = Occ_sc, aes(x = Datep, y = Lynx, color = Site)) + geom_point(size = 2) + scale_x_date()
time
## too much to show on one --> break into time chunks of 3 months to look for spatiotemporal correlation (determine temporal scales)
time + scale_x_date(limits = as.Date(c("2015-11-01", "2016-02-01"))) ## Algar 16 - 3 days apart, Algar06 - 8 days apart
time + scale_x_date(limits = as.Date(c("2016-02-01", "2016-05-01"))) ## Algar06 - 7 days apart
time + scale_x_date(limits = as.Date(c("2016-12-01", "2017-03-01"))) ## Algar34 - 8 days apart
time + scale_x_date(limits = as.Date(c("2017-09-01", "2017-12-01")))

### 10 day occasion length will aggregate some of these occurrences, but difficult to tell if these are same individual or not...

## Instead of increasing occasion length --> removing sites where both species are absent?
## Need a species by station matrix
spst <- read.csv("Station_data/detectionsByStation.csv")

spst <- spst[which(spst$Canis.latrans > 0 & spst$Lynx.canadensis > 0 & spst$Ursus.americanus > 0 & spst$Canis.lupus > 0), ]

Pres <- spst$X
Pres

Occ_pres <- Occ_sc %>% filter(Site == "Algar02" | Site == "Algar03"| Site == "Algar06" | Site == "Algar07" | Site == "Algar08" | Site == "Algar10" | Site == "Algar13" | Site == "Algar14" | Site == "Algar16" | Site == "Algar17" | Site == "Algar18" | Site == "Algar19" | Site == "Algar22" | Site == "Algar27" | Site == "Algar34" | Site == "Algar47")

## 10 916 obs. vs 32 395 obs.

## Test top Lynx model
L <- glmmTMB(Lynx~ Coyote + LowCon1500 + UpCon1500 + pOpen1500 + (1|Site), data = Occ_pres, family = "binomial")

summary(L.pres) ## Larger standard errors in interaction model, AND doesn't adequately sample whole Algar area (focused heavily on SE corner in upland area)

time.Wolf <- ggplot(data = Occ_sc, aes(x = Datep, y = Wolf, color = Site)) + geom_point(size = 2) + scale_x_date()
time.Wolf
time.Wolf + scale_x_date(limits = as.Date(c("2015-11-05", "2016-01-01"))) ## Algar 06- 1 day apart
time.Wolf + scale_x_date(limits = as.Date(c("2016-01-01", "2016-04-01")))
time + scale_x_date(limits = as.Date(c("2017-09-01", "2017-12-01")))


