#################################
## Ch3_LynxHyp_modelling.R
## Modelling co-occurrence hypotheses for lynx

library(glmmTMB)
library(bbmle)
library(ggplot2)

devtools::install_github(repo = "florianhartig/DHARMa", subdir = "DHARMa", 
                         dependencies = T, build_vignettes = T)

library(DHARMa)
library(lme4)


setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

Occ <- read.csv("Algar60_speciesOccurrence.csv")
glimpse(Occ)
Occ$X <- NULL

#### Standardize and scale input variables
## covscale function
covscale <- function(covariate){
  centre <- (covariate - mean(covariate, na.rm = TRUE)) # where covariate is a vector of numeric input values for one covariate
  standardize <- centre/(2*sd(covariate, na.rm = TRUE)) # Divide centred value by 2 SD
}

Occ_sc <- cbind.data.frame(Occ[ ,1:17], lapply(Occ[ , 18:23], covscale)) ## Exclude Snow from standardizing 
Occ_sc$Snow <- Occ$Snow[match(Occ$Site_SD, Occ_sc$Site_SD)]

mean(Occ_sc$pOpen1500)
mean(Occ$pOpen1500)
sd(Occ_sc$pOpen1500)
sd(Occ$pOpen1500)

## Lumping Hare + Squirrel into 'Prey' category
Occ_sc$Prey <- rowSums(Occ_sc[ , c("Hare", "Squirrel")], na.rm=T)
table(Occ_sc$Prey) ## 241 single occurrences now, 2  occurrences of both in one day --> convert to ones
Occ_sc$Prey <- ifelse(Occ_sc$Prey > 0 , 1, 0)
table(Occ_sc$Prey) ## 243 occurrences

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
Lynx.tab <- ICtab(L0, L1, L1.season, L1.LD1750, L2, L2.season, L1.s.Add, L2.s.Add, L2.LD1750, L3, L3.season, L3.LD1750, L4, L4.season, L4.LD1750,
                  type = "AIC",
                  weights = TRUE,
                  delta = TRUE,
                  logLik = TRUE)
Lynx.tab


summary(L2.season)
summary(L2.s.Add)
summary(L1.s.Add)

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


## Simulate residuals with DHARMa
## For interaction model
res <- simulateResiduals(L2.season)

## For additive model
res.add2 <- simulateResiduals(L2.s.Add)
## residuals with glmmTMB
res.add2.TMB <- residuals(L2.s.Add)

## residuals with glmmTMB
res.glmmTMB <- residuals(L2.season)
length(res.glmmTMB)

plot(fitted(L2.season), resid(L2.season), xlab = "Fitted values", ylab = "Lynx*snow residuals")
plot(fitted(L2.s.Add), res.add2.TMB,  xlab = "Fitted values", ylab = "Lynx + snow residuals")

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


## For additive model
res.add2 <- simulateResiduals(L2.s.Add)
testUniformity(res.add2) ## Deviation significant still

res.Lynx.add <- residuals(L2.s.Add)
plot(fitted(L2.s.Add), res.add2.TMB,  xlab = "Fitted values", ylab = "Lynx + snow residuals")

plot(Occ_naomit$Coyote, res.Lynx.add, xlab = "Coyote occurrence", ylab = "Lynx+snow residuals") ## uneven spread of residuals -- should there be roughly equal points in all corners?
plot(Occ_naomit$LowCon1500, res.Lynx.add,xlab = "Lowland coniferous forest", ylab = "Lynx+snow residuals") ## variance is not constant about mean of LowCon (0) --> fewer points < 0 (accurate for LowCon values)
plot(Occ_naomit$UpCon1500, res.Lynx.add, xlab = "Upland coniferous forest", ylab = "Lynx+snow residuals")

plot(Occ_naomit$pOpen1500, res.Lynx.add, xlab = "Open forest", ylab = "Lynx+snow residuals")

plot(Occ_naomit$Snow, res.Lynx.add, xlab = "Snow presence", ylab = "Lynx+snow residuals")



### testing temporal autocorrelation


testData = createData(sampleSize = 100, family = poisson(), temporalAutocorrelation = 5)

fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson() )

simulationOutput <- simulateResiduals(fittedModel = fittedModel)

testTemporalAutocorrelation(simulationOutput = simulationOutput, time = testData$time)

### Plotting each site over time?


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


#### Graphing results: Lynx occurrence as a function of:
## Gillian's code for plotting glmms on binomial data
## Register function for binomial smoother
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
  
} # Use loess smoother for binomial distributions

## Need dataframe with NAs omitted to compare Occ data to predicted data from model
length(predict(L2.season)) ## 31964, vs 32395 in Occ
## Occ_naomit, if not run before:
Occ_naomit <- na.omit(Occ_sc) ## now same number of observations as residuals


## 1. Coyotes
pl1 <- ggplot(Occ_naomit) + geom_point(aes(x = Coyote , y = jitter(Lynx), colour = Site), size = 1)

pl1 <- pl1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) # remove background

pl1 <- pl1 + xlab("Probability of Coyote Occurrence") + ylab("Probability of Lynx Occurrence") + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text = element_text(size = 12))
pl1 <- pl1 + binomial_smooth(aes(x = Coyote, y = predict(L2.s.Add, type = "response"), colour = Site), size = 1, se = FALSE)

print(pl1)


### Plotting coefficients
Mod.coef <- as.data.frame(coef(summary(L2.season))[["cond"]])
Mod.coef
Mod.coef$Predictor <- c("Intercept", "Coyote", "Snow", "LowlandConifer", "UplandConifer", "OpenForest", "Coyote:Snow")
colnames(Mod.coef) <- c("Coefficient", "StdError", "zvalue", "Prob", "Predictor")


est2 <- ggplot(data = Mod.coef, aes(x = Predictor, y = Coefficient)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= Coefficient - StdError, ymax = Coefficient + StdError, width = 0.3)) + scale_x_discrete(limits=c("Coyote", "Snow", "LowlandConifer", "UplandConifer", "OpenForest", "Coyote:Snow")) + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + geom_hline(yintercept = 0) + scale_y_continuous(limits = c(-20, 20))
est2 <- est2 + ggtitle("Lynx") + theme(plot.title = element_text(colour = "black", hjust = 0.5, vjust= -3, size = 24))

est2

## Additive model
Mod.coef <- as.data.frame(coef(summary(L2.s.Add))[["cond"]])
Mod.coef
Mod.coef$Predictor <- c("Intercept","Coyote", "Snow", "LowlandConifer", "UplandConifer", "OpenForest")
colnames(Mod.coef) <- c("Coefficient", "StdError", "zvalue", "Prob", "Predictor")
Mod.coef

est3 <- ggplot(data = Mod.coef, aes(x = Predictor, y = Coefficient)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= Coefficient - StdError, ymax = Coefficient + StdError, width = 0.3)) + scale_x_discrete(limits=c("Coyote", "Snow", "LowlandConifer", "UplandConifer", "OpenForest")) + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + geom_hline(yintercept = 0) + scale_y_continuous(limits = c(-3, 5))
est3 <- est3 + ggtitle("Lynx") + theme(plot.title = element_text(colour = "black", hjust = 0.5, vjust= -3, size = 24))
est3


## Plotting wolves over time (for comparison)
time <- ggplot(data = Occ_sc, aes(x = Datep, y = Wolf, color = Site)) + geom_point(size = 2) + scale_x_date()
time
class(Occ_sc$Datep)

###### Plotting coefficients from habitat modelling ###
## Lynx
## Taken from top model output
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -6.9600     0.2560 -27.186  < 2e-16 ***
# LowCon        6.3169     1.9933   3.169  0.00153 ** 
# LowDecid     -0.8590     0.7579  -1.133  0.25702    
# Tamarack      3.0419     1.6934   1.796  0.07244 .  
# UpCon         1.9109     0.8424   2.269  0.02329 *  
# UpDecid       0.8449     0.7730   1.093  0.27442    
# pOpen        -2.8134     0.5986  -4.700  2.6e-06 ***

Predictor <- c( "LowCon", "LowDecid","Tamarack","UpCon", "UpDecid",  "OpenForest")
Coefficient <- c(6.3169, -0.8590, 3.0419, 1.9109, 0.8449, -2.8134)
StdError <- c(1.9933, 0.7579, 1.6934, 0.8424, 0.7730, 0.5986)
Mod.coef <- cbind.data.frame(Predictor, Coefficient, StdError)

est3 <- ggplot(data = Mod.coef, aes(x = Predictor, y = Coefficient)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= Coefficient - StdError, ymax = Coefficient + StdError, width = 0.3)) + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + geom_hline(yintercept = 0)
est3 <- est3 + ggtitle("Lynx") + theme(plot.title = element_text(colour = "black", hjust = 0.5, vjust= -3, size = 24))
est3
