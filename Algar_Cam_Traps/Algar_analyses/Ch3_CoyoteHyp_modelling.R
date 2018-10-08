## Ch3_CoyoteHyp_modelling.R
## Modelling co-occurrence hypotheses for Coyote

library(glmmTMB)
library(bbmle)
library(dplyr)

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

### model validation

library(DHARMa)


## Simulate residuals with DHARMa
res <- simulateResiduals(C2.season)

## residuals with glmmTMB
res.glmmTMB <- residuals(C2.season)
length(res.glmmTMB)

plot(fitted(C2.season), resid(C2.season), xlab = "Fitted values", ylab = "Coyote*snow residuals")

## Qualtitative check with plots
plot(res) ## qq plot shows deviation from expected, residuals vs. predicted with quantile lines --> ideally lines are straight, horizontal, and at 0.25, 0.5, 0.75. BUT deviations can be expected even in good models

## base R plot of residuals
Occ_naomit <- na.omit(Occ_sc) ## now same number of observations as residuals
plot(Occ_naomit$Coyote, res.glmmTMB, xlab = "Lynx occurrence", ylab = "Coyote*snow residuals")
plot(Occ_naomit$pOpen1750, res.glmmTMB, xlab = "Open forest", ylab = "Coyote*snow residuals")

plot(Occ_naomit$Snow, res.glmmTMB, xlab = "Snow presence", ylab = "Coyote*snow residuals")

## Formal goodness of fit tests
#Kolmorgorov-Smirnov test for uniformity
testUniformity(res) # D = 0.00897 , max. absolute difference between sim. and observed, p-value = prob. of finding D if observed and sim. are same. p = 0.0116

### Plotting each site over time?
library(ggplot2)

Occ_sc$Datep <- Occ$Datep[match(Occ_sc$StudyDay, Occ$StudyDay)]
class(Occ_sc$Datep)

Occ_sc$Datep <- as.Date(Occ_sc$Datep)

time <- ggplot(data = Occ_sc, aes(x = Datep, y = Coyote, color = Site)) + geom_point() + scale_x_date()
time


### break into time chunks of 3 months to look for spatiotemporal correlation (determine temporal scales)

time + scale_x_date(limits = as.Date(c("2015-11-01", "2016-02-01"))) ## Algar 17 - 2 days apart, 18 days, 7 days, 11 days, 5 days,
time + scale_x_date(limits = as.Date(c("2016-02-01", "2016-05-01"))) ## 
time + scale_x_date(limits = as.Date(c("2016-12-01", "2017-03-01"))) ## 
time + scale_x_date(limits = as.Date(c("2017-07-01", "2017-09-01"))) ## Algar17 - constantly between Aug.1 and Aug.16 ish


## Algar 17 is clearly a problem site -- how many occurrences do I have if I toss that site?
Occ.no17 <- Occ_sc %>% filter(Site != "Algar17")

sum(Occ_sc$Coyote) #131
sum(Occ.no17$Coyote) #74 --> removes 57 detections

## How does top model compare?
Cno17 <- glmmTMB(Coyote~ Lynx*Snow + pOpen1750 + (1|Site), data = Occ.no17, family = "binomial")
summary(Cno17) ## Estimates are just as poor
