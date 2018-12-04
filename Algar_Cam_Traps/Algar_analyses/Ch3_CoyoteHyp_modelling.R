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

## Lumping Hare + Squirrel into 'Prey' category
Occ_sc$Prey.Coyotes <- rowSums(Occ_sc[ , c("Hare", "Squirrel", "WTDeer")], na.rm=T)
table(Occ_sc$Prey.Coyotes) ## 241 single occurrences now, 2  occurrences of both in one day --> convert to ones
Occ_sc$Prey.Coyotes <- ifelse(Occ_sc$Prey.Coyotes > 0 , 1, 0)
table(Occ_sc$Prey.Coyotes) ## 739 occurrences

#Core model
Sys.time()
C0 <- glmmTMB(Coyote~ pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

## C1 = Intraguild competition/ facilitation
C1 <- glmmTMB(Coyote~ Wolf + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

## C1 with seasonal effects
C1.season <- glmmTMB(Coyote~ Wolf*Snow + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

C1.s.Add <- glmmTMB(Coyote~ Wolf + Snow + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")


## C1 with LD
C1.LD1500 <- glmmTMB(Coyote~ Wolf*LD1500 + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

## C2 = Resource competition --> which could also be framed as intraguild comp./facilitation with another mesocarnivore
C2 <- glmmTMB(Coyote~Lynx + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

## C2 with seasonal effects
C2.season <- glmmTMB(Coyote~ Lynx*Snow + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

## C2 seasonal additive
C2.s.Add <- glmmTMB(Coyote~ Lynx + Snow + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")


## C2 with LD
C2.LD1500 <- glmmTMB(Coyote~ Lynx*LD1500 + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

## C3 = Prey resources
C3 <- glmmTMB(Coyote~ Prey.Coyotes + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

## C3 with seasonal effects
C3.season <- glmmTMB(Coyote~ Prey.Coyotes*Snow + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

C3.s.Add <- glmmTMB(Coyote~ Prey.Coyotes + Snow + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

## C3 with LD
C3.LD1500 <- glmmTMB(Coyote~ Prey.Coyotes*LD1500 + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

Sys.time()

### AIC model selection
Coyote.tab <- ICtab(C0, C1, C1.season, C1.s.Add, C1.LD1500, C2, C2.season,C2.s.Add, C2.LD1500, C3, C3.season, C3.s.Add, C3.LD1500, 
                  type = "AIC",
                  weights = TRUE,
                  delta = TRUE,
                  logLik = TRUE)
Coyote.tab

summary(C2.s.Add)
summary(C1.s.Add)
summary(C3.s.Add)
summary(C2.LD1500)
summary(C2.Snow.LD)



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

## simulate residuals for additive model (take most parsimonious)
res.add.Coyote <- simulateResiduals(C2.s.Add)
testUniformity(res.add.Coyote)

res.Coyote.TMB <- residuals(C2.s.Add)
plot(fitted(C2.s.Add), res.Coyote.TMB, xlab = "Fitted values", ylab = "Coyote+snow residuals")

plot(Occ_naomit$Coyote, res.Coyote.TMB, xlab = "Lynx occurrence", ylab = "Coyote+snow residuals")
plot(Occ_naomit$pOpen1750, res.Coyote.TMB, xlab = "Open forest", ylab = "Coyote+snow residuals")

plot(Occ_naomit$Snow, res.Coyote.TMB, xlab = "Snow presence", ylab = "Coyote+snow residuals")


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




#### Graphing results: Coyote occurrence as a function of lynx and season
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
pl1 <- pl1 + binomial_smooth(aes(x = Coyote, y = predict(C2.s.Add, type = "response"), colour = Site), size = 1, se = FALSE)

print(pl1)


### Plotting coefficients

## Interaction model
Mod.coef <- as.data.frame(coef(summary(C2.season))[["cond"]])
Mod.coef
Mod.coef$Predictor <- c("Intercept", "Lynx", "Snow", "OpenForest", "Lynx:Snow")
colnames(Mod.coef) <- c("Coefficient", "StdError", "zvalue", "Prob", "Predictor")
Mod.coef

est2 <- ggplot(data = Mod.coef, aes(x = Predictor, y = Coefficient)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= Coefficient - StdError, ymax = Coefficient + StdError, width = 0.3)) + scale_x_discrete(limits=c("Lynx", "Snow", "OpenForest", "Lynx:Snow")) + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + geom_hline(yintercept = 0) + scale_y_continuous(limits = c(-20, 20))
est2 <- est2 + ggtitle("Coyote") + theme(plot.title = element_text(colour = "black", hjust = 0.5, vjust= -3, size = 24))
est2

## Additive model
Mod.coef <- as.data.frame(coef(summary(C2.s.Add))[["cond"]])
Mod.coef
Mod.coef$Predictor <- c("Intercept", "Lynx", "Snow", "OpenForest")
colnames(Mod.coef) <- c("Coefficient", "StdError", "zvalue", "Prob", "Predictor")
Mod.coef

est3 <- ggplot(data = Mod.coef, aes(x = Predictor, y = Coefficient)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= Coefficient - StdError, ymax = Coefficient + StdError, width = 0.3)) + scale_x_discrete(limits=c("Lynx", "Snow", "OpenForest")) + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + geom_hline(yintercept = 0) + scale_y_continuous(limits = c(-3, 3))
est3 <- est3 + ggtitle("Coyote") + theme(plot.title = element_text(colour = "black", hjust = 0.5, vjust= -3, size = 24))
est3


### Exploring a fully additive model including both Snow and LD 
## With both additional covariates
C2.Snow.LD <- glmmTMB(Coyote~ Lynx + Snow + LD1500 + pOpen1750 + (1|Site), data = Occ_sc, family = "binomial")

Coyote.tab <- ICtab(C0, C1, C1.season, C1.LD1500, C2, C2.season,C2.s.Add, C2.Snow.LD, C2.LD1500, C3, C3.season, C3.LD1500, 
                    type = "AIC",
                    weights = TRUE,
                    delta = TRUE,
                    logLik = TRUE)
Coyote.tab

## Increases model fit relative to other two dramatically
summary(C2.Snow.LD)



## residuals with glmmTMB
res.glmmTMB <- residuals(C2.Snow.LD)
length(res.glmmTMB)

plot(fitted(C2.Snow.LD), resid(C2.Snow.LD), xlab = "Fitted values", ylab = "Coyote + snow + LD residuals")



## base R plot of residuals
Occ_naomit <- na.omit(Occ_sc) ## now same number of observations as residuals
plot(Occ_naomit$Coyote, res.glmmTMB, xlab = "Lynx occurrence", ylab = "Coyote + snow + LD residuals")
plot(Occ_naomit$pOpen1750, res.glmmTMB, xlab = "Open forest", ylab = "Coyote + snow + LD residuals")

plot(Occ_naomit$Snow, res.glmmTMB, xlab = "Snow presence", ylab = "Coyote + snow + LD residuals")




## Simulate residuals with DHARMa
library(DHARMa)
res <- simulateResiduals(C2.Snow.LD)

## Formal goodness of fit tests
#Kolmorgorov-Smirnov test for uniformity
testUniformity(res) ## deviation still significant

###### Plotting coefficients from habitat modelling ###
## Coyote
   
## (Intercept)  -7.6494     0.3716
## LowCon        2.5966     2.8286    
## LowDecid     -0.8923     0.8569    
## Tamarack      0.7349     2.3687    
## UpCon        -0.8228     1.2509   
## UpDecid      -1.4327     1.1690    
## pOpen        -4.5798     1.0188

Predictor <- c( "LowCon", "LowDecid","Tamarack","UpCon", "UpDecid",  "OpenForest")
Coefficient <- c(2.5966, -0.8923, 0.7349, -0.8228, -1.4327, -4.5798)
StdError <- c(2.8286, 0.8569, 2.3687, 1.2509, 1.1690, 1.0188)
Mod.coef <- cbind.data.frame(Predictor, Coefficient, StdError)

est3 <- ggplot(data = Mod.coef, aes(x = Predictor, y = Coefficient)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= Coefficient - StdError, ymax = Coefficient + StdError, width = 0.3)) + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + geom_hline(yintercept = 0)
est3 <- est3 + ggtitle("Coyote") + theme(plot.title = element_text(colour = "black", hjust = 0.5, vjust= -3, size = 24))
est3
