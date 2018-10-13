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

## B1 with LD, additive
B1.addLD1500 <- glmmTMB(Blackbear~Wolf + LD1500 + pOpen250 + (1|Site), data = Bears_sc, family = "binomial")

## B1 with LD
B1.LD1500 <- glmmTMB(Blackbear~Wolf*LD1500 + pOpen250 + (1|Site), data = Bears_sc, family = "binomial")

## B2 Predation
B2 <- glmmTMB(Blackbear~Prey + pOpen250 + (1|Site), data = Bears_sc,  family = "binomial")

## B2 with LD
B2.LD1500 <- glmmTMB(Blackbear~Prey*LD1500 + pOpen250 + (1|Site), data = Bears_sc, family = "binomial")

##B3 Predation (separated)
B3 <- glmmTMB(Blackbear~WTDeer + Moose + Caribou + pOpen250 + (1|Site), data = Bears_sc, family = "binomial")

## B3 with LD??

Bear.tab <- ICtab(B0, B1, B1.LD1500, B1.addLD1500, B2, B2.LD1500, B3,
                  type = "AIC",
                  weights = TRUE,
                  delta = TRUE,
                  logLik = TRUE)

Bear.tab ## B1 and B1.LD1500 are equivalent, B1 is more parsimonious. Including additive model improves fit

summary(B1.addLD1500)

#### Model validation with dHARMa
devtools::install_github(repo = "florianhartig/DHARMa", subdir = "DHARMa", 
                         dependencies = T, build_vignettes = T)

library(DHARMa)


## Simulate residuals with DHARMa
res <- simulateResiduals(B1)
res.LD <- simulateResiduals(B1.LD1500)
res.add <- simulateResiduals(B1.addLD1500)

## Qualtitative check with plots
plot(res) ## qq plot shows deviation from expected, residuals vs. predicted with quantile lines --> ideally lines are straight, horizontal, and at 0.25, 0.5, 0.75. BUT deviations can be expected even in good models

## Plot against other predictors
plotResiduals() ## objects of different lengths...

recalc.Occ <- recalculateResiduals(res, group = (Occ_sc$Site), aggregateBy = sum)

## residuals with glmmTMB
res.glmmTMB <- residuals(B1)
length(res.glmmTMB)
res.addbear.TMB <- residuals(B1.addLD1500)

plot(fitted(B1), resid(B1), xlab = "Fitted values", ylab = "Bear residuals")
plot(fitted(B1.addLD1500), res.addbear.TMB, xlab = "Fitted values", ylab = "Bear residuals")


## base R plot of residuals
Bears_naomit <- na.omit(Bears_sc) ## now same number of observations as residuals
plot(Bears_naomit$Wolf, res.addbear.TMB, xlab = "Wolf Occurrence", ylab = "Bear residuals") ## uneven spread of residuals -- should there be roughly equal points in all corners?


plot(Bears_naomit$pOpen250, res.addbear.TMB, xlab = "Open forest", ylab = "Bear residuals")

plot(Bears_naomit$LD1500, res.addbear.TMB, xlab = "Linear Density", ylab = "Bear residuals")

recalc.Bears <- recalculateResiduals(res, group = (Bears_sc$Site), aggregateBy = sum)


## Formal goodness of fit tests
#Kolmorgorov-Smirnov test for uniformity
testUniformity(res) # D = 0.0088 , max. absolute difference between sim. and observed, p-value = prob. of finding D if observed and sim. are same
## p = 0.229 --> simulated values are not significantly different from observed

testUniformity(res.LD)

testUniformity(res.add)

plot(fitted(B1.LD1500), resid(B1.LD1500), xlab = "Fitted values", ylab = "Bear*LD residuals")


## Checking temporal autocorrelation
library(ggplot2)

Bears_sc$Datep <- Bears$Datep[match(Bears_sc$StudyDay, Bears$StudyDay)]
class(Bears_sc$Datep)

Bears_sc$Datep <- as.Date(Bears_sc$Datep)

time <- ggplot(data = Bears_sc, aes(x = Datep, y = Blackbear, color = Site)) + geom_point() + scale_x_date()
time

## No clear temporal autocorrelation when plotted together. Spatial?

## Graphing results: effect of wolf co-occurrence, open forest, and linear density

ggplot(data = Bears, aes(x=Wolf, y = Blackbear)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=TRUE) + theme_classic()

ggplot(data = Bears, aes(x=pOpen250, y = Blackbear)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=TRUE) + theme_classic()

## Not sure if above trend lines account for random effects...
ggplot(data = Bears, aes(x=Wolf, y = Blackbear)) + geom_point(size = 2) + geom_line(aes(y = predict(B1, type = "response"), group = Site, col = Site)) + theme_classic()
pred.B1 <- predict(B1, type = "response")

## Gillian's code for plotting glmms on binomial data
## Register function for binomial smoother
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
  
} # Use loess smoother for binomial distributions


### Black bear occurrence as a function of Wolf Occurrence
Bearplot <- ggplot(Bears) + geom_point(aes(x = Wolf, y = jitter(Blackbear), colour = Site), size = 1)

Bearplot <- Bearplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) # remove background

Bearplot <- Bearplot + xlab("Probability of Wolf Occurrence") + ylab("Probability of Black bear occurrence") + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text = element_text(size = 12)) + theme(legend.position="none")
Bearplot <- Bearplot + binomial_smooth(aes(x = Wolf, y = predict(B1.addLD1500, type = "response"), colour = Site), size = 1, se = FALSE)

print(Bearplot)

warnings() ## warns that values are not integers, not an issue

## Black bear occurrence as a function of pOpen
Openplot <- ggplot(Bears) + geom_point(aes(x = pOpen250, y = Blackbear), size = 2)

Openplot <- Openplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) # remove background

Openplot <- Openplot + xlab("Proportion of Open Forest") + ylab("Probability of Black bear occurrence") + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text = element_text(size = 12))
Openplot <- Openplot + binomial_smooth(aes(x = pOpen250, y = predict(B1.addLD1500, type = "response")), size = 1.25, se = TRUE)

print(Openplot)

## binomial smoother binned by Site doesn't show any difference of Site because pOpen is constant at each Site!! Therefore, remove colour = Site and jitter, on smoother set se = TRUE

## Check this with LD1500 -- TRUE
LDplot <- ggplot(Bears) + geom_point(aes(x = LD1500, y = Blackbear), size = 2)

LDplot <- LDplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) # remove background

LDplot <- LDplot + xlab("Linear Density (km)") + ylab("Probability of Black bear occurrence") + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text = element_text(size = 12))
LDplot <- LDplot + binomial_smooth(aes(x = LD1500, y = predict(B1.addLD1500, type = "response")), size = 1, se = TRUE)

print(LDplot)

### Plotting Black bear occurrence with Wolf occurrence
## Add column for wolf presence vs wolf absence
Bears$WolfPA <- ifelse(Bears$Wolf == 1, "Present", "Absent")
table(Bears$WolfPA)

bp1 <- ggplot(data = Bears, aes(x = WolfPA, y = Blackbear, fill = WolfPA)) + geom_boxplot() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
bp1

## Poor visualisation because of overwhelming number of zeroes
table(Bears$WolfPA) ## 13 805 absent, 179 Present
table(Bears$Blackbear) ## 13 669 0, 179 Present

## For clarity, I will remove all Sites in which both were absent (will still get 0-0 from absent days at present sites)
## Need a species by station matrix
spst <- read.csv("Station_data/detectionsByStation.csv")

spst <- spst[which(spst$Ursus.americanus > 0 & spst$Canis.lupus > 0), ]
SitesPA <- spst$X
SitesPA ## 37 --> will still be toooo many zeroes


### Back-transforming linear predictor estimates
## Logit link = log(x/1-x)
## Inverse = 1/(1+exp(-x))
## Tranformation of Estimate = 1/(1+exp(-Est.)) where Est. = estimate from model summary
## Transformation of upper and lower CI: 1/ (1+exp(-(Est. +/- SE))) --> applies to intercept only I think

## Create subsettable table of estimates and std. error
Mod.coef <- coef(summary(B1))[["cond"]]
Mod.coef
## confint function
confint(B1) ## Confidence intervals on the linear predictor estimates, already taking into account adjustments for the intercept (I think)
confint(B1)[1,1] ## subsettable
Mod.coef[1,1] + 1.96*Mod.coef[1,2] ## same as confint
Mod.coef[2,1] + 1.96*Mod.coef[2,2] ## same as confint; but should account for covariance with the intercept

## Empty matrix for logit coefficients, backtransformed coefficients with upper and lower CIs (3 x 4)
bt <- matrix(NA, nrow = 3, ncol = 4)
colnames(bt) <- c("logit.coef", "bt.coef", "UCI", "LCI")
rownames(bt) <- c("Intercept", "Wolf", "pOpen250")

## Function for backtransforming 
logit.bt <- function(x){
  1/(1+exp(-x))
}

###Intercept (and wolf absent)
bt[1,1] <- Mod.coef[1,1] #extracting logit coef
bt[1,2] <- logit.bt(bt[1,1]) #backtransforming
bt[1,3] <- logit.bt(bt[1,1] + 1.96*Mod.coef[1,2]) ## upper CI
bt[1,4] <- logit.bt(bt[1,1] - 1.96*Mod.coef[1,2]) ## lower CI

bt

## In calculating CIs for predictors, variance = variance(U+v) --> given variance (SE^2) + variance of intercept
# Var(U + V) = Var(U) + Var(V) + 2*Cov(U,V)


vcov(B1)[["cond"]][1,2] ## subsetting vcov for a glmmTMB object --> extracting values of the conditional model

## Loop for calculating wolf and pOpen backtransformed estimates
for(i in 2:3){
  bt[i,1] <- bt[1,1] + Mod.coef[i,1] ## adding est. of intercept and wolf/pOpen to obtain estimate
  bt[i,2] <- logit.bt(bt[i,1]) ## adding mean effect to matrix
  Var.U <- Mod.coef[1,2]^2 ## variance of the intercept
  Var.V <- Mod.coef[i,2]^2 ## variance of predictor
  Cov.UV <- vcov(B1)[["cond"]][1,i] ## covariance between intercept and predictor
  bt[i,3] = logit.bt(bt[i,1] + 1.96*(sqrt(Var.U + Var.V+ 2*Cov.UV))) ## Calculating CIs and adding to matrix
  bt[i,4] = logit.bt(bt[i,1] - 1.96*(sqrt(Var.U + Var.V+ 2*Cov.UV)))
}

bt
## Back transformed effects are soooo tiny per day -- plot them? (Plotted with Confidence intervals)
bt <- as.data.frame(bt)
bt$Est <- c("Intercept", "Wolf", "OpenForest")
colnames(bt) <- c("logit.coef", "Coefficient", "UCI", "LCI", "Predictor")
est <- ggplot(data = bt, aes(x = Predictor, y = Coefficient)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= LCI, ymax = UCI, width = 0.3)) + scale_x_discrete(limits=c("OpenForest", "Wolf")) + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + scale_y_continuous(limits = c(-0.05, 0.1)) + geom_hline(yintercept = 0)
est

### Non-backtransformed estimates (plotting with SE)
Mod.coef <- coef(summary(B1.addLD1500))[["cond"]]
Mod.coef <- as.data.frame(Mod.coef)
Mod.coef$Predictor <- c("Intercept", "Wolf", "LinearDensity", "OpenForest")
colnames(Mod.coef) <- c("Coefficient", "StdError", "zvalue", "Prob", "Predictor")
Mod.coef

est2 <- ggplot(data = Mod.coef, aes(x = Predictor, y = Coefficient)) + geom_point(size = 5, stroke = 0, shape = 16) + geom_errorbar(aes(ymin= Coefficient - StdError, ymax = Coefficient + StdError, width = 0.3)) + scale_x_discrete(limits=c("Wolf", "LinearDensity", "OpenForest")) + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 22)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 22)) + theme(strip.text = element_text(colour = "black", size = 22)) + geom_hline(yintercept = 0) + scale_y_continuous(limits = c(-2, 2))
est2 <- est2 + ggtitle("Black bear") + theme(plot.title = element_text(colour = "black", hjust = 0.5, vjust= -3, size = 24))
est2
