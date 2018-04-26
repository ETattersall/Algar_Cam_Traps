##########################################
# Ch1_models.R
# ZINB GLMMs testing species line use as a function of line characteristics and env. variables
# Started Apr. 19, 2018
##########################################

library(glmmTMB)
library(bbmle) #AICtab function
library(ggplot2)
library(lmtest)
library(reshape)
library(dplyr)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

#Load detection data
det <- read.csv("MonthlyDetections_nov2015-nov2017.csv")
det$X.1 <- NULL
det$X <- NULL

#Load lowland data
low <- read.csv("newAVIE_lowland8buffersizes.csv")

#Combine data
det$low250 <- low$low250[match(det$Site, low$CamStation)]
det$low500 <- low$low500[match(det$Site, low$CamStation)]
det$low750 <- low$low750[match(det$Site, low$CamStation)]
det$low1000 <- low$low1000[match(det$Site, low$CamStation)]
det$low1250 <- low$low1250[match(det$Site, low$CamStation)]
det$low1500 <- low$low1500[match(det$Site, low$CamStation)]
det$low1750 <- low$low1750[match(det$Site, low$CamStation)]
det$low2000 <- low$low2000[match(det$Site, low$CamStation)]

#Load linedensity data
LineDens <- read.csv("AlgarStationsLD_Lines.csv")

# Combine datasets
det$LD250 <- LineDens$X250m[match(det$Site, LineDens$CamStation)]
det$LD500 <- LineDens$X500m[match(det$Site, LineDens$CamStation)]
det$LD750 <- LineDens$X750m[match(det$Site, LineDens$CamStation)]
det$LD1000 <- LineDens$X1000m[match(det$Site, LineDens$CamStation)]
det$LD1250 <- LineDens$X1250m[match(det$Site, LineDens$CamStation)]
det$LD1500 <- LineDens$X1500m[match(det$Site, LineDens$CamStation)]
det$LD1750 <- LineDens$X1750m[match(det$Site, LineDens$CamStation)]
det$LD2000 <- LineDens$X2000m[match(det$Site, LineDens$CamStation)]


#Load line width and veg height
sp <- read.csv("Spatial_covariates.csv")

det$VegHt <- sp$VegHt[match(det$Site, sp$Site)]
det$LineWidth <- sp$LineWidth[match(det$Site, sp$Site)]

hist(det$Caribou)
hist(det$SnowDays)
hist(det$low1750)
hist(det$LD1750)
hist(det$VegHt)
hist(det$LineWidth)


## Visualising patterns using predict function (predicting new data, given the best-fit model)
## Need to first remove NAs from explanatory variables
newdat <-  det[ , c(1,2,12:34)] #Naming columns in df
newdat <- na.omit(newdat) ## Exluding NA rows, which causes lengths to differ

#### Caribou models ####
## Lowland measured at 1750m, LD measured at 1750m
# Not including Dist2Water --> no corresponding hypothesis

### Exploring distribution and overdispersion (code from Zuur & Ieno, 2016)
##1. Proportion of 0's in data
table(is.na(det$Caribou)) #647 NAs, 853 non-NAs
table(det$Caribou == 0) #781
sum(det$Caribou==0, na.rm = TRUE)/nrow(det) #52% 0's in data

##2. Fitting most basic model - Poisson GLM - to global model 
glm1 <- glmmTMB(Caribou~ Treatment + LineWidth + LD1750 + VegHt + low1750 + SnowDays + (1| Site) + (1|Month), data = det, family = poisson)
# Residuals and overdispersion
E1 <- resid(glm1, type="pearson")
N <- nrow(det)
p <- length(coef(glm1))
sum(E1^2)/(N-p) #0.815 --> less than one, so not overdispersed

##3. Simulating data from the model
#function for calculating overdispersion (as above)
MyDispersion <- function(y, mu, N, p){
  e <- (y-mu)/sqrt(mu)
  sum(e^2)/(N-p)
}
muFit <- fitted(glm1) #fitted values from model
Ysim <- matrix(nrow = N, ncol = 100000) #matrix storing simulated responses
DispersionSim <- vector(length = 100000)#vector storing Dispersion value
#Loop running simulation of Poisson distributed data and calculating Dispersion value
for(i in 1:100000){
  Ysim[,i] <- rpois(N,lambda = muFit)
  DispersionSim[i] <- MyDispersion(Ysim[,i], muFit, N, p) #warnings generated but vector obtained
}
#Histogram of dispersion statistics
hist(DispersionSim)# Simulated statistics are mostly on either side of 1 -->unlikely to have overdispersion

## Comparing proportions of 0's in simulated data to that in sampled data
zeros <- vector(length = 100000)
for(i in 1:100000){
  zeros[i] <- sum(Ysim[,i] == 0) / N
}

plot(table(zeros),
     axes = FALSE,
     xlim = c(0.4, 1),
     xlab = "Percentage of zeros",
     ylab = "Frequency")

axis(2)
axis(1, 
     at = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
     labels = c("40%", "50%", "60%", "70%", "80%", "90%"))
points(x = sum(det$Caribou==0, na.rm = TRUE)/ N,
       y = 0, pch = 16, cex = 5, col = 2)
## Model comparisons of distributions and zero-inflation
glm2 <- glmmTMB(Caribou~ Treatment + LineWidth + LD1750 + VegHt + low1750 + SnowDays + (1| Site) + (1|Month), data = det, family = nbinom1(link= "log"))
glm3 <- glmmTMB(Caribou~ Treatment + LineWidth + LD1750 + VegHt + low1750 + SnowDays + (1| Site) + (1|Month), data = det, family = nbinom2(link = "log"))
glm4 <- glmmTMB(Caribou~ Treatment + LineWidth + LD1750 + VegHt + low1750 + SnowDays + (1| Site) + (1|Month), zi = ~1, data = det, family = poisson)
glm5 <- glmmTMB(Caribou~ Treatment + LineWidth + LD1750 + VegHt + low1750 + SnowDays +(1| Site) + (1|Month), zi = ~1, data = det, family = nbinom1(link= "log"))
glm6 <- glmmTMB(Caribou~ Treatment + LineWidth + LD1750 + VegHt + low1750 + SnowDays + (1| Site) + (1|Month), zi = ~1, data = det, family = nbinom2(link= "log"))

modnames <- c("Poisson", "Nbinom1", "Nbinom2", "ZIP", "ZINB1", "ZINB2")
ICtab(glm1,glm2,glm3,glm4,glm5,glm6, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

summary(glm2)
summary(glm5)


#### Finding random structure (using 'beyond optimal' - global- model)
r0 <- glmmTMB(Caribou~ LineWidth + LD1750 + VegHt + low1750*Treatment + SnowDays, data = det, family = nbinom1)
rSite <- glmmTMB(Caribou~ LineWidth + LD1750 + VegHt + low1750*Treatment + SnowDays + (1|Site), data = det, family = nbinom1) 
rMonth <- glmmTMB(Caribou~ LineWidth + LD1750 + VegHt + low1750*Treatment + SnowDays + (1|Month), data = det, family = nbinom1)
r2 <- glmmTMB(Caribou~ LineWidth + LD1750 + VegHt + low1750*Treatment + SnowDays + (1|Site) + (1|Month), data = det, family = nbinom1)
r3 <- glmmTMB(Caribou~ LineWidth + LD1750 + VegHt + low1750*Treatment + SnowDays + (Treatment|Site) + (1|Month), data = det, family = nbinom1) #Will not converge
summary(r3)#NAs

ICtab(r0, rSite, rMonth, r2, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
#       dLogLik dAIC df weight
# r2      8.0     0.0 16 0.6817
# rSite   6.2     1.5 15 0.3141
# rMonth  1.3    11.3 15 0.0025
# r0      0.0    11.9 14 0.0018

### Continue with 2 random effects

#### I. Environmental models ####
#1. Null model
E1 <- glmmTMB(Caribou~1 + (1|Site) + (1|Month), data = det, family = nbinom1)

#2. Global model
E2 <- glmmTMB(Caribou~low1750 + SnowDays + (1|Site) + (1|Month), data = det, family = nbinom1)

#3. Lowland only
E3 <- glmmTMB(Caribou~low1750 + (1|Site) + (1|Month), data = det, family = nbinom1)

#4. Snow only
E4 <- glmmTMB(Caribou~SnowDays + (1|Site) + (1|Month), data = det, family = nbinom1)


#### Line characteristic models ####
#0. Null model (using top model from environmental models)
L0 <- glmmTMB(Caribou~low1750 + SnowDays + (1|Site) + (1|Month), data = det, family = nbinom1)

#1. Treatment
L1 <- glmmTMB(Caribou~Treatment + low1750 + SnowDays + (1|Site) + (1|Month), data = det, family = nbinom1)

#2. VegHt
L2 <- glmmTMB(Caribou~ VegHt + low1750 + SnowDays + (1|Site) + (1|Month), data = det, family = nbinom1)

#3. Line density
L3 <- glmmTMB(Caribou~ LD1750 + low1750 + SnowDays + (1|Site) + (1|Month), data = det, family = nbinom1)

#4. Line Width
L4 <- glmmTMB(Caribou~ LineWidth + low1750 + SnowDays + (1|Site) + (1|Month), data = det, family = nbinom1)

#5. Treatment + LineWidth
L5 <- glmmTMB(Caribou~ Treatment + LineWidth + low1750 + SnowDays + (1|Site) + (1|Month), data = det, family = nbinom1)

#6. LineWidth + VegHt
L6 <- glmmTMB(Caribou~ LineWidth + VegHt + low1750 + SnowDays + (1|Site) + (1|Month), data = det, family = nbinom1)

#7. Treatment + Line density
L7 <- glmmTMB(Caribou~ Treatment + LD1750 + low1750 + SnowDays + (1|Site) + (1|Month), data = det, family = nbinom1)

#8. VegHt + Treatment
L8 <- glmmTMB(Caribou~ Treatment + VegHt + low1750 + SnowDays + (1|Site) + (1|Month), data = det, family = nbinom1)

#9. Treatment, LineWidth, VegHt
L9 <- glmmTMB(Caribou~ Treatment + LineWidth + VegHt + low1750 + SnowDays + (1|Site) + (1|Month), data = det, family = nbinom1)

#10. Treatment, LineWidth, Line density
L10 <- glmmTMB(Caribou~ Treatment + LineWidth + LD1750 + low1750 + SnowDays + (1|Site) + (1|Month), data = det, family = nbinom1)

#11. Treatment, Line density, VegHt
L11 <- glmmTMB(Caribou~ Treatment + VegHt + LD1750 + low1750 + SnowDays + (1|Site) + (1|Month), data = det, family = nbinom1)

#12. Global model - Treatment, Line density, VegHt, LineWidth
L12 <-  glmmTMB(Caribou~ Treatment + LineWidth + LD1750 + VegHt + low1750 + SnowDays + (1|Site) + (1|Month), data = det, family = nbinom1)

## Interaction terms
# Treatment*low1750
L13 <-  glmmTMB(Caribou~ LineWidth + LD1750 + VegHt + low1750*Treatment + SnowDays + (1|Site) + (1|Month), data = det, family = nbinom1)



# Comparing all models together (environmental and linear characteristic)
cabtab <- ICtab(E1, E2, E3, E4, L0, L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
cabtab

lrtest(L2,L10)

# Model 1: Caribou ~ VegHt + low1750 + SnowDays + (1 | Site) + (1 | Month)
# Model 2: Caribou ~ Treatment + LineWidth + LD1750 + low1750 + SnowDays + 
#  (1 | Site) + (1 | Month)
 #   Df  LogLik Df  Chisq Pr(>Chisq)  
# 1   8 -265.60                       
# 2  12 -261.63  4 7.9497    0.09344 .

lrtest(CH10,CH11) #Line width significantly better than veg ht, but only a tiny tiny (0.14) difference in neg.LogLik

summary(CH10)
summary(CH2)
summary(CH12)


## Residual plot for global model(CH12)
op <- par(mfrow = c(1,2))
pred <- predict(CH12, zitype = "response")
resid <- residuals(CH12)

## Plot residuals against predicted --> diagonal lines characteristic of discrete data
plot(pred, resid, main = "Residuals vs. Predicted", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, lty = 2)  
lines(smooth.spline(pred, resid))
plot(fitted(CH12), residuals(CH12), main= "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)

#### Residual plot for most parsimonious model (CH2)
op <- par(mfrow = c(1,2))
pred <- predict(CH2, zitype = "response")
resid <- residuals(CH2)

## Plot residuals against predicted --> diagonal lines characteristic of discrete data
plot(pred, resid, main = "Residuals vs. Predicted", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, lty = 2)  
lines(smooth.spline(pred, resid))
plot(fitted(CH2), residuals(CH2), main= "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)

## Global model is a better fit, which is expected given that it hase more parameters


## Standardizing estimates? Evidence ratios? Model averaging?
