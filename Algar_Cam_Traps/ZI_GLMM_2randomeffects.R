##############################################
# ZI_GLMM_2ME.R
# ZINB glmms with 2 random effects: site and Month (NOT yr-month)
# Compare top model to top model with one random effect
# Started Feb. 13, 2018
#############################################

### Modelling with glmmTMB
library(glmmTMB)
library(bbmle) #AICtab function
library(ggplot2)
library(lmtest)
library(reshape)
library(dplyr)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
dat <- read.csv("monthlydetections_nov2015-apr2017.csv") # First 2 deployments monthly detection data + snow days
head(dat)
dat$X <- NULL
head(dat)

#Loading % lowland data
low <- read.csv("Lowlandcover_9buffersizes.csv")
##Add 250m and 500m to monthly detections
dat$low250 <- low$Prop250[match(dat$Site, low$CamStation)]
dat$low500 <- low$Prop500[match(dat$Site, low$CamStation)]

## Adding column for month only
str(dat) #Yr_Month is a factor
dat$Month <- as.Date(dat$Yr_Month, format = "%Y-%m") #NAs
dat$Month <- as.factor(format(as.Date(dat$Yr_Month, format = "%Y-%m-%d"), "%b")) #NAs
dat$Month <- NULL

## Separating elements from Yr_Month
dat$Yr_Month2 <- dat$Yr_Month #replicating Yr_Month for separation
dat <- cbind(dat[,1:15],
      colsplit(dat$Yr_Month2, "[-]", names=c("Year", "Month")))

## Including mean VegHt (measured in Nov 2017)
nov <- read.csv("Algar_CameraStationData_Nov2017.csv")
#Subset nov for 60 cams on lines
nov <- nov[1:60,]
#### Veg. Height - from Nov. 2017 ####
## Avg. Veg Height measured 
nov$LineVeg_Ht_avg <- rowMeans(nov[ , 24:26], na.rm = TRUE)
dat$VegHt <- nov$LineVeg_Ht_avg[match(dat$Site, nov$SiteID)]


#### Wolf models ####
## Top wolf models with 1 random effect
## Model 6: Wolf detections best predicted by SnowDays
wolf6 <- glmmTMB(Wolf~SnowDays + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model 4: Wolf detections best predicted by Treatment, %lowland, and SnowDays
wolf4 <- glmmTMB(Wolf~Treatment + low500 + SnowDays + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Wolf models with 2 random effects
## Model 0: Null, wolf detections best predicted by themselves
wzinb0 <- glmmTMB(Wolf~1 + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 1: Wolf detections best predicted by Treatment
wzinb1 <- glmmTMB(Wolf~Treatment + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 2: Wolf detections best predicted by Treatment and %lowland
wzinb2 <- glmmTMB(Wolf~Treatment + low500 + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 3: Wolf detections best predicted by an interaction between Treatment and %lowland
wzinb3 <- glmmTMB(Wolf~Treatment*low500 + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 4: Wolf detections best predicted by Treatment, %lowland, and SnowDays
wzinb4 <- glmmTMB(Wolf~Treatment + low500 + SnowDays + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 5: Wolf detections best predicted by Treatment and SnowDays
wzinb5 <- glmmTMB(Wolf~Treatment + SnowDays + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 6: Wolf detections best predicted by SnowDays
wzinb6 <- glmmTMB(Wolf~SnowDays + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 7: Wolf detections best predicted by %lowland
wzinb7 <- glmmTMB(Wolf~low500 + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 8: Wolf detections best predicted by Veg Height
wzinb8 <- glmmTMB(Wolf~VegHt + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 9: Wolf detections best predicted by Veg Height and Treatment
wzinb9 <- glmmTMB(Wolf~Treatment + VegHt + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 10: Wolf detections best predicted by Veg Height, Treatment, and %lowland
wzinb10 <- glmmTMB(Wolf~Treatment + VegHt + low500 + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 11: Wolf detections best predicted by Veg Height, Treatment, and %lowland
wzinb11 <- glmmTMB(Wolf~Treatment + VegHt + low500 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model selection with AICctab (bbmle) --> exluding VegHt
modnames <- c("1RE Snow", "1RE all cov", "Null", "Treat", "Treat + Low500", "Treat*Low500", "Treat + Low500 + Snow", "Treat + Snow", "Snow", "Low500" )
wolftab <- ICtab(wolf6, wolf4, wzinb0,wzinb1, wzinb2,wzinb3,wzinb4,wzinb5,wzinb6,wzinb7, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
wolftab

#                     dLogLik dAIC df weight
# 1RE Snow               5.0     0.0 5  0.3691
# 1RE all cov            8.8     0.5 9  0.2928
# Snow                   5.0     2.0 6  0.1358
# Treat + Low500 + Snow  8.8     2.5 10 0.1077
# Treat + Snow           7.6     2.9 9  0.0875
# Null                   0.0    10.1 5  0.0024
# Treat + Low500         3.8    10.5 9  0.0019
# Treat                  2.5    11.0 8  0.0015
# Low500                 0.2    11.7 6  0.0010
# Treat*Low500           4.8    14.4 12 <0.001

summary(wolf6)
summary(wzinb6) ## accounts for a tiiiiny amount of variance
summary(wzinb4) ## Same
summary(wzinb5)

modnames <- c("Null", "Treat", "Treat + Low500", "Treat*Low500", "Treat + Low500 + Snow", "Treat + Snow", "Snow", "Low500", "VegHt", "Treat + VegHt", "Treat+low500+VegHt", "Treat+low500+SnowDays+VegHt")
wolftab <- ICtab(wzinb0,wzinb1, wzinb2,wzinb3,wzinb4,wzinb5,wzinb6,wzinb7,wzinb8, wzinb9, wzinb10, wzinb11, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
wolftab

#                             dLogLik dAIC df weight
# Snow                         5.0     0.0 6  0.3572
# Treat + Low500 + Snow        8.8     0.5 10 0.2834
# Treat + Snow                 7.6     0.9 9  0.2303
# Treat+low500+SnowDays+VegHt  8.8     2.5 11 0.1043
# Null                         0.0     8.1 5  0.0064
# Treat + Low500               3.8     8.5 9  0.0051
# Treat                        2.5     9.0 8  0.0039
# Low500                       0.2     9.7 6  0.0028
# VegHt                        0.1     9.8 6  0.0027
# Treat+low500+VegHt           3.8    10.5 10 0.0019
# Treat + VegHt                2.5    11.0 9  0.0015
# Treat*Low500                 4.8    12.4 12 <0.001

summary(wzinb8)
summary(wzinb11) ## VegHt adds next to nothing to predictions

lrtest(wzinb6,wzinb4)
# Model 1: Wolf ~ SnowDays + (1 | Site) + (1 | Month)
# Model 2: Wolf ~ Treatment + low500 + SnowDays + (1 | Site) + (1 | Month)
#   #Df  LogLik Df  Chisq Pr(>Chisq)
# 1   6 -361.64                     
# 2  10 -357.87  4 7.5371     0.1101

lrtest(wzinb6, wzinb5)
# Model 1: Wolf ~ SnowDays + (1 | Site) + (1 | Month)
# Model 2: Wolf ~ Treatment + SnowDays + (1 | Site) + (1 | Month)
#   #Df  LogLik Df  Chisq Pr(>Chisq)
# 1   6 -361.64                     
# 2   9 -359.08  3 5.1217     0.1631

sigma(wzinb4) #returns the dispersion parameter for that particular family of distribution
# For nbinom2 family, larger sigma = LOWER VARIANCE

wzinb4$fit$objective## neg. logLik
wzinb4$fit$par ##parameters

#### Analyzing residuals for top models, comparing to lower models
## Pearson residuals not used in ZI models

## Visualising patterns using predict function (predicting new data, given the best-fit model)
newdat <-  dat[ , c("Site", "Month", "Treatment", "low500", "SnowDays")] #Naming columns in df
newdat <- na.omit(newdat) ## Exluding NA rows, which causes lengths to differ
temp<- predict(wzinb4, newdata = newdat, se.fit = TRUE, zitype = "response") #putting predictions in  a temp df
length(temp$fit) #627
length(newdat$Site) #627 after removing NAs

newdat$predFE <- temp$fit # mean prediction added to new data frame
newdat$predFE.min <- temp$fit - 1.98*temp$se.fit # min limit
newdat$predFE.max = temp$fit + 1.98*temp$se.fit # max limit


library(plyr)
library(digest)
real=ddply(dat, ~Site+Month+Treatment+low500+SnowDays, summarize, m=mean(Wolf)) # real data (haven't figured out how to add)


ggplot(newdat, aes(Treatment, predFE, fill=Treatment)) + geom_boxplot(aes(ymin=predFE.min, ymax=predFE.max)) +theme_classic() + xlab("Sampling Strata") + ylab("Wolf detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

### Adding real data?
# + geom_boxplot(data=real, aes(x=Treatment, y= m))
# + ylab("Wolf Detections/month")

### Plotting residuals
op <- par(mfrow = c(1, 2))
## Residuals vs. fitted plots
plot(fitted(wzinb4), residuals(wzinb4), main= "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")  
abline(h = 0, lty = 2)  
lines(smooth.spline(fitted(wzinb4), residuals(wzinb4)))  
### Clearly shows patterns in the residuals

plot(newdat$Treatment, residuals(wzinb4), main= "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
## Median residuals is slightly different across treatments, but basically zero

## Suggested to plot residuals vs predicted instead
op <- par(mfrow = c(1,1))
pred <- predict(wzinb4, se.fit = TRUE, zitype = "response")
resid <- residuals(wzinb4)
plot(pred, resid, main = "Residuals vs. Predicted", xlab = "Predicted Values", ylab = "Residuals")# Error: x and y lengths differ

length(pred$fit) #627
length(resid)#627
length(fitted(wzinb4))#627

#Try na.omit?
plot(na.omit(pred), na.omit(resid), main = "Residuals vs. Predicted", xlab = "Predicted Values", ylab = "Residuals") #same error
#na.exclude?
plot(na.exclude(pred), na.exclude(resid), main = "Residuals vs. Predicted", xlab = "Predicted Values", ylab = "Residuals")#same error

## Removing se.fit from predict --> works
pred <- predict(wzinb4, zitype = "response")
resid <- residuals(wzinb4)
plot(pred, resid, main = "Residuals vs. Predicted", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, lty = 2)  
lines(smooth.spline(pred, resid))


#### Plotting coefficients for top model
## Run top model as intercept free
wolfIF <- glmmTMB(Wolf~Treatment + low500 + SnowDays + (1| Site) + (1|Month) -1, zi = ~1, data = dat, family = nbinom2)
summary(wolfIF) ## Control estimate is same as intercept, proceed with original model
wolfcoeffs <- wzinb4$fit$par[1:6]
## SE? Can't figure out how to call from output, will enter manually
wolfSE <- c(0.806879, 0.607718, 0.685087, 0.547692, 1.069428, 0.006722) #for estimates in the conditional model
COVnames <- (c("Intercept", "HumanUse", "NatRegen", "SPP", "Low", "Snow")) #Covariate names

WCoef <- cbind.data.frame(COVnames, wolfcoeffs,wolfSE)
colnames(WCoef) <- c("Covariates","Coefficients", "SE")
str(WCoef)

ggplot(data = WCoef, aes( x = Covariates, y = Coefficients)) + geom_point() + geom_errorbar(aes(ymin=Coefficients - SE, ymax = Coefficients + SE))+ geom_hline(yintercept = 0)  + scale_x_discrete(limits=c("Intercept","NatRegen","SPP", "HumanUse", "Low", "Snow"))+theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + theme(strip.text = element_text(colour = "black", size = 14))



#### Bear models ####
## Bear dataset
bear <- dat %>% filter(Yr_Month == "2016-04" | Yr_Month == "2016-05" | Yr_Month == "2016-06"| Yr_Month == "2016-07"| Yr_Month == "2016-08"| Yr_Month == "2016-09"| Yr_Month == "2016-10"| Yr_Month == "2017-04") %>% select(Site, Treatment,Yr_Month, Site_ym, Blackbear, SnowDays, low250,low500, Month, VegHt)

## Model 0: Null, Blackbear detections best predicted by themselves
bbzinb0 <- glmmTMB(Blackbear~1 + (1|Site) + (1|Month), zi = ~1, data = bear, family = nbinom2)

## Model 1: Blackbear detections best predicted by Treatment
bbzinb1 <- glmmTMB(Blackbear~Treatment + (1| Site) + (1| Month), zi = ~1, data = bear, family = nbinom2)

## Model 2: Blackbear detections best predicted by Treatment and %lowland
bbzinb2 <- glmmTMB(Blackbear~Treatment + low500 + (1| Site) + (1|Month), zi = ~1, data = bear, family = nbinom2)

## Model 3: Blackbear detections best predicted by an interaction between Treatment and %lowland
bbzinb3 <- glmmTMB(Blackbear~Treatment*low500 + (1| Site) + (1|Month), zi = ~1, data = bear, family = nbinom2)
# Warning message:
#  In fitTMB(TMBStruc) :
#  Model convergence problem; extreme or very small eigen values detected. See vignette
# ('troubleshooting')

## Model 7: Blackbear detections best predicted by %lowland
bbzinb7 <- glmmTMB(Blackbear~low500 + (1|Site) + (1|Month), zi = ~1, data = bear, family = nbinom2)

## Model 8: Blackbear detections best predicted by Veg Height
bbzinb8 <- glmmTMB(Blackbear~VegHt + (1|Site) + (1|Month), zi = ~1, data = bear, family = nbinom2)

## Model 9: Blackbear detections best predicted by Veg Height and Treatment
bbzinb9 <- glmmTMB(Blackbear~Treatment + VegHt + (1|Site) + (1|Month), zi = ~1, data = bear, family = nbinom2)

## Model 10: Blackbear detections best predicted by Veg Height, Treatment, and %lowland
bbzinb10 <- glmmTMB(Blackbear~Treatment + VegHt + low500 + (1|Site) + (1|Month), zi = ~1, data = bear, family = nbinom2)


modnames <- c("Null", "Treat", "Treat + Low500", "Low500", "VegHt", "Treat + VegHt", "Treat+low500+VegHt") #exclude interaction model
beartab <- ICtab(bbzinb0,bbzinb1, bbzinb2, bbzinb7,bbzinb8, bbzinb9,bbzinb10, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
beartab

#                   dLogLik dAIC  df weight
# Treat+low500+VegHt  9.3     0.0 10 0.5874
# Treat + Low500      7.6     1.5 9  0.2805
# Low500              3.2     4.2 6  0.0732
# Treat + VegHt       5.4     5.9 9  0.0307
# VegHt               1.7     7.3 6  0.0155
# Null                0.0     8.6 5  0.0078
# Treat               2.5     9.6 8  0.0048

summary(bbzinb0) ## zero-inflation makes no difference
summary(bbzinb1)
summary(bbzinb2)
summary(bbzinb7)

lrtest(bbzinb2,bbzinb7)
# Model 1: Blackbear ~ Treatment + low500 + (1 | Site) + (1 | Month)
# Model 2: Blackbear ~ low500 + (1 | Site) + (1 | Month)
# #Df  LogLik Df  Chisq Pr(>Chisq)  
#  1   9 -202.56                       
#  2   6 -206.90 -3 8.6864    0.03376 *

lrtest(bbzinb10,bbzinb11)
# Model 1: Blackbear ~ Treatment + VegHt + low500 + (1 | Site) + (1 | Month)
# Model 2: Blackbear ~ Treatment + VegHt + low500 + SnowDays + (1 | Site) + 
#   (1 | Month)
#   #Df  LogLik Df  Chisq Pr(>Chisq)  
# 1  10 -200.82                       
# 2  11 -198.11  1 5.4241    0.01986 *

lrtest(bbzinb2, bbzinb10)
# Model 1: Blackbear ~ Treatment + low500 + (1 | Site) + (1 | Month)
# Model 2: Blackbear ~ Treatment + VegHt + low500 + (1 | Site) + (1 | Month)
#   #Df  LogLik Df  Chisq Pr(>Chisq)  
# 1   9 -202.56                       
# 2  10 -200.82  1 3.4785    0.06217 .



summary(bbzinb10) #Very slight  positive effect of VegHt
summary(bbzinb2)

## Plotting bbzinb10 residuals
op <- par(mfrow = c(1,2))
pred <- predict(bbzinb10, zitype = "response")
resid <- residuals(bbzinb10)
plot(pred, resid, main = "Residuals vs. Predicted", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, lty = 2)  
lines(smooth.spline(pred, resid))
plot(bear$Treatment, residuals(bbzinb10), main= "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
length(bear$Treatment) #480
length(residuals(bbzinb10)) #224
length(na.omit(bear$Treatment)) #No NAs

##New data frame with NA rows omitted
newbear <-  bear[ , c("Site", "Month", "Treatment", "low500", "SnowDays")] #Naming columns in df
newbear <- na.omit(newbear) ## Exluding NA rows, which causes lengths to differ

op <- par(mfrow = c(1,2))
pred <- predict(bbzinb10, zitype = "response")
resid <- residuals(bbzinb10)
plot(pred, resid, main = "Residuals vs. Predicted", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, lty = 2)  
lines(smooth.spline(pred, resid))
plot(newbear$Treatment, residuals(bbzinb10), main= "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")

#### Plotting coefficients for top model bbzinb10
bearcoeffs <- bbzinb10$fit$par[1:6]
## SE? Can't figure out how to call from output, will enter manually
bearSE <- c(0.8311,1830,1.258,0.3643,0.2122,0.9797) #for estimates in the conditional model
COVnames <- (c("Intercept", "HumanUse", "NatRegen", "SPP", "VegHt", "Low")) #Covariate names

BCoef <- cbind.data.frame(COVnames, bearcoeffs,bearSE)
colnames(BCoef) <- c("Covariates","Coefficients", "SE")
str(BCoef)

## Removed HumanUse - no data, so inflated scale
ggplot(data = BCoef, aes( x = Covariates, y = Coefficients)) + geom_point() + geom_errorbar(aes(ymin=Coefficients - SE, ymax = Coefficients + SE))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("Intercept", "NatRegen","SPP", "VegHt", "Low")) +theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + theme(strip.text = element_text(colour = "black", size = 14)) + ylim(-5,5)

##### Caribou models ####

## Top models with 1 random effect
cab4 <- glmmTMB(Caribou~Treatment + low500 + SnowDays + (1| Site), zi = ~1, data = dat, family = nbinom2)


## Model 0: Null, Caribou detections best predicted by themselves
cabzinb0 <- glmmTMB(Caribou~1 + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 1: Caribou detections best predicted by Treatment
cabzinb1 <- glmmTMB(Caribou~Treatment + (1| Site)+ (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 2: Caribou detections best predicted by Treatment and %lowland
cabzinb2 <- glmmTMB(Caribou~Treatment + low500 + (1| Site)+ (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 3: Caribou detections best predicted by an interaction between Treatment and %lowland
cabzinb3 <- glmmTMB(Caribou~Treatment*low500 + (1| Site)+ (1|Month), zi = ~1, data = dat, family = nbinom2)
# Warning message:
#  In fitTMB(TMBStruc) :
#  Model convergence problem; extreme or very small eigen values detected. See vignette
# ('troubleshooting')

## Model 4: Caribou detections best predicted by Treatment, %lowland, and SnowDays
cabzinb4 <- glmmTMB(Caribou~Treatment + low500 + SnowDays + (1| Site)+ (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 5: Caribou detections best predicted by Treatment and SnowDays
cabzinb5 <- glmmTMB(Caribou~Treatment + SnowDays + (1|Site)+ (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 6: Caribou detections best predicted by SnowDays
cabzinb6 <- glmmTMB(Caribou~SnowDays + (1|Site)+ (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 7: Caribou detections best predicted by %lowland
cabzinb7 <- glmmTMB(Caribou~low500 + (1|Site)+ (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 8: Caribou detections best predicted by Veg Height
cabzinb8 <- glmmTMB(Caribou~VegHt + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 9: Caribou detections best predicted by Veg Height and Treatment
cabzinb9 <- glmmTMB(Caribou~Treatment + VegHt + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 10: Caribou detections best predicted by Veg Height, Treatment, and %lowland
cabzinb10 <- glmmTMB(Caribou~Treatment + VegHt + low500 + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 11: Caribou detections best predicted by Veg Height, Treatment, and %lowland
cabzinb11 <- glmmTMB(Caribou~Treatment + VegHt + low500 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

### Comparing difference from adding time random effect
modnames <- c("1RE", "Null", "Treat", "Treat + Low500", "Treat*Low500", "Treat + Low500 + Snow", "Treat + Snow", "Snow", "Low500")
cabtab <- ICtab(cab4, cabzinb0,cabzinb1, cabzinb2,cabzinb3,cabzinb4,cabzinb5,cabzinb6,cabzinb7, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
cabtab

#                       dLogLik dAIC df weight
# Low500                 9.8     0.0 6  0.276 
# 1RE                   12.8     0.0 9  0.271 
# Treat + Low500 + Snow 13.6     0.3 10 0.233 
# Treat + Low500        12.2     1.2 9  0.151 
# Treat*Low500          14.4     2.8 12 0.068 
# Treat + Snow           6.2    13.1 9  <0.001
# Treat                  4.5    14.6 8  <0.001
# Snow                   2.1    15.4 6  <0.001
# Null                   0.0    17.6 5  <0.001

## Excluding 1RE and interaction model
modnames <- c("Null", "Treat", "Treat + Low500", "Treat + Low500 + Snow", "Treat + Snow", "Snow", "Low500", "VegHt", "Treat + VegHt", "Treat+low500+VegHt", "Treat+low500+SnowDays+VegHt")
cabtab <- ICtab(cabzinb0,cabzinb1, cabzinb2,cabzinb4,cabzinb5,cabzinb6,cabzinb7,cabzinb8,cabzinb9,cabzinb10,cabzinb11, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
cabtab

#                      dLogLik dAIC df weight
# Low500                       9.8     0.0 6  0.30  
# Treat + Low500 + Snow       13.6     0.3 10 0.25  
# Treat+low500+SnowDays+VegHt 14.3     1.0 11 0.18  
# Treat + Low500              12.2     1.2 9  0.16  
# Treat+low500+VegHt          12.8     2.0 10 0.11  
# Treat + VegHt                7.0    11.5 9  <0.001
# VegHt                        3.2    13.1 6  <0.001
# Treat + Snow                 6.2    13.1 9  <0.001
# Treat                        4.5    14.6 8  <0.001
# Snow                         2.1    15.4 6  <0.001
# Null                         0.0    17.6 5  <0.001

lrtest(cabzinb7,cabzinb4)
lrtest(cabzinb2,cabzinb4)
lrtest(cabzinb7, cabzinb2)
lrtest(cabzinb4, cabzinb11) #no sig. diff.


summary(cabzinb7)
summary(cabzinb4)
summary(cabzinb11)


##Changing the intercept for canzinb4
str(dat$Treatment)
dat$Treatment <- factor(dat$Treatment, levels = c("HumanUse", "Control", "SPP", "NatRegen"))
str(dat$Treatment)
## Re-running model with HumanUse as intercept
cabzinb4.INT <- glmmTMB(Caribou~Treatment + low500 + SnowDays + (1| Site)+ (1|Month), zi = ~1, data = dat, family = nbinom2)

summary(cabzinb4.INT) ## No real difference between Control and SPP

## Returning Treatment to original levels
dat$Treatment <- factor(dat$Treatment, levels = c("Control", "HumanUse", "SPP", "NatRegen"))
str(dat$Treatment)

## Residual plots for cabzinb7 and cabzin4
## cabzinb7
op <- par(mfrow = c(1,2))
pred <- predict(cabzinb7, zitype = "response")
resid <- residuals(cabzinb7)
plot(pred, resid, main = "Residuals vs. Predicted", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, lty = 2)  
lines(smooth.spline(pred, resid))
plot(newdat$low500, residuals(cabzinb7), main= "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")

## cabzinb4
op <- par(mfrow = c(1,2))
pred <- predict(cabzinb4, zitype = "response")
resid <- residuals(cabzinb4)
plot(pred, resid, main = "Residuals vs. Predicted", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, lty = 2)  
lines(smooth.spline(pred, resid))
plot(newdat$Treatment, residuals(cabzinb4), main= "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")

#### Plotting coefficients for top models: cabzinb7 and cabzinb4
## cabzinb7
cab7coeffs <- cabzinb7$fit$par[2]
## SE? Can't figure out how to call from output, will enter manually
cab7SE <- c(2.180) #for estimates in the conditional model
COVnames <- "Low" #Covariate names

cab7Coef <- cbind.data.frame(COVnames, cab7coeffs,cab7SE)
colnames(cab7Coef) <- c("Covariates","Coefficients", "SE")
str(cab7Coef)

ggplot(data = cab7Coef, aes( x = Covariates, y = Coefficients)) + geom_point() + geom_errorbar(aes(ymin=Coefficients - SE, ymax = Coefficients + SE))+ geom_hline(yintercept = 0) +theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + theme(strip.text = element_text(colour = "black", size = 14)) 

##cabzinb4
cab4coeffs <- cabzinb4$fit$par[1:6]
## SE? Can't figure out how to call from output, will enter manually
cab4SE <- c(2.15396,1.10735,7597.85406,0.81296,2.44822,0.02138) #for estimates in the conditional model
COVnames <- c("Intercept", "HumanUse", "NatRegen","SPP","Low", "Snow") #Covariate names

cab4Coef <- cbind.data.frame(COVnames, cab4coeffs,cab4SE)
colnames(cab4Coef) <- c("Covariates","Coefficients", "SE")
str(cab4Coef)

## Removed NatRegen - 1 data point, so inflated scale
ggplot(data = cab4Coef, aes( x = Covariates, y = Coefficients)) + geom_point() + geom_errorbar(aes(ymin=Coefficients - SE, ymax = Coefficients + SE))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("Intercept","SPP", "HumanUse", "Low", "Snow"))+theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + theme(strip.text = element_text(colour = "black", size = 14)) + ylim(-10,10) 



#### WT Deer ####

## Model 0: Null, WTDeer detections best predicted by themselves
WTDzinb0 <- glmmTMB(WTDeer~1 + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 1: WTDeer detections best predicted by Treatment
WTDzinb1 <- glmmTMB(WTDeer~Treatment + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 2: WTDeer detections best predicted by Treatment and %lowland
WTDzinb2 <- glmmTMB(WTDeer~Treatment + low500 + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 3: WTDeer detections best predicted by an interaction between Treatment and %lowland
WTDzinb3 <- glmmTMB(WTDeer~Treatment*low500 + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 4: WTDeer detections best predicted by Treatment, %lowland, and SnowDays
WTDzinb4 <- glmmTMB(WTDeer~Treatment + low500 + SnowDays + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 5: WTDeer detections best predicted by Treatment and SnowDays
WTDzinb5 <- glmmTMB(WTDeer~Treatment + SnowDays + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 6: WTDeer detections best predicted by SnowDays
WTDzinb6 <- glmmTMB(WTDeer~SnowDays + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 7: WTDeer detections best predicted by %lowland
WTDzinb7 <- glmmTMB(WTDeer~low500 + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 8: WTDeer detections best predicted by Veg Height
WTDzinb8 <- glmmTMB(WTDeer~VegHt + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 9: WTDeer detections best predicted by Veg Height and Treatment
WTDzinb9 <- glmmTMB(WTDeer~Treatment + VegHt + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 10: WTDeer detections best predicted by Veg Height, Treatment, and %lowland
WTDzinb10 <- glmmTMB(WTDeer~Treatment + VegHt + low500 + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 11: WTDeer detections best predicted by Veg Height, Treatment, and %lowland
WTDzinb11 <- glmmTMB(WTDeer~Treatment + VegHt + low500 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

modnames <- c("Null", "Treat", "Treat + Low500", "Treat*Low", "Treat + Low500 + Snow", "Treat + Snow", "Snow", "Low500", "VegHt", "Treat + VegHt", "Treat+low500+VegHt", "Treat+low500+SnowDays+VegHt")
WTDtab <- ICtab(WTDzinb0,WTDzinb1, WTDzinb2, WTDzinb3,WTDzinb4,WTDzinb5,WTDzinb6,WTDzinb7, WTDzinb8, WTDzinb9, WTDzinb10, WTDzinb11, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
WTDtab

#                      dLogLik dAIC df weight
# Treat + Low500 + Snow       15.1     0.0 10 0.368 
# Low500                      10.5     1.2 6  0.198 
# Treat + Low500              13.4     1.4 9  0.181 
# Treat+low500+SnowDays+VegHt 15.3     1.7 11 0.160 
# Treat+low500+VegHt          13.6     3.0 10 0.081 
# Treat*Low                   13.6     7.0 12 0.011 
# Treat + Snow                 7.7    12.9 9  <0.001
# Treat                        5.9    14.4 8  <0.001
# Treat + VegHt                6.4    15.4 9  <0.001
# Snow                         1.7    18.8 6  <0.001
# Null                         0.0    20.2 5  <0.001
# VegHt                        1.0    20.3 6  <0.001

lrtest(WTDzinb7, WTDzinb4)
# Model 1: WTDeer ~ low500 + (1 | Site) + (1 | Month)
# Model 2: WTDeer ~ Treatment + low500 + SnowDays + (1 | Site) + (1 | Month)
#    #Df  LogLik Df Chisq Pr(>Chisq)  
# 1   6 -425.19                      
# 2  10 -420.57  4 9.241    0.05535 .

lrtest(WTDzinb2, WTDzinb4)
# Model 1: WTDeer ~ Treatment + low500 + (1 | Site) + (1 | Month)
# Model 2: WTDeer ~ Treatment + low500 + SnowDays + (1 | Site) + (1 | Month)
#    #Df  LogLik Df  Chisq Pr(>Chisq)  
# 1   9 -422.28                       
# 2  10 -420.57  1 3.4153    0.06459 .

summary(WTDzinb4)
summary(WTDzinb7)
summary(WTDzinb11)

## Residuals for WTDzinb4
op <- par(mfrow = c(1,2))
pred <- predict(WTDzinb4, zitype = "response")
resid <- residuals(WTDzinb4)
plot(pred, resid, main = "Residuals vs. Predicted", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, lty = 2)  
lines(smooth.spline(pred, resid))
plot(newdat$Treatment, residuals(WTDzinb4), main= "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")


## Plotting coefficients for top model: WTDzinb4
WTD4coeffs <- WTDzinb4$fit$par[1:6]
## SE? Can't figure out how to call from output, will enter manually
WTD4SE <- c(1.04140, 0.77584, 0.74029, 0.70341, 1.45937, 0.01510) #for estimates in the conditional model
COVnames <- c("Intercept", "HumanUse", "NatRegen","SPP","Low", "Snow") #Covariate names

WTD4Coef <- cbind.data.frame(COVnames, WTD4coeffs,WTD4SE)
colnames(WTD4Coef) <- c("Covariates","Coefficients", "SE")
str(WTD4Coef)

## Removed NatRegen - 1 data point, so inflated scale
ggplot(data = WTD4Coef, aes( x = Covariates, y = Coefficients)) + geom_point() + geom_errorbar(aes(ymin=Coefficients - SE, ymax = Coefficients + SE))+ geom_hline(yintercept = 0) + scale_x_discrete(limits=c("Intercept","NatRegen", "SPP", "HumanUse", "Low", "Snow"))+theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + theme(strip.text = element_text(colour = "black", size = 14))

#### Moose models ####

## Model 0: Null, Moose detections best predicted by themselves
MOOzinb0 <- glmmTMB(Moose~1 + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 1: Moose detections best predicted by Treatment
MOOzinb1 <- glmmTMB(Moose~Treatment + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 2: Moose detections best predicted by Treatment and %lowland
MOOzinb2 <- glmmTMB(Moose~Treatment + low500 + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 3: Moose detections best predicted by an interaction between Treatment and %lowland
MOOzinb3 <- glmmTMB(Moose~Treatment*low500 + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 4: Moose detections best predicted by Treatment, %lowland, and SnowDays
MOOzinb4 <- glmmTMB(Moose~Treatment + low500 + SnowDays + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 5: Moose detections best predicted by Treatment and SnowDays
MOOzinb5 <- glmmTMB(Moose~Treatment + SnowDays + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 6: Moose detections best predicted by SnowDays
MOOzinb6 <- glmmTMB(Moose~SnowDays + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 7: Moose detections best predicted by %lowland
MOOzinb7 <- glmmTMB(Moose~low500 + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 8: Moose detections best predicted by Veg Height
MOOzinb8 <- glmmTMB(Moose~VegHt + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 9: Moose detections best predicted by Veg Height and Treatment
MOOzinb9 <- glmmTMB(Moose~Treatment + VegHt + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 10: Moose detections best predicted by Veg Height, Treatment, and %lowland
MOOzinb10 <- glmmTMB(Moose~Treatment + VegHt + low500 + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 11: Moose detections best predicted by Veg Height, Treatment, and %lowland
MOOzinb11 <- glmmTMB(Moose~Treatment + VegHt + low500 + SnowDays + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

modnames <- c("Null", "Treat", "Treat + Low500", "Treat*Low", "Treat + Low500 + Snow", "Treat + Snow", "Snow", "Low500", "VegHt", "Treat + VegHt", "Treat+low500+VegHt")
MOOtab <- ICtab(MOOzinb0,MOOzinb1, MOOzinb2, MOOzinb3,MOOzinb4,MOOzinb5,MOOzinb6,MOOzinb7, MOOzinb8, MOOzinb9, MOOzinb10, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
MOOtab

MOOzinb3$fit$message

## Moose models without zero inflation

## Model 0: Null, Moose detections best predicted by themselves
MOO0 <- glmmTMB(Moose~1 + (1|Site) + (1|Month), data = dat, family = nbinom2)

## Model 1: Moose detections best predicted by Treatment
MOO1 <- glmmTMB(Moose~Treatment + (1| Site) + (1|Month), data = dat, family = nbinom2)

## Model 2: Moose detections best predicted by Treatment and %lowland
MOO2 <- glmmTMB(Moose~Treatment + low500 + (1| Site) + (1|Month), data = dat, family = nbinom2)

## Model 3: Moose detections best predicted by an interaction between Treatment and %lowland
MOO3 <- glmmTMB(Moose~Treatment*low500 + (1| Site) + (1|Month), data = dat, family = nbinom2)

## Model 4: Moose detections best predicted by Treatment, %lowland, and SnowDays
MOO4 <- glmmTMB(Moose~Treatment + low500 + SnowDays + (1| Site) + (1|Month), data = dat, family = nbinom2)

## Model 5: Moose detections best predicted by Treatment and SnowDays
MOO5 <- glmmTMB(Moose~Treatment + SnowDays + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 6: Moose detections best predicted by SnowDays
MOO6 <- glmmTMB(Moose~SnowDays + (1|Site) + (1|Month), data = dat, family = nbinom2)

## Model 7: Moose detections best predicted by %lowland
MOO7 <- glmmTMB(Moose~low500 + (1|Site) + (1|Month), data = dat, family = nbinom2)

## Model 8: Moose detections best predicted by Veg Height
MOO8 <- glmmTMB(Moose~VegHt + (1|Site) + (1|Month), data = dat, family = nbinom2)

## Model 9: Moose detections best predicted by Veg Height and Treatment
MOO9 <- glmmTMB(Moose~Treatment + VegHt + (1|Site) + (1|Month), data = dat, family = nbinom2)

## Model 10: Moose detections best predicted by Veg Height, Treatment, and %lowland
MOO10 <- glmmTMB(Moose~Treatment + VegHt + low500 + (1|Site) + (1|Month), data = dat, family = nbinom2)

## Model 11: Moose detections best predicted by Veg Height, Treatment, and %lowland
MOO11 <- glmmTMB(Moose~Treatment + VegHt + low500 + SnowDays + (1|Site) + (1|Month), data = dat, family = nbinom2)

modnames <- c("Null", "Treat", "Treat + Low500", "Treat*Low", "Treat + Low500 + Snow", "Treat + Snow", "Snow", "Low500", "VegHt", "Treat + VegHt", "Treat+low500+VegHt", "Treat+low500+VegHt+Snow")
MOOtab <- ICtab(MOO0,MOO1, MOO2, MOO3,MOO4,MOO5,MOO6,MOO7, MOO8, MOO9, MOO10, MOO11,  mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
MOOtab ## Everything except MOO10 and MOO3 result in NAs

AICctab()

summary(MOO10) 
summary(MOO0)




#### Coyote models ####
## Model 0: Null, Coyote detections best predicted by themselves
COYzinb0 <- glmmTMB(Coyote~1 + (1|Site)+ (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 1: Coyote detections best predicted by Treatment
COYzinb1 <- glmmTMB(Coyote~Treatment + (1| Site)+ (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 2: Coyote detections best predicted by Treatment and %lowland
COYzinb2 <- glmmTMB(Coyote~Treatment + low500 + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 3: Coyote detections best predicted by an interaction between Treatment and %lowland
COYzinb3 <- glmmTMB(Coyote~Treatment*low500 + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 4: Coyote detections best predicted by Treatment, %lowland, and SnowDays
COYzinb4 <- glmmTMB(Coyote~Treatment + low500 + SnowDays + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 5: Coyote detections best predicted by Treatment and SnowDays
COYzinb5 <- glmmTMB(Coyote~Treatment + SnowDays + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 6: Coyote detections best predicted by SnowDays
COYzinb6 <- glmmTMB(Coyote~SnowDays + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 7: Coyote detections best predicted by %lowland
COYzinb7 <- glmmTMB(Coyote~low500 + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

modnames <- c("Null", "Treat", "Treat + Low500", "Treat*Low", "Treat + Low500 + Snow", "Treat + Snow", "Snow", "Low500")
COYtab <- ICtab(COYzinb0,COYzinb1, COYzinb2, COYzinb3,COYzinb4,COYzinb5,COYzinb6,COYzinb7, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
COYtab

lrtest(COYzinb0, COYzinb7)
summary(COYzinb0)

#### Lynx models ####
## Model 0: Null, Lynx detections best predicted by themselves
LYNzinb0 <- glmmTMB(Lynx~1 + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 1: Lynx detections best predicted by Treatment
LYNzinb1 <- glmmTMB(Lynx~Treatment + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 2: Lynx detections best predicted by Treatment and %lowland
LYNzinb2 <- glmmTMB(Lynx~Treatment + low500 + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 3: Lynx detections best predicted by an interaction between Treatment and %lowland
LYNzinb3 <- glmmTMB(Lynx~Treatment*low500 + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 4: Lynx detections best predicted by Treatment, %lowland, and SnowDays
LYNzinb4 <- glmmTMB(Lynx~Treatment + low500 + SnowDays + (1| Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 5: Lynx detections best predicted by Treatment and SnowDays
LYNzinb5 <- glmmTMB(Lynx~Treatment + SnowDays + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 6: Lynx detections best predicted by SnowDays
LYNzinb6 <- glmmTMB(Lynx~SnowDays + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

## Model 7: Lynx detections best predicted by %lowland
LYNzinb7 <- glmmTMB(Lynx~low500 + (1|Site) + (1|Month), zi = ~1, data = dat, family = nbinom2)

modnames <- c("Null", "Treat + Low500 + Snow", "Treat + Snow", "Snow", "Low500")
LYNtab <- ICtab(LYNzinb0,LYNzinb4,LYNzinb5,LYNzinb6,LYNzinb7, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
LYNtab

lrtest(LYNzinb0, LYNzinb6)
summary(LYNzinb0)

### Total number of detections across 2 deployments
detect <- na.omit(dat[ ,5:11])
detect <- as.data.frame(t(detect))
detect$Total <- apply(detect[ , 1:627], 1, sum)
SpSums <- cbind.data.frame(rownames(detect), detect$Total)

