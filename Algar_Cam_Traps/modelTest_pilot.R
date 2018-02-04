############################################
# modelTest_pilot.R
# Model testing with data up to April 2017
# Started Jan. 28, 2018 by Erin T.
############################################

library(lme4)
library(MASS)
library(AICcmodavg)
library(ggplot2)
library(lmtest)

### Modelling with glmmTMB
library(glmmTMB)
library(bbmle) #AICtab function

getwd()

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
dat <- read.csv("monthlydetections_nov2015-apr2017.csv") # First 2 deployments monthly detection data + snow days
head(dat)
dat$X <- NULL
head(dat)

#Loading % lowland data
low <- read.csv("Lowlandcover_9buffersizes.csv")
# Forgot to include 750m scale. Not really imp. as there is little diff. across scales
prop4areas <- cbind.data.frame(low$Prop250, low$Prop500, low$Prop1000, low$Prop2000)
colnames(prop4areas) <- c("250m", "500m", "1000m", "2000m")
prop4areas$Station <- low$CamStation
summary(prop4areas) #approximately the same

##Add 250m and 500m to monthly detections
dat$low250 <- low$Prop250[match(dat$Site, low$CamStation)]
dat$low500 <- low$Prop500[match(dat$Site, low$CamStation)]

# Update CSV with lowland data
write.csv(dat, "monthlydetections_nov2015-apr2017.csv")

### Try ZINBmer with glmmADMB one more time...
library(glmmADMB)
# need to omit na values
d1 <- na.omit(dat)

ZINBmer.wolf <- glmmadmb(Wolf~Treatment + (1| Site), data = d1, family = "nbinom", link = "log", zeroInflation = TRUE) ## Same error as before


#### Wolf Model 0: null glmm with neg. binomial distribution. ####
m0.wolf <- glmer.nb(Wolf~1 + (1|Site), data = dat)
summary(m0.wolf)

#### Wolf Model 1: Treatment GLMM
m1.wolf <- glmer.nb(Wolf~Treatment + (1|Site), data = dat)
summary(m1.wolf)


#### Wolf Model 2: Treatment + %lowland 500m GLMM
m2.wolf <- glmer.nb(Wolf~Treatment + low500 + (1|Site), data = dat)
summary(m2.wolf) #Human use sig. different from Control

#### Wolf Model 3: Interaction between treatment and low
m3.wolf <- glmer.nb(Wolf~ Treatment*low500 + (1|Site), data = dat)
summary(m3.wolf) #Model failed to converge with max|grad....


### Testing convergence issues
relgrad <- with(m3.wolf@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad)) #3.3935e-06

## Will re-fit with current parameter estimates set (suggested by lme4 documentation)

m3.wolf@theta #current estimate = 1.15

m3.wolf <- glmer.nb(Wolf~ Treatment*low500 + (1|Site), data = dat, start = m3.wolf@theta) #same convergence error
summary(m3.wolf)


#Still failing to converge, same results
# Try new optimizer using control = glmerControl() argument
?glmerControl
m3.wolf <- glmer.nb(Wolf~ Treatment*low500 + (1|Site), data = dat, control = glmerControl(optimizer = "Nelder_Mead")) #Same warning
summary(m3.wolf) # Consistent results. lme4 documentation indicates there is not really a problem



#### Wolf Model 4: Treat +%lowland 500m + Snow
m4.wolf <- glmer.nb(Wolf~Treatment + low500 + SnowDays + (1|Site), data = dat)
summary(m4.wolf)

#### Wolf Model 5: Wolf detections vary as a function of treatment and snow
m5.wolf <- glmer.nb(Wolf~Treatment + SnowDays + (1|Site), data = dat)

#### Wolf Model 6: vary as a function of snow only
m6.wolf <- glmer.nb(Wolf~SnowDays + (1|Site), data = dat)




## Model Selection with AICC tables
cand.set.wolf <- c(m0.wolf, m1.wolf, m2.wolf, m3.wolf, m4.wolf, m5.wolf, m6.wolf)
names <- c("NULL", "TREAT", "TREAT+LOW", "TREAT+LOW INTERACT", "TREAT+LOW+SNOW", "TREAT+SNOW", "SNOW")
aictab(cand.set.wolf, modnames = names, second.ord = TRUE, nobs = NULL,
       sort = TRUE)

# Model selection based on AICc:
  
#                     K   AICc Delta_AICc AICcWt Cum.Wt      LL
# SNOW                4 734.26       0.00   0.40   0.40 -363.10
# TREAT+LOW+SNOW      8 734.61       0.35   0.34   0.74 -359.19
# TREAT+SNOW          7 735.17       0.91   0.25   0.99 -360.49
# NULL                3 743.17       8.91   0.00   0.99 -368.57
# TREAT+LOW           7 743.97       9.72   0.00   1.00 -364.90
# TREAT               6 744.72      10.46   0.00   1.00 -366.29
# TREAT+LOW INTERACT 10 748.05      13.79   0.00   1.00 -363.84  

summary(m6.wolf)
summary(m4.wolf)

#### Attempt 2: ZINBglmms with glmmTMB for Wolf data ####

## Model 0: Null, wolf detections best predicted by themselves
wzinb0 <- glmmTMB(Wolf~1 + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model 1: Wolf detections best predicted by Treatment
wzinb1 <- glmmTMB(Wolf~Treatment + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Model 2: Wolf detections best predicted by Treatment and %lowland
wzinb2 <- glmmTMB(Wolf~Treatment + low500 + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Model 3: Wolf detections best predicted by an interaction between Treatment and %lowland
wzinb3 <- glmmTMB(Wolf~Treatment*low500 + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Model 4: Wolf detections best predicted by Treatment, %lowland, and SnowDays
wzinb4 <- glmmTMB(Wolf~Treatment + low500 + SnowDays + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Model 5: Wolf detections best predicted by Treatment and SnowDays
wzinb5 <- glmmTMB(Wolf~Treatment + SnowDays + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model 6: Wolf detections best predicted by SnowDays
wzinb6 <- glmmTMB(Wolf~SnowDays + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model 7: Wolf detections best predicted by %lowland
wzinb7 <- glmmTMB(Wolf~low500 + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model selection with AICctab (bbmle)
AICctab(wzinb0,wzinb1, wzinb2,wzinb3,wzinb4,wzinb5,wzinb6,wzinb7)

## LRT tests comparing top models (lmtest)
lrtest(wzinb6,wzinb4)
#     Df  LogLik Df  Chisq Pr(>Chisq)
# 1   5 -361.64                     
# 2   9 -357.87  4 7.5371     0.1101


lrtest(wzinb6,wzinb5)
#     Df  LogLik Df  Chisq Pr(>Chisq)
# 1   5 -361.64                     
# 2   8 -359.08  3 5.1217     0.163
# Indicates that one does not have significantly higher explanatory power over another
#According to Ockham's razor, we then select the most parsimonious (Snow)

summary(wzinb4)
#fit intercept-free
wzinb4.i0 <- glmmTMB(Wolf~Treatment + low500 + SnowDays + (1| Site)-1, zi = ~1, data = dat, family = nbinom2)
summary(wzinb4.i0)
summary(wzinb5)
summary(wzinb6)

modnames <- c("Null", "Treat", "Treat + Low500", "Treat*Low500", "Treat + Low500 + Snow", "Treat + Snow", "Snow", "Low500" )
wolftab <- ICtab(wzinb0,wzinb1, wzinb2,wzinb3,wzinb4,wzinb5,wzinb6,wzinb7, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
wolftab



##### Black bear Models: zero-inflated NB glmms ####
library(glmmADMB) # if not already loaded
# Need to first omit NAs from data
d1 <- na.omit(dat)

# Null model: Black bear detections not predicted by covariates

m0.bear <- glmmadmb(Blackbear~1 + (1| Site), data = d1, family = "nbinom", link = "log", zeroInflation = TRUE)

# Model 1: Treatment
m0.bear <- glmmadmb(Blackbear~Treatment + (1| Site), data = d1, family = "nbinom", link = "log", zeroInflation = TRUE) ## Same issue I was having with wolf ZINBglmms before

#### Run all models as glmm.nb to start ####
## Blackbear Model 0: null glmm with neg. binomial distribution. ##
m0.Blackbear <- glmer.nb(Blackbear~1 + (1|Site), data = dat)
summary(m0.Blackbear)

## Blackbear Model 1: Treatment GLMM
m1.Blackbear <- glmer.nb(Blackbear~Treatment + (1|Site), data = dat)
summary(m1.Blackbear)


## Blackbear Model 2: Treatment + %lowland 500m GLMM
m2.Blackbear <- glmer.nb(Blackbear~Treatment + low500 + (1|Site), data = dat)
summary(m2.Blackbear) # 14 warnings: Suggests re-scaling variables
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  ... :
#               Model is nearly unidentifiable: large eigenvalue ratio
#             - Rescale variables?

## Blackbear Model 3: Interaction between treatment and low
m3.Blackbear <- glmer.nb(Blackbear~ Treatment*low500 + (1|Site), data = dat)
#21 warnings:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  ... :
#                   Hessian is numerically singular: parameters are not uniquely determined


#### Blackbear Model 4: Treat + low500 + Snow
m4.Blackbear <- glmer.nb(Blackbear~ Treatment + low500 + SnowDays  + (1|Site), data = dat)
# 23 warnings, same as m3.Blackbear

#### Blackbear Model 5: Treat + Snow
m5.Blackbear <- glmer.nb(Blackbear~ Treatment + SnowDays  + (1|Site), data = dat)
# 30 warnings, same as m3 and m4

#### Blackbear Model 6: Snow
m6.Blackbear <- glmer.nb(Blackbear~ SnowDays  + (1|Site), data = dat)

#15 warnings:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  ... :
#               Model failed to converge with max|grad| = 0.0260832 (tol = 0.001, component 1)

### Testing convergence issues
relgrad <- with(m6.Blackbear@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad)) #4.9952e-05 Likely only a small issue

## Model Selection with AICC tables
cand.set.Blackbear <- c(m0.Blackbear, m1.Blackbear, m2.Blackbear, m3.Blackbear, m4.Blackbear, m5.Blackbear, m6.Blackbear)
names <- c("NULL", "TREAT", "TREAT+LOW", "TREAT+LOW INTERACT", "TREAT+LOW+SNOW", "TREAT+SNOW", "SNOW")
aictab(cand.set.Blackbear, modnames = names, second.ord = TRUE, nobs = NULL,
       sort = TRUE)
#### Attempt 2: ZINBglmms with glmmTMB for Blackbear data ####

## Model 0: Null, Blackbear detections best predicted by themselves
bbzinb0 <- glmmTMB(Blackbear~1 + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model 1: Blackbear detections best predicted by Treatment
bbzinb1 <- glmmTMB(Blackbear~Treatment + (1| Site), zi = ~1, data = dat, family = nbinom2)
#Warning messages:
#  1: In fitTMB(TMBStruc) :
#  Model convergence problem; extreme or very small eigen values detected. See vignette('troubleshooting')
# 2: In fitTMB(TMBStruc) :
#  Model convergence problem; singular convergence (7). See vignette('troubleshooting')

## Test with alternative underlying distribution (nbinom1 or compois)
bbzinb1.nbinom1 <- glmmTMB(Blackbear~Treatment + (1| Site), zi = ~1, data = dat, family = nbinom1)
#worked

## Model 2: Blackbear detections best predicted by Treatment and %lowland
bbzinb2 <- glmmTMB(Blackbear~Treatment + low500 + (1| Site), zi = ~1, data = dat, family = nbinom2)
#worked

## Model 3: Blackbear detections best predicted by an interaction between Treatment and %lowland
bbzinb3 <- glmmTMB(Blackbear~Treatment*low500 + (1| Site), zi = ~1, data = dat, family = nbinom2)

#same warning as above, try new distribution
bbzinb3.nbinom1 <- glmmTMB(Blackbear~Treatment*low500 + (1| Site), zi = ~1, data = dat, family = nbinom1) #same warning
bbzinb3.compois <- glmmTMB(Blackbear~Treatment*low500 + (1| Site), zi = ~1, data = dat, family = compois)
## compois takes forever to run. Troubleshoot other models first

## Model 4: Blackbear detections best predicted by Treatment, %lowland, and SnowDays
bbzinb4 <- glmmTMB(Blackbear~Treatment + low500 + SnowDays + (1| Site), zi = ~1, data = dat, family = nbinom2)
summary(bbzinb4)
# Warning messages:
#  1: In fitTMB(TMBStruc) :
#  Model convergence problem; non-positive-definite Hessian matrix. See vignette('troubleshooting')
# 2: In fitTMB(TMBStruc) :
#  Model convergence problem; singular convergence (7). See vignette('troubleshooting')

#Model is overparameterized, data does not contain enough information to estimate parameters

## Model 5: Blackbear detections best predicted by Treatment and SnowDays
bbzinb5 <- glmmTMB(Blackbear~Treatment + SnowDays + (1|Site), zi = ~1, data = dat, family = nbinom2)

bbzinb5.nbinom1 <- glmmTMB(Blackbear~Treatment + SnowDays + (1|Site), zi = ~1, data = dat, family = nbinom1)
# worked

## Model 6: Blackbear detections best predicted by SnowDays
bbzinb6 <- glmmTMB(Blackbear~SnowDays + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model 7: Blackbear detections best predicted by %lowland
bbzinb7 <- glmmTMB(Blackbear~low500 + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model selection with AICctab (bbmle)
## Model selection including those with convergence issues
AICctab(bbzinb0,bbzinb1, bbzinb1.nbinom1, bbzinb2,bbzinb3,bbzinb3.nbinom1,bbzinb4,bbzinb5,bbzinb5.nbinom1,bbzinb6,bbzinb7)
#                 dAICc df
# bbzinb5           0.0 8 
# bbzinb6           3.6 5 
# bbzinb5.nbinom1   4.2 8 
# bbzinb2         128.0 8 
# bbzinb3         132.3 11
# bbzinb1         141.1 7 
# bbzinb3.nbinom1 141.7 11
# bbzinb1.nbinom1 148.2 7 
# bbzinb7         155.2 5 
# bbzinb0         159.6 4 
# bbzinb4            NA 9

## Model selection excluding those with convergence issues
AICctab(bbzinb0,bbzinb1.nbinom1, bbzinb2, bbzinb5.nbinom1, bbzinb6, bbzinb7)

#               dAICc df
# bbzinb6           0.0 5 
# bbzinb5.nbinom1   0.6 8 
# bbzinb2         124.4 8 
# bbzinb1.nbinom1 144.6 7 
# bbzinb7         151.5 5 
# bbzinb0         156.0 4 

lrtest(bbzinb6, bbzinb5.nbinom1)
summary(bbzinb5.nbinom1)

#### Compare NBglmms from lme4 to  ZINBglmms from glmmTMB (including those with warnings)
AICctab(bbzinb0,bbzinb1.nbinom1, bbzinb2, bbzinb5.nbinom1, bbzinb6, bbzinb7, 
        m0.Blackbear, m1.Blackbear, m2.Blackbear, m3.Blackbear, m4.Blackbear, m5.Blackbear, m6.Blackbear)

#               dAICc df
# m4.Blackbear      0.0 8 
# m5.Blackbear     10.6 7 
# m6.Blackbear     14.1 4 
# bbzinb6          16.6 5 
# bbzinb5.nbinom1  17.2 8 
# m2.Blackbear    139.0 7 
# bbzinb2         141.0 8 
# m3.Blackbear    143.2 10
# m1.Blackbear    152.1 6 
# bbzinb1.nbinom1 161.2 7 
# bbzinb7         168.1 5 
# m0.Blackbear    170.4 3 
# bbzinb0         172.6 4

lrtest(m4.Blackbear,m5.Blackbear)

summary(m4.Blackbear) #warnings seem to arise from NA values

d1 <- na.omit(dat)
m4.Blackbear <- glmer.nb(Blackbear~ Treatment + low500 + SnowDays  + (1|Site), data = d1)
summary(m4.Blackbear) ##Same warnings, same output


###### Modelling Blackbears with glmmTMB with underlying NB distrib. (zi=0)
m4.BBtmbNB <- glmmTMB(Blackbear~Treatment + low500 + SnowDays + (1| Site), data = dat, family = nbinom2)
## Compare to glmer.nb (ZINB with TMB did not run)
AICctab(m4.Blackbear, m4.BBtmbNB)
lrtest(m4.Blackbear, m4.BBtmbNB) #Loglik only 0.18 different, but pchisq <2.2e-16

##Compare non-ZI with ZI that ran: bbzinb5.binom1
m5.BBtmbNB <- glmmTMB(Blackbear~Treatment + SnowDays + (1| Site), data = dat, family = nbinom2)
AICctab(m5.BBtmbNB,bbzinb5.nbinom1)
lrtest(m5.BBtmbNB,bbzinb5.nbinom1)
# Difference is non-significant when run with nbinom1 on both, but when m5BBtmbNB is switched to nbinom2
# it has significantly higher explanatory power

#### NB glmms with glmmTMB for Blackbears (trying to elminate warnings) ####

## Model 0: Null, Blackbear detections best predicted by themselves
b0 <- glmmTMB(Blackbear~1 + (1|Site), data = dat, family = nbinom2)

## Model 1: Blackbear detections best predicted by Treatment
b1 <- glmmTMB(Blackbear~Treatment + (1| Site), data = dat, family = nbinom2)


## Model 2: Blackbear detections best predicted by Treatment and %lowland
b2 <- glmmTMB(Blackbear~Treatment + low500 + (1| Site), data = dat, family = nbinom2)

## Model 3: Blackbear detections best predicted by an interaction between Treatment and %lowland
b3 <- glmmTMB(Blackbear~Treatment*low500 + (1| Site), data = dat, family = nbinom2)
# same warning as previous
b3 <- glmmTMB(Blackbear~Treatment*low500 + (1| Site), data = dat, family = nbinom1) #did not fix. Leave for now

## Model 4: Blackbear detections best predicted by Treatment, %lowland, and SnowDays
b4 <- glmmTMB(Blackbear~Treatment + low500 + SnowDays + (1| Site), data = dat, family = nbinom2)

## Model 5: Blackbear detections best predicted by Treatment and SnowDays
b5 <- glmmTMB(Blackbear~Treatment + SnowDays + (1|Site), data = dat, family = nbinom2)


## Model 6: Blackbear detections best predicted by SnowDays
b6 <- glmmTMB(Blackbear~SnowDays + (1|Site), data = dat, family = nbinom2)

## Model 7: Blackbear detections best predicted by %lowland
b7 <- glmmTMB(Blackbear~low500 + (1|Site), data = dat, family = nbinom2)

AICctab(b0,b1,b2,b3,b4,b5,b6,b7) # Keeping in consideration that b3 had eigen value problems
#     dAICc df
# b4   0.0 8 
# b5  10.5 7 
# b6  14.2 4 
# b2 138.6 7 
# b3 142.8 10
# b1 151.6 6 
# b7 165.7 4 
# b0 170.2 3

lrtest(b4, b5)
# Model 1: Blackbear ~ Treatment + low500 + SnowDays + (1 | Site)
# Model 2: Blackbear ~ Treatment + SnowDays + (1 | Site)
#   #Df  LogLik Df  Chisq Pr(>Chisq)    
# 1   8 -210.76                         
# 2   7 -217.06 -1 12.587  0.0003884 ***

summary(b4)

modnames <- c("Null", "Treat", "Treat + Low500", "Treat*Low500", "Treat + Low500 + Snow", "Treat + Snow", "Snow", "Low500" )
bbtab <- ICtab(b0,b1,b2,b3,b4,b5,b6,b7, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
bbtab




#### Caribou Model 0: null glmm with neg. binomial distribution ####
m0.Caribou <- glmer.nb(Caribou~1 + (1|Site), data = dat)


#### Caribou Model 1: Treatment GLMM
m1.Caribou <- glmer.nb(Caribou~Treatment + (1|Site), data = dat)



#### Caribou Model 2: Treatment + %lowland 500m GLMM
m2.Caribou <- glmer.nb(Caribou~Treatment + low500 + (1|Site), data = dat)


#### Caribou Model 3: Treat + %lowland 500m +interactions
m3.Caribou <- glmer.nb(Caribou~Treatment + low500 + Treatment*low500 + (1|Site), data = dat)

#### Caribou Model 4: Treat +%lowland 500m + Snow
m4.Caribou <- glmer.nb(Caribou~Treatment + low500 + SnowDays + (1|Site), data = dat)

# Models failing to converge


## Model Selection with AICC tables
cand.set.Caribou <- c(m0.Caribou, m1.Caribou, m2.Caribou, m3.Caribou, m4.Caribou)
names <- c("NULL", "TREAT", "TREAT+LOW", "TREAT+LOW INTERACT", "TREAT+LOW+SNOW")
aictab(cand.set.Caribou, modnames = names, second.ord = TRUE, nobs = NULL,
       sort = TRUE)

#### Attempt 2: ZINBglmms with glmmTMB for Caribou data ####

## Model 0: Null, Caribou detections best predicted by themselves
cabzinb0 <- glmmTMB(Caribou~1 + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model 1: Caribou detections best predicted by Treatment
cabzinb1 <- glmmTMB(Caribou~Treatment + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Model 2: Caribou detections best predicted by Treatment and %lowland
cabzinb2 <- glmmTMB(Caribou~Treatment + low500 + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Model 3: Caribou detections best predicted by an interaction between Treatment and %lowland
cabzinb3 <- glmmTMB(Caribou~Treatment*low500 + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Model 4: Caribou detections best predicted by Treatment, %lowland, and SnowDays
cabzinb4 <- glmmTMB(Caribou~Treatment + low500 + SnowDays + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Model 5: Caribou detections best predicted by Treatment and SnowDays
cabzinb5 <- glmmTMB(Caribou~Treatment + SnowDays + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model 6: Caribou detections best predicted by SnowDays
cabzinb6 <- glmmTMB(Caribou~SnowDays + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model 7: Caribou detections best predicted by %lowland
cabzinb7 <- glmmTMB(Caribou~low500 + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model selection with AICctab (bbmle)
AICctab(cabzinb0,cabzinb1, cabzinb2,cabzinb3,cabzinb4,cabzinb5,cabzinb6,cabzinb7)
#          dAICc df
# cabzinb4  0.0  9 
# cabzinb2  4.2  8 
# cabzinb7  4.6  5 
# cabzinb3  7.1  11
# cabzinb5 12.2  8 
# cabzinb6 14.5  5 
# cabzinb1 17.0  7 
# cabzinb0 20.5  4 

lrtest(cabzinb4,cabzinb2)
# Model 1: Caribou ~ Treatment + low500 + SnowDays + (1 | Site)
# Model 2: Caribou ~ Treatment + low500 + (1 | Site)
#     Df  LogLik Df Chisq Pr(>Chisq)  
# 1   9 -160.21                      
# 2   8 -163.32 -1 6.213    0.01268 *

summary(cabzinb4)
summary(cabzinb2)

modnames <- c("Null", "Treat", "Treat + Low500", "Treat*Low500", "Treat + Low500 + Snow", "Treat + Snow", "Snow", "Low500" )
cabtab <- ICtab(cabzinb0,cabzinb1, cabzinb2,cabzinb3,cabzinb4,cabzinb5,cabzinb6,cabzinb7, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
cabtab


##Zero-inflation model doesn't appear to be significant. Run glmmTMB with zi = 0 and compare
cabnb4 <- glmmTMB(Caribou~Treatment + low500 + SnowDays + (1|Site), data = dat, family = nbinom2)

AICctab(cabzinb4,cabnb4)
lrtest(cabzinb4,cabnb4) # Lower AIC, but not significantly different. Stick with ZINB for biological reasons
summary(cabnb4)



#### WTDeer modelling ####
#### WTDeer Model 0: null glmm with neg. binomial distribution. ####
m0.WTDeer <- glmer.nb(WTDeer~1 + (1|Site), data = dat)
summary(m0.WTDeer)

#### WTDeer Model 1: Treatment GLMM
m1.WTDeer <- glmer.nb(WTDeer~Treatment + (1|Site), data = dat)
summary(m1.WTDeer)


#### WTDeer Model 2: Treatment + %lowland 500m GLMM
m2.WTDeer <- glmer.nb(WTDeer~Treatment + low500 + (1|Site), data = dat)
summary(m2.WTDeer) #Human use sig. different from Control

#### WTDeer Model 3: Interaction between treatment and low
m3.WTDeer <- glmer.nb(WTDeer~ Treatment*low500 + (1|Site), data = dat)
summary(m3.WTDeer) #Model failed to converge with max|grad....


### Testing convergence issues
relgrad <- with(m3.WTDeer@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad)) #2.581762e-05

## Will re-fit with current parameter estimates set (suggested by lme4 documentation)

m3.WTDeer@theta 

m3.WTDeer <- glmer.nb(WTDeer~ Treatment*low500 + (1|Site), data = dat, start = m3.WTDeer@theta) #same convergence error
summary(m3.WTDeer)


#Still failing to converge, same results
# Try new optimizer using control = glmerControl() argument
?glmerControl
m3.WTDeer <- glmer.nb(WTDeer~ Treatment*low500 + (1|Site), data = dat, control = glmerControl(optimizer = "Nelder_Mead")) #Same warning
summary(m3.WTDeer) # Results very similar, but slightly different. Not sure if this is a problem or not



#### WTDeer Model 4: Treat +%lowland 500m + Snow
m4.WTDeer <- glmer.nb(WTDeer~Treatment + low500 + SnowDays + (1|Site), data = dat) #Model failed to converge with max|grad...
summary(m4.WTDeer)

### Testing convergence issues
relgrad <- with(m4.WTDeer@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad)) #2.4589e-06

## Will re-fit with current parameter estimates set (suggested by lme4 documentation)

m4.WTDeer@theta #current estimate = 1.482

m4.WTDeer <- glmer.nb(WTDeer~ Treatment*low500 + (1|Site), data = dat, start = m4.WTDeer@theta) #same convergence error, very different parameter estimates
summary(m4.WTDeer)


#Still failing to converge, same results
# Try new optimizer using control = glmerControl() argument
?glmerControl
m4.WTDeer <- glmer.nb(WTDeer~ Treatment*low500 + (1|Site), data = dat, control = glmerControl(optimizer = "Nelder_Mead")) #Same warning, estimates more similar to trial with specified starting estimate
summary(m4.WTDeer) 


#### WTDeer Model 5: WTDeer detections vary as a function of treatment and snow
m5.WTDeer <- glmer.nb(WTDeer~Treatment + SnowDays + (1|Site), data = dat)

#### WTDeer Model 6: vary as a function of snow only
m6.WTDeer <- glmer.nb(WTDeer~SnowDays + (1|Site), data = dat)




## Model Selection with AICC tables
cand.set.WTDeer <- c(m0.WTDeer, m1.WTDeer, m2.WTDeer, m3.WTDeer, m4.WTDeer, m5.WTDeer, m6.WTDeer)
names <- c("NULL", "TREAT", "TREAT+LOW", "TREAT+LOW INTERACT", "TREAT+LOW+SNOW", "TREAT+SNOW", "SNOW")
aictab(cand.set.WTDeer, modnames = names, second.ord = TRUE, nobs = NULL,
       sort = TRUE)
# Model selection based on AICc:
  
#  K   AICc Delta_AICc AICcWt Cum.Wt      LL
# TREAT+SNOW          7 888.16       0.00   0.94   0.94 -436.99
# SNOW                4 893.85       5.69   0.05   0.99 -442.89
# TREAT+LOW           7 898.67      10.51   0.00   1.00 -442.25
# TREAT+LOW+SNOW     10 904.49      16.33   0.00   1.00 -442.07
# TREAT+LOW INTERACT 10 904.49      16.33   0.00   1.00 -442.07
# TREAT               6 912.80      24.63   0.00   1.00 -450.33
# NULL                3 919.56      31.40   0.00   1.00 -456.76

# Warning message:
#  In aictab.AICglmerMod(cand.set.WTDeer, modnames = names, second.ord = TRUE,  :
                          
#                          Check model structure carefully as some models may be redundant

#### Attempt 2: ZINBglmms with glmmTMB for WTDeer data ####

## Model 0: Null, WTDeer detections best predicted by themselves
WTDzinb0 <- glmmTMB(WTDeer~1 + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model 1: WTDeer detections best predicted by Treatment
WTDzinb1 <- glmmTMB(WTDeer~Treatment + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Model 2: WTDeer detections best predicted by Treatment and %lowland
WTDzinb2 <- glmmTMB(WTDeer~Treatment + low500 + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Model 3: WTDeer detections best predicted by an interaction between Treatment and %lowland
WTDzinb3 <- glmmTMB(WTDeer~Treatment*low500 + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Model 4: WTDeer detections best predicted by Treatment, %lowland, and SnowDays
WTDzinb4 <- glmmTMB(WTDeer~Treatment + low500 + SnowDays + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Model 5: WTDeer detections best predicted by Treatment and SnowDays
WTDzinb5 <- glmmTMB(WTDeer~Treatment + SnowDays + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model 6: WTDeer detections best predicted by SnowDays
WTDzinb6 <- glmmTMB(WTDeer~SnowDays + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model 7: WTDeer detections best predicted by %lowland
WTDzinb7 <- glmmTMB(WTDeer~low500 + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model selection with AICctab (bbmle)
AICctab(WTDzinb0,WTDzinb1, WTDzinb2,WTDzinb3,WTDzinb4,WTDzinb5,WTDzinb6,WTDzinb7)
#         dAICc df
# WTDzinb4  0.0  9 
# WTDzinb5 12.8  8 
# WTDzinb6 18.4  5 
# WTDzinb2 23.5  8 
# WTDzinb7 23.8  5 
# WTDzinb3 29.3  11
# WTDzinb1 37.7  7 
# WTDzinb0 44.3  4 

lrtest(WTDzinb4, WTDzinb5)
# Model 1: WTDeer ~ Treatment + low500 + SnowDays + (1 | Site)
# Model 2: WTDeer ~ Treatment + SnowDays + (1 | Site)
#     Df  LogLik Df  Chisq Pr(>Chisq)    
# 1   9 -430.01                         
# 2   8 -437.42 -1 14.818  0.0001184 ***

summary(WTDzinb4)

modnames <- c("Null", "Treat", "Treat + Low500", "Treat*Low500", "Treat + Low500 + Snow", "Treat + Snow", "Snow", "Low500" )
WTDtab <- ICtab(WTDzinb0,WTDzinb1, WTDzinb2,WTDzinb3,WTDzinb4,WTDzinb5,WTDzinb6,WTDzinb7, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
WTDtab


#### Moose modelling (glmmTMB only) ####
## Model 0: Null, Moose detections best predicted by themselves
MOOzinb0 <- glmmTMB(Moose~1 + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model 1: Moose detections best predicted by Treatment
MOOzinb1 <- glmmTMB(Moose~Treatment + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Model 2: Moose detections best predicted by Treatment and %lowland
MOOzinb2 <- glmmTMB(Moose~Treatment + low500 + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Model 3: Moose detections best predicted by an interaction between Treatment and %lowland
MOOzinb3 <- glmmTMB(Moose~Treatment*low500 + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Model 4: Moose detections best predicted by Treatment, %lowland, and SnowDays
MOOzinb4 <- glmmTMB(Moose~Treatment + low500 + SnowDays + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Model 5: Moose detections best predicted by Treatment and SnowDays
MOOzinb5 <- glmmTMB(Moose~Treatment + SnowDays + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model 6: Moose detections best predicted by SnowDays
MOOzinb6 <- glmmTMB(Moose~SnowDays + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model 7: Moose detections best predicted by %lowland
MOOzinb7 <- glmmTMB(Moose~low500 + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Model selection with AICctab (bbmle)
AICctab(MOOzinb0,MOOzinb1, MOOzinb2,MOOzinb3,MOOzinb4,MOOzinb5,MOOzinb6,MOOzinb7)
#         dAICc df
# MOOzinb0  0.0  4 
# MOOzinb6  0.3  5 
# MOOzinb7  1.7  5 
# MOOzinb1  4.8  7 
# MOOzinb5  5.3  8 
# MOOzinb2  6.6  8 
# MOOzinb3  7.0  11
# MOOzinb4  7.2  9 

# Top model with treatment: Moose~Treatment
summary(MOOzinb1)

# Compare top 2
lrtest(MOOzinb0, MOOzinb6)
lrtest(MOOzinb6, MOOzinb7)
# Model 1: Moose ~ SnowDays + (1 | Site)
# Model 2: Moose ~ low500 + (1 | Site)
#   #Df  LogLik Df  Chisq Pr(>Chisq)    
# 1   5 -219.75                         
# 2   5 -220.41  0 1.3173  < 2.2e-16 ***

summary(MOOzinb0)
summary(MOOzinb6)

MOOtab <- ICtab(MOOzinb0,MOOzinb1, MOOzinb2,MOOzinb3,MOOzinb4,MOOzinb5,MOOzinb6,MOOzinb7, mnames = modnames, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)
MOOtab
