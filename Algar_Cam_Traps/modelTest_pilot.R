############################################
# modelTest_pilot.R
# Model testing with data up to April 2017
# Started Jan. 28, 2018 by Erin T.
############################################

library(lme4)
library(MASS)
library(AICcmodavg)
library(ggplot2)

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

### Try ZINBmer one more time...
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