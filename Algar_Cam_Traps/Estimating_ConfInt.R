############################
# Estimating_ConfInt.R
# Estimating confidence intervals for top models
# Started Feb. 4, 2018 by Erin T.
############################

library(ggplot2) #Plotting
library(glmmTMB) #Running glmms. Particularly useful for ZI models
library(bbmle) # Model selection tables
library(lmtest) # LRT test for comparing nested models


setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
dat <- read.csv("monthlydetections_nov2015-apr2017.csv") # First 2 deployments monthly detection data + snow days + % lowland for 250m and 500m (with cut-off AVIE data)


#### Top models, according to AIC and LRT model selection (as of Feb. 4): ####
## Wolf (Two top models, Snow and Treat+Low+Snow)
wzinb4 <- glmmTMB(Wolf~Treatment + low500 + SnowDays + (1| Site), zi = ~1, data = dat, family = nbinom2)
wzinb6 <- glmmTMB(Wolf~SnowDays + (1|Site), zi = ~1, data = dat, family = nbinom2)

## Blackbear (note: NOT ZI. ZINB had errors for overparameterization)
b4 <- glmmTMB(Blackbear~Treatment + low500 + SnowDays + (1| Site), data = dat, family = nbinom2)

## Caribou
cabzinb4 <- glmmTMB(Caribou~Treatment + low500 + SnowDays + (1| Site), zi = ~1, data = dat, family = nbinom2)

## WTD
WTDzinb4 <- glmmTMB(WTDeer~Treatment + low500 + SnowDays + (1| Site), zi = ~1, data = dat, family = nbinom2)

## Moose
MOOzinb0 <- glmmTMB(Moose~1 + (1|Site), zi = ~1, data = dat, family = nbinom2)


#### Wolves: Estimating ####
## Saving coefficients
summary(wzinb4)
wzinb4.coeff <- summary(wzinb4)$coefficients ## divided into cond and zi. Subsetting these returns individual coefficients

wzinb4.coeff$cond[1] # Estimate term for Intercept
wzinb4.coeff$cond[2] # Est. term for HumanUse
wzinb4.coeff$cond[,2] # Standard error column
wzinb4.coeff$cond[3,2] #Std. error for NatRegen
wzinb4.cond <- wzinb4.coeff$cond
wzinb4.cond[3,2] #Matches std. error for NatRegen
wzinb4.cond[1] # Matches est. for intercept
wzinb4.cond

## Estimates for wolf detections per treatment (Treatment as a discrete variable), low500 and SnowDays
## List of empty vectors to feed into loop
wolf.log = numeric(6) ## log(monthly detections)
wolf.mean = numeric(6) ## mean detections per month
UCI = numeric(6) ## Upper confidence (mean + 1.96*SE)
LCI = numeric(6) ## Lower confidence (mean - 1.96*SE)

### Filling vectors for Control treatments
## Models used a neg.binom. distrib., with log link function
wolf.log[1] = wzinb4.cond[1]
wolf.mean[1] = exp(wzinb4.cond[1])
UCI[1] = exp(wzinb4.cond[1] + 1.96*wzinb4.cond[1,2])
LCI[1] = exp(wzinb4.cond[1] - 1.96*wzinb4.cond[1,2])

## Vector of Treatments
Treatment = row.names(wzinb4.cond[2:6,])

## For loop adding elements to above vectors for other 3 treatments
for (i in 2:6){
  wolf.log[i] = wolf.log[1] + wzinb4.cond[Treatment[i-1],1] ## adding estimate per treatment to vector
  wolf.mean[i] = exp(wolf.log[i]) ## adding mean per year to vector
  Var.U <- wzinb4.cond['(Intercept)', 2]^2 ## SE of the intercept
  Var.V <- wzinb4.cond[Treatment[i-1], 2]^2 ## SE  of estimate per treatment
  Cov.UV <- vcov(wzinb4)$cond[i,i] ##Covariance between U and V (need to specify the conditional model - $cond - because ZI is a mixture model
  UCI[i] = exp(wolf.log[i] + 1.96*(sqrt(Var.U + Var.V+ 2*Cov.UV))) ##Adding upper confidence limit
  LCI[i] = exp(wolf.log[i] - 1.96*(sqrt(Var.U + Var.V + 2*Cov.UV))) ## Adding lower confidence limit
}

## Entering in data.frame
CI.wzinb4 = as.data.frame(cbind(wolf.log,wolf.mean, LCI, UCI), row.names = c("Control", "HumanUse", "NatRegen", "SPP", "Low500", "SnowDays"))

## Rudimentary plots comparing detections, Lowland habitat, and SnowDays
plot(x = dat$low500, y = dat$Wolf) #difficult to visualize, but looks like slightly more zeros at higher %low

plot(x=dat$SnowDays, y = dat$Wolf) # Looks like more detections with fewer SnowDays








