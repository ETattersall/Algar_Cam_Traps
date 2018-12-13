####################################
## Ch3_SpatialOverlap.R
### Modelling spatial overlap of predators across Algar area
## Started on 12 December, 2018
##################################

library(ggplot2)
library(bbmle)
library(glmmTMB)
library(dplyr)
library(tidyr)
library(camtrapR)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

### 1. Read in necessary data: Record tables, deployment data (for active days) habitat variables, linear density
rec <- read.csv("AlgarRecordTable_nov2015-apr2018.csv")
dep <- read.csv("Station_data/AlgarStations_DeploymentData.csv")
hab <- read.csv("Algar_HabitatData_8scales.csv")
lin <- read.csv("AlgarStationLineDensity_8scales.csv")

str(rec)
str(dep)

### 2. Create Site by Species matrix of relevant species
S <- as.data.frame.matrix(table(rec$Station, rec$Species))
## Remove offline sites
S <- S[ 1:60, ]

summary(S) ## Change to one word common names; Need A. alces, C.latrans, C.lupus, L. americanus, L. canadensis, O. virginianus, R. tarandus, U.americanus
S$Station <- row.names(S)
S$Station <- as.factor(S$Station)
str(S)
colnames(S) <- c("Moose", "Birdspp", "Coyote", "Wolf", "Beaver","SandhillCrane", "Wolverine", "Human", "Hare", "Otter", "Lynx", "Marten", "Fisher", "WTDeer", "Cougar", "Caribou", "Squirrel", "Blackbear_response", "Fox", "Station")
str(S)

## Select relevant spp.
S2 <- S %>% select(Station, Moose,Coyote,Wolf,Hare,Lynx,WTDeer,Caribou,Squirrel,Blackbear)
str(S2)

## Create 'Prey' for all 3
S2$Prey_Blackbear <- S2$Moose + S2$WTDeer + S2$Caribou
S2$Prey_Lynx <- S2$Hare + S2$Squirrel
S2$Prey_Coyote <- S2$Hare +S2$Squirrel + S2$WTDeer



### 3. Create camera operability matrix to calculate active days
seiscams <- dep[1:60,]
camEff0 <- cameraOperation(seiscams, 
                           stationCol = "CamStation", 
                           setupCol = "setup_date", 
                           retrievalCol = "retrieval_date",
                           hasProblems = TRUE,
                           dateFormat = "%d/%m/%Y", 
                           writecsv = FALSE)
class(camEff0)
glimpse(camEff0)
camEff0[camEff0 == 0] <- NA # changes all 0 to NA, as 0 is not operational

## Calculate ActiveDays by summing each row (station) 
ActiveDays <- rowSums(camEff0, na.rm = TRUE)
ActiveDays <- cbind.data.frame(row.names(camEff0), ActiveDays)
colnames(ActiveDays) <- c("Station", "ActiveDays")
str(ActiveDays)

## Add ActiveDays to S2
S2$ActiveDays <- ActiveDays$ActiveDays[match(S2$Station, ActiveDays$Station)]

str(S2)

## 4. Calculate RA (# detections per day)
RelAbund <- function(detect, days){
  RelAbund <- detect/days
  print(RelAbund)
}
RA <- as.data.frame(lapply(S2[, 2:13], RelAbund, S2[,14])) ## Checked for functionality --> works
## Add Stations to RAs
RA$Station <- S2$Station
str(RA)
RA <- RA %>% select(Station, Moose, Coyote, Wolf, Hare, Lynx, WTDeer, Caribou, Squirrel, Blackbear, Prey_Blackbear, Prey_Lynx, Prey_Coyote)
str(RA) ## Columns re-ordered so station is FIRST

### 5. Add Habitat and line density data and scale all variables
str(hab) ## habitat data at 8 scales for all 60 sites
str(lin) #LD as km/km2 at 8 scales for all 60 sites
hab$X <- NULL
lin$X <- NULL

RA2 <- cbind.data.frame(RA,hab,lin)
str(RA2)
RA2$CamStation <- NULL ## only removed one, need to run twice
 ## Remove extra station data
RA2[, 14:18] <- NULL
str(RA2)
hist(RA2$Lynx)

## Scaling variables --> EVERYTHING except Station
## covscale function
covscale <- function(covariate){
  centre <- (covariate - mean(covariate, na.rm = TRUE)) # where covariate is a vector of numeric input values for one covariate
  standardize <- centre/(2*sd(covariate, na.rm = TRUE)) # Divide centred value by 2 SD
}

RA3 <- cbind.data.frame(RA2$Station, lapply(RA2[,2:73], covscale))

summary(RA3) #Centred around 0

## Re-add Lynx, Coyote, and Blackbear_response from RA2 (not centred) for response variable
RA3$Lynx_response <- RA2$Lynx
RA3$Blackbear_response <- RA2$Blackbear
RA3$Coyote_response <- RA2$Coyote
str(RA3)

### 6. Determine Distributions for response variables

hist(RA3$Blackbear_response)
hist(RA3$Lynx_response)
hist(RA3$Coyote_response)

## Testing for Overdispersion
glmBB0 <- glmmTMB(Blackbear_response~LowCon250 + UpCon250 + UpDecid250 + Tamarack250 + pOpen250, data = RA3, family = poisson)
summary(glmBB0)
glmBB1 <- glmmTMB(Blackbear_response~LowCon250 + UpCon250 + UpDecid250 + Tamarack250 + pOpen250, data = RA3, family = nbinom1)
glmBB2 <- glmmTMB(Blackbear_response~LowCon250 + UpCon250 + UpDecid250 + Tamarack250 + pOpen250, data = RA3, family = nbinom2)
ICtab(glmBB0,glmBB1,glmBB2)
## Poisson fits best for black bear, which is most dispersed of the 3 --> assume Poisson


## Proportions of zeroes (testing for zero-inflation)
sum(RA3$Blackbear_response==0, na.rm = TRUE)/nrow(RA3) ## 0.26
sum(RA3$Lynx_response==0, na.rm = TRUE)/nrow(RA3) ## 0.55
sum(RA3$Coyote_response==0, na.rm = TRUE)/nrow(RA3) ## 0.62

## Coyote and Lynx may be zero-inflated. For simplicity of analyses, will model as poisson, non-ZI

### 7. Scale Analysis --> GLMs comparing Predator relative abundance as a function of habitat variables

## A. Black bear
Bhab0 <- glmmTMB(Blackbear_response~1, data = RA3, family = poisson)
Bhab250 <- glmmTMB(Blackbear_response~LowCon250 + UpCon250 + UpDecid250 + Tamarack250 + pOpen250, data = RA3, family = poisson)
Bhab500 <- glmmTMB(Blackbear_response~LowCon500 + UpCon500 + UpDecid500 + Tamarack500 + LowDecid500 + pOpen500, data = RA3, family = poisson)
Bhab750 <- glmmTMB(Blackbear_response~LowCon750 + UpCon750 + UpDecid750 + Tamarack750 + LowDecid750 + pOpen750, data = RA3, family = poisson)
Bhab1000 <- glmmTMB(Blackbear_response~LowCon1000 + UpCon1000 + UpDecid1000 + Tamarack1000 + LowDecid1000 + pOpen1000, data = RA3, family = poisson)
Bhab1250 <- glmmTMB(Blackbear_response~LowCon1250 + UpCon1250 + UpDecid1250 + Tamarack1250 + LowDecid1250 + pOpen1250, data = RA3, family = poisson)
Bhab1500 <- glmmTMB(Blackbear_response~LowCon1500 + UpCon1500 + UpDecid1500 + Tamarack1500 + LowDecid1500 + pOpen1500, data = RA3, family = poisson)
Bhab1750 <- glmmTMB(Blackbear_response~LowCon1750 + UpCon1750 + UpDecid1750 + Tamarack1750 + LowDecid1750 + pOpen1750, data = RA3, family = poisson)
Bhab2000 <- glmmTMB(Blackbear_response~LowCon2000 + UpCon2000 + UpDecid2000 + Tamarack2000 + LowDecid2000 + pOpen2000, data = RA3, family = poisson)

BB.scale <- ICtab(Bhab0, Bhab250, Bhab500, Bhab750, Bhab1000, Bhab1250, Bhab1500, Bhab1750, Bhab2000, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

BB.scale
##          dLogLik dAIC df weight
#  Bhab0     0.0     0.0 1  0.9721
#  Bhab250   0.2     9.6 6  0.0080
#  Bhab500   0.2    11.6 7  0.0030
#  Bhab750   0.2    11.6 7  0.0029
#  Bhab1000  0.2    11.7 7  0.0028
#  Bhab1750  0.2    11.7 7  0.0028
#  Bhab1500  0.1    11.7 7  0.0028
#  Bhab1250  0.1    11.7 7  0.0028
#  Bhab2000  0.1    11.7 7  0.0028

## B. Lynx
Lynxhab0 <- glmmTMB(Lynx_response~1, data = RA3, family = 'poisson')
Lynxhab250 <- glmmTMB(Lynx_response~LowCon250 + UpCon250 + UpDecid250 + Tamarack250 + pOpen250, data = RA3, family = poisson)
Lynxhab500 <- glmmTMB(Lynx_response~LowCon500 + UpCon500 + UpDecid500 + Tamarack500 + LowDecid500 + pOpen500, data = RA3, family = poisson)
Lynxhab750 <- glmmTMB(Lynx_response~LowCon750 + UpCon750 + UpDecid750 + Tamarack750 + LowDecid750 + pOpen750, data = RA3, family = poisson)
Lynxhab1000 <- glmmTMB(Lynx_response~LowCon1000 + UpCon1000 + UpDecid1000 + Tamarack1000 + LowDecid1000 + pOpen1000, data = RA3, family = poisson)
Lynxhab1250 <- glmmTMB(Lynx_response~LowCon1250 + UpCon1250 + UpDecid1250 + Tamarack1250 + LowDecid1250 + pOpen1250, data = RA3, family = poisson)
Lynxhab1500 <- glmmTMB(Lynx_response~LowCon1500 + UpCon1500 + UpDecid1500 + Tamarack1500 + LowDecid1500 + pOpen1500, data = RA3, family = poisson)
Lynxhab1750 <- glmmTMB(Lynx_response~LowCon1750 + UpCon1750 + UpDecid1750 + Tamarack1750 + LowDecid1750 + pOpen1750, data = RA3, family = poisson)
Lynxhab2000 <- glmmTMB(Lynx_response~LowCon2000 + UpCon2000 + UpDecid2000 + Tamarack2000 + LowDecid2000 + pOpen2000, data = RA3, family = poisson)

Lynx.scale <- ICtab(Lynxhab0, Lynxhab250, Lynxhab500, Lynxhab750, Lynxhab1000, Lynxhab1250, Lynxhab1500, Lynxhab1750, Lynxhab2000, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

Lynx.scale
##           dLogLik dAIC df weight
# Lynxhab0     0.0     0.0 1  0.9754
# Lynxhab250   0.0     9.9 6  0.0069
# Lynxhab1500  0.1    11.9 7  0.0025
# Lynxhab1750  0.1    11.9 7  0.0025
# Lynxhab2000  0.1    11.9 7  0.0025
# Lynxhab1250  0.0    11.9 7  0.0025
# Lynxhab1000  0.0    11.9 7  0.0025
# Lynxhab500   0.0    11.9 7  0.0025
# Lynxhab750   0.0    11.9 7  0.0025

## B. Coyote
Coyotehab0 <- glmmTMB(Coyote_response~1, data = RA3, family = 'poisson')
Coyotehab250 <- glmmTMB(Coyote_response~LowCon250 + UpCon250 + UpDecid250 + Tamarack250 + pOpen250, data = RA3, family = poisson)
Coyotehab500 <- glmmTMB(Coyote_response~LowCon500 + UpCon500 + UpDecid500 + Tamarack500 + LowDecid500 + pOpen500, data = RA3, family = poisson)
Coyotehab750 <- glmmTMB(Coyote_response~LowCon750 + UpCon750 + UpDecid750 + Tamarack750 + LowDecid750 + pOpen750, data = RA3, family = poisson)
Coyotehab1000 <- glmmTMB(Coyote_response~LowCon1000 + UpCon1000 + UpDecid1000 + Tamarack1000 + LowDecid1000 + pOpen1000, data = RA3, family = poisson)
Coyotehab1250 <- glmmTMB(Coyote_response~LowCon1250 + UpCon1250 + UpDecid1250 + Tamarack1250 + LowDecid1250 + pOpen1250, data = RA3, family = poisson)
Coyotehab1500 <- glmmTMB(Coyote_response~LowCon1500 + UpCon1500 + UpDecid1500 + Tamarack1500 + LowDecid1500 + pOpen1500, data = RA3, family = poisson)
Coyotehab1750 <- glmmTMB(Coyote_response~LowCon1750 + UpCon1750 + UpDecid1750 + Tamarack1750 + LowDecid1750 + pOpen1750, data = RA3, family = poisson)
Coyotehab2000 <- glmmTMB(Coyote_response~LowCon2000 + UpCon2000 + UpDecid2000 + Tamarack2000 + LowDecid2000 + pOpen2000, data = RA3, family = poisson)

Coyote.scale <- ICtab(Coyotehab0, Coyotehab250, Coyotehab500, Coyotehab750, Coyotehab1000, Coyotehab1250, Coyotehab1500, Coyotehab1750, Coyotehab2000, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

Coyote.scale
##              dLogLik dAIC df weight
# Coyotehab0     0.0     0.0 1  0.9726
# Coyotehab250   0.1     9.7 6  0.0075
# Coyotehab1750  0.2    11.6 7  0.0030
# Coyotehab1500  0.2    11.6 7  0.0029
# Coyotehab2000  0.2    11.6 7  0.0029
# Coyotehab1250  0.2    11.7 7  0.0029
# Coyotehab1000  0.2    11.7 7  0.0028
# Coyotehab750   0.1    11.7 7  0.0027
# Coyotehab500   0.1    11.8 7  0.0027

### Using null model for all three

### 8. Linear density

BlackbearLD0 <- glmmTMB(Blackbear_response~1, data = RA3, family = poisson)
BlackbearLD250 <- glmmTMB(Blackbear_response~X250m, data = RA3, family = poisson)
BlackbearLD500 <- glmmTMB(Blackbear_response~X500m, data = RA3, family = poisson)
BlackbearLD750 <- glmmTMB(Blackbear_response~X750m, data = RA3, family = poisson)
BlackbearLD1000 <- glmmTMB(Blackbear_response~X1000m, data = RA3, family = poisson)
BlackbearLD1250 <- glmmTMB(Blackbear_response~X1250m, data = RA3, family = poisson)
BlackbearLD1500 <- glmmTMB(Blackbear_response~X1500m, data = RA3, family = poisson)
BlackbearLD1750 <- glmmTMB(Blackbear_response~X1750m, data = RA3, family = poisson)
BlackbearLD2000 <- glmmTMB(Blackbear_response~X2000m, data = RA3, family = poisson)

BlackbearLD <- ICtab(BlackbearLD0, BlackbearLD250, BlackbearLD500, BlackbearLD750, BlackbearLD1000, BlackbearLD1250, BlackbearLD1500, BlackbearLD1750, BlackbearLD2000, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

BlackbearLD
##               dLogLik dAIC df weight
# BlackbearLD0    0.0     0.0  1  0.251 
# BlackbearLD750  0.0     2.0  2  0.094 
# BlackbearLD250  0.0     2.0  2  0.094 
# BlackbearLD500  0.0     2.0  2  0.094 
# BlackbearLD1000 0.0     2.0  2  0.094 
# BlackbearLD2000 0.0     2.0  2  0.093 
# BlackbearLD1750 0.0     2.0  2  0.093 
# BlackbearLD1250 0.0     2.0  2  0.093 
# BlackbearLD1500 0.0     2.0  2  0.093 

## B. Lynx
LynxLD0 <- glmmTMB(Lynx_response~1, data = RA3, family = poisson)
LynxLD250 <- glmmTMB(Lynx_response~X250m, data = RA3, family = poisson)
LynxLD500 <- glmmTMB(Lynx_response~X500m, data = RA3, family = poisson)
LynxLD750 <- glmmTMB(Lynx_response~X750m, data = RA3, family = poisson)
LynxLD1000 <- glmmTMB(Lynx_response~X1000m, data = RA3, family = poisson)
LynxLD1250 <- glmmTMB(Lynx_response~X1250m, data = RA3, family = poisson)
LynxLD1500 <- glmmTMB(Lynx_response~X1500m, data = RA3, family = poisson)
LynxLD1750 <- glmmTMB(Lynx_response~X1750m, data = RA3, family = poisson)
LynxLD2000 <- glmmTMB(Lynx_response~X2000m, data = RA3, family = poisson)

LynxLD <- ICtab(LynxLD0, LynxLD250, LynxLD500, LynxLD750, LynxLD1000, LynxLD1250, LynxLD1500, LynxLD1750, LynxLD2000, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

LynxLD
##          dLogLik dAIC df weight
# LynxLD0    0.0     0.0  1  0.249 
# LynxLD1250 0.0     1.9  2  0.095 
# LynxLD1000 0.0     1.9  2  0.095 
# LynxLD1750 0.0     1.9  2  0.094 
# LynxLD1500 0.0     1.9  2  0.094 
# LynxLD2000 0.0     1.9  2  0.094 
# LynxLD750  0.0     1.9  2  0.094 
# LynxLD500  0.0     2.0  2  0.093 
# LynxLD250  0.0     2.0  2  0.092

## C. Coyote
CoyoteLD0 <- glmmTMB(Coyote_response~1, data = RA3, family = poisson)
CoyoteLD250 <- glmmTMB(Coyote_response~X250m, data = RA3, family = poisson)
CoyoteLD500 <- glmmTMB(Coyote_response~X500m, data = RA3, family = poisson)
CoyoteLD750 <- glmmTMB(Coyote_response~X750m, data = RA3, family = poisson)
CoyoteLD1000 <- glmmTMB(Coyote_response~X1000m, data = RA3, family = poisson)
CoyoteLD1250 <- glmmTMB(Coyote_response~X1250m, data = RA3, family = poisson)
CoyoteLD1500 <- glmmTMB(Coyote_response~X1500m, data = RA3, family = poisson)
CoyoteLD1750 <- glmmTMB(Coyote_response~X1750m, data = RA3, family = poisson)
CoyoteLD2000 <- glmmTMB(Coyote_response~X2000m, data = RA3, family = poisson)

CoyoteLD <- ICtab(CoyoteLD0, CoyoteLD250, CoyoteLD500, CoyoteLD750, CoyoteLD1000, CoyoteLD1250, CoyoteLD1500, CoyoteLD1750, CoyoteLD2000, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

CoyoteLD
##             dLogLik dAIC df weight
# CoyoteLD0    0.0     0.0  1  0.226 
# CoyoteLD1000 0.2     1.6  2  0.104 
# CoyoteLD1250 0.2     1.6  2  0.103 
# CoyoteLD1500 0.2     1.6  2  0.101 
# CoyoteLD1750 0.2     1.7  2  0.098 
# CoyoteLD750  0.2     1.7  2  0.098 
# CoyoteLD2000 0.1     1.7  2  0.096 
# CoyoteLD500  0.1     1.8  2  0.090 
# CoyoteLD250  0.0     2.0  2  0.084 

### 9. Co-occurrence modelling
## Thus far, null has been best model for all three species
## Try interaction with top LD scale?

## A. Blackbear
BlackbearCo0 <- glmmTMB(Blackbear_response~1, data = RA3, family = poisson)
BlackbearCo0LD <- glmmTMB(Blackbear_response~X750m, data = RA3, family = poisson)
BlackbearCo1 <- glmmTMB(Blackbear_response~Wolf, data = RA3, family = poisson)
BlackbearCo1LD1 <- glmmTMB(Blackbear_response~Wolf + X750m, data = RA3, family = poisson)
BlackbearCo1LD2 <- glmmTMB(Blackbear_response~Wolf * X750m, data = RA3, family = poisson)
BlackbearCo2 <- glmmTMB(Blackbear_response~Prey_Blackbear, data = RA3, family = poisson)
BlackbearCo2LD1 <- glmmTMB(Blackbear_response~Prey_Blackbear + X750m, data = RA3, family = poisson)
BlackbearCo2LD2 <- glmmTMB(Blackbear_response~Prey_Blackbear * X750m, data = RA3, family = poisson)

BlackbearCo <- ICtab(BlackbearCo0, BlackbearCo0LD, BlackbearCo1, BlackbearCo1LD1, BlackbearCo1LD2, BlackbearCo2, BlackbearCo2LD1, BlackbearCo2LD2,  type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

BlackbearCo
##           dLogLik dAIC df weight
# BlackbearCo0    0.0     0.0  1  0.382 
# BlackbearCo1    0.1     1.8  2  0.157 
# BlackbearCo2    0.1     1.8  2  0.154 
# BlackbearCo0LD  0.0     2.0  2  0.144 
# BlackbearCo1LD1 0.2     3.7  3  0.061 
# BlackbearCo2LD1 0.1     3.8  3  0.057 
# BlackbearCo1LD2 0.2     5.5  4  0.024 
# BlackbearCo2LD2 0.1     5.8  4  0.021

summary(BlackbearCo1) ## Not significant

## B. Lynx
LynxCo0 <- glmmTMB(Lynx_response~1, data = RA3, family = poisson)
LynxCo0LD <- glmmTMB(Lynx_response~X750m, data = RA3, family = poisson)
LynxCo1 <- glmmTMB(Lynx_response~Wolf, data = RA3, family = poisson)
LynxCo1LD1 <- glmmTMB(Lynx_response~Wolf + X750m, data = RA3, family = poisson)
LynxCo1LD2 <- glmmTMB(Lynx_response~Wolf * X750m, data = RA3, family = poisson)
LynxCo2 <- glmmTMB(Lynx_response~Prey_Lynx, data = RA3, family = poisson)
LynxCo2LD1 <- glmmTMB(Lynx_response~Prey_Lynx + X750m, data = RA3, family = poisson)
LynxCo2LD2 <- glmmTMB(Lynx_response~Prey_Lynx * X750m, data = RA3, family = poisson)
LynxCo3 <- glmmTMB(Lynx_response~Coyote, data = RA3, family = poisson)
LynxCo3LD1 <- glmmTMB(Lynx_response~Coyote + X750m, data = RA3, family = poisson)
LynxCo3LD2 <- glmmTMB(Lynx_response~Coyote * X750m, data = RA3, family = poisson)

LynxCo <- ICtab(LynxCo0, LynxCo0LD, LynxCo1, LynxCo1LD1, LynxCo1LD2, LynxCo2, LynxCo2LD1, LynxCo2LD2, LynxCo3, LynxCo3LD1, LynxCo3LD2, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

LynxCo
 
##          dLogLik dAIC df weight
# LynxCo0    0.0     0.0  1  0.327 
# LynxCo0LD  0.0     1.9  2  0.124 
# LynxCo3    0.0     2.0  2  0.121 
# LynxCo2    0.0     2.0  2  0.121 
# LynxCo1    0.0     2.0  2  0.121 
# LynxCo1LD1 0.0     3.9  3  0.046 
# LynxCo2LD1 0.0     3.9  3  0.045 
# LynxCo3LD1 0.0     3.9  3  0.045 
# LynxCo1LD2 0.0     5.9  4  0.017 
# LynxCo3LD2 0.0     5.9  4  0.017 
# LynxCo2LD2 0.0     5.9  4  0.017

summary(LynxCo0LD) # Not significant


## C. Coyote
CoyoteCo0 <- glmmTMB(Coyote_response~1, data = RA3, family = poisson)
CoyoteCo0LD <- glmmTMB(Coyote_response~X750m, data = RA3, family = poisson)
CoyoteCo1 <- glmmTMB(Coyote_response~Wolf, data = RA3, family = poisson)
CoyoteCo1LD1 <- glmmTMB(Coyote_response~Wolf + X750m, data = RA3, family = poisson)
CoyoteCo1LD2 <- glmmTMB(Coyote_response~Wolf * X750m, data = RA3, family = poisson)
CoyoteCo2 <- glmmTMB(Coyote_response~Prey_Coyote, data = RA3, family = poisson)
CoyoteCo2LD1 <- glmmTMB(Coyote_response~Prey_Coyote + X750m, data = RA3, family = poisson)
CoyoteCo2LD2 <- glmmTMB(Coyote_response~Prey_Coyote * X750m, data = RA3, family = poisson)
CoyoteCo3 <- glmmTMB(Coyote_response~Lynx, data = RA3, family = poisson)
CoyoteCo3LD1 <- glmmTMB(Coyote_response~Lynx + X750m, data = RA3, family = poisson)
CoyoteCo3LD2 <- glmmTMB(Coyote_response~Lynx * X750m, data = RA3, family = poisson)

CoyoteCo <- ICtab(CoyoteCo0, CoyoteCo0LD, CoyoteCo1, CoyoteCo1LD1, CoyoteCo1LD2, CoyoteCo2, CoyoteCo2LD1, CoyoteCo2LD2, CoyoteCo3, CoyoteCo3LD1, CoyoteCo3LD2, type= "AIC", weights = TRUE, delta = TRUE, logLik = TRUE, sort=TRUE)

CoyoteCo

##            dLogLik dAIC df weight
# CoyoteCo0    0.0     0.0  1  0.308 
# CoyoteCo0LD  0.2     1.7  2  0.133 
# CoyoteCo2    0.1     1.9  2  0.122 
# CoyoteCo3    0.1     1.9  2  0.120 
# CoyoteCo1    0.0     2.0  2  0.114 
# CoyoteCo2LD1 0.2     3.7  3  0.049 
# CoyoteCo3LD1 0.2     3.7  3  0.049 
# CoyoteCo1LD1 0.2     3.7  3  0.049 
# CoyoteCo1LD2 0.2     5.6  4  0.018 
# CoyoteCo2LD2 0.2     5.7  4  0.018 
# CoyoteCo3LD2 0.2     5.7  4  0.018  

summary(CoyoteCo0LD) ## Not significant
