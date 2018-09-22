########################################
## Ch3_Habitat_scaleanalysis.R
## Multivariate scale analysis to determine appropriate scales of measurement for each species
## Started Sep. 21, 2018
########################################


library(tidyr)		# for data formatting functions
library(dplyr)
library(glmmTMB)
library(bbmle)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

Occ <- read.csv("Algar60_speciesOccurrence.csv")
Bears <- read.csv("Algar60_speciesOccurrence_SUMMER.csv")
Hab <- read.csv("Algar_HabitatData_8scales.csv")


#### 1. Create data frames for each scale (16 in total, since I need to duplicate separately for Bears) ####
#a. 250 m 
Occ250 <- Occ


Occ250$LowCon <- Hab$LowCon250[match(Occ250$Site, Hab$CamStation)]
Occ250$Tamarack <- Hab$Tamarack250[match(Occ250$Site, Hab$CamStation)]
Occ250$UpCon <- Hab$UpCon250[match(Occ250$Site, Hab$CamStation)]
Occ250$UpDecid <- Hab$UpDecid250[match(Occ250$Site, Hab$CamStation)]
Occ250$pOpen <- Hab$pOpen250[match(Occ250$Site, Hab$CamStation)]

##LowDecid not rep. at 250 m scale for any stations --> add variable of 0's so it can at least be included in models
Occ250$LowDecid <- rep(0,nrow(Occ))

#b. 500 m 
Occ500 <- Occ


Occ500$LowCon <- Hab$LowCon500[match(Occ500$Site, Hab$CamStation)]
Occ500$LowDecid <- Hab$LowDecid500[match(Occ500$Site, Hab$CamStation)]
Occ500$Tamarack <- Hab$Tamarack500[match(Occ500$Site, Hab$CamStation)]
Occ500$UpCon <- Hab$UpCon500[match(Occ500$Site, Hab$CamStation)]
Occ500$UpDecid <- Hab$UpDecid500[match(Occ500$Site, Hab$CamStation)]
Occ500$pOpen <- Hab$pOpen500[match(Occ500$Site, Hab$CamStation)]

#c. 750 m
Occ750 <- Occ


Occ750$LowCon <- Hab$LowCon750[match(Occ750$Site, Hab$CamStation)]
Occ750$LowDecid <- Hab$LowDecid750[match(Occ750$Site, Hab$CamStation)]
Occ750$Tamarack <- Hab$Tamarack750[match(Occ750$Site, Hab$CamStation)]
Occ750$UpCon <- Hab$UpCon750[match(Occ750$Site, Hab$CamStation)]
Occ750$UpDecid <- Hab$UpDecid750[match(Occ750$Site, Hab$CamStation)]
Occ750$pOpen <- Hab$pOpen750[match(Occ750$Site, Hab$CamStation)]

#d. 1000 m 
Occ1000 <- Occ


Occ1000$LowCon <- Hab$LowCon1000[match(Occ1000$Site, Hab$CamStation)]
Occ1000$LowDecid <- Hab$LowDecid1000[match(Occ1000$Site, Hab$CamStation)]
Occ1000$Tamarack <- Hab$Tamarack1000[match(Occ1000$Site, Hab$CamStation)]
Occ1000$UpCon <- Hab$UpCon1000[match(Occ1000$Site, Hab$CamStation)]
Occ1000$UpDecid <- Hab$UpDecid1000[match(Occ1000$Site, Hab$CamStation)]
Occ1000$pOpen <- Hab$pOpen1000[match(Occ1000$Site, Hab$CamStation)]

#e. 1250 m 
Occ1250 <- Occ


Occ1250$LowCon <- Hab$LowCon1250[match(Occ1250$Site, Hab$CamStation)]
Occ1250$LowDecid <- Hab$LowDecid1250[match(Occ1250$Site, Hab$CamStation)]
Occ1250$Tamarack <- Hab$Tamarack1250[match(Occ1250$Site, Hab$CamStation)]
Occ1250$UpCon <- Hab$UpCon1250[match(Occ1250$Site, Hab$CamStation)]
Occ1250$UpDecid <- Hab$UpDecid1250[match(Occ1250$Site, Hab$CamStation)]
Occ1250$pOpen <- Hab$pOpen1250[match(Occ1250$Site, Hab$CamStation)]

#f. 1500 m
Occ1500 <- Occ


Occ1500$LowCon <- Hab$LowCon1500[match(Occ1500$Site, Hab$CamStation)]
Occ1500$LowDecid <- Hab$LowDecid1500[match(Occ1500$Site, Hab$CamStation)]
Occ1500$Tamarack <- Hab$Tamarack1500[match(Occ1500$Site, Hab$CamStation)]
Occ1500$UpCon <- Hab$UpCon1500[match(Occ1500$Site, Hab$CamStation)]
Occ1500$UpDecid <- Hab$UpDecid1500[match(Occ1500$Site, Hab$CamStation)]
Occ1500$pOpen <- Hab$pOpen1500[match(Occ1500$Site, Hab$CamStation)]

#g. 1750 m 
Occ1750 <- Occ


Occ1750$LowCon <- Hab$LowCon1750[match(Occ1750$Site, Hab$CamStation)]
Occ1750$LowDecid <- Hab$LowDecid1750[match(Occ1750$Site, Hab$CamStation)]
Occ1750$Tamarack <- Hab$Tamarack1750[match(Occ1750$Site, Hab$CamStation)]
Occ1750$UpCon <- Hab$UpCon1750[match(Occ1750$Site, Hab$CamStation)]
Occ1750$UpDecid <- Hab$UpDecid1750[match(Occ1750$Site, Hab$CamStation)]
Occ1750$pOpen <- Hab$pOpen1750[match(Occ1750$Site, Hab$CamStation)]

#h. 2000 m 
Occ2000 <- Occ


Occ2000$LowCon <- Hab$LowCon2000[match(Occ2000$Site, Hab$CamStation)]
Occ2000$LowDecid <- Hab$LowDecid2000[match(Occ2000$Site, Hab$CamStation)]
Occ2000$Tamarack <- Hab$Tamarack2000[match(Occ2000$Site, Hab$CamStation)]
Occ2000$UpCon <- Hab$UpCon2000[match(Occ2000$Site, Hab$CamStation)]
Occ2000$UpDecid <- Hab$UpDecid2000[match(Occ2000$Site, Hab$CamStation)]
Occ2000$pOpen <- Hab$pOpen2000[match(Occ2000$Site, Hab$CamStation)]

#### Bears
#a. 250 m 
Bears250 <- Bears


Bears250$LowCon <- Hab$LowCon250[match(Bears250$Site, Hab$CamStation)]
Bears250$Tamarack <- Hab$Tamarack250[match(Bears250$Site, Hab$CamStation)]
Bears250$UpCon <- Hab$UpCon250[match(Bears250$Site, Hab$CamStation)]
Bears250$UpDecid <- Hab$UpDecid250[match(Bears250$Site, Hab$CamStation)]
Bears250$pOpen <- Hab$pOpen250[match(Bears250$Site, Hab$CamStation)]

#b. 500 m 
Bears500 <- Bears


Bears500$LowCon <- Hab$LowCon500[match(Bears500$Site, Hab$CamStation)]
Bears500$LowDecid <- Hab$LowDecid500[match(Bears500$Site, Hab$CamStation)]
Bears500$Tamarack <- Hab$Tamarack500[match(Bears500$Site, Hab$CamStation)]
Bears500$UpCon <- Hab$UpCon500[match(Bears500$Site, Hab$CamStation)]
Bears500$UpDecid <- Hab$UpDecid500[match(Bears500$Site, Hab$CamStation)]
Bears500$pOpen <- Hab$pOpen500[match(Bears500$Site, Hab$CamStation)]

#c. 750 m
Bears750 <- Bears


Bears750$LowCon <- Hab$LowCon750[match(Bears750$Site, Hab$CamStation)]
Bears750$LowDecid <- Hab$LowDecid750[match(Bears750$Site, Hab$CamStation)]
Bears750$Tamarack <- Hab$Tamarack750[match(Bears750$Site, Hab$CamStation)]
Bears750$UpCon <- Hab$UpCon750[match(Bears750$Site, Hab$CamStation)]
Bears750$UpDecid <- Hab$UpDecid750[match(Bears750$Site, Hab$CamStation)]
Bears750$pOpen <- Hab$pOpen750[match(Bears750$Site, Hab$CamStation)]

#d. 1000 m 
Bears1000 <- Bears


Bears1000$LowCon <- Hab$LowCon1000[match(Bears1000$Site, Hab$CamStation)]
Bears1000$LowDecid <- Hab$LowDecid1000[match(Bears1000$Site, Hab$CamStation)]
Bears1000$Tamarack <- Hab$Tamarack1000[match(Bears1000$Site, Hab$CamStation)]
Bears1000$UpCon <- Hab$UpCon1000[match(Bears1000$Site, Hab$CamStation)]
Bears1000$UpDecid <- Hab$UpDecid1000[match(Bears1000$Site, Hab$CamStation)]
Bears1000$pOpen <- Hab$pOpen1000[match(Bears1000$Site, Hab$CamStation)]

#e. 1250 m 
Bears1250 <- Bears


Bears1250$LowCon <- Hab$LowCon1250[match(Bears1250$Site, Hab$CamStation)]
Bears1250$LowDecid <- Hab$LowDecid1250[match(Bears1250$Site, Hab$CamStation)]
Bears1250$Tamarack <- Hab$Tamarack1250[match(Bears1250$Site, Hab$CamStation)]
Bears1250$UpCon <- Hab$UpCon1250[match(Bears1250$Site, Hab$CamStation)]
Bears1250$UpDecid <- Hab$UpDecid1250[match(Bears1250$Site, Hab$CamStation)]
Bears1250$pOpen <- Hab$pOpen1250[match(Bears1250$Site, Hab$CamStation)]

#f. 1500 m
Bears1500 <- Bears


Bears1500$LowCon <- Hab$LowCon1500[match(Bears1500$Site, Hab$CamStation)]
Bears1500$LowDecid <- Hab$LowDecid1500[match(Bears1500$Site, Hab$CamStation)]
Bears1500$Tamarack <- Hab$Tamarack1500[match(Bears1500$Site, Hab$CamStation)]
Bears1500$UpCon <- Hab$UpCon1500[match(Bears1500$Site, Hab$CamStation)]
Bears1500$UpDecid <- Hab$UpDecid1500[match(Bears1500$Site, Hab$CamStation)]
Bears1500$pOpen <- Hab$pOpen1500[match(Bears1500$Site, Hab$CamStation)]

#g. 1750 m 
Bears1750 <- Bears


Bears1750$LowCon <- Hab$LowCon1750[match(Bears1750$Site, Hab$CamStation)]
Bears1750$LowDecid <- Hab$LowDecid1750[match(Bears1750$Site, Hab$CamStation)]
Bears1750$Tamarack <- Hab$Tamarack1750[match(Bears1750$Site, Hab$CamStation)]
Bears1750$UpCon <- Hab$UpCon1750[match(Bears1750$Site, Hab$CamStation)]
Bears1750$UpDecid <- Hab$UpDecid1750[match(Bears1750$Site, Hab$CamStation)]
Bears1750$pOpen <- Hab$pOpen1750[match(Bears1750$Site, Hab$CamStation)]

#h. 2000 m 
Bears2000 <- Bears


Bears2000$LowCon <- Hab$LowCon2000[match(Bears2000$Site, Hab$CamStation)]
Bears2000$LowDecid <- Hab$LowDecid2000[match(Bears2000$Site, Hab$CamStation)]
Bears2000$Tamarack <- Hab$Tamarack2000[match(Bears2000$Site, Hab$CamStation)]
Bears2000$UpCon <- Hab$UpCon2000[match(Bears2000$Site, Hab$CamStation)]
Bears2000$UpDecid <- Hab$UpDecid2000[match(Bears2000$Site, Hab$CamStation)]
Bears2000$pOpen <- Hab$pOpen2000[match(Bears2000$Site, Hab$CamStation)]




#### 2. Create lists of data frames ####
Lynx.Coyote.df <- c("Occ250", "Occ500", "Occ750", "Occ1000", "Occ1250", "Occ1500", "Occ1750", "Occ2000")
Bears.df <-  c("Bears250", "Bears500", "Bears750", "Bears1000", "Bears1250", "Bears1500", "Bears1750", "Bears2000")



### 3. Write function to fit models and perform stepwise AIC --- Standardize and scale covariates first!
## Use buildglmmTMB
##a. Lynx
Lynx.model <- function(x){
  x <- covscale
  model.lynx <- glmmTMB(Lynx~)
}
