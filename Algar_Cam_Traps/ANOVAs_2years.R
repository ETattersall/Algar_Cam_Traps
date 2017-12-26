##################################
# ANOVAs_2years.R
# Basic ANOVAs, analyeses for Nov2015 to Nov 2017 data
# Started Dec 11, 2017 by Erin
##################################


## Uses data 'detectionsByStation.csv', read in as 'S' (Algar32 doesn't need to be added)

library(ggplot2)
library(dplyr)
library(tidyr)
library(camtrapR)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

## Boxplot comparing total detections by treatment
ggplot(data = S, aes(x = Treatment, y = Total, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Total Detections") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red", "lightgreen", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))


tapply(S$Total, S$Treatment, mean)
# Control HumanUse NatRegen  OffLine      SPP 
# 64.83333 38.53846 18.75000       NA 55.27273 
tapply(S$Total, S$Treatment, median)
# Control HumanUse NatRegen  OffLine      SPP 
# 39.0     30.0     15.5       NA     31.5

Tot.lm <-  lm(Total ~ Treatment, data = S)
summary(Tot.lm) ##Need to make HumanUse the intercept?

# Residuals:
#  Min     1Q Median     3Q    Max 
# -54.27 -31.41 -11.27   9.75 221.17 

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)         64.833     14.827   4.373 5.51e-05 ***
#  TreatmentHumanUse  -26.295     20.561  -1.279   0.2063    
#  TreatmentNatRegen  -46.083     20.968  -2.198   0.0322 *  
#  TreatmentSPP        -9.561     18.432  -0.519   0.6061    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 51.36 on 55 degrees of freedom
# Multiple R-squared:  0.0981,	Adjusted R-squared:  0.04891 
# F-statistic: 1.994 on 3 and 55 DF,  p-value: 0.1255

anova(Tot.lm)
# Analysis of Variance Table

# Response: Total
# Df Sum Sq Mean Sq F value Pr(>F)
# Treatment  3  15782  5260.6  1.9942 0.1255
# Residuals 55 145092  2638.0 

confint(Tot.lm)
#                       2.5 %    97.5 %
# (Intercept)        35.11965 94.547014
# TreatmentHumanUse -67.50033 14.910590
# TreatmentNatRegen -88.10482 -4.061843
# TreatmentSPP      -46.49956 27.378346

ggplot(data = S, aes(x = Treatment, y = Richness, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Species Richness") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red","lightgreen", "purple")) +theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

Rich.lm <-  lm(Richness ~ Treatment, data = S)
summary(Rich.lm)

# Residuals:
#   Min      1Q  Median      3Q     Max 
#  -5.8636 -1.8902  0.8333  1.4913  6.1364 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)         7.1667     0.7928   9.039 1.83e-12 ***
#  TreatmentHumanUse  -1.0128     1.0995  -0.921   0.3610    
#  TreatmentNatRegen  -2.2500     1.1212  -2.007   0.0497 *  
#  TreatmentSPP       -0.3030     0.9856  -0.307   0.7597    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 2.746 on 55 degrees of freedom
# Multiple R-squared:  0.08579,	Adjusted R-squared:  0.03592 
# F-statistic:  1.72 on 3 and 55 DF,  p-value: 0.1735


anova(Rich.lm)
# Analysis of Variance Table

# Response: Richness
# Df Sum Sq Mean Sq F value Pr(>F)
# Treatment  3  38.93  12.977  1.7204 0.1735
# Residuals 55 414.87   7.543  
               
confint(Rich.lm)
#                      2.5 %       97.5 %
#  (Intercept)        5.577792  8.755541420
#  TreatmentHumanUse -3.216193  1.190552331
#  TreatmentNatRegen -4.497008 -0.002991774
#  TreatmentSPP      -2.278261  1.672200195

###### Proportion of occupied sites and Detection rates (relative abundance index) #####
### Loop to calculate proportion of occupied sites for seven target species
sp7 <- c("Rangifer tarandus","Canis lupus", "Ursus americanus", "Odocoileus virginianus", "Alces alces", "Canis latrans", "Lynx canadensis")
naiv.occ <- NULL ##num.stations occupied/59 (active sites)

for (sp in sp7) {
  spat <- as.data.frame(table(All.rec[All.rec$Species == sp, "Station"]))
  stp <- spat %>% filter(Freq > 0)
  naiv.occ[sp] <- nrow(stp)/59
}
naiv.occ <- as.data.frame(naiv.occ)

## RAI Loop (# detections/1000TD)
rel.ab <- NULL

## Need to calculate total active days for survey to date
# Nov 2016 and Apr 2017 Check dates will be counted twice. Add all 'total trap days. Subtract 24 days # for Nov 2016, and 60 days for Apr 2017?

8911 + 9025 + 7703 #25639
25639-24-60 #25555



for (sp in sp7) {
  spat <- as.data.frame(table(All.rec[All.rec$Species == sp, "Station"]))
  stp <- spat %>% filter(Freq > 0)
  rel.ab[sp] <- (sum(stp$Freq)/25555)*1000
}
rel.ab <- as.data.frame(rel.ab)

## Data frame of naive occupancy and relative abundance
desc <- cbind(naiv.occ, rel.ab, deparse.level = 1)
desc$Species <- row.names(desc)
fix(desc) ##fix for species names
## Naive occupancy
ggplot(desc, aes(x = Species, y = naiv.occ))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Proportion of occupied sites") + scale_x_discrete(limits = c("Wolf", "Black bear","Moose","WT deer", "Caribou", "Lynx", "Coyote")) +theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 25, colour = "black", size = 12, vjust = 0.7)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))


## Relative abundance
ggplot(desc, aes(x = Species, y = rel.ab))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Detections/1000 Trap Days") + scale_x_discrete(limits = c("WT deer","Black bear", "Wolf", "Coyote", "Moose", "Caribou", "Lynx")) +theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 25, colour = "black", size = 12, vjust = 0.7)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) 

#####  Detection rates for 7 target mammal species across treatment types ####
## First need trap days by treatment type. Requires deployment info for entire survey
## Try AlgarStations_DeploymentData.csv (StatData), using Session1Start as beginning and Session4Start as end--> need to create a separate column of setup dates (done in Excel)
StatData <- read.csv("Station_data/AlgarStations60.csv")


##Active days by station:
camEff <- as.data.frame(cameraOperation(StatData, 
                                        stationCol = "CamStation", 
                                        setupCol = "SetupDay", 
                                        retrievalCol = "Session4Start",
                                        hasProblems = TRUE,
                                        dateFormat = "%d/%m/%Y", 
                                        writecsv = FALSE))
camEff$Treatment <- StatData$Treatment[match(row.names(camEff),StatData$CamStation)] ## Adding treatment to each station
camEff$Treatment

camEff$Total <- apply(camEff[, 1:737],1,sum, na.rm = T)
camEff$Total[32]

TD_treat <- camEff %>% select(Total, Treatment) 
TD.con <- filter(TD_treat, Treatment== "Control")
### camera days by treatment
sum(TD.con$Total) #8072
TD.con$trapdays <- rep(8072, length(12))

TD.HU <- filter(TD_treat, Treatment== "HumanUse")
sum(TD.HU$Total) #3825
TD.HU$trapdays <- rep(3825, length(14))

TD.NR <- filter(TD_treat, Treatment== "NatRegen")
sum(TD.NR$Total) #3456
TD.NR$trapdays <- rep(3456, length(12))

TD.SPP <- filter(TD_treat, Treatment== "SPP")
sum(TD.SPP$Total) #10202
TD.SPP$trapdays <- rep(10202, length(22))

TD_treat <- rbind(TD.con,TD.HU,TD.NR, TD.SPP, deparse.level = 0) ## Each site now has trapdays for that treatment

fix(S) # Convert to common names
S7 <- gather(S, Species, Sp.detect, 1:19)
S7 <- S7[(S7$Species == "Moose") | (S7$Species =="Coyote") | (S7$Species =="Wolf")| (S7$Species =="Lynx") | (S7$Species =="WT deer") | (S7$Species =="Caribou") | (S7$Species == "Black bear"), ] ## dataframe for 7 species and their detections

### Add detection rate to S7
S7$trapdays <- TD_treat$trapdays[match(S7$Treatment,TD_treat$Treatment)]
S7$det.rate <- (S7$Sp.detect/S7$trapdays)*1000

ggplot(data = S7, aes(x = Treatment, y = det.rate, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Detections/1000 Trap Days") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red", "lightgreen", "purple")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species, nrow=3, scales = "free_y") + theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + theme(strip.text = element_text(colour = "black", size = 14)) + theme(aspect.ratio=1)

#### Simple lms and ANOVA for each target species ####
fix(S) #species names can't have spaces

## Wolf
wolf.lm <- lm(Wolf~Treatment, data = S)
summary(wolf.lm) ## Nothing of note
# Residuals:
#   Min     1Q Median     3Q    Max 
# -7.385 -3.917 -2.727 -1.385 67.273 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)          3.917      3.248   1.206    0.233
# TreatmentHumanUse    3.468      4.504   0.770    0.445
# TreatmentNatRegen   -0.500      4.593  -0.109    0.914
# TreatmentSPP         1.811      4.037   0.448    0.656

# Residual standard error: 11.25 on 55 degrees of freedom
# Multiple R-squared:  0.01777,	Adjusted R-squared:  -0.03581 
# F-statistic: 0.3316 on 3 and 55 DF,  p-value: 0.8025

anova(wolf.lm)

Bear.lm <- lm(Bear ~ Treatment, data = S)
summary(Bear.lm) ## Nothing of note

Lynx.lm <- lm(Lynx ~ Treatment, data = S)
summary(Lynx.lm) ## HumanUse significantly higher than Control... no real reason

Coyote.lm <- lm(Coyote~ Treatment, data = S)
summary(Coyote.lm) ## Nothing of note

Deer.lm <- lm(WTdeer~Treatment, data = S)
summary(Deer.lm) #Control significantly higher different from all others

Moose.lm <- lm(Moose~Treatment, data = S)
summary(Moose.lm) #NatRegen significantly diff from Control (p = 0.0336)

Caribou.lm <- lm(Caribou~Treatment, data = S)
summary(Caribou.lm) ## Nothing of note
