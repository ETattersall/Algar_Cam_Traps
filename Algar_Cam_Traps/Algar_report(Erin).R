### Algar report figures
library(ggplot2)
library(dplyr)
library(tidyr)
library(camtrapR)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")


### Figure1. Total species detections, both deployments
## Combine 2 recordTables
##'detections_fullrecord_pilot.csv' has been edited to include P_concolor and Mustelid spp instead of other.
rec.spec <- read.csv("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2015.01/detections_fullrecord_pilot.csv")
rec.spec$X <- NULL

All.rec <- rbind.data.frame(rec.spec, rec.2016.01,deparse.level = 0) ##Warning messages:
                                            ## 1: In `[<-.factor`(`*tmp*`, ri, value = c(1486020754, 1490769575, 1491068388,  :
                                            ## invalid factor level, NA generated
                                         ## 2: In `[<-.factor`(`*tmp*`, ri, value = c(17199, 17254, 17257, 17258,  :
                                            ## invalid factor level, NA generated

write.csv(All.rec, "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data/recordsNov2015-apr2017.csv")
 ### Ignore warning unless it affects bar graph
### Deployments have different naming structures for Stations. Will need to change if we ever use this for station- related analysis (not imp. ##  now)

sp.1 <- All.rec$Species
sp.plot1 <- rev(sort(table(sp.1)))
sp.plot1 <- as.data.frame(sp.plot1) ##data frame summing detections --> fix scientific names to common
fix(sp.plot1)
ggplot(data = sp.plot1, aes(x = sp.1, y = Freq)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Total Detections") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("White-tailed deer", "Sandhill crane", "Grey wolf", "Black bear", "Coyote", "Other birds", "Snowshoe hare", "Moose", "Woodland caribou", "Human", "Red squirrel", "Canada lynx", "Red fox", "Mustelid spp", "American marten", "Cougar", "Fisher", "Wolverine"))

## Add Treatments to All.rec
StatData <- read.csv("Station_data/AlgarStations_DeploymentData.csv")
colnames(StatData)
colnames(All.rec)
All.rec$Treatment <- StatData$Treatment[match(All.rec$Station,StatData$CamStation)]
colnames(All.rec)



### Figure2. Total detections and species richness across treatments for both deployments
### Box plots of total detections by treatment (*** Updated on lines 58-77)
## 2016
fix(S2016.01) ## Rename Research lines to Untreated Control, SP+P to Treated
ggplot(data = S2016.01, aes(x = Treatment, y = Total, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Total Detections") + scale_x_discrete(limits=c("Human Use", "Untreated Control", "Treated", "Nat Regen")) + scale_fill_manual(values=c("red", "light green", "orange", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))




## 2015
fix(S2015.01) ## Rename Research lines to Untreated Control, SP+P to Treated
ggplot(data = S2015.01, aes(x = Treatment, y = Total, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Total Detections") + scale_fill_manual(values = c("orange", "purple"))  +theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

### t-test
tapply(S2015.01$Total,S2015.01$Treatment,mean) ### Research            SP+P 
                                               ### 35.50000        46.33333
tapply(S2015.01$Total,S2015.01$Treatment,sd) ### Research            SP+P 
                                            ###  40.88231        26.61624
tapply(S2015.01$Total,S2015.01$Treatment,median) ###  Research            SP+P 
                                                 ###   22              42
t.test(S2015.01$Total~S2015.01$Treatment) ### data:  S2015.01$Total by S2015.01$Treatment
                                          ### t = -0.76928, df = 18.905, p-value = 0.4512
                                          ### alternative hypothesis: true difference in means is not equal to 0
                                          ###  95 percent confidence interval:
                                          ###  -40.31831  18.65164

#### Nov 2015- Nov 2017
## Uses data 'detectionsByStation.csv', read in as 'S' (Algar32 doesn't need to be added)
ggplot(data = S, aes(x = Treatment, y = Total, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Total Detections") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("red", "light green", "orange", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))


tapply(S$Total, S$Treatment, mean)
# Control HumanUse NatRegen  OffLine      SPP 
# 64.83333 38.53846 18.75000       NA 55.27273 
tapply(S$Total, S$Treatment, median)
# Control HumanUse NatRegen  OffLine      SPP 
# 39.0     30.0     15.5       NA     31.5


###### Species Richness boxplots
#### Problem with S2016.01 site by species data.frame = dropped sites with NO detections. Need to add back in
### '2016.01detectionsByStation.csv' has sites with no detections
# create a one-row matrix the same length as data

temprow <- matrix(c(rep.int(NA,length(S2016.01))),nrow=2,ncol=length(S2016.01))

# make it a data.frame and give cols the same names as data

newrow <- data.frame(temprow)
colnames(newrow) <- colnames(S2016.01)

# rbind the empty row to data

T2016.01 <- rbind(S2016.01,newrow)
## Fill in new rows with Station data for Algar 52 and 56
fix(T2016.01) 
## New boxplots
ggplot(data = T2016.01, aes(x = Treatment, y = Total, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Total Detections") + scale_x_discrete(limits=c("Human Use", "Untreated Control", "Treated", "Nat Regen")) + scale_fill_manual(values=c("red", "light green","orange", "purple")) +theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

Tot.lm <-  lm(Total ~ Treatment, data = T2016.01)
summary(Tot.lm)
 # Call:
# lm(formula = Total ~ Treatment, data = T2016.01)

# Residuals:
# Min     1Q Median     3Q    Max 
# -7.167 -4.583 -2.850  2.321 28.833 

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          8.5833     2.0573   4.172 0.000115 ***
#  TreatmentNat Regen  -2.5833     2.9095  -0.888 0.378685    
# TreatmentSP+P       -2.7333     2.6023  -1.050 0.298419    
# TreatmentResearch   -0.4167     2.9095  -0.143 0.886678    
# ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 7.127 on 52 degrees of freedom
# Multiple R-squared:  0.03111,	Adjusted R-squared:  -0.02479 
# F-statistic: 0.5565 on 3 and 52 DF,  p-value: 0.6461

anova(Tot.lm)
# Analysis of Variance Table

# Response: Total
#           Df Sum Sq Mean Sq F value Pr(>F)
# Treatment  3   84.8  28.265  0.5565 0.6461
# Residuals 52 2641.1  50.791  

confint(Tot.lm)
#                        2.5 %    97.5 %
# (Intercept)         4.455009 12.711658
# TreatmentNat Regen -8.421666  3.254999
# TreatmentSP+P      -7.955296  2.488630
# TreatmentResearch  -6.254999  5.421666

ggplot(data = T2016.01, aes(x = Treatment, y = Richness, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Species Richness") + scale_x_discrete(limits=c("Human Use", "Untreated Control", "Treated", "Nat Regen")) + scale_fill_manual(values=c("red", "light green","orange", "purple")) +theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

Rich.lm <-  lm(Richness ~ Treatment, data = T2016.01)
summary(Rich.lm)
# Call:
#  lm(formula = Richness ~ Treatment, data = T2016.01)

# Residuals:
#  Min      1Q  Median      3Q     Max 
# -3.3333 -0.4875 -0.0833  0.6667  2.6667 

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          4.0833     0.3769  10.834 6.07e-15 ***
#  TreatmentNat Regen  -0.7500     0.5330  -1.407    0.165    
# TreatmentSP+P       -0.6833     0.4767  -1.433    0.158    
# TreatmentResearch   -0.3333     0.5330  -0.625    0.534    
# ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 1.306 on 52 degrees of freedom
# Multiple R-squared:  0.0506,	Adjusted R-squared:  -0.004174 
# F-statistic: 0.9238 on 3 and 52 DF,  p-value: 0.4359

anova(Rich.lm)
# Response: Richness
# Df Sum Sq Mean Sq F value Pr(>F)
# Treatment  3  4.724  1.5746  0.9238 0.4359
# Residuals 52 88.633  1.7045               
confint(Rich.lm)
#                        2.5 %    97.5 %
# (Intercept)         3.327063 4.8396039
# TreatmentNat Regen -1.819528 0.3195281
# TreatmentSP+P      -1.639948 0.2732816
# TreatmentResearch  -1.402861 0.7361947

## 2015.01 Richness plot
ggplot(data = S2015.01, aes(x = Treatment, y = Richness, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Species Richness") + scale_fill_manual(values = c("orange", "purple"))  +theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))


### t-test
tapply(S2015.01$Richness,S2015.01$Treatment,mean) ### Research            SP+P 
                                                  ### 5.500000        7.166667 
tapply(S2015.01$Richness,S2015.01$Treatment,sd) ### Research            SP+P 
                                                ###  2.067058        1.992410
tapply(S2015.01$Richness,S2015.01$Treatment,median) ###  Research            SP+P 
                                                    ###   5.5             6.5 
t.test(S2015.01$Richness~S2015.01$Treatment) 
### data:  S2015.01$Richness by S2015.01$Treatment
### t = -2.011, df = 21.97, p-value = 0.05675
### alternative hypothesis: true difference in means is not equal to 0
### 95 percent confidence interval:
###  -3.38557638  0.05224305

#### Figure 3. Detections from first 24 cameras across 3 seasons
## winter 2015-2016
fix(win2015) ##Change names
ggplot(data = win2015, aes(x = Treatment, y = Total, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Total Detections") + scale_fill_manual(values = c("orange", "purple"))  +theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + ggtitle("Winter 2015-2016") +theme(plot.title = element_text(angle = 0, colour = "black", size = 14))

tapply(win2015$Total,win2015$Treatment,mean) ### Research            SP+P 
                                             ###  20.00000        21.91667  
tapply(win2015$Total,win2015$Treatment,sd) ### Research            SP+P 
                                           ###  22.17287        16.64855
tapply(win2015$Total,win2015$Treatment,median) ###  Research         SP+P 
                                               ###  12.5            17.0    
t.test(win2015$Total~win2015$Treatment)
## t = -0.23946, df = 20.412, p-value = 0.8131
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -18.59162  14.75828


##Summer 2016
fix(summer2016) ##Change names
ggplot(data = summer2016, aes(x = Treatment, y = Total, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Total Detections") + scale_fill_manual(values = c("orange", "purple"))  +theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + ggtitle("Summer 2016") +theme(plot.title = element_text(angle = 0, colour = "black", size = 14))

tapply(summer2016$Total,summer2016$Treatment,mean) ### Research            SP+P 
                                                   ###  26.75000        37.08333  
tapply(summer2016$Total,summer2016$Treatment,sd) ### Research            SP+P 
                                                ###  32.39002        20.92610
tapply(summer2016$Total,summer2016$Treatment,median) ###  Research         SP+P 
                                                     ###   16.0            37.5   
t.test(summer2016$Total~summer2016$Treatment)

## t = -0.92827, df = 18.82, p-value = 0.365
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -33.64761  12.98094


## Winter 2016-2017
f24 <- S2016.01[1:23, ]
ggplot(data = f24, aes(x = Treatment, y = Total, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Total Detections") + scale_fill_manual(values = c("orange", "purple")) + scale_x_discrete(limits=c( "Untreated Control", "Treated")) +theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))  + ggtitle("Winter 2016-2017") +theme(plot.title = element_text(angle = 0, colour = "black", size = 14))

tapply(f24$Total,f24$Treatment,mean) ###      SP+P  Research
                                     ###  5.727273  8.166667  
tapply(f24$Total,f24$Treatment,sd) ###  SP+P          Research 
                                  ###  5.293220       9.824213 
tapply(f24$Total,f24$Treatment,median) ###  SP+P  Research 
                                       ###  3.0       4.5 
t.test(f24$Total~f24$Treatment)
## t = -0.74961, df = 17.176, p-value = 0.4636
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -9.299885  4.421097



###### Figure4. Proportion of occupied sites and Detection rates (relative abundance index)
## 2016, all 56 active sites
### Loop to calculate proportion of occupied sites for seven target species
sp7 <- c("R_tarandus","C_lupus", "U_americanus", "O_virginianus", "A_alces", "C_latrans", "L_canadensis")
naiv.occ <- NULL

for (sp in sp7) {
  spat <- as.data.frame(table(rec.2016.01[rec.2016.01$Species == sp, "Station"]))
  stp <- spat %>% filter(Freq > 0)
  naiv.occ[sp] <- nrow(stp)/56
}
naiv.occ <- as.data.frame(naiv.occ)

## RAI Loop (# detections/1000TD)
rel.ab <- NULL
for (sp in sp7) {
  spat <- as.data.frame(table(rec.2016.01[rec.2016.01$Species == sp, "Station"]))
  stp <- spat %>% filter(Freq > 0)
  rel.ab[sp] <- (sum(stp$Freq)/9025)*1000
}
rel.ab <- as.data.frame(rel.ab)

## Data frame of naive occupancy and relative abundance
desc2016 <- cbind(naiv.occ, rel.ab, deparse.level = 1)
desc2016$Species <- row.names(desc2016)
fix(desc2016) ##fix for species names
## Naive occupancy
ggplot(desc2016, aes(x = Species, y = naiv.occ))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Proportion of occupied sites") + scale_x_discrete(limits = c("Wolf", "WT deer", "Moose", "Lynx", "Coyote", "Caribou", "Black bear")) +theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))


## Relative abundance
ggplot(desc2016, aes(x = Species, y = rel.ab))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Detections/1000 Trap Days") + scale_x_discrete(limits = c("WT deer", "Wolf", "Coyote", "Moose", "Lynx", "Caribou", "Black bear")) +theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) 


## 2015, full deployment
### Loop to calculate proportion of occupied sites for seven target species
sp7 <- c("R_tarandus","C_lupus", "U_americanus", "O_virginianus", "A_alces", "C_latrans", "L_canadensis")
naiv.occ <- NULL

for (sp in sp7) {
  spat <- as.data.frame(table(rec.spec[rec.spec$Species == sp, "Station"]))
  stp <- spat %>% filter(Freq > 0)
  naiv.occ[sp] <- nrow(stp)/24
}
naiv.occ <- as.data.frame(naiv.occ)

## RAI Loop (# detections/1000TD)
rel.ab <- NULL
for (sp in sp7) {
  spat <- as.data.frame(table(rec.spec[rec.spec$Species == sp, "Station"]))
  stp <- spat %>% filter(Freq > 0)
  rel.ab[sp] <- (sum(stp$Freq)/8911)*1000
}
rel.ab <- as.data.frame(rel.ab)

## Data frame of naive occupancy and relative abundance
desc2015 <- cbind(naiv.occ, rel.ab, deparse.level = 1)
desc2015$Species <- row.names(desc2015)
fix(desc2015) ##fix for species names

## Naive occupancy
ggplot(desc2015, aes(x = Species, y = naiv.occ))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Proportion of occupied sites") +theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + scale_x_discrete(limits = c("Black bear", "Wolf", "WT deer",  "Lynx", "Coyote", "Moose", "Caribou"))

## Relative abundance
ggplot(desc2015, aes(x = Species, y = rel.ab))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Detections/1000 Trap Days") +theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + scale_x_discrete(limits = c("WT deer", "Black bear", "Wolf",  "Coyote", "Caribou", "Moose", "Lynx"))







######## Figure 5. and 6. Detection rates  for 7 target species between seasons for first deployment (Fig. 5 = Winter, Fig. 6 = Summer)
### Winter 2015-16 (Fig. 5)
##Active days by station:
camEff <- as.data.frame(cameraOperation(cams2015, 
                                        stationCol = "CamStation", 
                                        setupCol = "DeployDate", 
                                        retrievalCol = "SeasonBreak",
                                        hasProblems = FALSE,
                                        dateFormat = "%d/%m/%Y", 
                                        writecsv = FALSE))
camEff$Treatment <- cams2015$Treatment[match(row.names(camEff),cams2015$CamStation)] ## Adding treatment to each station
fix(camEff) ### Editing to change "Control" to "Untreated Control" and "Site Prep Plant" to "SP+P"(consistent with other dataframes)
camEff$Total <- apply(camEff[, 1:168],1,sum, na.rm = T)
TD_treat <- camEff %>% select(Total, Treatment) 
TD.res <- filter(TD_treat, Treatment== "Untreated Control")
### camera days by treatment
sum(TD.res$Total) #2006
TD.res$trapdays <- rep(2006, length(12))

TD.SPP <- filter(TD_treat, Treatment== "Treated")
sum(TD.SPP$Total) #2003
TD.SPP$trapdays <- rep(2003, length(12))

TD_treat <- rbind(TD.res, TD.SPP, deparse.level = 0) ## Each site now has trapdays for that treatment
win2015.1 <- gather(win2015, Species, Sp.detect, 1:14)
win2015.1 <- win2015.1[(win2015.1$Species == "A_alces") | (win2015.1$Species =="C_latrans") | (win2015.1$Species =="C_lupus")| (win2015.1$Species =="L_canadensis") | (win2015.1$Species =="O_virginianus") | (win2015.1$Species =="R_tarandus"), ] ## dataframe for 7 species and their detections
fix(win2015.1) ## Changing species to common names
### Add detection rate to win2015.1
win2015.1$trapdays <- TD_treat$trapdays[match(win2015.1$Treatment,TD_treat$Treatment)]
win2015.1$det.rate <- (win2015.1$Sp.detect/win2015.1$trapdays)*1000


ggplot(data = win2015.1, aes(x = Treatment, y = det.rate, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Detections/1000 Trap Days") + scale_x_discrete(limits=c("Untreated Control", "Treated")) + scale_fill_manual(values=c("orange", "purple")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species, ncol = 2, scales = "free_y") + theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + theme(strip.text = element_text(colour = "black", size = 14)) + theme(aspect.ratio=1)

## T-tests for ea. species
t.test(win2015$R_tarandus~win2015$Treatment)
t.test(win2015$C_latrans~win2015$Treatment)
t.test(win2015$C_lupus~win2015$Treatment)
t.test(win2015$L_canadensis~win2015$Treatment)
t.test(win2015$A_alces~win2015$Treatment)
t.test(win2015$O_virginianus~win2015$Treatment)


### Summer 2016 (Fig.6)
##Active days by station:
camEff <- as.data.frame(cameraOperation(cams2015, 
                                        stationCol = "CamStation", 
                                        setupCol = "SeasonBreak", 
                                        retrievalCol = "CheckDate1",
                                        hasProblems = FALSE,
                                        dateFormat = "%d/%m/%Y", 
                                        writecsv = FALSE))
camEff$Treatment <- cams2015$Treatment[match(row.names(camEff),cams2015$CamStation)] ## Adding treatment to each station
fix(camEff) ### Editing to change "Control" to "Research" and "Site Prep Plant" to "Treated"(consistent with other dataframes)
camEff$Total <- apply(camEff[, 1:206],1,sum, na.rm = T)
TD_treat <- camEff %>% select(Total, Treatment) 
TD.res <- filter(TD_treat, Treatment== "Untreated Control")### camera days by treatment
sum(TD.res$Total) #2463
TD.res$trapdays <- rep(2463, length(12))

TD.SPP <- filter(TD_treat, Treatment== "Treated")
sum(TD.SPP$Total) #2003
TD.SPP$trapdays <- rep(2463, length(12))

TD_treat <- rbind(TD.res, TD.SPP, deparse.level = 0) ## Each site now has trapdays for that treatment
summer2016.1 <- gather(summer2016, Species, Sp.detect, 1:14)
summer2016.1 <- summer2016.1[(summer2016.1$Species == "A_alces") | (summer2016.1$Species =="C_latrans") | (summer2016.1$Species =="C_lupus")| (summer2016.1$Species =="L_canadensis") | (summer2016.1$Species =="O_virginianus") | (summer2016.1$Species =="R_tarandus") | (summer2016.1$Species == "U_americanus"), ] ## dataframe for 7 species and their detections
fix(summer2016.1) ## Changing species to common names
### Add detection rate to summer2016.1
summer2016.1$trapdays <- TD_treat$trapdays[match(summer2016.1$Treatment,TD_treat$Treatment)]
summer2016.1$det.rate <- (summer2016.1$Sp.detect/summer2016.1$trapdays)*1000

ggplot(data = summer2016.1, aes(x = Treatment, y = det.rate, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Detections/1000 Trap Days") + scale_x_discrete(limits=c("Untreated Control", "Treated")) + scale_fill_manual(values=c("orange", "purple")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species,nrow = 4, scales = "free_y") + theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + theme(strip.text = element_text(colour = "black", size = 14)) + theme(aspect.ratio=1)
### No noticeable change because the number of trap days is roughly the same between treatments

## t-tests for each species
t.test(summer2016$U_americanus~summer2016$Treatment)
t.test(summer2016$R_tarandus~summer2016$Treatment)
t.test(summer2016$C_latrans~summer2016$Treatment)
t.test(summer2016$L_canadensis~summer2016$Treatment)
t.test(summer2016$A_alces~summer2016$Treatment)
t.test(summer2016$O_virginianus~summer2016$Treatment)
t.test(summer2016$C_lupus~summer2016$Treatment)

##### Figure 7. Detection rates for 7 target mammal species across treatment types (56 stations)
## First need trap days by treatment type
##Active days by station:
camEff <- as.data.frame(cameraOperation(cams2016, 
                                        stationCol = "CamStation", 
                                        setupCol = "CheckDate1", 
                                        retrievalCol = "CheckDate2",
                                        hasProblems = TRUE,
                                        dateFormat = "%d/%m/%Y", 
                                        writecsv = FALSE))
camEff$Treatment <- cams2016$TreatmentType[match(row.names(camEff),cams2016$CamStation)] ## Adding treatment to each station
fix(camEff) ### Editing to change "Control" to "Untreated Control", "SP+P" to "Treated"
camEff$Total <- apply(camEff[, 1:162],1,sum, na.rm = T)
TD_treat <- camEff %>% select(Total, Treatment) 
TD.res <- filter(TD_treat, Treatment== "Untreated Control")
### camera days by treatment
sum(TD.res$Total) #1935
TD.res$trapdays <- rep(1935, length(12))

TD.HU <- filter(TD_treat, Treatment== "Human Use")
sum(TD.HU$Total) #2235
TD.HU$trapdays <- rep(2235, length(14))

TD.NR <- filter(TD_treat, Treatment== "Nat Regen")
sum(TD.NR$Total) #1784
TD.NR$trapdays <- rep(1784, length(12))

TD.SPP <- filter(TD_treat, Treatment== "Treated")
sum(TD.SPP$Total) #3071
TD.SPP$trapdays <- rep(3071, length(22))

TD_treat <- rbind(TD.res,TD.HU,TD.NR, TD.SPP, deparse.level = 0) ## Each site now has trapdays for that treatment

T2.2016.01 <- gather(T2016.01, Species, Sp.detect, 1:14)
T2.2016.01 <- T2.2016.01[(T2.2016.01$Species == "A_alces") | (T2.2016.01$Species =="C_latrans") | (T2.2016.01$Species =="C_lupus")| (T2.2016.01$Species =="L_canadensis") | (T2.2016.01$Species =="O_virginianus") | (T2.2016.01$Species =="R_tarandus"), ] ## dataframe for 7 species and their detections
fix(T2.2016.01) ## Changing species to common names,
### Add detection rate to T2.2016.01
T2.2016.01$trapdays <- TD_treat$trapdays[match(T2.2016.01$Treatment,TD_treat$Treatment)]
T2.2016.01$det.rate <- (T2.2016.01$Sp.detect/T2.2016.01$trapdays)*1000

ggplot(data = T2.2016.01, aes(x = Treatment, y = det.rate, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Detections/1000 Trap Days") + scale_x_discrete(limits=c("Human Use", "Untreated Control", "Treated", "Nat Regen")) + scale_fill_manual(values=c("red", "light green", "orange", "purple")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species, nrow=3, scales = "free_y") + theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + theme(strip.text = element_text(colour = "black", size = 14)) + theme(aspect.ratio=1)



###### Spatial figures
#### Spatial plots: same code, subsitituting values. Using T2016.01 (winter 2016-17 all 56 cams), win2015 (winter 2015-16), summer2016 
with(T2016.01, symbols(x=utmE, y=utmN, circles=Richness, inches=2/3, bg="lightblue", fg="black", 
                       main = "Winter 2016/17"))

win2015$utmE <- cams2015$utmE[match(row.names(win2015),cams2015$CamStation)]
win2015$utmN <- cams2015$utmN[match(row.names(win2015),cams2015$CamStation)]
with(win2015, symbols(x=utmE, y=utmN, circles=Richness, inches=2/3, bg="lightblue", fg="black", main = "Winter 2015/16"))

summer2016$utmE <- cams2015$utmE[match(row.names(summer2016),cams2015$CamStation)]
summer2016$utmN <- cams2015$utmN[match(row.names(summer2016),cams2015$CamStation)]
with(summer2016, symbols(x=utmE, y=utmN, circles=Richness, inches=2/3, bg="lightblue", fg="black", main = "Summer 2016"))


with(f24, symbols(x=utmE, y=utmN, circles=Total, inches=2/3, bg="lightblue", fg="black", 
                  main = "Winter 2016/17"))

## Individual species
with(summer2016, symbols(x=utmE, y=utmN, circles=U_americanus, inches=2/3, bg="lightblue", fg="black", main = "Black bear"))

### Combining detection rates
a3 <- win2015.1 %>% select(utmE,utmN, Species, det.rate) ## Winter 2015-2016
a4 <- summer2016.1 %>% select(utmE,utmN, Species, det.rate) ## Summer 2016
a5 <- T2.2016.01 %>% select(utmE,utmN, Species, det.rate) ## winter 2016-2017
det.frame <- rbind(a3,a4,a5) 
moose <- det.frame %>% filter(Species == "Moose")
with(moose, symbols(x=utmE, y=utmN, circles=det.rate, inches=2/3, bg="lightblue", fg="black", main = "Moose"))

caribou <- det.frame %>% filter(Species == "Caribou")
with(caribou, symbols(x=utmE, y=utmN, circles=det.rate, inches=2/3, bg="lightblue", fg="black", main = "Caribou"))

Coyote <- det.frame %>% filter(Species == "Coyote")
with(Coyote, symbols(x=utmE, y=utmN, circles=det.rate, inches=2/3, bg="lightblue", fg="black", main = "Coyote"))

Lynx <- det.frame %>% filter(Species == "Lynx")
with(Lynx, symbols(x=utmE, y=utmN, circles=det.rate, inches=2/3, bg="lightblue", fg="black", main = "Lynx"))

Wolf <- det.frame %>% filter(Species == "Wolf")
with(Wolf, symbols(x=utmE, y=utmN, circles=det.rate, inches=2/3, bg="lightblue", fg="black", main = "Wolf"))

WT_deer <- det.frame %>% filter(Species == "WT deer")
with(WT_deer, symbols(x=utmE, y=utmN, circles=det.rate, inches=2/3, bg="lightblue", fg="black", main = "WT deer"))

Black_bear <- det.frame %>% filter(Species == "Black bear")
with(Black_bear, symbols(x=utmE, y=utmN, circles=det.rate, inches=2/3, bg="lightblue", fg="black", main = "Black bear"))
