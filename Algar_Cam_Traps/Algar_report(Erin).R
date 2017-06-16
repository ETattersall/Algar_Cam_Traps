### Algar report figures
library(ggplot2)
library(dplyr)
library(tidyr)
library(camtrapR)


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
 ### Ignore warning unless it affects bar graph
### Deployments have different naming structures for Stations. Will need to change if we ever use this for station- related analysis (not imp. ##  now)

sp.1 <- All.rec$Species
sp.plot1 <- rev(sort(table(sp.1)))
sp.plot1 <- as.data.frame(sp.plot1)
ggplot(data = sp.plot1, aes(x = sp.1, y = Freq)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Total Detections") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"))




### Figure2. Total detections and species richness across treatments for both deployments
### Box plots of total detections by treatment
## 2016
ggplot(data = S2016.01, aes(x = Treatment, y = Total, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Total Detections") + scale_x_discrete(limits=c("Human Use", "Research", "SP+P", "Nat Regen")) + scale_fill_manual(values=c("red", "light green", " light blue", "purple"))
## 2015
ggplot(data = S2015.01, aes(x = Treatment, y = Total, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Total Detections") + scale_fill_manual(values = c("purple", "light blue"))

###### Species Richness boxplots
#### Problem with S2016.01 site by species data.frame = dropped sites with NO detections. Need to add back in
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
ggplot(data = T2016.01, aes(x = Treatment, y = Total, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Total Detections") + scale_x_discrete(limits=c("Human Use", "Research", "SP+P", "Nat Regen")) + scale_fill_manual(values=c("red", "light green", " light blue", "purple"))

ggplot(data = T2016.01, aes(x = Treatment, y = Richness, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Species Richness") + scale_x_discrete(limits=c("Human Use", "Research", "SP+P", "Nat Regen")) + scale_fill_manual(values=c("red", "light green", " light blue", "purple"))

#### Figure 3. Detections from first 24 cameras across 3 seasons
## winter 2015-2016
ggplot(data = win2015, aes(x = Treatment, y = Total, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Total Detections") + scale_fill_manual(values = c("purple", "light blue"))

##Summer 2016
ggplot(data = summer2016, aes(x = Treatment, y = Total, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Total Detections") + scale_fill_manual(values = c("purple", "light blue"))
## Winter 2016-2017
f24 <- S2016.01[1:23, ]
ggplot(data = f24, aes(x = Treatment, y = Total, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Total Detections") + scale_fill_manual(values = c("light blue", "purple")) + scale_x_discrete(limits=c("Research", "SP+P"))


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

## Naive occupancy
ggplot(desc2016, aes(x = Species, y = naiv.occ))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Proportion of occupied sites") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("C_lupus","O_virginianus", "A_alces", "L_canadensis", "C_latrans", "R_tarandus", "U_americanus"))

## Relative abundance
ggplot(desc2016, aes(x = Species, y = rel.ab))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Detections/1000 Trap Days") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("O_virginianus", "C_lupus",  "C_latrans", "A_alces","L_canadensis", "R_tarandus", "U_americanus"))


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

## Naive occupancy
ggplot(desc2015, aes(x = Species, y = naiv.occ))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Proportion of occupied sites") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("U_americanus", "C_lupus","O_virginianus", "L_canadensis", "C_latrans", "A_alces","R_tarandus"))

## Relative abundance
ggplot(desc2015, aes(x = Species, y = rel.ab))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Detections/1000 Trap Days") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("O_virginianus", "U_americanus", "C_lupus",  "C_latrans", "R_tarandus", "A_alces", "L_canadensis"))


##### Figure 5. Detection rates for 7 target mammal species across treatment types
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
fix(camEff) ### Editing to change "Control" to "Research" (consistent with other dataframes)
camEff$Total <- apply(camEff[, 1:162],1,sum, na.rm = T)
TD_treat <- camEff %>% select(Total, Treatment) 
TD.res <- filter(TD_treat, Treatment== "Research")
### camera days by treatment
sum(TD.res$Total) #1935
TD.res$trapdays <- rep(1935, length(12))

TD.HU <- filter(TD_treat, Treatment== "Human Use")
sum(TD.HU$Total) #2235
TD.HU$trapdays <- rep(2235, length(14))

TD.NR <- filter(TD_treat, Treatment== "Nat Regen")
sum(TD.NR$Total) #1784
TD.NR$trapdays <- rep(1784, length(12))

TD.SPP <- filter(TD_treat, Treatment== "SP+P")
sum(TD.SPP$Total) #3071
TD.SPP$trapdays <- rep(3071, length(22))

TD_treat <- rbind(TD.res,TD.HU,TD.NR, TD.SPP, deparse.level = 0) ## Each site now has trapdays for that treatment

T2.2016.01 <- gather(T2016.01, Species, Sp.detect, 1:14)
T2.2016.01 <- T2.2016.01[(T2.2016.01$Species == "A_alces") | (T2.2016.01$Species =="C_latrans") | (T2.2016.01$Species =="C_lupus")| (T2.2016.01$Species =="L_canadensis") | (T2.2016.01$Species =="O_virginianus") | (T2.2016.01$Species =="R_tarandus"), ] ## dataframe for 7 species and their detections
### Add detection rate to T2.2016.01
T2.2016.01$trapdays <- TD_treat$trapdays[match(T2.2016.01$Treatment,TD_treat$Treatment)]
T2.2016.01$det.rate <- (T2.2016.01$Sp.detect/T2.2016.01$trapdays)*1000

ggplot(data = T2.2016.01, aes(x = Treatment, y = det.rate, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Detections/1000 Trap Days") + scale_x_discrete(limits=c("Human Use", "Research", "SP+P", "Nat Regen")) + scale_fill_manual(values=c("red", "light green", "light blue", "purple")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"))
#### Individual boxes should not have changed, but placement in relation to other boxes should have
### Deer mainly responsible for scale on y-axis, separate by species
moose <- filter(T2.2016.01, Species == "A_alces")
ggplot(data = moose, aes(x = Treatment, y = det.rate, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Detections/1000 Trap Days") + scale_x_discrete(limits=c("Human Use", "Research", "SP+P", "Nat Regen")) + scale_fill_manual(values=c("red", "light green", "light blue", "purple")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"))

coyote <- filter(T2.2016.01, Species == "C_latrans")
ggplot(data = coyote, aes(x = Treatment, y = det.rate, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Detections/1000 Trap Days") + scale_x_discrete(limits=c("Human Use", "Research", "SP+P", "Nat Regen")) + scale_fill_manual(values=c("red", "light green", "light blue", "purple")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"))

wolf <- filter(T2.2016.01, Species == "C_lupus")
ggplot(data = wolf, aes(x = Treatment, y = det.rate, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Detections/1000 Trap Days") + scale_x_discrete(limits=c("Human Use", "Research", "SP+P", "Nat Regen")) + scale_fill_manual(values=c("red", "light green", "light blue", "purple")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"))

lynx <- filter(T2.2016.01, Species == "L_canadensis")
ggplot(data = lynx, aes(x = Treatment, y = det.rate, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Detections/1000 Trap Days") + scale_x_discrete(limits=c("Human Use", "Research", "SP+P", "Nat Regen")) + scale_fill_manual(values=c("red", "light green", "light blue", "purple")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"))

caribou <- filter(T2.2016.01, Species == "R_tarandus")
ggplot(data = caribou, aes(x = Treatment, y = det.rate, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Detections/1000 Trap Days") + scale_x_discrete(limits=c("Human Use", "Research", "SP+P", "Nat Regen")) + scale_fill_manual(values=c("red", "light green", "light blue", "purple")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"))

deer <- filter(T2.2016.01, Species == "O_virginianus")
ggplot(data = deer, aes(x = Treatment, y = det.rate, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Detections/1000 Trap Days") + scale_x_discrete(limits=c("Human Use", "Research", "SP+P", "Nat Regen")) + scale_fill_manual(values=c("red", "light green", "light blue", "purple")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"))
#### Separating plots doesn't really show anything new (variation isn't more noticeable in other species when separated)


### Winter 2015-16
##Active days by station:
camEff <- as.data.frame(cameraOperation(cams2015, 
                                        stationCol = "CamStation", 
                                        setupCol = "DeployDate", 
                                        retrievalCol = "SeasonBreak",
                                        hasProblems = FALSE,
                                        dateFormat = "%d/%m/%Y", 
                                        writecsv = FALSE))
camEff$Treatment <- cams2015$Treatment[match(row.names(camEff),cams2015$CamStation)] ## Adding treatment to each station
fix(camEff) ### Editing to change "Control" to "Research" and "Site Prep Plant" to "SP+P"(consistent with other dataframes)
camEff$Total <- apply(camEff[, 1:168],1,sum, na.rm = T)
TD_treat <- camEff %>% select(Total, Treatment) 
TD.res <- filter(TD_treat, Treatment== "Research")
### camera days by treatment
sum(TD.res$Total) #2006
TD.res$trapdays <- rep(2006, length(12))

TD.SPP <- filter(TD_treat, Treatment== "SP+P")
sum(TD.SPP$Total) #2003
TD.SPP$trapdays <- rep(2003, length(12))

TD_treat <- rbind(TD.res, TD.SPP, deparse.level = 0) ## Each site now has trapdays for that treatment
win2015.1 <- gather(win2015, Species, Sp.detect, 1:14)
win2015.1 <- win2015.1[(win2015.1$Species == "A_alces") | (win2015.1$Species =="C_latrans") | (win2015.1$Species =="C_lupus")| (win2015.1$Species =="L_canadensis") | (win2015.1$Species =="O_virginianus") | (win2015.1$Species =="R_tarandus"), ] ## dataframe for 7 species and their detections
### Add detection rate to win2015.1
win2015.1$trapdays <- TD_treat$trapdays[match(win2015.1$Treatment,TD_treat$Treatment)]
win2015.1$det.rate <- (win2015.1$Sp.detect/win2015.1$trapdays)*1000

ggplot(data = win2015.1, aes(x = Treatment, y = det.rate, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Detections/1000 Trap Days") + scale_x_discrete(limits=c("Research", "SP+P")) + scale_fill_manual(values=c("Purple", "light blue")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) 
### No noticeable change because the number of trap days is roughly the same between treatments

### Summer 2016
##Active days by station:
camEff <- as.data.frame(cameraOperation(cams2015, 
                                        stationCol = "CamStation", 
                                        setupCol = "SeasonBreak", 
                                        retrievalCol = "CheckDate1",
                                        hasProblems = FALSE,
                                        dateFormat = "%d/%m/%Y", 
                                        writecsv = FALSE))
camEff$Treatment <- cams2015$Treatment[match(row.names(camEff),cams2015$CamStation)] ## Adding treatment to each station
fix(camEff) ### Editing to change "Control" to "Research" and "Site Prep Plant" to "SP+P"(consistent with other dataframes)
camEff$Total <- apply(camEff[, 1:206],1,sum, na.rm = T)
TD_treat <- camEff %>% select(Total, Treatment) 
TD.res <- filter(TD_treat, Treatment== "Research")### camera days by treatment
sum(TD.res$Total) #2463
TD.res$trapdays <- rep(2463, length(12))

TD.SPP <- filter(TD_treat, Treatment== "SP+P")
sum(TD.SPP$Total) #2003
TD.SPP$trapdays <- rep(2463, length(12))

TD_treat <- rbind(TD.res, TD.SPP, deparse.level = 0) ## Each site now has trapdays for that treatment
summer2016.1 <- gather(summer2016, Species, Sp.detect, 1:14)
summer2016.1 <- summer2016.1[(summer2016.1$Species == "A_alces") | (summer2016.1$Species =="C_latrans") | (summer2016.1$Species =="C_lupus")| (summer2016.1$Species =="L_canadensis") | (summer2016.1$Species =="O_virginianus") | (summer2016.1$Species =="R_tarandus") | (summer2016.1$Species == "U_americanus"), ] ## dataframe for 7 species and their detections
### Add detection rate to summer2016.1
summer2016.1$trapdays <- TD_treat$trapdays[match(summer2016.1$Treatment,TD_treat$Treatment)]
summer2016.1$det.rate <- (summer2016.1$Sp.detect/summer2016.1$trapdays)*1000

ggplot(data = summer2016.1, aes(x = Treatment, y = det.rate, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Detections/1000 Trap Days") + scale_x_discrete(limits=c("Research", "SP+P")) + scale_fill_manual(values=c("Purple", "light blue")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) 
### No noticeable change because the number of trap days is roughly the same between treatments