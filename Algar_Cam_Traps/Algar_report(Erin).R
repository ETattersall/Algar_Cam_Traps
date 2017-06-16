### Algar report figures
library(ggplot2)
library(dplyr)
library(tidyr)
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
## 2016
ggplot(data = S2016.01, aes(x = Treatment, y = Richness, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Species Richness") + scale_x_discrete(limits=c("Human Use", "Research", "SP+P", "Nat Regen")) + scale_fill_manual(values=c("red", "light green", " light blue", "purple"))
## 2015
ggplot(data = S2015.01, aes(x = Treatment, y = Richness, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Species Richness") + scale_fill_manual(values = c("purple", "light blue"))


