###### Graphing summary stats for Algar 2016.01 deployment
### Started 6 June, 2017 by Erin. T

library(ggplot2)
library(dplyr)
library(tidyr)

### Need to edit dataframes to make treatment names consistent (Control = Research Line, Treatment = SP+P)
fix(S2015.01)
## S2016.01 = dataframe of detections by station and species for 2016.01 deployment


## 2015.01 deployment data,frame S2015.01
# add coordinates and treatment to dataframe (both deployments)
cams2016$CamStation <- toupper(cams2016$CamStation)
S2016.01$utmE <- cams2016$utmE[match(row.names(S2016.01),cams2016$CamStation)]
S2016.01$utmN <- cams2016$utmN[match(row.names(S2016.01),cams2016$CamStation)]

S2016.01$Treatment <- cams2016$TreatmentType[match(row.names(S2016.01),cams2016$CamStation)]

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

## winter 2015-2016
ggplot(data = win2015, aes(x = Treatment, y = Richness, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Species Richness") + scale_fill_manual(values = c("purple", "light blue"))
##Summer 2016
ggplot(data = summer2016, aes(x = Treatment, y = Richness, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Species Richness") + scale_fill_manual(values = c("purple", "light blue"))

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
### Detection histograms
sp_detect <- ani.rec$Species
st_detect <- ani.rec$Station

### Frequency histograms

par(mfrow = c(1,1))## Multiple plots on same page (1 row, 1 column)

sp.plot <- rev(sort(table(sp_detect))) 
sp.plot <- as.data.frame(sp.plot)
ggplot(data = sp.plot, aes(x = sp_detect, y = Freq)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Total Detections") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"))

##Making 2015 histogram with "Others" edited to add P_concolor
table(ani.2015$Species == "Other")
other.2015 <- ani.2015 %>% filter(Species == "Other") %>% select(Station, Species) ## Algar3 and Algar17 are Cougars
ani.2015.cougar <- ani.2015 %>% select(Station, Species) ## fix THIS data frame to edit "Other"
fix(ani.2015.cougar) ## Changed Algar3 and Algar17 "Other" to "P_concolor"
ani.2015.cougar <- ani.2015.cougar[!ani.2015.cougar$Species == "Other", ]

## Remove other "Others"
## 2015 plot with cougars, no others
sp.1 <- ani.2015.cougar$Species
sp.plot1 <- rev(sort(table(sp.1)))
sp.plot1 <- as.data.frame(sp.plot1)
ggplot(data = sp.plot1, aes(x = sp.1, y = Freq)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Total Detections") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) 

### Naive occupancy (# sites species observed at/total sites)

## 2016

### Naive occupancy loop
sp7 <- c("R_tarandus","C_lupus", "U_americanus", "O_virginianus", "A_alces", "C_latrans", "L_canadensis")
naiv.occ <- NULL

for (sp in sp7) {
  spat <- as.data.frame(table(rec.2016.01[rec.2016.01$Species == sp, "Station"]))
  stp <- spat %>% filter(Freq > 0)
  naiv.occ[sp] <- nrow(stp)/56
}
naiv.occ <- as.data.frame(naiv.occ)

## Relative abundance loop (# detections/1000TD)
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
ggplot(desc2016, aes(x = Species, y = naiv.occ))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Naive Occupancy") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("C_lupus","O_virginianus", "A_alces", "L_canadensis", "C_latrans", "R_tarandus", "U_americanus"))

## Relative abundance
ggplot(desc2016, aes(x = Species, y = rel.ab))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Detections/1000 Trap Days") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("O_virginianus", "C_lupus",  "C_latrans", "L_canadensis", "A_alces", "R_tarandus", "U_americanus"))



## Box plots for 2015 winter, 2016 summer, 2016 winter(24) for 7 target species
## Research and SP+P only
## 2015 winter (win2015), summer2016 (summer2016), winter(24)
species <- unique(ani.rec$Species)
f24 <- S2016.01[1:23, ]


f24.1 <- gather(f24, Species, Sp.detect, 1:14)
f24.1 <- f24.1[(f24.1$Species == "A_alces") | (f24.1$Species =="C_latrans") | (f24.1$Species =="C_lupus")| (f24.1$Species =="L_canadensis") | (f24.1$Species =="O_virginianus") | (f24.1$Species =="R_tarandus"), ]
ggplot(data = win2015.1, aes(x = Treatment, y = Sp.detect, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("No. Detections") + scale_fill_manual(values=c("light blue", "purple")) + scale_x_discrete(limits = c("Research", "SP+P")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species)


win2015.1 <- gather(win2015, Species, Sp.detect, 1:14)
win2015.1 <- win2015.1[(win2015.1$Species == "A_alces") | (win2015.1$Species =="C_latrans") | (win2015.1$Species =="C_lupus")| (win2015.1$Species =="L_canadensis") | (win2015.1$Species =="O_virginianus") | (win2015.1$Species =="R_tarandus"), ]
ggplot(data = win2015.1, aes(x = Treatment, y = Sp.detect, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("No. Detections") + scale_fill_manual(values=c("purple", "light blue")) + scale_x_discrete(limits = c("Research", "SP+P")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species)

summer2016.1 <- gather(summer2016, Species, Sp.detect, 1:14)
summer2016.1 <- summer2016.1[(summer2016.1$Species == "A_alces") | (summer2016.1$Species =="C_latrans") | (summer2016.1$Species =="C_lupus")| (summer2016.1$Species =="L_canadensis") | (summer2016.1$Species =="O_virginianus") | (summer2016.1$Species =="R_tarandus") | (summer2016.1$Species == "U_americanus"), ]
ggplot(data = summer2016.1, aes(x = Treatment, y = Sp.detect, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("No. Detections") + scale_fill_manual(values=c("purple", "light blue")) + scale_x_discrete(limits = c("Research", "SP+P")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species)

### Box plots for 7 target species at all 56 sites


T2.2016.01 <- gather(T2016.01, Species, Sp.detect, 1:14)
T2.2016.01 <- T2.2016.01[(T2.2016.01$Species == "A_alces") | (T2.2016.01$Species =="C_latrans") | (T2.2016.01$Species =="C_lupus")| (T2.2016.01$Species =="L_canadensis") | (T2.2016.01$Species =="O_virginianus") | (T2.2016.01$Species =="R_tarandus"), ]

ggplot(data = T2.2016.01, aes(x = Treatment, y = Sp.detect, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Total Detections") + scale_x_discrete(limits=c("Human Use", "Research", "SP+P", "Nat Regen")) + scale_fill_manual(values=c("red", "light green", "light blue", "purple")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) 


#### Spatial plots
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

