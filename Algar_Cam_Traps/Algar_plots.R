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

### Detection histograms
sp_detect <- ani.rec$Species
st_detect <- ani.rec$Station

### Frequency histograms

par(mfrow = c(1,1))## Multiple plots on same page (2 rows, 1 column)

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

##For each species, make object occ.species (to be gathered into table later)
## 2016

### Naive occupancy loop
sp7 <- c("R_tarandus","C_lupus", "U_americanus", "O_virginianus", "A_alces", "C_latrans", "L_canadensis")
naiv.occ <- NULL

for (sp in sp7) {
  spat <- as.data.frame(table(win2016.f24[win2016.f24$Species == sp, "Station"]))
  stp <- spat %>% filter(Freq > 0)
  naiv.occ[sp] <- nrow(stp)/24
}
naiv.occ <- as.data.frame(naiv.occ)

## Relative abundance loop (# detections/1000TD)
rel.ab <- NULL
for (sp in sp7) {
  spat <- as.data.frame(table(win2016.f24[win2016.f24$Species == sp, "Station"]))
  stp <- spat %>% filter(Freq > 0)
  rel.ab[sp] <- (sum(stp$Freq)/3559)*1000
  }
rel.ab <- as.data.frame(rel.ab)

## Data frame of naive occupancy and relative abundance
desc2016w <- cbind(naiv.occ, rel.ab, deparse.level = 1)
desc2016w$Species <- row.names(desc2016w)

## Naive occupancy
ggplot(desc2016w, aes(x = Species, y = naiv.occ))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Naive Occupancy") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("C_lupus","O_virginianus", "L_canadensis", "C_latrans", "A_alces", "R_tarandus", "U_americanus"))

## Relative abundance
ggplot(desc2016w, aes(x = Species, y = rel.ab))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Detections/1000 Trap Days") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("O_virginianus", "C_lupus",  "C_latrans", "L_canadensis", "A_alces", "R_tarandus", "U_americanus"))



## Box plots for 2015 winter, 2016 summer, 2016 winter(24) for 7 target species
## Research and SP+P only
## 2015 winter (win2015), summer2016 (summer2016), winter(24)
species <- unique(ani.rec$Species)
f24 <- S2016.01[1:23, ]


f24.1 <- gather(f24, Species, Sp.detect, 1:14) %>% filter(Species == sp7)
f24.1 <- f24.1[!f24.1$Species == "U_americanus", ]
ggplot(data = f24.1, aes(x = Treatment, y = Sp.detect, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("No. Detections") + scale_fill_manual(values=c("light blue", "purple")) + scale_x_discrete(limits = c("Research", "SP+P")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species)


win2015 <- win2015[-25, ]
win2015.1 <- gather(win2015, Species, Sp.detect, 1:14) %>% filter(Species == sp7)
win2015.1 <- win2015.1[!win2015.1$Species == "U_americanus", ]
ggplot(data = win2015.1, aes(x = Treatment, y = Sp.detect, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("No. Detections") + scale_fill_manual(values=c("purple", "light blue")) + scale_x_discrete(limits = c("Research", "SP+P")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species)

summer2016.1 <- gather(summer2016, Species, Sp.detect, 1:14) %>% filter(Species == sp7)
ggplot(data = summer2016.1, aes(x = Treatment, y = Sp.detect, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("No. Detections") + scale_fill_manual(values=c("light blue", "purple")) + scale_x_discrete(limits = c("Research", "SP+P")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species)

### Box plots for 7 target species at all 60 sites
S2.2016.01 <- gather(S2016.01, Species, Sp.detect, 1:14) %>% filter(Species == sp7)
S2.2016.01 <- S2.2016.01[!S2.2016.01$Species == "U_americanus", ]

ggplot(data = S2.2016.01, aes(x = Treatment, y = Sp.detect, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Total Detections") + scale_x_discrete(limits=c("Human Use", "Research", "SP+P", "Nat Regen")) + scale_fill_manual(values=c("red", "light green", "light blue", "purple")) + guides(fill = guide_legend(title = NULL)) + facet_wrap( ~ Species) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"))