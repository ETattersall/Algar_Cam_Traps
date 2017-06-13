###### Graphing summary stats for Algar 2016.01 deployment
### Started 6 June, 2017 by Erin. T

library(ggplot2)
library(dplyr)

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
  spat <- as.data.frame(table(ani.rec[ani.rec$Species == sp, "Station"]))
  stp <- spat %>% filter(Freq > 0)
  naiv.occ[sp] <- nrow(stp)/60
}
naiv.occ <- as.data.frame(naiv.occ)

## Relative abundance loop (# detections/1000TD)
rel.ab <- NULL
for (sp in sp7) {
  spat <- as.data.frame(table(ani.rec[ani.rec$Species == sp, "Station"]))
  stp <- spat %>% filter(Freq > 0)
  rel.ab[sp] <- (sum(stp$Freq)/9025)*1000
  }
rel.ab <- as.data.frame(rel.ab)

## Data frame of naive occupancy and relative abundance
desc2016 <- cbind(naiv.occ, rel.ab, deparse.level = 1)
desc2016$Species <- row.names(desc2016)

## Naive occupancy
ggplot(desc2016, aes(x = Species, y = naiv.occ))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Naive Occupancy") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("C_lupus","O_virginianus", "A_alces", "L_canadensis",  "C_latrans", "R_tarandus", "U_americanus"))

## Relative abundance
ggplot(desc2016, aes(x = Species, y = rel.ab))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Detections/1000 Trap Days") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("O_virginianus","C_lupus", "C_latrans", "A_alces", "L_canadensis", "R_tarandus","U_americanus"))

