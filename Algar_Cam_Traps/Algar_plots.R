###### Graphing summary stats for Algar 2016.01 deployment
### Started 6 June, 2017 by Erin. T
### Edited May 23, 2018 for nov 2015- apr 2018 --> both full survey and seismic only

library(ggplot2)
library(dplyr)
library(tidyr)
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

## Loading recordTable and deployment data
rec <- read.csv("AlgarRecordTable_nov2015-apr2018.csv")
dep <- read.csv("Station_data/AlgarStations_DeploymentData.csv")
str(rec)

#Need 2 sets: seismic only between Apr 2017- Nov 2017, and all stations between nov 2017-apr2018
# Summer
rec$Date <- as.Date(rec$Date)
t1 <- as.Date("2017-04-01")
t2 <- as.Date("2017-11-09")
Sum.rec <- rec[rec$Date %in% t1:t2, ]

## Converting recorTable to site x species matrix
sp_detect <- Sum.rec$Species
st_detect <- Sum.rec$Station
Summer <- as.data.frame.matrix(table(st_detect,sp_detect)) #Creates matrix where stations = rows, species = columns


# species totals to compare with table(sp_detect)
apply(Summer,2,sum)
table(sp_detect)

# number of sites per species
sp.sites <- apply(Summer,2,function(x) sum(ifelse(x>0,1,0)))
sp.plot2 <- as.data.frame(rev(sort(sp.sites)))
colnames(sp.plot2) <- c("NumSites")
fix(sp.plot2) #Changing scientific names to common
sp.plot2$Naiv.occ <- sp.plot2$NumSites/60

## Naive Occupancy (proportion of sites where species was detected)
ggplot(data = sp.plot2, aes(x = row.names(sp.plot2), y = Naiv.occ)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Naive Occupancy") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("Black bear", "White-tailed deer", "Wolf", "Moose", "Woodland caribou", "Lynx", "Coyote"))

# add total count and total species (richness) for each site
Summer$Total <- apply(Summer,1,sum)

Summer$Richness <- apply(Summer[,1:19],1,function(x) sum(ifelse(x>0,1,0)))
## Edit to Cole's code: S$Richness included total in Richness count. Adding [,1:19] to specify counting only species 
## columns

# add coordinates and treatment to dataframe (here dep is a separate dataframe with station data)
Summer$utmE <- dep$utmE[match(row.names(Summer),dep$CamStation)]
Summer$utmN <- dep$utmN[match(row.names(Summer),dep$CamStation)]

Summer$Treatment <- dep$Treatment[match(row.names(Summer),dep$CamStation)]


#### Full survey, winter
rec$Date <- as.Date(rec$Date)
t1 <- as.Date("2017-11-10")
t2 <- as.Date("2018-04-11")
Wint.rec <- rec[rec$Date %in% t1:t2, ]

## Converting recorTable to site x species matrix
sp_detect <- Wint.rec$Species
st_detect <- Wint.rec$Station
Winter <- as.data.frame.matrix(table(st_detect,sp_detect)) #Creates matrix where stations = rows, species = columns

# species totals to compare with table(sp_detect)
apply(Winter,2,sum)
table(sp_detect)

# number of sites per species
sp.sites <- apply(Winter,2,function(x) sum(ifelse(x>0,1,0)))
sp.plot3 <- as.data.frame(rev(sort(sp.sites)))
colnames(sp.plot3) <- c("NumSites")
fix(sp.plot3) #Changing scientific names to common
sp.plot3$Naiv.occ <- sp.plot3$NumSites/71

ggplot(data = sp.plot2, aes(x = row.names(sp.plot2), y = Naiv.occ)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Naive Occupancy") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c( "White-tailed deer","Wolf", "Moose", "Woodland caribou","Lynx", "Coyote"))

# add total count and total species (richness) for each site
S2$Total <- apply(S2,1,sum)

S2$Richness <- apply(S2[,1:19],1,function(x) sum(ifelse(x>0,1,0)))
## Edit to Cole's code: S$Richness included total in Richness count. Adding [,1:19] to specify counting only species 
## columns

# add coordinates and treatment to dataframe (here dep is a separate dataframe with station data)
S2$utmE <- dep$utmE[match(row.names(S2),dep$CamStation)]
S2$utmN <- dep$utmN[match(row.names(S2),dep$CamStation)]

S2$Treatment <- dep$Treatment[match(row.names(S2),dep$CamStation)]


### Box plots of total detections by treatment
## Seismic only, Apr 2017- 2018
ggplot(data = S, aes(x = Treatment, y = Total, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Total Detections") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red", " light green", "purple"))
## Full survey, Nov 2017 - Apr 2018
ggplot(data = S2, aes(x = Treatment, y = Total, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Total Detections") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen", "OffLine")) + scale_fill_manual(values=c("orange", "red", " light green", "dark blue", "purple"))

###### Species Richness boxplots
## Seismic only, Apr 2017- 2018
ggplot(data = S, aes(x = Treatment, y = Richness, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Species Richness") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red", " light green", "purple"))
## Full survey, Nov 2017 - Apr 2018
ggplot(data = S2, aes(x = Treatment, y = Richness, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Treatment Type") + ylab("Species Richness") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen", "OffLine")) + scale_fill_manual(values=c("orange", "red", " light green", "dark blue", "purple"))


##### Naive occupancy measures for Seismic, nov 2015-apr 2018
## Converting recorTable to site x species matrix
sp_detect <- rec$Species
st_detect <- rec$Station
S3 <- as.data.frame.matrix(table(st_detect,sp_detect)) #Creates matrix where stations = rows, species = columns
S3 <- S3[1:60, ] ## Seismic line sites only

# species totals to compare with table(sp_detect)
apply(S3,2,sum)


# number of sites per species
sp.sites <- apply(S3,2,function(x) sum(ifelse(x>0,1,0)))
sp.plot2 <- as.data.frame(rev(sort(sp.sites)))
colnames(sp.plot2) <- c("NumSites")
fix(sp.plot2) #Changing scientific names to common
sp.plot2$Naiv.Occ <- sp.plot2$NumSites/60 # naive occupancy = proportion of sites where species is present

## Total number of detections per species
S.t <- as.data.frame(t(S3)) # transpose so species are rows
S.t$Total <- rowSums(S.t)
fix(S.t) # change species names

# add total count and total species (richness) for each site
S3$Total <- apply(S3,1,sum)

S3$Richness <- apply(S3[,1:19],1,function(x) sum(ifelse(x>0,1,0)))
## Edit to Cole's code: S$Richness included total in Richness count. Adding [,1:19] to specify counting only species 
## columns

# add coordinates and treatment to dataframe (here dep is a separate dataframe with station data)
S3$utmE <- dep$utmE[match(row.names(S3),dep$CamStation)]
S3$utmN <- dep$utmN[match(row.names(S3),dep$CamStation)]

S3$Treatment <- dep$Treatment[match(row.names(S3),dep$CamStation)]


## Naive Occupancy (proportion of sites where species was detected)
ggplot(data = sp.plot2, aes(x = row.names(sp.plot2), y = Naiv.Occ)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Naive Occupancy") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("Wolf", "Black bear", "Moose", "WT deer", "Caribou"))

## Total detections
ggplot(data = S.t, aes(x = row.names(S.t), y = Total)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Total Detections") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"))  + scale_x_discrete(limits = c("WT deer", "Black bear", "Wolf", "Moose","Caribou"))

## Spatial plots
fix(S3) #Change to common names
with(S3, symbols(x=utmE, y=utmN, circles=Caribou, inches=2/3, bg="royalblue3", fg="darkblue", 
                main = "Caribou distribution"))
with(S3, symbols(x=utmE, y=utmN, circles=Wolf, inches=2/3, bg="royalblue3", fg="darkblue", 
                 main = "Wolf distribution"))
with(S3, symbols(x=utmE, y=utmN, circles=Caribou, inches=2/3, bg="royalblue3", fg="darkblue", 
                 main = "Black bear distribution"))
with(S3, symbols(x=utmE, y=utmN, circles=Coyote, inches=2/3, bg="royalblue3", fg="darkblue", 
                 main = "Coyote distribution"))
with(S3, symbols(x=utmE, y=utmN, circles=Lynx, inches=2/3, bg="royalblue3", fg="darkblue", 
                 main = "Lynx distribution"))
with(S3, symbols(x=utmE, y=utmN, circles=Moose, inches=2/3, bg="royalblue3", fg="darkblue", 
                 main = "Moose distribution"))
with(S3, symbols(x=utmE, y=utmN, circles=WTDeer, inches=2/3, bg="royalblue3", fg="darkblue", 
                 main = "White-tailed deer distribution"))

##### Relative abundance for Apr 2017 - 2018 (divided into summer and winter deployments)

sp7 <- c("Canis lupus", "Odocoileus virginianus", "Rangifer tarandus", "Alces alces", "Ursus americanus", "Canis latrans", "Lynx canadensis")
## Relative abundance loop (# detections/1000TD)
rel.ab <- NULL
for (sp in sp7) {
  spat <- as.data.frame(table(Sum.rec[Sum.rec$Species == sp, "Station"]))
  stp <- spat %>% filter(Freq > 0)
  rel.ab[sp] <- (sum(stp$Freq)/7779)*1000 #(total detections / total trap days)*1000
  }
rel.ab <- as.data.frame(rel.ab)
fix(rel.ab)
rel.ab$Species <- row.names(rel.ab)
colnames(rel.ab) <- c("RA", "Species")

ggplot(rel.ab, aes(x = Species, y = RA))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Detections/1000 Trap Days") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("White-tailed deer", "Black bear",  "Wolf", "Woodland caribou", "Moose", "Coyote", "Lynx"))

### Winter 2017-18
sp6 <- c("Canis lupus", "Odocoileus virginianus", "Rangifer tarandus", "Alces alces", "Canis latrans", "Lynx canadensis")
## Relative abundance loop (# detections/1000TD)
rel.ab <- NULL
for (sp in sp6) {
  spat <- as.data.frame(table(Wint.rec[Wint.rec$Species == sp, "Station"]))
  stp <- spat %>% filter(Freq > 0)
  rel.ab[sp] <- (sum(stp$Freq)/8723)*1000 #(total detections / total trap days)*1000
}
rel.ab <- as.data.frame(rel.ab)
fix(rel.ab)
rel.ab$Species <- row.names(rel.ab)
colnames(rel.ab) <- c("RA", "Species")

ggplot(rel.ab, aes(x = Species, y = RA))  + geom_bar(stat = "identity", fill = "light blue", colour = "black") + theme_classic() + xlab("Species") + ylab("Detections/1000 Trap Days") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("White-tailed deer", "Moose", "Wolf", "Woodland caribou", "Coyote", "Lynx"))

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

