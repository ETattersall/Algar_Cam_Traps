####################################
## SpatTemp_Explore.R
## Exploring Algar data for spatiotemporal interactions
## May 30, 2018
###################################


library(camtrapR)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")
rec <- read.csv("AlgarRecordTable_nov2015-apr2018.csv")

str(rec)
## Exploring daily detections
day <- read.csv("Algar_DailyDetections_30months.csv")

table(rec$Species)

## Re-order
rec <- rec[order(rec$Station, rec$DateTimeOriginal), ]
unique(rec$Station)
## Site x Species matrix
## Converting recorTable to site x species matrix
sp_detect <- rec$Species
st_detect <- rec$Station
Smat <- as.data.frame.matrix(table(st_detect,sp_detect)) #Creates matrix where stations = rows, species = columns

## Fix column names to remove spaces
colnam <- make.names(colnames(Smat),unique = TRUE)
colnames(Smat) <- colnam


##### Detection plot for predators
Smat.t <- t(Smat)
det <-  as.data.frame(rowSums(Smat.t, na.rm = FALSE))
det <- cbind.data.frame(c("Coyote", "Wolf", "Lynx", "Black bear"), det[c(3,4,11,18), ]) #Predators only
colnames(det) <- c("Species", "Detections")

ggplot(data = det, aes(x = Species, y = Detections)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Total Detections") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c( "Black bear","Wolf", "Coyote", "Lynx")) + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 14)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 15)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 15)) + theme(strip.text = element_text(colour = "black", size = 14))



### Summer records (for bear overlap)
rec$Month <- as.factor(format(as.Date(rec$Datep), "%m"))
class(rec$Month) #factor

Bear.rec <- rec[which(rec$Month == "04" | rec$Month == "05" | rec$Month == "06" | rec$Month == "07" | rec$Month == "08" | rec$Month == "09" | rec$Month == "10"), ]

#Re-order
Bear.rec <- Bear.rec[order(Bear.rec$Station, Bear.rec$DateTimeOriginal), ]

# Site-species matrix
sp_detect <- rec$Species
st_detect <- rec$Station
Bmat <- as.data.frame.matrix(table(st_detect,sp_detect)) #Creates matrix where stations = rows, species = columns

## Fix column names to remove spaces
colnam <- make.names(colnames(Bmat),unique = TRUE)
colnames(Bmat) <- colnam

## Add column for station names
Smat$Station <- row.names(Smat)
Bmat$Station <- row.names(Bmat)

#### Species of interest
## Predators: wolves, black bears, coyotes, lynx
## Prey, WTDeer, Moose, Caribou, snowshoe hare, squirrel

## How many stations do predators overlap at?
## Boxplots compare number of detections at overlap sites --> do not indicate avoidance, may just be a product of abundance

## Bear sites
Bear.pres <- Bmat %>% filter(Ursus.americanus>0) %>% select(Ursus.americanus, Station)
## Wolves and bears - 38 sites of overlap
Wobear <- Bmat %>% filter(Canis.lupus>0 & Ursus.americanus>0) %>% select(Canis.lupus, Ursus.americanus, Station)
## Sites that only have wolves, no bears
Wolf1.bear0 <- Bmat %>% filter(Canis.lupus>0 & Ursus.americanus==0) %>% select(Canis.lupus, Ursus.americanus, Station) #8

## Sites that only have bears, no wolves
Wolf0.bear1 <- Bmat %>% filter(Canis.lupus==0 & Ursus.americanus>0) %>% select(Canis.lupus, Ursus.americanus, Station) #6
## Boxplot comparing no. detections for each species at shared sites
Wobear.boxplot <- gather(Wobear, key = "Species", value = "Detections", 1:2)
ggplot(data = Wobear.boxplot, aes(x = Species, y = Detections, fill = Species)) + geom_boxplot() + theme_classic() + ylab("Co-occurrance detections") + scale_fill_manual(values=c( "red", "purple"))


## Bears and coyotes - 18 sites of overlap
Coybear <- Bmat %>% filter(Canis.latrans>0 & Ursus.americanus>0) %>% select(Canis.latrans, Ursus.americanus, Station)
## Boxplot comparing no. detections for each species
Coybear.boxplot <- gather(Coybear, key = "Species", value = "Detections", 1:2)
ggplot(data = Coybear.boxplot, aes(x = Species, y = Detections, fill = Species)) + geom_boxplot() + theme_classic() + ylab("Co-occurrance detections") + scale_fill_manual(values=c( "light blue", "purple"))

## Bears and lynx - 24 sites of overlap
Lynbear <- Bmat %>% filter(Lynx.canadensis>0 & Ursus.americanus>0) %>% select(Lynx.canadensis, Ursus.americanus, Station)
## Boxplot comparing no. detections for each species
Lynbear.boxplot <- gather(Lynbear, key = "Species", value = "Detections", 1:2)
ggplot(data = Lynbear.boxplot, aes(x = Species, y = Detections, fill = Species)) + geom_boxplot() + theme_classic() + ylab("Co-occurrance detections") + scale_fill_manual(values=c( "yellow", "purple"))

###### Wolves and Coyotes - 21 sites of overlap
Coywolf<- Smat %>% filter(Canis.lupus>0 & Canis.latrans>0) %>% select(Canis.lupus, Canis.latrans, Station)


## Coyote sites
Coyote.pres <- Smat %>% filter(Canis.latrans>0) %>% select(Canis.latrans, Station)

## Sites that only have wolves, no coyotes
Wolf1.coyote0 <- Smat %>% filter(Canis.lupus>0 & Canis.latrans==0) %>% select(Canis.lupus, Canis.latrans, Station) #25

## Sites that only have coyotes, no wolves
Wolf0.coyote1 <- Smat %>% filter(Canis.lupus==0 & Canis.latrans>0) %>% select(Canis.lupus, Canis.latrans, Station) #2
## Boxplot comparing no. detections for each species
Coywolf.boxplot <- gather(Coywolf, key = "Species", value = "Detections", 1:2)
ggplot(data = Coywolf.boxplot, aes(x = Species, y = Detections, fill = Species)) + geom_boxplot() + theme_classic() + ylab("Co-occurrance detections") + scale_fill_manual(values=c( "red", "light blue"))



###### Wolves and Lynx - 23 sites of overlap
Lynwolf<- Smat %>% filter(Canis.lupus>0 & Lynx.canadensis>0) %>% select(Canis.lupus, Lynx.canadensis, Station)

## Lynx sites
Lynx.pres <- Smat %>% filter(Lynx.canadensis>0) %>% select(Lynx.canadensis, Station)

## Sites that only have wolves, no Lynx
Wolf1.Lynx0 <- Smat %>% filter(Canis.lupus>0 & Lynx.canadensis==0) %>% select(Canis.lupus, Lynx.canadensis, Station) #23

## Sites that only have Lynx, no wolves
Wolf0.Lynx1 <- Smat %>% filter(Canis.lupus==0 & Lynx.canadensis>0) %>% select(Canis.lupus, Lynx.canadensis, Station) #4
## Boxplot comparing no. detections for each species
Lynwolf.boxplot <- gather(Lynwolf, key = "Species", value = "Detections", 1:2)
ggplot(data = Lynwolf.boxplot, aes(x = Species, y = Detections, fill = Species)) + geom_boxplot() + theme_classic() + ylab("Co-occurrance detections") + scale_fill_manual(values=c( "red", "yellow"))


###### Coyotes and Lynx - 17 sites of overlap
LynCoyote<- Smat %>% filter(Canis.latrans>0 & Lynx.canadensis>0) %>% select(Canis.latrans, Lynx.canadensis, Station)

## Sites that only have Coyotes, no Lynx
Coyote1.Lynx0 <- Smat %>% filter(Canis.latrans>0 & Lynx.canadensis==0) %>% select(Canis.latrans, Lynx.canadensis, Station) #6

## Sites that only have Lynx, no Coyotes
Coyote0.Lynx1 <- Smat %>% filter(Canis.latrans==0 & Lynx.canadensis>0) %>% select(Canis.latrans, Lynx.canadensis, Station) #10
## Boxplot comparing no. detections for each species
LynCoyote.boxplot <- gather(LynCoyote, key = "Species", value = "Detections", 1:2)
ggplot(data = LynCoyote.boxplot, aes(x = Species, y = Detections, fill = Species)) + geom_boxplot() + theme_classic() + ylab("Co-occurrance detections") + scale_fill_manual(values=c( "light blue", "yellow"))


##### Activity overlap -- takes detections at all stations (regardless of overlap) to find activity density
# Record tables need species names with no spaces
rec$Spec.dot <- make.names(rec$Species, unique = FALSE)
Bear.rec$Spec.dot <- make.names(Bear.rec$Species, unique = FALSE)

## Bears and wolves
OLWobear <- activityOverlap(Bear.rec, speciesA = "Canis.lupus", speciesB = "Ursus.americanus", speciesCol = "Spec.dot", plotR = TRUE)

## Bears and coyotes
OLCoybear <- activityOverlap(Bear.rec, speciesA = "Canis.latrans", speciesB = "Ursus.americanus", speciesCol = "Spec.dot", plotR = TRUE)

## Bears and lynx
OLLynbear <- activityOverlap(Bear.rec, speciesA = "Lynx.canadensis", speciesB = "Ursus.americanus", speciesCol = "Spec.dot", plotR = TRUE)

## Wolves and coyotes
OLCoywolf <- activityOverlap(rec, speciesA = "Canis.lupus", speciesB = "Canis.latrans", speciesCol = "Spec.dot", plotR = TRUE)

## Wolves and lynx
OLLynwolf <- activityOverlap(rec, speciesA = "Canis.lupus", speciesB = "Lynx.canadensis", speciesCol = "Spec.dot", plotR = TRUE)

## Lynx and coyote
OLLynCoyote <- activityOverlap(rec, speciesA = "Canis.latrans", speciesB = "Lynx.canadensis", speciesCol = "Spec.dot", plotR = TRUE)


### Temporal overlap at shared sites? Not sure if I will have enough data...
## Coyote-lynx overlap is limiting, I will test this
# record table for overlapping sites
CL.sites <- LynCoyote$Station
CL.rec <- rec %>% filter(Station %in% CL.sites)
unique(CL.rec$Station)
unique(CL.sites) 
OL2LynCoyote <- activityOverlap(CL.rec, speciesA = "Canis.latrans", speciesB = "Lynx.canadensis", speciesCol = "Spec.dot", plotR = TRUE)

## Even when restricting to only overlap sites, can still obtain temporal overlap


### How many detections do I have per season? 
## Winter= Nov.- Apr.; Summer= Apr. - Nov.
rec$Date <- as.Date(rec$Date)

## Winter 2015- 2016
t1 <- as.Date("2015-11-01")
t2 <- as.Date("2016-03-31")
Wrec.15 <- rec[rec$Date %in% t1:t2, ]
table(Wrec.15$Species)

## Summer 2016
t1 <- as.Date("2016-04-01")
t2 <- as.Date("2016-10-31")
Srec.16 <- rec[rec$Date %in% t1:t2, ]
table(Srec.16$Species)
## Winter 2016-17
t1 <- as.Date("2016-11-01")
t2 <- as.Date("2017-03-31")
Wrec.16 <- rec[rec$Date %in% t1:t2, ]
table(Wrec.16$Species)
## Summer 2017
t1 <- as.Date("2017-04-01")
t2 <- as.Date("2017-10-31")
Srec.17 <- rec[rec$Date %in% t1:t2, ]
table(Srec.17$Species)
## Winter 2017-18
t1 <- as.Date("2017-11-01")
t2 <- as.Date("2018-03-31")
Wrec.17 <- rec[rec$Date %in% t1:t2, ]
table(Wrec.17$Species)

### Dataframe of detections
seasons <- as.data.frame(table(Wrec.15$Species))

seasons$Wrec.16 <- table(Wrec.16$Species)
seasons$Wrec.17 <- table(Wrec.17$Species)
seasons$Srec.16 <- table(Srec.16$Species)
seasons$Srec.17 <- table(Srec.17$Species)
colnames(seasons) <- c("Species", "Winter2015", "Winter2016", "Winter2017", "Summer2016", "Summer2017")

###############################################
## Proportion of sites each predator is detected --> Naive occupancy
sp_detect <- rec$Species
st_detect <- rec$Station
S3 <- as.data.frame.matrix(table(st_detect,sp_detect)) #Creates matrix where stations = rows, species = columns

# species totals to compare with table(sp_detect)
apply(S3,2,sum)


# number of sites per species
sp.sites <- apply(S3,2,function(x) sum(ifelse(x>0,1,0)))
sp.plot2 <- as.data.frame(rev(sort(sp.sites)))
colnames(sp.plot2) <- c("NumSites")

##Removing spaces from species names
rownam <- make.names(row.names(sp.plot2),unique = TRUE)
row.names(sp.plot2) <- rownam

sp.plot2$Naiv.Occ <- sp.plot2$NumSites/71
ggplot(data = sp.plot2, aes(x = row.names(sp.plot2), y = Naiv.Occ)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Naive Occupancy") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("Canis.lupus", "Ursus.americanus", "Lynx.canadensis", "Canis.latrans"))

ggplot(data = sp.plot2, aes(x = row.names(sp.plot2), y = NumSites)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Number of Occupied Sites") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) + scale_x_discrete(limits = c("Canis.lupus", "Ursus.americanus", "Lynx.canadensis", "Canis.latrans"))

### Data distributions at occupied sites

### Blackbear
### Filter for occupied sites -- if a predator is EVER detected at that site, keep all
bearSites <- row.names(Bmat[which(Bmat$Ursus.americanus>0), ])

bears <- day %>% filter(Site %in% bearSites)
unique(bears$Site)
str(bears)

##Need to also eliminate all winter dates. Stick with rough estimates of April - October for now
bears$Datep <- as.Date(bears$Datep)
t1 <- as.Date("2016-04-01")
t2 <- as.Date("2016-10-31")
sum1 <- bears[bears$Datep %in% t1:t2, ]
t3 <- as.Date("2017-04-01")
t4 <- as.Date("2017-10-31")
sum2 <- bears[bears$Datep %in% t3:t4, ]
bears <- bind_rows(sum1,sum2)

hist(bears$Blackbear)
table(bears$Blackbear) #18 517 zeroes, 38 counts that aren't 1
sum(bears$Blackbear==0, na.rm = TRUE)/nrow(bears) ## 0.98 of data is zeroes

### Lynx 
LynxSites <- row.names(Smat[which(Smat$Lynx.canadensis>0), ])
Lynx <- day %>% filter(Site %in% LynxSites)

hist(Lynx$Lynx)
table(Lynx$Lynx) #2 counts of 2, 69 counts of 1, 24000 counts of zero
sum(Lynx$Lynx==0, na.rm = TRUE)/nrow(Lynx) ## 0.998


### Coyote 
CoyoteSites <- row.names(Smat[which(Smat$Canis.latrans>0), ])
Coyote <- day %>% filter(Site %in% CoyoteSites)

hist(Coyote$Coyote)
table(Coyote$Coyote) #114 counts of 1, 13 counts of 2, 2 counts of 3 and 4 each
sum(Coyote$Coyote==0, na.rm = TRUE)/nrow(Coyote) ## 0.994


#### Changing temporal grain - week?
## Adding YrWk to data using DayLookup
day$YrWk <- DayLookup$YrWeek[match(day$StudyDay, DayLookup$StudyDay)]


### Aggregate each species
w.deer <- day %>%
  group_by(Site, Treatment, YrWk) %>% 
  summarise(sum(WTDeer, na.rm = TRUE))
colnames(w.deer) <- c("Site","Treatment","YrWk","WTDeer")
summary(w.deer)
table(w.deer$WTDeer)
sum(w.deer$WTDeer==0, na.rm = TRUE)/nrow(w.deer) # 95%

w.Wolf <- day %>%
  group_by(Site, Treatment, YrWk) %>% 
  summarise(sum(Wolf, na.rm = TRUE))
colnames(w.Wolf) <- c("Site","Treatment","YrWk","Wolf")
summary(w.Wolf)
table(w.Wolf$Wolf)
sum(w.Wolf$Wolf==0, na.rm = TRUE)/nrow(w.Wolf) # 98%

w.Blackbear <- day %>%
  group_by(Site, Treatment, YrWk) %>% 
  summarise(sum(Blackbear, na.rm = TRUE))
colnames(w.Blackbear) <- c("Site","Treatment","YrWk","Blackbear")
summary(w.Blackbear)
table(w.Blackbear$Blackbear)
sum(w.Blackbear$Blackbear==0, na.rm = TRUE)/nrow(w.Blackbear) #97%

w.Coyote <- day %>%
  group_by(Site, Treatment, YrWk) %>% 
  summarise(sum(Coyote, na.rm = TRUE))
colnames(w.Coyote) <- c("Site","Treatment","YrWk","Coyote")
summary(w.Coyote)
table(w.Coyote$Coyote)

w.Lynx <- day %>%
  group_by(Site, Treatment, YrWk) %>% 
  summarise(sum(Lynx, na.rm = TRUE))
colnames(w.Lynx) <- c("Site","Treatment","YrWk","Lynx")
summary(w.Lynx)
table(w.Lynx$Lynx)

w.Caribou <- day %>%
  group_by(Site, Treatment, YrWk) %>% 
  summarise(sum(Caribou, na.rm = TRUE))
colnames(w.Caribou) <- c("Site","Treatment","YrWk","Caribou")
summary(w.Caribou)
table(w.Caribou$Caribou)

w.Moose <- day %>%
  group_by(Site, Treatment, YrWk) %>% 
  summarise(sum(Moose, na.rm = TRUE))
colnames(w.Moose) <- c("Site","Treatment","YrWk","Moose")
summary(w.Moose)
table(w.Moose$Moose)

w.Hare <- day %>%
  group_by(Site, Treatment, YrWk) %>% 
  summarise(sum(Hare, na.rm = TRUE))
colnames(w.Hare) <- c("Site","Treatment","YrWk","Hare")
summary(w.Hare)
table(w.Hare$Hare)

w.Squirrel <- day %>%
  group_by(Site, Treatment, YrWk) %>% 
  summarise(sum(Squirrel, na.rm = TRUE))
colnames(w.Squirrel) <- c("Site","Treatment","YrWk","Squirrel")
summary(w.Squirrel)
table(w.Squirrel$Squirrel)

w.Human <- day %>%
  group_by(Site, Treatment, YrWk) %>% 
  summarise(sum(Human, na.rm = TRUE))
colnames(w.Human) <- c("Site","Treatment","YrWk","Human")
summary(w.Human)
table(w.Human$Human)

w.CamActive <- day %>%
  group_by(Site, Treatment, YrWk) %>% 
  summarise(sum(CamActive, na.rm = TRUE))
colnames(w.CamActive) <- c("Site","Treatment","YrWk","CamActive")
summary(w.CamActive)
table(w.CamActive$CamActive) ## 43 8s --> should not be there
WRONG <- w.CamActive[which(w.CamActive$CamActive == 8), ] ### all week 2017-52, 43 stations

d2 <- day[order(day$Site, day$Datep, day$YrWk), ]
d2 <- d2[which(d2$YrWk == "2017-52"), ] #### 2017-01-01 marked as 2017-52, which is incorrect
## Problem stems from Day Lookup table, where pasting Year to Week results in some days at the beginning of the Year being tacked on to the week at the end. Will need to resolve
