##############################################
# Species_plots.R
# Plotting species detection by treatment
# Started Feb5. 2018
##############################################

library(ggplot2)
library(dplyr)
library(tidyr)

setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")

dat <- read.csv("monthlydetections_nov2015-apr2017.csv") # First 2 deployments monthly detection data + snow days
head(dat)
dat$X <- NULL
head(dat)
str(dat)

#Loading % lowland data
low <- read.csv("Lowlandcover_9buffersizes.csv")
##Add 250m and 500m to monthly detections
dat$low250 <- low$Prop250[match(dat$Site, low$CamStation)]
dat$low500 <- low$Prop500[match(dat$Site, low$CamStation)]

w <- ggplot(data = dat, aes(x = Treatment, y = Wolf, fill = Treatment)) + geom_boxplot()
w + theme_classic() + xlab("Sampling Strata") + ylab("Wolf detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

wsnow <- ggplot(data = dat, aes(x=SnowDays, y = Wolf)) +geom_point()+ theme_classic() + xlab("Snow Days/month") + ylab("Wolf detections/month")
wsnow

plot(Wolf~SnowDays,
     data = dat,
     col = alpha("azure4", 1),
     pch = 16, cex = 1.0,
     xlab = "Snow Days/month",
     ylab = "Wolf detections/month")

wolf.snow <- ggplot(data = dat, aes(x = SnowDays, y = Wolf)) + geom_point() + theme_classic() + xlab("Snow Days/month") + ylab("Wolf detections/month")
wolf.snow


#### Caribou ####
CA <- ggplot(data = dat, aes(x = Treatment, y = Caribou, fill = Treatment)) + geom_boxplot()
CA + theme_classic() + xlab("Sampling Strata") + ylab("Caribou detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

ca.low <- ggplot(data = dat, aes(x = low500, y = Caribou)) + geom_point() + theme_classic() + xlab("Proportion lowland habitat (500m buffer)") + ylab("Caribou detections/month")
ca.low


#### WTDeer ####
WTD <- ggplot(data = dat, aes(x = Treatment, y = WTDeer, fill = Treatment)) + geom_boxplot()
WTD + theme_classic() + xlab("Sampling Strata") + ylab("WTDeer detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

WTD.low <- ggplot(data = dat, aes(x = low500, y = WTDeer)) + geom_point() + theme_classic() + xlab("Proportion lowland habitat (500m buffer)") + ylab("WTDeer detections/month")
WTD.low

WTD.snow <- ggplot(data = dat, aes(x = SnowDays, y = WTDeer)) + geom_point() + theme_classic() + xlab("Snow Days/month") + ylab("WTDeer detections/month")
WTD.snow

## Moose
MOO <- ggplot(data = dat, aes(x = Treatment, y = Moose, fill = Treatment)) + geom_boxplot()
MOO + theme_classic() + xlab("Sampling Strata") + ylab("Moose detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))


#### Updated individual species plots
updat <- read.csv("MonthlyDetections_nov2015-nov2017.csv")

## Wolf
ggplot(data = updat, aes(x = Treatment, y = Wolf, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Wolf detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

# Caribou
ggplot(data = updat, aes(x = Treatment, y = Caribou, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Caribou detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

# WTDeer
ggplot(data = updat, aes(x = Treatment, y = WTDeer, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("WT deer detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

# Moose
ggplot(data = updat, aes(x = Treatment, y = Moose, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Moose detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))


### Practice plots
# Add up total detections
dat$Total <- apply(dat[ , 5:11], 1, sum)
ggplot(data = dat, aes(x = low500, y = Total, fill = Treatment)) + geom_point()






## Box plots of detection per month have too many zeros --> log transform for graphical purposes
## log(0) is non-finite, which is dropped --> okay for graphing?
## Wolf
w <- ggplot(data = dat, aes(x = Treatment, y = log(Wolf), fill = Treatment)) + geom_boxplot()
w + theme_classic() + xlab("Sampling Strata") + ylab("log(Wolf detections/month)") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

## Caribou
CA <- ggplot(data = dat, aes(x = Treatment, y = log(Caribou), fill = Treatment)) + geom_boxplot()
CA + theme_classic() + xlab("Sampling Strata") + ylab("log(Caribou detections/month)") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP")) + scale_fill_manual(values=c("orange", "red", "purple")) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

#### WTDeer ####
WTD <- ggplot(data = dat, aes(x = Treatment, y = log(WTDeer), fill = Treatment)) + geom_boxplot()
WTD + theme_classic() + xlab("Sampling Strata") + ylab("log(WTDeer detections/month)") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))


## Moose
MOO <- ggplot(data = dat, aes(x = Treatment, y = log(Moose), fill = Treatment)) + geom_boxplot()
MOO + theme_classic() + xlab("Sampling Strata") + ylab("log(Moose detections/month)") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

# Blackbear (using truncated bear dataset (summer 2016, April 2017))
ggplot(data = bear, aes(x = Treatment, y = log(Blackbear), fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("log(Blackbear detections/month)") + scale_x_discrete(limits=c("Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))


### Updat = 2 years of data nov 2015-2017
## Wolf
ggplot(data = updat, aes(x = Treatment, y = log(Wolf), fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("log(Wolf detections/month)") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

# Caribou
ggplot(data = updat, aes(x = Treatment, y = log(Caribou), fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("log(Caribou detections/month)") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

# WTDeer
ggplot(data = updat, aes(x = Treatment, y = log(WTDeer), fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("log(WT deer detections/month)") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

# Moose
ggplot(data = updat, aes(x = Treatment, y = log(Moose), fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("log(Moose detections/month)") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))


## Black bear - truncating full data set (for plot only - doesnt have all the covariates for modelling)
bear2 <- updat %>% filter(Yr_Month == "2016-04" | Yr_Month == "2016-05" | Yr_Month == "2016-06"| Yr_Month == "2016-07"| Yr_Month == "2016-08"| Yr_Month == "2016-09"| Yr_Month == "2016-10"| Yr_Month == "2017-04" |Yr_Month == "2017-05" |Yr_Month == "2017-06" |Yr_Month == "2017-07" |Yr_Month == "2017-08" |Yr_Month == "2017-09" |Yr_Month == "2017-10") %>% select(Site, Treatment,Yr_Month, Site_ym, Blackbear)

## Boxplot
ggplot(data = bear2, aes(x = Treatment, y = log(Blackbear), fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("log(Blackbear detections/month)") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))

## not log scale
ggplot(data = bear2, aes(x = Treatment, y = Blackbear, fill = Treatment)) + geom_boxplot() + theme_classic() + xlab("Sampling Strata") + ylab("Blackbear detections/month") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red",  "light green", "purple" )) +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 0, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14))



### Plotting all updated data as detections/1000 trap days
## Can't use for presentation because it uses data I am not presenting
## From ANOVAs_2years.R
S <- read.csv("detectionsByStation.csv")

## First need trap days by treatment type
##Active days by station:
library(camtrapR)

StatData <- read.csv("Station_data/AlgarStations60.csv")
S <- read.csv("detectionsByStation.csv")


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
camEff$Total[32] #Algar32 only active for 8 days

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

S7 <- gather(S, Species, Sp.detect, 2:20) #Confirm which columns correspond to species
S7 <- S7[(S7$Species == "Moose") | (S7$Species =="Coyote") | (S7$Species =="Wolf")| (S7$Species =="Lynx") | (S7$Species =="WT deer") | (S7$Species =="Caribou") | (S7$Species == "Black bear"), ] ## dataframe for 7 species and their detections



### Add detection rates to S7
S7$trapdays <- TD_treat$trapdays[match(S7$Treatment,TD_treat$Treatment)]
# Per 1000 days
S7$det1000 <- (S7$Sp.detect/S7$trapdays)*1000
# Per 100 days
S7$det100 <- (S7$Sp.detect/S7$trapdays)*100

## Detection rates for Wolves
WolfDR <- S7 %>% filter(Species == "Wolf") %>% select(X, Treatment, Species, Sp.detect, trapdays, det1000, det100)
#1000 camtrap days
ggplot(data = WolfDR, aes(Treatment, y = det1000, fill = Treatment)) + geom_boxplot() +theme_classic() + xlab("Sampling Strata") + ylab("Detections/1000 Trap Days") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red", "lightgreen", "purple")) + guides(fill = guide_legend(title = NULL)) + theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + theme(strip.text = element_text(colour = "black", size = 14)) + theme(aspect.ratio=1)

#100 camtrap days
ggplot(data = WolfDR, aes(Treatment, y = det100, fill = Treatment)) + geom_boxplot() +theme_classic() + xlab("Sampling Strata") + ylab("Detections/100 Trap Days") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red", "lightgreen", "purple")) + guides(fill = guide_legend(title = NULL)) + theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + theme(strip.text = element_text(colour = "black", size = 14)) + theme(aspect.ratio=1)

##100 and 1000 CT day plots are identical, just different scales. Will use 1000 so det rate is >1


## Detection rates for Black bears (detection rates will not be accurate b/c they include active days in winter)
## Will need to do something different for black bears to restrict active days?
BearDR <- S7 %>% filter(Species == "Black bear") %>% select(X, Treatment, Species, Sp.detect, trapdays, det1000, det100)
#1000 camtrap days
ggplot(data = BearDR, aes(Treatment, y = det1000, fill = Treatment)) + geom_boxplot() +theme_classic() + xlab("Sampling Strata") + ylab("Detections/1000 Trap Days") + scale_x_discrete(limits=c("HumanUse", "Control", "SPP", "NatRegen")) + scale_fill_manual(values=c("orange", "red", "lightgreen", "purple")) + guides(fill = guide_legend(title = NULL)) + theme(legend.position = "none") +theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 14)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 14)) + theme(strip.text = element_text(colour = "black", size = 14)) + theme(aspect.ratio=1)


### Bar graph of species detections for 5 target species (original code from Algar_report(Erin).R)
All.rec <- read.csv("recordTable_nov2015-apr2017.csv")
sp.1 <- All.rec$Species
sp.plot1 <- rev(sort(table(sp.1)))
sp.plot1 <- as.data.frame(sp.plot1) ##data frame summing detections --> fix scientific names to common
fix(sp.plot1)

ggplot(data = sp.plot1, aes(x = sp.1, y = Freq)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + theme_classic() + xlab("Species") + ylab("Total Detections") + theme(axis.text.x = element_text(angle = 25, hjust = 1, colour = "black", size = 24)) + scale_x_discrete(limits = c("White-tailed deer", "Grey wolf", "Black bear", "Moose", "Woodland caribou"))  + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 24)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 24)) + theme(axis.text.y = element_text(colour = "black", size = 16))
