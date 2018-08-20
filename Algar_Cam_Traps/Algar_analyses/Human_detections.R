########################################
## Human_detections.R
## Exploring dets of humans by treatment
## Started July 13, 2018
########################################

library(camtrapR)
library(reshape2)	# for formatting data frames
# library(plyr) need for renaming Treatments for consistency, but conflicts with dplyr
library(dplyr)		# only load after using revalue function, for applying functions to subsets of data frames
library(ggplot2)	# for data visualization
library(stringr)	# for working with character strings
library(tidyr)		# for data formatting functions



setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/Data")


daily <- read.csv(file.choose()) #Detections by day, 30 months
rec <- read.csv(file.choose()) # record table

str(daily)
str(rec)

plot(daily$Treatment, daily$Human)
plot(daily$Datep, daily$Human) ## Detections all in winter

sum(daily$Human) #94

human <- aggregate(Human~Treatment, daily, function(x) sum(x))
## Control - 12
## Human Use - 75
## NatRegen - 0 
## SPP - 7

# create Site x Species matrix
S <- as.data.frame.matrix(table(rec$Station,rec$Species))
str(S)

colnames(S) <- make.names(colnames(S), unique = T)
colnames(S)

S$Treatment <- daily$Treatment[match(row.names(S), daily$Site)]

h2 <- aggregate(Homo.sapiens~Treatment, S, function(x) sum(x))
## Same as daily counts


### Plot Human detections by date
## Remove Offline sites
daily <- daily %>% filter(Treatment != "OffLine")
daily$Date <- as.Date(daily$Datep)
ggplot(data = daily, aes(x=Date, y = Human, col = Treatment)) + geom_point() + scale_color_manual(values = c("orange", "red", "green", "purple"))+ theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 24)) + theme(axis.title.x = element_text(angle = 0, colour = "black", size = 28)) + theme(axis.title.y = element_text(angle = 90, colour = "black", size = 28)) + theme(axis.text = element_text(colour = "black", size = 24)) + scale_y_continuous(limits = c(1, 4)) + scale_x_date() + theme_classic() ## x-axis needs to be formatted


###### Same plots for seismic monthly detections
det <- read.csv(file.choose())
det <- det[,4:18]
str(det)
## Problem: det does not include human monthly detections

## Copy code from 'Data_cleaning...' to aggregate human detections by month
####--- aggregate species detection data by month ####
daily$StudyDay.date <- as.Date(daily$StudyDay,origin = "2015-11-04") # Origin = day before first detection

daily$Year <- as.factor(format(as.Date(daily$StudyDay.date), "%Y")) ##Separating date info into year, month, and year_month (to distinguish Nov.2015 from Nov. 2016)
daily$Month <- as.factor(format(as.Date(daily$StudyDay.date), "%b"))
daily$Yr_Month <- as.factor(format(as.Date(daily$StudyDay.date), "%Y-%m"))

##Combining each species' detections by month
m.hare <- daily %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Hare, na.rm = TRUE))
colnames(m.hare) <- c("Site","Treatment","Yr_Month","Hare")
summary(m.hare) 
sum(m.hare$Hare)

m.human <- daily %>%
  group_by(Site, Treatment, Yr_Month) %>% 
  summarise(sum(Human, na.rm = TRUE))
colnames(m.human) <- c("Site","Treatment","Yr_Month","Human")
summary(m.human) # max 19 obs in one month
sum(m.human$Human)

## Combining into seismic data frame
m.hare$Site_ym <- paste(m.hare$Site,m.hare$Yr_Month)
det$Hare <- m.hare$Hare[match(det$Site_ym,m.hare$Site_ym)]

m.human$Site_ym <- paste(m.human$Site,m.human$Yr_Month)
det$Human <- m.human$Human[match(det$Site_ym,m.human$Site_ym)]

str(det)
summary(det)

write.csv(det, "Seismic_nov2015-apr2018-HareHuman.csv")

#### NOW plot human detections

hum <- ggplot(data = det, aes(x=Yr_Month, y = Human, col = Treatment, shape=Treatment, size = Treatment)) + geom_point(alpha=2/3) + scale_color_manual(values = c("orange", "red", "green", "purple")) + scale_size_manual(values = c(5,4,4,5)) + scale_shape_manual(values = c(16,16,17,17)) + scale_x_discrete(name = "Year-Month", breaks = c("2015-11", "2016-04", "2016-11", "2017-04", "2017-11", "2018-04"))+ theme_classic()+ scale_y_continuous(limits= c(1,NA)) + geom_jitter(width = 0, height = 1)

hum + theme(legend.position="none") + theme(axis.text.x = element_text(colour = "black", size = 16)) + theme(axis.title.x = element_text(colour = "black", size = 30)) + ylab("Human Detections") + theme(axis.text.y = element_text(colour = "black", size = 24)) + theme(axis.title.y = element_text(colour = "black", size = 30)) + theme(axis.text.x = element_text(colour = "black", size = 26)) + theme(plot.margin = unit(c(1,1.25,0.5,0.5), "cm")) 

### Wolf detections
wol <- ggplot(data = det, aes(x=Yr_Month, y = Wolf, col = Treatment, shape=Treatment, size = Treatment)) + geom_point(alpha=2/3) + scale_color_manual(values = c("orange", "red", "green", "purple")) + scale_size_manual(values = c(5,4,4,5)) + scale_shape_manual(values = c(16,16,17,17)) + scale_x_discrete(name = "", breaks = c("2015-11", "2016-04", "2016-11", "2017-04", "2017-11", "2018-04"))+ theme_classic() + scale_y_continuous(limits= c(1,NA)) + geom_jitter(width = 0, height = 1)

wol + theme(legend.position="none") + theme(axis.text.x = element_text(colour = "black", size = 16)) + theme(axis.title.x = element_text(colour = "black", size = 20)) + ylab("Wolf Detections") + theme(axis.text.y = element_text(colour = "black", size = 24)) + theme(axis.title.y = element_text(colour = "black", size = 30)) + theme(axis.text.x = element_text(colour = "black", size = 26)) + theme(plot.margin = unit(c(1,1.25,0.5,0.5), "cm")) 


### Black bear detections
bbear <- ggplot(data = det, aes(x=Yr_Month, y = Blackbear, col = Treatment, shape=Treatment, size = Treatment)) + geom_point(alpha=2/3) + scale_color_manual(values = c("orange", "red", "green", "purple")) + scale_size_manual(values = c(5,4,4,5)) + scale_shape_manual(values = c(16,16,17,17)) + scale_x_discrete(name = "", breaks = c("2015-11", "2016-04", "2016-11", "2017-04", "2017-11", "2018-04"))+ theme_classic() + scale_y_continuous(limits= c(1,NA)) + geom_jitter(width = 0, height = 1)

bbear + theme(legend.position="none") + theme(axis.text.x = element_text(colour = "black", size = 16)) + theme(axis.title.x = element_text(colour = "black", size = 20)) + ylab("Black Bear Detections") + theme(axis.text.y = element_text(colour = "black", size = 24)) + theme(axis.title.y = element_text(colour = "black", size = 30)) + theme(axis.text.x = element_text(colour = "black", size = 26)) + theme(plot.margin = unit(c(1,1.25,0.5,0.5), "cm")) 

### Caribou detections
int_breaks <- function(x, n = 5) pretty(x, n)[pretty(x, n) %% 1 == 0] ## function for setting breaks as integers

car <- ggplot(data = det, aes(x=Yr_Month, y = Caribou, col = Treatment, shape=Treatment, size = Treatment)) + geom_point(alpha=2/3) + scale_color_manual(values = c("orange", "red", "green", "purple")) + scale_size_manual(values = c(5,4,4,5)) + scale_shape_manual(values = c(16,16,17,17)) + scale_x_discrete(name = "Year-Month", breaks = c("2015-11", "2016-04", "2016-11", "2017-04", "2017-11", "2018-04"))+ theme_classic() + scale_y_continuous(limits= c(1,NA), breaks = int_breaks) + geom_jitter(width = 0, height = 1)

car + theme(legend.position="none") + theme(axis.text.x = element_text(colour = "black", size = 16)) + theme(axis.title.x = element_text(colour = "black", size = 20)) + ylab("Caribou Detections") + theme(axis.text.y = element_text(colour = "black", size = 24)) + theme(axis.title.y = element_text(colour = "black", size = 30)) + theme(axis.text.x = element_text(colour = "black", size = 26)) + theme(plot.margin = unit(c(1,1.25,0.5,0.5), "cm")) 

### WTD detections
wtd <- ggplot(data = det, aes(x=Yr_Month, y = WTDeer, col = Treatment, shape=Treatment, size = Treatment)) + geom_point(alpha=2/3) + scale_color_manual(values = c("orange", "red", "green", "purple")) + scale_size_manual(values = c(5,4,4,5)) + scale_shape_manual(values = c(16,16,17,17)) + scale_x_discrete(name = "", breaks = c("2015-11", "2016-04", "2016-11", "2017-04", "2017-11", "2018-04"))+ theme_classic() + scale_y_continuous(limits= c(1,20), breaks = c(5,10,15,20)) + geom_jitter(width = 0, height = 1)

wtd + theme(legend.position="none") + theme(axis.text.x = element_text(colour = "black", size = 16)) + theme(axis.title.x = element_text(colour = "black", size = 20)) + ylab("WT Deer Detections") + theme(axis.text.y = element_text(colour = "black", size = 24)) + theme(axis.title.y = element_text(colour = "black", size = 30)) + theme(axis.text.x = element_text(colour = "black", size = 26)) + theme(plot.margin = unit(c(1,1.25,0.5,0.5), "cm"))

### Moose detections
Moo <- ggplot(data = det, aes(x=Yr_Month, y = Moose, col = Treatment, shape=Treatment, size = Treatment)) + geom_point(alpha=2/3) + scale_color_manual(values = c("orange", "red", "green", "purple")) + scale_size_manual(values = c(5,4,4,5)) + scale_shape_manual(values = c(16,16,17,17)) + scale_x_discrete(name = "", breaks = c("2015-11", "2016-04", "2016-11", "2017-04", "2017-11", "2018-04"))+ theme_classic() + scale_y_continuous(limits= c(1,NA)) + geom_jitter(width = 0, height = 1)

Moo + theme(legend.position="none") + theme(axis.text.x = element_text(colour = "black", size = 16)) + theme(axis.title.x = element_text(colour = "black", size = 20)) + ylab("Moose Detections") + theme(axis.text.y = element_text(colour = "black", size = 24)) + theme(axis.title.y = element_text(colour = "black", size = 30)) + theme(axis.text.x = element_text(colour = "black", size = 26)) + theme(plot.margin = unit(c(1,1.25,0.5,0.5), "cm"))

## Zero-detections removed for clarity
## Vertical jitter added to display multiple sites with same number of detections within same month
