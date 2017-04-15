#############################################################
# Algar_prelim_analysis.R
# created by Joanna Burgar, 13-Apr-2017
# script for running basic analyses for first year of Algar data
#############################################################


library(reshape2)	# for formatting data frames
library(dplyr)		# for applying functions to subsets of data frames
library(ggplot2)	# for data visualization
library(stringr)	# for working with character strings
library(tidyr)		# for data formatting functions
library(knitr)		# for the "kable" function for formatting tables


###--- set directory, load files, check that they loaded correctly
### Modifying Jo's script to fit my directory/data
setwd("C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2015.01")
rec.spec <- read.csv("detections_fullrecord_pilot.csv")
glimpse(rec.spec)
str(rec.spec)

###--- Add columns for the Date.Time and Date in POSIX format:
rec.spec$Date.Time <- as.POSIXct(strptime(rec.spec$DateTimeOriginal, format = "%Y-%m-%d %T"))
rec.spec$Datep <- as.POSIXct(strptime(rec.spec$Date, format = "%Y-%m-%d"))
str(rec.spec)

rec.spec$Treatment <- as.factor(ifelse(rec.spec$Station=="Algar1"|rec.spec$Station=="Algar2"|rec.spec$Station=="Algar3"|rec.spec$Station=="Algar4"|
                rec.spec$Station=="Algar5"|rec.spec$Station=="Algar6"|rec.spec$Station=="Algar7"|rec.spec$Station=="Algar8"|
                  rec.spec$Station=="Algar9"|rec.spec$Station=="Algar10"|rec.spec$Station=="Algar11"|rec.spec$Station=="Algar12",
                     "Legacy", "Restored"))

glimpse(rec.spec)
summary(rec.spec)

#############################################################
###--- Transform data to have counts of species detections per site
###--- Need to think about how you want to structure this - detections per site per day, per week, per month?
###--- Data is in date format so relatively simple to group by date frequencies once decided

rec.spec$count <- 1 #add count column - 1 indiv/detection per row

levels(rec.spec$Species)

d.bear <- rec.spec %>%
  filter(Species == "U_americanus") %>%
  group_by(Station, Datep) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.bear) <- c("Site","Date","Blackbear")
glimpse(d.bear)

d.caribou <- rec.spec %>%
  filter(Species == "R_tarandus") %>%
  group_by(Station, Datep) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.caribou) <- c("Site","Date","Caribou")
glimpse(d.caribou)

d.coyote <- rec.spec %>%
  filter(Species == "C_latrans") %>%
  group_by(Station, Datep) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.coyote) <- c("Site","Date","Coyote")
glimpse(d.coyote)

d.lynx <- rec.spec %>%
  filter(Species == "L_canadensis") %>%
  group_by(Station, Datep) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.lynx) <- c("Site","Date","Lynx")
glimpse(d.lynx)

d.wolf <- rec.spec %>%
  filter(Species == "C_lupus") %>%
  group_by(Station, Datep) %>% 
  summarise(sum(count, na.rm = TRUE))
colnames(d.wolf) <- c("Site","Date","Wolf")
glimpse(d.wolf)

names(rec.spec)
data2 <- rec.spec[c("Station","Treatment")]
colnames(data2) <- c("Site","Treatment")
str(data2)

data2$Blackbear <- d.bear$Blackbear[match(data2$Site, d.bear$Site)]
data2$Caribou <- d.caribou$Caribou[match(data2$Site, d.caribou$Site)]
data2$Coyote <- d.coyote$Coyote[match(data2$Site, d.coyote$Site)]
data2$Lynx <- d.lynx$Lynx[match(data2$Site, d.lynx$Site)]
data2$Wolf <- d.wolf$Wolf[match(data2$Site, d.wolf$Site)]

summary(data2)

data2[is.na(data2)] <- 0 ##Co-erces NA's into 0's
summary(data2 )

sum(data2$Blackbear) #1007
sum(data2$Caribou) #273
sum(data2$Coyote) #679
sum(data2$Lynx) #693
sum(data2$Wolf) #933

### data2 shows detections of target species (caribou, wolvf, bear,coyote, lynx) by Site and Treatment
## No date recorded. Once I know how I want to group my data (by month, week, etc,) each group can be shown like this

#############################################################
###--- Simple poisson regressions
###--- Check if there is a difference in species use of Legacy vs Restored Seismic lines
###--- Run mixed effect models with site as a random effect

library(lme4)

m1.bear <- glmer(Blackbear ~ Treatment + (1|Site), family=poisson, data=data2) 
m0.bear <- glm(Blackbear ~ Treatment, family=poisson, data=data2) 
anova(m1.bear, m0.bear)
summary(m1.bear)

m1.caribou <- glmer(Caribou ~ Treatment + (1|Site), family=poisson, data=data2) # need to check...too few detections?
m0.caribou <- glm(Caribou ~ Treatment, family=poisson, data=data2) 
anova(m1.caribou, m0.caribou)
summary(m1.caribou)

m1.coyote <- glmer(Coyote ~ Treatment + (1|Site), family=poisson, data=data2) 
m0.coyote <- glm(Coyote ~ Treatment, family=poisson, data=data2) 
anova(m1.coyote,m0.coyote)
summary(m1.coyote)

m1.lynx <- glmer(Lynx ~ Treatment + (1|Site), family=poisson, data=data2) 
m0.lynx <- glm(Lynx ~ Treatment, family=poisson, data=data2) 
anova(m1.lynx, m0.lynx)
summary(m1.lynx)

m1.wolf <- glmer(Wolf ~ Treatment + (1|Site), family=poisson, data=data2) 
m0.wolf <- glm(Wolf ~ Treatment, family=poisson, data=data2) 
anova(m1.wolf,m0.wolf)
summary(m1.wolf)


#############################################################
###--- Simple poisson regressions
###--- Check if there is a difference in species use of Legacy vs Restored Seismic lines
###--- Run mixed effect models with site as a random effect
###--- Is there a wolf influence?

library(lme4)

m2.bear <- glmer(Blackbear ~ Treatment + Wolf + (1|Site), family=poisson, data=data2) 
anova(m2.bear,m1.bear, m0.bear) #m1.bear still the best


#m2.caribou <- glmer(Caribou ~ Treatment + Wolf + (1|Site), family=poisson, data=data2) # need to check...too few detections?
#anova(m2.caribou,m1.caribou, m0.caribou)

m2.coyote <- glmer(Coyote ~ Treatment + Wolf + (1|Site), family=poisson, data=data2) 
anova(m2.coyote,m1.coyote,m0.coyote)
summary(m2.coyote)

m2.lynx <- glmer(Lynx ~ Treatment + Wolf + (1|Site), family=poisson, data=data2) 
anova(m2.lynx,m1.lynx, m0.lynx)
summary(m2.lynx)

m2.wolf <- glmer(Wolf ~ Treatment + Caribou + (1|Site), family=poisson, data=data2) 
anova(m2.wolf,m1.wolf,m0.wolf) #m1.wolf still the best
