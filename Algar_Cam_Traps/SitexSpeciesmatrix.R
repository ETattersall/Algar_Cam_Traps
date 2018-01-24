##########################################################
# Script for creating Site by Species matrices
# Input = camera trap record table, where each row = a detection (consisting of a sequence of images
# of the same species with 30 minutes of each other)
# Columns = Species, Station, Date, Time, Time elapsed since last detection of that species, etc
#########################################################

All.rec <- read.csv("recordTable_nov2015-nov2017.csv") #Data already cleaned, just loading in
str(All.rec)
# 'data.frame':	2693 obs. of  14 variables:
# $ X               : int  1 2 3 4 5 6 7 8 9 10 ...
# $ Station         : Factor w/ 59 levels "Algar01","Algar02",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Species         : Factor w/ 19 levels "Alces alces",..: 1 1 1 14 14 14 14 14 14 18 ...
# $ DateTimeOriginal: Factor w/ 2687 levels "2015-11-05 19:55:00",..: 384 411 477 38 53 196 290 567 923 603 ...
# $ Date            : Factor w/ 630 levels "2015-11-05","2015-11-08",..: 130 136 148 14 19 92 114 164 250 171 ...
# $ Time            : Factor w/ 2655 levels "0:00:34","0:01:51",..: 2562 2299 2462 1898 2334 2599 2444 2454 1571 2332 ...
# $ delta.time.secs : int  0 498894 1050601 0 522487 11679037 1891841 4320767 7554395 0 ...
# $ delta.time.mins : int  0 8315 17510 0 8708 194651 31531 72013 125907 0 ...
# $ delta.time.hours: num  0 139 292 0 145 ...
# $ delta.time.days : num  0 5.8 12.2 0 6 ...
# $ Directory       : Factor w/ 341 levels "C:/Users/ETattersall/Desktop/Algar_Cam_Traps/Algar_Camera_Traps/2016.01/Species_Images/Algar01/A_alces",..: 174 174 174 175 175 175 175 175 175 176 ...

sp_detect <- All.rec$Species
# All.rec$Station <- toupper(All.rec$Station) ### Need to do to match CamStations in cam2016
st_detect <- All.rec$Station ##Will need to add row for Algar32 at some point (no detections)
S <- as.data.frame.matrix(table(st_detect,sp_detect)) #Creates matrix where stations = rows, species = columns

# species totals to compare with table(sp_detect)
apply(S,2,sum)
# number of sites per species
sp.sites <- apply(S,2,function(x) sum(ifelse(x>0,1,0)))
sp.plot2 <- as.data.frame(rev(sort(sp.sites)))
colnames(sp.plot2) <- c("NumSites")
fix(sp.plot2) #Changing scientific names to common

# add total count and total species (richness) for each site
S$Total <- apply(S,1,sum)

S$Richness <- apply(S[,1:19],1,function(x) sum(ifelse(x>0,1,0)))
## Edit to Cole's code: S$Richness included total in Richness count. Adding [,1:19] to specify counting only species 
## columns

# add coordinates and treatment to dataframe (here camdata is a separate dataframe with station data)
S$utmE <- camdata$utmE[match(row.names(S),camdata$CamStation)]
S$utmN <- camdata$utmN[match(row.names(S),camdata$CamStation)]

S$Treatment <- camdata$Treatment[match(row.names(S),camdata$CamStation)]