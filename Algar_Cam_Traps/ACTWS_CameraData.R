##############################################################
# ACTWS_CameraData.R
# Created by Joanna Burgar, 23-Feb-2018
# Formatting camera data to use in analyses
#############################################################

###--- Load packages
library(plyr)
library(dplyr)    # for viewing and manipulating data
library(tidyr)    # for manipulating data
library(reshape2) # for manipulating data
library(camtrapR) # for creating camera Site operability matrix

###--- Import txt file from study area camera database

getwd()
setwd("C:/Users/JBurgar/Google Drive/ACTWS/data manipulation/") # set working directory


# Site-covariate data

camloc <- read.csv("TBL-CameraLocation.csv", header = TRUE)
glimpse(camloc) # check for correct loading of data - to see data classes and initial values
summary(camloc) # overview of data and check for NAs

levels(camloc$Site) # 63 camera Sites, including 2 non-study area

camloc <- camloc[order(camloc$Dist.Road),] # to remove the non camera Sites, sort by Dist.Road
nrow(camloc) # 63 cameras, last two are not in the study area

tail(camloc)
camdf <-camloc[1:62,] # create a dataframe with only  camera Sites and covariates/UTMs
nrow(camdf) # 62 cameras

summary(camdf)

#############################################################
###--- Bring in camera effort data
cameff <- read.csv("TBL-Download.csv", header = TRUE)
glimpse(cameff) # check for correct loading of data - to see data classes and initial values
summary(cameff) # overview of data and check for NAs
View(cameff)

###--- Add columns for the Date/Time in POSIX format:
cameff$DateStartp <- as.POSIXct(strptime(cameff$DateStart, format = "%d/%m/%Y"))
cameff$DateLWp <- as.POSIXct(strptime(cameff$DateLastWorking, format = "%d/%m/%Y"))

#############################################################
###--- Combine effort and camera Site information 
###--- Create camera effort table - in camtrapR format

cameff$utm_y <- camdf$Northing[match(cameff$Site,camdf$Site)]
cameff$utm_x <- camdf$Easting[match(cameff$Site,camdf$Site)]

mindate <- aggregate(DateStartp~Site, cameff, function(x) min(x))
maxdate <- aggregate(DateLWp~Site, cameff, function(x) max(x))

mindate <- mindate[order(mindate$Site),] # order by Site
maxdate <- maxdate[order(maxdate$Site),] # order by Site

min(maxdate$DateLWp - mindate$DateStartp) # 42 days
max(maxdate$DateLWp - mindate$DateStartp) # 1535 days
mean(maxdate$DateLWp - mindate$DateStartp) # 1110.18 days
# range of 42-1535 days for cameras being in the same Site; mean 1110 days

cameff$setup_date <- mindate$DateStartp[match(cameff$Site,mindate$Site)]
cameff$retrieval_date <- maxdate$DateLWp[match(cameff$Site,maxdate$Site)]

head(cameff)
unique(cameff$Site) # 67 camera Sites - includes non-camera Sites
names(cameff)

#cameff2 <- cameff[c(1,30:33,28,29)]

cameff2 <- cameff %>%
  filter(!is.na(utm_y))

###--- Create a camera trap Site table similar to use in camtrapR / determine effort
cameff2$c <- with(cameff2, ave(seq_along(Site), Site, FUN=seq_along))
table(cameff2$Site,cameff2$c) # there are between 1-11 check periods per camera

# create a column to subtract DateStartup from previous DateLWp for each Site
# NA is the first record for each Site
# a value <0 indicates a problem period
cameff3 <- cameff2 %>%
  group_by(Site) %>%
  mutate(D = lag(DateLWp) - DateStartp)
  
# create columns for dates when cameras were not operational
# need to write function to allow ifelse to maintain Date class
safe.ifelse <- function(cond, yes, no)
  structure(ifelse(cond, yes, no), class = class(yes))

cameff3$Problem_from <- safe.ifelse(cameff3$D<0,lag(cameff3$DateLWp), NA)
cameff3$Problem_to <- safe.ifelse(cameff3$D<0, cameff3$DateStartp, NA)

cameff4 <- cameff3 %>%
  group_by(Site) %>%
  filter(c==1 | D<0)
View(cameff4)
names(cameff4)

###--- organize problem dates so that the dataframe is one row per unique Site and problem to/from dates are columns
cameff5 <- spread(cameff4, c, Problem_from)
glimpse(cameff5)
ncol(cameff5)
names(cameff5)
colnames(cameff5)[13:18] <- paste("Problem", seq(1:6),"_from", sep="")
View(cameff5)

cameff5$Problem1_to <- safe.ifelse(!is.na(cameff5$Problem1_from), cameff5$Problem_to, NA)
cameff5$Problem2_to <- safe.ifelse(!is.na(cameff5$Problem2_from), cameff5$Problem_to, NA)
cameff5$Problem3_to <- safe.ifelse(!is.na(cameff5$Problem3_from), cameff5$Problem_to, NA)
cameff5$Problem4_to <- safe.ifelse(!is.na(cameff5$Problem4_from), cameff5$Problem_to, NA)
cameff5$Problem5_to <- safe.ifelse(!is.na(cameff5$Problem5_from), cameff5$Problem_to, NA)
cameff5$Problem6_to <- safe.ifelse(!is.na(cameff5$Problem6_from), cameff5$Problem_to, NA)

# temp data.frames to add back into dataframe for each problem set
cameff.prob1 <- cameff5 %>%
  filter(!is.na(Problem1_from))
cameff.prob2 <- cameff5 %>%
  filter(!is.na(Problem2_from))
cameff.prob3 <- cameff5 %>%
  filter(!is.na(Problem3_from))
cameff.prob4 <- cameff5 %>%
  filter(!is.na(Problem4_from))
cameff.prob5 <- cameff5 %>%
  filter(!is.na(Problem5_from))
cameff.prob6 <- cameff5 %>%
  filter(!is.na(Problem6_from))


# add each unique problem set back in to dataframe with single row per Site
cameff6 <- cameff4 %>%
  filter(c==1)

cameff6 <- cameff6[c(1,8,9)]
names(cameff6)
nrow(cameff6)

cameff8 <- left_join(cameff6, cameff.prob1[c("Site","Problem1_from","Problem1_to")],by="Site")
cameff8 <- left_join(cameff8, cameff.prob2[c("Site","Problem2_from","Problem2_to")],by="Site")
cameff8 <- left_join(cameff8, cameff.prob3[c("Site","Problem3_from","Problem3_to")],by="Site")
cameff8 <- left_join(cameff8, cameff.prob4[c("Site","Problem4_from","Problem4_to")],by="Site")
cameff8 <- left_join(cameff8, cameff.prob5[c("Site","Problem5_from","Problem5_to")],by="Site")
cameff8 <- left_join(cameff8, cameff.prob6[c("Site","Problem6_from","Problem6_to")],by="Site")


glimpse(cameff8)
camdf <- as.data.frame((cameff8))
names(camdf)
View(camdf)

# camdf is now in the correct format and class to use camtrapR to determine operability matrix
# add in covariates to camdf

names(camloc)
nrow(camloc)
nrow(camdf)
camdf <- left_join(camdf, camloc[c(1,5:8)], by="Site")


nrow(camdf) # one row per Site
names(camdf) # columns in correct order to fit into operability  matrix


###--- Camera trap Site operability matrix
camop <- cameraOperation(camdf, 
                stationCol = "Site", 
                setupCol = "setup_date", 
                retrievalCol = "retrieval_date", 
                hasProblems = TRUE,
                dateFormat = "%Y-%m-%d", 
                writecsv = TRUE,
                outDir = getwd())


View(camop)
#Legend: NA: camera(s) not set up, 0: camera(s) not operational, 1 (or higher): number of operational camera(s)
dim(camop) # 65 Sites x 1536 days

camop[camop == 0] <- NA # changes all 0 to NA, as 0 is not operational

camop <- camop[order(rownames(camop)), ] 
camdf <- camdf[order(camdf$Site), ]

#############################################################
###--- Housekeeping - cleaning up files
###--- Saving only camop matrix and camdf covariate dataframe

rm(list= ls()[!(ls() %in% c('camop','camdf'))])
save.image(file="camop.RData") 
